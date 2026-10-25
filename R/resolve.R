# Reference resolution: resolve a relative or absolute URL reference against a
# base URL (RFC 3986 section 5), then canonicalize the result through the same
# url_standard-governed machinery as safe_parse_url(s). Everything downstream of
# the merge (path percent/dot handling, host IPv4/reg-name model, port elision,
# WHATWG backslash recognition, diagnostics) is delegated to safe_parse_urls().
#
# The merge itself is NOT standard-agnostic. PRD v2 D6 claimed it was and this
# file used to repeat the claim; decision P2.7 D-B
# (design/work/url-v3/decisions/P2.7-display-and-resolver-output.md) retires
# that claim against measurement -- WHATWG applies several rules while parsing
# the REFERENCE that RFC 3986 section 5 has no equivalent for. Those rules fire
# under `url_standard = "whatwg"` only; under "rfc3986" the merge is RFC 3986
# section 5.2-5.3 (P2.7 D-C).
#
# The SCHEME PRODUCTION is the one exception, and it splits on a different axis:
# `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )` is RFC 3986 section 3.1's own
# grammar, identical to WHATWG's, so it is a conformance fix under BOTH named
# profiles rather than a WHATWG rule -- see `.split_uri_ref()`.
#
# The NULL selector is frozen byte-for-byte throughout (ADR 0007): no rule in
# this file, including the scheme production, is reachable from it.

# The scheme production, `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )`. It is
# RFC 3986 section 3.1's own grammar and, byte for byte, WHATWG's "scheme start
# state" plus "scheme state" -- the two standards do not disagree here, which is
# why this constant carries no standard in its name. It is also already the
# production the ABSOLUTE-parse path uses everywhere (R/parse-state.R,
# R/url-key.R, R/serialize.R spell the same class inline); the resolver's
# splitter was the one place in the package reading a looser one.
.URI_SCHEME_PRODUCTION <- "[A-Za-z][A-Za-z0-9+.\\-]*"

# The Appendix B scheme group as actually written in RFC 3986's own
# **non-validating** reference regex: "any run of bytes that are not `: / ? #`".
# Appendix B says of itself that it is deliberately permissive, and that
# permissiveness is a defect for reference resolution specifically: a relative
# PATH whose first segment merely contains a colon (`10.0.0.7:8080/foo.html`,
# `[61:24:74]:98`) is read as a scheme, so section 5.2.2's first branch fires
# and the base is discarded. RFC 3986 section 4.2 names the hazard itself.
#
# It is retained ONLY for `url_standard = NULL`, which ADR 0007 freezes
# byte-for-byte; see `.split_uri_ref()`.
.URI_SCHEME_PRODUCTION_APPENDIX_B <- "[^:/?#]+"

# Split a URI reference into its five components using the RFC 3986 Appendix B
# regular expression. Each of scheme / authority / query / fragment is either a
# string (the component was PRESENT, possibly empty) or NA (ABSENT -- e.g. no
# "//" means authority is NA, distinct from "" for "http:///"). `path` is always
# a string (RFC 3986 always matches a, possibly empty, path). Returns NA fields
# for an NA input.
#
# The SCHEME production is the one part that is not standard-agnostic, and not
# because the standards disagree -- they agree (`.URI_SCHEME_PRODUCTION`). What
# differs is what each SELECTOR is allowed to change:
#
#   * `"rfc3986"` and `"whatwg"` both get the real production. Under "rfc3986"
#     that is the RFC's own section 3.1 grammar replacing Appendix B's
#     self-described non-validating shorthand, so it is a conformance fix on the
#     profile's own terms, not an import from the other standard.
#   * `NULL` keeps Appendix B's loose group. It is observably looser -- under
#     the real production `resolve_url("10.0.0.7:8080/x", "http://a/b/c")`
#     stops being an absolute reference and starts merging against the base --
#     and the NULL selector is frozen byte-for-byte by ADR 0007. The freeze
#     governs whether the bytes may move, not whether they are right; the fix
#     ships on the two selectors that CLAIM a standard, and NULL is the surface
#     that claims none.
.split_uri_ref <- function(ref, url_standard = NULL) {
  na_ref <- list(
    scheme = NA_character_, authority = NA_character_, path = NA_character_,
    query = NA_character_, fragment = NA_character_
  )
  if (is.na(ref)) {
    return(na_ref)
  }
  scheme_re <- if (is.null(url_standard)) {
    .URI_SCHEME_PRODUCTION_APPENDIX_B
  } else {
    .URI_SCHEME_PRODUCTION
  }
  # ^((<scheme>):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
  # Groups: 2 scheme, 4 authority, 5 path, 7 query, 9 fragment.
  m <- regmatches(
    ref,
    regexec(
      paste0(
        "^((", scheme_re, "):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$"
      ),
      ref,
      perl = TRUE
    )
  )[[1]]
  if (length(m) < 10L) {
    # This regex (all groups optional, anchored) matches every string, so a
    # short vector is unreachable in practice; stay defensive and treat the
    # whole input as an opaque path.
    na_ref$path <- ref
    return(na_ref)
  }
  # m[[1]] is the full match; capture groups follow. A component counts as
  # PRESENT only when its delimiter group matched: m[[3]] scheme (via the
  # "scheme:" group m[[2]]), m[[4]] = "//authority" gating m[[5]] authority,
  # m[[6]] path (always present), m[[7]] = "?query" gating m[[8]] query,
  # m[[9]] = "#fragment" gating m[[10]] fragment.
  scheme <- if (nzchar(m[[2L]])) m[[3L]] else NA_character_
  authority <- if (startsWith(m[[4L]], "//")) m[[5L]] else NA_character_
  path <- m[[6L]]
  query <- if (startsWith(m[[7L]], "?")) m[[8L]] else NA_character_
  fragment <- if (startsWith(m[[9L]], "#")) m[[10L]] else NA_character_
  list(
    scheme = scheme, authority = authority, path = path,
    query = query, fragment = fragment
  )
}

# Split a path[?query][#fragment] tail -- the Appendix B grammar with BOTH the
# scheme and the authority productions removed. Used only by
# `.split_after_scheme()` below, which has already decided both.
.split_ref_tail <- function(rest) {
  # ^([^?#]*)(\?([^#]*))?(#(.*))?
  # Groups: 1 path, 2 "?query", 3 query, 4 "#fragment", 5 fragment.
  m <- regmatches(
    rest,
    regexec("^([^?#]*)(\\?([^#]*))?(#(.*))?$", rest, perl = TRUE)
  )[[1]]
  if (length(m) < 6L) {
    # Unreachable in practice (every group is optional); stay defensive.
    return(list(path = rest, query = NA_character_, fragment = NA_character_))
  }
  list(
    path = m[[2L]],
    query = if (startsWith(m[[3L]], "?")) m[[4L]] else NA_character_,
    fragment = if (startsWith(m[[5L]], "#")) m[[6L]] else NA_character_
  )
}

# Split the portion of a reference that follows an already-consumed SPECIAL
# scheme, as a relative reference. Returns the same five-field list
# `.split_uri_ref()` does, with `scheme` always NA.
#
# It has TWO callers in `.resolve_one_raw()`, both gated on
# `.whatwg_special_base_scheme()`, because WHATWG reaches the same state chain
# two ways: with the base's own special scheme consumed ("special relative or
# authority state", `.whatwg_reference_is_relative()`) and with no scheme at all
# ("relative state" -> "relative slash state"). The scheme production below is
# absent either way, and the leading `sub()` is a no-op for the scheme-less
# entry, so one function serves both.
#
# Two deliberate departures from `.split_uri_ref()`:
#
#   * There is NO scheme production. That omission is the point: re-running the
#     full splitter on the remainder would re-read a first path segment that
#     happens to contain a colon as a scheme of its own, so `file:C:/` would
#     lose its `C:` and `http::@c:29` would grow a host.
#   * The authority is introduced by TWO leading `/`-or-`\` code points, either
#     of which may be a backslash, and it ends at the next `/ \ ? #`. That is
#     WHATWG's own authority entry for the state reached after a special scheme
#     is consumed, so it belongs to the rule implemented here rather than to the
#     separate backslash-in-reference family: `http:\\foo.com\` against an
#     `http:` base is `http://foo.com/`. Both departures are scoped to this
#     function, so a reference with NO scheme is untouched by either.
#
# `file` differs from the other five special schemes on exactly one point, which
# is why the scheme is a parameter: it has its own state machine (file state ->
# file slash state -> file host state) which consumes exactly TWO slashes, so
# `file:///x` has an EMPTY host and the path `/x`. The other five reach "special
# authority ignore slashes", which skips a run of ANY length, so `http:///x` has
# the host `x`.
.split_after_scheme <- function(ref, scheme_lc) {
  # The leading strip uses the REAL scheme production
  # (`.URI_SCHEME_PRODUCTION`), not Appendix B's loose group, and
  # unconditionally: both call sites are gated
  # on `.whatwg_special_base_scheme()`, so NULL and "rfc3986" never reach here
  # and neither can move. Appendix B's group would break the scheme-LESS entry
  # outright -- once `.split_uri_ref()` stopped calling `10.0.0.7:8080/foo.html`
  # scheme-bearing, that reference started arriving here, and a loose strip
  # would eat `10.0.0.7:` off a path it must leave alone. With the real
  # production the strip is a genuine no-op for anything the splitter already
  # found scheme-less, which is what the comment above claims.
  rest <- sub(paste0("^", .URI_SCHEME_PRODUCTION, ":"), "", ref, perl = TRUE)
  run <- if (identical(scheme_lc, "file")) "{2}" else "{2,}"
  auth <- regmatches(
    rest,
    regexec(
      paste0("^[/\\\\]", run, "([^/?#\\\\]*)(.*)$"), rest,
      perl = TRUE
    )
  )[[1]]
  if (length(auth) == 3L) {
    tail <- .split_ref_tail(auth[[3L]])
    return(list(
      scheme = NA_character_, authority = auth[[2L]], path = tail$path,
      query = tail$query, fragment = tail$fragment
    ))
  }
  tail <- .split_ref_tail(rest)
  list(
    scheme = NA_character_, authority = NA_character_,
    # One leading `\` roots the path exactly as `/` does (WHATWG "relative slash
    # state" -- reached from the same chain, and the reason this rewrite is not
    # the separate backslash-in-reference family's). Only the FIRST byte is
    # rewritten: the rest of the path is left for the serializer's own
    # special-scheme backslash recognition, and `\` inside a query or fragment
    # is not a separator at all, so neither may be touched here.
    path = sub("^\\\\", "/", tail$path, perl = TRUE),
    query = tail$query, fragment = tail$fragment
  )
}

# The base's own scheme, ASCII-lowercased, when BOTH of these hold: this
# resolution is running the WHATWG rules (`url_standard = "whatwg"`), and that
# scheme is one of WHATWG's six special schemes (`.WHATWG_SPECIAL_SCHEMES`,
# R/utils.R -- not a second hand-rolled list). NA_character_ otherwise.
#
# It is the single gate for every WHATWG-only reference-parsing rule below,
# because every one of them is a state WHATWG reaches only from a special
# scheme: "special relative or authority state", "relative slash state" ->
# "special authority ignore slashes state", and `file`'s own file-slash chain.
# A non-special base (`non-spec:/p`) reaches none of them and so is untouched,
# and so is the frozen NULL selector and `"rfc3986"` (ADR 0007 / P2.7 D-C).
.whatwg_special_base_scheme <- function(base, url_standard) {
  if (!.is_whatwg(url_standard) || is.na(base)) {
    return(NA_character_)
  }
  base_scheme <- .split_uri_ref(base, url_standard)$scheme
  if (is.na(base_scheme)) {
    return(NA_character_)
  }
  base_scheme <- .ascii_tolower(base_scheme)
  if (!(base_scheme %in% .WHATWG_SPECIAL_SCHEMES)) {
    return(NA_character_)
  }
  base_scheme
}

# WHATWG "special relative or authority state" (P2.7 D-B): when the reference
# carries the base's OWN special scheme, the scheme is CONSUMED and what follows
# is parsed relatively against the base -- `http:foo.com` against
# `http://example.org/foo/bar` is `http://example.org/foo/foo.com`, not
# `http://foo.com/`. RFC 3986 section 5.2.2 has no such rule: any scheme makes
# the reference absolute. So this fires under `url_standard = "whatwg"` only.
#
# The predicate is deliberately narrow on both halves. `base_scheme` is
# `.whatwg_special_base_scheme()`'s output -- already NA unless the selector is
# "whatwg" and the base's scheme is special -- and the reference's scheme must
# equal it under ASCII case folding: a DIFFERENT special scheme (`https:`
# against an `http:` base) stays absolute, exactly as WHATWG's "special
# authority slashes state" requires.
.whatwg_reference_is_relative <- function(ref_scheme, base_scheme) {
  !is.na(ref_scheme) && !is.na(base_scheme) &&
    identical(.ascii_tolower(ref_scheme), base_scheme)
}

# ---- WHATWG `file:` state machine, resolution half (RURL-ufsltsit) -----------
#
# WHATWG parses a reference against a `file:` base through its own chain --
# "file state" -> "file slash state" -> "file host state" -> "path state" --
# and three of its rules have no RFC 3986 section 5 counterpart. All three are
# about Windows drive letters, and each is spelled once below:
#
#   * "file state": a scheme-less, slash-less reference that BEGINS with a
#     drive letter (`C|/foo`, `C|`, `C|?`, `C|#`) EMPTIES the base path instead
#     of shortening it, so the drive letter becomes the whole path
#     (`.whatwg_file_ref_starts_with_drive()`).
#   * "file slash state": a reference that is a lone `/`-rooted path (`/`,
#     `/x`) INHERITS the base's drive letter when the base path starts with a
#     normalized one and the reference does not (`.whatwg_file_base_drive()`).
#   * "shorten a URL's path": `..` never removes a path whose only segment is a
#     normalized drive letter, and "path state" rewrites `C|` to `C:` when it
#     is the first segment (`.whatwg_file_remove_dot_segments()`).
#
# The fourth rule, "file host state" -- an authority that IS a drive letter
# becomes an empty host plus a path segment -- lives in the ABSOLUTE parser
# (`.parse_whatwg_file_urls_vec()`, R/parse-phases.R), because the resolver's
# output is re-parsed there and the same rule applies to `file://C:/` typed
# directly. Every rule here is reached only from `.whatwg_special_base_scheme()`
# reporting `file`, so `"rfc3986"` and the frozen NULL selector never see them.

# WHATWG "Windows drive letter": two code points, an ASCII alpha then `:` or
# `|`. "Normalized" narrows the second to `:`. Both spellings are recognized
# where the standard's parsed base would already carry the normalized one
# (the base here is SPLIT, not parsed, so `file:///C|/a` arrives unnormalized).
.WHATWG_DRIVE_LETTER_RE <- "^[A-Za-z][:|]"

# "Starts with a Windows drive letter": a drive letter that is the whole string
# or is followed by `/`, `\`, `?` or `#`. The path operand here has already
# been cut at `?`/`#`, so "followed by a delimiter" reduces to end-of-string.
.whatwg_file_ref_starts_with_drive <- function(path) {
  grepl(paste0(.WHATWG_DRIVE_LETTER_RE, "([/\\\\]|$)"), path, perl = TRUE)
}

# The base path's first segment, when it is a drive letter: `"C"` for `/C:/a`
# and `/C|`, NA otherwise. The SPLIT base's path is rooted, so the segment is
# read after the leading `/`.
.whatwg_file_base_drive <- function(base_path) {
  m <- regmatches(
    base_path,
    regexec(paste0("^/([A-Za-z])[:|](/|$)"), base_path, perl = TRUE)
  )[[1]]
  if (length(m) < 2L) NA_character_ else m[[2L]]
}

# WHATWG "path state" over an already-merged path, replacing RFC 3986 section
# 5.2.4 for a `file:` result: `..` shortens the path EXCEPT past a lone
# normalized drive letter ("shorten a URL's path"), `.` and a final `..` leave
# an empty segment behind exactly as section 5.2.4 does, and a first segment
# spelled `C|` is rewritten to `C:` (the "(platform-independent) Windows drive
# letter quirk"). The segment cut keeps a TRAILING empty segment -- plain
# `strsplit` drops one, and `/C:/` is ["C:", ""] -- which is the same trap the
# serializer's `/.` guard records (R/parse-phases.R); hence
# `keep_trailing_empty = TRUE`, which is also what the stringi split it
# replaces did (ADR 0019). The split runs in byte mode, so a U+FEFF that
# starts the body is data and stays on its segment: the "/" once spliced on
# to shield it from stringi's mark skipping (RURL-dglgcwit) is no longer
# needed. An empty body (`path` is "/") gives one empty segment, as before.
.whatwg_file_remove_dot_segments <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  rooted <- startsWith(path, "/")
  body <- if (rooted) substring(path, 2L, .Machine$integer.max) else path
  segs <- .bytes_split_fixed(body, "/", keep_trailing_empty = TRUE)[[1L]]
  out <- character(0)
  n <- length(segs)
  for (i in seq_len(n)) {
    s <- segs[[i]]
    last <- i == n
    if (identical(s, "..")) {
      keep_drive <- length(out) == 1L &&
        grepl("^[A-Za-z]:$", out[[1L]], perl = TRUE)
      if (!keep_drive && length(out) > 0L) {
        out <- out[-length(out)]
      }
      if (last) {
        out <- c(out, "")
      }
    } else if (identical(s, ".")) {
      if (last) {
        out <- c(out, "")
      }
    } else {
      if (length(out) == 0L) {
        s <- sub("^([A-Za-z])\\|$", "\\1:", s, perl = TRUE)
      }
      out <- c(out, s)
    }
  }
  paste0(if (rooted) "/" else "", paste(out, collapse = "/"))
}

# RFC 3986 section 5.2.3: merge a relative-reference path with the base path.
# When the base has an authority and an empty path, the merged path is the
# reference path prefixed with "/"; otherwise it is the base path up to and
# including its last "/", followed by the reference path.
#
# `whatwg_file` adds the one point where WHATWG's "file state" merge differs:
# it CLONES the base path and shortens it, and "shorten" refuses to remove a
# lone normalized drive letter, so `foo` against `file:///C:` is `/C:/foo`
# where section 5.2.3 gives `/foo`.
.merge_ref_path <- function(base_authority, base_path, ref_path,
                            whatwg_file = FALSE) {
  if (!is.na(base_authority) && !nzchar(base_path)) {
    return(paste0("/", ref_path))
  }
  if (whatwg_file &&
        grepl(paste0("^/[A-Za-z][:|]$"), base_path, perl = TRUE)) {
    return(sprintf("%s/%s", sub("\\|$", ":", base_path, perl = TRUE),
                   ref_path))
  }
  slash <- regexpr("/[^/]*$", base_path, perl = TRUE)
  if (slash == -1L) {
    # Base path has no "/": the "everything up to the last /" prefix is empty.
    return(ref_path)
  }
  paste0(substr(base_path, 1L, slash), ref_path)
}

# RFC 3986 section 5.2.2: transform a parsed reference `r` against a parsed base
# `b` into the target components. Returns a component list (scheme / authority /
# path / query / fragment). This step is standard-agnostic; what is NOT is which
# reference reaches it as scheme-bearing (see
# `.whatwg_reference_is_relative()`), what a scheme-bearing one's path gets
# (`whatwg`, below), and -- under `whatwg_file` only -- the three drive-letter
# rules of WHATWG's `file:` chain documented above.
# `._remove_dot_segments()` (R/path-query.R) is reused for the mandated
# dot-segment removal; `whatwg_file` swaps in the drive-letter-aware one.
#
# `whatwg` (RURL-rdkewxlx): a reference that is still scheme-bearing here is
# one the WHATWG basic URL parser reads WITHOUT the base, so its path is left
# exactly as written and the re-parse of the recomposed string does the rest:
# "path state" removes `.` and `..` (and shortens past a drive letter only as
# "shorten a URL's path" allows) and "opaque path state" removes nothing. Doing
# section 5.2.4 here instead turned `foo:a/../b` into `foo:/b`, lost the drive
# letter of `file:/C:/../x`, and, for a special scheme with no `//`, removed
# dots from what the re-parse then read as a HOST (`ws:a/../b` gave `ws://b/`).
.transform_reference <- function(r, b, whatwg_file = FALSE, whatwg = FALSE) {
  remove_dots <- if (whatwg_file) {
    .whatwg_file_remove_dot_segments
  } else {
    ._remove_dot_segments
  }
  if (!is.na(r$scheme)) {
    return(list(
      scheme = r$scheme,
      authority = r$authority,
      path = if (whatwg) r$path else ._remove_dot_segments(r$path),
      query = r$query,
      fragment = r$fragment
    ))
  }
  if (!is.na(r$authority)) {
    authority <- r$authority
    path <- remove_dots(r$path)
    query <- r$query
  } else {
    authority <- b$authority
    if (nzchar(r$path)) {
      if (whatwg_file && .whatwg_file_ref_starts_with_drive(r$path)) {
        # "file state": the remainder begins with a drive letter, so the base
        # path is EMPTIED rather than shortened and the reference is parsed
        # from the root -- `C|/foo/bar` against `file:///tmp/mock/path` is
        # `file:///C:/foo/bar`.
        path <- remove_dots(paste0("/", r$path))
      } else if (startsWith(r$path, "/")) {
        p <- r$path
        if (whatwg_file) {
          # "file slash state": a rooted reference that does not itself start
          # with a drive letter inherits the base's -- `/` against
          # `file:///C:/a/b` is `file:///C:/`, not `file:///`.
          drive <- .whatwg_file_base_drive(b$path)
          if (!is.na(drive) &&
                !.whatwg_file_ref_starts_with_drive(
                  substring(p, 2L, .Machine$integer.max)
                )) {
            p <- paste0("/", drive, ":", p)
          }
        }
        path <- remove_dots(p)
      } else {
        path <- remove_dots(
          .merge_ref_path(b$authority, b$path, r$path, whatwg_file)
        )
      }
      query <- r$query
    } else {
      path <- b$path
      query <- if (is.na(r$query)) b$query else r$query
    }
  }
  list(
    scheme = b$scheme,
    authority = authority,
    path = path,
    query = query,
    fragment = r$fragment
  )
}

# RFC 3986 section 5.3: recompose target components into a URI string. A
# component contributes its delimiter only when it is present (non-NA).
#
# THE `/.` GUARD (RURL-bedensww). Section 5.2.4's remove_dot_segments can hand
# back a path whose FIRST segment is empty -- `/..//path` against `non-spec:/p`
# merges to `//path` -- and section 3.3 forbids exactly that string: "If a URI
# does not contain an authority component, then the path cannot begin with two
# slash characters". Written out verbatim, `non-spec://path` re-reads as an
# AUTHORITY `path` with an empty path, which is not the components that were
# resolved. The resolved string is handed to a parser (safe_parse_urls() or
# serialize_url()), so the recomposition must spell the path in a way the
# grammar reads back as the same components. `/.` is that spelling: a
# dot-segment section 5.2.4 removes again on the next parse, and the guard the
# WHATWG URL serializer emits for the same four conditions (host null, path a
# list, size > 1, first segment empty) -- which `serialize_url()` already
# implements (`.serialize_whatwg_full_vec`, R/parse-phases.R) and was LOSING
# here because it was handed the unguarded string.
#
# Gated on a SELECTED standard, like the scheme production: it is RFC 3986's
# own section 3.3 constraint under "rfc3986" and the serializer's guard under
# "whatwg", and `url_standard = NULL` is byte-frozen (ADR 0007), so the frozen
# selector keeps emitting the unguarded string it always did.
.recompose_uri <- function(t, url_standard = NULL) {
  out <- ""
  if (!is.na(t$scheme)) {
    out <- paste0(out, t$scheme, ":")
  }
  if (!is.na(t$authority)) {
    out <- paste0(out, "//", t$authority)
  } else if (!is.null(url_standard) && !is.na(t$path) &&
               startsWith(t$path, "//")) {
    out <- sprintf("%s/.", out)
  }
  out <- paste0(out, if (!is.na(t$path)) t$path else "")
  if (!is.na(t$query)) {
    out <- paste0(out, "?", t$query)
  }
  if (!is.na(t$fragment)) {
    out <- paste0(out, "#", t$fragment)
  }
  out
}

# Octets resolve_url() splits under `url_standard = NULL` for a row that is
# not valid UTF-8 (RURL-cwzickqy). R's string functions cannot split such a
# string in a UTF-8 locale (they throw) and split it byte by byte in the C
# locale, so the row's answer depended on LC_CTYPE. The resolution reads only
# ASCII delimiters, so every octet 0x80-0xFF of BOTH operands is spelled as
# the private-use code point U+F700 + octet while it runs, and the result is
# spelled back octet for octet: the C-locale split, in every locale. Both
# operands of the row are hidden, so every non-ASCII code point in the result
# is a hidden octet and the mapping back is exact, whatever the input held.
.hide_high_octets <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    b <- as.integer(charToRaw(s))
    high <- b >= 0x80L
    b[high] <- b[high] + 0xF700L
    intToUtf8(b)
  }, character(1), USE.NAMES = FALSE)
}

.restore_high_octets <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    cp <- utf8ToInt(s)
    high <- cp >= 0x80L
    cp[high] <- cp[high] - 0xF700L
    rawToChar(as.raw(cp))
  }, character(1), USE.NAMES = FALSE)
}

# TRUE where `base` is a base the selected standard's own parser refuses
# (RURL-vrvxxfvm); all FALSE under `url_standard = NULL`, which is frozen
# (ADR 0007), and FALSE for an NA base, which is NO base rather than a failing
# one. The standard's parser is `serialize_url()`, the standards surface, which
# is NA exactly where that parser fails. `get_parse_status()` is not the
# oracle: under the default `scheme_acceptance = "web"` it calls `non-spec:/p`
# or `about:blank` an "error" by rurl's acceptance policy (ADR 0004), not by
# either standard, and every one of those is a base WPT resolves against.
# Each distinct base is parsed once and mapped back to its rows.
.refused_base_mask <- function(base, url_standard) {
  refused <- logical(length(base))
  if (is.null(url_standard)) {
    return(refused)
  }
  distinct <- unique(base[!is.na(base)])
  if (length(distinct) == 0L) {
    return(refused)
  }
  failing <- distinct[is.na(serialize_url(distinct, standard = url_standard))]
  !is.na(base) & base %in% failing
}

# TRUE where `ref` is an RFC 3986 URI-reference (section 4.1:
# `URI-reference = URI / relative-ref`), FALSE where it is not, NA for NA.
# Built on the package's one RFC 3986 grammar, `.rfc3986_generic_uri_ok()`
# (R/parse-state.R), which accepts only the `URI` alternative (a scheme is
# required), strict US-ASCII included (RUL-030):
#
#   * A reference with a scheme (section 3.1, the same production
#     `.split_uri_ref()` uses under "rfc3986") is a `URI`: walked as written.
#   * A reference without one is a `relative-ref` (section 4.2):
#     `relative-part [ "?" query ] [ "#" fragment ]`. Its `relative-part`
#     alternatives are `hier-part`'s one for one (`"//" authority
#     path-abempty`, `path-absolute`, `path-empty`), except that
#     `path-noscheme` stands where `hier-part` has `path-rootless`; the two
#     differ only in the first segment, which `path-noscheme` writes as
#     `segment-nz-nc` (no ":"). So the reference is walked behind a one-letter
#     scheme, `x:`, which reads the rest as exactly that `hier-part`, query and
#     fragment, and a first segment holding a ":" is then refused. That segment
#     is the run before the first "/", "?" or "#", and a ":" in it is what
#     `\A[^/?#]*:` finds; a reference starting "/", "?" or "#" has an empty
#     first segment and never matches.
.rfc3986_uri_reference_ok <- function(ref) {
  ok <- rep(NA, length(ref))
  present <- !is.na(ref)
  absolute <- present & !is.na(.scheme_prefix_len(ref))
  relative <- present & !absolute
  if (any(absolute)) {
    ok[absolute] <- .rfc3986_generic_uri_ok(ref[absolute])$ok
  }
  if (any(relative)) {
    rel <- ref[relative]
    ok[relative] <- .rfc3986_generic_uri_ok(paste0("x:", rel))$ok &
      !.state_bytes_test(rel, "\\A[^/?#]*:")
  }
  ok
}

# Resolve ONE (reference, base) pair to a raw absolute URI string, or NA when
# resolution cannot yield an absolute URL (base not absolute and reference not
# absolute either). ._remove_dot_segments happens inside .transform_reference(),
# except for a scheme-bearing reference under "whatwg", whose path is passed
# through as written for the WHATWG re-parse (RURL-rdkewxlx).
# `url_standard` selects the reference-parsing rules that precede the merge
# (P2.7 D-B); its default NULL is the frozen selector and reproduces the
# pre-D-B behavior exactly, as does "rfc3986".
.resolve_one_raw <- function(ref, base, url_standard = NULL) {
  if (is.na(ref)) {
    return(NA_character_)
  }
  # WHATWG basic URL parser step 1, applied to the REFERENCE before anything
  # reads it: strip a leading/trailing C0-control-or-SPACE run, then remove
  # every ASCII tab/LF/CR. The absolute-parse path already runs step 1 through
  # `.strip_whatwg_control_chars_vec()` (R/parse-phases.R) and that function is
  # deliberately the single seam for it, so this reuses it rather than
  # re-spelling the character classes. It self-gates on the selector and is a
  # byte-for-byte no-op under "rfc3986" and under NULL. Stripping to the EMPTY
  # string is meaningful, not a degenerate case: an empty reference is RFC 3986
  # section 5.2.2's "base minus its fragment" branch, which
  # `.transform_reference()` below already implements.
  ref <- .strip_whatwg_control_chars_vec(ref, url_standard)$url
  r <- .split_uri_ref(ref, url_standard)
  base_scheme <- .whatwg_special_base_scheme(base, url_standard)
  if (.whatwg_reference_is_relative(r$scheme, base_scheme)) {
    # The base's own special scheme: consume it and continue relatively.
    r <- .split_after_scheme(ref, .ascii_tolower(r$scheme))
  } else if (is.na(r$scheme) && !is.na(base_scheme)) {
    # A scheme-LESS reference under a special base reaches WHATWG's "relative
    # state", whose leading-`/`-or-`\` chain is the SAME chain a consumed
    # special scheme reaches: `\` counts exactly as `/`, a second one enters the
    # authority ("special authority ignore slashes" skips the whole run for the
    # five non-`file` special schemes; `file` consumes exactly two), and a
    # single leading `\` roots the path as `/` does ("relative slash state").
    # That chain is `.split_after_scheme()`, so it is CALLED here rather than
    # reimplemented -- the two entries differ only in whether a scheme was
    # consumed first, which its `sub()` handles as a no-op for a reference the
    # RFC splitter already found scheme-less. RFC 3986 has no such chain (`\` is
    # an ordinary path byte and `//` is the whole authority production), so this
    # is reachable under `url_standard = "whatwg"` only.
    r <- .split_after_scheme(ref, base_scheme)
  }
  if (!is.na(r$scheme)) {
    # Absolute reference: base is irrelevant (section 5.2.2 first branch).
    empty_base <- .split_uri_ref(NA_character_, url_standard)
    return(.recompose_uri(
      .transform_reference(r, empty_base, whatwg = .is_whatwg(url_standard)),
      url_standard
    ))
  }
  # Relative reference: the base must be an absolute URL (have a scheme).
  if (is.na(base)) {
    return(NA_character_)
  }
  # The BASE is split with the same production as the reference: a "base" whose
  # scheme is only a scheme under Appendix B's loose group (`10.0.0.7:8080/x`)
  # is not an absolute URL, and the `is.na(b$scheme)` guard below is exactly the
  # place that must say so.
  b <- .split_uri_ref(base, url_standard)
  if (is.na(b$scheme)) {
    return(NA_character_)
  }
  # `base_scheme` is already NA unless the selector is "whatwg" and the base's
  # scheme is special, so this is the WHATWG `file:` chain's gate and nothing
  # else's.
  whatwg_file <- identical(base_scheme, "file")
  .recompose_uri(.transform_reference(r, b, whatwg_file), url_standard)
}

#' Resolve a URL reference against a base URL
#'
#' Resolves a relative or absolute URL reference against a base URL following
#' the RFC 3986 section 5 reference-resolution algorithm, then renders the
#' resolved absolute URL on the output surface \code{output} selects. Under the
#' default \code{output = "clean"} the result is canonicalized with the same
#' machinery as \code{\link{safe_parse_url}}; under \code{output = "serialized"}
#' it is handed to \code{\link{serialize_url}} instead. \code{url_standard} and
#' any \code{...} options flow straight through to the parse, so the host
#' IPv4/reg-name model, path percent/dot-segment handling, default-port elision,
#' WHATWG backslash-as-slash recognition, and diagnostics are exactly those of a
#' direct \code{safe_parse_url()} call on the resolved URL.
#'
#' @section Reference resolution is standard-aware:
#'
#' The merge itself (empty reference, fragment-only, query-only, scheme-relative
#' \code{//host} reference, absolute-path reference, and relative-path merge) is
#' RFC 3986 section 5.2--5.3 under \code{url_standard = "rfc3986"} and under the
#' default \code{NULL} selector, with one exception under \code{"rfc3986"} noted
#' last below. Under \code{url_standard = "whatwg"} the WHATWG
#' URL Standard's reference-parsing rules are applied first, because they are
#' rules the two standards genuinely disagree on rather than composition of the
#' axes \code{url_standard} already governs (decision P2.7 D-B,
#' \code{design/work/url-v3/decisions/P2.7-display-and-resolver-output.md}):
#'
#' \itemize{
#'   \item \strong{A reference carrying the base's own special scheme is
#'     relative, not absolute.} WHATWG's \dQuote{special relative or authority
#'     state} consumes a scheme equal to the base's when that scheme is special
#'     (\code{http}, \code{https}, \code{ws}, \code{wss}, \code{ftp},
#'     \code{file}) and keeps parsing against the base, so
#'     \code{resolve_url("http:foo.com", "http://example.org/foo/bar",
#'     url_standard = "whatwg")} is \code{"http://example.org/foo/foo.com"},
#'     while RFC 3986 treats any scheme as making the reference absolute and
#'     gives \code{"http://foo.com/"}. A \emph{different} scheme stays absolute
#'     under both, even when it is also special.
#'   \item \strong{Under a special base, a leading \code{\\} in the reference is
#'     a \code{/}, and a run of either introduces an authority.} WHATWG's
#'     \dQuote{relative slash state} reads \code{\\} exactly as \code{/}, so
#'     \code{resolve_url("\\x", "http://example.org/foo/bar", url_standard =
#'     "whatwg", output = "serialized")} is \code{"http://example.org/x"}; a
#'     second slash-or-backslash enters the authority, and
#'     \dQuote{special authority ignore slashes} then skips the whole run for
#'     the five non-\code{file} special schemes, so \code{"///example.org/x"}
#'     and \code{"/\\/\\//example.org/x"} both resolve with the host
#'     \code{example.org}. \code{file} has its own state machine and consumes
#'     exactly two. RFC 3986 has neither rule: \code{\\} is an ordinary path
#'     byte and \code{//} is the entire authority production.
#'   \item \strong{The reference is stripped before it is read.} The WHATWG
#'     basic URL parser's step 1 removes a leading and trailing run of C0
#'     control or SPACE (U+0000--U+0020) and then every ASCII tab, LF and CR,
#'     so \code{" foo.com "} resolves as \code{"foo.com"} does. A reference that
#'     strips to the empty string is the empty reference, which resolves to the
#'     base minus its fragment. RFC 3986 has no strip step -- such bytes are
#'     required to be percent-encoded -- so under \code{"rfc3986"} and
#'     \code{NULL} they stay in the reference.
#'   \item \strong{Against a \code{file:} base, Windows drive letters follow
#'     the WHATWG \code{file:} state machine.} A reference that \emph{begins}
#'     with a drive letter empties the base path instead of shortening it, so
#'     \code{resolve_url("C|/foo", "file:///tmp/mock/path", url_standard =
#'     "whatwg", output = "serialized")} is \code{"file:///C:/foo"}; a rooted
#'     reference inherits the base's drive letter (\code{"/"} against
#'     \code{"file:///C:/a/b"} is \code{"file:///C:/"}); \code{..} never
#'     removes a lone drive letter (\code{".."} against \code{"file:///C:/"}
#'     is \code{"file:///C:/"}); and \code{C|} in the first segment is
#'     normalized to \code{C:}. A drive letter in the authority position
#'     (\code{"//d:"}) is an empty host plus a path segment,
#'     \code{"file:///d:"}. RFC 3986 has no drive-letter concept, so under
#'     \code{"rfc3986"} and \code{NULL} the plain section 5.2 merge applies.
#'   \item \strong{Any other scheme-bearing reference resolves to its own
#'     parse.} The base plays no part, and the reference's path is left to the
#'     WHATWG path state, so an opaque path keeps its dot segments
#'     (\code{resolve_url("foo:a/../b", "http://h/y", url_standard = "whatwg",
#'     output = "serialized")} is \code{"foo:a/../b"}) and a \code{file:}
#'     drive letter survives \code{..} (\code{"file:/C:/../x"} gives
#'     \code{"file:///C:/x"}). RFC 3986 section 5.2.2 removes the dot segments
#'     of such a path, so under \code{"rfc3986"} the first is \code{"foo:/b"}.
#' }
#'
#' The \code{NULL} selector is frozen and unaffected (ADR 0007; P2.7 D-C):
#' every rule above is reachable only through
#' \code{url_standard = "whatwg"}, or through a \code{profile} that bundles it
#' (see \code{...}).
#'
#' Two further rules apply under \strong{both} named profiles, because the two
#' standards agree on them. First, a resolved path whose first segment is
#' empty is recomposed with the \code{/.} guard: RFC 3986 section 3.3 forbids
#' a path beginning with \code{//} after no authority, and the WHATWG URL
#' serializer emits the same guard, so \code{resolve_url("/..//path",
#' "non-spec:/p", url_standard = "whatwg", output = "serialized")} is
#' \code{"non-spec:/.//path"} rather than a string that re-reads as the
#' authority \code{path}. Under \code{"whatwg"} a reference with a scheme of
#' its own reaches the parse as written, so the WHATWG serializer, not the
#' resolver, writes its guard: \code{"non-spec:/..//x"} gives
#' \code{"non-spec:/.//x"}. The \code{NULL} selector recomposes the unguarded
#' string, as it always did. Second, a scheme is
#' \code{ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )} -- RFC 3986 section 3.1's
#' own grammar, and WHATWG's -- so a relative path whose first segment merely
#' \emph{contains} a colon is a path, not an absolute reference:
#' \code{resolve_url("[61:24:74]:98", "http://example.org/foo/bar",
#' url_standard = "whatwg", output = "serialized")} is
#' \code{"http://example.org/foo/[61:24:74]:98"}. \code{"rfc3986"} reads no
#' scheme there either, but refuses the reference (see below). The \code{NULL}
#' selector instead keeps RFC 3986 Appendix B's
#' explicitly \emph{non-validating} \code{[^:/?#]+}, which reads
#' \code{"10.0.0.7"} as a scheme and discards the base; that is frozen behavior
#' (ADR 0007), not a recommendation.
#'
#' Under \code{"rfc3986"} the reference must itself be an RFC 3986
#' URI-reference (section 4.1: a \code{URI} or a \code{relative-ref}), or the
#' row is \code{NA}. This is checked on the reference as written, before the
#' merge, so a segment that dot-segment removal would delete still counts:
#' \code{resolve_url("a/\\u00e4/../b", "http://b.de/", url_standard =
#' "rfc3986")} is \code{NA}, since U+00E4 is a raw non-ASCII code point
#' (sections 2 and 2.5), as are \code{"/\%zz/../b"} (a \code{\%} that starts
#' no triplet) and
#' \code{"10.0.0.7:8080/x"} (section 4.2: a first segment holding a colon
#' cannot start a relative-path reference; write \code{"./10.0.0.7:8080/x"}).
#' \code{"whatwg"} and \code{NULL} do not check the reference. Each rule in
#' this section applies to the standard \code{url_standard} names, or to the
#' one a \code{profile} in \code{...} bundles.
#'
#' @section Which output surface you want:
#'
#' \code{output} selects between two different products, not two settings of
#' one (decision P2.7 D-A,
#' \code{design/work/url-v3/decisions/P2.7-display-and-resolver-output.md}):
#'
#' \itemize{
#'   \item \code{output = "clean"} (the default) returns the \emph{canonical}
#'     \code{clean_url} of the resolved reference, not a verbatim RFC 3986
#'     recomposition: as everywhere else in rurl, the fragment and userinfo are
#'     excluded from \code{clean_url}, the query is included only when
#'     \code{query_handling != "drop"} (the default drops it), and the port only
#'     when \code{port_handling != "exclude"}. This surface is
#'     \strong{intentionally lossy} -- it is a cleaning/SEO product driven by
#'     presentation policy, and it therefore \strong{cannot carry a conformance
#'     claim}. This differs from a generic resolver such as
#'     \code{xml2::url_absolute()} or Python's \code{urljoin}, which preserve
#'     every component verbatim; \code{resolve_url()} resolves \emph{and}
#'     canonicalizes.
#'   \item \code{output = "serialized"} returns
#'     \code{\link{serialize_url}(<resolved absolute URL>, standard =
#'     url_standard, form = form)}: the standard's own full-string
#'     serialization, with the fragment preserved and credentials
#'     reconstructed. This is the standards surface -- the one a conformance
#'     claim may be measured on -- and it is where RFC 3986 section 5.4's own
#'     expectations are reproduced exactly (\code{resolve_url("?y",
#'     "http://a/b/c/d;p?q", url_standard = "rfc3986", output = "serialized")}
#'     is \code{"http://a/b/c/d;p?y"}).
#' }
#'
#' \code{output = "serialized"} \strong{requires} an explicit
#' \code{url_standard}: \code{NULL} selects no standard, so there is nothing to
#' serialize \emph{to}, and the combination is an error rather than a silent
#' choice of one. Because \code{\link{serialize_url}} accepts no presentation
#' options at all, \code{output = "serialized"} also rejects any \code{...}
#' argument: honoring, say, \code{port_handling = "exclude"} is impossible on
#' that surface, and accepting-then-discarding it would misreport what was
#' returned.
#'
#' To inspect individual resolved components (including the fragment), resolve
#' first and pass the result to \code{\link{safe_parse_url}}.
#'
#' @param relative_or_absolute A character vector of URL references to resolve.
#'   Each may be relative (\code{"../b"}, \code{"?q=1"}, \code{"#frag"},
#'   \code{"//host/p"}) or already absolute (\code{"https://host/p"}). An
#'   absolute reference takes nothing from \code{base_url}, except that under
#'   \code{url_standard = "whatwg"} (or a \code{profile} that bundles it) a
#'   base that standard refuses makes the row \code{NA} (see
#'   \code{base_url}). That includes a scheme-less base such as
#'   \code{"example.com/a/b"}, even where the profile's own parse would fix
#'   the base up.
#' @param base_url A character vector of base URLs, recycled against
#'   \code{relative_or_absolute}. Each base must itself be an absolute URL
#'   (carry a scheme); a relative reference resolved against a scheme-less or
#'   \code{NA} base yields \code{NA}. Under a named \code{url_standard}
#'   (given here or bundled by a \code{profile}), a base
#'   that standard's parser refuses (\code{\link{serialize_url}} gives
#'   \code{NA}) is no base, row by row: under \code{"whatwg"} the row is
#'   \code{NA} even for an absolute reference (the URL Standard's API URL
#'   parser fails on a failing base before it reads the input); under
#'   \code{"rfc3986"} a relative reference is \code{NA} and an absolute one
#'   still resolves (RFC 3986 sections 5.1 and 5.2.2). The frozen \code{NULL}
#'   selector (no \code{url_standard} and no \code{profile}) does not check
#'   the base.
#' @param url_standard Optional standard profile forwarded to the parse:
#'   \code{NULL} (default), \code{"rfc3986"}, or \code{"whatwg"}. See
#'   \code{\link{safe_parse_url}} for the axes it governs, and
#'   \emph{Reference resolution is standard-aware} for the reference-parsing
#'   rules \code{"whatwg"} adds ahead of the merge. Required (non-\code{NULL})
#'   when \code{output = "serialized"}.
#' @param output Which output surface to return: \code{"clean"} (default,
#'   today's canonical \code{clean_url} bytes) or \code{"serialized"} (the
#'   selected standard's full-string serialization of the resolved absolute
#'   URL, via \code{\link{serialize_url}}). See \emph{Which output surface you
#'   want}.
#' @param form For \code{output = "serialized"} with
#'   \code{url_standard = "rfc3986"} only, the RFC posture forwarded to
#'   \code{\link{serialize_url}}: \code{"source"} (default, source-preserving)
#'   or \code{"normalized"}. Ignored for \code{"whatwg"}, whose serializer has a
#'   single spec-defined form, and ignored under \code{output = "clean"}, whose
#'   rendering is driven by the cleaning dials instead -- the same
#'   argument-is-inert-where-it-does-not-apply contract
#'   \code{\link{serialize_url}} itself holds for \code{form}.
#' @param ... Additional arguments forwarded to \code{\link{safe_parse_urls}}
#'   (e.g. \code{port_handling}, \code{query_handling}, \code{host_encoding}).
#'   Passing a governed low-level knob that conflicts with \code{url_standard}
#'   errors, exactly as it does for \code{\link{safe_parse_url}}. A
#'   \code{profile} passed here selects the standard it bundles (see
#'   \code{\link{url_profile}}) for the resolution as well as for the parse:
#'   \code{profile = "rfc-syntax"} resolves as \code{url_standard = "rfc3986"}
#'   and \code{"whatwg"}, \code{"browser"}, \code{"seo"} and its alias
#'   \code{"canonical"} as \code{"whatwg"}. An explicit
#'   \code{url_standard} overrides the profile's, as it does for
#'   \code{\link{safe_parse_url}}. These are
#'   presentation dials consumed by the \code{"clean"} path only; supplying any
#'   of them together with \code{output = "serialized"} is an error, because
#'   \code{\link{serialize_url}} takes no presentation arguments and the dial
#'   could not be honored.
#' @return A character vector the same length as the recycled inputs, unnamed
#'   (names are not data). Under \code{output = "clean"} each element is the
#'   canonical \code{clean_url} of the resolved reference; under
#'   \code{output = "serialized"} it is the standard's full-string
#'   serialization of the resolved absolute URL. \code{NA} where resolution
#'   cannot produce an absolute URL, where under \code{"rfc3986"} (named or
#'   bundled by a \code{profile}) the
#'   reference is no RFC 3986 URI-reference, or where the resolved URL is not
#'   accepted by the parser (\code{"clean"}) or by the selected standard's
#'   parser (\code{"serialized"}).
#' @seealso \code{\link{safe_parse_url}}, \code{\link{get_clean_url}},
#'   \code{\link{serialize_url}}
#' @export
#' @examples
#' resolve_url("../g", "http://a/b/c/d;p?q") # -> "http://a/b/g"
#' resolve_url("g", "http://a/b/c/d;p?q") # -> "http://a/b/c/g"
#' resolve_url("//example.org/p", "http://a/b/c") # -> "http://example.org/p"
#' resolve_url("https://x.com/y", "http://a/b/c") # absolute ref, base unused
#' resolve_url(c("g", "../h"), "http://a/b/c/") # vectorized
#'
#' # The standards surface keeps the query and the fragment RFC 3986 section
#' # 5.4 requires; the (lossy) clean surface drops both by design.
#' resolve_url("?y", "http://a/b/c/d;p?q",
#'             url_standard = "rfc3986", output = "serialized")
#' resolve_url("#s", "http://a/b/c/d;p?q",
#'             url_standard = "whatwg", output = "serialized")
#' resolve_url("#s", "http://a/b/c/d;p?q")
resolve_url <- function(relative_or_absolute, base_url, url_standard = NULL,
                        output = c("clean", "serialized"),
                        form = c("source", "normalized"),
                        ...) {
  url_standard <- .validate_url_standard(url_standard)
  output <- match.arg(output)
  form <- match.arg(form)
  dots <- list(...)

  if (identical(output, "serialized")) {
    # `NULL` selects no standard, so there is nothing to serialize TO; and
    # serialize_url() takes no presentation arguments, so a `...` dial provably
    # cannot apply. Both are errors rather than silent choices (P2.7 D-A).
    if (is.null(url_standard)) {
      stop(
        "output = \"serialized\" requires an explicit `url_standard` ",
        "(\"rfc3986\" or \"whatwg\"): url_standard = NULL selects no ",
        "standard, so there is no serialization to return.",
        call. = FALSE
      )
    }
    if (length(dots) > 0L) {
      nm <- names(dots)
      if (is.null(nm)) {
        nm <- rep("", length(dots))
      }
      nm[!nzchar(nm)] <- "<unnamed>"
      stop(
        sprintf(
          paste0(
            "output = \"serialized\" accepts no parse or presentation ",
            "options; drop `%s` or use output = \"clean\". The standard ",
            "serialization is an identity, so serialize_url() takes no such ",
            "arguments and one passed here could not be honored."
          ),
          paste(nm, collapse = "`, `")
        ),
        call. = FALSE
      )
    }
  } else {
    # url_standard conflict check across the `...` seam (same contract as
    # canonical_join(): missing() cannot see through `...`, so read the governed
    # knobs straight from the captured dots).
    .check_url_standard_conflicts_dots(
      c(list(url_standard = url_standard), dots)
    )
  }
  # The standard the RESOLUTION reads (RURL-nwujwmft). A named profile bundles
  # a `url_standard` (`.URL_PROFILES`, R/parse.R) and the parse below reads it
  # through `...`; the rules that run before the parse -- the undecodable-octet
  # reading, the rfc3986 URI-reference gate, the refused-base rule and the
  # merge -- must read the same one, or `profile = "rfc-syntax"` parses as
  # RFC 3986 but resolves as the frozen NULL selector. It comes from
  # `.resolve_profile()`, the helper safe_parse_url() uses, so an explicit
  # `url_standard` overrides the bundle's exactly as it does there (the iron
  # rule). `serialized` takes no `...`, so there it is `url_standard`.
  # `...` reaches safe_parse_urls(), which matches an abbreviated `prof =`
  # to its `profile` formal, so read it the same way here.
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
  spu_formals <- names(formals(safe_parse_urls))
  dot_formals <- spu_formals[pmatch(dot_names, spu_formals,
                                    duplicates.ok = TRUE)]
  profile_at <- which(dot_formals == "profile")
  profile <- .validate_profile(
    if (length(profile_at) == 1L) dots[[profile_at]] else NULL
  )
  resolution_standard <- if (is.null(profile)) {
    url_standard
  } else {
    .resolve_profile(
      profile, list(url_standard = url_standard)
    )$opts$url_standard
  }

  # Both operands pass the input boundary before anything reads them
  # (RURL-cwzickqy). .resolve_one_raw() splits, merges and recomposes them as
  # strings, and R's string functions read a string by LOCALE: a latin1 one
  # as CP1252 in a UTF-8 locale (0x9F became U+0178, RURL-ummmdiwu) and an
  # unmarked one holding invalid UTF-8 as an error there (the whole call
  # threw) and as single bytes in the C locale (the row resolved). A
  # bytes-marked base resolved too, although the parser refuses it.
  ref_in <- .prepare_url_input(as.character(relative_or_absolute))
  base_in <- .prepare_url_input(as.character(base_url))
  ref <- ref_in$url
  base <- base_in$url

  # A zero-length operand yields a zero-length result, as R vectorized ops do.
  if (length(ref) == 0L || length(base) == 0L) {
    return(character(0))
  }
  # Recycle both inputs to the common length (base_url is commonly scalar).
  n <- max(length(ref), length(base))
  ref <- rep_len(ref, n)
  base <- rep_len(base, n)
  # A string that is not valid UTF-8 cannot be split as one, in any locale,
  # so each standard reads the row here as its parser reads it at the parse
  # seams; this function reshapes the strings before any parser sees them.
  # `whatwg` parses a scalar-value string, so it reads an undecodable octet
  # as U+FFFD, as its parse seam does (UTF-8 decode with replacement); the
  # conversion validates, so it is the same in every locale. `rfc3986`
  # rejects the octet (RFC 3986 section 2: a URI is a sequence of
  # characters), so an undecodable reference resolves to NA and an
  # undecodable base is no base (NA, as a bytes one already is), and an
  # absolute reference still resolves without it. `NULL` accepts the octet
  # in some components (a path, query or fragment of a web URL, RURL-yarkccco)
  # and not in others, so the parse of the resolved URL decides: see below.
  ref_bad <- !rep_len(ref_in$decodable, n)
  base_bad <- !rep_len(base_in$decodable, n)
  hidden <- logical(n)
  if (.is_whatwg(resolution_standard)) {
    # The replacement is the reading, not a fault, and the parse seam reads
    # the same octet without a warning; stringi's "fixing" notice is muffled.
    # stri_enc_toutf8() reads a U+FEFF that starts its input as a byte-order
    # mark and drops it (ADR 0019 RC2), but in a URL the code point is data:
    # the URL Standard strips no U+FEFF and percent-encodes it in a path
    # (RURL-tfttelwo). So a row led by the mark is decoded with an ASCII `x`
    # spliced on first, and that one byte is cut off after, as
    # .url_key_frame() does. The decode stays stringi's, for its one U+FFFD
    # per maximal subpart (ADR 0019, semantic exception 2(d)). Every element
    # here is marked UTF-8 by the input boundary, so paste0() does not
    # translate it by the locale.
    replace <- function(x) {
      lead <- .starts_with_bom(x)
      x[lead] <- paste0("x", x[lead])
      out <- suppressWarnings(stringi::stri_enc_toutf8(x, validate = TRUE))
      out[lead] <- .bytes_cut(out[lead], 2L, .Machine$integer.max)
      out
    }
    ref[ref_bad] <- replace(ref[ref_bad])
    base[base_bad] <- replace(base[base_bad])
  } else if (!is.null(resolution_standard)) {
    ref[ref_bad] <- NA_character_
    base[base_bad] <- NA_character_
  } else {
    # `NULL`: an undecodable row resolves octet for octet, as a decodable one
    # resolves character for character, and the parser below reads the
    # result as it reads any URL: a path octet becomes %XX, one in the host
    # fails the row. As for a decodable base, the parser is not asked about
    # the base itself.
    hidden <- ref_bad | base_bad
    ref[hidden] <- .hide_high_octets(ref[hidden])
    base[hidden] <- .hide_high_octets(base[hidden])
  }
  # WHATWG basic URL parser step 1, applied to the BASE (RURL-mvjgpthk): the
  # API URL parser runs the basic URL parser on the base too, so a leading or
  # trailing C0-control-or-SPACE run is stripped and every ASCII tab/LF/CR
  # removed before the base is split. The reference gets the same step inside
  # `.resolve_one_raw()`, through the same single seam. Done here, before the
  # refused-base mask, so the mask's oracle (`serialize_url()`, which runs
  # step 1 itself) and the resolver read the same string. A byte-for-byte
  # no-op under "rfc3986" and NULL.
  base <- .strip_whatwg_control_chars_vec(base, resolution_standard)$url
  # Under `rfc3986` the REFERENCE must itself be an RFC 3986 URI-reference
  # (section 4.1: URI / relative-ref), or there is nothing to resolve: the row
  # is NA. Checking only the resolved URL is not enough, because section
  # 5.2.4's dot-segment removal can delete the segment that breaks the
  # grammar (`a/ä/../b`, `/%zz/../b` resolve to valid URIs). Each
  # distinct reference is checked once, row by row, so one refused reference
  # moves no other row. `whatwg` has no such gate (its parser reads the
  # reference with the base), and `NULL` is frozen (ADR 0007).
  if (identical(resolution_standard, "rfc3986")) {
    distinct <- unique(ref[!is.na(ref)])
    if (length(distinct) > 0L) {
      refused_ref <- distinct[!.rfc3986_uri_reference_ok(distinct)]
      ref[!is.na(ref) & ref %in% refused_ref] <- NA_character_
    }
  }
  # A base the selected standard's parser refuses is no base (RURL-vrvxxfvm).
  # `whatwg`: the URL Standard's API URL parser returns failure when the base
  # fails to parse, before it reads the input, so the whole row is NA, an
  # absolute reference included. `rfc3986`: section 5.1 requires the base to
  # be an absolute URI, and section 5.2.2 reads the base only when the
  # reference has no scheme, so the row resolves as against an NA base: a
  # relative reference gives NA, an absolute one resolves. `NULL` is not
  # asked: it resolves against such a base as it always has, because the
  # selector is frozen (ADR 0007) and ADR 0016 moves it only with a witness
  # that the default path is defective and a declared signature, which this
  # change does not carry.
  refused <- .refused_base_mask(base, resolution_standard)
  base[refused] <- NA_character_
  if (.is_whatwg(resolution_standard)) {
    ref[refused] <- NA_character_
  }

  resolved_raw <- vapply(
    seq_len(n),
    function(i) .resolve_one_raw(ref[[i]], base[[i]], resolution_standard),
    character(1)
  )
  resolved_raw[hidden] <- .restore_high_octets(resolved_raw[hidden])

  if (identical(output, "serialized")) {
    # Surface (b): hand the resolved absolute string to the standard serializer
    # (P2.7 D-A). `engine` is deliberately NOT forwarded -- resolve_url() has no
    # such argument and adding one is a separate surface question, so the call
    # OMITS it rather than passing NULL through (RURL-owrdsivt house rule).
    # serialize_url() is length-preserving and returns NA for an NA element, so
    # the unresolvable rows stay NA exactly as under output = "clean".
    return(serialize_url(resolved_raw, standard = url_standard, form = form))
  }

  # Delegate ALL normalization/rendering/diagnostics to the shared parser so
  # resolve_url() adds no divergent behavior. safe_parse_urls() memoizes, so
  # duplicate resolved URLs cost only a match().
  parsed <- safe_parse_urls(resolved_raw, url_standard = url_standard, ...)
  out <- parsed$clean_url
  # A reference that could not resolve to an absolute URL (NA raw) is NA here
  # anyway, since safe_parse_urls() returns NA clean_url for an NA input.
  out
}
