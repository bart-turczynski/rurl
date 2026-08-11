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

# RFC 3986 section 5.2.3: merge a relative-reference path with the base path.
# When the base has an authority and an empty path, the merged path is the
# reference path prefixed with "/"; otherwise it is the base path up to and
# including its last "/", followed by the reference path.
.merge_ref_path <- function(base_authority, base_path, ref_path) {
  if (!is.na(base_authority) && !nzchar(base_path)) {
    return(paste0("/", ref_path))
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
# `.whatwg_reference_is_relative()`).
# `._remove_dot_segments()` (R/path-query.R) is reused for the mandated
# dot-segment removal.
.transform_reference <- function(r, b) {
  if (!is.na(r$scheme)) {
    return(list(
      scheme = r$scheme,
      authority = r$authority,
      path = ._remove_dot_segments(r$path),
      query = r$query,
      fragment = r$fragment
    ))
  }
  if (!is.na(r$authority)) {
    authority <- r$authority
    path <- ._remove_dot_segments(r$path)
    query <- r$query
  } else {
    authority <- b$authority
    if (nzchar(r$path)) {
      if (startsWith(r$path, "/")) {
        path <- ._remove_dot_segments(r$path)
      } else {
        path <- ._remove_dot_segments(
          .merge_ref_path(b$authority, b$path, r$path)
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
.recompose_uri <- function(t) {
  out <- ""
  if (!is.na(t$scheme)) {
    out <- paste0(out, t$scheme, ":")
  }
  if (!is.na(t$authority)) {
    out <- paste0(out, "//", t$authority)
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

# Resolve ONE (reference, base) pair to a raw absolute URI string, or NA when
# resolution cannot yield an absolute URL (base not absolute and reference not
# absolute either). ._remove_dot_segments happens inside .transform_reference().
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
    return(.recompose_uri(.transform_reference(r, empty_base)))
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
  .recompose_uri(.transform_reference(r, b))
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
#' }
#'
#' The \code{NULL} selector is frozen and unaffected (ADR 0007; P2.7 D-C):
#' every rule above is reachable only through
#' \code{url_standard = "whatwg"}.
#'
#' One further rule applies under \strong{both} named profiles, because the two
#' standards agree on it. A scheme is
#' \code{ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )} -- RFC 3986 section 3.1's
#' own grammar, and WHATWG's -- so a relative path whose first segment merely
#' \emph{contains} a colon is a path, not an absolute reference:
#' \code{resolve_url("[61:24:74]:98", "http://example.org/foo/bar",
#' url_standard = "whatwg", output = "serialized")} is
#' \code{"http://example.org/foo/[61:24:74]:98"}, and \code{"rfc3986"} merges
#' the same way. The \code{NULL} selector instead keeps RFC 3986 Appendix B's
#' explicitly \emph{non-validating} \code{[^:/?#]+}, which reads
#' \code{"10.0.0.7"} as a scheme and discards the base; that is frozen behavior
#' (ADR 0007), not a recommendation.
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
#'   \code{"//host/p"}) or already absolute (\code{"https://host/p"}); an
#'   absolute reference ignores \code{base_url}.
#' @param base_url A character vector of base URLs, recycled against
#'   \code{relative_or_absolute}. Each base must itself be an absolute URL
#'   (carry a scheme); a relative reference resolved against a scheme-less or
#'   \code{NA} base yields \code{NA}.
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
#'   errors, exactly as it does for \code{\link{safe_parse_url}}. These are
#'   presentation dials consumed by the \code{"clean"} path only; supplying any
#'   of them together with \code{output = "serialized"} is an error, because
#'   \code{\link{serialize_url}} takes no presentation arguments and the dial
#'   could not be honored.
#' @return A character vector the same length as the recycled inputs, unnamed
#'   (names are not data). Under \code{output = "clean"} each element is the
#'   canonical \code{clean_url} of the resolved reference; under
#'   \code{output = "serialized"} it is the standard's full-string
#'   serialization of the resolved absolute URL. \code{NA} where resolution
#'   cannot produce an absolute URL, or where the resolved URL is not accepted
#'   by the parser (\code{"clean"}) or by the selected standard's parser
#'   (\code{"serialized"}).
#' @seealso \code{\link{safe_parse_url}}, \code{\link{get_clean_url}},
#'   \code{\link{serialize_url}}
#' @export
#' @examples
#' resolve_url("../g", "http://a/b/c/d;p?q") # -> "http://a/b/g"
#' resolve_url("g", "http://a/b/c/d;p?q") # -> "http://a/b/c/g"
#' resolve_url("//example.org/p", "http://a/b/c") # -> "http://example.org/p"
#' resolve_url("https://x.com/y", "http://a/b/c") # absolute ref, base ignored
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

  ref <- as.character(relative_or_absolute)
  base <- as.character(base_url)

  # A zero-length operand yields a zero-length result, as R vectorized ops do.
  if (length(ref) == 0L || length(base) == 0L) {
    return(character(0))
  }
  # Recycle both inputs to the common length (base_url is commonly scalar).
  n <- max(length(ref), length(base))
  ref <- rep_len(ref, n)
  base <- rep_len(base, n)

  resolved_raw <- vapply(
    seq_len(n),
    function(i) .resolve_one_raw(ref[[i]], base[[i]], url_standard),
    character(1)
  )

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
