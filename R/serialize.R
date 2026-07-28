# Output surface (b) -- the public entry point for the full-string standard
# serializers (FSSS). The serializers themselves live in R/parse-phases.R
# (.serialize_whatwg_full_vec / .serialize_rfc_full_vec); everything here is the
# build of the LOSSLESS SERIALIZER-INPUT RECORD they consume, plus the exported
# wrapper.
#
# Surface discipline (design/work/url-v3/contracts/output-contracts.md):
#
#   * This is NOT `clean_url`. `clean_url` is surface (c), a policy-driven
#     SEO/canonicalization product that is explicitly "not a serializer,
#     identity, redirect target, or conformance oracle" (P2.2 sec 1c/5.1). The
#     two surfaces are separate by contract (C-04), not two versions of one
#     thing, and this file never calls the clean assembly path.
#   * NO PRESENTATION DIAL reaches this surface (C-05, ADR 0011).
#     `serialize_url` takes no `port_handling`, `trailing_slash_handling`,
#     `path_encoding`, `www_handling`, `index_page_handling` or query-filter
#     argument, and the record builder below pins every one of them to its
#     identity value. The
#     only knobs are `standard` and `form`, which select WHICH standard's
#     serialization is being asked for -- not how to dress it up.
#   * The record is built from the PARSE state, never from the 18-field public
#     projection (S3-F2). `.blank_to_na()` collapses "" to NA on the way out of
#     Stage A, which destroys the empty-vs-absent distinction a full-string
#     serializer needs, so the three-valued presence facts are recovered
#     lexically from the source instead (.fsss_source_lex below).

# Parse posture per standard. Both are EXISTING public profiles, deliberately:
# the FSSS serializes what the standard's own parser accepted, so the parse
# posture must be the standard's, not rurl's web-cleaning default.
#
#   whatwg     -> scheme_acceptance "general", scheme_policy "require"
#                 (absolute-URL, no-base spec posture; scheme-less input is a
#                 parse failure under WHATWG without a base URL, so `require`
#                 is the conformant reading, not a foot-gun).
#   rfc-syntax -> the same acceptance/policy with no normalization at all, which
#                 is what `form = "source"` needs. `form = "normalized"` uses
#                 the SAME parse posture and applies RFC 3986 sec 6.2.2/6.2.3
#                 inside the serializer, so the two forms differ only in
#                 normalization and never in what was accepted.
.FSSS_PROFILE <- c(whatwg = "whatwg", rfc3986 = "rfc-syntax")

# Build the `.parse_options()` bundle for a standard, mirroring how the public
# `profile =` seam expands (.merge_profile_args): profile knobs become
# arguments, `path_identity` is handed over as `profile_authorized` so it
# survives the url_standard profile expansion, and nothing else is supplied --
# every presentation dial keeps its identity default.
.fsss_parse_options <- function(standard, engine = NULL) {
  bundle <- .URL_PROFILES[[.FSSS_PROFILE[[standard]]]]
  authorized <- bundle[intersect(
    names(bundle), c("path_normalization", "path_identity")
  )]
  args <- bundle[setdiff(names(bundle), "path_identity")]
  args$engine <- engine
  if (length(authorized) > 0L) {
    args$profile_authorized <- authorized
  }
  do.call(.parse_options, args)
}

# Lexical recovery of the facts Stage A cannot carry.
#
# Two of the record's fields are DELIMITER facts, not value facts, and Stage A
# reports both through `.blank_to_na()`-collapsed values: `http://h/?` and
# `http://h/` both arrive with `raw_query = NA`. `query_kind`/`fragment_kind`
# are three-valued precisely so a present-but-empty delimiter survives
# (ADR 0012 D2), so they are read off the source string here. The undivided
# `userinfo` slice is recovered the same way -- the web-route parser hands back
# a `user`/`password` split, and RFC 3986 has no such split to reconstruct
# from -- and so is the `host` slice, which that parser percent-decodes and
# case-folds during the parse (RURL-xkhbhaje).
#
# The WHATWG strips (tab/LF/CR removal, then the leading/trailing
# C0-control-or-space run) are applied only under `whatwg`, matching
# `._prepare_urls_vec`; under `rfc3986` the source is read verbatim, which is
# what the source-preserving posture requires.
# The host of a userinfo-free authority: an unterminated "[" keeps everything
# (there is no IP-literal to delimit), a terminated one stops at "]", and any
# other authority drops the last ":"-introduced run, which is the port.
#
# Located AND cut in BYTES (RURL-kmpnbvdl). Shared with the parse-side shim, so
# both sides of the seam have to survive the same input: an authority captured
# by stringi arrives DECLARED UTF-8 whatever its octets, and character-indexed
# slicing throws `invalid multibyte string` on `<80>:80`. Byte indices are also
# the only ones that agree with the locating scan under `LC_ALL=C`.
.fsss_host_slice <- function(s) {
  if (startsWith(s, "[")) {
    close <- .first_byte_index(s, "]")
    return(if (close > 0L) .byte_substring(s, 1L, close) else s)
  }
  pos <- .last_byte_index(s, ":")
  if (pos == 0L) {
    return(s)
  }
  .byte_substring(s, 1L, pos - 1L)
}

.fsss_source_lex <- function(url, url_standard) {
  n <- length(url)
  if (n == 0L) {
    return(list(
      query_kind = character(0), fragment_kind = character(0),
      userinfo = character(0), host = character(0)
    ))
  }
  u <- ifelse(is.na(url), "", as.character(url))
  if (.is_whatwg(url_standard)) {
    u <- .strip_whatwg_control_chars_vec(u, url_standard)$url
    u <- stringi::stri_replace_first_regex(u, "^[\\u0000-\\u0020]+", "")
    u <- stringi::stri_replace_last_regex(u, "[\\u0000-\\u0020]+$", "")
    # Reverse-solidus rewrite, exactly as the parser performs it before reading
    # the authority. Lexing the RAW source instead would let a "\" that the
    # standard maps to the authority/path boundary stay inside the authority
    # slice, so the text before the last "@" -- which is PATH -- would be
    # recovered as a phantom userinfo the parser never found
    # (`http://google.com:80\@yahoo.com`: host `google.com`, path `/@yahoo.com`,
    # no credentials).
    u <- .rewrite_whatwg_backslashes_vec(u, url_standard)$url
  }

  # Located AND cut in BYTES (RURL-kmpnbvdl), like the authority slice below.
  # `regexpr()` without `useBytes` WARNS on a source holding invalid octets and
  # takes a different path per locale to reach it -- silent under `LC_ALL=C`,
  # noisy under UTF-8 -- which is not a property the conformance oracle should
  # have, whichever answer it lands on.
  fragment <- rep(NA_character_, n)
  body <- u
  hash <- .first_byte_index_vec(u, "#")
  hit <- hash > 0L
  fragment[hit] <- .byte_substring_vec(u[hit], hash[hit] + 1L)
  body[hit] <- .byte_substring_vec(u[hit], 1L, hash[hit] - 1L)

  query <- rep(NA_character_, n)
  qmark <- .first_byte_index_vec(body, "?")
  qhit <- qmark > 0L
  query[qhit] <- .byte_substring_vec(body[qhit], qmark[qhit] + 1L)
  hier <- body
  hier[qhit] <- .byte_substring_vec(body[qhit], 1L, qmark[qhit] - 1L)

  # Authority payload: everything after `scheme://` up to the path start. The
  # userinfo is what precedes the LAST "@" in it -- last, not first, because
  # "@" is legal (percent-encoded or not) inside a userinfo but terminates it
  # only once, at the host boundary.
  authority <- stringi::stri_match_first_regex(
    hier, "^[A-Za-z][A-Za-z0-9+.\\-]*://([^/]*)"
  )[, 2L]
  #
  # The host slice is recovered here too, and for the same reason: the parse
  # percent-decodes and case-folds the host before this point, so Stage
  # A's `final_host` cannot answer "which bytes did the source spell here?" --
  # which is exactly what RFC 3986's source-preserving posture emits, and what
  # section 6.2.2.2 normalization must be applied TO rather than after
  # (RURL-xkhbhaje). Host = the authority after any userinfo, minus a trailing
  # ":port"; a bracketed IPv6 literal keeps its "[...]" and its inner colons.
  #
  # Both slices are cut by POSITION rather than by an anchored regex. A regex
  # "." does not match a line terminator and "$" matches before a trailing one,
  # so an authority carrying a raw NEL/LS/PS -- all of which reach here, since
  # the RFC posture strips nothing -- would keep its userinfo and emit it twice.
  # The position is a BYTE position (RURL-kmpnbvdl). `authority` comes from
  # `stri_match_first_regex()`, which declares every capture UTF-8 whatever
  # octets it holds, so a source carrying a stray `<80>` made `substring()`
  # throw `invalid multibyte string` and took the whole `serialize_url()` call
  # -- the FSSS conformance oracle -- down with it. Bytes also keep the locate
  # and the cut in the same unit, which is what makes the slice agree with
  # itself under `LC_ALL=C`.
  userinfo <- rep(NA_character_, n)
  host <- rep(NA_character_, n)
  has_auth <- !is.na(authority)
  if (any(has_auth)) {
    auth <- authority[has_auth]
    at <- vapply(
      auth, .last_byte_index, integer(1), ch = "@", USE.NAMES = FALSE
    )
    slice <- rep(NA_character_, length(auth))
    found <- at > 0L
    slice[found] <- vapply(
      which(found),
      function(i) .byte_substring(auth[i], 1L, at[i] - 1L),
      character(1), USE.NAMES = FALSE
    )
    userinfo[has_auth] <- slice

    host[has_auth] <- vapply(
      seq_along(auth),
      function(i) .fsss_host_slice(.byte_substring(auth[i], at[i] + 1L)),
      character(1), USE.NAMES = FALSE
    )
  }

  list(
    query_kind = .presence_kind(query),
    fragment_kind = .presence_kind(fragment),
    userinfo = userinfo,
    host = host
  )
}

# The lossless serializer-input record for a character vector of URLs, under one
# standard. Returns a columnar list plus `ok`, the mask of rows that parsed.
#
# Every transform applied here is one the SELECTED STANDARD's parser performs;
# none is a cleaning or presentation step:
#
#   * host -- WHATWG's host parser emits the ASCII (punycode) domain, lower-
#     cased, so `host_encoding = "idna"` is the standard's own rendering, not
#     the `clean_url` default (`keep`, which leaves the Unicode spelling).
#     Applied only to hosts D2 makes transform-eligible: an opaque general host
#     is not a domain and is never routed through domain.R (ADR 0002).
#   * path -- WHATWG's parser resolves dot segments during parsing. RFC keeps
#     the source path; `form = "normalized"` removes dot segments inside the
#     serializer (sec 6.2.2.3), where it belongs.
#   * port -- WHATWG's parser sets the port to null when it equals the scheme's
#     default, so the record carries no default port. This is a PARSE fact,
#     which is why the serializer emits whatever port it is handed. RFC keeps
#     it; the normalized form elides it per sec 6.2.3.
.fsss_record_vec <- function(url, standard, engine = NULL) {
  n <- length(url)
  is_whatwg <- identical(standard, "whatwg")
  opts <- .fsss_parse_options(standard, engine)

  a <- ._parse_stage_a_vec(url, opts)
  ok <- !attr(a, "null_row")

  lex <- .fsss_source_lex(url, opts$url_standard)

  # The general-routed rows' true state kinds. Stage A already installed their
  # COMPONENTS, but not the kinds (they are not cached Stage-A fields), so the
  # pure parser is re-run on the same stripped input Stage A fed it.
  gen <- .general_parse_vec(
    .strip_whatwg_control_chars_vec(url, opts$url_standard)$url,
    opts$url_standard, "general"
  )
  gp <- gen$general_parsed & ok

  scheme_lc <- .ascii_tolower(a$final_scheme)
  is_special <- !is.na(scheme_lc) & scheme_lc %in% .WHATWG_SPECIAL_SCHEMES
  is_ip_host <- .detect_ip_host_vec(a$final_host)

  path_kind <- .whatwg_path_kind(is_special, a$raw_path)
  host_kind <- .host_kind(a$final_host)
  rfc_path_form <- .rfc_path_form(!is.na(a$final_host), a$raw_path)
  # The authority test runs on the backslash-rewritten source for the same
  # reason the lexer does: WHATWG's special-authority-ignore-slashes state
  # accepts a "\"-bearing run as the authority introducer, so
  # `https:/\/\/\github.com/foo/bar` HAS an authority (host `github.com`) even
  # though it carries no literal "://". Testing the raw source dropped the host.
  # The strip must run BEFORE the rewrite, in the parser's own order: the
  # rewrite is anchored on the scheme, so a leading C0-or-space would make it a
  # no-op and the host would still be dropped.
  authority_delimiter_present <- .has_explicit_authority(
    .rewrite_whatwg_backslashes_vec(
      .strip_whatwg_control_chars_vec(
        ifelse(is.na(url), "", as.character(url)), opts$url_standard
      )$url,
      opts$url_standard
    )$url,
    opts$url_standard
  )
  if (any(gp)) {
    path_kind[gp] <- gen$path_kind[gp]
    path_kind[gp & is.na(path_kind)] <- "list"
    host_kind[gp] <- gen$host_kind[gp]
    rfc_path_form[gp] <- gen$rfc_path_form[gp]
    authority_delimiter_present[gp] <- gen$authority_delimiter_present[gp]
  }

  elig <- .stage_b_eligibility(
    "general", a$final_scheme, opts$url_standard,
    path_kind = path_kind, host_kind = host_kind, is_ip_host = is_ip_host
  )

  host <- a$final_host
  path <- a$raw_path
  port <- a$raw_port
  if (is_whatwg) {
    ascii_host <- .apply_host_encoding_vec(host, "idna", is_ip_host, "whatwg")
    keep <- elig$host_transform_eligible & !is.na(ascii_host)
    host[keep] <- .ascii_tolower(ascii_host[keep])

    dotted <- .normalize_path_vec(
      path,
      path_encoding = "keep",           # C-05: never consulted by surface (b)
      path_normalization = "dot_segments",
      index_page_handling = "keep",     # cleaning, not parsing
      trailing_slash_handling = "none", # presentation, not parsing
      path_identity = ".whatwg_preserve"
    )
    path[elig$path_eligible] <- dotted[elig$path_eligible]

    default_port <- .scheme_default_port_vec(a$final_scheme)
    port[!is.na(port) & !is.na(default_port) & port == default_port] <-
      NA_character_
  } else {
    # RFC 3986 has no host-decoding or host-case PARSE step: section 6.2.2.1
    # and 6.2.2.2 are NORMALIZATION rules, which belong to `form =
    # "normalized"` inside the serializer -- where the other four components'
    # already are -- not to the parse. The web-route parser performs both
    # anyway, so the record takes the source spelling instead of `final_host`
    # (RURL-xkhbhaje).
    # Substituted only where the source and the parse AGREE that a host is
    # there: same NA-ness and same emptiness. They disagree when the parse read
    # a host the authority slice does not hold (`http:///p`, whose host the
    # parser reads as `p` out of the path), and swapping one component of a
    # disagreeing
    # pair while the rest of the record stays Stage A's would emit a string
    # neither of them describes.
    src_host <- lex$host
    take <- !is.na(host) & !is.na(src_host) & (nzchar(host) == nzchar(src_host))
    host[take] <- src_host[take]
  }

  list(
    ok = ok,
    scheme = scheme_lc,
    userinfo = lex$userinfo,
    host = host,
    host_kind = host_kind,
    authority_delimiter_present = authority_delimiter_present,
    path = path,
    path_kind = path_kind,
    rfc_path_form = rfc_path_form,
    query = a$raw_query,
    query_kind = lex$query_kind,
    fragment = a$raw_fragment,
    fragment_kind = lex$fragment_kind,
    port = port
  )
}

#' Serialize URLs to a standard's own full-string form
#'
#' Renders each URL as the selected standard would serialize it: the
#' **full string**, credentials and fragment included, with a present-but-empty
#' `?` or `#` delimiter preserved. This is rurl's *standard serialization*
#' surface, and it is deliberately **not** [get_clean_url()].
#'
#' @section Which surface you want:
#'
#' `serialize_url()` answers *"what does this URL look like under the URL
#' Standard / RFC 3986?"*. [get_clean_url()] answers *"what is the canonical,
#' tidied form of this URL for SEO or deduplication?"*. They are different
#' products, not two settings of one:
#'
#' * `serialize_url()` preserves userinfo, the fragment, and empty delimiters;
#'   takes **no** presentation options; and is the substrate rurl's conformance
#'   claims are measured on.
#' * [get_clean_url()] drops userinfo and the fragment by design, and is driven
#'   by cleaning policy (`www_handling`, `trailing_slash_handling`,
#'   `index_page_handling`, query filtering, port handling, and so on).
#'
#' Because a standard serialization is an *identity*, `serialize_url()` accepts
#' no presentation arguments at all. There is no `port_handling`,
#' `trailing_slash_handling` or `path_encoding` to pass; asking a serializer to
#' strip a trailing slash would be a category error.
#'
#' @section Parse posture:
#'
#' Each standard is parsed under its own spec posture -- the `"whatwg"` and
#' `"rfc-syntax"` profiles (see [url_profile()]). Both accept any scheme, and
#' both **require** a scheme: neither standard defines a base-URL-free parse of
#' `example.com/x`, so scheme-less input returns `NA` rather than being silently
#' upgraded to `https://`. Input that the standard's parser rejects also returns
#' `NA`.
#'
#' @section Standards and forms:
#'
#' \describe{
#'   \item{`standard = "whatwg"`}{The WHATWG URL Standard's URL serializer
#'     (`#concept-url-serializer`). Spec-exact, which makes credentials lossy in
#'     one direction: WHATWG appends credentials only when the username or
#'     password is non-empty, so `http://@h/` serializes as `http://h/` and
#'     `http://u:@h/` as `http://u@h/`. Both are pinned by the Web Platform
#'     Tests. `form` is ignored.}
#'   \item{`standard = "rfc3986", form = "source"`}{RFC 3986 section 5.3
#'     component recomposition with **no** normalization: source bytes are
#'     preserved and the undivided `userinfo` slice is emitted verbatim (RFC
#'     3986 has no username/password split), so every credential spelling `u@`,
#'     `u:@`, `:p@`, `@` -- survives.}
#'   \item{`standard = "rfc3986", form = "normalized"`}{Adds RFC 3986 section
#'     6.2.2 syntax-based normalization (scheme and host case, percent-encoding
#'     triplet case and unreserved-octet decoding, dot-segment removal) and the
#'     section 6.2.3 default-port elision.}
#' }
#'
#' Both RFC forms are exposed because choosing one would forfeit either the
#' round-trip oracle (`source`) or the normalized comparison substrate
#' (`normalized`).
#'
#' @param url A character vector of URLs.
#' @param standard The standard to serialize to: `"whatwg"` (default) or
#'   `"rfc3986"`.
#' @param form For `standard = "rfc3986"` only, the RFC posture: `"source"`
#'   (default, source-preserving) or `"normalized"`. Ignored for `"whatwg"`,
#'   whose serializer has a single spec-defined form.
#' @param engine Optional `psl_engine` object from `pslr::psl_engine()` for
#'   per-request Public Suffix List resolution. `NULL` (default) uses the
#'   session-global engine.
#'
#' @return A character vector the same length as `url`. `NA_character_` for
#'   input the selected standard's parser does not accept.
#'
#' @seealso [get_clean_url()] for the cleaning surface, [safe_parse_url()] for
#'   the parsed components, and [url_profile()] for the parse postures used
#'   here.
#'
#' @examples
#' # The fragment and credentials survive; clean_url drops both by design.
#' serialize_url("http://user:pw@Example.COM:80/a/../b?q=1#frag")
#' get_clean_url("http://user:pw@Example.COM:80/a/../b?q=1#frag")
#'
#' # A present-but-empty delimiter carries information and is preserved.
#' serialize_url(c("http://example.com/", "http://example.com/#",
#'                 "http://example.com/?"))
#'
#' # RFC 3986: source-preserving versus normalized.
#' serialize_url("HTTP://Example.COM:80/a/%7Euser/../x", standard = "rfc3986")
#' serialize_url("HTTP://Example.COM:80/a/%7Euser/../x", standard = "rfc3986",
#'               form = "normalized")
#'
#' # Any scheme is accepted; no scheme is not.
#' serialize_url(c("urn:ietf:rfc:2648", "mailto:a@b.com", "foo://h/x"))
#' serialize_url("example.com/x")
#'
#' @export
serialize_url <- function(url,
                          standard = c("whatwg", "rfc3986"),
                          form = c("source", "normalized"),
                          engine = NULL) {
  standard <- match.arg(standard)
  form <- match.arg(form)
  engine <- .validate_engine(engine)

  # Input coercion matches `safe_parse_urls()`: factors parse as their labels,
  # names are not data (RURL-vhdsqaln), and a non-character (or non-scalar
  # list) element is simply not parseable and becomes NA.
  if (is.factor(url)) {
    url <- as.character(url)
  }
  if (!is.null(names(url))) {
    url <- unname(url)
  }
  n <- length(url)
  if (n == 0L) {
    return(character(0))
  }
  if (!is.character(url)) {
    url <- vapply(
      as.list(url),
      function(u) if (is.character(u) && length(u) == 1L) u else NA_character_,
      character(1)
    )
  }

  rec <- .fsss_record_vec(url, standard, engine)
  out <- rep(NA_character_, n)
  keep <- which(rec$ok)
  if (length(keep) == 0L) {
    return(out)
  }

  slice <- function(x) x[keep]
  out[keep] <- if (identical(standard, "whatwg")) {
    .serialize_whatwg_full_vec(
      scheme = slice(rec$scheme), userinfo = slice(rec$userinfo),
      host = slice(rec$host), host_kind = slice(rec$host_kind),
      authority_delimiter_present = slice(rec$authority_delimiter_present),
      path = slice(rec$path), path_kind = slice(rec$path_kind),
      query = slice(rec$query), query_kind = slice(rec$query_kind),
      fragment = slice(rec$fragment), fragment_kind = slice(rec$fragment_kind),
      port = slice(rec$port)
    )
  } else {
    .serialize_rfc_full_vec(
      scheme = slice(rec$scheme), userinfo = slice(rec$userinfo),
      host = slice(rec$host), host_kind = slice(rec$host_kind),
      authority_delimiter_present = slice(rec$authority_delimiter_present),
      path = slice(rec$path), rfc_path_form = slice(rec$rfc_path_form),
      query = slice(rec$query), query_kind = slice(rec$query_kind),
      fragment = slice(rec$fragment), fragment_kind = slice(rec$fragment_kind),
      port = slice(rec$port), form = form
    )
  }
  out
}
