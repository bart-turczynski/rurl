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
    close <- .bytes_locate(s, "]", fixed = TRUE)[[1L, "start"]]
    return(if (!is.na(close)) .bytes_cut(s, 1L, close) else s)
  }
  pos <- .last_byte_index(s, ":")
  if (pos == 0L) {
    return(s)
  }
  .bytes_cut(s, 1L, pos - 1L)
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
    # Leading and trailing C0 control or space (WHATWG URL, basic URL parser
    # step 1), in byte mode (ADR 0019). The end anchor is `\z`: ICU's `$`
    # also matched before a final U+0085 or U+2028, so a run of spaces in
    # front of one was stripped although neither code point is a C0 control
    # or space.
    u <- .bytes_replace(u, "\\A[\\x00-\\x20]+", "")
    u <- .bytes_replace(u, "[\\x00-\\x20]+\\z", "")
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
  hash <- .bytes_locate(u, "#", fixed = TRUE)[, "start"]
  hit <- !is.na(hash)
  fragment[hit] <- .bytes_cut(u[hit], hash[hit] + 1L, .Machine$integer.max)
  body[hit] <- .bytes_cut(u[hit], 1L, hash[hit] - 1L)

  query <- rep(NA_character_, n)
  qmark <- .bytes_locate(body, "?", fixed = TRUE)[, "start"]
  qhit <- !is.na(qmark)
  query[qhit] <- .bytes_cut(body[qhit], qmark[qhit] + 1L, .Machine$integer.max)
  hier <- body
  hier[qhit] <- .bytes_cut(body[qhit], 1L, qmark[qhit] - 1L)

  # Authority payload: everything after `scheme://` up to the path start. The
  # userinfo is what precedes the LAST "@" in it -- last, not first, because
  # "@" is legal (percent-encoded or not) inside a userinfo but terminates it
  # only once, at the host boundary.
  # Byte mode (ADR 0019): `[^/]` takes every byte but "/", as ICU's negated
  # class took every code point but "/", line terminators included.
  authority <- .bytes_capture(
    hier, "\\A[A-Za-z][A-Za-z0-9+.\\-]*+://([^/]*+)"
  )[, 2L]
  # A source led by U+FEFF has no scheme: a scheme starts with an ASCII letter
  # (RFC 3986 section 3.1; WHATWG scheme start state), and neither standard
  # strips the mark. The byte-mode capture reads `hier`, after the WHATWG
  # strip, which can leave the mark in front (`" \ufeffhttp://h/"`), and finds
  # no scheme there, so it needs no mask (ADR 0005's guard, RURL-rynmvtwp,
  # retired by ADR 0019, RURL-dddxutjf). The rest of this function needs no
  # guard either: the control-character strip is a guarded helper (ADR 0005),
  # the backslash rewrite runs in byte mode, and every locate and cut below
  # is base R on bytes.
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
  # The position is a BYTE position (RURL-kmpnbvdl). `authority` is declared
  # UTF-8 whatever octets it holds (it came from `stri_match_first_regex()`
  # then, from `.bytes_capture()` now), so a source carrying a stray `<80>`
  # made `substring()`
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
      function(i) .bytes_cut(auth[i], 1L, at[i] - 1L),
      character(1), USE.NAMES = FALSE
    )
    userinfo[has_auth] <- slice

    host[has_auth] <- vapply(
      seq_along(auth),
      function(i) {
        .fsss_host_slice(.bytes_cut(auth[i], at[i] + 1L, .Machine$integer.max))
      },
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
#
# `opts` exists ONLY so a second identity surface can reuse this builder under a
# DIFFERENT parse posture without duplicating it. Surface (b) never passes it
# and is byte-for-byte unaffected; surface (e), the comparison key, passes its
# bundle because the FSSS posture (`scheme_policy = "require"`) makes every
# scheme-less input a parse failure, which would leave the key's missing-scheme
# truth-table rows unreachable (`contracts/key-join-contracts.md:112-115`).
# Porting the lexical recovery and the general-parse kind merge into a private
# copy is the alternative, and it is the one that silently narrows.
.fsss_record_vec <- function(url, standard, engine = NULL, opts = NULL) {
  # The record reads `url` itself, not through safe_parse_urls()'s input
  # seams, so it owns the input boundary for serialize_url(), format_url() and
  # the key (RURL-cwzickqy): a bytes-marked element is a row that does not
  # parse, where it used to throw out of the whole call.
  url <- .prepare_url_input(url)$url
  n <- length(url)
  is_whatwg <- identical(standard, "whatwg")
  if (is.null(opts)) {
    opts <- .fsss_parse_options(standard, engine)
  }

  a <- ._parse_stage_a_vec(url, opts)
  ok <- !attr(a, "null_row")

  lex <- .fsss_source_lex(url, opts$url_standard)

  # The general-routed rows' true state kinds. Stage A already installed their
  # COMPONENTS, but not the kinds (they are not cached Stage-A fields), so the
  # pure parser is re-run here, on the string Stage A gave it:
  # .general_route_input_vec(), the browser fixer, then the WHATWG strip
  # (RURL-vmsmlflr), then the `http:` or `https:` prefix for a `//` row under
  # rfc3986 with "keep", "http" or "https" (RURL-ozpxupgq, RURL-vubwgoxo).
  # Reading only the strip here missed that prefix, so `///p` was never
  # claimed as general-routed.
  gen <- .general_parse_vec(
    .general_route_input_vec(url, opts),
    opts$url_standard, "general", opts$scheme_policy
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
  # A `//` row's authority delimiter is the `//` that starts it (RFC 3986
  # section 4.2, `"//" authority path-abempty`). `.has_explicit_authority()`
  # looks for `scheme://`, which a scheme-less row never spells, so it
  # reported every `//host` row as having no authority (RURL-ozpxupgq).
  scheme_relative <- a$is_scheme_relative
  authority_delimiter_present[scheme_relative] <- TRUE

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

  # Under "keep" a `//` row was parsed behind a stand-in `http:` the source
  # never spelled, and the parse surface already reports its scheme as NA
  # (Phase 13). The record says the same, so the key frames no scheme for a
  # network-path reference, where it framed the stand-in (RURL-ozpxupgq). Read
  # only after the path kind above, which takes the stand-in's special-ness:
  # a reference with an authority has a list path, never an opaque one.
  stand_in <- scheme_relative &
    identical(opts$scheme_relative_handling, "keep")
  scheme_lc[stand_in] <- NA_character_
  source_scheme <- a$source_scheme
  source_scheme[stand_in] <- NA_character_

  list(
    ok = ok,
    scheme = scheme_lc,
    # The scheme AS SPELLED, read by `form = "source"` alone (RUL-007): the
    # key and join surfaces keep reading the folded `scheme` above.
    source_scheme = source_scheme,
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
    port = port,
    # The port AS SPELLED, before the WHATWG default-port elision above. Surface
    # (b) must not use it -- nulling a special scheme's default port is a WHATWG
    # PARSE fact and the serializer emits what it is handed. Surface (e) must:
    # key-policy v1 normalizes ONLY http:80 and https:443, so `ftp://h:21/` and
    # `ws://h:80/` stay DISTINCT from their portless spellings
    # (`contracts/key-join-contracts.md:116-117`, P3.1 ratification Q8) even
    # though WHATWG's parser elides all five defaults.
    syntactic_port = a$raw_port,
    # TRUE where the source is a `//host` network-path reference. A posture
    # with `scheme_relative_handling = "keep"` (the `rfc-syntax` profile)
    # parses such a row behind a stand-in `http:` prefix, and `scheme` above is
    # NA for it. Surface (b) reads this to refuse the row (RURL-xjyrhqin), as
    # a relative reference has no absolute form to serialize; surface (e)
    # frames the row's scheme presence itself and does not read it.
    scheme_relative = scheme_relative
  )
}

# The record as the selected standard's URL record holds it: the one step that
# percent-encodes the components the URL key shares with the serializer
# (RURL-ommuyaqi). Both read its output -- `serialize_url()` and
# `.url_key_state_vec()` -- so the key frames the path and query as the
# serialization spells them. The WHATWG serializer still re-applies the
# same idempotent encoders to this output, which changes no byte; making it
# read the step alone needs an edit in R/parse-phases.R.
#
# Under `whatwg` the parser encodes as it reads: the path state encodes a list
# path with the path percent-encode set, the opaque path state uses the
# C0-control set (and `%20` for a space right before the `?` or `#` that ends
# the path), and the query state uses the query set, or the special-query set
# under a special scheme. `.fsss_record_vec()` keeps a list path and the
# query as written, so this step applies the same encoders
# (R/path-query.R) the serializer does; the record itself stays lossless for
# `format_url()`, which escapes for display instead. An encoder copies `%` and
# hex digits as written, so it is idempotent: a triplet keeps its hex case
# (RUL-026), and the serializer's own pass over this output changes no byte.
# Each encoder touches only the rows holding an octet it would change.
#
# Under `rfc3986` the record is returned as it came: the source spelling is
# the identity there, and section 6.2.2 normalization belongs to `form =
# "normalized"` inside the serializer, never to the key (RUL-007).
#
# NOT encoded here, and why: the fragment and the userinfo are in no key
# (ratification Q5), and the serializer splits the undivided userinfo at its
# first `:` before encoding each half, which a pre-encoded `:` would defeat.
# The serializer encodes both itself.
.fsss_encode_record_vec <- function(rec, standard) {
  if (!identical(standard, "whatwg")) {
    return(rec)
  }
  path <- rec$path
  kind <- rec$path_kind
  has_path <- !is.na(path) & !is.na(kind)
  list_rows <- which(has_path & kind != "opaque")
  rec$path[list_rows] <- .whatwg_path_percent_encode(path[list_rows])
  # An opaque path reaches the record already encoded (the parser's opaque
  # path state), so this pass is a no-op on it today; it stays so the step,
  # not the parser, is what the key and the serializer rely on.
  opaque_rows <- which(has_path & kind == "opaque")
  if (length(opaque_rows) > 0L) {
    follows <- rec$query_kind[opaque_rows] != "absent" |
      rec$fragment_kind[opaque_rows] != "absent"
    rec$path[opaque_rows] <- .whatwg_opaque_path_encode(
      path[opaque_rows], follows
    )
  }
  # The row's OWN scheme picks the set, never a collapsed key token; a row
  # whose scheme the key's posture inferred reads as that inferred `http`.
  q_rows <- which(!is.na(rec$query))
  rec$query[q_rows] <- .whatwg_query_percent_encode(
    rec$query[q_rows], rec$scheme[q_rows]
  )
  rec
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
#' upgraded to `https://`. That includes a `//host` network-path reference such
#' as `//example.com/x`: it is a relative reference (RFC 3986 section 4.2), not
#' a URI, so it returns `NA` even where the `"rfc-syntax"` parse accepts it.
#' Input that the standard's parser rejects also returns `NA`.
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
#'     section 6.2.3 default-port elision. Dot segments are removed from a path
#'     without a leading `/` too (`foo:a/../b` becomes `foo:/b`), as section
#'     5.2.2 does for any reference with a scheme; the result is equivalent
#'     in the generic syntax, which a scheme's own rules may not share. A path
#'     with no authority keeps its dot segments where removing them would
#'     leave it starting with `//`,
#'     which section 3.3 forbids (`foo:/a/..//p` stays as written rather than
#'     becoming `foo://p`, a URI with the host `p`).}
#' }
#'
#' Both RFC forms are exposed because choosing one would forfeit either the
#' round-trip oracle (`source`) or the normalized comparison substrate
#' (`normalized`).
#'
#' @param url A character vector of URLs.
#' @param standard The standard to serialize to: `"whatwg"` (default) or
#'   `"rfc3986"`. Unlike the parse surface, `NULL` is not accepted: the parse
#'   surface's `url_standard = NULL` is a frozen legacy profile that names no
#'   standard (ADR 0007), so there is nothing to serialize *as*. Passing
#'   `NULL` is an error rather than a silent `"whatwg"`.
#' @param form For `standard = "rfc3986"` only, the RFC posture: `"source"`
#'   (default, source-preserving) or `"normalized"`. Ignored for `"whatwg"`,
#'   whose serializer has a single spec-defined form.
#' @param engine Optional `psl_engine` object from `pslr::psl_engine()` for
#'   per-request Public Suffix List resolution. `NULL` (default) uses the
#'   session-global engine.
#'
#' @return A character vector the same length as `url`. `NA_character_` for
#'   input the selected standard's parser does not accept, and for input with
#'   no scheme, a `//host` network-path reference included (see "Parse
#'   posture").
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
  # `NULL` is rejected rather than absorbed, exactly as `url_key_policy()` does
  # (R/url-key.R). `match.arg(NULL, choices)` quietly returns `choices[[1]]`,
  # which would make `standard = NULL` a silent "whatwg" -- and on the parse
  # surface `url_standard = NULL` means the frozen legacy profile that names
  # no standard at all (ADR 0007). A serialization *as* no standard does not
  # exist, so the mistake is refused at the edge instead of guessed
  # (RURL-ouorolhb).
  if (is.null(standard)) {
    stop(
      "`standard` must be named; NULL is not accepted. ",
      "url_standard = NULL is the frozen legacy parse profile, which names ",
      "no standard to serialize as. Pass standard = \"whatwg\" or \"rfc3986\".",
      call. = FALSE
    )
  }
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

  rec <- .fsss_encode_record_vec(
    .fsss_record_vec(url, standard, engine), standard
  )
  out <- rep(NA_character_, n)
  # A `//host` network-path reference has no scheme: it is a relative
  # reference (RFC 3986 section 4.2), not a URI (section 3), and with no base
  # there is no absolute form to serialize -- the documented NA for scheme-less
  # input. `whatwg` already rejects it at the parse (`scheme_relative_handling
  # = "error"`); the `rfc-syntax` posture parses it behind a stand-in `http:`
  # scheme, which emitted that scheme and, finding no `://` in the source, no
  # authority (`//h/p` gave `http:/p`) (RURL-xjyrhqin).
  keep <- which(rec$ok & !rec$scheme_relative)
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
      # Section 6.2.2.1 folds scheme case in `normalized` only (RUL-007).
      scheme = slice(
        if (identical(form, "source")) rec$source_scheme else rec$scheme
      ),
      userinfo = slice(rec$userinfo),
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

#' @rdname serialize_url
#' @details `serialise_url()` is the British-spelling alias of
#'   `serialize_url()`: the same function under a second name.
#' @export
serialise_url <- serialize_url
