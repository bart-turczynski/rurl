# Parsing phases: the decomposed helpers behind the parse engine.
#
# ---------------------------------------------------------------------------
# Each phase owns one normalization step. The `*_vec` functions are the real,
# vectorized implementation: they accept and return length-n column vectors
# (options are validated scalars per call, so every switch()/if hoists OUTSIDE
# the vector ops -- each phase is straight-line vectorized code selected once).
# The vector engine chains them in two stages (parse.R): ._parse_stage_a_vec()
# runs the option-independent, cacheable phases and ._parse_stage_b_vec() runs
# the presentation phases over the cached Stage A columns.
#
# The scalar helpers of the same base name are kept as thin wrappers that
# delegate to their `*_vec` counterpart on a length-1 input, so the per-phase
# unit tests (test-parse-phases.R) still pin behavior through them and the
# scalar orchestrator ._safe_parse_url_impl() (parse.R) keeps working.
# .parse_with_curl()/.extract_raw_components() stay scalar because curl is
# scalar-only; the engine reuses .parse_with_curl() in its single per-URL loop
# and extracts the raw columns itself.
# ---------------------------------------------------------------------------

# Phase 1 helper (vector): classify the host token of each raw input for the
# host-shape gate. rurl only fabricates a scheme for URL-shaped input, so we
# must isolate and inspect the host before curl sees it. Returns, per element:
#   has_userinfo    - an '@' appears in the authority (user[:pass]@host)
#   is_localhost    - the bare host is an allowlisted single-label host
#   is_dotted_name  - >= 2 non-empty dot-separated labels (example.com, a.b.c)
#   is_ipish        - the host is an IP *attempt*: all-decimal / 0x-hex / octal
#                     / short-form groups, or anything containing ':' (IPv6)
#   is_canonical_ip - a valid IPv4 dotted-quad or bracketed IPv6 per the strict
#                     detector (rejects leading-zero octets and coerced forms)
# Purely a function of the input string; used to drive rejection in Phase 1.
.classify_input_host_vec <- function(url) {
  # Strip a leading "scheme://" or scheme-relative "//", then the path/query/
  # fragment, to isolate the authority (userinfo + host + port).
  s <- stringi::stri_replace_first_regex(url, "^[a-zA-Z][a-zA-Z0-9+.-]*://", "")
  s <- stringi::stri_replace_first_regex(s, "^//", "")
  authority <- stringi::stri_replace_first_regex(s, "[/?#].*$", "")

  has_userinfo <- stringi::stri_detect_fixed(authority, "@")
  has_userinfo[is.na(has_userinfo)] <- FALSE

  # Host = authority after any userinfo, minus the port. Bracketed IPv6 keeps
  # its "[...]"; otherwise drop a single trailing ":port".
  host_ui <- stringi::stri_replace_first_regex(authority, "^.*@", "")
  bracketed <- stringi::stri_startswith_fixed(host_ui, "[")
  bracketed[is.na(bracketed)] <- FALSE
  host_token <- ifelse(
    bracketed,
    stringi::stri_replace_first_regex(host_ui, "^(\\[[^\\]]*\\]).*$", "$1"),
    stringi::stri_replace_first_regex(host_ui, ":[^:]*$", "")
  )

  host_lower <- stringi::stri_trans_tolower(host_token)
  is_localhost <- host_lower %in% .SPECIAL_SINGLE_LABEL_HOSTS

  is_dotted_name <- stringi::stri_detect_regex(host_token, "^[^.]+(\\.[^.]+)+$")
  is_dotted_name[is.na(is_dotted_name)] <- FALSE

  # inet_aton IP attempts: dotted groups each all-decimal or 0x-hex (12345,
  # 0x7f000001, 017700000001, 192.168, 1.2.3.4, 256.1.1.1, 1.2.3.4.5), plus any
  # ':' host (IPv6, incl. bracketed).
  ipv4ish <- stringi::stri_detect_regex(
    host_token, "^(0[xX][0-9a-fA-F]+|[0-9]+)(\\.(0[xX][0-9a-fA-F]+|[0-9]+))*$"
  )
  ipv4ish[is.na(ipv4ish)] <- FALSE
  ipv6ish <- stringi::stri_detect_fixed(host_token, ":")
  ipv6ish[is.na(ipv6ish)] <- FALSE
  is_ipish <- ipv4ish | ipv6ish

  # Canonical per the strict detector (Phase 5): rejects leading-zero octets,
  # out-of-range/wrong-arity, and integer/hex/octal coercion; accepts 1.2.3.4
  # and [::1].
  is_canonical_ip <- .detect_ip_host_vec(host_token)

  list(
    has_userinfo = has_userinfo,
    is_localhost = is_localhost,
    is_dotted_name = is_dotted_name,
    is_ipish = is_ipish,
    is_canonical_ip = is_canonical_ip
  )
}

# Phase 1 (vector): scheme detection, supported-scheme policy, the host-shape
# gate, and building the string handed to curl. Returns the per-URL columns plus
# a logical `rejected` column marking rows the scalar pipeline returned NULL for
# (scheme-relative under "error" handling; a bare unsupported scheme under
# keep/none; a scheme-less input that is not host-shaped (D1); or an IP attempt
# that is not a canonical literal (D2/D3)) and a `scheme_less_userinfo` flag
# (D5). An input's supported scheme is decided against .SUPPORTED_SCHEMES.
.prepare_urls_for_curl_vec <- function(url,
                                       protocol_handling,
                                       scheme_relative_handling) {
  n <- length(url)
  url_lower <- stringi::stri_trans_tolower(url)

  # A scheme-bearing input is "allowed" only if its scheme is one rurl supports
  # (.SUPPORTED_SCHEMES). any(startsWith(., "<scheme>://")) per row, no loop.
  allowed_prefixes <- paste0(.SUPPORTED_SCHEMES, "://")
  original_has_allowed_scheme <- Reduce(
    `|`, lapply(allowed_prefixes, function(p) startsWith(url_lower, p))
  )
  original_has_allowed_scheme[is.na(original_has_allowed_scheme)] <- FALSE

  scheme_match <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):"
  )
  looks_like_protocol <- !is.na(scheme_match[, 2L])

  has_scheme_slashes <- stringi::stri_detect_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):\\/\\/"
  )
  has_scheme_slashes[is.na(has_scheme_slashes)] <- FALSE

  is_scheme_relative <- stringi::stri_startswith_fixed(url, "//")
  is_scheme_relative[is.na(is_scheme_relative)] <- FALSE

  rejected <- rep(FALSE, n)
  if (scheme_relative_handling == "error") {
    rejected <- rejected | is_scheme_relative
  }
  if (scheme_relative_handling %in% c("http", "https")) {
    # Treat scheme-relative URLs as having an inferred scheme for the logic.
    looks_like_protocol[is_scheme_relative] <- TRUE
    original_has_allowed_scheme[is_scheme_relative] <- TRUE
  }

  looks_like_host_port <- rep(FALSE, n)
  maybe_host_port <- looks_like_protocol &
    !original_has_allowed_scheme &
    !has_scheme_slashes
  if (any(maybe_host_port)) {
    lhp <- stringi::stri_detect_regex(url, "^[^/]+:[0-9]+($|/)")
    lhp[is.na(lhp)] <- FALSE
    looks_like_host_port[maybe_host_port] <- lhp[maybe_host_port]
  }

  protocol_kept <- protocol_handling == "keep" || protocol_handling == "none"
  if (protocol_kept) {
    bare_protocol_kept <- looks_like_protocol &
      !original_has_allowed_scheme &
      !looks_like_host_port
    rejected <- rejected | bare_protocol_kept
  }

  # Host-shape classification (D1/D2/D5).
  cls <- .classify_input_host_vec(url)

  # D2/D3: an IP attempt that is not a canonical literal is a coerced/malformed
  # IP. Applies to every row (scheme-bearing too), so http://12345 and
  # http://192.168.010.1 are rejected as well as their scheme-less forms.
  rejected <- rejected | (cls$is_ipish & !cls$is_canonical_ip)

  # Rows that get an inferred http:// (scheme-less, non-scheme-relative).
  add_http <- !is_scheme_relative &
    (!looks_like_protocol | looks_like_host_port)

  # D1: only fabricate a scheme when the token is host-shaped -- a canonical IP,
  # localhost, a dotted name, or an explicit host:port. Otherwise the input is
  # not a URL (asdfghjkl, "hello world", /relative/path, bare 12345).
  host_like <- cls$is_canonical_ip |
    cls$is_localhost |
    cls$is_dotted_name |
    looks_like_host_port
  rejected <- rejected | (add_http & !host_like)

  # D5: scheme-less input carrying userinfo (user@example.com). Not rejected --
  # host/domain/tld/user still resolve -- but Stage B suppresses clean_url and
  # sets warning-userinfo. Only flagged for otherwise-parseable host-like rows.
  scheme_less_userinfo <- add_http & host_like & cls$has_userinfo & !rejected

  url_to_parse <- url
  if (scheme_relative_handling %in% c("http", "https")) {
    url_to_parse[is_scheme_relative] <- paste0(
      scheme_relative_handling, ":", url[is_scheme_relative]
    )
  } else {
    url_to_parse[is_scheme_relative] <- paste0(
      "http:", url[is_scheme_relative]
    )
  }
  url_to_parse[add_http] <- paste0("http://", url[add_http])

  list(
    url_to_parse = url_to_parse,
    looks_like_protocol = looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    is_scheme_relative = is_scheme_relative,
    looks_like_host_port = looks_like_host_port,
    scheme_less_userinfo = scheme_less_userinfo,
    rejected = rejected
  )
}

# Phase 1 (scalar wrapper): returns the per-URL list, or NULL when the URL must
# be rejected. Delegates to .prepare_urls_for_curl_vec().
.prepare_url_for_curl <- function(url,
                                  protocol_handling,
                                  scheme_relative_handling) {
  cols <- .prepare_urls_for_curl_vec(
    url, protocol_handling, scheme_relative_handling
  )
  if (cols$rejected[1L]) {
    return(NULL)
  }
  list(
    url_to_parse = cols$url_to_parse[1L],
    looks_like_protocol = cols$looks_like_protocol[1L],
    original_has_allowed_scheme = cols$original_has_allowed_scheme[1L],
    is_scheme_relative = cols$is_scheme_relative[1L],
    looks_like_host_port = cols$looks_like_host_port[1L],
    scheme_less_userinfo = cols$scheme_less_userinfo[1L]
  )
}

# Phase 2a: parse the prepared URL with curl, returning NULL on failure.
# `decode = FALSE, params = FALSE` are load-bearing: with curl's defaults
# (`decode = TRUE, params = TRUE`) curl percent-decodes the path/query/fragment/
# userinfo before rurl sees them (so `path_encoding = "keep"` could not keep,
# and `%2F` structurally merged path segments) and splits the query into
# decoded params (losing the raw query byte-for-byte). Parsing raw lets rurl own
# every encoding decision downstream. curl never percent-decodes the host.
# curl is scalar-only, so the engine calls this once per URL in its single loop.
.parse_with_curl <- function(url_to_parse) {
  tryCatch(
    curl::curl_parse_url(url_to_parse, decode = FALSE, params = FALSE),
    error = function(e) NULL
  )
}

# Phase 2b: pull the raw components used downstream out of the curl result.
# With `params = FALSE` (see .parse_with_curl) `parsed_curl$query` is already
# the raw (percent-encoded) query string, so it is taken verbatim; downstream
# parsers split on raw "&"/"=" then decode per-pair. scheme/host/path as-is.
.extract_raw_components <- function(parsed_curl) {
  list(
    scheme = parsed_curl$scheme %||% NA_character_,
    host = parsed_curl$host %||% NA_character_,
    path = parsed_curl$path %||% NA_character_,
    # .blank_to_na(): present-but-empty query "" -> NA (libcurl-version stable).
    query = .blank_to_na(parsed_curl$query %||% NA_character_)
  )
}

# Phase 3 (vector): path decoding, slash/dot normalization, index stripping,
# trailing-slash policy, and optional percent-encoding. The genuinely scalar
# steps (RFC 3986 dot-segment resolution, index stripping, segment encoding) run
# via vapply only over the rows that can change (a stri_detect mask), so real
# data pays for them on ~0 rows.
.normalize_path_vec <- function(raw_path,
                                path_encoding,
                                path_normalization,
                                index_page_handling,
                                trailing_slash_handling) {
  path_work <- raw_path

  # Decode path (before normalization/index handling) when requested.
  if (path_encoding %in% c("decode", "encode")) {
    mask <- !is.na(path_work)
    if (any(mask)) {
      decoded <- tryCatch(
        curl::curl_unescape(path_work[mask]),
        error = function(e) NULL
      )
      if (!is.character(decoded) || length(decoded) != sum(mask)) {
        decoded <- vapply(
          path_work[mask],
          function(p) tryCatch(curl::curl_unescape(p), error = function(e) p),
          character(1),
          USE.NAMES = FALSE
        )
      }
      path_work[mask] <- decoded
    }
  }

  # Slash collapsing.
  if (path_normalization %in% c("collapse_slashes", "both")) {
    mask <- !is.na(path_work) & nzchar(path_work)
    path_work[mask] <- gsub("/+", "/", path_work[mask], perl = TRUE)
  }

  # Dot-segment resolution (RFC 3986). ._remove_dot_segments() is identity for
  # paths without a "." / ".." segment, so only those are processed.
  if (path_normalization %in% c("dot_segments", "both")) {
    mask <- !is.na(path_work) & nzchar(path_work) &
      stringi::stri_detect_regex(path_work, "(^|/)\\.{1,2}(/|$)")
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], ._remove_dot_segments, character(1), USE.NAMES = FALSE
      )
    }
  }

  # Index/default page stripping (only where such a trailing segment appears).
  if (index_page_handling == "strip") {
    mask <- !is.na(path_work) & nzchar(path_work) &
      stringi::stri_detect_regex(path_work, "(?i)/(index|default)\\.[^/]+/?$")
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], ._strip_index_page, character(1), USE.NAMES = FALSE
      )
    }
  }

  # Trailing-slash policy (after normalization/index handling).
  if (trailing_slash_handling == "strip") {
    mask <- !is.na(path_work) & nzchar(path_work) & path_work != "/" &
      stringi::stri_endswith_fixed(path_work, "/")
    path_work[mask] <- stringi::stri_sub(
      path_work[mask], 1L, stringi::stri_length(path_work[mask]) - 1L
    )
  } else if (trailing_slash_handling == "keep") {
    mask <- !is.na(path_work) & nzchar(path_work) & path_work != "/" &
      !stringi::stri_endswith_fixed(path_work, "/")
    path_work[mask] <- paste0(path_work[mask], "/")
  }

  # Path percent-encoding (after normalization/index logic).
  if (path_encoding == "encode") {
    mask <- !is.na(path_work)
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], ._encode_path_segments, character(1), USE.NAMES = FALSE
      )
    }
  }

  path_work
}

# Phase 3 (scalar wrapper): delegates to .normalize_path_vec().
.normalize_path <- function(raw_path,
                            path_encoding,
                            path_normalization,
                            index_page_handling,
                            trailing_slash_handling) {
  .normalize_path_vec(
    raw_path, path_encoding, path_normalization,
    index_page_handling, trailing_slash_handling
  )
}

# Phase 4 (vector): resolve the final scheme according to protocol policy.
.derive_final_scheme_vec <- function(protocol_handling,
                                     looks_like_protocol,
                                     raw_scheme) {
  n <- length(looks_like_protocol)
  switch(protocol_handling,
    none = ifelse(looks_like_protocol, raw_scheme, NA_character_),
    strip = rep(NA_character_, n),
    http = rep("http", n),
    https = rep("https", n),
    keep = raw_scheme
  )
}

# Phase 4 (scalar wrapper): delegates to .derive_final_scheme_vec().
.derive_final_scheme <- function(protocol_handling,
                                 looks_like_protocol,
                                 raw_scheme) {
  .derive_final_scheme_vec(protocol_handling, looks_like_protocol, raw_scheme)
}

# Phase 5 (vector): detect whether each host is an IP literal (IPv4 or IPv6).
# NA/"" hosts are FALSE; IPv4 requires four dot-separated CANONICAL octets --
# 0..255 with NO leading zeros (D3: a leading zero is octal in inet_aton, so
# "192.168.010.1" silently means "192.168.8.1"; rurl refuses to guess and treats
# zero-padded octets as non-IP); IPv6 requires balanced brackets and either a
# valid embedded canonical dotted-quad tail or a conservative hex/colon match.
# This is the single strict IP validator, reused by the Phase-1 host-shape gate
# (.classify_input_host_vec) against the input token.
.detect_ip_host_vec <- function(raw_host) {
  n <- length(raw_host)
  valid_h <- !is.na(raw_host) & raw_host != ""

  # Canonical IPv4 octet: 0..255, no leading zeros (rejects 00, 01, 007, 010).
  oct <- "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])"

  # IPv4.
  ipv4 <- rep(FALSE, n)
  ipv4_re <- paste0("^", oct, "(\\.", oct, "){3}$")
  match4 <- stringi::stri_detect_regex(raw_host, ipv4_re)
  match4[is.na(match4)] <- FALSE
  ipv4 <- valid_h & match4

  # IPv6.
  ipv6 <- rep(FALSE, n)
  has_open <- stringi::stri_detect_fixed(raw_host, "[")
  has_close <- stringi::stri_detect_fixed(raw_host, "]")
  balanced <- valid_h & (has_open == has_close)
  balanced[is.na(balanced)] <- FALSE

  tail_match <- stringi::stri_match_first_regex(
    raw_host,
    paste0("^\\[?[0-9a-fA-F:]+:(", oct, "(?:\\.", oct, "){3})\\]?$")
  )
  # The tail regex already enforces canonical octets, so a match is sufficient.
  has_tail <- balanced & !is.na(tail_match[, 1L])
  ipv6[has_tail] <- TRUE

  # Conservative check for balanced hosts without an embedded dotted-quad tail.
  no_tail <- balanced & !has_tail
  if (any(no_tail)) {
    hex_shape <- stringi::stri_detect_regex(raw_host, "^\\[?[0-9a-fA-F:]+\\]?$")
    hex_shape[is.na(hex_shape)] <- FALSE
    has_colon <- stringi::stri_detect_fixed(raw_host, ":")
    has_colon[is.na(has_colon)] <- FALSE
    ipv6[no_tail] <- (hex_shape & has_colon)[no_tail]
  }

  ipv4 | ipv6
}

# Phase 5 (scalar wrapper): delegates to .detect_ip_host_vec().
.detect_ip_host <- function(raw_host) {
  .detect_ip_host_vec(raw_host)
}

# Phase 6 (vector): apply the www-prefix policy to (non-IP) hosts.
.apply_www_policy_vec <- function(raw_host, www_handling, is_ip_host) {
  final_host <- raw_host
  elig <- !is_ip_host & !is.na(raw_host) & raw_host != ""
  if (!any(elig)) {
    return(final_host)
  }
  ci <- stringi::stri_opts_regex(case_insensitive = TRUE)

  if (www_handling == "strip") {
    final_host[elig] <- stringi::stri_replace_first_regex(
      raw_host[elig], "^(www[0-9]*\\.)(.*)", "$2", opts_regex = ci
    )
  } else if (www_handling == "keep") {
    lower <- stringi::stri_trans_tolower(raw_host)
    has_www <- stringi::stri_detect_regex(lower, "^www[0-9]*\\.")
    has_www[is.na(has_www)] <- FALSE
    hw <- elig & has_www
    no_www <- elig & !has_www
    if (any(hw)) {
      match_res <- stringi::stri_match_first_regex(
        raw_host, "^(www[0-9]*\\.)(.*)", opts_regex = ci
      )
      bare_host_part <- match_res[, 3L]
      # Group 3 (.*) always matches once the prefix is confirmed; the fallback
      # is defensive only.
      bare_host_part[is.na(bare_host_part)] <-
        raw_host[is.na(bare_host_part)] # nocov
      final_host[hw] <- paste0("www.", bare_host_part[hw])
    }
    if (any(no_www)) {
      final_host[no_www] <- paste0("www.", raw_host[no_www])
    }
  } else if (www_handling == "if_no_subdomain") {
    final_host[elig] <- .apply_www_if_no_subdomain_vec(raw_host[elig])
  }
  final_host
}

# Phase 6 (www_handling = "if_no_subdomain", vector): add a leading "www." only
# when the host is itself an apex (registrable domain with no subdomain labels).
# The STRUCTURAL decision uses pslr's canonical decomposition (batched over the
# eligible hosts) so an A-label host and its Unicode equivalent take the same
# branch; the emitted host keeps the input spelling (candidate_host).
.apply_www_if_no_subdomain_vec <- function(raw_host) {
  ci <- stringi::stri_opts_regex(case_insensitive = TRUE)
  lower <- stringi::stri_trans_tolower(raw_host)

  candidate_host <- raw_host
  has_www <- stringi::stri_detect_regex(lower, "^www[0-9]*\\.")
  has_www[is.na(has_www)] <- FALSE
  if (any(has_www)) {
    match_res <- stringi::stri_match_first_regex(
      raw_host, "^(www[0-9]*\\.)(.*)", opts_regex = ci
    )
    bare_part <- match_res[, 3L]
    bare_part[is.na(bare_part)] <- raw_host[is.na(bare_part)]
    candidate_host[has_www] <- paste0("www.", bare_part[has_www])
  }

  host_for_domain_check <- candidate_host
  cand_lower <- stringi::stri_trans_tolower(candidate_host)
  cw <- stringi::stri_startswith_fixed(cand_lower, "www.")
  cw[is.na(cw)] <- FALSE
  if (any(cw)) {
    match_res_bare <- stringi::stri_match_first_regex(
      candidate_host, "^www\\.(.*)", opts_regex = ci
    )
    bare_host <- match_res_bare[, 2L]
    bare_host[is.na(bare_host)] <- candidate_host[is.na(bare_host)] # nocov
    host_for_domain_check[cw] <- bare_host[cw]
  }

  decomp <- .psl_suffix_extract(host_for_domain_check, "all")
  derived_domain <- decomp$registrable_domain
  derived_subdomain <- decomp$subdomain

  no_derived_domain <- is.na(derived_domain) | derived_domain == ""
  # Apex iff the canonical decomposition has no subdomain labels.
  host_equals_domain <- !is.na(derived_subdomain) & derived_subdomain == ""

  cand_has_www <- stringi::stri_startswith_fixed(cand_lower, "www.")
  cand_has_www[is.na(cand_has_www)] <- FALSE

  result <- candidate_host
  add_www <- !no_derived_domain & host_equals_domain & !cand_has_www
  result[add_www] <- paste0("www.", candidate_host[add_www])
  result
}

# Phase 6 (scalar wrapper): delegates to .apply_www_policy_vec().
.apply_www_policy <- function(raw_host, www_handling, is_ip_host) {
  .apply_www_policy_vec(raw_host, www_handling, is_ip_host)
}

# Phase 7 (vector): derive the registered domain and TLD from each host using
# the Public Suffix List. This is the hot path: pslr is queried ONCE per output
# spelling over the UNIQUE non-IP hosts (host-level de-dup; many URLs share a
# host) rather than once per URL. `host_encoding` selects the emitted spelling,
# mirroring get_host(): "unicode" decodes IDNs, "idna" emits ASCII A-labels, and
# "keep" follows each input host's own spelling (ASCII if it is an A-label).
.derive_domain_tld_vec <- function(final_host, is_ip_host, tld_source,
                                   host_encoding = "keep") {
  n <- length(final_host)
  domain <- rep(NA_character_, n)
  tld <- rep(NA_character_, n)
  elig <- !is_ip_host & !is.na(final_host) & final_host != ""
  if (!any(elig)) {
    return(list(domain = domain, tld = tld))
  }

  hosts <- final_host[elig]
  uniq_hosts <- unique(hosts)

  spelling <- if (host_encoding == "idna") {
    rep("ascii", length(uniq_hosts))
  } else if (host_encoding == "unicode") {
    rep("unicode", length(uniq_hosts))
  } else {
    ifelse(.host_is_ace_vec(uniq_hosts), "ascii", "unicode")
  }

  dom_uniq <- rep(NA_character_, length(uniq_hosts))
  tld_uniq <- rep(NA_character_, length(uniq_hosts))

  ascii_mask <- spelling == "ascii"
  if (any(ascii_mask)) {
    dom_uniq[ascii_mask] <- .psl_registered_domain(
      uniq_hosts[ascii_mask], tld_source, "ascii"
    )
    tld_uniq[ascii_mask] <- .psl_public_suffix(
      uniq_hosts[ascii_mask], tld_source, "ascii"
    )
  }
  uni_mask <- !ascii_mask
  if (any(uni_mask)) {
    dom_uniq[uni_mask] <- .psl_registered_domain(
      uniq_hosts[uni_mask], tld_source, "unicode"
    )
    tld_uniq[uni_mask] <- .psl_public_suffix(
      uniq_hosts[uni_mask], tld_source, "unicode"
    )
  }

  pos <- match(hosts, uniq_hosts)
  domain[elig] <- dom_uniq[pos]
  tld[elig] <- tld_uniq[pos]
  list(domain = domain, tld = tld)
}

# Phase 7 (scalar wrapper): delegates to .derive_domain_tld_vec().
.derive_domain_tld <- function(final_host, is_ip_host, tld_source,
                               host_encoding = "keep") {
  .derive_domain_tld_vec(final_host, is_ip_host, tld_source, host_encoding)
}

# Phase 8 (vector): keep only the requested number of subdomain levels. Only
# runs when subdomain_levels_to_keep is non-NULL. The STRUCTURAL decomposition
# is batched over the eligible hosts; the (variable-length) label reconstruction
# runs via vapply on the affected hosts only.
.apply_subdomain_policy_vec <- function(final_host, domain,
                                        subdomain_levels_to_keep, is_ip_host) {
  if (is.null(subdomain_levels_to_keep)) {
    return(final_host)
  }
  can_trim <- !is_ip_host & !is.na(domain) & domain != "" &
    !is.na(final_host) & final_host != ""
  if (!any(can_trim)) {
    return(final_host)
  }

  idx <- which(can_trim)
  fh <- final_host[idx]
  lower <- stringi::stri_trans_tolower(fh)
  has_www_prefix <- stringi::stri_startswith_fixed(lower, "www.")

  host_part <- fh
  if (any(has_www_prefix)) {
    host_part[has_www_prefix] <- stringi::stri_sub(fh[has_www_prefix], 5L)
  }

  decomp <- .psl_suffix_extract(host_part, "all")
  derived_subdomain <- decomp$subdomain
  has_subdomain <- !is.na(derived_subdomain) & nzchar(derived_subdomain)

  # base strsplit keeps the documented trailing-empty behavior (see CLAUDE.md).
  sub_split <- strsplit(derived_subdomain, ".", fixed = TRUE)
  raw_split <- strsplit(host_part, ".", fixed = TRUE)
  num_sub_labels <- lengths(sub_split)
  num_raw_labels <- lengths(raw_split)

  act <- has_subdomain & (num_raw_labels > num_sub_labels)
  act_pos <- which(act)
  if (length(act_pos) > 0L) {
    slk <- subdomain_levels_to_keep
    recon <- vapply(act_pos, function(j) {
      raw_labels <- raw_split[[j]]
      ns <- num_sub_labels[j]
      # The registrable-domain portion is lowercased (matching the historical
      # reconstruction); the kept subdomain labels preserve the input spelling.
      registrable_labels <- stringi::stri_trans_tolower(
        utils::tail(raw_labels, length(raw_labels) - ns)
      )
      sub_labels <- utils::head(raw_labels, ns)

      kept_sub_labels <- character(0)
      if (slk > 0) {
        num_keep <- min(length(sub_labels), slk)
        if (num_keep > 0) {
          kept_sub_labels <- utils::tail(sub_labels, num_keep)
        }
      }
      reconstructed <- paste(
        c(kept_sub_labels, registrable_labels), collapse = "."
      )
      if (has_www_prefix[j]) {
        paste0("www.", reconstructed)
      } else {
        reconstructed
      }
    }, character(1), USE.NAMES = FALSE)
    fh[act_pos] <- recon
  }

  final_host[idx] <- fh
  final_host
}

# Phase 8 (scalar wrapper): delegates to .apply_subdomain_policy_vec().
.apply_subdomain_policy <- function(final_host, domain,
                                    subdomain_levels_to_keep, is_ip_host) {
  .apply_subdomain_policy_vec(
    final_host, domain, subdomain_levels_to_keep, is_ip_host
  )
}

# Phase 9 (vector): re-encode (non-IP) hosts to IDNA/Punycode or Unicode on
# request. The Punycode round-trip semantics are owned by the DO-NOT-ALTER
# helpers .normalize_and_punycode_vec()/.punycode_to_unicode_vec() (domain.R);
# this phase only selects the eligible rows and applies the scalar fallback
# rule (keep the pre-encode host when the encode returns NA, or when the decode
# returns NA / "").
.apply_host_encoding_vec <- function(final_host, host_encoding, is_ip_host) {
  host_for_clean <- final_host
  if (host_encoding != "idna" && host_encoding != "unicode") {
    return(host_for_clean)
  }
  elig <- !is.na(final_host) & final_host != "" & !is_ip_host
  if (!any(elig)) {
    return(host_for_clean)
  }

  subset <- final_host[elig]
  if (host_encoding == "idna") {
    encoded <- .normalize_and_punycode_vec(subset)
    keep_orig <- is.na(encoded)
    encoded[keep_orig] <- subset[keep_orig]
    host_for_clean[elig] <- encoded
  } else {
    decoded <- .punycode_to_unicode_vec(subset)
    keep_orig <- is.na(decoded) | decoded == ""
    decoded[keep_orig] <- subset[keep_orig]
    host_for_clean[elig] <- decoded
  }
  host_for_clean
}

# Phase 9 (scalar wrapper): delegates to .apply_host_encoding_vec().
.apply_host_encoding <- function(final_host, host_encoding, is_ip_host) {
  .apply_host_encoding_vec(final_host, host_encoding, is_ip_host)
}

# Phase 10 (vector): apply the case policy to host, path, and scheme.
.apply_case_policy_vec <- function(host_output, path_output, scheme_output,
                                   case_handling) {
  h_mask <- !is.na(host_output) & host_output != ""
  if (any(h_mask)) {
    if (case_handling == "lower" || case_handling == "lower_host") {
      host_output[h_mask] <- stringi::stri_trans_tolower(host_output[h_mask])
    } else if (case_handling == "upper") {
      host_output[h_mask] <- stringi::stri_trans_toupper(host_output[h_mask])
    }
  }

  p_mask <- !is.na(path_output)
  if (any(p_mask)) {
    if (case_handling == "lower") {
      path_output[p_mask] <- stringi::stri_trans_tolower(path_output[p_mask])
    } else if (case_handling == "upper") {
      path_output[p_mask] <- stringi::stri_trans_toupper(path_output[p_mask])
    }
  }

  s_mask <- !is.na(scheme_output)
  if (any(s_mask)) {
    if (case_handling == "lower" || case_handling == "lower_host") {
      scheme_output[s_mask] <-
        stringi::stri_trans_tolower(scheme_output[s_mask])
    } else if (case_handling == "upper") {
      scheme_output[s_mask] <-
        stringi::stri_trans_toupper(scheme_output[s_mask])
    }
  }

  list(host = host_output, path = path_output, scheme = scheme_output)
}

# Phase 10 (scalar wrapper): delegates to .apply_case_policy_vec().
.apply_case_policy <- function(host_for_clean, path_final, final_scheme,
                               case_handling) {
  .apply_case_policy_vec(
    host_for_clean, path_final, final_scheme, case_handling
  )
}

# Phase 11 (vector): reconstruct the canonical "clean" URL from cased
# components. NA/empty host yields NA.
.build_clean_url_vec <- function(scheme_output, host_output, path_output,
                                 trailing_slash_handling) {
  n <- length(host_output)
  clean_url <- rep(NA_character_, n)
  has_host <- !is.na(host_output) & host_output != ""
  if (!any(has_host)) {
    return(clean_url)
  }

  scheme_part <- ifelse(!is.na(scheme_output), paste0(scheme_output, "://"), "")
  path_part <- ifelse(!is.na(path_output), path_output, "")
  if (trailing_slash_handling == "strip") {
    path_part[path_part == "/"] <- ""
  }
  clean_url[has_host] <- paste0(
    scheme_part[has_host], host_output[has_host], path_part[has_host]
  )
  clean_url
}

# Phase 11 (scalar wrapper): delegates to .build_clean_url_vec().
.build_clean_url <- function(scheme_output, host_output, path_output,
                             trailing_slash_handling) {
  .build_clean_url_vec(
    scheme_output, host_output, path_output, trailing_slash_handling
  )
}

# Phase 12 (vector): classify the parse outcome (ok / ok-ftp / warning-* /
# error / ok-scheme-relative). `curl_ok` is TRUE for rows curl parsed (the
# scalar wrapper passes !is.null(parsed_curl)). Progressive mask assignment
# mirrors the scalar precedence exactly.
.derive_parse_status_vec <- function(curl_ok, final_host, is_ip_host, tld,
                                     domain, protocol_handling, final_scheme,
                                     looks_like_protocol,
                                     original_has_allowed_scheme,
                                     is_scheme_relative,
                                     scheme_relative_handling) {
  n <- length(final_host)
  status <- rep(.STATUS_ERROR, n)

  host_present <- curl_ok & !is.na(final_host) & final_host != ""

  status[host_present & is_ip_host] <- .STATUS_OK

  non_ip <- host_present & !is_ip_host
  host_has_dot <- stringi::stri_detect_fixed(final_host, ".")
  host_has_dot[is.na(host_has_dot)] <- FALSE
  tld_empty <- is.na(tld) | !nzchar(tld)
  domain_empty <- is.na(domain) | !nzchar(domain)

  status[non_ip & !host_has_dot] <- .STATUS_WARN_NO_TLD
  status[non_ip & host_has_dot & tld_empty] <- .STATUS_WARN_INVALID_TLD
  status[non_ip & host_has_dot & !tld_empty & domain_empty] <-
    .STATUS_WARN_PUBLIC_SUFFIX
  status[non_ip & host_has_dot & !tld_empty & !domain_empty] <- .STATUS_OK

  if (protocol_handling != "strip") {
    ftp_candidate <- host_present & status == .STATUS_OK & !is.na(final_scheme)
    scheme_lower <- stringi::stri_trans_tolower(final_scheme)
    is_ftp <- ftp_candidate & scheme_lower %in% c("ftp", "ftps")
    is_ftp[is.na(is_ftp)] <- FALSE
    status[is_ftp] <- .STATUS_OK_FTP
  }

  protocol_kept <- protocol_handling == "keep" || protocol_handling == "none"
  if (protocol_kept) {
    unsupported <- looks_like_protocol & !original_has_allowed_scheme
    status[unsupported] <- .STATUS_ERROR
  }
  status[!curl_ok] <- .STATUS_ERROR

  if (scheme_relative_handling == "keep") {
    status[is_scheme_relative & status == .STATUS_OK] <- .STATUS_OK_SCHEME_REL
  }

  status
}

# Phase 12 (scalar wrapper): delegates to .derive_parse_status_vec().
.derive_parse_status <- function(parsed_curl, final_host, is_ip_host, tld,
                                 domain, protocol_handling, final_scheme,
                                 looks_like_protocol,
                                 original_has_allowed_scheme,
                                 is_scheme_relative,
                                 scheme_relative_handling) {
  .derive_parse_status_vec(
    curl_ok = !is.null(parsed_curl),
    final_host = final_host,
    is_ip_host = is_ip_host,
    tld = tld,
    domain = domain,
    protocol_handling = protocol_handling,
    final_scheme = final_scheme,
    looks_like_protocol = looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    is_scheme_relative = is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )
}

# Phase 13 (vector): build the 14 result columns. Coerces the scheme-relative
# "keep" scheme to NA and an empty host to NA, matching the scalar assembler.
# The engine applies error-row defaults for NULL-equivalent rows afterward.
.assemble_parse_result_vec <- function(original_url, scheme_output, host_output,
                                       port, path_output, raw_query, fragment,
                                       user, password, domain, tld, is_ip_host,
                                       clean_url, parse_status,
                                       is_scheme_relative,
                                       scheme_relative_handling) {
  scheme_return <- scheme_output
  if (scheme_relative_handling == "keep") {
    scheme_return[is_scheme_relative] <- NA_character_
  }

  host_return <- host_output
  host_return[is.na(host_output) | host_output == ""] <- NA_character_

  list(
    original_url = original_url,
    scheme = scheme_return,
    host = host_return,
    port = port,
    path = path_output,
    query = raw_query,
    # fragment/user/password are returned raw (as written in the URL): with
    # `decode = FALSE` (see .parse_with_curl) curl no longer percent-decodes
    # them, keeping them consistent with the raw path/query.
    fragment = fragment,
    user = user,
    password = password,
    domain = domain,
    tld = tld,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status
  )
}

# Phase 13 (scalar wrapper): extracts port/fragment/user/password from the curl
# object, then delegates to .assemble_parse_result_vec().
.assemble_parse_result <- function(original_input_url, scheme_output,
                                   host_output, parsed_curl, path_output,
                                   raw_query, domain, tld, is_ip_host,
                                   clean_url, parse_status, is_scheme_relative,
                                   scheme_relative_handling) {
  .assemble_parse_result_vec(
    original_url = original_input_url,
    scheme_output = scheme_output,
    host_output = host_output,
    port = suppressWarnings(as.integer(parsed_curl$port %||% NA_integer_)),
    path_output = path_output,
    # .blank_to_na(): present-but-empty raw components "" -> NA (see utils.R).
    raw_query = .blank_to_na(raw_query %||% NA_character_),
    fragment = .blank_to_na(parsed_curl$fragment %||% NA_character_),
    user = .blank_to_na(parsed_curl$user %||% NA_character_),
    password = .blank_to_na(parsed_curl$password %||% NA_character_),
    domain = domain,
    tld = tld,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status,
    is_scheme_relative = is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )
}
