# Parsing phases: the decomposed helpers behind ._safe_parse_url_impl().

# ---------------------------------------------------------------------------
# Decomposed phase helpers for ._safe_parse_url_impl().
#
# Each helper owns one normalization phase and can be reasoned about and
# tested independently. The code inside is moved verbatim from the original
# monolithic implementation (the only behavior changes are the separately
# tracked port/IPv6/subdomain bug fixes). None of these are memoized; the
# memoizing wrapper ._safe_parse_url_scalar() caches the whole result.
# ---------------------------------------------------------------------------

# Phase 1: scheme detection, supported-scheme policy, and building the string
# handed to curl. Returns NULL when the URL must be rejected (scheme-relative
# with "error" handling, or an unsupported explicit scheme under keep/none).
.prepare_url_for_curl <- function(url,
                                  protocol_handling,
                                  scheme_relative_handling) {
  allowed_prefixes <- c("http://", "https://", "ftp://", "ftps://")
  original_url_lower <- stringi::stri_trans_tolower(url)
  original_has_allowed_scheme <- any(
    startsWith(original_url_lower, allowed_prefixes)
  )

  scheme_match <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):"
  )
  scheme_candidate <- if (!is.na(scheme_match[1, 2])) {
    scheme_match[1, 2]
  } else {
    NA_character_
  }
  looks_like_protocol <- !is.na(scheme_candidate)
  has_scheme_slashes <- stringi::stri_detect_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):\\/\\/"
  )
  if (is.na(has_scheme_slashes)) {
    has_scheme_slashes <- FALSE
  }

  is_scheme_relative <- stringi::stri_startswith_fixed(url, "//")
  if (is_scheme_relative && scheme_relative_handling == "error") {
    return(NULL)
  }
  if (is_scheme_relative && scheme_relative_handling %in% c("http", "https")) {
    # Treat scheme-relative URLs as having an inferred scheme for handling logic
    looks_like_protocol <- TRUE
    original_has_allowed_scheme <- TRUE
  }

  looks_like_host_port <- FALSE
  maybe_host_port <- looks_like_protocol &&
    !original_has_allowed_scheme &&
    !has_scheme_slashes
  if (maybe_host_port) {
    looks_like_host_port <- stringi::stri_detect_regex(
      url, "^[^/]+:[0-9]+($|/)"
    )
    if (is.na(looks_like_host_port)) {
      looks_like_host_port <- FALSE
    }
  }

  protocol_kept <- protocol_handling == "keep" || protocol_handling == "none"
  bare_protocol_kept <- protocol_kept &&
    looks_like_protocol &&
    !original_has_allowed_scheme &&
    !looks_like_host_port
  if (bare_protocol_kept) {
    return(NULL)
  }

  url_to_parse <- url
  if (is_scheme_relative) {
    if (scheme_relative_handling %in% c("http", "https")) {
      url_to_parse <- paste0(scheme_relative_handling, ":", url)
    } else {
      url_to_parse <- paste0("http:", url)
    }
  } else if (!looks_like_protocol || looks_like_host_port) {
    url_to_parse <- paste0("http://", url)
  }

  list(
    url_to_parse = url_to_parse,
    looks_like_protocol = looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    is_scheme_relative = is_scheme_relative,
    looks_like_host_port = looks_like_host_port
  )
}

# Phase 2a: parse the prepared URL with curl, returning NULL on failure.
.parse_with_curl <- function(url_to_parse) {
  tryCatch(
    curl::curl_parse_url(url_to_parse),
    error = function(e) NULL
  )
}

# Phase 2b: pull the raw components used downstream out of the curl result,
# reconstructing the query string from params when curl did not surface one.
.extract_raw_components <- function(parsed_curl) {
  raw_query <- parsed_curl$query %||% NA_character_
  rebuild_query_from_params <- is.na(raw_query) &&
    !is.null(parsed_curl$params) &&
    length(parsed_curl$params) > 0
  if (rebuild_query_from_params) {
    # curl_parse_url() always decodes params, so a literal "&"/"=" inside a
    # value is indistinguishable from a delimiter once joined. Re-encode each
    # key/value so the reconstructed query is a faithful RAW (percent-encoded)
    # string; downstream parsers split on raw "&"/"=" then decode per-pair.
    raw_query <- paste(
      vapply(names(parsed_curl$params), curl::curl_escape, character(1)),
      vapply(unname(parsed_curl$params), curl::curl_escape, character(1)),
      sep = "=",
      collapse = "&"
    )
  }
  list(
    scheme = parsed_curl$scheme %||% NA_character_,
    host = parsed_curl$host %||% NA_character_,
    path = parsed_curl$path %||% NA_character_,
    query = raw_query
  )
}

# Phase 3: path decoding, slash/dot normalization, index stripping, trailing
# slash policy, and optional percent-encoding.
.normalize_path <- function(raw_path,
                            path_encoding,
                            path_normalization,
                            index_page_handling,
                            trailing_slash_handling) {
  path_work <- raw_path

  # Decode path if requested, before normalization/index handling
  if (!is.na(path_work) && path_encoding %in% c("decode", "encode")) {
    path_work <- tryCatch(
      curl::curl_unescape(path_work),
      error = function(e) path_work
    )
  }

  # Path normalization (slashes and dot segments)
  if (!is.na(path_work)) {
    if (path_normalization %in% c("collapse_slashes", "both")) {
      path_work <- ._collapse_path_slashes(path_work)
    }
    if (path_normalization %in% c("dot_segments", "both")) {
      path_work <- ._remove_dot_segments(path_work)
    }
  }

  # Index/default page handling
  if (!is.na(path_work) && index_page_handling == "strip") {
    path_work <- ._strip_index_page(path_work)
  }

  # Trailing slash handling (after normalization/index handling)
  if (!is.na(path_work) && nzchar(path_work)) {
    if (trailing_slash_handling == "strip") {
      if (path_work != "/" && stringi::stri_endswith_fixed(path_work, "/")) {
        path_work <- stringi::stri_sub(
          path_work, 1, stringi::stri_length(path_work) - 1
        )
      }
    } else if (trailing_slash_handling == "keep") {
      if (path_work != "/" && !stringi::stri_endswith_fixed(path_work, "/")) {
        path_work <- paste0(path_work, "/")
      }
    }
  }

  # Path percent-encoding handling (performed after normalization/index logic)
  if (!is.na(path_work) && path_encoding == "encode") {
    path_work <- ._encode_path_segments(path_work)
  }

  path_work
}

# Phase 4: resolve the final scheme according to protocol policy.
.derive_final_scheme <- function(protocol_handling,
                                 looks_like_protocol,
                                 raw_scheme) {
  switch(protocol_handling,
    none = if (looks_like_protocol) raw_scheme else NA_character_,
    strip = NA_character_,
    http = "http",
    https = "https",
    keep = raw_scheme
  )
}

# Phase 5: detect whether the host is an IP literal (IPv4 or IPv6).
.detect_ip_host <- function(raw_host) {
  if (is.na(raw_host) || raw_host == "") {
    return(FALSE)
  }
  # IPv4: exactly four dot-separated octets, each an integer in 0..255.
  ipv4 <- FALSE
  if (isTRUE(stringi::stri_detect_regex(
    raw_host, "^\\d{1,3}(\\.\\d{1,3}){3}$"
  ))) {
    labels <- strsplit(raw_host, ".", fixed = TRUE)[[1]]
    octets <- suppressWarnings(as.integer(labels))
    ipv4 <- length(octets) == 4L &&
      !anyNA(octets) &&
      all(octets >= 0L & octets <= 255L)
  }
  # IPv6: [2001:db8::1] or 2001:db8::1. Require balanced brackets when present
  # (reject if exactly one of '[' / ']' appears). Conservative hex/colon check
  # otherwise; a full RFC 4291 validator is out of scope.
  has_open <- stringi::stri_detect_fixed(raw_host, "[")
  has_close <- stringi::stri_detect_fixed(raw_host, "]")
  ipv6 <- FALSE
  if (has_open == has_close) {
    ipv6 <- isTRUE(stringi::stri_detect_regex(
      raw_host, "^\\[?[0-9a-fA-F:]+\\]?$"
    )) && isTRUE(stringi::stri_detect_regex(raw_host, ":"))
  }
  ipv4 || ipv6
}

# Phase 6: apply the www-prefix policy to a (non-IP) host.
.apply_www_policy <- function(raw_host, www_handling, is_ip_host) {
  final_host <- raw_host
  if (!is_ip_host && !is.na(raw_host) && raw_host != "") {
    if (www_handling == "strip") {
      final_host <- stringi::stri_replace_first_regex(
        raw_host,
        "^(www[0-9]*\\.)(.*)",
        "$2",
        opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
      )
    } else if (www_handling == "keep") {
      current_host_lower <- stringi::stri_trans_tolower(raw_host)
      if (stringi::stri_detect_regex(current_host_lower, "^www[0-9]*\\.")) {
        match_res <- stringi::stri_match_first_regex(
          raw_host,
          "^(www[0-9]*\\.)(.*)",
          opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )
        bare_host_part <- if (!is.na(match_res[1, 3])) {
          match_res[1, 3]
        } else {
          raw_host
        }
        final_host <- paste0("www.", bare_host_part)
      } else {
        final_host <- paste0("www.", raw_host)
      }
    } else if (www_handling == "if_no_subdomain") {
      candidate_host <- raw_host
      if (stringi::stri_detect_regex(
        stringi::stri_trans_tolower(raw_host), "^www[0-9]*\\."
      )) {
        match_res <- stringi::stri_match_first_regex(
          raw_host,
          "^(www[0-9]*\\.)(.*)",
          opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )
        bare_part <- if (!is.na(match_res[1, 3])) match_res[1, 3] else raw_host
        candidate_host <- paste0("www.", bare_part)
      }
      host_for_domain_check <- candidate_host
      if (stringi::stri_startswith_fixed(
        stringi::stri_trans_tolower(candidate_host), "www."
      )) {
        match_res_bare <- stringi::stri_match_first_regex(
          candidate_host,
          "^www\\.(.*)",
          opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )
        host_for_domain_check <- if (!is.na(match_res_bare[1, 2])) {
          match_res_bare[1, 2]
        } else {
          candidate_host
        }
      }

      # The www heuristic always uses the all-section decomposition to decide
      # whether the host is itself an apex (eTLD+1) and thus eligible to gain a
      # leading "www.". The STRUCTURAL decision (is there a subdomain? is there
      # a registrable domain at all?) is made on pslr's canonical decomposition
      # so an A-label host and its Unicode equivalent take the same branch;
      # only the *decision* is canonicalized, the emitted host below still uses
      # the input spelling (candidate_host).
      decomp <- .psl_suffix_extract(host_for_domain_check, "all")
      derived_domain <- decomp$registrable_domain[[1]]
      derived_subdomain <- decomp$subdomain[[1]]

      no_derived_domain <- is.na(derived_domain) || derived_domain == ""
      if (no_derived_domain) {
        final_host <- candidate_host
      } else {
        # Apex iff the canonical decomposition has no subdomain labels.
        host_equals_domain <- !is.na(derived_subdomain) &&
          derived_subdomain == ""
        if (host_equals_domain) {
          candidate_has_www <- stringi::stri_startswith_fixed(
            stringi::stri_trans_tolower(candidate_host), "www."
          )
          if (!candidate_has_www) {
            final_host <- paste0("www.", candidate_host)
          } else {
            final_host <- candidate_host
          }
        } else {
          final_host <- candidate_host
        }
      }
    }
  }
  final_host
}

# Phase 7: derive the registered domain and TLD from the host using the Public
# Suffix List. `host_encoding` selects the emitted spelling, mirroring
# get_host(): "unicode" decodes IDNs, "idna" emits ASCII A-labels, and "keep"
# (the default) follows the input host's own spelling — ASCII if it arrived as
# an A-label, Unicode otherwise.
.derive_domain_tld <- function(final_host, is_ip_host, tld_source,
                               host_encoding = "keep") {
  domain <- NA_character_
  tld <- NA_character_

  if (!is_ip_host && !is.na(final_host) && final_host != "") {
    psl_output <- switch(host_encoding,
      idna = "ascii",
      unicode = "unicode",
      keep = if (.host_is_ace(final_host)) "ascii" else "unicode",
      "unicode"
    )
    # Both the registered domain and the TLD honor the requested section and
    # output spelling, so a given parse is internally consistent (the domain
    # always ends in the TLD, in one spelling). pslr canonicalizes the host
    # (case/NFC/IDNA) internally.
    domain <- .psl_registered_domain(final_host, tld_source, psl_output)
    tld <- .psl_public_suffix(final_host, tld_source, psl_output)
  }

  list(domain = domain, tld = tld)
}

# Phase 8: keep only the requested number of subdomain levels.
.apply_subdomain_policy <- function(final_host, domain,
                                    subdomain_levels_to_keep, is_ip_host) {
  can_trim_subdomains <- !is.null(subdomain_levels_to_keep) &&
    !is_ip_host &&
    !is.na(domain) &&
    domain != "" &&
    !is.na(final_host) &&
    final_host != ""
  if (can_trim_subdomains) {
    current_host_lower <- stringi::stri_trans_tolower(final_host)
    www_prefix_str <- "www."

    has_www_prefix <- stringi::stri_startswith_fixed(
      current_host_lower, www_prefix_str
    )

    host_part_to_analyze <- final_host
    if (has_www_prefix) {
      host_part_to_analyze <- stringi::stri_sub(
        final_host, stringi::stri_length(www_prefix_str) + 1
      )
    }

    # STRUCTURAL decision on the canonical decomposition: how many subdomain
    # labels does this host have, and where is the registrable boundary? Doing
    # this on pslr's canonical spelling makes an A-label host and its Unicode
    # equivalent yield the same label counts (and thus the same trim), instead
    # of depending on a raw-host string-compare that only matches one spelling.
    decomp <- .psl_suffix_extract(host_part_to_analyze, "all")
    derived_subdomain <- decomp$subdomain[[1]]

    # Number of subdomain labels per the canonical decomposition (""=none). We
    # trim by *label count* and reconstruct from the raw host's OWN labels so
    # the input's A-label-vs-Unicode spelling is preserved in the output; only
    # the structural decision (how many labels to drop) is canonicalized.
    has_subdomain <- !is.na(derived_subdomain) && nzchar(derived_subdomain)
    if (has_subdomain) {
      num_sub_labels <- length(strsplit(derived_subdomain, "\\.")[[1]])
      raw_labels <- strsplit(host_part_to_analyze, "\\.")[[1]]

      # Defensive: only act when the raw host actually has at least as many
      # labels as the subdomain reported (it always should, since both describe
      # the same logical host); otherwise leave the host untouched.
      if (length(raw_labels) > num_sub_labels) {
        # The registrable-domain portion is lowercased (matching the historical
        # reconstruction, which built from the lowercased registered domain),
        # while the kept subdomain labels preserve the input's case/spelling.
        # We lowercase the raw registrable labels rather than substituting
        # pslr's Unicode domain so that an A-label input keeps its A-label
        # spelling and a Unicode input keeps its Unicode spelling.
        registrable_labels <- stringi::stri_trans_tolower(
          utils::tail(raw_labels, length(raw_labels) - num_sub_labels)
        )
        sub_labels <- utils::head(raw_labels, num_sub_labels)

        kept_sub_labels <- character(0)
        if (subdomain_levels_to_keep > 0) {
          num_sub_labels_to_keep <- min(
            length(sub_labels), subdomain_levels_to_keep
          )
          if (num_sub_labels_to_keep > 0) {
            kept_sub_labels <- utils::tail(sub_labels, num_sub_labels_to_keep)
          }
        }

        reconstructed_host_part <- paste(
          c(kept_sub_labels, registrable_labels), collapse = "."
        )

        if (has_www_prefix) {
          final_host <- paste0(www_prefix_str, reconstructed_host_part)
        } else {
          final_host <- reconstructed_host_part
        }
      }
    }
  }
  final_host
}

# Phase 9: re-encode the (non-IP) host to IDNA/Punycode or Unicode on request.
.apply_host_encoding <- function(final_host, host_encoding, is_ip_host) {
  host_for_clean <- final_host
  if (!is.na(host_for_clean) && host_for_clean != "" && !is_ip_host) {
    if (host_encoding == "idna") {
      encoded_host <- .normalize_and_punycode(host_for_clean)
      if (!is.na(encoded_host)) host_for_clean <- encoded_host
    } else if (host_encoding == "unicode") {
      decoded_host <- .punycode_to_unicode(host_for_clean)
      if (!is.na(decoded_host) && decoded_host != "") {
        host_for_clean <- decoded_host
      }
    }
  }
  host_for_clean
}

# Phase 10: apply the case policy to host, path, and scheme.
.apply_case_policy <- function(host_for_clean, path_final, final_scheme,
                               case_handling) {
  host_output <- host_for_clean
  path_output <- path_final
  scheme_output <- final_scheme

  if (!is.na(host_output) && host_output != "") {
    host_output <- switch(case_handling,
      lower = stringi::stri_trans_tolower(host_output),
      upper = stringi::stri_trans_toupper(host_output),
      lower_host = stringi::stri_trans_tolower(host_output),
      keep = host_output
    )
  }

  if (!is.na(path_output)) {
    path_output <- switch(case_handling,
      lower = stringi::stri_trans_tolower(path_output),
      upper = stringi::stri_trans_toupper(path_output),
      lower_host = path_output,
      keep = path_output
    )
  }

  if (!is.na(scheme_output)) {
    scheme_output <- switch(case_handling,
      lower = stringi::stri_trans_tolower(scheme_output),
      upper = stringi::stri_trans_toupper(scheme_output),
      lower_host = stringi::stri_trans_tolower(scheme_output),
      keep = scheme_output
    )
  }

  list(host = host_output, path = path_output, scheme = scheme_output)
}

# Phase 11: reconstruct the canonical "clean" URL from cased components.
.build_clean_url <- function(scheme_output, host_output, path_output,
                             trailing_slash_handling) {
  clean_url <- NA_character_
  if (!is.na(host_output) && host_output != "") {
    scheme_part <- if (!is.na(scheme_output)) {
      paste0(scheme_output, "://")
    } else {
      ""
    }
    path_part <- if (!is.na(path_output)) path_output else ""
    if (trailing_slash_handling == "strip" && identical(path_part, "/")) {
      path_part <- ""
    }
    clean_url <- paste0(scheme_part, host_output, path_part)
  }
  clean_url
}

# Phase 12: classify the parse outcome (ok / ok-ftp / warning-* / error /
# ok-scheme-relative).
.derive_parse_status <- function(parsed_curl, final_host, is_ip_host, tld,
                                 domain, protocol_handling, final_scheme,
                                 looks_like_protocol,
                                 original_has_allowed_scheme,
                                 is_scheme_relative,
                                 scheme_relative_handling) {
  parse_status <- .STATUS_ERROR

  if (!is.null(parsed_curl)) {
    host_is_present <- !is.na(final_host) && final_host != ""

    if (host_is_present) {
      if (is_ip_host) {
        parse_status <- .STATUS_OK
      } else {
        host_has_dot <- stringi::stri_detect_fixed(final_host, ".")
        if (is.na(host_has_dot)) host_has_dot <- FALSE

        if (!host_has_dot) {
          parse_status <- .STATUS_WARN_NO_TLD
        } else if (is.na(tld) || !nzchar(tld)) {
          parse_status <- .STATUS_WARN_INVALID_TLD
        } else if (is.na(domain) || !nzchar(domain)) {
          parse_status <- .STATUS_WARN_PUBLIC_SUFFIX
        } else {
          parse_status <- .STATUS_OK
        }
      }

      ok_with_scheme <- parse_status == .STATUS_OK &&
        protocol_handling != "strip" &&
        !is.na(final_scheme)
      if (ok_with_scheme) {
        current_scheme_lower <- stringi::stri_trans_tolower(final_scheme)
        if (current_scheme_lower %in% c("ftp", "ftps")) {
          parse_status <- .STATUS_OK_FTP
        }
      }
    }
  }

  protocol_kept <- protocol_handling == "keep" || protocol_handling == "none"
  unsupported_scheme_kept <- protocol_kept &&
    looks_like_protocol &&
    !original_has_allowed_scheme
  if (is.null(parsed_curl) || unsupported_scheme_kept) {
    parse_status <- .STATUS_ERROR
  }

  kept_scheme_relative <- is_scheme_relative &&
    scheme_relative_handling == "keep" &&
    parse_status == .STATUS_OK
  if (kept_scheme_relative) {
    parse_status <- .STATUS_OK_SCHEME_REL
  }

  parse_status
}

# Phase 13: coerce types and assemble the result list returned to callers.
.assemble_parse_result <- function(original_input_url, scheme_output,
                                   host_output, parsed_curl, path_output,
                                   raw_query, domain, tld, is_ip_host,
                                   clean_url, parse_status, is_scheme_relative,
                                   scheme_relative_handling) {
  scheme_return <- scheme_output
  if (is_scheme_relative && scheme_relative_handling == "keep") {
    scheme_return <- NA_character_
  }

  list(
    original_url = original_input_url,
    scheme = scheme_return,
    host = if (is.na(host_output) || host_output == "") {
      NA_character_
    } else {
      host_output
    },
    port = suppressWarnings(as.integer(parsed_curl$port %||% NA_integer_)),
    path = path_output,
    query = raw_query %||% NA_character_,
    fragment = parsed_curl$fragment %||% NA_character_,
    user = parsed_curl$user %||% NA_character_,
    password = parsed_curl$password %||% NA_character_,
    domain = domain,
    tld = tld,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status
  )
}
