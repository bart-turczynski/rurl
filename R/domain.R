# Domain/TLD derivation (PSL) and punycode helpers. DO NOT modify punycode logic.

# Internal helper to encode hostnames using IDNA (Punycode)
# Accepts encode_fn for testability and fallback
.normalize_and_punycode <- function(host, encode_fn = punycoder::puny_encode) {
  if (is.na(host) || !nzchar(host)) {
    return(host)
  }

  # Memoize on the raw host so repeated calls within a vector parse (the same
  # host hits this across phases 6/7/9) and duplicate hosts across URLs collapse
  # to one R->C++ crossing. Bypass when a non-default encode_fn is injected so
  # test doubles never read or pollute the shared cache.
  use_cache <- identical(encode_fn, punycoder::puny_encode)
  if (use_cache) {
    cached <- .cache_get("puny_encode", host)
    if (!identical(cached, .rurl_cache_sentinel)) {
      return(cached)
    }
  }

  host_nfc <- stringi::stri_trans_nfc(host) # Normalize Unicode

  # Fall back to a non-strict encode so malformed-but-encodable hosts can still
  # produce output instead of NA. (strict must be passed explicitly: punycoder
  # sets options(punycoder.strict = TRUE) in .onLoad, so the unqualified retry
  # would re-run with the identical strict = TRUE and always fail the same way.)
  encoded <- tryCatch(
    encode_fn(host_nfc, strict = TRUE),
    error = function(e) {
      tryCatch(
        encode_fn(host_nfc, strict = FALSE),
        error = function(e2) NA_character_
      )
    }
  )

  if (!is.character(encoded) || length(encoded) != 1L) {
    # nocov start
    return(NA_character_)
    # nocov end
  }
  if (use_cache) {
    .cache_set("puny_encode", host, encoded)
  }
  encoded
}

# Internal helper to decode Punycode domain parts to Unicode
.punycode_to_unicode <- function(
  domain_puny,
  decode_fn = punycoder::puny_decode
) {
  if (is.na(domain_puny)) {
    return(NA_character_)
  }
  if (!nzchar(domain_puny)) {
    return("")
  }

  # Memoize on the punycode input (see .normalize_and_punycode); bypass when a
  # non-default decode_fn is injected so test doubles stay isolated.
  use_cache <- identical(decode_fn, punycoder::puny_decode)
  if (use_cache) {
    cached <- .cache_get("puny_decode", domain_puny)
    if (!identical(cached, .rurl_cache_sentinel)) {
      return(cached)
    }
  }

  parts_puny <- strsplit(domain_puny, "\\.")[[1]]

  # No strict-retry here: the first attempt is already the lenient strict =
  # FALSE decode, and a strict = TRUE retry (punycoder's getOption default) is
  # only ever stricter, so it can never recover what strict = FALSE rejected.
  decoded_labels <- tryCatch(
    decode_fn(parts_puny, strict = FALSE),
    error = function(e) rep(NA_character_, length(parts_puny)) # nocov
  )

  if (!is.character(decoded_labels) ||
    length(decoded_labels) != length(parts_puny)) {
    # nocov start
    decoded_labels <- rep(NA_character_, length(parts_puny))
    # nocov end
  }

  decoded_labels[is.na(decoded_labels)] <- parts_puny[is.na(decoded_labels)]

  # Ensure labels are valid UTF-8 and drop irrecoverable bytes.
  sane_labels <- iconv(decoded_labels, from = "UTF-8", to = "UTF-8", sub = "")
  sane_labels[is.na(sane_labels)] <- ""

  result <- paste(sane_labels, collapse = ".")
  if (use_cache) {
    .cache_set("puny_decode", domain_puny, result)
  }
  result
}

# Internal helper to derive a registered domain from host + TLD
._derive_domain_from_tld <- function(host_unicode, tld_unicode) {
  if (is.na(host_unicode) || !nzchar(host_unicode)) {
    return(NA_character_)
  }
  if (is.na(tld_unicode) || !nzchar(tld_unicode)) {
    return(NA_character_)
  }

  host_lc <- stringi::stri_trans_tolower(host_unicode)
  tld_lc <- stringi::stri_trans_tolower(tld_unicode)

  if (host_lc == tld_lc) {
    return(NA_character_)
  }
  suffix <- paste0(".", tld_lc)
  if (!stringi::stri_endswith_fixed(host_lc, suffix)) {
    return(NA_character_)
  }

  host_left <- stringi::stri_sub(
    host_lc,
    1,
    stringi::stri_length(host_lc) - stringi::stri_length(suffix)
  )
  if (!nzchar(host_left)) {
    return(NA_character_)
  }

  labels <- strsplit(host_left, "\\.")[[1]]
  paste0(utils::tail(labels, 1), ".", tld_lc)
}

# Internal helper to derive registered domain using Public Suffix List
# Expects hostname_encoded to already be NFC-normalized, lowercased,
# and Punycode-encoded if non-ASCII.
# Uses pre-computed hash sets from .onLoad for O(1) lookups
# instead of linear %in% searches.
# Results are memoized for performance.
.get_registered_domain <- function(hostname) {
  # Check cache first
  cached <- .cache_get("domain", hostname)
  if (!identical(cached, .rurl_cache_sentinel)) {
    return(cached)
  }

  result <- ._get_registered_domain_impl(hostname)

  # Cache the result
  .cache_set("domain", hostname, result)
  result
}

# Split a host into its non-empty labels for PSL matching.
#
# Shared preamble for the registered-domain and TLD extraction paths so both
# normalize hosts identically: strips a single trailing dot, rejects empty
# labels, and requires at least two labels. Returns the label vector, or NULL
# when the host cannot yield a registered domain / TLD (callers map NULL to
# NA_character_).
.host_labels <- function(host) {
  if (is.na(host) || !nzchar(host)) {
    return(NULL)
  }

  host_core <- if (stringi::stri_endswith_fixed(host, ".")) {
    stringi::stri_sub(host, 1, stringi::stri_length(host) - 1)
  } else {
    host
  }

  if (!nzchar(host_core)) {
    return(NULL)
  }

  parts <- strsplit(host_core, "\\.")[[1]]
  if (any(!nzchar(parts))) {
    return(NULL)
  }

  if (length(parts) < 2L) {
    return(NULL)
  }

  parts
}

# Internal implementation of .get_registered_domain (not memoized)
._get_registered_domain_impl <- function(hostname) {
  parts <- .host_labels(hostname)
  if (is.null(parts)) {
    return(NA_character_)
  }

  n <- length(parts)

  # 1. Exception rules (take precedence)
  # Uses .psl_exception_set (environment) for O(1) lookup
  for (i in seq_len(n)) {
    candidate <- paste(parts[i:n], collapse = ".")

    if (.in_set(candidate, .psl_exception_set)) {
      # Exception match: treat the exception domain as *not* a suffix
      # So return one label above it
      return(candidate)
    }
  }

  # 2. Track best match length
  best_match_len <- 0L

  for (i in seq_len(n)) { # i is the start index of a suffix candidate in parts
    candidate_suffix_str <- paste(parts[i:n], collapse = ".")
    num_parts_in_candidate_suffix <- n - i + 1L

    # Check if candidate_suffix_str is an exact match in normal_rules
    # Uses .psl_normal_set (environment) for O(1) lookup
    if (.in_set(candidate_suffix_str, .psl_normal_set)) {
      if (num_parts_in_candidate_suffix > best_match_len) {
        best_match_len <- num_parts_in_candidate_suffix
      }
    }

    # Check if candidate_suffix_str matches a wildcard rule.
    # A wildcard rule means "*." + some_suffix_in_wildcard_rules.
    # Uses .psl_wildcard_set (environment) for O(1) lookup
    # Must have at least "label.wildcard_part"
    if (num_parts_in_candidate_suffix > 1L) {
      potential_wildcard_match_part <- paste(parts[(i + 1L):n], collapse = ".")
      if (.in_set(potential_wildcard_match_part, .psl_wildcard_set)) {
        if (num_parts_in_candidate_suffix > best_match_len) {
          best_match_len <- num_parts_in_candidate_suffix
        }
      }
    }
  }

  if (best_match_len == 0L) {
    return(NA_character_)
  }

  # If hostname is a public suffix itself
  # (or shorter than the matched public suffix),
  # it cannot be a "registered domain" by the eTLD+1 definition.
  if (n <= best_match_len) {
    return(NA_character_)
  }

  # Standard case: n > best_match_len
  # The registered domain is the public suffix (best_match_len parts)
  # plus one additional label to the left.
  return(paste(parts[(n - best_match_len):n], collapse = "."))
}

# Internal helper using the exact original get_tld logic for TLD extraction
# Uses hash set (environment) for O(1) lookup instead of linear %in% search.
# Results are memoized for performance.
._extract_tld_original_logic <- function(
  host_to_process,
  current_tld_set,
  tld_source_id = "all"
) {
  if (is.na(host_to_process) || !nzchar(host_to_process)) {
    return(NA_character_)
  }

  # Generate cache key including both host and source
  cache_key <- paste(host_to_process, tld_source_id, sep = "\x1F")
  cache_key <- stringi::stri_escape_unicode(enc2utf8(cache_key))

  # Check cache
  cached <- .cache_get("tld", cache_key)
  if (!identical(cached, .rurl_cache_sentinel)) {
    return(cached)
  }

  result <- ._extract_tld_impl(host_to_process, current_tld_set)

  # Cache the result
  .cache_set("tld", cache_key, result)
  result
}

# Internal implementation of TLD extraction (not memoized)
._extract_tld_impl <- function(host_to_process, current_tld_set) {
  normalized_host <- stringi::stri_trans_nfc(
    stringi::stri_trans_tolower(host_to_process)
  )
  encoded_host <- .normalize_and_punycode(normalized_host)

  parts <- .host_labels(encoded_host)
  if (is.null(parts)) {
    return(NA_character_)
  }

  n <- length(parts)

  if (n > 1L) {
    for (i in seq_len(n - 1L)) { # Checks suffixes of length n down to 2
      candidate <- paste(parts[i:n], collapse = ".") # Candidate is Punycode
      if (.in_set(candidate, current_tld_set)) { # O(1) lookup
        return(.punycode_to_unicode(candidate)) # Decodes matched Punycode TLD
      }
    }
  }

  if (n > 0L) { # Fallback to last part
    last_candidate <- parts[n] # Punycode
    if (.in_set(last_candidate, current_tld_set)) { # O(1) lookup
      return(.punycode_to_unicode(last_candidate))
    }
  }

  return(NA_character_)
}
