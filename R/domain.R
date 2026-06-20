# Domain/TLD derivation (PSL) and punycode helpers.
# DO NOT modify punycode logic.

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

  decoded_labels_invalid <- !is.character(decoded_labels) ||
    length(decoded_labels) != length(parts_puny)
  if (decoded_labels_invalid) {
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

# Public Suffix List queries are delegated to the pslr package. rurl maps its
# own source selection and output contract onto pslr's query API:
#
#   * source "all" / "icann" / "private"  -> pslr `section` of the same name.
#   * output is always Unicode (`output = "unicode"`), preserving rurl's
#     historical decoded-IDN output even though pslr defaults to ASCII A-labels.
#   * `unknown = "na"` so a host under an unknown TLD yields NA, matching rurl's
#     long-standing "no PSL match => NA" behavior rather than pslr's default
#     implicit `*` rule (which treats any unknown single label as a suffix).
#   * `invalid = "na"` so malformed hosts return NA instead of erroring, per
#     rurl's tolerant parsing contract.
#
# These helpers accept the host in any spelling pslr understands (Unicode,
# lower/mixed case, or A-label); pslr canonicalizes via punycoder internally, so
# callers no longer need to NFC-normalize or Punycode-encode the host first.

# Registered (eTLD+1) domain for a host, in Unicode. Vectorized.
.psl_registered_domain <- function(host, section = "all") {
  pslr::registrable_domain(
    host,
    section = section,
    output = "unicode",
    unknown = "na",
    invalid = "na"
  )
}

# Public suffix (TLD) for a host, in Unicode. Vectorized.
.psl_public_suffix <- function(host, section = "all") {
  pslr::public_suffix(
    host,
    section = section,
    output = "unicode",
    unknown = "na",
    invalid = "na"
  )
}

# Full canonical decomposition of a host, in Unicode. Vectorized; returns a
# data.frame with one row per input host and columns `subdomain`, `domain`,
# `suffix`, `registrable_domain` (plus the canonicalized `host`). Used to make
# STRUCTURAL policy decisions (subdomain presence, registrable boundary,
# subdomain label count) on a single canonical spelling so that an A-label and
# its Unicode equivalent take the same branch. pslr canonicalizes the host
# (case / NFC / IDNA) internally, so the decomposition is identical for both
# spellings. Same fixed contract as the other PSL seams: Unicode output, unknown
# TLDs and invalid hosts become NA rather than `*` / errors.
.psl_suffix_extract <- function(host, section = "all") {
  pslr::suffix_extract(
    host,
    section = section,
    output = "unicode",
    unknown = "na",
    invalid = "na"
  )
}
