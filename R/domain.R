# Domain/TLD derivation (PSL) and punycode helpers.
# DO NOT modify punycode logic.

# Internal helper to encode hostnames using IDNA (Punycode)
# Accepts encode_fn for testability and fallback. With the default encode_fn
# (production) it delegates to the vectorized .normalize_and_punycode_vec() so
# there is a single implementation; a non-default encode_fn (test double) takes
# the scalar path below, bypassing the shared cache by design.
.normalize_and_punycode <- function(host, encode_fn = punycoder::puny_encode) {
  if (identical(encode_fn, punycoder::puny_encode)) {
    return(.normalize_and_punycode_vec(host))
  }

  if (is.na(host) || !nzchar(host)) {
    return(host)
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
  encoded
}

# Vectorized IDNA/Punycode host encoder. Batch analogue of
# .normalize_and_punycode() that preserves its exact per-host semantics:
# NA/empty hosts pass through unchanged; each remaining host is NFC-normalized
# then Punycode-encoded, strictly first and falling back to a lenient
# (strict = FALSE) encode only for the hosts the strict pass rejects, and to NA
# only when even the lenient encode errors (matching the scalar case preserving,
# malformed-tolerant contract). Hosts are de-duplicated and memoized in the
# puny_encode cache, so each unique unmemoized host costs one R->C++ crossing.
# Always uses punycoder::puny_encode; the scalar wrapper keeps the encode_fn
# injection seam for test doubles.
.normalize_and_punycode_vec <- function(host) {
  result <- host
  process <- !is.na(host) & nzchar(host)
  if (!any(process)) {
    return(unname(result))
  }

  h <- host[process]
  uniq_hosts <- unique(h)
  hit <- logical(length(uniq_hosts))
  encoded_uniq <- rep(NA_character_, length(uniq_hosts))
  for (k in seq_along(uniq_hosts)) {
    cached <- .cache_get("puny_encode", uniq_hosts[k])
    if (!identical(cached, .rurl_cache_sentinel)) {
      hit[k] <- TRUE
      encoded_uniq[k] <- cached
    }
  }

  miss_idx <- which(!hit)
  if (length(miss_idx) > 0L) {
    nfc <- stringi::stri_trans_nfc(uniq_hosts[miss_idx])
    encoded <- tryCatch(
      punycoder::puny_encode(nfc, strict = TRUE),
      error = function(e) NULL
    )
    if (!is.character(encoded) || length(encoded) != length(nfc)) {
      # A label made the strict batch throw: reproduce the scalar per-host
      # strict -> lenient -> NA fallback exactly, element by element.
      encoded <- vapply(
        nfc,
        function(x) {
          tryCatch(
            punycoder::puny_encode(x, strict = TRUE),
            error = function(e) {
              tryCatch(
                punycoder::puny_encode(x, strict = FALSE),
                error = function(e2) NA_character_
              )
            }
          )
        },
        character(1),
        USE.NAMES = FALSE
      )
    }
    for (j in seq_along(miss_idx)) {
      encoded_uniq[miss_idx[j]] <- encoded[j]
      .cache_set("puny_encode", uniq_hosts[miss_idx[j]], encoded[j])
    }
  }

  result[process] <- encoded_uniq[match(h, uniq_hosts)]
  unname(result)
}

# Internal helper to decode Punycode domain parts to Unicode. With the default
# decode_fn (production) it delegates to the vectorized
# .punycode_to_unicode_vec() so there is a single implementation; a non-default
# decode_fn (test double) takes the scalar path below, bypassing the cache.
.punycode_to_unicode <- function(
  domain_puny,
  decode_fn = punycoder::puny_decode
) {
  if (identical(decode_fn, punycoder::puny_decode)) {
    return(.punycode_to_unicode_vec(domain_puny))
  }

  if (is.na(domain_puny)) {
    return(NA_character_)
  }
  if (!nzchar(domain_puny)) {
    return("")
  }

  parts_puny <- strsplit(domain_puny, ".", fixed = TRUE)[[1]]

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

  paste(sane_labels, collapse = ".")
}

# Vectorized Punycode -> Unicode decoder. Batch analogue of
# .punycode_to_unicode() preserving its exact per-host semantics: NA -> NA,
# "" -> "", and any other host decoded per label with the lenient
# (strict = FALSE) decode, an undecodable label falling back to its original
# spelling, iconv sanitizing to valid UTF-8, and the labels rejoined with ".".
# All hosts are split to labels once and decoded in a single flattened
# puny_decode call (regrouped by contiguous offsets, not split(), to avoid
# factor-level ordering pitfalls); on a batch throw or shape mismatch it falls
# back to a per-host decode that mirrors the scalar contract. De-duplicated and
# memoized in the puny_decode cache. Always uses punycoder::puny_decode; the
# scalar wrapper keeps the decode_fn injection seam for test doubles.
.punycode_to_unicode_vec <- function(domain_puny) {
  result <- rep(NA_character_, length(domain_puny))
  result[!is.na(domain_puny) & !nzchar(domain_puny)] <- ""
  process <- !is.na(domain_puny) & nzchar(domain_puny)
  if (!any(process)) {
    return(unname(result))
  }

  d <- domain_puny[process]
  uniq_hosts <- unique(d)
  hit <- logical(length(uniq_hosts))
  decoded_uniq <- rep(NA_character_, length(uniq_hosts))
  for (k in seq_along(uniq_hosts)) {
    cached <- .cache_get("puny_decode", uniq_hosts[k])
    if (!identical(cached, .rurl_cache_sentinel)) {
      hit[k] <- TRUE
      decoded_uniq[k] <- cached
    }
  }

  miss_idx <- which(!hit)
  if (length(miss_idx) > 0L) {
    miss_hosts <- uniq_hosts[miss_idx]
    parts_list <- strsplit(miss_hosts, ".", fixed = TRUE)
    lens <- lengths(parts_list)
    flat <- unlist(parts_list, use.names = FALSE)

    decoded_flat <- tryCatch(
      punycoder::puny_decode(flat, strict = FALSE),
      error = function(e) NULL
    )
    if (!is.character(decoded_flat) || length(decoded_flat) != length(flat)) {
      # Batch decode threw or returned an unexpected shape: fall back to the
      # scalar per-host contract (a failed host decodes to its original labels).
      decoded_list <- lapply(parts_list, function(p) {
        dl <- tryCatch(
          punycoder::puny_decode(p, strict = FALSE),
          error = function(e) rep(NA_character_, length(p)) # nocov
        )
        if (!is.character(dl) || length(dl) != length(p)) {
          rep(NA_character_, length(p)) # nocov
        } else {
          dl
        }
      })
      decoded_flat <- unlist(decoded_list, use.names = FALSE)
    }

    na_lab <- is.na(decoded_flat)
    decoded_flat[na_lab] <- flat[na_lab]
    sane <- iconv(decoded_flat, from = "UTF-8", to = "UTF-8", sub = "")
    sane[is.na(sane)] <- ""

    ends <- cumsum(lens)
    starts <- ends - lens + 1L
    for (j in seq_along(miss_idx)) {
      rejoined <- paste(sane[starts[j]:ends[j]], collapse = ".")
      decoded_uniq[miss_idx[j]] <- rejoined
      .cache_set("puny_decode", miss_hosts[j], rejoined)
    }
  }

  result[process] <- decoded_uniq[match(d, uniq_hosts)]
  unname(result)
}

# Public Suffix List queries are delegated to the pslr package. rurl maps its
# own source selection and output contract onto pslr's query API:
#
#   * source "all" / "icann" / "private"  -> pslr `section` of the same name.
#   * output defaults to Unicode (`output = "unicode"`), preserving rurl's
#     historical decoded-IDN output even though pslr defaults to ASCII A-labels.
#     Structural/decision callers (the www and subdomain-trim heuristics) keep
#     this default so an A-label host and its Unicode form take the same branch.
#     The emitted-value path (.derive_domain_tld -> parsed$domain / $tld)
#     instead selects the spelling from host_encoding: "unicode", "ascii"
#     ("idna"), or the input's own spelling ("keep", the default; see
#     .host_is_ace()).
#   * `unknown = "na"` so a host under an unknown TLD yields NA, matching rurl's
#     long-standing "no PSL match => NA" behavior rather than pslr's default
#     implicit `*` rule (which treats any unknown single label as a suffix).
#   * `invalid = "na"` so malformed hosts return NA instead of erroring, per
#     rurl's tolerant parsing contract.
#
# These helpers accept the host in any spelling pslr understands (Unicode,
# lower/mixed case, or A-label); pslr canonicalizes via punycoder internally, so
# callers no longer need to NFC-normalize or Punycode-encode the host first.

# TRUE if any label of `host` is an ACE label (the "xn--" A-label prefix). Used
# by the "keep" host_encoding to decide whether a derived domain/TLD should
# mirror the input's punycode spelling (ASCII A-labels) rather than be decoded
# to Unicode. Scalar input.
.host_is_ace <- function(host) {
  if (length(host) != 1L || is.na(host) || !nzchar(host)) {
    return(FALSE)
  }
  grepl("(^|\\.)xn--", host, ignore.case = TRUE)
}

# Vectorized .host_is_ace(): TRUE per element when any label is an ACE A-label.
# NA or empty hosts are FALSE. Used by the vectorized domain/TLD phase to pick
# the emitted spelling under host_encoding = "keep".
.host_is_ace_vec <- function(host) {
  res <- grepl("(^|\\.)xn--", host, ignore.case = TRUE)
  res[is.na(host) | !nzchar(host)] <- FALSE
  res
}

# Registered (eTLD+1) domain for a host. Vectorized. `output` selects the
# spelling: "unicode" (default, preserving rurl's historical decoded-IDN output)
# or "ascii" (lowercase A-labels). The structural callers that only need a
# canonical decomposition keep the Unicode default; the emitted-value path
# (.derive_domain_tld) overrides it to honor host_encoding.
.psl_registered_domain <- function(host, section = "all", output = "unicode") {
  pslr::registrable_domain(
    host,
    section = section,
    output = output,
    unknown = "na",
    invalid = "na"
  )
}

# Public suffix (TLD) for a host. Vectorized. `output` as in
# .psl_registered_domain().
.psl_public_suffix <- function(host, section = "all", output = "unicode") {
  pslr::public_suffix(
    host,
    section = section,
    output = output,
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

# --- punycoder DNS-length / UTS-46 diagnostic probe --------------------------
#
# Delegates the url_standard DNS-length/UTS-46 diagnostic seam (T6,
# RURL-vowqpmdg) entirely to `punycoder::host_normalize()`; rurl owns only the
# two structural detectors (empty-label, length subtyping) that
# `host_normalize()` cannot itself express as an isolated flag. The algorithm
# is a LOCKED design from T5 (RURL-kqmpbwye) -- see
# `_scratch/T5-dns-uts46-probe-design-lock.md` for the full empirical
# derivation and `tests/testthat/test-punycoder-host-probe-characterization.R`
# for the version-drift tripwire pinning these exact `host_normalize()` call
# shapes against the currently-installed punycoder. Do not re-derive the
# design below without reading that doc first.
#
# Design summary:
#   * An ALL-STRICT baseline with one flag relaxed at a time is ambiguous (a
#     host failing 2-of-3 checks is indistinguishable from one failing
#     3-of-3). The correct design inverts this: an ALL-RELAXED baseline, then
#     exactly one flag ENABLED per isolated call. Each isolated call's
#     NA/non-NA reading is then an independent fact about that one check
#     alone, regardless of how many OTHER checks are simultaneously failing.
#   * The 3 isolated calls (`call_a`/`call_b`/`call_c`, one per flag) are only
#     trustworthy when `baseline` is non-NA; a NA baseline means a structural
#     problem outside all 3 flags (only "domain-empty-label" in practice, e.g.
#     "a..com"), never "fails all 3 checks".
#   * `domain-empty-label` is a direct strsplit check, not a probe call: it is
#     cheaper than a scoped `validate_domain()` call and does not compete with
#     `host_normalize()`'s ambiguity at all (it never inspects other rules).
#   * Length subtyping (label-too-long vs. name-too-long, independent and
#     co-firing facts) reuses `baseline`'s own ACE-encoded (xn--...) output --
#     never the raw input host -- because DNS length limits apply to the
#     punycode-encoded label, not the raw Unicode codepoint count (boundary
#     verified exactly at 63/253 octets on the ACE form). A scoped
#     `validate_domain()` call was considered and rejected for this: it
#     collapses both length facts to a single code when they co-occur.
#   * `domain-std3-violation` (isolated `use_std3`) is a verified STRICT
#     SUPERSET of WHATWG's forbidden-host-code-point set: it also rejects
#     non-LDH ASCII punctuation (`_ + ~ * $`) that WHATWG does not itself
#     forbid at the host-code-point level. Its meaning is "this host violates
#     STD3 ASCII hostname rules", never narrowly "contains a WHATWG-forbidden
#     code point".
#
# Callers are responsible for restricting `host` to the rows worth probing --
# in particular, excluding IP literals. `use_std3` treats a bracketed IPv6
# literal's "[", "]", ":" as violations (verified empirically), which would
# misclassify every IPv6 host as a STD3 violation, and DNS-length/UTS-46
# rules are meaningless for an IP literal in the first place. NA/empty
# elements are treated as "nothing to probe" and read FALSE for every fact
# (mirrors the empty-label-is-ambiguous-NA guard, without polluting a logical
# vector with NA via `nzchar(NA)`).
#
# Vectorized; not deduplicated/cached (callers already dedup at the URL
# level via ._url_metadata_vec()'s unique(url), and T5's benchmark found the
# 4-call probe cost negligible -- ~12 microseconds/host -- so no cheaper
# design is required here).
#
# Returns a list of 5 parallel logical vectors, same length as `host`:
#   label_too_long, name_too_long, empty_label, hyphen_violation,
#   std3_violation.
.punycoder_host_probe <- function(host) {
  n <- length(host)
  label_too_long <- rep(FALSE, n)
  name_too_long <- rep(FALSE, n)
  empty_label <- rep(FALSE, n)
  hyphen_violation <- rep(FALSE, n)
  std3_violation <- rep(FALSE, n)
  out <- list(
    label_too_long = label_too_long,
    name_too_long = name_too_long,
    empty_label = empty_label,
    hyphen_violation = hyphen_violation,
    std3_violation = std3_violation
  )

  probe_idx <- which(!is.na(host) & nzchar(host))
  if (length(probe_idx) == 0L) {
    return(out)
  }

  h <- host[probe_idx]

  # domain-empty-label: direct structural detector, no host_normalize() call.
  # strsplit(..., fixed = TRUE) drops a trailing "" for a trailing dot
  # ("a.com." -> c("a", "com")), which is exactly the FQDN-tolerant behavior
  # host_normalize() itself exhibits -- do not swap for stringi's
  # stri_split_fixed(), which keeps the trailing "" and would misfire on a
  # valid trailing-root-dot FQDN (rurl house convention; see CLAUDE.md).
  labels_list <- strsplit(h, ".", fixed = TRUE)
  out$empty_label[probe_idx] <- vapply(
    labels_list,
    function(labels) !all(nzchar(labels)) || length(labels) == 0L,
    logical(1)
  )

  # Accepted design (T5): all-relaxed baseline, then one flag enabled at a
  # time. Only rows where the baseline succeeds feed the 3 isolated calls.
  baseline <- punycoder::host_normalize(
    h, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
  )
  baseline_ok <- which(!is.na(baseline))
  if (length(baseline_ok) == 0L) {
    return(out)
  }

  ok_idx <- probe_idx[baseline_ok]
  hb <- h[baseline_ok]

  call_a <- punycoder::host_normalize(
    hb, check_hyphens = TRUE, use_std3 = FALSE, verify_dns_length = FALSE
  )
  call_b <- punycoder::host_normalize(
    hb, check_hyphens = FALSE, use_std3 = TRUE, verify_dns_length = FALSE
  )
  call_c <- punycoder::host_normalize(
    hb, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = TRUE
  )

  out$hyphen_violation[ok_idx] <- is.na(call_a)
  out$std3_violation[ok_idx] <- is.na(call_b)

  # Length subtyping: only meaningful where call_c genuinely failed. Both
  # facts are independent booleans and can co-fire on the same host.
  length_failed <- which(is.na(call_c))
  if (length(length_failed) > 0L) {
    base_ace <- baseline[baseline_ok][length_failed]
    len_idx <- ok_idx[length_failed]
    ace_labels <- strsplit(base_ace, ".", fixed = TRUE)
    out$label_too_long[len_idx] <- vapply(
      ace_labels, function(x) any(nchar(x) > 63L), logical(1)
    )
    out$name_too_long[len_idx] <- nchar(sub("[.]$", "", base_ace)) > 253L
  }

  out
}
