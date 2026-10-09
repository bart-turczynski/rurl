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

# Vectorized IDNA/Punycode host encoder. Batch analog of
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

# Split hosts into their labels for the Punycode DECODE seam, preserving empty
# labels — including the trailing one a root-dot FQDN ends with (RURL-eikgtrqf).
#
# This is the one dot split that deliberately does NOT use base
# `strsplit(..., fixed = TRUE)`. ADR 0005 keeps that call for the STRUCTURAL
# host/subdomain splits and records exactly why it is not `stringi`-equivalent:
# it drops the trailing empty, so `"example.com."` splits to `c("example",
# "com")`. A decision caller does not care — but this seam rejoins its labels
# and returns the result as the rendered host, so the dropped label is dropped
# from the OUTPUT. That silently collapsed `example.com.` onto `example.com`,
# two hosts P3.2 (KJ-O1..O8) holds distinct, under `host_encoding = "unicode"`
# alone. Preserving the label count is the whole point here, so the documented
# equivalence gap is the reason to take the `stringi` side of it, not to avoid
# it. Do not "align" this back to `strsplit` for consistency with the splits
# ADR 0005 names.
.split_host_labels <- function(hosts) {
  stringi::stri_split_fixed(hosts, ".")
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

  parts_puny <- .split_host_labels(domain_puny)[[1]]

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

# Vectorized Punycode -> Unicode decoder. Batch analog of
# .punycode_to_unicode() preserving its exact per-host semantics: NA -> NA,
# "" -> "", and any other host decoded per label with the lenient
# (strict = FALSE) decode, .ace_decode_settle() retrying a rejected A-label
# with rurl's own decode and keeping the original spelling of a label that
# still fails or holds a URL delimiter, iconv sanitizing to valid UTF-8, and
# the labels rejoined with ".".
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
    parts_list <- .split_host_labels(miss_hosts)
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

    decoded_flat <- .ace_decode_settle(flat, decoded_flat)
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

# Settles each label after punycoder's lenient decode (RURL-oizpyvdz). An
# `xn--` label (any ASCII case) punycoder rejected is decoded by
# `.rfc3492_decode()`: punycoder 1.3.0 rejects non-LDH basic code points
# (`xn--a_-wia`), which RFC 3492 section 6.2 accepts and UTS #46 section 4
# decodes with. An `xn--` label whose payload holds a URL delimiter
# (# / : ? @) keeps its spelling whichever decoder would have read it, so a
# rendered host never gains one (RFC 3986 section 3.2.2 reg-name). Any other
# label a decode fails or empties keeps its spelling too.
#
# punycoder 1.2.1 rejected the same delimiter labels, except a `:` whose
# prefix is not scheme-shaped, which it decoded. Such a label reaches the
# helper only as a percent-decoded `%3A` under `rfc3986`; everywhere else the
# rendering is byte-identical to 1.2.1's (measured on 36,149 fuzzed labels,
# against a 1.2.1 build without libidn2; a build linked against it also reads
# `_` as a Punycode digit, which RFC 3492 section 5 does not allow, and 1.3.0
# does not). An amendment to ADR 0002 records this.
#
# The patterns match bytes (`useBytes = TRUE`) so a label that is not valid
# UTF-8 neither warns nor throws; `.rfc3492_decode()` returns NA for it.
.ace_decode_settle <- function(labels, decoded) {
  ace <- grepl("^[Xx][Nn]--", labels, useBytes = TRUE)
  delim <- ace & grepl("^[Xx][Nn]--.*[#/:?@]", labels, useBytes = TRUE)
  retry <- ace & !delim & is.na(decoded)
  if (any(retry)) {
    payload <- sub("^[Xx][Nn]--", "", labels[retry], useBytes = TRUE)
    decoded[retry] <- vapply(payload, .rfc3492_decode, character(1),
      USE.NAMES = FALSE
    )
  }
  keep <- delim | is.na(decoded) | (retry & !nzchar(decoded))
  decoded[keep] <- labels[keep]
  decoded
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
#
# `.grepl_decodable()` rather than bare `grepl()` (RURL-kmpnbvdl): an
# undecodable host reaches here, and `ignore.case = TRUE` without `perl` is the
# WIDE-CHARACTER path, which warns `unable to translate '<80>!' to a wide
# string` and resolves case against LC_CTYPE. FALSE is the same answer it
# already produced -- "xn--" is ASCII and an undecodable host is not a valid
# A-label either way -- but now it is reached without consulting the locale.
.host_is_ace_vec <- function(host) {
  res <- .grepl_decodable("(^|\\.)xn--", host, ignore.case = TRUE)
  res[is.na(host) | !nzchar(host)] <- FALSE
  res
}

# Strict IDNA2008 domain validity, vectorized. Unlike .normalize_and_punycode()
# -- which is deliberately TOLERANT of malformed-but-encodable hosts (ADR 0002)
# and therefore CANNOT be used to decide conformance -- this seam delegates to
# punycoder's strict validator to answer a yes/no IDNA question. Used only by
# the email/userinfo diagnostics (mailto_domain_form = idna2008-domain), never
# by the parse/present pipeline. NA in -> NA out; TRUE only for a domain that
# passes strict IDNA2008.
.validate_idna_domain_vec <- function(host) {
  res <- rep(NA, length(host))
  ok_in <- !is.na(host) & nzchar(host)
  if (any(ok_in)) {
    v <- punycoder::validate_domain(host[ok_in], strict = TRUE)
    res[ok_in] <- as.logical(v$valid)
  }
  res
}

# Registered (eTLD+1) domain for a host. Vectorized. `output` selects the
# spelling: "unicode" (default, preserving rurl's historical decoded-IDN output)
# or "ascii" (lowercase A-labels). The structural callers that only need a
# canonical decomposition keep the Unicode default; the emitted-value path
# (.derive_domain_tld) overrides it to honor host_encoding.
#
# `engine` is the per-request pslr engine seam (RURL-mhibnqbd): NULL (the
# default) OMITS the argument entirely so pslr resolves against its own
# session-global default engine -- byte-identical to the pre-engine behavior --
# while a `pslr::psl_engine()` snapshot resolves against that specific list.
# NULL must not be forwarded: pslr rejects `engine = NULL` (it wants a
# `psl_engine`), so the omit-when-NULL contract is load-bearing.
.psl_registered_domain <- function(host, section = "all", output = "unicode",
                                   engine = NULL) {
  args <- list(
    host,
    section = section,
    output = output,
    unknown = "na",
    invalid = "na"
  )
  if (!is.null(engine)) {
    args$engine <- engine
  }
  do.call(pslr::registrable_domain, args)
}

# Public suffix (TLD) for a host. Vectorized. `output` / `engine` as in
# .psl_registered_domain().
.psl_public_suffix <- function(host, section = "all", output = "unicode",
                               engine = NULL) {
  args <- list(
    host,
    section = section,
    output = output,
    unknown = "na",
    invalid = "na"
  )
  if (!is.null(engine)) {
    args$engine <- engine
  }
  do.call(pslr::public_suffix, args)
}

# Full canonical decomposition of a host, in Unicode. Vectorized; returns a
# data.frame with one row per input host and columns `subdomain`, `domain`,
# `suffix`, `registrable_domain` (plus the canonicalized `host`). Used to make
# STRUCTURAL policy decisions (subdomain presence, registrable boundary,
# subdomain label count) on a single canonical spelling so that an A-label and
# its Unicode equivalent take the same branch. pslr canonicalizes the host
# (case / NFC / IDNA) internally, so the decomposition is identical for both
# spellings. Same fixed contract as the other PSL seams: Unicode output, unknown
# TLDs and invalid hosts become NA rather than `*` / errors. `engine` as in
# .psl_registered_domain(): NULL omits the arg (session-global default engine).
.psl_suffix_extract <- function(host, section = "all", engine = NULL) {
  args <- list(
    host,
    section = section,
    output = "unicode",
    unknown = "na",
    invalid = "na"
  )
  if (!is.null(engine)) {
    args$engine <- engine
  }
  do.call(pslr::suffix_extract, args)
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
#     trustworthy when `baseline` is non-NA; a NA baseline means a problem
#     outside all 3 flags, never "fails all 3 checks". It has three causes: an
#     empty label ("a..com", "domain-empty-label") up to punycoder 1.3.0, an
#     invalid ACE label ("xn--a.com", "domain-invalid-ace-label"), and a
#     cross-label Bidi failure (criterion 9 of UTS #46 section 4.1 in a Bidi
#     domain name), which no token reports. On such a host the three flag facts
#     are unknown and read FALSE. From punycoder's -v3 profile the baseline
#     admits an empty label, as UTS #46 section 4.2 step 4 does without
#     VerifyDnsLength, so the isolated calls run on that host and a hyphen,
#     STD3 or length defect in it co-fires with "domain-empty-label". Every
#     empty label fails `call_c`; the length subtyping counts octets, so an
#     empty label alone never fires a length fact.
#   * `domain-invalid-ace-label` (RURL-vicyvlvh, ruling RUL-023) is decided PER
#     LABEL, never from `baseline`: each label beginning `xn--` (ASCII
#     case-insensitive) is judged ALONE, so an empty label or a Bidi neighbor
#     elsewhere in the host neither masks nor mimics it. rurl decodes the
#     payload itself (`.rfc3492_decode()`, RURL-mfmgauos), because punycoder's
#     decoder moved under it once: 1.3.0 rejects non-LDH basic code points,
#     which RFC 3492 and UTS #46 section 4 step 4 accept. A failed decode, an
#     empty or all-ASCII result, and a decoded label beginning `xn--`
#     (criterion 4, "If not CheckHyphens, the label must not begin with
#     'xn--'") are rurl's own checks. Criteria 1, 6, 7, 8 and single-label 9
#     come from the all-relaxed call (WHATWG's non-strict UTS #46 processing)
#     on the decoded label, which must map to itself (`.ace_label_table()`).
#     Criterion 5 (no U+002E) cannot fail: Punycode deltas never yield an ASCII
#     code point, and the label was split on dots. The characterization tests
#     pin all of this; `?get_url_diagnostics` states the completeness
#     guarantee consumers rely on. Genuine A-labels also reach the baseline
#     decoded, for the same reason.
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
# Returns a list of 6 parallel logical vectors, same length as `host`:
#   label_too_long, name_too_long, empty_label, hyphen_violation,
#   std3_violation, invalid_ace_label.
.punycoder_host_probe <- function(host) {
  n <- length(host)
  label_too_long <- rep(FALSE, n)
  name_too_long <- rep(FALSE, n)
  empty_label <- rep(FALSE, n)
  hyphen_violation <- rep(FALSE, n)
  std3_violation <- rep(FALSE, n)
  invalid_ace_label <- rep(FALSE, n)
  out <- list(
    label_too_long = label_too_long,
    name_too_long = name_too_long,
    empty_label = empty_label,
    hyphen_violation = hyphen_violation,
    std3_violation = std3_violation,
    invalid_ace_label = invalid_ace_label
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
  # valid trailing-root-dot FQDN (rurl house convention; see ADR 0005).
  labels_list <- strsplit(h, ".", fixed = TRUE)
  out$empty_label[probe_idx] <- vapply(
    labels_list,
    function(labels) !all(nzchar(labels)) || length(labels) == 0L,
    logical(1)
  )

  ace_table <- .ace_label_table(labels_list)
  out$invalid_ace_label[probe_idx] <- .invalid_ace_label_any(
    labels_list, ace_table$invalid
  )

  # Genuine A-labels reach punycoder already decoded (RURL-mfmgauos), so the
  # baseline and the isolated calls below judge the same Unicode label UTS #46
  # section 4 step 4 validates, and punycoder's own decoder is never on the
  # path. Invalid ACE labels stay as written and fail the baseline as before.
  if (length(ace_table$decoded) > 0L) {
    has_ace <- which(vapply(
      labels_list, function(labels) any(labels %in% names(ace_table$decoded)),
      logical(1)
    ))
    h[has_ace] <- vapply(has_ace, function(j) {
      labels <- labels_list[[j]]
      hit <- labels %in% names(ace_table$decoded)
      labels[hit] <- ace_table$decoded[labels[hit]]
      # strsplit() dropped one trailing "" for a trailing dot; restore it.
      trail <- if (endsWith(h[[j]], ".")) "." else ""
      paste0(paste(labels, collapse = "."), trail)
    }, character(1))
  }

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

# RFC 3492 section 6.2 decode of one ACE payload (the label after `xn--`),
# returning the decoded code points as a UTF-8 string, or NA on any failure.
# rurl owns this decode (RURL-mfmgauos) so that the `domain-invalid-ace-label`
# predicate and the relaxed baseline do not move when punycoder's decoder
# does: punycoder 1.3.0 rejects non-LDH basic code points (`xn--a_-wia`),
# which RFC 3492 accepts and UTS #46 section 4 step 4 decodes with. This is
# not one of the ADR 0002 helpers, which stay on punycoder.
#
# Follows the RFC's reference decoder exactly: the basic string is everything
# before the LAST delimiter, and only when that delimiter is not at position
# 0; a digit outside a-z/A-Z/0-9, a truncated integer, overflow past
# 0x7FFFFFFF, an inserted basic code point, or a result outside Unicode
# scalar values fails. Arithmetic is in doubles, exact below 2^53.
.rfc3492_decode <- function(payload) {
  base <- 36
  tmin <- 1
  tmax <- 26
  maxint <- 2147483647
  adapt <- function(delta, numpoints, firsttime) {
    delta <- if (firsttime) delta %/% 700 else delta %/% 2
    delta <- delta + delta %/% numpoints
    k <- 0
    while (delta > ((base - tmin) * tmax) %/% 2) {
      delta <- delta %/% (base - tmin)
      k <- k + base
    }
    k + ((base - tmin + 1) * delta) %/% (delta + 38)
  }
  cp <- utf8ToInt(payload)
  if (anyNA(cp) || any(cp >= 128L)) {
    return(NA_character_)
  }
  delims <- which(cp == 45L)
  b <- if (length(delims) > 0L) max(delims) - 1L else 0L
  out <- if (b > 0L) cp[seq_len(b)] else integer(0)
  pos <- if (b > 0L) b + 2L else 1L
  n <- 128
  i <- 0
  bias <- 72
  len <- length(cp)
  while (pos <= len) {
    oldi <- i
    w <- 1
    k <- base
    repeat {
      if (pos > len) {
        return(NA_character_)
      }
      ch <- cp[pos]
      pos <- pos + 1L
      digit <- if (ch >= 48L && ch <= 57L) {
        ch - 22L
      } else if (ch >= 65L && ch <= 90L) {
        ch - 65L
      } else if (ch >= 97L && ch <= 122L) {
        ch - 97L
      } else {
        return(NA_character_)
      }
      if (digit > (maxint - i) %/% w) {
        return(NA_character_)
      }
      i <- i + digit * w
      t <- if (k <= bias) tmin else if (k >= bias + tmax) tmax else k - bias
      if (digit < t) {
        break
      }
      if (w > maxint %/% (base - t)) {
        return(NA_character_)
      }
      w <- w * (base - t)
      k <- k + base
    }
    npts <- length(out) + 1
    bias <- adapt(i - oldi, npts, oldi == 0)
    if (i %/% npts > maxint - n) {
      return(NA_character_)
    }
    n <- n + i %/% npts
    i <- i %% npts
    if (n < 128 || n > 1114111 || (n >= 55296 && n <= 57343)) {
      return(NA_character_)
    }
    out <- append(out, as.integer(n), after = i)
    i <- i + 1
  }
  intToUtf8(out)
}

# Decode each distinct label that begins `xn--` (ASCII case-insensitive) and
# judge it against RUL-023's predicate. Returns a list: `invalid`, the labels
# (as written) that are not genuine A-labels, and `decoded`, a character
# vector of the genuine ones' decoded forms named by the label as written.
#
# Only ASCII letters are folded to lowercase, which is all the UTS #46 mapping
# step does to an ASCII label. A label is invalid when rurl's RFC 3492 decode
# fails (a non-ASCII code point in the label included, as section 4 step 4
# requires), yields an empty or all-ASCII result, or yields a label beginning
# `xn--` (criterion 4, which punycoder's relaxed call does not enforce). The
# remaining section 4.1 criteria are judged by the relaxed `host_normalize()`
# call on the DECODED label, which must come back as the same label: it maps
# and NFC-normalizes Unicode input, so a decoded label carrying a mapped or
# ignored code point, or one not in NFC, would otherwise be repaired rather
# than rejected. The round trip goes through rurl's own decode, so punycoder's
# decoder is never consulted.
.ace_label_table <- function(labels_list) {
  all_labels <- unlist(labels_list, use.names = FALSE)
  ace <- unique(grep("^[Xx][Nn]--", all_labels, value = TRUE))
  none <- list(invalid = character(0), decoded = character(0))
  if (length(ace) == 0L) {
    return(none)
  }
  decode <- function(labels) {
    vapply(labels, function(x) .rfc3492_decode(substring(x, 5L)),
           character(1), USE.NAMES = FALSE)
  }
  folded <- chartr(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", ace
  )
  decoded <- decode(folded)
  all_ascii <- nchar(decoded, type = "bytes") == nchar(decoded, type = "chars")
  bad <- is.na(decoded) | all_ascii | startsWith(decoded, "xn--")
  check <- which(!bad)
  if (length(check) > 0L) {
    relaxed <- punycoder::host_normalize(
      decoded[check],
      check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    )
    same <- !is.na(relaxed) & startsWith(relaxed, "xn--") &
      !grepl(".", relaxed, fixed = TRUE)
    same[same] <- decode(relaxed[same]) == decoded[check][same]
    bad[check] <- !same
  }
  good <- decoded[!bad]
  names(good) <- ace[!bad]
  list(invalid = ace[bad], decoded = good)
}

# For each host's label vector, TRUE when any label is in `invalid`.
.invalid_ace_label_any <- function(
    labels_list, invalid = .ace_label_table(labels_list)$invalid) {
  vapply(labels_list, function(labels) any(labels %in% invalid), logical(1))
}
