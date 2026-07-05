# Diagnostics + host_type infrastructure for the url_standard selector
# (RURL-csdrxdoj, epic RURL-eqzkkohm; PRD §6.2, §6.3, §7).
#
# This file ships the *plumbing* and the (initially empty) companion-helper
# surface. The host model (RURL-luwvkwhd) and the two path profiles
# (RURL-gjltzwmp / RURL-bbmuehsx) fill the single emit seam
# (.derive_url_metadata_vec) with the actual host_type classification and
# diagnostic tokens; they must emit INTO this accumulator, not invent their own
# shape.
#
# Design decision (PRD §6.3, resolves Open Question 1): metadata is surfaced
# through COMPANION HELPERS -- get_host_type() and get_url_diagnostics() -- and
# is deliberately NOT added as columns on safe_parse_urls() or fields on the
# safe_parse_url() list. That keeps the url_standard = NULL compatibility story
# airtight: with no selector nothing is widened and nothing is even parsed here,
# so every existing function's output shape is byte-for-byte unchanged
# (acceptance criterion 1). The whole path is a no-op when url_standard is NULL.

# --- Vocabulary (single source of truth) -------------------------------------

# The v1 diagnostics vocabulary (PRD §7). These names are AUTHORITATIVE and
# supersede the provisional research-doc drafts (`non-decimal-ipv4`,
# `ambiguous-octet`, `decoded-reserved`). Downstream consumers (pagerankr,
# sitemapr, semantic) gate on these tokens. The host tickets emit the `ipv4-*`
# tokens; the path tickets emit `encoded-dot-segment` /
# `encoded-reserved-path-byte`. `.diag_add()` rejects any token not listed here.
.URL_DIAGNOSTICS <- c(
  "ipv4-number-form",
  "ipv4-non-dotted",
  "ipv4-short-form",
  "ipv4-non-decimal",
  "ipv4-octal",
  "ipv4-leading-zero",
  "ipv4-out-of-range",
  "encoded-dot-segment",
  "encoded-reserved-path-byte"
)

# The host_type vocabulary (PRD §6.3). get_host_type() emits exactly one of
# these per URL once the host model (RURL-luwvkwhd) lands; T2 leaves it NA.
# host_type is a function of (host, url_standard), not the host string alone:
# `2130706433` is `reg-name` under rfc3986 and `ipv4` under whatwg.
.HOST_TYPES <- c("domain", "ipv4", "ipv6", "reg-name", "missing")

# --- Diagnostics accumulator -------------------------------------------------
#
# A per-URL list of character vectors (one URL can carry several tokens). The
# emit API is vectorized: .diag_add() appends a token to every position matching
# a logical mask, which is the exact shape the host/path phases push in
# ("for the rows with this shape, add this token").

# Fresh accumulator: a length-n list of empty character vectors.
.diag_new <- function(n) {
  if (n == 0L) {
    return(list())
  }
  rep(list(character(0)), n)
}

# Append `token` to each accumulator element where `mask` is TRUE. `token` must
# be a single string from .URL_DIAGNOSTICS -- the guard catches typos in the
# emitting tickets at dev time rather than silently coining a new vocabulary.
.diag_add <- function(diag, mask, token) {
  if (length(token) != 1L || !token %in% .URL_DIAGNOSTICS) {
    stop(
      "diagnostic token must be one of .URL_DIAGNOSTICS; got ",
      deparse(token),
      call. = FALSE
    )
  }
  mask[is.na(mask)] <- FALSE
  if (any(mask)) {
    diag[mask] <- lapply(diag[mask], c, token)
  }
  diag
}

# --- Emit seam ---------------------------------------------------------------

# THE seam the host + path tickets fill in. Given the Stage-A parse columns `a`
# (option-independent host/path core), the validated `opts` (carrying the
# selected `opts$url_standard`), and the row count `n`, derive:
#   - host_type: one .HOST_TYPES token per row (or NA).
#   - diagnostics: a .diag_new(n) accumulator, tokens pushed via .diag_add().
#
# T2 ships it empty: NA host_type and no diagnostics for every row. Later
# tickets add their emit rules in the marked blocks below WITHOUT changing the
# return shape.
.derive_url_metadata_vec <- function(a, opts, n) {
  host_type <- rep(NA_character_, n)
  diag <- .diag_new(n)
  null_row <- attr(a, "null_row")
  if (is.null(null_row)) {
    null_row <- rep(FALSE, n)
  }
  live <- !null_row

  # --- host model (RURL-luwvkwhd, PRD §6.2) ----------------------------------
  # host_type is an (host, url_standard) function. Read the POST-model Stage-A
  # host/IP flag (a$final_host / a$is_ip_host: RFC reg-name vs WHATWG-coerced
  # IPv4) and the PSL registrable domain (a$domain_ascii). Null/error rows keep
  # host_type = NA.
  final_host <- a$final_host
  host_missing <- is.na(final_host) | !nzchar(final_host)
  host_type[live & host_missing] <- "missing"

  is_ip <- a$is_ip_host & !is.na(a$is_ip_host)
  host_has_colon <- stringi::stri_detect_fixed(final_host, ":")
  host_has_colon[is.na(host_has_colon)] <- FALSE
  host_type[live & !host_missing & is_ip & host_has_colon] <- "ipv6"
  host_type[live & !host_missing & is_ip & !host_has_colon] <- "ipv4"

  has_domain <- !is.na(a$domain_ascii) & nzchar(a$domain_ascii)
  non_ip <- live & !host_missing & !is_ip
  host_type[non_ip & has_domain] <- "domain"
  host_type[non_ip & !has_domain] <- "reg-name"

  # ipv4-* diagnostics keyed to the ORIGINAL host SHAPE, in BOTH modes ("facts
  # not policy"): a numeric-looking reg-name under RFC carries the same tokens
  # it would under WHATWG. Suppressed on null rows (a WHATWG-fatal numeric host
  # is an error, so it reports no diagnostics).
  input_host <- a$input_host
  if (is.null(input_host)) {
    input_host <- rep(NA_character_, n)
  }
  for (i in which(live)) {
    tokens <- .ipv4_host_diagnostics(input_host[i])
    for (tok in tokens) {
      diag <- .diag_add(diag, seq_len(n) == i, tok)
    }
  }

  # --- path diagnostics (RURL-gjltzwmp / RURL-bbmuehsx, PRD §6.1, §7) ---------
  # Emit `encoded-dot-segment` when an encoded-dot segment is recognized/removed
  # and `encoded-reserved-path-byte` when the clean path preserves an encoded
  # reserved byte (%2F / %3F / %23). Both v1 profiles ALWAYS resolve dot
  # segments and NEVER decode reserved bytes (PRD §6.1), so -- like the ipv4-*
  # host tokens above -- these are shape facts keyed to the raw path alone,
  # fired identically in RFC and WHATWG mode (PRD "path-diagnostic triggers"
  # note), independent of which profile's transform actually performed the
  # removal.
  raw_path <- a$raw_path
  if (is.null(raw_path)) {
    raw_path <- rep(NA_character_, n)
  }
  has_path <- live & !is.na(raw_path)

  has_reserved_byte <- has_path &
    stringi::stri_detect_regex(raw_path, "(?i)%(2f|3f|23)")
  diag <- .diag_add(diag, has_reserved_byte, "encoded-reserved-path-byte")

  has_encoded_dot <- has_path &
    stringi::stri_detect_regex(
      raw_path, "(?i)(^|/)(%2e|\\.%2e|%2e\\.|%2e%2e)(/|$)"
    )
  diag <- .diag_add(diag, has_encoded_dot, "encoded-dot-segment")

  list(host_type = host_type, diagnostics = diag)
}

# --- IPv4 host shape diagnostics ---------------------------------------------

# ipv4-ish host token: dot-separated groups, each all-decimal or 0x-hex. Matches
# the Phase-1 classifier's own `ipv4ish` test, so the diagnostics are keyed to
# exactly the tokens the host model treats as IPv4 attempts.
.RE_IPV4ISH <-
  "^(0[xX][0-9a-fA-F]+|[0-9]+)(\\.(0[xX][0-9a-fA-F]+|[0-9]+))*$"

# Parse one IPv4 part to its numeric value (as a double, so 32-bit values stay
# exact) honoring the WHATWG radix rules: `0x`/`0X` prefix -> hex, a leading
# `0` on a multi-digit part -> octal, else decimal. Returns NA for a digit
# outside the detected radix (e.g. the `8` in octal `08`).
.parse_ipv4_part_value <- function(part) {
  if (grepl("^0[xX]", part)) {
    digits <- sub("^0[xX]", "", part)
    base <- 16
  } else if (grepl("^0[0-9]+$", part)) {
    digits <- part
    base <- 8
  } else {
    digits <- part
    base <- 10
  }
  chars <- strsplit(digits, "", fixed = TRUE)[[1]]
  vals <- match(toupper(chars), c(0:9, "A", "B", "C", "D", "E", "F")) - 1
  if (length(vals) == 0L || anyNA(vals) || any(vals >= base)) {
    return(NA_real_)
  }
  Reduce(function(acc, d) acc * base + d, vals, 0)
}

# Diagnostics for one host token, keyed to its SHAPE (PRD §6.2 / §7). Returns
# character(0) for a non-IPv4-attempt host or a canonical dotted-quad. Emits
# every applicable token as an independent fact (the rule adopted for the two
# octal rows of §6.2): a leading-zero octal part is at once octal, leading-zero,
# and non-decimal.
.ipv4_host_diagnostics <- function(token) {
  if (is.na(token) || !grepl(.RE_IPV4ISH, token)) {
    return(character(0))
  }
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  k <- length(parts)

  is_hex <- grepl("^0[xX][0-9a-fA-F]+$", parts)
  # Leading-zero decimal-shaped part (010, 0177, 08): an octal attempt.
  is_lead_zero <- grepl("^0[0-9]+$", parts)
  is_plain_dec <- !is_hex & !is_lead_zero

  tokens <- character(0)
  if (k == 1L) {
    tokens <- c(tokens, "ipv4-number-form")
    if (is_plain_dec[1L]) {
      tokens <- c(tokens, "ipv4-non-dotted")
    }
  } else if (k < 4L) {
    tokens <- c(tokens, "ipv4-short-form")
  }
  if (any(is_hex | is_lead_zero)) {
    tokens <- c(tokens, "ipv4-non-decimal")
  }
  if (any(is_lead_zero)) {
    tokens <- c(tokens, "ipv4-octal", "ipv4-leading-zero")
  }

  # Out-of-range: only meaningful for a dotted host of a valid IPv4 arity (2-4
  # parts) -- a single whole-host number is a valid shorthand (not out of range)
  # and a > 4-part host is a structural non-IPv4 (a numeric reg-name), not a
  # per-part range problem. Each of the first k-1 parts must be <= 255; the last
  # may fill the remaining octets (max 256^(5-k) - 1).
  if (k >= 2L && k <= 4L) {
    values <- vapply(
      parts, .parse_ipv4_part_value, numeric(1), USE.NAMES = FALSE
    )
    head_over <- any(values[-k] > 255, na.rm = TRUE)
    last_max <- 256^(5 - k) - 1
    last_over <- !is.na(values[k]) && values[k] > last_max
    if (head_over || last_over) {
      tokens <- c(tokens, "ipv4-out-of-range")
    }
  }

  tokens
}

# --- Metadata pipeline -------------------------------------------------------

# Compute the (host_type, diagnostics) metadata for a vector of URLs under the
# validated `opts`. Deliberately SEPARATE from the 14-column parse result so it
# widens neither safe_parse_urls() nor safe_parse_url() (AC #1). Returns a list
# with `host_type` (length-n character vector) and `diagnostics` (length-n list
# of character vectors).
#
# Fast path / compatibility contract: with no selector there is no metadata
# surface, so this returns NA host_type + empty diagnostics WITHOUT parsing.
# When a standard is selected it parses the option-independent Stage-A host/path
# core once over the unique inputs (uncached; RURL-luwvkwhd owns the Stage-A
# cache-scope decision) and hands those columns to the emit seam.
._url_metadata_vec <- function(url, opts) {
  n <- length(url)
  empty <- list(host_type = rep(NA_character_, n), diagnostics = .diag_new(n))

  # No selector (or empty input): nothing to classify or flag.
  if (is.null(opts$url_standard) || n == 0L) {
    return(empty)
  }

  # Parse the host/path shape once per unique input, then expand per row.
  uniq <- unique(url)
  a <- ._parse_stage_a_vec(uniq, opts)
  meta_uniq <- .derive_url_metadata_vec(a, opts, length(uniq))

  pos <- match(url, uniq)
  list(
    host_type = meta_uniq$host_type[pos],
    diagnostics = meta_uniq$diagnostics[pos]
  )
}
