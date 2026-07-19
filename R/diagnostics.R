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
  "encoded-reserved-path-byte",
  "explicit-default-port",
  "non-default-port",
  "invalid-reverse-solidus",
  "control-char-stripped",
  "host-charset-shimmed",
  "domain-label-too-long",
  "domain-name-too-long",
  "domain-empty-label",
  "domain-hyphen-violation",
  "domain-std3-violation",
  # --- Layer 5 SELECTED diagnostics (ADR 0012 D5, RURL-izsouyxs) -------------
  # These are SELECTED facts, NOT a conformance oracle: absence of any of them
  # NEVER implies conformance (D5). All fire only under scheme_acceptance =
  # "general" (the general-parser branch); the WHATWG validation-error names are
  # WHATWG-verbatim (deliberate mixed case for `invalid-URL-unit`). The
  # per-scheme facts below are rurl-coined kebab tokens (no reserved names exist
  # for them in ADR 0012 or the design PRDs; the email PRD's `userinfo-form`
  # etc. govern the SEPARATE D7 scheme-less thread, not these).
  "invalid-URL-unit",
  "invalid-credentials",
  "unicode-outside-rfc3986-uri",
  "transform-skipped-ineligible-scheme",
  "ws-fragment-forbidden",
  "ws-userinfo-forbidden",
  "mailto-fragment-discouraged",
  "tel-missing-phone-context",
  "data-missing-comma",
  "file-non-absolute-path",
  "file-forbidden-component"
)

# The host_type vocabulary (PRD §6.3). get_host_type() emits exactly one of
# these per URL once the host model (RURL-luwvkwhd) lands; T2 leaves it NA.
# host_type is a function of (host, url_standard), not the host string alone:
# `2130706433` is `reg-name` under rfc3986 and `ipv4` under whatwg.
.HOST_TYPES <- c("domain", "ipv4", "ipv6", "reg-name", "missing")

# The scheme_class vocabulary (PRD v2 §5 D7, RURL-jlvyjwog). get_scheme_class()
# emits exactly one of these per URL. Unlike host_type, the classification
# itself (which scheme is WHATWG-special) does not differ between "rfc3986"
# and "whatwg" -- it is a static fact about the resolved scheme, gated by
# url_standard the same way host_type is gated (NA with no selector at all).
.SCHEME_CLASSES <- c("special", "non-special", "missing-or-error")

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
.derive_url_metadata_vec <- function(a, opts, n, url = NULL) {
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

  # --- port diagnostics (RURL-qdlvldts, PRD v2 D1, §5.3) ---------------------
  # `explicit-default-port` / `non-default-port` are facts about the raw
  # (unfiltered) port versus the resolved scheme's WHATWG default port table
  # (.SCHEME_DEFAULT_PORTS) -- fired in BOTH standard modes (facts not policy,
  # the same pattern as the ipv4-* host tokens above), independent of the
  # standalone `port_handling` presentation knob. A scheme with no defined
  # default (ftps, or an absent/rejected scheme) can never match, so any port
  # present on it always registers as `non-default-port`.
  raw_port <- a$raw_port
  if (is.null(raw_port)) {
    raw_port <- rep(NA_integer_, n)
  }
  final_scheme <- a$final_scheme
  if (is.null(final_scheme)) {
    final_scheme <- rep(NA_character_, n)
  }
  has_port <- live & !is.na(raw_port)
  default_port <- .scheme_default_port_vec(final_scheme)
  is_default_port <- has_port & !is.na(default_port) & raw_port == default_port
  diag <- .diag_add(diag, is_default_port, "explicit-default-port")
  diag <- .diag_add(diag, has_port & !is_default_port, "non-default-port")

  # --- backslash diagnostics (RURL-ledntyab, PRD v2 D2, §5.2) ----------------
  # `invalid-reverse-solidus` fires exactly where Phase 1's WHATWG recognizer
  # (.rewrite_whatwg_backslashes_vec) actually reinterpreted a literal "\" as a
  # "/" -- never merely because a backslash is present, and never under
  # rfc3986 / no selector / a non-special scheme (ftps), where backslash stays
  # inert and the flag is always FALSE.
  backslash_rewritten <- a$backslash_rewritten
  if (is.null(backslash_rewritten)) {
    backslash_rewritten <- rep(FALSE, n)
  }
  diag <- .diag_add(
    diag, live & backslash_rewritten, "invalid-reverse-solidus"
  )

  # --- control-char strip diagnostic (RURL-tyetpjym) -------------------------
  # `control-char-stripped` fires exactly where Phase 1's WHATWG control-char
  # stripper (.strip_whatwg_control_chars_vec) actually removed an ASCII tab/LF/
  # CR from the input -- surfacing the mutation as a FACT (ADR 0006) rather than
  # silently stripping. Always FALSE under rfc3986 / no selector (that profile
  # rejects such bytes instead of stripping).
  control_char_stripped <- a$control_char_stripped
  if (is.null(control_char_stripped)) {
    control_char_stripped <- rep(FALSE, n)
  }
  diag <- .diag_add(
    diag, live & control_char_stripped, "control-char-stripped"
  )

  # --- host-charset shim diagnostic (RURL-dxwxeamq, ADR 0009) ----------------
  # `host-charset-shimmed` fires exactly where Phase 1's shim
  # (.shim_whatwg_host_charset_vec) accepted a host code point libcurl rejects
  # but WHATWG keeps (! " $ & ' ( ) * + , ; = ` { }) -- surfacing the accepted
  # WHATWG boundary as a FACT (ADR 0006). Always FALSE under rfc3986 / no
  # selector; RFC 3986 sub-delim recovery is standards conformance, not this
  # WHATWG diagnostic.
  host_charset_shimmed <- a$host_charset_shimmed
  if (is.null(host_charset_shimmed)) {
    host_charset_shimmed <- rep(FALSE, n)
  }
  diag <- .diag_add(
    diag, live & host_charset_shimmed, "host-charset-shimmed"
  )

  # --- DNS-length / UTS-46 diagnostics (RURL-vowqpmdg, T5 design lock) -------
  # Delegates to punycoder::host_normalize() via the .punycoder_host_probe()
  # seam (domain.R) for 3 of the 5 facts, plus 2 rurl-owned structural
  # detectors (empty-label, length subtyping); see that seam's header comment
  # for the full design. Facts, not policy (same pattern as every other block
  # above): fired identically in BOTH standard modes, keyed to host SHAPE
  # alone. Probed against `final_host` (the resolved, pre-Stage-B host
  # spelling -- host_normalize() accepts either Unicode or ASCII/punycode
  # input and reads the same case-preserving spelling verbatim; confirmed
  # empirically for an IDN host at this point in the pipeline). IP literals
  # are excluded from the probe: `use_std3` flags a bracketed IPv6 literal's
  # "[", "]", ":" as violations (verified empirically), which would
  # misclassify every IPv6 host as a STD3 violation, and DNS-length/UTS-46
  # rules are meaningless for an IP literal in the first place.
  probe_host <- final_host
  probe_host[!(live & !host_missing & !is_ip)] <- NA_character_
  host_probe <- .punycoder_host_probe(probe_host)
  diag <- .diag_add(diag, host_probe$label_too_long, "domain-label-too-long")
  diag <- .diag_add(diag, host_probe$name_too_long, "domain-name-too-long")
  diag <- .diag_add(diag, host_probe$empty_label, "domain-empty-label")
  diag <- .diag_add(
    diag, host_probe$hyphen_violation, "domain-hyphen-violation"
  )
  diag <- .diag_add(diag, host_probe$std3_violation, "domain-std3-violation")

  # --- Layer 5 SELECTED diagnostics (ADR 0012 D5 + Layer 5, RURL-izsouyxs) ---
  # SELECTED facts, never a conformance oracle: absence of any token below does
  # NOT imply the URL conforms to its scheme's RFC or to WHATWG (D5). Reached
  # only through PURE RE-CALLS (.general_parse_vec / .rfc3986_generic_uri_ok /
  # .stage_b_eligibility), the codebase's established Stage-B pattern; Stage A
  # stays cacheable.
  general <- identical(opts$scheme_acceptance, "general")
  is_whatwg <- .is_whatwg(opts$url_standard)
  is_rfc <- identical(opts$url_standard, "rfc3986")

  # WHATWG-GENERIC validation-error facts (invalid-credentials / invalid-URL-
  # unit) gate on the INTERPRETING standard, not the acceptance axis (ADR 0012
  # D5, RURL-sgjzbqzk). They are route-independent, string-level facts -- any
  # credentials present; a malformed `%`-escape or a non-URL code point -- true
  # of the input regardless of which parser ran, so they fire whenever
  # url_standard is WHATWG, INCLUDING the default `web` acceptance path. The
  # DEFAULT combo (web + url_standard = NULL) is untouched because is_whatwg is
  # FALSE there, so the D4 byte-identity / CRAN contract holds. userinfo is
  # carried by libcurl on the special-scheme route (http/https/ftp/ws/wss); the
  # general parser sets user/password NA for opaque/RFC rows, so this bounded
  # detection covers the WHATWG special-scheme routes. All OTHER Layer 5 facts
  # stay general-gated below: they are parse-structural or ride the general
  # parser (ws/wss are only admissible under general acceptance at all).
  has_userinfo <- !is.na(a$raw_user) | !is.na(a$raw_password)
  if (is_whatwg && !is.null(url)) {
    # `invalid-credentials` (WHATWG, verbatim): raised for ANY credentials in a
    # URL -- not specifically the multiple-`@` case (ADR 0012 D5 / A.1).
    diag <- .diag_add(diag, live & has_userinfo, "invalid-credentials")

    # `invalid-URL-unit` (WHATWG, verbatim): the SINGLE error name covering BOTH
    # a malformed `%`-escape AND a non-URL code point. Deliberately BOUNDED (a
    # selected fact, not a full WHATWG validation engine): a `%` not followed by
    # two hex digits, or a clearly non-URL ASCII code point (space " < > ` { }
    # | ^). Backslash and C0 controls are excluded -- they are surfaced by
    # `invalid-reverse-solidus` / `control-char-stripped`.
    malformed_pct <- stringi::stri_detect_regex(url, "%(?![0-9A-Fa-f]{2})")
    non_url_cp <- stringi::stri_detect_regex(url, "[ \"<>\\u0060{}|^]")
    bad_unit <- malformed_pct | non_url_cp
    bad_unit[is.na(bad_unit)] <- FALSE
    diag <- .diag_add(diag, live & bad_unit, "invalid-URL-unit")
  }

  if (general && !is.null(url)) {
    scheme_lc <- .ascii_tolower(a$final_scheme)

    # `transform-skipped-ineligible-scheme`: mirror ._parse_stage_b_vec's
    # eligibility computation exactly -- re-run the pure general parser to
    # recover the state kinds, build the path/host-kind proxies the classifier
    # needs, then read `!semantic_transform_eligible` (the non-HTTP(S) rows a
    # global SEO transform would skip under general acceptance, ADR 0012 D2).
    gen_b <- .general_parse_vec(url, opts$url_standard, opts$scheme_acceptance)
    gp <- gen_b$general_parsed & live
    is_special_row <- !is.na(scheme_lc) & scheme_lc %in% .WHATWG_SPECIAL_SCHEMES
    pk <- .whatwg_path_kind(is_special_row, a$raw_path)
    hk <- .host_kind(a$final_host)
    if (any(gp)) {
      pk[gp] <- gen_b$path_kind[gp]
      pk[gp & is.na(pk)] <- "list"
      hk[gp] <- gen_b$host_kind[gp]
    }
    elig <- .stage_b_eligibility(
      "general", a$final_scheme, opts$url_standard,
      path_kind = pk, host_kind = hk, is_ip_host = is_ip
    )
    diag <- .diag_add(
      diag, live & !elig$semantic_transform_eligible,
      "transform-skipped-ineligible-scheme"
    )

    # `unicode-outside-rfc3986-uri`: the sole D1 Unicode tolerance (rfc3986
    # posture, a directly-written non-ASCII scalar value accepted by the generic
    # grammar gate). The gate already returns the literal token in `$diagnostic`
    # for exactly those rows; thread it out.
    if (is_rfc) {
      g <- .rfc3986_generic_uri_ok(url)
      diag <- .diag_add(
        diag, live & !is.na(g$diagnostic), "unicode-outside-rfc3986-uri"
      )
    }

    if (is_whatwg) {
      # ws/wss (RFC 6455 forbids both a fragment AND userinfo) -- TWO separate
      # facts. ws/wss parse via the libcurl SPECIAL-scheme route under whatwg
      # (they are in .WHATWG_SPECIAL_SCHEMES), NOT via .general_parse_vec, so
      # read fragment/userinfo from the Stage-A columns for that route.
      is_ws <- !is.na(scheme_lc) & scheme_lc %in% c("ws", "wss")
      diag <- .diag_add(
        diag, live & is_ws & !is.na(a$raw_fragment), "ws-fragment-forbidden"
      )
      diag <- .diag_add(
        diag, live & is_ws & has_userinfo, "ws-userinfo-forbidden"
      )
    }

    # Per-scheme facts on the general-routed opaque/RFC rows (`gp`). mailto/tel/
    # data route to the general parser under BOTH postures; `file` routes there
    # only under rfc3986 (whatwg `file` is a special scheme on the libcurl
    # route), so the file facts are rfc-only.
    gs <- .ascii_tolower(gen_b$scheme)
    # `mailto`: fragment present (RFC 6068 section 2 SHOULD NOT).
    diag <- .diag_add(
      diag, gp & gs == "mailto" & !is.na(gen_b$fragment),
      "mailto-fragment-discouraged"
    )
    # `data`: missing the mandatory comma (RFC 2397).
    data_has_comma <- !is.na(gen_b$path) &
      stringi::stri_detect_fixed(gen_b$path, ",")
    diag <- .diag_add(
      diag, gp & gs == "data" & !data_has_comma, "data-missing-comma"
    )
    # `tel`: a LOCAL number (no leading "+") missing the required
    # `;phone-context=` parameter (RFC 3966; parameter name case-insensitive).
    tel_num <- gen_b$path
    tel_global <- !is.na(tel_num) & startsWith(tel_num, "+")
    tel_ctx <- !is.na(tel_num) &
      stringi::stri_detect_regex(tel_num, "(?i);phone-context=")
    diag <- .diag_add(
      diag, gp & gs == "tel" & !tel_global & !tel_ctx,
      "tel-missing-phone-context"
    )
    # `file` under rfc-syntax: non-absolute path; OR userinfo/port/query/
    # fragment present (outside RFC 8089's grammar). userinfo is not surfaced by
    # the RFC file parser, so this bounded fact covers port/query/fragment.
    if (is_rfc) {
      is_file <- gp & gs == "file"
      diag <- .diag_add(
        diag,
        is_file & gen_b$rfc_path_form %in% c("rootless", "empty"),
        "file-non-absolute-path"
      )
      file_forbidden <- !is.na(gen_b$port) | !is.na(gen_b$query) |
        !is.na(gen_b$fragment)
      diag <- .diag_add(
        diag, is_file & file_forbidden, "file-forbidden-component"
      )
    }
  }

  list(host_type = host_type, diagnostics = diag)
}

# --- IPv4 host shape diagnostics ---------------------------------------------

# ipv4-ish host token: dot-separated groups, each all-decimal or 0x-hex. Matches
# the Phase-1 classifier's own `ipv4ish` test, so the diagnostics are keyed to
# exactly the tokens the host model treats as IPv4 attempts. WHATWG treats a
# bare `0x` prefix as zero after stripping the prefix, so the hex digit run may
# be empty.
.RE_IPV4ISH <-
  "^(0[xX][0-9a-fA-F]*|[0-9]+)(\\.(0[xX][0-9a-fA-F]*|[0-9]+))*$"

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
  if (length(vals) == 0L) {
    return(0)
  }
  if (anyNA(vals) || any(vals >= base)) {
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

  is_hex <- grepl("^0[xX][0-9a-fA-F]*$", parts)
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
  meta_uniq <- .derive_url_metadata_vec(a, opts, length(uniq), url = uniq)

  pos <- match(url, uniq)
  list(
    host_type = meta_uniq$host_type[pos],
    diagnostics = meta_uniq$diagnostics[pos]
  )
}
