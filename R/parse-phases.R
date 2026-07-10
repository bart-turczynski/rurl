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

  # inet_aton / WHATWG IPv4 attempts: dotted groups each all-decimal or 0x-hex
  # (12345, 0x7f000001, 0x, 017700000001, 192.168, 1.2.3.4, 256.1.1.1,
  # 1.2.3.4.5), plus any ':' host (IPv6, incl. bracketed). WHATWG's IPv4
  # number parser treats a bare "0x" prefix as zero after stripping the prefix.
  ipv4ish <- stringi::stri_detect_regex(
    host_token, "^(0[xX][0-9a-fA-F]*|[0-9]+)(\\.(0[xX][0-9a-fA-F]*|[0-9]+))*$"
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
    is_canonical_ip = is_canonical_ip,
    # Original (pre-curl) host token and the IPv4-attempt flag, surfaced for the
    # url_standard host model (RURL-luwvkwhd): libcurl coerces numeric IPv4
    # forms itself, so the model reads the original token to keep an RFC 3986
    # reg-name uncoerced and to compute shape-keyed diagnostics.
    host_token = host_token,
    is_ipv4ish = ipv4ish
  )
}

# Phase 1 helper (vector): WHATWG literal backslash-as-slash recognition
# (RURL-ledntyab, PRD v2 D2, §5.2). Under url_standard = "whatwg", for schemes
# in .WHATWG_SPECIAL_SCHEMES (http/https/ftp/file -- NOT ftps, which WHATWG
# does not define as special), a literal backslash is treated identically to a
# forward slash everywhere the WHATWG state machine checks for one: the
# scheme-relative "//" marker, the authority/path boundary, and path-segment
# separators. For rurl's authority-based special schemes (http/https/ftp), the
# same pre-curl step also implements the WHATWG special-authority-slashes state
# for inputs with no slash run at all: `http:example.com` is handed to curl as
# `http://example.com`. This must run BEFORE curl ever sees the URL (libcurl
# never treats "\" as a separator and rejects missing `//` authority forms), so
# it rewrites the raw string here in Phase 1, ahead of every other scheme/host
# regex in this function -- once rewritten, the existing "://"/"//" detection,
# host classification, and curl handoff need no further changes.
#
# The leading run right after "scheme:" is handled as its own case (mirroring
# the WHATWG "special authority slashes"/"special authority ignore slashes"
# states) rather than a blind 1:1 "\" -> "/" substitution: that run -- of ANY
# length and ANY mix of "/" and "\" -- is collapsed to exactly "//" before
# authority parsing continues, so a single "\" (RURL-ledntyab's own acceptance
# case "http:\host\path"), a double "\\" ("http:\\host\path"), and an
# already-canonical "//" all normalize the same way. A run of length 0 (no
# separator at all, e.g. "http:host/path") is authority-introducing for the
# http/https/ftp subset, but not for file (left to the file-state slice). AFTER
# the authority marker, a literal "\" remains a plain 1:1 "\" -> "/" rewrite
# (path-segment separators, and the authority/path boundary when it isn't part
# of the run).
#
# Recognition only, no decoding: `%5C` (a percent-encoded backslash) is inert
# literal text here, never treated as a separator -- it contains no actual
# backslash byte. The query and fragment are never touched: only the span from
# just after "scheme:" up to the first literal "?"/"#" is eligible, so a
# backslash inside `?q=a\b#frag\c` stays untouched even on a rewritten row.
#
# Returns `url` (rewritten for eligible rows, unchanged otherwise) and
# `backslash_rewritten`, a logical mask marking rows where a literal "\" byte
# actually participated in a rewrite (in the leading run or the remainder) --
# used to emit the `invalid-reverse-solidus` diagnostic. A leading run that was
# ALREADY all forward slashes (a plain "http://", or the spec-accurate
# multi-slash collapse of e.g. "http:///host") changes no backslash, so it does
# NOT set this flag even though the string may still be rewritten.
.rewrite_whatwg_backslashes_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(url = url, backslash_rewritten = rep(FALSE, n))
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }

  scheme_match <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):"
  )
  scheme_lower <- stringi::stri_trans_tolower(scheme_match[, 2L])
  eligible <- !is.na(scheme_lower) & scheme_lower %in% .WHATWG_SPECIAL_SCHEMES
  if (!any(eligible)) {
    return(no_op)
  }

  colon_len <- stringi::stri_length(scheme_match[, 1L])
  rest <- stringi::stri_sub(url, colon_len + 1L)

  # The query/fragment boundary is the first literal '?' or '#' -- an
  # unencoded delimiter byte, so it is safe to locate before any rewriting.
  qf_start <- stringi::stri_locate_first_regex(rest, "[?#]")[, 1L]
  before <- ifelse(
    is.na(qf_start), rest, stringi::stri_sub(rest, 1L, qf_start - 1L)
  )
  after <- ifelse(is.na(qf_start), "", stringi::stri_sub(rest, qf_start))

  # Leading run of one or more '/'/'\' right after "scheme:" -- the
  # authority-introducing marker, of any length/composition.
  run <- stringi::stri_match_first_regex(before, "^[/\\\\]+")[, 1L]
  has_run <- !is.na(run)

  run_len <- ifelse(has_run, stringi::stri_length(run), 0L)
  remainder <- ifelse(has_run, stringi::stri_sub(before, run_len + 1L), before)

  # Beyond the leading run, a literal backslash is a plain separator: rewrite
  # it 1:1 to '/' (path segments, and the authority/path boundary when the
  # authority had no run of its own, e.g. "http://host\path").
  rewritten_remainder <- stringi::stri_replace_all_fixed(remainder, "\\", "/")

  no_run_authority <- eligible & !has_run &
    scheme_lower %in% .SPECIAL_AUTHORITY_SCHEMES &
    !is.na(before) & before != ""

  # No run at all is usually left untouched. For http/https/ftp under WHATWG,
  # synthesize the missing authority marker. Otherwise: collapse an existing
  # run to exactly "//" and splice the (possibly rewritten) remainder back on.
  rewritten_before <- ifelse(
    has_run | no_run_authority, paste0("//", rewritten_remainder), before
  )

  run_had_backslash <- has_run & stringi::stri_detect_fixed(run, "\\")
  run_had_backslash[is.na(run_had_backslash)] <- FALSE
  remainder_had_backslash <- (has_run | no_run_authority) &
    stringi::stri_detect_fixed(remainder, "\\")
  remainder_had_backslash[is.na(remainder_had_backslash)] <- FALSE
  # file:///path carries an empty file authority. Collapsing an all-forward-
  # slash run from "///" to "//" would turn it into file://path, which libcurl
  # treats as a non-local file host and rejects. Keep file rows byte-for-byte
  # when no literal backslash is present; actual backslash repair still uses the
  # shared special-scheme path above.
  file_no_backslash <- scheme_lower == "file" & has_run &
    !run_had_backslash & !remainder_had_backslash
  file_no_backslash[is.na(file_no_backslash)] <- FALSE
  rewritten_before[file_no_backslash] <- before[file_no_backslash]

  changed <- eligible & (run_had_backslash | remainder_had_backslash)
  changed[is.na(changed)] <- FALSE

  url_out <- url
  url_out[eligible] <- paste0(
    scheme_match[eligible, 1L], rewritten_before[eligible], after[eligible]
  )

  list(url = url_out, backslash_rewritten = changed)
}

# WHATWG control-character stripping (RURL-tyetpjym). The WHATWG basic URL
# parser's very first step removes EVERY ASCII tab (U+0009), LF (U+000A), and CR
# (U+000D) from the input, everywhere in the string, before any component is
# parsed. rurl otherwise rejects a control char in the authority (libcurl
# errors) -- correct under RFC 3986, which has no strip step and requires such
# bytes to be percent-encoded -- so this runs ONLY under url_standard ==
# "whatwg" and is a byte-for-byte no-op otherwise. Stripping is not silent: the
# returned `control_char_stripped` mask (rows where a byte was removed) drives
# the `control-char-stripped` diagnostic (ADR 0006 -- surface the mutation as a
# FACT). Only tab/LF/CR are removed; other C0 controls are left for libcurl.
# Resolves the control-char-in-authority family: c0-tab probe, eq-U6 (LF),
# yal-002 (TAB), yal-003 (CR/LF), which WHATWG strips-and-accepts.
.strip_whatwg_control_chars_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(url = url, control_char_stripped = rep(FALSE, n))
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }
  had <- stringi::stri_detect_regex(url, "[\\t\\n\\r]")
  had[is.na(had)] <- FALSE
  if (!any(had)) {
    return(no_op)
  }
  url_out <- url
  url_out[had] <- stringi::stri_replace_all_regex(url[had], "[\\t\\n\\r]", "")
  list(url = url_out, control_char_stripped = had)
}

# WHATWG / UTS-46 alternative full-stop mapping (RURL-odsmwsxu). UTS-46
# domain-to-ASCII -- the "map" step the WHATWG host parser runs for special
# schemes -- maps three alternative full-stop code points to ASCII "." before a
# host is split into labels: U+3002 (ideographic full stop), U+FF0E (fullwidth
# full stop), and U+FF61 (halfwidth ideographic full stop). rurl hands the raw
# string to libcurl, which does NOT apply UTS-46, so a Unicode-dot host like
# "127。0。0。1" reaches curl with its separators intact, is never split into
# numeric labels, and so never coerces to the canonical dotted-quad (an
# SSRF-relevant loopback/metadata obfuscation -- browsers coerce). The upstream
# host-shape gate is no help either: .classify_input_host_vec()'s ipv4-attempt
# test splits on ASCII "." only, so a literal-Unicode-dot host is not even seen
# as an IPv4 attempt. Mapping the three code points to "." here, BEFORE curl,
# lets the existing IPv4 coercion and label handling see "127.0.0.1" (and, for
# names, "例え。jp" -> "例え.jp").
#
# SCOPED TO THE AUTHORITY ONLY. A full-stop variant in the path/query/fragment
# is legitimate content (e.g. a path "/文書。pdf") and MUST NOT be rewritten, so
# only the authority span -- between the "//" that introduces it and the first
# "/", "?" or "#" -- is eligible. The whole authority (userinfo + host + port)
# is mapped rather than the host alone: a variant full stop in userinfo is a
# negligible edge (userinfo is not itself a domain) and not worth splitting the
# authority to exclude. Opaque "scheme:foo" inputs (no "//") carry no authority
# and are left untouched; a backslash run has already been collapsed to "//"
# upstream, so this sees the normalized form. Runs ONLY under url_standard ==
# "whatwg" (RFC 3986 has no UTS-46 mapping -- these bytes stay literal), and is
# a byte-for-byte no-op otherwise. Recognition only: a percent-encoded form
# ("%E3%80%82") is inert literal text, never mapped. No diagnostic is emitted --
# this is ordinary WHATWG host normalization (like host-lowercasing), not a
# lossy repair of malformed input.
.map_whatwg_domain_separators_vec <- function(url, url_standard) {
  no_op <- list(url = url)
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }
  m <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*:)?(//)([^/?#]*)(.*)$"
  )
  authority <- m[, 4L]
  eligible <- !is.na(authority) &
    stringi::stri_detect_regex(authority, "[\\u3002\\uFF0E\\uFF61]")
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }
  scheme <- ifelse(is.na(m[, 2L]), "", m[, 2L])
  mapped_authority <- stringi::stri_replace_all_regex(
    authority, "[\\u3002\\uFF0E\\uFF61]", "."
  )
  url_out <- url
  url_out[eligible] <- paste0(
    scheme[eligible], m[eligible, 3L], mapped_authority[eligible],
    m[eligible, 5L]
  )
  no_op$url <- url_out
  no_op
}

# Authority userinfo repair for selector profiles (RURL-zqhgezuq). libcurl
# rejects authorities with more than one literal "@", while the WHATWG authority
# state uses the LAST "@" as the userinfo/host delimiter and percent-encodes
# earlier "@" bytes into userinfo. RFC 3986 does not admit a raw "@" inside
# userinfo either, but recovering at the last delimiter is the only
# host-preserving parse; selector profiles use it to avoid dropping the host.
# `url_standard = NULL` remains a no-op for backward compatibility.
.encode_excess_authority_at_vec <- function(url, url_standard) {
  no_op <- list(url = url)
  if (is.null(url_standard)) {
    return(no_op)
  }

  m <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*:)(//)([^/?#]*)(.*)$"
  )
  authority <- m[, 4L]
  at_count <- stringi::stri_count_fixed(authority, "@")
  eligible <- !is.na(authority) & at_count > 1L
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }

  repaired <- vapply(authority[eligible], function(a) {
    at_pos <- gregexpr("@", a, fixed = TRUE)[[1L]]
    last <- at_pos[length(at_pos)]
    userinfo <- substr(a, 1L, last - 1L)
    host_part <- substr(a, last, nchar(a))
    paste0(gsub("@", "%40", userinfo, fixed = TRUE), host_part)
  }, character(1), USE.NAMES = FALSE)

  url_out <- url
  url_out[eligible] <- paste0(
    m[eligible, 2L], m[eligible, 3L], repaired, m[eligible, 5L]
  )
  no_op$url <- url_out
  no_op
}

.parse_whatwg_ipv4_number <- function(part) {
  if (grepl("^0[xX]", part)) {
    digits <- sub("^0[xX]", "", part)
    base <- 16
  } else if (grepl("^0[0-9]+$", part)) {
    digits <- substring(part, 2L)
    base <- 8
  } else {
    digits <- part
    base <- 10
  }
  if (!nzchar(digits)) {
    return(0)
  }

  chars <- strsplit(digits, "", fixed = TRUE)[[1L]]
  vals <- match(toupper(chars), c(0:9, "A", "B", "C", "D", "E", "F")) - 1
  if (anyNA(vals) || any(vals >= base)) {
    return(NA_real_)
  }
  Reduce(function(acc, d) acc * base + d, vals, 0)
}

.parse_whatwg_ipv4_host <- function(host) {
  if (is.na(host) || !nzchar(host)) {
    return(NA_character_)
  }

  host <- stringi::stri_replace_first_regex(host, "\\.$", "")
  parts <- strsplit(host, ".", fixed = TRUE)[[1L]]
  if (length(parts) > 4L || any(parts == "")) {
    return(NA_character_)
  }

  numbers <- vapply(
    parts, .parse_whatwg_ipv4_number, numeric(1), USE.NAMES = FALSE
  )
  if (anyNA(numbers)) {
    return(NA_character_)
  }

  k <- length(numbers)
  if (k > 1L && any(numbers[-k] > 255)) {
    return(NA_character_)
  }
  if (numbers[k] > 256^(5L - k) - 1) {
    return(NA_character_)
  }

  ipv4 <- numbers[k]
  if (k > 1L) {
    for (i in seq_len(k - 1L)) {
      ipv4 <- ipv4 + numbers[i] * 256^(4L - i)
    }
  }
  octets <- vapply(3L:0L, function(pow) {
    floor(ipv4 / 256^pow) %% 256
  }, numeric(1), USE.NAMES = FALSE)
  paste(octets, collapse = ".")
}

# WHATWG IPv4 canonicalization before curl. libcurl handles many numeric forms,
# but rejects WPT-valid empty-hex-zero parts such as `0x.0x.0`; canonicalizing
# valid WHATWG IPv4 hosts here lets curl parse the rest of the URL structure.
# Invalid "ends in a number" hosts are left untouched and rejected either by
# curl or by the later WHATWG host model.
.rewrite_whatwg_ipv4_hosts_vec <- function(url, url_standard) {
  no_op <- list(url = url)
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }

  m <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*:)(//)([^/?#]*)(.*)$"
  )
  authority <- m[, 4L]
  eligible <- !is.na(authority)
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }

  after_ui <- stringi::stri_replace_first_regex(authority, "^.*@", "")
  bracketed <- stringi::stri_startswith_fixed(after_ui, "[")
  bracketed[is.na(bracketed)] <- FALSE
  host <- ifelse(
    bracketed,
    stringi::stri_replace_first_regex(after_ui, "^(\\[[^\\]]*\\]).*$", "$1"),
    stringi::stri_replace_first_regex(after_ui, ":[^:]*$", "")
  )

  attempt <- eligible & !bracketed & .host_ends_in_number_vec(host)
  attempt[is.na(attempt)] <- FALSE
  if (!any(attempt)) {
    return(no_op)
  }

  parsed <- rep(NA_character_, length(url))
  parsed[attempt] <- vapply(
    host[attempt], .parse_whatwg_ipv4_host, character(1), USE.NAMES = FALSE
  )
  rewrite <- attempt & !is.na(parsed)
  if (!any(rewrite)) {
    return(no_op)
  }

  ui_prefix <- stringi::stri_sub(
    authority, 1L, stringi::stri_length(authority) -
      stringi::stri_length(after_ui)
  )
  port_suffix <- stringi::stri_sub(after_ui, stringi::stri_length(host) + 1L)
  new_authority <- paste0(ui_prefix, parsed, port_suffix)

  url_out <- url
  url_out[rewrite] <- paste0(
    m[rewrite, 2L], m[rewrite, 3L], new_authority[rewrite], m[rewrite, 5L]
  )
  no_op$url <- url_out
  no_op
}

# Decode percent-triplets in a WHATWG host just far enough for the host model to
# see the real code points after curl has parsed the URL's structure. A
# malformed or NUL-containing sequence maps to a C0 sentinel so the WHATWG
# forbidden-host gate rejects the row; no successful output depends on that
# placeholder.
.whatwg_percent_decode_host <- function(host) {
  if (is.na(host) || !nzchar(host) ||
      !grepl("%[0-9A-Fa-f]{2}", host, perl = TRUE)) {
    return(host)
  }
  tryCatch(utils::URLdecode(host), error = function(e) "\u0001")
}

# Host shim (RURL-dxwxeamq, ADR 0009; extended by RURL-rgjpcbuk and
# RURL-dnddogce). libcurl's host handling is too eager for selector mode in two
# ways:
#
# * WHATWG accepts 15 literal ASCII code points that libcurl rejects in a host
#   ("Bad hostname"). The shim replaces those bytes 1:1 with filler so curl can
#   parse structure, then restores the true host before the host model runs.
#   RFC 3986 uses the same seam for the 11 literal reg-name sub-delims it
#   permits in section 3.2.2.
# * libcurl percent-decodes host triplets before validation. That rejects the
#   encoded spelling of the same selector-valid gap bytes, such as `%60`. The
#   shim masks those host percent-triplets as three filler letters (`aaa`), then
#   restores the profile-correct host spelling: WHATWG percent-decoded, RFC
#   unreserved-decoded while preserving the encoded gap triplet.
#
# The replacement is length- and delimiter-preserving, so curl returns
# byte-identical scheme/userinfo/port/path/query/fragment; only `$host` is a
# placeholder. The caller restores `shimmed_true_host` before IP detection and
# forbidden-code-point checks, so the shim widens no validation gate.
#
# Literal host-character widening is selector-scoped: WHATWG gets its full
# ada-confirmed gap set for special schemes; RFC 3986 gets only its reg-name
# sub-delims for authority-based supported schemes. NULL keeps historical curl
# behavior.
.shim_whatwg_host_charset_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(
    url = url,
    restore_host_shimmed = rep(FALSE, n),
    host_charset_shimmed = rep(FALSE, n),
    shimmed_true_host = rep(NA_character_, n)
  )
  if (!.is_whatwg(url_standard) &&
      !identical(url_standard, "rfc3986")) {
    return(no_op)
  }

  # Split "scheme://" authority "rest". Literal gap chars and percent-triplets
  # are non-structural, so the authority boundary (first /?#) is safe to locate
  # before any substitution.
  m <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):(//)([^/?#]*)(.*)$"
  )
  scheme_lower <- stringi::stri_trans_tolower(m[, 2L])
  authority <- m[, 4L]
  authority_schemes <- if (.is_whatwg(url_standard)) {
    .WHATWG_SPECIAL_SCHEMES
  } else {
    setdiff(.SUPPORTED_SCHEMES, "file")
  }
  eligible <- !is.na(authority) & scheme_lower %in% authority_schemes
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }

  # Host = authority after any userinfo (up to the last "@"), minus a trailing
  # ":port". Bracketed IPv6 keeps its "[...]" (never carries a gap byte anyway).
  after_ui <- stringi::stri_replace_first_regex(authority, "^.*@", "")
  bracketed <- stringi::stri_startswith_fixed(after_ui, "[")
  bracketed[is.na(bracketed)] <- FALSE
  host <- ifelse(
    bracketed,
    stringi::stri_replace_first_regex(after_ui, "^(\\[[^\\]]*\\]).*$", "$1"),
    stringi::stri_replace_first_regex(after_ui, ":[^:]*$", "")
  )

  has_pct <- grepl("%[0-9A-Fa-f]{2}", host, perl = TRUE)
  has_pct[is.na(has_pct)] <- FALSE
  invalid_pct <- grepl("%(?![0-9A-Fa-f]{2})", host, perl = TRUE)
  invalid_pct[is.na(invalid_pct)] <- FALSE

  model_host <- host
  pct_ok <- eligible & has_pct & !invalid_pct
  pct_decoded_host <- host
  if (any(pct_ok)) {
    pct_decoded_host[pct_ok] <- vapply(
      host[pct_ok], .whatwg_percent_decode_host, character(1),
      USE.NAMES = FALSE
    )
    if (.is_whatwg(url_standard)) {
      model_host[pct_ok] <- pct_decoded_host[pct_ok]
    } else {
      model_host[pct_ok] <- vapply(
        host[pct_ok], .rfc_unreserved_normalize, character(1),
        USE.NAMES = FALSE
      )
    }
  }

  literal_gap_whatwg <- eligible &
    stringi::stri_detect_regex(host, .WHATWG_HOST_CHARSET_SHIM_CP)
  literal_gap_whatwg[is.na(literal_gap_whatwg)] <- FALSE
  literal_gap_rfc3986 <- eligible &
    stringi::stri_detect_regex(host, .RFC3986_REG_NAME_SUB_DELIM_CP)
  literal_gap_rfc3986[is.na(literal_gap_rfc3986)] <- FALSE
  decoded_gap <- eligible &
    stringi::stri_detect_regex(model_host, .WHATWG_HOST_CHARSET_SHIM_CP)
  decoded_gap[is.na(decoded_gap)] <- FALSE
  pct_gap <- eligible & pct_ok &
    stringi::stri_detect_regex(pct_decoded_host, .WHATWG_HOST_CHARSET_SHIM_CP)
  pct_gap[is.na(pct_gap)] <- FALSE

  pct_mask <- if (.is_whatwg(url_standard)) {
    pct_ok & decoded_gap
  } else {
    pct_gap
  }
  literal_mask <- if (.is_whatwg(url_standard)) {
    literal_gap_whatwg
  } else {
    literal_gap_rfc3986
  }
  restore <- eligible & (pct_mask | literal_mask)
  restore[is.na(restore)] <- FALSE
  if (!any(restore)) {
    return(no_op)
  }

  # Filler swaps preserve length: `%HH` -> `aaa`, literal gaps -> `a`.
  filled_host <- host
  filled_host[pct_mask] <- gsub(
    "%[0-9A-Fa-f]{2}", "aaa", filled_host[pct_mask], perl = TRUE
  )
  literal_fill_cp <- if (.is_whatwg(url_standard)) {
    .WHATWG_HOST_CHARSET_SHIM_CP
  } else {
    .RFC3986_REG_NAME_SUB_DELIM_CP
  }
  filled_host[literal_mask] <- stringi::stri_replace_all_regex(
    filled_host[literal_mask], literal_fill_cp, "a"
  )
  ui_prefix <- stringi::stri_sub(
    authority, 1L, stringi::stri_length(authority) -
      stringi::stri_length(after_ui)
  )
  port_suffix <- stringi::stri_sub(after_ui, stringi::stri_length(host) + 1L)
  new_authority <- paste0(ui_prefix, filled_host, port_suffix)

  url_out <- url
  url_out[restore] <- paste0(
    m[restore, 2L], ":", m[restore, 3L], new_authority[restore],
    m[restore, 5L]
  )
  true_host <- rep(NA_character_, n)
  true_host[restore] <- model_host[restore]

  list(
    url = url_out,
    restore_host_shimmed = restore,
    host_charset_shimmed = .is_whatwg(url_standard) & decoded_gap,
    shimmed_true_host = true_host
  )
}

# WHATWG path/query/fragment curl fallback. libcurl rejects some WPT-valid
# bytes outside the authority (for example raw DEL, non-ASCII fragment bytes,
# and C0 controls other than tab/LF/CR), while WHATWG serializes those
# component bytes with the path/query/fragment percent-encode sets. This helper
# prepares a curl-safe spelling of ONLY the post-authority components; Stage A
# uses it as a fallback after the original curl parse fails, so already-accepted
# readable paths keep their historical raw spelling under path_encoding="keep".
.sanitize_whatwg_pqf_for_curl_vec <- function(url, url_standard) {
  if (!.is_whatwg(url_standard)) {
    return(url)
  }
  vapply(url, .sanitize_whatwg_pqf_for_curl_one, character(1),
    USE.NAMES = FALSE
  )
}

.sanitize_whatwg_pqf_for_curl_one <- function(url) {
  if (is.na(url) || !nzchar(url)) {
    return(url)
  }
  m <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]*)(.*)$"
  )
  if (is.na(m[1L, 1L])) {
    return(url)
  }

  prefix <- m[1L, 2L]
  rest <- m[1L, 3L]
  scheme <- sub(":.*$", "", prefix)

  hash <- regexpr("#", rest, fixed = TRUE)[1L]
  qmark <- regexpr("?", rest, fixed = TRUE)[1L]
  has_query <- qmark > 0L && (hash < 0L || qmark < hash)
  has_fragment <- hash > 0L

  path_end <- nchar(rest)
  if (has_query) {
    path_end <- min(path_end, qmark - 1L)
  }
  if (has_fragment) {
    path_end <- min(path_end, hash - 1L)
  }
  path <- if (path_end > 0L) substr(rest, 1L, path_end) else ""

  query <- NA_character_
  if (has_query) {
    query_end <- if (has_fragment) hash - 1L else nchar(rest)
    query <- substr(rest, qmark + 1L, query_end)
  }

  fragment <- NA_character_
  if (has_fragment) {
    fragment <- substr(rest, hash + 1L, nchar(rest))
  }

  out <- paste0(prefix, .whatwg_path_percent_encode(path))
  if (has_query) {
    out <- paste0(out, "?", .whatwg_query_percent_encode(query, scheme))
  }
  if (has_fragment) {
    out <- paste0(out, "#", .whatwg_fragment_percent_encode(fragment))
  }
  out
}

# RFC 3986 scheme + path-rootless support for special schemes without `//`
# (RURL-pwsacxvo). In RFC 3986 section 3, an authority is present only when the
# scheme-specific part starts with a literal `//`; otherwise `http:example.com`
# is `scheme = "http"`, no authority, `path = "example.com"`. libcurl rejects
# these as malformed HTTP URLs, so selector mode records the components here and
# Stage A installs them directly instead of going through curl.
#
# This slice is intentionally limited to host-shaped path-rootless
# (`scheme:example.com[/...]`) for the http/https/ftp family that WHATWG treats
# as recoverable authority URLs. Other RFC-valid path-rootless strings (for
# example ones starting with `@` or `:`), single-slash (`scheme:/path`) forms,
# and file-state variants are left outside this change. Literal backslash
# remains inert/rejected under RFC 3986.
.rfc3986_path_rootless_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(
    is_path_rootless = rep(FALSE, n),
    scheme = rep(NA_character_, n),
    path = rep(NA_character_, n),
    query = rep(NA_character_, n),
    fragment = rep(NA_character_, n)
  )
  if (!identical(url_standard, "rfc3986")) {
    return(no_op)
  }

  m <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):(.*)$"
  )
  scheme <- m[, 2L]
  rest <- m[, 3L]
  scheme_lower <- stringi::stri_trans_tolower(scheme)
  first_segment <- stringi::stri_replace_first_regex(rest, "[/?#].*$", "")
  host_shaped_first_segment <- stringi::stri_detect_regex(
    first_segment, "^[A-Za-z0-9._~-]+(\\.[A-Za-z0-9._~-]+)+$"
  )
  host_shaped_first_segment[is.na(host_shaped_first_segment)] <- FALSE
  eligible <- !is.na(rest) &
    scheme_lower %in% .SPECIAL_AUTHORITY_SCHEMES &
    host_shaped_first_segment &
    rest != "" &
    !stringi::stri_startswith_fixed(rest, "/") &
    !stringi::stri_startswith_fixed(rest, "\\") &
    !stringi::stri_detect_fixed(rest, "\\")
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }

  split <- lapply(rest[eligible], function(x) {
    hash <- regexpr("#", x, fixed = TRUE)[1L]
    qmark <- regexpr("?", x, fixed = TRUE)[1L]

    end_path <- nchar(x)
    if (qmark > 0L) {
      end_path <- min(end_path, qmark - 1L)
    }
    if (hash > 0L) {
      end_path <- min(end_path, hash - 1L)
    }
    path <- if (end_path > 0L) substr(x, 1L, end_path) else ""

    query <- NA_character_
    if (qmark > 0L && (hash < 0L || qmark < hash)) {
      query_end <- if (hash > 0L) hash - 1L else nchar(x)
      query <- substr(x, qmark + 1L, query_end)
    }

    fragment <- NA_character_
    if (hash > 0L) {
      fragment <- substr(x, hash + 1L, nchar(x))
    }

    c(path = path, query = query, fragment = fragment)
  })
  parts <- do.call(rbind, split)

  no_op$is_path_rootless[eligible] <- TRUE
  no_op$scheme[eligible] <- scheme[eligible]
  no_op$path[eligible] <- parts[, "path"]
  no_op$query[eligible] <- .blank_to_na(parts[, "query"])
  no_op$fragment[eligible] <- .blank_to_na(parts[, "fragment"])
  no_op
}

.whatwg_file_split_rest <- function(rest) {
  hash <- regexpr("#", rest, fixed = TRUE)[1L]
  qmark <- regexpr("?", rest, fixed = TRUE)[1L]

  end_path <- nchar(rest)
  if (qmark > 0L) {
    end_path <- min(end_path, qmark - 1L)
  }
  if (hash > 0L) {
    end_path <- min(end_path, hash - 1L)
  }
  path <- if (end_path > 0L) substr(rest, 1L, end_path) else ""

  query <- NA_character_
  if (qmark > 0L && (hash < 0L || qmark < hash)) {
    query_end <- if (hash > 0L) hash - 1L else nchar(rest)
    query <- substr(rest, qmark + 1L, query_end)
  }

  fragment <- NA_character_
  if (hash > 0L) {
    fragment <- substr(rest, hash + 1L, nchar(rest))
  }

  c(path = path, query = query, fragment = fragment)
}

.whatwg_file_normalize_host <- function(host) {
  if (is.na(host) || host == "") {
    return(NA_character_)
  }
  if (stringi::stri_startswith_fixed(host, "[")) {
    return(host)
  }

  decoded <- if (grepl("%[0-9A-Fa-f]{2}", host, perl = TRUE)) {
    tryCatch(utils::URLdecode(host), error = function(e) host)
  } else {
    host
  }
  if (stringi::stri_detect_regex(decoded, .WHATWG_FORBIDDEN_HOST_CP)) {
    return("\u0001")
  }
  normalized <- punycoder::host_normalize(
    decoded, check_hyphens = FALSE, use_std3 = FALSE,
    verify_dns_length = FALSE
  )
  if (!is.na(normalized)) {
    decoded <- normalized
  }
  if (identical(stringi::stri_trans_tolower(decoded), "localhost")) {
    return(NA_character_)
  }
  decoded
}

.whatwg_file_drive_path <- function(path) {
  stringi::stri_replace_first_regex(path, "^/([A-Za-z])\\|(?=/|$)", "/$1:")
}

.parse_whatwg_file_urls_vec <- function(url, backslash_rewritten) {
  n <- length(url)
  out <- list(
    ok = rep(FALSE, n),
    host = rep(NA_character_, n),
    path = rep(NA_character_, n),
    query = rep(NA_character_, n),
    fragment = rep(NA_character_, n)
  )
  if (n == 0L) {
    return(out)
  }

  m <- stringi::stri_match_first_regex(
    url, "^[Ff][Ii][Ll][Ee]:(.*)$"
  )
  rest <- m[, 2L]
  ok <- !is.na(rest)
  if (!any(ok)) {
    return(out)
  }

  split <- lapply(rest[ok], .whatwg_file_split_rest)
  parts <- do.call(rbind, split)
  file_path_raw <- parts[, "path"]
  file_path <- stringi::stri_replace_all_fixed(file_path_raw, "\\", "/")

  host <- rep(NA_character_, length(file_path))
  path <- file_path
  has_authority <- stringi::stri_detect_regex(file_path_raw, "^[/\\\\]{2}")
  has_authority[is.na(has_authority)] <- FALSE
  if (any(has_authority)) {
    after_marker <- stringi::stri_sub(file_path_raw[has_authority], 3L)
    slash <- stringi::stri_locate_first_regex(after_marker, "[/\\\\]")[, 1L]
    authority <- ifelse(
      is.na(slash),
      after_marker,
      stringi::stri_sub(after_marker, 1L, slash - 1L)
    )
    auth_path <- ifelse(
      is.na(slash), "/", stringi::stri_sub(after_marker, slash)
    )
    authority <- stringi::stri_replace_all_fixed(authority, "\\", "/")
    auth_path <- stringi::stri_replace_all_fixed(auth_path, "\\", "/")

    drive_authority <- stringi::stri_detect_regex(authority, "^[A-Za-z]\\|$")
    drive_authority[is.na(drive_authority)] <- FALSE
    if (any(drive_authority)) {
      auth_path[drive_authority] <- paste0(
        "/",
        stringi::stri_sub(authority[drive_authority], 1L, 1L),
        ":",
        auth_path[drive_authority]
      )
      authority[drive_authority] <- ""
    }

    host[has_authority] <- vapply(
      authority, .whatwg_file_normalize_host, character(1), USE.NAMES = FALSE
    )
    path[has_authority] <- auth_path
  }

  no_authority <- !has_authority
  if (any(no_authority)) {
    no_auth_idx <- which(no_authority)
    path[no_auth_idx] <- ifelse(path[no_auth_idx] == "", "/", path[no_auth_idx])
    needs_leading <- !stringi::stri_startswith_fixed(path[no_auth_idx], "/")
    needs_leading[is.na(needs_leading)] <- FALSE
    lead_idx <- no_auth_idx[needs_leading]
    path[lead_idx] <- paste0("/", path[lead_idx])
  }

  path <- .whatwg_file_drive_path(path)
  # A backslash-introduced empty file authority serializes with a double-slash
  # path. `file:` and `file://` proper keep their ordinary single slash.
  empty_backslash_authority <- has_authority & is.na(host) &
    backslash_rewritten[ok] & path == "/"
  empty_backslash_authority[is.na(empty_backslash_authority)] <- FALSE
  path[empty_backslash_authority] <- "//"

  out$ok[ok] <- TRUE
  out$host[ok] <- host
  out$path[ok] <- path
  out$query[ok] <- .blank_to_na(parts[, "query"])
  out$fragment[ok] <- .blank_to_na(parts[, "fragment"])
  out
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
                                       scheme_relative_handling,
                                       url_standard = NULL,
                                       scheme_policy = "infer",
                                       scheme_acceptance = "web") {
  n <- length(url)

  # WHATWG control-character stripping (RURL-tyetpjym) runs FIRST -- it is the
  # WHATWG parser's step 1 (remove all ASCII tab/LF/CR), so every scheme/host
  # regex and the backslash recognizer below see the already-stripped string.
  # A no-op (byte-for-byte `url` unchanged) unless url_standard == "whatwg".
  cc <- .strip_whatwg_control_chars_vec(url, url_standard)
  url <- cc$url
  whatwg_file_input <- url

  # WHATWG literal backslash recognition (RURL-ledntyab) runs next: for
  # eligible rows it rewrites "\" to "/" ahead of every scheme/host regex
  # below, so the rest of this function sees an already-normalized string.
  # A no-op (byte-for-byte `url` unchanged) unless url_standard == "whatwg".
  bs <- .rewrite_whatwg_backslashes_vec(url, url_standard)
  url <- bs$url

  # WHATWG UTS-46 alternative full-stop mapping (RURL-odsmwsxu) runs next: for
  # eligible rows it maps U+3002/U+FF0E/U+FF61 to ASCII "." in the AUTHORITY
  # only, so a Unicode-dot host coerces through the existing IPv4/label handling
  # (and IDN names normalize their separators) instead of reaching curl as an
  # un-splittable literal. A no-op unless url_standard == "whatwg".
  sep <- .map_whatwg_domain_separators_vec(url, url_standard)
  url <- sep$url

  url_lower <- stringi::stri_trans_tolower(url)
  is_whatwg_file <- .is_whatwg(url_standard) &
    stringi::stri_detect_regex(whatwg_file_input, "^[Ff][Ii][Ll][Ee]:")
  is_whatwg_file[is.na(is_whatwg_file)] <- FALSE

  # A scheme-bearing input is "allowed" only if its scheme is one rurl supports
  # (.SUPPORTED_SCHEMES). any(startsWith(., "<scheme>://")) per row, no loop.
  allowed_prefixes <- paste0(.SUPPORTED_SCHEMES, "://")
  original_has_allowed_scheme <- Reduce(
    `|`, lapply(allowed_prefixes, function(p) startsWith(url_lower, p))
  )
  original_has_allowed_scheme[is.na(original_has_allowed_scheme)] <- FALSE
  original_has_allowed_scheme <- original_has_allowed_scheme | is_whatwg_file

  scheme_match <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):"
  )
  looks_like_protocol <- !is.na(scheme_match[, 2L])
  rfc_rootless <- .rfc3986_path_rootless_vec(url, url_standard)
  original_has_allowed_scheme <-
    original_has_allowed_scheme | rfc_rootless$is_path_rootless

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

  # ADR 0012 D3 (Option B): the unsupported-scheme reject is armed on the
  # scheme-ACCEPTANCE axis, NOT on protocol_handling. Under "web" a
  # scheme-bearing token outside the curated allowlist is an error regardless
  # of how the scheme would be presented (keep/none/strip/http/https). Under
  # "general" this reject is suppressed -- general admits any valid scheme token
  # and defers shaping to L3/L4 (unimplemented here). The
  # `!looks_like_host_port` term stays so host:port inputs (`example.com:8080`,
  # which match the scheme regex) are never demoted -- Phase 1 recognizes and
  # parses them as host:port.
  if (scheme_acceptance == "web") {
    unsupported <- looks_like_protocol &
      !original_has_allowed_scheme &
      !looks_like_host_port
    rejected <- rejected | unsupported
  }

  # Host-shape classification (D1/D2/D5).
  cls <- .classify_input_host_vec(url)

  # D2/D3: an IP attempt that is not a canonical literal is a coerced/malformed
  # IP. Applies to every row (scheme-bearing too), so http://12345 and
  # http://192.168.010.1 are rejected as well as their scheme-less forms.
  #
  # url_standard host model (RURL-luwvkwhd): under a selector, a numeric IPv4
  # attempt is parsed faithfully instead of rejected -- RFC 3986 keeps it as a
  # reg-name, WHATWG coerces it (and only the post-curl model, not this gate,
  # decides WHATWG-fatal cases like 256.1.1.1 / 1.2.3.4.5). So the reject is
  # suppressed for the ipv4-ish attempts; non-canonical IPv6 (ipv6ish) stays
  # rejected in both modes. NULL selector keeps the historical hard reject.
  bad_ip <- cls$is_ipish & !cls$is_canonical_ip
  if (!is.null(url_standard)) {
    bad_ip <- bad_ip & !cls$is_ipv4ish
  }
  rejected <- rejected | bad_ip

  # Rows that get an inferred http:// (scheme-less, non-scheme-relative).
  add_http <- !is_scheme_relative &
    (!looks_like_protocol | looks_like_host_port)

  # D1: only fabricate a scheme when the token is host-shaped -- a canonical IP,
  # localhost, a dotted name, or an explicit host:port. Otherwise the input is
  # not a URL (asdfghjkl, "hello world", /relative/path, bare 12345). Under a
  # selector, a numeric IPv4 attempt (bare 2130706433, 0x7f000001) is host-like
  # too, so scheme-less numeric hosts reach the host model rather than D1.
  host_like <- cls$is_canonical_ip |
    cls$is_localhost |
    cls$is_dotted_name |
    looks_like_host_port
  if (!is.null(url_standard)) {
    host_like <- host_like | cls$is_ipv4ish
  }
  rejected <- rejected | (add_http & !host_like)

  # scheme_policy == "require" (RURL-vzgeurae): strict acceptance. Scheme
  # inference (the `add_http` path -- fabricating "http://" for host-shaped
  # scheme-less input) is rurl's browser-omnibox-style affordance, NOT a
  # WHATWG/RFC parser behavior. Under "require" the user opts out of it: any row
  # that WOULD receive a fabricated scheme is folded into the reject set instead
  # (a parse_status = "error"), so rurl behaves like a pure parser on this axis.
  # This is orthogonal to protocol_handling (which governs scheme
  # *presentation*, not *acceptance*) and url_standard (which governs
  # *interpretation*).
  # Scheme-relative //host input is deliberately NOT governed here -- it has its
  # own dedicated axis (scheme_relative_handling, incl. an "error" mode); this
  # covers only the add_http inference path. Default "infer" leaves `rejected`
  # untouched, so the historical behavior is byte-for-byte unchanged.
  if (scheme_policy == "require") {
    rejected <- rejected | add_http
  }

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

  # Authority userinfo repair (RURL-zqhgezuq): selector profiles recover the
  # host at the last "@" and encode earlier "@" bytes in userinfo before curl.
  at <- .encode_excess_authority_at_vec(url_to_parse, url_standard)
  url_to_parse <- at$url

  ipv4 <- .rewrite_whatwg_ipv4_hosts_vec(url_to_parse, url_standard)
  url_to_parse <- ipv4$url

  # Host shim (RURL-dxwxeamq / RURL-rgjpcbuk) runs LAST -- after scheme
  # fabrication, authority userinfo repair, and WHATWG IPv4 canonicalization,
  # so scheme-less host-like inputs (now "http://..") are in scope and repeated
  # "@" no longer blocks curl from parsing an otherwise valid host.
  shim <- .shim_whatwg_host_charset_vec(url_to_parse, url_standard)
  url_to_parse <- shim$url
  whatwg_pqf_url <- .sanitize_whatwg_pqf_for_curl_vec(
    url_to_parse, url_standard
  )

  list(
    url_to_parse = url_to_parse,
    whatwg_pqf_url = whatwg_pqf_url,
    looks_like_protocol = looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    is_scheme_relative = is_scheme_relative,
    looks_like_host_port = looks_like_host_port,
    scheme_less_userinfo = scheme_less_userinfo,
    rejected = rejected,
    # Original host token + IPv4-attempt flag for the url_standard host model
    # (RURL-luwvkwhd), consumed by ._parse_stage_a_vec()'s model phase.
    input_host = cls$host_token,
    is_ipv4_attempt = cls$is_ipv4ish,
    # WHATWG backslash recognition (RURL-ledntyab): TRUE where a literal "\"
    # was actually reinterpreted as "/", consumed by the url_standard
    # diagnostics seam to emit `invalid-reverse-solidus`.
    backslash_rewritten = bs$backslash_rewritten,
    # WHATWG control-char strip (RURL-tyetpjym): TRUE where a tab/LF/CR was
    # removed, consumed by the diagnostics seam to emit `control-char-stripped`.
    control_char_stripped = cc$control_char_stripped,
    # Host shim: TRUE where curl saw filler bytes in the host and Stage A must
    # restore `shimmed_true_host`.
    restore_host_shimmed = shim$restore_host_shimmed,
    # WHATWG host-charset shim diagnostic: TRUE where a libcurl-rejected but
    # WHATWG-valid host code point was accepted via filler substitution.
    host_charset_shimmed = shim$host_charset_shimmed,
    shimmed_true_host = shim$shimmed_true_host,
    rfc3986_path_rootless = rfc_rootless$is_path_rootless,
    rfc3986_path_rootless_scheme = rfc_rootless$scheme,
    rfc3986_path_rootless_path = rfc_rootless$path,
    rfc3986_path_rootless_query = rfc_rootless$query,
    rfc3986_path_rootless_fragment = rfc_rootless$fragment,
    whatwg_file = is_whatwg_file,
    whatwg_file_input = whatwg_file_input
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
    scheme_less_userinfo = cols$scheme_less_userinfo[1L],
    rfc3986_path_rootless = cols$rfc3986_path_rootless[1L]
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

# Uppercase the two hex digits of every %XX percent-triplet, leaving the rest of
# the string untouched (`%2f` -> `%2F`). This keeps the historical no-selector
# `path_encoding = "keep"` behavior and the RFC 3986 section 6.2.2.1 case
# canonicalization path. Malformed `%` (not followed by two hex digits) is left
# as-is. `\U\1` (perl) uppercases just the captured pair.
.uppercase_percent_hex <- function(x) {
  na <- is.na(x)
  if (all(na)) {
    return(x)
  }
  x[!na] <- gsub("%([0-9a-fA-F]{2})", "%\\U\\1", x[!na], perl = TRUE)
  x
}

# Recover the raw request path from the prepared URL string (the exact bytes
# curl was handed), rather than `parsed_curl$path`. libcurl's `$path` applies
# two normalizations even under `decode = FALSE`: it uppercases percent-hex and
# resolves RFC 3986 dot segments, INCLUDING percent-encoded ones
# (`/a/%2e%2e/b` -> `/b`). Both are profile/presentation decisions, so raw
# extraction stays byte-faithful and later phases apply the selected rules.
# Extracting from the input lets rurl own dot-segment resolution
# (`._remove_dot_segments`, literal `.`/`..` only, per RFC 3986 section 5.2.4 --
# an encoded `%2e` is NOT a dot segment), gated by `path_normalization`.
#
# Extraction (every parseable prepared row carries an explicit `scheme://`
# authority -- opaque/unsupported schemes are rejected upstream): strip the
# scheme, then the path is the run from the first literal `/` (after the
# authority, which cannot contain one) up to the first `?`/`#`. If the body
# starts with `/`, the prepared URL had an empty authority (`scheme:///...`) and
# curl may have promoted the following segment into `$host`; in that shape the
# path from the prepared string is no longer the fetched path, so keep curl's
# coherent `$path` instead of duplicating the promoted host into the path. When
# the authority is followed directly by `?`, `#`, or end-of-string there is no
# path, so fall back to curl's `$path` (the canonical "/" trailing-slash
# expects).
.extract_raw_path_vec <- function(prepared, curl_path) {
  out <- curl_path
  ok <- !is.na(prepared) & !is.na(curl_path)
  if (!any(ok)) {
    return(out)
  }
  body <- stringi::stri_replace_first_regex(
    prepared[ok], "^[a-zA-Z][a-zA-Z0-9+.-]*://", ""
  )
  first <- stringi::stri_locate_first_regex(body, "[/?#]")[, 1L]
  empty_authority <- stringi::stri_startswith_fixed(body, "/")
  empty_authority[is.na(empty_authority)] <- FALSE
  has_path <- !is.na(first) &
    stringi::stri_sub(body, first, first) == "/" &
    !empty_authority
  raw <- curl_path[ok]
  if (any(has_path)) {
    bp <- body[has_path]
    start <- first[has_path]
    tail <- stringi::stri_sub(bp, start)
    stop_rel <- stringi::stri_locate_first_regex(tail, "[?#]")[, 1L]
    end <- ifelse(
      is.na(stop_rel), stringi::stri_length(bp), start + stop_rel - 2L
    )
    raw[has_path] <- stringi::stri_sub(bp, start, end)
  }
  out[ok] <- raw
  out
}

# Phase 2b: pull the raw components used downstream out of the curl result.
# With `params = FALSE` (see .parse_with_curl) `parsed_curl$query` is already
# the raw (percent-encoded) query string, so it is taken verbatim; downstream
# parsers split on raw "&"/"=" then decode per-pair. scheme/host as-is. The path
# is re-derived from the prepared input by .extract_raw_path_vec() (see there)
# so dot segments survive to `path_normalization`.
.extract_raw_components <- function(parsed_curl, prepared) {
  list(
    scheme = parsed_curl$scheme %||% NA_character_,
    host = parsed_curl$host %||% NA_character_,
    path = .extract_raw_path_vec(
      prepared, parsed_curl$path %||% NA_character_
    ),
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
                                trailing_slash_handling,
                                path_identity = "none") {
  path_work <- raw_path

  # Two ORTHOGONAL axes (ADR 0011), applied as independent, composing steps:
  #   1. path IDENTITY (`path_identity`) -- the url_standard profile's
  #      normalization of which percent-octets are canonicalized in the path a
  #      URL denotes (".rfc3986_unreserved" / ".whatwg_preserve"; "none" =
  #      default). Profile-internal; never a public argument.
  #   2. path PRESENTATION (`path_encoding`) -- the public keep/encode/decode
  #      readable-vs-browser rendering, which LAYERS on any identity mode.
  # Non-WHATWG presentation `decode`/`encode` full-decodes FIRST (as it always
  # has), then the identity mode. WHATWG `encode` is deliberately different:
  # the standard's path serializer preserves existing percent spellings, so it
  # must not full-decode before encoding.
  whatwg_encode <- path_encoding == "encode" &&
    identical(path_identity, ".whatwg_preserve")

  # Presentation: decode path (before normalization/index handling) when the
  # public knob requests decode OR encode (encode decodes first, re-encodes
  # last).
  if (path_encoding %in% c("decode", "encode") && !whatwg_encode) {
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

  # Identity: apply the profile's path-identity normalization (independent of
  # the presentation step above).
  if (path_identity == ".rfc3986_unreserved") {
    # url_standard = "rfc3986" (RURL-gjltzwmp, PRD S6.1): decode ONLY
    # unreserved percent-encoded octets, and do it BEFORE dot-segment removal
    # -- ordering is normative, not incidental. Decoding first folds an
    # encoded dot (%2E / %2E%2E) to a literal "."/".." segment, which the
    # dot_segments step below then resolves via the existing literal matcher.
    # Reserved bytes (%2F, %3F, %23, ...) are never decoded here.
    mask <- !is.na(path_work) & stringi::stri_detect_fixed(path_work, "%")
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], .rfc_unreserved_normalize, character(1),
        USE.NAMES = FALSE
      )
    }
  } else if (path_identity == ".whatwg_preserve" && !whatwg_encode) {
    # url_standard = "whatwg" (RURL-bbmuehsx, PRD S6.1): never decode or
    # canonicalize existing percent-triplets. Dot-segment resolution below uses
    # the encoded-dot-aware remover, so encoded dot segments (%2e/%2e%2e) still
    # resolve without a general decode.
    mask <- !is.na(path_work) & stringi::stri_detect_fixed(path_work, "%")
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], .whatwg_preserve_normalize, character(1),
        USE.NAMES = FALSE
      )
    }
  } else if (path_identity == "none" && path_encoding == "keep") {
    # Preserve the historical no-selector `keep` contract after Stage A became
    # byte-faithful: percent-triplet hex case is canonicalized here instead of
    # during raw extraction.
    mask <- !is.na(path_work) & stringi::stri_detect_fixed(path_work, "%")
    if (any(mask)) {
      path_work[mask] <- .uppercase_percent_hex(path_work[mask])
    }
  }

  # Slash collapsing.
  if (path_normalization %in% c("collapse_slashes", "both")) {
    mask <- !is.na(path_work) & nzchar(path_work)
    path_work[mask] <- gsub("/+", "/", path_work[mask], perl = TRUE)
  }

  # Dot-segment resolution (RFC 3986 S5.2.4), or -- under the "whatwg" path
  # profile (RURL-bbmuehsx) -- the encoded-dot-aware variant that recognizes
  # "%2e"/"%2E" alongside literal "." without a general path decode. Both
  # removers are identity for paths without a matching dot segment, so only
  # candidate rows are processed.
  if (path_normalization %in% c("dot_segments", "both")) {
    is_whatwg <- identical(path_identity, ".whatwg_preserve")
    dot_remover <- if (is_whatwg) {
      ._remove_dot_segments_whatwg
    } else {
      ._remove_dot_segments
    }
    detect_pattern <- if (is_whatwg) {
      "(?i)(^|/)(\\.|%2e){1,2}(/|$)"
    } else {
      "(^|/)\\.{1,2}(/|$)"
    }
    mask <- !is.na(path_work) & nzchar(path_work) &
      stringi::stri_detect_regex(path_work, detect_pattern)
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], dot_remover, character(1), USE.NAMES = FALSE
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
  if (whatwg_encode) {
    mask <- !is.na(path_work)
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], .whatwg_path_percent_encode, character(1),
        USE.NAMES = FALSE
      )
    }
  } else if (path_encoding == "encode") {
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
                            trailing_slash_handling,
                            path_identity = "none") {
  .normalize_path_vec(
    raw_path, path_encoding, path_normalization,
    index_page_handling, trailing_slash_handling,
    path_identity = path_identity
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

# Phase 5b helper (vector): the WHATWG "ends in a number" checker
# (https://url.spec.whatwg.org/#ends-in-a-number-checker) -- the trigger that
# forces a host through the WHATWG IPv4 parser. Drops a single trailing dot,
# then returns TRUE when the FINAL label is a decimal integer (all ASCII digits,
# so leading-zero "09"/"08" count) or a hex literal ("0x"/"0X" + hex digits).
# Those are exactly the labels the IPv4 number parser accepts, so a TRUE host
# MUST parse to a valid IPv4 address or the whole host parse fails -- there is
# no reg-name fallback. This is broader than the all-numeric-parts is_ipv4ish
# flag: it also fires on mixed reg-name/number hosts (foo.09), hex/octal final
# labels (foo.0x4), trailing-dot forms (1.2.3.08.), and >4-part hosts libcurl
# leaves literal (0x1.2.3.4.5.). NA/"" hosts are FALSE.
.host_ends_in_number_vec <- function(host) {
  n <- length(host)
  ok <- !is.na(host) & host != ""
  if (!any(ok)) {
    return(rep(FALSE, n))
  }
  # Drop one trailing "." (a single empty final part), then take the last label.
  trimmed <- stringi::stri_replace_first_regex(host, "\\.$", "")
  last <- stringi::stri_replace_first_regex(trimmed, "^.*\\.", "")
  is_dec <- stringi::stri_detect_regex(last, "^[0-9]+$")
  is_hex <- stringi::stri_detect_regex(last, "^0[xX][0-9a-fA-F]*$")
  is_dec[is.na(is_dec)] <- FALSE
  is_hex[is.na(is_hex)] <- FALSE
  ok & (is_dec | is_hex)
}

# Phase 5b helper (scalar): WHATWG IPv6 serializer for bracketed literals. The
# WHATWG host parser stores IPv6 as eight 16-bit pieces; dotted-quad tails are
# folded into two pieces before serialization, and the longest zero run is
# compressed (`[::127.0.0.1]` -> `[::7f00:1]`). Invalid inputs return unchanged;
# validation/fatal decisions stay with the existing host model.
.serialize_whatwg_ipv6_host <- function(host) {
  if (is.na(host) || !stringi::stri_detect_regex(host, "^\\[.*\\]$")) {
    return(host)
  }

  inner <- stringi::stri_sub(host, 2L, -2L)
  oct <- "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])"
  m <- stringi::stri_match_first_regex(
    inner, paste0("^(.*:)(", oct, "\\.", oct, "\\.", oct, "\\.", oct, ")$")
  )
  if (!is.na(m[1L, 1L])) {
    quad <- strsplit(m[1L, 3L], ".", fixed = TRUE)[[1L]]
    octets <- suppressWarnings(as.integer(quad))
    if (length(octets) != 4L || anyNA(octets) ||
        any(octets < 0L | octets > 255L)) {
      return(host)
    }
    inner <- paste0(
      m[1L, 2L],
      as.hexmode(octets[1L] * 256L + octets[2L]), ":",
      as.hexmode(octets[3L] * 256L + octets[4L])
    )
  }

  parts <- strsplit(inner, "::", fixed = TRUE)[[1L]]
  if (length(parts) > 2L) {
    return(host)
  }

  split_side <- function(x) {
    if (is.na(x) || x == "") {
      return(character(0))
    }
    strsplit(x, ":", fixed = TRUE)[[1L]]
  }
  if (length(parts) == 1L) {
    hextets <- split_side(parts[1L])
    if (length(hextets) != 8L) {
      return(host)
    }
  } else {
    left <- split_side(parts[1L])
    right <- split_side(parts[2L])
    zero_count <- 8L - length(left) - length(right)
    if (zero_count < 1L) {
      return(host)
    }
    hextets <- c(left, rep("0", zero_count), right)
  }
  if (length(hextets) != 8L ||
      !all(grepl("^[0-9A-Fa-f]{1,4}$", hextets))) {
    return(host)
  }

  pieces <- suppressWarnings(strtoi(hextets, base = 16L))
  if (anyNA(pieces) || any(pieces < 0L | pieces > 65535L)) {
    return(host)
  }

  is_zero <- pieces == 0L
  run <- rle(is_zero)
  ends <- cumsum(run$lengths)
  starts <- ends - run$lengths + 1L
  zero_runs <- which(run$values & run$lengths > 1L)
  compress_start <- NA_integer_
  compress_len <- 0L
  if (length(zero_runs) > 0L) {
    best <- zero_runs[which.max(run$lengths[zero_runs])]
    compress_start <- starts[best]
    compress_len <- run$lengths[best]
  }

  rendered <- as.character(as.hexmode(pieces))
  if (is.na(compress_start)) {
    return(paste0("[", paste(rendered, collapse = ":"), "]"))
  }

  compress_end <- compress_start + compress_len - 1L
  before <- if (compress_start > 1L) {
    rendered[seq_len(compress_start - 1L)]
  } else {
    character(0)
  }
  after <- if (compress_end < 8L) {
    rendered[(compress_end + 1L):8L]
  } else {
    character(0)
  }

  serialized <- if (length(before) == 0L && length(after) == 0L) {
    "::"
  } else if (length(before) == 0L) {
    paste0("::", paste(after, collapse = ":"))
  } else if (length(after) == 0L) {
    paste0(paste(before, collapse = ":"), "::")
  } else {
    paste0(paste(before, collapse = ":"), "::", paste(after, collapse = ":"))
  }
  paste0("[", serialized, "]")
}

# Phase 5b helper (vector): delegates IPv6 WHATWG serialization to the scalar
# parser only for bracketed rows that can change.
.serialize_whatwg_ipv6_hosts_vec <- function(host) {
  out <- host
  elig <- !is.na(host) &
    stringi::stri_detect_regex(host, "^\\[[0-9A-Fa-f:.]+\\]$")
  elig[is.na(elig)] <- FALSE
  if (any(elig)) {
    out[elig] <- vapply(
      host[elig], .serialize_whatwg_ipv6_host, character(1),
      USE.NAMES = FALSE
    )
  }
  out
}

# Phase 5b (vector): the url_standard host IPv4/reg-name model (RURL-luwvkwhd,
# PRD §6.2). A no-op when url_standard is NULL (returns curl's host / IP flag
# unchanged, never fatal), so the default pipeline is byte-for-byte unaffected
# (AC #1).
#
# libcurl already coerces numeric IPv4 forms itself (2130706433 -> 127.0.0.1,
# 192.168.010.1 -> 192.168.8.1) and keeps out-of-range / over-arity forms
# literal (256.1.1.1, 1.2.3.4.5). So `curl_host` is the coerced spelling and
# `input_host` (the original pre-curl token) is the un-coerced one. For an
# IPv4 attempt (`is_attempt`):
#   - rfc3986: parse faithfully as a reg-name -- restore the original token and
#     treat it as an IP only if the ORIGINAL was already a canonical quad.
#     Never fatal (every such token is a valid RFC 3986 reg-name).
#   - whatwg: adopt curl's coercion; it is a valid WHATWG IPv4 exactly when
#     curl's output is a canonical dotted-quad. When it is not (out-of-range or
#     > 4 parts, which curl leaves literal), the WHATWG IPv4 parser rejects it,
#     so the row is fatal.
# Non-attempt hosts (ordinary names, IPv6, missing) pass through untouched.
# Returns updated `host`, `is_ip`, and a `fatal` mask the caller folds into the
# null-row set.
.apply_host_standard_model_vec <- function(input_host, curl_host, is_ip_curl,
                                           url_standard, is_attempt) {
  n <- length(curl_host)
  host <- curl_host
  is_ip <- is_ip_curl
  fatal <- rep(FALSE, n)
  if (is.null(url_standard)) {
    return(list(host = host, is_ip = is_ip, fatal = fatal))
  }

  if (identical(url_standard, "rfc3986")) {
    # Reg-name: keep the original token; IP only if it was already canonical.
    # RFC 3986 has no numeric-host special case, so the all-numeric is_ipv4ish
    # attempt set is the right (and unchanged) trigger here.
    att <- is_attempt & !is.na(is_attempt)
    if (any(att)) {
      input_canonical <- .detect_ip_host_vec(input_host)
      host[att] <- input_host[att]
      is_ip[att] <- input_canonical[att]
    }
  } else {
    # WHATWG: a host that "ends in a number" MUST parse as IPv4 or the whole
    # host parse fails -- independent of whether libcurl chose to coerce it.
    # This trigger (not is_ipv4ish) closes the gap where obfuscated/mixed forms
    # libcurl leaves as reg-names (foo.09, foo.0x4, 1.2.3.08., 0x1.2.3.4.5.)
    # bypassed the gate and were wrongly accepted with warning-invalid-tld.
    att <- .host_ends_in_number_vec(input_host)
    if (any(att)) {
      curl_canonical <- .detect_ip_host_vec(curl_host)
      is_ip[att] <- curl_canonical[att]
      fatal[att] <- !curl_canonical[att]
    }

    # WHATWG forbidden host/domain code points (RURL-jfuqpwvh). A special-scheme
    # host is a DOMAIN: WHATWG fails the host parse if domain-to-ASCII produces
    # a forbidden domain code point, or if domain-to-ASCII itself fails (a
    # disallowed / ignored-to-empty code point). rurl previously accepted these
    # as reg-names with warning-no-tld; the whatwg profile now rejects them
    # (flips the ADR 0004 boundary; governed axis per ADR 0007). Two narrow
    # signals on the resolved reg-name host (IP literals excluded):
    #   (B) an ASCII forbidden code point survives to the host (| ^ DEL space %
    #       and structural bytes) -- a cheap charclass, catches what UTS-46
    #       leaves intact (or, for DEL, silently drops);
    #   (A) a NON-ASCII host fails UTS-46 domain-to-ASCII (U+FFFD/U+FFFF
    #       noncharacters, a soft-hyphen-only label collapsing to empty) -- one
    #       vectorized punycoder::host_normalize() over just the non-ASCII rows.
    reg <- !is_ip & !is.na(host) & host != ""
    if (any(reg)) {
      bad_cp <- reg &
        stringi::stri_detect_regex(host, .WHATWG_FORBIDDEN_HOST_CP)
      bad_cp[is.na(bad_cp)] <- FALSE
      fatal <- fatal | bad_cp

      nonascii <- reg & !bad_cp &
        stringi::stri_detect_regex(host, "[^\\u0001-\\u007f]")
      nonascii[is.na(nonascii)] <- FALSE
      if (any(nonascii)) {
        norm <- punycoder::host_normalize(
          host[nonascii], check_hyphens = FALSE, use_std3 = FALSE,
          verify_dns_length = FALSE
        )
        bad_uts46 <- rep(FALSE, n)
        bad_uts46[nonascii] <- is.na(norm)
        fatal <- fatal | bad_uts46
      }
    }

    # WHATWG IPv6 serializer (RURL-thjmzaam): bracketed IPv6 literals serialize
    # as eight 16-bit pieces with zero compression; an embedded dotted-quad IPv4
    # tail never remains dotted.
    host <- .serialize_whatwg_ipv6_hosts_vec(host)
  }

  list(host = host, is_ip = is_ip, fatal = fatal)
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
.apply_host_encoding_vec <- function(final_host, host_encoding, is_ip_host,
                                     url_standard = NULL) {
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
    if (.is_whatwg(url_standard)) {
      encoded <- punycoder::host_normalize(
        subset, check_hyphens = FALSE, use_std3 = FALSE,
        verify_dns_length = FALSE
      )
      retry <- is.na(encoded)
      if (any(retry)) {
        encoded[retry] <- .normalize_and_punycode_vec(subset[retry])
      }
    } else {
      encoded <- .normalize_and_punycode_vec(subset)
    }
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
.apply_host_encoding <- function(final_host, host_encoding, is_ip_host,
                                 url_standard = NULL) {
  .apply_host_encoding_vec(final_host, host_encoding, is_ip_host, url_standard)
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

# Query-filter (vector): map the scalar query engine (._filter_query_params,
# path-query.R) over each row's raw query, returning the canonical, re-encoded
# query WITHOUT a leading "?" per row -- or "" where nothing survives or the
# query is absent. "drop" (the default) short-circuits to "" for every row,
# preserving the historical query-free clean_url. Only non-NA/non-empty raw
# queries reach the scalar engine, so the common (query-less) URL pays nothing.
#
# This is a Stage B (presentation) transform: it depends only on the raw query
# (a Stage A column) and the query options, never on the expensive parse core.
# Like clean_url itself it is recomputed on every call and never cached, which
# is exactly why the query options are excluded from the full_parse cache key
# (see .parse_cache_keys, parse.R).
.filter_query_vec <- function(raw_query, opts) {
  n <- length(raw_query)
  out <- rep("", n)
  if (identical(opts$query_handling, "drop")) {
    return(out)
  }
  idx <- which(!is.na(raw_query) & nzchar(raw_query))
  if (length(idx) == 0L) {
    return(out)
  }
  # vapply passes the query options straight through to ._filter_query_params()
  # as trailing named args (the raw query is its first positional argument).
  out[idx] <- vapply(
    raw_query[idx], ._filter_query_params, character(1),
    query_handling = opts$query_handling,
    params_keep = opts$params_keep,
    params_drop = opts$params_drop,
    params_case_sensitive = opts$params_case_sensitive,
    sort_params = opts$sort_params,
    empty_param_handling = opts$empty_param_handling,
    decode_plus = opts$decode_plus,
    USE.NAMES = FALSE
  )
  out
}

# Look up the WHATWG default port for a (possibly mixed-case) scheme vector
# (PRD v2 D1, RURL-qdlvldts). Case-folds before the table lookup since `scheme`
# may carry the caller's original casing (case_handling need not be
# "lower_host") -- mirrors the ftp-status scheme fold in
# .derive_parse_status_vec(). Returns NA for a scheme with no WHATWG default
# (ftps, NA, or anything outside .SCHEME_DEFAULT_PORTS).
.scheme_default_port_vec <- function(scheme) {
  unname(.SCHEME_DEFAULT_PORTS[stringi::stri_trans_tolower(scheme)])
}

# Phase 13 port output: WHATWG parsing nulls a port that equals the special
# scheme's default, so the public parse-result `port` column must use that
# effective value for parity. `port_handling = "keep"` is the explicit
# non-parity escape hatch for callers that need to retain the syntactic port.
.apply_port_output_policy_vec <- function(scheme, port, port_handling,
                                          url_standard) {
  if (is.null(port) ||
      !.is_whatwg(url_standard) ||
      identical(port_handling, "keep")) {
    return(port)
  }

  out <- port
  has_port <- !is.na(out)
  if (!any(has_port)) {
    return(out)
  }

  default_port <- .scheme_default_port_vec(scheme)
  scheme_lc <- stringi::stri_trans_tolower(scheme)
  elide <- has_port &
    !is.na(default_port) &
    out == default_port &
    scheme_lc %in% .WHATWG_SPECIAL_SCHEMES
  out[elide] <- NA_integer_
  out
}

# Phase 11 port component (PRD v2 D1, RURL-qdlvldts): "" (excluded) or
# ":<port>" per the standalone `port_handling` knob. `strip_default` is the
# WHATWG/RFC-style default-port elision renderer. `keep` is literal, including
# under `url_standard = "whatwg"`; callers use it as an explicit non-parity
# override when they need to retain a syntactic default port. `scheme` keys the
# default-port table only -- it is never rendered here (the caller already
# embeds the cased scheme in `scheme_part`).
.build_port_part_vec <- function(scheme, port, port_handling, url_standard) {
  # Retained for call-site stability; port rendering is selected entirely by
  # `port_handling` now that `keep` is the literal override in every profile.
  force(url_standard)
  n <- length(scheme)
  port_part <- rep("", n)
  if (is.null(port) ||
        port_handling %in% c("exclude", "strip_all")) {
    return(port_part)
  }
  has_port <- !is.na(port)
  if (!any(has_port)) {
    return(port_part)
  }

  default_port <- .scheme_default_port_vec(scheme)
  is_default <- has_port & !is.na(default_port) & port == default_port

  if (identical(port_handling, "strip_default")) {
    keep <- has_port & !is_default
  } else {
    keep <- has_port
  }
  port_part[keep] <- paste0(":", port[keep])
  port_part
}

# Phase 11 (vector): reconstruct the canonical "clean" URL from cased
# components, then append the filtered query. NA/empty host yields NA.
# `query` is a per-row canonical query WITHOUT a leading "?" (from
# .filter_query_vec); "" means no query for that row. It is appended AFTER the
# cased scheme/host/path because query values are case-exempt (see
# ._parse_stage_b_vec). NULL `query` (the scalar wrapper and the phase unit
# tests) appends nothing, so clean_url stays scheme+host+path only. `port` is
# the raw (unfiltered) port column; NULL (the scalar wrapper and phase unit
# tests) or `port_handling = "exclude"` (the default) keeps clean_url
# port-free, exactly as before.
.build_clean_url_vec <- function(scheme_output, host_output, path_output,
                                 trailing_slash_handling, query = NULL,
                                 port = NULL, port_handling = "exclude",
                                 url_standard = NULL) {
  n <- length(host_output)
  clean_url <- rep(NA_character_, n)
  has_host <- !is.na(host_output) & host_output != ""
  scheme_lc <- stringi::stri_trans_tolower(scheme_output)
  is_file <- !is.na(scheme_lc) & scheme_lc == "file"
  buildable <- has_host | is_file
  if (!any(buildable)) {
    return(clean_url)
  }

  scheme_part <- ifelse(!is.na(scheme_output), paste0(scheme_output, "://"), "")
  port_part <- .build_port_part_vec(
    scheme_output, port, port_handling, url_standard
  )
  path_part <- ifelse(!is.na(path_output), path_output, "")
  if (trailing_slash_handling == "strip") {
    path_part[path_part == "/"] <- ""
  }
  host_part <- ifelse(has_host, host_output, "")
  clean_url[buildable] <- paste0(
    scheme_part[buildable], host_part[buildable], port_part[buildable],
    path_part[buildable]
  )
  if (!is.null(query)) {
    q_present <- buildable & !is.na(query) & nzchar(query)
    clean_url[q_present] <- paste0(clean_url[q_present], "?", query[q_present])
  }
  clean_url
}

# Phase 11 (scalar wrapper): delegates to .build_clean_url_vec(). Keeps the
# historical query-free signature (no `query` arg) as the default, so the
# scalar orchestrator and phase unit tests reconstruct scheme+host+path only
# unless a caller explicitly opts into the port arguments.
.build_clean_url <- function(scheme_output, host_output, path_output,
                             trailing_slash_handling, port = NULL,
                             port_handling = "exclude", url_standard = NULL) {
  .build_clean_url_vec(
    scheme_output, host_output, path_output, trailing_slash_handling,
    port = port, port_handling = port_handling, url_standard = url_standard
  )
}

# --- Layer 3b posture serializers (ADR 0012 D2 / Appendix A.1, RURL-mgmviuta) -
#
# These render a clean_url-style string for the non-special / opaque /
# empty-host / RFC-generic URL shapes that the (not-yet-public) `general`
# acceptance will produce. They are PURELY ADDITIVE: L4b activates `general`
# and wires them into the Phase-11 dispatch. Nothing here is called from the
# live pipeline, so byte-identity of every existing output is automatic.
# `.build_clean_url_vec` above is left verbatim as the special/host-present
# serializer L4b keeps using.
#
# Both follow rurl's clean_url product contract: the FRAGMENT is EXCLUDED, and
# an empty-but-PRESENT query serializes as a trailing "?". They take
# already-parsed state as explicit args (the state vocabulary from
# R/parse-state.R: host_kind, path_kind, query_kind, rfc_path_form) and use the
# repo's pre-allocate + logical-mask assignment idiom.

# WHATWG serializer (ADR 0012 A.1 #concept-url-serializer + D2). Keys authority
# emission off the null-vs-empty host distinction and applies the four-condition
# `/.` guard for a null-host list path. Reuses the existing byte-level encoders
# (R/path-query.R): opaque paths take the C0-control set only
# (`.whatwg_component_percent_encode(path, integer(0))`), list paths take the
# path percent-encode set (`.whatwg_path_percent_encode`), and both already
# preserve existing `%xx` spellings. `port` may be NULL (no port) or a vector.
.serialize_whatwg_vec <- function(scheme, host, host_kind, path, path_kind,
                                  query, query_kind, port, port_handling,
                                  trailing_slash_handling) {
  n <- max(
    length(scheme), length(host), length(host_kind), length(path),
    length(path_kind), length(query), length(query_kind)
  )
  scheme <- rep_len(scheme, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  path <- rep_len(path, n)
  path_kind <- rep_len(path_kind, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  if (!is.null(port)) {
    port <- rep_len(port, n)
  }

  scheme_prefix <- paste0(scheme, ":")
  port_part <- .build_port_part_vec(scheme, port, port_handling, "whatwg")
  is_opaque <- path_kind == "opaque"

  # Per-row body encoding + the `/.` guard. The byte encoders are scalar
  # (they walk one string), so this row loop is the vectorization seam; the
  # structural assembly below stays mask-based.
  path_body <- character(n)
  guard <- rep("", n)
  for (i in seq_len(n)) {
    p <- path[i]
    if (is.na(p)) {
      path_body[i] <- ""
      next
    }
    if (is_opaque[i]) {
      # Opaque path: WHATWG C0-control percent-encode set (empty extra set ->
      # encodes only C0/DEL/non-ASCII). No `/.` guard for opaque paths.
      path_body[i] <- .whatwg_component_percent_encode(p, integer(0))
      next
    }
    # List path. Optional trailing-slash strip mirrors .build_clean_url_vec
    # (a lone "/" -> ""); it only ever touches a size-1 path, never the guard.
    p_render <- p
    strip_slash <- identical(trailing_slash_handling, "strip")
    if (strip_slash && identical(p_render, "/")) {
      p_render <- ""
    }
    path_body[i] <- .whatwg_path_percent_encode(p_render)

    # Four-condition `/.` guard (ADR 0012 A.1 #concept-url-serializer): fires
    # when host is null (host_kind "absent"), the path is a list (not opaque),
    # its size is > 1, and its FIRST segment is empty -- i.e. the serialized
    # path would begin with "//" and be misread as an authority.
    #
    # SEGMENT DERIVATION: rurl holds a list path as its serialized STRING (each
    # WHATWG path segment rendered as "/" + segment, so a rooted list path
    # always begins with "/"). To recover the WHATWG path-list segment view we
    # split on "/" and DROP the leading "" element (the empty piece before the
    # first "/"): strsplit("/bar") -> c("","bar") -> list c("bar") (size 1);
    # strsplit("//bar") -> c("","","bar") -> list c("","bar") (size 2, first
    # ""); strsplit("/") -> c("") -> list character(0) (size 0). strsplit drops
    # a single TRAILING "" (a trailing-slash path), but the guard only inspects
    # size>1 and the first segment, so that omission is immaterial here.
    if (host_kind[i] == "absent") {
      segs <- strsplit(p, "/", fixed = TRUE)[[1]]
      segs <- segs[-1L]
      if (length(segs) > 1L && !is.na(segs[1L]) && segs[1L] == "") {
        guard[i] <- "/."
      }
    }
  }

  query_suffix <- .whatwg_query_suffix_vec(query, query_kind, scheme)

  out <- character(n)
  out[is_opaque] <- paste0(scheme_prefix[is_opaque], path_body[is_opaque])

  # List path with an authority (host present OR empty-but-non-null): emit
  # `//` + host (host may be "") + port. `foo:///bar` = "foo://" + "" + "/bar".
  auth <- !is_opaque & host_kind != "absent"
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", host_str[auth], port_part[auth], path_body[auth]
  )

  # List path with a null host (no authority): NO `//`; the `/.` guard, if it
  # fired, sits between the scheme and the path. `foo:/bar` -> "foo:/bar".
  noauth <- !is_opaque & host_kind == "absent"
  out[noauth] <- paste0(
    scheme_prefix[noauth], guard[noauth], path_body[noauth]
  )

  paste0(out, query_suffix)
}

# RFC 3986 generic serializer (ADR 0012 D1/D2, rfc-syntax posture). NO
# normalization, NO opaque/list distinction, NO `/.` guard, NO dot-segment
# removal, NO case folding: it is a faithful generic serialization that
# preserves the source path bytes. `rfc_path_form` is informational under this
# posture -- the path string is already in its final source-preserving shape,
# so nothing structural keys off it here (force() marks it deliberately
# consumed, mirroring .build_port_part_vec's force(url_standard)). The query is
# preserved verbatim (no percent-encoder) under rfc-syntax's "preserve source"
# disclaimer. `port` may be NULL or a vector.
.serialize_rfc_generic_vec <- function(scheme, host, host_kind, path,
                                       rfc_path_form, query, query_kind,
                                       port, port_handling) {
  force(rfc_path_form)
  n <- max(
    length(scheme), length(host), length(host_kind), length(path),
    length(query), length(query_kind)
  )
  scheme <- rep_len(scheme, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  path <- rep_len(path, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  if (!is.null(port)) {
    port <- rep_len(port, n)
  }

  scheme_prefix <- paste0(scheme, ":")
  port_part <- .build_port_part_vec(scheme, port, port_handling, "rfc3986")
  path_body <- ifelse(is.na(path), "", path) # source-preserving; no encoding

  out <- character(n)
  auth <- host_kind != "absent"
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", host_str[auth], port_part[auth], path_body[auth]
  )
  out[!auth] <- paste0(scheme_prefix[!auth], path_body[!auth])

  # rfc-syntax preserves source bytes: append the query delimiter/value
  # verbatim rather than through a WHATWG percent-encoder. present -> "?query";
  # empty -> bare "?"; absent -> nothing. Fragment always excluded.
  query_suffix <- rep("", n)
  is_present <- query_kind == "present"
  query_suffix[is_present] <- paste0("?", ifelse(
    is.na(query[is_present]), "", query[is_present]
  ))
  query_suffix[query_kind == "empty"] <- "?"

  paste0(out, query_suffix)
}

# Shared WHATWG query suffix (ADR 0012 D2): present -> "?" + special-ness-keyed
# percent-encode; empty -> bare "?"; absent -> nothing. Fragment excluded.
.whatwg_query_suffix_vec <- function(query, query_kind, scheme) {
  n <- length(query_kind)
  suffix <- rep("", n)
  for (i in seq_len(n)) {
    if (query_kind[i] == "empty") {
      suffix[i] <- "?"
    } else if (query_kind[i] == "present") {
      encoded <- .whatwg_query_percent_encode(query[i], scheme[i])
      if (is.na(encoded)) {
        encoded <- ""
      }
      suffix[i] <- paste0("?", encoded)
    }
  }
  suffix
}

# Phase 12 (vector): classify the parse outcome (ok / ok-ftp / warning-* /
# error / ok-scheme-relative). `curl_ok` is TRUE for rows curl parsed (the
# scalar wrapper passes !is.null(parsed_curl)). Progressive mask assignment
# mirrors the scalar precedence exactly.
.derive_parse_status_vec <- function(curl_ok, final_host, is_ip_host, tld,
                                     domain, protocol_handling, final_scheme,
                                     looks_like_protocol,
                                     original_has_allowed_scheme,
                                     looks_like_host_port,
                                     is_scheme_relative,
                                     scheme_relative_handling,
                                     rfc3986_path_rootless = NULL,
                                     scheme_acceptance = "web") {
  n <- length(final_host)
  if (is.null(rfc3986_path_rootless)) {
    rfc3986_path_rootless <- rep(FALSE, n)
  }
  status <- rep(.STATUS_ERROR, n)

  scheme_lower <- stringi::stri_trans_tolower(final_scheme)
  is_file <- curl_ok & !is.na(scheme_lower) & scheme_lower == "file"
  is_rfc3986_path_rootless <- curl_ok & rfc3986_path_rootless
  host_present <- curl_ok & !is.na(final_host) & final_host != ""

  status[host_present & is_ip_host] <- .STATUS_OK
  status[is_file] <- .STATUS_OK
  status[is_rfc3986_path_rootless] <- .STATUS_OK

  non_ip <- host_present & !is_ip_host & !is_file
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
    is_ftp <- ftp_candidate & scheme_lower %in% c("ftp", "ftps")
    is_ftp[is.na(is_ftp)] <- FALSE
    status[is_ftp] <- .STATUS_OK_FTP
  }

  # ADR 0012 D3 (Option B): armed on the scheme-ACCEPTANCE axis, in lock-step
  # with Phase 1 (.prepare_urls_for_curl_vec). Under "web" an unsupported
  # scheme-bearing token is demoted to error regardless of protocol_handling;
  # under "general" the demotion is suppressed. host:port inputs match the
  # scheme regex (`example.com:` looks like a scheme) but Phase 1 already
  # recognizes and parses them as host:port, so the `!looks_like_host_port`
  # term keeps them from being demoted here (RURL-aldwnots).
  if (scheme_acceptance == "web") {
    unsupported <- looks_like_protocol &
      !original_has_allowed_scheme &
      !looks_like_host_port
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
                                 looks_like_host_port,
                                 is_scheme_relative,
                                 scheme_relative_handling,
                                 rfc3986_path_rootless = NULL) {
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
    looks_like_host_port = looks_like_host_port,
    is_scheme_relative = is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling,
    rfc3986_path_rootless = rfc3986_path_rootless
  )
}

# Phase 13 (vector): build the 14 result columns. Coerces the scheme-relative
# "keep" scheme to NA and an empty host to NA, matching the scalar assembler.
# The engine applies error-row defaults for NULL-equivalent rows afterward.
.assemble_parse_result_vec <- function(original_url, scheme_output, host_output,
                                       port, path_output, raw_query, fragment,
                                       user, password, domain, tld,
                                       domain_ascii, domain_unicode,
                                       tld_ascii, tld_unicode, is_ip_host,
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
    domain_ascii = domain_ascii,
    domain_unicode = domain_unicode,
    tld_ascii = tld_ascii,
    tld_unicode = tld_unicode,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status
  )
}

# Phase 13 (scalar wrapper): extracts port/fragment/user/password from the curl
# object, then delegates to .assemble_parse_result_vec().
.assemble_parse_result <- function(original_input_url, scheme_output,
                                   host_output, parsed_curl, path_output,
                                   raw_query, domain, tld,
                                   domain_ascii, domain_unicode,
                                   tld_ascii, tld_unicode, is_ip_host,
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
    domain_ascii = domain_ascii,
    domain_unicode = domain_unicode,
    tld_ascii = tld_ascii,
    tld_unicode = tld_unicode,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status,
    is_scheme_relative = is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )
}
