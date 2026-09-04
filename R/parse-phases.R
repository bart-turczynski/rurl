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
# .parse_web_url_one()/.extract_raw_components() stay scalar because the web
# parser is scalar; the engine reuses it in its single per-URL loop
# and extracts the raw columns itself.
# ---------------------------------------------------------------------------

# Phase 1 helper (vector): classify the host token of each raw input for the
# host-shape gate. rurl only fabricates a scheme for URL-shaped input, so we
# must isolate and inspect the host before the web parse. Returns, per element:
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

  host_lower <- .ascii_tolower(host_token)
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
    # Original (pre-parse) host token and the IPv4-attempt flag, surfaced for
    # the url_standard host model (RURL-luwvkwhd): the web parser coerces
    # numeric IPv4
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
# same pre-parse step also implements the WHATWG special-authority-slashes
# state for inputs with no slash run at all: `http:example.com` is handed to the
# parser as `http://example.com`. This must run BEFORE the parse (the parser
# never treats "\" as a separator and rejects missing `//` authority forms), so
# it rewrites the raw string here in Phase 1, ahead of every other scheme/host
# regex in this function -- once rewritten, the existing "://"/"//" detection,
# host classification, and the parser handoff need no further changes.
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
  scheme_lower <- .ascii_tolower(scheme_match[, 2L])
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
  # slash run from "///" to "//" would turn it into file://path, which the
  # parser
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

# WHATWG input stripping -- the basic URL parser's step 1, BOTH halves
# (RURL-tyetpjym, RURL-yvxpanix). Step 1 says, in this order:
#   1a. Remove any leading and trailing C0 control or SPACE (U+0000..U+0020)
#       from the input.
#   1b. Remove ALL ASCII tab (U+0009), LF (U+000A) and CR (U+000D) from the
#       input, everywhere in the string.
# Both run before any component is parsed, so every downstream scheme/host
# regex and the backslash recognizer see the already-stripped string. rurl
# otherwise rejects a control char in the authority (the parser errors) and
# percent-encodes a trailing space into the path -- correct under RFC 3986,
# which has no strip step and requires such bytes to be percent-encoded -- so
# this runs ONLY under url_standard == "whatwg" and is a byte-for-byte no-op
# otherwise. Note the spec order matters for fact attribution: a LEADING tab is
# removed by 1a, not 1b.
#
# Neither strip is silent. Two SEPARATE masks are returned, because they are two
# different facts (ADR 0006 -- surface the mutation as a FACT; ADR 0009's
# precedent of a new token for a new fact):
#   - `leading_trailing_stripped` -> the `leading-trailing-stripped` diagnostic
#     (rows where 1a removed a leading/trailing C0-or-space run).
#   - `control_char_stripped` -> the `control-char-stripped` diagnostic (rows
#     where 1b removed an interior tab/LF/CR). Only tab/LF/CR are removed by
#     1b; other interior C0 controls are left to the web parser.
# A row can carry both. Resolves the control-char-in-authority family: c0-tab
# probe, eq-U6 (LF), yal-002 (TAB), yal-003 (CR/LF), which WHATWG
# strips-and-accepts.
#
# This function is deliberately the single seam shared by prep, the Stage-A
# general route, the Stage-B general re-parse and `.has_explicit_authority()`:
# putting step 1 here (never at a call site) is what keeps every stage fed
# byte-identical input by construction.
.strip_whatwg_control_chars_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(
    url = url,
    control_char_stripped = rep(FALSE, n),
    leading_trailing_stripped = rep(FALSE, n)
  )
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }

  # Step 1a: leading/trailing C0-control-or-SPACE. `\z` (true end of input), not
  # `$`: ICU's `$` also matches before a final line terminator, and LF/CR/VT/FF
  # all live inside this class.
  url_out <- url
  trimmed <- stringi::stri_replace_first_regex(
    url_out, "^[\\u0000-\\u0020]+", ""
  )
  trimmed <- stringi::stri_replace_first_regex(
    trimmed, "[\\u0000-\\u0020]+\\z", ""
  )
  lt <- !is.na(trimmed) & !is.na(url_out) & trimmed != url_out
  lt[is.na(lt)] <- FALSE
  if (any(lt)) {
    url_out[lt] <- trimmed[lt]
  }

  # Step 1b: every remaining tab/LF/CR, anywhere.
  had <- stringi::stri_detect_regex(url_out, "[\\t\\n\\r]")
  had[is.na(had)] <- FALSE
  if (any(had)) {
    url_out[had] <- stringi::stri_replace_all_regex(
      url_out[had], "[\\t\\n\\r]", ""
    )
  }

  list(
    url = url_out,
    control_char_stripped = had,
    leading_trailing_stripped = lt
  )
}

# WHATWG / UTS-46 alternative full-stop mapping (RURL-odsmwsxu). UTS-46
# domain-to-ASCII -- the "map" step the WHATWG host parser runs for special
# schemes -- maps three alternative full-stop code points to ASCII "." before a
# host is split into labels: U+3002 (ideographic full stop), U+FF0E (fullwidth
# full stop), and U+FF61 (halfwidth ideographic full stop). rurl hands the raw
# string to the web parser, which does NOT apply UTS-46, so a Unicode-dot host
# like "127。0。0。1" reaches it with its separators intact, is never split into
# numeric labels, and so never coerces to the canonical dotted-quad (an
# SSRF-relevant loopback/metadata obfuscation -- browsers coerce). The upstream
# host-shape gate is no help either: .classify_input_host_vec()'s ipv4-attempt
# test splits on ASCII "." only, so a literal-Unicode-dot host is not even seen
# as an IPv4 attempt. Mapping the three code points to "." here, BEFORE the
# parse,
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

# WHATWG userinfo charset acceptance (RURL-micalqvh, half (a)). The parser
# refuses an authority whose userinfo carries any of 30 ASCII code points --
# SPACE (0x20), the C0 controls (0x00-0x1F) and DEL (0x7F) -- so
# `http://a b@host/` errors even though WHATWG parses it and keeps the host.
# Every other userinfo byte the parser already accepts, including non-ASCII and
# existing percent-triplets, so the set is exactly those 30 and no wider. Each
# is a member of the WHATWG userinfo percent-encode set, i.e. the
# percent-encoded form written here IS the spelling WHATWG stores; the parser
# is asked
# to parse with `decode = FALSE`, so no restore step is needed (contrast the ADR
# 0009 host shim, which substitutes non-spec filler and must restore). "%" is
# not in the set, so an already-encoded userinfo (`%25DOMAIN`, `u%40ser`) is
# never double-encoded.
#
# THIS IS NOT COMPENSATION, which is why it survived RURL-ezhzpkhg deletion 3
# while the repeated-"@" repair that used to share this function did not. That
# repair (RURL-zqhgezuq) compensated for a parse seam that rejected a second
# "@" -- the C-03 disposition in design/work/url-v3/contracts/
# validation-intervention-contract.md owns that history. Splitting at the LAST
# "@" is what the WHATWG authority state does, so the behaviour moved into the
# parser as `.parse_web_url_one(last_at_userinfo = TRUE)`. What is left
# here changes the SPELLING of a userinfo the parser would otherwise refuse, and
# the spelling it writes is the one WHATWG stores -- a normalization the parser
# cannot infer, because `rfc3986` must keep the same bytes source-preserving.
#
# GATED ON `.is_whatwg()` EXPLICITLY, for exactly that reason: `rfc3986` has no
# userinfo production for a space or a control byte.
.encode_userinfo_charset_vec <- function(url, url_standard) {
  no_op <- list(url = url)
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }

  m <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*:)(//)([^/?#]*)(.*)$"
  )
  authority <- m[, 4L]
  has_at <- !is.na(authority) & stringi::stri_count_fixed(authority, "@") > 0L
  has_at[is.na(has_at)] <- FALSE

  # Any of the 30 parser-refused code points inside the userinfo span
  # (everything before the LAST "@"). `[\s\S]` rather than `.` because ICU
  # excludes U+000B/U+000C from `.`, and those are in the set.
  eligible <- rep(FALSE, length(url))
  if (any(has_at)) {
    userinfo_span <- stringi::stri_replace_last_regex(
      authority[has_at], "@[^@]*\\z", ""
    )
    hit <- stringi::stri_detect_regex(
      userinfo_span, "[\\u0000-\\u0020\\u007F]"
    )
    hit[is.na(hit)] <- FALSE
    eligible[has_at] <- hit
  }

  if (!any(eligible)) {
    return(no_op)
  }

  repaired <- vapply(which(eligible), function(i) {
    a <- authority[i]
    # Byte-indexed for the same reason as the shim's slice (RURL-kmpnbvdl):
    # `authority` is a stringi capture, so it is declared UTF-8 no matter what
    # octets it holds, and `substr()` threw on `http://a@@<80>b/`.
    last <- .last_byte_index(a, "@")
    paste0(
      .percent_encode_userinfo_charset(.byte_substring(a, 1L, last - 1L)),
      .byte_substring(a, last)
    )
  }, character(1), USE.NAMES = FALSE)

  url_out <- url
  url_out[eligible] <- paste0(
    m[eligible, 2L], m[eligible, 3L], repaired, m[eligible, 5L]
  )
  no_op$url <- url_out
  no_op
}

# Percent-encode the 30 ASCII code points refused in a userinfo: SPACE,
# the C0 controls and DEL. U+0000 cannot occur in an R string (and
# `rawToChar(as.raw(0L))` is ""), so the literal table covers 0x01-0x20 and
# 0x7F; the regex above still names the full 0x00-0x20 range. "%" is absent from
# the table, so the substitution is idempotent over already-encoded input.
.percent_encode_userinfo_charset <- function(userinfo) {
  codes <- c(seq.int(1L, 32L), 127L)
  chars <- vapply(codes, function(i) rawToChar(as.raw(i)), character(1))
  stringi::stri_replace_all_fixed(
    userinfo, chars, sprintf("%%%02X", codes),
    vectorize_all = FALSE
  )
}

# The WHATWG IPv4 canonicalization that used to run here is gone
# (RURL-ezhzpkhg deletion 4, the last of the five). It rewrote a WHATWG-valid
# IPv4 host inside the URL STRING before the parse, because the old engine
# refused WPT-valid forms such as `0x.0x.0`; the in-tree parser owns the address
# grammar directly, as `host_ipv4` (R/parse-web.R). Its companion
# `.parse_whatwg_ipv4_host()` / `.parse_whatwg_ipv4_number()` went with it,
# reconciled into `.web_ipv4_normalize(host, ipv4)` -- one function with a flag,
# so the three forms the two flavours disagree about are stated once instead of
# having to be rediscovered by diffing two near-identical normalizers.
#
# `.host_ends_in_number_vec()` STAYS: it is also the WHATWG host model's trigger
# in `.apply_host_standard_model_vec()`, which is where "a host that ends in a
# number must parse as an address" belongs.

# Decode percent-triplets in a WHATWG host just far enough for the host model to
# see the real code points after the parse has resolved the URL's structure. A
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

# Uppercase the hex digits of every percent-triplet, leaving all other
# characters alone (RFC 3986 section 6.2.2.1). Unlike
# `.rfc_unreserved_normalize` this does NOT decode unreserved triplets -- it is
# the case rule only, for callers that must not change which octets stay
# encoded.
.pct_hex_upper <- function(x) {
  .gsub_decodable("%([0-9a-f]{2})", "%\\U\\1", x, perl = TRUE)
}

# RFC 3986 scheme + path-rootless support for special schemes without `//`
# (RURL-pwsacxvo). In RFC 3986 section 3, an authority is present only when the
# scheme-specific part starts with a literal `//`; otherwise `http:example.com`
# is `scheme = "http"`, no authority, `path = "example.com"`. The web parser
# rejects
# these as malformed HTTP URLs, so selector mode records the components here and
# Stage A installs them directly instead of going through the web parser.
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
  scheme_lower <- .ascii_tolower(scheme)
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

# The WHATWG `file:` host, which is NEVER null (RURL-uhwivndf). WHATWG's "file
# state" sets url's host to the EMPTY STRING before any authority is read, and
# nothing in the file host state can put it back to null: `localhost` is
# explicitly mapped to the empty string, not dropped. So this returns "" -- an
# empty, PRESENT host -- where it used to return NA, and NA is now reserved for
# "this row did not parse". That distinction is what lets the serializer emit
# the `//` the standard requires for every `file:` URL.
.whatwg_file_normalize_host <- function(host) {
  if (is.na(host) || host == "") {
    return("")
  }
  if (stringi::stri_startswith_fixed(host, "[")) {
    return(host)
  }

  # `utils::URLdecode()` rebuilds the string with `rawToChar()`, so the decoded
  # octets come back marked "unknown" (native) -- an INPUT-side gap the host
  # chokepoint in `.parse_urls_vec()` cannot cover, because the WHATWG `file:`
  # parser is in-tree and produces this host itself. Under `LC_ALL=C` those
  # bytes are then read as native characters, so `file://a%C2%ADb/p` (soft
  # hyphen) missed the UTS-46 mapping that `file://a<U+00AD>b/p` gets and the
  # row was rejected. `.mark_host_utf8()` DECLARES the decoded octets UTF-8
  # (`Encoding<-`; never `enc2utf8()`, which would transcode from the session
  # locale) so the forbidden-code-point gate and `host_normalize()` below see
  # the same host in every locale.
  decoded <- if (grepl("%[0-9A-Fa-f]{2}", host, perl = TRUE)) {
    .mark_host_utf8(tryCatch(utils::URLdecode(host), error = function(e) host))
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
  if (identical(.ascii_tolower(decoded), "localhost")) {
    return("")
  }
  decoded
}

.whatwg_file_drive_path <- function(path) {
  stringi::stri_replace_first_regex(path, "^/([A-Za-z])\\|(?=/|$)", "/$1:")
}

# WHATWG "path state" for a `file:` path rooted at a normalized Windows drive
# letter (RURL-msefniuz). The standard's `..` step calls "shorten a URL's
# path", which returns WITHOUT removing anything when "url's scheme is 'file',
# path's size is 1, and path[0] is a normalized Windows drive letter" -- so
# `file:///C:/../` is `file:///C:/`, and `file:///C:/a/../..` is `file:///C:/`.
# The dot-segment resolution every other row gets, Phase 3's
# `._remove_dot_segments_whatwg()` (`.normalize_path_vec()` below), is RFC 3986
# section 5.2.4's algorithm and knows no drive letter: it shortened past `C:`
# and produced `file:///`.
#
# Phase 3 receives the path but not the scheme, and `http://h/C:/..` MUST
# still shorten to `/` -- the drive-letter clause is `file:`-only -- so the
# rule lives here, in the one parser that only ever sees `file:` rows, and is
# scoped to the rows where section 5.2.4 gives the wrong answer: a path whose
# FIRST segment is a normalized drive letter and which carries a dot segment.
# Every other `file:` path still reaches Phase 3 with its dots intact, exactly
# as before. The resolver's `.whatwg_file_remove_dot_segments()` (R/resolve.R,
# the same "shorten" rule for a relative reference) does the walk; WHATWG's
# dot segments are ATOMS (`.`, `%2e`, case-insensitive; a double-dot is any two
# of them), and that routine reads only the literal spellings, so whole
# encoded dot segments are folded to literals first. Phase 3 then finds no dot
# segment in these rows and leaves them untouched.
.whatwg_file_shorten_drive_path <- function(path) {
  mask <- !is.na(path) &
    stringi::stri_detect_regex(path, "^/[A-Za-z]:(?=/|$)") &
    stringi::stri_detect_regex(path, "(?i)/(\\.|%2e){1,2}(/|$)")
  if (!any(mask)) {
    return(path)
  }
  p <- path[mask]
  p <- stringi::stri_replace_all_regex(p, "(?i)(?<=/)(\\.|%2e){2}(?=/|$)", "..")
  p <- stringi::stri_replace_all_regex(p, "(?i)(?<=/)%2e(?=/|$)", ".")
  path[mask] <- vapply(
    p, .whatwg_file_remove_dot_segments, character(1), USE.NAMES = FALSE
  )
  path
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

  # Empty, not NA: WHATWG's file state gives every `file:` URL a non-null host,
  # and the host-less forms (`file:`, `file:/p`, `file:C|/m/`) carry the empty
  # string. See .whatwg_file_normalize_host().
  host <- rep("", length(file_path))
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

    # WHATWG "file host state": a buffer that is a Windows drive letter -- an
    # ASCII alpha followed by `:` OR `|` -- is not a host at all; the state
    # falls through to "path state" with the buffer intact, and the host stays
    # the empty string the file state set. `file://C:/` is `file:///C:/`
    # exactly as `file://C|/` is (RURL-ufsltsit: the `:` spelling was missed,
    # so `file://C:/` was rejected outright).
    drive_authority <- stringi::stri_detect_regex(authority, "^[A-Za-z][:|]$")
    drive_authority[is.na(drive_authority)] <- FALSE
    if (any(drive_authority)) {
      # The buffer IS the first path segment, so a slash-less `file://d:` has
      # the path `/d:` -- the "/" a real host would get from the path start
      # state is not appended, because the path state consumed the buffer.
      drive_tail <- ifelse(
        is.na(slash[drive_authority]), "", auth_path[drive_authority]
      )
      auth_path[drive_authority] <- paste0(
        "/",
        stringi::stri_sub(authority[drive_authority], 1L, 1L),
        ":",
        drive_tail
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
  path <- .whatwg_file_shorten_drive_path(path)
  # A backslash-introduced empty file authority serializes with a double-slash
  # path. `file:` and `file://` proper keep their ordinary single slash.
  empty_backslash_authority <- has_authority & !nzchar(host) &
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

# Bounded browser string fixer (RURL-jynceqrj, ADR 0012 Layer 6a; PRD Part 1).
# A deterministic single pass, gated on fixup_posture == "browser", that repairs
# the raw input string before any parsing. A byte-for-byte no-op for every other
# posture (returns `url` untouched), so the default parse path is unchanged.
#
# Steps (each feeds the next):
#   1. Outer C0/space trim -- strip leading/trailing U+0000..U+0020 from the
#      whole input (WHATWG "C0 control or space"). This trim has a whatwg-gated
#      sibling: WHATWG step 1's first half, in
#      .strip_whatwg_control_chars_vec() (RURL-yvxpanix), does the same trim for
#      every posture when url_standard == "whatwg". This one stays load-bearing
#      because it is the ONLY outer trim under url_standard = "rfc3986" / no
#      selector, where step 1 does not run at all.
#   2. `;`->`:` -- rewrite a leading `scheme;` to `scheme:` ONLY when the scheme
#      token is in the recognized-scheme set. A `;` after any other token is
#      left verbatim.
#   3. `://` insertion -- insert `://` after `scheme:` for a scheme in the
#      authority table that is missing its authority slashes (e.g.
#      `http:example.com` -> `http://example.com`). Outside the set: verbatim.
#
# Both tables resolve to the SAME set = .WHATWG_SPECIAL_SCHEMES (referenced
# directly, not copied). `ftps` is naturally excluded (it is not special under
# WHATWG), so `ftps;host`/`ftps:host` stay verbatim -- no special-casing.
# Step 4 (fallback `http`) is NOT here: it is the existing scheme_policy =
# "infer" prepend seam (ADR 0012 D4).
.apply_browser_fixup_vec <- function(url, fixup_posture) {
  if (!identical(fixup_posture, "browser")) {
    return(url)
  }

  # Step 1: outer C0/space trim (U+0000..U+0020 at either end). NA passes
  # through stringi unchanged.
  url <- stringi::stri_replace_first_regex(url, "^[\\x00-\\x20]+", "")
  url <- stringi::stri_replace_first_regex(url, "[\\x00-\\x20]+$", "")

  # Recognized-scheme set = authority table = .WHATWG_SPECIAL_SCHEMES. Schemes
  # are case-insensitive; the ordered `;`/`:` delimiter after the alternation
  # keeps a longer scheme (https) from being truncated to a shorter prefix
  # (http) and keeps non-scheme tokens (httpx;) from matching.
  scheme_alt <- paste(.WHATWG_SPECIAL_SCHEMES, collapse = "|")

  # Step 2: leading `scheme;` -> `scheme:`, recognized schemes only. $1 keeps
  # the matched scheme's original case.
  url <- stringi::stri_replace_first_regex(
    url, paste0("(?i)^(", scheme_alt, ");"), "$1:"
  )

  # Step 3: `scheme:` with no authority slashes -> `scheme://`, authority-table
  # schemes only. The negative lookahead `(?!//)` leaves an already-slashed
  # `scheme://` (incl. the step-2 output) untouched.
  url <- stringi::stri_replace_first_regex(
    url, paste0("(?i)^(", scheme_alt, "):(?!//)"), "$1://"
  )

  url
}

# Phase 1 (vector): scheme detection, supported-scheme policy, the host-shape
# gate, and building the string handed to the parser. Returns the per-URL
# columns plus
# a logical `rejected` column marking rows the scalar pipeline returned NULL for
# (scheme-relative under "error" handling; a bare unsupported scheme under
# keep/none; a scheme-less input that is not host-shaped (D1); or an IP attempt
# that is not a canonical literal (D2/D3)) and a `scheme_less_userinfo` flag
# (D5). An input's supported scheme is decided against .SUPPORTED_SCHEMES.
.prepare_urls_for_parse_vec <- function(url,
                                       protocol_handling,
                                       scheme_relative_handling,
                                       url_standard = NULL,
                                       scheme_policy = "infer",
                                       scheme_acceptance = "web",
                                       fixup_posture = "none") {
  n <- length(url)

  # Bounded browser fixer (RURL-jynceqrj, ADR 0012 Layer 6a) runs BEFORE the
  # standards-required WHATWG preprocessing below. It is a deterministic single
  # pass that repairs the input STRING (outer C0/space trim, `;`->`:` and `://`
  # insertion for recognized special schemes) so the fixed string flows through
  # the ordinary parse path. A byte-for-byte no-op unless fixup_posture ==
  # "browser". The step-4 fallback `http` prepend is NOT here: it is the
  # existing scheme_policy = "infer" seam at the add_http mask below, shared
  # with the default posture (ADR 0012 D4 -- one prepend impl).
  url <- .apply_browser_fixup_vec(url, fixup_posture)

  # WHATWG input stripping (RURL-tyetpjym, RURL-yvxpanix) runs FIRST -- it is
  # the WHATWG parser's step 1 (trim leading/trailing C0-or-space, then remove
  # all ASCII tab/LF/CR), so every scheme/host
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
  # (and IDN names normalize their separators) instead of reaching the parser
  # as an
  # un-splittable literal. A no-op unless url_standard == "whatwg".
  sep <- .map_whatwg_domain_separators_vec(url, url_standard)
  url <- sep$url

  url_lower <- .ascii_tolower(url)
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
    # The authority-part must be COLON-FREE. This is the same regex as
    # `.general_parsed_mask()`'s (R/parse-state.R), and the two must stay in
    # step: the carve-out exists for the scheme-LESS `example.com:8080` form,
    # which the scheme regex also matches (a dot is a legal scheme char). With
    # `[^/]+` the class ran greedily across colons, so `urn:ietf:rfc:2648` read
    # as "authority `urn:ietf:rfc`, port 2648", was flagged host:port, and was
    # diverted from the opaque parser to the web route that rejects `urn:`. Any
    # opaque payload ending in `:<digits>` was unparseable -- `urn:a:1`,
    # `sc:x:80`, `urn:isbn:0451450523` -- and only a trailing `?`/`#` rescued
    # it, by breaking the `($|/)` anchor. RURL-jnvtttfm repaired the sibling
    # site only; this is the second one (RURL-uafjkaas).
    lhp <- stringi::stri_detect_regex(url, "^[^/:]+:[0-9]+($|/)")
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
  # reg-name, WHATWG coerces it (and only the post-parse model, not this gate,
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

  # The string the uniform RFC 3986 gate judges (RURL-qrfrvmkg). Snapshotted
  # HERE, between the two kinds of rewrite this function performs:
  #   * BEFORE it, scheme inference (`add_http`) and scheme-relative expansion
  #     have run. Those are rurl's INPUT-ACCEPTANCE affordances, governed by
  #     `scheme_policy` / `scheme_relative_handling` (ADR 0010), not by
  #     `url_standard` -- so the gate must judge the input as accepted, or
  #     every scheme-less row would fail RFC 3986's mandatory `scheme ":"`
  #     and the grammar gate would silently re-implement scheme_policy.
  #   * AFTER it comes the one PARSER-COMPAT rewrite still standing (WHATWG
  #     userinfo charset acceptance). It exists to get a string past the web
  #     parser; judging its OUTPUT would
  #     let a
  #     repair launder an input the RFC has no production for -- exactly the
  #     "gated where rurl owns the parser" asymmetry this gate removes. The
  #     general/`file:` routes gate the source string, and this keeps the
  #     web route's subject identical to theirs.
  rfc_gate_input <- url_to_parse

  # WHATWG userinfo charset acceptance (RURL-micalqvh): rewrite the SPACE / C0 /
  # DEL bytes the parser refuses into the percent-encoded spelling WHATWG
  # stores. The repeated-"@" recovery that used to run here is now parser
  # behaviour (RURL-ezhzpkhg deletion 3).
  at <- .encode_userinfo_charset_vec(url_to_parse, url_standard)
  url_to_parse <- at$url

  # `.encode_userinfo_charset_vec()` is now the ONLY rewrite left after the gate
  # snapshot, and it is a survivor rather than a leftover: it writes the
  # spelling WHATWG STORES for a userinfo byte, which the parser cannot infer
  # because `rfc3986` must stay source-preserving. The other four are gone --
  # the host-charset shim and the host percent-decode order (deletions 1 and 2,
  # ADR 0013 superseding ADR 0009), the excess-"@" authority split (3), the pqf
  # fallback (5) and the WHATWG IPv4 canonicalization (4). Each was a parser
  # property being decided in front of the parser, and each was gated by a
  # regex over the whole URL that narrowed the set it claimed to cover.

  list(
    url_to_parse = url_to_parse,
    # Subject of the uniform RFC 3986 gate (RURL-qrfrvmkg); see above.
    rfc_gate_input = rfc_gate_input,
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
    # WHATWG leading/trailing strip (RURL-yvxpanix, step 1's first half): TRUE
    # where a leading/trailing C0-control-or-space run was removed, consumed by
    # the same seam to emit `leading-trailing-stripped`.
    leading_trailing_stripped = cc$leading_trailing_stripped,
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
# be rejected. Delegates to .prepare_urls_for_parse_vec().
.prepare_url_for_parse <- function(url,
                                  protocol_handling,
                                  scheme_relative_handling) {
  cols <- .prepare_urls_for_parse_vec(
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


# Uppercase the two hex digits of every %XX percent-triplet, leaving the rest of
# the string untouched (`%2f` -> `%2F`). This keeps the historical no-selector
# `path_encoding = "keep"` behavior and the RFC 3986 section 6.2.2.1 case
# canonicalization path. Malformed `%` (not followed by two hex digits) is left
# as-is. `\U\1` (perl) uppercases just the captured pair.
#
# The `\U` case mapping is locale-proof here (the captured pair is hex by
# construction, and no locale case-maps `[0-9a-f]` outside `[0-9A-F]`), but the
# ENCODING MARK of `gsub(perl = TRUE)` is not: handed native (`"unknown"`)
# bytes it returns them still `"unknown"` under `LC_ALL=C` and marked `"UTF-8"`
# under a UTF-8 session -- identical bytes, divergent mark, on every path/query
# value that carries a non-ASCII byte. `.mark_host_utf8()` pins the mark the
# same way the input and output chokepoints do: it DECLARES with `Encoding<-`
# and never transcodes (`enc2utf8()` would re-decode in the session locale,
# which is the locale sensitivity being removed). Pure-ASCII values are
# unaffected -- R ignores a mark on them -- so this only touches the rows that
# diverged.
.uppercase_percent_hex <- function(x) {
  na <- is.na(x)
  if (all(na)) {
    return(x)
  }
  x[!na] <- .mark_host_utf8(
    gsub("%([0-9a-fA-F]{2})", "%\\U\\1", x[!na], perl = TRUE)
  )
  x
}

# Recover the raw request path from the prepared URL string (the exact bytes
# the parser was handed), rather than `parsed_web$path`. Its `$path` applies
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
# the parser may have promoted the following segment into `$host`; in that shape
# the path from the prepared string is no longer the fetched path, so keep its
# coherent `$path` instead of duplicating the promoted host into the path. When
# the authority is followed directly by `?`, `#`, or end-of-string there is no
# path, so fall back to the parser's `$path` (the canonical "/" trailing-slash
# expects).
.extract_raw_path_vec <- function(prepared, engine_path,
                                  pqf_bytes = "reject") {
  out <- engine_path
  ok <- !is.na(prepared) & !is.na(engine_path)
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
  raw <- engine_path[ok]
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
  # This slice bypasses the parser's component pass by design (dot segments must
  # survive to `path_normalization`), so under `pqf_bytes = "encode"` it is also
  # the one place a C0/SP/DEL byte could reach a public surface UNESCAPED --
  # while `query` and `fragment`, which come straight off the parser, are
  # escaped. That asymmetry is not a spelling preference; it would put a literal
  # space in `clean_url()` and a raw control byte in `path`.
  if (identical(pqf_bytes, "encode")) {
    raw <- vapply(raw, .web_escape_pqf_bytes, character(1), USE.NAMES = FALSE)
  }
  out[ok] <- raw
  out
}

# Phase 2b: pull the raw components used downstream out of the parse result.
# The web parser never decodes, so `parsed_web$query` is already
# the raw (percent-encoded) query string, so it is taken verbatim; downstream
# parsers split on raw "&"/"=" then decode per-pair. scheme/host as-is. The path
# is re-derived from the prepared input by .extract_raw_path_vec() (see there)
# so dot segments survive to `path_normalization`.
.extract_raw_components <- function(parsed_web, prepared,
                                    pqf_bytes = "reject") {
  list(
    scheme = parsed_web$scheme %||% NA_character_,
    host = parsed_web$host %||% NA_character_,
    path = .extract_raw_path_vec(
      prepared, parsed_web$path %||% NA_character_, pqf_bytes
    ),
    # .blank_to_na(): present-but-empty query "" -> NA.
    query = .blank_to_na(parsed_web$query %||% NA_character_)
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
  # Presentation is terminal: identity normalization and the cleaning policies
  # below must inspect the pre-presentation path, where an encoded separator is
  # data rather than structure. WHATWG `encode` is deliberately different from
  # the legacy encoder: the standard's path serializer preserves existing
  # percent spellings, so it never full-decodes before encoding.
  whatwg_encode <- path_encoding == "encode" &&
    identical(path_identity, ".whatwg_preserve")

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

  # Terminal path presentation. Decode only after identity normalization and
  # cleaning have decided which structural rules fire. Legacy `encode` still
  # means decode then segment-encode; the difference is that both presentation
  # operations now happen after index/trailing-slash handling.
  if (path_encoding %in% c("decode", "encode") && !whatwg_encode) {
    mask <- !is.na(path_work)
    if (any(mask)) {
      decoded <- tryCatch(
        .pct_unescape(path_work[mask]),
        error = function(e) NULL
      )
      if (!is.character(decoded) || length(decoded) != sum(mask)) {
        decoded <- vapply(
          path_work[mask],
          function(p) tryCatch(.pct_unescape(p), error = function(e) p),
          character(1),
          USE.NAMES = FALSE
        )
      }
      path_work[mask] <- decoded
    }
  }

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
# labels (foo.0x4), trailing-dot forms (1.2.3.08.), and >4-part hosts the parser
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
# PRD §6.2). A no-op when url_standard is NULL (returns the parsed host / IP
# flag
# unchanged, never fatal), so the default pipeline is byte-for-byte unaffected
# (AC #1).
#
# The web parser already coerces numeric IPv4 forms itself (2130706433 ->
# 127.0.0.1,
# 192.168.010.1 -> 192.168.8.1) and keeps out-of-range / over-arity forms
# literal (256.1.1.1, 1.2.3.4.5). So `engine_host` is the coerced spelling and
# `input_host` (the original pre-parse token) is the un-coerced one. For an
# IPv4 attempt (`is_attempt`):
#   - rfc3986: parse faithfully as a reg-name -- restore the original token and
#     treat it as an IP only if the ORIGINAL was already a canonical quad.
#     Never fatal (every such token is a valid RFC 3986 reg-name).
#   - whatwg: adopt the parser's coercion; it is a valid WHATWG IPv4 exactly
#     when that output is a canonical dotted-quad. When it is not (out-of-range
#     or > 4 parts, which the parser leaves literal), the WHATWG IPv4 parser
#     rejects it,
#     so the row is fatal.
# Non-attempt hosts (ordinary names, IPv6, missing) pass through untouched.
# Returns updated `host`, `is_ip`, and a `fatal` mask the caller folds into the
# null-row set.
.apply_host_standard_model_vec <- function(input_host, engine_host,
                                           is_ip_engine, url_standard,
                                           is_attempt) {
  n <- length(engine_host)
  host <- engine_host
  is_ip <- is_ip_engine
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
    # host parse fails -- independent of whether the web parser coerced it.
    # This trigger (not is_ipv4ish) closes the gap where obfuscated/mixed forms
    # the web parser leaves as reg-names (foo.09, foo.0x4, 1.2.3.08.,
    # 0x1.2.3.4.5.)
    # bypassed the gate and were wrongly accepted with warning-invalid-tld.
    #
    # The trigger is read in the HOST PARSER's own order (RURL-lxdwuacn):
    # percent-decode, then domain-to-ASCII, and only THEN "ends in a number"
    # (#concept-host-parser steps 3-6). `engine_host` is already
    # percent-decoded under this profile (`host_pct`, R/parse-web.R), and the
    # UTS-46 mapping is applied here for the test only -- the recorded host
    # keeps its Unicode spelling, since `host_encoding` selects the rendering
    # later. Reading the trigger off the SOURCE token missed both spellings
    # WPT pins: `%30%78%63%30%2e%30%32%35%30.01` decodes to `0xc0.0250.01` and
    # the fullwidth `０Ｘｃ０．０２５０．０１` maps to it, and each is
    # `192.168.0.1`. The web parser itself never takes the IPv4 reading of a
    # token holding a "%", and that stays so: it reproduces the engine it was
    # calibrated against (ADR 0018); this is the standard's host MODEL, layered
    # over it, and the address grammar it applies is the one already in tree
    # (`.web_ipv4_normalize()`).
    reg_like <- !is.na(host) & host != "" &
      !stringi::stri_startswith_fixed(host, "[")
    reg_like[is.na(reg_like)] <- FALSE
    candidate <- host
    nonascii_all <- reg_like &
      stringi::stri_detect_regex(host, "[^\\u0001-\\u007f]")
    nonascii_all[is.na(nonascii_all)] <- FALSE
    mapped <- rep(NA_character_, n)
    if (any(nonascii_all)) {
      mapped[nonascii_all] <- punycoder::host_normalize(
        host[nonascii_all], check_hyphens = FALSE, use_std3 = FALSE,
        verify_dns_length = FALSE
      )
      use_mapped <- nonascii_all & !is.na(mapped)
      candidate[use_mapped] <- mapped[use_mapped]
    }
    att <- reg_like & .host_ends_in_number_vec(candidate)
    if (any(att)) {
      ipv4 <- vapply(
        candidate[att], .web_ipv4_normalize, character(1), ipv4 = "whatwg",
        USE.NAMES = FALSE
      )
      canonical <- .detect_ip_host_vec(ipv4)
      host[att] <- ipv4
      is_ip[att] <- canonical
      fatal[att] <- !canonical
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

      # `mapped` is the one domain-to-ASCII pass above, computed over every
      # non-ASCII reg-name; a row still non-ASCII here was not an address.
      nonascii <- reg & !bad_cp &
        stringi::stri_detect_regex(host, "[^\\u0001-\\u007f]")
      nonascii[is.na(nonascii)] <- FALSE
      fatal <- fatal | (nonascii & is.na(mapped))
    }

    # WHATWG IPv6 serializer (RURL-thjmzaam): bracketed IPv6 literals serialize
    # as eight 16-bit pieces with zero compression; an embedded dotted-quad IPv4
    # tail never remains dotted.
    host <- .serialize_whatwg_ipv6_hosts_vec(host)
  }

  list(host = host, is_ip = is_ip, fatal = fatal)
}

# Phase 6 (vector): apply the www-prefix policy to (non-IP) hosts.
.apply_www_policy_vec <- function(raw_host, www_handling, is_ip_host,
                                  engine = NULL) {
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
    lower <- .ascii_tolower(raw_host)
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
    final_host[elig] <- .apply_www_if_no_subdomain_vec(raw_host[elig], engine)
  }
  final_host
}

# Phase 6 (www_handling = "if_no_subdomain", vector): add a leading "www." only
# when the host is itself an apex (registrable domain with no subdomain labels).
# The STRUCTURAL decision uses pslr's canonical decomposition (batched over the
# eligible hosts) so an A-label host and its Unicode equivalent take the same
# branch; the emitted host keeps the input spelling (candidate_host).
.apply_www_if_no_subdomain_vec <- function(raw_host, engine = NULL) {
  ci <- stringi::stri_opts_regex(case_insensitive = TRUE)
  lower <- .ascii_tolower(raw_host)

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
  cand_lower <- .ascii_tolower(candidate_host)
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

  decomp <- .psl_suffix_extract(host_for_domain_check, "all", engine)
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
.apply_www_policy <- function(raw_host, www_handling, is_ip_host,
                              engine = NULL) {
  .apply_www_policy_vec(raw_host, www_handling, is_ip_host, engine)
}

# Phase 7 (vector, RURL-jhsbzmsj): the PSL ANNOTATION candidate for a host.
#
# `rfc3986` keeps a reg-name's percent-encoding in the host IDENTITY (host_pct =
# "keep"), because the RFC profile has to be source-preserving. The PSL cannot
# read a label containing "%", so a percent-encoded IDN host would carry no
# domain/TLD at all. RFC 3986 §3.2.2 explicitly admits percent-encoded UTF-8
# non-ASCII names in reg-name and requires IDNA transformation before a DNS
# lookup, while §6.2.2.2 authorizes only unreserved decoding for URI
# normalization -- so the decoded view is a legitimate basis for a DNS-facing
# ANNOTATION even though it must never become the identity.
#
# Hence: decode EXACTLY ONCE as UTF-8, solely to build the candidate. This must
# not affect acceptance, `final_host`, serialization, or identity -- it is
# reachable only from this function, which returns domain/TLD and nothing else.
#
# "Exactly once" is well defined here because no ACCEPTED host can carry a
# literal "%": an input spelling one (`%25`) is rejected under every profile, so
# a "%" in `final_host` is always an undecoded triplet. Verified across the
# whole octet sweep, not assumed.
#
# A failed decode (a malformed triplet, a decoded NUL) or a decode that is not
# valid UTF-8 yields NA -- the `unknown` annotation. `validUTF8()` is the
# validator on purpose: it is byte-based and locale-independent (the
# RURL-kmpnbvdl defect class), and strict enough to reject overlong forms and
# encoded surrogates, which are not legitimate spellings of a code point.
#
# Everything after the decode is the EXISTING pslr path, deliberately: the
# annotation for an `rfc3986` host then agrees with the one `whatwg` computes
# for the same decoded host, including its rejections (U+FFFD stays unknown, a
# soft hyphen is mapped away by IDNA).
.psl_annotation_host_vec <- function(hosts) {
  candidate <- hosts
  pct <- stringi::stri_detect_fixed(hosts, "%")
  pct[is.na(pct)] <- FALSE
  if (!any(pct)) {
    return(candidate)
  }
  candidate[pct] <- vapply(hosts[pct], function(one) {
    decoded <- .web_host_percent_decode(one)
    if (is.null(decoded) || !validUTF8(decoded)) {
      return(NA_character_)
    }
    Encoding(decoded) <- "UTF-8"
    decoded
  }, character(1), USE.NAMES = FALSE)
  candidate
}

# Phase 7 (vector): derive the registered domain and TLD from each host using
# the Public Suffix List. This is the hot path: pslr is queried ONCE per output
# spelling over the UNIQUE non-IP hosts (host-level de-dup; many URLs share a
# host) rather than once per URL. `host_encoding` selects the emitted spelling,
# mirroring get_host(): "unicode" decodes IDNs, "idna" emits ASCII A-labels, and
# "keep" follows each input host's own spelling (ASCII if it is an A-label).
#
# The PSL is queried on the ANNOTATION candidate (above), never on the identity
# host -- the two differ only for a percent-encoded `rfc3986` reg-name.
.derive_domain_tld_vec <- function(final_host, is_ip_host, tld_source,
                                   host_encoding = "keep", engine = NULL) {
  n <- length(final_host)
  domain <- rep(NA_character_, n)
  tld <- rep(NA_character_, n)
  annot_host <- .psl_annotation_host_vec(final_host)
  elig <- !is_ip_host & !is.na(annot_host) & annot_host != ""
  if (!any(elig)) {
    return(list(domain = domain, tld = tld))
  }

  hosts <- annot_host[elig]
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
      uniq_hosts[ascii_mask], tld_source, "ascii", engine
    )
    tld_uniq[ascii_mask] <- .psl_public_suffix(
      uniq_hosts[ascii_mask], tld_source, "ascii", engine
    )
  }
  uni_mask <- !ascii_mask
  if (any(uni_mask)) {
    dom_uniq[uni_mask] <- .psl_registered_domain(
      uniq_hosts[uni_mask], tld_source, "unicode", engine
    )
    tld_uniq[uni_mask] <- .psl_public_suffix(
      uniq_hosts[uni_mask], tld_source, "unicode", engine
    )
  }

  pos <- match(hosts, uniq_hosts)
  domain[elig] <- dom_uniq[pos]
  tld[elig] <- tld_uniq[pos]
  list(domain = domain, tld = tld)
}

# Phase 7 (scalar wrapper): delegates to .derive_domain_tld_vec().
.derive_domain_tld <- function(final_host, is_ip_host, tld_source,
                               host_encoding = "keep", engine = NULL) {
  .derive_domain_tld_vec(
    final_host, is_ip_host, tld_source, host_encoding, engine
  )
}

# Phase 8 (vector): keep only the requested number of subdomain levels. Only
# runs when subdomain_levels_to_keep is non-NULL. The STRUCTURAL decomposition
# is batched over the eligible hosts; the (variable-length) label reconstruction
# runs via vapply on the affected hosts only.
.apply_subdomain_policy_vec <- function(final_host, domain,
                                        subdomain_levels_to_keep, is_ip_host,
                                        engine = NULL) {
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
  lower <- .ascii_tolower(fh)
  has_www_prefix <- stringi::stri_startswith_fixed(lower, "www.")

  host_part <- fh
  if (any(has_www_prefix)) {
    host_part[has_www_prefix] <- stringi::stri_sub(fh[has_www_prefix], 5L)
  }

  decomp <- .psl_suffix_extract(host_part, "all", engine)
  derived_subdomain <- decomp$subdomain
  has_subdomain <- !is.na(derived_subdomain) & nzchar(derived_subdomain)

  # base strsplit keeps the documented trailing-empty behavior (see ADR 0005).
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
      registrable_labels <- .ascii_tolower(
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
                                    subdomain_levels_to_keep, is_ip_host,
                                    engine = NULL) {
  .apply_subdomain_policy_vec(
    final_host, domain, subdomain_levels_to_keep, is_ip_host, engine
  )
}

# Phase 9 (vector): re-encode (non-IP) hosts to IDNA/Punycode or Unicode on
# request. The Punycode round-trip semantics are owned by the DO-NOT-ALTER
# helpers .normalize_and_punycode_vec()/.punycode_to_unicode_vec() (domain.R);
# this phase only selects the eligible rows and applies the scalar fallback
# rule (keep the pre-encode host when the encode returns NA, or when the decode
# returns NA / ""). Under `whatwg` the default ("keep") presentation is NOT a
# no-op: it renders the UTS-46-mapped host (RUL-002; see
# .whatwg_host_presentation_vec()).
.apply_host_encoding_vec <- function(final_host, host_encoding, is_ip_host,
                                     url_standard = NULL) {
  host_for_clean <- final_host
  if (host_encoding == "keep" && !.is_whatwg(url_standard)) {
    return(host_for_clean)
  }
  elig <- !is.na(final_host) & final_host != "" & !is_ip_host
  if (!any(elig)) {
    return(host_for_clean)
  }

  subset <- final_host[elig]
  # Under `rfc3986` the host IDENTITY preserves its source spelling: section
  # 6.2.2.2 lets only unreserved triplets decode, so `a%C2%ADb` stays written
  # that way (`host_pct = "keep"`, R/parse-web.R). IDNA and Unicode are
  # PRESENTATIONS of that identity, and both need real code points -- UTS-46
  # has nothing to say about the characters "%", "C", "2". So decode here,
  # where the rendering is chosen, rather than in the parse, where it would
  # change what the host IS (the ADR 0011 separation).
  #
  # Scoped to `rfc3986` precisely because that is the only profile that leaves
  # a triplet standing; `whatwg` and the no-selector default have already
  # decoded the host once, and decoding again would strip a LEVEL of encoding
  # (`a%2560b` -> `a%60b` -> "a`b") rather than reveal one.
  if (identical(url_standard, "rfc3986")) {
    subset <- vapply(subset, .whatwg_percent_decode_host, character(1),
      USE.NAMES = FALSE
    )
  }
  if (.is_whatwg(url_standard)) {
    host_for_clean[elig] <- .whatwg_host_presentation_vec(subset, host_encoding)
  } else if (host_encoding == "idna") {
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

# WHATWG host presentation (RUL-002). The WHATWG host parser runs UTS-46
# "domain to ASCII" (Transitional_Processing false, CheckHyphens false,
# UseSTD3ASCIIRules false, VerifyDnsLength false) and the URL record stores that
# ASCII host; "domain to Unicode" is UTS-46 ToUnicode of it (UTS #46 section 4,
# 4.2, 4.3). So under `whatwg` every spelling is a rendering of ONE mapped form:
#   idna    -> ToASCII(host)
#   unicode -> ToUnicode(ToASCII(host))
#   keep    -> ToUnicode(ToASCII(host)) for a host written in Unicode, and
#              ToASCII(host) for a host already carrying an ACE (`xn--`) label,
#              mirroring how Stage B picks the `domain`/`tld` spelling for
#              "keep" (`host_is_ace`, R/parse.R) so host and domain agree.
# The mapped form is computed once via `punycoder::host_normalize()` -- the
# call the `idna` branch already made, never the ADR 0002 helpers, which stay
# the reversible (unmapped) renderers of the `rfc3986` and `NULL` arms. When
# domain-to-ASCII fails the pre-encode host is kept, as before.
.whatwg_host_presentation_vec <- function(subset, host_encoding) {
  mapped <- punycoder::host_normalize(
    subset, check_hyphens = FALSE, use_std3 = FALSE,
    verify_dns_length = FALSE
  )
  retry <- is.na(mapped)
  if (any(retry)) {
    mapped[retry] <- .normalize_and_punycode_vec(subset[retry])
  }
  keep_orig <- is.na(mapped)
  mapped[keep_orig] <- subset[keep_orig]
  if (host_encoding == "idna") {
    return(mapped)
  }
  # Only an ACE label decodes to anything other than itself; skip the
  # Punycode pass (and its cache) for the all-ASCII majority.
  decoded <- mapped
  ace_mapped <- .host_is_ace_vec(mapped)
  if (any(ace_mapped)) {
    dec <- .punycode_to_unicode_vec(mapped[ace_mapped])
    keep_mapped <- is.na(dec) | dec == ""
    dec[keep_mapped] <- mapped[ace_mapped][keep_mapped]
    decoded[ace_mapped] <- dec
  }
  if (host_encoding == "unicode") {
    return(decoded)
  }
  ace_source <- .host_is_ace_vec(subset)
  decoded[ace_source] <- mapped[ace_source]
  decoded
}

# Phase 9 (scalar wrapper): delegates to .apply_host_encoding_vec().
.apply_host_encoding <- function(final_host, host_encoding, is_ip_host,
                                 url_standard = NULL) {
  .apply_host_encoding_vec(final_host, host_encoding, is_ip_host, url_standard)
}

# Phase 10 (vector): apply the case policy to host, path, and scheme.
.apply_case_policy_vec <- function(host_output, path_output, scheme_output,
                                   case_handling) {
  # The HOST branch splits by DIRECTION (RURL-ugfpuotu).
  #
  # LOWERING a host is protocol syntax: RFC 3986 section 6.2.2.1 and the WHATWG
  # URL Standard both normalize hosts to lowercase, and WHATWG spells it "ASCII
  # lowercase". So `lower` / `lower_host` use the ASCII-only helper, which takes
  # locale, ICU version, and Unicode version out of the path entirely -- a
  # Turkish/Azeri session can no longer map "I" -> "ı" and silently name a
  # DIFFERENT domain. That is the bug being fixed here, and it is a LOWERING
  # bug.
  #
  # UPPERCASING a host is not protocol syntax at all: no standard ever
  # uppercases a host. `case_handling = "upper"` is a user-facing presentation
  # transform rurl offers as a convenience, in the same category as the path
  # branch below -- so the "syntax => ASCII-only" rule does not reach it, and
  # full Unicode case mapping is kept (pinned to the same non-tailoring locale
  # as the path branch, so it is still session-invariant). ASCII-only
  # uppercasing would mangle a non-ASCII host into mixed case
  # ("bücher.example" -> "BüCHER.EXAMPLE"), which serves nobody.
  h_mask <- !is.na(host_output) & host_output != ""
  if (any(h_mask)) {
    if (case_handling == "lower" || case_handling == "lower_host") {
      # Case folding applies to the reg-name, not to the hex digits of a
      # surviving percent-triplet, which RFC 3986 section 6.2.2.1 renders
      # uppercase (RURL-savatsuc).
      host_output[h_mask] <- .pct_hex_upper(
        .ascii_tolower(host_output[h_mask])
      )
    } else if (case_handling == "upper") {
      host_output[h_mask] <- stringi::stri_trans_toupper(
        host_output[h_mask],
        locale = .ASCII_SAFE_ICU_LOCALE
      )
    }
  }

  # The PATH is the one component where `case_handling` is a USER-REQUESTED
  # transformation of free text that may legitimately be non-ASCII, so full
  # Unicode case mapping is kept here (unlike host LOWERING above and the scheme
  # branch below, which are protocol SYNTAX and use ASCII-only mapping;
  # RURL-ugfpuotu).
  # The locale is pinned explicitly so the result does not vary with the R
  # session's locale; see .ASCII_SAFE_ICU_LOCALE in R/utils.R for why "root" and
  # "und" do NOT work here.
  p_mask <- !is.na(path_output)
  if (any(p_mask)) {
    if (case_handling == "lower") {
      path_output[p_mask] <- stringi::stri_trans_tolower(
        path_output[p_mask],
        locale = .ASCII_SAFE_ICU_LOCALE
      )
    } else if (case_handling == "upper") {
      path_output[p_mask] <- stringi::stri_trans_toupper(
        path_output[p_mask],
        locale = .ASCII_SAFE_ICU_LOCALE
      )
    }
  }

  # The SCHEME stays ASCII-only in BOTH directions: a scheme is ASCII by grammar
  # (RFC 3986 `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )`), so there is no
  # non-ASCII scheme for ASCII-only mapping to mangle, and both directions are
  # syntax.
  s_mask <- !is.na(scheme_output)
  if (any(s_mask)) {
    if (case_handling == "lower" || case_handling == "lower_host") {
      scheme_output[s_mask] <-
        .ascii_tolower(scheme_output[s_mask])
    } else if (case_handling == "upper") {
      scheme_output[s_mask] <-
        .ascii_toupper(scheme_output[s_mask])
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
  unname(.SCHEME_DEFAULT_PORTS[.ascii_tolower(scheme)])
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
  scheme_lc <- .ascii_tolower(scheme)
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
  scheme_lc <- .ascii_tolower(scheme_output)
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
  # RUL-005 (ADR 0017 D1): the strip must not leave an output whose authority
  # is only dots. "http://./" would otherwise clean to "http://." -- a string
  # that is structurally valid but not display-oriented, so D2 row 5 does not
  # apply to a dots-only host. The separator is kept for `.`, `..`, `...` and
  # any run of dots; root-dot FQDNs ("a.", "example.com.") strip as before,
  # and a dots-only host with a non-empty path ("http://./x/") still strips.
  dots_only_host <- has_host & grepl("^\\.+$", host_output)
  if (trailing_slash_handling == "strip") {
    path_part[path_part == "/" & !dots_only_host] <- ""
  }
  host_part <- ifelse(has_host, host_output, "")

  # A hostless row renders as `scheme://` + path, so a non-empty path that does
  # not begin with "/" lands in the AUTHORITY position and fabricates a host:
  # "file:C:/W" measured as clean_url "file://C:/W" (authority "C:" -- an SMB
  # fetch on Windows), "file:etc/passwd" as "file://etc/passwd", "file:." as
  # "file://.". Only `file:` reaches here hostless (`buildable` above), and
  # such row is `parse_status = "error"`, so there is no canonical spelling to
  # emit: rurl does not fabricate one (RURL-hnddjptl). Same defect class the
  # WHATWG serializer already guards with `/.` for an absent host and a
  # "//"-leading path (see .serialize_whatwg_vec below) -- but that guard
  # rewrites a path it can still spell; an error row has no spelling at all.
  fabricates_authority <- !has_host & nzchar(path_part) &
    !startsWith(path_part, "/")
  buildable <- buildable & !fabricates_authority
  if (!any(buildable)) {
    return(clean_url)
  }

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

# Does the WHATWG serializer emit the `//` authority introducer for this row?
#
# The standard names ONE condition -- "if url's host is non-null, append //"
# (#concept-url-serializer step 2) -- so this is `host_kind != "absent"` and
# nothing else. The `authority_delimiter_present` syntactic fact is NOT
# consulted: it is a property of the SOURCE string, and WHATWG serializes the
# parsed URL, not the source.
#
# THIS REPLACES P1.2 D-C, which keyed the introducer off
# `authority_delimiter_present` because `host_kind` "cannot tell a
# delimiter-present empty authority from a delimiter-absent input". That
# premise was itself the defect (RURL-uhwivndf): the only rows where the two
# collapsed were `file:` rows whose host the parser recorded as NULL where
# WHATWG gives every special scheme a non-null -- here empty -- host. With the
# `file:` host model repaired in `.parse_whatwg_file_urls_vec()`, host_kind
# distinguishes them, and the two conditions agree on every WPT success row
# EXCEPT the ten this fixes. `authority_delimiter_present` stays in the record
# and stays load-bearing for the RFC serializers, where the source spelling IS
# the fact being rendered.
.whatwg_authority_emitted <- function(host_kind) {
  !is.na(host_kind) & host_kind != "absent"
}

# WHATWG serializer (ADR 0012 A.1 #concept-url-serializer + D2). Emits the `//`
# authority introducer per `.whatwg_authority_emitted()` -- the standard's own
# condition, a NON-NULL host. The CONTENT of the authority (host, port) is
# driven by the respective component states, and the four-condition `/.` guard
# keys off a null HOST, which is what the WHATWG serializer's own condition
# names.
#
# Reuses the existing byte-level encoders
# (R/path-query.R): opaque paths take the C0-control set only
# (`.whatwg_component_percent_encode(path, integer(0))`), list paths take the
# path percent-encode set (`.whatwg_path_percent_encode`), and both already
# preserve existing `%xx` spellings. `port` may be NULL (no port) or a vector.
.serialize_whatwg_vec <- function(scheme, host, host_kind,
                                  authority_delimiter_present, path, path_kind,
                                  query, query_kind, port, port_handling,
                                  trailing_slash_handling) {
  # Part of the state the caller hands the serializer, but not consulted: the
  # `//` introducer follows the host, not the source spelling. force() marks it
  # deliberately consumed, as .serialize_rfc_generic_vec() does for host_kind.
  force(authority_delimiter_present)
  n <- max(
    length(scheme), length(host), length(host_kind),
    length(authority_delimiter_present), length(path),
    length(path_kind), length(query), length(query_kind)
  )
  scheme <- rep_len(scheme, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
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
    # It also mirrors the RUL-005 exception: a dots-only host ("foo://./",
    # host ".") keeps its separator, so the output never reads "foo://.".
    p_render <- p
    strip_slash <- identical(trailing_slash_handling, "strip")
    dots_only_host <- !is.na(host[i]) && grepl("^\\.+$", host[i])
    if (strip_slash && identical(p_render, "/") && !dots_only_host) {
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
    # always begins with "/"). On that rendering the two path conditions
    # collapse into ONE string test: the first segment is empty exactly when
    # the string begins with "//", and any such string carries at least two
    # "/" -- hence at least two segments -- so `size > 1` is implied and need
    # not be tested separately. "/bar" -> ["bar"]; "//bar" -> ["","bar"];
    # "//" -> ["",""]; "/" -> [""].
    #
    # Do NOT re-derive this by splitting on "/": `strsplit` drops a single
    # TRAILING "", so a path of exactly "//" (WHATWG list ["",""], size 2)
    # measures as size 1 and the guard misses -- emitting `non-spec://`, which
    # re-reads as an empty AUTHORITY rather than a path. That was a real defect
    # against WPT rows `non-spec:/.//`, `non-spec:/..//` and `non-spec:/a/..//`.
    if (host_kind[i] == "absent" && isTRUE(startsWith(p, "//"))) {
      guard[i] <- "/."
    }
  }

  query_suffix <- .whatwg_query_suffix_vec(query, query_kind, scheme)

  out <- character(n)
  out[is_opaque] <- paste0(scheme_prefix[is_opaque], path_body[is_opaque])

  # List path under a NON-NULL host: emit `//` + host (host may be "") + port.
  # `foo:///bar` = "foo://" + "" + "/bar".
  delim <- .whatwg_authority_emitted(host_kind)
  auth <- !is_opaque & delim
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", host_str[auth], port_part[auth], path_body[auth]
  )

  # List path with no delimiter: NO `//`; the `/.` guard, if it fired, sits
  # between the scheme and the path. `foo:/bar` -> "foo:/bar".
  noauth <- !is_opaque & !delim
  out[noauth] <- paste0(
    scheme_prefix[noauth], guard[noauth], path_body[noauth]
  )

  paste0(out, query_suffix)
}

# RFC 3986 generic serializer (ADR 0012 D1/D2, rfc-syntax posture). NO
# normalization, NO opaque/list distinction, NO `/.` guard, NO dot-segment
# removal, NO case folding: it is a faithful generic serialization that
# preserves the source path bytes. `//` is emitted iff
# `authority_delimiter_present` (P1.2 D-C), so `host_kind` -- like
# `rfc_path_form` -- is now informational under this posture: the path string
# is already in its final source-preserving shape and host presence no longer
# decides the authority introducer, so nothing structural keys off either here
# (force() marks them deliberately consumed, mirroring .build_port_part_vec's
# force(url_standard)). Both stay in the signature because they are part of the
# state the caller hands the serializer. The query is
# preserved verbatim (no percent-encoder) under rfc-syntax's "preserve source"
# disclaimer. `port` may be NULL or a vector.
.serialize_rfc_generic_vec <- function(scheme, host, host_kind,
                                       authority_delimiter_present, path,
                                       rfc_path_form, query, query_kind,
                                       port, port_handling) {
  force(rfc_path_form)
  force(host_kind)
  n <- max(
    length(scheme), length(host), length(host_kind),
    length(authority_delimiter_present), length(path),
    length(query), length(query_kind)
  )
  scheme <- rep_len(scheme, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
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
  auth <- !is.na(authority_delimiter_present) & authority_delimiter_present
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

# ---------------------------------------------------------------------------
# FSSS -- the full-string standard serializers (output surface (b)).
#
# These are the STANDARD serializers. The `.serialize_*_vec()` pair above are
# the CLEAN serializers (surface c): they implement rurl's `clean_url` product
# contract, which excludes the fragment and credentials by design. The two are
# separate surfaces, not versions of one thing (output-contracts.md, C-04).
#
# Three properties distinguish the FSSS, all contractual:
#
#   1. FULL-STRING. Credentials and the fragment are emitted, and a delimiter
#      that was present with an empty value survives as a trailing `?` / `#`
#      (ADR 0012 D2).
#   2. IDENTITY, NOT PRESENTATION. They take NO presentation dial -- no
#      `port_handling`, no `trailing_slash_handling`, no `path_encoding`. The
#      path is rendered by the selected standard's own percent-encode set and
#      `path_encoding` is never consulted (output-contracts.md, C-05; ADR 0011).
#      Passing one would be a category error, so there is no argument to pass.
#   3. LOSSLESS RECORD IN. They consume the lossless serializer-input record
#      (the R/parse-state.R vocabulary), never the 18-field public projection
#      and never a cleaned or formatted component (S3-F2).
#
# `port` is the identity port and is emitted whenever it is non-NA. The
# public projection's default-port nulling is a projection policy applied
# downstream; it is not this surface's business.

# WHATWG userinfo percent-encode set (#userinfo-percent-encode-set): the path
# set plus `/ : ; = @ [ \ ] ^ |`. `^` is already a path-set member.
.whatwg_userinfo_percent_encode <- function(x) {
  .whatwg_component_percent_encode(
    x,
    c(
      0x20L, 0x22L, 0x23L, 0x3CL, 0x3EL, 0x3FL, 0x5EL, 0x60L, 0x7BL, 0x7DL,
      0x2FL, 0x3AL, 0x3BL, 0x3DL, 0x40L, 0x5BL, 0x5CL, 0x5DL, 0x7CL
    )
  )
}

# WHATWG full-string serializer (#concept-url-serializer, ADR 0012 A.1 + D2).
#
# CREDENTIALS ARE SPEC-EXACT, WHICH MEANS LOSSY, AND THAT IS CORRECT HERE.
# WHATWG appends credentials iff the URL "includes credentials" -- username or
# password non-empty -- so a bare `http://@h/` serializes as `http://h/` and
# `http://u:@h/` as `http://u@h/`, both pinned by WPT. The undivided source
# userinfo is NOT used: this serializer emits the standard's own serialization
# of the parsed state, and WHATWG's state is the username/password split. The
# LOSSLESSNESS the contract requires lives in the RECORD (which keeps the
# undivided slice), not in this serializer's output; the RFC source-preserving
# form below is where the undivided spelling is rendered verbatim.
#
# Round-tripping is unaffected: the identity oracle is idempotence of
# parse -> serialize -- serialize(parse(serialize(parse(x)))) equals
# serialize(parse(x)) -- not recovery of the input bytes, which is surface (a).
.serialize_whatwg_full_vec <- function(scheme, userinfo, host, host_kind,
                                       authority_delimiter_present, path,
                                       path_kind, query, query_kind,
                                       fragment, fragment_kind, port) {
  # Not consulted; see .serialize_whatwg_vec(). The `//` introducer follows the
  # parsed host, which is what #concept-url-serializer names.
  force(authority_delimiter_present)
  n <- max(
    length(scheme), length(userinfo), length(host), length(host_kind),
    length(authority_delimiter_present), length(path), length(path_kind),
    length(query), length(query_kind), length(fragment), length(fragment_kind)
  )
  scheme <- rep_len(scheme, n)
  userinfo <- rep_len(userinfo, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
  path <- rep_len(path, n)
  path_kind <- rep_len(path_kind, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  fragment <- rep_len(fragment, n)
  fragment_kind <- rep_len(fragment_kind, n)
  port <- if (is.null(port)) rep(NA_character_, n) else rep_len(port, n)

  scheme_prefix <- paste0(scheme, ":")
  is_opaque <- path_kind == "opaque"

  # Identity port: emitted whenever present. No `port_handling`.
  port_part <- rep("", n)
  has_port <- !is.na(port)
  port_part[has_port] <- paste0(":", port[has_port])

  cred_part <- character(n)
  path_body <- character(n)
  guard <- rep("", n)
  for (i in seq_len(n)) {
    # Credentials: split the recorded userinfo at the FIRST ":" into the
    # WHATWG username/password pair, then apply #concept-url-serializer.
    ui <- userinfo[i]
    if (is.na(ui)) {
      cred_part[i] <- ""
    } else {
      colon <- regexpr(":", ui, fixed = TRUE)
      if (colon > 0L) {
        username <- substring(ui, 1L, colon - 1L)
        password <- substring(ui, colon + 1L)
      } else {
        username <- ui
        password <- ""
      }
      if (nzchar(username) || nzchar(password)) {
        out_cred <- .whatwg_userinfo_percent_encode(username)
        if (nzchar(password)) {
          out_cred <- paste0(
            out_cred, ":", .whatwg_userinfo_percent_encode(password)
          )
        }
        cred_part[i] <- paste0(out_cred, "@")
      } else {
        # "includes credentials" is false -- both halves empty. WHATWG drops
        # the whole userinfo, delimiter included.
        cred_part[i] <- ""
      }
    }

    p <- path[i]
    if (is.na(p)) {
      path_body[i] <- ""
      next
    }
    if (is_opaque[i]) {
      # An opaque path's trailing space is encoded only when a `?`/`#`
      # actually follows it. Unlike the clean serializer, the FSSS emits the
      # fragment too, so the fragment counts as a following delimiter here.
      delimiter_follows <- query_kind[i] != "absent" ||
        fragment_kind[i] != "absent"
      path_body[i] <- .whatwg_opaque_path_encode(p, delimiter_follows)
      next
    }
    path_body[i] <- .whatwg_path_percent_encode(p)

    # Four-condition `/.` guard -- identical to the clean serializer's, but
    # with no trailing-slash strip ahead of it, since that is a presentation
    # dial this surface does not take. See .serialize_whatwg_vec for the
    # segment-derivation reasoning, including why this must NOT be re-derived
    # by splitting on "/".
    if (host_kind[i] == "absent" && isTRUE(startsWith(p, "//"))) {
      guard[i] <- "/."
    }
  }

  out <- character(n)
  out[is_opaque] <- paste0(scheme_prefix[is_opaque], path_body[is_opaque])

  delim <- .whatwg_authority_emitted(host_kind)
  auth <- !is_opaque & delim
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", cred_part[auth], host_str[auth],
    port_part[auth], path_body[auth]
  )

  noauth <- !is_opaque & !delim
  out[noauth] <- paste0(
    scheme_prefix[noauth], guard[noauth], path_body[noauth]
  )

  paste0(
    out,
    .whatwg_query_suffix_vec(query, query_kind, scheme),
    .whatwg_fragment_suffix_vec(fragment, fragment_kind)
  )
}

# WHATWG fragment suffix (ADR 0012 D2), mirroring .whatwg_query_suffix_vec:
# present -> "#" + fragment-set encode; empty -> bare "#"; absent -> nothing.
# The empty case is the whole point -- `http://h/#` must not collapse to
# `http://h/`.
.whatwg_fragment_suffix_vec <- function(fragment, fragment_kind) {
  n <- length(fragment_kind)
  suffix <- rep("", n)
  for (i in seq_len(n)) {
    if (fragment_kind[i] == "empty") {
      suffix[i] <- "#"
    } else if (fragment_kind[i] == "present") {
      encoded <- .whatwg_fragment_percent_encode(fragment[i])
      if (is.na(encoded)) {
        encoded <- ""
      }
      suffix[i] <- paste0("#", encoded)
    }
  }
  suffix
}

# RFC 3986 full-string serializer (section 5.3 component recomposition), in
# BOTH postures the contract leaves open (OUT-O3):
#
#   form = "source"     -- rfc-syntax. No normalization of any kind: source
#                          bytes are preserved, the undivided userinfo slice is
#                          emitted verbatim (RFC 3986 has no credential concept
#                          to be spec-exact about, so nothing is dropped), and
#                          the query/fragment are appended without an encoder.
#                          This is the posture round-trip fidelity needs.
#   form = "normalized" -- section 6.2.2 syntax-based normalization plus the
#                          section 6.2.3 default-port removal: case
#                          normalization of scheme and host, percent-encoding
#                          normalization (triplets upper-cased, unreserved
#                          octets decoded), and path segment normalization.
#                          This is the posture a conformance claim needs.
#
# Both are exposed rather than one being chosen, because choosing forfeits
# either the round-trip oracle or the claim substrate.
.serialize_rfc_full_vec <- function(scheme, userinfo, host, host_kind,
                                    authority_delimiter_present, path,
                                    rfc_path_form, query, query_kind,
                                    fragment, fragment_kind, port,
                                    form = "source") {
  force(rfc_path_form)
  force(host_kind)
  n <- max(
    length(scheme), length(userinfo), length(host),
    length(authority_delimiter_present), length(path), length(query),
    length(query_kind), length(fragment), length(fragment_kind)
  )
  scheme <- rep_len(scheme, n)
  userinfo <- rep_len(userinfo, n)
  host <- rep_len(host, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
  path <- rep_len(path, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  fragment <- rep_len(fragment, n)
  fragment_kind <- rep_len(fragment_kind, n)
  port <- if (is.null(port)) rep(NA_character_, n) else rep_len(port, n)

  normalized <- identical(form, "normalized")

  if (normalized) {
    scheme <- .ascii_tolower(scheme)
    # The host takes all three rules, in the order the section states them:
    # 6.2.2.2 decodes the triplets encoding an unreserved octet, 6.2.2.1
    # case-folds the reg-name (including any octet that decoding just
    # revealed), and the hex digits of every triplet that survived are rendered
    # uppercase. The last step is separate because case folding lowercases the
    # triplet too, so the host was the one component whose triplets escaped the
    # hex normalization the path/query/fragment below already get
    # (RURL-savatsuc); the first is here rather than in the parse because
    # 6.2.2.2 is normalization, and applying it during the parse made the
    # decoding depend on the SCHEME instead of the requested form
    # (RURL-xkhbhaje).
    host <- .pct_hex_upper(.ascii_tolower(.rfc_pct_normalize(host)))
    path <- .rfc_pct_normalize(path)
    # A raw byte >= 0x80 in the query or fragment is percent-encoded HERE, not
    # on the parse record (RUL-015): section 2.1 makes the triplet a
    # representation of the octet, so the `source` form hands the octet back
    # as written and the `normalized` form renders it as the ASCII grammar
    # requires. Before RUL-015 the web-route record already carried the
    # encoding, so for that route this is byte-identical; the general route
    # (`foo://h/p?q=<C3><BC>`) never encoded, and now agrees with it. The path
    # is deliberately not touched: its raw byte has always been rendered raw
    # in both forms, and moving it is not this ruling's question.
    query <- .rfc_pct_normalize(.rfc_pct_encode_high(query))
    fragment <- .rfc_pct_normalize(.rfc_pct_encode_high(fragment))
    userinfo <- .rfc_pct_normalize(userinfo)
    # Path segment normalization (section 6.2.2.3) applies to a path that has
    # an authority or is absolute; a rootless path has no dot-segment meaning
    # to remove.
    seg_norm <- !is.na(path) & startsWith(path, "/")
    if (any(seg_norm)) {
      path[seg_norm] <- vapply(
        path[seg_norm], ._remove_dot_segments, character(1), USE.NAMES = FALSE
      )
    }
    # Section 6.2.3, verbatim: "a URI that uses the generic syntax for
    # authority with an empty path should be normalized to a path of '/'".
    # This is the ONLY home for that "/" -- the parse leaves `path-abempty`'s
    # empty match empty (RURL-epoinamh), so without this step the normalized
    # form would lose a normalization the RFC states.
    #
    # Keyed on the AUTHORITY delimiter rather than on the scheme, because that
    # is what the sentence is keyed on: it fires on a non-special scheme too,
    # and not at all on a URI with no authority, which has no `path-abempty`
    # and so no sentence to apply. `urn://:443` therefore normalizes to
    # `urn://:443/`, and that is deliberate -- ADR 0012 rules that
    # scheme-specific restrictions are overlays, not generic parse gates, so a
    # string being an invalid URN (RFC 8141) does not change its RFC 3986
    # generic-normalization spelling. Reporting that violation is the
    # RURL-eqrpggvz reader's job, not this dial's.
    empty_abempty <- !is.na(authority_delimiter_present) &
      authority_delimiter_present & (is.na(path) | !nzchar(path))
    path[empty_abempty] <- "/"
  }

  scheme_prefix <- paste0(scheme, ":")

  port_part <- rep("", n)
  has_port <- !is.na(port)
  if (normalized) {
    # Section 6.2.3: a port equal to the scheme's default is elided.
    default_port <- .scheme_default_port_vec(scheme)
    has_port <- has_port & !(!is.na(default_port) & port == default_port)
  }
  port_part[has_port] <- paste0(":", port[has_port])

  # RFC has no username/password split: the undivided slice is the component,
  # so every delimiter state (`u@`, `u:@`, `:p@`, `@`) survives verbatim.
  cred_part <- rep("", n)
  has_cred <- !is.na(userinfo)
  cred_part[has_cred] <- paste0(userinfo[has_cred], "@")

  path_body <- ifelse(is.na(path), "", path)

  out <- character(n)
  auth <- !is.na(authority_delimiter_present) & authority_delimiter_present
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", cred_part[auth], host_str[auth],
    port_part[auth], path_body[auth]
  )
  out[!auth] <- paste0(scheme_prefix[!auth], path_body[!auth])

  query_suffix <- rep("", n)
  is_present <- query_kind == "present"
  query_suffix[is_present] <- paste0("?", ifelse(
    is.na(query[is_present]), "", query[is_present]
  ))
  query_suffix[query_kind == "empty"] <- "?"

  fragment_suffix <- rep("", n)
  frag_present <- fragment_kind == "present"
  fragment_suffix[frag_present] <- paste0("#", ifelse(
    is.na(fragment[frag_present]), "", fragment[frag_present]
  ))
  fragment_suffix[fragment_kind == "empty"] <- "#"

  paste0(out, query_suffix, fragment_suffix)
}

# RFC 3986 section 6.2.2.1 + 6.2.2.2 percent-encoding normalization, vectorized
# over the scalar .rfc_unreserved_normalize() (R/path-query.R), which already
# does both halves: it decodes triplets encoding an unreserved octet and
# upper-cases the hex digits of every triplet it leaves encoded.
.rfc_pct_normalize <- function(x) {
  out <- x
  keep <- !is.na(x)
  if (!any(keep)) {
    return(x)
  }
  out[keep] <- vapply(
    x[keep], .rfc_unreserved_normalize, character(1), USE.NAMES = FALSE
  )
  out
}

# RFC 3986 section 2.1 rendering of a raw octet >= 0x80 as its uppercase
# triplet, vectorized over the scalar `.web_escape_high_bytes()`
# (R/parse-web.R). Every other byte -- an existing triplet included -- is left
# as written; the hex-case fold is `.rfc_pct_normalize()`'s job, applied after.
.rfc_pct_encode_high <- function(x) {
  out <- x
  keep <- !is.na(x)
  if (!any(keep)) {
    return(x)
  }
  out[keep] <- vapply(
    x[keep], .web_escape_high_bytes, character(1), USE.NAMES = FALSE
  )
  out
}

# Phase 12 (vector): classify the parse outcome (ok / ok-ftp / warning-* /
# error / ok-scheme-relative). `web_ok` is TRUE for rows the web parser read
# (the
# scalar wrapper passes !is.null(parsed_web)).
#
# The status is now DERIVED, not decided here: Phase 12 computes the three
# independent verdict layers (R/verdicts.R) and projects them through pi. That
# projection reproduces the historical cascade exactly -- including the two
# places the old code let a later fact overwrite an earlier one (an FTP scheme
# only reached `ok-ftp` when the PSL cascade had left the row `ok`; a
# scheme-relative row likewise) -- because pi considers the L3 annotation rows
# BEFORE the L2 accept sub-states. Keeping one status-deciding path is what
# makes `get_parse_verdicts()` incapable of drifting from `parse_status`.
#
# ADR 0012 D5 (RURL-qbnelzku): a general-routed opaque / non-special-authority
# row is `ok` and EXEMPT from the reg-name host_has_dot / tld / domain cascade
# -- an opaque or arbitrary-scheme host has no PSL domain, so it must NOT
# become warning-no-tld. Empty under "web" (no general row exists there), so
# this is a pure no-op for the default posture.
.derive_parse_status_vec <- function(web_ok, final_host, is_ip_host, tld,
                                     domain, protocol_handling, final_scheme,
                                     looks_like_protocol,
                                     original_has_allowed_scheme,
                                     looks_like_host_port,
                                     is_scheme_relative,
                                     scheme_relative_handling,
                                     rfc3986_path_rootless = NULL,
                                     scheme_acceptance = "web",
                                     is_general = NULL,
                                     scheme_less_userinfo = NULL) {
  .project_parse_status_vec(.derive_verdict_layers_vec(
    web_ok = web_ok,
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
    rfc3986_path_rootless = rfc3986_path_rootless,
    scheme_acceptance = scheme_acceptance,
    is_general = is_general,
    scheme_less_userinfo = scheme_less_userinfo
  ))
}

# Phase 12 (scalar wrapper): delegates to .derive_parse_status_vec().
.derive_parse_status <- function(parsed_web, final_host, is_ip_host, tld,
                                 domain, protocol_handling, final_scheme,
                                 looks_like_protocol,
                                 original_has_allowed_scheme,
                                 looks_like_host_port,
                                 is_scheme_relative,
                                 scheme_relative_handling,
                                 rfc3986_path_rootless = NULL,
                                 scheme_less_userinfo = NULL) {
  .derive_parse_status_vec(
    web_ok = !is.null(parsed_web),
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
    rfc3986_path_rootless = rfc3986_path_rootless,
    scheme_less_userinfo = scheme_less_userinfo
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
    # fragment/user/password are never percent-DECODED: with `decode = FALSE`
    # the web parser never decodes them, keeping them
    # consistent with the raw path/query. Under `url_standard = "whatwg"` the
    # caller has already re-encoded these three with their WHATWG percent-encode
    # sets (fragment / userinfo) before they reach this assembler; under
    # `rfc3986` or no selector they are the raw source spelling.
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

# Phase 13 (scalar wrapper): extracts port/fragment/user/password from the parse
# object, then delegates to .assemble_parse_result_vec().
.assemble_parse_result <- function(original_input_url, scheme_output,
                                   host_output, parsed_web, path_output,
                                   raw_query, domain, tld,
                                   domain_ascii, domain_unicode,
                                   tld_ascii, tld_unicode, is_ip_host,
                                   clean_url, parse_status, is_scheme_relative,
                                   scheme_relative_handling) {
  .assemble_parse_result_vec(
    original_url = original_input_url,
    scheme_output = scheme_output,
    host_output = host_output,
    port = suppressWarnings(as.integer(parsed_web$port %||% NA_integer_)),
    path_output = path_output,
    # .blank_to_na(): present-but-empty raw components "" -> NA (see utils.R).
    raw_query = .blank_to_na(raw_query %||% NA_character_),
    fragment = .blank_to_na(parsed_web$fragment %||% NA_character_),
    user = .blank_to_na(parsed_web$user %||% NA_character_),
    password = .blank_to_na(parsed_web$password %||% NA_character_),
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
