# Package-wide utilities and global-variable declarations.

# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# The URL schemes rurl supports. rurl's domain is authority-based (hierarchical)
# URLs. http/https/ftp/ftps carry "scheme://host[:port]/path"; file is the
# one supported hostless hierarchical scheme. In the WHATWG profile, `file:`
# has a small parser slice for drive-letter, host, and backslash state-machine
# forms; default/RFC behavior remains limited to libcurl-parseable local forms.
# ftps is FTP-over-TLS (the https-analogue for ftp), not the unrelated
# SSH-based sftp.
# This is the single source of truth: a scheme-bearing input whose scheme is not
# here is rejected (opaque schemes like mailto:/tel:/data:, and unrecognized
# schemes like ws:/ssh:/typos).
.SUPPORTED_SCHEMES <- c("http", "https", "ftp", "ftps", "file")

# The WHATWG "special scheme" subset of .SUPPORTED_SCHEMES (PRD v2 D7,
# RURL-jlvyjwog; file added by RURL-rutsdflg). WHATWG defines http/https/ftp/
# ws/wss/file as special; rurl still does not support ws/wss. ftp**s** is
# rurl's own addition (FTP-over-TLS) and is NOT a WHATWG special scheme.
.WHATWG_SPECIAL_SCHEMES <- c("http", "https", "ftp", "file")

# Special schemes whose WHATWG no-slash authority recovery is shared by this
# parser's authority-based URL model. `file` has a separate state machine and is
# intentionally left to the existing file:// slice.
.SPECIAL_AUTHORITY_SCHEMES <- c("http", "https", "ftp")

# WHATWG forbidden host/domain code points (RURL-jfuqpwvh) that can survive to a
# resolved reg-name host and must fail the host parse under url_standard =
# "whatwg". An ICU regex character class covering: C0 controls U+0001-U+001F and
# DEL U+007F, and the printable forbidden bytes space # % / : < > ? @ [ ] \ ^ |.
# NUL (U+0000) is excluded (an R string cannot hold it); tab/LF/CR are already
# stripped upstream (.strip_whatwg_control_chars_vec, RURL-tyetpjym). Hyphen and
# underscore are LEGAL and deliberately absent. Applied only to non-IP hosts
# (IPv6 literals legitimately contain "[" "]" ":"). See ADR 0004/0007.
.WHATWG_FORBIDDEN_HOST_CP <- "[\\u0001-\\u001f\\u007f #%/:<>?@\\[\\]\\\\^|]"

# WHATWG host-charset shim code points (RURL-dxwxeamq, ADR 0009). The 15 ASCII
# code points libcurl rejects in a host ("Bad hostname") that the WHATWG URL
# Standard keeps verbatim in the host (ada-confirmed): ! " $ & ' ( ) * + , ; = `
# { }. libcurl's host allowed-set is narrower than WHATWG's; without the shim
# the whole row is dropped. These are all NON-forbidden (none appears in
# .WHATWG_FORBIDDEN_HOST_CP) and NON-structural (none delimits userinfo/port/
# path/query/fragment), so the host span is locatable before substitution.
# U+0025 "%" is DELIBERATELY EXCLUDED -- it is a forbidden domain code point
# (WHATWG drops it too), so libcurl rejecting it is correct. An ICU regex class.
.WHATWG_HOST_CHARSET_SHIM_CP <- paste0(
  "[\\u0021\\u0022\\u0024\\u0026\\u0027\\u0028\\u0029\\u002a",
  "\\u002b\\u002c\\u003b\\u003d\\u0060\\u007b\\u007d]"
)

# RFC 3986 reg-name sub-delims (RURL-dnddogce): the subset of libcurl-rejected
# host bytes that RFC 3986 section 3.2.2 permits literally in a reg-name.
.RFC3986_REG_NAME_SUB_DELIM_CP <- paste0(
  "[\\u0021\\u0024\\u0026\\u0027\\u0028\\u0029\\u002a",
  "\\u002b\\u002c\\u003b\\u003d]"
)

# Default ports for rurl's WHATWG-special schemes (PRD v2 D1, RURL-qdlvldts).
# Only http/https/ftp have a WHATWG-defined default; ftps (rurl's own
# FTP-over-TLS addition, not a WHATWG special scheme per D2) has none, so a
# port on ftps never matches this table -- it is never elided under
# port_handling = "keep" and always registers as the non-default-port
# diagnostic fact, regardless of url_standard.
.SCHEME_DEFAULT_PORTS <- c(http = 80L, https = 443L, ftp = 21L)

# Bare single-label hosts (no dot) accepted from scheme-less input. Only
# `localhost` is a genuine resolvable single-label host; other RFC 6761/2606
# reserved names are dotted suffixes handled by the normal path (.onion/.arpa
# are in the PSL; .local/.test/.invalid/.example -> warning-invalid-tld).
.SPECIAL_SINGLE_LABEL_HOSTS <- "localhost"

# Convert a glob to an anchored regex where ONLY '*' is special (matches any
# run of characters). Every other character -- including '.', '?', '[' -- is
# escaped to a literal, so a query param named "a.b" or "a[0]" matches only
# itself and a literal '*' in a pattern is unsupported (documented). Used to
# compile the built-in denylist and user params_keep/params_drop into matchers.
# Implementation: escape every non-word char (so '*' becomes the two-char '\*'),
# then turn that escaped star back into the regex '.*', then anchor.
.glob_to_regex <- function(glob) {
  escaped <- stringi::stri_replace_all_regex(glob, "([^A-Za-z0-9_])", "\\\\$1")
  escaped <- gsub("\\*", ".*", escaped, fixed = TRUE)
  paste0("^", escaped, "$")
}

# Coerce present-but-empty ("") raw components to NA, vectorized. curl's
# `curl_parse_url()` is inconsistent across libcurl versions for a present-but-
# empty component (e.g. the query of "https://example.com/?"): older libcurl
# returns NULL (-> NA via %||%), newer returns "". This normalizes "" -> NA so
# rurl's raw query/fragment/userinfo output is deterministic across libcurl
# versions, matching the long-shipped "empty component == absent" behavior.
.blank_to_na <- function(x) {
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# Field spec for safe_parse_urls() result columns. Single source of truth that
# keeps the empty-frame template, the per-row error fallback, and the populated
# frame in sync with safe_parse_url()'s result fields. Each entry carries the
# column name, the default used when a field is absent, and a length-1 vapply
# type template (its zero-length form `template[0]` types the empty frame).
.spu_result_fields <- list(
  list(name = "original_url", default = NA_character_, template = character(1)),
  list(name = "scheme", default = NA_character_, template = character(1)),
  list(name = "host", default = NA_character_, template = character(1)),
  list(name = "port", default = NA_integer_, template = integer(1)),
  list(name = "path", default = NA_character_, template = character(1)),
  list(name = "query", default = NA_character_, template = character(1)),
  list(name = "fragment", default = NA_character_, template = character(1)),
  list(name = "user", default = NA_character_, template = character(1)),
  list(name = "password", default = NA_character_, template = character(1)),
  list(name = "domain", default = NA_character_, template = character(1)),
  list(name = "tld", default = NA_character_, template = character(1)),
  # Encoding-independent identity spellings (RURL-owrdsivt): the registrable
  # domain / public suffix in BOTH canonical spellings, regardless of
  # host_encoding. `domain`/`tld` above still follow host_encoding (a rendering
  # choice); these four are stable identity keys so a Unicode host and its
  # A-label share one domain_ascii (and one domain_unicode) without re-parsing.
  list(name = "domain_ascii", default = NA_character_, template = character(1)),
  list(
    name = "domain_unicode", default = NA_character_, template = character(1)
  ),
  list(name = "tld_ascii", default = NA_character_, template = character(1)),
  list(name = "tld_unicode", default = NA_character_, template = character(1)),
  list(name = "is_ip_host", default = NA, template = logical(1)),
  list(name = "clean_url", default = NA_character_, template = character(1)),
  list(name = "parse_status", default = "error", template = character(1))
)

# Field spec for the option-INDEPENDENT parse core (Stage A) that the full_parse
# cache stores (RURL-dkwrebdt). One entry per cached column, in the order the
# unnamed per-row cache value packs them, with the vapply type template used to
# gather cache hits (mirrors .spu_result_fields). Stage A holds the expensive,
# presentation-independent work -- curl components, IP detection, the post-www
# host, and the PSL decomposition in BOTH spellings (so host_encoding stays a
# Stage-B choice) -- keyed only by url x protocol x www x tld_source x
# scheme_relative. Stage B (._parse_stage_b_vec) derives every remaining column
# from these plus the presentation options and is never cached.
.spu_stage_a_fields <- list(
  list(name = "final_scheme", default = NA_character_, template = character(1)),
  list(name = "final_host", default = NA_character_, template = character(1)),
  list(name = "is_ip_host", default = NA, template = logical(1)),
  list(name = "raw_path", default = NA_character_, template = character(1)),
  list(name = "raw_query", default = NA_character_, template = character(1)),
  list(name = "raw_fragment", default = NA_character_, template = character(1)),
  list(name = "raw_user", default = NA_character_, template = character(1)),
  list(name = "raw_password", default = NA_character_, template = character(1)),
  list(name = "raw_port", default = NA_integer_, template = integer(1)),
  list(name = "domain_ascii", default = NA_character_, template = character(1)),
  list(
    name = "domain_unicode", default = NA_character_, template = character(1)
  ),
  list(name = "tld_ascii", default = NA_character_, template = character(1)),
  list(name = "tld_unicode", default = NA_character_, template = character(1)),
  list(name = "host_is_ace", default = FALSE, template = logical(1)),
  list(name = "looks_like_protocol", default = FALSE, template = logical(1)),
  list(
    name = "original_has_allowed_scheme", default = FALSE,
    template = logical(1)
  ),
  list(name = "is_scheme_relative", default = FALSE, template = logical(1)),
  # host:port input (RURL-aldwnots): matches the scheme regex but is a valid
  # host:port form, so it must be excluded from the unsupported-scheme demotion
  # in parse-status derivation. Cached with the rest of Stage A.
  list(name = "looks_like_host_port", default = FALSE, template = logical(1)),
  # Scheme-less input carrying userinfo (D5): drives the warning-userinfo status
  # and the NA clean_url in Stage B. Cached with the rest of Stage A.
  list(name = "scheme_less_userinfo", default = FALSE, template = logical(1)),
  # RFC 3986 scheme + path-rootless special-scheme rows (`http:example.com`):
  # parseable but hostless, so Stage B must not demote them to an error solely
  # because no authority exists.
  list(name = "rfc3986_path_rootless", default = FALSE, template = logical(1))
)
