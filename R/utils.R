# Package-wide utilities and global-variable declarations.

# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# The URL schemes rurl supports. rurl's domain is authority-based (hierarchical)
# URLs -- these all share the "scheme://host[:port]/path" structure rurl parses,
# normalizes, and rebuilds. ftps is FTP-over-TLS (the https-analogue for ftp),
# not the unrelated SSH-based sftp. This is the single source of truth: a
# scheme-bearing input whose scheme is not here is rejected (opaque schemes like
# mailto:/tel:/data:, and unrecognized schemes like ws:/ssh:/typos). Adding a
# scheme is a one-line change here.
.SUPPORTED_SCHEMES <- c("http", "https", "ftp", "ftps")

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
  list(name = "scheme_less_userinfo", default = FALSE, template = logical(1))
)
