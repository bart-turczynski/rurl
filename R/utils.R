# Package-wide utilities and global-variable declarations.

# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

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
  list(name = "is_scheme_relative", default = FALSE, template = logical(1))
)
