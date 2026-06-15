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

utils::globalVariables(c(
  "psl_clean",
  "tld_icann",
  "tld_private",
  "tld_all"
))
