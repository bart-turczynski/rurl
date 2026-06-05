# Package-wide utilities and global-variable declarations.

# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

utils::globalVariables(c(
  "psl_clean",
  "tld_icann",
  "tld_private",
  "tld_all"
))
