# Global variables declaration for R CMD check
utils::globalVariables(c(
  "psl_clean",
  "tld_all",
  "tld_private",
  "tld_icann"
))

# Package-level variables for caches and pre-computed data
# These are initialized in .onLoad

# Memoization caches
.rurl_cache <- new.env(parent = emptyenv())

# Pre-computed PSL rule sets as environments for O(1) lookup
.psl_exception_set <- new.env(parent = emptyenv())
.psl_wildcard_set <- new.env(parent = emptyenv())
.psl_normal_set <- new.env(parent = emptyenv())

# TLD sets as environments for O(1) lookup
.tld_all_set <- new.env(parent = emptyenv())
.tld_icann_set <- new.env(parent = emptyenv())
.tld_private_set <- new.env(parent = emptyenv())

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize memoization caches
  .rurl_cache$full_parse <<- new.env(parent = emptyenv())
  .rurl_cache$domain <<- new.env(parent = emptyenv())
  .rurl_cache$tld <<- new.env(parent = emptyenv())

  # Get the package namespace to access internal data from sysdata.rda
  ns <- asNamespace(pkgname)

  # Pre-compute PSL rule sets from psl_clean (loaded from sysdata.rda)
  # This is the key optimization - do this once at load time, not on every call
  if (exists("psl_clean", envir = ns, inherits = FALSE)) {
    psl_data <- get("psl_clean", envir = ns, inherits = FALSE)

    # Extract exception rules (prefixed with !)
    exception_rules <- sub("^!", "", grep("^!", psl_data, value = TRUE))
    # Extract wildcard base rules (prefixed with *.)
    wildcard_rules <- sub("^\\*\\.", "", grep("^\\*\\.", psl_data, value = TRUE))
    # Normal rules are everything else
    normal_rules <- setdiff(
      psl_data,
      c(paste0("!", exception_rules), paste0("*.", wildcard_rules))
    )

    # Convert to environments (hash sets) for O(1) lookup
    # Using environment as hash: assign TRUE for each key
    for (rule in exception_rules) {
      assign(rule, TRUE, envir = .psl_exception_set)
    }
    for (rule in wildcard_rules) {
      assign(rule, TRUE, envir = .psl_wildcard_set)
    }
    for (rule in normal_rules) {
      assign(rule, TRUE, envir = .psl_normal_set)
    }
  }

  # Pre-compute TLD sets for O(1) lookup
  if (exists("tld_all", envir = ns, inherits = FALSE)) {
    tld_data <- get("tld_all", envir = ns, inherits = FALSE)
    for (tld in tld_data) {
      assign(tld, TRUE, envir = .tld_all_set)
    }
  }
  if (exists("tld_icann", envir = ns, inherits = FALSE)) {
    tld_data <- get("tld_icann", envir = ns, inherits = FALSE)
    for (tld in tld_data) {
      assign(tld, TRUE, envir = .tld_icann_set)
    }
  }
  if (exists("tld_private", envir = ns, inherits = FALSE)) {
    tld_data <- get("tld_private", envir = ns, inherits = FALSE)
    for (tld in tld_data) {
      assign(tld, TRUE, envir = .tld_private_set)
    }
  }

  invisible()
}

#' Clear all rurl caches
#'
#' Clears the memoization caches used by rurl functions. This is useful
#' if you need to free memory or if you've updated the PSL data.
#'
#' @return Invisibly returns NULL.
#' @export
#' @examples
#' rurl_clear_caches()
rurl_clear_caches <- function() {
  # Clear memoization caches by replacing with fresh environments
  .rurl_cache$full_parse <- new.env(parent = emptyenv())
  .rurl_cache$domain <- new.env(parent = emptyenv())
  .rurl_cache$tld <- new.env(parent = emptyenv())
  invisible(NULL)
}

# Helper function to check if a key exists in a hash set (environment)
.in_set <- function(key, set_env) {
  exists(key, envir = set_env, inherits = FALSE)
}
