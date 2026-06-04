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

# Cache configuration (enable/disable switches and the full_parse bound).
# Initialized in .onLoad; defaults preserve the historical behavior (all
# caches on, unbounded).
.rurl_config <- new.env(parent = emptyenv())

# Unique sentinel returned by .cache_get() on a miss. Distinct from every
# legitimately cached value, including NULL (unparseable URLs cache NULL) and
# NA, so a hit storing NULL is never mistaken for a miss.
.rurl_cache_sentinel <- new.env(parent = emptyenv())

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

  # Initialize cache configuration to the historical defaults. Use assign()
  # so we mutate the .rurl_config environment in place rather than rebinding
  # the (locked) namespace binding, mirroring how the cache environments above
  # are populated by reference.
  assign("full_parse_enabled", TRUE, envir = .rurl_config)
  assign("domain_enabled", TRUE, envir = .rurl_config)
  assign("tld_enabled", TRUE, envir = .rurl_config)
  assign("full_parse_max", Inf, envir = .rurl_config)

  # Get the package namespace to access internal data from sysdata.rda
  ns <- asNamespace(pkgname)

  normalize_tld_data <- function(x) {
    x <- enc2utf8(as.character(x))
    x <- trimws(x)
    x <- sub("^\\*\\.", "", x)
    x <- sub("^!", "", x)
    x <- sub("^\\.+", "", x)
    x <- x[nzchar(x)]
    unique(x)
  }

  # Pre-compute PSL rule sets from psl_clean (loaded from sysdata.rda)
  # This is the key optimization - do this once at load time, not on every call
  if (exists("psl_clean", envir = ns, inherits = FALSE)) {
    psl_data <- get("psl_clean", envir = ns, inherits = FALSE)

    # Extract exception rules (prefixed with !)
    exception_rules <- sub("^!", "", grep("^!", psl_data, value = TRUE))
    # Extract wildcard base rules (prefixed with *.)
    wildcard_rules <- sub(
      "^\\*\\.", "", grep("^\\*\\.", psl_data, value = TRUE)
    )
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
    tld_data <- normalize_tld_data(get("tld_all", envir = ns, inherits = FALSE))
    for (tld in tld_data) {
      assign(tld, TRUE, envir = .tld_all_set)
    }
  }
  if (exists("tld_icann", envir = ns, inherits = FALSE)) {
    tld_data <- normalize_tld_data(
      get("tld_icann", envir = ns, inherits = FALSE)
    )
    for (tld in tld_data) {
      assign(tld, TRUE, envir = .tld_icann_set)
    }
  }
  if (exists("tld_private", envir = ns, inherits = FALSE)) {
    tld_data <- normalize_tld_data(
      get("tld_private", envir = ns, inherits = FALSE)
    )
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

# Whether a named cache is currently enabled.
.cache_enabled <- function(cache_name) {
  switch(cache_name,
    full_parse = .rurl_config$full_parse_enabled,
    domain = .rurl_config$domain_enabled,
    tld = .rurl_config$tld_enabled
  )
}

# Look up `key` in the named cache. Returns the stored value (which may be
# NULL), or .rurl_cache_sentinel on a miss / when the cache is disabled.
.cache_get <- function(cache_name, key) {
  if (!.cache_enabled(cache_name)) {
    return(.rurl_cache_sentinel)
  }
  env <- .rurl_cache[[cache_name]]
  if (exists(key, envir = env, inherits = FALSE)) {
    get(key, envir = env, inherits = FALSE)
  } else {
    .rurl_cache_sentinel
  }
}

# Store `value` under `key` in the named cache, respecting the enable switch
# and (for full_parse) the max-entry bound. When adding a new key to a full
# full_parse cache, the cache is reset first so peak memory never exceeds the
# configured maximum. domain/tld are unbounded by design (they stay small —
# bounded by the number of unique hosts, not URL+option combinations).
.cache_set <- function(cache_name, key, value) {
  if (!.cache_enabled(cache_name)) {
    return(invisible(NULL))
  }
  env <- .rurl_cache[[cache_name]]
  if (cache_name == "full_parse" && is.finite(.rurl_config$full_parse_max)) {
    if (length(env) >= .rurl_config$full_parse_max &&
      !exists(key, envir = env, inherits = FALSE)) {
      .rurl_cache$full_parse <- new.env(parent = emptyenv())
      env <- .rurl_cache$full_parse
    }
  }
  assign(key, value, envir = env)
  invisible(NULL)
}

#' Inspect the rurl memoization caches
#'
#' Reports the number of entries currently held in each memoization cache,
#' along with whether the cache is enabled and any configured entry bound.
#'
#' @return A data.frame with one row per cache (\code{full_parse},
#'   \code{domain}, \code{tld}) and columns \code{entries}, \code{enabled},
#'   and \code{max_entries}.
#' @seealso \code{\link{rurl_cache_config}}, \code{\link{rurl_clear_caches}}
#' @export
#' @examples
#' get_domain("https://www.example.com")
#' rurl_cache_info()
rurl_cache_info <- function() {
  data.frame(
    cache = c("full_parse", "domain", "tld"),
    entries = c(
      length(.rurl_cache$full_parse),
      length(.rurl_cache$domain),
      length(.rurl_cache$tld)
    ),
    enabled = c(
      .rurl_config$full_parse_enabled,
      .rurl_config$domain_enabled,
      .rurl_config$tld_enabled
    ),
    max_entries = c(.rurl_config$full_parse_max, Inf, Inf),
    stringsAsFactors = FALSE
  )
}

#' Configure the rurl memoization caches
#'
#' Enables or disables individual caches and sets an optional bound on the
#' \code{full_parse} cache. Called with no arguments, it leaves the
#' configuration unchanged and returns the current state.
#'
#' Disabling a cache stops new writes to it (existing entries are left in
#' place until \code{\link{rurl_clear_caches}} is called). When
#' \code{full_parse} reaches \code{max_full_parse} entries, it is reset before
#' the next new entry is stored, so its peak size never exceeds the bound; the
#' default of \code{Inf} preserves the historical unbounded behavior. The
#' \code{domain} and \code{tld} caches are unbounded by design.
#'
#' @param full_parse Logical; enable/disable the full URL parse cache.
#' @param domain Logical; enable/disable the registered-domain cache.
#' @param tld Logical; enable/disable the TLD-extraction cache.
#' @param max_full_parse A single number (\eqn{\ge 1}) or \code{Inf} bounding
#'   the \code{full_parse} cache.
#' @return Invisibly, the updated \code{\link{rurl_cache_info}} data.frame.
#' @seealso \code{\link{rurl_cache_info}}, \code{\link{rurl_clear_caches}}
#' @export
#' @examples
#' rurl_cache_config(max_full_parse = 10000)
#' rurl_cache_config(domain = FALSE)
#' rurl_cache_config() # inspect current configuration
rurl_cache_config <- function(full_parse = NULL,
                              domain = NULL,
                              tld = NULL,
                              max_full_parse = NULL) {
  if (!is.null(full_parse)) {
    .rurl_config$full_parse_enabled <- isTRUE(full_parse)
  }
  if (!is.null(domain)) {
    .rurl_config$domain_enabled <- isTRUE(domain)
  }
  if (!is.null(tld)) {
    .rurl_config$tld_enabled <- isTRUE(tld)
  }
  if (!is.null(max_full_parse)) {
    if (!is.numeric(max_full_parse) ||
      length(max_full_parse) != 1L ||
      is.na(max_full_parse) ||
      max_full_parse < 1) {
      stop(
        "max_full_parse must be a single number >= 1 (or Inf).",
        call. = FALSE
      )
    }
    .rurl_config$full_parse_max <- max_full_parse
  }
  invisible(rurl_cache_info())
}

# Helper function to check if a key exists in a hash set (environment)
.in_set <- function(key, set_env) {
  exists(key, envir = set_env, inherits = FALSE)
}
