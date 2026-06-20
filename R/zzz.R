# Package initialization and the rurl memoization caches.
#
# Public Suffix List matching and data now live in the pslr package (queried via
# the .psl_* helpers in domain.R); rurl no longer ships PSL data or builds its
# own rule/TLD hash sets here. The caches below only memoize rurl's own work:
# full URL parses and the Punycode encode/decode round-trips used to reconstruct
# hosts. (PSL query results are cached inside pslr itself.)

# Internal registry: one entry per memoization cache, in the canonical order
# that public functions (rurl_cache_info, rurl_clear_caches, .cache_enabled)
# must preserve.
#
# `max_field` is optional: when present it names the .rurl_config field holding
# the cache's configured max-entry bound; when NULL the cache is unbounded
# (max_entries reported as Inf). This lets rurl_cache_info() read max_entries
# uniformly instead of special-casing full_parse.
.CACHE_REGISTRY <- list(
  list(
    name = "full_parse",
    default_enabled = TRUE,
    config_field = "full_parse_enabled",
    max_field = "full_parse_max"
  ),
  list(
    name = "puny_encode",
    default_enabled = TRUE,
    config_field = "puny_encode_enabled",
    max_field = NULL
  ),
  list(
    name = "puny_decode",
    default_enabled = TRUE,
    config_field = "puny_decode_enabled",
    max_field = NULL
  )
)

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

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize memoization caches from registry
  for (entry in .CACHE_REGISTRY) {
    .rurl_cache[[entry$name]] <<- new.env(parent = emptyenv())
    assign(entry$config_field, entry$default_enabled, envir = .rurl_config)
  }

  # Initialize cache configuration to the historical defaults. Use assign()
  # so we mutate the .rurl_config environment in place rather than rebinding
  # the (locked) namespace binding, mirroring how the cache environments above
  # are populated by reference.
  assign("full_parse_max", Inf, envir = .rurl_config)

  invisible()
}

#' Clear all rurl caches
#'
#' Clears the memoization caches used by rurl functions. This is useful
#' if you need to free memory.
#'
#' @return Invisibly returns NULL.
#' @export
#' @examples
#' rurl_clear_caches()
rurl_clear_caches <- function() {
  # Clear memoization caches by replacing with fresh environments
  for (entry in .CACHE_REGISTRY) {
    .rurl_cache[[entry$name]] <- new.env(parent = emptyenv())
  }
  invisible(NULL)
}

# Whether a named cache is currently enabled.
.cache_enabled <- function(cache_name) {
  entry <- Filter(function(e) e$name == cache_name, .CACHE_REGISTRY)
  if (length(entry) == 0L) return(FALSE)
  get(entry[[1L]]$config_field, envir = .rurl_config, inherits = FALSE)
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
# configured maximum. puny_encode/puny_decode are unbounded by design (they
# stay small — bounded by the number of unique hosts/labels, not URL+option
# combinations).
.cache_set <- function(cache_name, key, value) {
  if (!.cache_enabled(cache_name)) {
    return(invisible(NULL))
  }
  env <- .rurl_cache[[cache_name]]
  if (cache_name == "full_parse" && is.finite(.rurl_config$full_parse_max)) {
    cache_full_for_new_key <- length(env) >= .rurl_config$full_parse_max &&
      !exists(key, envir = env, inherits = FALSE)
    if (cache_full_for_new_key) {
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
#'   \code{puny_encode}, \code{puny_decode}) and columns \code{entries},
#'   \code{enabled}, and \code{max_entries}.
#' @seealso \code{\link{rurl_cache_config}}, \code{\link{rurl_clear_caches}}
#' @export
#' @examples
#' get_domain("https://www.example.com")
#' rurl_cache_info()
rurl_cache_info <- function() {
  data.frame(
    cache = vapply(.CACHE_REGISTRY, function(e) e$name, character(1L)),
    entries = vapply(
      .CACHE_REGISTRY,
      function(e) length(.rurl_cache[[e$name]]),
      integer(1L)
    ),
    enabled = vapply(
      .CACHE_REGISTRY,
      function(e) get(e$config_field, envir = .rurl_config, inherits = FALSE),
      logical(1L)
    ),
    max_entries = vapply(
      .CACHE_REGISTRY,
      function(e) {
        if (is.null(e$max_field)) {
          Inf
        } else {
          get(e$max_field, envir = .rurl_config, inherits = FALSE)
        }
      },
      numeric(1L)
    ),
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
#' \code{full_parse} reaches \code{max_full_parse} entries, the \emph{entire}
#' cache is cleared before the next new entry is stored, so its peak size never
#' exceeds the bound. This is a hard reset-watermark, not an LRU or FIFO
#' eviction policy: \code{max_full_parse} caps peak memory, but is \emph{not} a
#' working-set size — once the bound is hit the cache empties completely and
#' rebuilds from scratch. The default of \code{Inf} preserves the historical
#' unbounded behavior. The
#' \code{puny_encode} and \code{puny_decode} caches are unbounded by design
#' (each stays small — bounded by the number of unique hosts/labels seen, not
#' URL+option combinations).
#'
#' @param full_parse Logical; enable/disable the full URL parse cache.
#' @param puny_encode Logical; enable/disable the IDNA/Punycode encode cache.
#' @param puny_decode Logical; enable/disable the Punycode decode cache.
#' @param max_full_parse A single number (\eqn{\ge 1}) or \code{Inf} bounding
#'   the \code{full_parse} cache.
#' @return Invisibly, the updated \code{\link{rurl_cache_info}} data.frame.
#' @seealso \code{\link{rurl_cache_info}}, \code{\link{rurl_clear_caches}}
#' @export
#' @examples
#' rurl_cache_config(max_full_parse = 10000)
#' rurl_cache_config(puny_encode = FALSE)
#' rurl_cache_config() # inspect current configuration
rurl_cache_config <- function(full_parse = NULL,
                              puny_encode = NULL,
                              puny_decode = NULL,
                              max_full_parse = NULL) {
  .require_flag <- function(value, name) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(
        sprintf("%s must be a single non-NA logical (TRUE or FALSE).", name),
        call. = FALSE
      )
    }
    value
  }
  if (!is.null(full_parse)) {
    .rurl_config$full_parse_enabled <- .require_flag(full_parse, "full_parse")
  }
  if (!is.null(puny_encode)) {
    .rurl_config$puny_encode_enabled <-
      .require_flag(puny_encode, "puny_encode")
  }
  if (!is.null(puny_decode)) {
    .rurl_config$puny_decode_enabled <-
      .require_flag(puny_decode, "puny_decode")
  }
  if (!is.null(max_full_parse)) {
    invalid_max_full_parse <- !is.numeric(max_full_parse) ||
      length(max_full_parse) != 1L ||
      is.na(max_full_parse) ||
      max_full_parse < 1 ||
      !(is.infinite(max_full_parse) ||
          max_full_parse %% 1 == 0)
    if (invalid_max_full_parse) {
      stop(
        "max_full_parse must be a single integer >= 1 (or Inf).",
        call. = FALSE
      )
    }
    .rurl_config$full_parse_max <- max_full_parse
  }
  invisible(rurl_cache_info())
}
