# The memoization caches are environments, and a cache key is used as a
# variable name (exists/get/assign/mget). R caps a variable name at 10,000
# bytes, so a key at or past that cap must bypass the cache instead of raising
# "variable names are limited to 10000 bytes" (RURL-tlmoybsl). The pins below
# hold the behavior the bypass must not disturb: short keys still hit the warm
# path, and the cache stays transparent for ordinary URLs.

# Snapshot the cache configuration, empty the caches, and restore both when the
# calling test exits, so no modified global state leaks into other files.
lk_local_caches <- function(env = parent.frame()) {
  before <- rurl_cache_info()
  withr::defer(
    {
      rurl_cache_config(
        full_parse = before$enabled[before$cache == "full_parse"],
        puny_encode = before$enabled[before$cache == "puny_encode"],
        puny_decode = before$enabled[before$cache == "puny_decode"],
        max_full_parse = before$max_entries[before$cache == "full_parse"]
      )
      rurl_clear_caches()
    },
    envir = env
  )
  rurl_cache_config(full_parse = TRUE, puny_encode = TRUE, puny_decode = TRUE)
  rurl_clear_caches()
  invisible(before)
}

# Record the URLs every Stage A computation receives. A warm cache hit skips
# Stage A, so the recorder shows exactly which inputs missed the cache.
lk_record_stage_a <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$urls <- character()
  orig <- get("._parse_stage_a_vec", envir = asNamespace("rurl"))
  testthat::local_mocked_bindings(
    ._parse_stage_a_vec = function(parse_input, opts) {
      seen$urls <- c(seen$urls, parse_input)
      orig(parse_input, opts)
    },
    .env = env
  )
  seen
}

# Parse with the full_parse cache off: the transparency oracle.
lk_uncached <- function(fn, ...) {
  rurl_cache_config(full_parse = FALSE)
  on.exit(rurl_cache_config(full_parse = TRUE), add = TRUE)
  fn(...)
}

lk_short <- c(
  "https://www.Example.COM/A/b?q=1&x=2#frag",
  "http://user:pw@sub.example.co.uk:8080/p/",
  "https://münchen.de/straße"
)

test_that("short keys still hit the warm vector path", {
  lk_local_caches()
  cold <- safe_parse_urls(lk_short, url_standard = "whatwg")
  seen <- lk_record_stage_a()
  warm <- safe_parse_urls(lk_short, url_standard = "whatwg")
  expect_identical(seen$urls, character())
  expect_identical(warm, cold)
  expect_identical(
    rurl_cache_info()$entries[rurl_cache_info()$cache == "full_parse"],
    length(lk_short)
  )
})

test_that("the cache stays transparent for ordinary URLs", {
  lk_local_caches()
  expect_identical(
    safe_parse_urls(lk_short, url_standard = "whatwg"),
    lk_uncached(safe_parse_urls, lk_short, url_standard = "whatwg")
  )
  for (u in lk_short) {
    expect_identical(
      safe_parse_url(u, url_standard = "whatwg"),
      lk_uncached(safe_parse_url, u, url_standard = "whatwg")
    )
  }
})
