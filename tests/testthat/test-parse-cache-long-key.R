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

# Long inputs: a 12,000-character ASCII URL, and a 2,000 x "e-acute" path whose
# \uXXXX-escaped key spends six bytes per code point. Both keys are past R's
# 10,000-byte variable-name cap.
lk_long_ascii <- paste0("http://a.example.com/", strrep("a", 12000L - 21L))
lk_long_utf8 <- paste0("http://a.example.com/", strrep("é", 2000L))

test_that("a long ASCII URL parses with the cache on", {
  lk_local_caches()
  expect_identical(nchar(lk_long_ascii), 12000L)
  cached <- safe_parse_url(lk_long_ascii, url_standard = "whatwg")
  expect_false(is.null(cached))
  expect_identical(
    cached,
    lk_uncached(safe_parse_url, lk_long_ascii, url_standard = "whatwg")
  )
  # Warm call: still a result, still equal.
  expect_identical(
    safe_parse_url(lk_long_ascii, url_standard = "whatwg"), cached
  )
})

test_that("a long non-ASCII URL parses with the cache on", {
  lk_local_caches()
  cached <- safe_parse_url(lk_long_utf8, url_standard = "whatwg")
  expect_false(is.null(cached))
  expect_identical(
    cached,
    lk_uncached(safe_parse_url, lk_long_utf8, url_standard = "whatwg")
  )
  expect_identical(
    safe_parse_url(lk_long_utf8, url_standard = "whatwg"), cached
  )
})

test_that("a mixed vector caches the short keys and bypasses the long ones", {
  lk_local_caches()
  mixed <- c(lk_short[1L], lk_long_ascii, lk_short[2L], lk_long_utf8)
  cold <- safe_parse_urls(mixed, url_standard = "whatwg")
  expect_identical(
    cold, lk_uncached(safe_parse_urls, mixed, url_standard = "whatwg")
  )
  # Only the two short keys were stored.
  expect_identical(
    rurl_cache_info()$entries[rurl_cache_info()$cache == "full_parse"], 2L
  )
  # Warm call: the short keys hit, the long ones are recomputed.
  seen <- lk_record_stage_a()
  warm <- safe_parse_urls(mixed, url_standard = "whatwg")
  expect_identical(warm, cold)
  expect_setequal(seen$urls, c(lk_long_ascii, lk_long_utf8))
})

test_that("a long host bypasses the Punycode cache", {
  lk_local_caches()
  # 1,100 labels of "xn--9caaa" make an ASCII host of 10,999 bytes, so the
  # puny_decode key alone is past the cap: the scalar .cache_get/.cache_set
  # path, reached whether or not full_parse is enabled.
  long_host <- paste0(
    "http://", paste(rep("xn--9caaa", 1100L), collapse = "."), "/"
  )
  cached <- safe_parse_url(long_host, url_standard = "whatwg")
  expect_false(is.null(cached))
  expect_identical(
    safe_parse_url(long_host, url_standard = "whatwg"), cached
  )
  rurl_cache_config(full_parse = FALSE)
  expect_identical(safe_parse_url(long_host, url_standard = "whatwg"), cached)
  rurl_cache_config(puny_encode = FALSE, puny_decode = FALSE)
  expect_identical(safe_parse_url(long_host, url_standard = "whatwg"), cached)
})

test_that("the key cap admits exactly R's 10,000-byte limit", {
  expect_true(.cache_key_usable(strrep("a", 10000L)))
  expect_false(.cache_key_usable(strrep("a", 10001L)))
  expect_identical(
    .cache_key_usable(c("a", strrep("a", 10001L), "")), c(TRUE, FALSE, TRUE)
  )
})
