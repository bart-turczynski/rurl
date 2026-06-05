# Tests for cache introspection (rurl_cache_info), configuration
# (rurl_cache_config), and the full_parse bounding policy.

# Restore the default cache configuration and empty the caches. Called at the
# start of every test so they are independent, and once at the end so no
# modified global state leaks into other test files.
reset_caches <- function() {
  rurl_cache_config(
    full_parse = TRUE, domain = TRUE, tld = TRUE,
    puny_encode = TRUE, puny_decode = TRUE, max_full_parse = Inf
  )
  rurl_clear_caches()
}

test_that("rurl_cache_info reports the caches with expected columns", {
  reset_caches()
  info <- rurl_cache_info()
  expect_s3_class(info, "data.frame")
  expect_equal(
    info$cache,
    c("full_parse", "domain", "tld", "puny_encode", "puny_decode")
  )
  expect_named(info, c("cache", "entries", "enabled", "max_entries"))
  expect_equal(info$entries, c(0, 0, 0, 0, 0))
  expect_true(all(info$enabled))
  expect_equal(info$max_entries, c(Inf, Inf, Inf, Inf, Inf))
})

test_that("cache info reflects entries after parsing and clearing resets it", {
  reset_caches()
  invisible(get_domain(c(
    "https://www.a.example.com", "http://b.example.org/x"
  )))
  info <- rurl_cache_info()
  expect_gt(info$entries[info$cache == "full_parse"], 0)
  expect_gt(info$entries[info$cache == "domain"], 0)

  rurl_clear_caches()
  expect_equal(rurl_cache_info()$entries, c(0, 0, 0, 0, 0))
})

test_that("disabled full_parse cache does not store results", {
  reset_caches()
  rurl_cache_config(full_parse = FALSE)
  invisible(safe_parse_url("https://disabled.example.com"))
  expect_equal(rurl_cache_info()$entries[1], 0)
  reset_caches()
})

test_that("disabling caches preserves correctness", {
  reset_caches()
  enabled <- safe_parse_url("https://www.sub.example.co.uk:8080/a/?q=1")
  reset_caches()
  rurl_cache_config(full_parse = FALSE, domain = FALSE, tld = FALSE)
  disabled <- safe_parse_url("https://www.sub.example.co.uk:8080/a/?q=1")
  expect_identical(disabled, enabled)
  reset_caches()
})

test_that("punycode caches populate and preserve correctness", {
  reset_caches()
  idn <- c(
    "https://münchen.de/a", "http://例え.jp/b",
    "https://bücher.example.com/c", "https://xn--mnchen-3ya.de/d"
  )
  # Distinct full URLs so the full_parse cache never hides the punycode path.
  enabled <- safe_parse_urls(idn, host_encoding = "idna")
  enabled_uni <- safe_parse_urls(idn, host_encoding = "unicode")
  info <- rurl_cache_info()
  expect_gt(info$entries[info$cache == "puny_encode"], 0)
  expect_gt(info$entries[info$cache == "puny_decode"], 0)

  reset_caches()
  rurl_cache_config(puny_encode = FALSE, puny_decode = FALSE)
  expect_identical(safe_parse_urls(idn, host_encoding = "idna"), enabled)
  expect_identical(
    safe_parse_urls(idn, host_encoding = "unicode"), enabled_uni
  )
  # Disabled puny caches must not store anything.
  info_off <- rurl_cache_info()
  expect_equal(info_off$entries[info_off$cache == "puny_encode"], 0)
  expect_equal(info_off$entries[info_off$cache == "puny_decode"], 0)
  reset_caches()
})

test_that("full_parse honors max_full_parse and never exceeds it", {
  reset_caches()
  rurl_cache_config(max_full_parse = 50)
  for (i in seq_len(300)) {
    invisible(safe_parse_url(paste0("http://h", i, ".example.com/p")))
  }
  expect_lte(rurl_cache_info()$entries[1], 50)
  reset_caches()
})

test_that("rurl_cache_config validates max_full_parse", {
  reset_caches()
  expect_error(rurl_cache_config(max_full_parse = 0), "max_full_parse")
  expect_error(rurl_cache_config(max_full_parse = -5), "max_full_parse")
  expect_error(rurl_cache_config(max_full_parse = c(1, 2)), "max_full_parse")
  expect_error(rurl_cache_config(max_full_parse = "lots"), "max_full_parse")
  reset_caches()
})

test_that("rurl_cache_config() with no args reports without changing state", {
  reset_caches()
  rurl_cache_config(domain = FALSE, max_full_parse = 1234)
  before <- rurl_cache_config()
  after <- rurl_cache_info()
  expect_identical(before, after)
  expect_false(after$enabled[after$cache == "domain"])
  expect_equal(after$max_entries[1], 1234)
  reset_caches()
})

# Final safety net: make sure default config is restored for later test files.
reset_caches()
