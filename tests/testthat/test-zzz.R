test_that("rurl_clear_caches resets memoization environments", {
  assign("k1", "v1", envir = rurl:::.rurl_cache$full_parse)
  assign("k2", "v2", envir = rurl:::.rurl_cache$puny_encode)
  assign("k3", "v3", envir = rurl:::.rurl_cache$puny_decode)

  old_envs <- list(
    full = rurl:::.rurl_cache$full_parse,
    puny_encode = rurl:::.rurl_cache$puny_encode,
    puny_decode = rurl:::.rurl_cache$puny_decode
  )

  rurl_clear_caches()

  expect_true(is.environment(rurl:::.rurl_cache$full_parse))
  expect_true(is.environment(rurl:::.rurl_cache$puny_encode))
  expect_true(is.environment(rurl:::.rurl_cache$puny_decode))
  expect_false(identical(old_envs$full, rurl:::.rurl_cache$full_parse))
  expect_false(identical(old_envs$puny_encode, rurl:::.rurl_cache$puny_encode))
  expect_false(identical(old_envs$puny_decode, rurl:::.rurl_cache$puny_decode))
  expect_equal(ls(rurl:::.rurl_cache$full_parse), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_encode), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_decode), character(0))
})

test_that(".onLoad initializes the memoization caches and config", {
  assign("stale", TRUE, envir = rurl:::.rurl_cache$full_parse)
  assign("stale", TRUE, envir = rurl:::.rurl_cache$puny_encode)
  assign("stale", TRUE, envir = rurl:::.rurl_cache$puny_decode)

  ns <- asNamespace("rurl")
  was_locked <- bindingIsLocked(".rurl_cache", ns)
  if (was_locked) {
    unlockBinding(".rurl_cache", ns)
    withr::defer(lockBinding(".rurl_cache", ns), testthat::teardown_env())
  }

  rurl:::.onLoad(NULL, "rurl")

  # Caches are reinitialized and empty
  expect_equal(ls(rurl:::.rurl_cache$full_parse), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_encode), character(0))
  expect_equal(ls(rurl:::.rurl_cache$puny_decode), character(0))

  # Config restored to historical defaults
  info <- rurl_cache_info()
  expect_equal(info$cache, c("full_parse", "puny_encode", "puny_decode"))
  expect_true(all(info$enabled))
})
