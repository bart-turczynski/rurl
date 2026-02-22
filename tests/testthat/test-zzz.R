test_that(".in_set only checks the target environment", {
  parent_env <- new.env(parent = emptyenv())
  assign("shared_key", TRUE, envir = parent_env)

  child_env <- new.env(parent = parent_env)
  assign("child_key", TRUE, envir = child_env)

  expect_true(rurl:::.in_set("child_key", child_env))
  expect_false(rurl:::.in_set("shared_key", child_env))
})

test_that("rurl_clear_caches resets memoization environments", {
  assign("k1", "v1", envir = rurl:::.rurl_cache$full_parse)
  assign("k2", "v2", envir = rurl:::.rurl_cache$domain)
  assign("k3", "v3", envir = rurl:::.rurl_cache$tld)

  old_envs <- list(
    full = rurl:::.rurl_cache$full_parse,
    domain = rurl:::.rurl_cache$domain,
    tld = rurl:::.rurl_cache$tld
  )

  rurl_clear_caches()

  expect_true(is.environment(rurl:::.rurl_cache$full_parse))
  expect_true(is.environment(rurl:::.rurl_cache$domain))
  expect_true(is.environment(rurl:::.rurl_cache$tld))
  expect_false(identical(old_envs$full, rurl:::.rurl_cache$full_parse))
  expect_false(identical(old_envs$domain, rurl:::.rurl_cache$domain))
  expect_false(identical(old_envs$tld, rurl:::.rurl_cache$tld))
  expect_equal(ls(rurl:::.rurl_cache$full_parse), character(0))
  expect_equal(ls(rurl:::.rurl_cache$domain), character(0))
  expect_equal(ls(rurl:::.rurl_cache$tld), character(0))
})

test_that(".onLoad populates PSL and TLD hash sets", {
  if (exists("last.warning", envir = baseenv(), inherits = FALSE)) {
    assign("last.warning", NULL, envir = baseenv())
  }
  # Clear existing sets to mimic a fresh package load
  for (env in list(
    rurl:::.psl_exception_set,
    rurl:::.psl_wildcard_set,
    rurl:::.psl_normal_set,
    rurl:::.tld_all_set,
    rurl:::.tld_icann_set,
    rurl:::.tld_private_set
  )) {
    rm(list = ls(envir = env, all.names = TRUE), envir = env)
  }

  assign("stale", TRUE, envir = rurl:::.rurl_cache$full_parse)
  assign("stale", TRUE, envir = rurl:::.rurl_cache$domain)
  assign("stale", TRUE, envir = rurl:::.rurl_cache$tld)

  ns <- asNamespace("rurl")
  was_locked <- bindingIsLocked(".rurl_cache", ns)
  if (was_locked) {
    unlockBinding(".rurl_cache", ns)
    withr::defer(lockBinding(".rurl_cache", ns), testthat::teardown_env())
  }

  suppressWarnings(rurl:::.onLoad(NULL, "rurl"))

  # Derive representative rules from current PSL data (data-driven)
  psl <- rurl:::psl_clean
  wildcard_rules <- sub("^\\*\\.", "", grep("^\\*\\.", psl, value = TRUE))
  exception_rules <- sub("^!", "", grep("^!", psl, value = TRUE))
  normal_rules <- setdiff(
    psl,
    c(paste0("!", exception_rules), paste0("*.", wildcard_rules))
  )
  if (length(wildcard_rules) == 0 ||
      length(exception_rules) == 0 ||
      length(normal_rules) == 0) {
    testthat::skip(
      "PSL data does not contain expected rule types to validate .onLoad sets."
    )
  }

  # Exception rules (stored without the leading "!")
  expect_true(rurl:::.in_set(exception_rules[1], rurl:::.psl_exception_set))
  # Wildcard rules (stored without the leading "*.")
  expect_true(rurl:::.in_set(wildcard_rules[1], rurl:::.psl_wildcard_set))
  # Normal rules
  expect_true(rurl:::.in_set(normal_rules[1], rurl:::.psl_normal_set))

  # TLD sets
  expect_true(rurl:::.in_set("com", rurl:::.tld_icann_set))
  expect_true(rurl:::.in_set("blogspot.com", rurl:::.tld_private_set))
  expect_true(rurl:::.in_set("blogspot.com", rurl:::.tld_all_set))

  # Caches are reinitialized and empty
  expect_equal(ls(rurl:::.rurl_cache$full_parse), character(0))
  expect_equal(ls(rurl:::.rurl_cache$domain), character(0))
  expect_equal(ls(rurl:::.rurl_cache$tld), character(0))

  if (exists("last.warning", envir = baseenv(), inherits = FALSE)) {
    assign("last.warning", NULL, envir = baseenv())
  }
})
