# Conflict-matrix tests for `case_handling` under `url_standard` (RURL-mevmyhxz,
# epic RURL-rzqpbpyp / parent RURL-uyjheruh). Pure conflict-matrix closure: no
# new casing logic. Under a selector, only case_handling = "lower_host" is
# accepted; "keep", "lower", and "upper" all conflict, because "lower" also
# lowercases the path (R/parse-phases.R .apply_case_policy_vec), which neither
# RFC 3986 nor WHATWG sanctions.

test_that("safe_parse_url()/safe_parse_urls() enforce case_handling conflict", {
  u <- "http://EX.com/Path"

  for (std in c("rfc3986", "whatwg")) {
    for (ch in c("keep", "lower", "upper")) {
      expect_error(
        safe_parse_url(u, url_standard = std, case_handling = ch),
        "governs `case_handling`"
      )
      expect_error(
        safe_parse_urls(u, url_standard = std, case_handling = ch),
        "governs `case_handling`"
      )
    }
    # The value the profile requires is accepted.
    expect_silent(
      safe_parse_url(u, url_standard = std, case_handling = "lower_host")
    )
    expect_silent(
      safe_parse_urls(u, url_standard = std, case_handling = "lower_host")
    )
  }
})

test_that("no case_handling conflict without url_standard or selector alone", {
  u <- "http://EX.com/Path"
  expect_silent(safe_parse_url(u, case_handling = "upper"))
  expect_silent(safe_parse_url(u, url_standard = "rfc3986"))
  expect_silent(safe_parse_urls(u, url_standard = "whatwg"))
})

test_that("get_host/get_path/get_clean_url enforce case_handling conflict", {
  u <- "http://EX.com/Path"

  expect_error(
    get_host(u, url_standard = "rfc3986", case_handling = "upper"),
    "governs `case_handling`"
  )
  expect_error(
    get_path(u, url_standard = "whatwg", case_handling = "lower"),
    "governs `case_handling`"
  )
  expect_error(
    get_clean_url(u, url_standard = "rfc3986", case_handling = "keep"),
    "governs `case_handling`"
  )

  expect_silent(get_host(u, url_standard = "rfc3986"))
  expect_silent(get_path(u, url_standard = "whatwg"))
  expect_silent(get_clean_url(u, url_standard = "rfc3986"))

  # Accepted value passes through each accessor.
  expect_silent(
    get_host(u, url_standard = "rfc3986", case_handling = "lower_host")
  )
  expect_silent(
    get_path(u, url_standard = "whatwg", case_handling = "lower_host")
  )
  expect_silent(
    get_clean_url(u, url_standard = "rfc3986", case_handling = "lower_host")
  )
})

test_that("canonical_join() enforces case_handling conflict through `...`", {
  A <- data.frame(URL = "http://EX.com/a", ValA = 1L, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://EX.com/a", ValB = 2L, stringsAsFactors = FALSE)

  expect_error(
    canonical_join(A, B, url_standard = "whatwg", case_handling = "upper"),
    "governs `case_handling`"
  )

  joined <- canonical_join(
    A, B,
    url_standard = "rfc3986", case_handling = "lower_host"
  )
  expect_equal(nrow(joined), 1L)
})

test_that("url_standard = NULL keeps case_handling output byte-identical", {
  u <- c("http://EX.com/Path", "HTTP://Another.Example/A%2fB")
  expect_identical(
    safe_parse_urls(u, case_handling = "upper", url_standard = NULL),
    safe_parse_urls(u, case_handling = "upper")
  )
  expect_identical(
    get_host(u, case_handling = "keep", url_standard = NULL),
    get_host(u, case_handling = "keep")
  )
  expect_identical(
    get_path(u, case_handling = "lower", url_standard = NULL),
    get_path(u, case_handling = "lower")
  )
  expect_identical(
    get_clean_url(u, case_handling = "upper", url_standard = NULL),
    get_clean_url(u, case_handling = "upper")
  )
})
