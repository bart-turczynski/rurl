# Conflict-matrix tests for `case_handling` under `url_standard` (RURL-mevmyhxz,
# epic RURL-rzqpbpyp / parent RURL-uyjheruh). Under a selector the parse record
# takes case_handling = "lower_host". Under "rfc3986" every other value
# conflicts, because "lower" also lowercases the path
# (R/parse-phases.R .apply_case_policy_vec), which neither standard
# sanctions. Under "whatwg", the default since ADR 0020, "lower" and "upper"
# shape clean_url only (D4a); "keep" conflicts, since the parsed URL no longer
# holds the input's scheme and host case.

test_that("safe_parse_url()/safe_parse_urls() enforce case_handling conflict", {
  u <- "http://EX.com/Path"

  for (std in c("rfc3986", "whatwg")) {
    bad <- if (std == "rfc3986") c("keep", "lower", "upper") else "keep"
    for (ch in bad) {
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

test_that("whatwg confines lower and upper to clean_url (ADR 0020 D4a)", {
  u <- "http://EX.com/Path"
  ref <- safe_parse_urls(u, url_standard = "whatwg")
  for (ch in c("lower", "upper")) {
    r <- safe_parse_urls(u, url_standard = "whatwg", case_handling = ch)
    expect_identical(r[setdiff(names(r), "clean_url")],
                     ref[setdiff(names(ref), "clean_url")], info = ch)
  }
  expect_identical(
    safe_parse_urls(u, case_handling = "upper")$clean_url, "HTTP://EX.COM/PATH"
  )
  expect_identical(
    safe_parse_urls(u, case_handling = "lower")$clean_url, "http://ex.com/path"
  )
})

test_that("no case_handling conflict for a confinable value or selector", {
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
  # The host and the path are components: whatwg confines nothing there.
  expect_error(
    get_host(u, case_handling = "upper"), "governs `case_handling`"
  )
  expect_error(
    get_path(u, case_handling = "upper"), "governs `case_handling`"
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

test_that("the conflict error names a way out the caller can take", {
  u <- "http://EX.com/Path"
  # get_host() and get_path() take no `profile`, so they point at the
  # safe_parse_urls() column instead, with the knob the caller passed.
  expect_error(
    get_host(u, case_handling = "upper"),
    paste0(
      "safe_parse_urls(x, profile = \"rfc-syntax\", ",
      "case_handling = \"upper\")$host"
    ),
    fixed = TRUE
  )
  expect_error(
    get_path(u, case_handling = "upper"),
    paste0(
      "safe_parse_urls(x, profile = \"rfc-syntax\", ",
      "case_handling = \"upper\")$path"
    ),
    fixed = TRUE
  )
  # The escape says what else rfc-syntax changes, since following it can
  # drop a scheme-less row.
  expect_error(
    get_clean_url(u, case_handling = "keep"),
    "parse with profile = \"rfc-syntax\"", fixed = TRUE
  )
  expect_error(
    get_clean_url(u, case_handling = "keep"),
    "scheme_policy = \"require\"", fixed = TRUE
  )
})

test_that("canonical_join() enforces case_handling conflict through `...`", {
  A <- data.frame(URL = "http://EX.com/a", ValA = 1L, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://EX.com/a", ValB = 2L, stringsAsFactors = FALSE)

  expect_error(
    cj_deprecated(
      canonical_join(A, B, url_standard = "whatwg", case_handling = "keep")
    ),
    "governs `case_handling`"
  )

  joined <- cj_legacy(canonical_join(
    A, B,
    url_standard = "rfc3986", case_handling = "lower_host"
  ))
  expect_equal(nrow(joined), 1L)
})

test_that("url_standard = NULL reads as the default whatwg for case_handling", {
  u <- c("http://EX.com/Path", "HTTP://Another.Example/A%2fB")
  expect_identical(
    safe_parse_urls(u, case_handling = "upper", url_standard = NULL),
    safe_parse_urls(u, case_handling = "upper", url_standard = "whatwg")
  )
  expect_identical(
    safe_parse_urls(u, case_handling = "upper", url_standard = NULL),
    safe_parse_urls(u, case_handling = "upper")
  )
  expect_identical(
    get_host(u, case_handling = "lower", url_standard = NULL),
    get_host(u, case_handling = "lower")
  )
  expect_identical(
    get_clean_url(u, case_handling = "upper", url_standard = NULL),
    get_clean_url(u, case_handling = "upper")
  )
  expect_error(
    get_host(u, case_handling = "keep", url_standard = NULL),
    "governs `case_handling`"
  )
})
