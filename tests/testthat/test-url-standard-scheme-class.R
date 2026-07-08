# Tests for the special-scheme lookup + get_scheme_class() companion helper
# (RURL-jlvyjwog, epic RURL-rzqpbpyp / parent RURL-uyjheruh; PRD v2 §5 D7).
# Pure metadata: no expansion of rurl's allowed-scheme set, no widening of
# safe_parse_url()/safe_parse_urls() output. Follows the get_host_type()/
# get_url_diagnostics() companion-helper shape from v1 T2.

# --- Vocabulary --------------------------------------------------------------

test_that("the WHATWG special-scheme lookup covers exactly rurl's allowlist", {
  expect_setequal(
    rurl:::.WHATWG_SPECIAL_SCHEMES, c("http", "https", "ftp", "file")
  )
  # ftps (rurl's own FTP-over-TLS addition) is deliberately NOT special.
  expect_false("ftps" %in% rurl:::.WHATWG_SPECIAL_SCHEMES)
  # No expansion to ws/wss (non-goal, PRD v2 §3).
  expect_false(any(c("ws", "wss") %in% rurl:::.WHATWG_SPECIAL_SCHEMES))
  # Every special scheme is one rurl actually supports.
  expect_true(
    all(rurl:::.WHATWG_SPECIAL_SCHEMES %in% rurl:::.SUPPORTED_SCHEMES)
  )
})

test_that("the scheme_class vocabulary is special/non-special/missing", {
  expect_setequal(
    rurl:::.SCHEME_CLASSES, c("special", "non-special", "missing-or-error")
  )
})

# --- get_scheme_class() with no selector --------------------------------------

test_that("get_scheme_class is NA-equivalent with no selector", {
  u <- c("http://example.com/", "ftps://example.com/", "not-a-url")
  res <- get_scheme_class(u)
  expect_type(res, "character")
  expect_length(res, length(u))
  expect_true(all(is.na(res)))
  expect_identical(get_scheme_class(u, url_standard = NULL), res)
})

# --- Classification under a selector ------------------------------------------

test_that("get_scheme_class pins http/https/ftp/file as special", {
  u <- c("http://ex.com/", "https://ex.com/", "ftp://ex.com/", "file:///x")
  expect_identical(
    get_scheme_class(u, url_standard = "whatwg"),
    c("special", "special", "special", "special")
  )
  # The classification is standard-invariant: same result under rfc3986 (D7 --
  # "special scheme" is a fixed WHATWG fact about the scheme string, not
  # something RFC 3986 redefines).
  expect_identical(
    get_scheme_class(u, url_standard = "rfc3986"),
    c("special", "special", "special", "special")
  )
})

test_that("get_scheme_class pins ftps as non-special", {
  expect_identical(
    get_scheme_class("ftps://ex.com/", url_standard = "whatwg"), "non-special"
  )
  expect_identical(
    get_scheme_class("ftps://ex.com/", url_standard = "rfc3986"), "non-special"
  )
})

test_that("get_scheme_class reports missing-or-error for absent/bad schemes", {
  u <- c(
    "mailto:x@y.com", # unsupported scheme -> rejected upstream, scheme NA
    "not a url", # unparseable
    "//example.com/path", # scheme-relative, default scheme_relative_handling
    "" # empty input
  )
  expect_identical(
    get_scheme_class(u, url_standard = "whatwg"),
    rep("missing-or-error", length(u))
  )
})

test_that("get_scheme_class validates input and length-0", {
  expect_identical(get_scheme_class(character(0)), character(0))
  expect_identical(
    get_scheme_class(character(0), url_standard = "whatwg"), character(0)
  )
  expect_error(get_scheme_class(123), "must be a character vector")
  expect_error(
    get_scheme_class("http://ex.com/", "bogus"), "url_standard must be"
  )
})

# --- No new output shape anywhere (AC #1 corollary) ---------------------------

test_that("get_scheme_class adds no columns/fields to the parse result", {
  cols_before <- names(safe_parse_urls("http://ex.com/"))
  invisible(get_scheme_class("http://ex.com/", url_standard = "whatwg"))
  cols_after <- names(safe_parse_urls("http://ex.com/"))
  expect_identical(cols_after, cols_before)

  fields <- names(safe_parse_url("http://ex.com/", url_standard = "whatwg"))
  expect_false("scheme_class" %in% fields)
})
