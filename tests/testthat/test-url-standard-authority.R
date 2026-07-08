# Tests for selector-profile authority recovery slices.

test_that("whatwg uses last at-sign as authority host delimiter", {
  u <- "http://username@@@@example.com"
  res <- safe_parse_url(u, url_standard = "whatwg")

  expect_identical(res$host, "example.com")
  expect_identical(res$user, "username%40%40%40")
  expect_true(is.na(res$password))
  expect_identical(res$clean_url, "http://example.com/")
  expect_identical(res$parse_status, "ok")
})

test_that("rfc3986 selector recovers host at last ambiguous at-sign", {
  u <- "http://username@@@@example.com"
  res <- safe_parse_url(u, url_standard = "rfc3986")

  expect_identical(res$host, "example.com")
  expect_identical(res$user, "username%40%40%40")
  expect_true(is.na(res$password))
  expect_identical(res$clean_url, "http://example.com/")
  expect_identical(res$parse_status, "ok")
})

test_that("no selector leaves repeated at-sign authority baseline unchanged", {
  expect_null(safe_parse_url("http://username@@@@example.com"))
})

test_that("whatwg recovers authority for special schemes without slashes", {
  res <- safe_parse_urls(
    c("http:example.com", "https:example.com/path"),
    url_standard = "whatwg"
  )

  expect_identical(res$scheme, c("http", "https"))
  expect_identical(res$host, c("example.com", "example.com"))
  expect_identical(res$path, c("/", "/path"))
  expect_identical(
    res$clean_url, c("http://example.com/", "https://example.com/path")
  )
  expect_identical(res$parse_status, c("ok", "ok"))
})

test_that("rfc3986 keeps missing-slash special schemes as path-rootless", {
  res <- safe_parse_urls(
    c("http:example.com", "https:example.com/path"),
    url_standard = "rfc3986"
  )

  expect_identical(res$scheme, c("http", "https"))
  expect_true(all(is.na(res$host)))
  expect_identical(res$path, c("example.com", "example.com/path"))
  expect_true(all(is.na(res$clean_url)))
  expect_identical(res$parse_status, c("ok", "ok"))
})

test_that("no selector keeps special schemes without slashes as errors", {
  urls <- c("http:example.com", "https:example.com/path")

  expect_true(all(is.na(get_clean_url(urls))))
  expect_identical(get_parse_status(urls), c("error", "error"))
})

test_that("rfc3986 empty authority does not duplicate host into path", {
  urls <- c("https:///evil.com", "http:///evil.com")
  res <- safe_parse_urls(urls, url_standard = "rfc3986")

  expect_identical(res$host, c("evil.com", "evil.com"))
  expect_identical(res$path, c("/", "/"))
  expect_identical(
    res$clean_url, c("https://evil.com/", "http://evil.com/")
  )
  expect_identical(res$parse_status, c("ok", "ok"))

  for (i in seq_along(urls)) {
    occurrences <- sum(
      c(res$host[[i]], res$path[[i]]) == "evil.com" |
        c(res$host[[i]], res$path[[i]]) == "/evil.com",
      na.rm = TRUE
    )
    expect_identical(occurrences, 1L)
  }
})

test_that("whatwg empty-authority special schemes stay host/path coherent", {
  urls <- c("https:///evil.com", "https:////evil.com", "http:///evil.com")
  res <- safe_parse_urls(urls, url_standard = "whatwg")

  expect_identical(res$host, c("evil.com", "evil.com", "evil.com"))
  expect_identical(res$path, c("/", "/", "/"))
  expect_identical(
    res$clean_url,
    c("https://evil.com/", "https://evil.com/", "http://evil.com/")
  )
  expect_identical(res$parse_status, c("ok", "ok", "ok"))
})

test_that("rfc3986 rejects unsupported excess-slash empty authority", {
  res <- safe_parse_url("https:////evil.com", url_standard = "rfc3986")

  expect_null(res)
  expect_identical(
    get_parse_status("https:////evil.com", url_standard = "rfc3986"), "error"
  )
})
