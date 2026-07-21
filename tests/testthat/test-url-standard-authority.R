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

# --- WHATWG non-special empty-host + port validation (RURL-kknambrz, T2) -----
# Under scheme_acceptance="general", url_standard="whatwg", the non-special
# authority parser must reject an empty host that carries a (non-null) port
# (WHATWG host-missing rule), while keeping an empty host with NO port legal for
# non-special schemes. RFC-profile authority/port rules are a separate path
# (sibling T4/T5) and must stay untouched.

test_that("whatwg general rejects empty host with a port", {
  bad <- c("data://:443", "sc://:12/")
  res <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, c("error", "error"))
})

test_that("whatwg general keeps empty host with no port accepted", {
  # foo:///bar (empty host, no port) stays legal; data://: is empty host + a
  # null (empty) port, which is also legal for a non-special scheme.
  ok <- c("foo:///bar", "data://:")
  res <- safe_parse_urls(
    ok, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, c("ok", "ok"))
  expect_true(all(is.na(res$host)))
  expect_true(all(is.na(res$port)))
})

test_that("rfc3986 general empty-host-with-port behavior is unchanged", {
  # T2 is WHATWG-only: the RFC profile still accepts these (its reg-name/port
  # rules are owned by sibling tickets), so the fix must not perturb it.
  res <- safe_parse_urls(
    c("data://:443", "sc://:12/"),
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(res$parse_status, c("ok", "ok"))
  expect_identical(res$port, c(443L, 12L))
})
