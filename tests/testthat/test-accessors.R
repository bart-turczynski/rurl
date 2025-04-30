test_that("get_clean_url returns expected values", {
  urls <- c("example.com", "http://test.com/page")
  expect_equal(
    unname(get_clean_url(urls)),
    c("http://example.com/", "http://test.com/page")
  )
})

test_that("get_domain works with subdomains", {
  expect_equal(unname(get_domain("https://sub.example.co.uk")), "example.co.uk")
  expect_equal(unname(get_domain("http://localhost")), NA_character_)
})

test_that("get_scheme respects protocol_handling", {
  expect_equal(unname(get_scheme("https://x.com")), "https")
  expect_equal(unname(get_scheme("x.com", "http")), "http")
  expect_equal(unname(get_scheme("x.com", "none")), NA_character_)
})

test_that("get_parse_status returns 'ok' or 'error'", {
  expect_equal(unname(get_parse_status("example.com")), "ok")
  expect_equal(unname(get_parse_status("not a url")), "error")
})

test_that("get_parse_status flags ftp correctly", {
  expect_equal(unname(get_parse_status("ftp://example.com")), "ok-ftp")
})

test_that("get_parse_status validates allowed schemes only", {
  expect_equal(unname(get_parse_status("http://example.com")), "ok")
  expect_equal(unname(get_parse_status("https://example.com")), "ok")
  expect_equal(unname(get_parse_status("ftp://example.com")), "ok-ftp")
  expect_equal(unname(get_parse_status("ftps://example.com")), "ok-ftp")
})

test_that("get_parse_status rejects unsupported or malformed schemes", {
  expect_equal(unname(get_parse_status("mailto:x@example.com")), "error")
  expect_equal(unname(get_parse_status("file:///home/user/file.txt")), "error")
  expect_equal(unname(get_parse_status("ftp:example.com")), "error")     # Missing slashes
  expect_equal(unname(get_parse_status("s3://bucket-name")), "error")
  expect_equal(unname(get_parse_status("data:text/plain,hello")), "error")
  expect_equal(unname(get_parse_status("htp://fake.com")), "error")      # Misspelled http
})

test_that("safe_parse_url returns NULL for NA, non-character, or empty input", {
  expect_null(safe_parse_url(NA_character_))
  expect_null(safe_parse_url(""))
  expect_null(safe_parse_url(12345))
})

test_that("get_parse_status returns error for parseable but unsupported schemes", {
  expect_equal(unname(get_parse_status("ws://example.com")), "error")
})

test_that("get_parse_status detects no-TLD hosts", {
  expect_equal(unname(get_parse_status("http://just-a-path")), "warning-no-tld")
  expect_equal(unname(get_parse_status("http://localhost")), "warning-no-tld")
  expect_equal(unname(get_parse_status("http://example.com")), "ok")
})

test_that("get_clean_url returns NA if parsed is NULL or host/path is missing", {
  expect_true(is.na(get_clean_url("mailto:user@example.com")))  # parsed is NULL
  # expect_true(is.na(get_clean_url("http:///just-a-path")))      # host is empty string
  expect_true(is.na(get_clean_url("")))                         # parsed is NULL
})

test_that("get_domain returns NA when parsed or host is missing", {
  expect_true(is.na(get_domain("mailto:user@example.com")))
  expect_true(is.na(get_domain("http:///nohost")))
  expect_true(is.na(get_domain("not a url")))
})

test_that("get_scheme returns NA when parsing fails", {
  expect_true(is.na(get_scheme("mailto:user@example.com")))
  expect_true(is.na(get_scheme("not a url")))
})

test_that("get_host extracts host or returns NA", {
  expect_equal(unname(get_host("http://example.com/path")), "example.com")
  expect_equal(unname(get_host("https://sub.domain.org/")), "sub.domain.org")
  expect_true(is.na(get_host("mailto:user@example.com")))
  expect_true(is.na(get_host("not a url")))
})
