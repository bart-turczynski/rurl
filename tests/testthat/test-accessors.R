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
