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
  expect_equal(unname(get_parse_status("ftp:example.com")), "error")
  expect_equal(unname(get_parse_status("s3://bucket-name")), "error")
  expect_equal(unname(get_parse_status("data:text/plain,hello")), "error")
  expect_equal(unname(get_parse_status("htp://fake.com")), "error")
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
  expect_true(is.na(get_clean_url("mailto:user@example.com")))
  expect_true(is.na(get_clean_url("")))
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

test_that("get_path extracts path or returns NA", {
  expect_equal(unname(get_path("http://example.com/test")), "/test")
  expect_equal(unname(get_path("https://x.org/hello/world")), "/hello/world")
  expect_true(is.na(get_path("mailto:user@example.com")))
  expect_true(is.na(get_path("not a url")))
})

test_that(".get_registered_domain handles known cases correctly", {
  expect_equal(rurl:::.get_registered_domain("example.com"), "example.com")
  expect_equal(rurl:::.get_registered_domain("sub.example.co.uk"), "example.co.uk")
  expect_equal(rurl:::.get_registered_domain("sub.dev-builder.code.com"), "dev-builder.code.com")
  expect_equal(rurl:::.get_registered_domain("city.kawasaki.jp"), "city.kawasaki.jp")
  expect_equal(rurl:::.get_registered_domain("foo.bar.city.kawasaki.jp"), "city.kawasaki.jp")
  expect_equal(rurl:::.get_registered_domain("unknown.tld"), "unknown.tld")
  expect_equal(rurl:::.get_registered_domain("localhost"), NA_character_)
})

test_that(".get_registered_domain handles suffix-only domains", {
  expect_true(is.na(rurl:::.get_registered_domain("com")))
  expect_true(is.na(rurl:::.get_registered_domain("co.uk")))
})

test_that("get_parse_status falls through to final error", {
  mock_parse <- function(...) list(scheme = "gopher", host = "example.com")
  mockery::stub(get_parse_status, "safe_parse_url", mock_parse)
  expect_identical(unname(get_parse_status("whatever")), "error")
})

test_that("get_tld matches correct TLDs across sources", {
  # Simple domains
  expect_identical(unname(get_tld("sub.pl")), "pl")
  expect_identical(unname(get_tld("sub.com.pl")), "com.pl")
  expect_identical(unname(get_tld("sub.google")), "google")

  # Blogspot (private)
  expect_identical(unname(get_tld("sub.blogspot.com", source = "all")), "blogspot.com")
  expect_identical(unname(get_tld("sub.blogspot.com", source = "private")), "blogspot.com")
  expect_identical(unname(get_tld("sub.blogspot.com", source = "icann")), "com")

  # Wordpress (not a PSL private domain)
  expect_identical(unname(get_tld("sub.wordpress.com", source = "all")), "com")
  expect_identical(unname(get_tld("sub.wordpress.com", source = "private")), NA_character_)
  expect_identical(unname(get_tld("sub.wordpress.com", source = "icann")), "com")

  # Warszawa.pl (ICANN only)
  expect_identical(unname(get_tld("sub.warszawa.pl", source = "all")), "warszawa.pl")
  expect_identical(unname(get_tld("sub.warszawa.pl", source = "private")), NA_character_)
  expect_identical(unname(get_tld("sub.warszawa.pl", source = "icann")), "warszawa.pl")

  # Deep subdomains
  expect_identical(unname(get_tld("sub.sub.warszawa.pl", source = "all")), "warszawa.pl")
  expect_identical(unname(get_tld("sub.sub.warszawa.pl", source = "private")), NA_character_)
  expect_identical(unname(get_tld("sub.sub.warszawa.pl", source = "icann")), "warszawa.pl")
})

test_that("get_tld handles NA and edge cases", {
  expect_identical(unname(get_tld(NA_character_)), NA_character_)
  expect_identical(unname(get_tld("not a url")), NA_character_)
  expect_identical(unname(get_tld("http://localhost")), NA_character_)
  expect_identical(unname(get_tld("")), NA_character_)
  expect_identical(unname(get_tld("ftp://example.com")), "com")  # Non-http scheme
  expect_identical(unname(get_tld("http://192.168.1.1")), NA_character_)  # IP address
  expect_identical(unname(get_tld("http://example.")), NA_character_)  # Trailing dot
})

test_that("get_tld handles internationalized domain names (IDNs)", {
  expect_identical(unname(get_tld("石川.jp")), "石川.jp")
  expect_identical(unname(get_tld("münchen.de")), "de")
  expect_identical(unname(get_tld("παράδειγμα.ελ")), "ελ")
  expect_identical(unname(get_tld("xn--hxajbheg2az3al.xn--qxam")), "ελ")
  expect_identical(unname(get_tld("中国.中国")), "中国")
  expect_identical(unname(get_tld("xn--fiqs8s.xn--fiqs8s")), "中国")
})

test_that("get_tld handles edge cases and unexpected inputs gracefully", {
  expect_identical(unname(get_tld(NA_character_)), NA_character_)
  expect_identical(unname(get_tld("")), NA_character_)
  expect_identical(unname(get_tld("not a url")), NA_character_)
  expect_identical(unname(get_tld("http://localhost")), NA_character_)
  expect_identical(unname(get_tld("http://example.")), NA_character_)
  expect_identical(unname(get_tld("http://192.168.1.1")), NA_character_)
  expect_identical(unname(get_tld("ftp://example.com")), "com")

  # IDN edge cases
  expect_identical(unname(get_tld("xn--fiqs8s.xn--fiqs8s")), "中国")
  expect_identical(unname(get_tld("中国.中国")), "中国")
  expect_identical(unname(get_tld("xn--hxajbheg2az3al.xn--qxam")), "ελ")
  expect_identical(unname(get_tld("παράδειγμα.ελ")), "ελ")

  # Weird but valid cases
  expect_identical(unname(get_tld(".com")), "com")
  expect_true(is.na(rurl:::.get_registered_domain(".com")))
  expect_identical(unname(get_tld("example")), NA_character_)
  expect_identical(unname(get_tld("example..com")), "com")
})

test_that(".to_ascii falls back gracefully", {
  expect_equal(rurl:::.to_ascii("ascii-only.com"), "ascii-only.com")
})

test_that(".to_ascii handles edge cases and punycode encoding", {
  # NA and empty string handling
  expect_identical(unname(rurl:::.to_ascii(NA_character_)), NA_character_)
  expect_identical(unname(rurl:::.to_ascii("")), "")

  # ASCII-only domain should be returned unchanged
  expect_identical(unname(rurl:::.to_ascii("ascii-only.com")), "ascii-only.com")

  # Punycode encoding for non-ASCII domains
  result <- rurl:::.to_ascii("παράδειγμα.ελ")
  expect_true(grepl("^xn--", result))
})

test_that(".to_ascii falls back gracefully when urltools is unavailable", {
  skip_if_not_installed("mockery")

  # Redefine the function inside a local to safely stub
  local_fn <- rurl:::.to_ascii
  mockery::stub(local_fn, "requireNamespace", FALSE)

  expect_identical(unname(local_fn("δοκιμή.δοκιμή")), "δοκιμή.δοκιμή")
})

test_that(".normalize_and_punycode handles Unicode normalization and punycode encoding", {
  expect_match(unname(rurl:::.normalize_and_punycode("παράδειγμα.ελ")), "^xn--")
  expect_identical(unname(rurl:::.normalize_and_punycode("ascii-only.com")), "ascii-only.com")
})

test_that(".normalize_and_punycode returns NA_character_ on error", {
  fail_encode <- function(x) stop("fail")

  result <- rurl:::.normalize_and_punycode("παράδειγμα.ελ", encode_fn = fail_encode)
  expect_identical(unname(result), NA_character_)
})

test_that(".normalize_and_punycode handles NA and empty input", {
  expect_identical(unname(rurl:::.normalize_and_punycode(NA_character_)), NA_character_)
  expect_identical(unname(rurl:::.normalize_and_punycode("")), "")
})
