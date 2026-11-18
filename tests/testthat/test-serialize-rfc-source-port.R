# RURL-zrvhcsud (standards audit finding 15, RUL-035): under
# `serialize_url(standard = "rfc3986", form = "source")` the port is written as
# the source spelled it. RFC 3986 section 5.3 recomposes the authority as
# written; dropping leading zeros or an empty port's ":" is normalization
# (section 3.2.3 lets "URI producers and normalizers" omit the ":"), which the
# source form disclaims. The parsed integer port column does not move.

test_that("rfc3986 source form keeps the port as written", {
  x <- c(
    "http://example.com:080/", "http://example.com:/", "foo://host:/",
    "http://example.com:0000/"
  )
  expect_identical(
    serialize_url(x, standard = "rfc3986", form = "source"), x
  )
})

test_that("rfc3986 source form keeps the port spelling in other authorities", {
  x <- c(
    "http://[::1]:080/p", "http://[::1]:/p", "http://u:p@h:00443/p?q#f",
    "foo://u@h:/p", "HTTP://Example.COM:0080/a/../%7e", "http://h:65535/"
  )
  expect_identical(
    serialize_url(x, standard = "rfc3986", form = "source"), x
  )
})

test_that("rfc3986 source form controls stay as written", {
  x <- c("http://example.com:8080/", "HTTP://Example.COM/a/../%7e")
  expect_identical(
    serialize_url(x, standard = "rfc3986", form = "source"), x
  )
})

test_that("the normalized form and whatwg still render the parsed port", {
  x <- c(
    "http://example.com:080/", "http://example.com:/", "foo://host:/",
    "http://example.com:0000/", "foo://host:0080/"
  )
  expect_identical(
    serialize_url(x, standard = "rfc3986", form = "normalized"),
    c(
      "http://example.com/", "http://example.com/", "foo://host/",
      "http://example.com:0/", "foo://host:80/"
    )
  )
  expect_identical(
    serialize_url(x, standard = "whatwg"),
    c(
      "http://example.com/", "http://example.com/", "foo://host/",
      "http://example.com:0/", "foo://host:80/"
    )
  )
})

test_that("the parsed port column stays an integer under rfc3986", {
  p <- safe_parse_urls(
    c("http://example.com:080/", "http://example.com:/", "foo://host:0080/"),
    url_standard = "rfc3986", scheme_policy = "require",
    scheme_acceptance = "general"
  )
  expect_identical(p$port, c(80L, NA_integer_, 80L))
})
