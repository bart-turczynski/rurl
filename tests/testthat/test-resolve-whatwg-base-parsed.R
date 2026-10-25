# RURL-vgmyrgxb: resolve_url() under whatwg and a base the parser respells.
#
# The URL Standard's API URL parser parses the base into a URL record before
# it reads the input, so the merge runs against that record, not against the
# base as written. A special-scheme base spelled with backslashes
# (`http:\\a\b\c`, "special authority slashes state") or without its slashes
# (`http:x`, "special relative or authority state") is the base
# `http://a/b/c` or `http://x/`. RFC 3986 has no such reading, and the NULL
# selector is frozen (ADR 0007).
#
# This file opens with PINS: the rfc3986 and NULL rows, and a base the parser
# only re-encodes, must not move.

respelled <- list(
  list(base = "http:\\\\a\\b\\c", ref = "g", want = "http://a/b/g"),
  list(base = "http:\\\\a\\b\\c", ref = "../g", want = "http://a/g"),
  list(base = "http:\\\\a\\b\\c", ref = "..", want = "http://a/"),
  list(base = "http:x", ref = "g", want = "http://x/g"),
  list(base = "http:/x/y", ref = "/g", want = "http://x/g"),
  list(base = "file:\\\\h\\a\\b", ref = "g", want = "file://h/a/g"),
  list(base = "file:c:/x", ref = "/g", want = "file:///c:/g"),
  list(base = "ws:/a", ref = "g", want = "ws://a/g")
)

test_that("pin: rfc3986 and NULL read a respelled base as written", {
  expect_identical(
    resolve_url("g", "http:\\\\a\\b\\c", url_standard = "rfc3986",
                output = "serialized"),
    NA_character_
  )
  expect_identical(
    resolve_url("g", "http:x", url_standard = "rfc3986",
                output = "serialized"),
    "http:g"
  )
  expect_identical(resolve_url("g", "http:x"), NA_character_)
})

test_that("pin: a base the parser only re-encodes keeps its spelling", {
  expect_identical(
    resolve_url("g", "http://bücher.de/ä/b", url_standard = "whatwg"),
    "http://bücher.de/ä/g"
  )
  expect_identical(
    resolve_url("g", "http://a/é/c", url_standard = "whatwg"),
    "http://a/é/g"
  )
  expect_identical(
    resolve_url("g", "http://bücher.de/ä/b", url_standard = "whatwg",
                output = "serialized"),
    "http://xn--bcher-kva.de/%C3%A4/g"
  )
})

test_that("whatwg merges against the base its parser produced", {
  for (row in respelled) {
    expect_identical(
      resolve_url(row$ref, row$base, url_standard = "whatwg",
                  output = "serialized"),
      row$want, info = paste(row$base, row$ref)
    )
  }
  expect_identical(
    resolve_url("g", "http:\\\\a\\b\\c", url_standard = "whatwg"),
    "http://a/b/g"
  )
  expect_identical(
    resolve_url("g", "http:x", profile = "whatwg"),
    "http://x/g"
  )
})

test_that("a respelled base resolves the same in a vector of rows", {
  bases <- c("http://a/b/c", "http:\\\\a\\b\\c", NA, "http:x", "http://a/b/c")
  expect_identical(
    resolve_url("g", bases, url_standard = "whatwg", output = "serialized"),
    c("http://a/b/g", "http://a/b/g", NA, "http://x/g", "http://a/b/g")
  )
})

test_that("a row that moves takes the parsed spelling of a Unicode base", {
  expect_identical(
    resolve_url("g", "http:\\\\bücher.de\\ä\\b", url_standard = "whatwg"),
    "http://xn--bcher-kva.de/%C3%A4/g"
  )
})
