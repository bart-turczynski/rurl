# ADR 0012 Layer 4a (RURL-sxssynfu): the INDEPENDENT RFC 3986 generic-URI
# grammar gate (`.rfc3986_generic_uri_ok`) and its hand-authored ABNF fixture
# corpus -- the NORMATIVE ORACLE for the new RFC-general branch (D1). The gate
# is independent of libcurl: a permissive component splitter (curl/WHATWG)
# "accepts" strings D1 deliberately rejects (e.g. a repeated raw @), so parity
# with a backend is NOT proof of grammar conformance. These tests assert the
# gate's verdict directly against the CSV, never against a backend.

.rfc3986_fixtures <- function() {
  utils::read.csv(
    testthat::test_path("fixtures", "rfc3986-abnf-fixtures.csv"),
    stringsAsFactors = FALSE,
    colClasses = "character",
    encoding = "UTF-8"
  )
}

test_that("the gate verdict matches every hand-authored ABNF fixture", {
  fx <- .rfc3986_fixtures()
  # sanity: the corpus carries the 8 RURL-wncwfasl false-rejects + adversarial
  # cases, and exercises BOTH branches.
  expect_gte(nrow(fx), 18L)
  expect_true(all(fx$expect %in% c("accept", "reject")))
  expect_true(any(fx$expect == "accept"))
  expect_true(any(fx$expect == "reject"))

  res <- .rfc3986_generic_uri_ok(fx$input)
  expect_length(res$ok, nrow(fx))

  for (i in seq_len(nrow(fx))) {
    want <- identical(fx$expect[i], "accept")
    expect_identical(
      isTRUE(res$ok[i]), want,
      info = paste0(fx$id[i], ": ", fx$input[i], " -- ", fx$reason[i])
    )
  }
})

test_that("all 8 RURL-wncwfasl false-reject inputs get their verdict", {
  known <- list(
    list(input = "mailto:a@b.com", accept = TRUE),
    list(input = "data:space?test#test", accept = TRUE),
    list(input = "fs:/hello.eth", accept = TRUE),
    list(input = "a:b#", accept = TRUE),
    list(input = "scheme:example.com", accept = TRUE),
    list(input = "scheme:example.com/path", accept = TRUE),
    list(input = "scheme://username@@@@example.com", accept = FALSE),
    list(input = "foo://///////bar.com/", accept = TRUE)
  )
  inputs <- vapply(known, function(k) k$input, character(1L))
  want <- vapply(known, function(k) k$accept, logical(1L))
  res <- .rfc3986_generic_uri_ok(inputs)
  expect_identical(
    as.logical(res$ok), want,
    info = paste(inputs, collapse = " | ")
  )
})

test_that("directly-written non-ASCII is tolerated and flagged, not rejected", {
  res <- .rfc3986_generic_uri_ok("http://exämple.com/")
  expect_true(isTRUE(res$ok))
  expect_identical(res$diagnostic, "unicode-outside-rfc3986-uri")

  # a pure-ASCII accept carries NO diagnostic
  ascii <- .rfc3986_generic_uri_ok("http://example.com/")
  expect_true(isTRUE(ascii$ok))
  expect_true(is.na(ascii$diagnostic))

  # the tolerance does NOT relax the ASCII grammar: a bad triplet still FAILS
  # even with non-ASCII present, and a reject is never flagged.
  bad <- .rfc3986_generic_uri_ok("http://a%2 b/ä")
  expect_false(isTRUE(bad$ok))
  expect_true(is.na(bad$diagnostic))
})

test_that("the gate is a pure function, independent of curl/WHATWG leniency", {
  # curl / WHATWG accept-and-escape scheme://username@@@@example.com (emitting
  # invalid-credentials); the RFC generic gate REJECTS it (repeated raw @ in the
  # authority). This asserts the verdict WITHOUT any backend call.
  res <- .rfc3986_generic_uri_ok("scheme://username@@@@example.com")
  expect_false(isTRUE(res$ok))

  # vectorized + pure: same inputs, same verdicts, no side effects.
  vec <- .rfc3986_generic_uri_ok(
    c("http://[::1]:8080/", "http://[::1/", NA_character_)
  )
  expect_identical(vec$ok, c(TRUE, FALSE, NA))
})
