# ADR 0012 Layer 4a (RURL-sxssynfu): the INDEPENDENT RFC 3986 generic-URI
# grammar gate (`.rfc3986_generic_uri_ok`) and its hand-authored ABNF fixture
# corpus -- the NORMATIVE ORACLE for the new RFC-general branch (D1). The gate
# is independent of the parse engine: a permissive component splitter
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

test_that("the gate is a pure function, independent of parser leniency", {
  # The web route / WHATWG accept-and-escape scheme://username@@@@example.com
  # (emitting
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

# --- the gate binds by STANDARD, not by route (RURL-qrfrvmkg) ----------------
#
# The gate above is the grammar; these pin WHEN it binds. Until this unit it
# bound only where rurl owned the parser -- the RFC 8089 `file:` overlay and the
# general-routed rows -- so `file://C|/x` errored while `http://a|b/` parsed,
# with '|' admitted by no RFC 3986 production either way. Selecting a standard
# now selects its grammar for every row, whichever parser handles it.

test_that("rfc3986 rejects a forbidden ASCII byte on EVERY route", {
  # Same offending character ('|'), four different routes through Stage A:
  # the web route (http authority), the RFC 8089 `file:` overlay, the
  # path-rootless
  # slice (`http:` with no "//"), and a userinfo-bearing authority. All four
  # must reject identically -- the route must not decide the verdict.
  inputs <- c(
    "http://a|b/", "file://C|/x", "http:ex.com/a|b", "http://u|v@e.com/"
  )
  res <- safe_parse_urls(inputs, url_standard = "rfc3986")
  expect_identical(res$parse_status, rep("error", length(inputs)))
  expect_true(all(is.na(res$clean_url)))

  # ...and the grammar itself agrees on every one, so this is the gate binding,
  # not four coincidences.
  expect_false(any(.rfc3986_generic_uri_ok(inputs)$ok))
})

test_that("the uniform gate leaves whatwg and the NULL selector untouched", {
  inputs <- c("http://a|b/", "file://C|/x", "http://u|v@e.com/")
  # whatwg has its own host model and file state machine; the RFC grammar gate
  # must not reach them. `file://C|/x` in particular STAYS parseable there.
  whatwg <- safe_parse_urls(inputs, url_standard = "whatwg")
  expect_identical(whatwg$parse_status[2L], "ok")
  expect_identical(whatwg$clean_url[2L], "file:///C:/x")
  # the no-selector default is the historical baseline and is unchanged.
  base <- safe_parse_urls(inputs)
  expect_identical(base$parse_status[1L], "warning-no-tld")
  expect_identical(base$clean_url[1L], "http://a|b/")
})

test_that("the uniform gate adds only SYNTAX rejections, not policy ones", {
  # ADR 0002 / ADR 0011: a directly-written non-ASCII host is rurl's ONE
  # tolerated extension and the gate FLAGS it rather than failing it, so
  # binding the gate uniformly must not turn that diagnostic into an error.
  res <- safe_parse_url("http://exämple.com/", url_standard = "rfc3986")
  expect_identical(res$parse_status, "ok")
  expect_identical(res$clean_url, "http://exämple.com/")
  expect_identical(
    .rfc3986_generic_uri_ok("http://exämple.com/")$diagnostic,
    "unicode-outside-rfc3986-uri"
  )

  # A reg-name that WHATWG's forbidden-domain-code-point rule rejects but RFC
  # 3986's `reg-name` production admits (pct-encoded octets) stays accepted:
  # the gate enforces the grammar, it does not import WHATWG's host rules.
  expect_false(
    get_parse_status("http://a%7Cb/", url_standard = "rfc3986") == "error"
  )
  # scheme inference is the scheme_policy axis (ADR 0010), NOT the grammar's:
  # a scheme-less host-shaped input must not start failing RFC 3986's
  # mandatory `scheme ":"` just because a standard was selected.
  expect_identical(
    get_parse_status("example.com/path", url_standard = "rfc3986"), "ok"
  )
})
