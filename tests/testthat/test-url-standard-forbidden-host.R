# Tests for WHATWG forbidden-host/domain-code-point rejection under
# url_standard = "whatwg" (RURL-jfuqpwvh, epic RURL-moselrwp). A special-scheme
# host is a DOMAIN: WHATWG fails the host parse when domain-to-ASCII yields a
# forbidden domain code point, or when domain-to-ASCII itself fails (a
# disallowed / ignored-to-empty code point). rurl previously surfaced these as
# reg-names with `warning-no-tld`; the whatwg profile now rejects them (flips
# the ADR 0004 boundary into a governed axis, ADR 0007). rfc3986 and the default
# (NULL) keep RFC reg-name permissiveness -- an expected profile divergence.

# --- ASCII forbidden host code points: reject under whatwg --------------------

test_that("whatwg rejects a bare pipe in the host", {
  expect_identical(get_parse_status("http://a|b/", url_standard = "whatwg"),
                   "error")
  expect_true(is.na(get_clean_url("http://a|b/", url_standard = "whatwg")))
})

test_that("whatwg rejects a percent-encoded pipe (decodes to forbidden)", {
  expect_identical(get_parse_status("http://ho%7Cst/", url_standard = "whatwg"),
                   "error")
  expect_identical(get_parse_status("https://%43%7C/", url_standard = "whatwg"),
                   "error")
})

test_that("whatwg rejects DEL (U+007F) in the host", {
  expect_identical(get_parse_status("http://ho%7Fst/", url_standard = "whatwg"),
                   "error")
})

test_that("whatwg rejects a caret in the host", {
  expect_identical(get_parse_status("http://a^b/", url_standard = "whatwg"),
                   "error")
})

# --- non-ASCII UTS-46 failures: reject under whatwg ---------------------------

test_that("whatwg rejects U+FFFD / U+FFFF noncharacters in the host", {
  expect_identical(get_parse_status("https://�", url_standard = "whatwg"),
                   "error")
  expect_identical(
    get_parse_status(paste0("https://", intToUtf8(0xFFFF), "y"),
                     url_standard = "whatwg"),
    "error"
  )
})

test_that("whatwg rejects a soft-hyphen-only label (UTS-46 -> empty)", {
  expect_identical(
    get_parse_status(paste0("https://", intToUtf8(0x00AD), "/"),
                     url_standard = "whatwg"),
    "error"
  )
  expect_identical(get_parse_status("https://%C2%AD/", url_standard = "whatwg"),
                   "error")
})

# --- rfc3986 / default keep RFC reg-name permissiveness -----------------------

test_that("rfc3986 keeps forbidden-code-point hosts as reg-names", {
  # RFC 3986 has no forbidden-host-code-point concept; these are accepted
  # (flagged warning-no-tld), NOT rejected -- an intended profile divergence.
  expect_false(is.na(
    suppressWarnings(get_parse_status("http://a|b/", url_standard = "rfc3986"))
  ))
  expect_false(
    get_parse_status("http://a|b/", url_standard = "rfc3986") == "error"
  )
})

# --- no over-rejection of valid hosts ----------------------------------------

test_that("whatwg still accepts valid ASCII / IDN / punycode / IP hosts", {
  ok <- c("http://example.com/", "http://münchen.de/",
          "http://xn--mnchen-3ya.de/", "http://127.0.0.1/",
          "http://foo-bar.co.uk/", "http://sub.dom.example.com/",
          "http://Yağız.com/", "http://user:pass@example.com/",
          "http://2130706433/", "http://[::1]/")
  for (u in ok) {
    expect_false(get_parse_status(u, url_standard = "whatwg") == "error",
                 info = u)
  }
})

test_that("hyphen and underscore in the host are not forbidden", {
  expect_false(
    get_parse_status("http://a-b_c.example.com/", url_standard = "whatwg") ==
      "error"
  )
})
