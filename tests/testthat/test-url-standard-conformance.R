# Conformance fixtures + WPT reference corpus + regression assertions
# (RURL-cuqafsif, epic RURL-eqzkkohm; PRD §9). The spec-conformance backstop:
# every row below is checked against RFC 3986 grammar/normalization rules or a
# pinned WHATWG reference (the web-platform-tests `url/resources/
# urltestdata.json` corpus, https://github.com/web-platform-tests/wpt), not
# just against rurl's own current output. WPT-sourced rows record the exact
# `input` string from that file in `source_reference` so a future re-pin can
# re-locate them.
#
# Percent-triplet hex case is canonicalized (uppercased) by rurl in both
# profiles, so fixture expectations use rurl's canonical spelling rather than
# a WPT/browser's byte-for-byte href (PRD §9.2 / Open Q4 note) -- this is
# documented per-row in `notes` where it applies.
#
# Most RFC-side rows are derived from RFC 3986 grammar/normalization prose
# (there is no RFC equivalent of WPT's machine-checkable corpus). The
# `path-rfc-5-4-*` rows are the exception: RFC 3986 §5.4's "abnormal examples"
# table gives worked reference-resolution vectors against a base URI, and two
# of them (`/./g`, `/../g`) use an absolute-path reference, which §5.3 routes
# straight to `remove_dot_segments()` with no base merge -- so those two are
# genuine RFC-text-verbatim, base-independent dot-segment vectors, not a
# hand-derivation.

# --- Golden fixture table (PRD §9.1) ----------------------------------------

test_that("url_standard conformance fixtures match pinned expectations", {
  path <- testthat::test_path("fixtures", "url-standard-conformance.csv")
  fx <- utils::read.csv(
    path, stringsAsFactors = FALSE, colClasses = "character", na.strings = "NA"
  )

  split_tokens <- function(x) {
    if (is.na(x)) character(0) else strsplit(x, "|", fixed = TRUE)[[1]]
  }

  for (i in seq_len(nrow(fx))) {
    row <- fx[i, ]
    label <- sprintf("[%s] %s (%s): %s", row$id, row$input, row$url_standard,
      row$notes)

    expect_identical(
      get_clean_url(row$input, url_standard = row$url_standard),
      row$expected_clean_url, label = paste("clean_url", label)
    )
    expect_identical(
      get_host(row$input, url_standard = row$url_standard),
      row$expected_host, label = paste("host", label)
    )
    expect_identical(
      get_host_type(row$input, url_standard = row$url_standard),
      row$expected_host_type, label = paste("host_type", label)
    )
    expect_identical(
      get_path(row$input, url_standard = row$url_standard),
      row$expected_path, label = paste("path", label)
    )
    expect_setequal(
      get_url_diagnostics(row$input, url_standard = row$url_standard),
      split_tokens(row$expected_diagnostics)
    )
  }
})

# --- Required regression assertions (PRD §9.3) ------------------------------

eq <- function(a, b, ...) {
  identical(get_clean_url(a, ...), get_clean_url(b, ...))
}

test_that("PRD §9.3 required regression assertions hold", {
  expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
    url_standard = "whatwg"))
  expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
    url_standard = "rfc3986"))

  expect_true(eq("http://ex.com/%41%42", "http://ex.com/AB",
    url_standard = "rfc3986"))
  expect_false(eq("http://ex.com/%41%42", "http://ex.com/AB",
    url_standard = "whatwg"))

  expect_equal(get_host("http://2130706433/", url_standard = "rfc3986"),
    "2130706433")
  expect_equal(get_host("http://2130706433/", url_standard = "whatwg"),
    "127.0.0.1")

  # path accessor honors the profile percent policy directly
  expect_equal(get_path("http://ex.com/%41%42", url_standard = "rfc3986"),
    "/AB")
  expect_equal(get_path("http://ex.com/%41%42", url_standard = "whatwg"),
    "/%41%42")

  # diagnostics fire in BOTH modes for numeric-looking hosts (facts, not policy)
  expect_true("ipv4-non-dotted" %in%
    get_url_diagnostics("http://2130706433/", url_standard = "whatwg"))
  expect_true("ipv4-number-form" %in%
    get_url_diagnostics("http://2130706433/", url_standard = "rfc3986"))

  # host_type is (host, url_standard)-dependent
  expect_equal(get_host_type("http://2130706433/", url_standard = "rfc3986"),
    "reg-name")
  expect_equal(get_host_type("http://2130706433/", url_standard = "whatwg"),
    "ipv4")

  # NULL selector leaves the output shape untouched (no new columns/fields)
  expect_named(
    safe_parse_urls("http://ex.com/", url_standard = NULL),
    names(safe_parse_urls("http://ex.com/"))
  )
})

# --- get_path()/get_host() honor the selector consistently with get_clean_url
# (AC #8): spot-check the remaining accessors on a representative fixture row.

test_that("get_parse_status/domain/tld/subdomain honor the standard (AC #8)", {
  # WHATWG-fatal numeric host: every accessor reports the failure consistently.
  u <- "http://256.1.1.1/"
  expect_identical(get_parse_status(u, url_standard = "whatwg"), "error")
  expect_true(is.na(get_domain(u, url_standard = "whatwg")))
  expect_true(is.na(get_tld(u, url_standard = "whatwg")))
  expect_true(is.na(get_subdomain(u, url_standard = "whatwg")))
  # Under RFC the reg-name parses (not fatal), but a numeric-looking reg-name
  # has no valid TLD, so it carries the existing invalid-TLD warning status.
  expect_identical(
    get_parse_status(u, url_standard = "rfc3986"), "warning-invalid-tld"
  )
})
