# Host percent-decoding: output pins and a linear-time bound (RURL-adlwbqxn).
#
# The pins hold what the host percent path (`.web_host_percent_decode()` and
# `.web_host_pct_unreserved()`, R/parse-web.R) renders and rejects, so a change
# to how it runs cannot quietly change what it returns. The timing tests hold
# the path linear: a `%` in a 1.5e6-character host used to cost minutes.

.hpl_urls <- c(
  "http://ex%41mple.com/",
  "http://exa%6dple.com/",
  "http://a%2db.com/",
  "http://a%7eb.com/",
  "http://a%2Fb.com/",
  "http://a%zzb.com/",
  "http://a%4.com/",
  "http://example.com%/",
  "http://example.com%4/",
  "http://%%41b.com/",
  "http://caf%C3%A9.com/",
  "http://b\u00fccher%41.de/",
  "http://a%00b.com/",
  "http://%7f.com/",
  "http://a%60b.com/",
  "http://a%2fb.com/",
  "http://caf%c3%a9.com/",
  # One non-hex digit in an otherwise admissible spot: a decode that skipped
  # either digit's check would yield a byte the later gates accept.
  "http://a%4gb.com/",
  "http://a%g4b.com/"
)

.hpl_cafe <- "caf\u00e9.com"
.hpl_buecher <- "b\u00fcchera.de"

test_that("host percent output is pinned under url_standard = 'whatwg'", {
  host <- c(
    "example.com", "example.com", "a-b.com", "a~b.com",
    NA, NA, NA, NA, NA, NA, .hpl_cafe, .hpl_buecher, NA, NA,
    "a`b.com", NA, .hpl_cafe, NA, NA
  )
  status <- c(
    "ok", "ok", "ok", "warning-invalid-tld",
    "error", "error", "error", "error", "error", "error", "ok", "ok",
    "error", "error", "warning-invalid-tld", "error", "ok",
    "error", "error"
  )
  clean <- ifelse(is.na(host), NA_character_, paste0("http://", host, "/"))
  expect_identical(get_host(.hpl_urls, url_standard = "whatwg"), host)
  expect_identical(
    safe_parse_urls(.hpl_urls, url_standard = "whatwg")$parse_status, status
  )
  expect_identical(get_clean_url(.hpl_urls, url_standard = "whatwg"), clean)
})

test_that("host percent output is pinned under url_standard = 'rfc3986'", {
  # Row 12 carries a raw U+00FC, which rfc3986 rejects: reg-name admits only
  # its percent-encoded UTF-8 form (RFC 3986 sections 2, 2.5, 3.2.2; RUL-030,
  # owner ruling 2026-10-12, RURL-pntbujzc). Row 11 is its encoded twin.
  host <- c(
    "example.com", "example.com", "a-b.com", "a~b.com",
    "a%2Fb.com", NA, NA, NA, NA, NA, "caf%C3%A9.com", NA,
    "a%00b.com", "%7F.com", "a%60b.com", "a%2Fb.com", "caf%C3%A9.com", NA, NA
  )
  status <- c(
    "ok", "ok", "ok", "warning-invalid-tld",
    "warning-invalid-tld", "error", "error", "error", "error", "error",
    "ok", "error", "warning-invalid-tld", "warning-invalid-tld",
    "warning-invalid-tld", "warning-invalid-tld", "ok", "error", "error"
  )
  clean <- ifelse(is.na(host), NA_character_, paste0("http://", host, "/"))
  expect_identical(get_host(.hpl_urls, url_standard = "rfc3986"), host)
  expect_identical(
    safe_parse_urls(.hpl_urls, url_standard = "rfc3986")$parse_status, status
  )
  expect_identical(get_clean_url(.hpl_urls, url_standard = "rfc3986"), clean)
})

# --- linear time --------------------------------------------------------------

.hpl_n <- 1.5e6

# One long label ending in a single triplet, and one host made of nothing but
# triplets (`%61` is "a"), so both the plain-byte and the per-triplet cost of
# the scan are exercised at 1.5e6 characters.
.hpl_long_rows <- function() {
  c(
    paste0("http://", strrep("a", .hpl_n), "%41.example/"),
    paste0("http://", strrep("%61", .hpl_n / 3), ".example/")
  )
}

# `%41` decodes to "A", which the host then lowercases, as "ex%41mple" above.
.hpl_long_hosts <- c(
  paste0(strrep("a", .hpl_n + 1), ".example"),
  paste0(strrep("a", .hpl_n / 3), ".example")
)

.hpl_timed_host <- function(url_standard) {
  rows <- .hpl_long_rows()
  # The cap only stops a regression from running for minutes; the bound the
  # test asserts is the `expect_lt()` below.
  setTimeLimit(elapsed = 120, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)
  elapsed <- system.time(
    host <- get_host(rows, url_standard = url_standard)
  )[["elapsed"]]
  list(host = host, elapsed = elapsed)
}

test_that("a `%` in a 1.5e6-character host parses in seconds under whatwg", {
  skip_on_cran() # a few seconds and tens of MB; the defect is not R-bound
  res <- .hpl_timed_host("whatwg")
  expect_identical(res$host, .hpl_long_hosts)
  expect_lt(res$elapsed, 30)
})

test_that("a `%` in a 1.5e6-character host parses in seconds under rfc3986", {
  skip_on_cran() # a few seconds and tens of MB; the defect is not R-bound
  # `%41` and `%61` are unreserved, so RFC 3986 S6.2.2.2 decodes them too.
  res <- .hpl_timed_host("rfc3986")
  expect_identical(res$host, .hpl_long_hosts)
  expect_lt(res$elapsed, 30)
})
