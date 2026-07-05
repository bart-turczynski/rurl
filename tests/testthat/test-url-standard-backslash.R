# Tests for the WHATWG literal backslash-as-slash recognition vertical slice
# (RURL-ledntyab, epic RURL-uyjheruh; PRD v2 D2, §5.2). Under
# url_standard = "whatwg" and only for schemes in .WHATWG_SPECIAL_SCHEMES
# (http/https/ftp -- NOT ftps), a literal "\" is treated identically to "/"
# wherever the WHATWG state machine checks for a slash: the scheme-relative
# "//" marker, the authority/path boundary, and path-segment separators.
# rfc3986, no selector, and any non-special scheme leave "\" fully inert.

# --- WHATWG + special scheme: backslash recognized as slash ------------------

test_that("whatwg recognizes a colon+backslash scheme-relative marker", {
  expect_identical(
    get_clean_url("http:\\\\host\\path", url_standard = "whatwg"),
    "http://host/path"
  )
})

test_that("whatwg recognizes a SINGLE colon+backslash marker (ticket AC)", {
  # The ticket's own acceptance case is "http:\host\path" -- ONE backslash
  # right after the colon (not two). Built via paste0() rather than a string
  # literal to avoid any ambiguity about how many backslash bytes an R string
  # escape actually produces.
  single_backslash <- paste0("http:", "\\", "host", "\\", "path")
  expect_identical(
    get_clean_url(single_backslash, url_standard = "whatwg"),
    "http://host/path"
  )
  expect_identical(
    get_url_diagnostics(single_backslash, url_standard = "whatwg"),
    "invalid-reverse-solidus"
  )
})

test_that("whatwg recognizes a backslash at the authority/path boundary", {
  expect_identical(
    get_clean_url("http://host\\path", url_standard = "whatwg"),
    "http://host/path"
  )
})

test_that("whatwg recognizes backslash path-segment separators", {
  expect_identical(
    get_clean_url("http://host/a\\b\\c", url_standard = "whatwg"),
    "http://host/a/b/c"
  )
})

test_that("whatwg backslash recognition applies to https and ftp too", {
  expect_identical(
    get_clean_url("https://host\\path", url_standard = "whatwg"),
    "https://host/path"
  )
  expect_identical(
    get_clean_url("ftp://host\\path", url_standard = "whatwg"),
    "ftp://host/path"
  )
})

# --- ftps is not WHATWG-special: backslash stays inert ------------------------

test_that("ftps backslash stays inert even under whatwg", {
  no_selector <- safe_parse_url("ftps://host\\path")
  whatwg <- safe_parse_url("ftps://host\\path", url_standard = "whatwg")
  expect_identical(whatwg, no_selector)
})

test_that("ftps backslash stays inert under rfc3986", {
  no_selector <- safe_parse_url("ftps://host\\path")
  rfc <- safe_parse_url("ftps://host\\path", url_standard = "rfc3986")
  expect_identical(rfc, no_selector)
})

# --- rfc3986 / no selector: byte-for-byte unchanged baseline ------------------

test_that("rfc3986 leaves backslash fully inert (byte-for-byte baseline)", {
  single_backslash <- paste0("http:", "\\", "host", "\\", "path")
  urls <- c(
    single_backslash,
    "http:\\\\host\\path", "http://host\\path", "http://host/a\\b\\c"
  )
  no_selector <- lapply(urls, safe_parse_url)
  rfc <- lapply(urls, safe_parse_url, url_standard = "rfc3986")
  expect_identical(rfc, no_selector)
})

test_that("no url_standard leaves backslash fully inert (today's baseline)", {
  # A colon+backslash "scheme-relative" form and a bare backslash after the
  # authority are not host-shaped once "\" is inert, so today's pipeline
  # rejects/mis-parses them exactly as it always has -- this pins that this
  # ticket introduces NO behavior change when no selector is given.
  expect_identical(
    safe_parse_url("http://host\\path"),
    safe_parse_url("http://host\\path", url_standard = NULL)
  )
  expect_identical(
    get_clean_url("http://host/a\\b\\c"),
    get_clean_url("http://host/a\\b\\c", url_standard = NULL)
  )
})

# --- %5C is never treated as a separator (recognition, not decoding) --------

test_that("percent-encoded backslash is never treated as a separator", {
  expect_identical(
    get_clean_url("http://host/a%5Cb", url_standard = "whatwg"),
    "http://host/a%5Cb"
  )
  expect_identical(
    get_url_diagnostics("http://host/a%5Cb", url_standard = "whatwg"),
    character(0)
  )
  # Mixed literal + encoded: only the literal one is a separator.
  expect_identical(
    get_clean_url("http://host/a%5Cb\\c", url_standard = "whatwg"),
    "http://host/a%5Cb/c"
  )
})

# --- Query and fragment are never reinterpreted -------------------------------

test_that("backslashes in query/fragment are never reinterpreted", {
  u <- "http://host/path?q=a\\b#frag\\c"
  res <- safe_parse_url(u, url_standard = "whatwg")
  expect_identical(res$query, "q=a\\b")
  expect_identical(res$fragment, "frag\\c")
  expect_identical(
    get_url_diagnostics(u, url_standard = "whatwg"), character(0)
  )
})

# --- invalid-reverse-solidus diagnostic ---------------------------------------

test_that("invalid-reverse-solidus fires only on an actual rewrite", {
  expect_identical(
    get_url_diagnostics("http://host\\path", url_standard = "whatwg"),
    "invalid-reverse-solidus"
  )
  expect_identical(
    get_url_diagnostics("http:\\\\host\\path", url_standard = "whatwg"),
    "invalid-reverse-solidus"
  )
  expect_identical(
    get_url_diagnostics("http://host/a\\b\\c", url_standard = "whatwg"),
    "invalid-reverse-solidus"
  )
})

test_that("invalid-reverse-solidus does not fire when backslash is inert", {
  # ftps: not a WHATWG-special scheme.
  expect_identical(
    get_url_diagnostics("ftps://host\\path", url_standard = "whatwg"),
    character(0)
  )
  # rfc3986: backslash recognition is a whatwg-only axis.
  expect_identical(
    get_url_diagnostics("http://host\\path", url_standard = "rfc3986"),
    character(0)
  )
  # No selector at all.
  expect_identical(
    get_url_diagnostics("http://host\\path"), character(0)
  )
})

test_that("invalid-reverse-solidus does not fire on mere backslash presence", {
  # No backslash anywhere: no diagnostic.
  expect_identical(
    get_url_diagnostics("http://host/path", url_standard = "whatwg"),
    character(0)
  )
  # A percent-encoded backslash alone is not a rewrite.
  expect_identical(
    get_url_diagnostics("http://host/a%5Cb", url_standard = "whatwg"),
    character(0)
  )
})

# --- No widening of safe_parse_url()/safe_parse_urls() output shape ----------

test_that("backslash recognition adds no new columns/fields", {
  cols_before <- names(safe_parse_urls("http://host/path"))
  invisible(safe_parse_urls(
    "http://host\\path", url_standard = "whatwg"
  ))
  cols_after <- names(safe_parse_urls("http://host/path"))
  expect_identical(cols_after, cols_before)

  fields_before <- names(safe_parse_url("http://host/path"))
  fields_after <- names(
    safe_parse_url("http://host\\path", url_standard = "whatwg")
  )
  expect_identical(fields_after, fields_before)
})

# --- Cache correctness: switching url_standard on the same string ------------

test_that("switching url_standard on one url never returns a stale result", {
  rurl_clear_caches()
  u <- "http://host\\path"
  expect_identical(
    get_clean_url(u, url_standard = "whatwg"), "http://host/path"
  )
  expect_identical(get_clean_url(u, url_standard = "rfc3986"), NA_character_)
  expect_identical(
    get_clean_url(u, url_standard = "whatwg"), "http://host/path"
  )
})
