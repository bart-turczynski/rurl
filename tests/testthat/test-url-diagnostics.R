# Tests for the diagnostics + host_type infrastructure (RURL-csdrxdoj, epic
# RURL-eqzkkohm; PRD §6.2, §6.3, §7). T2 ships the plumbing and the empty
# companion-helper surface: with no emitter wired yet the helpers return the
# documented empty shapes, and url_standard = NULL adds no new output anywhere.
# The host (RURL-luwvkwhd) and path (RURL-gjltzwmp / RURL-bbmuehsx) tickets add
# the emissions and assert token presence against this same helper contract.

# --- Vocabulary: single source of truth, matching PRD §7 exactly -------------

test_that("the diagnostics vocabulary matches the PRD §7 table verbatim", {
  # Authoritative names; these supersede the research-doc drafts. Order is not
  # semantically meaningful, so compare as sets.
  expect_setequal(
    rurl:::.URL_DIAGNOSTICS,
    c(
      "ipv4-number-form", "ipv4-non-dotted", "ipv4-short-form",
      "ipv4-non-decimal", "ipv4-octal", "ipv4-leading-zero",
      "ipv4-out-of-range", "encoded-dot-segment", "encoded-reserved-path-byte"
    )
  )
  # No duplicates, no stray whitespace.
  expect_identical(anyDuplicated(rurl:::.URL_DIAGNOSTICS), 0L)
  expect_false(any(grepl("\\s", rurl:::.URL_DIAGNOSTICS)))
  # The superseded research-doc drafts must NOT leak into the vocabulary.
  expect_false(any(
    c("non-decimal-ipv4", "ambiguous-octet", "decoded-reserved") %in%
      rurl:::.URL_DIAGNOSTICS
  ))
})

test_that("the host_type vocabulary matches PRD §6.3", {
  expect_setequal(
    rurl:::.HOST_TYPES,
    c("domain", "ipv4", "ipv6", "reg-name", "missing")
  )
})

# --- Accumulator ------------------------------------------------------------

test_that(".diag_new builds a length-n list of empty character vectors", {
  expect_identical(rurl:::.diag_new(0L), list())
  acc <- rurl:::.diag_new(3L)
  expect_length(acc, 3L)
  expect_true(all(vapply(acc, identical, logical(1), character(0))))
})

test_that(".diag_add appends a token to the masked rows only", {
  acc <- rurl:::.diag_new(3L)
  acc <- rurl:::.diag_add(acc, c(TRUE, FALSE, TRUE), "ipv4-octal")
  expect_identical(acc[[1L]], "ipv4-octal")
  expect_identical(acc[[2L]], character(0))
  expect_identical(acc[[3L]], "ipv4-octal")
  # A second token accumulates (a URL can carry several).
  acc <- rurl:::.diag_add(acc, c(TRUE, FALSE, FALSE), "ipv4-leading-zero")
  expect_identical(acc[[1L]], c("ipv4-octal", "ipv4-leading-zero"))
  # NA mask positions are treated as FALSE, not appended.
  acc <- rurl:::.diag_add(acc, c(NA, NA, NA), "ipv4-non-dotted")
  expect_identical(acc[[3L]], "ipv4-octal")
})

test_that(".diag_add rejects tokens outside the vocabulary", {
  acc <- rurl:::.diag_new(1L)
  expect_error(
    rurl:::.diag_add(acc, TRUE, "not-a-real-token"),
    "diagnostic token must be one of"
  )
  expect_error(
    rurl:::.diag_add(acc, TRUE, c("ipv4-octal", "ipv4-non-dotted")),
    "diagnostic token must be one of"
  )
})

# --- get_host_type() empty surface (T2) -------------------------------------

test_that("get_host_type is NA-equivalent with no selector", {
  u <- c("http://example.com/", "http://2130706433/", "not-a-url")
  res <- get_host_type(u)
  expect_type(res, "character")
  expect_length(res, length(u))
  expect_true(all(is.na(res)))
  expect_identical(get_host_type(u, url_standard = NULL), res)
})

test_that("get_host_type classifies per selector (host model, RURL-luwvkwhd)", {
  # The host model fills the T2 surface: host_type is an (host, url_standard)
  # function. Full table coverage lives in test-url-standard-host.R; this pins
  # the helper wiring returns one token per URL, not NA, under a selector.
  u <- c("http://example.com/", "http://2130706433/")
  expect_identical(
    get_host_type(u, url_standard = "rfc3986"), c("domain", "reg-name")
  )
  expect_identical(
    get_host_type(u, url_standard = "whatwg"), c("domain", "ipv4")
  )
})

test_that("get_host_type validates input and length-0", {
  expect_identical(get_host_type(character(0)), character(0))
  expect_error(get_host_type(123), "must be a character vector")
  # url_standard is the last (2nd positional) argument.
  expect_error(get_host_type("http://ex.com/", "bogus"), "url_standard must be")
})

# --- get_url_diagnostics() empty surface (T2) -------------------------------

test_that("get_url_diagnostics returns a bare vector for a length-1 url", {
  res <- get_url_diagnostics("http://example.com/", url_standard = "rfc3986")
  expect_type(res, "character")
  expect_identical(res, character(0))
  # No selector: also empty.
  expect_identical(get_url_diagnostics("http://2130706433/"), character(0))
})

test_that("get_url_diagnostics returns a length-n list for a vector url", {
  u <- c("http://example.com/", "http://2130706433/", "not-a-url")
  res <- get_url_diagnostics(u, url_standard = "whatwg")
  expect_type(res, "list")
  expect_length(res, length(u))
  # A clean host and an unparseable input carry no tokens; the numeric host
  # does (full token tables live in test-url-standard-host.R).
  expect_identical(res[[1L]], character(0))
  expect_setequal(res[[2L]], c("ipv4-number-form", "ipv4-non-dotted"))
  expect_identical(res[[3L]], character(0))
  # NULL selector: every element is empty (no metadata surface without one).
  null_res <- get_url_diagnostics(u, url_standard = NULL)
  expect_true(all(vapply(null_res, identical, logical(1), character(0))))
})

test_that("get_url_diagnostics handles length-0 and validates input", {
  expect_identical(get_url_diagnostics(character(0)), list())
  expect_identical(
    get_url_diagnostics(character(0), url_standard = "rfc3986"), list()
  )
  expect_error(get_url_diagnostics(list("x")), "must be a character vector")
  expect_error(
    get_url_diagnostics("http://ex.com/", "bogus"), "url_standard must be"
  )
})

# --- No new output shape anywhere (AC #1 corollary) -------------------------

test_that("the helpers add no columns/fields to the parse result", {
  # Exercising the companion helpers must not perturb the parse result shape:
  # metadata lives ONLY on the helpers, never on safe_parse_urls()/
  # safe_parse_url() (PRD §6.3). Guards against a future emitter leaking a
  # column back into the engine.
  cols_before <- names(safe_parse_urls("http://ex.com/"))
  invisible(get_host_type("http://ex.com/", url_standard = "whatwg"))
  invisible(get_url_diagnostics("http://ex.com/", url_standard = "whatwg"))
  cols_after <- names(safe_parse_urls("http://ex.com/"))
  expect_identical(cols_after, cols_before)

  fields <- names(safe_parse_url("http://ex.com/", url_standard = "whatwg"))
  expect_false(any(c("host_type", "diagnostics") %in% fields))
})
