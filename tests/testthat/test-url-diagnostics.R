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
      "ipv4-out-of-range", "encoded-dot-segment", "encoded-reserved-path-byte",
      "explicit-default-port", "non-default-port", "invalid-reverse-solidus",
      "control-char-stripped", "host-charset-shimmed",
      "domain-label-too-long", "domain-name-too-long", "domain-empty-label",
      "domain-hyphen-violation", "domain-std3-violation",
      # Layer 5 SELECTED diagnostics (ADR 0012 D5, RURL-izsouyxs).
      "invalid-URL-unit", "invalid-credentials",
      "unicode-outside-rfc3986-uri", "transform-skipped-ineligible-scheme",
      "ws-fragment-forbidden", "ws-userinfo-forbidden",
      "mailto-fragment-discouraged", "tel-missing-phone-context",
      "data-missing-comma", "file-non-absolute-path",
      "file-forbidden-component"
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

# --- Layer 5 SELECTED diagnostics (ADR 0012 D5, RURL-izsouyxs) --------------
# SELECTED facts, never a conformance oracle: absence of a token never implies
# conformance. Two of them -- invalid-credentials and invalid-URL-unit -- are
# WHATWG-GENERIC and gate on the interpreting standard (url_standard =
# "whatwg"), not the acceptance axis, so they also fire on the `web` accept path
# (RURL-sgjzbqzk). Every OTHER L5 fact stays general-only. The true DEFAULT
# (url_standard = NULL) stays silent for ALL of them -- that is the D4
# byte-identity / CRAN lock.

gd <- function(u, standard = "whatwg") {
  get_url_diagnostics(u, url_standard = standard, scheme_acceptance = "general")
}

# The general-only L5 facts (everything except the two WHATWG-generic ones).
l5_general_only <- c(
  "unicode-outside-rfc3986-uri", "transform-skipped-ineligible-scheme",
  "ws-fragment-forbidden", "ws-userinfo-forbidden",
  "mailto-fragment-discouraged", "tel-missing-phone-context",
  "data-missing-comma", "file-non-absolute-path", "file-forbidden-component"
)
l5_whatwg_generic <- c("invalid-credentials", "invalid-URL-unit")

test_that("the general-only L5 facts stay dormant under web acceptance", {
  # An input that fires several under `general` fires none of the general-only
  # facts under the default `web` acceptance, even with url_standard = "whatwg".
  web <- get_url_diagnostics("mailto:a@b.com#f", url_standard = "whatwg")
  expect_length(intersect(web, l5_general_only), 0L)
  web2 <- get_url_diagnostics("http://user@ex.com/%zz", url_standard = "whatwg")
  expect_length(intersect(web2, l5_general_only), 0L)
})

test_that("WHATWG-generic L5 facts fire under web + whatwg (RURL-sgjzbqzk)", {
  # invalid-credentials / invalid-URL-unit gate on the interpreting standard,
  # not acceptance -- so they fire on the pre-existing web/libcurl path under
  # url_standard = "whatwg" (no scheme_acceptance = "general" needed).
  web2 <- get_url_diagnostics("http://user@ex.com/%zz", url_standard = "whatwg")
  expect_true("invalid-credentials" %in% web2)
  expect_true("invalid-URL-unit" %in% web2)
  # A clean web+whatwg URL fires neither.
  clean <- get_url_diagnostics("http://ex.com/clean", url_standard = "whatwg")
  expect_length(intersect(clean, l5_whatwg_generic), 0L)
})

test_that("the true default (url_standard = NULL) stays silent for all L5", {
  # The D4 byte-identity / CRAN lock: the DEFAULT combination (web acceptance +
  # url_standard = NULL) fires NONE of the L5 facts, generic ones included.
  l5 <- c(l5_general_only, l5_whatwg_generic)
  def <- get_url_diagnostics("http://user@ex.com/%zz")
  expect_length(intersect(def, l5), 0L)
})

test_that("transform-skipped-ineligible-scheme fires on non-HTTP(S) rows", {
  # Non-HTTP(S) schemes are ineligible for the SEO/semantic transforms (D2).
  expect_true(
    "transform-skipped-ineligible-scheme" %in% gd("ftp://ex.com/x")
  )
  expect_true(
    "transform-skipped-ineligible-scheme" %in% gd("mailto:a@b.com")
  )
  # ...and never on an eligible HTTP(S) row.
  expect_false(
    "transform-skipped-ineligible-scheme" %in% gd("http://ex.com/clean")
  )
  expect_false(
    "transform-skipped-ineligible-scheme" %in% gd("https://ex.com/clean")
  )
})

test_that("invalid-credentials fires for ANY userinfo under whatwg general", {
  expect_true("invalid-credentials" %in% gd("http://user@ex.com/"))
  expect_true("invalid-credentials" %in% gd("http://u:p@ex.com/"))
  expect_false("invalid-credentials" %in% gd("http://ex.com/"))
  # rfc3986 posture routes raw `@` to its own generic-grammar gate; the WHATWG
  # credentials fact does not fire there.
  expect_false("invalid-credentials" %in% gd("http://user@ex.com/", "rfc3986"))
})

test_that("invalid-URL-unit fires on malformed %-escapes and non-URL cps", {
  expect_true("invalid-URL-unit" %in% gd("http://ex.com/%zz"))
  expect_true("invalid-URL-unit" %in% gd("http://ex.com/a%2Gb"))
  expect_true("invalid-URL-unit" %in% gd("http://ex.com/a b"))
  expect_true("invalid-URL-unit" %in% gd("http://ex.com/a<b>c"))
  # A well-formed percent triplet and a clean path do NOT fire it.
  expect_false("invalid-URL-unit" %in% gd("http://ex.com/a%20b"))
  expect_false("invalid-URL-unit" %in% gd("http://ex.com/clean"))
  # A backslash is `invalid-reverse-solidus`, not `invalid-URL-unit`.
  expect_false("invalid-URL-unit" %in% gd("http://ex.com/a\\b"))
})

test_that("unicode-outside-rfc3986-uri fires on the sole D1 tolerance", {
  expect_true(
    "unicode-outside-rfc3986-uri" %in% gd("http://ex.com/ä", "rfc3986")
  )
  # ASCII-only rfc3986 input does not fire it; nor does a whatwg parse.
  expect_false(
    "unicode-outside-rfc3986-uri" %in% gd("http://ex.com/x", "rfc3986")
  )
  expect_false(
    "unicode-outside-rfc3986-uri" %in% gd("http://ex.com/ä", "whatwg")
  )
})

test_that("ws/wss fragment and userinfo are two separate facts", {
  frag_only <- gd("ws://host/#f")
  expect_true("ws-fragment-forbidden" %in% frag_only)
  expect_false("ws-userinfo-forbidden" %in% frag_only)

  user_only <- gd("wss://user@host/")
  expect_true("ws-userinfo-forbidden" %in% user_only)
  expect_false("ws-fragment-forbidden" %in% user_only)
  # WHATWG's generic credentials fact ALSO fires for the ws userinfo.
  expect_true("invalid-credentials" %in% user_only)

  both <- gd("ws://user@host/#frag")
  expect_true(all(
    c("ws-fragment-forbidden", "ws-userinfo-forbidden") %in% both
  ))
  # A clean ws URL carries neither ws fact.
  clean <- gd("wss://host/path")
  expect_false(any(
    c("ws-fragment-forbidden", "ws-userinfo-forbidden") %in% clean
  ))
})

test_that("mailto fragment is a selected fact (RFC 6068)", {
  expect_true(
    "mailto-fragment-discouraged" %in% gd("mailto:a@b.com#section")
  )
  expect_false("mailto-fragment-discouraged" %in% gd("mailto:a@b.com"))
})

test_that("data missing comma is a selected fact (RFC 2397)", {
  expect_true("data-missing-comma" %in% gd("data:text/plain"))
  expect_false("data-missing-comma" %in% gd("data:text/plain,hi"))
  expect_false("data-missing-comma" %in% gd("data:,"))
})

test_that("tel missing phone-context is a selected fact (RFC 3966)", {
  # A local number (no leading "+") without ;phone-context= fires.
  expect_true("tel-missing-phone-context" %in% gd("tel:555-1234"))
  # A global number (leading "+") is exempt.
  expect_false("tel-missing-phone-context" %in% gd("tel:+1-800-555-1234"))
  # A local number WITH ;phone-context= is exempt.
  expect_false(
    "tel-missing-phone-context" %in%
      gd("tel:555-1234;phone-context=+1-555")
  )
})

test_that("file rfc-syntax shape facts fire (RFC 8089)", {
  # Non-absolute (rootless) path.
  expect_true(
    "file-non-absolute-path" %in% gd("file:relative/path", "rfc3986")
  )
  expect_false(
    "file-non-absolute-path" %in% gd("file:/etc/hosts", "rfc3986")
  )
  # Query/fragment/port outside RFC 8089's grammar.
  expect_true(
    "file-forbidden-component" %in% gd("file:/p?q=1", "rfc3986")
  )
  expect_true(
    "file-forbidden-component" %in% gd("file:/p#frag", "rfc3986")
  )
  expect_true(
    "file-forbidden-component" %in% gd("file://h:8080/p", "rfc3986")
  )
  expect_false(
    "file-forbidden-component" %in% gd("file:/ok/path", "rfc3986")
  )
})
