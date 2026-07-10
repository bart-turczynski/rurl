# Tests for the scheme_acceptance axis (RURL-pcobmxwv, ADR 0012 D3): the
# scheme-*acceptance* knob, orthogonal to protocol_handling (presentation) and
# url_standard (interpretation). "web" (default) keeps the curated web-scheme
# allowlist; "general" admits any valid scheme token and requires an explicit
# url_standard. "general" is internal-only (Layer-2 build-order gate), so the
# axis is driven through rurl::: rather than any public signature. Also covers
# the Option B decoupling: the unsupported-scheme reject fires on the acceptance
# axis, regardless of protocol_handling.

test_that("general requires an explicit url_standard", {
  err <- expect_error(
    rurl:::.parse_options(scheme_acceptance = "general", url_standard = NULL)
  )
  expect_match(conditionMessage(err), "url_standard")
})

test_that("general composes with either governed standard", {
  for (std in c("whatwg", "rfc3986")) {
    opts <- rurl:::.parse_options(
      scheme_acceptance = "general", url_standard = std
    )
    expect_identical(opts$scheme_acceptance, "general")
    expect_identical(opts$url_standard, std)
  }
})

test_that("web is the default and composes freely with url_standard", {
  expect_identical(rurl:::.parse_options()$scheme_acceptance, "web")
  expect_identical(
    rurl:::.parse_options(scheme_acceptance = "web")$scheme_acceptance, "web"
  )
  # "web" needs no url_standard and never conflicts with one.
  for (std in list(NULL, "rfc3986", "whatwg")) {
    opts <- rurl:::.parse_options(scheme_acceptance = "web", url_standard = std)
    expect_identical(opts$scheme_acceptance, "web")
  }
})

test_that("match.arg rejects a bogus scheme_acceptance", {
  expect_error(rurl:::.parse_options(scheme_acceptance = "bogus"))
})

test_that("Stage-A cache key differs when only scheme_acceptance differs", {
  opts_web <- rurl:::.parse_options(
    scheme_acceptance = "web", url_standard = "whatwg"
  )
  opts_general <- rurl:::.parse_options(
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  key_web <- rurl:::.parse_cache_keys("http://x.com", opts_web)
  key_general <- rurl:::.parse_cache_keys("http://x.com", opts_general)
  expect_false(identical(key_web, key_general))
})

test_that("Option B: unsupported scheme errors under any protocol_handling", {
  # ws:// is outside the curated web allowlist. Under the default "web"
  # acceptance the reject fires for strip/http/https too (not only keep/none).
  for (ph in c("strip", "http", "https")) {
    d <- safe_parse_urls("ws://example.com/s", protocol_handling = ph)
    expect_identical(
      d$parse_status, "error",
      info = paste("protocol_handling =", ph)
    )
    expect_true(is.na(d$clean_url), info = paste("protocol_handling =", ph))
  }
})

test_that("Option B does not demote a supported scheme under strip", {
  d <- safe_parse_urls("http://example.com/p", protocol_handling = "strip")
  expect_identical(d$parse_status, "ok")
  expect_false(is.na(d$clean_url))
})
