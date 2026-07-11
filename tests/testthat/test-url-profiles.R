# Tests for the public `profile` axis and the url_profile() inspector
# (ADR 0012 Layer 6b, RURL-djmgzjmr).

test_that("url_profile() resolves each bundle to its exact knob set", {
  expect_identical(
    url_profile("browser"),
    list(
      url_standard = "whatwg",
      scheme_acceptance = "general",
      scheme_policy = "infer",
      scheme_relative_handling = "http",
      fixup_posture = "browser",
      customized = FALSE
    )
  )
  expect_identical(
    url_profile("whatwg"),
    list(
      url_standard = "whatwg",
      scheme_acceptance = "general",
      scheme_policy = "require",
      scheme_relative_handling = "error",
      customized = FALSE
    )
  )
  expect_identical(
    url_profile("rfc-syntax"),
    list(
      url_standard = "rfc3986",
      scheme_acceptance = "general",
      scheme_policy = "require",
      scheme_relative_handling = "keep",
      path_normalization = "none",
      case_handling = "keep",
      path_identity = "none",
      customized = FALSE
    )
  )
  expect_identical(
    url_profile("seo"),
    list(
      scheme_acceptance = "web",
      protocol_handling = "https",
      www_handling = "strip",
      trailing_slash_handling = "strip",
      index_page_handling = "strip",
      query_handling = "filter",
      customized = FALSE
    )
  )
})

test_that("canonical is an alias resolving identically to seo", {
  expect_identical(url_profile("canonical"), url_profile("seo"))
})

test_that("explicit overrides win and flag customized", {
  res <- url_profile("browser", scheme_policy = "require")
  expect_identical(res$scheme_policy, "require")
  expect_true(res$customized)
  # a pure bundle is not customized
  expect_false(url_profile("browser")$customized)
})

test_that("url_profile() rejects unknown profiles and overrides", {
  expect_error(url_profile("nope"), "profile must be NULL")
  expect_error(url_profile(), "requires a profile name")
  expect_error(
    url_profile("browser", not_a_knob = "x"),
    "does not recognize override"
  )
})

test_that("profile = NULL is byte-identical to omitting the argument", {
  url <- "http://www.Example.com/index.html?utm_source=x&id=1"
  expect_identical(
    safe_parse_url(url, profile = NULL),
    safe_parse_url(url)
  )
  expect_identical(
    get_clean_url(url, profile = NULL),
    get_clean_url(url)
  )
  expect_identical(
    safe_parse_urls(c(url, "example.com"), profile = NULL),
    safe_parse_urls(c(url, "example.com"))
  )
})

test_that("browser profile repairs and http-prepends", {
  # `;` -> `:` repair via the bounded fixer
  fixed <- safe_parse_url("http;//example.com", profile = "browser")
  expect_identical(fixed$parse_status, "ok")
  expect_identical(fixed$scheme, "http")
  expect_identical(fixed$host, "example.com")
  # scheme-less host-shaped input gains http (scheme_policy = "infer")
  prepended <- safe_parse_url("example.com", profile = "browser")
  expect_identical(prepended$parse_status, "ok")
  expect_identical(prepended$scheme, "http")
})

test_that("whatwg profile rejects scheme-less input", {
  # safe_parse_url() returns NULL for a rejected scalar input (existing scalar
  # contract), so the rejection surfaces as a NULL result.
  expect_null(safe_parse_url("example.com", profile = "whatwg"))
  # in the vectorized wrapper the same input becomes an "error" row.
  res <- safe_parse_urls("example.com", profile = "whatwg")
  expect_identical(res$parse_status, "error")
})

test_that("rfc-syntax profile parses without normalizing", {
  res <- safe_parse_url("http://EXAMPLE.com/a/../b", profile = "rfc-syntax")
  expect_identical(res$parse_status, "ok")
  # case preserved (case_handling = "keep")
  expect_true(grepl("EXAMPLE", res$clean_url, fixed = TRUE))
  # dot-segments preserved (path_normalization = "none")
  expect_true(grepl("/a/../b", res$clean_url, fixed = TRUE))
})

test_that("rfc-syntax authorized exception is profile-only", {
  # A DIRECT (no-profile) call combining rfc3986 with path_normalization="none"
  # must STILL be rejected exactly as today (ADR 0007 conflict matrix intact).
  expect_error(
    safe_parse_url(
      "http://example.com/a/../b",
      url_standard = "rfc3986",
      path_normalization = "none"
    ),
    "governs"
  )
})

test_that("seo profile cleans origin URLs", {
  out <- get_clean_url(
    "http://www.example.com/index.html?utm_source=x",
    profile = "seo"
  )
  # seo strips the trailing slash (trailing_slash_handling = "strip")
  expect_identical(out, "https://example.com")
})

# --- profile across the canonical_join() `...` seam (RURL-cujzicqf) ----------

test_that("canonical_join() forwards a profile bundle through `...`", {
  A <- data.frame(
    URL = "http://www.Example.com/Page/index.html?utm_source=x",
    ValA = 1L, stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "https://example.com/Page",
    ValB = 2L, stringsAsFactors = FALSE
  )
  # seo canonicalization (https + strip www / index / trailing slash / tracking
  # params) makes the two rows share a key and join.
  joined <- canonical_join(A, B, profile = "seo")
  expect_identical(nrow(joined), 1L)
  expect_identical(joined$JoinKey, "https://example.com/Page")
})

test_that("canonical_join() skips the conflict matrix on the profile path", {
  A <- data.frame(URL = "http://ex.com/a", ValA = 1L, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://ex.com/a", ValB = 2L, stringsAsFactors = FALSE)

  # A profile authorizes its own combination, so an explicit governed knob that
  # would conflict under a DIRECT url_standard call is accepted here (iron
  # rule), exactly as in safe_parse_url(). This must NOT error.
  expect_silent(
    canonical_join(
      A, B,
      profile = "seo", url_standard = "whatwg", path_normalization = "none"
    )
  )
  # Sanity: the same combination WITHOUT a profile still fails fast.
  expect_error(
    canonical_join(A, B, url_standard = "whatwg", path_normalization = "none"),
    "governs `path_normalization`"
  )
})

test_that("canonical_join() rejects an unknown profile up front", {
  A <- data.frame(URL = "http://ex.com/a", ValA = 1L, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://ex.com/a", ValB = 2L, stringsAsFactors = FALSE)
  expect_error(
    canonical_join(A, B, profile = "nope"),
    "profile must be NULL or one of"
  )
})
