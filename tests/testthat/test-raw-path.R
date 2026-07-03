# Raw-path extraction (RURL-chdrlyci): path_normalization = "none" is lossless.
#
# rurl reads the request path from the input verbatim rather than from libcurl's
# pre-normalized $path, so dot segments (and percent-encoded dot forms) survive
# to path_normalization. The only libcurl normalization replayed is percent-hex
# case (RFC 3986 section 6.2.2.1), which keeps %2F/%2f join-equivalent.

test_that("path_normalization = 'none' preserves dot segments (default)", {
  expect_equal(get_clean_url("http://ex.com/a/../b"), "http://ex.com/a/../b")
  expect_equal(get_clean_url("http://ex.com/a/./b"), "http://ex.com/a/./b")
  expect_equal(
    get_clean_url("http://ex.com/a//b/./c/../d"),
    "http://ex.com/a//b/./c/../d"
  )
  expect_equal(get_path("http://ex.com/a/../b"), "/a/../b")
})

test_that("dot_segments / both resolve literal . and .. (rurl-owned)", {
  expect_equal(
    get_clean_url("http://ex.com/a/../b", path_normalization = "dot_segments"),
    "http://ex.com/b"
  )
  expect_equal(
    get_clean_url("http://ex.com/a/./b", path_normalization = "dot_segments"),
    "http://ex.com/a/b"
  )
  expect_equal(
    get_clean_url("http://ex.com/a//b/./c/../d", path_normalization = "both"),
    "http://ex.com/a/b/d"
  )
})

test_that("percent-encoded %2e is a path byte, never a dot segment", {
  # Preserved under "none" (hex-uppercased) ...
  expect_equal(
    get_clean_url("http://ex.com/a/%2e%2e/b"),
    "http://ex.com/a/%2E%2E/b"
  )
  # ... and NOT collapsed even when dot-segment resolution is requested
  # (RFC 3986 section 5.2.4 operates on literal segments only). This kills the
  # silent encoded-dot traversal that libcurl's $path used to perform.
  expect_equal(
    get_clean_url(
      "http://ex.com/a/%2e%2e/b", path_normalization = "dot_segments"
    ),
    "http://ex.com/a/%2E%2E/b"
  )
})

test_that("percent-hex case is canonicalized to uppercase under 'keep'", {
  expect_equal(get_clean_url("http://ex.com/a%2fb"), "http://ex.com/a%2Fb")
  expect_equal(get_clean_url("http://ex.com/a%2Fb"), "http://ex.com/a%2Fb")
  expect_equal(
    get_clean_url("http://ex.com/a%2fb"),
    get_clean_url("http://ex.com/a%2Fb")
  )
})

test_that("encoded slashes and general encoding survive under 'keep'", {
  expect_equal(get_clean_url("http://ex.com/a%2Fb"), "http://ex.com/a%2Fb")
  expect_equal(get_clean_url("http://ex.com/a%252Fb"), "http://ex.com/a%252Fb")
  expect_equal(get_clean_url("http://ex.com/%41%42"), "http://ex.com/%41%42")
  expect_equal(get_clean_url("http://ex.com/a//b"), "http://ex.com/a//b")
})

test_that("query/fragment slashes never leak into the path", {
  expect_equal(get_path("http://ex.com/p?x=/y#/z"), "/p")
  expect_equal(get_clean_url("http://ex.com/p?x=a/b"), "http://ex.com/p")
})

test_that("scheme-relative and userinfo/port authorities extract correctly", {
  expect_equal(
    get_clean_url("//ex.com/a/../b", scheme_relative_handling = "http"),
    "http://ex.com/a/../b"
  )
  expect_equal(
    get_path("http://user:pass@ex.com:8080/a/../b"),
    "/a/../b"
  )
})

test_that("empty path falls back to canonical '/'", {
  expect_equal(get_clean_url("http://ex.com"), "http://ex.com/")
  expect_equal(get_clean_url("http://ex.com?x=1"), "http://ex.com/")
  expect_equal(get_path("http://ex.com"), "/")
})

test_that("dot-segment paths yield distinct canonical_join keys by default", {
  # canonical_join keys on clean_url, so preserving dot segments means
  # /a/../b no longer collides with /b.
  expect_false(
    identical(
      get_clean_url("http://ex.com/a/../b"),
      get_clean_url("http://ex.com/b")
    )
  )
  # ... but they DO collide once dot segments are resolved, as expected.
  expect_identical(
    get_clean_url("http://ex.com/a/../b", path_normalization = "dot_segments"),
    get_clean_url("http://ex.com/b", path_normalization = "dot_segments")
  )
})

test_that("vector and standalone scalar engines agree on raw paths", {
  urls <- c(
    "http://ex.com/a/../b", "http://ex.com/a/%2e%2e/b",
    "http://ex.com/a%2fb", "http://ex.com", "http://ex.com/p?x=/y"
  )
  scalar_impl <- function(u) {
    res <- rurl:::._safe_parse_url_impl(
      url = u, protocol_handling = "keep", www_handling = "none",
      tld_source = "all", case_handling = "keep",
      trailing_slash_handling = "none", index_page_handling = "keep",
      path_normalization = "none", scheme_relative_handling = "keep",
      subdomain_levels_to_keep = NULL, host_encoding = "keep",
      path_encoding = "keep"
    )
    if (is.null(res)) NA_character_ else res$clean_url
  }
  vec <- safe_parse_urls(urls, case_handling = "keep")$clean_url
  scal <- vapply(urls, scalar_impl, character(1), USE.NAMES = FALSE)
  expect_equal(vec, scal)
})
