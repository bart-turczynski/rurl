# Raw-path extraction (RURL-chdrlyci): path_normalization = "none" is lossless.
#
# rurl reads the request path from the input verbatim rather than from the
# parser's
# pre-normalized $path, so dot segments (and percent-encoded dot forms) survive
# to path_normalization. The only parser normalization replayed is percent-hex
# case (RFC 3986 section 6.2.2.1), which keeps %2F/%2f join-equivalent.
#
# The default is `url_standard = "whatwg"` (ADR 0020), whose path state
# resolves dot segments, `%2e` spellings included, and keeps a triplet's hex
# case; there `path_normalization = "none"` is a conflict (ADR 0020 D4a). The
# lossless raw path is reached through `profile = "rfc-syntax"` (RFC 3986
# syntax, no normalization), which these pins use with the web acceptance.

rs <- function(url, ...) {
  safe_parse_urls(url, profile = "rfc-syntax", scheme_acceptance = "web", ...)
}

test_that("the default whatwg path state resolves dot segments", {
  expect_equal(get_clean_url("http://ex.com/a/../b"), "http://ex.com/b")
  expect_equal(get_clean_url("http://ex.com/a/./b"), "http://ex.com/a/b")
  expect_equal(
    get_clean_url("http://ex.com/a//b/./c/../d"),
    "http://ex.com/a//b/d"
  )
  expect_equal(get_path("http://ex.com/a/../b"), "/b")
  # A `%2e` spelling is a dot segment in the WHATWG path state.
  expect_equal(get_clean_url("http://ex.com/a/%2e%2e/b"), "http://ex.com/b")
  # An existing triplet keeps its hex case.
  expect_equal(get_clean_url("http://ex.com/a%2fb"), "http://ex.com/a%2fb")
  expect_error(
    get_clean_url("http://ex.com/a/../b", path_normalization = "none"),
    "governs `path_normalization`"
  )
})

test_that("path_normalization = 'none' preserves dot segments (rfc-syntax)", {
  expect_equal(rs("http://ex.com/a/../b")$clean_url, "http://ex.com/a/../b")
  expect_equal(rs("http://ex.com/a/./b")$clean_url, "http://ex.com/a/./b")
  expect_equal(
    rs("http://ex.com/a//b/./c/../d")$clean_url,
    "http://ex.com/a//b/./c/../d"
  )
  expect_equal(rs("http://ex.com/a/../b")$path, "/a/../b")
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

test_that("percent-encoded %2e is a path byte, not a dot (rfc-syntax)", {
  # Preserved under "none" (hex-uppercased) ...
  expect_equal(
    rs("http://ex.com/a/%2e%2e/b")$clean_url,
    "http://ex.com/a/%2E%2E/b"
  )
  # ... and NOT collapsed even when dot-segment resolution is requested
  # (RFC 3986 section 5.2.4 operates on literal segments only).
  expect_equal(
    rs("http://ex.com/a/%2e%2e/b",
       path_normalization = "dot_segments")$clean_url,
    "http://ex.com/a/%2E%2E/b"
  )
})

test_that("percent-hex case is canonicalized to uppercase (rfc-syntax)", {
  expect_equal(rs("http://ex.com/a%2fb")$clean_url, "http://ex.com/a%2Fb")
  expect_equal(rs("http://ex.com/a%2Fb")$clean_url, "http://ex.com/a%2Fb")
  expect_equal(
    rs("http://ex.com/a%2fb")$clean_url,
    rs("http://ex.com/a%2Fb")$clean_url
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
    rs("//ex.com/a/../b", scheme_relative_handling = "http")$clean_url,
    "http://ex.com/a/../b"
  )
  expect_equal(
    rs("http://user:pass@ex.com:8080/a/../b")$path,
    "/a/../b"
  )
})

test_that("empty path falls back to canonical '/'", {
  expect_equal(get_clean_url("http://ex.com"), "http://ex.com/")
  expect_equal(get_clean_url("http://ex.com?x=1"), "http://ex.com/")
  expect_equal(get_path("http://ex.com"), "/")
})

test_that("dot-segment paths and canonical_join keys", {
  # canonical_join keys on clean_url. The default whatwg resolves the dot
  # segments, so /a/../b collides with /b ...
  expect_identical(
    get_clean_url("http://ex.com/a/../b"),
    get_clean_url("http://ex.com/b")
  )
  # ... and the raw rfc-syntax path keeps them apart.
  expect_false(
    identical(
      rs("http://ex.com/a/../b")$clean_url,
      rs("http://ex.com/b")$clean_url
    )
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
      path_encoding = "keep", url_standard = "rfc3986"
    )
    if (is.null(res)) NA_character_ else res$clean_url
  }
  vec <- rs(urls)$clean_url
  scal <- vapply(urls, scalar_impl, character(1), USE.NAMES = FALSE)
  expect_equal(vec, scal)
})
