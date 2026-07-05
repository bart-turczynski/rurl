# RFC 3986 path profile: unreserved-only percent normalizer, decode-then-dot
# (RURL-gjltzwmp, epic RURL-eqzkkohm; PRD §6.1, §7). Under
# `url_standard = "rfc3986"` rurl decodes ONLY unreserved percent-encoded
# octets (never reserved bytes), and does so BEFORE dot-segment removal so an
# encoded dot (%2e / %2e%2e) folds to a literal dot segment and is resolved.
# Diagnostics fire in BOTH modes, keyed to raw-path shape (PRD "facts not
# policy"), so this file also pins the whatwg side of the path-diagnostic
# fixtures ahead of the WHATWG path transform landing (RURL-bbmuehsx).

eq <- function(a, b, ...) {
  identical(get_clean_url(a, ...), get_clean_url(b, ...))
}

# --- Behavior matrix (PRD §6.1 table) ---------------------------------------

test_that("get_path matches the rfc3986 behavior matrix", {
  cases <- list(
    c("/a/../b/./c", "/b/c"),
    c("/%2e%2e/a", "/a"),
    c("/%41%42", "/AB"),
    c("/%7euser", "/~user"),
    c("/%20foo", "/%20foo"),
    c("/caf%C3%A9", "/caf%C3%A9"),
    c("/a%2Fb", "/a%2Fb"),
    c("/a%3Fb", "/a%3Fb")
  )
  for (cs in cases) {
    input <- paste0("http://ex.com", cs[1L])
    expect_identical(
      get_path(input, url_standard = "rfc3986"), cs[2L], info = cs[1L]
    )
  }
})

test_that("get_clean_url matches the rfc3986 behavior matrix", {
  expect_identical(
    get_clean_url("http://ex.com/a/../b/./c", url_standard = "rfc3986"),
    "http://ex.com/b/c"
  )
  expect_identical(
    get_clean_url("http://ex.com/%2e%2e/a", url_standard = "rfc3986"),
    "http://ex.com/a"
  )
  expect_identical(
    get_clean_url("http://ex.com/a%2Fb", url_standard = "rfc3986"),
    "http://ex.com/a%2Fb"
  )
})

test_that("mixed hex case is folded consistently", {
  expect_identical(
    get_path("http://ex.com/%2E%2e/a", url_standard = "rfc3986"), "/a"
  )
  expect_identical(
    get_path("http://ex.com/%41%62", url_standard = "rfc3986"), "/Ab"
  )
  # An untouched reserved/non-unreserved triplet is still hex-canonicalized.
  expect_identical(
    get_path("http://ex.com/a%2fb", url_standard = "rfc3986"), "/a%2Fb"
  )
})

# --- Regression assertions (PRD §9.3) ---------------------------------------

test_that("reserved-byte identity is preserved under rfc3986", {
  expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
    url_standard = "rfc3986"))
})

test_that("unreserved percent-decode makes %41%42 and AB equivalent keys", {
  expect_true(eq("http://ex.com/%41%42", "http://ex.com/AB",
    url_standard = "rfc3986"))
})

test_that("get_path honors the profile directly", {
  expect_equal(
    get_path("http://ex.com/%41%42", url_standard = "rfc3986"), "/AB"
  )
})

# --- Path diagnostics (PRD "path-diagnostic triggers" note) -----------------

test_that("encoded-dot-segment fires on /%2e%2e/a in both modes", {
  for (std in c("rfc3986", "whatwg")) {
    expect_true(
      "encoded-dot-segment" %in%
        get_url_diagnostics("http://ex.com/%2e%2e/a", url_standard = std),
      info = std
    )
  }
})

test_that("encoded-reserved-path-byte fires on %2F/%3F paths in both modes", {
  for (std in c("rfc3986", "whatwg")) {
    expect_true(
      "encoded-reserved-path-byte" %in%
        get_url_diagnostics("http://ex.com/a%2Fb", url_standard = std),
      info = std
    )
    expect_true(
      "encoded-reserved-path-byte" %in%
        get_url_diagnostics("http://ex.com/a%3Fb", url_standard = std),
      info = std
    )
  }
})

test_that("plain literal dot segments and clean paths carry no path tokens", {
  expect_identical(
    get_url_diagnostics("http://ex.com/a/../b", url_standard = "rfc3986"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics("http://ex.com/%41%42", url_standard = "rfc3986"),
    character(0)
  )
})

# --- AC #1: NULL selector is untouched ---------------------------------------

test_that("without a selector the historical path_encoding behavior is intact",
  {
    expect_identical(get_path("http://ex.com/%41%42"), "/%41%42")
    expect_identical(
      get_path("http://ex.com/%41%42", path_encoding = "decode"), "/AB"
    )
    expect_identical(
      get_path("http://ex.com/a/../b", path_normalization = "dot_segments"),
      "/b"
    )
  }
)
