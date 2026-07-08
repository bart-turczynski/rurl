# WHATWG path profile: preserve unreserved + encoded-dot recognizer
# (RURL-bbmuehsx, epic RURL-eqzkkohm; PRD §6.1). Under
# `url_standard = "whatwg"` rurl NEVER decodes percent-encoded bytes (reserved
# or unreserved), preserves percent-triplet spelling, and resolves dot segments
# via an encoded-dot recognizer that treats "."/"%2e"/"%2E" (and their
# double-dot combinations) as dot segments without a general path decode.

eq <- function(a, b, ...) {
  identical(get_clean_url(a, ...), get_clean_url(b, ...))
}

# --- Behavior matrix (PRD §6.1 table) ---------------------------------------

test_that("get_path matches the whatwg behavior matrix", {
  cases <- list(
    c("/a/../b/./c", "/b/c"),
    c("/%2e%2e/a", "/a"),
    c("/%41%42", "/%41%42"),
    c("/%7euser", "/%7euser"),
    c("/%20foo", "/%20foo"),
    c("/caf%C3%A9", "/caf%C3%A9"),
    c("/a%2Fb", "/a%2Fb"),
    c("/a%3Fb", "/a%3Fb")
  )
  for (cs in cases) {
    input <- paste0("http://ex.com", cs[1L])
    expect_identical(
      get_path(input, url_standard = "whatwg"), cs[2L], info = cs[1L]
    )
  }
})

test_that("get_clean_url matches the whatwg behavior matrix", {
  expect_identical(
    get_clean_url("http://ex.com/a/../b/./c", url_standard = "whatwg"),
    "http://ex.com/b/c"
  )
  expect_identical(
    get_clean_url("http://ex.com/%2e%2e/a", url_standard = "whatwg"),
    "http://ex.com/a"
  )
  expect_identical(
    get_clean_url("http://ex.com/a%2Fb", url_standard = "whatwg"),
    "http://ex.com/a%2Fb"
  )
})

# --- Encoded-dot recognizer (PRD §6.1, normative) ----------------------------

test_that("encoded-dot forms resolve like their literal counterparts", {
  cases <- list(
    c("/%2e/a", "/a"),
    c("/%2E/a", "/a"),
    c("/%2e%2e/a", "/a"),
    c("/%2E%2E/a", "/a"),
    c("/.%2e/a", "/a"),
    c("/%2e./a", "/a"),
    c("/a/%2e./b", "/b"),
    c("/a/%2e%2e/b", "/b")
  )
  for (cs in cases) {
    input <- paste0("http://ex.com", cs[1L])
    expect_identical(
      get_path(input, url_standard = "whatwg"), cs[2L], info = cs[1L]
    )
  }
})

test_that("a partial encoded-dot inside a larger segment is left as data", {
  expect_identical(get_path("http://ex.com/%2eab", url_standard = "whatwg"),
    "/%2eab")
  expect_identical(get_path("http://ex.com/ab%2e", url_standard = "whatwg"),
    "/ab%2e")
})

test_that("mixed hex case is preserved", {
  expect_identical(
    get_path("http://ex.com/%41%62", url_standard = "whatwg"), "/%41%62"
  )
  expect_identical(
    get_path("http://ex.com/a%2fb", url_standard = "whatwg"), "/a%2fb"
  )
})

# --- Regression assertions (Definition of Done) ------------------------------

test_that("unreserved percent-encoded bytes stay distinct canonical keys", {
  expect_false(eq("http://ex.com/%41%42", "http://ex.com/AB",
    url_standard = "whatwg"))
})

test_that("reserved-byte identity is preserved under whatwg", {
  expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
    url_standard = "whatwg"))
})

test_that("get_path honors the profile directly", {
  expect_equal(
    get_path("http://ex.com/%2e%2e/a", url_standard = "whatwg"), "/a"
  )
})

# --- Path diagnostics (already emitted in both modes per RURL-gjltzwmp) -----

test_that("encoded-dot-segment fires on the whatwg profile", {
  expect_true(
    "encoded-dot-segment" %in%
      get_url_diagnostics("http://ex.com/%2e%2e/a", url_standard = "whatwg")
  )
})

test_that("encoded-reserved-path-byte fires on the whatwg profile", {
  expect_true(
    "encoded-reserved-path-byte" %in%
      get_url_diagnostics("http://ex.com/a%2Fb", url_standard = "whatwg")
  )
})

# --- AC #1: NULL selector is untouched ---------------------------------------

test_that("without a selector the historical path_encoding behavior is intact",
  {
    # libcurl uppercases percent-triplet hex by default, independent of
    # url_standard; the dot segment is left literal (no dot resolution without
    # an explicit path_normalization).
    expect_identical(get_path("http://ex.com/%2e%2e/a"), "/%2E%2E/a")
    expect_identical(
      get_path("http://ex.com/a/../b", path_normalization = "dot_segments"),
      "/b"
    )
  }
)
