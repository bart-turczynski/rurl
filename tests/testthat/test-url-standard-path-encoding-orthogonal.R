# path_encoding is an orthogonal presentation axis that LAYERS on any
# url_standard profile (ADR 0011 / RURL-sjnqhwtl). It is no longer governed by
# the selector -- the profile sets the internal path IDENTITY mode, and the
# public keep/encode/decode presentation applies on top, mirroring how
# host_encoding selects a spelling independent of the selector.

test_that("path_encoding = 'keep' is the profile's canonical identity", {
  # keep (the default) must reproduce today's profile output byte-for-byte:
  # whatwg never decodes, rfc decodes unreserved only.
  expect_identical(
    get_path("http://ex.com/%41%42", url_standard = "whatwg",
      path_encoding = "keep"),
    get_path("http://ex.com/%41%42", url_standard = "whatwg")
  )
  expect_identical(
    get_path("http://ex.com/%41%42", url_standard = "whatwg"), "/%41%42"
  )
  expect_identical(
    get_path("http://ex.com/%41%42", url_standard = "rfc3986"), "/AB"
  )
})

test_that("path_encoding = 'encode' renders the browser form under a profile", {
  # Retires benchmark caveat #3: a readable non-ASCII path emits its
  # percent-encoded UTF-8 form under either profile.
  for (std in c("rfc3986", "whatwg")) {
    expect_identical(
      get_path("https://ex.com/école", url_standard = std,
        path_encoding = "encode"),
      "/%C3%A9cole",
      info = std
    )
  }
})

test_that("path_encoding = 'decode' renders the readable form on a profile", {
  for (std in c("rfc3986", "whatwg")) {
    expect_identical(
      get_path("http://ex.com/%C3%A9cole", url_standard = std,
        path_encoding = "decode"),
      "/école",
      info = std
    )
  }
})

test_that("encode/decode are presentation not identity: reserved octets fold", {
  # Documented consequence (ADR 0011): the presentation forms may re-encode or
  # decode reserved octets, so a profile's %2F identity does NOT survive them.
  # keep is the value that preserves it.
  expect_identical(
    get_path("http://ex.com/a%2Fb/c", url_standard = "whatwg"), "/a%2Fb/c"
  )
  expect_identical(
    get_path("http://ex.com/a%2Fb/c", url_standard = "whatwg",
      path_encoding = "decode"),
    "/a/b/c"
  )
})

test_that("path_encoding composes with host_encoding under a profile", {
  # Both presentation axes are orthogonal to the selector and to each other.
  res <- safe_parse_url("https://xn--mnchen-3ya.de/école",
    url_standard = "whatwg", host_encoding = "unicode",
    path_encoding = "encode")
  expect_identical(res$host, "münchen.de")
  expect_identical(res$path, "/%C3%A9cole")
})

test_that("canonical_join layers path_encoding through the `...` seam", {
  A <- data.frame(URL = "https://ex.com/école", ValA = 1L,
    stringsAsFactors = FALSE)
  B <- data.frame(URL = "https://ex.com/%C3%A9cole", ValB = 2L,
    stringsAsFactors = FALSE)
  # encode collapses both spellings of the path to the browser form, so the two
  # rows join on one canonical key.
  joined <- canonical_join(A, B, url_standard = "whatwg",
    path_encoding = "encode")
  expect_identical(nrow(joined), 1L)
  expect_identical(joined$ValA, 1L)
  expect_identical(joined$ValB, 2L)
})
