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
    get_path("http://ex.com/%7euser", url_standard = "whatwg"), "/%7euser"
  )
  expect_identical(
    get_path("http://ex.com/%4a%6A", url_standard = "whatwg"), "/%4a%6A"
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

test_that("whatwg path_encoding = 'encode' uses the WHATWG path encode set", {
  cases <- c(
    "http://ex.com/w|m" = "/w|m",
    "http://ex.com/@asdf%40" = "/@asdf%40",
    "http://ex.com/jqueryui@1.2.3" = "/jqueryui@1.2.3",
    "http://ex.com/foo%" = "/foo%",
    "http://ex.com/foo%2" = "/foo%2",
    "http://ex.com/foo%2zbar" = "/foo%2zbar",
    "http://ex.com/foo%41%7a" = "/foo%41%7a",
    "http://ex.com/foo%2Ehtml" = "/foo%2Ehtml",
    "http://ex.com/%3a" = "/%3a",
    "http://ex.com/\"quoted\"" = "/%22quoted%22",
    "http://ex.com/école" = "/%C3%A9cole"
  )
  for (input in names(cases)) {
    expect_identical(
      get_path(input, url_standard = "whatwg", path_encoding = "encode"),
      unname(cases[[input]]),
      info = input
    )
  }
  expect_identical(
    rurl:::.whatwg_path_percent_encode("\"#<>?`{} é"),
    "%22%23%3C%3E%3F%60%7B%7D%20%C3%A9"
  )
})

test_that("whatwg profile serializes query and fragment encode sets", {
  parsed <- safe_parse_urls(
    c(
      "http://host/?'",
      "http://example.org/test?\"",
      "http://example.org/test?<",
      "http://example.org/test?>",
      "http://foo.bar/baz?qux#foo\"bar",
      "http://foo.bar/baz?qux#foo<bar",
      "http://foo.bar/baz?qux#foo>bar",
      "http://foo.bar/baz?qux#foo`bar",
      "https://localhost?q=🔥#🔥"
    ),
    url_standard = "whatwg",
    scheme_policy = "require",
    host_encoding = "idna",
    path_encoding = "encode"
  )

  expect_identical(
    parsed$query,
    c("%27", "%22", "%3C", "%3E", "qux", "qux", "qux", "qux",
      "q=%F0%9F%94%A5")
  )
  expect_identical(
    parsed$fragment,
    c(NA_character_, NA_character_, NA_character_, NA_character_,
      "foo%22bar", "foo%3Cbar", "foo%3Ebar", "foo%60bar",
      "%F0%9F%94%A5")
  )
  expect_identical(
    rurl:::.whatwg_query_percent_encode("\"#<>' é", "http"),
    "%22%23%3C%3E%27%20%C3%A9"
  )
  expect_identical(
    rurl:::.whatwg_fragment_percent_encode("\"<>` é"),
    "%22%3C%3E%60%20%C3%A9"
  )
})

test_that("whatwg accepts WPT-valid path query fragment bytes curl rejects", {
  urls <- c(
    paste0("http://www.google.com/foo?bar=baz# ", intToUtf8(0x00BB)),
    paste0("http://foo.bar/baz?qux#foo", intToUtf8(0x08), "bar"),
    paste0(
      "https://www.example.com/path{", intToUtf8(0x7F),
      "path.html?query'", intToUtf8(0x7F),
      "=query#fragment<", intToUtf8(0x7F), "fragment"
    )
  )
  parsed <- safe_parse_urls(
    urls,
    url_standard = "whatwg",
    scheme_policy = "require",
    host_encoding = "idna",
    path_encoding = "encode",
    query_handling = "keep",
    port_handling = "strip_default"
  )

  expect_identical(parsed$parse_status, c("ok", "ok", "ok"))
  expect_identical(
    parsed$path,
    c("/foo", "/baz", "/path%7B%7Fpath.html")
  )
  expect_identical(
    parsed$query,
    c("bar=baz", "qux", "query%27%7F=query")
  )
  expect_identical(
    parsed$fragment,
    c("%20%C2%BB", "foo%08bar", "fragment%3C%7Ffragment")
  )
  expect_identical(
    parsed$clean_url,
    c(
      "http://www.google.com/foo?bar=baz",
      "http://foo.bar/baz?qux=",
      "https://www.example.com/path%7B%7Fpath.html?query%27%7F=query"
    )
  )
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
