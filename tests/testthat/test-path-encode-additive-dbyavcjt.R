# RURL-dbyavcjt: `path_encoding = "encode"` only adds escapes. It never
# decodes, so an existing triplet, every "/" and every empty segment stay as
# written under every `url_standard` (RFC 3986 sections 2.2 and 3.3; the WHATWG
# path percent-encode set; ADR 0020).

# `url_standard = NULL` reads as "whatwg" (ADR 0020 D4), so it is not a third
# arm here.
stds <- list(rfc3986 = "rfc3986", whatwg = "whatwg")

test_that("encode keeps empty segments and %2F in every surface", {
  cases <- c(
    "http://example.com/a//" = "/a//",
    "http://example.com//" = "//",
    "http://example.com/a%2Fb/" = "/a%2Fb/",
    "http://example.com/a//b" = "/a//b"
  )
  for (sn in names(stds)) {
    std <- stds[[sn]]
    u <- names(cases)
    expect_identical(
      get_path(u, path_encoding = "encode", url_standard = std),
      unname(cases), info = sn
    )
    expect_identical(
      safe_parse_urls(u, path_encoding = "encode", url_standard = std)$path,
      unname(cases), info = sn
    )
    expect_identical(
      get_clean_url(u, path_encoding = "encode", url_standard = std),
      paste0("http://example.com", cases), info = sn
    )
  }
})

test_that("encode under rfc3986 escapes only what pchar excludes", {
  # Sub-delimiters, ":" and "@" are pchar and stay literal; a `%00` is a
  # triplet and stays.
  expect_identical(
    get_path("http://example.com/a;b=c:d@e/!$&'()*+,/%00",
             path_encoding = "encode", url_standard = "rfc3986"),
    "/a;b=c:d@e/!$&'()*+,/%00"
  )
  # A raw non-ASCII character is not URI syntax, so "rfc3986" rejects it
  # before presentation.
  expect_identical(
    get_path("http://example.com/école", path_encoding = "encode",
             url_standard = "rfc3986"),
    NA_character_
  )
})

test_that("encode under the default whatwg uses the path percent-encode set", {
  # The WHATWG path percent-encode set holds every code point above U+007E,
  # so a raw non-ASCII character is written as its UTF-8 octets; it holds no
  # `%`, so a `%` that begins no triplet and an existing triplet stay as
  # written, hex case included.
  expect_identical(
    get_path("http://example.com/école", path_encoding = "encode"),
    "/%C3%A9cole"
  )
  expect_identical(
    get_path("http://example.com/%zz/%41/%2f", path_encoding = "encode"),
    "/%zz/%41/%2f"
  )
})

test_that("the RFC 3986 path encoder maps octets one by one", {
  enc <- .rfc3986_path_percent_encode
  expect_identical(enc(c("/a b/", "/[x]", "/a%2Fb//", NA)),
                   c("/a%20b/", "/%5Bx%5D", "/a%2Fb//", NA))
  expect_identical(enc("/%"), "/%25")
  expect_identical(enc("/%2"), "/%252")
  expect_identical(enc("/%00/%2500"), "/%00/%2500")
})
