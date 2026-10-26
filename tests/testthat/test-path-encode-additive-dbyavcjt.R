# RURL-dbyavcjt: `path_encoding = "encode"` only adds escapes. It never
# decodes, so an existing triplet, every "/" and every empty segment stay as
# written under every `url_standard` (RFC 3986 sections 2.2 and 3.3; the WHATWG
# path percent-encode set; ADR 0020).

stds <- list(`NULL` = NULL, rfc3986 = "rfc3986", whatwg = "whatwg")

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

test_that("encode under the RFC 3986 arms escapes only what pchar excludes", {
  for (sn in c("NULL", "rfc3986")) {
    std <- stds[[sn]]
    # Sub-delimiters, ":" and "@" are pchar and stay literal; a `%00` is a
    # triplet and stays.
    expect_identical(
      get_path("http://example.com/a;b=c:d@e/!$&'()*+,/%00",
               path_encoding = "encode", url_standard = std),
      "/a;b=c:d@e/!$&'()*+,/%00", info = sn
    )
  }
  # A raw non-ASCII character is not URI syntax, so "rfc3986" rejects it
  # before presentation; NULL accepts it and encodes its UTF-8 octets.
  expect_identical(
    get_path("http://example.com/école", path_encoding = "encode"),
    "/%C3%A9cole"
  )
  # Under NULL a `%` that does not begin a triplet becomes `%25`, an existing
  # triplet is not decoded, and its hex is uppercased as under "keep".
  expect_identical(
    get_path("http://example.com/%zz/%41/%2f", path_encoding = "encode"),
    "/%25zz/%41/%2F"
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
