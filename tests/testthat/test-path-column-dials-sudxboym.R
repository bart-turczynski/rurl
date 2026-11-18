# RURL-sudxboym (standards-literal audit finding 8): `trailing_slash_handling`
# and `index_page_handling` shape `clean_url` only (ADR 0017 D2 rows 5-6). The
# `path` column and `get_path()` are the input's path under the selected
# standard: the WHATWG URL Standard's path state keeps the input's segments,
# and RFC 3986 section 6.2.3 sanctions no trailing-slash or index-page
# equivalence, so `/dir` and `/dir/` are different paths in both.

test_that("get_path() returns the parsed path under either dial", {
  for (std in c("whatwg", "rfc3986")) {
    got <- c(
      get_path("https://example.com/dir/file.txt",
               trailing_slash_handling = "keep", url_standard = std),
      get_path("https://example.com/dir/",
               trailing_slash_handling = "strip", url_standard = std),
      get_path("https://example.com/foo/index.html",
               index_page_handling = "strip", url_standard = std)
    )
    expect_identical(got, c("/dir/file.txt", "/dir/", "/foo/index.html"),
                     info = std)
  }
})

test_that("the seo profile's path column is the parsed path", {
  x <- c("http://www.Example.com/a/", "HTTPS://example.com:443/index.html?q=1",
         "http://example.com/a/../index.html")
  out <- safe_parse_urls(x, profile = "seo")
  expect_identical(out$path, c("/a/", "/index.html", "/index.html"))
  # The bundle's clean_url still strips both (ADR 0017 D2 rows 5-6).
  expect_identical(
    out$clean_url,
    c("https://example.com/a", "https://example.com", "https://example.com")
  )
})

edge_inputs <- c(
  "http://example.com/", "http://example.com", "",
  NA, "http://example.com/a/../index.html",
  "﻿http://example.com/index.html", "http://example.com/bücher/",
  "http://example.com/b%FFc/", "http://example.com/a//b/index.html",
  "http://./", "http://./x/", "foo://h.example/p/index.html"
)

test_that("edge inputs keep the dial-free record under every value", {
  for (std in c("whatwg", "rfc3986")) {
    for (sa in c("web", "general")) {
      ref <- safe_parse_urls(edge_inputs, url_standard = std,
                             scheme_acceptance = sa)
      ref$clean_url <- NULL
      for (ts in c("none", "keep", "strip")) {
        for (ip in c("keep", "strip")) {
          got <- safe_parse_urls(edge_inputs, url_standard = std,
                                 scheme_acceptance = sa,
                                 trailing_slash_handling = ts,
                                 index_page_handling = ip)
          got$clean_url <- NULL
          expect_identical(got, ref, info = paste(std, sa, ts, ip))
        }
      }
    }
  }
})

# Controls: the dials still do their whole job on clean_url.
test_that("clean_url still takes both dials", {
  expect_identical(
    get_clean_url(c("http://example.com/dir/",
                    "http://example.com/a/index.html",
                    "http://./", "http://./x/"),
                  trailing_slash_handling = "strip",
                  index_page_handling = "strip"),
    c("http://example.com/dir", "http://example.com/a", "http://./",
      "http://./x")
  )
  expect_identical(
    get_clean_url("http://example.com/dir/file.txt",
                  trailing_slash_handling = "keep"),
    "http://example.com/dir/file.txt/"
  )
  # D4a: the clean-only slash collapse still feeds index and trailing-slash
  # handling, while the record keeps the parsed path.
  out <- safe_parse_urls("http://example.com/a//b/index.html",
                         path_normalization = "both",
                         trailing_slash_handling = "strip",
                         index_page_handling = "strip")
  expect_identical(out$clean_url, "http://example.com/a/b")
  expect_identical(out$path, "/a//b/index.html")
})

# The invariant behind the ticket: over the fixture corpora, a non-default
# trailing_slash_handling or index_page_handling may move `clean_url` and
# nothing else. Feeding the dialled path back into the record turns it red.
test_that("only clean_url moves with the path dials over the corpora", {
  skip_if_not_installed("jsonlite")
  ext_path <- testthat::test_path("fixtures", "external-url-vectors.csv")
  txt <- rawToChar(readBin(ext_path, "raw", file.size(ext_path)))
  Encoding(txt) <- "UTF-8"
  fx <- utils::read.csv(
    text = txt, stringsAsFactors = FALSE, colClasses = "character",
    na.strings = "NA", encoding = "UTF-8"
  )
  ext <- vapply(fx$input_json[fx$runnable == "yes"], jsonlite::fromJSON,
                character(1), USE.NAMES = FALSE)
  x <- unique(c(parse_corpus_urls(), ext))
  for (std in c("whatwg", "rfc3986")) {
    for (sa in c("web", "general")) {
      ref <- safe_parse_urls(x, url_standard = std, scheme_acceptance = sa)
      ref$clean_url <- NULL
      for (ts in c("none", "keep", "strip")) {
        for (ip in c("keep", "strip")) {
          if (ts == "none" && ip == "keep") next
          got <- safe_parse_urls(x, url_standard = std,
                                 scheme_acceptance = sa,
                                 trailing_slash_handling = ts,
                                 index_page_handling = ip)
          got$clean_url <- NULL
          expect_identical(got, ref, info = paste(std, sa, ts, ip))
        }
      }
    }
  }
})
