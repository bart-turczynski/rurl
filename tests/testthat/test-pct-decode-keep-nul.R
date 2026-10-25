# RURL-fpujlvoq: a `%00` in a path or query no longer ends the decode.
#
# `%00` is URI data (RFC 3986 sections 2.1 and 2.4), but the NUL it decodes to
# cannot live in an R string, and the decode used to stop there: the path
# presentation and get_query() returned only the prefix before it. The
# triplet now stays as written and everything after it decodes. The mailto
# field decode tests for the triplet itself and does not move
# (test-mailto-decoded-nul.R).
#
# This file opens with PINS: rows without a `%00` must not move.

nul_paths <- c(
  "http://e.com/a%00b/c", "http://e.com/%00", "http://e.com/a%00/",
  "http://e.com/%00%00x/"
)

test_that("pin: a path without %00 presents as before", {
  for (std in c("rfc3986", "whatwg")) {
    expect_identical(
      get_clean_url(c("http://e.com/a%2500b", "http://e.com/a%25b"),
                    path_encoding = "encode", url_standard = std),
      c("http://e.com/a%2500b", "http://e.com/a%25b"), info = std
    )
    expect_identical(
      get_clean_url("http://e.com/a%41b/%E9", path_encoding = "decode",
                    url_standard = std),
      "http://e.com/aAb/%E9", info = std
    )
  }
})

test_that("the path keeps everything after a %00", {
  for (std in c("rfc3986", "whatwg")) {
    for (pe in c("encode", "decode")) {
      expect_identical(
        get_clean_url(nul_paths, path_encoding = pe, url_standard = std),
        nul_paths, info = paste(std, pe)
      )
    }
  }
  expect_identical(
    get_clean_url(
      c("http://e.com/%41%00%42/c%7E%C3%A9", "http://e.com/%41%42/c%7E%C3%A9"),
      path_encoding = "encode", url_standard = "rfc3986"
    ),
    c("http://e.com/A%00B/c~%C3%A9", "http://e.com/AB/c~%C3%A9")
  )
  expect_identical(
    get_clean_url("http://e.com/%C3%A9%00%E9", path_encoding = "decode",
                  url_standard = "whatwg"),
    "http://e.com/é%00%E9"
  )
})

test_that("get_query() keeps everything after a %00", {
  u <- "http://e.com/?a=b%00c&d%00x=1"
  expect_identical(get_query(u, url_standard = "rfc3986"), "a=b%00c&d%00x=1")
  expect_identical(
    get_query(u, url_standard = "rfc3986", format = "list"),
    list(list(a = "b%00c", `d%00x` = "1"))
  )
})

test_that("the outputs do not depend on the locale", {
  utf8 <- hi_utf8_locale()
  for (loc in c("C", if (!is.na(utf8)) utf8)) {
    rurl_clear_caches()
    withr::with_locale(c(LC_CTYPE = loc), {
      expect_identical(
        get_clean_url(c(nul_paths, "http://e.com/%C3%A9%00x"),
                      path_encoding = "decode", url_standard = "whatwg"),
        c(nul_paths, "http://e.com/\u00e9%00x"), info = loc
      )
      expect_identical(
        get_query("http://e.com/?a=b%00c&d=1", url_standard = "rfc3986"),
        "a=b%00c&d=1", info = loc
      )
    })
  }
  rurl_clear_caches()
})
