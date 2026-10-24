# RURL-yqmoztoj: the offset-bearing string calls in R/parse-state.R,
# R/parse-web.R and R/serialize.R (ADR 0019).
#
# Each chain here locates or captures a delimiter, then cuts at that offset:
# the scheme capture and the cut after its colon, the first `#`, `?`, `/`,
# `]` and `:` and the last `@` in an authority, the `/` run after a scheme,
# the `::` in a bracketed IPv6 literal, and the serializer's authority
# capture. Every case puts multibyte text in front of the delimiter, so a
# position counted in characters but cut in bytes, or the reverse, lands on
# the wrong byte. Each U+FEFF row has a U+200B twin (ADR 0005, amendment):
# the mark is data, and a row it leads has no scheme.

u8 <- function(x) enc2utf8(x)
parse_cols <- c("scheme", "user", "password", "host", "port", "path",
                "query", "fragment", "parse_status")
parse_general <- function(x, std) {
  p <- safe_parse_urls(u8(x), url_standard = std,
                       scheme_acceptance = "general")
  as.list(p[1L, parse_cols])
}
row <- function(scheme = NA_character_, user = NA_character_,
                password = NA_character_, host = NA_character_,
                port = NA_integer_, path = NA_character_,
                query = NA_character_, fragment = NA_character_,
                parse_status) {
  list(scheme = scheme, user = user, password = password, host = host,
       port = port, path = path, query = query, fragment = fragment,
       parse_status = parse_status)
}
expect_row <- function(x, std, expected) {
  got <- parse_general(x, std)
  got$port <- as.integer(got$port)
  expected$port <- as.integer(expected$port)
  expect_identical(got, expected, label = paste(std, x))
}

test_that("rfc3986 general: the authority and ?/# cuts after multibyte text", {
  expect_row(
    "foo://é日:p@hé:81/pé?qé#fé", "rfc3986",
    row("foo", "é日", "p", "hé", 81L, "/pé", "qé",
        "fé", parse_status = "ok")
  )
  expect_row(
    "foo://\U0001F600@[::1]:8/x", "rfc3986",
    row("foo", "\U0001F600", host = "[::1]", port = 8L, path = "/x",
        parse_status = "ok")
  )
  expect_row(
    "foo:é#é?x", "rfc3986",
    row("foo", path = "é", fragment = "é?x", parse_status = "ok")
  )
  expect_row(
    "foo:日?é#", "rfc3986",
    row("foo", path = "日", query = "é", parse_status = "ok")
  )
  # Two raw `@` fail the RFC 3986 authority (S3.2.1).
  expect_row("foo://é@a@h/", "rfc3986", row(parse_status = "error"))
})

test_that("whatwg general: the last `@` splits userinfo after multibyte text", {
  expect_row(
    "foo://é日:p@a@hé:81/pé?qé#fé", "whatwg",
    row("foo", "%C3%A9%E6%97%A5", "p%40a", "h%C3%A9", 81L, "/pé",
        "q%C3%A9", "f%C3%A9", parse_status = "ok")
  )
  expect_row(
    "foo://é@[::1]:8/x", "whatwg",
    row("foo", "%C3%A9", host = "[::1]", port = 8L, path = "/x",
        parse_status = "ok")
  )
  expect_row(
    "foo:é#é?x", "whatwg",
    row("foo", path = "%C3%A9", fragment = "%C3%A9?x", parse_status = "ok")
  )
})

test_that("rfc3986 file overlay: cuts after multibyte text", {
  expect_row(
    "file://é@hé/pé?q#f", "rfc3986",
    row("file", "é", host = "hé", path = "/pé", query = "q",
        fragment = "f", parse_status = "ok")
  )
  expect_row(
    "FILE:///é?é#é", "rfc3986",
    row("file", path = "/é", query = "é", fragment = "é",
        parse_status = "ok")
  )
})

test_that("a leading U+FEFF is data: no scheme, as for its U+200B twin", {
  for (std in c("rfc3986", "whatwg")) {
    for (lead in c("﻿", "​", "\U0001F600")) {
      expect_row(paste0(lead, "foo://u@h/"), std, row(parse_status = "error"))
      expect_row(paste0(lead, "file:///p"), std, row(parse_status = "error"))
    }
  }
  # A mark inside the authority is userinfo data.
  expect_row(
    "foo://﻿u@h/", "rfc3986",
    row("foo", "﻿u", host = "h", path = "/", parse_status = "ok")
  )
  expect_row(
    "foo://﻿u@h/", "whatwg",
    row("foo", "%EF%BB%BFu", host = "h", path = "/", parse_status = "ok")
  )
  expect_identical(
    .rfc3986_generic_uri_ok(u8(c("﻿foo://h/", "​foo://h/",
                                 "foo://h/﻿")))$ok,
    c(FALSE, FALSE, TRUE)
  )
})

test_that("rfc3986: a slash run other than two after a web scheme", {
  expect_row("http:///hé/p", "rfc3986",
             row("http", path = "/hé/p", parse_status = "ok"))
  expect_row("http:/hé/p", "rfc3986",
             row("http", path = "/hé/p", parse_status = "ok"))
  expect_row("http:////h/p", "rfc3986",
             row("http", path = "//h/p", parse_status = "ok"))
  expect_row("http://hé/p", "rfc3986",
             row("http", host = "hé", path = "/p",
                 parse_status = "warning-no-tld"))
  expect_row("http:///hé/p", "whatwg",
             row("http", host = "hé", path = "/p",
                 parse_status = "warning-no-tld"))
})

test_that("the rfc3986 grammar walk on invalid UTF-8, NA and a latin1 row", {
  bad <- c("foo://a\x80@h/", "foo://h/\xff", "\x80foo://h/")
  Encoding(bad) <- "UTF-8"
  g <- .rfc3986_generic_uri_ok(c(bad, NA, "foo://é@h:1/p?q#f"))
  expect_identical(g$ok, c(FALSE, FALSE, FALSE, NA, TRUE))
  expect_identical(g$diagnostic,
                   c(NA, NA, NA, NA, "unicode-outside-rfc3986-uri"))
  l1 <- "foo://caf\xe9@h\xe9:1/p\xe9?q\xe9#\xe9"
  Encoding(l1) <- "latin1"
  expect_true(.rfc3986_generic_uri_ok(l1)$ok)
  p <- safe_parse_urls(c(bad, NA), url_standard = "rfc3986",
                       scheme_acceptance = "general")
  expect_identical(p$parse_status, rep("error", 4L))
  p <- safe_parse_urls(c(bad, NA), url_standard = "whatwg",
                       scheme_acceptance = "general")
  expect_identical(p$parse_status, c("ok", "ok", "error", "error"))
  expect_identical(p$user, c("a%EF%BF%BD", NA, NA, NA))
  expect_identical(p$path, c("/", "/�", NA, NA))
})

test_that("bracketed IPv6 literals: the `::` and embedded-IPv4 cuts", {
  v <- c("http://[::ffff:1.2.3.4]/", "http://[1::2:3.4.5.6]/",
         "http://[::1.2.3.4]/", "http://[1:2:3:4:5:6:7:8]/",
         "http://[1::2]:8/", "http://[::1::2]/", "http://ué@[a::b]/",
         "http://[::1]é/", "ftp://[::1:1.2.3.4]/", "http://[::é]/",
         "http://[1:2::é:1.2.3.4]/")
  legacy <- c("[::ffff:1.2.3.4]", "[1::2:3.4.5.6]", "[::1.2.3.4]",
              "[1:2:3:4:5:6:7:8]", "[1::2]", NA, "[a::b]", NA,
              "[::1:1.2.3.4]", NA, NA)
  status <- c("ok", "ok", "ok", "ok", "ok", "error", "ok", "error", "ok-ftp",
              "error", "error")
  for (std in list(NULL, "rfc3986")) {
    p <- safe_parse_urls(u8(v), url_standard = std)
    expect_identical(p$host, legacy, label = format(std))
    expect_identical(p$parse_status, status, label = format(std))
  }
  p <- safe_parse_urls(u8(v), url_standard = "whatwg")
  expect_identical(
    p$host,
    c("[::ffff:102:304]", "[1::2:304:506]", "[::102:304]",
      "[1:2:3:4:5:6:7:8]", "[1::2]", NA, "[a::b]", NA, "[::1:102:304]",
      NA, NA)
  )
  expect_identical(p$parse_status, status)
  expect_identical(get_host(u8("http://[1::2]:8/")), "[1::2]")
})

test_that("serialize_url: the authority slice after multibyte text", {
  s <- u8(c("http://é日:p@a@hé:81/p", "http://u@h/",
            "﻿http://u@h/", "​http://u@h/", "http://hé/",
            "http://[::1]:8/", NA, "http://\U0001F600@h/",
            "http://é:日@HÉ/p?q#f"))
  w <- c("http://%C3%A9%E6%97%A5:p%40a@xn--h-bga:81/p", "http://u@h/", NA,
         NA, "http://xn--h-bga/", "http://[::1]:8/", NA,
         "http://%F0%9F%98%80@h/",
         "http://%C3%A9:%E6%97%A5@xn--h-bga/p?q#f")
  expect_identical(serialize_url(s, "whatwg", "source"), w)
  expect_identical(serialize_url(s, "whatwg", "normalized"), w)
  r <- c(NA, "http://u@h/", NA, NA, "http://hé/", "http://[::1]:8/",
         NA, "http://\U0001F600@h/", "http://é:日@HÉ/p?q#f")
  expect_identical(serialize_url(s, "rfc3986", "source"), r)
})

test_that("get_url_key: multibyte userinfo and a leading mark", {
  k <- get_url_key(u8(c("http://é@hé/p", "﻿http://h/",
                        "​http://h/", "http://hé/p")))
  expect_identical(is.na(as.character(k)), c(FALSE, TRUE, TRUE, FALSE))
})
