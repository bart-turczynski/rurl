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

# RUL-030 (RURL-pntbujzc): under rfc3986 a raw non-ASCII code point is a
# parse error (RFC 3986 S2: a URI is US-ASCII; S2.5: non-ASCII data is
# percent-encoded UTF-8; S3.2.2: reg-name is ASCII), so every multibyte row
# below fails on the public surface. The general route still decomposes the
# row before the gate's verdict masks it, and the email facts read that
# decomposition, so the byte cuts stay pinned on `.general_parse_vec()`.
gen_cut <- function(x) {
  g <- .general_parse_vec(u8(x), "rfc3986", "general", "infer")
  g[c("ok", "scheme", "userinfo", "host", "port", "path", "query",
      "fragment")]
}

test_that("rfc3986 general: the authority and ?/# cuts after multibyte text", {
  multibyte <- c(
    "foo://\u00e9\u65e5:p@h\u00e9:81/p\u00e9?q\u00e9#f\u00e9",
    "foo://\U0001F600@[::1]:8/x", "foo:\u00e9#\u00e9?x", "foo:\u65e5?\u00e9#",
    # Two raw `@` fail the RFC 3986 authority (S3.2.1) as well.
    "foo://\u00e9@a@h/"
  )
  for (x in multibyte) {
    expect_row(x, "rfc3986", row(parse_status = "error"))
  }
  expect_identical(gen_cut(multibyte[1:4]), list(
    ok = rep(FALSE, 4L),
    scheme = rep("foo", 4L),
    userinfo = c("\u00e9\u65e5:p", "\U0001F600", NA, NA),
    host = c("h\u00e9", "[::1]", NA, NA),
    port = c("81", "8", NA, NA),
    path = c("/p\u00e9", "/x", "\u00e9", "\u65e5"),
    query = c("q\u00e9", NA, NA, "\u00e9"),
    fragment = c("f\u00e9", NA, "\u00e9?x", "")
  ))
})

test_that("whatwg general: the last `@` splits userinfo after multibyte text", {
  expect_row(
    "foo://\u00e9\u65e5:p@a@h\u00e9:81/p\u00e9?q\u00e9#f\u00e9", "whatwg",
    row("foo", "%C3%A9%E6%97%A5", "p%40a", "h%C3%A9", 81L, "/p\u00e9",
        "q%C3%A9", "f%C3%A9", parse_status = "ok")
  )
  expect_row(
    "foo://\u00e9@[::1]:8/x", "whatwg",
    row("foo", "%C3%A9", host = "[::1]", port = 8L, path = "/x",
        parse_status = "ok")
  )
  expect_row(
    "foo:\u00e9#\u00e9?x", "whatwg",
    row("foo", path = "%C3%A9", fragment = "%C3%A9?x", parse_status = "ok")
  )
})

test_that("rfc3986 file overlay: cuts after multibyte text", {
  x <- c("file://\u00e9@h\u00e9/p\u00e9?q#f", "FILE:///\u00e9?\u00e9#\u00e9")
  # RUL-030: a parse error under rfc3986 (RFC 3986 S2, S2.5, S3.2.2).
  for (u in x) expect_row(u, "rfc3986", row(parse_status = "error"))
  # No public record reaches the overlay's cuts with a multibyte row now (the
  # frozen NULL profile that did is retired, ADR 0020), so they are pinned on
  # the overlay itself, whose gate fails the row.
  p <- .parse_rfc_file_urls_vec(u8(x))
  expect_identical(
    p[c("ok", "scheme", "userinfo", "host", "path", "query", "fragment")],
    list(ok = c(FALSE, FALSE), scheme = c("file", "FILE"),
         userinfo = c("\u00e9", NA), host = c("h\u00e9", ""),
         path = c("/p\u00e9", "/\u00e9"), query = c("q", "\u00e9"),
         fragment = c("f", "\u00e9"))
  )
})

test_that("a leading U+FEFF is data: no scheme, as for its U+200B twin", {
  for (std in c("rfc3986", "whatwg")) {
    for (lead in c("\ufeff", "\u200b", "\U0001F600")) {
      expect_row(paste0(lead, "foo://u@h/"), std, row(parse_status = "error"))
      expect_row(paste0(lead, "file:///p"), std, row(parse_status = "error"))
    }
  }
  # A mark inside the authority is userinfo data. Under rfc3986 the row is a
  # parse error (RUL-030; RFC 3986 S2, S2.5), but the decomposition still
  # cuts the mark into the userinfo.
  expect_row("foo://\ufeffu@h/", "rfc3986", row(parse_status = "error"))
  expect_identical(gen_cut("foo://\ufeffu@h/")[c("userinfo", "host", "path")],
                   list(userinfo = "\ufeffu", host = "h", path = "/"))
  expect_row(
    "foo://\ufeffu@h/", "whatwg",
    row("foo", "%EF%BB%BFu", host = "h", path = "/", parse_status = "ok")
  )
  marks <- u8(c("\ufefffoo://h/", "\u200bfoo://h/", "foo://h/\ufeff"))
  expect_identical(.rfc3986_generic_uri_ok(marks)$ok, c(FALSE, FALSE, FALSE))
  # On its ASCII grammar alone (the component walk) only the mark-led rows
  # fail: a leading mark leaves no scheme (S3.1).
  expect_identical(.rfc3986_generic_uri_ok_walk(marks), c(FALSE, FALSE, TRUE))
})

test_that("rfc3986: a slash run other than two after a web scheme", {
  # RUL-030: each multibyte row is a parse error under rfc3986 (RFC 3986 S2,
  # S2.5, S3.2.2); the ASCII row keeps its parse.
  for (x in c("http:///h\u00e9/p", "http:/h\u00e9/p", "http://h\u00e9/p")) {
    expect_row(x, "rfc3986", row(parse_status = "error"))
  }
  expect_row("http:////h/p", "rfc3986",
             row("http", path = "//h/p", parse_status = "ok"))
  expect_row("http:///h\u00e9/p", "whatwg",
             row("http", host = "h\u00e9", path = "/p",
                 parse_status = "warning-no-tld"))
})

test_that("the rfc3986 grammar walk on invalid UTF-8, NA and a latin1 row", {
  bad <- c("foo://a\x80@h/", "foo://h/\xff", "\x80foo://h/")
  Encoding(bad) <- "UTF-8"
  # RUL-030: the valid multibyte row fails and is flagged (RFC 3986 S2,
  # S2.5); the undecodable rows fail without the token (no scalar value).
  g <- .rfc3986_generic_uri_ok(c(bad, NA, "foo://\u00e9@h:1/p?q#f"))
  expect_identical(g$ok, c(FALSE, FALSE, FALSE, NA, FALSE))
  expect_identical(g$diagnostic,
                   c(NA, NA, NA, NA, "unicode-outside-rfc3986-uri"))
  # The component walk alone admits the multibyte row on its ASCII grammar.
  expect_true(.rfc3986_generic_uri_ok_walk("foo://\u00e9@h:1/p?q#f"))
  l1 <- "foo://caf\xe9@h\xe9:1/p\xe9?q\xe9#\xe9"
  Encoding(l1) <- "latin1"
  expect_false(.rfc3986_generic_uri_ok(l1)$ok)
  expect_identical(.rfc3986_generic_uri_ok(l1)$diagnostic,
                   "unicode-outside-rfc3986-uri")
  # The latin1 walk itself still reads the text it earns.
  expect_true(.rfc3986_generic_uri_ok_walk(.latin1_input_to_utf8(l1)))
  p <- safe_parse_urls(c(bad, NA), url_standard = "rfc3986",
                       scheme_acceptance = "general")
  expect_identical(p$parse_status, rep("error", 4L))
  p <- safe_parse_urls(c(bad, NA), url_standard = "whatwg",
                       scheme_acceptance = "general")
  expect_identical(p$parse_status, c("ok", "ok", "error", "error"))
  expect_identical(p$user, c("a%EF%BF%BD", NA, NA, NA))
  expect_identical(p$path, c("/", "/\ufffd", NA, NA))
})

test_that("bracketed IPv6 literals: the `::` and embedded-IPv4 cuts", {
  v <- c("http://[::ffff:1.2.3.4]/", "http://[1::2:3.4.5.6]/",
         "http://[::1.2.3.4]/", "http://[1:2:3:4:5:6:7:8]/",
         "http://[1::2]:8/", "http://[::1::2]/", "http://u\u00e9@[a::b]/",
         "http://[::1]\u00e9/", "ftp://[::1:1.2.3.4]/", "http://[::\u00e9]/",
         "http://[1:2::\u00e9:1.2.3.4]/")
  legacy <- c("[::ffff:1.2.3.4]", "[1::2:3.4.5.6]", "[::1.2.3.4]",
              "[1:2:3:4:5:6:7:8]", "[1::2]", NA, "[a::b]", NA,
              "[::1:1.2.3.4]", NA, NA)
  status <- c("ok", "ok", "ok", "ok", "ok", "error", "ok", "error", "ok-ftp",
              "error", "error")
  # `legacy` is the source spelling the frozen NULL profile reported; rfc3986
  # keeps it except on `http://ué@[a::b]/`: a raw non-ASCII userinfo is a
  # parse error there (RUL-030; RFC 3986 S2, S2.5, S3.2.1). The default is
  # whatwg (ADR 0020), asserted below.
  p <- safe_parse_urls(u8(v), url_standard = "rfc3986")
  expect_identical(p$host, replace(legacy, 7L, NA))
  expect_identical(p$parse_status, replace(status, 7L, "error"))
  p <- safe_parse_urls(u8(v), url_standard = "whatwg")
  expect_identical(
    p$host,
    c("[::ffff:102:304]", "[1::2:304:506]", "[::102:304]",
      "[1:2:3:4:5:6:7:8]", "[1::2]", NA, "[a::b]", NA, "[::1:102:304]",
      NA, NA)
  )
  expect_identical(p$parse_status, status)
  expect_identical(safe_parse_urls(u8(v)), p)
  expect_identical(get_host(u8("http://[1::2]:8/")), "[1::2]")
})

test_that("serialize_url: the authority slice after multibyte text", {
  s <- u8(c("http://\u00e9\u65e5:p@a@h\u00e9:81/p", "http://u@h/",
            "\ufeffhttp://u@h/", "\u200bhttp://u@h/", "http://h\u00e9/",
            "http://[::1]:8/", NA, "http://\U0001F600@h/",
            "http://\u00e9:\u65e5@H\u00c9/p?q#f"))
  w <- c("http://%C3%A9%E6%97%A5:p%40a@xn--h-bga:81/p", "http://u@h/", NA,
         NA, "http://xn--h-bga/", "http://[::1]:8/", NA,
         "http://%F0%9F%98%80@h/",
         "http://%C3%A9:%E6%97%A5@xn--h-bga/p?q#f")
  expect_identical(serialize_url(s, "whatwg", "source"), w)
  expect_identical(serialize_url(s, "whatwg", "normalized"), w)
  # rfc3986: every multibyte row is a parse error (RUL-030; RFC 3986 S2,
  # S2.5, S3.2.2), so only the ASCII rows serialize.
  r <- c(NA, "http://u@h/", NA, NA, NA, "http://[::1]:8/", NA, NA, NA)
  expect_identical(serialize_url(s, "rfc3986", "source"), r)
})

test_that("get_url_key: multibyte userinfo and a leading mark", {
  k <- get_url_key(u8(c("http://\u00e9@h\u00e9/p", "\ufeffhttp://h/",
                        "\u200bhttp://h/", "http://h\u00e9/p")))
  expect_identical(is.na(as.character(k)), c(FALSE, TRUE, TRUE, FALSE))
})

test_that("the constant byte-mode patterns run without the per-call check", {
  # Each runs through base R directly on a per-row path, so the check the
  # wrappers make on every call is made here once (ADR 0019).
  for (re in list(.SCHEME_PREFIX_RE, .FILE_SCHEME_PREFIX_RE,
                  .WEB_IPV6_QUAD_RE)) {
    expect_true(.bytes_check_pattern(re, fixed = FALSE))
  }
  x <- u8(c("foo:x", "Foo+b.c-1:", "\ufefffoo:", "\u200bfoo:", "1a:", ":",
            "f\u00e9:", NA, ""))
  expect_identical(.scheme_prefix_len(x),
                   c(3L, 9L, NA, NA, NA, NA, NA, NA, NA))
  expect_identical(.scheme_prefix(x),
                   c("foo", "Foo+b.c-1", NA, NA, NA, NA, NA, NA, NA))
  expect_identical(.rfc3986_first_index(u8("\u00e9\ufeff#"), "#"), 6L)
  expect_identical(.rfc3986_first_index(NA_character_, "#"), 0L)
  expect_identical(.rfc3986_first_index("", "#"), 0L)
})
