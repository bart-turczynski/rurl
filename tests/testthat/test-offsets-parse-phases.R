# Pins over the R/parse-phases.R helpers whose offset-bearing calls (locate,
# capture, cut, length, split) move from stringi to base R in byte mode
# (RURL-yqmoztoj, ADR 0019). stringi counts code points after skipping a
# leading U+FEFF; byte mode counts bytes and reads the mark as data. Each pin
# holds what the stringi chain gave on multibyte prefixes, a leading U+FEFF
# with its U+200B twin, invalid-UTF-8 rows, line terminators (ICU's `.` stops
# at one, its `$` also matches before a final one), empty and unmatched
# captures, no match and NA. Values are compared as bytes, and every
# non-ASCII result must be declared UTF-8.

off_u <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}
off_inv <- function(...) {
  x <- rawToChar(as.raw(c(...)))
  Encoding(x) <- "UTF-8"
  x
}
off_bytes <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) NA_character_ else paste(charToRaw(s), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}
expect_same_bytes <- function(object, expected) {
  expect_identical(off_bytes(object), off_bytes(off_u(expected)))
  high <- !is.na(object) & grepl("[^\\x01-\\x7f]", object, perl = TRUE,
                                       useBytes = TRUE)
  expect_true(all(Encoding(object[high]) == "UTF-8"))
}

test_that("the WHATWG backslash rewrite cuts at the same spans", {
  url <- off_u(c(
    "http:\\\\h\\p\\q?x=\\y#\\z", "\u00e9http:\\\\h\\p",
    "http:\\\\\u00e9\u4f8b\\p\\q?a\\b", "\ufeffhttp:\\\\h\\p",
    "\u200bhttp:\\\\h\\p", "http:h\\p", "https:/\\h/p?q", "file:\\\\h\\p",
    "file:///p", "http://h/p\n", "http:\\\\h\\p\u2028", "http:\\\\h\\p\nq",
    NA, "", "mailto:a\\b",
    off_inv(c(charToRaw("http:\\\\h\\"), 0xff, charToRaw("\\p?q"))),
    "http:\\\\h\ufeff\\p", "http:\\\\\u00e9"
  ))
  r <- rurl:::.rewrite_whatwg_backslashes_vec(url, "whatwg")
  expect_same_bytes(r$url, c(
    "http://h/p/q?x=\\y#\\z", "\u00e9http:\\\\h\\p",
    "http://\u00e9\u4f8b/p/q?a\\b",
    "\ufeffhttp:\\\\h\\p", "\u200bhttp:\\\\h\\p", "http://h/p", "https://h/p?q",
    "file://h/p", "file:///p", "http://h/p\n", "http://h/p\u2028",
    "http://h/p\nq", NA, "", "mailto:a\\b",
    off_inv(c(charToRaw("http://h/"), 0xff, charToRaw("/p?q"))),
    "http://h\ufeff/p", "http://\u00e9"
  ))
  expect_identical(r$backslash_rewritten, c(
    TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
    TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE
  ))
})

test_that("the UTS-46 full-stop mapping cuts the host at the same bytes", {
  url <- off_u(c(
    "http://a\u3002b/p\u3002q", "http://u\u3002i@h\uff0ex/p",
    "\u00e9\u4f8b://a\u3002b/", "//a\uff61b?q", "http://\ufeffu@h\u3002x/",
    "http://\u200bu@h\u3002x/", "http://[a\u3002b]/", "\ufeffhttp://a\u3002b/",
    "http://a\u3002b\n/p", "http://a\u3002b/p\nq", "http://a\u3002b/p\u2028",
    "http://a\u3002b/p\r\n", "http://x\ny@a\u3002b/p", NA, "", "http://a.b/",
    off_inv(c(charToRaw("http://a"), 0xe3, 0x80, 0x82, charToRaw("b/"), 0xff)),
    "http://a@b@c\u3002d:80#f", "http:a\u3002b"
  ))
  r <- rurl:::.map_whatwg_domain_separators_vec(url, "whatwg")
  # An inner terminator after the authority leaves ICU's `(.*)$` unmatched,
  # so the row is not mapped; a final one is kept after the rebuilt host.
  expect_same_bytes(r$url, c(
    "http://a.b/p\u3002q", "http://u\u3002i@h.x/p", "\u00e9\u4f8b://a\u3002b/",
    "//a.b?q", "http://\ufeffu@h.x/", "http://\u200bu@h.x/",
    "http://[a\u3002b]/", "\ufeffhttp://a\u3002b/", "http://a.b\n/p",
    "http://a\u3002b/p\nq", "http://a.b/p\u2028", "http://a.b/p\r\n",
    "http://x\ny@a.b/p", NA, "", "http://a.b/",
    off_inv(c(charToRaw("http://a.b/"), 0xff)), "http://a@b@c.d:80#f",
    "http:a\u3002b"
  ))
})

test_that("the userinfo charset encoding rebuilds from the same captures", {
  url <- off_u(c(
    "http://a b@h/p", "http://\u00e9 x@h/p?q#f", "http://a\u0001b@c d@h/",
    "http://ab@h/", "http://a b@h/p\n", "http://a b@h/p\nq",
    "http://a b@h/p\u2028", "http://a\nb@h/p", "\ufeffhttp://a b@h/",
    "http://\ufeffa b@h/", "http://\u200ba b@h/", NA, "", "//a b@h/",
    off_inv(c(charToRaw("http://a "), 0xff, charToRaw("@h/"))),
    "http://a b@h\r\n"
  ))
  r <- rurl:::.encode_userinfo_charset_vec(url, "whatwg")
  # ICU's `(.*)$` leaves a final terminator out of the last capture, so a
  # rebuilt row drops it; an inner one after the authority is no match.
  expect_same_bytes(r$url, c(
    "http://a%20b@h/p", "http://\u00e9%20x@h/p?q#f", "http://a%01b@c%20d@h/",
    "http://ab@h/", "http://a%20b@h/p", "http://a b@h/p\nq",
    "http://a%20b@h/p", "http://a%0Ab@h/p", "\ufeffhttp://a b@h/",
    "http://\ufeffa%20b@h/", "http://\u200ba%20b@h/", NA, "", "//a b@h/",
    off_inv(c(charToRaw("http://a%20"), 0xff, charToRaw("@h/"))),
    "http://a%20b@h\r\n"
  ))
})

test_that("the RFC 3986 path-rootless split keeps its captures", {
  url <- off_u(c(
    "http:ex.com/p?q#f", "https:a.b", "ftp:a.b.c?x", "http:ex.com/p\n",
    "http:ex.com/p\nx", "http:ex.com\u2028",
    "http:ex.com/\u00e9\u4f8b?\u00e9#\u4f8b",
    "\ufeffhttp:ex.com/p", "http:\ufeffex.com/p", "http:ex.com#a?b",
    "http:ex.com/p\r\n", NA, "", "http:/ex.com", "mailto:a.b",
    off_inv(c(charToRaw("http:ex.com/"), 0xff, charToRaw("?q#f")))
  ))
  r <- rurl:::.rfc3986_path_rootless_vec(url, "rfc3986")
  expect_identical(r$is_path_rootless, c(
    TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
    FALSE, FALSE, FALSE, FALSE, TRUE
  ))
  expect_identical(r$scheme, c(
    "http", "https", "ftp", "http", NA, "http", "http", NA, NA, "http",
    "http", NA, NA, NA, NA, "http"
  ))
  expect_same_bytes(r$path, c(
    "ex.com/p", "a.b", "a.b.c", "ex.com/p", NA, "ex.com", "ex.com/\u00e9\u4f8b",
    NA, NA, "ex.com", "ex.com/p", NA, NA, NA, NA,
    off_inv(c(charToRaw("ex.com/"), 0xff))
  ))
  expect_same_bytes(r$query, c(
    "q", NA, "x", NA, NA, NA, "\u00e9", NA, NA, NA, NA, NA, NA, NA, NA, "q"
  ))
  expect_same_bytes(r$fragment, c(
    "f", NA, NA, NA, NA, NA, "\u4f8b", NA, NA, "a?b", NA, NA, NA, NA, NA, "f"
  ))
})

test_that("the WHATWG file parser cuts host and path at the same bytes", {
  url <- off_u(c(
    "file:///C:/x", "file://host/share/p", "file:\\\\srv\\s\\p", "file://C|/p",
    "file://c:", "file:/p?q#f", "file:", "FILE://h/p", "file://\u00e9\u4f8b/p",
    "file://h/\u00e9?\u4f8b#x", "file://h/p\n", "file://h/p\nx",
    "file://h\n/p", "file:///p\u2028", "file://h/p\r\n", "file://\ufeffh/p",
    "file://\u200bh/p", "file:\ufeff/p", "file://h", "file:////p",
    "file://h/q"
  ))
  r <- rurl:::.parse_whatwg_file_urls_vec(url)
  expect_identical(r$ok, c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
  ))
  expect_same_bytes(r$host, c(
    "", "host", "srv", "", "", "", "", "h", "xn--9ca4731b", "h", "h", NA, NA,
    "", "h", "h", "h", "", "h", "", "h"
  ))
  expect_same_bytes(r$path, c(
    "/C:/x", "/share/p", "/s/p", "/C:/p", "/c:", "/p", "/", "/p", "/p",
    "/\u00e9", "/p", NA, NA, "/p", "/p", "/p", "/p", "/\ufeff/p", "/", "//p",
    "/q"
  ))
  expect_same_bytes(r$query, c(
    NA, NA, NA, NA, NA, "q", NA, NA, NA, "\u4f8b", rep(NA, 11)
  ))
  expect_same_bytes(r$fragment, c(
    NA, NA, NA, NA, NA, "f", NA, NA, NA, "x", rep(NA, 11)
  ))
})

test_that("the raw path is cut from the prepared row at the same bytes", {
  prepared <- off_u(c(
    "http://h/p/q?x#y", "http://\u00e9\u4f8b/p\u00e9?q", "http://\ufeffh/p",
    "http://\u200bh/p", "http:///p", "http://h", "http://h?q/x",
    "http://h/p#f?x", "h/p", "http://h/p\n", "http://h/\u2028/x",
    off_inv(c(charToRaw("http://h/caf"), 0xe9, charToRaw("?q"))),
    "http://h/\ufeffp", NA
  ))
  engine <- c(rep("/E", 13), NA)
  # The undecodable row keeps its octets: the escape the frozen NULL profile
  # ran here went with it (ADR 0020).
  expect_same_bytes(rurl:::.extract_raw_path_vec(prepared, engine), c(
    "/p/q", "/p\u00e9", "/p", "/p", "/E", "/E", "/E", "/p", "/p", "/p\n",
    "/\u2028/x", off_inv(c(charToRaw("/caf"), 0xe9)), "/\ufeffp", NA
  ))
})

test_that("the WHATWG IPv6 serializer splits and cuts as before", {
  host <- c(
    "[::1]", "[1:2:3:4:5:6:7::]", "[::127.0.0.1]", "[1::]",
    "[0:0:0:0:0:0:0:1]", "[1:0:0:2:0:0:0:3]", "[::ffff:1.2.3.4]", "[8:]",
    "[1::2::3]", "[::1]\n", "[::1]\r\n", "[::1]\u2028",
    "[1:2:3:4:5:6:1.2.3.4]", "[1:2:3:4:5:6:7:8]", "[::]", "[1:2:3:4:5:6:7]",
    "[1:2:3:4:5:6:7:]", "[:1:2:3:4:5:6:7]"
  )
  got <- vapply(off_u(host), rurl:::.serialize_whatwg_ipv6_host, character(1),
                USE.NAMES = FALSE)
  expect_same_bytes(got, c(
    "[::1]", "[1:2:3:4:5:6:7:0]", "[::7f00:1]", "[1::]", "[::1]",
    "[1:0:0:2::3]", "[::ffff:102:304]", "[8:]", "[1::2::3]", "[::1]\n",
    "[::1]\r\n", "[::1]\u2028", "[1:2:3:4:5:6:102:304]", "[1:2:3:4:5:6:7:8]",
    "[::]", "[1:2:3:4:5:6:7]", "[1:2:3:4:5:6:7:]", "[:1:2:3:4:5:6:7]"
  ))
})

test_that("the IPv6 serializer's unchecked pattern and split hold", {
  # The per-host serializer runs its constant without the byte layer's
  # pattern check (as `.const_detect()` does); the check runs here.
  expect_true(rurl:::.bytes_check_pattern(rurl:::.IPV6_QUAD_TAIL,
                                          fixed = FALSE))
  # Its split is `.bytes_split_fixed(keep_trailing_empty = TRUE)`.
  s <- c("", ":", "::", ":::", "::::", "1::", "::1", "1::2", "1:2:", "a::b::",
         "1:::2", "8:")
  for (sep in c("::", ":")) {
    for (x in s) {
      expect_identical(
        rurl:::.const_split_keep_trailing(x, sep),
        rurl:::.bytes_split_fixed(x, sep, keep_trailing_empty = TRUE)[[1L]],
        info = paste(sep, x)
      )
    }
  }
  # A long run of groups stays linear: the atomic group never retries the
  # quad after an earlier ":".
  long <- paste0("[", strrep("1:", 2e5), "1.2.3.4]")
  expect_identical(rurl:::.serialize_whatwg_ipv6_host(long), long)
})

test_that("the www policies capture the bare host up to ICU's `.*` stop", {
  host <- off_u(c(
    "www.ex.com", "WWW3.Ex.com", "Www.\u00e9\u4f8b.com", "www.ex.com\nx",
    "www.ex\u2028.com", "www.ex.com\r\n", "ex.com", "\ufeffwww.ex.com",
    "\u200bwww.ex.com", "www.\ufeffex.com", "www12.a.b",
    off_inv(c(charToRaw("www."), 0xff, charToRaw(".com")))
  ))
  # The helper's non-WHATWG reading: whatwg maps the host first.
  keep <- rurl:::.apply_www_policy_vec(host, "keep", rep(FALSE, 12),
                                       url_standard = "rfc3986")
  expect_same_bytes(keep, c(
    "www.ex.com", "www.Ex.com", "www.\u00e9\u4f8b.com", "www.ex.com", "www.ex",
    "www.ex.com", "www.ex.com", "www.\ufeffwww.ex.com", "www.\u200bwww.ex.com",
    "www.\ufeffex.com", "www.a.b", off_inv(c(charToRaw("www."), 0xff,
                                             charToRaw(".com")))
  ))
  ins <- rurl:::.apply_www_if_no_subdomain_vec(host)
  expect_same_bytes(ins, c(
    "www.ex.com", "www.Ex.com", "www.\u00e9\u4f8b.com", "www.ex.com", "www.ex",
    "www.ex.com", "www.ex.com", "\ufeffwww.ex.com", "\u200bwww.ex.com",
    "www.\ufeffex.com", "www.a.b", off_inv(c(charToRaw("www."), 0xff,
                                             charToRaw(".com")))
  ))
  expect_identical(
    rurl:::.apply_www_policy_vec(host, "if_no_subdomain", rep(FALSE, 12),
                                 url_standard = "rfc3986"),
    ins
  )
  expect_identical(rurl:::.apply_www_if_no_subdomain_vec(character(0)),
                   character(0))
})

test_that("the subdomain trim cuts the www prefix at the same byte", {
  host <- off_u(c(
    "www.a.ex.com", "WWW.b.ex.co.uk", "www.\u00e9.\u4f8b.com",
    "\ufeffwww.a.ex.com", "\u200bwww.a.ex.com", "a.b.ex.com", "www.ex.com"
  ))
  dom <- off_u(c(
    "ex.com", "ex.co.uk", "\u4f8b.com", "ex.com", "ex.com", "ex.com", "ex.com"
  ))
  expect_same_bytes(
    rurl:::.apply_subdomain_policy_vec(host, dom, 0, rep(FALSE, 7),
                                       url_standard = "rfc3986"),
    c("www.ex.com", "www.ex.co.uk", "www.\u4f8b.com", "ex.com", "ex.com",
      "ex.com", "www.ex.com")
  )
  expect_same_bytes(
    rurl:::.apply_subdomain_policy_vec(host, dom, 1, rep(FALSE, 7),
                                       url_standard = "rfc3986"),
    c("www.a.ex.com", "www.b.ex.co.uk", "www.\u00e9.\u4f8b.com", "a.ex.com",
      "a.ex.com", "b.ex.com", "www.ex.com")
  )
})

test_that("the public API keeps its values over the moved chains", {
  url <- off_u(c(
    "http:\\\\\u00e9\u4f8b.com\\p\\q?a\\b",
    "http://u\u3002i@h\u3002example.com/p\u3002q", "http://a b@example.com/p",
    "file://host/\u00e9?q#f", "file://C:/x", "http://[1:0:0:2:0:0:0:3]/",
    "http://[::127.0.0.1]/", "http://example.com/p\u00e9?q",
    "\ufeffhttp://example.com/p", "\u200bhttp://example.com/p",
    off_inv(c(charToRaw("http://example.com/caf"), 0xe9, charToRaw("?q"))), NA
  ))
  w <- safe_parse_urls(url, url_standard = "whatwg")
  expect_same_bytes(w$clean_url, c(
    "http://\u00e9\u4f8b.com/p/q", "http://h.example.com/p\u3002q",
    "http://example.com/p", "file://host/\u00e9", "file:///C:/x",
    "http://[1:0:0:2::3]/", "http://[::7f00:1]/", "http://example.com/p\u00e9",
    NA, NA, "http://example.com/caf\ufffd", NA
  ))
  expect_same_bytes(w$host, c(
    "\u00e9\u4f8b.com", "h.example.com", "example.com", "host", NA,
    "[1:0:0:2::3]", "[::7f00:1]", "example.com", NA, NA, "example.com", NA
  ))
  # The default is whatwg (ADR 0020).
  expect_identical(safe_parse_urls(url), w)
  expect_same_bytes(w$path, c(
    "/p/q", "/p\u3002q", "/p", "/\u00e9", "/C:/x", "/", "/", "/p\u00e9", NA,
    NA, "/caf\ufffd", NA
  ))
  r <- safe_parse_urls(off_u(c(
    "http:example.com/\u00e9?q#f", "https:a.example.com", "http:example.com/p"
  )), url_standard = "rfc3986")
  # Row 1's raw U+00E9 is a parse error under rfc3986 (RFC 3986 sections 2,
  # 2.5; RUL-030, RURL-pntbujzc); the ASCII rows keep their offsets.
  expect_same_bytes(r$path, c(NA, "a.example.com", "example.com/p"))
  expect_identical(r$query, c(NA_character_, NA, NA))
  hosts <- off_u(c(
    "WWW3.Example.com/p", "www.\u00e9\u4f8b.com/", "a.b.example.com/",
    "\ufeffwww.example.com/"
  ))
  # Under the default whatwg, UTS-46 maps the leading U+FEFF away first.
  expect_same_bytes(safe_parse_urls(hosts, www_handling = "keep")$host, c(
    "www.example.com", "www.\u00e9\u4f8b.com", "www.a.b.example.com",
    "www.example.com"
  ))
  expect_same_bytes(
    safe_parse_urls(hosts, www_handling = "if_no_subdomain")$host,
    c("www.example.com", "www.\u00e9\u4f8b.com", "a.b.example.com",
      "www.example.com")
  )
  expect_same_bytes(
    safe_parse_urls(hosts, subdomain_levels_to_keep = 0)$host,
    c("example.com", "www.\u00e9\u4f8b.com", "example.com",
      "www.example.com")
  )
  k <- get_url_key(url)
  expect_identical(unclass(k)[c(1, 4, 8, 11)], c(
    "1:21:16:whatwg5:exact8:explicit4:http1:T7:present16:xn--9ca4731b.com-:4:list7:abempty4:/p/q7:present3:a\\b", # nolint: line_length_linter.
    "1:21:16:whatwg5:exact8:explicit4:file1:T7:present4:host-:4:list7:abempty7:/%C3%A97:present1:q", # nolint: line_length_linter.
    "1:21:16:whatwg5:exact8:explicit4:http1:T7:present11:example.com-:4:list7:abempty8:/p%C3%A97:present1:q", # nolint: line_length_linter.
    "1:21:16:whatwg5:exact8:explicit4:http1:T7:present11:example.com-:4:list7:abempty13:/caf%EF%BF%BD7:present1:q" # nolint: line_length_linter.
  ))
  expect_identical(attr(k, "keyability")[9:12], c(
    "invalid-parse", "invalid-parse", "ok", "missing-input"
  ))
})

test_that("the userinfo split finds the last \"@\" of a long authority", {
  # The authority after the "@" is just long enough that the greedy
  # `[^/?#]*@` exceeded PCRE's match limit by backtracking byte by byte: the
  # "@" was not found and the userinfo was mapped as host (RURL-yqmoztoj).
  a <- strrep("a", 1e7)
  url <- off_u(paste0("http://u。i@h", a, "。b/p"))
  expect_no_warning(
    r <- rurl:::.map_whatwg_domain_separators_vec(url, "whatwg")
  )
  expect_true(identical(r$url, off_u(paste0("http://u。i@h", a, ".b/p"))))
  # With no "@" the failing locate backtracked as far, and only warned.
  url <- off_u(paste0("http://h", a, "。b/p"))
  expect_no_warning(
    r <- rurl:::.map_whatwg_domain_separators_vec(url, "whatwg")
  )
  expect_true(identical(r$url, off_u(paste0("http://h", a, ".b/p"))))
  # The last "@" before the first "/", "?" or "#" still wins.
  url <- off_u(c(
    "http://a@b。c@d。e/f@g。h", "http://a。b?c@d。e",
    "http://a。b#c@d", "http://@a。b", "http://a@。b",
    "http://a@@b。c"
  ))
  expect_same_bytes(rurl:::.map_whatwg_domain_separators_vec(url, "whatwg")$url,
                    c("http://a@b。c@d.e/f@g。h", "http://a.b?c@d。e",
                      "http://a.b#c@d", "http://@a.b", "http://a@.b",
                      "http://a@@b.c"))
})

test_that("the IPv6 quad tail stays linear on a long last group", {
  # The last group is just long enough that `(?>.*:)` exceeded PCRE's match
  # limit by backtracking to the last ":" (RURL-yqmoztoj).
  host <- paste0("[1:", strrep("1", 1e7), "]")
  expect_no_warning(got <- rurl:::.serialize_whatwg_ipv6_host(host))
  expect_true(identical(got, host))
})

test_that("the WHATWG file parser splits an invalid-UTF-8 rest per row", {
  url <- off_u(c("file:/ok", off_inv(c(charToRaw("file:/a"), 0xff,
                                       charToRaw("b?q#f"))), "file:/p?q#f"))
  r <- rurl:::.parse_whatwg_file_urls_vec(url)
  expect_identical(r$ok, c(TRUE, TRUE, TRUE))
  expect_identical(r$host, c("", "", ""))
  expect_same_bytes(r$path, c("/ok", off_inv(c(charToRaw("/a"), 0xff,
                                               charToRaw("b"))), "/p"))
  expect_identical(r$query, c(NA, "q", "q"))
  expect_identical(r$fragment, c(NA, "f", "f"))
})

test_that("a U+FEFF-led file: row stays off the WHATWG file parser", {
  # `.parse_whatwg_file_urls_vec()` does not mask a row led by the mark; its
  # caller never selects one, because its `^[Ff][Ii][Ll][Ee]:` test reads the
  # mark as data too. The public whatwg path keeps main's value.
  url <- off_u(c("\ufefffile:///C:/x", "\ufefffile:/p?q#f",
                 "\ufefffile://host/share/p"))
  w <- safe_parse_urls(url, url_standard = "whatwg")
  expect_identical(w$parse_status, rep("error", 3))
  expect_identical(w$clean_url, rep(NA_character_, 3))
  expect_identical(w$host, rep(NA_character_, 3))
  expect_identical(w$path, rep(NA_character_, 3))
  expect_identical(attr(get_url_key(url), "keyability"),
                   rep("invalid-parse", 3))
  # Called directly, the byte-mode capture finds no `file:` scheme there;
  # stringi read past the mark and parsed these rows.
  expect_identical(rurl:::.parse_whatwg_file_urls_vec(url)$ok,
                   rep(FALSE, 3))
})

test_that("the raw path of a U+FEFF-led prepared row is cut where located", {
  # Unreachable from the public API: Phase 1 prefixes "http://" to every row
  # led by U+FEFF, so no prepared row starts with the mark. Under stringi the
  # locate counted the mark and the cut read past it, landing one character
  # late; in bytes the locate and the cut both count it, so the path starts
  # at the located "/". The byte value is the consistent one.
  prepared <- off_u(c("\ufeffhttp://h/p/q?x", "\ufeffhttp://\u00e9/p#f"))
  expect_same_bytes(
    rurl:::.extract_raw_path_vec(prepared, c("/E", "/E")),
    c("//h/p/q", "//\u00e9/p")
  )
})
