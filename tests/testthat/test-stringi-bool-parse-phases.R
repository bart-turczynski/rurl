# Pins over the R/parse-phases.R helpers whose boolean and replacement calls
# move from stringi to base R in byte mode (RURL-oahhlnvf, ADR 0019). Each pin
# holds a behavior the old ICU calls had and the byte-mode calls must keep:
# ICU's `$` (which also matches before a final line terminator: LF, CR, CRLF,
# VT, FF, U+0085, U+2028, U+2029), ICU's `.` (which matches none of them), a
# leading U+FEFF read as data next to its U+200B twin (ADR 0005), and the
# UTF-8 declaration on a rewritten result.

utf8_bytes <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) NA_character_ else paste(charToRaw(s), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

test_that("the host classifier reads ICU `$` and `.` as before", {
  cls <- rurl:::.classify_input_host_vec(c(
    "http://u@a.b.com:80/x", "//[::1]:8/x", "a.b", ".a.b", "a..b", "a.b.",
    "1.2.3.4", "0x7f.1", "1.2.", "1.2\n", "1.2.3.4\u2028", "h/a\nb/c",
    "a\nb@c.d/x", "[::1]\nx", "h:1:2"
  ))
  expect_identical(cls$host_token, c(
    "a.b.com", "[::1]", "a.b", ".a.b", "a..b", "a.b.",
    "1.2.3.4", "0x7f.1", "1.2.", "1.2\n", "1.2.3.4\u2028", "h/a\nb",
    "a\nb@c.d", "[::1]\nx", "h:1"
  ))
  expect_identical(cls$has_userinfo, c(
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, TRUE, FALSE, FALSE
  ))
  expect_identical(cls$is_dotted_name, c(
    TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
    TRUE, FALSE, TRUE, FALSE, FALSE
  ))
  # ICU `$` lets an IPv4 attempt end in a final LF or U+2028.
  expect_identical(cls$is_ipv4ish, c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
    TRUE, FALSE, FALSE, FALSE, FALSE
  ))
  expect_identical(cls$is_ipish, cls$is_ipv4ish | c(
    FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, TRUE, TRUE
  ))
})

test_that("the host classifier reads a leading U+FEFF as data", {
  cls <- rurl:::.classify_input_host_vec(c(
    "http://\ufeff[::1]/", "http://\u200b[::1]/", "\ufeffa.b", "\u200ba.b",
    "\ufeff1.2.3.4", NA
  ))
  # A mark-led host is not bracketed, so its last ":" starts the port.
  expect_identical(utf8_bytes(cls$host_token), utf8_bytes(c(
    "\ufeff[:", "\u200b[:", "\ufeffa.b", "\u200ba.b",
    "\ufeff1.2.3.4", NA
  )))
  expect_identical(cls$is_dotted_name, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(cls$is_ipish, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(cls$is_canonical_ip, rep(FALSE, 6))
})

test_that("the strict IP detector keeps ICU `$` at a final terminator", {
  expect_identical(
    rurl:::.detect_ip_host_vec(c(
      "1.2.3.4", "1.2.3.4\n", "1.2.3.4\u2028", "1.2.3.4\r\n", "1.2.3.4\nx",
      "01.2.3.4", "[::1]", "[::1]\r", "[::1.2.3.4]\u0085", "[::1", "::1",
      "\ufeff1.2.3.4", "\u200b1.2.3.4", "", NA
    )),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
      FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("ends-in-a-number reads ICU `$` and `.` as before", {
  expect_identical(
    rurl:::.host_ends_in_number_vec(c(
      "a.1", "a.1.", "a.0x", "a.0xg", "a.b", "1", "a.1\n", "a\n.1", "a.1.\n",
      "a..", "\ufeff1", NA, ""
    )),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE,
      FALSE, FALSE, FALSE)
  )
})

test_that("WHATWG step 1 trims and strips by the same classes", {
  cc <- rurl:::.strip_whatwg_control_chars_vec(c(
    " \ta\nb\r ", "\ufeffa ", "\u200ba ", "a\u2028", "\x01a\x01", "ok", NA,
    "a \u2028"
  ), "whatwg")
  expect_identical(utf8_bytes(cc$url), utf8_bytes(c(
    "ab", "\ufeffa", "\u200ba", "a\u2028", "a", "ok", NA, "a \u2028"
  )))
  expect_identical(cc$leading_trailing_stripped,
                   c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(cc$control_char_stripped,
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(Encoding(cc$url[2]), "UTF-8")
  off <- rurl:::.strip_whatwg_control_chars_vec(" a ", "rfc3986")
  expect_identical(off$url, " a ")
})

test_that("the browser fixer trims to ICU `$` and repairs special schemes", {
  fx <- function(x, ...) rurl:::.apply_browser_fixup_vec(x, "browser", ...)
  expect_identical(
    utf8_bytes(fx(c(
      "  HTTP;example.com ", "https;h/p", "http:/a", "Ftp:h", "ftps;h",
      "file;///a", "file:p", "x \u2028", "x \u0085", "x \r\n", "\ufeffhttp;h",
      "\u200bhttp;h", NA
    ))),
    utf8_bytes(c(
      "HTTP://example.com", "https://h/p", "http://a", "Ftp://h", "ftps;h",
      "file:///a", "file:p", "x\u2028", "x\u0085", "x", "\ufeffhttp;h",
      "\u200bhttp;h", NA
    ))
  )
  expect_identical(
    fx(c("/Users/me", "//h", "/\t/h", "/\\h", "a"), "whatwg", "infer"),
    c("file:///Users/me", "//h", "/\t/h", "/\\h", "a")
  )
  expect_identical(fx("/Users/me", "rfc3986", "infer"), "/Users/me")
  expect_identical(rurl:::.apply_browser_fixup_vec(" x ", "none"), " x ")
})

test_that("the WHATWG backslash rewrite and its facts hold", {
  bs <- rurl:::.rewrite_whatwg_backslashes_vec(c(
    "http:\\\\h\\p?q\\r#f\\g", "http://h/a", "http:h/p", "file:///a",
    "file:\\\\h\\x", "foo:\\a", "\ufeffhttp:\\\\h"
  ), "whatwg")
  expect_identical(bs$url, c(
    "http://h/p?q\\r#f\\g", "http://h/a", "http://h/p", "file:///a",
    "file://h/x", "foo:\\a", "\ufeffhttp:\\\\h"
  ))
  expect_identical(bs$backslash_rewritten,
                   c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
})

test_that("WHATWG full-stop variants map in the host only", {
  m <- rurl:::.map_whatwg_domain_separators_vec(c(
    "http://a\u3002b\uff0ec\uff61d/x\u3002y", "http://u\u3002v@c\u3002d/",
    "http://[\u3002]/", "foo:a\u3002b", "\ufeffhttp://a\u3002b/"
  ), "whatwg")$url
  expect_identical(utf8_bytes(m), utf8_bytes(c(
    "http://a.b.c.d/x\u3002y", "http://u\u3002v@c.d/", "http://[\u3002]/",
    "foo:a\u3002b", "\ufeffhttp://a\u3002b/"
  )))
})

test_that("the WHATWG userinfo charset encodes from the last @", {
  out <- rurl:::.encode_userinfo_charset_vec(c(
    "http://a b@h/", "http://a@b c@h/", "http://ab@h/", "http://h/a b",
    "http://a\x7f\tb@h/", "\ufeffhttp://a b@h/", "http://\ufeffa b@h/"
  ), "whatwg")$url
  expect_identical(utf8_bytes(out), utf8_bytes(c(
    "http://a%20b@h/", "http://a@b%20c@h/", "http://ab@h/", "http://h/a b",
    "http://a%7F%09b@h/", "\ufeffhttp://a b@h/", "http://\ufeffa%20b@h/"
  )))
  expect_identical(
    utf8_bytes(rurl:::.percent_encode_userinfo_charset(
      c("\ufeffa b", "%20 x", NA)
    )),
    utf8_bytes(c("\ufeffa%20b", "%20%20x", NA))
  )
})

test_that("the RFC 3986 rootless-path carve-out reads the same rows", {
  r <- rurl:::.rfc3986_path_rootless_vec(c(
    "http:a.b/c", "http:a.b", "http:ab/c", "http:/a.b", "http:a.b\\c",
    "http:.a.b", "http:a.b.", "http:a.b\n", "http:a.b\nx/y", "ftp:a~.b?q#f",
    "\ufeffhttp:a.b", "mailto:a.b"
  ), "rfc3986")
  expect_identical(r$is_path_rootless, c(
    TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
    FALSE
  ))
})

test_that("the WHATWG file helpers keep ICU `$` and the %2e spellings", {
  expect_identical(
    rurl:::.whatwg_file_drive_path(c("/C|/x", "/C|", "/C|\n", "/C|x", "/CC|",
                                     NA)),
    c("/C:/x", "/C:", "/C:\n", "/C|x", "/CC|", NA)
  )
  expect_identical(
    rurl:::.whatwg_file_shorten_drive_path(c(
      "/C:/a/%2E%2e/..", "/C:/%2e/x", "/C:/a/.%2E", "/D:/../..", "/a/../b",
      "/C:x/..", NA
    )),
    c("/C:/", "/C:/x", "/C:/", "/D:/", "/a/../b", "/C:x/..", NA)
  )
  f <- rurl:::.parse_whatwg_file_urls_vec(c(
    "file://C:/x", "file://c|", "file:\\\\h\\a\\b", "file:/\ufeffp\\q",
    "file:\u200bp\\q", "file:///C|/a", "http://x"
  ))
  expect_identical(f$ok, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(f$host, c("", "", "h", "", "", "", NA))
  expect_identical(utf8_bytes(f$path), utf8_bytes(c(
    "/C:/x", "/c:", "/a/b", "/\ufeffp/q", "/\u200bp/q", "/C:/a", NA
  )))
})

test_that("the path normalizer masks find the same candidate rows", {
  np <- function(x, ...) rurl:::.normalize_path_vec(x, ...)
  expect_identical(
    np(c("/a/index.html", "/a/INDEX.PHP/", "/a/Default.aspx", "/a/index.",
         "/a/index.html/x", "/index.html\u2028", NA),
       "keep", "none", "strip", "none"),
    c("/a", "/a", "/a", "/a/index.", "/a/index.html/x", "/", NA)
  )
  expect_identical(
    np(c("/a/%2E/b", "/a/%2e%2E/b", "/a/./b", "/a/b"), "keep",
       "dot_segments", "keep", "none", ".whatwg_preserve"),
    c("/a/b", "/b", "/a/b", "/a/b")
  )
})

test_that("the www policies read the host as before", {
  expect_identical(
    rurl:::.apply_www_policy_vec(
      c("WWW.a.com", "www2.b.com", "\ufeffwww.c.com", "wwwx.d.com", "1.2.3.4"),
      "strip", c(FALSE, FALSE, FALSE, FALSE, TRUE)
    ),
    c("a.com", "b.com", "\ufeffwww.c.com", "wwwx.d.com", "1.2.3.4")
  )
})

test_that("the IPv6 host serializer picks the same rows", {
  expect_identical(
    rurl:::.serialize_whatwg_ipv6_hosts_vec(c(
      "[::127.0.0.1]", "[0:0::1]", "[::1]\n", "h", NA
    )),
    c("[::7f00:1]", "[::1]", "[::1]\n", "h", NA)
  )
})
