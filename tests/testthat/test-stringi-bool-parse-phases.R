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
      "strip", c(FALSE, FALSE, FALSE, FALSE, TRUE), url_standard = "rfc3986"
    ),
    c("a.com", "b.com", "\ufeffwww.c.com", "wwwx.d.com", "1.2.3.4")
  )
  # Under whatwg (the helper's default) the host is UTS-46-mapped first, so
  # the mark-led row is the bare `www.` host.
  expect_identical(
    rurl:::.apply_www_policy_vec(
      c("WWW.a.com", "www2.b.com", "\ufeffwww.c.com", "wwwx.d.com", "1.2.3.4"),
      "strip", c(FALSE, FALSE, FALSE, FALSE, TRUE)
    ),
    c("a.com", "b.com", "c.com", "wwwx.d.com", "1.2.3.4")
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

# The one behavior change in the slice (ADR 0019, decision 2). The browser
# fixer matched the special-scheme names with ICU's caseless `(?i)`, which
# also folds U+017F (long s) to "s", U+00DF (sharp s) to "ss" and U+FB01 (the
# fi ligature) to "fi". A scheme is matched ASCII case-insensitively (WHATWG
# URL Standard, "ASCII case-insensitive"; RFC 3986 section 3.1), so only the
# ASCII case variants are repaired now.
test_that("the browser fixer repairs ASCII case variants of a scheme only", {
  fx <- function(x) rurl:::.apply_browser_fixup_vec(x, "browser")
  expect_identical(
    fx(c("HTTPS;h", "hTtP:h", "WsS;h", "FiLe;///a", "fTp:/h")),
    c("HTTPS://h", "hTtP://h", "WsS://h", "FiLe:///a", "fTp://h")
  )
  folds <- c("http\u017f;h.com", "http\u017f:h", "w\u00df;h", "w\u017fs:h",
             "\ufb01le;///a")
  # Was "httpſ://h.com", "httpſ://h", "wß://h", "wſs://h", "ﬁle:///a".
  expect_identical(utf8_bytes(fx(folds)), utf8_bytes(folds))
  # Not a scheme, the long-s row now parses as a host, as its U+017F maps to
  # "s" under UTS #46: the infer seam reads it like "https;h.com".
  out <- safe_parse_urls("http\u017f;h.com", profile = "browser")
  expect_identical(out$host, "https;h.com")
  expect_identical(out$scheme, "http")
})

# The host:port routing test ends at the true end now (`\z`), as its twin
# in R/parse-state.R does: a port is digits only (WHATWG URL Standard, port
# state; RFC 3986 section 3.2.3), and ICU's `$` also took the point before a
# final U+0085, U+2028 or U+2029. Those rows no longer read as host:port.
test_that("the Phase-1 host:port test reads the true end", {
  x <- c("example.com:8080", "example.com:8080/p", "tel:123x",
         "example.com:8080\u2028", "example.com:8080\u0085", "tel:123\u2029",
         "tel:123\n")
  prep <- rurl:::.prepare_urls_for_parse_vec(
    x, "keep", "keep", "whatwg", "infer", "general"
  )
  # Was TRUE for the three rows ending in U+2028, U+0085 or U+2029; the LF
  # row is stripped by WHATWG step 1 first and stays a host:port.
  expect_identical(prep$looks_like_host_port,
                   c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
  # With the routing twin, such a row is keyed like its non-digit sibling
  # `tel:123x`: an explicit scheme and an opaque path.
  k <- get_url_key(c("tel:123x", "tel:123\u0085", "example.com:8080\u2028"),
                   url_key_policy("whatwg"))
  expect_identical(attr(k, "keyability"), c("ok", "ok", "ok"))
  expect_match(unclass(k), "8:explicit", fixed = TRUE)
  expect_match(unclass(k)[1:2], "8:explicit3:tel", fixed = TRUE)
  expect_match(unclass(k)[3L], "8:explicit11:example.com", fixed = TRUE)
  # With any scheme accepted, `h:1<U+2028>` was diverted to the host:port
  # route and failed; it now routes as `h:1x` does. Under `whatwg` it parses
  # as an opaque path.
  hp <- c("h:1x", "h:1\u2028", "h:1\u0085", "h:1\u2029")
  wg <- safe_parse_urls(hp, url_standard = "whatwg",
                        scheme_acceptance = "general")
  expect_identical(wg$scheme, c("h", "h", "h", "h"))
  expect_identical(wg$parse_status, c("ok", "ok", "ok", "ok"))
  # Under RFC 3986 the three terminator rows are now parse errors for the
  # raw non-ASCII code point itself, not for the routing: a URI is US-ASCII
  # (RFC 3986 sections 2, 2.5; RUL-030, RURL-pntbujzc).
  out <- safe_parse_urls(hp, profile = "rfc-syntax")
  expect_identical(out$scheme, c("h", NA, NA, NA))
  expect_identical(out$parse_status, c("ok", "error", "error", "error"))
  expect_identical(
    get_url_diagnostics(hp[-1L], "rfc3986", scheme_acceptance = "general"),
    rep(list("unicode-outside-rfc3986-uri"), 3L)
  )
})

# --- ICU's `.*` and `$`, exact and linear (RURL-oahhlnvf review) -------------

# Strings over the terminators, the delimiters the cuts look for, and the
# lead bytes 0xC2 and 0xE2 of a terminator and of other characters.
icu_fuzz_rows <- function(n, seed) {
  alpha <- enc2utf8(c(
    "a", "1", "0x", ".", "@", "/", "?", "#", "[", "]", ":", "\n", "\r",
    "\r\n", "\v", "\f", "\u0085", "\u2028", "\u2029", "\u00e9", "\u20ac",
    "\u00a2", "\u2027"
  ))
  withr::with_seed(seed, {
    x <- vapply(seq_len(n), function(i) {
      paste(sample(alpha, sample(0:8, 1L), TRUE), collapse = "")
    }, character(1))
  })
  unique(enc2utf8(c(x, "", "[", "[]", "[]\r\n", "a.1\r\n", "a\r\n", "\r\n")))
}

test_that("`.ICU_END` is ICU's `$`, a final CRLF taken whole", {
  x <- icu_fuzz_rows(4000, 11)
  got <- gsub(rurl:::.ICU_END, "X", x, perl = TRUE, useBytes = TRUE)
  Encoding(got) <- "UTF-8"
  expect_identical(utf8_bytes(got),
                   utf8_bytes(stringi::stri_replace_all_regex(x, "$", "X")))
  # ICU's `$` does not take the point between the CR and LF of a final CRLF.
  expect_identical(gsub(rurl:::.ICU_END, "X", "a\r\n", perl = TRUE),
                   "aX\r\nX")
})

test_that("the line-aware cuts are ICU's `.*` cuts", {
  x <- icu_fuzz_rows(4000, 12)
  st <- function(pat, rep) stringi::stri_replace_first_regex(x, pat, rep)
  expect_identical(utf8_bytes(rurl:::.icu_cut_tail(x)),
                   utf8_bytes(st("[/?#].*$", "")))
  expect_identical(utf8_bytes(rurl:::.icu_cut_through_last_at(x)),
                   utf8_bytes(st("^.*@", "")))
  expect_identical(utf8_bytes(rurl:::.icu_cut_after_bracket(x)),
                   utf8_bytes(st("^(\\[[^\\]]*\\]).*$", "$1")))
  expect_identical(rurl:::.ipv6_bracketed_shape(x),
                   stringi::stri_detect_regex(x, "^\\[.*\\]$"))
  last <- stringi::stri_replace_first_regex(st("\\.$", ""), "^.*\\.", "")
  expect_identical(
    rurl:::.host_ends_in_number_vec(x),
    x != "" & (stringi::stri_detect_regex(last, "^[0-9]+$") |
                 stringi::stri_detect_regex(last, "^0[xX][0-9a-fA-F]*$"))
  )
  expect_identical(rurl:::.icu_cut_tail(c(NA, "a/b")), c(NA, "a"))
  expect_identical(rurl:::.icu_cut_through_last_at(c(NA, "u@h")), c(NA, "h"))
  expect_identical(rurl:::.icu_cut_after_bracket(c(NA, "[a]b")), c(NA, "[a]"))
})

test_that("a multi-megabyte row is cut, not left whole by PCRE's limit", {
  # A group repeated per byte or per character hit PCRE's match limit here;
  # `sub()` only warned, left the row uncut, and the classifier read the
  # wrong host.
  big <- 6e6
  x <- enc2utf8(c(
    paste0("localhost/x", strrep("x", big)),
    paste0(strrep("u", big), "@localhost/p"),
    paste0("localhost/p", strrep("\u20ac", big / 3)),
    paste0(strrep("u@", big / 2), "localhost/p")
  ))
  expect_no_warning(cls <- rurl:::.classify_input_host_vec(x))
  expect_identical(cls$host_token, rep("localhost", 4L))
  expect_identical(cls$has_userinfo, c(FALSE, TRUE, FALSE, TRUE))
  expect_no_warning(out <- safe_parse_urls(x[1:2]))
  expect_identical(out$host, c("localhost", "localhost"))
  expect_identical(out$parse_status, c("warning-no-tld", "warning-userinfo"))
  expect_no_warning(ein <- rurl:::.host_ends_in_number_vec(c(
    paste0(strrep("a.", big / 2), "1"), enc2utf8(strrep("\u20ac.", big / 4))
  )))
  expect_identical(ein, c(TRUE, FALSE))
  v6 <- enc2utf8(paste0("[", strrep("\u20ac", big / 3), "]"))
  expect_no_warning(expect_identical(rurl:::.serialize_whatwg_ipv6_host(v6),
                                     v6))
  # The embedded-IPv4 test retried its dotted quad after every ":".
  colons <- paste0("[", strrep("1:", big / 2), "]")
  expect_no_warning(expect_identical(
    rurl:::.detect_ip_host_vec(c(colons, paste0(colons, "x"))), c(TRUE, FALSE)
  ))
})

test_that("the path cut is linear past an inner line terminator", {
  # The cut retried every "/" before the terminator, each time scanning to
  # it: quadratic, and 1e5 segments did not finish.
  x <- paste0("http://a.com/", strrep("x/", 1e5), "\nq")
  elapsed <- system.time(
    cls <- rurl:::.classify_input_host_vec(x)
  )[["elapsed"]]
  # The last line holds no "/", so nothing is cut, as under ICU.
  expect_true(identical(cls$host_token, substring(x, 8L)))
  expect_lt(elapsed, 5)
})

test_that("each constant the helpers run unchecked passes the byte check", {
  # `.const_detect()`, `.const_replace()` and the line-aware cuts call base R
  # without `.bytes_check_pattern()`, for speed; this is where their
  # patterns are checked: no locale-table class, no bare `$`, no `(?i)`.
  pats <- list(
    end = .ICU_END, term = .ICU_TERMINATOR,
    final = .ICU_FINAL_TERMINATOR, inner = .ICU_INNER_TERMINATOR,
    from_delim = .ICU_FROM_PATH_DELIM, last_at = .ICU_THROUGH_LAST_AT,
    bracket = .ICU_BRACKET_HEAD, v6_shape = .IPV6_BRACKETED_SHAPE,
    v4 = .ICU_IPV4_CANONICAL, v6_tail = .ICU_IPV6_IPV4_TAIL,
    v6_hex = .ICU_IPV6_HEX_SHAPE, v6_ser = .ICU_IPV6_SERIALIZABLE,
    trailing_dot = .ICU_TRAILING_DOT, number = .ICU_NUMBER_LAST_LABEL
  )
  for (nm in names(pats)) {
    expect_true(.bytes_check_pattern(pats[[nm]], fixed = FALSE), info = nm)
  }
})

test_that("the per-host helpers do not recheck their constant patterns", {
  local_mocked_bindings(
    .bytes_check_pattern = function(...) stop("pattern rechecked")
  )
  expect_identical(.host_ends_in_number_vec(c("a.b.1", "a.0x", "a.b", NA)),
                   c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(.serialize_whatwg_ipv6_hosts_vec("[0:0::1]"), "[::1]")
})

test_that("no other migrated pattern dropped a non-ASCII fold", {
  # ICU's caseless match folds nothing else onto "e", "w", "index" or
  # "default", so these rows read as they did.
  expect_identical(
    rurl:::.apply_www_policy_vec(c("\u1e98ww.a.com", "WwW.a.com"), "strip",
                                 c(FALSE, FALSE)),
    c("\u1e98ww.a.com", "a.com")
  )
  np <- function(x) {
    rurl:::.normalize_path_vec(x, "keep", "none", "strip", "none")
  }
  expect_identical(np(c("/\u0130ndex.html", "/\u0131ndex.html", "/INDEX.x")),
                   c("/\u0130ndex.html", "/\u0131ndex.html", "/"))
})
