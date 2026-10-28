# RURL-oahhlnvf (ADR 0019): the boolean and replacement string calls in
# R/parse-state.R, R/diagnostics.R, R/parse-web.R, R/serialize.R,
# R/email-diagnostics.R and R/verdicts.R run in base R byte mode instead of
# stringi. Each test below pins one property the move had to keep or fix.

# --- the patterns the per-row helpers pass to grepl() directly ---------------

test_that("each pattern the hot paths run unchecked passes the byte check", {
  # `.state_bytes_test()` and the web dot-segment gate call grepl() without
  # `.bytes_check_pattern()`, for speed. This is where those patterns are
  # checked instead: no locale-table class, no bare `$`, no `(?i)`.
  pats <- list(
    host_bad = .RFC3986_HOST_BAD_RE,
    userinfo_bad = .RFC3986_USERINFO_BAD_RE,
    path_bad = .RFC3986_PATH_BAD_RE,
    qf_bad = .RFC3986_QF_BAD_RE,
    port = .RFC3986_PORT_RE,
    whatwg_port = .WHATWG_PORT_RE,
    ipv4 = .RFC3986_IPV4_RE,
    ipv6 = .RFC3986_IPV6_RE,
    ipvfuture = .RFC3986_IPVFUTURE_RE,
    bracket_ipvfuture = .RFC3986_BRACKET_IPVFUTURE_RE,
    forbidden_host = .WHATWG_FORBIDDEN_HOST_ONLY_BYTES,
    dot_segment = .WEB_DOT_SEGMENT_RE
  )
  for (nm in names(pats)) {
    expect_true(.bytes_check_pattern(pats[[nm]], fixed = FALSE), info = nm)
  }
})

test_that("the byte forbidden-host class is the code-point class", {
  # `.WHATWG_FORBIDDEN_HOST_ONLY_CP` (R/utils.R) is written for ICU, with a
  # ` ` escape that PCRE lacks; the byte class must match the same set.
  ascii <- vapply(1:127, intToUtf8, character(1))
  expect_identical(
    grepl(.WHATWG_FORBIDDEN_HOST_ONLY_BYTES, ascii, perl = TRUE,
          useBytes = TRUE),
    stringi::stri_detect_regex(ascii, .WHATWG_FORBIDDEN_HOST_ONLY_CP)
  )
})

# --- the RFC 3986 class search and its decodable mask ------------------------

test_that("the byte non-ASCII range admits every UTF-8 scalar, nothing else", {
  ok <- enc2utf8(c("\u0080", "߿", "ࠀ", "￿", "\U00010000",
                   "\U0010FFFF", "aéb", "﻿", "​"))
  expect_identical(.rfc3986_class_ok(ok, .RFC3986_HOST_BAD_RE),
                   rep(TRUE, length(ok)))
  # A stray octet is not a scalar value: the decodable mask rejects it, where
  # the byte range alone would admit it.
  bad <- c("a\x80", "\xc3", "\xff", "\xed\xa0\x80")
  Encoding(bad) <- "UTF-8"
  expect_identical(.rfc3986_class_ok(bad, .RFC3986_HOST_BAD_RE),
                   rep(FALSE, length(bad)))
  expect_identical(
    grepl(.RFC3986_HOST_BAD_RE, bad, perl = TRUE, useBytes = TRUE),
    rep(FALSE, length(bad))
  )
  # A latin1 element decodes by definition; its high byte is one non-ASCII
  # character, admitted as stringi admitted its UTF-8 translation.
  l1 <- "caf\xe9"
  Encoding(l1) <- "latin1"
  expect_true(.rfc3986_class_ok(l1, .RFC3986_HOST_BAD_RE))
})

test_that("the grammar's non-ASCII diagnostic reads bytes per row", {
  x <- enc2utf8(c("foo://h/é", "foo://h/a", "foo://h/﻿",
                  "foo://h/​", NA))
  # RUL-030: each non-ASCII row is rejected (RFC 3986 S2, S2.5); on the
  # ASCII grammar alone (the component walk) it passes.
  g <- .rfc3986_generic_uri_ok(x)
  expect_identical(g$ok, c(FALSE, TRUE, FALSE, FALSE, NA))
  expect_identical(.rfc3986_generic_uri_ok_walk(x),
                   c(TRUE, TRUE, TRUE, TRUE, NA))
  expect_identical(
    g$diagnostic,
    c("unicode-outside-rfc3986-uri", NA, "unicode-outside-rfc3986-uri",
      "unicode-outside-rfc3986-uri", NA)
  )
  inv <- "foo://h/\x80"
  Encoding(inv) <- "UTF-8"
  g2 <- .rfc3986_generic_uri_ok(c(inv, "foo://h/a"))
  expect_identical(g2$ok, c(FALSE, TRUE))
  expect_identical(g2$diagnostic, c(NA_character_, NA_character_))
})

# --- U+FEFF is data at every migrated helper ---------------------------------

test_that("a leading U+FEFF and its U+200B twin get the same answers", {
  for (lead in c("﻿", "​")) {
    lead <- enc2utf8(lead)
    info <- sprintf("U+%04X", utf8ToInt(lead))
    expect_false(.rfc3986_match(paste0(lead, "80"), .RFC3986_PORT_RE),
                 info = info)
    expect_false(.rfc_host_one(paste0(lead, "1.2.3.4"))$is_v4, info = info)
    expect_true(.rfc_host_one("1.2.3.4")$is_v4, info = info)
    expect_false(.rfc3986_valid_ip_literal(paste0(lead, "::1")), info = info)
    expect_true(.whatwg_opaque_host_one(paste0(lead, "h"))$ok, info = info)
    expect_false(.whatwg_opaque_host_one(paste0(lead, "h@"))$ok, info = info)
    expect_identical(
      .rfc_host_form(paste0(lead, "[v1.x]"), FALSE, FALSE, resolve = TRUE),
      "reg-name", info = info
    )
    # One "@" after the mark: a userinfo of the mark alone, admitted as a
    # non-ASCII scalar. Two are a grammar failure either way.
    expect_true(.rfc3986_valid_authority(paste0(lead, "@h")), info = info)
    expect_false(.rfc3986_valid_authority(paste0(lead, "@@h")), info = info)
    expect_true(.email_has_non_ascii(lead), info = info)
  }
})

test_that("the raw-@ count is a byte count, whatever surrounds it", {
  expect_true(.rfc3986_valid_authority("h"))
  expect_true(.rfc3986_valid_authority("u@h"))
  expect_false(.rfc3986_valid_authority("u@@h"))
  expect_false(.rfc3986_valid_authority("u@v@h"))
  expect_true(.rfc3986_valid_authority(enc2utf8("é@h")))
  expect_false(.rfc3986_valid_authority(enc2utf8("é@é@h")))
})

# --- case-insensitive keywords spelled as ASCII classes ----------------------

test_that("the keyword classes match exactly what ICU's caseless match did", {
  # The `(?i)` patterns became ASCII letter classes (ADR 0019, decision 2).
  # ICU's caseless match also folds U+017F to "s", U+212A to "k" and the
  # ligatures to their letter pairs, but none of those letters, nor any pair,
  # occurs in these keywords, so no row moved. These pins hold the old and the
  # new pattern to one answer on each fold candidate and on the case variants.
  pairs <- list(
    list(old = "(?i)%(2f|3f|23)", new = "%(?:2[Ff]|3[Ff]|23)"),
    list(
      old = "(?i)(\\A|/)(%2e|\\.%2e|%2e\\.|%2e%2e)(/|\\z)",
      new = "(?:\\A|/)(?:%2[Ee]|\\.%2[Ee]|%2[Ee]\\.|%2[Ee]%2[Ee])(?:/|\\z)"
    ),
    list(old = "(?i);phone-context=",
         new = ";[Pp][Hh][Oo][Nn][Ee]-[Cc][Oo][Nn][Tt][Ee][Xx][Tt]="),
    list(old = "(?i)(\\A|/)(\\.|%2e){1,2}(/|\\z)", new = .WEB_DOT_SEGMENT_RE)
  )
  probes <- enc2utf8(c(
    "%2f", "%2F", "%3f", "%3F", "%23", "%2ſ", "%2K", "%2ﬀ",
    "/%2e/", "/%2E/", "/.%2E/", "/%2e%2E", "%2E.", "/%2е/", "/../",
    "/./", "/.ſ/", ";phone-context=", ";PHONE-CONTEXT=",
    ";Phone-Context=", ";phone-conteſt=", ";phone-Kontext=",
    ";phone-conﬅext=", ";phone-context", "ﬀ", "ſ", "K",
    ";рhone-context="
  ))
  for (p in pairs) {
    expect_identical(
      .bytes_detect(probes, p$new),
      stringi::stri_detect_regex(probes, p$old),
      info = p$new
    )
  }
})

test_that("tel phone-context and the path facts read either case", {
  d <- get_url_diagnostics(
    c("tel:555;phone-context=a", "tel:555;PHONE-CONTEXT=a", "tel:555-1234"),
    url_standard = "whatwg", scheme_acceptance = "general"
  )
  expect_false("tel-missing-phone-context" %in% d[[1L]])
  expect_false("tel-missing-phone-context" %in% d[[2L]])
  expect_true("tel-missing-phone-context" %in% d[[3L]])
  p <- get_url_diagnostics(
    c("http://a/b%2Fc", "http://a/b%2fc", "http://a/%2E/b", "http://a/b"),
    url_standard = "rfc3986"
  )
  expect_true("encoded-reserved-path-byte" %in% p[[1L]])
  expect_true("encoded-reserved-path-byte" %in% p[[2L]])
  expect_true("encoded-dot-segment" %in% p[[3L]])
  expect_false(any(c("encoded-dot-segment", "encoded-reserved-path-byte") %in%
                     p[[4L]]))
})

# --- invalid UTF-8 gets its own answer, and the vector survives ---------------

test_that("an invalid row does not cost the vector its answers", {
  x <- c("http://a/%zz", "http://a/\x80", "http://a/ok")
  Encoding(x) <- "UTF-8"
  d <- get_url_diagnostics(x, url_standard = "whatwg")
  expect_length(d, 3L)
  expect_true("invalid-URL-unit" %in% d[[1L]])
  expect_false("invalid-URL-unit" %in% d[[3L]])
  bad <- "a\x80"
  Encoding(bad) <- "UTF-8"
  expect_identical(.email_has_non_ascii(c(bad, "a", NA)), c(TRUE, FALSE, NA))
})

# --- the end anchor is the true end (behavior changes, NEWS.md) --------------

test_that("a trailing line terminator is not the end of a host:port", {
  # ICU's `$` matched before a final U+0085 / U+2028 / U+2029, so these rows
  # were read as a bare `host:port` and then failed. A port is digits only
  # (WHATWG port state; RFC 3986 sec 3.2.3), so they parse as the non-digit
  # sibling `example.com:8080x` does.
  x <- enc2utf8(c(
    "example.com:8080 ", "example.com:8080\u0085",
    "example.com:8080 ", "tel:123 ", "example.com:8080x"
  ))
  r <- safe_parse_urls(x, url_standard = "whatwg",
                       scheme_acceptance = "general")
  expect_identical(r$parse_status, rep("ok", 5L))
  expect_identical(
    r$scheme, c("example.com", "example.com", "example.com", "tel",
                "example.com")
  )
  expect_identical(
    r$path, c("8080%E2%80%A8", "8080%C2%85", "8080%E2%80%A9", "123%E2%80%A8",
              "8080x")
  )
  # The terminator-free row keeps the carve-out.
  plain <- safe_parse_urls("example.com:8080", url_standard = "whatwg",
                           scheme_acceptance = "general")
  expect_identical(plain$host, "example.com")
  expect_identical(plain$port, 8080L)
})

test_that("a segment ending in a line terminator is not an encoded dot", {
  # WHATWG "single-dot URL path segment": the whole segment is "." or "%2e".
  d <- get_url_diagnostics(
    enc2utf8(c("http://a/%2e ", "http://a/%2e\u0085", "http://a/%2e",
               "http://a/%2e/b")),
    url_standard = "whatwg"
  )
  expect_false("encoded-dot-segment" %in% d[[1L]])
  expect_false("encoded-dot-segment" %in% d[[2L]])
  expect_true("encoded-dot-segment" %in% d[[3L]])
  expect_true("encoded-dot-segment" %in% d[[4L]])
})

test_that("the source lexer strips only C0 controls and spaces at the ends", {
  # Byte-mode `\z`: the spaces before a final U+2028 / U+0085 are data.
  expect_identical(
    serialize_url(enc2utf8(c("http://h/p#f  ", "http://h/p#f \u0085",
                             " \u0001http://u@h/ \t")), "whatwg"),
    c("http://h/p#f%20%E2%80%A8", "http://h/p#f%20%C2%85", "http://u@h/")
  )
})

test_that("latin1-marked input still reads at the internal byte checks", {
  # .prepare_url_input() converts latin1 at every export, but these helpers
  # document latin1 as legal input, and the byte layer refuses that mark.
  x <- "http://caf\xe9@h/p"
  Encoding(x) <- "latin1"
  # The walk reads the latin1 text; the strict test then rejects its
  # non-ASCII character (RUL-030; RFC 3986 S2, S2.5).
  expect_true(.rfc3986_generic_uri_ok_walk(.latin1_input_to_utf8(x)))
  expect_false(.rfc3986_generic_uri_ok(x)$ok)
  expect_identical(.rfc3986_generic_uri_ok(x)$diagnostic,
                   "unicode-outside-rfc3986-uri")
  y <- "caf\xe9"
  Encoding(y) <- "latin1"
  expect_identical(.email_has_non_ascii(c(y, "cafe", NA)), c(TRUE, FALSE, NA))
})
