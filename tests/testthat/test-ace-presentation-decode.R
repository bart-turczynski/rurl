# Unicode presentation of an A-label whose basic string holds a non-LDH code
# point (RURL-oizpyvdz). RFC 3492 section 6.2 accepts any basic code point
# before the last delimiter, and UTS #46 section 4 (ToUnicode, with
# UseSTD3ASCIIRules false) decodes with it, so "xn--a_-wia" is "a_" + U+00E4.
# punycoder 1.2.1 decoded it; 1.3.0 requires letter-digit-hyphen basic code
# points, so the ADR 0002 decode helper fell back to the A-label under every
# arm. The helper now decodes such a label with rurl's own RFC 3492 decoder,
# and these expectations hold under either punycoder version.

test_that("a non-LDH A-label renders in Unicode under every arm", {
  url <- "http://xn--a_-wia.example/"
  expected <- charToRaw("a_ä.example")
  # The NULL arm: the pre-fix output under punycoder 1.2.1, which 1.3.0 moved.
  # Restoring it keeps NULL at its frozen bytes (ADR 0007, ADR 0016); omitting
  # the selector and passing NULL must agree.
  expect_identical(
    charToRaw(get_host(url, host_encoding = "unicode")), expected
  )
  expect_identical(
    charToRaw(get_host(url, host_encoding = "unicode", url_standard = NULL)),
    expected
  )
  # whatwg: ToUnicode(ToASCII(host)) (RUL-002). rfc3986: the reversible
  # rendering. Both render the label; neither was correct under 1.3.0.
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(
      charToRaw(get_host(url, host_encoding = "unicode", url_standard = s)),
      expected,
      info = s
    )
  }
})

test_that("the fallback decodes exactly what punycoder 1.2.1 decoded", {
  # The signature the fix stays inside (ADR 0016): an `xn--` label (any ASCII
  # case) that punycoder rejects is decoded by `.rfc3492_decode()`, keeping
  # the case of its basic string, as 1.2.1 did.
  decoded <- rurl:::.punycode_to_unicode_vec(c(
    "xn--a_-wia.example", "XN--A_-wia.example", "xn--a~b-wia.example",
    "xn--a$-wia.example", "xn--a_-.example", "xn--mnchen-3ya.xn--a!-wia"
  ))
  expect_identical(
    lapply(decoded, charToRaw),
    lapply(c(
      "a_ä.example", "A_ä.example", "a~Ëb.example", "a$ä.example",
      "a_.example", "münchen.a!ä"
    ), charToRaw)
  )
  # A host rendering must not produce a URL delimiter (# / : ? @), so those
  # labels stay as written, as 1.2.1 left them. So do labels RFC 3492
  # rejects, and an empty payload.
  kept <- c(
    "xn--a#-wia", "xn--a/-wia", "xn--a:-wia", "xn--a?-wia", "xn--a@-wia",
    "xn---", "xn--", "xn--a_", "xn--a_-w!a", "a_b"
  )
  expect_identical(
    rurl:::.punycode_to_unicode_vec(paste0(kept, ".example")),
    paste0(kept, ".example")
  )
})

test_that("a percent-encoded delimiter never decodes into the rfc3986 host", {
  # The rfc3986 arm percent-decodes the host before rendering it, so
  # "xn--a%23-wia" reaches the helper as "xn--a#-wia"; the delimiter
  # exclusion keeps it an A-label. The colon row is the one place the fix
  # departs from punycoder 1.2.1, which decoded a `:` after a prefix that is
  # not scheme-shaped and rendered ";~" + U+4E2D + ":_" + U+00F6, a colon
  # inside a reg-name (RFC 3986 section 3.2.2).
  urls <- c(
    "http://xn--a%23-wia.example/", "http://xn--;~%3A_-8qa3179i.example/"
  )
  expect_identical(
    get_host(urls, host_encoding = "unicode", url_standard = "rfc3986"),
    c("xn--a#-wia.example", "xn--;~:_-8qa3179i.example")
  )
})

test_that("a label that is not valid UTF-8 neither warns nor throws", {
  # The helper is total. Such a label fails every decode and keeps its
  # spelling, which the UTF-8 sanitizer then scrubs, as before the fix.
  x <- c("xn--a\xff_-wia.example", "xn--\xfe:.example")
  Encoding(x) <- "UTF-8"
  expect_silent(out <- rurl:::.punycode_to_unicode_vec(x))
  expect_identical(out, c("xn--a_-wia.example", "xn--:.example"))
})
