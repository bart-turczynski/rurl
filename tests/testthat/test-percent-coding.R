# In-tree percent-coding primitives (RURL-robgajml).
#
# `.pct_escape()` / `.pct_unescape()` replaced `curl::curl_escape()` /
# `curl::curl_unescape()`. Every expectation below is written as a LITERAL, not
# as a differential against curl: the point of the swap is that curl leaves the
# package entirely (RURL-cunfohwy forbids a curl reference in tests too), so the
# oracle has to be the standard and the recorded libcurl behavior, not a live
# call.
#
# Parity was established before the swap by a differential sweep of 4,608
# inputs -- every octet 0x01-0xFF standalone, every `%XX` triplet in both hex
# cases bare and embedded, the malformed-`%` family, multibyte UTF-8, and 4,000
# random mixes -- comparing VALUES, RAW BYTES and `Encoding()` marks, in both
# directions and round-tripped: 0 differences.
#
# All non-ASCII literals are \u escapes so the file parses identically in any
# locale (same reason as test-locale-invariance.R).

# --- .pct_escape -------------------------------------------------------------

test_that("the unreserved set passes through and everything else escapes", {
  # RFC 3986 section 2.3: ALPHA / DIGIT / "-" / "." / "_" / "~". This is the
  # whole contract; the two vectors below are the complete ASCII partition.
  unreserved <- paste0(
    paste(LETTERS, collapse = ""), paste(letters, collapse = ""),
    "0123456789", "-._~"
  )
  expect_identical(rurl:::.pct_escape(unreserved), unreserved)

  octets <- vapply(1:255, function(b) rawToChar(as.raw(b)), character(1))
  escaped <- rurl:::.pct_escape(octets)
  # `useBytes` in the TEST's own scan too: a lone high octet is not valid UTF-8
  # and a validity-checking scan would warn here, not in the code under test.
  keep <- grepl("^[A-Za-z0-9._~-]$", octets, useBytes = TRUE)
  expect_identical(escaped[keep], octets[keep])
  expect_identical(escaped[!keep], sprintf("%%%02X", (1:255)[!keep]))
})

test_that("escape hex is uppercase and reserved delimiters are not spared", {
  expect_identical(rurl:::.pct_escape("a b"), "a%20b")
  expect_identical(rurl:::.pct_escape("/a/b"), "%2Fa%2Fb")
  expect_identical(rurl:::.pct_escape("a?b=c&d#e"), "a%3Fb%3Dc%26d%23e")
  # A percent sign is itself escaped -- this is NOT the WHATWG component
  # serializer, which preserves existing spellings.
  expect_identical(rurl:::.pct_escape("%41"), "%2541")
})

test_that("escape operates on UTF-8 octets, not characters", {
  expect_identical(rurl:::.pct_escape("é"), "%C3%A9")
  expect_identical(rurl:::.pct_escape("€"), "%E2%82%AC")
  expect_identical(rurl:::.pct_escape("中文"), "%E4%B8%AD%E6%96%87")
})

test_that("escape is vectorized and NA/empty-safe", {
  expect_identical(rurl:::.pct_escape(character(0)), character(0))
  expect_identical(rurl:::.pct_escape(""), "")
  expect_identical(
    rurl:::.pct_escape(c("a b", NA, "plain", "é")),
    c("a%20b", NA, "plain", "%C3%A9")
  )
})

# --- .pct_unescape -----------------------------------------------------------

test_that("every triplet decodes, in either hex case", {
  # 0x00 is excluded: a decoded NUL terminates the value (see below).
  expect_identical(
    rurl:::.pct_unescape(sprintf("%%%02X", 1:255)),
    vapply(1:255, function(b) rawToChar(as.raw(b)), character(1))
  )
  expect_identical(rurl:::.pct_unescape("%c3%a9"), "é")
  expect_identical(rurl:::.pct_unescape("%C3%A9"), "é")
})

test_that("a malformed percent passes through byte-for-byte", {
  expect_identical(rurl:::.pct_unescape("%"), "%")
  expect_identical(rurl:::.pct_unescape("%2"), "%2")
  expect_identical(rurl:::.pct_unescape("a%zzb%2"), "a%zzb%2")
  # Scanning is left-to-right and non-overlapping, and the decoded output is
  # never re-scanned: "%2525" is "%" + "25", not "%".
  expect_identical(rurl:::.pct_unescape("%%41"), "%A")
  expect_identical(rurl:::.pct_unescape("%2525"), "%25")
  expect_identical(rurl:::.pct_unescape("%4%41"), "%4A")
})

test_that("a decoded NUL truncates the value", {
  # Inherited from libcurl handing back a NUL-terminated C string, and kept
  # deliberately: an R string cannot carry an embedded NUL either, so the
  # alternative would smuggle the tail past callers that never see it today.
  expect_identical(rurl:::.pct_unescape("a%00b"), "a")
  expect_identical(rurl:::.pct_unescape("%00"), "")
})

test_that("unescape is not form decoding", {
  # '+' is a literal here. Callers that want form semantics map it themselves
  # (see .decode_query_tokens()'s decode_plus).
  expect_identical(rurl:::.pct_unescape("a+b"), "a+b")
})

test_that("unescape is vectorized and NA/empty-safe", {
  expect_identical(rurl:::.pct_unescape(character(0)), character(0))
  expect_identical(rurl:::.pct_unescape(""), "")
  expect_identical(
    rurl:::.pct_unescape(c("a%20b", NA, "plain", "%C3%A9")),
    c("a b", NA, "plain", "é")
  )
})

test_that("a literal run between triplets is sliced by BYTE position", {
  # The guard against reintroducing character-offset slicing: multibyte UTF-8
  # sitting between two triplets makes character and byte offsets disagree.
  expect_identical(
    rurl:::.pct_unescape("%41中%42é%43"),
    "A中BéC"
  )
})

# --- encoding contract -------------------------------------------------------

test_that("decoded output is declared UTF-8 and never transcoded", {
  # Non-ASCII output carries the mark; R refuses to mark a pure-ASCII string,
  # so "unknown" is correct there and must not be "fixed" (same rule as
  # test-locale-invariance.R).
  expect_identical(Encoding(rurl:::.pct_unescape("%C3%A9")), "UTF-8")
  expect_identical(Encoding(rurl:::.pct_unescape("abc")), "unknown")
  expect_identical(Encoding(rurl:::.pct_unescape("é")), "UTF-8")
  # Invalid UTF-8 is passed through as raw octets without a warning or a
  # substitution -- percent-coding is byte transport, not validation.
  expect_silent(bad <- rurl:::.pct_unescape("%FF%FE"))
  expect_identical(charToRaw(bad), as.raw(c(0xFF, 0xFE)))
})

test_that("escaping declares rather than transcodes its input", {
  # `.pct_as_utf8()` uses `Encoding<-`, so the octets read are the octets
  # present regardless of `LC_CTYPE`. An "unknown"-marked UTF-8 string (what an
  # unmarked user input looks like) must escape to the same triplets as a
  # marked one -- this is the assertion that fails if `enc2utf8()` ever comes
  # back.
  marked <- "é"
  unmarked <- marked
  Encoding(unmarked) <- "unknown"
  expect_identical(rurl:::.pct_escape(unmarked), rurl:::.pct_escape(marked))
  expect_identical(rurl:::.pct_escape(unmarked), "%C3%A9")
})

# --- round trip --------------------------------------------------------------

test_that("escape then unescape is the identity on every octet", {
  octets <- vapply(1:255, function(b) rawToChar(as.raw(b)), character(1))
  round <- rurl:::.pct_unescape(rurl:::.pct_escape(octets))
  expect_identical(
    lapply(round, charToRaw), lapply(octets, charToRaw)
  )
})
