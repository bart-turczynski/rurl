# RURL-dddxutjf, the ADR 0019 close-out: the last calls outside the kept set
# moved to base R, or into the one shared semantic exception 2(d) decode.
# Each block pins the reading the stringi call gave.

raw_utf8 <- function(...) {
  x <- rawToChar(as.raw(c(...)))
  Encoding(x) <- "UTF-8"
  x
}
hex <- function(x) paste(charToRaw(x), collapse = " ")

test_that("the whatwg trim decodes an undecodable row, U+FFFD per subpart", {
  fffd <- "ef bf bd"
  cases <- list(
    list(raw_utf8(0x68, 0xff, 0x61), paste("68", fffd, "61")),
    list(raw_utf8(0x68, 0xe2, 0x82, 0x61), paste("68", fffd, "61")),
    list(raw_utf8(0x68, 0xc0, 0xaf, 0x61), paste("68", fffd, fffd, "61")),
    list(raw_utf8(0x68, 0xed, 0xa0, 0x80), paste("68", fffd, fffd, fffd)),
    list(raw_utf8(0x68, 0xf0, 0x9f, 0x98), paste("68", fffd)),
    # The trim runs after the decode, and a U+FEFF after the run is kept.
    list(raw_utf8(0x20, 0xef, 0xbb, 0xbf, 0xff, 0x20),
         paste("ef bb bf", fffd))
  )
  for (cs in cases) {
    out <- .strip_whatwg_control_chars_vec(cs[[1]], "whatwg")$url
    expect_identical(hex(out), cs[[2]], info = cs[[2]])
    expect_true(validUTF8(out))
  }
  # A row led by U+FEFF keeps the mark through the decode.
  led <- raw_utf8(0xef, 0xbb, 0xbf, 0x61, 0xff)
  expect_identical(
    hex(.utf8_decode_replacing(led)), paste("ef bb bf 61", "ef bf bd")
  )
  # rfc3986 and NULL leave the row as written.
  bad <- raw_utf8(0x20, 0x68, 0xff)
  expect_identical(.strip_whatwg_control_chars_vec(bad, "rfc3986")$url, bad)
  expect_identical(.strip_whatwg_control_chars_vec(bad, NULL)$url, bad)
})

test_that("the host label split keeps the trailing empty label and a U+FEFF", {
  labels <- .split_host_labels(
    c("example.com.", "a..b", "﻿xn--nxasmq6b.com", "", NA)
  )
  expect_identical(labels[[1]], c("example", "com", ""))
  expect_identical(labels[[2]], c("a", "", "b"))
  expect_identical(labels[[3]], c("﻿xn--nxasmq6b", "com"))
  expect_identical(labels[[4]], "")
  expect_identical(labels[[5]], NA_character_)
  expect_identical(Encoding(labels[[3]][1]), "UTF-8")
})

test_that("the key frame counts the bytes of a field, a U+FEFF included", {
  expect_identical(
    .url_key_frame(list(c("﻿h.example.com", "h.example.com", NA))),
    c("16:﻿h.example.com", "13:h.example.com", "-:")
  )
})
