# RURL-rynmvtwp: .fsss_source_lex() and a source that starts with U+FEFF.
#
# The serializer-input record (R/serialize.R) recovers the userinfo and host
# slices lexically, off the source, with a start-anchored `scheme://` match.
# Some stringi functions read past a U+FEFF that starts their input (see
# .starts_with_bom()), so that match lexed an authority out of a source whose
# first code point is the mark, although no scheme can start there: a scheme
# starts with an ASCII letter (RFC 3986 section 3.1; WHATWG URL Standard,
# scheme start state), and neither standard strips the mark. Such a row fails
# the parse, so the record never used the slices and no output moved: the
# defect is latent. Each U+FEFF row has a U+200B twin, a zero-width code point
# stringi does not skip, so a twin that already lexes no authority shows the
# defect is the mark's alone.

bom <- "﻿"
zwsp <- "​"
lex_arms <- c("whatwg", "rfc3986")

test_that("the U+200B twins lex no authority under either standard", {
  for (arm in lex_arms) {
    lex <- rurl:::.fsss_source_lex(
      c(paste0(zwsp, "http://u@h:1/p?q#f"), paste0(zwsp, "http://h/")), arm
    )
    expect_identical(lex$userinfo, c(NA_character_, NA_character_),
                     label = arm)
    expect_identical(lex$host, c(NA_character_, NA_character_), label = arm)
    expect_identical(lex$query_kind, c("present", "absent"), label = arm)
    expect_identical(lex$fragment_kind, c("present", "absent"), label = arm)
  }
})

test_that("negative controls: a source with no mark, or a mark not first", {
  for (arm in lex_arms) {
    lex <- rurl:::.fsss_source_lex(
      c(
        "http://u@h:1/p?q#f", "http://h/", "http://[::1]:8/",
        paste0("http://", bom, "u@h/"), paste0("http://", bom, "h/")
      ),
      arm
    )
    expect_identical(
      lex$userinfo, c("u", NA, NA, paste0(bom, "u"), NA), label = arm
    )
    expect_identical(
      lex$host, c("h", "h", "[::1]", "h", paste0(bom, "h")), label = arm
    )
    expect_identical(
      lex$query_kind, c("present", rep("absent", 4L)), label = arm
    )
    expect_identical(
      lex$fragment_kind, c("present", rep("absent", 4L)), label = arm
    )
  }
})

test_that("a mark-led source fails the parse, so the record never reads it", {
  # The latency claim: both standards reject the row, so serialize_url()
  # emits NA whatever the lexer recovers for it.
  for (arm in lex_arms) {
    expect_identical(
      serialize_url(paste0(bom, "http://u@h:1/p?q#f"), standard = arm),
      NA_character_, label = arm
    )
  }
})

test_that("a mark-led source lexes no authority", {
  for (arm in lex_arms) {
    lex <- rurl:::.fsss_source_lex(
      c(paste0(bom, "http://u@h:1/p?q#f"), paste0(bom, "http://h/")), arm
    )
    # The start-anchored stringi match read past the mark and lexed `u` and
    # `h` out of a source that has no scheme (RURL-rynmvtwp).
    expect_identical(lex$userinfo, c(NA_character_, NA_character_),
                     label = arm)
    expect_identical(lex$host, c(NA_character_, NA_character_), label = arm)
    # The delimiter facts are cut as bytes and do not depend on the mark.
    expect_identical(lex$query_kind, c("present", "absent"), label = arm)
    expect_identical(lex$fragment_kind, c("present", "absent"), label = arm)
  }
})

test_that("whatwg: the mark that leads after the C0-or-space strip counts", {
  # Under whatwg the lexer applies the basic URL parser's step 1 first, which
  # removes the leading space and leaves the mark in front. RFC 3986 strips
  # nothing, so there the space still leads and no scheme matches.
  src <- paste0(" ", bom, "http://u@h/")
  lex <- rurl:::.fsss_source_lex(src, "whatwg")
  # The guard reads the mark after the strip, not before (RURL-rynmvtwp).
  expect_identical(lex$userinfo, NA_character_)
  expect_identical(lex$host, NA_character_)
  lex <- rurl:::.fsss_source_lex(src, "rfc3986")
  expect_identical(lex$userinfo, NA_character_)
  expect_identical(lex$host, NA_character_)
})
