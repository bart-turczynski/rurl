# Inputs longer than 1e6 characters keep every character (RURL-hxeaefzp).
#
# Before R 4.6.0, base `substring(x, first)` defaulted to `last = 1000000L`,
# so a two-argument suffix cut silently dropped everything past character
# 1e6; DESCRIPTION admits R 4.1.0. R 4.6.0 made the default "to the end of the
# string", so on a current R these tests pass whether or not a call passes
# `last`: they document the contract, and tools/substring-last-gate.R is the
# pin. Each input is just over 1e6 characters and ends in a marker the
# truncation would have cut off. With the old default emulated inside the
# namespace, every test below but the first failed on the code before
# RURL-hxeaefzp; the first is the ticket's own example, whose cuts in
# R/path-query.R already passed `last`.
#
# The inputs cost seconds and hundreds of MB, so CRAN skips these tests on
# R 4.6.0 and later, where they cannot fail; its older-R flavors, where the
# defect lives, still run them.
#
# The RFC 3986 surface is not covered here: its uniform gate's regex
# overflows ICU's backtrack stack (U_REGEX_STACK_OVERFLOW) on inputs of about
# 6e5 characters, before any of these cuts runs. That is RURL-vewhvxnb.

skip_long_on_cran <- function() {
  if (getRversion() >= "4.6.0") {
    skip_on_cran()
  }
}

long <- strrep("a", 1000000L)
tail_z <- paste0(long, "z")

test_that("a WHATWG path longer than 1e6 characters keeps its tail", {
  skip_long_on_cran()
  res <- safe_parse_url(paste0("http://h.example/./", tail_z),
                        url_standard = "whatwg")
  expect_identical(res$path, paste0("/", tail_z))
})

test_that("a general-scheme path, query and fragment keep their tails", {
  skip_long_on_cran()
  # One long component is enough: the query and fragment are cut from a
  # string over 1e6 characters long, at a position past 1e6.
  res <- safe_parse_url(paste0("foo://h.example/", tail_z, "?q#f"),
                        url_standard = "whatwg", scheme_acceptance = "general")
  expect_identical(res$path, paste0("/", tail_z))
  expect_identical(res$query, "q")
  expect_identical(res$fragment, "f")
})

test_that("an opaque path longer than 1e6 characters keeps its tail", {
  skip_long_on_cran()
  res <- safe_parse_url(paste0("foo:", tail_z, "?q#f"),
                        url_standard = "whatwg", scheme_acceptance = "general")
  expect_identical(res$path, tail_z)
  expect_identical(res$query, "q")
  expect_identical(res$fragment, "f")
})

test_that("a general-scheme password and host keep their tails", {
  skip_long_on_cran()
  # The host is cut from an authority over 1e6 characters long.
  res <- safe_parse_url(paste0("foo://u:", tail_z, "@h.example/x"),
                        url_standard = "whatwg", scheme_acceptance = "general")
  expect_identical(res$user, "u")
  expect_identical(res$password, tail_z)
  expect_identical(res$host, "h.example")
  expect_identical(res$path, "/x")
})

test_that("a WHATWG password keeps its tail through serialize_url()", {
  skip_long_on_cran()
  url <- paste0("http://u:", tail_z, "@h.example/")
  expect_identical(serialize_url(url, standard = "whatwg"), url)
})

test_that("resolve_url() keeps the tail of a long file path", {
  skip_long_on_cran()
  expect_identical(
    resolve_url(paste0("/", tail_z), "file:///C:/a/b", url_standard = "whatwg"),
    paste0("file:///C:/", tail_z)
  )
  expect_identical(
    resolve_url(sprintf("../%s", tail_z), "file:///a/b",
                url_standard = "whatwg"),
    paste0("file:///", tail_z)
  )
})

test_that("an email IPv6 address literal is judged on its whole body", {
  skip_long_on_cran()
  # Called directly: the public path, get_mailto_recipients(), lexes a
  # recipient one unit at a time and needs minutes on a 1e6-character input.
  # 500000 groups of "1:" put the bad final group past character 1e6.
  body <- paste0("IPv6:", strrep("1:", 500000L))
  expect_true(.email_valid_address_literal(paste0(body, "1")))
  expect_false(.email_valid_address_literal(paste0(body, "x")))
})

test_that("a long parse-cache key led by U+FEFF keeps its tail", {
  skip_long_on_cran()
  expect_identical(.cache_key_ascii(paste0("\ufeff", tail_z)),
                   paste0("\\zzefbbbf", strrep("61", 1000000L), "7a"))
})
