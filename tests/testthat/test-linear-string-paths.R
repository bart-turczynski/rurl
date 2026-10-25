# String paths outside the R/parse-phases.R ICU emulations stay linear in the
# row and under PCRE's match limit (RURL-rweowcfz, after RURL-atlidhyh). Each
# block pins the helper's reading on short rows first, then a long row.

# PCRE2's default match limit is 1e7; a row a little past it. The long rows
# run through the helper that reads them, not a full parse, so the file stays
# fast.
long_n <- 10500000L

test_that("the general route's host:port carve-out reads digits to / or end", {
  x <- c("example.com:8080", "example.com:8080/p", "h:1x", "urn:a:1",
         "h:80?q", "h:", "h:8080 ", "sc:x:80", "h:80/", NA)
  want <- c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  expect_identical(.general_parsed_mask(x, "whatwg", "general"), want)
  expect_identical(.general_parsed_mask(x, NULL, "general"), want)
})

test_that("the general route's host:port test stays under the match limit", {
  # A ~10 MB row, copied several times: not on CRAN.
  skip_on_cran()
  # `[0-9]+(?:\z|/)` gave a digit run back byte by byte before failing: with
  # ~1e7 digits and no "/" or end after them it warned.
  x <- paste0("h:", strrep("1", long_n), "x")
  expect_no_warning(out <- .general_parsed_mask(x, "whatwg", "general"))
  expect_true(out)
})

test_that(".last_byte_index() finds the last single-byte literal", {
  expect_identical(.last_byte_index("a@b@c", "@"), 4L)
  expect_identical(.last_byte_index("@", "@"), 1L)
  expect_identical(.last_byte_index("abc", ":"), 0L)
  expect_identical(.last_byte_index("", ":"), 0L)
  # Positions are bytes, and an undecodable row is read as its octets.
  expect_identical(.last_byte_index("é@é", "@"), 3L)
  expect_identical(.last_byte_index("\xff@\xfe", "@"), 2L)
  expect_identical(.last_byte_index("﻿:", ":"), 4L)
})

test_that(".last_byte_index() is linear in the number of matches", {
  # A fixed `gregexpr()` listed every match, quadratic in their number:
  # 8e5 "@" took about 7 s, so 1e6 takes over 10 s. A linear scan takes
  # well under 1 s, so the bound sits far from both.
  skip_on_cran()
  skip_on_covr()
  s <- strrep("@", 1000000L)
  elapsed <- system.time(out <- .last_byte_index(s, "@"))[["elapsed"]]
  expect_identical(out, 1000000L)
  expect_lt(elapsed, 3)
})

locate_last <- function(x, pattern) {
  .bytes_locate(x, pattern, fixed = TRUE, last = TRUE)
}

test_that(".bytes_locate(fixed, last) reports the last non-overlapping match", {
  m <- function(...) {
    matrix(c(...), ncol = 2L, byrow = TRUE,
           dimnames = list(NULL, c("start", "end")))
  }
  # Matches do not overlap, scanning left to right: "aa" in "aaaaa" is 1-2,
  # 3-4, so the last is 3-4, not 4-5.
  expect_identical(
    locate_last(c("aaaaa", "xyz", NA, ""), "aa"),
    m(3L, 4L, NA, NA, NA, NA, NA, NA)
  )
  # A multibyte needle, in bytes.
  expect_identical(
    locate_last(c("aééb", "é"), "é"),
    m(4L, 5L, 1L, 2L)
  )
  # Regex metacharacters are literals.
  expect_identical(locate_last(c("a.b.c", "abc"), "."), m(4L, 4L, NA, NA))
  expect_identical(locate_last("a\\Eb\\Ec", "\\E"), m(5L, 6L))
  expect_identical(locate_last(c("a+b+", "a+"), "+"), m(4L, 4L, 2L, 2L))
  expect_identical(locate_last("a(b)(", "("), m(5L, 5L))
  expect_identical(locate_last("a\\Qb\\Q", "\\Q"), m(5L, 6L))
  # An undecodable row is read as its octets.
  expect_identical(locate_last(c("\xff@\xfe@x", "@"), "@"), m(4L, 4L, 1L, 1L))
  expect_identical(locate_last(character(), "@"), m(integer(), integer()))
})

test_that(".bytes_locate(fixed, last) is linear in the number of matches", {
  # The fixed `gregexpr()` is quadratic in the match count, as above.
  skip_on_cran()
  skip_on_covr()
  s <- strrep("@", 1000000L)
  elapsed <- system.time(out <- locate_last(s, "@"))[["elapsed"]]
  expect_identical(unname(out[1L, ]), c(1000000L, 1000000L))
  expect_lt(elapsed, 3)
})

test_that("the WHATWG IPv6 serializer folds a quad after the last \":\"", {
  h <- c("[::ffff:1.2.3.4]", "[::1.2.3.4]", "[1:2:3:4:5:6:1.2.3.4]",
         "[::1.2.3.256]", "[::01.2.3.4]", "[::1]", "[1.2.3.4]",
         "[::1.2.3.4]\n", "[:::]", "[a:b:c:d:e:f:255.255.255.255]",
         "[::1.2.3.4:]", "[::1.2.3]")
  expect_identical(
    vapply(h, .serialize_whatwg_ipv6_host, "", USE.NAMES = FALSE),
    c("[::ffff:102:304]", "[::102:304]", "[1:2:3:4:5:6:102:304]",
      "[::1.2.3.256]", "[::01.2.3.4]", "[::1]", "[1.2.3.4]",
      "[::1.2.3.4]\n", "[:::]", "[a:b:c:d:e:f:ffff:ffff]",
      "[::1.2.3.4:]", "[::1.2.3]")
  )
})

test_that("the IPv6 dotted-quad capture stays under the match limit", {
  # A ~10 MB row, copied several times: not on CRAN.
  skip_on_cran()
  # `(?:[^:]*+:)++` counted one step per ":" group: a host of ~1e7 colons
  # warned (the host still came back unchanged).
  h <- paste0("[", strrep(":", long_n), "]")
  expect_no_warning(out <- .serialize_whatwg_ipv6_host(h))
  expect_true(identical(out, h))
  h <- paste0("[", strrep(":", long_n), "1.2.3.4]")
  expect_no_warning(out <- .serialize_whatwg_ipv6_host(h))
  expect_true(identical(out, h))
})
