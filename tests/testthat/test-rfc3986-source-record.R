# RURL-gkmwqpos, ruling RUL-007: under `url_standard = "rfc3986"` the parse
# record stores the query and fragment as the source spelled them, and carries
# the scheme's source spelling beside the folded classification token. The
# serializer-level property lives in test-rfc3986-serialization-properties.R
# and test-serialize-fsss.R; this file pins the CONSUMER surfaces that move
# with the record, and the two arms that must not move at all.
#
# The frame-vs-payload rule (design/posture-card.md): a behavior claimed
# invariant across arms is tested by looping the selector, not by varying the
# input under one. Every grid row below is parsed under all three arms.

rul007_grid <- c(
  # mixed-case triplets in all three components, special and non-special
  "http://h/p%7ca?q=%7ca#f%7ca",
  "https://h/p%7Ca?q=%7Ca#f%7Ca",
  "ftp://h/p%2f?q=%2f&r=%2F#f%2f",
  "foo://h/p%7ca?q=%7ca#f%7ca",
  # a pre-existing "%" beside a fresh non-ASCII byte
  "http://h/p?q=%7cü#f%7cü",
  "http://h/p?q=ü#fü",
  # scheme spellings
  "HTTP://EXAMPLE.COM/", "Http://h/p?Q=%7ca", "FOO://x/?q=%7ca",
  # empty delimiters
  "http://h/p?#", "http://h/p?", "http://h/p#",
  # the raw-query consumers
  "http://h/p?a=1&utm_source=%7cx&b=%7C",
  "http://h/p?a=%7ca&A=%7CA"
)

rul007_parse <- function(url_standard) {
  if (is.null(url_standard)) {
    safe_parse_urls(rul007_grid, url_standard = NULL,
                    scheme_policy = "infer", scheme_acceptance = "web")
  } else {
    safe_parse_urls(rul007_grid, url_standard = url_standard,
                    scheme_policy = "require", scheme_acceptance = "general")
  }
}

test_that("rfc3986 query/fragment columns carry the source hex case", {
  p <- rul007_parse("rfc3986")
  expect_identical(p$query[1:4], c("q=%7ca", "q=%7Ca", "q=%2f&r=%2F", "q=%7ca"))
  expect_identical(p$fragment[1:4], c("f%7ca", "f%7Ca", "f%2f", "f%7ca"))
  # Special-scheme rows now agree with the general-routed `foo://` row, which
  # never folded (rows 1 and 4 differ only in scheme).
  expect_identical(p$query[1L], p$query[4L])
  expect_identical(p$fragment[1L], p$fragment[4L])
  # A pre-existing triplet keeps its case beside a raw non-ASCII byte, which
  # the record now stores as written too (RUL-015; RFC 3986 sec 2.1 makes the
  # triplet a representation of the octet, and the path already kept it raw).
  # `form = "normalized"` is where it is percent-encoded.
  expect_identical(p$query[5L], "q=%7cü")
  expect_identical(p$fragment[5L], "f%7cü")
  expect_identical(p$query[6L], "q=ü")
  # The scheme column is the folded classification token, on every route.
  expect_identical(p$scheme[7:9], c("http", "http", "foo"))
  # Empty delimiters are absent components on the public columns, as before.
  expect_identical(p$query[10:12], rep(NA_character_, 3L))
  expect_identical(p$fragment[10:12], rep(NA_character_, 3L))
})

test_that("rfc3986 accessors read the source hex case off the record", {
  rfc <- list(url_standard = "rfc3986", scheme_policy = "require",
              scheme_acceptance = "general")
  q <- do.call(get_query, c(list(rul007_grid[c(1L, 13L, 14L)], decode = FALSE),
                            rfc))
  expect_identical(
    q, c("q=%7ca", "a=1&utm_source=%7cx&b=%7C", "a=%7ca&A=%7CA")
  )
  # Decoding is case-insensitive over the triplet, so the decoded view is the
  # same string it always was.
  qd <- do.call(get_query, c(list(rul007_grid[1:2], decode = TRUE), rfc))
  expect_identical(qd, c("q=|a", "q=|a"))
  f <- do.call(get_fragment, c(list(rul007_grid[1:3]), rfc))
  expect_identical(f, c("f%7ca", "f%7Ca", "f%2f"))
  # The cleaning surface is a lossy policy projection, never a conformance
  # oracle (ADR 0017): its query canonicalizes hex case on the way out, so a
  # retained query and the tracking-parameter filter both read the same as
  # before the record moved -- and the filter still matches by name.
  keep <- do.call(get_clean_url, c(
    list(rul007_grid[13L], query_handling = "keep"), rfc
  ))
  expect_identical(keep, "http://h/p?a=1&utm_source=%7Cx&b=%7C")
  filt <- do.call(get_clean_url, c(
    list(rul007_grid[13L], query_handling = "filter"), rfc
  ))
  expect_identical(filt, "http://h/p?a=1&b=%7C")
})

test_that("the rfc3986 URL key carries the exact structural query", {
  # key-join-contracts.md: the default key is the exact structural query, and
  # the path already preserved its hex case, so `%7ca` and `%7Ca` are distinct
  # keys in both positions -- while scheme identity stays case-insensitive.
  pol <- url_key_policy(standard = "rfc3986")
  k <- get_url_key(c("http://h/p?q=%7ca", "http://h/p?q=%7Ca",
                     "http://h/p%7ca", "http://h/p%7Ca",
                     "HTTP://h/p?q=%7ca"), pol)
  expect_false(identical(k[[1L]], k[[2L]]))
  expect_false(identical(k[[3L]], k[[4L]]))
  expect_identical(k[[1L]], k[[5L]])
})

test_that("the whatwg and NULL arms do not move (ADR 0007, ADR 0016)", {
  # Byte-pinned against the pre-RUL-007 output, which these two arms never
  # deviated from: the component pass's normalized spelling is still what
  # they store. `url_standard = NULL` is the frozen profile; if this test
  # fails there, the fix has leaked past the `rfc3986` selector.
  w <- rul007_parse("whatwg")
  expect_identical(
    w$query[1:6],
    c("q=%7Ca", "q=%7Ca", "q=%2F&r=%2F", "q=%7ca", "q=%7C%C3%BC", "q=%C3%BC")
  )
  expect_identical(
    w$fragment[1:6],
    c("f%7Ca", "f%7Ca", "f%2F", "f%7ca", "f%7C%C3%BC", "f%C3%BC")
  )
  expect_identical(w$scheme[7:9], c("http", "http", "foo"))
  expect_identical(w$query[13:14],
                   c("a=1&utm_source=%7Cx&b=%7C", "a=%7Ca&A=%7CA"))
  # (The WHATWG path keeps its spelling -- the re-derived path always did --
  # while the query and fragment carry the component pass's uppercase.)
  expect_identical(
    serialize_url(rul007_grid[c(1L, 7L, 13L)], standard = "whatwg"),
    c("http://h/p%7ca?q=%7Ca#f%7Ca", "http://example.com/",
      "http://h/p?a=1&utm_source=%7Cx&b=%7C")
  )

  n <- rul007_parse(NULL)
  expect_identical(n$query[1:3], c("q=%7Ca", "q=%7Ca", "q=%2F&r=%2F"))
  expect_identical(n$fragment[1:3], c("f%7Ca", "f%7Ca", "f%2F"))
  expect_identical(n$query[5:6], c("q=%7C%C3%BC", "q=%C3%BC"))
  expect_identical(n$scheme[7:8], c("http", "http"))
  expect_identical(n$query[13:14],
                   c("a=1&utm_source=%7Cx&b=%7C", "a=%7Ca&A=%7CA"))
  expect_identical(
    get_query(rul007_grid[c(1L, 13L)], decode = FALSE),
    c("q=%7Ca", "a=1&utm_source=%7Cx&b=%7C")
  )
})

test_that("the parser dial is the rfc3986 selector and nothing else", {
  expect_identical(rurl:::.web_pqf_source_policy("rfc3986"), "preserve")
  expect_identical(rurl:::.web_pqf_source_policy("whatwg"), "normalize")
  expect_identical(rurl:::.web_pqf_source_policy(NULL), "normalize")
  # Acceptance is unchanged by the dial: a forbidden byte still rejects under
  # `pqf_bytes = "reject"`, whichever spelling would have been stored.
  expect_null(rurl:::.parse_web_url_one(
    "http://h/p?q= x", pqf_bytes = "reject", pqf_source = "preserve"
  ))
  one <- rurl:::.parse_web_url_one(
    "http://h/p?q=%7ca#f%7ca", pqf_source = "preserve"
  )
  expect_identical(one$query, "q=%7ca")
  expect_identical(one$fragment, "f%7ca")
  two <- rurl:::.parse_web_url_one("http://h/p?q=%7ca#f%7ca")
  expect_identical(two$query, "q=%7Ca")
  expect_identical(two$fragment, "f%7Ca")
})
