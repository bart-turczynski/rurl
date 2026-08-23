# RURL-ogruzocw. Stripping a terminal index/default page EXPOSES dot segments
# the page name was hiding, and nothing re-normalized afterwards, so
# `index_page_handling = "strip"` emitted paths ending in `.` or `..` --
# including `http://example.com/..`, which escapes above the document root.
#
# The pipeline order is not the bug: `path_normalization` (processing-order
# step 2) correctly precedes `index_page_handling` (step 3). The bug is that
# step 3 had an unstated precondition -- a resolved path -- that step 2 does
# not guarantee, because `path_normalization` defaults to "none".
#
# This fix MOVES `url_standard = NULL` rows, which ADR 0007 freezes, and ADR
# 0016 is what licenses it: the freeze binds selector-CAUSED drift, and this
# is a defect in the default path itself -- `http://example.com/..` escapes the
# document root under no standard, profile or documented contract. Per ADR 0016
# the fix carries a witness (the tests below run under the default selector, and
# the first pins that omitting the argument and passing NULL explicitly agree)
# and a signature (index_page_handling = "strip" on paths whose strip leaves a
# dot segment in FINAL position -- the "path_normalization = 'none'" test below
# bounds it from the other side).
#
# The named arms were ALREADY correct, by a different route: their
# path_normalization = "dot_segments" resolves at step 2, so the new branch is
# unreachable there ("selector arms are unreachable", below). One defect did not
# manifest identically across the three arms -- which is exactly why membership
# in `.URL_STANDARD_PROFILES` does NOT license this fix. That table is
# conflict-matrix ownership, not behavioral independence; ADR 0016 falsifies the
# reading this comment used to assert.

test_that("stripping an index page never exposes a trailing dot segment", {
  inputs <- c(
    "http://example.com/a/./b/../index.html",
    "http://example.com/a/../index.html",
    "http://example.com/./index.html",
    "http://example.com/a/b/../../index.html",
    "http://example.com/../index.html"
  )
  expected <- c(
    "http://example.com/a/",
    "http://example.com/",
    "http://example.com/",
    "http://example.com/",
    "http://example.com/"
  )
  expect_identical(
    safe_parse_urls(inputs, index_page_handling = "strip")$clean_url,
    expected
  )
  # ADR 0016's witness requirement: the omitted argument and an explicit NULL
  # are the same arm, so the defect this file fixes is demonstrably in the
  # frozen default path rather than in a selector arm.
  expect_identical(
    safe_parse_urls(inputs,
      url_standard = NULL, index_page_handling = "strip"
    )$clean_url,
    expected
  )
})

test_that("no stripped path escapes above the document root", {
  # The worst shape in the family: pre-fix this emitted "http://example.com/..".
  expect_identical(
    safe_parse_urls("http://example.com/../index.html",
      index_page_handling = "strip"
    )$clean_url,
    "http://example.com/"
  )
  expect_identical(
    get_path("http://example.com/a/b/../../../index.html",
      index_page_handling = "strip"
    ),
    "/"
  )
})

test_that("the fix is not index-specific: default.<ext> behaves identically", {
  # The strip regex covers `(index|default)` case-insensitively; a fix keyed on
  # the literal "index" would leave these three red.
  pairs <- c("index.html", "default.aspx", "Default.asp", "INDEX.htm",
             "index.php")
  urls <- paste0("http://example.com/a/../", pairs)
  expect_identical(
    safe_parse_urls(urls, index_page_handling = "strip")$clean_url,
    rep("http://example.com/", length(pairs))
  )
})

test_that("path_normalization = 'none' still means none for visible dots", {
  # Dot segments that were ALREADY visible before the strip are the caller's
  # normalization choice to make; resolving them would be an unrequested
  # normalization. Only a dot segment the strip left in FINAL position is
  # resolved.
  expect_identical(
    get_path("http://example.com/a/./b/index.html",
      index_page_handling = "strip"
    ),
    "/a/./b"
  )
  expect_identical(
    get_path("http://example.com/a/../b/index.html",
      index_page_handling = "strip"
    ),
    "/a/../b"
  )
})

test_that("index_page_handling = 'keep' output is untouched", {
  inputs <- c(
    "http://example.com/a/./b/../index.html",
    "http://example.com/../index.html",
    "http://example.com/a/../default.aspx"
  )
  expect_identical(
    safe_parse_urls(inputs, index_page_handling = "keep")$clean_url,
    inputs
  )
})

test_that("selector arms are unreachable by this branch and stay conformant", {
  # Both standard profiles set path_normalization = "dot_segments", which runs
  # at step 2 and leaves the strip nothing to expose -- so these rows are
  # produced by the pre-existing route, not by the new resolution.
  inputs <- c(
    "http://example.com/a/./b/../index.html",
    "http://example.com/a/../index.html",
    "http://example.com/../index.html"
  )
  expect_identical(
    safe_parse_urls(inputs,
      url_standard = "whatwg", index_page_handling = "strip"
    )$clean_url,
    c("http://example.com/a", "http://example.com/", "http://example.com/")
  )
  expect_identical(
    safe_parse_urls(inputs,
      url_standard = "rfc3986", index_page_handling = "strip"
    )$clean_url,
    c("http://example.com/a", "http://example.com/", "http://example.com/")
  )
})

test_that("the invariant holds across the frame, not just one URL shape", {
  # The one-at-a-time corpus trap: a frame-specific fix passes a fixed-frame
  # probe by coincidence. Vary the frame (scheme, port, userinfo, query,
  # fragment, trailing slash, scheme-relative) as well as the dot payload.
  frames <- c(
    "http://example.com%s/%s", "https://example.com%s/%s",
    "http://example.com:8080%s/%s", "http://user:pw@example.com%s/%s",
    "http://example.com%s/%s?q=1", "http://example.com%s/%s#frag",
    "http://example.com%s/%s/", "ftp://example.com%s/%s",
    "//example.com%s/%s"
  )
  payloads <- c("", "/.", "/..", "/a/.", "/a/..", "/./a", "/../a",
                "/a/./b/..", "/a/b/../..")
  grid <- expand.grid(frame = frames, payload = payloads,
                      stringsAsFactors = FALSE)
  urls <- sprintf(grid$frame, grid$payload, "index.html")

  for (pn in c("none", "collapse_slashes", "dot_segments", "both")) {
    for (ts in c("none", "keep", "strip")) {
      cleaned <- safe_parse_urls(urls,
        index_page_handling = "strip", path_normalization = pn,
        trailing_slash_handling = ts
      )$clean_url
      paths <- get_path(cleaned)
      offenders <- cleaned[!is.na(paths) &
        stringi::stri_detect_regex(paths, "(^|/)\\.{1,2}/?$")]
      expect_identical(offenders, character(0))
    }
  }
})

test_that("._strip_index_page is total on bases that are pure dot segments", {
  # Unit-level guard on the helper itself: every return path is a resolved,
  # non-empty, root-anchored path.
  expect_identical(._strip_index_page("/./index.html"), "/")
  expect_identical(._strip_index_page("/../index.html"), "/")
  expect_identical(._strip_index_page("/a/../index.html"), "/")
  expect_identical(._strip_index_page("/a/./b/../index.html"), "/a/")
  expect_identical(._strip_index_page("a/../index.html"), "/")
  expect_identical(._strip_index_page("/a/b/index.html"), "/a/b")
  expect_identical(._strip_index_page("/index.html"), "/")
})
