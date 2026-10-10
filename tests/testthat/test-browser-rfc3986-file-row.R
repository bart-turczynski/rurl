# RURL-vmsmlflr: under `profile = "browser"` with an explicit
# `url_standard = "rfc3986"`, a row the bounded browser fixer rewrote (outer
# trim, `;`->`:`, `://` insertion) parses exactly as its fixed spelling would if
# typed directly. The fixer runs in front of every route, so the general route
# (which owns the RFC 8089 `file:` overlay under `rfc3986`) reads the fixed
# string too, not the raw input.

# The parse-record columns compared below; `original_url` is the input and is
# the one column that must differ between a raw and a fixed spelling.
.bff_cols <- c(
  "scheme", "host", "port", "path", "query", "fragment", "user", "password",
  "clean_url", "parse_status"
)

.bff_parse <- function(u, ...) {
  as.data.frame(safe_parse_urls(u, ...))[, .bff_cols]
}

# --- Pins: behavior the fix must NOT move ------------------------------------

test_that("an unfixed file: row under browser + rfc3986 reads RFC 8089", {
  r <- .bff_parse(
    c("file:///a/b", "file:p", "file://h/p", "file:"),
    profile = "browser", url_standard = "rfc3986"
  )
  expect_identical(r$scheme, rep("file", 4L))
  expect_identical(r$host, c(NA, NA, "h", NA))
  expect_identical(r$path, c("/a/b", "p", "/p", ""))
  expect_identical(
    r$clean_url, c("file:///a/b", "file:p", "file://h/p", "file:")
  )
  expect_identical(r$parse_status, rep("ok", 4L))
})

test_that("the browser profile's default whatwg arm reads fixed file: rows", {
  u <- c("  file:///a/b", "file;///a/b", "file;p", "  file:p", "FILE;p")
  r <- .bff_parse(u, profile = "browser")
  expect_identical(r$scheme, rep("file", 5L))
  expect_identical(r$host, rep(NA_character_, 5L))
  expect_identical(r$path, c("/a/b", "/a/b", "/p", "/p", "/p"))
  expect_identical(
    r$clean_url,
    c("file:///a/b", "file:///a/b", "file:///p", "file:///p", "file:///p")
  )
  # An explicit NULL is not an override: the browser bundle's whatwg stands.
  expect_identical(
    .bff_parse(u, profile = "browser", url_standard = NULL), r
  )
})

test_that("without the browser fixer, rfc3986 does not repair the spelling", {
  r <- .bff_parse(
    c("file:///a/b", "  file:///a/b", "file;///a/b", "file:p", "file;p"),
    url_standard = "rfc3986"
  )
  expect_identical(r$parse_status, c("ok", "error", "error", "error", "error"))
  expect_identical(r$host, rep(NA_character_, 5L))
  expect_identical(r$path, c("/a/b", NA, NA, "p", NA))
})

test_that("fixed web rows under browser + rfc3986 keep their parse", {
  r <- .bff_parse(
    c(
      "  http://a/b", "http;//a/b", "  https://a.com/x?q#f", "HTTP;//A.com/p ",
      "  foo://h/p"
    ),
    profile = "browser", url_standard = "rfc3986"
  )
  expect_identical(r$scheme, c("http", "http", "https", "http", "foo"))
  expect_identical(r$host, c("a", "a", "a.com", "a.com", "h"))
  expect_identical(r$path, c("/b", "/b", "/x", "/p", "/p"))
  expect_identical(r$query, c(NA, NA, "q", NA, NA))
  expect_identical(r$fragment, c(NA, NA, "f", NA, NA))
  expect_identical(
    r$clean_url,
    c(
      "http://a/b", "http://a/b", "https://a.com/x", "http://a.com/p",
      "foo://h/p"
    )
  )
})
