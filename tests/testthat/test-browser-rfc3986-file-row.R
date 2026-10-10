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

# --- The fix: a fixed row parses as its fixed spelling -----------------------

# RFC 8089 §2: `file-URI = file-scheme ":" file-hier-part`, with
# `file-hier-part = ( "//" auth-path ) / local-path` and `auth-path =
# [ file-auth ] path-absolute`; RFC 3986 §3.2 / §3.3: after `//` the authority
# runs to the next `/`, so in `file:///a/b` it is empty and the path is `/a/b`.
# The trimmed and `;` spellings were read by the web route, which took `a` as
# the host.
test_that("a fixed file:///a/b under browser + rfc3986 has no host", {
  u <- c("  file:///a/b", "file:///a/b  ", "file;///a/b", " file;///a/b ")
  r <- .bff_parse(u, profile = "browser", url_standard = "rfc3986")
  expect_identical(r$scheme, rep("file", 4L))
  expect_identical(r$host, rep(NA_character_, 4L))
  expect_identical(r$path, rep("/a/b", 4L))
  expect_identical(r$clean_url, rep("file:///a/b", 4L))
  expect_identical(r$parse_status, rep("ok", 4L))
})

# RFC 3986 §3: `hier-part` with no `//` is `path-rootless`, so `file:p` is the
# path `p`; RFC 8089 §2's narrower `local-path = path-absolute` is an overlay
# reported as a fact, not a parse gate (ADR 0012 D5). The fixed `file;p` was a
# parse failure.
test_that("a fixed file:p under browser + rfc3986 is the path p", {
  u <- c("file;p", "  file:p", "FILE;p", "file;p  ")
  r <- .bff_parse(u, profile = "browser", url_standard = "rfc3986")
  expect_identical(r$scheme, rep("file", 4L))
  expect_identical(r$host, rep(NA_character_, 4L))
  expect_identical(r$path, rep("p", 4L))
  expect_identical(r$clean_url, rep("file:p", 4L))
  expect_identical(r$parse_status, rep("ok", 4L))
})

# The rule itself, looped over both arms the browser profile reaches: a row the
# fixer rewrote has the parse record of its fixed spelling typed directly.
test_that("a fixed row parses as its fixed spelling under either standard", {
  u <- c(
    "  file:///a/b", "file;///a/b", "file;p", "  file:p", "FILE;p",
    "  file:", "file;", "file;//", "  file:/a", "file;//h/p", " file://h/p ",
    "http;/a", "http;///a", "https;//", "  http://a/b", "http;//a/b",
    "  mailto:x@y.com", "  urn:a:b ", "  foo:bar", "ws;a"
  )
  for (std in c("whatwg", "rfc3986")) {
    fixed <- rurl:::.apply_browser_fixup_vec(u, "browser", std, "infer")
    expect_false(any(fixed == u))
    expect_identical(
      .bff_parse(u, profile = "browser", url_standard = std),
      .bff_parse(fixed, profile = "browser", url_standard = std),
      info = std
    )
  }
})

# Step 3's `://` insertion now reaches the rows the rfc3986 general route used
# to claim on their raw spelling, so `http:a/b` reads like `  http:a/b` and
# `http;a/b`, which the web route already read as `http://a/b`.
test_that("step 3's // insertion holds under browser + rfc3986", {
  u <- c("http:a/b", "  http:a/b", "http;a/b", "http:/a", "HTTPS:a.com")
  r <- .bff_parse(u, profile = "browser", url_standard = "rfc3986")
  expect_identical(r$host, c("a", "a", "a", NA, "a.com"))
  expect_identical(r$path, c("/b", "/b", "/b", "/a", ""))
  expect_identical(
    r,
    .bff_parse(
      rurl:::.apply_browser_fixup_vec(u, "browser", "rfc3986", "infer"),
      profile = "browser", url_standard = "rfc3986"
    )
  )
})
