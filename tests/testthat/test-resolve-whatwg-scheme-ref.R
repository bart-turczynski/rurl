# RURL-rdkewxlx: a reference with a scheme of its own under `whatwg`.
#
# The WHATWG URL Standard's basic URL parser reads a reference whose scheme is
# not the base's special scheme without the base: the base's path never enters
# it, and the reference's own path goes through "path state" (special and
# hierarchical paths: `.` and `..` segments, "shorten a URL's path") or
# "opaque path state" (no dot handling at all). Every `whatwg`
# `output = "serialized"` expected value below is `new URL(ref, base).href` in
# Node 26; the `clean` outputs and the raw resolver values are rurl's own
# spellings, not Node hrefs.

whatwg_ser <- function(ref, base) {
  resolve_url(ref, base, url_standard = "whatwg", output = "serialized")
}
rfc_ser <- function(ref, base) {
  resolve_url(ref, base, url_standard = "rfc3986", output = "serialized")
}

# --- Pins: behavior the fix must not move ------------------------------------

test_that("whatwg: a hierarchical scheme-bearing reference loses its dots", {
  # Special schemes with an authority: "path state" removes the dot segments.
  expect_identical(whatwg_ser("https://h2/a/./b/../c", "http://h/y"),
                   "https://h2/a/c")
  expect_identical(whatwg_ser("http://h2/a/../b", "http://h/y"),
                   "http://h2/b")
  expect_identical(whatwg_ser("ws://h2/a/../b", "http://h/y"), "ws://h2/b")
  expect_identical(
    resolve_url("https://h2/a/./b/../c", "http://h/y", url_standard = "whatwg"),
    "https://h2/a/c"
  )
  # A non-special scheme whose path starts with `/` is not opaque.
  expect_identical(whatwg_ser("foo:/a/../b", "http://h/y"), "foo:/b")
  expect_identical(whatwg_ser("foo://h/a/../b", "http://h/y"), "foo://h/b")
  expect_identical(whatwg_ser("foo:/a/%2e%2e/b", "http://h/y"), "foo:/b")
  expect_identical(whatwg_ser("foo:/..//p", "http://h/y"), "foo:/.//p")
  # `file:` without a drive letter.
  expect_identical(whatwg_ser("file:/a/./b/../c", "http://h/y"),
                   "file:///a/c")
})

test_that("whatwg: the base's own special scheme stays relative", {
  # "special relative or authority state": the scheme is consumed and the
  # remainder is resolved against the base, so the base path is shortened.
  expect_identical(whatwg_ser("http:../x", "http://h/a/b"), "http://h/x")
  expect_identical(whatwg_ser("HTTP:../x", "http://h/a/b"), "http://h/x")
  expect_identical(whatwg_ser("https:./a/../x", "https://h/a/b"),
                   "https://h/a/x")
  expect_identical(resolve_url("http:../x", "http://h/a/b",
                               url_standard = "whatwg"),
                   "http://h/x")
  # `file:` against a `file:` base keeps its drive letter.
  expect_identical(whatwg_ser("file:/C:/../x", "file:///y"), "file:///C:/x")
  expect_identical(whatwg_ser("file:C|/../x", "file:///y"), "file:///C:/x")
})

test_that("rfc3986: a scheme-bearing reference's path is dot-removed", {
  # Section 5.2.2: T.path = remove_dot_segments(R.path), whatever the scheme.
  expect_identical(rfc_ser("foo:a/../b", "http://h/y"), "foo:/b")
  expect_identical(rfc_ser("foo:\ufeff/../b", "http://h/y"), "foo:/b")
  expect_identical(rfc_ser("file:/C:/../x", "http://h/y"), "file:/x")
  expect_identical(rfc_ser("file:/C:/../x", "file:///y"), "file:/x")
  expect_identical(rfc_ser("http:/a/../b", "https://h/y"), "http:/b")
  expect_identical(rfc_ser("ws:a/../b", "http://h/y"), "ws:/b")
  expect_identical(rfc_ser("http:../x", "http://h/a/b"), "http:x")
  expect_identical(rfc_ser("https://h2/a/./b/../c", "http://h/y"),
                   "https://h2/a/c")
  expect_identical(.resolve_one_raw("foo:a/../b", "http://h/y", "rfc3986"),
                   "foo:/b")
  expect_identical(.resolve_one_raw("non-spec:/..//x", "http://a/b", "rfc3986"),
                   "non-spec:/.//x")
})

test_that("NULL: the frozen selector's scheme-bearing resolution", {
  frozen <- list(
    c("foo:a/../b", "http://h/y", "foo:/b"),
    c("foo:\ufeff/../b", "http://h/y", "foo:/b"),
    c("file:/C:/../x", "http://h/y", "file:/x"),
    c("file:///C:/../x", "http://h/y", "file:///x"),
    c("http:/a/../b", "https://h/y", "http:/b"),
    c("ws:a/../b", "http://h/y", "ws:/b"),
    c("http:../x", "http://h/a/b", "http:x"),
    c("non-spec:/..//x", "http://a/b", "non-spec://x")
  )
  for (row in frozen) {
    expect_identical(.resolve_one_raw(row[[1L]], row[[2L]]), row[[3L]],
                     label = row[[1L]])
    expect_identical(.resolve_one_raw(row[[1L]], row[[2L]], NULL), row[[3L]],
                     label = paste("NULL", row[[1L]]))
  }
  expect_identical(resolve_url("https://h2/a/./b/../c", "http://h/y"),
                   "https://h2/a/c")
  expect_identical(resolve_url("file:///C:/../x", "http://h/y"),
                   "file:///x")
  expect_identical(resolve_url("file:/C:/../x", "http://h/y"), NA_character_)
  expect_identical(resolve_url("http:/a/../b", "https://h/y"), NA_character_)
})

# --- Conformance: the reference is read without the base ----------------------

test_that("whatwg: an opaque path keeps its dot segments", {
  # "opaque path state" removes nothing; RFC 3986 section 5.2.4 used to run
  # first and gave `foo:/b`, a path that is no longer opaque.
  expect_identical(whatwg_ser("foo:a/../b", "http://h/y"), "foo:a/../b")
  expect_identical(whatwg_ser("foo:\ufeff/../b", "http://h/y"),
                   "foo:%EF%BB%BF/../b")
  expect_identical(whatwg_ser("foo:\ufeff/./b", "http://h/y"),
                   "foo:%EF%BB%BF/./b")
  expect_identical(whatwg_ser("foo:a/./b", "http://h/y"), "foo:a/./b")
  expect_identical(whatwg_ser("foo:./a", "http://h/y"), "foo:./a")
  expect_identical(whatwg_ser("foo:..", "http://h/y"), "foo:..")
  expect_identical(whatwg_ser("foo:a/../b", "foo:/y"), "foo:a/../b")
  expect_identical(whatwg_ser("foo:a/../b?q/../r#f/../g", "http://h/y"),
                   "foo:a/../b?q/../r#f/../g")
  expect_identical(.resolve_one_raw("foo:a/../b", "http://h/y", "whatwg"),
                   "foo:a/../b")
})

test_that("whatwg: a file: reference against another base keeps its drive", {
  # "shorten a URL's path" does not remove a lone normalized drive letter.
  for (base in c("http://h/y", "https://h/y", "foo://h/y")) {
    expect_identical(whatwg_ser("file:/C:/../x", base), "file:///C:/x",
                     label = base)
    expect_identical(whatwg_ser("file:///C:/../x", base), "file:///C:/x",
                     label = base)
    expect_identical(whatwg_ser("file:C|/../x", base), "file:///C:/x",
                     label = base)
  }
  expect_identical(whatwg_ser("file:/C:/..", "http://h/y"), "file:///C:/")
  expect_identical(whatwg_ser("file:/C|/a/../b", "https://h/y"),
                   "file:///C:/b")
  expect_identical(whatwg_ser("file:..", "http://h/y"), "file:///")
  # The same answer the reference already got against a `file:` base.
  expect_identical(whatwg_ser("file:/C:/../x", "http://h/y"),
                   whatwg_ser("file:/C:/../x", "file:///y"))
  expect_identical(
    resolve_url("file:/C:/../x", "http://h/y", url_standard = "whatwg"),
    "file:///C:/x"
  )
})

test_that("whatwg: a special scheme without `//` keeps its host", {
  # "special authority ignore slashes state" reads the first segment as the
  # host, so dot removal belongs to the path after it, not to the host.
  expect_identical(whatwg_ser("http:/a/../b", "https://h/y"), "http://a/b")
  expect_identical(whatwg_ser("https:/a/../b", "http://h/y"), "https://a/b")
  expect_identical(whatwg_ser("ws:a/../b", "http://h/y"), "ws://a/b")
  expect_identical(whatwg_ser("ftp:a/../b", "http://h/y"), "ftp://a/b")
  expect_identical(whatwg_ser("wss:/a/./b/../c", "ws://h/y"), "wss://a/c")
  expect_identical(whatwg_ser("http:/../x", "https://h/y"), "http://../x")
  expect_identical(whatwg_ser("https:..", "http://h/y"), "https://../")
  expect_identical(
    resolve_url("http:/a/../b", "https://h/y", url_standard = "whatwg"),
    "http://a/b"
  )
})

test_that("whatwg: resolving a scheme-bearing reference equals parsing it", {
  # The basic URL parser ignores the base unless the reference carries the
  # base's own special scheme, so each row resolves to its own parse.
  refs <- c(
    "foo:a/../b", "foo:\ufeff/../b", "foo:/a/../b", "foo://h/a/../b",
    "foo:/a/%2e%2e/b", "file:/C:/../x", "file:///C:/../x", "file:C|/../x",
    "file:/a/./b/../c", "http:/a/../b", "ws:a/../b", "https://h2/a/./b/../c"
  )
  for (ref in refs) {
    expect_identical(whatwg_ser(ref, "foo://h/y"),
                     serialize_url(ref, standard = "whatwg"), label = ref)
  }
  # `https:` against an `https:` base is the one relative row.
  for (ref in setdiff(refs, "https://h2/a/./b/../c")) {
    expect_identical(whatwg_ser(ref, "https://h/y"),
                     serialize_url(ref, standard = "whatwg"), label = ref)
  }
})

test_that("whatwg: a scheme-bearing reference cleans to its own clean_url", {
  # Under general acceptance a non-special scheme's clean_url keeps the dot
  # segments its parsed path drops (RURL-lzsjlxgu); the resolved clean output
  # follows the reference's own parse rather than hiding that.
  refs <- c("foo:/a/../b", "foo://h/a/../b", "foo:a/../b", "http:/a/../b")
  for (ref in refs) {
    expect_identical(
      resolve_url(ref, "https://h/y", url_standard = "whatwg",
                  scheme_acceptance = "general"),
      safe_parse_url(ref, url_standard = "whatwg",
                     scheme_acceptance = "general")$clean_url,
      label = ref
    )
  }
  expect_identical(
    resolve_url("foo:/a/../b", "http://h/y", url_standard = "whatwg",
                scheme_acceptance = "general"),
    "foo:/a/../b"
  )
})
