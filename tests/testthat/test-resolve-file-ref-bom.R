# RURL-xrjtdnko: a scheme-bearing reference whose path starts with U+FEFF.
#
# The WHATWG URL Standard's path state removes `.` and `..` segments and
# nothing else, and RFC 3986 section 5.2.4 removes only a `.` or `..` that is
# a whole segment. A segment that starts with U+FEFF (the byte-order mark) is
# an ordinary segment under both. Every `whatwg` expected value below is
# `new URL(ref, base).href` in Node 26.

bom <- "\ufeff"
zwsp <- "\u200b"

whatwg_ser <- function(ref, base) {
  resolve_url(ref, base, url_standard = "whatwg", output = "serialized")
}

# --- Pins: behavior the fix must not move ------------------------------------

test_that("whatwg: a mark-led file: reference against a file: base keeps it", {
  # Against a `file:` base the reference is relative and takes the `file:`
  # path walk (RURL-dglgcwit), which already keeps the mark.
  expect_identical(whatwg_ser(paste0("file:", bom, "/./b"), "file:///y"),
                   "file:///%EF%BB%BF/b")
  expect_identical(
    resolve_url(paste0("file:", bom, "/./b"), "file:///y",
                url_standard = "whatwg"),
    "file:///\ufeff/b"
  )
  expect_identical(whatwg_ser(paste0("file:", bom, "/./b"), "file://h/y"),
                   "file://h/%EF%BB%BF/b")
  expect_identical(whatwg_ser(paste0("file:", bom, "../x"), "file:///y"),
                   "file:///%EF%BB%BF../x")
})

test_that("whatwg: the U+200B and http: twins already keep the code point", {
  expect_identical(whatwg_ser(paste0("file:", zwsp, "/./b"), "http://h/y"),
                   "file:///%E2%80%8B/b")
  expect_identical(
    resolve_url(paste0("file:", zwsp, "/./b"), "http://h/y",
                url_standard = "whatwg"),
    "file:///\u200b/b"
  )
  expect_identical(whatwg_ser(paste0("file:", zwsp, "../x"), "http://h/y"),
                   "file:///%E2%80%8B../x")
  # `http:` against an `http:` base is relative, as `file:` against `file:`.
  expect_identical(whatwg_ser(paste0("http:", bom, "/./b"), "http://h/y"),
                   "http://h/%EF%BB%BF/b")
  expect_identical(whatwg_ser(paste0("http:", zwsp, "/./b"), "http://h/y"),
                   "http://h/%E2%80%8B/b")
  # The mark leading a later segment is not at the start of the walk's input.
  expect_identical(whatwg_ser("file:/\ufeff/./b", "http://h/y"),
                   "file:///%EF%BB%BF/b")
})

test_that("whatwg: a file: reference against a non-file: base removes dots", {
  expect_identical(whatwg_ser("file:a/./b/../c", "http://h/y"), "file:///a/c")
  expect_identical(whatwg_ser("file:/./a/../b", "http://h/y"), "file:///b")
  expect_identical(whatwg_ser("file:./a/./b", "http://h/y"), "file:///a/b")
  expect_identical(whatwg_ser("file:../x", "http://h/y"), "file:///x")
})

test_that("RFC 3986 dot removal of ordinary segments does not move", {
  # Section 5.4.2 / 5.2.4 examples.
  expect_identical(._remove_dot_segments("/a/b/c/./../../g"), "/a/g")
  expect_identical(._remove_dot_segments("mid/content=5/../6"), "mid/6")
  # The mark that is a whole segment, or leads a later one, is kept.
  expect_identical(._remove_dot_segments("\ufeff.."), "\ufeff..")
  expect_identical(._remove_dot_segments("/\ufeff/./b"), "/\ufeff/b")
  # The whatwg remover matches with base R and already keeps the mark.
  expect_identical(._remove_dot_segments_whatwg("\ufeff../x"), "\ufeff../x")
  expect_identical(._remove_dot_segments_whatwg("\ufeff/./b"), "\ufeff/b")
  expect_identical(._remove_dot_segments_whatwg("\ufeff%2e/b"), "\ufeff%2e/b")
})

test_that("rfc3986 and NULL negative controls do not move", {
  rfc_ser <- function(ref, base) {
    resolve_url(ref, base, url_standard = "rfc3986", output = "serialized")
  }
  # Section 5.2.2: a scheme-bearing reference's path is dot-removed as is.
  expect_identical(rfc_ser("foo:a/./b/../c", "http://h/y"), "foo:a/c")
  expect_identical(rfc_ser(paste0("file:", zwsp, "/./b"), "http://h/y"),
                   "file:\u200b/b")
  expect_identical(rfc_ser("foo:/\ufeff/./b", "http://h/y"),
                   "foo:/\ufeff/b")
  # NULL: the frozen selector's resolution, omitted and explicit alike.
  expect_identical(resolve_url("a/./b/../c", "http://h/y"), "http://h/a/c")
  expect_identical(resolve_url("a/./b/../c", "http://h/y", url_standard = NULL),
                   "http://h/a/c")
  expect_identical(.resolve_one_raw("foo:a/./b/../c", "http://h/y", NULL),
                   "foo:a/c")
  expect_identical(
    .resolve_one_raw(paste0("file:", zwsp, "/./b"), "http://h/y", NULL),
    "file:\u200b/b"
  )
  # The parse's dot-segment step runs the same remover.
  expect_identical(
    safe_parse_url(paste0("file:", zwsp, "/./b"),
                   path_normalization = "dot_segments")$path,
    "\u200b/b"
  )
  expect_identical(
    safe_parse_url("file:/a/./b/../c",
                   path_normalization = "dot_segments")$path,
    "/a/c"
  )
})
