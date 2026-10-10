# RURL-xrjtdnko: a scheme-bearing reference whose path starts with U+FEFF.
#
# The WHATWG URL Standard's path state removes `.` and `..` segments and
# nothing else, and RFC 3986 section 5.2.4 removes only a `.` or `..` that is
# a whole segment. A segment that starts with U+FEFF (the byte-order mark) is
# an ordinary segment under both. Every `whatwg` `output = "serialized"`
# expected value below is `new URL(ref, base).href` in Node 26; the `clean`
# outputs and the raw resolver values are rurl's own spellings of the same
# URL, not Node hrefs.

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

# --- Conformance: the mark-led first segment survives -------------------------

test_that("whatwg: a mark-led file: reference keeps it whatever the base", {
  # A `file:` reference against a non-`file:` base is absolute, and its path
  # went through the RFC 3986 remover, which read past the mark: against
  # `http://h/y`, `file:\ufeff/./b` gave `file:///b`. Each U+200B twin sits
  # beside its U+FEFF row, and the href is the same against every base.
  rows <- list(
    c("/./b", "file:///%EF%BB%BF/b", "file:///%E2%80%8B/b"),
    c("../x", "file:///%EF%BB%BF../x", "file:///%E2%80%8B../x"),
    c("./x", "file:///%EF%BB%BF./x", "file:///%E2%80%8B./x"),
    c("/a/../b", "file:///%EF%BB%BF/b", "file:///%E2%80%8B/b"),
    c("/.", "file:///%EF%BB%BF/", "file:///%E2%80%8B/"),
    c("/../b", "file:///b", "file:///b")
  )
  for (base in c("http://h/y", "https://h/y", "foo://h/y", "file:///y")) {
    for (row in rows) {
      expect_identical(whatwg_ser(paste0("file:", bom, row[[1L]]), base),
                       row[[2L]], label = paste(row[[1L]], base))
      expect_identical(whatwg_ser(paste0("file:", zwsp, row[[1L]]), base),
                       row[[3L]], label = paste("U+200B", row[[1L]], base))
    }
  }
  # The unencoded form keeps the mark as written.
  expect_identical(
    resolve_url(paste0("file:", bom, "/./b"), "http://h/y",
                url_standard = "whatwg"),
    "file:///\ufeff/b"
  )
  expect_identical(.resolve_one_raw("file:\ufeff/./b", "http://h/y", "whatwg"),
                   "file:\ufeff/b")
  # The same remover serves every scheme other than the base's. An opaque
  # path keeps the mark; a special scheme whose authority is the mark fails,
  # since domain to ASCII maps it to the empty string (host parsing).
  expect_identical(whatwg_ser("foo:\ufeff../x", "http://h/y"),
                   "foo:%EF%BB%BF../x")
  expect_identical(whatwg_ser("http:\ufeff/./b", "file:///y"), NA_character_)
  expect_identical(whatwg_ser("http:\ufeff/./b", "foo://h/y"), NA_character_)
})

test_that("RFC 3986 dot removal keeps a mark-led first segment", {
  # Section 5.2.4: `\ufeff..` and `\ufeff.` are not dot segments (rules A and
  # D match only `../`, `./`, `.` and `..`), and rule E moves the first
  # segment to the output whole.
  expect_identical(._remove_dot_segments("\ufeff../x"), "\ufeff../x")
  expect_identical(._remove_dot_segments("\ufeff./x"), "\ufeff./x")
  expect_identical(._remove_dot_segments("\ufeff/./b"), "\ufeff/b")
  expect_identical(._remove_dot_segments("\ufeff/."), "\ufeff/")
  expect_identical(._remove_dot_segments("\ufeff/../b"), "/b")
  expect_identical(._remove_dot_segments("\ufeff/.."), "/")
  expect_identical(._remove_dot_segments("\u200b../x"), "\u200b../x")
})

test_that("rfc3986: a mark-led reference path keeps the mark", {
  rfc_ser <- function(ref, base) {
    resolve_url(ref, base, url_standard = "rfc3986", output = "serialized")
  }
  # Section 5.2.2: T.path = remove_dot_segments(R.path).
  expect_identical(rfc_ser("foo:\ufeff../x", "http://h/y"), "foo:\ufeff../x")
  expect_identical(rfc_ser("file:\ufeff/./b", "http://h/y"), "file:\ufeff/b")
  expect_identical(rfc_ser("http:\ufeff./x", "http://h/y"), "http:\ufeff./x")
  # Section 5.2.3: no authority and an empty base path merge to R.path.
  expect_identical(rfc_ser("\ufeff../x", "foo:"), "foo:\ufeff../x")
  # The parse's dot-segment step (section 6.2.2.3) runs the same remover.
  expect_identical(
    safe_parse_url("http:\ufeff/./b", url_standard = "rfc3986",
                   scheme_acceptance = "general")$path,
    "\ufeff/b"
  )
})

test_that("NULL: the default path keeps a mark-led first segment (ADR 0016)", {
  # Witness: the frozen selector runs the same RFC 3986 section 5.2.4
  # remover, and dropping the mark is sanctioned by no standard, so this is a
  # default-path defect, not selector-caused drift. Omitted and explicit
  # `NULL` agree.
  for (p in list(
    safe_parse_url("file:\ufeff/./b", path_normalization = "dot_segments"),
    safe_parse_url("file:\ufeff/./b", url_standard = NULL,
                   path_normalization = "dot_segments")
  )) {
    expect_identical(p$path, "\ufeff/b")
  }
  expect_identical(.resolve_one_raw("foo:\ufeff../x", "http://h/y", NULL),
                   "foo:\ufeff../x")
  expect_identical(.resolve_one_raw("file:\ufeff/./b", "http://h/y", NULL),
                   "file:\ufeff/b")
  # Signature: the fix moves only a path whose first segment starts with the
  # mark. `resolve_url()`'s clean output under NULL rejects these rows before
  # and after, and the parse's own path and clean_url without dot removal do
  # not move.
  expect_identical(resolve_url("file:\ufeff/./b", "http://h/y"), NA_character_)
  expect_identical(resolve_url("foo:\ufeff../x", "http://h/y"), NA_character_)
  p <- safe_parse_url("file:\ufeff/./b")
  expect_identical(p$path, "\ufeff/./b")
  expect_identical(p$clean_url, NA_character_)
})
