# RURL-lzsjlxgu: under `scheme_acceptance = "general"`, the clean_url of a
# non-special hierarchical URL kept the dot segments its parsed `path` column
# removed.
#
# Posture: clean surface. `clean_url` is never a conformance oracle (ADR 0017),
# but one row must not contradict itself: the `path` column and the path inside
# `clean_url` are two renderings of the same parse. The general route renders
# its clean_url through `.serialize_whatwg_vec()` or
# `.serialize_rfc_generic_vec()` (R/parse.R, the ADR 0012 Layer 4b-2
# serializer dispatch), not through `.build_clean_url_vec()`, so the
# special-scheme rows below are the control for the other renderer.

general_rows <- function(urls, ...) {
  safe_parse_urls(urls, scheme_acceptance = "general", ...)
}

# --- Pins: behavior the fix must not move ------------------------------------

test_that("special schemes already clean to the dot-removed path", {
  urls <- c(
    "http://example.com/a/../b", "https://example.com/a/./b/../c",
    "ws://example.com/a/%2e%2e/b", "file:///a/../b"
  )
  expected <- c(
    "http://example.com/b", "https://example.com/a/c", "ws://example.com/b",
    "file:///b"
  )
  expect_identical(general_rows(urls, url_standard = "whatwg")$clean_url,
                   expected)
  # Under rfc3986 only the web schemes take the special-scheme builder; `ws:`
  # and `file:` are general-routed there (the rfc3986 pin below).
  expect_identical(
    general_rows(urls[1:2], url_standard = "rfc3986")$clean_url, expected[1:2]
  )
  # The web acceptance renders through the same special-scheme builder.
  r <- safe_parse_urls(urls[1:2], url_standard = "whatwg")
  expect_identical(r$clean_url, expected[1:2])
})

test_that("NULL: the frozen arm's clean_url follows path_normalization", {
  # `scheme_acceptance = "general"` is an error under url_standard = NULL, so
  # the general-route serializers are unreachable from the NULL arm. Its
  # default `path_normalization = "none"` keeps the dots in both columns.
  urls <- c("http://example.com/a/../b", "https://example.com/a/./b/../c")
  for (r in list(safe_parse_urls(urls),
                 safe_parse_urls(urls, url_standard = NULL))) {
    expect_identical(r$path, c("/a/../b", "/a/./b/../c"))
    expect_identical(r$clean_url, urls)
  }
  expect_identical(
    safe_parse_urls(urls, path_normalization = "dot_segments")$clean_url,
    c("http://example.com/b", "https://example.com/a/c")
  )
  expect_error(safe_parse_urls("foo:/a/../b", scheme_acceptance = "general"))
})

test_that("whatwg: an opaque path keeps its dot segments", {
  # Opaque path state does no dot handling, so `path` and clean_url agree.
  r <- general_rows(c("foo:a/../b", "foo:./x", "mailto:a/../b@example.com"),
                    url_standard = "whatwg")
  expect_identical(r$path, c("a/../b", "./x", "a/../b@example.com"))
  expect_identical(
    r$clean_url, c("foo:a/../b", "foo:./x", "mailto:a/../b@example.com")
  )
})

test_that("whatwg: a non-special path with no dot segment does not move", {
  urls <- c(
    "foo:/a/b", "foo://h/a/b/", "foo://h/a/b?q=1#f", "foo://h:99/a",
    "foo:///a", "foo://h", "foo:/.//p", "foo://h/a%2Fb", "foo://h/a.b/..c/c.."
  )
  r <- general_rows(urls, url_standard = "whatwg", port_handling = "keep")
  expect_identical(
    r$clean_url,
    c(
      "foo:/a/b", "foo://h/a/b/", "foo://h/a/b?q=1", "foo://h:99/a",
      "foo:///a", "foo://h", "foo:/.//p", "foo://h/a%2Fb",
      "foo://h/a.b/..c/c.."
    )
  )
  expect_identical(
    general_rows("foo://h/a/", url_standard = "whatwg",
                 trailing_slash_handling = "strip")$clean_url,
    "foo://h/a/"
  )
})

test_that("rfc3986: clean_url carries the normalized path", {
  # Owner ruling 2026-10-11 (RURL-rwlrcvnh): `path_normalization` governs the
  # general-route clean_url. The direct bundle normalizes (RFC 3986 sec
  # 6.2.2.2, then sec 5.2.4 dot removal, which has no rootless exemption);
  # where the result would start with "//" with no authority, the row keeps
  # its source path (sec 3.3). `ws:` and `file:` are general-routed under
  # rfc3986, so they follow too.
  urls <- c("foo:/a/../b", "foo://h/a/../b", "foo://h/a/%2e%2e/b",
            "foo:a/../b", "foo:/a/..//p", "ws://example.com/a/../b")
  r <- general_rows(urls, url_standard = "rfc3986")
  expect_identical(r$path, c("/b", "/b", "/b", "/b", "//p", "/b"))
  expect_identical(
    r$clean_url,
    c("foo:/b", "foo://h/b", "foo://h/b", "foo:/b", "foo:/a/..//p",
      "ws://example.com/b")
  )
})

test_that("rfc-syntax: no path normalization, so path and clean_url agree", {
  urls <- c("foo:/a/../b", "foo://h/a/./b/", "foo:a/../b", "http://h/a/../b")
  r <- safe_parse_urls(urls, profile = "rfc-syntax")
  expect_identical(r$path, c("/a/../b", "/a/./b/", "a/../b", "/a/../b"))
  expect_identical(r$clean_url, urls)
})

# --- The fix -----------------------------------------------------------------

test_that("whatwg: a non-special list path cleans to its parsed path", {
  # WHATWG URL Standard, path state: a `.` or `..` segment (ASCII
  # case-insensitively also `%2e`, `.%2e`, `%2e.`, `%2e%2e`) is removed while
  # parsing, and `..` shortens the path. The URL serializer then writes that
  # path, with `/.` ahead of a hostless path that starts with an empty segment.
  urls <- c(
    "foo:/a/../b", "foo://h/a/../b", "foo://h/a/%2e%2e/b", "foo://h/a/.%2E/b",
    "foo://h/a/./b/", "foo:/a/./b", "foo://h/a/..", "foo:/.", "foo:/a/..//p",
    "foo://h/a/../b?q=1#f", "foo:///a/../b"
  )
  expected <- c(
    "foo:/b", "foo://h/b", "foo://h/b", "foo://h/b", "foo://h/a/b/",
    "foo:/a/b", "foo://h/", "foo:/", "foo:/.//p", "foo://h/b?q=1",
    "foo:///b"
  )
  for (args in list(list(url_standard = "whatwg"),
                    list(profile = "whatwg"))) {
    r <- do.call(general_rows, c(list(urls), args))
    expect_identical(r$clean_url, expected, label = names(args))
    # One row, one path: re-parsing clean_url gives back the `path` column.
    again <- do.call(general_rows, c(list(r$clean_url), args))
    expect_identical(again$path, r$path, label = names(args))
  }
})

test_that("whatwg: the trailing-slash strip still sees the dot-removed path", {
  expect_identical(
    general_rows(c("foo://h/a/..", "foo://h/a/../b/"), url_standard = "whatwg",
                 trailing_slash_handling = "strip")$clean_url,
    c("foo://h", "foo://h/b/")
  )
})

test_that("whatwg: a path_normalization override does not reach clean_url", {
  # Only the path state's dot removal applies to a non-special list path;
  # slash collapsing is a hierarchical transform (ADR 0012 D2), and an
  # explicit "none" over a profile does not undo what the parse removed.
  urls <- c("foo:/.//p", "foo://h/a//b/../c", "foo:/a/../b")
  expected <- c("foo:/.//p", "foo://h/a//c", "foo:/b")
  for (pn in c("both", "collapse_slashes", "none")) {
    expect_identical(
      general_rows(urls, profile = "whatwg", path_normalization = pn)$clean_url,
      expected, label = pn
    )
  }
})

test_that("whatwg: resolve_url()'s clean output follows the reference", {
  expect_identical(
    resolve_url(c("foo:/a/../b", "foo://h/a/./b", "foo:a/../b"), "http://h/y",
                url_standard = "whatwg", scheme_acceptance = "general"),
    c("foo:/b", "foo://h/a/b", "foo:a/../b")
  )
})

# --- RURL-rwlrcvnh / RURL-dsjqseap: the rfc3986 arm --------------------------
#
# Pins for the ruling that `path_normalization` governs the general-route
# clean_url under `url_standard = "rfc3986"`, and that a normalized path which
# would start with "//" on a hostless row keeps its source path (RFC 3986
# sec 3.3). What that change must not move:

rfc_rows <- c(
  "foo:/a/../b", "foo://h/a/../b", "foo://h/a/%2e%2e/b", "foo:a/../b",
  "foo:/a/..//p", "foo://h/a/..//p", "ws://example.com/a/../b",
  "file:///a/../b", "foo://h/%41b/C%2fd", "foo://h/a/./b/", "foo:a/..",
  "foo:/a/b", "http:/a/..//p", "urn:a:b/../c"
)

test_that("rfc3986: the path column is already normalized", {
  r <- general_rows(rfc_rows, url_standard = "rfc3986")
  expect_identical(
    r$path,
    c(
      "/b", "/b", "/b", "/b", "//p", "//p", "/b", "/b", "/Ab/C%2Fd", "/a/b/",
      "/", "/a/b", "//p", "/c"
    )
  )
})

test_that("rfc-syntax: clean_url keeps the source path on the general route", {
  expect_identical(
    safe_parse_urls(rfc_rows, profile = "rfc-syntax")$clean_url, rfc_rows
  )
  expect_identical(
    general_rows(rfc_rows, profile = "rfc-syntax")$path,
    c(
      "/a/../b", "/a/../b", "/a/%2E%2E/b", "a/../b", "/a/..//p", "/a/..//p",
      "/a/../b", "/a/../b", "/%41b/C%2Fd", "/a/./b/", "a/..", "/a/b",
      "/a/..//p", "a:b/../c"
    )
  )
})

test_that("rfc3986: the web route already cleans to the normalized path", {
  expect_identical(
    safe_parse_urls(c("http://example.com/a/../b", "https://example.com/./x"),
                    url_standard = "rfc3986")$clean_url,
    c("http://example.com/b", "https://example.com/x")
  )
})

test_that("serialize_url(): what the //-guard must not move", {
  # `source` never normalizes; `normalized` removes dots from a path with an
  # authority or a leading "/", and leaves a rootless path as written.
  expect_identical(
    serialize_url(rfc_rows, standard = "rfc3986", form = "source"), rfc_rows
  )
  normalized <- serialize_url(rfc_rows, standard = "rfc3986",
                              form = "normalized")
  keep <- !rfc_rows %in% c("foo:/a/..//p", "http:/a/..//p")
  expect_identical(
    normalized[keep],
    c(
      "foo:/b", "foo://h/b", "foo://h/b", "foo:a/../b", "foo://h//p",
      "ws://example.com/b", "file:///b", "foo://h/Ab/C%2Fd", "foo://h/a/b/",
      "foo:a/..", "foo:/a/b", "urn:a:b/../c"
    )
  )
  expect_identical(
    serialize_url(c("foo:/a/..//p", "foo://h/a/..//p"), standard = "whatwg"),
    c("foo:/.//p", "foo://h//p")
  )
})

# --- The rfc3986 fix ---------------------------------------------------------

test_that("rfc3986: every general row cleans to its normalized path", {
  r <- general_rows(rfc_rows, url_standard = "rfc3986")
  expect_identical(
    r$clean_url,
    c(
      "foo:/b", "foo://h/b", "foo://h/b", "foo:/b", "foo:/a/..//p",
      "foo://h//p", "ws://example.com/b", "file:///b", "foo://h/Ab/C%2Fd",
      "foo://h/a/b/", "foo:/", "foo:/a/b", "http:/a/..//p", "urn:/c"
    )
  )
  # One row, one path: re-parsing clean_url gives back the `path` column,
  # including the two rows that kept their source path.
  again <- general_rows(r$clean_url, url_standard = "rfc3986")
  expect_identical(again$path, r$path)
  expect_true(all(is.na(again$host[c(1, 4, 5, 11, 12, 13, 14)])))
})

test_that("rfc-syntax: an explicit dot_segments reaches clean_url", {
  # The dial governs, not the profile: a caller who asks rfc-syntax for dot
  # removal gets it in clean_url, and slash collapsing still does not reach a
  # non-special path (ADR 0012 D2).
  urls <- c("foo:/a/../b", "foo:/a/..//p", "foo://h/a//b/../c")
  for (pn in c("dot_segments", "both")) {
    expect_identical(
      general_rows(urls, profile = "rfc-syntax",
                   path_normalization = pn)$clean_url,
      c("foo:/b", "foo:/a/..//p", "foo://h/a//c"),
      label = pn
    )
  }
})

test_that("serialize_url(): a normalized hostless path never starts with //", {
  # RURL-dsjqseap. RFC 3986 sec 3.3: "When authority is not present, the
  # path cannot begin with two slash characters". `foo:/a/..//p` normalized
  # to `foo://p`, which names the host "p".
  urls <- c("foo:/a/..//p", "foo:/.//p", "foo:/..//p", "http:/a/..//p")
  normalized <- serialize_url(urls, standard = "rfc3986", form = "normalized")
  expect_identical(
    normalized, c("foo:/a/..//p", "foo:/.//p", "foo:/..//p", "http:/a/..//p")
  )
  again <- general_rows(normalized, url_standard = "rfc3986")
  expect_true(all(is.na(again$host)))
  expect_identical(again$path, rep("//p", 4L))
})
