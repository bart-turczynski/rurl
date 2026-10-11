# RURL-kbjzwrkt: with `path_normalization = "both"` (or "collapse_slashes"),
# the `path` column of a general-routed row collapsed duplicate slashes, while
# its clean_url did not. ADR 0012 D2 keeps slash collapsing and the other
# hierarchical path transforms off a non-special path, and RUL-027 restates
# that for the general-route clean_url: only the identity and dot steps reach
# it. The `path` column and the path inside clean_url are two renderings of the
# same parse, so they must not disagree on the slashes.
#
# Reachability. `scheme_acceptance = "general"` is an error under
# `url_standard = NULL`, and a direct `url_standard = "rfc3986"` or
# `"whatwg"` rejects an explicit `path_normalization` as selector-governed.
# The only route to a general-routed row with a collapsing dial is a named
# profile plus an explicit override (ADR 0012 D6). The default `NULL` path can
# therefore never reach this code, so no NULL output moves and no ADR 0016
# drift evidence applies; the NULL test below pins both refusals.

dials <- c("both", "collapse_slashes")

# --- Reachability -------------------------------------------------------------

test_that("NULL and the direct selectors cannot reach a collapsing dial", {
  for (pn in dials) {
    expect_error(
      safe_parse_urls("foo://h/a//b", scheme_acceptance = "general",
                      path_normalization = pn),
      "requires an explicit url_standard"
    )
    for (std in c("rfc3986", "whatwg")) {
      expect_error(
        safe_parse_urls("foo://h/a//b", url_standard = std,
                        scheme_acceptance = "general",
                        path_normalization = pn),
        "governs `path_normalization`"
      )
    }
  }
})

# --- Controls: rows the fix must not move ------------------------------------

test_that("special schemes still collapse under both", {
  url <- "http://h.com/a//b/./c"
  for (prof in c("rfc-syntax", "whatwg", "browser")) {
    r <- safe_parse_urls(url, profile = prof, path_normalization = "both")
    expect_identical(r$path, "/a/b/c", label = prof)
    expect_identical(r$clean_url, "http://h.com/a/b/c", label = prof)
  }
  # Under whatwg, ws: and file: are special and take the web route.
  r <- safe_parse_urls(c("ws://h/a//b/./c", "file:///a//b/./c"),
                       profile = "whatwg", path_normalization = "both")
  expect_identical(r$path, c("/a/b/c", "/a/b/c"))
  expect_identical(r$clean_url, c("ws://h/a/b/c", "file:///a/b/c"))
  # The default NULL arm collapses on request, as before.
  r <- safe_parse_urls(c("http://h.com/a//b/./c", "file:///a//b/./c"),
                       path_normalization = "both")
  expect_identical(r$path, c("/a/b/c", "/a/b/c"))
  expect_identical(r$clean_url, c("http://h.com/a/b/c", "file:///a/b/c"))
  r <- safe_parse_urls("http://h.com//a///b",
                       path_normalization = "collapse_slashes")
  expect_identical(r$path, "/a/b")
})

test_that("general-route clean_url keeps the slashes under either dial", {
  url <- "foo://h/a//b/./c"
  for (prof in c("rfc-syntax", "whatwg", "browser")) {
    r <- safe_parse_urls(url, profile = prof, path_normalization = "both")
    expect_identical(r$clean_url, "foo://h/a//b/c", label = prof)
  }
  expect_identical(
    safe_parse_urls(url, profile = "rfc-syntax",
                    path_normalization = "collapse_slashes")$clean_url,
    "foo://h/a//b/./c"
  )
  # The whatwg arm runs the standard's dot step on clean_url whatever the dial.
  expect_identical(
    safe_parse_urls(url, profile = "whatwg",
                    path_normalization = "collapse_slashes")$clean_url,
    "foo://h/a//b/c"
  )
})

test_that("whatwg: an opaque path is untouched under either dial", {
  for (pn in dials) {
    r <- safe_parse_urls("foo:a//b/./c", profile = "whatwg",
                         path_normalization = pn)
    expect_identical(r$path, "a//b/./c", label = pn)
    expect_identical(r$clean_url, "foo:a//b/./c", label = pn)
  }
})

test_that("dot_segments and none already leave the slashes alone", {
  url <- "foo://h/a//b/./c"
  for (prof in c("rfc-syntax", "whatwg")) {
    expect_identical(
      safe_parse_urls(url, profile = prof,
                      path_normalization = "dot_segments")$path,
      "/a//b/c", label = prof
    )
    expect_identical(
      safe_parse_urls(url, profile = prof, path_normalization = "none")$path,
      "/a//b/./c", label = prof
    )
  }
})

# --- The fix ------------------------------------------------------------------
# Each test below was a DEFECT witness pinning the collapsed path (`/a/b/c`,
# `/a/b/./c`) before RURL-kbjzwrkt.

test_that("both: the general path column keeps the slashes clean_url keeps", {
  url <- "foo://h/a//b/./c"
  for (prof in c("rfc-syntax", "whatwg", "browser")) {
    r <- safe_parse_urls(url, profile = prof, path_normalization = "both")
    expect_identical(r$path, "/a//b/c", label = prof)
    expect_identical(r$clean_url, paste0("foo://h", r$path), label = prof)
  }
  r <- safe_parse_urls(url, profile = "browser", url_standard = "rfc3986",
                       path_normalization = "both")
  expect_identical(r$path, "/a//b/c")
  expect_identical(r$clean_url, "foo://h/a//b/c")
})

test_that("collapse_slashes: the general path column matches none", {
  url <- "foo://h/a//b/./c"
  for (prof in c("rfc-syntax", "whatwg")) {
    expect_identical(
      safe_parse_urls(url, profile = prof,
                      path_normalization = "collapse_slashes")$path,
      safe_parse_urls(url, profile = prof, path_normalization = "none")$path,
      label = prof
    )
  }
  # rfc-syntax: no dot step either, so the path matches clean_url exactly.
  r <- safe_parse_urls(url, profile = "rfc-syntax",
                       path_normalization = "collapse_slashes")
  expect_identical(r$path, "/a//b/./c")
  expect_identical(r$clean_url, "foo://h/a//b/./c")
})

test_that("every general-routed row under rfc-syntax keeps its slashes", {
  # Under rfc3986, ws:, file:, rootless paths and the odd-slash http: rows
  # take the general route and its generic serializer.
  urls <- c("ws://h/a//b/./c", "file:///a//b/./c", "foo:a//b/./c",
            "http:/a//b/./c", "urn:a//b")
  r <- safe_parse_urls(urls, profile = "rfc-syntax",
                       path_normalization = "both")
  expect_identical(
    r$path, c("/a//b/c", "/a//b/c", "a//b/c", "/a//b/c", "a//b")
  )
  expect_identical(
    r$clean_url,
    c("ws://h/a//b/c", "file:///a//b/c", "foo:a//b/c", "http:/a//b/c",
      "urn:a//b")
  )
})

test_that("seo and canonical with general acceptance keep the slashes", {
  for (prof in c("seo", "canonical")) {
    r <- safe_parse_urls("foo://h/a//b/./c", profile = prof,
                         scheme_acceptance = "general",
                         path_normalization = "both")
    expect_identical(r$path, "/a//b/c", label = prof)
  }
})

test_that("a mixed call collapses by route, row by row", {
  r <- safe_parse_urls(c("foo://h/a//b/./c", "http://h.com/a//b/./c"),
                       profile = "rfc-syntax", path_normalization = "both")
  expect_identical(r$path, c("/a//b/c", "/a/b/c"))
  expect_identical(r$clean_url, c("foo://h/a//b/c", "http://h.com/a/b/c"))
})
