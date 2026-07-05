# Tests for resolve_url() (RURL-wrfcildb, epic RURL-ehlircjt / parent
# RURL-uyjheruh; PRD v2 §5.6 D6). resolve_url() composes: standard-agnostic
# RFC 3986 §5.2.2 base-merge, then the same safe_parse_urls() machinery for
# host/path/port/query normalization and diagnostics. It adds NO per-standard
# behavior of its own. The return is the CANONICAL clean_url of the resolved
# reference (fragment/userinfo dropped, query per query_handling, port per
# port_handling), which is why several RFC §5.4 expectations below differ from
# a verbatim resolver on the query/fragment components only.

base <- "http://a/b/c/d;p?q"

# --- RFC 3986 §5.4.1 normal examples (path merge is the observable part) -----

test_that("RFC 3986 §5.4.1 normal examples resolve their path/authority", {
  # query/fragment are dropped by clean_url canonicalization, so we assert the
  # scheme+host+path portion the merge is responsible for.
  expect_identical(resolve_url("g", base), "http://a/b/c/g")
  expect_identical(resolve_url("./g", base), "http://a/b/c/g")
  expect_identical(resolve_url("g/", base), "http://a/b/c/g/")
  expect_identical(resolve_url("/g", base), "http://a/g")
  expect_identical(resolve_url("//g", base), "http://g/")
  expect_identical(resolve_url("g?y", base), "http://a/b/c/g")
  expect_identical(resolve_url("g#s", base), "http://a/b/c/g")
  expect_identical(resolve_url(".", base), "http://a/b/c/")
  expect_identical(resolve_url("./", base), "http://a/b/c/")
  expect_identical(resolve_url("..", base), "http://a/b/")
  expect_identical(resolve_url("../", base), "http://a/b/")
  expect_identical(resolve_url("../g", base), "http://a/b/g")
  expect_identical(resolve_url("../..", base), "http://a/")
  expect_identical(resolve_url("../../g", base), "http://a/g")
})

test_that("empty / fragment-only / query-only references resolve the base", {
  # Empty ref, fragment-only ref, and query-only ref all reduce to the base's
  # resource once the (dropped-by-default) fragment/query are set aside.
  expect_identical(resolve_url("", base), "http://a/b/c/d;p")
  expect_identical(resolve_url("#s", base), "http://a/b/c/d;p")
  expect_identical(resolve_url("?y", base), "http://a/b/c/d;p")
})

test_that("RFC 3986 §5.4.2 abnormal examples clamp excess ..", {
  expect_identical(resolve_url("../../../g", base), "http://a/g")
  expect_identical(resolve_url("../../../../g", base), "http://a/g")
  expect_identical(resolve_url("/./g", base), "http://a/g")
  expect_identical(resolve_url("/../g", base), "http://a/g")
  # A trailing dot segment resolves to the containing directory.
  expect_identical(resolve_url("g.", base), "http://a/b/c/g.")
  expect_identical(resolve_url(".g", base), "http://a/b/c/.g")
  expect_identical(resolve_url("g..", base), "http://a/b/c/g..")
  expect_identical(resolve_url("./../g", base), "http://a/b/g")
  expect_identical(resolve_url("./g/.", base), "http://a/b/c/g/")
  expect_identical(resolve_url("g/./h", base), "http://a/b/c/g/h")
  expect_identical(resolve_url("g/../h", base), "http://a/b/c/h")
})

# --- Absolute references ignore the base -------------------------------------

test_that("an absolute reference ignores the base entirely", {
  expect_identical(
    resolve_url("https://x.com/y/z", base), "https://x.com/y/z"
  )
  # Base may even be NA when the reference is absolute.
  expect_identical(
    resolve_url("http://x.com/y", NA_character_), "http://x.com/y"
  )
  # Dot segments in an absolute reference are still resolved.
  expect_identical(
    resolve_url("http://x.com/a/b/../c", "http://a/b"), "http://x.com/a/c"
  )
})

test_that("an unsupported-scheme absolute ref yields NA (rurl allowlist)", {
  # "g:h" resolves (per RFC) to the opaque "g:h", but rurl only canonicalizes
  # http/https/ftp/ftps, so clean_url is NA -- same as parsing "g:h" directly.
  expect_true(is.na(resolve_url("g:h", base)))
  expect_identical(resolve_url("g:h", base), get_clean_url("g:h"))
})

# --- Base must be absolute for a relative reference --------------------------

test_that("a relative reference needs an absolute base", {
  expect_true(is.na(resolve_url("g", NA_character_)))
  expect_true(is.na(resolve_url("g", "not-a-url/path")))
  expect_true(is.na(resolve_url("../x", "//host/only")))
})

# --- Query / fragment are canonicalized like clean_url elsewhere -------------

test_that("query follows query_handling; merge is genuinely computed", {
  # With query_handling = "keep" the resolved query becomes observable, proving
  # the query-only and query-bearing merges compute the right query.
  expect_identical(
    resolve_url("?y", base, query_handling = "keep"), "http://a/b/c/d;p?y="
  )
  expect_identical(
    resolve_url("g?y", base, query_handling = "keep"), "http://a/b/c/g?y="
  )
  # An empty reference inherits the base query.
  expect_identical(
    resolve_url("", base, query_handling = "keep"), "http://a/b/c/d;p?q="
  )
  # A reference with its own query does NOT inherit the base query.
  expect_identical(
    resolve_url("g?a=1", "http://a/b/c?q", query_handling = "keep"),
    "http://a/b/g?a=1"
  )
})

# --- url_standard pass-through: no divergent behavior of its own -------------

test_that("port default-port elision flows from url_standard", {
  expect_identical(
    resolve_url("/p", "http://a:80/b", port_handling = "keep",
                url_standard = "whatwg"),
    "http://a/p"
  )
  expect_identical(
    resolve_url("/p", "http://a:80/b", port_handling = "keep",
                url_standard = "rfc3986"),
    "http://a:80/p"
  )
  expect_identical(
    resolve_url("/p", "http://a:8080/b", port_handling = "keep",
                url_standard = "whatwg"),
    "http://a:8080/p"
  )
})

test_that("WHATWG backslash-as-slash flows from url_standard", {
  # Backslash recognition is applied by the downstream parser on the resolved
  # absolute URL, governed by url_standard exactly as in safe_parse_url.
  expect_identical(
    resolve_url("g\\h", "http://a/b/c/", url_standard = "whatwg"),
    "http://a/b/c/g/h"
  )
  expect_identical(
    resolve_url("g\\h", "http://a/b/c/", url_standard = "rfc3986"),
    "http://a/b/c/g\\h"
  )
})

test_that("resolved output equals a direct parse of the resolved URL", {
  # The composition contract: resolving then reading clean_url is identical to
  # parsing the RFC-recomposed URL directly, for every governed axis.
  expect_identical(
    resolve_url("../g?x=1", base, query_handling = "keep",
                url_standard = "rfc3986"),
    get_clean_url("http://a/b/g?x=1", query_handling = "keep",
                  url_standard = "rfc3986")
  )
})

# --- url_standard conflict check across the `...` seam -----------------------

test_that("a governed knob conflicting with url_standard errors", {
  expect_error(
    resolve_url("g", base, url_standard = "whatwg", case_handling = "upper"),
    "governs `case_handling`"
  )
  expect_error(
    resolve_url("g", base, url_standard = "rfc3986",
                path_normalization = "collapse_slashes"),
    "governs `path_normalization`"
  )
  # The value the profile would pick is accepted.
  expect_identical(
    resolve_url("g", base, url_standard = "rfc3986",
                path_normalization = "dot_segments"),
    "http://a/b/c/g"
  )
})

# --- Vectorization, recycling, and NA handling -------------------------------

test_that("resolve_url() is vectorized and recycles the base", {
  expect_identical(
    resolve_url(c("g", "../h", "/i", "//j/k"), "http://a/b/c/"),
    c("http://a/b/c/g", "http://a/b/h", "http://a/i", "http://j/k")
  )
  # A vector of bases, recycled against a scalar reference.
  expect_identical(
    resolve_url("g", c("http://a/b/", "http://x/y/")),
    c("http://a/b/g", "http://x/y/g")
  )
})

test_that("NA and empty inputs propagate to NA / empty output", {
  expect_identical(
    resolve_url(c("g", NA_character_), "http://a/b/c/"),
    c("http://a/b/c/g", NA_character_)
  )
  expect_identical(resolve_url(character(0), "http://a/b/"), character(0))
})

# --- IDN / host model pass-through -------------------------------------------

test_that("host encoding and IDN handling flow through to the resolved host", {
  # host_encoding governs the resolved host spelling as in safe_parse_url.
  expect_identical(
    resolve_url("/p", "http://münchen.de/x", host_encoding = "idna"),
    get_clean_url("http://münchen.de/p", host_encoding = "idna")
  )
  expect_identical(
    resolve_url("/p", "http://münchen.de/x", host_encoding = "unicode"),
    get_clean_url("http://münchen.de/p", host_encoding = "unicode")
  )
})
