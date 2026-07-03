# Curated WHATWG/WPT-derived encoding fixture suite (RURL-zzihiumo).
#
# A small, inspectable set of path/query/userinfo cases with EXPLICIT expected
# outputs (unlike the whole-corpus characterization snapshot, which pins
# whatever the code currently emits). It documents rurl's intended contract for
# percent-encoding and path structure and pins it against libcurl drift.
#
# rurl is an RFC 3986 / curl parser, NOT a WHATWG URL parser. Cases drawn from
# the WHATWG URL test data where rurl *intentionally* diverges (encoded-dot
# resolution, backslash-as-separator) are kept and annotated, so a future
# accidental behavior change is caught rather than silently "conforming".

test_that("path/encoding fixtures match explicit expectations", {
  path <- testthat::test_path("fixtures", "encoding-fixtures.csv")
  fx <- utils::read.csv(
    path, stringsAsFactors = FALSE, colClasses = "character"
  )

  for (i in seq_len(nrow(fx))) {
    actual <- get_clean_url(
      fx$input[i],
      path_encoding = fx$path_encoding[i],
      path_normalization = fx$path_normalization[i]
    )
    expect_identical(
      actual, fx$expected[i],
      label = sprintf(
        "[%s] %s (path_encoding=%s, path_normalization=%s): %s",
        fx$id[i], fx$input[i], fx$path_encoding[i],
        fx$path_normalization[i], fx$note[i]
      )
    )
  }
})

test_that("raw query fidelity honors the decode flag", {
  # get_query returns the raw query string; decode=FALSE keeps percent-encoding
  # (hex uppercased), decode=TRUE percent-decodes.
  expect_identical(
    get_query("http://ex.com/p?x=a%2Fb&y=1", decode = FALSE), "x=a%2Fb&y=1"
  )
  expect_identical(
    get_query("http://ex.com/p?x=a%2Fb&y=1", decode = TRUE), "x=a/b&y=1"
  )
  # Lowercase hex in the query is uppercased too (libcurl, section 6.2.2.1).
  expect_identical(
    get_query("http://ex.com/p?x=a%2fb", decode = FALSE), "x=a%2Fb"
  )
  # A bare key keeps no fabricated "=" (raw pass-through).
  expect_identical(get_query("http://ex.com/p?flag", decode = FALSE), "flag")
  # Repeated keys are preserved verbatim.
  expect_identical(
    get_query("http://ex.com/p?a=1&a=2", decode = FALSE), "a=1&a=2"
  )
})

test_that("userinfo encoding is preserved verbatim", {
  expect_identical(get_user("http://u%40ser:p%40ss@ex.com/x"), "u%40ser")
  expect_identical(get_password("http://u%40ser:p%40ss@ex.com/x"), "p%40ss")
  expect_identical(get_user("http://user:pass@ex.com/x"), "user")
  expect_identical(get_password("http://user:pass@ex.com/x"), "pass")
})

test_that("canonical_join equivalence follows the encoding contract", {
  eq <- function(a, b, ...) {
    identical(get_clean_url(a, ...), get_clean_url(b, ...))
  }
  # %2f and %2F are RFC 3986-equivalent -> same join key.
  expect_true(eq("http://ex.com/a%2fb", "http://ex.com/a%2Fb"))
  # %2F and its double-encoding are distinct -> different keys.
  expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a%252Fb"))
  # dot segments are distinct under the default, equal once resolved.
  expect_false(eq("http://ex.com/a/../b", "http://ex.com/b"))
  expect_true(
    eq("http://ex.com/a/../b", "http://ex.com/b",
       path_normalization = "dot_segments")
  )
  # An encoded dot never equals a literal one (rurl treats %2e as a byte).
  expect_false(
    eq("http://ex.com/a/%2e%2e/b", "http://ex.com/a/../b",
       path_normalization = "dot_segments")
  )
})
