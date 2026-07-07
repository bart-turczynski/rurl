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

test_that("path_encoding picks readable vs browser path form (ada-003/022)", {
  # The presentation choice the WHATWG-vs-readable divergence (ada-003 /ecole,
  # ada-022 /"quoted") comes down to. é is U+00E9; its UTF-8 percent-encoding is
  # %C3%A9. The default "keep" is a faithful passthrough -- it forces NEITHER
  # readable nor encoded; "encode" renders the browser/percent-encoded form;
  # "decode" renders the readable form. (Source is UTF-8; see DESCRIPTION.)
  readable <- "http://ex.com/école"
  encoded <- "http://ex.com/%C3%A9cole"

  # A readable (non-ASCII) path: keep leaves it readable; encode -> browser.
  expect_identical(get_clean_url(readable, path_encoding = "keep"), readable)
  expect_identical(get_clean_url(readable, path_encoding = "encode"), encoded)

  # A percent-encoded path: keep preserves the bytes; decode -> readable;
  # encode -> the normalized browser form.
  expect_identical(get_clean_url(encoded, path_encoding = "keep"), encoded)
  expect_identical(get_clean_url(encoded, path_encoding = "decode"), readable)
  expect_identical(get_clean_url(encoded, path_encoding = "encode"), encoded)

  # ada-022: ASCII bytes outside the path-safe set (double quotes).
  expect_identical(
    get_clean_url("http://ex.com/\"quoted\"", path_encoding = "keep"),
    "http://ex.com/\"quoted\""
  )
  expect_identical(
    get_clean_url("http://ex.com/\"quoted\"", path_encoding = "encode"),
    "http://ex.com/%22quoted%22"
  )
})

test_that("url_standard does not force path percent-encoding by default", {
  # A profile governs path IDENTITY (dot/percent handling) but must NOT silently
  # switch the path to the browser/percent-encoded rendering: the readable
  # default survives. (Combining an explicit path_encoding WITH a profile is a
  # separate, larger change -- the profile currently governs path_encoding.)
  readable <- "http://ex.com/école"
  expect_identical(get_clean_url(readable, url_standard = "whatwg"), readable)
  expect_identical(get_clean_url(readable, url_standard = "rfc3986"), readable)
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
