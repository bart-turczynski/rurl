# Curated WHATWG/WPT-derived encoding fixture suite (RURL-zzihiumo).
#
# A small, inspectable set of path/query/userinfo cases with EXPLICIT expected
# outputs (unlike the whole-corpus characterization snapshot, which pins
# whatever the code currently emits). It documents rurl's intended contract for
# percent-encoding and path structure and pins it against engine drift.
#
# rurl's default profile is an RFC 3986 parser, NOT a WHATWG URL parser. Cases
# drawn from
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
  # Lowercase hex in the query is uppercased too (section 6.2.2.1).
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

test_that("userinfo percent-triplets are never decoded or double-encoded", {
  expect_identical(get_user("http://u%40ser:p%40ss@ex.com/x"), "u%40ser")
  expect_identical(get_password("http://u%40ser:p%40ss@ex.com/x"), "p%40ss")
  expect_identical(get_user("http://user:pass@ex.com/x"), "user")
  expect_identical(get_password("http://user:pass@ex.com/x"), "pass")
  # Idempotency holds under `whatwg` too, where the userinfo percent-encode set
  # IS applied: the encoder re-emits an existing triplet verbatim rather than
  # re-encoding its "%" (so no `%2540` / `%2525`).
  res <- safe_parse_urls(
    c("http://u%40ser:p%40ss@ex.com/x",
      "http://%25DOMAIN:foobar@foodomain.com/"),
    url_standard = "whatwg"
  )
  expect_identical(res$user, c("u%40ser", "%25DOMAIN"))
  expect_identical(res$password, c("p%40ss", "foobar"))
})

test_that("whatwg applies the userinfo percent-encode set to user/password", {
  chars <- c("^", "\"", "<", ">", "|", "{", "}", ";")
  urls <- paste0("http://a", chars, "b@host/")
  res <- safe_parse_urls(urls, url_standard = "whatwg")
  expect_identical(
    res$user,
    c("a%5Eb", "a%22b", "a%3Cb", "a%3Eb", "a%7Cb", "a%7Bb", "a%7Db", "a%3Bb")
  )
  # A ":" inside the password is a member of the set.
  pw <- safe_parse_urls("http://u:p:q@ex.com/", url_standard = "whatwg")
  expect_identical(pw$user, "u")
  expect_identical(pw$password, "p%3Aq")
  # The set also covers every non-ASCII byte (C0-control-set inheritance), so a
  # non-ASCII userinfo gains UTF-8 escapes.
  na <- safe_parse_urls("http://\u00e9x@host/", url_standard = "whatwg")
  expect_identical(na$user, "%C3%A9x")
  # Interaction with the userinfo charset acceptance shim: a SPACE arrives here
  # already pre-encoded as %20 and must NOT be double-encoded to %2520.
  sp <- safe_parse_urls("http://a b@host/", url_standard = "whatwg")
  expect_identical(sp$user, "a%20b")
})

test_that("rfc3986 and no selector keep the raw userinfo spelling", {
  rfc <- safe_parse_urls(
    c("http://u:p:q@ex.com/", "http://a;b@ex.com/", "file://u;v@h/p"),
    url_standard = "rfc3986"
  )
  expect_identical(rfc$user, c("u", "a;b", "u;v"))
  expect_identical(rfc$password, c("p:q", NA_character_, NA_character_))
  nul <- safe_parse_urls(c("http://u:p:q@ex.com/", "http://a^b@host/"))
  expect_identical(nul$user, c("u", "a^b"))
  expect_identical(nul$password, c("p:q", NA_character_))
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

# Input-side locale invariance (RURL-pqrutnio). The host chokepoint declares the
# parsed host UTF-8, but non-host values reach locale-sensitive operations by
# other routes. Each case below diverged under `LC_ALL=C` before the fix; the
# `Encoding<-` calls reproduce, IN ANY session, the "unknown"/native mark those
# values carry in a non-UTF-8 session.
test_that("path percent-encoding reads UTF-8 octets in any locale", {
  # `.whatwg_component_percent_encode()` used `enc2utf8()`, which re-decodes
  # native-marked bytes in the session locale: under LC_ALL=C `/école` came out
  # as `/%3Cc3%3E%3Ca9%3Ecole`.
  p <- "/école"
  Encoding(p) <- "unknown"
  expect_identical(rurl:::.whatwg_path_percent_encode(p), "/%C3%A9cole")

  u <- "http://ex.com/école"
  Encoding(u) <- "unknown"
  expect_identical(
    get_path(u, url_standard = "whatwg", path_encoding = "encode"),
    "/%C3%A9cole"
  )
})

test_that("a percent-decoded file: host is read as UTF-8 in any locale", {
  # `utils::URLdecode()` hands back native-marked octets, so under LC_ALL=C the
  # soft hyphen in `a%C2%ADb` missed its UTS-46 mapping and the row was
  # rejected. Percent-encoded and literal spellings must agree.
  expect_identical(
    get_host("file://a%C2%ADb/p", url_standard = "whatwg"),
    get_host("file://a\u00adb/p", url_standard = "whatwg")
  )
  expect_identical(get_host("file://a%C2%ADb/p", url_standard = "whatwg"), "ab")
  expect_identical(
    get_parse_status("file://a%C2%ADb/p", url_standard = "whatwg"), "ok"
  )
})

test_that("a host that percent-decodes to invalid UTF-8 is rejected", {
  # The historical engine decoded the host unconditionally, and its R binding
  # then threw in a UTF-8 session but returned raw bytes
  # under LC_ALL=C -- where those bytes went on to break `pslr`'s regex ops.
  for (std in list(NULL, "rfc3986", "whatwg")) {
    expect_identical(
      get_parse_status("http://example.com%80/", url_standard = std), "error"
    )
    expect_true(
      is.na(get_host("ftp://example.com%80/", url_standard = std))
    )
  }
  # Only the host is affected: an invalid-UTF-8 octet in the path is kept.
  expect_identical(get_path("http://ex.com/%80"), "/%80")
})
