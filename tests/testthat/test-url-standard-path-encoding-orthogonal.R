# path_encoding is an orthogonal presentation axis that LAYERS on any
# url_standard profile (ADR 0011 / RURL-sjnqhwtl). It is no longer governed by
# the selector -- the profile sets the internal path IDENTITY mode, and the
# public keep/encode/decode presentation applies on top, mirroring how
# host_encoding selects a spelling independent of the selector.

test_that("path_encoding = 'keep' is the profile's canonical identity", {
  # keep (the default) must reproduce today's profile output byte-for-byte:
  # whatwg never decodes, rfc decodes unreserved only.
  expect_identical(
    get_path("http://ex.com/%41%42", url_standard = "whatwg",
      path_encoding = "keep"),
    get_path("http://ex.com/%41%42", url_standard = "whatwg")
  )
  expect_identical(
    get_path("http://ex.com/%41%42", url_standard = "whatwg"), "/%41%42"
  )
  expect_identical(
    get_path("http://ex.com/%7euser", url_standard = "whatwg"), "/%7euser"
  )
  expect_identical(
    get_path("http://ex.com/%4a%6A", url_standard = "whatwg"), "/%4a%6A"
  )
  expect_identical(
    get_path("http://ex.com/%41%42", url_standard = "rfc3986"), "/AB"
  )
})

test_that("path_encoding = 'encode' renders the browser form under a profile", {
  # Retires benchmark caveat #3: a readable non-ASCII path emits its
  # percent-encoded UTF-8 form under either profile.
  for (std in c("rfc3986", "whatwg")) {
    expect_identical(
      get_path("https://ex.com/école", url_standard = std,
        path_encoding = "encode"),
      "/%C3%A9cole",
      info = std
    )
  }
})

test_that("whatwg path_encoding = 'encode' uses the WHATWG path encode set", {
  # Inputs are VALUES, not names: R translates a non-representable NAME to its
  # `<U+00E9>` escape when it parses this file under a non-UTF-8 LC_CTYPE (the
  # UTF-8 mark survives on a string value but not on a name), so a named-vector
  # table silently handed rurl a different, ASCII-mangled input under
  # `LC_ALL=C`. Same cases, same expectations, locale-invariant transport.
  inputs <- c(
    "http://ex.com/w|m",
    "http://ex.com/@asdf%40",
    "http://ex.com/jqueryui@1.2.3",
    "http://ex.com/foo%",
    "http://ex.com/foo%2",
    "http://ex.com/foo%2zbar",
    "http://ex.com/foo%41%7a",
    "http://ex.com/foo%2Ehtml",
    "http://ex.com/%3a",
    "http://ex.com/\"quoted\"",
    "http://ex.com/école"
  )
  expected <- c(
    "/w|m",
    "/@asdf%40",
    "/jqueryui@1.2.3",
    "/foo%",
    "/foo%2",
    "/foo%2zbar",
    "/foo%41%7a",
    "/foo%2Ehtml",
    "/%3a",
    "/%22quoted%22",
    "/%C3%A9cole"
  )
  for (i in seq_along(inputs)) {
    expect_identical(
      get_path(inputs[i], url_standard = "whatwg", path_encoding = "encode"),
      expected[i],
      info = inputs[i]
    )
  }
  expect_identical(
    rurl:::.whatwg_path_percent_encode("\"#<>?^`{} é"),
    "%22%23%3C%3E%3F%5E%60%7B%7D%20%C3%A9"
  )
  # `^` (U+005E) is a member of the path set (RURL-qxpgcwie): WPT pins
  # `foo://host/...^...` -> `%5E`. It was the one missing member, so every other
  # character of that row already matched.
  expect_identical(
    get_path("http://ex.com/a^b", url_standard = "whatwg",
             path_encoding = "encode"),
    "/a%5Eb"
  )
})

test_that("whatwg profile serializes query and fragment encode sets", {
  parsed <- safe_parse_urls(
    c(
      "http://host/?'",
      "http://example.org/test?\"",
      "http://example.org/test?<",
      "http://example.org/test?>",
      "http://foo.bar/baz?qux#foo\"bar",
      "http://foo.bar/baz?qux#foo<bar",
      "http://foo.bar/baz?qux#foo>bar",
      "http://foo.bar/baz?qux#foo`bar",
      "https://localhost?q=🔥#🔥"
    ),
    url_standard = "whatwg",
    scheme_policy = "require",
    host_encoding = "idna",
    path_encoding = "encode"
  )

  expect_identical(
    parsed$query,
    c("%27", "%22", "%3C", "%3E", "qux", "qux", "qux", "qux",
      "q=%F0%9F%94%A5")
  )
  expect_identical(
    parsed$fragment,
    c(NA_character_, NA_character_, NA_character_, NA_character_,
      "foo%22bar", "foo%3Cbar", "foo%3Ebar", "foo%60bar",
      "%F0%9F%94%A5")
  )
  expect_identical(
    rurl:::.whatwg_query_percent_encode("\"#<>' é", "http"),
    "%22%23%3C%3E%27%20%C3%A9"
  )
  expect_identical(
    rurl:::.whatwg_fragment_percent_encode("\"<>` é"),
    "%22%3C%3E%60%20%C3%A9"
  )
})

test_that("whatwg accepts WPT-valid path query fragment bytes web rejects", {
  urls <- c(
    paste0("http://www.google.com/foo?bar=baz# ", intToUtf8(0x00BB)),
    paste0("http://foo.bar/baz?qux#foo", intToUtf8(0x08), "bar"),
    paste0(
      "https://www.example.com/path{", intToUtf8(0x7F),
      "path.html?query'", intToUtf8(0x7F),
      "=query#fragment<", intToUtf8(0x7F), "fragment"
    )
  )
  parsed <- safe_parse_urls(
    urls,
    url_standard = "whatwg",
    scheme_policy = "require",
    host_encoding = "idna",
    path_encoding = "encode",
    query_handling = "keep",
    port_handling = "strip_default"
  )

  expect_identical(parsed$parse_status, c("ok", "ok", "ok"))
  expect_identical(
    parsed$path,
    c("/foo", "/baz", "/path%7B%7Fpath.html")
  )
  expect_identical(
    parsed$query,
    c("bar=baz", "qux", "query%27%7F=query")
  )
  expect_identical(
    parsed$fragment,
    c("%20%C2%BB", "foo%08bar", "fragment%3C%7Ffragment")
  )
  expect_identical(
    parsed$clean_url,
    c(
      "http://www.google.com/foo?bar=baz",
      "http://foo.bar/baz?qux=",
      "https://www.example.com/path%7B%7Fpath.html?query%27%7F=query"
    )
  )
})

test_that("path_encoding = 'decode' renders the readable form on a profile", {
  for (std in c("rfc3986", "whatwg")) {
    expect_identical(
      get_path("http://ex.com/%C3%A9cole", url_standard = std,
        path_encoding = "decode"),
      "/école",
      info = std
    )
  }
})

test_that("path presentation runs after index and trailing-slash cleaning", {
  index_input <- "https://ex.com/a%2Findex.html"
  slash_input <- "https://ex.com/a%2F"

  # Each cleaning dial alone correctly treats %2F as data, not a separator.
  expect_identical(
    get_clean_url(index_input, index_page_handling = "strip"),
    index_input
  )
  expect_identical(
    get_clean_url(slash_input, trailing_slash_handling = "strip"),
    slash_input
  )

  for (encoding in c("decode", "encode")) {
    # Presentation alone may render the encoded slash as "/". Combining it
    # with cleaning must not retroactively expose a separator to that earlier
    # cleaning step and change which index/slash rule fires.
    expect_identical(
      get_clean_url(index_input, path_encoding = encoding),
      "https://ex.com/a/index.html",
      info = encoding
    )
    expect_identical(
      get_clean_url(
        index_input,
        index_page_handling = "strip",
        path_encoding = encoding
      ),
      "https://ex.com/a/index.html",
      info = encoding
    )
    expect_identical(
      get_clean_url(slash_input, path_encoding = encoding),
      "https://ex.com/a/",
      info = encoding
    )
    expect_identical(
      get_clean_url(
        slash_input,
        trailing_slash_handling = "strip",
        path_encoding = encoding
      ),
      "https://ex.com/a/",
      info = encoding
    )
  }
})

test_that("encode/decode are presentation not identity: reserved octets fold", {
  # Documented consequence (ADR 0011): the presentation forms may re-encode or
  # decode reserved octets, so a profile's %2F identity does NOT survive them.
  # keep is the value that preserves it.
  expect_identical(
    get_path("http://ex.com/a%2Fb/c", url_standard = "whatwg"), "/a%2Fb/c"
  )
  expect_identical(
    get_path("http://ex.com/a%2Fb/c", url_standard = "whatwg",
      path_encoding = "decode"),
    "/a/b/c"
  )
})

test_that("path_encoding composes with host_encoding under a profile", {
  # Both presentation axes are orthogonal to the selector and to each other.
  res <- safe_parse_url("https://xn--mnchen-3ya.de/école",
    url_standard = "whatwg", host_encoding = "unicode",
    path_encoding = "encode")
  expect_identical(res$host, "münchen.de")
  expect_identical(res$path, "/%C3%A9cole")
})

test_that("canonical_join() path_encoding is LEGACY: it warns, it re-keys", {
  A <- data.frame(URL = "https://ex.com/école", ValA = 1L,
    stringsAsFactors = FALSE)
  B <- data.frame(URL = "https://ex.com/%C3%A9cole", ValB = 2L,
    stringsAsFactors = FALSE)
  # P3.1 D-E / RURL-nfpjtxpq. This pins LEGACY behavior, NOT a feature:
  # canonical_join() keys on `clean_url`, so `path_encoding` -- a presentation
  # dial that takes no part in URL identity -- changes WHICH ROWS MATCH. D-E
  # types that as compatibility-only, explicitly not the v3 identity model,
  # retained for a deprecation window and required to warn. Both halves of
  # that legacy contract are pinned below: the warning, then the match set.
  # The identity half of the row lives in the next block; neither is complete
  # without the other, because the contract is a CONTRAST between them.
  expect_warning(
    canonical_join(A, B, url_standard = "whatwg", path_encoding = "encode"),
    class = "rurl_legacy_join_dial_warning"
  )
  # encode collapses both spellings of the path to the browser form, so the two
  # rows join on one legacy key. The warning is muted here because this half of
  # the test pins the (unchanged) VALUES, not the condition.
  joined <- cj_legacy(canonical_join(A, B, url_standard = "whatwg",
    path_encoding = "encode"))
  expect_identical(nrow(joined), 1L)
  expect_identical(joined$ValA, 1L)
  expect_identical(joined$ValB, 2L)
})

test_that("the identity family answers the same question invariantly", {
  # key-join-contracts.md :183, the identity half of the LEGACY block above,
  # asserted on the same two frames. That block pins that `path_encoding`
  # MOVES canonical_join()'s match set; this one pins that the identity
  # family's answer to the same question cannot be moved at all.
  #
  # Deliberately not a duplicate of two neighbouring pins: test-url-key-join-
  # api.R :: "no public cleaning or profile dial can reach the exported key"
  # covers get_url_key(), and test-url-join.R :: "the family has no
  # presentation dials to forward" covers the unexported ENGINE. The six
  # exported join wrappers are a third surface, and the match-set contrast
  # against the legacy frames exists nowhere.
  A <- data.frame(URL = "https://ex.com/école", ValA = 1L,
    stringsAsFactors = FALSE)
  B <- data.frame(URL = "https://ex.com/%C3%A9cole", ValB = 2L,
    stringsAsFactors = FALSE)

  # Identity gives a DIFFERENT answer, not a coincidentally equal one:
  # `path_encoding = "encode"` collapses both spellings onto one legacy key and
  # joins them (1 row, above), while identity holds `/école` and `/%C3%A9cole`
  # apart, so nothing matches. Worth stating because a test that happened to
  # agree with the legacy result here could pass while the invariance it claims
  # to check was broken.
  k <- get_url_key(c(A$URL, B$URL))
  expect_false(identical(k[[1]], k[[2]]))
  expect_identical(nrow(url_inner_join(A, B, by = "URL")), 0L)

  # And that 0 cannot be argued up to 1: no member of canonical_join()'s own
  # presentation-dial list is a formal of any of the six. Derived from
  # .CJ_LEGACY_PRESENTATION_DIALS rather than transcribed, so a dial added
  # there is covered here for free. `engine` is the single admitted member,
  # for the reason recorded in test-url-join.R :: "the family has no
  # presentation dials to forward".
  joins <- list(inner = url_inner_join, left = url_left_join,
    right = url_right_join, full = url_full_join,
    semi = url_semi_join, anti = url_anti_join)
  for (nm in names(joins)) {
    expect_identical(
      intersect(names(formals(joins[[nm]])), .CJ_LEGACY_PRESENTATION_DIALS),
      "engine",
      info = nm
    )
  }

  # Passing one anyway is refused rather than absorbed: none of the six take
  # `...`, so a legacy call ported over verbatim fails loudly instead of
  # quietly keeping its old match set. R translates "unused argument" under
  # some locales, so the condition is asserted and its text is not.
  expect_error(url_inner_join(A, B, by = "URL", path_encoding = "encode"))
})

test_that("path_encoding = 'keep' does not depend on a sibling component", {
  # RURL-ezhzpkhg deletion 5. Until the pqf fallback was removed, "keep" was
  # not keeping: a failed first parse triggered a re-parse of the input
  # respelled with the FULL WHATWG encode sets, so the stored path picked up
  # escapes it had no reason to carry. Because the retry was gated on FAILURE
  # and rewrote all three components at once, whether the "<" in this PATH was
  # kept or escaped was decided by what the QUERY happened to hold.
  #
  # Same path, two queries. "keep" must give the same answer to both.
  expect_identical(
    get_path("http://h.com/a<b?k= v", url_standard = "whatwg",
      path_encoding = "keep"),
    get_path("http://h.com/a<b?k=<v", url_standard = "whatwg",
      path_encoding = "keep")
  )
  expect_identical(
    get_path("http://h.com/a<b?k= v", url_standard = "whatwg",
      path_encoding = "keep"),
    "/a<b"
  )
  # The other two settings were already conjunction-free and must not move.
  expect_identical(
    get_path("http://h.com/a<b?k= v", url_standard = "whatwg",
      path_encoding = "encode"),
    "/a%3Cb"
  )
  expect_identical(
    get_path("http://h.com/a<b?k= v", url_standard = "whatwg",
      path_encoding = "decode"),
    "/a<b"
  )
  # The SPACE is a different case and must still be escaped under "keep": a
  # C0/SP/DEL byte parses at all only because WHATWG escapes it, so keeping it
  # raw would keep something the parser never accepted -- and would put a
  # literal space in `clean_url()`.
  expect_identical(
    get_path("http://h.com/a b?k=<v", url_standard = "whatwg",
      path_encoding = "keep"),
    "/a%20b"
  )
  # The serializer is unmoved throughout: rendering the full encode set is its
  # job, and it did it before and after.
  expect_identical(
    serialize_url("http://h.com/a<b?k= v", standard = "whatwg"),
    "http://h.com/a%3Cb?k=%20v"
  )
})

test_that("VT and FF are escaped like any other C0 control under whatwg", {
  # These two were rejected under `whatwg` for a reason that had nothing to do
  # with URLs: the deleted fallback matched the post-authority remainder with
  # an ICU ".", which excludes the Unicode line terminators, so the rewrite it
  # was gated on never fired. WHATWG strips only tab/LF/CR; VT and FF are
  # ordinary C0 controls and percent-encode.
  for (ch in c("\u000b", "\u000c")) {
    u <- paste0("http://h.com/a", ch, "b")
    hexpair <- if (identical(ch, "\u000b")) "%0B" else "%0C"
    expect_identical(
      get_parse_status(u, url_standard = "whatwg"), "ok",
      info = hexpair
    )
    expect_identical(
      get_path(u, url_standard = "whatwg", path_encoding = "keep"),
      paste0("/a", hexpair, "b"),
      info = hexpair
    )
    # `rfc3986` and the no-selector baseline still reject: the dial is scoped
    # to the WHATWG profile, which is the only one with an escape-it rule.
    expect_identical(
      get_parse_status(u, url_standard = "rfc3986"), "error",
      info = hexpair
    )
    expect_identical(get_parse_status(u), "error", info = hexpair)
  }
})
