# The PUBLIC full-string standard serializer, `serialize_url()` -- output
# surface (b), cell SURF-b.
#
# tests/testthat/test-serialize-fsss.R covers the two serializer INTERNALS
# against hand-built records. This file covers what only the export can be
# wrong about: that the record handed to them is built from the parse state
# under the standard's own posture, that no presentation dial can reach the
# surface, and that (b) and (c) are visibly different products.

# --- SURF-b: the export exists and is a standard serializer ------------------

test_that("serialize_url() emits the full string, credentials and fragment", {
  expect_identical(
    serialize_url("http://user:pw@Example.COM:80/a/../b?q=1#frag"),
    "http://user:pw@example.com/b?q=1#frag"
  )
})

test_that("serialize_url() is not clean_url", {
  # C-04. The same input on the two surfaces, side by side: (c) drops the
  # userinfo and the fragment by contract and keeps the Unicode host spelling;
  # (b) keeps the credentials and fragment and emits the standard's ASCII host.
  # If these two ever agree on this input, one of the surfaces has drifted.
  input <- "http://user:pw@Example.COM:80/a/../b?q=1#frag"
  expect_false(identical(serialize_url(input), get_clean_url(input)))
  expect_identical(
    serialize_url("http://ПРИВЕТ.рф/A"),
    "http://xn--b1agh1afp.xn--p1ai/A"
  )
})

test_that("a present-but-empty query or fragment delimiter survives", {
  # ADR 0012 D2, through the PUBLIC entry point. Stage A collapses "" to NA, so
  # this is the property most likely to be lost in the record build rather than
  # in the serializer -- which is exactly why it is asserted here too.
  expect_identical(
    serialize_url(c(
      "http://example.com/", "http://example.com/#",
      "http://example.com/?", "http://example.com/?#"
    )),
    c(
      "http://example.com/", "http://example.com/#",
      "http://example.com/?", "http://example.com/?#"
    )
  )
})

# --- FSSS-7 / CAP-2: credential postures differ by standard, correctly -------

test_that("credential serialization is spec-exact per standard", {
  input <- c("http://@h/p", "http://u:@h/p", "http://:p@h/p", "http://u:p@h/p")
  # WHATWG appends credentials only when a half is non-empty (WPT-pinned).
  expect_identical(
    serialize_url(input),
    c("http://h/p", "http://u@h/p", "http://:p@h/p", "http://u:p@h/p")
  )
  # RFC 3986 has no username/password split, so every spelling survives.
  expect_identical(serialize_url(input, standard = "rfc3986"), input)
})

# --- OUT-O3: both RFC postures are reachable ---------------------------------

test_that("the RFC source form normalizes nothing", {
  input <- "HTTP://Example.COM:80/a/%7Euser/../x"
  expect_identical(
    serialize_url(input, standard = "rfc3986", form = "source"),
    "http://Example.COM:80/a/%7Euser/../x"
  )
})

test_that("the RFC normalized form applies 6.2.2 and 6.2.3", {
  # Case, percent-triplet decoding of an unreserved octet, dot-segment removal
  # and default-port elision, all in one row.
  expect_identical(
    serialize_url("HTTP://Example.COM:80/a/%7Euser/../x",
                  standard = "rfc3986", form = "normalized"),
    "http://example.com/a/x"
  )
})

test_that("form is ignored under whatwg", {
  input <- "HTTP://Example.COM:80/a/../b"
  expect_identical(
    serialize_url(input, form = "source"),
    serialize_url(input, form = "normalized")
  )
})

# --- C-05 / INV-2: no presentation dial can reach surface (b) ----------------

test_that("serialize_url() takes no presentation argument", {
  # The guard that makes C-05 non-regressible at the PUBLIC boundary: if a
  # cleaning or presentation knob is ever added to this signature, this fails.
  # `standard` and `form` select which standard's serialization is wanted;
  # `engine` is PSL resolution, not presentation.
  expect_named(formals(serialize_url), c("url", "standard", "form", "engine"))
})

test_that("a default port is elided or kept per the standard, not per a dial", {
  expect_identical(
    serialize_url(c("https://h:443/", "http://h:443/", "http://h:8080/x")),
    c("https://h/", "http://h:443/", "http://h:8080/x")
  )
  # RFC source keeps it; only the normalized form elides it (6.2.3).
  expect_identical(
    serialize_url("https://h:443/", standard = "rfc3986"), "https://h:443/"
  )
  expect_identical(
    serialize_url("https://h:443/", standard = "rfc3986",
                  form = "normalized"),
    "https://h/"
  )
})

# --- parse posture: any scheme, but a scheme is required ---------------------

test_that("non-special schemes serialize on both standards", {
  expect_identical(
    serialize_url(c("mailto:a@b.com", "foo://h:80/x?#")),
    c("mailto:a@b.com", "foo://h:80/x?#")
  )
  expect_identical(
    serialize_url("foo://h:80/x?#", standard = "rfc3986"), "foo://h:80/x?#"
  )
})

test_that("scheme-less input is NA, not silently upgraded", {
  # Neither standard defines a base-URL-free parse of `example.com/x`. The
  # `https://` prepend is rurl's browser-like fix-up (surface c's business), and
  # it must not leak into a standard serialization.
  expect_identical(serialize_url("example.com/x"), NA_character_)
  expect_identical(
    serialize_url("example.com/x", standard = "rfc3986"), NA_character_
  )
})

# --- vectorization and input shapes -----------------------------------------

test_that("serialize_url() is vectorized and NA-preserving", {
  expect_identical(
    serialize_url(c(NA, "", "http://h/", "not a url")),
    c(NA_character_, NA_character_, "http://h/", NA_character_)
  )
  expect_identical(serialize_url(character(0)), character(0))
})

test_that("names are not data and factors parse as their labels", {
  expect_identical(
    serialize_url(c(a = "http://h/", b = "http://h/x")),
    c("http://h/", "http://h/x")
  )
  expect_identical(
    serialize_url(factor(c("http://h/", "http://h/x"))),
    c("http://h/", "http://h/x")
  )
})

test_that("invalid standard or form is rejected", {
  expect_error(serialize_url("http://h/", standard = "rfc1738"))
  expect_error(serialize_url("http://h/", standard = "rfc3986", form = "raw"))
  # match.arg() partial matching applies, as it does across the rest of the API.
  expect_identical(
    serialize_url("https://h:443/", standard = "rfc"), "https://h:443/"
  )
})

test_that("standard = NULL is refused, not absorbed as whatwg", {
  # RURL-ouorolhb. `match.arg(NULL, choices)` silently returns the first
  # choice, so before this guard `serialize_url("http://a/", standard = NULL)`
  # returned "http://a/" -- a WHATWG answer for a selector value that, on the
  # parse surface, names the frozen legacy profile and no standard (ADR 0007).
  # The message shape mirrors url_key_policy(standard = NULL) (R/url-key.R).
  expect_error(
    serialize_url("http://a/", standard = NULL),
    "`standard` must be named; NULL is not accepted",
    fixed = TRUE
  )
  expect_error(
    serialize_url("http://a/", standard = NULL),
    "Pass standard = \"whatwg\" or \"rfc3986\"",
    fixed = TRUE
  )
  expect_error(url_key_policy(standard = NULL), "NULL is not accepted",
               fixed = TRUE)
})

test_that("both named standards still serialize byte-identically", {
  # Pinned literals, so the guard above cannot have shifted the named arms
  # and this cannot pass by accident. Each row exercises something the two
  # serializers render differently or identically on purpose.
  corpus <- c(
    "HTTP://User:Pw@Example.COM:80/a/./b/../c?q=1#f",
    "https://h:443/",
    "http://h/#",
    "foo://h:80/x?#",
    "http://\u041f\u0420\u0418\u0412\u0415\u0422.\u0440\u0444/A"
  )
  expected <- list(
    whatwg = c(
      "http://User:Pw@example.com/a/c?q=1#f",
      "https://h/",
      "http://h/#",
      "foo://h:80/x?#",
      "http://xn--b1agh1afp.xn--p1ai/A"
    ),
    rfc3986 = c(
      "http://User:Pw@Example.COM:80/a/./b/../c?q=1#f",
      "https://h:443/",
      "http://h/#",
      "foo://h:80/x?#",
      "http://\u041f\u0420\u0418\u0412\u0415\u0422.\u0440\u0444/A"
    )
  )
  for (standard in c("whatwg", "rfc3986")) {
    expect_identical(
      serialize_url(corpus, standard = standard), expected[[standard]],
      info = standard
    )
  }
  # The default is, and stays, "whatwg".
  expect_identical(serialize_url(corpus), expected$whatwg)
})

# --- the oracle: parse -> serialize -> parse idempotence, publicly -----------

test_that("serialize_url() is idempotent on both standards", {
  # The identity oracle P5.3's claims stand on, asserted through the export
  # rather than only against hand-built records.
  corpus <- c(
    "http://user:pw@example.com/a/b?q=1#f", "https://h/", "http://h/#",
    "http://h/?", "http://h:8080/x/y", "http://xn--b1agh1afp.xn--p1ai/A",
    "mailto:a@b.com", "foo://h/x", "http://1.1.1.1/", "http://[::1]/",
    "file:///c/x", "http://h/a%20b", "http://:p@h/p", "https://h/?a=1&b=2"
  )
  for (standard in c("whatwg", "rfc3986")) {
    once <- serialize_url(corpus, standard = standard)
    expect_identical(
      serialize_url(once, standard = standard), once,
      info = standard
    )
  }
})

test_that("the RFC source form reproduces an already-conformant input", {
  # Asserted separately from idempotence on purpose: byte reproduction is a
  # STRONGER claim, so a future normalization change should fail on this test
  # -- the claim it actually breaks -- rather than quietly weakening the
  # idempotence assertion above.
  corpus <- c(
    "http://user:pw@example.com/a/b?q=1#f", "http://Example.COM:8080/A/B",
    "http://h/#", "http://h/?", "http://@h/p", "http://u:@h/p", "foo://h/x"
  )
  expect_identical(serialize_url(corpus, standard = "rfc3986"), corpus)
})

test_that("the WHATWG reverse-solidus rewrite reaches the serializer surface", {
  # Regression: the FSSS read the RAW source where the parser reads the
  # backslash-rewritten one, so a "\" the standard maps to the authority/path
  # boundary stayed inside the lexer's authority slice. The text before the
  # last "@" -- which is PATH -- was recovered as a userinfo the parser never
  # found, and the emitted host was duplicated into the credentials.
  # Oracles are the WHATWG-reference outcomes carried by the `youarealiar` and
  # `equivocal-urls` corpus rows (CVE-2020-26291 family).
  expect_identical(
    serialize_url("http://google.com:80\\@yahoo.com", standard = "whatwg"),
    "http://google.com/@yahoo.com"
  )
  expect_identical(
    serialize_url("https://n.pr\\@e.gg", standard = "whatwg"),
    "https://n.pr/@e.gg"
  )
  expect_identical(
    serialize_url(
      "http://example.com:80\\@localhost:8080/secret.txt",
      standard = "whatwg"
    ),
    "http://example.com/@localhost:8080/secret.txt"
  )
  # The parser found no credentials, so the serializer must emit none.
  expect_true(is.na(
    safe_parse_urls("http://google.com:80\\@yahoo.com",
                    url_standard = "whatwg")$user
  ))
})

test_that("a backslash authority introducer still carries an authority", {
  # Regression: `.has_explicit_authority()` greps a literal "://", which this
  # input does not contain, so the FSSS emitted no "//" and DROPPED the host
  # even though the parser had resolved it. WHATWG's
  # special-authority-ignore-slashes state accepts the "\"-bearing run.
  expect_identical(
    serialize_url("https:/\\/\\/\\github.com/foo/bar", standard = "whatwg"),
    "https://github.com/foo/bar"
  )
  deceptive <- paste0(
    "https://malware.testing.google.test",
    "\\testing\\malware\\*@letsencrypt.org"
  )
  expect_identical(
    serialize_url(deceptive, standard = "whatwg"),
    "https://malware.testing.google.test/testing/malware/*@letsencrypt.org"
  )
  # RFC 3986 has no reverse-solidus mapping and admits no raw "\" in a path, so
  # it rejects outright. The rewrite must stay confined to the WHATWG standard.
  expect_true(is.na(
    serialize_url("https:/\\/\\/\\github.com/foo/bar", standard = "rfc3986")
  ))
})

test_that("the backslash rewrite survives a leading control character", {
  # Regression on the ORDER of the two source normalizations: the rewrite is
  # anchored on the scheme, so running it before the C0-or-space strip made it
  # a no-op and the host was still dropped. The parser resolves `github.com`
  # for every one of these; the serializer must agree.
  variants <- c(
    "https:/\\/\\/\\github.com/foo/bar",
    " https:/\\/\\/\\github.com/foo/bar",
    "\thttps:/\\/\\/\\github.com/foo/bar",
    "https:/\\/\\/\\github.com/foo/bar "
  )
  expect_identical(
    serialize_url(variants, standard = "whatwg"),
    rep("https://github.com/foo/bar", length(variants))
  )
})
