# Locale invariance of rurl's output (RURL-izzuenjx).
#
# The contract these tests lock in, stated plainly:
#
#   1. rurl's output does not depend on `LC_CTYPE`. The same input yields the
#      same VALUES and the same `Encoding()` marks under `LC_ALL=C` and under a
#      UTF-8 locale. The cross-locale half of that is enforced by CI: the
#      `Tests (LC_ALL=C)` cell in .github/workflows/verify.yml runs this whole
#      suite under a genuinely non-UTF-8 LC_CTYPE. What THIS file asserts is
#      the in-process half -- the marks and the bytes -- which is what makes
#      the two runs agree.
#   2. Every returned character component carries `Encoding() == "UTF-8"` when
#      it holds non-ASCII. R never marks a pure-ASCII string, so an ASCII
#      component correctly reports "unknown"; that is asserted too, so a future
#      reader does not "fix" it into a bug.
#   3. `enc2utf8()` and `iconv()`-from-native must not reappear in the parse
#      path. Both transcode FROM THE SESSION LOCALE, which is precisely the
#      defect the marks replaced (`Encoding<-` declares, it never converts).
#
# Every assertion here would fail if one of the six fixes that established the
# contract were reverted (e9defab, c6a0457, b26a334, 62a24a9, d7181a1, 0a339ce).
#
# All non-ASCII literals below are written as \u escapes on purpose: an escape
# parses to a UTF-8-marked string in ANY locale, so these tests cannot
# themselves become the locale-dependent thing they are testing.

# http://<Cyrillic "primer">.<Cyrillic "rf">/<Cyrillic "p"> -- a wholly
# non-ASCII URL whose registrable domain AND public suffix are both IDN, so
# host / domain / tld / *_unicode / path all carry non-ASCII at once.
idn_url <- "http://\u043f\u0440\u0438\u043c\u0435\u0440.\u0440\u0444/\u043f"
ascii_url <- "http://example.com/p"

# Components that can carry non-ASCII, and the ones that are ASCII by
# construction (percent-encoded or a fixed vocabulary).
utf8_components <- c(
  "original_url", "host", "path", "domain", "tld", "domain_unicode",
  "tld_unicode", "clean_url"
)
ascii_components <- c("scheme", "domain_ascii", "tld_ascii", "parse_status")


# --- 1. Encoding() marks on the scalar parser --------------------------------

test_that("an IDN URL's non-ASCII components are declared UTF-8", {
  res <- safe_parse_url(idn_url)
  for (nm in utf8_components) {
    expect_identical(Encoding(res[[nm]]), "UTF-8", label = nm)
  }
})

test_that("ASCII-by-construction components are correctly unmarked", {
  # NOT a gap: R refuses to attach an encoding mark to a pure-ASCII string, so
  # "unknown" IS the correct answer for these and `.mark_result_utf8()` is a
  # deliberate no-op on them. Do not "fix" this into an expectation of "UTF-8"
  # -- it is unreachable.
  res <- safe_parse_url(idn_url)
  for (nm in ascii_components) {
    expect_identical(Encoding(res[[nm]]), "unknown", label = nm)
  }

  # A fully ASCII URL: every character component stays unmarked.
  ascii <- safe_parse_url(ascii_url)
  chr <- vapply(ascii, is.character, logical(1))
  expect_identical(
    unique(Encoding(unlist(ascii[chr], use.names = FALSE))), "unknown"
  )
})


# --- 2. Encoding() marks on the vector parser and the accessors --------------

test_that("safe_parse_urls() marks columns the same as safe_parse_url()", {
  # The Stage-B assembly chokepoint (c6a0457) is a different code path from the
  # scalar one; both must land on the same contract.
  df <- safe_parse_urls(c(idn_url, ascii_url))
  for (nm in utf8_components) {
    expect_identical(Encoding(df[[nm]][1]), "UTF-8", label = nm)
    expect_identical(Encoding(df[[nm]][2]), "unknown", label = nm)
  }
  for (nm in ascii_components) {
    expect_identical(Encoding(df[[nm]]), c("unknown", "unknown"), label = nm)
  }
})

test_that("accessors return UTF-8-marked values for an IDN URL", {
  expect_identical(Encoding(get_host(idn_url)), "UTF-8")
  expect_identical(Encoding(get_domain(idn_url)), "UTF-8")
  expect_identical(Encoding(get_tld(idn_url)), "UTF-8")
  expect_identical(Encoding(get_clean_url(idn_url)), "UTF-8")
  # ...and the unmarked-ASCII counterpart, for the same reason as above.
  expect_identical(Encoding(get_host(ascii_url)), "unknown")
  expect_identical(Encoding(get_domain(ascii_url)), "unknown")
})


# --- 3. Byte-level equality --------------------------------------------------

test_that("non-ASCII output is byte-identical to its UTF-8 encoding", {
  # A mark is not enough: a transcoding regression would change the BYTES.
  # These are the literal UTF-8 octets, spelled out rather than eyeballed from
  # a printed string.
  res <- safe_parse_url("http://m\u00fcnchen.de/\u00e9cole")

  expect_identical(
    charToRaw(res$host),
    as.raw(c(0x6d, 0xc3, 0xbc, 0x6e, 0x63, 0x68, 0x65, 0x6e, 0x2e, 0x64, 0x65))
  )
  expect_identical(
    charToRaw(res$path),
    as.raw(c(0x2f, 0xc3, 0xa9, 0x63, 0x6f, 0x6c, 0x65))
  )
  # domain / domain_unicode carry the same octets as the host they came from,
  # even though they are round-tripped through pslr and punycoder.
  expect_identical(charToRaw(res$domain), charToRaw(res$host))
  expect_identical(charToRaw(res$domain_unicode), charToRaw(res$host))

  # The IDN public suffix, likewise: Cyrillic "rf" is U+0440 U+0444.
  idn <- safe_parse_url(idn_url)
  expect_identical(charToRaw(idn$tld), as.raw(c(0xd1, 0x80, 0xd1, 0x84)))
  expect_identical(charToRaw(idn$tld_unicode), charToRaw(idn$tld))
  expect_identical(charToRaw(idn$path), as.raw(c(0x2f, 0xd0, 0xbf)))
})


# --- 4. The specific regressions the determinism epic fixed ------------------

test_that("domain is not NA for an IDN host", {
  # Pre-e9defab, the host reached pslr as unmarked native bytes; under a
  # non-UTF-8 LC_CTYPE pslr re-decoded them in the session locale and returned
  # NA, so domain / tld / *_ascii / *_unicode were ALL NA in a C locale and
  # populated in a UTF-8 one.
  for (u in c(idn_url, "http://xn--e1afmkfd.xn--p1ai/")) {
    res <- safe_parse_url(u)
    expect_false(is.na(res$domain))
    expect_false(is.na(res$tld))
    expect_identical(res$domain_ascii, "xn--e1afmkfd.xn--p1ai")
    expect_identical(res$tld_ascii, "xn--p1ai")
  }
})

test_that("a non-ASCII path percent-encodes to the UTF-8 octets", {
  # Pre-d7181a1 the path encoder read `charToRaw(enc2utf8(x))`. Under LC_ALL=C
  # `enc2utf8()` RE-DECODED the already-UTF-8 octets as native, so the encoder
  # saw the literal text "<c3><a9>" and emitted `/%3Cc3%3E%3Ca9%3Ecole`.
  encoded <- get_clean_url("http://ex.com/\u00e9cole", path_encoding = "encode")
  expect_identical(encoded, "http://ex.com/%C3%A9cole")
  expect_false(grepl("%3C", encoded, fixed = TRUE))
})

test_that("a U+FFFF mailto local part does not error", {
  # Base `toupper()` routes through `utf8towcs()`, which ERRORS on the U+FFFF
  # noncharacter; 62a24a9 pinned the site to `.ascii_toupper()`. Primary
  # coverage lives in test-email-diagnostics.R; repeated here because it is one
  # of the named cases this locale-invariance gate exists to hold.
  expect_no_error(
    out <- get_mailto_recipients(
      "mailto:a\uffffb@example.com", scheme_acceptance = "general"
    )
  )
  expect_identical(nrow(out), 1L)
  expect_identical(out$mailto_local_part_form, "invalid")
})


# --- 5. Meta-guard: no transcoding-from-native in the package ----------------

# Which namespace functions reference `fn`, and how many times. Reads the
# parsed BODY (`all.names()`), not the file text, so comments -- including the
# several that discuss `enc2utf8()` by name -- cannot produce a false hit, and
# the guard works whether or not package sources ship with the tests.
ns_callers <- function(fn) {
  ns <- asNamespace("rurl")
  nms <- ls(ns, all.names = TRUE)
  counts <- vapply(nms, function(nm) {
    obj <- get(nm, envir = ns)
    if (!is.function(obj)) {
      return(0L)
    }
    sum(all.names(body(obj)) == fn)
  }, integer(1))
  counts[counts > 0L]
}

test_that("enc2utf8() is confined to the one known open site", {
  # `enc2utf8()` transcodes FROM the session locale. It must not reappear
  # anywhere in the parse path.
  #
  # Two calls remain, both in `.email_valid_quoted_content()`
  # (R/email-diagnostics.R:63,71). That is DELIBERATELY still open as
  # RURL-iwtrwayz and is out of scope here -- so this guard asserts the exact
  # expected set and count rather than pretending the count is zero. When
  # RURL-iwtrwayz lands, this expectation becomes `integer(0)`-shaped.
  expect_identical(
    ns_callers("enc2utf8"), c(.email_valid_quoted_content = 2L)
  )
})

test_that("iconv() is confined to the two UTF-8-to-UTF-8 sanitizers", {
  # The surviving `iconv()` calls are `from = "UTF-8", to = "UTF-8", sub = ""`
  # in R/domain.R -- an invalid-sequence scrubber, NOT a from-native
  # transcode, so they carry no locale dependency. Any new caller is a
  # regression until proven otherwise.
  expect_identical(
    ns_callers("iconv"),
    c(.punycode_to_unicode = 1L, .punycode_to_unicode_vec = 1L)
  )
})
