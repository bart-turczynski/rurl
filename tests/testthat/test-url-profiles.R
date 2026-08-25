# Tests for the public `profile` axis and the url_profile() inspector
# (ADR 0012 Layer 6b, RURL-djmgzjmr).

test_that("url_profile() resolves each bundle to its exact knob set", {
  expect_identical(
    url_profile("browser"),
    list(
      url_standard = "whatwg",
      scheme_acceptance = "general",
      scheme_policy = "infer",
      scheme_relative_handling = "http",
      fixup_posture = "browser",
      customized = FALSE
    )
  )
  expect_identical(
    url_profile("whatwg"),
    list(
      url_standard = "whatwg",
      scheme_acceptance = "general",
      scheme_policy = "require",
      scheme_relative_handling = "error",
      customized = FALSE
    )
  )
  expect_identical(
    url_profile("rfc-syntax"),
    list(
      url_standard = "rfc3986",
      scheme_acceptance = "general",
      scheme_policy = "require",
      scheme_relative_handling = "keep",
      path_normalization = "none",
      case_handling = "keep",
      path_identity = "none",
      customized = FALSE
    )
  )
  expect_identical(
    url_profile("seo"),
    list(
      url_standard = "whatwg",
      scheme_acceptance = "web",
      protocol_handling = "https",
      www_handling = "strip",
      trailing_slash_handling = "strip",
      index_page_handling = "strip",
      host_encoding = "unicode",
      query_handling = "drop",
      customized = FALSE
    )
  )
})

test_that("canonical is an alias resolving identically to seo", {
  expect_identical(url_profile("canonical"), url_profile("seo"))
})

test_that("explicit overrides win and flag customized", {
  res <- url_profile("browser", scheme_policy = "require")
  expect_identical(res$scheme_policy, "require")
  expect_true(res$customized)
  # a pure bundle is not customized
  expect_false(url_profile("browser")$customized)
})

test_that("url_profile() rejects unknown profiles and overrides", {
  expect_error(url_profile("nope"), "profile must be NULL")
  expect_error(url_profile(), "requires a profile name")
  expect_error(
    url_profile("browser", not_a_knob = "x"),
    "does not recognize override"
  )
})

test_that("profile = NULL is byte-identical to omitting the argument", {
  url <- "http://www.Example.com/index.html?utm_source=x&id=1"
  expect_identical(
    safe_parse_url(url, profile = NULL),
    safe_parse_url(url)
  )
  expect_identical(
    get_clean_url(url, profile = NULL),
    get_clean_url(url)
  )
  expect_identical(
    safe_parse_urls(c(url, "example.com"), profile = NULL),
    safe_parse_urls(c(url, "example.com"))
  )
})

test_that("browser profile repairs and http-prepends", {
  # `;` -> `:` repair via the bounded fixer
  fixed <- safe_parse_url("http;//example.com", profile = "browser")
  expect_identical(fixed$parse_status, "ok")
  expect_identical(fixed$scheme, "http")
  expect_identical(fixed$host, "example.com")
  # scheme-less host-shaped input gains http (scheme_policy = "infer")
  prepended <- safe_parse_url("example.com", profile = "browser")
  expect_identical(prepended$parse_status, "ok")
  expect_identical(prepended$scheme, "http")
})

test_that("whatwg profile rejects scheme-less input", {
  # safe_parse_url() returns NULL for a rejected scalar input (existing scalar
  # contract), so the rejection surfaces as a NULL result.
  expect_null(safe_parse_url("example.com", profile = "whatwg"))
  # in the vectorized wrapper the same input becomes an "error" row.
  res <- safe_parse_urls("example.com", profile = "whatwg")
  expect_identical(res$parse_status, "error")
})

test_that("rfc-syntax profile parses without normalizing", {
  res <- safe_parse_url("http://EXAMPLE.com/a/../b", profile = "rfc-syntax")
  expect_identical(res$parse_status, "ok")
  # case preserved (case_handling = "keep")
  expect_true(grepl("EXAMPLE", res$clean_url, fixed = TRUE))
  # dot-segments preserved (path_normalization = "none")
  expect_true(grepl("/a/../b", res$clean_url, fixed = TRUE))
})

test_that("rfc-syntax authorized exception is profile-only", {
  # A DIRECT (no-profile) call combining rfc3986 with path_normalization="none"
  # must STILL be rejected exactly as today (ADR 0007 conflict matrix intact).
  expect_error(
    safe_parse_url(
      "http://example.com/a/../b",
      url_standard = "rfc3986",
      path_normalization = "none"
    ),
    "governs"
  )
})

test_that("seo profile cleans origin URLs", {
  out <- get_clean_url(
    "http://www.example.com/index.html?utm_source=x",
    profile = "seo"
  )
  # seo strips the trailing slash (trailing_slash_handling = "strip")
  expect_identical(out, "https://example.com")
})

# --- the clean-URL definition (RURL-hcntbqku, ADR 0017) ----------------------
#
# A clean URL is a LOSSY POLICY PROJECTION of a WHATWG-parsed URL, never a
# separate, weaker construction. These pin the definition items the `seo`
# bundle carries; each was verified to FAIL against the pre-change bundle.

test_that("seo: folders resolve -- no dot segment survives cleaning", {
  expect_identical(
    get_clean_url("http://example.com/a/./b/../c", profile = "seo"),
    "https://example.com/a/c"
  )
  # the compounding case: index-page strip on a path that still needs resolving
  expect_identical(
    get_clean_url("http://example.com/a/./b/../index.html", profile = "seo"),
    "https://example.com/a"
  )
  # corpus sweep: no cleaned SEO output carries "/./" or "/../"
  corpus <- read.csv(
    testthat::test_path("fixtures", "parse-corpus.csv"),
    stringsAsFactors = FALSE
  )$url
  cleaned <- get_clean_url(corpus, profile = "seo")
  cleaned <- cleaned[!is.na(cleaned)]
  expect_false(any(grepl("/\\./|/\\.\\./", cleaned)))
  expect_false(any(grepl("/\\.$|/\\.\\.$", cleaned)))
})

test_that("seo: the domain is Unicode regardless of the input spelling", {
  # host_encoding = "keep" echoed whichever spelling the input used, so the
  # same site yielded xn-- from one row and Unicode from the next.
  expect_identical(
    get_clean_url("https://xn--mnchen-3ya.de/a", profile = "seo"),
    "https://münchen.de/a"
  )
  for (host in c("łódź.pl", "домен.рф", "münchen.de", "例え.テスト")) {
    unicode_url <- paste0("http://", host, "/a")
    puny_host <- get_host(
      unicode_url,
      url_standard = "whatwg", host_encoding = "idna"
    )
    expect_identical(
      get_clean_url(unicode_url, profile = "seo"),
      get_clean_url(paste0("http://", puny_host, "/a"), profile = "seo")
    )
  }
})

test_that("seo: the fragment and the port do not survive", {
  expect_identical(
    get_clean_url("https://example.com:8080/a#top", profile = "seo"),
    "https://example.com/a"
  )
})

test_that("seo: the whole query goes, not just names on the denylist", {
  # ADR 0017 D3. `filter` was name-matched, so it kept every parameter it did
  # not recognize -- including `utm=x`, which LOOKS like a tracker and is not
  # on the list. `drop` is the projection the definition asks for.
  expect_identical(
    get_clean_url("https://example.com/p?utm_source=nl&id=7&ref=x",
                  profile = "seo"),
    "https://example.com/p"
  )
  expect_identical(
    get_clean_url("https://example.com/p?utm=x&id=1", profile = "seo"),
    "https://example.com/p"
  )
  # a URL with no tracker at all used to come back completely untouched
  expect_identical(
    get_clean_url("https://example.com/p?a=1&b=2", profile = "seo"),
    "https://example.com/p"
  )
})

test_that("seo is never LESS clean on parameters than passing no profile", {
  # The anomaly that decided item 7: asking for the SEO preset used to buy
  # strictly less parameter cleaning than asking for nothing. This is the
  # regression pin, stated as the relation rather than as two literals.
  urls <- c(
    "https://example.com/p?utm_source=nl&id=7&ref=x",
    "https://example.com/p?a=1&b=2",
    "https://example.com/p?utm=x&id=1",
    "https://example.com/p"
  )
  expect_identical(
    get_clean_url(urls, profile = "seo"),
    get_clean_url(urls, protocol_handling = "https")
  )
})

test_that("seo: tracker-only removal survives as an explicit opt-in", {
  # `filter` is not the preset any more, but it is not gone: a caller who wants
  # semantic parameters preserved says so, and the iron rule makes it win.
  expect_identical(
    get_clean_url("https://example.com/p?utm_source=nl&id=7",
                  profile = "seo", query_handling = "filter"),
    "https://example.com/p?id=7"
  )
})

test_that("seo: an explicit dial still overrides the profile (iron rule)", {
  # the two knobs the definition added must stay overridable
  expect_identical(
    get_clean_url(
      "https://xn--mnchen-3ya.de/a",
      profile = "seo", host_encoding = "keep"
    ),
    "https://xn--mnchen-3ya.de/a"
  )
  # the bundle now selects a url_standard, whose .parse_options() expansion
  # governs path_normalization -- an explicit value must still win over it
  expect_identical(
    get_clean_url(
      "http://example.com/a/./b/../c",
      profile = "seo", path_normalization = "none"
    ),
    "https://example.com/a/./b/../c"
  )
  res <- url_profile("seo", host_encoding = "keep")
  expect_true(res$customized)
  expect_identical(res$host_encoding, "keep")
})

test_that("seo enrichment does not disturb the ADR 0007 NULL-selector freeze", {
  # The definition is delivered entirely inside the `seo` bundle, so a caller
  # who passes no profile must be byte-identical to before. Sentinels chosen
  # from the rows the seo column actually moved.
  expect_identical(
    safe_parse_urls("https://xn--mnchen-3ya.de/")$clean_url,
    "https://xn--mnchen-3ya.de/"
  )
  expect_identical(
    safe_parse_urls("https://example.com/a/./b/../c")$clean_url,
    "https://example.com/a/./b/../c"
  )
  expect_identical(
    safe_parse_urls("https://example.com./")$clean_url,
    "https://example.com./"
  )
})

# --- profile across the canonical_join() `...` seam (RURL-cujzicqf) ----------

test_that("canonical_join() profile via `...` is LEGACY: it warns, re-keys", {
  A <- data.frame(
    URL = "http://www.Example.com/Page/index.html?utm_source=x",
    ValA = 1L, stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "https://example.com/Page",
    ValB = 2L, stringsAsFactors = FALSE
  )
  # P3.1 D-E / RURL-bgzzyfwd. This pins LEGACY `clean_url` keying, NOT the v3
  # identity model: `profile` is a presentation/cleaning bundle that takes no
  # part in URL identity, yet forwarding it through `...` changes WHICH ROWS
  # MATCH. D-E keeps that as compatibility-only for a deprecation window and
  # requires the dial to warn -- so both halves are pinned here.
  expect_warning(
    canonical_join(A, B, profile = "seo"),
    class = "rurl_legacy_join_dial_warning"
  )
  # seo canonicalization (https + strip www / index / trailing slash / tracking
  # params) makes the two rows share a legacy key and join.
  joined <- cj_legacy(canonical_join(A, B, profile = "seo"))
  expect_identical(nrow(joined), 1L)
  expect_identical(joined$JoinKey, "https://example.com/Page")
})

test_that("the LEGACY join seam converges on its own default under ADR 0017", {
  # `clean_url` doubles as the legacy join key, so seo's query_handling decides
  # WHICH ROWS MATCH here. ADR 0017 moves that toward canonical_join()'s own
  # no-profile default rather than away from it: `?id=7` and `?id=8` already
  # keyed together with no profile, and `filter` was the only thing keeping
  # them apart. Pinned so the consequence is recorded, not discovered.
  A <- data.frame(URL = "https://example.com/p?id=7", ValA = 7L,
                  stringsAsFactors = FALSE)
  B <- data.frame(URL = "https://example.com/p?id=8", ValB = 8L,
                  stringsAsFactors = FALSE)
  bare <- canonical_join(A, B)
  seo <- suppressWarnings(canonical_join(A, B, profile = "seo"))
  expect_identical(nrow(seo), nrow(bare))
  expect_identical(cj_legacy(seo)$JoinKey, cj_legacy(bare)$JoinKey)
  expect_identical(cj_legacy(seo)$JoinKey, "https://example.com/p")
  # still legacy, still non-silent
  expect_warning(
    canonical_join(A, B, profile = "seo"),
    class = "rurl_legacy_join_dial_warning"
  )
})

test_that("the LEGACY profile seam skips the conflict matrix (P3.1 D-E)", {
  A <- data.frame(URL = "http://ex.com/a", ValA = 1L, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://ex.com/a", ValB = 2L, stringsAsFactors = FALSE)

  # P3.1 D-E / RURL-bgzzyfwd: the `...` profile seam is legacy `clean_url`
  # keying, not the v3 identity model. Pinning how it dispatches is not an
  # endorsement of a presentation bundle reaching the join key.
  #
  # A profile authorizes its own combination, so an explicit governed knob that
  # would conflict under a DIRECT url_standard call is accepted here (iron
  # rule), exactly as in safe_parse_url(). This must NOT error. It does warn:
  # `profile` is a presentation bundle, and P3.1 D-E.1 makes those non-silent.
  expect_warning(
    canonical_join(
      A, B,
      profile = "seo", url_standard = "whatwg", path_normalization = "none"
    ),
    class = "rurl_legacy_join_dial_warning"
  )
  # Sanity: the same combination WITHOUT a profile still fails fast.
  expect_error(
    canonical_join(A, B, url_standard = "whatwg", path_normalization = "none"),
    "governs `path_normalization`"
  )
})

test_that("canonical_join() rejects an unknown profile up front", {
  A <- data.frame(URL = "http://ex.com/a", ValA = 1L, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://ex.com/a", ValB = 2L, stringsAsFactors = FALSE)
  expect_error(
    canonical_join(A, B, profile = "nope"),
    "profile must be NULL or one of"
  )
})
