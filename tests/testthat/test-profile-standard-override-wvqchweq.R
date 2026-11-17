# RURL-wvqchweq: a `url_standard` passed beside a `profile` that differs from
# the bundle's own standard. `profile = "rfc-syntax"` bundles rfc3986 with the
# authorized exceptions `path_normalization = "none"`, `case_handling = "keep"`
# and `path_identity = "none"` (ADR 0012 D6). They hold for rfc3986 only:
# under an override to "whatwg" (or NULL, which means "whatwg", ADR 0020 D4)
# the record is the WHATWG one, whose path state resolves dot segments
# (WHATWG URL, path state), and equals the direct "whatwg" call with the
# bundle's remaining knobs. It used to keep `/a/../b`, a path neither
# standard gives.

wvq_inputs <- c(
  "http://EX.com/a/../b",
  "http://ex.com/a/%2e%2e/b",
  "http://ex.com/a/./b",
  "http://bücher.de/a/../b",
  "http://ex.com/%7e/x",
  NA,
  ""
)

wvq_direct_whatwg <- function(x) {
  safe_parse_urls(
    x,
    url_standard = "whatwg", scheme_policy = "require",
    scheme_acceptance = "general", scheme_relative_handling = "keep"
  )
}

test_that("an overridden rfc-syntax gives the WHATWG record", {
  for (std in list("whatwg", NULL)) {
    res <- safe_parse_urls(
      wvq_inputs,
      profile = "rfc-syntax", url_standard = std
    )
    expect_identical(
      res$path,
      c("/b", "/b", "/a/b", "/b", "/%7e/x", NA, NA)
    )
    # RUL-034: under "whatwg" the host is the ASCII one.
    expect_identical(
      res$host,
      c("ex.com", "ex.com", "ex.com", "xn--bcher-kva.de", "ex.com", NA, NA)
    )
    expect_identical(
      res$clean_url,
      c(
        "http://ex.com/b", "http://ex.com/b", "http://ex.com/a/b",
        "http://xn--bcher-kva.de/b", "http://ex.com/%7e/x", NA, NA
      )
    )
    # Every column equals the direct call with the bundle's other knobs.
    expect_identical(res, wvq_direct_whatwg(wvq_inputs))
  }
})

test_that("the scalar wrapper and get_clean_url() agree with the vector", {
  one <- safe_parse_url(
    "http://EX.com/a/../b",
    profile = "rfc-syntax", url_standard = "whatwg"
  )
  expect_identical(one$path, "/b")
  expect_identical(one$clean_url, "http://ex.com/b")
  expect_identical(
    get_clean_url(
      "http://EX.com/a/../b",
      profile = "rfc-syntax", url_standard = "whatwg"
    ),
    "http://ex.com/b"
  )
  expect_identical(
    get_clean_url(
      "http://EX.com/a/../b",
      profile = "rfc-syntax", url_standard = NULL
    ),
    "http://ex.com/b"
  )
})

test_that("url_profile() prints no governed knobs under override", {
  for (std in list("whatwg", NULL)) {
    res <- url_profile("rfc-syntax", url_standard = std)
    expect_identical(res$url_standard, "whatwg")
    expect_null(res$path_normalization)
    expect_null(res$case_handling)
    expect_null(res$path_identity)
    expect_true(res$customized)
  }
  # A governed knob the caller passes by name is kept; the others still drop.
  # (Values WHATWG would refuse beside an override: RURL-ghdkisxb.)
  named <- url_profile(
    "rfc-syntax",
    url_standard = "whatwg", path_normalization = "dot_segments"
  )
  expect_identical(named$path_normalization, "dot_segments")
  expect_null(named$case_handling)
  expect_null(named$path_identity)
})

test_that("controls: the bundle's own standard keeps the exceptions", {
  ascii <- wvq_inputs[c(1L, 2L, 3L, 5L)]
  kept <- c("/a/../b", "/a/%2e%2e/b", "/a/./b", "/%7e/x")
  expect_identical(safe_parse_urls(ascii, profile = "rfc-syntax")$path, kept)
  # Same-standard override: the authorized knobs stay (RURL-wvqchweq
  # decision 3: compare with the bundle's own standard).
  same <- safe_parse_urls(
    ascii,
    profile = "rfc-syntax", url_standard = "rfc3986"
  )
  expect_identical(same$path, kept)
  expect_identical(
    same$clean_url[1L], "http://EX.com/a/../b"
  )
  expect_identical(
    url_profile("rfc-syntax", url_standard = "rfc3986")$path_normalization,
    "none"
  )
  # No profile, "whatwg": the WHATWG path state resolves the dot segments.
  expect_identical(
    wvq_direct_whatwg(wvq_inputs)$path,
    c("/b", "/b", "/a/b", "/b", "/%7e/x", NA, NA)
  )
})
