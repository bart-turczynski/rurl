# Scaffold tests for the url_standard selector (RURL-bbojhnhu, epic
# RURL-eqzkkohm). T1 shipped the argument as a deliberate NO-OP: it validates,
# conflict-checks, and threads through every public entry point, but does not
# change path/host output. T3 (RURL-gjltzwmp)/T4 (RURL-luwvkwhd)/T5
# (RURL-bbmuehsx) replace the placeholder behavior WITHOUT changing the
# conflict matrix these tests assert (still covered below), and reuse the
# AC#1 corpus gate below (which remains the release-safety invariant: NULL
# stays byte-for-byte inert forever).

# --- AC #1: url_standard = NULL is byte-for-byte inert (no output/shape drift)

test_that("url_standard = NULL reproduces the full corpus byte-for-byte", {
  urls <- parse_corpus_urls()
  combos <- parse_corpus_combos()

  for (combo_name in names(combos)) {
    combo <- combos[[combo_name]]
    baseline <- do.call(safe_parse_urls, c(list(url = urls), combo))
    with_null <- do.call(
      safe_parse_urls, c(list(url = urls, url_standard = NULL), combo)
    )
    expect_identical(with_null, baseline, info = combo_name)
  }
})

test_that("url_standard = NULL does not change the output shape", {
  # safe_parse_urls() column set / order.
  cols_default <- names(safe_parse_urls("http://ex.com/"))
  cols_null <- names(safe_parse_urls("http://ex.com/", url_standard = NULL))
  expect_identical(cols_null, cols_default)

  # safe_parse_url() list field set / order.
  fields_default <- names(safe_parse_url("http://ex.com/"))
  fields_null <- names(safe_parse_url("http://ex.com/", url_standard = NULL))
  expect_identical(fields_null, fields_default)

  # Accessors that take url_standard: NULL matches omitting it.
  u <- c("http://www.example.co.uk/A%2FB", "2130706433", "not-a-url")
  expect_identical(get_host(u, url_standard = NULL), get_host(u))
  expect_identical(get_path(u, url_standard = NULL), get_path(u))
  expect_identical(get_clean_url(u, url_standard = NULL), get_clean_url(u))
  expect_identical(get_domain(u, url_standard = NULL), get_domain(u))
  expect_identical(get_tld(u, url_standard = NULL), get_tld(u))
  expect_identical(get_subdomain(u, url_standard = NULL), get_subdomain(u))
  expect_identical(
    get_parse_status(u, url_standard = NULL), get_parse_status(u)
  )
})

# --- Validation

test_that("invalid url_standard values error clearly", {
  # A length-1 valid string is fine; every other malformed value errors.
  for (val in list("rfc", "RFC3986", "", NA_character_, 1L)) {
    expect_error(
      safe_parse_url("http://ex.com/", url_standard = val),
      "url_standard must be NULL"
    )
  }
  expect_error(
    safe_parse_url("http://ex.com/", url_standard = c("rfc3986", "whatwg")),
    "url_standard must be NULL"
  )
  # Valid values are accepted (no error) even though behavior is a no-op here.
  expect_silent(safe_parse_url("http://ex.com/", url_standard = "rfc3986"))
  expect_silent(safe_parse_url("http://ex.com/", url_standard = "whatwg"))
})

# --- Positional-argument backward-compatibility (hard CRAN constraint)

test_that("a positional 2nd arg still binds to the historical parameter", {
  # safe_parse_url()'s 2nd positional is protocol_handling; "none" must NOT add
  # a scheme to a scheme-less host, proving url_standard did not steal slot 2.
  expect_identical(
    safe_parse_url("example.com", "none")$scheme,
    safe_parse_url("example.com", protocol_handling = "none")$scheme
  )
  expect_true(is.na(safe_parse_url("example.com", "none")$scheme))

  # get_host()'s 2nd positional is likewise protocol_handling.
  expect_identical(
    get_host("example.com", "none"),
    get_host("example.com", protocol_handling = "none")
  )
})

# --- Conflict matrix (final matrix, direct args)

test_that("explicit governed knobs conflicting with the profile error", {
  u <- "http://ex.com/%41%42"

  # path_encoding: the profile's required value is a profile-internal mode with
  # no public enum equivalent, so ANY explicit public path_encoding conflicts.
  for (std in c("rfc3986", "whatwg")) {
    for (pe in c("keep", "encode", "decode")) {
      expect_error(
        safe_parse_url(u, url_standard = std, path_encoding = pe),
        "governs `path_encoding`"
      )
    }
  }

  # path_normalization: profiles resolve dot segments, so only "dot_segments"
  # is accepted; every other value conflicts.
  for (std in c("rfc3986", "whatwg")) {
    for (pn in c("none", "collapse_slashes", "both")) {
      expect_error(
        safe_parse_url(u, url_standard = std, path_normalization = pn),
        "governs `path_normalization`"
      )
    }
    # The value the profile would pick is accepted.
    expect_silent(
      safe_parse_url(u, url_standard = std, path_normalization = "dot_segments")
    )
  }
})

test_that("no conflict without url_standard or with the selector alone", {
  u <- "http://ex.com/%41%42"
  expect_silent(safe_parse_url(u, path_encoding = "decode"))
  expect_silent(safe_parse_url(u, path_normalization = "both"))
  expect_silent(safe_parse_url(u, url_standard = "rfc3986"))
  expect_silent(safe_parse_urls(u, url_standard = "whatwg"))
})

test_that("rfc3986 path profile is no longer a no-op (RURL-gjltzwmp)", {
  # Literal dot segments resolve identically either way (that part of the
  # matrix predates T3), but the profile's unreserved-only percent decode is
  # new behavior: %41%42 folds to "AB" under the selector, and is left alone
  # by the equivalent explicit low-level knob.
  expect_identical(
    safe_parse_urls("http://ex.com/a/../b", url_standard = "rfc3986")$path,
    safe_parse_urls("http://ex.com/a/../b",
      path_normalization = "dot_segments")$path
  )
  expect_identical(
    safe_parse_url("http://ex.com/%41%42", url_standard = "rfc3986")$path,
    "/AB"
  )
  expect_false(identical(
    safe_parse_url("http://ex.com/%41%42",
      path_normalization = "dot_segments")$path,
    "/AB"
  ))
})

# --- Conflict matrix through the get_path()/get_clean_url() seams

test_that("get_path()/get_clean_url() enforce the conflict matrix", {
  u <- "http://ex.com/%41%42"
  expect_error(
    get_path(u, url_standard = "rfc3986", path_encoding = "decode"),
    "governs `path_encoding`"
  )
  expect_error(
    get_clean_url(u, url_standard = "whatwg", path_normalization = "both"),
    "governs `path_normalization`"
  )
  expect_silent(get_path(u, url_standard = "rfc3986"))
  expect_silent(get_clean_url(u, url_standard = "whatwg"))
  # Accepted value passes through the accessor.
  expect_silent(
    get_path(u, url_standard = "rfc3986", path_normalization = "dot_segments")
  )
})

# --- Conflict matrix across the canonical_join() `...` seam

test_that("canonical_join() enforces the conflict matrix through `...`", {
  A <- data.frame(URL = "http://ex.com/a", ValA = 1L, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://ex.com/a", ValB = 2L, stringsAsFactors = FALSE)

  expect_error(
    canonical_join(A, B, url_standard = "rfc3986", path_encoding = "keep"),
    "governs `path_encoding`"
  )
  expect_error(
    canonical_join(A, B, url_standard = "whatwg", path_normalization = "none"),
    "governs `path_normalization`"
  )
  expect_error(
    canonical_join(A, B, url_standard = "bogus"),
    "url_standard must be NULL"
  )
  # Selector alone forwards cleanly and still joins.
  joined <- canonical_join(A, B, url_standard = "whatwg")
  expect_equal(nrow(joined), 1L)
})
