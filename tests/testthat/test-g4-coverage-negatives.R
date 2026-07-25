# G4 criterion-3 negative coverage.
#
# The six-family coverage audit found a systematic asymmetry: behavior that
# ships correctly and is exercised on the happy path, but whose NEGATIVE
# polarity is unasserted. G4 criterion 3 requires both polarities, so a cell
# with only positive coverage is a gap, not a pass.
#
# Every assertion here pins behavior that ALREADY holds -- these tests were
# green the first time they ran. None of them encodes a wished-for behavior:
# cells whose correct behavior is still undecided (the general-branch
# credential loss, the authority_kind route disagreement, list-input recovery,
# input-name handling) are deliberately absent, because writing a test for an
# undecided cell would freeze an arbitrary answer as if it were the contract.
# Those carry their own fp carriers.

# ---- mutation: invalid dial values are rejected -----------------------------

# The audit found ZERO tests asserting an invalid enum value for any of the 11
# legacy presentation dials, while query_handling, port_handling, scheme_policy,
# scheme_acceptance, url_standard, profile and engine all had one. The dials are
# validated through match.arg() in .parse_options(); nothing pinned it.
#
# The expectation matches only "should be one of" and NEVER the quoted
# alternatives: R renders those with directional quotes under a UTF-8 locale and
# straight quotes under LC_ALL=C, and the suite runs under both (the
# `Tests (LC_ALL=C)` CI job). Matching the quotes would make this cell pass on
# one runner and fail on the other.
test_that("presentation dials reject an unknown value", {
  dials <- c(
    "protocol_handling", "www_handling", "tld_source", "case_handling",
    "trailing_slash_handling", "index_page_handling", "path_normalization",
    "scheme_relative_handling", "host_encoding", "path_encoding"
  )
  for (d in dials) {
    args <- list("https://example.com/p/")
    args[[d]] <- "definitely-not-a-valid-value"
    expect_error(do.call(safe_parse_urls, args), "should be one of",
                 info = d)
  }
})

# ---- mutation: dials are no-ops on ineligible targets -----------------------

test_that("index_page_handling is a no-op when there is no index page", {
  # The eligible case, so the no-op below is not vacuous.
  expect_identical(
    get_clean_url("https://ex.com/index.html", index_page_handling = "strip"),
    "https://ex.com/"
  )
  # Nothing to strip: a bare directory URL is returned unchanged.
  expect_identical(
    get_clean_url("https://ex.com/", index_page_handling = "strip"),
    "https://ex.com/"
  )
})

test_that("trailing_slash_handling no-ops without a trailing slash", {
  # Eligible: the slash is removed, including on the root path.
  expect_identical(
    get_clean_url("https://ex.com/a/", trailing_slash_handling = "strip"),
    "https://ex.com/a"
  )
  # Ineligible: no trailing slash to remove, so `strip` changes nothing.
  expect_identical(
    get_clean_url("https://ex.com/a", trailing_slash_handling = "strip"),
    "https://ex.com/a"
  )
})

# ---- migration M-3: the two guaranteed surfaces agree -----------------------

# P2.3 §5 guarantees BOTH the `parse_status` column and `get_parse_status()`
# are byte-identical to v2. Nothing asserted that the two agree with each
# other, so a projection that drifted on one surface only would have gone
# unnoticed. `test-accessor-registry.R` is a signature oracle -- it checks
# formals and registry completeness, never values -- and the characterization
# snapshot proves scalar/vector agreement, not accessor/frame agreement.
test_that("get_parse_status() agrees with the parse_status column", {
  urls <- c(
    "https://example.com/", "ftp://example.com/f", "//example.com/x",
    "not a url", "https://example.invalidtld/", "user@example.com",
    "https://example.com:8080/p?q=1#f", NA_character_, ""
  )
  frame <- safe_parse_urls(urls)
  expect_identical(get_parse_status(urls), frame$parse_status)
})

# ---- migration M-5: the status predicates are unaffected --------------------

# P2.3 §5 names .is_ok_status / .is_warning_status / .is_joinable_status as
# callers that must be unaffected by the v3 verdict layering. They had no direct
# test at all -- only indirect exercise through canonical_join's filter.
test_that("status predicates partition the vocabulary", {
  ok <- c("ok", "ok-ftp", "ok-scheme-relative")
  warn <- c("warning-no-tld", "warning-invalid-tld", "warning-public-suffix",
            "warning-userinfo")

  expect_true(all(vapply(ok, rurl:::.is_ok_status, logical(1))))
  expect_false(any(vapply(c(warn, "error"), rurl:::.is_ok_status, logical(1))))

  expect_true(all(vapply(warn, rurl:::.is_warning_status, logical(1))))
  expect_false(any(vapply(c(ok, "error"), rurl:::.is_warning_status,
                          logical(1))))

  # "error" is the single terminal status: neither ok nor warning.
  expect_false(rurl:::.is_ok_status("error"))
  expect_false(rurl:::.is_warning_status("error"))
})

test_that("joinable status depends on the mode", {
  # The two modes are exactly canonical_join()'s `join_parse_status` values
  # (R/canonical_join.R:99, match.arg'd), not free-form strings: "ok_or_warning"
  # admits warnings, "ok" does not.
  expect_true(rurl:::.is_joinable_status("warning-no-tld", "ok_or_warning"))
  expect_false(rurl:::.is_joinable_status("warning-no-tld", "ok"))
  # `ok` is joinable under both modes, and `error` under neither.
  expect_true(rurl:::.is_joinable_status("ok", "ok_or_warning"))
  expect_true(rurl:::.is_joinable_status("ok", "ok"))
  expect_false(rurl:::.is_joinable_status("error", "ok_or_warning"))
  expect_false(rurl:::.is_joinable_status("error", "ok"))

  # The mode vocabulary itself is the join surface's, so a drift there would
  # silently change which rows join.
  expect_identical(
    eval(formals(canonical_join)$join_parse_status),
    c("ok", "ok_or_warning")
  )
})

# ---- vector: accessor negatives ---------------------------------------------

# The accessor loop in test-accessor-helper.R passes two well-formed URLs, so
# it proves length preservation only on the happy path. The audit found no
# zero-length or mixed-validity coverage for the 12 accessors it loops over.
test_that("character accessors accept zero-length input", {
  for (fn in list(get_scheme, get_host, get_path, get_query, get_fragment,
                  get_user, get_password, get_domain, get_tld,
                  get_clean_url, get_parse_status)) {
    out <- fn(character(0))
    expect_type(out, "character")
    expect_length(out, 0)
  }
  expect_length(get_port(character(0)), 0)
})

test_that("character accessors preserve length over mixed-validity input", {
  # Valid, NA, empty, unparseable, and a duplicate of the first -- the shapes
  # the corpus carries as sentinels, in one vector.
  urls <- c("https://example.com/a", NA_character_, "", "not a url",
            "https://example.com/a")
  for (fn in list(get_scheme, get_host, get_path, get_clean_url,
                  get_parse_status, get_domain, get_tld)) {
    expect_length(fn(urls), length(urls))
  }
  # Duplicates resolve identically: the unique-parse expansion must not
  # reorder or drop rows.
  expect_identical(get_host(urls)[1], get_host(urls)[5])
  # The invalid positions are NA on a component accessor, not dropped.
  expect_true(is.na(get_host(urls)[4]))
})

# ---- full-string: byte-level assertion on a whole serialized string ---------

# test-locale-invariance.R asserts exact octets on host/path/domain/tld -- i.e.
# on COMPONENTS. The output contract's ENC-2 cell asks for the guarantee on the
# serialized string itself, which nothing asserted. This is the full-string
# analogue: the whole clean_url, byte for byte.
test_that("clean_url is byte-exact for an IDN host", {
  u <- "https://bucher.example/p"
  expect_identical(charToRaw(get_clean_url(u)), charToRaw(get_clean_url(u)))

  idn <- "https://bücher.example/p"
  ascii <- get_clean_url(idn, host_encoding = "idna")
  expect_identical(ascii, "https://xn--bcher-kva.example/p")
  # Pure ASCII on the wire: the punycode form must carry no high bytes.
  expect_true(all(as.integer(charToRaw(ascii)) < 128L))
  expect_identical(
    charToRaw(ascii),
    charToRaw("https://xn--bcher-kva.example/p")
  )
})
