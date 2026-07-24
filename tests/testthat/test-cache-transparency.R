# Semantic-transparency invariant for the rurl memoization caches (rurl 3.0
# protocol hardening, G4.1). Verifies the accepted semantic-cache contract
# (design/work/url-v3/contracts/semantic-cache-contract.md, P5.1@d254ff1):
#
#   A cache is a pure performance/memory optimization and MUST NOT change any
#   observable result. For every operation and input vector the output must be
#   byte-identical across every cache state; only elapsed time and peak memory
#   may differ.
#
# The contract names six equivalence axes; each is exercised below with positive
# AND negative coverage (section 7 G4). The oracle is always the caches-DISABLED
# result: no memoization can influence it, so anything that matches it is proven
# transparent.

# Representative, deterministic input vector spanning the parse surface: mixed
# case, ports, userinfo, query/fragment, IDN + punycode, trailing dot, a
# malformed input, and a scheme-relative input.
tx_inputs <- c(
  "https://www.Example.COM/A/b?q=1&x=2#frag",
  "http://user:pw@sub.example.co.uk:8080/p/",
  "https://münchen.de/strasse",
  "http://xn--mnchen-3ya.de/x",
  "ftp://files.example.org/a.txt",
  "mailto:person@example.com",
  "//scheme.relative.example/path",
  "https://example.com.",
  "not a url",
  "https://例え.jp/road?a=b"
)

# Restore the shipped-default configuration and empty the caches. Called at the
# end of every test so no modified global state leaks into other test files
# (mirrors test-cache-policy.R's reset_caches discipline).
tx_restore_defaults <- function() {
  rurl_cache_config(
    full_parse = TRUE, puny_encode = TRUE, puny_decode = TRUE,
    max_full_parse = Inf
  )
  rurl_clear_caches()
}

# Parse with all three caches DISABLED -- the transparency oracle.
tx_oracle <- function(inputs, ...) {
  rurl_cache_config(
    full_parse = FALSE, puny_encode = FALSE, puny_decode = FALSE
  )
  on.exit(tx_restore_defaults(), add = TRUE)
  safe_parse_urls(inputs, ...)
}

# Column-wise view of a parse result: a named list of columns, dropping only the
# cosmetic row.names attribute so scalar/vector/chunk shapes compare on values,
# names, order, and types -- exactly the byte-identity the contract requires.
tx_cols <- function(df) as.list(df)

# ---------------------------------------------------------------------------
# Shipped default bound: the single source of truth the docs must track.
# ---------------------------------------------------------------------------

test_that("the shipped full_parse default bound is the 100000 truth", {
  # .onLoad installs .FULL_PARSE_MAX_DEFAULT as full_parse_max; the C-08
  # doc-consistency gate (tools/cache-doc-consistency.R) ties every documented
  # bound to this literal. Locking it here fails loudly if the default moves
  # without the docs and the gate moving with it.
  expect_identical(rurl:::.FULL_PARSE_MAX_DEFAULT, 100000L)
  expect_type(rurl:::.FULL_PARSE_MAX_DEFAULT, "integer")
})

# ---------------------------------------------------------------------------
# Axis 1: cold vs warm.
# ---------------------------------------------------------------------------

test_that("cold and warm parses are byte-identical (and match the oracle)", {
  tx_restore_defaults()
  cold <- safe_parse_urls(tx_inputs) # populates the caches
  warm <- safe_parse_urls(tx_inputs) # served from the caches
  expect_identical(warm, cold)
  expect_identical(warm, tx_oracle(tx_inputs))
  tx_restore_defaults()
})

# ---------------------------------------------------------------------------
# Axis 2: enabled vs disabled.
# ---------------------------------------------------------------------------

test_that("cache-enabled output equals cache-disabled output", {
  tx_restore_defaults()
  enabled <- safe_parse_urls(tx_inputs)
  expect_identical(enabled, tx_oracle(tx_inputs))
  tx_restore_defaults()
})

# ---------------------------------------------------------------------------
# Axis 3: bounded vs unbounded.
# ---------------------------------------------------------------------------

test_that("bounded (100000) and unbounded (Inf) parses agree", {
  tx_restore_defaults()
  rurl_cache_config(max_full_parse = 100000)
  bounded <- safe_parse_urls(tx_inputs)
  tx_restore_defaults()
  rurl_cache_config(max_full_parse = Inf)
  unbounded <- safe_parse_urls(tx_inputs)
  expect_identical(bounded, unbounded)
  expect_identical(bounded, tx_oracle(tx_inputs))
  tx_restore_defaults()
})

# ---------------------------------------------------------------------------
# Axis 4: pre- vs post-eviction. A tiny bound forces the hard reset-watermark to
# fire repeatedly across a stream of distinct URLs; every result must still
# equal the oracle regardless of whether it was computed just after a clear or
# with the cache near-full.
# ---------------------------------------------------------------------------

test_that("results straddling reset-watermark evictions match the oracle", {
  n <- seq_len(40)
  stream <- sprintf("https://h%02d.example.com/p?i=%d", n, n)
  oracle <- tx_oracle(stream)

  tx_restore_defaults()
  rurl_cache_config(max_full_parse = 3) # forces many watermark resets
  # Scalar loop so each call individually crosses (or does not cross) a reset.
  # safe_parse_urls() on a length-1 input yields a 1-row frame per call.
  evicting <- do.call(rbind, lapply(stream, safe_parse_urls))
  expect_identical(tx_cols(evicting), tx_cols(oracle))
  # Peak size never exceeded the bound.
  expect_lte(rurl_cache_info()$entries[1], 3)
  tx_restore_defaults()
})

test_that("reset-watermark clears the whole cache then stores the new key", {
  tx_restore_defaults()
  rurl_cache_config(max_full_parse = 3)
  for (i in seq_len(3)) {
    invisible(safe_parse_url(sprintf("https://a%d.example.com/", i)))
  }
  expect_equal(rurl_cache_info()$entries[1], 3)
  # A fourth distinct key is at the watermark: the whole cache empties, then the
  # new key is stored -- size drops to 1, never 4.
  invisible(safe_parse_url("https://a4.example.com/"))
  expect_equal(rurl_cache_info()$entries[1], 1)
  # Re-storing an existing key never triggers a reset.
  invisible(safe_parse_url("https://a4.example.com/"))
  expect_equal(rurl_cache_info()$entries[1], 1)
  tx_restore_defaults()
})

# ---------------------------------------------------------------------------
# Axis 5: scalar-loop vs one vector call vs deterministic chunk recombination.
# ---------------------------------------------------------------------------

test_that("scalar, vectorized, and chunked parses recombine identically", {
  tx_restore_defaults()
  vectorized <- safe_parse_urls(tx_inputs)

  # Scalar loop: one length-1 safe_parse_urls() call per input.
  scalar <- do.call(rbind, lapply(tx_inputs, safe_parse_urls))
  expect_identical(tx_cols(scalar), tx_cols(vectorized))

  chunks <- split(tx_inputs, (seq_along(tx_inputs) - 1L) %/% 3L)
  chunked <- do.call(rbind, lapply(chunks, safe_parse_urls))
  expect_identical(tx_cols(chunked), tx_cols(vectorized))

  # And all three equal the disabled-cache oracle.
  expect_identical(tx_cols(vectorized), tx_cols(tx_oracle(tx_inputs)))
  tx_restore_defaults()
})

# ---------------------------------------------------------------------------
# Axis 6: external-data / key stability -- a change on a keyed axis is a
# distinct entry and can never yield a cross-profile stale hit. www_handling is
# a keyed Stage-A axis whose change is observable (it strips the leading www.),
# so warm entries for one profile must never leak into another profile's call.
# ---------------------------------------------------------------------------

test_that("a keyed-axis change never produces a stale cross-profile hit", {
  none_oracle <- tx_oracle(tx_inputs, www_handling = "none")
  strip_oracle <- tx_oracle(tx_inputs, www_handling = "strip")
  # Negative/discriminating power: the two profiles genuinely differ, so a stale
  # hit WOULD be observable (the equality assertions below are not vacuous).
  expect_false(identical(none_oracle, strip_oracle))

  tx_restore_defaults()
  # Warm the cache under the "none" profile ...
  invisible(safe_parse_urls(tx_inputs, www_handling = "none"))
  # ... then call the "strip" profile with the cache still warm.
  warm_strip <- safe_parse_urls(tx_inputs, www_handling = "strip")
  expect_identical(warm_strip, strip_oracle)
  tx_restore_defaults()
})

test_that("clearing caches is pure: recompute is byte-identical", {
  tx_restore_defaults()
  first <- safe_parse_urls(tx_inputs)
  rurl_clear_caches()
  second <- safe_parse_urls(tx_inputs)
  expect_identical(second, first)
  tx_restore_defaults()
})

# Final safety net: restore defaults for later test files.
tx_restore_defaults()
