# T2 (RURL-mzbnubrx): the query-filter engine wired into safe_parse_url() /
# safe_parse_urls() -> clean_url, option validation in .parse_options(), the
# case_handling exemption, cache correctness across query profiles, and the
# canonical_join() consequence. The engine internals themselves are pinned in
# test-query-engine.R (T1); this file exercises the PUBLIC surface.

# --- Defaults preserve current output (backward compatibility) --------------

test_that("query_handling defaults to 'drop': clean_url stays query-free", {
  u <- "https://www.youtube.com/watch?v=yzRJDl5GQVg&utm_source=x"
  expect_equal(safe_parse_url(u)$clean_url, "https://www.youtube.com/watch")
  # The raw query field is the faithful original regardless of query_handling.
  expect_equal(safe_parse_url(u)$query, "v=yzRJDl5GQVg&utm_source=x")

  df <- safe_parse_urls(u)
  expect_equal(df$clean_url, "https://www.youtube.com/watch")
  expect_equal(df$query, "v=yzRJDl5GQVg&utm_source=x")
})

test_that("a URL with no query is unaffected by any mode", {
  u <- "https://example.com/path"
  for (m in c("drop", "filter", "allow", "keep")) {
    expect_equal(
      safe_parse_url(u, query_handling = m, params_keep = "v")$clean_url,
      "https://example.com/path"
    )
  }
})

# --- Modes on the public surface --------------------------------------------

test_that("query_handling modes shape clean_url (scalar + vector agree)", {
  u <- "https://ex.com/p?a=1&utm_source=x&gclid=9"

  expect_equal(safe_parse_url(u, query_handling = "drop")$clean_url,
    "https://ex.com/p")
  expect_equal(safe_parse_url(u, query_handling = "filter")$clean_url,
    "https://ex.com/p?a=1")
  expect_equal(safe_parse_url(u, query_handling = "keep")$clean_url,
    "https://ex.com/p?a=1&utm_source=x&gclid=9")
  expect_equal(
    safe_parse_url(u, query_handling = "allow", params_keep = "a")$clean_url,
    "https://ex.com/p?a=1"
  )

  # safe_parse_urls() produces the identical clean_url.
  for (m in c("drop", "filter", "keep")) {
    expect_equal(
      safe_parse_urls(u, query_handling = m)$clean_url,
      safe_parse_url(u, query_handling = m)$clean_url
    )
  }
})

test_that("YouTube ?v= survives 'filter'; utm_* does not", {
  u <- "https://www.youtube.com/watch?v=yzRJDl5GQVg&utm_source=news&fbclid=1"
  expect_equal(
    safe_parse_url(u, query_handling = "filter")$clean_url,
    "https://www.youtube.com/watch?v=yzRJDl5GQVg"
  )
})

test_that("params_drop extends the denylist; params_keep rescues in 'filter'", {
  u <- "https://ex.com/p?a=1&ref=2"
  expect_equal(
    safe_parse_url(u, query_handling = "filter", params_drop = "ref")$clean_url,
    "https://ex.com/p?a=1"
  )
  # rescue a denylisted tracker.
  u2 <- "https://ex.com/p?utm_source=x&a=1"
  expect_equal(
    safe_parse_url(
      u2, query_handling = "filter", params_keep = "utm_source"
    )$clean_url,
    "https://ex.com/p?utm_source=x&a=1"
  )
})

test_that("sort_params / empty_param_handling / decode_plus thread through", {
  # Stable sort by decoded key.
  expect_equal(
    safe_parse_url(
      "https://ex.com/p?b=2&a=1", query_handling = "keep", sort_params = TRUE
    )$clean_url,
    "https://ex.com/p?a=1&b=2"
  )
  # empty_param_handling = "drop" removes empty-valued params.
  expect_equal(
    safe_parse_url(
      "https://ex.com/p?a=1&ref=", query_handling = "keep",
      empty_param_handling = "drop"
    )$clean_url,
    "https://ex.com/p?a=1"
  )
  # decode_plus = TRUE turns '+' in values into %20.
  expect_equal(
    safe_parse_url(
      "https://ex.com/p?q=a+b", query_handling = "keep", decode_plus = TRUE
    )$clean_url,
    "https://ex.com/p?q=a%20b"
  )
  # decode_plus = FALSE (default) keeps '+' literal (re-encoded to %2B).
  expect_equal(
    safe_parse_url("https://ex.com/p?q=a+b", query_handling = "keep")$clean_url,
    "https://ex.com/p?q=a%2Bb"
  )
})

test_that("params_case_sensitive controls denylist matching", {
  u <- "https://ex.com/p?UTM_SOURCE=x&a=1"
  # Case-insensitive (default): the upper-case tracker is dropped.
  expect_equal(
    safe_parse_url(u, query_handling = "filter")$clean_url,
    "https://ex.com/p?a=1"
  )
  # Case-sensitive: UTM_SOURCE no longer matches the lower-case denylist entry.
  expect_equal(
    safe_parse_url(
      u, query_handling = "filter", params_case_sensitive = TRUE
    )$clean_url,
    "https://ex.com/p?UTM_SOURCE=x&a=1"
  )
})

# --- case_handling exemption (regression) -----------------------------------

test_that("query is exempt from case_handling ('lower' and 'upper')", {
  u <- "https://www.EXAMPLE.com/PATH?Token=AbC"
  # lower folds scheme/host/path but NOT the query.
  lower <- safe_parse_url(u, query_handling = "keep", case_handling = "lower")
  expect_equal(lower$clean_url, "https://www.example.com/path?Token=AbC")
  # upper folds scheme/host/path but NOT the query.
  upper <- safe_parse_url(u, query_handling = "keep", case_handling = "upper")
  expect_equal(upper$clean_url, "HTTPS://WWW.EXAMPLE.COM/PATH?Token=AbC")
  # lower_host (default) leaves path and query alone.
  expect_equal(
    safe_parse_url(u, query_handling = "keep")$clean_url,
    "https://www.example.com/PATH?Token=AbC"
  )
})

# --- Cache correctness (no stale memo across query profiles) ----------------

test_that("same URL, different query opts -> different clean_url (no stale)", {
  rurl_clear_caches()
  u <- "https://ex.com/p?id=1&utm_source=x"
  # Prime the cache with the default (drop).
  a <- safe_parse_url(u)$clean_url
  # Re-request the SAME url with different query options within the session.
  b <- safe_parse_url(u, query_handling = "filter")$clean_url
  cc <- safe_parse_url(u, query_handling = "keep")$clean_url

  expect_equal(a, "https://ex.com/p")
  expect_equal(b, "https://ex.com/p?id=1")
  expect_equal(cc, "https://ex.com/p?id=1&utm_source=x")

  # The query-bearing outputs are all distinct from the primed drop default.
  expect_false(a == b)
  expect_false(b == cc)

  # The batch path is equally immune to a primed drop entry.
  expect_equal(
    safe_parse_urls(u, query_handling = "filter")$clean_url, b
  )
})

# --- Option validation ------------------------------------------------------

test_that("invalid query options error", {
  expect_error(
    safe_parse_url("https://ex.com/?a=1", query_handling = "nope"),
    "should be one of"
  )
  expect_error(
    safe_parse_url("https://ex.com/?a=1", empty_param_handling = "nope"),
    "should be one of"
  )
  expect_error(
    safe_parse_url("https://ex.com/?a=1", params_keep = 42),
    "params_keep must be NULL or a character vector"
  )
  expect_error(
    safe_parse_url("https://ex.com/?a=1", params_drop = list("a")),
    "params_drop must be NULL or a character vector"
  )
  expect_error(
    safe_parse_url("https://ex.com/?a=1", sort_params = "yes"),
    "sort_params must be a single logical"
  )
  expect_error(
    safe_parse_url("https://ex.com/?a=1", params_case_sensitive = NA),
    "params_case_sensitive must be a single logical"
  )
  expect_error(
    safe_parse_url("https://ex.com/?a=1", decode_plus = c(TRUE, FALSE)),
    "decode_plus must be a single logical"
  )
})

# --- canonical_join() consequence -------------------------------------------

test_that("canonical_join is unchanged under the default 'drop'", {
  A <- data.frame(
    URL = c("https://ex.com/p?id=1", "https://ex.com/p?id=2"),
    a = 1:2, stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "https://ex.com/p?id=1&utm_source=x", b = 99,
    stringsAsFactors = FALSE
  )
  j <- canonical_join(A, B)
  # Query-free join key: the id-only differences collapse to one key.
  expect_equal(unique(j$JoinKey), "https://ex.com/p")
})

test_that("under 'filter' the query enters the join key (?id splits)", {
  A <- data.frame(
    URL = c("https://ex.com/p?id=1", "https://ex.com/p?id=2"),
    a = 1:2, stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "https://ex.com/p?id=1&utm_source=x", b = 99,
    stringsAsFactors = FALSE
  )
  j <- canonical_join(A, B, query_handling = "filter")
  # B's utm-only difference collapses onto id=1; id=2 does NOT match.
  expect_equal(nrow(j), 1L)
  expect_equal(j$JoinKey, "https://ex.com/p?id=1")
  expect_equal(j$a_A, 1L)
  expect_equal(j$b_B, 99)
})
