# canonical_join()'s legacy presentation/cleaning dial warning.
#
# Governing decision: design/work/url-v3/decisions/
#   P3.1-identity-canonical-join.md
#   D-E.1 "The unrestricted `...` that currently makes every cleaning/display
#          dial an equality dial is closed: comparison-irrelevant
#          cleaning/display arguments warn."
#   D-E.3 "No caller is silently re-matched."
# and design/work/url-v3/contracts/key-join-contracts.md, "forwarded parse
# dials" / "forwarded display/cleaning dials" rows.
#
# The per-dial classification is the `key-affecting?` column of
# design/work/url-v3/contracts/cleaning-mutation-contracts.md
# section "cleaning-semantics" (all 25 shipped dials, every relevant row
# SETTLED). Row numbers below refer to that table.
#
# This slice adds the WARNING ONLY. The join key, the match set, and every
# returned value stay byte-identical -- that is what the invariance block at
# the bottom of this file proves.

make_a <- function() {
  data.frame(
    URL = c("http://www.Example.com/A%2fB/", "https://Example.com/x"),
    ValA = 1:2, stringsAsFactors = FALSE
  )
}
make_b <- function() {
  data.frame(
    URL = c("http://www.example.com/A%2FB/", "https://www.example.com/x"),
    ValB = c("p", "q"), stringsAsFactors = FALSE
  )
}

# --- The condition itself ----------------------------------------------------

test_that("a presentation dial forwarded through `...` warns", {
  A <- make_a()
  B <- make_b()
  expect_warning(
    canonical_join(A, B, www_handling = "strip"),
    class = "rurl_legacy_join_dial_warning"
  )
})

test_that("the warning fires ONCE per call, not once per parsed side", {
  A <- make_a()
  B <- make_b()
  # canonical_join() calls safe_parse_urls() twice (one per side); the warning
  # is emitted from the argument-policy block above them, so it must not
  # double up.
  warnings_seen <- testthat::capture_warnings(
    canonical_join(A, B, www_handling = "strip")
  )
  expect_length(warnings_seen, 1L)
})

test_that("one warning names every offending dial", {
  A <- make_a()
  B <- make_b()
  warnings_seen <- testthat::capture_warnings(
    canonical_join(
      A, B,
      www_handling = "strip", path_encoding = "decode", port_handling = "keep"
    )
  )
  expect_length(warnings_seen, 1L)
  expect_match(warnings_seen, "www_handling", fixed = TRUE)
  expect_match(warnings_seen, "path_encoding", fixed = TRUE)
  expect_match(warnings_seen, "port_handling", fixed = TRUE)
  # It must say what the classification MEANS, not merely that it happened.
  expect_match(
    warnings_seen, "do not participate in URL identity", fixed = TRUE
  )
  expect_match(warnings_seen, "legacy", fixed = TRUE)
  expect_match(warnings_seen, "change which rows match", fixed = TRUE)
})

test_that("the condition is classed, so it can be muted selectively", {
  A <- make_a()
  B <- make_b()
  expect_silent(
    suppressWarnings(
      canonical_join(A, B, www_handling = "strip"),
      classes = "rurl_legacy_join_dial_warning"
    )
  )
})

# --- Classification: what stays silent ---------------------------------------

test_that("input and interpretation axes stay silent", {
  A <- make_a()
  B <- make_b()
  # cleaning-semantics rows 9 / 21 / 22 / 23 are the table's four BOUNDARY
  # rows: input-acceptance and interpretation axes, "governed as input, not by
  # cleaning". They are legitimate inputs to identity and must not warn.
  expect_silent(canonical_join(A, B, url_standard = "rfc3986"))    # row 23
  expect_silent(canonical_join(A, B, scheme_acceptance = "web"))   # row 22
  expect_silent(canonical_join(A, B, scheme_policy = "infer"))     # row 21
  expect_silent(
    canonical_join(A, B, scheme_relative_handling = "https")       # row 9
  )
  expect_silent(
    canonical_join(
      A, B,
      url_standard = "rfc3986", scheme_acceptance = "web",
      scheme_policy = "infer", scheme_relative_handling = "keep"
    )
  )
})

test_that("every cleaning dial marked key-affecting `no` warns", {
  A <- make_a()
  B <- make_b()
  # One live call per warning dial reachable through this seam, so the warn
  # set cannot silently narrow. Rows are cleaning-semantics table rows.
  reachable <- list(
    protocol_handling = "strip",         # row 1
    www_handling = "strip",              # row 2
    tld_source = "icann",                # row 4
    case_handling = "lower",             # row 5
    trailing_slash_handling = "strip",   # row 6
    index_page_handling = "strip",       # row 7
    path_normalization = "both",         # row 8
    subdomain_levels_to_keep = 0L,       # row 10
    host_encoding = "idna",              # row 11
    path_encoding = "decode",            # row 12
    query_handling = "filter",           # rows 13-19
    params_keep = "id",
    params_drop = "utm_source",
    params_case_sensitive = TRUE,
    sort_params = TRUE,
    empty_param_handling = "drop",
    decode_plus = TRUE,
    port_handling = "keep",              # row 20
    profile = "seo"                      # row 25
  )
  for (dial in names(reachable)) {
    # name_A / name_B explicitly: under do.call, deparse(substitute()) would
    # otherwise try to name the columns after the whole data frame literal.
    args <- list(A, B, name_A = "A", name_B = "B")
    args[[dial]] <- reachable[[dial]]
    expect_warning(
      do.call(canonical_join, args),
      class = "rurl_legacy_join_dial_warning",
      label = dial
    )
  }
})

test_that("the PSL engine dial warns (cleaning-semantics row 24)", {
  A <- make_a()
  B <- make_b()
  # engine is "clean/presentation input ... must not alter identity".
  # engine = NULL is the documented no-op default and is byte-identical to
  # omitting it, so it exercises the classification without changing results.
  expect_warning(
    canonical_join(A, B, engine = NULL),
    class = "rurl_legacy_join_dial_warning"
  )
})

test_that("`source` is classified even though the seam cannot reach it", {
  A <- make_a()
  B <- make_b()
  # Row 3 `source` is the PSL-section dial's canonical name; row 4
  # `tld_source` is its deprecated alias. Only `tld_source` is a formal of
  # safe_parse_urls() -- `source` lives on the accessors -- so forwarding
  # `source` still fails downstream. It is classified anyway, so the list
  # stays correct if the seam ever widens.
  expect_identical(rurl:::.cj_classify_dots("source"), "source")
  expect_error(
    suppressWarnings(
      canonical_join(A, B, source = "icann"),
      classes = "rurl_legacy_join_dial_warning"
    ),
    "unused argument"
  )
})

test_that("a plain call with no forwarded dials stays silent", {
  expect_silent(canonical_join(make_a(), make_b()))
})

test_that("the classifier partitions supplied names, not values", {
  expect_identical(
    rurl:::.cj_classify_dots(
      c("www_handling", "url_standard", "engine", "scheme_relative_handling")
    ),
    c("www_handling", "engine")
  )
  # No dots at all, and unnamed dots, classify to nothing.
  expect_identical(rurl:::.cj_classify_dots(NULL), character(0))
  expect_identical(rurl:::.cj_classify_dots(""), character(0))
  # Every query cleaning dial is covered (P3.1 D-A.3 "every query cleaning
  # dial"), as is the whole profile bundle.
  query_dials <- c(
    "query_handling", "params_keep", "params_drop", "sort_params",
    "empty_param_handling", "params_case_sensitive", "decode_plus"
  )
  expect_identical(rurl:::.cj_classify_dots(query_dials), query_dials)
  expect_identical(rurl:::.cj_classify_dots("profile"), "profile")
})

# --- Hard errors still preempt the warning -----------------------------------

test_that("a governed-knob conflict errors without also warning", {
  A <- make_a()
  B <- make_b()
  # The conflict matrix runs first: a call that cannot proceed errors rather
  # than warning about dials it will never use.
  expect_error(
    canonical_join(A, B, url_standard = "whatwg", case_handling = "upper"),
    "governs `case_handling`"
  )
  expect_error(
    canonical_join(A, B, profile = "nope"),
    "profile must be NULL or one of"
  )
})

# --- D-E.3: the warning is additive, results are byte-identical --------------

test_that("path_encoding results are unchanged by the warning", {
  # Values captured from the shipped (pre-warning) implementation.
  joined <- cj_legacy(canonical_join(
    make_a(), make_b(),
    name_A = "A", name_B = "B", path_encoding = "decode", join = "full"
  ))
  expect_identical(
    joined$A,
    c("http://www.Example.com/A%2fB/", "https://Example.com/x", NA)
  )
  expect_identical(
    joined$B,
    c("http://www.example.com/A%2FB/", NA, "https://www.example.com/x")
  )
  expect_identical(
    joined$JoinKey,
    c(
      "http://www.example.com/A/B/", "https://example.com/x",
      "https://www.example.com/x"
    )
  )
  expect_identical(joined$ValA_A, c(1L, 2L, NA))
  expect_identical(joined$ValB_B, c("p", NA, "q"))
})

test_that("www_handling results are unchanged by the warning", {
  # Values captured from the shipped (pre-warning) implementation. Note this
  # dial MOVES the match set (3 rows -> 2): that is the legacy behavior the
  # warning surfaces, and it must be preserved, not corrected, in this slice.
  joined <- cj_legacy(canonical_join(
    make_a(), make_b(),
    name_A = "A", name_B = "B", www_handling = "strip", join = "full"
  ))
  expect_identical(
    joined$A,
    c("http://www.Example.com/A%2fB/", "https://Example.com/x")
  )
  expect_identical(
    joined$B,
    c("http://www.example.com/A%2FB/", "https://www.example.com/x")
  )
  expect_identical(
    joined$JoinKey,
    c("http://example.com/A%2FB/", "https://example.com/x")
  )
  expect_identical(joined$ValA_A, 1:2)
  expect_identical(joined$ValB_B, c("p", "q"))
})

test_that("path_normalization results are unchanged by the warning", {
  # Captured by running the pre-change HEAD definition of canonical_join()
  # against these exact inputs, not by re-recording post-change output.
  # path_normalization is one of the counter-pinned dials a later unit
  # depends on, so its results-unchanged property is pinned explicitly here.
  joined <- cj_legacy(canonical_join(
    make_a(), make_b(),
    name_A = "A", name_B = "B", path_normalization = "both", join = "full"
  ))
  expect_identical(
    joined$A,
    c("http://www.Example.com/A%2fB/", "https://Example.com/x", NA)
  )
  expect_identical(
    joined$B,
    c("http://www.example.com/A%2FB/", NA, "https://www.example.com/x")
  )
  expect_identical(
    joined$JoinKey,
    c(
      "http://www.example.com/A%2FB/", "https://example.com/x",
      "https://www.example.com/x"
    )
  )
  expect_identical(joined$ValA_A, c(1L, 2L, NA))
  expect_identical(joined$ValB_B, c("p", NA, "q"))
})

test_that("protocol_handling results are unchanged by the warning", {
  # Values captured from the shipped (pre-warning) implementation.
  joined <- cj_legacy(canonical_join(
    make_a(), make_b(),
    name_A = "A", name_B = "B", protocol_handling = "strip", join = "full"
  ))
  expect_identical(
    joined$JoinKey,
    c("www.example.com/A%2FB/", "example.com/x", "www.example.com/x")
  )
  expect_identical(joined$ValA_A, c(1L, 2L, NA))
  expect_identical(joined$ValB_B, c("p", NA, "q"))
})
