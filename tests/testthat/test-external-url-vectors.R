# External adversarial/conformance URL vectors imported from third-party suites
# (RURL-dbazixkr, epic RURL-wncwfasl — the R Journal paper's disagreement
# table). Unlike the hand-derived golden table in
# test-url-standard-conformance.R, these rows are transcribed from external
# sources and their oracle is the
# SOURCE's expectation, recorded per row in `standard` + `standard_expectation`
# (and, for class-C paper rows, `paper_claimed_behavior` kept SEPARATE from the
# standard's "should"). The fixture doubles as the results table: it records
# rurl's actual output in both profiles and a `diverges` flag vs the row oracle.
#
# Slice 1 seeds it with the WHATWG web-platform-tests `url/resources/
# urltestdata.json` FAILURE cases (BSD-3-Clause) — inputs the WHATWG URL parser
# MUST reject. rurl rejects all but a small, pinned set where its WHATWG profile
# still accepts (forbidden host code points surfaced as `warning-no-tld`). These
# are NOT declared correct-forever: each is triaged in the divergence ledger
# (_scratch/divergence-ledger.md) as documented-boundary vs candidate-bug — some
# (e.g. the UTS-46 soft-hyphen rows) are open bug candidates, not settled
# behavior. This test is a characterization/regression guard: it pins rurl's
# recorded output and the divergence set so BOTH a regression (a new must-fail
# input starts being accepted) AND progress (a tracked divergence gets fixed)
# force a deliberate, reviewed update here.

fixture_path <- testthat::test_path("fixtures", "external-url-vectors.csv")

read_vectors <- function() {
  utils::read.csv(
    fixture_path, stringsAsFactors = FALSE, colClasses = "character",
    na.strings = "NA"
  )
}

test_that("external-url-vectors fixture is well-formed", {
  fx <- read_vectors()
  expect_gt(nrow(fx), 0)
  expect_setequal(
    names(fx),
    c("id", "source_class", "source", "source_reference", "input",
      "input_json", "standard", "standard_expectation",
      "paper_claimed_behavior", "runnable", "rurl_rfc_status",
      "rurl_rfc_clean", "rurl_whatwg_status", "rurl_whatwg_clean",
      "diverges", "notes")
  )
  expect_identical(anyDuplicated(fx$id), 0L)
  expect_true(all(fx$source_class %in% c("A", "B", "C")))
  expect_true(all(fx$standard %in% c("rfc3986", "whatwg", "both")))
  # runnable rows carry an input; non-runnable rows are recorded for provenance
  # (relative-resolution / NUL-byte) with no rurl output.
  runnable <- fx$runnable == "yes"
  expect_false(anyNA(fx$input[runnable]))
  expect_true(all(is.na(fx$rurl_whatwg_status[!runnable])))
})

test_that("rurl output on external vectors matches recorded characterization", {
  fx <- read_vectors()
  fx <- fx[fx$runnable == "yes", , drop = FALSE]
  inputs <- fx$input

  rfc <- safe_parse_urls(inputs, url_standard = "rfc3986")
  wha <- safe_parse_urls(inputs, url_standard = "whatwg")

  expect_identical(rfc$parse_status, fx$rurl_rfc_status)
  expect_identical(rfc$clean_url, fx$rurl_rfc_clean)
  expect_identical(wha$parse_status, fx$rurl_whatwg_status)
  expect_identical(wha$clean_url, fx$rurl_whatwg_clean)
})

test_that("WPT failure divergences are pinned to the documented boundary set", {
  fx <- read_vectors()
  wpt <- fx[fx$source == "wpt-urltestdata" & fx$runnable == "yes", ,
    drop = FALSE]

  # WPT oracle is WHATWG: every runnable row MUST reject. rurl's WHATWG profile
  # rejects all but these 9 forbidden-host-code-point rows, tracked as open
  # divergences in the ledger (some documented-boundary, some candidate-bug).
  # This is a WATCH list, not an approval list: new acceptances (regressions) or
  # newly fixed rows must update it deliberately, with a ledger triage note.
  diverging_ids <- wpt$id[wpt$diverges == "yes"]
  expect_setequal(
    diverging_ids,
    c("wpt-fail-038", "wpt-fail-039", "wpt-fail-118", "wpt-fail-164",
      "wpt-fail-165", "wpt-fail-184", "wpt-fail-242", "wpt-fail-244",
      "wpt-fail-245")
  )
  # All diverging rows are the same documented shape: accepted as a reg-name
  # with warning-no-tld rather than hard-rejected.
  expect_true(all(wpt$rurl_whatwg_status[wpt$diverges == "yes"] ==
    "warning-no-tld"))
  # And every non-diverging runnable WPT row is genuinely rejected (NA clean).
  expect_true(all(is.na(wpt$rurl_whatwg_clean[wpt$diverges == "no"])))
})

test_that("IPv4-obfuscation divergences pin to the UTS-46 separator set", {
  fx <- read_vectors()
  ip <- fx[fx$source == "ip-obfuscation", , drop = FALSE]

  # These are hand-generated arithmetic encodings of a few target IPs
  # (obfuscation technique from cujanovic/SSRF-Testing + JorianWoltjer/ipobf,
  # NOT vendored). Oracle is the WHATWG IPv4 parser: octal/hex/dword/short/mixed
  # forms must canonicalize, overflow/zone-id forms must fail. rurl handles all
  # of those correctly (validating RURL-cdjnhnvf) EXCEPT the Unicode alternative
  # separators, which UTS-46 domain-to-ASCII should map to '.' before the IPv4
  # parse. rurl leaves them a literal reg-name -> a tracked candidate-bug in the
  # divergence ledger, pinned here as a WATCH list (not an approval).
  diverging_ids <- ip$id[ip$diverges == "yes"]
  expect_setequal(
    diverging_ids,
    c("ipobf-011", "ipobf-012", "ipobf-013")
  )
  # The divergence shape: accepted with warning-no-tld as a literal reg-name
  # instead of coercing to the canonical dotted-quad.
  expect_true(all(ip$rurl_whatwg_status[ip$diverges == "yes"] ==
    "warning-no-tld"))
  # Non-diverging accept rows must equal the recorded conformant WHATWG
  # serialization; the failure rows must genuinely reject (NA clean).
  ok_rows <- ip[ip$diverges == "no" & ip$standard_expectation != "failure", ,
    drop = FALSE]
  expect_identical(ok_rows$rurl_whatwg_clean, ok_rows$standard_expectation)
  fail_rows <- ip[ip$standard_expectation == "failure", , drop = FALSE]
  expect_true(all(is.na(fail_rows$rurl_whatwg_clean)))
})
