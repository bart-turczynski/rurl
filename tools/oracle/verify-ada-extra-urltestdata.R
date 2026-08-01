#!/usr/bin/env Rscript
# RURL-ozdejfzl -- prove the 24 `ada-extra-urltestdata` rows re-locate upstream.
#
# WHAT THIS DISCHARGES. The group's `generation_command` was
# MISSING[RURL-vwurxmzm], and the scratch builder that produced it,
# _scratch/build-ada-vectors.R, resolved its input through an absolute path into
# a dead session scratchpad -- un-runnable not merely from a clean checkout but
# on the machine that wrote it. Both Ada JSONs were gone from this machine when
# the port began; both were re-fetched and both hash-match the recorded digests
# exactly, which is what made this slice possible at all.
#
# THE PINNED REVISION DOES NOT REPRODUCE THIS BLOCK, AND THAT IS THE POINT.
# Upstream commit fbea5b01 (2026-07-17, ada #1186 "fix no scheme state accepting
# any input containing a fragment") changed one entry's expectation and added
# three more. The record's pin is a VERIFIED-AT pin taken after that commit, so:
#
#   - all 24 inputs re-locate at the pin, in order;
#   - ONE expected value does not (ada-024, `..#`, recorded a:b/# and now
#     `failure` upstream);
#   - FOUR upstream entries are outside the committed block, from two different
#     causes -- three added by that commit, one ("" with base about:blank) that
#     predates the import and is absent for an unrecorded reason.
#
# Both deltas are carried here as EXACT LEDGERS rather than tolerances. An
# inexact ledger ("allow up to N disagreements") would absorb the next drift
# silently, which is precisely the failure this file exists to make loud: a
# ledger entry that stops being true is as much a failure as a new one.
#
# THE SECOND ANCHOR IS WHAT MAKES THE LEDGER HONEST. A ledger alone says "we
# know about this one" without evidence that the recorded value was ever right.
# So the gate also grades the block against the revision at which it DOES
# reproduce -- and that revision was not guessed: every one of the 17 commits
# that ever touched this path was swept, and exactly one, aa8e4043 (2025-07-16),
# reproduces 24/24. Every earlier revision reproduces fewer and every later one
# 23/24, which bounds the unrecorded import to [2025-07-16, 2026-07-17) without
# inventing a retrieval date.
#
# Usage:
#   Rscript tools/oracle/verify-ada-extra-urltestdata.R
#   Rscript tools/oracle/verify-ada-extra-urltestdata.R --self-test
#
# Exit 0 = the committed rows re-locate; 1 = they do not; 2 = source unavailable.

FIXTURE <- "tests/testthat/fixtures/external-url-vectors.csv"
GROUP <- "ada-extra-urltestdata"

here <- function(...) file.path("tools", "oracle", ...)
source(here("fetch-source.R"), local = FALSE)
source(here("relocate-wpt-format.R"), local = FALSE)

# This group runs every row it can, so its `runnable` column spells the
# base-relative reason as a bare "no". A NUL-bearing row would need a label this
# group has never had, so the classifier reaching that reason is a failure.
RUNNABLE_LABEL <- c(runnable = "yes", `base-relative` = "no")

# ---- the two ledgers ---------------------------------------------------------
#
# DRIFT: rows whose committed expectation is NOT what the pinned revision says,
# with the reason. Exact in both directions.
DRIFT_LEDGER <- list(
  list(id = "ada-024", input = "..#",
       committed = "a:b/#", upstream_at_pin = "failure",
       cause = paste("upstream commit fbea5b017d72a1804863fb0c73fbb278c40f8f3b",
                     "(2026-07-17, ada #1186) re-expected this case as a",
                     "failure; the committed row records the href it carried",
                     "when the block was imported"))
)

# UPSTREAM-ONLY: entries present at the pinned revision that the committed block
# does not carry. Two causes, and they are not interchangeable -- three arrived
# after the import, one was there and was dropped.
UPSTREAM_ONLY_LEDGER <- list(
  list(input = "", cause = paste(
    "PREDATES THE IMPORT and is absent for an UNRECORDED reason. It is present",
    "at aa8e4043, the revision this block reproduces at, so the builder saw it",
    "and did not take it. An empty input is also the one value the fixture's",
    "`input` column cannot distinguish from NA, which is a plausible reason and",
    "is NOT recorded anywhere, so it is not claimed here.")),
  list(input = "//evil.com/p#", cause = "added by fbea5b01, after the import"),
  list(input = "x#f", cause = "added by fbea5b01, after the import"),
  list(input = "#f", cause = "added by fbea5b01, after the import")
)

# The revision at which the committed expectations DO reproduce, established by
# sweeping all 17 commits that ever touched this path rather than by assumption.
REPRODUCING <- list(
  project = "ada-url/ada",
  revision = "aa8e4043819f44c1b62c931fc9e6fde2284d85ac",
  date = "2025-07-16",
  path = "tests/wpt/ada_extra_urltestdata.json",
  sha256 = "8fbbdea58dd8e45f5f9f63a8768cdd8d4fd0c32ee290f48fcf5f736fae2b29b2"
)

read_fixture <- function(path) {
  if (!file.exists(path)) stop("FATAL: fixture not found: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = "NA")
}

# ---- checks -----------------------------------------------------------------

# Check A -- ORDERED RE-LOCATION at the pinned revision. Every committed input
# must appear upstream, and the committed order must be the upstream order with
# the ledgered entries removed. Order matters for the same reason it does in the
# WPT group: the ada-NNN ids are positional.
check_relocation <- function(committed, upstream, ledger = UPSTREAM_ONLY_LEDGER) {
  fail <- character(0)
  ledgered <- vapply(ledger, `[[`, character(1), "input")
  expected_order <- upstream$input[!(upstream$input %in% ledgered)]

  gone <- setdiff(committed$input, upstream$input)
  if (length(gone)) {
    fail <- c(fail, sprintf(
      "%d committed input(s) do not appear at the pinned revision at all: %s",
      length(gone), paste(encodeString(gone), collapse = ", ")))
  }
  if (length(expected_order) != nrow(committed)) {
    fail <- c(fail, sprintf(
      "upstream carries %d non-ledgered entr(ies), the committed block has %d",
      length(expected_order), nrow(committed)))
  }
  n <- min(length(expected_order), nrow(committed))
  if (n) {
    bad <- which(committed$input[seq_len(n)] != expected_order[seq_len(n)])
    if (length(bad)) {
      fail <- c(fail, sprintf(
        "%d row(s) do not re-locate at the same position upstream:", length(bad)))
      for (i in utils::head(bad, 10L)) {
        fail <- c(fail, sprintf("    %s\n      committed=%s\n      upstream =%s",
                                committed$id[i], encodeString(committed$input[i]),
                                encodeString(expected_order[i])))
      }
    }
  }
  fail
}

# Check B -- THE UPSTREAM-ONLY LEDGER IS EXACT. Every upstream entry outside the
# committed block must be ledgered, and every ledger row must still be outside
# it. A new upstream case that nobody looked at is the thing this catches.
check_upstream_only <- function(committed, upstream, ledger = UPSTREAM_ONLY_LEDGER) {
  fail <- character(0)
  ledgered <- vapply(ledger, `[[`, character(1), "input")
  actual <- setdiff(upstream$input, committed$input)
  unledgered <- setdiff(actual, ledgered)
  stale <- setdiff(ledgered, actual)
  if (length(unledgered)) {
    fail <- c(fail, sprintf(
      paste0("%d upstream entr(ies) are outside the committed block and are NOT ",
             "ledgered -- upstream grew a case nobody triaged: %s"),
      length(unledgered), paste(encodeString(unledgered), collapse = ", ")))
  }
  if (length(stale)) {
    fail <- c(fail, sprintf(
      paste0("%d ledger row(s) claim an upstream entry is outside the block, ",
             "but it is not: %s"),
      length(stale), paste(encodeString(stale), collapse = ", ")))
  }
  fail
}

# Check C -- EXPECTATIONS AT THE PIN, WITH THE DRIFT LEDGER APPLIED EXACTLY.
# A disagreement that is not ledgered fails; a ledger row that no longer
# disagrees, or that disagrees differently, also fails.
check_expectations_at_pin <- function(committed, upstream, ledger = DRIFT_LEDGER) {
  fail <- character(0)
  idx <- match(committed$input, upstream$input)
  drift_by_id <- stats::setNames(ledger,
                                 vapply(ledger, `[[`, character(1), "id"))
  seen <- character(0)
  for (i in seq_len(nrow(committed))) {
    if (is.na(idx[i])) next
    got <- upstream$expected[idx[i]]
    want <- committed$standard_expectation[i]
    entry <- drift_by_id[[committed$id[i]]]
    if (identical(got, want)) {
      if (!is.null(entry)) {
        fail <- c(fail, sprintf(
          paste0("%s is ledgered as drifted, but it now AGREES with upstream ",
                 "(%s) -- the ledger row is stale and must be retired"),
          committed$id[i], encodeString(got)))
      }
      next
    }
    if (is.null(entry)) {
      fail <- c(fail, sprintf(
        "%s disagrees with the pinned revision and is NOT ledgered:\n      committed=%s\n      upstream =%s",
        committed$id[i], encodeString(want), encodeString(got)))
      next
    }
    seen <- c(seen, committed$id[i])
    if (!identical(entry$committed, want) ||
        !identical(entry$upstream_at_pin, got)) {
      fail <- c(fail, sprintf(
        paste0("%s drifts, but not as the ledger records it:\n",
               "      ledger says committed=%s upstream=%s\n",
               "      actually   committed=%s upstream=%s"),
        committed$id[i], encodeString(entry$committed),
        encodeString(entry$upstream_at_pin), encodeString(want),
        encodeString(got)))
    }
  }
  missed <- setdiff(names(drift_by_id), seen)
  if (length(missed)) {
    fail <- c(fail, sprintf(
      "%d drift-ledger row(s) never matched a committed row: %s",
      length(missed), paste(missed, collapse = ", ")))
  }
  fail
}

# Check D -- THE SECOND ANCHOR. At the revision the sweep identified, all 24
# committed expectations must reproduce EXACTLY, with no ledger. This is what
# turns "we know about that one drifted row" into evidence that the recorded
# value was correct when it was taken.
check_reproducing_revision <- function(committed) {
  pin <- oracle_source_pin_literal(
    sprintf("%s (expectation-reproducing revision)", GROUP),
    REPRODUCING$project, REPRODUCING$revision, REPRODUCING$path,
    REPRODUCING$sha256)
  upstream <- wpt_upstream_frame(oracle_source_cases(pin))

  fail <- character(0)
  idx <- match(committed$input, upstream$input)
  if (anyNA(idx)) {
    fail <- c(fail, sprintf(
      "%d committed input(s) do not appear at the reproducing revision %s: %s",
      sum(is.na(idx)), substr(REPRODUCING$revision, 1L, 12L),
      paste(encodeString(committed$input[is.na(idx)]), collapse = ", ")))
    return(fail)
  }
  bad <- which(upstream$expected[idx] != committed$standard_expectation)
  if (length(bad)) {
    fail <- c(fail, sprintf(
      paste0("%d expectation(s) do NOT reproduce at %s, which the record names ",
             "as the revision they reproduce at:"),
      length(bad), substr(REPRODUCING$revision, 1L, 12L)))
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  committed=%s  upstream=%s",
                              committed$id[i],
                              encodeString(committed$standard_expectation[i]),
                              encodeString(upstream$expected[idx[i]])))
    }
  }
  fail
}

# Check E -- THE RUNNABLE CLASSIFICATION RE-DERIVES. This is the corpus that
# exercises the about:blank rule: 21 of the 24 rows carry an about:blank base
# and 20 of them still run, so a classifier that treated any base as
# disqualifying would fail here -- which is exactly the mutation the
# wpt-urltestdata corpus was measured NOT to catch.
check_runnable <- function(committed, upstream) {
  fail <- character(0)
  idx <- match(committed$input, upstream$input)
  ok <- !is.na(idx)
  if (!any(ok)) return(fail)
  want <- unname(RUNNABLE_LABEL[upstream$reason[idx[ok]]])
  if (anyNA(want)) {
    fail <- c(fail, sprintf(
      "the classifier produced %d reason(s) this group has no label for: %s",
      sum(is.na(want)),
      paste(unique(upstream$reason[idx[ok]][is.na(want)]), collapse = ", ")))
    return(fail)
  }
  bad <- which(committed$runnable[ok] != want)
  if (length(bad)) {
    fail <- c(fail, sprintf("%d row(s) are classified differently:", length(bad)))
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  recorded=%s  derived=%s  input=%s",
                              committed$id[ok][i], committed$runnable[ok][i],
                              want[i], encodeString(committed$input[ok][i])))
    }
  }
  fail
}

# ---- self-test --------------------------------------------------------------

self_test <- function() {
  pass <- 0L
  fail <- character(0)
  expect <- function(label, got, want) {
    if (identical(got, want)) pass <<- pass + 1L
    else fail <<- c(fail, sprintf("%s: got %s, want %s", label,
                                  encodeString(as.character(got)),
                                  encodeString(as.character(want))))
  }

  # The checks take their ledger as an argument so the self-test can grade the
  # LOGIC against a two-row ledger; main() always passes the real ones.
  drift1 <- list(list(id = "ada-024", input = "..#", committed = "a:b/#",
                      upstream_at_pin = "failure", cause = "synthetic"))
  only1 <- list(list(input = "#f", cause = "synthetic"))

  fx <- function(id, input, expected, runnable = "yes") {
    data.frame(id = id, input = input, standard_expectation = expected,
               runnable = runnable, stringsAsFactors = FALSE)
  }
  up <- function(...) wpt_upstream_frame(list(...))

  # The drift ledger, in all four of its states.
  committed <- rbind(fx("ada-001", "a:1", "a:1"), fx("ada-024", "..#", "a:b/#"))
  upstream <- up(list(input = "a:1", href = "a:1"),
                 list(input = "..#", base = "a:b", failure = TRUE))
  expect("a ledgered drift passes",
         length(check_expectations_at_pin(committed, upstream, drift1)), 0L)

  drifted <- rbind(fx("ada-001", "a:1", "a:2"), fx("ada-024", "..#", "a:b/#"))
  expect("an unledgered drift fails",
         length(check_expectations_at_pin(drifted, upstream, drift1)) > 0L, TRUE)

  agreed <- rbind(fx("ada-001", "a:1", "a:1"), fx("ada-024", "..#", "failure"))
  expect("a stale ledger row fails",
         length(check_expectations_at_pin(agreed, upstream, drift1)) > 0L, TRUE)

  moved <- up(list(input = "a:1", href = "a:1"),
              list(input = "..#", base = "a:b", href = "a:b#"))
  expect("a drift that moved AGAIN fails",
         length(check_expectations_at_pin(committed, moved, drift1)) > 0L, TRUE)

  # The upstream-only ledger, both directions.
  known <- up(list(input = "a:1", href = "a:1"),
              list(input = "..#", base = "a:b", failure = TRUE),
              list(input = "#f", base = "a:b", href = "a:b#f"))
  expect("a ledgered upstream-only entry passes",
         length(check_upstream_only(committed, known, only1)), 0L)
  grew <- up(list(input = "a:1", href = "a:1"),
             list(input = "..#", base = "a:b", failure = TRUE),
             list(input = "brand:new", href = "brand:new"))
  expect("an unledgered upstream entry fails",
         length(check_upstream_only(committed, grew, only1)) > 0L, TRUE)
  shrank <- up(list(input = "a:1", href = "a:1"),
               list(input = "..#", base = "a:b", failure = TRUE))
  expect("a ledger row that is no longer outside the block fails",
         length(check_upstream_only(committed, shrank, only1)) > 0L, TRUE)

  # Order, which is what the ada-NNN ids depend on.
  swapped <- rbind(fx("ada-024", "..#", "a:b/#"), fx("ada-001", "a:1", "a:1"))
  expect("a permutation fails",
         length(check_relocation(swapped, known, only1)) > 0L, TRUE)
  expect("the committed order passes",
         length(check_relocation(committed, known, only1)), 0L)

  # The about:blank rule, which this group is the corpus for.
  expect("about:blank does not disqualify an absolute input",
         wpt_runnable_reason(list(input = "https://x/", base = "about:blank")),
         "runnable")
  expect("about:blank does disqualify a relative one",
         wpt_runnable_reason(list(input = "#x", base = "about:blank")),
         "base-relative")
  expect("a real base disqualifies either way",
         wpt_runnable_reason(list(input = "https://x/", base = "a:b")),
         "base-relative")
  # A NUL-bearing row would need a label this group has never carried, so the
  # lookup must produce NA and check E must report it rather than invent one.
  expect("a NUL reason has no label in this group",
         is.na(unname(RUNNABLE_LABEL["nul-byte"])), TRUE)
  nul_up <- data.frame(input = "a:1", expected = "a:1", reason = "nul-byte",
                       stringsAsFactors = FALSE)
  expect("an unlabelled reason fails rather than defaulting",
         length(check_runnable(fx("ada-001", "a:1", "a:1"), nul_up)) > 0L, TRUE)

  cat(sprintf("self-test: %d passed, %d failed\n", pass, length(fail)))
  if (length(fail)) {
    cat(paste0("  - ", fail, collapse = "\n"), "\n", sep = "")
    stop("verify-ada-extra-urltestdata self-test: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  fixture <- read_fixture(FIXTURE)
  committed <- fixture[!is.na(fixture$source) & fixture$source == GROUP, ,
                       drop = FALSE]
  if (!nrow(committed)) {
    stop("FATAL: no rows with source == '", GROUP, "' in ", FIXTURE,
         " -- this gate grades a group that is not there.", call. = FALSE)
  }
  committed$input <- fixture_inputs(committed)

  pin <- oracle_source_pin(GROUP)
  upstream <- wpt_upstream_frame(oracle_source_cases(pin))

  cat(sprintf("group          : %s\n", GROUP))
  cat(sprintf("committed rows : %d\n", nrow(committed)))
  cat(sprintf("upstream cases : %d at the pinned revision %s\n",
              nrow(upstream), substr(pin$revision, 1L, 12L)))

  fails <- c(
    check_relocation(committed, upstream),
    check_upstream_only(committed, upstream),
    check_expectations_at_pin(committed, upstream),
    check_runnable(committed, upstream),
    wpt_check_restatement(committed, committed$runnable == "yes"),
    check_reproducing_revision(committed)
  )

  if (length(fails)) {
    cat("\n== failures ==\n")
    cat(paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
    cat("\nORACLE RE-LOCATION: FAIL\n")
    quit(status = 1L)
  }
  cat(sprintf("re-location    : %d/%d rows re-locate in upstream order\n",
              nrow(committed), nrow(committed)))
  cat(sprintf("drift ledger   : %d row(s) drifted at the pin, exactly as recorded\n",
              length(DRIFT_LEDGER)))
  cat(sprintf("upstream-only  : %d entr(ies) outside the block, all ledgered\n",
              length(UPSTREAM_ONLY_LEDGER)))
  cat(sprintf("runnable       : %d/%d classifications re-derive\n",
              nrow(committed), nrow(committed)))
  cat(sprintf("restatement    : %d/%d oracle_kind/oracle_value cells re-derive\n",
              nrow(committed), nrow(committed)))
  cat(sprintf("second anchor  : %d/%d expectations reproduce at %s (%s)\n",
              nrow(committed), nrow(committed),
              substr(REPRODUCING$revision, 1L, 12L), REPRODUCING$date))
  cat("ORACLE RE-LOCATION: PASS\n")
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) self_test() else with_oracle_source(main())
}
