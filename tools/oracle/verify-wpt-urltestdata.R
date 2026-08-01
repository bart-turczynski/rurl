#!/usr/bin/env Rscript
# RURL-ozdejfzl -- prove the 267 `wpt-urltestdata` rows re-locate upstream.
#
# WHAT THIS DISCHARGES. The group's `generation_command` was
# MISSING[RURL-vwurxmzm]: the block was produced by an untracked scratch builder
# that TRUNCATED the fixture, writing only its own 267 rows, so re-running it
# would have destroyed the other six groups. This replaces it with a tracked
# verifier that writes nothing.
#
# RE-LOCATION, NOT RE-DERIVATION. Every expected value in this group is
# upstream's own verdict, read out of hash-pinned bytes -- there is no algorithm
# to re-run and this file does not pretend otherwise. What it establishes is
# that the committed block still IS the upstream failure arm at the pinned
# revision: same inputs, same order, same verdict, nothing added, nothing lost.
#
# TIER 2 READS THE NETWORK, SO IT IS NOT A BLOCKING GATE. The bytes are not
# vendored, so this cannot run offline from a clean checkout; it follows the
# tools/oracle/check-uts46-mapping-pin.R posture -- run by hand, and by CI only
# as `--self-test`, which is fully offline. A fetch failure is exit status 2 and
# is never a pass.
#
# WHY IT LOADS NO PACKAGE. Same reason as every module here: there is no route
# from this file to rurl's own answer.
#
# Usage:
#   Rscript tools/oracle/verify-wpt-urltestdata.R
#   Rscript tools/oracle/verify-wpt-urltestdata.R --self-test
#
# Exit 0 = the committed rows re-locate; 1 = they do not; 2 = source unavailable.

FIXTURE <- "tests/testthat/fixtures/external-url-vectors.csv"
GROUP <- "wpt-urltestdata"

here <- function(...) file.path("tools", "oracle", ...)
source(here("fetch-source.R"), local = FALSE)
source(here("relocate-wpt-format.R"), local = FALSE)

# How the group's applicability_selector reads, executably: the upstream failure
# arm and nothing else.
UPSTREAM_SELECTOR <- function(cases) Filter(function(e) isTRUE(e$failure), cases)

# The fixture's spelling of each classifier reason, for THIS group.
RUNNABLE_LABEL <- c(runnable = "yes",
                    `base-relative` = "no-relative-resolution",
                    `nul-byte` = "no-nul-byte-r-cannot-represent")

read_fixture <- function(path) {
  if (!file.exists(path)) stop("FATAL: fixture not found: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = "NA")
}

# ---- checks -----------------------------------------------------------------

# Check A -- ORDERED RE-LOCATION. The committed inputs must be the upstream
# failure arm's inputs, in upstream order.
#
# ORDER IS CHECKED ON PURPOSE, and it is stronger than it looks. Four of the 263
# distinct inputs occur TWICE upstream (the same input under different bases),
# so a set comparison cannot even see the duplicates, and a multiset comparison
# cannot see a permutation. Upstream order is also what makes the row ids
# meaningful: wpt-fail-NNN is positional, so a reordering silently re-points
# every id in the block at a different case.
check_relocation <- function(committed, upstream) {
  fail <- character(0)
  if (nrow(committed) != nrow(upstream)) {
    fail <- c(fail, sprintf(
      "the committed block has %d row(s), the upstream failure arm has %d",
      nrow(committed), nrow(upstream)))
  }
  n <- min(nrow(committed), nrow(upstream))
  if (n) {
    bad <- which(committed$input[seq_len(n)] != upstream$input[seq_len(n)])
    if (length(bad)) {
      fail <- c(fail, sprintf(
        "%d row(s) do not re-locate at the same position upstream:", length(bad)))
      for (i in utils::head(bad, 10L)) {
        fail <- c(fail, sprintf("    %s\n      committed=%s\n      upstream =%s",
                                committed$id[i], encodeString(committed$input[i]),
                                encodeString(upstream$input[i])))
      }
    }
  }
  # Both directions independently of position, so a same-length permutation is
  # reported as what it is rather than as 267 unrelated mismatches.
  gone <- setdiff(committed$input, upstream$input)
  grew <- setdiff(upstream$input, committed$input)
  if (length(gone)) {
    fail <- c(fail, sprintf(
      "%d committed input(s) are not in the upstream failure arm at all: %s",
      length(gone), paste(encodeString(utils::head(gone, 5L)), collapse = ", ")))
  }
  if (length(grew)) {
    fail <- c(fail, sprintf(
      "%d upstream failure case(s) are absent from the committed block: %s",
      length(grew), paste(encodeString(utils::head(grew, 5L)), collapse = ", ")))
  }
  fail
}

# Check B -- THE VERDICT IS UPSTREAM'S. Every row records standard_expectation
# = "failure" because upstream declares `failure: true`, not because someone
# judged it. A row whose expectation drifted to a serialization would mean this
# group had silently acquired a second kind of claim.
check_verdict <- function(committed, upstream) {
  fail <- character(0)
  n <- min(nrow(committed), nrow(upstream))
  if (!n) return(fail)
  bad <- which(committed$standard_expectation[seq_len(n)] !=
                 upstream$expected[seq_len(n)])
  if (length(bad)) {
    fail <- c(fail, sprintf(
      "%d committed expectation(s) disagree with upstream's own verdict:",
      length(bad)))
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  recorded=%s  upstream=%s",
                              committed$id[i],
                              encodeString(committed$standard_expectation[i]),
                              encodeString(upstream$expected[i])))
    }
  }
  fail
}

# Check C -- THE RUNNABLE CLASSIFICATION RE-DERIVES. This is the one column in
# the block that upstream does not state, so it is the one a later pass could
# change without upstream noticing. See the classifier's own note on why it is a
# reconstruction and what that does and does not buy.
check_runnable <- function(committed, upstream) {
  fail <- character(0)
  n <- min(nrow(committed), nrow(upstream))
  if (!n) return(fail)
  want <- unname(RUNNABLE_LABEL[upstream$reason[seq_len(n)]])
  if (anyNA(want)) {
    fail <- c(fail, sprintf(
      "the classifier produced %d reason(s) this group has no label for: %s",
      sum(is.na(want)),
      paste(unique(upstream$reason[seq_len(n)][is.na(want)]), collapse = ", ")))
    return(fail)
  }
  bad <- which(committed$runnable[seq_len(n)] != want)
  if (length(bad)) {
    fail <- c(fail, sprintf("%d row(s) are classified differently:", length(bad)))
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  recorded=%s  derived=%s  input=%s",
                              committed$id[i], committed$runnable[i], want[i],
                              encodeString(committed$input_json[i])))
    }
  }
  fail
}

# Check D -- THE INPUT COLUMN'S NA CONVENTION. `input` is NA exactly where the
# row is not runnable, and equals the decoded `input_json` everywhere else. This
# is what makes input_json authoritative rather than merely present, and it is
# the column a later "tidy the NAs" pass would quietly break.
check_input_column <- function(committed) {
  fail <- character(0)
  runnable <- committed$runnable == "yes"
  wrong_na <- which(!runnable & !is.na(committed$input))
  if (length(wrong_na)) {
    fail <- c(fail, sprintf(
      "%d non-runnable row(s) carry a non-NA `input`: %s", length(wrong_na),
      paste(committed$id[utils::head(wrong_na, 5L)], collapse = ", ")))
  }
  wrong_val <- which(runnable & (is.na(committed$input) |
                                   committed$input != committed$input_decoded))
  if (length(wrong_val)) {
    fail <- c(fail, sprintf(
      "%d runnable row(s) have an `input` that is not their decoded input_json: %s",
      length(wrong_val),
      paste(committed$id[utils::head(wrong_val, 5L)], collapse = ", ")))
  }
  fail
}

# Check E -- THE BLOCK'S CONSTANT COLUMNS. source_class, standard and
# source_reference are one value each across all 267 rows; a row that arrived
# from somewhere else would differ, and nothing else in the record would notice.
check_constants <- function(committed) {
  fail <- character(0)
  want <- list(source_class = "A", standard = "whatwg",
               source_reference = paste0(
                 "WPT url/resources/urltestdata.json (BSD-3-Clause) failure case"))
  for (col in names(want)) {
    bad <- which(committed[[col]] != want[[col]])
    if (length(bad)) {
      fail <- c(fail, sprintf("%d row(s) carry an unexpected %s (first: %s = %s)",
                              length(bad), col, committed$id[bad[1]],
                              encodeString(committed[[col]][bad[1]])))
    }
  }
  fail
}

# ---- self-test --------------------------------------------------------------
#
# Offline, synthetic, and the only mode CI runs. It grades the two pieces of
# this file that are logic rather than comparison -- the upstream selector and
# the runnable classifier -- plus the NUL shim, which is the piece whose failure
# mode is a false PASS rather than a visible error.
self_test <- function() {
  pass <- 0L
  fail <- character(0)
  expect <- function(label, got, want) {
    if (identical(got, want)) pass <<- pass + 1L
    else fail <<- c(fail, sprintf("%s: got %s, want %s", label,
                                  encodeString(as.character(got)),
                                  encodeString(as.character(want))))
  }

  cases <- list(
    list(input = "http://f:b/c", failure = TRUE),
    list(input = "http://a b/", failure = TRUE, base = "http://other.com/"),
    list(input = "https://x/", href = "https://x/"),
    list(input = "#x", base = "about:blank", href = "about:blank#x"),
    list(input = "https://y/", base = "about:blank", href = "https://y/")
  )

  sel <- UPSTREAM_SELECTOR(cases)
  expect("selector takes the failure arm only", length(sel), 2L)
  expect("selector keeps upstream order", sel[[1]]$input, "http://f:b/c")

  expect("failure entry expects failure", wpt_expected_value(cases[[1]]),
         "failure")
  expect("success entry expects its href", wpt_expected_value(cases[[3]]),
         "https://x/")
  expect("an entry with neither aborts",
         inherits(try(wpt_expected_value(list(input = "z")), silent = TRUE),
                  "try-error"), TRUE)

  expect("no base is runnable", wpt_runnable_reason(cases[[1]]), "runnable")
  expect("a real base is not", wpt_runnable_reason(cases[[2]]), "base-relative")
  # about:blank is the case the classifier exists to get right, in BOTH
  # directions: it does not disqualify an absolute input, and it does
  # disqualify a relative one.
  expect("about:blank with a relative input is base-relative",
         wpt_runnable_reason(cases[[4]]), "base-relative")
  expect("about:blank with an absolute input is runnable",
         wpt_runnable_reason(cases[[5]]), "runnable")
  expect("scheme detection accepts a scheme", wpt_is_absolute("sc://a"), TRUE)
  expect("scheme detection rejects a path", wpt_is_absolute("./foo"), FALSE)
  expect("scheme detection rejects a fragment", wpt_is_absolute("#x"), FALSE)

  # THE NUL SHIM. Its failure mode is silent agreement, so the negative case is
  # the one that matters: without the shim these two DIFFERENT inputs decode to
  # the same truncated string, and a re-location check would call them equal.
  a <- '"sc://a\\u0000b/"'
  b <- '"sc://a\\u0000ZZZ"'
  expect("unshimmed decoding collapses two different inputs",
         identical(jsonlite::fromJSON(a), jsonlite::fromJSON(b)), TRUE)
  expect("shimmed decoding keeps them apart",
         identical(jsonlite::fromJSON(json_text_nul_safe(a, "t")),
                   jsonlite::fromJSON(json_text_nul_safe(b, "t"))), FALSE)
  expect("shimmed input keeps its full length",
         nchar(jsonlite::fromJSON(json_text_nul_safe(a, "t"))), 9L)
  expect("shimmed NUL is classified as a nul-byte",
         wpt_runnable_reason(list(
           input = jsonlite::fromJSON(json_text_nul_safe(a, "t")))), "nul-byte")
  expect("an already-present shim code point aborts",
         inherits(try(json_text_nul_safe(NUL_SHIM_CHAR, "t"), silent = TRUE),
                  "try-error"), TRUE)
  expect("an escaped NUL escape aborts",
         inherits(try(json_text_nul_safe("\"a\\\\u0000b\"", "t"), silent = TRUE),
                  "try-error"), TRUE)

  # An input that is both NUL-bearing and base-relative has no recorded
  # precedence, so it must abort rather than be filed under a guessed label.
  expect("an unmodeled combination aborts",
         inherits(try(wpt_runnable_reason(list(
           input = paste0("a", NUL_SHIM_CHAR, "b"),
           base = "http://other.com/")), silent = TRUE), "try-error"), TRUE)

  cat(sprintf("self-test: %d passed, %d failed\n", pass, length(fail)))
  if (length(fail)) {
    cat(paste0("  - ", fail, collapse = "\n"), "\n", sep = "")
    stop("verify-wpt-urltestdata self-test: FAIL", call. = FALSE)
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
  committed$input_decoded <- fixture_inputs(committed)
  committed$input <- ifelse(is.na(committed$input), NA_character_,
                            committed$input)

  pin <- oracle_source_pin(GROUP)
  cases <- oracle_source_cases(pin)
  upstream <- wpt_upstream_frame(UPSTREAM_SELECTOR(cases))

  cat(sprintf("group          : %s\n", GROUP))
  cat(sprintf("committed rows : %d\n", nrow(committed)))
  cat(sprintf("upstream cases : %d total, %d in the failure arm\n",
              length(cases), nrow(upstream)))

  # The re-location comparison runs on the decoded inputs, which is where the
  # NUL shim is load-bearing.
  cmp <- committed
  cmp$input <- committed$input_decoded

  fails <- c(
    check_relocation(cmp, upstream),
    check_verdict(cmp, upstream),
    check_runnable(cmp, upstream),
    check_input_column(committed),
    check_constants(committed)
  )

  if (length(fails)) {
    cat("\n== failures ==\n")
    cat(paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
    cat("\nORACLE RE-LOCATION: FAIL\n")
    quit(status = 1L)
  }
  cat(sprintf("re-location    : %d/%d rows re-locate in upstream order\n",
              nrow(committed), nrow(committed)))
  cat("verdict        : every expectation is upstream's own `failure: true`\n")
  cat(sprintf("runnable       : %d/%d classifications re-derive\n",
              nrow(committed), nrow(committed)))
  cat("ORACLE RE-LOCATION: PASS\n")
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) self_test() else with_oracle_source(main())
}
