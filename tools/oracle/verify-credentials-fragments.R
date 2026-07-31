#!/usr/bin/env Rscript
# RURL-ozdejfzl -- prove the 43 `wpt-credentials-fragments` rows are re-derivable.
#
# WHAT THIS DISCHARGES. `oracle-provenance.json` claims, in prose, that
#
#     "43/43 inputs re-locate in the in-repo import and 43/43 expected values
#      re-derive from its recorded components by URL Standard sec 4.5"
#
# and the provenance gate (PV5) accepts that claim without testing it -- PV5
# checks that a `generation_command` is RECORDED, not that it runs. The claim was
# verified once, by hand, in the session that filed it. This turns it into an
# executable check, so it is re-verified on every run instead of aging into a
# statement nobody has re-tested.
#
# WHY IT LOADS NO PACKAGE. The derivation is a transcription of the WHATWG URL
# serializer and must stay independent of rurl -- an oracle that consults the
# implementation it grades proves nothing. `devtools::load_all()` is deliberately
# absent, which makes the independence structural rather than a comment: there is
# no route from here to rurl's own answer.
#
# Usage:
#   Rscript tools/oracle/verify-credentials-fragments.R
#   Rscript tools/oracle/verify-credentials-fragments.R --self-test
#
# Exit status 0 = the committed rows re-derive; non-zero = they do not.

FIXTURE <- "tests/testthat/fixtures/external-url-vectors.csv"
CONFORMANCE <- "tests/testthat/fixtures/url-standard-conformance.csv"
GROUP <- "wpt-credentials-fragments"

here <- function(...) file.path("tools", "oracle", ...)
source(here("derive-credentials-fragments.R"), local = FALSE)

read_fixture <- function(path) {
  if (!file.exists(path)) {
    stop("FATAL: fixture not found: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = "NA")
}

# The committed corpus's `input` column is NA for non-runnable rows (a control
# byte cannot survive a CSV round trip), so the JSON spelling is authoritative
# where present. This mirrors the fixture's own convention.
recover_inputs <- function(d) {
  inp <- d$input
  runnable <- !is.na(d$runnable) & d$runnable == "yes"
  if (any(runnable)) {
    inp[runnable] <- vapply(d$input_json[runnable], jsonlite::fromJSON,
                            character(1), USE.NAMES = FALSE)
  }
  inp
}

# ---- checks -----------------------------------------------------------------

# Check A -- RE-DERIVATION. For every committed row of the group, the derivation
# must produce that input, and the expected value the fixture records must equal
# the independently derived section 4.5 serialization. This is exactly the claim
# the provenance record makes, and it is independent of selection order.
check_rederivation <- function(committed, derived) {
  fail <- character(0)
  idx <- match(committed$input, derived$input)
  missing <- committed$id[is.na(idx)]
  if (length(missing)) {
    fail <- c(fail, sprintf(
      "%d committed row(s) do not re-derive from the import at all: %s",
      length(missing), paste(missing, collapse = ", ")))
  }
  ok <- !is.na(idx)
  if (any(ok)) {
    got <- derived$href[idx[ok]]
    want <- committed$standard_expectation[ok]
    bad <- which(got != want)
    if (length(bad)) {
      fail <- c(fail, sprintf(
        "%d committed expectation(s) disagree with the derivation:", length(bad)))
      for (i in bad) {
        fail <- c(fail, sprintf(
          "    %s  input=%s\n      recorded=%s\n      derived =%s",
          committed$id[ok][i], encodeString(committed$input[ok][i]),
          encodeString(want[i]), encodeString(got[i])))
      }
    }
    # The `notes` column records which shape admitted the row; a drift there
    # means the selector moved even when the href happens to agree.
    want_notes <- sprintf("shape coverage: %s", derived$kind[idx[ok]])
    bad_notes <- which(committed$notes[ok] != want_notes)
    if (length(bad_notes)) {
      fail <- c(fail, sprintf(
        "%d committed row(s) record a different admitting shape:",
        length(bad_notes)))
      for (i in bad_notes) {
        fail <- c(fail, sprintf("    %s  recorded=%s  derived=%s",
                                committed$id[ok][i], committed$notes[ok][i],
                                want_notes[i]))
      }
    }
  }
  fail
}

# Check B -- SELECTION. The derivation over-produces: rows already covered by
# another group, or by the conformance fixture, were dropped when the block was
# built. Re-applying that de-duplication must land on exactly the committed set.
# This is strictly stronger than check A and is the one that notices if a later
# corpus addition silently changed what this group ought to contain.
check_selection <- function(committed, derived, fixture, conformance) {
  others <- fixture[is.na(fixture$source) | fixture$source != GROUP, ,
                    drop = FALSE]
  already <- unique(c(recover_inputs(others), conformance$input))
  already <- already[!is.na(already)]
  selected <- derived$input[!(derived$input %in% already)]

  fail <- character(0)
  extra <- setdiff(selected, committed$input)
  short <- setdiff(committed$input, selected)
  if (length(extra)) {
    fail <- c(fail, sprintf(
      paste0("selection now admits %d input(s) the committed block does not ",
             "carry -- the corpus grew a shape this group should cover:"),
      length(extra)))
    fail <- c(fail, paste0("    ", encodeString(utils::head(extra, 10L))))
  }
  if (length(short)) {
    fail <- c(fail, sprintf(
      paste0("selection now excludes %d committed input(s) -- another group or ",
             "the conformance fixture has since claimed them:"), length(short)))
    fail <- c(fail, paste0("    ", encodeString(utils::head(short, 10L))))
  }
  fail
}

# ---- self-test --------------------------------------------------------------
#
# Positive and negative cases over a synthetic import, so a broken derivation is
# caught by this file rather than by the corpus it grades. The negatives are the
# two constructions the scratch builder got wrong at least once.
self_test <- function() {
  pass <- 0L
  fail <- character(0)
  expect <- function(label, got, want) {
    if (identical(got, want)) {
      pass <<- pass + 1L
    } else {
      fail <<- c(fail, sprintf("%s: got %s, want %s", label,
                               encodeString(as.character(got)),
                               encodeString(as.character(want))))
    }
  }

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  cases <- list(
    `_meta` = list(upstream_project = "web-platform-tests/wpt",
                   upstream_revision = "0123456789abcdef0123456789abcdef01234567"),
    success = list(
      # credentials present -- both halves serialize
      list(input = "https://u:p@h/", protocol = "https:", hostname = "h",
           username = "u", password = "p", pathname = "/"),
      # password empty -- the ":" is dropped, the "@" is kept
      list(input = "https://u:@h/", protocol = "https:", hostname = "h",
           username = "u", password = "", pathname = "/"),
      # BOTH halves empty -- "includes credentials" is false, so the "@" goes
      list(input = "https://@h/", protocol = "https:", hostname = "h",
           username = "", password = "", pathname = "/"),
      # present-but-empty query: `search` is "" exactly as when absent, so the
      # delimiter must be recovered lexically from the input
      list(input = "https://h/?", protocol = "https:", hostname = "h",
           username = "", password = "", pathname = "/", search = ""),
      # present-but-empty query FOLLOWED by a fragment -- the construction that
      # dropped the "?" when it was read off `search` alone
      list(input = "https://h/?#f", protocol = "https:", hostname = "h",
           username = "", password = "", pathname = "/", search = "",
           hash = "#f"),
      # port is emitted only when non-empty
      list(input = "https://u@h:8080/", protocol = "https:", hostname = "h",
           username = "u", password = "", port = "8080", pathname = "/"),
      # NOT admitted: no credentials, no fragment, no empty delimiter
      list(input = "https://h/plain", protocol = "https:", hostname = "h",
           username = "", password = "", pathname = "/plain"),
      # NOT admitted: an "@" that is in the PATH is not an authority delimiter
      list(input = "https://h/a@b", protocol = "https:", hostname = "h",
           username = "", password = "", pathname = "/a@b"),
      # NOT admitted: non-special scheme, so "//" would be a guess
      list(input = "sc://u@h/", protocol = "sc:", hostname = "h",
           username = "u", password = "", pathname = "/")
    )
  )
  writeLines(jsonlite::toJSON(cases, auto_unbox = TRUE, null = "null"), tmp)

  d <- derive_credentials_fragments(tmp)
  href <- setNames(d$href, d$input)
  kind <- setNames(d$kind, d$input)

  expect("credentials both halves", href[["https://u:p@h/"]], "https://u:p@h/")
  expect("empty password keeps @", href[["https://u:@h/"]], "https://u@h/")
  expect("empty credentials drop @", href[["https://@h/"]], "https://h/")
  expect("empty-credentials shape", kind[["https://@h/"]], "empty-credentials")
  expect("present-but-empty query", href[["https://h/?"]], "https://h/?")
  expect("empty query before fragment", href[["https://h/?#f"]],
         "https://h/?#f")
  expect("port emitted", href[["https://u@h:8080/"]], "https://u@h:8080/")
  expect("plain row not admitted", "https://h/plain" %in% d$input, FALSE)
  expect("path @ not admitted", "https://h/a@b" %in% d$input, FALSE)
  expect("non-special not admitted", "sc://u@h/" %in% d$input, FALSE)

  # An import with no usable rows must abort rather than record an empty oracle.
  empty <- tempfile(fileext = ".json")
  on.exit(unlink(empty), add = TRUE)
  writeLines(jsonlite::toJSON(list(
    `_meta` = cases[["_meta"]],
    success = list(list(input = "https://h/plain", protocol = "https:",
                        hostname = "h", pathname = "/plain"))
  ), auto_unbox = TRUE, null = "null"), empty)
  expect("empty derivation fails closed",
         inherits(try(derive_credentials_fragments(empty), silent = TRUE),
                  "try-error"), TRUE)

  # A missing import must abort, not silently derive nothing.
  expect("missing import fails closed",
         inherits(try(derive_credentials_fragments(tempfile()), silent = TRUE),
                  "try-error"), TRUE)

  cat(sprintf("self-test: %d passed, %d failed\n", pass, length(fail)))
  if (length(fail)) {
    cat(paste0("  - ", fail, collapse = "\n"), "\n", sep = "")
    stop("verify-credentials-fragments self-test: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  fixture <- read_fixture(FIXTURE)
  conformance <- read_fixture(CONFORMANCE)
  committed <- fixture[!is.na(fixture$source) & fixture$source == GROUP, ,
                       drop = FALSE]
  if (!nrow(committed)) {
    stop("FATAL: no rows with source == '", GROUP, "' in ", FIXTURE,
         " -- this gate grades a group that is not there.", call. = FALSE)
  }
  committed$input <- recover_inputs(committed)

  derived <- derive_credentials_fragments()

  cat(sprintf("group          : %s\n", GROUP))
  cat(sprintf("committed rows : %d\n", nrow(committed)))
  cat(sprintf("derived rows   : %d (before de-duplication)\n", nrow(derived)))

  fails <- c(
    check_rederivation(committed, derived),
    check_selection(committed, derived, fixture, conformance)
  )

  if (length(fails)) {
    cat("\n== failures ==\n")
    cat(paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
    cat("\nORACLE RE-DERIVATION: FAIL\n")
    quit(status = 1L)
  }
  cat(sprintf("re-derivation  : %d/%d expected values re-derive by sec 4.5\n",
              nrow(committed), nrow(committed)))
  cat("selection      : de-duplication reproduces the committed set exactly\n")
  cat("ORACLE RE-DERIVATION: PASS\n")
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
  } else {
    main()
  }
}
