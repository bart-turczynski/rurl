#!/usr/bin/env Rscript
# RURL-ozdejfzl -- prove the 17 `ada-verifydnslength` rows, which are the one
# tier-2 group whose expectations are DERIVED rather than read out of upstream.
#
# THE GROUP'S RECORD WAS WRONG ABOUT ITSELF. See derive-verifydnslength.R for
# the measurement: upstream marks 10 of 17 entries `failure: true` and the
# fixture records `accept` for all 17, so ten expectations are hand-derived from
# the URL Standard's text rather than copied. The record now carries a real
# source pin instead of pin_status "not-applicable".
#
# SO THIS GATE IS BOTH KINDS AT ONCE, and that is the interesting part. It
# re-locates (the inputs are upstream's, hash-pinned) AND it re-derives (the
# verdict is not). It also derives the reading that produces upstream's OWN
# verdict, so the disagreement between the two is checked rather than tolerated:
# a hand-written "these ten differ" ledger would record that they differ; two
# derivations show the difference is exactly the DNS-length axis, and nothing
# else. If upstream ever rejects one of these hosts for a different reason, the
# disagreement stops being explained and this fails.
#
# Usage:
#   Rscript tools/oracle/verify-ada-verifydnslength.R
#   Rscript tools/oracle/verify-ada-verifydnslength.R --self-test
#
# Exit 0 = the rows re-locate and re-derive; 1 = they do not; 2 = source
# unavailable.

FIXTURE <- "tests/testthat/fixtures/external-url-vectors.csv"
GROUP <- "ada-verifydnslength"

here <- function(...) file.path("tools", "oracle", ...)
source(here("fetch-source.R"), local = FALSE)
source(here("relocate-wpt-format.R"), local = FALSE)
source(here("derive-verifydnslength.R"), local = FALSE)

read_fixture <- function(path) {
  if (!file.exists(path)) stop("FATAL: fixture not found: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = "NA")
}

# ---- checks -----------------------------------------------------------------

# Check A -- ORDERED RE-LOCATION. The inputs ARE upstream's, so they must all
# re-locate at the pinned revision, in upstream order, with nothing added or
# dropped on either side. This group has no ledger: unlike ada-extra, upstream
# has not moved since the import.
check_relocation <- function(committed, upstream) {
  fail <- character(0)
  if (nrow(committed) != length(upstream)) {
    fail <- c(fail, sprintf(
      "the committed block has %d row(s), upstream has %d entr(ies)",
      nrow(committed), length(upstream)))
  }
  up_in <- vapply(upstream, function(e) e$input, character(1))
  n <- min(nrow(committed), length(up_in))
  if (n) {
    bad <- which(committed$input[seq_len(n)] != up_in[seq_len(n)])
    if (length(bad)) {
      fail <- c(fail, sprintf(
        "%d row(s) do not re-locate at the same position upstream:", length(bad)))
      for (i in utils::head(bad, 10L)) {
        fail <- c(fail, sprintf("    %s\n      committed=%s\n      upstream =%s",
                                committed$id[i], encodeString(committed$input[i]),
                                encodeString(up_in[i])))
      }
    }
  }
  gone <- setdiff(committed$input, up_in)
  grew <- setdiff(up_in, committed$input)
  if (length(gone)) {
    fail <- c(fail, sprintf("%d committed input(s) are not upstream: %s",
                            length(gone),
                            paste(encodeString(gone), collapse = ", ")))
  }
  if (length(grew)) {
    fail <- c(fail, sprintf("%d upstream entr(ies) are not committed: %s",
                            length(grew),
                            paste(encodeString(grew), collapse = ", ")))
  }
  fail
}

# Check B -- THE EXPECTATION RE-DERIVES FROM THE STANDARD. Every row must derive
# to `accept` under the URL Standard's own default, which is the claim the
# fixture makes and the claim the record now pins a revision for.
check_whatwg_derivation <- function(committed) {
  fail <- character(0)
  for (i in seq_len(nrow(committed))) {
    got <- dnslen_whatwg_accepts(committed$input[i])
    want <- committed$standard_expectation[i] == "accept"
    if (!identical(got, want)) {
      fail <- c(fail, sprintf(
        "%s: derived %s under the WHATWG default, recorded %s",
        committed$id[i], if (got) "accept" else "failure",
        committed$standard_expectation[i]))
    }
  }
  # A block that derived nothing would pass the loop above vacuously.
  if (!nrow(committed)) {
    fail <- c(fail, "no rows to derive -- the gate would pass over an empty set")
  }
  fail
}

# Check C -- THE DISAGREEMENT WITH UPSTREAM IS EXACTLY THE DNS-LENGTH AXIS.
# For every row, upstream's own verdict must equal the RFC 1035 section 2.3.4
# reading, and where the two readings differ the fixture must be following the
# WHATWG one. This is what makes "the fixture contradicts upstream on 10 rows"
# an explanation rather than an exception.
check_disagreement <- function(committed, upstream) {
  fail <- character(0)
  n <- min(nrow(committed), length(upstream))
  differ <- 0L
  for (i in seq_len(n)) {
    host <- dnslen_host_of(committed$input[i])
    upstream_rejects <- isTRUE(upstream[[i]]$failure)
    rfc_violates <- dnslen_rfc1035_violates(host)
    if (!identical(upstream_rejects, rfc_violates)) {
      fail <- c(fail, sprintf(
        paste0("%s: upstream %s this host, but RFC 1035 section 2.3.4 %s -- ",
               "the disagreement with the fixture is no longer explained by ",
               "the DNS-length axis alone"),
        committed$id[i],
        if (upstream_rejects) "REJECTS" else "accepts",
        if (rfc_violates) "is violated" else "is satisfied"))
    }
    if (upstream_rejects) differ <- differ + 1L
  }
  # A floor, because a check that silently exercises zero disagreeing rows
  # passes. The whole reason this group needs a derivation is that some rows
  # DO disagree; if none did, its expectations would be upstream's after all
  # and the record's source pin would be describing a duty it no longer owes.
  if (differ == 0L) {
    fail <- c(fail, paste0(
      "no row disagrees with upstream any more -- if that is real, this group ",
      "no longer derives anything and its normative_dependencies entry must ",
      "be revisited rather than left claiming a pin it does not need"))
  }
  attr(fail, "differ") <- differ
  fail
}

# Check D -- THE BLOCK'S CONSTANT COLUMNS AND THE RESTATEMENT. `runnable` is
# "yes" for all 17 (every host is representable), and the machine-readable
# restatement must follow the expectation.
check_shape <- function(committed) {
  fail <- character(0)
  bad <- which(committed$runnable != "yes")
  if (length(bad)) {
    fail <- c(fail, sprintf("%d row(s) are not runnable: %s", length(bad),
                            paste(committed$id[bad], collapse = ", ")))
  }
  bad <- which(committed$oracle_kind != "accept")
  if (length(bad)) {
    fail <- c(fail, sprintf(
      "%d row(s) restate an oracle_kind other than `accept`: %s", length(bad),
      paste(committed$id[bad], collapse = ", ")))
  }
  bad <- which(!is.na(committed$oracle_value))
  if (length(bad)) {
    fail <- c(fail, sprintf(
      "%d row(s) carry an oracle_value -- an `accept` claim names no serialization: %s",
      length(bad), paste(committed$id[bad], collapse = ", ")))
  }
  bad <- which(committed$standard != "whatwg")
  if (length(bad)) {
    fail <- c(fail, sprintf("%d row(s) name a standard other than whatwg: %s",
                            length(bad), paste(committed$id[bad], collapse = ", ")))
  }
  fail
}

# ---- self-test --------------------------------------------------------------
#
# Offline and synthetic, and the only mode CI runs. It grades the two readings,
# which are the logic; the comparisons are exercised by the full run.
self_test <- function() {
  pass <- 0L
  fail <- character(0)
  expect <- function(label, got, want) {
    if (identical(got, want)) pass <<- pass + 1L
    else fail <<- c(fail, sprintf("%s: got %s, want %s", label,
                                  encodeString(as.character(got)),
                                  encodeString(as.character(want))))
  }
  errs <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

  long_label <- strrep("a", 64L)
  ok_label <- strrep("a", 63L)

  # Reading 1 -- the WHATWG default accepts what RFC 1035 rejects. These are
  # the four shapes the corpus turns on.
  expect("a 64-character label is accepted",
         dnslen_whatwg_accepts(paste0("http://", long_label, ".com")), TRUE)
  expect("an empty label is accepted",
         dnslen_whatwg_accepts("http://example..com.br"), TRUE)
  expect("a leading dot is accepted",
         dnslen_whatwg_accepts("http://.example.com.br"), TRUE)
  expect("a 280-character name is accepted",
         dnslen_whatwg_accepts(paste0("http://", strrep("example.com.", 24L),
                                      "br")), TRUE)
  expect("a trailing slash is the same host",
         dnslen_whatwg_accepts("http://example.com/"), TRUE)
  # ... and still rejects what the URL Standard itself rejects, so "accept
  # everything" is not what the derivation does.
  expect("a forbidden domain code point is still rejected",
         dnslen_whatwg_accepts("http://exa<mple.com"), FALSE)
  expect("an unmodeled shape aborts",
         errs(dnslen_whatwg_accepts("https://example.com/")), TRUE)
  expect("a path aborts",
         errs(dnslen_whatwg_accepts("http://example.com/a")), TRUE)

  # Reading 2 -- RFC 1035 section 2.3.4.
  expect("64 characters violates the label limit",
         dnslen_rfc1035_violates(paste0(long_label, ".com")), TRUE)
  expect("63 characters does not",
         dnslen_rfc1035_violates(paste0(ok_label, ".com")), FALSE)
  expect("an empty interior label violates",
         dnslen_rfc1035_violates("example..com.br"), TRUE)
  expect("a leading dot violates",
         dnslen_rfc1035_violates(".example.com.br"), TRUE)
  # THE TRAILING-FIELD TRAP, in both directions. One trailing dot is the root
  # label and is legal; two is an empty label and is not. Base strsplit() drops
  # trailing empty fields, so a naive split scores the second as legal -- which
  # it did, on a real row, the first time this was measured.
  expect("one trailing dot is the root label",
         dnslen_rfc1035_violates("example.com."), FALSE)
  expect("two trailing dots is an empty label",
         dnslen_rfc1035_violates("example.com.."), TRUE)
  expect("four trailing dots too",
         dnslen_rfc1035_violates("example.com...."), TRUE)
  expect("253 characters is within the name limit",
         dnslen_rfc1035_violates(paste0(strrep("ab.", 84L), "c")), FALSE)
  expect("254 without a trailing dot is not",
         dnslen_rfc1035_violates(paste0(strrep("ab.", 84L), "cd")), TRUE)
  expect("254 WITH a trailing dot is",
         dnslen_rfc1035_violates(paste0(strrep("ab.", 84L), "c.")), FALSE)

  # The floor: a corpus where nothing disagrees must fail rather than pass
  # vacuously, because that is the state in which the group's source pin would
  # be describing a duty it no longer owes.
  agreeing <- data.frame(id = "x", input = "http://example.com/",
                         stringsAsFactors = FALSE)
  expect("a corpus with no disagreement fails the floor",
         length(check_disagreement(agreeing,
                                   list(list(input = "http://example.com/")))) > 0L,
         TRUE)

  cat(sprintf("self-test: %d passed, %d failed\n", pass, length(fail)))
  if (length(fail)) {
    cat(paste0("  - ", fail, collapse = "\n"), "\n", sep = "")
    stop("verify-ada-verifydnslength self-test: FAIL", call. = FALSE)
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
  upstream <- oracle_source_cases(pin)

  cat(sprintf("group          : %s\n", GROUP))
  cat(sprintf("committed rows : %d\n", nrow(committed)))
  cat(sprintf("upstream cases : %d at the pinned revision %s\n",
              length(upstream), substr(pin$revision, 1L, 12L)))

  disagreement <- check_disagreement(committed, upstream)
  fails <- c(
    check_relocation(committed, upstream),
    check_whatwg_derivation(committed),
    as.character(disagreement),
    check_shape(committed)
  )

  if (length(fails)) {
    cat("\n== failures ==\n")
    cat(paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
    cat("\nORACLE RE-LOCATION + RE-DERIVATION: FAIL\n")
    quit(status = 1L)
  }
  cat(sprintf("re-location    : %d/%d inputs re-locate in upstream order\n",
              nrow(committed), nrow(committed)))
  cat(sprintf("re-derivation  : %d/%d expectations derive as `accept` under the\n",
              nrow(committed), nrow(committed)))
  cat("                 WHATWG default (host parser step 6 runs the domain\n")
  cat("                 parser with beStrict = false; ToASCII sets\n")
  cat("                 VerifyDnsLength to beStrict)\n")
  cat(sprintf("disagreement   : %d/%d rows are rejected upstream, and upstream's\n",
              attr(disagreement, "differ"), nrow(committed)))
  cat("                 verdict equals the RFC 1035 section 2.3.4 reading on\n")
  cat("                 every row -- so the difference is that axis alone\n")
  cat("ORACLE RE-LOCATION + RE-DERIVATION: PASS\n")
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) self_test() else with_oracle_source(main())
}
