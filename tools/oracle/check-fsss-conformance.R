#!/usr/bin/env Rscript
# RURL-drkcvzex -- the FSSS conformance assertion the tier-2 gates carry, in its
# OWN file because it is not an oracle check.
#
# THIS IS NOT PROVENANCE, AND THE FILE BOUNDARY IS THE POINT. Everything else
# under tools/oracle/ answers a question about where an expected value CAME FROM:
# it derives from pinned upstream bytes or from pinned normative text, and never
# from anything rurl produced. This file answers a different question -- does the
# implementation's recorded serialization AGREE with that expected value -- and
# its left-hand side is `fsss_whatwg`, a CAPTURED rurl output column.
#
# It was living inside wpt_check_restatement() in relocate-wpt-format.R, which
# made the two indistinguishable: a tier-2 gate reported "ORACLE RE-LOCATION:
# PASS" over a set of checks one of which was grading the implementation. That is
# not circular -- `oracle_value` is independent of rurl, so the comparison has a
# real subject -- but a conformance assertion filed under an oracle's label is
# how the next reader comes to believe the oracle was checked against rurl, or
# that rurl was checked against itself. Naming and separating it costs nothing
# and removes the ambiguity.
#
# WHY THE COMPARISON IS *NOT* CONDITIONED ON `rurl_deviation`, unlike the tier-3
# gates. The obvious review question, because verify-youarealiar.R and
# verify-equivocal-urls.R assert their FSSS equality only where `rurl_deviation`
# is NA, to avoid the RURL-nknytzxz co-confirmation trap. Two of these rows
# (ada-003, ada-006) DO carry a deviation and DO satisfy the equality, which is
# the shape of that trap -- so it was measured rather than argued.
#
# The asymmetry is real and the two cases are not the same comparison. Tier 3's
# `oracle_value` is a HOST ("google.com"), which is why those gates compare it to
# `fsss_host`; tier 2's is a full SERIALIZATION, compared to `fsss_whatwg`.
# Measured across the whole fixture: all 16 rows where fsss_whatwg differs from
# oracle_value are tier-3 rows, and they differ because the two columns hold
# different KINDS of value there, not because an implementation deviates.
#
# And the deviations on ada-003 (ADR 0011, path_encoding) and ada-006 (ADR 0002,
# Punycode) are deviations of `clean_url` -- the presentation surface -- while
# `fsss_whatwg` is the conformance serialization, and both rows carry
# fsss_conforms = "yes". So the equality holding there is the EXPECTED state, not
# luck. Conditioning on `rurl_deviation` would have been the error: it would
# switch the check off on exactly the rows where a presentation surface deviates
# and the conformance surface still has to agree. REQUIRED_DEVIATION_IDS below
# names those two rows so that reasoning cannot be undone by a floor that erodes
# to zero deviating rows without anyone noticing.

# The two rows whose coverage is the whole argument above. A caller states them
# explicitly rather than trusting a count, because "2 deviating rows are graded"
# and "these 2 deviating rows are graded" are different claims and only the
# second survives a corpus edit.
FSSS_REQUIRED_DEVIATION_IDS <- c("ada-003", "ada-006")

# Grade `fsss_whatwg` against `oracle_value` on the rows that carry both.
#
# `min_rows` / `require_ids` are FLOORS, not expectations: a corpus may grow, but
# it may not silently shrink to a population that makes this pass for nothing.
# min_rows = 0 is legitimate and is not a loophole -- the wpt-urltestdata failure
# arm has no serialization to compare, by construction -- but it must be PASSED,
# so a caller states that it grades nothing rather than discovering it.
fsss_check_conformance <- function(committed, min_rows, require_ids = character(0)) {
  fail <- character(0)
  both <- !is.na(committed$fsss_whatwg) & !is.na(committed$oracle_value)
  graded <- committed$id[both]
  bad <- which(both & committed$fsss_whatwg != committed$oracle_value)
  if (length(bad)) {
    fail <- c(fail, sprintf(
      "%d row(s) carry an fsss_whatwg that is not their oracle_value:",
      length(bad)))
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  fsss_whatwg=%s  oracle_value=%s",
                              committed$id[i],
                              encodeString(committed$fsss_whatwg[i]),
                              encodeString(committed$oracle_value[i])))
    }
  }
  if (length(graded) < min_rows) {
    fail <- c(fail, sprintf(
      paste0("only %d row(s) are graded, and the floor is %d -- a conformance ",
             "check whose population shrinks passes for the wrong reason"),
      length(graded), min_rows))
  }
  uncovered <- setdiff(require_ids, graded)
  if (length(uncovered)) {
    fail <- c(fail, sprintf(
      paste0("%d row(s) that MUST stay covered are not graded: %s -- these are ",
             "the deviation-carrying rows the unconditional comparison exists ",
             "for; losing them would quietly restore the rurl_deviation ",
             "conditioning this check refuses"),
      length(uncovered), paste(uncovered, collapse = ", ")))
  }
  attr(fail, "graded") <- length(graded)
  attr(fail, "deviating") <- sum(both & !is.na(committed$rurl_deviation))
  fail
}

# The one-line report, printed under its own heading by each caller so the
# implementation arm is never absorbed into the oracle verdict.
fsss_report_line <- function(result) {
  sprintf(paste("fsss conformance: %d row(s) graded, %d of them carrying a",
                "documented rurl_deviation"),
          attr(result, "graded"), attr(result, "deviating"))
}
