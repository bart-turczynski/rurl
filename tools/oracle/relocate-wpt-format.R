#!/usr/bin/env Rscript
# RURL-ozdejfzl -- reading the WPT urltestdata format, shared by the two groups
# that were imported in it: wpt-urltestdata and ada-extra-urltestdata.
#
# WHAT A TIER-2 VERIFIER CAN AND CANNOT ESTABLISH. Tier 1 re-DERIVES an expected
# value from the standard; tier 3 can only check a transcription is INTACT. Tier
# 2 sits between them and the honest word is RE-LOCATION: the expected values
# were read out of upstream bytes, so what is checkable is that each committed
# row still corresponds to an upstream entry at the pinned revision and still
# records that entry's own verdict. Nothing here re-derives anything, and the
# gates say ORACLE RE-LOCATION rather than ORACLE RE-DERIVATION for that reason.
#
# WHAT IS DERIVED, THOUGH. Two columns are NOT copied from upstream and are
# recomputed here: the expected value's spelling (`failure` versus the entry's
# own href) and the `runnable` classification. See the note on the classifier.

# The expected value the fixture records for an upstream entry. Upstream states
# it two ways -- `failure: true`, or a full set of component fields whose `href`
# is the serialization -- and the fixture flattens both into one column.
wpt_expected_value <- function(entry) {
  if (isTRUE(entry$failure)) return("failure")
  href <- entry$href
  if (is.null(href) || !is.character(href)) {
    stop("FATAL: upstream entry for input ", encodeString(entry$input),
         " is neither a failure case nor carries an href -- the format changed ",
         "and this reader would otherwise invent an expectation.", call. = FALSE)
  }
  href
}

# THE RUNNABLE CLASSIFIER, AND WHY IT IS A RECONSTRUCTION.
#
# `runnable` is the fixture's own column: it records whether rurl can be pointed
# at the row at all. The scratch builders computed it and nothing preserved the
# rule, so this is a rule RE-CONSTRUCTED to reproduce the committed
# classification -- 267/267 and 24/24 -- not a rule recovered from the builder.
# Say that plainly, because a reconstruction that fits its own training data
# proves nothing on its own. What it buys is the FUTURE: once the rule is
# executable, a row silently re-classified later stops agreeing with it.
#
# The rule has two parts, and both were measured:
#
#   base-relative -- upstream supplies a `base` that is load-bearing, so the row
#     tests relative resolution, which THIS harness does not exercise: it scores
#     absolute parsing. (rurl itself does resolve references -- `resolve_url()`
#     -- and those rows are scored by tests/testthat/test-wpt-base-relative.R.
#     The label marks a scope boundary of this fixture, not a package limit.)
#     A base of "about:blank" is NOT load-bearing when the input is
#     itself absolute, which is why the ada group runs 20 of its 24 rows despite
#     nearly all of them carrying that base -- and it IS load-bearing when the
#     input is a relative reference, which is why "#x" does not run.
#
#   nul-byte -- the input contains U+0000, which cannot survive an R character
#     vector, so the row is recorded but not executed. Only wpt-urltestdata has
#     these (3 rows).
#
# Returns a REASON, not a label. The two groups spell the same reason
# differently in their `runnable` column ("no-relative-resolution" versus a bare
# "no"), and a shared classifier that returned a label would have to pick one.
wpt_runnable_reason <- function(entry) {
  # U+0000 arrives here as the shim code point -- see json_text_nul_safe().
  # A raw NUL cannot reach this line: it cannot exist in an R string at all.
  has_nul <- grepl(NUL_SHIM_CHAR, entry$input, fixed = TRUE)
  base <- entry$base
  load_bearing_base <- !is.null(base) && is.character(base) &&
    (!identical(base, "about:blank") || !wpt_occupies_scheme_position(entry$input))
  # Both at once is unmodeled: the fixture has no such row, so which label wins
  # was never decided, and guessing would put a row under a reason nobody chose.
  if (has_nul && load_bearing_base) {
    stop("FATAL: upstream entry ", encodeString(entry$input),
         " is both NUL-bearing and base-relative -- the committed corpus has no ",
         "such row, so this classifier has no recorded precedence to apply.",
         call. = FALSE)
  }
  if (load_bearing_base) return("base-relative")
  if (has_nul) return("nul-byte")
  "runnable"
}

# "Does this input occupy the scheme position?" -- the only sense the classifier
# needs, because that is what decides whether a base is consulted at all.
#
# THE NAME IS PART OF THE CLAIM, so it says `occupies_scheme_position` and not
# `is_absolute` (renamed in RURL-drkcvzex). This is FITTED APPLICABILITY
# METADATA, not an absoluteness oracle: it answers "can rurl be pointed at this
# row without a base?", which is a question about this fixture's `runnable`
# column, and it deliberately DISAGREES with the WHATWG notion of an absolute
# URL. Reading it as a spec predicate is the specific error the old name
# invited, and it would be wrong in both directions -- see below.
#
# IT IS ALSO NOT RFC 3986's SCHEME PRODUCTION, and that difference was found by
# measurement rather than chosen. The first cut of this rule WAS that production
# (ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) ":"). It reproduces all 267
# wpt-urltestdata rows -- and disagrees with the committed corpus on exactly one
# ada row: `schéme://example.com` (ada-017), whose scheme is invalid because of
# the non-ASCII é. WHATWG would indeed fall back to the base there, and this
# function says TRUE anyway, because the fixture RUNS that row: rurl can be
# pointed at it and rejects it. What makes a row unrunnable is needing a base to
# have a meaning at all, not having a VALID scheme.
#
# So the rule is positional: a non-empty run of characters that are none of
# "/", "?", "#" or ":", followed by ":". Non-empty because ":foo" has an EMPTY
# scheme and is a relative reference, not a scheme-ful one.
#
# WHAT ITS AGREEMENT WITH THE CORPUS DOES AND DOES NOT ESTABLISH. Reproducing all
# 291 committed classifications is the FIT, not evidence the rule is right -- the
# 291 rows are the data it was fitted to, so it could hardly do otherwise, and
# the ada-017 finding above is the proof that a perfect fit on one corpus is
# compatible with a wrong rule. What the fit buys is entirely prospective: once
# the rule is executable, a row silently re-classified LATER stops agreeing with
# it. Anything stronger would have to come from the builder that originally
# computed the column, and that rule was never preserved.
wpt_occupies_scheme_position <- function(input) {
  grepl("^[^/?#:]+:", input)
}

# Every upstream entry, flattened to the three things a re-location check reads.
wpt_upstream_frame <- function(cases) {
  data.frame(
    input = vapply(cases, function(e) e$input, character(1)),
    expected = vapply(cases, wpt_expected_value, character(1)),
    reason = vapply(cases, wpt_runnable_reason, character(1)),
    stringsAsFactors = FALSE
  )
}

# THE MACHINE-READABLE RESTATEMENT, and why it needs its own check.
#
# `standard_expectation` is prose-ish: it holds either the literal "failure" or
# a serialization. `oracle_kind` / `oracle_value` are a later, machine-readable
# restatement of the same fact, and a restatement can drift from what it
# restates -- which is exactly the check the tier-3 gates carry for their own
# restatement columns. Found here by falsification: corrupting a restatement
# column left the first cut of both tier-2 gates green.
#
# The mapping is exact across all 291 tier-2 rows, measured not assumed:
#
#   kind  = "not-runnable" when the row does not run;
#           "failure"      when it runs and upstream rejects;
#           "exact"        otherwise
#   value = the expectation for "exact", and NA for the other two -- a rejected
#           or unrun case has no serialization to carry
wpt_restatement <- function(runs, expectation) {
  kind <- ifelse(!runs, "not-runnable",
                 ifelse(expectation == "failure", "failure", "exact"))
  list(kind = kind,
       value = ifelse(kind == "exact", expectation, NA_character_))
}

# `fsss_whatwg` IS ALSO GRADED, BUT NOT HERE AND NOT AS PROVENANCE. It is a
# captured rurl output column, so comparing it to `oracle_value` is an
# implementation-conformance assertion rather than a statement about where the
# expected value came from. It lives in tools/oracle/check-fsss-conformance.R
# (RURL-drkcvzex) and is reported by each caller under its own heading; that file
# carries the reasoning for why the comparison is deliberately unconditional on
# `rurl_deviation`. It was inside this function, which made a conformance check
# read as part of "ORACLE RE-LOCATION: PASS".
wpt_check_restatement <- function(committed, runs) {
  want <- wpt_restatement(runs, committed$standard_expectation)
  fail <- character(0)
  bad <- which(committed$oracle_kind != want$kind)
  if (length(bad)) {
    fail <- c(fail, sprintf("%d row(s) restate a different oracle_kind:",
                            length(bad)))
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  recorded=%s  derived=%s", committed$id[i],
                              committed$oracle_kind[i], want$kind[i]))
    }
  }
  bad <- which(!identical_na(committed$oracle_value, want$value))
  if (length(bad)) {
    fail <- c(fail, sprintf("%d row(s) restate a different oracle_value:",
                            length(bad)))
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  recorded=%s  derived=%s", committed$id[i],
                              encodeString(as.character(committed$oracle_value[i])),
                              encodeString(as.character(want$value[i]))))
    }
  }
  fail
}

# `whatwg_expected` -- THE THIRD RESTATEMENT, AND IT NOW HAS AN OWNER.
#
# It was deliberately ungraded here, on two stated reasons, and one of them was
# wrong (RURL-drkcvzex). The good half: its NA PATTERN is exactly
# `divergence_class` in (`aligned`, `not-runnable`), a column derived from how
# rurl ANSWERS, so an oracle module that derived the pattern would be consulting
# the implementation it grades. That still holds, and this check does not touch
# the pattern -- absence is not graded at all.
#
# The bad half was extending that to the VALUE. A non-NA `whatwg_expected` is the
# WHATWG expectation for the row, and on every tier-2 row the pinned upstream
# bytes state it -- so it is derivable from the same independent source as
# `standard_expectation`, with no reference to rurl. Leaving it out meant a
# corrupted value could pass: the structural assertions in
# tests/testthat/test-external-url-vectors.R relate it to `divergence_class` and
# `rfc3986_expected` rather than to any upstream fact, and a row carrying a
# `rurl_deviation` can satisfy them with a wrong value. Falsification confirmed
# it: corrupting `whatwg_expected` on ada-003, a deviation-carrying row, left
# every gate green.
#
# `derived` is the expectation read out of the pinned upstream bytes, aligned
# row-for-row with `committed`, and NA where the caller cannot align a row. A
# graded row with no derived value is a FAILURE, not a skip -- "we could not
# check it" must never report as "it agrees".
wpt_check_whatwg_expected <- function(committed, derived, min_rows,
                                      min_deviation_rows = 0L) {
  fail <- character(0)
  graded <- which(!is.na(committed$whatwg_expected))
  undeliverable <- graded[is.na(derived[graded])]
  if (length(undeliverable)) {
    fail <- c(fail, sprintf(
      paste0("%d row(s) record a whatwg_expected that could not be derived from ",
             "the pinned upstream bytes at all: %s"),
      length(undeliverable),
      paste(committed$id[utils::head(undeliverable, 5L)], collapse = ", ")))
  }
  cmp <- setdiff(graded, undeliverable)
  bad <- cmp[committed$whatwg_expected[cmp] != derived[cmp]]
  if (length(bad)) {
    fail <- c(fail, sprintf("%d row(s) record a whatwg_expected that upstream",
                            length(bad)))
    fail <- c(fail, "    does not state:")
    for (i in utils::head(bad, 10L)) {
      fail <- c(fail, sprintf("    %s  recorded=%s  upstream=%s",
                              committed$id[i],
                              encodeString(committed$whatwg_expected[i]),
                              encodeString(derived[i])))
    }
  }
  if (length(graded) < min_rows) {
    fail <- c(fail, sprintf(
      paste0("only %d row(s) carry a whatwg_expected, and the floor is %d -- a ",
             "column check whose population shrinks passes for the wrong reason"),
      length(graded), min_rows))
  }
  ndev <- sum(!is.na(committed$rurl_deviation[graded]))
  if (ndev < min_deviation_rows) {
    fail <- c(fail, sprintf(
      paste0("only %d graded row(s) carry a rurl_deviation, and the floor is %d ",
             "-- those are the rows the fixture's own structural assertions ",
             "cannot referee, so they are the reason this check exists"),
      ndev, min_deviation_rows))
  }
  attr(fail, "graded") <- length(graded)
  attr(fail, "deviating") <- ndev
  fail
}

# NA-aware elementwise equality: NA == NA is TRUE here, because "both absent" is
# agreement for a column whose absence is meaningful.
identical_na <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

# The fixture's `input` column is NA wherever the value cannot survive a CSV
# round trip, so `input_json` is the byte-exact carrier and is authoritative
# everywhere. This mirrors the fixture's own convention and the tier-1 readers.
#
# The shim is applied cell by cell for the same reason it is applied upstream:
# without it jsonlite truncates the three NUL-bearing inputs at the NUL and the
# comparison silently stops covering everything after it.
fixture_inputs <- function(d) {
  cells <- json_text_nul_safe(d$input_json, "fixture input_json")
  vapply(cells, jsonlite::fromJSON, character(1), USE.NAMES = FALSE)
}
