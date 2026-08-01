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
#     tests relative resolution and rurl (absolute parse-only) is the wrong
#     referee. A base of "about:blank" is NOT load-bearing when the input is
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
    (!identical(base, "about:blank") || !wpt_is_absolute(entry$input))
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

# "Is this input an absolute URL?" in the only sense the classifier needs: does
# it begin with a scheme, so that a base is never consulted. This is RFC 3986's
# scheme production (ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) ":"), applied to
# the raw input. It deliberately does not attempt to parse the rest.
wpt_is_absolute <- function(input) {
  grepl("^[A-Za-z][A-Za-z0-9+.-]*:", input)
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
