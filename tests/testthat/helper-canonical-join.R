# canonical_join() warns (class "rurl_legacy_join_dial_warning") whenever a
# legacy presentation/cleaning dial is forwarded through `...` -- P3.1 D-E.1
# ("comparison-irrelevant cleaning/display arguments warn") and D-E.3 ("no
# caller is silently re-matched"). The warning is purely additive: results are
# byte-identical with and without it.
#
# Most tests below exercise those dials for their VALUES, not for the warning,
# so they mute exactly that one condition class and nothing else -- an
# unrelated warning still surfaces. The warning itself, its once-per-call
# behavior, and the value-invariance proof live in
# test-canonical-join-legacy-dials.R.
cj_legacy <- function(expr) {
  suppressWarnings(expr, classes = "rurl_legacy_join_dial_warning")
}
