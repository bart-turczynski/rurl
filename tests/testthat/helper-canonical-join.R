# canonical_join() is deprecated (RURL-atrvocqe): every call warns once, with
# class "rurl_canonical_join_deprecated". Tests that exercise its VALUES mute
# exactly that class with cj_deprecated(), and nothing else -- an unrelated
# warning still surfaces. The deprecation itself is pinned in
# test-canonical-join-deprecation.R.
cj_deprecated <- function(expr) {
  suppressWarnings(expr, classes = "rurl_canonical_join_deprecated")
}

# canonical_join() also warns (class "rurl_legacy_join_dial_warning") whenever
# a legacy presentation/cleaning dial is forwarded through `...` -- P3.1 D-E.1
# ("comparison-irrelevant cleaning/display arguments warn") and D-E.3 ("no
# caller is silently re-matched"). The warning is purely additive: results are
# byte-identical with and without it.
#
# Most tests below exercise those dials for their VALUES, not for the warning,
# so they mute exactly those two condition classes (the dial warning and the
# deprecation) and nothing else. The dial warning itself, its once-per-call
# behavior, and the value-invariance proof live in
# test-canonical-join-legacy-dials.R.
cj_legacy <- function(expr) {
  suppressWarnings(
    expr,
    classes = c(
      "rurl_canonical_join_deprecated", "rurl_legacy_join_dial_warning"
    )
  )
}
