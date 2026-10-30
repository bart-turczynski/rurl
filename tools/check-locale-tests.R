#!/usr/bin/env Rscript
#
# The test suite under LC_ALL=C, with a positive assertion on the
# locale-relevant files (RURL-izzuenjx, RURL-kmpnbvdl). A cheap gate of the
# fleet-ci local gate's full tier (.fleet-ci.yml), run as:
#
#   LC_ALL=C LANG=C Rscript tools/check-locale-tests.R
#
# rurl's output must not depend on LC_CTYPE: the same input gives the same
# values and the same Encoding() marks under a UTF-8 locale and under C. The
# full tier's R CMD check runs the suite under C.UTF-8, and fleet-ci's
# t2-locale leg, though it sets LC_ALL=C, runs it through tests/testthat.R,
# which selects en_US.UTF-8, so a defect that only appears under a
# non-UTF-8 charset is invisible to both. This is the run that sees it: the
# whole suite under C through testthat::test_local(), which does not source
# tests/testthat.R, as tools/verify.R's former locale stage (the manifest's
# `Tests (LC_ALL=C)` cell) did on every push.
#
# It EXCLUDES the two third-party advisory audits, test-security.R (OSS
# Index) and test-osv.R (OSV) (SEOR-fftbjnpl): an advisory is a fact about
# the world rather than about the tree. test_local() sets NOT_CRAN=true, so
# their skip_on_cran() would not keep them out. T2's osv-audit and
# security-audit jobs run them.
#
# POSITIVE, not merely "no failure": it fails unless
#   * the session really is in the C locale (LC_CTYPE "C", not UTF-8): an
#     LC_ALL that never reached R would otherwise pass vacuously;
#   * no test anywhere in the suite failed or errored;
#   * every locale-relevant file named below exists, so a rename cannot drop
#     one silently, and ran at least one passing expectation with no skip.
#     A skip there would mean a locale the test asks for is missing: list it
#     in .fleet-ci.yml's `locales`. Skips elsewhere are reported, not fatal,
#     as in the former cell.

FILES <- c(
  "test-locale-invariance.R",
  "test-strings-base.R",
  "test-encoding-fixtures.R",
  "test-latin1-input.R",
  "test-resolve-latin1-input.R",
  "test-invalid-utf8-batch.R",
  "test-query-invalid-utf8.R",
  "test-path-invalid-utf8-yarkccco.R"
)

fail <- function(...) {
  cat("check-locale-tests: FAIL: ", ..., "\n", sep = "")
  quit(status = 1L)
}

ctype <- Sys.getlocale("LC_CTYPE")
utf8 <- isTRUE(l10n_info()[["UTF-8"]])
cat("check-locale-tests: LC_CTYPE=", ctype, ", UTF-8=", utf8, "\n", sep = "")
if (!identical(ctype, "C") || utf8) {
  fail("the session is not in the C locale; run it under LC_ALL=C LANG=C")
}

present <- file.exists(file.path("tests", "testthat", FILES))
if (!all(present)) {
  fail("missing test file(s): ", paste(FILES[!present], collapse = ", "))
}

t0 <- Sys.time()
res <- testthat::test_local(".", filter = "^(security|osv)$", invert = TRUE,
                            reporter = "summary", stop_on_failure = FALSE)
secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
df <- as.data.frame(res)

tally <- function(rows) {
  c(passed = sum(rows$passed), failed = sum(rows$failed),
    skipped = sum(rows$skipped), error = sum(rows$error))
}

whole <- tally(df)
cat(sprintf(paste("check-locale-tests: whole suite, %d file(s): passed %d",
                  "failed %d  skipped %d  error %d  (%.0fs)\n"),
            length(unique(df$file)), whole[["passed"]], whole[["failed"]],
            whole[["skipped"]], whole[["error"]], secs))
broken <- unique(df$file[df$failed > 0 | df$error])
if (length(broken)) {
  fail("under LC_ALL=C, these file(s) failed or errored: ",
       paste(broken, collapse = ", "))
}

bad <- character(0)
for (f in FILES) {
  n <- tally(df[df$file == f, , drop = FALSE])
  cat(sprintf("check-locale-tests: %-36s passed %4d  failed %d  skipped %d  error %d\n",
              f, n[["passed"]], n[["failed"]], n[["skipped"]], n[["error"]]))
  if (n[["passed"]] < 1L || n[["skipped"]]) {
    bad <- c(bad, f)
  }
}
if (length(bad)) {
  fail("under LC_ALL=C, these locale-relevant file(s) did not run clean with ",
       "at least one passing expectation: ", paste(bad, collapse = ", "))
}
cat("check-locale-tests: PASS: the whole suite (", whole[["passed"]],
    " expectation(s)) under LC_ALL=C, ", length(FILES),
    " locale-relevant file(s) asserted\n", sep = "")
