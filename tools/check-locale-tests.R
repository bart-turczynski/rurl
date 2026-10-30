#!/usr/bin/env Rscript
#
# The locale-relevant tests under LC_ALL=C, as a positive assertion
# (RURL-izzuenjx, RURL-kmpnbvdl). A cheap gate of the fleet-ci local gate's
# full tier (.fleet-ci.yml), run as:
#
#   LC_ALL=C LANG=C Rscript tools/check-locale-tests.R
#
# rurl's output must not depend on LC_CTYPE: the same input gives the same
# values and the same Encoding() marks under a UTF-8 locale and under C. The
# full tier's R CMD check runs the suite under C.UTF-8, so a defect that only
# appears under a non-UTF-8 charset is invisible there. This runs the test
# files that exercise the charset (the contract in test-locale-invariance.R,
# the base-R string layer, latin1 and invalid UTF-8 input, the encoding
# fixtures) under C. It replaces tools/verify.R's former locale stage, which
# reran the whole suite under C on every push; the whole-suite C run is now
# fleet-ci's t2-locale job, in T2 (the weekly deep-check and every release).
#
# POSITIVE, not merely "no failure": it fails unless
#   * the session really is in the C locale (LC_CTYPE "C", not UTF-8): an
#     LC_ALL that never reached R would otherwise pass vacuously;
#   * every file named below exists, so a rename cannot drop one silently;
#   * every one of them ran at least one passing expectation, with no
#     failure, no error and no skip. A skip would mean a locale the test asks
#     for is missing: list it in .fleet-ci.yml's `locales`.
#
# test-osv.R and test-security.R are not in the list: they audit third-party
# advisories, a fact about the world rather than about the tree.

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

contexts <- sub("^test-", "", sub("[.][Rr]$", "", FILES))
filter <- paste0("^(", paste(gsub(".", "\\.", contexts, fixed = TRUE),
                              collapse = "|"), ")$")
res <- testthat::test_local(".", filter = filter, reporter = "summary",
                            stop_on_failure = FALSE)
df <- as.data.frame(res)

per_file <- lapply(FILES, function(f) {
  rows <- df[df$file == f, , drop = FALSE]
  c(passed = sum(rows$passed), failed = sum(rows$failed),
    skipped = sum(rows$skipped), error = sum(rows$error))
})
names(per_file) <- FILES

bad <- character(0)
for (f in FILES) {
  n <- per_file[[f]]
  cat(sprintf("check-locale-tests: %-36s passed %4d  failed %d  skipped %d  error %d\n",
              f, n[["passed"]], n[["failed"]], n[["skipped"]], n[["error"]]))
  if (n[["passed"]] < 1L || n[["failed"]] || n[["skipped"]] || n[["error"]]) {
    bad <- c(bad, f)
  }
}
if (length(bad)) {
  fail("under LC_ALL=C, these did not run clean with at least one passing ",
       "expectation: ", paste(bad, collapse = ", "))
}
cat("check-locale-tests: PASS: ", length(FILES), " file(s), ",
    sum(vapply(per_file, `[[`, numeric(1), "passed")),
    " expectation(s) under LC_ALL=C\n", sep = "")
