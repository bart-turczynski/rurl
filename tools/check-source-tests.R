#!/usr/bin/env Rscript
#
# The tests that need the source tree, run from it, as a positive assertion.
# A cheap gate of the fleet-ci local gate's full tier (.fleet-ci.yml):
#
#   Rscript tools/check-source-tests.R
#
# R CMD check runs the suite against the installed package, where tools/,
# design/ and R/ do not exist, so these tests skip there by design (each
# skip is on .fleet-ci.yml's allowed_skips, pointing here). The former
# locale cell ran the whole suite with testthat::test_local() from the
# source tree, which reached them; this runs exactly them that way, so the
# gate still checks what they assert:
#
#   test-uts46-transcription-pin.R  tools/oracle/derive-ip-obfuscation.R
#                                   transcribes the 29 UTS-46 mappings
#   test-url-join.R                 the VD-001 row of the deferral register
#   test-stringi-bool-key-query.R   the migrated R/ files call no boolean or
#                                   replacement stringi function
#
# POSITIVE: it fails unless every file named below exists and ran at least
# one passing expectation, with no failure, no error and no skip.

FILES <- c(
  "test-uts46-transcription-pin.R",
  "test-url-join.R",
  "test-stringi-bool-key-query.R"
)

fail <- function(...) {
  cat("check-source-tests: FAIL: ", ..., "\n", sep = "")
  quit(status = 1L)
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

bad <- character(0)
total <- 0
for (f in FILES) {
  rows <- df[df$file == f, , drop = FALSE]
  n <- c(passed = sum(rows$passed), failed = sum(rows$failed),
         skipped = sum(rows$skipped), error = sum(rows$error))
  total <- total + n[["passed"]]
  cat(sprintf("check-source-tests: %-32s passed %4d  failed %d  skipped %d  error %d\n",
              f, n[["passed"]], n[["failed"]], n[["skipped"]], n[["error"]]))
  if (n[["passed"]] < 1L || n[["failed"]] || n[["skipped"]] || n[["error"]]) {
    bad <- c(bad, f)
  }
}
if (length(bad)) {
  fail("from the source tree, these did not run clean with at least one ",
       "passing expectation: ", paste(bad, collapse = ", "))
}
cat("check-source-tests: PASS: ", length(FILES), " file(s), ", total,
    " expectation(s) from the source tree\n", sep = "")
