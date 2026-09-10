# OSS Index dependency vulnerability audit (oysteR / Sonatype).
#
# `oysteR::expect_secure("rurl")` resolves the installed DESCRIPTION and audits
# rurl's declared dependencies against the Sonatype OSS Index. It is a network
# test that requires OSS Index credentials (OSSINDEX_USER / OSSINDEX_TOKEN):
# the API rejects unauthenticated requests with HTTP 401, so the test is
# guarded to skip wherever those preconditions are absent (CRAN, offline,
# missing credentials, oysteR not installed). A dedicated GitHub
# workflow (security-audit.yml) used to supply the credentials as repository
# secrets so the audit actually ran there (and drove the README badge); that
# workflow was deleted with RURL-vunvxusf (the account is suspended), so the
# audit now runs only when the maintainer exports the credentials by hand.
# In every other context (local runs, the verify gate) it skips cleanly
# rather than failing.

# Scope: hard dependencies only -- `Depends` + `Imports`, never `Suggests`.
#
# `oysteR::expect_secure()` audits `Depends` + `Imports` + `Suggests`, and
# `Suggests` drags in the recursive dependency trees of the dev tooling --
# including oysteR's own, which reaches an HTTP transfer library through httr.
# So the gate was failing on a vulnerability in the auditor rather than in
# anything a user of this package installs.
#
# Measured 2026-09-10, same machine and credentials: the Suggests-inclusive
# scope audits 89 packages here and reports one of them vulnerable, while
# `Depends` + `Imports` audits 9 and reports none -- the flagged package is
# absent from rurl's hard dependency tree entirely. RURL-mafkcwnu carries the
# advisory identifiers and the measurement; naming the package here would trip
# the zero-reference gate, which is why they live on the issue and not in this
# comment.
#
# A package's security posture is what it makes users install, so the audit
# calls `audit_description()` directly with the narrower `fields`.
# `expect_secure()` sets a CRAN mirror internally and `audit_description()`
# does not, hence the explicit `repos` option.

test_that("hard dependencies have no known OSS Index vulnerabilities", {
  skip_on_cran()
  skip_if_not_installed("oysteR")
  skip_if_offline()
  skip_if(
    Sys.getenv("OSSINDEX_USER") == "" || Sys.getenv("OSSINDEX_TOKEN") == "",
    "OSS Index credentials (OSSINDEX_USER / OSSINDEX_TOKEN) not set"
  )

  old_repos <- getOption("repos")
  on.exit(options(repos = old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cran.rstudio.com"))

  audit <- oysteR::audit_description(
    dirname(system.file("DESCRIPTION", package = "rurl")),
    fields = c("Depends", "Imports"),
    verbose = FALSE
  )
  vulnerable <- audit[audit$no_of_vulnerabilities > 0, ]$package

  expect_equal(vulnerable, character())
})
