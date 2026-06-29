# OSS Index dependency vulnerability audit (oysteR / Sonatype).
#
# `oysteR::expect_secure("rurl")` resolves the installed DESCRIPTION and audits
# rurl's declared dependencies against the Sonatype OSS Index. It is a network
# test that requires OSS Index credentials (OSSINDEX_USER / OSSINDEX_TOKEN):
# the API rejects unauthenticated requests with HTTP 401, so the test is
# guarded to skip wherever those preconditions are absent (CRAN, offline,
# missing credentials, oysteR not installed). The dedicated
# security-audit.yml workflow supplies the credentials as repository secrets
# so the audit actually runs there (and drives the README badge); in every
# other context (local runs, the verify/full-check/rhub suites) it skips
# cleanly rather than failing.

test_that("declared dependencies have no known OSS Index vulnerabilities", {
  skip_on_cran()
  skip_if_not_installed("oysteR")
  skip_if_offline()
  skip_if(
    Sys.getenv("OSSINDEX_USER") == "" || Sys.getenv("OSSINDEX_TOKEN") == "",
    "OSS Index credentials (OSSINDEX_USER / OSSINDEX_TOKEN) not set"
  )

  oysteR::expect_secure("rurl")
})
