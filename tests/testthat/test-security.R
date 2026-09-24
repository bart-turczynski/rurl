# OSS Index dependency vulnerability audit (oysteR / Sonatype).
#
# `oysteR::audit_description()` resolves the installed DESCRIPTION and audits
# rurl's hard dependencies against the Sonatype OSS Index. It is a network
# test that requires OSS Index credentials (OSSINDEX_USER / OSSINDEX_TOKEN):
# the API rejects unauthenticated requests with HTTP 401. Every reported
# advisory must have a disposition in helper-security.R; that file states the
# three rules this test enforces.
#
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
# comment. Re-measured 2026-09-24: `Depends` + `Imports` audits 10 packages and
# reports none.
#
# A package's security posture is what it makes users install, so the audit
# calls `audit_description()` directly with the narrower `fields`.
# `expect_secure()` sets a CRAN mirror internally and `audit_description()`
# does not, hence the explicit `repos` option.
#
# WHERE THIS RUNS, AND WHY THE PRECONDITIONS ARE NOT ALWAYS SKIPS.
#
# Everywhere ordinary -- a local `testthat::test_local()`, a plain
# `R CMD check --as-cran` -- a missing precondition is a skip. That is right:
# a developer without OSS Index credentials is not a security regression.
#
# In a job that exists to run this audit, it is not right: a job without
# credentials would skip, report success, and have audited nothing -- the
# failure `PUNY-rsxtbbln` records in punycoder. A green job that audited
# nothing is worse than no job, because it answers a question it was never
# asked. So under OSSINDEX_AUDIT_REQUIRED=true every precondition below becomes
# a hard failure with a message naming what is missing.
#
# rurl has NO such job today. The GitHub workflow that used to run this audit
# (security-audit.yml) was deleted with RURL-vunvxusf, and `.gitlab-ci.yml`
# carries no replacement, so nothing sets the flag.
#
# NOT IN THE PRE-PUSH GATE. `tools/verify.R`'s LC_ALL=C cell runs
# `testthat::test_local()`, which sets NOT_CRAN=true, so `skip_on_cran()` does
# not fire there -- and `~/.Renviron` puts the credentials in scope inside R.
# This audit therefore ran live on every push, and a new upstream advisory
# blocked an unrelated push on 2026-09-09. That cell now excludes this file and
# test-osv.R (SEOR-fftbjnpl). `R CMD check` leaves NOT_CRAN unset, so the check
# stage skips it too. The consequence, stated plainly: NOTHING runs this audit
# automatically. Run it deliberately with
# `testthat::test_local(filter = "security")`; add
# `OSSINDEX_AUDIT_REQUIRED=true` to make a missing precondition fail.

test_that("the OSS Index allow-list is well formed", {
  expect_equal(oss_index_allowlist_violations(oss_index_allowlist), character())
})

test_that("the allow-list validator rejects rows that are not decisions", {
  sound <- list(
    id = "CVE-2026-18924",
    package = "somepkg",
    version_seen = "8.0.0",
    review = as.Date("2026-12-01"),
    reason = paste(
      "A rationale long enough to be an argument rather than a placeholder,",
      "naming the advisory, the exposure assessed and why it is accepted."
    )
  )
  expect_equal(oss_index_allowlist_violations(list(sound)), character())

  broken <- function(field, value) {
    row <- sound
    row[[field]] <- value
    oss_index_allowlist_violations(list(row))
  }

  expect_match(
    broken("id", "GHSA-xxxx"),
    "single CVE identifier",
    fixed = TRUE
  )
  expect_match(broken("package", ""), "single package name", fixed = TRUE)
  expect_match(
    broken("version_seen", "not-a-version"),
    "parseable version",
    fixed = TRUE
  )
  expect_match(broken("review", "2026-12-01"), "single Date", fixed = TRUE)
  expect_match(
    broken("reason", "unfixable"),
    "too short to be an argument",
    fixed = TRUE
  )

  expect_match(
    oss_index_allowlist_violations(list(sound[-5])),
    "missing field(s): reason",
    fixed = TRUE
  )
  expect_match(
    oss_index_allowlist_violations(list(c(sound, list(owner = "me")))),
    "unknown field(s): owner",
    fixed = TRUE
  )
  expect_match(
    oss_index_allowlist_violations(list(sound, sound)),
    "duplicate allow-list id",
    fixed = TRUE
  )
})

test_that("hard dependencies report only allow-listed OSS Index advisories", {
  # A job that exists to run this audit. A precondition it cannot meet is a
  # failure there, never a skip -- see the header.
  required <- identical(Sys.getenv("OSSINDEX_AUDIT_REQUIRED"), "true")
  no_credentials <- Sys.getenv("OSSINDEX_USER") == "" ||
    Sys.getenv("OSSINDEX_TOKEN") == ""

  if (required) {
    if (!requireNamespace("oysteR", quietly = TRUE)) {
      stop(
        "OSSINDEX_AUDIT_REQUIRED is set but {oysteR} is not installed, so ",
        "this run cannot audit anything. Install it or unset the flag; do ",
        "not let the run report success."
      )
    }
    if (no_credentials) {
      stop(
        "OSSINDEX_AUDIT_REQUIRED is set but OSSINDEX_USER / OSSINDEX_TOKEN ",
        "are absent, so OSS Index would reject every request with HTTP 401 ",
        "and this run would report success having audited nothing. Set both ",
        "(in ~/.Renviron locally, or as CI/CD variables in a pipeline)."
      )
    }
    # Deliberately no skip_if_offline() on this path: a network the run cannot
    # reach is the same vacuous green as a credential it does not have, so let
    # the audit attempt the call and fail on the transport error.
  } else {
    skip_on_cran()
    skip_if_not_installed("oysteR")
    skip_if_offline()
    skip_if(
      no_credentials,
      "OSS Index credentials (OSSINDEX_USER / OSSINDEX_TOKEN) not set"
    )
  }

  old_repos <- getOption("repos")
  on.exit(options(repos = old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cran.rstudio.com"))

  audit <- oysteR::audit_description(
    dirname(system.file("DESCRIPTION", package = "rurl")),
    fields = c("Depends", "Imports"),
    verbose = FALSE
  )
  found <- oss_index_reported(audit)
  allowed <- vapply(oss_index_allowlist, function(row) row$id, character(1))

  # An audit that resolved nothing is not a clean audit. Without this an empty
  # result satisfies rules A and B vacuously, which is the same green-on-
  # nothing failure the credential guard above exists to stop.
  expect_gt(nrow(audit), 0)

  # Rule A -- an advisory reported and not allow-listed.
  expect_equal(sort(setdiff(found$id, allowed)), character())

  # Rule B -- an allow-listed advisory no longer reported. The list may not
  # over-permit, so a row that has outlived its justification fails here.
  expect_equal(sort(setdiff(allowed, found$id)), character())

  # Rule C -- drift warns, never fails. See helper-security.R.
  for (row in oss_index_allowlist) {
    if (Sys.Date() > row$review) {
      warning(
        sprintf(
          "OSS Index allow-list row %s is past its %s review date.",
          row$id,
          format(row$review)
        ),
        call. = FALSE
      )
    }
    hit <- found[found$id == row$id, ]
    if (
      nrow(hit) > 0 &&
        package_version(hit$version[1]) > package_version(row$version_seen)
    ) {
      warning(
        sprintf(
          paste(
            "OSS Index allow-list row %s was written against %s %s;",
            "the audit now reports %s. Re-read the advisory."
          ),
          row$id,
          row$package,
          row$version_seen,
          hit$version[1]
        ),
        call. = FALSE
      )
    }
  }
})
