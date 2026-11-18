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
# advisory identifiers and the measurement. Re-measured 2026-09-24: `Depends` +
# `Imports` audits 10 packages and reports none.
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
    broken("package", NA_character_),
    "single package name",
    fixed = TRUE
  )
  expect_match(
    broken("package", "lib curl"),
    "single package name",
    fixed = TRUE
  )
  expect_match(
    broken("version_seen", "not-a-version"),
    "parseable version",
    fixed = TRUE
  )
  expect_match(
    broken("version_seen", NA_character_),
    "parseable version",
    fixed = TRUE
  )
  expect_match(broken("review", "2026-12-01"), "single Date", fixed = TRUE)
  expect_match(broken("review", as.Date(NA)), "single Date", fixed = TRUE)
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
    "duplicate allow-list row",
    fixed = TRUE
  )
})

test_that("the fixture row is itself well formed", {
  expect_equal(
    oss_index_allowlist_violations(list(oss_index_fixture_row("CVE-2026-1"))),
    character()
  )
})

test_that("rules A and B pass every allow-listed advisory as reported", {
  field <- function(name) {
    vapply(oss_index_allowlist, function(row) row[[name]], character(1))
  }
  found <- data.frame(
    package = field("package"),
    version = field("version_seen"),
    id = field("id")
  )
  expect_equal(
    oss_index_rule_findings(found, oss_index_allowlist),
    list(unexpected = character(), stale = character())
  )
})

test_that("rules A and B report unexpected and stale advisories", {
  rows <- list(
    oss_index_fixture_row("CVE-2026-18924"),
    oss_index_fixture_row("CVE-2026-3783")
  )
  found <- data.frame(
    package = "curl",
    version = "8.1.0",
    id = c("CVE-2026-18924", "CVE-2026-99999")
  )
  findings <- oss_index_rule_findings(found, rows)
  expect_equal(findings$unexpected, "curl CVE-2026-99999")
  expect_equal(findings$stale, "curl CVE-2026-3783")

  # An audit that reported nothing leaves every row stale.
  none <- data.frame(
    package = character(),
    version = character(),
    id = character()
  )
  findings <- oss_index_rule_findings(none, rows)
  expect_equal(findings$unexpected, character())
  expect_equal(
    findings$stale,
    c("curl CVE-2026-18924", "curl CVE-2026-3783")
  )
})

test_that("a row covers its advisory in its own package only", {
  rows <- list(oss_index_fixture_row("CVE-2026-18924"))
  found <- data.frame(
    package = "otherbinding",
    version = "1.0.0",
    id = "CVE-2026-18924"
  )
  findings <- oss_index_rule_findings(found, rows)
  expect_equal(findings$unexpected, "otherbinding CVE-2026-18924")
  expect_equal(findings$stale, "curl CVE-2026-18924")

  # A reported advisory with no package names nothing a row could cover.
  found$package <- NA_character_
  findings <- oss_index_rule_findings(found, rows)
  expect_equal(findings$unexpected, "NA CVE-2026-18924")
  expect_equal(findings$stale, "curl CVE-2026-18924")

  # Not even a row whose package or id is the string "NA".
  literal <- list(
    oss_index_fixture_row("CVE-2026-18924", "NA"),
    oss_index_fixture_row("NA", "curl")
  )
  found <- data.frame(
    package = c(NA, "curl"),
    version = "8.1.0",
    id = c("CVE-2026-18924", NA)
  )
  findings <- oss_index_rule_findings(found, literal)
  expect_equal(findings$unexpected, c("NA CVE-2026-18924", "curl NA"))
  expect_equal(findings$stale, c("NA CVE-2026-18924", "curl NA"))

  # A malformed row covers nothing and is stale as NA.
  malformed <- list(oss_index_fixture_row("CVE-2026-18924", NA_character_))
  findings <- oss_index_rule_findings(found[1, ], malformed)
  expect_equal(findings$unexpected, "NA CVE-2026-18924")
  expect_equal(findings$stale, NA_character_)
})

test_that("one advisory allow-listed for two packages is two decisions", {
  rows <- list(
    oss_index_fixture_row("CVE-2026-18924", "curl"),
    oss_index_fixture_row("CVE-2026-18924", "otherbinding")
  )
  found <- data.frame(
    package = c("curl", "otherbinding"),
    version = "8.1.0",
    id = "CVE-2026-18924"
  )
  expect_equal(oss_index_allowlist_violations(rows), character())
  expect_equal(
    oss_index_rule_findings(found, rows),
    list(unexpected = character(), stale = character())
  )

  expect_match(
    oss_index_allowlist_violations(rows[c(1, 1)]),
    "duplicate allow-list row(s): curl CVE-2026-18924",
    fixed = TRUE
  )
})

test_that("rule C warns on drift and never fails", {
  row <- list(oss_index_fixture_row("CVE-2026-18924"))
  before_review <- as.Date("2026-11-30")
  at <- function(version) {
    data.frame(package = "curl", version = version, id = "CVE-2026-18924")
  }

  expect_silent(oss_index_warn_drift(at("8.1.0"), row, before_review))
  expect_silent(oss_index_warn_drift(at(NA_character_), row, before_review))
  expect_warning(
    oss_index_warn_drift(at("8.2.0"), row, before_review),
    "was written against curl 8.1.0; the audit now reports 8.2.0",
    fixed = TRUE
  )
  expect_warning(
    oss_index_warn_drift(at("8.1.0"), row, as.Date("2026-12-02")),
    "is past its 2026-12-01 review date",
    fixed = TRUE
  )

  # Another package reporting the same advisory at a later version is not
  # drift in this row.
  other <- data.frame(
    package = c("otherbinding", "curl"),
    version = c("9.0.0", "8.1.0"),
    id = "CVE-2026-18924"
  )
  expect_silent(oss_index_warn_drift(other, row, before_review))
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
  findings <- oss_index_rule_findings(found, oss_index_allowlist)

  # An audit that resolved nothing is not a clean audit. Without this an empty
  # result satisfies rules A and B vacuously, which is the same green-on-
  # nothing failure the credential guard above exists to stop.
  expect_gt(nrow(audit), 0)

  # Rule A -- an advisory reported and not allow-listed.
  expect_equal(findings$unexpected, character())

  # Rule B -- an allow-listed advisory no longer reported. The list may not
  # over-permit, so a row that has outlived its justification fails here.
  expect_equal(findings$stale, character())

  # Rule C -- drift warns, never fails. See helper-security.R.
  oss_index_warn_drift(found, oss_index_allowlist)
})
