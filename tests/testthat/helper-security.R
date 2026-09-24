# Disposition machinery for the OSS Index dependency audit in
# test-security.R. The allow-list lives here rather than in the test file
# because more than one test block reads it -- the offline well-formedness
# check and the network audit itself.
#
# WHY A LIST THAT IS EMPTY. The contract this file implements is "every
# reported advisory has a current, explicit disposition", replacing "the scan
# reports nothing" (SEOR-fkvlzltx, deciding SEOR-feqgnbwt route (a);
# SEOR-fftbjnpl ported it here). For rurl the two contracts coincide *today*:
# measured 2026-09-10 and again 2026-09-24 with live OSS Index credentials,
# the hard dependency closure reports zero advisories, so there is nothing to
# dispose of and the list is empty.
#
# Rows are deliberately NOT written ahead of an advisory (SEOR-sxvcbuia): rule
# B below fails a row that is not currently reported, so an anticipatory row is
# stale on arrival and would fail the gate on the day it is added. The
# machinery is what is portable; the rows are not. Adding one later is a
# one-block edit to `oss_index_allowlist`.
#
# Three rules keep the list honest. test-security.R enforces them:
#
#   A  UNEXPECTED  an advisory reported and not allow-listed FAILS. This is
#                  the gate still doing its job.
#   B  STALE       an allow-listed advisory NO LONGER reported FAILS. An
#                  allow-list may not over-permit; a row that has outlived its
#                  justification is deleted, not left to widen the exemption
#                  silently. Same rule as C0 in the zero-reference gate
#                  under tools/.
#   C  DRIFT       past its review date, or audited at a package version later
#                  than `version_seen`, a row WARNS. Deliberately not a
#                  failure: a calendar date that turns every run red is how a
#                  gate teaches people to reach for --no-verify.
#
# Adding a row is a decision, not a formality. Rule B is what keeps it one.
#
# The reference implementation with live rows is in sitemapr's
# `tests/testthat/helper-security.R`.

oss_index_allowlist <- list()

# The fields every allow-list row carries, in the order they are documented.
oss_index_allowlist_fields <- c(
  "id",
  "package",
  "version_seen",
  "review",
  "reason"
)

# Problems with a set of allow-list rows, as a character vector; empty when the
# set is well formed. Written as a function returning findings rather than as a
# block of expectations so that it can be exercised against fixtures -- with no
# live rows, a loop of expectations over `oss_index_allowlist` would assert
# nothing at all and still report as a passing test.
oss_index_allowlist_violations <- function(rows) {
  out <- character()

  parses_as_version <- function(x) {
    tryCatch(
      {
        package_version(x)
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
  }

  for (i in seq_along(rows)) {
    row <- rows[[i]]
    label <- sprintf("row %d", i)

    absent <- setdiff(oss_index_allowlist_fields, names(row))
    unknown <- setdiff(names(row), oss_index_allowlist_fields)
    if (length(absent) > 0) {
      out <- c(
        out,
        sprintf("%s: missing field(s): %s", label, toString(absent))
      )
    }
    if (length(unknown) > 0) {
      out <- c(
        out,
        sprintf("%s: unknown field(s): %s", label, toString(unknown))
      )
    }
    if (length(absent) > 0) {
      next
    }

    label <- sprintf("row %d (%s)", i, paste(row$id, collapse = " "))
    if (
      !is.character(row$id) ||
        length(row$id) != 1L ||
        !grepl("^CVE-[0-9]{4}-[0-9]+$", row$id)
    ) {
      out <- c(out, sprintf("%s: `id` is not a single CVE identifier", label))
    }
    if (
      !is.character(row$package) ||
        length(row$package) != 1L ||
        !nzchar(row$package)
    ) {
      out <- c(
        out,
        sprintf("%s: `package` is not a single package name", label)
      )
    }
    if (
      !is.character(row$version_seen) ||
        length(row$version_seen) != 1L ||
        !parses_as_version(row$version_seen)
    ) {
      out <- c(
        out,
        sprintf("%s: `version_seen` is not a parseable version", label)
      )
    }
    if (!inherits(row$review, "Date") || length(row$review) != 1L) {
      out <- c(out, sprintf("%s: `review` is not a single Date", label))
    }
    # A reason long enough to be an argument rather than a placeholder.
    if (
      !is.character(row$reason) ||
        length(row$reason) != 1L ||
        nchar(row$reason) <= 80
    ) {
      out <- c(
        out,
        sprintf("%s: `reason` is missing or too short to be an argument", label)
      )
    }
  }

  ids <- vapply(
    rows,
    function(row) {
      if (is.character(row$id) && length(row$id) == 1L) {
        row$id
      } else {
        NA_character_
      }
    },
    character(1)
  )
  duplicates <- unique(ids[duplicated(ids) & !is.na(ids)])
  if (length(duplicates) > 0) {
    out <- c(
      out,
      sprintf("duplicate allow-list id(s): %s", toString(duplicates))
    )
  }

  out
}

# Flatten an oysteR::audit_description() result to one row per reported
# advisory. The `vulnerabilities` column is a list of per-package lists, empty
# for a clean package, so the zero-advisory case has to survive unlist().
oss_index_reported <- function(audit) {
  ids <- lapply(audit$vulnerabilities, function(v) {
    if (length(v) == 0) {
      return(character())
    }
    vapply(v, function(x) as.character(x$id), character(1))
  })
  data.frame(
    package = rep(as.character(audit$package), lengths(ids)),
    version = rep(as.character(audit$version), lengths(ids)),
    id = as.character(unlist(ids, use.names = FALSE)),
    stringsAsFactors = FALSE
  )
}
