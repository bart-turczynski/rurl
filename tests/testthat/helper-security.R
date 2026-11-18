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
# Three rules keep the list honest. test-security.R enforces them. Each matches
# a row to a reported advisory on the pair (`package`, `id`), never on the id
# alone, so a row covers its CVE in its own package and nowhere else:
#
#   A  UNEXPECTED  an advisory reported and not allow-listed FAILS. This is
#                  the gate still doing its job.
#   B  STALE       an allow-listed advisory NO LONGER reported FAILS. An
#                  allow-list may not over-permit; a row that has outlived its
#                  justification is deleted, not left to widen the exemption
#                  silently.
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

# A well-formed allow-list row for the offline tests, which must not depend on
# what the live list happens to hold.
oss_index_fixture_row <- function(id, package = "curl") {
  list(
    id = id,
    package = package,
    version_seen = "8.1.0",
    review = as.Date("2026-12-01"),
    reason = paste(
      "A fixture rationale long enough to be an argument rather than a",
      "placeholder, so that the validator accepts the row."
    )
  )
}

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
    if (!oss_index_is_package_name(row$package)) {
      out <- c(
        out,
        sprintf("%s: `package` is not a single package name", label)
      )
    }
    # package_version(NA) parses, so NA is refused before it is tried.
    if (
      !is.character(row$version_seen) ||
        length(row$version_seen) != 1L ||
        is.na(row$version_seen) ||
        !parses_as_version(row$version_seen)
    ) {
      out <- c(
        out,
        sprintf("%s: `version_seen` is not a parseable version", label)
      )
    }
    if (
      !inherits(row$review, "Date") ||
        length(row$review) != 1L ||
        anyNA(row$review)
    ) {
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

  duplicates <- oss_index_allowlist_duplicates(rows)
  if (length(duplicates) > 0) {
    out <- c(
      out,
      sprintf("duplicate allow-list row(s): %s", toString(duplicates))
    )
  }

  out
}

# An advisory is keyed by the pair (package, id), never by id alone: a row
# argues one package's exposure, so the same CVE reported against a second
# package (another libcurl binding, say) needs a row of its own
# (SEOR-hzjslawq). A space appears in neither a package name nor a CVE id.
oss_index_key <- function(package, id) {
  paste(package, id)
}

# A single R package name: no NA, and no space for the key to trip on.
oss_index_is_package_name <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    grepl("^[A-Za-z][A-Za-z0-9.]*$", x)
}

# An allow-list row's key, or NA for a row whose `package` or `id` is
# malformed; the validator reports such a row.
oss_index_row_key <- function(row) {
  id_ok <- is.character(row$id) && length(row$id) == 1L && !is.na(row$id)
  if (id_ok && oss_index_is_package_name(row$package)) {
    oss_index_key(row$package, row$id)
  } else {
    NA_character_
  }
}

# The keys that more than one allow-list row carries.
oss_index_allowlist_duplicates <- function(rows) {
  keys <- vapply(rows, oss_index_row_key, character(1))
  unique(keys[duplicated(keys) & !is.na(keys)])
}

# Rules A and B of the header, as findings rather than expectations, so that
# they can be exercised against fixtures. `found` is shaped like
# oss_index_reported(), `rows` like `oss_index_allowlist`. Returns the keys
# reported and not allow-listed (`unexpected`, rule A) and the keys
# allow-listed and not reported (`stale`, rule B), each sorted. A reported
# advisory with a missing package or id matches no row, even one whose
# package or id is the string "NA"; a malformed row is stale as NA.
oss_index_rule_findings <- function(found, rows) {
  allowed <- vapply(rows, oss_index_row_key, character(1))
  reported <- oss_index_key(found$package, found$id)
  known <- !is.na(found$package) & !is.na(found$id)
  # Radix sorting is in C-locale order, so findings read the same everywhere.
  list(
    unexpected = sort(
      unique(reported[!(known & reported %in% allowed)]),
      method = "radix"
    ),
    stale = sort(
      setdiff(allowed, reported[known]),
      na.last = TRUE,
      method = "radix"
    )
  )
}

# Rule C of the header: a row past its review date, or reported at a version
# later than `version_seen`, warns. Never fails. A reported version that is
# missing is no evidence of drift, so it neither warns nor errors.
oss_index_warn_drift <- function(found, rows, today = Sys.Date()) {
  for (row in rows) {
    if (today > row$review) {
      warning(
        sprintf(
          "OSS Index allow-list row %s is past its %s review date.",
          row$id,
          format(row$review)
        ),
        call. = FALSE
      )
    }
    hit <- found[which(found$package == row$package & found$id == row$id), ]
    if (
      nrow(hit) > 0 &&
        !is.na(hit$version[1]) &&
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
  invisible(NULL)
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
