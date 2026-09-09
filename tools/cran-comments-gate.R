#!/usr/bin/env Rscript

# cran-comments.md consistency gate (RURL-ladruqhn).
#
# WHY THIS EXISTS. `cran-comments.md` is the note CRAN reviewers read, and until
# 2026-08-16 it was not even tracked (commit e029b11 removed it and .gitignore
# kept it out). It lived on one machine, so no diff, no review and no gate ever
# saw it. Measured 2026-08-15 it had drifted ELEVEN releases:
#
#   * it announced a "1.4.0 -> 2.2.0" release. CRAN held only 1.2.0 and
#     DESCRIPTION said 3.0.0 -- wrong at BOTH ends, since 1.4.0 was never
#     released either;
#   * it carried a live "ACTION BEFORE CRAN SUBMISSION -- remove the `Remotes:`
#     field" block. RURL-rdocgshe had already removed that field, so the
#     instruction was a no-op that read as a blocker;
#   * it quoted `pslr (>= 1.0.2)` while DESCRIPTION had moved to `>= 1.1.0`.
#
# Tracking the file gives it a diff. It does NOT give it a check -- a diff is
# only read by someone who happens to look. This is the check.
#
# WHAT IT CHECKS. Five properties, all of them local and network-free:
#   1. the file exists at all;
#   2. it carries exactly one machine-readable span pin,
#      `<!-- submission-span: from=X to=Y -->`;
#   3. `to` equals the release DESCRIPTION's `Version:` names -- the version
#      itself while a release is being prepared, and the release the cycle
#      descends from once `Version:` is a development version (see below);
#   4. both `from` and `to` appear literally in the visible prose, so the pin
#      and the sentence a reviewer reads cannot drift apart;
#   5. `Remotes:` and every dependency floor the note quotes agree with
#      DESCRIPTION.
#
# DEVELOPMENT VERSIONS ARE NOT SUBMISSIONS (RURL-efbcrhjc). Property 3 used to
# be a literal `to == Version`, which made CONTRIBUTING.md's release checklist
# step 8 -- "bump DESCRIPTION to the next development version" -- unexecutable:
# the bump moved `Version:` away from the pin and the gate failed the push, and
# there was no honest value to put in the pin either, because `3.0.1.9000` will
# never be submitted to anyone. The rule is now the invariant that actually
# holds in both phases: the note describes the release `Version:` names. While
# preparing 3.0.1 that release is 3.0.1; during the cycle that follows it, at
# `3.0.1.9000`, it is still 3.0.1. This is a real check, not an exemption -- a
# bump to `3.1.0.9000` with the note still pinned to 3.0.1 is flagged, because
# naming a new target is exactly when the note has to be rewritten.
#
# THE `from` VERSION IS NOT CHECKED OFFLINE, ON PURPOSE. Whether `from` is the
# version CRAN actually publishes is a fact about the rest of the world, and
# `--online` is the only way to ask. That half is deliberately NOT in the gate
# list, for the reason tools/dependency-resolvability-gate.R states: wiring a
# network dependency into the only non-optional pre-push gate makes every push
# fail on a train. Run `--online` before a release. The offline half still
# catches the defect that actually shipped, because "to" being wrong is what
# made the note describe the wrong release.
#
# WHICH END `--online` COMPARES ALSO DEPENDS ON THE PHASE. Before a submission
# it is `from` that must name what CRAN serves: `to` is the version not yet
# published, and the pin claims the release will move CRAN from one to the
# other. After acceptance that claim has been discharged and `to` is what CRAN
# serves, so during a development cycle `--online` compares `to` instead.
# Comparing `from` in both phases would make `--online` fail permanently from
# the moment a release is accepted until the next note is written, which is
# most of a package's life.
#
# Usage:
#   Rscript tools/cran-comments-gate.R              # offline, exit 1 on drift
#   Rscript tools/cran-comments-gate.R --online     # also check `from` vs CRAN
#   Rscript tools/cran-comments-gate.R --self-test  # positive/negative cases

PIN_RE <- "<!--\\s*submission-span:\\s*from=(\\S+)\\s+to=(\\S+)\\s*-->"

# One package-and-floor pair, e.g. `pslr (>= 1.1.0)`. Shared by the DESCRIPTION
# reader and the note reader so the two can never disagree about what a floor
# looks like -- a gate that parsed them differently could report drift that is
# only its own.
FLOOR_RE <- "([A-Za-z][A-Za-z0-9.]*)\\s*\\(\\s*>=\\s*([0-9.-]+)"

# --- versions ----------------------------------------------------------------

# A version's components, or integer(0) if it does not parse. Returning empty
# rather than erroring keeps a malformed DESCRIPTION on the ordinary literal
# comparison below, where it produces a readable message, instead of aborting
# the gate with a parse error about a field that is not what is being checked.
version_parts <- function(version) {
  if (length(version) != 1L || is.na(version)) return(integer(0))
  tryCatch(
    as.integer(unclass(numeric_version(version))[[1L]]),
    error = function(e) integer(0),
    warning = function(w) integer(0)
  )
}

# The R convention: a fourth component of 9000 or more marks a development
# version, i.e. one that exists only between releases and is never submitted.
is_dev_version <- function(version) {
  parts <- version_parts(version)
  length(parts) == 4L && parts[[4L]] >= 9000L
}

# The release a version names: X.Y.Z for the development version X.Y.Z.9000,
# and the version itself otherwise.
release_named_by <- function(version) {
  if (is_dev_version(version)) {
    paste(version_parts(version)[1:3], collapse = ".")
  } else {
    version
  }
}

# --- inputs ------------------------------------------------------------------

# DESCRIPTION reduced to the three facts this gate compares against. `floors` is
# a named character vector of package -> version floor, gathered from every
# dependency field a floor may legally appear in.
description_facts <- function(path) {
  if (!file.exists(path)) {
    stop("cannot find DESCRIPTION at: ", path, call. = FALSE)
  }
  dcf <- read.dcf(path)
  # unname() is load-bearing: read.dcf carries the column name through, and a
  # named "3.0.0" is not identical() to a bare "3.0.0" -- which would make every
  # version comparison below fail while printing two strings that look equal.
  field <- function(nm) {
    if (nm %in% colnames(dcf)) unname(dcf[1L, nm]) else NA_character_
  }
  dep_fields <- vapply(
    c("Depends", "Imports", "LinkingTo"),
    function(nm) {
      v <- field(nm)
      if (is.na(v)) "" else v
    },
    character(1)
  )
  dep_text <- toString(dep_fields)
  hits <- regmatches(
    dep_text,
    gregexpr("([A-Za-z][A-Za-z0-9.]*)\\s*\\(\\s*>=\\s*([0-9.-]+)\\s*\\)",
             dep_text)
  )[[1L]]
  floors <- character(0)
  for (h in hits) {
    m <- regmatches(h, regexec(FLOOR_RE, h))[[1L]]
    if (length(m) == 3L) floors[[m[[2L]]]] <- m[[3L]]
  }
  list(
    package = field("Package"),
    version = field("Version"),
    has_remotes = "Remotes" %in% colnames(dcf),
    floors = floors
  )
}

# The note, split into the machine-readable pin and the prose a reviewer reads.
# `prose` deliberately excludes HTML comments so property 4 compares the pin
# against text that is actually rendered, not against itself.
comments_facts <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path, warn = FALSE)
  body <- paste(lines, collapse = "\n")
  pins <- regmatches(body, gregexpr(PIN_RE, body))[[1L]]
  parsed <- lapply(pins, function(p) {
    m <- regmatches(p, regexec(PIN_RE, p))[[1L]]
    list(from = m[[2L]], to = m[[3L]])
  })
  prose <- gsub("<!--.*?-->", "", body)
  hits <- regmatches(
    prose,
    gregexpr("`?([A-Za-z][A-Za-z0-9.]*)\\s*\\(\\s*>=\\s*([0-9.-]+)\\s*\\)",
             prose)
  )[[1L]]
  quoted <- character(0)
  for (h in hits) {
    m <- regmatches(h, regexec(FLOOR_RE, h))[[1L]]
    if (length(m) == 3L) quoted[[m[[2L]]]] <- m[[3L]]
  }
  list(pins = parsed, prose = prose, quoted_floors = quoted)
}

# --- the checks --------------------------------------------------------------

# Property 2 + 3 + 4. Split out so the pin rules stay readable on their own.
check_pin <- function(cc, desc) {
  out <- character(0)
  if (length(cc$pins) != 1L) {
    return(sprintf(
      paste0("cran-comments.md must carry exactly one span pin ",
             "`<!-- submission-span: from=X to=Y -->`; found %d. ",
             "It is what ties the note to a release."),
      length(cc$pins)
    ))
  }
  pin <- cc$pins[[1L]]
  expected <- release_named_by(desc$version)
  if (!identical(pin$to, expected)) {
    out <- c(out, if (is_dev_version(desc$version)) {
      sprintf(
        paste0("DESCRIPTION is at development version %s, so cran-comments.md ",
               "must still describe the release that cycle descends from ",
               "(%s); the pin says %s. If %s is the next target, rewrite the ",
               "note for it rather than repointing the pin alone."),
        desc$version, expected, pin$to, expected
      )
    } else {
      sprintf(
        paste0("cran-comments.md describes release %s but DESCRIPTION says ",
               "%s. The note is about a different version than the one being ",
               "built."),
        pin$to, desc$version
      )
    })
  }
  for (v in unique(c(pin$from, pin$to))) {
    if (identical(v, "none")) next
    if (!grepl(v, cc$prose, fixed = TRUE)) {
      out <- c(out, sprintf(
        paste0("the span pin names %s but no visible sentence mentions it, ",
               "so the pin and the prose a reviewer reads have drifted."),
        v
      ))
    }
  }
  out
}

# Property 5. Both directions: a stale instruction to remove a field that is
# gone, and a field that is present with nothing in the note addressing it.
check_remotes <- function(cc, desc) {
  out <- character(0)
  removal <- grepl("(remove|delete|drop)[^.]{0,60}Remotes", cc$prose,
                   ignore.case = TRUE)
  if (removal && !desc$has_remotes) {
    out <- c(out, paste0(
      "cran-comments.md instructs removing a `Remotes:` field, but ",
      "DESCRIPTION has none. The instruction is a no-op that reads as a ",
      "blocker (this is the RURL-rdocgshe drift)."
    ))
  }
  if (desc$has_remotes && !grepl("Remotes", cc$prose, fixed = TRUE)) {
    out <- c(out, paste0(
      "DESCRIPTION carries a `Remotes:` field and cran-comments.md never ",
      "mentions it. CRAN rejects `Remotes:`, so the note must address it."
    ))
  }
  out
}

check_floors <- function(cc, desc) {
  out <- character(0)
  for (pkg in names(cc$quoted_floors)) {
    said <- cc$quoted_floors[[pkg]]
    if (!pkg %in% names(desc$floors)) {
      out <- c(out, sprintf(
        paste0("cran-comments.md quotes a floor for '%s' (>= %s) that ",
               "DESCRIPTION does not declare at all."),
        pkg, said
      ))
    } else if (!identical(said, desc$floors[[pkg]])) {
      out <- c(out, sprintf(
        "cran-comments.md says %s (>= %s); DESCRIPTION says (>= %s).",
        pkg, said, desc$floors[[pkg]]
      ))
    }
  }
  out
}

check_repo <- function(root) {
  desc <- description_facts(file.path(root, "DESCRIPTION"))
  cc <- comments_facts(file.path(root, "cran-comments.md"))
  if (is.null(cc)) {
    return(list(desc = desc, cc = NULL, violations = paste0(
      "cran-comments.md is missing. It is the note CRAN reviewers read, and ",
      "it is tracked precisely so it cannot go absent unnoticed."
    )))
  }
  violations <- c(check_pin(cc, desc), check_remotes(cc, desc),
                  check_floors(cc, desc))
  list(desc = desc, cc = cc, violations = violations)
}

# --- the online half (opt-in, never in the gate list) ------------------------

# `from` must name the version CRAN actually serves today. Kept apart from
# check_repo() so nothing in the offline path can reach the network.
check_from_against_cran <- function(desc, cc) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("--online needs the 'jsonlite' package", call. = FALSE)
  }
  if (is.null(cc) || length(cc$pins) != 1L) {
    stop("--online needs a well-formed span pin; fix the offline gate first",
         call. = FALSE)
  }
  pin <- cc$pins[[1L]]
  # Before a submission the pin claims CRAN will move `from` -> `to`, so `from`
  # is the end that must match what CRAN serves. Once the release is accepted
  # and DESCRIPTION has moved on to a development version, that claim is spent
  # and `to` is the end CRAN now serves.
  dev <- is_dev_version(desc$version)
  end <- if (dev) "to" else "from"
  claim <- if (dev) pin$to else pin$from
  url <- sprintf("https://crandb.r-pkg.org/%s/all", desc$package)
  got <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  published <- if (is.null(got)) character(0) else names(got$versions)
  cat(sprintf("  CRAN publishes: %s\n",
              if (length(published) == 0L) "(nothing)"
              else toString(published)))
  latest <- if (length(published) == 0L) "none" else got$latest
  if (!identical(claim, latest)) {
    return(sprintf(
      paste0("the span pin says %s=%s, but CRAN currently publishes %s. ",
             "Re-derive the span from crandb, not from NEWS.md and not from ",
             "the note's own previous claim."),
      end, claim, latest
    ))
  }
  character(0)
}

# --- self-test (positive + negative coverage, executable) --------------------

write_fixture <- function(dir, version, remotes, imports, comments) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  d <- c("Package: fixture", paste0("Version: ", version),
         paste0("Imports: ", imports))
  if (nzchar(remotes)) d <- c(d, paste0("Remotes: ", remotes))
  writeLines(d, file.path(dir, "DESCRIPTION"))
  writeLines(comments, file.path(dir, "cran-comments.md"))
  dir
}

self_test <- function() {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  base <- tempfile("cran-comments-gate-selftest-")
  ok_note <- c("<!-- submission-span: from=1.2.0 to=3.0.0 -->",
               "CRAN holds 1.2.0; this is 3.0.0.",
               "It imports pslr (>= 1.1.0).")

  scenario <- function(tag, version, remotes, imports, comments) {
    check_repo(write_fixture(file.path(base, tag), version, remotes,
                             imports, comments))$violations
  }

  v <- scenario("ok", "3.0.0", "", "pslr (>= 1.1.0)", ok_note)
  if (length(v) > 0L) {
    fail(sprintf("false positive on a sound note: %s",
                 paste(v, collapse = "; ")))
  }

  # A first submission has no predecessor; from=none must not demand prose.
  v <- scenario("first", "1.0.0", "", "pslr (>= 1.1.0)",
                c("<!-- submission-span: from=none to=1.0.0 -->",
                  "First submission of 1.0.0.", "It imports pslr (>= 1.1.0)."))
  if (length(v) > 0L) {
    fail(sprintf("false positive on a first submission: %s",
                 paste(v, collapse = "; ")))
  }

  # A development version is not a submission: the note still describes the
  # release the cycle descends from, and must not be forced to name X.Y.Z.9000.
  v <- scenario("dev-cycle", "3.0.0.9000", "", "pslr (>= 1.1.0)", ok_note)
  if (length(v) > 0L) {
    fail(sprintf("false positive during a development cycle: %s",
                 paste(v, collapse = "; ")))
  }

  # NEGATIVE: the dev-version rule is a check, not a hole. Naming a new target
  # without rewriting the note is exactly the drift this gate exists to catch.
  v <- scenario("dev-cycle-retarget", "3.1.0.9000", "", "pslr (>= 1.1.0)",
                ok_note)
  if (!any(grepl("descends from", v, fixed = TRUE))) {
    fail("did not flag a development version that outran its cran-comments.md")
  }

  # NEGATIVE: the measured defect -- the note describes an older release.
  v <- scenario("stale-to", "3.0.0", "", "pslr (>= 1.1.0)",
                c("<!-- submission-span: from=1.4.0 to=2.2.0 -->",
                  "This is a feature release (1.4.0 -> 2.2.0).",
                  "It imports pslr (>= 1.1.0)."))
  if (!any(grepl("but DESCRIPTION says", v, fixed = TRUE))) {
    fail("did not flag a note describing a different version")
  }

  # NEGATIVE: the pin was updated and the sentence beneath it was not.
  v <- scenario("drifted-prose", "3.0.0", "", "pslr (>= 1.1.0)",
                c("<!-- submission-span: from=1.2.0 to=3.0.0 -->",
                  "This is a feature release (1.4.0 -> 2.2.0).",
                  "It imports pslr (>= 1.1.0)."))
  if (!any(grepl("no visible sentence mentions it", v, fixed = TRUE))) {
    fail("did not flag a pin that drifted from the prose")
  }

  # NEGATIVE: a pin hidden in a comment cannot satisfy the prose rule itself.
  v <- scenario("no-pin", "3.0.0", "", "pslr (>= 1.1.0)",
                c("A note with no pin at all.", "It imports pslr (>= 1.1.0)."))
  if (!any(grepl("exactly one span pin", v, fixed = TRUE))) {
    fail("did not flag a missing span pin")
  }

  # NEGATIVE: the RURL-rdocgshe drift.
  v <- scenario("stale-remotes", "3.0.0", "", "pslr (>= 1.1.0)",
                c(ok_note,
                  "ACTION: remove the `Remotes:` field before submitting."))
  if (!any(grepl("instructs removing a `Remotes:` field", v, fixed = TRUE))) {
    fail("did not flag an instruction to remove a field that is gone")
  }

  # NEGATIVE: a live Remotes: the note never mentions.
  v <- scenario("silent-remotes", "3.0.0", "gitlab::x/y", "pslr (>= 1.1.0)",
                ok_note)
  if (!any(grepl("never mentions it", v, fixed = TRUE))) {
    fail("did not flag an unaddressed Remotes: field")
  }

  # NEGATIVE: a floor that moved in DESCRIPTION and not in the note.
  v <- scenario("stale-floor", "3.0.0", "", "pslr (>= 1.1.0)",
                c("<!-- submission-span: from=1.2.0 to=3.0.0 -->",
                  "CRAN holds 1.2.0; this is 3.0.0.",
                  "It imports pslr (>= 1.0.2)."))
  if (!any(grepl("DESCRIPTION says", v, fixed = TRUE))) {
    fail("did not flag a quoted floor that no longer matches DESCRIPTION")
  }

  # NEGATIVE: the file absent entirely.
  d <- file.path(base, "absent")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Package: fixture", "Version: 3.0.0"),
             file.path(d, "DESCRIPTION"))
  if (!any(grepl("is missing", check_repo(d)$violations, fixed = TRUE))) {
    fail("did not flag a missing cran-comments.md")
  }

  unlink(base, recursive = TRUE)
  cat("cran-comments-gate self-test: PASS (3 positive + 8 negative cases)\n")
  invisible(TRUE)
}

# --- main --------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }

  res <- check_repo(root)
  cat(sprintf("cran-comments.md consistency gate\n  DESCRIPTION version: %s\n",
              res$desc$version))
  if (!is.null(res$cc) && length(res$cc$pins) == 1L) {
    cat(sprintf("  span pin: from=%s to=%s\n",
                res$cc$pins[[1L]]$from, res$cc$pins[[1L]]$to))
  }
  violations <- res$violations
  if ("--online" %in% args && length(violations) == 0L) {
    cat("  [--online] asking CRAN what it publishes\n")
    violations <- c(violations, check_from_against_cran(res$desc, res$cc))
  }
  if (length(violations) > 0L) {
    cat("DRIFT:\n")
    for (v in violations) cat("  - ", v, "\n", sep = "")
    stop(sprintf("cran-comments.md is out of sync (%d finding(s))",
                 length(violations)), call. = FALSE)
  }
  cat("PASS: the note names this release, and its claims match DESCRIPTION.\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  if (sys.nframe() == 0L) {
    main()
  }
}
