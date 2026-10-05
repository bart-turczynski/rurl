#!/usr/bin/env Rscript
#
# Generated-docs drift gate: fails if man/, NAMESPACE or DESCRIPTION differ
# from what roxygen2 would regenerate from the roxygen comments in R/.
#
# ONE COPY. This file is seor's scripts/check-docs-drift.R, and every fleet
# package vendors it byte for byte as scripts/check-docs-drift.R; seor's
# scripts/check-fleet-standard.py reports a copy that differs (line endings
# aside) as a local-gate gap (SEOR-lyciowif). Change it here, then copy it
# out. Nothing in it is specific to one repository: its one argument is the
# package directory, and the caller decides where it runs.
#
# Why this exists: a stale .Rd is still perfectly valid .Rd, so nothing else in
# a verify gate can see it. lintr::lint_package() reads R/ and never looks at
# man/; spelling::spell_check_package() reads man/ as it is; R CMD check
# --as-cran validates the Rd it is given, not whether that Rd still matches its
# source comment. The fleet's logo sweep added man/figures/logo.svg without
# re-running devtools::document(), so man/<pkg>-package.Rd went stale in rurl
# and pslr with every gate green, and was fixed by hand (rurl !176, pslr !89;
# SEOR-oopopupm). robotstxtr had been bitten the same way by a roxygen comment
# reflowed for the linter (ROBO-cbzemsnq). Regenerating and diffing is the only
# thing that catches it (SEOR-nwfmerhu).
#
# DESCRIPTION is watched too. roxygenise() owns fields there:
# Config/roxygen2/version (pinned below, so it cannot move here), RoxygenNote
# on older roxygen2, and Collate, which it rewrites from @include tags. A
# stale Collate is drift like a stale .Rd.
#
# Roxygen runs through its default loader, the same one devtools::document()
# uses, so what this gate demands is exactly what the documented fix produces.
# A cheaper loader (load_code = "source") evaluates package code differently,
# so a dynamic doc could make the gate reject a tree that document() considers
# clean -- an unfixable failure, the worst kind for a gate to have. For a
# package with src/ that load compiles it, and pkgload leaves .o files and a
# .so (and may rewrite generated glue) behind in src/.
#
# WHERE IT RUNS: ON A THROWAWAY COPY. roxygenise() writes into the directory
# it is given, so on drift it rewrites man/, NAMESPACE and DESCRIPTION there,
# and a compile leaves build residue. A pre-push gate therefore runs it on a
# `git archive` export of the commit being pushed (PRE_COMMIT_TO_REF, else
# HEAD), never the working tree: that judges the commit rather than the disk,
# so a fix left uncommitted does not rescue a stale committed man/, and an
# untracked man/*.Rd does not count as present. A CI job may run it in place,
# in a checkout of the pushed commit that nothing else uses. Its callers
# belong to each package: a pre-push wrapper that makes the export, and a CI
# job where one installs the pinned roxygen2.
#
# git is used for one thing: printing the line diff on drift (git diff
# --no-index, which works outside a repository). The verdict itself -- which
# generated files are changed, missing or stale -- is computed here, byte for
# byte, so without git on PATH (rocker/r-ver images ship none) the check is
# just as complete: the report lists the files and says the diff is omitted.
#
# Usage (from the package root):
#   Rscript scripts/check-docs-drift.R [package-dir]
#   Rscript scripts/check-docs-drift.R --self-test
#
# Arguments:
#   package-dir   The package to check, regenerated in place. Default ".".
#   --self-test   Run this script against throwaway fixture packages (docs
#                 missing, changed, stale and in sync; with and without git)
#                 and check the roxygen2 guards. Needs roxygen2, any version.
#
# Environment: none of its own. It reads PATH (for git) and the library paths
# roxygen2 is loaded from, like any R script.
#
# Exit status: 0 when the docs are in sync; 1 on drift, after printing which
# files differ and the diff, and LEAVING the regenerated files in place (run by
# hand on a working tree, the fix is then already applied and only needs
# committing). An R error (status 1 too, with "Error:") when the check cannot
# run: no DESCRIPTION, no Config/roxygen2/version, roxygen2 missing, or a
# roxygen2 other than the pinned one.

# The roxygen2 version DESCRIPTION pins, from Config/roxygen2/version.
#
# Exact equality is the gate (check_roxygen2()). roxygen2 changes its output
# formatting between releases, so an unpinned runner reports version skew as
# drift -- and when the installed version is the newer one, roxygen2 quietly
# rewrites Config/roxygen2/version in DESCRIPTION, which is itself an unwanted
# diff. Refusing to guess keeps every failure this gate reports a real one.
# A package's pre-push chain may check the same pin earlier (the fleet's
# scripts/check-toolchain.R does); repeating it here keeps the script safe to
# run on its own, as CI does.
roxygen_pin <- function(pkg) {
  desc_path <- file.path(pkg, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop(
      sprintf(
        "No DESCRIPTION at %s (run from the package root?)",
        normalizePath(pkg, mustWork = FALSE)
      ),
      call. = FALSE
    )
  }
  field <- "Config/roxygen2/version"
  desc <- read.dcf(desc_path)
  if (!field %in% colnames(desc)) {
    stop(
      sprintf(
        paste0(
          "DESCRIPTION has no %s field. This gate needs that pin to tell ",
          "real doc drift apart from roxygen2 version skew; it is written ",
          "automatically by running devtools::document() with the intended ",
          "roxygen2 version."
        ),
        field
      ),
      call. = FALSE
    )
  }
  # unname(): indexing a read.dcf() matrix carries the column name along, and
  # a named string is never identical() to the plain one from packageVersion().
  unname(trimws(desc[1L, field]))
}

# The installed roxygen2's version, or NA when it is not installed.
installed_roxygen2 <- function() {
  if (!requireNamespace("roxygen2", quietly = TRUE)) {
    return(NA_character_)
  }
  as.character(utils::packageVersion("roxygen2"))
}

# Refuse to run unless `installed` is exactly `pinned`. A missing roxygen2 is
# a broken environment, not a green tree, so it gets its own named fix.
check_roxygen2 <- function(pinned, installed) {
  if (is.na(installed)) {
    stop(
      sprintf(
        paste0(
          "roxygen2 is not installed, so the docs-drift gate cannot run. A ",
          "missing checker is a broken environment, not a green tree. Install ",
          "the pinned version: pak::pkg_install(\"roxygen2@%s\")."
        ),
        pinned
      ),
      call. = FALSE
    )
  }
  if (!identical(pinned, installed)) {
    stop(
      sprintf(
        paste0(
          "roxygen2 version skew: DESCRIPTION pins Config/roxygen2/version = ",
          "%s, but roxygen2 %s is installed. Either install the pinned ",
          "version (pak::pkg_install(\"roxygen2@%s\")), or adopt the newer ",
          "roxygen2 deliberately by running devtools::document() and ",
          "committing the resulting man/, NAMESPACE and DESCRIPTION changes ",
          "together."
        ),
        pinned,
        installed,
        pinned
      ),
      call. = FALSE
    )
  }
  invisible(installed)
}

# The generated surface roxygen2 owns, relative to the package root.
# DESCRIPTION exists (roxygen_pin() checked) and is listed whole: see the
# header.
watched_files <- function(root) {
  rd <- list.files(
    file.path(root, "man"),
    pattern = "[.]Rd$",
    recursive = TRUE
  )
  c(
    "DESCRIPTION",
    if (file.exists(file.path(root, "NAMESPACE"))) "NAMESPACE",
    if (length(rd) > 0L) file.path("man", rd)
  )
}

# Copy a set of package-relative paths into a flat mirror directory, so the two
# states can be compared and diffed as trees.
mirror <- function(root, files, dest) {
  for (f in files) {
    target <- file.path(dest, f)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(root, f), target)) {
      stop(sprintf("could not copy %s into %s", f, dest), call. = FALSE)
    }
  }
  dest
}

read_bytes <- function(path) readBin(path, "raw", file.size(path))

# What roxygen changed: the watched files it would create (added), the ones it
# would delete (removed), and the ones whose bytes differ (changed). Computed
# here, not by git, so the verdict does not depend on git being installed.
compare_docs <- function(committed_dir, committed_files, root) {
  regenerated_files <- watched_files(root)
  common <- intersect(committed_files, regenerated_files)
  list(
    regenerated = regenerated_files,
    added = setdiff(regenerated_files, committed_files),
    removed = setdiff(committed_files, regenerated_files),
    changed = common[vapply(
      common,
      function(f) {
        !identical(
          read_bytes(file.path(committed_dir, f)),
          read_bytes(file.path(root, f))
        )
      },
      logical(1L)
    )]
  )
}

# The line diff between the two mirrors, from git diff --no-index. Exit status
# 1 just means "differences found", so the non-zero status warning is
# expected. Without git the file list is the whole report, and this says so
# rather than printing nothing, or a "git: not found" line posing as a diff.
docs_diff <- function(committed_dir, regenerated_dir) {
  git <- Sys.which("git")
  if (!nzchar(git[[1L]])) {
    return(paste0(
      "(git not found on PATH, so the line diff is omitted; the files listed ",
      "above are the whole verdict.)"
    ))
  }
  diff_out <- tryCatch(
    suppressWarnings(system2(
      git[[1L]],
      c(
        "diff",
        "--no-index",
        "--src-prefix=committed/",
        "--dst-prefix=regenerated/",
        "--",
        shQuote(committed_dir),
        shQuote(regenerated_dir)
      ),
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(e) {
      sprintf("(git diff could not run: %s)", conditionMessage(e))
    }
  )
  # git renders a --no-index path as <prefix><absolute path minus its leading
  # slash>, so the temp directory sits between the side label and the
  # package-relative path. Strip it to leave "committed/man/foo.Rd".
  strip_dir <- function(x, dir) {
    gsub(paste0(sub("^/", "", dir), "/"), "", x, fixed = TRUE)
  }
  strip_dir(strip_dir(diff_out, committed_dir), regenerated_dir)
}

report_paths <- function(label, paths) {
  if (length(paths) > 0L) {
    message(sprintf("  %s (%d):", label, length(paths)))
    message(paste0("    ", paths, collapse = "\n"))
  }
}

# The check itself: 0 when in sync, 1 on drift (reported).
check_docs_drift <- function(pkg) {
  pinned <- roxygen_pin(pkg)
  installed <- check_roxygen2(pinned, installed_roxygen2())

  committed_files <- watched_files(pkg)
  committed_dir <- mirror(pkg, committed_files, tempfile("docs-committed-"))

  message(sprintf(
    "Regenerating man/, NAMESPACE and DESCRIPTION in %s with roxygen2 %s ...",
    normalizePath(pkg),
    installed
  ))
  roxygen2::roxygenise(pkg)

  drift <- compare_docs(committed_dir, committed_files, pkg)
  if (
    length(drift$added) == 0L &&
      length(drift$removed) == 0L &&
      length(drift$changed) == 0L
  ) {
    message(
      "Docs in sync: man/, NAMESPACE and DESCRIPTION match the roxygen ",
      "comments in R/."
    )
    return(0L)
  }

  regenerated_dir <- mirror(
    pkg,
    drift$regenerated,
    tempfile("docs-regenerated-")
  )
  message("")
  message("Generated documentation is out of date.")
  report_paths("Changed", drift$changed)
  report_paths("Missing (roxygen would create)", drift$added)
  report_paths("Stale (roxygen would delete)", drift$removed)
  diff_out <- docs_diff(committed_dir, regenerated_dir)
  if (length(diff_out) > 0L) {
    message("")
    message(paste(diff_out, collapse = "\n"))
  }
  message("")
  message(
    "Fix: run devtools::document() in your checkout, with roxygen2 ",
    installed,
    ", and commit the resulting man/, NAMESPACE and DESCRIPTION changes. A ",
    "gate that checks a commit does not see a fix left uncommitted."
  )
  1L
}

# --- self-test ---------------------------------------------------------------
#
# Runs this script, as its own process, against throwaway fixture packages:
# docs missing, in sync, changed and stale, with git on PATH and without it.
# The roxygen2 guards are called directly. It needs roxygen2 (any version: the
# fixture pins the installed one) and nothing else.

# The path of this script, as Rscript was given it.
self_test_script <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", file_arg[[1L]]))
}

# A directory of links to every program on PATH except git, so a run with it
# as its whole PATH sees a machine with no git, as on a CI image that ships
# none (rocker/r-ver).
path_without_git <- function() {
  bin <- tempfile("docs-drift-no-git-")
  dir.create(bin)
  dirs <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1L]]
  dirs <- unique(dirs[nzchar(dirs) & dir.exists(dirs)])
  progs <- unlist(lapply(dirs, list.files, full.names = TRUE))
  progs <- progs[!dir.exists(progs)]
  progs <- progs[!duplicated(basename(progs)) & basename(progs) != "git"]
  file.symlink(progs, file.path(bin, basename(progs)))
  bin
}

fixture_package <- function(roxygen_version) {
  pkg <- tempfile("docs-drift-fixture-")
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  writeLines(
    c(
      "Package: docsdriftfixture",
      "Title: Docs-Drift Self-Test Fixture",
      "Version: 0.0.1",
      "Description: A fixture.",
      "License: MIT",
      "Encoding: UTF-8",
      paste0("Config/roxygen2/version: ", roxygen_version)
    ),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c(
      "#' Add one",
      "#'",
      "#' @param x A number.",
      "#' @return x plus one.",
      "#' @export",
      "f <- function(x) x + 1"
    ),
    file.path(pkg, "R", "f.R")
  )
  pkg
}

copy_package <- function(pkg) {
  dest <- tempfile("docs-drift-case-")
  dir.create(dest)
  file.copy(list.files(pkg, full.names = TRUE), dest, recursive = TRUE)
  dest
}

edit_lines <- function(path, from, to) {
  writeLines(sub(from, to, readLines(path), fixed = TRUE), path)
}

# Run this script on `pkg` in its own process, with `path` as its PATH.
run_check <- function(script, pkg, path = Sys.getenv("PATH")) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), shQuote(pkg)),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("PATH=", shQuote(path))
  ))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, out = out)
}

error_text <- function(expr) {
  tryCatch(
    {
      expr
      ""
    },
    error = conditionMessage
  )
}

self_test <- function() {
  if (!requireNamespace("roxygen2", quietly = TRUE)) {
    stop("The self-test runs roxygen2, which is not installed.", call. = FALSE)
  }
  script <- self_test_script()
  no_git <- path_without_git()
  check <- function(tag, ok, res = NULL) {
    message(if (ok) "ok    " else "FAIL  ", tag)
    if (!ok && !is.null(res)) {
      message(paste0("      | ", res$out, collapse = "\n"))
    }
    stats::setNames(ok, tag)
  }
  has <- function(res, lines) all(lines %in% res$out)
  # A section of the report: its header, then exactly these files, in order.
  lists <- function(res, lines) {
    at <- match(lines[[1L]], res$out)
    after <- res$out[at + length(lines)]
    !is.na(at) &&
      identical(res$out[seq(at, length.out = length(lines))], lines) &&
      (is.na(after) || !startsWith(after, "    "))
  }
  with_git <- nzchar(Sys.which("git")[[1L]])
  no_diff <- function(res) !any(startsWith(res$out, "diff --git "))
  says_omitted <- function(res) {
    any(grepl("diff is omitted", res$out, fixed = TRUE))
  }

  base <- fixture_package(as.character(utils::packageVersion("roxygen2")))

  # Added: nothing roxygen generates is there yet. The run regenerates in
  # place, which leaves `base` in sync for the cases below.
  added <- run_check(script, base, no_git)
  in_sync <- run_check(script, base)

  changed_pkg <- copy_package(base)
  edit_lines(file.path(changed_pkg, "R", "f.R"), "Add one", "Add two")
  changed_git_pkg <- copy_package(changed_pkg)
  changed <- run_check(script, changed_pkg, no_git)
  changed_git <- run_check(script, changed_git_pkg)

  # Deleted: an Rd roxygen generated once, for a topic R/ no longer has.
  stale_pkg <- copy_package(base)
  rd <- readLines(file.path(stale_pkg, "man", "f.Rd"))
  writeLines(
    sub("{f}", "{g}", rd, fixed = TRUE),
    file.path(stale_pkg, "man", "g.Rd")
  )
  stale <- run_check(script, stale_pkg, no_git)

  missing_error <- error_text(check_roxygen2("8.0.0", NA_character_))
  skew_error <- error_text(check_roxygen2("8.0.0", "7.3.2"))

  verdicts <- c(
    check(
      "the no-git PATH has no git",
      !file.exists(file.path(no_git, "git"))
    ),
    check(
      "added, no git: exits 1, NAMESPACE and man/f.Rd listed as missing",
      added$status == 1L &&
        lists(
          added,
          c(
            "  Missing (roxygen would create) (2):",
            "    NAMESPACE",
            "    man/f.Rd"
          )
        ) &&
        no_diff(added),
      added
    ),
    check(
      "in sync: exits 0 and says so",
      in_sync$status == 0L && any(startsWith(in_sync$out, "Docs in sync")),
      in_sync
    ),
    check(
      "changed, no git: exits 1, man/f.Rd listed as changed",
      changed$status == 1L &&
        lists(changed, c("  Changed (1):", "    man/f.Rd")) &&
        no_diff(changed),
      changed
    ),
    check(
      "deleted, no git: exits 1, man/g.Rd listed as stale",
      stale$status == 1L &&
        lists(
          stale,
          c("  Stale (roxygen would delete) (1):", "    man/g.Rd")
        ) &&
        no_diff(stale),
      stale
    ),
    # Only where git is on PATH; a machine without it skips this one case.
    if (!with_git) {
      message("skip  changed, with git: no git on PATH")
    },
    if (with_git) {
      check(
        "changed, with git: exits 1, prints the diff, paths package-relative",
        changed_git$status == 1L &&
          has(
            changed_git,
            c(
              "--- committed/man/f.Rd",
              "+++ regenerated/man/f.Rd",
              "-\\title{Add one}",
              "+\\title{Add two}"
            )
          ) &&
          !says_omitted(changed_git),
        changed_git
      )
    },
    check(
      "no git: each report says the diff was omitted",
      says_omitted(added) && says_omitted(changed) && says_omitted(stale),
      changed
    ),
    check(
      "roxygen2 missing: refused, naming the pinned install",
      grepl("roxygen2 is not installed", missing_error, fixed = TRUE) &&
        grepl(
          "pak::pkg_install(\"roxygen2@8.0.0\")",
          missing_error,
          fixed = TRUE
        )
    ),
    check(
      "roxygen2 skew: refused, naming both versions",
      grepl("pins Config/roxygen2/version = 8.0.0", skew_error, fixed = TRUE) &&
        grepl("roxygen2 7.3.2 is installed", skew_error, fixed = TRUE)
    ),
    check(
      "roxygen2 at the pin: accepted",
      !nzchar(error_text(check_roxygen2("8.0.0", "8.0.0")))
    )
  )
  if (all(verdicts)) {
    message(sprintf(
      "check-docs-drift self-test: %d checks ok",
      length(verdicts)
    ))
    return(0L)
  }
  message(sprintf(
    "check-docs-drift self-test: %d of %d checks FAILED",
    sum(!verdicts),
    length(verdicts)
  ))
  1L
}

args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "--self-test")) {
  quit(status = self_test())
}
quit(status = check_docs_drift(if (length(args) > 0L) args[[1L]] else "."))
