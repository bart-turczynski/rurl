#!/usr/bin/env Rscript

# Local verify gate (RURL-mvsxmyww).
#
# WHY THIS EXISTS. Every gate this repository owns ran in exactly one place --
# GitHub Actions -- so when pushing stopped, verification stopped with it. The
# cost was not hypothetical: an integration branch accumulated 61 commits during
# the outage, and by the time anyone looked, the package DID NOT BUILD (two
# files missing from `Collate:`), a test ERRORED under `R CMD check`, and FOUR
# gates were red, one of them for days. `devtools::test()` was green throughout.
# That is the whole point: a green test suite is not a green package, and the
# only instrument that knows the difference is a real build plus check.
#
# THE GATE LIST IS DERIVED, NOT TRANSCRIBED. The gate steps below are read out
# of .github/workflows/verify.yml at run time. A hand-maintained copy would be
# one more list to forget: add a gate to CI, and a local mirror that repeats it
# by hand is silently incomplete from that moment. Reading the workflow means a
# new CI gate is picked up here the day it lands, and a gate REMOVED from CI
# stops running here too. It also means this script cannot claim to mirror CI
# while quietly running something else.
#
# GATE SELF-TESTS ARE A SEPARATE RISK CLASS. Their positive/negative fixtures
# prove the verifier, not the product, so routine runs select them only when the
# corresponding script changed relative to main. `--release` deliberately runs
# every self-test. The real tree scans still run in every complete local gate.
#
# WHAT IT DOES NOT COVER, stated so nobody reads a green run as more than it is:
#   * cross-platform and multi-R-version checks (full-check.yml, rhub.yaml) --
#     this runs one platform, one R;
#   * README.md re-render (verify.yml `readme`), pkgdown, coverage, the OSV and
#     security audits, news-version, and the determinism matrix -- all need
#     network, a pandoc/LaTeX toolchain, or a Docker matrix;
#   * the C7 curl clean room, which needs its own R CMD check against a poisoned
#     library. `--release` adds it; the default does not, because it doubles the
#     slowest stage to re-prove a criterion that only matters at release.
#
# It is not a strict subset of CI in either direction, so neither "green here
# means green there" nor its converse holds. The check stage runs `--as-cran`,
# which is STRICTER than verify.yml's fast gate (that one passes `--no-manual`
# alone and leaves `--as-cran` to full-check.yml), so this can fail where CI's
# fast gate would pass. Everything in the list above runs only in CI. A pass
# here means "the fast gate's checks hold on this machine" -- not "the release
# is ready", and not "CI will be green".
#
# STAGE ORDER is cheapest-first, so a broken tree fails in seconds rather than
# after a five-minute check. Stages are independent: a failure does not stop the
# run, because knowing all of what is broken beats knowing the first thing.
#
# Usage:
#   Rscript tools/verify.R            # gates + relevant self-tests + full gate
#   Rscript tools/verify.R --fast     # gates + relevant self-tests + lint
#   Rscript tools/verify.R --release  # everything, plus the curl clean room
#   Rscript tools/verify.R --list     # print the stage plan and exit
# `--fast` is iteration feedback only. It is never sufficient verification for
# a behavioral slice; the unsuffixed command remains the end-of-slice gate.
#
# Base R only. Exits 1 if any BLOCKING stage fails; advisory stages report and
# never fail the run.

WORKFLOW <- ".github/workflows/verify.yml"

# The control-plane gate is ADVISORY here, and that is a deliberate reading of
# its semantics rather than leniency. It goes red when a contract body is edited
# under an ACCEPTED acceptance gate -- the reopening rule working as designed --
# and clearing it needs an owner seal-merge, not a code change. Wiring a
# legitimately-red gate as blocking would make every unrelated commit fail,
# which trains people to pass `--no-verify`; the curl-zero gate's own header
# makes the same argument for the same reason. It is still RUN, and its verdict
# still printed, because an advisory gate nobody looks at is not a gate.
ADVISORY <- "design/work/url-v3/tools/ci-gate.R"

args <- commandArgs(trailingOnly = TRUE)
opt_fast <- "--fast" %in% args
opt_release <- "--release" %in% args
opt_list <- "--list" %in% args

# ---- helpers ----------------------------------------------------------------

repo_root <- function() {
  if (!file.exists("DESCRIPTION") || !dir.exists(".git")) {
    stop("run this from the repository root", call. = FALSE)
  }
  normalizePath(".")
}

# Gate invocations as CI actually spells them. `run: Rscript <script> [args]`
# is the shape every gate step uses; the workflow's other steps are actions or
# multi-line shell, and neither is a gate.
workflow_gates <- function(path) {
  if (!file.exists(path)) {
    stop("cannot read ", path, " -- the gate list is derived from it",
         call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  hits <- grep("^\\s*run: Rscript\\s+\\S", lines, value = TRUE)
  cmds <- sub("^\\s*run: Rscript\\s+", "", hits)
  unique(trimws(cmds[!grepl(" --self-test", cmds, fixed = TRUE)]))
}

workflow_self_tests <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hits <- grep("^\\s*run: Rscript\\s+\\S+ --self-test\\s*$", lines,
               value = TRUE)
  unique(trimws(sub("^\\s*run: Rscript\\s+", "", hits)))
}

changed_files <- function() {
  committed <- suppressWarnings(system2(
    "git", c("diff", "--name-only", "main...HEAD"),
    stdout = TRUE, stderr = FALSE
  ))
  uncommitted <- suppressWarnings(system2(
    "git", c("diff", "--name-only", "HEAD"),
    stdout = TRUE, stderr = FALSE
  ))
  unique(c(committed, uncommitted))
}

# One command, output captured. Only a failure prints its log: a passing gate
# that dumps 40 lines is how a real failure gets scrolled past.
run_step <- function(label, command, args = character(0), env = character(0)) {
  log <- tempfile(fileext = ".log")
  t0 <- Sys.time()
  status <- suppressWarnings(system2(command, args, stdout = log,
                                     stderr = log, env = env))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok <- identical(as.integer(status), 0L)
  cat(sprintf("  %-4s %-58s %5.1fs\n", if (ok) "PASS" else "FAIL",
              substr(label, 1L, 58L), secs))
  if (!ok) {
    txt <- tryCatch(readLines(log, warn = FALSE), error = function(e) character())
    tail_n <- utils::tail(txt, 25L)
    cat(paste0("       | ", tail_n, collapse = "\n"), "\n", sep = "")
  }
  list(label = label, ok = ok, secs = secs, log = log)
}

# ---- stages -----------------------------------------------------------------

stage_gates <- function(root) {
  cmds <- workflow_gates(file.path(root, WORKFLOW))
  cat(sprintf("[gates] %d step(s) derived from %s\n", length(cmds), WORKFLOW))
  lapply(cmds, function(cmd) {
    parts <- strsplit(cmd, "\\s+")[[1]]
    run_step(cmd, "Rscript", parts)
  })
}

stage_self_tests <- function(root) {
  cmds <- workflow_self_tests(file.path(root, WORKFLOW))
  changed <- changed_files()
  scripts <- sub(" --self-test$", "", cmds)
  selected <- if (opt_release) rep(TRUE, length(cmds)) else scripts %in% changed
  cat(sprintf(
    "[gate-self-tests] %d/%d corresponding implementation(s) changed\n",
    sum(selected), length(cmds)
  ))
  lapply(cmds[selected], function(cmd) {
    parts <- strsplit(cmd, "\\s+")[[1]]
    run_step(cmd, "Rscript", parts)
  })
}

stage_lint <- function() {
  cat("[lint] lintr::lint_package()\n")
  code <- paste(
    "l <- lintr::lint_package()",
    "if (length(l)) { print(l); quit(status = 1) }",
    "cat('0 lints\n')",
    sep = "; "
  )
  list(run_step("lintr::lint_package()", "Rscript", c("-e", shQuote(code))))
}

# The load-bearing stage, and the one no `devtools::test()` can stand in for.
# `R CMD check` must run on a BUILT TARBALL: building is what reads `Collate:`,
# and checking the tarball is what runs the tests against an INSTALLED package,
# where the file layout differs from the source tree. Both defects that got
# through were invisible to any instrument that skipped one of those two steps.
stage_check <- function(root) {
  cat("[check] R CMD build + R CMD check --as-cran (on the tarball)\n")
  # NOT under tempfile(): R deletes its session tempdir on exit, which would
  # take 00check.log with it -- so the one run you actually want to read, the
  # one that flagged something, is the one whose evidence is already gone.
  dir <- file.path(root, "_scratch", "verify-check")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dir)
  res <- run_step("R CMD build", file.path(R.home("bin"), "R"),
                  c("CMD", "build", shQuote(root)))
  if (!res$ok) {
    return(list(res))
  }
  tarball <- list.files(dir, pattern = "^rurl_.*\\.tar\\.gz$")
  if (length(tarball) != 1L) {
    cat("  FAIL build produced", length(tarball), "tarballs\n")
    return(list(res, list(label = "tarball", ok = FALSE, secs = 0)))
  }
  # _R_CHECK_SYSTEM_CLOCK_: a network-restricted machine cannot reach the time
  # server, and the resulting "unable to verify current time" NOTE is about the
  # sandbox, not the package (CLAUDE.md records the same workaround).
  chk <- run_step("R CMD check --as-cran", file.path(R.home("bin"), "R"),
                  c("CMD", "check", "--no-manual", "--as-cran", tarball),
                  env = "_R_CHECK_SYSTEM_CLOCK_=false")
  out <- file.path(dir, "rurl.Rcheck", "00check.log")
  if (file.exists(out)) {
    log <- readLines(out, warn = FALSE)
    # `R CMD check` exits 0 on a WARNING, so exit status alone is not the
    # verdict. NOTEs are reported but tolerated: several are unavoidable here
    # (the `Remotes` field, and every github.com URL 404s while the account is
    # suspended), and failing on them would make the gate cry wolf.
    flagged <- grep("^\\* checking .*(WARNING|NOTE)$", log, value = TRUE)
    if (length(flagged)) {
      cat(paste0("       ", flagged, collapse = "\n"), "\n", sep = "")
    }
    if (any(grepl("WARNING", flagged, fixed = TRUE)) && chk$ok) {
      cat("  FAIL check reported a WARNING (exit status alone does not)\n")
      chk$ok <- FALSE
    }
    cat(paste0("       ", grep("^Status:", log, value = TRUE), collapse = "\n"),
        "\n", sep = "")
    cat("       full log: ", sub(paste0("^", root, "/?"), "", out), "\n",
        sep = "")
  }
  list(res, chk)
}

# verify.yml's `Tests (LC_ALL=C)` cell. It is here rather than in the check
# stage because R CMD check runs in the ambient locale: a defect that only
# appears under a non-UTF-8 charset is invisible to every other stage, and this
# codebase has shipped that exact class of defect before (RURL-kmpnbvdl).
stage_locale <- function() {
  cat("[locale] test suite under LC_ALL=C\n")
  code <- paste(
    "stopifnot(identical(Sys.getlocale('LC_CTYPE'), 'C'))",
    "testthat::test_local(reporter = 'summary', stop_on_failure = TRUE)",
    sep = "; "
  )
  list(run_step("testthat under LC_ALL=C", "Rscript",
                c("-e", shQuote(code)),
                env = c("LC_ALL=C", "LANG=C")))
}

stage_release <- function() {
  cat("[release] curl zero-reference clean room (C7)\n")
  list(run_step("curl-zero-gate.R (full, incl. C7)", "Rscript",
                "tools/curl-zero-gate.R"))
}

stage_advisory <- function(root) {
  cat("[advisory] control-plane gate -- reported, never blocking\n")
  base <- suppressWarnings(system2("git", c("rev-parse", "main"),
                                   stdout = TRUE, stderr = FALSE))
  env <- if (length(base) == 1L) paste0("CI_GATE_BASE_SHA=", base) else
    character(0)
  res <- run_step("ci-gate.R (advisory)", "Rscript", ADVISORY, env = env)
  res$advisory <- TRUE
  list(res)
}

# ---- main -------------------------------------------------------------------

root <- repo_root()
plan <- c("gates", "selftests", "lint")
if (!opt_fast) {
  plan <- c(plan, "check", "locale")
}
if (opt_release) {
  plan <- c(plan, "release")
}
plan <- c(plan, "advisory")

if (opt_list) {
  cat("stage plan:", paste(plan, collapse = " -> "), "\n")
  cat("derived gate steps:\n")
  cat(paste0("  ", workflow_gates(file.path(root, WORKFLOW))), sep = "\n")
  cat("\nconditional gate self-tests:\n")
  cat(paste0("  ", workflow_self_tests(file.path(root, WORKFLOW))), sep = "\n")
  cat("\n")
  quit(status = 0)
}

cat("rurl local verify gate --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("stages:", paste(plan, collapse = " -> "), "\n\n")

results <- list()
for (st in plan) {
  results <- c(results, switch(
    st,
    gates = stage_gates(root),
    selftests = stage_self_tests(root),
    lint = stage_lint(),
    check = stage_check(root),
    locale = stage_locale(),
    release = stage_release(),
    advisory = stage_advisory(root)
  ))
  cat("\n")
}

blocking <- Filter(function(r) !isTRUE(r$advisory), results)
failed <- Filter(function(r) !r$ok, blocking)
advisory_failed <- Filter(function(r) isTRUE(r$advisory) && !r$ok, results)

cat(sprintf("%d blocking step(s), %d failed\n", length(blocking),
            length(failed)))
if (length(advisory_failed)) {
  cat("advisory RED (not blocking):",
      paste(vapply(advisory_failed, function(r) r$label, character(1)),
            collapse = ", "), "\n")
}
if (length(failed)) {
  cat("VERDICT: FAIL --",
      paste(vapply(failed, function(r) r$label, character(1)),
            collapse = ", "), "\n")
  quit(status = 1)
}
cat("VERDICT: PASS (this machine, this R -- see the header for what is not",
    "covered)\n")
