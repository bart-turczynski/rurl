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
#   * README.md re-render (verify.yml `readme`), coverage, the OSV and
#     security audits, news-version, and the determinism matrix (pkgdown is
#     the release-time `pages` job in .gitlab-ci.yml) -- all need
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
# WARNING-ONLY SIGNALS (RURL-pbihchti). A step's output is printed only when it
# FAILS, so anything real that does not change an exit status is structurally
# invisible here -- across a full run that is every passing step. The rationale
# for the quiet default is sound and is NOT reverted: a gate that dumps 40 lines
# per passing step is a gate whose real failures scroll past. What was missing
# is an escape hatch, plus a default-on instrument for the one place we know
# carries such signal.
#
# `--verbose` prints every step's log in full regardless of status. In FULL, not
# as the 25-line failure tail: RURL-aajradge's testthat WARN was ever visible
# only because it happened to fall inside those 25 lines while an unrelated
# defect made the same step FAIL. That is luck twice over, and a tail that can
# truncate the block you came for is not an escape hatch.
#
# `watch =` is the default-on half. A step may declare a regex; on a PASS whose
# log matches it, the matched block prints under a `!` marker. Quiet when there
# is nothing to say, loud exactly when there is -- so the known-hazardous step
# is honest by default rather than opt-in, and opt-in is precisely how
# `aajradge` stayed hidden. The check stage has done this ad hoc for WARNING and
# NOTE lines since it was written; `watch` is that idea, named and reusable.
#
# Usage:
#   Rscript tools/verify.R            # gates + relevant self-tests + full gate
#   Rscript tools/verify.R --gates    # gates + relevant self-tests ONLY
#   Rscript tools/verify.R --fast     # the above plus lint
#   Rscript tools/verify.R --release  # everything, plus the curl clean room
#   Rscript tools/verify.R --verbose  # print every step's log, passing included
#   Rscript tools/verify.R --list     # print the stage plan and exit
#   Rscript tools/verify.R --self-test  # prove the two instruments above
# `--fast` is iteration feedback only. It is never sufficient verification for
# a behavioral slice; the unsuffixed command remains the end-of-slice gate.
#
# `--gates` exists for CI on a compute-minutes budget: the gate family needs
# almost no installed packages and runs in about a minute, where the check stage
# needs the full toolchain and every dependency. It lets a pipeline run the
# cheap half on every push and the whole thing on the default branch WITHOUT
# hand-listing the gates in a CI config -- which is the drift this script exists
# to prevent (see "THE GATE LIST IS DERIVED, NOT TRANSCRIBED" above). Like
# `--fast`, it is not sufficient verification for a behavioral slice.
#
# Base R only. Exits 1 if any BLOCKING stage fails.

WORKFLOW <- ".github/workflows/verify.yml"

# There is no longer an advisory stage. The control-plane gate used to be one:
# it went red whenever a contract body was edited under an ACCEPTED acceptance
# gate, and clearing it needed an owner seal-merge rather than a code change, so
# wiring it as blocking would have failed every unrelated commit and trained
# people to pass `--no-verify`.
#
# ADR 0014 removed the cause instead of tolerating the symptom: the hash cascade
# and the seal are gone, and what survives of validate-records.R is structural
# and clearable by fixing the tree. It is wired into verify.yml as an ordinary
# blocking gate, so it arrives here through the derived list like any other.

args <- commandArgs(trailingOnly = TRUE)
opt_gates <- "--gates" %in% args
opt_fast <- "--fast" %in% args
opt_release <- "--release" %in% args
opt_list <- "--list" %in% args
opt_verbose <- "--verbose" %in% args
opt_self_test <- "--self-test" %in% args

# ---- helpers ----------------------------------------------------------------

repo_root <- function() {
  # `.git` is a directory in a clone and a FILE in a `git worktree`; both are
  # repositories, and subagents run the gate from worktrees.
  if (!file.exists("DESCRIPTION") || !file.exists(".git")) {
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

# git, with a failure reported as "no output" rather than an R error. A bad
# revision must not abort the run before a single self-test has been chosen.
git_lines <- function(args) {
  out <- tryCatch(
    suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL
  )
  if (is.null(out) || !is.null(attr(out, "status"))) character(0) else out
}

# The self-test selection diffs against main, so main has to EXIST. GitLab CI
# checks out a shallow, single-ref clone: no local `main`, no `origin/main`, and
# `git diff main...HEAD` is a fatal bad revision. That took down the whole gates
# job after all 14 gates had passed.
#
# Returning NULL means "cannot tell", which is NOT the same as "nothing
# changed" -- and the difference matters, because the two answers select
# opposite sets. Reading a missing ref as an empty diff would silently skip
# every self-test in the one place that is not opt-in per clone.
base_ref <- function() {
  for (ref in c("main", "origin/main")) {
    ok <- tryCatch(
      suppressWarnings(system2(
        "git", c("rev-parse", "--verify", "--quiet", ref),
        stdout = FALSE, stderr = FALSE
      )),
      error = function(e) 1L
    )
    if (identical(as.integer(ok), 0L)) {
      return(ref)
    }
  }
  NULL
}

rev_of <- function(ref) {
  out <- git_lines(c("rev-parse", ref))
  if (length(out) == 1L && nzchar(out)) out else NA_character_
}

# A classed empty vector, not NULL: R refuses to set an attribute on NULL
# ("attempt to set an attribute on NULL"), and the `reason` is what lets the
# printed line name which of the two unanswerable cases it hit instead of
# asserting the wrong one.
cannot_tell <- function(reason) {
  structure(character(0), class = "cannot_tell", reason = reason)
}

changed_files <- function() {
  base <- base_ref()
  if (is.null(base)) {
    return(cannot_tell("no base ref to diff against"))
  }
  uncommitted <- git_lines(c("diff", "--name-only", "HEAD"))
  # ON THE BASE BRANCH ITSELF the committed diff is vacuously empty -- nothing
  # has changed relative to main when you ARE main -- so selection quietly
  # picked ZERO self-tests in the job that is supposed to be the thorough one.
  # Measured on GitLab: two jobs of ONE pipeline, same commit, disagreed 16/16
  # against 0/16, purely because the check stage's apt install transitively
  # pulls in git while the gates stage's does not. Verification depth must not
  # hinge on a package manager's transitive closure.
  #
  # Uncommitted edits are still a real signal here, though, and dropping them
  # would cost the local optimization on a freshly branched tree -- where HEAD
  # is still the base commit but files are already modified. So fall back to
  # them, and only give up when the tree is clean too, which is exactly the CI
  # case.
  head_rev <- rev_of("HEAD")
  base_rev <- rev_of(base)
  if (!is.na(head_rev) && identical(head_rev, base_rev)) {
    if (length(uncommitted)) {
      return(uncommitted)
    }
    return(cannot_tell(sprintf("HEAD is %s and the tree is clean", base)))
  }
  committed <- git_lines(c("diff", "--name-only", paste0(base, "...HEAD")))
  unique(c(committed, uncommitted))
}

read_log <- function(path) {
  tryCatch(readLines(path, warn = FALSE), error = function(e) character())
}

# The block a `watch` hit selects: from the first matching line to the end of
# the log. Matching lines alone would be useless for the case that motivated
# this -- testthat's `== Warnings ==` header carries no information, the
# numbered entries UNDER it do. Capped, because "to the end" is only short by
# convention.
watch_block <- function(txt, watch, cap = 40L) {
  hit <- grep(watch, txt)
  if (!length(hit)) {
    return(character(0))
  }
  utils::head(txt[seq(hit[1L], length(txt))], cap)
}

# One command, output captured. What prints afterwards, in precedence order:
#
#   --verbose  -> the whole log, pass or fail. See the header for why full and
#                 not the tail.
#   FAIL       -> the last 25 lines. Unchanged; this is the default that works.
#   PASS+watch -> the watched block under a `!` marker, if the log matched.
#   otherwise  -> nothing. A passing gate that dumps 40 lines is how a real
#                 failure gets scrolled past.
run_step <- function(label, command, args = character(0), env = character(0),
                     watch = NULL, verbose = opt_verbose) {
  log <- tempfile(fileext = ".log")
  t0 <- Sys.time()
  status <- suppressWarnings(system2(command, args, stdout = log,
                                     stderr = log, env = env))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok <- identical(as.integer(status), 0L)
  cat(sprintf("  %-4s %-58s %5.1fs\n", if (ok) "PASS" else "FAIL",
              substr(label, 1L, 58L), secs))
  if (verbose) {
    cat(paste0("       | ", read_log(log), collapse = "\n"), "\n", sep = "")
  } else if (!ok) {
    cat(paste0("       | ", utils::tail(read_log(log), 25L), collapse = "\n"),
        "\n", sep = "")
  } else if (!is.null(watch)) {
    block <- watch_block(read_log(log), watch)
    if (length(block)) {
      cat("       ! this step PASSED but its output matched a watched",
          "pattern\n       ! (--verbose for the whole log):\n")
      cat(paste0("       ! ", block, collapse = "\n"), "\n", sep = "")
    }
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
  unknown <- inherits(changed, "cannot_tell")
  selected <- if (opt_release || unknown) {
    rep(TRUE, length(cmds))
  } else {
    scripts %in% changed
  }
  cat(sprintf(
    "[gate-self-tests] %d/%d %s\n", sum(selected), length(cmds),
    if (unknown) {
      sprintf("-- %s, so running every one", attr(changed, "reason"))
    } else {
      "corresponding implementation(s) changed"
    }
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
  # sandbox, not the package. Set it for a local `R CMD check` too if you hit
  # that NOTE off-network; it is not needed on a machine with normal access.
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

# testthat's summary reporter heads its warning section with a rule of box
# characters -- BUT it degrades that rule to plain `=` when the locale cannot
# represent U+2550, which is exactly the locale this step forces. Both spellings
# have to match or the watch is vacuous in the only step that carries it.
# Written as an escape, not as the literal character: this file is source that
# gets parsed in whatever locale the runner happens to be in.
TESTTHAT_WARNINGS <- "^(=|\u2550){2} Warnings"

# verify.yml's `Tests (LC_ALL=C)` cell. It is here rather than in the check
# stage because R CMD check runs in the ambient locale: a defect that only
# appears under a non-UTF-8 charset is invisible to every other stage, and this
# codebase has shipped that exact class of defect before (RURL-kmpnbvdl).
#
# It is also the step with a KNOWN warning-only signal: RURL-aajradge is a
# testthat WARN that reproduces under Linux/C and changes no exit status, so
# this step reports PASS and drops it. Hence the `watch` -- the summary reporter
# emits a warnings section only when there are warnings, so this prints nothing
# on a clean run and the whole section when there is one.
stage_locale <- function() {
  cat("[locale] test suite under LC_ALL=C\n")
  code <- paste(
    "stopifnot(identical(Sys.getlocale('LC_CTYPE'), 'C'))",
    "testthat::test_local(reporter = 'summary', stop_on_failure = TRUE)",
    sep = "; "
  )
  list(run_step("testthat under LC_ALL=C", "Rscript",
                c("-e", shQuote(code)),
                env = c("LC_ALL=C", "LANG=C"),
                watch = TESTTHAT_WARNINGS))
}

stage_release <- function() {
  cat("[release] curl zero-reference clean room (C7)\n")
  list(run_step("curl-zero-gate.R (full, incl. C7)", "Rscript",
                "tools/curl-zero-gate.R"))
}

# ---- self-test --------------------------------------------------------------

# A verbosity feature that finds nothing is indistinguishable from a verbosity
# feature that is broken, so the cases below are run BOTH ways: every positive
# control is paired with the negative control that fails on today's code. Case 1
# is that negative control -- it asserts the defect RURL-pbihchti describes is
# real, and it is the one case that must keep passing after the fix, because the
# quiet default is deliberate.
#
# The output is captured rather than eyeballed. `run_step()` writes with `cat`,
# so `capture.output()` sees exactly what a runner would.
self_test <- function() {
  # A case is a name and a thunk, evaluated below. Building the list first keeps
  # the tally out of a mutable counter, which this repo's linter set rejects.
  case <- function(name, cond) list(name = name, cond = cond)

  # A canary chosen to be the real thing: this is the substring RURL-aajradge's
  # warning is identified by.
  canary <- "strings not representable in native encoding"
  emit <- function(status) {
    c("-e", shQuote(sprintf("cat(%s); quit(status = %d)",
                            shQuote(canary), status)))
  }
  step <- function(status, ...) {
    paste(capture.output(
      run_step("self-test", "Rscript", emit(status), ...)
    ), collapse = "\n")
  }
  saw <- function(out) grepl(canary, out, fixed = TRUE)

  # The block, not just the matching line: the header carries no information.
  block <- watch_block(
    c("noise", "== Warnings ====", "1. a test ('t.R:6:3') - the message",
      "== DONE ===="),
    TESTTHAT_WARNINGS
  )

  cases <- list(
    case(
      "NEGATIVE: a PASSING step with no watch stays quiet (the default)",
      !saw(step(0L, verbose = FALSE))
    ),
    case(
      "POSITIVE: --verbose surfaces a PASSING step's output",
      saw(step(0L, verbose = TRUE))
    ),
    case(
      "POSITIVE: a matching watch surfaces it without --verbose",
      saw(step(0L, verbose = FALSE, watch = canary))
    ),
    case(
      "NEGATIVE: a NON-matching watch stays quiet",
      !saw(step(0L, verbose = FALSE, watch = "^no such line$"))
    ),
    case(
      "UNCHANGED: a FAILING step still prints its tail with no flag",
      saw(step(1L, verbose = FALSE))
    ),
    case(
      "UNCHANGED: a watch does not suppress a FAILING step's tail",
      saw(step(1L, verbose = FALSE, watch = "^no such line$"))
    ),
    case(
      "the watch marker says PASSED, so a `!` block is not read as failure",
      grepl("PASSED but its output matched",
            step(0L, verbose = FALSE, watch = canary), fixed = TRUE)
    ),
    # The locale step's pattern, against both spellings testthat can emit. A
    # watch that only matched the UTF-8 rule would be VACUOUS in the one step
    # that carries it, since that step forces the locale which degrades it.
    case(
      "TESTTHAT_WARNINGS matches the C-locale (ASCII) header",
      grepl(TESTTHAT_WARNINGS, "== Warnings =========")
    ),
    case(
      "TESTTHAT_WARNINGS matches the UTF-8 header",
      grepl(TESTTHAT_WARNINGS, "\u2550\u2550 Warnings \u2550\u2550\u2550")
    ),
    case(
      "TESTTHAT_WARNINGS does not match the section that always prints",
      !grepl(TESTTHAT_WARNINGS, "== DONE =========")
    ),
    case(
      "a watch hit prints the entries UNDER the header, not it alone",
      any(grepl("the message", block, fixed = TRUE))
    ),
    case(
      "a watch hit drops what preceded the header",
      !any(grepl("noise", block, fixed = TRUE))
    )
  )

  ok <- vapply(cases, function(k) isTRUE(k$cond), logical(1))
  for (i in seq_along(cases)) {
    cat(sprintf("  %-4s %s\n", if (ok[i]) "ok" else "FAIL", cases[[i]]$name))
  }
  cat(sprintf("\n%d case(s), %d failed\n", length(cases), sum(!ok)))
  if (!all(ok)) {
    quit(status = 1)
  }
  cat("SELF-TEST PASS\n")
  quit(status = 0)
}

if (opt_self_test) {
  cat("tools/verify.R --self-test: step output visibility (RURL-pbihchti)\n")
  self_test()
}

# ---- main -------------------------------------------------------------------

root <- repo_root()
plan <- c("gates", "selftests")
if (!opt_gates) {
  plan <- c(plan, "lint")
}
if (!opt_gates && !opt_fast) {
  plan <- c(plan, "check", "locale")
}
if (opt_release) {
  plan <- c(plan, "release")
}

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
    release = stage_release()
  ))
  cat("\n")
}

blocking <- results
failed <- Filter(function(r) !r$ok, blocking)

cat(sprintf("%d blocking step(s), %d failed\n", length(blocking),
            length(failed)))
if (length(failed)) {
  cat("VERDICT: FAIL --",
      paste(vapply(failed, function(r) r$label, character(1)),
            collapse = ", "), "\n")
  quit(status = 1)
}
cat("VERDICT: PASS (this machine, this R -- see the header for what is not",
    "covered)\n")
