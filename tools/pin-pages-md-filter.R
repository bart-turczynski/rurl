#!/usr/bin/env Rscript

# Pin for the `pages` job's agent-md filter in .gitlab-ci.yml (SEOR-wqxhftpv).
#
# WHY THIS EXISTS. pkgdown renders every top-level .md file except the handful
# pkgdown:::package_mds() hardcodes (README/NEWS/LICENSE/cran-comments), and
# _pkgdown.yml has no setting that can exclude one. The `pages` job's
# agent-md-filter step therefore keeps a named allow-list and moves everything
# else out of the checkout before build_site() runs. A deny-list version of
# that step (name the two known-bad files, leave everything else) is
# allow-by-default: a new agent-tooling file under any other name survives and
# gets published with nothing noticing -- how the AGENTS.html/CLAUDE.html leak
# reached four sibling repos after being fixed in one.
#
# WHAT THIS PINS, AND HOW. Not R behavior, the CI FILE'S TEXT, and it does not
# keep a second, hand-typed copy of the keep-list to compare against: it reads
# the filter script verbatim out of `.gitlab-ci.yml` (via the
# `agent-md-filter:start`/`:end` markers), PARSES the same case statement the
# script itself executes to learn what it claims to keep, then RUNS that exact
# script text against a scratch copy of the repo's real tracked top-level .md
# files plus one randomly-named canary file that cannot be on any real
# keep-list. It asserts the script's actual survivor set equals what its own
# case statement predicts. A hand-maintained second list would drift the
# moment someone edits the case statement without remembering to edit this
# file too; parsing the same text the script runs cannot drift that way.
# Instead this catches the OTHER two ways the step can silently stop doing its
# job: the case statement's `*)` catch-all arm being removed (the canary then
# survives, because the parsed keep-list still predicts it should not) and the
# `for req in ...` fail-loud guard being defeated (a required file goes
# missing but the filter still exits 0). See tools/verify-manifest.yml's
# `agent_md_filter` / `agent_md_filter_selftest` filters for where this is
# wired into the gate.
#
# Usage:
#   Rscript tools/pin-pages-md-filter.R             # check the real repo
#   Rscript tools/pin-pages-md-filter.R --self-test  # positive/negative unit checks

# --- extraction ---------------------------------------------------------------

read_ci_file <- function(path) {
  if (!file.exists(path)) {
    stop("cannot find ", path, call. = FALSE)
  }
  readLines(path, warn = FALSE)
}

# The exact lines between (and including) the marker comments, so what this
# script runs is what the real job runs -- never a re-transcription of it.
extract_filter_script <- function(ci_lines) {
  start <- grep("agent-md-filter:start", ci_lines)
  end <- grep("agent-md-filter:end", ci_lines)
  if (length(start) == 0L || length(end) == 0L) {
    stop("could not find agent-md-filter:start/:end markers in the CI file",
         call. = FALSE)
  }
  if (end[[1L]] < start[[1L]]) {
    stop("agent-md-filter:end appears before :start in the CI file",
         call. = FALSE)
  }
  ci_lines[seq.int(start[[1L]], end[[1L]])]
}

# The literal names in the case statement's FIRST arm -- `case "$f" in`,
# then the very next non-blank line, which must be a single-line
# `NAME|NAME|...)` pattern list. That is also the arm the script itself
# treats as "keep": everything else falls to the `*)` default and gets moved.
parse_case_keep_list <- function(script_lines) {
  case_at <- grep('case\\s+"\\$f"\\s+in\\s*$', script_lines)
  if (length(case_at) == 0L) {
    stop('could not find `case "$f" in` in the filter script', call. = FALSE)
  }
  rest <- script_lines[seq.int(case_at[[1L]] + 1L, length(script_lines))]
  rest <- rest[nzchar(trimws(rest))]
  if (length(rest) == 0L || !grepl("\\)\\s*$", rest[[1L]])) {
    stop("the case statement's first arm is not a single-line pattern list",
         call. = FALSE)
  }
  arm <- sub("\\)\\s*$", "", trimws(rest[[1L]]))
  trimws(strsplit(arm, "\\|")[[1L]])
}

# The names in `for req in NAME NAME ...; do` -- the files whose absence the
# script is claimed to fail loud on. Absent line means no such guard exists.
parse_required_files <- function(script_lines) {
  hit <- grep("^\\s*for req in ", script_lines)
  if (length(hit) == 0L) {
    return(character(0))
  }
  m <- regmatches(script_lines[[hit[[1L]]]],
                   regexec("for req in (.*); do", script_lines[[hit[[1L]]]]))[[1L]]
  if (length(m) < 2L) {
    return(character(0))
  }
  strsplit(trimws(m[[2L]]), "\\s+")[[1L]]
}

tracked_top_level_md <- function(root) {
  out <- suppressWarnings(system2("git", c("-C", root, "ls-files", "--", "*.md"),
                                   stdout = TRUE, stderr = FALSE))
  status <- attr(out, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop("`git ls-files -- *.md` failed", call. = FALSE)
  }
  sort(unique(out[!grepl("/", out, fixed = TRUE)]))
}

# --- execution -----------------------------------------------------------

# Run the extracted script text VERBATIM (only /tmp/agent-md is redirected, to
# a private scratch dir, so this never touches a real pipeline's /tmp) against
# a scratch directory seeded with `seed_files`. Returns the exit status, the
# combined stdout/stderr, and which of the seeded files still sit at the
# scratch root afterwards.
run_filter <- function(script_lines, seed_files, scratch_target) {
  work <- tempfile("agent-md-filter-run-")
  dir.create(work)
  old_wd <- setwd(work)
  status <- 0L
  out <- character(0)
  tryCatch(
    {
      for (f in seed_files) {
        file.create(f)
      }
      script_text <- paste(script_lines, collapse = "\n")
      script_text <- gsub("/tmp/agent-md", scratch_target, script_text,
                           fixed = TRUE)
      script_file <- tempfile("agent-md-filter-script-", fileext = ".sh")
      writeLines(script_text, script_file)
      on.exit(unlink(script_file), add = TRUE)
      res <- suppressWarnings(system2("sh", script_file, stdout = TRUE,
                                       stderr = TRUE))
      st <- attr(res, "status")
      status <- if (is.null(st)) 0L else as.integer(st)
      out <- res
    },
    finally = setwd(old_wd)
  )
  survivors <- sort(basename(Sys.glob(file.path(work, "*.md"))))
  unlink(work, recursive = TRUE)
  unlink(scratch_target, recursive = TRUE)
  list(status = status, output = out, survivors = survivors)
}

# --- the check -----------------------------------------------------------

# Returns keep_list, required, the run result, and a character vector of
# violation messages (empty means sound). `extra_files` is where a self-test
# injects a canary or a deliberately-missing-required scenario.
check_pages_filter <- function(ci_path, tracked_files, extra_files = character(0)) {
  ci_lines <- read_ci_file(ci_path)
  script_lines <- extract_filter_script(ci_lines)
  keep_list <- parse_case_keep_list(script_lines)
  required <- parse_required_files(script_lines)

  seed <- sort(unique(c(tracked_files, extra_files)))
  scratch_target <- tempfile("agent-md-filter-moved-")
  run <- run_filter(script_lines, seed, scratch_target)

  violations <- character(0)
  missing_required <- setdiff(required, seed)

  if (length(missing_required) > 0L) {
    if (identical(run$status, 0L)) {
      violations <- c(violations, sprintf(
        paste0("the filter exited 0 despite missing required file(s) %s -- ",
               "the fail-loud guard is broken"),
        paste(missing_required, collapse = ", ")
      ))
    }
  } else if (!identical(run$status, 0L)) {
    violations <- c(violations, sprintf(
      "the filter exited %d even though every required file was present: %s",
      run$status, paste(run$output, collapse = " | ")
    ))
  } else {
    expected <- sort(intersect(seed, keep_list))
    if (!identical(run$survivors, expected)) {
      violations <- c(violations, sprintf(
        paste0("the filter's survivor set does not match its own case ",
               "statement's keep-list.\n    expected: %s\n    actual:   %s"),
        paste(expected, collapse = ", "), paste(run$survivors, collapse = ", ")
      ))
    }
  }

  list(keep_list = keep_list, required = required, seed = seed, run = run,
       violations = violations)
}

# --- self-test (positive + negative coverage, executable) --------------------

fixture_ci_text <- function(body_lines) {
  c(
    "# fixture: minimal pages job for pin-pages-md-filter.R's self-test",
    "pages:",
    "  script:",
    "    - |",
    body_lines
  )
}

correct_body <- c(
  "      # --- agent-md-filter:start ---",
  "      set -e",
  "      mkdir -p /tmp/agent-md",
  "      for req in README.md NEWS.md LICENSE.md; do",
  '        test -f "$req" || { echo "keep-list file missing: $req" >&2; exit 1; }',
  "      done",
  "      for f in *.md; do",
  '        case "$f" in',
  "          README.md|NEWS.md|LICENSE.md|CONTRIBUTING.md)",
  "            ;;",
  "          *)",
  '            mv "$f" /tmp/agent-md/',
  "            ;;",
  "        esac",
  "      done",
  "      # --- agent-md-filter:end ---"
)

# BUG: the `*)` catch-all arm is gone. `case` with no default silently does
# nothing for a name it does not match, so an unlisted file is left in place
# -- exactly the fail-open regression SEOR-wqxhftpv exists to catch. The
# keep-list text itself is untouched, so a parser reading only that text would
# predict the same (safe) survivor set a correct filter would produce; only
# actually running the script reveals the gap.
missing_default_body <- c(
  "      # --- agent-md-filter:start ---",
  "      set -e",
  "      mkdir -p /tmp/agent-md",
  "      for req in README.md NEWS.md LICENSE.md; do",
  '        test -f "$req" || { echo "keep-list file missing: $req" >&2; exit 1; }',
  "      done",
  "      for f in *.md; do",
  '        case "$f" in',
  "          README.md|NEWS.md|LICENSE.md|CONTRIBUTING.md)",
  "            ;;",
  "        esac",
  "      done",
  "      # --- agent-md-filter:end ---"
)

# BUG: the fail-loud guard swallows the failure instead of exiting on it.
swallowed_required_body <- c(
  "      # --- agent-md-filter:start ---",
  "      set -e",
  "      mkdir -p /tmp/agent-md",
  "      for req in README.md NEWS.md LICENSE.md; do",
  '        test -f "$req" || echo "warning: missing $req" >&2',
  "      done",
  "      for f in *.md; do",
  '        case "$f" in',
  "          README.md|NEWS.md|LICENSE.md|CONTRIBUTING.md)",
  "            ;;",
  "          *)",
  '            mv "$f" /tmp/agent-md/',
  "            ;;",
  "        esac",
  "      done",
  "      # --- agent-md-filter:end ---"
)

self_test <- function() {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  base <- tempfile("pin-pages-md-filter-selftest-")
  dir.create(base)

  write_ci <- function(tag, body_lines) {
    d <- file.path(base, tag)
    dir.create(d)
    p <- file.path(d, ".gitlab-ci.yml")
    writeLines(fixture_ci_text(body_lines), p)
    p
  }

  seed_ok <- c("README.md", "NEWS.md", "LICENSE.md", "CONTRIBUTING.md",
               "ARCHITECTURE.md")

  # POSITIVE: correct filter keeps exactly the keep-list names present, and
  # excludes ARCHITECTURE.md, which is not on it.
  ci_ok <- write_ci("ok", correct_body)
  res <- check_pages_filter(ci_ok, seed_ok)
  if (length(res$violations) > 0L) {
    fail(sprintf("false positive on a correct filter: %s",
                 paste(res$violations, collapse = "; ")))
  }
  if (!identical(res$run$survivors,
                 sort(c("README.md", "NEWS.md", "LICENSE.md", "CONTRIBUTING.md")))) {
    fail("correct filter did not keep exactly the expected files")
  }

  # NEGATIVE: the measured regression -- no `*)` arm, so an injected, unlisted
  # canary file survives even though the parsed keep-list still predicts it
  # should not.
  ci_bug <- write_ci("missing-default", missing_default_body)
  res <- check_pages_filter(ci_bug, seed_ok, extra_files = "GEMINI.md")
  if (!("GEMINI.md" %in% res$run$survivors)) {
    fail("fixture did not reproduce the fail-open bug it exists to demonstrate")
  }
  if (!any(grepl("does not match its own case statement", res$violations))) {
    fail("did not flag an unlisted file surviving a filter with no default arm")
  }

  # NEGATIVE: the fail-loud guard is defeated -- README.md is absent but the
  # filter exits 0 anyway.
  ci_swallow <- write_ci("swallowed-required", swallowed_required_body)
  res <- check_pages_filter(ci_swallow, c("NEWS.md", "LICENSE.md"))
  if (!identical(res$run$status, 0L)) {
    fail("fixture did not reproduce exit-0-despite-missing-file")
  }
  if (!any(grepl("fail-loud guard is broken", res$violations))) {
    fail("did not flag a fail-loud guard that swallows a missing required file")
  }

  # POSITIVE: the real fail-loud guard correctly aborts when README.md is
  # absent, and that is read as compliant, not broken.
  res <- check_pages_filter(ci_ok, c("NEWS.md", "LICENSE.md"))
  if (length(res$violations) > 0L) {
    fail(sprintf("false positive when the real fail-loud guard correctly aborts: %s",
                 paste(res$violations, collapse = "; ")))
  }
  if (identical(res$run$status, 0L)) {
    fail("expected the real fail-loud guard to abort (non-zero exit) when README.md is missing")
  }

  # Parser unit checks, directly against the "ok" fixture's own text.
  ci_lines <- readLines(ci_ok, warn = FALSE)
  sl <- extract_filter_script(ci_lines)
  if (!identical(parse_case_keep_list(sl),
                 c("README.md", "NEWS.md", "LICENSE.md", "CONTRIBUTING.md"))) {
    fail("parse_case_keep_list did not extract the fixture's keep-list")
  }
  if (!identical(parse_required_files(sl),
                 c("README.md", "NEWS.md", "LICENSE.md"))) {
    fail("parse_required_files did not extract the fixture's required list")
  }

  # NEGATIVE: missing markers must error, not silently return nothing.
  no_marker_ok <- tryCatch(
    { extract_filter_script(c("pages:", "  script:", "    - echo hi")); FALSE },
    error = function(e) TRUE
  )
  if (!no_marker_ok) {
    fail("extract_filter_script did not error when the markers are absent")
  }

  unlink(base, recursive = TRUE)
  cat("pin-pages-md-filter self-test: PASS (2 positive + 2 negative + 3 parser checks)\n")
  invisible(TRUE)
}

# --- main ------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }

  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  ci_path <- file.path(root, ".gitlab-ci.yml")
  tracked <- tracked_top_level_md(root)
  canary <- sprintf(
    "AGENT-MD-FILTER-CANARY-%s.md",
    paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
  )

  res <- check_pages_filter(ci_path, tracked, extra_files = canary)

  cat(sprintf(
    paste0("pages agent-md filter gate\n",
           "  %d top-level .md file(s) tracked\n",
           "  keep-list parsed from .gitlab-ci.yml: %s\n",
           "  required (fail-loud): %s\n",
           "  canary (must never survive): %s\n"),
    length(tracked), paste(res$keep_list, collapse = ", "),
    paste(res$required, collapse = ", "), canary
  ))

  if (length(res$violations) > 0L) {
    cat("GAP:\n")
    for (v in res$violations) cat("  - ", v, "\n", sep = "")
    stop(sprintf(
      "the pages job's agent-md filter is out of sync with its own keep-list (%d finding(s))",
      length(res$violations)
    ), call. = FALSE)
  }
  cat("PASS: the filter's survivor set matches its own declared keep-list, ",
      "and the canary file was excluded.\n", sep = "")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  if (sys.nframe() == 0L) {
    main()
  }
}
