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
# of tools/verify-manifest.yml at run time. A hand-maintained copy would be
# one more list to forget: add a gate to the manifest, and a local mirror that
# repeats it by hand is silently incomplete from that moment. Reading the
# manifest means a new gate is picked up here the day it lands, and a gate
# REMOVED from it stops running here too. It also means this script cannot
# claim to mirror the manifest while quietly running something else.
#
# The manifest was `.github/workflows/verify.yml` until RURL-vunvxusf moved it
# under tools/ and deleted the dead GitHub workflows around it. It keeps the
# GitHub-Actions `jobs:`/`steps:` shape because that shape is what this
# parser reads and what carries each gate's rationale; no forge executes it.
# `.gitlab-ci.yml` runs this script, so GitLab consumes the same manifest.
#
# GATE SELF-TESTS ARE A SEPARATE RISK CLASS. Their positive/negative fixtures
# prove the verifier, not the product, so routine runs select one only when the
# diff against main touches a path in the manifest's paths filter that gates it
# (the `changes` job's `filters:` block), or the self-test's own script. A
# self-test that maps to no filter is selected and named, never skipped (see
# "self-test selection" below). `--release` deliberately runs every self-test,
# and so does a run that cannot tell what changed. The real tree scans still
# run in every complete local gate.
#
# WHAT IT DOES NOT COVER, stated so nobody reads a green run as more than it is:
#   * cross-platform and multi-R-version checks -- this runs one platform,
#     one R, and the GitHub matrix workflows that used to cover the rest
#     (full-check, rhub) are deleted, so nothing does;
#   * README.md re-render (the manifest's `readme` job), coverage,
#     news-version, and the determinism matrix (pkgdown is the release-time
#     `pages` job in .gitlab-ci.yml) -- all need network, a pandoc/LaTeX
#     toolchain, or a Docker matrix;
#   * the OSV and OSS Index advisory audits (test-osv.R, test-security.R).
#     The locale cell EXCLUDES them by name and `R CMD check` skips them
#     (NOT_CRAN unset), so no stage here runs them -- see stage_locale();
#   * the C7 curl clean room, which needs its own R CMD check against a poisoned
#     library. `--release` adds it; the default does not, because it doubles the
#     slowest stage to re-prove a criterion that only matters at release.
#
# It is not a strict subset of the manifest in either direction, so neither
# "green here means green there" nor its converse holds. The check stage runs
# `--as-cran`, which is STRICTER than the manifest's `check` job (that one
# passes `--no-manual` alone and left `--as-cran` to the deleted full-check
# workflow), so this can fail where that job would have passed. A pass here
# means "the fast gate's checks hold on this machine" -- not "the release is
# ready", and not "CI will be green".
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
#   Rscript tools/verify.R --fast     # the above plus lint and spelling
#   Rscript tools/verify.R --release  # everything, plus the curl clean room
#   Rscript tools/verify.R --verbose  # print every step's log, passing included
#   Rscript tools/verify.R --list     # print the stage plan, and the
#                                     # self-tests this diff selects, and exit
#   Rscript tools/verify.R --self-test  # prove the two instruments above,
#                                       # and self-test selection
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
# Base R, plus `yaml` to read the self-tests' paths filters; without `yaml`,
# self-test selection falls back to each script's own path and says so. Exits 1
# if any BLOCKING stage fails.

MANIFEST <- "tools/verify-manifest.yml"

# There is no longer an advisory stage. The control-plane gate used to be one:
# it went red whenever a contract body was edited under an ACCEPTED acceptance
# gate, and clearing it needed an owner seal-merge rather than a code change, so
# wiring it as blocking would have failed every unrelated commit and trained
# people to pass `--no-verify`.
#
# ADR 0014 removed the cause instead of tolerating the symptom: the hash cascade
# and the seal are gone, and what survives of validate-records.R is structural
# and clearable by fixing the tree. It is wired into the manifest as an
# ordinary blocking gate, so it arrives here through the derived list like any
# other.

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

# Gate invocations as the manifest spells them. `run: Rscript <script> [args]`
# is the shape every gate step uses; the manifest's other steps are actions or
# multi-line shell, and neither is a gate.
manifest_gates <- function(path) {
  if (!file.exists(path)) {
    stop("cannot read ", path, " -- the gate list is derived from it",
         call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  hits <- grep("^\\s*run: Rscript\\s+\\S", lines, value = TRUE)
  cmds <- sub("^\\s*run: Rscript\\s+", "", hits)
  unique(trimws(cmds[!grepl(" --self-test", cmds, fixed = TRUE)]))
}

manifest_self_tests <- function(path) {
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

# ---- self-test selection (RURL-etafkksg) -----------------------------------
#
# A self-test runs when the diff touches a path in the paths filter that gates
# it in the manifest, not only its own script: the `changes` job declares one
# filter per self-test, and a self-test can be the only check on a file other
# than its script (tools/local-ci-plan.R --self-test is the only one on
# tools/local-ci.sh). The chain is the step running
# `Rscript <script> --self-test` -> the `needs.changes.outputs.<filter>` its
# `if:` names (the step's own, else its job's) -> that filter's path list.
# The two functions below are pure over a parsed manifest, so the self-test
# feeds them fixtures without a repository.

# The `changes` job's paths filters as a named list, filter -> paths.
# dorny/paths-filter takes `filters:` as a YAML document inside a YAML string,
# so it is parsed a second time. A missing or unparsable block yields an empty
# list: every self-test is then unmappable, and so selected, never skipped.
manifest_filters <- function(manifest) {
  steps <- manifest[["jobs"]][["changes"]][["steps"]]
  hit <- Filter(function(s) {
    is.list(s) && is.character(s[["uses"]]) &&
      startsWith(s[["uses"]][1L], "dorny/paths-filter")
  }, steps)
  text <- if (length(hit)) hit[[1L]][["with"]][["filters"]]
  if (!is.character(text) || length(text) != 1L) {
    return(list())
  }
  parsed <- tryCatch(yaml::yaml.load(text), error = function(e) NULL)
  if (!is.list(parsed)) {
    return(list())
  }
  lapply(parsed, function(p) as.character(unlist(p)))
}

# The filter names an `if:` expression reads.
if_filters <- function(cond) {
  if (!is.character(cond) || length(cond) != 1L) {
    return(character(0))
  }
  hits <- regmatches(cond, gregexpr(
    "needs\\.changes\\.outputs\\.[A-Za-z0-9_-]+", cond
  ))[[1L]]
  unique(sub("^needs\\.changes\\.outputs\\.", "", hits))
}

# Self-test command (as manifest_self_tests() spells it) -> the paths of the
# filter that gates it. An empty vector means the command maps to no filter
# with paths: its `if:` names none, or names one the `changes` job lacks.
self_test_filter_map <- function(manifest) {
  filters <- manifest_filters(manifest)
  entries <- unlist(lapply(manifest[["jobs"]], function(job) {
    lapply(job[["steps"]], function(s) {
      run <- if (is.list(s)) s[["run"]]
      if (!is.character(run) || length(run) != 1L ||
            !grepl("^\\s*Rscript\\s+\\S+ --self-test\\s*$", run)) {
        return(NULL)
      }
      gate <- if_filters(s[["if"]])
      if (!length(gate)) {
        gate <- if_filters(job[["if"]])
      }
      list(cmd = sub("^Rscript\\s+", "", trimws(run)),
           paths = unlist(filters[gate], use.names = FALSE))
    })
  }), recursive = FALSE)
  entries <- Filter(Negate(is.null), entries)
  cmds <- unique(vapply(entries, function(e) e$cmd, character(1)))
  stats::setNames(lapply(cmds, function(cmd) {
    mine <- Filter(function(e) identical(e$cmd, cmd), entries)
    unique(as.character(unlist(lapply(mine, function(e) e$paths))))
  }), cmds)
}

# A dorny/paths-filter (picomatch) glob as an anchored regex, for the two
# wildcards the manifest uses: `**` crosses `/`, `*` does not. A `**/` also
# matches no directory at all, as picomatch has it. Everything else is literal.
glob_to_regex <- function(glob) {
  rx <- gsub("([][.+?^$(){}|\\\\])", "\\\\\\1", glob, perl = TRUE)
  rx <- gsub("**/", "\001", rx, fixed = TRUE)
  rx <- gsub("**", "\002", rx, fixed = TRUE)
  rx <- gsub("*", "[^/]*", rx, fixed = TRUE)
  rx <- gsub("\001", "(?:.*/)?", rx, fixed = TRUE)
  rx <- gsub("\002", ".*", rx, fixed = TRUE)
  paste0("^", rx, "$")
}

# Which self-tests a diff selects: those whose own script or any filter path
# matches a changed file, plus every unmappable one, which is SELECTED rather
# than skipped and named in the `unmappable` attribute so the caller can say
# so. A self-test the runner cannot reason about must not go quiet.
select_self_tests <- function(map, changed) {
  touched <- function(globs) {
    any(vapply(globs, function(g) {
      any(grepl(glob_to_regex(g), changed, perl = TRUE))
    }, logical(1)))
  }
  unmappable <- names(map)[lengths(map) == 0L]
  sel <- vapply(names(map), function(cmd) {
    cmd %in% unmappable || sub(" --self-test$", "", cmd) %in% changed ||
      touched(map[[cmd]])
  }, logical(1))
  structure(sel, unmappable = unmappable)
}

# The map for the manifest's self-test commands `cmds`, aligned to them: a
# command the yaml reading did not find maps to nothing, so it is selected.
# Without `yaml` (CI's gates image installs r-cran-yaml) each self-test maps
# to its own script path, the selection before RURL-etafkksg, and the `note`
# attribute says so.
read_self_test_map <- function(path, cmds) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    return(structure(
      stats::setNames(as.list(sub(" --self-test$", "", cmds)), cmds),
      note = paste("[gate-self-tests] the `yaml` package is not installed,",
                   "so each self-test is selected by its own script path",
                   "alone, not by its paths filter")
    ))
  }
  parsed <- tryCatch(yaml::read_yaml(path), error = function(e) e)
  failed <- inherits(parsed, "error")
  map <- if (failed) list() else self_test_filter_map(parsed)
  structure(stats::setNames(lapply(cmds, function(cmd) {
    if (cmd %in% names(map)) map[[cmd]] else character(0)
  }), cmds), note = if (failed) {
    sprintf("[gate-self-tests] cannot parse %s (%s), so no self-test maps",
            MANIFEST, conditionMessage(parsed))
  })
}

# What the [gate-self-tests] stage runs on this diff, and the lines that say
# why. `--list` prints the same plan without running it.
plan_self_tests <- function(root) {
  path <- file.path(root, MANIFEST)
  cmds <- manifest_self_tests(path)
  map <- read_self_test_map(path, cmds)
  changed <- changed_files()
  unknown <- inherits(changed, "cannot_tell")
  picked <- select_self_tests(map, changed)
  selected <- if (opt_release || unknown) {
    rep(TRUE, length(cmds))
  } else {
    unname(picked)
  }
  notes <- c(attr(map, "note"), sprintf(
    "[gate-self-tests] %s maps to no paths filter in %s, so it is selected",
    attr(picked, "unmappable"), MANIFEST
  ))
  summary <- sprintf(
    "[gate-self-tests] %d/%d %s", sum(selected), length(cmds),
    if (unknown) {
      sprintf("-- %s, so running every one", attr(changed, "reason"))
    } else if (opt_release) {
      "-- --release runs every one"
    } else {
      "-- the diff touches their manifest paths filters"
    }
  )
  list(cmds = cmds, selected = selected, notes = notes, summary = summary)
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
  cmds <- manifest_gates(file.path(root, MANIFEST))
  cat(sprintf("[gates] %d step(s) derived from %s\n", length(cmds), MANIFEST))
  lapply(cmds, function(cmd) {
    parts <- strsplit(cmd, "\\s+")[[1]]
    run_step(cmd, "Rscript", parts)
  })
}

stage_self_tests <- function(root) {
  plan <- plan_self_tests(root)
  cat(paste0(c(plan$notes, plan$summary), "\n"), sep = "")
  lapply(plan$cmds[plan$selected], function(cmd) {
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

# Spelling (SEOR-mtbzfroz). `R CMD check` spell-checks DESCRIPTION only when an
# English aspell/hunspell dictionary is installed, and on a machine without one
# it skips the check without a word -- so a typo first surfaced as win-builder's
# NOTE, after submission. spelling::spell_check_package() bundles its own
# dictionaries and also covers man/, vignettes and NEWS.md. Words it does not
# know but that are real go in inst/WORDLIST; a typo gets fixed at its source.
stage_spelling <- function() {
  cat("[spelling] spelling::spell_check_package()\n")
  code <- paste(
    "bad <- spelling::spell_check_package()",
    "if (nrow(bad)) { print(bad); quit(status = 1) }",
    "cat('0 misspelled words\n')",
    sep = "; "
  )
  list(run_step("spelling::spell_check_package()", "Rscript",
                c("-e", shQuote(code))))
}

# Generated-docs drift (SEOR-nwfmerhu). man/ and NAMESPACE are roxygen output,
# and a stale .Rd is still valid .Rd, so lint and R CMD check both pass it: the
# logo sweep left man/rurl-package.Rd stale and nothing here noticed (fixed by
# hand in rurl !176). scripts/check-docs-drift.R regenerates and diffs; read its
# header for why it uses roxygen's default loader.
#
# IT JUDGES THE COMMIT BEING PUSHED, not the working tree, unlike the stages
# around it. What reaches the remote is the commit; a man/ fix that sits
# uncommitted on disk does not, so letting it pass would push stale docs.
# pre-commit exports the pushed commit as $PRE_COMMIT_TO_REF for a pre-push
# hook, and tools/verify-on-push.sh hands its environment straight through.
# With several refs in one push, pre-commit names only the first one it would
# check. A hand run, or a push pre-commit treats as --all-files (a history
# with no commit on the remote), sets nothing, and HEAD is checked instead. In
# CI the checked-out commit is HEAD, but the r-base `check` job skips this
# stage (VERIFY_SKIP_DOCS=docs-drift-job) because the pinned roxygen2 is not
# a Debian binary; its own `docs-drift` job runs the script there. Only that
# exact value skips: any other one, say a value left over in a developer's
# shell, is reported and ignored, so it cannot drop the stage from a push
# that still reads PASS.
#
# It runs on a THROWAWAY EXPORT of that commit (`git archive`), never in
# `root`: on drift the script rewrites man/ and NAMESPACE where it runs, and
# `root` is what the check stage below builds from and what a push must not
# dirty. The export touches no index, ref or stash, so it needs no committer
# identity and cannot race another git process for index.lock. on.exit()
# removes it on every path out of this function, failure included.
stage_docs <- function(root) {
  skip <- Sys.getenv("VERIFY_SKIP_DOCS")
  if (identical(skip, "docs-drift-job")) {
    cat(paste0(
      "[docs] SKIPPED: VERIFY_SKIP_DOCS=docs-drift-job. Nothing here checks ",
      "man/ or NAMESPACE; CI's docs-drift job runs scripts/check-docs-drift.R\n"
    ))
    return(list())
  }
  if (nzchar(skip)) {
    cat(sprintf(paste0(
      "[docs] VERIFY_SKIP_DOCS=%s ignored: only CI's `docs-drift-job` ",
      "value skips this stage\n"
    ), skip))
  }
  ref <- Sys.getenv("PRE_COMMIT_TO_REF")
  if (!nzchar(ref)) {
    ref <- "HEAD"
  }
  cat(sprintf("[docs] scripts/check-docs-drift.R on an export of %s\n",
              if (identical(ref, "HEAD")) "HEAD" else
                paste("the pushed commit", substr(ref, 1L, 12L))))
  work <- tempfile("docs-drift-")
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  export <- file.path(work, "tree")
  dir.create(export, recursive = TRUE)
  tarball <- file.path(work, "tree.tar")
  # `^{commit}` makes a ref that names no commit fail here, by name, rather
  # than as an empty export the drift check would then misread.
  exported <- run_step(
    sprintf("git archive %s (docs export)", substr(ref, 1L, 12L)), "sh",
    c("-c", shQuote(sprintf(
      "git archive --format=tar -o %s %s && tar -xf %s -C %s",
      shQuote(tarball), shQuote(paste0(ref, "^{commit}")), shQuote(tarball),
      shQuote(export)
    )))
  )
  if (!exported$ok) {
    return(list(exported))
  }
  list(exported, run_step(
    "check-docs-drift.R (man/, NAMESPACE vs roxygen)", "Rscript",
    c(shQuote(file.path(root, "scripts", "check-docs-drift.R")),
      shQuote(export))
  ))
}

# The load-bearing stage, and the one no `devtools::test()` can stand in for.
# `R CMD check` must run on a BUILT TARBALL: building is what reads `Collate:`,
# and checking the tarball is what runs the tests against an INSTALLED package,
# where the file layout differs from the source tree. Both defects that got
# through were invisible to any instrument that skipped one of those two steps.
#
# THROUGH rcmdcheck, failing on a WARNING (error_on; the fleet standard, seor
# design/fleet-standard.md, "CI on every push to main"). rcmdcheck builds the
# tarball and checks it, so the built-tarball property above holds. It stops
# on a WARNING itself, and the guard after it fails on R CMD check's own exit
# status, because rcmdcheck reads a check that halted partway as 0/0/0 and
# returns normally (SEOR-maavnxdm). The 00check.log scan below stays as the
# printed summary and as a second WARNING tripwire.
stage_check <- function(root) {
  cat("[check] rcmdcheck: R CMD build + R CMD check --as-cran",
      "(on the tarball, failing on a WARNING)\n")
  # NOT under tempfile(): R deletes its session tempdir on exit, which would
  # take 00check.log with it -- so the one run you actually want to read, the
  # one that flagged something, is the one whose evidence is already gone.
  dir <- file.path(root, "_scratch", "verify-check")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  code <- paste(
    sprintf(paste(
      "res <- rcmdcheck::rcmdcheck(%s, args = c('--no-manual', '--as-cran'),",
      "build_args = '--no-manual', error_on = 'warning', check_dir = %s)"
    ), deparse(root), deparse(dir)),
    paste(
      "if (!identical(as.integer(res$status), 0L)) stop('R CMD check exited",
      "with status ', res$status, '; the run did not complete.', call. = FALSE)"
    ),
    sep = "; "
  )
  # _R_CHECK_SYSTEM_CLOCK_: a network-restricted machine cannot reach the time
  # server, and the resulting "unable to verify current time" NOTE is about the
  # sandbox, not the package. Set it for a local `R CMD check` too if you hit
  # that NOTE off-network; it is not needed on a machine with normal access.
  chk <- run_step("R CMD check --as-cran", "Rscript", c("-e", shQuote(code)),
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
  list(chk)
}

# testthat's summary reporter heads its warning section with a rule of box
# characters -- BUT it degrades that rule to plain `=` when the locale cannot
# represent U+2550, which is exactly the locale this step forces. Both spellings
# have to match or the watch is vacuous in the only step that carries it.
# Written as an escape, not as the literal character: this file is source that
# gets parsed in whatever locale the runner happens to be in.
TESTTHAT_WARNINGS <- "^(=|\u2550){2} Warnings"

# The manifest's `Tests (LC_ALL=C)` cell. It is here rather than in the check
# stage because R CMD check runs in the ambient locale: a defect that only
# appears under a non-UTF-8 charset is invisible to every other stage, and this
# codebase has shipped that exact class of defect before (RURL-kmpnbvdl).
#
# It is also the step with a KNOWN warning-only signal: RURL-aajradge is a
# testthat WARN that reproduces under Linux/C and changes no exit status, so
# this step reports PASS and drops it. Hence the `watch` -- the summary reporter
# emits a warnings section only when there are warnings, so this prints nothing
# on a clean run and the whole section when there is one.
#
# It EXCLUDES the two third-party advisory audits, test-security.R (OSS Index)
# and test-osv.R (OSV) (SEOR-fftbjnpl). test_local() sets NOT_CRAN=true, so
# their skip_on_cran() does not fire here, and ~/.Renviron puts the OSS Index
# credentials in scope -- so both ran live on every push, and a new upstream
# advisory blocked an unrelated push on 2026-09-09. An advisory is a fact about
# the world, not about the tree being pushed. `filter` matches the context name
# (the file name minus `test-` and `.R`); testthat passes `invert` through to
# the same file filter. Nothing runs the audits automatically after this; run
# them deliberately with testthat::test_local(filter = "^(security|osv)$").
stage_locale <- function() {
  cat("[locale] test suite under LC_ALL=C\n")
  code <- paste(
    "stopifnot(identical(Sys.getlocale('LC_CTYPE'), 'C'))",
    paste(
      "testthat::test_local(reporter = 'summary', stop_on_failure = TRUE,",
      "filter = '^(security|osv)$', invert = TRUE)"
    ),
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

# Self-test selection (RURL-etafkksg). A self-test is selected when the diff
# touches a path in the paths filter that gates it in the manifest, not only
# its own script: tools/local-ci-plan.R --self-test holds the only checks on
# tools/local-ci.sh. The fixture is a manifest in miniature, parsed with the
# same reader as the real one, and the last two cases read the REAL manifest so
# that an edit breaking the mapping goes red here.
selection_cases <- function(case) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    return(list(case(
      "the `yaml` package is installed (the selection cases need it)", FALSE
    )))
  }
  fixture <- yaml::yaml.load(paste(c(
    "jobs:",
    "  changes:",
    "    steps:",
    "      - uses: dorny/paths-filter@v3",
    "        with:",
    "          filters: |",
    "            a_selftest:",
    "              - 'tools/a.R'",
    "              - 'tools/a-helper.sh'",
    "            g_selftest:",
    "              - 'tools/g/**'",
    "              - 'tools/*.cfg'",
    "  a:",
    "    if: needs.changes.outputs.a_selftest == 'true'",
    "    steps:",
    "      - run: Rscript tools/a.R --self-test",
    "  g:",
    "    if: needs.changes.outputs.other == 'true'",
    "    steps:",
    "      - if: needs.changes.outputs.g_selftest == 'true'",
    "        run: Rscript tools/g.R --self-test",
    "  orphan:",
    "    steps:",
    "      - run: Rscript tools/orphan.R --self-test",
    "  undeclared:",
    "    if: needs.changes.outputs.no_such_filter == 'true'",
    "    steps:",
    "      - run: Rscript tools/undeclared.R --self-test"
  ), collapse = "\n"))
  map <- self_test_filter_map(fixture)
  sel <- function(...) select_self_tests(map, c(...))
  a <- "tools/a.R --self-test"
  g <- "tools/g.R --self-test"
  orphans <- c("tools/orphan.R --self-test", "tools/undeclared.R --self-test")
  unrelated <- sel("R/x.R")

  real_ok <- file.exists(MANIFEST)
  real <- if (real_ok) self_test_filter_map(yaml::read_yaml(MANIFEST))
  real_cmds <- if (real_ok) manifest_self_tests(MANIFEST)
  covers <- function(cmd) {
    sub(" --self-test$", "", cmd) %in% real[[cmd]]
  }

  list(
    case(
      "a change to a non-script path in a self-test's filter selects it",
      isTRUE(sel("tools/a-helper.sh")[[a]])
    ),
    case(
      "an unrelated change selects neither mapped self-test",
      !unrelated[[a]] && !unrelated[[g]]
    ),
    case(
      "a self-test's own script selects it even if its filter omits it",
      isTRUE(sel("tools/g.R")[[g]])
    ),
    case(
      "the STEP's filter gates a self-test, not its job's",
      !isTRUE(sel("tools/a-helper.sh")[[g]]) &&
        isTRUE(sel("tools/x.cfg")[[g]])
    ),
    case(
      "a `**` filter entry matches across directories",
      isTRUE(sel("tools/g/deep/er/x.R")[[g]])
    ),
    case(
      "a `*` filter entry matches within one directory, not across",
      isTRUE(sel("tools/x.cfg")[[g]]) && !sel("tools/sub/x.cfg")[[g]]
    ),
    case(
      "an unmappable self-test is SELECTED, and named, on any diff",
      all(unrelated[orphans]) &&
        setequal(attr(unrelated, "unmappable"), orphans)
    ),
    case(
      "REAL manifest: every self-test maps to a filter covering its script",
      real_ok && all(real_cmds %in% names(real)) &&
        all(vapply(real_cmds, covers, logical(1)))
    ),
    case(
      "REAL manifest: a tools/local-ci.sh change selects its planner's test",
      real_ok && isTRUE(select_self_tests(real, "tools/local-ci.sh")[[
        "tools/local-ci-plan.R --self-test"
      ]])
    )
  )
}

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
  cases <- c(cases, selection_cases(case))

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
  cat("tools/verify.R --self-test: step output visibility (RURL-pbihchti)",
      "and self-test selection (RURL-etafkksg)\n")
  self_test()
}

# ---- main -------------------------------------------------------------------

root <- repo_root()
plan <- c("gates", "selftests")
if (!opt_gates) {
  plan <- c(plan, "lint", "spelling")
}
if (!opt_gates && !opt_fast) {
  plan <- c(plan, "docs", "check", "locale")
}
if (opt_release) {
  plan <- c(plan, "release")
}

if (opt_list) {
  cat("stage plan:", paste(plan, collapse = " -> "), "\n")
  cat("derived gate steps:\n")
  cat(paste0("  ", manifest_gates(file.path(root, MANIFEST))), sep = "\n")
  st <- plan_self_tests(root)
  cat("\nconditional gate self-tests (* = selected for this diff):\n")
  cat(paste0(ifelse(st$selected, "* ", "  "), st$cmds), sep = "\n")
  cat(paste0(c(st$notes, st$summary), "\n"), sep = "")
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
    spelling = stage_spelling(),
    docs = stage_docs(root),
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
