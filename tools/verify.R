#!/usr/bin/env Rscript

# The manifest-gate runner (RURL-mvsxmyww).
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
# WHERE IT RUNS NOW. This script was the whole pre-push gate: the manifest
# gates, lint, spelling, docs drift, `R CMD check --as-cran` and the test
# suite under LC_ALL=C. Since the move to fleet-ci's tiers (seor ADR 0012) the
# merge gate is fleet-ci's local gate, `.fleet-ci/gate`, which runs in the
# fleet image from the pre-push hook and reads its steps from `.fleet-ci.yml`.
# That gate runs lint, spelling, docs drift and the check itself, and runs
# this script as one of its cheap gates (`Rscript tools/verify.R --gates`) for
# the part only rurl has: the gates `tools/verify-manifest.yml` declares and
# their self-tests. The locale cell became `tools/check-locale-tests.R`,
# another of those cheap gates: the whole suite under LC_ALL=C, with a
# positive assertion over the locale-relevant tests. (fleet-ci's `t2-locale`
# sets LC_ALL=C too, but its R CMD check runs `tests/testthat.R`, which
# selects en_US.UTF-8, so its tests do not run under C.)
#
# The gate runs it on an export of the pushed commit that has no `.git`, so
# self-test selection cannot diff against main there and runs every
# self-test (see "self-test selection" below).
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
# The fleet-ci gate runs this script, so it consumes the same manifest.
#
# GATE SELF-TESTS ARE A SEPARATE RISK CLASS. Their positive/negative fixtures
# prove the verifier, not the product, so routine runs select one only when the
# diff against main touches a path in the manifest's paths filter that gates it
# (the `changes` job's `filters:` block), or the self-test's own script. A
# self-test that maps to no filter is selected and named, never skipped (see
# "self-test selection" below). `--release` deliberately runs every self-test,
# and so does a run that cannot tell what changed, which is every run in the
# fleet-ci gate's export. The real tree scans run every time.
#
# WHAT IT DOES NOT COVER, stated so nobody reads a green run as more than it is:
# everything but the manifest gates and their self-tests. Lint, spelling,
# docs drift, `R CMD check` and the suite under LC_ALL=C are the fleet-ci
# gate's (`.fleet-ci.yml`); the R-version matrix, the floor, the locale leg,
# coverage and the OSV and OSS Index audits are its T2 jobs
# (`.gitlab-ci.yml`, design/ci.md). `--release` adds the C7 curl clean room,
# which needs its own R CMD check against a poisoned library and only matters
# at release.
#
# Stages are independent: a failure does not stop the run, because knowing all
# of what is broken beats knowing the first thing.
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
# `aajradge` stayed hidden.
#
# Usage:
#   Rscript tools/verify.R --gates    # gates + relevant self-tests (what the
#                                     # fleet-ci gate runs; also the default)
#   Rscript tools/verify.R --release  # the above, plus the curl clean room
#   Rscript tools/verify.R --verbose  # print every step's log, passing included
#   Rscript tools/verify.R --list     # print the stage plan, and the
#                                     # self-tests this diff selects, and exit
#   Rscript tools/verify.R --self-test  # prove the two instruments above,
#                                       # and self-test selection
# None of these is the delivery bar: that is `.fleet-ci/gate auto`, which runs
# this script among its cheap gates.
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
# Any other flag is an error, not ignored: the former runner's --fast no
# longer exists, and a run that silently dropped it would read as the gate it
# used to run. A bare run says what it covers, once.
known <- c("--gates", "--release", "--list", "--verbose", "--self-test")
unknown <- setdiff(args, known)
if (length(unknown)) {
  stop("tools/verify.R: unknown or retired flag(s) ",
       paste(unknown, collapse = " "), "; it runs the manifest gates only (",
       paste(known, collapse = " "), "). The full gate is ",
       "`.fleet-ci/gate auto`.", call. = FALSE)
}
if (!length(args)) {
  cat("tools/verify.R: the manifest gates only; the full gate is",
      "`.fleet-ci/gate auto`\n")
}
opt_release <- "--release" %in% args
opt_list <- "--list" %in% args
opt_verbose <- "--verbose" %in% args
opt_self_test <- "--self-test" %in% args

# ---- helpers ----------------------------------------------------------------

repo_root <- function() {
  # The package root, found by its DESCRIPTION and the manifest. Not by
  # `.git`: the fleet-ci gate runs this on an export of the pushed commit,
  # which has none.
  if (!file.exists("DESCRIPTION") || !file.exists(MANIFEST)) {
    stop("run this from the package root", call. = FALSE)
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
# checked out a shallow, single-ref clone: no local `main`, no `origin/main`,
# and `git diff main...HEAD` is a fatal bad revision. That took down the whole
# gates job after all 14 gates had passed. The fleet-ci gate's export has no
# repository at all, which lands here too.
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
  uncommitted <- git_lines(c("diff", "--name-only", "--no-renames", "HEAD"))
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
  committed <- git_lines(c("diff", "--name-only", "--no-renames",
                           paste0(base, "...HEAD")))
  unique(c(committed, uncommitted))
}

# ---- self-test selection (RURL-etafkksg) -----------------------------------
#
# A self-test runs when the diff touches a path in the paths filter that gates
# it in the manifest, not only its own script: the `changes` job declares one
# filter per self-test, and a self-test can be the only check on a file other
# than its script (tools/verify.R --self-test is the only one on the
# manifest's self-test mapping). The chain is the step running
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
# matches no directory at all, as picomatch has it. Everything else is literal,
# so a glob using any other picomatch syntax (`?`, `[...]`, `{a,b}`, `!`, or a
# `**` that is not a whole path segment) would match nothing here while
# matching in CI; `unsupported_glob()` flags those, and the self-test they gate
# is treated as unmappable rather than silently skipped.
unsupported_glob <- function(glob) {
  grepl("[][{}?!]", glob) ||
    grepl("[^/]\\*\\*|\\*\\*[^/]", glob)
}

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
  unreadable <- vapply(map, function(globs) {
    any(vapply(globs, unsupported_glob, logical(1)))
  }, logical(1))
  unmappable <- names(map)[lengths(map) == 0L | unreadable]
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
  # A manifest-wide note (no yaml, or an unparsable manifest) already says why
  # nothing maps; one line per self-test on top of it would bury the cause.
  notes <- if (length(attr(map, "note"))) attr(map, "note") else sprintf(
    paste("[gate-self-tests] %s has no paths filter in %s that this runner",
          "can read, so it is selected"),
    attr(picked, "unmappable"), MANIFEST
  )
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

stage_release <- function() {
  cat("[release] curl zero-reference clean room (C7)\n")
  list(run_step("curl-zero-gate.R (full, incl. C7)", "Rscript",
                "tools/curl-zero-gate.R"))
}

# ---- self-test --------------------------------------------------------------

# Self-test selection (RURL-etafkksg). A self-test is selected when the diff
# touches a path in the paths filter that gates it in the manifest, not only
# its own script: this runner's own self-test checks the manifest's mapping,
# so a manifest edit must select it. The fixture is a manifest in miniature,
# parsed with the same reader as the real one, and the last two cases read the
# REAL manifest so that an edit breaking the mapping goes red here.
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
    "            u_selftest:",
    "              - 'tools/{u,v}.R'",
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
    "      - run: Rscript tools/undeclared.R --self-test",
    "  u:",
    "    if: needs.changes.outputs.u_selftest == 'true'",
    "    steps:",
    "      - run: Rscript tools/u.R --self-test"
  ), collapse = "\n"))
  map <- self_test_filter_map(fixture)
  sel <- function(...) select_self_tests(map, c(...))
  a <- "tools/a.R --self-test"
  g <- "tools/g.R --self-test"
  u <- "tools/u.R --self-test"
  orphans <- c("tools/orphan.R --self-test", "tools/undeclared.R --self-test",
               u)
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
      "a filter glob this runner cannot read makes its self-test unmappable",
      isTRUE(unrelated[[u]]) && u %in% attr(unrelated, "unmappable") &&
        unsupported_glob("tools/?.R") && unsupported_glob("tools/**.R") &&
        !unsupported_glob("tools/g/**") && !unsupported_glob("**/x.R")
    ),
    case(
      "REAL manifest: every self-test maps to a filter covering its script",
      real_ok && all(real_cmds %in% names(real)) &&
        all(vapply(real_cmds, covers, logical(1)))
    ),
    case(
      "REAL manifest: a manifest change selects this runner's own test",
      real_ok && isTRUE(select_self_tests(real, MANIFEST)[[
        "tools/verify.R --self-test"
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
    "^== Warnings"
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

cat("rurl manifest gates --", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("stages:", paste(plan, collapse = " -> "), "\n\n")

results <- list()
for (st in plan) {
  results <- c(results, switch(
    st,
    gates = stage_gates(root),
    selftests = stage_self_tests(root),
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
cat("VERDICT: PASS (the manifest gates only -- see the header for what is not",
    "covered)\n")
