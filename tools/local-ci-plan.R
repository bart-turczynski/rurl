#!/usr/bin/env Rscript

# Job planner for tools/local-ci.sh.
#
# WHY THIS IS DERIVED RATHER THAN HAND-WRITTEN. `.gitlab-ci.yml` already refuses
# to transcribe the gate list, for a stated reason: a hand-maintained copy is
# silently incomplete from the moment someone adds a gate. A local runner that
# re-spelled the job list, the image, the apt lines and the `rules:` in shell
# would reintroduce exactly that drift one layer up -- and worse, it would drift
# INVISIBLY, because a local runner nobody compares against a real pipeline has
# nothing to disagree with. So this reads the CI config and reports what GitLab
# would have done with it.
#
# UNKNOWN CONSTRUCTS ARE AN ERROR, NEVER A SKIP. The failure mode that matters
# for a selector is going vacuous: reporting "0 jobs" because nothing was
# understood reads identically to "0 jobs apply". Every rule form this evaluator
# does not implement -- regex operators, parentheses, `changes:`, `exists:` --
# stops the run and names itself, so adding one to CI produces a loud local
# failure instead of a quietly narrower local gate.
#
# `workflow:` IS DELIBERATELY IGNORED. It governs whether GitLab CREATES a
# pipeline, which is a question about the forge's scheduler and its compute
# budget, not about whether the checks hold. Running this script by hand IS the
# deliberate trigger, so honoring a `workflow: rules:` that exists to stop
# automatic pipelines would make the local runner refuse to run precisely when
# it is the only gate left. The presence of the block is reported instead.
#
# Usage:
#   Rscript tools/local-ci-plan.R --jobs [context]   # job names, one per line
#   Rscript tools/local-ci-plan.R --list [context]   # human-readable plan
#   Rscript tools/local-ci-plan.R --script <job>     # flattened script lines
#   Rscript tools/local-ci-plan.R --image <job>      # resolved image
#   Rscript tools/local-ci-plan.R --self-test        # fixture cases, no config
#
# Context flags (all optional, defaulting to an empty value):
#   --branch <name>   $CI_COMMIT_BRANCH      --tag <name>  $CI_COMMIT_TAG
#   --source <name>   $CI_PIPELINE_SOURCE    --all         ignore rules entirely
#
# Base R plus `yaml`, which the gate stage already installs.

CONFIG <- ".gitlab-ci.yml"
DEFAULT_BRANCH <- "main"

# Top-level keys that configure the pipeline rather than declaring a job. Keys
# beginning with "." are YAML anchor holders (`.gate_deps`) and are handled
# separately -- GitLab treats them as hidden regardless of their content.
#
# `pages` is NOT on this list. It is a job name with a special meaning to
# GitLab (the job whose `public/` artifact becomes the Pages site), not a
# pipeline keyword; listing it here made the planner drop the `pages` job in
# `.gitlab-ci.yml` without a word -- the vacuous-selector failure the header
# above names (RURL-vkltgopc).
RESERVED <- c(
  "stages", "default", "workflow", "include", "variables", "image",
  "before_script", "after_script", "cache", "services"
)

args <- commandArgs(trailingOnly = TRUE)

flag_value <- function(name, default = "") {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1L]]
}

die <- function(...) stop(paste0(...), call. = FALSE)

# ---- rule evaluation --------------------------------------------------------

resolve_operand <- function(tok, vars) {
  tok <- trimws(tok)
  if (grepl('^".*"$', tok) || grepl("^'.*'$", tok)) {
    return(substr(tok, 2L, nchar(tok) - 1L))
  }
  name <- sub("^\\$\\{?([A-Za-z_][A-Za-z0-9_]*)\\}?$", "\\1", tok)
  if (identical(name, tok)) {
    die("unsupported operand in a CI rule: ", tok)
  }
  val <- vars[[name]]
  if (is.null(val)) "" else as.character(val)
}

atom_matches <- function(atom, vars) {
  atom <- trimws(atom)
  if (grepl("=~|!~", atom)) {
    die("regex rule operators are not implemented by the local planner: ", atom)
  }
  m <- regmatches(atom, regexec("^(.*?)\\s*(==|!=)\\s*(.*)$", atom))[[1]]
  if (length(m) == 4L) {
    lhs <- resolve_operand(m[[2]], vars)
    rhs <- resolve_operand(m[[4]], vars)
    return(if (identical(m[[3]], "==")) identical(lhs, rhs) else
      !identical(lhs, rhs))
  }
  # A bare `$VAR` is true when the variable is defined and non-empty.
  nzchar(resolve_operand(atom, vars))
}

expr_matches <- function(expr, vars) {
  if (grepl("[()]", expr)) {
    die("parenthesized CI rule expressions are not implemented: ", expr)
  }
  ors <- strsplit(expr, "||", fixed = TRUE)[[1]]
  any(vapply(ors, function(clause) {
    ands <- strsplit(clause, "&&", fixed = TRUE)[[1]]
    all(vapply(ands, atom_matches, logical(1), vars = vars))
  }, logical(1)))
}

# Returns TRUE/FALSE plus the reason, so --list can say WHY a job was skipped
# rather than leaving the operator to re-read the YAML and guess.
# A job that ONLY a pipeline schedule can start: every rule that admits it
# requires `$CI_PIPELINE_SOURCE == "schedule"`. Those are the dependency
# audits (osv-audit, security-audit; SEOR-fftbjnpl), which need the network
# and forge-held credentials and answer a question about the world rather than
# the tree -- so `--all`, which exists to run the rationed release-time jobs
# after a merge, leaves them out instead of letting a new upstream advisory (or
# absent credentials) turn a post-merge run red. Derived from the rules, not a
# name list, so a new schedule-only job is covered the day it lands.
schedule_only <- function(job) {
  rules <- job[["rules"]]
  if (is.null(rules)) {
    return(FALSE)
  }
  admitting <- Filter(function(rule) {
    is.list(rule) && !identical(rule[["when"]], "never")
  }, rules)
  length(admitting) > 0L && all(vapply(admitting, function(rule) {
    cond <- rule[["if"]]
    !is.null(cond) &&
      grepl('\\$CI_PIPELINE_SOURCE\\s*==\\s*"schedule"', cond)
  }, logical(1)))
}

job_verdict <- function(job, vars, ignore_rules) {
  if (ignore_rules && schedule_only(job)) {
    return(list(run = FALSE, why = "--all: schedule-only audit, left out"))
  }
  if (ignore_rules) {
    return(list(run = TRUE, why = "--all: rules ignored"))
  }
  rules <- job[["rules"]]
  if (is.null(rules)) {
    return(list(run = TRUE, why = "no rules: always runs"))
  }
  for (rule in rules) {
    if (!is.list(rule)) {
      die("only mapping-form `rules:` entries are implemented, got: ", rule)
    }
    unsupported <- intersect(names(rule), c("changes", "exists", "allow_failure"))
    if (length(unsupported)) {
      die("unsupported rule key(s): ", paste(unsupported, collapse = ", "))
    }
    cond <- rule[["if"]]
    if (is.null(cond) || expr_matches(cond, vars)) {
      when <- rule[["when"]]
      label <- if (is.null(cond)) "unconditional rule" else cond
      if (!is.null(when) && identical(when, "never")) {
        return(list(run = FALSE, why = paste0("matched `", label, "` -> never")))
      }
      return(list(run = TRUE, why = paste0("matched `", label, "`")))
    }
  }
  list(run = FALSE, why = "no rule matched")
}

# ---- scripts -----------------------------------------------------------------

# YAML aliases arrive as nested lists, so a `script:` built from an anchor is a
# list-of-lists. Flattening is what turns it back into the line sequence the
# runner executes.
job_script <- function(job) {
  lines <- as.character(unlist(c(job[["before_script"]], job[["script"]]),
                               use.names = FALSE))
  if (any(grepl("\n", lines, fixed = TRUE))) {
    die("a script entry spans multiple lines; the runner emits one per line")
  }
  lines
}

# What `--script` prints: the text tools/local-ci.sh writes after its own
# `set -ex` line and hands to `bash`. One entry per line, then a blank line.
render_script <- function(entries) {
  paste0(c(entries, ""), "\n", collapse = "")
}

# One job's script block in the `--list` plan.
render_plan_script <- function(entries) {
  paste0(c(sprintf("    $ %s", entries), ""), "\n", collapse = "")
}

# ---- self-test ---------------------------------------------------------------

# Fixtures are inline CI configs, parsed with the same `yaml` reader the real
# run uses; no file, no git, no docker. The shell cases run the rendered script
# under `bash` behind the same `set -ex` preamble tools/local-ci.sh writes, so
# they prove what the job shell does with it, not only what the text is.
self_test <- function() {
  st <- new.env()
  st$pass <- 0L
  st$fail <- character(0)
  expect <- function(what, ok) {
    if (isTRUE(ok)) {
      st$pass <- st$pass + 1L
    } else {
      st$fail <- c(st$fail, what)
    }
  }
  fixture <- function(text) yaml::yaml.load(text)
  run_bash <- function(script) {
    path <- tempfile(fileext = ".sh")
    on.exit(unlink(path))
    writeLines(paste0("set -ex\n", script), path, sep = "")
    out <- suppressWarnings(system2("bash", path, stdout = TRUE,
                                    stderr = TRUE))
    status <- attr(out, "status")
    list(status = if (is.null(status)) 0L else status, out = out)
  }

  # Single-line entries, one of them arriving through an anchor alias, which
  # yaml hands over as a nested list.
  single <- fixture(paste(
    ".deps: &deps",
    "  - 'apt-get update -qq'",
    "  - 'apt-get install -y r-cran-yaml'",
    "job:",
    "  before_script:",
    "    - 'echo before'",
    "  script:",
    "    - *deps",
    "    - 'Rscript -e ''cat(1)'''",
    sep = "\n"
  ))
  entries <- job_script(single$job)
  expect("single-line: anchor flattened, before_script first",
         identical(entries, c("echo before", "apt-get update -qq",
                              "apt-get install -y r-cran-yaml",
                              "Rscript -e 'cat(1)'")))
  expect("single-line: --script text is one line per entry plus a blank",
         identical(render_script(entries), paste0(
           "echo before\napt-get update -qq\n",
           "apt-get install -y r-cran-yaml\nRscript -e 'cat(1)'\n\n")))
  expect("single-line: --list text prefixes every entry",
         identical(render_plan_script(entries), paste0(
           "    $ echo before\n    $ apt-get update -qq\n",
           "    $ apt-get install -y r-cran-yaml\n",
           "    $ Rscript -e 'cat(1)'\n\n")))
  expect("empty script renders as the bare separator",
         identical(render_script(character(0)), "\n"))

  # errexit: a failing single-line entry stops the job before the next one.
  res <- run_bash(render_script(c("echo one", "false", "echo reached")))
  expect("single-line: a failing entry fails the job",
         res$status != 0L && !any(res$out == "reached"))
  res <- run_bash(render_script(c("echo one", "echo two")))
  expect("single-line: passing entries pass the job",
         res$status == 0L && any(res$out == "two"))

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("local-ci-plan self-test: FAIL", call. = FALSE)
  }
  cat("VERDICT PASS\n")
  invisible(TRUE)
}

if ("--self-test" %in% args) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    die("the `yaml` package is required for the self-test")
  }
  self_test()
  quit(save = "no", status = 0L)
}

# ---- config ------------------------------------------------------------------

if (!file.exists(CONFIG)) {
  die("run this from the repository root -- cannot read ", CONFIG)
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  die("the `yaml` package is required to read ", CONFIG)
}

cfg <- yaml::read_yaml(CONFIG)

job_names <- Filter(function(nm) {
  !startsWith(nm, ".") && !(nm %in% RESERVED) &&
    is.list(cfg[[nm]]) && !is.null(cfg[[nm]][["script"]])
}, names(cfg))

job_or_die <- function(nm) {
  if (!(nm %in% job_names)) {
    die("no job named `", nm, "` in ", CONFIG, " (have: ",
        paste(job_names, collapse = ", "), ")")
  }
  cfg[[nm]]
}

job_image <- function(job) {
  img <- job[["image"]]
  if (is.null(img)) img <- cfg[["default"]][["image"]]
  if (is.null(img)) die("no image for this job and no `default: image:`")
  as.character(img)
}

vars <- list(
  CI_DEFAULT_BRANCH = DEFAULT_BRANCH,
  CI_COMMIT_BRANCH = flag_value("--branch"),
  CI_COMMIT_TAG = flag_value("--tag"),
  CI_PIPELINE_SOURCE = flag_value("--source", "push")
)
ignore_rules <- "--all" %in% args

selected <- Filter(
  function(nm) job_verdict(cfg[[nm]], vars, ignore_rules)$run,
  job_names
)

# ---- modes -------------------------------------------------------------------

if ("--script" %in% args) {
  cat(render_script(job_script(job_or_die(flag_value("--script")))))
} else if ("--image" %in% args) {
  cat(job_image(job_or_die(flag_value("--image"))), "\n", sep = "")
} else if ("--list" %in% args) {
  cat("config: ", CONFIG, "\n", sep = "")
  cat("context: ",
      paste(sprintf("%s=%s", names(vars), unlist(vars)), collapse = "  "),
      "\n", sep = "")
  if (!is.null(cfg[["workflow"]])) {
    cat("note: a `workflow:` block is present and deliberately IGNORED --",
        "it gates pipeline CREATION on the forge, not whether checks hold\n")
  }
  cat("\n")
  for (nm in job_names) {
    v <- job_verdict(cfg[[nm]], vars, ignore_rules)
    cat(sprintf("  %-4s %-10s %s\n", if (v$run) "RUN" else "skip", nm, v$why))
  }
  cat("\n")
  for (nm in selected) {
    cat(sprintf("[%s] image=%s\n", nm, job_image(cfg[[nm]])))
    cat(render_plan_script(job_script(cfg[[nm]])))
  }
} else {
  cat(selected, sep = "\n")
  if (length(selected)) cat("\n")
}
