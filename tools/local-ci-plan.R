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
#   Rscript tools/local-ci-plan.R --not-judged [context]  # secret-skipped jobs
#   Rscript tools/local-ci-plan.R --list [context]   # human-readable plan
#   Rscript tools/local-ci-plan.R --script <job>     # flattened script lines
#   Rscript tools/local-ci-plan.R --image <job>      # resolved image
#   Rscript tools/local-ci-plan.R --self-test        # fixture cases, no config
#
# Context flags (all optional, defaulting to an empty value):
#   --branch <name>   $CI_COMMIT_BRANCH      --tag <name>  $CI_COMMIT_TAG
#   --source <name>   $CI_PIPELINE_SOURCE    --all         ignore rules entirely
#
# Exit status: 0, or 3 from `--jobs` when every job that applies is not judged
# (see SECRET_JOBS); 2 when a SECRET_JOBS entry is malformed or names no job;
# 1 for any other error.
#
# Base R plus `yaml`, which the gate stage already installs.

CONFIG <- ".gitlab-ci.yml"
DEFAULT_BRANCH <- "main"

# A JOB THAT NEEDS A CI SECRET IS NOT JUDGED WHEN THE SECRET IS UNSET HERE
# (RURL-hlcpduoq, moved here from tools/local-ci.sh by RURL-bsfwpfil so that
# `--list` and a real run say the same thing). Such a secret lives only as a
# GitLab CI/CD variable, so locally the job fails on every run whatever the
# commit holds, and a verdict that is always red says nothing. Each entry is
# `job = "VARIABLE"`; a job may have several. When one of a job's variables is
# not set (exported, non-empty) in this process's environment -- which, like a
# real job's, holds only what the caller exported -- a job its rules select
# becomes NOT JUDGED instead of RUN, and the verdict names the variable. It
# stays out of the judgment, which the other jobs make (the rule seor's
# `scripts/check-fleet-standard.py` follows: not judged, not a gap). When all
# are set, the job runs exactly as any other job and can still fail.
#
# A declared secret ONLY decides run or not judged: it is never forwarded into
# the container. Forwarding `FOSSA_API_KEY` would make a local run on any ref
# upload to the production FOSSA project, past CI's default-branch-only rule.
#
# AN ENTRY THAT NAMES NO JOB IS AN ERROR (exit 2), not a no-op: rename `fossa:`
# in the CI config and a silent entry would match nothing, and the job would go
# back to reading red on every run. The next secret-gated job needs one entry
# here, nothing else.
SECRET_JOBS <- c(fossa = "FOSSA_API_KEY")

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

# ---- CI secrets ----------------------------------------------------------------

# Set but empty counts as unset, as `$VAR` does in a CI rule.
local_env <- function(var) Sys.getenv(var, unset = "")

# Each malformed or dangling SECRET_JOBS entry, as a message; none is valid.
secret_problems <- function(secrets, job_names) {
  nms <- names(secrets)
  if (is.null(nms)) nms <- rep("", length(secrets))
  bad <- !nzchar(nms) | !grepl("^[A-Za-z_][A-Za-z0-9_]*$", secrets)
  dup <- !bad & duplicated(paste(nms, secrets))
  c(
    sprintf('`%s = "%s"` is not job = "VARIABLE"', nms[bad], secrets[bad]),
    sprintf('`%s = "%s"` is declared twice', nms[dup], secrets[dup]),
    sprintf("`%s` names no job in %s", setdiff(unique(nms[!bad]), job_names),
            CONFIG)
  )
}

# The rule verdict first, then the secret gate: a job its rules leave out is
# skipped for that reason, and only a job they select can be not judged.
# `unset` names the job's missing variables, empty unless it is not judged.
plan_verdict <- function(nm, job, vars, ignore_rules, secrets,
                         getenv = local_env) {
  v <- job_verdict(job, vars, ignore_rules)
  v$unset <- character(0)
  if (!v$run) {
    return(v)
  }
  declared <- unname(secrets[names(secrets) == nm])
  unset <- declared[!nzchar(vapply(declared, getenv, character(1)))]
  if (length(unset)) {
    v <- list(run = FALSE, unset = unset, why = paste0(
      "NOT JUDGED, CI secret unset locally: ", paste(unset, collapse = " "),
      " (", v$why, ")"))
  }
  v
}

# What the run as a whole does with a set of verdicts: the jobs to run, the
# not-judged summary the VERDICT line carries, and the status `--jobs` exits
# with. 3 means jobs applied and none can be judged, so a run would end NONE;
# no job applying at all stays 0.
plan_outcome <- function(verdicts) {
  run <- as.character(names(verdicts)[vapply(verdicts, `[[`, logical(1),
                                               "run")])
  unjudged <- Filter(function(v) length(v$unset) > 0L, verdicts)
  not_judged <- paste(sprintf("%s (%s unset)", names(unjudged),
                              vapply(unjudged, function(v) {
                                paste(v$unset, collapse = " ")
                              }, character(1))),
                      collapse = ", ")
  list(run = run, not_judged = not_judged,
       status = if (!length(run) && length(unjudged)) 3L else 0L)
}

# ---- scripts -----------------------------------------------------------------

# YAML aliases arrive as nested lists, so a `script:` built from an anchor is a
# list-of-lists. Flattening is what turns it back into the entry sequence the
# runner executes.
#
# A BLOCK SCALAR (`- |`) IS ONE ENTRY, NOT SEVERAL (RURL-gysfdtcd). It arrives
# as a single string holding newlines, plus the one trailing newline YAML's
# default clip chomping adds. GitLab's runner does not split it: it writes the
# block into the job's shell script as it stands, so the block runs as one unit
# under the job's errexit, and a failing command inside it fails the job. This
# emits it the same way, verbatim, dropping only that trailing newline so the
# entry ends where a single-line one does. The `set -ex` tools/local-ci.sh
# writes first traces each command in the block as it runs, as it does for a
# single-line entry. This used to refuse any entry with a newline in it, which
# stopped the whole plan at the `pages` job.
job_script <- function(job) {
  entries <- as.character(unlist(c(job[["before_script"]], job[["script"]]),
                                 use.names = FALSE))
  sub("\n$", "", entries)
}

# What `--script` prints: the text tools/local-ci.sh writes after its own
# `set -ex` line and hands to `bash`. One entry per line, then a blank line.
render_script <- function(entries) {
  paste0(c(entries, ""), "\n", collapse = "")
}

# One job's script block in the `--list` plan. A block entry keeps its lines
# together under a single `$`, continuation lines indented beneath it.
render_plan_script <- function(entries) {
  shown <- vapply(strsplit(entries, "\n", fixed = TRUE), function(lines) {
    if (!length(lines)) lines <- ""
    rest <- lines[-1L]
    rest[nzchar(rest)] <- paste0("      ", rest[nzchar(rest)])
    paste(c(paste0("    $ ", lines[[1L]]), rest), collapse = "\n")
  }, character(1))
  paste0(c(shown, ""), "\n", collapse = "")
}

# ---- self-test ---------------------------------------------------------------

# Fixtures are inline CI configs, parsed with the same `yaml` reader the real
# run uses; no repository config, no docker. The script-level cases write
# theirs to a temp folder and run this script there; the plan-from-ref cases
# commit theirs to a throwaway git repository and run tools/local-ci.sh
# --list there, and are skipped, visibly, without git. The shell cases run
# the rendered script
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

  # A block-scalar entry, the shape of the `pages` job's keep-list filter: it
  # stays one entry, verbatim, and runs as one shell unit between its
  # neighbors.
  multi <- fixture(paste(
    "job:",
    "  script:",
    "    - 'echo first'",
    "    - |",
    "      set -e",
    "      for f in a b; do",
    "        case \"$f\" in",
    "          a) echo \"got $f\" ;;",
    "          *) echo \"other $f\" ;;",
    "        esac",
    "      done",
    "    - 'echo last'",
    "stripped:",
    "  script:",
    "    - |-",
    "      echo one",
    "      echo two",
    "clipped:",
    "  script:",
    "    - |",
    "      echo one",
    "      echo two",
    sep = "\n"
  ))
  block <- paste(
    "set -e", "for f in a b; do", "  case \"$f\" in",
    "    a) echo \"got $f\" ;;", "    *) echo \"other $f\" ;;", "  esac",
    "done",
    sep = "\n"
  )
  entries <- job_script(multi$job)
  expect("multi-line: a block scalar stays ONE entry, verbatim",
         identical(entries, c("echo first", block, "echo last")))
  expect("multi-line: clip and strip chomping give the same entry",
         identical(job_script(multi$clipped), job_script(multi$stripped)) &&
           identical(job_script(multi$clipped), "echo one\necho two"))
  expect("multi-line: --script text carries the block intact, in order",
         identical(render_script(entries),
                   paste0("echo first\n", block, "\necho last\n\n")))
  expect("multi-line: --list keeps the block under one `$`",
         identical(render_plan_script(entries), paste0(
           "    $ echo first\n",
           "    $ set -e\n",
           "      for f in a b; do\n",
           "        case \"$f\" in\n",
           "          a) echo \"got $f\" ;;\n",
           "          *) echo \"other $f\" ;;\n",
           "        esac\n",
           "      done\n",
           "    $ echo last\n\n")))
  res <- run_bash(render_script(entries))
  expect("multi-line: the block runs as one unit between its neighbors",
         res$status == 0L &&
           identical(res$out[!startsWith(res$out, "+")],
                     c("first", "got a", "other b", "last")))
  failing <- job_script(fixture(paste(
    "job:",
    "  script:",
    "    - |",
    "      echo inside",
    "      false",
    "      echo after-false",
    "    - 'echo next-entry'",
    sep = "\n"
  ))$job)
  res <- run_bash(render_script(failing))
  expect("multi-line: a failing command inside the block fails the job",
         res$status != 0L && any(res$out == "inside") &&
           !any(res$out %in% c("after-false", "next-entry")))

  # The secret gate, with the environment injected: `env` stands in for the
  # caller's exported variables, so no case reads or sets a real one.
  gated <- fixture(paste(
    "always:",
    "  script: ['true']",
    "upload:",
    "  script: ['true']",
    "audit:",
    "  rules:",
    "    - if: $CI_PIPELINE_SOURCE == \"schedule\"",
    "  script: ['true']",
    sep = "\n"
  ))
  secrets <- c(upload = "UPLOAD_KEY", upload = "UPLOAD_ORG",
               audit = "AUDIT_TOKEN")
  verdicts <- function(env, jobs = names(gated), all = FALSE) {
    getenv <- function(var) {
      val <- env[var]
      if (is.na(val)) "" else unname(val)
    }
    vs <- lapply(jobs, function(nm) {
      plan_verdict(nm, gated[[nm]], list(CI_PIPELINE_SOURCE = "push"), all,
                   secrets, getenv)
    })
    names(vs) <- jobs
    vs
  }
  both <- c(UPLOAD_KEY = "k", UPLOAD_ORG = "o")

  v <- verdicts(c(UPLOAD_KEY = "k"))$upload
  expect("secret: one unset variable makes a selected job not judged, named",
         !v$run && identical(v$unset, "UPLOAD_ORG") &&
           grepl("NOT JUDGED", v$why) && grepl("UPLOAD_ORG", v$why) &&
           !grepl("UPLOAD_KEY", v$why))
  v <- verdicts(c(UPLOAD_KEY = "", UPLOAD_ORG = "o"))$upload
  expect("secret: set but empty counts as unset",
         !v$run && identical(v$unset, "UPLOAD_KEY"))
  v <- verdicts(both)$upload
  expect("secret: every variable set runs the job as any other",
         v$run && !length(v$unset) && identical(v$why, "no rules: always runs"))
  v <- verdicts(character(0))$audit
  expect("secret: a job its rules leave out is skipped for the rule, not judged",
         !v$run && !length(v$unset) && identical(v$why, "no rule matched"))
  v <- verdicts(character(0), all = TRUE)$audit
  expect("secret: --all leaves a schedule-only job out for that reason",
         !v$run && !length(v$unset) && grepl("schedule-only", v$why))
  v <- verdicts(character(0))$always
  expect("secret: a job that declares no secret is untouched",
         v$run && !length(v$unset))

  out <- plan_outcome(verdicts(character(0)))
  expect("outcome: partial skip runs the rest and names what was not judged",
         identical(out$run, "always") && out$status == 0L &&
           identical(out$not_judged, "upload (UPLOAD_KEY UPLOAD_ORG unset)"))
  out <- plan_outcome(verdicts(both))
  expect("outcome: nothing unset judges every selected job",
         identical(out$run, c("always", "upload")) && out$status == 0L &&
           identical(out$not_judged, ""))
  out <- plan_outcome(verdicts(character(0), jobs = "upload"))
  expect("outcome: every applying job not judged exits 3 (NONE)",
         !length(out$run) && out$status == 3L &&
           identical(out$not_judged, "upload (UPLOAD_KEY UPLOAD_ORG unset)"))
  out <- plan_outcome(verdicts(character(0), jobs = "audit"))
  expect("outcome: no job applying at all exits 0, not NONE",
         !length(out$run) && out$status == 0L &&
           identical(out$not_judged, ""))
  out <- plan_outcome(list())
  expect("outcome: an empty config exits 0",
         identical(out$run, character(0)) && out$status == 0L)

  expect("secret entries: valid entries raise nothing",
         !length(secret_problems(secrets, names(gated))))
  probs <- secret_problems(c(renamed = "UPLOAD_KEY", upload = "UPLOAD_KEY"),
                           names(gated))
  expect("secret entries: an entry naming no job is reported, by name",
         length(probs) == 1L && grepl("`renamed` names no job", probs))
  probs <- secret_problems(c(upload = "1BAD", "UPLOAD_KEY"), names(gated))
  expect("secret entries: a bad variable or a missing job name is reported",
         length(probs) == 2L && all(grepl("is not job", probs)))
  probs <- secret_problems(c(upload = "UPLOAD_KEY", upload = "UPLOAD_KEY"),
                           names(gated))
  expect("secret entries: a duplicate entry is reported once",
         length(probs) == 1L && grepl("declared twice", probs))

  # The script-level exits, end to end: this script run as tools/local-ci.sh
  # runs it, in a temp folder holding a fixture config, against the REAL
  # SECRET_JOBS, with each declared variable set or emptied (empty counts as
  # unset) in the child's environment only.
  self <- normalizePath(sub("^--file=", "", grep(
    "^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]))
  secret_vars <- unique(unname(SECRET_JOBS))
  plan <- function(config, mode, set) {
    dir <- tempfile("local-ci-plan-")
    dir.create(dir)
    old <- setwd(dir)
    on.exit({
      setwd(old)
      unlink(dir, recursive = TRUE)
    })
    writeLines(config, CONFIG)
    env <- paste0(secret_vars, "=", if (set) "set" else "")
    out <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), c(shQuote(self), mode),
      stdout = TRUE, stderr = TRUE, env = env))
    status <- attr(out, "status")
    list(status = if (is.null(status)) 0L else status, out = out)
  }
  job_yaml <- function(nm) c(paste0(nm, ":"), "  script: ['true']")
  secret_only <- unlist(lapply(unique(names(SECRET_JOBS)), job_yaml))
  with_plain <- c(job_yaml("plain"), secret_only)

  res <- plan(secret_only, "--jobs", set = FALSE)
  expect("script: --jobs exits 3 and lists nothing when every job is not judged",
         res$status == 3L && !length(res$out))
  res <- plan(secret_only, "--not-judged", set = FALSE)
  expect("script: --not-judged names each declared variable",
         res$status == 0L && length(res$out) == 1L &&
           all(vapply(secret_vars, grepl, logical(1), x = res$out,
                      fixed = TRUE)))
  res <- plan(with_plain, "--jobs", set = FALSE)
  expect("script: --jobs exits 0 with the judged jobs on a partial skip",
         res$status == 0L && identical(res$out, "plain"))
  res <- plan(with_plain, "--jobs", set = TRUE)
  expect("script: with every secret set, --jobs runs the secret-gated jobs",
         res$status == 0L &&
           identical(res$out, c("plain", unique(names(SECRET_JOBS)))))
  res <- plan(with_plain, "--not-judged", set = TRUE)
  expect("script: with every secret set, --not-judged prints nothing",
         res$status == 0L && !length(res$out))
  for (mode in c("--jobs", "--list")) {
    res <- plan(job_yaml("plain"), mode, set = TRUE)
    expect(paste("script: an entry naming no job stops", mode, "with exit 2"),
           res$status == 2L && any(grepl("names no job", res$out)))
  }

  # tools/local-ci.sh PLANS FROM THE REF UNDER TEST (RURL-ecwpdtci). Its jobs
  # run in a clean clone at the ref, so the job list, images, scripts and the
  # secret gate must come from that ref's planner and that ref's CI config, not
  # from whatever the checkout holds. These cases build a throwaway repository
  # whose working tree disagrees with the ref and run `tools/local-ci.sh
  # --list` there (no docker: `--list` stops before the clone). They need git,
  # which the gates job's image does not install, so without it they are
  # skipped and the skip is printed.
  runner <- file.path(dirname(self), "local-ci.sh")
  if (nzchar(Sys.which("git"))) {
    git_env <- c(
      "GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_NOSYSTEM=1",
      "GIT_AUTHOR_NAME=self-test", "GIT_AUTHOR_EMAIL=self-test@invalid",
      "GIT_COMMITTER_NAME=self-test", "GIT_COMMITTER_EMAIL=self-test@invalid"
    )
    repo <- tempfile("local-ci-ref-")
    dir.create(file.path(repo, "tools"), recursive = TRUE)
    git <- function(...) {
      out <- suppressWarnings(system2("git", c("-C", shQuote(repo), ...),
                                      stdout = TRUE, stderr = TRUE,
                                      env = git_env))
      if (!is.null(attr(out, "status"))) {
        stop("self-test git call failed: ", paste(c(...), collapse = " "),
             "\n", paste(out, collapse = "\n"), call. = FALSE)
      }
      out
    }
    at <- function(path) file.path(repo, path)
    commit <- function(...) {
      if (length(c(...))) git("add", ...)
      git("commit", "-q", "-m", "fixture")
      git("rev-parse", "HEAD")
    }
    list_ref <- function(ref) {
      old <- setwd(repo)
      on.exit(setwd(old))
      out <- suppressWarnings(system2(
        "bash", c(shQuote(runner), "--list", shQuote(ref)),
        stdout = TRUE, stderr = TRUE,
        env = c(git_env, paste0("PATH=", shQuote(paste(
          R.home("bin"), Sys.getenv("PATH"), sep = .Platform$path.sep))))))
      status <- attr(out, "status")
      list(status = if (is.null(status)) 0L else status, out = out)
    }
    has <- function(res, pattern) any(grepl(pattern, res$out, fixed = TRUE))
    # The refusal line itself, not any line: the header names both files too.
    refuses <- function(res, path) {
      res$status == 2L && any(grepl(paste0("has no ", path), res$out,
                                    fixed = TRUE) &
                                grepl("HEAD", res$out, fixed = TRUE))
    }

    # `--list` prints each selected job's image, so the fixture needs one.
    ci_config <- function(job) {
      c("default:", "  image: fixture:latest", job_yaml(job), secret_only)
    }
    git("init", "-q")
    file.copy(self, at("tools/local-ci-plan.R"))
    writeLines(ci_config("from_ref"), at(CONFIG))
    ref_sha <- commit("tools/local-ci-plan.R", CONFIG)
    writeLines(ci_config("from_head"), at(CONFIG))
    head_sha <- commit(CONFIG)

    # The motivating case: `--list origin/main` from a feature branch.
    res <- list_ref(ref_sha)
    expect("ref: --list <ref> plans from the ref's CI config, not the tree's",
           res$status == 0L && has(res, "from_ref") && !has(res, "from_head"))
    expect("ref: the header names the revision the plan comes from",
           any(grepl(paste0("^plan: .*", ref_sha), res$out)))

    writeLines("cat('worktree planner\\n')", at("tools/local-ci-plan.R"))
    res <- list_ref(head_sha)
    expect("ref: --list <ref> runs the ref's planner, not the checkout's",
           res$status == 0L && has(res, "from_head") &&
             !has(res, "worktree planner"))
    file.copy(self, at("tools/local-ci-plan.R"), overwrite = TRUE)

    # A ref that lacks either file stops with exit 2 and names it; the copy
    # left in the working tree must not stand in for it.
    git("rm", "-q", "--cached", CONFIG)
    commit()
    res <- list_ref("HEAD")
    expect("ref: a ref without the CI config exits 2, naming it and the ref",
           refuses(res, CONFIG) && !has(res, "from_head"))
    git("add", CONFIG)
    git("rm", "-q", "--cached", "tools/local-ci-plan.R")
    commit()
    res <- list_ref("HEAD")
    expect("ref: a ref without the planner exits 2, naming it and the ref",
           refuses(res, "tools/local-ci-plan.R") && !has(res, "from_head"))
    unlink(repo, recursive = TRUE)
  } else {
    cat("self-test: SKIPPED the plan-from-ref cases -- git is not on PATH\n")
  }

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

problems <- secret_problems(SECRET_JOBS, job_names)
if (length(problems)) {
  message(paste0("SECRET_JOBS: ", problems, collapse = "\n"))
  quit(save = "no", status = 2L)
}

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

verdicts <- lapply(job_names, function(nm) {
  plan_verdict(nm, cfg[[nm]], vars, ignore_rules, SECRET_JOBS)
})
names(verdicts) <- job_names
outcome <- plan_outcome(verdicts)
selected <- outcome$run

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
    v <- verdicts[[nm]]
    cat(sprintf("  %-4s %-10s %s\n", if (v$run) "RUN" else "skip", nm, v$why))
  }
  cat("\n")
  if (nzchar(outcome$not_judged)) {
    cat("not judged: ", outcome$not_judged,
        if (outcome$status == 3L) " -- a run would end NONE", "\n\n", sep = "")
  }
  for (nm in selected) {
    cat(sprintf("[%s] image=%s\n", nm, job_image(cfg[[nm]])))
    cat(render_plan_script(job_script(cfg[[nm]])))
  }
} else if ("--not-judged" %in% args) {
  if (nzchar(outcome$not_judged)) cat(outcome$not_judged, "\n", sep = "")
} else {
  if (length(selected)) cat(paste0(selected, "\n"), sep = "")
  quit(save = "no", status = outcome$status)
}
