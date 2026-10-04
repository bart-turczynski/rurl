#!/usr/bin/env Rscript
# check-toolchain v3
#
# Fail fast when the MACHINE, not the tree, is what makes the verify gate red.
#
# WHY THIS EXISTS. Twice on 2026-09-22, in two different repositories, the
# local gate was red before anything had been changed, and both times the fix
# was to the machine rather than to any tracked file (SEOR-tcytizic):
#
#   robotstxtr  dev/check-docs-drift.R aborted on roxygen2 skew -- 8.1.0
#               installed, 8.0.0 pinned. Fixed with
#               pak::pkg_install("roxygen2@8.0.0").
#   seor        R CMD check WARNING "package 'pslr' was built under R version
#               4.6.1" on a machine running 4.6.0. Fixed by reinstalling the
#               fleet members from CRAN SOURCE under the running R.
#
# Both cost real time and both LOOKED LIKE A CODE DEFECT. An agent picking up a
# ticket sees a red gate and reasonably assumes its own change caused it; one
# spent a large fraction of its budget proving the failure was pre-existing by
# re-running the gate against a pristine worktree. That proof step is the right
# instinct and it should not be needed twice a day.
#
# The second failure is also the expensive kind: it surfaced as a WARNING deep
# inside R CMD check, minutes in, wearing the costume of a package problem.
#
# WHAT IT CHECKS.
#
# 1. roxygen2's installed version equals this package's
#    `Config/roxygen2/version`. That field is the record -- there is no second
#    place to keep the pin in sync with, and all eight fleet DESCRIPTIONs
#    already carry it. A mismatch means `devtools::document()` would rewrite
#    man/ in a different dialect, which is why the docs-drift guards abort.
#
# 2. No package reachable on .libPaths() was BUILT under a newer R than the one
#    running. R records the build version in each installed package's
#    DESCRIPTION, and `R CMD check` turns a newer build into a WARNING --
#    which, with `error_on = "warning"`, is a failed gate. Older builds within
#    the same major are left alone: R loads them and does not warn, so flagging
#    them would be noise.
#
# 3. No direct hard dependency (Depends, Imports, LinkingTo) is installed at a
#    version OLDER than the one CRAN serves now (v2, SEOR-hxxxkmws). R CMD
#    check uses whatever is installed, so a stale library checks the package
#    against a dependency CRAN no longer ships. rurl 3.1.0's first upload
#    failed CRAN's pre-test that way on 2026-10-01: CRAN had served punycoder
#    1.3.0 since 2026-09-30, the machine still held 1.2.1, and the gate passed.
#    A dependency installed NEWER than CRAN (a development sibling) and one
#    CRAN does not serve are listed for information, never failed.
#
#    CRAN is read from https://cloud.r-project.org, not from the `repos`
#    option: Posit mirrors lag CRAN by days, which is the lag this check
#    exists to catch. If CRAN cannot be reached, the check says so and
#    passes: a network blip must not reject a push.
#
# 4. The pandoc rmarkdown uses here is the one CI pins (v3, SEOR-egfbijyi).
#    pandoc's markdown writer reflows text and pads tables differently between
#    versions, so README.md is byte-stable only under the pandoc that knit it.
#    Once Homebrew's pandoc moves past the pin, a local build_readme() knits a
#    README that CI's readme gate reports as drift, after the push passed.
#    This check reads the pin from `PANDOC_VERSION` in .gitlab-ci.yml, the
#    value CI installs; two different values there are reported, not
#    resolved. Its reader is the fleet's only one: check-fleet-standard.py
#    runs `--pandoc-assignments` (below) instead of parsing the pin itself.
#    Unlike check 1's field, it is not the only place the version lives: a
#    bump also moves the two sha256 digests beside it, a re-knit of
#    README.md, and the fleet's PANDOC_PIN in check-fleet-standard.py. A
#    repository whose CI records no pin is not checked here.
#
# WHAT IT DELIBERATELY DOES NOT DO.
#
# * It pins no R version of its own. The rule that matters is RELATIVE -- fleet
#   members must be built under whatever R is running here -- so a recorded
#   constant would be a second thing to keep in sync and a new way to be wrong.
#   Check 2 compares against the live interpreter and needs no record.
# * It installs nothing and repairs nothing. It prints the command that fixes
#   what it found and exits 1. A gate that silently mutates the machine is a
#   gate nobody can reason about.
# * Checks 1, 2 and 4 are offline. Check 3 makes one request, for CRAN's
#   source package index.
#
#   Rscript scripts/check-toolchain.R              # exit 1 on drift
#   Rscript scripts/check-toolchain.R --self-test  # positive/negative cases
#   Rscript --vanilla scripts/check-toolchain.R --pandoc-assignments < .gitlab-ci.yml

package_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  self <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(self) != 1L) {
    return(normalizePath(".", mustWork = TRUE))
  }
  normalizePath(file.path(dirname(self), ".."), mustWork = TRUE)
}

pinned_roxygen <- function(root) {
  dcf <- read.dcf(file.path(root, "DESCRIPTION"))
  field <- "Config/roxygen2/version"
  if (!field %in% colnames(dcf)) {
    return(NA_character_)
  }
  trimws(dcf[[1L, field]])
}

check_roxygen <- function(pinned, installed) {
  if (is.na(pinned)) {
    return(character())
  }
  if (is.na(installed)) {
    return(paste0(
      "roxygen2 is not installed, but DESCRIPTION pins ",
      pinned,
      ". Install it with: ",
      sprintf('Rscript -e \'pak::pkg_install("roxygen2@%s")\'', pinned)
    ))
  }
  if (identical(installed, pinned)) {
    return(character())
  }
  paste0(
    "roxygen2 version mismatch: installed ",
    installed,
    ", DESCRIPTION pins ",
    pinned,
    " (Config/roxygen2/version). This is a MACHINE problem, not a problem ",
    "with your change. Fix it with: ",
    sprintf('Rscript -e \'pak::pkg_install("roxygen2@%s")\'', pinned)
  )
}

# `installed.packages()` reports the R version each package was built under in
# its "Built" column. Only a NEWER build is a problem -- that is the one R CMD
# check turns into a WARNING.
built_under_newer <- function(built, running) {
  keep <- !is.na(built)
  if (!any(keep)) {
    return(character())
  }
  versions <- numeric_version(built[keep], strict = FALSE)
  ok <- !is.na(versions) & versions > running
  names(built[keep])[ok]
}

check_built_versions <- function(offenders, running) {
  if (!length(offenders)) {
    return(character())
  }
  paste0(
    "built under a newer R than this one (",
    running,
    "): ",
    paste(sort(offenders), collapse = ", "),
    ". R CMD check reports each as a WARNING, and the verify gate runs with ",
    "error_on = \"warning\", so the gate is red before your change is read. ",
    "This is a MACHINE problem. Reinstall them from source under the running ",
    "R with: Rscript -e 'install.packages(c(",
    paste(sprintf('"%s"', sort(offenders)), collapse = ", "),
    "), type = \"source\")'"
  )
}

installed_builds <- function() {
  ip <- utils::installed.packages(fields = "Built")
  if (!nrow(ip)) {
    return(stats::setNames(character(), character()))
  }
  stats::setNames(unname(ip[, "Built"]), rownames(ip))
}

# --- Check 3: dependencies older than CRAN ------------------------------------

cran_url <- "https://cloud.r-project.org"

# Direct hard dependencies named in a DESCRIPTION, without version bounds, R
# itself, or base packages (which ship with R, not from CRAN).
hard_dependencies <- function(dcf, base) {
  fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(dcf))
  values <- dcf[1L, fields]
  raw <- unlist(strsplit(values[!is.na(values)], ",", fixed = TRUE))
  pkgs <- trimws(sub("[(].*", "", raw))
  pkgs <- unique(pkgs[nzchar(pkgs)])
  setdiff(pkgs, c("R", base))
}

# `installed` and `cran` are named version strings. Returns the dependencies
# installed older than CRAN, newer than CRAN, and installed but absent from
# CRAN. A dependency that is not installed is left to R CMD check, which
# names it.
compare_with_cran <- function(installed, cran) {
  installed <- installed[!is.na(installed)]
  on_cran <- intersect(names(installed), names(cran))
  have <- numeric_version(installed[on_cran])
  want <- numeric_version(cran[on_cran])
  list(
    older = on_cran[have < want],
    newer = on_cran[have > want],
    absent = setdiff(names(installed), names(cran))
  )
}

check_stale <- function(older, installed, cran) {
  if (!length(older)) {
    return(character())
  }
  older <- sort(older)
  paste0(
    "installed older than CRAN: ",
    paste(
      sprintf("%s %s < %s", older, installed[older], cran[older]),
      collapse = ", "
    ),
    ". R CMD check uses the installed version, so the gate is not checking ",
    "this package against what CRAN serves. This is a MACHINE problem. ",
    "Update them with: Rscript -e 'install.packages(c(",
    paste(sprintf('"%s"', older), collapse = ", "),
    "), repos = \"",
    cran_url,
    "\", type = \"source\")'"
  )
}

# One request: CRAN's source package index. NULL when it cannot be read.
cran_versions <- function() {
  old <- options(timeout = 30)
  on.exit(options(old))
  db <- tryCatch(
    utils::available.packages(
      contriburl = utils::contrib.url(cran_url, "source"),
      type = "source"
    ),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(db) || !nrow(db)) {
    return(NULL)
  }
  stats::setNames(unname(db[, "Version"]), rownames(db))
}

installed_versions <- function(pkgs) {
  stats::setNames(
    vapply(
      pkgs,
      function(p) {
        tryCatch(as.character(utils::packageVersion(p)), error = function(e) {
          NA_character_
        })
      },
      character(1)
    ),
    pkgs
  )
}

run_stale_check <- function(root) {
  base <- rownames(utils::installed.packages(priority = "base"))
  deps <- hard_dependencies(read.dcf(file.path(root, "DESCRIPTION")), base)
  if (!length(deps)) {
    return(character())
  }
  cran <- cran_versions()
  if (is.null(cran)) {
    cat(
      "check-toolchain: could not read ",
      cran_url,
      "; the check for ",
      "dependencies older than CRAN was skipped.\n",
      sep = ""
    )
    return(character())
  }
  installed <- installed_versions(deps)
  found <- compare_with_cran(installed, cran)
  for (p in sort(found$newer)) {
    cat(sprintf(
      "check-toolchain: note: %s %s is newer than CRAN's %s.\n",
      p,
      installed[[p]],
      cran[[p]]
    ))
  }
  for (p in sort(found$absent)) {
    cat(sprintf("check-toolchain: note: %s is not on CRAN.\n", p))
  }
  check_stale(found$older, installed, cran)
}

# --- Check 4: pandoc matches the CI pin ---------------------------------------

# The pin's spellings. This is the only reader of them: check-fleet-standard.py
# runs this script with --pandoc-assignments and judges what it prints, so the
# fleet checker and this check cannot disagree on a pin (SEOR-xhyrogfm).
# A trailing comment is dropped first, a `#` that starts a word; then a pin is
#   * a YAML entry, `PANDOC_VERSION: "3.10"`, quoted or not, or
#   * a shell assignment where sh reads one, as a command: at the start of the
#     line or of a list item, after `;`, `&&`, `||`, `(` or `{`, after
#     `then`, `do` or `else`, or after `export` or other assignments:
#     `PANDOC_VERSION=3.10`, `export R_X=1 PANDOC_VERSION="3.10"`.
#     `echo PANDOC_VERSION=3.9` assigns nothing, and neither does text in a
#     quoted argument to a command, `echo "a; PANDOC_VERSION=3.9"`.
# Every line of .gitlab-ci.yml counts, whichever job it is in. A shell
# assignment overrides `variables:` at run time, so which value a job installs
# depends on where each one sits; the rule is therefore one value, and a second
# distinct value anywhere is reported rather than resolved.
pandoc_yaml_re <- "^\\s*(?:-\\s+)?PANDOC_VERSION:\\s*(\\S.*)$"
pandoc_shell_re <- paste0(
  "(?:^\\s*(?:-\\s+)?|[;&|({\\[,]\\s*|\\b(?:then|do|else|export)\\s+)",
  "(?:\\w+=\\S*\\s+)*[\"']?PANDOC_VERSION=[\"']?([\\w.-]+)"
)

# `line` with each quoted argument to a command blanked: a quoted word after
# another word, as in `echo "…"`. A YAML-quoted list item and a quoted value
# after `=` keep their text.
mask_quoted_args <- function(line) {
  m <- gregexpr(
    "\\w\\s+\\K(?:\"(?:[^\"\\\\]|\\\\.)*\"|'[^']*')",
    line,
    perl = TRUE
  )
  regmatches(line, m) <- lapply(
    regmatches(line, m),
    function(s) strrep("_", nchar(s))
  )
  line
}

# A one-line YAML scalar's value: quotes removed, a trailing comment dropped.
yaml_scalar <- function(raw) {
  raw <- trimws(raw)
  quoted <- regmatches(
    raw,
    regexpr("^(?:\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^']|'')*')", raw, perl = TRUE)
  )
  if (length(quoted)) {
    inner <- substr(quoted, 2L, nchar(quoted) - 1L)
    if (startsWith(quoted, "'")) {
      inner <- gsub("''", "'", inner, fixed = TRUE)
    }
    return(inner)
  }
  value <- sub("\\s+#.*$", "", raw, perl = TRUE)
  # Unquoted, a decimal is a YAML float: GitLab reads `3.10` as 3.1.
  if (grepl("^[-+]?[0-9]+\\.[0-9]+$", value)) {
    value <- as.character(as.numeric(value))
  }
  value
}

# Each PANDOC_VERSION assignment in `lines`, in any spelling above: a data
# frame of the line number and the value, in file order.
pandoc_assignments <- function(lines) {
  lines <- sub("(^|\\s)#.*$", "", lines, perl = TRUE)
  per_line <- lapply(lines, function(line) {
    yaml <- regmatches(line, regexec(pandoc_yaml_re, line, perl = TRUE))[[1L]]
    code <- mask_quoted_args(line)
    shell <- regmatches(code, gregexpr(pandoc_shell_re, code, perl = TRUE))[[
      1L
    ]]
    values <- c(
      if (length(yaml)) yaml_scalar(yaml[[2L]]),
      sub(pandoc_shell_re, "\\1", shell, perl = TRUE)
    )
    values[nzchar(values)]
  })
  data.frame(
    line = rep(seq_along(lines), lengths(per_line)),
    value = as.character(unlist(per_line)),
    stringsAsFactors = FALSE
  )
}

# What --pandoc-assignments prints for `lines`, several .gitlab-ci.yml texts
# separated by a line holding only a form feed: `<text>\t<line>\t<value>` per
# assignment, texts and lines numbered from 1.
pandoc_assignment_report <- function(lines) {
  text <- cumsum(lines == "\f") + 1L
  keep <- lines != "\f"
  texts <- split(lines[keep], factor(text[keep], levels = seq_len(max(text))))
  unlist(lapply(seq_along(texts), function(k) {
    found <- pandoc_assignments(texts[[k]])
    sprintf("%d\t%d\t%s", k, found$line, found$value)
  }))
}

# Every distinct PANDOC_VERSION value .gitlab-ci.yml assigns. Empty when it
# assigns none; more than one is reported by check_pandoc().
pinned_pandoc <- function(lines) {
  unique(pandoc_assignments(lines)$value)
}

read_pandoc_pin <- function(root) {
  path <- file.path(root, ".gitlab-ci.yml")
  if (!file.exists(path)) {
    return(character())
  }
  pinned_pandoc(readLines(path, warn = FALSE))
}

# The version build_readme() renders with, as rmarkdown chooses it: the NEWEST
# pandoc among RSTUDIO_PANDOC, PATH and ~/opt/pandoc. NA when rmarkdown is
# missing or finds no pandoc.
installed_pandoc <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(NA_character_)
  }
  version <- tryCatch(rmarkdown::pandoc_version(), error = function(e) NULL)
  if (is.null(version) || version == "0") {
    return(NA_character_)
  }
  as.character(version)
}

check_pandoc <- function(pinned, installed) {
  if (!length(pinned)) {
    return(character())
  }
  if (length(pinned) > 1L) {
    return(paste0(
      ".gitlab-ci.yml assigns PANDOC_VERSION more than once, with different ",
      "values (",
      paste(pinned, collapse = ", "),
      "). Keep one pin, then re-knit README.md under it."
    ))
  }
  fix <- paste0(
    "Install pandoc ",
    pinned,
    " from https://github.com/jgm/pandoc/releases/tag/",
    pinned,
    ". rmarkdown uses the NEWEST pandoc on RSTUDIO_PANDOC, PATH and ",
    "~/opt/pandoc, so remove or unlink a newer one (Homebrew: ",
    "`brew unlink pandoc`)."
  )
  if (is.na(installed)) {
    return(paste0(
      "rmarkdown finds no pandoc (or rmarkdown is not installed), but ",
      ".gitlab-ci.yml pins pandoc ",
      pinned,
      " (PANDOC_VERSION). ",
      fix
    ))
  }
  if (identical(installed, pinned)) {
    return(character())
  }
  paste0(
    "pandoc version mismatch: rmarkdown uses ",
    installed,
    ", .gitlab-ci.yml pins ",
    pinned,
    " (PANDOC_VERSION). README.md is byte-stable only under the pinned ",
    "pandoc, so build_readme() here knits what CI reports as drift. This is ",
    "a MACHINE problem, not a problem with your change. ",
    fix
  )
}

run_checks <- function(root) {
  installed <- tryCatch(
    as.character(utils::packageVersion("roxygen2")),
    error = function(e) NA_character_
  )
  running <- getRversion()
  c(
    check_roxygen(pinned_roxygen(root), installed),
    check_built_versions(
      built_under_newer(installed_builds(), running),
      running
    ),
    run_stale_check(root),
    check_pandoc(read_pandoc_pin(root), installed_pandoc())
  )
}

report <- function(problems) {
  if (!length(problems)) {
    cat("check-toolchain: machine matches what this package expects.\n")
    return(TRUE)
  }
  cat("check-toolchain FAILED -- the machine, not the tree:\n", file = stderr())
  for (p in problems) {
    cat("  - ", p, "\n", sep = "", file = stderr())
  }
  cat(
    "\nA red gate on an untouched tree is a known class of problem ",
    "(SEOR-tcytizic).\nNothing above is caused by your change.\n",
    sep = "",
    file = stderr()
  )
  FALSE
}

# --- Proving the check can disagree ------------------------------------------

self_test <- function() {
  expect <- function(tag, condition) {
    if (!condition) {
      stop("self-test FAILED (", tag, ")", call. = FALSE)
    }
  }
  flagged <- function(tag, problems, needle) {
    expect(tag, length(problems) == 1L)
    expect(tag, grepl(needle, problems[[1L]], fixed = TRUE))
  }

  expect("roxygen-match", length(check_roxygen("8.0.0", "8.0.0")) == 0L)
  expect(
    "roxygen-unpinned",
    length(check_roxygen(NA_character_, "8.1.0")) == 0L
  )
  flagged("roxygen-skew", check_roxygen("8.0.0", "8.1.0"), "installed 8.1.0")
  flagged("roxygen-skew", check_roxygen("8.0.0", "8.1.0"), "pak::pkg_install")
  flagged(
    "roxygen-absent",
    check_roxygen("8.0.0", NA_character_),
    "not installed"
  )

  running <- numeric_version("4.6.0")
  builds <- c(older = "4.5.1", same = "4.6.0", newer = "4.6.1", broken = NA)
  offenders <- built_under_newer(builds, running)
  expect("built-newer-only", identical(offenders, "newer"))
  expect("built-none", length(built_under_newer(builds[1:2], running)) == 0L)
  expect("built-unparseable-ignored", !("broken" %in% offenders))
  flagged(
    "built-message",
    check_built_versions("newer", running),
    "type = \"source\""
  )
  flagged(
    "built-message",
    check_built_versions("newer", running),
    "MACHINE problem"
  )

  dcf <- read.dcf(textConnection(paste(
    "Package: x",
    "Depends: R (>= 4.1.0), utils",
    "Imports: punycoder (>= 1.2.1),\n    stringi, tools",
    "LinkingTo: Rcpp",
    "Suggests: testthat",
    sep = "\n"
  )))
  deps <- hard_dependencies(dcf, base = c("utils", "tools"))
  expect("deps-hard-only", identical(deps, c("punycoder", "stringi", "Rcpp")))
  expect(
    "deps-none",
    length(hard_dependencies(read.dcf(textConnection("Package: x")), "")) == 0L
  )

  cran <- c(punycoder = "1.3.0", stringi = "1.8.7", rurl = "3.1.0")
  installed <- c(
    punycoder = "1.2.1",
    stringi = "1.8.7",
    rurl = "3.1.0.9000",
    robotstxtr = "0.3.0",
    missing = NA
  )
  found <- compare_with_cran(installed, cran)
  expect("stale-older", identical(found$older, "punycoder"))
  expect("stale-newer", identical(found$newer, "rurl"))
  expect("stale-absent", identical(found$absent, "robotstxtr"))
  expect(
    "stale-current",
    !length(compare_with_cran(cran, cran)$older)
  )
  flagged(
    "stale-message",
    check_stale("punycoder", installed, cran),
    "punycoder 1.2.1 < 1.3.0"
  )
  flagged(
    "stale-message",
    check_stale("punycoder", installed, cran),
    "MACHINE problem"
  )
  expect("stale-none", !length(check_stale(character(), installed, cran)))

  ci <- c(
    "  variables:",
    "    PANDOC_VERSION: \"3.10\"",
    "    - curl \"https://example.org/${PANDOC_VERSION}/pandoc.deb\"",
    "    # PANDOC_VERSION: \"9.9\" in a comment is not a pin",
    "    #   - PANDOC_VERSION=9.9 neither"
  )
  expect("pandoc-pin-yaml", identical(pinned_pandoc(ci), "3.10"))
  expect("pandoc-pin-none", !length(pinned_pandoc(ci[3:5])))
  # The spellings check-fleet-standard.py's self-test passes, one by one.
  spellings <- c(
    "    - PANDOC_VERSION=3.10",
    "    - PANDOC_VERSION=\"3.10\"",
    "    - export PANDOC_VERSION=3.10",
    "    - PANDOC_VERSION=3.10  # the fleet pin",
    "    - export PANDOC_VERSION='3.10' # pinned",
    "  PANDOC_VERSION: \"3.10\"",
    "  PANDOC_VERSION: '3.10'",
    "  PANDOC_VERSION: \"3.10\"  # the fleet pin",
    "  PANDOC_VERSION: '3.10' # pinned"
  )
  for (line in spellings) {
    expect(paste("pandoc-pin:", line), identical(pinned_pandoc(line), "3.10"))
  }
  # Unquoted, YAML reads 3.10 as the float 3.1, and so does this check.
  for (line in c("  PANDOC_VERSION: 3.10", "  PANDOC_VERSION: 3.10 # pinned")) {
    expect(paste("pandoc-pin:", line), identical(pinned_pandoc(line), "3.1"))
  }
  expect("pandoc-match", !length(check_pandoc("3.10", "3.10")))
  expect("pandoc-unpinned", !length(check_pandoc(character(), "3.11")))
  flagged("pandoc-skew", check_pandoc("3.10", "3.11"), "rmarkdown uses 3.11")
  flagged("pandoc-skew", check_pandoc("3.10", "3.11"), "MACHINE problem")
  flagged("pandoc-patch", check_pandoc("3.10", "3.10.1"), "pins 3.10")
  flagged("pandoc-absent", check_pandoc("3.10", NA_character_), "finds no")
  flagged(
    "pandoc-two-pins",
    check_pandoc(c("3.10", "3.9"), "3.10"),
    "more than once"
  )
  # A shell assignment overrides `variables:` at run time, so a second value is
  # a second pin whichever one CI would install (SEOR-xhyrogfm).
  both <- c("  PANDOC_VERSION: \"3.10\"", "    - PANDOC_VERSION=3.9")
  flagged(
    "pandoc-variables-and-shell",
    check_pandoc(pinned_pandoc(both), "3.10"),
    "more than once"
  )
  expect(
    "pandoc-same-pin-twice",
    identical(pinned_pandoc(c(both[[1L]], "    - PANDOC_VERSION=3.10")), "3.10")
  )
  # Only an assignment sh would run counts: not one in a trailing comment or
  # in echo and printf text, which would read as a second pin (SEOR-xhyrogfm).
  for (line in c(
    "    - PANDOC_VERSION=3.10  # was PANDOC_VERSION=3.9",
    "    - echo \"PANDOC_VERSION=3.9 is gone\"; PANDOC_VERSION=3.10",
    "    - printf 'PANDOC_VERSION=%s\\n' 3.9 && export PANDOC_VERSION=3.10",
    "  before_script: [PANDOC_VERSION=3.10, echo PANDOC_VERSION=3.9]",
    "    - echo \"pinned; PANDOC_VERSION=3.9 was old\" && PANDOC_VERSION=3.10",
    "    - echo \"use export PANDOC_VERSION=3.9\"; PANDOC_VERSION=3.10",
    "    - export R_X=1 PANDOC_VERSION=3.10",
    "  before_script: ['PANDOC_VERSION=3.10', 'echo hi']"
  )) {
    expect(paste("pandoc-pin:", line), identical(pinned_pandoc(line), "3.10"))
  }
  # check-fleet-standard.py reads the pin through --pandoc-assignments: these
  # texts, separated by a form feed and numbered as in their files, are what
  # it judges.
  report <- pandoc_assignment_report(c(
    "variables:",
    "  PANDOC_VERSION: \"3.10\"",
    "  # PANDOC_VERSION=9.9",
    "    - export PANDOC_VERSION=3.9",
    "\f",
    "x: 1",
    "\f",
    "  - PANDOC_VERSION=3.8"
  ))
  expect(
    "pandoc-assignment-report",
    identical(report, c("1\t2\t3.10", "1\t4\t3.9", "3\t1\t3.8"))
  )
  expect(
    "pandoc-assignment-none",
    !length(pandoc_assignment_report(ci[3:5]))
  )

  paste0(
    "check-toolchain self-test: PASS (5 roxygen cases, 5 build-version ",
    "cases, 9 CRAN-version cases, 27 pandoc cases)\n"
  )
}

# The self-test runs on EVERY invocation, not only under --self-test. It is
# pure and instant -- no I/O, no network, no library scan -- and a check whose
# negative cases only run when someone remembers to ask for them is a check
# nobody is running. The flag remains for running it alone.
#
# --pandoc-assignments is the exception: it reads .gitlab-ci.yml texts on
# stdin, separated by a line holding a form feed, and prints each pin they
# assign as `<text>\t<line>\t<value>`, nothing else.
# check-fleet-standard.py reads the pin this way rather than with a parser of
# its own, and runs it with every fixture of its self-test at once.
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--pandoc-assignments" %in% args) {
    lines <- readLines(file("stdin"), warn = FALSE, encoding = "UTF-8")
    writeLines(pandoc_assignment_report(lines))
    return(0L)
  }
  cat(self_test())
  if ("--self-test" %in% args) {
    return(0L)
  }
  if (report(run_checks(package_root()))) 0L else 1L
}

quit(status = main())
