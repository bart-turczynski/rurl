#!/usr/bin/env Rscript
# check-toolchain v4
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
#    runs `--pandoc-assignments` (below) instead of parsing the pin itself,
#    and `--pandoc-unread` for the unread settings this check stops on.
#    It parses the file with the yaml package (v4, SEOR-bqroclcz, ADR 0009)
#    and reads shell assignments in its script items; a file that does not
#    load stops it. GitLab's expanded form, a `value:` mapping, is an error
#    that asks for a plain scalar: read as a pin it would be none, or the
#    wrong one. Unlike check 1's field, it is not the only place the version
#    lives: a bump also moves the two sha256 digests beside it, a re-knit of
#    README.md, and the fleet's PANDOC_PIN in check-fleet-standard.py. A
#    repository whose CI never names PANDOC_VERSION is not checked here.
#    One that uses `$PANDOC_VERSION` with no pin this reads fails: a pin in
#    an included file would otherwise pass as no pin at all (SEOR-mbiwrxql).
#    And a PANDOC_VERSION value set in a spelling this does not read fails
#    whatever pin it does read: beside a global `PANDOC_VERSION: "3.10"`, a
#    job's `env PANDOC_VERSION=3.9` would otherwise pass as 3.10 while that
#    job installs 3.9 (SEOR-usrdxvbj). A null value (`PANDOC_VERSION:` bare,
#    `~` or `null`) sets no version, and passes.
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
#   Rscript scripts/check-toolchain.R --pandoc-assignments < .gitlab-ci.yml
#   Rscript scripts/check-toolchain.R --pandoc-unread < .gitlab-ci.yml

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
#
# .gitlab-ci.yml is parsed with the yaml package (ADR 0009) as YAML 1.1, the
# way GitLab reads it: quoting, comments, flow and block collections,
# anchors, aliases and `<<` merge keys are the parser's. A pin is
#   * the value of a PANDOC_VERSION key in any mapping at any depth: the
#     global `variables:`, a job's or a template's, a `rules:` or a
#     `parallel: matrix` entry's. Unquoted, `3.10` is the float 3.1, as
#     GitLab reads it; or
#   * a shell assignment in a script item where sh reads one as a command:
#     at the start of a line, after `;`, `&&`, `||`, `|`, a `(` subshell, a
#     `{` group or a `case` pattern's `)` (after `case WORD in`, `;;` or
#     `;&`, or starting a line), and after `if`, `elif`, `while`, `until`,
#     `then`, `do`, `else`, `time` or `!` there, plain or through `export`,
#     `readonly`, `declare` or `typeset`, after other assignments or as a
#     command's prefix: `PANDOC_VERSION=3.10`,
#     `export R_X=1 PANDOC_VERSION="3.10"`. Quotes, escapes, backquotes and
#     substitutions are read once, by sh_mask(), and a `\` continues a line.
#     A script item names PANDOC_VERSION only in forms this reads (ADR
#     0010): a pin, a use (`$PANDOC_VERSION`, `${PANDOC_VERSION:-x}`,
#     `Sys.getenv("PANDOC_VERSION")`), a bare `export PANDOC_VERSION`, or a
#     comment. Any other mention is refused, not read, echoed text included:
#     sh sets a variable in more ways than a list of refusals can name. So
#     is a line that names it split or escaped (disguised_lines()). A
#     script item is
#     a string in a YAML sequence, at any depth, or the value of `script`,
#     `before_script` or `after_script`, so an anchored list and a
#     `!reference` target count whether a job uses them or not.
# A pin is a literal version: letters, digits, `.`, `+` and `-`. Every one
# in the file counts, whichever job it is in. A shell assignment overrides
# `variables:` at run time, so which value a job installs depends on where
# each one sits; the rule is therefore one value, and a second distinct value
# anywhere is reported rather than resolved.
# GitLab's expanded form is not a pin this reads: the key with a mapping that
# holds `value:`, `description:`, `expand:` or `options:`. pinned_pandoc()
# stops on it and names the fix, a plain scalar (SEOR-zvnrwaku);
# --pandoc-assignments prints no pin for it.
pandoc_tag <- "PANDOC_VERSION__"
pandoc_literal_re <- "^[A-Za-z0-9.+-]+$"
script_keys <- c("script", "before_script", "after_script")
# Keys whose lists GitLab never runs as shell: file paths, job and stage
# names, tags, rule conditions, includes. Their strings are text.
text_keys <- c(
  "paths",
  "exclude",
  "untracked",
  "files",
  "changes",
  "exists",
  "needs",
  "dependencies",
  "extends",
  "stages",
  "tags",
  "only",
  "except",
  "refs",
  "include",
  "options",
  "exit_codes"
)
expanded_keys <- c("value", "description", "expand", "options")

# The oldest yaml package this reader runs on: yaml.load() takes
# `merge.precedence` from 2.2.1 (the package's NEWS), and the handlers and
# `eval.expr` it also passes are older.
yaml_minimum <- "2.2.1"

# The yaml package at `minimum` or later, or a one-line stop naming it, so a
# machine without it is not reported as broken YAML. This script is vendored
# into every fleet package, and its pre-push hook runs the R on PATH.
need_yaml <- function(pkg = "yaml", minimum = yaml_minimum) {
  installed <- if (requireNamespace(pkg, quietly = TRUE)) {
    utils::packageVersion(pkg)
  }
  if (is.null(installed) || installed < minimum) {
    stop(
      "check-toolchain.R reads .gitlab-ci.yml with the R package ",
      pkg,
      " ",
      minimum,
      " or later, ",
      if (is.null(installed)) {
        "which is not installed"
      } else {
        paste0("but ", installed, " is installed")
      },
      ". Install it with: ",
      sprintf("Rscript -e 'install.packages(\"%s\")'", pkg),
      call. = FALSE
    )
  }
}

# A quoted string in sh: double quotes, where a backslash escapes, or single
# quotes, where nothing does.
sh_quoted <- "\"(?:[^\"\\\\]|\\\\.)*\"|'[^']*'"

# `lines`, shell, masked: each part of a word sh does not read as plain text
# hidden, every character of it turned into `.`, and a trailing comment cut.
# The parts are quoted strings (`'...'`, `"..."`, `$'...'`), an escape
# (`\x`), a backquoted command, and `$(...)`, `$((...))`, `${...}`, `<(...)`
# and `>(...)` with all they nest, so a `)`, `;`, `#` or space inside one is
# hidden with it. A comment is a `#` that starts a word: at the start of the
# line, after a space or after one of `;&|()<>`. A part a line does not close
# is hidden to its end. A masked line keeps each character it does not cut
# where it was, so what a pattern finds on it is read on the line itself.
# This is the one reading of quotes, escapes and substitutions the shell
# side has: the pins, the command heads and the comments are all found on
# its output.
sh_mask <- function(lines) {
  vapply(lapply(lines, sh_mask_line), `[[`, character(1), "masked")
}

# What ends a word before a `#` that starts a comment.
sh_word_ends <- c(" ", "\t", ";", "&", "|", "(", ")", "<", ">")

sh_mask_line <- function(line) {
  ch <- strsplit(line, "", fixed = TRUE)[[1L]]
  n <- length(ch)
  at <- function(i) if (i <= n) ch[[i]] else ""
  quotes <- which(ch == "'")
  found <- new.env()
  found$continued <- FALSE
  found$open <- FALSE
  # From `i`, the index after the `close` that ends a part opened before
  # `i`, or n + 1 when none does. A backslash escapes in every part: in
  # `'` (as `$'...'`) and `` ` `` nothing else is read; in `"`, `$(`, `` ` ``
  # and `${` open a nested part; in `)` and `}`, quotes do too, and in `)`
  # each `(` needs a `)` of its own.
  skip <- function(i, close) {
    depth <- 0L
    while (i <= n) {
      c <- ch[[i]]
      if (c == "\\") {
        found$continued <- i == n
        i <- i + 2L
        next
      }
      if (c == close && depth == 0L) {
        return(i + 1L)
      }
      if (close %in% c("'", "`")) {
        i <- i + 1L
        next
      }
      nested <- close != "\""
      i <- if (c == "$" && at(i + 1L) == "(") {
        skip(i + 2L, ")")
      } else if (nested && c == "$" && at(i + 1L) == "'") {
        skip(i + 2L, "'")
      } else if (c == "$" && at(i + 1L) == "{") {
        skip(i + 2L, "}")
      } else if (c == "`") {
        skip(i + 1L, "`")
      } else if (nested && c == "\"") {
        skip(i + 1L, "\"")
      } else if (nested && c == "'") {
        single(i + 1L)
      } else {
        if (close == ")" && c == "(") {
          depth <- depth + 1L
        } else if (close == ")" && c == ")") {
          depth <- depth - 1L
        }
        i + 1L
      }
    }
    found$open <- TRUE
    n + 1L
  }
  # A single-quoted string, where nothing escapes.
  single <- function(i) {
    end <- quotes[quotes >= i]
    if (!length(end)) {
      found$open <- TRUE
      return(n + 1L)
    }
    end[[1L]] + 1L
  }
  hide <- logical(n)
  keep <- n
  i <- 1L
  while (i <= n) {
    c <- ch[[i]]
    after <- at(i + 1L)
    end <- if (c == "\\") {
      found$continued <- i == n
      i + 2L
    } else if (c == "'") {
      single(i + 1L)
    } else if (c %in% c("\"", "`")) {
      skip(i + 1L, c)
    } else if (c == "$" && after == "'") {
      skip(i + 2L, "'")
    } else if (c == "$" && after == "(") {
      skip(i + 2L, ")")
    } else if (c %in% c("<", ">") && after == "(") {
      skip(i + 2L, ")")
    } else if (c == "$" && after == "{") {
      skip(i + 2L, "}")
    }
    # A hidden character before it, a part's, is the same word's.
    if (
      c == "#" && (i == 1L || ch[[i - 1L]] %in% sh_word_ends && !hide[[i - 1L]])
    ) {
      keep <- i - 1L
      break
    }
    if (is.null(end)) {
      i <- i + 1L
    } else {
      end <- min(end, n + 1L)
      hide[i:(end - 1L)] <- TRUE
      i <- end
    }
  }
  ch[hide] <- "."
  list(
    masked = paste(ch[seq_len(keep)], collapse = ""),
    continued = keep == n && found$continued,
    open = keep == n && found$open
  )
}

# `lines` with each trailing comment dropped.
drop_comments <- function(lines) substr(lines, 1L, nchar(sh_mask(lines)))

# A PANDOC_VERSION token, not one inside an anchor's or an alias's name
# (`&pv-PANDOC_VERSION`, `*PANDOC_VERSION`): renamed there, the anchor and
# its alias would no longer match. An anchor or an alias starts only a node:
# at the start of a line, after `- `, `: ` or `? `, or after `[`, `{` or `,`.
# Elsewhere `&` and `*` are text (`?os=linux&v=$PANDOC_VERSION`,
# `pandoc*$PANDOC_VERSION.deb`), and the token in them is renamed. A name
# followed by `=` or `+=` is a shell assignment (`true &&PANDOC_VERSION=3.9`),
# not a name.
pandoc_token_re <- paste0(
  "(?:^\\s*|(?<=[-:?]\\s)\\s*|(?<=[\\[{,])\\s*)",
  "[&*][^\\s,\\[\\]{}]*?(?<!\\w)PANDOC_VERSION(?!\\w|\\+?=)",
  "[^\\s,\\[\\]{}]*(*SKIP)(*FAIL)|(?<!\\w)PANDOC_VERSION(?!\\w)"
)

# `lines` with each PANDOC_VERSION token renamed PANDOC_VERSION__<id>, the
# ids counting the tokens in file order, and `at`, each id's line. The yaml
# package keeps no line numbers, so the reader parses the renamed text: a key
# or a script item the parser hands back names its own line. An anchor's or
# an alias's name keeps its text.
tag_pandoc <- function(lines) {
  m <- gregexpr(pandoc_token_re, lines, perl = TRUE)
  n <- vapply(m, function(x) sum(x > 0L), integer(1))
  at <- rep(seq_along(lines), n)
  ids <- split(seq_along(at), factor(at, levels = seq_along(lines)))
  regmatches(lines, m) <- lapply(ids, function(id) paste0(pandoc_tag, id))
  list(lines = lines, at = at)
}

# The ids of the tokens in `text`, matched by `re` with the id as its group.
tagged_ids <- function(text, re) {
  found <- regmatches(text, gregexpr(re, text, perl = TRUE))
  ids <- sub(paste0("^.*", pandoc_tag, "(\\d+).*$"), "\\1", unlist(found))
  as.integer(ids)
}

# The lines of `lines`, a .gitlab-ci.yml text, that name PANDOC_VERSION
# where pandoc_token_re cannot see it: split by a quote or a backslash
# (`PANDOC_"VERSION"`, `PAN\DOC_VERSION`), written with an escape that YAML
# or sh's `$'...'` decodes (`PANDOC_\x56ERSION`, `"x\nPANDOC_VERSION=3.9"`),
# or broken across an escaped line break. The parser or sh reads the name
# and this reader would not, so the line is refused, the first of a broken
# one. A name computed at run time (`${P}_VERSION`) no reader sees.
disguised_lines <- function(lines) {
  count <- function(x) {
    lengths(regmatches(
      x,
      gregexpr("(?<!\\w)PANDOC_VERSION(?!\\w)", x, perl = TRUE)
    ))
  }
  # Each escape decoded once, a letter one (`\n`, `\t`) to a space as YAML
  # and `$'...'` read it.
  decoded_layer <- function(x) {
    m <- gregexpr(
      "\\\\(?:x[0-9A-Fa-f]{1,2}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8}|[0-7]{1,3}|.)",
      x,
      perl = TRUE
    )
    regmatches(x, m) <- lapply(regmatches(x, m), function(esc) {
      code <- substring(esc, 2L)
      hex <- grepl("^[xuU]", code)
      octal <- grepl("^[0-7]", code)
      out <- ifelse(grepl("^[A-Za-z]$", code), " ", code)
      out[hex] <- vapply(
        strtoi(substring(code[hex], 2L), 16L),
        intToUtf8,
        character(1)
      )
      out[octal] <- vapply(strtoi(code[octal], 8L), intToUtf8, character(1))
      out[is.na(out)] <- " "
      out
    })
    x
  }
  if (!length(lines)) {
    return(integer())
  }
  broken <- c(FALSE, grepl("\\\\$", lines[-length(lines)]))
  group <- cumsum(!broken)
  first <- which(!broken)
  joined <- vapply(
    split(lines, group),
    function(x) {
      paste(sub("^\\s+", "", sub("\\\\$", "", x)), collapse = "")
    },
    character(1)
  )
  raw <- vapply(split(count(lines), group), sum, integer(1))
  # A `$'...'` in a YAML double-quoted string is decoded twice, so each
  # layer is decoded in turn; and sh drops the backslash of `\D` where YAML
  # reads `\n`, so each layer is also read with its backslashes dropped.
  seen <- integer(length(joined))
  layer <- joined
  for (pass in 1:3) {
    seen <- pmax(seen, count(gsub("[\"'\\\\]", "", layer)))
    layer <- decoded_layer(layer)
    seen <- pmax(seen, count(gsub("[\"'\\\\]", "", layer)))
  }
  first[seen > raw]
}

# GitLab's `!reference [...]`, kept as what it is: neither the parser nor
# this reader resolves it.
gitlab_reference <- function(x) structure(list(x), class = "gitlab_reference")

# A YAML sequence, marked: the parser hands a one-item list back otherwise
# as a bare scalar, and only a sequence's strings are script items.
yaml_sequence <- function(x) {
  attr(x, "yaml_sequence") <- TRUE
  x
}

# The yaml package's warnings that change nothing this reader reads: a
# number out of R's integer or double range loads as NA, which a
# PANDOC_VERSION key refuses as unread, and which is no script item.
yaml_harmless <- "is out of (?:integer|real) range$"

# Each YAML document in `tagged$lines` (a `---` line starts one) parsed, or
# the earliest problem in file order: list(docs, error), the error's line
# (NA when the parser names none) and message, its line numbers counted from
# the top of the text. The earliest problem is in the first document that
# has one: of its duplicate PANDOC_VERSION keys, the parser's error and its
# warnings, the one on the first line, one with no line last. A duplicate
# is a PANDOC_VERSION key after the first in one mapping: renamed, they no
# longer collide, so the parser cannot refuse them as it refuses any other
# duplicate key, and GitLab keeps the last. A key a `<<` merge brings in was
# a key of the mapping it came from first, and is no duplicate. A warning
# fails the load unless it is harmless: the yaml package otherwise loads an
# unknown alias as a string, and a `!!int` it cannot read as NA. A `spec:`
# header (spec_header()) declares an include's inputs, not variables: its
# keys set nothing, so it is neither parsed nor returned, and no key in it
# is refused, whatever its name.
load_ci <- function(tagged) {
  lines <- tagged$lines
  starts <- unique(c(1L, grep("^---(?:\\s|$)", lines, perl = TRUE)))
  ends <- c(starts[-1L] - 1L, length(lines))
  docs <- vector("list", length(starts))
  header <- length(starts) > 1L && spec_header(lines[seq_len(ends[[1L]])])
  for (d in seq_along(starts)) {
    if (d == 1L && header) {
      next
    }
    text <- lines[seq.int(
      starts[[d]],
      length.out = ends[[d]] - starts[[d]] + 1L
    )]
    parsed <- yaml_parse(text)
    duplicates <- yaml_duplicates(text, parsed)
    messages <- c(
      if (inherits(parsed$doc, "error")) conditionMessage(parsed$doc),
      parsed$warnings
    )
    problems <- do.call(
      rbind,
      c(
        list(data.frame(
          line = tagged$at[duplicates],
          message = rep(
            "Duplicate map key: 'PANDOC_VERSION'",
            length(duplicates)
          )
        )),
        lapply(messages, yaml_error, starts[[d]], ends[[d]])
      )
    )
    if (nrow(problems)) {
      first <- order(problems$line, na.last = TRUE)[[1L]]
      return(list(docs = list(), error = as.list(problems[first, ])))
    }
    docs[d] <- list(parsed$doc)
  }
  list(docs = if (header) docs[-1L] else docs, error = NULL)
}

# Whether `text`, the first document's lines, is a `spec:` header: its one
# line at the top level, past comments and `---`, starts `spec:`, and a
# `---` follows it, whatever comes after that. Read from
# the text, not the parse, so a header the parser refuses (a duplicate
# input, say) is still one. A header written another way, `"spec":` or
# `{spec: ...}`, is parsed as config, where a setting in it is refused.
spec_header <- function(text) {
  top <- grep("^[^\\s#]", text, perl = TRUE, value = TRUE)
  top <- top[!grepl("^(?:---|\\.\\.\\.)(?:\\s|$)|^%", top, perl = TRUE)]
  length(top) == 1L && grepl("^spec:(?:\\s|$)", top, perl = TRUE)
}

# `text`, a document's lines, parsed: list(doc, duplicates, warnings), `doc`
# the document or the parser's error, `duplicates` the ids of the
# PANDOC_VERSION keys after the first in a mapping the parser built, and
# `warnings` the messages of the warnings that are not harmless. A warning
# is kept and the parse goes on, so every mapping it was inside is built.
yaml_parse <- function(text) {
  found <- new.env()
  found$seen <- integer()
  found$duplicates <- integer()
  found$warnings <- character()
  # The parser builds a merge's source, and every mapping inside another,
  # before the mapping that holds it.
  yaml_mapping <- function(x) {
    ids <- tagged_ids(names(x), paste0("^", pandoc_tag, "\\d+$"))
    own <- sort(setdiff(ids, found$seen))
    found$duplicates <- c(found$duplicates, own[-1L])
    found$seen <- c(found$seen, ids)
    x
  }
  doc <- tryCatch(
    withCallingHandlers(
      yaml::yaml.load(
        paste(text, collapse = "\n"),
        eval.expr = FALSE,
        merge.precedence = "override",
        handlers = list(
          seq = yaml_sequence,
          map = yaml_mapping,
          reference = gitlab_reference
        )
      ),
      warning = function(w) {
        if (!grepl(yaml_harmless, conditionMessage(w), perl = TRUE)) {
          found$warnings <- c(found$warnings, conditionMessage(w))
        }
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  list(doc = doc, duplicates = found$duplicates, warnings = found$warnings)
}

# The ids of the duplicate PANDOC_VERSION keys in `text`, a document's lines,
# `parsed` its yaml_parse(). A mapping still open where the parser stops on
# an error is never built, so its keys are not seen: the lines above the
# line the error stops on, the last it names, are parsed again for theirs,
# and so on while that parse stops too. A mapping no such prefix closes (a
# flow mapping the error is inside) and an error that names no line (a
# duplicate of another key) leave its keys unseen; the load still fails,
# naming the error.
yaml_duplicates <- function(text, parsed) {
  duplicates <- parsed$duplicates
  if (!inherits(parsed$doc, "error")) {
    return(duplicates)
  }
  named <- regmatches(
    conditionMessage(parsed$doc),
    gregexpr("(?<=line )\\d+", conditionMessage(parsed$doc), perl = TRUE)
  )[[1L]]
  if (!length(named)) {
    return(duplicates)
  }
  stop_at <- as.integer(named[[length(named)]])
  above <- text[seq_len(min(stop_at, length(text)) - 1L)]
  if (!length(above)) {
    return(duplicates)
  }
  union(duplicates, yaml_duplicates(above, yaml_parse(above)))
}

# What the parser's error or warning `message` on the document from line
# `start` to `end` says: its line (NA when the parser names none) and
# message, the message's line numbers counted from the top of the text, as
# one row.
yaml_error <- function(message, start, end) {
  message <- gsub(paste0(pandoc_tag, "\\d+"), "PANDOC_VERSION", message)
  message <- trimws(gsub("\\s+", " ", message))
  # The parser counts lines from the document's start.
  m <- gregexpr("(?<=line )\\d+", message, perl = TRUE)
  regmatches(message, m) <- lapply(
    regmatches(message, m),
    function(n) as.character(start - 1L + as.integer(n))
  )
  at <- regmatches(message, m)[[1L]]
  # The end of the text is the line after its last.
  line <- if (length(at)) {
    min(as.integer(at[[length(at)]]), end)
  } else {
    NA_integer_
  }
  data.frame(line = line, message = message)
}

# What a parsed document holds for the reader: `keys`, each PANDOC_VERSION
# key's id and value; `shell`, the script items; `text`, every other string,
# and every string under one of text_keys. A `!reference` holds names, not
# text.
ci_nodes <- function(node, shell = FALSE, text = FALSE) {
  found <- list(keys = list(), shell = character(), text = character())
  if (inherits(node, "gitlab_reference")) {
    return(found)
  }
  if (is.character(node)) {
    found[[if (shell) "shell" else "text"]] <- node
    return(found)
  }
  if (!is.list(node)) {
    return(found)
  }
  sequence <- isTRUE(attr(node, "yaml_sequence")) || is.null(names(node))
  keys <- if (sequence) character(length(node)) else names(node)
  for (i in seq_along(node)) {
    id <- tagged_ids(keys[[i]], paste0("^", pandoc_tag, "\\d+$"))
    if (length(id)) {
      found$keys[[length(found$keys) + 1L]] <- list(id = id, value = node[[i]])
    }
    under_text <- text || keys[[i]] %in% text_keys
    inner <- ci_nodes(
      node[[i]],
      !under_text && (sequence || keys[[i]] %in% script_keys),
      under_text
    )
    found$keys <- c(found$keys, inner$keys)
    found$shell <- c(found$shell, inner$shell)
    found$text <- c(found$text, inner$text)
  }
  found
}

# A number as GitLab writes a YAML float or integer into a variable: 3.1 for
# `3.10`, 3.0 for `3.0`.
yaml_number <- function(x) {
  if (is.double(x) && is.finite(x) && x == round(x)) {
    return(sprintf("%.1f", x))
  }
  as.character(x)
}

# What the parsed `value` of the PANDOC_VERSION key with `id` sets, the key
# on `line`: "pin" and its version; "none", null (`PANDOC_VERSION:` bare, `~`,
# `null`), which sets no version; "expanded", GitLab's expanded form; or
# "unread": a `!reference`, a list, another mapping, a boolean, an empty or
# computed value, or a number the parser may have typed where GitLab reads a
# string. The yaml package types a block scalar (`>-`, then `3.10` below) as
# it types plain text, 3.1, where GitLab reads "3.10", and an alias's value
# has no style left to see; so a number counts only when its plain text
# follows the key on the key's own line.
yaml_setting <- function(value, line, id) {
  if (is.null(value)) {
    return(list(kind = "none"))
  }
  if (inherits(value, "gitlab_reference")) {
    return(list(kind = "unread"))
  }
  if (is.list(value)) {
    expanded <- !isTRUE(attr(value, "yaml_sequence")) &&
      any(names(value) %in% expanded_keys)
    return(list(kind = if (expanded) "expanded" else "unread"))
  }
  if (length(value) != 1L || !(is.character(value) || is.numeric(value))) {
    return(list(kind = "unread"))
  }
  if (is.numeric(value)) {
    key <- regexpr(paste0(pandoc_tag, id, "[\"']?\\s*:"), line, perl = TRUE)
    inline <- if (key > 0L) {
      substring(line, key + attr(key, "match.length"))
    } else {
      ""
    }
    inline <- sub("^\\s*(?:[&!][^\\s,{}\\[\\]]*\\s*)*", "", inline, perl = TRUE)
    if (!nzchar(inline) || grepl("^[|>*#]", inline)) {
      return(list(kind = "unread"))
    }
    value <- yaml_number(value)
  }
  if (!grepl(pandoc_literal_re, value)) {
    return(list(kind = "unread"))
  }
  list(kind = "pin", value = value)
}

# The reserved words a command may follow: `if`, `elif`, `while`, `until`,
# `then`, `do`, `else`, `time` (`-p` too) and `!`.
sh_reserved <- "(?:(?:if|elif|while|until|then|do|else|time(?:\\s+-p)?|!)\\s+)*"
# Where sh starts a command outside a `case`: the start of a line, `;`, `&`,
# `|`, a `(` subshell or a `{` group. `((` starts arithmetic, no command.
sh_start <- "(?:^|[;&|{]|(?<!\\()\\((?!\\())"
# A word on a masked line (sh_mask()): its quoted, escaped and substituted
# parts are hidden there, so it is plain text.
sh_word <- "[^\\s;&|<>()]*"
# A word in a `case` head or pattern list.
sh_case_word <- "[^\\s;&|<>()]+"
# A `case` pattern list and its `)`: `a)`, `a|b)`, `a | b)`, `( a )`. One
# follows `case WORD in` where a command starts, `;;`, `;;&` or `;&`, or
# starts a line, as in a `case` written over several.
sh_case_pattern <- paste0(
  "(?:^|;;&?|;&|",
  sh_start,
  "\\s*",
  sh_reserved,
  "case\\s+",
  sh_case_word,
  "\\s+in\\s)\\s*\\(?\\s*",
  sh_case_word,
  "(?:\\s*\\|\\s*",
  sh_case_word,
  ")*\\s*\\)"
)
# sh's command head: where a command starts, or a `case` pattern's `)`,
# then any reserved words.
sh_head <- paste0("(?:", sh_case_pattern, "|", sh_start, ")\\s*", sh_reserved)
# What stands before a pin on a masked line, from sh's command head to the
# token: an assignment in the current shell or a command's prefix, plain or
# through `export`, `readonly`, `declare` or `typeset`, after other
# assignments.
sh_before_pin <- paste0(
  sh_head,
  "(?:(?:export|readonly|declare|typeset)(?:\\s+[-+]\\w+)*",
  "(?:\\s+\\w+(?:=",
  sh_word,
  ")?)*\\s+|(?:\\w+=",
  sh_word,
  "\\s+)*)$"
)
# What stands before a bare name `export` or `readonly` hands on unchanged:
# `export PANDOC_VERSION`, `export R_X=1 PANDOC_VERSION`.
sh_before_bare <- paste0(
  sh_head,
  "(?:export|readonly)(?:\\s+\\w+(?:=",
  sh_word,
  ")?)*\\s+$"
)
# What follows a token that is set: `=` or `+=` (its first group), then the
# value (its second).
sh_after <- paste0("^(\\+?)=(", sh_word, ")")
# A token, its id the first group.
sh_token <- paste0(pandoc_tag, "(\\d+)")
# A use, which reads the value and sets nothing: `$PANDOC_VERSION`,
# `${PANDOC_VERSION}` and the expansions of it that set nothing
# (`${PANDOC_VERSION:-x}`, `${#PANDOC_VERSION}`), and R's
# `Sys.getenv("PANDOC_VERSION")`. `${PANDOC_VERSION:=x}`,
# `${PANDOC_VERSION=x}` and a subscript are no use.
sh_use_re <- paste0(
  "\\$",
  pandoc_tag,
  "\\d+|\\$\\{[#!]?",
  pandoc_tag,
  "\\d+(?![\\d\\[]|:?=)|Sys\\.getenv\\(\\s*\\\\?[\"']",
  pandoc_tag,
  "\\d+\\\\?[\"']"
)

# Shell words' values: one quoted string loses its quotes.
sh_unquote <- function(words) {
  quoted <- grepl(paste0("^(?:", sh_quoted, ")$"), words, perl = TRUE)
  words[quoted] <- substr(words[quoted], 2L, nchar(words[quoted]) - 1L)
  words
}

# The lines of one script item, `lines`, joined as sh reads them: a line a
# `\` continues is joined to the next without it, and a line that ends
# inside a quote or a substitution is joined to the next with a space, so a
# `#` or a `)` there is read as the part's, not as a comment or a command
# head. A token on a joined line keeps its id, so it still names its own
# line. Only a line holding a quote, a backslash or a substitution can
# continue.
sh_join <- function(lines) {
  out <- character()
  i <- 1L
  while (i <= length(lines)) {
    line <- lines[[i]]
    while (i < length(lines) && grepl(sh_part_re, line, perl = TRUE)) {
      mask <- sh_mask_line(line)
      if (mask$continued) {
        line <- paste0(substr(line, 1L, nchar(line) - 1L), lines[[i + 1L]])
      } else if (mask$open) {
        line <- paste(line, lines[[i + 1L]])
      } else {
        break
      }
      i <- i + 1L
    }
    out <- c(out, line)
    i <- i + 1L
  }
  out
}
# What a line holds before it can continue: a quote, a backquote, a
# backslash or a substitution.
sh_part_re <- "[\"'`\\\\]|[$<>]\\(|\\$\\{"

# What the lines of script items, `lines`, set: the ids they assign a
# literal pin (`pins`, `values`) and the ids of every other mention
# (`set`). This reads the shell by what it allows, not by what it refuses:
# a token is a pin, a use (sh_use_re), a bare name `export` or `readonly`
# hands on, or in a comment, and any other mention, quoted or not, is
# refused. sh sets a variable in more ways than a reader can list (`eval`,
# `let`, `(( ))`, `read`, `declare -n`, `unset`, a quoted declaration
# argument, a wrapper such as `command`), and each is refused without being
# named (SEOR-eeswpcpq). Each line is masked (sh_mask()), the pins found on
# the mask and their values read from the line where the mask has them.
sh_read <- function(lines) {
  found <- list(pins = integer(), values = character(), set = integer())
  lines <- grep(pandoc_tag, lines, fixed = TRUE, value = TRUE)
  if (!length(lines)) {
    return(found)
  }
  masked <- sh_mask(lines)
  lines <- substr(lines, 1L, nchar(masked))
  tokens <- gregexpr(sh_token, masked, perl = TRUE)
  on <- rep(seq_along(masked), vapply(tokens, function(x) sum(x > 0L), 1L))
  start <- as.integer(unlist(lapply(tokens, function(x) x[x > 0L])))
  end <- start -
    1L +
    as.integer(unlist(lapply(tokens, function(x) {
      attr(x, "match.length")[x > 0L]
    })))
  id <- as.integer(substring(masked[on], start + nchar(pandoc_tag), end))
  before <- substr(masked[on], 1L, start - 1L)
  after <- substring(masked[on], end + 1L)
  m <- regexpr(sh_after, after, perl = TRUE)
  set <- m > 0L
  at <- attr(m, "capture.start")
  size <- attr(m, "capture.length")
  value <- sh_unquote(substr(
    substring(lines[on], end + 1L),
    at[, 2L],
    at[, 2L] + size[, 2L] - 1L
  ))
  is <- function(re) grepl(re, before, perl = TRUE)
  pin <- set &
    size[, 1L] == 0L &
    is(sh_before_pin) &
    grepl(pandoc_literal_re, value)
  bare <- !set &
    is(sh_before_bare) &
    grepl("^(?:[\\s;&|)]|$)", after, perl = TRUE)
  found$pins <- id[pin]
  found$values <- value[pin]
  found$set <- setdiff(
    tagged_ids(lines, sh_token),
    c(id[pin | bare], tagged_ids(lines, sh_use_re))
  )
  found
}

# Every PANDOC_VERSION setting in `lines`, a .gitlab-ci.yml text, by line:
# `pins`, a data frame of each pin's line and value in file order;
# `expanded`, the lines in GitLab's expanded form; `unread`, the lines that
# set a value in a spelling this does not read as a pin; `used`, the lines
# that use `$PANDOC_VERSION`, `${PANDOC_VERSION}` or
# `Sys.getenv("PANDOC_VERSION")`; `error`, NULL, or why the text does not
# load. A text that never names PANDOC_VERSION is not parsed.
read_pandoc <- function(lines) {
  read <- list(
    pins = data.frame(line = integer(), value = character()),
    expanded = integer(),
    unread = integer(),
    used = integer(),
    error = NULL
  )
  if (!any(grepl("PANDOC_VERSION", lines, fixed = TRUE))) {
    read$unread <- disguised_lines(lines)
    return(read)
  }
  tagged <- tag_pandoc(lines)
  loaded <- load_ci(tagged)
  if (!is.null(loaded$error)) {
    read$error <- loaded$error
    return(read)
  }
  nodes <- lapply(loaded$docs, ci_nodes)
  pins <- integer()
  values <- character()
  set <- integer()
  expanded <- integer()
  for (key in unlist(lapply(nodes, `[[`, "keys"), recursive = FALSE)) {
    line <- tagged$lines[[tagged$at[[key$id]]]]
    setting <- yaml_setting(key$value, line, key$id)
    if (setting$kind == "pin") {
      pins <- c(pins, key$id)
      values <- c(values, setting$value)
    } else if (setting$kind == "expanded") {
      expanded <- c(expanded, key$id)
    } else if (setting$kind == "unread") {
      set <- c(set, key$id)
    }
  }
  shell <- unlist(lapply(
    strsplit(unlist(lapply(nodes, `[[`, "shell")), "\n", fixed = TRUE),
    sh_join
  ))
  found <- sh_read(shell)
  pins <- c(pins, found$pins)
  values <- c(values, found$values)
  set <- c(set, found$set)
  text <- unlist(strsplit(
    unlist(lapply(nodes, `[[`, "text")),
    "\n",
    fixed = TRUE
  ))
  # Only shell has comments: elsewhere a `#` is text.
  text <- c(
    drop_comments(grep(pandoc_tag, shell, fixed = TRUE, value = TRUE)),
    text
  )
  used <- tagged_ids(text, sh_use_re)
  # An alias or a merge key hands the parser one node in several places.
  keep <- !duplicated(pins)
  rank <- order(pins[keep])
  at <- function(ids) sort(unique(tagged$at[ids]))
  read$pins <- data.frame(
    line = tagged$at[pins[keep][rank]],
    value = values[keep][rank]
  )
  read$expanded <- at(expanded)
  read$unread <- sort(unique(c(
    at(setdiff(set, c(pins, expanded))),
    disguised_lines(lines)
  )))
  read$used <- at(used)
  read
}

# What --pandoc-assignments and --pandoc-unread print for `lines`, several
# .gitlab-ci.yml texts separated by a line holding only a form feed, texts
# and lines numbered from 1: `<text>\t<line>\t<value>` per pin, then
# `unread\t<text>\t<line>` per line read_pandoc() finds unread, the setting
# pinned_pandoc() refuses whatever pin it reads (SEOR-mcstkogt), or in
# GitLab's expanded form at any depth (check-fleet-standard.py finds that
# form itself only in the global and top-level `variables:`, and reports its
# own gap in place of this one there; elsewhere, such as a `rules:` entry's
# variables, this line is the gap it reports), and
# `unloadable\t<text>\t<line>\t<message>` for a text that does not load (line
# 0 when the parser names none). The words keep these apart from a pin's
# line when both flags are given.
pandoc_report <- function(lines, assignments = TRUE, unread = TRUE) {
  reads <- lapply(ci_texts(lines), read_pandoc)
  k <- seq_along(reads)
  c(
    if (assignments) {
      unlist(lapply(k, function(i) {
        sprintf("%d\t%d\t%s", i, reads[[i]]$pins$line, reads[[i]]$pins$value)
      }))
    },
    if (unread) {
      unlist(lapply(k, function(i) {
        error <- reads[[i]]$error
        if (!is.null(error)) {
          line <- if (is.na(error$line)) 0L else error$line
          return(sprintf("unloadable\t%d\t%d\t%s", i, line, error$message))
        }
        sprintf(
          "unread\t%d\t%d",
          i,
          sort(union(reads[[i]]$unread, reads[[i]]$expanded))
        )
      }))
    }
  )
}

# `lines` split into texts at each line holding only a form feed.
ci_texts <- function(lines) {
  text <- cumsum(lines == "\f") + 1L
  keep <- lines != "\f"
  split(lines[keep], factor(text[keep], levels = seq_len(max(text))))
}

# The fix every refusal in pinned_pandoc() names.
pin_fix <- paste0(
  "Write the pin in .gitlab-ci.yml as a plain scalar, ",
  "`PANDOC_VERSION: \"<version>\"`, or as a shell assignment, ",
  "`PANDOC_VERSION=<version>`."
)

# " on line 3" or " on lines 3, 7".
on_lines <- function(lines) {
  paste0(" on line", if (length(lines) > 1L) "s", " ", toString(lines))
}

# Every distinct PANDOC_VERSION value .gitlab-ci.yml assigns. Empty when it
# assigns none; more than one is reported by check_pandoc(). It stops when
# the file does not load as YAML. The expanded form stops here with the fix:
# read as a pin, it is none or the wrong one. So does a `$PANDOC_VERSION`
# with no pin read: CI installs a version this cannot see. So does a value
# set in a spelling this does not read as a pin, whatever pins it does read:
# a global `PANDOC_VERSION: "3.10"` beside a job's `env PANDOC_VERSION=3.9`
# would read as 3.10 while that job installs 3.9 (SEOR-usrdxvbj).
pinned_pandoc <- function(lines) {
  read <- read_pandoc(lines)
  if (!is.null(read$error)) {
    stop(
      ".gitlab-ci.yml does not load as YAML",
      if (!is.na(read$error$line)) on_lines(read$error$line),
      " (",
      read$error$message,
      "), so check-toolchain.R cannot read its PANDOC_VERSION pin. Fix the ",
      "YAML.",
      call. = FALSE
    )
  }
  if (length(read$expanded)) {
    stop(
      ".gitlab-ci.yml writes PANDOC_VERSION in GitLab's expanded form (a ",
      "mapping with `value:`)",
      on_lines(read$expanded),
      ", which check-toolchain.R does not read. ",
      pin_fix,
      call. = FALSE
    )
  }
  pins <- unique(read$pins$value)
  if (!length(pins) && length(read$used)) {
    stop(
      ".gitlab-ci.yml uses $PANDOC_VERSION",
      on_lines(read$used),
      ", but check-toolchain.R reads no pin from it. It reads only a literal ",
      "version set in .gitlab-ci.yml itself: not one in an included file or ",
      "in the CI/CD settings, nor a null, empty, computed or defaulted value. ",
      pin_fix,
      call. = FALSE
    )
  }
  if (length(read$unread)) {
    stop(
      ".gitlab-ci.yml sets PANDOC_VERSION",
      on_lines(read$unread),
      " in a spelling check-toolchain.R does not read. In YAML it reads a ",
      "plain scalar: not a `!reference`, a list, a mapping, a boolean, an ",
      "empty or computed value, a number YAML typed from a block scalar, an ",
      "alias or a line below its key. In a script it reads the pin ",
      "(`PANDOC_VERSION=<version>`, plain or through `export`, `readonly`, ",
      "`declare` or `typeset`), a use (`$PANDOC_VERSION`, ",
      "`${PANDOC_VERSION}`, `${PANDOC_VERSION:-...}`, ",
      "`Sys.getenv(\"PANDOC_VERSION\")`), a bare `export PANDOC_VERSION` or ",
      "`readonly PANDOC_VERSION`, and ",
      "a comment, and refuses any other mention, echoed text included, ",
      "since sh can set the value through it. Whatever pin it ",
      "reads elsewhere, a job that sees this value installs a pandoc this ",
      "check never compares. ",
      pin_fix,
      call. = FALSE
    )
  }
  pins
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
  # First: a pin in GitLab's expanded form stops the run, before CRAN is read.
  pandoc_pin <- read_pandoc_pin(root)
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
    check_pandoc(pandoc_pin, installed_pandoc())
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

  # The pandoc reader's cases. Every fixture is a .gitlab-ci.yml GitLab
  # would load (bar the load-error cases); each case asserts one outcome:
  # the pins read, a pass with no pin, or a refusal naming its line.
  # `cases` counts them.
  cases <- new.env()
  cases$n <- 0L
  case <- function(tag, condition) {
    cases$n <- cases$n + 1L
    expect(tag, condition)
  }
  refusal <- function(lines) {
    tryCatch(
      {
        pinned_pandoc(lines)
        ""
      },
      error = conditionMessage
    )
  }
  # A job whose script items are `...`, YAML text.
  job <- function(..., name = "job") {
    c(paste0(name, ":"), "  script:", paste0("    - ", c(...)))
  }
  # A job whose one script item is a `- |` block of the lines `...`.
  block_job <- function(...) {
    c("job:", "  script:", "    - |", paste0("      ", c(...)))
  }
  global <- c("variables:", "  PANDOC_VERSION: \"3.10\"")
  use <- "curl -o p.deb \"https://example.org/${PANDOC_VERSION}/p.deb\""
  comments <- c(
    "# PANDOC_VERSION: \"9.9\" in a comment is not a pin",
    "#   - PANDOC_VERSION=9.9 neither"
  )
  # A job's variables, `...` YAML text, after the global pin.
  beside <- function(...) {
    c(global, "job:", "  variables:", paste0("    ", c(...)))
  }

  # Read: the pins each fixture assigns, in file order.
  read <- list(
    `variables` = list(c(global, comments, job(use)), "3.10"),
    `comments only` = list(comments, character()),
    `shell` = list(job("PANDOC_VERSION=3.10"), "3.10"),
    `shell, double-quoted` = list(job("PANDOC_VERSION=\"3.10\""), "3.10"),
    `export` = list(job("export PANDOC_VERSION=3.10"), "3.10"),
    `shell, trailing comment` = list(
      job("PANDOC_VERSION=3.10  # the fleet pin"),
      "3.10"
    ),
    `export, single-quoted, comment` = list(
      job("export PANDOC_VERSION='3.10' # pinned"),
      "3.10"
    ),
    `double-quoted` = list("  PANDOC_VERSION: \"3.10\"", "3.10"),
    `single-quoted` = list("  PANDOC_VERSION: '3.10'", "3.10"),
    `double-quoted, comment` = list(
      "  PANDOC_VERSION: \"3.10\"  # the fleet pin",
      "3.10"
    ),
    `single-quoted, comment` = list(
      "  PANDOC_VERSION: '3.10' # pinned",
      "3.10"
    ),
    # Unquoted, YAML reads 3.10 as the float 3.1, and so does this check.
    `unquoted` = list(c("variables:", "  PANDOC_VERSION: 3.10"), "3.1"),
    `unquoted, comment` = list(
      c("variables:", "  PANDOC_VERSION: 3.10 # pinned"),
      "3.1"
    ),
    `unquoted, a whole float` = list(
      c("variables:", "  PANDOC_VERSION: 3.0"),
      "3.0"
    ),
    # An anchored or tagged scalar is a pin: the property is not its text.
    `anchored and tagged` = list(
      c("variables:", "  PANDOC_VERSION: &pv !!str \"3.10\""),
      "3.10"
    ),
    # A shell assignment overrides `variables:` at run time, so a second
    # value is a second pin whichever one CI would install (SEOR-xhyrogfm).
    `variables and shell` = list(
      c(global, job("PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `the same pin twice` = list(
      c(global, job("PANDOC_VERSION=3.10")),
      "3.10"
    ),
    # Only an assignment sh would run counts: not one in a trailing comment
    # (SEOR-xhyrogfm).
    `after a comment` = list(
      job("PANDOC_VERSION=3.10  # was PANDOC_VERSION=3.9"),
      "3.10"
    ),
    `export after another` = list(
      job("export R_X=1 PANDOC_VERSION=3.10"),
      "3.10"
    ),
    `a quoted flow item` = list(
      c("job:", "  before_script: ['PANDOC_VERSION=3.10', 'echo hi']"),
      "3.10"
    ),
    # The builtins that assign in the current shell: check-fleet-standard.py's
    # pin_assignment() reads them too (SEOR-bqroclcz).
    `readonly` = list(job("readonly PANDOC_VERSION=3.10"), "3.10"),
    `declare -x` = list(job("declare -x PANDOC_VERSION=3.10"), "3.10"),
    `typeset -x` = list(job("typeset -x PANDOC_VERSION=3.10"), "3.10"),
    # A command's prefix after `!` is one as after any head.
    `a negated command's prefix` = list(
      c("job:", "  script:", "    - |", "      ! PANDOC_VERSION=3.9 sh x.sh"),
      "3.9"
    ),
    # A command's prefix after a reserved word that starts a command, and
    # after a `case` pattern's `)`, is one as after any head.
    `beside: an if condition's prefix` = list(
      c(global, job("if PANDOC_VERSION=3.9 sh install.sh; then :; fi")),
      c("3.10", "3.9")
    ),
    `beside: an elif condition's prefix` = list(
      c(
        global,
        job("if false; then :; elif PANDOC_VERSION=3.9 sh i.sh; then :; fi")
      ),
      c("3.10", "3.9")
    ),
    `beside: a while condition's prefix` = list(
      c(global, job("while PANDOC_VERSION=3.9 sh poll.sh; do sleep 1; done")),
      c("3.10", "3.9")
    ),
    `beside: a negated until condition's prefix` = list(
      c(global, job("until ! PANDOC_VERSION=3.9 sh poll.sh; do sleep 1; done")),
      c("3.10", "3.9")
    ),
    `beside: time` = list(
      c(global, job("time PANDOC_VERSION=3.9 sh i.sh")),
      c("3.10", "3.9")
    ),
    `beside: a case pattern` = list(
      c(global, job("case x in x) PANDOC_VERSION=3.9 ;; esac")),
      c("3.10", "3.9")
    ),
    `beside: a case pattern on its own line` = list(
      c(
        global,
        block_job(
          "case \"$X\" in",
          "  a|b) PANDOC_VERSION=3.9 sh i.sh ;;",
          "  (c) : ;;",
          "esac"
        )
      ),
      c("3.10", "3.9")
    ),
    # A pattern list may hold spaces around `|` and inside `( ... )`, and a
    # later one follows `;;` (SEOR-lmfgkesn).
    `beside: a case pattern list with spaces` = list(
      c(global, job("case $X in a | b) PANDOC_VERSION=3.9 ;; esac")),
      c("3.10", "3.9")
    ),
    `beside: a parenthesized case pattern with spaces` = list(
      c(global, job("case $X in ( a ) PANDOC_VERSION=3.9 ;; esac")),
      c("3.10", "3.9")
    ),
    `beside: a parenthesized case pattern starting a line` = list(
      c(
        global,
        block_job("case \"$X\" in", "  ( a ) PANDOC_VERSION=3.9 ;;", "esac")
      ),
      c("3.10", "3.9")
    ),
    `beside: a later case pattern list` = list(
      c(global, job("case $X in a) true ;; b | c) PANDOC_VERSION=3.9 ;; esac")),
      c("3.10", "3.9")
    ),
    `beside: quoted and escaped case patterns` = list(
      c(
        global,
        job("case \"$X\" in \"a b\"|c\\ d) PANDOC_VERSION=3.9 ;; esac")
      ),
      c("3.10", "3.9")
    ),
    `beside: a case on a command's output` = list(
      c(global, job("case $(uname -s) in Linux) PANDOC_VERSION=3.9 ;; esac")),
      c("3.10", "3.9")
    ),
    `beside: a case on nested command substitutions` = list(
      c(global, job("case $(a $(b $(c))) in x) PANDOC_VERSION=3.9 ;; esac")),
      c("3.10", "3.9")
    ),
    # An assignment after one whose value holds a substitution, arithmetic,
    # an escape or a defaulted use with a space sets its value too
    # (SEOR-evttkqjl).
    `beside: after an assigned command substitution` = list(
      c(global, job("X=$(date) PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `beside: after assigned arithmetic` = list(
      c(global, job("X=$((1+2)) PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `beside: after an escaped space` = list(
      c(global, job("X=a\\ b PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `beside: after a defaulted use with a space` = list(
      c(global, job("X=${Y:-a b} PANDOC_VERSION=3.9 sh i.sh")),
      c("3.10", "3.9")
    ),
    # Each setting a command makes is read, not only its first.
    `beside: two settings in one export` = list(
      c(global, job("export PANDOC_VERSION=3.10 X PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    # A `#` after a substitution or an escape is the same word's, and
    # `$'...'` escapes its `'` inside a substitution too (SEOR-evttkqjl).
    `beside: a # after a substitution` = list(
      c(global, job("echo $(x)#; PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `beside: a # after an escape` = list(
      c(global, job("echo a\\;#; PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `beside: an escaped quote in a substitution` = list(
      c(global, job("echo $(echo $'a\\'b'); PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    # A `#` that starts a word after `;` starts a comment.
    `beside: a comment after ;` = list(
      c(global, block_job("true;# was; PANDOC_VERSION=3.9")),
      "3.10"
    ),
    # A `#` inside quotes starts no comment.
    `beside: a prefix after a quoted #` = list(
      c(global, block_job("X='a #b' PANDOC_VERSION=3.9 sh i.sh")),
      c("3.10", "3.9")
    ),
    # `eval` of a use sets nothing.
    `beside: eval of a use` = list(
      c(global, job("eval \"echo $PANDOC_VERSION\"")),
      "3.10"
    ),
    `beside: eval of a computed use` = list(
      c(global, job("eval \"$(make-url ${PANDOC_VERSION})\"")),
      "3.10"
    ),
    # A declaration's quoted value is a pin.
    `export, the pin double-quoted` = list(
      c(global, job("export PANDOC_VERSION=\"3.10\"")),
      "3.10"
    ),
    # A `\` continues a line, so the pin on the next is read; a `\` that
    # ends a comment continues nothing (SEOR-eeswpcpq).
    `a pin on a continued line` = list(
      block_job("export \\", "  PANDOC_VERSION=3.10"),
      "3.10"
    ),
    `a pin after a comment ending in a backslash` = list(
      block_job("true # note \\", "PANDOC_VERSION=3.10"),
      "3.10"
    ),
    `beside: a length use` = list(
      c(global, job("echo ${#PANDOC_VERSION}")),
      "3.10"
    ),
    # A quote a line leaves open runs on into the next, so a `#` there is
    # the quote's and the setting after it is read (SEOR-eeswpcpq).
    `beside: after a double quote left open, a # line` = list(
      c(global, block_job("echo \"a", "# \" ; PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `beside: after a single quote left open, a # line` = list(
      c(global, block_job("echo 'a", "# ' ; PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    # A list GitLab never runs as shell is text, not script items.
    `beside: an artifact path naming it` = list(
      c(
        global,
        "job:",
        "  script: [true]",
        "  artifacts:",
        "    paths: [PANDOC_VERSION.txt]"
      ),
      "3.10"
    ),
    `beside: a needed job naming it` = list(
      c(
        global,
        "job:",
        "  script: [true]",
        "  needs: [\"pandoc PANDOC_VERSION\"]"
      ),
      "3.10"
    ),
    # A folded item is one line to sh: here the pin is curl's prefix.
    `a folded item` = list(
      c(
        "job:",
        "  script:",
        "    - >",
        "      PANDOC_VERSION=3.10",
        "      curl x"
      ),
      "3.10"
    ),
    # The parser reads what the line reader refused (SEOR-usrdxvbj): a flow
    # mapping, a quoted or spaced key, an explicit key, a quoted value on
    # the line below the key, an alias to a quoted scalar, a merge key, and
    # an assignment after a quoted value with a space.
    `flow variables` = list(
      c("job:", "  variables: {PANDOC_VERSION: \"3.9\"}"),
      "3.9"
    ),
    `beside: flow variables` = list(
      c(global, "job:", "  variables: {PANDOC_VERSION: \"3.9\"}"),
      c("3.10", "3.9")
    ),
    `beside: a quoted key` = list(
      beside("\"PANDOC_VERSION\": \"3.9\""),
      c("3.10", "3.9")
    ),
    `beside: a single-quoted key` = list(
      beside("'PANDOC_VERSION': '3.9'"),
      c("3.10", "3.9")
    ),
    `beside: a space before the colon` = list(
      beside("PANDOC_VERSION : \"3.9\""),
      c("3.10", "3.9")
    ),
    `beside: an explicit key` = list(
      beside("? PANDOC_VERSION", ": \"3.9\""),
      c("3.10", "3.9")
    ),
    `beside: a quoted value below the key` = list(
      beside("PANDOC_VERSION:", "  \"3.9\""),
      c("3.10", "3.9")
    ),
    `an alias to a quoted scalar` = list(
      c(".pin: &pv \"3.10\"", "variables:", "  PANDOC_VERSION: *pv"),
      "3.10"
    ),
    `a merge key` = list(
      c(
        ".vars: &vars",
        "  PANDOC_VERSION: \"3.10\"",
        "job:",
        "  variables:",
        "    <<: *vars",
        "  script:",
        paste0("    - ", use)
      ),
      "3.10"
    ),
    `beside: after a quoted value with a space` = list(
      c(global, job("export X=\"a b\" PANDOC_VERSION=3.9")),
      c("3.10", "3.9")
    ),
    `beside: a prefix after a quoted value` = list(
      c(global, job("X='a b' PANDOC_VERSION=3.9 sh install.sh")),
      c("3.10", "3.9")
    ),
    # An own key beside a `<<` merge that brings in the same key overrides
    # it, as YAML allows: two keys of one mapping, but no duplicate.
    `a merge key and an own key` = list(
      c(
        ".vars: &vars",
        "  PANDOC_VERSION: \"3.10\"",
        "job:",
        "  variables:",
        "    <<: *vars",
        "    PANDOC_VERSION: \"3.10\""
      ),
      "3.10"
    ),
    # An anchor's or an alias's name keeps its text, so the two still match.
    `an anchor named after the pin` = list(
      c(
        ".x: &a-PANDOC_VERSION \"3.10\"",
        "variables:",
        "  PANDOC_VERSION: *a-PANDOC_VERSION"
      ),
      "3.10"
    ),
    # After `&&` with no space, the name is an assignment, not an alias.
    `beside: a prefix after && with no space` = list(
      c(global, job("true &&PANDOC_VERSION=3.9 sh i.sh")),
      c("3.10", "3.9")
    ),
    # An anchored list counts whether a job uses it or not, and once
    # however many jobs do.
    `an anchored list` = list(
      c(
        ".install: &install",
        "  - PANDOC_VERSION=\"3.10\"",
        "  - echo hi",
        "a:",
        "  before_script: [*install]",
        "b:",
        "  before_script: [*install]"
      ),
      "3.10"
    ),
    # A `spec:` header document, then the config. An input the header
    # declares is no variable, whatever its name and however it is written.
    `a header document` = list(
      c("spec:", "  inputs: {}", "---", global, job(use)),
      "3.10"
    ),
    `a header document declaring a PANDOC_VERSION input` = list(
      c(
        "spec:",
        "  inputs:",
        "    PANDOC_VERSION: {description: pin, default: \"3.10\"}",
        "---",
        global,
        job(use)
      ),
      "3.10"
    ),
    # Nor is a duplicate input it declares refused, whatever its name
    # (SEOR-lmfgkesn, SEOR-evttkqjl).
    `a header document declaring an input twice` = list(
      c(
        "spec:",
        "  inputs:",
        "    PANDOC_VERSION: {default: \"3.9\"}",
        "    PANDOC_VERSION: {default: \"3.10\"}",
        "---",
        global,
        job(use)
      ),
      "3.10"
    ),
    `a header document declaring another input twice` = list(
      c(
        "spec:",
        "  inputs:",
        "    stage: {default: test}",
        "    stage: {default: build}",
        "---",
        global,
        job(use)
      ),
      "3.10"
    ),
    # Whatever follows it: here a trailing `---` (SEOR-evttkqjl).
    `a header document, then a trailing ---` = list(
      c(
        "spec:",
        "  inputs:",
        "    PANDOC_VERSION: {default: \"3.10\"}",
        "---",
        global,
        job(use),
        "---"
      ),
      "3.10"
    ),
    # A number out of R's range loads as NA with a warning that changes
    # nothing read here, so it is no load error.
    `numbers out of range elsewhere` = list(
      c(
        "variables:",
        "  PROJECT: 12345678901",
        "  MASK: 0x7FFFFFFFFF",
        "  HUGE: 1.0e+400",
        "  PANDOC_VERSION: \"3.10\""
      ),
      "3.10"
    ),
    # Beside a readable pin, lines that set no second value pass with that
    # pin alone: a null key, however spelled (bare, `~`, `null`, quoted,
    # spaced or explicit), a use, a defaulted use, an export with no value,
    # and the name in a comment or in R's `Sys.getenv()`.
    `beside: a bare key` = list(beside("PANDOC_VERSION:"), "3.10"),
    `beside: a bare key, a sibling below` = list(
      beside("PANDOC_VERSION:", "R_VERSION: \"4.6.1\""),
      "3.10"
    ),
    `beside: ~` = list(beside("PANDOC_VERSION: ~"), "3.10"),
    `beside: null` = list(beside("PANDOC_VERSION: null"), "3.10"),
    `beside: a quoted bare key` = list(beside("\"PANDOC_VERSION\":"), "3.10"),
    `beside: a spaced bare key` = list(beside("PANDOC_VERSION :"), "3.10"),
    `beside: an explicit bare key` = list(beside("? PANDOC_VERSION"), "3.10"),
    `beside: a defaulted use` = list(
      c(global, job("echo \"pandoc ${PANDOC_VERSION:-3.9}\"")),
      "3.10"
    ),
    `beside: export with no value` = list(
      c(global, job("export PANDOC_VERSION")),
      "3.10"
    ),
    `beside: Sys.getenv` = list(
      c(global, job("Rscript -e 'Sys.getenv(\"PANDOC_VERSION\")'")),
      "3.10"
    ),
    `beside: in a comment` = list(
      beside("# PANDOC_VERSION: {value: \"3.9\"}"),
      "3.10"
    ),
    `beside: a variable's value` = list(
      beside("SETUP: \"PANDOC_VERSION=3.9\""),
      "3.10"
    ),
    # A bare key whose next line is no mapping of its own is not the
    # expanded form: a sibling key, even one named `value`, or nothing.
    # With nothing deeper below it, its value is null and it passes.
    `bare key, a sibling value:` = list(
      c("variables:", "  PANDOC_VERSION:", "  value: \"3.10\""),
      character()
    ),
    `bare key, a sibling's expanded form` = list(
      c(
        "variables:",
        "  PANDOC_VERSION:",
        "  R_VERSION:",
        "    value: \"4.6.1\""
      ),
      character()
    ),
    `bare key alone` = list(c("variables:", "  PANDOC_VERSION:"), character()),
    # A text that never names PANDOC_VERSION is not parsed: CI that does not
    # load is GitLab's to report, not this check's.
    `no PANDOC_VERSION, not YAML` = list(c("a: [1, 2", "b: *x"), character())
  )
  for (name in names(read)) {
    lines <- read[[name]][[1L]]
    tag <- paste("pandoc-read:", name)
    case(tag, !nzchar(refusal(lines)))
    case(tag, identical(pinned_pandoc(lines), read[[name]][[2L]]))
  }

  # Refused: each fixture stops, naming the line and the plain scalar.
  # GitLab's expanded form, a `value:` mapping, block or flow, keys in any
  # order: read as a pin it would be none (block) or the version
  # `{value: "3.10"}` (flow) (SEOR-zvnrwaku).
  expanded <- "GitLab's expanded form"
  # `$PANDOC_VERSION` with no pin read: CI would install a pin this cannot
  # see (SEOR-mbiwrxql).
  no_pin <- "reads no pin"
  # A value set in a spelling this does not read, whatever pin it reads
  # beside it: read as 3.10, the job would install 3.9 (SEOR-usrdxvbj).
  unread <- "in a spelling check-toolchain.R"
  block <- c("variables:", "  PANDOC_VERSION:", "    value: \"3.10\"")
  flow <- c("variables:", "  PANDOC_VERSION: {value: \"3.10\"}")
  refused <- list(
    `expanded, block` = list(block, expanded, 2L),
    `expanded, flow` = list(flow, expanded, 2L),
    `expanded, block, description first` = list(
      c(
        "variables:",
        "  PANDOC_VERSION:  # the fleet pin",
        "    # pinned",
        "    description: the fleet's pandoc",
        "    expand: false",
        "    value: \"3.10\""
      ),
      expanded,
      2L
    ),
    `expanded, flow, description first` = list(
      c("variables:", "  PANDOC_VERSION: {description: pin, value: \"3.10\"}"),
      expanded,
      2L
    ),
    `expanded, flow, anchored` = list(
      c("variables:", "  PANDOC_VERSION: &pv {value: 3.10}"),
      expanded,
      2L
    ),
    `expanded, block, anchored and tagged` = list(
      c("variables:", "  PANDOC_VERSION: &pv !!map", "    value: \"3.10\""),
      expanded,
      2L
    ),
    `expanded, behind a quoted key` = list(
      c("variables:", "  \"PANDOC_VERSION\": {value: \"3.10\"}"),
      expanded,
      2L
    ),
    `expanded, an alias to it` = list(
      c(".pin: &pv {value: \"3.10\"}", "variables:", "  PANDOC_VERSION: *pv"),
      expanded,
      3L
    ),
    `use, no pin` = list(job(use), no_pin, 3L),
    `use, a bare $PANDOC_VERSION` = list(
      job("echo $PANDOC_VERSION"),
      no_pin,
      3L
    ),
    `use, an included file` = list(
      c("include:", "  - local: ci/pandoc.yml", job(use)),
      no_pin,
      5L
    ),
    `use, Sys.getenv` = list(
      c(
        "include:",
        "  - local: ci/pandoc.yml",
        job("Rscript -e 'Sys.getenv(\"PANDOC_VERSION\")'")
      ),
      no_pin,
      5L
    ),
    `use, after a quoted #` = list(
      block_job("X='a #b' curl -o p.deb \"x/${PANDOC_VERSION}\""),
      no_pin,
      4L
    ),
    # `&` and `*` inside a word start no anchor or alias.
    `use, after & in a query` = list(
      job("curl -o p.deb \"https://x.org/dl?os=linux&v=$PANDOC_VERSION\""),
      no_pin,
      3L
    ),
    `use, after * in a glob` = list(
      job("dpkg -i pandoc*$PANDOC_VERSION.deb"),
      no_pin,
      3L
    ),
    `use, in a variable's value` = list(
      c("variables:", "  URL: \"https://example.org/${PANDOC_VERSION}\""),
      no_pin,
      2L
    ),
    `use, computed` = list(
      job("PANDOC_VERSION=$(cat .pandoc-version)", use),
      no_pin,
      4L
    ),
    `use, empty` = list(
      c("variables:", "  PANDOC_VERSION: \"\"", job(use)),
      no_pin,
      5L
    ),
    `use, null` = list(
      c("variables:", "  PANDOC_VERSION: null", job(use)),
      no_pin,
      5L
    ),
    `use, a mapping below the key` = list(
      c("variables:", "  PANDOC_VERSION:", "    default: \"3.10\"", job(use)),
      no_pin,
      6L
    ),
    `computed` = list(
      job("PANDOC_VERSION=$(cat .pandoc-version)"),
      unread,
      3L
    ),
    `empty` = list(c("variables:", "  PANDOC_VERSION: \"\""), unread, 2L),
    `a mapping below the key` = list(
      c("variables:", "  PANDOC_VERSION:", "    default: \"3.10\""),
      unread,
      2L
    ),
    `beside: empty` = list(beside("PANDOC_VERSION: \"\""), unread, 5L),
    `beside: computed` = list(
      c(global, job("PANDOC_VERSION=$(cat .pandoc-version)")),
      unread,
      5L
    ),
    `beside: computed beside a read one` = list(
      c(global, job("PANDOC_VERSION=3.10; PANDOC_VERSION=${OLD}")),
      unread,
      5L
    ),
    `beside: env prefix` = list(
      c(global, job("env PANDOC_VERSION=3.9 sh install.sh")),
      unread,
      5L
    ),
    `beside: local` = list(
      c(global, job("f() { local PANDOC_VERSION=3.9; }")),
      unread,
      5L
    ),
    `beside: assign default` = list(
      c(global, job("': \"${PANDOC_VERSION:=3.9}\"'")),
      unread,
      5L
    ),
    `beside: a mapping below the key` = list(
      beside("PANDOC_VERSION:", "  default: \"3.9\""),
      unread,
      5L
    ),
    # The spellings the line reader took for a wrong pin (SEOR-bqroclcz).
    # The yaml package types a block scalar as plain text: 3.1, where
    # GitLab reads "3.10".
    `beside: a block scalar` = list(
      beside("PANDOC_VERSION: >-", "  3.10"),
      unread,
      5L
    ),
    `beside: a block scalar below the key` = list(
      beside("PANDOC_VERSION:", "  >-", "    3.9"),
      unread,
      5L
    ),
    `beside: a literal block` = list(
      beside("PANDOC_VERSION: |", "  3.9"),
      unread,
      5L
    ),
    `beside: an unquoted value below the key` = list(
      beside("PANDOC_VERSION:", "  3.9"),
      unread,
      5L
    ),
    `beside: an explicit key, unquoted` = list(
      beside("? PANDOC_VERSION", ": 3.9"),
      unread,
      5L
    ),
    `beside: an alias to an unquoted scalar` = list(
      c(
        ".old: &old 3.9",
        global,
        "job:",
        "  variables:",
        "    PANDOC_VERSION: *old"
      ),
      unread,
      6L
    ),
    `beside: a !reference` = list(
      c(
        ".v:",
        "  PANDOC_VERSION: \"3.10\"",
        "job:",
        "  variables:",
        "    PANDOC_VERSION: !reference [.v, PANDOC_VERSION]"
      ),
      unread,
      5L
    ),
    # Untagged, `!reference .pin` would read as the pin ".pin".
    `beside: a !reference to one name` = list(
      beside("PANDOC_VERSION: !reference .pin"),
      unread,
      5L
    ),
    `beside: a list` = list(
      beside("PANDOC_VERSION: [\"3.9\", \"3.10\"]"),
      unread,
      5L
    ),
    `beside: a boolean` = list(beside("PANDOC_VERSION: yes"), unread, 5L),
    # Out of R's integer range, the number loads as NA: no pin.
    `beside: a number out of range` = list(
      beside("PANDOC_VERSION: 12345678901"),
      unread,
      5L
    ),
    `beside: a computed variable` = list(
      beside("PANDOC_VERSION: \"3.9${SUFFIX}\""),
      unread,
      5L
    ),
    `beside: a suffix after the literal` = list(
      c(global, job("PANDOC_VERSION=3.10${SUFFIX}")),
      unread,
      5L
    ),
    # An assignment in arithmetic sets the variable in the shell that runs
    # it, though the mask hides it (SEOR-evttkqjl).
    `beside: arithmetic assigning` = list(
      c(global, job("echo $((PANDOC_VERSION=3))")),
      unread,
      5L
    ),
    `beside: arithmetic incrementing in quotes` = list(
      c(global, job("echo \"$((PANDOC_VERSION++))\"")),
      unread,
      5L
    ),
    # A `spec:` alone, no `---` after it, is no header: its keys are read.
    `a spec: with no document after it` = list(
      c("spec:", "  inputs:", "    PANDOC_VERSION: {default: \"3.9\"}"),
      unread,
      3L
    ),
    # A `#` in a string that is not shell is text, not a comment.
    `use, after a # in a variable's value` = list(
      c("variables:", "  NOTE: \"pandoc #42: ${PANDOC_VERSION}\""),
      no_pin,
      2L
    ),
    `beside: an append` = list(
      c(global, job("PANDOC_VERSION+=.1")),
      unread,
      5L
    ),
    # The settings the line reader missed (SEOR-bqroclcz).
    `beside: eval` = list(
      c(global, job("eval PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: eval, quoted` = list(
      c(global, job("eval \"PANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: eval of read` = list(
      c(global, job("eval \"read -r PANDOC_VERSION < v\"")),
      unread,
      5L
    ),
    `beside: eval of unset` = list(
      c(global, job("eval \"unset PANDOC_VERSION\"")),
      unread,
      5L
    ),
    `beside: eval of getopts` = list(
      c(global, job("eval \"getopts v PANDOC_VERSION\"")),
      unread,
      5L
    ),
    `beside: env with an option's argument` = list(
      c(global, job("env -u X PANDOC_VERSION=3.9 sh install.sh")),
      unread,
      5L
    ),
    `beside: for` = list(
      c(global, job("for PANDOC_VERSION in 3.9; do true; done")),
      unread,
      5L
    ),
    `beside: read` = list(
      c(global, job("read -r PANDOC_VERSION < .pandoc-version")),
      unread,
      5L
    ),
    `beside: printf -v` = list(
      c(global, job("printf -v PANDOC_VERSION '%s' 3.9")),
      unread,
      5L
    ),
    # A `)` that ends no `case` pattern this reads, nor a `$(...)`: an `in`
    # that is an argument starts no pattern. The setting after it is
    # refused, not read as a pin nor passed (SEOR-lmfgkesn).
    `beside: a ) after an echoed in` = list(
      c(global, job("echo logged in x) PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: a ) after an echoed case` = list(
      c(global, job("echo case x in a) PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: eval after a stray )` = list(
      c(global, job("echo logged in x) eval PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    # A declaration's quoted argument and `let` set the variable, though no
    # setting pattern reads them (SEOR-qakbchzg).
    `beside: export, the argument double-quoted` = list(
      c(global, job("export \"PANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: declare -x, the argument single-quoted` = list(
      c(global, job("declare -x 'PANDOC_VERSION=3.9'")),
      unread,
      5L
    ),
    `beside: export, the argument ANSI-C quoted` = list(
      c(global, job("export $'PANDOC_VERSION=3.9'")),
      unread,
      5L
    ),
    `beside: local, the argument double-quoted` = list(
      c(global, job("local \"PANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: readonly, the value quoted with its =` = list(
      c(global, job("readonly PANDOC_VERSION\"=3.9\"")),
      unread,
      5L
    ),
    `beside: typeset, the = escaped` = list(
      c(global, job("typeset PANDOC_VERSION\\=3.9")),
      unread,
      5L
    ),
    `beside: env, the argument single-quoted` = list(
      c(global, job("env 'PANDOC_VERSION=3.9' sh install.sh")),
      unread,
      5L
    ),
    `beside: export after an assignment` = list(
      c(global, job("X=1 export PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: eval after an assignment` = list(
      c(global, job("X=1 eval PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: let` = list(
      c(global, job("let PANDOC_VERSION=3")),
      unread,
      5L
    ),
    `beside: let appending` = list(
      c(global, job("let PANDOC_VERSION+=1")),
      unread,
      5L
    ),
    `beside: let, quoted and spaced` = list(
      c(global, job("let x=1 'PANDOC_VERSION = 3'")),
      unread,
      5L
    ),
    # Text that names the variable without setting it is refused too:
    # echoed, printed, in a here-document, in arithmetic or a `let` that
    # reads it, or in another variable's value. Such text can be written to
    # a file and sourced, and a reader that passes it has to tell it from a
    # setting (SEOR-eeswpcpq).
    `after echo text` = list(
      job("echo \"PANDOC_VERSION=3.9 is gone\"; PANDOC_VERSION=3.10"),
      unread,
      3L
    ),
    `after printf text` = list(
      job(paste(
        "printf 'PANDOC_VERSION=%s\\n' 3.9 &&",
        "export PANDOC_VERSION=3.10"
      )),
      unread,
      3L
    ),
    `after quoted text with a ;` = list(
      job("echo \"pinned; PANDOC_VERSION=3.9 was old\" && PANDOC_VERSION=3.10"),
      unread,
      3L
    ),
    `after quoted text with export` = list(
      job("echo \"use export PANDOC_VERSION=3.9\"; PANDOC_VERSION=3.10"),
      unread,
      3L
    ),
    `beside: a here-document's JSON` = list(
      c(
        global,
        "job:",
        "  script:",
        "    - |",
        "      cat <<EOF > v.json",
        "      {\"PANDOC_VERSION\": \"3.9\"}",
        "      EOF"
      ),
      unread,
      7L
    ),
    `beside: after a command substitution` = list(
      c(global, job("echo $(date) PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: after a process substitution` = list(
      c(global, job("echo $(cat <(ls)) PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: after a quoted ) in a substitution` = list(
      c(global, job("echo $(printf \"%s\" \")\") PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: after an escaped )` = list(
      c(global, job("echo :\\) PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: after arithmetic` = list(
      c(global, job("echo $((1 + 2)) PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: after nested arithmetic` = list(
      c(global, job("echo $(( (1 + 2) * 3 )) PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: an echoed quoted export` = list(
      c(global, job("echo export \"PANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: arithmetic comparing` = list(
      c(global, job("echo $((PANDOC_VERSION == 3))")),
      unread,
      5L
    ),
    `beside: echoed` = list(
      c(global, job("echo PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: echoed flow mapping` = list(
      c(
        global,
        "job:",
        "  script:",
        "    - |",
        "      echo {PANDOC_VERSION: 3}"
      ),
      unread,
      6L
    ),
    `beside: echoed if` = list(
      c(global, job("echo if PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: echoed local` = list(
      c(global, job("echo local PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: echoed YAML` = list(
      c(global, job("'echo \"PANDOC_VERSION: 3.9\"'")),
      unread,
      5L
    ),
    `beside: echoed YAML after a comma` = list(
      c(
        global,
        "job:",
        "  script:",
        "    - |",
        "      echo $X, PANDOC_VERSION: 3.9"
      ),
      unread,
      6L
    ),
    `beside: export of a quoted value naming it` = list(
      c(global, job("export X=\"PANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: export of a value holding the text` = list(
      c(global, job("export X=\"a PANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: let comparing` = list(
      c(global, job("let 'x = PANDOC_VERSION == 3'")),
      unread,
      5L
    ),
    `beside: let of a use` = list(
      c(global, job("let \"x = PANDOC_VERSION + 1\"")),
      unread,
      5L
    ),
    `in a flow sequence` = list(
      c(
        "job:",
        "  before_script: [PANDOC_VERSION=3.10, echo PANDOC_VERSION=3.9]"
      ),
      unread,
      2L
    ),
    # The spellings the denylist missed (SEOR-eeswpcpq): a continued line,
    # a wrapper, an arithmetic command, a name split, escaped or broken
    # across a YAML line, and the builtins no pattern listed.
    `beside: export, its argument on a continued line` = list(
      c(global, block_job("export \\", "  \"PANDOC_VERSION=3.9\"")),
      unread,
      7L
    ),
    `beside: let, its argument on a continued line` = list(
      c(global, block_job("let \\", "  PANDOC_VERSION=3")),
      unread,
      7L
    ),
    `beside: command export` = list(
      c(global, job("command export PANDOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: builtin export, quoted` = list(
      c(global, job("builtin export \"PANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: nohup env, quoted` = list(
      c(global, job("nohup env 'PANDOC_VERSION=3.9' sh install.sh")),
      unread,
      5L
    ),
    `beside: an arithmetic command, spaced` = list(
      c(global, job("(( PANDOC_VERSION = 3 ))")),
      unread,
      5L
    ),
    `beside: an arithmetic command` = list(
      c(global, job("((PANDOC_VERSION=3))")),
      unread,
      5L
    ),
    `beside: an arithmetic command incrementing` = list(
      c(global, job("(( PANDOC_VERSION++ ))")),
      unread,
      5L
    ),
    `beside: a name split by quotes` = list(
      c(global, job("export PANDOC_\"VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: a name split by an escape` = list(
      c(global, job("export PAN\\DOC_VERSION=3.9")),
      unread,
      5L
    ),
    `beside: an escaped = in $'...'` = list(
      c(global, job("export $'PANDOC_VERSION\\x3d3.9'")),
      unread,
      5L
    ),
    `beside: an escaped name in $'...'` = list(
      c(global, job("export $'PANDOC_\\x56ERSION=3.9'")),
      unread,
      5L
    ),
    `beside: a YAML newline escape before the name` = list(
      c(global, job("\"echo x\\nPANDOC_VERSION=3.9\"")),
      unread,
      5L
    ),
    `beside: a name broken across a YAML line` = list(
      c(
        global,
        "job:",
        "  script:",
        "    - \"export PANDOC_\\",
        "      VERSION=3.9\""
      ),
      unread,
      5L
    ),
    `beside: declare -n` = list(
      c(global, job("declare -n r=PANDOC_VERSION")),
      unread,
      5L
    ),
    `beside: unset` = list(
      c(global, job("unset PANDOC_VERSION")),
      unread,
      5L
    ),
    `beside: a bare local` = list(
      c(global, job("local PANDOC_VERSION")),
      unread,
      5L
    ),
    `beside: mapfile` = list(
      c(global, job("mapfile -t PANDOC_VERSION < v")),
      unread,
      5L
    ),
    `beside: trap` = list(
      c(global, job("trap 'PANDOC_VERSION=3.9' EXIT")),
      unread,
      5L
    ),
    `beside: a subscripted use` = list(
      c(global, job("echo ${PANDOC_VERSION[0]}")),
      unread,
      5L
    ),
    # From the review of the allowlist (SEOR-eeswpcpq): a file whose only
    # mention is disguised, and a name escaped for YAML and then for
    # `$'...'`.
    `only a disguised name` = list(
      c("job:", "  script:", "    - export PANDOC_\"VERSION=3.9\""),
      unread,
      3L
    ),
    `beside: a name escaped twice` = list(
      c(global, job("\"export $'PANDOC_\\\\x56ERSION=3.9'\"")),
      unread,
      5L
    ),
    # A file that does not load stops, naming the line the parser names.
    `not YAML` = list(
      c(global, "job:", "  script: [a, b"),
      "does not load as YAML",
      4L
    ),
    # The parser cannot see a duplicate PANDOC_VERSION key once the two are
    # renamed apart, so the reader refuses it, naming the second: GitLab
    # keeps the last, here null, which no pin read as 3.10 would show.
    `a duplicate PANDOC_VERSION key` = list(
      c("variables:", "  PANDOC_VERSION: \"3.10\"", "  PANDOC_VERSION:"),
      "does not load as YAML",
      3L
    ),
    # Of several problems, the earliest in file order is named: here a
    # duplicate before a document, or a line, the parser cannot load. A
    # duplicate in the config after a `spec:` header still counts
    # (SEOR-lmfgkesn).
    `a duplicate key, then a document that does not load` = list(
      c(global, "  PANDOC_VERSION: \"3.9\"", "---", "x: [1"),
      "does not load as YAML",
      3L
    ),
    `a duplicate key before a parser error` = list(
      c(global, "  PANDOC_VERSION: \"3.9\"", "x: [1"),
      "does not load as YAML",
      3L
    ),
    # Also in a mapping the parser has not closed where an error or a
    # warning stops it (SEOR-evttkqjl).
    `a duplicate key in a mapping a parser error leaves open` = list(
      c(global, "  PANDOC_VERSION: \"3.9\"", "  y: [1"),
      "does not load as YAML",
      3L
    ),
    `a duplicate key in a block mapping the parser cannot close` = list(
      c(global, "  PANDOC_VERSION: \"3.9\"", "  x: y", "  - a"),
      "does not load as YAML",
      3L
    ),
    `a duplicate key in a mapping a warning leaves open` = list(
      c(global, "  PANDOC_VERSION: \"3.9\"", "  y: *nothing"),
      "does not load as YAML",
      3L
    ),
    `a duplicate key after a header` = list(
      c(
        "spec:",
        "  inputs:",
        "    PANDOC_VERSION: {default: \"3.9\"}",
        "    PANDOC_VERSION: {default: \"3.10\"}",
        "---",
        global,
        "  PANDOC_VERSION: \"3.9\""
      ),
      "does not load as YAML",
      8L
    ),
    `a duplicate key` = list(
      c(global, "job: 1", "job: 2"),
      "does not load as YAML",
      NA
    ),
    `an unknown alias` = list(
      c(global, "job:", "  variables: *nothing"),
      "does not load as YAML",
      NA
    ),
    # A warning that changes what is read still fails the load: the yaml
    # package loads a `!!int` it cannot read as NA.
    `a tag its value cannot hold` = list(
      c(global, "job:", "  script: [!!int PANDOC_VERSION=3.9]"),
      "does not load as YAML",
      NA
    )
  )
  for (name in names(refused)) {
    why <- refusal(refused[[name]][[1L]])
    tag <- paste("pandoc-refused:", name)
    case(tag, grepl(refused[[name]][[2L]], why, fixed = TRUE))
    at <- refused[[name]][[3L]]
    if (!is.na(at)) {
      case(tag, grepl(sprintf("on line %d[, ]", at), why, perl = TRUE))
    }
    if (!grepl("does not load", why, fixed = TRUE)) {
      case(tag, grepl("PANDOC_VERSION: \"<version>\"", why, fixed = TRUE))
    }
  }
  # Without the yaml package the check stops in one line, naming it.
  stops <- function(...) {
    tryCatch(
      {
        need_yaml(...)
        ""
      },
      error = conditionMessage
    )
  }
  why <- stops("yaml.not.installed")
  case("pandoc-yaml-missing", grepl("yaml.not.installed", why, fixed = TRUE))
  case("pandoc-yaml-missing", grepl("install.packages", why, fixed = TRUE))
  # So does one with a yaml too old for the arguments this reader passes.
  why <- stops(minimum = "999.0")
  case("pandoc-yaml-old", grepl("yaml 999.0 or later, but", why, fixed = TRUE))
  case("pandoc-yaml-old", grepl("install.packages", why, fixed = TRUE))

  # check-fleet-standard.py reads the pin through --pandoc-assignments and
  # --pandoc-unread: these texts, separated by a form feed and numbered as
  # in their files, are what it judges.
  assignments <- function(lines) pandoc_report(lines, unread = FALSE)
  unread_lines <- function(lines) pandoc_report(lines, assignments = FALSE)
  case(
    "pandoc-report: assignments",
    identical(
      assignments(c(
        global,
        "  # PANDOC_VERSION=9.9",
        job("export PANDOC_VERSION=3.9"),
        "\f",
        "x: 1",
        "\f",
        "  - PANDOC_VERSION=3.8"
      )),
      c("1\t2\t3.10", "1\t6\t3.9", "3\t1\t3.8")
    )
  )
  case("pandoc-report: none", !length(assignments(comments)))
  # No pin is printed for the expanded form, nor for an alias to it; a
  # plain pin beside it still is.
  case(
    "pandoc-report: expanded",
    identical(
      assignments(c(block, "\f", flow, job("PANDOC_VERSION=3.9"))),
      "2\t5\t3.9"
    )
  )
  case(
    "pandoc-report: expanded, anchored",
    !length(assignments(c("variables:", "  PANDOC_VERSION: &pv {value: 3.10}")))
  )
  # It prints only what it reads, and never stops: a value set in a
  # spelling it does not read, which pinned_pandoc() refuses, prints no
  # line.
  case(
    "pandoc-report: unread key",
    identical(assignments(beside("PANDOC_VERSION: \"\"")), "1\t2\t3.10")
  )
  # --pandoc-unread prints that refusal's lines, numbered as above, so the
  # fleet checker reports what this check stops on (SEOR-mcstkogt), and a
  # text that does not load, with the line the parser names.
  case(
    "pandoc-report: unread",
    identical(
      unread_lines(c(
        beside("PANDOC_VERSION: \"\""),
        "\f",
        "x: 1",
        "\f",
        job("PANDOC_VERSION=$(cat v)", "env PANDOC_VERSION=3.8 sh i.sh"),
        "\f",
        global,
        "x: [1"
      )),
      c(
        "unread\t1\t5",
        "unread\t3\t3",
        "unread\t3\t4",
        paste0(
          "unloadable\t4\t3\tParser error: while parsing a flow sequence at ",
          "line 3, column 4 did not find expected ',' or ']' at line 4, ",
          "column 1"
        )
      )
    )
  )
  # The parser counts a later document's lines from its `---`; the message
  # counts them from the top of the file, as the line does.
  case(
    "pandoc-report: unloadable, a later document",
    identical(
      unread_lines(c(global, "---", "x: [1")),
      paste0(
        "unloadable\t1\t4\tParser error: while parsing a flow sequence at ",
        "line 4, column 4 did not find expected ',' or ']' at line 5, ",
        "column 1"
      )
    )
  )
  case(
    "pandoc-report: unread, none",
    !length(unread_lines(c(global, "\f", read$variables[[1L]])))
  )
  # The expanded form, which pinned_pandoc() stops on, prints its line at
  # any depth: check-fleet-standard.py finds it itself only in the global
  # and top-level `variables:`, so a `rules:` entry's is its gap this way.
  case(
    "pandoc-report: unread, the expanded form",
    identical(
      unread_lines(c(
        block,
        "\f",
        global,
        "job:",
        "  rules:",
        "    - if: $X",
        "      variables: {PANDOC_VERSION: {value: \"3.9\"}}",
        "  script: [echo]"
      )),
      c("unread\t1\t2", "unread\t2\t6")
    )
  )
  case(
    "pandoc-report: both, assignments first",
    identical(
      pandoc_report(c(beside("PANDOC_VERSION: \"\""), "\f", global)),
      c("1\t2\t3.10", "2\t2\t3.10", "unread\t1\t5")
    )
  )

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

  paste0(
    "check-toolchain self-test: PASS (5 roxygen cases, 5 build-version ",
    "cases, 9 CRAN-version cases, ",
    cases$n + 7L,
    " pandoc cases)\n"
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
# --pandoc-unread reads the same and prints `unread\t<text>\t<line>` per line
# that sets a value in a spelling the reader does not read or in GitLab's
# expanded form, and
# `unloadable\t<text>\t<line>\t<message>` for a text that does not load as
# YAML. With both flags, one run prints both kinds of line, assignments
# first, from one parse of each text (pandoc_report()).
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  flags <- c("--pandoc-assignments", "--pandoc-unread") %in% args
  if (any(flags)) {
    lines <- readLines(file("stdin"), warn = FALSE, encoding = "UTF-8")
    # Only a text that names PANDOC_VERSION is parsed (read_pandoc()).
    if (any(grepl("PANDOC_VERSION", lines, fixed = TRUE))) {
      need_yaml()
    }
    writeLines(pandoc_report(lines, flags[[1L]], flags[[2L]]))
    return(0L)
  }
  # The self-test's fixtures are parsed with the yaml package.
  need_yaml()
  cat(self_test())
  if ("--self-test" %in% args) {
    return(0L)
  }
  if (report(run_checks(package_root()))) 0L else 1L
}

quit(status = main())
