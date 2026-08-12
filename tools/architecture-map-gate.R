#!/usr/bin/env Rscript

# ARCHITECTURE.md load-order + file-map + dependency gate
# (RURL-vppxuyfs, extended by RURL-rnwfclja).
#
# WHY THIS EXISTS. Three sections of ARCHITECTURE.md are exact enumerations of
# `Collate:` written as prose, and nothing checked either one. Both drifted, and
# both drifted silently: measured 2026-08-12, the `## Load order` block listed
# 14 of 24 files and the `## File / responsibility map` documented 16 of 24. The
# missing entries were load-bearing subsystems, not helpers -- `parse-web.R`
# holds the in-tree syntactic parser ARCHITECTURE.md exists to explain, and a
# reader sent there to learn it would not have found it named. Every gap was
# created by a slice that added an R file and had no reason to open
# ARCHITECTURE.md.
#
# WHY A GATE IS THE RIGHT ANSWER HERE, when a bijection gate on prose usually is
# not. The objection is that such a gate forces a stub entry for every helper
# file, and a stub is worse than an honest silence. That objection does not bind
# in this package: `Collate:` is 24 files, every one of them a subsystem with a
# header comment explaining itself, and the cost of the rule is one paragraph
# per new R file. The gate also judges no prose -- it asks only whether the
# filename appears, which is precisely the property that went wrong. Quality is
# still a reviewer's job; silence is now the gate's.
#
# WHAT IT CHECKS. Four properties, with DESCRIPTION as the single authority
# throughout (ARCHITECTURE.md says so itself):
#   1. the `## Load order` block lists exactly `Collate:`, in the same ORDER --
#      the block claims to be the load order, so a set comparison would let it
#      lie about the thing it is for;
#   2. every `Collate:` file has a `**R/<file>**` entry in the file map;
#   3. no file-map entry names a file that is not in `Collate:` (this is the
#      direction a rename or deletion breaks);
#   4. `## Dependencies` names exactly the packages in `Imports:`, each with the
#      same version floor -- see below.
#
# WHY PROPERTY 4 GATES THE FLOOR RATHER THAN BANNING IT (RURL-rnwfclja). The
# section transcribes `Imports:`, and had already drifted by omission: measured
# 2026-08-12 it listed three of the four imports, missing `utils` entirely. The
# floors are the second, scheduled half of that drift -- RURL-tffcqpho exists to
# move `punycoder (>= 1.2.0)` and `pslr (>= 1.1.0)`, and nothing would have made
# it open this file. The cheaper rule was available and was rejected: forbidding
# version numbers in the prose removes the drift surface, but it also removes
# the answer to the question the section is asked most ("which packages, at what
# minimum"), and sends the reader to DESCRIPTION for half of what they came for.
# A gate makes the transcription safe, which is the outcome banning it only
# approximates. The coupling it creates is the intended one: a floor bump now
# touches DESCRIPTION and this document in the same commit.
#
# Property 4 compares as a SET, unlike property 1. The load-order block's
# subject is the order; the dependency list's is not -- it groups by importance,
# where `Imports:` is alphabetical, and there is nothing to lie about.
#
# NON-VACUITY. A gate whose population is empty passes for the wrong reason, so
# parsing failures are errors, not zero findings: an unparseable or under-full
# `Collate:` or `Imports:`, a missing section heading, a missing fenced block,
# and a `## Dependencies` section with no entries each abort.
#
# Deterministic and network-free. Base R over two tracked files (DESCRIPTION,
# ARCHITECTURE.md) -- no package build, no yaml dependency, nothing installed.
#
# Usage:
#   Rscript tools/architecture-map-gate.R             # scan the repo, exit 1 on a gap
#   Rscript tools/architecture-map-gate.R --self-test # positive/negative unit checks
#
# NOT IN SCOPE, on purpose: `## Key internal functions` is a curated SELECTION,
# not an enumeration. The argument that justifies a bijection over 24 files --
# each one a subsystem with a header comment already explaining itself -- does
# not transfer to several hundred internal functions, where the rule would buy
# stub entries and nothing else. Leave it ungated.

# --- inputs ------------------------------------------------------------------

# The `Collate:` field as a sequence of bare filenames, in field order. R's
# `Collate:` is a continuation-indented list of quoted names; order is the load
# order and is what property 1 compares against.
collate_files <- function(path) {
  if (!file.exists(path)) {
    stop("cannot find DESCRIPTION at: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  start <- grep("^Collate:", lines)
  if (length(start) == 0L) {
    stop("DESCRIPTION has no `Collate:` field -- this gate has no population",
         call. = FALSE)
  }
  start <- start[[1L]]
  out <- character(0)
  i <- start + 1L
  while (i <= length(lines) && grepl("^[[:space:]]", lines[i])) {
    tok <- trimws(gsub("['\"]", "", lines[i]))
    if (nzchar(tok)) out <- c(out, tok)
    i <- i + 1L
  }
  if (length(out) < 2L) {
    stop(sprintf("parsed only %d file(s) out of `Collate:` -- refusing to score",
                 length(out)), call. = FALSE)
  }
  out
}

# One `pkg (>= x.y.z)` requirement split into its name and its floor. The floor
# is kept as the written string (">= 1.2.0"), not a version object: the gate
# compares two transcriptions of the same field, so a difference in how the
# constraint is SPELLED is a finding, not something to normalize away.
split_requirement <- function(tok) {
  name <- trimws(sub("[(].*$", "", tok))
  floor <- NA_character_
  if (grepl("[(]", tok)) {
    floor <- trimws(gsub("[[:space:]]+", " ", sub("^[^(]*[(]([^)]*)[)].*$", "\\1", tok)))
  }
  list(name = name, floor = floor)
}

# The `Imports:` field as a named vector of floors: names are package names,
# values are the written constraint or NA where the field states none. Same
# continuation-indented shape as `Collate:`, but comma-separated.
imports_requirements <- function(path) {
  if (!file.exists(path)) {
    stop("cannot find DESCRIPTION at: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  start <- grep("^Imports:", lines)
  if (length(start) == 0L) {
    stop("DESCRIPTION has no `Imports:` field -- this gate has no population",
         call. = FALSE)
  }
  start <- start[[1L]]
  body <- sub("^Imports:", "", lines[start])
  i <- start + 1L
  while (i <= length(lines) && grepl("^[[:space:]]", lines[i])) {
    body <- c(body, lines[i])
    i <- i + 1L
  }
  toks <- trimws(unlist(strsplit(paste(body, collapse = " "), ",")))
  toks <- toks[nzchar(toks)]
  if (length(toks) < 2L) {
    stop(sprintf("parsed only %d package(s) out of `Imports:` -- refusing to score",
                 length(toks)), call. = FALSE)
  }
  parsed <- lapply(toks, split_requirement)
  stats::setNames(
    vapply(parsed, `[[`, character(1), "floor"),
    vapply(parsed, `[[`, character(1), "name")
  )
}

# The packages named by `## Dependencies` entries, as the same named vector of
# written floors. An ENTRY is a list item that OPENS with a backticked name --
# the same entry-versus-mention distinction the file map needs, and it is load
# bearing here too: the section closes with a paragraph naming, in backticks, a
# package that is NO LONGER a dependency -- which must not be read as an import.
dependency_entries <- function(lines) {
  head <- grep("^## Dependencies[[:space:]]*$", lines)
  if (length(head) == 0L) {
    stop("ARCHITECTURE.md has no `## Dependencies` heading", call. = FALSE)
  }
  head <- head[[1L]]
  stop_at <- grep("^## ", lines)
  stop_at <- stop_at[stop_at > head]
  last <- if (length(stop_at) > 0L) stop_at[[1L]] - 1L else length(lines)
  block <- lines[seq.int(head + 1L, last)]

  items <- grep("^- `[^`]+`", block, value = TRUE)
  if (length(items) == 0L) {
    stop("the `## Dependencies` section lists no packages -- refusing to score",
         call. = FALSE)
  }
  names_ <- sub("^- `([^`]+)`.*$", "\\1", items)
  rest <- sub("^- `[^`]+`", "", items)
  floors <- vapply(rest, function(r) {
    if (!grepl("^[[:space:]]*[(]", r)) return(NA_character_)
    trimws(gsub("[[:space:]]+", " ", sub("^[[:space:]]*[(]([^)]*)[)].*$", "\\1", r)))
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(floors, names_)
}

# The body of the first fenced code block under `## Load order`, flattened to
# the sequence of filenames it names. The block spells the order with `->`
# arrows wrapped across lines, so the arrows and whitespace are separators and
# everything else is a token.
load_order_files <- function(lines) {
  head <- grep("^## Load order[[:space:]]*$", lines)
  if (length(head) == 0L) {
    stop("ARCHITECTURE.md has no `## Load order` heading", call. = FALSE)
  }
  head <- head[[1L]]
  stop_at <- grep("^## ", lines)
  stop_at <- stop_at[stop_at > head]
  last <- if (length(stop_at) > 0L) stop_at[[1L]] - 1L else length(lines)
  block <- lines[seq.int(head + 1L, last)]

  fences <- grep("^```", block)
  if (length(fences) < 2L) {
    stop("the `## Load order` section has no fenced code block", call. = FALSE)
  }
  body <- block[seq.int(fences[[1L]] + 1L, fences[[2L]] - 1L)]
  toks <- unlist(strsplit(paste(body, collapse = " "), "[[:space:]]+"))
  toks <- toks[nzchar(toks) & toks != "→" & toks != "->"]
  if (length(toks) == 0L) {
    stop("the `## Load order` block is empty -- refusing to score", call. = FALSE)
  }
  toks
}

# The files named by `**R/<file>**` entries in the file-map section, in order of
# appearance. Bold is what every existing entry uses to open itself, and it is
# what distinguishes an ENTRY from a passing mention of the same file in another
# entry's prose (`R/domain.R` is referenced seven times but is one entry).
file_map_files <- function(lines) {
  head <- grep("^## File / responsibility map[[:space:]]*$", lines)
  if (length(head) == 0L) {
    stop("ARCHITECTURE.md has no `## File / responsibility map` heading",
         call. = FALSE)
  }
  head <- head[[1L]]
  stop_at <- grep("^## ", lines)
  stop_at <- stop_at[stop_at > head]
  last <- if (length(stop_at) > 0L) stop_at[[1L]] - 1L else length(lines)
  block <- lines[seq.int(head + 1L, last)]

  hits <- unlist(regmatches(
    block, gregexpr("[*][*]R/[A-Za-z0-9_.-]+[.]R[*][*]", block)
  ))
  sub("^[*][*]R/", "", sub("[*][*]$", "", hits))
}

architecture_lines <- function(path) {
  if (!file.exists(path)) {
    stop("cannot find ARCHITECTURE.md at: ", path, call. = FALSE)
  }
  readLines(path, warn = FALSE)
}

# --- the check ---------------------------------------------------------------

# Returns a character vector of violation messages; empty means both sections
# agree with `Collate:`.
check_map <- function(collate, load_order, mapped) {
  out <- character(0)

  if (!identical(load_order, collate)) {
    absent <- setdiff(collate, load_order)
    extra <- setdiff(load_order, collate)
    if (length(absent) > 0L) {
      out <- c(out, sprintf(
        "the `## Load order` block omits %d file(s) from `Collate:`: %s",
        length(absent), paste(absent, collapse = ", ")
      ))
    }
    if (length(extra) > 0L) {
      out <- c(out, sprintf(
        "the `## Load order` block names %d file(s) not in `Collate:`: %s",
        length(extra), paste(extra, collapse = ", ")
      ))
    }
    if (length(absent) == 0L && length(extra) == 0L) {
      out <- c(out, paste0(
        "the `## Load order` block lists the right files in the WRONG ORDER; ",
        "`Collate:` order is: ", paste(collate, collapse = " -> ")
      ))
    }
  }

  absent <- setdiff(collate, mapped)
  if (length(absent) > 0L) {
    out <- c(out, sprintf(
      "%d file(s) in `Collate:` have no `**R/<file>**` entry in the file map: %s",
      length(absent), paste(absent, collapse = ", ")
    ))
  }

  extra <- setdiff(mapped, collate)
  if (length(extra) > 0L) {
    out <- c(out, sprintf(
      "the file map documents %d file(s) not in `Collate:` (renamed or deleted?): %s",
      length(extra), paste(extra, collapse = ", ")
    ))
  }

  dup <- unique(mapped[duplicated(mapped)])
  if (length(dup) > 0L) {
    out <- c(out, sprintf(
      "the file map opens more than one entry for the same file: %s",
      paste(dup, collapse = ", ")
    ))
  }

  out
}

# Property 4. `imports` and `documented` are both named vectors of written
# floors, NA where none is stated.
check_dependencies <- function(imports, documented) {
  out <- character(0)

  absent <- setdiff(names(imports), names(documented))
  if (length(absent) > 0L) {
    out <- c(out, sprintf(
      "the `## Dependencies` section omits %d package(s) from `Imports:`: %s",
      length(absent), paste(absent, collapse = ", ")
    ))
  }

  extra <- setdiff(names(documented), names(imports))
  if (length(extra) > 0L) {
    out <- c(out, sprintf(
      "the `## Dependencies` section names %d package(s) not in `Imports:`: %s",
      length(extra), paste(extra, collapse = ", ")
    ))
  }

  dup <- unique(names(documented)[duplicated(names(documented))])
  if (length(dup) > 0L) {
    out <- c(out, sprintf(
      "the `## Dependencies` section opens more than one entry for: %s",
      paste(dup, collapse = ", ")
    ))
  }

  # One message shape covers all three ways a floor can disagree: dropped from
  # the prose, invented in the prose, or transcribed and then left behind.
  shown <- function(x) if (is.na(x)) "no version floor" else sprintf("(%s)", x)
  for (pkg in intersect(names(imports), names(documented))) {
    have <- documented[[pkg]]
    want <- imports[[pkg]]
    if (!identical(have, want)) {
      out <- c(out, sprintf(
        "the `## Dependencies` entry for `%s` says %s; `Imports:` says %s",
        pkg, shown(have), shown(want)
      ))
    }
  }

  out
}

check_repo <- function(root) {
  description <- file.path(root, "DESCRIPTION")
  collate <- collate_files(description)
  imports <- imports_requirements(description)
  lines <- architecture_lines(file.path(root, "ARCHITECTURE.md"))
  load_order <- load_order_files(lines)
  mapped <- file_map_files(lines)
  documented <- dependency_entries(lines)
  list(
    collate = collate, load_order = load_order, mapped = mapped,
    imports = imports, documented = documented,
    violations = c(
      check_map(collate, load_order, mapped),
      check_dependencies(imports, documented)
    )
  )
}

# --- self-test (positive + negative coverage, executable) --------------------

# Real files rather than hand-built argument lists: the two parsers are as much
# of the gate as the comparison is, and a self-test that skipped them would pass
# while the section reader was broken.
write_fixture <- function(dir, collate, load_order, map_entries,
                          imports = c("alpha", "beta (>= 1.0.0)"),
                          deps = c("alpha", "beta (>= 1.0.0)")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("Package: fixture", "Version: 0.0.1",
      "Imports:",
      sprintf("    %s%s", imports,
              c(rep(",", max(length(imports) - 1L, 0L)), "")),
      "Collate:",
      sprintf("    '%s'", collate)),
    file.path(dir, "DESCRIPTION")
  )
  render_dep <- function(tok) {
    p <- split_requirement(tok)
    if (is.na(p$floor)) {
      sprintf("- `%s` — needed for something.", p$name)
    } else {
      sprintf("- `%s` (%s) — needed for something.", p$name, p$floor)
    }
  }
  writeLines(
    c("# fixture architecture", "",
      "## Load order", "",
      "```",
      # Wrapped across lines on purpose: the real block wraps, and a reader
      # that only handled one line would pass here and fail in the repo.
      paste(head(load_order, 1L), collapse = " "),
      paste(c("→", paste(load_order[-1L], collapse = " → ")),
            collapse = " "),
      "```", "",
      "## File / responsibility map", "",
      sprintf("- **R/%s** — does a thing.", map_entries), "",
      "## Dependencies", "",
      if (length(deps) > 0L) vapply(deps, render_dep, character(1),
                                    USE.NAMES = FALSE),
      "",
      # Present in every fixture: the real section closes with exactly this
      # shape, and a parser that read it as an entry would invent an import.
      "`gamma` is no longer a dependency.", "",
      "## Something else", "", "tail."),
    file.path(dir, "ARCHITECTURE.md")
  )
  dir
}

self_test <- function() {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  base <- tempfile("architecture-map-gate-selftest-")

  scenario <- function(tag, collate, load_order, map_entries, ...) {
    d <- write_fixture(file.path(base, tag), collate, load_order, map_entries,
                       ...)
    check_repo(d)$violations
  }

  three <- c("a.R", "b.R", "c.R")

  # POSITIVE: both sections agree with `Collate:`.
  v <- scenario("ok", three, three, three)
  if (length(v) > 0L) {
    fail(sprintf("false positive on a complete file: %s",
                 paste(v, collapse = "; ")))
  }

  # POSITIVE: the map may carry passing mentions of a file inside another
  # entry's prose without opening a second entry for it.
  d <- write_fixture(file.path(base, "prose"), three, three, three)
  a <- readLines(file.path(d, "ARCHITECTURE.md"))
  a <- sub("^- [*][*]R/a[.]R[*][*] .*$",
           "- **R/a.R** — talks about R/b.R and R/c.R at length.", a)
  writeLines(a, file.path(d, "ARCHITECTURE.md"))
  if (length(check_repo(d)$violations) > 0L) {
    fail("counted a passing mention in prose as a duplicate entry")
  }

  # NEGATIVE: the measured defect -- a file shipped, the map never updated.
  v <- scenario("map-gap", three, three, c("a.R", "b.R"))
  if (!any(grepl("no `[*][*]R/<file>[*][*]` entry", v))) {
    fail("did not flag a Collate file missing from the file map")
  }

  # NEGATIVE: the other measured defect -- the load-order block left behind.
  v <- scenario("order-gap", three, c("a.R", "b.R"), three)
  if (!any(grepl("omits 1 file", v))) {
    fail("did not flag a Collate file missing from the load-order block")
  }

  # NEGATIVE: the block lists the right files in the wrong order. A set
  # comparison would pass this, and the block's whole subject is the order.
  v <- scenario("order-wrong", three, c("a.R", "c.R", "b.R"), three)
  if (!any(grepl("WRONG ORDER", v))) {
    fail("did not flag a load-order block whose order disagrees with Collate:")
  }

  # NEGATIVE: a map entry for a file that no longer exists (rename/deletion).
  v <- scenario("map-stale", three, three, c(three, "gone.R"))
  if (!any(grepl("not in `Collate:` \\(renamed or deleted", v))) {
    fail("did not flag a file-map entry with no Collate file")
  }

  # NEGATIVE: two entries opened for one file.
  v <- scenario("map-dup", three, three, c(three, "a.R"))
  if (!any(grepl("more than one entry", v))) {
    fail("did not flag a file documented by two entries")
  }

  # POSITIVE: a backticked package named in the section's closing PROSE is not
  # an entry. Asserted on the parser, not only through a green scenario: `ok`
  # would also pass if the parser dropped every entry it should have kept.
  d <- write_fixture(file.path(base, "dep-prose"), three, three, three)
  documented <- dependency_entries(readLines(file.path(d, "ARCHITECTURE.md")))
  if (!identical(names(documented), c("alpha", "beta"))) {
    fail(sprintf("read the dependency section as: %s",
                 paste(names(documented), collapse = ", ")))
  }
  if (!identical(unname(documented), c(NA_character_, ">= 1.0.0"))) {
    fail("did not read the written version floors off the section")
  }

  # NEGATIVE: the measured defect -- `utils` in `Imports:`, absent from the
  # section because nothing ever asked anyone to add it.
  v <- scenario("dep-gap", three, three, three,
                imports = c("alpha", "beta (>= 1.0.0)", "utils"),
                deps = c("alpha", "beta (>= 1.0.0)"))
  if (!any(grepl("omits 1 package", v))) {
    fail("did not flag an `Imports:` package missing from `## Dependencies`")
  }

  # NEGATIVE: an entry for a package that is no longer imported.
  v <- scenario("dep-stale", three, three, three,
                deps = c("alpha", "beta (>= 1.0.0)", "delta"))
  if (!any(grepl("names 1 package\\(s\\) not in `Imports:`", v))) {
    fail("did not flag a documented package that `Imports:` lacks")
  }

  # NEGATIVE: the scheduled drift (RURL-tffcqpho) -- DESCRIPTION's floor moves,
  # the prose keeps claiming the old one.
  v <- scenario("dep-floor-stale", three, three, three,
                imports = c("alpha", "beta (>= 2.0.0)"))
  if (!any(grepl("`beta` says \\(>= 1[.]0[.]0\\); `Imports:` says \\(>= 2[.]0[.]0\\)", v))) {
    fail("did not flag a version floor that disagrees with `Imports:`")
  }

  # NEGATIVE: the floor dropped from the prose. Silence must not pass, or the
  # drift reopens by omission the moment an entry is reworded.
  v <- scenario("dep-floor-dropped", three, three, three, deps = c("alpha", "beta"))
  if (!any(grepl("`beta` says no version floor", v))) {
    fail("did not flag an entry that dropped a floor `Imports:` states")
  }

  # NEGATIVE: a floor invented in the prose that DESCRIPTION does not impose.
  v <- scenario("dep-floor-invented", three, three, three,
                deps = c("alpha (>= 3.0.0)", "beta (>= 1.0.0)"))
  if (!any(grepl("`alpha` says \\(>= 3[.]0[.]0\\); `Imports:` says no version floor", v))) {
    fail("did not flag a floor the prose invented")
  }

  # NEGATIVE: two entries opened for one package.
  v <- scenario("dep-dup", three, three, three,
                deps = c("alpha", "beta (>= 1.0.0)", "alpha"))
  if (!any(grepl("more than one entry for: alpha", v))) {
    fail("did not flag a package documented by two entries")
  }

  # NON-VACUITY: an unreadable population must abort, not score zero findings.
  d <- write_fixture(file.path(base, "vacuous"), three, three, three)
  writeLines(c("Package: fixture", "Version: 0.0.1"),
             file.path(d, "DESCRIPTION"))
  if (!inherits(try(check_repo(d), silent = TRUE), "try-error")) {
    fail("scored a DESCRIPTION with no `Collate:` instead of aborting")
  }

  d <- write_fixture(file.path(base, "no-section"), three, three, three)
  a <- readLines(file.path(d, "ARCHITECTURE.md"))
  writeLines(sub("^## File / responsibility map$", "## Files", a),
             file.path(d, "ARCHITECTURE.md"))
  if (!inherits(try(check_repo(d), silent = TRUE), "try-error")) {
    fail("scored an ARCHITECTURE.md with no file-map heading instead of aborting")
  }

  d <- write_fixture(file.path(base, "no-fence"), three, three, three)
  a <- readLines(file.path(d, "ARCHITECTURE.md"))
  writeLines(a[!grepl("^```$", a)], file.path(d, "ARCHITECTURE.md"))
  if (!inherits(try(check_repo(d), silent = TRUE), "try-error")) {
    fail("scored a load-order section with no fenced block instead of aborting")
  }

  d <- write_fixture(file.path(base, "no-imports"), three, three, three)
  a <- readLines(file.path(d, "DESCRIPTION"))
  writeLines(a[!grepl("^Imports:$|^    (alpha|beta)", a)],
             file.path(d, "DESCRIPTION"))
  if (!inherits(try(check_repo(d), silent = TRUE), "try-error")) {
    fail("scored a DESCRIPTION with no `Imports:` instead of aborting")
  }

  d <- write_fixture(file.path(base, "no-deps-section"), three, three, three)
  a <- readLines(file.path(d, "ARCHITECTURE.md"))
  writeLines(sub("^## Dependencies$", "## Packages we use", a),
             file.path(d, "ARCHITECTURE.md"))
  if (!inherits(try(check_repo(d), silent = TRUE), "try-error")) {
    fail("scored an ARCHITECTURE.md with no `## Dependencies` heading instead of aborting")
  }

  # An empty section is the shape that would score zero findings while
  # documenting nothing -- the exact way a bijection gate passes vacuously.
  d <- write_fixture(file.path(base, "no-deps-entries"), three, three, three,
                     deps = character(0))
  if (!inherits(try(check_repo(d), silent = TRUE), "try-error")) {
    fail("scored a `## Dependencies` section with no entries instead of aborting")
  }

  unlink(base, recursive = TRUE)
  cat("architecture-map-gate self-test: PASS (3 positive + 17 negative cases)\n")
  invisible(TRUE)
}

# --- main --------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }

  res <- check_repo(root)
  cat(sprintf(
    paste0("ARCHITECTURE.md load-order + file-map + dependency gate\n",
           "  %d file(s) in `Collate:`\n",
           "  %d file(s) in the load-order block\n",
           "  %d entry/entries in the file map\n",
           "  %d package(s) in `Imports:`\n",
           "  %d entry/entries under `## Dependencies`\n"),
    length(res$collate), length(res$load_order), length(res$mapped),
    length(res$imports), length(res$documented)
  ))
  if (length(res$violations) > 0L) {
    cat("GAP:\n")
    for (v in res$violations) cat("  - ", v, "\n", sep = "")
    stop(sprintf("ARCHITECTURE.md is out of sync with DESCRIPTION (%d finding(s))",
                 length(res$violations)), call. = FALSE)
  }
  cat(paste0("PASS: the load order matches `Collate:`, every file has a map ",
             "entry, and\n      `## Dependencies` matches `Imports:`.\n"))
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  if (sys.nframe() == 0L) {
    main()
  }
}
