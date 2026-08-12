#!/usr/bin/env Rscript

# ARCHITECTURE.md load-order + file-map gate (RURL-vppxuyfs).
#
# WHY THIS EXISTS. Two sections of ARCHITECTURE.md are exact enumerations of
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
# WHAT IT CHECKS. Three properties, with `Collate:` in DESCRIPTION as the single
# authority (ARCHITECTURE.md says so itself):
#   1. the `## Load order` block lists exactly `Collate:`, in the same ORDER --
#      the block claims to be the load order, so a set comparison would let it
#      lie about the thing it is for;
#   2. every `Collate:` file has a `**R/<file>**` entry in the file map;
#   3. no file-map entry names a file that is not in `Collate:` (this is the
#      direction a rename or deletion breaks).
#
# NON-VACUITY. A gate whose population is empty passes for the wrong reason, so
# parsing failures are errors, not zero findings: an unparseable or under-full
# `Collate:`, a missing section heading, and a missing fenced block each abort.
#
# Deterministic and network-free. Base R over two tracked files (DESCRIPTION,
# ARCHITECTURE.md) -- no package build, no yaml dependency, nothing installed.
#
# Usage:
#   Rscript tools/architecture-map-gate.R             # scan the repo, exit 1 on a gap
#   Rscript tools/architecture-map-gate.R --self-test # positive/negative unit checks

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

check_repo <- function(root) {
  collate <- collate_files(file.path(root, "DESCRIPTION"))
  lines <- architecture_lines(file.path(root, "ARCHITECTURE.md"))
  load_order <- load_order_files(lines)
  mapped <- file_map_files(lines)
  list(
    collate = collate, load_order = load_order, mapped = mapped,
    violations = check_map(collate, load_order, mapped)
  )
}

# --- self-test (positive + negative coverage, executable) --------------------

# Real files rather than hand-built argument lists: the two parsers are as much
# of the gate as the comparison is, and a self-test that skipped them would pass
# while the section reader was broken.
write_fixture <- function(dir, collate, load_order, map_entries) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("Package: fixture", "Version: 0.0.1", "Collate:",
      sprintf("    '%s'", collate)),
    file.path(dir, "DESCRIPTION")
  )
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
      "## Something else", "", "tail."),
    file.path(dir, "ARCHITECTURE.md")
  )
  dir
}

self_test <- function() {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  base <- tempfile("architecture-map-gate-selftest-")

  scenario <- function(tag, collate, load_order, map_entries) {
    d <- write_fixture(file.path(base, tag), collate, load_order, map_entries)
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

  unlink(base, recursive = TRUE)
  cat("architecture-map-gate self-test: PASS (2 positive + 8 negative cases)\n")
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
    paste0("ARCHITECTURE.md load-order + file-map gate\n",
           "  %d file(s) in `Collate:`\n",
           "  %d file(s) in the load-order block\n",
           "  %d entry/entries in the file map\n"),
    length(res$collate), length(res$load_order), length(res$mapped)
  ))
  if (length(res$violations) > 0L) {
    cat("GAP:\n")
    for (v in res$violations) cat("  - ", v, "\n", sep = "")
    stop(sprintf("ARCHITECTURE.md is out of sync with `Collate:` (%d finding(s))",
                 length(res$violations)), call. = FALSE)
  }
  cat("PASS: the load order matches `Collate:`, and every file has a map entry.\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  if (sys.nframe() == 0L) {
    main()
  }
}
