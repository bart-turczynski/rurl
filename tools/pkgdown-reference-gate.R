#!/usr/bin/env Rscript

# pkgdown reference-index gate (RURL-mqexzpwt).
#
# WHY THIS EXISTS. `_pkgdown.yml`'s `reference:` index is hand-maintained and
# nothing verified it covered the package's exports. Measured 2026-08-12 during
# RURL-qxtbfavl, it was missing TEN of 40 exports -- serialize_url, get_url_key,
# url_key_policy, the six url_*_join functions, and format_url. Every one of
# them shipped in a slice that never opened `_pkgdown.yml`, and no gate said a
# word. The omission is invisible until someone builds the site, at which point
# pkgdown reports undocumented topics for a surface that has been public for
# several releases.
#
# WHAT IT CHECKS. Four properties, all of them pkgdown's own rules:
#   1. every `export()` in NAMESPACE reaches the index (via its topic);
#   2. every index entry resolves to a real topic in man/;
#   3. no topic is listed twice (pkgdown treats that as an error);
#   4. every export has an Rd topic at all.
#
# TOPICS, NOT NAMES. An export is usually its own topic, but not always: the six
# `url_*_join` functions are aliases of the single `url_join` topic, so the
# index lists `url_join` once and covers all six. Comparing names would demand
# six entries and pkgdown would then reject the file for listing one topic six
# times. So the gate maps export -> Rd file (by `\alias`) and asks whether ANY
# name of that Rd file appears in the index.
#
# Deterministic and network-free. It reads three tracked files (NAMESPACE,
# man/*.Rd, _pkgdown.yml) with base R only -- no package build, no yaml
# dependency, nothing installed.
#
# Usage:
#   Rscript tools/pkgdown-reference-gate.R             # scan the repo, exit 1 on a gap
#   Rscript tools/pkgdown-reference-gate.R --self-test # positive/negative unit checks

# --- inputs ------------------------------------------------------------------

# The exported names, as NAMESPACE spells them. S3method() registrations are
# deliberately excluded: pkgdown documents methods under their generic's topic,
# not as index entries of their own.
namespace_exports <- function(path) {
  if (!file.exists(path)) {
    stop("cannot find NAMESPACE at: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  hits <- grep("^\\s*export\\(", lines, value = TRUE)
  out <- sub("^\\s*export\\(\\s*", "", hits)
  out <- sub("\\s*\\).*$", "", out)
  sort(unique(gsub("[\"']", "", out)))
}

# One record per Rd file: its `\name`, every `\alias`, and the file it came
# from. `names` is what an index entry may legitimately say to reach the topic.
rd_topics <- function(man_dir) {
  files <- sort(Sys.glob(file.path(man_dir, "*.Rd")))
  lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    grab <- function(tag) {
      hits <- grep(paste0("^\\\\", tag, "\\{"), lines, value = TRUE)
      sub("\\}.*$", "", sub(paste0("^\\\\", tag, "\\{"), "", hits))
    }
    nm <- grab("name")
    list(
      file = basename(f),
      name = if (length(nm) > 0L) nm[[1L]] else sub("\\.Rd$", "", basename(f)),
      names = unique(c(nm, grab("alias")))
    )
  })
}

# The entries of every `contents:` list under the top-level `reference:` key,
# in file order. The reference block runs from `^reference:` to the next
# top-level key; inside it, a `contents:` line opens a list of bare topic names
# and any other key (`- title:`, `desc:`, `title:`) closes it.
#
# A pkgdown selector function (`starts_with("get_")`, `matches(...)`) is
# returned as-is so the caller can refuse to score a file it cannot resolve,
# rather than silently reading it as a topic named `starts_with("get_")`.
pkgdown_reference_entries <- function(path) {
  if (!file.exists(path)) {
    stop("cannot find _pkgdown.yml at: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  start <- grep("^reference:\\s*$", lines)
  if (length(start) == 0L) {
    stop("_pkgdown.yml has no top-level `reference:` key", call. = FALSE)
  }
  start <- start[[1L]]
  rest <- seq.int(start + 1L, length(lines))
  # The block ends at the next key in column 0 (a comment or blank line is not
  # a key and does not end it).
  ends <- rest[grepl("^[A-Za-z_][A-Za-z0-9_]*:", lines[rest])]
  last <- if (length(ends) > 0L) ends[[1L]] - 1L else length(lines)
  block <- lines[seq.int(start + 1L, last)]

  entries <- character(0)
  in_contents <- FALSE
  for (ln in block) {
    if (grepl("^\\s*contents:\\s*$", ln)) {
      in_contents <- TRUE
      next
    }
    if (grepl("^\\s*(-\\s*)?(title|desc|subtitle):", ln)) {
      in_contents <- FALSE
      next
    }
    if (!in_contents) next
    if (grepl("^\\s*$", ln) || grepl("^\\s*#", ln)) next
    m <- regmatches(ln, regexec("^\\s*-\\s*(\\S.*?)\\s*$", ln))[[1L]]
    if (length(m) == 2L) entries <- c(entries, m[[2L]])
  }
  entries
}

# --- the check ---------------------------------------------------------------

# Returns a character vector of violation messages; empty means the index is
# sound. `topics` is the rd_topics() list.
check_index <- function(exports, topics, entries) {
  out <- character(0)

  selectors <- entries[grepl("\\(", entries)]
  if (length(selectors) > 0L) {
    # Refuse to score rather than guess: a selector can cover exports this gate
    # would report as missing, and reading one as a literal topic name would
    # make every answer below unreliable.
    return(sprintf(
      paste0("_pkgdown.yml uses pkgdown selector function(s) this gate cannot ",
             "resolve: %s. Teach the gate to expand them before relying on it."),
      paste(selectors, collapse = ", ")
    ))
  }

  # entry -> index of the Rd topic it names (NA when it resolves to nothing).
  resolve <- function(name) {
    hit <- which(vapply(topics, function(t) name %in% t$names, logical(1)))
    if (length(hit) == 0L) NA_integer_ else hit[[1L]]
  }
  resolved <- vapply(entries, resolve, integer(1), USE.NAMES = FALSE)

  unknown <- entries[is.na(resolved)]
  if (length(unknown) > 0L) {
    out <- c(out, sprintf(
      "_pkgdown.yml lists %d entry/entries with no matching topic in man/: %s",
      length(unknown), paste(unknown, collapse = ", ")
    ))
  }

  # pkgdown errors when one topic is claimed by two entries.
  known <- resolved[!is.na(resolved)]
  dup <- unique(known[duplicated(known)])
  if (length(dup) > 0L) {
    out <- c(out, sprintf(
      "_pkgdown.yml lists the same topic more than once (pkgdown treats this as an error): %s",
      paste(vapply(dup, function(i) {
        sprintf("%s (via %s)", topics[[i]]$file,
                paste(entries[!is.na(resolved) & resolved == i], collapse = " + "))
      }, character(1)), collapse = "; ")
    ))
  }

  covered_names <- unlist(lapply(topics[known], function(t) t$names))
  undocumented <- character(0)
  missing <- character(0)
  for (e in exports) {
    owner <- which(vapply(topics, function(t) e %in% t$names, logical(1)))
    if (length(owner) == 0L) {
      undocumented <- c(undocumented, e)
    } else if (!(e %in% covered_names)) {
      missing <- c(missing, e)
    }
  }
  if (length(undocumented) > 0L) {
    out <- c(out, sprintf(
      "%d export(s) have no Rd topic at all: %s",
      length(undocumented), paste(undocumented, collapse = ", ")
    ))
  }
  if (length(missing) > 0L) {
    out <- c(out, sprintf(
      "%d export(s) are absent from the _pkgdown.yml reference index: %s",
      length(missing), paste(missing, collapse = ", ")
    ))
  }
  out
}

check_repo <- function(root) {
  exports <- namespace_exports(file.path(root, "NAMESPACE"))
  topics <- rd_topics(file.path(root, "man"))
  entries <- pkgdown_reference_entries(file.path(root, "_pkgdown.yml"))
  list(
    exports = exports, topics = topics, entries = entries,
    violations = check_index(exports, topics, entries)
  )
}

# --- self-test (positive + negative coverage, executable) --------------------

# Build a throwaway package skeleton so the negative cases are real files rather
# than hand-built argument lists: the parsers are as much of the gate as the
# comparison is, and a self-test that skipped them would pass while the YAML
# reader was broken.
write_fixture <- function(dir, exports, rd, contents) {
  dir.create(file.path(dir, "man"), recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf("export(%s)", exports), file.path(dir, "NAMESPACE"))
  for (nm in names(rd)) {
    writeLines(
      c(sprintf("\\name{%s}", nm), sprintf("\\alias{%s}", rd[[nm]]),
        "\\title{x}"),
      file.path(dir, "man", paste0(nm, ".Rd"))
    )
  }
  writeLines(
    c("template:", "  bootstrap: 5", "", "reference:",
      "  - title: \"All\"", "    desc: \"everything\"", "    contents:",
      sprintf("      - %s", contents), "", "footer:", "  structure:"),
    file.path(dir, "_pkgdown.yml")
  )
  dir
}

self_test <- function() {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  base <- tempfile("pkgdown-gate-selftest-")

  scenario <- function(tag, exports, rd, contents) {
    d <- write_fixture(file.path(base, tag), exports, rd, contents)
    check_repo(d)$violations
  }

  # A shared topic with aliases -- the url_join shape the real package has.
  rd_full <- list(alpha = c("alpha"), joins = c("joins", "j_left", "j_right"))

  # POSITIVE: complete index, one entry covering three aliased exports.
  v <- scenario("ok", c("alpha", "j_left", "j_right"), rd_full,
                c("alpha", "joins"))
  if (length(v) > 0L) {
    fail(sprintf("false positive on a complete index: %s",
                 paste(v, collapse = "; ")))
  }

  # POSITIVE: an alias, not the \name, may be the entry that reaches the topic.
  v <- scenario("alias-entry", c("alpha", "j_left"), rd_full,
                c("alpha", "j_left"))
  if (length(v) > 0L) {
    fail(sprintf("false positive when an alias names the topic: %s",
                 paste(v, collapse = "; ")))
  }

  # NEGATIVE: the measured defect -- an export shipped, index never updated.
  v <- scenario("missing", c("alpha", "j_left"), rd_full, c("alpha"))
  if (!any(grepl("absent from the _pkgdown.yml reference index", v))) {
    fail("did not flag an export missing from the index")
  }

  # NEGATIVE: one topic claimed twice (a pkgdown error).
  v <- scenario("dup", c("alpha", "j_left"), rd_full,
                c("alpha", "joins", "j_left"))
  if (!any(grepl("more than once", v))) {
    fail("did not flag a topic listed twice")
  }

  # NEGATIVE: an index entry naming a topic that does not exist.
  v <- scenario("unknown", c("alpha"), rd_full, c("alpha", "ghost"))
  if (!any(grepl("no matching topic", v))) {
    fail("did not flag an index entry with no topic")
  }

  # NEGATIVE: an export with no Rd file at all.
  v <- scenario("undoc", c("alpha", "orphan"), rd_full, c("alpha"))
  if (!any(grepl("no Rd topic at all", v))) {
    fail("did not flag an export with no Rd topic")
  }

  # A selector function must suspend scoring, not be read as a topic name.
  v <- scenario("selector", c("alpha"), rd_full, c("starts_with(\"a\")"))
  if (!any(grepl("selector function", v))) {
    fail("did not refuse to score an index using a pkgdown selector")
  }

  unlink(base, recursive = TRUE)
  cat("pkgdown-reference-gate self-test: PASS (2 positive + 5 negative cases)\n")
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
    paste0("pkgdown reference-index gate\n  %d export(s) in NAMESPACE\n",
           "  %d topic(s) in man/\n  %d entry/entries in the reference index\n"),
    length(res$exports), length(res$topics), length(res$entries)
  ))
  if (length(res$violations) > 0L) {
    cat("GAP:\n")
    for (v in res$violations) cat("  - ", v, "\n", sep = "")
    stop(sprintf("_pkgdown.yml's reference index is out of sync (%d finding(s))",
                 length(res$violations)), call. = FALSE)
  }
  cat("PASS: every export reaches the reference index, and every entry resolves.\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  if (sys.nframe() == 0L) {
    main()
  }
}
