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
#   Rscript tools/pkgdown-reference-gate.R --regenerate
#       # append every missing export's topic to an "Uncategorized (TODO:
#       # place)" section of `_pkgdown.yml`, in place, and exit 0. The gate
#       # knows which topics are unreachable, not which section they belong
#       # in -- placement is editorial -- so the section's title carries the
#       # remaining task. Nothing existing is reordered or rewritten.

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

# Which exports the index reaches, and which it cannot. ONE predicate, shared by
# the check and by `--regenerate`, so the entry the regenerator writes is the
# entry the check was complaining about (design/measurement-traps.md section 5:
# two agreeing regexes are how a generator and its gate drift apart).
#
#   undocumented -- exports with no Rd topic at all (nothing can be written)
#   missing      -- exports whose topic exists but no entry reaches it
#   topic_of     -- export -> the \name of its Rd topic (NA when undocumented)
export_coverage <- function(exports, topics, entries) {
  resolve <- function(name) {
    hit <- which(vapply(topics, function(t) name %in% t$names, logical(1)))
    if (length(hit) == 0L) NA_integer_ else hit[[1L]]
  }
  resolved <- vapply(entries, resolve, integer(1), USE.NAMES = FALSE)
  known <- resolved[!is.na(resolved)]
  covered_names <- unlist(lapply(topics[known], function(t) t$names))
  undocumented <- character(0)
  missing <- character(0)
  topic_of <- stats::setNames(rep(NA_character_, length(exports)), exports)
  for (e in exports) {
    owner <- which(vapply(topics, function(t) e %in% t$names, logical(1)))
    if (length(owner) == 0L) {
      undocumented <- c(undocumented, e)
    } else {
      topic_of[[e]] <- topics[[owner[[1L]]]]$name
      if (!(e %in% covered_names)) {
        missing <- c(missing, e)
      }
    }
  }
  list(undocumented = undocumented, missing = missing, topic_of = topic_of)
}

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

  cov <- export_coverage(exports, topics, entries)
  undocumented <- cov$undocumented
  missing <- cov$missing
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

# --- regenerate --------------------------------------------------------------

# The title of the section `--regenerate` appends to. The gate has no notion of
# WHERE an export belongs -- it checks reachability, not placement, and the
# curated sections above are editorial -- so a regenerated entry lands here,
# where the title itself says it still has to be moved. Nothing existing is
# reordered; when the section already exists the entry joins its list.
REGEN_SECTION_TITLE <- "Uncategorized (TODO: place)"

# Insert every missing export's topic into `_pkgdown.yml`, in place. Returns
# (invisibly) a list describing what was written; prints it as it goes.
# Exports with no Rd topic are reported and skipped: pkgdown would reject an
# entry naming a topic that does not exist, so writing one would trade a gate
# finding for a site build error.
regenerate_index <- function(root) {
  path <- file.path(root, "_pkgdown.yml")
  exports <- namespace_exports(file.path(root, "NAMESPACE"))
  topics <- rd_topics(file.path(root, "man"))
  entries <- pkgdown_reference_entries(path)
  if (any(grepl("\\(", entries))) {
    stop(paste0("_pkgdown.yml uses a pkgdown selector function; the gate ",
                "cannot score it, so --regenerate refuses to edit it"),
         call. = FALSE)
  }
  cov <- export_coverage(exports, topics, entries)

  for (e in cov$undocumented) {
    cat(sprintf("  ! cannot add `%s`: it has no Rd topic in man/\n", e))
  }
  # One entry per TOPIC: six aliased exports of one Rd file need one line, and
  # a second would be the duplicate-topic finding the gate also reports.
  add <- unique(unname(cov$topic_of[cov$missing]))
  if (length(add) == 0L) {
    cat("  nothing to regenerate: every documented export reaches the index\n")
    return(invisible(list(added = character(0), section_created = FALSE)))
  }

  lines <- readLines(path, warn = FALSE)
  start <- grep("^reference:\\s*$", lines)[[1L]]
  rest <- seq.int(start + 1L, length(lines))
  ends <- rest[grepl("^[A-Za-z_][A-Za-z0-9_]*:", lines[rest])]
  block_last <- if (length(ends) > 0L) ends[[1L]] - 1L else length(lines)
  block <- seq.int(start + 1L, block_last)

  # Indentation as the file already spells it: the first `contents:` line and
  # the first entry under it, so the appended section matches its neighbours.
  contents_at <- block[grepl("^\\s*contents:\\s*$", lines[block])]
  item_indent <- "      "
  title_indent <- "  "
  if (length(contents_at) > 0L) {
    ci <- contents_at[[1L]]
    item_line <- lines[ci + 1L]
    if (grepl("^\\s*-\\s*\\S", item_line)) {
      item_indent <- sub("^(\\s*).*$", "\\1", item_line)
    }
    title_at <- block[block < ci & grepl("^\\s*-\\s*title:", lines[block])]
    if (length(title_at) > 0L) {
      title_indent <- sub("^(\\s*)-.*$", "\\1", lines[title_at[length(title_at)]])
    }
  }
  new_items <- sprintf("%s- %s", item_indent, add)

  section_at <- block[grepl(
    sprintf("^\\s*-\\s*title:\\s*\"?%s\"?\\s*$",
            gsub("([][(){}.*+?^$|\\\\])", "\\\\\\1", REGEN_SECTION_TITLE)),
    lines[block]
  )]
  created <- length(section_at) == 0L
  if (!created) {
    # Append after the last entry of the existing catch-all section: its
    # contents run until the next `- title:` in the block or the block's end.
    after <- block[block > section_at[[1L]] & grepl("^\\s*-\\s*title:", lines[block])]
    sect_end <- if (length(after) > 0L) after[[1L]] - 1L else block_last
    while (sect_end > section_at[[1L]] && grepl("^\\s*$", lines[sect_end])) {
      sect_end <- sect_end - 1L
    }
    lines <- append(lines, new_items, after = sect_end)
  } else {
    last_content <- block_last
    while (last_content > start && grepl("^\\s*$", lines[last_content])) {
      last_content <- last_content - 1L
    }
    section <- c(
      "",
      sprintf("%s- title: \"%s\"", title_indent, REGEN_SECTION_TITLE),
      sprintf("%s  desc: >", title_indent),
      sprintf(paste0("%s    Added by `tools/pkgdown-reference-gate.R ",
                     "--regenerate`. Move each entry into the section it ",
                     "belongs to."), title_indent),
      sprintf("%s  contents:", title_indent),
      new_items
    )
    lines <- append(lines, section, after = last_content)
  }
  writeLines(lines, path, useBytes = TRUE)

  cat(sprintf("  %s section \"%s\" with %d entry/entries: %s\n",
              if (created) "created" else "extended", REGEN_SECTION_TITLE,
              length(add), paste(add, collapse = ", ")))
  invisible(list(added = add, section_created = created))
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

# The lines `after` holds that `before` did not, PROVIDED every line of
# `before` survives in order; NULL otherwise. This is the shape a --regenerate
# self-test has to assert -- "exactly the one entry, nothing rewritten" -- and a
# set difference cannot say it, because a reordered or duplicated line is
# invisible to a set.
lines_added <- function(before, after) {
  i <- 1L
  extra <- character(0)
  for (ln in after) {
    if (i <= length(before) && identical(ln, before[[i]])) {
      i <- i + 1L
    } else {
      extra <- c(extra, ln)
    }
  }
  if (i <= length(before)) NULL else extra
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

  # --regenerate: the measured defect, repaired in place. The fixture is the
  # `missing` scenario above; after regeneration the gate must pass and the
  # file must differ from its pre-regeneration bytes by exactly the appended
  # catch-all section -- nothing reordered, nothing else touched.
  d <- write_fixture(file.path(base, "regen"), c("alpha", "j_left"), rd_full,
                     c("alpha"))
  before <- readLines(file.path(d, "_pkgdown.yml"))
  if (!any(grepl("absent from", check_repo(d)$violations))) {
    fail("regenerate fixture is not red before regeneration")
  }
  out <- utils::capture.output(res <- regenerate_index(d))
  after <- readLines(file.path(d, "_pkgdown.yml"))
  v <- check_repo(d)$violations
  if (length(v) > 0L) {
    fail(sprintf("gate still fails after --regenerate: %s",
                 paste(v, collapse = "; ")))
  }
  if (!identical(res$added, "joins") || !isTRUE(res$section_created)) {
    fail("regenerate did not report the one missing topic as a new section")
  }
  added <- lines_added(before, after)
  if (is.null(added)) {
    fail("regenerate rewrote or reordered lines that were already there")
  }
  if (!identical(sum(grepl("^\\s*- joins\\s*$", added)), 1L) ||
        !any(grepl(REGEN_SECTION_TITLE, added, fixed = TRUE))) {
    fail(sprintf("regenerate diff is not exactly the catch-all section: %s",
                 paste(added, collapse = " / ")))
  }
  if (!any(grepl("created section", out))) {
    fail("regenerate did not print what it changed")
  }

  # --regenerate, second run: the catch-all already exists, so a further gap
  # joins it as ONE line rather than opening a second section.
  writeLines(sprintf("export(%s)", c("alpha", "j_left", "beta")),
             file.path(d, "NAMESPACE"))
  writeLines(c("\\name{beta}", "\\alias{beta}", "\\title{x}"),
             file.path(d, "man", "beta.Rd"))
  before <- after
  utils::capture.output(res <- regenerate_index(d))
  after <- readLines(file.path(d, "_pkgdown.yml"))
  if (length(check_repo(d)$violations) > 0L || isTRUE(res$section_created) ||
        length(after) != length(before) + 1L ||
        !identical(setdiff(after, before), sprintf("      - %s", "beta"))) {
    fail("regenerate did not extend the existing catch-all by exactly one entry")
  }

  # --regenerate must refuse to invent an entry for an export with no topic.
  d <- write_fixture(file.path(base, "regen-undoc"), c("alpha", "orphan"),
                     rd_full, c("alpha"))
  before <- readLines(file.path(d, "_pkgdown.yml"))
  out <- utils::capture.output(res <- regenerate_index(d))
  if (length(res$added) != 0L ||
        !identical(readLines(file.path(d, "_pkgdown.yml")), before) ||
        !any(grepl("cannot add `orphan`", out, fixed = TRUE))) {
    fail("regenerate wrote an entry for an export that has no Rd topic")
  }

  # --regenerate is a no-op on a complete index.
  d <- write_fixture(file.path(base, "regen-noop"), c("alpha", "j_left"),
                     rd_full, c("alpha", "joins"))
  before <- readLines(file.path(d, "_pkgdown.yml"))
  utils::capture.output(regenerate_index(d))
  if (!identical(readLines(file.path(d, "_pkgdown.yml")), before)) {
    fail("regenerate touched a file that had nothing to regenerate")
  }

  unlink(base, recursive = TRUE)
  cat(paste0("pkgdown-reference-gate self-test: PASS (2 positive + 5 negative ",
             "+ 4 regenerate cases)\n"))
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
  if ("--regenerate" %in% args) {
    cat("pkgdown reference-index gate --regenerate\n")
    regenerate_index(root)
    cat("regenerated _pkgdown.yml\n")
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
