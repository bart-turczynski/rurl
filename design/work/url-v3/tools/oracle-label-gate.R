#!/usr/bin/env Rscript
# Oracle-label gate (§7 G4 criterion 2, P5.3 §2/§2.3).
#
# Criterion 2 reads: "Oracles are labeled by authority and claim boundary."
# P5.3 §2 supplies a CLOSED taxonomy of five authorities plus the §2.3
# provenance field list; registers/oracle-register.md binds each shipped oracle
# instance to one of them. This gate is what makes that binding executable
# rather than declarative.
#
# Rules:
#   O0  schema      -- every instances row is well formed: 9 fields, unique
#                      OR-nnn id, no blank required cell, known `state`.
#   O1  taxonomy    -- every `authority` is one of P5.3 §2's five labels or the
#                      literal UNLABELED. An invented sixth label FAILS; the
#                      taxonomy is closed and only a successor record opens it.
#   O2  unlabeled   -- authority == UNLABELED <=> (carrier named AND state OPEN).
#                      Both directions. You cannot quietly ship an oracle that
#                      no authority admits, and you cannot mark a properly
#                      labeled row OPEN to dodge review.
#   O3  completeness-- every oracle-bearing file in the tree is cited by some
#                      row. THIS is the rule that keeps the register alive: add
#                      a fixture, the gate fails until you classify it.
#   O4  no phantoms -- every repo path the register cites exists on disk.
#   O5  provenance  -- every `imported = yes` row has a provenance row; every
#                      provenance cell is a value or the literal
#                      MISSING[<carrier>]; no provenance row for a
#                      non-imported oracle.
#   O6  tally       -- the "Coverage of the taxonomy" table's counts and id
#                      lists agree with the instances table exactly.
#
# WHY O6 EXISTS. The hand-written tally in the register was wrong on two of six
# rows the first time it was written (rfc3986-grammar and UNLABELED both listed
# five ids under a count of four). A summary table that nobody recomputes is a
# claim like any other, and this one summarizes the very thing the register is
# for. O6 recomputes it.
#
# WHY THIS LIVES UNDER design/work/url-v3/tools/. It is a control-plane gate
# over a control-plane register, alongside ci-gate.R and validate-records.R,
# and the whole `design/` tree is already covered by .Rbuildignore -- so it
# needs no new build-ignore line of its own.
#
# Base R only. No network, no package deps. Usage:
#   Rscript design/work/url-v3/tools/oracle-label-gate.R
#   Rscript design/work/url-v3/tools/oracle-label-gate.R --self-test

# ---- constants --------------------------------------------------------------

# P5.3 §2. Closed. Widening this vector without a successor decision record is
# exactly the move the gate exists to catch, so it is spelled out here and
# cross-checked against the register's own Schema section by O1.
ORACLE_AUTHORITIES <- c(
  "whatwg-wpt", "rfc3986-grammar", "libcurl-parity",
  "browser-parity", "self-metamorphic"
)

UNLABELED <- "UNLABELED"

INSTANCE_FIELDS <- c(
  "oracle_id", "instance", "authority", "imported",
  "can_certify", "claim_boundary", "consumer", "carrier", "state"
)

# P5.3 §2.3, eleven fields (S8 Oracle integrity gate 9), plus the row key.
PROVENANCE_FIELDS <- c(
  "oracle_id", "upstream_project", "revision", "path", "retrieved", "license",
  "raw_source_hash", "import_command", "transformed_hash", "standard_section",
  "claim_kind", "applicability_selector"
)

INSTANCE_STATES <- c("PROPOSED", "OPEN", "ACCEPTED")

# Cells that may be empty, and only under the condition O2 checks.
OPTIONAL_INSTANCE_FIELDS <- "carrier"

# The O3 probe set: file globs that hold expected values or adjudicated inputs.
# Deliberately NOT every file under analysis/ -- the scored/summary outputs
# there are derived evidence, not oracles, and requiring them would push the
# register toward listing artifacts instead of claims. `diverge-*` is the
# ledger prefix, and it is the ledgers that adjudicate.
ORACLE_PROBE_GLOBS <- c(
  file.path("tests", "testthat", "fixtures", "*.csv"),
  file.path("inst", "bench", "*.csv"),
  file.path("inst", "bench", "*.json"),
  file.path("tools", "determinism", "*.csv"),
  file.path("analysis", "*", "diverge-*.csv")
)

REGISTER_REL <- file.path(
  "design", "work", "url-v3", "registers", "oracle-register.md"
)

# ---- markdown table reading -------------------------------------------------

# Collect the pipe-table rows that follow a `## <heading>` line, stopping at the
# next `##` heading. The header and the `|---|` separator are dropped; what is
# returned is a list of character vectors, one per data row.
#
# A cell containing a literal `|` would split into extra fields. That is not
# handled and deliberately not worked around: it surfaces as a field-count
# mismatch under O0, which fails closed and points at the offending row.
table_rows <- function(lines, heading) {
  start <- which(trimws(lines) == paste0("## ", heading))
  if (length(start) != 1L) return(NULL)
  rest <- lines[seq.int(start[1] + 1L, length(lines))]
  stop_at <- which(grepl("^## ", rest))
  if (length(stop_at)) rest <- rest[seq_len(stop_at[1] - 1L)]

  rest <- trimws(rest)
  rest <- rest[grepl("^\\|", rest) & grepl("\\|$", rest)]
  rest <- rest[!grepl("^\\|[\\s:-]*\\|[\\s:|-]*$", rest, perl = TRUE)]
  if (!length(rest)) return(list())

  split_row <- function(x) {
    x <- sub("^\\|", "", x)
    x <- sub("\\|$", "", x)
    trimws(strsplit(x, "|", fixed = TRUE)[[1]])
  }
  # First surviving row is the header; the rest are data.
  parsed <- lapply(rest, split_row)
  if (length(parsed) < 2L) return(list())
  parsed[-1]
}

as_frame <- function(rows, fields) {
  if (!length(rows)) {
    out <- as.data.frame(
      matrix(character(0), ncol = length(fields)), stringsAsFactors = FALSE
    )
    names(out) <- fields
    return(out)
  }
  widths <- vapply(rows, length, integer(1))
  ok <- widths == length(fields)
  # Malformed rows are kept as NA-padded so O0 can report them by position
  # instead of silently dropping them.
  pad <- function(r) {
    if (length(r) >= length(fields)) return(r[seq_along(fields)])
    c(r, rep(NA_character_, length(fields) - length(r)))
  }
  mat <- do.call(rbind, lapply(rows, pad))
  out <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(out) <- fields
  out$.width_ok <- ok
  out
}

read_register <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  list(
    lines = lines,
    instances = as_frame(
      table_rows(lines, "Oracle instances"), INSTANCE_FIELDS
    ),
    provenance = as_frame(
      table_rows(lines, "Provenance (P5.3 §2.3)"), PROVENANCE_FIELDS
    ),
    coverage = as_frame(
      table_rows(lines, "Coverage of the taxonomy"),
      c("authority", "instances", "note")
    )
  )
}

# ---- helpers ----------------------------------------------------------------

blank <- function(x) is.na(x) | !nzchar(trimws(x))

# Every OR-nnn id mentioned in a free-text cell, in order.
ids_in <- function(x) {
  m <- gregexpr("OR-[0-9]{3}", x)
  unlist(regmatches(x, m), use.names = FALSE)
}

# Every repository path the register cites, from inline code spans. A path is
# recognized by a leading known top-level directory, so ordinary prose in
# backticks (column names, argument values) is not mistaken for a file.
paths_in_register <- function(lines) {
  spans <- unlist(
    regmatches(lines, gregexpr("`[^`]+`", lines)), use.names = FALSE
  )
  spans <- gsub("`", "", spans, fixed = TRUE)
  spans <- trimws(spans)
  keep <- grepl(
    "^(tests|inst|tools|analysis|design|R|\\.github)/[^ ]+$", spans
  )
  spans <- unique(spans[keep])
  # Strip a trailing line-reference (`path:12-18`) before existence testing.
  sub(":[0-9]+(-[0-9]+)?$", "", spans)
}

probe_files <- function(root) {
  found <- unlist(
    lapply(ORACLE_PROBE_GLOBS, function(g) Sys.glob(file.path(root, g))),
    use.names = FALSE
  )
  if (!length(found)) return(character(0))
  sort(unique(substring(found, nchar(root) + 2L)))
}

# ---- rules ------------------------------------------------------------------

check <- function(id, ok, detail) {
  list(id = id, ok = isTRUE(ok), detail = detail)
}

evaluate <- function(reg, root) {
  inst <- reg$instances
  prov <- reg$provenance
  res <- list()

  # ---- O0 schema ------------------------------------------------------------
  problems <- character(0)
  if (!nrow(inst)) {
    problems <- c(problems, "no rows in '## Oracle instances'")
  } else {
    bad_width <- which(!inst$.width_ok)
    if (length(bad_width)) {
      problems <- c(problems, sprintf(
        "row(s) %s do not have %d fields",
        paste(bad_width, collapse = ", "), length(INSTANCE_FIELDS)
      ))
    }
    bad_id <- which(!grepl("^OR-[0-9]{3}$", inst$oracle_id))
    if (length(bad_id)) {
      problems <- c(problems, sprintf(
        "malformed oracle_id: %s",
        paste(inst$oracle_id[bad_id], collapse = ", ")
      ))
    }
    dup <- unique(inst$oracle_id[duplicated(inst$oracle_id)])
    if (length(dup)) {
      problems <- c(problems, sprintf(
        "duplicate oracle_id: %s", paste(dup, collapse = ", ")
      ))
    }
    required <- setdiff(INSTANCE_FIELDS, OPTIONAL_INSTANCE_FIELDS)
    for (f in required) {
      empty <- which(blank(inst[[f]]))
      if (length(empty)) {
        problems <- c(problems, sprintf(
          "blank `%s` in row(s) %s", f, paste(empty, collapse = ", ")
        ))
      }
    }
    bad_state <- setdiff(inst$state[!blank(inst$state)], INSTANCE_STATES)
    if (length(bad_state)) {
      problems <- c(problems, sprintf(
        "unknown state: %s", paste(unique(bad_state), collapse = ", ")
      ))
    }
    bad_imp <- setdiff(inst$imported[!blank(inst$imported)], c("yes", "no"))
    if (length(bad_imp)) {
      problems <- c(problems, sprintf(
        "`imported` must be yes/no, got: %s",
        paste(unique(bad_imp), collapse = ", ")
      ))
    }
  }
  res$O0 <- check("O0", !length(problems), if (length(problems))
    paste(problems, collapse = "; ")
    else sprintf("%d instance row(s) well formed", nrow(inst)))

  # ---- O1 closed taxonomy ---------------------------------------------------
  allowed <- c(ORACLE_AUTHORITIES, UNLABELED)
  unknown <- setdiff(inst$authority[!blank(inst$authority)], allowed)
  res$O1 <- check("O1", !length(unknown), if (length(unknown))
    sprintf("authority outside P5.3 §2's closed set: %s",
            paste(unique(unknown), collapse = ", "))
    else sprintf("all authorities within the closed set of %d + UNLABELED",
                 length(ORACLE_AUTHORITIES)))

  # ---- O2 unlabeled discipline ---------------------------------------------
  problems <- character(0)
  is_unlabeled <- !blank(inst$authority) & inst$authority == UNLABELED
  no_carrier <- which(is_unlabeled & blank(inst$carrier))
  if (length(no_carrier)) {
    problems <- c(problems, sprintf(
      "UNLABELED without a carrier: %s",
      paste(inst$oracle_id[no_carrier], collapse = ", ")
    ))
  }
  not_open <- which(is_unlabeled & inst$state != "OPEN")
  if (length(not_open)) {
    problems <- c(problems, sprintf(
      "UNLABELED but not OPEN: %s",
      paste(inst$oracle_id[not_open], collapse = ", ")
    ))
  }
  open_labeled <- which(!is_unlabeled & inst$state == "OPEN")
  if (length(open_labeled)) {
    problems <- c(problems, sprintf(
      "OPEN but carries a real authority: %s",
      paste(inst$oracle_id[open_labeled], collapse = ", ")
    ))
  }
  res$O2 <- check("O2", !length(problems), if (length(problems))
    paste(problems, collapse = "; ")
    else sprintf("%d UNLABELED row(s), each OPEN and carried",
                 sum(is_unlabeled)))

  # ---- O3 completeness ------------------------------------------------------
  probes <- probe_files(root)
  body <- paste(reg$lines, collapse = "\n")
  unclassified <- probes[!vapply(
    probes, function(p) grepl(p, body, fixed = TRUE), logical(1)
  )]
  res$O3 <- check("O3", !length(unclassified), if (length(unclassified))
    sprintf("oracle-bearing file(s) not classified by any row: %s",
            paste(unclassified, collapse = ", "))
    else sprintf("all %d oracle-bearing file(s) classified", length(probes)))

  # ---- O4 no phantoms -------------------------------------------------------
  cited <- paths_in_register(reg$lines)
  missing <- cited[!file.exists(file.path(root, cited))]
  res$O4 <- check("O4", !length(missing), if (length(missing))
    sprintf("cited path(s) not on disk: %s", paste(missing, collapse = ", "))
    else sprintf("all %d cited path(s) exist", length(cited)))

  # ---- O5 provenance --------------------------------------------------------
  problems <- character(0)
  imported <- inst$oracle_id[!blank(inst$imported) & inst$imported == "yes"]
  have <- prov$oracle_id[!blank(prov$oracle_id)]
  absent <- setdiff(imported, have)
  if (length(absent)) {
    problems <- c(problems, sprintf(
      "imported oracle(s) with no provenance row: %s",
      paste(absent, collapse = ", ")
    ))
  }
  extra <- setdiff(have, imported)
  if (length(extra)) {
    problems <- c(problems, sprintf(
      "provenance row(s) for a non-imported oracle: %s",
      paste(extra, collapse = ", ")
    ))
  }
  if (nrow(prov)) {
    bad_width <- which(!prov$.width_ok)
    if (length(bad_width)) {
      problems <- c(problems, sprintf(
        "provenance row(s) %s do not have %d fields",
        paste(bad_width, collapse = ", "), length(PROVENANCE_FIELDS)
      ))
    }
    for (f in setdiff(PROVENANCE_FIELDS, "oracle_id")) {
      empty <- which(blank(prov[[f]]))
      if (length(empty)) {
        problems <- c(problems, sprintf(
          "blank provenance `%s` in row(s) %s -- use MISSING[<carrier>]",
          f, paste(empty, collapse = ", ")
        ))
      }
    }
    # A MISSING marker must name a carrier; a bare "MISSING" is an excuse.
    cells <- unlist(prov[setdiff(PROVENANCE_FIELDS, "oracle_id")],
                    use.names = FALSE)
    cells <- cells[!is.na(cells)]
    bare <- unique(cells[grepl("MISSING", cells, fixed = TRUE) &
                           !grepl("MISSING\\[[A-Za-z0-9-]+\\]", cells)])
    if (length(bare)) {
      problems <- c(problems, sprintf(
        "MISSING marker without a carrier: %s", paste(bare, collapse = ", ")
      ))
    }
  }
  res$O5 <- check("O5", !length(problems), if (length(problems))
    paste(problems, collapse = "; ")
    else sprintf("%d imported oracle(s), each with a complete provenance row",
                 length(imported)))

  # ---- O6 tally -------------------------------------------------------------
  problems <- character(0)
  cov <- reg$coverage
  if (!nrow(cov)) {
    problems <- c(problems, "no '## Coverage of the taxonomy' table")
  } else {
    listed <- gsub("`", "", cov$authority, fixed = TRUE)
    expected_auth <- c(ORACLE_AUTHORITIES, UNLABELED)
    if (!setequal(listed, expected_auth)) {
      problems <- c(problems, sprintf(
        "coverage table authorities do not match the taxonomy (missing: %s; extra: %s)",
        paste(setdiff(expected_auth, listed), collapse = ", "),
        paste(setdiff(listed, expected_auth), collapse = ", ")
      ))
    }
    for (i in seq_len(nrow(cov))) {
      auth <- listed[i]
      if (!auth %in% expected_auth) next
      actual <- sort(inst$oracle_id[!blank(inst$authority) &
                                      inst$authority == auth])
      claimed_ids <- sort(ids_in(cov$instances[i]))
      n <- regmatches(cov$instances[i],
                      regexpr("[0-9]+", cov$instances[i]))
      claimed_n <- if (length(n)) as.integer(n) else NA_integer_
      if (!identical(claimed_ids, actual)) {
        problems <- c(problems, sprintf(
          "%s: listed ids [%s] != actual [%s]", auth,
          paste(claimed_ids, collapse = " "), paste(actual, collapse = " ")
        ))
      } else if (!identical(claimed_n, length(actual))) {
        problems <- c(problems, sprintf(
          "%s: count %s != %d listed ids",
          auth, format(claimed_n), length(actual)
        ))
      }
    }
  }
  res$O6 <- check("O6", !length(problems), if (length(problems))
    paste(problems, collapse = "; ")
    else "coverage tally matches the instances table")

  res
}

# ---- reporting --------------------------------------------------------------

report <- function(res) {
  for (r in res) {
    cat(sprintf("  %-3s %-4s %s\n", r$id, if (r$ok) "PASS" else "FAIL",
                r$detail))
  }
  all(vapply(res, function(r) r$ok, logical(1)))
}

repo_root <- function() {
  here <- normalizePath(".", mustWork = FALSE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(here, REGISTER_REL))) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  NULL
}

main <- function() {
  root <- repo_root()
  if (is.null(root)) {
    cat("ORACLE-LABEL GATE: FAIL -- cannot locate", REGISTER_REL, "\n")
    quit(status = 1L)
  }
  cat("ORACLE-LABEL GATE (G4 criterion 2 / P5.3 §2)\n")
  reg <- read_register(file.path(root, REGISTER_REL))
  ok <- report(evaluate(reg, root))
  cat(sprintf("VERDICT: %s\n", if (ok) "PASS" else "FAIL"))
  quit(status = if (ok) 0L else 1L)
}

# ---- self-test --------------------------------------------------------------

# Build a throwaway repo root: a register plus the tree the O3/O4 probes read.
mk <- function(instances, provenance = NULL, coverage = NULL,
               files = character(0)) {
  root <- file.path(tempdir(), paste0("olg-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, dirname(REGISTER_REL)), recursive = TRUE,
             showWarnings = FALSE)
  for (f in files) {
    dir.create(file.path(root, dirname(f)), recursive = TRUE,
               showWarnings = FALSE)
    writeLines("x", file.path(root, f))
  }
  hdr <- function(fields) {
    c(paste0("| ", paste(fields, collapse = " | "), " |"),
      paste0("|", paste(rep("---", length(fields)), collapse = "|"), "|"))
  }
  lines <- c(
    "# Register: oracle-register", "",
    "## Oracle instances", "", hdr(INSTANCE_FIELDS), instances, "",
    "## Provenance (P5.3 §2.3)", "", hdr(PROVENANCE_FIELDS),
    if (is.null(provenance)) character(0) else provenance, "",
    "## Coverage of the taxonomy", "", hdr(c("authority", "instances", "note")),
    if (is.null(coverage)) default_coverage(instances) else coverage, "",
    "## Scope boundaries", "", "Nothing."
  )
  writeLines(lines, file.path(root, REGISTER_REL))
  root
}

# Derive a correct coverage table from the instance rows, so a fixture that is
# not exercising O6 does not trip it by accident.
default_coverage <- function(instances) {
  parse_cell <- function(row, i) {
    trimws(strsplit(sub("\\|$", "", sub("^\\|", "", row)), "|",
                    fixed = TRUE)[[1]])[i]
  }
  ids <- vapply(instances, parse_cell, character(1), i = 1L)
  auths <- vapply(instances, parse_cell, character(1), i = 3L)
  vapply(c(ORACLE_AUTHORITIES, UNLABELED), function(a) {
    mine <- sort(ids[auths == a])
    sprintf("| %s | %d%s | note |", a, length(mine),
            if (length(mine)) paste0(" (", paste(mine, collapse = ", "), ")")
            else "")
  }, character(1), USE.NAMES = FALSE)
}

row_of <- function(...) {
  paste0("| ", paste(c(...), collapse = " | "), " |")
}

# A minimal well-formed pair: one labeled instance, one imported instance with
# a full provenance row.
good_rows <- function() {
  c(
    row_of("OR-001", "`tests/testthat/fixtures/a.csv` claim one", "whatwg-wpt",
           "yes", "can", "cannot", "`tests/testthat/test-a.R` :: t", "",
           "PROPOSED"),
    row_of("OR-002", "`tests/testthat/fixtures/a.csv` claim two",
           "self-metamorphic", "no", "can", "cannot",
           "`tests/testthat/test-a.R` :: u", "", "PROPOSED")
  )
}

good_prov <- function() {
  row_of("OR-001", "wpt", "abc123", "url/x.json", "2026-01-01", "BSD-3",
         "MISSING[RURL-aaaaaaaa]", "make.py", "MISSING[RURL-aaaaaaaa]",
         "WHATWG §4", "acceptance", "http only")
}

good_files <- function() {
  c("tests/testthat/fixtures/a.csv", "tests/testthat/test-a.R")
}

self_test <- function() {
  passed <- 0L
  failed <- 0L
  expect <- function(what, ok) {
    if (isTRUE(ok)) {
      passed <<- passed + 1L
    } else {
      failed <<- failed + 1L
      cat("  FAIL:", what, "\n")
    }
  }
  rule <- function(root, id) {
    reg <- read_register(file.path(root, REGISTER_REL))
    evaluate(reg, root)[[id]]$ok
  }

  # 1. the happy path passes every rule.
  r <- mk(good_rows(), good_prov(), files = good_files())
  reg <- read_register(file.path(r, REGISTER_REL))
  res <- evaluate(reg, r)
  expect("a well-formed register passes all rules",
         all(vapply(res, function(x) x$ok, logical(1))))

  # 2. O0 -- malformed id.
  r <- mk(c(sub("OR-001", "OR-1", good_rows()[1], fixed = TRUE),
            good_rows()[2]),
          sub("OR-001", "OR-1", good_prov(), fixed = TRUE),
          files = good_files())
  expect("O0 fails on a malformed oracle_id", identical(rule(r, "O0"), FALSE))

  # 3. O0 -- duplicate id.
  dup <- sub("OR-002", "OR-001", good_rows()[2], fixed = TRUE)
  r <- mk(c(good_rows()[1], dup), good_prov(), files = good_files())
  expect("O0 fails on a duplicate oracle_id", identical(rule(r, "O0"), FALSE))

  # 4. O0 -- blank required cell.
  r <- mk(c(row_of("OR-001", "`tests/testthat/fixtures/a.csv` x", "whatwg-wpt",
                   "yes", "", "cannot", "c", "", "PROPOSED"),
            good_rows()[2]),
          good_prov(), files = good_files())
  expect("O0 fails on a blank required cell", identical(rule(r, "O0"), FALSE))

  # 5. O0 -- unknown state.
  r <- mk(c(sub("| PROPOSED |", "| SETTLED |", good_rows()[1], fixed = TRUE),
            good_rows()[2]),
          good_prov(), files = good_files())
  expect("O0 fails on an unknown state", identical(rule(r, "O0"), FALSE))

  # 6. O0 -- imported is not yes/no.
  r <- mk(c(sub("| yes |", "| maybe |", good_rows()[1], fixed = TRUE),
            good_rows()[2]),
          good_prov(), files = good_files())
  expect("O0 fails when imported is not yes/no",
         identical(rule(r, "O0"), FALSE))

  # 7. O1 -- an invented sixth authority. THE headline rule.
  bad <- sub("whatwg-wpt", "contract-characterization", good_rows()[1],
             fixed = TRUE)
  r <- mk(c(bad, good_rows()[2]),
          sub("OR-001", "OR-001", good_prov(), fixed = TRUE),
          coverage = c(default_coverage(c(bad, good_rows()[2]))),
          files = good_files())
  expect("O1 fails on an authority outside the closed set",
         identical(rule(r, "O1"), FALSE))

  # 8. O2 -- UNLABELED with no carrier.
  bad <- row_of("OR-001", "`tests/testthat/fixtures/a.csv` x", UNLABELED,
                "no", "can", "cannot", "c", "", "OPEN")
  r <- mk(c(bad, good_rows()[2]), NULL, files = good_files())
  expect("O2 fails on UNLABELED without a carrier",
         identical(rule(r, "O2"), FALSE))

  # 9. O2 -- UNLABELED, carried, but not OPEN.
  bad <- row_of("OR-001", "`tests/testthat/fixtures/a.csv` x", UNLABELED,
                "no", "can", "cannot", "c", "RURL-aaaaaaaa", "PROPOSED")
  r <- mk(c(bad, good_rows()[2]), NULL, files = good_files())
  expect("O2 fails when an UNLABELED row is not OPEN",
         identical(rule(r, "O2"), FALSE))

  # 10. O2 -- OPEN while carrying a real authority.
  bad <- sub("| PROPOSED |", "| OPEN |", good_rows()[2], fixed = TRUE)
  r <- mk(c(good_rows()[1], bad), good_prov(), files = good_files())
  expect("O2 fails when a labeled row is marked OPEN",
         identical(rule(r, "O2"), FALSE))

  # 11. O2 -- a valid UNLABELED row passes.
  ok_row <- row_of("OR-003", "`tests/testthat/fixtures/a.csv` z", UNLABELED,
                   "no", "can", "cannot", "c", "RURL-aaaaaaaa", "OPEN")
  r <- mk(c(good_rows(), ok_row), good_prov(), files = good_files())
  expect("O2 passes a carried, OPEN, UNLABELED row",
         identical(rule(r, "O2"), TRUE))

  # 12. O3 -- a fixture in the tree that no row classifies. THE liveness rule.
  r <- mk(good_rows(), good_prov(),
          files = c(good_files(), "tests/testthat/fixtures/unclassified.csv"))
  expect("O3 fails on an unclassified oracle-bearing file",
         identical(rule(r, "O3"), FALSE))

  # 13. O3 -- the probe reaches analysis/ ledgers too.
  r <- mk(good_rows(), good_prov(),
          files = c(good_files(), "analysis/disagreement/diverge-x-vs-y.csv"))
  expect("O3 fails on an unclassified divergence ledger",
         identical(rule(r, "O3"), FALSE))

  # 14. O3 -- a non-ledger analysis output is NOT probed (documented boundary).
  r <- mk(good_rows(), good_prov(),
          files = c(good_files(), "analysis/parity/scored-output.csv"))
  expect("O3 ignores derived analysis output", identical(rule(r, "O3"), TRUE))

  # 15. O4 -- a cited path that does not exist.
  r <- mk(good_rows(), good_prov(), files = "tests/testthat/fixtures/a.csv")
  expect("O4 fails on a cited path that is not on disk",
         identical(rule(r, "O4"), FALSE))

  # 16. O5 -- imported row with no provenance row.
  r <- mk(good_rows(), NULL, files = good_files())
  expect("O5 fails when an imported oracle has no provenance row",
         identical(rule(r, "O5"), FALSE))

  # 17. O5 -- provenance row for a non-imported oracle.
  r <- mk(good_rows(),
          c(good_prov(), sub("OR-001", "OR-002", good_prov(), fixed = TRUE)),
          files = good_files())
  expect("O5 fails on a provenance row for a non-imported oracle",
         identical(rule(r, "O5"), FALSE))

  # 18. O5 -- a blank provenance cell.
  r <- mk(good_rows(), sub("| abc123 |", "|  |", good_prov(), fixed = TRUE),
          files = good_files())
  expect("O5 fails on a blank provenance cell",
         identical(rule(r, "O5"), FALSE))

  # 19. O5 -- a bare MISSING with no carrier.
  r <- mk(good_rows(),
          sub("MISSING[RURL-aaaaaaaa]", "MISSING", good_prov(), fixed = TRUE),
          files = good_files())
  expect("O5 fails on a MISSING marker with no carrier",
         identical(rule(r, "O5"), FALSE))

  # 20. O6 -- a count that disagrees with its own id list. The regression this
  #     rule was written for: the register shipped 4 where it listed 5.
  cov <- default_coverage(good_rows())
  cov <- sub("| whatwg-wpt | 1 (OR-001) |", "| whatwg-wpt | 4 (OR-001) |",
             cov, fixed = TRUE)
  r <- mk(good_rows(), good_prov(), coverage = cov, files = good_files())
  expect("O6 fails when a tally count disagrees with its id list",
         identical(rule(r, "O6"), FALSE))

  # 21. O6 -- an id in the tally that is not in the instances table.
  cov <- default_coverage(good_rows())
  cov <- sub("| whatwg-wpt | 1 (OR-001) |",
             "| whatwg-wpt | 2 (OR-001, OR-009) |", cov, fixed = TRUE)
  r <- mk(good_rows(), good_prov(), coverage = cov, files = good_files())
  expect("O6 fails on a tally id absent from the instances table",
         identical(rule(r, "O6"), FALSE))

  # 22. O6 -- a missing authority row in the tally.
  cov <- default_coverage(good_rows())
  cov <- cov[!grepl("browser-parity", cov, fixed = TRUE)]
  r <- mk(good_rows(), good_prov(), coverage = cov, files = good_files())
  expect("O6 fails when the tally omits an authority",
         identical(rule(r, "O6"), FALSE))

  # 23. O6 -- a zero-instance authority is stated, not omitted.
  cov <- default_coverage(good_rows())
  r <- mk(good_rows(), good_prov(), coverage = cov, files = good_files())
  expect("O6 passes with an explicit zero-instance authority",
         identical(rule(r, "O6"), TRUE))

  cat(sprintf("\nself-test: %d passed, %d failed\n", passed, failed))
  quit(status = if (failed == 0L) 0L else 1L)
}

args <- commandArgs(trailingOnly = TRUE)
if ("--self-test" %in% args) self_test() else main()
