#!/usr/bin/env Rscript

# Determinism acceptance gate (rurl 3.0 protocol hardening, G4.2; implements the
# ACCEPTED decision P5.2 / C-09). This is the FAILING cross-platform comparison
# P5.2 mandates -- distinct from, and additive to, the shipped non-gating
# determinism-probe.yml. It compares the canonical parse-dump projections across
# the comparable matrix cells and FAILS the build on any divergence, missing
# evidence, invalid (unarmed) axis, or degraded comparable cell that is not
# covered by an unexpired, signature-matching approved exception.
#
# Tolerance (P5.2 part 2): byte-exact ZERO unapproved divergence. There is no
# fuzzy margin; tolerance is granted ONLY through the approved-exception
# register (P5.2 part 3, registers/determinism-exceptions.md).
#
# Zero dependencies beyond base R (utils, tools). It reduces each cell to a
# canonical projection, groups the comparable+valid cells, and distinguishes a
# per-cell DETECTION hash (does this cell match the group?) from an exception
# SIGNATURE (the fingerprint of a specific keyed row/column/value diff, so an
# exception authorizes an exact diff and cannot silently widen).
#
# Usage:
#   Rscript tools/determinism/compare-gate.R \
#       [--dumps DIR] [--expected CSV] [--exceptions MD] [--manifest-out CSV]
#   Rscript tools/determinism/compare-gate.R --self-test
#
# P5.2 pins TWO invariance axes, and this gate checks both:
#   * ACROSS cells   -- platform invariance (one detection hash for the group).
#   * ACROSS RUNS    -- repeat-run reproducibility ("two pinned runs of the same
#                       comparable cell"). Cross-cell comparison structurally
#                       cannot see a cell that is unstable against ITSELF, since
#                       each cell contributes exactly one observation; the
#                       harness therefore re-runs parse-dump.R in a fresh
#                       process per cell, and this gate requires the pair to
#                       be identical. Run 2 is PERTURBED, not a copy: it feeds
#                       the corpus in a shuffled order under a fixed seed
#                       recorded in its env-<LABEL>.csv (RURL-ptsijueb). Order
#                       must not matter, so the pair must still project
#                       identically -- which makes the axis catch
#                       order-dependence and session-cache state leakage, not
#                       just process/temporal variation.
# Neither axis is a regression check: comparing against a committed baseline
# would test output drift under a declared input (owned by
# tests/testthat/_snaps/characterization-snapshot.md), not determinism.
#
# Verdict categories (a red gate is always a real finding, but not always parser
# nondeterminism -- the manifest distinguishes them):
#   PASS
#   FAIL_NONDETERMINISM    a comparable cell did not reproduce its own output
#   FAIL_DIVERGENCE        comparable cells produced different output
#   FAIL_MISSING_EVIDENCE  an expected comparable cell has no dump/metadata,
#                          or produced no usable repeat run
#   FAIL_INVALID_AXIS      a comparable cell did not arm its charset/locale axis
#   FAIL_DEGRADED          an expected comparable cell reported DEGRADED

# ---- base-R helpers ---------------------------------------------------------

md5_of_string <- function(s) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(paste(s, collapse = "\n")), tf)
  unname(tools::md5sum(tf))
}

# Decode one metadata flag from the probe's on-disk representation. parse-dump.R
# and the probe write every value as a JSON string LITERAL via esc(): a boolean
# is the six-char string "true"/"false" (inner quotes included), and R NA is the
# bare token null (no inner quotes). After the CSV layer round-trips through
# read.csv, a true value reads back as `"true"` (with embedded quotes) and NA as
# `null`. This decodes that exact form and is FAIL-CLOSED: anything else -- a
# bare token, an unexpected value, a missing or duplicated key -- yields NA, so
# an unrecognized cell can never be treated as a valid comparable cell.
decode_meta_flag <- function(kv, key) {
  hit <- kv$value[kv$key == key]
  if (length(hit) != 1L) {
    return(NA)                         # missing or duplicated key: fail-closed
  }
  v <- hit[[1]]
  if (is.na(v) || identical(v, "null")) {
    return(NA)                         # JSON null sentinel
  }
  if (grepl('^".*"$', v)) {
    inner <- tolower(substr(v, 2L, nchar(v) - 1L))
    if (inner == "true") return(TRUE)
    if (inner == "false") return(FALSE)
  }
  NA
}

read_meta_kv <- function(path) {
  utils::read.csv(path, colClasses = "character", check.names = FALSE)
}

# ---- canonical dump projection ---------------------------------------------

# The projection: rows sorted by the (id, url_standard) key, every non-key
# column in a fixed order, rendered as one string per row. Deterministic output
# makes the whole projection byte-identical across cells (the frozen sweep
# proved one MD5 across the matrix), so string equality detects divergence and
# the per-row records feed exact diff signatures.
#
# ORDER-INSENSITIVE BY CONSTRUCTION (RURL-ptsijueb). The sort below already
# canonicalizes row order, and diff_signature() works off the sorted `by_key`
# map, so FILE row order is not observable in `serialized`, `hash` or any
# signature. That is what lets parse-dump.R's repeat run shuffle the corpus:
# no extra "canonical re-sort before diffing" was needed here, and adding a
# second sort would have been dead code. Self-test fixture (22) pins the
# property; (23) pins that it does not also swallow a real value difference.
#
# The one way a permutation COULD leak into the projection is a duplicated
# (id, url_standard) key: order() is stable, so tied rows would keep their file
# order, and `by_key` would silently drop all but one of them. Before the
# shuffle that was invisible (both runs shared one order); now it would read as
# nondeterminism. So it is rejected fail-closed as a MALFORMED dump -- an
# honest evidence defect beats a fabricated parser finding.
dump_projection <- function(path) {
  d <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  key_cols <- c("id", "url_standard")
  if (!all(key_cols %in% names(d))) {
    stop("dump is missing key columns id/url_standard: ", path)
  }
  val_cols <- sort(setdiff(names(d), key_cols))
  ord <- order(d[["id"]], d[["url_standard"]])
  d <- d[ord, , drop = FALSE]
  keys <- paste(d[["id"]], d[["url_standard"]], sep = "\x1f")
  if (anyDuplicated(keys) > 0L) {
    dup <- unique(keys[duplicated(keys)])
    shown <- sub("\x1f", "/", utils::head(dup, 3L), fixed = TRUE)
    stop("dump has duplicate (id, url_standard) key(s), so its row order is ",
         "not canonical: ", toString(shown), " -- ", path)
  }
  rows <- do.call(paste, c(d[val_cols], sep = "\x1f"))
  list(
    key_cols = key_cols, val_cols = val_cols,
    keys = keys, rows = rows,
    serialized = paste(keys, rows, sep = "\x1f"),
    by_key = stats::setNames(rows, keys)
  )
}

# The exact diff between a cell projection and the reference projection, as a
# canonical, order-independent record of every keyed cell that differs. Hashing
# THIS (not the whole-dump hash) is the exception signature: it pins the precise
# divergence and scope, so an exception cannot authorize an unrelated diff.
diff_signature <- function(cell, reference) {
  ref_keys <- names(reference$by_key)
  cell_keys <- names(cell$by_key)
  all_keys <- sort(union(cell_keys, ref_keys))
  deltas <- character(0)
  for (k in all_keys) {
    # `by_key` is a NAMED CHARACTER VECTOR, and `[[` on one ERRORS for an absent
    # name -- it does not return NULL. So presence must be tested explicitly: a
    # key in only one projection means a row appeared or vanished, which is a
    # real, reportable delta, not a crash in the gate before it writes its
    # manifest.
    a <- if (k %in% ref_keys) reference$by_key[[k]] else "<absent>"
    b <- if (k %in% cell_keys) cell$by_key[[k]] else "<absent>"
    if (!identical(a, b)) {
      deltas <- c(deltas, paste(k, a, b, sep = "\x1f"))
    }
  }
  if (length(deltas) == 0L) {
    return(list(fingerprint = "IDENTICAL", n = 0L))
  }
  list(fingerprint = md5_of_string(sort(deltas)), n = length(deltas))
}

# ---- approved-exception register --------------------------------------------

# Parse the append-only determinism-exceptions register: the pipe table under
# `## Exceptions`. Empty (header only) is the correct zero-divergence steady
# state and yields no active exceptions.
parse_exceptions <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(list())
  }
  lines <- readLines(path, warn = FALSE)
  start <- grep("^##\\s+Exceptions", lines)
  if (length(start) == 0L) {
    return(list())
  }
  body <- lines[(start[1] + 1L):length(lines)]
  rows <- grep("^\\s*\\|", body, value = TRUE)
  # Drop the header row and the |---|---| separator.
  rows <- rows[!grepl("^\\s*\\|[-:\\s|]+\\|\\s*$", rows)]
  if (length(rows) < 1L) {
    return(list())
  }
  split_row <- function(r) {
    inner <- sub("^\\s*\\|", "", sub("\\|\\s*$", "", r))
    cells <- strsplit(inner, "|", fixed = TRUE)[[1]]
    trimws(cells)
  }
  header <- split_row(rows[1])
  out <- list()
  for (r in rows[-1]) {
    cells <- split_row(r)
    if (length(cells) != length(header)) next
    out[[length(out) + 1L]] <- stats::setNames(as.list(cells), header)
  }
  out
}

exception_active <- function(ex, today = Sys.Date()) {
  # Fail-closed: only a fully populated, ACCEPTED, unexpired row with a
  # parseable date expiry is active. A malformed row is inactive (no slack).
  # ALL schema fields must be present and non-blank -- justification and
  # tracking_issue included -- matching the register's "a row missing any field
  # does not grant tolerance" contract (registers/determinism-exceptions.md).
  need <- c("exception_id", "owner", "approver", "justification", "scope",
            "signature", "expiry", "tracking_issue", "state")
  if (!all(need %in% names(ex))) return(FALSE)
  if (any(vapply(ex[need], function(v) !nzchar(v) ||
                 grepl("^(TBD|pending|-)$", v), logical(1)))) {
    return(FALSE)
  }
  if (!identical(toupper(ex$state), "ACCEPTED")) return(FALSE)
  d <- suppressWarnings(as.Date(ex$expiry))
  # Expiry is inclusive: the register says the row stops matching ON/after the
  # expiry date, so an exception expiring today is already inactive.
  if (is.na(d) || d <= today) return(FALSE)
  TRUE
}

# A divergence (cell label + signature fingerprint) is covered iff some active
# exception's scope names the label (or "*") and its signature equals the diff.
divergence_covered <- function(label, fingerprint, exceptions,
                               today = Sys.Date()) {
  for (ex in exceptions) {
    if (!exception_active(ex, today)) next
    scope <- trimws(strsplit(ex$scope, "[;, ]+")[[1]])
    if (!("*" %in% scope || label %in% scope)) next
    if (identical(ex$signature, fingerprint)) {
      return(ex$exception_id)
    }
  }
  NA_character_
}

# ---- expected-cell manifest -------------------------------------------------

# Read + VALIDATE the expected-cell manifest, fail-closed. read.csv with the
# default fill=TRUE silently absorbs a malformed row (an unquoted comma in a
# note spills into a phantom row; a short row pads with NA), and a typo'd
# `comparable` value degrades to FALSE -- either of which could drop a real
# comparable cell out of the comparison and let the gate pass on fewer cells.
# So we assert structure before trusting it: every line has exactly the header's
# field count (catches spilled/short rows), labels are unique, `comparable` is
# strictly true/false (catches typos), and at least two comparable cells remain.
read_expected <- function(path) {
  expected <- utils::read.csv(path, colClasses = "character",
                              check.names = FALSE)
  req_cols <- c("label", "os", "r", "locale", "comparable")
  if (!all(req_cols %in% names(expected))) {
    stop("expected-cells manifest missing required column(s): ",
         toString(setdiff(req_cols, names(expected))), call. = FALSE)
  }
  widths <- suppressWarnings(utils::count.fields(path, sep = ",", quote = "\""))
  if (anyNA(widths) || any(widths != ncol(expected))) {
    bad <- which(is.na(widths) | widths != ncol(expected))
    stop(sprintf(
      "expected-cells manifest malformed: line(s) %s have %s field(s), not %d ",
      toString(bad), toString(widths[bad]), ncol(expected)),
      "(unquoted comma or short row?)", call. = FALSE)
  }
  if (anyDuplicated(expected$label)) {
    dup <- unique(expected$label[duplicated(expected$label)])
    stop("expected-cells manifest has duplicate label(s): ", toString(dup),
         call. = FALSE)
  }
  cmp <- tolower(expected$comparable)
  if (!all(cmp %in% c("true", "false"))) {
    bad <- unique(expected$comparable[!cmp %in% c("true", "false")])
    stop("expected-cells 'comparable' must be true/false; bad value(s): ",
         toString(bad), call. = FALSE)
  }
  expected$comparable <- cmp == "true"
  if (sum(expected$comparable) < 2L) {
    stop("expected-cells manifest declares <2 comparable cells; ",
         "nothing to compare (fail-closed)", call. = FALSE)
  }
  expected
}

# ---- the gate ---------------------------------------------------------------

# The repeat-run output tag. MUST stay in step with RURL_DETERMINISM_RUN in
# tools/determinism/gha/_determinism-cells.yml: the harness writes
# `<RURL_DETERMINISM_RUN>-dump-<LABEL>.csv` and the gate reads it back here.
rerun_prefix_default <- "rerun-"

run_gate <- function(dumps_dir, expected_csv, exceptions_md,
                     manifest_out = NULL, today = Sys.Date(),
                     rerun_prefix = rerun_prefix_default) {
  expected <- read_expected(expected_csv)
  exceptions <- parse_exceptions(exceptions_md)

  cells <- list()          # per-cell record
  observed_labels <- sub("^dump-", "", sub("\\.csv$", "",
    list.files(dumps_dir, pattern = "^dump-.*\\.csv$")))

  for (i in seq_len(nrow(expected))) {
    label <- expected$label[i]
    comparable <- expected$comparable[i]
    dump_path <- file.path(dumps_dir, paste0("dump-", label, ".csv"))
    loc_path <- file.path(dumps_dir, paste0("locale-", label, ".csv"))
    degraded <- file.exists(
      file.path(dumps_dir, paste0("DEGRADED-", label, ".txt")))

    rec <- list(label = label, comparable = comparable, status = "OK",
                hash = NA_character_, detail = "", rerun = "")

    if (degraded) {
      rec$status <- if (comparable) "DEGRADED" else "DEGRADED_EVIDENCE"
      rec$detail <- "DEGRADED marker present"
    } else if (file.exists(dump_path)) {
      # Metadata validity. Fail-closed: a comparable cell must literally decode
      # cross_os_comparable=true and charset_as_requested=true (a tr cell must
      # also decode hazard_armed=true). NA -- missing/duplicated/undecodable --
      # is invalid evidence, never a pass. Non-comparable cells are not gated.
      if (file.exists(loc_path)) {
        kv <- read_meta_kv(loc_path)
        cross_ok <- decode_meta_flag(kv, "cross_os_comparable")
        charset_ok <- decode_meta_flag(kv, "charset_as_requested")
        armed <- decode_meta_flag(kv, "hazard_armed")
        is_tr <- identical(expected$locale[i], "tr")
        if (comparable && !isTRUE(cross_ok)) {
          rec$status <- "META_CONFLICT"
          rec$detail <- "cross_os_comparable did not decode to true"
        } else if (comparable && !isTRUE(charset_ok)) {
          rec$status <- "UNARMED_AXIS"
          rec$detail <- "charset_as_requested not true (not like-for-like)"
        } else if (comparable && is_tr && !isTRUE(armed)) {
          rec$status <- "UNARMED_AXIS"
          rec$detail <- "tr cell not hazard_armed (duplicates default)"
        }
      } else if (comparable) {
        rec$status <- "MISSING_META"
        rec$detail <- "no locale-<label>.csv"
      }
      if (rec$status == "OK") {
        proj <- tryCatch(dump_projection(dump_path), error = function(e) e)
        if (inherits(proj, "error")) {
          rec$status <- "MALFORMED"
          rec$detail <- conditionMessage(proj)
        } else {
          rec$projection <- proj
          rec$hash <- md5_of_string(proj$serialized)
        }
      }
    } else {
      rec$status <- if (comparable) "MISSING_DUMP" else "MISSING_EVIDENCE"
      rec$detail <- "no dump-<label>.csv"
    }
    cells[[label]] <- rec
  }

  # Repeat-run reproducibility (P5.2: "two pinned runs of the same comparable
  # cell"). Each comparable cell that produced a valid first dump must also have
  # produced a byte-identical second one, from a fresh process on the same
  # runner. Fail-closed: an absent or unreadable repeat run is missing evidence,
  # never a pass -- a cell that cannot be re-run cannot demonstrate it is
  # reproducible. Only findings are recorded; a reproducible cell is silent.
  reruns <- list()
  for (label in names(cells)) {
    rc <- cells[[label]]
    if (!rc$comparable || !identical(rc$status, "OK") ||
          is.null(rc$projection)) {
      next
    }
    rerun_path <- file.path(dumps_dir,
                            paste0(rerun_prefix, "dump-", label, ".csv"))
    if (!file.exists(rerun_path)) {
      cells[[label]]$rerun <- "MISSING"
      reruns[[label]] <- list(label = label, kind = "MISSING",
                              signature = NA_character_, n_deltas = 0L,
                              covered_by = NA_character_)
      next
    }
    proj2 <- tryCatch(dump_projection(rerun_path), error = function(e) e)
    if (inherits(proj2, "error")) {
      cells[[label]]$rerun <- "MALFORMED"
      reruns[[label]] <- list(label = label, kind = "MALFORMED",
                              signature = NA_character_, n_deltas = 0L,
                              covered_by = NA_character_)
      next
    }
    if (identical(md5_of_string(proj2$serialized), rc$hash)) {
      cells[[label]]$rerun <- "REPRODUCIBLE"
      next
    }
    # Typed like the DEGRADED sentinel. A repeat-run divergence is a different
    # KIND of finding from a cross-cell one, so its authorizing fingerprint is
    # namespaced `RERUN:` -- a cross-cell exception can never silently satisfy
    # a nondeterminism finding that happens to hash to the same delta set, or
    # the reverse. Still scope- AND signature-pinned, so it cannot widen.
    sig <- diff_signature(proj2, rc$projection)
    fingerprint <- paste0("RERUN:", sig$fingerprint)
    covered <- divergence_covered(label, fingerprint, exceptions, today)
    reruns[[label]] <- list(label = label, kind = "NONDETERMINISTIC",
                            signature = fingerprint, n_deltas = sig$n,
                            covered_by = covered)
    cells[[label]]$rerun <-
      if (is.na(covered)) "NONDETERMINISTIC" else "NONDETERMINISTIC_COVERED"
  }

  # Unexpected observed cells (a dump with no expected-cell row).
  unexpected <- setdiff(observed_labels, expected$label)

  # Comparable, valid cells enter the equality comparison.
  valid <- Filter(function(c) c$comparable && c$status == "OK" &&
                    !is.null(c$projection), cells)
  divergences <- list()
  if (length(valid) >= 2L) {
    hashes <- vapply(valid, function(c) c$hash, character(1))
    ref_hash <- names(sort(table(hashes), decreasing = TRUE))[1]
    ref_cell <- valid[[which(hashes == ref_hash)[1]]]
    for (label in names(valid)) {
      c <- valid[[label]]
      if (identical(c$hash, ref_hash)) next
      sig <- diff_signature(c$projection, ref_cell$projection)
      covered <- divergence_covered(label, sig$fingerprint, exceptions, today)
      divergences[[label]] <- list(
        label = label, signature = sig$fingerprint, n_deltas = sig$n,
        covered_by = covered
      )
      cells[[label]]$status <-
        if (is.na(covered)) "DIVERGENT" else "DIVERGENT_COVERED"
      cells[[label]]$detail <- sprintf("signature=%s deltas=%d%s",
        sig$fingerprint, sig$n,
        if (is.na(covered)) "" else paste0(" covered_by=", covered))
    }
  }

  # A DEGRADED comparable cell is a failure UNLESS a registered exception covers
  # the gap (P5.2 part 1: "unless that gap is itself a registered exception").
  # A gap has no output to diff, so its authorizing fingerprint is the typed
  # sentinel `DEGRADED:<label>` -- still scope- AND signature-pinned to that
  # exact absent cell, so an exception cannot widen to an output divergence or
  # another cell. Reuses the same active-exception matching as a divergence.
  degraded_gaps <- list()
  for (label in names(cells)) {
    dc <- cells[[label]]
    if (!(dc$comparable && identical(dc$status, "DEGRADED"))) next
    sig <- paste0("DEGRADED:", label)
    covered <- divergence_covered(label, sig, exceptions, today)
    degraded_gaps[[label]] <- list(label = label, signature = sig,
                                   covered_by = covered)
    if (!is.na(covered)) {
      cells[[label]]$status <- "DEGRADED_COVERED"
      cells[[label]]$detail <- paste0("DEGRADED covered_by=", covered)
    }
  }

  # Verdict. Missing/degraded/invalid are distinct failure classes from
  # divergence (P5.2: an un-runnable comparable cell cannot prove determinism).
  fail_missing <- Filter(function(c) c$comparable &&
    c$status %in% c("MISSING_DUMP", "MISSING_META"), cells)
  fail_degraded <- Filter(
    function(c) c$comparable && c$status == "DEGRADED", cells)
  fail_invalid <- Filter(function(c) c$comparable &&
    c$status %in% c("UNARMED_AXIS", "META_CONFLICT", "MALFORMED"), cells)
  uncovered <- Filter(function(d) is.na(d$covered_by), divergences)
  # An unusable repeat run is an evidence gap, not a divergence: we never got
  # the second observation, so nothing was compared.
  fail_rerun_evidence <- Filter(
    function(r) r$kind %in% c("MISSING", "MALFORMED"), reruns)
  fail_nondet <- Filter(
    function(r) r$kind == "NONDETERMINISTIC" && is.na(r$covered_by), reruns)

  # FAIL_NONDETERMINISM outranks FAIL_DIVERGENCE: a cell that cannot reproduce
  # its own output makes its cross-cell hash unreliable evidence, so report the
  # more fundamental finding first. (The unstable cell is deliberately left in
  # the cross-cell comparison so its divergence, if any, is still reported.)
  verdict <- if (length(fail_nondet) > 0L) {
    "FAIL_NONDETERMINISM"
  } else if (length(uncovered) > 0L) {
    "FAIL_DIVERGENCE"
  } else if (length(fail_missing) > 0L || length(unexpected) > 0L ||
               length(fail_rerun_evidence) > 0L) {
    "FAIL_MISSING_EVIDENCE"
  } else if (length(fail_invalid) > 0L) {
    "FAIL_INVALID_AXIS"
  } else if (length(fail_degraded) > 0L) {
    "FAIL_DEGRADED"
  } else {
    "PASS"
  }

  manifest <- data.frame(
    label = vapply(cells, function(c) c$label, character(1)),
    comparable = vapply(cells, function(c) c$comparable, logical(1)),
    status = vapply(cells, function(c) c$status, character(1)),
    hash = vapply(cells, function(c) c$hash, character(1)),
    rerun = vapply(cells, function(c) c$rerun, character(1)),
    detail = vapply(cells, function(c) c$detail, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  if (!is.null(manifest_out)) {
    dir.create(dirname(manifest_out), showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(manifest, manifest_out, row.names = FALSE)
  }

  list(verdict = verdict, manifest = manifest, divergences = divergences,
       reruns = reruns, unexpected = unexpected,
       n_comparable = sum(expected$comparable),
       n_valid = length(valid),
       n_reproducible = sum(vapply(cells,
         function(c) identical(c$rerun, "REPRODUCIBLE"), logical(1))))
}

print_result <- function(res) {
  cat("== rurl 3.0 determinism acceptance gate (P5.2 / C-09) ==\n")
  cat(sprintf("comparable cells expected: %d ; valid & compared: %d\n",
              res$n_comparable, res$n_valid))
  cat(sprintf("repeat runs reproduced byte-identically: %d\n",
              res$n_reproducible))
  print(res$manifest)
  if (length(res$unexpected) > 0L) {
    cat("UNEXPECTED cells (no expected-cell row):\n  ",
        toString(res$unexpected), "\n", sep = "")
  }
  if (length(res$divergences) > 0L) {
    cat("DIVERGENCES (across cells):\n")
    for (d in res$divergences) {
      cat(sprintf("  - %s: signature=%s deltas=%d covered_by=%s\n",
                  d$label, d$signature, d$n_deltas,
                  if (is.na(d$covered_by)) "NONE" else d$covered_by))
    }
  }
  if (length(res$reruns) > 0L) {
    cat("REPEAT-RUN FINDINGS (same cell, second process):\n")
    for (r in res$reruns) {
      if (identical(r$kind, "NONDETERMINISTIC")) {
        cat(sprintf("  - %s: NONDETERMINISTIC signature=%s deltas=%d",
                    r$label, r$signature, r$n_deltas),
            sprintf(" covered_by=%s\n",
                    if (is.na(r$covered_by)) "NONE" else r$covered_by),
            sep = "")
      } else {
        cat(sprintf("  - %s: repeat run %s (no second observation)\n",
                    r$label, r$kind))
      }
    }
  }
  cat("VERDICT:", res$verdict, "\n")
}

# ---- self-test (positive + negative fixtures) -------------------------------

self_test <- function() {
  fail <- function(m) stop("self-test FAILED: ", m, call. = FALSE)
  root <- tempfile("detgate-"); dir.create(root)
  exp_csv <- file.path(root, "expected.csv")
  utils::write.csv(data.frame(
    label = c("gha-A-utf8", "gha-B-utf8", "gha-C-tr", "gha-D-default"),
    os = "x", r = "release",
    locale = c("utf8", "utf8", "tr", "default"),
    comparable = c("true", "true", "true", "false"),
    note = "", stringsAsFactors = FALSE
  ), exp_csv, row.names = FALSE)

  write_cell <- function(dir, label, rows, comparable = TRUE,
                         charset_ok = TRUE, armed = TRUE, degraded = FALSE,
                         rerun_rows = rows, rerun = TRUE, rerun_perm = NULL) {
    if (degraded) {
      writeLines("SKIPPED", file.path(dir, paste0("DEGRADED-", label, ".txt")))
      return(invisible())
    }
    mk_dump <- function(r) {
      data.frame(id = seq_along(r), url_standard = "whatwg", host = r,
                 stringsAsFactors = FALSE)
    }
    utils::write.csv(mk_dump(rows),
                     file.path(dir, paste0("dump-", label, ".csv")),
                     row.names = FALSE)
    # Every cell also gets a repeat run, identical by default: that is the
    # reproducible steady state. Fixtures opt out (rerun = FALSE) or perturb it
    # to exercise the repeat-run axis -- `rerun_rows` changes VALUES, while
    # `rerun_perm` reorders whole ROWS (ids travel with their values, which is
    # what parse-dump.R's seeded corpus shuffle produces and what must NOT
    # register as a finding).
    if (isTRUE(rerun)) {
      rr <- mk_dump(rerun_rows)
      if (!is.null(rerun_perm)) {
        rr <- rr[rerun_perm, , drop = FALSE]
      }
      utils::write.csv(
        rr,
        file.path(dir, paste0(rerun_prefix_default, "dump-", label, ".csv")),
        row.names = FALSE)
    }
    # Encode flags exactly as the probe does: esc() renders a boolean as the
    # JSON string literal "true"/"false" (inner quotes included); write.csv then
    # quotes the field. This is the real producer representation the decoder
    # must invert -- fixtures that wrote bare booleans hid the encoding.
    esc_flag <- function(b) if (isTRUE(b)) "\"true\"" else "\"false\""
    meta <- data.frame(
      key = c("cross_os_comparable", "charset_as_requested", "hazard_armed"),
      value = c(esc_flag(comparable), esc_flag(charset_ok), esc_flag(armed)),
      stringsAsFactors = FALSE)
    utils::write.csv(meta, file.path(dir, paste0("locale-", label, ".csv")),
                     row.names = FALSE)
  }

  good <- c("a.com", "b.com", "c.com")
  mk <- function() {
    d <- file.path(root, basename(tempfile()))
    dir.create(d)
    d
  }

  # Write a one-row exception register (header + separator + the given row).
  reg_hdr <- paste("| exception_id | owner | approver | justification |",
                   "scope | signature | expiry | tracking_issue | state |")
  reg_sep <- "|---|---|---|---|---|---|---|---|---|"
  write_reg <- function(name, scope, signature, expiry, state,
                        just = "j", track = "RURL-x") {
    path <- file.path(root, name)
    row <- sprintf("| DET-EX-1 | o | a | %s | %s | %s | %s | %s | %s |",
                   just, scope, signature, expiry, track, state)
    writeLines(c("## Exceptions", "", reg_hdr, reg_sep, row), path)
    path
  }

  # (1) all comparable cells identical -> PASS.
  d1 <- mk()
  write_cell(d1, "gha-A-utf8", good); write_cell(d1, "gha-B-utf8", good)
  write_cell(d1, "gha-C-tr", good)
  write_cell(d1, "gha-D-default", c("x", "y", "z"), comparable = FALSE)
  r1 <- run_gate(d1, exp_csv, NULL)
  if (r1$verdict != "PASS") fail(paste("identical cells not PASS:", r1$verdict))

  # (2) one comparable cell diverges, no exception -> FAIL_DIVERGENCE.
  d2 <- mk()
  write_cell(d2, "gha-A-utf8", good); write_cell(d2, "gha-B-utf8", good)
  write_cell(d2, "gha-C-tr", c("a.com", "B.COM", "c.com"))
  write_cell(d2, "gha-D-default", good, comparable = FALSE)
  r2 <- run_gate(d2, exp_csv, NULL)
  if (r2$verdict != "FAIL_DIVERGENCE") {
    fail(paste("divergence not caught:", r2$verdict))
  }
  sig <- r2$divergences[["gha-C-tr"]]$signature

  # (3) divergence covered by a matching, unexpired ACCEPTED exception -> PASS.
  reg <- write_reg("exc.md", "gha-C-tr", sig, "2099-01-01", "ACCEPTED")
  r3 <- run_gate(d2, exp_csv, reg)
  if (r3$verdict != "PASS") {
    fail(paste("covered divergence not PASS:", r3$verdict))
  }

  # (4) expired exception -> divergence fails again.
  reg_exp <- write_reg("exc-expired.md", "gha-C-tr", sig, "2000-01-01",
                       "ACCEPTED")
  if (run_gate(d2, exp_csv, reg_exp)$verdict != "FAIL_DIVERGENCE") {
    fail("expired exception still matched")
  }

  # (5) wrong-signature exception -> does not match.
  reg_wrong <- write_reg("exc-wrong.md", "gha-C-tr", "deadbeef", "2099-01-01",
                         "ACCEPTED")
  if (run_gate(d2, exp_csv, reg_wrong)$verdict != "FAIL_DIVERGENCE") {
    fail("wrong-signature exception matched")
  }

  # (6) DEGRADED comparable cell -> FAIL_DEGRADED.
  d6 <- mk()
  write_cell(d6, "gha-A-utf8", good); write_cell(d6, "gha-B-utf8", good)
  write_cell(d6, "gha-C-tr", good, degraded = TRUE)
  write_cell(d6, "gha-D-default", good, comparable = FALSE)
  if (run_gate(d6, exp_csv, NULL)$verdict != "FAIL_DEGRADED") {
    fail("degraded comparable cell not caught")
  }

  # (7) missing comparable dump -> FAIL_MISSING_EVIDENCE.
  d7 <- mk()
  write_cell(d7, "gha-A-utf8", good); write_cell(d7, "gha-B-utf8", good)
  write_cell(d7, "gha-D-default", good, comparable = FALSE) # C absent
  if (run_gate(d7, exp_csv, NULL)$verdict != "FAIL_MISSING_EVIDENCE") {
    fail("missing comparable dump not caught")
  }

  # (8) unarmed tr axis (charset ok, hazard not armed) -> FAIL_INVALID_AXIS.
  d8 <- mk()
  write_cell(d8, "gha-A-utf8", good); write_cell(d8, "gha-B-utf8", good)
  write_cell(d8, "gha-C-tr", good, armed = FALSE)
  write_cell(d8, "gha-D-default", good, comparable = FALSE)
  if (run_gate(d8, exp_csv, NULL)$verdict != "FAIL_INVALID_AXIS") {
    fail("unarmed tr axis not caught")
  }

  # (9) metadata decoding round-trips the probe's exact esc() + write.csv
  # representation, and is fail-closed on the bare/absent forms.
  d9 <- mk()
  m9 <- data.frame(
    key = c("cross_os_comparable", "charset_as_requested", "hazard_armed",
            "na_flag"),
    value = c("\"true\"", "\"false\"", "\"true\"", "null"),
    stringsAsFactors = FALSE)
  utils::write.csv(m9, file.path(d9, "locale-x.csv"), row.names = FALSE)
  kv9 <- read_meta_kv(file.path(d9, "locale-x.csv"))
  if (!isTRUE(decode_meta_flag(kv9, "cross_os_comparable"))) {
    fail("did not decode probe-encoded true")
  }
  if (!isFALSE(decode_meta_flag(kv9, "charset_as_requested"))) {
    fail("did not decode probe-encoded false")
  }
  if (!is.na(decode_meta_flag(kv9, "na_flag"))) fail("null not decoded to NA")
  if (!is.na(decode_meta_flag(kv9, "absent"))) fail("absent key not NA")
  m9b <- data.frame(key = "cross_os_comparable", value = "true",
                    stringsAsFactors = FALSE)
  utils::write.csv(m9b, file.path(d9, "locale-y.csv"), row.names = FALSE)
  kv9b <- read_meta_kv(file.path(d9, "locale-y.csv"))
  if (!is.na(decode_meta_flag(kv9b, "cross_os_comparable"))) {
    fail("bare (unquoted) boolean was not fail-closed")
  }

  # (10) exception with a BLANK required governance field (justification) does
  # not grant tolerance -- the register mandates every field be populated.
  reg_blank <- write_reg("exc-blankjust.md", "gha-C-tr", sig, "2099-01-01",
                         "ACCEPTED", just = "")
  if (run_gate(d2, exp_csv, reg_blank)$verdict != "FAIL_DIVERGENCE") {
    fail("exception with blank justification still matched")
  }

  # (11) inclusive expiry boundary: an exception is active the day BEFORE its
  # expiry and inactive ON the expiry date (register: "on/after it stops
  # matching").
  reg_bound <- write_reg("exc-boundary.md", "gha-C-tr", sig, "2099-06-15",
                         "ACCEPTED")
  if (run_gate(d2, exp_csv, reg_bound,
               today = as.Date("2099-06-14"))$verdict != "PASS") {
    fail("exception inactive the day before expiry")
  }
  if (run_gate(d2, exp_csv, reg_bound,
               today = as.Date("2099-06-15"))$verdict != "FAIL_DIVERGENCE") {
    fail("exception still active ON its expiry date (off-by-one)")
  }

  # (12) malformed manifest -- a non-boolean `comparable` value is rejected
  # (fail-closed) rather than silently degrading to non-comparable.
  bad_bool <- file.path(root, "bad-bool.csv")
  utils::write.csv(data.frame(
    label = c("gha-A-utf8", "gha-B-utf8"), os = "x", r = "release",
    locale = "utf8", comparable = c("true", "treu"), note = "",
    stringsAsFactors = FALSE), bad_bool, row.names = FALSE)
  if (!inherits(tryCatch(run_gate(d1, bad_bool, NULL),
                         error = function(e) e), "error")) {
    fail("manifest with a non-boolean comparable value was not rejected")
  }

  # (13) malformed manifest -- an unquoted comma spilling a note into a phantom
  # row (a field-width mismatch) is rejected, not silently absorbed.
  bad_width <- file.path(root, "bad-width.csv")
  writeLines(c("label,os,r,locale,comparable,note",
               "gha-A-utf8,x,release,utf8,true,note with, comma",
               "gha-B-utf8,x,release,utf8,true,ok"), bad_width)
  if (!inherits(tryCatch(run_gate(d1, bad_width, NULL),
                         error = function(e) e), "error")) {
    fail("manifest with a field-width mismatch was not rejected")
  }

  # (14) a DEGRADED comparable cell COVERED by a matching sentinel exception
  # (signature DEGRADED:<label>) -> PASS: a governed, cell-pinned gap allowance.
  reg_deg <- write_reg("exc-degraded.md", "gha-C-tr", "DEGRADED:gha-C-tr",
                       "2099-01-01", "ACCEPTED")
  if (run_gate(d6, exp_csv, reg_deg)$verdict != "PASS") {
    fail("DEGRADED cell with a matching sentinel exception not PASS")
  }

  # (15) the DEGRADED sentinel is signature-pinned: a wrong sentinel does not
  # cover the gap, so the cell still fails.
  reg_deg_wrong <- write_reg("exc-degraded-wrong.md", "gha-C-tr",
                             "DEGRADED:gha-OTHER", "2099-01-01", "ACCEPTED")
  if (run_gate(d6, exp_csv, reg_deg_wrong)$verdict != "FAIL_DEGRADED") {
    fail("DEGRADED cell with a wrong sentinel exception not FAIL_DEGRADED")
  }

  # (16) a comparable cell whose SECOND run differs from its first ->
  # FAIL_NONDETERMINISM. Cross-cell comparison is blind to this: every cell
  # still agrees with the group on its first run, which is exactly the hole the
  # repeat-run axis exists to close.
  d16 <- mk()
  write_cell(d16, "gha-A-utf8", good); write_cell(d16, "gha-B-utf8", good)
  write_cell(d16, "gha-C-tr", good, rerun_rows = c("a.com", "b.com", "C.COM"))
  write_cell(d16, "gha-D-default", good, comparable = FALSE)
  r16 <- run_gate(d16, exp_csv, NULL)
  if (r16$verdict != "FAIL_NONDETERMINISM") {
    fail(paste("repeat-run divergence not caught:", r16$verdict))
  }
  rsig <- r16$reruns[["gha-C-tr"]]$signature
  if (!grepl("^RERUN:", rsig)) fail("repeat-run signature is not namespaced")

  # (17) a repeat-run divergence covered by a matching, unexpired exception ->
  # PASS: tolerance is granted the same governed way on both axes.
  reg_rerun <- write_reg("exc-rerun.md", "gha-C-tr", rsig, "2099-01-01",
                         "ACCEPTED")
  if (run_gate(d16, exp_csv, reg_rerun)$verdict != "PASS") {
    fail("repeat-run divergence with a matching exception not PASS")
  }

  # (18) the RERUN: namespace is load-bearing -- the same delta set registered
  # WITHOUT the prefix (i.e. as a cross-cell divergence) must not grant
  # tolerance for a nondeterminism finding.
  reg_bare <- write_reg("exc-rerun-bare.md", "gha-C-tr",
                        sub("^RERUN:", "", rsig), "2099-01-01", "ACCEPTED")
  if (run_gate(d16, exp_csv, reg_bare)$verdict != "FAIL_NONDETERMINISM") {
    fail("un-namespaced exception covered a repeat-run divergence")
  }

  # (19) a comparable cell with a valid first dump but NO repeat run is missing
  # evidence, not a pass: a cell that cannot be re-run proves nothing.
  d19 <- mk()
  write_cell(d19, "gha-A-utf8", good); write_cell(d19, "gha-B-utf8", good)
  write_cell(d19, "gha-C-tr", good, rerun = FALSE)
  write_cell(d19, "gha-D-default", good, comparable = FALSE)
  if (run_gate(d19, exp_csv, NULL)$verdict != "FAIL_MISSING_EVIDENCE") {
    fail("comparable cell without a repeat run not caught")
  }

  # (20) divergence by ROW SET, not just row value: a cell with an extra keyed
  # row must yield a signature naming the appearance, not error out. `[[` on a
  # named vector throws for an absent name, so an unguarded lookup crashed the
  # gate here -- before the manifest was written, destroying the diagnostic.
  d20 <- mk()
  write_cell(d20, "gha-A-utf8", good); write_cell(d20, "gha-B-utf8", good)
  write_cell(d20, "gha-C-tr", c(good, "extra.com"))
  write_cell(d20, "gha-D-default", good, comparable = FALSE)
  r20 <- run_gate(d20, exp_csv, NULL)
  if (r20$verdict != "FAIL_DIVERGENCE") {
    fail(paste("row-set divergence not caught:", r20$verdict))
  }
  if (r20$divergences[["gha-C-tr"]]$n_deltas != 1L) {
    fail("row-set divergence did not report exactly one appeared row")
  }

  # (21) the same asymmetry on the repeat-run axis: a second run that DROPS a
  # row is nondeterminism, and must survive signature computation.
  d21 <- mk()
  write_cell(d21, "gha-A-utf8", good); write_cell(d21, "gha-B-utf8", good)
  write_cell(d21, "gha-C-tr", good, rerun_rows = good[-3])
  write_cell(d21, "gha-D-default", good, comparable = FALSE)
  if (run_gate(d21, exp_csv, NULL)$verdict != "FAIL_NONDETERMINISM") {
    fail("repeat run with a dropped row not caught")
  }

  # (22) PERTURBED repeat run, order-INSENSITIVE case (RURL-ptsijueb). Run 2
  # now feeds the corpus in a seeded shuffled order, so its dump's ROW ORDER
  # legitimately differs from run 1's. A pure row permutation must PASS: order
  # is not part of the contract, and a false red here would make the whole
  # perturbation unusable. This is NOT a vacuous pass -- the cell must be
  # compared and found REPRODUCIBLE, so the reproduced count is asserted too
  # (a gate that "passes" by never pairing the runs proves nothing).
  d22 <- mk()
  write_cell(d22, "gha-A-utf8", good); write_cell(d22, "gha-B-utf8", good)
  write_cell(d22, "gha-C-tr", good, rerun_perm = c(3L, 1L, 2L))
  write_cell(d22, "gha-D-default", good, comparable = FALSE)
  r22 <- run_gate(d22, exp_csv, NULL)
  if (r22$verdict != "PASS") {
    fail(paste("row-permuted repeat run read as a finding:", r22$verdict))
  }
  if (r22$n_reproducible != 3L) {
    fail(sprintf("permuted repeat run not actually compared (%d of 3 pairs)",
                 r22$n_reproducible))
  }
  if (length(r22$reruns) != 0L) fail("permutation produced a rerun finding")

  # (23) the other half of the pair: a repeat run that is BOTH permuted AND
  # differs in one VALUE is still FAIL_NONDETERMINISM. Without this, (22) alone
  # would be satisfied by a gate that had simply stopped looking at the repeat
  # run. The delta count is asserted exactly: the permutation must contribute
  # ZERO deltas, so the single changed cell is the only finding.
  d23 <- mk()
  write_cell(d23, "gha-A-utf8", good); write_cell(d23, "gha-B-utf8", good)
  write_cell(d23, "gha-C-tr", good, rerun_rows = c("a.com", "b.com", "C.COM"),
             rerun_perm = c(2L, 3L, 1L))
  write_cell(d23, "gha-D-default", good, comparable = FALSE)
  r23 <- run_gate(d23, exp_csv, NULL)
  if (r23$verdict != "FAIL_NONDETERMINISM") {
    fail(paste("value divergence hidden by a permutation:", r23$verdict))
  }
  if (r23$reruns[["gha-C-tr"]]$n_deltas != 1L) {
    fail(sprintf("permutation leaked into the delta set (%d deltas, want 1)",
                 r23$reruns[["gha-C-tr"]]$n_deltas))
  }

  # (24) the projection's order-insensitivity rests on the key being unique. A
  # dump with a duplicated (id, url_standard) key has no canonical row order,
  # so under a shuffled run 2 it would manufacture a nondeterminism finding out
  # of nothing. Rejected fail-closed as MALFORMED evidence instead.
  d24 <- mk()
  write_cell(d24, "gha-A-utf8", good); write_cell(d24, "gha-B-utf8", good)
  write_cell(d24, "gha-C-tr", good)
  write_cell(d24, "gha-D-default", good, comparable = FALSE)
  utils::write.csv(
    data.frame(id = c(1L, 1L, 2L), url_standard = "whatwg", host = good,
               stringsAsFactors = FALSE),
    file.path(d24, "dump-gha-C-tr.csv"), row.names = FALSE)
  if (run_gate(d24, exp_csv, NULL)$verdict != "FAIL_INVALID_AXIS") {
    fail("duplicate-key dump was not rejected as malformed evidence")
  }

  cat("determinism compare-gate self-test: PASS (24 fixtures)\n")
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }
  getopt <- function(flag, default) {
    i <- match(flag, args)
    if (is.na(i) || i == length(args)) default else args[i + 1L]
  }
  dumps_dir <- getopt("--dumps", "tools/determinism/out")
  expected_csv <- getopt("--expected", "tools/determinism/expected-cells.csv")
  exceptions_md <- getopt("--exceptions",
    "design/work/url-v3/registers/determinism-exceptions.md")
  manifest_out <- getopt("--manifest-out",
    file.path(dumps_dir, "gate-manifest.csv"))

  res <- run_gate(dumps_dir, expected_csv, exceptions_md, manifest_out)
  print_result(res)
  cat("manifest written:", manifest_out, "\n")
  if (res$verdict != "PASS") {
    stop("determinism gate: ", res$verdict, call. = FALSE)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
