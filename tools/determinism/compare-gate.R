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
# Verdict categories (a red gate is always a real finding, but not always parser
# nondeterminism -- the manifest distinguishes them):
#   PASS
#   FAIL_DIVERGENCE        comparable cells produced different output
#   FAIL_MISSING_EVIDENCE  an expected comparable cell has no dump/metadata
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
  all_keys <- sort(union(names(cell$by_key), names(reference$by_key)))
  deltas <- character(0)
  for (k in all_keys) {
    a <- reference$by_key[[k]]
    b <- cell$by_key[[k]]
    if (is.null(a)) a <- "<absent>"
    if (is.null(b)) b <- "<absent>"
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
  need <- c("exception_id", "owner", "approver", "scope", "signature",
            "expiry", "state")
  if (!all(need %in% names(ex))) return(FALSE)
  if (any(vapply(ex[need], function(v) !nzchar(v) ||
                 grepl("^(TBD|pending|-)$", v), logical(1)))) {
    return(FALSE)
  }
  if (!identical(toupper(ex$state), "ACCEPTED")) return(FALSE)
  d <- suppressWarnings(as.Date(ex$expiry))
  if (is.na(d) || d < today) return(FALSE)
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

# ---- the gate ---------------------------------------------------------------

run_gate <- function(dumps_dir, expected_csv, exceptions_md,
                     manifest_out = NULL, today = Sys.Date()) {
  expected <- utils::read.csv(expected_csv, colClasses = "character",
                              check.names = FALSE)
  expected$comparable <- tolower(expected$comparable) == "true"
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
                hash = NA_character_, detail = "")

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

  # Verdict. Missing/degraded/invalid are distinct failure classes from
  # divergence (P5.2: an un-runnable comparable cell cannot prove determinism).
  fail_missing <- Filter(function(c) c$comparable &&
    c$status %in% c("MISSING_DUMP", "MISSING_META"), cells)
  fail_degraded <- Filter(
    function(c) c$comparable && c$status == "DEGRADED", cells)
  fail_invalid <- Filter(function(c) c$comparable &&
    c$status %in% c("UNARMED_AXIS", "META_CONFLICT", "MALFORMED"), cells)
  uncovered <- Filter(function(d) is.na(d$covered_by), divergences)

  verdict <- if (length(uncovered) > 0L) {
    "FAIL_DIVERGENCE"
  } else if (length(fail_missing) > 0L || length(unexpected) > 0L) {
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
    detail = vapply(cells, function(c) c$detail, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
  if (!is.null(manifest_out)) {
    dir.create(dirname(manifest_out), showWarnings = FALSE, recursive = TRUE)
    utils::write.csv(manifest, manifest_out, row.names = FALSE)
  }

  list(verdict = verdict, manifest = manifest, divergences = divergences,
       unexpected = unexpected,
       n_comparable = sum(expected$comparable),
       n_valid = length(valid))
}

print_result <- function(res) {
  cat("== rurl 3.0 determinism acceptance gate (P5.2 / C-09) ==\n")
  cat(sprintf("comparable cells expected: %d ; valid & compared: %d\n",
              res$n_comparable, res$n_valid))
  print(res$manifest)
  if (length(res$unexpected) > 0L) {
    cat("UNEXPECTED cells (no expected-cell row):\n  ",
        toString(res$unexpected), "\n", sep = "")
  }
  if (length(res$divergences) > 0L) {
    cat("DIVERGENCES:\n")
    for (d in res$divergences) {
      cat(sprintf("  - %s: signature=%s deltas=%d covered_by=%s\n",
                  d$label, d$signature, d$n_deltas,
                  if (is.na(d$covered_by)) "NONE" else d$covered_by))
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
                         charset_ok = TRUE, armed = TRUE, degraded = FALSE) {
    if (degraded) {
      writeLines("SKIPPED", file.path(dir, paste0("DEGRADED-", label, ".txt")))
      return(invisible())
    }
    d <- data.frame(id = seq_along(rows), url_standard = "whatwg",
                    host = rows, stringsAsFactors = FALSE)
    utils::write.csv(d, file.path(dir, paste0("dump-", label, ".csv")),
                     row.names = FALSE)
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
  write_reg <- function(name, scope, signature, expiry, state) {
    path <- file.path(root, name)
    row <- sprintf("| DET-EX-1 | o | a | j | %s | %s | %s | RURL-x | %s |",
                   scope, signature, expiry, state)
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

  cat("determinism compare-gate self-test: PASS (9 fixtures)\n")
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
