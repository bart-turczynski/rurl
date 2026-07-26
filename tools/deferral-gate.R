#!/usr/bin/env Rscript

# G4 deferral gate (rurl 3.0 protocol hardening; verifies the decision P0.5).
# This is the executable form of P0.5's exit rule for gate G4 criterion 3:
# "verified where shipped; specified-but-unshipped cells must be explicitly
# deferred to a named carrier", where a deferred cell remains UNSATISFIED.
#
# This VERIFIES; it decides nothing. Every rule traces to P0.5. Nothing here
# grants coverage, closes a cell, or judges whether a surface should be built.
#
# WHY A REGISTER AND NOT THE TRACKER. P0.1 section 5 makes tracker state
# non-authoritative ("Nothing stated solely in the tracker is normative"), so
# this gate never reads an `fp` issue. The register row's `state` is the
# authority; the `carrier` field is a navigation pointer only. That is also why
# failure condition D3 probes the REPOSITORY (NAMESPACE / R/) rather than asking
# whether a ticket was closed: "the surface shipped" is a fact about the tree.
#
# WHY NO DATE EXPIRY. A deferral ends when its surface arrives, not on a
# calendar date. D3 is the expiry and it is exact. A date would either fire
# early (false red on work legitimately still unbuilt) or late (silence during
# precisely the window that matters).
#
# THE FOUR CHECKS
#   D0  schema     -- every row fully populated; `state` in
#                     {ACCEPTED, DISCHARGED}.
#   D1  carrier    -- every row names a carrier. (P0.5 condition 1.)
#   D2  discharge  -- a DISCHARGED row is claimed by some verification slice
#                     via `DISCHARGED[VD-nnn]`. Closing a carrier is not
#                     evidence about the cell. (P0.5 condition 2.)
#   D3  graduation -- no ACCEPTED row's `surface_probe` symbol is present in
#                     the tree. If the surface shipped, the cells must be
#                     verified, not still deferred. (P0.5 condition 3.)
#   D4  citations  -- every `DEFERRED[VD-nnn]` cited by a verification slice
#                     resolves to an ACCEPTED row. Stops a slice excusing a
#                     cell against a row that is missing, malformed or
#                     discharged.
#
# Zero dependencies beyond base R. Deterministic and network-free.
#
# Usage:
#   Rscript tools/deferral-gate.R             # verify, exit 1 on violation
#   Rscript tools/deferral-gate.R --self-test # positive/negative fixtures

REGISTER_REL <- file.path("design", "work", "url-v3", "registers",
                          "verification-deferrals.md")
SLICE_GLOB_REL <- file.path("design", "work", "url-v3", "verification")
VALID_STATES <- c("ACCEPTED", "DISCHARGED")
ROW_FIELDS <- c("deferral_id", "family", "artifact", "cells", "surface_probe",
                "carrier", "justification", "state")

# ---- register reading -------------------------------------------------------

# Read the pipe table under the "## Deferrals" heading. Returns a list of named
# character vectors, one per row. Deliberately not a general Markdown parser:
# it wants exactly this register's shape and fails loudly on anything else.
read_deferrals <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("deferral register not found: %s", path), call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  start <- grep("^##[[:space:]]+Deferrals[[:space:]]*$", lines)
  if (length(start) != 1L) {
    stop("register must contain exactly one '## Deferrals' heading",
         call. = FALSE)
  }
  body <- lines[seq.int(start + 1L, length(lines))]
  # Stop at the next heading, if any.
  nxt <- grep("^##[[:space:]]", body)
  if (length(nxt)) body <- body[seq_len(nxt[1] - 1L)]

  is_row <- grepl("^[[:space:]]*\\|", body)
  rows <- trimws(body[is_row])
  if (!length(rows)) return(list())

  cells_of <- function(r) {
    r <- sub("^\\|", "", r)
    r <- sub("\\|$", "", r)
    trimws(strsplit(r, "|", fixed = TRUE)[[1]])
  }
  header <- cells_of(rows[1])
  # Drop the |---|---| separator and the header itself.
  rest <- rows[-1]
  rest <- rest[!grepl("^\\|[[:space:]:|-]*\\|?$", rest)]
  out <- list()
  for (r in rest) {
    v <- cells_of(r)
    length(v) <- length(header)
    names(v) <- header
    out[[length(out) + 1L]] <- v
  }
  out
}

# ---- surface probes ---------------------------------------------------------

# `export:<name>` -- present iff exported in NAMESPACE.
# `symbol:<name>`  -- present iff the identifier occurs anywhere under R/.
# Any other prefix is malformed and is reported as a schema violation, never
# silently treated as absent (a typo would then look like a clean deferral).
probe_present <- function(probe, root) {
  if (grepl("^export:", probe)) {
    nm <- sub("^export:", "", probe)
    ns <- file.path(root, "NAMESPACE")
    if (!file.exists(ns)) return(NA)
    txt <- readLines(ns, warn = FALSE)
    return(any(grepl(sprintf("^export\\(%s\\)[[:space:]]*$",
                             gsub("([.\\\\])", "\\\\\\1", nm)), txt)))
  }
  if (grepl("^symbol:", probe)) {
    nm <- sub("^symbol:", "", probe)
    rdir <- file.path(root, "R")
    if (!dir.exists(rdir)) return(NA)
    files <- list.files(rdir, pattern = "\\.[Rr]$", full.names = TRUE)
    pat <- sprintf("\\b%s\\b", gsub("([.\\\\])", "\\\\\\1", nm))
    for (f in files) {
      if (any(grepl(pat, readLines(f, warn = FALSE)))) return(TRUE)
    }
    return(FALSE)
  }
  NA
}

split_probes <- function(s) {
  p <- trimws(strsplit(s, ";", fixed = TRUE)[[1]])
  p[nzchar(p)]
}

# ---- slice citations --------------------------------------------------------

# Collect every DEFERRED[VD-nnn] / DISCHARGED[VD-nnn] marker in the verification
# slices. These are the slices' positive claims about what they excuse and what
# they now cover.
slice_citations <- function(root) {
  dir <- file.path(root, SLICE_GLOB_REL)
  out <- list(deferred = character(0), discharged = character(0))
  if (!dir.exists(dir)) return(out)
  files <- list.files(dir, pattern = "\\.md$", full.names = TRUE)
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    grab <- function(kw) {
      m <- gregexpr(sprintf("%s\\[(VD-[0-9]+)\\]", kw), txt)[[1]]
      if (identical(as.integer(m)[1], -1L)) return(character(0))
      hits <- regmatches(txt, gregexpr(sprintf("%s\\[(VD-[0-9]+)\\]", kw),
                                       txt))[[1]]
      unique(sub(sprintf("^%s\\[", kw), "", sub("\\]$", "", hits)))
    }
    out$deferred <- c(out$deferred, grab("DEFERRED"))
    out$discharged <- c(out$discharged, grab("DISCHARGED"))
  }
  out$deferred <- unique(out$deferred)
  out$discharged <- unique(out$discharged)
  out
}

# ---- the checks -------------------------------------------------------------

finding <- function(id, ok, detail) {
  list(list(id = id, ok = ok, detail = detail))
}

check_deferrals <- function(root = ".") {
  findings <- list()
  reg <- file.path(root, REGISTER_REL)
  rows <- tryCatch(read_deferrals(reg), error = function(e) e)
  if (inherits(rows, "error")) {
    return(finding("D0", FALSE, conditionMessage(rows)))
  }

  # ---- D0 schema ------------------------------------------------------------
  bad <- character(0)
  for (r in rows) {
    id <- if ("deferral_id" %in% names(r)) r[["deferral_id"]] else "<no id>"
    missing <- ROW_FIELDS[!ROW_FIELDS %in% names(r)]
    if (length(missing)) {
      bad <- c(bad, sprintf("%s: missing column(s) %s", id, toString(missing)))
      next
    }
    empty <- ROW_FIELDS[vapply(ROW_FIELDS, function(k) {
      v <- r[[k]]
      is.na(v) || !nzchar(trimws(v))
    }, logical(1))]
    if (length(empty)) {
      bad <- c(bad, sprintf("%s: empty field(s) %s", id, toString(empty)))
    }
    if (!is.na(r[["state"]]) && !r[["state"]] %in% VALID_STATES) {
      bad <- c(bad, sprintf("%s: state '%s' not in {%s}", id, r[["state"]],
                            toString(VALID_STATES)))
    }
    did <- r[["deferral_id"]]
    if (!is.na(did) && !grepl("^VD-[0-9]+$", did)) {
      bad <- c(bad, sprintf("%s: id fails pattern ^VD-[0-9]+$", id))
    }
    for (p in split_probes(r[["surface_probe"]])) {
      if (is.na(probe_present(p, root))) {
        bad <- c(bad, sprintf(
          "%s: malformed surface_probe '%s' (want export:<n>/symbol:<n>)",
          id, p))
      }
    }
  }
  ids <- vapply(rows, function(r) as.character(r[["deferral_id"]] %||% ""),
                character(1))
  dup <- unique(ids[duplicated(ids)])
  if (length(dup)) {
    bad <- c(bad, sprintf("duplicate deferral_id: %s", toString(dup)))
  }
  findings <- c(findings, finding(
    "D0", length(bad) == 0L,
    if (length(bad)) paste(bad, collapse = "; ")
    else sprintf("%d row(s), all populated with a known state", length(rows))))

  # ---- D1 carrier (P0.5 failure condition 1) --------------------------------
  nocarrier <- character(0)
  for (r in rows) {
    if (!"carrier" %in% names(r)) next
    v <- r[["carrier"]]
    if (is.na(v) || !nzchar(trimws(v)) || identical(trimws(v), "-")) {
      nocarrier <- c(nocarrier, as.character(r[["deferral_id"]]))
    }
  }
  findings <- c(findings, finding(
    "D1", length(nocarrier) == 0L,
    if (length(nocarrier)) sprintf("deferral(s) naming no carrier: %s",
                                   toString(nocarrier))
    else "every deferral names a carrier"))

  # ---- D2 discharge (P0.5 failure condition 2) ------------------------------
  cit <- slice_citations(root)
  unclaimed <- character(0)
  for (r in rows) {
    if (!identical(r[["state"]], "DISCHARGED")) next
    if (!r[["deferral_id"]] %in% cit$discharged) {
      unclaimed <- c(unclaimed, as.character(r[["deferral_id"]]))
    }
  }
  findings <- c(findings, finding(
    "D2", length(unclaimed) == 0L,
    if (length(unclaimed))
      sprintf("DISCHARGED row(s) unclaimed by any slice: %s",
              toString(unclaimed))
    else "every discharged deferral is claimed by a verification slice"))

  # ---- D3 graduation (P0.5 failure condition 3) -----------------------------
  shipped <- character(0)
  for (r in rows) {
    if (!identical(r[["state"]], "ACCEPTED")) next
    for (p in split_probes(r[["surface_probe"]])) {
      pres <- probe_present(p, root)
      if (isTRUE(pres)) {
        shipped <- c(shipped, sprintf("%s (%s)", r[["deferral_id"]], p))
      }
    }
  }
  findings <- c(findings, finding(
    "D3", length(shipped) == 0L,
    if (length(shipped))
      sprintf("surface SHIPPED while still deferred -- verify or discharge: %s",
              toString(shipped))
    else "no deferred surface has shipped"))

  # ---- D4 citations ---------------------------------------------------------
  accepted_ids <- vapply(rows, function(r) {
    if (identical(r[["state"]], "ACCEPTED")) {
      as.character(r[["deferral_id"]])
    } else {
      NA_character_
    }
  }, character(1))
  accepted_ids <- accepted_ids[!is.na(accepted_ids)]
  dangling <- setdiff(cit$deferred, accepted_ids)
  findings <- c(findings, finding(
    "D4", length(dangling) == 0L,
    if (length(dangling))
      sprintf("DEFERRED[..] citation(s) with no ACCEPTED row: %s",
              toString(dangling))
    else sprintf("all %d slice deferral citation(s) resolve to ACCEPTED rows",
                 length(cit$deferred))))

  findings
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- reporting --------------------------------------------------------------

print_findings <- function(findings) {
  ok <- TRUE
  for (f in findings) {
    cat(sprintf("%-4s %-5s %s\n", f$id, if (f$ok) "PASS" else "FAIL", f$detail))
    if (!f$ok) ok <- FALSE
  }
  cat(sprintf("VERDICT %s\n", if (ok) "PASS" else "FAIL"))
  ok
}

# ---- self-test (positive + negative fixtures) -------------------------------

self_test <- function() {
  # State lives in an environment rather than behind `<<-`: the package lint
  # profile forbids the operator, and an explicit env is what it recommends.
  st <- new.env(parent = emptyenv())
  st$pass <- 0L
  st$fail <- character(0)
  expect <- function(label, cond) {
    if (isTRUE(cond)) {
      st$pass <- st$pass + 1L
    } else {
      st$fail <- c(st$fail, label)
    }
  }

  # Build a throwaway tree: NAMESPACE + R/ + register + verification slices.
  mk <- function(rows_md, namespace = "export(get_host)\n", rfile = "x <- 1\n",
                 slices = character(0)) {
    root <- tempfile("deferral-fixture-")
    dir.create(file.path(root, "R"), recursive = TRUE)
    dir.create(file.path(root, SLICE_GLOB_REL), recursive = TRUE)
    dir.create(dirname(file.path(root, REGISTER_REL)), recursive = TRUE)
    writeLines(namespace, file.path(root, "NAMESPACE"))
    writeLines(rfile, file.path(root, "R", "a.R"))
    hdr <- paste("| deferral_id | family | artifact | cells |",
                 "surface_probe | carrier | justification | state |")
    writeLines(c("# fixture", "", "## Deferrals", "",
                 hdr, "|---|---|---|---|---|---|---|---|", rows_md),
               file.path(root, REGISTER_REL))
    if (length(slices)) {
      writeLines(slices, file.path(root, SLICE_GLOB_REL, "s-slice.md"))
    }
    root
  }
  verdict <- function(root) {
    fs <- check_deferrals(root)
    all(vapply(fs, function(f) isTRUE(f$ok), logical(1)))
  }
  rule <- function(root, id) {
    fs <- check_deferrals(root)
    for (f in fs) if (identical(f$id, id)) return(isTRUE(f$ok))
    NA
  }

  good <- paste("| VD-001 | join | contracts/key-join-contracts.md |",
                "key surface (7 cells) | export:get_url_key | RURL-abc |",
                "unbuilt surface | ACCEPTED |")

  # 1. Clean positive: surface genuinely absent, carrier named.
  r <- mk(good)
  expect("positive: clean register passes", verdict(r))

  # 2. D1 -- carrier empty.
  r <- mk(sub("RURL-abc", "", good, fixed = TRUE))
  expect("D1 fails on empty carrier", identical(rule(r, "D1"), FALSE))

  # 3. D1 -- carrier is a bare dash.
  r <- mk(sub("RURL-abc", "-", good, fixed = TRUE))
  expect("D1 fails on '-' carrier", identical(rule(r, "D1"), FALSE))

  # 4. D3 -- the surface shipped (export appears in NAMESPACE).
  r <- mk(good, namespace = "export(get_host)\nexport(get_url_key)\n")
  expect("D3 fails when the deferred export ships",
         identical(rule(r, "D3"), FALSE))

  # 5. D3 -- symbol probe: identifier appears under R/.
  sym <- sub("export:get_url_key", "symbol:authority_payload_kind", good,
             fixed = TRUE)
  r <- mk(sym, rfile = "authority_payload_kind <- 'present'\n")
  expect("D3 fails when the deferred symbol ships",
         identical(rule(r, "D3"), FALSE))

  # 6. D3 -- symbol probe absent stays green.
  r <- mk(sym, rfile = "something_else <- 1\n")
  expect("D3 passes when the symbol is absent", identical(rule(r, "D3"), TRUE))

  # 7. D3 must not fire on a DISCHARGED row (that row's cells are covered).
  d <- sub("| ACCEPTED |", "| DISCHARGED |", good, fixed = TRUE)
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] now covered."))
  expect("D3 ignores discharged rows", identical(rule(r, "D3"), TRUE))

  # 8. D2 -- discharged but no slice claims it.
  r <- mk(d, namespace = "export(get_url_key)\n")
  expect("D2 fails on an unclaimed discharge", identical(rule(r, "D2"), FALSE))

  # 9. D2 -- discharged and claimed.
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] covered by tests."))
  expect("D2 passes when a slice claims the discharge",
         identical(rule(r, "D2"), TRUE))

  # 10. D4 -- slice cites a deferral that does not exist.
  r <- mk(good, slices = c("# s", "cell X is DEFERRED[VD-999]."))
  expect("D4 fails on a dangling deferral citation",
         identical(rule(r, "D4"), FALSE))

  # 11. D4 -- slice cites a DISCHARGED row as still deferred.
  r <- mk(d, slices = c("# s", "DISCHARGED[VD-001]", "also DEFERRED[VD-001]"))
  expect("D4 fails when a slice defers against a discharged row",
         identical(rule(r, "D4"), FALSE))

  # 12. D4 -- valid citation passes.
  r <- mk(good, slices = c("# s", "cell X is DEFERRED[VD-001]."))
  expect("D4 passes on a resolving citation", identical(rule(r, "D4"), TRUE))

  # 13. D0 -- unknown state.
  r <- mk(sub("| ACCEPTED |", "| MAYBE |", good, fixed = TRUE))
  expect("D0 fails on an unknown state", identical(rule(r, "D0"), FALSE))

  # 14. D0 -- empty required field (cells).
  r <- mk(sub("key surface (7 cells)", "", good, fixed = TRUE))
  expect("D0 fails on an empty required field", identical(rule(r, "D0"), FALSE))

  # 15. D0 -- malformed probe prefix is a schema error, not a silent absence.
  r <- mk(sub("export:get_url_key", "exports:get_url_key", good, fixed = TRUE))
  expect("D0 fails on a malformed surface_probe",
         identical(rule(r, "D0"), FALSE))

  # 16. D0 -- duplicate ids.
  r <- mk(c(good, good))
  expect("D0 fails on duplicate deferral_id", identical(rule(r, "D0"), FALSE))

  # 17. D0 -- bad id pattern.
  r <- mk(sub("VD-001", "VD1", good, fixed = TRUE))
  expect("D0 fails on a bad id pattern", identical(rule(r, "D0"), FALSE))

  # 18. Empty register (no rows) is legitimate: nothing deferred.
  r <- mk(character(0))
  expect("empty register passes", verdict(r))

  # 19. A multi-probe row fails if ANY probe has shipped.
  multi <- sub("export:get_url_key",
               "export:get_url_key;export:url_semi_join", good, fixed = TRUE)
  r <- mk(multi, namespace = "export(url_semi_join)\n")
  expect("D3 fails when any one of several probes ships",
         identical(rule(r, "D3"), FALSE))

  # 20. Missing register is a hard failure, not an empty pass.
  root <- tempfile("deferral-missing-")
  dir.create(root)
  expect("missing register fails closed", identical(verdict(root), FALSE))

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("deferral-gate self-test: FAIL", call. = FALSE)
  }
  cat("VERDICT PASS\n")
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }
  ok <- print_findings(check_deferrals("."))
  if (!ok) {
    stop("deferral gate: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
