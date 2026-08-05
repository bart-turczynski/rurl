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
#   D2  discharge  -- a DISCHARGED row is claimed via `DISCHARGED[VD-nnn]` by a
#                     REGISTERED claimant: a verification slice, or the
#                     discharge record registered for that very deferral.
#                     Closing a carrier is not evidence about the cell.
#                     (P0.5 condition 2.)
#   D3  graduation -- no ACCEPTED row's `surface_probe` symbol is present in
#                     the tree. If the surface shipped, the cells must be
#                     verified, not still deferred. (P0.5 condition 3.)
#   D4  citations  -- every `DEFERRED[VD-nnn]` cited by a verification slice
#                     resolves to an ACCEPTED row. Stops a slice excusing a
#                     cell against a row that is missing, malformed or
#                     discharged.
#
# WHAT COUNTS AS A CLAIMANT, AND WHY THIS GATE READS ANOTHER RECORD (P0.7 D-E,
# RURL-ogktvhgp). D2 used to glob every `design/work/url-v3/verification/*.md`
# and treat each one as a verification slice, so a discharge was satisfiable by
# ANY file in that directory whatever its name or content -- a discharge could
# be "claimed" by a file that claims nothing. Meanwhile
# `design/work/url-v3/tools/traceability-gate.R` rule T5 checked a fixed
# ten-name registry that three of the four claimant records on disk were not in.
# Both gates passed while contradicting each other about the same file, and the
# traceability map's census consequently reported as unowned a body of claims
# for which shipped evidence existed and was already claimed cell by cell.
#
# The two definitions are reconciled toward the NARROWER one, and the registry
# lives in ONE place: the traceability map's `## Verification slices` and
# `## Discharge records` tables. This gate reads them; that gate's T5/T8 hold
# them to disk in both directions. A copied registry here would be a second
# list to forget, and the drift would be silent in exactly the way this pair of
# rules exists to prevent -- so if the map is missing, or has no discharge
# registry, D2 fails CLOSED rather than falling back to the old glob.
#
# Note the asymmetry with D4, which is deliberate. D2 asks "who may GRANT a
# discharge" and must be narrow. D4 asks "does any file in the tree cite a
# deferral that does not resolve", and stays wide on purpose: a dangling
# citation is worth catching wherever it is written.
#
# Zero dependencies beyond base R. Deterministic and network-free.
#
# Usage:
#   Rscript tools/deferral-gate.R             # verify, exit 1 on violation
#   Rscript tools/deferral-gate.R --self-test # positive/negative fixtures

REGISTER_REL <- file.path("design", "work", "url-v3", "registers",
                          "verification-deferrals.md")
# Where verification records live. It is NOT the definition of a claimant --
# treating "in this directory" as "is a verification slice" is precisely the
# defect P0.7 D-E fixed -- it is only where citations are read from.
VERIFICATION_DIR <- file.path("design", "work", "url-v3", "verification")
MAP_REL <- file.path(VERIFICATION_DIR, "traceability-map.md")
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

# ---- the claimant registry --------------------------------------------------

# Data rows of the pipe table under a `## <heading>` in the traceability map,
# stopping at the next `##`. Same shape as read_deferrals() above and equally
# unforgiving: it wants a pipe table there and reports its absence rather than
# inventing an empty one.
map_table <- function(lines, heading) {
  start <- grep(sprintf("^##[[:space:]]+%s[[:space:]]*$", heading), lines)
  if (length(start) != 1L) return(NULL)
  rest <- lines[seq.int(start[1] + 1L, length(lines))]
  nxt <- grep("^##[[:space:]]", rest)
  if (length(nxt)) rest <- rest[seq_len(nxt[1] - 1L)]
  rest <- trimws(rest)
  rest <- rest[grepl("^\\|", rest) & !grepl("^\\|[-:| ]*$", rest)]
  if (length(rest) < 2L) return(list())
  lapply(rest[-1], function(r) {
    cells <- sub("\\|$", "", sub("^\\|", "", r))
    trimws(strsplit(cells, "|", fixed = TRUE)[[1]])
  })
}

# Who may claim a discharge, read off the traceability map. Returns a list with
# `slices` (basenames of registered slice files) and `discharges` (a named
# character vector: record basename -> the ONE deferral it is registered for),
# or an `error` string when the map cannot supply the registry -- in which case
# D2 fails rather than guessing.
claimant_registry <- function(root) {
  path <- file.path(root, MAP_REL)
  if (!file.exists(path)) {
    return(list(error = sprintf("claimant registry unavailable: %s not found",
                                MAP_REL)))
  }
  lines <- readLines(path, warn = FALSE)
  sl <- map_table(lines, "Verification slices")
  dr <- map_table(lines, "Discharge records")
  if (is.null(sl)) {
    return(list(error = paste(MAP_REL,
                              "has no '## Verification slices' table")))
  }
  if (is.null(dr)) {
    return(list(error = paste(MAP_REL, "has no '## Discharge records' table")))
  }
  slices <- vapply(sl, function(r) if (length(r)) r[1] else NA_character_,
                   character(1))
  ids <- vapply(dr, function(r) if (length(r) >= 2L) r[2] else NA_character_,
                character(1))
  names(ids) <- vapply(dr, function(r) if (length(r)) r[1] else NA_character_,
                       character(1))
  list(slices = paste0(slices[!is.na(slices)], ".md"),
       discharges = ids[!is.na(ids) & !is.na(names(ids))])
}

# ---- slice citations --------------------------------------------------------

# Collect every DEFERRED[VD-nnn] / DISCHARGED[VD-nnn] marker written under the
# verification directory, keeping the FILE each one came from: D2 credits a
# discharge only to a registered claimant, so "which file said it" is part of
# the fact, not an implementation detail.
slice_citations <- function(root) {
  dir <- file.path(root, VERIFICATION_DIR)
  out <- list(deferred = character(0), discharged = character(0),
              claims = list())
  if (!dir.exists(dir)) return(out)
  files <- list.files(dir, pattern = "\\.md$", full.names = TRUE)
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    grab <- function(kw) {
      hits <- regmatches(txt, gregexpr(sprintf("%s\\[(VD-[0-9]+)\\]", kw),
                                       txt))[[1]]
      if (!length(hits)) return(character(0))
      unique(sub(sprintf("^%s\\[", kw), "", sub("\\]$", "", hits)))
    }
    out$deferred <- c(out$deferred, grab("DEFERRED"))
    discharged <- grab("DISCHARGED")
    out$discharged <- c(out$discharged, discharged)
    if (length(discharged)) out$claims[[basename(f)]] <- discharged
  }
  out$deferred <- unique(out$deferred)
  out$discharged <- unique(out$discharged)
  out
}

# The files whose `DISCHARGED[id]` claim D2 may credit: a registered slice, or
# the discharge record registered for THAT deferral. A discharge record listed
# against VD-002 cannot vouch for VD-004 -- it is narrow by construction, and
# the registry says which one it is narrow to.
eligible_claimants <- function(id, claims, reg) {
  claimants <- names(claims)[vapply(claims, function(v) id %in% v, logical(1))]
  keep <- vapply(claimants, function(f) {
    f %in% reg$slices ||
      identical(unname(reg$discharges[sub("\\.md$", "", f)]), id)
  }, logical(1))
  list(all = claimants, eligible = claimants[keep])
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
  reg <- claimant_registry(root)
  unclaimed <- character(0)
  unregistered <- character(0)
  discharged_rows <- 0L
  for (r in rows) {
    if (!identical(r[["state"]], "DISCHARGED")) next
    discharged_rows <- discharged_rows + 1L
    if (!is.null(reg$error)) next
    id <- as.character(r[["deferral_id"]])
    hit <- eligible_claimants(id, cit$claims, reg)
    if (length(hit$eligible)) next
    if (length(hit$all)) {
      # The narrowing's whole point: a claim exists, but from a file no
      # registry admits. Name the file -- the fix is to register it (or to
      # write the evidence in something that is registered), not to hunt for a
      # missing citation that is right there.
      unregistered <- c(unregistered, sprintf("%s (claimed only by %s)", id,
                                              toString(hit$all)))
    } else {
      unclaimed <- c(unclaimed, id)
    }
  }
  findings <- c(findings, finding(
    "D2",
    is.null(reg$error) && !length(unclaimed) && !length(unregistered),
    if (!is.null(reg$error)) {
      reg$error
    } else if (length(unclaimed)) {
      sprintf("DISCHARGED row(s) claimed by nothing: %s", toString(unclaimed))
    } else if (length(unregistered)) {
      sprintf("DISCHARGED row(s) claimed by an unregistered file: %s",
              toString(unregistered))
    } else {
      sprintf(paste("all %d discharged deferral(s) claimed by a registered",
                    "slice or their own discharge record"), discharged_rows)
    }))

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

  # Build a throwaway tree: NAMESPACE + R/ + register + verification records +
  # the traceability map that says which of those records may claim a
  # discharge. `slice_file` is the file the citation is WRITTEN in and
  # `registry_*` is what the map admits; a fixture makes them disagree to
  # exercise the narrowing.
  mk <- function(rows_md, namespace = "export(get_host)\n", rfile = "x <- 1\n",
                 slices = character(0), slice_file = "cache-slice.md",
                 registry_slices = "cache-slice",
                 registry_discharges = character(0), map = TRUE) {
    root <- tempfile("deferral-fixture-")
    dir.create(file.path(root, "R"), recursive = TRUE)
    dir.create(file.path(root, VERIFICATION_DIR), recursive = TRUE)
    dir.create(dirname(file.path(root, REGISTER_REL)), recursive = TRUE)
    writeLines(namespace, file.path(root, "NAMESPACE"))
    writeLines(rfile, file.path(root, "R", "a.R"))
    hdr <- paste("| deferral_id | family | artifact | cells |",
                 "surface_probe | carrier | justification | state |")
    writeLines(c("# fixture", "", "## Deferrals", "",
                 hdr, "|---|---|---|---|---|---|---|---|", rows_md),
               file.path(root, REGISTER_REL))
    if (length(slices)) {
      writeLines(slices, file.path(root, VERIFICATION_DIR, slice_file))
    }
    if (!identical(map, FALSE)) {
      row <- function(...) paste0("| ", paste(c(...), collapse = " | "), " |")
      tracked <- function(id) file.path(VERIFICATION_DIR, paste0(id, ".md"))
      writeLines(c(
        "# fixture map", "",
        "## Verification slices", "",
        "| slice_id | tracked_path | state |", "|---|---|---|",
        vapply(registry_slices, function(s) row(s, tracked(s), "SHIPPED"),
               character(1), USE.NAMES = FALSE),
        "",
        if (identical(map, "no-discharge-table")) character(0) else c(
          "## Discharge records", "",
          "| record_id | deferral_id | tracked_path | contract | scope |",
          "|---|---|---|---|---|",
          vapply(seq_along(registry_discharges), function(i) {
            id <- names(registry_discharges)[i]
            row(id, registry_discharges[[i]], tracked(id), "CS", "one row")
          }, character(1), USE.NAMES = FALSE)
        )
      ), file.path(root, MAP_REL))
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

  # 9. D2 -- discharged and claimed by a REGISTERED slice.
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] covered by tests."))
  expect("D2 passes when a registered slice claims the discharge",
         identical(rule(r, "D2"), TRUE))

  # 9a. The narrowing itself (P0.7 D-E). The same citation, in a file the old
  # glob accepted because it merely sat in the directory. Green before, red now.
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] covered by tests."),
          slice_file = "loose-notes.md")
  expect("D2 fails when the only claimant is an unregistered file",
         identical(rule(r, "D2"), FALSE))

  # 9b. A registered discharge record may claim the deferral it is registered
  # for ...
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] covered by tests."),
          slice_file = "join-discharge.md",
          registry_discharges = c("join-discharge" = "VD-001"))
  expect("D2 passes when the registered discharge record claims its deferral",
         identical(rule(r, "D2"), TRUE))

  # 9c. ... and only that one. A discharge record is narrow by construction, so
  # being registered for VD-002 does not let it vouch for VD-001.
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] covered by tests."),
          slice_file = "join-discharge.md",
          registry_discharges = c("join-discharge" = "VD-002"))
  expect(paste("D2 fails when a discharge record claims a deferral it is not",
               "registered for"),
         identical(rule(r, "D2"), FALSE))

  # 9d/9e. No registry, no verdict: D2 fails closed rather than falling back to
  # the glob it replaced. This is what makes the two halves land together --
  # the tightened rule is red until the map carries the discharge registry.
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] covered by tests."),
          map = FALSE)
  expect("D2 fails closed when the traceability map is missing",
         identical(rule(r, "D2"), FALSE))
  r <- mk(d, namespace = "export(get_url_key)\n",
          slices = c("# s", "DISCHARGED[VD-001] covered by tests."),
          map = "no-discharge-table")
  expect("D2 fails closed when the map has no discharge registry",
         identical(rule(r, "D2"), FALSE))

  # 10. D4 -- slice cites a deferral that does not exist.
  r <- mk(good, slices = c("# s", "cell X is DEFERRED[VD-999]."))
  expect("D4 fails on a dangling deferral citation",
         identical(rule(r, "D4"), FALSE))

  # 10a. D4 stays WIDE where D2 narrowed. A dangling citation is worth catching
  # wherever it is written, including in a file no registry admits -- narrowing
  # both rules together would have opened a hole while closing one.
  r <- mk(good, slices = c("# s", "cell X is DEFERRED[VD-999]."),
          slice_file = "loose-notes.md")
  expect("D4 still sees a dangling citation in an unregistered file",
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
