#!/usr/bin/env Rscript
# validate-records.R — G0.3a common-envelope + record-schema validator for the
# §6 authority records. Consumes schema/{lifecycle,envelope,record-schemas}.yaml
# and manifest.yaml; validates every present record and reports the five §6
# rejection classes (envelope.yaml:rejection_classes):
#   duplicate_ids, broken_references, unknown_states, missing_required_fields,
#   manifest_hash_drift.
#
# Import rule (P0.1 §Consequences): P0.1-P0.3 must validate here WITHOUT any
# substance edit. Run from the repository root:
#   Rscript design/work/url-v3/tools/validate-records.R

suppressWarnings(suppressMessages({
  ok <- requireNamespace("yaml", quietly = TRUE) &&
    requireNamespace("digest", quietly = TRUE)
}))
if (!ok) stop("validate-records.R needs the 'yaml' and 'digest' packages")
`%||%` <- function(a, b) if (is.null(a)) b else a

root <- "design/work/url-v3"
sdir <- file.path(root, "schema")
lifecycle <- yaml::read_yaml(file.path(sdir, "lifecycle.yaml"))
rschemas  <- yaml::read_yaml(file.path(sdir, "record-schemas.yaml"))
manifest  <- yaml::read_yaml(file.path(root, "manifest.yaml"))

fail <- character(0); pass <- 0L
check <- function(cond, msg) if (isTRUE(cond)) pass <<- pass + 1L else fail <<- c(fail, msg)
valid_states <- names(lifecycle$states)
sha256_of <- function(p) digest::digest(file = p, algo = "sha256")

read_frontmatter <- function(path) {
  ln <- readLines(path, warn = FALSE)
  idx <- which(ln == "---")
  if (length(idx) < 2) return(NULL)
  yaml::yaml.load(paste(ln[(idx[1] + 1):(idx[2] - 1)], collapse = "\n"))
}

## --- owner-decision records ------------------------------------------------
dec <- rschemas$record_types$`owner-decision`
dec_files <- Sys.glob(dec$path_glob)
records <- list()
for (f in dec_files) {
  fm <- read_frontmatter(f)
  check(!is.null(fm), sprintf("owner-decision: no frontmatter: %s", f))
  if (is.null(fm)) next
  # missing_required_fields
  for (k in dec$required_fields) {
    check(!is.null(fm[[k]]),
          sprintf("[missing_required_fields] %s: missing '%s'", basename(f), k))
  }
  id <- as.character(fm$id %||% "")
  check(grepl(dec$id_pattern, id),
        sprintf("%s: id '%s' fails pattern %s", basename(f), id, dec$id_pattern))
  st <- as.character(fm$state %||% "")
  # unknown_states
  check(st %in% valid_states, sprintf("[unknown_states] %s: state '%s'", basename(f), st))
  check(st %in% dec$allowed_states,
        sprintf("%s: state '%s' not allowed for owner-decision", basename(f), st))
  records[[id]] <- list(state = st, path = f, fm = fm)
}

## --- duplicate_ids ---------------------------------------------------------
ids <- vapply(dec_files, function(f) as.character(read_frontmatter(f)$id %||% NA_character_), "")
ids <- ids[!is.na(ids)]
dups <- unique(ids[duplicated(ids)])
check(length(dups) == 0, sprintf("[duplicate_ids] %s", paste(dups, collapse = ", ")))

## --- broken_references (depends_on/supersedes/superseded_by) ---------------
known <- names(records)
for (id in known) {
  fm <- records[[id]]$fm
  for (rf in dec$reference_fields) {
    refs <- as.character(unlist(fm[[rf]] %||% character(0)))
    for (r in refs) {
      check(r %in% known,
            sprintf("[broken_references] %s: %s -> unknown id '%s'", id, rf, r))
    }
  }
}

## --- single-writer = owner for ACCEPTED; append-only substance -------------
for (id in known) {
  r <- records[[id]]
  if (identical(r$state, "ACCEPTED")) {
    check(!is.null(r$fm$owner) && !is.null(r$fm$approver),
          sprintf("%s: ACCEPTED record missing owner/approver", id))
    check(grepl("owner", tolower(r$fm$authority %||% "")),
          sprintf("%s: ACCEPTED record authority is not owner-held", id))
  }
}

## --- manifest_hash_drift (incl. append-only proof for ACCEPTED decisions) --
drift <- c(manifest$sources, manifest$artifacts, manifest$decisions,
           list(manifest$tracker_snapshot))
for (reg in manifest$registers) if (isTRUE(reg$present)) drift <- c(drift, list(reg))
for (e in drift) {
  p <- e$path %||% NULL
  if (is.null(p) || is.null(e$sha256)) next
  if (!file.exists(p)) { check(FALSE, sprintf("manifest references missing file: %s", p)); next }
  check(identical(sha256_of(p), e$sha256),
        sprintf("[manifest_hash_drift] %s (append-only violation if ACCEPTED)", p))
}

## --- register presence: snapshots.log columnar shape -----------------------
snap <- file.path(root, "registers", "snapshots.log")
if (file.exists(snap)) {
  rows <- readLines(snap, warn = FALSE)
  rows <- rows[nzchar(trimws(rows)) & !grepl("^#", rows)]
  check(length(rows) >= 1, "snapshots.log: no binding rows")
  for (rw in rows) {
    cols <- strsplit(trimws(rw), "[[:space:]]+")[[1]]
    check(length(cols) == 4,
          sprintf("snapshots.log: row is not 4 columns: '%s'", rw))
  }
}

## --- G1.1 registers: finding (RCON) + source-claim (S) -----------------------
## Parses the two Markdown row-registers and enforces the reconciliation §9
## bidirectional coverage: forward (§4 Sources, transferred into findings.md)
## must equal the inverse (source-claims target_ref + parsed §9). Every frozen
## finding is represented exactly once, multi-RCON links allowed. (RURL-jalwcgzk)
reg_dir   <- file.path(root, "registers")
recon     <- file.path(root, "protocol-review-reconciliation.md")
sc_path   <- file.path(reg_dir, "source-claims.md")
fd_path   <- file.path(reg_dir, "findings.md")
sc_pat    <- rschemas$record_types$register$variants$`source-claim`$id_pattern
fd_pat    <- rschemas$record_types$register$variants$finding$id_pattern
sc_fields <- rschemas$record_types$register$variants$`source-claim`$row_fields
fd_fields <- rschemas$record_types$register$variants$finding$row_fields
env_fields <- yaml::read_yaml(file.path(sdir, "envelope.yaml"))$required_envelope_fields

# Read the pipe-table under the "## Rows" heading of a register file.
gv <- function(r, k) if (!is.null(names(r)) && k %in% names(r)) r[[k]] else NA_character_
read_rows <- function(path) {
  ln <- readLines(path, warn = FALSE)
  h  <- which(grepl("^##\\s+Rows\\s*$", ln))
  if (length(h) != 1) return(NULL)
  nxt <- which(grepl("^##\\s", ln) & seq_along(ln) > h)
  end <- if (length(nxt)) min(nxt) - 1L else length(ln)
  body <- ln[(h + 1):end]
  tbl  <- body[grepl("^\\|", body)]
  if (length(tbl) < 3) return(NULL)
  header <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", tbl[1])), "\\|")[[1]])
  data <- tbl[-(1:2)]                                   # drop header + separator
  data <- data[!grepl("^\\|[-:\\s|]*$", data)]
  rows <- lapply(data, function(r) {
    cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", r)), "\\|")[[1]])
    stats::setNames(cells, header[seq_along(cells)])
  })
  list(header = header, rows = rows)
}

# Expand one "S<n> ..." report-group into canonical claim_ids.
expand_group <- function(g) {
  g <- trimws(g)
  m <- regmatches(g, regexec("^S([0-9]+)[[:space:]]+(.*)$", g))[[1]]
  if (length(m) < 3) stop(sprintf("unparseable report group: '%s'", g))
  rn <- m[2]; rest <- m[3]
  is_finding <- grepl("finding", rest)
  rest <- gsub("findings?", "", rest)
  rest <- gsub("[[:space:]]+and[[:space:]]+", ",", rest)
  rest <- gsub("[[:space:]]+", "", rest)
  items <- strsplit(rest, ",")[[1]]; items <- items[nzchar(items)]
  out <- character(0)
  for (it in items) {
    if (rn == "2") {
      mm <- regmatches(it, regexec("^S2-([0-9]+)(-S2-([0-9]+))?$", it))[[1]]
      if (length(mm) < 2 || !nzchar(mm[2])) stop(sprintf("bad S2 item '%s'", it))
      a <- as.integer(mm[2]); b <- if (nzchar(mm[4])) as.integer(mm[4]) else a
      for (k in a:b) out <- c(out, sprintf("S2 S2-%02d", k))
    } else if (rn == "9") {
      mm <- regmatches(it, regexec("^([CHM])([0-9]+)(-([CHM])([0-9]+))?$", it))[[1]]
      if (length(mm) < 3 || !nzchar(mm[2])) stop(sprintf("bad S9 item '%s'", it))
      L <- mm[2]; a <- as.integer(mm[3]); b <- if (nzchar(mm[6])) as.integer(mm[6]) else a
      for (k in a:b) out <- c(out, sprintf("S9 %s%d", L, k))
    } else if (is_finding) {
      mm <- regmatches(it, regexec("^([0-9]+)(-([0-9]+))?$", it))[[1]]
      if (length(mm) < 2 || !nzchar(mm[2])) stop(sprintf("bad finding item '%s'", it))
      a <- as.integer(mm[2]); b <- if (nzchar(mm[4])) as.integer(mm[4]) else a
      for (k in a:b) out <- c(out, sprintf("S%s finding %d", rn, k))
    } else {
      mm <- regmatches(it, regexec("^F([0-9]+)(-F([0-9]+))?$", it))[[1]]
      if (length(mm) < 2 || !nzchar(mm[2])) stop(sprintf("bad F item '%s'", it))
      a <- as.integer(mm[2]); b <- if (nzchar(mm[4])) as.integer(mm[4]) else a
      for (k in a:b) out <- c(out, sprintf("S%s F%d", rn, k))
    }
  }
  out
}
expand_cell <- function(cell) {
  cell <- sub("\\(.*$", "", trimws(cell))            # drop trailing parenthetical
  parts <- strsplit(cell, ";")[[1]]; parts <- parts[nzchar(trimws(parts))]
  unique(unlist(lapply(parts, expand_group)))
}

sc_reg <- if (file.exists(sc_path)) read_rows(sc_path) else NULL
fd_reg <- if (file.exists(fd_path)) read_rows(fd_path) else NULL
check(!is.null(sc_reg), "source-claims.md: no parseable Rows table")
check(!is.null(fd_reg), "findings.md: no parseable Rows table")

if (!is.null(sc_reg) && !is.null(fd_reg)) {
  # envelope block present in each register
  for (rf in list(list("source-claims.md", sc_path), list("findings.md", fd_path))) {
    txt <- paste(readLines(rf[[2]], warn = FALSE), collapse = "\n")
    for (ef in env_fields) {
      check(grepl(sprintf("\\|\\s*%s\\s*\\|", ef), txt),
            sprintf("[missing_required_fields] %s: envelope field '%s'", rf[[1]], ef))
    }
  }

  # --- source-claim rows: schema, id pattern, states, uniqueness --------------
  sc_rows <- sc_reg$rows
  for (f in sc_fields) check(f %in% sc_reg$header,
    sprintf("[missing_required_fields] source-claims.md: column '%s'", f))
  sc_ids <- vapply(sc_rows, function(r) gv(r, "claim_id"), "")
  for (id in sc_ids) check(grepl(sc_pat, id),
    sprintf("source-claims.md: claim_id '%s' fails %s", id, sc_pat))
  dup_sc <- unique(sc_ids[duplicated(sc_ids)])
  check(length(dup_sc) == 0, sprintf("[duplicate_ids] source-claims: %s", paste(dup_sc, collapse = ", ")))
  for (r in sc_rows) check((gv(r, "state")) %in% valid_states,
    sprintf("[unknown_states] source-claims %s: '%s'", gv(r, "claim_id"), gv(r, "state")))
  # §10 path-correction discipline: S4/S6 rows carry a correction, others "—"
  for (r in sc_rows) {
    rep <- sub("[[:space:]].*$", "", gv(r, "claim_id"))
    pc  <- gv(r, "path_correction")
    if (rep %in% c("S4", "S6")) {
      check(grepl("§10", pc), sprintf("source-claims %s: missing §10 path_correction", gv(r, "claim_id")))
    } else {
      check(identical(pc, "—"), sprintf("source-claims %s: unexpected path_correction '%s'", gv(r, "claim_id"), pc))
    }
  }
  # target_ref per claim (RCON set)
  sc_target <- list()
  for (r in sc_rows) {
    refs <- regmatches(gv(r, "target_ref"), gregexpr("RCON-[0-9]+", gv(r, "target_ref")))[[1]]
    check(length(refs) >= 1, sprintf("source-claims %s: no target_ref RCON", gv(r, "claim_id")))
    sc_target[[gv(r, "claim_id")]] <- sort(unique(refs))
  }

  # --- finding rows: schema, id pattern, disposition --------------------------
  fd_rows <- fd_reg$rows
  for (f in fd_fields) check(f %in% fd_reg$header,
    sprintf("[missing_required_fields] findings.md: column '%s'", f))
  fd_ids <- vapply(fd_rows, function(r) gv(r, "finding_id"), "")
  for (id in fd_ids) check(grepl(fd_pat, id),
    sprintf("findings.md: finding_id '%s' fails %s", id, fd_pat))
  dup_fd <- unique(fd_ids[duplicated(fd_ids)])
  check(length(dup_fd) == 0, sprintf("[duplicate_ids] findings: %s", paste(dup_fd, collapse = ", ")))
  for (r in fd_rows) check((gv(r, "disposition")) %in% valid_states,
    sprintf("[unknown_states] findings %s: '%s'", gv(r, "finding_id"), gv(r, "disposition")))

  # --- parse reconciliation §9 (ground-truth inverse) + §4 (forward) ----------
  rl <- readLines(recon, warn = FALSE)
  # §9 table rows: between the "## 9." heading and the next "## " heading.
  s9 <- which(grepl("^## 9\\.", rl)); s9e <- which(grepl("^## 10\\.", rl))
  block9 <- rl[(s9 + 1):(s9e - 1)]
  trows <- block9[grepl("^\\|", block9) & grepl("RCON-", block9)]
  gt_inv <- list()                                   # claim_id -> sorted RCON set
  for (tr in trows) {
    cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", tr)), "\\|")[[1]])
    claims <- expand_cell(cells[1])
    rcons  <- sort(unique(regmatches(cells[2], gregexpr("RCON-[0-9]+", cells[2]))[[1]]))
    for (c in claims) gt_inv[[c]] <- sort(unique(c(gt_inv[[c]], rcons)))
  }
  # §4 Sources per RCON (forward), transferred verbatim into findings.md; also
  # self-check the reconciliation's own §4 against §9.
  gt_fwd <- list(); cur <- NA_character_
  for (line in rl) {
    hm <- regmatches(line, regexec("^### (RCON-[0-9]+)", line))[[1]]
    if (length(hm) >= 2) cur <- hm[2]
    if (grepl("^\\*\\*Sources:\\*\\*", line) && !is.na(cur)) {
      gt_fwd[[cur]] <- sort(expand_cell(sub(".*\\*\\*Sources:\\*\\*", "", line)))
      cur <- NA_character_
    }
  }

  # invert §9 -> forward, and assert reconciliation §4 == inverse of §9
  inv_of_9 <- list()
  for (c in names(gt_inv)) for (rc in gt_inv[[c]]) inv_of_9[[rc]] <- sort(unique(c(inv_of_9[[rc]], c)))
  for (rc in sort(union(names(gt_fwd), names(inv_of_9)))) {
    check(identical(gt_fwd[[rc]] %||% character(0), inv_of_9[[rc]] %||% character(0)),
          sprintf("reconciliation §4 Sources for %s disagree with §9 inverse", rc))
  }

  # --- completeness: register claim set == §9 claim set (bijection) -----------
  check(setequal(sc_ids, names(gt_inv)),
        sprintf("source-claims rows != §9 findings (rows=%d, §9=%d)", length(sc_ids), length(gt_inv)))
  check(length(sc_ids) == length(gt_inv),
        sprintf("source-claims exact count mismatch: %d vs §9 %d", length(sc_ids), length(gt_inv)))

  # --- inverse check: source-claims target_ref == §9 inverse (per claim) ------
  for (c in sc_ids) {
    check(identical(sc_target[[c]] %||% character(0), gt_inv[[c]] %||% character(0)),
          sprintf("source-claims %s target_ref != §9 inverse", c))
  }

  # --- forward check: findings.md sources expand == §9 forward (per RCON) -----
  fd_src <- list()
  for (r in fd_rows) fd_src[[gv(r, "finding_id")]] <- sort(expand_cell(gv(r, "sources")))
  for (rc in sort(union(names(fd_src), names(inv_of_9)))) {
    check(identical(fd_src[[rc]] %||% character(0), inv_of_9[[rc]] %||% character(0)),
          sprintf("findings.md %s sources expand != §9 forward map", rc))
    # every expanded source resolves to an actual source-claim row (no orphan)
    for (c in fd_src[[rc]] %||% character(0))
      check(c %in% sc_ids, sprintf("findings.md %s: source '%s' has no source-claim row", rc, c))
  }
  check(length(fd_ids) == 10L, sprintf("findings register expected 10 RCON rows, got %d", length(fd_ids)))
}

## --- the public surface, DERIVED from source ---------------------------------
## Every artifact-4 check compares against these rather than against a number
## written into a document (P0.6 I1; RURL-oygqsykd). Memoized: several sections
## use them. A transcribed count is a maintenance tax and a stale-fact risk —
## the "29 exports / 51 rows" prose that survived the 51->52 growth is the proof.
.psd_cache <- new.env(parent = emptyenv())
psd_ns_exports <- function() {
  if (is.null(.psd_cache$exports)) {
    ns <- readLines("NAMESPACE", warn = FALSE)
    e <- regmatches(ns, regexpr("(?<=^export\\()[^)]+", ns, perl = TRUE))
    .psd_cache$exports <- sort(e[nzchar(e)])
  }
  .psd_cache$exports
}
psd_result_fields <- function() {
  if (is.null(.psd_cache$fields)) {
    ut <- readLines(file.path("R", "utils.R"), warn = FALSE)
    i0 <- grep("^\\.spu_result_fields\\s*<-\\s*list\\(", ut)
    f <- character(0)
    if (length(i0) == 1) {
      ends <- grep("^\\)\\s*$", ut)
      ends <- ends[ends > i0]
      if (length(ends)) {
        blk <- ut[i0:ends[[1]]]
        m <- regmatches(blk, regexpr('(?<=name = ")[^"]+', blk, perl = TRUE))
        f <- sort(m[nzchar(m)])
      }
    }
    .psd_cache$fields <- f
  }
  .psd_cache$fields
}

## --- G1.2 register: public-surface-inventory (§6 artifact 4) -----------------
## Parses the artifact-4 inventory register and enforces: PROPOSED envelope,
## every row DISCOVERED with non-empty required columns, the NAMESPACE bijection
## over exported-function/exported-data rows (mirrors G1.1's row-count
## bijection), and the public-output-field rows. Both bijections are by NAME
## against derived sources — no count is pinned here (P0.6 I1). (RURL-dmsgpcak)
psi_path <- file.path(reg_dir, "public-surface-inventory.md")
psi_reg  <- if (file.exists(psi_path)) read_rows(psi_path) else NULL
check(!is.null(psi_reg), "public-surface-inventory.md: no parseable Rows table")
psi_n <- 0L
psi_checks_before <- pass + length(fail)
if (!is.null(psi_reg)) {
  ptxt <- paste(readLines(psi_path, warn = FALSE), collapse = "\n")
  # envelope: every required field present, lifecycle_state == PROPOSED
  for (ef in env_fields) {
    check(grepl(sprintf("\\|\\s*%s\\s*\\|", ef), ptxt),
          sprintf("[missing_required_fields] public-surface-inventory.md: envelope field '%s'", ef))
  }
  check(grepl("\\|\\s*lifecycle_state\\s*\\|\\s*PROPOSED\\s*\\|", ptxt),
        "public-surface-inventory.md: lifecycle_state must be PROPOSED")

  # required row columns present
  psi_cols <- c("item_id", "kind", "arguments", "behavior_dials", "interactions",
                "vector_error", "docs", "migration", "test_family", "state",
                "disposition", "missing_evidence")
  for (col in psi_cols) {
    check(col %in% psi_reg$header,
          sprintf("[missing_required_fields] public-surface-inventory.md: column '%s'", col))
  }

  psi_rows <- psi_reg$rows
  psi_n <- length(psi_rows)
  psi_kinds <- c("exported-function", "exported-data", "public-output-field",
                 "curl-dependency", "migration-surface")
  # every row: state == DISCOVERED, kind in the closed set, non-empty required cells
  for (r in psi_rows) {
    id <- gv(r, "item_id")
    check(identical(gv(r, "state"), "DISCOVERED"),
          sprintf("[unknown_states] public-surface-inventory %s: state '%s' != DISCOVERED", id, gv(r, "state")))
    check(gv(r, "kind") %in% psi_kinds,
          sprintf("public-surface-inventory %s: kind '%s' not in closed set", id, gv(r, "kind")))
    for (col in psi_cols) {
      check(!is.na(gv(r, col)) && nzchar(gv(r, col)),
            sprintf("public-surface-inventory %s: empty required column '%s'", id, col))
    }
  }
  # unique item_ids
  psi_ids <- vapply(psi_rows, function(r) gv(r, "item_id"), "")
  dup_psi <- unique(psi_ids[duplicated(psi_ids)])
  check(length(dup_psi) == 0,
        sprintf("[duplicate_ids] public-surface-inventory: %s", paste(dup_psi, collapse = ", ")))

  # --- NAMESPACE bijection over exported-function/exported-data rows, BY NAME --
  # No count is asserted: setequal + the two setdiff loops below are strictly
  # stronger than a cardinality check, and unlike a literal they never need
  # editing when the surface grows (P0.6 I1).
  ns_exports <- psd_ns_exports()
  is_export_row <- vapply(psi_rows,
                          function(r) gv(r, "kind") %in% c("exported-function", "exported-data"), TRUE)
  fn_ids <- sort(vapply(psi_rows[is_export_row], function(r) gv(r, "item_id"), ""))
  check(setequal(fn_ids, ns_exports),
        sprintf("public-surface-inventory export rows != NAMESPACE (rows=%d, NAMESPACE=%d)",
                length(fn_ids), length(ns_exports)))
  for (e in setdiff(ns_exports, fn_ids))
    check(FALSE, sprintf("public-surface-inventory: missing export row '%s'", e))
  for (e in setdiff(fn_ids, ns_exports))
    check(FALSE, sprintf("public-surface-inventory: extra export row '%s' not in NAMESPACE", e))

  # --- public-output-field rows, exact name set derived from .spu_result_fields -
  pub_fields <- psd_result_fields()
  check(length(pub_fields) >= 1,
        "public-surface-inventory: could not derive the field set from .spu_result_fields")
  is_field_row <- vapply(psi_rows, function(r) identical(gv(r, "kind"), "public-output-field"), TRUE)
  field_ids <- sort(vapply(psi_rows[is_field_row], function(r) gv(r, "item_id"), ""))
  check(setequal(field_ids, pub_fields),
        sprintf("public-surface-inventory public-output-field rows != .spu_result_fields (rows=%d, source=%d)",
                length(field_ids), length(pub_fields)))
  for (f in setdiff(pub_fields, field_ids))
    check(FALSE, sprintf("public-surface-inventory: missing public-output-field row '%s'", f))

  # --- curl dependency + migration surface (§10) explicitly present -----------
  n_curl <- sum(vapply(psi_rows, function(r) identical(gv(r, "kind"), "curl-dependency"), TRUE))
  n_migr <- sum(vapply(psi_rows, function(r) identical(gv(r, "kind"), "migration-surface"), TRUE))
  check(n_curl >= 1L, "public-surface-inventory: no curl-dependency row (§10 curl surface)")
  check(n_migr >= 1L, "public-surface-inventory: no migration-surface row (§10 migration surface)")
}
psi_checks <- (pass + length(fail)) - psi_checks_before

## --- G2 register: contradictions (§6 artifact 2) -----------------------------
## Parses the artifact-2 contradiction register and enforces: PROPOSED envelope
## (the register artifact itself is not sealed into the manifest until
## cp-snapshot-2), the required columns, the 12-row C-01..C-12 bijection (no
## missing/extra/dup), related_rcon values all within RCON-01..RCON-10, and a
## non-empty bound_owner_tier.
##
## G2 FINALIZATION (RURL-oknltrux): every contradiction now carries an owner
## disposition (§7 G2 exit), projected from the accepted P-tier. So each row is
## asserted DISPOSED, not scaffold: disposition_state == ACCEPTED, a typed
## disposition_type in {ACCEPTED, REJECTED, SUPERSEDED, COMPATIBILITY-ONLY}
## (§5 intro), a non-placeholder owner_decision_ref naming the accepted decision,
## and affected_claims / invalidated_artifacts filled (no "TBD at disposition").
con_path <- file.path(reg_dir, "contradictions.md")
con_pat  <- rschemas$record_types$register$variants$contradiction$id_pattern
con_reg  <- if (file.exists(con_path)) read_rows(con_path) else NULL
check(!is.null(con_reg), "contradictions.md: no parseable Rows table")
con_n <- 0L
con_checks_before <- pass + length(fail)
if (!is.null(con_reg)) {
  ctxt <- paste(readLines(con_path, warn = FALSE), collapse = "\n")
  # envelope: every required field present, lifecycle_state == PROPOSED
  for (ef in env_fields) {
    check(grepl(sprintf("\\|\\s*%s\\s*\\|", ef), ctxt),
          sprintf("[missing_required_fields] contradictions.md: envelope field '%s'", ef))
  }
  check(grepl("\\|\\s*lifecycle_state\\s*\\|\\s*PROPOSED\\s*\\|", ctxt),
        "contradictions.md: lifecycle_state must be PROPOSED")

  # required row columns present
  con_cols <- c("id", "contradiction", "required_disposition", "related_rcon",
                "bound_owner_tier", "disposition_type", "disposition_state",
                "authority", "owner_decision_ref", "affected_claims",
                "invalidated_artifacts", "verification_ref")
  for (col in con_cols) {
    check(col %in% con_reg$header,
          sprintf("[missing_required_fields] contradictions.md: column '%s'", col))
  }

  con_rows <- con_reg$rows
  con_n <- length(con_rows)
  disp_types <- c("ACCEPTED", "REJECTED", "SUPERSEDED", "COMPATIBILITY-ONLY")
  # per-row: id pattern, DISPOSED shape, related_rcon in range, tier present
  for (r in con_rows) {
    id <- gv(r, "id")
    check(grepl(con_pat, id),
          sprintf("contradictions.md: id '%s' fails %s", id, con_pat))
    # G2: every contradiction is owner-disposed (§7 G2). The row's
    # disposition_state is the lifecycle state of the disposition itself
    # (ACCEPTED = owner-accepted), independent of disposition_type (which may
    # itself be REJECTED — the claim is rejected, but that rejection is accepted).
    check(identical(gv(r, "disposition_state"), "ACCEPTED"),
          sprintf("[unknown_states] contradictions %s: disposition_state '%s' != ACCEPTED (G2 disposition)",
                  id, gv(r, "disposition_state")))
    check(gv(r, "disposition_type") %in% disp_types,
          sprintf("contradictions %s: disposition_type '%s' not in {%s}",
                  id, gv(r, "disposition_type"), paste(disp_types, collapse = ", ")))
    odr <- gv(r, "owner_decision_ref")
    check(!is.na(odr) && nzchar(odr) &&
            !grepl("TBD|pending", odr, ignore.case = TRUE),
          sprintf("contradictions %s: owner_decision_ref must name the accepted decision (got '%s')",
                  id, odr %||% "<NA>"))
    for (col in c("affected_claims", "invalidated_artifacts")) {
      v <- gv(r, col)
      check(!is.na(v) && nzchar(v) && !grepl("TBD at disposition", v, fixed = TRUE),
            sprintf("contradictions %s: %s still scaffold placeholder ('%s')",
                    id, col, v %||% "<NA>"))
    }
    check(!is.na(gv(r, "bound_owner_tier")) && nzchar(gv(r, "bound_owner_tier")),
          sprintf("contradictions %s: empty bound_owner_tier", id))
    rcons <- regmatches(gv(r, "related_rcon"),
                        gregexpr("RCON-[0-9]+", gv(r, "related_rcon")))[[1]]
    check(length(rcons) >= 1,
          sprintf("contradictions %s: no related_rcon", id))
    for (rc in rcons) {
      n <- suppressWarnings(as.integer(sub("RCON-", "", rc)))
      check(!is.na(n) && n >= 1L && n <= 10L,
            sprintf("contradictions %s: related_rcon '%s' outside RCON-01..RCON-10", id, rc))
    }
  }
  # 12-row C-01..C-12 bijection (no missing / extra / duplicate)
  con_ids <- vapply(con_rows, function(r) gv(r, "id"), "")
  dup_con <- unique(con_ids[duplicated(con_ids)])
  check(length(dup_con) == 0,
        sprintf("[duplicate_ids] contradictions: %s", paste(dup_con, collapse = ", ")))
  con_expect <- sprintf("C-%02d", 1:12)
  check(length(con_ids) == 12L,
        sprintf("contradictions register expected 12 rows, got %d", length(con_ids)))
  check(setequal(con_ids, con_expect),
        sprintf("contradictions rows != C-01..C-12 (got %s)", paste(sort(con_ids), collapse = ", ")))
  for (e in setdiff(con_expect, con_ids))
    check(FALSE, sprintf("contradictions: missing row '%s'", e))
  for (e in setdiff(con_ids, con_expect))
    check(FALSE, sprintf("contradictions: extra row '%s' not in C-01..C-12", e))
}
con_checks <- (pass + length(fail)) - con_checks_before

## --- Gate-acceptance records: input-hash integrity (G2.A onward) --------------
## Each design/work/url-v3/gates/*.md records the exact input hashes captured at
## the gate's acceptance commit. The envelope state must be ACCEPTED, and every
## '## Inputs' hash is recomputed here; a mismatch fails the record, which
## reopens the gate acceptance and (via ci-gate) the control plane. This is the
## machine half of the §7 G2 reopening rule. (RURL-uksahklp; §6 lifecycle.)
##
## SUPERSESSION (RURL-kpyapioi). A gate's reopening rule states that
## "re-acceptance requires a new gate-acceptance record superseding this one".
## Until this section understood supersession that rule was NOT executable: the
## predecessor stayed in gates/, so it was still hash-checked against inputs it
## no longer describes, and the only ways to green the build were to sweep its
## hashes (which defeats the reopening rule) or to hide the file from the glob
## (which makes an audit record invisible by a path trick). Both are worse than
## the problem.
##
## So a record may instead declare `state: SUPERSEDED` with a non-empty
## `superseded_by`. Such a record is retained and readable but is NOT
## hash-checked -- it is history, describing inputs as they stood at ITS
## acceptance commit, and drift against today's tree is expected rather than a
## defect. Two rules keep that from becoming an escape hatch:
##   * `superseded_by` must name a gate record that EXISTS in gates/, so a
##     record cannot retire into a dangling reference; and
##   * each `gate` value must have EXACTLY ONE ACCEPTED record, so supersession
##     can never leave a gate with zero live acceptances (nor two rival ones).
## The live record is hash-checked exactly as before. Nothing is weakened for
## the acceptance that is actually in force.
gates_dir <- file.path(root, "gates")
gate_n <- 0L
gate_checks_before <- pass + length(fail)
gate_field <- function(gln, name) {
  hit <- grep(sprintf("^\\|\\s*%s\\s*\\|", name), gln, value = TRUE)
  if (length(hit) != 1) return(NA_character_)
  cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", hit[[1]])),
                           "\\|")[[1]])
  if (length(cells) >= 2) cells[[2]] else NA_character_
}
if (dir.exists(gates_dir)) {
  gate_live <- list()   # gate id -> count of ACCEPTED records
  gate_ids <- character(0)
  for (gf in list.files(gates_dir, pattern = "\\.md$", full.names = TRUE)) {
    gln <- readLines(gf, warn = FALSE)
    gate_ids <- c(gate_ids, gate_field(gln, "id"))
  }
  for (gf in list.files(gates_dir, pattern = "\\.md$", full.names = TRUE)) {
    gln <- readLines(gf, warn = FALSE)
    st <- grep("^\\|\\s*state\\s*\\|", gln, value = TRUE)
    superseded <- length(st) == 1 && grepl("\\|\\s*SUPERSEDED\\s*\\|", st[[1]])
    gname <- gate_field(gln, "gate")
    if (superseded) {
      sby <- gate_field(gln, "superseded_by")
      check(!is.na(sby) && nzchar(sby) && !identical(sby, "pending"),
            sprintf("gates: %s is SUPERSEDED but names no superseded_by",
                    basename(gf)))
      check(!is.na(sby) && sby %in% gate_ids,
            sprintf("gates: %s superseded_by '%s' names no record in gates/",
                    basename(gf), sby))
      # Retained as history; its inputs describe its own acceptance commit and
      # are deliberately not recomputed against today's tree.
      next
    }
    if (!is.na(gname)) {
      gate_live[[gname]] <- (gate_live[[gname]] %||% 0L) + 1L
    }
    check(length(st) == 1 && grepl("\\|\\s*ACCEPTED\\s*\\|", st[[1]]),
          sprintf("gates: %s envelope state must be ACCEPTED or SUPERSEDED",
                  basename(gf)))
    h <- which(grepl("^##\\s+Inputs\\s*$", gln))
    check(length(h) == 1,
          sprintf("gates: %s must have exactly one '## Inputs' section", basename(gf)))
    if (length(h) == 1) {
      nxt <- which(grepl("^##\\s", gln) & seq_along(gln) > h)
      end <- if (length(nxt)) min(nxt) - 1L else length(gln)
      tbl <- gln[(h + 1):end]
      tbl <- tbl[grepl("^\\|", tbl) & !grepl("^\\|[-:[:space:]|]*$", tbl)]
      rows <- if (length(tbl) >= 1) tbl[-1] else character(0)  # drop | path | sha256 | header
      check(length(rows) >= 1, sprintf("gates: %s '## Inputs' has no rows", basename(gf)))
      for (r in rows) {
        cells <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", r)), "\\|")[[1]])
        gate_n <- gate_n + 1L
        p <- cells[[1]]
        recorded <- if (length(cells) >= 2) cells[[2]] else ""
        if (!file.exists(p)) {
          check(FALSE, sprintf("gates: %s input missing: %s", basename(gf), p))
        } else {
          got <- sha256_of(p)
          check(identical(got, recorded),
                sprintf("gates: %s input hash drift for %s (recorded %s, got %s) — gate acceptance reopened",
                        basename(gf), p, substr(recorded, 1, 12), substr(got, 1, 12)))
        }
      }
    }
  }
  # Supersession must never leave a gate unattended, nor with rival live
  # acceptances. Exactly one ACCEPTED record per gate.
  for (g in names(gate_live)) {
    check(identical(gate_live[[g]], 1L),
          sprintf("gates: gate %s must have exactly one ACCEPTED record (found %d)",
                  g, gate_live[[g]]))
  }
}
gate_checks <- (pass + length(fail)) - gate_checks_before

## --- G3 contract-family records (§6 artifacts 3–10 + 4 + the G3.X capstone) ----
## The ten design/work/url-v3/contracts/*.md are a record family parallel to the
## registers and gate-acceptance records. This section is the validator coverage
## each contract's envelope promises ("<name> validator section stages with the
## cp-snapshot-3 seal"). It asserts, WITHOUT inventing product semantics, only what
## each contract's own envelope + completion_rule already promise:
##   * common envelope: the 17 required fields non-empty; lifecycle_state PROPOSED
##     (the seal is a manifest present:true hash-pin, NOT an envelope flip — mirrors
##     the registers); tracked_location == the file's own path; unique id.
##   * tamper-evident ## Inputs: each projected source exists and its recorded
##     sha256 recomputes equal (same mechanism as gates/*.md) — a drifted projection
##     FAILS the record, exactly like the G2-acceptance reopening rule.
##   * cell discipline (the completion_rule's "each carry a non-placeholder
##     owner_decision_ref with status SETTLED or an explicit status OPEN"): every
##     matrix table carrying an `owner_decision_ref` + `status` column pair has each
##     row status in {SETTLED, OPEN}, and each SETTLED row cites a non-placeholder
##     ref; every `## Open cells` section that lists an -O cell states destinations.
## The canonical-state contract (artifact 3) additionally gets the deep staged
## section (_scratch/orchestrate/g3-seal-staging/canonical-state-validator-section.md):
## Rows columns/enums, 18-field completeness, verdict-layer sets, open-cell coverage.
## Objective per-contract counts (the capstone's five criterion verdicts; the three
## caches) are asserted only where the contract states an exact cardinality.
## (RURL-huneoffx; the cp-snapshot-3 seal.)
##
## ARTIFACT 4 IS TWO FILES (P0.6; RURL-oygqsykd). public-surface-closure.md is the
## INVARIANT (I1-I5 + the owning-contract legend) and is a G3 `## Inputs` hash pin;
## public-surface-disposition.md is the ROSTER (one row per surface cell) and is
## deliberately NOT a gate input, because G3 criterion 1 is a property over the
## surface, not a statement about its size. Pinning the roster in the gate made
## every added export reopen contract-matrix closure for a change that SATISFIED
## the property. So the roster's conformance is asserted here instead, and — this
## is the part that matters — the population is DERIVED from NAMESPACE and
## `.spu_result_fields` and compared BY NAME. No surface count is written in this
## file or in either contract; a transcribed number is a stale fact waiting to
## happen, and the 51->52 growth that left "29 exports" in three places of the
## closure prose is the proof.
contracts_dir <- file.path(root, "contracts")
contract_n <- 0L
contract_checks_before <- pass + length(fail)

# Split a table row into cells, honoring GitHub's backslash-escaped `\|` pipes
# (a `\|` inside a cell is literal, not a column separator) via a perl lookbehind.
.tcells  <- function(s) {
  s <- sub("^\\s*\\|", "", sub("\\|\\s*$", "", s))
  parts <- strsplit(s, "(?<!\\\\)\\|", perl = TRUE)[[1]]
  trimws(gsub("\\|", "|", parts, fixed = TRUE))
}
.is_trow <- function(s) grepl("^\\s*\\|", s)
.is_tsep <- function(s) grepl("^\\s*\\|[-:|[:space:]]*$", s) & grepl("-", s)
# Parse every GitHub-style pipe table in a line vector into list(header, rows).
parse_pipe_tables <- function(ln) {
  tabs <- list(); i <- 1L; N <- length(ln)
  while (i <= N) {
    if (.is_trow(ln[i]) && i < N && .is_tsep(ln[i + 1L])) {
      header <- .tcells(ln[i]); j <- i + 2L; rows <- list()
      while (j <= N && .is_trow(ln[j]) && !.is_tsep(ln[j])) {
        cs <- .tcells(ln[j])
        rows[[length(rows) + 1L]] <- stats::setNames(cs, header[seq_along(cs)])
        j <- j + 1L
      }
      tabs[[length(tabs) + 1L]] <- list(header = header, rows = rows); i <- j
    } else i <- i + 1L
  }
  tabs
}
# The 2-column ## Envelope table -> named character.
read_envelope <- function(ln) {
  h <- which(grepl("^##\\s+Envelope\\s*$", ln)); if (length(h) != 1) return(NULL)
  nxt <- which(grepl("^##\\s", ln) & seq_along(ln) > h)
  end <- if (length(nxt)) min(nxt) - 1L else length(ln)
  tbl <- ln[(h + 1):end]; tbl <- tbl[.is_trow(tbl) & !.is_tsep(tbl)]
  kv <- list()
  for (r in tbl[-1]) { cs <- .tcells(r); if (length(cs) >= 2 && nzchar(cs[[1]])) kv[[cs[[1]]]] <- cs[[2]] }
  kv
}
# Lines of a "## <name>" section (exclusive of the next "## ").
section_lines <- function(ln, name) {
  h <- which(grepl(sprintf("^##\\s+%s\\s*$", name), ln)); if (length(h) != 1) return(character(0))
  nxt <- which(grepl("^##\\s", ln) & seq_along(ln) > h)
  end <- if (length(nxt)) min(nxt) - 1L else length(ln)
  ln[(h + 1):end]
}
.placeholder <- function(x) is.na(x) || !nzchar(trimws(x %||% "")) ||
  grepl("^(—|-|tbd|pending|n/?a)$", trimws(x), ignore.case = TRUE)

# --- artifact-4 helpers (P0.6: invariant half + roster half) -----------------
# The three roster table shapes. Named once so section F can assert their ABSENCE
# from the gate-pinned invariant and section G their presence in the roster.
PSD_EXPORT_HDR <- c("export", "owning contract(s)", "v3 disposition", "status")
PSD_FIELD_HDR  <- c("field", "owning contract(s)", "v3 disposition", "status")
PSD_ITEM_HDR   <- c("item", "owning contract", "v3 disposition", "status")

# G3 leaf -> owning contract filename, read out of the invariant's legend table.
# Read rather than hardcoded: the legend is the invariant's to state, and a leaf
# that names a nonexistent contract must fail loudly instead of being ignored.
psd_legend <- function(ln) {
  out <- list()
  lg <- Filter(function(t) "G3 leaf" %in% t$header && "contract" %in% t$header,
               parse_pipe_tables(ln))
  if (length(lg) != 1) return(out)
  for (r in lg[[1]]$rows) {
    leaf <- trimws(gv(r, "G3 leaf") %||% "")
    cn <- gsub("`", "", trimws(gv(r, "contract") %||% ""), fixed = TRUE)
    if (grepl("^G3\\.[0-9A-Z]+$", leaf) && nzchar(cn)) out[[leaf]] <- paste0(cn, ".md")
  }
  out
}

env_required <- c("id", "name", "artifact_number", "schema_version", "tracked_location",
                  "owner", "single_writer", "lifecycle_state", "dependencies", "bound_decision",
                  "bound_evidence", "closes_finding", "completion_rule", "content_hash",
                  "approval_evidence", "validation_command", "validator_note")
seen_cids <- character(0)
if (dir.exists(contracts_dir)) {
  for (cf in sort(list.files(contracts_dir, pattern = "\\.md$", full.names = TRUE))) {
    bn <- basename(cf); ln <- readLines(cf, warn = FALSE)
    contract_n <- contract_n + 1L

    ## A. common envelope --------------------------------------------------------
    env <- read_envelope(ln)
    check(!is.null(env), sprintf("contracts %s: no parseable ## Envelope", bn))
    if (!is.null(env)) {
      for (ef in env_required)
        check(!is.null(env[[ef]]) && nzchar(env[[ef]]),
              sprintf("[missing_required_fields] contracts %s: envelope field '%s'", bn, ef))
      check(identical(env[["lifecycle_state"]], "PROPOSED"),
            sprintf("[unknown_states] contracts %s: lifecycle_state must be PROPOSED until cp-snapshot-3 (got '%s')",
                    bn, env[["lifecycle_state"]] %||% "<NA>"))
      check(identical(env[["tracked_location"]] %||% "", cf),
            sprintf("contracts %s: tracked_location '%s' != own path '%s'",
                    bn, env[["tracked_location"]] %||% "<NA>", cf))
      check(!is.null(env[["single_writer"]]) && grepl("owner", env[["single_writer"]], ignore.case = TRUE),
            sprintf("contracts %s: single_writer must name the repository owner", bn))
      cid <- env[["id"]] %||% ""
      check(nzchar(cid) && !(cid %in% seen_cids),
            sprintf("[duplicate_ids] contracts %s: id '%s'", bn, cid))
      seen_cids <- c(seen_cids, cid)
    }
    for (sec in c("Inputs", "Scope boundaries", "Open cells"))
      check(any(grepl(sprintf("^##\\s+%s\\s*$", sec), ln)),
            sprintf("contracts %s: missing '## %s' section", bn, sec))

    ## B. tamper-evident ## Inputs ----------------------------------------------
    itbl <- section_lines(ln, "Inputs"); itbl <- itbl[.is_trow(itbl) & !.is_tsep(itbl)]
    irows <- if (length(itbl) >= 1) itbl[-1] else character(0)
    seen_paths <- character(0)
    for (r in irows) {
      cs <- .tcells(r); p <- cs[[1]]; recorded <- if (length(cs) >= 2) cs[[2]] else ""
      if (!grepl("^design/work/url-v3/", p)) next
      check(!(p %in% seen_paths), sprintf("contracts %s: duplicate ## Inputs path %s", bn, p))
      seen_paths <- c(seen_paths, p)
      if (!file.exists(p)) {
        check(FALSE, sprintf("contracts %s: ## Inputs path missing: %s", bn, p))
      } else {
        got <- sha256_of(p)
        check(identical(got, recorded),
              sprintf("contracts %s: ## Inputs hash drift for %s (recorded %s, got %s) — projection stale",
                      bn, p, substr(recorded, 1, 12), substr(got, 1, 12)))
      }
    }
    check(length(seen_paths) >= 1, sprintf("contracts %s: ## Inputs has no source rows", bn))

    ## C. cell discipline over owner_decision_ref+status matrix tables -----------
    disc_tables <- 0L
    for (tb in parse_pipe_tables(ln)) {
      if (!("status" %in% tb$header && "owner_decision_ref" %in% tb$header)) next
      disc_tables <- disc_tables + 1L
      for (r in tb$rows) {
        raw <- gv(r, "status")
        if (is.na(raw) || !nzchar(trimws(raw))) next        # spacer/continuation row
        st <- sub("\\s.*$", "", trimws(raw))                 # leading token
        check(st %in% c("SETTLED", "OPEN"),
              sprintf("contracts %s: status '%s' not in {SETTLED, OPEN}", bn, raw))
        if (identical(st, "SETTLED"))
          check(!.placeholder(gv(r, "owner_decision_ref")),
                sprintf("contracts %s: SETTLED row cites placeholder owner_decision_ref ('%s')",
                        bn, gv(r, "owner_decision_ref") %||% "<NA>"))
      }
    }

    ## D. Open cells name destinations ------------------------------------------
    obody <- section_lines(ln, "Open cells")
    if (any(grepl("[A-Z]+-O[0-9]+", obody)))
      check(any(grepl("Settles at|destination|CLOSED|closed", obody)),
            sprintf("contracts %s: ## Open cells lists -O cells but names no settlement destination", bn))

    ## E. deep section — canonical-state (artifact 3) ---------------------------
    if (identical(bn, "canonical-state-contract.md") && !is.null(env)) {
      check(identical(env[["artifact_number"]], "3"),
            sprintf("canonical-state: artifact_number must be 3 (got '%s')", env[["artifact_number"]] %||% "<NA>"))
      check(identical(env[["bound_decision"]], "P1.1"),
            "canonical-state: bound_decision must be P1.1")
      check(identical(env[["bound_evidence"]], "S1"),
            "canonical-state: bound_evidence must be S1")
      check(grepl("RCON-02", env[["closes_finding"]] %||% ""),
            "canonical-state: closes_finding must name RCON-02")
      cr <- read_rows(cf)
      check(!is.null(cr), "canonical-state: no parseable ## Rows table")
      if (!is.null(cr)) {
        want <- c("field", "type", "presence", "provenance", "invariants",
                  "public_projection", "lifecycle", "owner_decision_ref", "status")
        check(identical(cr$header, want),
              sprintf("canonical-state: Rows columns must be exactly [%s] (got [%s])",
                      paste(want, collapse = ", "), paste(cr$header, collapse = ", ")))
        fields <- vapply(cr$rows, function(r) gv(r, "field"), "")
        dupf <- unique(fields[duplicated(fields)])
        check(length(dupf) == 0, sprintf("canonical-state: duplicate field(s) %s", paste(dupf, collapse = ", ")))
        presence_ok    <- c("absent", "present-empty", "present-nonempty", "n/a")
        provenance_ok  <- c("source", "parsed", "derived", "classifier", "PSL", "projection")
        projection_ok  <- c("public", "internal", "companion")
        for (r in cr$rows) {
          fn <- gv(r, "field")
          for (col in c("type", "presence", "provenance", "invariants", "public_projection", "lifecycle")) {
            v <- gv(r, col)
            check(!is.na(v) && nzchar(trimws(v %||% "")),
                  sprintf("canonical-state %s: empty '%s' (no unowned cells)", fn, col))
          }
          # presence: strip a trailing "(...)" note, then each " / "-joined token
          # must be in the closed set (n/a is a single token with a bare slash).
          pbase <- sub("\\s*\\(.*\\)\\s*$", "", gv(r, "presence") %||% "")
          pv <- trimws(strsplit(pbase, "\\s+/\\s+")[[1]])
          check(length(pv) >= 1 && all(pv %in% presence_ok),
                sprintf("canonical-state %s: presence '%s' outside {%s}", fn, gv(r, "presence"),
                        paste(presence_ok, collapse = ", ")))
          check(sub("\\s.*$", "", gv(r, "provenance") %||% "") %in% provenance_ok,
                sprintf("canonical-state %s: provenance leading token '%s' outside set", fn, gv(r, "provenance")))
          check(sub("\\s.*$", "", gv(r, "public_projection") %||% "") %in% projection_ok,
                sprintf("canonical-state %s: public_projection leading token '%s' outside set", fn, gv(r, "public_projection")))
          st <- gv(r, "status")
          check(st %in% c("SETTLED", "OPEN"),
                sprintf("canonical-state %s: status '%s' not in {SETTLED, OPEN}", fn, st))
          if (identical(st, "SETTLED"))
            check(grepl("^P1\\.1@[0-9a-f]{7,40}$", gv(r, "owner_decision_ref") %||% ""),
                  sprintf("canonical-state %s: SETTLED owner_decision_ref must match P1.1@<sha> (got '%s')",
                          fn, gv(r, "owner_decision_ref") %||% "<NA>"))
        }
        # completeness: the 18 public .spu_result_fields present as public rows
        pub18 <- c("original_url", "scheme", "host", "port", "path", "query", "fragment",
                   "user", "password", "domain", "tld", "domain_ascii", "domain_unicode",
                   "tld_ascii", "tld_unicode", "is_ip_host", "clean_url", "parse_status")
        for (f in pub18) {
          idx <- which(fields == f)
          check(length(idx) == 1 &&
                  identical(sub("\\s.*$", "", gv(cr$rows[[idx[1]]], "public_projection") %||% ""), "public"),
                sprintf("canonical-state: public field '%s' missing or not public_projection=public", f))
        }
        # structural-kind fields present
        for (f in c("host_kind", "authority_kind", "query_kind", "fragment_kind"))
          check(f %in% fields, sprintf("canonical-state: structural field '%s' missing", f))
        # verdict-layer fields: companion projection + layer3 enum set
        for (f in c("layer1_syntax_verdict", "layer2_policy_verdict", "layer3_annotation_state")) {
          idx <- which(fields == f)
          check(length(idx) == 1 &&
                  grepl("^companion", gv(cr$rows[[idx[1]]], "public_projection") %||% ""),
                sprintf("canonical-state: verdict field '%s' missing or not companion", f))
        }
        idx3 <- which(fields == "layer3_annotation_state")
        if (length(idx3) == 1) {
          ty <- gv(cr$rows[[idx3[1]]], "type") %||% ""
          for (tok in c("not-requested", "not-applicable", "known", "unknown", "invalid-input", "dependency-error"))
            check(grepl(tok, ty, fixed = TRUE),
                  sprintf("canonical-state: layer3_annotation_state type must enumerate '%s'", tok))
        }
        # authority_kind stays OPEN (regression guard — P1.1 Q3 deferred it)
        idxa <- which(fields == "authority_kind")
        if (length(idxa) == 1)
          check(identical(gv(cr$rows[[idxa[1]]], "status"), "OPEN"),
                "canonical-state: authority_kind must remain status OPEN (P1.1 Q3 deferred)")
        # every OPEN row's field is named in ## Open cells
        for (r in cr$rows) if (identical(gv(r, "status"), "OPEN")) {
          fn <- gv(r, "field")
          check(any(grepl(fn, obody, fixed = TRUE)),
                sprintf("canonical-state: OPEN field '%s' absent from ## Open cells", fn))
        }
      }
    }

    ## F. artifact 4, invariant half — the ownership rule itself -----------------
    ## The G3-pinned file must state I1-I5 and the legend, and must NOT contain a
    ## roster table: the split is enforced structurally, not just described in a
    ## comment, so a well-meaning future edit cannot quietly re-merge the halves
    ## and restore the cascade.
    if (identical(bn, "public-surface-closure.md")) {
      tabs <- parse_pipe_tables(ln)
      for (h in list(PSD_EXPORT_HDR, PSD_FIELD_HDR, PSD_ITEM_HDR))
        check(!any(vapply(tabs, function(t) identical(t$header, h), TRUE)),
              sprintf("public-surface-closure: roster table [%s] belongs in public-surface-disposition.md (P0.6)",
                      paste(h, collapse = " | ")))
      for (i in sprintf("I%d", 1:5))
        check(any(grepl(sprintf("\\*\\*%s ", i), ln, fixed = FALSE)),
              sprintf("public-surface-closure: invariant clause %s is not stated", i))
      check(!any(grepl("^##\\s+Bijection", ln)),
            "public-surface-closure: a Bijection (count) table must not live in the gate-pinned invariant (P0.6 I1)")
      lg <- psd_legend(ln)
      check(length(lg) >= 8L,
            sprintf("public-surface-closure: owning-contract legend must map at least 8 G3 leaves (got %d)",
                    length(lg)))
      for (leaf in names(lg))
        check(file.exists(file.path(contracts_dir, lg[[leaf]])),
              sprintf("public-surface-closure: legend leaf %s names no contract file (%s)", leaf, lg[[leaf]]))
    }

    ## G. artifact 4, roster half — I1-I5 over the per-cell rows -----------------
    ## Not a gate input (P0.6). Everything here is DERIVED: the export set from
    ## NAMESPACE, the field set from .spu_result_fields, the legend from the
    ## invariant half. No count is transcribed, so growing the surface cannot make
    ## this section stale.
    if (identical(bn, "public-surface-disposition.md")) {
      tabs <- parse_pipe_tables(ln)
      exp_tab <- Filter(function(t) identical(t$header, PSD_EXPORT_HDR), tabs)
      fld_tab <- Filter(function(t) identical(t$header, PSD_FIELD_HDR), tabs)
      itm_tab <- Filter(function(t) identical(t$header, PSD_ITEM_HDR), tabs)
      check(length(exp_tab) == 1, "public-surface-disposition: exactly one exported-function roster table")
      check(length(fld_tab) == 1, "public-surface-disposition: exactly one public-output-field roster table")
      check(length(itm_tab) == 1, "public-surface-disposition: exactly one curl/migration roster table")

      ## --- I1 + I2: population derived from source, compared BY NAME ----------
      unq <- function(x) gsub("`", "", trimws(x %||% ""), fixed = TRUE)
      if (length(exp_tab) == 1) {
        rows_e <- vapply(exp_tab[[1]]$rows, function(r) unq(gv(r, "export")), "")
        for (e in setdiff(psd_ns_exports(), rows_e))
          check(FALSE, sprintf("public-surface-disposition: NAMESPACE exports '%s' with no roster row (I2)", e))
        for (e in setdiff(rows_e, psd_ns_exports()))
          check(FALSE, sprintf("public-surface-disposition: roster row '%s' is not a NAMESPACE export (I2)", e))
        dup <- unique(rows_e[duplicated(rows_e)])
        check(length(dup) == 0,
              sprintf("public-surface-disposition: duplicate export row(s) %s (I2)", paste(dup, collapse = ", ")))
      }
      if (length(fld_tab) == 1) {
        rows_f <- vapply(fld_tab[[1]]$rows, function(r) unq(gv(r, "field")), "")
        src_f <- psd_result_fields()
        check(length(src_f) >= 1,
              "public-surface-disposition: could not derive the field set from .spu_result_fields (I1)")
        for (f in setdiff(src_f, rows_f))
          check(FALSE, sprintf("public-surface-disposition: .spu_result_fields has '%s' with no roster row (I2)", f))
        for (f in setdiff(rows_f, src_f))
          check(FALSE, sprintf("public-surface-disposition: roster row '%s' is not a .spu_result_fields entry (I2)", f))
      }
      if (length(itm_tab) == 1) {
        items <- vapply(itm_tab[[1]]$rows, function(r) unq(gv(r, "item")), "")
        check(sum(grepl("^curl-", items)) >= 1L,
              "public-surface-disposition: no curl-dependency row (§10 curl surface)")
        check(sum(items == "migration-surface") == 1L,
              "public-surface-disposition: exactly one migration-surface row required")
      }

      ## --- the Bijection table is VERIFIED against source, never pinned -------
      bij <- Filter(function(t) "surface class" %in% t$header && "count" %in% t$header, tabs)
      check(length(bij) == 1, "public-surface-disposition: bijection table (surface class | count) not found")
      if (length(bij) == 1 && length(exp_tab) == 1 && length(fld_tab) == 1 && length(itm_tab) == 1) {
        labels <- vapply(bij[[1]]$rows, function(r) tolower(gv(r, "surface class") %||% ""), "")
        nums <- vapply(bij[[1]]$rows, function(r) {
          d <- gsub("[^0-9]", "", gv(r, "count") %||% "")
          if (nzchar(d)) as.integer(d) else NA_integer_
        }, integer(1))
        tot_i <- grep("total", labels)
        comp_i <- setdiff(seq_along(labels), tot_i)
        derived <- length(psd_ns_exports()) + length(psd_result_fields()) +
          length(itm_tab[[1]]$rows)
        check(sum(nums[comp_i], na.rm = TRUE) == derived,
              sprintf("public-surface-disposition: bijection components sum to %d but the derived surface is %d (NAMESPACE + .spu_result_fields + curl/migration rows)",
                      sum(nums[comp_i], na.rm = TRUE), derived))
        check(length(tot_i) == 1 && identical(nums[tot_i[1]], derived),
              sprintf("public-surface-disposition: bijection total is %s but the derived surface is %d",
                      if (length(tot_i) == 1) nums[tot_i[1]] else "<none>", derived))
      }

      ## --- I2/I3/I4: every row is owned, and every citation resolves ----------
      inv_ln <- if (file.exists(file.path(contracts_dir, "public-surface-closure.md")))
        readLines(file.path(contracts_dir, "public-surface-closure.md"), warn = FALSE) else character(0)
      lg <- psd_legend(inv_ln)
      check(length(lg) >= 8L,
            "public-surface-disposition: could not read the owning-contract legend from the invariant half")
      ctext <- list()
      for (f in unique(unlist(lg))) {
        p <- file.path(contracts_dir, f)
        ctext[[f]] <- if (file.exists(p)) readLines(p, warn = FALSE) else character(0)
      }
      for (tb in c(exp_tab, fld_tab, itm_tab)) {
        key <- tb$header[[1]]
        own_col <- if ("owning contract(s)" %in% tb$header) "owning contract(s)" else "owning contract"
        for (r in tb$rows) {
          cell <- unq(gv(r, key))
          owner_raw <- gv(r, own_col) %||% ""
          st_raw <- gv(r, "status") %||% ""
          st <- sub("\\s.*$", "", trimws(st_raw))
          check(st %in% c("SETTLED", "OPEN"),
                sprintf("public-surface-disposition %s: status '%s' not in {SETTLED, OPEN}", cell, st_raw))
          leaves <- unique(regmatches(owner_raw, gregexpr("G3\\.[0-9A-Z]+", owner_raw))[[1]])
          downstream <- grepl("artifact 11|artifact 4", owner_raw)
          check(length(leaves) >= 1 || downstream,
                sprintf("public-surface-disposition %s: names no owning contract (I2): '%s'", cell, owner_raw))
          for (lf in leaves)
            check(lf %in% names(lg),
                  sprintf("public-surface-disposition %s: owning contract '%s' is not in the legend (I2)", cell, lf))
          disp <- gv(r, "v3 disposition") %||% ""
          if (identical(st, "SETTLED")) {
            # I3: a SETTLED row must cite the accepted decision its owner projects
            # (P-tier ref, an ADR, or — for the migration row — its discharge).
            ok <- grepl("P[0-9]+\\.[0-9]+@[0-9a-f]{7}", disp) ||
              grepl("ADR [0-9]{4}", disp) || grepl("discharged", disp)
            check(ok, sprintf("public-surface-disposition %s: SETTLED cites no decision, ADR, or discharge (I3): '%s'",
                              cell, disp))
          } else if (identical(st, "OPEN")) {
            # I3/I4: each -O id cited must EXIST in one of the owning contracts.
            # "HOST-O2/O4" is shorthand: a bare /O<n> inherits the last prefix.
            toks <- regmatches(st_raw, gregexpr("[A-Z]+-O[0-9]+|/O[0-9]+", st_raw))[[1]]
            ids <- character(0); last_pref <- NA_character_
            for (tk in toks) {
              if (grepl("^/O", tk)) {
                if (!is.na(last_pref)) ids <- c(ids, paste0(last_pref, "-", sub("^/", "", tk)))
              } else {
                ids <- c(ids, tk); last_pref <- sub("-O[0-9]+$", "", tk)
              }
            }
            named_downstream <- grepl("artifact 11|S1 s/v|RCON-08|P4 host", st_raw) || downstream
            check(length(ids) >= 1 || named_downstream,
                  sprintf("public-surface-disposition %s: OPEN cites neither an open-cell id nor a named downstream artifact (I3): '%s'",
                          cell, st_raw))
            for (id in unique(ids)) {
              hay <- unlist(ctext[unique(unlist(lg[leaves]))], use.names = FALSE)
              if (length(hay) == 0) hay <- unlist(ctext, use.names = FALSE)
              check(any(grepl(id, hay, fixed = TRUE)),
                    sprintf("public-surface-disposition %s: OPEN cites '%s', which appears in none of its owning contracts (%s) — dangling citation (I3)",
                            cell, id, paste(leaves, collapse = ", ")))
            }
          }
        }
      }

      ## --- I4: the roster opens nothing of its own ----------------------------
      obody2 <- section_lines(ln, "Open cells")
      check(!any(grepl("PSD-O[0-9]+", obody2)),
            "public-surface-disposition: the roster must not define open cells of its own (I4)")
      check(any(grepl("PSC-O", obody2)),
            "public-surface-disposition: ## Open cells must forward to the invariant's PSC-O groups (I4)")
    }

    if (identical(bn, "cross-artifact-consistency.md")) {
      crit <- Filter(function(t) "verdict" %in% t$header && "#" %in% t$header, parse_pipe_tables(ln))
      crit <- Filter(function(t) length(t$rows) == 5L, crit)
      check(length(crit) >= 1,
            "cross-artifact-consistency: criterion table with five (i)-(v) rows not found")
      if (length(crit) >= 1)
        for (r in crit[[1]]$rows)
          check(identical(gv(r, "verdict"), "PASS"),
                sprintf("cross-artifact-consistency: criterion %s verdict must be PASS (got '%s')",
                        gv(r, "#"), gv(r, "verdict")))
    }
    if (identical(bn, "semantic-cache-contract.md")) {
      inv <- Filter(function(t) "cache" %in% t$header && "eviction" %in% t$header, parse_pipe_tables(ln))
      check(length(inv) >= 1 && length(inv[[1]]$rows) == 3L,
            "semantic-cache: cache-inventory table must enumerate exactly 3 caches (P5.1)")
    }
  }
}
contract_checks <- (pass + length(fail)) - contract_checks_before

cat("validate-records.R\n")
cat(sprintf("contracts: %d files, %d added checks\n", contract_n, contract_checks))
cat(sprintf("public-surface-inventory: %d rows, %d added checks\n", psi_n, psi_checks))
cat(sprintf("contradictions: %d rows, %d added checks\n", con_n, con_checks))
cat(sprintf("gate-acceptance inputs: %d checked, %d added checks\n", gate_n, gate_checks))
cat(sprintf("registers: source-claims=%d rows, findings=%d rows\n",
            if (!is.null(sc_reg)) length(sc_reg$rows) else 0L,
            if (!is.null(fd_reg)) length(fd_reg$rows) else 0L))
cat(sprintf("owner-decision records: %d (%s)\n", length(records), paste(known, collapse = ", ")))
cat(sprintf("checks passed: %d\n", pass))
if (length(fail) > 0) {
  cat(sprintf("checks FAILED: %d\n", length(fail)))
  for (f in fail) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("VALIDATION PASSED\n")
