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

## --- G1.2 register: public-surface-inventory (§6 artifact 4) -----------------
## Parses the artifact-4 inventory register and enforces: PROPOSED envelope,
## every row DISCOVERED with non-empty required columns, the 29-export NAMESPACE
## bijection over exported-function/exported-data rows (mirrors G1.1's row-count
## bijection), and the 18 public-output-field rows. (RURL-dmsgpcak)
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

  # --- 29-export NAMESPACE bijection over exported-function/exported-data rows -
  ns <- readLines("NAMESPACE", warn = FALSE)
  ns_exports <- regmatches(ns, regexpr("(?<=^export\\()[^)]+", ns, perl = TRUE))
  ns_exports <- sort(ns_exports[nzchar(ns_exports)])
  is_export_row <- vapply(psi_rows,
                          function(r) gv(r, "kind") %in% c("exported-function", "exported-data"), TRUE)
  fn_ids <- sort(vapply(psi_rows[is_export_row], function(r) gv(r, "item_id"), ""))
  check(length(ns_exports) == 29L,
        sprintf("NAMESPACE expected 29 export() lines, got %d", length(ns_exports)))
  check(length(fn_ids) == 29L,
        sprintf("public-surface-inventory expected 29 export rows, got %d", length(fn_ids)))
  check(setequal(fn_ids, ns_exports),
        sprintf("public-surface-inventory export rows != NAMESPACE (rows=%d, NAMESPACE=%d)",
                length(fn_ids), length(ns_exports)))
  for (e in setdiff(ns_exports, fn_ids))
    check(FALSE, sprintf("public-surface-inventory: missing export row '%s'", e))
  for (e in setdiff(fn_ids, ns_exports))
    check(FALSE, sprintf("public-surface-inventory: extra export row '%s' not in NAMESPACE", e))

  # --- 18 public-output-field rows present, exact name set --------------------
  pub_fields <- c("original_url", "scheme", "host", "port", "path", "query",
                  "fragment", "user", "password", "domain", "tld", "domain_ascii",
                  "domain_unicode", "tld_ascii", "tld_unicode", "is_ip_host",
                  "clean_url", "parse_status")
  is_field_row <- vapply(psi_rows, function(r) identical(gv(r, "kind"), "public-output-field"), TRUE)
  field_ids <- sort(vapply(psi_rows[is_field_row], function(r) gv(r, "item_id"), ""))
  check(length(field_ids) == 18L,
        sprintf("public-surface-inventory expected 18 public-output-field rows, got %d", length(field_ids)))
  check(setequal(field_ids, pub_fields),
        sprintf("public-surface-inventory public-output-field rows != 18 known fields (got %d)", length(field_ids)))
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

cat("validate-records.R\n")
cat(sprintf("public-surface-inventory: %d rows, %d added checks\n", psi_n, psi_checks))
cat(sprintf("contradictions: %d rows, %d added checks\n", con_n, con_checks))
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
