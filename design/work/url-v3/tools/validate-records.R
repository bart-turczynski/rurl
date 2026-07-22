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

cat("validate-records.R\n")
cat(sprintf("owner-decision records: %d (%s)\n", length(records), paste(known, collapse = ", ")))
cat(sprintf("checks passed: %d\n", pass))
if (length(fail) > 0) {
  cat(sprintf("checks FAILED: %d\n", length(fail)))
  for (f in fail) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("VALIDATION PASSED\n")
