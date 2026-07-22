#!/usr/bin/env Rscript
# validate-manifest.R — schema + referential-integrity validator for the rurl 3.0
# protocol-hardening manifest (§6 artifact 1). Packaged by G0.2 (RURL-qcduzpex).
#
# Checks (all must pass; non-zero exit on any failure):
#   1. Required top-level keys are present.
#   2. schema_version is semver (x.y.z).
#   3. Baseline vs snapshot are distinct keys and never conflated.
#   4. Every sources[]/artifacts[]/decisions[] entry and the tracker_snapshot
#      names an existing file whose recomputed SHA-256 matches the record.
#   5. Self-reference rule: manifest.yaml is NOT listed among artifacts/sources.
#   6. Every source carries frozen: true and a stable identity.
#   7. Referential integrity: every decisions/*.md on disk is referenced in
#      decisions[], each with state ACCEPTED (read from the record frontmatter).
#   8. Register referential integrity: any registers[] entry that is marked
#      present must exist and hash; not-yet-created registers are declared, not
#      silently omitted.
#
# Usage:  Rscript design/work/url-v3/tools/validate-manifest.R [MANIFEST_PATH]
# Run from the repository root; manifest paths are repo-relative.

suppressWarnings(suppressMessages({
  ok_yaml <- requireNamespace("yaml", quietly = TRUE)
  ok_digest <- requireNamespace("digest", quietly = TRUE)
}))
if (!ok_yaml || !ok_digest) {
  stop("validate-manifest.R needs the 'yaml' and 'digest' packages")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)
manifest_path <- if (length(args) >= 1) args[[1]] else "design/work/url-v3/manifest.yaml"
workspace <- dirname(manifest_path)

fail <- character(0)
pass <- 0L
check <- function(cond, msg) {
  if (isTRUE(cond)) {
    pass <<- pass + 1L
  } else {
    fail <<- c(fail, msg)
  }
}

if (!file.exists(manifest_path)) stop(sprintf("manifest not found: %s", manifest_path))
m <- yaml::read_yaml(manifest_path)

# 1. required top-level keys
required <- c(
  "schema_version", "source_baseline", "control_plane_snapshot", "repo_commit",
  "sources", "tracker_snapshot", "artifacts", "decisions", "registers",
  "external_revisions", "tool_versions", "restore_instructions"
)
for (k in required) check(!is.null(m[[k]]), sprintf("missing required key: %s", k))

# 2. semver schema_version
check(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", m$schema_version %||% ""),
      sprintf("schema_version not semver: %s", m$schema_version))

# 3. baseline vs snapshot distinct
check(!identical(m$source_baseline, m$control_plane_snapshot),
      "source_baseline and control_plane_snapshot must be distinct")

sha256_of <- function(p) digest::digest(file = p, algo = "sha256")

verify_entry <- function(e, label) {
  p <- e$path
  if (is.null(p) || !file.exists(p)) {
    check(FALSE, sprintf("%s: file missing: %s", label, p %||% "<no path>"))
    return(invisible())
  }
  got <- sha256_of(p)
  check(identical(got, e$sha256),
        sprintf("%s: sha256 mismatch for %s (recorded %s, got %s)",
                label, p, substr(e$sha256 %||% "", 1, 12), substr(got, 1, 12)))
}

# 4 + 6. sources
for (s in m$sources) {
  verify_entry(s, "sources")
  check(isTRUE(s$frozen), sprintf("source not frozen: %s", s$path %||% s$identity))
  check(!is.null(s$identity) && nzchar(s$identity),
        sprintf("source missing identity: %s", s$path))
}

# 4. tracker snapshot
verify_entry(m$tracker_snapshot, "tracker_snapshot")
check(identical(m$tracker_snapshot$as_of_commit, m$control_plane_snapshot),
      "tracker_snapshot.as_of_commit must equal control_plane_snapshot")

# 4. artifacts
for (a in m$artifacts) verify_entry(a, "artifacts")

# 5. self-reference rule
all_paths <- c(vapply(m$sources, function(x) x$path %||% "", ""),
               vapply(m$artifacts, function(x) x$path %||% "", ""))
check(!(manifest_path %in% all_paths) &&
        !any(basename(all_paths) == "manifest.yaml"),
      "self-reference rule: manifest.yaml must not appear in sources/artifacts")

# 4 + 7. decisions referential integrity
read_state <- function(p) {
  ln <- readLines(p, warn = FALSE)
  fm <- grep("^state:\\s*", ln, value = TRUE)
  if (length(fm) == 0) return(NA_character_)
  trimws(sub("^state:\\s*", "", fm[[1]]))
}
for (d in m$decisions) {
  verify_entry(d, "decisions")
  if (!is.null(d$path) && file.exists(d$path)) {
    st <- read_state(d$path)
    check(identical(st, "ACCEPTED"),
          sprintf("decision %s not ACCEPTED (state=%s)", d$path, st %||% "NA"))
    check(identical(d$state, st),
          sprintf("decision %s manifest state '%s' != record state '%s'",
                  d$path, d$state %||% "NA", st %||% "NA"))
  }
}
# every decision record on disk is referenced
decisions_dir <- file.path(workspace, "decisions")
if (dir.exists(decisions_dir)) {
  on_disk <- list.files(decisions_dir, pattern = "\\.md$", full.names = TRUE)
  referenced <- vapply(m$decisions, function(x) x$path %||% "", "")
  for (f in on_disk) {
    check(f %in% referenced,
          sprintf("decision record on disk not referenced in manifest: %s", f))
  }
}

# 8. registers: present ones must verify; declared-absent ones are allowed
for (r in m$registers) {
  if (isTRUE(r$present)) {
    verify_entry(r, "registers")
  } else {
    check(!is.null(r$planned_gate),
          sprintf("absent register %s must declare planned_gate", r$id %||% "?"))
  }
}

cat(sprintf("manifest: %s\n", manifest_path))
cat(sprintf("checks passed: %d\n", pass))
if (length(fail) > 0) {
  cat(sprintf("checks FAILED: %d\n", length(fail)))
  for (f in fail) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("VALIDATION PASSED\n")
