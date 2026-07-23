#!/usr/bin/env Rscript
# ci-gate.R — lifecycle-aware CI driver for the rurl 3.0 control-plane validators.
#
# WHY THIS EXISTS
#   The three control-plane validators (validate-records.R, validate-manifest.R,
#   validate-transitions.R) are the deterministic truth of the §6 control plane,
#   but they are NOT wired into .github/workflows — so a green package CI run has
#   never implied a green control plane. This driver closes that gap by running
#   all three on every push/PR, WITHOUT relaxing any of them.
#
# THE ONE LIFECYCLE TOLERANCE (explicit, not suppression)
#   A PROPOSED owner-decision record legitimately cannot appear in
#   manifest.decisions[] yet: registration requires state ACCEPTED, which is the
#   owner's seal-merge (P0.1 §4). So on a PR that ADDS one or more PROPOSED
#   decision records, validate-manifest's decisions-completeness check fails with
#   EXACTLY one "decision record on disk not referenced in manifest: <path>" line
#   per new PROPOSED file — and nothing else. This driver treats that precise set
#   as expected-red and every other manifest failure as fatal.
#
#   Crucially this is keyed off FACTS, never a branch name: the tolerated set is
#   (files added on this branch relative to the base) ∩ (state: PROPOSED on disk),
#   and the manifest failures must be a SUBSET of the by-design lines those files
#   produce. Therefore:
#     * main / seal / process-evidence PRs add no PROPOSED records → no tolerance
#       is ever applied → all three validators must pass strictly.
#     * a seal PR flips PROPOSED→ACCEPTED and references the files (a MODIFY, not
#       an add) → not in the added set → manifest must pass strictly.
#     * any real manifest defect (sha mismatch, missing key, an ACCEPTED-but-
#       unreferenced record, a referenced-but-still-PROPOSED record, …) is NOT a
#       by-design line → fatal.
#
# BASE REF
#   CI_GATE_BASE_SHA names the commit to diff against (the PR base sha on a
#   pull_request; empty on push to main). Empty/unset → strict mode: no files are
#   tolerated, so all three validators must pass exactly as on main.
#
# Usage:  Rscript design/work/url-v3/tools/ci-gate.R
#   Run from the repository root.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

WS <- "design/work/url-v3"
TOOLS <- file.path(WS, "tools")
DECISIONS <- file.path(WS, "decisions")
MANIFEST <- file.path(WS, "manifest.yaml")

# --- helpers ---------------------------------------------------------------

# Run a validator, capture combined stdout+stderr and exit status.
run_validator <- function(script, args = character(0)) {
  out <- suppressWarnings(system2(
    "Rscript", c(script, args),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status") %||% 0L
  list(status = as.integer(status), lines = out)
}

# Extract the "  - <msg>" failure lines a validator prints after "checks FAILED".
failure_lines <- function(lines) {
  hits <- grep("^  - ", lines, value = TRUE)
  sub("^  - ", "", hits)
}

read_state <- function(p) {
  ln <- readLines(p, warn = FALSE)
  fm <- grep("^state:\\s*", ln, value = TRUE)
  if (length(fm) == 0) return(NA_character_)
  trimws(sub("^state:\\s*", "", fm[[1]]))
}

# Decision records ADDED on this branch relative to the base that are PROPOSED.
newly_added_proposed <- function(base_sha) {
  if (!nzchar(base_sha)) return(character(0))
  diff <- suppressWarnings(system2(
    "git",
    c("diff", "--diff-filter=A", "--name-only",
      sprintf("%s...HEAD", base_sha), "--", DECISIONS),
    stdout = TRUE, stderr = TRUE
  ))
  if (!is.null(attr(diff, "status")) && attr(diff, "status") != 0L) {
    stop("ci-gate: could not diff against base ", base_sha, ":\n",
         paste(diff, collapse = "\n"))
  }
  added <- diff[grepl("\\.md$", diff)]
  added <- added[file.exists(added)]
  added[vapply(added, function(f) identical(read_state(f), "PROPOSED"), logical(1))]
}

say <- function(...) cat(..., "\n", sep = "")

# --- run -------------------------------------------------------------------

base_sha <- Sys.getenv("CI_GATE_BASE_SHA", "")
say("== rurl 3.0 control-plane CI gate ==")
say("base ref for lifecycle diff: ", if (nzchar(base_sha)) base_sha else "(none — strict)")

fatal <- character(0)

# 1 + 2. records and transitions must pass strictly, always.
records <- run_validator(file.path(TOOLS, "validate-records.R"))
say("\n-- validate-records.R --"); say(paste(records$lines, collapse = "\n"))
if (records$status != 0L) fatal <- c(fatal, "validate-records.R FAILED")

transitions <- run_validator(file.path(TOOLS, "validate-transitions.R"))
say("\n-- validate-transitions.R --"); say(paste(transitions$lines, collapse = "\n"))
if (transitions$status != 0L) fatal <- c(fatal, "validate-transitions.R FAILED")

# 3. manifest: strict, except the by-design PROPOSED-not-referenced lines.
manifest <- run_validator(file.path(TOOLS, "validate-manifest.R"), MANIFEST)
say("\n-- validate-manifest.R --"); say(paste(manifest$lines, collapse = "\n"))

proposed <- newly_added_proposed(base_sha)
expected_lines <- sprintf("decision record on disk not referenced in manifest: %s", proposed)

if (manifest$status == 0L) {
  say("\nmanifest: PASS (strict)")
} else {
  actual <- failure_lines(manifest$lines)
  unexpected <- setdiff(actual, expected_lines)
  say("\nmanifest: FAILED strictly — applying lifecycle tolerance")
  say("  newly-added PROPOSED decision records: ",
      if (length(proposed)) paste(proposed, collapse = ", ") else "(none)")
  say("  by-design tolerated failures        : ", length(expected_lines))
  say("  actual manifest failures            : ", length(actual))
  if (length(unexpected)) {
    say("  UNEXPECTED (fatal) failures:")
    for (u in unexpected) say("    - ", u)
    fatal <- c(fatal, sprintf(
      "validate-manifest.R has %d failure(s) not explained by newly-added PROPOSED records",
      length(unexpected)))
  } else {
    say("  all manifest failures are by-design (PROPOSED-decision PR) — tolerated")
  }
}

# --- verdict ---------------------------------------------------------------

say("\n== verdict ==")
if (length(fatal)) {
  for (f in fatal) say("FATAL: ", f)
  say("CONTROL-PLANE GATE: FAIL")
  quit(status = 1L)
}
say("CONTROL-PLANE GATE: PASS")
