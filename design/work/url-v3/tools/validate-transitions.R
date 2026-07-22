#!/usr/bin/env Rscript
# validate-transitions.R — G0.3b lifecycle-transition + gate-propagation
# validator. Loads schema/lifecycle.yaml and every schema/fixtures/*.yaml and
# asserts, for each fixture:
#   (1) legality: every record transition from->to is in lifecycle.transitions;
#   (2) evidence: every transition carries lifecycle.transition_evidence_fields;
#   (3) propagation: for the fixture's change event, each expected record reaches
#       INVALIDATED, each replacement record is DISCOVERED and links (supersedes)
#       an invalidated id, and the named downstream gate is FAILED_OR_REOPENED —
#       never left ACCEPTED while its durable acceptance record is stale.
#
# Run from the repository root:
#   Rscript design/work/url-v3/tools/validate-transitions.R

suppressWarnings(suppressMessages({
  ok <- requireNamespace("yaml", quietly = TRUE)
}))
if (!ok) stop("validate-transitions.R needs the 'yaml' package")
`%||%` <- function(a, b) if (is.null(a)) b else a

root <- "design/work/url-v3"
lifecycle <- yaml::read_yaml(file.path(root, "schema", "lifecycle.yaml"))
trans <- lifecycle$transitions
ev_fields <- unlist(lifecycle$transition_evidence_fields)
fixture_files <- sort(Sys.glob(file.path(root, "schema", "fixtures", "*.yaml")))

fail <- character(0); pass <- 0L
check <- function(cond, msg) if (isTRUE(cond)) pass <<- pass + 1L else fail <<- c(fail, msg)

legal <- function(from, to) {
  allowed <- unlist(trans[[from]] %||% character(0))
  to %in% allowed
}
final_state <- function(rec) {
  ts <- rec$transitions
  if (length(ts) == 0) return(rec$state %||% NA_character_)
  ts[[length(ts)]]$to
}

check(length(fixture_files) >= 5,
      sprintf("expected >=5 transition fixtures, found %d", length(fixture_files)))

for (ff in fixture_files) {
  fx <- yaml::read_yaml(ff)
  tag <- fx$name %||% basename(ff)

  # index records by id, collect final states
  states <- list()
  for (rec in fx$records) {
    rid <- rec$id
    # (1) legality + (2) evidence for every transition
    for (tr in rec$transitions) {
      check(legal(tr$from, tr$to),
            sprintf("%s/%s: illegal transition %s -> %s", tag, rid, tr$from, tr$to))
      for (k in ev_fields) {
        check(!is.null(tr[[k]]),
              sprintf("%s/%s: transition %s->%s missing evidence field '%s'",
                      tag, rid, tr$from, tr$to, k))
      }
    }
    states[[rid]] <- final_state(rec)
  }

  # (3) propagation expectations
  exp <- fx$expect
  check(!is.null(fx$event), sprintf("%s: missing event", tag))
  check(!is.null(exp), sprintf("%s: missing expect block", tag))

  for (rid in unlist(exp$invalidated %||% character(0))) {
    check(identical(states[[rid]], "INVALIDATED"),
          sprintf("%s: expected %s INVALIDATED, got %s", tag, rid, states[[rid]] %||% "MISSING"))
  }

  # replacement records: DISCOVERED and linked to an invalidated id
  repl <- fx$replacement_records %||% list()
  repl_ids <- vapply(repl, function(r) r$id %||% "", "")
  for (rid in unlist(exp$replacement_discovered %||% character(0))) {
    check(rid %in% repl_ids, sprintf("%s: replacement %s not declared", tag, rid))
    r <- repl[[which(repl_ids == rid)[1]]]
    if (!is.null(r)) {
      check(identical(r$state, "DISCOVERED"),
            sprintf("%s: replacement %s not DISCOVERED (got %s)", tag, rid, r$state %||% "NA"))
      sup <- r$supersedes %||% ""
      check(sup %in% unlist(exp$invalidated %||% character(0)),
            sprintf("%s: replacement %s must supersede an invalidated id (got '%s')", tag, rid, sup))
    }
  }

  # downstream gate must not stay accepted
  g <- exp$downstream_gate
  if (!is.null(g)) {
    check(identical(g$result, "FAILED_OR_REOPENED"),
          sprintf("%s: downstream gate %s left in state '%s' (must be FAILED_OR_REOPENED)",
                  tag, g$id %||% "?", g$result %||% "NA"))
  }
}

cat("validate-transitions.R\n")
cat(sprintf("fixtures: %d (%s)\n", length(fixture_files),
            paste(basename(fixture_files), collapse = ", ")))
cat(sprintf("checks passed: %d\n", pass))
if (length(fail) > 0) {
  cat(sprintf("checks FAILED: %d\n", length(fail)))
  for (f in fail) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
cat("VALIDATION PASSED\n")
