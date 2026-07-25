#!/usr/bin/env Rscript

# parse_status documentation-consistency gate (rurl 3.0 protocol hardening, G4).
# The parse-status vocabulary has one authoritative definition -- the
# `.STATUS_*` constants in R/status-constants.R -- and every place that
# ENUMERATES it must agree with that definition, in both directions.
#
# This VERIFIES; it decides nothing. It is the migration-family sibling of the
# C-08 cache-doc gate: same shape, same fail-closed posture, same base-R-only
# constraint. It reads the source of truth by sourcing R/status-constants.R, so
# it needs no package build.
#
# THE CELLS IT CLOSES
#   M-8   the vocabulary is frozen at the shipped values; no token appears
#         silently. Previously only incidental coverage existed.
#   M-14  (the half that is true today) parse_status documentation stays
#         consistent with the constants. Nothing gated this before.
#
# THE RULES
#   S1 completeness -- each ENUMERATING doc site lists every constant value.
#   S2 no phantoms  -- no doc mentions a status-shaped literal that is not a
#                      real constant. Catches a rename that leaves stale docs.
#   S3 partition    -- the constants partition exactly into ok / warning / one
#                      terminal error under .is_ok_status/.is_warning_status;
#                      none unclassified, none double-classified.
#   S4 frozen count -- the vocabulary size is pinned. Adding a status is a
#                      deliberate act that must update this gate too.
#
# DELIBERATELY NOT ENFORCED (M-14's other half). P2.3 requires parse_status to
# be documented as a LOSSY COMPATIBILITY PROJECTION carrying a pointer to the
# layered verdict accessor. That framing is FALSE today -- R/accessors.R and
# R/parse.R enumerate the vocabulary and say nothing about lossiness -- and only
# becomes required at the 3.0 flip, when get_parse_verdicts() exists (carrier
# RURL-glkuulyr). Enforcing it now would paint every commit red for a reason
# unrelated to correctness. The rule is recorded here so it is not forgotten;
# it is switched on with the flip, not before.
#
# ENUMERATING vs MENTIONING. Only two doc sites enumerate the whole vocabulary
# (get_parse_status's and safe_parse_url's @return). Four other man pages
# mention a subset -- canonical_join, get_clean_url, get_scheme,
# safe_parse_urls -- and requiring THOSE to list all values would be wrong: a
# page that documents one status's effect is not a vocabulary index. S1 applies
# to enumerating sites; S2 applies everywhere.
#
# Zero dependencies beyond base R. Deterministic and network-free.
#
# Usage:
#   Rscript tools/status-doc-consistency.R             # verify, exit 1 on drift
#   Rscript tools/status-doc-consistency.R --self-test # positive/negative

ENUMERATING_RD <- c("man/get_parse_status.Rd", "man/safe_parse_url.Rd")

# ---- source of truth --------------------------------------------------------

status_env <- function(root) {
  f <- file.path(root, "R", "status-constants.R")
  if (!file.exists(f)) {
    stop(sprintf("status constants not found: %s", f), call. = FALSE)
  }
  e <- new.env(parent = baseenv())
  sys.source(f, envir = e)
  e
}

status_values <- function(e) {
  nms <- ls(e, all.names = TRUE)
  nms <- nms[grepl("^\\.STATUS_", nms)]
  vals <- vapply(nms, function(n) as.character(get(n, envir = e)), character(1))
  sort(unname(vals))
}

# ---- doc scanning -----------------------------------------------------------

# A status literal is one PRESENTED AS A LITERAL -- either quoted, or wrapped
# in \code{}. Both forms occur in the real tree (`"warning-no-tld"` in the
# @return enumerations, bare `\code{warning-no-tld}` in prose), so matching only
# one would miss real mentions.
#
# Matching the bare token instead would be wrong, and the self-test pins why:
# ordinary hyphenated English like "warning-free" starts with `warning-` and is
# not a status at all. That is the same false-positive class the C-10 prose gate
# hit, and it is fixed here at the root -- by requiring the literal's
# presentation -- rather than by a denylist of English words, which would rot.
#
# Bare "ok" and "error" are deliberately out of scope: they are ordinary words,
# and a rename would strand the hyphenated tokens, which are the ones worth
# scanning. `\code{warning-*}` (a glob, in canonical_join.Rd) is naturally
# excluded because `*` is not a letter.
STATUS_PATTERNS <- c(
  quoted = '"(?:ok|warning)-[a-z][a-z-]*"',
  coded  = "\\\\code\\{(?:ok|warning)-[a-z][a-z-]*\\}"
)

# Strip the presentation delimiters, leaving the bare token.
undelimit <- function(x) {
  x <- sub('^"', "", sub('"$', "", x))
  x <- sub("^\\\\code\\{", "", sub("\\}$", "", x))
  x
}

doc_files <- function(root) {
  c(list.files(file.path(root, "man"), pattern = "\\.Rd$", full.names = TRUE),
    list.files(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE))
}

# Every status-shaped literal in a file, with its line number.
mentions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  out <- list()
  for (i in seq_along(lines)) {
    for (pat in STATUS_PATTERNS) {
      hits <- regmatches(lines[i],
                         gregexpr(pat, lines[i], perl = TRUE))[[1]]
      for (raw in hits) {
        out[[length(out) + 1L]] <- list(file = path, line = i,
                                        token = undelimit(raw))
      }
    }
  }
  out
}

finding <- function(id, ok, detail) {
  list(list(id = id, ok = ok, detail = detail))
}

check_status_docs <- function(root = ".", expected_n = 8L) {
  findings <- list()
  e <- tryCatch(status_env(root), error = function(err) err)
  if (inherits(e, "error")) {
    return(finding("S0", FALSE, conditionMessage(e)))
  }
  vals <- status_values(e)
  hyphenated <- vals[grepl("-", vals, fixed = TRUE)]

  # ---- S1 completeness ------------------------------------------------------
  gaps <- character(0)
  for (rel in ENUMERATING_RD) {
    p <- file.path(root, rel)
    if (!file.exists(p)) {
      gaps <- c(gaps, sprintf("%s: missing", rel))
      next
    }
    txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
    absent <- vals[!vapply(vals, function(v) {
      grepl(sprintf('"%s"', v), txt, fixed = TRUE)
    }, logical(1))]
    if (length(absent)) {
      gaps <- c(gaps, sprintf("%s: does not document %s", rel,
                              toString(absent)))
    }
  }
  findings <- c(findings, finding(
    "S1", length(gaps) == 0L,
    if (length(gaps)) paste(gaps, collapse = "; ")
    else sprintf("both enumerating doc sites list all %d status values",
                 length(vals))))

  # ---- S2 no phantoms -------------------------------------------------------
  phantom <- character(0)
  for (f in doc_files(root)) {
    for (m in mentions(f)) {
      if (!m$token %in% hyphenated) {
        phantom <- c(phantom, sprintf("%s:%d '%s'",
                                      sub(paste0("^", root, "/?"), "", m$file),
                                      m$line, m$token))
      }
    }
  }
  findings <- c(findings, finding(
    "S2", length(phantom) == 0L,
    if (length(phantom))
      sprintf("documented status literal(s) with no constant: %s",
              toString(phantom))
    else "every documented status literal resolves to a constant"))

  # ---- S3 partition ---------------------------------------------------------
  broken <- character(0)
  is_ok <- get(".is_ok_status", envir = e)
  is_warn <- get(".is_warning_status", envir = e)
  terminal <- character(0)
  for (v in vals) {
    o <- isTRUE(is_ok(v))
    w <- isTRUE(is_warn(v))
    if (o && w) {
      broken <- c(broken, sprintf("'%s' is both ok and warning", v))
    }
    if (!o && !w) terminal <- c(terminal, v)
  }
  if (length(terminal) != 1L) {
    broken <- c(broken, sprintf(
      "expected exactly 1 terminal status, found %d (%s)",
      length(terminal), toString(terminal)))
  }
  findings <- c(findings, finding(
    "S3", length(broken) == 0L,
    if (length(broken)) paste(broken, collapse = "; ")
    else sprintf("partition holds: %d ok, %d warning, terminal '%s'",
                 sum(vapply(vals, function(v) isTRUE(is_ok(v)), logical(1))),
                 sum(vapply(vals, function(v) isTRUE(is_warn(v)), logical(1))),
                 terminal)))

  # ---- S4 frozen count ------------------------------------------------------
  findings <- c(findings, finding(
    "S4", length(vals) == expected_n,
    if (length(vals) != expected_n)
      sprintf(paste("vocabulary size %d, gate pinned at %d -- a status was",
                    "added or removed; update this gate deliberately (M-8)"),
              length(vals), expected_n)
    else sprintf("vocabulary frozen at %d values", expected_n)))

  findings
}

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

# ---- self-test --------------------------------------------------------------

self_test <- function() {
  st <- new.env(parent = emptyenv())
  st$pass <- 0L
  st$fail <- character(0)
  expect <- function(label, cond) {
    if (isTRUE(cond)) st$pass <- st$pass + 1L else st$fail <- c(st$fail, label)
  }

  consts <- c(
    '.STATUS_OK <- "ok"',
    '.STATUS_OK_FTP <- "ok-ftp"',
    '.STATUS_ERROR <- "error"',
    '.STATUS_WARN_NO_TLD <- "warning-no-tld"',
    '.is_ok_status <- function(s) s %in% c("ok", "ok-ftp")',
    '.is_warning_status <- function(s) startsWith(s, "warning-")'
  )
  enum <- '\\value{one of "ok", "ok-ftp", "warning-no-tld", "error"}'

  mk <- function(constants = consts, rd = enum, extra_rd = NULL,
                 extra_r = NULL) {
    root <- tempfile("statusdoc-")
    dir.create(file.path(root, "man"), recursive = TRUE)
    dir.create(file.path(root, "R"), recursive = TRUE)
    writeLines(constants, file.path(root, "R", "status-constants.R"))
    for (f in c("get_parse_status.Rd", "safe_parse_url.Rd")) {
      writeLines(rd, file.path(root, "man", f))
    }
    if (!is.null(extra_rd)) {
      writeLines(extra_rd, file.path(root, "man", "other.Rd"))
    }
    if (!is.null(extra_r)) {
      writeLines(extra_r, file.path(root, "R", "other.R"))
    }
    root
  }
  rule <- function(root, id, n = 4L) {
    for (f in check_status_docs(root, expected_n = n)) {
      if (identical(f$id, id)) return(isTRUE(f$ok))
    }
    NA
  }
  verdict <- function(root, n = 4L) {
    all(vapply(check_status_docs(root, expected_n = n),
               function(f) isTRUE(f$ok), logical(1)))
  }

  # 1. Clean positive.
  expect("positive: consistent tree passes", verdict(mk()))

  # 2. S1 -- an enumerating site omits a value.
  r <- mk(rd = '\\value{one of "ok", "ok-ftp", "error"}')
  expect("S1 fails when an enumerating site omits a status",
         identical(rule(r, "S1"), FALSE))

  # 3. S1 -- a missing enumerating file is a failure, not a silent pass.
  root <- mk()
  file.remove(file.path(root, "man", "safe_parse_url.Rd"))
  expect("S1 fails on a missing enumerating doc",
         identical(rule(root, "S1"), FALSE))

  # 4. S2 -- a phantom status in an Rd (the stale-rename case).
  r <- mk(extra_rd = 'see \\code{"warning-deprecated-thing"} for details')
  expect("S2 fails on a phantom status in Rd",
         identical(rule(r, "S2"), FALSE))

  # 5. S2 -- a phantom status in roxygen.
  r <- mk(extra_r = "#' status is \"ok-legacy\" in this case")
  expect("S2 fails on a phantom status in roxygen",
         identical(rule(r, "S2"), FALSE))

  # 6. S2 -- a real status mentioned in a non-enumerating page is fine.
  r <- mk(extra_rd = 'rows with \\code{"warning-no-tld"} are excluded')
  expect("S2 passes on a real status in a mentioning page",
         identical(rule(r, "S2"), TRUE))

  # 7. S2 -- ordinary hyphenated English must NOT be flagged. This is the
  #    false-positive class the C-10 gate hit; the pattern is anchored to the
  #    ok-/warning- prefixes precisely to avoid it.
  r <- mk(extra_rd = "the check is warning-free and the result is ok here")
  expect("S2 does not flag 'warning-free' prose",
         identical(rule(r, "S2"), TRUE))

  # 7b. S2 -- the BARE \code{} form is a real literal and must be scanned. The
  #     tree uses it (get_parse_status.Rd:81), so a quoted-only pattern would
  #     silently miss a stale rename living in exactly that form.
  r <- mk(extra_rd = "Warning statuses such as \\code{warning-phantom} apply")
  expect("S2 catches a phantom in the bare \\code{} form",
         identical(rule(r, "S2"), FALSE))

  # 7c. S2 -- and the same form with a REAL status passes.
  r <- mk(extra_rd = "Warning statuses such as \\code{warning-no-tld} apply")
  expect("S2 passes a real status in the bare \\code{} form",
         identical(rule(r, "S2"), TRUE))

  # 7d. S2 -- a glob, not a literal (canonical_join.Rd:68 really contains this).
  r <- mk(extra_rd = "all \\code{warning-*} statuses are joinable")
  expect("S2 does not flag the \\code{warning-*} glob",
         identical(rule(r, "S2"), TRUE))

  # 8. S3 -- a status classified as both ok and warning.
  bad <- c(consts, NULL)
  bad[5] <- paste('.is_ok_status <- function(s) s %in%',
                  'c("ok", "ok-ftp", "warning-no-tld")')
  r <- mk(constants = bad)
  expect("S3 fails on a double-classified status",
         identical(rule(r, "S3"), FALSE))

  # 9. S3 -- two terminal statuses (none matching either predicate).
  bad2 <- c(consts, '.STATUS_ODD <- "mystery"')
  r <- mk(constants = bad2,
          rd = '\\value{"ok", "ok-ftp", "warning-no-tld", "error", "mystery"}')
  expect("S3 fails when more than one status is terminal",
         identical(rule(r, "S3", n = 5L), FALSE))

  # 10. S4 -- vocabulary grew.
  grown <- c(consts, '.STATUS_NEW <- "warning-new"')
  r <- mk(constants = grown,
          rd = paste('\\value{"ok", "ok-ftp", "warning-no-tld",',
                     '"error", "warning-new"}'))
  expect("S4 fails when the vocabulary grows", identical(rule(r, "S4"), FALSE))

  # 11. S4 -- and the grown tree is otherwise consistent, proving S4 is what
  #     caught it rather than incidental S1/S2 drift.
  expect("S4 is the only rule that catches a clean addition",
         identical(rule(r, "S1"), TRUE) && identical(rule(r, "S2"), TRUE))

  # 12. Missing constants file fails closed.
  root <- tempfile("statusdoc-empty-")
  dir.create(root)
  expect("missing constants file fails closed", identical(verdict(root), FALSE))

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("status-doc-consistency self-test: FAIL", call. = FALSE)
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
  ok <- print_findings(check_status_docs("."))
  if (!ok) {
    stop("status-doc-consistency gate: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
