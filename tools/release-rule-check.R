#!/usr/bin/env Rscript

# C-10 release-rule gate (rurl 3.0 protocol hardening, G4.3; verifies the
# ACCEPTED decision P0.4). This is the executable form of the owner-approved
# release rule RCON-10 requires: the "no CRAN until curl-free" hold is bound to
# the v3 (3.0.0) curl-free line and does NOT reach back onto the v2.x
# curl-bearing maintenance line, whose releasability is governed by its own
# determinism gate and a separate owner release act.
#
# This VERIFIES; it decides nothing. Every rule traces to P0.4; nothing here
# names a release line, moves a boundary, or lifts a hold.
#
# SCOPE BOUNDARY (deliberate; owner decision 2026-07-25). The curl-free closure
# criterion itself -- zero `curl` reference on the v3 line -- is FALSE today
# (DESCRIPTION declares Version 2.7.0 with `Imports: curl`, the held v2.x
# release). This gate therefore does NOT assert "3.x cannot ship with curl in
# Imports"; that criterion is carried whole by its own G4 leaf, the curl
# zero-reference closure gate (RURL-cunfohwy). Half-building the curl gate here
# and half there would leave neither leaf able to state its own completion.
# What IS executable today is the release-LINE rule, and that is what this
# checks -- including P0.4's complementary requirement that no normative
# language broaden the hold onto v2.x or permit a curl-bearing v3.
#
# A NOTE ON READING RECORDS. Record STATE is read from YAML frontmatter, never
# from body prose. Most ACCEPTED v3 records still head their body
# `## Decision (PROPOSED)`, and P0.4's own ratification block says "state
# remains PROPOSED": that is proposal-time trace, frozen by design (P0.1 §Seal
# phase 3 enumerates frontmatter only; P0.1 §6 forbids in-place substance
# edits). Frontmatter plus the lifecycle log govern. A checker that read body
# prose would report every ACCEPTED record as unratified.
#
# Zero dependencies beyond base R. Deterministic and network-free.
#
# Usage:
#   Rscript tools/release-rule-check.R             # verify, exit 1 on violation
#   Rscript tools/release-rule-check.R --self-test # positive/negative fixtures

# ---- record reading ---------------------------------------------------------

# Minimal frontmatter reader: the leading `---` block, flat `key: value` pairs
# only. Enough for the fields this gate reads (state/id/affects), and
# deliberately not a general YAML parser.
read_frontmatter <- function(path) {
  if (!file.exists(path)) {
    stop("record not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  if (length(lines) < 2L || !grepl("^---\\s*$", lines[1])) {
    stop("record has no frontmatter block: ", path, call. = FALSE)
  }
  close_at <- grep("^---\\s*$", lines)[2]
  if (is.na(close_at)) {
    stop("record frontmatter is unterminated: ", path, call. = FALSE)
  }
  body <- lines[2:(close_at - 1L)]
  kv <- list()
  for (ln in body) {
    m <- regmatches(ln, regexec("^([A-Za-z_][A-Za-z0-9_]*):\\s*(.*)$", ln))[[1]]
    if (length(m) == 3L) {
      kv[[m[2]]] <- trimws(m[3])
    }
  }
  kv
}

# Split a bracketed YAML flow sequence (`[A, B]`) into its members.
yaml_flow_list <- function(v) {
  if (is.null(v) || !nzchar(v)) {
    return(character(0))
  }
  inner <- sub("^\\[", "", sub("\\]$", "", trimws(v)))
  if (!nzchar(inner)) {
    return(character(0))
  }
  trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
}

# ---- prose predicates -------------------------------------------------------

# Sentence splitter for normative prose. Markdown hard-wraps at ~80 columns, so
# splitting on newlines would shatter almost every sentence into fragments and
# strip the very scope token ("...bound specifically to the v3 line") that the
# next line carries -- reporting scoped prose as unscoped. Lines are therefore
# rejoined into blank-line-delimited paragraphs first, and only then split on
# sentence punctuation. A table row is its own unit, so one register row does
# not merge with its neighbours.
sentences <- function(text) {
  split_para <- function(p) {
    if (length(p) == 0L) {
      return(character(0))
    }
    unlist(strsplit(paste(p, collapse = " "), "(?<=[.;:])\\s+", perl = TRUE))
  }
  lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
  out <- character(0)
  para <- character(0)
  for (tl in lines) {
    if (grepl("^\\|", tl)) {
      out <- c(out, split_para(para), tl)   # a table row is its own unit
      para <- character(0)
    } else if (nzchar(tl)) {
      para <- c(para, tl)
    } else {
      out <- c(out, split_para(para))
      para <- character(0)
    }
  }
  out <- trimws(c(out, split_para(para)))
  out[nzchar(out)]
}

# Strip quoted spans ("..." and `...`). A record that QUOTES the unscoped
# protocol sentence is citing the defect, not asserting it -- and P0.4 requires
# that sentence be annotated rather than deleted, so the quotations are
# mandatory. The check is about what the corpus ASSERTS, not what it cites.
strip_quotations <- function(s) {
  s <- gsub("“[^”]*”", " ", s)   # curly double quotes
  s <- gsub("\"[^\"]*\"", " ", s)
  gsub("`[^`]*`", " ", s)
}

# Does this sentence ASSERT the CRAN hold (rather than merely mention CRAN, or
# name it as a topic)? A prohibition construction is required: the bare noun
# phrase "CRAN hold scope; exact graduation criteria" is a heading-like topic
# label in a list of open decisions, and asserts nothing.
asserts_cran_hold <- function(s) {
  t <- tolower(strip_quotations(s))
  grepl("cran", t, fixed = TRUE) &&
    grepl(paste0("prohibit|forbid|\\bno release\\b|submit no|not submit|",
                 "until curl-free|until the curl-free|until it is curl-free|",
                 "no cran until|hold until"), t)
}

# Does the sentence REPORT a defect about the hold rather than assert it? The
# reconciliation's RCON-10 finding reads "The stated CRAN prohibition also
# conflicts with the latest accepted release disposition unless its target
# version is clarified" -- that is the very gap P0.4 closes, stated as a
# finding. Describing an under-scoped prohibition is not imposing one.
#
# Deliberately narrow: a sentence that reports a conflict AND ALSO imposes the
# hold ("...conflicts with v2.7, so no package must be submitted on any line")
# is still a violation, because it broadens the hold in its own voice.
reports_defect_about_hold <- function(s) {
  t <- tolower(s)
  reports <- grepl(paste0("conflicts?\\b|contradict|not executable|",
                          "unless [^.]{0,60}(clarified|named|scoped)|",
                          "required (amendment|disposition|correction)|",
                          "ambiguit|unresolved"), t)
  imposes <- grepl(paste0("\\bmust\\b|\\bshall\\b|\\bwill not\\b|",
                          "is prohibited|is forbidden|\\bdo not\\b|",
                          "\\bnever\\b"), t)
  reports && !imposes
}

# Is the hold statement bound to the line it governs? C-10's required
# disposition is literally "name the release line governed by the hold", so a
# statement is scoped iff it names A line. Naming the v2.x line counts: prose
# about lifting the v2.7 hold is a statement about the OTHER line's own gate,
# not an unscoped freeze. Unscoped means naming NEITHER line -- that is the
# C-10 defect, a sentence that silently governs everything.
hold_is_scoped <- function(s) {
  t <- tolower(s)
  grepl(paste0("v3|3\\.0\\.0|3\\.0 |curl-free line|curl-free 3\\.0|version 3|",
               "v2\\.|2\\.7|2\\.x|maintenance line"), t)
}

# Negation markers. P0.4 legitimately discusses what must NOT happen ("nor
# silently narrow it (letting a v3 release ship while curl is still present)"),
# so a permission scan that ignored negation would flag the record that
# FORBIDS the thing.
is_negated <- function(s) {
  t <- tolower(s)
  grepl(paste0("\\bnot\\b|\\bnor\\b|\\bnever\\b|\\bno\\b|without|",
               "cannot|can't|prohibit|forbid|prevent|refus"), t)
}

# Does this sentence PERMIT a curl-bearing RELEASE on the v3 line? That would
# silently narrow the hold to nothing. A release verb is required: "curl-shim
# architecture may be mistaken for immutable v3 design" mentions v3, curl and
# "may", but permits no release and must not be flagged.
permits_curl_bearing_v3 <- function(s) {
  # Quoted text is cited, not asserted -- same rule as the hold scan.
  t <- tolower(strip_quotations(s))
  # A question poses the fork, it does not settle it: S8's "Is v2.7 eligible
  # now...?" is the open design fork C-10 exists to close, not a permission.
  if (grepl("?", t, fixed = TRUE)) {
    return(FALSE)
  }
  mentions_v3 <- grepl("v3|3\\.0\\.0|version 3", t)
  mentions_curl <- grepl("curl", t, fixed = TRUE)
  # `\\ballow` alone matched "allowlisted" in the curl-removal gate's own
  # description -- a sentence REQUIRING the gate, flagged as permitting it.
  permits <- grepl(paste0("\\bmay\\b|\\bcan\\b|\\ballow(s|ed|ing)?\\b|",
                          "\\bpermit(s|ted|ting)?\\b|",
                          "eligible|is free to|okay to|fine to"), t)
  releases <- grepl("ship|releas|submit|publish|cran", t)
  mentions_v3 && mentions_curl && permits && releases && !is_negated(s)
}

# ---- the checks -------------------------------------------------------------

# The tracked NORMATIVE corpus: the records that bind. `evidence/` (imported
# review inputs such as S8, plus tracker snapshots) and `worklog/` (working
# notes) are historical or advisory, not this project's assertions -- S8 states
# the open fork C-10 exists to close, so scanning it would report the QUESTION
# as an answer. `schema/` and `tools/` carry no release prose.
#
# The reconstruction protocol itself lives in gitignored `_scratch/` and is
# therefore not checkable at all -- named as an explicit boundary in the release
# slice rather than silently skipped.
normative_corpus <- function(root) {
  base <- file.path(root, "design", "work", "url-v3")
  # `verification/` is excluded on purpose. A verification slice documents the
  # forms its gate REJECTS ("R5 fails on a sentence permitting a curl-bearing
  # v3 release"), so scanning it would make this gate trip over its own
  # specification -- and every future slice would need a bespoke exemption.
  # Verification records verify; by their own single-writer envelopes they
  # never decide release policy, so a release rule asserted there would be
  # ultra vires regardless. Release policy lives in the records below.
  dirs <- file.path(base, c("decisions", "registers", "contracts", "gates"))
  files <- unlist(lapply(dirs[dir.exists(dirs)], dir,
                         pattern = "\\.md$", recursive = TRUE,
                         full.names = TRUE))
  recon <- file.path(base, "protocol-review-reconciliation.md")
  c(files, recon[file.exists(recon)])
}

finding <- function(id, ok, detail) {
  list(list(id = id, ok = ok, detail = detail))
}

check_release_rule <- function(root = ".") {
  findings <- list()

  # R1 -- P0.4 is ACCEPTED and claims C-10 + G4.3. Frontmatter only.
  p04 <- file.path(root, "design", "work", "url-v3", "decisions",
                   "P0.4-cran-release-boundary.md")
  fm <- tryCatch(read_frontmatter(p04), error = function(e) e)
  findings <- c(findings, if (inherits(fm, "error")) {
    finding("R1", FALSE, conditionMessage(fm))
  } else {
    affects <- yaml_flow_list(fm$affects)
    evidence_ok <- !is.null(fm$accepted_evidence) &&
      nzchar(fm$accepted_evidence) &&
      !identical(fm$accepted_evidence, "pending")
    finding("R1",
            identical(fm$state, "ACCEPTED") &&
              all(c("C-10", "G4.3") %in% affects) && evidence_ok,
            sprintf("P0.4 state=%s affects=[%s] accepted_evidence=%s",
                    fm$state %||% "<none>", toString(affects),
                    fm$accepted_evidence %||% "<none>"))
  })

  # R2 -- the contradiction register's C-10 row is ACCEPTED and cites P0.4.
  reg <- file.path(root, "design", "work", "url-v3", "registers",
                   "contradictions.md")
  reg_rows <- if (file.exists(reg)) {
    grep("^\\s*\\|\\s*C-10\\s*\\|", readLines(reg, warn = FALSE), value = TRUE)
  } else {
    NULL
  }
  findings <- c(findings, if (is.null(reg_rows)) {
    finding("R2", FALSE, "contradiction register not found")
  } else if (length(reg_rows) == 1L) {
    finding("R2",
            grepl("P0.4", reg_rows[1], fixed = TRUE) &&
              grepl("ACCEPTED", reg_rows[1], fixed = TRUE),
            "C-10 row cites P0.4 with an ACCEPTED disposition")
  } else {
    finding("R2", FALSE, sprintf("expected exactly 1 C-10 row, found %d",
                                 length(reg_rows)))
  })

  # R3 -- both release lines are named, each with its governing condition.
  findings <- c(findings, if (is.null(reg_rows)) {
    finding("R3", FALSE, "contradiction register not found")
  } else {
    r <- paste(reg_rows, collapse = " ")
    finding("R3", grepl("v3|3\\.0\\.0", r) && grepl("v2\\.|2\\.7", r),
            "the C-10 disposition names BOTH the v3 line and the v2.x line")
  })

  # R4/R5 -- the corpus scan. No normative record may state the hold unscoped
  # (widening it onto v2.x), nor permit a curl-bearing v3 (narrowing it away).
  unscoped <- character(0)
  permissive <- character(0)
  for (f in normative_corpus(root)) {
    for (s in sentences(paste(readLines(f, warn = FALSE), collapse = "\n"))) {
      if (asserts_cran_hold(s) && !hold_is_scoped(s) &&
            !reports_defect_about_hold(s)) {
        unscoped <- c(unscoped, sprintf("%s :: %s", basename(f), s))
      }
      if (permits_curl_bearing_v3(s)) {
        permissive <- c(permissive, sprintf("%s :: %s", basename(f), s))
      }
    }
  }
  findings <- c(findings, finding("R4", length(unscoped) == 0L,
    if (length(unscoped) == 0L) {
      "no unscoped CRAN-hold statement (hold never widens onto v2.x)"
    } else {
      paste(length(unscoped), "unscoped hold statement(s):\n    ",
            paste(unscoped, collapse = "\n    "))
    }))
  findings <- c(findings, finding("R5", length(permissive) == 0L,
    if (length(permissive) == 0L) {
      "no statement permits a curl-bearing v3 release"
    } else {
      paste(length(permissive), "permissive statement(s):\n    ",
            paste(permissive, collapse = "\n    "))
    }))

  # R6 -- the deferred criteria name their carriers, so neither is a
  # promissory note pointing at nothing.
  slice <- file.path(root, "design", "work", "url-v3", "verification",
                     "release-slice.md")
  findings <- c(findings, if (file.exists(slice)) {
    txt <- paste(readLines(slice, warn = FALSE), collapse = "\n")
    finding("R6",
            grepl("RURL-cunfohwy", txt, fixed = TRUE) &&
              grepl("RURL-gxqdmpcp", txt, fixed = TRUE),
            "release slice names the curl-free + v2.x release carriers")
  } else {
    finding("R6", FALSE, "release slice not found")
  })

  findings
}

`%||%` <- function(a, b) if (is.null(a)) b else a

print_findings <- function(findings) {
  cat("== rurl 3.0 C-10 release-rule gate (P0.4 / G4.3) ==\n")
  for (f in findings) {
    cat(sprintf("  [%s] %s -- %s\n", if (f$ok) "PASS" else "FAIL",
                f$id, f$detail))
  }
  bad <- Filter(function(f) !f$ok, findings)
  cat("VERDICT:", if (length(bad) == 0L) "PASS" else "FAIL", "\n")
  invisible(length(bad) == 0L)
}

# ---- self-test (positive + negative fixtures) -------------------------------

self_test <- function() {
  fail <- function(m) stop("self-test FAILED: ", m, call. = FALSE)

  # (1) an unscoped hold statement is caught -- this is the C-10 defect, a
  # sentence that freezes every line rather than the v3 line.
  bad <- "The project will submit no package to CRAN until it is curl-free."
  if (!asserts_cran_hold(bad)) fail("did not recognize a CRAN-hold assertion")
  if (hold_is_scoped(bad)) fail("unscoped hold read as scoped")

  # (2) the same claim, correctly scoped, passes.
  good <- paste("Submit no package to CRAN on the v3 line until the",
                "curl-free 3.0 slice lands.")
  if (!asserts_cran_hold(good)) fail("scoped hold not recognized as a hold")
  if (!hold_is_scoped(good)) fail("scoped hold read as unscoped")

  # (3) a sentence merely MENTIONING CRAN is not a hold assertion.
  mention <- "The CRAN submission checklist lives in the release runbook."
  if (asserts_cran_hold(mention)) fail("bare CRAN mention read as a hold")

  # (4) permission for a curl-bearing v3 is caught.
  perm <- "A v3 release may ship while curl is still in Imports."
  if (!permits_curl_bearing_v3(perm)) fail("permissive v3+curl not caught")

  # (5) NEGATION is respected: P0.4's own prose forbids exactly that, and must
  # not be flagged as permitting it.
  neg <- paste("back-compat must not silently narrow it (letting a v3 release",
               "ship while curl is still present)")
  if (permits_curl_bearing_v3(neg)) {
    fail("a sentence FORBIDDING curl-bearing v3 was flagged as permitting it")
  }

  # (6) frontmatter state is read from frontmatter, NOT body prose -- an
  # ACCEPTED record whose body still says PROPOSED must read as ACCEPTED.
  tf <- tempfile(fileext = ".md")
  writeLines(c("---", "id: X", "state: ACCEPTED", "affects: [C-10, G4.3]",
               "accepted_evidence: abc123", "---", "",
               "## Decision (PROPOSED)", "state remains PROPOSED"), tf)
  fm <- read_frontmatter(tf)
  if (!identical(fm$state, "ACCEPTED")) {
    fail("frontmatter state was not read as ACCEPTED (body prose leaked in)")
  }
  if (!identical(yaml_flow_list(fm$affects), c("C-10", "G4.3"))) {
    fail("affects flow-list not parsed")
  }

  # (7) a record with no frontmatter is an error, not a silent pass.
  tf2 <- tempfile(fileext = ".md")
  writeLines(c("# just a heading", "no frontmatter here"), tf2)
  if (!inherits(tryCatch(read_frontmatter(tf2), error = function(e) e),
                "error")) {
    fail("record without frontmatter did not error")
  }

  # (8) a QUOTED unscoped hold is a citation, not an assertion -- P0.4 requires
  # the protocol's bare sentence be annotated rather than deleted, so records
  # must be free to quote it. The same claim in the record's own voice is not.
  quoted <- paste("The protocol says \"submit no package to CRAN until",
                  "curl-free\", which this record scopes.")
  if (asserts_cran_hold(quoted)) fail("a quoted hold was read as asserted")
  if (!asserts_cran_hold("We submit no package to CRAN until curl-free.")) {
    fail("an unquoted hold assertion was missed")
  }

  # (9) a sentence REPORTING the defect is exempt; one that reports AND imposes
  # in its own voice is still a violation.
  rep <- paste("The stated CRAN prohibition also conflicts with the latest",
               "accepted release disposition unless its target version is",
               "clarified.")
  if (!reports_defect_about_hold(rep)) fail("finding statement not exempted")
  impose <- paste("That prohibition conflicts with v2.7, so no package must",
                  "be submitted to CRAN until curl-free on any line.")
  if (reports_defect_about_hold(impose)) {
    fail("a sentence that IMPOSES the hold was exempted as a mere finding")
  }

  # (10) "allowlisted" must not read as a permission -- the curl-removal gate's
  # own description says "allowlisted static scan" while REQUIRING the gate.
  allowlisted <- paste("Before any v3 CRAN submission it must pass the",
                       "curl-removal closure gate (allowlisted static scan).")
  if (permits_curl_bearing_v3(allowlisted)) {
    fail("'allowlisted' was read as permitting a curl-bearing v3 release")
  }

  # (11) a QUOTED permission example -- the shape a verification slice's own
  # fixture table contains -- is a citation, not an assertion.
  quoted_perm <-
    '| 4 | "a v3 release may ship while curl is in Imports" | caught |'
  if (permits_curl_bearing_v3(quoted_perm)) {
    fail("a quoted permission example was read as a permission")
  }

  # (12) end-to-end against the real tree: the shipped corpus must PASS.
  fs <- check_release_rule(".")
  if (length(fs) != 6L) fail("expected 6 checks")
  bad_ids <- vapply(Filter(function(f) !f$ok, fs), function(f) f$id,
                    character(1))
  if (length(bad_ids) > 0L) {
    fail(paste("live tree fails:", toString(bad_ids)))
  }

  cat("release-rule gate self-test: PASS (12 fixtures)\n")
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }
  ok <- print_findings(check_release_rule("."))
  if (!ok) {
    stop("release-rule gate: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
