#!/usr/bin/env Rscript

# Diagnostics-vocabulary documentation-consistency gate (epic RURL-rnobeauh,
# RURL-vwkjtoer option 3).
#
# The diagnostics vocabulary has one authoritative definition -- the
# `.URL_DIAGNOSTICS` registry in R/diagnostics.R -- and one authoritative
# ENUMERATION of it in the user-facing docs: the "Diagnostic vocabulary
# (canonical)" section of `get_url_diagnostics()`. This gate holds those two to
# each other in both directions, and forbids a diagnostic-shaped literal
# anywhere in the docs that no longer resolves to a real token.
#
# WHY IT EXISTS. Twice now a conformance change added or renamed a token at one
# seam without sweeping the prose that describes it, and nothing caught it: the
# only "docs" the vocabulary ever pointed at was `design/prd/
# url-standard-selector.md` section 7, a HISTORICAL accepted spec that stopped
# tracking the code many releases ago (ADR 0008 -- a graduated PRD is a record,
# not a registry). Renaming the test that claimed to check it would have been
# honest but would have left the class open. This gate closes the class: the
# canonical enumeration now lives in a doc site that ships with the package,
# and it cannot drift from the registry silently.
#
# THE RULES
#   D1 completeness -- the canonical section documents every registry token.
#   D2 no extras    -- the canonical section documents nothing that is not a
#                      registry token (catches a rename leaving a stale row).
#   D3 no phantoms  -- no R/ or man/ file mentions a diagnostic-shaped literal
#                      from a REAL token's prefix family that is not itself a
#                      real token. Family-anchored on purpose: the tokens have
#                      no single shared prefix, so an unanchored kebab-case
#                      scan would flag every unrelated vocabulary in the tree.
#                      A rename within a family is the realistic drift, and
#                      that is exactly what this catches.
#   D4 frozen count -- the vocabulary size is pinned. Adding a token is a
#                      deliberate act that must update this gate too.
#
# This VERIFIES; it decides nothing. Sibling of the C-08 cache-doc gate and the
# M-8/M-14 status-doc gate: same shape, same fail-closed posture, base R only,
# deterministic and network-free. It reads the source of truth by sourcing
# R/diagnostics.R, so it needs no package build.
#
# Usage:
#   Rscript tools/diagnostics-doc-consistency.R             # verify, exit 1
#   Rscript tools/diagnostics-doc-consistency.R --self-test # positive/negative

CANONICAL_RD <- "man/get_url_diagnostics.Rd"
CANONICAL_SECTION <- "Diagnostic vocabulary (canonical)"

# ---- source of truth --------------------------------------------------------

diagnostics_env <- function(root) {
  f <- file.path(root, "R", "diagnostics.R")
  if (!file.exists(f)) {
    stop(sprintf("diagnostics registry not found: %s", f), call. = FALSE)
  }
  # R/diagnostics.R defines functions that reference package internals, but
  # sourcing only evaluates the definitions, so a bare-baseenv parent is enough
  # to reach the vector literal we need.
  e <- new.env(parent = baseenv())
  sys.source(f, envir = e)
  e
}

registry_tokens <- function(e) {
  if (!exists(".URL_DIAGNOSTICS", envir = e, inherits = FALSE)) {
    stop("R/diagnostics.R defines no .URL_DIAGNOSTICS", call. = FALSE)
  }
  sort(as.character(get(".URL_DIAGNOSTICS", envir = e)))
}

# Values of the package's OTHER enumerated vocabularies. A hyphenated literal
# can legitimately belong to one of them and still land in a diagnostic family:
# the layered-verdict state `invalid-input` (P2.3 §2) shares the `invalid-`
# family with `invalid-URL-unit`. Those are not phantom diagnostics, and a
# curated denylist would rot -- so the exemption is READ FROM THE SAME SOURCE
# the vocabularies are defined in, exactly as the token registry is. The
# `non-special` precedent is handled instead by the family-size rule below,
# which is why that vocabulary needs no entry here.
SIBLING_VOCABULARY_FILES <- c("verdicts.R", "parse-state.R")

sibling_vocabulary <- function(root) {
  out <- character(0)
  for (nm in SIBLING_VOCABULARY_FILES) {
    f <- file.path(root, "R", nm)
    if (!file.exists(f)) next
    e <- new.env(parent = baseenv())
    # Definitions only; nothing here is called, so package internals the
    # function bodies reference are never reached (same trick as above).
    try(sys.source(f, envir = e), silent = TRUE)
    for (obj in ls(e, all.names = TRUE)) {
      val <- get(obj, envir = e)
      if (is.character(val) && length(val) > 0L) {
        out <- c(out, val)
      }
    }
  }
  unique(out)
}

# ---- the canonical section --------------------------------------------------

# Extract one `\section{<title>}{ ... }` body by brace matching. Rd nests braces
# heavily (\code{}, \itemize{}), so a regex cannot find the closing brace.
rd_section <- function(path, title) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  head <- sprintf("\\section{%s}{", title)
  at <- regexpr(head, txt, fixed = TRUE)
  if (at < 0L) {
    return(NULL)
  }
  start <- at + attr(at, "match.length")
  chars <- strsplit(substring(txt, start), "", fixed = TRUE)[[1]]
  depth <- 1L
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (ch != "{" && ch != "}") next
    # An escaped brace (\{ or \}) is a literal, not nesting -- but only an ODD
    # run of preceding backslashes escapes it. `\code{\\}` really does close,
    # and treating its `}` as escaped is what a naive one-char lookback gets
    # wrong (the tree contains exactly that, for `invalid-reverse-solidus`).
    back <- 0L
    j <- i - 1L
    while (j >= 1L && chars[j] == "\\") {
      back <- back + 1L
      j <- j - 1L
    }
    if (back %% 2L == 1L) next
    if (ch == "{") depth <- depth + 1L
    if (ch == "}") {
      depth <- depth - 1L
      if (depth == 0L) {
        return(paste(chars[seq_len(i - 1L)], collapse = ""))
      }
    }
  }
  NULL
}

# A diagnostic literal is one PRESENTED AS A LITERAL: `\code{token}` in Rd, or
# a quoted string in roxygen/R. Requiring the presentation is what keeps
# ordinary hyphenated English out of the scan (the same false-positive class
# the status-doc gate is anchored against).
TOKEN_RE <- "[a-zA-Z][a-zA-Z0-9]*(?:-[a-zA-Z0-9]+)+"

DOC_PATTERNS <- c(
  coded  = sprintf("\\\\code\\{(%s)\\}", TOKEN_RE),
  quoted = sprintf('"(%s)"', TOKEN_RE)
)

extract_tokens <- function(txt) {
  out <- character(0)
  for (pat in DOC_PATTERNS) {
    m <- gregexpr(pat, txt, perl = TRUE)
    hits <- regmatches(txt, m)[[1]]
    hits <- sub('^"', "", sub('"$', "", hits))
    hits <- sub("^\\\\code\\{", "", sub("\\}$", "", hits))
    out <- c(out, hits)
  }
  unique(out)
}

# The first hyphen-separated segment, lowercased: the "family" a token belongs
# to (`ipv4`, `domain`, `file`, `ws`, `invalid`, ...).
family <- function(x) tolower(sub("-.*$", "", x))

# A first segment is a NAMESPACE only when the vocabulary actually uses it as
# one -- two or more tokens share it. That is what makes it safe to treat any
# unknown token in the family as drift.
#
# The rule is self-maintaining, and it is the reason `non-` is not scanned:
# `non-default-port` is the only token in it, while `non-special` is a real
# `scheme_class` value living in the same tree. A curated family list would
# have had to grow a denylist entry for that; this derives the answer instead.
# The singleton families (`explicit-`, `control-`, `leading-`, `host-`,
# `unicode-`, `transform-`, `mailto-`, `tel-`, `data-`) are covered by D1/D2/D4
# rather than D3 -- a rename there still cannot pass the gate.
token_families <- function(tokens) {
  f <- family(tokens)
  names(which(table(f) >= 2L))
}

doc_files <- function(root) {
  c(list.files(file.path(root, "man"), pattern = "\\.Rd$", full.names = TRUE),
    list.files(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE))
}

finding <- function(id, ok, detail) {
  list(list(id = id, ok = ok, detail = detail))
}

check_diagnostics_docs <- function(root = ".", expected_n = 32L) {
  e <- tryCatch(diagnostics_env(root), error = function(err) err)
  if (inherits(e, "error")) {
    return(finding("D0", FALSE, conditionMessage(e)))
  }
  tokens <- tryCatch(registry_tokens(e), error = function(err) err)
  if (inherits(tokens, "error")) {
    return(finding("D0", FALSE, conditionMessage(tokens)))
  }
  families <- token_families(tokens)
  findings <- list()

  rd <- file.path(root, CANONICAL_RD)
  section <- if (file.exists(rd)) rd_section(rd, CANONICAL_SECTION) else NULL
  documented <- if (is.null(section)) character(0) else extract_tokens(section)

  # ---- D1 completeness ------------------------------------------------------
  if (is.null(section)) {
    findings <- c(findings, finding(
      "D1", FALSE,
      sprintf("%s has no '%s' section", CANONICAL_RD, CANONICAL_SECTION)))
  } else {
    missing <- setdiff(tokens, documented)
    findings <- c(findings, finding(
      "D1", length(missing) == 0L,
      if (length(missing))
        sprintf("the canonical section does not document %s", toString(missing))
      else sprintf("the canonical section documents all %d tokens",
                   length(tokens))))
  }

  # ---- D2 no extras ---------------------------------------------------------
  # Restricted to first segments the vocabulary uses at all, so ordinary
  # hyphenated prose inside the section (`url_standard`, `scheme_acceptance`)
  # is not mistaken for a token. The net is WIDER than D3's: this rule is
  # confined to the canonical section, where a literal in any token family
  # really is claiming to be a token, so singleton families are in scope here.
  all_families <- unique(family(tokens))
  extra <- setdiff(documented[family(documented) %in% all_families], tokens)
  findings <- c(findings, finding(
    "D2", length(extra) == 0L,
    if (length(extra))
      sprintf("the canonical section documents non-token(s): %s",
              toString(extra))
    else "the canonical section documents no token that is not in the registry"))

  # ---- D3 no phantoms -------------------------------------------------------
  siblings <- sibling_vocabulary(root)
  phantom <- character(0)
  for (f in doc_files(root)) {
    lines <- readLines(f, warn = FALSE)
    for (i in seq_along(lines)) {
      for (tok in extract_tokens(lines[i])) {
        if (family(tok) %in% families && !tok %in% tokens &&
              !tok %in% siblings) {
          phantom <- c(phantom, sprintf("%s:%d '%s'",
                                        sub(paste0("^", root, "/?"), "", f),
                                        i, tok))
        }
      }
    }
  }
  findings <- c(findings, finding(
    "D3", length(phantom) == 0L,
    if (length(phantom))
      sprintf("documented diagnostic literal(s) with no registry token: %s",
              toString(phantom))
    else "every documented diagnostic literal resolves to a registry token"))

  # ---- D4 frozen count ------------------------------------------------------
  findings <- c(findings, finding(
    "D4", length(tokens) == expected_n,
    if (length(tokens) != expected_n)
      sprintf(paste("vocabulary size %d, gate pinned at %d -- a diagnostic was",
                    "added or removed; update this gate deliberately"),
              length(tokens), expected_n)
    else sprintf("vocabulary frozen at %d tokens", expected_n)))

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

  # A miniature vocabulary with the two shapes the real one has: namespaced
  # families (`ipv4-`, `domain-`) and singletons (`invalid-`, `non-`).
  reg <- c(
    '.URL_DIAGNOSTICS <- c(',
    '  "ipv4-octal",',
    '  "ipv4-leading-zero",',
    '  "domain-empty-label",',
    '  "domain-std3-violation",',
    '  "invalid-URL-unit",',
    '  "non-default-port"',
    ')'
  )
  sect <- c(
    "\\section{Diagnostic vocabulary (canonical)}{",
    "\\itemize{",
    "\\item \\code{ipv4-octal} --- octal.",
    "\\item \\code{ipv4-leading-zero} --- leading zero.",
    "\\item \\code{domain-empty-label} --- empty label.",
    "\\item \\code{domain-std3-violation} --- STD3.",
    "\\item \\code{invalid-URL-unit} --- validation error.",
    "\\item \\code{non-default-port} --- non-default port.",
    "}",
    "}"
  )

  mk <- function(registry = reg, rd = sect, extra_rd = NULL, extra_r = NULL,
                 verdicts = NULL) {
    root <- tempfile("diagdoc-")
    dir.create(file.path(root, "man"), recursive = TRUE)
    dir.create(file.path(root, "R"), recursive = TRUE)
    writeLines(registry, file.path(root, "R", "diagnostics.R"))
    if (!is.null(verdicts)) {
      writeLines(verdicts, file.path(root, "R", "verdicts.R"))
    }
    writeLines(rd, file.path(root, "man", "get_url_diagnostics.Rd"))
    if (!is.null(extra_rd)) {
      writeLines(extra_rd, file.path(root, "man", "other.Rd"))
    }
    if (!is.null(extra_r)) {
      writeLines(extra_r, file.path(root, "R", "other.R"))
    }
    root
  }
  rule <- function(root, id, n = 6L) {
    for (f in check_diagnostics_docs(root, expected_n = n)) {
      if (identical(f$id, id)) return(isTRUE(f$ok))
    }
    NA
  }
  verdict <- function(root, n = 6L) {
    all(vapply(check_diagnostics_docs(root, expected_n = n),
               function(f) isTRUE(f$ok), logical(1)))
  }

  # 1. Clean positive.
  expect("positive: consistent tree passes", verdict(mk()))

  # 2. D1 -- a token the canonical section never documents. This is the exact
  #    drift that shipped twice unnoticed.
  r <- mk(rd = sect[-4])
  expect("D1 fails when the canonical section omits a token",
         identical(rule(r, "D1"), FALSE))

  # 3. D1 -- a missing canonical section fails closed rather than passing on
  #    an empty scan.
  r <- mk(rd = "\\title{no section here}")
  expect("D1 fails when the canonical section is absent",
         identical(rule(r, "D1"), FALSE))

  # 4. D2 -- a stale row left behind by a rename.
  r <- mk(rd = append(sect, "\\item \\code{ipv4-gone-away} --- stale.",
                      after = 5L))
  expect("D2 fails on a documented non-token", identical(rule(r, "D2"), FALSE))

  # 5. D2 -- ordinary hyphenated prose inside the section is not a token.
  r <- mk(rd = append(sect, "set \\code{url_standard} and \\code{some-prose}.",
                      after = 5L))
  expect("D2 ignores prose outside the token families",
         identical(rule(r, "D2"), TRUE))

  # 6. D3 -- a phantom in another Rd (the stale-rename case).
  r <- mk(extra_rd = "reject on \\code{ipv4-nonsense} here")
  expect("D3 fails on a phantom in another Rd",
         identical(rule(r, "D3"), FALSE))

  # 7. D3 -- a phantom in roxygen.
  r <- mk(extra_r = '#\' emits "domain-legacy-label" for these')
  expect("D3 fails on a phantom in roxygen",
         identical(rule(r, "D3"), FALSE))

  # 8. D3 -- a real token mentioned elsewhere is fine.
  r <- mk(extra_rd = "rows carrying \\code{ipv4-octal} are dropped")
  expect("D3 passes a real token mentioned elsewhere",
         identical(rule(r, "D3"), TRUE))

  # 9. D3 -- an unrelated vocabulary sharing no family is untouched. Without
  #    the family anchor this would be a flood of false positives, since the
  #    tokens have no shared prefix to key on.
  r <- mk(extra_rd = 'see \\code{lower_host} and "warning-no-tld" and
    \\code{path-encoding-thing}')
  expect("D3 ignores unrelated hyphenated vocabularies",
         identical(rule(r, "D3"), TRUE))

  # 9b. D3 -- a SINGLETON family is not scanned, which is what keeps the real
  #     `scheme_class` value "non-special" from being read as drift against
  #     the lone `non-default-port`. The live tree really does contain this.
  r <- mk(extra_rd = 'the class is \\code{non-special} for these schemes')
  expect("D3 does not scan a singleton family",
         identical(rule(r, "D3"), TRUE))

  # 9d. D3 -- a value of a SIBLING vocabulary that lands in a diagnostic family
  #     is not a phantom. The live case: the layered-verdict annotation state
  #     `invalid-input` shares the `invalid-` family with `invalid-URL-unit`.
  #     Needs a registry where `invalid-` is a real family (two tokens), or the
  #     singleton rule above would exempt it for the wrong reason.
  reg_inv <- append(reg, '  "invalid-credentials",', after = 6L)
  sect_inv <- append(sect, "\\item \\code{invalid-credentials} --- creds.",
                     after = 7L)
  mk_inv <- function(...) mk(registry = reg_inv, rd = sect_inv, ...)
  vocab <- '.LAYER3_ANNOTATION_STATE <- c("not-applicable", "invalid-input")'

  # Negative half first: with no sibling vocabulary it IS drift.
  r <- mk_inv(extra_rd = "the state is \\code{invalid-input} here")
  expect("D3 fails on an invalid- literal with no sibling vocabulary",
         identical(rule(r, "D3", n = 7L), FALSE))
  r <- mk_inv(extra_rd = "the state is \\code{invalid-input} here",
              verdicts = vocab)
  expect("D3 exempts a sibling-vocabulary value",
         identical(rule(r, "D3", n = 7L), TRUE))
  # ...and the exemption is not a blanket one: a real phantom in the same
  # family still fails even with the sibling vocabulary present.
  r <- mk_inv(extra_rd = "reject on \\code{invalid-nonsense} here",
              verdicts = vocab)
  expect("D3 still fails a phantom when a sibling vocabulary exists",
         identical(rule(r, "D3", n = 7L), FALSE))

  # 9c. Section extraction must survive an escaped BACKSLASH before a closing
  #     brace. `\code{\\}` (the `invalid-reverse-solidus` row in the live
  #     section) really does close; a one-char lookback reads its `}` as
  #     escaped, runs off the end of the section, and silently reports "no
  #     section" -- a gate that passes nothing and catches nothing.
  r <- mk(rd = append(sect, "a literal \\code{\\\\} is rewritten.", after = 5L))
  expect("section extraction handles \\code{\\\\}",
         identical(rule(r, "D1"), TRUE))

  # 10. D4 -- the vocabulary grew, and D1/D2/D3 are all clean, proving D4 is
  #     what caught it.
  r <- mk(registry = append(reg, '  "ipv4-new-form",', after = 1L),
          rd = append(sect, "\\item \\code{ipv4-new-form} --- new.",
                      after = 5L))
  expect("D4 fails when the vocabulary grows", identical(rule(r, "D4"), FALSE))
  expect("D4 is the only rule that catches a fully-swept addition",
         identical(rule(r, "D1"), TRUE) && identical(rule(r, "D2"), TRUE) &&
           identical(rule(r, "D3"), TRUE))

  # 11. Missing registry file fails closed.
  root <- tempfile("diagdoc-empty-")
  dir.create(root)
  expect("missing registry file fails closed", identical(verdict(root), FALSE))

  # 12. A registry file with no .URL_DIAGNOSTICS fails closed too.
  r <- mk(registry = "x <- 1")
  expect("registry without .URL_DIAGNOSTICS fails closed",
         identical(verdict(r), FALSE))

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("diagnostics-doc-consistency self-test: FAIL", call. = FALSE)
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
  ok <- print_findings(check_diagnostics_docs("."))
  if (!ok) {
    stop("diagnostics-doc-consistency gate: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
