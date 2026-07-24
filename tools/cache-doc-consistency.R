#!/usr/bin/env Rscript

# C-08 documentation-consistency gate (rurl 3.0 protocol hardening, G4.1).
#
# The `full_parse` cache has ONE authoritative default bound: the literal
# `.FULL_PARSE_MAX_DEFAULT` in R/zzz.R, which `.onLoad` installs as the shipped
# runtime default. Every piece of user-facing documentation that names that
# default must agree with it. This script is the executable check the accepted
# semantic-cache contract (P5.1@d254ff1 §2.3; design/work/url-v3/contracts/
# semantic-cache-contract.md) requires in the verify chain: it reads the
# source-of-truth literal and every documented bound and FAILS if any diverges.
#
# It is deterministic and network-free. It reads the source-of-truth by sourcing
# R/zzz.R (top level only defines objects and functions -- .onLoad is not run),
# so it needs no package build and no dependencies beyond base R.
#
# Usage:
#   Rscript tools/cache-doc-consistency.R            # scan the repo, exit 1 on drift
#   Rscript tools/cache-doc-consistency.R --self-test # run the positive/negative unit checks
#
# The gate distinguishes the two legitimate "unbounded" statements from the one
# forbidden one:
#   - "full_parse ... unbounded by default"  -> FORBIDDEN (the C-08 defect)
#   - "puny_encode/puny_decode ... unbounded by design" -> CORRECT, never flagged
#   - "max_full_parse = Inf ... unbounded behavior" (the opt-in) -> CORRECT

# --- source-of-truth ---------------------------------------------------------

# Read `.FULL_PARSE_MAX_DEFAULT` from R/zzz.R without loading the package.
read_source_of_truth <- function(zzz_path) {
  if (!file.exists(zzz_path)) {
    stop("cannot find R/zzz.R at: ", zzz_path, call. = FALSE)
  }
  e <- new.env(parent = baseenv())
  # Top-level of zzz.R only binds constants, registry lists, environments, and
  # function definitions; nothing there executes cache work or touches Suggests.
  sys.source(zzz_path, envir = e)
  val <- e[[".FULL_PARSE_MAX_DEFAULT"]]
  if (is.null(val)) {
    stop("R/zzz.R does not define .FULL_PARSE_MAX_DEFAULT", call. = FALSE)
  }
  as.integer(val)
}

# --- predicates over a block of documentation text ---------------------------

# A forbidden claim: the full_parse cache is unbounded *by default*. Matches the
# defect regardless of the exact intervening words ("cache is unbounded by
# default", "unbounded default", ...), while never matching "unbounded by
# design" (the correct Punycode statement) or the "= Inf" opt-in sentence.
doc_claims_full_parse_unbounded_default <- function(text) {
  t <- tolower(text)
  # "unbounded" within a short window before "by default" / "default", but not
  # "by design".
  grepl("unbounded[^.]{0,40}\\bdefault", t) &&
    !grepl("unbounded by design", t)
}

# The set of default-bound numbers a doc block asserts for full_parse. Anchored
# to the phrasings that actually STATE the default ("bounded at N", "default
# bound is N", "bound of N", "N entries", "N unique url") so that example
# argument values (e.g. `rurl_cache_config(max_full_parse = 10000)`) and the
# `1e5` in a code example are never misread as the documented default. Returns
# an integer vector (thousands separators stripped, markdown emphasis removed).
doc_stated_default_bounds <- function(text) {
  # Drop backticks / asterisks so "bound is `100000`" reads like "bound is
  # 100000"; a number is [0-9] with optional comma thousands separators.
  t <- gsub("[`*]", "", tolower(text))
  num <- "([0-9]{1,3}(?:,[0-9]{3})+|[0-9]{4,})"
  patterns <- c(
    paste0("bounded at\\s+", num),
    paste0("bound (?:is|of)\\s+", num),
    paste0(num, "\\s+entries"),
    paste0(num, "\\s+unique url")
  )
  hits <- character(0)
  for (p in patterns) {
    m <- regmatches(t, gregexpr(p, t, perl = TRUE))[[1]]
    if (length(m) > 0L) {
      # Recover just the captured number from each match.
      nums <- regmatches(m, regexpr(num, m, perl = TRUE))
      hits <- c(hits, nums)
    }
  }
  if (length(hits) == 0L) {
    return(integer(0))
  }
  as.integer(gsub(",", "", hits))
}

# Scan one documentation block; return a character vector of violation messages
# (empty when consistent). `default` is the source-of-truth integer.
scan_doc_block <- function(label, text, default) {
  out <- character(0)
  if (doc_claims_full_parse_unbounded_default(text)) {
    out <- c(out, sprintf(
      "%s: claims the full_parse cache is UNBOUNDED by default; the runtime default is %d (bounded reset-watermark)",
      label, default
    ))
  }
  bounds <- doc_stated_default_bounds(text)
  bad <- bounds[bounds != default]
  if (length(bad) > 0L) {
    out <- c(out, sprintf(
      "%s: states a default full_parse bound of %s; the runtime default is %d",
      label, paste(unique(bad), collapse = ", "), default
    ))
  }
  out
}

# --- file collection ---------------------------------------------------------

# Pull the block(s) of a file that mention the full_parse cache, so scanning is
# scoped to relevant prose rather than the whole file. Splits on blank lines
# (paragraphs) and keeps paragraphs naming full_parse or max_full_parse.
relevant_blocks <- function(path) {
  if (!file.exists(path)) {
    return(character(0))
  }
  lines <- readLines(path, warn = FALSE)
  # Paragraph split on runs of blank lines.
  para_id <- cumsum(grepl("^\\s*$", lines))
  blocks <- split(lines, para_id)
  keep <- vapply(blocks, function(b) {
    any(grepl("full_parse|max_full_parse", b, ignore.case = TRUE))
  }, logical(1))
  vapply(blocks[keep], function(b) paste(b, collapse = "\n"), character(1))
}

# The documentation surfaces C-08 §2.3 enumerates: README, the cache roxygen ->
# man/*.Rd, the vignettes, and inline R/ cache-bound comments.
doc_targets <- function(root) {
  c(
    file.path(root, "README.md"),
    file.path(root, "README.Rmd"),
    Sys.glob(file.path(root, "man", "rurl_cache_*.Rd")),
    Sys.glob(file.path(root, "vignettes", "*.Rmd")),
    file.path(root, "R", "zzz.R")
  )
}

check_repo <- function(root) {
  zzz <- file.path(root, "R", "zzz.R")
  default <- read_source_of_truth(zzz)
  violations <- character(0)
  scanned <- 0L
  for (path in doc_targets(root)) {
    for (blk in relevant_blocks(path)) {
      scanned <- scanned + 1L
      violations <- c(
        violations,
        scan_doc_block(sub(paste0("^", root, "/?"), "", path), blk, default)
      )
    }
  }
  list(default = default, scanned = scanned, violations = violations)
}

# --- self-test (positive + negative coverage, executable) --------------------

self_test <- function() {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  d <- 100000L

  # NEGATIVE cases: the gate must flag these.
  neg <- c(
    "The `full_parse` cache is unbounded by default (`max_full_parse = Inf`).",
    "full_parse is unbounded by default; set a bound to cap memory.",
    "The default bound is 50000 entries."
  )
  for (t in neg) {
    if (length(scan_doc_block("neg", t, d)) == 0L) {
      fail(sprintf("did not flag a divergent doc block: %s", t))
    }
  }

  # POSITIVE cases: the gate must NOT flag these.
  pos <- c(
    "The `full_parse` cache is bounded at 100000 entries by default; set `max_full_parse = Inf` for the historical unbounded behavior.",
    "The default bound is 100000 unique url x core-option combinations.",
    "The default bound is `100000`. The puny_encode and puny_decode caches are unbounded by design.",
    "The puny_encode and puny_decode caches are unbounded by design.",
    "Set max_full_parse = Inf to opt into unbounded behavior."
  )
  for (t in pos) {
    v <- scan_doc_block("pos", t, d)
    if (length(v) > 0L) {
      fail(sprintf("false positive on a consistent doc block: %s -> %s",
                   t, paste(v, collapse = "; ")))
    }
  }

  # The "100,000" thousands-separated form must read as 100000, not flagged.
  if (length(scan_doc_block("sep", "Default bound is 100,000 entries.", d)) > 0L) {
    fail("thousands-separated 100,000 was misread as a divergence")
  }
  # And a wrong thousands-separated form MUST be flagged.
  if (length(scan_doc_block("sep2", "Default bound is 200,000 entries.", d)) == 0L) {
    fail("did not flag a wrong thousands-separated bound")
  }

  cat("cache-doc-consistency self-test: PASS (", length(neg),
      "negative +", length(pos), "positive cases)\n")
  invisible(TRUE)
}

# --- main --------------------------------------------------------------------

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  # Repo root: the parent of this script's tools/ directory, or getwd() when
  # invoked plainly from the package root.
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }

  res <- check_repo(root)
  cat(sprintf(
    "C-08 cache-doc-consistency gate\n  source of truth: .FULL_PARSE_MAX_DEFAULT = %d (R/zzz.R)\n  scanned %d relevant doc block(s)\n",
    res$default, res$scanned
  ))
  if (length(res$violations) > 0L) {
    cat("DIVERGENCE:\n")
    for (v in res$violations) cat("  - ", v, "\n", sep = "")
    stop(
      sprintf("cache documentation diverges from the runtime default (%d violation(s))",
              length(res$violations)),
      call. = FALSE
    )
  }
  cat("PASS: every documented full_parse bound matches the runtime default.\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  # Run main() only when executed as a script (Rscript / R CMD BATCH), not when
  # sourced for its functions by a test.
  if (sys.nframe() == 0L) {
    main()
  }
}
