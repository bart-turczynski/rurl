#!/usr/bin/env Rscript

# curl zero-reference closure gate (RURL-cunfohwy).
#
# This is the executable form of the v3 line's CRAN release criterion from P0.4
# ("The v2/v3 boundary (concrete)") + RCON-09 section 4 + S8 gates 2-3/8: the
# v3 line does not ship while any curl reference survives.
#
# It VERIFIES; it decides nothing. It does not judge whether dropping libcurl
# was right, and it grants no coverage to any other gate.
#
# THIS GATE IS RED ON PURPOSE TODAY. `curl::curl_parse_url()` is still live at
# the single remaining parse seam, so C1/C2/C3/C6 fail. Landing the gate before
# the work is the v3 protocol's own pattern (G4.2, G4.3): the criterion is a
# promissory note until it is executable, and an executable criterion that
# nobody can quietly reinterpret is worth more red than absent.
#
# WHY A SHADOWING SHIM AND NOT AN UNINSTALL. The clean room cannot uninstall
# curl: on a stock R the base library and curl live in the SAME directory, and
# `.Library` is compiled in, so no `R_LIBS` setting can hide it. Instead the
# clean room installs a POISONED package named `curl` into a temporary library
# placed AHEAD of the real one. It shadows the genuine package, and its
# `.onLoad()` stops. Anything that loads curl -- at load time, in an example, in
# a vignette, in a test, in a check -- dies loudly and by name. C7 additionally
# asserts the shim is the copy that resolves, because a clean room that
# silently found the real curl would be a false green, which is worse than no
# clean room at all.
#
# The install-time half of "curl ABSENT" is C1's job, not the clean room's:
# R CMD check enforces DECLARED dependencies, so a DESCRIPTION with no curl in
# any dependency field is the proof that a curl-less machine can install rurl.
#
# WHY A SCAN AND NOT A GREP. C3/C4 strip comments by PARSING, because this
# codebase deliberately keeps prose that names libcurl (why a shim existed, why
# a construct was routed around it). That commentary is history and must not be
# what fails the gate. C6 is the opposite instrument: it scans raw text
# INCLUDING comments across the declared scope and requires every surviving
# mention to be allowlisted with a written reason, so stale libcurl commentary
# cannot accumulate unnoticed after the dependency is gone.
#
# THE CHECKS
#   C0  allowlist  -- every allowlist entry exists AND still has a hit. A stale
#                     entry is a violation: an allowlist may not over-permit.
#   C1  DESCRIPTION-- no curl in any dependency field (Depends / Imports /
#                     Suggests / LinkingTo / Enhances / Remotes).
#   C2  NAMESPACE  -- no import(curl) / importFrom(curl, ...).
#   C3  R/ runtime -- no curl in parsed R/ code: `curl::`, `curl:::`,
#                     library/require(curl), and the indirect string forms
#                     (requireNamespace / loadNamespace / getNamespace /
#                     asNamespace / getExportedValue). Comments excluded.
#   C4  tests      -- the same scan over tests/. RCON-09 forbids a curl
#                     reference in tests too, so the oracles outlive the
#                     dependency.
#   C5  man        -- no curl in generated help (man/*.Rd), which is where an
#                     `@importFrom` leaks into the shipped docs.
#   C6  static     -- raw-text scan of the declared scope, comments included,
#                     minus the allowlist, minus the ONE categorical exemption
#                     below (a provenance command field of a registered
#                     oracle fixture).
#   C7  clean room -- build + R CMD check against a temporary library whose
#                     `curl` is poisoned. Runs only in full mode.
#
# THE CATEGORICAL EXEMPTION (RURL-vuxlhehz). P5.3 section 2.3 requires every
# imported oracle fixture to record its `import_command`, and for a fetched
# upstream artifact that command is a curl invocation. Allowlisting each such
# fixture by PATH keyed the exemption on the file rather than on the reason,
# so every new oracle fixture needed a one-off row here. The exemption is now
# stated as the reason: a curl hit is inert when it sits inside the value of a
# provenance COMMAND field (`import_command`, `generation_command`,
# `pin_fetch_command`) of a file REGISTERED as a fixture in
# tests/testthat/fixtures/oracle-provenance.json. Nothing else is reached by
# it: an unregistered file gets no exemption however its `_meta` reads, and a
# curl mention anywhere else in a registered file still fails C6. The registry
# is read with base R (bracket depth, no JSON library), and a missing registry
# means no exemption -- the rule fails closed.
#
# Zero dependencies beyond base R. Deterministic. C0-C6 are network-free; C7
# runs R CMD check, which is not.
#
# Usage:
#   Rscript tools/curl-zero-gate.R               # full: C0-C7, exit 1 on any
#   Rscript tools/curl-zero-gate.R --static-only # C0-C6; VERDICT PARTIAL
#   Rscript tools/curl-zero-gate.R --self-test   # positive/negative fixtures
#
# `--static-only` never satisfies the release criterion, and says so in its
# verdict line: only the full run closes RCON-09.

DEP_FIELDS <- c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances",
                "Remotes")

# Directories and files C6 scans as raw text. Everything the package SHIPS or
# runs, plus tools/. Deliberately NOT the whole repo: design/, NEWS.md and the
# ADRs are the historical record and must keep naming libcurl -- excluding them
# by scope is honest, where allowlisting them would imply they are exceptions
# waiting to be cleaned up.
SCAN_SCOPE <- c("DESCRIPTION", "NAMESPACE", "R", "tests", "man", "vignettes",
                "inst", "tools")

# Paths that may keep a curl mention, each with the reason it is permitted.
# Adding a row here is a decision, not a formality: C0 fails a row that no
# longer has a hit, so the list cannot outlive its justification.
ALLOWLIST <- list(
  list(
    path = "tools/curl-zero-gate.R",
    reason = paste("this gate; it must name what it forbids in order to",
                   "forbid it")
  ),
  list(
    path = "tools/verify-manifest.yml",
    reason = paste("the gate manifest tools/verify.R derives its gate list",
                   "from; it schedules this gate by file name, so it cannot",
                   "be curl-free while this gate runs (RURL-vunvxusf moved",
                   "it here from .github/workflows/, which was out of scope)")
  ),
  list(
    path = "tools/determinism",
    reason = paste("the libcurl-version determinism harness (RURL-gxqdmpcp)",
                   "measures libcurl itself and is reference material, not",
                   "package runtime")
  ),
  # --- the in-tree replacements (RURL-robgajml) ------------------------------
  # These four are the files that REPLACED libcurl. Their whole content is a
  # reproduction of measured libcurl behaviour, and every constant, refusal and
  # spelling in them is justified by what libcurl did. Sweeping the references
  # would not remove a dependency -- it would delete the provenance of the
  # rules and leave a pile of unexplained magic numbers. Naming what they
  # reproduce is what makes them auditable.
  list(
    path = "R/parse-web.R",
    reason = paste("the in-tree web/special-scheme parser that replaced the",
                   "libcurl seam; every rule in it is a measured libcurl",
                   "behaviour and is cited as such")
  ),
  list(
    path = "tests/testthat/test-parse-web.R",
    reason = paste("the literal oracle for R/parse-web.R; it records which",
                   "libcurl behaviour each literal was frozen from, and the",
                   "literals ARE the oracle -- no live curl call")
  ),
  list(
    path = "R/percent-coding.R",
    reason = paste("the in-tree percent-coding helpers that replaced",
                   "curl_escape/curl_unescape; the byte-exactness contract",
                   "and its two documented deviations are stated against",
                   "libcurl")
  ),
  list(
    path = "tests/testthat/test-percent-coding.R",
    reason = paste("the literal oracle for R/percent-coding.R; same reason",
                   "as test-parse-web.R -- literals only, no live curl call")
  ),
  # --- provenance of imported data -------------------------------------------
  # `curl -fsSL <url> -o <file>` is the SHELL COMMAND that fetched a pinned
  # upstream fixture. It is a reproducibility record, not a package
  # dependency: rewriting it would falsify how the data actually arrived.
  #
  # The fixtures that carry that command in their own `_meta` block are NOT
  # listed here any more: they are covered by the categorical exemption
  # (PROVENANCE_REGISTRY + PROVENANCE_COMMAND_KEYS below), which reaches every
  # registered fixture without a row per file. The rows that remain in this
  # section are the ones the category cannot reach, each saying why.
  list(
    # Cannot collapse: this is the REGISTRY, not a registered fixture, and
    # besides its command fields it names curl in a narrative
    # `normative_dependencies[].note` (the SecWeb 2022 cross-tested parsers).
    path = "tests/testthat/fixtures/oracle-provenance.json",
    reason = paste("records the shell command that imported each pinned",
                   "upstream oracle; a provenance record must not be",
                   "rewritten")
  ),
  list(
    path = "tools/oracle/transcribe-youarealiar.R",
    reason = paste("curl is one of the seven PARSERS the SecWeb 2022 paper",
                   "cross-tested, so it is named inside transcribed",
                   "paper_claimed_behavior strings ('curl Error',",
                   "'curl host=\\\\'); rewriting them would falsify the",
                   "transcription, and the paper is the primary source")
  ),
  list(
    path = "tools/oracle/verify-youarealiar.R",
    reason = paste("emits the `curl -fsSL` fetch command a reader would need",
                   "if the group's byte-verification sentinels are ever",
                   "filled in -- same reproducibility-record rationale as",
                   "oracle-provenance.json, whose import_command it mirrors")
  ),
  list(
    path = "tools/oracle/fetch-source.R",
    reason = paste("builds the `curl -fsSL` command a reader runs by hand when",
                   "a tier-2 upstream source cannot be resolved -- the abort",
                   "message IS the reproducibility record, and printing a",
                   "command nobody can paste would defeat it. It fetches",
                   "through utils::download.file(), so no curl code path is",
                   "taken; the string is instructions for a human")
  ),
  list(
    # Cannot collapse: it EMITS the command from Python source, and it is a
    # generator, not a registered fixture.
    path = "inst/bench/make-wpt-fixture.py",
    reason = "emits that import command, so it must contain it verbatim"
  ),
  list(
    # Cannot collapse: its self-test fixtures are R string literals, not a
    # registered fixture file.
    path = "tools/oracle-provenance-gate.R",
    reason = paste("validates those import_command strings, so its fixtures",
                   "contain one")
  ),
  # --- curl as a MEASURED SUBJECT, not a dependency --------------------------
  list(
    path = "inst/bench/parser-disagreement.R",
    reason = paste("the cross-implementation comparison harness; curl is one",
                   "of the PARTICIPANTS being measured (alongside adaR and",
                   "urltools), the same standing tools/determinism has")
  ),
  list(
    # Cannot collapse: it IS registered in the provenance registry, but its
    # curl is a data column (a measured parser), not a provenance field, so
    # the categorical exemption does not reach it -- and must not.
    path = "tests/testthat/fixtures/external-url-vectors.csv",
    reason = paste("published research data recording what OTHER parsers do",
                   "with each vector; curl is one of the columns and the",
                   "record is factual, not a rurl behaviour")
  ),
  # --- the release rule that requires all of the above -----------------------
  list(
    path = "tools/release-rule-check.R",
    reason = paste("polices the 'no CRAN until curl-free' release rule",
                   "(RCON-10); like this gate, it must name what it checks")
  ),
  list(
    path = "tools/verify.R",
    reason = paste("the local verify gate; it RUNS this gate and documents",
                   "why the C7 clean room is release-only, so it must name",
                   "the thing it invokes")
  )
)

# The categorical exemption's two coordinates. A file is REGISTERED when its
# repo-relative path is the `path` of an element of the registry's top-level
# `fixtures` array; a hit is INERT when the whole line is one of these keys
# paired with a single JSON string value. Both coordinates are required; a
# curl anywhere else in a registered file, or in a provenance-shaped line of
# an unregistered file, is a violation like any other.
PROVENANCE_REGISTRY <- "tests/testthat/fixtures/oracle-provenance.json"
PROVENANCE_COMMAND_KEYS <- c("import_command", "generation_command",
                             "pin_fetch_command")

# The token, matched case-insensitively so `curl`, `libcurl`, `curl_parse_url`
# and `RCurl` all hit. Over-matching is intentional -- a false positive costs
# an allowlist row with a reason, a false negative costs the criterion.
CURL_PATTERN <- "curl"

# Indirect load forms: a namespace named by STRING rather than by `::`.
INDIRECT_FNS <- c("requireNamespace", "loadNamespace", "getNamespace",
                  "asNamespace", "getExportedValue", "library", "require",
                  "attachNamespace")

# ---- helpers ----------------------------------------------------------------

r_files <- function(dir) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  list.files(dir, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
}

has_curl <- function(x) {
  grepl(CURL_PATTERN, x, ignore.case = TRUE)
}

# Every code token of a file with comments removed. Parses rather than greps:
# `parse()` drops comments, and deparsing the expressions yields the symbols,
# the `::` calls and the string literals -- which is exactly the set that can
# reference a namespace at runtime.
code_text <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = FALSE),
                    error = function(e) NULL)
  if (is.null(exprs)) {
    # An unparseable file cannot be cleared. Fall back to raw text so the file
    # fails the check rather than passing on a technicality.
    return(readLines(path, warn = FALSE))
  }
  unlist(lapply(exprs, deparse, width.cutoff = 500L), use.names = FALSE)
}

# Runtime curl references in one file's parsed code. Returns matching lines.
runtime_hits <- function(path) {
  txt <- code_text(path)
  direct <- grepl("\\bcurl:{2,3}", txt)
  indirect <- grepl(
    sprintf("\\b(%s)\\s*\\(\\s*[\"']?curl[\"']?",
            paste(INDIRECT_FNS, collapse = "|")),
    txt
  )
  txt[direct | indirect]
}

# ---- allowlist --------------------------------------------------------------

allow_paths <- function() {
  vapply(ALLOWLIST, `[[`, character(1), "path")
}

is_allowed <- function(rel) {
  any(vapply(allow_paths(), function(p) {
    identical(rel, p) || startsWith(rel, paste0(p, "/"))
  }, logical(1)))
}

# Every file C6 looks at, as repo-relative paths.
scan_files <- function(root) {
  out <- character(0)
  for (s in SCAN_SCOPE) {
    p <- file.path(root, s)
    if (dir.exists(p)) {
      out <- c(out, list.files(p, recursive = TRUE, full.names = TRUE))
    } else if (file.exists(p)) {
      out <- c(out, p)
    }
  }
  # Skip binaries and generated artifacts: a byte scan of a .rds or a .png
  # produces noise, never a reference.
  out <- out[!grepl("\\.(rds|RData|rda|png|jpg|jpeg|gif|pdf|ico|so|dll|o)$",
                    out, ignore.case = TRUE)]
  sub(paste0("^", normalizePath(root, mustWork = FALSE), "/"), "",
      normalizePath(out, mustWork = FALSE))
}

file_has_curl <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE),
                    error = function(e) character(0))
  any(has_curl(lines))
}

# ---- the categorical exemption ----------------------------------------------

# Bracket depth of each line of a JSON text, computed with string bodies
# blanked first so a `{` or `[` inside a value does not count. Returns, per
# line, the depth BEFORE the line and the count of opens/closes on it.
blank_json_strings <- function(x) {
  gsub('"(\\\\.|[^"\\\\])*"', '""', x)
}

count_of <- function(x, re) {
  nchar(x) - nchar(gsub(re, "", x))
}

json_depths <- function(lines) {
  bare <- blank_json_strings(lines)
  opens <- count_of(bare, "\\[|\\{")
  closes <- count_of(bare, "\\]|\\}")
  after <- cumsum(opens - closes)
  list(before = c(0L, utils::head(after, -1L)), opens = opens,
       closes = closes)
}

# The repo-relative paths registered as fixtures: the `path` key of every
# element of the registry's top-level `fixtures` array, and no other `path`
# key (the registry nests `path` keys inside source groups too, and its
# `governing_decision` carries one). Base R by design -- this gate must not
# grow a dependency to read a record -- so the registry is walked by bracket
# depth, one key per line, which is the shape the pretty-printed record has.
# A missing or unreadable registry registers nothing.
registered_fixture_paths <- function(root) {
  reg <- file.path(root, PROVENANCE_REGISTRY)
  if (!file.exists(reg)) {
    return(character(0))
  }
  lines <- tryCatch(readLines(reg, warn = FALSE),
                    error = function(e) character(0))
  if (!length(lines)) {
    return(character(0))
  }
  d <- json_depths(lines)
  out <- character(0)
  in_fixtures <- FALSE
  fx_depth <- NA_integer_
  for (i in seq_along(lines)) {
    line <- lines[i]
    if (!in_fixtures) {
      if (d$before[i] == 1L && grepl('^\\s*"fixtures"\\s*:\\s*\\[', line)) {
        in_fixtures <- TRUE
        fx_depth <- 2L
      }
      next
    }
    if (d$before[i] < fx_depth) {
      # The array closed on an earlier line; nothing after it registers.
      break
    }
    m <- regexec('"path"\\s*:\\s*"((\\\\.|[^"\\\\])*)"', line)[[1L]]
    if (m[1L] < 0L) {
      next
    }
    # Depth AT the key, not at the start of the line, so a one-line element
    # (`{ "path": ... }`) is read the same way as a pretty-printed one.
    prefix <- blank_json_strings(substr(line, 1L, m[1L] - 1L))
    depth_at <- d$before[i] + count_of(prefix, "\\[|\\{") -
      count_of(prefix, "\\]|\\}")
    if (depth_at == fx_depth + 1L) {
      len <- attr(m, "match.length")[2L]
      out <- c(out, substr(line, m[2L], m[2L] + len - 1L))
    }
  }
  out
}

# TRUE for a line that is, in its entirety, one provenance command key paired
# with one JSON string value (escapes allowed, trailing comma allowed). The
# regex consumes the whole line, so a curl on such a line can only be inside
# the value -- there is nowhere else on the line for it to be.
is_provenance_command_line <- function(lines) {
  grepl(sprintf('^\\s*"(%s)"\\s*:\\s*"(\\\\.|[^"\\\\])*"\\s*,?\\s*$',
                paste(PROVENANCE_COMMAND_KEYS, collapse = "|")),
        lines)
}

# The curl hits of one file that the categorical exemption does NOT clear:
# every hit if the file is unregistered, otherwise every hit outside a
# provenance command line. Returns 1-based line numbers.
live_curl_hits <- function(path, registered) {
  lines <- tryCatch(readLines(path, warn = FALSE),
                    error = function(e) character(0))
  hit <- has_curl(lines)
  if (registered) {
    hit <- hit & !is_provenance_command_line(lines)
  }
  which(hit)
}

# ---- the checks -------------------------------------------------------------

finding <- function(id, ok, detail) {
  list(id = id, ok = ok, detail = detail)
}

check_allowlist <- function(root) {
  stale <- character(0)
  for (a in ALLOWLIST) {
    p <- file.path(root, a$path)
    if (!file.exists(p) && !dir.exists(p)) {
      stale <- c(stale, sprintf("%s: allowlisted path does not exist", a$path))
      next
    }
    files <- if (dir.exists(p)) {
      list.files(p, recursive = TRUE, full.names = TRUE)
    } else {
      p
    }
    if (!any(vapply(files, file_has_curl, logical(1)))) {
      stale <- c(stale, sprintf(
        "%s: allowlisted but has no curl reference left -- remove the row",
        a$path
      ))
    }
    if (!nzchar(trimws(a$reason))) {
      stale <- c(stale, sprintf("%s: allowlist row has no reason", a$path))
    }
  }
  finding("C0", length(stale) == 0L,
          if (length(stale)) paste(stale, collapse = "; ")
          else sprintf("%d allowlist row(s), all live and justified",
                       length(ALLOWLIST)))
}

check_description <- function(root) {
  path <- file.path(root, "DESCRIPTION")
  if (!file.exists(path)) {
    return(finding("C1", FALSE, "DESCRIPTION not found"))
  }
  dcf <- read.dcf(path)
  bad <- character(0)
  for (f in DEP_FIELDS) {
    if (!f %in% colnames(dcf)) {
      next
    }
    v <- dcf[1L, f]
    if (!is.na(v) && has_curl(v)) {
      deps <- trimws(strsplit(v, ",", fixed = TRUE)[[1]])
      hit <- deps[has_curl(deps)]
      bad <- c(bad, sprintf("%s: %s", f, toString(hit)))
    }
  }
  finding("C1", length(bad) == 0L,
          if (length(bad)) paste(bad, collapse = "; ")
          else "no curl in any dependency field")
}

check_namespace <- function(root) {
  path <- file.path(root, "NAMESPACE")
  if (!file.exists(path)) {
    return(finding("C2", FALSE, "NAMESPACE not found"))
  }
  lines <- readLines(path, warn = FALSE)
  directives <- paste("import", "importFrom", "importClassesFrom",
                      "importMethodsFrom", "useDynLib", sep = "|")
  hit <- lines[grepl(sprintf("^\\s*(%s)\\s*\\(\\s*curl\\b", directives), lines)]
  finding("C2", length(hit) == 0L,
          if (length(hit)) toString(hit) else "no curl import")
}

scan_runtime <- function(root, dir, id, label) {
  files <- r_files(file.path(root, dir))
  bad <- character(0)
  for (f in files) {
    hits <- runtime_hits(f)
    if (length(hits)) {
      rel <- sub(paste0("^", root, "/?"), "", f)
      bad <- c(bad, sprintf("%s: %s", rel, trimws(hits[1L])))
    }
  }
  finding(id, length(bad) == 0L,
          if (length(bad)) {
            sprintf("%d file(s): %s", length(bad),
                    paste(utils::head(bad, 5L), collapse = " | "))
          } else {
            sprintf("no runtime curl reference in %s (%d file(s) parsed)",
                    label, length(files))
          })
}

check_man <- function(root) {
  dir <- file.path(root, "man")
  files <- if (dir.exists(dir)) {
    list.files(dir, pattern = "\\.Rd$", full.names = TRUE)
  } else {
    character(0)
  }
  bad <- character(0)
  for (f in files) {
    if (file_has_curl(f)) {
      bad <- c(bad, basename(f))
    }
  }
  finding("C5", length(bad) == 0L,
          if (length(bad)) sprintf("%d Rd file(s): %s", length(bad),
                                   toString(utils::head(bad, 8L)))
          else sprintf("no curl in %d generated Rd file(s)", length(files)))
}

check_static <- function(root) {
  files <- scan_files(root)
  registered <- registered_fixture_paths(root)
  bad <- character(0)
  cleared <- character(0)
  for (rel in files) {
    if (is_allowed(rel)) {
      next
    }
    is_reg <- rel %in% registered
    hits <- live_curl_hits(file.path(root, rel), is_reg)
    if (length(hits)) {
      bad <- c(bad, if (is_reg) {
        sprintf(paste("%s (registered fixture; curl outside a provenance",
                      "field at line %d)"), rel, hits[1L])
      } else {
        rel
      })
    } else if (is_reg && file_has_curl(file.path(root, rel))) {
      cleared <- c(cleared, rel)
    }
  }
  finding("C6", length(bad) == 0L,
          if (length(bad)) {
            sprintf("%d unallowlisted file(s) mention curl: %s",
                    length(bad), toString(utils::head(bad, 8L)))
          } else {
            sprintf(paste("%d file(s) scanned, %d allowlisted path(s),",
                          "%d registered fixture(s) cleared by the",
                          "provenance-field exemption"),
                    length(files), length(ALLOWLIST), length(cleared))
          })
}

# ---- C7: the clean room -----------------------------------------------------

# Write and install a package named `curl` whose every entry point is fatal.
# Returns the library path it was installed into.
install_poisoned_curl <- function(lib) {
  src <- file.path(tempfile("curl-shim-"), "curl")
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c(
    "Package: curl",
    "Title: Poisoned Clean-Room Shim",
    "Version: 999.0.0",
    "Description: Fails on load. Installed AHEAD of the real curl so that any",
    "    attempt to use libcurl during a clean-room check dies by name.",
    "Author: rurl clean room",
    "Maintainer: rurl clean room <noreply@example.com>",
    "License: MIT + file LICENSE",
    "Encoding: UTF-8"
  ), file.path(src, "DESCRIPTION"))
  writeLines(c("YEAR: 2026", "COPYRIGHT HOLDER: rurl clean room"),
             file.path(src, "LICENSE"))
  writeLines(c(
    ".onLoad <- function(libname, pkgname) {",
    "  stop('CLEAN-ROOM VIOLATION: curl was loaded', call. = FALSE)",
    "}"
  ), file.path(src, "R", "zzz.R"))
  writeLines("exportPattern('^[[:alpha:]]+')", file.path(src, "NAMESPACE"))

  # `--no-test-load` is load-bearing: R CMD INSTALL loads the package it just
  # installed to check it can be loaded, and this one is BUILT to fail exactly
  # then. Without the flag the shim can never be installed at all.
  rbin <- file.path(R.home("bin"), "R")
  out <- suppressWarnings(system2(
    rbin, c("CMD", "INSTALL", "--no-test-load",
            sprintf("--library=%s", shQuote(lib)), shQuote(src)),
    stdout = TRUE, stderr = TRUE
  ))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) {
    stop("clean room: could not install the poisoned curl shim:\n",
         paste(out, collapse = "\n"), call. = FALSE)
  }
  lib
}

# The two WARNING bodies the clean room MANUFACTURES, sentence by sentence.
#
# Tolerating the two check NAMES was the old rule, and it was too coarse
# (RURL-pdrrmfmu): R CMD check reports several findings under one heading, so
# anything else filed under "files in 'vignettes'" or "package vignettes"
# became invisible. A stray `vignettes/leftover.log` really did produce
# "The following files look like leftovers/mistakes:" under an already-
# tolerated heading, and C7 still returned PASS.
#
# So the heading is tolerated only when EVERY line of its body is one of these
# sentences. One unrecognized line and the whole block counts again. The
# file-list lines are held to vignette source extensions specifically, so a
# leftover named in the list is caught even when no new sentence accompanies
# it. Quotes may be directional or plain depending on the check's locale.
CLEAN_ROOM_ARTIFACT_BODY <- c(
  "^Files in the .vignettes. directory but no files in .inst/doc.:$",
  "^Directory .inst/doc. does not exist\\.$",
  "^Package vignettes without corresponding single PDF/HTML:$",
  paste0("^([‘'][^’']+\\.(Rmd|Rnw|Rtex|Rhtml|Rasciidoc|Rrst)",
         "[’']\\s*)+$")
)
CLEAN_ROOM_ARTIFACT_HEAD <- paste0(
  "^\\* checking (files in .vignettes.|package vignettes) \\.\\.\\. WARNING$"
)

# Every flagged line in a 00check.log that the clean room does NOT manufacture.
# Pure, so the self-test can exercise the tolerance without running a real
# R CMD check (which is why C7 was previously untestable at unit scale).
clean_room_findings <- function(lines) {
  starts <- grep("^\\* ", lines)
  if (!length(starts)) {
    return(trimws(grep("^Status:", lines, value = TRUE)))
  }
  ends <- c(starts[-1L] - 1L, length(lines))
  flagged <- character(0)
  for (i in seq_along(starts)) {
    head_line <- trimws(lines[starts[i]])
    if (!grepl("(ERROR|WARNING)\\s*$", head_line)) {
      next
    }
    body <- character(0)
    if (ends[i] > starts[i]) {
      body <- trimws(lines[(starts[i] + 1L):ends[i]])
      body <- body[nzchar(body)]
    }
    tolerated <- grepl(CLEAN_ROOM_ARTIFACT_HEAD, head_line) &&
      all(vapply(body, function(b) {
        any(vapply(CLEAN_ROOM_ARTIFACT_BODY, grepl, logical(1), x = b))
      }, logical(1)))
    if (!tolerated) {
      # Carry the unrecognized body lines: "... WARNING" alone does not say
      # what went wrong, and the check directory does not survive the run.
      extra <- body[!vapply(body, function(b) {
        any(vapply(CLEAN_ROOM_ARTIFACT_BODY, grepl, logical(1), x = b))
      }, logical(1))]
      flagged <- c(flagged, if (length(extra)) {
        sprintf("%s [%s]", head_line, toString(utils::head(extra, 2L)))
      } else {
        head_line
      })
    }
  }
  # The trailing "Status: N WARNINGs" line summarizes what was just filtered,
  # so it only counts when a real finding survived.
  if (length(flagged)) {
    flagged <- c(flagged, trimws(grep("^Status:", lines, value = TRUE)))
  }
  flagged
}

check_clean_room <- function(root) {
  lib <- tempfile("curl-zero-lib-")
  dir.create(lib, recursive = TRUE)
  shim <- tryCatch(install_poisoned_curl(lib), error = function(e) e)
  if (inherits(shim, "error")) {
    return(finding("C7", FALSE, conditionMessage(shim)))
  }

  rbin <- file.path(R.home("bin"), "R")
  libs <- paste(lib, paste(.libPaths(), collapse = .Platform$path.sep),
                sep = .Platform$path.sep)
  env <- c(sprintf("R_LIBS=%s", libs), "R_LIBS_USER=", "R_LIBS_SITE=")

  # The clean room is only a clean room if the shim is what resolves. A run
  # that silently found the real curl would report a false green.
  resolved <- suppressWarnings(system2(
    rbin, c("--vanilla", "-s", "-e",
            shQuote("cat(find.package('curl'))")),
    env = env, stdout = TRUE, stderr = TRUE
  ))
  # Compare CANONICAL paths: on macOS the temp dir is reached through a
  # symlink (/var -> /private/var), so a literal prefix test would report a
  # false "not clean" against the very shim it just installed.
  canon <- function(p) normalizePath(p, mustWork = FALSE)
  if (!any(startsWith(canon(resolved), canon(lib)))) {
    return(finding("C7", FALSE, sprintf(
      "clean room is not clean: curl resolves to %s, not the shim at %s",
      paste(resolved, collapse = " "), lib
    )))
  }

  tarball <- tryCatch({
    tmp <- tempfile("curl-zero-build-")
    dir.create(tmp)
    out <- suppressWarnings(system2(
      rbin, c("CMD", "build", "--no-build-vignettes", shQuote(root)),
      stdout = TRUE, stderr = TRUE
    ))
    built <- list.files(".", pattern = "\\.tar\\.gz$", full.names = TRUE)
    if (!length(built)) {
      stop(paste(out, collapse = "\n"), call. = FALSE)
    }
    newest <- built[order(file.mtime(built), decreasing = TRUE)][1L]
    dest <- file.path(tmp, basename(newest))
    file.rename(newest, dest)
    dest
  }, error = function(e) e)
  if (inherits(tarball, "error")) {
    return(finding("C7", FALSE,
                   paste("R CMD build failed:", conditionMessage(tarball))))
  }

  chk <- tempfile("curl-zero-check-")
  dir.create(chk)
  out <- suppressWarnings(system2(
    rbin, c("CMD", "check", "--no-manual", "--no-build-vignettes",
            sprintf("--output=%s", shQuote(chk)), shQuote(tarball)),
    env = env, stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  violated <- any(grepl("CLEAN-ROOM VIOLATION", out, fixed = TRUE))
  log <- file.path(chk, "rurl.Rcheck", "00check.log")
  # 00check.log reports a failed step as "* checking <thing> ... ERROR", so the
  # verdict word is at the END of the line, not the start. Anchoring at the
  # start would find nothing and report a bare exit code -- and the check
  # directory does not outlive this process, so that log is unreadable
  # afterwards. Read it while it exists.
  # Two WARNINGs are MANUFACTURED BY THIS GATE and must not count against it.
  # The clean room builds and checks with `--no-build-vignettes` -- deliberately,
  # because rebuilding vignettes would drag the whole rmarkdown/knitr toolchain
  # into the poisoned library and test THAT rather than rurl. R CMD check then
  # always reports "files in 'vignettes' ... WARNING" and "package vignettes ...
  # WARNING" for a package that HAS vignettes, because `inst/doc` is absent by
  # construction. Both reproduce with no shim at all, so treating them as
  # clean-room failures made C7 unpassable for this package no matter what.
  #
  # The tolerance is scoped by CONTENT, not by check name -- see
  # `clean_room_findings()`. Any other WARNING, any ERROR, and any unrecognized
  # line under a tolerated heading still count. `--no-build-vignettes` does NOT
  # skip "checking running R code from vignettes", so a vignette that loaded
  # curl would still be caught there.
  errors <- if (file.exists(log)) {
    clean_room_findings(readLines(log, warn = FALSE))
  } else {
    character(0)
  }
  ok <- !violated && (is.null(status) || status == 0L) && !length(errors)
  detail <- if (violated) {
    "curl was LOADED during the clean-room check"
  } else if (length(errors)) {
    sprintf("R CMD check: %s", toString(utils::head(errors, 4L)))
  } else if (!is.null(status) && status != 0L) {
    # A check that dies BEFORE writing 00check.log (a dependency it cannot
    # install, a load failure) leaves nothing to grep, so carry the tail of the
    # transcript instead: the temp check directory does not survive the run,
    # and a bare exit code would be undiagnosable after the fact.
    tail_lines <- out[nzchar(trimws(out))]
    sprintf("R CMD check exited %d: %s", status,
            paste(utils::tail(tail_lines, 3L), collapse = " / "))
  } else {
    "install + load + examples + tests + check clean with curl poisoned"
  }
  finding("C7", ok, detail)
}

# ---- reporting --------------------------------------------------------------

LABELS <- c(
  C0 = "allowlist ", C1 = "DESCRIPTION", C2 = "NAMESPACE  ",
  C3 = "R/ runtime ", C4 = "tests      ", C5 = "man/       ",
  C6 = "static scan", C7 = "clean room "
)

print_findings <- function(findings, mode) {
  for (f in findings) {
    cat(sprintf("  %s  %-11s %-4s %s\n", f$id, LABELS[[f$id]],
                if (f$ok) "PASS" else "FAIL", f$detail))
  }
  ok <- all(vapply(findings, `[[`, logical(1), "ok"))
  if (identical(mode, "static-only")) {
    cat(sprintf("VERDICT %s (STATIC ONLY -- does NOT satisfy RCON-09; the",
                if (ok) "PARTIAL-PASS" else "FAIL"),
        "release criterion requires the full run's clean room)\n")
  } else {
    cat(sprintf("VERDICT %s\n", if (ok) "PASS" else "FAIL"))
  }
  ok
}

run_gate <- function(root = ".", mode = "full") {
  findings <- list(
    check_allowlist(root),
    check_description(root),
    check_namespace(root),
    scan_runtime(root, "R", "C3", "R/"),
    scan_runtime(root, "tests", "C4", "tests/"),
    check_man(root),
    check_static(root)
  )
  if (identical(mode, "full")) {
    findings <- c(findings, list(check_clean_room(root)))
  }
  findings
}

# ---- self-test --------------------------------------------------------------

# Fixtures are minimal package trees in tempdir(). C7 is excluded: it runs a
# real R CMD check, which is not a unit-test-scale operation. Its own false-
# green mode (a clean room that resolved the real curl) is covered by the
# explicit shim-resolution assertion inside check_clean_room().
self_test <- function() {
  st <- new.env()
  st$pass <- 0L
  st$fail <- character(0)
  expect <- function(what, ok) {
    if (isTRUE(ok)) {
      st$pass <- st$pass + 1L
    } else {
      st$fail <- c(st$fail, what)
    }
  }

  mk <- function(description, namespace = "export(get_host)\n",
                 r_code = "f <- function(x) x\n",
                 test_code = "test_that('x', expect_true(TRUE))\n",
                 rd = "\\name{f}\n") {
    root <- tempfile("curl-zero-fx-")
    dir.create(file.path(root, "R"), recursive = TRUE)
    dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
    dir.create(file.path(root, "man"), recursive = TRUE)
    writeLines(description, file.path(root, "DESCRIPTION"))
    writeLines(namespace, file.path(root, "NAMESPACE"))
    writeLines(r_code, file.path(root, "R", "code.R"))
    writeLines(test_code, file.path(root, "tests", "testthat", "test-a.R"))
    writeLines(rd, file.path(root, "man", "f.Rd"))
    root
  }

  clean_desc <- c("Package: fx", "Version: 1.0.0",
                  "Imports: stringi, utils")
  rule <- function(root, id) {
    fs <- run_gate(root, mode = "static-only")
    for (f in fs) if (identical(f$id, id)) return(f$ok)
    NA
  }

  # 1. A curl-free tree passes every static check.
  root <- mk(clean_desc)
  fs <- run_gate(root, mode = "static-only")
  expect("a curl-free tree passes C1-C6",
         all(vapply(fs[-1L], `[[`, logical(1), "ok")))

  # 2. C1 -- a declared dependency, in any field.
  for (f in c("Imports", "Suggests", "Depends", "Enhances")) {
    root <- mk(c("Package: fx", "Version: 1.0.0",
                 sprintf("%s: stringi, curl", f)))
    expect(sprintf("C1 fails on %s: curl", f),
           identical(rule(root, "C1"), FALSE))
  }

  # 3. C1 -- a version-pinned declaration still hits.
  root <- mk(c("Package: fx", "Version: 1.0.0", "Imports: curl (>= 5.0.0)"))
  expect("C1 fails on a version-pinned curl",
         identical(rule(root, "C1"), FALSE))

  # 4. C2 -- both import forms.
  root <- mk(clean_desc, namespace = "importFrom(curl,curl_parse_url)\n")
  expect("C2 fails on importFrom(curl, ...)",
         identical(rule(root, "C2"), FALSE))
  root <- mk(clean_desc, namespace = "import(curl)\n")
  expect("C2 fails on import(curl)", identical(rule(root, "C2"), FALSE))

  # 5. C3 -- a direct `::` call.
  root <- mk(clean_desc, r_code = "f <- function(x) curl::curl_escape(x)\n")
  expect("C3 fails on curl::", identical(rule(root, "C3"), FALSE))

  # 6. C3 -- the internal `:::` form.
  root <- mk(clean_desc, r_code = "f <- function(x) curl:::internal(x)\n")
  expect("C3 fails on curl:::", identical(rule(root, "C3"), FALSE))

  # 7. C3 -- indirect load by string, the form a `::` grep misses.
  for (fn in c("requireNamespace", "loadNamespace", "getNamespace",
               "asNamespace")) {
    root <- mk(clean_desc,
               r_code = sprintf("f <- function() %s('curl')\n", fn))
    expect(sprintf("C3 fails on %s('curl')", fn),
           identical(rule(root, "C3"), FALSE))
  }
  root <- mk(clean_desc, r_code = "f <- function() library(curl)\n")
  expect("C3 fails on library(curl)", identical(rule(root, "C3"), FALSE))

  # 8. C3 -- a COMMENT naming libcurl is history, not a reference. This is the
  # check that keeps the gate from punishing the prose that explains why the
  # dependency went away.
  root <- mk(clean_desc,
             r_code = "# was curl::curl_parse_url() once\nf <- function(x) x\n")
  expect("C3 ignores a curl mention in a comment",
         identical(rule(root, "C3"), TRUE))

  # 9. C6 -- but the static scan does NOT ignore it, unless allowlisted.
  expect("C6 fails on a curl mention in a comment",
         identical(rule(root, "C6"), FALSE))

  # 10. C4 -- tests are in scope.
  root <- mk(clean_desc,
             test_code = paste0("test_that('x', expect_true(",
                                "is.function(curl::curl_escape)))\n"))
  expect("C4 fails on curl in a test", identical(rule(root, "C4"), FALSE))

  # 11. C5 -- generated help.
  root <- mk(clean_desc, rd = "\\name{f}\n\\note{uses curl}\n")
  expect("C5 fails on curl in an Rd file", identical(rule(root, "C5"), FALSE))

  # Materialize EVERY allowlisted path in a fixture tree, each with (or
  # without) a curl hit. Driven off ALLOWLIST itself rather than a hardcoded
  # pair, so adding a row cannot silently break these two cases -- which is
  # exactly what it used to do. A row whose path has no extension is treated as
  # a directory, matching `check_allowlist()`'s dir/file handling.
  materialize_allowlist <- function(root, content) {
    for (a in ALLOWLIST) {
      p <- file.path(root, a$path)
      if (grepl("\\.[A-Za-z0-9]+$", basename(a$path))) {
        dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
        writeLines(content, p)
      } else {
        dir.create(p, recursive = TRUE, showWarnings = FALSE)
        writeLines(content, file.path(p, "d.R"))
      }
    }
  }

  # 12. C0 -- a stale allowlist row (path exists, no hit left) is a violation.
  root <- mk(clean_desc)
  materialize_allowlist(root, "x <- 1\n")
  expect("C0 fails when an allowlisted path has no curl left",
         identical(rule(root, "C0"), FALSE))

  # 13. C0 -- a live allowlist row passes, and C6 honors it.
  materialize_allowlist(root, "# measures libcurl versions\n")
  expect("C0 passes when every allowlisted path still has a hit",
         identical(rule(root, "C0"), TRUE))
  expect("C6 does not flag an allowlisted path",
         identical(rule(root, "C6"), TRUE))

  # 14. Missing DESCRIPTION / NAMESPACE fail closed rather than pass empty.
  root <- tempfile("curl-zero-empty-")
  dir.create(root)
  expect("C1 fails closed with no DESCRIPTION",
         identical(rule(root, "C1"), FALSE))
  expect("C2 fails closed with no NAMESPACE",
         identical(rule(root, "C2"), FALSE))

  # 16. The categorical exemption (RURL-vuxlhehz). A registry that registers
  # two fixture paths, shaped like the real record: a `governing_decision`
  # with its own `path` key BEFORE the array, and a nested `path` deep inside
  # a source group, neither of which may register anything. Every case below
  # runs through the same `rule()`, i.e. through `--static-only`, which is the
  # mode verify.yml runs; C6 is one function in both modes.
  fetch <- paste("curl -fsSL https://example.invalid/upstream.json",
                 "-o upstream.json")
  registry <- c(
    "{",
    '  "record_kind": "oracle-provenance",',
    '  "governing_decision": {',
    '    "path": "design/not-a-fixture.md"',
    "  },",
    '  "fixtures": [',
    "    {",
    '      "path": "inst/bench/reg-cases.json",',
    '      "source_groups": [',
    "        {",
    '          "group": "wpt",',
    sprintf('          "import_command": "%s",', fetch),
    '          "technique_references": [{ "path": "ip.py" }]',
    "        }",
    "      ]",
    "    },",
    '    { "path": "tests/testthat/fixtures/reg-relative.json" }',
    "  ]",
    "}"
  )
  fixture_with <- function(meta_lines) {
    c("{", '  "_meta": {', '    "upstream_project": "wpt/wpt",',
      meta_lines, '    "counts": { "success": 1 }', "  },",
      '  "success": [{ "input": "http://a/", "href": "http://a/" }]', "}")
  }
  provenance_tree <- function(registry_lines = registry) {
    root <- mk(clean_desc)
    dir.create(file.path(root, "tests", "testthat", "fixtures"),
               recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(root, "inst", "bench"), recursive = TRUE,
               showWarnings = FALSE)
    if (!is.null(registry_lines)) {
      writeLines(registry_lines, file.path(root, PROVENANCE_REGISTRY))
    }
    root
  }
  put <- function(root, rel, lines) {
    writeLines(lines, file.path(root, rel))
  }
  reg_paths <- registered_fixture_paths(provenance_tree())
  expect("the registry reader returns exactly the fixtures[].path values",
         identical(reg_paths, c("inst/bench/reg-cases.json",
                                "tests/testthat/fixtures/reg-relative.json")))

  # 16a. A registered fixture whose only curl is its import_command passes,
  # with no allowlist row naming the file -- the acceptance criterion.
  root <- provenance_tree()
  put(root, "inst/bench/reg-cases.json",
      fixture_with(sprintf('    "import_command": "%s",', fetch)))
  put(root, "tests/testthat/fixtures/reg-relative.json",
      fixture_with(sprintf('    "import_command": "%s",', fetch)))
  expect("C6 passes a registered fixture with curl in import_command",
         identical(rule(root, "C6"), TRUE))
  expect("no allowlist row names the registered fixtures",
         !any(c("inst/bench/reg-cases.json",
                "tests/testthat/fixtures/reg-relative.json") %in%
                allow_paths()))

  # 16b. The other command keys are provenance fields too.
  root <- provenance_tree()
  put(root, "inst/bench/reg-cases.json",
      fixture_with(c(sprintf('    "pin_fetch_command": "%s",', fetch),
                     '    "generation_command": "curl-free-generator.py",')))
  expect("C6 passes curl in pin_fetch_command / generation_command",
         identical(rule(root, "C6"), TRUE))

  # 16c. The SAME bytes at an unregistered path fail: the exemption is keyed
  # on registration, not on the shape of the file.
  root <- provenance_tree()
  put(root, "tests/testthat/fixtures/reg-relative-copy.json",
      fixture_with(sprintf('    "import_command": "%s",', fetch)))
  expect("C6 fails the same import_command in an unregistered copy",
         identical(rule(root, "C6"), FALSE))

  # 16d. ...and with no registry at all, nothing is registered.
  root <- provenance_tree(registry_lines = NULL)
  put(root, "inst/bench/reg-cases.json",
      fixture_with(sprintf('    "import_command": "%s",', fetch)))
  expect("C6 fails closed when the registry is absent",
         identical(rule(root, "C6"), FALSE))

  # 16e. A registered fixture with curl OUTSIDE a provenance field fails, in
  # each of the positions a stray reference could take: a narrative `_meta`
  # key, a data row, and a hit that shares the file with a legitimate one.
  root <- provenance_tree()
  put(root, "inst/bench/reg-cases.json",
      fixture_with('    "note": "compared against curl 8.x",'))
  expect("C6 fails a registered fixture with curl in a non-command _meta key",
         identical(rule(root, "C6"), FALSE))
  root <- provenance_tree()
  put(root, "inst/bench/reg-cases.json",
      c("{", '  "_meta": { "import_command": "python3 make.py" },',
        '  "success": [{ "input": "curl://a/", "href": "curl://a/" }]', "}"))
  expect("C6 fails a registered fixture with curl in a data row",
         identical(rule(root, "C6"), FALSE))
  root <- provenance_tree()
  put(root, "inst/bench/reg-cases.json",
      fixture_with(c(sprintf('    "import_command": "%s",', fetch),
                     '    "license": "same terms as libcurl",')))
  fs <- run_gate(root, mode = "static-only")
  c6 <- Filter(function(f) identical(f$id, "C6"), fs)[[1L]]
  expect("C6 fails a registered fixture with one live hit beside an inert one",
         identical(c6$ok, FALSE) &&
           grepl("outside a provenance field at line", c6$detail, fixed = TRUE))

  # 16f. The line predicate itself: a command key whose value is not a single
  # JSON string, or that shares its line with anything else, is not inert.
  expect("a provenance command line is recognised, with escapes and a comma",
         identical(is_provenance_command_line(c(
           sprintf('  "import_command": "%s",', fetch),
           '"generation_command": "say \\"curl\\" -o x"',
           '  "import_command": ["curl", "-o", "x"],',
           '  "import_command": "curl -o x", "note": "curl"',
           '  "import_command_note": "run curl by hand"'
         )), c(TRUE, TRUE, FALSE, FALSE, FALSE)))

  # 16g. Registration is the fixtures[].path key only: the decision's `path`
  # and a source group's nested `path` register nothing.
  expect("a governing_decision path does not register",
         !"design/not-a-fixture.md" %in% reg_paths)
  expect("a nested source-group path does not register",
         !"ip.py" %in% reg_paths)

  # 15. C7 log judging (RURL-pdrrmfmu). The full C7 runs a real R CMD check and
  # so stays out of the self-test, but the TOLERANCE is pure and is exercised
  # here -- which is what made the blind spot findable in the first place. The
  # artifact block below is the verbatim log the clean room produces.
  artifact_log <- c(
    "* checking installed package size ... OK",
    "* checking files in ‘vignettes’ ... WARNING",
    "Files in the 'vignettes' directory but no files in 'inst/doc':",
    "  ‘getting-started.Rmd’ ‘url-standard.Rmd’",
    "* checking examples ... OK",
    "* checking package vignettes ... WARNING",
    "Directory 'inst/doc' does not exist.",
    "Package vignettes without corresponding single PDF/HTML:",
    "  ‘getting-started.Rmd’",
    "  ‘url-standard.Rmd’",
    "* checking running R code from vignettes ... OK",
    "* DONE",
    "Status: 2 WARNINGs"
  )
  expect("C7 tolerates the clean room's own two vignette artifacts",
         identical(clean_room_findings(artifact_log), character(0)))

  # The measured blind spot: an extra finding filed UNDER a tolerated heading.
  leftover_log <- append(
    artifact_log,
    c("The following files look like leftovers/mistakes:",
      "  ‘leftover.log’"),
    after = 4L
  )
  expect("C7 catches a leftover reported under a tolerated heading",
         any(grepl("leftovers", clean_room_findings(leftover_log),
                   fixed = TRUE)))

  # ...and the same file named only in the list, with no new sentence: the
  # list lines are held to vignette source extensions for exactly this case.
  listed_log <- artifact_log
  listed_log[4L] <- "  ‘getting-started.Rmd’ ‘leftover.log’"
  expect("C7 catches a non-vignette file named in the artifact list",
         any(grepl("leftover", clean_room_findings(listed_log),
                   fixed = TRUE)))

  # Unrelated findings under other headings still count, at either severity.
  expect("C7 catches an unrelated WARNING",
         length(clean_room_findings(c(
           "* checking for missing documentation entries ... WARNING",
           "Undocumented code objects:", "  ‘g’",
           "Status: 1 WARNING"
         ))) > 0L)
  expect("C7 catches an ERROR",
         length(clean_room_findings(c(
           "* checking whether the package can be loaded ... ERROR",
           "Status: 1 ERROR"
         ))) > 0L)
  # A wholly clean log stays clean, and the summary line alone is not a finding.
  expect("C7 passes a clean log",
         identical(clean_room_findings(c("* checking tests ... OK", "* DONE",
                                         "Status: OK")), character(0)))

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("curl-zero-gate self-test: FAIL", call. = FALSE)
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
  mode <- if ("--static-only" %in% args) "static-only" else "full"
  cat(sprintf("curl zero-reference gate (RURL-cunfohwy), mode: %s\n", mode))
  ok <- print_findings(run_gate(".", mode), mode)
  if (!ok) {
    stop("curl zero-reference gate: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
