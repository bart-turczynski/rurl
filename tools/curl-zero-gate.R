#!/usr/bin/env Rscript

# curl zero-reference gate (RURL-cunfohwy).
#
# This is the executable form of the v3 line's CRAN release criterion from P0.4
# ("The v2/v3 boundary (concrete)") + RCON-09 section 4 + S8 gates 2-3/8: the
# v3 line does not ship while rurl depends on curl in any way -- declared,
# imported, called, loaded by string, cross-referenced from its help, or loaded
# at all during build and check.
#
# It VERIFIES; it decides nothing. It does not judge whether dropping libcurl
# was right, and it grants no coverage to any other gate.
#
# WHAT IT DOES NOT POLICE: THE WORD (RURL-dtzvekmf). curl went because rurl
# grew its own parser; the goal was no curl DEPENDENCY, never a ban on naming
# it. A comment, a NEWS entry, a design note, a fixture's provenance command or
# a WORDLIST entry that says "curl" or "libcurl" is not a dependency, and the
# in-tree replacements have to cite the libcurl behaviour they reproduce to stay
# auditable. The raw-text scan that used to police the word (C6), its allowlist
# and the allowlist's own staleness check (C0) were retired for that reason.
# The remaining checks keep their numbers, so C7 is still C7 in tools/verify.R.
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
# WHY IT PARSES AND DOES NOT GREP. C3/C4 strip comments by PARSING, and C5
# reads only the Rd constructs that reach another package, so prose naming
# libcurl never fails the gate.
#
# THE CHECKS
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
#   C5  man        -- no cross-reference into curl in generated help
#                     (`\link[curl]{...}`, which R CMD check resolves against
#                     an installed curl), and no curl call in an Rd's
#                     \examples or \usage. Prose in an Rd is not checked.
#   C7  clean room -- build + R CMD check against a temporary library whose
#                     `curl` is poisoned. Runs only in full mode.
#
# Zero dependencies beyond base R. Deterministic. C1-C5 are network-free; C7
# runs R CMD check, which is not.
#
# Usage:
#   Rscript tools/curl-zero-gate.R               # full: C1-C7, exit 1 on any
#   Rscript tools/curl-zero-gate.R --static-only # C1-C5; VERDICT PARTIAL
#   Rscript tools/curl-zero-gate.R --self-test   # positive/negative fixtures
#
# `--static-only` never satisfies the release criterion, and says so in its
# verdict line: only the full run closes RCON-09.

DEP_FIELDS <- c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances",
                "Remotes")

# The message the poisoned shim dies with, and the string C7 looks for in the
# check transcript. One constant, so the two cannot drift apart.
CLEAN_ROOM_SIGNAL <- "CLEAN-ROOM VIOLATION"

# The token C1 looks for in a dependency field, matched case-insensitively so
# `curl` and `RCurl` both hit. Over-matching is intentional inside a dependency
# field: any package with curl in its name is a curl dependency by another
# route.
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

# Runtime curl references in lines of code. Returns the matching lines.
runtime_lines <- function(txt) {
  direct <- grepl("\\bcurl:{2,3}", txt)
  indirect <- grepl(
    sprintf("\\b(%s)\\s*\\(\\s*[\"']?curl[\"']?",
            paste(INDIRECT_FNS, collapse = "|")),
    txt
  )
  txt[direct | indirect]
}

# Runtime curl references in one file's parsed code.
runtime_hits <- function(path) {
  runtime_lines(code_text(path))
}

# ---- the checks -------------------------------------------------------------

finding <- function(id, ok, detail) {
  list(id = id, ok = ok, detail = detail)
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

# The curl dependencies one Rd file carries: a cross-package link into curl,
# which R CMD check resolves against an installed curl, or a curl call in the
# code sections (\examples runs, \usage is code). Prose anywhere else in the
# file is not a dependency and is not read. An Rd that will not parse cannot
# be cleared, so its raw lines are scanned for the same forms instead.
rd_hits <- function(path) {
  lines <- readLines(path, warn = FALSE)
  link <- lines[grepl("\\\\link\\[curl[]:]", lines)]
  rd <- tryCatch(tools::parse_Rd(path), error = function(e) NULL)
  code <- if (is.null(rd)) {
    lines
  } else {
    tags <- vapply(rd, function(x) {
      tag <- attr(x, "Rd_tag")
      if (is.null(tag)) "" else tag
    }, character(1))
    unlist(lapply(rd[tags %in% c("\\examples", "\\usage")], function(x) {
      strsplit(paste(unlist(x), collapse = ""), "\n", fixed = TRUE)[[1L]]
    }), use.names = FALSE)
  }
  c(link, runtime_lines(code))
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
    hits <- rd_hits(f)
    if (length(hits)) {
      bad <- c(bad, sprintf("%s: %s", basename(f), trimws(hits[1L])))
    }
  }
  finding("C5", length(bad) == 0L,
          if (length(bad)) sprintf("%d Rd file(s): %s", length(bad),
                                   paste(utils::head(bad, 5L),
                                         collapse = " | "))
          else sprintf("no curl link or example call in %d Rd file(s)",
                       length(files)))
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
    sprintf("  stop('%s: curl was loaded', call. = FALSE)",
            CLEAN_ROOM_SIGNAL),
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
  log_lines <- if (file.exists(log)) readLines(log, warn = FALSE) else NULL
  clean_room_verdict(out, attr(out, "status"), log_lines)
}

# The C7 finding for one R CMD check run: its transcript, its exit status and
# its 00check.log (NULL when the check died before writing one). Pure, so the
# self-test can prove that a curl load fails C7 without running a real check.
clean_room_verdict <- function(out, status, log_lines) {
  violated <- any(grepl(CLEAN_ROOM_SIGNAL, out, fixed = TRUE))
  errors <- if (is.null(log_lines)) {
    character(0)
  } else {
    clean_room_findings(log_lines)
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
  C1 = "DESCRIPTION", C2 = "NAMESPACE  ", C3 = "R/ runtime ",
  C4 = "tests      ", C5 = "man/       ", C7 = "clean room "
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
    check_description(root),
    check_namespace(root),
    scan_runtime(root, "R", "C3", "R/"),
    scan_runtime(root, "tests", "C4", "tests/"),
    check_man(root)
  )
  if (identical(mode, "full")) {
    findings <- c(findings, list(check_clean_room(root)))
  }
  findings
}

# ---- self-test --------------------------------------------------------------

# Fixtures are minimal package trees in tempdir(). The full C7 is excluded: it
# runs a real R CMD check, which is not a unit-test-scale operation. Its verdict
# is pure (`clean_room_verdict()`) and is exercised below; its own false-green
# mode (a clean room that resolved the real curl) is covered by the explicit
# shim-resolution assertion inside check_clean_room().
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
  put <- function(root, rel, lines) {
    dir.create(dirname(file.path(root, rel)), recursive = TRUE,
               showWarnings = FALSE)
    writeLines(lines, file.path(root, rel))
  }

  clean_desc <- c("Package: fx", "Version: 1.0.0",
                  "Imports: stringi, utils")
  all_pass <- function(root) {
    fs <- run_gate(root, mode = "static-only")
    all(vapply(fs, `[[`, logical(1), "ok"))
  }
  rule <- function(root, id) {
    fs <- run_gate(root, mode = "static-only")
    for (f in fs) if (identical(f$id, id)) return(f$ok)
    NA
  }

  # 1. A curl-free tree passes every static check, and they are C1-C5 only.
  root <- mk(clean_desc)
  expect("a curl-free tree passes C1-C5", all_pass(root))
  expect("the static checks are exactly C1-C5",
         identical(vapply(run_gate(root, mode = "static-only"), `[[`,
                          character(1), "id"),
                   c("C1", "C2", "C3", "C4", "C5")))

  # 2. THE WORD IS NOT A DEPENDENCY (RURL-dtzvekmf). Every place prose names
  # curl or libcurl, in one tree, passes the whole static gate: a comment in
  # R/ and in tests/, NEWS, a design note, a fixture's provenance command and
  # a narrative _meta field, the spelling dictionary, a tool's comment, and
  # prose in an Rd file.
  root <- mk(clean_desc,
             r_code = "# libcurl did X; rurl does Y\nf <- function(x) x\n",
             test_code = paste0("# frozen from curl::curl_parse_url()\n",
                                "test_that('x', expect_true(TRUE))\n"),
             rd = c("\\name{f}",
                    "\\details{Replaces \\code{curl::curl_escape()}.}"))
  put(root, "NEWS.md", "- Dropped the curl dependency; libcurl is gone.")
  put(root, "design/why.md", "curl::curl_parse_url() coerced hosts.")
  put(root, "tests/testthat/fixtures/f.json", c(
    "{", '  "_meta": {',
    '    "import_command": "curl -fsSL https://x.invalid/a.json -o a.json",',
    '    "note": "compared against curl 8.x"', "  }", "}"))
  put(root, "inst/WORDLIST", c("libcurl", "urltools"))
  put(root, "tools/x.R", "# measures libcurl versions\nx <- 1\n")
  expect("prose naming curl, anywhere in the tree, passes the static gate",
         all_pass(root))

  # 3. C1 -- a declared dependency, in any field.
  for (f in DEP_FIELDS) {
    root <- mk(c("Package: fx", "Version: 1.0.0",
                 sprintf("%s: stringi, curl", f)))
    expect(sprintf("C1 fails on %s: curl", f),
           identical(rule(root, "C1"), FALSE))
  }

  # 4. C1 -- a version-pinned declaration still hits.
  root <- mk(c("Package: fx", "Version: 1.0.0", "Imports: curl (>= 5.0.0)"))
  expect("C1 fails on a version-pinned curl",
         identical(rule(root, "C1"), FALSE))

  # 5. C2 -- both import forms.
  root <- mk(clean_desc, namespace = "importFrom(curl,curl_parse_url)\n")
  expect("C2 fails on importFrom(curl, ...)",
         identical(rule(root, "C2"), FALSE))
  root <- mk(clean_desc, namespace = "import(curl)\n")
  expect("C2 fails on import(curl)", identical(rule(root, "C2"), FALSE))

  # 6. C3 and C4 -- every runtime form, in R/ and in tests/ alike. The
  # indirect string forms are the ones a `::` grep misses.
  forms <- c("curl::curl_escape(x)", "curl:::internal(x)", "library(curl)",
             "require(curl)", sprintf("%s('curl')", INDIRECT_FNS),
             "getExportedValue('curl', 'curl_escape')")
  for (form in forms) {
    code <- sprintf("f <- function(x) %s\n", form)
    root <- mk(clean_desc, r_code = code)
    expect(sprintf("C3 fails on %s in R/", form),
           identical(rule(root, "C3"), FALSE))
    root <- mk(clean_desc,
               test_code = sprintf("test_that('x', {%s})\n", form))
    expect(sprintf("C4 fails on %s in tests/", form),
           identical(rule(root, "C4"), FALSE))
  }

  # 7. C3 -- a COMMENT naming curl is history, not a reference.
  root <- mk(clean_desc,
             r_code = "# was curl::curl_parse_url() once\nf <- function(x) x\n")
  expect("C3 ignores a curl mention in a comment",
         identical(rule(root, "C3"), TRUE))

  # 8. C5 -- the Rd leaks, and only those. Prose passes; a link into curl and a
  # curl call in \examples or \usage fail.
  root <- mk(clean_desc, rd = c("\\name{f}", "\\note{uses libcurl rules}"))
  expect("C5 passes curl named in Rd prose",
         identical(rule(root, "C5"), TRUE))
  root <- mk(clean_desc,
             rd = c("\\name{f}", "\\seealso{\\link[curl]{curl_escape}}"))
  expect("C5 fails on \\link[curl]{...}", identical(rule(root, "C5"), FALSE))
  root <- mk(clean_desc,
             rd = c("\\name{f}",
                    "\\seealso{\\link[curl:curl_escape]{escape}}"))
  expect("C5 fails on \\link[curl:topic]{...}",
         identical(rule(root, "C5"), FALSE))
  root <- mk(clean_desc,
             rd = c("\\name{f}", "\\examples{", "curl::curl_escape('a b')",
                    "}"))
  expect("C5 fails on a curl call in \\examples",
         identical(rule(root, "C5"), FALSE))
  root <- mk(clean_desc,
             rd = c("\\name{f}", "\\examples{", "requireNamespace('curl')",
                    "}"))
  expect("C5 fails on an indirect curl load in \\examples",
         identical(rule(root, "C5"), FALSE))

  # 9. Missing DESCRIPTION / NAMESPACE fail closed rather than pass empty.
  root <- tempfile("curl-zero-empty-")
  dir.create(root)
  expect("C1 fails closed with no DESCRIPTION",
         identical(rule(root, "C1"), FALSE))
  expect("C2 fails closed with no NAMESPACE",
         identical(rule(root, "C2"), FALSE))

  # 10. C7 -- a curl load during build or check fails the clean room, even when
  # the check itself exits 0 and its log is clean: the shim's signal alone is
  # the verdict.
  clean_log <- c("* checking tests ... OK", "* DONE", "Status: OK")
  loaded <- c("* installing *source* package 'fx' ...",
              sprintf("Error: %s: curl was loaded", CLEAN_ROOM_SIGNAL))
  expect("C7 fails when curl was loaded during the check",
         identical(clean_room_verdict(loaded, NULL, clean_log)$ok, FALSE))
  expect("C7 fails when curl was loaded before 00check.log existed",
         identical(clean_room_verdict(loaded, 1L, NULL)$ok, FALSE))
  expect("C7 passes a clean run",
         identical(clean_room_verdict("* DONE", NULL, clean_log)$ok, TRUE))
  expect("C7 fails a check that exited non-zero with no log",
         identical(clean_room_verdict("boom", 1L, NULL)$ok, FALSE))

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
