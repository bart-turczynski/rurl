#!/usr/bin/env Rscript

# substring() explicit-`last` gate (RURL-hxeaefzp).
#
# WHY. Before R 4.6.0, base `substring(text, first, last = 1000000L)` defaulted
# `last` to one million, so a two-argument suffix cut such as
# `substring(path, 2L)` silently dropped everything past character 1e6. R 4.6.0
# changed the default to `last = NULL`, "to the end of string" (R NEWS 4.6.0,
# PR#18851), so on a current R the truncation cannot be reproduced at all --
# and DESCRIPTION still admits R (>= 4.1.0), where it can. A URL path, query,
# fragment or userinfo can be that long. ADR 0005's Consequences carry the rule:
# a base-R suffix cut passes `last` explicitly (`.Machine$integer.max`) or uses
# `.byte_substring()`.
#
# Because the defect is invisible on the R this repository's gates run, a value
# test cannot pin it; this static check is the pin. It has NO exceptions list on
# purpose: a call whose `text` is provably short (a scheme, a fixed token) still
# passes `last`, so nothing here needs maintaining as code moves.
#
# WHAT IT CHECKS. Every `substring(...)` call in R/ -- plain, `base::`-
# qualified, or as the replacement form `substring(x, i) <- v`, whose default
# was the same -- must supply `last`, by name or as the third positional
# argument. The call is found by PARSING, not by a regex: a regex misses calls
# whose arguments contain parentheses or span lines. Each call's arguments are
# matched against substring()'s own formals with `match.call()`, so partial
# names and positional forms are judged the way R judges them. A call that
# cannot be matched (one forwarding `...`) cannot be cleared and fails. So does
# a reference to `substring` as a value (`Map(substring, x, i)`), whose `last`
# the gate cannot see, and a file that does not parse. `substr()` is not
# checked: its `stop` has no default.
#
# Scope is R/ only: the shipped code. Tests and tools run on the R the
# developer has, and a tests/ scan would see peer worktrees' files.
#
# Zero dependencies beyond base R. Deterministic and network-free.
#
# Usage:
#   Rscript tools/substring-last-gate.R             # scan R/, exit 1 on a hit
#   Rscript tools/substring-last-gate.R --self-test # positive/negative fixtures

# The formals every call is matched against. Written out rather than read from
# `base::substring`, so the verdict does not depend on which R runs the gate
# (R 4.6.0 changed the default of `last`, not its name or position).
SUBSTRING_FORMALS <- function(text, first, last) NULL

# ---- the scan ---------------------------------------------------------------

r_files <- function(dir) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  sort(list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE))
}

# Does one parsed `substring(...)` call supply `last`? TRUE, FALSE, or NA when
# the call cannot be matched (a forwarded `...`).
call_has_last <- function(call) {
  if (any(vapply(as.list(call)[-1L], identical, logical(1),
                 quote(...)))) {
    return(NA)
  }
  matched <- tryCatch(match.call(SUBSTRING_FORMALS, call),
                      error = function(e) NULL)
  if (is.null(matched)) {
    return(NA)
  }
  "last" %in% names(matched)
}

# Every offending `substring` site in one file, as "<rel>:<line>: <reason>".
file_hits <- function(path, rel) {
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) {
    return(sprintf("%s: does not parse, so it cannot be cleared (%s)", rel,
                   conditionMessage(exprs)))
  }
  pd <- utils::getParseData(exprs, includeText = TRUE)
  if (is.null(pd) || !nrow(pd)) {
    return(character(0))
  }
  hits <- character(0)

  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "substring", ]
  for (i in seq_len(nrow(calls))) {
    fn_expr <- calls$parent[i]
    call_id <- pd$parent[pd$id == fn_expr]
    src <- pd$text[pd$id == call_id]
    call <- tryCatch(str2lang(src), error = function(e) NULL)
    ok <- if (is.null(call)) NA else call_has_last(call)
    if (!isTRUE(ok)) {
      reason <- if (is.na(ok)) {
        "arguments cannot be matched, so `last` cannot be confirmed"
      } else {
        "no explicit `last`"
      }
      hits <- c(hits, sprintf("%s:%d: %s: %s", rel, calls$line1[i], reason,
                              gsub("\\s+", " ", src)))
    }
  }

  refs <- pd[pd$token == "SYMBOL" & pd$text == "substring", ]
  for (i in seq_len(nrow(refs))) {
    hits <- c(hits, sprintf(
      "%s:%d: `substring` used as a value, so its `last` cannot be checked",
      rel, refs$line1[i]
    ))
  }
  hits
}

run_gate <- function(root = ".") {
  files <- r_files(file.path(root, "R"))
  hits <- character(0)
  for (f in files) {
    rel <- sub(paste0("^", root, "/?"), "", f)
    hits <- c(hits, file_hits(f, rel))
  }
  list(files = length(files), hits = hits)
}

# ---- self-test --------------------------------------------------------------

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
  tree <- function(code) {
    root <- tempfile("substring-last-fx-")
    dir.create(file.path(root, "R"), recursive = TRUE)
    writeLines(code, file.path(root, "R", "code.R"))
    root
  }
  n_hits <- function(code) length(run_gate(tree(code))$hits)

  # Passing forms: `last` by name, by partial name, by position, and on the
  # replacement form; a three-argument call spread over lines.
  passing <- c(
    "f <- function(x) substring(x, 2L, .Machine$integer.max)",
    "f <- function(x) substring(x, 2L, last = .Machine$integer.max)",
    "f <- function(x) substring(first = 2L, x, last = nchar(x))",
    "f <- function(x) substring(x, 2L, la = 5L)",
    "f <- function(x) base::substring(x, 2L, 5L)",
    "f <- function(x) { substring(x, 1L, 1L) <- 'a'; x }",
    "f <- function(x) substring(\n  x,\n  nchar(x) - 1L,\n  nchar(x)\n)",
    "f <- function(x) substr(x, 2L, 3L)",
    "# substring(x, 2L) in a comment is not a call\nf <- function(x) x",
    "f <- function(x) 'substring(x, 2L) in a string is not a call'"
  )
  for (code in passing) {
    expect(sprintf("passes: %s", gsub("\n", " ", code, fixed = TRUE)),
           identical(n_hits(code), 0L))
  }

  # Failing forms: every two-argument shape, including the ones a one-line
  # regex misses (nested parentheses, a call over several lines).
  failing <- c(
    "f <- function(x) substring(x, 2L)",
    "f <- function(x) substring(text = x, first = 2L)",
    "f <- function(x) base::substring(x, 2L)",
    "f <- function(x, i) substring(x, max(i, 1L) + nchar(sub('a', '', x)))",
    "f <- function(x) substring(\n  x,\n  2L\n)",
    "f <- function(x) { substring(x, 2L) <- 'a'; x }",
    "f <- function(x, ...) substring(x, ...)",
    "f <- function(x, i) Map(substring, x, i)"
  )
  for (code in failing) {
    expect(sprintf("fails: %s", gsub("\n", " ", code, fixed = TRUE)),
           identical(n_hits(code), 1L))
  }

  # Every offender is listed, with its line, not only the first.
  res <- run_gate(tree(c("a <- function(x) substring(x, 2L)",
                         "b <- function(x) substring(x, 3L, 4L)",
                         "c <- function(x) substring(x, 5L)")))
  expect("lists every offender", length(res$hits) == 2L)
  expect("names file and line",
         identical(sub(":.*$", "", sub("^R/code\\.R:", "", res$hits)),
                   c("1", "3")))

  # An unparseable file fails closed rather than passing empty.
  expect("an unparseable file fails", identical(n_hits("f <- function("), 1L))

  # A tree with no R/ has nothing to clear, and says how many files it read.
  empty <- tempfile("substring-last-empty-")
  dir.create(empty)
  expect("no R/ reads zero files",
         identical(run_gate(empty)$files, 0L) &&
           identical(run_gate(empty)$hits, character(0)))

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("substring-last-gate self-test: FAIL", call. = FALSE)
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
  cat("substring() explicit-last gate (RURL-hxeaefzp)\n")
  res <- run_gate(".")
  if (length(res$hits)) {
    cat(sprintf("%d substring() site(s) without an explicit `last`",
                length(res$hits)),
        "(ADR 0005: pass .Machine$integer.max, or use .byte_substring()):\n")
    for (h in res$hits) cat(sprintf("  %s\n", h))
    cat("VERDICT FAIL\n")
    stop("substring-last-gate: FAIL", call. = FALSE)
  }
  cat(sprintf("every substring() call in R/ passes `last` (%d file(s))\n",
              res$files))
  cat("VERDICT PASS\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
