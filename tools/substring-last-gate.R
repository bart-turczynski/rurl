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
# a base-R suffix cut passes `last` explicitly (`.Machine$integer.max`), as a
# `.bytes_cut()` call does.
#
# Because the defect is invisible on the R this repository's gates run, a value
# test cannot pin it; this static check is the pin. It has NO exceptions list on
# purpose: a call whose `text` is provably short (a scheme, a fixed token) still
# passes `last`, so nothing here needs maintaining as code moves.
#
# WHAT IT CHECKS. Every call in R/ to `substring` or `substring<-` -- plain,
# backquoted, quoted (`'substring'(x, i)`), `base::`-qualified, as the
# replacement form `substring(x, i) <- v`, or on the right of a native pipe
# (`x |> substring(i, ...)`, whose left side is the first argument) -- must
# supply `last`, by name or as the third positional argument. The call is found
# by PARSING, not by a regex: a regex misses calls whose arguments contain
# parentheses or span lines. Each call's arguments are matched against the
# function's own formals with `match.call()`, so partial names and positional
# forms are judged the way R judges them. A call that forwards `...` passes
# only when it names `last`; otherwise its `last` cannot be confirmed and it
# fails.
#
# `last` must not be a constant other than 2147483647 (`.Machine$integer.max`):
# a smaller one, such as `1000000L` or `1e6`, cuts as the old default did, a
# larger one or `NA` gives NA, and `NULL` errors before R 4.6.0. A fixed-width
# cut uses `substr()`. Any other expression (`.Machine$integer.max`,
# `nchar(x)`, a variable) passes: its value is not knowable here.
#
# The gate fails closed on what it cannot see: a reference to the function as
# a value (`Map(substring, x, i)`), a string naming it anywhere but in call
# position (`do.call("substring", ...)`, `match.fun("substring<-")`,
# `vapply(x, "substring", "", i)`), and a file that does not parse.
# `substr()` is not checked: its `stop` has no default.
#
# Scope is R/ only: the shipped code. Tests and tools run on the R the
# developer has, and a tests/ scan would see peer worktrees' files.
#
# Zero dependencies beyond base R. Deterministic and network-free.
#
# Usage:
#   Rscript tools/substring-last-gate.R             # scan R/, exit 1 on a hit
#   Rscript tools/substring-last-gate.R --self-test # positive/negative fixtures

# The formals every call is matched against, by function name. Written out
# rather than read from `base`, so the verdict does not depend on which R runs
# the gate (R 4.6.0 changed the default of `last`, not its name or position).
CHECKED_FORMALS <- list(
  "substring" = function(text, first, last) NULL,
  "substring<-" = function(text, first, last, value) NULL
)
CHECKED_NAMES <- names(CHECKED_FORMALS)

# The only constant `last` that keeps a whole string: `.Machine$integer.max`.
LAST_MAX <- 2147483647

# ---- the scan ---------------------------------------------------------------

r_files <- function(dir) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  sort(list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE))
}

# A symbol token's name without its backquotes: `substring<-` -> substring<-.
bare_name <- function(text) {
  sub("^`(.*)`$", "\\1", text)
}

# The value of a STR_CONST token (quoted or raw), or NA.
string_value <- function(text) {
  v <- tryCatch(str2lang(text), error = function(e) NULL)
  if (is.character(v) && length(v) == 1L) v else NA_character_
}

# Is `e` a constant? Parentheses and unary signs are looked through, so
# `-1L` and `(5L)` count. Returns list(constant = TRUE/FALSE, value).
constant_last <- function(e) {
  sign <- 1
  repeat {
    if (is.call(e) && length(e) == 2L &&
          (identical(e[[1L]], as.name("(")) ||
             identical(e[[1L]], as.name("-")) ||
             identical(e[[1L]], as.name("+")))) {
      if (identical(e[[1L]], as.name("-"))) sign <- -sign
      e <- e[[2L]]
      next
    }
    break
  }
  if (is.null(e) || (is.atomic(e) && length(e) == 1L)) {
    list(constant = TRUE, value = if (is.numeric(e)) sign * e else e)
  } else {
    list(constant = FALSE, value = NULL)
  }
}

# What is wrong with one parsed call to `fn` (a name in CHECKED_NAMES)?
# NULL when it supplies an acceptable `last`, else the reason.
call_problem <- function(call, fn) {
  fmls <- CHECKED_FORMALS[[fn]]
  args <- as.list(call)[-1L]
  dots <- vapply(args, identical, logical(1), quote(...))
  if (any(dots)) {
    # `...` may hold any argument, so only a NAMED `last` is certain.
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    hit <- pmatch(nms[!dots & nzchar(nms)], names(formals(fmls)),
                  duplicates.ok = TRUE)
    if (!any(names(formals(fmls))[hit] == "last", na.rm = TRUE)) {
      return(paste("forwards `...` without naming `last`, so `last` cannot",
                   "be confirmed"))
    }
    call <- call[c(TRUE, !dots)]
  }
  matched <- tryCatch(match.call(fmls, call), error = function(e) NULL)
  if (is.null(matched)) {
    return("arguments cannot be matched, so `last` cannot be confirmed")
  }
  if (!("last" %in% names(matched))) {
    return("no explicit `last`")
  }
  last <- constant_last(matched[["last"]])
  if (last$constant) {
    v <- last$value
    if (!(is.numeric(v) && !is.na(v) && v >= LAST_MAX && v < LAST_MAX + 1)) {
      return(sprintf(paste(
        "`last` is the constant %s, which cuts short, gives NA or (NULL)",
        "errors before R 4.6.0; pass .Machine$integer.max, or use substr()",
        "for a fixed-width cut"
      ), deparse1(matched[["last"]])))
    }
  }
  NULL
}

# The source of the call whose expression id is `call_id`. On the right of a
# native pipe, the pipe expression is returned instead: parsing it puts the
# left side in as the first argument, as R does.
call_source <- function(pd, call_id) {
  up <- pd$parent[pd$id == call_id]
  if (length(up) == 1L && up != 0L) {
    sib <- pd[pd$parent == up, ]
    sib <- sib[order(sib$line1, sib$col1), ]
    pipe <- which(sib$token == "PIPE")
    if (length(pipe) && which(sib$id == call_id) > pipe[1L]) {
      return(pd$text[pd$id == up])
    }
  }
  pd$text[pd$id == call_id]
}

# Is the expression `fn_expr` the function slot of a call (followed by `(`)?
in_call_position <- function(pd, fn_expr) {
  up <- pd$parent[pd$id == fn_expr]
  if (length(up) != 1L || up == 0L) {
    return(FALSE)
  }
  sib <- pd[pd$parent == up, ]
  sib <- sib[order(sib$line1, sib$col1), ]
  nrow(sib) >= 2L && sib$id[1L] == fn_expr && sib$token[2L] == "'('"
}

# Every offending site in one file, as "<rel>:<line>: <reason>".
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
  acc <- new.env()
  acc$hits <- character(0)
  hit <- function(line, reason, src = NULL) {
    acc$hits <- c(acc$hits, if (is.null(src)) {
      sprintf("%s:%d: %s", rel, line, reason)
    } else {
      sprintf("%s:%d: %s: %s", rel, line, reason, gsub("\\s+", " ", src))
    })
  }
  check_call <- function(fn_expr, fn, line) {
    src <- call_source(pd, pd$parent[pd$id == fn_expr])
    call <- tryCatch(str2lang(src), error = function(e) NULL)
    problem <- if (is.null(call)) {
      "the call does not parse alone, so `last` cannot be confirmed"
    } else {
      call_problem(call, fn)
    }
    if (!is.null(problem)) hit(line, problem, src)
  }

  # Calls by name: plain, backquoted, `base::`-qualified, replacement form.
  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" &
                bare_name(pd$text) %in% CHECKED_NAMES, ]
  for (i in seq_len(nrow(calls))) {
    check_call(calls$parent[i], bare_name(calls$text[i]), calls$line1[i])
  }

  # A string naming the function: a call when it heads one ('substring'(x)),
  # otherwise a use the gate cannot follow (do.call, match.fun, *apply).
  strs <- pd[pd$token == "STR_CONST", ]
  strs <- strs[vapply(strs$text, string_value, character(1)) %in%
                 CHECKED_NAMES, ]
  for (i in seq_len(nrow(strs))) {
    fn <- string_value(strs$text[i])
    if (in_call_position(pd, strs$parent[i])) {
      check_call(strs$parent[i], fn, strs$line1[i])
    } else {
      hit(strs$line1[i], sprintf(paste(
        "the string %s names `%s`, so the call it makes and its `last`",
        "cannot be checked; call it directly"
      ), strs$text[i], fn))
    }
  }

  # The function as a value: Map(substring, ...), Map(`substring<-`, ...).
  refs <- pd[pd$token == "SYMBOL" & bare_name(pd$text) %in% CHECKED_NAMES, ]
  for (i in seq_len(nrow(refs))) {
    hit(refs$line1[i], sprintf(
      "`%s` used as a value, so its `last` cannot be checked",
      bare_name(refs$text[i])
    ))
  }
  acc$hits
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

# Each fixture is one R/ file. A passing one yields no hit; a failing one
# yields exactly one.
FIXTURES_PASS <- c(
  # `last` by name, by partial name, by position; a call over several lines.
  "f <- function(x) substring(x, 2L, .Machine$integer.max)",
  "f <- function(x) substring(x, 2L, last = .Machine$integer.max)",
  "f <- function(x) substring(first = 2L, x, last = nchar(x))",
  "f <- function(x) substring(x, 2L, la = nchar(x))",
  "f <- function(x) base::substring(x, 2L, nchar(x))",
  "f <- function(x) substring(\n  x,\n  nchar(x) - 1L,\n  nchar(x)\n)",
  # The replacement form, called either way.
  "f <- function(x) { substring(x, 1L, .Machine$integer.max) <- 'a'; x }",
  "f <- function(x, v) `substring<-`(x, 2L, .Machine$integer.max, value = v)",
  "f <- function(x, v) base::`substring<-`(x, 2L, last = nchar(x), value = v)",
  # Backquoted and quoted heads are calls like any other.
  "f <- function(x) `substring`(x, 2L, .Machine$integer.max)",
  "f <- function(x) 'substring'(x, 2L, .Machine$integer.max)",
  # The native pipe's left side is the first argument.
  "f <- function(x) x |> substring(2L, .Machine$integer.max)",
  "f <- function(x) x |> toupper() |> substring(2L, nchar(x))",
  "f <- function(x) substring(x, 2L, .Machine$integer.max) |> toupper()",
  # A forwarded `...` passes once `last` is named.
  "f <- function(x, ...) substring(x, ..., last = .Machine$integer.max)",
  "f <- function(x, ...) substring(x, ..., la = nchar(x))",
  # Constants: only the integer maximum keeps the whole string.
  "f <- function(x) substring(x, 2L, 2147483647L)",
  "f <- function(x) substring(x, 2L, 2147483647)",
  "f <- function(x, n) substring(x, 2L, n)",
  # Not calls, not references.
  "f <- function(x) substr(x, 2L, 3L)",
  "# substring(x, 2L) in a comment is not a call\nf <- function(x) x",
  "f <- function(x) 'substring(x, 2L) in a string is not a call'",
  "f <- function(x) c('substrings', 'a substring')"
)

FIXTURES_FAIL <- c(
  # Every two-argument shape, including the ones a one-line regex misses
  # (nested parentheses, a call over several lines).
  "f <- function(x) substring(x, 2L)",
  "f <- function(x) substring(text = x, first = 2L)",
  "f <- function(x) base::substring(x, 2L)",
  "f <- function(x, i) substring(x, max(i, 1L) + nchar(sub('a', '', x)))",
  "f <- function(x) substring(\n  x,\n  2L\n)",
  "f <- function(x) { substring(x, 2L) <- 'a'; x }",
  # Backquoted, quoted and replacement-function heads.
  "f <- function(x) `substring`(x, 2L)",
  "f <- function(x) 'substring'(x, 2L)",
  "f <- function(x, v) `substring<-`(x, 2L, value = v)",
  "f <- function(x, v) base::`substring<-`(x, 2L, value = v)",
  # On the right of a pipe, with the left side counted.
  "f <- function(x) x |> substring(2L)",
  # A forwarded `...` without a named `last`.
  "f <- function(x, ...) substring(x, ...)",
  "f <- function(x, ...) substring(x, 2L, ...)",
  # A constant `last` that truncates, gives NA or errors before R 4.6.0.
  "f <- function(x) substring(x, 2L, 1000000L)",
  "f <- function(x) substring(x, 2L, 1e6)",
  "f <- function(x) substring(x, 2L, last = 5L)",
  "f <- function(x) substring(x, 2L, -1L)",
  "f <- function(x) substring(x, 2L, 3e9)",
  "f <- function(x) substring(x, 2L, Inf)",
  "f <- function(x) substring(x, 2L, NA)",
  "f <- function(x) substring(x, 2L, NULL)",
  "f <- function(x) { substring(x, 1L, 1L) <- 'a'; x }",
  "f <- function(x, ...) substring(x, ..., last = 5L)",
  # The function as a value.
  "f <- function(x, i) Map(substring, x, i)",
  "f <- function(x, i) Map(base::substring, x, i)",
  "f <- function(x, i) Map(`substring<-`, x, i, 'a')",
  # The function named by a string: unverifiable, even with `last` supplied.
  "f <- function(x) do.call(\"substring\", list(x, 2L))",
  "f <- function(x) do.call('substring', list(x, 2L, .Machine$integer.max))",
  "f <- function(x) vapply(x, \"substring\", \"\", 2L)",
  "f <- function() match.fun(\"substring\")",
  "f <- function() match.fun(\"substring<-\")",
  "f <- function() match.fun(r\"(substring)\")"
)

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

  for (code in FIXTURES_PASS) {
    expect(sprintf("passes: %s", gsub("\n", " ", code, fixed = TRUE)),
           identical(n_hits(code), 0L))
  }
  for (code in FIXTURES_FAIL) {
    expect(sprintf("fails: %s", gsub("\n", " ", code, fixed = TRUE)),
           identical(n_hits(code), 1L))
  }

  # Every offender is listed, with its line, not only the first.
  res <- run_gate(tree(c("a <- function(x) substring(x, 2L)",
                         "b <- function(x) substring(x, 3L, nchar(x))",
                         "c <- function(x) substring(x, 5L)")))
  expect("lists every offender", length(res$hits) == 2L)
  expect("names file and line",
         identical(sub(":.*$", "", sub("^R/code\\.R:", "", res$hits)),
                   c("1", "3")))

  # A string naming the function says so, rather than claiming a call.
  res <- run_gate(tree("f <- function() match.fun('substring')"))
  expect("a string reference says it is a string",
         length(res$hits) == 1L &&
           grepl("names `substring`", res$hits, fixed = TRUE))

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
    cat(sprintf("%d substring() site(s) without an acceptable explicit `last`",
                length(res$hits)),
        "(ADR 0005: pass .Machine$integer.max):\n")
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
