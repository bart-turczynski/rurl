#!/usr/bin/env Rscript

# stringi call gate (RURL-lbccaixb, ADR 0019).
#
# WHY. ADR 0019 moves string operations from `stringi` to base R. `stringi`
# reads a U+FEFF that starts its UTF-8 input as a byte-order mark and drops
# it, and that cannot be turned off (gagolews/stringi#516). The migration
# runs in slices (RURL-oahhlnvf, RURL-yqmoztoj, RURL-yyeohqah, then the
# close-out RURL-dddxutjf). While it runs, a new `stringi` call would grow
# the work and could reopen the U+FEFF defect, so this gate blocks any
# reference beyond today's. design/stringi-inventory.md describes each
# reference it starts from.
#
# THE ALLOWLIST IS KEYED SO A MOVE OR A REMOVAL NEVER FAILS. Each row of
# tools/stringi-call-allowlist.csv is (file, enclosing top-level function,
# reference, count). No row holds a line number, so an edit that moves a call
# within its function changes nothing. A key found fewer times than allowed,
# or not at all, passes. The gate then prints an "allowlist can shrink" line
# rather than failing, because peer branches remove calls concurrently and
# one branch's removal must not fail another's run. The shrink itself is not
# optional: each migration slice runs `--write-allowlist` and commits the
# shrink in the same MR, so the list shrinks at every merge (ADR 0019,
# decision 4). Only a key found MORE times than allowed, or not listed at
# all, fails. One consequence is deliberate: a call
# moved into a new top-level function, or kept through a rename, counts as
# new, because a reviewer has to see it.
#
# WHAT COUNTS AS A REFERENCE. The files are R/ (recursively, every .R/.r/
# .S/.s/.q file) and NAMESPACE. They are read by PARSING (`getParseData()`),
# not with a regex, so comments and strings that mention a call do not count,
# and a call over several lines or with nested parentheses is found:
#   - `stringi::f` and `stringi:::f`, as a call, a value or an alias
#     (`d <- stringi::stri_detect_regex`), on either side of a pipe. The
#     reference is `stringi::f` or `stringi:::f`, so a `:::` use never
#     matches an allowlisted `::` row;
#   - a bare `stri_*` symbol, called or used as a value. That is how
#     `importFrom(stringi, ...)`, `library(stringi)` or a top-level alias
#     surface in a function body. A list element (`x$stri_a`), an argument
#     name (`f(stri_a = 1)`) and a formal do not count;
#   - the package named as a symbol or a string anywhere except before `::`:
#     `library(stringi)`, `requireNamespace("stringi")`,
#     `asNamespace("stringi")`, `getExportedValue("stringi", ...)`,
#     `` `::`(stringi, f) ``, and `import(stringi)` / `importFrom(stringi, ...)`
#     in NAMESPACE;
#   - a string that is exactly a `stri_*` name (`do.call("stri_sub", ...)`,
#     `match.fun("stri_detect_fixed")`);
#   - a roxygen `@import stringi`, `@importFrom stringi ...` or
#     `@rawNamespace` line that imports from stringi.
# A file that does not parse fails, because the gate cannot clear what it
# cannot read. Naming a function of our own `stri_*` fails too.
#
# SCOPE is the repository root it is given: R/ and NAMESPACE under it, never
# tests/, tools/ or .claude/ (peer worktrees live under the real clone's
# .claude/worktrees/).
#
# Zero dependencies beyond base R and utils. Deterministic and network-free.
#
# Usage:
#   Rscript tools/stringi-call-gate.R                    # scan, exit 1 on a new reference
#   Rscript tools/stringi-call-gate.R --self-test        # positive/negative fixtures
#   Rscript tools/stringi-call-gate.R --root DIR         # scan another tree
#   Rscript tools/stringi-call-gate.R --write-allowlist  # rewrite the allowlist
#       # from the tree. This blesses every reference present, so use it only
#       # to shrink the list in a migration slice's MR, and read the diff: a
#       # grown count or a new row in it is a new stringi reference.

ALLOWLIST <- file.path("tools", "stringi-call-allowlist.csv")
ALLOW_COLS <- c("file", "function", "reference", "count")

# ---- the scan ---------------------------------------------------------------

source_files <- function(root) {
  dir <- file.path(root, "R")
  r <- if (dir.exists(dir)) {
    list.files(dir, pattern = "\\.[RrSsq]$", full.names = TRUE,
               recursive = TRUE)
  } else {
    character(0)
  }
  ns <- file.path(root, "NAMESPACE")
  sort(c(r, if (file.exists(ns)) ns), method = "radix")
}

# A token's name without backquotes or quotes: `stri_sub` and "stri_sub"
# are stri_sub.
bare_name <- function(text) {
  v <- tryCatch(str2lang(text), error = function(e) NULL)
  if (is.character(v) && length(v) == 1L) {
    return(v)
  }
  sub("^`(.*)`$", "\\1", text)
}

# The top-level ancestor of each id in `ids`.
top_ancestor <- function(pd, ids) {
  parent <- stats::setNames(pd$parent, pd$id)
  vapply(ids, function(id) {
    repeat {
      p <- parent[[as.character(id)]]
      if (p <= 0L) {
        return(id)
      }
      id <- p
    }
  }, integer(1))
}

# The name a top-level expression assigns, or "<top-level>".
top_name <- function(pd, top_id) {
  kids <- pd[pd$parent == top_id, ]
  kids <- kids[order(kids$line1, kids$col1), ]
  op <- which(kids$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))
  if (!length(op)) {
    return("<top-level>")
  }
  side <- if (kids$token[op[1L]] == "RIGHT_ASSIGN") {
    kids[nrow(kids), ]
  } else {
    kids[1L, ]
  }
  sym <- pd[pd$parent == side$id & pd$token %in% c("SYMBOL", "STR_CONST"), ]
  if (side$token %in% c("SYMBOL", "STR_CONST")) {
    return(bare_name(side$text))
  }
  if (nrow(sym)) bare_name(sym$text[1L]) else "<top-level>"
}

# Tokens of the same parent expression as token `i`.
siblings <- function(pd, i) {
  pd$token[pd$parent == pd$parent[i]]
}

ROXYGEN_IMPORT <- paste0(
  "^#'\\s*(@importFrom\\s+stringi(\\s|$)|@import\\s+(\\S+\\s+)*stringi(\\s|$)",
  "|@rawNamespace\\s.*import(From)?\\s*\\(\\s*[\"'`]?stringi)"
)

# Every stringi reference in one file: a data frame (file, function,
# reference, line), or a character error when the file does not parse.
file_refs <- function(path, rel) {
  exprs <- tryCatch(parse(path, keep.source = TRUE, encoding = "UTF-8"),
                    error = function(e) e)
  if (inherits(exprs, "error")) {
    return(sprintf("%s: does not parse, so it cannot be cleared (%s)", rel,
                   conditionMessage(exprs)))
  }
  empty <- file_refs_empty()
  pd <- utils::getParseData(exprs, includeText = TRUE)
  if (is.null(pd) || !nrow(pd)) {
    return(empty)
  }
  acc <- new.env()
  acc$hits <- list()
  add <- function(i, ref) {
    acc$hits[[length(acc$hits) + 1L]] <- list(i = i, ref = ref)
  }
  # Only tokens that can be a reference are looked at closely; the sibling
  # lookups below are linear in the file, so a full pass would be quadratic.
  name_tok <- c("SYMBOL", "SYMBOL_FUNCTION_CALL", "STR_CONST")
  cand <- which(pd$terminal & (
    pd$token %in% c("NS_GET", "NS_GET_INT") |
      (pd$token == "COMMENT" & grepl("stringi", pd$text, fixed = TRUE)) |
      (pd$token %in% name_tok & grepl("stri", pd$text, fixed = TRUE))
  ))
  for (i in cand) {
    tok <- pd$token[i]
    txt <- pd$text[i]
    if (tok %in% c("NS_GET", "NS_GET_INT")) {
      sib <- pd[pd$parent == pd$parent[i], ]
      sib <- sib[order(sib$line1, sib$col1), ]
      pkg <- sib$text[sib$token == "SYMBOL_PACKAGE"]
      at <- which(sib$id == pd$id[i])
      if (length(pkg) && bare_name(pkg[1L]) == "stringi" &&
            at < nrow(sib)) {
        add(i, paste0("stringi", txt, bare_name(sib$text[at + 1L])))
      }
      next
    }
    if (tok == "COMMENT") {
      if (grepl(ROXYGEN_IMPORT, txt, perl = TRUE)) {
        add(i, sub("^#'\\s*(@\\S+).*$", "\\1 stringi", txt, perl = TRUE))
      }
      next
    }
    name <- bare_name(txt)
    is_pkg <- identical(name, "stringi")
    if (!is_pkg && !grepl("^stri_[A-Za-z0-9_]+$", name)) {
      next
    }
    if (any(siblings(pd, i) %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'"))) {
      next
    }
    if (is_pkg) {
      add(i, "stringi")
    } else {
      add(i, if (tok == "STR_CONST") sprintf("\"%s\"", name) else name)
    }
  }
  hits <- acc$hits
  if (!length(hits)) {
    return(empty)
  }
  idx <- vapply(hits, function(h) h$i, integer(1))
  # A comment's parent is the negated id of the expression it precedes (or
  # 0); its function is "<roxygen>" either way.
  is_comment <- pd$token[idx] == "COMMENT"
  fun <- rep("<roxygen>", length(idx))
  if (any(!is_comment)) {
    tops <- top_ancestor(pd, pd$id[idx[!is_comment]])
    fun[!is_comment] <- vapply(tops, function(t) top_name(pd, t), "")
  }
  data.frame(file = rep(rel, length(idx)), "function" = fun,
             reference = vapply(hits, function(h) h$ref, ""),
             line = pd$line1[idx], check.names = FALSE,
             stringsAsFactors = FALSE)
}

scan_tree <- function(root) {
  files <- source_files(root)
  refs <- list()
  errors <- character(0)
  for (f in files) {
    rel <- substring(f, nchar(root) + 2L, .Machine$integer.max)
    r <- file_refs(f, rel)
    if (is.character(r)) {
      errors <- c(errors, r)
    } else {
      refs[[length(refs) + 1L]] <- r
    }
  }
  refs <- if (length(refs)) do.call(rbind, refs) else file_refs_empty()
  list(files = length(files), refs = refs, errors = errors)
}

file_refs_empty <- function() {
  data.frame(file = character(0), "function" = character(0),
             reference = character(0), line = integer(0),
             check.names = FALSE, stringsAsFactors = FALSE)
}

key_of <- function(df) {
  paste(df$file, df[["function"]], df$reference, sep = "\t")
}

# Observed references as allowlist rows: one per key, with its count.
observed_rows <- function(refs) {
  if (!nrow(refs)) {
    return(data.frame(file = character(0), "function" = character(0),
                      reference = character(0), count = integer(0),
                      check.names = FALSE, stringsAsFactors = FALSE))
  }
  k <- key_of(refs)
  first <- !duplicated(k)
  out <- refs[first, c("file", "function", "reference")]
  out$count <- as.integer(table(k)[k[first]])
  o <- order(out$file, out[["function"]], out$reference, method = "radix")
  rownames(out) <- NULL
  out[o, ]
}

read_allowlist <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf(paste(
      "%s is missing, so nothing can be cleared; regenerate it with",
      "--write-allowlist and review the diff"
    ), path), call. = FALSE)
  }
  a <- utils::read.csv(path, comment.char = "#", colClasses = "character",
                       check.names = FALSE, stringsAsFactors = FALSE)
  if (!identical(names(a), ALLOW_COLS)) {
    stop(sprintf("%s: columns must be %s", path,
                 paste(ALLOW_COLS, collapse = ",")), call. = FALSE)
  }
  count <- suppressWarnings(as.integer(a$count))
  if (anyNA(count) || any(count < 1L) ||
        !all(grepl("^[0-9]+$", a$count))) {
    stop(sprintf("%s: every count must be a positive integer", path),
         call. = FALSE)
  }
  a$count <- count
  if (anyDuplicated(key_of(a))) {
    stop(sprintf("%s: a (file, function, reference) key is listed twice",
                 path), call. = FALSE)
  }
  a
}

write_allowlist <- function(root) {
  res <- scan_tree(root)
  if (length(res$errors)) {
    stop(paste(res$errors, collapse = "\n"), call. = FALSE)
  }
  rows <- observed_rows(res$refs)
  path <- file.path(root, ALLOWLIST)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(c(
    "# stringi references allowed in R/ and NAMESPACE (RURL-lbccaixb, ADR 0019).",
    "# Read by tools/stringi-call-gate.R; see its header. A key found fewer",
    "# times than its count passes; more times, or unlisted, fails."
  ), con)
  utils::write.csv(rows, con, row.names = FALSE)
  invisible(rows)
}

# Compare a tree with an allowlist. Returns list(fail, shrink, files, refs).
run_gate <- function(root = ".", allowlist = file.path(root, ALLOWLIST)) {
  res <- scan_tree(root)
  allow <- read_allowlist(allowlist)
  seen <- observed_rows(res$refs)
  allowed <- stats::setNames(allow$count, key_of(allow))
  have <- stats::setNames(seen$count, key_of(seen))
  fail <- res$errors
  for (k in names(have)) {
    cap <- if (k %in% names(allowed)) allowed[[k]] else 0L
    if (have[[k]] > cap) {
      parts <- strsplit(k, "\t", fixed = TRUE)[[1L]]
      lines <- res$refs$line[key_of(res$refs) == k]
      fail <- c(fail, sprintf(
        "%s: %s: %s found %d time(s), %d allowed (line(s) %s)",
        parts[1L], parts[2L], parts[3L], have[[k]], cap,
        paste(sort(lines), collapse = ", ")
      ))
    }
  }
  shrink <- character(0)
  for (k in names(allowed)) {
    got <- if (k %in% names(have)) have[[k]] else 0L
    if (got < allowed[[k]]) {
      parts <- strsplit(k, "\t", fixed = TRUE)[[1L]]
      shrink <- c(shrink, sprintf("%s: %s: %s found %d time(s), %d allowed",
                                  parts[1L], parts[2L], parts[3L], got,
                                  allowed[[k]]))
    }
  }
  list(fail = fail, shrink = shrink, files = res$files,
       refs = nrow(res$refs))
}

# ---- self-test --------------------------------------------------------------

# Every fixture is one R/code.R file, checked against an allowlist that grants
# function `f` one `stringi::stri_detect_regex`. A passing fixture yields no
# failure; a failing one yields at least one.
BASE_ALLOW <- data.frame(file = "R/code.R", "function" = "f",
                         reference = "stringi::stri_detect_regex", count = 1L,
                         check.names = FALSE, stringsAsFactors = FALSE)

FIXTURES_PASS <- c(
  "f <- function(x) stringi::stri_detect_regex(x, 'a')",
  # The call moved within its function, and lines were added above it.
  "\n\n\nf <- function(x) {\n  y <- 1\n  stringi::stri_detect_regex(\n    x, 'a'\n  )\n}",
  # Removed altogether: a removal never fails.
  "f <- function(x) grepl('a', x)",
  "",
  # `=` and `->` assignments name the function as `<-` does.
  "f = function(x) stringi::stri_detect_regex(x, 'a')",
  # The allowlisted value converted to an alias in the same function.
  "f <- function(x) { d <- stringi::stri_detect_regex; d(x, 'a') }",
  # Mentions that are not references.
  "# stringi::stri_sub(x) and library(stringi) in a comment\nf <- function(x) x",
  "#' Uses stringi for NFC, see @importFrom elsewhere.\nf <- function(x) x",
  "f <- function(x) 'uses stringi::stri_sub here'",
  "f <- function(x) c('stringify', 'stri', 'stri_', 'my_stri_sub')",
  "f <- function(x) x$stri_sub",
  "f <- function(x) list(stri_sub = 1, stringi = 2)",
  "f <- function(x) otherpkg::stri_sub(x)"
)

FIXTURES_FAIL <- c(
  # A new call in a new function, and an extra call in the allowlisted one.
  "f <- function(x) stringi::stri_detect_regex(x, 'a')\ng <- function(x) stringi::stri_detect_fixed(x, 'a')",
  "f <- function(x) stringi::stri_detect_regex(x, 'a') & stringi::stri_detect_regex(x, 'b')",
  # A different stringi function in the allowlisted one.
  "f <- function(x) stringi::stri_detect_fixed(x, 'a')",
  # `:::` never matches a `::` row.
  "f <- function(x) stringi:::stri_detect_regex(x, 'a')",
  # The allowlisted call moved to a new top-level function.
  "g <- function(x) stringi::stri_detect_regex(x, 'a')",
  # Aliases, at top level and inside a function.
  "g <- stringi::stri_detect_fixed",
  "stringi::stri_detect_fixed -> g",
  "f <- function(x) { stringi::stri_detect_regex(x, 'a'); d <- stringi::stri_sub; d(x, 1, 2) }",
  "f <- function(x) stringi::stri_detect_regex(x, 'a')\nd <- stringi::stri_detect_regex",
  # Backquoted and quoted forms.
  "f <- function(x) `stringi`::stri_detect_regex(x, 'a') & stringi::`stri_sub`(x, 1, 1)",
  "g <- stringi::\"stri_sub\"",
  # On the right of a pipe.
  "f <- function(x) x |> stringi::stri_detect_regex('a') |> stringi::stri_sub(1, 2)",
  # Bare calls and values, as importFrom or library() would allow.
  "f <- function(x) stringi::stri_detect_regex(x, 'a') & stri_detect_fixed(x, 'b')",
  "g <- function(x) Map(stri_sub, x, 1, 2)",
  "g <- function(x) `stri_sub`(x, 1, 2)",
  # Our own names in the stri_* space read as bare references: rename them.
  "f <- function(stri_x) stri_x",
  "stri_mine <- function(x) x",
  # The package named.
  "library(stringi)",
  "f <- function() require(stringi)",
  "f <- function() requireNamespace('stringi', quietly = TRUE)",
  "f <- function() loadNamespace(\"stringi\")",
  "f <- function() asNamespace('stringi')$stri_sub",
  "f <- function() getExportedValue('stringi', 'stri_sub')",
  "f <- function(x) `::`(stringi, stri_sub)(x, 1, 2)",
  "f <- function() library(package = stringi)",
  # A string naming a stri_* function.
  "f <- function(x) do.call('stri_sub', list(x, 1, 2))",
  "f <- function() match.fun(\"stri_detect_fixed\")",
  # Roxygen imports.
  "#' @importFrom stringi stri_detect_fixed\nf <- function(x) stringi::stri_detect_regex(x, 'a')",
  "#' @import stringi\nf <- function(x) x",
  "#' @import utils stringi\nf <- function(x) x",
  "#' @rawNamespace import(stringi, except = stri_sub)\nf <- function(x) x",
  # A file that does not parse.
  "f <- function("
)

# NAMESPACE fixtures, each beside a passing R/code.R.
NAMESPACE_FAIL <- c(
  "importFrom(stringi, stri_sub)",
  "importFrom(\"stringi\", \"stri_sub\")",
  "import(stringi)",
  "export(f)\nif (TRUE) import(stringi)"
)
NAMESPACE_PASS <- c(
  "export(f)\nimportFrom(utils, head)",
  "# import(stringi) in a comment\nexport(f)"
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
  tree <- function(code, allow = BASE_ALLOW, namespace = NULL) {
    root <- tempfile("stringi-call-fx-")
    dir.create(file.path(root, "R"), recursive = TRUE)
    dir.create(file.path(root, "tools"))
    writeLines(code, file.path(root, "R", "code.R"))
    if (!is.null(namespace)) {
      writeLines(namespace, file.path(root, "NAMESPACE"))
    }
    utils::write.csv(allow, file.path(root, ALLOWLIST), row.names = FALSE)
    root
  }
  label <- function(code) gsub("\n", " ", code, fixed = TRUE)
  ok_code <- "f <- function(x) stringi::stri_detect_regex(x, 'a')"

  for (code in FIXTURES_PASS) {
    expect(sprintf("passes: %s", label(code)),
           length(run_gate(tree(code))$fail) == 0L)
  }
  for (code in FIXTURES_FAIL) {
    expect(sprintf("fails: %s", label(code)),
           length(run_gate(tree(code))$fail) >= 1L)
  }
  for (ns in NAMESPACE_PASS) {
    expect(sprintf("passes NAMESPACE: %s", label(ns)),
           length(run_gate(tree(ok_code, namespace = ns))$fail) == 0L)
  }
  for (ns in NAMESPACE_FAIL) {
    expect(sprintf("fails NAMESPACE: %s", label(ns)),
           length(run_gate(tree(ok_code, namespace = ns))$fail) >= 1L)
  }

  # A removal passes and says the allowlist can shrink.
  res <- run_gate(tree("f <- function(x) x"))
  expect("a removal prints a shrink line",
         length(res$fail) == 0L && length(res$shrink) == 1L &&
           grepl("found 0 time(s), 1 allowed", res$shrink, fixed = TRUE))

  # A count of two allows two, and only two.
  two <- BASE_ALLOW
  two$count <- 2L
  pair <- paste("f <- function(x) stringi::stri_detect_regex(x, 'a') |",
                "stringi::stri_detect_regex(x, 'b')")
  expect("a count of two allows two",
         length(run_gate(tree(pair, allow = two))$fail) == 0L)
  three <- paste(pair, "| stringi::stri_detect_regex(x, 'c')")
  res <- run_gate(tree(three, allow = two))
  expect("a third call fails and names its key, counts and lines",
         length(res$fail) == 1L &&
           grepl(paste0("R/code.R: f: stringi::stri_detect_regex found 3 ",
                        "time(s), 2 allowed (line(s) 1, 1, 1)"),
                 res$fail, fixed = TRUE))

  # Each failure names the function it is in.
  res <- run_gate(tree(FIXTURES_FAIL[1L]))
  expect("a new function's call is reported under that function",
         length(res$fail) == 1L &&
           grepl("R/code.R: g: stringi::stri_detect_fixed found 1", res$fail,
                 fixed = TRUE))

  # A roxygen import is reported as such.
  res <- run_gate(tree("#' @importFrom stringi stri_sub\nf <- function(x) x"))
  expect("a roxygen import is named",
         length(res$fail) == 1L &&
           grepl("<roxygen>: @importFrom stringi", res$fail, fixed = TRUE))

  # The allowlist fails closed: missing, malformed, duplicated.
  root <- tree(ok_code)
  unlink(file.path(root, ALLOWLIST))
  expect("a missing allowlist stops",
         inherits(tryCatch(run_gate(root), error = function(e) e), "error"))
  bad <- BASE_ALLOW
  bad$count <- 0L
  expect("a zero count stops", inherits(tryCatch(
    run_gate(tree(ok_code, allow = bad)), error = function(e) e
  ), "error"))
  expect("a duplicated key stops", inherits(tryCatch(
    run_gate(tree(ok_code, allow = rbind(BASE_ALLOW, BASE_ALLOW))),
    error = function(e) e
  ), "error"))
  names(bad) <- c("file", "fun", "reference", "count")
  bad$count <- 1L
  expect("wrong columns stop", inherits(tryCatch(
    run_gate(tree(ok_code, allow = bad)), error = function(e) e
  ), "error"))

  # --write-allowlist round-trips: the tree it was written from passes.
  root <- tree(paste(FIXTURES_FAIL[c(1L, 13L)], collapse = "\n"))
  rows <- write_allowlist(root)
  res <- run_gate(root)
  expect("a written allowlist clears its own tree",
         nrow(rows) == 3L && length(res$fail) == 0L &&
           length(res$shrink) == 0L)

  # A tree with no R/ and no NAMESPACE reads zero files.
  empty <- tempfile("stringi-call-empty-")
  dir.create(file.path(empty, "tools"), recursive = TRUE)
  utils::write.csv(BASE_ALLOW[0L, ], file.path(empty, ALLOWLIST),
                   row.names = FALSE)
  res <- run_gate(empty)
  expect("no R/ reads zero files",
         res$files == 0L && length(res$fail) == 0L)

  cat(sprintf("self-test: %d passed, %d failed\n", st$pass, length(st$fail)))
  if (length(st$fail)) {
    for (f in st$fail) cat(sprintf("  FAILED: %s\n", f))
    stop("stringi-call-gate self-test: FAIL", call. = FALSE)
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
  root <- "."
  at <- match("--root", args)
  if (!is.na(at)) {
    if (at == length(args)) stop("--root needs a directory", call. = FALSE)
    root <- sub("/+$", "", args[at + 1L])
  }
  if ("--write-allowlist" %in% args) {
    rows <- write_allowlist(root)
    cat(sprintf("wrote %s: %d key(s), %d reference(s)\n",
                file.path(root, ALLOWLIST), nrow(rows), sum(rows$count)))
    return(invisible(TRUE))
  }
  cat("stringi call gate (RURL-lbccaixb, ADR 0019)\n")
  res <- run_gate(root)
  for (s in res$shrink) cat(sprintf("  allowlist can shrink: %s\n", s))
  if (length(res$fail)) {
    cat(sprintf(paste(
      "%d new stringi reference(s) in R/ or NAMESPACE (ADR 0019: string",
      "operations run in base R; see R/strings-base.R):\n"
    ), length(res$fail)))
    for (f in res$fail) cat(sprintf("  %s\n", f))
    cat("VERDICT FAIL\n")
    stop("stringi-call-gate: FAIL", call. = FALSE)
  }
  cat(sprintf("%d stringi reference(s) in %d file(s), none beyond %s\n",
              res$refs, res$files, ALLOWLIST))
  cat("VERDICT PASS\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive() &&
      sys.nframe() == 0L) {
  main()
}
