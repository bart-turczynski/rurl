#!/usr/bin/env Rscript
# Exact per-input divergence tables for three focused pairings, derived from the
# frozen disagreement-long.csv (regenerate that first with
# inst/bench/parser-disagreement.R). Companion to README.md.
#
#   1. rurl(whatwg)  vs adaR           -- WHATWG reference (Ada, C++)
#   2. rurl(rfc3986) vs curl           -- libcurl (C)
#   3. rurl(rfc3986) vs python-urllib  -- CPython urllib.parse (non-R baseline)
#
# Scoring matches the harness: scheme/host/port verbatim, path presentation
# canonicalized (canon_path) so readable-vs-percent-encoded of the SAME
# character is not counted (that is the path_encoding axis, ADR 0011).
# Writes diverge-<pair>.csv next to this script and prints each table.
#
#   Rscript analysis/disagreement/pairwise-divergences.R
#
# Requires python3 on PATH for the third pairing (skipped with a note if
# absent).

here <- tryCatch(dirname(sub("^--file=", "", grep("^--file=",
  commandArgs(FALSE), value = TRUE)[1])), error = function(e) ".")
if (is.na(here) || !nzchar(here)) here <- "analysis/disagreement"
long <- read.csv(file.path(here, "disagreement-long.csv"),
                 stringsAsFactors = FALSE, colClasses = "character")

rej <- function(s) s %in% c("error", "reject")
inp <- unique(long$input)
get <- function(p) { d <- long[long$parser == p, ]; d[match(inp, d$input), ] }

.forbidden_path <- c(" ", "\"", "<", ">", "`", "{", "}", "|", "\\", "^")
canon <- function(x) vapply(x, function(s) {
  if (is.na(s) || !nzchar(s)) return(s)
  b <- charToRaw(s)
  paste(vapply(b, function(z) {
    iv <- as.integer(z); ch <- rawToChar(z)
    if (iv > 127L || iv < 32L || ch %in% .forbidden_path) {
      sprintf("%%%02X", iv)
    } else {
      ch
    }
  }, character(1)), collapse = "")
}, character(1), USE.NAMES = FALSE)

comps <- c("scheme", "host", "port", "path")
diff_view <- function(a, b, name_a, name_b, slug) {
  rows <- list()
  for (i in seq_along(inp)) {
    ra <- rej(a$status[i]); rb <- rej(b$status[i])
    if (ra && rb) next
    difc <- character(0); va <- character(0); vb <- character(0)
    if (ra != rb) {
      difc <- "ACCEPT/REJECT"
      va <- if (ra) paste0("<", a$status[i], ">") else "accept"
      vb <- if (rb) paste0("<", b$status[i], ">") else "accept"
    } else {
      for (cc in comps) {
        xa <- if (cc == "path") canon(a[[cc]][i]) else a[[cc]][i]
        xb <- if (cc == "path") canon(b[[cc]][i]) else b[[cc]][i]
        if (!identical(xa, xb)) {
          difc <- c(difc, cc)
          va <- c(va, a[[cc]][i]); vb <- c(vb, b[[cc]][i])
        }
      }
    }
    if (length(difc)) {
      rows[[length(rows) + 1L]] <- data.frame(
        input = inp[i], component = paste(difc, collapse = ","),
        a = paste(va, collapse = " ; "), b = paste(vb, collapse = " ; "),
        stringsAsFactors = FALSE)
    }
  }
  df <- if (length(rows)) do.call(rbind, rows) else
    data.frame(input = character(), component = character(),
               a = character(), b = character(), stringsAsFactors = FALSE)
  names(df)[3:4] <- c(name_a, name_b)
  utils::write.csv(df, file.path(here, paste0("diverge-", slug, ".csv")),
                   row.names = FALSE)
  cat(sprintf("\n#### %s vs %s -- %d divergent rows ####\n",
              name_a, name_b, nrow(df)))
  tb <- sort(table(df$component), decreasing = TRUE)
  cat("  by component:",
      paste(sprintf("%s=%d", names(tb), tb), collapse = "  "), "\n")
  invisible(df)
}

diff_view(get("rurl(whatwg)"), get("adaR"),
          "rurl(whatwg)", "adaR", "whatwg-vs-adaR")
diff_view(get("rurl(rfc3986)"), get("curl"),
          "rurl(rfc3986)", "curl", "rfc-vs-curl")

# ---- external non-R RFC reference (CPython urllib.parse) -------------------
py_bin <- Sys.which("python3")
if (nzchar(py_bin)) {
  tmp_in <- tempfile(fileext = ".json")
  jsonlite::write_json(inp, tmp_in)
  raw <- system2(py_bin, c(shQuote(file.path(here, "cross-language-rfc.py")),
                           shQuote(tmp_in)), stdout = TRUE)
  pj <- jsonlite::fromJSON(paste(raw, collapse = ""))
  np <- function(x) ifelse(is.na(x), "", as.character(x))
  p <- ifelse(is.na(pj$path) | pj$path == "", "/", pj$path)
  py <- data.frame(
    input  = pj$input,
    scheme = tolower(sub(":$", "", np(pj$scheme))),
    host   = np(pj$host),
    port   = ifelse(is.na(pj$port) | pj$port == "", "", pj$port),
    path   = ifelse(startsWith(p, "/"), p, paste0("/", p)),
    status = np(pj$status), stringsAsFactors = FALSE)
  py <- py[match(inp, py$input), ]
  diff_view(get("rurl(rfc3986)"), py,
            "rurl(rfc3986)", "python-urllib", "rfc-vs-python")
} else {
  message("NOTE: python3 not on PATH -- skipping rurl(rfc) vs python-urllib.")
}
