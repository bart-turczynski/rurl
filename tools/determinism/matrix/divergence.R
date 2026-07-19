#!/usr/bin/env Rscript
# Per-id divergence join across every collected libcurl probe (RURL-ttrcfneq).
#
#   Rscript tools/determinism/matrix/divergence.R
#
# Inputs:  tools/determinism/out/curl-<LABEL>.csv (every label present)
# Output:  tools/determinism/out/curl-divergence.csv
#
# RAW DATA ONLY. This script does not bucket, rank or interpret anything -- it
# emits the honest diff for the downstream analysis unit. A row appears iff at
# least two labels disagree on that (id, column). Cells are already canonical
# ASCII JSON literals, so byte inequality IS value inequality: no encoding,
# locale or line-ending noise can manufacture a difference.
#
# Output columns: id, construct, input_json, column, n_distinct, then one
# column per label (label names sanitized to syntactic names, sorted). Sorted
# by id then column position, so re-runs are byte-stable.
#
# Base R only.
# ----------------------------------------------------------------------------

find_dir <- function() {
  cand <- c(Sys.getenv("RURL_DETERMINISM_OUT", ""),
            file.path("tools", "determinism", "out"),
            file.path("..", "out"), "out")
  cand <- cand[nzchar(cand)]
  hit <- cand[dir.exists(cand)][1]
  if (is.na(hit)) {
    stop("cannot locate tools/determinism/out (run from the repo root).")
  }
  normalizePath(hit)
}

out_dir <- find_dir()
files <- sort(list.files(out_dir, pattern = "^curl-.*\\.csv$",
                         full.names = TRUE))
files <- files[basename(files) != "curl-divergence.csv"]
if (length(files) < 2L) {
  stop("need at least 2 curl-<LABEL>.csv probes in ", out_dir,
       "; found ", length(files))
}

labels <- sub("\\.csv$", "", sub("^curl-", "", basename(files)))
cat("labels:", paste(labels, collapse = ", "), "\n")

VALUE_COLS <- c("curl_status", "url", "scheme", "user", "password",
                "host", "port", "path", "query", "fragment")

read_probe <- function(path) {
  d <- utils::read.csv(path, colClasses = "character", stringsAsFactors = FALSE)
  need <- c("id", "construct", "input_json", VALUE_COLS)
  miss <- setdiff(need, names(d))
  if (length(miss)) {
    stop(basename(path), " is missing column(s): ", toString(miss))
  }
  d[order(d$id), need, drop = FALSE]
}

probes <- lapply(files, read_probe)
names(probes) <- labels

ids <- probes[[1L]]$id
for (i in seq_along(probes)) {
  if (!identical(probes[[i]]$id, ids)) {
    stop("label ", labels[i], " has a different id set than ", labels[1L],
         " -- probes must share one corpus.")
  }
}

meta <- probes[[1L]][, c("id", "construct", "input_json"), drop = FALSE]
col_label <- make.names(labels, unique = TRUE)

rows <- list()
for (k in VALUE_COLS) {
  mat <- vapply(probes, function(d) d[[k]], character(length(ids)))
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = length(ids))
  ndist <- apply(mat, 1L, function(v) length(unique(v)))
  hit <- which(ndist > 1L)
  if (!length(hit)) next
  part <- data.frame(
    id = meta$id[hit],
    construct = meta$construct[hit],
    input_json = meta$input_json[hit],
    column = k,
    n_distinct = ndist[hit],
    stringsAsFactors = FALSE
  )
  for (j in seq_along(col_label)) part[[col_label[j]]] <- mat[hit, j]
  rows[[k]] <- part
}

if (!length(rows)) {
  cat("NO divergence: all", length(labels), "labels agree on all",
      length(ids), "ids and", length(VALUE_COLS), "columns.\n")
  div <- data.frame(
    id = character(0), construct = character(0), input_json = character(0),
    column = character(0), n_distinct = integer(0), stringsAsFactors = FALSE
  )
  for (j in seq_along(col_label)) div[[col_label[j]]] <- character(0)
} else {
  div <- do.call(rbind, rows)
  # deterministic order: id, then the fixed column order above
  div <- div[order(div$id, match(div$column, VALUE_COLS)), , drop = FALSE]
  rownames(div) <- NULL
}

path <- file.path(out_dir, "curl-divergence.csv")
con <- file(path, open = "wb")                # binary: force LF, never CRLF
utils::write.csv(div, con, row.names = FALSE, quote = TRUE, eol = "\n")
close(con)

cat(sprintf("wrote %s (%d divergent rows over %d ids x %d labels)\n",
            path, nrow(div), length(ids), length(labels)))
if (nrow(div)) {
  tab <- sort(table(div$construct), decreasing = TRUE)
  cat("divergent rows per construct:\n")
  print(tab)
}
