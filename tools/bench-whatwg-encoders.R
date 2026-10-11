#!/usr/bin/env Rscript
# Advisory benchmark for the WHATWG component percent encoders
# (R/path-query.R, RURL-khcicsca). NOT a gate: timings depend on the machine
# and on whatever else it is running, so nothing here fails on a number.
#
#   Rscript tools/bench-whatwg-encoders.R
#   Rscript tools/bench-whatwg-encoders.R --ref <path to another checkout>
#   Rscript tools/bench-whatwg-encoders.R --rows 5000 --reps 5
#
# It times each encoder (path, query, userinfo, fragment, C0) and two
# end-to-end WHATWG surfaces (`serialize_url()`, `safe_parse_urls()`) over
# four batches of the same row count:
#
#   ascii    printable ASCII queries/paths that still hold encodable octets
#            (spaces, quotes) -- the common shape
#   idn      non-ASCII text (Cyrillic, CJK, accented Latin, emoji)
#   invalid  undecodable octets (bare 0x80-0xFF, truncated sequences)
#   long     ~2 KB rows mixing all of the above
#
# and reports milliseconds per batch, nanoseconds per input octet, and each
# hostile batch's per-octet cost over the ascii batch's. The batches differ in
# size (a long row is ~100 ascii rows), so the per-octet ratio is the one that
# says whether hostile input is disproportionately slow.
#
# With `--ref`, the same measurements run against the other checkout in a
# child process (`devtools::load_all()` on it) and a main-vs-branch table is
# printed. An encoder that only takes one string at a time -- the per-row
# shape before RURL-khcicsca -- is driven row by row there, which is what its
# callers did.

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- match(flag, args)
  if (is.na(hit) || hit == length(args)) default else args[hit + 1L]
}
script_path <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    file.path("tools", "bench-whatwg-encoders.R")
  } else {
    normalizePath(sub("^--file=", "", file_arg[1L]))
  }
})
tree <- arg_value("--tree", dirname(dirname(script_path)))
ref <- arg_value("--ref")
emit <- arg_value("--emit")
rows <- as.integer(arg_value("--rows", "2000"))
reps <- as.integer(arg_value("--reps", "3"))

suppressMessages(devtools::load_all(tree, quiet = TRUE, export_all = TRUE))
ns <- asNamespace("rurl")

# ---- batches ------------------------------------------------------------------
octets <- function(...) rawToChar(as.raw(c(...)))
make_batch <- function(kind, n) {
  # A row-number suffix keeps every row distinct, so the parse surfaces'
  # de-duplication cannot collapse a batch to its four templates.
  pick <- function(pool) paste0(rep_len(pool, n), "&n=", seq_len(n))
  switch(kind,
    ascii = pick(c(
      "q=hello world&x=\"quoted\"&y=<tag>", "a b c d e f g h i j k l m n",
      "path/with spaces/and#hash", "key='value' & other=\"thing\""
    )),
    idn = pick(c(
      "q=\u043f\u0440\u0438\u0432\u0435\u0442 \u043c\u0438\u0440",
      "\u65e5\u672c\u8a9e\u306e\u30c6\u30ad\u30b9\u30c8/\u30d1\u30b9",
      "caf\u00e9 cr\u00e8me br\u00fbl\u00e9e", "emoji \U0001F600\U0001F680"
    )),
    invalid = {
      bad <- c(
        octets(0x61, 0xff, 0x62, 0x20, 0xfe), octets(0xc3, 0x28, 0xa0, 0xa1),
        octets(0xe2, 0x28, 0xa1, 0x20, 0x80), octets(0xf0, 0x90, 0x28, 0xbc)
      )
      Encoding(bad) <- "UTF-8"
      pick(bad)
    },
    long = {
      # ASCII, a UTF-8 CJK pair (U+65E5 U+672C) and a stray 0x80, as octets.
      unit <- rawToChar(c(
        charToRaw("q=hello world&"), as.raw(c(0xe6, 0x97, 0xa5, 0xe6, 0x9c)),
        as.raw(c(0xac, 0x26, 0x80)), charToRaw("&x=\"y\"&")
      ))
      Encoding(unit) <- "UTF-8"
      pick(paste(rep(unit, 2048L %/% nchar(unit, type = "bytes")),
                 collapse = ""))
    }
  )
}
kinds <- c("ascii", "idn", "invalid", "long")
batches <- setNames(lapply(kinds, make_batch, n = rows), kinds)

# ---- encoders, vectorized or driven row by row ------------------------------------
vector_or_rows <- function(f) {
  vectorized <- tryCatch(
    length(f(c("a b", "c d"))) == 2L,
    error = function(e) FALSE
  )
  if (vectorized) {
    return(f)
  }
  function(x) vapply(x, f, character(1), USE.NAMES = FALSE)
}
encode_c0 <- function(x) {
  get(".whatwg_component_percent_encode", ns)(x, integer(0))
}
encode_query <- function(x) {
  get(".whatwg_query_percent_encode", ns)(x, "http")
}
encoders <- list(
  path = vector_or_rows(get(".whatwg_path_percent_encode", ns)),
  query = vector_or_rows(encode_query),
  userinfo = vector_or_rows(get(".whatwg_userinfo_percent_encode", ns)),
  fragment = vector_or_rows(get(".whatwg_fragment_percent_encode", ns)),
  c0 = vector_or_rows(encode_c0)
)
surfaces <- list(
  serialize_url = function(x) {
    serialize_url(paste0("http://h/p?", x), standard = "whatwg")
  },
  safe_parse_urls = function(x) {
    safe_parse_urls(paste0("http://h/p?", x), url_standard = "whatwg")
  }
)

# The parse caches would turn every repeat of an end-to-end call into a cache
# hit, so they are cleared before each timed run (outside the timing).
time_ms <- function(f, x) {
  f(x[seq_len(min(10L, length(x)))]) # warm the encode tables
  t <- vapply(seq_len(reps), function(i) {
    rurl_clear_caches()
    unname(system.time(f(x))[["elapsed"]])
  }, numeric(1))
  1000 * stats::median(t)
}

results <- do.call(rbind, lapply(
  c(names(encoders), names(surfaces)),
  function(nm) {
    f <- if (nm %in% names(encoders)) encoders[[nm]] else surfaces[[nm]]
    data.frame(
      target = nm, batch = kinds,
      ms = vapply(kinds, function(k) time_ms(f, batches[[k]]), numeric(1)),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
))

if (!is.null(emit)) {
  saveRDS(results, emit)
  quit(save = "no")
}

batch_bytes <- vapply(
  batches, function(b) sum(nchar(b, type = "bytes")), numeric(1)
)
results$ns_per_octet <- round(
  1e6 * results$ms / batch_bytes[results$batch], 1
)
is_ascii <- results$batch == "ascii"
ascii_ms <- results$ms[is_ascii][match(results$target, results$target[is_ascii])]
ascii_ns <- results$ns_per_octet[is_ascii][
  match(results$target, results$target[is_ascii])
]
# A batch under the 1 ms timer resolution has no meaningful ratio.
results$vs_ascii <- ifelse(
  ascii_ms >= 1, round(results$ns_per_octet / ascii_ns, 2), NA_real_
)
results$ms <- round(results$ms, 1)
cat(sprintf("rows per batch: %d, median of %d runs, tree: %s\n",
            rows, reps, tree))
cat("input octets per batch:",
    paste(names(batch_bytes), batch_bytes, sep = "=", collapse = " "), "\n")
cat("vs_ascii: per-octet cost over the ascii batch's",
    "(NA: ascii batch under 1 ms)\n\n")
print(results, row.names = FALSE)

if (!is.null(ref)) {
  out <- tempfile(fileext = ".rds")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script_path), "--tree", shQuote(ref), "--emit", shQuote(out),
      "--rows", rows, "--reps", reps)
  )
  if (!identical(status, 0L) || !file.exists(out)) {
    stop("the reference run failed (status ", status, ")")
  }
  base <- readRDS(out)
  cmp <- merge(
    base[, c("target", "batch", "ms")],
    results[, c("target", "batch", "ms")],
    by = c("target", "batch"), suffixes = c("_ref", "_tree")
  )
  cmp$speedup <- round(cmp$ms_ref / pmax(cmp$ms_tree, 0.1), 1)
  cmp$ms_ref <- round(cmp$ms_ref, 1)
  cat(sprintf("\nreference: %s\n\n", ref))
  print(cmp[order(cmp$target, match(cmp$batch, kinds)), ], row.names = FALSE)
}
