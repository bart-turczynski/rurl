#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
env_file <- Sys.getenv("RURL_COVERAGE_REPORT", unset = "")

out_file <- if (length(args) > 0 && nzchar(args[[1]])) {
  args[[1]]
} else if (nzchar(env_file)) {
  env_file
} else {
  file.path("_scratch", "covr_report.html")
}

out_dir <- dirname(out_file)
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

if (!dir.exists(out_dir)) {
  stop("Coverage report directory does not exist and could not be created: ",
       out_dir, call. = FALSE)
}

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Package 'covr' is required to generate a coverage report.",
       call. = FALSE)
}

coverage <- covr::package_coverage(quiet = FALSE)
covr::report(coverage, file = out_file, browse = FALSE)

cat("Coverage report written to ", normalizePath(out_file, mustWork = FALSE),
    "\n", sep = "")
