#!/usr/bin/env Rscript
# Standard-conformance parity harness (RURL-wncwfasl companion).
#
# NOT a test and NOT run on CRAN. Answers a single question: how close is rurl
# to the STANDARDS THEMSELVES -- not to other parsers. The disagreement harness
# (parser-disagreement.R) measures divergence between tools; this one measures
# CONFORMANCE against the spec's own oracle, so "% parity with WHATWG /
# RFC 3986" is a tracked number to close conformance issues against.
#
#   Rscript inst/bench/standard-parity.R
#   Rscript -e 'devtools::load_all(); source("inst/bench/standard-parity.R")'
#
# Oracles (committed, regenerable):
#   inst/bench/wpt-url-cases.json  -- WHATWG: derived from web-platform-tests
#     urltestdata.json (BSD-3-Clause) by make-wpt-fixture.py. success cases
#     carry the spec's expected components; failure cases MUST be rejected.
#     Success is limited to the schemes rurl supports (http/https/ftp/file)
#     -- the "additional protocols notwithstanding" carve-out.
#   inst/bench/rfc3986-probes.csv  -- RFC: hand-authored against RFC 3986's
#     grammar/normalization rules (no official suite exists), each row tagged
#     with its section.
#
# WHATWG success cases are scored in rurl's CANONICAL-OUTPUT config
# (scheme_policy="require", host_encoding="idna", path_encoding="encode") -- the
# settings that ask rurl for the WHATWG serialization; residual mismatches are
# genuine (the standard cannot be reached by any knob). Failure cases use only
# scheme_policy="require".
#
# Outputs (CSV) go to $RURL_PARITY_OUT (default: _scratch/parity):
#   whatwg-success-scored.csv / whatwg-failure-scored.csv
#   rfc-probes-scored.csv
# ----------------------------------------------------------------------------

if (!requireNamespace("rurl", quietly = TRUE) &&
    !exists("safe_parse_urls", mode = "function")) {
  stop("rurl not available: install it or run after devtools::load_all().")
}
spu <- if (exists("safe_parse_urls", mode = "function")) {
  get("safe_parse_urls")
} else {
  rurl::safe_parse_urls
}

find_file <- function(rel) {
  cand <- c(rel, file.path("inst", "bench", basename(rel)))
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot locate ", rel, " (run from the repo root).")
  hit
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a)) b else a
rej <- function(s) s %in% c("error", "reject")
blank <- function(x) ifelse(is.na(x), "", as.character(x))

wpt <- jsonlite::fromJSON(find_file("wpt-url-cases.json"),
                          simplifyVector = FALSE)
succ <- do.call(rbind, lapply(wpt$success, function(e) data.frame(
  input = e$input, protocol = e$protocol %||% "", hostname = e$hostname %||% "",
  port = e$port %||% "", pathname = e$pathname %||% "",
  stringsAsFactors = FALSE)))
fail_inputs <- vapply(wpt$failure, function(e) e$input, character(1))

# ---- WHATWG success: canonical-output config ------------------------------
cfg <- spu(succ$input, url_standard = "whatwg", scheme_policy = "require",
           host_encoding = "idna", path_encoding = "encode")
cfg <- suppressWarnings(cfg)
acc <- !rej(cfg$parse_status)
exp_scheme <- sub(":$", "", succ$protocol)
s_ok <- acc & blank(cfg$scheme) == exp_scheme
h_ok <- acc & blank(cfg$host) == succ$hostname
p_ok <- acc & blank(cfg$port) == succ$port
path_ok <- acc & blank(cfg$path) == succ$pathname
full <- s_ok & h_ok & p_ok & path_ok

succ_scored <- data.frame(
  input = succ$input, accepted = acc,
  scheme_ok = s_ok, host_ok = h_ok, port_ok = p_ok, path_ok = path_ok,
  full_parity = full,
  rurl_scheme = blank(cfg$scheme), exp_scheme = exp_scheme,
  rurl_host = blank(cfg$host), exp_host = succ$hostname,
  rurl_port = blank(cfg$port), exp_port = succ$port,
  rurl_path = blank(cfg$path), exp_path = succ$pathname,
  rurl_status = cfg$parse_status, stringsAsFactors = FALSE)

# ---- WHATWG failure: must reject ------------------------------------------
fdf <- suppressWarnings(spu(fail_inputs, url_standard = "whatwg",
                            scheme_policy = "require"))
f_rejected <- rej(fdf$parse_status)
fail_scored <- data.frame(
  input = fail_inputs, rurl_status = fdf$parse_status,
  conformant = f_rejected, stringsAsFactors = FALSE)

# ---- RFC 3986 probes ------------------------------------------------------
rp <- utils::read.csv(find_file("rfc3986-probes.csv"), stringsAsFactors = FALSE)
rr <- suppressWarnings(spu(rp$input, url_standard = "rfc3986"))
r_acc <- !rej(rr$parse_status)
want_acc <- rp$expect == "accept"
r_accept_ok <- r_acc == want_acc
r_host_ok <- !want_acc | (r_acc & blank(rr$host) == rp$expected_host)
r_path_ok <- !want_acc | (r_acc & blank(rr$path) == rp$expected_path)
r_pass <- r_accept_ok & r_host_ok & r_path_ok
rfc_scored <- data.frame(
  id = rp$id, input = rp$input, rfc_section = rp$rfc_section,
  expect = rp$expect, pass = r_pass,
  rurl_status = rr$parse_status,
  rurl_host = blank(rr$host), exp_host = rp$expected_host,
  rurl_path = blank(rr$path), exp_path = rp$expected_path,
  note = rp$note, stringsAsFactors = FALSE)

# ---- write ----------------------------------------------------------------
out_dir <- Sys.getenv("RURL_PARITY_OUT",
                      unset = file.path("_scratch", "parity"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(succ_scored, file.path(out_dir, "whatwg-success-scored.csv"),
                 row.names = FALSE)
utils::write.csv(fail_scored, file.path(out_dir, "whatwg-failure-scored.csv"),
                 row.names = FALSE)
utils::write.csv(rfc_scored, file.path(out_dir, "rfc-probes-scored.csv"),
                 row.names = FALSE)

# ---- report ---------------------------------------------------------------
pk <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                           error = function(e) "absent")
pct <- function(x) sprintf("%d/%d (%.0f%%)", sum(x), length(x), 100 * mean(x))
cat("standard-conformance parity harness (RURL-wncwfasl companion)\n")
cat(strrep("=", 74), "\n", sep = "")
cat(sprintf("  R %s | rurl %s | pslr %s | punycoder %s\n",
            getRversion(), pk("rurl"), pk("pslr"), pk("punycoder")))
cat(sprintf("  WHATWG oracle : WPT urltestdata (%d success + %d failure)\n",
            nrow(succ), length(fail_inputs)))
cat(sprintf("  RFC oracle    : %d hand-authored RFC-3986 probes\n", nrow(rp)))
cat(sprintf("  output dir    : %s\n\n", normalizePath(out_dir)))

cat("== WHATWG (canonical-output config) ==\n")
cat("  success accepted        :", pct(acc), "\n")
cat("  success FULL parity     :", pct(full), "\n")
cat("    scheme", pct(s_ok), "| host", pct(h_ok),
    "| port", pct(p_ok), "| path", pct(path_ok), "\n")
cat("  failure correctly reject:", pct(f_rejected), "\n")
cat("  overall WHATWG acceptance conformance:",
    pct(c(acc, f_rejected)), "\n\n")

cat("== RFC 3986 (probe set) ==\n")
cat("  probes passed           :", pct(r_pass), "\n")
if (!all(r_pass)) {
  cat("  FAILURES:\n")
  bad <- rfc_scored[!r_pass, ]
  for (i in seq_len(nrow(bad))) {
    cat(sprintf(
      "   [%s S%s] %s -> status=%s host=%s (want %s) path=%s (want %s)\n",
      bad$id[i], bad$rfc_section[i], bad$input[i], bad$rurl_status[i],
      bad$rurl_host[i], bad$exp_host[i], bad$rurl_path[i], bad$exp_path[i]))
  }
}

cat("\n== WHATWG success: non-conformances by component ==\n")
for (cc in c("scheme_ok", "host_ok", "port_ok", "path_ok")) {
  bad <- acc & !succ_scored[[cc]]
  cat(sprintf("  %-9s %d\n", sub("_ok", "", cc, fixed = TRUE), sum(bad)))
}
cat("  (over-strict rejects of WPT-valid input:", sum(!acc), ")\n")

invisible(NULL)
