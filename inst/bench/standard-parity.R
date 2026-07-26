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
#     Since RURL-ghdlrcjv the success set spans EVERY scheme WPT exercises
#     (opaque, ws:, wss:, ...), not just http/https/ftp/file; only
#     base-relative rows are out of scope (rurl parses absolute URLs).
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
# scheme_acceptance is passed EXPLICITLY and scored at BOTH postures, never
# left implicit at the exported default: "web" (the ADR 0004 closed allowlist,
# http/https/ftp/ftps/file) and "general" (every scheme). A low "web" success
# count is the closed scheme set working as designed, NOT a conformance
# failure; the grammar figure is the "general" one.
#
# Outputs (CSV) go to $RURL_PARITY_OUT (default: _scratch/parity):
#   whatwg-success-scored.csv          -- scheme_acceptance = "web"
#   whatwg-success-scored-general.csv  -- scheme_acceptance = "general"
#   whatwg-failure-scored.csv          -- scheme_acceptance = "web"
#   whatwg-failure-scored-general.csv  -- scheme_acceptance = "general"
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
  search = e$search %||% "", hash = e$hash %||% "",
  stringsAsFactors = FALSE)))
fail_inputs <- vapply(wpt$failure, function(e) e$input, character(1))

# ---- WHATWG success: canonical-output config ------------------------------
# Scored at BOTH scheme-acceptance postures, each passed explicitly.
exp_scheme <- sub(":$", "", succ$protocol)
exp_query <- sub("^\\?", "", succ$search)
exp_fragment <- sub("^#", "", succ$hash)

score_success <- function(posture) {
  cfg <- suppressWarnings(spu(
    succ$input, url_standard = "whatwg", scheme_acceptance = posture,
    scheme_policy = "require", host_encoding = "idna",
    path_encoding = "encode"))
  acc <- !rej(cfg$parse_status)
  ok <- list(
    scheme = acc & blank(cfg$scheme) == exp_scheme,
    host = acc & blank(cfg$host) == succ$hostname,
    port = acc & blank(cfg$port) == succ$port,
    path = acc & blank(cfg$path) == succ$pathname,
    query = acc & blank(cfg$query) == exp_query,
    fragment = acc & blank(cfg$fragment) == exp_fragment)
  full <- Reduce(`&`, ok)
  scored <- data.frame(
    input = succ$input, posture = posture, accepted = acc,
    scheme_ok = ok$scheme, host_ok = ok$host, port_ok = ok$port,
    path_ok = ok$path, query_ok = ok$query, fragment_ok = ok$fragment,
    full_parity = full,
    rurl_scheme = blank(cfg$scheme), exp_scheme = exp_scheme,
    rurl_host = blank(cfg$host), exp_host = succ$hostname,
    rurl_port = blank(cfg$port), exp_port = succ$port,
    rurl_path = blank(cfg$path), exp_path = succ$pathname,
    rurl_query = blank(cfg$query), exp_query = exp_query,
    rurl_fragment = blank(cfg$fragment), exp_fragment = exp_fragment,
    rurl_status = cfg$parse_status, stringsAsFactors = FALSE)
  list(posture = posture, acc = acc, ok = ok, full = full, scored = scored)
}

web <- score_success("web")

# ---- WHATWG failure: must reject ------------------------------------------
# Also scored at both postures. At "general" the ADR 0004 allowlist cannot be
# the reason a non-web-scheme row rejects, so the general column is the
# in-band evidence that the GRAMMAR rejects them.
score_failure <- function(posture) {
  fdf <- suppressWarnings(spu(fail_inputs, url_standard = "whatwg",
                              scheme_acceptance = posture,
                              scheme_policy = "require"))
  rejected <- rej(fdf$parse_status)
  list(posture = posture, rejected = rejected, scored = data.frame(
    input = fail_inputs, posture = posture, rurl_status = fdf$parse_status,
    conformant = rejected, stringsAsFactors = FALSE))
}

web_fail <- score_failure("web")
f_rejected <- web_fail$rejected

# ---- WHATWG at scheme_acceptance = "general" ------------------------------
# Same dials otherwise. This is the posture that scores the GRAMMAR over the
# whole spec corpus: no scheme is excluded by the ADR 0004 allowlist, so a
# rejection or a component mismatch here is a real conformance gap.
general <- score_success("general")
general_fail <- score_failure("general")

# ---- RFC 3986 probes ------------------------------------------------------
# Two-sided since RURL-wlqhmbdw: the set carries accept AND reject probes.
#
# `rurl_deviation` splits the reject rows into two populations that must NEVER
# be summed into one number:
#   blank      -- the RFC grammar itself rejects. rurl rejecting is CONFORMANCE.
#   non-blank  -- the RFC grammar ACCEPTS; rurl declines by policy (ADR 0004).
#                 Pinning it is regression-locking a DEPARTURE, not conformance.
# Folding the second group into a conformance score would let rurl raise its own
# "RFC conformance" by rejecting more of what the RFC allows -- the metric would
# reward the opposite of conformance. They are reported separately below.
rp <- utils::read.csv(find_file("rfc3986-probes.csv"), stringsAsFactors = FALSE)
# Not `%||%`: that helper calls is.na() on its argument, which errors on a
# vector. These are whole columns, so test for the column's absence directly.
rp$rurl_deviation <- if (is.null(rp$rurl_deviation)) {
  rep("", nrow(rp))
} else {
  blank(rp$rurl_deviation)
}
# Rows whose input cannot be written literally (control bytes) carry a JSON
# spelling in `input_json`, which is authoritative when present; `input` is then
# only a lossy rendering for diff legibility.
rp_input <- rp$input
if (!is.null(rp$input_json)) {
  ij <- blank(rp$input_json)
  has_json <- nzchar(ij)
  rp_input[has_json] <- vapply(ij[has_json], jsonlite::fromJSON, character(1))
}
rr <- suppressWarnings(spu(rp_input, url_standard = "rfc3986"))
r_acc <- !rej(rr$parse_status)
want_acc <- rp$expect == "accept"
r_accept_ok <- r_acc == want_acc
r_host_ok <- !want_acc | (r_acc & blank(rr$host) == rp$expected_host)
r_path_ok <- !want_acc | (r_acc & blank(rr$path) == rp$expected_path)
r_pass <- r_accept_ok & r_host_ok & r_path_ok

is_departure <- nzchar(rp$rurl_deviation)
conf <- !is_departure                       # rows that score as conformance
rfc_scored <- data.frame(
  id = rp$id, input = rp$input, rfc_section = rp$rfc_section,
  expect = rp$expect, is_departure = is_departure, pass = r_pass,
  rurl_status = rr$parse_status,
  rurl_host = blank(rr$host), exp_host = rp$expected_host,
  rurl_path = blank(rr$path), exp_path = rp$expected_path,
  rurl_deviation = rp$rurl_deviation,
  note = rp$note, stringsAsFactors = FALSE)

# ---- write ----------------------------------------------------------------
out_dir <- Sys.getenv("RURL_PARITY_OUT",
                      unset = file.path("_scratch", "parity"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(web$scored, file.path(out_dir, "whatwg-success-scored.csv"),
                 row.names = FALSE)
utils::write.csv(general$scored,
                 file.path(out_dir, "whatwg-success-scored-general.csv"),
                 row.names = FALSE)
utils::write.csv(web_fail$scored,
                 file.path(out_dir, "whatwg-failure-scored.csv"),
                 row.names = FALSE)
utils::write.csv(general_fail$scored,
                 file.path(out_dir, "whatwg-failure-scored-general.csv"),
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

report_success <- function(r) {
  cat("  success accepted        :", pct(r$acc), "\n")
  cat("  success FULL parity     :", pct(r$full), "\n")
  cat("    scheme", pct(r$ok$scheme), "| host", pct(r$ok$host),
      "| port", pct(r$ok$port), "| path", pct(r$ok$path),
      "| query", pct(r$ok$query), "| fragment", pct(r$ok$fragment), "\n")
}

cat("== WHATWG (canonical-output config)",
    "| scheme_acceptance = \"general\" ==\n")
cat("  the GRAMMAR figure: every WPT scheme in scope, nothing excluded\n")
report_success(general)
cat("  failure correctly reject:", pct(general_fail$rejected), "\n")
cat("  overall WHATWG acceptance conformance (general):",
    pct(c(general$acc, general_fail$rejected)), "\n\n")

cat("== WHATWG (canonical-output config)",
    "| scheme_acceptance = \"web\" ==\n")
cat("  the ADR 0004 closed allowlist (http/https/ftp/ftps/file). A lower\n")
cat("  accepted count is that allowlist working, NOT a conformance failure.\n")
report_success(web)
cat("  failure correctly reject:", pct(f_rejected), "\n")
cat(sprintf(
  "  of the accepted %d, full parity %d -- the other %d are ADR 0004\n",
  sum(web$acc), sum(web$full), sum(!web$acc)))
cat("  policy rejections, NOT conformance misses (see the general block).\n\n")

cat("== RFC 3986 (probe set, two-sided since RURL-wlqhmbdw) ==\n")
cat("  accept-conformance      :", pct(r_pass[conf & want_acc]),
    "  (RFC admits; rurl must accept + match components)\n")
cat("  reject-conformance      :", pct(r_pass[conf & !want_acc]),
    "  (RFC rejects; rurl must reject)\n")
cat("  two-sided conformance   :", pct(r_pass[conf]), "\n")
cat("  documented departures   :", pct(r_pass[is_departure]),
    "  (RFC ADMITS these; rurl declines by policy -- NOT conformance)\n")
if (any(is_departure)) {
  cat("    departures are pinned so intentional strictness cannot drift",
      "silently;\n    they are EXCLUDED from the conformance figures above",
      "on purpose.\n")
}
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

for (r in list(general, web)) {
  cat(sprintf(
    "\n== WHATWG success: non-conformances by component [%s] ==\n",
    r$posture))
  for (cc in c("scheme_ok", "host_ok", "port_ok", "path_ok",
               "query_ok", "fragment_ok")) {
    bad <- r$acc & !r$scored[[cc]]
    cat(sprintf("  %-9s %d\n", sub("_ok", "", cc, fixed = TRUE), sum(bad)))
  }
  cat("  (rejects of WPT-valid input:", sum(!r$acc), ")\n")
}

invisible(NULL)
