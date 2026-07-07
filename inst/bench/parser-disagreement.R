#!/usr/bin/env Rscript
# Cross-parser URL-parsing disagreement harness (RURL-wncwfasl).
#
# NOT a test and NOT run on CRAN. This is a standalone, reproducible correctness
# study: it runs a fixed set of URL parsers over a shared corpus, extracts a
# normalized set of comparable components, and tabulates WHERE THEY DIVERGE.
# The output is the paper's headline table -- "here is where rurl (RFC vs
# WHATWG), the R incumbents, and a WHATWG reference parser disagree on the same
# corpus, and why the choice of url_standard matters."
#
#   Rscript inst/bench/parser-disagreement.R
#   Rscript -e 'devtools::load_all(); source("inst/bench/parser-disagreement.R")' # nolint: line_length_linter.
#
# Outputs (CSV) are written to $RURL_BENCH_OUT (default: _scratch/disagreement,
# which is gitignored -- the SCRIPT is the versioned, citable artifact; the
# tables are regenerated):
#   disagreement-long.csv     one row per (input x parser): status + components
#   disagreement-matrix.csv   one row per input: per-parser verdict + which
#                             components diverge (the headline table)
#   per-axis-summary.csv      divergence counts/ratios grouped by axis
#
# ----------------------------------------------------------------------------
# FRAMING (keep honest -- reviewers will check; see RURL-wncwfasl):
#   * rurl is a NORMALIZATION/POLICY layer over libcurl + pslr + punycoder, not
#     a new parser. The url_standard selector governs a SUBSET of axes (path
#     percent/dot, host IPv4/reg-name, case, port elision, backslash) -- NOT
#     IDNA rendering or query handling.
#   * This is a correctness/divergence study, not a performance study.
#
# COMPARISON MODEL:
#   Divergence is scored on four comparable components: scheme, host, port,
#   path. query/fragment are captured in the long output for completeness but
#   are NOT scored (query handling is outside the selector's remit).
#   Normalization applied before comparison, and WHY:
#     scheme  -- lowercased, trailing ":" stripped (adaR reports "http:").
#     host    -- verbatim (case and trailing-dot ARE divergence axes; do not
#                fold them away).
#     port    -- absent (NA / "") -> ""; a retained "80" vs an elided ""
#                default port IS the divergence axis, so it is preserved.
#     path    -- leading "/" added when missing (urltools drops it on EVERY
#                path -- a formatting quirk that would otherwise drown the real
#                signal); dot-segment resolution is NOT normalized because that
#                resolution difference is itself an axis.
#   A parser that rejects/errors on an input contributes the sentinel verdict
#   "<reject>" / "<error>" for every component, so accept-vs-reject (the most
#   important divergence, e.g. WHATWG rejecting an out-of-range integer host)
#   is captured naturally by distinct-value counting.
# ----------------------------------------------------------------------------

# ---- resolve rurl (installed or dev tree) ----------------------------------
if (!requireNamespace("rurl", quietly = TRUE) &&
    !exists("safe_parse_urls", mode = "function")) {
  stop("rurl not available: install it or run after devtools::load_all().")
}
spu <- if (exists("safe_parse_urls", mode = "function")) {
  get("safe_parse_urls")
} else {
  rurl::safe_parse_urls
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- component normalizers -------------------------------------------------
norm_scheme <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  sub(":$", "", tolower(x))
}
norm_host <- function(x) ifelse(is.na(x), "", as.character(x))
norm_port <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", "", x)
}
norm_path <- function(x) {
  # empty / NA path means root; collapse to "/" so the urltools quirk of
  # reporting an empty path for "/" does not falsely diverge on every URL.
  x <- ifelse(is.na(x) | x == "", "/", as.character(x))
  ifelse(startsWith(x, "/"), x, paste0("/", x))
}

# A parser "did not produce components" only when it truly rejected the input.
# rurl warnings (warning-no-tld, warning-invalid-tld, ...) mean "parsed WITH a
# caveat" -- the host/path are real and must be compared, not sentinel-ized;
# the warning itself is preserved in the long table's status column.
rejected <- function(status) status %in% c("error", "reject")
strip_lead <- function(x, ch) {
  x <- ifelse(is.na(x), "", as.character(x))
  sub(paste0("^", ch), "", x)
}

empty_row <- function(n) {
  data.frame(
    scheme = character(n), host = character(n), port = character(n),
    path = character(n), query = character(n), fragment = character(n),
    status = character(n), stringsAsFactors = FALSE
  )
}

# ---- adapters: each maps a character vector of URLs to a normalized frame ---
adapt_rurl <- function(urls, standard) {
  df <- suppressWarnings(spu(urls, url_standard = standard))
  data.frame(
    scheme   = norm_scheme(df$scheme),
    host     = norm_host(df$host),
    port     = norm_port(df$port),
    path     = norm_path(df$path),
    query    = ifelse(is.na(df$query), "", df$query),
    fragment = ifelse(is.na(df$fragment), "", df$fragment),
    status   = df$parse_status,
    stringsAsFactors = FALSE
  )
}

adapt_curl <- function(urls) {
  out <- empty_row(length(urls))
  for (i in seq_along(urls)) {
    r <- tryCatch(curl::curl_parse_url(urls[[i]]), error = function(e) NULL)
    if (is.null(r)) {
      out$status[i] <- "error"
      next
    }
    params <- r$params
    query <- if (length(params)) {
      paste(names(params), unname(params), sep = "=", collapse = "&")
    } else {
      ""
    }
    out$scheme[i]   <- norm_scheme(r$scheme %||% "")
    out$host[i]     <- norm_host(r$host %||% "")
    out$port[i]     <- norm_port(r$port %||% "")
    out$path[i]     <- norm_path(r$path %||% "")
    out$query[i]    <- query
    out$fragment[i] <- r$fragment %||% ""
    out$status[i]   <- "ok"
  }
  out
}

adapt_urltools <- function(urls) {
  df <- urltools::url_parse(urls)
  # urltools accepts everything -- it never rejects. That permissiveness is
  # itself a finding, so status is always "ok".
  data.frame(
    scheme   = norm_scheme(df$scheme),
    host     = norm_host(df$domain),
    port     = norm_port(df$port),
    path     = norm_path(df$path),
    query    = ifelse(is.na(df$parameter), "", df$parameter),
    fragment = ifelse(is.na(df$fragment), "", df$fragment),
    status   = "ok",
    stringsAsFactors = FALSE
  )
}

adapt_adar <- function(urls) {
  df <- adaR::ada_url_parse(urls, decode = FALSE)
  data.frame(
    scheme   = norm_scheme(df$protocol),
    host     = norm_host(df$hostname),
    port     = norm_port(df$port),
    path     = norm_path(df$pathname),
    query    = strip_lead(df$search, "\\?"),
    fragment = strip_lead(df$hash, "#"),
    # ada echoes href even on failure but nulls the structural components;
    # protocol is NA iff the parse failed (a valid URL always has a scheme).
    status   = ifelse(is.na(df$protocol), "reject", "ok"),
    stringsAsFactors = FALSE
  )
}

# ---- participants ----------------------------------------------------------
# rurl (both profiles) and curl are REQUIRED; adaR and urltools are included
# when installed (adaR is the only genuinely WHATWG-conformant R parser).
participants <- list(
  `rurl(rfc3986)` = function(u) adapt_rurl(u, "rfc3986"),
  `rurl(whatwg)`  = function(u) adapt_rurl(u, "whatwg"),
  `curl`          = adapt_curl
)
if (!requireNamespace("curl", quietly = TRUE)) {
  stop("curl is a required participant but is not installed.")
}
if (requireNamespace("adaR", quietly = TRUE)) {
  participants[["adaR"]] <- adapt_adar
} else {
  message("NOTE: adaR not installed (install.packages('adaR')); ",
          "the WHATWG reference column will be missing.")
}
if (requireNamespace("urltools", quietly = TRUE)) {
  participants[["urltools"]] <- adapt_urltools
} else {
  message("NOTE: urltools not installed; the RFC-ish incumbent column ",
          "will be missing.")
}

# ---- corpus ----------------------------------------------------------------
# (1) Curated axis-probe set: the paper's "interesting columns". Each row is a
#     hand-picked adversarial input tagged with its divergence axis + citation.
probe <- function(axis, input, ref) {
  data.frame(axis = axis, input = input, source_reference = ref,
             stringsAsFactors = FALSE)
}
axis_probes <- rbind(
  probe("pct-unreserved", "http://ex.com/%41%42",
        "RFC 3986 S2.3 / WHATWG S1.3 (unreserved should decode)"),
  probe("pct-reserved", "http://ex.com/a%2Fb",
        "RFC 3986 S2.2 (reserved slash stays data)"),
  probe("ipv4-whole-decimal", "http://2130706433/",
        "WHATWG host parser (=127.0.0.1); RFC reg-name"),
  probe("ipv4-hex", "http://0x7f.0.0.1/",
        "WHATWG IPv4 parser hex form"),
  probe("ipv4-octal", "http://0177.0.0.1/",
        "WHATWG IPv4 parser octal form"),
  probe("ipv4-short-form", "http://127.1/",
        "WHATWG IPv4 short form (=127.0.0.1)"),
  probe("ipv4-overflow", "http://999999999999/",
        "WHATWG host parser MUST reject (out of range)"),
  probe("host-trailing-dot", "http://example.com./",
        "RFC 3986 reg-name / WHATWG host"),
  probe("backslash-as-slash", "http:\\\\example.com\\a",
        "WHATWG special-scheme treats \\ as /; RFC rejects"),
  probe("default-port-elision", "http://ex.com:80/",
        "Default port for http elided by WHATWG serializer"),
  probe("idn-unicode-host", "http://münchen.de/",
        "IDNA / Punycode host rendering"),
  probe("idn-punycode-host", "http://xn--mnchen-3ya.de/",
        "IDNA / Punycode host rendering"),
  probe("dot-segment", "http://ex.com/a/./b/../c",
        "RFC 3986 S5.2.4 / WHATWG remove-dot-segments"),
  probe("empty-path", "http://ex.com",
        "Empty path vs '/' normalization"),
  probe("www-prefix", "http://www.ex.com/",
        "www prefix handling"),
  probe("userinfo", "http://user:pass@ex.com/",
        "userinfo component handling"),
  probe("c0-tab", "http://ex\tample.com/",
        "WHATWG strips ASCII tab/CR/LF; rurl rejects (ADR 0004 boundary)")
)

# (2) The versioned WPT-provenanced conformance corpus, tagged with a coarse
#     axis derived from the fixture id.
classify_axis <- function(base) {
  if (grepl("^path-reserved", base)) return("pct-reserved")
  if (grepl("^path-unreserved", base)) return("pct-unreserved")
  if (grepl("^path-(encoded|dot-encoded|literal-dots|rfc-5-4)", base)) {
    return("dot-segment")
  }
  if (grepl("^path-non-ascii", base)) return("non-ascii-path")
  if (grepl("^path-space", base)) return("space-in-path")
  if (grepl("^host-canonical-ipv4", base)) return("ipv4-canonical")
  if (grepl("^host-whole-decimal", base)) return("ipv4-whole-decimal")
  if (grepl("^host-whole-hex", base)) return("ipv4-hex")
  if (grepl("^host-(whole-octal|dotted-octal)", base)) return("ipv4-octal")
  if (grepl("^host-short-form", base)) return("ipv4-short-form")
  if (grepl("^host-out-of-range", base)) return("ipv4-overflow")
  if (grepl("^host-over", base)) return("ipv4-over-arity")
  "other"
}
axis_from_id <- function(id) {
  base <- sub("-(rfc3986|rfc|whatwg)$", "", id)
  vapply(base, classify_axis, character(1), USE.NAMES = FALSE)
}

corpus <- axis_probes
fixture <- file.path("tests", "testthat", "fixtures",
                     "url-standard-conformance.csv")
if (file.exists(fixture)) {
  cf <- utils::read.csv(fixture, stringsAsFactors = FALSE)
  cf <- cf[!duplicated(cf$input), , drop = FALSE]
  corpus <- rbind(corpus, data.frame(
    axis = axis_from_id(cf$id),
    input = cf$input,
    source_reference = cf$source_reference,
    stringsAsFactors = FALSE
  ))
} else {
  message("NOTE: conformance fixture not found at ", fixture,
          " -- running curated probes only.")
}

# (3) External adversarial/conformance vectors imported from third-party suites
#     (RURL-dbazixkr). Runnable rows only; axis is the coarse source label so
#     the per-axis summary keeps imported cases separable from curated probes.
ext_fixture <- file.path("tests", "testthat", "fixtures",
                         "external-url-vectors.csv")
if (file.exists(ext_fixture)) {
  ef <- utils::read.csv(ext_fixture, stringsAsFactors = FALSE,
                        na.strings = "NA")
  ef <- ef[!is.na(ef$input) & ef$runnable == "yes", , drop = FALSE]
  corpus <- rbind(corpus, data.frame(
    axis = paste0("ext-", ef$source),
    input = ef$input,
    source_reference = ef$source_reference,
    stringsAsFactors = FALSE
  ))
}

corpus <- corpus[!duplicated(corpus$input), , drop = FALSE]
inputs <- corpus$input

# ---- run every participant over the corpus ---------------------------------
runs <- lapply(participants, function(fn) fn(inputs))

# ---- long table ------------------------------------------------------------
components <- c("scheme", "host", "port", "path")
long <- do.call(rbind, lapply(names(runs), function(p) {
  r <- runs[[p]]
  data.frame(
    input = inputs, axis = corpus$axis,
    source_reference = corpus$source_reference,
    parser = p, status = r$status,
    scheme = r$scheme, host = r$host, port = r$port, path = r$path,
    query = r$query, fragment = r$fragment,
    stringsAsFactors = FALSE
  )
}))

# ---- per-component verdict + divergence scoring ----------------------------
# verdict(input, parser, component): the sentinel-or-value used for comparison.
verdict <- function(comp) {
  vapply(names(runs), function(p) {
    r <- runs[[p]]
    ifelse(rejected(r$status), paste0("<", r$status, ">"), r[[comp]])
  }, character(length(inputs)))
  # returns a matrix: rows = inputs, cols = parsers
}
verdicts <- lapply(components, verdict)
names(verdicts) <- components

# a component diverges on an input when it has >1 distinct verdict value
diverges <- vapply(components, function(comp) {
  apply(verdicts[[comp]], 1L, function(v) length(unique(v)) > 1L)
}, logical(length(inputs)))
colnames(diverges) <- components
divergent_components <- apply(diverges, 1L, function(row) {
  paste(components[row], collapse = ",")
})
any_divergence <- rowSums(diverges) > 0L

# ---- matrix (headline table) -----------------------------------------------
# one row per input; per-parser compact verdict "scheme|host|port|path".
# warnings get a trailing marker so rurl's facts-not-policy diagnostics stay
# visible in the headline table without being mistaken for a rejection.
compact <- vapply(names(runs), function(p) {
  r <- runs[[p]]
  cell <- paste(r$scheme, r$host, r$port, r$path, sep = "|")
  cell <- ifelse(grepl("^warning", r$status),
                 paste0(cell, " [", r$status, "]"), cell)
  ifelse(rejected(r$status), paste0("<", r$status, ">"), cell)
}, character(length(inputs)))
matrix_tbl <- data.frame(
  input = inputs, axis = corpus$axis,
  divergent_components = divergent_components,
  stringsAsFactors = FALSE, check.names = FALSE
)
matrix_tbl <- cbind(
  matrix_tbl, as.data.frame(compact, stringsAsFactors = FALSE)
)
names(matrix_tbl)[seq(4L, ncol(matrix_tbl))] <- names(runs)

# ---- per-axis summary ------------------------------------------------------
axes <- unique(corpus$axis)
per_axis <- do.call(rbind, lapply(axes, function(ax) {
  sel <- corpus$axis == ax
  data.frame(
    axis = ax,
    n_inputs = sum(sel),
    n_divergent = sum(any_divergence[sel]),
    ratio = round(mean(any_divergence[sel]), 3L),
    diverging_components = paste(
      components[colSums(diverges[sel, , drop = FALSE]) > 0L], collapse = ","
    ),
    stringsAsFactors = FALSE
  )
}))
per_axis <- per_axis[order(-per_axis$ratio, -per_axis$n_divergent), ]

# ---- write artifacts -------------------------------------------------------
out_dir <- Sys.getenv("RURL_BENCH_OUT", unset = file.path("_scratch",
                                                         "disagreement"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
utils::write.csv(long, file.path(out_dir, "disagreement-long.csv"),
                 row.names = FALSE)
utils::write.csv(matrix_tbl, file.path(out_dir, "disagreement-matrix.csv"),
                 row.names = FALSE)
utils::write.csv(per_axis, file.path(out_dir, "per-axis-summary.csv"),
                 row.names = FALSE)

# ---- environment / version manifest ----------------------------------------
libcurl <- tryCatch(curl::curl_version()$version, error = function(e) NA)
pkg_ver <- function(p) {
  tryCatch(as.character(utils::packageVersion(p)), error = function(e) "absent")
}
cat("cross-parser URL-parsing disagreement harness (RURL-wncwfasl)\n")
cat(strrep("=", 74), "\n", sep = "")
cat(sprintf("  R              %s\n", getRversion()))
cat(sprintf("  platform       %s\n", R.version$platform))
cat(sprintf("  libcurl        %s\n", libcurl))
for (p in c("rurl", "curl", "adaR", "urltools", "pslr", "punycoder")) {
  cat(sprintf("  %-14s %s\n", p, pkg_ver(p)))
}
cat(sprintf("\n  corpus         %d inputs across %d axes\n",
            length(inputs), length(axes)))
cat(sprintf("  participants   %s\n", toString(names(runs))))
cat(sprintf("  output dir     %s\n", normalizePath(out_dir)))

cat("\nper-axis divergence summary (most-divergent first):\n")
print(per_axis, row.names = FALSE)

cat(sprintf("\ndivergent inputs: %d of %d (%.0f%%)\n",
            sum(any_divergence), length(inputs),
            100 * mean(any_divergence)))
cat("\ndisagreement matrix (divergent rows only; cell = scheme|host|port|path,",
    "\n  or <status> when the parser did not accept the input):\n\n", sep = "")
show <- matrix_tbl[any_divergence, , drop = FALSE]
old <- options(width = 200L)
print(show, row.names = FALSE, right = FALSE)
options(old)

invisible(NULL)
