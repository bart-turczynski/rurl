#!/usr/bin/env Rscript
#
# RFC 3986 conformance sweep: the `rfc3986` profile vs the standard's own parser.
#
# WHY THIS EXISTS (RURL-xfbzkico, RURL-yeikpnan). The repo scores the WHATWG
# side against a normative oracle -- web-platform-tests `urltestdata.json`, the
# WHATWG's own suite, 336 success + 202 failure rows (test-wpt-full-suite.R).
# The RFC side had no equivalent. Every RFC-side harness scored ACCEPTANCE only
# (`rfc3986_abnf_accepts()`, a grammar matcher), and acceptance cannot see a
# DECOMPOSITION defect: `http:/evil.com` is accepted by the grammar matcher and
# by rurl, and is still wrong, because rurl reports "no authority delimiter was
# written" while simultaneously reporting `host = evil.com` and leaving
# `/evil.com` in the path.
#
# So the authority-slash family did NOT hide for lack of corpus shapes. Measured:
# 51 of the 336 WPT success rows carry a 1- or 3+-slash run, and rurl passes
# 336/336. It hid because NO corpus scored the rfc3986 profile's decomposition
# against an RFC oracle at all. WPT exercises the shapes but scores the WHATWG
# profile against WHATWG's `href`. This script closes that gap.
#
# ORACLE. `rfc3986_reference_parse()` (tests/testthat/helper-rfc3986-appendix-b.R)
# = RFC 3986 Appendix B, which SPECIFIES the parser as a regular expression, plus
# section 3.2 for the authority split. Because the decomposition is part of the
# standard rather than a reading of it, this needs no consensus-of-libraries and
# no external runtime. Cross-checked against Go net/url, Python urlsplit, Ruby
# URI, libxml2 and PHP parse_url: 5/5 agreement on the slash family.
#
# SUBSTRATE. `.fsss_record_vec()` -- the identity record -- and nothing else.
# NOT `safe_parse_urls()`, whose `host` is the PRESENTATION host (post-www,
# `host_encoding`-rendered), and NEVER `clean_url`, which is surface (c), "a
# policy-driven SEO/canonicalization product; NOT a serializer, identity,
# redirect target, or conformance oracle" and is barred as a claim substrate by
# P5.3 CLAIM-1. Scoring the presentation accessors was a measurement error made
# and corrected during the triage; it changed verdicts.
#
# The FSSS profiles already carry `scheme_acceptance = "general"`, so the ADR
# 0004 closed scheme set cannot mask conformance here. It DOES mask it through
# `safe_parse_urls()`, which is the other half of that same correction.
#
# SCOPE. Only rows the generic URI grammar admits are compared. rurl is entitled
# to reject anything else, and the ABNF oracle is explicit that "accepts" means
# only "the grammar admits this string", never "rurl is wrong to reject it".
#
# Usage:
#   Rscript tools/rfc3986-conformance-sweep.R <pkg-dir> <out.tsv>
#
# Compare two runs with plain `diff`. A row moving from `.` to a field name is a
# regression; the reverse is a fix. The summary counts are what a claim of "100%
# RFC 3986" has to be able to cite, and until every class is either fixed or
# dispositioned in a decision record, that claim is not available.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("usage: rfc3986-conformance-sweep.R <pkg-dir> <out.tsv>")
}
pkg <- normalizePath(args[[1L]], mustWork = TRUE)
out_path <- args[[2L]]

suppressMessages(devtools::load_all(pkg, quiet = TRUE))
source(file.path(pkg, "tests/testthat/helper-rfc3986-abnf.R"))
source(file.path(pkg, "tests/testthat/helper-rfc3986-appendix-b.R"))

# Every corpus the repo already scores, unioned and de-duplicated. Deliberately
# not a freshly-invented corpus: the point is that these rows were ALREADY in
# the repository while the defects went unseen, so the instrument -- not the
# inputs -- is what was missing.
fixture <- function(f, col) {
  utils::read.csv(file.path(pkg, "tests/testthat/fixtures", f),
                  stringsAsFactors = FALSE)[[col]]
}
wpt_inputs <- function() {
  path <- system.file("bench", "wpt-url-cases.json", package = "rurl")
  if (!nzchar(path) || !file.exists(path)) {
    return(character(0))
  }
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  gf <- function(rows) {
    vapply(rows, function(x) {
      v <- x[["input"]]
      if (is.null(v)) "" else v
    }, character(1))
  }
  c(gf(j$success), gf(j$failure))
}
corpus <- c(
  fixture("parse-corpus.csv", "url"),
  fixture("url-standard-conformance.csv", "input"),
  fixture("external-url-vectors.csv", "input"),
  wpt_inputs()
)
corpus <- corpus[!is.na(corpus)]
# De-duplicate on BYTES, not on the character values. `unique()` compares
# encoding-normalized strings, and `read.csv()` declares the fixture rows'
# encoding differently depending on the running locale -- so plain `unique()`
# made the CORPUS SIZE itself locale-dependent: 773 rows under en_US.UTF-8, 787
# under LC_ALL=C, from the same files. Every verdict count was identical
# (grammar-valid 519, and the same conforms/REJECTED/DIVERGES split), because the
# 14 extra rows were byte-duplicates of rows already present -- but an instrument
# whose corpus quietly changes under the environment cannot be diffed run-to-run,
# which is the whole workflow it exists to support.
corpus_hex <- vapply(corpus, function(s) {
  paste(sprintf("%02x", as.integer(charToRaw(s))), collapse = "")
}, character(1), USE.NAMES = FALSE)
corpus <- corpus[!duplicated(corpus_hex)]

ref <- rfc3986_reference_parse(corpus)
rec <- rurl:::.fsss_record_vec(corpus, "rfc3986", NULL)

# rurl reports presence through `*_kind`; the oracle through NA-vs-"". Project
# the record onto the oracle's convention, so a reported mismatch is a real
# disagreement about the URL and not a difference of bookkeeping.
kind_proj <- function(kind, val) {
  ifelse(kind == "absent", NA_character_, ifelse(kind == "empty", "", val))
}
ne <- function(a, b) {
  (is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
}

# RFC 3986 section 6.2.2.1 (Case Normalization) makes the scheme and the host
# case-INSENSITIVE and endorses folding both to lowercase. rurl does; Appendix B
# is a splitter and preserves the source case. Comparing the two raw therefore
# reports a divergence where rurl is following the standard -- measured: it
# scored `HTTP://EXAMPLE.COM/` as a scheme defect. Fold both sides for exactly
# these two components, and only these two: path, query, fragment and userinfo
# are case-SENSITIVE, so folding them would MASK real defects.
#
# The fold is done BYTE-WISE, over 0x41-0x5A only. Not `tolower()`, which is
# locale-dependent (the Turkish-I problem) and would make this instrument's
# verdicts vary by locale -- the exact failure it exists to rule out. Not
# `chartr()` either: it decodes its input, and the corpus carries hosts that are
# not valid UTF-8, on which it throws ("invalid input in 'utf8towcs'"). A
# conformance instrument must survive every byte sequence a URL can hold,
# including the ill-formed ones, because those are precisely the interesting rows.
fold_ascii <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    b <- charToRaw(s)
    i <- b >= as.raw(0x41L) & b <= as.raw(0x5AL)
    if (any(i)) b[i] <- as.raw(as.integer(b[i]) + 0x20L)
    rawToChar(b)
  }, character(1), USE.NAMES = FALSE)
}

in_scope <- ref$valid
accepted <- in_scope & rec$ok
cmp <- list(
  scheme = ne(fold_ascii(ref$scheme), fold_ascii(rec$scheme)),
  authority = ref$authority_present != rec$authority_delimiter_present,
  host = ne(fold_ascii(ref$host),
            fold_ascii(kind_proj(rec$host_kind, rec$host))),
  userinfo = ne(ref$userinfo, rec$userinfo),
  path = ne(ref$path, rec$path),
  query = ne(ref$query, kind_proj(rec$query_kind, rec$query)),
  fragment = ne(ref$fragment, kind_proj(rec$fragment_kind, rec$fragment))
)
cmp <- lapply(cmp, function(v) accepted & !is.na(v) & v)

verdict <- ifelse(
  !in_scope, "out-of-scope",
  ifelse(!rec$ok, "REJECTED",
         ifelse(Reduce(`|`, cmp), "DIVERGES", "conforms"))
)
fields <- vapply(seq_along(corpus), function(i) {
  hit <- names(cmp)[vapply(cmp, `[`, logical(1), i)]
  if (length(hit) == 0L) "." else paste(hit, collapse = ",")
}, character(1))

# EVERY variable-content column is escaped, not just the input. Applying this to
# the input alone -- as this script first did -- left raw LF bytes in the `host`
# and `path` columns, which split 20 rows across multiple lines and produced a
# TSV whose `verdict` column held fragments like `s`, `0` and `TRUE`. A malformed
# instrument that still prints a plausible summary is the worst kind.
#
# Escaping is to PRINTABLE ASCII, so the output is byte-identical under any
# locale. Writing non-ASCII through unescaped made the two runs differ in 133
# lines purely by rendering (`例え.jp` vs `<e4><be><8b>...`) while every verdict
# was in fact identical -- a diff that reports a difference where there is none
# trains its reader to ignore it.
ascii_escape <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) {
      return("<NA>")
    }
    b <- as.integer(charToRaw(s))
    parts <- ifelse(b >= 0x20L & b <= 0x7EL & b != 0x5CL,
                    vapply(b, function(i) rawToChar(as.raw(i)), character(1)),
                    sprintf("\\x%02x", b))
    paste(parts, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}
hexed <- vapply(corpus, function(s) {
  paste(sprintf("%02x", as.integer(charToRaw(s))), collapse = "")
}, character(1), USE.NAMES = FALSE)
shw <- function(v) ascii_escape(as.character(v))
lines <- c(
  paste("input_hex", "verdict", "fields", "oracle_authority", "rurl_authority",
        "oracle_host", "rurl_host", "oracle_path", "rurl_path", sep = "\t"),
  paste(hexed, verdict, fields,
        shw(ref$authority), shw(rec$authority_delimiter_present),
        shw(ref$host), shw(kind_proj(rec$host_kind, rec$host)),
        shw(ref$path), shw(rec$path), sep = "\t")
)
writeLines(lines, out_path)

n_scope <- sum(in_scope)
cat(sprintf("corpus=%d grammar-valid=%d locale=%s -> %s\n",
            length(corpus), n_scope, Sys.getlocale("LC_CTYPE"), out_path))
cat(sprintf("  %-14s %d\n", "conforms", sum(verdict == "conforms")))
cat(sprintf("  %-14s %d  (grammar-valid, rurl rejects)\n", "REJECTED",
            sum(verdict == "REJECTED")))
cat(sprintf("  %-14s %d  (accepted, decomposed differently)\n", "DIVERGES",
            sum(verdict == "DIVERGES")))
cat("  by field:\n")
for (nm in names(cmp)) {
  cat(sprintf("    %-12s %d\n", nm, sum(cmp[[nm]])))
}
cat(sprintf("  RFC 3986 conformance: %d/%d = %.1f%%\n",
            sum(verdict == "conforms"), n_scope,
            100 * sum(verdict == "conforms") / n_scope))
