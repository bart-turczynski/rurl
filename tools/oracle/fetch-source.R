#!/usr/bin/env Rscript
# RURL-ozdejfzl -- the shared hash-verified source resolver for tier-2 groups.
#
# WHAT A TIER-2 GROUP IS. Three of the seven groups in
# tests/testthat/fixtures/external-url-vectors.csv were imported from upstream
# JSON that this repository does NOT vendor: wpt-urltestdata,
# ada-extra-urltestdata and ada-verifydnslength. Their provenance record pins a
# revision AND a raw_source_sha256, so the bytes can be re-fetched and VERIFIED
# rather than trusted -- which is the whole reason a tier-2 verifier is possible
# at all. tools/oracle/README.md's tier table is the reference.
#
# WHY THE PINS ARE READ AND NOT COPIED. Every coordinate here comes out of
# tests/testthat/fixtures/oracle-provenance.json at run time. Hard-coding the
# revision or the digest in this file would create a second copy of the pin, and
# the record's own `no_second_copy` convention exists because a second copy is
# what drifts. It also puts the failure in the right place: change the pin in
# the record and the fetch follows it, so a pin that names bytes which do not
# exist fails HERE, loudly, instead of being discovered years later.
#
# FAIL CLOSED MEANS ABORT. If the bytes cannot be resolved -- offline, upstream
# down, a digest that does not match -- this signals a condition and the caller
# exits non-zero with the exact command a human can run. It never returns a
# partial or empty source: deriving a short block from a truncated download is
# how a gate reports PASS over an oracle it did not actually check.
#
# NO PACKAGE IS LOADED. Like every other module in this directory, there is no
# route from here to rurl's own answer. jsonlite and digest are read-only tools.

ORACLE_PROVENANCE <- "tests/testthat/fixtures/oracle-provenance.json"

# Exit status 2 is reserved for "the upstream source could not be resolved",
# distinct from status 1 = "the committed oracle disagrees with upstream". A
# reader who sees 2 has a connectivity or pin problem; a reader who sees 1 has a
# fixture problem. Collapsing them would make an outage look like a defect.
EXIT_SOURCE_UNAVAILABLE <- 2L

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

# ---- the pin ----------------------------------------------------------------

# Pull one group's section-2.3 source coordinates out of the provenance record.
# Every field it needs is required of an in-scope group by gate rule PV5, so a
# missing one is a record defect and is reported as such rather than defaulted.
oracle_source_pin <- function(group, record = ORACLE_PROVENANCE) {
  if (!file.exists(record)) {
    stop("FATAL: provenance record not found: ", record, call. = FALSE)
  }
  rec <- jsonlite::fromJSON(record, simplifyVector = FALSE)
  hit <- NULL
  for (fx in rec$fixtures) {
    for (g in fx$source_groups %||% list()) {
      if (identical(g$group, group)) hit <- g
    }
  }
  if (is.null(hit)) {
    stop("FATAL: no group '", group, "' in ", record,
         " -- this resolver was asked for a pin that the record does not carry.",
         call. = FALSE)
  }
  need <- c("upstream_project", "upstream_revision", "upstream_path",
            "raw_source_sha256")
  absent <- need[!vapply(need, function(k) {
    v <- hit[[k]]
    !is.null(v) && is.character(v) && nzchar(v) && !grepl("^MISSING\\[", v)
  }, logical(1))]
  if (length(absent)) {
    stop("FATAL: group '", group, "' cannot be fetched -- ",
         paste(absent, collapse = ", "),
         " is absent or still a MISSING[...] sentinel in ", record,
         call. = FALSE)
  }
  url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s",
                 hit$upstream_project, hit$upstream_revision, hit$upstream_path)
  list(group = group,
       project = hit$upstream_project,
       revision = hit$upstream_revision,
       path = hit$upstream_path,
       sha256 = hit$raw_source_sha256,
       url = url,
       command = sprintf("curl -fsSL %s -o %s", url, basename(hit$upstream_path)))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# A pin that is not in the record: the second anchor of ada-extra-urltestdata is
# a revision the record names in prose rather than in the section-2.3 fields,
# because it is not the group's import pin. Built here so the fetch path, the
# digest check and the abort message stay identical for both kinds.
oracle_source_pin_literal <- function(group, project, revision, path, sha256) {
  url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s",
                 project, revision, path)
  list(group = group, project = project, revision = revision, path = path,
       sha256 = sha256, url = url,
       command = sprintf("curl -fsSL %s -o %s", url, basename(path)))
}

# ---- resolution -------------------------------------------------------------

oracle_source_cache_dir <- function() {
  Sys.getenv("RURL_ORACLE_SOURCE_DIR", unset = file.path("_scratch",
                                                         "oracle-sources"))
}

# Content-addressed, so a cache entry can never be served for a different pin:
# the digest is in the name AND re-checked after reading. A cache keyed by file
# name would happily hand back last month's bytes after a re-pin.
cache_path <- function(pin) {
  file.path(oracle_source_cache_dir(),
            sprintf("%s-%s", substr(pin$sha256, 1L, 12L), basename(pin$path)))
}

source_unavailable <- function(pin, why) {
  cond <- structure(
    class = c("oracle_source_unavailable", "error", "condition"),
    list(message = sprintf(
      paste0("upstream source for '%s' could not be resolved: %s\n",
             "  pinned  : %s @ %s\n  path    : %s\n  sha256  : %s\n",
             "  fetch it: %s\n",
             "  then re-run with RURL_ORACLE_SOURCE_DIR pointing at its ",
             "directory, or place it at %s"),
      pin$group, why, pin$project, pin$revision, pin$path, pin$sha256,
      pin$command, cache_path(pin)),
      call = NULL))
  stop(cond)
}

# Resolve a pin to a local file whose bytes hash to the recorded digest.
#
# Order: the cache first, then the network unless RURL_ORACLE_OFFLINE is set.
# The digest is verified in BOTH paths -- a truncated or half-written cache
# entry is exactly as dangerous as a corrupted download, and the cache is the
# path that gets reused.
oracle_source_file <- function(pin, quiet = FALSE) {
  cached <- cache_path(pin)
  if (file.exists(cached)) {
    got <- sha256_file(cached)
    if (identical(got, pin$sha256)) {
      if (!quiet) cat(sprintf("source         : cache %s (sha256 verified)\n",
                              cached))
      return(cached)
    }
    # Do not delete it: a cache entry whose digest moved is evidence, and
    # silently replacing it would erase the only trace of what happened.
    source_unavailable(pin, sprintf(
      "the cached file %s hashes to %s, not the pinned digest -- move it aside",
      cached, got))
  }

  if (nzchar(Sys.getenv("RURL_ORACLE_OFFLINE"))) {
    source_unavailable(pin, "RURL_ORACLE_OFFLINE is set and the cache has no entry")
  }

  dir.create(dirname(cached), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(cached, ".part")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({
    utils::download.file(pin$url, destfile = tmp, quiet = TRUE, mode = "wb")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(tmp)) {
    source_unavailable(pin, "the fetch failed (offline, or upstream unreachable)")
  }
  got <- sha256_file(tmp)
  if (!identical(got, pin$sha256)) {
    # This is the interesting failure: a revision is supposed to be immutable,
    # so a digest mismatch means either the record's pin is wrong or the bytes
    # at that revision are not what was recorded. Neither is a retry.
    source_unavailable(pin, sprintf(
      "the fetched bytes hash to %s, NOT the pinned digest -- the pin and the upstream revision disagree",
      got))
  }
  file.rename(tmp, cached)
  if (!quiet) cat(sprintf("source         : fetched %s @ %s (sha256 verified)\n",
                          pin$path, substr(pin$revision, 1L, 12L)))
  cached
}

# ---- the NUL shim -----------------------------------------------------------
#
# THE TRAP THIS CLOSES, WHICH IS SILENT AND WOULD HAVE SCORED A CONFIDENT PASS.
# Three of the 267 wpt-urltestdata inputs contain U+0000, written as the JSON
# escape \u0000 in
# both the upstream JSON and the fixture's input_json cell. An R character
# vector CANNOT hold a NUL, and jsonlite does not error on one -- it TRUNCATES:
#
#     jsonlite::fromJSON('"sc://a\\u0000b/"')  ==>  "sc://a"
#
# So a re-location check written the obvious way compares "sc://a" on one side
# with "sc://a" on the other, agrees, and reports a pass it did not earn: after
# truncation any two inputs sharing a prefix before their NUL are equal, and the
# entire remainder of the input -- the part those three rows exist to record --
# is not compared at all.
#
# The fix is to never decode a NUL. The \u0000 ESCAPE is rewritten in the JSON
# SOURCE TEXT to \ue000 -- U+E000, a Private Use Area code point R holds
# happily -- before
# either side is parsed. The substitution is applied identically to the upstream
# bytes and to the fixture cells, so the comparison stays exact and injective;
# it is a transport encoding for the comparison only and nothing is written back.
#
# Two things make it safe, and both are asserted rather than assumed: no input
# on either side already contains U+E000 (measured: zero occurrences), and the
# escape is never itself escaped upstream (`\\u0000`, a literal backslash
# followed by "u0000", measured: zero occurrences). Either would make the
# rewrite lossy, so either aborts.
NUL_ESCAPE <- "\\u0000"
NUL_SHIM_ESCAPE <- "\\ue000"
NUL_SHIM_CHAR <- "\ue000"

json_text_nul_safe <- function(text, what) {
  if (any(grepl("\\\\\\\\u0000", text))) {
    stop("FATAL: ", what, " contains an ESCAPED \\\\u0000 sequence -- the NUL ",
         "shim would rewrite a literal backslash and corrupt the comparison.",
         call. = FALSE)
  }
  if (any(grepl(NUL_SHIM_ESCAPE, text, fixed = TRUE)) ||
      any(grepl(NUL_SHIM_CHAR, text, fixed = TRUE))) {
    stop("FATAL: ", what, " already contains U+E000 -- the NUL shim is no ",
         "longer injective and a shimmed NUL would be indistinguishable from ",
         "real data.", call. = FALSE)
  }
  gsub(NUL_ESCAPE, NUL_SHIM_ESCAPE, text, fixed = TRUE)
}

# Read a tier-2 upstream JSON as the list of its test-case OBJECTS. Both WPT
# urltestdata format and Ada's files interleave plain comment STRINGS between
# the objects; those carry no expectation and are dropped here so every caller
# indexes the same population.
oracle_source_cases <- function(pin, quiet = FALSE) {
  path <- oracle_source_file(pin, quiet = quiet)
  text <- json_text_nul_safe(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    sprintf("upstream %s", pin$path))
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  if (!is.list(parsed) || !length(parsed)) {
    source_unavailable(pin, "the resolved file did not parse as a non-empty JSON array")
  }
  cases <- Filter(function(e) is.list(e) && !is.null(e$input), parsed)
  if (!length(cases)) {
    source_unavailable(pin, "the resolved file carries no test-case objects")
  }
  cases
}

# A caller's boilerplate, in one place: run `expr`, and turn an unresolvable
# source into exit status 2 with the pin's own instructions.
with_oracle_source <- function(expr) {
  withCallingHandlers(
    expr,
    oracle_source_unavailable = function(e) {
      cat("\n== upstream source unavailable ==\n")
      cat(conditionMessage(e), "\n", sep = "")
      cat("\nORACLE RE-LOCATION: NOT RUN (source unavailable)\n")
      quit(status = EXIT_SOURCE_UNAVAILABLE)
    })
}
