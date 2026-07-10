# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rurl` is an R package for parsing, normalizing, cleaning, and joining URLs. It provides vectorized, pipe-friendly functions with fine-grained control over protocols, www prefixes, case handling, and trailing slashes. Uses the Public Suffix List (PSL) for accurate domain/TLD extraction.

## Build and Test Commands

```r
# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-accessors.R")

# Build documentation (Roxygen2)
devtools::document()

# Rebuild README.md; requires pak metadata/network access for Remotes.
devtools::build_readme()

# Full package check
R CMD check .
# In network-restricted local sandboxes only, R may be unable to verify the
# current time and emit a future-timestamp NOTE; rerun with normal network
# access, or set `_R_CHECK_SYSTEM_CLOCK_=false` for that local check.

# Lint (must stay clean — see .lintr below)
lintr::lint_package()

# HTML coverage report (defaults to _scratch/covr_report.html)
Rscript tools/coverage-report.R
```

`.lintr` is deliberately kept in sync with the linter set
`goodpractice::gp()` runs, so a local `lintr::lint_package()` reproduces the
findings surfaced in review/CI. The header of `.lintr` documents every
intentional deviation (e.g. `object_name_linter`/`expect_identical_linter`
off for published mixed-case API params and testthat idioms) — read it before
"fixing" a lint or adding a linter. CI runs in `.github/workflows/` (`verify`
is the headline gate; also `full-check`, `pkgdown`, `rhub`, `news-version`).

PSL data is no longer maintained in this repo. Domain/TLD extraction is
delegated to the `pslr` package, which owns the list, its parsing, and any
refresh (`pslr::psl_refresh()`).

## Architecture

The structural reference — load order, file/responsibility map, key internal
functions, the PSL delegation contract, the parse data flow, the Stage-A/Stage-B
split, and the caches — lives in **[`ARCHITECTURE.md`](ARCHITECTURE.md)**. Read
it before working on the parse pipeline; the durable detail is maintained there,
not duplicated here.

The *why* behind load-bearing decisions is recorded as Architecture Decision
Records in **[`design/adr/`](design/adr/)**; accepted specs live in
**[`design/prd/`](design/prd/)**. Notable ADRs: 0001 (delegate PSL to `pslr`),
0002 (keep the Punycode helpers), 0003 (parse/present Stage split), 0004 (strict
host-shape gate), 0005 (base-R string exceptions), 0006 (diagnostics as
companion helpers only), 0007 (`url_standard` selector), 0008 (this docs home +
PRD graduation policy), 0009 (`whatwg` host-charset shim — accept the 15 ASCII
host code points libcurl rejects but WHATWG keeps), 0010 (`scheme_policy` —
the input-acceptance axis; `require` rejects scheme-less input), 0011
(`path_encoding` is an orthogonal presentation axis, not governed by
`url_standard` — un-governs it and splits path identity from presentation).
Design PRDs are drafted in
gitignored `_scratch/` and
graduate to `design/prd/` once accepted and depended upon (ADR 0008).

## Critical Constraints

**DO NOT ALTER the following** (full rationale in the linked ADRs):

1. **Punycode functions** — do not alter `.normalize_and_punycode()` /
   `.punycode_to_unicode()` (R/domain.R) to force-lowercase or reject tolerated
   hosts, and do NOT replace them with `punycoder::host_normalize()`. They
   render the host reversibly, preserve case (case policy is a separate phase),
   and tolerate malformed-but-encodable hosts. See **ADR 0002**.
2. **PSL semantics live in `pslr`** — do not reintroduce an embedded matcher or
   bundled list; query `pslr` through the R/domain.R seam. See **ADR 0001**.
3. **Intentional base-R string exceptions** — the `stringi` migration is
   deliberately not total; do not "finish the migration" on the retained
   `strsplit(..., fixed = TRUE)` dot/path splits or the ASCII regexes. They are
   not `stringi`-equivalent. See **ADR 0005**.
4. **Diagnostics are companion-helpers only** — never widen
   `safe_parse_url(s)` columns/fields; surface metadata via `get_host_type()` /
   `get_scheme_class()` / `get_url_diagnostics()`. See **ADR 0006**.

## Dependencies

- `curl` - URL parsing via `curl_parse_url()`
- `stringi` - Unicode string manipulation (recently migrated from base `grep`)
- `punycoder` (>= 1.2.0) - Punycode encoding/decoding
- `pslr` (>= 1.0.2) - Public Suffix List matching (domain/TLD extraction)

## Test Framework

Uses testthat 3.0.0+ with edition 3 config. Test files are in `tests/testthat/`.

## Development Notes

- PSL data and refresh live in `pslr`; `rurl` ships no list of its own
- All core functions are fully vectorized
- Cache environments are initialized in `.onLoad` and persist for the R session

@FP_CLAUDE.md
