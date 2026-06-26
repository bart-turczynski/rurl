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

# Full package check
R CMD check .

# Lint (must stay clean — see .lintr below)
lintr::lint_package()
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

### Core Files

The `Collate` order in `DESCRIPTION` is authoritative; the load order is
`status-constants.R` → `utils.R` → `domain.R` → `path-query.R` →
`parse-phases.R` → `parse.R` → `accessors.R` → `canonical_join.R` → `zzz.R`.

- **R/parse.R** / **R/parse-phases.R** - Main parsing logic: `safe_parse_url()`, the scalar/vector wrappers and option validation (`parse.R`) and the 13 decomposed per-phase helpers `.prepare_url_for_curl()` … `.assemble_parse_result()` (`parse-phases.R`)
- **R/accessors.R** - Public `get_*()` accessor functions, all built on the shared `.extract_from_urls()` helper over `safe_parse_url()`
- **R/domain.R** - Punycode helpers and the `pslr` query seam (`.psl_registered_domain()`, `.psl_public_suffix()`, `.psl_suffix_extract()`, `.host_is_ace()`)
- **R/path-query.R** - Low-level path normalization (`._collapse_path_slashes()`, `._remove_dot_segments()`, `._strip_index_page()`, `._encode_path_segments()`) and query-string parsing (`._parse_query_string()`)
- **R/canonical_join.R** - Dataset joining by canonicalized URL keys (`canonical_join()`)
- **R/status-constants.R** - The `.STATUS_*` parse-status constants and the `.is_*_status()` predicates (incl. `.is_joinable_status()` used by `canonical_join()`)
- **R/utils.R** - The `%||%` null-coalescing operator and `.spu_result_fields` (the single source of truth for `safe_parse_urls()` result columns)
- **R/zzz.R** - Package initialization (`.onLoad`), the cache registry (`.CACHE_REGISTRY`), the memoization caches, and the public cache API (`rurl_clear_caches()`, `rurl_cache_info()`, `rurl_cache_config()`)

### Key Internal Functions

- `.normalize_and_punycode()` (R/domain.R) - IDNA/Punycode encoding with NFC normalization, used for host reconstruction
- `.punycode_to_unicode()` (R/domain.R) - per-label Punycode decoding to Unicode (lenient `puny_decode` + `iconv` sanitization)
- `.psl_registered_domain()` / `.psl_public_suffix()` (R/domain.R) - thin wrappers over `pslr::registrable_domain()` / `pslr::public_suffix()`
- `.psl_suffix_extract()` (R/domain.R) - full canonical decomposition (`subdomain` / `domain` / `suffix` / `registrable_domain`) via `pslr::suffix_extract()`, used to make STRUCTURAL policy decisions (www-prefix and subdomain-trim heuristics) on one canonical spelling
- `.host_is_ace()` (R/domain.R) - TRUE if any host label is an `xn--` A-label; drives the `host_encoding = "keep"` spelling choice in `.derive_domain_tld()`

### PSL delegation contract (R/domain.R)

`rurl` calls `pslr` with a fixed contract:

- `source` `"all"` / `"icann"` / `"private"` maps 1:1 onto `pslr` `section`.
- `output = "unicode"` by default (preserves rurl's historical decoded-IDN
  output; pslr defaults to ASCII A-labels). Structural/decision callers keep
  this default; the emitted domain/TLD path (`.derive_domain_tld()`) instead
  selects the spelling from `host_encoding` (`keep`/`idna`/`unicode`), so
  `get_domain()`/`get_tld()`/`get_subdomain()` mirror `get_host()`.
- `unknown = "na"` so an unknown TLD yields `NA` rather than pslr's implicit `*`.
- `invalid = "na"` so malformed hosts yield `NA` instead of erroring.
- Never use pslr session-global list switching (`psl_use()`) to implement
  per-request behavior (per the pslr PRD §12).

### Data Flow

1. `safe_parse_url()` is the primary workhorse - all `get_*()` functions are wrappers around it
2. Results are memoized for performance (~8x speedup for repeated URLs)
3. URL is parsed via `curl::curl_parse_url()` with fallback for malformed URLs
4. Host is normalized and Punycode-encoded via `.normalize_and_punycode()` for reconstruction
5. Domain/TLD derived via the `pslr` seam (`.psl_registered_domain()` / `.psl_public_suffix()`); pslr canonicalizes the host internally

### Performance Optimizations

- **Memoization**: `safe_parse_url()` and the Punycode encode/decode round-trips are cached in rurl; `pslr` caches its own PSL query results
- **Cache management**: Use `rurl_clear_caches()` to free memory; `rurl_cache_config()` covers `full_parse`, `puny_encode`, `puny_decode`

## Critical Constraints

**DO NOT ALTER the following:**

1. **Punycode functions** - `.normalize_and_punycode()` (host -> A-label, used by `host_encoding = "idna"`) and `.punycode_to_unicode()` (A-label -> Unicode, used by `host_encoding = "unicode"` and `get_host()`). These render the host reversibly while *preserving case* (the case policy is a separate later phase) and *tolerating* malformed-but-encodable hosts (lenient `strict = FALSE` fallback). They no longer carry per-TLD hardcoded workarounds — those were removed during the pslr migration; plain `punycoder::puny_encode`/`puny_decode` handle .ελ / .рф correctly.

   **Decision (RURL-ntdnoywx, 2026-06-20): keep these helpers; do NOT replace them with `punycoder::host_normalize()`.** `host_normalize()` is purpose-built for canonical *comparison* form (which is exactly why `pslr` applies it before PSL matching), not for rurl's reversible host *rendering*. It is not a drop-in for either helper: (a) it is one-directional with no inverse, so it cannot replace `.punycode_to_unicode()` at all; (b) on the encode path it force-lowercases (colliding with rurl's separate case policy, breaking `case_handling = "keep"`/`"upper"`) and returns `NA` for hosts rurl currently tolerates — STD3 `_`, leading/trailing hyphens, `--` in label positions 3-4 (CheckHyphens), and over-DNS-length labels. Characterization diff lives in the issue. Revisit only if rurl gains a dedicated "canonical match key" surface where lowercasing + UTS-46 strictness are desired.

2. **PSL semantics live in `pslr`** - do not reintroduce an embedded matcher or bundled list in this package; query `pslr` through the R/domain.R seam.

## Dependencies

- `curl` - URL parsing via `curl_parse_url()`
- `stringi` - Unicode string manipulation (recently migrated from base `grep`)
- `punycoder` (>= 1.2.0) - Punycode encoding/decoding
- `pslr` (>= 1.0.2) - Public Suffix List matching (domain/TLD extraction)

### Intentional base-R string exceptions

The migration to `stringi` is deliberately *not* total. The following base-R
string calls are retained on purpose — do not "finish the migration" for
consistency alone:

- **`strsplit()`** (host-label and subdomain splits on `"."`, and the path
  splits in `path-query.R`, all with `fixed = TRUE`) is **not** equivalent to
  `stringi::stri_split_fixed()`. `strsplit("a.b.", ".", fixed = TRUE)` drops the
  trailing empty (`c("a", "b")`) and returns `character(0)` for `""`, whereas
  `stri_split_fixed` keeps the trailing `""` and returns `""`. Swapping them
  would change parsing behavior, so the base calls stay. (The dot splits use
  `fixed = TRUE` rather than the regex `"\\."` — verified identical for the
  trailing-empty behavior — so `fixed_regex_linter` stays quiet too.)
- **Genuine regexes** (`gsub("/+", …)`, `sub("/?[^/]*$", …)`,
  `regexpr`/`regmatches` in `._remove_dot_segments`, `grepl("^www[0-9]*$", …)`,
  the status-pattern `grepl` in `canonical_join.R`) operate on ASCII
  delimiters/anchors where `stringi` offers no correctness or Unicode benefit.

## Test Framework

Uses testthat 3.0.0+ with edition 3 config. Test files are in `tests/testthat/`.

## Development Notes

- PSL data and refresh live in `pslr`; `rurl` ships no list of its own
- All core functions are fully vectorized
- Cache environments are initialized in `.onLoad` and persist for the R session

@FP_CLAUDE.md
