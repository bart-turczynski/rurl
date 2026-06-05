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

# Update Public Suffix List (regenerates R/sysdata.rda)
source("data-raw/update_psl.R")
```

## Architecture

### Core Files

- **R/rurl.R** - Main parsing logic: `safe_parse_url()` and all `get_*()` accessor functions
- **R/canonical_join.R** - Dataset joining by canonicalized URL keys (`canonical_join()`)
- **R/zzz.R** - Package initialization (`.onLoad`), pre-computes PSL hash sets and initializes caches

### Key Internal Functions (R/rurl.R)

- `.normalize_and_punycode()` - IDNA/Punycode encoding with NFC normalization
- `.punycode_to_unicode()` - Punycode decoding with hardcoded workarounds for problematic TLDs
- `.get_registered_domain()` - Derives registered domain using PSL rules (memoized)
- `._extract_tld_original_logic()` - TLD extraction using Public Suffix List (memoized)

### Data Flow

1. `safe_parse_url()` is the primary workhorse - all `get_*()` functions are wrappers around it
2. Results are memoized for performance (~8x speedup for repeated URLs)
3. URL is parsed via `curl::curl_parse_url()` with fallback for malformed URLs
4. Host is normalized and Punycode-encoded via `.normalize_and_punycode()`
5. Domain/TLD extracted using PSL rules loaded as hash sets (O(1) lookup) at package init

### Performance Optimizations

- **Multi-layer memoization**: `safe_parse_url()`, `.get_registered_domain()`, and TLD extraction are all cached
- **Hash sets for PSL rules**: Pre-computed at `.onLoad` for O(1) lookups instead of linear `%in%` searches
- **Cache management**: Use `rurl_clear_caches()` to free memory or reset after PSL updates

## Critical Constraints

**DO NOT ALTER the following:**

1. **Punycode functions** - `.normalize_and_punycode()`, `.punycode_to_unicode()`, and all hardcoded workarounds for problematic TLDs (Greek .ελ, Russian .рф, etc.)

2. **PSL processing logic** - The logic in `data-raw/update_psl.R` for processing the Public Suffix List must be preserved exactly

## Dependencies

- `curl` - URL parsing via `curl_parse_url()`
- `stringi` - Unicode string manipulation (recently migrated from base `grep`)
- `punycoder` - Punycode encoding/decoding

### Intentional base-R string exceptions

The migration to `stringi` is deliberately *not* total. The following base-R
string calls are retained on purpose — do not "finish the migration" for
consistency alone:

- **`strsplit()`** (host-label splits on `"\\."`, and the `fixed = TRUE`
  splits in `path-query.R`) is **not** equivalent to
  `stringi::stri_split_fixed()`. `strsplit("a.b.", ".")` drops the trailing
  empty (`c("a", "b")`) and returns `character(0)` for `""`, whereas
  `stri_split_fixed` keeps the trailing `""` and returns `""`. Swapping them
  would change parsing behavior, so the base calls stay.
- **Genuine regexes** (`gsub("/+", …)`, `sub("/?[^/]*$", …)`,
  `regexpr`/`regmatches` in `._remove_dot_segments`, `grepl("^www[0-9]*$", …)`,
  the status-pattern `grepl` in `canonical_join.R`) operate on ASCII
  delimiters/anchors where `stringi` offers no correctness or Unicode benefit.
- **`R/zzz.R` PSL hash-set construction** (`sub`/`grep` over the suffix list at
  `.onLoad`) is PSL-processing-adjacent and left untouched per the PSL
  constraint above.

## Test Framework

Uses testthat 3.0.0+ with edition 3 config. Test files are in `tests/testthat/`.

## Development Notes

- The `memoization.md` file contains the original design for performance optimization (now implemented)
- PSL data is shipped in `R/sysdata.rda` and never downloaded at runtime
- All core functions are fully vectorized
- Cache environments are initialized in `.onLoad` and persist for the R session

@FP_CLAUDE.md
