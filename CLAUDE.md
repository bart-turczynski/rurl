# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`rurl` is an R package for parsing, normalizing, and cleaning URLs. It provides vectorized, pipe-friendly functions with fine-grained control over protocols, www prefixes, case handling, and trailing slashes. Uses the Public Suffix List (PSL) for accurate domain/TLD extraction.

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
- **R/permute_url.R** - URL permutation generation (`permute_url()`)
- **R/permutation_join.R** - Dataset joining by URL permutations (`permutation_join()`)
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
- `urltools` - Punycode encoding/decoding

## Test Framework

Uses testthat 3.0.0+ with edition 3 config. Test files are in `tests/testthat/`.

## Development Notes

- The `memoization.md` file contains the original design for performance optimization (now implemented)
- PSL data is shipped in `R/sysdata.rda` and never downloaded at runtime
- All core functions are fully vectorized
- Cache environments are initialized in `.onLoad` and persist for the R session
