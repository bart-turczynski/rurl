# rurl 0.1.3

## Improvements

- Removed the dependency on the `psl` package.
- Implemented an internal registered domain extraction using the Public Suffix List.
- Added internal `update_psl.R` script to fetch and process the PSL during development.
- Improved test coverage to 100%.
- Cleaned up exports and internal helpers.

## Documentation

- README updated to reflect the use of the PSL and internal domain logic.
- LICENSE and attribution clarified for MIT + Mozilla Public Suffix List.

# rurl 0.1.2

## Stabilization & Coverage

- Achieved **100% test coverage**.
- Added examples to all exported functions.
- Improved documentation (`@param`, `@return`, etc.) for CRAN compliance.
- Cleaned up `NAMESPACE` and removed unnecessary functions like `hello()`.
- Refined URL parsing logic and improved output consistency.

# rurl 0.1.1

# rurl 0.1.0

- All `get_*()` functions are now vectorized and work on character vectors.
- Deprecated scalar-only behavior.
- Internal parsing made more robust using `curl` and `psl`.
- Ready for use in `mutate()` and other tidy workflows.
