## Test environments

* local: macOS 14.4 (Apple Silicon), R 4.4.1
* GitHub Actions: Ubuntu (via rhub GitHub Actions workflow)
* rhub::check() results:
  - macOS
  - Windows
  - Linux
  - Ubuntu release
* devtools::check_win_devel()

## R CMD check results

All checks passed with no ERRORs or WARNINGs across all platforms.

There was a single NOTE on Windows:

> Possibly misspelled words in DESCRIPTION:  
> Rurl (3:8)

This is a false positive. "Rurl" refers to the package name. It is capitalized in the `Title` field to conform to CRAN policy that package titles should be in title case. The package name is otherwise consistently styled as `rurl`.

## Test coverage

Full coverage (100%) as verified with covr::package_coverage().

## Additional QA

* Code linted with `lintr::lint_package()` — only minor line-length issues over 80 characters noted.
* Passed `goodpractice::gp()` with no actionable issues.

## Submission summary

This is the first CRAN submission of the `rurl` package.

`rurl` is a lightweight and pipe-friendly URL builder for R. It helps users construct and manage complex HTTP endpoints in a vectorized and readable way, with minimal dependencies.

The package has been tested across macOS, Windows, and Linux. It includes full documentation, unit tests, and examples.

No compiled code, no system dependencies, and no use of non-CRAN packages.
