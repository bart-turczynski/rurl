# Acknowledgments

This file records the work I lean on. It's shared, word for word, across
all three packages, so the thanks below apply to the whole ecosystem.

## The work that pointed the way

The biggest thanks go to the work I learned from:

- **urltools** (Oliver Keyes and contributors) — the package I reached
  for first, and the reason `rurl` exists.
- **hrbrmstr/punycode** (Bob Rudis) — the R package whose `puny_*` /
  `is_punycode()` surface `punycoder` descends from before rebuilding it
  on `libidn2` with explicit UTS #46 processing.

## What I rely on directly

The libraries I import:

- **Rcpp** (Dirk Eddelbuettel, Romain François, and contributors) — the
  R/C++ bridge in `punycoder`.
- **cpp11** (Davis Vaughan, Jim Hester, and Posit) — the R/C++ bridge in
  `pslr`'s matcher.
- **stringi** (Marek Gagolewski and contributors, on top of ICU) and
  **curl** (Jeroen Ooms) — the text and transfer machinery in `rurl`.
- **GNU libidn2** (Simon Josefsson and contributors) — the optional
  native Punycode backend in `punycoder`.
- **testthat**, **knitr**, **rmarkdown**, and **withr** — what keeps all
  three tested and documented.

The data I serve:

- **The Public Suffix List** and the volunteers at Mozilla and across the
  community who maintain it. `pslr` bundles a pinned snapshot as its
  source of truth for registrable domains. See <https://publicsuffix.org>.
- **The Unicode Consortium**, for the Unicode Character Database, the
  UTS #46 specification, and the IDNA conformance test data that
  `punycoder` is built and tested against.

And thanks to the R Core Team and CRAN's volunteers, whose standards make
the rest of this possible.

## Alternatives

If one of my packages isn't the right fit, here's other work worth
knowing about.

### Instead of `rurl`

- **urltools** (R) — <https://github.com/Ironholds/urltools>
- **httr2** URL helpers (R) — <https://github.com/r-lib/httr2>
- **furl** (Python) — <https://github.com/gruns/furl>

### Instead of `punycoder`

- **hrbrmstr/punycode** (R) — <https://github.com/hrbrmstr/punycode>
- **idna** (Python) — <https://github.com/kjd/idna>
- **punycoder** (Dart) — <https://pub.dev/packages/punycoder>
- **simonmittag/punycoder** (Go) — <https://github.com/simonmittag/punycoder>

### Instead of `pslr`

- **libpsl** (C) — <https://github.com/rockdaboot/libpsl>
- **publicsuffix** (Go) — <https://pkg.go.dev/golang.org/x/net/publicsuffix>
- **publicsuffix-go** (Go) — <https://github.com/weppos/publicsuffix-go>
- **tldextract** (Python) — <https://github.com/john-kurkowski/tldextract>
- **publicsuffixlist** (Python) — <https://pypi.org/project/publicsuffixlist/>

## A note on licenses

This file is about gratitude, not legal terms. The binding third-party
license and attribution notices live alongside the code in each package —
see that package's `THIRD_PARTY_NOTICES.md`, `inst/NOTICE`, and `LICENSE`
files. In particular, the Public Suffix List data bundled in `pslr` is
licensed under the Mozilla Public License 2.0, separately from the
MIT-licensed package code.
