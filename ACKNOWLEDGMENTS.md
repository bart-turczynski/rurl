# Acknowledgments

These packages stand on a great deal of other people's work — code I import,
data I serve, standards I follow, and research I build on. This file records
the debts.

## The work that pointed the way

The packages I learned from before writing my own:

- **urltools** (Oliver Keyes, Jay Jacobs, Drew Schmidt) — the R package I
  reached for first, and the reason `rurl` exists.
- **hrbrmstr/punycode** (Bob Rudis) — the R package whose `puny_*` /
  `is_punycode()` surface `punycoder` descends from before rebuilding it on
  `libidn2` with explicit UTS #46 processing.
- **xsitemap** (Florian Joly and contributors) — the prior R take on sitemaps
  that `sitemapr` measured itself against.
- **ultimate-sitemap-parser** (Python) — the parser whose behavior `sitemapr`
  used as a parity oracle.

## The people behind the projects I work on

These packages are an ecosystem — **rurl**, **pslr**, **punycoder**,
**pagerankr**, and **sitemapr** — that lean on one another: `rurl` builds on
`punycoder` and `pslr`; `pagerankr` and `sitemapr` build on `rurl`. My thanks
go to everyone who has reviewed, tested, reported against, or otherwise helped
shape any of them. The work is better for it.

## What I rely on directly

The third-party libraries I import at runtime:

- **curl** (Jeroen Ooms) — URL parsing and transfers, wrapping **libcurl**
  (Daniel Stenberg and contributors).
- **stringi** (Marek Gagolewski and contributors) — Unicode text handling, on
  top of **ICU** (the International Components for Unicode).
- **xml2** (Posit) — XML parsing and XSD validation, wrapping **libxml2**
  (Daniel Veillard and contributors).
- **httr2** (Hadley Wickham and Posit) — HTTP fetching, redirects, and retries.
- **htmltools**, **rlang**, and **tibble** (Posit and the tidyverse authors) —
  report rendering, tidy evaluation, and the output row model.
- **igraph** (Gábor Csárdi, Tamás Nepusz, and contributors) — the graph engine
  behind `pagerankr`, itself carrying **PRPACK** (David Gleich and others),
  **ARPACK**, and BLAS/LAPACK.
- **Rcpp** (Dirk Eddelbuettel, Romain François, and contributors) — the R/C++
  bridge in `punycoder`.
- **cpp11** (Davis Vaughan, Jim Hester, and Posit) — the R/C++ bridge in
  `pslr`'s matcher.
- **GNU libidn2** (Simon Josefsson and contributors) — the optional native
  Punycode backend in `punycoder`.

And the tooling that keeps all of this tested, documented, and honest:
**testthat**, **knitr**, **rmarkdown**, **withr**, **roxygen2**, **pkgdown**,
**lintr**, **goodpractice**, **covr**, **cucumber**, **digest**, **air**, and
the **oysteR** / **rosv** vulnerability auditors.

## The data I serve

- **The Public Suffix List** and the volunteers at Mozilla and across the
  community who maintain it. `pslr` bundles a pinned snapshot as its source of
  truth for registrable domains, under the Mozilla Public License 2.0. See
  <https://publicsuffix.org>.
- **The Unicode Consortium**, for the Unicode Character Database, the UTS #46
  specification, and the IDNA conformance test data (`IdnaTestV2`) that
  `punycoder` is built and tested against.
- **The Sitemaps.org protocol and Google's sitemap-extension documentation**,
  which `sitemapr`'s bundled schemas model (clean-room, not copied).
- **jparise/chrome-utm-stripper** and the **AdGuard** filter maintainers, whose
  published tracking-parameter lists seeded and sanity-checked `rurl`'s query
  denylist.

## Standards and specifications

Much of this code exists to implement, or faithfully defer to, published
standards. Some are cited by name in the source; others are only implied by the
behavior. Both are recorded here.

### URIs and URLs

- **RFC 3986** — Uniform Resource Identifier (URI): Generic Syntax.
- **RFC 3987** — Internationalized Resource Identifiers (IRIs).
- **RFC 2396** — URI: Generic Syntax (obsoleted by 3986, but part of the
  lineage the grammar traces back to). <https://www.ietf.org/rfc/rfc2396.txt>
- **RFC 1738** / **RFC 1808** — Uniform Resource Locators and Relative URLs.
- **The WHATWG URL Standard** — the living host-parsing and percent-encoding
  model `rurl` targets.
- **Web Platform Tests** (`web-platform-tests/wpt`) — the WHATWG conformance
  corpus (`urltestdata.json`) `rurl` validates against.

### Internationalized domain names and Unicode

- **RFC 3492** — Punycode, the Bootstring encoding for IDNA.
- **RFC 5890 / 5891 / 5892 / 5893 / 5894** — the IDNA2008 suite (definitions,
  protocol, tables, the Bidi rule, and rationale).
- **RFC 3490 / 3491 / 3454** — IDNA2003, Nameprep, and Stringprep, the
  superseded regime `punycoder` deliberately does not follow.
- **UTS #46** — Unicode IDNA Compatibility Processing.
- **UAX #15** — Unicode Normalization Forms (NFC).
- **UAX #44** — the Unicode Character Database.
- **UTS #39** / **UTR #36** — Unicode Security Mechanisms and Considerations
  (referenced as scope boundaries, not implemented).
- **RFC 8753** — IDNA review for new Unicode versions.
- **RFC 3629** — UTF-8.

### Hosts, DNS, and IP addresses

- **RFC 952** / **RFC 1123** — host-name syntax (the STD 3 ASCII rules).
- **RFC 1034** / **RFC 1035** — domain-name concepts and the label/length limits.
- **RFC 791** — the IPv4 dotted-quad presentation form.
- **RFC 4291** / **RFC 5952** — IPv6 addressing architecture and canonical text
  representation.
- **RFC 6874** — IPv6 zone identifiers in URLs.
- **RFC 2606** / **RFC 6761** — reserved and special-use domain names.
- **RFC 1918 / 6598 / 3927 / 4193 / 6052** — the private, shared, link-local,
  unique-local, and IPv4-embedded-IPv6 ranges behind `sitemapr`'s SSRF guard.

### The Public Suffix List

- **The Public Suffix List format and prevailing-rule algorithm** — the normal,
  wildcard, and exception rules, matched right-to-left with longest-match and
  exception-beats-wildcard precedence.

### Link analysis and the web graph

The research `pagerankr` implements:

- **Brin & Page (1998)** and **Page, Brin, Motwani & Winograd (1999)** —
  PageRank and the random-surfer model.
- **Kleinberg (1999)** — HITS (hubs and authorities).
- **Lempel & Moran (2001)** — SALSA.
- **Gyöngyi, Garcia-Molina & Pedersen (2004)** — TrustRank.
- **Haveliwala (2002)** — Topic-Sensitive PageRank.
- **Kamvar, Haveliwala & Golub (2004)** — PageRank convergence.
- **Langville & Meyer (2004)** — *Deeper Inside PageRank*.
- **Boldi, Santini & Vigna (2005)** — PageRank as a function of the damping
  factor.

### Sitemaps, crawling, and documents

- **The Sitemaps.org Protocol 0.9** — the core `<urlset>` / `<sitemapindex>`
  format and its limits.
- **Google's sitemap extensions** — Image 1.1, News 0.9, Video 1.1, PageMap
  1.0, and the `xhtml:link` hreflang convention.
- **RFC 9309** — the Robots Exclusion Protocol (the `Sitemap:` directive).
- **RFC 9110 / 9111** — HTTP semantics and caching.
- **W3C XML 1.0**, **XML Namespaces**, and **XML Schema 1.0 (XSD)**.
- **The W3C Date and Time Formats profile of ISO 8601** — `lastmod` parsing.
- **ISO 639**, **ISO 3166-1 alpha-2**, and **BCP 47 (RFC 5646)** — the language
  and region codes referenced by the sitemap extensions.
- **RFC 1952** — the gzip file format.

## R, CRAN, and the compiler ecosystem

Thanks to the **R Core Team**, whose language and base facilities (`utils`,
`tools`, the connection and compression layers) carry all of this; to the
**CRAN** volunteers, whose standards and checks make a package trustworthy; and
to the **C and C++ communities** behind the native backends — libcurl, ICU,
libidn2, libxml2, PRPACK/ARPACK — and the Rcpp and cpp11 bridges that let R
reach them.

## AI-assisted development

These packages were built with the help of AI development tools. **Claude**
(Anthropic), through Claude Code, was the primary development assistant and is
credited as co-author across the commit history; **Codex** (OpenAI) served as a
design reviewer and caught real errors in the harder specifications. The design
decisions, the standards conformance, and the responsibility for what ships are
mine.

## Alternatives

If one of my packages isn't the right fit, here's other work worth knowing
about.

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
- **psl** (Rust) — <https://crates.io/crates/psl>
- **tldextract** (Python) — <https://github.com/john-kurkowski/tldextract>
- **publicsuffixlist** (Python) — <https://pypi.org/project/publicsuffixlist/>
- **public_suffix** (Ruby) — <https://github.com/weppos/publicsuffix-ruby>
- **php-domain-parser** (PHP) — <https://github.com/jeremykendall/php-domain-parser>
- **tldts** (JS/TS) — <https://github.com/remusao/tldts>
- **Guava `InternetDomainName`** (Java) — <https://github.com/google/guava>

### Instead of `pagerankr`

- **igraph** `page_rank()` (R/C) — <https://github.com/igraph/igraph>
- **NetworkX** (Python) — <https://github.com/networkx/networkx>

### Instead of `sitemapr`

- **ultimate-sitemap-parser** (Python) — <https://github.com/GateNLP/ultimate-sitemap-parser>
- **advertools** (Python) — <https://github.com/eliasdabbas/advertools>
- **xsitemap** (R) — <https://github.com/pixgarden/xsitemap>

## A note on licenses

This file is about gratitude, not legal terms. The binding third-party license
and attribution notices live alongside the code in each package — see that
package's `THIRD_PARTY_NOTICES.md`, `inst/NOTICE`, and `LICENSE` files. In
particular, the Public Suffix List data bundled in `pslr` is licensed under the
Mozilla Public License 2.0, separately from the MIT-licensed package code.
