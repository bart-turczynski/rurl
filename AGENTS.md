# AGENTS.md

`rurl` is an R package for parsing, normalizing, cleaning, and joining URLs —
standards-conformant parsing (WHATWG / RFC 3986 profiles), with domain and
public-suffix extraction delegated to `pslr`.

## Verifying a change

```sh
Rscript tools/verify.R      # full gate; --fast = gates + lint, --list = plan
```

`tools/verify.R` reproduces CI's fast gate locally and is the bar a delivered
slice has to clear. `--fast` is iteration feedback, never sufficient on its own.
`devtools::test()` is not a substitute: it ignores `Collate:` and runs against
the source tree, so a green suite can hide a package that does not build. The
gate runs as a pre-push hook once you run `pre-commit install --hook-type
pre-push` in your clone — committing the config does not install it.

`lintr::lint_package()` must stay clean. `.lintr` mirrors the linter set
`goodpractice::gp()` runs, and its header documents every intentional
deviation — read that header before "fixing" a lint or adding a linter.

## Protected code

Four areas are frozen by ADR. Read the ADR before editing:

- Punycode helpers in `R/domain.R` — [ADR 0002](design/adr/0002-keep-punycode-helpers.md)
- PSL delegation seam in `R/domain.R` — [ADR 0001](design/adr/0001-delegate-psl-to-pslr.md)
- The retained base-R string operations — [ADR 0005](design/adr/0005-intentional-base-r-string-exceptions.md)
- `safe_parse_url()` columns; diagnostics stay companion helpers — [ADR 0006](design/adr/0006-diagnostics-companion-helpers-only.md)

## Where things live

- [ARCHITECTURE.md](ARCHITECTURE.md) — load order, file map, parse data flow,
  the Stage-A/Stage-B split, seams, caches. Read before working on the parser.
- [CONTRIBUTING.md](CONTRIBUTING.md) — issue and PR workflow, validation policy,
  CRAN release checklist.
- [design/](design/) — ADRs for *why* a decision holds, accepted PRDs for *what*
  was specified.
