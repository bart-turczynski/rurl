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

**GitLab CI is paused** — the free-tier compute allowance is exhausted, so
`.gitlab-ci.yml` creates a pipeline only for a hand-triggered web run
(RURL-psqmlgjf). Nothing verifies a push server-side. `tools/local-ci.sh` runs
the same CI jobs in the same image against a clean clone of a commit, which is
where environment-shaped defects live that a fully-populated local library hides:

```sh
tools/local-ci.sh --list          # which jobs apply to this ref, and why
tools/local-ci.sh                 # run them (needs Docker)
git fetch origin main && tools/local-ci.sh --all origin/main   # after a merge
```

Run it against `origin/main` after merging: a squash-merge produces a commit
that has never existed on any machine, and the pipeline that used to check it is
the one that is off. **`--all` is required** — the `check` job's rules select a
tag or a hand-started pipeline only, so without it you get the cheap half and no
`R CMD check --as-cran`. The runner is pull-based, so it does not restore the
property that mattered most about a real runner — that verification was not
opt-in.

The expensive `check` job runs on the forge only at release time: on a tag, or
on a pipeline started by hand as the finishing touch after `/cran` has been
worked through. It is deliberately not part of `/cran`, which stays local and
cheap.

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
