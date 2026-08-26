# rurl design docs

Durable, tracked design documentation for `rurl`. Unlike the working-tree
`_scratch/` directory (gitignored — never on a fresh clone or in CI), everything
here is committed. The whole `design/` tree is excluded from the package build
via `.Rbuildignore`, so it never affects `R CMD check` or the pkgdown site.

## Layout

- [`adr/`](adr/) — Architecture Decision Records: one file per load-bearing
  decision, capturing the *why* and a status (accepted / superseded). Start
  from [`adr/0000-adr-template.md`](adr/0000-adr-template.md).
- [`prd/`](prd/) — accepted Product Requirement Documents that an epic or
  shipped feature depends on, graduated out of `_scratch/` so they survive.
- [`work/`](work/) — durable control-plane artifacts for active, multi-step
  design work when a fresh clone must be able to recover the process state.
  These documents must state their status explicitly and are not accepted PRDs.
- [`goodpractice-triage.md`](goodpractice-triage.md) — standing triage for
  `goodpractice::gp()` findings that are intentionally retained or queued as
  scoped cleanup debt.
- [`backup-mirror.md`](backup-mirror.md) — standing record for the local
  archival mirror: how it stays fresh, why its `refs/remotes/origin/*` is a
  frozen pre-migration GitHub snapshot, and the triage of the branch tips that
  exist only there.
- [`measurement-traps.md`](measurement-traps.md) — cross-cutting method note on
  how a green test, gate or harness returns a plausible wrong number. Read
  before building or extending any instrument.
- [`oracle-pinning.md`](oracle-pinning.md) — the standing method behind the
  oracle record's pins: verification precedes the pin string, anchors beat
  section numbers, network checks stay out of the gate list.
- [`oracle-fixtures.md`](oracle-fixtures.md) — how to read and edit
  `tests/testthat/fixtures/external-url-vectors.csv`: which columns are
  posture-scoped claims, and which are characterization.
- [`release-chain.md`](release-chain.md) — the seven-package CRAN submission
  order and the sibling-pinning rule that protects it.

**Design docs cannot live in `docs/`.** That directory is pkgdown's **output**
(`build_site_github_pages`, `clean = TRUE`), so anything written there is
deleted on the next site build. Source design documents therefore live
top-level and under `design/`, both `.Rbuildignore`d.

See also [`../ARCHITECTURE.md`](../ARCHITECTURE.md) for the structural overview
(load order, file map, data flow, seams, caches).

## PRD graduation policy (ADR 0008)

PRDs are drafted and iterated in gitignored `_scratch/`. Once a PRD is
**accepted and an epic/feature depends on it**, it graduates into `prd/` with a
short provenance header so the durable spec travels with the code. Research
notes, brainstorms, and handoffs stay ephemeral in `_scratch/`; active process
controls that must survive a handoff live in `work/`; the ADRs are the durable
distillate of *why*, and the graduated PRD is the durable *what*.

## Writing an ADR

1. Copy `adr/0000-adr-template.md` to `adr/NNNN-short-slug.md` (next number).
2. Fill in Context / Decision / Consequences; set Status to `Accepted`.
3. When a later ADR overturns it, set this one's Status to
   `Superseded by ADR-NNNN` rather than deleting it — the history is the point.
4. Link the ADR from `ARCHITECTURE.md` where the structure it governs is
   described.
