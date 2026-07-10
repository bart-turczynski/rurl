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
- [`goodpractice-triage.md`](goodpractice-triage.md) — standing triage for
  `goodpractice::gp()` findings that are intentionally retained or queued as
  scoped cleanup debt.

See also [`../ARCHITECTURE.md`](../ARCHITECTURE.md) for the structural overview
(load order, file map, data flow, seams, caches).

## PRD graduation policy (ADR 0008)

PRDs are drafted and iterated in gitignored `_scratch/`. Once a PRD is
**accepted and an epic/feature depends on it**, it graduates into `prd/` with a
short provenance header so the durable spec travels with the code. Research
notes, brainstorms, and handoffs stay ephemeral in `_scratch/`; the ADRs are
the durable distillate of *why*, and the graduated PRD is the durable *what*.

## Writing an ADR

1. Copy `adr/0000-adr-template.md` to `adr/NNNN-short-slug.md` (next number).
2. Fill in Context / Decision / Consequences; set Status to `Accepted`.
3. When a later ADR overturns it, set this one's Status to
   `Superseded by ADR-NNNN` rather than deleting it — the history is the point.
4. Link the ADR from `ARCHITECTURE.md` where the structure it governs is
   described.
