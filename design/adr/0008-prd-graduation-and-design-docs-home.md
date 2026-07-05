# ADR 0008: Durable design-docs home; graduate accepted PRDs out of `_scratch/`

- **Status:** Accepted
- **Date:** 2026-07-05
- **Tracking:** RURL-bxmrybvo

## Context

rurl's design knowledge was fragmented and largely non-durable. `_scratch/` is
gitignored (`git check-ignore` confirmed), so PRDs (url_standard v1/v2, query,
MCP), research notes, brainstorms, and `*-HANDOFF.md` files never reached CI, a
fresh clone, or a new contributor/agent. `CLAUDE.md` doubled as an architecture
doc, and decisions lived only in commit messages, fp issues, and agent memory —
none discoverable from the repo itself. An epic (RURL-eqzkkohm) depended on a
PRD that a fresh clone could not see.

A hard constraint shaped the location: pkgdown builds its site into `docs/` and
uploads it as the GitHub Pages artifact (`build_site_github_pages`, `clean =
TRUE`), so `docs/` is not available for source design docs.

## Decision

- **`ARCHITECTURE.md`** (top-level, tracked) is the structural reference; the
  durable architecture content moved there and `CLAUDE.md` points at it rather
  than duplicating it.
- **`design/adr/`** holds Architecture Decision Records (this file included),
  with `0000-adr-template.md` as the template. ADRs capture *why* + status;
  superseded ADRs are marked, not deleted.
- **`design/prd/`** is the durable home for accepted PRDs. A PRD graduates from
  `_scratch/` into `prd/` once it is accepted **and** an epic/feature depends
  on it, carrying a short provenance header. Research/brainstorm/handoff notes
  stay ephemeral in `_scratch/`.
- The `url_standard` v1 and v2 PRDs are graduated now (an epic depends on them).
- `ARCHITECTURE.md` and the whole `design/` tree are added to `.Rbuildignore`
  so they never trip `R CMD check` or enter the package build.

## Consequences

- Design docs survive on a fresh clone and in CI; docs tickets (v1/v2
  `url_standard` docs) can cite tracked `design/prd/*` instead of gitignored
  paths.
- **Standing rule:** when a PRD is accepted and depended upon, graduate it to
  `design/prd/`; when a load-bearing decision is made, record an ADR. Keep
  `CLAUDE.md` pointing at `ARCHITECTURE.md`, not re-duplicating it.
- The graduated PRDs are point-in-time specs; where they and the code disagree,
  the code and the relevant ADR win. Companion research remains in the
  working-tree `_scratch/`.
