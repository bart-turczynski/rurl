# Record template — the post-ADR-0014 shape

Copy this file for any new record under `design/work/url-v3/` that is not an
ADR (ADRs use [`../../adr/0000-adr-template.md`](../../adr/0000-adr-template.md)).
It is the answer to "what does a record look like now that
[ADR 0014](../../adr/0014-retire-the-v3-control-plane.md) retired the control
plane": a record is **accepted when it is merged to `main`**, carries no
lifecycle, no `state:` field, no `content_hash`, no `approval_evidence`, and no
`## Inputs` sha256 table. Keep the whole thing short enough to read in one
sitting; the claim table is the only part a gate reads.

**This file must not live under `decisions/`** — `validate-records.R` globs
`decisions/*.md` as owner-decision records and requires frontmatter there.

---

# <Record title>

| Field | Value |
|---|---|
| id | `<family>-<slug>` (for example `rul-credential-handling`) |
| tracked_location | `design/work/url-v3/<dir>/<file>.md` |
| purpose | one sentence: what question this record settles, and for whom |
| owning issue | `RURL-xxxxxxxx` |
| validation_command | the gate or test that would go red if this record were wrong, or `none` (say so) |

## Claims

One row per claim. The **last cell is the claim state** and must be exactly
`SETTLED` or `OPEN`. For a record under `contracts/`, `traceability-gate.R`
reads that column to build the census and `validate-records.R` requires every
`OPEN` row to be named under `## Open cells` below; keep the same shape
everywhere so a record can move into `contracts/` without rewriting.

| id | claim | evidence (file:line, test name, fixture row, or clause) | state |
|---|---|---|---|
| `<abbr>-1` | … | … | SETTLED |

## Open cells

List every `OPEN` claim id with the issue that carries it, or write `none`.

## Consequences

What moves if the claims hold: tests to re-pin, NEWS entry, contract rows,
fixture columns. Name files; do not restate their contents.
