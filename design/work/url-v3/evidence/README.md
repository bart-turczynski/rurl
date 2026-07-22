# rurl 3.0 protocol — frozen evidence archive

This directory is the **raw-evidence archive** graduated out of gitignored
`_scratch/` by gate task **G0.1** (`RURL-wmoncyux`), under the workspace and
"raw-evidence archive exception" fixed by decision record
[`../decisions/P0.2-workspace-manifest-format.md`](../decisions/P0.2-workspace-manifest-format.md)
(§1–§2).

## Contents

Eleven frozen inputs, copied **verbatim** (byte-for-byte) from `_scratch/`:

| Identity | File | Origin in `_scratch/` |
|---|---|---|
| S1 | `S1-object-state-vector.md` | `orchestrate/v3-protocol-review/` |
| S2 | `S2-standards-validation-repair.md` | `orchestrate/v3-protocol-review/` |
| S3 | `S3-serialization-format-encoding.md` | `orchestrate/v3-protocol-review/` |
| S4 | `S4-cleaning-mutation.md` | `orchestrate/v3-protocol-review/` |
| S5 | `S5-keys-joins.md` | `orchestrate/v3-protocol-review/` |
| S6 | `S6-schemes-email.md` | `orchestrate/v3-protocol-review/` |
| S7 | `S7-host-idna-psl-ip.md` | `orchestrate/v3-protocol-review/` |
| S8 | `S8-performance-curl-conformance-migration.md` | `orchestrate/v3-protocol-review/` |
| S9 | `S9-process-red-team.md` | `orchestrate/v3-protocol-review/` |
| PROTOCOL | `url-v3-spec-reconstruction-protocol.md` | `_scratch/` |
| DRAFT-PRD | `prd-url-object-grammar-DRAFT.md` | `_scratch/` |

## Immutability

These files are **read-only frozen inputs**. Once graduated they are never
edited; they are referenced by stable identity and checksum only. Their SHA-256
values match the reconciliation
[`../protocol-review-reconciliation.md`](../protocol-review-reconciliation.md)
§2 ("Frozen review bundle") byte-for-byte, and are re-pinned in the manifest at
G0.2 (`RURL-qcduzpex`).

## Verifying integrity

```sh
cd design/work/url-v3/evidence
shasum -a 256 -c SHA256SUMS.txt
```

`SHA256SUMS.txt` lists the eleven frozen sources only (it does not list itself).
The G0.4 fresh-clone restore drill (`RURL-zyksymya`) re-runs this check.

## What this task does *not* do

Graduation is a verbatim freeze, **not** evidence transfer. Rewriting the stale
ADR/PRD reference paths embedded in the reports (reconciliation §10) is owned by
the G1 transfer issue (`RURL-jalwcgzk`), which produces the finding registers —
it does not touch these frozen files.
