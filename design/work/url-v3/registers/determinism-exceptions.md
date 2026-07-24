# Register: determinism-exceptions (§6 artifact 11 — determinism slice)

<!-- variant: determinism-exception (NEW register variant; not yet in
     schema/record-schemas.yaml). Row-based, append-only. Its schema is defined
     below and enforced at runtime by tools/determinism/compare-gate.R (the gate
     reads this register and fail-closes on any malformed row). A future
     control-plane snapshot adds a validate-records.R section and a
     record-schemas.yaml entry for this variant and pins it present:true in the
     manifest, exactly as the §6 contract registers were sealed at cp-snapshot-2
     / the contracts at cp-snapshot-3. Until then no validator globs it.

     This register is the ONLY channel by which the determinism acceptance gate
     (P5.2 / C-09) grants tolerance. There is no fuzzy margin: a divergence is
     either exactly zero, or a specific approved exception here. -->

## Envelope

| Field | Value |
|---|---|
| id | reg-determinism-exceptions |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/registers/determinism-exceptions.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — an exception is an owner authorization act (P0.1 authority) |
| lifecycle_state | PROPOSED |
| dependencies | P5.2 (determinism tolerance + exception policy); tools/determinism/compare-gate.R (the consuming gate); .github/workflows/determinism-gate.yml |
| completion_rule | every row carries the full schema below; the gate fail-closes on any malformed/incomplete row; validate-records.R (determinism-exceptions section, added at a future G4 snapshot) passes |
| content_hash | pinned at a future G4 control-plane snapshot; the gate hashes nothing of itself |
| approval_evidence | pending — pins at a future G4 snapshot (NOT an envelope flip) |
| validation_command | Rscript tools/determinism/compare-gate.R --self-test |

## Purpose

Records each *known, justified, owner-approved* determinism divergence so the
gate passes for it while still failing on any **new** divergence. It implements
P5.2 part 3 verbatim: tolerance is granted only through named, bounded, expiring
exception rows — never an ambient margin.

**Steady state is empty.** The frozen 2026-07-20 sweep reduced all comparable
dumps to a single MD5 (`3db0d6e9…`) — zero divergence. Zero-divergence is the
demonstrated baseline, so this register holds no rows until an *irreducible*
platform divergence is found and the owner authorizes a bounded allowance for it.

## Schema

Each row is one exception. The gate (`compare-gate.R`) parses the `## Exceptions`
pipe table and **fail-closes**: a row missing any field, not `state = ACCEPTED`,
past `expiry`, or whose `signature` does not equal the gate's computed divergence
fingerprint does **not** grant tolerance.

| Field | Meaning |
|---|---|
| `exception_id` | Stable identity, e.g. `DET-EX-001`. |
| `owner` | The owner (P0.1 authority). |
| `approver` | Who authorized it (the owner; P0.1 ratification). |
| `justification` | Why the divergence is acceptable and irreducible. |
| `scope` | Space/semicolon-separated comparable-cell labels the exception covers (matching `tools/determinism/expected-cells.csv`), or `*` for all comparable cells. |
| `signature` | The exact divergence fingerprint the gate emits (md5 over the canonicalized keyed row/column/value deltas vs the reference cell). Pins the precise diff so the exception cannot silently widen. |
| `expiry` | An ISO date `YYYY-MM-DD`; on/after it the row stops matching and the gate fails again. A non-date value is treated as expired (fail-closed). |
| `tracking_issue` | The `fp` issue owning remediation or permanent-acceptance. |
| `state` | Lifecycle state; only `ACCEPTED` grants tolerance. |

**Matching rule (P5.2 part 3).** A divergence is authorized **iff** an active
(`state = ACCEPTED`, unexpired, fully populated) exception's `scope` names the
diverging cell label (or `*`) **and** its `signature` equals the divergence
fingerprint. Any divergence with no matching active exception FAILS the gate.

## Exceptions

<!-- Append rows below the header. Empty = zero-divergence steady state. -->

| exception_id | owner | approver | justification | scope | signature | expiry | tracking_issue | state |
|---|---|---|---|---|---|---|---|---|
