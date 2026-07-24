# Verification contracts — determinism slice (§6 artifact 11 — determinism slice)

<!-- Verification artifact (§6 artifact 11, determinism slice). RCON-10 / C-09
     executable-evidence dimension. This record specifies the FAILING
     cross-platform determinism acceptance gate that implements the ACCEPTED
     decision P5.2 (determinism tolerance + approved-exception policy), and maps
     its parts onto executable evidence: the comparison engine, the exception
     register, and positive/negative fixtures.

     Second slice of §6 artifact 11 (after the cache slice), authored with its
     own envelope per the artifact-10/11 slice convention. It owns ONLY the
     determinism gate. The cache-transparency evidence (G4.1), the owner-approved
     release rule (G4.3, C-10/P0.4), the oracle taxonomy/claim policy (P5.3), and
     the remaining artifact-11 areas (migration, curl-removal closure, benchmark
     budgets, non-cache traceability) are named as boundaries owned elsewhere.

     lifecycle_state PROPOSED until a future G4 control-plane snapshot; a
     verification-family validator section, a record-schemas.yaml entry for the
     new determinism-exceptions register variant, and the manifest present-flip
     ride that seal. Until then no validator globs verification/ and the gate
     enforces the register schema at runtime. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-determinism-slice |
| name | verification-determinism-slice |
| artifact_number | 11 (determinism slice; cache slice is G4.1, release-rule is G4.3, remaining areas are later G4 leaves) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/determinism-slice.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this record is the SINGLE WRITER of the determinism-slice executable-evidence map; it verifies but never redefines the determinism tolerance policy |
| lifecycle_state | PROPOSED |
| verifies | P5.2 (determinism tolerance + exception policy); C-09; RCON-10 |
| dependencies | P5.2 (the normative policy); S8 (determinism gap evidence); the shipped determinism-probe.yml + tools/determinism/{parse-dump.R,corpus.csv} (research substrate the gate reuses); reconciliation §6 artifact 11, §4 RCON-09/10, §7 G4 |
| closes_finding | RCON-10 (determinism dimension); C-09 (executable side) |
| completion_rule | §7 G4 — the failing comparison exists over the comparable dump projections; the approved-exception register + matching engine exist; positive AND negative coverage is executable; the gate distinguishes divergence from missing/invalid/degraded evidence; exact local + CI commands are recorded; boundaries name the non-determinism verification areas |
| content_hash | pinned at a future G4 control-plane snapshot |
| approval_evidence | pending — seals at a future G4 control-plane snapshot (NOT an envelope flip) |
| validation_command | Rscript tools/determinism/compare-gate.R --self-test |
| validator_note | a verification-family validator section stages with the sealing G4 snapshot; until then no validator globs design/work/url-v3/verification/ and the gate enforces the register schema at runtime |

## Purpose

The executable-evidence contract for parse determinism: it demonstrates that
rurl's canonical parse output is byte-identical across the comparable
OS × R-build × charset/locale matrix, and it **fails the build** on any
unapproved divergence — the "separate failing acceptance comparison and
approved-exception mechanism" C-09 requires (reconciliation §5), distinct from
and additive to the shipped non-gating `determinism-probe.yml`.

This record **verifies**; it does not decide. Every rule traces to P5.2; nothing
here alters the tolerance (zero unapproved divergence), the comparable-cell set,
or the exception schema — it makes them executable.

## The gate (answers S8's six required questions)

`contract`/policy source: **P5.2** (`decisions/P5.2-*.md`, ACCEPTED,
`accepted_evidence 71b43bb`). Engine: **`tools/determinism/compare-gate.R`**
(base R only). Register: **`registers/determinism-exceptions.md`**. Expected-cell
manifest: **`tools/determinism/expected-cells.csv`**.

1. **What bytes/fields are compared.** The **canonical dump projection** of each
   cell: `parse-dump.R`'s `dump-<LABEL>.csv` rows sorted by the
   `(id, url_standard)` key, every non-key column in fixed order, reduced to one
   content hash per cell. Deterministic output makes the whole projection
   byte-identical across cells (the frozen 2026-07-20 sweep proved one MD5,
   `3db0d6e9…`). The gate separates the per-cell **detection hash** (does this
   cell match the group?) from an exception **signature** (an md5 over the
   canonicalized keyed row/column/value deltas vs the reference cell), so an
   exception authorizes an exact diff and cannot silently widen.
2. **Which cells are comparable.** The **14** `cross_os_comparable = true` cells
   (the 8-cell build axis with charset held at UTF-8, minus the declared
   macOS/devel exclusion, plus the 3 `c` and 3 `tr` charset/locale cells). The
   **3** runner-`default` cells are `cross_os_comparable = false`: retained as
   evidence, never entered into the equality comparison. A comparable cell whose
   `locale-<LABEL>.csv` reports `charset_as_requested = false` (or, for a `tr`
   cell, `hazard_armed = false`) is **invalid evidence** and FAILS — an unarmed
   axis cannot prove determinism even if its dump happens to match. The metadata
   flags are stored in the probe's JSON string-literal form (a boolean is
   `"true"`/`"false"` with inner quotes; NA is the bare token `null`); the gate
   decodes that exact representation **fail-closed** — a missing, duplicated,
   bare, or undecodable flag reads as NA and can never validate a comparable cell
   (a round-trip fixture against the probe's own `esc()` output guards this).
3. **What failure means.** The gate FAILS on any divergence among comparable
   valid cells not covered by an active exception; and — distinctly — on missing
   evidence, an unexpected cell, an invalid (unarmed/metadata-conflicting/
   malformed) comparable cell, or a `DEGRADED` comparable cell not covered by an
   active exception (P5.2 part 1: a registered gap allowance, matched by the typed
   sentinel signature `DEGRADED:<label>`, which pins the exact absent cell and
   cannot widen to an output diff). The verdict names the class
   (`FAIL_DIVERGENCE` / `FAIL_MISSING_EVIDENCE` / `FAIL_INVALID_AXIS` /
   `FAIL_DEGRADED`) so a red gate is a real finding but not necessarily parser
   nondeterminism.
4. **Where the result manifest is retained.** `compare-gate.R` writes a per-cell
   `gate-manifest.csv` (label, comparable, status, hash, detail) before exiting,
   uploaded as a CI artifact alongside the dumps; the probe's 90-day artifacts
   remain the raw-evidence retention.
5. **Which changes trigger it.** Per P5.2: `R/**`, `DESCRIPTION`, `NAMESPACE`,
   `tools/determinism/**`, and pinned dependency (`pslr`/`punycoder`) bumps, plus
   a schedule and a manual `workflow_dispatch` — not only harness changes (the
   C-09 defect).
6. **Which graduation step requires it.** The curl-free 3.0 slice cannot graduate
   until the gate passes over the comparable matrix (S8 acceptance gate 8;
   reconciliation §7 G4). `workflow_dispatch` at a chosen ref is the executable
   pre-graduation check.

## Tolerance and the exception mechanism

Tolerance is **byte-exact zero unapproved divergence** (P5.2 part 2) — no fuzzy
margin. The **only** channel for tolerance is the append-only
`registers/determinism-exceptions.md` (P5.2 part 3): a divergence passes iff an
active (`state = ACCEPTED`, unexpired, fully populated) exception whose `scope`
names the diverging cell has a `signature` equal to the divergence fingerprint.
The gate is **fail-closed**: a malformed, incomplete, non-ACCEPTED, or expired
row grants nothing. Steady state is an empty register (zero-divergence baseline).

## Positive and negative coverage (§7 G4)

`Rscript tools/determinism/compare-gate.R --self-test` builds synthetic dump
directories and asserts fifteen fixtures:

| # | fixture | expected verdict | sign |
|---|---|---|---|
| 1 | all comparable cells identical | PASS | positive |
| 2 | one comparable cell diverges, no exception | FAIL_DIVERGENCE | negative |
| 3 | that divergence covered by a matching unexpired ACCEPTED exception | PASS | positive |
| 4 | same exception but expired | FAIL_DIVERGENCE | negative |
| 5 | exception with a non-matching signature | FAIL_DIVERGENCE | negative |
| 6 | a comparable cell reports DEGRADED, no exception | FAIL_DEGRADED | negative |
| 7 | an expected comparable dump is missing | FAIL_MISSING_EVIDENCE | negative |
| 8 | a `tr` cell whose hazard is not armed | FAIL_INVALID_AXIS | negative |
| 9 | metadata decode round-trips the probe's `esc()` form, fail-closed on bare/absent | (decoder unit) | both |
| 10 | exception with a blank required governance field (justification) | FAIL_DIVERGENCE | negative |
| 11 | inclusive expiry: active the day before, inactive ON the expiry date | PASS / FAIL_DIVERGENCE | both |
| 12 | manifest with a non-boolean `comparable` value | rejected (error) | negative |
| 13 | manifest with a field-width mismatch (unquoted comma / short row) | rejected (error) | negative |
| 14 | DEGRADED cell covered by a matching `DEGRADED:<label>` sentinel exception | PASS | positive |
| 15 | DEGRADED cell with a wrong sentinel signature | FAIL_DEGRADED | negative |

## Exact commands

```sh
# The gate engine's own positive/negative coverage (deterministic, no network):
Rscript tools/determinism/compare-gate.R --self-test

# Run the gate over a directory of collected cell dumps (CI, or a local repro):
Rscript tools/determinism/compare-gate.R \
  --dumps tools/determinism/out \
  --expected tools/determinism/expected-cells.csv \
  --exceptions design/work/url-v3/registers/determinism-exceptions.md \
  --manifest-out tools/determinism/out/gate-manifest.csv
```

The multi-OS cell production runs in `.github/workflows/determinism-gate.yml`
(the gate workflow), which produces the dumps + metadata via the shared
`parse-dump.R` and then runs `compare-gate.R` as a failing step.

## Scope boundaries

This record owns the **determinism-slice executable evidence** and nothing else.
It does NOT define, and must not be read as redefining:

- **The determinism tolerance policy, comparable-cell set, and exception
  schema** — owned by **P5.2**. This record verifies them; it changes none.
- **Cache-transparency evidence** — owned by **G4.1** (the cache slice). The
  comparable axis extends to cache state as that graduates (P5.1 defines the
  states; this gate only includes them).
- **Oracle taxonomy and claim policy** — owned by **P5.3**. The gate's oracle
  discipline (named/bounded/expiring exceptions, never a fuzzy margin)
  deliberately mirrors P5.3's deviation ledger.
- **Owner-approved release rule (C-10) and the pre-graduation gate's
  authority** — owned by **G4.3** / P0.4. This record wires the executable check;
  the release decision is elsewhere.
- **The remaining §6 artifact-11 areas** — migration, curl-removal closure,
  benchmark budgets, and the non-cache/non-determinism traceability map — owned
  by later G4 leaves.
- **The shipped `determinism-probe.yml` posture** — unchanged. The gate is
  additive; the probe stays "evidence collection, not a gate."

## Open cells

**Cross-run repeat-run comparison (P5.2 "two pinned runs of the same comparable
cell") — one open leaf.** The gate today compares comparable cells to each other
*within one run* (cross-platform). P5.2 additionally names comparing two runs of
the same cell. The determinism-faithful reading of that clause is **repeat-run
reproducibility** — the same cell run twice from fresh processes must be
byte-identical — not a same-cell-at-two-commits output baseline: a cross-commit
change is a *behavior regression under a declared input (code)*, which is already
locked by `tests/testthat/_snaps/characterization-snapshot.md` (a `json2`
value-snapshot of `safe_parse_urls()` over corpus × combos, with a
readable-diff accept-after-review ritual). Duplicating that as a committed md5
baseline would be lower-resolution and, under the v3 parser rewrite, red on most
slices. The remaining executable-evidence leaf is therefore an **in-gate
repeat-run**: each comparable cell emits its dump twice and the gate compares the
pair as a distinct verdict class (`FAIL_NONDETERMINISM`), with an optional
follow-on that varies run 2 in a way that *must not* matter (corpus order
shuffled under a fixed seed; the projection is keyed and sorted on
`(id, url_standard)`), catching order/state-leak sensitivity the cross-cell
comparison structurally cannot isolate. Tracked as a follow-on G4.2 leaf.

- **Matrix-uniform temporal drift.** Output that depends on ambient state
  identical across cells on a given day (wall clock; a refreshed `pslr` snapshot)
  is missed by both cross-cell and repeat-run comparison. It is caught by the
  characterization snapshot on the next PR; the sharp *in-gate* fix is asserting
  the **engine / PSL-snapshot identity** in the cell metadata — pinning an
  **input** P5.2 already names as a required axis (RCON-09) — rather than an
  output baseline. Deferred with the cache/engine axis note below.
- **Cache-state and PSL-engine axes (RCON-09).** P5.2 requires the comparable
  axis set to extend to cache state (cold/warm/disabled) and PSL engine snapshot
  as those graduate. The cache-state transparency is already gated per process by
  the G4.1 cache slice; folding a cache-state or engine-snapshot dimension into
  THIS cross-platform matrix rides the same future graduation step that P5.1 /
  P0.4 define those states at — an inclusion task, not an open decision here.
- **New register variant.** `determinism-exceptions` is a register variant not
  yet in `schema/record-schemas.yaml`; the gate enforces its schema at runtime
  (fail-closed) and a future G4 control-plane snapshot adds the validate-records
  section + schema entry + manifest pin, exactly as prior register/contract
  families were sealed.
