# Verification — verdict-layer discharge (VD-004)

<!-- Verification artifact, discharge record. NARROW BY CONSTRUCTION, on the
     same terms as state-authority-discharge.md: it discharges exactly one
     deferral, VD-004, and is NOT the state-family verification slice. That
     slice remains a later G4 leaf.

     Why it exists: P0.5 failure condition 3 fires the moment a deferred
     surface ships. VD-004's surface ships in this PR, so the row cannot stay
     ACCEPTED; and a DISCHARGED row must be claimed by a verification slice
     (deferral-gate rule D2). This record is that claim.

     The former "Not discharged: migration cell M-11" carve-out is resolved
     (RURL-fcewylwv); see "Resolved: the M-11 citation" below. The row's cell
     list is now fully covered. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-state-verdicts-discharge |
| name | verification-state-verdicts-discharge |
| artifact_number | 11 (discharge record; the state-family slice proper is a later G4 leaf) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/state-verdicts-discharge.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| verifies | VD-004 (registers/verification-deferrals.md); P2.3 (authoritative decision); contracts/canonical-state-contract.md — verdict-layer cells only |
| dependencies | P0.5 (the deferral register and its gate); P2.3 (bound decision); P1.1 (state/status model) |
| closes_finding | VD-004 (fully, since RURL-fcewylwv removed the unlocatable `M-11` citation from the row) |
| completion_rule | every cell named by VD-004 maps to shipped executable evidence, cited as `path :: test name`; any cell that cannot be located is named as NOT covered and carried to an issue rather than silently claimed; `tools/deferral-gate.R` reports D2 and D3 PASS |
| approval_evidence | pending — rides the owner's merge of the PR carrying it |
| validation_command | Rscript tools/deferral-gate.R && devtools::test() |

## DISCHARGED[VD-004]

VD-004 deferred the verdict-layer cells because, in the register's words, "the
three verdict-layer fields and their companion projection are SETTLED in
artifact 3 but absent from the package; the only shipped status output is
`parse_status`." That premise expires with this PR: `RURL-glkuulyr` ships
`get_parse_verdicts()` and the three layer symbols, so the row's
`surface_probe` resolves present and P0.5 failure condition 3 applies.

The cells are not merely shipped — they are covered. `parse_status` is now
literally `π(layers)` rather than a parallel cascade, so the layer values and
the status column cannot drift apart by construction.

## Cell → evidence map

All evidence is in `tests/testthat/test-parse-verdicts.R`, which ships in this
PR and runs in the standard `devtools::test()` chain.

| VD-004 cell | Evidence | Coverage |
|---|---|---|
| `layer1_syntax_verdict` | `:: "the two error kinds are distinguishable, and parse_status is not"` | separates an L1 syntax failure from an L2 admission rejection — the distinction `parse_status` collapses |
| `layer2_policy_verdict` | `:: "L2 accept sub-states do not depend on the L3 outcome"` | the accept sub-states are asserted independent of the PSL outcome |
| `layer3_annotation_state` | `:: "L3 is a typed state on every row, and never fatal"` | L3 is total and non-fatal on every row |
| layer-independence invariant | `:: "L3 does not move L1 or L2 (the independence invariant)"` | the SETTLED invariant, asserted directly |
| π totality and fidelity | `:: "pi projects every layer combination to a shipped status value"`; `:: "pi reproduces parse_status across the option matrix"` | π is total over the layer product, and reproduces the shipped column across the option matrix |
| vocabulary containment | `:: "every produced verdict is inside its settled vocabulary"`; `:: "three L3 states have no producer in the shipped engine"` | no verdict escapes its settled enum; the three unproduced L3 states are asserted rather than left silent |
| companion-surface constraints | `:: "the companion never widens the parse frame"`; `:: "get_parse_verdicts is shaped and validated like its siblings"`; `:: "the verdicts do not depend on cache warmth"` | ADR 0006 (no frame widening), sibling shape/validation parity, and cache-warmth invariance (P1.1 §2) |

## Resolved: the `M-11` citation

This section previously carved out "migration cell M-11" as a cell this record
could not claim, because it could not be located. `RURL-fcewylwv` settled it:
**`M-11` was never defined, and the citation has been removed from VD-004.**

The evidence is a history check, not another search. `M-11` was born dangling:
the commit that introduced the VD-004 row (`76bc38d`, PR #246) defines no `M-n`
cell anywhere in its diff, and no later commit added one. There is nothing to
find, so the citation was dropped rather than repointed.

**The earlier framing here was itself an over-claim and is withdrawn.** This
section used to assert that "the `M-n` namespace is otherwise real — `M-8` and
`M-14` are the frozen-vocabulary rules implemented by
`tools/status-doc-consistency.R`". That is not what makes a cell real. No `M-n`
cell has a *definition* in any artifact: `M-3`, `M-5`, `M-8` and `M-14` occur
only as informal labels in tool and test comments
(`tools/status-doc-consistency.R`, `tools/diagnostics-doc-consistency.R`,
`tests/testthat/test-g4-coverage-negatives.R`, `.github/workflows/verify.yml`),
and `evidence/S8`'s v2-to-v3 migration matrix is eight unnumbered rows with no
`M-n` labels at all. A tool that *closes* a rule it names does not *define* it.
So `M-11` was not a dangling citation into a real family — the family itself
was only ever a working-audit vocabulary that never entered the repository.

This is recorded rather than papered over. Deferral-gate rule D2 only checks
that a DISCHARGED row is claimed by *some* slice; it does not verify coverage
cell by cell, so nothing mechanical would have caught the original silent
over-claim, and nothing would have caught this record's own weaker one either.
Both were found by reading, which is the standing lesson.

## Boundary: what this record does NOT claim

- It does **not** verify the rest of `contracts/canonical-state-contract.md`.
  Only VD-004's locatable cells are discharged.
- It makes **no product decision**. P2.3 and P1.1 remain the single writers of
  the verdict-layer semantics; this record only maps them to evidence.
- It does not re-open the `parse_status` deprecation plan (P2.3 §5), which is
  unaffected by this discharge.

## Inputs

| Path | Role |
|---|---|
| design/work/url-v3/registers/verification-deferrals.md | the register carrying VD-004 |
| design/work/url-v3/decisions/P0.5-g4-exit-criterion-scope.md | the disposition defining discharge |
| design/work/url-v3/decisions/P2.3-validation-verdicts-migration.md | the bound decision (π collapse table, §4) |
| tests/testthat/test-parse-verdicts.R | the cited executable evidence |
