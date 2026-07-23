# Gate acceptance: G2 — Authority & contradiction closure (§7 G2)

<!-- Gate-acceptance record (process/evidence). RURL-uksahklp. The acceptance
     fan-in for §7 G2: every §5 contradiction has a typed owner disposition, the
     claim roles stay assertion-distinct, and dependency edges propagate. Records
     the exact input hashes and validation results at the acceptance commit;
     validated by tools/validate-records.R (gates section), which recomputes each
     ## Inputs hash so any later drift in a gate input FAILS this record and
     thereby reopens the acceptance (fp: "any later invalidation fails/reopens
     this acceptance and blocks G3 onward"). Registered into the manifest and
     hash-sealed at cp-snapshot-2 (owner seal), like the registers before it. -->

## Envelope

| Field | Value |
|---|---|
| id | gate-G2-acceptance |
| gate | G2 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/gates/G2-acceptance.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| state | ACCEPTED |
| accepted_commit | 6369d75 (PR #220 squash-merge into main — G2 disposition finalization) |
| accepted_at | 2026-07-23 |
| depends_on | contradictions.md (C-01..C-12 disposed); P0.4, P1.1, P2.1, P2.2, P2.3, P2.4, P3.1, P4.1, P5.1, P5.2, P5.3 (ACCEPTED); reconciliation §4,§5,§7 |
| manifest_registration | pending — pins present:true at v3/cp-snapshot-2 (NOT the sealed cp-snapshot-1 manifest) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |

## §7 G2 exit criteria

Each criterion is met at the acceptance commit; evidence is a pointer, not a
re-argument.

| # | §7 G2 criterion | Result | Evidence |
|---|---|---|---|
| 1 | Every §5 contradiction and every newly discovered contradiction has an owner disposition. | PASS | `registers/contradictions.md` — all 12 rows C-01..C-12 at `disposition_state = ACCEPTED` with a typed `disposition_type` and an `owner_decision_ref`; no contradiction discovered beyond C-01..C-12 (12-row bijection asserted by validate-records). |
| 2 | Accepted, shipped, historical, proposed, and superseded claims remain distinguishable at assertion level. | PASS | The typed set {ACCEPTED, SUPERSEDED, COMPATIBILITY-ONLY} is recorded per row with `affected_claims`/`invalidated_artifacts` naming which claim each disposition governs; the source-claim register (`source-claims.md`) preserves per-assertion classification; no claim role was collapsed. |
| 3 | Source changes invalidate dependent decisions and reviews through recorded edges. | PASS | The lifecycle transition table (schema/lifecycle.yaml; validate-transitions.R, 180 checks) encodes source-delta → INVALIDATED propagation; the manifest freezes S1–S9 + tracker at cp-snapshot-1; this record's `## Inputs` hashes are recomputed by validate-records so a drifted G2 input fails the gate. |

No blocking `OPEN`/`CONFLICT`/`INVALIDATED` record remains: `grep -L "^state: ACCEPTED" decisions/*.md` is empty, and every contradiction row is `ACCEPTED`.

## Disposition summary (projected from the accepted P-tier)

| C | disposition_type | owner_decision_ref |
|---|---|---|
| C-01 | ACCEPTED | process/G2 owner merge (no P-tier) |
| C-02 | SUPERSEDED | P2.1@a4d1b45 |
| C-03 | ACCEPTED | P2.1@a4d1b45 |
| C-04 | ACCEPTED | P2.2@8292c7f |
| C-05 | COMPATIBILITY-ONLY | P2.2@8292c7f |
| C-06 | COMPATIBILITY-ONLY | P3.1@3b89b94 |
| C-07 | ACCEPTED | P1.1+P2.3@a7e0a59 |
| C-08 | ACCEPTED | P5.1@d254ff1 |
| C-09 | ACCEPTED | P5.2@71b43bb |
| C-10 | ACCEPTED | P0.4@7bc5358 |
| C-11 | SUPERSEDED | P2.4+P4.1@b017e87 |
| C-12 | COMPATIBILITY-ONLY | P2.2+P5.3@8292c7f |

Owner-noted soft spots at acceptance (recorded, not blocking): **C-01** is a
process ACCEPTED-vs-SUPERSEDED judgment (no product P-tier binds it); **C-12**
transcribes P5.3's own recommended typed value (P5.3 sealed ACCEPTED but its
Open-Q1 left the value as a recommendation).

## Inputs

The gate's central inputs, hashed at the acceptance commit. `validate-records.R`
recomputes each `sha256` on every run; a mismatch fails this record and reopens
the G2 acceptance. The 11 bound P-tier decisions are hash-enforced separately by
`validate-manifest.R` (`decisions[]`) and are not duplicated here.

| path | sha256 |
|---|---|
| design/work/url-v3/registers/contradictions.md | 6260a4b36777c482dd78d462fa03bcc0663d0e4f8a0d6029e6c1d36f660f7fe6 |
| design/work/url-v3/protocol-review-reconciliation.md | faa9c3b76f662f9548de6e4c83e6f577379557ba04b612c7a7fbd950be5ebb42 |

## Validation results at the acceptance commit

| validator | result |
|---|---|
| `validate-records.R` | VALIDATION PASSED (records + registers, incl. the 12 disposed contradiction rows) |
| `validate-manifest.R` | VALIDATION PASSED (116 checks; 11 P-tier decisions ACCEPTED + hash-verified) |
| `validate-transitions.R` | VALIDATION PASSED (180 checks) |
| `ci-gate.R` (control-plane) | CONTROL-PLANE GATE: PASS (strict) |

## Reopening rule

This acceptance is not terminal evidence: it holds only while its inputs hold. If
any `## Inputs` hash drifts, any bound P-tier decision leaves `state: ACCEPTED`,
or a new contradiction is discovered, `validate-records.R`/`validate-manifest.R`
fail — which reopens G2 and blocks G3 onward (§6 lifecycle: a source or
owner-decision change marks dependents `INVALIDATED` and opens replacements at
`DISCOVERED`; it never silently rewinds history). Re-acceptance requires a new
gate-acceptance record superseding this one.
