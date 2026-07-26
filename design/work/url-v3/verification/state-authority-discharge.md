# Verification — authority-state discharge (VD-005)

<!-- Verification artifact, discharge record. NARROW BY CONSTRUCTION: this file
     discharges exactly one deferral, VD-005, by mapping the authority-state
     cells it deferred onto executable evidence that now ships. It is NOT the
     state-family verification slice. That slice — the full artifact-11 map for
     contracts/canonical-state-contract.md — remains a later G4 leaf, exactly as
     cache-slice.md names the non-cache areas as boundaries it does not design.
     Authoring the whole state slice here to close one row would manufacture G4
     scope, which is the failure the cache slice's own envelope warns against.

     Why this record exists at all: P0.5 failure condition 3 fires the moment a
     deferred surface ships. VD-005's surface shipped in RURL-bewtdlua, so the
     row cannot stay ACCEPTED; and a DISCHARGED row must be claimed by a
     verification slice (deferral-gate rule D2) or the gate fails. This record
     is that claim, and nothing more. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-state-authority-discharge |
| name | verification-state-authority-discharge |
| artifact_number | 11 (discharge record; the state-family slice proper is a later G4 leaf) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/state-authority-discharge.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| verifies | VD-005 (registers/verification-deferrals.md); P1.2 (authoritative decision); contracts/canonical-state-contract.md — authority-state cells only |
| dependencies | P0.5 (the deferral register and its gate); P1.2 (bound decision); contracts/canonical-state-contract.md (the normative source) |
| closes_finding | VD-005 |
| completion_rule | every cell named by VD-005 maps to shipped executable evidence, cited as `path :: test name`, with the evidence present on `main` rather than promised; `tools/deferral-gate.R` reports D2 and D3 PASS |
| approval_evidence | pending — rides the owner's merge of the PR carrying it |
| validation_command | Rscript tools/deferral-gate.R && devtools::test() |

## DISCHARGED[VD-005]

VD-005 deferred the authority-state cells on an explicit, checkable premise —
in the register's own words, that **"neither field exists in `R/`"**. That
premise expired: `RURL-bewtdlua` shipped both fields, merged to `main` as
`645fe56` (PR #280). The deferral's `surface_probe`
(`symbol:authority_delimiter_present;symbol:authority_payload_kind`) now
resolves present in `R/parse-state.R`, `R/parse-phases.R` and `R/parse.R`, which
is precisely the condition P0.5 failure condition 3 names.

The register's rule is that the row dies when its surface arrives, not when a
date passes. This record is that death certificate: the cells are no longer
excused, they are **covered**.

## Cell → evidence map

All evidence is in `tests/testthat/test-p1-2-authority-state.R`, which shipped
with the same slice and runs in the standard `devtools::test()` chain.

| VD-005 cell | Evidence | Coverage |
|---|---|---|
| `authority_delimiter_present` (canonical field) | `test-p1-2-authority-state.R :: "P1.2 fixtures: canonical fields, host_kind, legacy projection"` | P1.2 D-B's seven required shapes, asserted through the real parsers rather than hand-built state |
| `authority_payload_kind` (canonical field) | same fixture set | includes the `empty` value, which was unreachable under the retired `authority_kind` enum |
| `host_kind` independence | `test-p1-2-authority-state.R :: "payload state and host state vary independently"` | the payload and host axes are asserted to vary separately, which is the property the single enum conflated |
| legacy `authority_kind` projection | `test-p1-2-authority-state.R :: "P1.2 fixtures: …"` | P1.2 D-D's derived read-only projection |
| "serializers emit `//` **iff** delimiter present" (P1.2 D-C) | `test-p1-2-authority-state.R :: "a delimiter-present empty authority survives serialization"` | both postures, through the public parse: `foo:/bar` and `foo:///bar` no longer collapse, and `file:///bar` keeps its delimiter |

## Boundary: what this record does NOT claim

- It does **not** verify the rest of `contracts/canonical-state-contract.md`.
  Only the cells VD-005 enumerated are discharged; every other cell of that
  contract keeps whatever status it already had.
- It does **not** discharge `VD-004`, the sibling row covering the verdict-layer
  fields and `get_parse_verdicts`. That surface is still unshipped on `main`, so
  VD-004 stays `ACCEPTED` and D3 correctly stays silent about it. When its
  carrier lands, VD-004 needs its own discharge on these same terms.
- It makes **no product decision** and re-decides no state semantics. P1.2
  remains the single writer of the authority-state vocabulary; this record only
  records where that vocabulary is executably verified.

## Inputs

| Path | Role |
|---|---|
| design/work/url-v3/registers/verification-deferrals.md | the register carrying VD-005 |
| design/work/url-v3/decisions/P0.5-g4-exit-criterion-scope.md | the disposition defining discharge |
| design/work/url-v3/decisions/P1.2-authority-state-vocabulary.md | the bound decision |
| tests/testthat/test-p1-2-authority-state.R | the cited executable evidence |
