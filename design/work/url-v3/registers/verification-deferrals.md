# Register: verification-deferrals (§6 artifact 11 — G4 exit scope)

<!-- variant: verification-deferral (NEW register variant; not yet in
     schema/record-schemas.yaml). Row-based, append-only. Its schema is defined
     below and enforced at runtime by tools/deferral-gate.R (the gate reads this
     register and fail-closes on any malformed row). A future control-plane
     snapshot adds a validate-records.R section and a record-schemas.yaml entry
     for this variant and pins it present:true in the manifest, exactly as the
     §6 contract registers were sealed at cp-snapshot-2 / the contracts at
     cp-snapshot-3. Until then no validator globs it.

     This register is the ONLY channel by which gate G4 may exit with an
     unsatisfied criterion-3 cell (P0.5). A cell is covered, or it has an
     ACCEPTED row here naming a carrier, or the gate fails. There is no third
     way and no ambient tolerance. -->

## Envelope

| Field | Value |
|---|---|
| id | reg-verification-deferrals |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/registers/verification-deferrals.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — a deferral is an owner authorization act (P0.1 authority) |
| lifecycle_state | PROPOSED |
| dependencies | P0.5 (G4 exit-criterion scope); tools/deferral-gate.R (the consuming gate); the six verification/*-slice.md records that cite rows |
| completion_rule | every row carries the full schema below; the gate fail-closes on any malformed/incomplete row, any missing carrier, any DISCHARGED row whose cells are not covered, and any row whose surface has shipped; validate-records.R (verification-deferrals section, added at a future G4 snapshot) passes |
| content_hash | pinned at a future G4 control-plane snapshot; the gate hashes nothing of itself |
| approval_evidence | pending — pins at a future G4 snapshot (NOT an envelope flip) |
| validation_command | Rscript tools/deferral-gate.R --self-test |

## Purpose

Records each criterion-3 cell that gate G4 **does not prove**, because the
surface the cell describes is specified but not shipped. Per P0.5 a row here
does **not** satisfy the cell — it makes the cell's unsatisfied state explicit,
attributable, and machine-checkable, so that G4 may exit as *partial
verification with named deferrals* rather than as false full coverage.

**Deferral is not satisfaction.** Rows in this register are counted separately
from covered cells and are never reported as met. `gates/G4-acceptance.md` must
tally the two in a form that cannot be read as one number.

**A deferral is only ever justified by an unshipped surface.** This register is
not a general escape hatch. A cell whose surface ships and whose property is
merely untested is a **gap**, not a deferral, and must be closed by a test.

## Schema

Each row is one deferral. The gate (`tools/deferral-gate.R`) parses the
`## Deferrals` pipe table and **fail-closes**: a row missing any field, holding
an unknown `state`, naming no carrier, or failing any of the three P0.5 failure
conditions does **not** authorize G4's exit.

| Field | Meaning |
|---|---|
| `deferral_id` | Stable identity, e.g. `VD-001`. Slices cite it as `DEFERRED[VD-001]`. |
| `family` | The criterion-3 property family the cells belong to: one of `full-string`, `state`, `vector`, `mutation`, `join`, `cache`, `determinism`, `migration`. |
| `artifact` | The owning G3 contract artifact that declares the cells SETTLED. |
| `cells` | Semicolon-separated cell ids this row covers, or a `<block> (n cells)` summary where the owning artifact numbers them as a block. The consuming slice enumerates them exactly. |
| `surface_probe` | The executable predicate proving the surface is unshipped, as a semicolon-separated list of `export:<name>` (present iff exported in `NAMESPACE`) or `symbol:<name>` (present iff the identifier occurs in `R/`). **If ANY listed symbol is present, the surface has shipped** and failure condition 3 fires. This is what makes "specified but not shipped" a fact about the repository rather than a claim. |
| `carrier` | The `fp` issue owning construction of the surface. A **pointer** for navigation; per P0.1 §5 its tracker state is NOT authoritative and the gate never reads it. Authority is this row's `state`. |
| `justification` | Why no test can close the cells today. |
| `state` | `ACCEPTED` — an active deferral, G4 may exit with these cells unsatisfied. `DISCHARGED` — the carrier's work landed; the cells must now be covered by a verification slice, and the gate fails if they are not. Any other value is malformed and fails. |

**No date expiry.** Unlike the determinism-exceptions register, deferrals carry
no `expiry` date, because the condition that ends a deferral is not the passage
of time but the arrival of the surface. Failure condition 3 *is* the expiry, and
it is exact: the row dies the moment its surface ships. A date would either
expire early (a false red on work that is legitimately still unbuilt) or late
(silence during the window that matters).

**Matching rule (P0.5).** G4's exit is authorized for a cell **iff** an
`ACCEPTED` row's `cells` names it, that row is fully populated, its `carrier` is
non-empty, and none of its `surface_probe` symbols is present in the tree. Any
criterion-3 cell that is neither covered by a slice nor named by such a row
FAILS the gate.

## Deferrals

<!-- Append rows below the header. Each row is authorized by the owner's merge
     of the PR that adds it (P0.1 §4). -->

| deferral_id | family | artifact | cells | surface_probe | carrier | justification | state |
|---|---|---|---|---|---|---|---|
| VD-001 | join | contracts/key-join-contracts.md | key surface (7 cells); key-policy rows (16 cells); scheme/port truth table (14 cells); six join operations (6 cells); cross-cutting relationship/multiplicity/resource/typed-condition/diagnostics rows (8 cells) | export:get_url_key;export:url_key_policy;export:url_inner_join;export:url_left_join;export:url_right_join;export:url_full_join;export:url_semi_join;export:url_anti_join | RURL-mihbyjsr | The comparison-key and six-join surface is SETTLED by P3.1/P3.2 but unbuilt; P3.2 §232-234 states it is "specified, not shipped here". `canonical_join` ships with `join = c("inner","left","right","full")` only. No test can assert a property of an absent function. | ACCEPTED |
| VD-002 | full-string | contracts/output-contracts.md | SURF-b; INV-2; FSSS-1; FSSS-3; FSSS-7; CAP-2; the fragment leg of FSSS-2 and REC-4 | export:serialize_url | RURL-zthbwebb | Output surface (b), the fragment- and credential-complete standards serializer, is unbuilt. The shipped `.serialize_whatwg_vec` / `.serialize_rfc_generic_vec` are the CLEAN serializers (surface c) and exclude fragments by contract. Consequently parse→serialize→parse, which P5.3 CLAIM-1 names as one of only two admissible v3 claim substrates, cannot be exercised at all. | ACCEPTED |
| VD-003 | full-string | contracts/output-contracts.md | SURF-d; DISP-1; DISP-2; CAP-4; the redaction leg of CRED-3 and INV-5 | export:format_url | RURL-nyjnplyh | Output surface (d), the safe human-facing formatter, is unbuilt. Its escape/redaction matrix detail is separately OPEN as OUT-O4. | ACCEPTED |
| VD-004 | state | contracts/canonical-state-contract.md | layer1_syntax_verdict; layer2_policy_verdict; layer3_annotation_state; migration cell M-11 | export:get_parse_verdicts;symbol:layer1_syntax_verdict;symbol:layer2_policy_verdict;symbol:layer3_annotation_state | RURL-glkuulyr | DISCHARGED: the stated premise — the fields "are SETTLED in artifact 3 but absent from the package" — expires with `RURL-glkuulyr`, which ships `get_parse_verdicts()` and the three layer symbols, so the `surface_probe` resolves present and failure condition 3 applies. `parse_status` is now literally π(layers) rather than a parallel cascade, so the layer values and the status column cannot drift apart by construction. Cells are covered by `verification/state-verdicts-discharge.md` — EXCEPT the cited "migration cell M-11", which is defined nowhere in the repository and is explicitly NOT claimed as covered; carried as `RURL-fcewylwv`. | DISCHARGED |
| VD-005 | state | contracts/canonical-state-contract.md | authority state (authority_delimiter_present + authority_payload_kind + the "serializers emit // iff delimiter present" rule) | symbol:authority_delimiter_present;symbol:authority_payload_kind | RURL-bewtdlua | DISCHARGED, not expired by time: the deferral's stated premise was that "neither field exists in `R/`", and `RURL-bewtdlua` shipped both to `main` as `645fe56` (PR #280), so the `surface_probe` now resolves present and failure condition 3 applies. The originally recorded grounds — that the serializer inferred `//` from `host_kind` against P1.2 D-C, pinned by `test-parse-serializers.R:62-71` — were resolved by that same slice, which rebound `//` emission to the delimiter fact. Cells are covered by `verification/state-authority-discharge.md`. | DISCHARGED |
