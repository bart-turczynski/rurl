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
| `state` | `ACCEPTED` — an active deferral, G4 may exit with these cells unsatisfied. `DISCHARGED` — the carrier's work landed; the cells must now be covered, and claimed by a **registered** claimant — a verification slice, or the discharge record registered for this very row in `verification/traceability-map.md` § `Discharge records` (P0.7 D-E). A file that merely sits in the verification directory is not a claimant. Any other value is malformed and fails. |

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
| VD-001 | join | contracts/key-join-contracts.md | key surface (7 cells); key-policy rows (16 cells); scheme/port truth table (14 cells); six join operations (6 cells); cross-cutting relationship/multiplicity/resource/typed-condition/diagnostics rows (8 cells) | export:get_url_key;export:url_key_policy;export:url_inner_join;export:url_left_join;export:url_right_join;export:url_full_join;export:url_semi_join;export:url_anti_join | RURL-mihbyjsr | DISCHARGED, not expired by time: the deferral's stated premise was that the surface is "SETTLED by P3.1/P3.2 but unbuilt" and that "no test can assert a property of an absent function". Both halves expired — `RURL-mihbyjsr` shipped the key engine (`8f32ac2`), the six-join engine (`eba6f6b`) and the relaxed scheme mode (`c324252`) unexported, then all eight exports in ONE change, so every `surface_probe` resolves present in `NAMESPACE` and failure condition 3 applies. Cells are covered by `verification/key-join-discharge.md`. NOTE the residual that record states explicitly and does NOT claim as satisfied: `scheme_equality = "http_https_missing"` is refused rather than answered, so 3 of the truth table's 14 rows are verified over two of three mode columns plus an asserted refusal (`RURL-ixxvjjwj`); and the contract's `unmatched` typed condition is DROPPED by owner ruling (P3.3 §2, `RURL-kcgsuzll`) rather than implemented. | DISCHARGED |
| VD-002 | full-string | contracts/output-contracts.md | SURF-b; INV-2; FSSS-1; FSSS-3; FSSS-7; CAP-2; the fragment leg of FSSS-2 and REC-4 | export:serialize_url | RURL-zthbwebb | DISCHARGED, not expired by time: the deferral's stated premise was that output surface (b) "is unbuilt" and that consequently parse→serialize→parse "cannot be exercised at all". Both halves expired — the serializers shipped in `df00da8` (`.serialize_whatwg_full_vec` / `.serialize_rfc_full_vec`, R/parse-phases.R) and the public entry point `serialize_url()` shipped with P2.5, so the `surface_probe` resolves present in NAMESPACE and failure condition 3 applies. The zero-instance parse→serialize→parse gap the G4 audit recorded is closed at both the serializer and the public level. Cells are covered by `verification/output-fsss-discharge.md`. NOTE the boundary that record states explicitly: an admissible claim substrate now EXISTS, but rurl's conformance evidence does not yet ride it — every harness still scores against `clean_url`, which is `RURL-yeikpnan` and is not claimed here. | DISCHARGED |
| VD-003 | full-string | contracts/output-contracts.md | SURF-d; DISP-1; DISP-2; CAP-4; the redaction leg of CRED-3 and INV-5 | export:format_url | RURL-nyjnplyh | DISCHARGED, not expired by time: the deferral's stated premise was that "output surface (d), the safe human-facing formatter, is unbuilt". That premise expired — `RURL-nyjnplyh` shipped `format_url()` implementing P2.7 D-D's E1–E5 matrix and exported it in the same change, so the `surface_probe` resolves present in `NAMESPACE` and failure condition 3 applies. Cells are covered by `verification/output-display-discharge.md`. NOTE the boundary that record states explicitly: the row's second clause — that the escape/redaction matrix detail "is separately OPEN as OUT-O4" — is a DIFFERENT fact and has NOT expired. P2.7 D-D decides the matrix and the behavior is now tested, but the cell moves P2.7 §7 projects onto `contracts/output-contracts.md` (`:93`, `:169`, `:283-290`) were `RURL-irfmmoer`, which has since landed: `OUT-O4` is CLOSED in the contract and the disposition roster row reads SETTLED. | DISCHARGED |
| VD-004 | state | contracts/canonical-state-contract.md | layer1_syntax_verdict; layer2_policy_verdict; layer3_annotation_state | export:get_parse_verdicts;symbol:layer1_syntax_verdict;symbol:layer2_policy_verdict;symbol:layer3_annotation_state | RURL-glkuulyr | DISCHARGED: the stated premise — the fields "are SETTLED in artifact 3 but absent from the package" — expires with `RURL-glkuulyr`, which ships `get_parse_verdicts()` and the three layer symbols, so the `surface_probe` resolves present and failure condition 3 applies. `parse_status` is now literally π(layers) rather than a parallel cascade, so the layer values and the status column cannot drift apart by construction. Cells are covered by `verification/state-verdicts-discharge.md`. ERRATUM (`RURL-fcewylwv`): this cell list also named "migration cell M-11", which was carried as NOT covered because it could not be located. It was never defined — it was already dangling in `76bc38d`, the commit that introduced this row, and no `M-n` cell has a definition in any artifact (the `M-n` labels in tool and test comments are a working-audit vocabulary that never entered the repository). The citation is therefore removed rather than repointed, and the discharge is now full. | DISCHARGED |
| VD-005 | state | contracts/canonical-state-contract.md | authority state (authority_delimiter_present + authority_payload_kind + the "serializers emit // iff delimiter present" rule) | symbol:authority_delimiter_present;symbol:authority_payload_kind | RURL-bewtdlua | DISCHARGED, not expired by time: the deferral's stated premise was that "neither field exists in `R/`", and `RURL-bewtdlua` shipped both to `main` as `645fe56` (PR #280), so the `surface_probe` now resolves present and failure condition 3 applies. The originally recorded grounds — that the serializer inferred `//` from `host_kind` against P1.2 D-C, pinned by `test-parse-serializers.R:62-71` — were resolved by that same slice, which rebound `//` emission to the delimiter fact. Cells are covered by `verification/state-authority-discharge.md`. | DISCHARGED |
| VD-006 | mutation | contracts/validation-intervention-contract.md | the three-posture repair axis (s1, 5 cells); repair/recovery provenance (s7, 5 cells); repaired-input revalidation (s8, 3 cells) | symbol:repair_posture | VAL-O2 / RCON-05 | ACCEPTED: the input-repair surface is specified but unbuilt — `repair_posture` has no occurrence anywhere under `R/`, and `fixup_posture` is a 2-valued profile-resolved internal explicitly not a public formal (`R/parse.R:1058`), so no test can assert a property of an absent posture axis. Family is `mutation` per `RUL-019`: the eight families classify the PROPERTY (a byte-changing transformation carrying provenance), not the subject area, and `mutation` already hosts a SETTLED repair-provenance section (CM s6, `verification/traceability-map.md:271`) as well as an entirely unbuilt one (CM s8, `mutate_url`). s8 sits here rather than under `state` because its cells are properties the repair posture PRODUCES — both verdicts exposed *when repair ran*, an inferred candidate reported as an inferred-candidate outcome — and it shares s1's `repair_posture` probe, so filing it under `state` would give the row a surface_probe that family's slice could never discharge and split one surface's deferral across two families. Per P0.5 this row does NOT satisfy the cells; it makes their unsatisfied state explicit and attributable. | ACCEPTED |
| VD-007 | mutation | contracts/validation-intervention-contract.md | the typed intervention ledger and its ordered-pipeline rows (s2, 7 cells) | symbol:intervention_ledger | VAL-O1 / P2.6 | ACCEPTED: the typed intervention ledger is specified but unbuilt — no ledger identifier occurs under `R/`, and the categorization it depends on is itself carried by P2.6. The ledger records, per intervention, a stable rule ID and the before/after byte spans of a rewrite, which is a provenance-bearing byte-mutation property and therefore `mutation` under `RUL-019` on the same reading as VD-006. Held as a separate row from VD-006 because P0.7 D-C routes it to a different carrier; the two are not discharged together. Per P0.5 this row does NOT satisfy the cells. | ACCEPTED |
| VD-008 | state | contracts/validation-intervention-contract.md | the resolver verdict surface (s9, 5 cells: base-URL verdict, reference verdict, merged-output verdict, the current NA-on-failure projection, and the surface row itself) | export:get_resolve_verdicts | VAL-O3 / RCON-03 | ACCEPTED: no resolver-verdict companion is exported. Family is `state` per `RUL-019`: these five cells assert L1/L2 verdict VALUES over three objects, the same verdict-layer vocabulary `state-slice` already owns for VI s3-s6. NOTE the boundary this row does not paper over, and which is why it is ACCEPTED rather than closed by a test: P2.7 D-E settled `resolve_url()` as exposing no resolver companion BY DECISION rather than by silence (`contracts/validation-intervention-contract.md:248`), and two of the three verdicts are already reachable by calling `get_parse_verdicts()` on the base and on the `output = "serialized"` result. Only the reference verdict is genuinely unmodeled — a relative reference has no standalone parse frame. So this deferral is narrower than five cells' worth of absence, and reopening it needs a record defining the relative-reference verdict model first. Per P0.5 this row does NOT satisfy the cells. | ACCEPTED |
