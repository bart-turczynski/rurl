# Register: findings (§6 artifact 2)

<!-- variant: finding (schema/record-schemas.yaml). Row-based register. -->


## Envelope

| Field | Value |
|---|---|
| id | reg-findings |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/registers/findings.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| dependencies | source-claims.md; protocol-review-reconciliation.md §4,§9 |
| completion_rule | the 10 consolidated RCON findings each list §4 sources that invert to §9; validate-records.R passes |
| content_hash | derived from reconciliation §4/§9 + source-claims.md |
| approval_evidence | pending — pins at v3/cp-snapshot-2 (NOT the sealed cp-snapshot-1 manifest) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |

## Purpose

The ten consolidated cross-report findings (RCON-01..RCON-10) from
reconciliation §4. `sources` is the verbatim §4 Sources list (the *forward*
map); the validator expands it and asserts it equals the *inverse* built from
the source-claims register and the §9 coverage map (forward ⇔ inverse). Every
RCON is a blocker. `disposition = DISCOVERED`: each finding's identity and
sources are recorded but its resolution (owner decisions P1–P5, §6 contract
artifacts, gates G2–G5) is pending; `verification_ref` names where it is
discharged. A finding closes only when its disposition and verification
evidence are recorded (reconciliation §6).

## Rows

| finding_id | severity | sources | disposition | verification_ref |
|---|---|---|---|---|
| RCON-01 | blocker | S9 C1-C3, H1-H3, H7-H9, M1-M5 | DISCOVERED | §6 artifacts 1-2 (manifest + authority records); gate G0 (control plane tracked, restore drill passes); owner tier P0 |
| RCON-02 | blocker | S1 F1, F5-F7, F9, F11; S3 F2, F4-F5; S4 F6-F10; S7 F1-F3, F7, F10 | DISCOVERED | §6 artifact 3 (canonical-state-contract); gate G3; owner tier P1 |
| RCON-03 | blocker | S1 F2; S2 S2-03, S2-07; S3 F1-F7; S4 F1-F5, F12-F13; S5 findings 3-6; S6 findings 3-4 and 9 | DISCOVERED | §6 artifact 7 (output-contracts); gate G3; owner tier P2 |
| RCON-04 | blocker | S2 S2-01-S2-03, S2-05-S2-09; S4 F3, F7, F11, F14-F15; S6 findings 5-7; S7 F3, F6, F8-F9 | DISCOVERED | §6 artifact 6 (validation-intervention-contract); gate G3; owner tier P2 |
| RCON-05 | blocker | S1 F4, F8; S2 S2-04; S4 F4-F5, F11; S6 findings 1-4 and 8-10; S7 F1, F6-F8; S8 findings 4, 8-10 | DISCOVERED | §6 artifacts 4-5 (public-surface registry + standard/profile/scheme matrices); gates G1.2/G3; owner tiers P2/P4 |
| RCON-06 | blocker | S4 F1-F15; S1 F6-F7; S3 F4-F5; S7 F5, F8 | DISCOVERED | §6 artifact 8 (cleaning-mutation-contracts); gate G3; owner tier P3 |
| RCON-07 | blocker | S5 findings 1-9; S1 F6-F7; S3 F5-F6; S4 F13; S8 findings 4 and 8 | DISCOVERED | §6 artifact 9 (key-join-contracts); gate G3; owner tier P3 |
| RCON-08 | blocker | S7 F1-F10; S1 F5; S2 S2-05; S4 F14; S6 findings 6-7; S8 findings 9-10 | DISCOVERED | §6 artifact 10 (host-annotation-contracts); gate G3; owner tier P4 |
| RCON-09 | blocker | S1 F3-F4, F10-F11; S4 F15; S5 findings 1-2 and 7-8; S8 findings 2-5 and 9-12 | DISCOVERED | §6 artifact 11 (verification-contracts) + artifact 4; gate G4; owner tier P5 |
| RCON-10 | blocker | S1 F10; S2 S2-03, S2-06-S2-08; S3 F1; S6 findings 5-6 and 10; S7 F9; S8 findings 1, 6-8 and 13; S9 H4-H6, H10-H11 | DISCOVERED | §6 artifact 11 (verification-contracts); gate G4; owner tier P5 |

## Consolidated finding titles (reconciliation §4)

| finding_id | title |
|---|---|
| RCON-01 | The reconstruction control plane is not durable |
| RCON-02 | The URL state model is not reconstructible |
| RCON-03 | Source reproduction, standards serialization, cleaning, and display are conflated |
| RCON-04 | Validation and recovery need independent verdicts and provenance |
| RCON-05 | Dials, profiles, schemes, and eligibility are not closed matrices |
| RCON-06 | Cleaning and mutation lack a state-transition and transaction contract |
| RCON-07 | Comparison keys and six join operations have no normative contract |
| RCON-08 | Host, IDNA, PSL, DNS, and IP concepts need separate typed contracts |
| RCON-09 | Vectorization, performance, caches, and dependency removal are not measurable gates |
| RCON-10 | Conformance, determinism, migration, and release claims are not executable |

