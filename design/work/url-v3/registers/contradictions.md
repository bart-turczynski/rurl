# Register: contradictions (§6 artifact 2)

<!-- variant: contradiction (schema/record-schemas.yaml). Row-based register.
     G2 (RURL-oknltrux). Validated by tools/validate-records.R (contradictions
     section). FINALIZED: every §5 contradiction now carries an owner disposition
     (§7 G2 exit), projected from the accepted P-tier — disposition_state
     ACCEPTED, a typed disposition_type, and an owner_decision_ref naming the
     accepted decision. The register envelope stays lifecycle_state PROPOSED until
     the cp-snapshot-2 seal (the artifact's own manifest acceptance). -->


## Envelope

| Field | Value |
|---|---|
| id | reg-contradictions |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/registers/contradictions.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| dependencies | protocol-review-reconciliation.md §4,§5,§8; findings.md |
| completion_rule | the 12-row C-01..C-12 bijection (no missing/extra/dup); every row disposition_state ACCEPTED with a typed disposition_type in {ACCEPTED, REJECTED, SUPERSEDED, COMPATIBILITY-ONLY}, a non-placeholder owner_decision_ref, and filled affected_claims/invalidated_artifacts; every row carries a bound_owner_tier and a related_rcon within RCON-01..RCON-10; validate-records.R passes |
| content_hash | derived from reconciliation §5 (C-01..C-12 verbatim) + §4/§8; RCON binding cross-checked against findings.md |
| approval_evidence | pending — pins at v3/cp-snapshot-2 (NOT the sealed cp-snapshot-1 manifest) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |

## Purpose

The §6 contradiction register. It transfers the twelve contradictions from
reconciliation §5 verbatim (their text and their *required* disposition) into
typed rows, binds each to the owner-decision P-tier(s) that dispose it (§8) and
to the consolidated finding(s) it stems from (RCON-01..RCON-10, §4).

G2 finalization (RURL-oknltrux): every row now carries an owner disposition,
projected from the now-ACCEPTED P-tier. Each contradiction's typed disposition
(`ACCEPTED`, `REJECTED`, `SUPERSEDED`, or `COMPATIBILITY-ONLY`) was decided by
its bound P-tier decision record — with authority, rationale, affected claims,
and invalidated artifacts recorded there (reconciliation §5, §6 lifecycle) —
and is transcribed here: `disposition_type` = the primary typed decision,
`disposition_state = ACCEPTED` (the disposition is owner-accepted, independent of
`disposition_type`, which may itself be `REJECTED`/`SUPERSEDED`),
`owner_decision_ref` = the accepted decision(s) at their acceptance SHA, and
`affected_claims`/`invalidated_artifacts` filled from the record's consequences.
Where a decision carries a compat-only *residue* alongside its primary type
(C-03, C-07, C-10, C-11), the primary type is recorded here and the residue lives
in the bound record via `owner_decision_ref`. The `required_disposition` column
remains the §5-mandated *shape* of the decision, transferred verbatim. C-01 has
no product P-tier: it is disposed by the owner's G2 process/evidence merge. A
contradiction row closes when its owner disposition and verification evidence are
recorded (gate G2; reconciliation §6, §7 G2); the register envelope itself seals
into the manifest at cp-snapshot-2.

## Rows

| id | contradiction | required_disposition | related_rcon | bound_owner_tier | disposition_type | disposition_state | authority | owner_decision_ref | affected_claims | invalidated_artifacts | verification_ref |
|---|---|---|---|---|---|---|---|---|---|---|---|
| C-01 | Accepted URL-standard PRD wrappers call the documents accepted/shipped while their internal status fields still say draft. | Correct the durable metadata or record the status field as superseded; classify claims section by section until then. | RCON-01 | process (no P-tier) | ACCEPTED | ACCEPTED | owner (process/evidence merge) | process/G2 owner merge | accepted/shipped PRD-wrapper status vs stale draft internal status fields; durable metadata corrected by the P0.1 lifecycle-stamping discipline | none — stale draft status corrected in place under RCON-01 supersession-state discipline; no document overturned | G2.A |
| C-02 | Accepted browser-fixer prose and shipped phase ordering disagree. | Choose and trace one ordering; label the other superseded or compatibility-only. | RCON-03, RCON-04 | P2.1 | SUPERSEDED | ACCEPTED | owner | P2.1@a4d1b45 | shipped browser-fixer phase ordering (complete fixer pass first, then WHATWG preprocessing as parser behavior) | browser-fixer PRD Part 1 interleaved bullet-list ordering (superseded by its own contiguous numbered ordering block) | P2.1 decision record; G2.A |
| C-03 | Direct RFC behavior recovers some repeated-`@` inputs while RFC-general rejects them. | Classify recovery as syntax, compatibility, or repair and bind it to an entry point. | RCON-03, RCON-04 | P2.1 | ACCEPTED | ACCEPTED | owner | P2.1@a4d1b45 | repeated-@ recovery classified REPAIR; excluded from strict, retained under compatibility/repair postures only (compat-only sub-disposition on the direct-selector path) | none — recovery reclassified across postures; no artifact overturned by the classification | P2.1 decision record; G2.A |
| C-04 | ADR 0012 requires fragment-preserving standard serialization while shipped clean serializers explicitly omit fragments. | Separate standard serialization from legacy clean output; decide whether the ADR work is unimplemented or superseded. | RCON-03 | P2.2 | ACCEPTED | ACCEPTED | owner | P2.2@8292c7f | standard serialization separated from clean output; ADR 0012 D2 fragment-preserving serializer unimplemented/scheduled, not superseded | none — ADR 0012 D2 applied not overturned; clean serializers legitimately keep omitting fragments | P2.2 decision record; G2.A |
| C-05 | ADR 0011 describes WHATWG encode behavior as lossy for encoded `/`, while shipped code preserves `%2F`. | Decide legacy compatibility and keep that dial out of the standards serializer contract. | RCON-03 | P2.2 | COMPATIBILITY-ONLY | ACCEPTED | owner | P2.2@8292c7f | shipped WHATWG %2F preservation retained as a clean/display path_encoding dial, kept out of the standard serializer contract | none — legacy %2F compat retained; ADR 0011 presentation-axis classification applied not overturned | P2.2 decision record; G2.A |
| C-06 | Accepted identity guidance separates identity from presentation, while shipped `canonical_join()` keys on `clean_url`. | Introduce the independent key contract and an explicit migration plan; do not reinterpret the shipped join as v3-compliant. | RCON-07 | P3.1 | COMPATIBILITY-ONLY | ACCEPTED | owner | P3.1@3b89b94 | shipped clean_url-keyed canonical_join() typed legacy with a deprecation window; independent identity key get_url_key() introduced | none — ADR 0011/0012 identity-before-presentation invariant upheld; clean_url-as-join-identity reframed legacy not superseded | P3.1 decision record; G2.A |
| C-07 | `parse_status` includes PSL-derived warning states although v3 proposals describe syntax, policy, and optional facts as separate layers. | Decide the v3 status model and preserve or migrate legacy projections explicitly. | RCON-02, RCON-04 | P1.1, P2.3 | ACCEPTED | ACCEPTED | owner | P1.1+P2.3@a7e0a59 | v3 three-layer verdict model (syntax/policy/optional PSL-annotation) adopted; legacy parse_status kept byte-identical as a lossy compat projection | none — additive; parse_status byte-identical, ADR 0006 amended to add get_parse_verdicts companion with no parse-frame widening | P1.1, P2.3 decision records; G2.A |
| C-08 | Runtime initializes the full-parse cache with a 100,000-entry bound, while `README.Rmd` says it is unbounded by default. | Correct one side and add a documentation-consistency gate. | RCON-09 | P5.1 | ACCEPTED | ACCEPTED | owner | P5.1@d254ff1 | runtime full_parse 100,000-entry bounded default is authoritative; a documentation-consistency gate is added to the verify chain | README.Rmd unbounded-by-default sentence — factually wrong, corrected to the 100,000 bounded default | P5.1 decision record; G2.A |
| C-09 | The determinism workflow calls itself evidence collection, never fails on divergence, and runs automatically only for harness changes. | Add a separate failing acceptance comparison and approved-exception mechanism. | RCON-09, RCON-10 | P5.2 | ACCEPTED | ACCEPTED | owner | P5.2@71b43bb | new failing determinism acceptance gate, zero-unapproved-divergence tolerance, and an approved-exception register; shipped probe stays non-gating | none — additive gate; determinism-probe.yml keeps its evidence-collection non-gating posture | P5.2 decision record; G2.A |
| C-10 | The protocol prohibits CRAN release until curl-free v3, while accepted tracker history permits a v2.7 release after its determinism gate. | Name the release line governed by the hold and record the owner decision. | RCON-10 | P0.4 | ACCEPTED | ACCEPTED | owner | P0.4@7bc5358 | curl-free-before-CRAN hold scoped to the v3 (3.0.0) line; v2.7 curl-bearing release governed by its own determinism gate on the v2.x line | none — protocol CRAN sentence scoped/annotated not superseded; v2.7 epic release disposition preserved | P0.4 decision record; G2.A |
| C-11 | Accepted general-parser scope allows arbitrary schemes, while older ADR 0004 records a closed web scheme set. | Apply ADR 0012's explicit supersession precisely and retain the old rule only where compatibility/default policy still calls for it. | RCON-05 | P2.4, P4.1 | SUPERSEDED | ACCEPTED | owner | P2.4+P4.1@b017e87 | ADR 0004 closed web scheme set superseded as a parser cap; retained compat-only as the default web allowlist; general admits any valid scheme token | ADR 0004 closed-scheme standing rule — superseded by ADR 0012 (P2.4 records the already-accepted supersession) | P2.4, P4.1 decision records; G2.A |
| C-12 | Historical drafts make strong conformance claims from projections while also acknowledging the missing faithful serialization surface. | Preserve them as historical evidence only; rebuild claims from full state and full-string outputs. | RCON-09, RCON-10 | P2.2, P5.3 | COMPATIBILITY-ONLY | ACCEPTED | owner | P2.2+P5.3@8292c7f | historical projection-based conformance claims preserved as evidence and retired as live oracles; v3 claims rebuilt on the FSSS full-string + parse→serialize→parse | none — projection history re-classified (retained) not deleted; ADR 0012 parity-not-oracle stance applied; FSSS is a new surface | P2.2, P5.3 decision records; G2.A |

## Provenance

Per-row justification for the `related_rcon` binding, derived from
reconciliation §4 (RCON descriptions + Sources) and §5 (the contradiction text),
and cross-checked against `findings.md` and the §9 finding-coverage map. Each
binding below holds on inspection; none required a `design-fork` flag.

- **C-01 → RCON-01.** RCON-01 (§4, lines 85-101) is the durable control-plane
  finding; its required amendment names "supersession state" and closing
  existing authority/supersession contradictions. The §5 text is exactly an
  accepted/shipped-vs-draft *metadata/supersession* contradiction, and its
  required disposition ("record the status field as superseded") is
  supersession-state work. Source finding S9 H3 ("Existing authority and
  supersession contradictions are not forced to closure") maps to RCON-01 in
  §9 (line 552). Disposed as a process/evidence merge, so no product P-tier.
- **C-02 → RCON-03, RCON-04.** The browser-fixer phase order is the ordered
  intervention ledger of RCON-04 (§4, lines 154-176: "browser repair" as a
  distinct, ordered transformation category); source S2 S2-09 ("browser-fixer
  phase order is contradictory across accepted prose and shipped code") maps to
  RCON-04 (§9, line 510-511 group; §4 line 157). It also touches RCON-03's
  separation of repair/cleaning/display output contracts (§4 lines 125-152;
  S6 finding 9 "…/fixer distinctions" → RCON-03, §9 line 534).
- **C-03 → RCON-03, RCON-04.** Repeated-`@` recovery is a validation/recovery
  and entry-point classification concern: RCON-04's "repaired-input conformance",
  "strict versus repairing entry points", and repair authorization/ordering
  (§4 lines 154-176). The `@`/userinfo modeling is an RCON-03 output-contract /
  credential-route concern (§4 lines 125-152; S6 finding 3 "Credentials/userinfo
  … distinct routes" → RCON-03, §9 line 534). Required disposition explicitly
  asks to "bind it to an entry point" (RCON-04).
- **C-04 → RCON-03.** RCON-03 (§4 lines 125-152) is precisely "standards
  serialization is not separated from lossy clean output"; §10 direct evidence
  (line 568) verifies "Current clean serializers omit fragments". The §5 text
  (fragment-preserving standard serialization vs clean omitting fragments) is
  the RCON-03 conflation. Sources S1 F2, S3 F1 → RCON-03 (§9 lines 502-group).
- **C-05 → RCON-03.** WHATWG encode behavior for encoded `/` (`%2F`) is an
  encoding-policy concern of the RCON-03 serializer contracts (§4 line 145-148:
  "encoding policy"; required disposition keeps the legacy dial "out of the
  standards serializer contract"). ADR 0011 is the path-encoding-orthogonality
  ADR, squarely an output/serialization presentation axis (RCON-03).
- **C-06 → RCON-07.** RCON-07 (§4 lines 224-245) is the comparison-key/join
  finding and states verbatim: "Shipped `canonical_join()` derives its key from
  lossy `clean_url`; that is legacy behavior, not the proposed v3 identity
  model." The §5 text and required disposition ("independent key contract +
  migration plan") are RCON-07's amendment. Source S5 finding 3 → RCON-07 (§9).
- **C-07 → RCON-02, RCON-04.** RCON-04 (§4 line 158-160) states verbatim:
  "`parse_status` currently combines parse success with host/PSL-derived warning
  states"; the required multi-verdict record is RCON-04. The v3 status *model*
  and its public projection are canonical-state concerns (RCON-02, §4 lines
  104-124). Source S7 F3 ("optional annotation outcomes and parse validity have
  no required state model") maps to RCON-02 and RCON-04 (§9 line 541).
- **C-08 → RCON-09.** RCON-09 (§4 lines 270-291) is the cache/vector/dependency
  measurable-gate finding; §10 direct evidence (line 571) verifies "Runtime
  bound 100,000; README says unbounded default" as a cache-documentation
  disagreement. Source S8 finding 5 ("Cache behavior … contradicts itself") →
  RCON-09 (§9 line 548-group). Required disposition adds a documentation gate.
- **C-09 → RCON-09, RCON-10.** RCON-10 (§4 lines 293-310) states verbatim: "The
  current determinism workflow is explicitly evidence collection and does not
  fail on cross-platform divergence"; required disposition ("failing acceptance
  comparison + approved-exception mechanism") is the RCON-10 executable-gate
  amendment. Determinism of caches/PSL-engine/parallel is also an RCON-09
  reproducibility concern (S8 finding 9 chunk/worker determinism → RCON-05,
  RCON-08, RCON-09, §9 line 551). Source S8 finding 7 → RCON-10.
- **C-10 → RCON-10.** RCON-10 (§4 lines 303-304) states verbatim: "The stated
  CRAN prohibition also conflicts with the latest accepted release disposition
  unless its target version is clarified." Required disposition names the
  release line. Source S8 finding 1 ("The CRAN rule conflicts with the accepted
  epic's latest release disposition") → RCON-10 (§9 line 547-group).
- **C-11 → RCON-05.** RCON-05 (§4 lines 178-198) is the closed-matrices finding
  for "schemes … scheme admission", with open decisions including "compatibility
  aliases and deprecations" and "v3 default admission". The §5 text (general
  parser's arbitrary schemes vs ADR 0004's closed web scheme set) and required
  disposition (apply ADR 0012 supersession, retain the old rule for compat) are
  RCON-05 scheme-admission work. Source S6 findings 1-2 → RCON-05 (§9 line 533).
- **C-12 → RCON-09, RCON-10.** RCON-10 (§4 lines 296-299) is the executable
  conformance finding: the protocol "permits examples, component projections …
  without requiring claim-level provenance"; §5 asks to "rebuild claims from
  full state and full-string outputs" (RCON-10). "Full state" and metamorphic
  state assertions are also RCON-09. Source S1 F10 ("does not mandate oracle
  classification plus metamorphic state assertions") maps to RCON-09 and RCON-10
  in §9 (line 508). Source S3 F1 additionally anchors the projection concern.
