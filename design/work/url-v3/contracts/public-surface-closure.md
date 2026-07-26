# Public-surface closure (§6 artifact 4)

<!-- Contract artifact (§6 artifact 4). RCON-05 (public-surface half) closure —
     the explicit G1→G3 handoff for the public-surface inventory. This record does
     NOT re-author the G1 inventory (register public-surface-inventory.md, 51 rows);
     it PROJECTS every inventory row's "unknown — RCON-0X pending owner tier PY"
     migration disposition onto the owning G3 contract that now governs it, marking
     each SETTLED (with the accepted decision the owning contract projects) or OPEN
     (with the exact open-cell ID in the owning contract, or a downstream artifact
     for surfaces outside G3). It makes NO new product decision: it assigns owners,
     it does not decide. The 29-export + 18-field + curl + migration-surface
     bijection is unchanged from baseline 89be90b. This is the SINGLE WRITER of the
     public-surface disposition-closure map; the per-export/field v3 SEMANTICS live
     in the owning contracts (3/5/6/7/8/9/10/K) and are referenced, never
     redefined. Terms/status/state fields are asserted to agree with artifacts 3
     and 5–10 (full cross-artifact enforcement is the G3.X capstone). Format follows
     the G3.3/G3.5/G3.6/G3.7/G3.8/G3.H/G3.K precedent. Envelope stays
     lifecycle_state PROPOSED until the cp-snapshot-3 seal; validator coverage +
     the register's normative-closure state flip ride that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-public-surface-closure |
| name | public-surface-closure |
| artifact_number | 4 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/public-surface-closure.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the public-surface disposition-closure map: the owning-contract assignment and SETTLED/OPEN v3 disposition of every exported function, public output field, curl-dependency surface, and the migration-surface row |
| lifecycle_state | PROPOSED |
| dependencies | reg-public-surface-inventory (the G1 inventory being closed, hashed below); the owning G3 contracts (canonical-state artifact 3; standard-scheme 5; validation-intervention 6; output 7; cleaning-mutation 8; semantic-cache + host-annotation 10; key-join 9) referenced by name; the accepted decisions those contracts project (hash-enforced in manifest); reconciliation §6 artifact 4, §4 RCON-05, §7 G1/G3 |
| bound_decision | none new — this record assigns owners; the governing decisions (P1.1/P1.2/P2.1–P2.4/P3.1/P3.2/P4.1/P5.1/P5.3) are projected by the owning contracts and cited per row |
| bound_evidence | reg-public-surface-inventory (the G1 inventory) |
| closes_finding | RCON-05 (public-surface registry half; the standard/profile/scheme-matrix half is G3.5) |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: every one of the 51 inventory rows (29 exported functions + 18 public output fields + 3 curl-dependency + 1 migration-surface) carries an owning G3 contract and a v3 disposition that is SETTLED (citing the owning contract's projected accepted decision) or OPEN (citing the owning contract's open-cell ID or a named downstream artifact); the 29+18 bijection matches NAMESPACE/`.spu_result_fields`; terms/status/state fields are asserted to agree with artifacts 3 and 5–10; validate-records.R (public-surface-closure section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (the inventory register), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip); the G1 inventory register is NOT transitioned by the seal — it stays historical PROPOSED/DISCOVERED evidence, present:true-pinned since cp-snapshot-2 |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | public-surface-closure validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative closure of the §6 artifact-4 public-surface inventory: it takes the
G1 inventory (51 DISCOVERED rows) through the G3 handoff by assigning every
export, field, and dependency surface to the owning G3 contract that now governs
its v3 disposition, and marking each SETTLED or OPEN. It settles no new product
decision — the per-surface v3 semantics live in the owning contracts; this record
is the *ownership map* that proves no public-surface cell is unowned.

This record **projects** the inventory register onto the owning contracts; it
makes no product decision. A `SETTLED` disposition cites the owning contract plus
the accepted decision that contract projects (e.g. "G3.6 / P2.3@a7e0a59"). An
`OPEN` disposition cites the owning contract's already-flagged open-cell ID (e.g.
"G3.8 CLEAN-O1") or a named downstream artifact (§6 artifact 11 / the unmade P4
host record) — never a new open question invented here.

## Inputs

The exact source this contract closes, hashed at authoring. `validate-records.R`
recomputes the hash at the cp-snapshot-3 seal. The owning G3 contracts are
referenced by name (siblings riding the same seal); the accepted decisions they
project are already hash-enforced by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/registers/public-surface-inventory.md | c1cb3ffca5766e40c99714985f8fc952f5608dc7e1324d9466e21079f1ba2f2b |

## Bijection (unchanged from baseline 89be90b)

| surface class | count | source of truth | status |
|---|---|---|---|
| exported functions | 30 | `NAMESPACE` `export(...)` (30 lines; all functions, zero data) | SETTLED (+1: `get_parse_verdicts`, the P2.3 §2 layered companion ADR 0006 was amended to authorize) |
| public output fields | 18 | `.spu_result_fields` (`R/utils.R:215-241`) | SETTLED (unchanged; the sole public surface per P1.1@a7e0a59 B3) |
| curl-dependency surfaces | 3 | DESCRIPTION/NAMESPACE + parse + escape/unescape | SETTLED (inventoried; removal downstream) |
| migration-surface | 1 | the disposition row this closure discharges | SETTLED (discharged here) |
| **total** | **52** | | |

The 21 Stage-A internal fields (`.spu_stage_a_fields`) remain internal, out of the
public surface (their public/internal split is the artifact-3 canonical-state
matrix, per P1.1@a7e0a59 §1.4); named here as explicit non-omission.

## Owning-contract legend

| G3 leaf | contract | §6 artifact | governing accepted decisions |
|---|---|---|---|
| G3.3 | canonical-state-contract | 3 | P1.1@a7e0a59, P1.2@bb3346e |
| G3.5 | standard-scheme-matrices | 5 | P2.4@b017e87, P4.1@b017e87 |
| G3.6 | validation-intervention-contract | 6 | P2.1@a4d1b45, P2.3@a7e0a59, P1.1@a7e0a59 |
| G3.7 | output-contracts | 7 | P2.2@8292c7f, P5.3@8292c7f, P3.1@3b89b94 |
| G3.8 | cleaning-mutation-contracts | 8 | P2.2@8292c7f, P3.1@3b89b94 |
| G3.9 | semantic-cache-contract | 10 (cache) | P5.1@d254ff1 |
| G3.H | host-annotation-contracts | 10 (host) | P4.1@b017e87 + host-facing boundaries (RCON-08 deep record UNMADE) |
| G3.K | key-join-contracts | 9 | P3.1@3b89b94, P3.2@bb3346e |
| — | verification contracts (NOT a G3 leaf) | 11 | RCON-09/RCON-10; G4 / P5.1 / P5.3 (downstream) |

## Exported-function disposition closure (29)

| export | owning contract(s) | v3 disposition | status |
|---|---|---|---|
| `canonical_join` | G3.K | migration to identity key, `clean_url` keying typed LEGACY with a deprecation window (P3.1@3b89b94 D-E, Q7/B7) | SETTLED |
| `check_hosts` | G3.H | named host policies + no-network SETTLED; typed DNS-policy contract → G3.H HOST-O5 | OPEN (HOST-O5) |
| `get_clean_url` | G3.8 (+ G3.7 surface c) | clean surface + 25 dials governed (P2.2@8292c7f §1c; G3.8); vocabulary rename → G3.8 CLEAN-O1 | OPEN (CLEAN-O1) |
| `get_domain` | G3.H | PSL delegation + identity spellings SETTLED (ADR 0001); `domain` overload → HOST-O2, PSL reproducibility → HOST-O4 | OPEN (HOST-O2/O4) |
| `get_fragment` | G3.3 | fragment field + three-valued presence (P1.1@a7e0a59 §1.2) | SETTLED |
| `get_host` | G3.H | host presence/form SETTLED (P1.2@bb3346e); full-host-identity → HOST-O7 | OPEN (HOST-O7) |
| `get_host_type` | G3.H (+ G3.6 companion) | companion diagnostic SETTLED (ADR 0006); de-overload → HOST-O2 | OPEN (HOST-O2) |
| `get_mailto_recipients` | G3.5 | recipient projection + `smtp_wire` SETTLED (P4.1@b017e87 D-C; ADR 0012 D7); email facts → G3.5 SCHEME-O3 | OPEN (SCHEME-O3) |
| `get_parse_status` | G3.6 | π(L1,L2,L3) compat projection, byte-identical retain (P2.3@a7e0a59 §4-5); deprecation window → VAL-O4 | OPEN (VAL-O4) |
| `get_parse_verdicts` | G3.6 | layered L1/L2/L3 companion, ADR 0006 amended to authorize it (P2.3@a7e0a59 §1-§2); defined at `url_standard = NULL`; L3 enum membership G3.6 | SETTLED |
| `get_password` | G3.7 (+ G3.5) | `user`/`password` split SETTLED; selector parity → G3.5 SCHEME-O2, undivided-userinfo → G3.7 OUT-O1 | OPEN (SCHEME-O2/OUT-O1) |
| `get_path` | G3.7 (+ G3.8) | path presentation dials SETTLED (ADR 0011; P2.2@8292c7f §3); path mutation → G3.8 MUT-O9 | OPEN (MUT-O9) |
| `get_port` | G3.K | thin port accessor; port equivalence truth table (P3.2@bb3346e; P3.1@3b89b94 D-B) | SETTLED |
| `get_query` | G3.8 | query cleaning dials SETTLED; ordered-multimap mutation → G3.8 MUT-O8 | OPEN (MUT-O8) |
| `get_scheme` | G3.5 | scheme admission/inference (P2.4@b017e87 D-B/D-C) | SETTLED |
| `get_scheme_class` | G3.5 | companion scheme-class diagnostic (P4.1@b017e87 D-A.3; ADR 0006) | SETTLED |
| `get_subdomain` | G3.H | PSL delegation SETTLED (ADR 0001); reproducibility → HOST-O4 | OPEN (HOST-O4) |
| `get_tld` | G3.H | PSL suffix delegation + identity spellings SETTLED (ADR 0001); reproducibility → HOST-O4 | OPEN (HOST-O4) |
| `get_url_diagnostics` | G3.6 (+ G3.H) | aggregator keeps token-list shape (P2.3@a7e0a59 §2; ADR 0006) | SETTLED |
| `get_user` | G3.7 | credential split SETTLED; undivided-userinfo → OUT-O1 | OPEN (OUT-O1) |
| `get_userinfo` | G3.7 | internal reassembly completeness SETTLED (S3-F5); public undivided-userinfo → OUT-O1 | OPEN (OUT-O1) |
| `is_valid_host` | G3.H | named policies + no-network SETTLED (host-validation-policy PRD); typed DNS contract → HOST-O5 | OPEN (HOST-O5) |
| `query_param_summary` | G3.8 (+ artifact 11) | query filtering model SETTLED (G3.8); introspection budget (RCON-09) → §6 artifact 11 / G4 | OPEN (artifact 11) |
| `resolve_url` | G3.6 (+ G3.7) | verdict layering SETTLED (P2.3@a7e0a59 §3); output shape → G3.6 VAL-O3 / G3.7 OUT-O4 | OPEN (VAL-O3/OUT-O4) |
| `rurl_cache_config` | G3.9 | cache contract, 100k watermark, doc-drift C-08 (P5.1@d254ff1) | SETTLED |
| `rurl_cache_info` | G3.9 | cache transparency introspection (P5.1@d254ff1) | SETTLED |
| `rurl_clear_caches` | G3.9 | cache lifecycle/reset (P5.1@d254ff1) | SETTLED |
| `safe_parse_url` | G3.3 (+ G3.5) | 18-field projection = sole public surface (P1.1@a7e0a59 B3); dial matrix (G3.5) | SETTLED |
| `safe_parse_urls` | G3.3 (+ artifact 11) | row-local recovery SETTLED (P1.1@a7e0a59 §3); the S1-F3 scalar/vector cell matrix + vectorization budget → §6 artifact 11 / the S1 scalar/vector contract | OPEN (artifact 11 / S1 s/v) |
| `url_profile` | G3.5 | profile expansion (P2.4@b017e87, P4.1@b017e87 D-B); direct-vs-profile divergence → G3.5 SCHEME-O1 | OPEN (SCHEME-O1) |

## Public-output-field disposition closure (18)

| field | owning contract(s) | v3 disposition | status |
|---|---|---|---|
| `original_url` | G3.3 (+ G3.7 surface a) | source field + provenance on every row (P1.1@a7e0a59 §3.2); byte-vs-`Encoding()` guarantee → G3.7 OUT-O5 | OPEN (OUT-O5) |
| `scheme` | G3.3 (+ G3.5) | presentation projection of `final_scheme` (P1.1@a7e0a59 §1.3); admission (G3.5) | SETTLED |
| `host` | G3.3 (+ G3.H) | presentation host + `host_kind` presence (P1.1@a7e0a59; P1.2@bb3346e); full-host-identity → HOST-O7 | OPEN (HOST-O7) |
| `port` | G3.3 (+ G3.K) | presentation port under `port_handling` (P1.1@a7e0a59); equivalence table (G3.K) | SETTLED |
| `path` | G3.3 (+ G3.7/G3.8) | Stage-B path projection, identity distinct from display (P1.1@a7e0a59 §1.3; ADR 0011) | SETTLED |
| `query` | G3.3 (+ G3.8) | three-valued presence ends empty-vs-absent collapse (P1.1@a7e0a59 §1.2); ordered-multimap mutation → MUT-O8 | OPEN (MUT-O8) |
| `fragment` | G3.3 | three-valued presence (P1.1@a7e0a59 §1.2) | SETTLED |
| `user` | G3.3 (+ G3.7) | parsed credential, distinct from source slice (P1.1@a7e0a59 §1.3); undivided-userinfo → OUT-O1 | OPEN (OUT-O1) |
| `password` | G3.3 (+ G3.7) | parsed credential (P1.1@a7e0a59 §1.3); undivided-userinfo → OUT-O1 | OPEN (OUT-O1) |
| `domain` | G3.3 (+ G3.H) | L3 PSL annotation in `host_encoding` spelling (P1.1@a7e0a59 §2); `domain` overload → HOST-O2 | OPEN (HOST-O2) |
| `tld` | G3.3 (+ G3.H) | L3 PSL public-suffix annotation (P1.1@a7e0a59 §2); reproducibility → HOST-O4 | OPEN (HOST-O4) |
| `domain_ascii` | G3.3 (+ G3.H) | encoding-independent registrable-domain identity key (P1.1@a7e0a59; ADR 0001) | SETTLED |
| `domain_unicode` | G3.3 (+ G3.H) | encoding-independent registrable-domain identity key (P1.1@a7e0a59) | SETTLED |
| `tld_ascii` | G3.3 (+ G3.H) | encoding-independent public-suffix identity key (P1.1@a7e0a59) | SETTLED |
| `tld_unicode` | G3.3 (+ G3.H) | encoding-independent public-suffix identity key (P1.1@a7e0a59) | SETTLED |
| `is_ip_host` | G3.3 (+ G3.H) | consumed fact spanning public + Stage-A (P1.1@a7e0a59 §1.4); IP annotation ineligibility (G3.H) | SETTLED |
| `clean_url` | G3.7 (+ G3.8) | clean surface (c), intentionally lossy, never identity (P2.2@8292c7f §1c, §5.1) | SETTLED |
| `parse_status` | G3.6 | π(L1,L2,L3) compat projection, byte-identical (P2.3@a7e0a59 §4-5); deprecation window → VAL-O4 | OPEN (VAL-O4) |

## curl-dependency + migration-surface disposition

| item | owning contract | v3 disposition | status |
|---|---|---|---|
| `curl-import-metadata` | §6 artifact 11 (verification) | curl-removal closure + negative gate (RCON-09) is a G4 / P5.1 concern **outside G3**; removal "needs more than replacing the parser call" (§10) | OPEN (artifact 11 / G4) |
| `curl-parse-call` | §6 artifact 11 | primary parser-call removal (RCON-09); downstream | OPEN (artifact 11 / G4) |
| `curl-escape-unescape` | §6 artifact 11 | percent enc/dec removal spans path/query/accessor/email (RCON-09); downstream | OPEN (artifact 11 / G4) |
| `migration-surface` | this closure | **discharged**: every export + field now maps to an owning G3 contract with a SETTLED or OPEN disposition; no public-surface cell is unowned | SETTLED (discharged) |

## Cross-artifact term / status / state-field agreement

The closure asserts the inventory's vocabulary agrees with the owning contracts
(full cross-artifact enforcement is the G3.X capstone; this section is the
public-surface slice of it).

| shared concept | canonical owner | agreement | status |
|---|---|---|---|
| the 18 public fields + three-valued presence | artifact 3 (G3.3) | every `public-output-field` row names an artifact-3 field with its presence/provenance | SETTLED |
| `parse_status` = π(L1,L2,L3) compat projection | artifact 6 (G3.6) | the `parse_status` / `get_parse_status` rows are the π projection, not a core verdict | SETTLED |
| `clean_url` = clean surface (c), not identity | artifact 7 (G3.7) | the `clean_url` / `get_clean_url` rows are surface (c); `canonical_join` `clean_url` keying is LEGACY | SETTLED |
| comparison key = `get_url_key`, not `clean_url` | artifact 9 (G3.K) | `canonical_join` migrates off `clean_url` to the identity key | SETTLED |
| host `domain*` = PSL registrable, not full-host | artifact 10 host (G3.H) | the `domain`/`domain_ascii`/… rows are PSL identity keys, not full-host identity | SETTLED |
| cache config/info/clear semantics | artifact 10 cache (G3.9) | the three `rurl_cache_*` rows defer cache semantics to G3.9 | SETTLED |
| scheme admission/interpretation + profile lattice | artifact 5 (G3.5) | `get_scheme`/`get_scheme_class`/`url_profile` rows defer to G3.5 | SETTLED |
| companion-helpers never widen the parse frame | ADR 0006 (via G3.6) | `get_host_type`/`get_scheme_class`/`get_url_diagnostics`/`get_parse_verdicts` stay companion | SETTLED |

## Scope boundaries

This contract owns the **public-surface disposition-closure map**. It does **not**
define, and must not be read as redefining:

- **The per-surface v3 semantics** — every export/field's actual v3 behavior is
  the owning contract's (artifacts 3/5/6/7/8/9/10). This record assigns owners and
  cites their SETTLED/OPEN cells; it redefines none of them.
- **The inventory register content** — `public-surface-inventory.md` (the G1
  artifact, 51 rows) is referenced and hashed, never edited; it stays historical
  PROPOSED/DISCOVERED G1 evidence (present:true-pinned at cp-snapshot-2). THIS
  closure contract — not a register edit — supplies the normative closure; the
  register's rows are not transitioned by the seal.
- **Verification contracts (§6 artifact 11)** — curl-removal, the vectorization/
  performance budgets, cache-transparency executable gates, oracle register, and
  introspection budgets (RCON-09/RCON-10) are **outside G3** (G4 / P5.1 / P5.3).
  Rows that defer there are OPEN-downstream, not open here.
- **The unmade P4 host record (RCON-08)** — host rows deferring to HOST-O1..O8
  point at that unmade owner decision (G3.H records them OPEN).
- **Full cross-artifact consistency** — the exhaustive terms/defaults/state-fields/
  status-codes enforcement across all ten artifacts is the **G3.X** capstone; this
  record asserts only the public-surface slice.

## Open cells

No open cell is invented here. Every OPEN disposition above forwards to an
already-flagged open cell in an owning G3 contract or to a named downstream
artifact. The residual open dispositions, grouped by destination:

- **PSC-O1 — host-surface dispositions → the unmade P4 host record (RCON-08).**
  `check_hosts`, `get_domain`, `get_host`, `get_host_type`, `get_subdomain`,
  `get_tld`, `is_valid_host`, and the `host`/`domain`/`tld` fields forward to
  G3.H HOST-O2/O4/O5/O7. **Impact:** their v3 host contract is unspecified until
  the P4 host record lands. **Settles at:** the P4 host record (G3.H open cells).
- **PSC-O2 — credential-surface dispositions → G3.7 OUT-O1 / G3.5 SCHEME-O2.**
  `get_user`, `get_password`, `get_userinfo`, and the `user`/`password` fields
  forward to the undivided-userinfo (OUT-O1) and `get_password` selector-parity
  (SCHEME-O2) open cells. **Impact:** the public credential surface's undivided
  form + selector parity are unspecified. **Settles at:** the credential-accessor
  slice (G3.7 OUT-O1, G3.5 SCHEME-O2).
- **PSC-O3 — cleaning/output/query dispositions → G3.8 / G3.7 open cells.**
  `get_clean_url` (CLEAN-O1), `get_path`/`get_query` and the `query` field
  (MUT-O8/O9), `resolve_url` (VAL-O3/OUT-O4), `original_url` (OUT-O5). **Impact:**
  the cleaning-vocabulary rename, query/path mutation model, resolver output shape,
  and source-reproduction guarantee are unspecified. **Settles at:** the named
  sibling open cells.
- **PSC-O4 — status/scheme/email dispositions → G3.6 / G3.5 open cells.**
  `get_parse_status` + `parse_status` field (VAL-O4), `get_mailto_recipients`
  (SCHEME-O3), `url_profile` (SCHEME-O1). **Impact:** the `parse_status`
  deprecation window, email diagnostics, and direct-vs-profile precedence are
  unspecified. **Settles at:** the named sibling open cells.
- **PSC-O5 — verification-surface dispositions → §6 artifact 11 / G4.**
  `query_param_summary` (introspection), `safe_parse_urls` (vectorization / the
  S1-F3 scalar/vector cell matrix), and the three `curl-*` dependency rows
  (curl-removal). **Impact:** these surfaces' executable/migration contracts live
  outside G3. **Settles at:** §6 artifact 11 verification contracts + the S1
  scalar/vector contract (G4 / P5.1 / P5.3).
