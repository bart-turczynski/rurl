# Public-surface disposition roster (§6 artifact 4 — roster half)

<!-- Contract artifact (§6 artifact 4, ROSTER half). Split out of
     public-surface-closure.md by P0.6, which separates artifact 4's INVARIANT
     (the ownership rule, legend, and citation discipline — still a G3 `## Inputs`
     hash pin) from its ROSTER (this file: one row per public-surface cell).

     WHY THE SPLIT. G3 exit criterion 1 is "the artifact set in §6 exists and
     contains no unowned cells" — a universally quantified property, not a
     statement about a particular surface size. While the invariant and the
     roster shared one file, the gate pinned both as one blob, so every added
     export drifted a G3 input hash and reopened contract-matrix closure even
     though the invariant was untouched and the new row SATISFIED it. The gate
     was pinning the extension of the property rather than the property. This
     file is therefore NOT a G3 `## Inputs` entry: it is byte-pinned in
     manifest.artifacts[] (tamper-evident, cheap to update) and its conformance
     to the invariant is asserted executably by validate-records.R on every run.

     NO LITERAL SURFACE COUNT IS AUTHORITATIVE HERE. The validator derives the
     export set from NAMESPACE and the field set from `.spu_result_fields`, and
     checks this roster against them by NAME, not by count. The counts in
     ## Bijection are documentation of the current state, verified against those
     sources — never a pinned bound. Adding an owned, SETTLED row is expected to
     change this file and expected NOT to reopen G3. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-public-surface-disposition |
| name | public-surface-disposition |
| artifact_number | 4 (roster half; the invariant half is `public-surface-closure.md`) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/public-surface-disposition.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this file is the SINGLE WRITER of the per-cell public-surface disposition roster: the owning-contract assignment and SETTLED/OPEN disposition of every exported function, public output field, curl-dependency surface, and the migration-surface row |
| lifecycle_state | PROPOSED |
| dependencies | contract-public-surface-closure (the invariant this roster instantiates); reg-public-surface-inventory (the G1 inventory, hashed below); the owning G3 contracts (canonical-state 3; standard-scheme 5; validation-intervention 6; output 7; cleaning-mutation 8; semantic-cache + host-annotation 10; key-join 9) referenced by name; P0.6 (the split) |
| bound_decision | P0.6 (the artifact-4 invariant/roster split); no product decision is made here — dispositions are projected from the owning contracts and cited per row |
| bound_evidence | reg-public-surface-inventory (the G1 inventory) |
| closes_finding | RCON-05 (public-surface registry half — roster instantiation; the invariant is closed by `public-surface-closure.md`) |
| completion_rule | §7 G3 criterion 1, roster half: every `export(...)` line in NAMESPACE has exactly one row here and every row names a real export (bijection by NAME, derived — no literal count); every `.spu_result_fields` entry has exactly one field row and vice versa; the curl-dependency and migration-surface rows are present; every row names an owning contract drawn from the ten §6 contracts; every SETTLED row cites the accepted decision its owning contract projects; every OPEN row cites an open-cell ID that EXISTS in the contract it names, or a named downstream artifact; validate-records.R (public-surface-disposition section) passes |
| content_hash | per-input sha256 under `## Inputs`, recomputed by validate-records.R on every run; this file itself is byte-pinned in manifest.artifacts[] |
| approval_evidence | pending — rides the P0.6 seal; NOT a G3 `## Inputs` entry by design (P0.6) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | public-surface-disposition section of validate-records.R enforces the invariant named in completion_rule; the roster carries no pinned counts |

## Purpose

The per-cell instantiation of artifact 4. For every public-surface cell it names
the owning G3 contract and the v3 disposition — `SETTLED` (citing the accepted
decision that contract projects) or `OPEN` (citing that contract's already-flagged
open-cell ID, or a named downstream artifact).

It makes **no product decision**. The per-surface v3 semantics live in the owning
contracts and are referenced, never redefined. The rule this roster must satisfy —
what counts as an owned cell, and what a citation must resolve to — is the
invariant half, `public-surface-closure.md`.

## Inputs

The G1 inventory this roster closes, hashed at authoring and recomputed by
`validate-records.R` on every run. The owning G3 contracts are referenced by
name; the accepted decisions they project are hash-enforced in
`manifest.decisions[]`.

Deliberately **not** pinned here: `public-surface-closure.md`. The invariant is
this roster's governing rule, and hashing it here would rebuild the very
cascade P0.6 removes — a roster edit would drift a pin the invariant's own
gate acceptance also records. The relationship is enforced by the validator
instead, which reads the legend out of the invariant and checks every row
against it.

| path | sha256 |
|---|---|
| design/work/url-v3/registers/public-surface-inventory.md | c1cb3ffca5766e40c99714985f8fc952f5608dc7e1324d9466e21079f1ba2f2b |

## Bijection

| surface class | count | source of truth | status |
|---|---|---|---|
| exported functions | 30 | `NAMESPACE` `export(...)` (30 lines; all functions, zero data) | SETTLED (`NAMESPACE` is the source of truth; the validator checks this roster against it by name) |
| public output fields | 18 | `.spu_result_fields` (`R/utils.R:215-241`) | SETTLED (unchanged; the sole public surface per P1.1@a7e0a59 B3) |
| curl-dependency surfaces | 3 | DESCRIPTION/NAMESPACE + parse + escape/unescape | SETTLED (inventoried; removal downstream) |
| migration-surface | 1 | the disposition row artifact 4 discharges | SETTLED (discharged here) |
| **total** | **52** | | |

The counts above are documentation of the current state, **verified** by
`validate-records.R` against `NAMESPACE` and `.spu_result_fields` — they are not a
pinned bound, and no gate records them. A surface change updates this table and
reopens nothing (P0.6).

The 21 Stage-A internal fields (`.spu_stage_a_fields`) remain internal, out of the
public surface (their public/internal split is the artifact-3 canonical-state
matrix, per P1.1@a7e0a59 §1.4); named here as explicit non-omission.

## Exported-function disposition roster

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

## Public-output-field disposition roster

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
| `migration-surface` | artifact 4 (this roster + its invariant) | **discharged**: every export + field now maps to an owning G3 contract with a SETTLED or OPEN disposition; no public-surface cell is unowned | SETTLED (discharged) |

## Scope boundaries

This file owns the **per-cell roster** and nothing else. It does **not** define,
and must not be read as redefining:

- **The closure invariant** — what makes a cell owned, what a `SETTLED` or `OPEN`
  citation must resolve to, and the owning-contract legend are
  `public-surface-closure.md` (artifact 4, invariant half). This roster
  instantiates that rule; it cannot relax it.
- **The per-surface v3 semantics** — every export/field's actual v3 behavior is
  the owning contract's (artifacts 3/5/6/7/8/9/10). This roster assigns owners
  and cites their SETTLED/OPEN cells; it redefines none of them.
- **The inventory register content** — `public-surface-inventory.md` (the G1
  artifact) is referenced and hashed, never edited; it stays historical
  PROPOSED/DISCOVERED G1 evidence.
- **Any open cell of its own** — no OPEN disposition below invents a question.
  Each forwards to an already-flagged open cell in an owning G3 contract or to a
  named downstream artifact, and the validator checks that the cited ID exists
  in the contract named.

## Open cells

No open cell is invented or owned here. The residual OPEN dispositions in the
roster above are grouped, with their settlement destinations, by the invariant
half as **PSC-O1 … PSC-O5** (`public-surface-closure.md`, `## Open cells`).
**Settles at:** the destinations that record names — the sibling G3 open cells
(G3.H HOST-O2/O4/O5/O7, G3.7 OUT-O1/O4/O5, G3.5 SCHEME-O1/O2/O3, G3.8
CLEAN-O1/MUT-O8/MUT-O9, G3.6 VAL-O3/VAL-O4), the unmade P4 host record
(RCON-08), and §6 artifact 11 / G4 for the verification surfaces.

Grouping them there rather than here is deliberate: which questions remain open
is a property of the ownership model, which is gate-pinned, not of the roster,
which is not.
