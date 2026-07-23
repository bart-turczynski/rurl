# Canonical state contract (§6 artifact 3)

<!-- Contract artifact (§6 artifact 3). RCON-02 closure. This record PROJECTS the
     ACCEPTED owner decision P1.1 (v3 state/status model) and the frozen evidence
     S1 (object state vector) into the normative canonical FIELD VOCABULARY of the
     v3 URL object. It makes NO new product decision: every SETTLED cell is
     transcribed from P1.1; every cell P1.1 left unsettled is flagged OPEN, never
     invented. This is the SINGLE-WRITER record of the canonical field names —
     downstream G3 contracts REFERENCE these names and never redefine them.
     Precedent-setting FORMAT for the other nine G3 contracts: Envelope table,
     Inputs (per-input sha256, tamper-evident), Rows pipe-table, Scope boundaries,
     Open cells. Validated by validate-records.R's canonical-state section, added
     at the cp-snapshot-3 seal (staged spec under
     _scratch/orchestrate/g3-seal-staging/). The envelope stays lifecycle_state
     PROPOSED until that snapshot seals it (registers/contracts seal at the next
     control-plane snapshot, not by an envelope flip). -->

## Envelope

| Field | Value |
|---|---|
| id | contract-canonical-state |
| name | canonical-state-contract |
| artifact_number | 3 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/canonical-state-contract.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the canonical field vocabulary |
| lifecycle_state | PROPOSED |
| dependencies | P1.1 (bound decision); S1 (bound evidence); P2.3 (vocabulary consistency only, not projected); reconciliation §6 artifact 3, §7 G3 |
| bound_decision | P1.1 |
| bound_evidence | S1 |
| closes_finding | RCON-02 |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: every canonical field has a non-empty type/presence/provenance/invariants, a public_projection, a lifecycle, and either a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line note; cross-artifact field names agree; validate-records.R (canonical-state section) passes |
| content_hash | per-input sha256 under `## Inputs` (P1.1, S1), recomputed by validate-records.R |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | validated by validate-records.R's canonical-state section, added at the cp-snapshot-3 seal |

## Purpose

The normative definition of the v3 URL object's canonical state vector. It
instantiates the half of the §6 artifact-3 mandate that P1.1 §Consequences
assigns here — the **canonical-record schema, the three-valued presence model,
per-component provenance, and the public/internal split** — as a field-by-field
matrix. It does not settle verdict semantics (the three layers' enum membership,
fatality, and the π collapse table are §6 artifact 6 / G3.6, per P1.1
§Consequences and P2.3); it names the verdict-layer state fields only so the
vocabulary is complete and consistent.

This record **projects** P1.1 + S1; it makes no product decision. Where P1.1
fixes a cell it is transcribed at `status: SETTLED` with `owner_decision_ref =
P1.1@a7e0a59`. Where P1.1 deferred a cell it is `status: OPEN` with the precise
question and its settlement destination — never filled by invention.

## Inputs

The exact sources this contract projects, hashed at authoring. `validate-records.R`
recomputes each `sha256` on every run; a mismatch fails this record (tamper-evident,
as with `gates/G2-acceptance.md`). The bound decision P1.1 is additionally
hash-enforced as an ACCEPTED record by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P1.1-v3-state-status-model.md | 656510b6d0c16991726ff7b41523712359f60921e1cc472d354f53b498a3cd29 |
| design/work/url-v3/evidence/S1-object-state-vector.md | dfde4d9d0b1bf0933732ce43b4d8dbed2bf191b1aadeb5a238d9330e4d0097be |

## Presence model (verbatim from P1.1 §1.2)

Every delimited component carries an explicit presence state, held **distinctly
from its payload value**, drawn from exactly three states — named here verbatim
as P1.1 §1.2 names them:

- **`absent`** — the component's delimiter is not present.
- **`present-empty`** — the delimiter is present, payload empty.
- **`present-nonempty`** — the delimiter is present, payload non-empty.

`absent` and `present-empty` MUST remain distinguishable in the canonical record
even where a compatibility projection renders both as `NA` (P1.1 §1.2; S1-F1
`foo:///x?#`; S7-F3). The `presence` column below uses these three names. The
shipped structural-kind enums (`.HOST_KIND`, `.AUTHORITY_KIND`, `.PRESENCE_KIND`
in `R/parse-state.R:37-47`) realize this same three-state model with the shipped
labels `{absent, empty, present}`; those labels are the internal spelling of the
`{absent, present-empty, present-nonempty}` model. `presence: n/a` marks fields
that are not delimiter-governed components (identity spellings, classifier flags,
object-level state, projections).

## Rows

One row per canonical field. `provenance`: `source` (immutable input slice),
`parsed` (Stage-A curl/general parse), `derived` (Stage-B presentation derivation),
`classifier` (Stage-A flag), `PSL` (pslr annotation output), `projection`
(collapse/compat output). `public_projection`: `public` = one of the 18
`.spu_result_fields` a caller consumes; `internal` = exists only to drive
parse/derivation (P1.1 §1.4 role criterion); `companion` = surfaced through an
ADR-0006 companion helper, not a parse-frame column. `lifecycle`: how the value
is set across parse→present.

| field | type | presence | provenance | invariants | public_projection | lifecycle | owner_decision_ref | status |
|---|---|---|---|---|---|---|---|---|
| original_url | character | n/a | source | verbatim immutable input slice; retained on EVERY row incl. a failed/invalid element (P1.1 §3.2, S1-F1); no canonical fact may be reconstructible only by reparsing it (S1-F1/F11) | public | captured at input; invariant through parse→present | P1.1@a7e0a59 | SETTLED |
| scheme | character | n/a (scheme-less state via flags) | derived | presentation projection of `final_scheme` under `protocol_handling`; distinct from source slice (P1.1 §1.3) | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| final_scheme | character | n/a | parsed | Stage-A normalized scheme; parse-affecting, cached; feeds `scheme` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| host | character | absent / present-empty / present-nonempty | derived | presentation host (post-www, `host_encoding` rendering of `final_host`); public NA collapses empty+absent but the canonical record keeps them distinct via `host_kind` (P1.1 §1.2) | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| final_host | character | absent / present-empty / present-nonempty | parsed | Stage-A post-www host; parse-affecting, cached; feeds `host` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| host_kind | enum {absent, empty, present} | absent / present-empty / present-nonempty | classifier | realizes the §1.2 three-valued presence for host; empty MUST stay distinct from absent (S1-F1) | internal | Stage-A | P1.1@a7e0a59 | SETTLED |
| is_ip_host | logical | n/a | classifier | consumed fact; deliberately spans public field AND Stage-A flag and stays public (P1.1 §1.4) | public | Stage-A (cached), surfaced Stage-B | P1.1@a7e0a59 | SETTLED |
| host_is_ace | logical | n/a | classifier | host was an A-label (ACE); Stage-A only | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| whatwg_host_form | enum {domain, opaque, ipv4, ipv6, empty} | n/a | classifier | WHATWG-posture host form (S1 structural kind, `R/parse-state.R:52`); reg-name/opaque split deferred to derivation | internal | Stage-A/B | P1.1@a7e0a59 | SETTLED |
| rfc_host_form | enum {reg-name, ipv4, ipv6, ipvfuture, empty} | n/a | classifier | RFC-posture host form (S1 structural kind, `R/parse-state.R:57`) | internal | Stage-A/B | P1.1@a7e0a59 | SETTLED |
| authority_kind | enum {absent, empty, present} | absent / present-empty / present-nonempty | classifier | whether `//` authority was present, and if so whether it carried a host; the **`empty` value's operational meaning is unsettled** (S1-F5) — see Open cells | internal | Stage-A | — (deferred; see Open cells) | OPEN |
| user | character | absent / present-empty / present-nonempty | parsed | parsed credential; distinct from source slice `raw_user` (P1.1 §1.3) | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| password | character | absent / present-empty / present-nonempty | parsed | parsed credential; distinct from source slice `raw_password` | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| raw_user | character | absent / present-empty / present-nonempty | source | Stage-A immutable user slice; feeds `user` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| raw_password | character | absent / present-empty / present-nonempty | source | Stage-A immutable password slice; feeds `password` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| scheme_less_userinfo | logical | n/a | classifier | scheme-less userinfo-shaped input; drives the L2 `warn-userinfo` policy verdict and NA `clean_url` (P2.3 §1) | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| port | integer | absent / present-empty / present-nonempty | parsed | presentation port under `port_handling`; parsed from `raw_port`; distinct from source | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| raw_port | integer | absent / present-empty / present-nonempty | source | Stage-A port slice; feeds `port` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| path | character | absent / present-empty / present-nonempty (where applicable) | derived | Stage-B path under `path_normalization`/`path_encoding`; the public scalar is a PROJECTION, not the canonical segment list (S1-F7); identity distinct from display (P1.1 §1.3, ADR 0011) | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| raw_path | character | absent / present-empty / present-nonempty | source | Stage-A immutable path slice; feeds `path` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| path_kind | enum {list, opaque} | n/a | classifier | WHATWG opaque/list discriminator (`.PATH_KIND`); RFC posture never inherits it | internal | Stage-A | P1.1@a7e0a59 | SETTLED |
| rfc_path_form | enum {abempty, absolute, rootless, empty} | n/a | classifier | RFC 3986 §3.3 path form (`.RFC_PATH_FORM`); carried instead of `path_kind` in RFC posture | internal | Stage-A | P1.1@a7e0a59 | SETTLED |
| rfc3986_path_rootless | logical | n/a | classifier | RFC scheme + rootless-path row (`http:example.com`); parseable but hostless — must not be demoted to error for lacking an authority | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| query | character | absent / present-empty / present-nonempty | derived | ends the empty-vs-absent collapse (P1.1 §1.2); public NA is the compat projection; distinct from `raw_query` | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| raw_query | character | absent / present-empty / present-nonempty | source | Stage-A immutable query slice; feeds `query`; mutation-grade token structure is out of scope (S1-F6, query contract) | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| query_kind | enum {absent, empty, present} | absent / present-empty / present-nonempty | classifier | delimiter presence for query; `empty` (`?`) distinct from `absent` so serializers can retain a trailing `?` | internal | Stage-A | P1.1@a7e0a59 | SETTLED |
| fragment | character | absent / present-empty / present-nonempty | derived | ends the empty-vs-absent collapse (P1.1 §1.2); public NA is the compat projection | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| raw_fragment | character | absent / present-empty / present-nonempty | source | Stage-A immutable fragment slice; feeds `fragment` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| fragment_kind | enum {absent, empty, present} | absent / present-empty / present-nonempty | classifier | delimiter presence for fragment; `empty` (`#`) distinct from `absent` | internal | Stage-A | P1.1@a7e0a59 | SETTLED |
| domain | character | n/a (follows host + annotation state) | PSL | registrable domain in the `host_encoding` spelling (a rendering choice); an L3 annotation output, never allowed to alter L1/L2 (P1.1 §2) | public | Stage-B (PSL) | P1.1@a7e0a59 | SETTLED |
| tld | character | n/a | PSL | public suffix in the `host_encoding` spelling; L3 annotation output | public | Stage-B (PSL) | P1.1@a7e0a59 | SETTLED |
| domain_ascii | character | n/a | PSL | encoding-independent registrable-domain identity key (A-label spelling); stable across `host_encoding` | public | Stage-A (cached, PSL) | P1.1@a7e0a59 | SETTLED |
| domain_unicode | character | n/a | PSL | encoding-independent registrable-domain identity key (U-label spelling) | public | Stage-A (cached, PSL) | P1.1@a7e0a59 | SETTLED |
| tld_ascii | character | n/a | PSL | encoding-independent public-suffix identity key (A-label) | public | Stage-A (cached, PSL) | P1.1@a7e0a59 | SETTLED |
| tld_unicode | character | n/a | PSL | encoding-independent public-suffix identity key (U-label) | public | Stage-A (cached, PSL) | P1.1@a7e0a59 | SETTLED |
| clean_url | character | n/a | projection (cleaning) | intentionally lossy SEO/display serialization; a distinct cleaning-projection form (P1.1 §1.3), NOT the standards serialization and NOT an identity key | public | Stage-B | P1.1@a7e0a59 | SETTLED |
| parse_status | enum (8 shipped values) | n/a | projection | the total, pure π(L1,L2,L3) compat view and the SOLE locus a PSL fact reaches a status output (P1.1 §4); byte-identical compat (P2.3 §5); loses the L1-fail vs L2-reject distinction by design | public (compat projection) | Stage-B (post-verdicts) | P1.1@a7e0a59 | SETTLED |
| layer1_syntax_verdict | enum (membership → G3.6) | n/a | derived | pure standard-parse outcome; computed FIRST; invariant under policy/annotation (P1.1 §2). Layer + independence fixed by P1.1; enum membership/fatality are G3.6 (P2.3 §1) | companion (`get_parse_verdicts()`) | Stage-A→verdict | P1.1@a7e0a59 | SETTLED |
| layer2_policy_verdict | enum (membership → G3.6) | n/a | derived | acceptance/admission outcome layered OVER a parsed URL; cannot be altered by L3 (P1.1 §2) | companion (`get_parse_verdicts()`) | Stage-A/B→verdict | P1.1@a7e0a59 | SETTLED |
| layer3_annotation_state | enum {not-requested, not-applicable, known, unknown, invalid-input, dependency-error} | n/a | PSL | optional typed annotation state (S7-F3); MUST NOT alter L1/L2 and MUST NOT be a bare NA (P1.1 §2). Typed-state set fixed by P1.1; resolution rules are G3.6 (P2.3 §2) | companion (`get_parse_verdicts()` L3, opt-in) | Stage-B (PSL)→verdict | P1.1@a7e0a59 | SETTLED |
| looks_like_protocol | logical | n/a | classifier | token looks like a scheme; feeds the L2 web-unsupported-scheme demotion | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| original_has_allowed_scheme | logical | n/a | classifier | input bore an allowed scheme; feeds L2 admission | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| is_scheme_relative | logical | n/a | classifier | scheme-relative input; feeds L2 `admitted-scheme-relative` | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |
| looks_like_host_port | logical | n/a | classifier | `host:port` form; excludes the row from unsupported-scheme demotion | internal | Stage-A (cached) | P1.1@a7e0a59 | SETTLED |

## Scope boundaries

This contract is the **single writer** of the canonical FIELD VOCABULARY — the
field names, their types, their three-valued presence, their provenance form,
their public/internal/companion role, and the SETTLED/OPEN status of each. Every
downstream G3 contract **references** these field names and never redefines them:

- **Verdicts that consume this state** — the three verdict layers' concrete enum
  membership, fatality classification, the π collapse table, and the
  annotation-state resolution rules — are **§6 artifact 6 / G3.6** (P1.1
  §Consequences; P2.3). This contract names `layer1_syntax_verdict`,
  `layer2_policy_verdict`, `layer3_annotation_state`, and `parse_status` as
  canonical fields and fixes the §2 layer-independence invariant; it does **not**
  enumerate their values.
- **Output surfaces that consume this state** (source reproduction, standards
  serialization, cleaning, safe display, comparison keys) — **G3.7**. `clean_url`
  and `path`/`host` presentation projections are named here; their equivalence and
  loss-budget contracts live in G3.7.
- **Keys / joins** built over these fields — **G3.K** (key/join contracts). The
  `*_ascii`/`*_unicode` identity keys are named here; key policy is G3.K's.
- **Cache participation and host/annotation contracts** — **G3.9 / G3.H**. The
  Stage-A/Stage-B `lifecycle` column states each field's ownership; the cache-key
  membership and host/PSL cost/failure model are those contracts'.
- **Scalar/vector shape** (container, coercion, row-local vs call-level failure)
  is the S1 scalar/vector contract, not this artifact; this contract fixes only
  that a failed element is a typed invalid canonical record carrying
  `original_url` + the layered verdicts (P1.1 §3.2).

## Open cells

Cells P1.1 did not settle. Not filled by invention (per §7 G3: unsettled product
cells carry an explicit owner-decision destination).

- **`authority_kind` — the `empty` value's operational meaning (S1-F5).** P1.1
  Q3 **DEFERRED** the authority presence vocabulary: it "stays scoped to C-07 and
  adopts the general three-valued presence model only," sending the specific
  `authority_kind` contradiction to "a dedicated P1 authority-state decision."
  S1-F5 records the live contradiction: `.AUTHORITY_KIND` declares all three
  values but `.authority_kind()` emits only absent/present (calling `empty`
  "reserved"), the general parser makes every `//` row authority-present, and the
  RFC `file:` overlay emits authority-empty for the same shape — so `foo:///bar`
  and `file:///bar` disagree. **Question:** what does `authority_kind = empty`
  mean operationally, or should authority state split into
  `authority_delimiter_present: logical` + `authority_payload_kind: {empty,
  present}`? **Settles at:** the deferred sibling **P1 authority-state decision**;
  this contract adopts that record's vocabulary once accepted.

- **Undivided `userinfo` source component vs the `user`/`password` split (S1 Q5).**
  P1.1 retains the shipped `user`/`password` (and `raw_user`/`raw_password`)
  fields and does **not** decide whether the canonical vector also adds an
  undivided source-preserving `userinfo` component, nor which credential states
  are public. The named `user`/`password` fields are SETTLED; only the *augmenting*
  question is open. **Settles at:** the credential handling clause of the output /
  serialization contract (**G3.7**), driven by S1 owner-question Q5.
