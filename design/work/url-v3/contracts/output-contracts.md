# Output contracts (§6 artifact 7)

<!-- Contract artifact (§6 artifact 7). RCON-03 closure. This record PROJECTS the
     ACCEPTED owner decisions P2.2 (the four output surfaces + the full-string
     standard-serialization surface FSSS + surface-assignment invariants; C-04/
     C-05/C-12-serialization dispositions), P5.3 (the claim/oracle policy over the
     FSSS; C-12 claim half), and P3.1 (the comparison-key surface — REFERENCED,
     not redefined) plus frozen evidence S3 (serialization/format/source/encoding)
     into the normative five-output-surface, surface-assignment, serializer-input-
     record, capability-classification, and encoding-invariance matrices. It makes
     NO new product decision: every SETTLED cell is transcribed from an accepted
     record; a cell those records left as an owner open question / deferral is
     flagged OPEN with impact + destination, never invented. This is the SINGLE
     WRITER of the v3 output-surface matrices. SCOPE SPLIT: the comparison-key
     surface (e) REFERENCES the G3.K key surface (get_url_key / url_key_policy /
     joins) and does not redefine it; the cleaning DIALS are G3.8's; the intervention
     ledger + verdicts are G3.6's; the oracle REGISTER + numeric budgets are §6
     artifact 11 / G4 / P5.1 / P5.2; host/PSL/IDNA vocabulary is G3.H's. Canonical
     FIELD NAMES come from artifact 3. This artifact OWNS the credential-handling /
     undivided-userinfo cell deferred from G3.3 (S1 Q5). Format follows the
     G3.3/G3.5/G3.6/G3.8/G3.K precedent: Envelope, tamper-evident Inputs, pipe-table
     Rows, Scope boundaries, Open cells. The envelope stays lifecycle_state PROPOSED
     until the cp-snapshot-3 seal; validator coverage and the manifest present-flip
     ride that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-output |
| name | output-contracts |
| artifact_number | 7 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/output-contracts.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the v3 output-surface matrices: the five output surfaces (source reproduction, standard serialization, clean output, safe display, comparison key), the surface-assignment invariants, the lossless serializer-input record, the claim/oracle policy over the FSSS, the capability classification, the encoding/locale-invariance rows, and the credential-handling / undivided-userinfo cell (S1 Q5) |
| lifecycle_state | PROPOSED |
| dependencies | P2.2 (bound decision — four surfaces + FSSS + invariants); P5.3 (bound decision — claim/oracle policy over the FSSS); P3.1 (bound decision — comparison-key surface, REFERENCED not redefined); S3 (bound evidence); contract-canonical-state (field vocabulary only, not projected); P2.1/P2.3 (repair posture / verdict layering — referenced); reconciliation §6 artifact 7, §4 RCON-03, §7 G3 |
| bound_decision | P2.2 + P5.3 + P3.1 |
| bound_evidence | S3 |
| closes_finding | RCON-03 |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: the five output surfaces, surface-assignment invariants, the standard-serialization (FSSS) contract, the lossless serializer-input record, source reproduction, clean output, safe display, the comparison-key reference, the claim/oracle policy, the capability classification, encoding/locale invariance, and the credential-handling / undivided-userinfo cell each carry a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line impact and owner-decision destination; cross-artifact field names agree with artifact 3; the key surface references G3.K without redefinition; validate-records.R (output section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (P2.2, P5.3, P3.1, S3), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | output validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative v3 contract that ends RCON-03's conflation of source reproduction,
standards serialization, cleaning, and safe display into one `clean_url` surface.
It fixes the **five** distinct output surfaces (P2.2's four plus the comparison
key owned by P3.1/G3.K), each with an input-state requirement, exact output,
fragment/credential policy, reparse guarantee, and allowed downstream uses; the
**surface-assignment invariants** that forbid one surface standing in for another;
the **lossless serializer-input record** the standard serializer consumes (S3-F2/
F5); the **claim/oracle policy** that binds every conformance claim to the
full-string standard-serialization surface (P5.3); the **capability
classification** of each produced string; and the **encoding/locale-invariance**
rules (S3-F7).

This record **projects** accepted P2.2 + P5.3 + P3.1 and S3; it does not implement
these surfaces and makes no product decision. A `SETTLED` row cites `P2.2@8292c7f`,
`P5.3@8292c7f`, or `P3.1@3b89b94`. A cell an accepted record left open or deferred
is `OPEN` with its exact impact and settlement destination. Notably, **P2.2 and
P5.3 each carry unresolved owner open-questions** (no ratification block resolved
them at acceptance); those surface here as explicit OPEN cells.

## Inputs

The exact sources this contract projects, hashed at authoring.
`validate-records.R` recomputes all hashes at the cp-snapshot-3 seal. P2.2, P5.3,
and P3.1 are already hash-enforced as ACCEPTED decisions by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P2.2-serializer-clean-output.md | 01542610a8aaa7f0c1abdbfaae373f65d220446b4f8ae8e6f29cc721cd263c9a |
| design/work/url-v3/decisions/P5.3-oracle-claim-policy.md | 3ca407467ed17f8ad34fc8fb46dda79f7465bb0631bc81bd20f2d490036d1c82 |
| design/work/url-v3/decisions/P3.1-identity-canonical-join.md | b1760889be4cf0d756cab48928211186a8395e5197b851cf692fdc9db919ad4e |
| design/work/url-v3/evidence/S3-serialization-format-encoding.md | 3c2e1170b62fcb4a1b77a9e52aa494fcd6ed9d369cbd6c15c7271fc15ff1e980 |

## Five output surfaces

RCON-03's conflation is separated into five surfaces. Four are P2.2's (§1); the
fifth — the comparison key — is P3.1's (owned by G3.K), listed to keep the
boundary complete. No surface may silently stand in for another.

| # | surface | v3 name | fragment | credentials | round-trips to source | spec-exact | owner_decision_ref | status |
|---|---|---|---|---|---|---|---|---|
| a | source reproduction | `url_source()` (← `original_url`) | verbatim | verbatim | yes (bytes, unmutated row) | n/a (echoes input) | P2.2@8292c7f (§1a) | SETTLED |
| b | standard serialization (FSSS) | `serialize_url(x, standard=)` (unimplemented) | **preserved** | **reconstructed** | via parse→serialize→parse identity | **yes** (WHATWG / RFC 3986) | P2.2@8292c7f (§1b, §4) | SETTLED |
| c | clean output | `clean_url` / `get_clean_url()` | **omitted** | **not reconstructed** | no (intentionally lossy) | no (SEO/policy product) | P2.2@8292c7f (§1c) | SETTLED |
| d | safe display | `format_url()` (unimplemented) | policy | redacted/annotated | no | no (human-readable) | P2.2@8292c7f (§1d) | SETTLED (surface; matrix OPEN — OUT-O4) |
| e | comparison key | `get_url_key()` (non-URL projection) | ignored (state kept for diagnostics) | ignored | n/a | n/a (identity, not a URL string) | P3.1@3b89b94 (D-A) — **owned by G3.K**, referenced | SETTLED (boundary) |

## Surface-assignment invariants (P2.2 §5)

The binding rules that keep the five surfaces distinct. Transcribed verbatim.

| # | invariant | owner_decision_ref | status |
|---|---|---|---|
| 1 | **no surface substitutes for another** — clean (c) is never a serializer, redirect target, comparison identity, or conformance oracle; standard (b) is never a human display; display (d) output never re-enters serialization/mutation/keys | P2.2@8292c7f (§5.1); S3-F3 | SETTLED |
| 2 | **standard serializers consume the lossless serializer-input record**, never the 18-field public projection or a cleaned/formatted component | P2.2@8292c7f (§5.2); S3-F2 | SETTLED |
| 3 | **presentation dials are clean/display-only** — `path_encoding`, `host_encoding`, `query_handling` filtering/sorting, and the cleaning dials feed (c)/(d) only, never (b) | P2.2@8292c7f (§5.3); ADR 0011/0012 | SETTLED |
| 4 | **fragment and credentials are serialized by (b), dropped by (c)** — a per-surface contract, not a global URL policy | P2.2@8292c7f (§5.4) | SETTLED |
| 5 | **credential-output policy governs all preserved credentials** — (c) never reconstructs credentials, (d) redacts, only (a)/(b) may reproduce them, for web AND generic-authority credentials alike (the P4.1-B9 secret-in-output guard) | P2.2@8292c7f (§5.5); P4.1 (B9) | SETTLED |

## Standard serialization — the FSSS (surface b)

The full-string standard-serialization surface that conformance is rebuilt from
(P2.2 §1b/§4). It disposes C-04 and C-05.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| pair of spec-exact serializers | one per standard — WHATWG (`#concept-url-serializer`) and RFC 3986 (§5.3 component recomposition) — emitting the standard's own serialization of the parsed state | P2.2@8292c7f (§1b) | SETTLED |
| full-string, fragment- + credential-complete | emits userinfo (with delimiter state), a trailing `?`/`#` when the delimiter was present with an empty value (ADR 0012 D2), and the fragment | P2.2@8292c7f (§1b); ADR 0012 D2 | SETTLED |
| consumes the lossless record | consumes the lossless serializer-input record (below), never the 18-field projection and never a cleaned/formatted component | P2.2@8292c7f (§1b, §5.2); S3-F2 | SETTLED |
| correctness oracle | parse→serialize→parse structural equivalence plus full-string fixtures — the substrate P5.3's claims stand on | P2.2@8292c7f (§4); P5.3@8292c7f (§1) | SETTLED |
| C-04 — fragment preservation | **SEPARATE / UNIMPLEMENTED, not superseded**: the shipped `.serialize_*_vec()` are the *clean* serializers (surface c) implementing the `clean_url` contract; the ADR 0012 D2 standard serializer preserves fragments/credentials and is scheduled (Layer 3b), not built | P2.2@8292c7f (§2); ADR 0012 D2 | SETTLED |
| C-05 — `%2F` / `path_encoding` | **compat retained in clean surface, excluded from (b)**: `path_encoding` is a clean/display presentation dial only; surface (b) renders the path from the identity record per the selected standard's own percent-encode set and **never consults `path_encoding`** | P2.2@8292c7f (§3); ADR 0011 | SETTLED |
| parse posture | (b) serializes the SELECTED standard's own parse: profile `whatwg` / `rfc-syntax`, both `scheme_acceptance = "general"` + `scheme_policy = "require"`. Any scheme is accepted; a scheme is REQUIRED — rurl's `https://` prepend is browser-like fix-up and belongs to surface (c), so scheme-less input is `NA`, never an assertion of conformance for a string the standard rejects | P2.5 (PROPOSED) (§3) | SETTLED |
| host spelling | (b) emits WHATWG's parsed host, the ASCII (punycode) domain; surface (c) keeps the Unicode spelling (`host_encoding = "keep"`, a readability choice). Default-port elision is likewise a PARSE fact under WHATWG, not a serializer policy | P2.5 (PROPOSED) (§4) | SETTLED |
| build dependency | surface (b) requires the lossless serializer-input record (S3-F2 → RCON-02 canonical-state, artifact 3) first, so it cannot be implemented before the state contract lands — this record fixes the contract + name, not the implementation | P2.2@8292c7f (Consequences) | SETTLED |
| public entry-point name | `serialize_url(x, standard = c("whatwg", "rfc3986"), form = c("source", "normalized"))` — one export, the standard as a VALUE (ADR 0007 selector idiom); rejected: a `standard=` arg on an existing accessor (accessors return components, (b) returns the whole string) and a split `serialize_whatwg()`/`serialize_rfc()` pair | P2.5 (PROPOSED) (§1) | SETTLED |
| RFC serializer posture | **both**, as `form=`: `"source"` (§5.3 recomposition, no normalization, undivided `userinfo` verbatim — the round-trip oracle) and `"normalized"` (§6.2.2 syntax-based normalization + §6.2.3 default-port elision — the comparison substrate). Choosing one forfeits the other; `form` is a standard-selector, not a presentation dial (C-05 intact) | P2.5 (PROPOSED) (§2) | SETTLED |

## Lossless serializer-input record (S3-F2 / S3-F5)

The mandated artifact surface (b) consumes (P2.2 §1b names it; S3-F2/F5 enumerate
it). Field *names* are the canonical vocabulary of artifact 3; this contract fixes
that the record is credential/delimiter/query-lexically **complete**, so
fragment/credential/empty-delimiter serialization is reconstructable.

| record element | contract | owner_decision_ref | status |
|---|---|---|---|
| authority + host state | `authority_delimiter_present` + `authority_payload_kind` (P1.2, artifact 3) and independent `host_kind` / host form, distinct spellings (source/parsed/canonical) | P2.2@8292c7f (§1b); artifact 3 (P1.2) | SETTLED |
| credential/delimiter state | `user`, `password`, `raw_user`/`raw_password` (source slices), and the delimiter facts (`userinfo`/password-delimiter presence) needed to reassemble `u@`, `u:@`, `:p@`, `@`, `u:p:q@` losslessly (S3-F5) | P2.2@8292c7f (§1b, §5.4); S3-F5 | SETTLED |
| query lexical state | the lossless query state (delimiter presence, bare-key vs equals-present, empty pairs, order, duplicates, literal `+`, percent-triplet case, malformed `%`) — standards/source serialization use this, never the cleaning query-pair view (S3-F4) | P2.2@8292c7f (§5.2); S3-F4 | SETTLED |
| query/fragment presence | `query_kind` / `fragment_kind` three-valued presence, so a trailing empty `?`/`#` is preserved by (b) (ADR 0012 D2) | P2.2@8292c7f (§1b); ADR 0012 D2; artifact 3 | SETTLED |
| path posture | posture-specific path identity (`path_kind` / `rfc_path_form`), source/parsed payload distinct from presentation | P2.2@8292c7f (§1b); artifact 3 | SETTLED |
| parse/repair provenance | selected standard + parse/repair provenance carried on the record (so a repaired candidate's serialization is attributable — G3.6 owns the ledger) | P2.2@8292c7f (§1b); P2.1 (ledger, via G3.6) | SETTLED (provenance ledger → G3.6) |

## Source reproduction (surface a)

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| own surface, never a serializer | faithful reproduction of the accepted input for an unmutated row (`original_url` today); source reproduction is its own surface, distinct from standard serialization (S3-F1) | P2.2@8292c7f (§1a); S3-F1 | SETTLED |
| byte vs `Encoding()`-label guarantee | whether v3 promises source-byte reproduction, the R `Encoding()` label, both, or a bounded subset (byte preservation and `Encoding()`-label preservation are already different guarantees) is **not** resolved (P2.2 Q4; S3-C7/S3-F1) | — (see Open cells OUT-O5) | OPEN |

## Clean output (surface c)

The clean surface's per-dial semantics are **G3.8's**; this artifact records only
its output-surface classification. Consistent with G3.8.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| intentionally lossy SEO product | `clean_url` / `get_clean_url()` is a policy-driven SEO/canonicalization product; **not** a serializer, identity, redirect target, or conformance oracle | P2.2@8292c7f (§1c, §5.1) | SETTLED |
| omits fragment + credentials | drops the fragment and never reconstructs credentials (per-surface contract) | P2.2@8292c7f (§1c, §5.4) | SETTLED |
| byte-compat scope | byte-for-byte compatible for every input that remains admitted under the active posture (D4); the strict-default flip is an admission change, not a clean-serialization change | P2.2@8292c7f (§1c, D4) | SETTLED |
| dials owned elsewhere | the 25 cleaning dials, their order, lossiness, and non-interference are **G3.8** — referenced, not redefined | P2.2@8292c7f (§5.3) — dials owned by **G3.8** | SETTLED (boundary) |
| `resolve_url()` coupling | `resolve_url()` currently returns the clean `clean_url` (dropping userinfo/fragment); decoupling its public return from cleaning is the RCON-03 resolve-output-shape question, tied to the G3.6 resolution verdict-layering (VAL-O3) | — (see Open cells OUT-O4 note; resolve output shape → RCON-03) | OPEN (see OUT-O4) |

## Safe display (surface d)

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| separate surface | a human-readable rendering with hazard handling (controls, bidi overrides, invisibles, spoof-safe host display); its output is **never** fed into serialization, mutation baselines, or keys | P2.2@8292c7f (§1d, §5.1); S3-F3 | SETTLED |
| redacts credentials | `format_url()` redacts credentials (the §5.5 secret-in-output guard) | P2.2@8292c7f (§5.5) | SETTLED |
| scope + escape/annotation matrix | whether `format_url()` is in scope for 3.0 and its component-by-component escape/annotation/host-display matrix (S3-F3; S3-C6) are **not** resolved (P2.2 Q3) | — (see Open cells OUT-O4) | OPEN |

## Comparison key (surface e) — reference to G3.K

The comparison key is a **first-class non-URL projection of the canonical
identity state, independent of cleaning/display** (P3.1 D-A). It is **owned by
G3.K / P3.1**; this artifact references it and does **not** redefine it.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| identity, never presentation | `get_url_key()` is derived from the canonical identity state *after* standard interpretation but *before* any cleaning/display transform; never from `clean_url` or any presentation string | P3.1@3b89b94 (D-A) — owned by **G3.K** | SETTLED (boundary) |
| non-interference | changing any cleaning/display dial leaves the key byte-identical (the invariant this artifact's surfaces rely on to keep (c)/(d) out of identity) | P3.1@3b89b94 (D-A.3) — owned by **G3.K / G3.8** | SETTLED (boundary) |
| key policy, truth tables, joins, migration | the versioned `url_key_policy`, scheme/port truth tables, six `url_*_join()` operations, and the `canonical_join()` migration are **G3.K's** single-writer territory, not redefined here | P3.1@3b89b94 (D-B..D-E) — owned by **G3.K** | SETTLED (boundary) |

## Claim / oracle policy over the FSSS (P5.3)

Every conformance claim stands on surface (b), never the projection. The
oracle *register* + numeric budgets are §6 artifact 11 / G4 / P5.1 / P5.2; this
artifact records the claim/oracle *policy* that governs output claims.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| claims against the FSSS only | a v3 conformance claim is scoped to one named standard + one labeled oracle, asserted against **exactly two substrates**: the FSSS full-string output and parse→serialize→parse structural equivalence — never any other | P5.3@8292c7f (§1.1) | SETTLED |
| the projection may not be an oracle | no conformance claim may be asserted against the 18-field component projection (it collapses empty-vs-absent, drops delimiter state, is shaped by presentation dials); projection equality is characterization/regression only, never a standards or state-preservation gate (S1-F10) | P5.3@8292c7f (§1.2); S1-F10 | SETTLED |
| historical projection claims retained-but-retired | pre-v3 projection-based claims are kept unaltered as historical evidence and retired as live oracles; a live claim reusing such a fixture re-derives its expected value from an independent oracle against the FSSS | P5.3@8292c7f (§1.3) | SETTLED |
| parity ≠ conformance | `libcurl-parity` / `browser-parity` produce agreement *ledgers*, reported separately; agreement is never conformance evidence | P5.3@8292c7f (§2.1) | SETTLED |
| labeled-oracle taxonomy | every oracle carries an authority label + explicit claim boundary (`whatwg-wpt`, `rfc3986-grammar`, `libcurl-parity`, `browser-parity`, `self-metamorphic`); the defensible headline is a labeled *N conforming / M documented deviations against `<authority>` `<standard>`* count, never a "100%" parity probe | P5.3@8292c7f (§2) | SETTLED |
| metamorphic assertions required | the `self-metamorphic` oracle must assert parse→serialize→parse equivalence, format non-authority (display never re-enters serialization), clean loss-budget, mutation preservation, scalar/vector parity, and cache transparency | P5.3@8292c7f (§3.3); S1-F10 | SETTLED |
| oracle register + budgets | the per-row oracle *register*, the closed-vs-open taxonomy refinement, headline-number governance, deviation lifecycle, and numeric budgets are **§6 artifact 11 / G4** (+ P5.1 cache/perf, P5.2 determinism) — P5.3's own Q2-Q5 defer there | P5.3@8292c7f (§Affected, Q2-Q5) — owned by **§6 artifact 11 / G4** | SETTLED (boundary) |

## Capability classification (S3-F6/F13)

Every produced string/key declares its safe uses; a display-friendly value may
never be mistaken for identity/routing/redirect-safe.

| produced value | reparsable | standards-valid | identity/comparison-safe | routing/redirect/join-safe | display-only | owner_decision_ref | status |
|---|---|---|---|---|---|---|---|
| `url_source()` (a) | yes (echoes input) | n/a (may echo non-conformant input) | no | no | yes | P2.2@8292c7f (§1a) | SETTLED |
| `serialize_url()` (b) | **yes** (parse→serialize→parse) | **yes** | n/a (identity is the key) | via re-parse only | no | P2.2@8292c7f (§1b); P5.3@8292c7f (§1) | SETTLED |
| `clean_url` (c) | no (lossy) | no | **no** | **no** (legacy `canonical_join` keying is LEGACY-to-migrate) | **yes** | P2.2@8292c7f (§1c, §5.1); P3.1@3b89b94 (D-E) | SETTLED |
| `format_url()` (d) | **no** (never re-enters serialization) | no | no | no | **yes** | P2.2@8292c7f (§1d, §5.1); S3-F3 | SETTLED |
| `get_url_key()` (e) | n/a (non-URL) | n/a | **yes** (the identity surface) | yes | no | P3.1@3b89b94 (D-A) — owned by **G3.K** | SETTLED (boundary) |

## Encoding / locale-invariance rows (S3-F7)

The shipped R output-encoding contract that v3 separation must preserve.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| UTF-8 marking | non-ASCII returned character components are declared UTF-8 (`.mark_result_utf8()`); pure-ASCII outputs remain `Encoding() == "unknown"` (R does not retain a mark on ASCII) | S3-F7; P2.2@8292c7f (§1a scope) | SETTLED (invariant to preserve) |
| locale invariance | returned byte values are locale-invariant; `serialize`/`format` fixtures must assert exact `charToRaw()` / `Encoding()` for non-ASCII outputs and the ASCII-mark rule | S3-F7 | SETTLED (invariant to preserve) |
| raw-byte vs percent-spelling distinction | literal `é` (bytes `C3 A9`, UTF-8-marked), `%C3%A9` (ASCII until decoded), and `%FF` (ASCII source bytes denoting a non-UTF-8 octet) are distinct and require distinct rules; the exact per-surface `Encoding()`/invalid-sequence policy detail is fixture-family work for G4 | S3-F7 | SETTLED (boundary — fixture detail → G4) |

## Credential handling / undivided-userinfo (owns S1 Q5, deferred from G3.3)

G3.3 deferred to this artifact the question of whether the canonical vector adds
an **undivided source-preserving `userinfo` component** beyond the shipped
`user`/`password` split, and which credential states are public (S1 Q5). This
artifact is its home. The **internal** serializer-input completeness is SETTLED;
the **public** component addition is OPEN.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| `user`/`password` split retained | the shipped `user`/`password` (+ `raw_user`/`raw_password`) fields stay; artifact 3 names them SETTLED | P2.2@8292c7f (§5.4); artifact 3 (P1.1) | SETTLED |
| internal reassembly completeness | the lossless serializer-input record carries the delimiter/`userinfo`-kind state needed to reassemble every credential spelling losslessly (S3-F5); surface (b) is credential-complete | P2.2@8292c7f (§1b, §5.4); S3-F5 | SETTLED |
| output governance | credentials are reproduced only by (a)/(b), dropped by (c), redacted by (d) — for web and generic-authority credentials alike | P2.2@8292c7f (§5.5); P4.1 (B9) | SETTLED |
| public undivided-`userinfo` component | whether v3 adds a public undivided source-preserving `userinfo` component (beyond `user`/`password`) and which credential states are public is **not** decided by P2.2/P3.1 | — (see Open cells OUT-O1) | OPEN |

## Scope boundaries

This contract owns the **output-surface** matrices. It does **not** define, and
must not be read as redefining:

- **The comparison-key surface (e)** — `get_url_key()`, `url_key_policy()`, the
  scheme/port truth tables, the six `url_*_join()` operations, and the
  `canonical_join()` migration are **G3.K / P3.1**. This artifact references the
  key as surface (e) and its non-interference guarantee; it redefines nothing.
- **The cleaning dials (surface c internals)** — the 25 dials, their processing
  order, lossiness, and eligibility are **G3.8**. This artifact records only
  surface (c)'s output classification.
- **The repair-posture pipeline, intervention ledger, and verdict layers** —
  **G3.6 / P2.1 / P2.3**. The serializer-input record's parse/repair provenance
  points to the ledger owned there; the `resolve_url` verdict layering is G3.6.
- **The oracle register + numeric budgets** — the per-row oracle register and
  provenance data (§6 artifact 11), the G4 executable-evidence gate, and the
  P5.1 cache/perf + P5.2 determinism budgets. This artifact records the claim/
  oracle *policy* (P5.3) over output, not the register or the budgets.
- **Canonical-state field vocabulary** (`authority_delimiter_present`,
  `authority_payload_kind`, `host_kind`, `query_kind`, `fragment_kind`,
  `user`/`password`, `path_kind`/`rfc_path_form`, the `*_ascii`/`*_unicode`
  identity keys) — consumed from artifact 3 without renaming.
- **Host / IDNA / PSL / DNS / IP vocabulary** — **G3.H**; the spoof-safe
  host-display *policy* named for surface (d) defers its host-form detail there.
- **Scheme admission / interpretation / eligibility** — **G3.5 / P2.4 / P4.1**.
- **Documentation hygiene** — correcting README / `man/get_clean_url.Rd` /
  `ARCHITECTURE.md` / `resolve_url` docs that call `clean_url` a "canonical key"
  or a serializer (S3-C4/C5; P3.1 Invalidated-artifacts) is a parked
  implementation-slice housekeeping item, not a cell of this contract.

## Open cells

Each cell an accepted record left open or deferred is recorded rather than
invented. None reopens a SETTLED default.

- **OUT-O1 — public undivided-`userinfo` component (S1 Q5, deferred from G3.3).**
  Whether the canonical vector adds a public undivided source-preserving
  `userinfo` component beyond the SETTLED `user`/`password` split, and which
  credential states are public, is not decided by P2.2/P3.1. The **internal**
  serializer-input reassembly completeness is SETTLED (S3-F5); only the **public**
  component addition is open. **Impact:** a caller cannot read the undivided
  source userinfo spelling as a public field; only `user`/`password` (empty-
  collapsed) are exposed. **Settles at:** a credential-accessor / public-surface
  owner decision (coordinated with G3.5 SCHEME-O2 `get_password` selector parity),
  driven by S1 Q5.
- **OUT-O4 — safe-display (surface d) scope + escape/annotation matrix, and the
  `resolve_url` output shape (P2.2 Q3; S3-F3/S3-C6; RCON-03 resolve half).**
  Whether `format_url()` is in scope for 3.0, its component-by-component
  escape/annotation/host-display matrix, and whether `resolve_url()` decouples its
  public return from `clean_url` are unresolved. **Impact:** safe display has a
  fixed surface boundary but no output matrix; `resolve_url` still returns the
  lossy clean string. **Settles at:** a dedicated safe-display P-tier record (S3-F3)
  and the RCON-03 `resolve_url` output-shape record (paired with G3.6 VAL-O3).
- **OUT-O5 — source-reproduction (surface a) guarantee (P2.2 Q4; S3-C7/S3-F1).**
  Whether v3 promises source-byte reproduction, the R `Encoding()` label, both, or
  a bounded subset is unresolved. **Impact:** the exact round-trip guarantee of
  `url_source()` (bytes vs mark) is unspecified. **Settles at:** an owner decision
  on the source-reproduction guarantee (with the surface-a naming slice).
