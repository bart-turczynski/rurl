# Cleaning / mutation contracts (§6 artifact 8)

<!-- Contract artifact (§6 artifact 8). RCON-06 closure. This record PROJECTS the
     ACCEPTED owner decisions P2.2 (output-surface boundaries — the clean-output
     surface c, its lossiness/credential/fragment policy, and the presentation-
     dials-are-clean-only invariant) and P3.1 (identity/comparison key — the
     cleaner/display NON-INTERFERENCE invariant and the per-dial migration
     disposition) plus frozen evidence S4 (cleaning coverage + proposed mutation)
     into the normative cleaning-semantics, processing-order, downstream-
     capability, repair-provenance, non-interference, and (open) mutation
     matrices. It makes NO new product decision: every SETTLED cell is transcribed
     from an accepted record; a cell those records left as an owner open decision /
     deferral is flagged OPEN with impact + destination, never invented. The
     MUTATION surface is unbuilt and undecided at the P-tier (S4 M1-M18; no
     mutate_url in R/): its cells are recorded OPEN, not filled. This is the SINGLE
     WRITER of the v3 cleaning/mutation matrices. Canonical FIELD NAMES come from
     artifact 3 (contract-canonical-state); the comparison KEY + join family are
     G3.K/P3.1; the standard SERIALIZER (surface b) + credential OUTPUT policy are
     P2.2/G3.7; scheme-family transform ELIGIBILITY is G3.5/P4.1; host/PSL/IDNA
     vocabulary is G3.H; cache is G3.9 — each referenced as a boundary, never
     redefined. Format follows the G3.3/G3.5/G3.6/G3.K precedent: Envelope,
     tamper-evident Inputs, pipe-table Rows, Scope boundaries, Open cells. The
     envelope stays lifecycle_state PROPOSED until the cp-snapshot-3 seal;
     validator coverage and the manifest present-flip ride that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-cleaning-mutation |
| name | cleaning-mutation-contracts |
| artifact_number | 8 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/cleaning-mutation-contracts.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the v3 cleaning/mutation matrices: the cleaning-semantics matrix, cleaning processing order, downstream-capability classification, cleaning repair-provenance, the cleaner/identity non-interference invariant, and the (open) mutation state-transition/dependency/transaction/security/invariants surface |
| lifecycle_state | PROPOSED |
| dependencies | P2.2 (bound decision — clean-output surface c + presentation-dials-are-clean-only invariant); P3.1 (bound decision — cleaner/display non-interference invariant + per-dial migration disposition); S4 (bound evidence); contract-canonical-state (field vocabulary only, not projected); P2.1/P2.4 (repair-posture default, admission boundary — referenced); P4.1 (scheme-transform eligibility — referenced via G3.5); reconciliation §6 artifact 8, §4 RCON-06, §7 G3 |
| bound_decision | P2.2 + P3.1 |
| bound_evidence | S4 |
| closes_finding | RCON-06 |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: the cleaning-semantics matrix (per-dial surface assignment, lossiness, non-interference), processing order, downstream-capability classification, repair-provenance, the non-interference invariant, and every named mutation cell (semantics/verbs, state transitions, dependency graph, transaction behavior, security, invariants) each carry a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line impact and owner-decision destination; cross-artifact field names agree with artifact 3; validate-records.R (cleaning-mutation section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (P2.2, P3.1, S4), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | cleaning-mutation validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative v3 contract for two output-adjacent surfaces: **cleaning** — the
shipped 25-dial `get_clean_url()` / `clean_url` SEO-canonicalization surface — and
**mutation** — the *proposed but unbuilt* component-edit API (`mutate_url()`).

For **cleaning**, this record fixes each dial's surface assignment (clean/display
only, never a serializer or identity — P2.2), the shipped processing order
(byte-compat-preserved for admitted rows — P2.2 D4), the downstream-capability
classification (display-only, never comparison/routing/redirect/join-safe — P2.2
+ P3.1), the repair-provenance boundary (cleaning is Stage-B presentation over a
parsed object; the strict-default admission change flows through — P2.1/P2.4),
and the load-bearing **cleaner/identity non-interference invariant** (changing any
cleaning dial leaves the comparison key byte-identical — P3.1 D-A.3).

For **mutation**, no accepted P-tier decision defines the API: S4 records M1-M18
as OPEN v3 owner decisions and finds no `mutate_url` in `R/`. This record
therefore **records the mutation cells as OPEN** — one per named question — with
impact and a settlement destination; it invents no verb, transaction model,
dependency rule, or eligibility cell.

This record **projects** accepted P2.2 + P3.1 and S4; it makes no product
decision. A `SETTLED` row cites `P2.2@8292c7f` or `P3.1@3b89b94`. A cell an
accepted record left open or deferred is `OPEN` with its exact impact and
settlement destination.

## Inputs

The exact sources this contract projects, hashed at authoring.
`validate-records.R` recomputes all hashes at the cp-snapshot-3 seal. P2.2 and
P3.1 are already hash-enforced as ACCEPTED decisions by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P2.2-serializer-clean-output.md | 01542610a8aaa7f0c1abdbfaae373f65d220446b4f8ae8e6f29cc721cd263c9a |
| design/work/url-v3/decisions/P3.1-identity-canonical-join.md | b1760889be4cf0d756cab48928211186a8395e5197b851cf692fdc9db919ad4e |
| design/work/url-v3/evidence/S4-cleaning-mutation.md | 567d59cc3429fafb9fa4149aa77e5c12c67ebe9cb54bf0255a43fdd5aab09416 |

## Cleaning surface contract (surface c)

The clean surface is fixed by P2.2 §1(c) and its surface-assignment invariants
(P2.2 §5). Transcribed, not reinterpreted.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| identity | `clean_url` field + `get_clean_url()` accessor: an **intentionally lossy**, policy-driven SEO/canonicalization product (surface c); **not** a serializer, comparison identity, redirect target, or conformance oracle | P2.2@8292c7f (§1c, §5.1) | SETTLED |
| fragment + credentials | clean output **omits the fragment** and **does not reconstruct credentials** (per-surface contract, not a global URL policy); only surfaces (a) source-reproduction and (b) standard-serialization may reproduce credentials; surface (d) `format_url()` redacts | P2.2@8292c7f (§1c, §5.4, §5.5) | SETTLED |
| byte-compat scope | stays **byte-for-byte compatible for every input that remains admitted under the active posture** (D4); it neither widens nor narrows the admitted set — the strict-default flip (P2.1/P2.4) is an **admission** change, not a clean-serialization change | P2.2@8292c7f (§1c, D4) | SETTLED |
| lossy-vs-valid fork (S4 F2) | **RESOLVED**: the clean surface may be lossy on purpose; validity lives on the standard serializer (surface b), which never consults presentation dials — the S4 F2 "lossy cleaning vs standards-valid output" conflation is dissolved by the surface split | P2.2@8292c7f (§1c, §3, §5.3) | SETTLED |

## Cleaning-semantics matrix (the 25 shipped dials)

The shipped public surface has 25 dials (S4 inventory). For every dial the
SETTLED v3 facts are the **surface assignment** — a presentation/clean-and-display
dial only, never feeding the standard serializer (P2.2 §5.3 invariant 3) — and
**non-interference** — it must leave the comparison key byte-identical (P3.1
D-A.3). Per-dial *lossiness* and *shipped meaning* are S4-evidenced facts,
preserved byte-compatibly under P2.2 D4. The v3 *vocabulary rename* (S4 F5) is
OPEN (CLEAN-O1).

| # | dial (shipped) | governance | lossy? | key-affecting? | owner_decision_ref | status |
|---|---|---|---|---|---|---|
| 1 | `protocol_handling` | clean/presentation (emits scheme presentation after inference; the inference itself is Stage-A input, P2.1 stage 3) | no | no (P3.1 D-A.3) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3) | SETTLED |
| 2 | `www_handling` | clean/presentation (PSL-derived subdomain edit) | no | no (`www`/subdomain excluded from default key) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3, D-B host row) | SETTLED |
| 3 | `source` (PSL section) | clean/presentation input (suffix rules → domain/suffix/subdomain boundaries) | no | no | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3) | SETTLED |
| 4 | `tld_source` (row 3's dial, spelled for the parse surface) | clean/presentation input (same suffix-rule role as row 3; `source` is the accessor formal, `tld_source` the `safe_parse_url(s)` formal, and the accessors forward `source` as `tld_source`) — **both live; neither is deprecated** | no | no | P2.2@8292c7f (§5.3) | SETTLED |
| 5 | `case_handling` | clean/presentation (host-only default; query exempt) | can be (whole-string lower/upper) | no (key compares normalized identity, not display case) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3, scheme-case row D-B) | SETTLED |
| 6 | `trailing_slash_handling` | clean/presentation | no | no | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3) | SETTLED |
| 7 | `index_page_handling` | clean/presentation | can be (drops a terminal index) | no | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3) | SETTLED |
| 8 | `path_normalization` | clean/presentation (after standard path identity) | can be (dot-segment / slash collapse) | no (key compares post-standard identity path, P3.1 D-B) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3, D-B path rows) | SETTLED |
| 9 | `scheme_relative_handling` | **input interpretation** (lives on the accessor but is a Stage-A axis, ADR 0010); not a clean transform | n/a | governed as input, not by cleaning | P2.1@a4d1b45 (input axis); P4.1@b017e87 (D-B, via G3.5) | SETTLED (boundary — input axis, not cleaning) |
| 10 | `subdomain_levels_to_keep` | clean/presentation (PSL-derived subdomain trim) | can be | no | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3) | SETTLED |
| 11 | `host_encoding` | clean/presentation (idna/unicode display for eligible domain hosts; no-op on IP/opaque/empty/absent) | no (spelling only) | no (key compares normalized domain identity, never unicode/punycode display, P3.1 D-B) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3, D-B host row) | SETTLED |
| 12 | `path_encoding` | clean/presentation (**C-05**: `%2F`/reserved-octet compat dial, kept OUT of the standard serializer) | **yes** (encode/decode fold reserved delimiters) | no (key preserves reserved bytes; `%2F` never silently becomes `/`, P3.1 D-B — the direct HIGH-3 fix) | P2.2@8292c7f (§3 C-05, §5.3); P3.1@3b89b94 (D-A.3, D-B path rows) | SETTLED |
| 13-19 | query dials (`query_handling`, `params_keep`, `params_drop`, `params_case_sensitive`, `sort_params`, `empty_param_handling`, `decode_plus`) | clean/presentation (filtering/sorting/encoding; query is an ordered sequence, not a map; whole-URL case never folds query data, S4 interaction 9) | can be (drop/filter/sort/canonical re-encode) | no (default key is **exact structural query**; SEO ignore/filter is a separate explicit key policy, never a display dial — P3.1 D-B, Q2/B6) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3, D-B query rows) | SETTLED |
| 20 | `port_handling` | clean/presentation output policy (`keep` is a literal override even under WHATWG) | can be | no (port equivalence is the P3.1 D-B truth table, not a clean dial) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3, D-B port table) | SETTLED |
| 21 | `scheme_policy` | **input acceptance** axis (not a clean transform); default flips `infer → require` at 3.0 | n/a | governed as input, not by cleaning | P2.1@a4d1b45 (§1, B1); P2.4@b017e87 (D-C, via G3.5) | SETTLED (boundary — input axis) |
| 22 | `scheme_acceptance` | **input acceptance** axis (web/general); `general` does not imply SEO-transform eligibility | n/a | governed as input, not by cleaning | P2.4@b017e87 (D-B, via G3.5) | SETTLED (boundary — input axis) |
| 23 | `url_standard` | **interpretation** axis (governs parse/identity, not cleaning presentation) | n/a | key is standard-scoped (a different standard is a different identity, not a clean edit) | P4.1@b017e87 (via G3.5); ADR 0007 | SETTLED (boundary — interpretation axis) |
| 24 | `engine` (PSL engine) | clean/presentation input (suffix source/version for domain decomposition) | no | no (annotation state, L3; must not alter identity — P3.1 uses normalized domain identity) | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3) | SETTLED (cache/engine identity → G3.9) |
| 25 | `profile` | bundle expansion (fills only unspecified dials; explicit args win; result becomes customized) | n/a | non-interference holds for the whole bundle (P3.1 D-A.3 names "a whole `profile` bundle") | P2.2@8292c7f (§5.3); P3.1@3b89b94 (D-A.3) | SETTLED |
| internal | `fixup_posture`, `path_identity` | profile-resolved input/identity controls (Stage-A repair posture; path identity before presentation) — **not** clean transforms | n/a | governed as input/identity, not by cleaning | P2.1@a4d1b45 (posture); P4.1@b017e87 (via G3.5) | SETTLED (boundary — input/identity controls) |
| eligibility | semantic-transform eligibility per scheme family | **automatic SEO semantic transforms are HTTP(S)-only**; other admitted schemes serialize but do not receive SEO transforms; host/subdomain dials no-op on non-domain hosts (IP/opaque/empty) — shipped behavior preserved by P2.2 D4 byte-compat | — | — | P4.1@b017e87 (D-A/D-B, via G3.5); P2.2@8292c7f (D4) | SETTLED (eligibility owned by G3.5) |
| vocabulary | `keep`/`none`/`strip` verb semantics diverge per dial (S4 F5) | the v3 rename to precise verbs (`preserve_if_present`/`ensure`/`omit`/`canonicalize`/`retain_explicit`) is **not** decided by P2.2/P3.1 | — | — | — (see Open cells CLEAN-O1) | OPEN |

**Errata on row 4 (`RURL-vwobubfs`).** This row previously read "`tld_source`
(deprecated alias of `source`)". That was false and is corrected above: nothing
in the package is deprecated (no `.Deprecated()` call, no lifecycle badge, no
removal schedule), and the two spellings are not aliases *of one another* —
they are the one PSL-section dial exposed on two surfaces, `source` on the
accessors and `tld_source` on `safe_parse_url(s)`.

The gloss did **not** come from the cited authority: P2.2 nowhere mentions
`tld_source` or deprecation. It was transcribed from evidence
`S4-cleaning-mutation.md:48` ("deprecated alias, default `NULL`"), a
review-slice characterization that was already inaccurate — the shipped default
is `"all"`, not `NULL`. S4 is frozen evidence (sha256-pinned in
`evidence/SHA256SUMS.txt`) and is therefore **left unedited on purpose**; this
note, not a correction to S4, is what prevents the claim being restored from it.
The same transcription was corrected in `R/canonical_join.R` and
`tests/testthat/test-canonical-join-legacy-dials.R`, which had inherited it.

## Cleaning processing-order rows

The shipped Stage-B order is a fixed pipeline (S4 interaction inventory 6);
changing it changes results. It is preserved byte-compatibly for admitted rows by
P2.2 D4. This contract records the order as the SETTLED cleaning pipeline; it does
not reorder it.

| step | operation | owner_decision_ref | status |
|---|---|---|---|
| 1 | standard-specific **path identity** (Stage-A/`path_identity`) precedes all explicit cleaning | P2.2@8292c7f (D4); P4.1@b017e87 (via G3.5) | SETTLED |
| 2 | `path_normalization` (explicit) | P2.2@8292c7f (D4) | SETTLED |
| 3 | `index_page_handling` (strip terminal index → directory) | P2.2@8292c7f (D4) | SETTLED |
| 4 | `trailing_slash_handling` | P2.2@8292c7f (D4) | SETTLED |
| 5 | `path_encoding` (last; lossy encode/decode) | P2.2@8292c7f (D4, §3) | SETTLED |
| 6 | host: PSL derivation → `www_handling` → `subdomain_levels_to_keep` (www tracked separately, before retained levels) | P2.2@8292c7f (D4) | SETTLED |
| 7 | `host_encoding` then `case_handling` (query data never case-folded) | P2.2@8292c7f (D4) | SETTLED |
| 8 | query phase (`query_handling` filter/sort/encode; ordered sequence, keep-list rescue precedes empty/drop) | P2.2@8292c7f (D4) | SETTLED |
| 9 | `port_handling` output policy; final assembly by the legacy clean builder (emits only nonempty query; omits userinfo + fragment) | P2.2@8292c7f (§1c, D4) | SETTLED |

## Downstream-capability classification (S4 F13)

Every produced string/key declares its safe uses. The clean surface is
**display-only**; it is never an identity, comparison, routing, or redirect key.

**`reparsable` and `standards-valid` are state-preservation properties**, not
properties of the emitted text read alone: `reparsable` means re-parsing
reconstructs the contractually relevant parsed state, and `standards-valid`
means the string is a faithful serialization of that state under a named
standard and is admissible as conformance evidence. Neither means "a parser
accepts this string". The definitions and the `RURL-szvncnou` case that made
them worth stating live with the twin table in
[`output-contracts.md`](output-contracts.md); this is the same classification,
so it carries the same readings.

| produced value | reparsable | standards-valid | comparison/identity-safe | join/redirect-safe | display-only | owner_decision_ref | status |
|---|---|---|---|---|---|---|---|
| `clean_url` / `get_clean_url()` (surface c) | no (intentionally lossy) | no (SEO/policy product) | **no** (never an identity — P2.2 §5.1) | **no** (never a join key — P3.1 D-A; legacy `canonical_join` `clean_url` keying is LEGACY-to-migrate) | **yes** | P2.2@8292c7f (§1c, §5.1); P3.1@3b89b94 (D-A, D-E) | SETTLED |
| comparison key `get_url_key()` | n/a (non-URL projection) | n/a | **yes** (the identity surface) | yes | no | P3.1@3b89b94 (D-A) — owned by **G3.K**, referenced | SETTLED (boundary — key owned by G3.K) |
| standard serialization `serialize_url()` (surface b) | yes (parse→serialize→parse) | **yes** | n/a (identity is the key, not the string) | n/a | no | P2.2@8292c7f (§1b) — owned by **P2.2/G3.7**, referenced | SETTLED (boundary) |

## Cleaner / identity non-interference invariant (P3.1 D-A.3)

The load-bearing invariant that binds cleaning to identity — the executable form
of the identity/presentation split.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| non-interference | for a fixed parse standard and key policy, changing **any** cleaning/profile/display-only dial (§cleaning-semantics rows 1-25 that are clean/presentation) MUST leave `get_url_key()` **byte-identical**; a key changes **only** when a named comparison-policy field changes | P3.1@3b89b94 (D-A.3) | SETTLED |
| the defect it fixes | the shipped `canonical_join()` forwarded `path_encoding` through `...`, letting a display dial change equality (`%2F`→`/` false-merge); v3 forbids this — cleaning cannot touch the key | P3.1@3b89b94 (D-A.3, HIGH-3); P2.2@8292c7f (§5.1) | SETTLED |
| test-revision obligation | the shipped `test-url-standard-path-encoding-orthogonal.R` assertion that `canonical_join()` forwards `path_encoding` is revised to assert **key invariance** (an implementation-slice obligation, not this contract) | P3.1@3b89b94 (Q7/B7) | SETTLED (boundary — impl slice) |

## Cleaning repair-provenance rows (S4 F3)

Cleaning is Stage-B presentation over a parsed object; any input repair is a
Stage-A concern owned by the posture axis (G3.6), never hidden inside cleaning.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| cleaning ≠ repair | cleaning is a present-stage projection over an already-parsed object (ADR 0003); the fixer/inference/recovery that may precede it are the Stage-A repair-posture pipeline (G3.6 / P2.1), never silent inside a clean call | P2.2@8292c7f (§1, ADR 0003); P2.1@a4d1b45 (pipeline) | SETTLED |
| strict-default admission flow-through | under the v3 `strict` default, scheme-less host-shaped input rejects (P2.1/P2.4), so a former `get_clean_url("example.com")→http://example.com/` inference **no longer occurs by default** — the row produces no clean output to preserve; this is the admission change flowing through cleaning, not a clean-surface behavior change | P2.1@a4d1b45 (§1, B1); P2.2@8292c7f (§1c D4 scope) | SETTLED |
| S4 F3 provenance boundary | whether/how a cleaning entry point *surfaces* which Stage-A repair ran (provenance exposure) is the intervention-ledger surface of G3.6, not a clean-surface field | G3.6 (validation-intervention-contract); P2.1@a4d1b45 (§2 ledger) | SETTLED (boundary — provenance owned by G3.6) |

## Credential / security rows (S4 F12)

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| clean surface never reconstructs credentials | surface (c) `clean_url` never emits userinfo; the secret-in-output guard that lets P4.1 preserve generic-authority credentials in canonical state rests on this surface split | P2.2@8292c7f (§5.5, §1c) | SETTLED |
| safe display redacts | surface (d) `format_url()` redacts credentials (surface fixed; its escape/annotation matrix is deferred by P2.2 Q3) | P2.2@8292c7f (§5.5, §1d) | SETTLED |
| **mutation** credential safety | redacted printing/errors, an explicit serialization opt-in, and whether mutation may create/change credentials (S4 F12/M16) are undecided — no mutation API exists | — (see Open cells MUT-O6) | OPEN |

## Mutation contract — status and cells

**No accepted P-tier decision defines a mutation API, and `R/` contains no
`mutate_url` / component setter (S4 §"Mutation gaps", M1-M18).** P2.2 governs
output *surfaces* and P3.1 governs *identity*; neither settles mutation
semantics. Per the "project, don't invent" mandate every mutation cell is
therefore **OPEN**, recorded with its S4 open-decision ID and a shared settlement
destination — **a dedicated P3-tier mutation-slice owner decision** (RCON-06's
owner tier is P3). This contract fixes only that these cells *exist* and are
unowned; it invents no verb, transaction model, dependency rule, or eligibility.

| cell (issue-named) | question | S4 ref | owner_decision_ref | status |
|---|---|---|---|---|
| semantics / verb vocabulary | input+result type; `add/set/replace/remove/preserve` as universal verbs vs component-specific ops vs declarative target-state | M1, M2 | — (see MUT-O1) | OPEN |
| component dependency graph | stored vs structural vs derived fields; authority⊃(credentials,host,port); hostname⊃(subdomain,registered-domain,suffix); legal parent/child co-writes; PSL recomputation | M6 | — (see MUT-O2) | OPEN |
| state transitions | per-verb behavior over `absent` / `present-empty` / `present-nonempty` (artifact-3 presence model), incl. delimiter-presence addressability | M8 | — (see MUT-O3) | OPEN |
| transaction behavior | atomic unit (row/vector/object); simultaneous-vs-ordered application; recycling / length mismatch; rollback; multi-error diagnostics; original-vs-intermediate validation state | M3, M4, M5, M15 | — (see MUT-O4) | OPEN |
| eligibility | apply / no-op / reject / unsupported for every operation × scheme family × authority kind × host kind × path kind | M7 | — (see MUT-O5) | OPEN |
| security | credential redaction in print/errors; serialization opt-in; whether mutation may create credentials | M16 | — (see MUT-O6) | OPEN |
| invariants | idempotence where claimed; serialize/parse round-trip; preservation of untouched components; collision detection for lossy projections | M17 | — (see MUT-O7) | OPEN |
| query model | ordered multimap; duplicate targeting; bare-vs-empty; raw/decoded matching; insertion position; plus/malformed-percent; encode-on-commit | M9 | — (see MUT-O8) | OPEN |
| path model | whole-path vs hierarchical-segment; encoded-delimiter identity; opaque/list-path prohibition; commit normalization/encoding order | M10 | — (see MUT-O9) | OPEN |
| PSL binding | source/version carriage; private/unknown-suffix behavior; IP/opaque hosts; suffix-write legality | M11, M14 (PSL) | — (see MUT-O10) | OPEN |
| standards / profile / repair interaction | which standard validates a mutation; profile expansion/override + customized labeling; whether mutation accepts invalid/repaired input or invokes fixup/inference | M12, M13, M14 | — (see MUT-O11) | OPEN |
| downstream safety | whether mutation output is comparison/routing/redirect/join-safe; which APIs reject display-only values | M18 | — (see MUT-O12) | OPEN |

## Scope boundaries

This contract owns the **cleaning + mutation** matrices. It does **not** define,
and must not be read as redefining:

- **Canonical-state field vocabulary** (presence model `absent`/`present-empty`/
  `present-nonempty`, component fields) — consumed from artifact 3
  (`contract-canonical-state`) without renaming.
- **The comparison key + six-join family** (`get_url_key()`, `url_key_policy()`,
  the port/scheme truth tables, `url_*_join()`, and the `canonical_join()`
  migration) — **G3.K / P3.1**. This contract consumes only P3.1's cleaner/display
  **non-interference invariant** and per-dial migration disposition; the key
  policy and joins are G3.K's.
- **The standard serializer (surface b) + source reproduction (a) + safe display
  (d), and credential OUTPUT policy** — **P2.2 / G3.7**. This contract owns the
  clean surface (c) only; `serialize_url()`, `url_source()`, and `format_url()`
  are named as boundaries.
- **Scheme-family transform eligibility + admission/interpretation axes**
  (`scheme_acceptance`, `scheme_policy`, `url_standard`, HTTP(S)-only SEO
  transforms) — **G3.5 / P2.4 / P4.1**. This contract states each cleaning dial's
  eligibility *by reference*, not by redefinition.
- **The repair-posture pipeline + intervention ledger + verdict layers** —
  **G3.6 / P2.1 / P2.3**. Cleaning's repair-provenance boundary points here.
- **Host / IDNA / PSL / DNS / IP vocabulary** and PSL section/engine identity —
  **G3.H**; cache participation of the engine — **G3.9**.
- **The entire mutation API surface** is **unowned at the P-tier**: this contract
  records its cells OPEN and does not design it. Building it requires the S4
  M1-M18 owner decisions first.
- **Documentation hygiene** — correcting README / `man/get_clean_url.Rd` /
  `ARCHITECTURE.md` prose that calls `clean_url` a "canonical key" (S4 F13; P3.1
  Invalidated-artifacts) is a parked implementation-slice housekeeping item, not a
  cell of this contract.

## Open cells

Each cell an accepted record left open or deferred is recorded rather than
invented. None reopens a SETTLED default.

- **CLEAN-O1 — cleaning-vocabulary rename.** The shipped `keep`/`none`/`strip`
  verbs carry per-dial-divergent semantics (S4 F5: `protocol_handling="keep"`
  adds a component; `query_handling="keep"` rewrites `?a`→`?a=`). The v3 rename to
  precise verbs and the compat-alias/deprecation lifecycle are **not** decided by
  P2.2 or P3.1. **Impact:** the clean-dial public vocabulary remains ambiguous and
  its migration is unscheduled. **Settles at:** the RCON-05 public-surface
  decision (coordinated with the P2 lattice; cf. G3.5/G3.6 posture-spelling open
  cells) or a dedicated cleaning-vocabulary migration slice.

- **MUT-O1 — mutation semantics / verb vocabulary (S4 M1/M2).** Input+result type
  and the operation vocabulary/API shape are undecided. **Impact:** the entire
  `mutate_url()` public shape is unspecified. **Settles at:** the P3 mutation-slice
  owner decision.
- **MUT-O2 — component dependency graph (S4 M6).** Stored/structural/derived
  field classification and legal parent/child co-writes are undecided. **Impact:**
  overlapping-component writes (authority/host/port; hostname/subdomain/domain/
  suffix) have no defined resolution. **Settles at:** the P3 mutation-slice.
- **MUT-O3 — mutation state transitions (S4 M8).** Per-verb behavior over
  `absent`/`present-empty`/`present-nonempty` and delimiter addressability is
  undecided. **Impact:** mutation could destroy the empty-vs-absent distinction
  artifact 3 mandates. **Settles at:** the P3 mutation-slice.
- **MUT-O4 — transaction behavior (S4 M3/M4/M5/M15).** Atomicity, ordering,
  recycling, rollback, and multi-error diagnostics are undecided. **Impact:**
  "transactional validation" is untestable; partial mutation is unspecified.
  **Settles at:** the P3 mutation-slice.
- **MUT-O5 — mutation eligibility (S4 M7).** The apply/no-op/reject/unsupported
  cell for every operation × scheme/authority/host/path kind is undecided.
  **Impact:** implementations may disagree reject-vs-no-op-vs-apply. **Settles
  at:** the P3 mutation-slice (crossing the G3.5 eligibility axes).
- **MUT-O6 — mutation credential safety (S4 M16/F12).** Redaction, serialization
  opt-in, and credential-creation policy for mutation are undecided. **Impact:**
  secrets could leak via print/errors/logs. **Settles at:** the P3 mutation-slice
  (consistent with P2.2 §5.5 for output surfaces).
- **MUT-O7 — mutation invariants (S4 M17).** Idempotence, round-trip,
  untouched-component preservation, and lossy-collision detection are undecided.
  **Impact:** "symmetric and predictable" is unverifiable. **Settles at:** the P3
  mutation-slice.
- **MUT-O8 — query mutation model (S4 M9).** The ordered-multimap edit model is
  undecided. **Impact:** duplicate/order loss or double-encoding risk. **Settles
  at:** the P3 mutation-slice (paired with the query PRD).
- **MUT-O9 — path mutation model (S4 M10).** Whole-path vs hierarchical-segment
  edits and encoded-delimiter identity are undecided. **Impact:** presentation
  choice could change structure; hierarchical ops on opaque paths. **Settles at:**
  the P3 mutation-slice.
- **MUT-O10 — mutation PSL binding (S4 M11/M14).** Source/version carriage and
  suffix-write legality for domain-derived mutation are undecided. **Impact:**
  mutations could change silently when the PSL dataset changes. **Settles at:**
  the P3 mutation-slice (with the host record / G3.H).
- **MUT-O11 — mutation standards/profile/repair interaction (S4 M12/M13/M14).**
  Validation standard, profile expansion/customized-labeling, and repair-input
  acceptance are undecided. **Impact:** profile labels could misrepresent mutated
  objects; validation against the wrong pre/post state. **Settles at:** the P3
  mutation-slice.
- **MUT-O12 — mutation downstream safety (S4 M18).** Whether mutation output is
  comparison/routing/redirect/join-safe is undecided. **Impact:** display-only
  values could be mistaken for identity-safe. **Settles at:** the P3 mutation-slice
  (consistent with the §downstream-capability classification).
