# Key and join contracts (§6 artifact 9)

<!-- Contract artifact (§6 artifact 9). RCON-07 closure. This record PROJECTS
     the ACCEPTED owner decision P3.1 (v3 identity and canonical_join migration)
     and frozen evidence S5 (comparison keys and URL joins) into normative key,
     equivalence, join, and migration matrices. It makes NO new product
     decision: every SETTLED cell is transcribed from P3.1; cells the accepted
     record did not close are flagged OPEN, never invented. This is the SINGLE
     WRITER of the v3 comparison-key and URL-join surface. G3.7 references its
     key capability and never redefines equality. Format follows the G3.3
     precedent: Envelope, tamper-evident Inputs, pipe-table Rows, Scope
     boundaries, Open cells. The envelope remains lifecycle_state PROPOSED
     until the cp-snapshot-3 seal; validator coverage and the manifest
     present-flip ride that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-key-join |
| name | key-join-contracts |
| artifact_number | 9 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/key-join-contracts.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of comparison-key policy, URL-join semantics, and canonical_join migration |
| lifecycle_state | PROPOSED |
| dependencies | P3.1 (bound decision); S5 (bound evidence); contract-canonical-state (field vocabulary only); P2.2 (cleaning/identity boundary only); reconciliation §6 artifact 9, §7 G3 |
| bound_decision | P3.1 |
| bound_evidence | S5 |
| closes_finding | RCON-07 |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: key representation/policy, component equivalence, scheme/port truth table, eligibility/collision behavior, six join operations, cross-cutting join behavior, and legacy migration each carry a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line impact and owner-decision destination; cross-artifact field names agree; validate-records.R (key-join section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (P3.1, S5), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | key-join validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative v3 contract for comparison keys and URL joins. It instantiates
P3.1's identity-before-presentation decision as a versioned, injectively framed
non-URL key; fixes the component and scheme/port equivalence rules P3.1 settled;
defines the six identity-keyed join operations; and records the non-silent
migration of legacy `canonical_join()` away from implicit `clean_url` equality.

This record **projects** P3.1 + S5; it does not implement these surfaces and
makes no product decision. A `SETTLED` row cites `P3.1@3b89b94`. A cell P3.1
left incomplete is `OPEN` with its exact impact and settlement destination.

## Inputs

The exact sources this contract projects, hashed at authoring.
`validate-records.R` recomputes both hashes at the cp-snapshot-3 seal. P3.1 is
already hash-enforced as an ACCEPTED decision by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P3.1-identity-canonical-join.md | b1760889be4cf0d756cab48928211186a8395e5197b851cf692fdc9db919ad4e |
| design/work/url-v3/evidence/S5-keys-joins.md | bd1506841a495a77c827dd715d7fb7a31e6afb864a0ae138a304e92a62b182e9 |

## Key surface rows

| surface | contract | invariants | owner_decision_ref | status |
|---|---|---|---|---|
| `get_url_key(url, policy = url_key_policy())` | exported, length-preserving, names-preserving comparison projection; accepts character/factor input under explicit coercion; policy is scalar and never silently recycled | returns a classed non-URL key; a non-keyable element is `NA` plus a companion reason; scalar/vector behavior is deterministic | P3.1@3b89b94 (D-A.1, D-C) | SETTLED |
| `url_key_policy()` | one immutable, symmetric, versioned policy shared by both sides of a comparison or join | side-specific rules are prohibited because equality must remain symmetric and transitive; a changed default/policy bumps the key version | P3.1@3b89b94 (D-B, D-D) | SETTLED |
| key representation | classed object carrying key-policy version and schema version in metadata; internal tuple uses injective length-prefixed/binary framing; printable form is diagnostics-only | never ambiguous delimiter concatenation; distinct component tuples cannot collide even when payloads contain separators/control bytes | P3.1@3b89b94 (D-A.2) | SETTLED |
| identity input | canonical identity state after standard interpretation and before cleaning/display | never `clean_url`; consumes canonical structural kinds and identity spellings from artifact 3 without renaming them | P3.1@3b89b94 (D-A, D-B) | SETTLED |
| non-interference | cleaning, profile, and display-only options cannot change key bytes for a fixed interpretation + key policy | includes `path_encoding`, `host_encoding`, protocol/case/www/subdomain/PSL presentation, query cleaning, `port_handling`, and profile bundles | P3.1@3b89b94 (D-A.3) | SETTLED |
| interpretation selector | standard interpretation is an explicit key-policy field and may change canonical identity | P3.1 requires a stable default but does not name it | — (see Open cells KJ-O1) | OPEN |
| diagnostic surface | keyability reason is exposed by a companion path; credentials are not emitted in diagnostic keys/errors | `NA` alone is not allowed to conflate missing input with invalid parse | P3.1@3b89b94 (D-C, D-D) | SETTLED |

## Key-policy rows

The key frames structural state, not presentation. `exact` below means the
selected standard's normalized identity, not source-byte equality.

| dimension | default / rule | explicit alternatives or limits | owner_decision_ref | status |
|---|---|---|---|---|
| scheme source | preserve original presence separately from parsed effective scheme | explicit, inferred, missing, scheme-relative, relative, malformed remain distinguishable; inference never exists only to erase missing | P3.1@3b89b94 (D-B) | SETTLED |
| scheme equality | `exact` | `http_https`, `http_https_missing` are opt-in; relaxed modes collapse only named web-scheme states; every other valid scheme remains exact | P3.1@3b89b94 (D-B; ratification Q1/B5) | SETTLED |
| scheme case | normalized case-insensitive scheme identity | retain raw spelling only for source reproduction, never key equality | P3.1@3b89b94 (D-B) | SETTLED |
| port | normalize absent vs explicit default for HTTP `80` and HTTPS `443` under each row's own explicit scheme | non-default, another scheme's default, empty/invalid, and missing-scheme ports remain significant; ftp/ws/wss/custom stay literal in key-policy v1 | P3.1@3b89b94 (D-B; ratification Q3/Q4/Q8) | SETTLED |
| authority | frame `authority_kind` independently from `host_kind`; never collapse absent and empty accidentally | the exact vocabulary is consumed from artifact 3 and therefore inherits its authority-state OPEN cell | P3.1@3b89b94 (D-B); contract-canonical-state Open cells | SETTLED |
| host kind | include domain/opaque/IPv4/IPv6/empty/absent kind; normalize according to kind | DNS/IDNA only for eligible domain hosts; canonical IP identity for IP kinds; opaque hosts use scheme/standard-appropriate identity | P3.1@3b89b94 (D-B) | SETTLED |
| domain spelling | internal normalized domain identity, never Unicode/Punycode display | trailing-root-dot equivalence is not fixed by P3.1 | — (see Open cells KJ-O2) | OPEN |
| host editing | `www`, subdomain/PSL trimming, and presentation encoding are excluded | a future lossy `host_scope` comparison mode requires a separately versioned policy and provenance | P3.1@3b89b94 (D-B) | SETTLED |
| path | selected standard's post-interpretation structural identity before presentation; path kind and segments framed | reserved encoded bytes remain data unless the selected standard parsed them structurally; `%2F` never becomes `/` merely for comparison | P3.1@3b89b94 (D-B) | SETTLED |
| path display/editing | excluded | case/presentation encoding, index removal, trailing-slash and SEO cleanup may exist only as separately named lossy comparison policies | P3.1@3b89b94 (D-A.3, D-B) | SETTLED |
| query presence | exact structural query participates by default; absent, present-empty, present-nonempty remain distinct | query filtering/ignore is an explicit versioned comparison policy, never a cleaning/profile dial | P3.1@3b89b94 (D-B; ratification Q2/B6) | SETTLED |
| query structure | order and duplicates significant; URL-query semantics, not form semantics | pair sorting/filtering or form-style plus handling requires an explicit comparison policy; whole-string decoding before delimiter parsing is forbidden | P3.1@3b89b94 (D-B) | SETTLED |
| fragment | ignored for default web-resource identity; structural state retained outside key for diagnostics | any included-fragment mode must be explicit and versioned | P3.1@3b89b94 (D-B; ratification Q5) | SETTLED |
| userinfo | ignored for default web-resource identity; structural state retained outside key for diagnostics/security | userinfo never enters or leaves equality merely because display strips it | P3.1@3b89b94 (D-B; ratification Q5) | SETTLED |
| missing/invalid | non-keyable → `NA` key + typed reason; default never-match | opt-in missing equality, if later exposed, may match missing-input only to missing-input and never invalid parse | P3.1@3b89b94 (D-C) | SETTLED |
| persisted-key stability | key carries policy version + schema version | no release may silently reinterpret a persisted older key | P3.1@3b89b94 (D-A.2, D-B) | SETTLED |

## Scheme and port truth table

Default-port normalization occurs **before** optional scheme collapsing and uses
each row's own explicit recognized scheme. `—` means the relaxed scheme mode
does not widen that scheme family.

| left state | right state | `exact` | `http_https` | `http_https_missing` | owner_decision_ref | status |
|---|---|---:|---:|---:|---|---|
| HTTP absent port | HTTP `:80` | equal | equal | equal | P3.1@3b89b94 (D-B) | SETTLED |
| HTTPS absent port | HTTPS `:443` | equal | equal | equal | P3.1@3b89b94 (D-B) | SETTLED |
| HTTP absent port | HTTP `:8080` | distinct | distinct | distinct | P3.1@3b89b94 (D-B) | SETTLED |
| HTTPS absent port | HTTPS `:80` | distinct | distinct | distinct | P3.1@3b89b94 (D-B) | SETTLED |
| HTTP `:443` | HTTPS absent | distinct | distinct | distinct | P3.1@3b89b94 (D-B; ratification Q3) | SETTLED |
| HTTP `:80` | HTTPS `:443` | distinct | distinct | distinct | P3.1@3b89b94 (D-B; ratification Q3) | SETTLED |
| HTTP absent | HTTPS absent | distinct | equal | equal | P3.1@3b89b94 (D-B) | SETTLED |
| missing scheme/no port | HTTP/no port | distinct | distinct | equal | P3.1@3b89b94 (D-B) | SETTLED |
| missing scheme `:80` | HTTP/no port | distinct | distinct | distinct | P3.1@3b89b94 (D-B; ratification Q4) | SETTLED |
| missing scheme `:443` | HTTPS/no port | distinct | distinct | distinct | P3.1@3b89b94 (D-B; ratification Q4) | SETTLED |
| scheme-relative/no port | missing host-shaped/no port | distinct | distinct | distinct | P3.1@3b89b94 (D-B; ratification Q4) | SETTLED |
| FTP `:21` | FTP/no port | distinct | — | — | P3.1@3b89b94 (D-B; ratification Q8) | SETTLED |
| WS `:80` / WSS `:443` | same scheme absent port | distinct | — | — | P3.1@3b89b94 (D-B; ratification Q8) | SETTLED |
| custom `:123` | same custom scheme/no port | distinct | — | — | P3.1@3b89b94 (D-B; ratification Q8) | SETTLED |

## Eligibility and collision rows

| concern | contract | owner_decision_ref | status |
|---|---|---|---|
| eligibility vocabulary | distinguish OK, warning, invalid parse, unsupported-but-valid scheme, relative/opaque reference, missing input, empty input | P3.1@3b89b94 (D-C) | SETTLED |
| warning rows | warning behavior is an explicit join axis, separate from invalid rows | P3.1@3b89b94 (D-D) | SETTLED |
| non-keyable rows | public key is `NA` and companion reason is typed; missing is never conflated with invalid | P3.1@3b89b94 (D-C) | SETTLED |
| missing matching | never-match by default; any future equality applies only missing↔missing, never missing↔invalid | P3.1@3b89b94 (D-C) | SETTLED |
| key collision | prevented by injective tuple framing | P3.1@3b89b94 (D-A.2, D-C) | SETTLED |
| duplicate keys | a join multiplicity/relationship fact; rows are not discarded to "resolve" it | P3.1@3b89b94 (D-C, D-D) | SETTLED |
| resource guard | preflight eligible-key counts and declared relationship before Cartesian materialization | P3.1@3b89b94 (D-D) | SETTLED |

## Six-join matrix

All operations use one symmetric `url_key_policy`; the join family is an
independent rurl contract, not a promise of compatibility with an unspecified
dplyr version.

| operation | retained rows and multiplicity | stable order | output | invalid/missing default | owner_decision_ref | status |
|---|---|---|---|---|---|---|
| `url_inner_join(x, y, ...)` | eligible matching x/y pairs; Cartesian duplicate expansion unless relationship/multiple policy rejects or narrows | x order; y match order within each x | x columns then non-key y columns under suffix rules; comparison-key visibility follows KJ-O7 | non-keyable rows do not match and are omitted | P3.1@3b89b94 (D-D) | SETTLED |
| `url_left_join(x, y, ...)` | every x row; eligible matches expand; unmatched x receives missing y payload | x order; y match order within each x | family prototype | non-keyable x retained unmatched; non-keyable y cannot match | P3.1@3b89b94 (D-D) | SETTLED |
| `url_right_join(x, y, ...)` | every y row; eligible matches expand; unmatched y receives missing x payload | P3.1 does not choose y-primary mirror versus another pinned algorithm | family prototype | non-keyable y retained unmatched; non-keyable x cannot match | — (see Open cells KJ-O3) | OPEN |
| `url_full_join(x, y, ...)` | all matched pairs plus unmatched rows from both sides | left-join result in x order, then unmatched y in y order | family prototype | non-keyable rows retained separately and never match | P3.1@3b89b94 (D-D) | SETTLED |
| `url_semi_join(x, y, ...)` | each x row whose eligible key has at least one eligible y match; never duplicate x for duplicate y | x order | x columns only | non-keyable x excluded; non-keyable y supplies no match | P3.1@3b89b94 (D-D) | SETTLED |
| `url_anti_join(x, y, ...)` | each x row with no eligible y match; one output per x | x order | x columns only | P3.1 does not confirm whether non-keyable x is retained by default | — (see Open cells KJ-O4) | OPEN |

## Cross-cutting join rows

| axis | contract | owner_decision_ref | status |
|---|---|---|---|
| URL columns | first release accepts one named URL column per side; exact public `by`/`col_x`/`col_y` signature is not fixed | — (see Open cells KJ-O5) | OPEN |
| key policy | one immutable policy applied symmetrically | P3.1@3b89b94 (D-D) | SETTLED |
| parse policy | selected interpretation may affect identity; cleaning profiles and display dials are rejected as match inputs | P3.1@3b89b94 (D-A.3, D-D) | SETTLED |
| relationship | `none`, `one-to-one`, `one-to-many`, `many-to-one`, `many-to-many`; validate eligible keys before materialization | P3.1@3b89b94 (D-D) | SETTLED |
| multiple matches | default `all`; any lossy first/last mode is separately named with stable order and never reuses `collision` | P3.1@3b89b94 (D-D) | SETTLED |
| duplicate counts | eligible, nonmissing keys only; missing/invalid counts reported separately | P3.1@3b89b94 (D-D) | SETTLED |
| invalid/warnings | separate `invalid = keep/drop/error` and `warnings = allow/reject/error` axes; do not reuse `on_parse_error` | P3.1@3b89b94 (D-D) | SETTLED |
| key visibility | P3.1 allows either hidden-by-default or explicit exposure under a collision-proof name; the exposed value is always the classed non-URL key | — (see Open cells KJ-O7) | OPEN |
| original URLs | preserve both originals when names differ; semi/anti keep x only; never overwrite with cleaned display | P3.1@3b89b94 (D-D) | SETTLED |
| suffix/name repair | behavior must be deterministic and reject ambiguity, but P3.1 does not choose distinct-suffix rejection versus a single repair algorithm | — (see Open cells KJ-O6) | OPEN |
| row order except right join | exact per settled cells of the six-join matrix, including duplicate and unmatched placement | P3.1@3b89b94 (D-D) | SETTLED |
| type/attributes | the family is an independent rurl contract, but P3.1 does not name an exact restoration strategy for data-frame subclasses/attributes or zero-row prototypes | — (see Open cells KJ-O8) | OPEN |
| conditions | stable typed conditions for invalid input, relationship, unmatched, suffix/name collision, and policy conflict; representative key/count data must not reveal credentials | P3.1@3b89b94 (D-D) | SETTLED |
| resource bound | preflight relationship/cardinality before result expansion | P3.1@3b89b94 (D-D) | SETTLED |
| diagnostics | companion result/path carries per-row parse/keyability facts; diagnostics never encoded in a URL-looking display key | P3.1@3b89b94 (D-C, D-D) | SETTLED |

## `canonical_join()` migration rows

`canonical_join()`'s shipped `clean_url` equality is
**LEGACY / COMPATIBILITY-ONLY**. It is not reinterpreted as the v3 identity
model. The new family defaults to the independent identity key.

| phase / surface | contract | owner_decision_ref | status |
|---|---|---|---|
| legacy freeze | retain `canonical_join()` for a documented deprecation window; expose its old equality as an explicit legacy key policy | P3.1@3b89b94 (D-E; ratification Q7/B7) | SETTLED |
| implicit equality dials | close unrestricted `...` as an equality surface; presentation/cleaning arguments warn rather than silently changing matches | P3.1@3b89b94 (D-E; ratification Q7/B7) | SETTLED |
| identity opt-in | delegate `canonical_join(join = inner/left/right/full)` to the corresponding new family when caller selects identity policy | P3.1@3b89b94 (D-E) | SETTLED |
| six-join replacement | `url_inner_join`, `url_left_join`, `url_right_join`, `url_full_join`, `url_semi_join`, `url_anti_join` are the v3 identity-keyed family | P3.1@3b89b94 (D-D, D-E) | SETTLED |
| audit before switch | provide a dual-key comparison/audit surface that quantifies changed matches without rematching silently | P3.1@3b89b94 (D-E) | SETTLED |
| current formal migration | `join` maps to the named family; `collision` is replaced by relationship/multiple axes; `on_parse_error` splits into invalid/warnings; `join_parse_status` maps to typed eligibility policy | P3.1@3b89b94 (D-D, D-E) | SETTLED |
| forwarded parse dials | `url_standard`, `scheme_acceptance`, and `scheme_policy` may participate only through explicit interpretation/key-policy fields | P3.1@3b89b94 (D-E) | SETTLED |
| forwarded display/cleaning dials | protocol/www/case/host/path/query/port presentation, `path_encoding`, and `profile` do not affect identity; legacy-policy calls preserve old results during the window and otherwise warn | P3.1@3b89b94 (D-A.3, D-E; ratification Q7/B7) | SETTLED |
| path-encoding regression | revise the shipped forwarding test to assert key invariance | P3.1@3b89b94 (ratification Q7/B7) | SETTLED |
| removal/default flip | later versioned step outside this contract; not part of v3.0 | P3.1@3b89b94 (D-E) | SETTLED |

## Scope boundaries

This contract is the **single writer** of comparison-key policy, key
representation, URL-join row semantics, and `canonical_join()` migration.
Other artifacts consume it without redefining equality:

- **Canonical fields and structural kinds** — artifact 3 / G3.3. This contract
  consumes `authority_kind`, `host_kind`, path/query kinds, identity spellings,
  and layered verdict fields. It does not rename or add canonical state.
- **Output capability classification** — artifact 7 / G3.7. That artifact may
  classify the comparison key as safe for comparison/joining and `clean_url` as
  intentionally lossy; it references this key contract and does not define key
  fields or equality again.
- **Cleaning/mutation** — artifact 8 / G3.8. Cleaning can produce display or
  redirect-map outputs but cannot alter the default comparison key.
- **Host/IDNA/PSL/IP identity internals** — artifact 10 / G3.H. That artifact
  owns how domain/IP identities and external provenance are produced. This
  contract owns only how the fixed identity inputs participate in equality.
- **Executable fixtures** — artifact 11 / G4. The S5 minimum suite (vector,
  injectivity, non-interference, scheme/port/host/path/query,
  fragment/userinfo, eligibility, six joins, relationship, names/prototypes,
  migration) verifies these rows; it does not choose their semantics.
- **Implementation and public release timing** are later product slices. This
  contract specifies the surfaces; it does not add exports or change shipped
  `canonical_join()` behavior.

## Open cells

P3.1 did not settle these exact choices. They are recorded rather than filled
by invention. Each blocks only the named implementation detail; the settled
identity-before-presentation contract remains usable by downstream drafting.

- **KJ-O1 — default parsing standard for `url_key_policy()`.** P3.1 D-B
  requires an explicit standard field with a stable owner-selected default but
  names no default. **Impact:** default key bytes cannot be frozen.
  **Settles at:** a dedicated P3 key/join closure decision before implementation.
- **KJ-O2 — trailing-root-dot domain equivalence.** P3.1 consumes normalized
  domain identity but does not say whether a terminal DNS root dot compares
  equal to its undotted spelling. **Impact:** one host-policy cell and fixtures
  remain open. **Settles at:** the same P3 closure decision, coordinated with
  G3.H's host identity vocabulary.
- **KJ-O3 — exact `url_right_join()` stable order.** P3.1 adopts an independent
  rurl contract but does not choose the S5 fork between a y-primary mirror and
  another pinned order. **Impact:** exact right-join output order and fixtures
  cannot be frozen. **Settles at:** the P3 closure decision.
- **KJ-O4 — non-keyable x rows in `url_anti_join()`.** S5 recommends retaining
  them as unmatched but marks owner confirmation required; P3.1 does not
  confirm it. **Impact:** anti-join invalid/missing default remains open.
  **Settles at:** the P3 closure decision.
- **KJ-O5 — public URL-column selector signature.** P3.1 fixes one URL column
  per side for the first release but not whether the API exposes `by`, separate
  `col_x`/`col_y`, or both. **Impact:** semantics are stable, public formals are
  not. **Settles at:** the P3 closure decision, then G3.4 records the exports.
- **KJ-O6 — suffix and duplicate-name repair.** P3.1 requires deterministic
  behavior and early rejection of ambiguity but does not choose strict distinct
  suffixes versus one repair algorithm. **Impact:** exact result prototypes for
  overlapping/duplicate column names remain open. **Settles at:** the P3
  closure decision.
- **KJ-O7 — comparison-key visibility default.** P3.1 requires any visible key
  to use a collision-proof explicit name and the classed non-URL value, but
  leaves the default at "hidden or explicit." **Impact:** the default output
  schema has one unresolved column. **Settles at:** the P3 closure decision.
- **KJ-O8 — type restoration and zero-row prototypes.** P3.1 chooses an
  independent rurl join contract but does not fix whether data-frame subclasses
  and non-column attributes are restored, nor the exact typed prototype for
  every zero-row/no-match result. **Impact:** type/attribute promises and empty
  fixtures remain open. **Settles at:** the P3 closure decision.
