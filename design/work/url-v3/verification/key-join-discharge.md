# Verification — key and join discharge (VD-001)

<!-- Verification artifact, discharge record. NARROW BY CONSTRUCTION: this file
     discharges exactly one deferral, VD-001, by mapping the 51 cells it
     deferred onto executable evidence that now ships. It is NOT the join-family
     verification slice proper — the full artifact-11 map for
     contracts/key-join-contracts.md, including the `canonical_join()` migration
     rows, remains a later G4 leaf. Authoring the whole slice here to close one
     row would manufacture G4 scope, which is the failure cache-slice.md's own
     envelope warns against.

     Why this record exists at all: P0.5 failure condition 3 fires the moment a
     deferred surface ships. VD-001's eight exports shipped with RURL-mihbyjsr,
     so the row cannot stay ACCEPTED; and a DISCHARGED row must be claimed by a
     verification slice (deferral-gate rule D2) or the gate fails. This record
     is that claim, and nothing more. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-key-join-discharge |
| name | verification-key-join-discharge |
| artifact_number | 11 (discharge record; the join-family slice proper is a later G4 leaf) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/key-join-discharge.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| verifies | VD-001 (registers/verification-deferrals.md); P3.1, P3.2, P3.3 (authoritative decisions); contracts/key-join-contracts.md — the 51 cells VD-001 enumerates, and no others |
| dependencies | P0.5 (the deferral register and its gate); P3.1 (bound decision); P3.2 (the closure decision settling KJ-O1..KJ-O8); P3.3 (row 6 amendment, the dropped `unmatched` condition); contracts/key-join-contracts.md (the normative source) |
| closes_finding | VD-001 |
| completion_rule | every cell named by VD-001 maps to shipped executable evidence, cited as `path :: test name`, with the evidence present on `main` rather than promised; residual cells are named as residuals and never counted as covered; `tools/deferral-gate.R` reports D2 and D3 PASS |
| approval_evidence | pending — rides the owner's merge of the PR carrying it |
| validation_command | Rscript tools/deferral-gate.R && Rscript tools/verify.R |

## DISCHARGED[VD-001]

VD-001 deferred the key and join cells on an explicit, checkable premise — in
the register's own words, that the surface is "SETTLED by P3.1/P3.2 but
**unbuilt**", and that "no test can assert a property of an **absent**
function". That premise expired. `RURL-mihbyjsr` shipped the key engine
(`8f32ac2`), the six-join engine (`eba6f6b`) and the relaxed scheme mode
(`c324252`) **unexported** — the VD-002 shape, invisible to an `export:` probe —
and then shipped all eight public wrappers in ONE change together with this
record. Every one of VD-001's eight `surface_probe` entries now resolves present
in `NAMESPACE`, which is precisely the condition P0.5 failure condition 3 names.

The register's rule is that the row dies when its surface arrives, not when a
date passes. This record is that death certificate: the cells are no longer
excused, they are **covered** — with two residuals named in full below and
counted as residuals, never as coverage.

## Where the 51 cells come from

VD-001's `cells` column names five blocks by size rather than by id, because the
contract numbers them as blocks. Each block is enumerated here against the
contract's own tables, so the arithmetic is **derived and checkable**, not
transcribed:

| block | contract table | rows | cells |
|---|---|---:|---:|
| key surface | `## Key surface rows` (`:65-71`) | 7 | 7 |
| key-policy | `## Key-policy rows` (`:80-95`) | 16 | 16 |
| scheme/port truth table | `## Scheme and port truth table` (`:105-118`) | 14 | 14 |
| six join operations | `## Six-join matrix` (`:140-145`) | 6 | 6 |
| cross-cutting | `## Cross-cutting join rows` (`:151-165`) | 15 | 8 |
| | | | **51** |

The cross-cutting block is the only one that is not a whole table, so its
selection is stated rather than assumed. Of its 15 rows, 4 were the OPEN cells
`KJ-O5` (`URL columns`), `KJ-O6` (`suffix/name repair`), `KJ-O7`
(`key visibility`) and `KJ-O8` (`type/attributes`) **when VD-001 was written**,
which is what put them outside its cell set; and 3 are counted in other blocks
(`key policy` and `parse policy` are the key surface's `url_key_policy` and
`non-interference` rows; `row order except right join` is the six-join matrix's
own order column). 15 − 4 − 3 = **8**: `relationship`, `multiple matches`,
`duplicate counts`, `invalid/warnings`, `original URLs`, `conditions`,
`resource bound`, `diagnostics` — which is exactly the register's phrase
"relationship/multiplicity/resource/typed-condition/diagnostics rows".

Those four cells are **not** part of VD-001's 51 and are not claimed by this
record's count. P3.2 D-E..D-H closed them after VD-001 was written, and
`RURL-ojrtnnhy` then applied that closure to the contract — so their `status`
column now reads `SETTLED`, and the "4" above is a fact about the contract as
VD-001 found it, not about its current bytes. They are implemented and tested;
their evidence is listed in a separate section below so a reader is not left
wondering whether they shipped untested.

## Cell → evidence map

Evidence lives in three files, all of which run in the standard `devtools::test()`
chain and under `Rscript tools/verify.R`:

* `tests/testthat/test-url-key.R` — the key engine (**148** passing).
* `tests/testthat/test-url-join.R` — the join engine (**173** passing).
* `tests/testthat/test-url-key-join-api.R` — the eight exported wrappers
  (**60** passing). The engines are pinned by the first two; this third file
  pins the only thing a thin wrapper can get wrong, the **signature**.

### Block 1 — key surface (7 cells, contract `:65-71`)

| cell | evidence | coverage |
|---|---|---|
| `get_url_key(url, policy = url_key_policy())` | `test-url-key.R :: "the key is length-preserving and names-preserving"`; `:: "a zero-length input yields a zero-length key, not an error"`; `:: "factor input is accepted under explicit coercion"`; `:: "the policy is not silently recycled and must be a policy object"`; `test-url-key-join-api.R :: "the exported wrappers add no behavior over the engines"` | length/names preservation, explicit factor coercion, scalar non-recycled policy, and byte-identity between the export and the engine |
| `url_key_policy()` | `test-url-key-join-api.R :: "url_key_policy validates its vocabulary at the public edge"`; `test-url-key.R :: "keys minted under different policies never compare equal"` | the immutable versioned object, its closed vocabulary, and the symmetry consequence |
| key representation | `test-url-key.R :: "the key is a classed non-URL object carrying both versions"`; `:: "the printable form is diagnostics-only and never a URL"`; `:: "adversarial component payloads cannot collide"`; `:: "distinct component tuples do not collide across a large sample"` | classed value, both versions in metadata, injective length-prefixed framing proven against payloads carrying separators and control bytes |
| identity input | `test-url-key.R :: "the key is not derived from clean_url"`; `test-url-join.R :: "matching is by identity key, not by cleaned display string"` | the key consumes the canonical identity record, never surface (c) |
| non-interference | `test-url-key.R :: "no presentation dial can reach the key surface"`; `:: "the key posture differs from surface (b)'s in exactly one axis"`; `test-url-key-join-api.R :: "no public cleaning or profile dial can reach the exported key"`; `test-url-join.R :: "the family has no presentation dials to forward"` | the load-bearing D-A.3 invariant, asserted at the engine AND at the export, where the surface simply has no such argument to pass |
| interpretation selector (KJ-O1) | `test-url-key.R :: "the policy default standard is whatwg (P3.2 D-A)"`; `:: "selecting a different standard changes the key"`; `:: "an unnamed standard is rejected -- key bytes must be freezable"` | KJ-O1's default, closed by P3.2 D-A; `NULL` is refused at the public edge too (`test-url-key-join-api.R :: "url_key_policy validates its vocabulary at the public edge"`) |
| diagnostic surface | `test-url-key.R :: "non-keyable rows are NA plus a typed reason"`; `:: "a credential never appears in the key or its display"`; `test-url-join.R :: "the eligibility vocabulary is projected, one label per row"` | typed reasons on a companion path, never a bare `NA`; no credential in a key or a diagnostic |

### Block 2 — key-policy rows (16 cells, contract `:80-95`)

| cell | evidence | coverage |
|---|---|---|
| scheme source (`:80`) | `test-url-key.R :: "a host:port input frames as a missing (inferred) scheme"`; `:: "an explicit scheme still frames as explicit"`; `:: "scheme-relative stays its own presence state, never missing"` | presence framed independently of the effective scheme, so inference never exists only to erase missing |
| scheme equality (`:81`) | `test-url-key.R :: "http_https is accepted and recorded in the policy"`; `:: "both standards agree on the whole http_https column"`; `:: "the collapse never reaches a non-web scheme pair"`; `:: "the collapsed token cannot be spelled by any real scheme"`; `:: "exact stays exact -- the mode cannot leak across policies"` | `exact` and `http_https` in full. `http_https_missing` is a **residual** — see below |
| scheme case (`:82`) | `test-url-key.R :: "scheme and host case are normalized case-insensitively"` | normalized case-insensitive identity |
| port (`:83`) | `test-url-key.R :: "HTTP and HTTPS default ports normalize against absent"`; `:: "a non-default port stays significant"`; `:: "another scheme's default port stays significant"`; `:: "ftp, ws, wss and custom default ports stay literal in v1"` | HTTP/S-only normalization under each row's own explicit scheme (ratification Q3/Q4/Q8) |
| authority (`:84`) | `test-url-key.R :: "the authority delimiter is framed independently of the host"`; `:: "delimiter framing inherits the standard's own fix-ups"` | P1.2 D-A's `authority_delimiter_present` framed independently of `host_kind`; absent and empty never collapsed; the delimiter fact comes from the parse, so WHATWG's special-scheme slash fix-up is inherited rather than re-litigated |
| host kind (`:85`) | `test-url-key.R :: "a Unicode host and its A-label share one key"` | one identity per host, whatever its display spelling |
| domain spelling (KJ-O2, `:86`) | `test-url-key.R :: "the trailing root dot is key-significant (P3.2 D-B)"` | KJ-O2's root-dot question, closed DISTINCT by P3.2 D-B |
| host editing (`:87`) | `test-url-key.R :: "host editing is excluded from the key"` | `www`, subdomain/PSL trimming and presentation encoding excluded |
| path (`:88`) | `test-url-key.R :: "reserved encoded path bytes remain data"` | `%2F` never becomes `/` merely for comparison |
| path display/editing (`:89`) | `test-url-key.R :: "path display and editing are excluded from the key"` | index removal, trailing slash and case cleanup excluded |
| query presence (`:90`) | `test-url-key.R :: "query presence is three-valued"` | absent, present-empty and present-nonempty stay distinct |
| query structure (`:91`) | `test-url-key.R :: "query order and duplicates are significant"` | URL-query semantics, not form semantics |
| fragment (`:92`) | `test-url-key.R :: "the fragment is ignored for web-resource identity"`; `test-url-join.R :: "fragment and userinfo are invisible to matching (Q5)"` | ratification Q5, at the key and through a join |
| userinfo (`:93`) | `test-url-key.R :: "userinfo is ignored for web-resource identity"`; `:: "a credential never appears in the key or its display"` | Q5, plus the security consequence |
| missing/invalid (`:94`) | `test-url-key.R :: "non-keyable rows never match each other (never-match default)"`; `:: "missing input is never conflated with an invalid parse"` | never-match default; the one conflation D-C forbids is prevented by construction |
| persisted-key stability (`:95`) | `test-url-key.R :: "the key is a classed non-URL object carrying both versions"`; `:: "keys minted under different policies never compare equal"` | both versions ride inside the framed bytes, so no release can silently reinterpret a persisted key |

### Block 3 — scheme/port truth table (14 cells, contract `:105-118`)

Each row is one cell. The `exact` and `http_https` columns are both covered;
`http_https_missing` is the residual named below.

| row | left / right | evidence |
|---:|---|---|
| 1 | HTTP absent port / HTTP `:80` | `test-url-key.R :: "HTTP and HTTPS default ports normalize against absent"`; `:: "both standards agree on the whole http_https column"` |
| 2 | HTTPS absent port / HTTPS `:443` | same pair |
| 3 | HTTP absent port / HTTP `:8080` | `test-url-key.R :: "a non-default port stays significant"`; `:: "both standards agree on the whole http_https column"` |
| 4 | HTTPS absent port / HTTPS `:80` | `test-url-key.R :: "another scheme's default port stays significant"`; `:: "ports are normalized under the row's own scheme, then collapsed"` |
| 5 | HTTP `:443` / HTTPS absent | `test-url-key.R :: "another scheme's default port stays significant"` |
| 6 | HTTP `:80` / HTTPS `:443` | `test-url-key.R :: "HTTP and HTTPS are distinct under exact scheme equality"`; `:: "the whole transitive class collapses to exactly one key"` — the relaxed cells amended to `equal` by P3.3@d9b0976 |
| 7 | HTTP absent / HTTPS absent | `test-url-key.R :: "HTTP and HTTPS are distinct under exact scheme equality"`; `:: "both standards agree on the whole http_https column"` |
| 8 | missing scheme/no port / HTTP no port | `test-url-key.R :: "a missing scheme is distinct from an explicit one"` (`exact`, `http_https`); the `http_https_missing` cell is the residual |
| 9 | missing scheme `:80` / HTTP no port | `test-url-key.R :: "a missing scheme does not guess an effective scheme for its port"`; `:: "a missing-scheme port is never normalized away (Q4, rows 9-10)"` |
| 10 | missing scheme `:443` / HTTPS no port | same pair |
| 11 | scheme-relative/no port / missing host-shaped/no port | `test-url-key.R :: "scheme-relative is its own kind, not 'missing'"`; `:: "scheme-relative stays its own presence state, never missing"` — see the residual note on the rfc3986 posture |
| 12 | FTP `:21` / FTP no port | `test-url-key.R :: "ftp, ws, wss and custom default ports stay literal in v1"` |
| 13 | WS `:80` / WSS `:443` vs same scheme absent | same test; plus `:: "the collapse never reaches a non-web scheme pair"`, which is the ws/wss trap: they share http/https's default ports and must NOT collapse |
| 14 | custom `:123` / same custom scheme no port | `test-url-key.R :: "ftp, ws, wss and custom default ports stay literal in v1"` |

### Block 4 — the six join operations (6 cells, contract `:140-145`)

| operation | evidence | coverage |
|---|---|---|
| `url_inner_join` | `test-url-join.R :: "inner join keeps matching pairs in x order, y order within x"` | retention, multiplicity and the pinned order |
| `url_left_join` | `test-url-join.R :: "left join keeps every x row and expands matches"`; `:: "the missing payload uses x's own column types, not logical NA"` | every x row; typed missing payload |
| `url_right_join` (KJ-O3) | `test-url-join.R :: "right join is the y-primary mirror of left (P3.2 D-C)"`; `:: "right join is the exact reflection of left with sides swapped"` | KJ-O3's fork, closed y-primary by P3.2 D-C, asserted as a reflection rather than as a second hand-written order |
| `url_full_join` | `test-url-join.R :: "full join is the left result then unmatched y rows in y order"`; `:: "a narrowed y row surfaces as unmatched in a full join"` | both halves, including under the lossy `multiple` narrowings |
| `url_semi_join` | `test-url-join.R :: "semi join emits each matching x row once, in x order"`; `:: "semi and anti emit x columns only and never suffix"` | one output per matching x, x columns only |
| `url_anti_join` (KJ-O4) | `test-url-join.R :: "anti join keeps unmatched x once, INCLUDING non-keyable (D-D)"` | KJ-O4's question, closed RETAIN by P3.2 D-D: a row that cannot match anything is exactly the anti-join predicate |

### Block 5 — cross-cutting rows (8 cells, contract `:151-165`)

| cell | evidence | coverage |
|---|---|---|
| relationship (`:154`) | `test-url-join.R :: "relationship defaults to none and does not constrain"`; `:: "one-to-one rejects duplicate keys on either side"`; `:: "one-to-many constrains x, many-to-one constrains y"`; `:: "many-to-many allows expansion when declared explicitly"`; `:: "relationship is checked only on keys present on BOTH sides"` | all five values, over eligible keys only |
| multiple matches (`:155`) | `test-url-join.R :: "multiple defaults to all and expands duplicates losslessly"`; `:: "multiple = first/last narrow to a defined stable order"`; `:: "multiple narrowing does not reuse the legacy collision dial"` | lossless default; the lossy narrowings separately named, with a stable order, and NOT the shipped `collision` dial |
| duplicate counts (`:156`) | `test-url-join.R :: "the relationship error reports counts and leaks no credentials"` | counts over eligible non-missing keys, reported separately |
| invalid/warnings (`:157`) | `test-url-join.R :: "invalid = keep retains non-keyable rows per the join's own rule"`; `:: "invalid = drop removes non-keyable rows before matching"`; `:: "invalid = error stops with a typed condition naming the reasons"`; `:: "warnings = allow lets warning rows match (the default)"`; `:: "warnings = reject makes a warning row ineligible but keeps it"`; `:: "warnings = error stops with its own typed condition"`; `:: "the invalid and warnings axes are independent, not on_parse_error"` | both axes in full, and their independence, which is the point of splitting `on_parse_error` |
| original URLs (`:159`) | `test-url-join.R :: "both original URL columns are preserved, never overwritten (:159)"`; `:: "semi and anti emit x columns only and never suffix"` | both originals preserved; never overwritten with cleaned display |
| conditions (`:163`) | `test-url-join.R :: "all typed conditions subclass one family class"`; `:: "the relationship error reports counts and leaks no credentials"`; `:: "by rejects malformed selectors as an early input error"`; `:: "a colliding key_name is an early error, never a silent rename"` | the stable typed-condition family, and the no-credential-in-a-condition rule. The `unmatched` condition is **dropped**, not implemented — see residual 2 |
| resource bound (`:164`) | `test-url-join.R :: "the relationship preflight runs before materialization"` | the preflight is a preflight: it runs on counts, before expansion |
| diagnostics (`:165`) | `test-url-join.R :: "the eligibility vocabulary is projected, one label per row"`; `:: "unsupported-scheme is enumerated but unreachable here"`; `:: "key_name exposes the CLASSED key, never a URL-looking string"` | per-row keyability facts on a companion path; never encoded in a URL-looking display key. The vocabulary's zero-instance term is ASSERTED as a gap rather than silently narrowed |

## Residuals — named, and NOT counted as coverage

Two cells of the 51 are covered by an assertion about a **refusal** rather than
by an assertion about an answer. That is a real difference and it is recorded
as one, because the register's own rule is that deferral is not satisfaction.

**Residual 1 — `scheme_equality = "http_https_missing"` (block 2 `:81`;
block 3 rows 8 and 11).** The mode is in the settled vocabulary and is
**refused**: `url_key_policy(scheme_equality = "http_https_missing")` errors,
citing `RURL-ixxvjjwj`. The grounds are contractual, not incidental. Row 8
declares `missing scheme/no port` equal to `HTTP/no port`, but that pair also
differs on `authority_delimiter_present`, which P1.2 D-A frames as **independent
identity in a SETTLED contract** — satisfying row 8 needs a third collapse
reaching into another artifact's settled vocabulary. Row 11's scheme-relative
branch is defective on top of that (`RURL-kmkyicpt`: under `rfc3986`, `//p` and
`///p` mint byte-identical keys though they are distinct references).

What IS verified: the refusal, its reason and its citation
(`test-url-key.R :: "http_https_missing is refused, not guessed"`;
`test-url-key-join-api.R :: "http_https_missing refuses at the public edge too,
with its reason"`). Guessing an answer would have been the failure mode; the
mode errors instead of silently picking a side. **3 of the truth table's 14 rows
therefore carry a disposition the contract text does not yet reflect** (row 6's
two amended relaxed cells, row 8's `http_https_missing` cell, and row 11 across
all three columns): 6 of the 42 mode cells, spanning 3 rows. VD-001 counts rows,
so the outstanding contract-text edit is 3 rows, not 6 cells. That edit is
`RURL-isbsbrry`.

**Residual 2 — the `unmatched` typed condition (block 5 `:163`).** The contract
row lists `unmatched` among the required typed conditions. It is **dropped by
owner ruling** (P3.3@d9b0976 §2, `RURL-kcgsuzll`), not left unimplemented: the
six joins already express unmatched-ness structurally, as `NA` on the
non-primary side, which is what a join *is*. A condition or an axis would
re-report the result's own shape as a diagnostic, and a caller who wants the
count already has it (`is.na()` over the exposed key column, or `relationship`).
The other four condition classes the row names — invalid input, relationship,
suffix/name collision and policy conflict — all ship with reachable triggers and
are covered above. The same contract-text edit (`RURL-isbsbrry`) carries this
row.

## Beyond VD-001 — the four cross-cutting cells it left out

Not part of the 51 and not counted here, but shipped in the same slice, so their
evidence is recorded rather than left implicit. All four were OPEN when VD-001
was written and closed by P3.2 afterwards.

| cell | closed by | evidence |
|---|---|---|
| KJ-O5 — public URL-column selector | P3.2 D-E (`by`, the named-vector idiom) | `test-url-join.R :: "by accepts a bare string and a length-one named vector"`; `:: "the join column must be character or factor"`; `:: "a factor join column is accepted and keys as its labels"` |
| KJ-O6 — suffix and duplicate-name repair | P3.2 D-F (one deterministic algorithm, ambiguity an early error) | `test-url-join.R :: "overlapping non-key names are suffixed deterministically"`; `:: "suffix ambiguity is an early error, never a silent repair"`; `:: "duplicate input column names are rejected before any join work"`; `:: "suffix must be two non-NA strings"` |
| KJ-O7 — comparison-key visibility | P3.2 D-G (hidden by default, classed on opt-in) | `test-url-join.R :: "the comparison key is hidden by default"`; `:: "key_name exposes the CLASSED key, never a URL-looking string"`; `:: "an unmatched y row still reports its own key (coalesced)"`; `:: "key_name must be NULL or one non-NA non-empty string"` |
| KJ-O8 — type restoration and zero-row prototypes | P3.2 D-H (row-slice `x`'s prototype; one path for both) | `test-url-join.R :: "a zero-row result carries the complete typed would-be schema"`; `:: "an empty input side yields the typed zero-row schema"`; `:: "the result is built by row-slicing x's prototype (D-H.1)"`; `:: "row names are reset rather than carrying NA slice artifacts"`; `:: "both sides empty yields a typed zero-row result on every type"` |

## Boundary: what this record does NOT claim

- It does **not** verify the rest of `contracts/key-join-contracts.md`. The
  eleven `canonical_join()` migration rows (`:175-184`) are outside VD-001's
  cell set and are untouched here: `canonical_join()` still keys on `clean_url`,
  and the delegation, the `...` equality closure and the dual-key audit surface
  are the remaining work on `RURL-mihbyjsr`. Exporting the family does not
  migrate the legacy join, and this record does not pretend it did.
- It does **not** edit `contracts/key-join-contracts.md`. Three truth-table rows
  and the `conditions` row carry dispositions the contract text does not yet
  reflect; that edit is `RURL-isbsbrry` and it is a contract change, not a
  verification act.
- It does **not** discharge any other deferral. `VD-003` (output surface (d),
  `format_url`) stays `ACCEPTED`, its surface is still unshipped, and D3
  correctly stays silent about it.
- It makes **no product decision** and re-decides no key or join semantics.
  P3.1/P3.2/P3.3 remain the single writers; this record only records where their
  cells are executably verified.

## Inputs

| Path | Role |
|---|---|
| design/work/url-v3/registers/verification-deferrals.md | the register carrying VD-001 |
| design/work/url-v3/decisions/P0.5-g4-exit-criterion-scope.md | the disposition defining discharge |
| design/work/url-v3/contracts/key-join-contracts.md | the normative source of the 51 cells |
| design/work/url-v3/decisions/P3.1-identity-canonical-join.md | the bound decision |
| design/work/url-v3/decisions/P3.2-key-join-closure.md | closes KJ-O1..KJ-O8 |
| design/work/url-v3/decisions/P3.3-relaxed-scheme-equality.md | row 6 amendment; the dropped `unmatched` condition |
| tests/testthat/test-url-key.R | cited executable evidence (key engine) |
| tests/testthat/test-url-join.R | cited executable evidence (join engine) |
| tests/testthat/test-url-key-join-api.R | cited executable evidence (the eight exports) |
