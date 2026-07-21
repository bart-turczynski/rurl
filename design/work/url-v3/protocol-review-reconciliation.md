# rurl 3.0 reconstruction protocol: joint review reconciliation

- **Status:** protocol rejected as written; amendment and re-review required
- **Reconciled:** 2026-07-22
- **Tracker:** RURL-jairsufw, child of RURL-gxqdmpcp; depends on completed review issue RURL-ulatmylm
- **Repository baseline:** `89be90bc1497507155ee9210dc6533020f127fc2`
- **Scope:** reconciliation of the nine frozen protocol reviews; this is not the rurl 3.0 product specification

## 1. Joint disposition

Do not start the successor product-specification draft from the reviewed
protocol.

The protocol has two sound foundations: its evidence-authority order, and its
separation of shipped behavior from proposed v3 behavior. Those foundations do
not yet make the reconstruction repeatable or the resulting claims testable.
All nine reviews independently found blocking omissions. The process red team
then demonstrated that a fresh clone cannot recover the protocol, its review
bundle, the decision state, or a frozen source baseline because the control
artifacts live under ignored `_scratch/` paths.

The protocol therefore has the following reconciled status:

1. **Retain** the authority hierarchy and shipped-versus-proposed distinction.
2. **Amend** the protocol with the durable control plane, artifact schemas,
   exhaustive contract matrices, evidence lifecycle, and executable gates in
   this document.
3. **Keep product choices open.** Reviewer recommendations are evidence and
   proposed defaults, not owner decisions.
4. **Re-run review** against the amended, tracked protocol and a frozen
   manifest.
5. **Draft no product specification** until every pre-draft gate in section 7
   is satisfied.

This disposition supersedes any reading of “review collection complete” as
permission to draft the product specification. It does not supersede accepted
ADRs, shipped behavior, or owner decisions.

## 2. Frozen review bundle

The reconciliation used the protocol, predecessor draft, and nine reports in
`_scratch/`. They remain unchanged. Their hashes record the exact local inputs,
but do not make those ignored files recoverable from a fresh clone; durable
graduation is a required next step.

| Input | SHA-256 |
|---|---|
| `url-v3-spec-reconstruction-protocol.md` | `10eb18b8a90d1219fd8f4f74cf8c73b39d11bd32ca005a02132bf07daf4defe5` |
| `prd-url-object-grammar-DRAFT.md` | `b0141cf9cca0c0cf964c068d90d9f8da5e3264e61ebe1f504905b6513ee6de41` |
| `S1-object-state-vector.md` | `dfde4d9d0b1bf0933732ce43b4d8dbed2bf191b1aadeb5a238d9330e4d0097be` |
| `S2-standards-validation-repair.md` | `b9301eecf2a7dcf41bf2b820e0fc0a6628e6e037a61db2ec011cbd40ed929bbd` |
| `S3-serialization-format-encoding.md` | `3c2e1170b62fcb4a1b77a9e52aa494fcd6ed9d369cbd6c15c7271fc15ff1e980` |
| `S4-cleaning-mutation.md` | `567d59cc3429fafb9fa4149aa77e5c12c67ebe9cb54bf0255a43fdd5aab09416` |
| `S5-keys-joins.md` | `bd1506841a495a77c827dd715d7fb7a31e6afb864a0ae138a304e92a62b182e9` |
| `S6-schemes-email.md` | `bc70876a6ec2af42411bc783ac30914893fa06a15cc275337ff982c2c0e05c99` |
| `S7-host-idna-psl-ip.md` | `8fb62419099dbe25d5c8d70ebae050f41fec67f15b0d7f19714157acd1c55636` |
| `S8-performance-curl-conformance-migration.md` | `f1d3f985f67d4b90382866a839ce1d6cc9155cfa53f395a144fb9278de75571b` |
| `S9-process-red-team.md` | `fc308055fc5a1465f1da942dba910ad43cd5091cac5959315dbb7f9429effa76` |

The nine reports total 2,974 lines and 399,102 bytes. Report references below
use their stable finding identifiers rather than treating line numbers as an
interface.

## 3. Reconciliation rules

The synthesis applies four classifications:

- **Verified current behavior:** observed directly in tracked source, tests,
  configuration, or tracker state at the baseline above.
- **Accepted authority:** an accepted ADR or durable accepted PRD, subject to
  explicit supersession and any recorded contradiction.
- **Reconciled protocol requirement:** a process or evidence requirement that
  must be added before drafting. It does not choose product behavior.
- **Open owner decision:** a product, compatibility, release, or public-API
  choice that reviewers cannot settle.

When reports overlap, the reconciliation merges the requirement and preserves
all source finding IDs. When recommendations conflict, the document records the
fork instead of selecting a product outcome. A current implementation is never
promoted to v3 intent merely because it exists, and an accepted design is never
reported as shipped without implementation evidence.

## 4. Consolidated findings

### RCON-01 — The reconstruction control plane is not durable

**Severity:** blocker
**Sources:** S9 C1-C3, H1-H3, H7-H9, M1-M5; all reports indirectly

The ignored protocol, reports, run state, and predecessor draft cannot be
recovered from a fresh clone. There is no tracked manifest binding a commit,
source inventory, issue snapshot, artifact hashes, owner decisions, findings,
and supersession state. The highest-authority inputs are consequently the least
durable.

**Required amendment:** establish one tracked protocol workspace with a
machine-readable manifest; immutable owner-decision records; typed decision,
contradiction, finding, and source-claim registers; explicit state transitions;
single-writer ownership; and a fresh-clone restore drill. External sources must
carry stable identity, revision, retrieval date, and checksum where feasible.

### RCON-02 — The URL state model is not reconstructible

**Severity:** blocker
**Sources:** S1 F1, F5-F7, F9, F11; S3 F2, F4-F5; S4 F6-F10; S7 F1-F3, F7, F10

The protocol names components and absent/empty distinctions but requires no
canonical state schema or lifetime invariant. Current public and internal
representations cannot, by themselves, prove lossless reconstruction of source
spelling, delimiter presence, authority kind, credentials, ordered query
tokens, hierarchical versus opaque paths, IP source spelling, and annotations.

**Required amendment:** require a field-by-field canonical state contract that
defines type, cardinality, provenance, absent/present-empty/nonempty states,
raw/decoded/normalized forms, derived-versus-stored ownership, invalid-state
representation, cache behavior, and survival through parse, access, mutate,
serialize, clean, format, key, join, and reparse operations.

**Open owner decisions:** public object shape; source-byte and R encoding-label
guarantees; authority and path models; query representation; credential
lifetime and security; public versus internal raw state; compatibility
projections for existing fields.

### RCON-03 — Source reproduction, standards serialization, cleaning, and display are conflated

**Severity:** blocker
**Sources:** S1 F2; S2 S2-03, S2-07; S3 F1-F7; S4 F1-F5, F12-F13; S5 findings 3-6; S6 findings 3-4 and 9

The reviewed protocol does not define independent contracts for:

1. reproducing accepted source spelling;
2. serializing full URL state under a named standard;
3. producing a lossy, policy-driven clean value;
4. formatting an untrusted value safely for humans; and
5. producing a non-URL comparison key.

Current `clean_url`-style serializers omit fragments and credentials, and
cleaning dials can intentionally change or decode presentation. That surface
cannot serve silently as a faithful serializer, comparison identity, redirect
target, or conformance oracle.

**Required amendment:** define five separate output contracts, each with an
input-state requirement, exact output type, delimiter and credential policy,
encoding policy, safety properties, reparse guarantee, loss report, and
allowed downstream uses. Require full-string serializer fixtures rather than
component projections alone.

**Open owner decisions:** exact public API names; whether and how legacy
`get_clean_url()` changes; fragment and userinfo defaults; exact RFC syntax
versus normalization surfaces; Unicode/control/bidi display escaping; source
reproduction boundary.

### RCON-04 — Validation and recovery need independent verdicts and provenance

**Severity:** blocker
**Sources:** S2 S2-01-S2-03, S2-05-S2-09; S4 F3, F7, F11, F14-F15; S6 findings 5-7; S7 F3, F6, F8-F9

`parse_status` currently combines parse success with host/PSL-derived warning
states. The protocol does not require separate verdicts for source conformance,
parse success, scheme admission, host policy, optional annotation availability,
repaired-input conformance, mutation commit, or serialized-output conformance.
It also lacks an ordered record separating standards-mandated preprocessing,
normalization, dependency compatibility shims, product compatibility recovery,
browser repair, cleaning, and display transformation.

**Required amendment:** define a multi-verdict validation record and an ordered,
typed intervention ledger. Every transformation must expose category, reason,
affected span/component, before/after state, lossiness, authority, and whether
it changes conformance. Optional annotations must not silently determine core
syntax validity unless the owner explicitly chooses and documents that policy.

**Open owner decisions:** strict versus repairing entry points; fatality and
warning boundaries; validator breadth; repair authorization and ordering;
resolution algorithm by standard; profile and direct-call precedence;
annotation failure behavior.

### RCON-05 — Dials, profiles, schemes, and eligibility are not closed matrices

**Severity:** blocker
**Sources:** S1 F4, F8; S2 S2-04; S4 F4-F5, F11; S6 findings 1-4 and 8-10; S7 F1, F6-F8; S8 findings 4, 8-10

A chapter list cannot reconstruct the cross-product of public arguments,
profiles, standards, scheme admission, parser route, authority/host/path kind,
component eligibility, and override precedence. The current API has 29 exported
symbols, and the parse surface alone has many independent controls; partial
examples will omit behavior.

**Required amendment:** require mechanically checked registries for every
public function and behavior dial, plus normative matrices for profile
expansion, explicit override precedence, standard-by-scheme parser route,
component/accessor eligibility, unsupported/no-op/reject outcomes, warning and
error behavior, vectorization, and migration disposition.

**Open owner decisions:** v3 default admission; direct selectors versus bundled
profiles; `ftp`/`ftps`/`sftp` treatment and default ports; generic credentials;
scheme-relative inputs; `mailto:` parsing and diagnostics; DNS-like eligibility
for generic schemes; compatibility aliases and deprecations.

### RCON-06 — Cleaning and mutation lack a state-transition and transaction contract

**Severity:** blocker
**Sources:** S4 F1-F15; S1 F6-F7; S3 F4-F5; S7 F5, F8

The protocol proposes mutation without defining its input/result type,
operation vocabulary, transaction unit, ordering, parent/child conflict rules,
rollback behavior, eligibility, vector recycling, diagnostics, or preservation
of untouched state. Query mutation needs an ordered multimap model; path
mutation needs structured and opaque models; domain-derived mutation depends on
a reproducible PSL source. Credential mutation also requires explicit
redaction and serialization rules.

**Required amendment:** require a cleaning-semantics matrix, mutation
state-transition matrix, dependency graph, transaction contract, vocabulary
ledger, output-capability classification, and invariant suite. Each operation
must specify absent/empty/nonempty behavior, validation standard, repair and
profile interaction, lossy-change reporting, and whether its output is safe for
comparison, joins, routing, redirects, display, and reparsing.

**Open owner decisions:** immutable versus mutable surface; patch semantics;
row versus vector atomicity; query/path models; derived-field write rules; PSL
mutation policy; invalid-input recovery; credential storage and display.

### RCON-07 — Comparison keys and six join operations have no normative contract

**Severity:** blocker
**Sources:** S5 findings 1-9; S1 F6-F7; S3 F5-F6; S4 F13; S8 findings 4 and 8

The proposal names `get_url_key()` and dplyr-shaped joins without specifying key
type, injectivity boundary, standard/profile dependence, component-equivalence
defaults, invalid rows, missing values, warnings, duplicates, relationships,
row order, columns, suffixes, names, or zero-row prototypes. Shipped
`canonical_join()` derives its key from lossy `clean_url`; that is legacy
behavior, not the proposed v3 identity model.

**Required amendment:** require a key-policy matrix, scheme/default-port truth
table, versioned key representation, collision contract, and a six-operation
join matrix covering inner, left, right, full, semi, and anti joins. Define
validation, diagnostics, multiplicity, relationship assertions, ordering,
column/name repair, and migration from all legacy equality dials.

**Open owner decisions:** key defaults and representation; inclusion of scheme,
userinfo, fragment, query order, empty delimiters, and default ports; relative
and opaque keyability; warning/invalid behavior; dplyr dependency or independent
compatibility target; legacy `canonical_join()` transition.

### RCON-08 — Host, IDNA, PSL, DNS, and IP concepts need separate typed contracts

**Severity:** blocker
**Sources:** S7 F1-F10; S1 F5; S2 S2-05; S4 F14; S6 findings 6-7; S8 findings 9-10

The protocol overloads terms such as “domain” across syntax, URL-standard host
parsing, DNS shape, registrability, and public result type. It does not pin the
IDNA algorithm/options/order, PSL source identity or private-section policy,
IP source and normalized identity, annotation laziness and failure state, cache
keys, or external-oracle authority.

**Required amendment:** reserve unambiguous host vocabulary and require a host
semantic matrix across standard, scheme/parser route, authority, host kind, and
source form. Define typed annotation results independently of parse status;
pin IDNA processing; record PSL version/fingerprint and section policy; split
DNS syntax, product policy, and network resolution; define IP identity and
serialization; and specify cost, caching, dependency failure, and oracle
boundaries.

**Open owner decisions:** IDNA profile; host identity fields; PSL public API and
legacy `NA` projections; private suffix policy; network DNS in or outside core;
RFC reg-name policy; IP spelling preservation; display fallback.

### RCON-09 — Vectorization, performance, caches, and dependency removal are not measurable gates

**Severity:** blocker
**Sources:** S1 F3-F4, F10-F11; S4 F15; S5 findings 1-2 and 7-8; S8 findings 2-5 and 9-12

The protocol states performance and curl-removal goals without a complete
dependency inventory, negative gate, workload definitions, budgets, hardware
normalization, memory limits, chunk/worker policy, or public-surface vector
contract. Cache behavior is observable but not specified as semantically
transparent, bounded, deterministic, or versioned by external data.

**Required amendment:** require scalar/vector behavior for all exports; a curl
inventory and zero-reference gate covering runtime, metadata, namespace, tests,
tools, and installation; benchmark datasets and thresholds; allocation and peak
memory budgets; chunk/parallel support boundaries; optional-annotation cost;
query encoding parity; and a cache contract covering keys, bounds, eviction,
clearing, external-data versions, and cached/uncached equivalence.

**Open owner decisions:** scalar failure form and vector recovery; recycling;
performance budgets and reference environment; cache defaults; chunk and worker
support; PSL engine boundary; staged replacement of accepted Stage A/B and curl
compatibility seams.

### RCON-10 — Conformance, determinism, migration, and release claims are not executable

**Severity:** blocker
**Sources:** S1 F10; S2 S2-03, S2-06-S2-08; S3 F1; S6 findings 5-6 and 10; S7 F9; S8 findings 1, 6-8 and 13; S9 H4-H6, H10-H11

The protocol permits examples, component projections, and artifact collection
without requiring claim-level provenance or failing acceptance gates. The
current determinism workflow is explicitly evidence collection and does not
fail on cross-platform divergence. Migration is chapter-shaped rather than
complete across all exports, behavior dials, docs, and downstream contracts.
The stated CRAN prohibition also conflicts with the latest accepted release
disposition unless its target version is clarified.

**Required amendment:** require a source-claim register; oracle-classified and
metamorphic fixtures; full-string conformance assertions; a failing determinism
comparison gate with approved exceptions; public-API migration inventory;
documentation and deprecation gates; exact local/CI command mapping; and an
owner-approved release rule that names the affected release line.

**Open owner decisions:** conformance claim granularity; oracle authority and
snapshot policy; acceptable platform divergence; compatibility duration;
CRAN hold scope; exact graduation and release criteria.

## 5. Contradictions requiring explicit disposition

These are not editorial cleanup items. Each must receive a typed decision such
as `ACCEPTED`, `REJECTED`, `SUPERSEDED`, or `COMPATIBILITY-ONLY`, with authority,
rationale, affected claims, and invalidated artifacts.

| ID | Contradiction | Required disposition |
|---|---|---|
| C-01 | Accepted URL-standard PRD wrappers call the documents accepted/shipped while their internal status fields still say draft. | Correct the durable metadata or record the status field as superseded; classify claims section by section until then. |
| C-02 | Accepted browser-fixer prose and shipped phase ordering disagree. | Choose and trace one ordering; label the other superseded or compatibility-only. |
| C-03 | Direct RFC behavior recovers some repeated-`@` inputs while RFC-general rejects them. | Classify recovery as syntax, compatibility, or repair and bind it to an entry point. |
| C-04 | ADR 0012 requires fragment-preserving standard serialization while shipped clean serializers explicitly omit fragments. | Separate standard serialization from legacy clean output; decide whether the ADR work is unimplemented or superseded. |
| C-05 | ADR 0011 describes WHATWG encode behavior as lossy for encoded `/`, while shipped code preserves `%2F`. | Decide legacy compatibility and keep that dial out of the standards serializer contract. |
| C-06 | Accepted identity guidance separates identity from presentation, while shipped `canonical_join()` keys on `clean_url`. | Introduce the independent key contract and an explicit migration plan; do not reinterpret the shipped join as v3-compliant. |
| C-07 | `parse_status` includes PSL-derived warning states although v3 proposals describe syntax, policy, and optional facts as separate layers. | Decide the v3 status model and preserve or migrate legacy projections explicitly. |
| C-08 | Runtime initializes the full-parse cache with a 100,000-entry bound, while `README.Rmd` says it is unbounded by default. | Correct one side and add a documentation-consistency gate. |
| C-09 | The determinism workflow calls itself evidence collection, never fails on divergence, and runs automatically only for harness changes. | Add a separate failing acceptance comparison and approved-exception mechanism. |
| C-10 | The protocol prohibits CRAN release until curl-free v3, while accepted tracker history permits a v2.7 release after its determinism gate. | Name the release line governed by the hold and record the owner decision. |
| C-11 | Accepted general-parser scope allows arbitrary schemes, while older ADR 0004 records a closed web scheme set. | Apply ADR 0012's explicit supersession precisely and retain the old rule only where compatibility/default policy still calls for it. |
| C-12 | Historical drafts make strong conformance claims from projections while also acknowledging the missing faithful serialization surface. | Preserve them as historical evidence only; rebuild claims from full state and full-string outputs. |

## 6. Required amendment artifact set

The amended protocol must make the following artifacts normative and give each
one a schema, owner, lifecycle, completion rule, and tracked location:

1. **Manifest:** repository commit, source inventory, tracker snapshot, artifact
   hashes, tool versions, external revisions, and restore instructions.
2. **Authority records:** immutable owner decisions, accepted design records,
   source-claim register, decision ledger, contradiction register, and finding
   register.
3. **Canonical state contract:** fields, types, presence states, provenance,
   invariants, public projections, and lifecycle.
4. **Public surface registry:** all exports, arguments, behavior dials, defaults,
   interactions, vector/error behavior, docs, migration, and tests.
5. **Standard/profile/scheme matrices:** expansion, precedence, parser routes,
   eligibility, credentials, email, resolution, and diagnostics.
6. **Validation/intervention contract:** independent verdicts, ordered
   transformations, repair provenance, and output revalidation.
7. **Output contracts:** source reproduction, standard serialization, cleaning,
   safe display, and comparison keys, including capability classification.
8. **Cleaning/mutation contracts:** semantics matrix, state transitions,
   dependency graph, transaction behavior, security, and invariants.
9. **Key/join contracts:** versioned key policy, port/scheme truth tables,
   collision rules, six joins, and legacy migration.
10. **Host/annotation contracts:** host vocabulary/matrix, IDNA, PSL, DNS, IP,
    external provenance, cost, cache, and failure states.
11. **Verification contracts:** fixture catalog, oracle classifications,
    metamorphic properties, traceability map, benchmark budgets, determinism,
    curl-removal closure, migration, and exact CI commands.

Recommended record lifecycle:

`DISCOVERED -> PROPOSED -> CONFLICT | ACCEPTED | REJECTED -> TRANSFERRED -> VERIFIED`

`SUPERSEDED` and `INVALIDATED` are terminal annotations that must name the
replacement or invalidating source change. Only the owner may create an
`ACCEPTED` product decision. A finding closes only when its disposition and
verification evidence are recorded.

## 7. Pre-draft gate sequence

The successor product specification may start only after these gates pass in
order:

### G0 — Durability and restore

- The protocol, manifest, registers, and required review inputs are tracked.
- A fresh clone can reconstruct the declared baseline without local memory.
- One writer owns each mutable artifact and concurrent review inputs are frozen.

### G1 — Baseline and inventory closure

- The manifest pins the repository commit, issue snapshot, external inputs, and
  checksums.
- Every exported function, argument, internal behavior dial, accepted design
  claim, relevant test family, curl dependency, benchmark, and migration surface
  has an inventory disposition.
- Missing or inaccessible evidence is explicit, not silently omitted.

### G2 — Authority and contradiction closure

- Every contradiction in section 5 and every newly discovered contradiction has
  an owner disposition.
- Accepted, shipped, historical, proposed, and superseded claims remain
  distinguishable at assertion level.
- Source changes invalidate dependent decisions and reviews through recorded
  edges.

### G3 — Contract-matrix closure

- The artifact set in section 6 exists and contains no unowned cells.
- Open product cells have an owner decision or are explicitly deferred outside
  v3 scope with impact recorded.
- Cross-artifact terms, defaults, state fields, and status codes agree.

### G4 — Executable evidence

- Every normative claim maps to tests, fixtures, benchmarks, or a documented
  manual verification with exact commands.
- Oracles are labeled by authority and claim boundary.
- Full-string, state, vector, mutation, join, cache, determinism, and migration
  properties have positive and negative coverage.

### G5 — Independent re-review and acceptance

- One context-free reviewer checks comprehensibility and completeness.
- One evidence-aware reviewer checks every material citation and executable
  claim against the frozen baseline.
- Every review finding has a disposition and closure state.
- All amended protocol acceptance questions have objective yes/no evidence.

Only after G5 may the successor product-specification draft begin. Code changes
that would prematurely choose an open v3 product decision remain out of scope.

## 8. Owner decision queue, ordered by dependency

The reviews produced many detailed questions. They reconcile into the following
dependency-ordered queue; none is answered by this document.

### P0 — Process and release authority

- Durable workspace and manifest format.
- Identity and approval mechanism for owner decisions.
- Fiberplane topology, snapshot boundary, and who may mutate it during review.
- CRAN hold scope and exact v2/v3 release boundary.

### P1 — State and public object

- Canonical state schema and public/internal split.
- Scalar failure, vector recovery, recycling, and result type.
- Authority, credential, path, query, delimiter, source-byte, and encoding state.
- Parsed versus derived fields and compatibility projections.

### P2 — Standards, admission, validation, and outputs

- Default standard/scheme admission and profile/direct-call lattice.
- Strict, compatibility, and repair entry points and their ordering.
- Validation verdicts, fatality, annotations, and resolution.
- Source reproduction, standard serializers, clean output, and safe display.

### P3 — Mutation, identity, and joins

- Mutation API, transaction unit, dependencies, validation, and rollback.
- Key equivalence, representation, collisions, invalid rows, and versioning.
- Six-join semantics and `canonical_join()` migration.

### P4 — Scheme and host specializations

- Generic credentials, `mailto:` raw/decoded state, email diagnostics, and
  unresolved lexer contexts.
- FTP-family and scheme-relative posture.
- Host vocabulary, IDNA profile, PSL policy/provenance, DNS boundary, and IP
  source/identity behavior.

### P5 — Operational contract

- Performance and memory budgets; chunk/worker boundary.
- Cache defaults and semantic-transparency rules.
- Curl-removal sequence and closure gate.
- Oracle policy, determinism tolerance, migration window, and graduation gates.

## 9. Finding coverage map

Every severity-ranked finding in the nine reports is represented below. This
map prevents consolidation from silently dropping a narrow review result.

| Report findings | Reconciled location |
|---|---|
| S1 F1, F5-F7, F9, F11 | RCON-02 |
| S1 F2 | RCON-03 |
| S1 F3-F4, F8, F10 | RCON-05, RCON-09, RCON-10 |
| S2 S2-01-S2-02, S2-05, S2-08-S2-09 | RCON-04 |
| S2 S2-03, S2-06-S2-07 | RCON-03, RCON-10 |
| S2 S2-04 | RCON-05 |
| S3 F1-F7 | RCON-02, RCON-03, RCON-10 |
| S4 F1-F5, F11-F14 | RCON-03, RCON-04, RCON-06 |
| S4 F6-F10, F15 | RCON-02, RCON-06, RCON-09 |
| S5 findings 1-9 | RCON-03, RCON-07, RCON-09 |
| S6 findings 1-4, 8-10 | RCON-03, RCON-05 |
| S6 findings 5-7 | RCON-04, RCON-10 |
| S7 F1-F3, F6-F10 | RCON-02, RCON-04, RCON-05, RCON-08 |
| S7 F4-F5 | RCON-08 |
| S8 findings 1, 6-8, 13 | RCON-10 |
| S8 findings 2-5, 9-12 | RCON-05, RCON-09 |
| S9 C1-C3, H1-H3, H7-H9, M1-M5 | RCON-01 |
| S9 H4-H6, H10-H11 | RCON-10 and gates G4-G5 |

The detailed owner questions, fixture families, and matrix skeletons remain in
the frozen reports and must be transferred—not summarized from memory—when the
corresponding tracked artifact is created.

## 10. Direct verification and evidence corrections

The reconciliation rechecked the most load-bearing claims at the recorded
baseline:

| Claim | Direct evidence | Result |
|---|---|---|
| The reconstruction control plane is ignored | `.gitignore` and `git check-ignore` for `_scratch/` protocol, reports, and run state | Verified |
| Public parsed output is narrower than internal state | `R/utils.R` public-field and Stage A templates | Verified: 18 public fields versus 20 Stage A fields, with additional state elsewhere |
| Current clean serializers omit fragments and do not reconstruct credentials | `R/parse-phases.R` serializer functions and comments | Verified |
| Current join identity is `clean_url` | `R/canonical_join.R` key construction | Verified |
| `parse_status` depends on PSL-derived `tld` and `domain` warnings | `.derive_parse_status_vec()` in `R/parse-phases.R` | Verified |
| Public-surface inventory cannot stop at parse functions | `NAMESPACE` | Verified: 29 exports at the baseline |
| Runtime and cache documentation disagree | `.onLoad()`/`rurl_cache_config()` in `R/zzz.R`; caching section in `README.Rmd` | Verified: runtime bound 100,000; README says unbounded default |
| Determinism is not currently a divergence gate | `.github/workflows/determinism-probe.yml` | Verified by explicit workflow comments and narrow triggers |
| curl removal needs more than replacing the parser call | `DESCRIPTION`, `NAMESPACE`, `R/`, and tests | Verified: metadata imports plus parsing, path/query, accessor, email, and test references |

Two evidence hygiene corrections apply when findings are transferred:

1. S4 uses shortened ADR paths that do not exist. The tracked files are
   [`0011-path-encoding-orthogonal-presentation.md`](../../adr/0011-path-encoding-orthogonal-presentation.md)
   and [`0012-general-url-parser-scope.md`](../../adr/0012-general-url-parser-scope.md).
2. S9's Fiberplane status is a point-in-time observation. At the frozen review
   collection boundary, RURL-ulatmylm was completed; this reconciliation is
   tracked separately as RURL-jairsufw.

These corrections do not change either report's substantive findings.

## 11. Next frontier

The next slice is protocol hardening, not product-spec drafting:

1. graduate the protocol and necessary frozen evidence into a tracked manifest;
2. define the record schemas and lifecycle;
3. resolve P0 owner decisions;
4. build and close the exhaustive inventories and contradiction register;
5. amend the protocol with the artifact and gate requirements above; and
6. perform the dual re-review in G5.

The nine original reports remain frozen evidence inputs. This reconciliation is
their joint disposition and coverage index, not a replacement for their detailed
implementation paths, test proposals, or owner-question lists.
