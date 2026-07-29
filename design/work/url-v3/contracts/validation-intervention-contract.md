# Validation / intervention contract (§6 artifact 6)

<!-- Contract artifact (§6 artifact 6). RCON-04 closure. This record PROJECTS the
     ACCEPTED owner decisions P2.1 (fixer/recovery ordering + strict/compatibility/
     repair entry points; C-02/C-03 dispositions), P2.3 (validation-verdict
     semantics + legacy parse_status migration; the P2 half of C-07), and P1.1
     (the layered state/status MODEL + independence invariant; the P1 half of
     C-07), plus frozen evidence S2 (standards/validation/repair review), into the
     normative repair-posture, ordered-intervention-ledger, verdict-layer,
     annotation-state, projection, provenance, revalidation, and resolution
     matrices. It makes NO new product decision: every SETTLED cell is transcribed
     from an accepted record; a cell those records left as an owner open question /
     deferral is flagged OPEN with impact + destination, never invented. This is
     the SINGLE WRITER of the v3 validation/intervention (input-side) matrices.
     Canonical FIELD NAMES are consumed from artifact 3 (contract-canonical-state)
     without renaming. Output/serialization revalidation + credential OUTPUT policy
     are P2.2's / G3.7's; cleaning/mutation is G3.8's; comparison keys are G3.K's;
     host/PSL/IDNA/IP vocabulary is G3.H's; cache is G3.9's — each referenced as a
     boundary, never redefined. Format follows the G3.3/G3.5/G3.K precedent:
     Envelope, tamper-evident Inputs, pipe-table Rows/matrices, Scope boundaries,
     Open cells. The envelope stays lifecycle_state PROPOSED until the
     cp-snapshot-3 seal; validator coverage and the manifest present-flip ride
     that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-validation-intervention |
| name | validation-intervention-contract |
| artifact_number | 6 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/validation-intervention-contract.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the v3 validation/intervention (input-side) matrices: repair-posture axis, ordered intervention ledger, verdict layers + fatality, annotation-state resolution, the π collapse table, repair/recovery provenance, repaired-input revalidation, and resolution verdict layering |
| lifecycle_state | PROPOSED |
| dependencies | P2.1 (bound decision — postures/ordering/C-02/C-03); P2.3 (bound decision — verdict semantics/migration); P1.1 (bound decision — layer model + independence invariant); S2 (bound evidence); contract-canonical-state (field vocabulary only, not projected); reconciliation §6 artifact 6, §4 RCON-04, §7 G3 |
| bound_decision | P2.1 + P2.3 + P1.1 |
| bound_evidence | S2 |
| closes_finding | RCON-04 |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: repair-posture axis + pipeline order, ordered intervention ledger, verdict-layer enums + fatality, shipped-value→layer map, annotation-state resolution, the π collapse table, repair/recovery provenance, repaired-input revalidation, resolution verdict layering, and the companion-helper/migration surface each carry a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line impact and owner-decision destination; cross-artifact field names agree with artifact 3; validate-records.R (validation-intervention section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (P2.1, P2.3, P1.1, S2), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | validation-intervention validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative v3 contract for everything that happens to an input **before and
around** the validation hand-off: which repair posture is active (P2.1 §1), the
fixed ordered pipeline every posture shares and the typed intervention ledger it
produces (P2.1 §2; RCON-04), the three independent verdict layers and their
fatality (P2.3 §1; P1.1 §2), how the optional PSL annotation resolves into a
typed state (P2.3 §2), the total pure projection that reproduces legacy
`parse_status` (P2.3 §4), the provenance every byte-changing intervention
records and the independent revalidation of any repaired candidate (P2.1 §1–3),
and the verdict layering for `resolve_url` reference resolution (P2.3 §3).

This record **projects** accepted P2.1 + P2.3 + P1.1 and S2; it does not
implement these surfaces and makes no product decision. A `SETTLED` row cites
`P2.1@a4d1b45`, `P2.3@a7e0a59`, or `P1.1@a7e0a59`. A cell an accepted record left
open or deferred is `OPEN` with its exact impact and settlement destination.

## Inputs

The exact sources this contract projects, hashed at authoring.
`validate-records.R` recomputes all hashes at the cp-snapshot-3 seal. P2.1, P2.3,
and P1.1 are already hash-enforced as ACCEPTED decisions by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P2.1-fixer-recovery-ordering.md | 37c4d3b6f60e2e67b7f356978bb8d44f7eac580b3006b86c5544ce0bcb4597e5 |
| design/work/url-v3/decisions/P2.3-validation-verdicts-migration.md | 1a1d1c4542a50a7c8310efc9055057c17c4ddbdaab2fb9988d08070253fa7a2e |
| design/work/url-v3/decisions/P1.1-v3-state-status-model.md | 656510b6d0c16991726ff7b41523712359f60921e1cc472d354f53b498a3cd29 |
| design/work/url-v3/evidence/S2-standards-validation-repair.md | b9301eecf2a7dcf41bf2b820e0fc0a6628e6e037a61db2ec011cbd40ed929bbd |

## Repair-posture axis rows

The input-acceptance / repair axis. Three postures, ordered by how much they
touch the input; a posture enables or disables pipeline stages but never
reorders them (P2.1 §1–2). The **public default is `strict`** at the 3.0 major
boundary (P2.1 §1 / B1). The public *spelling* of this axis — a new argument vs
an extension of `scheme_policy` / `fixup_posture` vs a `profile` expansion — is
NOT settled here (see Open cells VAL-O2).

| posture | touches input | what it does | v3 default | owner_decision_ref | status |
|---|---|---|---|---|---|
| `strict` | none | no repair, no recovery; raw bytes flow through standards-required preprocessing into a strict parse; non-conformant input **rejects**; the original-input verdict is authoritative; never silently validates a rewritten string | **default** (spec-exact, returns an original-input verdict) | P2.1@a4d1b45 (§1, B1) | SETTLED |
| `compatibility` | leniencies only | the v2-behavior-preserving posture: default scheme inference + the direct-selector repeated-`@` recovery; every allowance is **compatibility-only, never evidence of conformance**; distinct from `strict` | selectable, no longer default | P2.1@a4d1b45 (§1, B2) | SETTLED |
| `repair` | may rewrite bytes | explicit, opt-in, provenance-logged fix-up; the bounded browser fixer is the flagship; every repair rule has a stable ID, records before/after, and the repaired candidate is re-validated independently of the original; never implicit | opt-in | P2.1@a4d1b45 (§1) | SETTLED |
| posture ⇒ per-axis default (not a global lock) | — | a posture sets the *default* `scheme_policy` (`strict → require`, `compatibility → infer`); an explicit axis override wins (ADR 0012 D6), but an explicit `scheme_policy = "infer"` under `strict` takes only the scheme-less axis out of strict — the fabricated `http://…` is reported as an inferred candidate, never as strict conformance of the original bytes | — | P2.1@a4d1b45 (§2 stage 3); P2.4@b017e87 (D-C) | SETTLED |
| 3.0 supersession (recorded, not silent) | — | the strict default flip **supersedes at 3.0** the `scheme_policy = "infer"`-as-default rule of ADR 0010 and the scheme-less-inference half of ADR 0012 D4 (v3.0 amendment blocks); only `scheme_policy` flips — `scheme_acceptance = "web"` and `url_standard = NULL` stay byte-for-byte default | — | P2.1@a4d1b45 (§1 Supersession); P2.4@b017e87 (D-C) | SETTLED |

## Ordered pipeline / intervention-ledger rows

One deterministic six-stage order governs every posture (P2.1 §2). RCON-04
mandates that each stage that mutates the string record a typed row — the
**ordered intervention ledger** — carrying at least: stable rule ID, owning
standard/policy/dependency, phase, category, before/after span, whether the
original input is preserved, and whether the mutation changes conformance. The
**load-bearing invariant**: under `strict`, stages 1 and 4 are a tested no-op,
the original bytes are evaluated, and no hidden recovery runs before curl (P2.1
§2; S2-02 "repair off must be a tested no-op").

| # | stage | runs under | category (RCON-04) | contract | owner_decision_ref | status |
|---|---|---|---|---|---|---|
| 1 | repair (bounded browser fixer, one complete contiguous pre-parse pass on the raw string) | `repair` only | explicit repair | outer C0/space trim → `;`→`:` for a recognized special scheme → `://` insertion for an authority-table scheme; provenance logged; byte-for-byte no-op under `strict`/`compatibility`; **this ordering is the C-02 disposition** | P2.1@a4d1b45 (§2.1, §3 C-02) | SETTLED |
| 2 | standards-required preprocessing (*parser behavior*) | all postures | standards-required preprocessing | WHATWG control-character stripping, literal-backslash recognition, UTS-46 domain separator mapping, run as step 1 of the selected standard's parse on the (possibly repaired) string; parser behavior, never fixer policy; runs **after** the fixer, never interleaved | P2.1@a4d1b45 (§2.2, §3 C-02) | SETTLED |
| 3 | scheme admission + inference | all (inference only under `infer`) | product compatibility recovery (inference) | `scheme_acceptance` gate, then `scheme_policy` (`require` under `strict`; `infer` under `compatibility`); the fallback-`http` prepend is the single shared inference seam (ADR 0012 D4), deliberately outside the fixer; inference fabricates `http://…` — a byte-changing rewrite `strict` never performs | P2.1@a4d1b45 (§2.3); P2.4@b017e87 (D-C) | SETTLED |
| 4 | recovery (selector-scoped; chiefly repeated-`@` authority recovery) | `compatibility` / `repair` — **never `strict`** | explicit repair (repeated-`@`) | the **last** `@` is taken as the userinfo/host delimiter and the earlier `@` bytes are percent-encoded; under `strict` this stage is a no-op and a non-conformant authority rejects; **this binding is the C-03 disposition** | P2.1@a4d1b45 (§2.4, §3 C-03) | SETTLED |
| 5 | strict parse + component extraction | all | n/a (parse) | the selected standard's parse produces the canonical component slices | P2.1@a4d1b45 (§2.5) | SETTLED |
| 6 | validation verdicts | all | n/a (verdict) | the independent L1/L2/L3 verdicts (below) are produced here; both original-input and repaired-input verdicts are exposed when repair ran (RCON-04) | P2.1@a4d1b45 (§2.6); P2.3@a7e0a59 (§1) | SETTLED |
| ledger completeness | per-row category for recoveries **beyond** repeated-`@` (WHATWG IPv4 rewrite, host-charset shim, curl PQF sanitization, dependency shims) | — | **not assigned** | P2.1 categorized only the fixer steps, scheme inference, and repeated-`@`; the `strict`-excluded-vs-parser-behavior assignment for the other ledger recoveries was explicitly deferred | — (see Open cells VAL-O1) | OPEN |

## Verdict-layer rows

The single `parse_status` column is replaced by three **independent** verdict
layers (P1.1 §2), each with its own typed enum and fatality classification (P2.3
§1). **Fatality is a property of L1 and L2 only; L3 is never fatal** — the
load-bearing independence invariant (P1.1 §2: L3 MUST NOT change L1/L2; syntax
and policy are computed *before* annotation and are invariant under annotation
selection, cache warmth, PSL section/list/engine identity, and dependency
availability). Field names are consumed from artifact 3 (`layer1_syntax_verdict`,
`layer2_policy_verdict`, `layer3_annotation_state`).

| layer | field (artifact 3) | enum | fatality | contract | owner_decision_ref | status |
|---|---|---|---|---|---|---|
| L1 syntax / parse | `layer1_syntax_verdict` | `{ pass, fail }` | `fail` = **FATAL** (structural); `pass` = non-fatal | did the input parse into a well-formed URL under the selected standard? pure standard-parse outcome (`curl_ok`); computed **first**; depends on nothing outside the URL string and the standard | P2.3@a7e0a59 (§1); P1.1@a7e0a59 (§2) | SETTLED |
| L2 policy / admission | `layer2_policy_verdict` | `{ admitted, admitted-scheme-relative, admitted-ftp, rejected-scheme, warn-userinfo }` | `rejected-scheme` = **FATAL** (admission reject); `warn-userinfo` = **WARNING** (accept-with-note); `admitted*` = non-fatal accept | whether an already-parsed object is admitted, and with what note, under the active `scheme_acceptance`/`scheme_policy`/`scheme_relative_handling`/userinfo policy; layered **over** a syntactically parsed URL, not syntax | P2.3@a7e0a59 (§1); P1.1@a7e0a59 (§2) | SETTLED |
| L3 optional annotation | `layer3_annotation_state` | `{ not-requested, not-applicable, known, unknown, invalid-input, dependency-error }` | **never fatal** (not a fatality axis) | PSL registrability facts as typed annotation results (S7-F3), carrying value + provenance, never a bare `NA`; MUST NOT change L1 or L2; reaches a status-shaped output only through π (below) | P2.3@a7e0a59 (§1–2); P1.1@a7e0a59 (§2) | SETTLED |

### L2 sub-states (P2.3 §1)

| L2 value | meaning | fatal? | owner_decision_ref | status |
|---|---|---|---|---|
| `admitted` | accepted, no note | no | P2.3@a7e0a59 (§1) | SETTLED |
| `admitted-scheme-relative` | scheme-relative input retained under `scheme_relative_handling == "keep"` (shipped `ok-scheme-relative`); an **accept sub-state**, not a warning | no | P2.3@a7e0a59 (§1) | SETTLED |
| `admitted-ftp` | FTP-family scheme retained under `protocol_handling != "strip"` (shipped `ok-ftp`); an accept sub-state | no | P2.3@a7e0a59 (§1) | SETTLED |
| `rejected-scheme` | under `scheme_acceptance == "web"` an unsupported scheme-bearing token is demoted (`looks_like_protocol & !original_has_allowed_scheme & !looks_like_host_port`, ADR 0012 D3); a **policy** rejection, distinct from an L1 structural failure though both collapse to shipped `error` | **FATAL** | P2.3@a7e0a59 (§1) | SETTLED |
| `warn-userinfo` | scheme-less userinfo-shaped input (`user@example.com`): host/domain/tld/user resolve but `clean_url` is `NA` and the row is non-joinable; accepted with a note (confirmed a **Layer-2 policy** outcome, not L3 — P1.1 Q4) | no (accept-with-note) | P2.3@a7e0a59 (§1); P1.1@a7e0a59 (Q4) | SETTLED |

## Shipped-value → layer map (the C-07 map)

The single most consequential C-07 correction: shipped `error` conflates an L1
structural failure with an L2 admission rejection, and the three PSL `warning-*`
values are L3 facts wrongly shaped as parse verdicts. Transcribed verbatim from
P2.3 §1.

| shipped `parse_status` | layer | layer verdict | fatal? | owner_decision_ref | status |
|---|---|---|---|---|---|
| `error` (from `!curl_ok`) | L1 | `fail` | **FATAL** (structural) | P2.3@a7e0a59 (§1) | SETTLED |
| `error` (web unsupported-scheme demotion) | L2 | `rejected-scheme` | **FATAL** (admission) | P2.3@a7e0a59 (§1) | SETTLED |
| `warning-userinfo` | L2 | `warn-userinfo` | no (accept-with-note) | P2.3@a7e0a59 (§1) | SETTLED |
| `ok-scheme-relative` | L2 | `admitted-scheme-relative` | no (accept) | P2.3@a7e0a59 (§1) | SETTLED |
| `ok-ftp` | L2 | `admitted-ftp` | no (accept) | P2.3@a7e0a59 (§1) | SETTLED |
| `ok` | L1 pass + L2 admitted | `pass` / `admitted` | no | P2.3@a7e0a59 (§1) | SETTLED |
| `warning-no-tld` | L3 | annotation `unknown` (no dot) | **never** (L3) | P2.3@a7e0a59 (§1) | SETTLED |
| `warning-invalid-tld` | L3 | annotation `unknown` (dot, no suffix) | **never** (L3) | P2.3@a7e0a59 (§1) | SETTLED |
| `warning-public-suffix` | L3 | annotation `known`, suffix-only | **never** (L3) | P2.3@a7e0a59 (§1) | SETTLED |

## Annotation-state resolution rows (S7-F3)

The registrable-domain / PSL annotation returns exactly the S7-F3 typed states.
Transcribed from P2.3 §2. Invariance (P1.1 §2): L1/L2 are decided *before* the
annotation runs and are invariant under annotation selection, cache warmth, PSL
section/list/engine identity, and `pslr` availability — switching engine,
skipping the lookup, or a `dependency-error` may change **only** the annotation
state and its π projection, never L1/L2.

| annotation state | when it is decided | owner_decision_ref | status |
|---|---|---|---|
| `not-requested` | the PSL annotation was not requested for this call (lazy / companion not invoked); never silently `known` | P2.3@a7e0a59 (§2) | SETTLED |
| `not-applicable` | the host form has **no** registrable-domain concept: IP-literal host, `file`-scheme host, opaque/general non-special-authority host (ADR 0012 D5); `not-applicable`, **not** `unknown` | P2.3@a7e0a59 (§2) | SETTLED |
| `known` | a PSL match produced a registrable domain (`tld` and `domain` both non-empty), **or** the input is a known public suffix with no registrable domain above it (suffix-only — shipped `warning-public-suffix`); suffix-only is a *known fact*, not an error | P2.3@a7e0a59 (§2) | SETTLED |
| `unknown` | the host is reg-name-shaped but no public suffix matched: single-label no-dot host (shipped `warning-no-tld`) or dotted host with an unlisted suffix (shipped `warning-invalid-tld`) | P2.3@a7e0a59 (§2) | SETTLED |
| `invalid-input` | the host is malformed for the annotation's own policy (rejected by the annotation, not by URL syntax) | P2.3@a7e0a59 (§2) | SETTLED |
| `dependency-error` | `pslr` / the resolved engine is unavailable or errored; distinct from `unknown` — a lookup that *could not run* is not a lookup that *ran and found nothing* | P2.3@a7e0a59 (§2) | SETTLED |

## π collapse table (legacy `parse_status` projection)

Legacy `parse_status` is retained as the **total, pure** projection
`π(L1, L2, L3)` and is the **sole** locus a PSL fact reaches a status-shaped
output (P1.1 §4 (iv)). π reproduces `.derive_parse_status_vec()` **exactly** —
this table is the byte-identity contract (P2.3 §4; proof obligation verified by
G3 golden fixtures). Evaluate top to bottom; first match wins.

| # | condition | projected `parse_status` | owner_decision_ref | status |
|---|---|---|---|---|
| 1 | L1 = `fail` | `error` | P2.3@a7e0a59 (§4) | SETTLED |
| 2 | L2 = `rejected-scheme` (admission reject) | `error` | P2.3@a7e0a59 (§4) | SETTLED |
| 3 | L2 = `warn-userinfo` | `warning-userinfo` | P2.3@a7e0a59 (§4) | SETTLED |
| 4 | L3 = `unknown`, host has **no** dot | `warning-no-tld` | P2.3@a7e0a59 (§4) | SETTLED |
| 5 | L3 = `unknown`, host **has** a dot | `warning-invalid-tld` | P2.3@a7e0a59 (§4) | SETTLED |
| 6 | L3 = `known`, **suffix-only** (no registrable domain) | `warning-public-suffix` | P2.3@a7e0a59 (§4) | SETTLED |
| 7 | L2 = `admitted-scheme-relative` | `ok-scheme-relative` | P2.3@a7e0a59 (§4) | SETTLED |
| 8 | L2 = `admitted-ftp` | `ok-ftp` | P2.3@a7e0a59 (§4) | SETTLED |
| 9 | otherwise (L1 pass, L2 admitted, L3 `known`/`not-applicable`/`not-requested`) | `ok` | P2.3@a7e0a59 (§4) | SETTLED |

**Precedence invariants** (the C-07 "which wins" rules — P2.3 §4):

| rule | contract | owner_decision_ref | status |
|---|---|---|---|
| error-over-warning | any fatal layer (rows 1–2) collapses to `error` before any warning is considered; L1-fatal checked before L2-fatal but both project to `error` — the distinction survives only in the L1/L2 companion verdicts, not in `parse_status` (the documented lossy-projection **loss**) | P2.3@a7e0a59 (§4) | SETTLED |
| PSL-warning partition | the three PSL states are mutually exclusive by construction (`host_has_dot`/`tld_empty`/`domain_empty` cascade); rows 4→5→6 document a partition, not a tie-break | P2.3@a7e0a59 (§4) | SETTLED |
| userinfo-over-PSL | `warn-userinfo` (row 3) outranks any L3 warning, matching shipped behavior | P2.3@a7e0a59 (§4) | SETTLED |
| not-applicable / not-requested → nominal | IP/opaque/file host (`not-applicable`) and an un-run lookup (`not-requested`) both fall through to row 9 `ok`; they are **not** warnings | P2.3@a7e0a59 (§4) | SETTLED |
| dependency-error → nominal (v3 forward-consistency, NOT a byte-identity claim) | a PSL lookup that could not run projects to row 9 `ok` so dependency availability never changes the status; shipped `.derive_parse_status_vec()` has **no distinct dependency-error path** (a PSL failure surfacing as empty `tld`/`domain` is projected to `warning-no-tld`/`warning-invalid-tld`; a hard `pslr` error propagates), so `dependency-error → ok` is a v3 policy choice for a state v2 never surfaced — the byte-identity guarantee is scoped to inputs whose PSL lookup **runs** | P2.3@a7e0a59 (§4, Q3) | SETTLED |

## Repair / recovery provenance rows

Repair and recovery are provenance-bearing transformations attributable to a
named posture, never silent parser internals (P2.1 §2 invariant; RCON-04; the
input-side answer to RCON-03's parse-vs-repair conflation). The two contradiction
dispositions and the "repair off = no-op" invariant.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| C-02 — browser-fixer phase ordering | the **shipped ordering wins**: the browser fixer runs as one complete contiguous pre-parse pass; standards-required WHATWG preprocessing then runs as parser behavior on the fixer's output (pipeline stages 1 → 2); the PRD Part 1 **bullet-list** interleaved ordering is **superseded** (a documentation defect, not a compatibility surface — both orderings produce the same final string, differing only in phase provenance) | P2.1@a4d1b45 (§3 C-02) | SETTLED |
| C-03 — repeated-`@` RFC recovery | classified **REPAIR** (`@` → `%40` byte mutation, not syntax recognition); bound to `repair` (as a named provenance-logged rule) and `compatibility` (retained for v2 direct-RFC callers, labeled compatibility-only), **excluded from `strict`** (which rejects); strict RFC and general RFC then agree — both reject repeated raw `@` | P2.1@a4d1b45 (§3 C-03, B2) | SETTLED |
| repair-off = tested no-op | under `strict`, pipeline stages 1 and 4 do nothing, the original bytes are evaluated, and no hidden recovery runs before curl; a testable invariant for G4 (S2-02) | P2.1@a4d1b45 (§2 invariant) | SETTLED |
| per-intervention provenance | every stage that mutates the string records category, owning standard/policy, before/after span, and whether it changes conformance — the ordered intervention ledger (RCON-04); phase-provenance/audit traces are stable (the fixer is always a complete pre-parse pass; WHATWG preprocessing is always parser behavior) | P2.1@a4d1b45 (§2, §3 C-02) | SETTLED |
| absence-is-not-conformance | a "strict" path must not silently validate a repaired string; `parse_status`, absence of a selected diagnostic, `is_valid_host()`, `clean_url`, or successful parse/serialization may **never** serve as a conformance proxy (S2-01) | P2.1@a4d1b45 (§1); P2.3@a7e0a59 (§1, S2-01) | SETTLED |

## Repaired-input revalidation rows

The independent-verdicts requirement of RCON-04 at the input end: the original
bytes are evaluated independently of any repaired candidate, and a repaired
candidate is re-validated on its own.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| both verdicts exposed | when repair ran, **both** the original-input conformance verdict and the repaired-input conformance verdict are produced (RCON-04); the original-input verdict for the raw bytes is authoritative and unchanged by repair | P2.1@a4d1b45 (§1, §2.6) | SETTLED |
| inferred candidate is not strict conformance | an explicit `infer` under `strict` yields an inferred `http://…` candidate reported through the layered verdicts as an inferred-candidate outcome, never as strict conformance of the original scheme-less bytes; the original-input verdict (scheme-less, which `require` rejects) is unchanged | P2.1@a4d1b45 (§2.3); P2.4@b017e87 (D-C) | SETTLED |
| serialized-**output** revalidation (boundary) | revalidation of a *serialized output string* under the selected standard (faithful standard serialization, round-trip/idempotence) is **not** owned here; it is the output/serialization contract (**P2.2 / G3.7**). This contract owns the input-side original-vs-repaired **input** verdicts only | P2.2 (#212); reconciliation §6 artifact 7 | SETTLED (boundary) |

## Resolution verdict rows (`resolve_url`)

Reference resolution carries the same three-layer model applied to three
independent objects, not one blended `NA` (P2.3 §3). The verdict **layering** is
settled; whether/how the merged verdicts are **surfaced** is deferred.

| object | contract | owner_decision_ref | status |
|---|---|---|---|
| base-URL verdict | L1/L2 of the base; a base that fails L1 makes the resolution unresolvable | P2.3@a7e0a59 (§3) | SETTLED |
| reference verdict | L1/L2 of the relative/absolute reference | P2.3@a7e0a59 (§3) | SETTLED |
| merged-output verdict | L1/L2 of the resolved absolute URL (RFC 3986 recomposition then re-parse); resolution *success* is an **L1 fact** about the merged output, admission of the merged output is **L2**; the merged host's L3 annotation follows §annotation | P2.3@a7e0a59 (§3) | SETTLED |
| current `NA`-on-failure | `resolve_url()`'s existing `NA` return on unresolvable input / parse failure is an **L1-fatal projection**, preserved for back-compat; the verdict layering is independent of that output-shape question | P2.3@a7e0a59 (§3) | SETTLED |
| resolver verdict **surface** | whether `resolve_url()` exposes the base/reference/merged-output L1/L2 verdicts via a companion helper is **not** settled | — (see Open cells VAL-O3) | OPEN |

## Companion-helper surface + migration rows

The layered verdicts are surfaced through a **dedicated** companion helper, never
by widening the parse frame (ADR 0006). `parse_status` is retained as the lossy
compat view. Transcribed from P2.3 §2 and §5.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| `get_parse_verdicts()` | the v3.0 companion accessor returning L1 syntax, L2 policy, and typed L3 annotation states; **defined at `url_standard = NULL`** (L1/L2 describe the parse that actually occurred — **not** `NULL → NA`-gated the way `get_host_type()`/`get_scheme_class()` are; only the L3 annotation follows the opt-in pattern); ADR 0006 amended (v3.0) to authorize this fourth companion helper while preserving its never-widen rule | P2.3@a7e0a59 (§2, Q1); ADR 0006 | SETTLED |
| `get_url_diagnostics()` unchanged | keeps its existing token-list shape; it may surface the verdict/annotation states but is **not** reshaped to carry the layered-verdict columns; keeps its `url_standard`-gated contract | P2.3@a7e0a59 (§2, Q1) | SETTLED |
| `get_host_type()` / `get_scheme_class()` unchanged | classify host shape and scheme class and feed the diagnostic aggregation; host-form classification decides `not-applicable` vs an eligible reg-name | P2.3@a7e0a59 (§2) | SETTLED |
| parse frame keeps 18 public fields | the layered verdicts live in the companion surface; no new parse-frame columns (ADR 0006) | P2.3@a7e0a59 (§2) | SETTLED |
| `parse_status` retain-as-projection (3.0) | column + `get_parse_status()` kept and defined as π(L1,L2,L3); no new tokens; vocabulary frozen at the eight shipped values | P2.3@a7e0a59 (§5) | SETTLED |
| back-compat guarantee (3.0), scoped | for every input whose PSL lookup **runs**, `parse_status` (column) and `get_parse_status()` are **byte-identical** to v2 (π reproduces `.derive_parse_status_vec()` exactly); existing callers, incl. `canonical_join()`'s `join_parse_status` filter and the `.is_ok_status`/`.is_warning_status`/`.is_joinable_status` predicates, are unaffected; the single out-of-scope cell is `dependency-error → ok` | P2.3@a7e0a59 (§5, Q3) | SETTLED |
| `parse_status` reframed as compat, not deprecated in 3.0 | documented as a lossy compatibility projection (man/ + NEWS); **not** soft-deprecated in 3.0; the layered accessor is additive, nothing removed; the guaranteed loss (L1-fail vs L2-reject both → `error`) is documented with a pointer to the layered accessor | P2.3@a7e0a59 (§5, Q5) | SETTLED |
| `parse_status` hard-deprecation window | whether/when a hard deprecation with a defined removal window applies is **not** scheduled in 3.0 | — (see Open cells VAL-O4) | OPEN |

## Scope boundaries

This contract owns the **validation/intervention (input-side)** matrices. It does
**not** define, and must not be read as redefining:

- **Canonical-state field vocabulary** (`layer1_syntax_verdict`,
  `layer2_policy_verdict`, `layer3_annotation_state`, `parse_status`, the
  classifier flags `looks_like_protocol` / `original_has_allowed_scheme` /
  `is_scheme_relative` / `looks_like_host_port` / `scheme_less_userinfo`) —
  consumed from artifact 3 (`contract-canonical-state`) without renaming. This
  contract enumerates their **values/semantics**; artifact 3 names the fields.
- **Output / serialization contracts** — faithful standard serialization, human
  formatting, cleaning, safe display, and the revalidation of a serialized
  **output** string — are **P2.2 / G3.7**. This contract owns only the input-side
  original-vs-repaired **input** verdicts.
- **Cleaning / mutation semantics** — the transforms themselves (path
  normalization, www/protocol/trailing/index/query cleaning) are P2.1/P2.2 and
  **G3.8**; this contract states their pipeline position and provenance
  obligation, not the transforms.
- **Scheme admission + interpretation matrices** — which scheme tokens are
  admitted and how each is interpreted — are **G3.5** (P2.4 + P4.1). This
  contract states only where admission/inference sits in the pipeline (stage 3).
- **Host / IDNA / PSL / DNS / IP vocabulary** — RCON-08, **G3.H**. This contract
  names the L3 annotation *state*; the host-form and PSL-source detail are G3.H's.
- **Cache semantics** — **G3.9**. This contract states that L1/L2 are invariant
  under cache warmth; cache-key membership is G3.9's.
- **Comparison keys / URL joins** — **G3.K / P3.1**; `join_parse_status` is named
  here as an unaffected caller, not redefined.
- **The S1-F3 scalar/vector cell matrix** (container, coercion, row-local vs
  call-level failure) — the S1 scalar/vector contract, not this artifact; P1.1 §3
  fixes only that a failed element is a typed invalid canonical record carrying
  `original_url` + the layered verdicts.
- **`resolve_url` public return contract** (output shape) — RCON-03, deferred; this
  contract owns only the *verdict layering* of resolution.
- **Fixer/preprocessing double outer-trim consolidation** (P2.1 Q3) — an
  idempotent no-op on tested inputs with no known divergent output; parked as a
  post-acceptance **implementation-cleanup** slice, not a cell of this contract.

## Open cells

Each cell an accepted record left open or deferred is recorded rather than
invented. None reopens a SETTLED default.

- **VAL-O1 — intervention-ledger category for recoveries beyond repeated-`@`.**
  P2.1 categorized the fixer steps (explicit repair / standards-required
  preprocessing), scheme inference (stage 3), and the repeated-`@` recovery
  (REPAIR); it explicitly deferred (Q5) the per-row `strict`-excluded-vs-parser-
  behavior category for the other ledger recoveries — WHATWG IPv4 rewriting, the
  host-charset shim (ADR 0009), curl PQF sanitization, and dependency shims.
  **Impact:** those recoveries' posture-binding and RCON-04 category are
  unspecified, so whether each is `strict`-reachable is undefined for the ledger.
  **Settles at:** an owner-decision extension of P2.1 completing the RCON-04
  intervention-ledger categorization (P2.1 Q5 named "G3 intervention ledger", but
  the assignment is a product decision this projection cannot make).
- **VAL-O2 — repair-posture public surface naming.** P2.1 fixes the *axis and
  pipeline order*, not the public *spelling* — whether the posture is a new
  argument, an extension of `scheme_policy` / `fixup_posture`, or a `profile`
  expansion (Q4). **Impact:** the public surface a caller uses to select
  `strict` / `compatibility` / `repair` is unspecified. **Settles at:** the
  RCON-05 public-surface decision (coordinated with P2.4; cf. G3.5 posture rows).
- **VAL-O3 — resolver verdict surface.** Whether `resolve_url()` exposes the
  base / reference / merged-output L1/L2 verdicts via a companion helper is
  deferred (P2.3 Q4); the verdict *layering* is SETTLED, the *surface* is not.
  **Impact:** callers cannot read the resolved object's independent verdicts;
  `resolve_url` continues to signal only success/failure through `NA`.
  **Settles at:** the RCON-03 `resolve_url` output-shape record.
- **VAL-O4 — `parse_status` hard-deprecation window.** 3.0 retains `parse_status`
  as a documented lossy projection with **no** removal date; a hard-deprecation
  window is not scheduled (P2.3 Q2 CONFIRMED). **Impact:** the eventual removal
  timeline for the compat projection is open (the correctness-over-back-compat
  mandate permits eventual removal but this record does not schedule it).
  **Settles at:** a future owner deprecation-schedule decision (post-3.0).
