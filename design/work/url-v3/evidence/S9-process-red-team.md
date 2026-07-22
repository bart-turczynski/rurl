# S9 — Process red-team of the rurl 3.0 specification reconstruction protocol

Date: 2026-07-22  
Repository state inspected: `89be90bc1497507155ee9210dc6533020f127fc2` on `feature/url-v3-architecture-spec`  
Protocol inspected: `_scratch/url-v3-spec-reconstruction-protocol.md` (`Status: proposed review protocol`)  
Review posture: independent cold start; repository and read-only Fiberplane evidence only

## Executive verdict

**REJECT before product-specification drafting.** The protocol has a sound intent and useful semantic coverage, but it is not recoverable, reviewable, or enforceable after loss of the current worktree/orchestration context.

The blocking defect is structural: the protocol places its control plane under ignored `_scratch/` (`§5.1`, line 72), while the repository's accepted design policy explicitly says `_scratch/` is absent from a fresh clone and CI (`ARCHITECTURE.md:3-7`, `design/README.md:3-6`, ADR 0008:9-15). The protocol itself, predecessor draft, run states, evidence reports, audit bundle, future decision ledger, contradiction register, trace map, and successor draft therefore have no required durable home, immutable revision, content hash, or restore mechanism. A fresh reviewer cannot even obtain this protocol from the inspected commit.

The process also lacks executable schemas and lifecycle rules for owner decisions, contradictions, trace links, evidence snapshots, approvals, stale-source invalidation, review dispositions, and graduation. Its source hierarchy is readable but not operational: “the product owner's latest explicitly confirmed decision” has no canonical durable record, identity, timestamp, scope, or supersession chain. Existing durable documentation already demonstrates why this matters: accepted PRDs still label themselves “Draft”; ADR 0012 supersedes a rule in ADR 0004 while ADR 0004 remains `Accepted`; ADR 0012 retains a profile-name “Maintainer call” that shipped code has since resolved; and `ARCHITECTURE.md`'s load-order list is stale relative to `DESCRIPTION`.

The correction is actionable: first graduate this process into a tracked process document; establish a versioned evidence manifest and artifact schemas; freeze the source/tracker baseline; record owner approvals and supersessions durably; add objective batch/review/graduation gates; then begin product-specification prose. Raw bulky data may remain outside Git only if the tracked manifest makes it content-addressed and reproducibly retrievable.

Acceptance result against protocol §12: **2 yes, 8 no**. Because §12 requires every answer to be yes, the protocol is not acceptable as written.

## Cold-start execution test

### Test setup

I treated commit `89be90bc1497507155ee9210dc6533020f127fc2` as the only repository state guaranteed to a new agent, then attempted to execute the protocol without conversational context. I supplemented the tracked checkout only with evidence currently available in this worktree and read-only `fp` queries to determine whether those inputs have a specified recovery route.

### Execution result

| Step | Cold-start action | Result | Evidence / failure |
|---|---|---|---|
| 1 | Locate the governing reconstruction protocol in the commit | **Fail** | `_scratch/` is ignored by `.gitignore:23`; `git ls-files` returns no protocol path. The process cannot bootstrap from a fresh clone. |
| 2 | Recover the predecessor, run state, investigation reports, and audit evidence | **Fail** | The predecessor, both run states, audit CSVs/ledger/dials, and current reports are under `_scratch/`. Protocol §4 names three investigations but gives no report paths. The active run state says their reports are “retained in orchestration context,” which is exactly the state loss this protocol claims to survive. |
| 3 | Establish an immutable evidence baseline | **Fail** | No manifest pins repository SHA, artifact hashes, tracker query/as-of time, issue IDs, external fixture revisions, or source inventory. “123 Markdown-family files and 22 issues” (`§4`, line 62) is an unversioned count; the live tree currently has 23 nodes when the umbrella and new review issue are counted. |
| 4 | Determine which owner decisions are authoritative | **Fail** | Protocol §3 line 40 gives owner decisions highest precedence, but there is no canonical owner-decision record or definition of “explicitly confirmed.” The compact run state may temporarily hold approved decisions before transfer (`§5.1`, line 77), but it is ignored and has no transfer SLA or loss guard. |
| 5 | Construct the ledger/register/trace map consistently | **Fail** | The protocol lists fields but does not define filenames, durable paths, schema versions, owners, allowed state transitions, timestamps, revision rules, referential integrity, or completeness checks. Independent agents can create incompatible artifacts. |
| 6 | Prove all semantic surfaces and evidence were covered | **Fail** | Scope says “at least” (`§6`, line 120) but defines no closed public-API, dial, source, fixture, or test inventory. Current tracked architecture documentation itself omits loaded modules. |
| 7 | Run the five owner-review batches and know when each closes | **Fail** | “Confirmed” (`§9`, line 276) has no approver, approval artifact, decision IDs, entry/exit criteria, or invalidation rule when later batches change earlier assumptions. |
| 8 | Perform independent red-team review | **Partial / insufficient** | Draft-only review (`§10`, line 280) is useful for semantic cold reading, but cannot verify traceability, fixture provenance, authority, or the evidence map. There is no draft hash, findings ledger, disposition schema, or mandatory re-review after normative edits. |
| 9 | Graduate and open implementation work | **Fail** | “Owner approval,” “stable,” and “full verification gate” are undefined. The proposed umbrella epic is currently a narrower determinism epic. The successor WIP path and atomic promotion procedure are unspecified. |

**Cold-start conclusion:** the process fails before its first substantive reconstruction step. It can be continued only because the current untracked worktree and live orchestration context still exist, not because the protocol makes them recoverable.

## Evidence and artifact availability map

| Evidence / artifact | Current location or source | Tracked / recoverable on fresh clone? | Reviewability assessment |
|---|---|---:|---|
| Reconstruction protocol | `_scratch/url-v3-spec-reconstruction-protocol.md` | **No** | Governing process disappears with the worktree. |
| Protected predecessor | `_scratch/prd-url-object-grammar-DRAFT.md` | **No** | `§8` says preserve it, but supplies no tracked archive, hash, or restore source. |
| Active architecture-spec run state | `_scratch/orchestrate/RURL-v3-architecture-spec-RUN-STATE.md` | **No** | Paths/frontier are useful locally; investigation reports are referenced only as retained in orchestration context. |
| Prior spec-audit run state | `_scratch/orchestrate/RURL-v3-spec-audit-RUN-STATE.md` | **No** | Contains compact findings and an in-file correction from 19 to 18 fields, illustrating the need for versioned validation. |
| Existing audit report | `_scratch/orchestrate/RURL-iuhppfii-audit.md` | **No** | Useful discovery aid, unavailable after clean checkout. |
| Dial inventory | `_scratch/orchestrate/audit/dials.md` | **No** | Useful enumeration of public/internal knobs and profile conflicts, but not a durable source-of-truth artifact. |
| Generated conformance ledger | `_scratch/orchestrate/audit/LEDGER.md` plus CSVs | **No** | Includes useful package/tool snapshots and hundreds of rows, but has no tracked manifest, content hash, or guaranteed reproducible retrieval. |
| Robustness output | `_scratch/orchestrate/audit/robustness.md` | **No** | Same durability problem as the ledger. |
| Current source and tests | `R/`, `tests/testthat/`, `NAMESPACE`, `DESCRIPTION` | **Yes** | Strong shipped-behavior evidence at a pinned Git SHA; not automatically desired v3 behavior. |
| Accepted ADRs / PRDs | `design/adr/`, `design/prd/` | **Yes** | Durable, but status/supersession contradictions require per-claim reconciliation. |
| Architecture / contribution / CI instructions | `ARCHITECTURE.md`, `CLAUDE.md`, `FP_CLAUDE.md`, `CONTRIBUTING.md`, `.github/workflows/` | **Yes** | Durable; some descriptive content is stale and “full gate” is not named by the protocol. |
| Fiberplane issue state | Live `fp` backend; local `.fp/` ignored at `.gitignore:58` | **Partially** | Read-only queries recover current state, but the protocol requires no immutable export, as-of timestamp, query definition, or issue manifest. Live state can drift during reconstruction. |
| External conformance fixtures | `tests/testthat/fixtures/` | **Yes, fixture bodies only** | Tests cite upstream sources, but key corpora do not consistently pin upstream commit/retrieval date/schema version. |
| Future decision ledger, contradiction register, trace map, review reports, successor draft | Path not specified; run state implies `_scratch` working set | **Not guaranteed** | No durable location, schema, revision, ownership, or restoration contract. |

Minimum correction: add a tracked `design/work/url-v3/manifest.yml` (or equivalent) that pins the process revision, repository SHA, artifact schema version, each artifact URI/path and SHA-256, generation time/tool versions, Fiberplane query and as-of timestamp/export, external-source revisions, and supersession status. Track all normative/control-plane Markdown/CSV/YAML. For raw evidence too large for Git, require a stable archive URI, checksum, license/provenance, and tested retrieval command.

## Authority and supersession audit

### What works

- Protocol §3 has an explicit, sensible distinction between desired v3 behavior and shipped code/tests (`lines 40-54`).
- It correctly says a passing test may be compatibility, characterization, or conformance evidence (`line 51`).
- It forbids silent reconciliation and forbids normative reliance on unverified recollection (`lines 22, 49-54`).

### Why the hierarchy is not executable

| Authority concern | Repository evidence | Required correction |
|---|---|---|
| Highest-precedence owner decision has no durable identity | `§3:40` says “latest explicitly confirmed decision”; no schema identifies owner, decision time, exact text, scope, evidence URI, source baseline, approval state, or what it supersedes. | Introduce immutable `OWNER-*` records with owner identity/authority, timestamp, exact decision, affected requirement/decision IDs, base revision, source link/comment ID, supersedes links, and approval status. Only `ACCEPTED` records in the durable ledger may override lower sources. |
| Authority is assigned at document/category granularity instead of claim granularity | ADRs and PRDs mix shipped, superseded, open, and historical claims. | Create a source-claim register. Every `REQ-*`/`DEC-*` points to exact line/symbol/test/issue/comment revisions and declares its role: current fact, accepted v3 decision, historical intent, proposal, or contradiction. |
| Durable PRDs contradict their provenance headers | `design/prd/url-standard-selector.md:1-7` calls v1 durable, accepted, and shipped; `:12-16` calls it “Draft for review.” V2 repeats this at `url-standard-selector-v2.md:1-6` versus `:11-17`. | Reconcile status metadata in the source documents, or explicitly mark which metadata block is authoritative and file a durable correction before using them as accepted evidence. |
| Supersession is declared but not reflected in the old ADR's status | ADR 0012:3-8 and :712-721 supersede ADR 0004's closed-scheme rule. ADR 0004:3 still says `Accepted`, contrary to `design/README.md:30-35`. | Change the old ADR's durable status/link or represent partial supersession at named decision IDs if the ADR remains partly active. Add a lintable supersession graph. |
| An accepted ADR contains a blocker already resolved in code | ADR 0012:885-891 leaves `seo` versus `canonical` to a maintainer call; current `R/parse.R` defines `seo` primary with `canonical` alias. | Resolve the decision through an owner record and focused ADR/ADR update; do not infer desired v3 policy solely from shipped code. |
| Tracked architecture description has drifted | `ARCHITECTURE.md:24-36` says `DESCRIPTION` Collate is authoritative but its displayed list omits `parse-state.R`, `profiles.R`, `email-diagnostics.R`, and `host-policy.R`, all present in `DESCRIPTION:17-34`. | Add documentation/source consistency checks and repair the tracked description before treating it as complete architecture authority. |
| Existing decompositions conflict with the proposed “delay” | ADR 0012 contains a layered implementation plan; the predecessor draft contains suggested implementation decomposition; existing Fiberplane work is already decomposed. | Classify all existing implementation plans/issues as shipped, historical, provisional, or blocked. Enforce a tracker gate for new v3 implementation work rather than relying on prose. |

The protocol must also state how equal-rank conflicts resolve, how a newer source is established, how partial supersession works, how rejected proposals remain visible, and when a changed source invalidates an approved batch.

## Artifact-schema and lifecycle audit

### Schema assessment

| Artifact | Fields presently required | Missing fields / lifecycle controls |
|---|---|---|
| Compact run state (`§5.1`) | phase/frontier, completed evidence units, blockers/forks, pending-transfer approvals, artifact paths | Schema version, run ID, repository SHA, tracker snapshot ID/as-of, artifact hashes/revisions, updated-at/by, last successful gate, resumption commands, stale/invalid state, transfer SLA, backup location. |
| Decision ledger (`§5.2`) | ID, topic, current behavior, documented decision, owner direction, proposed contract, status, evidence, consequences | One row per atomic assertion rather than broad topic; owner/approver; created/updated timestamps; source-baseline revision; confidence; dependency IDs; supersedes/superseded-by; transition rules; approval evidence; rejection rationale; invalidation/reopen rules; normative requirement links. |
| Contradiction register (`§5.3`) | claims, evidence, temporal role, required owner decision, resolution/superseded source | Stable contradiction ID, state enum, owner, severity, affected `DEC-*`/`REQ-*`, opened/resolved timestamps, source revisions/hashes, resolution evidence, disposition, supersession application status, reopen rule. |
| Traceability map (`§5.4`) | requirement to owner/ADR/PRD/compatibility/conformance/new proposal | Reverse links; exact evidence location and revision; owning decision; verification method/test/fixture ID; oracle/provenance; contradiction links; implementation/migration issue; status; reviewer; last verification; orphan/coverage checks. |
| Fixture/example matrix (`§7`) | input categories and layer questions | Stable case ID; input encoding; operation/options/profile; expected parse state and each output; normative `REQ-*`; source/oracle and independence; upstream revision; license; expected diagnostic/status; platform sensitivity; current/v3 classification; coverage intent. |
| Review output (`§10`) | severity, section, example, owner questions, editorial list | Stable finding ID; reviewed draft hash; reviewer and timestamp; affected requirements; evidence; disposition/owner; target revision; verification; closed/reopened state; mandatory re-review trigger. |
| Graduation record (`§11`) | ordered prose steps only | Exact WIP and final paths; approved draft hash; approval record; gate results/URLs; source/trace closure metrics; ADR/PRD supersession mutations; tracker snapshot; atomic move/rollback; release criterion. |

### Required lifecycle

Use explicit states and audited transitions, for example:

1. `DISCOVERED` — evidence recorded against a frozen baseline.
2. `PROPOSED` — atomic contract and consequences written.
3. `CONFLICT` / `OPEN` — unresolved source or owner question blocks normative prose.
4. `ACCEPTED` or `REJECTED` — durable owner/reviewer evidence attached.
5. `TRANSFERRED` — normative `REQ-*` exists and trace links validate.
6. `VERIFIED` — fixture/test method and review evidence pass.
7. `SUPERSEDED` or `INVALIDATED` — explicit successor/source delta attached.

Transitions must declare who may perform them, required evidence, timestamps, and whether downstream batches/draft sections become stale. A validator should reject duplicate IDs, broken references, unknown status values, accepted decisions without approval evidence, requirements without verification links, and artifact hashes inconsistent with the manifest.

## Cadence, tracker, and graduation audit

### Owner-review cadence

The five thematic batches in §9 are understandable but not executable. They lack:

- entry criteria (which evidence/contradictions must be complete);
- a dependency graph for cross-cutting decisions such as object state, profiles, errors, and serialization;
- bounded batch size or an atomic decision unit;
- exact confirmation format and approver;
- a recorded batch/draft hash;
- exit criteria and coverage measures;
- change-propagation and reapproval rules when a later batch alters an earlier contract.

Correction: review `DEC-*` records in dependency order, produce a signed batch manifest listing accepted/open/rejected IDs and artifact hashes, and block prose transfer while any blocking contradiction/dependency is unresolved. Any normative change after approval marks affected downstream requirements and reviews stale.

### Independent review

Keep the draft-only cold reader, but add a second evidence review. The draft-only reviewer can find internal ambiguity without inheriting earlier conclusions; they cannot prove authority or traceability. The evidence reviewer must receive the frozen manifest, ledger, contradiction register, trace map, fixture provenance, and exact draft hash. Both reviews need a durable findings/disposition ledger and re-review after material normative changes.

### Fiberplane tracker

Read-only `fp tree RURL-gxqdmpcp` shows that the mandated umbrella is currently titled **“Epic: cross-platform parse determinism — eliminate libcurl-version variance”**, not “rurl 3.0 architecture specification.” It is `todo`, while review issue `RURL-ulatmylm` is `in-progress`; other open children concern Tier 2 behavior, RFC gating, uppercase percent hex, and case handling. The live tree now contains 23 nodes including the umbrella and the new review issue, whereas protocol §4 records 22 without defining whether the root is included or when the count was taken.

Protocol §11:313 must not silently recharter that epic. The owner must choose and record one of:

1. explicitly recharter `RURL-gxqdmpcp`, preserving its determinism lineage and mapping existing children to v3 requirements; or
2. keep it as the determinism implementation epic and create a specification/review parent with an explicit relationship, if project policy allows.

Whichever is chosen, snapshot all relevant issue IDs/statuses/dependencies/comments at an as-of time; map each implementation issue to accepted `REQ-*` and verification IDs; mark pre-existing work as shipped/historical/provisional; and define allowed tracker mutations at each gate.

### Graduation and verification

The protocol does not identify the successor draft path, tracked review branch, required checks, PR state, approval artifact, or rollback. Its “full verification gate” is ambiguous. `.github/workflows/verify.yml` runs the main PR checks, while `.github/workflows/full-check.yml` is manual/tag driven and the determinism probe describes itself as evidence rather than a gate. `CONTRIBUTING.md` supplies local package check guidance but not a v3-spec traceability gate.

Correction: define exact commands/workflows and required outcomes for process acceptance, draft review, PRD graduation, implementation readiness, and pre-release separately. Graduation should be an atomic reviewed change that adds the accepted PRD, focused ADRs, supersession/status updates, trace manifest, approval record, and architecture updates together. The accepted draft hash must equal the promoted content hash.

## Severity-ordered findings

### Critical

#### C1 — The process control plane is ephemeral

**Evidence:** Protocol §5.1:72 requires state under ignored scratch; `.gitignore:23` ignores `_scratch/`; protocol, predecessor, run states, audit artifacts, and reports are untracked. `ARCHITECTURE.md:3-7`, `design/README.md:3-6`, and ADR 0008:9-15 explicitly say `_scratch` is absent from fresh clones/CI.  
**Consequence:** loss of the worktree or orchestration context destroys the process, evidence index, pending approvals, and historical draft. The central purpose in protocol §1 cannot be met.  
**Correction:** graduate the protocol and all normative/control-plane artifacts to a tracked work area before drafting. Add a content-addressed evidence manifest and a CI fresh-clone restoration/validation check.

#### C2 — Highest-precedence owner decisions are not durably identifiable

**Evidence:** Protocol §3:40 and :49 elevate the latest confirmed owner decision; no record schema defines owner, confirmation, time, scope, exact decision, evidence link, baseline, or supersession. §5.1:77 permits approved decisions to remain temporarily only in ignored state.  
**Consequence:** an agent can unknowingly apply a stale recollection, lose an approval before transfer, or invisibly override an accepted document.  
**Correction:** require immutable `OWNER-*` records and prohibit authority elevation until the record is `ACCEPTED` in the durable ledger. Pending approvals must never be the sole copy of a decision.

#### C3 — The named investigations and evidence baseline cannot be independently recovered

**Evidence:** Protocol §4:58-64 describes three reports but supplies no paths, versions, authors, dates, hashes, or source baselines. Active run state says reports are retained in orchestration context. Counts are unversioned and already drift relative to the live tracker.  
**Consequence:** a successor cannot distinguish completed evidence from an assertion that an investigation occurred, reproduce its corpus, or detect source drift.  
**Correction:** persist each report or reproducible query in the evidence manifest with repository SHA, tracker export/as-of time, tool versions, content hash, provenance, and verification status.

### High

#### H1 — Artifact schemas and transitions are underspecified

**Evidence:** Protocol §§5.1-5.4 enumerate prose fields but omit versions, owners, timestamps, revisions, transition rules, dependencies, approval evidence, invalidation, and referential-integrity checks.  
**Consequence:** separate agents can construct incompatible ledgers and mark the same broad topic “decided” while atomic assertions remain contradictory or stale.  
**Correction:** adopt versioned machine-checkable schemas, atomic IDs, explicit state machines, and a validator as specified above.

#### H2 — There is no frozen source/tracker baseline or completeness closure

**Evidence:** §6 says the ledger covers “at least” listed topics; §4 uses unpinned document/issue counts. No public-export, dial, source, test, fixture, issue, or decision inventory is required. `NAMESPACE` exposes 29 symbols; `safe_parse_url()` alone has 24 controls after `url`; tracked architecture already omits four loaded modules.  
**Consequence:** omitted surfaces cannot be detected, and evidence may change during review without invalidating prior approvals.  
**Correction:** freeze a manifest at Git SHA and tracker snapshot; generate closed inventories and coverage reports; run changed-since/delta checks before every batch and graduation.

#### H3 — Existing authority and supersession contradictions are not forced to closure

**Evidence:** accepted/shipped PRD provenance conflicts with body “Draft” status; ADR 0012 supersedes ADR 0004 while the old status remains `Accepted`; ADR 0012 has an unresolved profile-name maintainer call despite shipped code; `ARCHITECTURE.md` load order is stale.  
**Consequence:** simply registering conflicts still leaves reviewers unable to know which claim is normative.  
**Correction:** make contradiction resolution and application to affected durable sources a batch/graduation gate; validate the supersession graph and source-status metadata.

#### H4 — Draft-only red-team review cannot validate evidence claims

**Evidence:** §10:280 gives a reviewer only the draft, while §10 asks them to find unverifiable requirements; §5 requires traceability elsewhere.  
**Consequence:** the reviewer can assess internal coherence but cannot detect fabricated/stale citations, orphan requirements, inadequate fixtures, or invisible supersession.  
**Correction:** retain the blind semantic review and add an evidence review against a frozen artifact bundle and draft hash, with durable dispositions and re-review triggers.

#### H5 — Approval and graduation gates are subjective

**Evidence:** “confirmed” (§9:276), “owner approval” (§11:309), “stable” (§11:315), and “full verification gate” (§11:317) are undefined.  
**Consequence:** incompatible interpretations can move unresolved requirements into normative prose, tracker work, or release.  
**Correction:** define gate owner, input hashes, required statuses/coverage, exact checks, approval record, failure handling, and stale/reopen semantics.

#### H6 — Traceability and examples are not sufficient to prove testability

**Evidence:** §5.4 maps requirements only to source categories; §7 lists inputs/layers but not case schemas, precise expected outputs, operations/options, oracle independence, revisions, or requirement coverage. Existing tests include characterization snapshots as well as conformance-oriented fixtures.  
**Consequence:** a normative requirement may have a citation but no falsifiable verification, or a current-behavior characterization may be mistaken for a v3 conformance oracle.  
**Correction:** require `REQ-* -> DEC-* -> evidence revision -> CASE/TEST-* -> expected result` links and automated orphan/reverse-coverage checks; classify every test as compatibility, characterization, regression, or conformance.

#### H7 — Tracker scope and mutation rules are unresolved

**Evidence:** §11:313 mandates `RURL-gxqdmpcp`; its live title and history are determinism-specific. Protocol count 22 has no as-of/query definition; current live tree includes the additional review issue.  
**Consequence:** silent rechartering can obscure existing commitments, while live issue drift breaks reproducibility and premature implementation work remains possible.  
**Correction:** obtain a durable owner decision on tracker topology, snapshot it, map every issue to requirement/gate IDs, and enforce implementation-ready status transitions.

#### H8 — Source changes do not invalidate prior decisions or reviews

**Evidence:** no protocol section defines a frozen revision, changed-source check, affected-requirement propagation, or reapproval after normative edits.  
**Consequence:** approvals can silently apply to an obsolete source/draft.  
**Correction:** hash all inputs/outputs; attach dependency links; mark downstream records stale on change; require affected batch approval and reviews to rerun.

#### H9 — Historical-draft preservation is asserted, not guaranteed

**Evidence:** §8:241 protects an ignored predecessor but records neither a content hash nor archive location. The successor draft has no exact path.  
**Consequence:** an overwrite, cleanup, or worktree loss cannot be detected or recovered; reviewers cannot prove which predecessor informed the successor.  
**Correction:** snapshot the predecessor by hash in tracked storage or durable content-addressed archive, specify the successor WIP path, and record lineage.

#### H10 — Implementation delay has no enforcement mechanism

**Evidence:** §§2 and 11 say implementation decomposition follows acceptance, but ADR 0012, the predecessor, and the existing epic already contain decomposition/issues.  
**Consequence:** proposed requirements may be implemented or treated as committed before architecture acceptance.  
**Correction:** classify existing work, block new v3 implementation issue transitions until linked requirements are `ACCEPTED`/`VERIFIED`, and audit the tracker at each gate.

#### H11 — The verification gate is not mapped to actual CI behavior

**Evidence:** §11:317 says “full verification gate” without naming checks. `verify.yml` is the regular PR gate; `full-check.yml` is manual/tag triggered; the determinism probe is explicitly evidence rather than a gate.  
**Consequence:** teams can disagree whether docs/trace validation, multi-platform checks, determinism probes, package tests, or CRAN checks are required at each stage.  
**Correction:** define exact stage-specific commands/workflows, required event/branch protection, artifacts, platforms, and pass criteria.

### Moderate

#### M1 — The compact run-state schema is too lossy for handoff

**Evidence:** §5.1 stores only frontier, units, blockers, pending decisions, and paths. Existing prior run state contains an internal correction from 19 to 18 public fields.  
**Consequence:** stale summaries and path-only pointers can be mistaken for current facts.  
**Correction:** add schema/version, artifact hashes, verified-at SHA/as-of, last gate, unresolved IDs, and a validator/resume command.

#### M2 — External fixture provenance is not revision-complete

**Evidence:** tracked external-vector tests cite WPT and other source references, but key fixture metadata does not consistently pin upstream commit SHA, retrieval date, schema version, or transformation hash.  
**Consequence:** future reviewers cannot reproduce the exact oracle or distinguish upstream drift from rurl behavior changes.  
**Correction:** require source URL, upstream revision, retrieval time, license, transformation script/version, raw and derived hashes, and oracle limitations per corpus.

#### M3 — Review findings have no disposition/closure lifecycle

**Evidence:** §10:297-305 describes output content and factual verification, not assignment, disposition, closure proof, or re-review.  
**Consequence:** severe findings can be acknowledged without being resolved, or edits can invalidate earlier review unnoticed.  
**Correction:** maintain versioned `FIND-*` records with owner, severity, affected IDs, disposition, target revision, verification evidence, and reopened/stale states.

#### M4 — Batch boundaries ignore cross-cutting dependencies

**Evidence:** object state and validation are reviewed before serialization, schemes, error behavior, and migration even though these contracts constrain one another. No dependency graph or back-propagation rule exists.  
**Consequence:** later decisions can invalidate an earlier “confirmed” batch.  
**Correction:** order atomic decisions by declared dependencies and automatically stale affected downstream approvals.

#### M5 — No restore drill or single-writer responsibility is defined

**Evidence:** protocol has no backup/restore exercise, artifact owner/RACI, lock/merge policy, or responsible approver per phase.  
**Consequence:** recoverability remains assumed, and concurrent edits can corrupt or fork the control artifacts.  
**Correction:** assign artifact owners, define merge/lock policy, and require one clean-checkout restoration drill before process acceptance.

## Acceptance criteria from protocol §12

| §12 question | Yes / No | Basis and minimum correction |
|---|---:|---|
| Avoid relying on compacted conversational memory? | **No** | Reports are retained in orchestration context and core artifacts are ignored. Track/archive and manifest them. |
| Provide an explicit authority order for conflicting evidence? | **Yes** | §3 gives a clear textual order, though it still needs executable owner/source records. |
| Distinguish shipped behavior from desired v3 behavior? | **Yes** | §§3 and 5 explicitly distinguish them; enforce this per atomic claim/test. |
| Preserve owner decisions without invisible supersession? | **No** | No canonical owner-decision schema or supersession chain. Add immutable `OWNER-*` records and status transitions. |
| Force contradictions and missing decisions into visible registers? | **No** | A register is requested, but no durable path, IDs, lifecycle, ownership, or closure gate exists. |
| Cover the full semantic surface implicated by repository and epic? | **No** | “At least” prose lacks closed inventories and coverage; tracked architecture is demonstrably incomplete. |
| Define a manageable owner review sequence? | **No** | Five batches exist, but have no atomic units, dependencies, entry/exit criteria, confirmation record, or reapproval rules. |
| Make every normative requirement traceable and testable? | **No** | Trace links stop at source categories; no required verification method, case schema, reverse coverage, or oracle provenance. |
| Preserve historical drafts rather than rewrite the evidence base? | **No** | Preservation is stated, but the predecessor is ignored and unversioned with no hash/archive/restore route. |
| Delay implementation decomposition until architecture is accepted? | **No** | The rule is unenforced and existing ADR/draft/tracker decompositions are not classified or gated. |

## Open owner and process decisions

These decisions must be recorded before product-specification drafting begins:

1. **Durable process home:** tracked `design/work/url-v3/`, another tracked path, or an external content-addressed store with a tracked manifest. Recommendation: tracked `design/work/url-v3/` for control artifacts; archive only bulky raw evidence externally.
2. **Canonical owner-decision channel:** tracked ledger record, immutable Fiberplane comment linked from the ledger, or both. Recommendation: tracked record with exact immutable Fiberplane evidence link where applicable.
3. **Tracker topology:** explicitly recharter `RURL-gxqdmpcp` or preserve its determinism scope and introduce a specification/review parent. Recommendation: do not silently recharter; record the owner choice and migration map.
4. **Supersession granularity:** whole-document status versus partial decision-level supersession. Recommendation: decision-level graph plus visible document-level status notes.
5. **Evidence freeze policy:** branch/commit SHA, Fiberplane export/as-of, external corpus revisions, and when a delta invalidates review. Recommendation: freeze each owner batch and the final draft, with automatic stale propagation.
6. **Review model:** blind semantic reviewer plus evidence/trace reviewer, and which changes mandate re-review. Recommendation: require both; any normative `REQ-*` change reruns affected review.
7. **Gate definitions:** exact meaning of confirmed, approved, stable, implementation-ready, graduated, and full verification. Recommendation: machine-checkable checklists with signed/hash-bound records.
8. **Existing work classification:** how ADR 0012's plan, predecessor decomposition, and current epic children are treated before v3 acceptance. Recommendation: label as shipped, historical, provisional, or blocked; none implies v3 acceptance.
9. **CI scope:** which process/trace validators run on every PR and which package/platform/CRAN checks apply at specification, implementation, and release gates.

## Files, issues, symbols, and commands checked

### Governing and durable documentation

- `_scratch/url-v3-spec-reconstruction-protocol.md`
- `CLAUDE.md`, `FP_CLAUDE.md`, `ARCHITECTURE.md`, `CONTRIBUTING.md`
- `design/README.md`
- all files under `design/adr/` (`0000` through `0012`)
- all accepted PRDs under `design/prd/`: browser fixer, email/userinfo diagnostics, host validation, and URL-selector v1/v2
- `.gitignore`, `.Rbuildignore`, `DESCRIPTION`, `NAMESPACE`, pull-request template, and relevant `.github/workflows/`

### Historical/current working evidence

- `_scratch/prd-url-object-grammar-DRAFT.md`
- `_scratch/orchestrate/RURL-v3-architecture-spec-RUN-STATE.md`
- `_scratch/orchestrate/RURL-v3-spec-audit-RUN-STATE.md`
- `_scratch/orchestrate/RURL-iuhppfii-audit.md`
- `_scratch/orchestrate/audit/dials.md`, `LEDGER.md`, `robustness.md`, ledger CSVs, and related audit scripts/fixtures
- Existing source-backed review reports named in protocol §4 were checked only through their durable/path availability; no S1-S8 peer-review conclusions were used in this verdict.

### Source/test surface sampled

- Public exports in `NAMESPACE`
- `DESCRIPTION` Collate order
- `R/parse.R`: `safe_parse_url()`, `safe_parse_urls()`, `.parse_options`, `.URL_STANDARD_PROFILES`, `.URL_PROFILES`, profile aliases
- `R/parse-state.R`: parse-state kinds
- `R/utils.R`: supported/special schemes and default ports
- host, email, diagnostics, accessor, resolve, canonical-join, profile, and cache surfaces
- `tests/testthat/` inventory, including external URL vectors, URL-standard conformance, characterization snapshot, encoding, parse corpus, and RFC ABNF fixtures

### Fiberplane evidence (read-only)

- `RURL-gxqdmpcp` — umbrella determinism epic and full descendant tree
- `RURL-ulatmylm` — current protocol-review issue
- Open descendant work including `RURL-robgajml`, `RURL-pfewxbhb`, `RURL-gpreigqu`, and `RURL-uazyrvsk`
- Commands: `fp tree RURL-gxqdmpcp`, `fp context RURL-gxqdmpcp`, `fp context RURL-ulatmylm`, `fp log`, and read-only issue listing

### Repository/availability commands

- `git status --short --branch`
- `git rev-parse HEAD`
- `git log` over design/source history
- `git ls-files` for protocol/predecessor/run-state/audit paths
- `git check-ignore` for `_scratch/` and `.fp/`
- `rg --files`, `rg`, `nl`, `sed`, and `wc` for source/document/test inventory and line verification

No code or test suite was changed or executed. This was a read-only process audit except for this report.
