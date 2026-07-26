# Verification contracts — claim-traceability map (§6 artifact 11 — traceability map)

<!-- Verification artifact (§6 artifact 11, traceability-map slice). RCON-10 /
     S9 H6. This record is the §7 G4 CRITERION 1 instrument: it defines the
     population of normative claims mechanically, assigns every claim-bearing
     contract section to the verification slice that owns its evidence, and
     makes the orphan/reverse-coverage check executable.

     It makes NO product decision and re-decides NO contract semantics. It
     does not restate evidence either: the per-claim `file:line` evidence and
     the positive/negative coverage argument belong to the individual
     verification slices, which remain the single writers of their surfaces.
     This record is the INDEX and the CLOSURE DEVICE over them.

     Sibling to the oracle register (G4 criterion 2), by that register's own
     words: "This register is the authority axis; traceability is the coverage
     axis" (its ## Scope boundaries). The two do not overlap and neither
     subsumes the other. That register is PROPOSED on an unmerged PR, so it is
     named here rather than cited by path -- T4 rejects a path that is not on
     disk, which is the rule doing its job.

     lifecycle_state PROPOSED until a future G4 control-plane snapshot; a
     verification-family validator section and the manifest present-flip ride
     that seal, exactly as the §6 contract family did at cp-snapshot-3. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-traceability-map |
| name | verification-traceability-map |
| artifact_number | 11 (traceability map; the property-family slices are owned by their own records) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/traceability-map.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this record is the SINGLE WRITER of the claim POPULATION rule and of section→slice ownership; it is never a writer of contract semantics or of per-claim evidence |
| lifecycle_state | PROPOSED |
| verifies | §7 G4 criterion 1 over the §6 contract family (artifacts 3–12) |
| dependencies | the nine claim-bearing §6 contracts (hashed under `## Inputs`); reconciliation §6 artifact 11, §7 G4; S9 H6 / RCON-10; P5.3 (oracle policy, the authority axis) |
| closes_finding | RCON-10 (traceability half; the release-rule half stays with P0.4/C-10 and the determinism half with P5.2/C-09) |
| completion_rule | §7 G4 criterion 1 — the claim population is derived, not transcribed; every claim-bearing contract section has exactly one owner; every owner is a registered verification slice or `UNASSIGNED` with a named carrier; the generated index and census regenerate byte-identically; the gate is in the verify chain and self-tested |
| content_hash | per-input sha256 under `## Inputs`, recomputed by the verification-family validator at the sealing G4 snapshot |
| approval_evidence | pending — seals at a future G4 control-plane snapshot (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/traceability-gate.R |
| validator_note | a verification-family validator section stages with the sealing G4 snapshot; until then no validator globs design/work/url-v3/verification/ |

## Purpose

§7 G4 criterion 1 reads:

> Every normative claim maps to tests, fixtures, benchmarks, or a documented
> manual verification with exact commands.

The load-bearing word is **every**. A criterion of that shape cannot be
discharged by a list of citations, because a list cannot show what it omits.
S9 H6 says exactly this — that "a normative requirement may have a citation but
no falsifiable verification" — and asks for "automated orphan/reverse-coverage
checks" (`design/work/url-v3/evidence/S9-process-red-team.md`, H6). This record
and `design/work/url-v3/tools/traceability-gate.R` are that check.

The design follows from one decision: **the population is derived from the
contracts, never transcribed.** A hand-copied index of 479 rows is a second
copy of the contract family that begins drifting the moment either side is
edited, and the drift is silent — which is the precise failure mode criterion 1
exists to prevent. So the claim index and the coverage census below are
generated and compared byte-for-byte against a fresh derivation on every CI
run. Add a row to a contract and it appears here; the gate fails until it is
owned.

What is hand-authored is small and reviewable: which slice owns which contract
**section** (67 rows), the slice registry, and the disposition of any contract
that contributes no claims.

## Inputs

The normative sources whose rows constitute the claim population, hashed at
authoring. A future verification-family validator recomputes them at the
sealing G4 snapshot; the manifest already hash-pins the contract family as of
v3/cp-snapshot-3.

Because nothing recomputes this table *yet*, it silently went stale: the
`public-surface-closure` and `cross-artifact-consistency` entries still carried
their acceptance-1 hashes after PR #281/#284 moved both files. All eleven rows are
re-derived here (P0.6), and `public-surface-disposition.md` is added as the
eleventh source. The gap that let them drift unnoticed is real and unfixed — the
recomputation is still owed by the G4 verification-family validator, and until it
lands this table is authored, not enforced.

| path | sha256 |
|---|---|
| `design/work/url-v3/contracts/canonical-state-contract.md` | efebe54e645dfcaf56aa2e7d78fcad37d1f266952431a247d5197624996f43ca |
| `design/work/url-v3/contracts/cleaning-mutation-contracts.md` | d8996daf621ca9409d7249116c15efa1740379eda836eb4ce81aa9278ff8a255 |
| `design/work/url-v3/contracts/cross-artifact-consistency.md` | 4931bde27370e5f95f6bfeeaaac67e6f6f3356862d5cd570a222db52b5073947 |
| `design/work/url-v3/contracts/host-annotation-contracts.md` | ec67597447dd0c57dd8c0c7bc9e2216d6bc3c0ee75e956ec729e9a499b551ee0 |
| `design/work/url-v3/contracts/key-join-contracts.md` | c8ab02251a2dda7760265ab32a889338134d81f938e0fba2a85de48d1063a7d3 |
| `design/work/url-v3/contracts/output-contracts.md` | d0570174098f2caf454acfcae7a25517c16fd11c703e7c07242cb221d3dff455 |
| `design/work/url-v3/contracts/public-surface-closure.md` | 9a1cd6c68141c55386818a6ec43e8aeecef973417ce442923e5a156a012e782a |
| `design/work/url-v3/contracts/public-surface-disposition.md` | 9f38f465bffba1da3bb7ac457158ca8452bdd80f265a09c4cf706508192d8ad7 |
| `design/work/url-v3/contracts/semantic-cache-contract.md` | a70acc712e3db5f1426d45106e925d445af048df03086a230e04a4545fc67001 |
| `design/work/url-v3/contracts/standard-scheme-matrices.md` | f36ddf6cb8283af7203e7267c2bb1635df49fc670129134cfe9825cda5589d18 |
| `design/work/url-v3/contracts/validation-intervention-contract.md` | aa06d046d495c94f5f1cba5bef923c666653cdceaedabc2ebee66d86d2679c18 |

## Population rule

> A **normative claim** is a data row of a pipe table in a §6 contract whose
> **last cell begins with `SETTLED` or `OPEN`**.

That is the status column the contract family already uses on every normative
table, so the rule reads the population off the contracts rather than
re-deciding it. Envelope, Inputs, legend, and prose tables carry no status
column and are therefore not claims — a property of how the contracts were
written, not an exclusion applied here.

Three consequences worth stating plainly:

- **`OPEN` cells are in the population.** An undecided cell is still a claim
  the map must account for; it simply cannot be discharged by evidence yet.
  Dropping them would make the census flatter and the artifact useless.
- **The id is derived, not assigned.** `TR-<contract>-s<n>-<key-slug>`, where
  `<n>` is the ordinal of the claim-bearing section within the file and the
  slug comes from the row's first cell. Ids are therefore stable against prose
  edits and against changes to sections that carry no claims. Where one
  section repeats a row key (the port truth table lists `HTTP absent port`
  under two profiles) the duplicates take a deterministic `-2` suffix.
- **Nothing is listed by hand, so nothing can be omitted by hand.** This is
  the whole point. The failure mode for a traceability artifact is not a wrong
  row, it is a missing one.

The current population is **479 claims across 9 contracts and 67 sections**.

## Verification slices

The slices that may own a contract section. The eight §7 G4 criterion-3
property families, plus the release slice (C-10/P0.4) and the host slice that
`design/work/url-v3/verification/cache-slice.md` already names as a boundary
("owned by **G3.H** and its future G4 verification slice"). Adding a name to
this table is adding scope, which is why the gate refuses any owner not on it.

`state` is `SHIPPED` when the file exists and `OWED` when it does not — the
gate checks both directions against disk, so authoring a slice without
re-owning its sections here fails.

| slice_id | tracked_path | state |
|---|---|---|
| cache-slice | design/work/url-v3/verification/cache-slice.md | SHIPPED |
| determinism-slice | design/work/url-v3/verification/determinism-slice.md | SHIPPED |
| release-slice | design/work/url-v3/verification/release-slice.md | SHIPPED |
| state-slice | design/work/url-v3/verification/state-slice.md | OWED |
| full-string-slice | design/work/url-v3/verification/full-string-slice.md | OWED |
| vector-slice | design/work/url-v3/verification/vector-slice.md | OWED |
| mutation-slice | design/work/url-v3/verification/mutation-slice.md | OWED |
| join-slice | design/work/url-v3/verification/join-slice.md | OWED |
| migration-slice | design/work/url-v3/verification/migration-slice.md | OWED |
| host-slice | design/work/url-v3/verification/host-slice.md | OWED |

**Three shipped slices own no contract section.**
`determinism-slice` and `release-slice` verify decision records (P5.2/C-09 and
P0.4/C-10) rather than contract table rows, and they cite no contract by path —
consistent with owning nothing here. `vector-slice` is different and is a
finding: see `## Open cells`.

## Section ownership

One row per claim-bearing contract section, assigned to the slice whose
property family verifies those claims at runtime. Where a section's claims are
dispositions, inventories, or cross-artifact agreements rather than runtime
properties, or where no registered slice's family covers them, the row is
`UNASSIGNED` with a carrier — the same posture the oracle register takes with
`UNLABELED`, and for the same reason: **inventing a slice name to make the
table tidy would freeze an arbitrary answer to a question the owner has not
been asked.**

| contract | sec | section | owning_slice | carrier |
|---|---|---|---|---|
| CS | s1 | Rows | state-slice | — |
| CM | s1 | Cleaning surface contract (surface c) | mutation-slice | — |
| CM | s2 | Cleaning-semantics matrix (the 25 shipped dials) | mutation-slice | — |
| CM | s3 | Cleaning processing-order rows | mutation-slice | — |
| CM | s4 | Downstream-capability classification (S4 F13) | mutation-slice | — |
| CM | s5 | Cleaner / identity non-interference invariant (P3.1 D-A.3) | mutation-slice | — |
| CM | s6 | Cleaning repair-provenance rows (S4 F3) | mutation-slice | — |
| CM | s7 | Credential / security rows (S4 F12) | mutation-slice | — |
| CM | s8 | Mutation contract — status and cells | mutation-slice | — |
| HA | s1 | Host presence + syntactic-form rows | host-slice | — |
| HA | s2 | Scheme-forced host posture rows (reference G3.5 / P4.1) | host-slice | — |
| HA | s3 | IDNA / rendering operation-distinctness rows | host-slice | — |
| HA | s4 | PSL annotation rows (reference ADR 0001 / G3.9) | host-slice | — |
| HA | s5 | Host annotation-state application rows (semantics owned by G3.6) | host-slice | — |
| HA | s6 | DNS-policy / no-network rows (reference host-validation-policy PRD) | host-slice | — |
| HA | s7 | IP / numeric-host rows | host-slice | — |
| HA | s8 | Full-host-identity rows | host-slice | — |
| HA | s9 | External-oracle / provenance rows (reference P5.3 / §6 artifact 11) | host-slice | — |
| HA | s10 | Cost / cache — delegated to G3.9 | host-slice | — |
| KJ | s1 | Key surface rows | join-slice | — |
| KJ | s2 | Key-policy rows | join-slice | — |
| KJ | s3 | Scheme and port truth table | join-slice | — |
| KJ | s4 | Eligibility and collision rows | join-slice | — |
| KJ | s5 | Six-join matrix | join-slice | — |
| KJ | s6 | Cross-cutting join rows | join-slice | — |
| KJ | s7 | `canonical_join()` migration rows | migration-slice | — |
| OUT | s1 | Five output surfaces | full-string-slice | — |
| OUT | s2 | Surface-assignment invariants (P2.2 §5) | full-string-slice | — |
| OUT | s3 | Standard serialization — the FSSS (surface b) | full-string-slice | — |
| OUT | s4 | Lossless serializer-input record (S3-F2 / S3-F5) | full-string-slice | — |
| OUT | s5 | Source reproduction (surface a) | full-string-slice | — |
| OUT | s6 | Clean output (surface c) | mutation-slice | — |
| OUT | s7 | Safe display (surface d) | full-string-slice | — |
| OUT | s8 | Comparison key (surface e) — reference to G3.K | join-slice | — |
| OUT | s9 | Claim / oracle policy over the FSSS (P5.3) | full-string-slice | — |
| OUT | s10 | Capability classification (S3-F6/F13) | full-string-slice | — |
| OUT | s11 | Encoding / locale-invariance rows (S3-F7) | full-string-slice | — |
| OUT | s12 | Credential handling / undivided-userinfo (owns S1 Q5, deferred from G3.3) | full-string-slice | — |
| PS | s1 | Cross-artifact term / status / state-field agreement | UNASSIGNED | RURL-sunrlgio |
| PSD | s1 | Bijection | UNASSIGNED | RURL-sunrlgio |
| PSD | s2 | Exported-function disposition roster | UNASSIGNED | RURL-sunrlgio |
| PSD | s3 | Public-output-field disposition roster | UNASSIGNED | RURL-sunrlgio |
| PSD | s4 | curl-dependency + migration-surface disposition | migration-slice | — |
| SC | s1 | Cache inventory rows | cache-slice | — |
| SC | s2 | Key-partition and external-data-versioning rows | cache-slice | — |
| SC | s3 | Bound, eviction, and clearing rows | cache-slice | — |
| SC | s4 | Semantic-transparency invariant (the load-bearing rule) | cache-slice | — |
| SC | s5 | C-08 disposition and documentation-consistency rows | cache-slice | — |
| SS | s1 | Axis lattice and profile-expansion rows | UNASSIGNED | RURL-lkyverse |
| SS | s2 | Precedence rows | UNASSIGNED | RURL-lkyverse |
| SS | s3 | Admission gate (input class × axis setting) | UNASSIGNED | RURL-lkyverse |
| SS | s4 | Interpretation / parser-route rows | UNASSIGNED | RURL-lkyverse |
| SS | s5 | Scheme-family specialization and eligibility rows | UNASSIGNED | RURL-lkyverse |
| SS | s6 | Credential rows | UNASSIGNED | RURL-lkyverse |
| SS | s7 | Email rows | UNASSIGNED | RURL-lkyverse |
| SS | s8 | Resolution rows | UNASSIGNED | RURL-lkyverse |
| SS | s9 | Diagnostics rows | UNASSIGNED | RURL-lkyverse |
| VI | s1 | Repair-posture axis rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s2 | Ordered pipeline / intervention-ledger rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s3 | Verdict-layer rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s4 | Shipped-value → layer map (the C-07 map) | UNASSIGNED | RURL-jdnlpydz |
| VI | s5 | Annotation-state resolution rows (S7-F3) | UNASSIGNED | RURL-jdnlpydz |
| VI | s6 | π collapse table (legacy `parse_status` projection) | UNASSIGNED | RURL-jdnlpydz |
| VI | s7 | Repair / recovery provenance rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s8 | Repaired-input revalidation rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s9 | Resolution verdict rows (`resolve_url`) | UNASSIGNED | RURL-jdnlpydz |
| VI | s10 | Companion-helper surface + migration rows | migration-slice | — |

### Why these assignments, and where they are contestable

- **`state-slice` is scoped to `canonical-state-contract.md` §Rows (44 cells)**
  on the evidence of the prior state audit, which reported 35 of 44 cells
  already covered both ways. The wider reading — that the state slice also
  covers the four `validation-intervention-contract.md` verdict sections (VI
  s3–s6), which elaborate `layer1_syntax_verdict`, `layer2_policy_verdict`,
  `layer3_annotation_state`, and `parse_status`, all of them fields §Rows
  already owns — is defensible and would move 37 claims. `RURL-jdnlpydz`
  carries the question; the narrower reading is recorded here because it is the
  one with evidence behind it, not because it is obviously right.
- **`OUT` §Encoding / locale-invariance rows goes to `full-string-slice`, not
  `determinism-slice`.** Locale invariance of *serialized output* is a property
  of the serializer, and the shipped determinism slice scopes itself to the
  cross-OS/R/charset determinism of parse outputs. Assigning it to the
  determinism slice would also have required editing a sibling record, which
  this one has no writer authority over.
- **Cleaning claims follow the cleaning family across contract boundaries.**
  `OUT` §Clean output (surface c) goes to `mutation-slice` alongside the
  `CM` cleaning sections, so one slice owns surface c rather than two owning
  half each. The same logic sends `OUT` §Comparison key to `join-slice`.

## Claim index

Generated. Do not edit by hand — the gate compares this block byte-for-byte
against a fresh derivation from the contracts and fails on any difference.
`coverage` is derived, never asserted: `MAPPED` means the owning slice is on
disk, `PENDING` that it is owed, `UNASSIGNED` that no registered slice's family
covers the section.

<!-- BEGIN GENERATED: claim-index -- regenerate with:
| claim_id | status | owning_slice | coverage | source |
|---|---|---|---|---|
| TR-CS-s1-original-url | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:101` |
| TR-CS-s1-scheme | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:102` |
| TR-CS-s1-final-scheme | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:103` |
| TR-CS-s1-host | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:104` |
| TR-CS-s1-final-host | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:105` |
| TR-CS-s1-host-kind | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:106` |
| TR-CS-s1-is-ip-host | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:107` |
| TR-CS-s1-host-is-ace | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:108` |
| TR-CS-s1-whatwg-host-form | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:109` |
| TR-CS-s1-rfc-host-form | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:110` |
| TR-CS-s1-authority-kind | OPEN | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:111` |
| TR-CS-s1-user | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:112` |
| TR-CS-s1-password | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:113` |
| TR-CS-s1-raw-user | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:114` |
| TR-CS-s1-raw-password | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:115` |
| TR-CS-s1-scheme-less-userinfo | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:116` |
| TR-CS-s1-port | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:117` |
| TR-CS-s1-raw-port | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:118` |
| TR-CS-s1-path | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:119` |
| TR-CS-s1-raw-path | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:120` |
| TR-CS-s1-path-kind | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:121` |
| TR-CS-s1-rfc-path-form | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:122` |
| TR-CS-s1-rfc3986-path-rootless | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:123` |
| TR-CS-s1-query | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:124` |
| TR-CS-s1-raw-query | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:125` |
| TR-CS-s1-query-kind | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:126` |
| TR-CS-s1-fragment | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:127` |
| TR-CS-s1-raw-fragment | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:128` |
| TR-CS-s1-fragment-kind | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:129` |
| TR-CS-s1-domain | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:130` |
| TR-CS-s1-tld | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:131` |
| TR-CS-s1-domain-ascii | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:132` |
| TR-CS-s1-domain-unicode | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:133` |
| TR-CS-s1-tld-ascii | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:134` |
| TR-CS-s1-tld-unicode | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:135` |
| TR-CS-s1-clean-url | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:136` |
| TR-CS-s1-parse-status | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:137` |
| TR-CS-s1-layer1-syntax-verdict | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:138` |
| TR-CS-s1-layer2-policy-verdict | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:139` |
| TR-CS-s1-layer3-annotation-state | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:140` |
| TR-CS-s1-looks-like-protocol | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:141` |
| TR-CS-s1-original-has-allowed-scheme | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:142` |
| TR-CS-s1-is-scheme-relative | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:143` |
| TR-CS-s1-looks-like-host-port | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/canonical-state-contract.md:144` |
| TR-CM-s1-identity | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:93` |
| TR-CM-s1-fragment-credentials | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:94` |
| TR-CM-s1-byte-compat-scope | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:95` |
| TR-CM-s1-lossy-vs-valid-fork-s4-f2 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:96` |
| TR-CM-s2-1 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:110` |
| TR-CM-s2-2 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:111` |
| TR-CM-s2-3 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:112` |
| TR-CM-s2-4 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:113` |
| TR-CM-s2-5 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:114` |
| TR-CM-s2-6 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:115` |
| TR-CM-s2-7 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:116` |
| TR-CM-s2-8 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:117` |
| TR-CM-s2-9 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:118` |
| TR-CM-s2-10 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:119` |
| TR-CM-s2-11 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:120` |
| TR-CM-s2-12 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:121` |
| TR-CM-s2-13-19 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:122` |
| TR-CM-s2-20 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:123` |
| TR-CM-s2-21 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:124` |
| TR-CM-s2-22 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:125` |
| TR-CM-s2-23 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:126` |
| TR-CM-s2-24 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:127` |
| TR-CM-s2-25 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:128` |
| TR-CM-s2-internal | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:129` |
| TR-CM-s2-eligibility | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:130` |
| TR-CM-s2-vocabulary | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:131` |
| TR-CM-s3-1 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:142` |
| TR-CM-s3-2 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:143` |
| TR-CM-s3-3 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:144` |
| TR-CM-s3-4 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:145` |
| TR-CM-s3-5 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:146` |
| TR-CM-s3-6 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:147` |
| TR-CM-s3-7 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:148` |
| TR-CM-s3-8 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:149` |
| TR-CM-s3-9 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:150` |
| TR-CM-s4-clean-url-get-clean-url-surface- | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:159` |
| TR-CM-s4-comparison-key-get-url-key | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:160` |
| TR-CM-s4-standard-serialization-serialize | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:161` |
| TR-CM-s5-non-interference | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:170` |
| TR-CM-s5-the-defect-it-fixes | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:171` |
| TR-CM-s5-test-revision-obligation | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:172` |
| TR-CM-s6-cleaning-repair | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:181` |
| TR-CM-s6-strict-default-admission-flow-th | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:182` |
| TR-CM-s6-s4-f3-provenance-boundary | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:183` |
| TR-CM-s7-clean-surface-never-reconstructs | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:189` |
| TR-CM-s7-safe-display-redacts | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:190` |
| TR-CM-s7-mutation-credential-safety | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:191` |
| TR-CM-s8-semantics-verb-vocabulary | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:206` |
| TR-CM-s8-component-dependency-graph | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:207` |
| TR-CM-s8-state-transitions | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:208` |
| TR-CM-s8-transaction-behavior | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:209` |
| TR-CM-s8-eligibility | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:210` |
| TR-CM-s8-security | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:211` |
| TR-CM-s8-invariants | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:212` |
| TR-CM-s8-query-model | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:213` |
| TR-CM-s8-path-model | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:214` |
| TR-CM-s8-psl-binding | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:215` |
| TR-CM-s8-standards-profile-repair-interac | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:216` |
| TR-CM-s8-downstream-safety | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:217` |
| TR-HA-s1-host-presence | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:96` |
| TR-HA-s1-whatwg-host-form | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:97` |
| TR-HA-s1-rfc-host-form | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:98` |
| TR-HA-s1-public-host-type-syntactic-form | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:99` |
| TR-HA-s2-special-http-https-ws-wss-ftp-fi | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:108` |
| TR-HA-s2-file | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:109` |
| TR-HA-s2-ftp | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:110` |
| TR-HA-s2-mailto | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:111` |
| TR-HA-s2-opaque-non-special-authority-ip- | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:112` |
| TR-HA-s3-whatwg-domain-to-ascii-the-parse | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:122` |
| TR-HA-s3-reversible-a-label-u-label-rende | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:123` |
| TR-HA-s3-best-effort-display-decoding | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:124` |
| TR-HA-s3-these-three-are-distinct | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:125` |
| TR-HA-s3-v3-canonical-idna-pinning | OPEN | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:126` |
| TR-HA-s4-delegation | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:136` |
| TR-HA-s4-section-vocabulary | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:137` |
| TR-HA-s4-identity-spellings | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:138` |
| TR-HA-s4-registrability-meaning | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:139` |
| TR-HA-s4-psl-reproducibility-provenance | OPEN | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:140` |
| TR-HA-s4-engine-identity-cache | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:141` |
| TR-HA-s5-typed-states | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:151` |
| TR-HA-s5-l1-l2-invariance | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:152` |
| TR-HA-s5-per-host-form-state | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:153` |
| TR-HA-s5-root-dot-identity | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:154` |
| TR-HA-s5-core-status-ownership | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:155` |
| TR-HA-s6-no-network-i-o | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:165` |
| TR-HA-s6-named-policies | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:166` |
| TR-HA-s6-length-basis | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:167` |
| TR-HA-s6-typed-dns-policy-contract | OPEN | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:168` |
| TR-HA-s6-rfc-reg-name-dns-host-eligibilit | OPEN | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:169` |
| TR-HA-s6-live-resolution | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:170` |
| TR-HA-s7-whatwg-numeric-model | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:179` |
| TR-HA-s7-rfc-numeric-model | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:180` |
| TR-HA-s7-ip-annotation-ineligibility | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:181` |
| TR-HA-s7-full-typed-ip-row-set | OPEN | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:182` |
| TR-HA-s8-existing-keys-are-psl-not-full-h | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:192` |
| TR-HA-s8-full-host-identity-fields | OPEN | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:193` |
| TR-HA-s9-plural-labeled-oracles | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:202` |
| TR-HA-s9-dns-length-is-a-separate-column | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:203` |
| TR-HA-s9-per-fixture-provenance | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:204` |
| TR-HA-s10-annotation-cost-cache | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/host-annotation-contracts.md:210` |
| TR-KJ-s1-get-url-key-url-policy-url-key-p | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:65` |
| TR-KJ-s1-url-key-policy | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:66` |
| TR-KJ-s1-key-representation | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:67` |
| TR-KJ-s1-identity-input | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:68` |
| TR-KJ-s1-non-interference | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:69` |
| TR-KJ-s1-interpretation-selector | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:70` |
| TR-KJ-s1-diagnostic-surface | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:71` |
| TR-KJ-s2-scheme-source | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:80` |
| TR-KJ-s2-scheme-equality | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:81` |
| TR-KJ-s2-scheme-case | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:82` |
| TR-KJ-s2-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:83` |
| TR-KJ-s2-authority | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:84` |
| TR-KJ-s2-host-kind | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:85` |
| TR-KJ-s2-domain-spelling | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:86` |
| TR-KJ-s2-host-editing | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:87` |
| TR-KJ-s2-path | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:88` |
| TR-KJ-s2-path-display-editing | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:89` |
| TR-KJ-s2-query-presence | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:90` |
| TR-KJ-s2-query-structure | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:91` |
| TR-KJ-s2-fragment | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:92` |
| TR-KJ-s2-userinfo | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:93` |
| TR-KJ-s2-missing-invalid | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:94` |
| TR-KJ-s2-persisted-key-stability | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:95` |
| TR-KJ-s3-http-absent-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:105` |
| TR-KJ-s3-https-absent-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:106` |
| TR-KJ-s3-http-absent-port-2 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:107` |
| TR-KJ-s3-https-absent-port-2 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:108` |
| TR-KJ-s3-http-443 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:109` |
| TR-KJ-s3-http-80 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:110` |
| TR-KJ-s3-http-absent | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:111` |
| TR-KJ-s3-missing-scheme-no-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:112` |
| TR-KJ-s3-missing-scheme-80 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:113` |
| TR-KJ-s3-missing-scheme-443 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:114` |
| TR-KJ-s3-scheme-relative-no-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:115` |
| TR-KJ-s3-ftp-21 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:116` |
| TR-KJ-s3-ws-80-wss-443 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:117` |
| TR-KJ-s3-custom-123 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:118` |
| TR-KJ-s4-eligibility-vocabulary | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:124` |
| TR-KJ-s4-warning-rows | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:125` |
| TR-KJ-s4-non-keyable-rows | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:126` |
| TR-KJ-s4-missing-matching | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:127` |
| TR-KJ-s4-key-collision | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:128` |
| TR-KJ-s4-duplicate-keys | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:129` |
| TR-KJ-s4-resource-guard | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:130` |
| TR-KJ-s5-url-inner-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:140` |
| TR-KJ-s5-url-left-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:141` |
| TR-KJ-s5-url-right-join-x-y | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:142` |
| TR-KJ-s5-url-full-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:143` |
| TR-KJ-s5-url-semi-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:144` |
| TR-KJ-s5-url-anti-join-x-y | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:145` |
| TR-KJ-s6-url-columns | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:151` |
| TR-KJ-s6-key-policy | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:152` |
| TR-KJ-s6-parse-policy | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:153` |
| TR-KJ-s6-relationship | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:154` |
| TR-KJ-s6-multiple-matches | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:155` |
| TR-KJ-s6-duplicate-counts | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:156` |
| TR-KJ-s6-invalid-warnings | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:157` |
| TR-KJ-s6-key-visibility | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:158` |
| TR-KJ-s6-original-urls | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:159` |
| TR-KJ-s6-suffix-name-repair | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:160` |
| TR-KJ-s6-row-order-except-right-join | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:161` |
| TR-KJ-s6-type-attributes | OPEN | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:162` |
| TR-KJ-s6-conditions | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:163` |
| TR-KJ-s6-resource-bound | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:164` |
| TR-KJ-s6-diagnostics | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:165` |
| TR-KJ-s7-legacy-freeze | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:175` |
| TR-KJ-s7-implicit-equality-dials | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:176` |
| TR-KJ-s7-identity-opt-in | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:177` |
| TR-KJ-s7-six-join-replacement | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:178` |
| TR-KJ-s7-audit-before-switch | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:179` |
| TR-KJ-s7-current-formal-migration | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:180` |
| TR-KJ-s7-forwarded-parse-dials | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:181` |
| TR-KJ-s7-forwarded-display-cleaning-dials | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:182` |
| TR-KJ-s7-path-encoding-regression | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:183` |
| TR-KJ-s7-removal-default-flip | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:184` |
| TR-OUT-s1-a | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:90` |
| TR-OUT-s1-b | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:91` |
| TR-OUT-s1-c | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:92` |
| TR-OUT-s1-d | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:93` |
| TR-OUT-s1-e | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:94` |
| TR-OUT-s2-1 | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:102` |
| TR-OUT-s2-2 | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:103` |
| TR-OUT-s2-3 | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:104` |
| TR-OUT-s2-4 | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:105` |
| TR-OUT-s2-5 | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:106` |
| TR-OUT-s3-pair-of-spec-exact-serializers | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:115` |
| TR-OUT-s3-full-string-fragment-credential- | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:116` |
| TR-OUT-s3-consumes-the-lossless-record | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:117` |
| TR-OUT-s3-correctness-oracle | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:118` |
| TR-OUT-s3-c-04-fragment-preservation | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:119` |
| TR-OUT-s3-c-05-2f-path-encoding | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:120` |
| TR-OUT-s3-build-dependency | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:121` |
| TR-OUT-s3-public-entry-point-name | OPEN | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:122` |
| TR-OUT-s3-rfc-serializer-posture | OPEN | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:123` |
| TR-OUT-s4-authority-host-state | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:134` |
| TR-OUT-s4-credential-delimiter-state | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:135` |
| TR-OUT-s4-query-lexical-state | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:136` |
| TR-OUT-s4-query-fragment-presence | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:137` |
| TR-OUT-s4-path-posture | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:138` |
| TR-OUT-s4-parse-repair-provenance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:139` |
| TR-OUT-s5-own-surface-never-a-serializer | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:145` |
| TR-OUT-s5-byte-vs-encoding-label-guarantee | OPEN | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:146` |
| TR-OUT-s6-intentionally-lossy-seo-product | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:155` |
| TR-OUT-s6-omits-fragment-credentials | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:156` |
| TR-OUT-s6-byte-compat-scope | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:157` |
| TR-OUT-s6-dials-owned-elsewhere | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:158` |
| TR-OUT-s6-resolve-url-coupling | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:159` |
| TR-OUT-s7-separate-surface | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:165` |
| TR-OUT-s7-redacts-credentials | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:166` |
| TR-OUT-s7-scope-escape-annotation-matrix | OPEN | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:167` |
| TR-OUT-s8-identity-never-presentation | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:177` |
| TR-OUT-s8-non-interference | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:178` |
| TR-OUT-s8-key-policy-truth-tables-joins-mi | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:179` |
| TR-OUT-s9-claims-against-the-fsss-only | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:189` |
| TR-OUT-s9-the-projection-may-not-be-an-ora | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:190` |
| TR-OUT-s9-historical-projection-claims-ret | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:191` |
| TR-OUT-s9-parity-conformance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:192` |
| TR-OUT-s9-labeled-oracle-taxonomy | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:193` |
| TR-OUT-s9-metamorphic-assertions-required | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:194` |
| TR-OUT-s9-oracle-register-budgets | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:195` |
| TR-OUT-s10-url-source-a | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:204` |
| TR-OUT-s10-serialize-url-b | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:205` |
| TR-OUT-s10-clean-url-c | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:206` |
| TR-OUT-s10-format-url-d | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:207` |
| TR-OUT-s10-get-url-key-e | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:208` |
| TR-OUT-s11-utf-8-marking | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:216` |
| TR-OUT-s11-locale-invariance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:217` |
| TR-OUT-s11-raw-byte-vs-percent-spelling-dis | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:218` |
| TR-OUT-s12-user-password-split-retained | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:230` |
| TR-OUT-s12-internal-reassembly-completeness | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:231` |
| TR-OUT-s12-output-governance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:232` |
| TR-OUT-s12-public-undivided-userinfo-compon | OPEN | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:233` |
| TR-PS-s1-the-18-public-fields-three-value | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:163` |
| TR-PS-s1-parse-status-l1-l2-l3-compat-pro | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:164` |
| TR-PS-s1-clean-url-clean-surface-c-not-id | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:165` |
| TR-PS-s1-comparison-key-get-url-key-not-c | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:166` |
| TR-PS-s1-host-domain-psl-registrable-not- | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:167` |
| TR-PS-s1-cache-config-info-clear-semantic | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:168` |
| TR-PS-s1-scheme-admission-interpretation- | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:169` |
| TR-PS-s1-companion-helpers-never-widen-th | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:170` |
| TR-PSD-s1-exported-functions | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:82` |
| TR-PSD-s1-public-output-fields | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:83` |
| TR-PSD-s1-curl-dependency-surfaces | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:84` |
| TR-PSD-s1-migration-surface | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:85` |
| TR-PSD-s2-canonical-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:101` |
| TR-PSD-s2-check-hosts | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:102` |
| TR-PSD-s2-get-clean-url | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:103` |
| TR-PSD-s2-get-domain | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:104` |
| TR-PSD-s2-get-fragment | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:105` |
| TR-PSD-s2-get-host | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:106` |
| TR-PSD-s2-get-host-type | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:107` |
| TR-PSD-s2-get-mailto-recipients | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:108` |
| TR-PSD-s2-get-parse-status | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:109` |
| TR-PSD-s2-get-parse-verdicts | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:110` |
| TR-PSD-s2-get-password | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:111` |
| TR-PSD-s2-get-path | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:112` |
| TR-PSD-s2-get-port | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:113` |
| TR-PSD-s2-get-query | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:114` |
| TR-PSD-s2-get-scheme | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:115` |
| TR-PSD-s2-get-scheme-class | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:116` |
| TR-PSD-s2-get-subdomain | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:117` |
| TR-PSD-s2-get-tld | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:118` |
| TR-PSD-s2-get-url-diagnostics | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:119` |
| TR-PSD-s2-get-user | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:120` |
| TR-PSD-s2-get-userinfo | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:121` |
| TR-PSD-s2-is-valid-host | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:122` |
| TR-PSD-s2-query-param-summary | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:123` |
| TR-PSD-s2-resolve-url | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:124` |
| TR-PSD-s2-rurl-cache-config | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:125` |
| TR-PSD-s2-rurl-cache-info | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:126` |
| TR-PSD-s2-rurl-clear-caches | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:127` |
| TR-PSD-s2-safe-parse-url | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:128` |
| TR-PSD-s2-safe-parse-urls | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:129` |
| TR-PSD-s2-url-profile | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:130` |
| TR-PSD-s3-original-url | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:136` |
| TR-PSD-s3-scheme | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:137` |
| TR-PSD-s3-host | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:138` |
| TR-PSD-s3-port | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:139` |
| TR-PSD-s3-path | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:140` |
| TR-PSD-s3-query | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:141` |
| TR-PSD-s3-fragment | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:142` |
| TR-PSD-s3-user | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:143` |
| TR-PSD-s3-password | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:144` |
| TR-PSD-s3-domain | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:145` |
| TR-PSD-s3-tld | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:146` |
| TR-PSD-s3-domain-ascii | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:147` |
| TR-PSD-s3-domain-unicode | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:148` |
| TR-PSD-s3-tld-ascii | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:149` |
| TR-PSD-s3-tld-unicode | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:150` |
| TR-PSD-s3-is-ip-host | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:151` |
| TR-PSD-s3-clean-url | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:152` |
| TR-PSD-s3-parse-status | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:153` |
| TR-PSD-s4-curl-import-metadata | OPEN | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:159` |
| TR-PSD-s4-curl-parse-call | OPEN | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:160` |
| TR-PSD-s4-curl-escape-unescape | OPEN | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:161` |
| TR-PSD-s4-migration-surface | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:162` |
| TR-SC-s1-full-parse | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:77` |
| TR-SC-s1-puny-encode | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:78` |
| TR-SC-s1-puny-decode | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:79` |
| TR-SC-s2-full-parse-key-completeness | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:87` |
| TR-SC-s2-external-data-version-in-key | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:88` |
| TR-SC-s2-punycode-key-scope | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:89` |
| TR-SC-s3-full-parse-default-bound | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:95` |
| TR-SC-s3-full-parse-eviction | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:96` |
| TR-SC-s3-rurl-cache-info | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:97` |
| TR-SC-s3-rurl-cache-config | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:98` |
| TR-SC-s3-rurl-clear-caches | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:99` |
| TR-SC-s4-cold-vs-warm | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:111` |
| TR-SC-s4-enabled-vs-disabled | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:112` |
| TR-SC-s4-bounded-vs-unbounded | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:113` |
| TR-SC-s4-pre-vs-post-eviction | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:114` |
| TR-SC-s4-scalar-loop-vs-one-vector-call-v | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:115` |
| TR-SC-s4-external-data-stability | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:116` |
| TR-SC-s5-c-08-authoritative-side | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:128` |
| TR-SC-s5-readme-correction | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:129` |
| TR-SC-s5-documentation-consistency-gate | SETTLED | cache-slice | MAPPED | `design/work/url-v3/contracts/semantic-cache-contract.md:130` |
| TR-SS-s1-scheme-acceptance | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:80` |
| TR-SS-s1-scheme-policy | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:81` |
| TR-SS-s1-url-standard | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:82` |
| TR-SS-s1-composition-rule | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:83` |
| TR-SS-s1-browser | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:89` |
| TR-SS-s1-whatwg | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:90` |
| TR-SS-s1-rfc-syntax | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:91` |
| TR-SS-s1-seo-alias-canonical | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:92` |
| TR-SS-s2-explicit-args-win | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:98` |
| TR-SS-s2-strict-override-scoping | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:99` |
| TR-SS-s2-direct-vs-profile-scheme-policy- | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:100` |
| TR-SS-s3-http-https | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:109` |
| TR-SS-s3-ftp | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:110` |
| TR-SS-s3-ftps | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:111` |
| TR-SS-s3-file | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:112` |
| TR-SS-s3-ws-wss | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:113` |
| TR-SS-s3-mailto-tel-data-sftp-arbitrary-f | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:114` |
| TR-SS-s3-scheme-less-host-shaped-example- | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:115` |
| TR-SS-s3-scheme-relative-host | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:116` |
| TR-SS-s4-whatwg-special-set | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:126` |
| TR-SS-s4-special-missing-host | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:127` |
| TR-SS-s4-whatwg-non-special-shapes | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:128` |
| TR-SS-s4-rfc-3986-route | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:129` |
| TR-SS-s4-opaque-path-trigger | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:130` |
| TR-SS-s4-authority-emission-binds-p1-2 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:131` |
| TR-SS-s5-http-https | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:137` |
| TR-SS-s5-ftp | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:138` |
| TR-SS-s5-ftps | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:139` |
| TR-SS-s5-sftp | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:140` |
| TR-SS-s5-file | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:141` |
| TR-SS-s5-ws-wss | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:142` |
| TR-SS-s5-mailto | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:143` |
| TR-SS-s5-tel-data-arbitrary-foo | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:144` |
| TR-SS-s5-default-port-data | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:145` |
| TR-SS-s6-five-distinct-credential-routes | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:151` |
| TR-SS-s6-generic-authority-credential-pre | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:152` |
| TR-SS-s6-credential-output-policy-boundar | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:153` |
| TR-SS-s6-get-password-selector-parity | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:154` |
| TR-SS-s7-recipient-projection | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:160` |
| TR-SS-s7-extraction-is-metadata-only | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:161` |
| TR-SS-s7-first-local-part-decode | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:162` |
| TR-SS-s7-email-helper-surface | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:163` |
| TR-SS-s7-indeterminate-lexer-url-level-em | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:164` |
| TR-SS-s8-scheme-less-input | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:170` |
| TR-SS-s8-scheme-relative-host | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:171` |
| TR-SS-s8-rfc-relative-reference-resolutio | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:172` |
| TR-SS-s9-get-scheme-class | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:178` |
| TR-SS-s9-selected-scheme-facts | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:179` |
| TR-SS-s9-absence-is-not-conformance | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:180` |
| TR-SS-s9-email-diagnostics | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:181` |
| TR-VI-s1-strict | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:88` |
| TR-VI-s1-compatibility | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:89` |
| TR-VI-s1-repair | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:90` |
| TR-VI-s1-posture-per-axis-default-not-a-g | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:91` |
| TR-VI-s1-3-0-supersession-recorded-not-si | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:92` |
| TR-VI-s2-1 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:107` |
| TR-VI-s2-2 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:108` |
| TR-VI-s2-3 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:109` |
| TR-VI-s2-4 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:110` |
| TR-VI-s2-5 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:111` |
| TR-VI-s2-6 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:112` |
| TR-VI-s2-ledger-completeness | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:113` |
| TR-VI-s3-l1-syntax-parse | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:128` |
| TR-VI-s3-l2-policy-admission | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:129` |
| TR-VI-s3-l3-optional-annotation | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:130` |
| TR-VI-s3-admitted | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:136` |
| TR-VI-s3-admitted-scheme-relative | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:137` |
| TR-VI-s3-admitted-ftp | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:138` |
| TR-VI-s3-rejected-scheme | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:139` |
| TR-VI-s3-warn-userinfo | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:140` |
| TR-VI-s4-error-from-curl-ok | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:151` |
| TR-VI-s4-error-web-unsupported-scheme-dem | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:152` |
| TR-VI-s4-warning-userinfo | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:153` |
| TR-VI-s4-ok-scheme-relative | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:154` |
| TR-VI-s4-ok-ftp | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:155` |
| TR-VI-s4-ok | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:156` |
| TR-VI-s4-warning-no-tld | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:157` |
| TR-VI-s4-warning-invalid-tld | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:158` |
| TR-VI-s4-warning-public-suffix | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:159` |
| TR-VI-s5-not-requested | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:172` |
| TR-VI-s5-not-applicable | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:173` |
| TR-VI-s5-known | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:174` |
| TR-VI-s5-unknown | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:175` |
| TR-VI-s5-invalid-input | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:176` |
| TR-VI-s5-dependency-error | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:177` |
| TR-VI-s6-1 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:189` |
| TR-VI-s6-2 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:190` |
| TR-VI-s6-3 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:191` |
| TR-VI-s6-4 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:192` |
| TR-VI-s6-5 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:193` |
| TR-VI-s6-6 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:194` |
| TR-VI-s6-7 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:195` |
| TR-VI-s6-8 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:196` |
| TR-VI-s6-9 | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:197` |
| TR-VI-s6-error-over-warning | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:203` |
| TR-VI-s6-psl-warning-partition | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:204` |
| TR-VI-s6-userinfo-over-psl | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:205` |
| TR-VI-s6-not-applicable-not-requested-nom | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:206` |
| TR-VI-s6-dependency-error-nominal-v3-forw | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:207` |
| TR-VI-s7-c-02-browser-fixer-phase-orderin | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:218` |
| TR-VI-s7-c-03-repeated-rfc-recovery | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:219` |
| TR-VI-s7-repair-off-tested-no-op | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:220` |
| TR-VI-s7-per-intervention-provenance | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:221` |
| TR-VI-s7-absence-is-not-conformance | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:222` |
| TR-VI-s8-both-verdicts-exposed | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:232` |
| TR-VI-s8-inferred-candidate-is-not-strict | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:233` |
| TR-VI-s8-serialized-output-revalidation-b | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:234` |
| TR-VI-s9-base-url-verdict | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:244` |
| TR-VI-s9-reference-verdict | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:245` |
| TR-VI-s9-merged-output-verdict | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:246` |
| TR-VI-s9-current-na-on-failure | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:247` |
| TR-VI-s9-resolver-verdict-surface | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:248` |
| TR-VI-s10-get-parse-verdicts | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:258` |
| TR-VI-s10-get-url-diagnostics-unchanged | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:259` |
| TR-VI-s10-get-host-type-get-scheme-class-u | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:260` |
| TR-VI-s10-parse-frame-keeps-18-public-fiel | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:261` |
| TR-VI-s10-parse-status-retain-as-projectio | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:262` |
| TR-VI-s10-back-compat-guarantee-3-0-scoped | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:263` |
| TR-VI-s10-parse-status-reframed-as-compat- | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:264` |
| TR-VI-s10-parse-status-hard-deprecation-wi | OPEN | migration-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:265` |
<!-- END GENERATED: claim-index -->

## Coverage census

Generated, from the index above. A tally nobody recomputes is a claim like any
other, and this one summarizes the very thing the record exists to establish.

<!-- BEGIN GENERATED: coverage-census -- regenerate with:
### By owning slice

| owning_slice | coverage | claims | SETTLED | OPEN |
|---|---|---|---|---|
| cache-slice | MAPPED | 20 | 20 | 0 |
| state-slice | PENDING | 44 | 43 | 1 |
| full-string-slice | PENDING | 49 | 44 | 5 |
| mutation-slice | PENDING | 64 | 49 | 15 |
| join-slice | PENDING | 68 | 60 | 8 |
| migration-slice | PENDING | 22 | 18 | 4 |
| host-slice | PENDING | 41 | 35 | 6 |
| UNASSIGNED | UNASSIGNED | 172 | 139 | 33 |
| **total** | — | 480 | 408 | 72 |

### By contract

| contract | path | claims | SETTLED | OPEN | sections |
|---|---|---|---|---|---|
| CS | `design/work/url-v3/contracts/canonical-state-contract.md` | 44 | 43 | 1 | 1 |
| CM | `design/work/url-v3/contracts/cleaning-mutation-contracts.md` | 59 | 45 | 14 | 8 |
| HA | `design/work/url-v3/contracts/host-annotation-contracts.md` | 41 | 35 | 6 | 10 |
| KJ | `design/work/url-v3/contracts/key-join-contracts.md` | 75 | 67 | 8 | 7 |
| OUT | `design/work/url-v3/contracts/output-contracts.md` | 57 | 51 | 6 | 12 |
| PS | `design/work/url-v3/contracts/public-surface-closure.md` | 8 | 8 | 0 | 1 |
| PSD | `design/work/url-v3/contracts/public-surface-disposition.md` | 56 | 26 | 30 | 4 |
| SC | `design/work/url-v3/contracts/semantic-cache-contract.md` | 20 | 20 | 0 | 5 |
| SS | `design/work/url-v3/contracts/standard-scheme-matrices.md` | 50 | 46 | 4 | 9 |
| VI | `design/work/url-v3/contracts/validation-intervention-contract.md` | 70 | 67 | 3 | 10 |
<!-- END GENERATED: coverage-census -->

## Excluded sources

A §6 contract that contributes no claims must say why, so that a contract
cannot fall out of the population silently. The gate checks both directions:
every zero-claim contract is listed, and no listed file actually carries
claims.

| path | claims | reason |
|---|---|---|
| `design/work/url-v3/contracts/cross-artifact-consistency.md` | 0 | Capstone, not a source of claims. Its rows are re-checkable assertions **about** the other nine contracts — criterion-(i)–(v) agreement checks, each already carrying its own inline evidence pointer and a `PASS` verdict — plus an open-cell census that indexes cells the siblings own. It states no product claim of its own, so it has no cell for a verification slice to discharge. Its assertions are verified where they are written. |

## Exact CI commands

```sh
# Self-test: T0-T7 against synthetic fixtures (network-free, base R).
Rscript design/work/url-v3/tools/traceability-gate.R --self-test

# Live gate over the real contract family and this record.
Rscript design/work/url-v3/tools/traceability-gate.R

# Regenerate the claim index and coverage census after a contract edit.
Rscript design/work/url-v3/tools/traceability-gate.R --regenerate
```

The gate runs as the `claim-traceability` job in
`.github/workflows/verify.yml`, self-test first, then the live run.

## Scope boundaries

This record owns the **claim population rule** and **section→slice ownership**,
and nothing else. It does NOT define, and must not be read as redefining:

- **Any contract semantic.** Every claim it indexes belongs to the contract
  that wrote it. This record cannot settle an `OPEN` cell, and the fact that a
  cell appears here confers no status on it.
- **Per-claim evidence.** Which test, at which line, with which positive and
  negative case, is owned by the verification slice for that surface — the
  `cache-slice.md` precedent. This record deliberately does not restate it;
  duplicating evidence citations would create a second writer and a new drift
  surface, which is the failure this artifact exists to prevent.
- **Oracle authority.** Owned by P5.3 and the oracle register. This record is
  the coverage axis; that register is the authority axis. A slice may cite
  `OR-nnn` for authority and `TR-*` for coverage; neither implies the other,
  and a well-labeled oracle can still fail to cover a claim.
- **Whether a deferral is legitimate.** Scope deferrals belong to P0.5 and the
  verification-deferrals register (proposed, not yet on `main`). This record
  cites neither by path, deliberately: a forward reference to unmerged work is
  exactly what the gate's T4 rule exists to catch. `UNASSIGNED` here is a
  factual statement that no registered slice's family covers a section — it is
  **not** a deferral and does not pretend to owner authority.
- **The eight property families themselves.** Fixed by §7 G4 criterion 3. That
  three of the ten registered slices own no contract section, and that 171
  claims fall outside all of them, are findings reported below — not license to
  invent a ninth family.

## Open cells

Four, all reported rather than resolved.

1. **171 of 479 claims (36%) have no owning verification slice.** Three
   carriers, each a question only the owner can answer:
   `RURL-jdnlpydz` (validation/intervention, 62 claims), `RURL-lkyverse`
   (standard/scheme matrices, 50), `RURL-sunrlgio` (public-surface
   dispositions, 59). This is the material finding of criterion 1: **the
   accepted verification-slice set does not span the contract family.** It is
   not a defect in any slice — each shipped slice is complete over what it
   claims — it is a gap between the eight §7 G4 property families and the ten
   §6 contracts, and it was not visible until the population was enumerated.

2. **`vector-slice` owns no contract section.** The vector property family is
   named in §7 G4 criterion 3, but no contract section states vector/scalar
   claims in status-bearing rows; vector behavior is described in
   `public-surface-closure.md` prose and in the §6 artifact-4 field list rather
   than as normative cells. So the slice, when authored, will have no cells in
   this map to discharge. Same shape as the oracle register's finding that
   `browser-parity` has zero instances: a named category certifying nothing.
   Whether that means the contracts under-specify vector behavior or the family
   is verified purely by test-level properties is an owner call.

3. **`MAPPED` is section-granular, not claim-granular.** `coverage = MAPPED`
   means the slice owning a claim's section is on disk — **not** that the slice
   cites that individual claim. Per-claim citation requires slices to quote
   `TR-*` ids; the six owed slices can do so from the start, the three shipped
   ones predate the scheme. Rule T7 is the partial substitute available today:
   a shipped owning slice must at least cite the contract it owns sections of.
   **Consequently `G4-acceptance.md` must report criterion 1 as PARTIALLY met.**
   Tightening T7 into a per-claim citation rule is the natural successor once
   two or three slices have adopted the ids.

4. **Claim ids are stable against prose edits but not against row insertion
   mid-section.** Inserting a row does not renumber ids (they key off the row's
   first cell, not its position), but *renaming* a row key or reordering
   claim-bearing sections does. The regeneration check makes any such change a
   visible reviewed diff rather than silent drift, which is the property that
   matters; it does not make ids immutable. Once slices cite `TR-*` ids, a
   rename becomes a breaking edit — and the gate will surface it as one.
