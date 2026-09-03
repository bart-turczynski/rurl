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
| single_writer | repository owner (sole); P0.3 §5 — this record is the SINGLE WRITER of the claim POPULATION rule, of section→slice ownership, and of the claim-level ownership overrides that dissent from it; it is never a writer of contract semantics or of per-claim evidence |
| lifecycle_state | PROPOSED |
| verifies | §7 G4 criterion 1 over the §6 contract family (artifacts 3–12) |
| dependencies | the ten claim-bearing §6 contracts (derived under `## Inputs`, with the eleventh, zero-claim source); reconciliation §6 artifact 11, §7 G4; S9 H6 / RCON-10; P5.3 (oracle policy, the authority axis) |
| closes_finding | RCON-10 (traceability half; the release-rule half stays with P0.4/C-10 and the determinism half with P5.2/C-09) |
| completion_rule | §7 G4 criterion 1 — the claim population is derived, not transcribed; every claim-bearing contract section has exactly one owner, and every claim its section's owner unless the override table dissents; every owner is a registered verification slice or `UNASSIGNED` with a named carrier; every verification record on disk is this map, a registered slice, or a registered discharge record; the generated source list, index and census regenerate byte-identically; the gate is in the verify chain and self-tested |
| content_hash | none — the `## Inputs` sha256 column was retired with the rest of the `## Inputs` hash comparison (ADR 0014); the source list is derived and T3-compared instead (RURL-lynlhzec) |
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
contracts, never transcribed.** A hand-copied index of 515 rows is a second
copy of the contract family that begins drifting the moment either side is
edited, and the drift is silent — which is the precise failure mode criterion 1
exists to prevent. So the source list, the claim index and the coverage census
below are generated and compared byte-for-byte against a fresh derivation on
every CI run. Add a row to a contract and it appears here; the gate fails until
it is owned. Add a *contract* and it appears in `## Inputs`; the gate fails
until the map accounts for it.

What is hand-authored is small and reviewable: which slice owns which contract
**section** (70 rows), the individual **claims** that dissent from their section
(`## Claim ownership overrides`, currently 0 rows), the slice registry, the
discharge-record registry, and the disposition of any contract that contributes
no claims.

## Inputs

The normative sources whose rows constitute the claim population — **derived**,
like everything else generated here, and regenerated with the claim index and
the census under T3.

**This table used to carry a sha256 per row, and no longer does**
(`RURL-lynlhzec`). Those pins were written to be "recomputed by the
verification-family validator at the sealing G4 snapshot". No validator ever
recomputed them; ADR 0014 then retired the `## Inputs` hash comparison as a
mechanism (`design/work/url-v3/tools/validate-records.R:678-684` records the
reasoning at the site); and by the time anyone measured, five of the eleven had
gone stale — `cleaning-mutation-contracts`, `output-contracts`,
`public-surface-closure`, `public-surface-disposition`,
`validation-intervention-contract`. A hash that
nothing recomputes is not tamper-evidence, and leaving it in place made an
unverified assertion read as a verified one.

Deleting the column loses nothing this record needs. A pin answers *did this
source change?* — which T3 already answers, and answers better, by regenerating
the population from those sources and byte-comparing the result. What a pin
could never answer is *is this the right set of sources?*: a contract added to
the family and omitted from this table drifted no hash, because a row that is
absent has none. That is the failure the generated block forecloses, and
`generate_sources()` additionally refuses to emit a table that omits a contract
which contributed claims.

**The capstone keeps its pins, and two of them stay stale.**
`contracts/cross-artifact-consistency.md`'s `## Inputs` is "the exact sources
this capstone asserts over", so a hash there records which bytes its
criterion-3 assertions (i)–(v) were checked against. The remedy when one drifts
is to re-check those assertions, not to re-hash — a silent re-pin would convert
an honest stale hash into a false fresh one. Two of its nine
(`validation-intervention-contract`, `cleaning-mutation-contracts`) are stale
and remain so under `RURL-lynlhzec`.

<!-- BEGIN GENERATED: contract-sources -->
| contract | path |
|---|---|
| CS | `design/work/url-v3/contracts/canonical-state-contract.md` |
| CM | `design/work/url-v3/contracts/cleaning-mutation-contracts.md` |
| CA | `design/work/url-v3/contracts/cross-artifact-consistency.md` |
| HA | `design/work/url-v3/contracts/host-annotation-contracts.md` |
| KJ | `design/work/url-v3/contracts/key-join-contracts.md` |
| OUT | `design/work/url-v3/contracts/output-contracts.md` |
| PS | `design/work/url-v3/contracts/public-surface-closure.md` |
| PSD | `design/work/url-v3/contracts/public-surface-disposition.md` |
| SC | `design/work/url-v3/contracts/semantic-cache-contract.md` |
| SS | `design/work/url-v3/contracts/standard-scheme-matrices.md` |
| VI | `design/work/url-v3/contracts/validation-intervention-contract.md` |
<!-- END GENERATED: contract-sources -->

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

The current population is **515 claims across 10 contracts and 70 sections**,
as the generated census below reports it. That block is the authority for the
number; this sentence is a summary and has drifted from it before.

The count moved by 24 at `RURL-fmkuunwj` without a single new contract cell
being written. `SS`'s scheme-family table stated four properties in four columns
of one table, so its nine rows were nine claims that asserted thirty-three
things; splitting it into one section per property makes the population count
assertions where it used to count rows. That is the rule working as written
rather than a change to it — and it is why a claim total is not a measure of how
much a contract says.

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

## Discharge records

A **discharge record** maps the cells of exactly one deferral onto shipped
executable evidence, and does nothing else. It is **not a slice**: it owns no
contract section, spans no property family, and grants no coverage in the
census below. Each of the five on disk says so in its own envelope comment —
*"NARROW BY CONSTRUCTION … it is NOT the state-family verification slice"*.

They are registered here because another gate credits them.
`tools/deferral-gate.R` rule D2 requires a `DISCHARGED` row of
`design/work/url-v3/registers/verification-deferrals.md` to be claimed by a
verification slice, and it used to accept **any** file under
`design/work/url-v3/verification/` as one — so a discharge could be claimed by
a file that claims nothing, and in practice was claimed by records this map had
never heard of. Two gates, two incompatible definitions of "slice", both green
(P0.7 D-E, `RURL-ogktvhgp`). D2 now reads its claimant registry out of this
section together with `## Verification slices`, and rule T8 holds that registry
to disk in **both** directions: a record listed here must exist and must
actually carry the `DISCHARGED[VD-nnn]` claim D2 will credit, and a
verification record on disk that appears in neither table fails the gate. The
directory therefore partitions into exactly three kinds of file — this map, a
registered slice, a registered discharge record — and admitting a fourth kind
is an explicit edit here, not a filename.

| record_id | deferral_id | tracked_path | contract | scope |
|---|---|---|---|---|
| key-join-discharge | VD-001 | design/work/url-v3/verification/key-join-discharge.md | KJ | the 51 key/join cells VD-001 enumerates, with the residual `http_https_missing` refusal named rather than claimed; not the join-family slice |
| output-display-discharge | VD-003 | design/work/url-v3/verification/output-display-discharge.md | OUT | the safe-display (surface d) cells VD-003 enumerates; surfaces (a), (b) and (c) untouched; OUT-O4's contract-cell move landed under `RURL-irfmmoer` and is not claimed here; not the full-string-family slice |
| output-fsss-discharge | VD-002 | design/work/url-v3/verification/output-fsss-discharge.md | OUT | the FSSS (surface b) cells VD-002 enumerates; surfaces (a), (c) and (d) untouched; not the full-string-family slice |
| state-authority-discharge | VD-005 | design/work/url-v3/verification/state-authority-discharge.md | CS | the authority-state cells only (`authority_delimiter_present`, `authority_payload_kind`, and the emit-`//`-iff-delimiter rule); not the state-family slice |
| state-verdicts-discharge | VD-004 | design/work/url-v3/verification/state-verdicts-discharge.md | CS | the verdict-layer cells only (the three layer fields and the π projection); not the state-family slice |

**What this table does not do, stated plainly.** It does not move a claim.
`coverage` in `## Coverage census` is section-granular and derives from slice
ownership, while a discharge record enumerates cells in the *deferral's*
vocabulary (`SURF-b`, `layer1_syntax_verdict`) rather than in `TR-*` ids —
translating one into the other would be per-claim evidence, which this record
does not restate, and in places an ownership ruling, which it has no authority
to make. So a section whose cells a discharge record evidences can still read
`UNASSIGNED` here, and that is not a contradiction: it says no registered
slice's family covers the section, which remains true. Where that gap is
material it has a carrier — `RURL-jdnlpydz` carried the verdict-layer sections
until P0.7 D-B's routing to `state-slice` was applied in `## Section ownership`
below — and the census moves when such a ruling lands, not when a record is
listed above. What this table changes
is that the evidence is no longer invisible to the record whose job is to
report what is missing.

## Section ownership

One row per claim-bearing contract section, assigned to the slice whose
property family verifies those claims at runtime. Where a section's claims are
dispositions, inventories, or cross-artifact agreements rather than runtime
properties, or where no registered slice's family covers them, the row is
`UNASSIGNED` with a carrier — the same posture the oracle register takes with
`UNLABELED`, and for the same reason: **inventing a slice name to make the
table tidy would freeze an arbitrary answer to a question the owner has not
been asked.**

An unassigned row may name a subtype, but only as `UNASSIGNED[subtype]`: that
is the one qualified spelling `## Coverage census` folds back into the parent
row, so it is the only one T2 accepts. Any other qualifier used to pass the
vocabulary rule and then match no census group, taking its claims out of the
by-slice tally while every rule stayed green (RURL-fymdhizq).

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
| SS | s5 | Scheme-family special-ness rows (whatwg) | UNASSIGNED | RURL-lkyverse |
| SS | s6 | Scheme-family default-port rows | join-slice | — |
| SS | s7 | Scheme-family host / PSL eligibility rows | host-slice | — |
| SS | s8 | Scheme-family semantic-transform eligibility rows | mutation-slice | — |
| SS | s9 | Credential rows | UNASSIGNED | RURL-lkyverse |
| SS | s10 | Email rows | UNASSIGNED | RURL-lkyverse |
| SS | s11 | Resolution rows | UNASSIGNED | RURL-lkyverse |
| SS | s12 | Diagnostics rows | UNASSIGNED | RURL-lkyverse |
| VI | s1 | Repair-posture axis rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s2 | Ordered pipeline / intervention-ledger rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s3 | Verdict-layer rows | state-slice | — |
| VI | s4 | Shipped-value → layer map (the C-07 map) | state-slice | — |
| VI | s5 | Annotation-state resolution rows (S7-F3) | state-slice | — |
| VI | s6 | π collapse table (legacy `parse_status` projection) | state-slice | — |
| VI | s7 | Repair / recovery provenance rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s8 | Repaired-input revalidation rows | UNASSIGNED | RURL-jdnlpydz |
| VI | s9 | Resolution verdict rows (`resolve_url`) | UNASSIGNED | RURL-jdnlpydz |
| VI | s10 | Companion-helper surface + migration rows | migration-slice | — |

### Why these assignments, and where they are contestable

- **`state-slice` owns `canonical-state-contract.md` §Rows (44 cells) plus the
  four `validation-intervention-contract.md` verdict sections (VI s3–s6, 37
  claims) — 81 claims.** The narrower scoping, CS §Rows alone on the evidence
  of the prior state audit (35 of 44 cells covered both ways), was recorded
  first because it was the reading with evidence behind it. P0.7 D-A/D-B
  (`RURL-jdnlpydz`) then adopted the wider one: s3–s6 elaborate the value sets
  and semantics of `layer1_syntax_verdict`, `layer2_policy_verdict`,
  `layer3_annotation_state` and `parse_status`, four fields §Rows already owns,
  and splitting a field's *name* from its *value set* across two slices would
  put one runtime property under two owners. The evidence P0.7 §2 measured is
  `tests/testthat/test-parse-verdicts.R`, whose named blocks
  `design/work/url-v3/verification/state-verdicts-discharge.md` already maps
  cell by cell (`:: "the two error kinds are distinguishable, and parse_status
  is not"`, `:: "L3 does not move L1 or L2 (the independence invariant)"`,
  `:: "pi projects every layer combination to a shipped status value"`,
  `:: "pi reproduces parse_status across the option matrix"`, `:: "every
  produced verdict is inside its settled vocabulary"`, `:: "three L3 states have
  no producer in the shipped engine"`, among them) — with P0.7's own limit
  carried: four s5/s6 cells (`not-requested`, `invalid-input`,
  `dependency-error`, and the `dependency-error → nominal` π row) have no
  producer in the shipped engine and are asserted *absent*, not covered, and one
  s6 row is coverable on its `not-applicable` leg only. The per-cell tally is the
  slice's to make when it is authored; the slice remains `OWED`, and this fixes
  its scope, not its existence. VI s1, s2 and s7–s9 (25 claims) are P0.7 D-C's
  unbuilt half and stay `UNASSIGNED` under `RURL-jdnlpydz` until that separate
  ruling.
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
- **The four `SS` scheme-family sections are that same principle applied to a
  table that was split so it could obey it** (`RURL-fmkuunwj`). Until the split
  those four properties shared one table, and a claim is a *row*, so the section
  had to carry one owner for four families' worth of claims. Each new section
  goes to the family the map already sends that property to: default port →
  `join-slice` on the `KJ s3` §Scheme and port truth table precedent; host / PSL
  eligibility → `host-slice` on `HA s2` (scheme-forced host posture) and `HA s4`
  (PSL annotation); semantic-transform eligibility → `mutation-slice` on `CM s2`.
  P0.8 §4 names all three. **Special-ness is the one that stays `UNASSIGNED`**:
  P0.8 §4 routes it to "the route property", which is `SS s4`
  §Interpretation / parser-route rows — itself unowned under `RURL-lkyverse`.
  Assigning it would invent the answer the other three did not have to invent,
  so it keeps the carrier and waits for the same ruling `SS s4` waits for.

## Claim ownership overrides

Ownership above is assigned per **section**, and some sections cannot be
expressed at that granularity: their columns state properties of different
families in one table. `SS`'s scheme-family table was the measured case — its
columns were `family | special-ness (whatwg) | default port | host / PSL
eligibility | semantic-transform eligibility`, which the map's own precedents
send to four different owners (default port → `join-slice` per `KJ s3`; host /
PSL eligibility → `host-slice` per `HA s2` and `HA s4`; semantic-transform
eligibility → `mutation-slice` per `CM s2`), so no single `owning_slice` for the
section was right for all nine of its claims. P0.8 D-D
(`design/work/url-v3/decisions/P0.8-scheme-claim-ownership.md`) rules that such
a section becomes claim-granular, and names this table as the derivation change
that requires; `RURL-sbhpzwzk` built it. That table has since been split into
one section per property (`RURL-fmkuunwj`), which is why the dissent list below
is empty — see the note under it for why the mechanism stays.

This table is the **dissent list: one row per claim whose owner differs from its
section's**. `## Section ownership` stays the default and carries everything
else, so the hand-authored surface grows by exactly the claims that disagree
with it. `## Claim index` and `## Coverage census` resolve every claim as
*override if listed, section owner otherwise* — the override is applied in the
derivation, not transcribed into the generated block, so a row here moves the
claim's `owning_slice`, its `coverage`, and its census tally together or not at
all. Rule T9 rejects a row naming a claim that does not exist, a duplicate
`claim_id`, and one that merely restates its section's owner; T2 holds these
owners to the same vocabulary and carrier discipline as the section rows.

| claim_id | owning_slice | carrier | reason |
|---|---|---|---|

**The list is empty, and that is a result rather than a gap.** It carried
exactly one row — `TR-SS-s5-default-port-data` → `join-slice` — and
`RURL-fmkuunwj` removed the condition that row existed to work around. The
reasoning is worth keeping because it is what the mechanism is for and what its
limit is.

A claim is a table *row*, so an override moves a whole row. `SS`'s
scheme-family table stated special-ness, default port, host / PSL eligibility
and semantic-transform eligibility in four columns of one table, and the map's
precedents send those to three different families plus the unowned route
property. Claim granularity made the *key* assignable but not the *row*: of
those nine rows only `default-port data`, whose other three columns were `—`,
had a single property to assign. Claim granularity was a strict improvement on
section granularity and it was **not sufficient**, so the residue was reported
as an open cell rather than closed by picking a dominant column. Splitting the
contract table into one section per property — the shape P0.8 D-D left to the
owner — is what actually resolved it, and it resolved the `default-port data`
row along with the other eight, which is why no dissent is left to record.

The table stays. It is the declared mechanism for the next section whose
columns cross families, T9 still guards it, and rule T2 still holds its owners
to the section rows' vocabulary. An empty dissent list asserts that no claim
currently disagrees with its section — a fact about this tree, not a dormant
feature.

The machinery is not left resting on a live row to prove it works: the gate's
self-tests build synthetic contracts, register an override over them, and assert
the moved value in the **generated** claim index, its re-derived coverage, and
the census tally — the three observables a derivation that parsed the table and
then ignored it would leave unchanged. Those tests are what would go red if the
apply step were removed, and they do not depend on this table having rows.

**What no rule here can do, and an empty table makes easy to forget.** Nothing
detects that a section *needs* an override. A future contract section whose
columns cross families will take one owner for all of its claims and pass T1,
T3 and T9 exactly as a correct section does — these rules check that ownership
is *declared and derived consistently*, never that it is *right*. That is how
the scheme-family table sat mis-owned until a reader noticed, and splitting it
changed nothing about the detection gap. The obligation stays with whoever adds
a claim-bearing section: if its columns state properties of different families,
split it or dissent, because no rule will ask.

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
| TR-CM-s3-1 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:159` |
| TR-CM-s3-2 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:160` |
| TR-CM-s3-3 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:161` |
| TR-CM-s3-4 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:162` |
| TR-CM-s3-5 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:163` |
| TR-CM-s3-6 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:164` |
| TR-CM-s3-7 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:165` |
| TR-CM-s3-8 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:166` |
| TR-CM-s3-9 | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:167` |
| TR-CM-s4-clean-url-get-clean-url-surface- | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:186` |
| TR-CM-s4-comparison-key-get-url-key | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:187` |
| TR-CM-s4-standard-serialization-serialize | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:188` |
| TR-CM-s5-non-interference | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:197` |
| TR-CM-s5-the-defect-it-fixes | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:198` |
| TR-CM-s5-test-revision-obligation | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:199` |
| TR-CM-s6-cleaning-repair | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:208` |
| TR-CM-s6-strict-default-admission-flow-th | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:209` |
| TR-CM-s6-s4-f3-provenance-boundary | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:210` |
| TR-CM-s7-clean-surface-never-reconstructs | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:216` |
| TR-CM-s7-safe-display-redacts | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:217` |
| TR-CM-s7-mutation-credential-safety | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:218` |
| TR-CM-s8-semantics-verb-vocabulary | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:233` |
| TR-CM-s8-component-dependency-graph | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:234` |
| TR-CM-s8-state-transitions | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:235` |
| TR-CM-s8-transaction-behavior | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:236` |
| TR-CM-s8-eligibility | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:237` |
| TR-CM-s8-security | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:238` |
| TR-CM-s8-invariants | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:239` |
| TR-CM-s8-query-model | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:240` |
| TR-CM-s8-path-model | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:241` |
| TR-CM-s8-psl-binding | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:242` |
| TR-CM-s8-standards-profile-repair-interac | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:243` |
| TR-CM-s8-downstream-safety | OPEN | mutation-slice | PENDING | `design/work/url-v3/contracts/cleaning-mutation-contracts.md:244` |
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
| TR-KJ-s1-get-url-key-url-policy-url-key-p | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:69` |
| TR-KJ-s1-url-key-policy | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:70` |
| TR-KJ-s1-key-representation | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:71` |
| TR-KJ-s1-identity-input | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:72` |
| TR-KJ-s1-non-interference | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:73` |
| TR-KJ-s1-interpretation-selector | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:74` |
| TR-KJ-s1-diagnostic-surface | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:75` |
| TR-KJ-s2-scheme-source | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:84` |
| TR-KJ-s2-scheme-equality | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:85` |
| TR-KJ-s2-scheme-case | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:86` |
| TR-KJ-s2-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:87` |
| TR-KJ-s2-authority | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:88` |
| TR-KJ-s2-host-kind | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:89` |
| TR-KJ-s2-domain-spelling | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:90` |
| TR-KJ-s2-host-editing | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:91` |
| TR-KJ-s2-path | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:92` |
| TR-KJ-s2-path-display-editing | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:93` |
| TR-KJ-s2-query-presence | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:94` |
| TR-KJ-s2-query-structure | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:95` |
| TR-KJ-s2-fragment | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:96` |
| TR-KJ-s2-userinfo | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:97` |
| TR-KJ-s2-missing-invalid | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:98` |
| TR-KJ-s2-persisted-key-stability | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:99` |
| TR-KJ-s3-http-absent-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:109` |
| TR-KJ-s3-https-absent-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:110` |
| TR-KJ-s3-http-absent-port-2 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:111` |
| TR-KJ-s3-https-absent-port-2 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:112` |
| TR-KJ-s3-http-443 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:113` |
| TR-KJ-s3-http-80 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:114` |
| TR-KJ-s3-http-absent | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:115` |
| TR-KJ-s3-missing-scheme-no-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:116` |
| TR-KJ-s3-missing-scheme-80 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:117` |
| TR-KJ-s3-missing-scheme-443 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:118` |
| TR-KJ-s3-scheme-relative-no-port | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:119` |
| TR-KJ-s3-ftp-21 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:120` |
| TR-KJ-s3-ws-80-wss-443 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:121` |
| TR-KJ-s3-custom-123 | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:122` |
| TR-KJ-s4-eligibility-vocabulary | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:143` |
| TR-KJ-s4-warning-rows | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:144` |
| TR-KJ-s4-non-keyable-rows | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:145` |
| TR-KJ-s4-missing-matching | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:146` |
| TR-KJ-s4-key-collision | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:147` |
| TR-KJ-s4-duplicate-keys | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:148` |
| TR-KJ-s4-resource-guard | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:149` |
| TR-KJ-s5-url-inner-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:159` |
| TR-KJ-s5-url-left-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:160` |
| TR-KJ-s5-url-right-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:161` |
| TR-KJ-s5-url-full-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:162` |
| TR-KJ-s5-url-semi-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:163` |
| TR-KJ-s5-url-anti-join-x-y | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:164` |
| TR-KJ-s6-url-columns | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:170` |
| TR-KJ-s6-key-policy | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:171` |
| TR-KJ-s6-parse-policy | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:172` |
| TR-KJ-s6-relationship | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:173` |
| TR-KJ-s6-multiple-matches | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:174` |
| TR-KJ-s6-duplicate-counts | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:175` |
| TR-KJ-s6-invalid-warnings | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:176` |
| TR-KJ-s6-key-visibility | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:177` |
| TR-KJ-s6-original-urls | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:178` |
| TR-KJ-s6-suffix-name-repair | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:179` |
| TR-KJ-s6-row-order-except-right-join | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:180` |
| TR-KJ-s6-type-attributes | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:181` |
| TR-KJ-s6-conditions | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:182` |
| TR-KJ-s6-resource-bound | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:183` |
| TR-KJ-s6-diagnostics | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:184` |
| TR-KJ-s7-legacy-freeze | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:194` |
| TR-KJ-s7-implicit-equality-dials | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:195` |
| TR-KJ-s7-identity-opt-in | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:196` |
| TR-KJ-s7-six-join-replacement | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:197` |
| TR-KJ-s7-audit-before-switch | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:198` |
| TR-KJ-s7-current-formal-migration | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:199` |
| TR-KJ-s7-forwarded-parse-dials | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:200` |
| TR-KJ-s7-forwarded-display-cleaning-dials | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:201` |
| TR-KJ-s7-path-encoding-regression | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:202` |
| TR-KJ-s7-removal-default-flip | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/key-join-contracts.md:203` |
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
| TR-OUT-s3-parse-posture | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:121` |
| TR-OUT-s3-host-spelling | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:122` |
| TR-OUT-s3-build-dependency | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:123` |
| TR-OUT-s3-public-entry-point-name | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:124` |
| TR-OUT-s3-rfc-serializer-posture | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:125` |
| TR-OUT-s4-authority-host-state | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:136` |
| TR-OUT-s4-credential-delimiter-state | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:137` |
| TR-OUT-s4-query-lexical-state | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:138` |
| TR-OUT-s4-query-fragment-presence | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:139` |
| TR-OUT-s4-path-posture | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:140` |
| TR-OUT-s4-parse-repair-provenance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:141` |
| TR-OUT-s5-own-surface-never-a-serializer | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:147` |
| TR-OUT-s5-byte-vs-encoding-label-guarantee | OPEN | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:148` |
| TR-OUT-s6-intentionally-lossy-seo-product | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:157` |
| TR-OUT-s6-omits-fragment-credentials | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:158` |
| TR-OUT-s6-byte-compat-scope | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:159` |
| TR-OUT-s6-dials-owned-elsewhere | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:160` |
| TR-OUT-s6-resolve-url-coupling | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:161` |
| TR-OUT-s7-separate-surface | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:167` |
| TR-OUT-s7-redacts-credentials | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:168` |
| TR-OUT-s7-scope-escape-annotation-matrix | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:169` |
| TR-OUT-s8-identity-never-presentation | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:179` |
| TR-OUT-s8-non-interference | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:180` |
| TR-OUT-s8-key-policy-truth-tables-joins-mi | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:181` |
| TR-OUT-s9-claims-against-the-fsss-only | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:191` |
| TR-OUT-s9-the-projection-may-not-be-an-ora | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:192` |
| TR-OUT-s9-historical-projection-claims-ret | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:193` |
| TR-OUT-s9-parity-conformance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:194` |
| TR-OUT-s9-labeled-oracle-taxonomy | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:195` |
| TR-OUT-s9-metamorphic-assertions-required | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:196` |
| TR-OUT-s9-oracle-register-budgets | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:197` |
| TR-OUT-s10-url-source-a | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:235` |
| TR-OUT-s10-serialize-url-b | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:236` |
| TR-OUT-s10-clean-url-c | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:237` |
| TR-OUT-s10-format-url-d | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:238` |
| TR-OUT-s10-get-url-key-e | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:239` |
| TR-OUT-s11-utf-8-marking | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:247` |
| TR-OUT-s11-locale-invariance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:248` |
| TR-OUT-s11-raw-byte-vs-percent-spelling-dis | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:249` |
| TR-OUT-s12-user-password-split-retained | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:261` |
| TR-OUT-s12-internal-reassembly-completeness | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:262` |
| TR-OUT-s12-output-governance | SETTLED | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:263` |
| TR-OUT-s12-public-undivided-userinfo-compon | OPEN | full-string-slice | PENDING | `design/work/url-v3/contracts/output-contracts.md:264` |
| TR-PS-s1-the-18-public-fields-three-value | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:252` |
| TR-PS-s1-parse-status-l1-l2-l3-compat-pro | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:253` |
| TR-PS-s1-clean-url-clean-surface-c-not-id | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:254` |
| TR-PS-s1-comparison-key-get-url-key-not-c | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:255` |
| TR-PS-s1-host-domain-psl-registrable-not- | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:256` |
| TR-PS-s1-cache-config-info-clear-semantic | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:257` |
| TR-PS-s1-scheme-admission-interpretation- | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:258` |
| TR-PS-s1-companion-helpers-never-widen-th | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-closure.md:259` |
| TR-PSD-s1-exported-functions | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:82` |
| TR-PSD-s1-public-output-fields | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:83` |
| TR-PSD-s1-curl-dependency-surfaces | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:84` |
| TR-PSD-s1-migration-surface | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:85` |
| TR-PSD-s2-canonical-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:101` |
| TR-PSD-s2-check-hosts | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:102` |
| TR-PSD-s2-format-url | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:103` |
| TR-PSD-s2-get-clean-url | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:104` |
| TR-PSD-s2-get-domain | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:105` |
| TR-PSD-s2-get-fragment | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:106` |
| TR-PSD-s2-get-host | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:107` |
| TR-PSD-s2-get-host-type | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:108` |
| TR-PSD-s2-get-mailto-recipients | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:109` |
| TR-PSD-s2-get-parse-status | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:110` |
| TR-PSD-s2-get-parse-verdicts | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:111` |
| TR-PSD-s2-get-password | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:112` |
| TR-PSD-s2-get-path | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:113` |
| TR-PSD-s2-get-port | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:114` |
| TR-PSD-s2-get-query | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:115` |
| TR-PSD-s2-get-scheme | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:116` |
| TR-PSD-s2-get-scheme-class | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:117` |
| TR-PSD-s2-get-subdomain | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:118` |
| TR-PSD-s2-get-tld | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:119` |
| TR-PSD-s2-get-url-diagnostics | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:120` |
| TR-PSD-s2-get-url-key | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:121` |
| TR-PSD-s2-get-user | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:122` |
| TR-PSD-s2-get-userinfo | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:123` |
| TR-PSD-s2-is-valid-host | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:124` |
| TR-PSD-s2-query-param-summary | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:125` |
| TR-PSD-s2-resolve-url | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:126` |
| TR-PSD-s2-rurl-cache-config | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:127` |
| TR-PSD-s2-rurl-cache-info | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:128` |
| TR-PSD-s2-rurl-clear-caches | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:129` |
| TR-PSD-s2-safe-parse-url | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:130` |
| TR-PSD-s2-safe-parse-urls | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:131` |
| TR-PSD-s2-serialize-url | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:132` |
| TR-PSD-s2-url-anti-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:133` |
| TR-PSD-s2-url-full-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:134` |
| TR-PSD-s2-url-inner-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:135` |
| TR-PSD-s2-url-key-policy | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:136` |
| TR-PSD-s2-url-left-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:137` |
| TR-PSD-s2-url-profile | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:138` |
| TR-PSD-s2-url-right-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:139` |
| TR-PSD-s2-url-semi-join | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:140` |
| TR-PSD-s3-original-url | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:146` |
| TR-PSD-s3-scheme | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:147` |
| TR-PSD-s3-host | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:148` |
| TR-PSD-s3-port | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:149` |
| TR-PSD-s3-path | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:150` |
| TR-PSD-s3-query | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:151` |
| TR-PSD-s3-fragment | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:152` |
| TR-PSD-s3-user | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:153` |
| TR-PSD-s3-password | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:154` |
| TR-PSD-s3-domain | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:155` |
| TR-PSD-s3-tld | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:156` |
| TR-PSD-s3-domain-ascii | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:157` |
| TR-PSD-s3-domain-unicode | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:158` |
| TR-PSD-s3-tld-ascii | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:159` |
| TR-PSD-s3-tld-unicode | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:160` |
| TR-PSD-s3-is-ip-host | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:161` |
| TR-PSD-s3-clean-url | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:162` |
| TR-PSD-s3-parse-status | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/public-surface-disposition.md:163` |
| TR-PSD-s4-curl-import-metadata | OPEN | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:169` |
| TR-PSD-s4-curl-parse-call | OPEN | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:170` |
| TR-PSD-s4-curl-escape-unescape | OPEN | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:171` |
| TR-PSD-s4-migration-surface | SETTLED | migration-slice | PENDING | `design/work/url-v3/contracts/public-surface-disposition.md:172` |
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
| TR-SS-s5-http-https | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:163` |
| TR-SS-s5-ftp | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:164` |
| TR-SS-s5-ftps | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:165` |
| TR-SS-s5-sftp | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:166` |
| TR-SS-s5-file | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:167` |
| TR-SS-s5-ws-wss | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:168` |
| TR-SS-s5-mailto | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:169` |
| TR-SS-s5-tel-data-arbitrary-foo | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:170` |
| TR-SS-s6-http-https | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:176` |
| TR-SS-s6-ftp | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:177` |
| TR-SS-s6-ftps | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:178` |
| TR-SS-s6-sftp | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:179` |
| TR-SS-s6-file | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:180` |
| TR-SS-s6-ws-wss | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:181` |
| TR-SS-s6-mailto | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:182` |
| TR-SS-s6-tel-data-arbitrary-foo | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:183` |
| TR-SS-s6-default-port-data | SETTLED | join-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:184` |
| TR-SS-s7-http-https | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:190` |
| TR-SS-s7-ftp | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:191` |
| TR-SS-s7-ftps | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:192` |
| TR-SS-s7-sftp | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:193` |
| TR-SS-s7-file | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:194` |
| TR-SS-s7-ws-wss | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:195` |
| TR-SS-s7-mailto | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:196` |
| TR-SS-s7-tel-data-arbitrary-foo | SETTLED | host-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:197` |
| TR-SS-s8-http-https | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:203` |
| TR-SS-s8-ftp | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:204` |
| TR-SS-s8-ftps | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:205` |
| TR-SS-s8-sftp | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:206` |
| TR-SS-s8-file | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:207` |
| TR-SS-s8-ws-wss | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:208` |
| TR-SS-s8-mailto | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:209` |
| TR-SS-s8-tel-data-arbitrary-foo | SETTLED | mutation-slice | PENDING | `design/work/url-v3/contracts/standard-scheme-matrices.md:210` |
| TR-SS-s9-five-distinct-credential-routes | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:216` |
| TR-SS-s9-generic-authority-credential-pre | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:217` |
| TR-SS-s9-credential-output-policy-boundar | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:218` |
| TR-SS-s9-get-password-selector-parity | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:219` |
| TR-SS-s10-recipient-projection | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:225` |
| TR-SS-s10-extraction-is-metadata-only | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:226` |
| TR-SS-s10-first-local-part-decode | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:227` |
| TR-SS-s10-email-helper-surface | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:228` |
| TR-SS-s10-indeterminate-lexer-url-level-em | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:229` |
| TR-SS-s11-scheme-less-input | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:235` |
| TR-SS-s11-scheme-relative-host | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:236` |
| TR-SS-s11-rfc-relative-reference-resolutio | OPEN | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:237` |
| TR-SS-s12-get-scheme-class | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:243` |
| TR-SS-s12-selected-scheme-facts | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:244` |
| TR-SS-s12-absence-is-not-conformance | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:245` |
| TR-SS-s12-email-diagnostics | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/standard-scheme-matrices.md:246` |
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
| TR-VI-s2-ledger-completeness | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:113` |
| TR-VI-s3-l1-syntax-parse | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:128` |
| TR-VI-s3-l2-policy-admission | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:129` |
| TR-VI-s3-l3-optional-annotation | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:130` |
| TR-VI-s3-admitted | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:136` |
| TR-VI-s3-admitted-scheme-relative | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:137` |
| TR-VI-s3-admitted-ftp | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:138` |
| TR-VI-s3-rejected-scheme | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:139` |
| TR-VI-s3-warn-userinfo | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:140` |
| TR-VI-s4-error-from-curl-ok | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:151` |
| TR-VI-s4-error-web-unsupported-scheme-dem | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:152` |
| TR-VI-s4-warning-userinfo | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:153` |
| TR-VI-s4-ok-scheme-relative | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:154` |
| TR-VI-s4-ok-ftp | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:155` |
| TR-VI-s4-ok | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:156` |
| TR-VI-s4-warning-no-tld | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:157` |
| TR-VI-s4-warning-invalid-tld | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:158` |
| TR-VI-s4-warning-public-suffix | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:159` |
| TR-VI-s5-not-requested | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:172` |
| TR-VI-s5-not-applicable | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:173` |
| TR-VI-s5-known | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:174` |
| TR-VI-s5-unknown | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:175` |
| TR-VI-s5-invalid-input | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:176` |
| TR-VI-s5-dependency-error | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:177` |
| TR-VI-s6-1 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:189` |
| TR-VI-s6-2 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:190` |
| TR-VI-s6-3 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:191` |
| TR-VI-s6-4 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:192` |
| TR-VI-s6-5 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:193` |
| TR-VI-s6-6 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:194` |
| TR-VI-s6-7 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:195` |
| TR-VI-s6-8 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:196` |
| TR-VI-s6-9 | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:197` |
| TR-VI-s6-error-over-warning | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:203` |
| TR-VI-s6-psl-warning-partition | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:204` |
| TR-VI-s6-userinfo-over-psl | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:205` |
| TR-VI-s6-not-applicable-not-requested-nom | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:206` |
| TR-VI-s6-dependency-error-nominal-v3-forw | SETTLED | state-slice | PENDING | `design/work/url-v3/contracts/validation-intervention-contract.md:207` |
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
| TR-VI-s9-resolver-verdict-surface | SETTLED | UNASSIGNED | UNASSIGNED | `design/work/url-v3/contracts/validation-intervention-contract.md:248` |
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
| state-slice | PENDING | 81 | 80 | 1 |
| full-string-slice | PENDING | 51 | 49 | 2 |
| mutation-slice | PENDING | 72 | 58 | 14 |
| join-slice | PENDING | 77 | 77 | 0 |
| migration-slice | PENDING | 22 | 18 | 4 |
| host-slice | PENDING | 49 | 43 | 6 |
| UNASSIGNED | UNASSIGNED | 144 | 114 | 30 |
| **total** | — | 516 | 459 | 57 |

### By contract

| contract | path | claims | SETTLED | OPEN | sections |
|---|---|---|---|---|---|
| CS | `design/work/url-v3/contracts/canonical-state-contract.md` | 44 | 43 | 1 | 1 |
| CM | `design/work/url-v3/contracts/cleaning-mutation-contracts.md` | 59 | 45 | 14 | 8 |
| HA | `design/work/url-v3/contracts/host-annotation-contracts.md` | 41 | 35 | 6 | 10 |
| KJ | `design/work/url-v3/contracts/key-join-contracts.md` | 75 | 75 | 0 | 7 |
| OUT | `design/work/url-v3/contracts/output-contracts.md` | 59 | 57 | 2 | 12 |
| PS | `design/work/url-v3/contracts/public-surface-closure.md` | 8 | 8 | 0 | 1 |
| PSD | `design/work/url-v3/contracts/public-surface-disposition.md` | 66 | 37 | 29 | 4 |
| SC | `design/work/url-v3/contracts/semantic-cache-contract.md` | 20 | 20 | 0 | 5 |
| SS | `design/work/url-v3/contracts/standard-scheme-matrices.md` | 74 | 70 | 4 | 12 |
| VI | `design/work/url-v3/contracts/validation-intervention-contract.md` | 70 | 69 | 1 | 10 |
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
# Self-test: T0-T8 against synthetic fixtures (network-free, base R).
Rscript design/work/url-v3/tools/traceability-gate.R --self-test

# Live gate over the real contract family and this record.
Rscript design/work/url-v3/tools/traceability-gate.R

# Regenerate the claim index and coverage census after a contract edit.
Rscript design/work/url-v3/tools/traceability-gate.R --regenerate
```

The gate runs as the `claim-traceability` job in
`.github/workflows/verify.yml`, self-test first, then the live run.

A second gate consumes this record. `tools/deferral-gate.R` reads
`## Verification slices` and `## Discharge records` as its claimant registry
(rule D2), so an edit to either table changes what that gate admits and both
must be run after one:

```sh
Rscript tools/deferral-gate.R --self-test
Rscript tools/deferral-gate.R
```

## Scope boundaries

This record owns the **claim population rule**, **section→slice ownership**,
and the **claim-level overrides** that dissent from it, and nothing else. It
does NOT define, and must not be read as redefining:

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
- **Whether a deferral is legitimate.** Scope deferrals belong to P0.5 and
  `design/work/url-v3/registers/verification-deferrals.md`, which is now on
  `main` and therefore citable — when this bullet was written it was not, and
  citing unmerged work is exactly what T4 exists to catch. What
  `## Discharge records` adds is a *fact*: which record on disk claims which
  `VD-nnn`, so that the gate crediting those claims and the record reporting
  coverage read the same registry. It judges no deferral, grants no cell, and
  overturns nothing P0.5 decides. `UNASSIGNED` remains a factual statement that
  no registered slice's family covers a section — **not** a deferral, and no
  claim to owner authority.
- **The eight property families themselves.** Fixed by §7 G4 criterion 3. That
  three of the ten registered slices own no contract section, and that 180
  claims fall outside all of them, are findings reported below — not license to
  invent a ninth family. Claim-granular ownership does not widen this: an
  override may only name a slice already in the registry.

## Open cells

Four, all reported rather than resolved.

1. **144 of 516 claims (28%) have no owning verification slice.** Three
   carriers, each a question only the owner can answer:
   `RURL-jdnlpydz` (validation/intervention, 25 claims), `RURL-lkyverse`
   (standard/scheme matrices, 49), `RURL-sunrlgio` (public-surface
   dispositions, 70). This is the material finding of criterion 1: **the
   accepted verification-slice set does not span the contract family.** It is
   not a defect in any slice — each shipped slice is complete over what it
   claims — it is a gap between the eight §7 G4 property families and the ten
   §6 contracts, and it was not visible until the population was enumerated.

   **Read the drop from 181 to 144 for what it is.** P0.7 D-A/D-B, applied
   under `RURL-jdnlpydz`, moved the four `VI` verdict-layer sections (s3–s6,
   37 claims) to `state-slice`: their subject surface — `get_parse_verdicts()`
   and the four fields CS §Rows already owns — is shipped, and the evidence a
   discharge record had already mapped (`state-verdicts-discharge`) now sits
   under a registered owner instead of beside one. That is the **netting** the
   previous revision of this item said was owed, and it is an ownership ruling,
   not a census edit: nothing here was newly covered — `state-slice` is still
   `OWED` — it was newly *owned*. The 25 that remain under `RURL-jdnlpydz` are
   P0.7 D-C's unbuilt half (s1, s2, s7–s9: the repair-posture axis, the
   intervention ledger, provenance, revalidation, and the resolver verdict
   surface that P2.7 D-E settled as not shipped); they are deferral candidates,
   not slice candidates, and that ruling is separate. `RURL-sunrlgio`'s figure
   reads 70 rather than the 69 the previous revision carried because one
   `PSD` claim entered the population after those numerals were last restated;
   these hand-written figures are a prose summary of the generated census
   above, so read the census, not this paragraph, when the two disagree.

   **Read the drop from 37% to 35% for what it is.** `RURL-fmkuunwj` split the
   `SS` scheme-family table into one section per property and assigned three of
   the four to the families the map already sends them to, which moved 24 claims
   into `join-slice`, `host-slice` and `mutation-slice`. The unowned count did
   **not** move: it is 180 before and 180 after. Every one of those 24 is newly
   *counted*, not newly *covered* — they came into the population in the same
   edit that owned them, because the rows they were split out of were already
   unowned as wholes. The percentage fell only because the denominator grew.
   None of the three carriers' figures changed either (62 / 49 / 69), and the
   one `SS` property with no precedent to follow — special-ness — stayed
   `UNASSIGNED` under `RURL-lkyverse` rather than being assigned to make the
   number smaller.

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

   **This has now been measured, not just predicted.** `RURL-fmkuunwj` split one
   `SS` section into four, which renumbered every claim in the four sections
   below it (`SS s6`–`s9` became `s9`–`s12`, 16 claims) and retired the id
   `TR-SS-s5-default-port-data` that `## Claim ownership overrides` cited. T9
   failed on that dead id — it did not quietly stop moving its claim — and T1
   reported the four new sections as ORPHANs in the same run. A renumbering
   edit is therefore loud in both directions today, before any slice has adopted
   the ids. What is still true is that nothing outside this record cites a
   `TR-*` id yet, so the blast radius has not actually been tested at scale.

**Closed since the last revision.** A fifth cell reported that eight of `SS s5`'s
nine claims were not expressible, because a claim is a row and those rows stated
four properties each. `RURL-fmkuunwj` took the first of the two shapes it
offered — split the contract table into one claim-bearing section per property —
and the cell is removed rather than restated. The residue it named is gone:
three of the four properties now sit in sections owned by the family the map
already sends them to, and the fourth, special-ness, is `UNASSIGNED` under
`RURL-lkyverse` for the ordinary reason every other unowned section is, not
because it cannot be expressed. What that split cost — 24 more claims in the
population and 16 renumbered ids — is recorded in `## Population rule` and in 4
above.
