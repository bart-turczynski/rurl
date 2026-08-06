# Cross-artifact consistency (G3.X capstone; §7 G3 exit criterion 3)

<!-- Contract artifact (the G3.X capstone; §7 G3 exit criterion 3). This record
     makes NO new product decision and owns NO product cell. It is the CONSISTENCY
     ASSERTION over the ten §6 artifacts authored by the nine G3 content leaves
     (artifacts 3, 5, 6, 7, 8, 9, 10-cache, 10-host, 9-key, 4): it verifies that
     the ten artifacts AGREE — shared terms, defaults, state fields, and status
     codes do not fork across them — and it produces the cross-artifact consistency
     assertion that feeds gates/G3-acceptance.md at the cp-snapshot-3 owner seal.
     It asserts §7 G3 exit criterion 3, parts (i)-(v), each with a PASS verdict and
     an evidence pointer that is mechanically re-checkable from the hashed inputs;
     and it carries the full OPEN-CELL CENSUS proving no cell is unowned across the
     artifact set. It introduces no new OPEN product cell — it only censuses the
     existing ones each sibling already flagged. The nine sibling contract files are
     hashed under `## Inputs` for tamper-evidence; the accepted decisions they
     project are already hash-enforced by validate-manifest.R. Format follows the
     G3.3/G3.5/G3.6/G3.7/G3.8/G3.9/G3.H/G3.K/G3.4 precedent. Envelope stays
     lifecycle_state PROPOSED until the cp-snapshot-3 seal; validator coverage rides
     that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-cross-artifact-consistency |
| name | cross-artifact-consistency |
| artifact_number | — (capstone; asserts across §6 artifacts 3–10 + 4) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/cross-artifact-consistency.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the cross-artifact consistency assertion: the §7 G3 criterion-3 (i)–(v) verdicts and the open-cell census. It writes NO product cell — the per-artifact semantics belong to the owning contracts and are referenced, never redefined |
| lifecycle_state | PROPOSED |
| dependencies | the nine G3 content contracts it asserts over (canonical-state 3; standard-scheme 5; validation-intervention 6; output 7; cleaning-mutation 8; semantic-cache 10-cache; host-annotation 10-host; key-join 9; public-surface-closure 4), hashed below; the accepted decisions those contracts project (hash-enforced in manifest); reconciliation §7 G3 exit criterion 3 |
| bound_decision | none — this record makes no product decision; it asserts agreement among the projections the nine leaves already made |
| bound_evidence | reconciliation §7 G3 exit criterion 3 (cross-artifact agreement) |
| closes_finding | none directly — it is the G3 exit gate's criterion-3 evidence; RCON-03/04/05/06/08/09-adjacent closures belong to the owning leaves |
| completion_rule | §7 G3 exit criterion 3 — the artifact exists and asserts (i)–(v) each PASS with a re-checkable evidence pointer, and the open-cell census shows every open cell in every sibling contract carries a named destination (a sibling cell, the unmade P4 host record / RCON-08, the unmade P3 mutation-slice owner decision, §6 artifact 11 / G4, or a future P5 decision) — no cell is unowned; validate-records.R (cross-artifact-consistency section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (the nine sibling contracts), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip); rides the same seal as the nine siblings |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | cross-artifact-consistency validator section stages with the cp-snapshot-3 seal |

## Purpose

The G3 exit gate's cross-artifact check. Nine content leaves each PROJECTED an
accepted decision onto one §6 artifact; this capstone verifies the ten resulting
artifacts **agree** — that the shared vocabulary, defaults, state fields, and
status codes are defined **once** by their canonical owner and only *referenced*
(never re-forked) by the others, and that **no cell is unowned** anywhere in the
set.

It settles nothing and owns no product cell. Each of the five criteria below is an
assertion with a PASS verdict and an evidence pointer that is mechanically
re-checkable from the hashed inputs (a single-writer declaration, a citation
census, or a grep). The open-cell census is a faithful roll-up of the OPEN cells
each sibling already flagged; if it surfaced a genuinely unowned cell that would be
a real finding for the owner — it does not.

## Inputs

The exact sources this capstone asserts over. A hash here records **which bytes
assertions (i)–(v) below were checked against** — it is not bookkeeping, so it
may only be refreshed by someone who has re-run those checks.

Nothing recomputes these. ADR 0014 retired the `## Inputs` sha256 comparison
along with the seal phase (`design/work/url-v3/tools/validate-records.R:678-684`
keeps the reasoning at the site; `validate-manifest.R`, named here until this
revision, was deleted by the same ADR). That is a deliberate gap, not an
oversight: the remedy when one of these drifts is to re-check the assertions,
which no machine can confirm happened. `RURL-lynlhzec` carries it.

**Two rows were re-verified and re-pinned on 2026-08-06 (`RURL-lynlhzec`).**
`validation-intervention-contract.md` and `cleaning-mutation-contracts.md` had
carried their authoring hashes (`5d25ce7`, `b2728d0`) through three later
commits. What the re-check found, per assertion:

- **(i)–(iii)** — unaffected. Both files' envelopes, and with them both
  `single_writer` declarations, are byte-identical to the pinned versions; the
  drift is confined to matrix rows and open-cell prose, and neither file
  re-declares artifact-3 field vocabulary, comparison-key policy, or cache
  semantics.
- **(iv)** — verdict holds, evidence was stale. `2f7c182`/`f9db0c1` settled VI's
  `ledger completeness` row and introduced a twelfth citation, `P2.6@5f4309b`.
  Re-running the census gives 12 distinct `Pn.n@sha` pairs and **zero** forks, so
  the assertion stands; its evidence cell, which listed eleven, is corrected.
- **(v)** — verdict holds, evidence was stale. VAL-O1 closed at source, which
  `### Census tally` already records (44 → 41, two of the three from P2.5, not
  from this drift). Assertion (v)'s own evidence cell still said 44 and now says
  41 — it disagreed with the tally three sections below it.

Both files' assertions therefore still hold over their current bytes, and only
then are the hashes advanced.

The envelope above and this record's header comment still describe the seal
phase and `validate-manifest.R`. They are left as written: ADR 0014 kept every
record's frontmatter deliberately, on the ground that rewriting it to strip a
retired lifecycle would be churn. The prose in this section, not the envelope,
is what a reader should believe about how these hashes are maintained.

Artifact 4 is represented here by its **invariant** half only
(`public-surface-closure.md`). Its roster half, `public-surface-disposition.md`, is
deliberately not hashed in this table: this capstone is itself a G3 `## Inputs`
entry, so pinning the roster here would put every added export back on the path to
a gate re-acceptance — the cascade P0.6 removes. The roster's vocabulary agreement
is asserted through the invariant's own agreement section, and its conformance is
checked executably by `validate-records.R` (P0.6 I1–I5).

| path | sha256 |
|---|---|
| design/work/url-v3/contracts/canonical-state-contract.md | efebe54e645dfcaf56aa2e7d78fcad37d1f266952431a247d5197624996f43ca |
| design/work/url-v3/contracts/standard-scheme-matrices.md | a41b45d1531efa05f11e8685e4891766e2eea15d8c3b63cec0640439dc8ec5de |
| design/work/url-v3/contracts/validation-intervention-contract.md | 8430df426ab06ed311df45d00d2ba6311d5e5b24e949c2c7c763aeba84e62dd7 |
| design/work/url-v3/contracts/output-contracts.md | 62087dad29481e78e204f2dcb511aabea3ac823dc0651ae3c918e0c0a588975f |
| design/work/url-v3/contracts/cleaning-mutation-contracts.md | 1169224b914482960767bd6f294f870d0bcb1ef59858aadb1f5d060e7fd9faed |
| design/work/url-v3/contracts/semantic-cache-contract.md | a70acc712e3db5f1426d45106e925d445af048df03086a230e04a4545fc67001 |
| design/work/url-v3/contracts/host-annotation-contracts.md | ec67597447dd0c57dd8c0c7bc9e2216d6bc3c0ee75e956ec729e9a499b551ee0 |
| design/work/url-v3/contracts/key-join-contracts.md | c8ab02251a2dda7760265ab32a889338134d81f938e0fba2a85de48d1063a7d3 |
| design/work/url-v3/contracts/public-surface-closure.md | 9bc15ab8a73a8ed88480611a272b023d31a7e601efc99cd94d3bc0182381eef8 |

## Criterion-3 assertions ((i)–(v))

Each row is an assertion over the hashed inputs with a re-checkable evidence
pointer. "Single writer" pointers cite the sibling's `single_writer` envelope
field (each declares the one contract that DEFINES a concept; all others reference
it). "Citation census" pointers are a grep over the nine files.

| # | assertion | evidence (re-checkable from inputs) | verdict |
|---|---|---|---|
| (i) | The canonical field vocabulary is defined **once** by artifact 3 (G3.3) and is the ONLY vocabulary used by G3.6 verdicts, G3.7 outputs, and G3.9 status/cache introspection — no forked field names. | `canonical-state-contract.md` `single_writer` = "SINGLE WRITER of the canonical field vocabulary"; G3.6/G3.7/G3.9 name artifact-3 fields (e.g. `parse_status`, the 18 public fields, three-valued presence) and re-declare none. G3.4's cross-artifact section already asserts the public-surface slice of this ("the 18 public fields + three-valued presence → artifact 3"). | PASS |
| (ii) | The comparison key is defined **once** in artifact 9 (G3.K) and only *referenced* by G3.7 (output surface e), G3.8, G3.H, and G3.4 — never redefined. | `key-join-contracts.md` `single_writer` = "SINGLE WRITER of comparison-key policy, URL-join semantics, and canonical_join migration"; `output-contracts.md` lists "comparison key" only as one of the five output **surfaces** (an assignment), not a policy re-definition; G3.4 records `canonical_join` migrating OFF `clean_url` ONTO the identity key (G3.K), and `get_url_key`/"identity key" appears in the siblings as a reference to G3.K. | PASS |
| (iii) | Cache is defined **once** in artifact 10-cache (G3.9) and only *delegated* from G3.H (and G3.4) — never redefined. | `semantic-cache-contract.md` `single_writer` = "SINGLE WRITER of v3 cache semantics"; `host-annotation-contracts.md` `single_writer` explicitly EXCEPTS "the annotation cost/cache contract (G3.9)"; G3.4's owning-contract legend + `rurl_cache_*` rows defer cache semantics to G3.9; HOST-O4 coordinates PSL cache **mechanics** with G3.9 rather than restating them. | PASS |
| (iv) | The strict-posture default (P2.1 B1) and every other P-tier default are cited **identically** everywhere — each `Pn.n` resolves to exactly one `@shortsha` across all nine contracts, with no fork. | Citation census over the nine files: each accepted record appears with exactly one short SHA — `P1.1@a7e0a59`, `P1.2@bb3346e`, `P2.1@a4d1b45`, `P2.2@8292c7f`, `P2.3@a7e0a59`, `P2.4@b017e87`, `P3.1@3b89b94`, `P3.2@bb3346e`, `P4.1@b017e87`, `P5.1@d254ff1`, `P5.3@8292c7f`, `P2.6@5f4309b`. `grep -hoE 'P[0-9]\.[0-9]@[0-9a-f]{7}' <the nine> \| sort -u` yields exactly 12 distinct `Pn.n@sha` pairs — one per decision, zero collisions on a `Pn.n` with two SHAs. | PASS |
| (v) | **No cell is unowned** across the artifact set: every open cell in every contract carries a named settlement destination. | The open-cell census below enumerates all **41** live open-cell IDs (see `### Census tally` for why the figure moved from 44); each row names an owning contract and a destination that is a sibling cell, the unmade P4 host record (RCON-08), the unmade P3 mutation-slice owner decision, a future P5 decision, or §6 artifact 11 / G4. It also records the frozen-text cells a later-accepted decision has since closed (G3.3 `authority_kind` → P1.2@bb3346e; G3.K KJ-O1..O8 → P3.2@bb3346e), and the one forwarded to a sibling (G3.3 undivided-`userinfo` → OUT-O1). None resolves to "nobody." | PASS |

### Note on criterion (iv)

The `@shortsha` in an `accepted_evidence` citation is the commit that carried the
decision onto `main`, so a forked short SHA on a P-tier citation would be a
cross-artifact drift this capstone exists to catch: two siblings would be citing
different versions of the same decision. The census found none: the twelve pairs
above are the complete set, each `Pn.n` mapping to one SHA. (`P5.3@8292c7f` and
`P2.2@8292c7f` share a commit because #212 accepted both; that is one commit
ratifying two decisions, not one decision with two SHAs — no fork.)

This note previously added that "validate-manifest.R hash-enforces the decision
**records** themselves". That validator was deleted by ADR 0014 along with the
rest of the seal phase, so the claim no longer holds and is removed rather than
restated. It was a corroborating remark, not the assertion's ground: (iv) is
re-checkable from the inputs by the census above, which is what the evidence
column requires. What is genuinely lost is the guarantee that a cited SHA still
resolves to the *record content* that was ratified there — under ADR 0014 a
citation is a git reference and nothing more, and re-checking it means reading
the commit.

## Open-cell census (criterion (v) evidence)

Every **live** OPEN cell flagged by any sibling contract, with its owning contract
and named destination. No cell resolves to an unowned owner. Destinations that are
**outside G3** — the unmade P4 host record (RCON-08), the unmade P3 mutation-slice
owner decision, §6 artifact 11 (verification, RCON-09/10) / G4, and future P5
performance decisions — are named as downstream, not as unowned. (The two G3.K and
G3.3 families a later-accepted decision has already closed are recorded separately
below, not here.) This capstone introduces **no** new open cell; it only censuses
these.

| open cell | owning contract (G3 leaf) | settlement destination |
|---|---|---|
| CACHE-O1 | semantic-cache (G3.9) | future P5 cache-performance decision (eviction alternative; default SETTLED) |
| CACHE-O2 | semantic-cache (G3.9) | P5 performance-budget decision (memory-based ceiling) |
| CACHE-O3 | semantic-cache (G3.9) | P5 cache-performance decision (Punycode-cache bound) |
| CACHE-O4 | semantic-cache (G3.9) | §6 artifact 11 / G4 verification (default + reset-watermark coverage) |
| CACHE-O5 | semantic-cache (G3.9) | the P5 parallel/chunk cell (worker/process cache scope) |
| SCHEME-O1 | standard-scheme (G3.5) | the standards/profile-precedence slice (direct-vs-profile `scheme_policy`) |
| SCHEME-O2 | standard-scheme (G3.5) | the credential-accessor implementation-cleanup slice (paired with OUT-O1) |
| SCHEME-O3 | standard-scheme (G3.5) | the P4-email slice + email PRD (`indeterminate` lexer, URL-level email facts) |
| SCHEME-O4 | standard-scheme (G3.5) | a dedicated resolution-layer decision (RFC relative-reference resolution; not owned by P2.4/P4.1) |
| VAL-O2 | validation-intervention (G3.6) | the RCON-05 public-surface decision (repair-posture spelling; cf. CLEAN-O1) |
| VAL-O3 | validation-intervention (G3.6) | the RCON-03 `resolve_url` output-shape record (paired with OUT-O4) |
| VAL-O4 | validation-intervention (G3.6) | a future owner deprecation-schedule decision, post-3.0 (`parse_status` removal window) |
| CLEAN-O1 | cleaning-mutation (G3.8) | the RCON-05 public-surface decision / a cleaning-vocabulary migration slice |
| MUT-O1 | cleaning-mutation (G3.8) | the unmade P3 mutation-slice owner decision (mutation semantics / verb vocabulary) |
| MUT-O2 | cleaning-mutation (G3.8) | the P3 mutation-slice (component dependency graph) |
| MUT-O3 | cleaning-mutation (G3.8) | the P3 mutation-slice (mutation state transitions) |
| MUT-O4 | cleaning-mutation (G3.8) | the P3 mutation-slice (transaction behavior) |
| MUT-O5 | cleaning-mutation (G3.8) | the P3 mutation-slice (mutation eligibility; crosses G3.5 axes) |
| MUT-O6 | cleaning-mutation (G3.8) | the P3 mutation-slice (mutation credential safety) |
| MUT-O7 | cleaning-mutation (G3.8) | the P3 mutation-slice (mutation invariants) |
| MUT-O8 | cleaning-mutation (G3.8) | the P3 mutation-slice, paired with the query PRD (query mutation model) |
| MUT-O9 | cleaning-mutation (G3.8) | the P3 mutation-slice (path mutation model) |
| MUT-O10 | cleaning-mutation (G3.8) | the P3 mutation-slice, with the P4 host record / G3.H (mutation PSL binding) |
| MUT-O11 | cleaning-mutation (G3.8) | the P3 mutation-slice (mutation standards/profile/repair interaction) |
| MUT-O12 | cleaning-mutation (G3.8) | the P3 mutation-slice (mutation downstream safety) |
| OUT-O1 | output (G3.7) | a credential-accessor / public-surface owner decision (S1 Q5; coordinated with SCHEME-O2). Also the destination of G3.3's undivided-`userinfo` open cell |
| OUT-O4 | output (G3.7) | a dedicated safe-display P-tier record + the RCON-03 `resolve_url` output-shape record (paired with VAL-O3) |
| OUT-O5 | output (G3.7) | an owner decision on the source-reproduction guarantee (with the surface-a naming slice) |
| HOST-O1 | host-annotation (G3.H) | the dedicated P4 host record (RCON-08; unmade) — unified typed host-state matrix |
| HOST-O2 | host-annotation (G3.H) | the P4 host record — de-overloaded host terminology + public type axes |
| HOST-O3 | host-annotation (G3.H) | the P4 host record — v3 canonical-IDNA pinning |
| HOST-O4 | host-annotation (G3.H) | the P4 host record — PSL knowledge-source reproducibility (cache mechanics coordinate with G3.9) |
| HOST-O5 | host-annotation (G3.H) | the P4 host record — typed DNS-policy contract + RFC-reg-name DNS-host eligibility |
| HOST-O6 | host-annotation (G3.H) | the P4 host record — full typed IP / numeric-host row-set |
| HOST-O7 | host-annotation (G3.H) | the P4 host record — full-host-identity fields (coordinates with G3.7 surfaces + G3.K keys) |
| HOST-O8 | host-annotation (G3.H) | the P4 host record — optional-result API shape + display-fallback policy |
| PSC-O1 | public-surface-closure (G3.4) | the unmade P4 host record (forwards the host-surface dispositions to G3.H HOST-O2/O4/O5/O7) |
| PSC-O2 | public-surface-closure (G3.4) | G3.7 OUT-O1 / G3.5 SCHEME-O2 (credential-surface dispositions) |
| PSC-O3 | public-surface-closure (G3.4) | G3.8 / G3.7 sibling opens (cleaning/output/query dispositions) |
| PSC-O4 | public-surface-closure (G3.4) | G3.6 / G3.5 sibling opens (status/scheme/email dispositions) |
| PSC-O5 | public-surface-closure (G3.4) | §6 artifact 11 / G4 (verification-surface dispositions; curl, vectorization, introspection) |

### Frozen-text open cells a later-accepted decision has since closed

Two sibling leaves were authored (and their leaf merged) **before** the P1.2 /
P3.2 owner decisions were sealed (both at `bb3346e`, PR #226/#227), so their frozen
`## Open cells` sections still list cells that a subsequently-accepted decision has
closed. The capstone records the post-authoring disposition — surfacing exactly the
cross-artifact closure a consistency check exists to catch. These are **not**
counted among the live open cells above.

| leaf | frozen open cell(s) | disposition |
|---|---|---|
| G3.3 (canonical-state) | `authority_kind` — the `empty` value's operational meaning (S1-F5) | **CLOSED by P1.2@bb3346e**: authority state splits into `authority_delimiter_present: logical` + `authority_payload_kind: {empty, present}` — exactly the resolution G3.3 Q3 deferred to "a dedicated P1 authority-state decision." G3.3's frozen text predates the seal. |
| G3.K (key-join) | KJ-O1..O8 (all eight) | **CLOSED by P3.2@bb3346e** (`P3.2-key-join-closure.md`, ACCEPTED, in `manifest.decisions[]`): `whatwg` default (KJ-O1), root-dot **DISTINCT** (KJ-O2), y-primary right-join mirror (KJ-O3), anti-join keeps non-keyable x (KJ-O4), `by`-only named-vector selector (KJ-O5), deterministic no-silent-repair suffix (KJ-O6), key hidden by default `key_name=NULL` (KJ-O7), type restoration + typed zero-row prototypes (KJ-O8). G3.K's frozen text (leaf #225) predates the P3.2 seal (#227). |

One further G3.3 frozen cell is a live forward, not a closure:

| leaf | frozen open cell | forward |
|---|---|---|
| G3.3 (canonical-state) | Undivided `userinfo` source component vs the `user`/`password` split (S1 Q5) | the `user`/`password` fields are SETTLED; only the augmenting undivided component is open → G3.7 **OUT-O1** (the output contract's credential-handling clause), driven by S1 Q5. (Counted at OUT-O1, not double-counted.) |

### Open cells closed at source

Distinct from the frozen-text group above: these reached their named destination
and were then closed **in their own contract**, so no post-authoring
reconciliation is carried here. They are listed only so a reader comparing this
census against an earlier revision can see why the count moved.

| cell | owning contract (G3 leaf) | disposition |
|---|---|---|
| VAL-O1 | validation-intervention (G3.6) | **CLOSED by P2.6@5f4309b** (`P2.6-ledger-recovery-categorization.md`), the owner-decision extension of P2.1 this cell named as its destination. The four recoveries beyond repeated-`@` are standard-selected parse behavior governed by `url_standard`, not posture-bound interventions, so the `ledger completeness` row settles as "no rows" and the ordered ledger stays at six stages (D-A–D-E). Closed in `validation-intervention-contract.md` itself (`ledger completeness` row + the VAL-O1 bullet), not here. |

### Census tally

**41 live open-cell IDs** across the nine contracts (CACHE 5, SCHEME 4, VAL 3,
CLEAN 1, MUT 12, OUT 3, HOST 8, PSC 5) — each with a named destination; **none is
unowned.** PSC-O1..O5 are pure forwarders to sibling cells (not independent product
cells); HOST-O1..O8 and MUT-O1..O12 forward to the two unmade owner decisions (the
P4 host record / RCON-08 and the P3 mutation-slice); CACHE-O1/O2/O3/O5 and VAL-O4
forward to future P5 / post-3.0 decisions; CACHE-O4 and PSC-O5 forward to §6
artifact 11 / G4. Separately, **9** frozen-text cells that predate a later seal are
already **closed** (G3.3 `authority_kind` → P1.2@bb3346e; G3.K KJ-O1..O8 →
P3.2@bb3346e), and G3.3's undivided-`userinfo` forwards to OUT-O1. No cell in the
set is unowned.

**Why this tally moved from 44 to 41 (`RURL-ztuodtcl`).** Three cells left the
live set, and only one of them is this revision's own work:

- **VAL-O1** — closed at source by P2.6@5f4309b; recorded in "Open cells closed
  at source" above.
- **OUT-O2 and OUT-O3** — closed by **P2.5** (`P2.5-standard-serializer-surface.md`
  §1/§2: the entry point is `serialize_url(x, standard=, form=)`, and both RFC
  postures are exposed as `form =`). That slice correctly removed both from
  `output-contracts.md`'s `## Open cells` and from the destination table above,
  **but did not update this prose**, which kept asserting "OUT 5" and a total of
  44. The count was therefore already wrong by two before VAL-O1 closed. Found
  while reconciling VAL-O1 and corrected here rather than carried.

The live count is reproducible from the destination table above by counting rows,
which is the check that caught the drift. Note that VAL-O1's bullet is *retained*
in `validation-intervention-contract.md`'s `## Open cells`, marked CLOSED, so its
deferral text survives as provenance — a per-contract grep for declared `-O` IDs
therefore still finds it, and this table, not that grep, is the live census.

## Scope boundaries

This contract owns the **cross-artifact consistency assertion** only. It does
**not** define, and must not be read as redefining:

- **Any per-artifact semantics** — the canonical fields (3), scheme/profile
  matrices (5), verdict layering (6), output surfaces (7), cleaning/mutation model
  (8), cache contract (10-cache), host/annotation matrices (10-host), key/join
  contract (9), and public-surface map (4) are their owning contracts'. This
  capstone references them and asserts they agree; it changes none.
- **Any open cell** — it introduces no new OPEN product cell. The census is a
  roll-up of cells the siblings already flagged; each destination is the sibling's
  own, not invented here.
- **The seal machinery** — flipping present-flags/envelopes PROPOSED→sealed,
  staging validator sections, and writing `gates/G3-acceptance.md` is the
  cp-snapshot-3 owner fan-in, NOT this content PR.
- **Downstream artifacts** — §6 artifact 11 (verification, RCON-09/10), the unmade
  P4 host record (RCON-08), and the unmade P3 mutation-slice owner decision are
  named as census destinations; their contracts are authored elsewhere (G4 / a
  later P-tier slice), not here. (The P3 key/join closure decision, by contrast, is
  already MADE — P3.2@bb3346e — and closes G3.K's KJ-O1..O8.)

## Open cells

**None.** This capstone is a consistency assertion; it makes no product decision
and opens no product cell. It only censuses the open cells the nine sibling
contracts already carry (above), each of which has a named destination — that is
the §7 G3 criterion-3 (v) result. If a future re-run of the census surfaced a
genuinely unowned cell, that would be a real finding to escalate to the owner, not
a cell to fill here.
