# Public-surface closure — the ownership invariant (§6 artifact 4)

<!-- Contract artifact (§6 artifact 4, INVARIANT half). RCON-05 (public-surface
     half) closure — the explicit G1→G3 handoff for the public-surface inventory.
     This record does NOT re-author the G1 inventory (register
     public-surface-inventory.md); it establishes the RULE by which every
     inventory row's "unknown — RCON-0X pending owner tier PY" migration
     disposition is projected onto the owning G3 contract that now governs it:
     each cell is marked SETTLED (with the accepted decision the owning contract
     projects) or OPEN (with the exact open-cell ID in the owning contract, or a
     downstream artifact for surfaces outside G3). It makes NO new product
     decision: it assigns owners, it does not decide.

     P0.6 SPLIT. Artifact 4 is deliberately two files. THIS file is the
     invariant: the ownership rule, the owning-contract legend, the citation
     discipline, the cross-artifact agreement slice, and the PSC-O1..O5 open-cell
     groups. The per-cell ROSTER — one row per export, field, curl surface, and
     the migration surface — is `public-surface-disposition.md`.

     The reason is that G3 exit criterion 1 ("the artifact set in §6 exists and
     contains no unowned cells") is a universally quantified PROPERTY, while a
     roster is its extension. While both lived here, the gate's `## Inputs` hash
     pinned the extension, so every added export reopened contract-matrix closure
     even though the invariant was untouched and the new row satisfied it. This
     file therefore stays a G3 `## Inputs` pin and changes only when the
     ownership model changes — which is exactly when G3 SHOULD reopen. The roster
     is byte-pinned in manifest.artifacts[] and checked executably instead.

     Consequently NO SURFACE COUNT APPEARS IN THIS FILE. Counts are roster facts,
     derived by validate-records.R from NAMESPACE and `.spu_result_fields`; the
     stale "29 exports / 51 rows" prose this file used to carry (surviving a
     51→52 growth that moved only the tables) is exactly the failure mode that
     removing them prevents.

     This is the SINGLE WRITER of the public-surface ownership invariant; the
     per-export/field v3 SEMANTICS live in the owning contracts (3/5/6/7/8/9/10/K)
     and are referenced, never redefined. Terms/status/state fields are asserted
     to agree with artifacts 3 and 5–10 (full cross-artifact enforcement is the
     G3.X capstone). Format follows the G3.3/G3.5/G3.6/G3.7/G3.8/G3.H/G3.K
     precedent. Envelope stays lifecycle_state PROPOSED until the cp-snapshot-3
     seal; validator coverage + the register's normative-closure state flip ride
     that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-public-surface-closure |
| name | public-surface-closure |
| artifact_number | 4 (invariant half; the per-cell roster is `public-surface-disposition.md`) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/public-surface-closure.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the public-surface ownership INVARIANT: what makes a surface cell owned, the owning-contract legend, the citation discipline, and the PSC-O1..O5 open-cell grouping. The per-cell assignments are written by `public-surface-disposition.md` (P0.6) |
| lifecycle_state | PROPOSED |
| dependencies | reg-public-surface-inventory (the G1 inventory being closed, hashed below); contract-public-surface-disposition (the roster this invariant governs — referenced by name, deliberately NOT hashed here; see `## Inputs`); the owning G3 contracts (canonical-state artifact 3; standard-scheme 5; validation-intervention 6; output 7; cleaning-mutation 8; semantic-cache + host-annotation 10; key-join 9) referenced by name; the accepted decisions those contracts project (hash-enforced in manifest); P0.6 (the invariant/roster split); reconciliation §6 artifact 4, §4 RCON-05, §7 G1/G3 |
| bound_decision | P0.6 (the artifact-4 invariant/roster split). No product decision is made here — this record assigns owners; the governing decisions (P1.1/P1.2/P2.1–P2.4/P3.1/P3.2/P4.1/P5.1/P5.3) are projected by the owning contracts and cited per roster row |
| bound_evidence | reg-public-surface-inventory (the G1 inventory) |
| closes_finding | RCON-05 (public-surface registry half; the standard/profile/scheme-matrix half is G3.5) |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells, stated as an invariant over the surface rather than over a count: **every** public-surface cell (exported function, public output field, curl-dependency surface, migration surface) carries an owning G3 contract drawn from the legend below and a v3 disposition that is SETTLED (citing the owning contract's projected accepted decision) or OPEN (citing an open-cell ID that exists in the owning contract, or a named downstream artifact); no OPEN disposition invents a question; terms/status/state fields agree with artifacts 3 and 5–10. The cell population is `NAMESPACE` + `.spu_result_fields` + the curl/migration rows, derived — never transcribed as a number. `validate-records.R` enforces this over `public-surface-disposition.md` on every run |
| content_hash | per-input sha256 under `## Inputs` (the inventory register), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip); the G1 inventory register is NOT transitioned by the seal — it stays historical PROPOSED/DISCOVERED evidence, present:true-pinned since cp-snapshot-2 |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | the public-surface-closure section of validate-records.R checks this invariant's own structure (legend, citation discipline, open-cell grouping); the public-surface-disposition section checks the roster against it. Neither pins a surface count |

## Purpose

The normative closure of the §6 artifact-4 public-surface inventory: it takes the
G1 inventory through the G3 handoff by requiring that every export, field, and
dependency surface be assigned to the owning G3 contract that now governs its v3
disposition, and marked SETTLED or OPEN. It settles no new product decision — the
per-surface v3 semantics live in the owning contracts; this record is the
*ownership rule* that makes "no public-surface cell is unowned" a checkable claim.

This record **projects** the inventory register onto the owning contracts; it
makes no product decision. A `SETTLED` disposition cites the owning contract plus
the accepted decision that contract projects (e.g. "G3.6 / P2.3@a7e0a59"). An
`OPEN` disposition cites the owning contract's already-flagged open-cell ID (e.g.
"G3.8 CLEAN-O1") or a named downstream artifact (§6 artifact 11 / the unmade P4
host record) — never a new open question invented here.

Under P0.6 the per-cell assignments live in the roster,
[`public-surface-disposition.md`](public-surface-disposition.md). This file states
the rule; that file instantiates it; `validate-records.R` checks the second
against the first on every run.

## Inputs

The exact source this contract closes. The hash records **which bytes I1–I5 and
the PSC-O groups were last checked against** — it is not bookkeeping, so it may
only be refreshed by someone who has re-run those checks. The owning G3
contracts are referenced by name; the accepted decisions they project are
hash-enforced elsewhere.

Nothing recomputes this hash. ADR 0014 retired the `## Inputs` sha256 comparison
along with the seal phase (`validate-records.R`'s
`## --- Gate-acceptance input hashes: RETIRED (ADR 0014)` section keeps the
reasoning at the site; `validate-manifest.R`, named here until this revision, was
deleted by the same ADR). The gap is deliberate: the remedy when
this input drifts is to re-run the invariant, which no machine can confirm
happened.

**Re-verified and re-pinned on 2026-08-07 (`RURL-arwfxzul`).** The register had
carried its authoring hash (`0616de7`) through the eight-export identity surface
of `RURL-mihbyjsr` (`9fce025`). That drift is eight new `exported-function` rows
plus one envelope edit deleting a transcribed export count. What the re-check
found, per clause:

- **I1 — holds, and the drift moved toward it.** The envelope edit deleted the
  literal count from the register's `completion_rule` in favour of a derived
  bijection, which is what I1 demands. The only numerals left in artifact 4 are
  this file's header comment recounting the stale prose that was *removed* — a
  description of the failure mode, not a normative count.
- **I2 — holds at the grown size.** The bijection was re-derived from `NAMESPACE`
  rather than read off a table: every export resolves to exactly one roster row
  and every roster row to an export, with no missing row, no extra row and no
  duplicate. All eight new cells name `G3.K`, which is in the legend.
- **I3 — held for seven of the eight; the eighth was a real finding, since
  repaired.** Each new row is `SETTLED` and cites a P3-tier decision. But
  `url_key_policy` cites `P3.3@d9b0976`, and at that revision **G3.K did not
  project P3.3**: `key-join-contracts.md` did not mention it, and the legend row
  below listed only P3.1 and P3.2. The cause was recorded in P3.3's own `scope` —
  the contract edit was deferred to the cp-snapshot-3 seal, which ADR 0014
  retired, leaving it uncarried. Closed by `RURL-nravluqd`: the truth table's row
  6 now carries P3.3's amendment, and the legend row below now lists
  `P3.3@d9b0976`. **That legend edit reopens G3** by this record's own reopening
  rule (see `## The closure invariant`) — which under ADR 0014 is discharged by
  the merge that carries it, acceptance being merge to `main` and nothing else.
- **I4 — holds, untouched.** Every one of the eight is `SETTLED`, so the drift
  invented no open question and did not change the OPEN set. Every open-cell ID
  cited by the roster still resolves in the contract that names it, and the
  PSC-O1…O5 groups below still account for every OPEN row — the three `curl-*`
  rows via PSC-O5's collective phrase, the rest by name.
- **I5 — holds, untouched.** The drift added no public-output-field row, so the
  Stage-A internal/public split is not implicated.

The `## Scope boundaries` claim that this register is "referenced and hashed,
never edited … not transitioned by the seal" also survives, in the sense it was
written: the drift added rows as the surface grew, but transitioned no row's
state — every row is still `DISCOVERED` and the register's `lifecycle_state` is
still `PROPOSED`. Only then is the hash advanced.

Two places where this file's prose was **narrower than what
`validate-records.R` already enforces** were also found, both on the single
`migration-surface` cell: I2 required an owner "drawn from the legend" and I3 a
cited accepted decision, while the validator deliberately admits `artifact 4` as
an owner (its `downstream` predicate in the `public-surface-disposition` section)
and `discharged` as a citation (the shape test in the same section's `SETTLED`
branch). Recorded but **not fixed at the time**, for the same reason as above —
the text at issue is I2 and I3 themselves, and this record's reopening rule makes
an edit to I1–I5 a change to the ownership model. Filed as `RURL-psyozxqw`.

**Since resolved.** The owner ruled that stating a carve-out the validator
already enforces is a **writing correction, not a change to the ownership
model** — the model is what the executable I1–I5 checks enforce, and P0.6 §3
rests the whole no-weakening argument on exactly that. The opposite reading is
self-defeating: it would make a false statement of I2/I3 permanently
uncorrectable, because the rule protecting the text would block every repair.
I2 and I3 above now name both carve-outs. The ruling is **scoped** — it holds
only for an edit that is provably conservative, and this one was proved so:
`validate-records.R` produced byte-identical output before and after (4090
checks, `VALIDATION PASSED`), and a mutation check confirmed that an owner
outside the documented set is still rejected, so the prose describes the
enforced set rather than widening it.

The envelope above and this record's header comment still describe the seal
phase and `validate-manifest.R`. They are left as written: ADR 0014 kept every
record's frontmatter deliberately, rather than churn it to strip a retired
lifecycle. The prose in this section, not the envelope, is what a reader should
believe about how this hash is maintained.

`public-surface-disposition.md` is deliberately **absent** from this table. Hashing
the roster here would put it back inside the pin chain this file's own gate
acceptance records, restoring the cascade P0.6 exists to remove: a new export would
drift this contract, then the capstone, then the G3 acceptance. The roster's
integrity is carried instead by its `manifest.artifacts[]` pin (tamper-evidence)
and by the executable I1–I5 checks (conformance) — neither of which requires a gate
re-acceptance to update.

The `sha256` column this table used to carry was retired 2026-09-04 as an
**inert** pin under ADR 0014 and `design/AGENTS.md` (RUL-013, RURL-rooxvstd):
no gate recomputed it after the `## Inputs` hash comparison was retired
(`validate-records.R` keeps only the path-resolves half), it had gone stale
(`4712cc07…` against a file whose digest had moved), and nothing in the tree
read it. The path list is what the validator still checks, and it stays.

| path |
|---|
| design/work/url-v3/registers/public-surface-inventory.md |

## The closure invariant

The property G3 exit criterion 1 asserts about the public surface, stated so that
it quantifies over the surface rather than describing a particular size of it.

**I1 — Population is derived, never transcribed.** The set of public-surface cells
is exactly: every `export(...)` line in `NAMESPACE`, every entry of
`.spu_result_fields`, the curl-dependency surfaces, and the migration surface. No
count of them is normative anywhere in artifact 4; a number written into a
document is a stale fact waiting to happen.

**I2 — Every cell is owned.** Each cell has exactly one roster row, and that row
names an owning contract drawn from the legend below, **or one of two downstream
artifacts: §6 artifact 11 (the verification contracts, the legend's non-leaf row)
or §6 artifact 4 (this roster and its own invariant).** A cell with no row, a row
with no cell, and a row naming a contract outside that set are each a violation.

The artifact-4 owner exists for exactly one cell, `migration-surface`, and is not
a loophole: that cell is *about the roster*, so it cannot name an owner in a
legend of **other** contracts without asserting something false. It is also the
reading the roster's own `completion_rule` already takes — owners "drawn from the
ten §6 contracts", of which artifact 4 is one. Naming it here removes a
disagreement between two governance records; it does not widen the set.

**I3 — Every disposition is SETTLED or OPEN, with a citation that resolves.** A
`SETTLED` row cites the accepted decision its owning contract projects — **or,
where the owner is artifact 4 and the settlement is a discharge rather than a
projection, the literal `discharged` together with the discharge it states.** An
`OPEN` row cites an open-cell ID **that exists in the contract it names**, or a
named downstream artifact (§6 artifact 11 / the unmade P4 host record). A citation
that resolves to nothing is a violation, not a formatting nit.

**I4 — No open question is invented in artifact 4.** Every OPEN disposition
forwards to a question already flagged by an owning contract or a named downstream
artifact. Artifact 4 assigns owners; it never opens cells.

**I5 — The Stage-A internals stay out.** The 21 `.spu_stage_a_fields` are internal
and are not public-surface cells; their public/internal split is the artifact-3
canonical-state matrix (P1.1@a7e0a59 §1.4). Named here as explicit non-omission.

I1–I5 are enforced by `validate-records.R` over the roster on every run — see that
file's `public-surface-disposition` section. **This is what the G3 acceptance
pins.** A roster edit that keeps I1–I5 true changes no fact this file asserts, and
therefore reopens nothing; a change to I1–I5 or to the legend is a change to the
ownership model and reopens G3 exactly as the reopening rule prescribes (P0.6).

## Owning-contract legend

| G3 leaf | contract | §6 artifact | governing accepted decisions |
|---|---|---|---|
| G3.3 | canonical-state-contract | 3 | P1.1@a7e0a59, P1.2@bb3346e |
| G3.5 | standard-scheme-matrices | 5 | P2.4@b017e87, P4.1@b017e87 |
| G3.6 | validation-intervention-contract | 6 | P2.1@a4d1b45, P2.3@a7e0a59, P1.1@a7e0a59 |
| G3.7 | output-contracts | 7 | P2.2@8292c7f, P5.3@8292c7f, P3.1@3b89b94 |
| G3.8 | cleaning-mutation-contracts | 8 | P2.2@8292c7f, P3.1@3b89b94 |
| G3.9 | semantic-cache-contract | 10 (cache) | P5.1@d254ff1 |
| G3.H | host-annotation-contracts | 10 (host) | P4.1@b017e87 + host-facing boundaries (RCON-08 deep record UNMADE) |
| G3.K | key-join-contracts | 9 | P3.1@3b89b94, P3.2@bb3346e, P3.3@d9b0976 |
| — | verification contracts (NOT a G3 leaf) | 11 | RCON-09/RCON-10; G4 / P5.1 / P5.3 (downstream) |

## Cross-artifact term / status / state-field agreement

The closure asserts the roster's vocabulary agrees with the owning contracts
(full cross-artifact enforcement is the G3.X capstone; this section is the
public-surface slice of it).

| shared concept | canonical owner | agreement | status |
|---|---|---|---|
| the 18 public fields + three-valued presence | artifact 3 (G3.3) | every field row of the roster names an artifact-3 field with its presence/provenance | SETTLED |
| `parse_status` = π(L1,L2,L3) compat projection | artifact 6 (G3.6) | the `parse_status` / `get_parse_status` rows are the π projection, not a core verdict | SETTLED |
| `clean_url` = clean surface (c), not identity | artifact 7 (G3.7) | the `clean_url` / `get_clean_url` rows are surface (c); `canonical_join` `clean_url` keying is LEGACY | SETTLED |
| comparison key = `get_url_key`, not `clean_url` | artifact 9 (G3.K) | `canonical_join` migrates off `clean_url` to the identity key | SETTLED |
| host `domain*` = PSL registrable, not full-host | artifact 10 host (G3.H) | the `domain`/`domain_ascii`/… rows are PSL identity keys, not full-host identity | SETTLED |
| cache config/info/clear semantics | artifact 10 cache (G3.9) | the three `rurl_cache_config` / `rurl_cache_info` / `rurl_clear_caches` rows defer cache semantics to G3.9 | SETTLED |
| scheme admission/interpretation + profile lattice | artifact 5 (G3.5) | `get_scheme`/`get_scheme_class`/`url_profile` rows defer to G3.5 | SETTLED |
| companion-helpers never widen the parse frame | ADR 0006 (via G3.6) | `get_host_type`/`get_scheme_class`/`get_url_diagnostics`/`get_parse_verdicts` stay companion | SETTLED |

## Scope boundaries

This contract owns the **public-surface ownership invariant** (I1–I5) and the
legend it quantifies over. It does **not** define, and must not be read as
redefining:

- **The per-cell roster** — which export or field is assigned to which owning
  contract is `public-surface-disposition.md` (P0.6). This file constrains those
  rows; it does not contain them, and it records no count of them.
- **The per-surface v3 semantics** — every export/field's actual v3 behavior is
  the owning contract's (artifacts 3/5/6/7/8/9/10). This record assigns owners and
  cites their SETTLED/OPEN cells; it redefines none of them.
- **The inventory register content** — `public-surface-inventory.md` (the G1
  artifact) is referenced and hashed, never edited; it stays historical
  PROPOSED/DISCOVERED G1 evidence (present:true-pinned at cp-snapshot-2). THIS
  closure contract — not a register edit — supplies the normative closure; the
  register's rows are not transitioned by the seal.
- **Verification contracts (§6 artifact 11)** — curl-removal, the vectorization/
  performance budgets, cache-transparency executable gates, oracle register, and
  introspection budgets (RCON-09/RCON-10) are **outside G3** (G4 / P5.1 / P5.3).
  Rows that defer there are OPEN-downstream, not open here.
- **The unmade P4 host record (RCON-08)** — host rows deferring to HOST-O1..O8
  point at that unmade owner decision (G3.H records them OPEN).
- **Full cross-artifact consistency** — the exhaustive terms/defaults/state-fields/
  status-codes enforcement across all ten artifacts is the **G3.X** capstone; this
  record asserts only the public-surface slice.

## Open cells

No open cell is invented here. Every OPEN disposition in the roster forwards to an
already-flagged open cell in an owning G3 contract or to a named downstream
artifact. The residual open dispositions, grouped by destination:

- **PSC-O1 — host-surface dispositions → the unmade P4 host record (RCON-08).**
  `check_hosts`, `get_domain`, `get_host`, `get_host_type`, `get_subdomain`,
  `get_tld`, `is_valid_host`, and the `host`/`domain`/`tld` fields forward to
  G3.H HOST-O2/O4/O5/O7. **Impact:** their v3 host contract is unspecified until
  the P4 host record lands. **Settles at:** the P4 host record (G3.H open cells).
- **PSC-O2 — credential-surface dispositions → G3.7 OUT-O1 / G3.5 SCHEME-O2.**
  `get_user`, `get_password`, `get_userinfo`, and the `user`/`password` fields
  forward to the undivided-userinfo (OUT-O1) and `get_password` selector-parity
  (SCHEME-O2) open cells. **Impact:** the public credential surface's undivided
  form + selector parity are unspecified. **Settles at:** the credential-accessor
  slice (G3.7 OUT-O1, G3.5 SCHEME-O2).
- **PSC-O3 — cleaning/output/query dispositions → G3.8 / G3.7 open cells.**
  `get_clean_url` (CLEAN-O1), `get_path`/`get_query` and the `query` field
  (MUT-O8/O9), `original_url` (OUT-O5); `resolve_url` left this group when
  P2.7 closed VAL-O3/OUT-O4 (`RURL-irfmmoer`). **Impact:** the
  cleaning-vocabulary rename, query/path mutation model, and source-reproduction
  guarantee are unspecified. **Settles at:** the named sibling open cells.
- **PSC-O4 — status/scheme/email dispositions → G3.6 / G3.5 open cells.**
  `get_parse_status` + `parse_status` field (VAL-O4), `get_mailto_recipients`
  (SCHEME-O3), `url_profile` (SCHEME-O1). **Impact:** the `parse_status`
  deprecation window, email diagnostics, and direct-vs-profile precedence are
  unspecified. **Settles at:** the named sibling open cells.
- **PSC-O5 — verification-surface dispositions → §6 artifact 11 / G4.**
  `query_param_summary` (introspection), `safe_parse_urls` (vectorization / the
  S1-F3 scalar/vector cell matrix), and the three `curl-*` dependency rows
  (curl-removal). **Impact:** these surfaces' executable/migration contracts live
  outside G3. **Settles at:** §6 artifact 11 verification contracts + the S1
  scalar/vector contract (G4 / P5.1 / P5.3).
