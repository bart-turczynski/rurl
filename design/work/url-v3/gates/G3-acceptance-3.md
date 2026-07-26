# Gate acceptance: G3 — Contract-matrix closure (§7 G3) — acceptance 3

<!-- Gate-acceptance record (process/evidence). SUPERSEDES gate-G3-acceptance-2,
     per that record's own reopening rule: "Re-acceptance requires a new
     gate-acceptance record superseding this one." It does NOT edit the
     predecessor's hashes -- the predecessor is retained at state SUPERSEDED and
     keeps describing the tree as it stood at ITS acceptance commit, which is
     what makes the reopening rule an audit trail rather than a rewrite.

     What reopened G3: P0.6 (PR #285, c0c8dca) split §6 artifact 4 into an
     ownership INVARIANT (public-surface-closure.md, still a G3 input) and a
     per-cell ROSTER (public-surface-disposition.md, deliberately NOT a G3
     input). Moving the roster out changed the invariant's bytes, which drifted
     this gate's ## Inputs hash for it and, through the capstone's own re-pin,
     for cross-artifact-consistency.md as well. Two of the ten contract hashes
     move; eight are byte-identical to acceptance 2.

     THIS IS THE LAST RE-ACCEPTANCE OF ITS KIND, which is the point of P0.6.
     Acceptance 2 existed only because a new export grew the surface; under P0.6
     that class of change no longer touches any gate input. The forward note
     acceptance 2 carried for serialize_url / get_url_key / the six url_*_join is
     hereby discharged: those surfaces will not reopen G3. -->

## Envelope

| Field | Value |
|---|---|
| id | gate-G3-acceptance-3 |
| gate | G3 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/gates/G3-acceptance-3.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| state | ACCEPTED |
| supersedes | gate-G3-acceptance-2 |
| accepted_commit | c0c8dca (PR #285 squash-merge into main — the P0.6 artifact-4 invariant/roster split; the first commit where the two-file artifact 4 and the I1–I5 enforcement are both in force) |
| accepted_at | 2026-07-26 |
| depends_on | the ten §6 contracts (canonical-state 3, standard-scheme 5, validation-intervention 6, output 7, cleaning-mutation 8, semantic-cache 10-cache, host-annotation 10-host, key-join 9, public-surface-closure 4-invariant, cross-artifact-consistency capstone) — hashed in ## Inputs; P0.6 (the closure-input scope, ACCEPTED at this seal and hash-enforced in manifest.decisions[]); P1.1, P1.2, P2.1, P2.2, P2.3, P2.4, P3.1, P3.2, P4.1, P5.1, P5.3 (ACCEPTED, hash-enforced in manifest.decisions[]); reconciliation §6, §7 G3 |
| manifest_registration | pinned in manifest.artifacts[] by the seal PR carrying this record |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |

## What changed since acceptance 2

The gate's substance is unchanged. **No cell moved, no disposition changed, and no
public surface was added or removed.** What changed is where artifact 4's roster
lives, and therefore which future changes can reopen this gate.

| Artifact | Change |
|---|---|
| `contracts/public-surface-closure.md` | becomes the INVARIANT half: states I1–I5 and keeps the legend, the agreement slice, the scope boundaries and the PSC-O1..O5 groups. The three roster tables, the bijection table, and every transcribed count are removed |
| `contracts/public-surface-disposition.md` | **new** — the ROSTER half, carrying all 52 rows verbatim. Pinned in `manifest.artifacts[]`; deliberately absent from this record's `## Inputs` |
| `contracts/cross-artifact-consistency.md` | its `## Inputs` pin for the invariant re-derived, plus a statement of why the roster is not pinned there either (projection, no assertion change) |
| `tools/validate-records.R` | enforces I1–I5 over the roster by NAME against `NAMESPACE` / `.spu_result_fields`; loses the `30L` / `18L` / `52L` literals; fails if a roster table reappears in the gate-pinned invariant |
| `tools/traceability-gate.R` | learns the roster as an eleventh claim source (prefix `PSD`) so the artifact-4 claims stay in the G4 population |
| `verification/traceability-map.md` | regenerated; `## Inputs` re-derived (two entries had silently gone stale) |
| `decisions/P0.6-g3-closure-input-scope.md` | the owner decision authorizing all of the above; ACCEPTED at this seal |

The authorization for the scope change is **P0.6**, accepted by the merge of PR
#285 and stamped by this seal. No ADR changes.

The other eight contract hashes are byte-identical to acceptance 2, which is the
machine-checkable statement that this re-acceptance has no wider blast radius.

## §7 G3 exit criteria

Each criterion is re-affirmed at this acceptance commit. Criterion 1 is restated
because the artifact that satisfies it is now two files. Criteria 2 and 3 are
unchanged — no open cell moved and no cross-artifact term changed — so their
evidence is carried forward by reference rather than re-argued.

| # | §7 G3 criterion | Result | Evidence |
|---|---|---|---|
| 1 | The artifact set in §6 exists and contains no unowned cells. | PASS | All ten gate-input contract artifacts present on main (hashed below). Artifact 4 satisfies the criterion as a stated invariant plus an enforced roster: `public-surface-closure.md` states I1–I5, and `validate-records.R` asserts them over `public-surface-disposition.md` on every run — every `NAMESPACE` export and every `.spu_result_fields` entry has exactly one roster row and vice versa (by name, not by count); every row names an owning contract drawn from the legend; every SETTLED row cites a decision or ADR; every OPEN row cites an open-cell ID that **exists in the contract it names**, or a named downstream artifact. That last check did not exist at acceptance 2. |
| 2 | Open product cells have an owner decision or are explicitly deferred outside G3. | PASS | Unchanged from acceptance 2: the G3.X open-cell census is untouched — relocating owned rows opens no cell, and I4 forbids the roster from opening one. |
| 3 | Cross-artifact terms, defaults, state fields, and status codes agree. | PASS | Unchanged from acceptance 2: the capstone's (i)–(v) verdicts still all read PASS, and `validate-records.R` asserts that. The capstone's only edits here are a re-derived input pin and a scope note, not an assertion. |

## Inputs

The gate's central inputs — the ten §6 contract artifacts — hashed at **this**
acceptance commit. `validate-records.R` recomputes each `sha256` on every run; a
mismatch fails this record and reopens the G3 acceptance. The bound P-tier
decisions (now including P0.6) are hash-enforced separately by
`validate-manifest.R` (`decisions[]`) and are not duplicated here.

`public-surface-disposition.md` is **deliberately not listed**. That omission is
the substance of P0.6, not an oversight: the roster is byte-pinned in
`manifest.artifacts[]` and its conformance to I1–I5 is asserted executably, so a
surface change is tamper-evident and checked without requiring a gate
re-acceptance.

| path | sha256 |
|---|---|
| design/work/url-v3/contracts/canonical-state-contract.md | efebe54e645dfcaf56aa2e7d78fcad37d1f266952431a247d5197624996f43ca |
| design/work/url-v3/contracts/standard-scheme-matrices.md | f36ddf6cb8283af7203e7267c2bb1635df49fc670129134cfe9825cda5589d18 |
| design/work/url-v3/contracts/validation-intervention-contract.md | aa06d046d495c94f5f1cba5bef923c666653cdceaedabc2ebee66d86d2679c18 |
| design/work/url-v3/contracts/output-contracts.md | d0570174098f2caf454acfcae7a25517c16fd11c703e7c07242cb221d3dff455 |
| design/work/url-v3/contracts/cleaning-mutation-contracts.md | d8996daf621ca9409d7249116c15efa1740379eda836eb4ce81aa9278ff8a255 |
| design/work/url-v3/contracts/semantic-cache-contract.md | a70acc712e3db5f1426d45106e925d445af048df03086a230e04a4545fc67001 |
| design/work/url-v3/contracts/host-annotation-contracts.md | ec67597447dd0c57dd8c0c7bc9e2216d6bc3c0ee75e956ec729e9a499b551ee0 |
| design/work/url-v3/contracts/key-join-contracts.md | c8ab02251a2dda7760265ab32a889338134d81f938e0fba2a85de48d1063a7d3 |
| design/work/url-v3/contracts/public-surface-closure.md | 9a1cd6c68141c55386818a6ec43e8aeecef973417ce442923e5a156a012e782a |
| design/work/url-v3/contracts/cross-artifact-consistency.md | 4931bde27370e5f95f6bfeeaaac67e6f6f3356862d5cd570a222db52b5073947 |

## Supersession mechanics

Unchanged from acceptance 2 and exercised a second time here, which is the first
evidence that the mechanism generalizes rather than having been fitted to one
case. `gates/G3-acceptance-2.md` is retained at `state: SUPERSEDED` with
`superseded_by: gate-G3-acceptance-3`; it is not hash-checked, because its
`## Inputs` describe the tree at *its* acceptance commit. `gates/G3-acceptance.md`
remains SUPERSEDED by acceptance 2 — a chain, not a rewrite. `validate-records.R`
still asserts **exactly one ACCEPTED record per gate**, so the chain can never
leave G3 unattended or with rival live acceptances.

## Why this should be the last acceptance of its kind

Acceptances 2 and 3 were both bookkeeping: acceptance 2 because one export grew
the surface, acceptance 3 because the fix for that had to move a file. Under P0.6
neither cause can recur — a new export changes the roster and the manifest pin,
both inside its own PR. The queued surfaces (`serialize_url` / `RURL-zthbwebb`,
the eight key/join exports / `RURL-mihbyjsr`, `format_url` / `RURL-nyjnplyh`) will
not reopen this gate, and acceptance 2's forward note to that effect is
discharged.

What *will* still reopen G3, correctly: an edit to I1–I5, to the owning-contract
legend, to the agreement slice, or to any of the other nine contracts' pinned
bytes, and any bound P-tier decision leaving `state: ACCEPTED`.

## Validation results at the acceptance commit

| validator | result |
|---|---|
| `validate-records.R` | VALIDATION PASSED |
| `validate-manifest.R` | VALIDATION PASSED (strict) |
| `validate-transitions.R` | VALIDATION PASSED |
| `ci-gate.R` (control-plane) | CONTROL-PLANE GATE: PASS (strict — run with no `CI_GATE_BASE_SHA`, as the push-to-main job runs it) |
| `traceability-gate.R` | PASS (claim population byte-identical across the split: 480 claims) |
| `deferral-gate.R` / `oracle-label-gate.R` | PASS |

## Reopening rule

Identical in force to acceptances 1 and 2, and inherited verbatim: this acceptance
is not terminal evidence and holds only while its inputs hold. If any `## Inputs`
hash drifts, any bound P-tier decision leaves `state: ACCEPTED`, or a contract's
cell discipline breaks, the validators fail and G3 reopens. Re-acceptance requires
a new gate-acceptance record superseding **this** one — never an edit to this
record's hashes.

## Lifecycle log

| when | state | by | evidence |
|---|---|---|---|
| 2026-07-26 | ACCEPTED | github:bart-turczynski | supersedes gate-G3-acceptance-2; re-accepted after P0.6 split artifact 4 into invariant + roster at `c0c8dca` (PR #285) |
