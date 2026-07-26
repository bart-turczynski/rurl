# Gate acceptance: G3 — Contract-matrix closure (§7 G3) — acceptance 2

<!-- Gate-acceptance record (process/evidence). SUPERSEDES gate-G3-acceptance,
     per that record's own reopening rule: "Re-acceptance requires a new
     gate-acceptance record superseding this one." It does NOT edit the
     predecessor's hashes -- the predecessor is retained at state SUPERSEDED and
     keeps describing the tree as it stood at ITS acceptance commit, which is
     what makes the reopening rule an audit trail rather than a rewrite.

     What reopened G3: `get_parse_verdicts()` shipped (PR #281, ff2140a),
     growing the closed public surface 51 -> 52. That edits G3.4's
     public-surface-closure contract, which is a G3 input, so the previous
     acceptance's ## Inputs hash for it drifted and the acceptance reopened
     exactly as designed. Two of the ten contract hashes move; eight are
     byte-identical to acceptance 1. -->

## Envelope

| Field | Value |
|---|---|
| id | gate-G3-acceptance-2 |
| gate | G3 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/gates/G3-acceptance-2.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| state | ACCEPTED |
| supersedes | gate-G3-acceptance |
| accepted_commit | ff2140a (PR #281 squash-merge into main — `get_parse_verdicts()` and the layered verdict surface; the first commit where the asserted 52-row public surface is accurate) |
| accepted_at | 2026-07-26 |
| depends_on | the ten §6 contracts (canonical-state 3, standard-scheme 5, validation-intervention 6, output 7, cleaning-mutation 8, semantic-cache 10-cache, host-annotation 10-host, key-join 9, public-surface-closure 4, cross-artifact-consistency capstone) — hashed in ## Inputs; P1.1, P1.2, P2.1, P2.2, P2.3, P2.4, P3.1, P3.2, P4.1, P5.1, P5.3 (ACCEPTED, hash-enforced in manifest.decisions[]); reconciliation §6, §7 G3 |
| manifest_registration | pinned in manifest.artifacts[] by the seal PR carrying this record |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |

## What changed since acceptance 1

The gate's substance is unchanged. One export was added to the closed public
surface, and the arithmetic that describes it moved in lockstep:

| Artifact | Change |
|---|---|
| `contracts/public-surface-closure.md` | exported functions 29 → 30; total 51 → 52; one new row for `get_parse_verdicts` (G3.6-owned, SETTLED); Stage-A internal fields 20 → 21 (`null_row`, which stays internal) |
| `registers/public-surface-inventory.md` | exactly one added row — the `get_parse_verdicts` inventory entry |
| `contracts/cross-artifact-consistency.md` | its `## Inputs` pin for the closure contract re-derived (projection, no assertion change) |
| `tools/validate-records.R` | pinned bounds 29 → 30 exports and bijection total 51 → 52; plus the supersession rule this record depends on (see below) |

The authorization for the new export is **not** granted here and predates this
record: ADR 0006's v3.0 amendment (P2.3, landed `a7e0a59` / PR #210) authorizes
`get_parse_verdicts()` as a fourth companion helper and fixes its
`url_standard = NULL` behavior. No ADR changes in this seal.

The other eight contract hashes are byte-identical to acceptance 1, which is
the machine-checkable statement that this re-acceptance has no wider blast
radius.

## §7 G3 exit criteria

Each criterion is re-affirmed at this acceptance commit. Criteria 2 and 3 are
unchanged from acceptance 1 — no open cell moved and no cross-artifact term
changed — so their evidence is carried forward by reference rather than
re-argued. Criterion 1 is restated because its count moved.

| # | §7 G3 criterion | Result | Evidence |
|---|---|---|---|
| 1 | The artifact set in §6 exists and contains no unowned cells. | PASS | All ten contract artifacts present on main (hashed below). The public-surface closure (G3.4) maps every one of the **52** inventory rows (**30** exports + 18 fields + 3 curl + 1 migration-surface) onto an owning G3 contract with a SETTLED/OPEN disposition — the new `get_parse_verdicts` row is owned by G3.6 and SETTLED, so no public-surface cell is unowned. `validate-records.R` asserts the bijection at the new totals. |
| 2 | Open product cells have an owner decision or are explicitly deferred outside G3. | PASS | Unchanged from acceptance 1: the G3.X open-cell census (44 live open cells, each with a named destination) is untouched by this change — adding an owned, SETTLED surface row opens no cell. |
| 3 | Cross-artifact terms, defaults, state fields, and status codes agree. | PASS | Unchanged from acceptance 1: the capstone's (i)–(v) verdicts still all read PASS, and `validate-records.R` asserts that. The capstone's only edit here is a re-derived input pin, not an assertion. |

## Inputs

The gate's central inputs — the ten §6 contract artifacts — hashed at **this**
acceptance commit. `validate-records.R` recomputes each `sha256` on every run; a
mismatch fails this record and reopens the G3 acceptance. The 11 bound P-tier
decisions are hash-enforced separately by `validate-manifest.R` (`decisions[]`)
and are not duplicated here.

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
| design/work/url-v3/contracts/public-surface-closure.md | 9b4fdb2d3bc67ceaf9c1aa579c160e4f173ff0df90ef98edf3f0f80a93005e46 |
| design/work/url-v3/contracts/cross-artifact-consistency.md | b678651cc70138269957cae6f5d189741ac360fb49a6c3ed95f58e3bd9ac88ce |

## Supersession mechanics

The predecessor `gates/G3-acceptance.md` is retained at `state: SUPERSEDED`
with `superseded_by: gate-G3-acceptance-2`. It is **not** hash-checked: its
`## Inputs` describe the tree at *its* acceptance commit, so drift against
today's tree is expected, not a defect.

That required teaching `validate-records.R` about supersession, because the
gates section previously demanded `state: ACCEPTED` from every file in
`gates/` and recomputed every hash. Under that rule the reopening rule was not
executable — the only ways to green the build were to sweep the predecessor's
hashes (defeating the rule) or to hide the file from the glob (making an audit
record invisible by a path trick). The validator now accepts a SUPERSEDED
record that names an existing `superseded_by` target, and additionally asserts
**exactly one ACCEPTED record per `gate`**, so supersession can never leave a
gate unattended or with two rival live acceptances.

## Validation results at the acceptance commit

| validator | result |
|---|---|
| `validate-records.R` | VALIDATION PASSED |
| `validate-manifest.R` | VALIDATION PASSED (strict) |
| `validate-transitions.R` | VALIDATION PASSED |
| `ci-gate.R` (control-plane) | CONTROL-PLANE GATE: PASS (strict — run with no `CI_GATE_BASE_SHA`, as the push-to-main job runs it) |

## Reopening rule

Identical in force to acceptance 1, and inherited verbatim: this acceptance is
not terminal evidence and holds only while its inputs hold. If any `## Inputs`
hash drifts, any bound P-tier decision leaves `state: ACCEPTED`, or a
contract's cell discipline breaks, the validators fail and G3 reopens.
Re-acceptance requires a new gate-acceptance record superseding **this** one —
never an edit to this record's hashes.

Note for the queued v3 surfaces (`serialize_url`, `get_url_key`, the six
`url_*_join`): each will grow the closed public surface and therefore reopen
G3 the same way. That is the gate working as scoped, but whether a
surface-count change *should* reopen contract-matrix closure is a live
question carried as `RURL-oygqsykd`; if the answer is no, the fix belongs in
the gate's input set, not in this record.

## Lifecycle log

| when | state | by | evidence |
|---|---|---|---|
| 2026-07-26 | ACCEPTED | github:bart-turczynski | supersedes gate-G3-acceptance; re-accepted after the public surface grew 51 → 52 at `ff2140a` (PR #281) |
