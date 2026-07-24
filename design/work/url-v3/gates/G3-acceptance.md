# Gate acceptance: G3 — Contract-matrix closure (§7 G3)

<!-- Gate-acceptance record (process/evidence). RURL-utiaefwl. The acceptance
     fan-in for §7 G3: the §6 artifact set (contracts 3–10 + 4 + the G3.X capstone)
     exists and contains no unowned cells, every open product cell has an owner
     decision or an explicit out-of-G3 deferral, and cross-artifact terms/defaults/
     state fields/status codes agree. Records the exact contract-input hashes and
     validation results at the acceptance commit; validated by tools/validate-records.R
     (gates section), which recomputes each ## Inputs hash so any later drift in a
     contract FAILS this record and thereby reopens the acceptance and blocks the
     seal. Machine-enforced additionally by the contract-family section of
     validate-records.R (added in PR #236), which asserts each contract's envelope,
     tamper-evident inputs, and SETTLED/OPEN cell discipline. Registered into the
     manifest and hash-sealed at cp-snapshot-3 (owner seal), like the G2 gate and
     the registers before it. -->

## Envelope

| Field | Value |
|---|---|
| id | gate-G3-acceptance |
| gate | G3 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/gates/G3-acceptance.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| state | ACCEPTED |
| accepted_commit | 150f48d (PR #235 squash-merge into main — G3.X cross-artifact-consistency capstone, the last G3 content leaf; all ten §6 contract artifacts present on main) |
| accepted_at | 2026-07-24 |
| depends_on | the ten §6 contracts (canonical-state 3, standard-scheme 5, validation-intervention 6, output 7, cleaning-mutation 8, semantic-cache 10-cache, host-annotation 10-host, key-join 9, public-surface-closure 4, cross-artifact-consistency capstone) — hashed in ## Inputs; P1.1, P1.2, P2.1, P2.2, P2.3, P2.4, P3.1, P3.2, P4.1, P5.1, P5.3 (ACCEPTED, hash-enforced in manifest.decisions[]); reconciliation §6, §7 G3 |
| manifest_registration | pending — pins present:true at v3/cp-snapshot-3 (NOT the sealed cp-snapshot-2 manifest); the ten contracts and this gate are swept into manifest.artifacts[] at the phase-1 sweep |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |

## §7 G3 exit criteria

Each criterion is met at the acceptance commit; evidence is a pointer, not a
re-argument. The three criteria are the reconciliation §7 "G3 — Contract-matrix
closure" exit list.

| # | §7 G3 criterion | Result | Evidence |
|---|---|---|---|
| 1 | The artifact set in §6 exists and contains no unowned cells. | PASS | All ten contract artifacts are present on main (`design/work/url-v3/contracts/*.md`, hashed below). The public-surface closure (G3.4) maps every one of the 46 inventory rows (29 exports + 18 fields + 3 curl + 1 migration-surface) onto an owning G3 contract with a SETTLED/OPEN disposition — no public-surface cell is unowned. `validate-records.R` (contract-family section, PR #236) asserts, per contract, the common envelope, tamper-evident `## Inputs`, and the SETTLED/OPEN cell discipline over every `owner_decision_ref`+`status` matrix (1662 checks). |
| 2 | Open product cells have an owner decision or are explicitly deferred outside G3. | PASS | The G3.X capstone's open-cell census enumerates 44 live open cells (CACHE 5, SCHEME 4, VAL 4, CLEAN 1, MUT 12, OUT 5, HOST 8, PSC 5), each with a named destination: a sibling cell, the unmade P4 host record (RCON-08), the unmade P3 mutation-slice owner decision, §6 artifact 11 / G4, or a future P5 decision. Nine frozen-text cells that predate a later seal are recorded closed (G3.3 `authority_kind` → P1.2@bb3346e; G3.K KJ-O1..O8 → P3.2@bb3346e). No cell resolves to an unowned owner. |
| 3 | Cross-artifact terms, defaults, state fields, and status codes agree. | PASS | The G3.X cross-artifact-consistency capstone asserts (i)–(v) each PASS with a re-checkable evidence pointer: (i) the canonical field vocabulary is defined once by artifact 3 and only referenced by G3.6/G3.7/G3.9; (ii) the comparison key is defined once in G3.K; (iii) cache is defined once in G3.9 and only delegated from G3.H/G3.4; (iv) every P-tier default is cited identically — 11 distinct `Pn.n@sha` pairs, zero forks; (v) no cell is unowned. `validate-records.R` asserts the capstone's five (i)–(v) verdicts are all `PASS`. |

No blocking `OPEN`/`CONFLICT`/`INVALIDATED` record remains: every contract envelope is `PROPOSED` (sealed by manifest hash-pin at cp-snapshot-3, not by an envelope flip), `grep -L "^state: ACCEPTED" decisions/*.md` is empty, and every contract's OPEN cells carry a named settlement destination.

## Closure summary (the ten §6 contract artifacts)

| §6 artifact | G3 leaf | contract | governing accepted decisions |
|---|---|---|---|
| 3 | G3.3 | canonical-state-contract | P1.1@a7e0a59, P1.2@bb3346e |
| 4 | G3.4 | public-surface-closure | (ownership map — projects all owning contracts) |
| 5 | G3.5 | standard-scheme-matrices | P2.4@b017e87, P4.1@b017e87, P1.2@bb3346e |
| 6 | G3.6 | validation-intervention-contract | P2.1@a4d1b45, P2.3@a7e0a59, P1.1@a7e0a59 |
| 7 | G3.7 | output-contracts | P2.2@8292c7f, P5.3@8292c7f, P3.1@3b89b94 |
| 8 | G3.8 | cleaning-mutation-contracts | P2.2@8292c7f, P3.1@3b89b94 |
| 9 | G3.K | key-join-contracts | P3.1@3b89b94, P3.2@bb3346e |
| 10 (cache) | G3.9 | semantic-cache-contract | P5.1@d254ff1 |
| 10 (host) | G3.H | host-annotation-contracts | P4.1@b017e87 (+ boundaries; the deep P4 host record RCON-08 is UNMADE) |
| capstone | G3.X | cross-artifact-consistency | (consistency assertion — no product decision) |

## Inputs

The gate's central inputs — the ten §6 contract artifacts — hashed at the
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
| design/work/url-v3/contracts/public-surface-closure.md | 9460e9898e4862bd601ef91a113e5c4fd895aad58bec38a44a1b0aa29f9fdc86 |
| design/work/url-v3/contracts/cross-artifact-consistency.md | 293a20fd89e587af358cd196a28cfe8979c943a90e927ef2583877baff6748ec |

## Validation results at the acceptance commit

| validator | result |
|---|---|
| `validate-records.R` | VALIDATION PASSED (contract-family section: 10 contracts, 1662 checks — envelopes, tamper-evident inputs, SETTLED/OPEN cell discipline, deep canonical-state, per-contract counts; plus registers + the gate-inputs section) |
| `validate-manifest.R` | VALIDATION PASSED (11 P-tier decisions ACCEPTED + hash-verified; artifact hashes match) |
| `validate-transitions.R` | VALIDATION PASSED (180 checks) |
| `ci-gate.R` (control-plane) | CONTROL-PLANE GATE: PASS (strict) |

## Reopening rule

This acceptance is not terminal evidence: it holds only while its inputs hold. If
any `## Inputs` hash drifts (a contract is edited without re-deriving), any bound
P-tier decision leaves `state: ACCEPTED`, or a contract's cell discipline breaks,
`validate-records.R`/`validate-manifest.R` fail — which reopens G3 and blocks the
cp-snapshot-3 seal (§6 lifecycle: a source or owner-decision change marks
dependents `INVALIDATED` and opens replacements at `DISCOVERED`; it never silently
rewinds history). Re-acceptance requires a new gate-acceptance record superseding
this one.
