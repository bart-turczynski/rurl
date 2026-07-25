# Verification contracts — release slice (§6 artifact 11 — release slice)

<!-- Verification artifact (§6 artifact 11, release slice). RCON-10 / C-10
     executable-evidence dimension. This record makes the ACCEPTED owner
     disposition P0.4 (CRAN hold scope and the exact v2/v3 release boundary)
     executable: the release-LINE rule, its two named lines, and P0.4's
     complementary requirement that no normative language broaden the hold onto
     v2.x or permit a curl-bearing v3.

     Third slice of §6 artifact 11 (after cache, G4.1, and determinism, G4.2),
     authored with its own envelope per the artifact-10/11 slice convention. It
     owns ONLY the release rule. The curl-free CLOSURE gate itself is a distinct
     leaf and is NOT built here (see "Scope boundary" below).

     lifecycle_state PROPOSED until a future G4 control-plane snapshot; a
     verification-family validator section and the manifest present-flip ride
     that seal. Until then no validator globs design/work/url-v3/verification/. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-release-slice |
| name | verification-release-slice |
| artifact_number | 11 (release slice; cache slice is G4.1, determinism slice is G4.2, remaining areas are later G4 leaves) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/release-slice.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this record is the SINGLE WRITER of the release-slice executable-evidence map; it verifies but never names a release line, moves a boundary, or lifts a hold |
| lifecycle_state | PROPOSED |
| verifies | P0.4 (CRAN hold scope + v2/v3 release boundary); C-10; RCON-10 |
| dependencies | P0.4 (the normative disposition); the C-10 row of `registers/contradictions.md`; S8 Blocker 1 (bound evidence); reconciliation §5 C-10, §4 RCON-10, §6 artifact 11, §7 G4 |
| closes_finding | RCON-10 (release-rule dimension); C-10 (executable side) |
| completion_rule | §7 G4 — the accepted release rule is executable and failing; both release lines are named with their governing condition; no normative language broadens the hold onto v2.x or permits a curl-bearing v3; the criteria that are NOT executable today name their carriers; exact local + CI commands are recorded |
| content_hash | pinned at a future G4 control-plane snapshot |
| approval_evidence | pending — seals at a future G4 control-plane snapshot (NOT an envelope flip) |
| validation_command | Rscript tools/release-rule-check.R --self-test |
| validator_note | a verification-family validator section stages with the sealing G4 snapshot; until then no validator globs design/work/url-v3/verification/ |

## Purpose

The executable-evidence contract for the **release rule**: it demonstrates that
the owner-approved disposition of C-10 actually holds in the tracked corpus —
that the "no CRAN until curl-free" hold is bound to the v3 (`3.0.0`) curl-free
line, does not reach back onto the v2.x curl-bearing maintenance line, and is
not silently narrowed away by prose permitting a curl-bearing v3.

This record **verifies**; it does not decide. Every rule traces to P0.4;
nothing here names a release line, moves the boundary, or lifts a hold. Whether
v2.7 ships is a separate owner release act (`RURL-gxqdmpcp`), untouched here.

## Scope boundary — the curl-free closure gate is a different leaf

P0.4 §Consequences gives G4.3 two jobs. The **second** is executable today and
is what this slice does: *"G4 must confirm no normative release language
broadens the v3 hold onto v2.x or lets v3 ship with curl present."*

The **first** — *"the v3 line's curl-free closure must be an executable,
failing gate (RCON-09 / S8 gates 2–3 and 8) — zero-curl static + clean-library
check"* — is **deliberately NOT built here.** Its subject matter is false today:
`DESCRIPTION` declares `Version: 2.7.0` with `Imports: curl`, i.e. the tree is
on the v2.x line, where curl is *permitted*. Carrier: **`RURL-cunfohwy`** (G4
leaf, curl zero-reference closure gate), blocked on the deferred Tier-2 curl
removal.

This slice therefore does **not** assert "3.x cannot ship with curl in
`Imports`". That assertion belongs to the closure gate, whole, in one leaf.
Splitting it — a fragment here, the rest there — would leave neither leaf able
to state its own completion, and would half-build the criterion this slice is
supposed to verify. This mirrors how the determinism slice bounded itself.
*(Owner decision, 2026-07-25.)*

## The gate (answers S8's six required questions)

Policy source: **P0.4** (`decisions/P0.4-cran-release-boundary.md`, ACCEPTED,
`accepted_evidence 7bc5358`). Engine: **`tools/release-rule-check.R`** (base R
only). Register row: the **C-10** row of `registers/contradictions.md`.

1. **What is compared.** Six checks (`R1`–`R6`) over record *structure* and
   normative *prose*. `R1` reads P0.4's **frontmatter** — `state: ACCEPTED`, a
   real `accepted_evidence` SHA, and `affects` claiming C-10 and G4.3. `R2`/`R3`
   read the single C-10 register row: it must cite `P0.4`, carry an `ACCEPTED`
   disposition, and name **both** lines. `R4`/`R5` scan the normative corpus
   sentence by sentence. `R6` requires this slice to name the carriers for the
   criteria it defers.
2. **Which records are in scope.** The **normative** corpus: `decisions/`,
   `registers/`, `contracts/`, `gates/`, and
   `protocol-review-reconciliation.md`. Deliberately excluded: `evidence/`
   (imported review inputs — S8 states the open fork C-10 exists to *close*, so
   scanning it would report the question as an answer) and `worklog/` (working
   notes); `schema/` and `tools/` carry no release prose. **`verification/` is
   excluded too, including this record:** a verification slice must document the
   forms its gate rejects ("`R5` fails on a sentence permitting …"), so scanning
   it would make the gate trip over its own specification and force every future
   slice to carry a bespoke exemption. Verification records verify; by their own
   single-writer envelopes they never decide release policy, so a release rule
   asserted there would be ultra vires regardless. **The reconstruction
   protocol itself is NOT checkable**: it lives in gitignored `_scratch/` and is
   untracked, so its CRAN sentence cannot be gated from CI. That is a real
   boundary of this slice, named rather than skipped — the tracked
   reconciliation is what carries the protocol's substance into the corpus.
3. **What failure means.** `R1`–`R3` fail on a broken chain of authority: a
   P0.4 that is not ACCEPTED, a missing/duplicated C-10 row, a row citing some
   other decision, or a disposition that names only one line. `R4` fails on an
   **unscoped** hold assertion — a sentence prohibiting CRAN release until
   curl-free that names *neither* line, which is precisely the C-10 defect,
   because an unscoped freeze silently governs v2.x too. `R5` fails on a
   sentence **permitting** a curl-bearing v3 release, which would narrow the
   hold to nothing. `R6` fails if a deferred criterion names no carrier — a
   promissory note pointing at nothing.
4. **Where the result is retained.** The check prints a per-rule PASS/FAIL line
   with the offending sentences quoted (file :: sentence), and exits non-zero on
   any failure. It is a verify-chain step, not an artifact producer.
5. **Which changes trigger it.** The verify chain, on every push/PR — the
   corpus it reads is documentation, so no path filter narrows it.
6. **Which graduation step requires it.** The v3 line cannot graduate to a CRAN
   submission while the release rule is unverified (§7 G4). The *other* half of
   that condition — the curl-free closure gate — is carried by `RURL-cunfohwy`
   and must also pass before any v3 submission.

## Reading records: frontmatter, never body prose

`R1` reads P0.4's **frontmatter**. This is load-bearing, not incidental: **13 of
the 16 ACCEPTED v3 decision records still head their body `## Decision
(PROPOSED)`**, and P0.4's own ratification block says *"`state` remains
`PROPOSED`"* while its frontmatter reads `state: ACCEPTED` with an
`accepted_evidence` SHA.

That is **frozen proposal-time trace, by design** — P0.1 §Seal phase 3
enumerates *frontmatter only* as what the seal touches, and P0.1 §6 forbids
in-place substance edits to a record's body. Frontmatter plus the lifecycle log
govern; body prose is historical evidence of what was proposed. A checker that
read body prose would report every ACCEPTED record as unratified, and a
"cleanup" that rewrote those headings would destroy trace while changing nothing
about what binds. Fixture 6 pins this behaviour.

## Positive and negative coverage (§7 G4)

`Rscript tools/release-rule-check.R --self-test` asserts twelve fixtures:

| # | fixture | expected | sign |
|---|---|---|---|
| 1 | an unscoped hold sentence is recognized as a hold, and as unscoped | caught | negative |
| 2 | the same claim scoped to the v3 line | not flagged | positive |
| 3 | a sentence merely mentioning CRAN | not a hold assertion | positive |
| 4 | "a v3 release may ship while curl is still in Imports" | caught | negative |
| 5 | P0.4's own sentence *forbidding* exactly that (negation) | not flagged | positive |
| 6 | frontmatter `ACCEPTED` + body `## Decision (PROPOSED)` | reads ACCEPTED | positive |
| 7 | a record with no frontmatter | error, not a silent pass | negative |
| 8 | a *quoted* unscoped hold vs the same claim in the record's own voice | citation exempt, assertion caught | both |
| 9 | a defect *report* about the hold vs one that reports **and** imposes | report exempt, imposing caught | both |
| 10 | "allowlisted static scan" inside a sentence *requiring* the gate | not a permission | positive |
| 11 | a *quoted* permission example, as a fixture table contains | citation, not a permission | positive |
| 12 | end-to-end: the live tracked corpus | all six rules PASS | positive |

Fixtures 8–11 exist because each is a false positive the first implementation
actually produced against the real corpus: quoting the protocol's bare sentence,
stating the RCON-10 finding, the word "allowlisted" inside the curl-removal
gate's own description, and this slice's own fixture table. They are pinned so a
future tightening cannot silently re-introduce them.

The gate was also verified to **fail** end-to-end, not merely to pass: injecting
`The project will submit no package to CRAN until it is curl-free.` into a
tracked register trips `R4`, and injecting `A v3 release may ship even while
curl remains in Imports.` trips `R5`. A gate that has only ever been observed
passing is not evidence.

## Exact commands

```sh
# The gate's own positive/negative coverage (deterministic, no network):
Rscript tools/release-rule-check.R --self-test

# The gate itself over the tracked normative corpus:
Rscript tools/release-rule-check.R
```

In CI: both run in the `verify` workflow, alongside the C-08 cache-doc gate.

## Boundaries (owned elsewhere, not designed here)

- **The curl-free closure gate** — `RURL-cunfohwy`; see "Scope boundary" above.
- **Whether v2.7 ships** — a separate owner release act, `RURL-gxqdmpcp`. P0.4's
  claim is only that the v3 hold does not bind it; O2 is explicitly deferred.
- **Further v2.x maintenance releases** — P0.4 O3, deferred to a later owner
  release decision on the v2.x line.
- **The reconstruction protocol's own prose** — untracked (`_scratch/`), so its
  annotation is verified by review, not by this gate (see question 2).
- **The remaining §6 artifact-11 areas** — migration, benchmark budgets, the
  fixture/oracle catalog, metamorphic properties, and non-cache/non-determinism
  traceability — owned by later G4 leaves.

## Open cells

**None for the release-LINE rule.** All six checks are executable today and
pass against the tracked corpus.

The two criteria this slice does not verify are **not open cells of this
slice** — they are scoped out with named carriers: the curl-free closure gate
(`RURL-cunfohwy`) and the v2.7 release act (`RURL-gxqdmpcp`). Neither is a
promissory note: both are tracked issues, and `R6` fails this gate if this slice
ever stops naming them.

**One acknowledged limit**, recorded rather than hidden: `R4`/`R5` are prose
predicates over natural-language records. They are pinned by both directions of
fixtures 8–10 and validated against the live corpus, but they detect *forms* of
statement, not intent. A sufficiently novel phrasing of "the hold covers v2.x"
could evade them. They are a regression guard on a corpus whose release
language is already correct — not a proof of its correctness for all future
prose.
