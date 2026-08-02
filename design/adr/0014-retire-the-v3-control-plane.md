# ADR 0014: Retire the v3 ratification layer, keep the executable evidence

- **Status:** Accepted
- **Date:** 2026-08-02
- **Tracking:** RURL-wsphrtjc (the blocker this closes), parent epic
  RURL-dorofzmb. Retires the operative half of **P0.1**, **P0.2** and **P0.11**;
  retires gate **G5**. Relates to ADR 0008 (design-docs home).
- **Archive:** tags `v3-control-plane-final` (integration line) and
  `v3-control-plane-final-main`.

## Context

`rurl` must conform to RFC 3986 and the WHATWG URL Standard exactly. **It
already does**: WPT is 336/336, the RFC 3986 profile is 519/519, and the
conformance audit closed with zero gaps. None of the open backlog was a
conformance gap.

What remained was governance. The v3 control plane under `design/work/url-v3/`
had grown to **91 files and ~21,700 lines — larger than `R/`** (18 files,
~13,000 lines): 27 decision records, 11 contracts, 8 registers and 8 validators
governing an 18-file package. Roughly half the open backlog was
evidence-about-evidence rather than product work.

**The structural cause was a mismatch between the mechanism and the team.** The
apparatus implemented a *separation between proposer and approver* — propose,
ratify, seal, with acceptance conferred only by the owner's merge (P0.1 §4).
That is a sound design when those are different people. Here they are the same
person, so every acceptance was the author approving their own proposal: the
ceremony produced a SHA, not an independent check, while costing a branch, a
merge request, a seal commit, a manifest hash and a lifecycle row each time.

Two measurements settled it:

1. When the bound GitHub identity was suspended, restoring the ability to
   *approve one's own proposals* cost a full decision record (P0.11), an
   independent consult, a complete reversal of that record's argument, and two
   merge requests — for zero product value. No step in that chain was a mistake;
   each followed correctly from the last. The premise was wrong.
2. On the rebased integration line the validators reported **6 + 8 failures, and
   every one of them was the ratification layer complaining that records were
   not sealed.** Not one indicated a product defect.

## Decision

**Keep what a machine runs. Cut what only a human stamps.**

Acceptance is now simply *merged to `main`*. There are no seals, no
`accepted_evidence`, no manifest hash-pinning and no lifecycle stamping.

### Retired

- **The seal phase**, and with it `manifest.yaml`, `validate-manifest.R`,
  `ci-gate.R`, `pin-inventory.R`, `restore-drill.{sh,md}`.
- **The gate-acceptance input-hash cascade** (`gates/`, and the section of
  `validate-records.R` that recomputed it). Editing a contract body reopened an
  ACCEPTED gate, which then stayed red until an owner seal-merge — for a change
  git already reports in full.
- **The propagation model** — `validate-transitions.R`, `schema/fixtures/`, and
  `lifecycle.yaml`'s `propagation:` block.
- **`control-plane.yml`**, whose only step ran `ci-gate.R`, and which could not
  run at all once `origin` left GitHub.
- **G5 (independent re-review).** It required "one context-free reviewer" and
  "one evidence-aware reviewer". Those reviewers do not exist, and P0.1 never
  delegated either role, so G5 could only have been satisfied by the author
  reviewing their own work and recording it as independent. A gate that cannot
  honestly be satisfied is not a gate; it is a permanent blocker — and this one
  blocked all remaining 3.0 work.
- **PV10** in the oracle-provenance gate, the rule that made
  `normative_dependencies` mandatory in every source group with no exemption.

### Kept, deliberately

- **Every decision record, contract, register and verification slice.** Only
  their lifecycle machinery is gone. Records keep their frontmatter; nothing
  reads it. Rewriting 28 files to strip `accepted_at` would be pure churn.
- **Every executable gate**: `traceability-gate.R`, `oracle-label-gate.R`,
  `deferral-gate.R`, `release-rule-check.R`, the three doc-consistency gates,
  the determinism gate, the oracle verifiers, and the conformance sweeps.
- **The structural half of `validate-records.R`** — unique ids, resolvable
  references, the `NAMESPACE` bijection, register reconciliation, contract cell
  discipline. These catch defects git does not report. 4564 checks with 6
  failures became **3888 checks, passing**.
- **Fixture integrity** — PV1/PV2/PV3/PV4/PV7/PV8 recompute sha256s, row counts
  and `_meta` mirrors from real bytes. That was never the expensive part.

## Consequences

**Verification is stronger, not weaker.** `validate-records.R` previously ran
only via `ci-gate.R`, which `tools/verify.R` treated as **advisory — it printed
a verdict and never failed a push** — and via a workflow that could not execute.
So the checks that catch real defects were the ones nothing enforced, while the
hash cascade that only a re-seal could clear was blocking. That is now inverted:
the structural validator is an ordinary blocking gate in `verify.yml`, and a
`.gitlab-ci.yml` runs the gate family on the forge that actually hosts the code.

**Contract drift is still caught.** `traceability-gate.R` T3 regenerates the
claim index from the contracts and byte-compares it, so editing a contract still
fails a machine check — one clearable by regenerating rather than by ceremony.
The hash cascade was a second, costlier instrument for a job T3 already does.

**Source pinning became opt-in, not absent.** PV9 skips groups that omit
`normative_dependencies` and PV11 judges only entries that exist, so retiring
PV10 *alone* leaves every written pin held to its full shape and coherence while
new fixtures owe nothing. The provenance data already recorded is retained as
documentation.

**Independent review still happens** — through CRAN's submission checks,
rOpenSci review and R Journal refereeing, by people who did not write the code.
That is what G5 was reaching for and could not provide.

**The acceptance chain ends here, on purpose.** A later reader will find records
whose `state:` says `PROPOSED` and no seal recording their acceptance. That is
accurate: they were merged, and merging is now what acceptance means. The prior
mechanism and everything it produced remain readable at the archive tags.

**What this does not do.** It touches no parser semantics, no public surface, no
test and no conformance fixture. `design/` is `.Rbuildignore`d, so none of it
ships. WPT stays 336/336 and RFC 3986 stays 519/519; any movement in those
numbers would mean this change did something it must not.

**If the project gains a second maintainer**, the separation of proposer from
approver becomes meaningful again and should be reinstated — as branch
protection with a required reviewer, which is the forge's own mechanism, rather
than as a bespoke record lifecycle.
