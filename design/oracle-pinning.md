# Pinning a normative source

Standing method for the pins that `tools/oracle/` and the oracle-provenance
record carry, and that PV9 and PV10 shape. The per-group taxonomy and status
table live in [`tools/oracle/README.md`](../tools/oracle/README.md); this file
holds the rules that apply to any pin, in any record.

## The verification precedes the string

`pin_status: "verified"` asserts, by the record's own convention, that the
derivation **was checked against that revision**. Writing the string before
doing the check converts an honest `missing` — a filed, visible gap — into a
false claim of verification.

So the cost of a pin is the verification work; the string itself is free and
worthless on its own. Before writing any `verified` pin:

1. run the check;
2. make it re-derivable by someone else;
3. falsify it, so you know it can go red;
4. **then** write the string.

**An unverified pin is strictly worse than an honest gap**, because the gap asks
the question and the pin silences it.

## Cite by anchor, not by section number

Section numbers move under a Living Standard while anchors hold. Host parsing
travelled §3.2 → §3.4 → §3.5 across revisions with `#concept-host-parser`
resolving throughout. Citations here are anchor-first everywhere.

## Sweep several revisions when auditing a citation

"Does not match today" and "never matched" are different claims implying
different remedies — drift is repaired by re-pinning, fabrication by deleting
the citation and re-deriving what it was supposed to support.

Date arguments alone are too weak to separate them. Checking the source's
heading structure at several widely separated revisions is what settles it: the
`§3.5.1` / `§3.5.2` citations this record once carried were the second kind —
§3.5 has no subsections in any revision, so no single revision makes the cited
set coherent.

## Ask whether the source is one artifact or two

Before pinning, ask whether the thing being pinned is one artifact or two on
independent release cadences. UTS #46 is a **document** with its own revision
numbering; the IDNA mapping table is **data** versioned by Unicode version, and
the two are not in lockstep — at one pinning both "pin the current UTS-46
revision" and "pin the current Unicode version" were defensible and gave
different answers. Pin both coordinates.

Relatedly, a `verified` pin is not required to be a git SHA. `revision_scheme`
carries `git-commit`, `document-version` or `unpinned` precisely so a rule
demanding a 40-character SHA cannot reject `"Unicode 15.1.0"`.

## Network-reading checks stay out of the gate list

[`tools/oracle/check-uts46-mapping-pin.R`](../tools/oracle/check-uts46-mapping-pin.R)
reads `unicode.org`, so it is run by hand — like `pslr::psl_refresh()` — and is
deliberately absent from `verify.yml`, which names its oracle steps one by one.
Wiring a network step into the gate list would make the gate depend on a third
party's uptime. That posture is the same one the tier-2 oracle checks follow;
`tools/oracle/README.md` records the exit-code convention that keeps an upstream
outage from reading as a fixture defect.

## Related

- [`measurement-traps.md`](measurement-traps.md) — §6 on why a stale pin must
  not be silently refreshed, and §7 on naming what a gate establishes.
- [ADR 0014](adr/0014-retire-the-v3-control-plane.md) — the retirement that
  leaves these pins enforced by nothing but `grep`.
