# The eight-package release chain

`rurl` does not ship alone. Seven sibling packages sit above and below it, and
the order in which they reach CRAN is a constraint on this repository even
though nothing in the tree encodes it. This is that record.

## Order

**Correction 2026-09-29: `rurl` shipped as 3.0.1, not 3.0.0.** The 3.0.0
submission failed CRAN's incoming pre-tests on 2026-09-06, and the fix reached
CRAN as **3.0.1** on 2026-09-09 (`cran-comments.md`, the `## rurl 3.0.1`
heading in `NEWS.md`, tag `v3.0.1`). There is no `v3.0.0` tag. The diagram and
the paragraph under it keep the 2026-09-06 plan as written. The downstream
floors moved with it: `pagerankr`, `sitemapr` and `robotstxtr` now all require
`rurl (>= 3.0.1)`, and CRAN serves `pagerankr` 0.1.0.

**Corrected 2026-09-06.** The version below replaces an order that put `rurl`
last. That order was correct when it was written on 2026-07-18 — `rurl` then
carried `Remotes:` git pins on unreleased siblings, and CRAN held `pslr` 1.0.1
and `punycoder` 1.1.0. Both of those links have since shipped and `rurl`'s
`Remotes:` field is gone, so the constraint it encoded no longer exists.

**`rurl`'s upstream is already on CRAN.** Verified 2026-09-06 against live
CRAN: `punycoder` **1.2.1** and `pslr` **1.1.1**, which are exactly the floors
`DESCRIPTION` names. Nothing upstream is waiting on anything.

```
                        [on CRAN already]
                   punycoder 1.2.1   pslr 1.1.1
                              \       /
                            rurl 3.0.0            <- the keystone; submit first
                                  |
                pagerankr + sitemapr + robotstxtr <- one at a time
                                  |
                                seor              <- last

raddr                     parallel, gates nothing (ADR 0018)
punycoder 1.3.0 -> pslr   parallel, gates nothing downstream
```

`rurl` 3.0.0 is the single unlock for three packages at once: `pagerankr` needs
`rurl (>= 3.0.0)`, `sitemapr` `(>= 2.1.0)` and `robotstxtr` `(>= 2.2.1)`, and
CRAN serves `rurl` **1.2.0**. Those three are also `seor`'s remaining blockers,
so one submission clears a four-deep chain. `raddr` is unattached to `rurl`
(see [ADR 0018](adr/0018-ip-literals-belong-to-raddr.md)) and moves in
parallel.

The orchestration epic is **SEOR-eqpdrqnl**, in the `seor` workspace. It
supersedes `PSLR-hrpalwzo`, which carried the order above and was filed inside
a member's tracker rather than at fleet level — the reason it drifted
unnoticed. Fleet sequence, gates and submission live in `SEOR-*`; `rurl`'s own
code work stays in `RURL-*`.

## Standing decision

**Drain the backlogs first, then submit** (2026-08-12). Nothing is submitted
while a repository still has codable work queued. The paired decision is that
`rurl` ships as **3.0.0** with no further version bumps. That held until CRAN's
pre-tests rejected 3.0.0 on 2026-09-06. The release shipped as 3.0.1, and the
freeze ended with it.

## Pinning a sibling: use a commit SHA, not a tag

`rurl`'s `DESCRIPTION` carries **no `Remotes:` field** today, and the version
floors are the whole contract. The rule below applies the moment a sibling pin
is reintroduced, and it is recorded here because the failure it describes has
already happened once in the family.

**A `Remotes:` entry that is not pinned silently imports unreleased data.**
`pslr` absorbed `punycoder`'s unreleased Unicode 17 table through an unpinned
remote: `R/sysdata.rda` was rebuilt under it, so CRAN `pslr` and local `pslr`
produce **different data with no version recording the difference**. Nothing is
red anywhere — the local tree is self-consistent and CRAN's is self-consistent,
and only a clean-room build compares them.

So: pin to a **commit SHA**, never to a tag. A tag is not a weaker SHA, it is a
different claim — in that case the newest `punycoder` tag *predated* the change
the pin needed and broke four tests, so pinning "the latest tag" was both
unpinned in effect and wrong in fact.

Two related hazards to check at the same time:

- **A floor may name a version that never existed.** Downstream siblings have
  carried `rurl (>= 3.0.0)` and `rurl (>= 2.1.0)` against tags that do not
  exist. `Rscript tools/dependency-resolvability-gate.R --all` is the instrument
  for this; `AGENTS.md` documents why it is deliberately not in the gate list.
- **A `Remotes:` entry masks the floor it was meant to satisfy.** With the
  development checkout in the library, the floor is never tested against what
  CRAN actually serves.

## Related

- `AGENTS.md` — the resolvability gate, and the rule for reaching into a sibling
  export from a test.
- `CONTRIBUTING.md` — rurl's deltas to the fleet CRAN release checklist.
