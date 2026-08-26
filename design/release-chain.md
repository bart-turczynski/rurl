# The seven-package release chain

`rurl` does not ship alone. Six sibling packages sit above and below it, and the
order in which they reach CRAN is a constraint on this repository even though
nothing in the tree encodes it. This is that record.

## Order

Bottom-up, each **live on CRAN** before the next is submitted:

```
raddr  ->  punycoder  ->  pslr  ->  rurl 3.0.0  ->  robotstxtr + pagerankr  ->  sitemapr
```

`raddr` is unattached to `rurl` today (see
[ADR 0018](adr/0018-ip-literals-belong-to-raddr.md)) and can move in parallel;
`punycoder` and `pslr` are hard `Imports:` and cannot. `robotstxtr` and
`pagerankr` are independent of each other and may go together.

The orchestration epic is **PSLR-hrpalwzo**, in the `pslr` workspace rather than
this one.

## Standing decision

**Drain the backlogs first, then submit** (2026-08-12). Nothing is submitted
while a repository still has codable work queued. The paired decision is that
`rurl` ships as **3.0.0** with no further version bumps — see `CONTRIBUTING.md`
for what that does to the release checklist.

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
- `CONTRIBUTING.md` — the CRAN release checklist, and what the 3.0.0 freeze
  changes about it.
