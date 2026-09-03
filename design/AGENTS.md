# design/AGENTS.md

Design documentation. The whole tree is excluded from the package build, so
nothing here affects `R CMD check` or the pkgdown site. See
[README.md](README.md) for the layout and the PRD graduation policy.

- **Before you touch `R/parse*.R` or a conformance test, the reading list is
  seven documents, not this tree:** [`posture-card.md`](posture-card.md);
  ADR 0007 (the selector); ADR 0012 (scope); whichever of ADR 0010 / 0011 /
  0013 governs the axis you are changing; [`oracle-fixtures.md`](oracle-fixtures.md);
  [`oracle-pinning.md`](oracle-pinning.md) with [`measurement-traps.md`](measurement-traps.md);
  and, only if the change flips a contract claim, the one owning contract under
  `work/url-v3/contracts/`. `decisions/`, `evidence/` and `worklog/` are
  archival — do not update them and do not cite them as current authority.
- A decision no gate can derive is a row in
  [`work/url-v3/registers/rulings.md`](work/url-v3/registers/rulings.md). Its
  standing rule says when an agent may add one by citing a clause and when the
  owner must.
- A decision is accepted when it is **merged to `main`**. There are no seals, no
  manifest hash-pinning, and no acceptance cascade —
  [ADR 0014](adr/0014-retire-the-v3-control-plane.md) retired all of it. Do not
  reintroduce a record lifecycle.
- **Two kinds of `## Inputs` pin, needing opposite fixes.** Ask what the record
  *asserts* over its inputs. If the assertion is re-derived by an executable
  gate anyway, the pin is **inert** — delete it and derive the table, which also
  closes what a hash structurally cannot answer (*is this the right SET of
  sources?*, since an absent row has no hash). If the assertion is prose a human
  checked, it is **semantic** — re-verify it by hand, then advance the hash.
  Do **not** build a gate that recomputes every `## Inputs` sha256: under
  ADR 0014 there is no seal, so such a gate clears by editing a file, and the
  only mechanical edit available is the false-fresh re-pin the gate exists to
  forbid. It would also un-retire the mechanism that
  `work/url-v3/tools/validate-records.R` records as deleted — see its
  `Gate-acceptance input hashes: RETIRED` block and its `## Inputs paths
  resolve` comment. This has been filed as a ticket and must stay refused.
- Frontmatter `state:` fields under `work/url-v3/decisions/` are historical.
  Nothing reads them; do not sweep or "fix" them.
- Ordinary work does not require reading the whole `work/url-v3/` workspace.
  Load only the records relevant to the files you are changing.
