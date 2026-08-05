# design/AGENTS.md

Design documentation. The whole tree is excluded from the package build, so
nothing here affects `R CMD check` or the pkgdown site. See
[README.md](README.md) for the layout and the PRD graduation policy.

- A decision is accepted when it is **merged to `main`**. There are no seals, no
  manifest hash-pinning, and no acceptance cascade —
  [ADR 0014](adr/0014-retire-the-v3-control-plane.md) retired all of it. Do not
  reintroduce a record lifecycle.
- Frontmatter `state:` fields under `work/url-v3/decisions/` are historical.
  Nothing reads them; do not sweep or "fix" them.
- Ordinary work does not require reading the whole `work/url-v3/` workspace.
  Load only the records relevant to the files you are changing.
