# FOSSA policy — source of truth for `rurl`

This file is the **reviewable, version-controlled record** of the license
allow-list / issue-resolution decisions for `rurl`. FOSSA enforces these
server-side in a named **Policy** (`rurl-cran-permissive`, referenced from
`.fossa.yml → project.policy`). `.fossa.yml` cannot hold these rules itself —
keep this file and the FOSSA policy in sync, and update this file in the same
PR whenever a policy rule changes.

## Project license

`rurl` is **MIT**. Its runtime dependency graph (`Imports`) is fully
permissive. The findings below are either dev-only (`Suggests`, not
distributed), benign weak-copyleft with attribution already satisfied, or
scanner false positives from bundled build tooling.

## Allow-list (approve these licenses)

| License | Why it is allowed |
|---|---|
| MIT, BSD-2/3-Clause, Apache-2.0, ISC | Permissive; the baseline for this project. |
| Unicode-DFS / ICU | Permissive; bundled by `stringi` (ICU). |
| MPL-2.0 | Weak (file-level) copyleft, MIT-compatible. Used by the bundled Public Suffix List in `pslr`; attribution already complete (see below). |
| Ubuntu Font License 1.0 (`ubuntu-font-1.0`) | Free font license; only reaches us via dev-only `rmarkdown`. |
| GPL-2.0/3.0 **with autoconf / autoconf-simple exception** | The exception exists precisely so autotools build scripts do not impose GPL on the built software. False positive — build tooling only. |

## Per-dependency resolutions

Each flagged dependency, the finding, the scope, and the disposition.

### `pslr` — MPL-2.0 — **runtime (`Imports`)** — APPROVE
- **Why flagged:** bundles the Public Suffix List data, published by Mozilla
  under MPL-2.0 (the package *code* is MIT).
- **Disposition:** real but benign. MPL-2.0 is file-level copyleft and does
  **not** infect `rurl`'s MIT code. Attribution obligations are **already
  fully satisfied** in `pslr`:
  - `inst/extdata/PSL-LICENSE` — verbatim MPL-2.0 text
  - `inst/NOTICE` — separates MIT (code) from MPL-2.0 (data); pins upstream
    commit + sha256
  - bundled `public_suffix_list.dat` retains the MPL Exhibit A header
  - NOTICE declares the derived `R/sysdata.rda` index is also MPL-2.0
- **Action:** approve MPL-2.0 for `pslr`. No code change.

### `stringi` — GPL-2.0-or-later, GPL-2.0-with-autoconf-exception, gpl-3.0-plus WITH autoconf-simple-exception — **runtime (`Imports`)** — APPROVE / IGNORE
- **Why flagged:** GPL-with-exception headers in the autotools build
  scaffolding shipped in the source tarball (`configure`, `config.guess`,
  `config.sub`, `install-sh`, m4 macros).
- **Disposition:** false positive. `stringi`'s actual code is **BSD-3-Clause**
  (+ bundled ICU/Unicode). The autoconf exception explicitly permits
  distributing the build output under any license.
- **Action:** approve the autoconf-exception licenses (or ignore these issues
  on `stringi`) as build-tooling-only.

### `knitr` — GPL-3.0-only — **dev-only (`Suggests`)** — IGNORE (scope)
- **Why flagged:** `knitr`'s declared license is GPL-3.
- **Disposition:** vignette builder (`VignetteBuilder: knitr`). Not linked,
  not installed at runtime, not in any distributed/deployed artifact.
- **Action:** exclude the dev/test scope from the gate, or ignore this issue.

### `rmarkdown` — GPL-3.0-only, LGPL-3.0-or-later, ubuntu-font-1.0 — **dev-only (`Suggests`)** — IGNORE (scope)
- **Why flagged:** GPL-3 (declared license), LGPL-3 (a bundled JS/CSS web
  asset), Ubuntu Font License (a bundled font).
- **Disposition:** vignette/doc tooling only. Same reasoning as `knitr`.
- **Action:** exclude the dev/test scope from the gate, or ignore these issues.

### `testthat` — GPL-2.0-or-later — **dev-only (`Suggests`)** — IGNORE (scope)
- **Why flagged:** deep-scan hit on an embedded file; `testthat` itself is
  declared MIT.
- **Disposition:** test framework only; never distributed at runtime.
- **Action:** exclude the dev/test scope from the gate, or ignore this issue.

## How to apply (FOSSA UI)

1. **Policies → create/edit** `rurl-cran-permissive`. Add the allow-list
   licenses above to the approved set.
2. For the dev-only `Suggests` (`knitr`, `rmarkdown`, `testthat`): prefer a
   **scope exclusion** (dev/test) over per-issue ignores so future dev-tool
   churn doesn't re-trip the gate.
3. For `stringi` autoconf-exception findings and `pslr` MPL-2.0: approve the
   licenses, or resolve the specific issues with a link back to this file.
4. Re-scan. FOSSA last resolved `pslr 1.0.1`; current is `1.0.2` — a fresh
   scan should reconcile the version.

## Notes
- Keep this file in sync with the FOSSA policy; treat it as the human-readable
  diff of any policy change.
- Anything genuinely runtime + strong-copyleft (GPL/AGPL without exception,
  in `Imports`) is **not** covered here and must be reviewed, not auto-allowed.
