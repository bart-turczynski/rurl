# FOSSA license findings — rationale for `rurl`

This file is the **reviewable, version-controlled record** of why each FOSSA
license finding for `rurl` is acceptable.

Custom FOSSA **Policies** are a paid feature not available on this account, so
these decisions are **not** enforced server-side and `.fossa.yml` binds no
policy. Instead, the chosen disposition is to treat FOSSA as **informational**
(not a required, deploy-blocking status check), because — as documented below —
none of the findings are a real licensing risk for an MIT project. This file is
the written justification for that decision.

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

## How this is handled (free tier — no custom policies)

Custom policies are paywalled, so the findings cannot be approved/allowed
server-side for this account.

**Verified: FOSSA gates nothing in GitHub.** `main` has no branch protection
and no rulesets, and FOSSA posts no commit status/check to the repo. The red
"failing" state is internal to the FOSSA dashboard (its default policy flags
copyleft); it does not block merges, the gh-pages/pkgdown deploy, or
`R-CMD-check` — all of which pass. There is therefore nothing to "unblock."

Disposition:

1. **Treat FOSSA as informational.** Since the red is cosmetic and internal to
   FOSSA, and every finding is benign for an MIT project, no action is required
   to keep deploying. This file is the justification.
2. **Optional — disconnect FOSSA.** If the red dashboard is unwanted, remove the
   `rurl` project in FOSSA or de-scope the FOSSA GitHub App for this repo. At
   the free tier with all-benign findings it adds little here.
3. FOSSA last resolved `pslr 1.0.1`; current is `1.0.2` — a fresh scan should
   reconcile the version.

## Notes
- If a paid plan with policies is ever adopted, the allow-list table above maps
  directly onto a named policy; bind it via `project.policy` in `.fossa.yml`.
- Anything genuinely runtime + strong-copyleft (GPL/AGPL without exception,
  in `Imports`) is **not** covered here and must be reviewed, not auto-allowed.
