# Contributing

Report bugs and request features in the GitLab issue tracker:
<https://gitlab.com/bart-turczynski/rurl/-/work_items>. Report security issues
privately as described in `SECURITY.md`. Send changes as merge requests on
GitLab; the GitHub repository is a read-only mirror.

New code needs tests, and each user-facing change needs one `NEWS.md` bullet.
A merge request must pass the verification command below.

Run verification (the pre-push chain: the hygiene hooks, the toolchain check,
the URL check, the citation and BugReports checks, then `tools/verify.R` in the
`verify` hook: the manifest gates, lintr, spelling, the roxygen docs-drift
check, `R CMD check --as-cran` and the test suite under `LC_ALL=C`):

```sh
pre-commit run --hook-stage pre-push --all-files
```

Install dependencies:

```sh
Rscript -e 'pak::local_install_deps(dependencies = TRUE)'
```

The source and test layout, and durable project context, live in
`ARCHITECTURE.md` and `design/`: the workflow and constraints in
[`design/workflow.md`](https://gitlab.com/bart-turczynski/rurl/-/blob/main/design/workflow.md),
the validation policy in
[`design/validation.md`](https://gitlab.com/bart-turczynski/rurl/-/blob/main/design/validation.md),
and the CRAN release deltas in
[`design/release-deltas.md`](https://gitlab.com/bart-turczynski/rurl/-/blob/main/design/release-deltas.md).

Keep local-only planning state in `_scratch/`. Do not commit `_scratch/`, `.fp/`, secrets, dependency folders, build outputs, or generated caches.
