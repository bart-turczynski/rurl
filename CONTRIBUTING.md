# Contributing

Report bugs and request features in the GitLab issue tracker:
<https://gitlab.com/bart-turczynski/rurl/-/work_items>. Report security issues
privately as described in `SECURITY.md`. Send changes as merge requests on
GitLab; the GitHub repository is a read-only mirror.

New code needs tests, and each user-facing change needs one `NEWS.md` bullet.
A merge request must pass the verification command below.

Install dependencies:

```sh
Rscript -e 'pak::local_install_deps(dependencies = TRUE)'
```

Verification is fleet-ci's local gate, which runs in the fleet Docker image
(settings in `.fleet-ci.yml`), so it needs Docker, python3, Ruby and git, and
a `docker login registry.gitlab.com`. Install the hooks once per clone, from
the package root:

```sh
pre-commit install
.fleet-ci/pre-push --install
```

From then on the gate runs on every push, on each pushed ref's tip, followed
by the pre-commit pre-push hooks (the citation, BugReports, NEWS and toolchain
checks). `pre-commit install -f` (which installs every hook type, from
`.pre-commit-config.yaml`'s `default_install_hook_types`) and
`pre-commit install -f --hook-type pre-push` replace that hook and drop the
gate; re-run `.fleet-ci/pre-push --install` after either. Without `-f`,
`pre-commit install` keeps the gate (pre-commit's migration mode). To run the
gate by hand on `HEAD`:

```sh
.fleet-ci/gate auto
```

The gate checks the committed tree, never uncommitted edits. Its full tier
runs the manifest gates (`tools/verify.R --gates`), docs drift, lintr,
spelling, the NEWS heading, the URL check, the libidn2 assertion, the
locale-relevant tests under `LC_ALL=C` and one `R CMD check --as-cran`.

The source and test layout, and durable project context, live in
`ARCHITECTURE.md` and `design/`: CI and the project settings it depends on in
[`design/ci.md`](https://gitlab.com/bart-turczynski/rurl/-/blob/main/design/ci.md),
the workflow and constraints in
[`design/workflow.md`](https://gitlab.com/bart-turczynski/rurl/-/blob/main/design/workflow.md),
the validation policy in
[`design/validation.md`](https://gitlab.com/bart-turczynski/rurl/-/blob/main/design/validation.md),
and the CRAN release deltas in
[`design/release-deltas.md`](https://gitlab.com/bart-turczynski/rurl/-/blob/main/design/release-deltas.md).

Keep local-only planning state in `_scratch/`. Do not commit `_scratch/`, `.fp/`, secrets, dependency folders, build outputs, or generated caches.
