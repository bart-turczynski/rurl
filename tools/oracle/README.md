# `tools/oracle/` — tracked oracle derivations

Every row of `tests/testthat/fixtures/external-url-vectors.csv` was produced by a
builder script. None of those builders were in the repository: they lived in
gitignored `_scratch/`, so `oracle-provenance.json` recorded `generation_command`
values that no clean checkout could run (`RURL-ozdejfzl`). This directory is
where they land as tracked, fail-closed code.

## The two things a builder was doing at once

The scratch builders interleaved two different jobs in one pass, which is why
none of them reads as either one thing or the other:

1. **Derivation (the oracle).** What does the standard say the answer is? This
   must be independent of `rurl` — an oracle that consults the implementation it
   grades is a characterization test wearing an oracle's label.
2. **Characterization.** What does `rurl` currently answer? This is the
   `rurl_rfc_*` / `rurl_whatwg_*` columns, and it necessarily loads the package.

Files here own **(1) only**. A derivation module must not call
`devtools::load_all()`; the independence is then structural rather than a promise
in a comment, because there is no route from the file to `rurl`'s own answer.

## Fail-closed, and verify rather than regenerate

The entry point for a group is a **verifier**, not a writer. It re-derives the
expected values and compares them to what the fixture already records, exiting
non-zero on any disagreement. It does not rewrite the fixture.

That direction is deliberate. Regenerating a committed oracle in place is how an
implementation change quietly becomes its own expectation: run the builder after
a behavior change and the "expected" column silently follows the code. A verifier
cannot do that — it can only agree or fail.

The scratch builders had the opposite shape, and worse:

- `build-external-vectors.R` **truncated** the fixture, writing only its own 267
  rows. Running it today would destroy the other six groups.
- The other six read the fixture and `rbind()`-appended, so they were
  order-dependent and non-idempotent.
- `build-ada-vectors.R` resolved its input through an **absolute path into a dead
  session scratchpad** (`/private/tmp/claude-501/.../8bfa947d-.../`), a directory
  that no longer exists. It was un-runnable not merely from a clean checkout but
  on the machine that wrote it.

## Reproducibility is not uniform across the seven groups

This is the part the ticket did not anticipate, and it constrains what
`generation_command` can honestly promise. The groups fall into three tiers:

| Tier | Groups | Rows | What re-derivation needs |
| --- | --- | --- | --- |
| **1 — fully offline** | `wpt-credentials-fragments`, `ip-obfuscation` | 43, 24 | Tracked inputs only. `wpt-credentials-fragments` derives from the committed `inst/bench/wpt-url-cases.json`; `ip-obfuscation` is arithmetic over hand-generated encodings and has no input at all. |
| **2 — hash-verified fetch** | `wpt-urltestdata`, `ada-extra-urltestdata`, `ada-verifydnslength` | 267, 24, 17 | Upstream bytes that are **not** vendored. All three record a pinned revision *and* a `raw_source_sha256`, so a fetch can be verified rather than trusted. Offline, these must abort with the exact fetch command — never silently derive a short block. |
| **3 — transcription** | `youarealiar`, `equivocal-urls` | 9, 12 | Irreducibly not re-derivable. These are class-C rows transcribed from paper PDFs; `equivocal-urls` has no released artifact at all. The transcription *is* the primary source, so it travels with the tracked builder as data, and `generation_command` can only name the file that carries it. |

Tier 2's `wpt-urltestdata` cannot be shortcut through the in-repo import: the
committed `wpt-url-cases.json` carries 202 failure cases, while the fixture's
group carries all **267** upstream `failure: true` entries. The 65-row difference
is the non-runnable rows the import drops, so 65 rows genuinely require the
upstream file.

The upstream WPT bytes were recovered locally during this work and hash-match the
recorded `355c9f1e…` exactly, which is what establishes that tier-2 fetch
verification is viable. The two Ada JSONs are **gone** from this machine; their
hashes are recorded, so a re-fetch can still be verified, but nothing local
proves it today.

## Status

| Group | Ported | Verifier |
| --- | --- | --- |
| `wpt-credentials-fragments` | yes | `verify-credentials-fragments.R` |
| `wpt-urltestdata` | no | — |
| `ada-extra-urltestdata` | no | — |
| `ada-verifydnslength` | no | — |
| `ip-obfuscation` | no | — |
| `youarealiar` | no | — |
| `equivocal-urls` | no | — |

`oracle-provenance.json` still carries `MISSING[RURL-vwurxmzm]` for the
un-ported groups' `generation_command`. Those sentinels are correct until the
group is ported; do not replace one with a `tools/oracle/` path before its
verifier exists and passes.

Note that `MISSING[RURL-vwurxmzm]` also covers `import_command` and
`retrieval_date` on several groups. Those are a **different** gap and are not
this directory's to close: nobody recorded when the Ada files were fetched, and
porting a builder cannot recover a date that was never written down.

## Running them

```bash
Rscript tools/oracle/verify-credentials-fragments.R             # verify
Rscript tools/oracle/verify-credentials-fragments.R --self-test # gate self-test
```

Both run in CI via the `oracle-rederivation` job in
`.github/workflows/verify.yml`, and therefore in `Rscript tools/verify.R`, which
derives its gate list from that workflow.
