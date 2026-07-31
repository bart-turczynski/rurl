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

## What a port discharges is not the same claim in every group

The ticket frames the gap as "`generation_command` names something no clean
checkout can run", which is exactly right for six of the seven groups.
`ip-obfuscation` is the exception, and it matters because it inverts the
priority.

That group records **no `generation_command` at all**. It carries
`section_2_3_applies = false`: nothing was imported, so there is no upstream
project, revision, path, license, raw hash or import command to pin. What stands
in for all of that is one prose sentence —

> each row is a decimal/octal/hex re-encoding of an IPv4 literal, **derivable
> from arithmetic alone**

— plus a `relocation_note` saying the same thing. So this is the group where
re-derivability is not a convenience that supplements a provenance chain; it
*is* the provenance chain, and it was the only claim in the record with nothing
behind it. A group with no upstream artifact is the worst place to leave a
re-derivability claim untested, not the safest.

Two shape consequences follow, and they generalize to the two tier-3
transcription groups:

- **The inputs are the irreducible data and travel with the script.** They
  cannot be re-fetched from anywhere — deliberately, since neither technique
  reference ships a LICENSE, so no bytes were vendored. The roster in
  `derive-ip-obfuscation.R` is therefore the only copy besides the fixture, and
  the verifier checks set equality in **both** directions: a row added to the
  fixture without being added to the roster is as much a provenance break as a
  deletion.
- **Declared intent is a third, independent statement.** Re-derivation alone
  cannot catch a mistyped encoding: fat-finger `0177` to `0176` and it derives
  cleanly to 126.0.0.1, the fixture can be updated to match, and the row simply
  stops demonstrating the obfuscation it claims to. So the roster also declares,
  per row, the 32-bit address the encoding is *meant* to denote, written in a
  notation the input does not use. `ipobf-005` proves that check is not vacuous:
  `0x7f.256` looks like a 127.0.0.1 encoding and is not one, so it declares
  2130706688 (127.0.1.0).

### Fail closed means aborting, not returning "failure"

Four of the 24 rows expect `failure`. That makes "failure" a dangerous default
return value: a derivation that answered `failure` for a construction it does
not model would agree with those four rows for entirely the wrong reason. So
every unmodeled construction in `derive-ip-obfuscation.R` **aborts** — non-ASCII
outside the three UTS-46 full-stop variants, a percent sign in a domain (host
parser step 4's percent-decode is not transcribed), a numeric part too long to
hold exactly in a double, and any input that is not of the form
`http://<host>/`. Only the spec's own reject paths return failure.

### One column, one oracle

The verifier grades the six columns that restate the group's WHATWG expectation
(`standard_expectation`, `whatwg_expected`, `oracle_kind`, `oracle_value`,
`fsss_whatwg`, `fsss_host`) plus `notes` and `source_reference`. It deliberately
does **not** grade `rfc3986_expected`: that is a different oracle and it already
has one — the `oracle-vs-grammar` test in `test-external-url-vectors.R` checks it
against a transcription of RFC 3986's own ABNF (`tools/oracle-audit-rfc3986.R`).
Two gates deriving one column is how they drift apart.

## Status

| Group | Ported | Verifier |
| --- | --- | --- |
| `wpt-credentials-fragments` | yes | `verify-credentials-fragments.R` |
| `ip-obfuscation` | yes | `verify-ip-obfuscation.R` |
| `wpt-urltestdata` | no | — |
| `ada-extra-urltestdata` | no | — |
| `ada-verifydnslength` | no | — |
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
Rscript tools/oracle/verify-ip-obfuscation.R                    # verify
Rscript tools/oracle/verify-ip-obfuscation.R --self-test        # gate self-test
```

A gate only ever observed to pass is not evidence, so each was falsified before
being trusted — both halves, in both directions. For `ip-obfuscation`: four
mutations of the committed fixture (a corrupted expectation, a deleted row, a
flipped `oracle_kind`, a re-paired `notes` cell) each exit 1 with an attributed
diff, and five mutations of the *derivation* (a naive split that drops the
trailing empty field, `>` for `>=` in IPv4 parser step 7, compressing a lone
zero piece, dropping the uppercase `0X` prefix, and letting unmodeled non-ASCII
pass through instead of aborting) are each caught by `--self-test`.

Worth recording from that exercise: the naive-split break is **not** caught by
`127.0.0.1.`, which parses correctly by accident under the wrong split. Only
`127.0.0.1..` distinguishes them. A single trailing-dot case would have scored a
truthful pass over a broken transcription.

Both run in CI via the `oracle-rederivation` job in
`.github/workflows/verify.yml`, and therefore in `Rscript tools/verify.R`, which
derives its gate list from that workflow.
