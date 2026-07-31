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

**But read `section_2_3_applies = false` narrowly.** It is a true statement about
the *rows* — no bytes were vendored — and it is *not* a statement that the group
has no upstream normative source. WHATWG's URL Standard is one, and since this
port the group's expectations are **computed by a transcription of it** rather
than hand-written, which makes the spec a load-bearing dependency the record
must date. "Derivable from arithmetic alone" is weaker than a reproducible
standards reference, because the URL Standard is a *Living* Standard: amend the
IPv4 parser upstream and the transcription silently becomes a reading of a
superseded revision.

Every other WHATWG group here dates its standard reference by proxy —
`"Living Standard (unversioned); pinned by upstream_revision"`, borrowing the
date from its WPT or Ada artifact. `ip-obfuscation` and `equivocal-urls` have no
artifact to borrow from. That gap is filed as **`RURL-qhwktfcw`** and is *not*
closed by this port; pinning a `whatwg/url` commit needs network access that was
unavailable, and no revision was invented. What the port does instead is anchor
the transcription to something that *is* pinned — see below.

The general question `RURL-qhwktfcw` raises is worth answering for the whole
record, not just this group: **does transcribing a normative standard create its
own pinning duty, independent of whether bytes were vendored?** The policy in
`conventions.section_2_3_scope` is framed entirely around vendored artifacts and
does not currently speak to it — and its claim that out-of-scope groups have
"nothing absent to pin" stops being true the moment a transcription becomes
load-bearing.

### Anchoring a transcription without a spec pin

`verify-ip-obfuscation.R`'s check D grades the transcription against a corpus
that carries both a revision and a hash: `inst/bench/wpt-url-cases.json` at WPT
`181476aa16e8`, `raw_source_sha256 355c9f1e5f34`. Two halves, each with a floor
so the check cannot silently erode to zero rows:

- **Idempotence** — every recorded `hostname` is an already-serialized,
  spec-conformant host, so re-parsing must return it unchanged. **152/152**, none
  outside the modeled subset.
- **Input → host** — for inputs whose authority is extractable without
  transcribing the URL parser, the derived host must equal the recorded one.
  **90/90**, 62 skipped.

Be precise about what that buys, because the tempting overstatement is what
would make `RURL-qhwktfcw` look closed when it is not. The anchor establishes
strong agreement with **a hash-verified WPT corpus revision**. It does *not*
establish that corpus as a proxy for the WHATWG spec revision the transcription
purports to implement — WPT is an independent compatibility suite with its own
release cadence, not a snapshot of the standard's text. So this is behavioral
evidence from a second, datable witness; it is not a spec pin, and it does not
date the sections transcribed. That gap is exactly why `RURL-qhwktfcw` stays
open.

Two URL-parser steps are
applied during extraction, because omitting them would misattribute a parser
rule to the host parser: ASCII tab/newline are removed from the input, and for a
special scheme `\` terminates the authority exactly as `/` does. Both were found
by measurement — they were the only four disagreements in the first run.

**The anchor is not redundant with the 24 rows.** Measured: mutating
"ends in a number" to treat un-prefixed hex digits as a number leaves checks
A–C *and* the whole hand-written case list green, and is caught **only** by the
anchor — it wrongly rejects the pinned hostnames `b`, `c` and `ab`. A 24-row
corpus about IPv4 obfuscation cannot notice a parser that breaks ordinary
domains.

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

## Tier 3: an integrity gate is not a re-derivation gate

The tier-1 and tier-2 groups have `derive-*.R` modules because their expected
values are computable. Tier 3's are computable from **nothing** — they are
transcribed from a paper's reference-implementation column, so the transcription
*is* the primary source. The module is therefore named `transcribe-*.R`, and the
gate reports `TRANSCRIPTION INTEGRITY`, never `ORACLE RE-DERIVATION`.

That naming is load-bearing. **No gate here can tell you the transcription is
correct** — there is no algorithm to re-run. It can only establish that the
committed rows *are* the transcription that was recorded. A gate labelled
"re-derivation" would invite a reader to assume an independent check happened,
which is the more dangerous error, so `verify-youarealiar.R` prints the
limitation in its own output rather than leaving it to this file.

What is genuinely checkable:

- **Primary-source integrity** — `input`, `standard_expectation`,
  `paper_claimed_behavior`, `source_reference`, against a roster whose source of
  truth is the original **builder**, not the fixture. Sourcing it from the
  fixture would make the check a tautology.
- **Byte exactness** — three of the nine rows exist *because* the paper's PDF
  escaping is ambiguous and a human resolved it: `yal-002` is a literal TAB and
  not a backslash (the paper displays `\t`), `yal-003` carries three CR LF
  pairs, and `yal-001/004/007` carry **single** backslashes. Declared as ordered
  code points plus a backslash count. This is the group's most perishable fact:
  `yal-003`'s CR bytes provably do not survive a CSV round trip, so `input_json`
  is the only faithful carrier and nothing was checking it stayed faithful.
- **Restatement fidelity** — `oracle_kind`/`oracle_value`/`fsss_host` are a
  later machine-readable restatement of the transcribed prose, and a restatement
  can drift from what it restates.
- **Citation integrity** — asserted against the *declared* section, not merely
  pattern-matched, so a row citing the **wrong** section fails too.

### The finding: a class-C row has two kinds of cell, and freezing both is wrong

Discovered by measurement, not foresight. Freezing `notes` against the builder
produced five disagreements — and in **every one the fixture was the more
current text**, because `rurl`'s behavior had legitimately moved: `rfc3986` now
rejects backslashes so `yal-001/007` no longer reproduce the paper's RFC column
(`RURL-qrfrvmkg`), `yal-006` was reclassified out of `aligned`
(`RURL-xfbzkico`/`RURL-kmkyicpt`), `yal-008`'s closed scheme set gained `file`,
and `yal-009` moved from "needs-investigation" to boundary-by-design once
`scheme_policy` existed.

So a class-C row mixes **immutable primary-source data** with **living
commentary about `rurl`**, and the two must be treated differently.
`input`/`standard_expectation`/`paper_claimed_behavior`/`source_reference`
matched the builder exactly across all nine rows — those are the paper's.
`notes` is ours, and it is *supposed* to change when behavior changes. Freezing
it would convert a correct update into a gate failure and pressure the next
author into reverting a true statement. What is stable inside `notes` is the
paper citation, so that is what is asserted. **This applies directly to
`equivocal-urls`**, the remaining tier-3 group.

The falsification run covers the boundary explicitly: rewriting a `notes` cell
while keeping its citation must **pass**, and does.

### A claim that is not re-runnable, and not because we are offline

`youarealiar`'s `source_reference` asserts "bytes verified vs
wspr-ncsu/urlparsing-framework", and the record pins revision `1577b534…` for
that cross-check. It cannot be re-run — but the reason is a **recording** gap,
not connectivity: `upstream_path`, `raw_source_sha256` and `import_command` are
all `MISSING[RURL-vwurxmzm]`, so nobody wrote down *which file* in that
repository the bytes were checked against. An offline skip would misreport that
as a network problem.

The gate therefore **reports** this every run rather than failing — a
permanently-red gate stops being read — but it is not silent, and it fails in
one direction: if those sentinels are ever filled in, the claim becomes
re-runnable and the gate demands enforcement with the exact fetch command
instead of continuing to describe it as un-runnable.

### `equivocal-urls`: the octet-notation exception

The second tier-3 group needs one check the other four do not, and it is the
group where tracking matters most and re-derivation helps least. Reynolds et al.
released **no artifact** — the 98,425-case fuzzing corpus was never published and
the paper carries no repository link across its 60 references — so unlike
`youarealiar` there is not even a third-party repo to byte-check against. Until
this port the 12 rows existed in exactly two places: the committed fixture, and
one gitignored builder on one machine.

Two Table-3 rows cannot be represented as R character strings at all:

- `eq-U1` — `n.pr[0x00]@e.gg`, an embedded NUL
- `eq-U7` — `n.pr[0xDD9ADCBD]e.gg`, octets that are not valid UTF-8

Both are `runnable = no` with `input = NA`, and their `input_json` holds **the
paper's `[0xNN]` notation** rather than a JSON-encoded copy of the bytes. That is
a deliberate exception to the fixture's own convention that `input_json` is the
byte-exact source of truth — and nothing was checking that the exception stayed
an exception. A later pass that "normalized" those cells into ordinary escaped
strings would convert a faithful record of *un-representable* octets into a false
claim about representable ones, and would make two rows look runnable that cannot
be run.

Check E asserts it in **both** directions: exactly those two rows are
notation-only and keep `input = NA` with `[0x..]` intact, and **no runnable row
carries octet notation** — the latter being what happens if someone records a new
un-representable input by copying the notation without also marking the row
non-runnable.

Also deliberately absent: the paper's VirusTotal example
`http://letsencrypt.org%2Fdocs%2F[redacted]/LS.exe`. `[redacted]` is the authors'
own redaction of the live host, so there is no faithful string to transcribe, and
fabricating a plausible host would be the one unrecoverable error here.

### `fsss_host` must be checked conditionally, or it re-opens a closed trap

Both tier-3 gates assert `fsss_host == oracle_value` **only where
`rurl_deviation` is NA**. That condition is the correct rule, not a loosening:

- `eq-U8` records `oracle_value = n.xn--prie-swc.gg` (the paper's Option A, where
  the dotted-İ folds into the host) while `rurl` reads the `@` as a userinfo
  delimiter and lands on `e.gg`. That disagreement **is** the row's point, and a
  `rurl_deviation` citation owns it.
- `yal-005` is the latent-trap case: it is a host row that *does* carry a
  deviation (ADR 0002 — the host stays reversibly Unicode and Punycode is a
  separate presentation phase) and happens to satisfy the equality anyway. An
  unconditional check passes there **by luck**, and would fail wrongly the moment
  that documented presentation phase changed.

Either way, an unconditional check would force the next author to choose between
writing a false oracle and deleting a documented deviation — which is exactly the
co-confirmation trap `RURL-nknytzxz` was filed for. The falsification run covers
both directions: an undocumented `fsss_host` difference must fail, a documented
one must pass, and **deleting the deviation must fail**.

## Status

| Group | Ported | Verifier |
| --- | --- | --- |
| `wpt-credentials-fragments` | yes | `verify-credentials-fragments.R` |
| `ip-obfuscation` | yes | `verify-ip-obfuscation.R` |
| `youarealiar` | yes | `verify-youarealiar.R` (integrity, not re-derivation) |
| `equivocal-urls` | yes | `verify-equivocal-urls.R` (integrity, not re-derivation) |
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
