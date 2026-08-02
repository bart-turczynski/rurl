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

All three recorded digests have since been re-fetched from upstream and
**reproduce byte-exact** — the WPT file at `355c9f1e…` and, on 2026-08-01, both
Ada files, which had been *gone* from this machine since before the port began
(the scratch builder resolved them through a deleted session scratchpad). That
is what establishes tier-2 fetch verification as viable rather than aspirational:
until it was run, nothing local proved the recorded Ada digests were reachable at
all.

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
artifact to borrow from. That gap was filed as **`RURL-qhwktfcw`**, which the
port could not close: pinning a `whatwg/url` commit needed network access that
was unavailable, and no revision was invented. What the port did instead was
anchor the transcription to something that *is* pinned — see below.

**The pin has since been taken directly, and the policy behind it split in two.**
The general question `RURL-qhwktfcw` raised was worth answering for the whole
record rather than this group alone: *does transcribing a normative standard
create its own pinning duty, independent of whether bytes were vendored?* It
does, and the record now says so. `conventions.section_2_3_scope` keeps its
original subject — **artifact pinning**, "did we vendor bytes?" — with
`section_2_3_applies` unchanged in meaning. Alongside it,
`conventions.normative_dependency_scope` states the second, orthogonal duty:
**source pinning**, which binds any group whose expected values are transcribed,
computed or hand-derived from a standard's text, vendored bytes or not. A group
can owe both, either, or neither. `wpt-credentials-fragments` owes both, and is
why one boolean could never have carried them: it has real artifact provenance
*and* transcribes the URL serializer.

The old policy justified its no-sentinel rule by asserting that out-of-scope
groups have "nothing absent to pin". That was simply false, and it has been
withdrawn rather than softened. `ip-obfuscation` was `section_2_3_applies =
false` and correctly so — yet something *was* absent to pin, namely the revision
of the standard its transcription reads, and because no field in the record
named that dependency, six section citations that resolve to no revision of the
spec survived review.

The duty is carried by a group-level `normative_dependencies` **array** — an
array because one derivation may read several sources pinned to different
degrees. `ip-obfuscation` reads two:

| Source | `pin_status` | State |
| --- | --- | --- |
| WHATWG URL Standard | `verified` | **Closed.** Pinned at `whatwg/url` `9dc3827f` (2026-07-06), the revision its transcribed algorithms were verified against. |
| UTS #46 IDNA mapping table | `verified` | **Closed.** Pinned at UTS #46 revision 35 (2025-09-04) with the mapping table at Unicode 16.0.0. Three rows turn on the treatment of U+3002/U+FF0E/U+FF61. |

Two things about that second pin are worth carrying forward.

**It names two coordinates on purpose.** UTS #46 is a *document* with its own
revision numbering; the IDNA mapping table is *data* versioned by Unicode
version, and the two run on independent cadences — when this was pinned the
document had already moved to revision 35, dated in the Unicode 17 era, while
`Public/idna/17.0.0/` still returned 404 and the newest published table was
16.0.0. Naming only "the current UTS-46 revision" or only "the current Unicode
version" would each have been a plausible-sounding pin to a *different thing*.

**It was verified before it was written.** `pin_status: verified` means the
derivation was checked against that revision — so writing it first and checking
later would be the same fabrication this whole record exists to prevent. The
check is re-derivable, not prose: `check-uts46-mapping-pin.R` sweeps every
`IdnaMappingTable.txt` published under `Public/idna` (5.2.0 → 16.0.0) for the 29
mappings the derivation transcribes — 3 full-stop variants plus 26 letters — and
reports **493 checks, 0 mismatches**. It reads unicode.org, so it is run by hand
like `pslr::psl_refresh()` and is deliberately not a gate. The pin claims those
29 mappings and nothing more; UTS-46 *Processing* is not implemented, and any
other non-ASCII code point aborts the derivation instead of being guessed.

**And it fixes bytes, not just a name.** A revision number alone does not pin
content. Unicode's versioned directories are *intended* to be immutable, but
intent is not a check: without a digest, an in-place reissue upstream would
leave the sweep re-deriving happily against changed content and reporting PASS —
the same silent-supersession hazard this whole duty exists to detect, moved one
level up. So `pinned_table_sha256` records the digest of the exact file the
mappings were checked against, the checker verifies it *before* parsing a single
mapping, and it carries digests for the other sixteen tables too, which makes
the historical-stability evidence tamper-evident as well as the pin.

Read that digest as **a verification record, not vendored provenance.** No
upstream bytes live in this repository; the group stays `section_2_3_applies =
false`, and PV3 recomputes digests for the record's own in-repo fixtures only.

Three details of that shape are load-bearing. `pin_status` is an **enum**
(`verified` / `missing` / `not-applicable`), never the `MISSING[…]` sentinel,
because gate rule PV6 fails any `section_2_3_applies = false` group whose
subtree contains that sentinel — writing one here would report a section-2.3 gap
this group does not have.

It is also a **status axis only**. Why the duty does not apply is a separate
question, carried by a `not_applicable_reason` that PV9 requires on exactly the
`not-applicable` entries and forbids elsewhere. That split (RURL-ynirvjxb)
corrected a real error: the member used to be glossed "the source is cited but
nothing is derived from it", which was false for two of the eight entries
carrying it — RFC 3986's 25 rows and PRD §6.1's rows *are* hand-derived from
their source's text, and what is inapplicable there is the **duty**, not the
derivation. The three measured reasons:

| Reason | Derives from the source's text? | Why no duty |
| --- | --- | --- |
| `no-derivation` | No | Values are read out of vendored, hash-pinned bytes, or the authority is another document (a transcribed paper). |
| `frozen-source` | **Yes** | A published, numbered document cannot be amended in place, so the drift the duty detects cannot arise. |
| `internal-source` | **Yes** | The source is in this repository, git-dated, and changes in the same pull request as the fixture it would invalidate. |

A separate key rather than a fourth `pin_status` member, because the status axis
is closed while the reason axis is open — it went from one recognised reason to
three inside a single 13-entry record.

And citations are **anchor-first**: the stable `<dfn>`
fragment id is the durable key and the section number is only a hint. That is
measured, not preferred — host parsing was section 3.2 in 2016, 3.4 in 2019 and
2022, and 3.5 today, while `#concept-host-parser` resolves in all of them.

### Anchoring the transcription against a second witness

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
would have made `RURL-qhwktfcw` look closed before it was. The anchor
establishes strong agreement with **a hash-verified WPT corpus revision**. It
does *not* establish that corpus as a proxy for the WHATWG spec revision the
transcription purports to implement — WPT is an independent compatibility suite
with its own release cadence, not a snapshot of the standard's text. So this is
behavioral evidence from a second, datable witness; **it is not a spec pin**,
and it does not date the sections transcribed. That distinction survives the
pin: `normative_dependencies[0]` is what dates the WHATWG algorithms, the anchor
is independent evidence that the transcription of them behaves, and neither
substitutes for the other.

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

## Tier 2: a re-location gate is not a re-derivation gate either

Tier 1 re-derives an expected value from the standard. Tier 3 can only show a
transcription is intact. Tier 2 sits between them, and the honest word is
**re-location**: the expected values were *read out of* upstream bytes, so what
is checkable is that each committed row still corresponds to an upstream entry
at the pinned revision and still records that entry's own verdict. Nothing is
re-derived, and the gates print `ORACLE RE-LOCATION` for the same reason the
tier-3 gates print `TRANSCRIPTION INTEGRITY`.

What makes tier 2 possible at all is that the record pins **both** a revision
and a `raw_source_sha256`, so the bytes can be re-fetched and *verified* rather
than trusted. All three digests were re-checked from upstream on 2026-08-01 and
reproduce byte-exact — including the two Ada files that had been gone from this
machine since the port began.

`fetch-source.R` is the shared resolver. Three properties are load-bearing:

- **It reads the pins out of `oracle-provenance.json` at run time** rather than
  copying them into code. A second copy of a pin is what drifts, and this way a
  pin naming bytes that do not exist fails loudly at the fetch instead of years
  later.
- **The digest is verified on every path**, cache included, before a byte is
  parsed. A half-written cache entry is exactly as dangerous as a corrupted
  download, and the cache is the path that gets reused.
- **Unresolvable is exit 2, never a pass.** Status 1 means the committed oracle
  disagrees with upstream; status 2 means the source could not be resolved.
  Collapsing them would make an outage read as a defect — and a partial pass
  over a short block is the failure this whole directory exists to prevent.

### They are not blocking gates, but they are no longer manual either

`.github/workflows/oracle-upstream.yml` (`RURL-drkcvzex`). Keeping the fetching
verifiers out of the merge gate was right; leaving them with **no automated path
at all** was not. `workflow_dispatch` on `verify.yml` does not help — it widens
the `if:` conditions on the tier-2 `--self-test` steps, and the full verifiers
are not steps there in the first place — so the fixture-to-upstream comparison
ran only when a person remembered to type the command.

The new workflow runs all three, weekly and on demand, and it distinguishes the
two outcomes the verifiers already distinguish:

| Exit | Meaning | Job |
| --- | --- | --- |
| 1 | the fixture disagrees with upstream | **fails** |
| 2 | the source could not be resolved | reported as `SOURCE UNAVAILABLE`, never as a pass |

An upstream outage is not a defect in this repository, so exit 2 does not fail
the job — but it is never silently absorbed either: each one emits a warning
annotation, and a run where all three are unresolvable states in its summary that
it **produced no evidence**. `workflow_dispatch` takes a `fail_on_unavailable`
input for when the question actually being asked is whether the sources can still
be reached.

It is a separate workflow rather than a job in `verify.yml` so that it is outside
the merge gate **by construction** rather than by an `if:` somebody can widen —
and so `tools/verify.R`, which derives its blocking gate list from `verify.yml`
alone, cannot pick up a network-reading check for the local pre-push hook. All
four paths were exercised before the file was trusted: unavailable-and-tolerated,
unavailable-and-requested-to-fail, clean, and one group drifted.

### These are not blocking CI gates

They read the network, so they follow `check-uts46-mapping-pin.R`'s posture: run
by hand, and by CI only as `--self-test`, which is fully offline. That is a
deliberate trade — an upstream outage is not a defect in this repository, and a
gate that goes red for one stops being read. If a future pass wants them
blocking, the cache directory (`RURL_ORACLE_SOURCE_DIR`, default
`_scratch/oracle-sources/`) is the seam that would make it possible without
vendoring.

### The NUL shim, whose failure mode is a silent pass

Three of the 267 `wpt-urltestdata` inputs contain U+0000. An R character vector
cannot hold a NUL, and `jsonlite` does not error on one — it **truncates**:

```r
jsonlite::fromJSON('"sc://a\\u0000b/"')   # => "sc://a"
```

So the obvious re-location check compares `"sc://a"` against `"sc://a"`, agrees,
and reports a pass it did not earn: after truncation any two inputs sharing a
prefix before their NUL are equal, and everything the three rows exist to record
is never compared. The `\u0000` **escape** is therefore rewritten in the JSON
*source text* to `\ue000` — U+E000, a Private Use Area code point R holds happily —
identically on both sides, before either is parsed. It is a transport encoding
for the comparison only; nothing is written back, and both preconditions (no
U+E000 already present, no escaped `\\u0000` upstream) are asserted rather than
assumed.

### When the pinned revision does not reproduce the block

`ada-extra-urltestdata` is the group where tier 2's central assumption breaks,
and it breaks quietly. Upstream commit `fbea5b01` (2026-07-17, ada #1186)
**re-expected** the `..#` case from href `a:b/#` to `failure` and added three new
entries. The record's pin is a `verified-at` pin taken two days later, so at the
pin all 24 inputs re-locate — and one expected value does not. The old note said
"the revision at which all 24 rows were re-located", which is true of *inputs*
and reads as true of *expectations*.

Both deltas are carried as **exact ledgers**, never tolerances:

| Ledger | Rows | What it fails on |
| --- | --- | --- |
| drift | 1 (`ada-024`) | a new disagreement, a ledger row that stops disagreeing, or one that disagrees *differently* |
| upstream-only | 4 | an upstream entry nobody triaged, or a ledger row that is no longer outside the block |

"A ledger row that stops disagreeing is a failure" is the load-bearing half. It
means you cannot close the gap by adopting upstream's current value: falsified,
and rewriting `ada-024` to `failure` fails the gate.

**The second anchor is what makes a ledger honest.** A ledger alone says "we
know about that one" with no evidence the recorded value was ever right. So the
gate also grades the block against the revision at which it *does* reproduce —
and that revision was not guessed. Sweeping all 17 commits that ever touched the
path finds **exactly one** revision reproducing 24/24, `aa8e4043` (2025-07-16);
every earlier revision reproduces fewer and every later one 23/24.

**What that sweep bounds is the upstream CONTENT STATE, and nothing else**
(corrected in `RURL-drkcvzex`; it was written as bounding the *import*). The
measurement is that the committed expectations agree with the bytes that existed
at `aa8e4043` and stopped existing at `fbea5b01` — so the content state the block
agrees with is `[2025-07-16, 2026-07-17)`, recorded as `content_state_bound` and
re-derived on every run rather than asserted.

It does **not** resolve the revision the rows were imported from, and it does not
date the fetch:

- no commit touched the path inside that window, so *every* revision in it has
  the same bytes — the sweep cannot single one out even in principle;
- content agreement is not import provenance. A block produced later from an
  older checkout, an older copy of the file, or by hand would agree just as well.

So `retrieval_date` keeps its sentinel because it is genuinely unrecorded, not
merely because "a bound is not a date". `RURL-vwurxmzm`'s conclusion that the
import revision cannot be resolved stands; what this adds is a bound on the
content, which is a weaker and different claim.

The anchor is deliberately *not* proposed as the group's `upstream_revision`.
Re-pinning to the revision a fixture happens to agree with would make the pin
follow the fixture, which is the direction this record exists to prevent.

### Two things falsification found that design did not

**The classifier's scheme rule was wrong, and only the second corpus could show
it.** The first cut used RFC 3986's scheme production (`ALPHA *( ALPHA / DIGIT /
"+" / "-" / "." ) ":"`). It reproduces all 267 `wpt-urltestdata` rows and
disagrees with exactly one ada row: `schéme://example.com`, whose scheme is
invalid because of the `é`. WHATWG would indeed fall back to the base there — but
the fixture *runs* that row, because what makes a row unrunnable is needing a
base to have a meaning at all, not having a *valid* scheme. The rule is
positional, and the point generalizes: a reconstruction that fits one corpus
perfectly is not thereby right.

That is also why the helper is called `wpt_occupies_scheme_position` and not
`wpt_is_absolute` (renamed in `RURL-drkcvzex`). It is **fitted applicability
metadata** — it answers "can `rurl` be pointed at this row without a base?",
a question about this fixture's `runnable` column — and it deliberately
disagrees with the WHATWG notion of an absolute URL, saying `TRUE` for
`schéme://…` exactly where WHATWG falls back to the base. The old name
invited it to be read as a spec predicate, which it is not.

And its agreement with the corpus is the **fit, not validation**. Reproducing all
291 committed classifications could hardly come out otherwise: those 291 rows are
the data the rule was fitted to, and `ada-017` is the standing proof that a
perfect fit on one corpus is compatible with a wrong rule. What the fit buys is
entirely prospective — a row silently re-classified *later* stops agreeing with
an executable rule. Anything stronger would have to come from the builder that
originally computed the column, and that rule was never preserved.

**A digest-only cache key leaves the revision unverified.** The resolver was
first content-addressed on the digest alone, which is sound for the bytes and
silently unsound for the pin: re-point `upstream_revision` at a revision serving
different content, leave the digest, and a warm cache hits and never fetches — so
the gate grades the right bytes while the revision it reports is a claim nothing
checked. Measured: with a digest-only key, re-pointing the second anchor at the
revision that *broke* the block left the gate green. The key is now
`(digest, revision)`, and that mutation misses the cache, fetches, and fails.

### The group that turned out not to be tier 2 at all

`ada-verifydnslength` is filed as an imported suite, and its record said so:
`pin_status: not-applicable`, `not_applicable_reason: no-derivation`, and a note
asserting that "every expected value here is READ OUT of vendored, hash-pinned
bytes". **That was false for 10 of its 17 rows.** Upstream marks ten entries
`failure: true`; the fixture records `accept` for *all seventeen*. Those ten
verdicts are not copied from anywhere — they are hand-derived from the URL
Standard's text, which is exactly what `normative_dependency_scope` says creates
a source-pinning duty.

The derivation is two steps long, at the pinned revision:

> host parser step 6 — run the **domain parser** with `domain` and **false**
> domain parser ToASCII — *VerifyDnsLength* is set to `beStrict`

So with `beStrict = false` there is no DNS length verification at all, and Ada's
`verify_dns_length` is an *optional* RFC 1035 §2.3.4 check the standard does not
perform. That is the whole reason all 17 accept.

Two things about how this was missed are worth carrying:

- **The record stated the derivation and then denied it.** The old note's own
  prose read "the rows are recorded against the WHATWG default
  (VerifyDnsLength = false), which is why all 17 carry
  standard_expectation = accept" — a derivation, described in the sentence that
  classified the group as deriving nothing. The contradiction was legible on the
  page.
- **A wrong answer passes both gate rules.** PV10 makes the source-pinning
  question mandatory; PV9 makes the answer well-formed. This group answered, in
  the required shape, incorrectly. Silence was never the only failure mode — and
  what caught it was not a gate but pointing an executable derivation at the
  group and seeing what its expectations actually depend on.
- **And then the repair itself misfiled the corrected object** (`RURL-drkcvzex`).
  The verified `9dc3827f…` pin — whose whole subject is these 17 rows and the
  `beStrict = false` reading — was inserted under **`ada-extra-urltestdata`**,
  while this group kept the false `no-derivation` entry. The record then
  contradicted its own `standard_version`, this README, the NEWS entry and the
  verifier, and PV1–PV10 stayed green for the whole commit range: a *well-formed
  answer to another group's question* satisfies a shape rule and a presence rule
  alike. So the record now says which group each dependency object describes,
  and `PV11` checks it. See "A dependency object has to be attached to the group
  it describes" below.

**Both readings are derived, which is what makes the disagreement an
explanation.** The gate computes the WHATWG reading (producing the committed
`accept`) *and* the RFC 1035 §2.3.4 reading, and requires the second to equal
upstream's own `failure: true` verdict on every row. A hand-written "these ten
differ" ledger would record *that* they differ; deriving both shows the
difference is that axis and nothing else, so if upstream ever rejects one of
these hosts for another reason the disagreement stops being explained and the
gate fails. There is a floor too: if *no* row disagrees, the gate fails rather
than passing vacuously, because that is the state in which the group would no
longer owe the pin it now carries.

**No second transcription.** `derive-ip-obfuscation.R` already transcribes
`#concept-host-parser` at the same revision and is falsified nine ways, so this
group sources it. That reuse immediately found a latent defect in it: the IPv4
number parser applied its double-precision guard **before** the digit-validity
check, so a 63-character *non-numeric* label aborted instead of failing cleanly.
The spec's order is validity first. It was fail-closed, so never a wrong answer
— but it was a refusal to answer a question the spec answers, and it would have
blocked exactly this reuse. Worth generalizing: pointing a second corpus at a
transcription is a cheaper way to find its unmodeled edges than adding cases to
the first.

### What this corpus cannot see, measured rather than assumed

Two mutations of the shared `runnable` classifier — dropping the `about:blank`
rule, and narrowing scheme detection to `://` forms — leave
`verify-wpt-urltestdata.R` **green**. That is not a gap in the gate; it is a
property of the corpus: no `wpt-urltestdata` entry carries an `about:blank`
base, so neither rule is exercised. They are covered by
`verify-ada-extra-urltestdata.R`, whose 24 rows are almost entirely
`about:blank`-based, and by `--self-test`. Recorded because a falsification run
that only reports its successes is how a blind spot survives.

### An implementation-conformance check is not an oracle check

`tools/oracle/check-fsss-conformance.R`, and the file boundary is the point
(`RURL-drkcvzex`). Everything else under `tools/oracle/` answers *where an
expected value came from* — pinned upstream bytes, or pinned normative text,
never anything `rurl` produced. The `fsss_whatwg` == `oracle_value` comparison
answers a different question: does the implementation's **captured** output agree
with that expected value. It was living inside `wpt_check_restatement()`, so a
tier-2 gate printed `ORACLE RE-LOCATION: PASS` over a set of checks one of which
was grading `rurl`.

That was not circular — `oracle_value` is independent of `rurl`, so the
comparison has a real subject — but a conformance assertion filed under an
oracle's label is how the next reader comes to believe the oracle was checked
against the implementation. Both tier-2 gates now print two verdicts, and a
failure is attributed to the arm that owns it:

```
ORACLE RE-LOCATION: PASS
fsss conformance: 13 row(s) graded, 2 of them carrying a documented rurl_deviation
IMPLEMENTATION FSSS CONFORMANCE: PASS
```

The comparison stays **unconditional** on `rurl_deviation`, for the reason
recorded in that file: `ada-003` (ADR 0011, `path_encoding`) and `ada-006`
(ADR 0002, Punycode) deviate on `clean_url`, the *presentation* surface, while
`fsss_whatwg` is the conformance serialization and both rows carry
`fsss_conforms = yes`. Conditioning would switch the check off on exactly the
rows where a presentation surface deviates and the conformance surface still has
to agree. Those two ids are **named** in `FSSS_REQUIRED_DEVIATION_IDS`, not
merely counted: a count alone would let them drop out and be replaced by two
non-deviating rows, silently restoring the conditioning the check refuses.

`wpt-urltestdata` passes a floor of **zero** and says "0 row(s) graded" out loud.
Every row there is an upstream failure expectation, so no row carries a
serialization — the check grades nothing by construction. A caller that simply
omitted it would be silent about that.

### `whatwg_expected` was ungraded for one good reason and one wrong one

It was left out of the tier-2 gates on two stated grounds. The good one stands:
its **NA pattern** is exactly `divergence_class` in (`aligned`, `not-runnable`),
a column derived from how `rurl` answers, so an oracle module that derived the
pattern would be consulting the implementation it grades. The check added by
`RURL-drkcvzex` does not touch the pattern — absence is not graded at all.

The wrong one extended that to the **value**. A non-NA `whatwg_expected` is the
WHATWG expectation for the row, and on every tier-2 row the pinned upstream bytes
state it, independently of `rurl`. Falsified: corrupting it on `ada-003` — a row
carrying a `rurl_deviation` — left every gate green, because
`test-external-url-vectors.R` relates the column to `divergence_class` and
`rfc3986_expected` rather than to any upstream fact, and a deviating row can
satisfy those with a wrong value.

So it now has an owner, deriving from pinned bytes only:

| Group | Rows graded | Of those, deviating | Derived from |
| --- | --- | --- | --- |
| `wpt-urltestdata` | 202 | 21 | upstream's own `failure: true` at the pin |
| `ada-extra-urltestdata` | 14 | 3 | the pin, or the **second anchor** on a drift-ledgered row |
| `ada-verifydnslength` | 0 | 0 | nothing to grade — all 17 are `aligned`, so the column is NA |

Both floors are "at least", so the corpus may grow but may not drain: dropping
`ada-003`'s value fails on the row count *and* on the deviating-row count. And a
graded row whose expectation cannot be derived at all is a **failure**, never a
skip — "we could not check it" must not report as "it agrees".

## A dependency object has to be attached to the group it describes

`PV11` in `tools/oracle-provenance-gate.R`. It exists because of a measured
defect, not a hypothesis: the `ada-verifydnslength` repair above landed its
corrected, verified pin under `ada-extra-urltestdata` and left the false
negative entry where it was, and **every earlier rule stayed green**. PV9 asks
whether an answer is well-formed and PV10 asks whether an answer exists — a
well-formed answer to somebody else's question satisfies both.

Three things a rule can check, and PV11 checks all three independently, so each
failure says something different:

| Check | What it reads | What it catches |
| --- | --- | --- |
| ownership | `applies_to_fixture` + `applies_to_group` on every entry | an object moved under the wrong group |
| revision agreement | a 40-hex sha named in the group's `standard_version` | a group claiming a direct pin its own array does not carry |
| declaration kind | `NEGATIVE declaration` / `POSITIVE declaration` in `normative_dependencies_note` | a note that contradicts its entries' `pin_status` |

**The declaration is a fixture-plus-group pair, not a bare name.** Two groups in
this record are both called `wpt-urltestdata` — one in
`inst/bench/wpt-url-cases.json`, one in `external-url-vectors.csv` — so a bare
name could not distinguish a swap between them from a correct record.

**Revision agreement fires on the real defect from the other side.**
`ada-verifydnslength`'s `standard_version` read "pinned **DIRECTLY** at
`whatwg/url` `9dc3827f…` by `normative_dependencies[0]`" above an entry whose
`revision_scheme` was `unpinned`. The converse is deliberately *not* enforced: a
verified git-commit pin under a `standard_version` that dates the standard by
proxy through an artifact revision is correct — that is
`wpt-credentials-fragments`, and its entry explains why.

**Declaration kind makes existing prose falsifiable.** The record already wrote
"the array is a NEGATIVE declaration" by convention; requiring the phrase and
checking it against the entries' `pin_status` turns it into a claim. The
misfiled object sat under exactly that sentence, above a verified pin, for a
whole commit range.

**Falsified against the real record, not only synthetically.** The gate's
`--self-test` reads the committed record, swaps the two Ada groups'
`normative_dependencies` arrays — the actual defect — and asserts PV11 goes red
while PV9 and PV10 stay green. Nine synthetic cases cover the rest: a missing
declaration, a blank one, a wrong group, a wrong *fixture*, a named sha nothing
pins, a named sha pinned only by **another** group, a note declaring neither
kind, a note declaring both, and a note contradicting its array in each
direction.

**The honest limit, stated rather than implied.** PV11 catches a *move* — the
object travels and its declaration does not. An author who edits the
declaration too is rewriting the claim, not misfiling it, and no structural rule
can referee that. Nor does PV11 decide whether a group *owes* a pin; it checks
that the record agrees with itself.

## Status

| Group | Ported | Verifier |
| --- | --- | --- |
| `wpt-credentials-fragments` | yes | `verify-credentials-fragments.R` |
| `ip-obfuscation` | yes | `verify-ip-obfuscation.R` |
| `youarealiar` | yes | `verify-youarealiar.R` (integrity, not re-derivation) |
| `equivocal-urls` | yes | `verify-equivocal-urls.R` (integrity, not re-derivation) |
| `wpt-urltestdata` | yes | `verify-wpt-urltestdata.R` (re-location, not re-derivation) |
| `ada-extra-urltestdata` | yes | `verify-ada-extra-urltestdata.R` (re-location + two exact ledgers) |
| `ada-verifydnslength` | yes | `verify-ada-verifydnslength.R` (re-location **and** re-derivation) |

**All seven groups are ported.** Every `generation_command` now names a tracked,
executable path, and no `MISSING[RURL-vwurxmzm]` sentinel remains on that field
anywhere in the record.

`import_command` is a different gap, and it is **still open** — deliberately.
The tier-2 slices established that the pinned bytes can be re-fetched and
hash-verified, and for one commit range that reproducing command was written into
`import_command` with a note saying it was not attested as the command originally
run. Honest prose in a machine-readable field that then read as *filled*: the
record counted one fewer sentinel while the historical provenance was exactly as
unrecorded as before.

So the two claims are two keys (`RURL-drkcvzex`):

| Key | Claim | Value |
| --- | --- | --- |
| `import_command` | the command originally run | `MISSING[RURL-vwurxmzm]` — unattested, unrecoverable |
| `pin_fetch_command` | a command that re-fetches the revision and reproduces `raw_source_sha256` | the fetch invocation, verified 2026-08-01 |

`PV5` refuses a `pin_fetch_command` beside an `import_command` holding a real
value, and requires a `pin_fetch_command_note` saying what the command proves and
what it does not. It proves the recorded digest is reachable at the recorded
revision and path, so the pin is checkable by anyone rather than resting on one
machine's history. It does not prove these bytes entered this repository that way,
or when.

`retrieval_date` cannot be recovered either and keeps its own sentinel for the
same reason: a reproducing command dates nothing. For `ada-extra-urltestdata` the
upstream **content state** the block agrees with is bounded — see the commit
sweep above — but that bounds the bytes, not the fetch, and not the revision the
rows were imported from.

## Running them

```bash
# Tier 1 and 3 -- offline, and wired into CI as blocking gates.
Rscript tools/oracle/verify-credentials-fragments.R             # verify
Rscript tools/oracle/verify-credentials-fragments.R --self-test # gate self-test
Rscript tools/oracle/verify-ip-obfuscation.R                    # verify
Rscript tools/oracle/verify-ip-obfuscation.R --self-test        # gate self-test
Rscript tools/oracle/verify-youarealiar.R
Rscript tools/oracle/verify-equivocal-urls.R

# Tier 2 -- these FETCH. CI runs only their --self-test; run the full form by
# hand. Exit 2 means the source could not be resolved, and is never a pass.
Rscript tools/oracle/verify-wpt-urltestdata.R
Rscript tools/oracle/verify-ada-extra-urltestdata.R
Rscript tools/oracle/verify-ada-verifydnslength.R

# Fetched sources are cached, so a second run is offline:
#   RURL_ORACLE_SOURCE_DIR   where to cache (default _scratch/oracle-sources/)
#   RURL_ORACLE_OFFLINE=1    never fetch; fail if the cache has no entry
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
