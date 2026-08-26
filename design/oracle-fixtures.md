# Reading `external-url-vectors.csv`

Standing record for `tests/testthat/fixtures/external-url-vectors.csv`, the
audited conformance fixture. It is documented here rather than in a header
comment on its consuming test because the rules below govern anyone *editing* a
cell, and an editor arrives at the CSV, not at the test.

The file mixes **characterization** columns — what `rurl` currently does — with
**claim** columns — what the oracle says and whether `rurl` conforms. The two
kinds re-baseline on different triggers, and treating a claim column as
characterization is how a correct cell gets "corrected" into an error.

## The claim columns are posture-scoped

`fsss_host`, `fsss_whatwg`, `oracle_kind`, `oracle_value` and `fsss_conforms`
describe the row's own **`standard`** column, which is usually `whatwg`. They do
not describe the RFC.

So an `rfc3986`-only fix moves `rurl_rfc_status`, `rurl_rfc_clean`,
`fsss_rfc_source` and `fsss_rfc_normalized`, and **must not touch the claim
columns**: WHATWG did not move, so those cells stay correct. Editing them
introduces the error it looks like it is avoiding.

`fsss_host` is the one to watch, because the bare name reads ambiguous next to
its explicit siblings. Measured rather than inferred, it is the **WHATWG** host:
it matches the WHATWG record on **104/104** rows carrying a value, matches the
`rfc3986` host on 51 of those, and tracks WHATWG on **all** the rows where the
two postures disagree.

Before "correcting" any `fsss_*` or `oracle_*` cell, establish which posture it
describes by scoring it against both postures over the rows where they disagree
— the agreeing rows cannot discriminate. Note also that `fsss_host` is only
*asserted* for `oracle_kind == "host"` rows, so a blank elsewhere may be
inapplicable rather than missing.

## `rurl_deviation` is claim accounting

Non-`NA` rows are **skipped** by `oracle_nonconformance_ids()`. Clearing the
column for a newly conforming row therefore changes which rows are scored and
which totals the conformance figures are computed over. It is a re-baselining
decision with an owner, never a side effect of a parser fix.

The same column is what conditions the oracle equality: see
[`measurement-traps.md`](measurement-traps.md) §7.

## Mechanics

- A `read.csv(colClasses = "character", na.strings = "NA")` →
  `write.csv(row.names = FALSE, na = "NA", quote = TRUE)` round-trip is
  **byte-identical** on this file. Verify that before any targeted rewrite.
- The file's `sha256` is pinned in **two** places, both checked by
  `tools/oracle-provenance-gate.R` (PV3):
  `tests/testthat/fixtures/oracle-provenance.json` and the OR-021 row of
  `design/work/url-v3/registers/oracle-register.md`. A pin refreshed in one and
  not the other fails the gate; a pin refreshed in both without re-verifying the
  assertion is the false-freshness trap in
  [`measurement-traps.md`](measurement-traps.md) §6.
- The file is in the shared `&byte-pinned` exclude in
  `.pre-commit-config.yaml`, because a whitespace fixer touching it breaks PV3
  on the push it runs during.
