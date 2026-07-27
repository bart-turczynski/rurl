# Register: oracle-register (§6 artifact 11 — oracle classifications)

<!-- variant: oracle-classification (NEW register variant; not yet in
     schema/record-schemas.yaml). Row-based, append-only. Its schema is defined
     below and enforced at runtime by design/work/url-v3/tools/oracle-label-gate.R (the gate reads
     this register and fail-closes on any malformed row, any authority outside
     P5.3 §2's closed set, any UNLABELED row without a carrier, and any
     oracle-bearing file in the tree that this register does not classify). A
     future control-plane snapshot adds a validate-records.R section and a
     record-schemas.yaml entry for this variant and pins it present:true in the
     manifest, exactly as the §6 contract registers were sealed at
     cp-snapshot-2 / the contracts at cp-snapshot-3. Until then no validator
     globs it.

     This register discharges §7 G4 criterion 2 ("Oracles are labeled by
     authority and claim boundary"). It classifies; it decides nothing. P5.3 §2
     owns the taxonomy and P5.3 §2.3 owns the provenance field list — where an
     instance fits no authority, this register records an OPEN row and a
     carrier rather than inventing a sixth label. -->

## Envelope

| Field | Value |
|---|---|
| id | reg-oracle-register |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/registers/oracle-register.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — a classification binds a claim to an authority, which is an owner act (P0.1 authority) |
| lifecycle_state | PROPOSED |
| dependencies | P5.3 (the taxonomy and the §2.3 provenance field list); reconciliation §6 artifact 11, §7 G4 criterion 2; design/work/url-v3/tools/oracle-label-gate.R (the consuming gate); verification/cache-slice.md and verification/determinism-slice.md (which boundary this artifact out and consume its classifications) |
| completion_rule | §7 G4 criterion 2 — every oracle instance in the tree carries exactly one authority label from P5.3 §2 or an OPEN row naming a carrier; every row states what it CAN and CANNOT certify; every imported instance has a §2.3 provenance row whose every field is populated or explicitly `MISSING[carrier]`; the gate exists in the verify chain and is self-tested |
| content_hash | pinned at a future G4 control-plane snapshot; the gate hashes nothing of itself |
| approval_evidence | pending — pins at a future G4 snapshot (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/oracle-label-gate.R --self-test |

## Purpose

§7 G4 criterion 2 requires that "oracles are labeled by authority and claim
boundary". P5.3 §2 supplies the vocabulary — a closed set of five authorities,
each with an explicit CAN-certify and CANNOT-certify column — and P5.3 Open
Question 4 reserves the *per-row lifecycle* to "the oracle register", which did
not exist. Both shipped verification slices name it as owed work and boundary it
out (`verification/cache-slice.md:176-182`, `verification/determinism-slice.md:190`).

This register is that artifact. It enumerates every oracle instance the
repository ships, binds each to one authority label, and records the claim it
may not make. It is the data side of P5.3's policy: **P5.3 says what the labels
mean; this register says which instance wears which label.**

**Classification is not certification.** A row here records what an oracle *is*
authorized to certify. It does not assert that the oracle passes, that its
fixtures are correct, or that the claims resting on it are true. The
`rfc3986-grammar` rows in particular exist because RURL-nknytzxz proved a
fixture can carry a confidently-labeled expected value that is simply wrong
about the standard.

**Where no label fits, the row is OPEN.** P5.3 §2 declares its taxonomy closed
and P5.3 Open Question 2 asks the owner to confirm that. Three classes of
shipped oracle fit none of the five (§Open cells). Inventing a sixth label to
make the table tidy would be exactly the false-acceptance move this phase exists
to prevent, so those rows are `UNLABELED` / `OPEN` and carry a carrier.

## Schema

Each row is one oracle instance. An *instance* is a (fixture set or comparator,
claim it adjudicates) pair — not a file. One file may host several instances
where it adjudicates several claims under different authorities, and
`external-url-vectors.csv` hosts three.

| Field | Meaning |
|---|---|
| `oracle_id` | Stable identity, e.g. `OR-001`. Slices and gates cite it. |
| `instance` | The fixture set, grammar seam, or differential comparator, by tracked path, plus the claim it adjudicates when the file hosts more than one. |
| `authority` | Exactly one of P5.3 §2's five labels — `whatwg-wpt`, `rfc3986-grammar`, `libcurl-parity`, `browser-parity`, `self-metamorphic` — or the literal `UNLABELED` when no §2 authority admits the instance. Any other value is malformed and fails the gate. |
| `imported` | `yes` if the instance's expected values originate outside this repository (P5.3 §2.3 then requires a provenance row); `no` if derived in-repo from a standard's text, from an accepted record, or from rurl itself. |
| `can_certify` | What this instance is authorized to prove, narrowed from §2's CAN column to this instance's actual scope. |
| `claim_boundary` | What it CANNOT prove. Inherited from §2's CANNOT column and narrowed. This column is the point of the register. |
| `consumer` | The test, gate, or harness that asserts against it, as `path :: name`. An oracle nothing consumes is dead evidence and is recorded as such. |
| `carrier` | Required iff `authority = UNLABELED`: the `fp` issue owning the taxonomy decision. Per P0.1 §5 its tracker state is NOT authoritative and the gate never reads it. |
| `state` | `PROPOSED` — classification proposed, authoritative on the owner's merge. `OPEN` — no admissible authority; an owner question is recorded in §Open cells. Any other value is malformed. |

## Oracle instances

<!-- Append rows below the header. Each row is authorized by the owner's merge
     of the PR that adds it (P0.1 §4). -->

| oracle_id | instance | authority | imported | can_certify | claim_boundary | consumer | carrier | state |
|---|---|---|---|---|---|---|---|---|
| OR-001 | `tests/testthat/fixtures/external-url-vectors.csv` — the `whatwg_expected` / `standard_expectation` columns (357 rows; 267 `wpt-urltestdata`, 41 Ada-sourced, 2 spec-derived) | whatwg-wpt | yes | That rurl's `whatwg` profile accepts/rejects as the WHATWG URL Standard requires, for the represented cases | Nothing about RFC 3986; nothing about unrepresented inputs; not a full-string serialization claim (the fixture records `clean_url`, which is output surface (c), not the FSSS — P5.3 §1.1 admits only the FSSS and parse→serialize→parse) | `tests/testthat/test-external-url-vectors.R :: rurl conforms to the spec oracle except at documented boundaries` | | PROPOSED |
| OR-002 | `tests/testthat/fixtures/external-url-vectors.csv` — the `rfc3986_expected` column, plus `rurl_deviation` naming the ADR/ticket that owns each departure | rfc3986-grammar | no | RFC 3986 syntax **acceptance/rejection** for the represented productions; the audited departure count — 164/93 on `clean_url`, **179/78** once scored on `serialize_url()` over the same 257-row scope, and 235/90 over the current 325-runnable corpus (P5.4 §2.2). This is an ACCEPTANCE axis and is **not** the full-string conformance headline; that is OR-021 | Full serialization; WHATWG algorithms; any claim that a deviation is correct — `rurl_deviation` records ownership, not justification | `tests/testthat/test-external-url-vectors.R :: the rfc3986 oracle agrees with the RFC 3986 grammar itself` | | PROPOSED |
| OR-003 | `tests/testthat/helper-rfc3986-abnf.R` — a direct transcription of the RFC 3986 ABNF (§3 + Appendix A); the oracle-for-the-oracle that broke the RURL-nknytzxz co-confirmation trap | rfc3986-grammar | no | That the generic URI grammar admits or rejects a given string, independently of rurl (shares no code with it) and independently of any backend | Never "rurl is wrong to reject it" — it is scheme-agnostic and knows nothing of RFC 8089, the PSL, or rurl's policy layer; it certifies grammar, not policy | `tests/testthat/test-external-url-vectors.R :: the RFC 3986 grammar transcription is itself sound` | | PROPOSED |
| OR-004 | `tests/testthat/fixtures/rfc3986-abnf-fixtures.csv` (27 hand-authored rows, each tagged with its RFC section) against `.rfc3986_generic_uri_ok` | rfc3986-grammar | no | The RFC-general branch's admission verdict (ADR 0012 Layer 4a, D1) against hand-authored ABNF cases, asserted directly and never against a backend | Nothing libcurl or WHATWG accepts is evidence here and vice versa: a permissive splitter accepts strings D1 deliberately rejects, so backend parity is NOT grammar conformance | `tests/testthat/test-rfc3986-grammar.R :: the gate verdict matches every hand-authored ABNF fixture` | | PROPOSED |
| OR-005 | `tests/testthat/fixtures/url-standard-conformance.csv` — the WPT-sourced rows (of 75; `source_reference` records the exact upstream input string so a re-pin can re-locate them) | whatwg-wpt | yes | WHATWG parse/serialize expectations for the represented inputs under `url_standard = "whatwg"` | Unrepresented cases; the RFC profile; and — per P5.3 §1.2 — the row's per-component `expected_host` / `expected_path` columns are the 18-field projection, valid for regression, never countable as conformance | `tests/testthat/test-url-standard-conformance.R :: url_standard conformance fixtures match pinned expectations` | | PROPOSED |
| OR-006 | `tests/testthat/fixtures/url-standard-conformance.csv` — the RFC-derived rows, including the two `path-rfc-5-4-*` rows taken verbatim from RFC 3986 §5.4's abnormal-examples table (§5.3 routes an absolute-path reference straight to `remove_dot_segments()`, so they are base-independent) | rfc3986-grammar | no | RFC 3986 normalization and dot-segment behavior for the cited sections | Only the two §5.4 rows are RFC-text-verbatim; the rest are hand-derivations from grammar prose (there is no RFC equivalent of WPT), so they certify a *reading* of the RFC, not the RFC | `tests/testthat/test-url-standard-conformance.R :: PRD §9.3 required regression assertions hold` | | PROPOSED |
| OR-007 | `inst/bench/wpt-url-cases.json` (336 success + 202 failure) via `inst/bench/standard-parity.R`; frozen run at `analysis/parity/whatwg-success-scored.csv` + `analysis/parity/whatwg-failure-scored.csv` | whatwg-wpt | yes | Acceptance and full-component agreement with WPT for schemes rurl supports (`http/https/ftp/file`), scored in the canonical-output config | **Not the v3 conformance headline.** P5.3 §2.2 forbids reporting a "100%" parity-probe result as conformance; the defensible figures are OR-002's labeled acceptance count and OR-022's labeled full-string count. Non-special schemes are out of scope by design, and success scoring is component-level — the projection P5.3 §1.2 bars from conformance totals | `inst/bench/standard-parity.R` (harness; frozen run committed under `analysis/parity/`) | | PROPOSED |
| OR-008 | `inst/bench/rfc3986-probes.csv` (19 probes, each tagged with its RFC section); frozen run at `analysis/parity/rfc-probes-scored.csv` | rfc3986-grammar | no | That rurl's RFC profile satisfies 19 hand-authored normalization/grammar accept cases | **Every probe is an accept case.** With no rejection rows it cannot detect over-permissiveness and barely constrains over-strictness; `analysis/parity/README.md` states this and RURL-lyhcyvsa tracks growing it. It must never be read as "100% RFC 3986 conformant" | `inst/bench/standard-parity.R` (harness; frozen run committed under `analysis/parity/`) | | PROPOSED |
| OR-009 | `analysis/disagreement/diverge-rfc-vs-curl.csv` (52 rows) via `inst/bench/parser-disagreement.R` — rurl(rfc3986) vs libcurl 8.14.1 over the 336-input / 28-axis corpus | libcurl-parity | no | Agreement and divergence between rurl's RFC profile and the libcurl backend — a compatibility fact | Conformance of either side. ADR 0012 treats parity as a compatibility layer, not a standard oracle, and P5.3 §2.1 forbids summing a parity ledger into a conformance total | `inst/bench/parser-disagreement.R` (harness; frozen run committed under `analysis/disagreement/`) | | PROPOSED |
| OR-010 | `analysis/disagreement/diverge-whatwg-vs-adaR.csv` (10 rows) — rurl(whatwg) vs adaR 0.3.5 (the R binding to Ada, Node's WHATWG parser) | UNLABELED | no | Agreement/divergence with a WHATWG reference implementation | No §2 authority admits it: adaR is neither libcurl nor a browser, and `browser-parity`'s boundary text ("browsers repair beyond both standards") is false of a spec-conformant library parser | `inst/bench/parser-disagreement.R` (harness; frozen run committed under `analysis/disagreement/`) | RURL-fertzafe | OPEN |
| OR-011 | `analysis/disagreement/diverge-rfc-vs-python.csv` (190 rows) — rurl(rfc3986) vs CPython `urllib.parse`, via `analysis/disagreement/cross-language-rfc.py` | UNLABELED | no | Agreement/divergence with a second-language RFC-family parser | Same gap as OR-010, and larger: 190 rows of divergence currently carry no authority at all, so nothing constrains how they may be cited | `analysis/disagreement/cross-language-rfc.py` (harness; frozen run committed under `analysis/disagreement/`) | RURL-fertzafe | OPEN |
| OR-012 | `tests/testthat/test-cache-transparency.R` — the caches-DISABLED result (`rurl_cache_config(full_parse = FALSE, puny_encode = FALSE, puny_decode = FALSE)`) as reference for every cache state | self-metamorphic | no | Internal cache transparency: that memoization changes nothing observable across the six equivalence axes of the semantic-cache contract | No external standard whatsoever. "Byte-identical" here means equal component values, `parse_status`, warnings, names, order and types; only time and peak memory may differ | `tests/testthat/test-cache-transparency.R :: cache-enabled output equals cache-disabled output` (and the five sibling axes); classification consumed by `verification/cache-slice.md` §The oracle | | PROPOSED |
| OR-013 | `tools/determinism/corpus.csv` + `tools/determinism/expected-cells.csv` via `tools/determinism/compare-gate.R` — the two P5.2 invariance axes (charset/locale, repeat-run) | self-metamorphic | no | That rurl's output is invariant across the pinned environment cells and across repeated runs, within P5.2's tolerance and its named/bounded/expiring exception mechanism | **Not a regression check** and not a conformance claim: neither axis compares against a committed baseline of expected VALUES, only against rurl's own output in another cell or another run | `tools/determinism/compare-gate.R --self-test` (21 fixtures); classification consumed by `verification/determinism-slice.md` | | PROPOSED |
| OR-014 | `tools/determinism/curl-probe.R` — libcurl-only probe (`curl::curl_parse_url(input, decode = FALSE, params = FALSE)`, byte-identical to rurl's own call in `R/parse-phases.R`), run across the Docker matrix | self-metamorphic | no | That **libcurl itself** is invariant across environments — isolating the one variable under study when a rurl cross-environment divergence appears | **Not a parity ledger.** It never compares rurl to libcurl; it compares libcurl to libcurl. It cannot certify agreement between the two, and labelling it `libcurl-parity` would imply evidence that does not exist (OR-009 is the only parity instance) | `tools/determinism/compare-gate.R` (consumes `out/curl-<LABEL>.csv`) | | PROPOSED |
| OR-015 | `tests/testthat/fixtures/parse-corpus.csv` (119 inputs) × `parse_corpus_combos()` → the `characterization-snapshot` golden file | UNLABELED | no | That the shipped public output of `safe_parse_urls()` has not changed — "known bugs included", by design | No §2 authority admits it. It is not conformance, not parity, and P5.3 §3.3 explicitly excludes it: "snapshot equality alone never discharges a standards or state-preservation claim" — which bars it from `self-metamorphic` rather than admitting it | `tests/testthat/test-characterization-snapshot.R :: safe_parse_urls() output matches characterization snapshot` | RURL-bmljwiot | OPEN |
| OR-016 | `tests/testthat/fixtures/encoding-fixtures.csv` (16 rows) — explicit expected outputs for path/query/userinfo encoding | UNLABELED | no | rurl's INTENDED percent-encoding and path-structure contract, pinned against libcurl drift | Deliberately not a WHATWG claim: cases drawn from WPT where rurl *intentionally* diverges are kept and annotated, so the fixture asserts the contract, not the standard. Same taxonomy gap as OR-015, reached from the opposite direction — hand-authored intent rather than recorded output | `tests/testthat/test-encoding-fixtures.R :: path/encoding fixtures match explicit expectations` | RURL-bmljwiot | OPEN |
| OR-017 | `tests/testthat/test-browser-fixer.R` — the browser-fixer PRD's Part 1 worked-examples table, keyed to the pure fixer output (steps 1-3) | UNLABELED | no | That `.apply_browser_fixup_vec()` reproduces the accepted PRD's worked examples | It is a **contract** oracle, not `browser-parity`: nothing in the tree observes a browser, so the fixer's implicit "this is what browsers do" claim has no differential evidence (RURL-mydybnpl) | `tests/testthat/test-browser-fixer.R :: PRD worked examples: pure fixer output (steps 1-3)` | RURL-bmljwiot | OPEN |
| OR-018 | `tests/testthat/test-url-standard-scaffold.R` — the AC#1 corpus gate: `url_standard = NULL` reproduces the full corpus byte-for-byte | self-metamorphic | no | Migration inertness: that the selector's null setting is and stays observationally identical to pre-selector rurl | Says nothing about whether either output is correct. It is a release-safety invariant over rurl's own output, so a shared bug on both sides is invisible to it | `tests/testthat/test-url-standard-scaffold.R :: url_standard = NULL reproduces the full corpus byte-for-byte` | | PROPOSED |
| OR-019 | `tests/testthat/test-locale-invariance.R` plus the `Tests (LC_ALL=C)` cell of `.github/workflows/verify.yml` | self-metamorphic | no | That output values and `Encoding()` marks do not depend on `LC_CTYPE` — the in-process half in the file, the cross-locale half in CI | Invariance only. Two locales agreeing does not make the shared value correct under any standard | `tests/testthat/test-locale-invariance.R :: non-ASCII output is byte-identical to its UTF-8 encoding` | | PROPOSED |
| OR-020 | `tests/testthat/test-characterization-snapshot.R` — the two metamorphic assertions distinct from the snapshot: scalar/vector row-for-row agreement, and raw component byte-for-byte round-trip | self-metamorphic | no | Scalar/vector parity and raw-component round-trip — two of the six metamorphic properties P5.3 §3.3 makes mandatory | Structural properties only. Split from OR-015 deliberately: the same file hosts a snapshot oracle that P5.3 §3.3 excludes and metamorphic assertions it requires, and conflating them would let snapshot equality inherit a label it is barred from | `tests/testthat/test-characterization-snapshot.R :: safe_parse_urls() agrees row-for-row with safe_parse_url()`; `:: raw scheme/host/path/query round-trip the input byte-for-byte` | | PROPOSED |
| OR-021 | `tests/testthat/fixtures/external-url-vectors.csv` — the `standard_expectation` column scored against `fsss_whatwg` via `oracle_kind`/`oracle_value` (92 string-valued rows: 33 Ada `href`, 43 WPT-component-derived credential/fragment, 16 paper host oracles) | whatwg-wpt | yes | That rurl's `whatwg` full serialization matches upstream expectations on a curated set of KNOWN HAZARDS — credentials, fragments, host confusion | **No longer the full-string headline; that is OR-022.** These 92 rows are an adversarial subset chosen to probe hazards, so a rate over them is not a conformance rate and must not be quoted as one (P5.3 §2.2; owner ruling recorded in P5.4). Acceptance-only rows (`failure`/`accept` sentinels) are reported on their own line and never folded into this count; RFC 3986 questions are OR-002's | `tests/testthat/test-external-url-vectors.R :: credential and fragment vectors match the WHATWG serializer` + the per-source watch lists | | PROPOSED |
| OR-022 | `inst/bench/wpt-url-cases.json` — the full imported web-platform-tests suite (336 success + 202 failure, base-null rows at the pinned upstream revision), success rows scored against upstream's own recorded **`href`** and never against a string re-assembled from the component getters | whatwg-wpt | yes | **The FSSS full-string conformance headline** P5.3 §1 reserved and §2.2 recorded as not yet built: WHATWG URL Standard full serialization over the standard's OWN suite — **326 exact / 10 documented deviations** across 336 success rows, and separately **202 / 202** must-fail rows rejected (P5.4 §2.1). Swept across all four `standard` × `form` configurations | The two substrates are reported separately and **never summed**: an aggregate would let must-fail rejections inflate a serialization figure (P5.3 §2.2). The RFC profile is NOT scored against this oracle — `href` is the WHATWG serialization, so a difference there is standard-vs-standard (`divergence_class`), not `rurl_deviation` (RURL-nknytzxz). Says nothing about inputs the suite does not represent, nor about base-relative resolution, which the import excludes | `tests/testthat/test-wpt-full-suite.R` | | PROPOSED |

## Provenance (P5.3 §2.3)

P5.3 §2.3: "Every imported oracle pins provenance" — eleven fields, from S8's
Oracle integrity gate 9. Rows are required for every instance with
`imported = yes`. A cell is either populated or the literal `MISSING[carrier]`;
the gate rejects a blank.

| oracle_id | upstream_project | revision | path | retrieved | license | raw_source_hash | import_command | transformed_hash | standard_section | claim_kind | applicability_selector |
|---|---|---|---|---|---|---|---|---|---|---|---|
| OR-001 | web-platform-tests/wpt (267 rows); ada-url/ada test data (41 rows) | `MISSING[RURL-hjgtwowf]` | `url/resources/urltestdata.json` | `MISSING[RURL-hjgtwowf]` | BSD-3-Clause (web-platform-tests contributors) | `MISSING[RURL-hjgtwowf]` | `MISSING[RURL-hjgtwowf]` | `MISSING[RURL-hjgtwowf]` | WHATWG URL Standard (version unpinned) | acceptance/rejection + `clean_url` string | failure cases + adversarial vectors; `runnable = "yes"` rows only |
| OR-005 | web-platform-tests/wpt | `MISSING[RURL-hjgtwowf]` | `url/resources/urltestdata.json` | `MISSING[RURL-hjgtwowf]` | BSD-3-Clause (web-platform-tests contributors) | `MISSING[RURL-hjgtwowf]` | hand-transcribed; `source_reference` records the exact upstream `input` string per row | `MISSING[RURL-hjgtwowf]` | WHATWG URL Standard (version unpinned) | component + `clean_url` expectations | rows whose `source_reference` names a WPT input |
| OR-007 | web-platform-tests/wpt | `MISSING[RURL-hjgtwowf]` | `url/resources/urltestdata.json` | 2026-07-08 | BSD-3-Clause (web-platform-tests contributors) | `MISSING[RURL-hjgtwowf]` | `inst/bench/make-wpt-fixture.py` (named in `analysis/parity/README.md`, absent from the fixture's own `_meta`) | `MISSING[RURL-hjgtwowf]` | WHATWG URL Standard (version unpinned) | acceptance + full-component parity | `base in {null, about:blank}`; success limited to `http/https/ftp/file`; failure = any base-null case; NUL inputs dropped |

**What the `MISSING` cells mean.** `revision` is the load-bearing one. P5.3 §2
makes "pinned to an immutable upstream revision + hash" *definitional* for the
`whatwg-wpt` authority, so all three instances currently wear a label whose
defining condition they do not meet. The claim "rurl conforms to WPT" has no
fixed referent until a commit SHA exists to conform *to*, and an upstream change
cannot be detected. This is the same class of defect as RURL-nknytzxz, which
concerned the oracle's **content**; this one concerns its **identity**.
`OR-007`'s `_meta` block is the best of the three (5 of 10 fields, plus a
genuinely precise applicability selector) and is the natural shape to extend.

## Coverage of the taxonomy

| P5.3 §2 authority | instances | note |
|---|---|---|
| `whatwg-wpt` | 3 (OR-001, OR-005, OR-007) | all three lack the revision pin the label's own definition requires |
| `rfc3986-grammar` | 5 (OR-002, OR-003, OR-004, OR-006, OR-008) | OR-003 is the independent transcription that keeps the other four honest |
| `libcurl-parity` | 1 (OR-009) | OR-014 is *not* one, despite calling libcurl |
| `browser-parity` | **0** | the authority certifies nothing today (RURL-mydybnpl) |
| `self-metamorphic` | 6 (OR-012, OR-013, OR-014, OR-018, OR-019, OR-020) | covers 3 of the 6 metamorphic properties P5.3 §3.3 mandates; parse→serialize→parse is unreachable while the FSSS is unbuilt (VD-002) |
| `UNLABELED` | 5 (OR-010, OR-011, OR-015, OR-016, OR-017) | two distinct taxonomy gaps, two carriers |

## Open cells

Three questions that this register surfaces and may not answer. Each needs an
owner act, because each would otherwise be settled by an agent inventing
vocabulary — the precise move P5.3 §2 exists to prevent.

1. **No authority for characterization/regression oracles** (RURL-bmljwiot;
   rows OR-015, OR-016, OR-017). P5.3 §2 declares the five-label set closed and
   §3.1 requires every normative claim to carry one of them, but §Consequences
   says characterization snapshots "stay, re-labeled `v2-characterization` /
   `regression`" — labels from S1-F10 that are not in §2's set. Three shipped
   instances fit no label, and §3.3's exclusion of snapshot equality bars the
   nearest candidate rather than admitting it. This is P5.3 Open Question 2,
   unanswered. *Recommendation:* admit a sixth `contract-characterization`
   authority by successor record, with a CANNOT column that explicitly bars
   every standards claim — the honest reading of what these fixtures already do,
   and cheaper than re-scoping 3 instances onto labels that misdescribe them.

2. **No authority for reference-implementation parity** (RURL-fertzafe; rows
   OR-010, OR-011). adaR (Ada, via Node) and CPython `urllib.parse` are neither
   libcurl nor browsers. 200 rows of committed divergence currently carry no
   authority, so nothing constrains how they may be cited. *Recommendation:*
   generalize `libcurl-parity` to a `implementation-parity` authority carrying a
   named comparand per instance, rather than adding one label per library —
   P5.3 §2.1's binding rule ("no parity oracle certifies conformance") already
   applies uniformly and does not care which implementation is on the other side.

3. **`browser-parity` has zero instances** (RURL-mydybnpl). Nothing observes a
   browser; the browser fixer is asserted against its PRD's worked examples
   (OR-017). *Recommendation:* record `browser-parity` as RESERVED with no
   instances and state in `gates/G4-acceptance.md` that no browser-agreement
   claim is made. Build the ledger only if such a claim is ever asserted.

A fourth item is a defect, not a question, and is tracked rather than open here:
the three `whatwg-wpt` instances do not pin an upstream revision or any hash
(RURL-hjgtwowf, §Provenance).

## Scope boundaries

- **The taxonomy itself** — owned by **P5.3**. This register consumes the five
  labels and the §2.3 field list; it defines neither and proposes no sixth.
- **Whether any oracle is correct** — out of scope. This register binds claims
  to authorities; RURL-nknytzxz is the standing proof that a well-labeled oracle
  can still be wrong.
- **The claim→evidence traceability map** (G4 criterion 1) — a sibling artifact.
  This register is the authority axis; traceability is the coverage axis, and it
  will cite `OR-nnn` rather than restate provenance.
- **Deferred criterion-3 cells** — owned by the verification-deferrals register
  and its gate (P0.5), which are proposed on PR #246 and not yet on `main`; this
  record deliberately cites neither by path, because a forward reference to
  unmerged work is exactly what O4 exists to catch. An oracle that cannot exist
  because its surface is unbuilt is a **deferral**, not an `UNLABELED` row — the
  distinction is whether the *surface* is missing or the *label* is. The two
  registers do not overlap: P0.5's VD-002 defers the FSSS **cells**, while this
  register records the consequence on the authority axis — parse→serialize→parse,
  one of P5.3's two admissible claim substrates, has no `self-metamorphic`
  instance while the serializer is unbuilt.
- **Deviation lifecycle** (owner/status/expiry per documented deviation, P5.3
  Open Question 4) — the recommendation there was to own the *schema* in P5.3
  and the *per-row lifecycle* in this register. The per-deviation rows are
  currently carried in `external-url-vectors.csv`'s `rurl_deviation` column
  (OR-002); migrating them here is a later leaf, not this record.
