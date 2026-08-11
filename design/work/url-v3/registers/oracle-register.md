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
| OR-021 | `tests/testthat/fixtures/external-url-vectors.csv` — the `standard_expectation` column scored against `fsss_whatwg` via `oracle_kind`/`oracle_value` (92 string-valued rows, recounted from the fixture rather than transcribed: 43 WPT-component-derived credential/fragment, 20 IPv4-obfuscation arithmetic, 13 Ada `href`, 16 class-C paper host oracles — an earlier revision of this cell read "33 Ada `href`", which folded the 20 hand-generated arithmetic rows into Ada's count) | whatwg-wpt | yes | That rurl's `whatwg` full serialization matches upstream expectations on a curated set of KNOWN HAZARDS — credentials, fragments, host confusion | **No longer the full-string headline; that is OR-022.** These 92 rows are an adversarial subset chosen to probe hazards, so a rate over them is not a conformance rate and must not be quoted as one (P5.3 §2.2; owner ruling recorded in P5.4). Acceptance-only rows (`failure`/`accept` sentinels) are reported on their own line and never folded into this count; RFC 3986 questions are OR-002's | `tests/testthat/test-external-url-vectors.R :: credential and fragment vectors match the WHATWG serializer` + the per-source watch lists | | PROPOSED |
| OR-022 | `inst/bench/wpt-url-cases.json` — the full imported web-platform-tests suite (336 success + 202 failure, base-null rows at the pinned upstream revision), success rows scored against upstream's own recorded **`href`** and never against a string re-assembled from the component getters | whatwg-wpt | yes | **The FSSS full-string conformance headline** P5.3 §1 reserved and §2.2 recorded as not yet built: WHATWG URL Standard full serialization over the standard's OWN suite — **326 exact / 10 documented deviations** across 336 success rows, and separately **202 / 202** must-fail rows rejected (P5.4 §2.1). Swept across all four `standard` × `form` configurations | The two substrates are reported separately and **never summed**: an aggregate would let must-fail rejections inflate a serialization figure (P5.3 §2.2). The RFC profile is NOT scored against this oracle — `href` is the WHATWG serialization, so a difference there is standard-vs-standard (`divergence_class`), not `rurl_deviation` (RURL-nknytzxz). Says nothing about inputs the suite does not represent, nor about base-relative resolution, which the import excludes — that complementary population is **OR-024**, and the two are separate measurements on disjoint row sets that are **never summed**. **Figure pointer (added RURL-fupsemxr T2.5, no cell rewritten):** the `326 / 10` above was true at P5.4's baseline; P1.3 closed the single host-less-`file:` deviation family (RURL-uhwivndf) and the live figure is **336 / 336**, carried by `tests/testthat/test-wpt-full-suite.R:109`, `NEWS.md` and P1.3 §5. P1.3 §6 deliberately left this cell's own number at its baseline; this pointer states where the current one lives rather than restating it | `tests/testthat/test-wpt-full-suite.R` | | PROPOSED |
| OR-023 | `tests/testthat/test-rfc3986-serialization-properties.R` — RFC 3986 §6.2.2/§6.2.3 stated as PROPERTIES over a generated 6506-input population (256 octets × 2 hex cases × 12 component positions × both scheme classes, plus 47 structural shapes), with every serialization run back through the OR-003 ABNF | rfc3986-grammar | no | **The RFC 3986 serialization quadrant** RURL-irhxumys recorded as empty: that every accepted `rfc3986` full-string output is admitted by the RFC's own grammar, re-parses to itself, and — under `form = "normalized"` — satisfies §6.2.2.1 case, §6.2.2.2 unreserved decoding, §6.2.2.3 dot-segment removal and §6.2.3 default-port elision. Transcribes NO expected strings, so it cannot co-confirm with the implementation the way RURL-nknytzxz caught OR-002 doing | **Necessary, not sufficient.** These are properties of the OUTPUT STRING: an output can satisfy every one of them and still describe the wrong URL, because nothing here checks that a component was sliced from the input correctly. The grammar (OR-003) admits far more than a normalized serialization may be, and the §6.2.2 properties bound it only where the RFC is mechanical. Acceptance is OR-002's axis and is not scored; WHATWG is not scored at all. Raw octets above 0x7F are unreachable until RURL-zexwmwxn, and directly-written non-ASCII is excluded as a SETTLED posture (ADR 0012), not as a passing case | `tests/testthat/test-rfc3986-serialization-properties.R` | | PROPOSED |
| OR-024 | `tests/testthat/fixtures/wpt-url-base-relative.json` — the BASE-RELATIVE half of the imported web-platform-tests suite (274 success rows, the exact complement of OR-022's base-null import at the same pinned revision), each row scored as `serialize_url(rurl:::.resolve_one_raw(input, base), standard = "whatwg")` against upstream's own `href` | whatwg-wpt | yes | That rurl's REFERENCE RESOLUTION agrees with the WHATWG URL Standard, over the standard's own base-carrying corpus — the axis no harness in this repo read at all before RURL-fupsemxr T2.1, because the import filtered base-carrying rows out | **It certifies a KNOWN-DIFFER SET, not conformance.** Re-derived at RURL-fupsemxr T2.5 (measured from the harness, not transcribed): **247 of 274 rows serialize exactly and 27 do not** — `DRIVE_LETTER` 18, `PATH_AS_AUTHORITY` 5, `ABSOLUTE_REF` 4. The 27 are enumerated by family in the consumer and the test is green because the measured set EQUALS the enumerated one. It read 218/56 when the harness landed (T2.1), 231/43 after SAME_SCHEME (T2.3), 245/29 after the reference-preprocessing families (T2.4) and 247/27 after the scheme production (T2.4b). Read it as a regression instrument, never as a conformance rate (P5.3 §2.2), and **never add it to OR-022**: that headline is measured on the base-NULL rows and this one on their complement, so a sum would be two different populations reported as one. Also: the substrate is the FSSS (`serialize_url()`), never the public `resolve_url()`, which returns surface (c) by default and is barred as a claim substrate (P2.7 D-A, P5.3 CLAIM-1); the base-carrying FAILURE rows are NOT imported, so this says nothing about must-fail acceptance under a base; four of the 27 (`ABSOLUTE_REF`) are absolute references whose deviation is in absolute parsing, not resolution — and because upstream files them WITH a base, they fall outside OR-022's population entirely, so OR-022's 336/336 is complete over its own rows and says nothing about these four (`tel:1234567890`, and three percent-encoded/fullwidth obfuscated-IPv4 hosts); and the resolution call passes `url_standard = "whatwg"`, so it scores neither the `rfc3986` profile nor the ADR 0007-frozen NULL selector | `tests/testthat/test-wpt-base-relative.R` :: WPT base-relative rows resolve to the standard's own href |  | PROPOSED |

## Provenance (P5.3 §2.3)

P5.3 §2.3: "Every imported oracle pins provenance" — eleven fields, from S8's
Oracle integrity gate 9. Rows are required for every instance with
`imported = yes`. A cell is either populated or the literal `MISSING[carrier]`;
the gate rejects a blank.

| oracle_id | upstream_project | revision | path | retrieved | license | raw_source_hash | import_command | transformed_hash | standard_section | claim_kind | applicability_selector |
|---|---|---|---|---|---|---|---|---|---|---|---|
| OR-001 | web-platform-tests/wpt (267 rows); ada-url/ada test data (41 rows) | wpt `181476aa16e8b28a07698bef3a0275fa53dd22e5`; ada `308110b26b32d12db492d460a41e57932366269d` — both **verified-at**, not retrieved-at; copied from the per-group fields of `tests/testthat/fixtures/oracle-provenance.json` | `url/resources/urltestdata.json` | `MISSING[RURL-vwurxmzm]` — unrecorded for all three imported groups and unrecoverable; the sentinel the sidecar itself carries, re-carried to the issue that owns it | BSD-3-Clause (web-platform-tests contributors) | wpt `355c9f1e5f34aae66ba8adfabf3c853f5cd30ea22964ef7a53eb292e7975d81e`; ada extra-urltestdata `027f702da55f49ba1d7e249fd8157816f00009333c3e6839c3c83406249ea8c6`; ada verifydnslength `1259165fd8baf69644914741ab318d08c3370184d5b720f7d4548d6cf8f44d6d` | `MISSING[RURL-vwurxmzm]` — unattested and unrecoverable for all three groups; each records a `pin_fetch_command` that re-fetches the pinned bytes, which is a different claim and does not discharge this field | `0e2c05d827a2c339d7175b261722b2221a29f24b0f32e1d691218aa4d18081ab` | WHATWG URL Standard (version unpinned) | acceptance/rejection + `clean_url` string | failure cases + adversarial vectors; `runnable = "yes"` rows only |
| OR-005 | web-platform-tests/wpt | `181476aa16e8b28a07698bef3a0275fa53dd22e5` — the sidecar's `citation_upstream_revision` (**verified-at**). It pins the cited upstream *input strings*, not the expected values, which are hand-derived here; 22 of 23 re-locate, the 23rd citation eliding a run of leading zeros | `url/resources/urltestdata.json` | not applicable — this fixture vendors no upstream bytes, so `tests/testthat/fixtures/oracle-provenance.json` records it `section_2_3_applies = false` and states "No missing sentinel is used in this fixture"; only the cited inputs have an upstream, and no date was recorded for the citation | BSD-3-Clause (web-platform-tests contributors) | `355c9f1e5f34aae66ba8adfabf3c853f5cd30ea22964ef7a53eb292e7975d81e` — the sidecar's `citation_raw_source_sha256`: the digest of the cited upstream file at that revision, not of a vendored import | hand-transcribed; `source_reference` records the exact upstream `input` string per row | `3c5ff62e610ea7232c8a7fcfd7f849ea3f6004e8b4b4df9a6d05d1542b0914fa` | WHATWG URL Standard (version unpinned) | component + `clean_url` expectations | rows whose `source_reference` names a WPT input |
| OR-007 | web-platform-tests/wpt | `181476aa16e8b28a07698bef3a0275fa53dd22e5` — **retrieved-at**: re-running the generation command against the raw source at this revision reproduces the committed fixture exactly, 336 success / 202 failure, both arrays byte-identical | `url/resources/urltestdata.json` | 2026-07-08 | BSD-3-Clause (web-platform-tests contributors) | `355c9f1e5f34aae66ba8adfabf3c853f5cd30ea22964ef7a53eb292e7975d81e` | `inst/bench/make-wpt-fixture.py` (named in `analysis/parity/README.md`, absent from the fixture's own `_meta`) | `5141babf1bad1ab2960421aff80a89e3a9288974b0f702f33e6d3b9c7fb28ae8` | WHATWG URL Standard (version unpinned) | acceptance + full-component parity | `base in {null, about:blank}`; success limited to `http/https/ftp/file`; failure = any base-null case; NUL inputs dropped |
| OR-021 | Per source group, recorded field-by-field in `tests/testthat/fixtures/oracle-provenance.json`; this row must not become a second copy of it. `web-platform-tests/wpt` (43 credential/fragment rows, derived from the in-repo import rather than fetched); `ada-url/ada` (13); NO upstream artifact for the 20 IPv4-obfuscation rows (hand-generated arithmetic) or the 16 class-C paper host rows | wpt `181476aa16e8b28a07698bef3a0275fa53dd22e5`; ada `308110b26b32d12db492d460a41e57932366269d` — both **verified-at**, not retrieved-at; not applicable for the 36 artifact-less rows | `url/resources/urltestdata.json`; `ada-url/ada tests/wpt/ada_extra_urltestdata.json` (repo-qualified: the bare path would collide with this repository's own `tests/` tree); none for the artifact-less rows | 2026-07-08 for the 43 WPT-derived rows, inherited from the in-repo import they derive from; `MISSING[RURL-vwurxmzm]` for the ada rows; not applicable for the artifact-less rows | BSD-3-Clause (web-platform-tests contributors); Apache-2.0 (Copyright 2023 Yagiz Nizipli and Daniel Lemire); the paper and arithmetic rows vendor no artifact | wpt `355c9f1e5f34aae66ba8adfabf3c853f5cd30ea22964ef7a53eb292e7975d81e`; ada `027f702da55f49ba1d7e249fd8157816f00009333c3e6839c3c83406249ea8c6`; not applicable for the artifact-less rows | wpt: the hop-1 `curl -fsSL .../181476aa16e8.../url/resources/urltestdata.json`; ada: `MISSING[RURL-vwurxmzm]`; the row builders that assembled the expected values are untracked — `MISSING[RURL-ozdejfzl]` | `0e2c05d827a2c339d7175b261722b2221a29f24b0f32e1d691218aa4d18081ab` | WHATWG URL Standard §4.5 URL serializing, scored on `fsss_whatwg` (surface (b)) | conformance over a curated HAZARD set — never a conformance rate (P5.3 §2.2) | the 92 string-valued rows, derived not transcribed: `oracle_kind = "exact"` (76 — 43 wpt-credentials-fragments, 20 ip-obfuscation, 13 ada-extra) or `"host"` (16 — 9 equivocal-urls, 7 youarealiar) |
| OR-022 | web-platform-tests/wpt | `181476aa16e8b28a07698bef3a0275fa53dd22e5` | `url/resources/urltestdata.json` | 2026-07-08 | BSD-3-Clause (web-platform-tests contributors) | `355c9f1e5f34aae66ba8adfabf3c853f5cd30ea22964ef7a53eb292e7975d81e` | `curl -fsSL https://raw.githubusercontent.com/web-platform-tests/wpt/181476aa16e8b28a07698bef3a0275fa53dd22e5/url/resources/urltestdata.json -o urltestdata.json` then `python3 inst/bench/make-wpt-fixture.py urltestdata.json --revision 181476aa16e8b28a07698bef3a0275fa53dd22e5 --retrieved 2026-07-08` | `5141babf1bad1ab2960421aff80a89e3a9288974b0f702f33e6d3b9c7fb28ae8` | WHATWG URL Standard §4.5 URL serializing (upstream's own `href`), over §4.4 parsing | conformance — the full-string FSSS headline | base = null only; success = every non-failure case at that revision, all 54 upstream schemes; failure = every base-null failure case; NUL inputs dropped |
| OR-024 | web-platform-tests/wpt | `181476aa16e8b28a07698bef3a0275fa53dd22e5` — the SAME upstream file at the SAME revision as OR-007/OR-022, re-extracted on the complementary `base` predicate, so no re-pin and no second raw source: the raw sha256 below was re-verified against the fetched bytes before the extraction was run | `url/resources/urltestdata.json` | 2026-07-08 — inherited unchanged from the OR-022 import, which is what a same-revision re-extraction is entitled to claim | BSD-3-Clause (web-platform-tests contributors) | `355c9f1e5f34aae66ba8adfabf3c853f5cd30ea22964ef7a53eb292e7975d81e` | `curl -fsSL https://raw.githubusercontent.com/web-platform-tests/wpt/181476aa16e8b28a07698bef3a0275fa53dd22e5/url/resources/urltestdata.json -o urltestdata.json` then `python3 inst/bench/make-wpt-fixture.py urltestdata.json --revision 181476aa16e8b28a07698bef3a0275fa53dd22e5 --retrieved 2026-07-08 --mode base-relative` | `1fb0b33fa2c2ebea1ffb77248ff79dbef46f17efed1b1f29314ecbb760786bc9` | WHATWG URL Standard §4.4 URL parsing with a base URL (reference resolution) and §4.5 URL serializing (upstream's own `href`) | conformance — scoped to reference resolution, and reporting a known-differ set rather than a rate | base != null only; success = every base-carrying non-failure case at that revision, all schemes; the base-carrying failure rows are NOT imported (no harness scores them); rows whose input or base contains NUL are dropped |

**What the `MISSING` cells mean.** `revision` was the load-bearing one, and it
is now filled on all three rows. P5.3 §2 makes "pinned to an immutable upstream
revision + hash" *definitional* for the `whatwg-wpt` authority, so until
RURL-hjgtwowf these instances wore a label whose defining condition they did not
meet: "rurl conforms to WPT" had no fixed referent, and an upstream change could
not be detected. Every value above was **copied** from
`tests/testthat/fixtures/oracle-provenance.json`, which records provenance per
source group; none was derived independently of it, and a disagreement between
the two is a defect in this table rather than a second opinion.

What a pin *means* still differs by row, and the rows say so rather than reading
alike. OR-007's is **retrieved-at** and reproduces — regenerating from the raw
source at that revision yields the committed fixture byte-identically. OR-001's
and OR-005's are **verified-at**: OR-001's rows re-locate at the pinned
revisions, OR-005's cited input *strings* do, and neither dates an import.

Two `MISSING` cells remain, both on OR-001, and both are re-carried to
**RURL-vwurxmzm**, which owns them: nobody recorded the retrieval date or the
import command for its three imported source groups, and nothing can recover
them. The sidecar's per-group `pin_fetch_command` re-fetches the pinned bytes
and reproduces the recorded digest, but it deliberately does not stand in either
field — a reproducing command dates nothing and attests nothing about how the
bytes arrived. OR-005's two former blanks are not gaps of that kind and take no
sentinel: its fixture vendors no upstream bytes at all, so the sidecar records it
`section_2_3_applies = false`, which puts it outside P5.3 §2.3's import
requirement rather than in violation of it.

## Coverage of the taxonomy

| P5.3 §2 authority | instances | note |
|---|---|---|
| `whatwg-wpt` | 6 (OR-001, OR-005, OR-007, OR-021, OR-022, OR-024) | all six carry the revision pin the label's definition requires; OR-001, OR-005 and OR-007 took theirs from the sidecar record (RURL-hjgtwowf), and only OR-001's retrieval date and import command remain unrecorded (RURL-vwurxmzm). OR-024 re-extracts OR-022's upstream artifact at the same revision on the complementary `base` predicate, so it adds an axis (reference resolution) rather than a source |
| `rfc3986-grammar` | 6 (OR-002, OR-003, OR-004, OR-006, OR-008, OR-023) | OR-003 is the independent transcription that keeps the other five honest — and the judge OR-023 runs every serialization through |
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

A fourth item was a defect, not a question — the three `whatwg-wpt` instances
OR-001, OR-005 and OR-007 pinned no upstream revision and no hash. It is
discharged in §Provenance (RURL-hjgtwowf): all three now carry the revision, the
raw-source digest and the transformed-fixture digest, copied from
`tests/testthat/fixtures/oracle-provenance.json`. What survives is narrower and
tracked elsewhere: OR-001's retrieval date and import command were never
recorded and cannot be recovered (RURL-vwurxmzm).

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
