# S8 — Performance, curl removal, conformance, determinism, and migration protocol review

## Executive verdict

**Verdict: not acceptable as written; amend the reconstruction protocol before product-specification drafting begins.**

The protocol names every major S8 topic, but it does not require the evidence structures or measurable gates needed to settle them. A successor draft could truthfully say that it covers “massive CSV/vector workflows,” caches, curl removal, conformance, determinism, and migration while leaving performance unmeasurable, curl calls in companion helpers, self-consistency fixtures labeled as conformance, a non-gating determinism probe, and most of the 29 exported APIs without a v2-to-v3 disposition.

The protocol needs five additional mandatory artifacts:

1. a curl-removal ledger that distinguishes shipped runtime/package coupling from tests, research oracles, generated documentation, and historical evidence, followed by a negative dependency gate;
2. a performance/vector/cache contract with workload classes, shapes, environment manifests, metrics, regression budgets, cache-state variants, and chunk/worker semantics;
3. an oracle manifest that records immutable upstream identity, license, import transformation, claim scope, applicability, and deviation ownership separately from implementation characterization;
4. a deterministic-output gate, not merely an evidence probe, covering OS, R version, locale/charset, cache state, vector shape, and explicit PSL engine snapshots;
5. a per-export v2-to-v3 migration ledger covering signatures, defaults, value shape, names/order, status/error/warning behavior, option/profile mappings, deprecation timing, and downstream validation.

There is also an owner-level release-governance fork that the protocol must not silently resolve. The protocol says CRAN submission occurs only after the coherent curl-free 3.0 slice (`_scratch/url-v3-spec-reconstruction-protocol.md:201-204`, `:307-317`). The latest accepted epic context says the v2.7 determinism gate was met, Tier 2/curl removal remains deferred, and lifting the v2.7 CRAN hold is an owner decision (`fp context RURL-gxqdmpcp`, comments dated 2026-07-20). That conflict blocks normative release language until the owner records whether the protocol supersedes the epic disposition or applies only to 3.0.

Status labels used below are **[Shipped]**, **[Accepted]**, **[Historical evidence]**, **[Protocol proposal]**, and **[Open]**. They must not be collapsed into one authority tier.

## Coverage map

| Area | Protocol coverage | Repository evidence | Sufficiency | Required protocol correction |
|---|---|---|---|---|
| High-volume performance | §6.8 says massive CSV/vector workflows are primary; §7 requires one mixed high-volume vector (`_scratch/url-v3-spec-reconstruction-protocol.md:195-205`, `:221-235`) | Manual 20,000-row benchmark with approximately 5,000 unique inputs and historical single-machine rates (`inst/bench/parse-bench.R:1-37`, `:62-149`) | **Insufficient** | Require workload taxonomy, metrics, environments, repetitions, regression budgets, memory ceilings, and gating policy |
| Vector behavior | §6.1 names scalar/vector/missing/invalid/zero length; red-team asks about duplicates and errors (`_scratch/url-v3-spec-reconstruction-protocol.md:118-132`, `:278-295`) | Scalar length guard; factor and zero-length handling; unique-plus-`match()` restoration (`R/parse.R:431-477`, `:585-625`, `:709-754`, `:1463-1579`) | **Partial** | Require a public-surface vector contract matrix, including names/order/types/recycling/list inputs/warnings and chunk invariance |
| Cache semantics | §6.8 says cache semantics and deterministic behavior (`_scratch/url-v3-spec-reconstruction-protocol.md:195-200`) | Three caches, bounded full-parse default, hard-reset watermark, Stage-A key, engine identity, cache-disabled parity (`R/zzz.R:65-81`, `:118-253`, `:296-350`; `R/parse.R:1388-1438`, `:1463-1579`) | **Insufficient** | Require cache boundaries, key partitions, lifecycle, default, eviction, process/worker ownership, disabled/cold/warm equivalence, and documentation consistency |
| Lazy annotations | §6.8 names lazy or explicit annotations and cost contracts (`_scratch/url-v3-spec-reconstruction-protocol.md:195-199`) | No-selector fast path; selected metadata reparses unique input through uncached Stage A (`R/diagnostics.R:529-559`); SMTP wire projection is opt-in in accepted email PRD | **Partial** | Require default posture, applicability, reuse/caching decision, peak cost, and overhead ceilings for each annotation family |
| Query scaling | §6.4 and §6.8 cover query behavior and performance only generally | Vectorized escape/unescape and linear grouping seams (`R/path-query.R:192-195`, `:294-348`, `:537-539`) | **Insufficient** | Add query-length/pair-count/adversarial-percent workloads, complexity and memory contracts, and opaque-token semantics |
| IDNA/PSL performance | §6.7 names IDNA/PSL semantics; §6.8 names scale (`_scratch/url-v3-spec-reconstruction-protocol.md:186-205`) | Unique host/label batching and Punycode caches (`R/domain.R:42-106`, `:154-229`); PSL delegation and engine seam (`R/domain.R:290-355`) | **Partial** | Require workload and determinism coverage by Unicode-label density, PSL section, unknown suffix, engine snapshot, and cache state |
| Engine, chunk, parallel behavior | PSL engines are mentioned semantically but worker/chunk behavior is not a required contract | Explicit engine identity is in cache keys; matcher is an external pointer and is not serializable to workers (`R/parse.R:1388-1400`; `ARCHITECTURE.md:106-128`) | **Missing** | Require an explicit support/non-support decision and contracts for chunk boundaries, ordering, worker initialization, engine snapshots, and per-process caches |
| Complete curl removal | §6.8 names the complete dependency surface and curl-free acceptance; §11 asks for gates (`_scratch/url-v3-spec-reconstruction-protocol.md:195-205`, `:307-317`) | Active coupling spans parser, path/query, accessors, email diagnostics, tests, metadata, and package imports | **Insufficient** | Require the inventory and negative gates below; “parsing and escape/unescape” is not auditable by itself |
| Conformance provenance | §3 requires primary-source verification; §8 requires conformance; red-team rejects unsupported universal claims (`_scratch/url-v3-spec-reconstruction-protocol.md:36-64`, `:239-264`, `:278-305`) | Strong RFC ABNF seam, mixed external fixture, hand-derived golden fixture, and characterization snapshots; prior oracle contamination is documented (`tests/testthat/test-external-url-vectors.R:1-48`, `:99-115`, `:155-217`; `NEWS.md:244-275`) | **Partial** | Require immutable source pins, import hashes/scripts, license, per-cell claim type, applicability, and deviation state |
| Cross-platform determinism | Desired outcome says deterministic; §6.8 names deterministic caches (`_scratch/url-v3-spec-reconstruction-protocol.md:24-34`, `:195-205`) | 17-cell OS/R/locale probe exists but explicitly cannot fail on divergence and does not trigger on parser changes (`.github/workflows/determinism-probe.yml:1-6`, `:36-60`, `:69-114`) | **Insufficient** | Define a required equality artifact and failing graduation gate for the curl-free implementation |
| v2-to-v3 migration | §6.8 names revoked compatibility and deprecation/removal; §8 reserves a migration chapter (`_scratch/url-v3-spec-reconstruction-protocol.md:201-205`, `:239-264`) | 29 exports, existing selector-only migration notes, named downstream consumers (`NAMESPACE:3-31`; `design/prd/url-standard-selector.md:354-383`, `:545-550`) | **Insufficient** | Require a per-export and per-option migration ledger plus downstream/reverse-dependency evidence |
| CRAN release rule | Protocol repeats curl-free-before-CRAN twice | Epic says the v2.7 determinism requirement was met and curl removal is deferred | **Contradictory** | Record an owner decision defining release/version scope before this statement becomes normative |

## Complete curl dependency inventory

### Shipped runtime and package dependency surface

| Surface | Exact coupling | Semantic contract that replacement evidence must preserve |
|---|---|---|
| Package metadata | `curl` in `Imports` (`DESCRIPTION:35-40`); three generated namespace imports (`NAMESPACE:32-34`); roxygen source declaration (`R/parse.R:349`) | Installation and namespace loading must be curl-free, not merely runtime-idle |
| Core parse backend | `.parse_with_curl()` calls `curl::curl_parse_url(..., decode = FALSE, params = FALSE)` (`R/parse-phases.R:1261-1275`) | Component presence, raw bytes, absent versus present-empty values, errors, scheme/authority parsing, platform-independent output |
| Path decoding | Vector call plus scalar fallback to `curl::curl_unescape()` (`R/parse-phases.R:1417-1425`) | Malformed percent triplets, valid UTF-8, arbitrary bytes, reserved delimiters, failure isolation per element |
| Path encoding | `.encode_path_segments()` calls vectorized `curl::curl_escape()` (`R/path-query.R:192-195`) | Segment boundaries, slash preservation, Unicode encoding, already-escaped input, uppercase/lowercase hex policy |
| Query decoding | `.curl_unescape_vec()` calls `curl::curl_unescape()` (`R/path-query.R:294-348`) | Plus-sign policy, invalid escapes, opaque-token preservation, vector failure semantics |
| Query serialization | Filter path calls `curl::curl_escape()` for decoded key/value columns (`R/path-query.R:536-539`) | Duplicates/order/empty key or value, opaque raw tokens, delimiter escaping, parameter allow/drop semantics |
| Query accessor | `get_query()` path directly calls `curl::curl_unescape()` (`R/accessors.R:689`) | Scalar/vector output, malformed escapes, raw-versus-decoded meaning, error fallback |
| Email recipient diagnostics | URL-level mail local/domain decoding (`R/email-diagnostics.R:383-384`) | Recipient token boundaries, percent-decoded local/domain values, invalid escape diagnostics |
| Dedicated mail helper | Recipient projection decoding (`R/email-diagnostics.R:572-573`) | Positional recipient rows, raw provenance, IDNA/PSL diagnostics, optional SMTP projection isolation |

The runtime inventory is broader than “replace the parser.” Removing `.parse_with_curl()` while leaving escape/unescape helpers, email diagnostics, imports, or roxygen output would fail the protocol’s own “complete curl dependency surface” language (`_scratch/url-v3-spec-reconstruction-protocol.md:200-201`).

### Test coupling that must be migrated, not mistaken for runtime coupling

| Evidence | Current role | v3 disposition required |
|---|---|---|
| `tests/testthat/test-parse-phases.R:263`, `:306`, `:336` | Builds expected parsed objects using `curl::curl_parse_url()` | Replace with independently specified fixtures or state constructors; a removed dependency cannot remain the expected-value oracle |
| `tests/testthat/test-rurl-coverage.R:128-140` | Rebinds namespace `curl_parse_url` to exercise an empty-host path | Replace with backend-neutral seam coverage or a direct state fixture |
| `tests/testthat/test-rurl-coverage.R:342` and `tests/testthat/test-encoding-fixtures.R:162` | Curl-specific characterization/comments | Reword and bind to the v3 contract, not the retired implementation |

### Evidence-only and historical curl uses

These uses are not shipped parser dependencies, but the reconstruction protocol must classify them so a raw repository search has an explicit allowlist rather than producing either a false failure or a silent exception.

| Surface | Exact evidence | Classification |
|---|---|---|
| Cross-platform probe | `tools/determinism/curl-probe.R:293`, `:345`; `tools/determinism/parse-dump.R:384` | **[Historical/reference evidence]** Useful for explaining old libcurl variance; not a v3 implementation oracle |
| Docker libcurl matrix | `tools/determinism/matrix/Dockerfile:8-42`, `run-matrix.sh:31`, `entrypoint.sh:8` | **[Historical/reference evidence]** A libcurl-version sweep no longer proves curl-free parser determinism |
| Parser-disagreement benchmark | `inst/bench/parser-disagreement.R:163`, `:438` | **[Reference comparator]** May remain only if `curl` becomes a non-package, optional research dependency with an explicit execution contract; otherwise archive or replace |
| Documentation/history | `NEWS.md`, `ARCHITECTURE.md`, `ACKNOWLEDGEMENTS.md`, man pages, comments | **[Mixed current/historical]** Current docs must describe the new backend; historical release notes should retain provenance and must not be rewritten as current architecture |

### Curl-free closure the protocol must require

The acceptance artifact should contain: every match; category; owner; replacement contract; test/fixture mapping; removal commit; and permitted historical exception. Its negative gate should prove all of the following:

- no `curl` in `DESCRIPTION`, `NAMESPACE`, generated help imports, or load-time namespace requirements;
- no runtime call or indirect `getFromNamespace()`/binding dependency in `R/`;
- package install, load, examples, vignettes, tests, and `R CMD check` succeed in a library where `curl` is absent;
- static repository scan has only reviewed historical/reference allowlist entries;
- parser, path, query, accessor, and email percent-processing replacement suites are independent of curl;
- any optional disagreement tool does not reintroduce `curl` into shipped dependencies or graduation gates.

## Performance, vectorization, and cache contract inventory

### Current shipped mechanics versus missing v3 requirements

| Dimension | Shipped/accepted evidence | What is already trustworthy | Missing protocol contract |
|---|---|---|---|
| Scalar/vector entry points | Scalar rejects length other than one (`R/parse.R:470-477`); vector accepts factor labels, zero length, character fast path, and non-character coercion rules (`R/parse.R:709-754`) | Basic parse entry-point shape is explicit | Same matrix is not required for every exported accessor/helper; names, row names, warning cardinality, list/data-frame inputs, and error isolation remain unspecified |
| Duplicate work | Unique input is parsed/cached once and restored by `match()` (`R/parse.R:745-754`, `:1487-1579`) | Order and duplicates are intentionally reconstructed | No normative complexity, peak-memory, name-preservation, or chunk-boundary contract |
| Stage split | Stage A holds expensive parse/PSL work; Stage B is rerun and never cached (`R/parse.R:1463-1478`, `:1556-1559`; accepted ADR 0003) | Presentation profiles can share parse work | The protocol does not decide whether this implementation shape survives v3 or only its observable cost/equality properties do |
| Full-parse key | URL plus Stage-A-affecting protocol/www/TLD/relative/standard/policy/acceptance/fixup/engine axes (`R/parse.R:1402-1438`) | Cross-profile stale hits are actively prevented | Required key partitions for the new parser/object model, PSL default snapshot, locale, versioning, and migrated selectors are not enumerated |
| Engine identity | Snapshot hash preferred, external-pointer print fallback (`R/parse.R:1388-1400`) | Explicit engines with identical snapshots can share entries | Null engine follows session-global `pslr` state; pointer fallback and worker/process identity need a deterministic v3 policy |
| Full-parse capacity | Default is 100,000 entries; next new entry at capacity clears the entire cache (`R/zzz.R:65-81`, `:187-215`, `:296-316`) | Peak entry count is bounded | Reset-watermark behavior, memory rather than count, default, observability, and long-running unique-stream behavior lack v3 acceptance criteria |
| Punycode caches | Encode/decode caches are unbounded and can be disabled; work is batched over unique hosts/labels (`R/zzz.R:187-192`, `:314-320`; `R/domain.R:42-106`, `:154-229`) | Cache-disabled output parity has targeted tests (`tests/testthat/test-cache-policy.R:52-88`) | “Small by design” is not a bound for massive unique-IDN input; no memory or churn acceptance rule |
| Cache key encoding | Locale-independent ASCII key seam with invalid-UTF-8 hex fallback (`R/zzz.R:118-170`) | Current cache naming avoids native-encoding variance | v3 must preserve injectivity and result equivalence, not accidentally canonize `stringi` implementation detail |
| Cache tests | Disabled parity, Punycode parity, and entry-count bound (`tests/testthat/test-cache-policy.R:44-110`) | Important correctness seams are covered | Test reset forces `max_full_parse = Inf` (`tests/testthat/test-cache-policy.R:7-16`), so the shipped 100,000 default and reset behavior are not tested as defaults |
| Optional URL metadata | No-selector fast path does no work; selected standard parses unique input via uncached Stage A (`R/diagnostics.R:529-559`) | Annotation selection is visibly optional | No reuse, latency/throughput overhead, memory, or duplicate/cache contract; current path can repeat expensive parse/PSL work after an ordinary accessor call |
| Query processing | Escape/unescape is vectorized and key grouping is linear (`R/path-query.R:192-195`, `:294-348`) | It avoids the prior quadratic key grouping | No giant-query workload, scaling slope, percent-error density, pair cardinality, or peak-allocation requirement |
| PSL | Delegated fixed seam (accepted ADR 0001); explicit engine supported (`R/domain.R:290-355`) | Ownership boundary is durable | Default snapshot provenance, refresh timing, worker transfer, per-engine cache interaction, and reproducible benchmark data are missing |
| Parallel/chunk execution | Engine matcher external pointer is not serializable; caches are process-local (`ARCHITECTURE.md:106-128`) | Constraint is documented | Protocol never says whether parallel parsing is supported, how a worker gets the same snapshot, or whether chunking changes warnings/cache/output/order |

### Current benchmark evidence and its claim boundary

`inst/bench/parse-bench.R` is explicitly manual and non-test evidence (`:1-8`). It measures one 20,000-row synthetic vector with roughly 5,000 unique spellings, cold and warm cache paths, a scalar accessor loop, and a five-profile pattern (`:10-37`, `:62-149`). Those numbers are useful historical evidence for the Stage-A/vector refactor; they are not a performance contract because they have:

- no pass/fail thresholds or regression budget;
- one recorded Apple Silicon/R 4.6.0 environment and no dependency/snapshot manifest;
- one `system.time()` observation per case (`inst/bench/parse-bench.R:85-92`), with no warmup, repetitions, distribution, or noise policy;
- no peak allocation/RSS or cache-memory measurement;
- no all-unique massive stream, bounded-cache churn, cache-disabled run, million-row/chunked run, or worker run;
- no selected diagnostics, SMTP projection, IDNA-heavy/PSL-heavy corpus, alternative engine snapshot, giant query, malformed-percent density, or curl-free replacement comparison.

The v3 protocol should require measurable scenarios but should not hard-code today’s Apple Silicon rates as universal targets. Threshold selection remains an owner decision. At minimum, each benchmark record needs package revision, R/OS/CPU, dependency and PSL snapshot versions, corpus hash/generator seed, cache configuration/state, repetitions/statistic, elapsed time, throughput, peak memory, and result hash.

## Conformance and oracle evidence matrix

| Evidence plane | Current artifact and provenance | What it can support | What it cannot support / protocol gap |
|---|---|---|---|
| Normative RFC grammar | `tests/testthat/fixtures/rfc3986-abnf.csv`; grammar cross-check in `tests/testthat/test-external-url-vectors.R:219-251`; accepted ADR 0012 D1 and layered plan | Independent syntax acceptance/rejection for represented RFC 3986 productions | Full serialization, WHATWG algorithms, Unicode/IRI behavior, API shapes, performance, or universal RFC conformance |
| External adversarial vectors | `tests/testthat/fixtures/external-url-vectors.csv`; schema/provenance narrative in `tests/testthat/test-external-url-vectors.R:1-48`, `:74-97` | Source-linked failure/acceptance/serialization cases and explicit rurl deviation rows | Fixture does not establish an immutable upstream revision/hash/import manifest for every source; `accept` asserts only admission, not serialization (`:190-201`) |
| Deviation ledger in fixture | `rurl_deviation` ownership and invariants (`tests/testthat/test-external-url-vectors.R:99-115`, `:171-181`) | Distinguishes implementation departure from standard relationship | Deviations are skipped by the conformance assertion (`:184-217`); schema lacks lifecycle/status/expiry or an explicit v3 disposition |
| Aligned bucket | Self-consistency invariant (`tests/testthat/test-external-url-vectors.R:155-163`) | Characterization that both current profiles agree | It is deliberately uncited and has no independent oracle; it must not be counted as conformance coverage |
| Hand-derived URL-standard fixture | `tests/testthat/fixtures/url-standard-conformance.csv`; tests in `tests/testthat/test-url-standard-conformance.R` | Repository decision examples and selected WHATWG/WPT-derived rows | “Pinned” inputs are not necessarily pinned upstream revisions; hand derivation and source-derived truth need separate claim labels |
| Characterization snapshot | `tests/testthat/fixtures/parse-characterization.csv`; `tests/testthat/test-characterization-fixtures.R` | Current-output regression and scalar/vector parity | Normative correctness or independent standard conformance |
| Encoding fixture | `tests/testthat/fixtures/encoding-fixtures.csv`; `tests/testthat/test-encoding-fixtures.R` | Locale/byte edge characterization and regression | Complete parser/serializer or cross-platform proof |
| Determinism corpus and dumps | `tools/determinism/corpus.R`, `parse-dump.R`, README run record; full component rows plus environment metadata | Cross-cell empirical equality for the exact revision/corpus/matrix | Permanent gate, future regression proof, or curl-free implementation proof; generated artifacts are not shipped and are retained externally |
| Historical parser comparison | `inst/bench/parser-disagreement.R`, research documents, paper tables | Discovery and disagreement examples | Normative oracle without primary-source re-derivation; protocol §3 correctly says audits are discovery aids (`_scratch/url-v3-spec-reconstruction-protocol.md:56-64`) |

The repository has already demonstrated why this separation is mandatory: WPT-derived expectations were once transcribed into the RFC column, rurl’s policy happened to agree, and oracle plus implementation falsely confirmed each other (`tests/testthat/test-external-url-vectors.R:32-45`, `:106-112`; `NEWS.md:244-275`). The current grammar cross-check is a good repair. The reconstruction protocol should convert that lesson into a required oracle schema rather than rely on reviewers remembering it.

For every fixture or imported row set, require: immutable upstream project/revision/path; retrieval date; license; raw-source hash; import/generation command or script; transformed-fixture hash; standard/version/section or algorithm step; claim kind (`syntax`, `acceptance`, `serialization`, `component`, `diagnostic`, `characterization`, `performance`); applicability selector/profile; expected value and comparison mode; known exclusions; implementation deviation owner/status/expiry; and the normative requirement ID tested. A fixture without an independent expected value may remain valuable, but must be labeled characterization or differential evidence.

## Determinism evidence and gate gap

The existing harness is unusually candid and useful as historical evidence:

- it says “EVIDENCE COLLECTION, NOT A GATE” and deliberately never fails on cell divergence (`.github/workflows/determinism-probe.yml:1-6`);
- it runs 17 explicit OS/R/locale cells, separating build, charset, Turkish-locale, and runner-default axes (`.github/workflows/determinism-probe.yml:36-37`, `:69-114`);
- its pull-request trigger is limited to the harness and workflow, not parser or dependency changes (`.github/workflows/determinism-probe.yml:40-60`);
- its historical README records a 2026-07-20 run where all 17 parse dumps were byte-identical (`tools/determinism/README.md:457-482`).

That supports the claim “this revision produced equal dumps in this evidence sweep.” It does not support a normative v3 guarantee. The protocol must define what bytes/fields are compared, which cells are comparable, what failure means, where the result manifest is retained, which changes trigger it, and which graduation step requires it. After curl removal, the old libcurl-version probe becomes historical causality evidence; v3 needs a parser/serializer/result matrix whose success is required.

## v2-to-v3 migration surface matrix

The package exports 29 symbols (`NAMESPACE:3-31`). The protocol’s current migration bullets do not force the successor draft to account for all of them.

| Public surface | Exports | Required migration questions |
|---|---|---|
| Parse entry points | `safe_parse_url`, `safe_parse_urls` | Signature/default selector changes; 14/current versus proposed field set; scalar `NULL` versus vector error row; factor/list/zero-length behavior; status/warning/error changes; names/row order; accepted/revoked parsing compatibility |
| Component accessors | `get_clean_url`, `get_domain`, `get_fragment`, `get_host`, `get_host_type`, `get_parse_status`, `get_password`, `get_path`, `get_port`, `get_query`, `get_scheme`, `get_scheme_class`, `get_subdomain`, `get_tld`, `get_url_diagnostics`, `get_user`, `get_userinfo` | Selector/profile coverage; output type/length/names; missing/invalid rows; absent versus empty components; warning behavior; lazy annotation cost; whether legacy names remain aliases or deprecations |
| Email helper | `get_mailto_recipients` | Default standard/acceptance; one-row-per-recipient shape; raw/decoded columns; diagnostics and optional SMTP wire cost; interaction with first-recipient generic accessors |
| Host helpers | `is_valid_host`, `check_hosts` | Syntax/DNS/PSL meaning; vector/data-frame shape; engine argument and snapshot; diagnostics; v2 validity terminology migration |
| Query helper | `query_param_summary` | Raw/decoded semantics; duplicates/order; plus handling; opaque tokens; empty/malformed input; giant-query complexity |
| Composition/resolution | `canonical_join`, `resolve_url` | Input recycling, key columns, grouping/order/names, standard/profile propagation, resolver algorithm, non-web/opaque forms, chunk behavior |
| Profile surface | `url_profile` | Old option-to-profile mapping; explicit override precedence; renamed/removed values; serialized configuration compatibility |
| Cache surface | `rurl_cache_config`, `rurl_cache_info`, `rurl_clear_caches` | Cache names/defaults/key/versioning/eviction; returned columns; old cache-setting code; process/worker semantics; whether v2 caches can survive an upgrade |

The existing selector PRD’s migration section is limited to opting into `url_standard`, retaining old defaults, and downstream pinning (`design/prd/url-standard-selector.md:354-383`). It explicitly names pagerankr and semantic and later asks for migration notes (`:367-383`, `:545-550`); the benchmark also identifies an rurl-mcp multi-accessor pattern (`inst/bench/parse-bench.R:118-124`). These are known consumer shapes, not a complete reverse-dependency inventory.

The v3 protocol must require, for each export and every public option/value: v2 behavior; proposed v3 behavior; change class (`preserve`, `intentional break`, `deprecate`, `remove`, `rename`, `unresolved`); compatibility adapter or absence; warning class/text/cardinality; first deprecated version; removal condition/version; documentation example; normative requirement IDs; fixtures/tests; and downstream owner/evidence. “Deprecate in 3.0 and remove in 3.1 where appropriate” (`_scratch/url-v3-spec-reconstruction-protocol.md:202-203`) is not a policy until “where appropriate” and the removal readiness criteria are explicit.

## Severity-ordered findings

### Blocker 1 — The CRAN rule conflicts with the accepted epic’s latest release disposition

- **Protocol section:** §6.8 and §11 (`_scratch/url-v3-spec-reconstruction-protocol.md:195-205`, `:307-317`).
- **Exact evidence:** The protocol twice requires curl-free 3.0 before CRAN. `fp context RURL-gxqdmpcp` records the 2026-07-19 decision to pause rurl 2.7 for libcurl determinism, then a 2026-07-20 result that UTF-8/C full-suite and all 17 dump comparisons were clean; its latest comment leaves lifting the **2.7** hold to the owner and defers Tier 2/curl removal.
- **Minimal example:** “Do not submit rurl 2.7 until cross-platform outputs are deterministic” and “submit no package until curl-free 3.0” yield opposite actions after the recorded v2.7 gate passes.
- **Consequence:** The reconstruction process could silently broaden a version-specific hold into a multi-release freeze, or a release could violate the protocol while following the accepted epic.
- **Required correction:** Add a visible owner-decision row: whether v2.7 may be submitted after its recorded determinism gate, and whether the curl-free rule applies specifically to 3.0. Cite the decision and mark the superseded statement; do not infer it during drafting.

### Blocker 2 — “Complete curl removal” has no auditable closure artifact or negative gate

- **Protocol section:** §6.8 and §11 (`_scratch/url-v3-spec-reconstruction-protocol.md:195-205`, `:307-317`).
- **Exact evidence:** Shipped coupling includes package imports (`DESCRIPTION:35-40`; `NAMESPACE:32-34`), core parse (`R/parse-phases.R:1273`), path/query processing (`R/parse-phases.R:1417-1423`; `R/path-query.R:195`, `:348`, `:537-539`), an accessor (`R/accessors.R:689`), and email diagnostics (`R/email-diagnostics.R:383-384`, `:572-573`). Tests also generate expectations from curl (`tests/testthat/test-parse-phases.R:263`, `:306`, `:336`).
- **Minimal example:** Removing `curl_parse_url()` and `Imports: curl` still leaves `get_query()` and `get_mailto_recipients()` requiring `curl_unescape()`.
- **Consequence:** A draft or implementation can declare the parser curl-free while retaining an install-time/runtime dependency, or can replace calls but retain curl as the conformance oracle.
- **Required correction:** Make the curl-removal ledger and closure gates in the inventory above mandatory deliverables, with a reviewed allowlist for historical/reference tools.

### Blocker 3 — Performance is a goal without a falsifiable contract

- **Protocol section:** §6.8, §7, and §8 (`_scratch/url-v3-spec-reconstruction-protocol.md:195-205`, `:221-235`, `:257-260`).
- **Exact evidence:** The only parser performance harness is manual, explicitly not a test, uses a 20,000/approximately-5,000-unique synthetic workload, and records single-machine historical figures (`inst/bench/parse-bench.R:1-37`, `:62-149`). The protocol supplies neither scenarios nor metrics, environment, thresholds, memory limits, or regression policy.
- **Minimal example:** An implementation can be faster for 20,000 duplicate-heavy URLs with a warm cache but 10× slower and memory-unbounded for one million unique IDN URLs; both satisfy “massive CSV/vector workflows” as written.
- **Consequence:** Reviewers cannot determine whether v3 preserves the package’s primary scale use case or whether optional annotations make it unusable.
- **Required correction:** Require a performance contract artifact with scenario taxonomy, environment/corpus manifests, cache variants, repetitions/statistics, throughput/latency/peak-memory metrics, and owner-approved regression budgets. Threshold values remain explicitly open until approved.

### High 4 — Vectorization is not reconstructed across the public API

- **Protocol section:** §6.1 and red-team checklist (`_scratch/url-v3-spec-reconstruction-protocol.md:118-132`, `:278-295`).
- **Exact evidence:** Core vector behavior is detailed in `safe_parse_urls()` (`R/parse.R:709-754`, `:1463-1579`), but the protocol requires no per-export matrix. The public surface contains 29 symbols (`NAMESPACE:3-31`) with vectors, lists, and data-frame returns.
- **Minimal example:** A vector accessor may preserve length/order yet silently drop input names, emit one aggregate warning instead of row-local status, or recycle a length-two option differently from v2.
- **Consequence:** “Vectorized” can mean only accepting length greater than one, while observable compatibility and deterministic chunking change.
- **Required correction:** Require the vector contract matrix and migration fields described above for every exported function, plus equality across scalar loop, one vector call, duplicate-heavy input, and deterministic chunk recombination where supported.

### High 5 — Cache behavior is under-specified and the current evidence base contradicts itself

- **Protocol section:** §6.8 (`_scratch/url-v3-spec-reconstruction-protocol.md:195-200`).
- **Exact evidence:** Runtime and generated help define a 100,000-entry hard-reset watermark (`R/zzz.R:65-81`, `:296-316`; `man/rurl_cache_config.Rd`), but `README.Rmd:265-269` says `full_parse` is unbounded by default. The cache-policy test helper forces `Inf`, and its first test then expects `Inf` (`tests/testthat/test-cache-policy.R:7-30`), so it does not test the shipped default. The actual Stage-A key has more axes than stale nearby prose claims (`R/parse.R:1402-1438`, `:1469-1478`).
- **Minimal example:** A million-unique URL stream is described as bounded in runtime documentation but unbounded in the README; a reviewer cannot know which contract to migrate.
- **Consequence:** The v3 draft may preserve stale documentation, choose an incompatible eviction/default, miss a key axis, or claim deterministic cache behavior without cold/warm/off equality.
- **Required correction:** Require a current-state contradiction row and an accepted cache contract covering key partitions, cache contents, default, lifecycle/eviction, memory posture, engine/PSL identity, process/worker scope, diagnostics, and cold/warm/off/bounded equivalence.

### High 6 — Conformance provenance and claim scope are not mandatory enough to prevent a known failure mode

- **Protocol section:** §3, §5, §8, and red-team review (`_scratch/url-v3-spec-reconstruction-protocol.md:36-117`, `:239-264`, `:278-305`).
- **Exact evidence:** The external fixture has useful source/reference and deviation columns (`tests/testthat/test-external-url-vectors.R:74-115`) but the aligned bucket substitutes uncited self-consistency for an independent oracle (`:155-163`), `accept` cells assert no serialization (`:190-201`), and known deviations are skipped (`:184-217`). A prior cross-standard oracle contamination produced false green results (`:32-45`, `:106-112`; `NEWS.md:244-275`).
- **Minimal example:** A row where both rurl profiles serialize identically can enter `aligned` with no standard expectation or reference and later be counted informally as “two-standard conformance.”
- **Consequence:** Unsupported universal claims can survive even though the protocol asks a reviewer to notice them.
- **Required correction:** Require the oracle manifest/schema above and a coverage report by claim kind and API layer. Self-consistency, characterization, differential comparison, normative grammar, and normative serialization must be counted separately.

### High 7 — Determinism is evidence collection, not an acceptance gate

- **Protocol section:** desired outcome and §6.8 (`_scratch/url-v3-spec-reconstruction-protocol.md:24-34`, `:195-205`).
- **Exact evidence:** The workflow explicitly never fails on divergence and triggers on harness files rather than parser changes (`.github/workflows/determinism-probe.yml:1-6`, `:40-60`). Its 17-cell matrix is good evidence for a particular run (`:69-114`; `tools/determinism/README.md:457-482`).
- **Minimal example:** A parser change can alter Windows C-locale output; normal CI stays green, and the evidence workflow neither runs nor fails automatically.
- **Consequence:** A curl-free implementation can graduate with the same class of cross-platform variance that motivated the epic.
- **Required correction:** Require a failing v3 graduation gate over a defined comparable matrix, result hash/manifest, parser-change trigger, artifact retention, and explicit triage path. Preserve the current probe as non-gating research if useful.

### High 8 — Migration coverage is chapter-shaped, not public-surface complete

- **Protocol section:** §6.8 and product-spec chapters 14-16 (`_scratch/url-v3-spec-reconstruction-protocol.md:201-205`, `:257-260`).
- **Exact evidence:** `NAMESPACE:3-31` exports 29 symbols. Existing migration evidence covers mainly the URL-standard selector and named consumers (`design/prd/url-standard-selector.md:354-383`, `:545-550`), not every signature/value shape. Repository history shows names and vector behavior have been compatibility-sensitive (`NEWS.md:608-658`).
- **Minimal example:** A v3 plan can explain `safe_parse_urls()` and deprecated aliases while omitting changed columns from `check_hosts()`, cache-info defaults, or `canonical_join()` name/order behavior.
- **Consequence:** Downstream breakage appears after architecture acceptance, when the protocol says implementation decomposition may finally begin.
- **Required correction:** Make the per-export/per-option migration ledger and downstream validation gate mandatory before the migration batch can be approved.

### Medium 9 — PSL engine, chunk, and worker determinism lack an explicit support boundary

- **Protocol section:** §6.7-6.8 (`_scratch/url-v3-spec-reconstruction-protocol.md:186-205`).
- **Exact evidence:** Cache identity prefers a PSL snapshot hash but can fall back to an external-pointer print (`R/parse.R:1388-1400`); architecture says matchers are external pointers and are not serialized to workers (`ARCHITECTURE.md:106-128`). Null engine uses the package/session default seam (`R/domain.R:290-355`).
- **Minimal example:** Two workers can initialize different default PSL snapshots or cannot receive an explicit engine, yet concatenated rows look structurally valid.
- **Consequence:** Parallel or chunked high-volume results may be nondeterministic even if the parser bytes are stable.
- **Required correction:** Require an owner decision on supported parallel/chunk modes and define snapshot provenance, worker initialization, cache scope, ordering, warning aggregation, and result-equivalence gates.

### Medium 10 — Optional annotation cost is named but not measurable

- **Protocol section:** §6.8 (`_scratch/url-v3-spec-reconstruction-protocol.md:195-199`).
- **Exact evidence:** URL metadata is free only when no standard is selected; otherwise it reparses unique inputs through uncached Stage A (`R/diagnostics.R:535-552`). The accepted email diagnostics design keeps SMTP wire work opt-in because it is more expensive.
- **Minimal example:** Calling a parse accessor and then diagnostics on the same million-row vector can redo core parse/PSL work even when ordinary parse results are warm.
- **Consequence:** A nominally optional annotation surface may dominate high-volume runtime and invalidate cache expectations.
- **Required correction:** For each annotation family require default/applicability, reuse/cache rule, duplicate behavior, overhead ceiling or regression budget, peak memory, and cache/off equality.

### Medium 11 — Query performance and encoding parity are hidden inside the broad vector example

- **Protocol section:** §6.4, §6.8, and fixture list (`_scratch/url-v3-spec-reconstruction-protocol.md:159-176`, `:195-205`, `:221-235`).
- **Exact evidence:** Query decode/grouping is vectorized (`R/path-query.R:294-348`), but escape calls remain curl-backed and filtering evaluates encoded values even when opaque results are selected (`R/path-query.R:536-539`).
- **Minimal example:** One URL containing 250,000 duplicate parameters and malformed percent tokens is a one-row vector; the required high-volume mixed vector does not exercise its time or allocation behavior.
- **Consequence:** Curl replacement can preserve ordinary examples while changing malformed escapes, opaque fallbacks, complexity, or memory catastrophically.
- **Required correction:** Require dedicated query workloads by URL length, pair count, unique-key ratio, duplicate distribution, percent-error density, Unicode, plus policy, and opaque tokens, with semantic fixtures and scaling/peak-memory evidence.

### Medium 12 — Accepted Stage A/B and curl-shim architecture may be mistaken for immutable v3 design

- **Protocol section:** authority order and source audit (`_scratch/url-v3-spec-reconstruction-protocol.md:36-117`).
- **Exact evidence:** Accepted ADR 0003 defines Stage-A caching around expensive curl/PSL work; accepted ADR 0009 defines a compatibility shim specifically for libcurl charset behavior. Accepted ADR 0012 distinguishes normative grammar/standard rules from current parser backend and explicitly treats parity as a compatibility layer, not a standard oracle.
- **Minimal example:** A reviewer copies “Stage A contains curl components” into the v3 normative cache contract even though curl must disappear.
- **Consequence:** Historical implementation constraints can fossilize into the curl-free architecture or be discarded with observable compatibility/performance behavior.
- **Required correction:** Require each accepted ADR decision to be classified as `normative behavior`, `implementation constraint`, `historical mitigation`, or `superseded candidate`, with the v3 disposition recorded. Preserve observable invariants unless intentionally revoked; do not require the old internal decomposition by default.

### Low 13 — Current-state documentation contradictions need an acceptance-time closure rule

- **Protocol section:** source audit and traceability (`_scratch/url-v3-spec-reconstruction-protocol.md:66-117`, `:319-334`).
- **Exact evidence:** README cache default conflicts with runtime/generated help (`README.Rmd:265-269`; `R/zzz.R:65-81`, `:296-316`), and Stage-A key comments at `R/parse.R:1469-1478` omit newer key axes shown by `.parse_cache_keys()` (`:1402-1438`).
- **Minimal example:** Both the stale default and current default can be cited as shipped truth unless the contradiction is resolved.
- **Consequence:** The reconstructed baseline can contain mutually incompatible statements even with a source audit.
- **Required correction:** Add a gate that every discovered contradiction is either resolved in the accepted decision ledger or retained with owner/status/impact; no unexplained contradiction may feed normative language.

## Contradictions and open owner decisions

| Decision | Conflicting evidence | Owner must decide |
|---|---|---|
| v2.7 versus v3 CRAN hold | Protocol `:204`/`:317` says curl-free before CRAN; latest `fp context RURL-gxqdmpcp` says v2.7 determinism gate met, curl removal deferred, release left to owner | Is v2.7 eligible now, with curl-free hold scoped to 3.0, or does the protocol intentionally supersede that disposition? **Design fork; blocker.** |
| Performance acceptance budgets | Protocol supplies qualitative goals; manual benchmark supplies historical rates only | Which workloads are release-blocking, what relative/absolute regression and memory budgets apply, and on which reference environments? |
| Cache architecture | Shipped default is 100,000 hard-reset; README says `Inf`; Punycode caches are unbounded | Preserve/change defaults and eviction; set an IDNA cache memory posture; define whether caches are API compatibility or implementation detail |
| Parallel/chunk support | High-volume use is primary; explicit engines are external-pointer-backed and caches are process-local | Support in-package workers, document caller-managed chunking only, or make parallelism a non-goal; define snapshot/result equivalence accordingly |
| Oracle version target | Protocol calls for primary standards, but no mandatory immutable version fields | Pin WHATWG/WPT to a revision for a release, track living-standard head, or maintain both a release pin and drift probe |
| Deviations | Current fixture skips rows with `rurl_deviation`; some are ADR-owned, some ticket-owned | Which deviations are intentional v3 compatibility, scheduled fixes, or expired historical boundaries? |
| Curl reference tools | Curl is useful as historical/differential evidence but must leave package dependency surface | Archive them, keep them in a separate optional research environment, or replace them; define the static-scan allowlist |
| Public deprecation policy | Protocol says deprecate in 3.0/remove in 3.1 “where appropriate” | Which symbols/values, what warning policy, what minimum migration window/usage evidence, and what removal readiness condition? |

## Required acceptance gates before product drafting or graduation

These are process gates and evidence requirements, not implementation-task decomposition.

1. **Owner fork gate.** Record and cite the version-scoped CRAN decision. Mark the superseded statement in either the protocol or epic context.
2. **Curl-free inventory gate.** Approve a complete categorized curl ledger. Runtime/package/tests must reach zero; historical/reference exceptions require an allowlist and cannot be shipped dependencies or normative expected-value generators.
3. **Curl-absent package gate.** Install, load, run examples/vignettes/tests, and complete `R CMD check` in a clean library without `curl`; verify `DESCRIPTION`, `NAMESPACE`, generated docs, and source scans.
4. **Percent-semantics parity gate.** Independent fixtures cover parse components, paths, queries, accessors, and email local/domain decoding across valid Unicode, arbitrary bytes, malformed percent triplets, reserved delimiters, plus handling, empty components, and opaque tokens.
5. **Public vector contract gate.** Every export has scalar/vector/zero/missing/invalid/factor/list/duplicate/name/order/type/recycling/warning/error rows as applicable, with scalar-loop and vector equality and a documented chunk posture.
6. **Performance gate.** Owner-approved corpus classes and regression budgets cover cold/warm/off/bounded caches; low/high uniqueness; 20,000 and genuinely massive/chunked streams; selected annotations; long queries; IDNA/PSL; explicit/default engines; elapsed throughput and peak memory. Each result has a reproducible environment/corpus manifest and repetitions/statistic.
7. **Cache correctness gate.** Output is byte-identical across cold/warm/disabled/bounded/reset states; every Stage-A-affecting axis partitions keys; default/eviction/docs/tests agree; invalid/non-UTF-8 keys remain injective and locale-invariant; PSL engine/default snapshot changes cannot produce stale hits.
8. **Determinism gate.** A required comparable OS/R/locale/charset matrix fails on result divergence, emits full output plus environment/snapshot/revision manifests, triggers for parser/encoding/PSL/cache changes, and is required for curl-free graduation. Non-comparable runner-default cells remain separately labeled.
9. **Oracle integrity gate.** Every normative fixture has immutable provenance, license, hashes/import path, standard/version/section, claim kind, selector applicability, expected comparison, and requirement IDs. Characterization/self-consistency/differential evidence is reported separately. Deviations have owner/status/expiry/v3 disposition.
10. **Migration completeness gate.** All 29 exports and public option values have v2/v3 behavior, change class, adapter/deprecation/removal criteria, examples, tests, and downstream evidence. Known pagerankr, semantic, and rurl-mcp patterns are covered, and a reproducible reverse-dependency inventory identifies any additional consumers.
11. **PSL/engine/chunk gate.** The spec states supported versus unsupported worker/chunk modes, default and explicit snapshot provenance, worker engine initialization, cache scope, stable ordering/warnings, and equivalence checks.
12. **Contradiction closure gate.** Every performance/cache/conformance/migration contradiction is either resolved by an accepted decision or remains in the explicit contradiction register with owner, status, and blocking impact.

## Files, symbols, tests, fixtures, tools, and decisions checked

### Protocol, repository guidance, package metadata, and public docs

- `_scratch/url-v3-spec-reconstruction-protocol.md` — read completely, especially authority/evidence §§3-5, scope §§6.7-6.8, examples §7, spec structure §8, red-team §10, graduation §11, acceptance §12.
- `CLAUDE.md`, `FP_CLAUDE.md`, `ARCHITECTURE.md`, `CONTRIBUTING.md`, `DESCRIPTION`, `NAMESPACE`.
- `README.Rmd`, generated `README.md`, `NEWS.md`, `vignettes/getting-started.Rmd`, `vignettes/url-standard.Rmd`, and cache help.

### Accepted and historical design evidence

- Accepted ADRs 0001 (PSL delegation), 0003 (Stage-A caching), 0005 (intentional base-R deviations), 0007 (URL-standard selector waves), 0009 (libcurl charset shim), 0011 (identity versus presentation), and 0012 (general parser, standard/acceptance separation, compatibility layers, cache key, conformance limits).
- `design/prd/url-standard-selector.md`, `design/prd/url-standard-selector-v2.md`, browser fixer, host-validation, and email/userinfo diagnostics PRDs.
- Read-only `fp context RURL-gxqdmpcp`, including owner decision and comments through 2026-07-20.

### Runtime symbols and seams

- `R/parse.R`: `safe_parse_url()`, `safe_parse_urls()`, `.spu_empty_result()`, `.engine_cache_token()`, `.parse_cache_keys()`, `._safe_parse_url_scalar()`, `._parse_urls_cached()`, Stage A/B boundary.
- `R/parse-phases.R`: `.parse_with_curl()` and path percent-decoding path.
- `R/path-query.R`: `.encode_path_segments()`, `.curl_unescape_vec()`, query filter/serializer seams.
- `R/accessors.R`: query decoding path and component accessor surface.
- `R/email-diagnostics.R`: URL-level and dedicated recipient percent decoding.
- `R/diagnostics.R`: `._url_metadata_vec()` optional metadata path.
- `R/domain.R`: batched Punycode encode/decode, caches, PSL engine/default seams.
- `R/zzz.R`: `.onLoad()`, `.cache_key_ascii()`, `.cache_get*()`, `.cache_set*()`, `rurl_cache_info()`, `rurl_cache_config()`, `rurl_clear_caches()`.

### Tests and fixtures

- `tests/testthat/test-cache-policy.R`, `test-parse-phases.R`, `test-rurl-coverage.R`, `test-external-url-vectors.R`, `test-url-standard-conformance.R`, `test-characterization-fixtures.R`, `test-encoding-fixtures.R`, and related parity/determinism tests discovered by repository search.
- `tests/testthat/fixtures/external-url-vectors.csv`, `rfc3986-abnf.csv`, `url-standard-conformance.csv`, `parse-characterization.csv`, and `encoding-fixtures.csv`.

### Benchmarks, probes, and CI

- `inst/bench/parse-bench.R`, `inst/bench/parser-disagreement.R`.
- `tools/determinism/README.md`, `corpus.R`, `parse-dump.R`, `curl-probe.R`, and `tools/determinism/matrix/`.
- `.github/workflows/determinism-probe.yml`, `.github/workflows/verify.yml`, and `.github/workflows/full-check.yml`.

No implementation code, test, protocol, design source, ADR, Fiberplane state, or Git history was changed in this review. The only deliverable is this evidence report.
