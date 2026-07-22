# S4 audit — cleaning coverage and proposed mutation

## Executive verdict

**Verdict: no-go for product-spec drafting until two protocol-level gates are added.** The reconstruction protocol is strong on authority, evidence, lifecycle separation, URL state, scheme diversity, and adversarial review. It also correctly treats SEO cleaning as intentional and potentially lossy and asks for a new component-aware mutation API. However, its cleaning/mutation paragraph is too compressed to force reconstruction of the behavior that already exists, and it quietly combines two contracts that accepted design records keep separate: lossy presentation cleaning and standards-valid serialization.

The protocol should require two additional artifacts before a v3 product spec can be accepted:

1. a **Cleaning Semantics Matrix** covering every public dial, default, applicability rule, processing position, governed/orthogonal status, lossiness, empty-state behavior, profile override, and downstream key effect; and
2. a **Mutation State and Transaction Matrix** defining component states, operation semantics, overlapping-component dependencies, eligibility, validation standard, vector atomicity, ordering, diagnostics, and rollback.

There is also an owner decision that the protocol must expose explicitly rather than pre-decide: whether `get_clean_url()` is a standards-valid serializer, a possibly non-URL-safe display/SEO projection, or two separately named surfaces. Accepted ADRs and current behavior support the separation; the proposed protocol currently says both “permitted lossy transformations” and “standards-valid output contract” in the same requirement (`_scratch/url-v3-spec-reconstruction-protocol.md:153-164`).

Status labels used below:

- **CURRENT** — behavior implemented and/or asserted by current tests.
- **ACCEPTED** — intent in an accepted ADR/PRD; it may be newer than an older PRD.
- **PROPOSED** — requirement in this reconstruction protocol, not yet a product decision.
- **OPEN** — a v3 owner decision that the protocol should require and record.

## Coverage map

| Concern | Protocol coverage | Audit judgment and evidence | Required protocol correction |
|---|---|---|---|
| Easy SEO cleaning is a product goal | Explicit in §6.4 (`_scratch/url-v3-spec-reconstruction-protocol.md:153-157`) | **Adequate at intent level.** It preserves the distinction between cleaning and generic parsing. | Keep, but bind it to a named eligibility and lossiness matrix. |
| Existing cleaning behavior | “Preserve existing cleaning features” (`_scratch/url-v3-spec-reconstruction-protocol.md:155-157`) | **Incomplete.** Current `get_clean_url()` exposes 25 option formals representing 24 logical controls, profiles, standards conflicts, and a fixed processing order (`R/accessors.R:228-360`, `R/parse.R:1224-1352`, `R/parse.R:1892-2087`). No mandatory inventory deliverable is named. | Require the Cleaning Semantics Matrix and conformance cases for every row. |
| Intentional lossiness | Explicit (`_scratch/url-v3-spec-reconstruction-protocol.md:155-157`) | **Covered but internally unresolved.** Accepted design makes path presentation orthogonal and permits lossy reserved-delimiter folding (`design/adr/0011-path-encoding-orthogonality.md:32-66`; `design/adr/0012-general-scheme-model-and-profile-strategy.md:275-299`). | Add an owner fork: serializer validity versus display/SEO projection, with separate API names if both survive. |
| Standards-valid output | Explicit (`_scratch/url-v3-spec-reconstruction-protocol.md:155-157`) | **Conflicts with unqualified lossiness.** `path_encoding = "decode"` can change component boundaries or make the emitted display string unsuitable as a conforming URL (`man/get_clean_url.Rd:169-192`; `tests/testthat/test-url-standard-path-encoding-orthogonal.R:181-193`). | State which surface guarantees validity, under which standard, and whether incompatible lossy options reject, escape, or move to formatting. |
| Legacy vocabulary | One example: retire `keep` when it really means add-if-absent (`_scratch/url-v3-spec-reconstruction-protocol.md:158`) | **Directionally right, not exhaustive.** `keep`/`none`/`strip` have different semantics across scheme, `www`, slash, query, encoding, and port controls. | Require a complete old-term → actual behavior → v3 verb/default/migration table. |
| Separation from repair, parse, serialize, format | Lifecycle sections are separated (`_scratch/url-v3-spec-reconstruction-protocol.md:142-164,239-264`) | **Good structure, incomplete boundary contract.** Default cleaning currently performs inferred scheme repair; browser profile performs additional fixup (`R/parse.R:913-1013`, `R/parse.R:1892-1929`). | Require a provenance/repair decision for cleaning inputs and a test showing which stages a clean call may invoke. |
| Scheme and host-kind applicability | Global protocol requires arbitrary schemes and an eligibility matrix (`_scratch/url-v3-spec-reconstruction-protocol.md:177-194`) | **Strong cross-cutting coverage, weak binding to cleaning/mutation.** Accepted ADR restricts automatic semantic transforms to HTTP(S)+explicit SEO while allowing serialization for broader forms (`design/adr/0012-general-scheme-model-and-profile-strategy.md:275-299`). | Require one eligibility cell for every clean dial and mutation operation across scheme and host/path kinds. |
| Empty/present state | Global object-model requirements cover absence and emptiness (`_scratch/url-v3-spec-reconstruction-protocol.md:121-140`) | **Good foundation, not operationalized for mutation.** `add/set/replace/remove/preserve` are undefined for absent versus present-empty authority, query, fragment, and path. | Bind the mutation matrix to the state taxonomy and require empty-state examples. |
| Profiles and standards | Covered globally in standards/profile work and examples (`_scratch/url-v3-spec-reconstruction-protocol.md:196-237`) | **Partial for cleaning/mutation.** Current profiles fill only unspecified options and explicit options create customized profiles (`R/accessors.R:308-360`, `R/profiles.R:1-88`). | Require precedence, conflict, and result-label rules for both cleaning and mutation. |
| Mutation component coverage | Names scheme, credentials, authority, hostname, subdomains, registered domain, suffix, port, path, query, fragment (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`) | **Broad enumeration, not an executable model.** Several named components overlap or are PSL-derived. No current mutation API exists. | Require a component dependency graph and per-operation state transitions before API naming. |
| Transactional mutation | Explicit phrase “transactional validation” (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`) | **Insufficiently defined.** Per-row versus whole-vector atomicity, operation order, rollback, recycling, and diagnostic behavior are open. | Add a transaction contract decision table and failure examples. |
| Redirects and identity consumers | Trace matrix calls out redirect mapping; downstream joins/keys are included (`_scratch/url-v3-spec-reconstruction-protocol.md:108-116,165-175`) | **Partial.** Current canonical join treats `clean_url` as its key and demonstrably false-merges lossy paths (`R/canonical_join.R:238-247`; `tests/testthat/test-url-standard-path-encoding-orthogonal.R:204-216`). | Require every clean projection to declare whether it is display-only, routing-safe, comparison-safe, or redirect-safe. |
| Verification and red team | Evidence, example, transition, edge-case, and red-team gates are strong (`_scratch/url-v3-spec-reconstruction-protocol.md:206-237,278-334`) | **Adequate framework.** It will only catch the issues if the two matrices are made required artifacts. | Add matrix completeness and decision closure to acceptance gates. |

## Complete current cleaning dial inventory

The public signature has 25 option arguments after `url`; `source` and deprecated `tld_source` are two formals for one logical control (`R/accessors.R:228-254`). The following is the complete current public inventory, not a proposed v3 API.

| # | CURRENT dial | Values/default | Current meaning and important interaction | Evidence |
|---:|---|---|---|---|
| 1 | `protocol_handling` | `keep`, `none`, `strip`, `http`, `https`; default `keep` | `keep` preserves an explicit scheme **or adds inferred `http` when absent**; `none` preserves explicit but leaves absent absent; `strip` omits; forced values replace/add. This is presentation after input scheme inference, not parse acceptance. | `man/get_clean_url.Rd:38-59`; `R/parse-phases.R:1561-1573`; `tests/testthat/test-accessors.R:1-99` |
| 2 | `www_handling` | `none`, `strip`, `keep`, `if_no_subdomain`; default `none` | `none` means do nothing; `strip` removes `www`/`www[digits]`; `keep` ensures one exists; conditional mode depends on PSL-derived subdomain state. | `man/get_clean_url.Rd:61-77`; `R/parse-phases.R:1882-1971`; `tests/testthat/test-accessors.R:810-870` |
| 3 | `source` | `all`, `private`, `icann`; default `all` | Chooses suffix rules and therefore registered domain/suffix/subdomain boundaries; affects `www` conditional behavior and subdomain truncation. | `man/get_clean_url.Rd:79-81`; `R/parse-phases.R:1979-2040` |
| 4 | `tld_source` | deprecated alias, default `NULL` | Compatibility alias for `source`; conflicts/normalization are handled before parsing. It must be in a migration table even if removed in v3. | `R/accessors.R:228-281`; `R/parse.R:1224-1352` |
| 5 | `case_handling` | `lower_host`, `keep`, `lower`, `upper`; default `lower_host` | Host-only lowercasing is default. Whole-string lower/upper transforms are broader; query is intentionally exempt. Standard bundles can govern allowed/default behavior. | `man/get_clean_url.Rd:83-94`; `R/parse-phases.R:2173-2253`; `tests/testthat/test-url-standard-case-handling.R:8-105` |
| 6 | `trailing_slash_handling` | `none`, `keep`, `strip`; default `none` | `keep` ensures a slash; `strip` removes it except the root form; runs after path identity/normalization and index stripping. | `man/get_clean_url.Rd:96-106`; `R/parse-phases.R:1502-1524` |
| 7 | `index_page_handling` | `keep`, `strip`; default `keep` | Removes recognized terminal index pages and produces a directory path before trailing-slash policy. | `man/get_clean_url.Rd:108-114`; `R/parse-phases.R:1502-1524` |
| 8 | `path_normalization` | `none`, `collapse_slashes`, `dot_segments`, `both`; default `none` | Applies after standard-specific path identity and before index/trailing/encoding. It is governed by selected standard/profile conflict rules. | `man/get_clean_url.Rd:116-130`; `R/parse-phases.R:1383-1545`; `R/parse.R:1083-1201` |
| 9 | `scheme_relative_handling` | `keep`, `http`, `https`, `error`; default `keep` | Controls `//host/path` input; profile bundles may force resolve/error behavior. This is input interpretation, despite living on the cleaning accessor. | `man/get_clean_url.Rd:132-140`; `R/parse.R:913-1013` |
| 10 | `subdomain_levels_to_keep` | `NULL` or nonnegative integer; default `NULL` | Retains the rightmost N PSL-derived subdomain labels while preserving `www` separately; requires a domain-like host and suffix derivation. | `man/get_clean_url.Rd:142-158`; `R/parse-phases.R:2042-2113`; `tests/testthat/test-accessors.R:1091-1157` |
| 11 | `host_encoding` | `keep`, `idna`, `unicode`; default `keep` | Presentation conversion for eligible domain hosts; not a generic transform for IPv4, IPv6, opaque, empty, or absent hosts. | `man/get_clean_url.Rd:160-167`; `R/parse-phases.R:2124-2165`; `tests/testthat/test-accessors.R:929-972` |
| 12 | `path_encoding` | `keep`, `encode`, `decode`; default `keep` | Orthogonal presentation control. `keep` still normalizes percent-escape hex case; encode/decode can fold encoded reserved delimiters and are intentionally lossy. | `man/get_clean_url.Rd:169-192`; `R/parse-phases.R:1383-1545`; `design/adr/0011-path-encoding-orthogonality.md:32-93`; `tests/testthat/test-url-standard-path-encoding-orthogonal.R:7-85,181-216` |
| 13 | `query_handling` | `drop`, `filter`, `allow`, `keep`; default `drop` | `keep` is canonical reserialization, not byte preservation; `filter`/`allow` combine with pattern lists; query processing is separate from case conversion. | `man/get_clean_url.Rd:194-219`; `R/path-query.R:351-541`; `tests/testthat/test-query-clean-url.R:9-53` |
| 14 | `params_keep` | character glob vector or `NULL` | Rescue/allow list. In filtering precedence, a match can rescue a key before empty/drop rules. | `man/get_clean_url.Rd:221-241`; `R/path-query.R:426-460`; `tests/testthat/test-query-clean-url.R:63-123` |
| 15 | `params_drop` | character glob vector or `NULL` | Deny/drop list. Matching operates on decoded keys with configurable case sensitivity. | `man/get_clean_url.Rd:221-241`; `R/path-query.R:402-460`; `tests/testthat/test-query-clean-url.R:63-123` |
| 16 | `params_case_sensitive` | logical scalar; default `FALSE` | Controls key-pattern matching only, not output key/value case. | `man/get_clean_url.Rd:221-241`; `R/path-query.R:402-460` |
| 17 | `sort_params` | logical scalar; default `FALSE` | Stable ordering by decoded key/value; duplicates remain ordered consistently rather than becoming a map. | `man/get_clean_url.Rd:194-219`; `R/path-query.R:462-505` |
| 18 | `empty_param_handling` | `keep`, `drop`; default `keep` | Drops or preserves query entries with empty values, subject to the keep-list rescue precedence. | `man/get_clean_url.Rd:194-241`; `R/path-query.R:426-460` |
| 19 | `decode_plus` | logical scalar; default `FALSE` | Chooses whether `+` is treated as space while decoding query fields; canonical output percent-encodes afterward. | `man/get_clean_url.Rd:194-219`; `R/path-query.R:351-400,508-541` |
| 20 | `port_handling` | `exclude`, `keep`, `strip_default`, `strip_all`; default `exclude` | `exclude` omits all ports. `keep` is a literal explicit override even under WHATWG; `strip_default` applies scheme-aware defaults; `strip_all` is an alias-like explicit removal. | `man/get_clean_url.Rd:243-255`; `R/parse-phases.R:2303-2363`; `tests/testthat/test-url-standard-port-handling.R:10-32,141-240` |
| 21 | `scheme_policy` | `infer`, `require`; default `infer` | Controls preprocessing of schemeless input. It is distinct from `protocol_handling`, `scheme_acceptance`, and `url_standard`. | `man/get_clean_url.Rd:257-270`; `design/adr/0010-scheme-inference-policy.md:19-77,104-109`; `R/parse.R:913-1013` |
| 22 | `scheme_acceptance` | `web`, `general`; default `web` | Selects admissible scheme/URL shapes. `general` does not imply that every SEO transform is semantically eligible. | `man/get_clean_url.Rd:272-287`; `R/parse.R:1920-1958`; `design/adr/0012-general-scheme-model-and-profile-strategy.md:275-299` |
| 23 | `url_standard` | `NULL`, `rfc3986`, `whatwg`; default `NULL` | Selects governing parsing/identity rules. Governed standalone options can conflict; orthogonal presentation options remain independently selectable. | `man/get_clean_url.Rd:289-310`; `R/parse.R:1083-1201`; `design/adr/0007-url-standard-selector.md:18-54` |
| 24 | `engine` | PSL engine or `NULL`; default `NULL` | Selects suffix source/version mechanics used for domain decomposition. It affects cleaning semantics that operate on `www`, subdomains, registered domain, and suffix. | `man/get_clean_url.Rd:312-323`; `R/parse-phases.R:1979-2113` |
| 25 | `profile` | `NULL`, `browser`, `whatwg`, `rfc-syntax`, `seo`, `canonical`; default `NULL` | Expands a bundle only into unspecified controls; caller-supplied values win and the result becomes customized. `canonical` aliases `seo`. | `man/get_clean_url.Rd:325-340`; `R/accessors.R:308-360`; `R/profiles.R:1-88`; `tests/testthat/test-url-profiles.R:4-178` |

Two internal/profile-resolved controls are also semantically important and must not disappear from reconstruction merely because they are not public formals:

- `fixup_posture` (`none`/`browser`) changes Stage A input repair (`R/parse.R:913-1013`, `R/parse.R:1892-1929`; `tests/testthat/test-url-profiles.R:90-100`).
- `path_identity` (`none`/`rfc`/`whatwg`) changes path identity before presentation normalization (`R/parse.R:913-1013`; `R/parse-phases.R:1383-1500`).

## Current interaction and ordering inventory

The product specification cannot safely reconstruct the dials independently. These are the current or accepted interactions that the protocol should force into one matrix:

1. **Resolution precedence — CURRENT/ACCEPTED.** A profile supplies defaults only for arguments the caller did not explicitly provide; explicit options win, standard-governed conflicts are validated, and the profile becomes customized (`R/accessors.R:255-360`, `R/parse.R:1015-1201`; `design/adr/0012-general-scheme-model-and-profile-strategy.md:519-527`).
2. **Stage boundary — ACCEPTED/CURRENT.** Stage A performs repair/fixup and structural parsing; Stage B performs cleaning/presentation (`design/adr/0003-two-stage-parse-architecture.md:17-36`; `R/parse.R:1892-2087`). A clean call currently invokes both.
3. **Input scheme versus output scheme — CURRENT/ACCEPTED.** `scheme_policy` decides whether missing schemes may be inferred; `scheme_acceptance` decides admitted shapes; `url_standard` governs interpretation; `protocol_handling` decides emitted presentation. The four are not aliases (`design/adr/0010-scheme-inference-policy.md:19-77`; `R/parse-phases.R:1561-1573`).
4. **Applicability — ACCEPTED/CURRENT.** Accepted intent allows structural serialization of arbitrary admitted schemes but limits automatic SEO semantic transforms to eligible HTTP(S) rows under explicit posture (`design/adr/0012-general-scheme-model-and-profile-strategy.md:275-299`). Current Stage B computes row eligibility before path, subdomain, host, case, query, and port work (`R/parse.R:1920-2056`) and replaces `clean_url` with a general serializer for general-routed rows (`R/parse.R:2064-2087`).
5. **PSL dependency — CURRENT.** `source`/`engine` determine suffix decomposition, which then affects conditional `www` and subdomain truncation. `www` is handled before retained subdomain levels, with `www` tracked separately (`R/parse-phases.R:1882-2113`).
6. **Path order — CURRENT.** Standard-specific identity precedes explicit path normalization; index stripping then precedes trailing-slash behavior; path encoding/decoding is last (`R/parse-phases.R:1383-1545`). Changing this order changes results.
7. **Path lossiness — ACCEPTED/CURRENT.** Percent-encoded reserved delimiters can collapse with literal delimiters under decode; presentation choice is orthogonal to RFC/WHATWG parsing (`design/adr/0011-path-encoding-orthogonality.md:32-93`; `tests/testthat/test-url-standard-path-encoding-orthogonal.R:181-216`).
8. **Query representation — CURRENT.** The query engine is an ordered sequence, not a map. Bare `a` and `a=` canonicalize to the same emitted `a=`; malformed percent bytes use a byte-preserving path; keep-list rescue precedes empty/drop rules; sorting is stable; output is re-encoded (`R/path-query.R:351-541`).
9. **Query isolation — CURRENT.** Whole-URL case controls do not case-fold query data (`tests/testthat/test-query-clean-url.R:127-140`). Query configuration must not be specified as a generic string transform.
10. **Port override — CURRENT.** Explicit `port_handling = "keep"` overrides WHATWG default-port omission; `strip_default` is the behavior that performs scheme-aware removal (`tests/testthat/test-url-standard-port-handling.R:141-240`).
11. **Empty/presence fidelity — CURRENT.** General serializers preserve distinctions such as absent versus present-empty query and authority states, while the legacy SEO clean builder emits only nonempty query and omits userinfo and fragment (`R/parse-phases.R:2365-2407,2422-2608`; `tests/testthat/test-parse-serializers.R:39-147`).
12. **Vector/error shape — CURRENT.** Public accessors take character vectors, reject parsed objects, and return per-row missing output for parse failures through the common extractor (`R/accessors.R:27-107,228-307`; `tests/testthat/test-accessor-helper.R:1-78`; `tests/testthat/test-accessors.R:1202-1230`). Option values are generally scalar except pattern vectors.
13. **Downstream identity — CURRENT.** `canonical_join()` takes `clean_url` as the exact key (`R/canonical_join.R:238-247`). Lossy path decoding can therefore merge distinct source URLs (`tests/testthat/test-url-standard-path-encoding-orthogonal.R:204-216`).
14. **Host policy separation — ACCEPTED/CURRENT.** Parsing/serialization success and application host allow/deny policy are separate layers; a policy failure is not a parse failure (`R/domain.R:280-365` `.validate_idna_domain_vec()`/`.psl_registered_domain()`/`.psl_public_suffix()`; `tests/testthat/test-host-policy.R:202-212`). Mutation validation must preserve that distinction.

## Severity findings

### F1 — HIGH — The protocol does not require a complete cleaning semantics artifact

- **Protocol section:** §6.4, especially “preserve existing cleaning features” (`_scratch/url-v3-spec-reconstruction-protocol.md:153-159`).
- **Exact evidence:** The current public surface has the 25 formals inventoried above (`R/accessors.R:228-254`), while behavior is distributed across option resolution (`R/parse.R:1015-1352`), ordered Stage B execution (`R/parse.R:1892-2087`), path phases (`R/parse-phases.R:1383-1545`), host transforms (`R/parse-phases.R:1882-2253`), query (`R/path-query.R:351-541`), and port/assembly (`R/parse-phases.R:2303-2407`). The protocol’s artifact ledger does not name a cleaning inventory (`_scratch/url-v3-spec-reconstruction-protocol.md:82-106`).
- **Minimal example:** For `https://Example.com/a//b/index.html?A&b=1`, combining `case_handling="lower"`, `path_normalization="collapse_slashes"`, `index_page_handling="strip"`, `trailing_slash_handling="strip"`, and `query_handling="keep"` is order-sensitive and query case is exempt.
- **Consequence:** A writer can satisfy the prose while silently dropping defaults, interactions, aliases, or processing order; tests then encode accidental behavior rather than an accepted contract.
- **Concrete correction:** Add a required **Cleaning Semantics Matrix** to §4 and §6.4. Require a row per public/internal semantic control with: accepted name, legacy name, values, default, eligible URL kinds, order, governed/orthogonal classification, lossiness, validity guarantee, absent/empty behavior, profile behavior, diagnostics, and mandatory examples/tests. Make matrix completeness an acceptance gate in §12.

### F2 — HIGH / DESIGN FORK — Lossy cleaning and standards-valid output are conflated

- **Protocol section:** §6.4 says both “permitted lossy transformations” and “a standards-valid output contract” (`_scratch/url-v3-spec-reconstruction-protocol.md:153-157`).
- **Exact evidence:** Accepted ADR 0011 defines `path_encoding` as orthogonal presentation and explicitly accepts lossy folding of encoded reserved delimiters (`design/adr/0011-path-encoding-orthogonality.md:32-93`). ADR 0012 says explicit encode/decode may make `clean_url` nonconformant and must not silently change the serializer’s standard contract (`design/adr/0012-general-scheme-model-and-profile-strategy.md:275-299,354-369`). Current docs warn that decoded output is presentation-oriented and lossy (`man/get_clean_url.Rd:169-192`).
- **Minimal example:** `/a%2Fb` and `/a/b` become the same displayed path under decode; decoding `%23` can introduce a literal `#` whose reparsing changes path text into a fragment boundary.
- **Consequence:** The v3 writer cannot tell whether to reject lossy options, re-escape their output, relax the validity promise, or split the surface. Any implicit choice contradicts either the proposed sentence or accepted design.
- **Concrete correction:** Replace the single combined requirement with an explicit owner decision and two separately specified contracts: **standard serializer** (always valid under the selected standard, component-preserving) and **clean/display projection** (declared transformations and lossiness, not automatically reparsable or identity-safe). If v3 keeps one function, require a decision table for incompatible combinations and their error behavior.

### F3 — HIGH — Cleaning’s repair provenance is not resolved

- **Protocol section:** lifecycle separation in §§6.1–6.4 and the red-team concern that cleaning not become implicit repair (`_scratch/url-v3-spec-reconstruction-protocol.md:142-164,278-305`).
- **Exact evidence:** Current `scheme_policy="infer"` is the default cleaning behavior (`man/get_clean_url.Rd:257-270`). `protocol_handling="keep"` can therefore add `http` to a schemeless input (`man/get_clean_url.Rd:38-59`). The browser profile additionally enables browser fixup (`R/parse.R:913-1013`; `tests/testthat/test-url-profiles.R:90-100`). Accepted ADR 0010 separates input inference from output presentation (`design/adr/0010-scheme-inference-policy.md:19-77,104-109`).
- **Minimal example:** `get_clean_url("example.com/a")` can emit `http://example.com/a` even though the presentation value is named `keep`.
- **Consequence:** The reconstructed contract may describe cleaning as projection over a parsed object while preserving a character-vector API that repairs before projecting, without disclosing which repair occurred. Provenance and error semantics become ambiguous.
- **Concrete correction:** Require every cleaning entry point to declare accepted input type, whether Stage A inference/fixup runs, how repairs are surfaced, and whether already-parsed objects are accepted. Add paired cases for schemeless input under default, strict, browser, and pre-parsed modes.

### F4 — HIGH — Per-scheme and per-shape cleaning/mutation eligibility is not mandatory

- **Protocol section:** §6.4 names cleaning and mutation, while scheme/host eligibility is only a cross-cutting requirement in §6.6 (`_scratch/url-v3-spec-reconstruction-protocol.md:153-164,177-194`).
- **Exact evidence:** Accepted ADR 0012 distinguishes serializability from semantic-transform eligibility across scheme, authority, host, and path families (`design/adr/0012-general-scheme-model-and-profile-strategy.md:228-310`). Current Stage B guards transformations and separately routes general serialization (`R/parse.R:1920-2087`). Host encoding, PSL subdomain operations, and domain/suffix mutation are nonsensical for several host kinds (`R/parse-phases.R:1979-2165`).
- **Minimal example:** For `mailto:person@example.com?utm_source=x`, should `query_handling="drop"` be a generic component removal, an SEO operation that is ineligible, or a no-op? For `[::1]`, what could “set registered domain” mean?
- **Consequence:** Implementations can disagree between reject/no-op/apply, and a successful serializer can be mistaken for proof that an SEO or host-derived transform was semantically valid.
- **Concrete correction:** In §6.4, require the eligibility matrix to cross every cleaning dial and mutation operation with scheme family, authority kind, host kind, and hierarchical/opaque/list path kind. Every cell must say apply, no-op, reject, or unsupported and name its diagnostic.

### F5 — MEDIUM — The vocabulary audit is scoped to one misleading `keep`

- **Protocol section:** legacy vocabulary sentence in §6.4 (`_scratch/url-v3-spec-reconstruction-protocol.md:157-159`).
- **Exact evidence:** Scheme `keep` may add inferred `http` (`man/get_clean_url.Rd:38-59`); `www` `keep` ensures/adds a prefix (`man/get_clean_url.Rd:61-77`); trailing slash `keep` ensures a slash (`man/get_clean_url.Rd:96-106`); query `keep` parses and canonically reserializes (`R/path-query.R:351-541`); path encoding `keep` normalizes percent hex case (`R/parse-phases.R:1460-1467`); port `keep` is a literal standards override (`tests/testthat/test-url-standard-port-handling.R:141-184`). `none` also means different things by dial.
- **Minimal example:** `protocol_handling="keep"` adds a component, while `query_handling="keep"` can rewrite `?a` to `?a=`.
- **Consequence:** Renaming one value leaves the same semantic ambiguity elsewhere and makes symmetry with mutation verbs misleading.
- **Concrete correction:** Require an exhaustive vocabulary ledger for every enum value: observed current behavior, proposed verb, whether it preserves bytes/state/meaning, compatibility alias, warning lifecycle, and migration example. Prefer precise verbs such as `preserve_if_present`, `ensure`, `omit`, `canonicalize`, and `retain_explicit` where those are the accepted semantics.

### F6 — HIGH — Mutation names overlapping and derived components without dependency rules

- **Protocol section:** proposed `mutate_url()` component list (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`).
- **Exact evidence:** `authority` contains credentials, host, and port; `hostname` overlaps registered domain, suffix, and subdomains; registered domain/suffix boundaries depend on PSL source/engine (`R/parse-phases.R:1979-2113`). The accepted object model distinguishes absent, empty, domain, IPv4, IPv6, and opaque host states (`design/adr/0012-general-scheme-model-and-profile-strategy.md:228-282`). Repository search found no current `mutate_url` or component setter implementation to supply inherited semantics.
- **Minimal example:** In one transaction, `set(hostname="a.example.com")` plus `set(suffix="org")` could yield `a.example.org`, reject as competing writes, or depend on argument order. “Set suffix” for `127.0.0.1` has no obvious meaning.
- **Consequence:** “Symmetric and predictable” is not testable. Operation order can accidentally overwrite sibling writes or recompute PSL-derived values differently.
- **Concrete correction:** Require a component dependency graph and a mutation capability table before API design. Classify fields as stored, structural, or derived; define legal parent/child co-writes; say when derived values are recomputed; and specify reject/no-op semantics per host kind.

### F7 — HIGH — “Transactional validation” has no transaction contract

- **Protocol section:** final mutation sentence (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`).
- **Exact evidence:** Current public operations are vectorized and report failures per row through the accessor helper (`R/accessors.R:27-107`), while option arguments are resolved globally (`R/accessors.R:255-360`). Neither the protocol nor an existing mutation implementation defines atomicity, recycling, ordering, rollback, or multi-error diagnostics.
- **Minimal example:** Mutating two rows with `set(scheme=c("https","bad scheme"))` and `remove(port=TRUE)`: does the valid row commit, does the whole vector roll back, and are component operations validated against original or intermediate state?
- **Consequence:** Two reasonable implementations can both claim to be transactional while returning different data and diagnostics. Partial mutation is particularly dangerous in redirects or persisted canonical identifiers.
- **Concrete correction:** Add a Mutation Transaction Matrix that decides: atomic unit (row/vector/object), simultaneous versus ordered application, scalar recycling, length mismatch, validation phase, rollback object, multi-error collection, original-value preservation, and diagnostic schema. Require one whole-vector failure and one per-row failure example.

### F8 — MEDIUM — Mutation verbs are not bound to absent and present-empty states

- **Protocol section:** `add`, `set`, `replace`, `remove`, `preserve` vocabulary in §6.4 (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`) versus global state requirements in §§5–6 (`_scratch/url-v3-spec-reconstruction-protocol.md:121-140`).
- **Exact evidence:** Current general serialization distinguishes absent from present-empty host, authority, query, and fragment states (`R/parse-phases.R:2422-2608`; `tests/testthat/test-parse-serializers.R:39-147`). The legacy clean builder drops userinfo/fragment and only emits a nonempty query (`R/parse-phases.R:2365-2407`).
- **Minimal example:** For `https://example.com/path?`, does `add(query="a=1")` require absence and therefore fail, append after an empty query, or behave like `set`? Does `remove(query)` remove the delimiter state or leave a present-empty query?
- **Consequence:** Mutation destroys distinctions the protocol elsewhere says v3 must preserve, and verb symmetry becomes cosmetic.
- **Concrete correction:** Require a state-transition table for every verb over absent, present-empty, and present-nonempty component states. Include empty authority, host, path, query, and fragment examples, plus a rule for whether delimiter presence is separately addressable.

### F9 — MEDIUM — Query mutation lacks an ordered multimap and encoding contract

- **Protocol section:** mutation includes query but provides no query-specific semantics (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`).
- **Exact evidence:** The current query engine preserves ordered duplicates, distinguishes parsing details before canonical output, collapses bare/empty-value syntax, supports malformed percent bytes, applies keep/drop precedence, optionally decodes plus, and stably sorts (`R/path-query.R:351-541`; `tests/testthat/test-query-clean-url.R:9-231`).
- **Minimal example:** Starting with `?a=1&a=2&b&x=%ZZ`, does “replace `a`” replace the first pair, all pairs, or a pair at an index? Does “add `b`” preserve bare `b` or emit `b=`? Does matching see raw or decoded keys?
- **Consequence:** A map-shaped API loses duplicates/order, and a string-shaped API can introduce double encoding or silently normalize malformed data.
- **Concrete correction:** Require an ordered query-pair data model with explicit raw/decoded views, duplicate targeting, pair versus key operations, insertion position, bare versus empty-value policy, plus handling, malformed-percent policy, and encode-on-commit rules. Include duplicate, empty, plus, Unicode, and malformed-byte cases.

### F10 — MEDIUM — Path mutation lacks a structured/opaque and encoded-delimiter contract

- **Protocol section:** mutation names `path`; path/url state is covered elsewhere but not connected (`_scratch/url-v3-spec-reconstruction-protocol.md:121-140,160-164`).
- **Exact evidence:** Accepted ADR 0012 has hierarchical, opaque, list-like, absent, and empty path families (`design/adr/0012-general-scheme-model-and-profile-strategy.md:228-282`). Current path processing has a strict order and lossy encoding option (`R/parse-phases.R:1383-1545`); accepted ADR 0011 treats encoding as orthogonal presentation (`design/adr/0011-path-encoding-orthogonality.md:32-93`).
- **Minimal example:** Replacing segment 2 in `/a%2Fb/c` after decoding sees either two or three segments. A segment operation on `mailto:` or `urn:` cannot assume slash hierarchy.
- **Consequence:** Mutation can change structure merely by choosing a presentation view, or apply hierarchical operations to opaque data.
- **Concrete correction:** Require separate whole-path and hierarchical-segment operations, define the canonical internal view used for edits, prohibit segment operations on non-hierarchical paths unless explicitly supported, and state normalization/encoding order at commit.

### F11 — MEDIUM — Standards/profile precedence is not specified for mutation or the new clean contract

- **Protocol section:** cleaning/mutation §6.4 and standards/profiles §6.7 are separate (`_scratch/url-v3-spec-reconstruction-protocol.md:153-164,196-205`).
- **Exact evidence:** Current profiles bundle scheme policy, acceptance, standard, fixup, and cleaning options (`R/parse.R:913-1013`; `tests/testthat/test-url-profiles.R:4-140`). Explicit caller options override bundle values and result in a customized profile (`R/accessors.R:308-360`; `design/adr/0012-general-scheme-model-and-profile-strategy.md:519-527`). Standard-governed conflicts are explicitly checked (`R/parse.R:1083-1201`).
- **Minimal example:** Under `profile="seo"`, does `mutate_url(..., set_scheme="http")` override the profile’s HTTPS presentation, customize the profile, or fail validation? Does a WHATWG mutation of default port preserve an explicit `:80` if the caller asks to keep it?
- **Consequence:** Profile names can misleadingly remain attached to objects that no longer satisfy the named bundle, and standard validation can occur against the wrong pre/post-mutation state.
- **Concrete correction:** Require one shared precedence algorithm: profile expansion, explicit override, mutation application, standard validation, presentation/serialization, and result labeling. Add standard-conflict and customized-profile examples for both clean and mutate flows.

### F12 — MEDIUM — Credential mutation lacks a security/display requirement

- **Protocol section:** credentials are explicitly mutable (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`), while the red-team list omits credential leakage (`_scratch/url-v3-spec-reconstruction-protocol.md:278-305`).
- **Exact evidence:** The legacy clean builder omits userinfo (`R/parse-phases.R:2365-2407`), while general serializers can preserve authority state (`R/parse-phases.R:2422-2608`). The proposed API would make credentials easier to create or change without specifying print, log, diagnostic, or redirect behavior.
- **Minimal example:** `mutate_url("https://example.com", set_credentials="user:secret")` followed by an error diagnostic or object print could expose `secret`.
- **Consequence:** Secrets can leak in console output, snapshots, errors, logs, or redirect tables even if structural mutation is correct.
- **Concrete correction:** Add a security row requiring credential redaction in printing and diagnostics, an explicit opt-in for credential serialization, tests proving no secret appears in failures, and a decision whether cleaning always removes credentials.

### F13 — MEDIUM — Cleaning outputs are not classified for downstream identity and redirect use

- **Protocol section:** cleaning must preserve redirect mapping; keys/joins are handled in §§6.5 and traceability (`_scratch/url-v3-spec-reconstruction-protocol.md:108-116,153-175`).
- **Exact evidence:** `canonical_join()` currently keys directly on `clean_url` (`R/canonical_join.R:238-247`). The test suite proves distinct encoded/literal paths false-merge when decoded (`tests/testthat/test-url-standard-path-encoding-orthogonal.R:204-216`). README describes `clean_url` as a canonical key while also documenting human-readable lossy decoding (`README.md:96-104,148-161`).
- **Minimal example:** Source rows for `/a%2Fb` and `/a/b` collapse to one join key under decoded path presentation.
- **Consequence:** A display-friendly clean value can corrupt joins, deduplication, caches, and redirect targets if the new spec continues to call it canonical without a safety classification.
- **Concrete correction:** Require each produced URL string/key to declare capabilities: reparsable, standards-valid, semantic-identity-safe, comparison-safe, routing-safe, redirect-safe, and display-only. Prohibit display-only projections as implicit join keys and require collision diagnostics where lossy keys remain opt-in.

### F14 — MEDIUM — PSL source/version is an unowned input to domain mutation

- **Protocol section:** suffix interpretation/version is listed as an owner decision globally (`_scratch/url-v3-spec-reconstruction-protocol.md:191-194`), but mutation proposes registered-domain and suffix writes without binding that decision (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`).
- **Exact evidence:** Current suffix, registered domain, and subdomain values are derived via `source` and `engine` (`R/parse-phases.R:1979-2113`). `www_handling="if_no_subdomain"` also consumes those results (`R/parse-phases.R:1882-1971`).
- **Minimal example:** Changing the suffix of `a.blogspot.com` depends on whether private suffix rules are enabled; the same labels decompose differently under ICANN-only versus private-inclusive sources.
- **Consequence:** Mutations and redirect mappings can change when the suffix dataset or source changes, without the URL text or mutation call changing.
- **Concrete correction:** Require domain-derived mutation to carry or explicitly select PSL source/version, define unknown-suffix behavior, record the source in diagnostics/provenance, and test one private-suffix and one unknown-suffix case.

### F15 — MEDIUM — Mutation has no vector and error-recovery contract

- **Protocol section:** proposed mutation surface (`_scratch/url-v3-spec-reconstruction-protocol.md:160-164`).
- **Exact evidence:** Current accessor calls are vectorized and preserve row alignment on failures (`R/accessors.R:27-107`); current accessor tests also enforce input-type boundaries (`tests/testthat/test-accessor-helper.R:1-78`; `tests/testthat/test-accessors.R:1202-1230`). The protocol’s general error section asks for structured diagnostics but does not bind it to mutation atomicity (`_scratch/url-v3-spec-reconstruction-protocol.md:266-276`).
- **Minimal example:** Three input URLs, two replacement hosts, and one invalid port can be rejected for length, recycled, partially committed, or returned with row diagnostics.
- **Consequence:** Data-frame pipelines cannot predict row alignment or determine which value is original versus mutated after partial failure.
- **Concrete correction:** Add explicit vectorization rules for URLs, operations, and component values; forbid accidental recycling or define it narrowly; require stable row identifiers and original/mutated values in structured diagnostics; and connect these rules to F7’s transaction scope.

## Mutation gaps and open owner decisions

No current mutation API was found in `R/`; the following are **OPEN** v3 decisions, not descriptions of present behavior. The protocol should require owners and closure states for each before accepting the product spec.

| ID | Open owner decision | Minimum decision record |
|---|---|---|
| M1 | Input and result type | Character vector, parsed object, immutable copy, mutable builder, or multiple surfaces; provenance retained or discarded. |
| M2 | Operation vocabulary/API shape | Whether `add/set/replace/remove/preserve` are universal verbs, component-specific operations, patch objects, or a single declarative target-state API. |
| M3 | Transaction unit | Atomic per row, per vector, or per object; rollback and returned original on failure. |
| M4 | Operation ordering | Simultaneous patch against original state versus ordered edits; how conflicting parent/child writes are detected. |
| M5 | Vectorization | Recycling, length mismatch, row naming, scalar options, and multiple operations per row. |
| M6 | Component dependency | Stored versus derived fields; authority/credentials/host/port overlap; hostname/subdomain/registered-domain/suffix overlap. |
| M7 | Eligibility | Apply/no-op/reject/unsupported for every operation by scheme, authority, host, and path kind. |
| M8 | Empty states | Semantics for absent, present-empty, and nonempty components, including delimiter-presence state. |
| M9 | Query model | Ordered pairs, duplicates, bare keys, empty values, raw/decoded matching, insertion position, plus handling, malformed encoding, and commit encoding. |
| M10 | Path model | Whole opaque path versus hierarchical segments, encoded delimiter identity, dot normalization, slash policy, and commit encoding order. |
| M11 | Domain/PSL behavior | Source/version, private suffixes, unknown suffixes, IP/opaque hosts, recomputation, and suffix-write legality. |
| M12 | Standard used for validation | Input standard, explicit output standard, profile-selected standard, or a separate mutation validator; conflict and incompatible-lossy behavior. |
| M13 | Profile interaction | Expansion/override order, customized labeling, whether mutations can violate a named profile, and whether profile cleanup runs automatically after mutation. |
| M14 | Repair interaction | Whether mutation accepts invalid/repaired input, can invoke browser fixup or scheme inference, and how repair provenance is exposed. |
| M15 | Diagnostics | Stable error codes, multiple-error accumulation, component location, original/intermediate/final values, and warnings for lossy changes. |
| M16 | Credential safety | Redacted printing/errors, serialization opt-in, storage lifetime, and cleaning behavior. |
| M17 | Invariants | Idempotence where claimed, serialize/parse round-trip, preservation of untouched components, and collision detection for lossy projections. |
| M18 | Downstream safety | Whether outputs are comparison-, routing-, redirect-, and join-safe; which APIs reject display-only values. |

## Recommended protocol edits

1. Add these mandatory ledger artifacts in §4: `Cleaning Semantics Matrix`, `Mutation State and Transaction Matrix`, `Legacy Cleaning Vocabulary Migration`, and `Output Capability Classification`.
2. Expand §6.4 into separate cleaning and mutation subsections. Make the current dial inventory, processing order, applicability, empty states, profiles, standards, repair provenance, and downstream safety required fields rather than optional prose.
3. Mark the lossy-cleaning/standards-valid-output question as an explicit **design fork** with an owner and status. Do not let the product-spec writer resolve it silently.
4. Bind §6.6’s scheme/host eligibility matrix and §6.9’s diagnostic taxonomy directly to every cleaning dial and mutation operation.
5. Add acceptance gates in §12 requiring: all current dials mapped or explicitly retired; every mutation state transition decided; no unowned open cells; conformance examples for ordering and empty states; collision tests for lossy projections; and credential-redaction tests.

## Files, symbols, and tests checked

### Protocol and repository guidance

- `_scratch/url-v3-spec-reconstruction-protocol.md` — full file, especially §§4–6 and §§9–12.
- `CLAUDE.md` — full file.
- `FP_CLAUDE.md` — full file.
- `ARCHITECTURE.md` — full file.

### Public documentation

- `man/get_clean_url.Rd:7-365` — full signature, all cleaning controls, profiles, and return/default contract.
- `man/url_profile.Rd:1-57` — profile names, overrides, customized profile semantics.
- `README.md:96-161` — canonical-key language and normalization/encoding examples.
- `NEWS.md:6-16,63-164,239-282` — current cleaning/profile/standards notes.

### Accepted design and product records

- `design/adr/0003-two-stage-parse-architecture.md:1-36` — Stage A/Stage B ownership.
- `design/adr/0007-url-standard-selector.md:1-54` — standards selector and governed versus orthogonal controls.
- `design/adr/0010-scheme-inference-policy.md:19-109` — scheme inference, presentation, acceptance, and standard separation.
- `design/adr/0011-path-encoding-orthogonality.md:32-93` — presentation orthogonality and lossy reserved folding.
- `design/adr/0012-general-scheme-model-and-profile-strategy.md:228-310,354-369,476-527,712-747` — state families, eligibility, four axes, profiles, and standing rules.
- `design/prd/url-standard-selector.md:1-550` — accepted point-in-time requirements and false-join risk; interpreted behind newer ADRs/code/tests where they differ.
- `design/prd/v2.md:1-375` — accepted point-in-time cleaning/port/profile requirements; interpreted behind newer ADRs/code/tests where stale.
- `design/prd/browser-fixer.md:74-91` — explicit fixer ordering and examples.
- `design/prd/host-validation-policy.md:1-207` — host policy as a separate layer.

### Implementation symbols traced

- `R/accessors.R:27-107` — `.extract_from_urls()` vector/error behavior.
- `R/accessors.R:228-360` — `get_clean_url()` signature, supplied-argument capture, validation, extraction, and profile override.
- `R/parse.R:834-881` — option constants.
- `R/parse.R:913-1013` — standard/profile bundles.
- `R/parse.R:1015-1081` — profile resolution.
- `R/parse.R:1083-1201` — standard-governed conflicts.
- `R/parse.R:1224-1352` — `.parse_options()`.
- `R/parse.R:1892-2087` — `._parse_stage_b_vec()`, eligibility, ordered transforms, clean construction, general serialization route.
- `R/parse-phases.R:1383-1545` — path identity/normalization/index/slash/encoding pipeline.
- `R/parse-phases.R:1561-1573` — final scheme presentation.
- `R/parse-phases.R:1882-1971` — `www` handling.
- `R/parse-phases.R:1979-2113` — PSL derivation and subdomain retention.
- `R/parse-phases.R:2124-2253` — host encoding and case handling.
- `R/parse-phases.R:2255-2291` — query phase integration.
- `R/parse-phases.R:2303-2407` — port handling and legacy clean builder.
- `R/parse-phases.R:2422-2608` — general serializers and empty-state preservation.
- `R/path-query.R:351-541` — ordered query parser/filter/sort/serializer.
- `R/domain.R:9-365` — host/domain/PSL helpers, including `.normalize_and_punycode()`, `.validate_idna_domain_vec()`, `.psl_registered_domain()`, and `.psl_public_suffix()`.
- `R/profiles.R:1-88` — public profile helpers and customized profile labeling.
- `R/utils.R:1-286` — scalar/value validation helpers used by the option surface, including `.is_whatwg()` and `.glob_to_regex()`.
- `R/canonical_join.R:92-164,238-247` — public join pipeline and exact `clean_url` key extraction.
- `R/resolve.R:160-249` — resolving/serializing behavior and clean result.
- Repository-wide search of `R/` for `mutate_url`, component setters, `set_*`, `replace_*`, and mutation terminology — no current URL mutation API found.

### Tests inspected

- `tests/testthat/test-accessors.R:1-99,242-480,810-972,1091-1230` — cleaning defaults, query controls, `www`, path/case/encoding, subdomains, ports, and accessor type boundary.
- `tests/testthat/test-query-clean-url.R:9-231` — drop/filter/allow/keep modes, pattern controls, precedence, case isolation, validation, and join consequences.
- `tests/testthat/test-url-profiles.R:4-178` — exact bundles, overrides, browser fixup, standards profiles, SEO/canonical behavior, and joins.
- `tests/testthat/test-url-standard-port-handling.R:10-32,55-75,141-240,377-403` — defaults, WHATWG behavior, literal keep override, stripping, and validation.
- `tests/testthat/test-url-standard-case-handling.R:8-105` — selector conflicts, accessors, joins, and null-standard identity.
- `tests/testthat/test-url-standard-path-encoding-orthogonal.R:7-85,181-216` — identity, encode/decode, reserved folding, and false join.
- `tests/testthat/test-parse-serializers.R:11-159` — output shapes, host/authority states, absent/empty query, RFC forms, and serializer round trips.
- `tests/testthat/test-host-policy.R:1-226` — host kinds, separate policy layer, and parse-versus-policy failures.
- `tests/testthat/test-accessor-helper.R:1-78` — rowwise extraction/error behavior.
- `tests/testthat/test-canonical_join.R:1-304` — key behavior, profiles, conflicts, multiplicity, and diagnostics.

This was a read-only evidence audit. No runtime tests were needed to establish the protocol gaps; current behavior claims above are grounded in implementation and existing assertions, while unresolved future behavior is explicitly labeled **OPEN**.
