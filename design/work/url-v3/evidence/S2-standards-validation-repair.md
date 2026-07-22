# S2 review — standards, validation, and repair

## Executive verdict

**Verdict: revise before accepting the reconstruction protocol.** The protocol has the right authority order, makes `browser` repair conceptually orthogonal to a standard, requires separate parsing and serialization contracts, and warns against unsupported conformance claims. Those are strong foundations. It is not yet sufficient to reconstruct this slice without silently carrying v2 category errors into v3.

The blocking weakness is procedural: §6.2 names topics, but the required artifacts and acceptance gates do not force the reconstruction to distinguish all of the states that the shipped implementation already conflates or composes:

- structural parse success;
- conformance of the original input to the selected standard;
- scheme-specific validity;
- practical host policy;
- optional annotation completion;
- conformance of a repaired input;
- conformance of serialized output.

Likewise, the protocol does not require an ordered inventory of every input rewrite, recovery, dependency shim, default leniency, and presentation transform before an owner decides what counts as ordinary parsing versus explicit repair. That omission is material: v2 contains at least browser repair, WHATWG-required preprocessing, curl adaptation, shared scheme inference, selector-scoped authority recovery, standards normalization, and lossy presentation in one pipeline (`R/parse-phases.R:933-984`, `R/parse-phases.R:993-1033`, `R/parse-phases.R:1138-1188`, `R/parse-phases.R:1388-1459`).

The protocol should therefore be amended with the mandatory artifacts and gates below before the successor product specification is drafted. This report audits the reconstruction process only; it does not select the desired v3 behavior.

## Coverage map

| Protocol requirement | Shipped/accepted evidence checked | Assessment and required process correction |
|---|---|---|
| Exact WHATWG/RFC selection point (`protocol` §6.2, lines 132-135) | Public selector and profiles in `R/parse.R:913-988`; effective option resolution in `R/parse.R:1036-1081`, `R/parse.R:1145-1201`, `R/parse.R:1224-1351`; accepted selector authority in `design/adr/0007-url-standard-selector.md:18-54` | **Partial.** Require an effective-configuration/route artifact for every public entry point, including defaults, profiles, overrides, conflicts, hidden knobs, and stage/cache ownership. Merely naming the selection point cannot expose the direct-selector/profile differences already shipped. |
| Separate WHATWG and RFC parsing and serialization (`protocol` §6.2, lines 134-136) | Parser preprocessing and routing in `R/parse-phases.R:91-332`, `R/parse-phases.R:689-772`, `R/parse-phases.R:986-1225`; serializers in `R/parse-phases.R:2438-2605`; state-level tests in `tests/testthat/test-parse-serializers.R:1-159` | **Partial.** Require independent parser, state, and serializer contracts plus revalidation of serialized output. Current serializer tests assert selected state shapes and explicitly omit fragments; they are not a public full-string validity gate. |
| Strict, non-repairing ordinary compliance (`protocol` §6.2, line 136) | `scheme_policy="infer"` is an omnibox affordance, not parser behavior (`design/adr/0010-scheme-policy-acceptance-axis.md:12-30`, `:41-59`); direct RFC selector repairs repeated `@` (`tests/testthat/test-url-standard-authority.R:14-23`) while RFC-general rejects it (`tests/testthat/test-general-acceptance.R:51-66`) | **Insufficient.** Require an original-input conformance verdict and a phase/provenance ledger. Without them, “strict” can still validate a recovered string or report `ok` for a repaired parse. |
| `browser` is explicit pre-parse repair (`protocol` §6.2, line 137) | Browser bundle in `R/parse.R:943-960`; fixer in `R/parse-phases.R:933-984`; actual ordering in `R/parse-phases.R:1002-1033`; behavior tests in `tests/testthat/test-browser-fixer.R:1-67`, `:101-165` and `tests/testthat/test-url-profiles.R:90-109` | **Conceptually covered, operationally incomplete.** Require an executable phase order, closed repair table, original/repaired strings, rule IDs, and “repair disabled” negative fixtures. Resolve the accepted PRD/code ordering contradiction before using it as v3 evidence. |
| Decide whether named RFC repair exists (`protocol` §6.2, line 138) | Shipped selector-scoped authority recovery in `R/parse-phases.R:294-332`, used for both standards at `R/parse-phases.R:1172-1175`; RFC direct/general difference above; RFC host shim scope discussed in `design/adr/0009-whatwg-host-charset-shim.md:130-139` | **Insufficient.** Before the owner answers yes/no, require a complete inventory of existing RFC-facing recoveries and dependency adaptations. Otherwise “no RFC repair” could silently retain repair under parser internals. |
| Meaning of validity with optional IDNA/PSL/DNS annotations skipped (`protocol` §6.2, line 139) | Parse/annotation split in `ARCHITECTURE.md:38-85`, `:130-159`; practical host policy explicitly is not conformance (`R/host-policy.R:1-21`, `:174-253`; `design/prd/host-validation-policy.md:25-36`, `:79-118`); engine changes outputs and cache identity (`tests/testthat/test-engine-per-request.R:62-95`) | **Insufficient.** A question is present, but no required result algebra or fixture taxonomy is specified. Require base validity to remain independent from annotation state and distinguish skipped, ineligible, unavailable, and failed annotations. |
| Full-string conformance fixtures and claim limits (`protocol` §6.2, line 140) | `tests/testthat/fixtures/url-standard-conformance.csv`; harness assertions in `tests/testthat/test-url-standard-conformance.R:25-60`; divergence classes derived from `expected_clean_url` at `:339-373`; broader audit ledger at `_scratch/orchestrate/audit/conformance-ledger.csv` | **Insufficient.** The existing fixture checks projections and `clean_url`, not an original-input full-string accept/reject verdict. Require oracle provenance, full state/serialization expectations, negative vectors, and a claim-coverage report. |
| Example layers separately answer parse/validation/repair (`protocol` §7, lines 206-237) | Shipped examples demonstrate same final string through different paths: browser fixup, scheme inference, standards preprocessing, or parser recovery | **Good structure, weak enforcement.** Make a phase trace and independent verdict fields mandatory fixture columns; prose questions alone do not prevent collapsed outcomes. |
| Source authority and contradiction handling (`protocol` §3 and §5.3, lines 36-55 and 98-106) | Accepted PRD wrappers conflict with internal status lines in `design/prd/url-standard-selector.md:1-16` and `design/prd/url-standard-selector-v2.md:1-17`; browser PRD contains shipped and pending parts in one accepted document (`design/prd/browser-fixer-and-url-search-classifier.md:1-19`, `:37-104`, `:245-256`) | **Partial.** Require assertion- or section-level lifecycle classification, not one status for a whole source. Record internal status contradictions as contradictions rather than silently treating the complete document as accepted. |
| Review catches standards weakened by browser behavior and invalid generated output (`protocol` §10, lines 278-305) | Red-team checklist names both hazards at lines 286-295 | **Good but late.** Add pre-draft gates for mutation provenance and output revalidation. A post-draft reviewer should verify these artifacts, not be the first mechanism likely to discover their absence. |

## Severity findings

### CRITICAL S2-01 — no mandatory multi-verdict validation model

**Protocol section:** §6.2 “Standards, validation, and repair” (`_scratch/url-v3-spec-reconstruction-protocol.md:132-140`), §7 (`:208-219`), acceptance criteria §12 (`:319-334`).

**Exact evidence:**

- Accepted ADR 0012 says parse success is not validity, selected diagnostics are not complete validators, absence of a diagnostic never implies conformance, and a full validator would be a separate named facility (`design/adr/0012-general-url-parser-scope.md:402-409`).
- It gives `http:example.com` as generic RFC syntax that is invalid as an HTTP URI and not flagged by current selected diagnostics (`design/adr/0012-general-url-parser-scope.md:440-451`).
- It defines `parse_status` as parser/domain-analysis outcome, never conformance (`design/adr/0012-general-url-parser-scope.md:460-470`); the implementation derives `ok` and PSL warnings from curl/general routing and host/PSL state (`R/parse-phases.R:2614-2688`).
- Diagnostics repeat the limitation in source (`R/diagnostics.R:47-54`, `:293-332`), and practical host validation says it is a policy layer, not a conformance oracle (`R/host-policy.R:1-21`, `:174-191`).

**Minimal example:** `safe_parse_url("http:example.com", url_standard="rfc3986")` is structurally parseable as an RFC rootless path and can have `parse_status="ok"` (`tests/testthat/test-url-standard-authority.R:44-55`), while accepted documentation says it is not a valid HTTP URI (`design/adr/0012-general-url-parser-scope.md:448-451`).

**Consequence:** A reconstruction can satisfy the current wording with one overloaded `valid` or `status` field, or infer validity from `parse_status`, diagnostics, host policy, or successful serialization. That would make ordinary “strict validation” untestable and would preserve the exact ambiguity v3 is intended to remove.

**Concrete protocol correction:** Require a validation-result artifact before product prose. For every representative row it must separately record, at minimum:

1. structural parse outcome;
2. original-input conformance to the selected URL/URI standard;
3. scheme-specific validity and severity, where applicable;
4. practical host-policy verdict, if requested;
5. optional-annotation completion/result;
6. repaired-input conformance, when repair was requested;
7. serialized-output conformance.

Require stable error identifiers and explicit `not-evaluated` states. Add an acceptance rule forbidding `parse_status`, absence of selected diagnostics, `is_valid_host()`, `clean_url`, or successful parsing/serialization from serving as a conformance proxy.

### CRITICAL S2-02 — no ordered mutation/recovery provenance gate

**Protocol section:** §6.2 (`protocol:132-140`), §7 (`:208-219`), §10 (`:282-295`).

**Exact evidence:** The shipped input path applies materially different operations in order:

- explicit browser repair (`R/parse-phases.R:933-984`, invoked at `:1002-1010`);
- standards-required WHATWG control stripping, backslash interpretation, and Unicode separator mapping (`R/parse-phases.R:1012-1033`);
- default scheme inference, which accepted ADR 0010 calls an omnibox affordance rather than parser behavior (`design/adr/0010-scheme-policy-acceptance-axis.md:26-30`; implementation `R/parse-phases.R:1138-1170`);
- selector-scoped repeated-`@` recovery (`R/parse-phases.R:1172-1175`);
- WHATWG IPv4 rewriting and a dependency shim/fallback (`R/parse-phases.R:1177-1188`; shim rationale in `design/adr/0009-whatwg-host-charset-shim.md:55-115`);
- standard normalization and independently selectable, potentially lossy path presentation (`R/parse-phases.R:1388-1459`; accepted boundary in `design/adr/0011-path-encoding-orthogonal-presentation.md:78-90`).

**Minimal examples:**

- `http://username@@@@example.com` with a direct RFC selector is repaired and returns `ok` (`tests/testthat/test-url-standard-authority.R:14-23`), but the RFC-general gate rejects the same repeated raw `@` (`tests/testthat/test-general-acceptance.R:51-66`).
- `example.com` is accepted under direct `url_standard="whatwg"` through shared inference but rejected by `profile="whatwg"` (`tests/testthat/test-scheme-policy.R:207-221`; `tests/testthat/test-url-profiles.R:102-109`).
- `http;//example.com` is repaired only under the browser bundle (`tests/testthat/test-url-profiles.R:90-100`).

**Consequence:** The reconstruction can label an operation as parser behavior because it currently resides before curl, validate only the changed string, or omit provenance because the final string looks conformant. This makes “non-repairing” dependent on implementation placement rather than a defined contract and makes an RFC repair decision impossible to take honestly.

**Concrete protocol correction:** Add a mandatory **ordered input-intervention ledger** with one row per operation and these fields: stable rule ID; owning standard/policy/dependency; phase; input eligibility; byte/component delta; whether the original input is preserved; category (`syntax recognition`, `standards-required preprocessing`, `normalization`, `dependency adaptation`, `compatibility recovery`, `explicit repair`, `presentation`, or `cleaning`); default/profile reachability; fatal/non-fatal behavior; diagnostic/provenance output; cache impact; fixtures. The owner must approve category and ordinary-path eligibility for every row before the v3 validation/repair section is normative.

Also require validation to be able to evaluate the original bytes independently of any optional repaired candidate. “Repair off” must be a tested no-op with no hidden compatibility recovery.

### HIGH S2-03 — conformance evidence gate permits projection matching to masquerade as full-string conformance

**Protocol section:** §6.2 line 140, traceability §5.4 (`protocol:108-116`), §7 (`:221-237`), §10 (`:294-295`).

**Exact evidence:**

- The existing conformance harness describes itself as a spec backstop (`tests/testthat/test-url-standard-conformance.R:1-21`) but asserts only `clean_url`, host, host type, path, and selected diagnostics (`:25-60`).
- Its `divergence_class` is derived only by comparing paired `expected_clean_url` values (`tests/testthat/test-url-standard-conformance.R:339-373`).
- `clean_url` is an opinionated/product key that excludes or conditions components: the builder is host-oriented and query-optional (`R/parse-phases.R:2365-2406`); resolver documentation explicitly says fragments and credentials are excluded and query/port depend on presentation knobs (`R/resolve.R:174-183`).
- The historical proposal’s “perfect path” and “zero gaps” claims are research evidence, not accepted truth (`_scratch/prd-url-object-grammar-DRAFT.md:3-7`, `:20-40`), while its own correction says `clean_url` is not the serialization (`:70-100`).

**Minimal example:** Two inputs that differ only by credentials or fragment can produce the same `clean_url`; the current golden harness would not assert that original full-string conformance, preserved state, or faithful serialization differs.

**Consequence:** A future suite can achieve a high “conformance” percentage by matching selected projections at a favorable dial bundle while missing rejection rules, delimiter presence, userinfo, fragment, repair provenance, or the standard serialization. The protocol’s claim-limit sentence does not define when a claim is permitted.

**Concrete protocol correction:** Require a **conformance claim matrix** keyed by standard, scheme class, grammar/state dimension, and operation. Each fixture must carry independent oracle provenance/version, exact original bytes, expected accept/reject, validation errors, parsed state including present-empty distinctions, standard serialization (or “no serialization”), and known exclusions. Require positive, negative, round-trip, idempotence, repaired-vs-original, scalar/vector, duplicate, and invalid-row cases. An automated coverage report must state which claims the corpus supports and which remain unclaimed. Projection or `clean_url` parity may be a regression assertion but must not count as full-string conformance.

### HIGH S2-04 — the dial/profile matrix is not a mandatory reconstruction artifact

**Protocol section:** §6.2 lines 132-140 and decision-ledger fields at §5.2 (`protocol:82-96`).

**Exact evidence:**

- The public parse signatures expose 25 interdependent knobs plus `profile` (`R/parse.R:431-469`, `:585-625`).
- The standard governs only selected axes in `.URL_STANDARD_PROFILES` (`R/parse.R:913-924`, `:1083-1091`), while profiles are separate bundles and can set hidden `fixup_posture`/`path_identity` (`R/parse.R:926-988`).
- Explicit arguments override profiles (`R/parse.R:1036-1057`), profile-authorized exceptions survive standard expansion (`R/parse.R:1060-1080`), and direct calls enforce a conflict matrix (`R/parse.R:1158-1201`).
- `profile="whatwg"` requires a scheme while direct `url_standard="whatwg"` inherits default inference (`R/parse.R:953-960`; `tests/testthat/test-url-profiles.R:102-109`). `rfc-syntax` deliberately preserves case/dot segments and bypasses direct conflict rules (`R/parse.R:962-975`; `tests/testthat/test-url-profiles.R:111-130`). Profiles also skip conflict behavior across `canonical_join()` (`tests/testthat/test-url-profiles.R:160-177`).

**Minimal example:** `example.com` parses under `url_standard="whatwg"` but errors under `profile="whatwg"`; the standard token alone therefore does not determine the ordinary compliance posture.

**Consequence:** A ledger row saying “WHATWG selected here” can still leave defaults, hidden posture, presentation, overrides, or route eligibility unstated. Reviewers cannot tell whether a fixture tests a standard, a profile, a hand-built bundle, or compatibility behavior.

**Concrete protocol correction:** Require a machine-readable **effective-configuration and interaction inventory** before decisions are reviewed. It must list every public and hidden dial, allowed values, default, conceptual axis, stage, cache-key effect, entry points, profile expansion, override precedence, conflicts, authorized exceptions, and eligibility by scheme/state. Generate a pairwise interaction matrix plus exhaustive coverage for all standard × repair × acceptance × scheme-inference × scheme-relative × validation-mode combinations. Every conformance result must record the fully expanded configuration, not only `url_standard` or profile name.

### HIGH S2-05 — optional annotation validity has no required state model

**Protocol section:** §6.2 line 139; product structure §8 items 11-13 (`protocol:255-257`).

**Exact evidence:**

- `ARCHITECTURE.md:38-85` separates parsing, component normalization, domain analysis, and status; Stage A currently includes PSL work and caches it (`ARCHITECTURE.md:130-159`).
- A custom PSL engine changes domain/TLD/subdomain results and its identity participates in the cache (`tests/testthat/test-engine-per-request.R:62-95`).
- General opaque/non-special hosts intentionally have no PSL or Punycode outputs despite successful parsing (`tests/testthat/test-general-acceptance.R:100-121`).
- Practical host helpers compose policy over parsing and explicitly do not change parse status or prove conformance (`R/host-policy.R:1-21`, `:235-253`; accepted PRD `design/prd/host-validation-policy.md:25-36`, `:79-90`).

**Minimal example:** `foo://host.example/x` can parse successfully under general acceptance while domain/TLD annotations are `NA` because the row is ineligible (`tests/testthat/test-general-acceptance.R:102-121`). `https://a.example.com/p` can produce different registrable-domain annotations under different valid engines (`tests/testthat/test-engine-per-request.R:62-80`). Neither case changes original URL-standard conformance.

**Consequence:** A single `NA`, `FALSE`, warning, or `valid` field cannot distinguish absent data, ineligible annotation, skipped work, unavailable dependency, failed annotation, or negative result. Lazy annotation work can accidentally weaken the meaning of validity or make it depend on engine/cache state.

**Concrete protocol correction:** Require a typed annotation-result model with at least `not-requested`, `ineligible`, `completed`, `failed`, and `unavailable`, separate from the annotation value and from URL validity. Require per-annotation provenance (engine/list/version), cost tier, failure policy, vector behavior, and cache identity. Add an acceptance invariant: enabling, disabling, failing, or switching an optional annotation may change only its own result/policy verdict, never the original-input standard-conformance verdict.

### HIGH S2-06 — evidence lifecycle is document-level where the sources require assertion-level classification

**Protocol section:** source authority §3 (`protocol:36-55`), decision ledger §5.2 (`:82-96`), contradiction register §5.3 (`:98-106`).

**Exact evidence:**

- `design/prd/url-standard-selector.md:1-7` calls the document durable, accepted, and shipped, while `:12-16` still says “Draft for review” and has an unresolved target.
- `design/prd/url-standard-selector-v2.md:1-6` likewise says durable, accepted, and shipped, while `:11-17` says draft and describes incomplete v1 closure.
- The accepted browser PRD explicitly mixes shipped Part 1 with pending Part 2 (`design/prd/browser-fixer-and-url-search-classifier.md:1-19`, `:37-104`, `:245-256`).
- The run state correctly requires preservation of `_scratch/prd-url-object-grammar-DRAFT.md` as historical evidence (`_scratch/orchestrate/RURL-v3-architecture-spec-RUN-STATE.md:34-40`), and that draft contains unaccepted conformance and validation proposals (`_scratch/prd-url-object-grammar-DRAFT.md:20-40`, `:124-136`, `:175-192`, `:228-247`).

**Minimal example:** Treating the browser PRD as one “accepted PRD” would elevate its still-pending URL-vs-search classifier along with the shipped fixer.

**Consequence:** The current single ledger `status` can classify a topic but not the lifecycle of individual imported claims. Stale draft metadata can be ignored silently, and mixed documents can be over-promoted.

**Concrete protocol correction:** Add a mandatory source/assertion manifest. For each imported claim record source path, section/lines, document hash or review date, authority class, and lifecycle: `SHIPPED`, `ACCEPTED-PENDING`, `HISTORICAL`, `OPEN`, `SUPERSEDED`, or `CONFLICT`. Require internal status inconsistencies and mixed-lifecycle documents to enter the contradiction register. Normative prose may cite only a classified assertion, never rely on a whole-document label.

### MEDIUM S2-07 — serializer/output validity is deferred to red-team review instead of gated before drafting

**Protocol section:** §6.2 lines 134-140, §6.3 (`protocol:142-150`), §10 lines 286-295.

**Exact evidence:**

- Current WHATWG and RFC serializers are internal state renderers (`R/parse-phases.R:2438-2589`). Their tests hand-build state and assert `clean_url`, explicitly excluding fragments (`tests/testthat/test-parse-serializers.R:1-9`, `:73-88`).
- `clean_url` can be non-faithful by design: host required for ordinary build, port/query optional, credentials and fragments absent (`R/parse-phases.R:2365-2406`; `R/resolve.R:174-183`).
- Accepted ADR 0011 allows path presentation that may be lossy or non-conformant and says it is not governed standard identity (`design/adr/0011-path-encoding-orthogonal-presentation.md:78-90`).

**Minimal example:** `HTTP://user:pass@Ex.com:80/a/../b?#` is already required by the protocol (`protocol:223`), but current serializer tests cannot prove that credentials, fragment, and bare empty delimiters survive a faithful standards serialization.

**Consequence:** The v3 draft could define separate serializers yet never prove that a parser-produced valid object serializes to valid output, or could accidentally test an opinionated cleaning string as the serializer.

**Concrete protocol correction:** Before drafting, require separate acceptance gates for (a) faithful standard serialization, (b) human formatting, and (c) cleaning. The serialization gate must cover parser-produced states, preserve required source distinctions, revalidate output under the selected standard, and define round-trip/idempotence invariants. Lossy formatting/cleaning must never satisfy this gate.

### MEDIUM S2-08 — reference resolution is at risk of being inherited as a fact rather than reconstructed

**Protocol section:** §6.2 and §6.8/curl-free migration (`protocol:132-140`, `:200-204`), traceability §5.4 (`:108-116`).

**Exact evidence:**

- Accepted v2 PRD D6 says the RFC and WHATWG base merge is identical (`design/prd/url-standard-selector-v2.md:106`).
- Shipped `resolve_url()` always performs one RFC 3986 recomposition, then delegates standard-specific handling to parsing (`R/resolve.R:160-172`, `:213-248`; characterization in `tests/testthat/test-resolve-url.R:1-8`, `:110-157`).
- The current return value is opinionated `clean_url`, which drops/conditions components (`R/resolve.R:174-183`).

**Minimal example:** A fragment-only relative reference can be merged structurally but the public result drops the fragment because resolution and cleaning are composed.

**Consequence:** A curl-free v3 design could copy the current common-merge assertion and output contract without independently verifying both standards’ base-URL algorithms or separating resolution from cleaning. Current code is shipped evidence, not proof of the desired v3 standard contract under protocol §3.

**Concrete protocol correction:** Add reference resolution/base parsing to the standards decision ledger as an explicit reconstruction item. Require independent primary-standard evidence and fixtures for RFC and WHATWG base handling, with merge output validated before formatting/cleaning. The owner must either reaffirm a shared algorithm with evidence or accept separate contracts; current implementation cannot close the decision.

### MEDIUM S2-09 — browser-fixer phase order is contradictory across accepted prose and shipped code

**Protocol section:** contradiction register §5.3 (`protocol:98-106`), browser repair in §6.2 line 137.

**Exact evidence:** The accepted browser PRD places selected WHATWG preprocessing between outer trimming and its remaining named fixer steps (`design/prd/browser-fixer-and-url-search-classifier.md:37-50`). Shipped code runs the complete three-step browser fixer first, then WHATWG control stripping, backslash recognition, and separator mapping (`R/parse-phases.R:933-984`, `:1002-1033`). Tests characterize the pure fixer and the downstream inference seam (`tests/testthat/test-browser-fixer.R:1-54`) but do not resolve the prose/code category and ordering mismatch.

**Minimal example:** `http;\texample.com` can reach the same final parse after different ownership/order narratives: strip the tab as standard preprocessing before fixing, or fix first and strip later. Even when the final URL agrees, phase provenance and which operation is considered “repair” differ.

**Consequence:** Reconstruction can silently choose prose order, code order, or final-output equivalence. That undermines the explicit-repair boundary and makes phase-specific diagnostics or audit logs unstable.

**Concrete protocol correction:** Seed this conflict into the contradiction register and require an executable ordered trace for phase-sensitive fixtures. The owner need not choose the v3 order in this protocol review, but the conflict must be resolved before repair/parser contracts are accepted.

## Contradictions and open owner decisions

### Contradictions that must be registered

| ID | Competing claims | Required disposition |
|---|---|---|
| S2-C01 | v1/v2 PRD wrapper comments say accepted/shipped, while their internal status fields say draft (`design/prd/url-standard-selector.md:1-16`; `design/prd/url-standard-selector-v2.md:1-17`). | Classify claims section-by-section and record the stale metadata conflict; do not silently normalize it. |
| S2-C02 | Accepted browser PRD describes WHATWG preprocessing inside the named ordering before later fixer steps, while shipped code performs the whole fixer first (`design/prd/browser-fixer-and-url-search-classifier.md:37-50`; `R/parse-phases.R:1002-1033`). | Owner-approved phase order and category ownership, backed by trace fixtures. |
| S2-C03 | Direct RFC selector recovers repeated `@` and reports `ok`, while RFC-general rejects the same grammar (`tests/testthat/test-url-standard-authority.R:14-23`; `tests/testthat/test-general-acceptance.R:51-66`). | Classify shipped paths as compatibility, parser behavior, or repair before deciding the ordinary v3 RFC contract. |
| S2-C04 | Accepted selector language promises coherent standard behavior for governed axes (`design/adr/0007-url-standard-selector.md:18-35`), while direct selector/profile combinations have different acceptance defaults and incomplete validation (`R/parse.R:943-975`; ADR 0012 D5). | State claim scope precisely; no bundle may be called a full compliance path until its validation and serialization gates pass. |
| S2-C05 | Historical draft claims zero genuine conformance gaps and near-perfect reachable bundles (`_scratch/prd-url-object-grammar-DRAFT.md:20-36`), while its own model says the faithful serialization surface is missing (`:70-100`) and current golden tests assert projections. | Keep as historical audit evidence only; re-derive v3 claims using the new claim matrix. |

### Owner decisions that must remain open until evidence artifacts exist

1. What exact public operation is the ordinary strict, non-repairing compliance path, and what result does it return for parseable-but-nonconforming input?
2. Which interventions are standards-required parsing/preprocessing, normalization, dependency adaptation, compatibility recovery, explicit repair, formatting, or cleaning?
3. Does a named RFC repair operation exist? If not, which current RFC-facing recoveries are removed or reclassified?
4. Is validation a complete standard validator, selected diagnostics plus a separate validator, or another explicitly bounded contract? What claims may each surface make?
5. Which validity layers are fatal, report-only, or policy-controlled: generic syntax, WHATWG validation errors, scheme-specific rules, and host policy?
6. What are the v3 defaults and precedence for standard, profile, scheme acceptance, scheme inference, scheme-relative handling, repair, and validation mode? Are direct standard and same-named profile still intentionally different?
7. How are optional IDNA, PSL, DNS-shape, and policy annotations represented when not requested, ineligible, unavailable, or failed?
8. Which independent oracles and corpus versions support RFC and WHATWG claims, and what untested surface is explicitly excluded?
9. Must every repaired candidate and standards serialization be revalidated, and are original-input and repaired-input verdicts both exposed?
10. Does reference resolution share one merge algorithm, or do RFC and WHATWG have separate base parsing/resolution contracts? Does its public result remain coupled to cleaning?
11. Which v2 compatibility recoveries are deliberately revoked in the 3.0 ordinary path, and which survive only behind an explicit compatibility/repair posture?

## Dial and interaction inventory

This is the minimum reconstruction inventory. “Current behavior” is shipped evidence, not a recommended v3 contract.

| Axis / dial | Current behavior and evidence | Interaction that the protocol must force into the ledger/tests |
|---|---|---|
| `url_standard` | `NULL`, RFC 3986, or WHATWG; governs only selected internal behavior (`R/parse.R:913-924`; ADR 0007 `:18-54`) | Standard × input grammar × scheme class × validation mode × serializer. Record the exact route and original-input verdict. |
| `profile` | Separate bundle; `browser`, `whatwg`, `rfc-syntax`, `seo`/`canonical`; explicit args win (`R/parse.R:926-1081`) | Profile × direct standard × every explicit override; record expanded config and `customized`. Do not infer conformance from name. |
| `scheme_acceptance` | `web` closed set vs `general`; general requires a selected standard (`tests/testthat/test-general-acceptance.R:10-25`) | Standard × web/general × special/non-special/opaque/file/ws/wss × validation overlay. |
| `scheme_policy` | `infer` fabricates HTTP; `require` rejects; independent of standard and protocol presentation (`design/adr/0010-scheme-policy-acceptance-axis.md:41-77`) | Repair posture × direct/profile × schemeless/host:port/invalid host-shape. Preserve original verdict before inference. |
| `scheme_relative_handling` | Dedicated keep/http/https/error axis, explicitly independent of inference (`design/adr/0010-scheme-policy-acceptance-axis.md:70-77`, `:104-109`) | Standard × repair × base/no-base × resolver. |
| hidden `fixup_posture` | Only profiles can select browser repair (`R/parse.R:941-952`) | Closed repair rules × phase order × “off” no-op × provenance × revalidation. |
| hidden `path_identity` | Selected by standard; `rfc-syntax` authorizes `none` (`R/parse.R:913-924`, `:962-975`) | Standard/profile × normalization × path encoding × serializer; distinguish identity from presentation. |
| `path_normalization`, `case_handling` | Direct standard conflict-checkable; profile path can authorize exceptions (`R/parse.R:1083-1091`, `:1158-1187`) | Direct vs profile; explicit same/different values; scheme/path eligibility; output validity. |
| `host_encoding`, `path_encoding` | Presentation dials; path encoding deliberately orthogonal and may be lossy/non-conformant (ADR 0011) | Parser identity × formatter/serializer/cleaner. A presentation match cannot prove conformance. |
| `port_handling` | Independent output policy, with WHATWG parsed-port elision and literal `keep` escape hatch (`R/parse-phases.R:2303-2363`) | Standard × special scheme × explicit default/non-default/invalid/empty port × parse component vs serialization vs cleaning. |
| `protocol_handling`, `www_handling`, subdomain, trailing/index, query knobs | Cleaning/presentation transforms in the 25-knob public surface (`R/parse.R:431-469`) | Eligibility × scheme/state × validator-before/after mutation. Ensure they cannot serve as implicit repair or compliance. |
| `tld_source`, `engine` | Optional PSL policy/source changes annotations and cache identity (`tests/testthat/test-engine-per-request.R:62-95`) | Requested/skipped/ineligible/failed × engine provenance × cache × base validity invariance. |
| host interpretation/recovery | WHATWG controls/backslash/separators/IPv4/forbidden-host behavior; RFC reg-name/rootless; shared repeated-`@` recovery; dependency shims (`R/parse-phases.R:91-687`, `:689-772`, `:1172-1188`, `:1786-1880`) | Standard × original bytes × recovery category × host kind × validation errors × serialized host. |
| diagnostics / host policy | Selected facts and practical policy, both explicitly non-oracles (`R/diagnostics.R:47-54`, `:293-332`; `R/host-policy.R:1-21`) | Diagnostic coverage × complete validation × policy. Test absence separately from a positive conformance result. |
| resolver/base | One RFC merge today, then parse/clean (`R/resolve.R:160-183`, `:213-248`) | RFC/WHATWG × absolute/relative/base state × fragment/query/authority × standard serializer, before cleaning. |

Minimum combination gate: exhaustive coverage across the small semantic core (`standard × repair posture × validation mode × scheme acceptance × scheme policy × scheme-relative policy`), pairwise coverage for remaining dials, and targeted higher-order fixtures wherever eligibility, conflict, override, or cache rules intersect. The test record must include the expanded options and route trace so a passing output is attributable to the intended layer.

## Files and symbols checked

### Protocol and repository rules/architecture

- `_scratch/url-v3-spec-reconstruction-protocol.md` — §§3-12, especially source authority, durable artifacts, §6.2, layered examples, red-team review, and acceptance criteria.
- `CLAUDE.md`; `FP_CLAUDE.md`; `ARCHITECTURE.md` — repository rules, phase ownership, Stage A/B, cache, diagnostics ownership.
- `_scratch/orchestrate/RURL-v3-architecture-spec-RUN-STATE.md` — current reconstruction state, historical-draft preservation, baseline test result.

### Shipped implementation

- `R/parse.R` — `safe_parse_url()`, `safe_parse_urls()`, `.URL_STANDARD_PROFILES`, `.URL_PROFILES`, `.resolve_profile()`, `.merge_profile_args()`, `.url_standard_governed_choices`, `.check_url_standard_conflicts()`, `.parse_options()`, `.parse_cache_key()`, `._parse_stage_a_vec()`, `._parse_stage_b_vec()`.
- `R/parse-phases.R` — `.rewrite_whatwg_backslashes_vec()`, `.strip_whatwg_control_chars_vec()`, `.map_whatwg_domain_separators_vec()`, `.encode_excess_authority_at_vec()`, `.rewrite_whatwg_ipv4_hosts_vec()`, `.shim_whatwg_host_charset_vec()`, `.sanitize_whatwg_pqf_for_curl_vec()`, `.rfc3986_path_rootless_vec()`, `.apply_browser_fixup_vec()`, `.prepare_urls_for_curl_vec()`, `.normalize_path_vec()`, `.apply_host_standard_model_vec()`, `.apply_port_output_policy_vec()`, `.build_port_part_vec()`, `.build_clean_url_vec()`, `.serialize_whatwg_vec()`, `.serialize_rfc_generic_vec()`, `.derive_parse_status_vec()`, `.assemble_parse_result_vec()`.
- `R/diagnostics.R` — diagnostic vocabulary, `.derive_url_metadata_vec()`, bounded WHATWG facts, public companion-helper infrastructure.
- `R/host-policy.R` — `.gather_host_policy()`, `is_valid_host()`, `check_hosts()`.
- `R/resolve.R` — RFC reference split/transform/recomposition, `.resolve_one_raw()`, `resolve_url()`.
- `R/status-constants.R`; `R/utils.R` — coarse status vocabulary, supported/special scheme sets, forbidden/shim character sets, default ports, blank-to-`NA` collapse.

### Accepted normative documents

- `design/adr/0004-strict-host-shape-gate.md`
- `design/adr/0007-url-standard-selector.md`
- `design/adr/0009-whatwg-host-charset-shim.md`
- `design/adr/0010-scheme-policy-acceptance-axis.md`
- `design/adr/0011-path-encoding-orthogonal-presentation.md`
- `design/adr/0012-general-url-parser-scope.md`
- `design/prd/url-standard-selector.md`
- `design/prd/url-standard-selector-v2.md`
- `design/prd/browser-fixer-and-url-search-classifier.md`
- `design/prd/host-validation-policy.md`

### Historical/open evidence (not treated as normative)

- `_scratch/prd-url-object-grammar-DRAFT.md` — draft object/render/tiering model, validation-strictness proposal, conformance claims, open decisions.
- `_scratch/orchestrate/audit/dials.md` — read-only dial inventory.
- `_scratch/orchestrate/audit/LEDGER.md`, `_scratch/orchestrate/audit/conformance-ledger.csv`, `_scratch/orchestrate/audit/robustness-findings.md` — local audit methodology/results and bundle-sensitive evidence.
- `_scratch/conformance-gap-report.md`, `_scratch/divergence-ledger.md`, `_scratch/wpt-failures-TRIAGE.md`, `_scratch/wpt-failures-FINDINGS.md` — local conformance/divergence research inputs.

### Tests and fixtures

- `tests/testthat/test-url-standard-conformance.R`
- `tests/testthat/fixtures/url-standard-conformance.csv`
- `tests/testthat/fixtures/external-url-vectors.csv`
- `tests/testthat/test-url-standard-authority.R`
- `tests/testthat/test-url-standard-control-chars.R`
- `tests/testthat/test-url-standard-backslash.R`
- `tests/testthat/test-url-standard-forbidden-host.R`
- `tests/testthat/test-url-standard-path-rfc.R`
- `tests/testthat/test-url-standard-path-whatwg.R`
- `tests/testthat/test-url-standard-port-handling.R`
- `tests/testthat/test-url-standard-path-encoding-orthogonal.R`
- `tests/testthat/test-general-acceptance.R`
- `tests/testthat/test-browser-fixer.R`
- `tests/testthat/test-scheme-policy.R`
- `tests/testthat/test-scheme-acceptance.R`
- `tests/testthat/test-url-profiles.R`
- `tests/testthat/test-url-diagnostics.R`
- `tests/testthat/test-parse-serializers.R`
- `tests/testthat/test-eligibility-matrix.R`
- `tests/testthat/test-engine-per-request.R`
- `tests/testthat/test-resolve-url.R`

No product behavior is selected by this review. The concrete recommendation is to strengthen the reconstruction protocol with the required artifacts and gates above, then bring the listed owner decisions forward in the first review batch (`protocol` §9, lines 266-276).
