# S1 review — object/state and scalar/vector contract

## Verdict

**CHANGES REQUIRED before product-specification drafting.** The reconstruction protocol names the right subject areas, applies the right source precedence, and requires traceability, but it does not yet force the durable contract artifacts needed to prevent the current implementation's lossy seams from being restated as a v3 object model. In particular, it needs mandatory state, scalar/vector, and dial matrices with explicit acceptance gates.

This review uses the protocol's authority order: accepted decisions are **accepted intent**; code and tests are **shipped behavior**; requirements introduced only by the reconstruction protocol are **protocol proposals**; choices not settled by those sources remain **open v3 decisions**. That distinction follows `_scratch/url-v3-spec-reconstruction-protocol.md:36-54`, and the required ledger vocabulary and evidence fields are at `_scratch/url-v3-spec-reconstruction-protocol.md:82-116`.

## Coverage map

| Assigned surface | Evidence covered | Result location |
|---|---|---|
| Original input; source spelling versus parsed identity | Public `original_url`, raw/transformed component flow, ASCII/Unicode identity columns, output encoding | S1-F1, S1-F9 |
| Absent/present-empty authority, host, query, fragment, userinfo, credentials, and delimiters | Internal enums/classifiers, public `NA` projection, authority contradiction, dropped generic credentials, lossy query tokenization | S1-F1, S1-F2, S1-F5, S1-F6 |
| WHATWG opaque/list path and RFC path forms | Posture-specific state, string-backed list-path implementation, serializer behavior | S1-F1, S1-F7 |
| Public versus internal shapes and state lifetime | 18-field result, 20-field Stage A cache record, Stage-B source reparse, companion-only metadata | S1-F1, S1-F11 |
| Scalar/vector/`NA`/empty/non-character/invalid/zero-length/list behavior | Scalar parser, vector parser, shared accessor engine, partial failure, duplicates, typed empty output | S1-F3 |
| Behavior dials, profiles, eligibility, accessor exposure, cache ownership | Public parse formals, internal/profile-only axes, profile resolution, cache key, stale accessor registry | S1-F4, S1-F8, S1-F11 |
| Test oracle and fixture sufficiency | State/serializer/general-acceptance/accessor/characterization tests and fixture boundaries | S1-F10 and “Minimum state cross-product for acceptance” |
| Protocol corrections and unresolved decisions | Mandatory artifacts, graduation gates, owner questions, editorial improvements | “Required protocol additions,” “Questions requiring owner judgment,” and final assessment |

## Existing behavior/dial inventory

### Object and state behavior

| Layer or entry point | Shipped contract observed | Material gap for v3 reconstruction |
|---|---|---|
| `safe_parse_url()` | Scalar list on success; `NULL` for scalar missing/empty/non-character/failed rows; length other than one is a call-level error (`R/parse.R:431-477`, `R/parse.R:1441-1460`) | No typed invalid scalar object; not aligned with vector/accessor input policy |
| `safe_parse_urls()` | 18-column data frame; typed zero-row result; factors become labels; rows retain order/duplicates; invalid inputs become error rows (`R/parse.R:585-625`, `R/parse.R:709-763`) | Some non-scalar list elements abort the whole call despite the row-local-error comment (`R/parse.R:721-743`, `R/parse.R:808-824`) |
| Accessors | Character-vector-only, unnamed typed vectors/list projections, shared cached parse path (`R/accessors.R:27-108`) | Non-character inputs error rather than following either parser coercion policy; option exposure differs by accessor |
| Public parse record | 18 fields, including `original_url`, component projections, four domain/TLD identity spellings, `clean_url`, status (`R/utils.R:210-241`) | Empty/absent host and top-level component states collapse; no structural state object |
| Stage A cache record | 20 cache fields for parse/PSL data and flags (`R/utils.R:243-285`) | Does not retain path/authority/query/fragment kinds; Stage B reparses source (`R/parse.R:1932-1958`) |
| Internal state | WHATWG path kind; RFC path form; host, authority, query, fragment kinds; host forms (`R/parse-state.R:25-57`) | Authority vocabulary is inconsistent; generic userinfo is parsed then dropped; path list is stored as a string |
| Standards-named serializers | Build `clean_url`, preserve empty query delimiter, omit fragment, credentials, and `authority_kind` (`R/parse-phases.R:2425-2589`) | They are not full-object standards serializers and cannot prove the protocol’s faithful serialization proposal |
| Query model | Raw whole query can survive; grouped/ordered parameter parsers collapse bare `a` versus `a=` and empty `&` segments (`R/path-query.R:294-378`) | Insufficient for faithful component mutation and reassembly |

### Dial inventory

| Dial family | Dials observed | Ownership/interaction that the protocol must record |
|---|---|---|
| Acceptance and interpretation | `scheme_policy`, `scheme_acceptance`, `scheme_relative_handling`, `url_standard` | Parse-affecting; composition validation; standard-specific routing; Stage A cache membership |
| Repair and profiles | public `profile`; internal `fixup_posture`, `profile_authorized`; bundles `browser`, `whatwg`, `rfc-syntax`, `seo`; alias `canonical` | Profile expansion, explicit-override precedence, repair-before-parse, authorized exceptions (`R/parse.R:926-1013`, `R/parse.R:1040-1070`) |
| Host/domain | `www_handling`, `subdomain_levels_to_keep`, `tld_source`, `host_encoding`, `case_handling`, `engine` | Parse/PSL versus presentation split, DNS/IDNA eligibility, engine-sensitive cache identity |
| Path | `path_normalization`, `trailing_slash_handling`, `index_page_handling`, `path_encoding`; internal `path_identity` | Standard identity versus knowingly lossy presentation; path-kind and scheme eligibility; Stage B ownership |
| Query | `query_handling`, `params_keep`, `params_drop`, `sort_params`, `empty_param_handling`, `params_case_sensitive`, `decode_plus` | Cleaning semantics, token preservation/canonicalization, HTTP(S)/profile eligibility, accessor exposure |
| Scheme/port presentation | `protocol_handling`, `port_handling` | Cleaning/output projection, standard default-port interpretation, semantic-transform eligibility |
| Public parser-wide configuration | The 24 formals after `url` on both parser entry points are listed at `R/parse.R:431-469` and `R/parse.R:585-625` | Registry must be mechanically checked against public formals, `.parse_options()`, profiles, consumers, and cache key |
| Accessor-only projections | `source`, query `format`/`decode`, subdomain `include_www`/`format`, query-summary `level` | Must be registered as projection controls or explicit aliases, with intentional omissions documented |
| Cache key | URL plus protocol, www, TLD source, scheme-relative, standard, scheme policy, scheme acceptance, fixup posture, and engine token (`R/parse.R:1418-1438`) | Every identity-affecting dial must enter the key; presentation dials must be proven cache-transparent |

The current accessor test registry is not exhaustive: its hand-written option source contains only 11 legacy options (`tests/testthat/test-accessor-registry.R:65-81`), and its completeness loops can therefore prove completeness only over that subset (`tests/testthat/test-accessor-registry.R:651-683`).

## Files and symbols checked

- Protocol/project instructions: `_scratch/url-v3-spec-reconstruction-protocol.md`; `CLAUDE.md`; `FP_CLAUDE.md`; `ARCHITECTURE.md`.
- Accepted decisions: `design/adr/0003-parse-present-stage-split.md`; `design/adr/0006-diagnostics-companion-helpers-only.md`; `design/adr/0007-url-standard-selector.md`; `design/adr/0011-path-encoding-orthogonal-presentation.md`; `design/adr/0012-general-url-parser-scope.md`.
- Implementation: `R/parse-state.R` (`.PATH_KIND`, `.RFC_PATH_FORM`, `.HOST_KIND`, `.AUTHORITY_KIND`, `.PRESENCE_KIND`, classifiers, `.stage_b_eligibility`, `.split_authority`, general/RFC-file parsers); `R/parse.R` (scalar/vector APIs, option/profile resolution, cache keys, Stage A/B, coercion); `R/parse-phases.R` (WHATWG/RFC generic serializers and public assembler); `R/accessors.R` (`.extract_from_urls`, clean/component accessors, metadata/query helpers); `R/path-query.R` (query parsers/filters and component encoders); `R/status-constants.R`; `R/profiles.R`; `R/utils.R` (public and Stage A field registries).
- Tests: `tests/testthat/test-parse-state.R`; `test-parse-serializers.R`; `test-general-acceptance.R`; `test-accessors.R`; `test-accessor-helper.R`; `test-characterization-snapshot.R`; `test-parse-phases.R`; `test-accessor-registry.R`; relevant fixture/snapshot inputs under `tests/testthat/fixtures/parse-corpus.csv`, `tests/testthat/fixtures/url-standard-conformance.csv`, and `tests/testthat/_snaps/characterization-snapshot.md`.

## Findings, ordered by severity

### S1-F1 — BLOCKER: no mandatory canonical state-schema artifact or lifetime invariant

**Protocol text.** Section 6.1 lists original input, parsed structural state, absent/present-empty components, source spelling versus identity, path/host/authority forms, credentials, and scalar/vector behavior (`_scratch/url-v3-spec-reconstruction-protocol.md:122-130`). Section 8 merely assigns these subjects to prose sections (`_scratch/url-v3-spec-reconstruction-protocol.md:243-264`). Neither section requires one canonical schema that declares fields, value domains, ownership, public projection, or invariants at every pipeline transition.

**Accepted intent.** ADR 0012 freezes the v2 public parse result at 18 fields while requiring richer internal state (`design/adr/0012-general-url-parser-scope.md:228-268`) and states that those internal kinds must round-trip (`design/adr/0012-general-url-parser-scope.md:623-640`, `design/adr/0012-general-url-parser-scope.md:733-736`). ADR 0006 separately says diagnostic metadata must not widen that parse result (`design/adr/0006-diagnostics-companion-helpers-only.md:18-29`). Those are accepted v2 decisions; neither decides whether v3 exposes a first-class URL object, retains the 18-field projection only as a compatibility view, or uses some other public shape.

**Shipped behavior.** The public result is the 18-field projection in `R/utils.R:210-241`. Stage A caches a different 20-field record and no `path_kind`, `rfc_path_form`, `authority_kind`, `query_kind`, or `fragment_kind` (`R/utils.R:243-285`). Stage B therefore reparses `original_url` to recover state for general URLs (`R/parse.R:1932-1958`). The public assembler collapses empty and absent hosts to the same `NA` (`R/parse-phases.R:2716-2756`). Comments still call these outputs 14 columns (`R/parse.R:1463-1477`, `R/parse.R:2110-2111`; `R/parse-phases.R:2716-2718`), while the live registry is 18, illustrating why prose and dispersed declarations are not a safe schema.

**Minimal failure example.** `foo:///x?#` needs, at minimum, the facts “authority delimiter present,” “host empty,” “query delimiter present with empty payload,” and “fragment delimiter present with empty payload.” The public row maps host/query/fragment to `NA`; Stage A does not retain the kinds; Stage B reconstructs some of them from the original string. A later mutation has no specified rule for whether reparsing the original may override changed state.

**Concrete protocol correction.** Before prose drafting, require a versioned **state-contract matrix** as a durable artifact. One row per field/state bit must contain:

- stable field identifier and standard/posture applicability;
- type and exact value domain, including `NA`, empty, invalid, and zero-length rules;
- source slice, parsed semantic value, canonical serialization value, and display value, kept distinct;
- delimiter/presence state and payload state;
- Stage A/Stage B ownership, cache participation, and whether repair/mutation may change it;
- public object exposure, 18-field compatibility projection, accessor projection, and diagnostic-only status;
- invariants across parse → validate → repair → mutate → serialize → format → clean → key;
- a fixture and assertion that proves each representable state survives without reparsing discarded source.

Add a gate: no product prose is complete until every serializer and mutator input is supplied from the canonical record, not recovered from `original_url` after state loss.

**Open v3 owner decision.** Is the canonical URL record a public scalar/vector class, an internal record behind public functions, or both? Is the v2 18-field shape retained as a compatibility projection, deprecated, or replaced in 3.0?

### S1-F2 — BLOCKER: “standards serialization” is not separated contractually from lossy `clean_url`

**Protocol proposal.** Section 6.3 requires faithful serialization by standard and preservation of fragments, credentials, and empty delimiters (`_scratch/url-v3-spec-reconstruction-protocol.md:142-151`). The representative fixture includes credentials plus empty query and fragment delimiters (`_scratch/url-v3-spec-reconstruction-protocol.md:221-235`). This direction is sound but is not yet precise enough to identify the object a standards serializer consumes or the equivalence it promises.

**Accepted intent.** ADR 0012 says query/fragment state is internal, standard serializers retain trailing `?`/`#`, and `clean_url` still excludes fragments (`design/adr/0012-general-url-parser-scope.md:262-268`). This already implies two different products, even though their API and state contracts were not fully specified.

**Shipped behavior.** The functions named `.serialize_whatwg_vec()` and `.serialize_rfc_generic_vec()` explicitly implement the **clean_url** contract and always exclude fragments (`R/parse-phases.R:2425-2436`). Their signatures accept neither fragment nor userinfo/credentials nor `authority_kind` (`R/parse-phases.R:2445-2461`, `R/parse-phases.R:2548-2564`); authority emission is inferred from `host_kind` (`R/parse-phases.R:2521-2534`, `R/parse-phases.R:2570-2588`). `get_clean_url()` likewise documents that fragment and userinfo are always excluded (`R/accessors.R:160-180`). Tests correctly characterize those helpers as state-level clean_url serializers, explicitly excluding fragments (`tests/testthat/test-parse-serializers.R:1-9`, `tests/testthat/test-parse-serializers.R:73-88`). They are not evidence for a full standards serializer.

**Credentials are already lost on a reachable branch.** `.split_authority()` recovers the text before the last `@` (`R/parse-state.R:581-614`), but `.parse_opaque_url_one()` omits it from its returned record (`R/parse-state.R:672-679`, `R/parse-state.R:793-799`). `.general_parse_vec()` only populates `userinfo` for the RFC `file:` overlay (`R/parse-state.R:1058-1081`, `R/parse-state.R:1109-1129`), and Stage A documents the same restriction (`R/parse.R:1734-1753`). Thus `foo://u:p@h/x?y#z` under either general posture currently exposes no user/password and cleans to a string without credentials or fragment. The repeated-`@` acceptance test verifies host recovery but not credential preservation (`tests/testthat/test-general-acceptance.R:51-66`).

**Concrete protocol correction.** Require separate named contracts and APIs for:

1. `serialize_url(object, standard=...)`: standards serialization from the canonical record, never SEO cleaning, with explicitly defined structural round-trip equivalence;
2. `format_url(object, ...)`: human-readable, possibly non-identity-preserving display;
3. `get_clean_url()`/`clean_url(...)`: intentionally lossy SEO output with an enumerated loss budget;
4. `get_url_key()`: a non-URL comparison projection.

The serializer gate must test `parse(serialize(parse(x)))` for structural-state equivalence, not necessarily source-byte equality. It must include credentials (absent, empty user, empty password, present colon, multiple raw/encoded `@`, encoded colon), query/fragment absent versus present-empty, empty host, authority without host, explicit empty port where the posture admits it, and encoded structural delimiters. Cleaning tests must be separate and must never serve as serializer conformance tests.

**Open v3 owner decisions.** Define whether RFC userinfo remains one undivided source-preserving component or gains a convenience user/password projection; define WHATWG credential normalization; define the behavior for syntactically accepted but diagnostically invalid credentials; and define whether “faithful” means exact source spelling, standard canonical serialization, or structural equivalence for each posture.

### S1-F3 — BLOCKER: scalar/vector behavior is a checklist item, not a normative behavior table

**Protocol text.** Section 6.1 says scalar, vector, missing, invalid, and zero-length behavior must be covered (`_scratch/url-v3-spec-reconstruction-protocol.md:124-130`); the fixture list asks for one mixed high-volume vector (`_scratch/url-v3-spec-reconstruction-protocol.md:221-235`); the red team checks scalar/vector/invalid-row/duplicate/error behavior (`_scratch/url-v3-spec-reconstruction-protocol.md:278-295`). No artifact is required to decide call-level errors versus row-local failures, coercion, shape, names, warning transport, or duplicate semantics.

**Shipped behavior is not one contract.** The scalar parser rejects length other than one before parsing and returns `NULL` for scalar `NA`, empty, or non-character input (`R/parse.R:431-477`, `R/parse.R:1441-1460`; characterized at `tests/testthat/test-accessors.R:137-148`). The vector parser returns a typed zero-row data frame for zero length, converts factors to labels, preserves order and duplicates through unique-parse expansion, and maps unparseable rows to error rows (`R/parse.R:585-625`, `R/parse.R:709-754`, `R/parse.R:757-763`). Non-character atomic scalars are stringified only for `original_url`, while parse input becomes `NA` (`R/parse.R:721-743`, `R/parse.R:808-824`). A list element that is a character vector of length greater than one contradicts the comment promising a row-local error: `.spu_coerce_original()` returns that vector, causing the surrounding `vapply(..., character(1))` to fail at call level (`R/parse.R:721-743`, `R/parse.R:808-824`). Accessors reject every non-character input at call level, strip names, and return vector-shaped projections (`R/accessors.R:27-108`); tests cover ordinary type/name behavior but not the full mixed failure matrix (`tests/testthat/test-accessor-helper.R:4-69`).

**Minimal divergence examples.** `safe_parse_url(1)` returns `NULL`; `safe_parse_urls(1)` returns a one-row error data frame whose `original_url` is `"1"`; `get_host(1)` errors. `safe_parse_urls(factor("example.com"))` parses the label. `safe_parse_urls(list("a", c("b", "c")))` aborts the entire call rather than returning one success and one error row. `safe_parse_url(character())` errors, while `safe_parse_urls(character())` returns the canonical zero-row frame and `get_host(character())` returns `character(0)`.

**Concrete protocol correction.** Require a **scalar/vector contract matrix** for every public parse/serialize/format/clean/key/mutate/accessor API. Rows must cover `NULL`, `character(0)`, character scalar/vector, `NA_character_`, `""`, invalid syntax, factor, numeric/logical/date-like atomic input, named vectors, lists, list elements of length 0/1/>1, and mixed vectors. Columns must state:

- accepted input classes and coercion, including preservation of `original_url`;
- return container, field types, length, row/name preservation, and zero-length shape;
- call-level validation error versus row-local parse/validation result;
- status/diagnostic/warning/error transport and partial-failure rules;
- order, duplicate, and cache transparency guarantees;
- scalar/vector parity rule, including how scalar failure projects from a one-row vector result.

Add generated conformance tests over that table. At least one vector fixture must interleave valid, warning, invalid, `NA`, empty, coerced, duplicate, and non-scalar-list cases, then repeat in a different order against cold and warm caches.

**Open v3 owner decision.** Should v3 enforce character-only input consistently, provide explicit coercion consistently, or retain today’s three different policies as compatibility behavior? Should scalar parse failure return `NULL`, a typed invalid URL object, or a one-row result with status?

### S1-F4 — HIGH: the protocol does not require an exhaustive, mechanically checked behavior-dial registry

**Accepted intent.** ADR 0007 says rurl has roughly 20 low-level knobs, distinguishes governed from standalone axes, and requires parse-affecting axes in the Stage A key (`design/adr/0007-url-standard-selector.md:9-24`, `design/adr/0007-url-standard-selector.md:44-56`). ADR 0011 splits internal `path_identity` from public `path_encoding` and assigns both to Stage B (`design/adr/0011-path-encoding-orthogonal-presentation.md:47-66`, `design/adr/0011-path-encoding-orthogonal-presentation.md:78-96`). ADR 0003 requires every new dial to be classified as parse-affecting or presentation-only (`design/adr/0003-parse-present-stage-split.md:17-38`).

**Shipped public parse dial inventory.** Both `safe_parse_url()` and `safe_parse_urls()` expose the same 24 behavior/configuration formals after `url`: `protocol_handling`, `www_handling`, `tld_source`, `case_handling`, `trailing_slash_handling`, `index_page_handling`, `path_normalization`, `scheme_relative_handling`, `subdomain_levels_to_keep`, `host_encoding`, `path_encoding`, `query_handling`, `params_keep`, `params_drop`, `sort_params`, `empty_param_handling`, `params_case_sensitive`, `decode_plus`, `port_handling`, `scheme_policy`, `scheme_acceptance`, `url_standard`, `engine`, and `profile` (`R/parse.R:431-469`, `R/parse.R:585-625`). `.parse_options()` adds or derives internal `fixup_posture`, `path_identity`, and `profile_authorized` (`R/parse.R:1224-1250`, `R/parse.R:1253-1281`, `R/parse.R:1322-1338`). Profiles and the `canonical` alias add bundle/override precedence (`R/parse.R:926-1013`, `R/parse.R:1040-1057`; `R/profiles.R:16-39`). Stage A’s cache key currently includes URL, protocol, www, TLD source, scheme-relative handling, standard, scheme policy, scheme acceptance, fixup posture, and engine identity (`R/parse.R:1418-1438`).

**Shipped projection-only dials.** Accessors add `source` (an alias/projection of `tld_source`), query `format` and `decode`, subdomain `include_www` and `format`, and query-summary `level`; they expose non-uniform subsets of the parser dials (`R/accessors.R:131`, `R/accessors.R:228`, `R/accessors.R:377`, `R/accessors.R:430`, `R/accessors.R:485`, `R/accessors.R:534`, `R/accessors.R:617`, `R/accessors.R:813`, `R/accessors.R:827`, `R/accessors.R:856`, `R/accessors.R:880`, `R/accessors.R:895`, `R/accessors.R:995`, `R/accessors.R:1056`, `R/accessors.R:1107`, `R/accessors.R:1194`, `R/accessors.R:1261`, `R/accessors.R:1323`). A concrete gap is `get_password()`, which exposes only `protocol_handling`, so it cannot request `scheme_acceptance="general"`/`url_standard` even if a future general parser retains passwords (`R/accessors.R:869-882`).

**The existing registry cannot be treated as complete evidence.** Its supposed ground truth contains only 11 legacy options (`tests/testthat/test-accessor-registry.R:65-81`) and its completeness tests quantify only over that hand-written subset (`tests/testthat/test-accessor-registry.R:651-683`, `tests/testthat/test-accessor-registry.R:685-734`). It omits the query family, port, scheme axes, standard, engine, and profile.

**Concrete protocol correction.** Require a machine-readable **dial registry**, derived/checked against live public formals and internal option construction. For every dial it must record allowed values/default, semantic category (acceptance, interpretation, repair, identity, presentation, cleaning, annotation, infrastructure), applicable standards/schemes/path/host forms, profile bundle values and explicit-override precedence, conflict rules, Stage A/B ownership, cache-key membership, affected fields/APIs, accessor exposure or intentional omission, diagnostics on skipped/ineligible transforms, and migration disposition. Add CI that fails on an unregistered formal or a registered dial absent from every consumer/cache decision.

### S1-F5 — HIGH: authority state has contradictory accepted/shipped meanings and no required owner resolution

**Accepted intent.** ADR 0012 gives both `host_kind` and `authority_kind` the vocabulary `{absent, empty, present}` (`design/adr/0012-general-url-parser-scope.md:230-255`) but then defines `authority_kind` as recording whether `//` was present and labels `foo:///bar` authority-present/host-empty (`design/adr/0012-general-url-parser-scope.md:257-261`). Those statements do not define a reachable meaning for `authority_kind="empty"`.

**Shipped contradiction.** `.AUTHORITY_KIND` contains all three values (`R/parse-state.R:35-47`), but `.authority_kind()` emits only absent/present and comments call empty “reserved” (`R/parse-state.R:92-110`). The general parser also makes every `//` row authority-present (`R/parse-state.R:725-746`). The RFC `file:` overlay instead emits authority-empty when the substring between `//` and the path is empty (`R/parse-state.R:905-919`). Both standard serializers ignore `authority_kind` and infer `//` solely from `host_kind` (`R/parse-phases.R:2521-2534`, `R/parse-phases.R:2570-2576`). The state tests enforce the two-value “whether `//`” interpretation (`tests/testthat/test-parse-state.R:75-81`) and do not expose the file divergence.

**Minimal contradiction.** `foo:///bar` and `file:///bar` each contain `//` followed by an empty authority substring before `/`; the generic classifier calls the first authority-present, while the RFC file overlay calls the second authority-empty. The protocol’s phrase “absent versus present-empty … authority” (`_scratch/url-v3-spec-reconstruction-protocol.md:124-129`) does not resolve which vocabulary is normative.

**Concrete protocol correction.** Put this in the contradiction register and require an owner decision before state prose. Recommended model: separate `authority_delimiter_present: logical` from `authority_payload_kind: empty|present`, then retain independent `host_kind`; serializers must consume the delimiter fact rather than infer authority from host. If the three-value `authority_kind` is retained instead, define every value operationally and prove all combinations with userinfo-only, empty-host, port-only, and empty-authority fixtures.

### S1-F6 — HIGH: top-level query presence is modeled, but mutation-grade query structure is not

**Protocol proposal.** The protocol requires absent/present-empty query and “related delimiters,” symmetric query-parameter mutation, and preservation of encoded structural delimiters (`_scratch/url-v3-spec-reconstruction-protocol.md:124-129`, `_scratch/url-v3-spec-reconstruction-protocol.md:153-163`, `_scratch/url-v3-spec-reconstruction-protocol.md:142-150`). It does not require a query sequence model capable of meeting all three.

**Shipped behavior.** The public raw query can remain a faithful whole string, but the grouped query parser collapses trailing `=` distinctions and skips empty `&` segments (`R/path-query.R:294-331`). The ordered pair parser retains duplicate order but explicitly maps bare `a` and `a=` to the same empty value and skips empty inter-`&` segments (`R/path-query.R:351-378`). Therefore mutation/filtering cannot reassemble all accepted source structures from its parsed pair state.

**Minimal example.** `?a&&a=&=x&` contains a bare key, an empty pair, a key with an explicit empty value, an empty key, a duplicate, and a trailing separator. A key/value-list representation cannot preserve those distinctions.

**Concrete protocol correction.** Require an ordered query-token sequence with, at minimum, raw key/value spelling, `equals_present`, pair-separator placement (including empty pairs and trailing separator), duplicate order, decoded comparison views, and malformed-percent opacity. Specify which operations preserve untouched token spelling and which deliberately canonicalize. Add parse/mutate/serialize fixtures for bare versus `=`, repeated keys, empty keys/values/pairs, `+`, malformed percent escapes, and encoded `&`/`=`.

### S1-F7 — HIGH: “list path” is a label over a string, not a specified path data model

**Accepted intent.** ADR 0012 requires posture-specific WHATWG `path_kind={opaque,list}` and RFC path forms (`design/adr/0012-general-url-parser-scope.md:230-255`, `design/adr/0012-general-url-parser-scope.md:270-282`). ADR 0011 distinguishes identity from knowingly lossy path presentation (`design/adr/0011-path-encoding-orthogonal-presentation.md:19-45`, `design/adr/0011-path-encoding-orthogonal-presentation.md:78-93`).

**Shipped behavior.** The WHATWG serializer says rurl “holds a list path as its serialized STRING” and reconstructs segments with `strsplit`; it also notes the split drops a trailing empty segment (`R/parse-phases.R:2493-2506`). The public `path` remains a scalar string. This is sufficient for some current cleaning cases but is not a first-class list-path object suitable for reliable component mutation and equality.

**Minimal example.** `/a//b/`, `/a/%2F/b`, `/`, empty path, and the null-host list path `//bar` differ in segment boundaries, leading/trailing empty segments, and structural interpretation. A scalar path plus a `path_kind` label does not specify how those identities survive segment mutation.

**Concrete protocol correction.** Require posture-specific path schemas: WHATWG opaque payload versus an ordered segment vector retaining leading/trailing empty segments and separator multiplicity; RFC raw path plus its grammar form and segment view where useful. For every path representation, specify source spelling, percent-octet identity, dot-segment timing, serialized form, display projection, and mutation operations. The public scalar `path` must be identified as a projection, not the canonical list representation.

### S1-F8 — HIGH: accepted eligibility rules and shipped dial consumption conflict

**Accepted intent.** ADR 0012 limits automatic semantic transforms to HTTP(S) plus explicit `canonical`/`seo` opt-in and limits hierarchical path transforms to HTTP(S) hierarchical/list paths under that gate; RFC built-in DNS-eligible reg-names also qualify for DNS work (`design/adr/0012-general-url-parser-scope.md:275-291`). Its implementation layer repeats “HTTP(S)+profile semantic-transform eligibility” (`design/adr/0012-general-url-parser-scope.md:635-640`) and standing rules repeat explicit profile opt-in (`design/adr/0012-general-url-parser-scope.md:725-736`).

**Shipped behavior.** `.stage_b_eligibility()` has no profile/profile-opt-in input, deliberately ignores `url_standard`, marks every non-opaque list path eligible regardless of scheme, marks semantic transforms eligible for HTTP(S) without profile state, and recognizes host transformation only via WHATWG-special scheme membership (`R/parse-state.R:225-293`). Stage B acknowledges that Stage-A `www_handling` and PSL derivation are not gated there (`R/parse.R:1920-1931`). This can neither encode the full accepted matrix nor prove a dial was skipped for the accepted reason.

**Concrete protocol correction.** The dial registry must include an explicit eligibility predicate for every transform and an explicit source of authorization (standard-required, low-level explicit request, or named cleaning profile). Require a generated cross-product fixture over posture × scheme class × path kind × host form × profile/explicit override. Any intentional change to ADR 0012 must be marked `SUPERSEDED`, not silently described as current behavior.

### S1-F9 — MEDIUM: source spelling, parsed identity, serialization, display, and cleaning are named but not field-mapped

**Protocol proposal.** The protocol correctly names source spelling versus normalized identity (`_scratch/url-v3-spec-reconstruction-protocol.md:124-129`) and forbids display from serving as serialization/equality (`_scratch/url-v3-spec-reconstruction-protocol.md:142-151`). It does not require component-by-component representations or equality rules.

**Shipped evidence.** The public result mixes projections: `original_url`; selected host spelling; four stable domain/TLD identity spellings (`R/utils.R:215-240`); raw-looking query/fragment/user/password; transformed path/scheme/host; and `clean_url`. Stage B percent-encodes public WHATWG query and fragment (`R/parse.R:2034-2055`) while the assembler still describes them generically as raw (`R/parse-phases.R:2735-2747`). ADR 0011 explicitly says `path_encoding=encode/decode` may not preserve identity (`design/adr/0011-path-encoding-orthogonal-presentation.md:78-90`).

**Concrete protocol correction.** Extend the state-contract matrix with four non-overloadable columns for each component: immutable input/source slice, parsed semantic identity, standards serialization value, and display value; add a fifth cleaning projection only where applicable. Define equality over identity, never over display or `clean_url` by default. Define encoding tags/UTF-8 validity and whether source spelling means bytes, R characters, or both.

### S1-F10 — MEDIUM: current tests are useful characterization, but the protocol does not mandate oracle classification plus metamorphic state assertions

**Shipped test floor.** The characterization snapshot explicitly freezes current behavior, known bugs included (`tests/testthat/test-characterization-snapshot.R:1-18`). Scalar/vector parity runs only over the character corpus and fills scalar `NULL` with public defaults (`tests/testthat/test-characterization-snapshot.R:48-82`). Its raw round-trip proof deliberately excludes userinfo, fragment, and default-port “weirdness” (`tests/testthat/test-characterization-snapshot.R:84-102`). State tests hand-construct four tuples rather than parse/serialize them (`tests/testthat/test-parse-state.R:1-11`); serializer tests hand-construct clean_url state and exclude fragments (`tests/testthat/test-parse-serializers.R:1-9`). General acceptance tests prove four public `clean_url` strings (`tests/testthat/test-general-acceptance.R:68-85`) but not complete state retention. Accessor credentials have one ordinary HTTP happy path (`tests/testthat/test-accessors.R:492-501`).

**Concrete protocol correction.** For every fixture, require an oracle label (`v2-characterization`, `accepted-ADR contract`, `external conformance`, `proposed-v3`, or `regression`) and the stable requirement ID it proves. Add metamorphic assertions over canonical state: parse→serialize→parse equivalence, format non-authority, clean loss-budget, mutation preservation of untouched components, scalar/vector parity, and cache transparency. Snapshot equality alone must not satisfy a standards or state-preservation gate.

### S1-F11 — MEDIUM: cache transparency is named only indirectly and state reconstruction from source remains possible

**Accepted intent.** ADR 0003 divides parse-affecting Stage A state from presentation-only Stage B and requires every new knob to be classified (`design/adr/0003-parse-present-stage-split.md:17-38`). The architecture describes the same split and cache-key policy (`ARCHITECTURE.md:130-158`).

**Shipped behavior.** Unique inputs are cached/expanded back to duplicates and `NA`/empty are never cached (`R/parse.R:1480-1559`), but richer state is absent from the cache record and reparsed from original input in Stage B (`R/parse.R:1932-1958`). That makes cache correctness depend on an unstated equivalence between the Stage A parser and a later state parser.

**Concrete protocol correction.** State that cache use must be observationally transparent for the complete canonical object, diagnostics, serialization, and row-local failures. Require cold/warm/permuted-duplicate tests across every parse-affecting dial and require all identity-bearing structural state in the cache value or in a demonstrably equivalent immutable parse record—not reconstructed from a source string after mutation or repair.

## Required protocol additions

The smallest protocol amendment that closes the blockers is to add these mandatory artifacts before the successor PRD is drafted:

| Artifact | Required content | Graduation gate |
|---|---|---|
| State-contract matrix | Canonical fields, types/enums, delimiter bits, source/identity/serialization/display values, posture applicability, pipeline ownership, cache/public/accessor projections | Every state has a fixture; serializer/mutator consume the canonical record; no reconstruction from discarded source |
| Scalar/vector contract matrix | Input classes/shapes, coercion, row-local versus call-level failure, return type/shape/names, partial failure, duplicates/order, warning/diagnostic transport, zero length | Generated tests cover every cell for all public API families |
| Dial registry | All public/internal/profile/accessor dials, defaults/values, semantic category, eligibility, conflict/override rules, stage/cache membership, affected outputs, migration | Checked mechanically against formals/options/profile bundles/cache keys and consumers |
| Structural round-trip matrix | Authority, host, path, userinfo/credentials, port, query, fragment, source spelling, both standards | Parse→standards-serialize→parse preserves declared structural equivalence |
| Oracle register | Fixture classification and requirement mapping | Characterization snapshots cannot be cited as conformance without reclassification |

These artifacts should be linked from the existing decision ledger rather than replacing it. The protocol already requires ledger rows and a contradiction register (`_scratch/url-v3-spec-reconstruction-protocol.md:82-106`); it should explicitly require the authority contradiction and v3 public-object decision to remain `OPEN`/`CONFLICT` until owner resolution.

## Minimum state cross-product for acceptance

The protocol’s fixture list is a useful start, but the state contract needs explicit cross-products rather than one overloaded example. At minimum:

- authority delimiter absent/present × authority payload empty/present × host absent/empty/present, including userinfo-only and port-only candidates where each posture permits them;
- userinfo absent/empty/present × colon absent/present × password absent/empty/present × raw/encoded `@` and `:`;
- query and fragment independently absent/empty/present, including all four `?`/`#` combinations and encoded delimiters;
- WHATWG opaque versus list path; RFC empty/absolute/rootless/abempty; empty path, `/`, repeated slashes, trailing empty segment, dot segments, and encoded `/`;
- source scheme/host/percent hex casing and Unicode/A-label spellings versus parsed identity and standard serialization;
- scalar/vector input matrix, including order, duplicates, mixed row outcomes, names, zero length, and cold/warm cache runs.

## Questions requiring owner judgment

1. What is the v3 public object/container, and what is the fate of the v2 scalar list and 18-column data frame?
2. Is v3 character-only, explicitly coercing, or intentionally different across scalar/vector/accessor entry points? What is the scalar failure value?
3. Should authority state be split into delimiter presence plus payload state (recommended), or retain a three-value `authority_kind`; if retained, what does `empty` mean?
4. What exact structural equivalence does each standards serializer guarantee, and which source spellings may canonicalize?
5. Is RFC userinfo an undivided component with optional convenience parsing, or a normative user/password pair? What credential states are public?
6. Does a query mutation model preserve empty pairs, separator placement, bare-key versus explicit `=`, duplicate order, and untouched raw spelling?
7. Is a WHATWG list path stored canonically as segments in v3, with public string access as a projection?
8. Must all automatic SEO transforms require named `seo`/`canonical` profile authorization as ADR 0012 says, or may explicit low-level knobs authorize them? Record any amendment to ADR 0012.
9. Are existing accessor-specific omissions compatibility decisions, accidental gaps, or to be replaced by object-first accessors?

## Non-blocking editorial improvements

- Replace generic phrases such as “related delimiters,” “faithful,” “invalid,” and “round-trip” with linked terminology entries and stable requirement IDs.
- In the example table, add separate “source-equivalent?”, “structurally equivalent?”, and “intentionally lossy?” assertions instead of a single output string.
- Correct stale 14-column comments during implementation planning; the canonical public registry is 18 fields (`R/utils.R:215-241`).
- State explicitly that `clean_url` tests, display snapshots, and accessor projection tests cannot establish full-object serializer conformance.
- Add an explicit rule that accepted v2 ADRs remain binding evidence but may be superseded by a recorded v3 owner decision; do not inherit “public shape frozen” invisibly into a breaking major.

## Final assessment against protocol acceptance criteria

The protocol already satisfies authority ordering, shipped-versus-intent separation, durable contradiction tracking, phased review, and stable traceability in principle (`_scratch/url-v3-spec-reconstruction-protocol.md:36-116`, `_scratch/url-v3-spec-reconstruction-protocol.md:266-276`). For this slice, however, the answer to “does it cover the full semantic surface?” and “does it make every normative requirement traceable and testable?” is **not yet yes** (`_scratch/url-v3-spec-reconstruction-protocol.md:319-334`). Add the five required artifacts and resolve—or visibly retain—the nine owner questions before drafting normative object/state or scalar/vector prose.
