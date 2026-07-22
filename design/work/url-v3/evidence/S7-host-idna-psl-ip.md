# S7 review — host, IDNA, PSL, DNS, and IP semantics

## Verdict

**Not yet reconstructible.** The protocol identifies nearly all of the relevant subjects, but it does not yet force a future author to define their relationships precisely. In particular, it can be satisfied by prose that uses `domain` for three different things, reports one `NA` for several different annotation outcomes, and lets an optional PSL lookup influence a parse-validity status. That would reproduce the current semantic collisions rather than reconstruct a coherent v3 contract.

The blocking requirement is a normative host-state and annotation matrix. Each accepted URL needs one syntax/parser classification and zero or more explicitly eligible annotations. Syntax validity, URL-standard host validity, DNS owner-name policy, PSL registrability, name resolution, source spelling, normalized identity, and display are separate axes. Optional annotations must not retroactively change the result of parsing.

The protocol is directionally strong: sections 6.1, 6.2, 6.3, 6.7, and 6.8 call out source spelling, normalized identity, absent/empty hosts, standards vocabulary, IDNA/PSL/DNS distinctions, display, laziness, cost, cache behavior, and determinism. Sections 7–10 also demand Unicode/ACE, numeric IP, malformed-host fixtures, host review, and overloaded-vocabulary red-teaming. The additions below turn those topics into testable reconstruction requirements.

## Coverage map

| Semantic concern | Protocol today | Evidence in the repository | Reconstruction judgment |
|---|---|---|---|
| Host presence and parser form | §6.1 and §6.7 name absent, present-empty, WHATWG domain/opaque/IPv4/IPv6 and RFC reg-name/IPv4/IPv6/IPvFuture | `R/parse-state.R` already carries `host_kind`, `authority_kind`, `whatwg_host_form`, and `rfc_host_form` | Good inventory, but no required cross-product or public projection |
| Source, normalized identity, serialization, display | §§6.1 and 6.3 distinguish them | `R/parse-phases.R`, `R/parse.R`, ADR-0002, and ADR-0012 implement several distinct spellings | Directionally covered; exact identities and failure behavior remain unspecified |
| URL-standard host validity | §6.7 requires WHATWG/RFC terminology | `R/parse-state.R`, `R/parse-phases.R`, ADR-0004, ADR-0009, ADR-0012, and standard conformance tests | Needs a mandatory state matrix and oracle ownership |
| IDNA | §6.7 names U-label/A-label; §§6.1/6.2 mention normalization and optional facts | WHATWG processing uses `punycoder::host_normalize()` with relaxed DNS checks; rendering helpers are deliberately lenient | Insufficient: algorithm/profile, options, eligibility, outputs, and failures are not fixed |
| DNS-shaped policy | §6.7 says syntax, DNS, registrability, and resolution are distinct | `R/host-policy.R`, `.punycoder_host_probe()`, host-policy PRD, URL-standard-selector-v2 PRD | Insufficient: owner-name policy, LDH policy, length basis, root dot, and result states need a contract |
| PSL registrability | §§6.2, 6.7, 6.8 mention PSL, optional facts, cost, and caches | `R/domain.R`, `R/parse-phases.R`, ADR-0001, `pslr` parity tests | Insufficient: source/list identity, section policy, result states, provenance, and parse-status independence are missing |
| Name resolution | §6.7 distinguishes resolution | Current host policy and mailto docs explicitly avoid DNS resolution/deliverability | Needs an explicit no-network/default boundary and a separately authorized annotation if ever added |
| IP literals and numeric hosts | §6.7 names IP forms; §7 requests unusual numeric hosts | `R/parse-phases.R`, ADR-0004/0012, host/conformance tests | Needs exact source/identity/serialization and annotation-ineligibility requirements |
| Lazy cost and cache behavior | §§6.2 and 6.8 require meaning, cost, caching, determinism | `ARCHITECTURE.md`, `R/parse.R`, `R/domain.R`, diagnostics code | Insufficient: no annotation-state model, dependency provenance, invalidation, or process-local engine rule |

## Existing semantic surface that v3 must account for

### 1. Syntax and host-state models

The implementation already contains two distinct syntax models:

- WHATWG special-scheme hosts use the domain/IPv4/IPv6 host parser. WHATWG non-special hosts use the opaque-host parser. Empty and absent hosts are represented internally, even though both can project to `NA` publicly.
- RFC syntax recognizes reg-name, IPv4, IPv6, IPvFuture, and empty host forms. It preserves a reg-name's source spelling rather than implicitly applying IDNA. Direct non-ASCII reg-names are accepted as a documented RFC-syntax posture, not claimed as IRI conformance.
- `file:` has its own authority/empty-host behavior. Hostless and opaque URLs do not become DNS names merely because a host-like substring exists elsewhere in the URL.
- Stage-B host transformations are eligible for WHATWG special, present, non-IP domain hosts. Opaque hosts, IP literals, absent hosts, and empty hosts are intentionally ineligible. ADR-0012 describes the same separation as a design invariant.

These classifications are not equivalent to public `host_type`. `R/diagnostics.R` currently derives public `host_type = "domain"` from the presence of a PSL-derived registered domain. By contrast, internal WHATWG `host_form = "domain"` means the syntactic host parser/form, whether or not the PSL knows its suffix. Public `domain` and `tld` columns also mean PSL-derived registrable domain and suffix, not the complete host. A v3 specification that says only “domain host” will therefore be ambiguous.

### 2. Current IDNA and Unicode behavior

There are at least three different operations that must remain distinct:

1. WHATWG domain parsing uses `punycoder::host_normalize()` as the domain-to-ASCII gate with `check_hyphens = FALSE`, `use_std3 = FALSE`, and `verify_dns_length = FALSE`. Alternate full-stop characters are mapped in the authority before numeric/IP classification. UTS-46 ignored and compatibility mappings are accepted; invalid join-control cases fail. DNS-length checking is deliberately not part of URL validity.
2. `.normalize_and_punycode()` supplies a reversible/display-oriented A-label conversion. It tries strict encoding and then a lenient fallback.
3. `.punycode_to_unicode()` is a best-effort per-label display decoder. Undecodable labels remain in their original form rather than making URL parsing fail.

ADR-0002 explicitly rejects treating the display helpers as canonical `host_normalize()` semantics. RFC reg-name and WHATWG opaque-host handling also do not imply IDNA. Consequently, “IDNA normalization” is not one operation and cannot be specified as one Boolean capability.

### 3. Current PSL behavior

`pslr` owns suffix and registered-domain knowledge. The wrapper contract currently selects the `all`, `icann`, or `private` section from `tld_source`; requests Unicode output by default; asks for `NA` for unknown and invalid inputs; and optionally accepts a process-local `pslr::psl_engine` external pointer. The default engine is session-global, and an explicit engine is not serializable. Stage-A cache keys include the selected PSL source and an engine identity token.

The public table exposes four stable PSL spellings (`domain_ascii`, `domain_unicode`, `tld_ascii`, and `tld_unicode`) plus rendering-selected `domain` and `tld`. These are registrability annotations. They are not full-host normalized identities, proof that a name is registered, proof that it exists, or proof that it resolves.

The historical `parse_status` nevertheless includes `warning-no-tld`, `warning-invalid-tld`, and `warning-public-suffix`. In `R/parse-phases.R`, a syntactically accepted non-IP host can move among those statuses depending on PSL-derived results. `man/get_parse_status.Rd` documents that those warnings depend on the PSL section. This is exactly the validity/optional-fact conflation that §6.2 asks v3 to resolve; it must be an explicit owner decision, not left to inference.

### 4. Current DNS-shaped facts and policy

The repository deliberately has two layers:

- `.punycoder_host_probe()` computes selected host facts under an all-relaxed A-label baseline, then enables one independent punycoder check at a time. It reports label/name length, empty-label, hyphen, and STD3 facts. It is diagnostic evidence, not a WHATWG or RFC conformance oracle. It excludes IP hosts.
- `is_valid_host()` applies named product policies. `url` tracks parser acceptance; `dns` permits underscore-bearing owner names; `web` requires an LDH-like name or IP literal; `registrable` requires a PSL-derived registered domain; and `seo` composes web/registrability constraints. It does not perform network resolution or ownership checks.

The length facts are calculated on A-label octets; a terminal root dot is excluded from the 253-octet name limit. `use_std3` is a strict superset of the WHATWG forbidden-domain-code-point rule, so an STD3 failure is not a URL parse failure. DNS owner-name policy, browser hostname convention, and URL syntax are therefore intentionally non-equivalent.

### 5. Current IP behavior

WHATWG and RFC syntax disagree intentionally for unusual numeric text. WHATWG recognizes decimal-integer, hexadecimal, octal, and shortened IPv4 forms and serializes them as a canonical dotted quad; an invalid host that “ends in a number” is fatal. RFC syntax treats non-canonical numeric-looking text as a reg-name and recognizes only canonical dotted-quad IPv4 as an IPv4 form. WHATWG canonicalizes IPv6, including embedded dotted IPv4, while the RFC posture preserves the source spelling. IPvFuture is an RFC bracketed form and is not accepted by the WHATWG IPv6 parser.

IP literals are ineligible for IDNA, PSL, DNS-label-shape probes, and registered-domain extraction. Source-shaped numeric diagnostics can still describe how an input was interpreted, but they are facts about the input and parser choice, not domain annotations.

## Findings ordered by severity

### S7-F1 — BLOCKER: no normative host-state and semantic-axis matrix

Sections 6.1 and 6.7 list the right categories, but the protocol does not require the final specification to cross them. A writer could describe absent hosts, opaque hosts, domains, reg-names, and IPs in separate paragraphs without saying which parser produced which identity, which serialization is standard-owned, or which optional operation is eligible. ADR-0012 already provides most of the conceptual matrix; the protocol should require a v3 replacement rather than treating the matrix as optional evidence.

The required matrix must cross at least:

`URL standard × scheme/parser class × authority state × host_kind × host_form`.

Each reachable row must state:

- source host token and whether source spelling is retained;
- syntax parser and URL-validity result;
- normalized comparison identity, standard serialization, and display spelling;
- public/API projection, including absent versus present-empty behavior;
- IDNA, DNS-policy/fact, PSL, and resolution eligibility;
- diagnostics and compatibility projections;
- whether the state is reachable only as an intermediate or recovery state.

Impossible combinations must be rejected or marked unreachable. Without this, the protocol cannot prove that an opaque WHATWG host, an RFC reg-name, a WHATWG domain host with an unknown suffix, and a PSL-registrable name remain distinct.

### S7-F2 — BLOCKER: `domain` vocabulary is overloaded across syntax, registrability, and public type

The current semantic surface uses:

- WHATWG `host_form = "domain"` for a syntactic host form;
- public `host_type = "domain"` for a non-IP host with a PSL-derived registered domain;
- `domain*` columns for the PSL registered-domain value;
- informal “domain” for a DNS-like name or an IDNA input.

Those meanings are observably different. For example, a single-label WHATWG special host is a WHATWG domain host but has no PSL registered domain and therefore does not receive public type `domain`. A WHATWG opaque host may contain host text but is not IDNA/PSL eligible. An RFC reg-name may be syntactically accepted without being a DNS-valid or registrable name.

The protocol currently asks reviewers to inspect overloaded vocabulary but does not reserve replacement terms. The v3 protocol must require a glossary with non-overlapping names and require every public field to name its semantic layer. Recommended terms are `whatwg-domain-host`, `whatwg-opaque-host`, `rfc-reg-name`, `registered-domain` or `registrable-domain`, `public-suffix`, `dns-owner-name-policy`, and `resolution-result`. If compatibility retains `host_type = "domain"` or `domain*`, the specification must label those as legacy PSL projections and must not reuse them for syntactic classification or full-host identity.

### S7-F3 — BLOCKER: optional annotation outcomes and parse validity have no required state model

Section 6.2 says the meaning of validity must be defined when optional IDNA/PSL/DNS annotations are skipped. It does not require a representation that can distinguish:

- not requested;
- not applicable to this host form;
- known/successful;
- unknown to a knowledge source, such as an unlisted PSL suffix;
- invalid for the annotation's own policy;
- dependency/configuration failure.

The current PSL wrappers collapse unknown and invalid to `NA`; absent host, IP host, skipped lookup, unknown suffix, suffix-only input, and lookup failure can also reach superficially similar missing values elsewhere. Historical `parse_status` can then change because a PSL value is missing. This makes lazy execution semantically unsafe: skipping an optional lookup could change what looks like the parse verdict.

The v3 contract must require a typed annotation result with an explicit state and optional value, facts, and provenance. `NA` alone is not a sufficient semantic result. URL parse validity and URL-standard host validity must be determined before optional annotation execution and must remain invariant under annotation selection, cache warmth, PSL source/list version, or resolver availability. If historical PSL warning statuses are retained, they must be a separately named compatibility/policy projection rather than the core parse result.

### S7-F4 — HIGH: the IDNA contract is under-specified

Naming U-labels and A-labels does not identify an algorithm. A reconstructible specification must pin:

- which operation is standard-owned WHATWG domain-to-ASCII and which operations are presentation helpers;
- library/algorithm/profile/version assumptions and all behavior-affecting options;
- eligibility by host form and scheme/parser class;
- ordering relative to percent decoding, alternate-separator mapping, numeric-host classification, forbidden-code-point checks, and case normalization;
- treatment of UTS-46 ignored characters, compatibility mappings, join controls, disallowed code points, hyphen rules, STD3, and DNS length;
- success identity, output spelling, standard serialization, and display behavior;
- a typed failure result and whether a display conversion may retain the source label.

The repository specifically proves that `verify_dns_length = FALSE` is correct for WHATWG URL parsing, that `use_std3` is not the WHATWG forbidden-host rule, that full-stop mapping happens only in WHATWG authority scope, and that lenient A-label/U-label rendering is not canonical normalization. Those distinctions need normative clauses and fixtures.

### S7-F5 — HIGH: the PSL contract lacks knowledge-source identity and reproducibility rules

`all`, `icann`, and `private` are policy choices, not interchangeable data. A session-global default list and an explicit process-local engine can also produce different knowledge results over time. The protocol asks for deterministic caching but does not require the specification to record or key on:

- PSL section policy (`all`, `icann`, `private`);
- list/engine identity and version/fingerprint;
- Unicode versus ASCII lookup/output spelling;
- unknown, invalid, suffix-only, and not-applicable results;
- explicit-engine lifecycle, serialization limits, and cross-process behavior;
- cache invalidation when a default list changes.

The spec must say that a PSL registered-domain result means “registrable according to this selected list and section,” not registered, owned, reachable, safe, or resolvable. Values derived under one list identity must not silently be reused under another. Compatibility output may hide provenance, but the semantic model and conformance fixtures may not.

### S7-F6 — HIGH: DNS syntax, product policy, and resolution need separate contracts

The protocol requires reviewers to distinguish DNS validity from resolution, but it does not require the final spec to define which “DNS validity” it means. The repository contains at least:

- label/name shape facts measured after IDNA-to-ASCII;
- an owner-name policy that permits underscores;
- an LDH-like browser/web hostname policy;
- PSL registrability policy;
- URL parser acceptance, which is wider than those policies.

The v3 specification must define the input spelling used for each fact, root-dot handling, empty-label handling, 63-octet label and 253-octet name measurements, the A-label-octet basis for Unicode input, underscore behavior, and the non-equivalence of STD3 with WHATWG validity. It must also state that default parsing and policy checks perform no network I/O. If live DNS resolution is ever offered, it must be a separately requested, timeout/error-bearing, non-deterministic annotation that cannot affect syntax validity, identity, canonical serialization, or PSL results.

### S7-F7 — HIGH: IP source, identity, serialization, and annotation eligibility are not explicit enough

The protocol mentions unusual numeric hosts but does not require the specification to distinguish input classification from normalized identity. Required rows include canonical dotted IPv4, shortened decimal, integer, hexadecimal, octal, invalid ends-in-number text, canonical and non-canonical IPv6, embedded dotted IPv4, and IPvFuture. For each, the spec must state WHATWG versus RFC acceptance, source retention, host form, normalized identity, standard serialization, display, and diagnostics.

It must also state categorically that IP and IPvFuture forms are not IDNA, DNS-label-shape, or PSL inputs. An API may expose IP-specific source diagnostics, but must not manufacture domain/TLD missingness as though the knowledge source simply did not know the suffix.

### S7-F8 — HIGH: laziness, cost, cache keys, and dependency failures are not reconstructible

Section 6.8 requests cost, cache semantics, and deterministic behavior, but the implementation demonstrates details the protocol does not yet force a writer to capture:

- the punycoder diagnostic probe can call normalization up to four times per unique eligible host and is currently outside the main parse caches;
- metadata/diagnostic helpers can reparse unique inputs;
- the PSL engine is an external pointer that is meaningful only in the process that created it;
- the session-global default knowledge source can change independently of URL text;
- Stage-A cache keys include standard, scheme posture, PSL source, and an engine token because those can change derived host results;
- display/rendering changes should not require repeating syntax parsing or PSL lookup when their semantic inputs are unchanged.

The protocol must require a cost tier and cache contract for every optional annotation: execution trigger, deduplication unit, worst-case dependency calls, cache layer, complete key, dependency/version token, failure caching, invalidation, and cold/warm equivalence. Laziness may change cost and availability of optional facts; it must not change parse validity or normalized identity.

### S7-F9 — HIGH: external oracles are named but their authority boundaries are not mandatory

The repository's oracle evidence is intentionally plural:

- WPT/Ada are WHATWG URL-parser/serialization evidence;
- RFC grammar and project posture own RFC-syntax expectations;
- punycoder owns IDNA and selected UTS-46/DNS-shape behavior;
- pslr plus a selected list owns PSL knowledge;
- libcurl behavior is implementation characterization, not the WHATWG oracle.

Ada's optional `verify_dns_length` mode is especially important: it rejects ten overlength/empty-label fixtures that the WHATWG URL standard accepts. The Ada DNS-length vector generator explicitly treats standard acceptance and optional DNS-length probe results as two different expected columns. The main external vector set also normalizes Ada serialization into rurl's presentation contract, so raw textual disagreement is not automatically parse disagreement.

The protocol's “full-string conformance fixtures” requirement is not enough. Every host fixture must declare oracle kind, version, configuration, semantic scope, and any presentation normalization. A diagnostic mode or library default cannot be promoted to URL-standard validity merely because the same dependency can compute both.

### S7-F10 — MEDIUM: public identity fields and compatibility projections need ownership

The protocol requires source spelling and normalized identity, but the current fields named `domain_ascii` and `domain_unicode` are PSL-derived registered domains, not complete-host identities. The `host` column is rendering-selected and can be case- or encoding-adjusted. The transient input host is not part of the public table. Public absent and present-empty hosts are often both `NA`.

The final v3 spec therefore needs explicit owner decisions for the full-host source token, normalized ASCII/Unicode identity, standard serialization, display host, and PSL registered-domain values. Existing names may be retained for compatibility, but the protocol must not assume that any current `domain*` field already fulfills the full-host identity requirement.

## Required protocol additions

The following text is suitable for insertion after §6.7, with the cache paragraph added to §6.8 and the fixture paragraph added to §7.

### Paste-ready clause: mandatory host semantic matrix

> **Host semantic matrix.** The reconstructed specification MUST contain a normative matrix crossing URL standard, scheme/parser class, authority state, `host_kind`, and standard-specific `host_form`. It MUST include every reachable WHATWG domain, opaque, IPv4, IPv6, empty, and absent state and every reachable RFC reg-name, IPv4, IPv6, IPvFuture, empty, and absent state. Each row MUST define the source host token, syntax parser, URL-validity outcome, normalized comparison identity, standard serialization, display spelling, public projection, diagnostics, and eligibility for IDNA, DNS-policy/fact, PSL, and resolution annotations. It MUST mark impossible or recovery-only combinations explicitly. Host absence and present-empty host MUST remain semantically distinguishable even if a compatibility projection renders both as missing.

### Paste-ready clause: reserved terminology

> **Host terminology.** The terms `whatwg-domain-host`, `whatwg-opaque-host`, `rfc-reg-name`, `ip-literal`, `ipvfuture`, `registered-domain`/`registrable-domain`, `public-suffix`, `dns-owner-name-policy`, and `resolution-result` name different semantic layers and MUST NOT be used interchangeably. In particular, a WHATWG domain host is a syntactic host form and does not imply a known PSL suffix; a registered-domain value is a PSL annotation and is not the complete host; a reg-name is not automatically an IDNA or DNS-valid name. Any retained public `domain`, `tld`, or `host_type = "domain"` field MUST be labeled as a compatibility projection with its exact dependency and missing-value semantics.

### Paste-ready clause: annotation result and validity invariants

> **Typed optional annotations.** Every IDNA presentation, DNS-policy/fact, PSL, or live-resolution operation MUST return an explicit state drawn from at least `not-requested`, `not-applicable`, `known`, `unknown`, `invalid-input`, and `dependency-error`, plus a value/fact payload and provenance when applicable. A scalar missing value MUST NOT be the only representation of these states. URL parse validity and URL-standard host validity MUST be decided before optional annotations and MUST be invariant under annotation selection, cache warmth, PSL source/list version, dependency availability, and resolver outcome. Historical PSL or policy warnings, if retained, MUST be exposed as a separately named compatibility/policy result rather than the core parse verdict.

### Paste-ready clause: IDNA

> **IDNA operations.** The specification MUST distinguish standard-owned WHATWG domain-to-ASCII processing, normalized comparison identity, reversible A-label/U-label conversion, and best-effort display decoding. For each operation it MUST pin the algorithm/library/profile/version, behavior-affecting options, eligible host forms, transformation order, input/output spelling, and typed failure behavior. It MUST define alternate full-stop mapping, percent-decoding order, ignored and compatibility mappings, join controls, forbidden code points, hyphen checks, STD3, and DNS-length checks. DNS-length and STD3 policy failures MUST NOT be treated as WHATWG URL parse failures unless the URL standard itself requires that result. RFC reg-names and WHATWG opaque hosts MUST NOT acquire IDNA semantics implicitly.

### Paste-ready clause: PSL, DNS, and resolution

> **PSL and DNS boundaries.** A PSL result MUST identify its selected section, list/engine identity or fingerprint, version when available, input/output spelling, and outcome state. `all`, `icann`, and `private` are distinct policies. A registered-domain result means registrable under that selected knowledge source; it does not establish registration, ownership, existence, safety, or resolution. DNS shape facts and named product policies MUST define their A-label-octet basis, root-dot and empty-label handling, length limits, underscore/LDH rules, and relation to URL validity. Default parsing, identity construction, IDNA, DNS-policy checks, and PSL lookup MUST perform no network resolution. Any live resolution feature MUST be a separately authorized, explicitly non-deterministic annotation with timeout, error, and provenance fields and MUST NOT affect syntax validity or identity.

### Paste-ready clause: numeric/IP semantics

> **IP and numeric hosts.** The specification MUST separately define source classification, URL-standard acceptance, normalized identity, standard serialization, display, and source diagnostics for canonical IPv4, WHATWG shortened/integer/hexadecimal/octal IPv4 forms, invalid ends-in-number inputs, IPv6 including embedded dotted IPv4, and RFC IPvFuture. It MUST state the WHATWG/RFC differences explicitly. IP and IPvFuture forms MUST be ineligible for IDNA, DNS-label-shape, and PSL annotations; their typed annotation state is `not-applicable`, not `unknown`.

### Paste-ready clause for §6.8: annotation cost and cache contract

> **Annotation execution contract.** For every optional host annotation, the specification MUST declare its trigger/default, eligibility gate, execution tier, deduplication unit, worst-case dependency calls, cache layer, complete cache key, dependency/list/version token, error-caching rule, invalidation rule, and cold/warm semantic equivalence. Process-local engine handles MUST NOT be treated as portable values. A change in selected URL standard, parser posture, IDNA profile/options, PSL section/list/engine identity, or policy version MUST not reuse an incompatible cached result. Laziness may change cost and the presence of a requested fact, but MUST NOT change parse validity, normalized identity, or standard serialization.

### Paste-ready clause for §7: oracle-labeled host fixtures

> **Host fixture evidence.** Every host fixture MUST label the authoritative layer (`WHATWG/WPT`, RFC grammar/project posture, IDNA/punycoder, PSL/pslr plus list identity, DNS-policy characterization, or implementation-only characterization), the oracle/dependency version and configuration, and any presentation normalization. Standard URL acceptance, optional DNS-length/STD3 checks, PSL knowledge, and display comparisons MUST occupy separate expected fields. A dependency's optional validation mode MUST NOT be used as the URL-standard oracle.

## Minimum cross-product and fixture matrix

The reconstructed specification should require these rows rather than merely asking for representative examples.

### State and eligibility rows

| Standard/parser class | Required host states/forms | Assertions required |
|---|---|---|
| WHATWG special | absent where allowed, empty where allowed, domain, IPv4, IPv6 | parser choice, validity, identity, serialization, IDNA eligibility, PSL/DNS eligibility |
| WHATWG non-special | absent, present-empty, opaque | no implicit IDNA/PSL/DNS; opaque forbidden-set and percent-encoding behavior; case/source behavior |
| RFC authority | absent, empty, reg-name, IPv4, IPv6, IPvFuture | grammar acceptance, source retention, no implicit IDNA, annotation eligibility by explicit DNS-host scheme policy |
| `file:`/hostless carve-outs | absent, empty, localhost, opaque/no-authority states | no accidental PSL/IDNA work; exact public projection |

### IDNA and spelling rows

- Unicode U-label and equivalent `xn--` A-label, asserting source, normalized identity, standard serialization, display, and equality.
- U+3002, U+FF0E, and U+FF61 separator inputs in WHATWG authority scope, plus the same text in RFC and opaque/path scope.
- UTS-46 ignored-character and compatibility-mapping cases, plus a join-control failure.
- A label that decodes successfully, a syntactically ACE-looking label that does not decode, and a mixed-label host.
- Hyphen, STD3-only, forbidden-domain-code-point, empty-label, 63/64-octet A-label, 253/254-octet name, root-dot, and Unicode-to-overlength-A-label cases.
- Every row must state which result is URL validity, which is IDNA conversion, which is DNS/policy fact, and whether a lenient display fallback occurred.

### PSL rows

- Ordinary ICANN suffix, multi-label suffix, wildcard, exception, IDN suffix, private suffix, unknown suffix, suffix-only input, single label, trailing root dot, IP, opaque host, empty host, and absent host.
- Each registrable case under `all`, `icann`, and `private`, including a private-suffix case that changes result.
- Equivalent Unicode and A-label inputs with ASCII and Unicode outputs.
- Explicit list/engine A and B (or pinned fingerprints) to prove cache separation and provenance.
- For every row: annotation state, suffix, registered domain, source/list identity, and proof that core parse validity is unchanged when the lookup is skipped or returns unknown.

### DNS-policy and resolution rows

- `_dmarc.example`, underscore within a label, plus and other STD3-only failures, leading/trailing hyphens, third/fourth-position hyphens, empty internal label, root dot, label/name boundary lengths, and Unicode whose A-label crosses a length boundary.
- Expected results separately for URL acceptance, DNS owner-name policy, LDH/web policy, registrability, and selected punycoder facts.
- A no-network assertion for all parse/policy/PSL cases. If resolution is in v3, separate success, NXDOMAIN/no-data, timeout, transient failure, and unavailable/not-requested rows; none may alter identity or parse validity.

### IP rows

- Canonical dotted IPv4; decimal integer; hex; octal; shortened IPv4; empty hex part; out-of-range; and other ends-in-number failures.
- Canonical and non-canonical IPv6, embedded dotted IPv4, bracket errors, and RFC IPvFuture.
- Every row under both WHATWG and RFC posture, asserting source form, host form/type, normalized identity, standard serialization, display, source diagnostics, and `not-applicable` for IDNA/PSL/DNS-label annotations.

### Cache and laziness rows

- The same URL with annotations not requested, then requested cold, then requested warm.
- The same URL under different URL standards, IDNA options/profile versions, PSL sections, PSL engine/list tokens, output spellings, and policy versions.
- Dependency failure followed by recovery, with an explicit failure-cache/invalidation expectation.
- Equivalent repeated hosts across multiple URLs to verify documented deduplication without erasing per-input source spelling.
- Parse validity, normalized identity, and standard serialization must be identical in every row where only annotation selection or cache warmth differs.

## Owner decisions and unresolved contradictions

These decisions cannot safely be inferred from the current protocol:

1. **Core status ownership.** Should v3 make `parse_status` syntax/standard-only and move `warning-no-tld`, `warning-invalid-tld`, and `warning-public-suffix` into a policy/compatibility result? **Recommendation: yes.** Otherwise optional or changing PSL knowledge controls the apparent parse verdict.
2. **Public type vocabulary.** Should syntactic `host_form`, IP family, and PSL registrability become separate public axes? **Recommendation: yes.** Retain current `host_type` only as a documented compatibility projection if required.
3. **Full-host identities.** Which fields expose source host, normalized ASCII host, normalized Unicode/display host, and standard serialization? Current `domain_ascii`/`domain_unicode` cannot serve this role because they contain only the PSL registered domain. **Recommendation: add explicit host identity fields or typed accessors.**
4. **IDNA pinning.** Is the v3 canonical operation WHATWG domain-to-ASCII via the current punycoder profile, and how is version drift surfaced? **Recommendation: pin the operation/options and record dependency/profile provenance; keep lenient display decoding separate.**
5. **PSL reproducibility.** Is the default list allowed to float per installed `pslr`, or must v3 pin/fingerprint it? **Recommendation: at minimum expose and cache-key a stable list/engine fingerprint; pin it for conformance fixtures.**
6. **Optional result API.** Are IDNA presentation, DNS facts/policies, PSL, and any resolution returned in the main table or companion results? **Recommendation: typed companion annotations by default, with explicit eager selection if a table projection is needed.**
7. **Resolution scope.** Is network DNS resolution in v3? **Recommendation: out of scope for core parsing.** If later added, require explicit authorization and a separate non-deterministic annotation contract.
8. **RFC reg-name annotation eligibility.** Which schemes declare an RFC reg-name to be a DNS-host candidate? ADR-0012 says this must be explicit, but no v3 policy is chosen. **Recommendation: scheme/parser posture owns an eligibility flag; reg-name syntax alone is insufficient.**
9. **Compatibility missingness.** May absent host, present-empty host, not-applicable annotation, unknown suffix, invalid annotation input, and dependency failure all still render as `NA` in legacy columns? **Recommendation: only in a labeled compatibility projection; the semantic result must preserve distinct states.**
10. **Display fallback.** When an A-label/U-label presentation conversion fails, should display retain the source label, return missing, or error? Current helpers retain source labels leniently. **Recommendation: preserve this only as an explicit display policy and expose the conversion state.**

## Files and evidence checked

### Core implementation and public surface

- `R/domain.R`: IDNA rendering helpers, strict/lenient fallbacks, PSL wrappers, section/output policy, and isolated punycoder host probes.
- `R/host-policy.R`: parser-independent `url`, `dns`, `web`, `registrable`, and `seo` policies and reasons.
- `R/parse-state.R`: host/authority presence, WHATWG/RFC host forms, generic host parsers, and Stage-B eligibility.
- `R/parse-phases.R`: alternate separator mapping, WHATWG numeric IPv4, standard host models, IPv6 serialization, IDNA application, PSL derivation, output assembly, and PSL-influenced parse status.
- `R/parse.R`: profiles, engine validation, cache keys, Stage A/B routing, host transforms, and domain/TLD projections.
- `R/diagnostics.R`: public host type, source-shaped IP diagnostics, selected UTS-46/DNS facts, and companion-only behavior.
- `R/accessors.R`: registered-domain, host, suffix, host-type, and diagnostics public contracts.
- `R/status-constants.R`, `R/profiles.R`, and `R/utils.R`: historical statuses, named postures, supported/special schemes, forbidden sets, charset shim, public fields, and stage fields.
- `NAMESPACE` and relevant generated manuals, including `safe_parse_url`, `get_parse_status`, `get_scheme_class`, `get_mailto_recipients`, `is_valid_host`, and host accessors.

### Accepted designs and PRDs

- ADR-0001: pslr ownership and fixed wrapper policy.
- ADR-0002: reversible rendering helpers versus canonical host normalization.
- ADR-0004: standard-specific numeric host model and forbidden host boundaries.
- ADR-0006: companion-only metadata/diagnostics and opt-in cost.
- ADR-0007: coherent standard selector, Stage-A/cache effects, and unchanged default.
- ADR-0009: Ada-confirmed curl/WHATWG host-character gap and shim scope.
- ADR-0012: distinct WHATWG/RFC models, host state/form vocabulary, operation eligibility matrix, acceptance-versus-validity rule, and cache fields.
- `design/prd/host-validation-policy.md`: syntax/policy/registrability/resolution separation and named policies.
- `design/prd/url-standard-selector.md` and `design/prd/url-standard-selector-v2.md`: numeric hosts, oracle policy, punycoder ownership, DNS/UTS-46 facts, costs, and library boundaries.

### Tests, fixtures, and external evidence

- Domain identity, host parser, PSL parity, host policy, forbidden-host, UTS-46 separator, DNS/UTS-46 characterization, standard host, conformance, and diagnostics tests.
- `tests/testthat/fixtures/url-standard-conformance.csv` and `tests/testthat/fixtures/external-url-vectors.csv`.
- `_scratch/build-ada-vectors.R`, `_scratch/build-ada-dnslength-vectors.R`, `_scratch/conformance-gap-report.md`, WPT findings/triage, disagreement findings, divergence ledger, and oracle audit summary/escalations.

## Non-blocking editorial improvements

- Require the final v3 document to include one compact glossary table before any host chapter; the current protocol introduces several ambiguous words before insisting on terminology review.
- Replace generic “normalization” in host-related checklist items with the exact operation: parser normalization, comparison identity, standard serialization, A-label/U-label conversion, or display rendering.
- Require every missing-value example to name why the value is missing. This catches absent, empty, not-applicable, unknown, invalid, skipped, and dependency-error collapses early.
- Add “knowledge-source drift” beside parser/dependency drift in the external evidence and cache sections; PSL changes are semantic even when code and URL bytes do not change.
- State that a fixture can have more than one correct oracle column. The Ada DNS-length evidence is a useful template: WHATWG acceptance and optional DNS-shape validation are separately true.

## Final assessment

The protocol has enough source coverage to discover the host subsystem but not enough normative structure to reconstruct it without semantic loss. S7 is ready to pass once the protocol requires: (1) a complete host state/eligibility matrix, (2) non-overloaded terminology, (3) typed optional-annotation outcomes independent of parse validity, (4) pinned IDNA and PSL contracts with provenance, (5) explicit DNS-policy/resolution and IP boundaries, and (6) oracle-labeled, cache-sensitive cross-product fixtures. Until then, a nominally compliant v3 spec could preserve the current conflations of syntax, registrability, and display while claiming all listed topics were covered.
