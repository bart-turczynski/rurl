# rurl 3.0 Specification Reconstruction Protocol

Status: proposed review protocol

Purpose: define how the rurl 3.0 architecture specification will be reconstructed, reviewed, and graduated without relying on conversational memory. This document is a process specification, not the rurl 3.0 product or API specification.

Intended reviewer: an agent or person with no prior conversation context. The reviewer should assess whether this process is sufficient to recover the relevant decisions, expose contradictions, and produce a traceable, testable specification.

## 1. Problem statement

The prospective rurl 3.0 specification draws on several partially overlapping sources:

- current source code and tests;
- accepted ADRs and PRDs;
- Fiberplane epics, issues, and comments;
- historical worklogs, research notes, and `_scratch` drafts;
- explicit product-owner decisions made during design discussions;
- current CRAN behavior, some of which may intentionally be broken in 3.0.

The design discussion has also passed through context compaction. A detail that existed only in discarded conversational context cannot be recovered reliably from model memory. The process must therefore reconstruct decisions from durable evidence, distinguish current behavior from desired behavior, and surface uncertainty rather than silently filling gaps.

No normative rurl 3.0 requirement may be based solely on unverified recollection.

## 2. Desired outcome

Produce an authoritative, detail-rich rurl 3.0 architecture specification that:

1. preserves all confirmed product requirements and deliberately supersedes obsolete ones;
2. separates current implementation facts, historical intent, proposals, and accepted v3 decisions;
3. defines testable contracts for parsing, validation, repair, mutation, serialization, display, cleaning, comparison, and joining;
4. covers web URLs, scheme-relative references, relevant non-web schemes, email-related extraction and diagnostics, IDNA/Unicode, PSL semantics, and component-level transformations;
5. establishes the contract for a curl-free implementation;
6. describes migration from the v2 API and identifies compatibility promises intentionally revoked by the major release;
7. can be translated into Fiberplane implementation work only after its architectural decisions are accepted.

## 3. Source authority and interpretation

Use the following precedence when sources conflict:

1. The product owner's latest explicitly confirmed decision.
2. Accepted ADRs and current normative project documentation.
3. Tests and source code as evidence of shipped behavior, not automatic evidence of desired v3 behavior.
4. Fiberplane issues and issue comments.
5. Historical PRDs, worklogs, analysis documents, and `_scratch` notes.
6. Recalled conversational context without a surviving durable source.

Additional rules:

- Newer owner decisions may intentionally supersede accepted documentation, but the supersession must be recorded explicitly.
- Code describes what the package does now. A major-version specification may revoke it.
- A passing test may be a compatibility pin, a characterization test, or a conformance assertion. Determine which before treating it as normative.
- `_scratch` documents are research inputs unless their decisions are reaffirmed.
- Absence of evidence is not evidence that a requirement was rejected.
- Conflicts must be presented for resolution; they may not be reconciled silently in prose.

## 4. Existing evidence collection

Three independent, read-only investigations have been performed:

1. Core-code audit: public parse shape, parser stages, browser fixups, standard routing, internal state, serializers, curl coupling, caching, vectorization, and compatibility pins.
2. Semantic-code audit: scheme behavior, component accessors, host kinds, IDNA/PSL, IPv4/IPv6, diagnostics, email and `mailto:`, validation policies, cost tiers, and round-trip behavior.
3. Documentation and issue audit: 123 Markdown-family files and the 22 issues under `RURL-gxqdmpcp`, including accepted decisions, historical proposals, known conflicts, and unfinished work.

These reports are discovery aids, not authority. The primary agent must verify every load-bearing finding against the cited source before placing it in the decision ledger or product specification. Disagreement between reports must trigger direct inspection.

## 5. Durable working artifacts

Before drafting product-specification prose, create and maintain the following artifacts.

### 5.1 Compact run state

Maintain a compact orchestration state under the repository's ignored scratch area. It should contain only:

- current phase and frontier;
- completed evidence units;
- unresolved blockers and design forks;
- approved decisions not yet transferred to the durable ledger;
- paths to the current ledger, draft, and review reports.

It must be sufficient to resume after context compaction without rehydrating conversation transcripts.

### 5.2 Decision ledger

Record one row per architectural topic with these fields:

- stable decision identifier;
- topic;
- current implementation behavior;
- existing documented decision;
- latest owner direction;
- proposed v3 contract;
- status: `DECIDED`, `PROPOSED`, `OPEN`, `SUPERSEDED`, or `CONFLICT`;
- evidence locations;
- implementation and migration consequences.

The ledger is the source for architectural review. Polished PRD prose must not be the first durable record of a decision.

### 5.3 Contradiction register

Record disagreements among code, tests, ADRs, PRDs, issues, notes, and owner decisions. Each entry must state:

- the competing claims;
- their evidence;
- whether they describe current behavior, historical intent, or desired v3 behavior;
- the owner decision required, if any;
- the eventual resolution and superseded source.

### 5.4 Traceability map

Every normative product-specification requirement must map to one or more of:

- an accepted owner decision;
- an ADR or accepted PRD;
- a deliberate v2 compatibility decision;
- a conformance source or fixture;
- an explicit new v3 proposal accepted during review.

## 6. Decision reconstruction scope

The ledger must cover at least the following areas.

### 6.1 Object and state model

- Original input preservation.
- Parsed structural state.
- Absent versus present-empty query, fragment, userinfo, authority, and related delimiters.
- Source spelling versus normalized component identity.
- Host kinds, path forms, authority forms, opaque paths, and scheme-specific state.
- Credentials and reassemblable userinfo.
- Scalar, vector, missing, invalid, and zero-length behavior.

### 6.2 Standards, validation, and repair

- The exact point at which WHATWG versus RFC interpretation is selected.
- Separate WHATWG and RFC parsing and serialization contracts.
- Strict, non-repairing validation as the ordinary compliance path.
- `browser` as an explicit, orthogonal pre-parse repair posture rather than a standard.
- Whether a separately named RFC repair operation exists and, if so, its bounded contract.
- The meaning of validity when optional IDNA, PSL, DNS-shape, or other annotations were skipped.
- Full-string conformance fixtures and limits on conformance claims.

### 6.3 Serialization, formatting, and display

- Lossless or faithful serialization guarantees by standard.
- Fragment, credentials, and empty-delimiter preservation in standards serialization.
- Unicode-readable presentation through `format_url()`.
- ASCII/Punycode and percent-encoded standards serialization.
- Safe display decoding across host, path, query, and fragment.
- Preservation of percent-encoded structural delimiters such as `%2F`, `%3F`, `%23`, `%26`, and `%3D` when decoding them would alter or obscure structure.
- Handling of invalid UTF-8, controls, bidi controls, and misleading invisible characters.
- A prohibition on using display output as the serialization or equality contract.

### 6.4 Cleaning and mutation

- `get_clean_url()` as the easy, high-volume, SEO-oriented convenience operation with reasonable defaults.
- Cleaning as an intentional product goal, not an accidental parser artifact.
- Its permitted lossy transformations and standards-valid output contract.
- Preservation of existing useful cleaning features, including scheme choice, `www`/subdomain behavior, domain/suffix transformations, path and parameter handling, and redirect-mapping workflows.
- Retirement of misleading legacy vocabulary such as `keep` where it means preserve-if-present but add-if-absent.
- `mutate_url()` as the component-level transformation API.
- Symmetric, predictable operations for scheme, authority, hostname, subdomains, registrable domain, suffix, port, path, query parameters, fragment, and credentials where applicable.
- Transactional validation of the resulting URL under an explicitly selected output standard.
- Clear treatment of adding, setting/replacing, removing, and preserving components.

### 6.5 Comparison keys and joins

- `get_url_key()` as a non-URL comparison projection, independent of display and cleaning.
- Explicit equivalence policies for scheme, default port, host spelling, path, query, fragment, and other components.
- Literal scheme policies such as `exact`, `http-https`, and `http-https-missing`.
- Default-port equivalence for HTTP 80 and HTTPS 443 without rejecting valid non-default ports.
- Missing-scheme behavior without guessing an effective port.
- Reuse with ordinary data-frame joins.
- A dplyr-shaped URL join family covering inner, left, right, full, semi, and anti joins.
- Duplicate/relationship handling, invalid-key handling, unmatched rows, key visibility, and suffix behavior.
- Reassessment of `canonical_join()` and its current dependence on `clean_url` as an implicit equality policy.

### 6.6 Schemes and extractable annotations

- HTTP, HTTPS, FTP, FTPS, file, WS, and WSS behavior.
- Scheme-relative network-path references such as `//example.com/path`.
- `mailto:`, email local parts and domains, multiple recipients, grammar-labelled diagnostics, SMTP wire facts, and the prohibition on claiming deliverability.
- `tel:`, `data:`, arbitrary opaque schemes, arbitrary hierarchical schemes, and unknown schemes.
- Which schemes use generic component extraction and which receive dedicated companion helpers.
- Separation of parser acceptance from scheme-protocol diagnostics.

### 6.7 Host, domain, and public-suffix semantics

- Hostname, subdomain labels, registrable domain, public suffix, and complete hostname terminology.
- ICANN versus private PSL sections and unknown suffixes.
- PSL registrability versus syntax validity, DNS validity, and resolution.
- Unicode U-label and ASCII A-label identity.
- WHATWG domains, opaque hosts, RFC reg-names, IPv4, IPv6, IPvFuture, empty hosts, and absent hosts.
- Eligibility boundaries for IDNA, DNS-shape checks, and PSL annotations.

### 6.8 Performance and migration

- Massive CSV/vector workflows as a primary use case.
- Lazy or explicit annotations and their cost contracts.
- Cache semantics and deterministic behavior.
- The complete curl dependency surface, including parsing and escape/unescape behavior.
- The curl-free 3.0 acceptance gate.
- Compatibility contracts intentionally revoked in 3.0.
- Legacy API deprecation in 3.0 and planned removal in 3.1 where appropriate.
- CRAN submission only after the coherent curl-free 3.0 slice is complete and verified.

## 7. Example-driven specification method

For each representative input, the product specification should show the outcome at every relevant layer:

| Layer | Required question |
|---|---|
| Parse record | What structure and source distinctions were observed? |
| Validation | Is the input compliant with the selected standard, and what is wrong if not? |
| Repair | What explicit optional intervention was performed? |
| Mutation | What components did the user deliberately change? |
| Serialization | What standards-conforming string represents the object? |
| Formatting | What readable Unicode representation should a person see? |
| Cleaning | What opinionated SEO-oriented result was requested? |
| Keying | Which distinctions were deliberately ignored for equality or joining? |

The fixture matrix must include, at minimum:

- `HTTP://user:pass@Ex.com:80/a/../b?#`;
- `example.com/path`;
- `//example.com/path`;
- Unicode and `xn--` hostnames;
- valid Unicode encoded in paths, queries, and fragments;
- encoded structural delimiters;
- IPv4, IPv6, and unusual WHATWG numeric hosts;
- explicit default and non-default ports;
- malformed hosts and authorities;
- URLs with absent and present-empty components;
- `mailto:` with multiple valid and invalid recipients;
- `file:`, `ws:`, `wss:`, FTP-family, and opaque URLs;
- high-volume vector input containing successes, warnings, failures, missing values, and duplicates.

Examples are conformance probes, not decorative documentation. If two layers return the same string for different reasons, their contracts must still be described separately.

## 8. Product-specification structure

Preserve `_scratch/prd-url-object-grammar-DRAFT.md` as historical input. Do not overwrite it during reconstruction.

Write a successor v3 draft with this structure:

1. Goals and non-goals.
2. Terminology and invariants.
3. Object and state model.
4. Parsing, standard selection, and optional repair.
5. Validation and diagnostics.
6. Mutation.
7. Standards serialization.
8. Unicode-readable formatting.
9. SEO cleaning.
10. Comparison keys and URL joins.
11. Scheme-specific behavior and companion helpers.
12. PSL, IDNA, email, and optional annotations.
13. Errors, vectorization, caching, and performance.
14. Public API proposal.
15. Migration from v2.
16. Conformance and verification requirements.
17. Explicit unresolved decisions.
18. Traceability and superseded documents.

Normative requirements should have stable identifiers that can be referenced by tests, ADRs, and Fiberplane issues.

## 9. Review cadence

Do not ask the owner to approve the entire document at once. Review the reconstructed decision ledger in coherent batches:

1. Object model, standards, validation, and repair.
2. Mutation, cleaning, serialization, formatting, and keys.
3. Joins and high-volume SEO workflows.
4. Schemes, email, host kinds, IDNA, and PSL.
5. Errors, performance, curl removal, conformance, and migration.

Only confirmed batches should become normative product-specification language. Unresolved decisions must remain visibly open.

## 10. Independent red-team review

After the first complete product draft, provide a fresh reviewer only with the draft and a bounded review request. Do not provide the design conversation or conclusions from earlier reviewers.

The reviewer should identify:

- contradictory contracts;
- undefined or overloaded vocabulary;
- display accidentally serving as serialization or identity;
- cleaning accidentally serving as implicit repair;
- standards validation weakened by browser-like behavior;
- scheme-specific rules incorrectly generalized to all URLs;
- transformations capable of generating invalid output;
- absent versus present-empty state lost before serialization;
- missing scalar, vector, invalid-row, duplicate, or error behavior;
- compatibility promises inconsistent with a breaking 3.0;
- requirements without a feasible verification method;
- implied claims of universal conformance unsupported by the fixture corpus.

Expected reviewer output:

1. Findings ordered by severity.
2. Exact section references.
3. A minimal example demonstrating each material problem.
4. A list of questions that require owner judgment.
5. A separate list of non-blocking editorial improvements.

The primary agent must verify the reviewer's factual claims before changing the draft.

## 11. Graduation and implementation planning

After owner approval:

1. Graduate the accepted product specification to `design/prd/`.
2. Record irreversible architectural decisions in focused ADRs where appropriate.
3. Keep `RURL-gxqdmpcp` as the umbrella Fiberplane epic rather than creating a competing epic.
4. Reconcile its existing issues with the accepted requirements.
5. Create implementation issues only after the relevant normative contract is stable.
6. Define dependencies and verification gates for the curl-free parser, object model, APIs, migration, and conformance suite.
7. Submit rurl 3.0 to CRAN only after the coherent curl-free release slice passes its full verification gate.

## 12. Acceptance criteria for this reconstruction protocol

This process is acceptable only if an independent reviewer can answer yes to all of the following:

- Does it avoid relying on compacted conversational memory?
- Does it provide an explicit authority order for conflicting evidence?
- Does it distinguish shipped behavior from desired v3 behavior?
- Does it preserve owner decisions without allowing them to supersede documents invisibly?
- Does it force contradictions and missing decisions into visible registers?
- Does it cover the full semantic surface already implicated by the repository and epic?
- Does it define a review sequence that is manageable for the owner?
- Does it make every normative requirement traceable and testable?
- Does it preserve historical drafts rather than rewriting the evidence base?
- Does it delay implementation decomposition until the architecture is accepted?

If any answer is no, the reviewer should recommend a concrete correction to this protocol before product-specification drafting begins.
