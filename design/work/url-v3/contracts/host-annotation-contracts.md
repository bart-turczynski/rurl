# Host / annotation contracts (§6 artifact 10, excl. cache)

<!-- Contract artifact (§6 artifact 10, host/annotation slice; the cache slice is
     G3.9). RCON-08 closure. RCON-08's owner tier is P4 and the dedicated P4 HOST
     RECORD was never written: P4.1 §D-E states "This record does not decide
     RCON-08" and defers the deep host vocabulary / IDNA / PSL / DNS / IP contracts
     to a separate P4 host record. Therefore this artifact PROJECTS only the
     host-facing BOUNDARY facts that accepted records + ADRs already fix — P4.1
     (scheme-forced host posture + the RCON-08 boundary), P1.2 (host_kind
     independence), P3.2 (root-dot DISTINCT for normalized-domain identity), P2.3
     (the S7-F3 typed annotation states + L1/L2 invariance, owned by G3.6), and the
     accepted ADRs 0001/0002/0004/0006/0009/0012 + the host-validation-policy PRD —
     plus frozen evidence S7. Every DEEP host-contract cell the unmade P4 host
     record owns is flagged OPEN with impact + destination, never invented. This is
     the SINGLE WRITER of the host/annotation matrices EXCEPT: the annotation
     cost/cache contract DELEGATES to G3.9 (semantic-cache); the verdict-layer
     annotation-state SEMANTICS are G3.6's (this artifact applies them to host
     forms); the comparison key is G3.K's; scheme admission/eligibility is G3.5's.
     Canonical FIELD NAMES come from artifact 3. Format follows the
     G3.3/G3.5/G3.6/G3.7/G3.8/G3.K precedent. Envelope stays lifecycle_state
     PROPOSED until the cp-snapshot-3 seal; validator coverage + manifest
     present-flip ride that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-host-annotation |
| name | host-annotation-contracts |
| artifact_number | 10 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/host-annotation-contracts.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the host/annotation matrices (host presence + form, scheme-forced posture, transform eligibility, IDNA operation distinctness, PSL delegation + identity spellings, host annotation-state application, DNS-policy/no-network boundary, IP/numeric-host model, full-host-identity clarification, external-oracle labeling) EXCEPT the annotation cost/cache contract (G3.9) and the verdict-layer annotation-state semantics (G3.6) |
| lifecycle_state | PROPOSED |
| dependencies | P4.1 (bound decision — scheme-forced host posture + RCON-08 boundary); P1.2 (bound decision — host_kind independence); P3.2 (bound decision — root-dot DISTINCT); P2.3 (bound decision — S7-F3 annotation states + invariance, semantics owned by G3.6); S7 (bound evidence); contract-canonical-state (field vocabulary only, not projected); ADR 0001/0002/0004/0006/0009/0012 + host-validation-policy PRD (accepted authority); G3.9 (cache — delegated); reconciliation §6 artifact 10, §4 RCON-08, §7 G3 |
| bound_decision | P4.1 + P1.2 + P3.2 + P2.3 (host-facing boundaries only; RCON-08's deep host record is UNMADE) |
| bound_evidence | S7 |
| closes_finding | RCON-08 (boundary + explicit open cells; the deep host record remains an owner decision) |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: host presence/form, scheme-forced posture, transform eligibility, IDNA operation distinctness, PSL delegation/identity, host annotation-state application, DNS-policy/no-network boundary, IP/numeric-host model, full-host-identity clarification, and external-oracle labeling each carry a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line impact and owner-decision destination; the cache/cost cell delegates to G3.9; cross-artifact field names agree with artifact 3; validate-records.R (host-annotation section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (P4.1, P1.2, P3.2, P2.3, S7), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | host-annotation validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative v3 contract for the host and its optional annotations — the
host-facing half of §6 artifact 10 (the cache half is G3.9). It fixes what
accepted authority already settles: host presence and syntactic form, the
scheme-forced host posture, Stage-B host-transform eligibility, the distinctness
of the IDNA/rendering operations, PSL delegation and identity spellings, the
application of the typed annotation states to host forms, the no-network DNS-policy
boundary, the WHATWG/RFC numeric-host model, the full-host-identity clarification,
and external-oracle labeling.

**RCON-08's deep host record is unmade.** Its owner tier is P4 and P4.1 §D-E
explicitly declines to decide it, deferring the deep host vocabulary / IDNA
profile / PSL reproducibility / DNS-policy / IP-identity / full-host-identity
contracts to a separate **P4 host record** that does not yet exist. S7 lists ten
such owner decisions "that cannot safely be inferred." This artifact therefore
**projects the boundaries and flags every deep-contract cell OPEN** with impact +
destination; it invents no host vocabulary, IDNA pinning, PSL fingerprint, DNS
policy, or IP-identity row.

This record **projects** the host-facing boundaries of accepted P4.1 + P1.2 +
P3.2 + P2.3 (and accepted ADRs) and S7; it makes no product decision. A `SETTLED`
row cites `P4.1@b017e87`, `P1.2@bb3346e`, `P3.2@bb3346e`, `P2.3@a7e0a59`, or a
named accepted ADR. A cell the unmade P4 host record owns is `OPEN` with its exact
impact and settlement destination.

## Inputs

The exact sources this contract projects, hashed at authoring.
`validate-records.R` recomputes all hashes at the cp-snapshot-3 seal. P4.1, P1.2,
P3.2, and P2.3 are already hash-enforced as ACCEPTED decisions by
`validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P4.1-scheme-specialization-posture.md | 0d1e9a4d941b7398a935cbe66b91f174c7aecef768114964c6b1ee91b51b7cb2 |
| design/work/url-v3/decisions/P1.2-authority-state-vocabulary.md | 1c8f68528f834f96fefb0b2912661ea6ee33328b1d14652be3701810bcfaad2d |
| design/work/url-v3/decisions/P3.2-key-join-closure.md | ac4bde5d136872ef84ed538e5a8cff5296a07b70c1bab5515ce9271d1440eab0 |
| design/work/url-v3/decisions/P2.3-validation-verdicts-migration.md | 1a1d1c4542a50a7c8310efc9055057c17c4ddbdaab2fb9988d08070253fa7a2e |
| design/work/url-v3/evidence/S7-host-idna-psl-ip.md | 8fb62419099dbe25d5c8d70ebae050f41fec67f15b0d7f19714157acd1c55636 |

## Host presence + syntactic-form rows

Host presence is three-valued and independent of the authority delimiter (P1.2);
the syntactic host forms are the artifact-3 classifier enums. Absence and
present-emptiness stay distinguishable even where a compat projection renders both
as `NA` (S7-F1).

| axis | contract | owner_decision_ref | status |
|---|---|---|---|
| host presence | `host_kind ∈ {absent, empty, present}` (artifact-3 spelling of the three-valued `{absent, present-empty, present-nonempty}` model); **independent of `authority_delimiter_present`** — `foo:///bar` and `file:///bar` agree (delimiter present, payload empty), host `absent`/`present-empty`/`present-nonempty` a separate axis | P1.2@bb3346e; artifact 3 | SETTLED |
| WHATWG host form | `whatwg_host_form ∈ {domain, opaque, ipv4, ipv6, empty}` (artifact 3, `R/parse-state.R`); special-scheme hosts use the domain/IPv4/IPv6 parser, non-special use the opaque-host parser | ADR 0012; ADR 0004; artifact 3 | SETTLED |
| RFC host form | `rfc_host_form ∈ {reg-name, ipv4, ipv6, ipvfuture, empty}` (artifact 3); reg-name preserves source spelling, no implicit IDNA; direct non-ASCII reg-name is a documented RFC-syntax posture, not IRI conformance | ADR 0012; ADR 0004; artifact 3 | SETTLED |
| public `host_type` ≠ syntactic form | public `host_type = "domain"` is a **PSL-derived** classification (non-IP host with a registered domain), NOT the syntactic `whatwg_host_form = "domain"`; the two are observably different (single-label special host is a WHATWG domain host with no PSL registered domain) | ADR 0006; S7-F2; artifact 3 | SETTLED (overload flagged; de-overloading OPEN — HOST-O2) |

## Scheme-forced host posture rows (reference G3.5 / P4.1)

The scheme forces a host *posture*; the vocabulary/identity detail is this
artifact's, but the posture itself is P4.1's (surfaced in G3.5).

| scheme family | host posture | owner_decision_ref | status |
|---|---|---|---|
| special (`http`/`https`/`ws`/`wss`/`ftp`/`file` under whatwg) | authority + host mandatory; a missing/empty host on a special non-`file` scheme is a host-missing parse **error** | P4.1@b017e87 (D-A.1); ADR 0012 | SETTLED |
| `file` | localhost/empty-host mapping; drive/path per standard; no accidental PSL/IDNA work | P4.1@b017e87 (D-A, D-E); ADR 0012 D5 | SETTLED |
| `ftp` | DNS/PSL-eligible host | P4.1@b017e87 (D-A, D-B) | SETTLED |
| `mailto` | recipient-domain PSL carve-out — the **one** place a general-routed host reaches the PSL (a recipient RHS is a domain) | P4.1@b017e87 (D-C); ADR 0012 D7 | SETTLED |
| opaque / non-special authority / IP / absent / empty | intentionally **ineligible** for Stage-B host transforms, IDNA, and PSL | P4.1@b017e87 (D-A, D-D); ADR 0012 | SETTLED |

## IDNA / rendering operation-distinctness rows

The three host operations MUST remain distinct (ADR 0002); "IDNA normalization"
is **not one operation** and cannot be one Boolean (S7-F4). The shipped behavior
is SETTLED-as-distinct; the v3 *pinning* of the canonical operation is OPEN.

| operation | contract | owner_decision_ref | status |
|---|---|---|---|
| WHATWG domain-to-ASCII (the parse gate) | `punycoder::host_normalize()` with `check_hyphens = FALSE`, `use_std3 = FALSE`, `verify_dns_length = FALSE`; alternate full-stop mapping in WHATWG authority scope before numeric/IP classification; DNS-length is deliberately **not** URL validity; STD3 failure is **not** a URL parse failure | ADR 0009; ADR 0004; S7 §2 | SETTLED (shipped behavior) |
| reversible A-label/U-label rendering | `.normalize_and_punycode()` — display-oriented, case-preserving, strict-then-lenient; **not** canonical `host_normalize()` semantics (do NOT alter to force-lowercase/reject — critical constraint) | ADR 0002 | SETTLED |
| best-effort display decoding | `.punycode_to_unicode()` — per-label; undecodable labels remain in source form rather than failing the parse | ADR 0002 | SETTLED |
| these three are distinct | RFC reg-names and WHATWG opaque hosts MUST NOT acquire IDNA semantics implicitly; a lenient render is not canonical normalization | ADR 0002; S7-F4 | SETTLED |
| v3 canonical-IDNA pinning | pinning the algorithm/profile/version/options as the v3 canonical operation, plus version-drift surfacing and provenance | — (see Open cells HOST-O3) | OPEN |

## PSL annotation rows (reference ADR 0001 / G3.9)

PSL knowledge is delegated to `pslr` (ADR 0001); this artifact does not
reintroduce a matcher. The identity spellings and section vocabulary are SETTLED;
the v3 knowledge-source reproducibility contract is OPEN.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| delegation | suffix/registered-domain knowledge is `pslr`'s; rurl queries through the `R/domain.R` seam and ships no list | ADR 0001 | SETTLED |
| section vocabulary | `all` / `icann` / `private` are **distinct policies**, not interchangeable data | ADR 0001; S7-F5 | SETTLED |
| identity spellings | four stable encoding-independent keys — `domain_ascii`, `domain_unicode`, `tld_ascii`, `tld_unicode` — plus rendering-selected `domain`/`tld` (a presentation choice); these are **registrability annotations**, never full-host identity, registration, existence, or resolution proof | artifact 3; S7 §3 | SETTLED |
| registrability meaning | a registered-domain result means "registrable according to the selected list + section," never registered/owned/reachable/safe/resolvable | S7-F5 | SETTLED (semantic clarification) |
| PSL reproducibility / provenance | whether the default list floats per installed `pslr` or is pinned/fingerprinted; list/engine identity + version surfaced in results; cache invalidation when a default list changes | — (see Open cells HOST-O4; cache mechanics → G3.9) | OPEN |
| engine identity + cache | the process-local `pslr::psl_engine` pointer and Stage-A cache keying on PSL source + engine token are the **G3.9** cache contract — delegated, not owned here | G3.9 (semantic-cache-contract) | SETTLED (boundary — cache → G3.9) |

## Host annotation-state application rows (semantics owned by G3.6)

The typed annotation states and the L1/L2 invariance are the verdict-layer
semantics **owned by G3.6** (projecting P2.3/P1.1); this artifact applies them to
host forms. Root-dot identity binds P3.2.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| typed states | the host/PSL annotation is a typed result in `{not-requested, not-applicable, known, unknown, invalid-input, dependency-error}` + value/provenance, never a bare `NA` (S7-F3) | P2.3@a7e0a59 (§2) — semantics owned by **G3.6** | SETTLED (boundary — states owned by G3.6) |
| L1/L2 invariance | host annotation MUST NOT alter the syntax (L1) or policy (L2) verdict; they are decided before annotation and invariant under annotation selection, cache warmth, PSL section/list/engine identity, and `pslr` availability | P2.3@a7e0a59 (§2); P1.1 (§2) — owned by **G3.6** | SETTLED (boundary — G3.6) |
| per-host-form state | IP-literal / `file` / opaque / general non-special-authority host → `not-applicable` (has no registrable-domain concept), **not** `unknown`; reg-name-shaped no-match → `unknown`; suffix-only → `known` | P2.3@a7e0a59 (§2); S7-F7 | SETTLED |
| root-dot identity | a trailing root dot is **DISTINCT** in normalized-domain identity (KJ-O2: WHATWG url-equivalence exempts only certificate comparison from the trailing dot); `example.com` ≠ `example.com.` for keys | P3.2@bb3346e (KJ-O2) | SETTLED |
| core-status ownership | the historical PSL warnings (`warning-no-tld`/`-invalid-tld`/`-public-suffix`) are **retained in `parse_status`** as the lossy π compat projection, but as L3 facts they can **never** alter L1/L2 — so optional/changing PSL knowledge cannot change the parse verdict (resolving the S7-F3 hazard at the verdict-layer level, not by removing the warnings) | P2.3@a7e0a59 (§1, §4) — owned by **G3.6** | SETTLED (boundary — G3.6) |

## DNS-policy / no-network rows (reference host-validation-policy PRD)

The host has layered DNS-shaped facts and named policies; default parsing does no
network I/O. The named policies are SETTLED (accepted PRD); the v3 *typed DNS
contract* and RFC-reg-name DNS-host eligibility are OPEN.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| no network I/O | default parsing, identity construction, IDNA, DNS-policy checks, and PSL lookup perform **no** network resolution | host-validation-policy PRD; S7-F6 | SETTLED |
| named policies | `is_valid_host()` composes parser-independent policies (`url` / `dns` / `web` / `registrable` / `seo`); a policy failure is **not** a parse failure (separate layer) | host-validation-policy PRD; ADR 0004 | SETTLED |
| length basis | length facts on A-label octets; a terminal root dot excluded from the 253-octet name limit; `use_std3` a strict superset of the WHATWG forbidden-domain rule | S7 §4; ADR 0009 | SETTLED (shipped facts) |
| typed DNS-policy contract | the normative v3 DNS owner-name-policy contract (root-dot/empty-label handling, 63/253-octet basis, underscore/LDH rules, STD3 non-equivalence) as a typed result | — (see Open cells HOST-O5) | OPEN |
| RFC reg-name DNS-host eligibility | which schemes declare an RFC reg-name a DNS-host candidate (ADR 0012 says this must be explicit; no v3 policy chosen) | — (see Open cells HOST-O5) | OPEN |
| live resolution | if ever added, must be a separately authorized, non-deterministic annotation with timeout/error/provenance that cannot affect syntax validity or identity; **out of scope for core v3** | S7-F6 (owner decision 7) | SETTLED (boundary — out of scope) |

## IP / numeric-host rows

The WHATWG/RFC numeric-host disagreement is intentional (ADR 0004); the model is
SETTLED, the full typed row-set is OPEN.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| WHATWG numeric model | recognizes decimal-integer, hex, octal, and shortened IPv4, serialized as a canonical dotted quad; an invalid host that "ends in a number" is fatal; canonicalizes IPv6 incl. embedded dotted IPv4 | ADR 0004; S7 §5 | SETTLED (model) |
| RFC numeric model | non-canonical numeric text is a reg-name; only canonical dotted-quad IPv4 is an IPv4 form; IPv6 preserves source spelling; IPvFuture is an RFC bracketed form not accepted by the WHATWG IPv6 parser | ADR 0004; S7 §5 | SETTLED (model) |
| IP annotation ineligibility | IP and IPvFuture forms are **not** IDNA, DNS-label-shape, or PSL inputs; their annotation state is `not-applicable`, never `unknown`; source-shaped numeric diagnostics are facts about the input, not domain annotations | ADR 0004; P2.3@a7e0a59 (§2); S7-F7 | SETTLED |
| full typed IP row-set | the complete per-form source/acceptance/identity/serialization/display/diagnostics rows across both postures (canonical/shortened/integer/hex/octal IPv4, IPv6 embedded-v4, IPvFuture, ends-in-number failures) | — (see Open cells HOST-O6) | OPEN |

## Full-host-identity rows

`domain_ascii`/`domain_unicode` are the **PSL registered domain**, not the
complete-host identity (S7-F10). The clarification is SETTLED; the full-host
identity fields are OPEN.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| existing keys are PSL, not full-host | `domain_ascii`/`domain_unicode`/`tld_ascii`/`tld_unicode` are encoding-independent **registrable-domain / public-suffix** identity keys, NOT the full-host normalized identity; `host` is rendering-selected (case/encoding-adjustable) | artifact 3; S7-F10 | SETTLED (clarification) |
| full-host identity fields | which fields expose the full-host source token, normalized ASCII host, normalized Unicode/display host, and standard host serialization (existing `domain*` cannot serve this role) | — (see Open cells HOST-O7) | OPEN |

## External-oracle / provenance rows (reference P5.3 / §6 artifact 11)

Host oracles are plural and must be labeled by authority + claim boundary (P5.3);
the register is §6 artifact 11.

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| plural labeled oracles | WPT/Ada = WHATWG parser/serialization; RFC grammar = RFC-syntax; punycoder = IDNA + selected UTS-46/DNS-shape; `pslr` + a selected list = PSL knowledge; libcurl = characterization, not a WHATWG oracle | P5.3@8292c7f (§2) — register owned by **§6 artifact 11** | SETTLED (boundary) |
| DNS-length is a separate column | Ada's optional `verify_dns_length` mode rejects fixtures the WHATWG standard accepts; standard acceptance and optional DNS-length probe are **two different expected columns**; a dependency's optional validation mode is never the URL-standard oracle | P5.3@8292c7f (§2.1); S7-F9 | SETTLED |
| per-fixture provenance | each host fixture labels oracle kind/version/config, semantic scope, PSL list identity, and any presentation normalization — the provenance schema is §6 artifact 11 / G4 | P5.3@8292c7f (§2.3) — owned by **§6 artifact 11 / G4** | SETTLED (boundary) |

## Cost / cache — delegated to G3.9

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| annotation cost + cache | the annotation execution trigger, deduplication unit, worst-case dependency calls, cache layer, complete cache key, dependency/version token, failure caching, invalidation, and cold/warm equivalence (S7-F8) are the **G3.9 semantic-cache contract** — delegated, NOT owned here; this artifact only asserts that laziness may change cost/availability but **never** parse validity or normalized identity | G3.9 (semantic-cache-contract, RURL-sapahqcb) | SETTLED (boundary — cache owned by G3.9) |

## Scope boundaries

This contract owns the **host + annotation** matrices (excl. cache). It does
**not** define, and must not be read as redefining:

- **The annotation cost/cache contract** — **G3.9** (semantic-cache). The issue's
  explicit scope split: G3.H delegates cache/cost to G3.9 and never redefines it.
- **The verdict-layer annotation-state semantics** (the typed states, fatality,
  the π projection, L1/L2 invariance) — **G3.6 / P2.3 / P1.1**. This artifact
  *applies* those states to host forms; it does not define them.
- **Scheme admission / interpretation / specialization** — **G3.5 / P2.4 / P4.1**.
  This artifact records only the host *posture* each scheme forces.
- **The comparison key / joins** (`get_url_key`, truth tables, `url_*_join`) —
  **G3.K / P3.1 / P3.2**. This artifact consumes the P3.2 root-dot DISTINCT rule
  for normalized-domain identity; it does not define keys.
- **Canonical-state field vocabulary** (`host`, `final_host`, `host_kind`,
  `whatwg_host_form`, `rfc_host_form`, `is_ip_host`, `host_is_ace`,
  `domain`/`tld`, `*_ascii`/`*_unicode`) — consumed from artifact 3 without
  renaming.
- **The comparison-key equality writer** — G3.K is the single writer of URL
  comparison equality; this artifact never redefines it.
- **The deep P4 host record (RCON-08)** — the unified typed host/IDNA/PSL/DNS/IP
  contract, IDNA pinning, PSL reproducibility fingerprint, DNS-policy typed
  contract, full IP row-set, full-host-identity fields, and de-overloaded host
  terminology are an **unmade P4 owner decision** (P4.1 §D-E). This artifact
  records their cells OPEN and does not design them.

## Open cells

Each cell the unmade P4 host record (RCON-08) owns is recorded rather than
invented. None reopens a SETTLED boundary. Shared destination unless noted: **the
dedicated P4 host record** (RCON-08; reconciliation §8 P4).

- **HOST-O1 — the unified typed host-state / semantic-axis matrix (S7-F1).** The
  full cross-product `URL-standard × scheme/parser class × authority state ×
  host_kind × host_form` with per-row source token / URL-validity / normalized
  identity / standard serialization / display / public projection / annotation
  eligibility. The *skeleton* (forms per standard, posture, eligibility) is
  SETTLED above; the complete typed matrix is the P4-host-record deliverable.
  **Impact:** without it, distinctness of a WHATWG opaque host vs an RFC reg-name
  vs a WHATWG domain host with an unknown suffix vs a PSL-registrable name is not
  provably specified. **Settles at:** the P4 host record.
- **HOST-O2 — de-overloaded host terminology + public type axes (S7-F2; owner
  decision 2).** Whether syntactic `host_form`, IP family, and PSL registrability
  become separate public axes, and the reserved glossary
  (`whatwg-domain-host`/`whatwg-opaque-host`/`rfc-reg-name`/`registered-domain`/
  `public-suffix`/…); whether `host_type = "domain"`/`domain*` are relabeled
  legacy PSL projections. **Impact:** `domain` stays overloaded across syntax,
  registrability, and public type. **Settles at:** the P4 host record.
- **HOST-O3 — v3 canonical-IDNA pinning (S7-F4; owner decision 4).** Pin the
  algorithm/library/profile/version + behavior-affecting options as the v3
  canonical WHATWG domain-to-ASCII operation, surface version drift, and keep
  lenient display decoding separate. **Impact:** IDNA behavior can drift with the
  `punycoder` dependency without a pinned contract or provenance. **Settles at:**
  the P4 host record.
- **HOST-O4 — PSL knowledge-source reproducibility (S7-F5; owner decision 5).**
  Whether the default list floats per installed `pslr` or is pinned/fingerprinted;
  list/engine identity + version surfaced in results; provenance carriage. **Impact:**
  a registered-domain value can change across `pslr` updates without the URL text
  changing, silently altering annotations. **Settles at:** the P4 host record
  (cache mechanics coordinate with G3.9).
- **HOST-O5 — typed DNS-policy contract + RFC-reg-name DNS-host eligibility
  (S7-F6; owner decisions 7, 8).** The normative DNS owner-name-policy contract
  (root-dot/empty-label handling, 63/253-octet basis, underscore/LDH rules, STD3
  non-equivalence) as a typed result, and which schemes declare an RFC reg-name a
  DNS-host candidate. **Impact:** DNS-shape facts and reg-name eligibility have no
  normative typed contract. **Settles at:** the P4 host record (live resolution
  stays out of core v3 scope).
- **HOST-O6 — full typed IP / numeric-host row-set (S7-F7).** The complete
  per-form source/acceptance/identity/serialization/display/diagnostics rows across
  both postures. The *model* is SETTLED above; the exhaustive typed rows are the
  P4-host-record deliverable. **Impact:** numeric-host identity/serialization is
  specified only at the model level, not row-by-row. **Settles at:** the P4 host
  record.
- **HOST-O7 — full-host-identity fields (S7-F10; owner decision 3).** Which fields
  expose the full-host source token, normalized ASCII host, normalized
  Unicode/display host, and standard host serialization (existing `domain*` are
  PSL registered domains, not full-host identities). **Impact:** there is no public
  full-host normalized-identity field; only the PSL registrable-domain keys exist.
  **Settles at:** the P4 host record (coordinates with the G3.7 output surfaces and
  G3.K keys).
- **HOST-O8 — optional-result API shape + display-fallback policy (S7-F8 API half;
  owner decisions 6, 10).** Whether IDNA presentation / DNS facts / PSL / any
  resolution are returned as typed companion annotations (ADR 0006) or an eager
  main-table projection, and the display-fallback policy when an A-label/U-label
  conversion fails (retain source label vs missing vs error). **Impact:** the
  public surface/typing of optional host facts is unspecified beyond the ADR 0006
  companion-only default. **Settles at:** the P4 host record (consistent with
  ADR 0006 and G3.6's `get_parse_verdicts()` companion pattern).
