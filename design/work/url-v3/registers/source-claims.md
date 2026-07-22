# Register: source-claims (§6 artifact 2)

<!-- variant: source-claim (schema/record-schemas.yaml). Row-based register. -->


## Envelope

| Field | Value |
|---|---|
| id | reg-source-claims |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/registers/source-claims.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| dependencies | manifest.yaml; evidence/S1..S9; protocol-review-reconciliation.md §4,§9,§10 |
| completion_rule | every §9 finding has exactly one row; target_ref matches §9; validate-records.R passes |
| content_hash | per-input: evidence/SHA256SUMS.txt (frozen S1..S9); this register transfers those sources |
| approval_evidence | pending — pins at v3/cp-snapshot-2 (NOT the sealed cp-snapshot-1 manifest) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |

## Purpose

Durable transfer (not memory summary) of every severity-ranked finding in the
nine frozen reports S1–S9 into typed source-claim rows. Each row carries the
report identity + hash, the stable in-report finding locator, the verbatim
finding assertion, its reconciliation classification, and the target
finding-register record(s) (RCON) that consolidate it, per reconciliation §9.
`state` is `DISCOVERED` for every row: the frozen source identity, baseline
(89be90b), and evidence location are recorded; onward transitions (PROPOSED →
… → TRANSFERRED to a normative REQ-*) belong to later gates.

## Sources & frozen hashes (report identity → evidence path → SHA-256, §2)

| id | path | sha256 |
|---|---|---|
| S1 | design/work/url-v3/evidence/S1-object-state-vector.md | `dfde4d9d0b1bf0933732ce43b4d8dbed2bf191b1aadeb5a238d9330e4d0097be` |
| S2 | design/work/url-v3/evidence/S2-standards-validation-repair.md | `b9301eecf2a7dcf41bf2b820e0fc0a6628e6e037a61db2ec011cbd40ed929bbd` |
| S3 | design/work/url-v3/evidence/S3-serialization-format-encoding.md | `3c2e1170b62fcb4a1b77a9e52aa494fcd6ed9d369cbd6c15c7271fc15ff1e980` |
| S4 | design/work/url-v3/evidence/S4-cleaning-mutation.md | `567d59cc3429fafb9fa4149aa77e5c12c67ebe9cb54bf0255a43fdd5aab09416` |
| S5 | design/work/url-v3/evidence/S5-keys-joins.md | `bd1506841a495a77c827dd715d7fb7a31e6afb864a0ae138a304e92a62b182e9` |
| S6 | design/work/url-v3/evidence/S6-schemes-email.md | `bc70876a6ec2af42411bc783ac30914893fa06a15cc275337ff982c2c0e05c99` |
| S7 | design/work/url-v3/evidence/S7-host-idna-psl-ip.md | `8fb62419099dbe25d5c8d70ebae050f41fec67f15b0d7f19714157acd1c55636` |
| S8 | design/work/url-v3/evidence/S8-performance-curl-conformance-migration.md | `f1d3f985f67d4b90382866a839ce1d6cc9155cfa53f395a144fb9278de75571b` |
| S9 | design/work/url-v3/evidence/S9-process-red-team.md | `fc308055fc5a1465f1da942dba910ad43cd5091cac5959315dbb7f9429effa76` |

## §10 canonical path corrections applied

The S4 and S6 reports cite shortened ADR/PRD paths (reconciliation §10). Rows
sourced from S4/S6 carry `path_correction = §10 (S4)` / `§10 (S6)`; the
canonical tracked paths are:

| Report | Stale reference | Canonical tracked path |
|---|---|---|
| S4 | `design/adr/0003-two-stage-parse-architecture.md` | `design/adr/0003-parse-present-stage-split.md` |
| S4 | `design/adr/0010-scheme-inference-policy.md` | `design/adr/0010-scheme-policy-acceptance-axis.md` |
| S4 | `design/adr/0011-path-encoding-orthogonality.md` | `design/adr/0011-path-encoding-orthogonal-presentation.md` |
| S4 | `design/adr/0012-general-scheme-model-and-profile-strategy.md` | `design/adr/0012-general-url-parser-scope.md` |
| S4 | `design/prd/browser-fixer.md` | `design/prd/browser-fixer-and-url-search-classifier.md` |
| S4 | `design/prd/v2.md` | `design/prd/url-standard-selector-v2.md` |
| S6 | `design/adr/0006-url-diagnostic-warnings.md` | `design/adr/0006-diagnostics-companion-helpers-only.md` |
| S6 | `design/adr/0007-url-standard-selection.md` | `design/adr/0007-url-standard-selector.md` |
| S6 | `design/adr/0010-scheme-policy.md` | `design/adr/0010-scheme-policy-acceptance-axis.md` |

## Rows

| claim_id | source | severity | assertion | classification | target_ref | state | locator | path_correction |
|---|---|---|---|---|---|---|---|---|
| S1 F1 | S1 | blocker | no mandatory canonical state-schema artifact or lifetime invariant | reconciled-protocol-requirement | RCON-02 | DISCOVERED | S1-F1 | — |
| S1 F2 | S1 | blocker | “standards serialization” is not separated contractually from lossy `clean_url` | reconciled-protocol-requirement | RCON-03 | DISCOVERED | S1-F2 | — |
| S1 F3 | S1 | blocker | scalar/vector behavior is a checklist item, not a normative behavior table | reconciled-protocol-requirement | RCON-09 | DISCOVERED | S1-F3 | — |
| S1 F4 | S1 | high | the protocol does not require an exhaustive, mechanically checked behavior-dial registry | reconciled-protocol-requirement | RCON-05; RCON-09 | DISCOVERED | S1-F4 | — |
| S1 F5 | S1 | high | authority state has contradictory accepted/shipped meanings and no required owner resolution | reconciled-protocol-requirement | RCON-02; RCON-08 | DISCOVERED | S1-F5 | — |
| S1 F6 | S1 | high | top-level query presence is modeled, but mutation-grade query structure is not | reconciled-protocol-requirement | RCON-02; RCON-06; RCON-07 | DISCOVERED | S1-F6 | — |
| S1 F7 | S1 | high | “list path” is a label over a string, not a specified path data model | reconciled-protocol-requirement | RCON-02; RCON-06; RCON-07 | DISCOVERED | S1-F7 | — |
| S1 F8 | S1 | high | accepted eligibility rules and shipped dial consumption conflict | reconciled-protocol-requirement | RCON-05 | DISCOVERED | S1-F8 | — |
| S1 F9 | S1 | medium | source spelling, parsed identity, serialization, display, and cleaning are named but not field-mapped | reconciled-protocol-requirement | RCON-02 | DISCOVERED | S1-F9 | — |
| S1 F10 | S1 | medium | current tests are useful characterization, but the protocol does not mandate oracle classification plus metamorphic state assertions | reconciled-protocol-requirement | RCON-09; RCON-10 | DISCOVERED | S1-F10 | — |
| S1 F11 | S1 | medium | cache transparency is named only indirectly and state reconstruction from source remains possible | reconciled-protocol-requirement | RCON-02; RCON-09 | DISCOVERED | S1-F11 | — |
| S2 S2-01 | S2 | critical | no mandatory multi-verdict validation model | reconciled-protocol-requirement | RCON-04 | DISCOVERED | S2-01 | — |
| S2 S2-02 | S2 | critical | no ordered mutation/recovery provenance gate | reconciled-protocol-requirement | RCON-04 | DISCOVERED | S2-02 | — |
| S2 S2-03 | S2 | high | conformance evidence gate permits projection matching to masquerade as full-string conformance | reconciled-protocol-requirement | RCON-03; RCON-04; RCON-10 | DISCOVERED | S2-03 | — |
| S2 S2-04 | S2 | high | the dial/profile matrix is not a mandatory reconstruction artifact | reconciled-protocol-requirement | RCON-05 | DISCOVERED | S2-04 | — |
| S2 S2-05 | S2 | high | optional annotation validity has no required state model | reconciled-protocol-requirement | RCON-04; RCON-08 | DISCOVERED | S2-05 | — |
| S2 S2-06 | S2 | high | evidence lifecycle is document-level where the sources require assertion-level classification | reconciled-protocol-requirement | RCON-04; RCON-10 | DISCOVERED | S2-06 | — |
| S2 S2-07 | S2 | medium | serializer/output validity is deferred to red-team review instead of gated before drafting | reconciled-protocol-requirement | RCON-03; RCON-04; RCON-10 | DISCOVERED | S2-07 | — |
| S2 S2-08 | S2 | medium | reference resolution is at risk of being inherited as a fact rather than reconstructed | reconciled-protocol-requirement | RCON-04; RCON-10 | DISCOVERED | S2-08 | — |
| S2 S2-09 | S2 | medium | browser-fixer phase order is contradictory across accepted prose and shipped code | reconciled-protocol-requirement | RCON-04 | DISCOVERED | S2-09 | — |
| S3 F1 | S3 | blocker | “Lossless or faithful” conflates source reproduction with standard serialization | reconciled-protocol-requirement | RCON-03; RCON-10 | DISCOVERED | S3-F1 | — |
| S3 F2 | S3 | blocker | The protocol does not require a serializer-ready lossless state record | reconciled-protocol-requirement | RCON-02; RCON-03 | DISCOVERED | S3-F2 | — |
| S3 F3 | S3 | high | `format_url()` safety is a topic list, not an executable contract | reconciled-protocol-requirement | RCON-03 | DISCOVERED | S3-F3 | — |
| S3 F4 | S3 | high | Query lexical spelling is not covered deeply enough for source or standards serialization | reconciled-protocol-requirement | RCON-02; RCON-03; RCON-06 | DISCOVERED | S3-F4 | — |
| S3 F5 | S3 | high | Credential reassembly needs delimiter state, not only `user` and `password` | reconciled-protocol-requirement | RCON-02; RCON-03; RCON-06; RCON-07 | DISCOVERED | S3-F5 | — |
| S3 F6 | S3 | high | Current presentation/identity behavior contains an unregistered accepted-design-versus-code conflict | reconciled-protocol-requirement | RCON-03; RCON-07 | DISCOVERED | S3-F6 | — |
| S3 F7 | S3 | medium | Output encoding marks and invalid-byte behavior are not part of the required artifact schema | reconciled-protocol-requirement | RCON-03 | DISCOVERED | S3-F7 | — |
| S4 F1 | S4 | high | The protocol does not require a complete cleaning semantics artifact | reconciled-protocol-requirement | RCON-03; RCON-06 | DISCOVERED | S4-F1 | §10 (S4) |
| S4 F2 | S4 | high | Lossy cleaning and standards-valid output are conflated (design fork) | reconciled-protocol-requirement | RCON-03; RCON-06 | DISCOVERED | S4-F2 | §10 (S4) |
| S4 F3 | S4 | high | Cleaning’s repair provenance is not resolved | reconciled-protocol-requirement | RCON-03; RCON-04; RCON-06 | DISCOVERED | S4-F3 | §10 (S4) |
| S4 F4 | S4 | high | Per-scheme and per-shape cleaning/mutation eligibility is not mandatory | reconciled-protocol-requirement | RCON-03; RCON-05; RCON-06 | DISCOVERED | S4-F4 | §10 (S4) |
| S4 F5 | S4 | medium | The vocabulary audit is scoped to one misleading `keep` | reconciled-protocol-requirement | RCON-03; RCON-05; RCON-06 | DISCOVERED | S4-F5 | §10 (S4) |
| S4 F6 | S4 | high | Mutation names overlapping and derived components without dependency rules | reconciled-protocol-requirement | RCON-02; RCON-06 | DISCOVERED | S4-F6 | §10 (S4) |
| S4 F7 | S4 | high | “Transactional validation” has no transaction contract | reconciled-protocol-requirement | RCON-02; RCON-04; RCON-06 | DISCOVERED | S4-F7 | §10 (S4) |
| S4 F8 | S4 | medium | Mutation verbs are not bound to absent and present-empty states | reconciled-protocol-requirement | RCON-02; RCON-06 | DISCOVERED | S4-F8 | §10 (S4) |
| S4 F9 | S4 | medium | Query mutation lacks an ordered multimap and encoding contract | reconciled-protocol-requirement | RCON-02; RCON-06 | DISCOVERED | S4-F9 | §10 (S4) |
| S4 F10 | S4 | medium | Path mutation lacks a structured/opaque and encoded-delimiter contract | reconciled-protocol-requirement | RCON-02; RCON-06 | DISCOVERED | S4-F10 | §10 (S4) |
| S4 F11 | S4 | medium | Standards/profile precedence is not specified for mutation or the new clean contract | reconciled-protocol-requirement | RCON-04; RCON-05; RCON-06 | DISCOVERED | S4-F11 | §10 (S4) |
| S4 F12 | S4 | medium | Credential mutation lacks a security/display requirement | reconciled-protocol-requirement | RCON-03; RCON-06 | DISCOVERED | S4-F12 | §10 (S4) |
| S4 F13 | S4 | medium | Cleaning outputs are not classified for downstream identity and redirect use | reconciled-protocol-requirement | RCON-03; RCON-06; RCON-07 | DISCOVERED | S4-F13 | §10 (S4) |
| S4 F14 | S4 | medium | PSL source/version is an unowned input to domain mutation | reconciled-protocol-requirement | RCON-04; RCON-06; RCON-08 | DISCOVERED | S4-F14 | §10 (S4) |
| S4 F15 | S4 | medium | Mutation has no vector and error-recovery contract | reconciled-protocol-requirement | RCON-04; RCON-06; RCON-09 | DISCOVERED | S4-F15 | §10 (S4) |
| S5 finding 1 | S5 | blocker | `get_url_key()` has no normative data or vector contract | reconciled-protocol-requirement | RCON-07; RCON-09 | DISCOVERED | BLOCKER 1 | — |
| S5 finding 2 | S5 | blocker | “dplyr-shaped” does not specify the six joins | reconciled-protocol-requirement | RCON-07; RCON-09 | DISCOVERED | BLOCKER 2 | — |
| S5 finding 3 | S5 | high | Shipped joins consume a lossy display/cleaning representation despite accepted identity rules | reconciled-protocol-requirement | RCON-03; RCON-07 | DISCOVERED | HIGH 3 | — |
| S5 finding 4 | S5 | high | Scheme and port equivalence is named but not closed under all input states | reconciled-protocol-requirement | RCON-03; RCON-07 | DISCOVERED | HIGH 4 | — |
| S5 finding 5 | S5 | high | Host, path, query, userinfo, and fragment equivalence defaults are absent | reconciled-protocol-requirement | RCON-03; RCON-07 | DISCOVERED | HIGH 5 | — |
| S5 finding 6 | S5 | high | There is no migration contract for the current equality-dial surface | reconciled-protocol-requirement | RCON-03; RCON-07 | DISCOVERED | HIGH 6 | — |
| S5 finding 7 | S5 | high | Invalid, missing, warning, duplicate, and relationship policies are not a coherent model | reconciled-protocol-requirement | RCON-07; RCON-09 | DISCOVERED | HIGH 7 | — |
| S5 finding 8 | S5 | medium | Result schema, order, name repair, suffix behavior, and zero-row prototypes are unspecified | reconciled-protocol-requirement | RCON-07; RCON-09 | DISCOVERED | MEDIUM 8 | — |
| S5 finding 9 | S5 | medium | Authority records do not state which older “canonical key” claims are superseded | reconciled-protocol-requirement | RCON-07 | DISCOVERED | MEDIUM 9 | — |
| S6 finding 1 | S6 | blocker | §6.6 does not require the scheme matrix needed to preserve current semantics | reconciled-protocol-requirement | RCON-05 | DISCOVERED | Blocker 1 | §10 (S6) |
| S6 finding 2 | S6 | blocker | The protocol does not reconstruct the selector lattice or profile/direct-call differences | reconciled-protocol-requirement | RCON-05 | DISCOVERED | Blocker 2 | §10 (S6) |
| S6 finding 3 | S6 | blocker | Credentials/userinfo are named but not modeled as distinct routes | reconciled-protocol-requirement | RCON-03; RCON-05 | DISCOVERED | Blocker 3 | §10 (S6) |
| S6 finding 4 | S6 | blocker | The `mailto:` checklist is too shallow to recover the accepted contract | reconciled-protocol-requirement | RCON-03; RCON-05 | DISCOVERED | Blocker 4 | §10 (S6) |
| S6 finding 5 | S6 | high | Resolution is absent as a reconstruction layer | reconciled-protocol-requirement | RCON-04; RCON-10 | DISCOVERED | High 5 | §10 (S6) |
| S6 finding 6 | S6 | high | The evidence artifacts do not force accepted-versus-shipped disposition per contract | reconciled-protocol-requirement | RCON-04; RCON-08; RCON-10 | DISCOVERED | High 6 | §10 (S6) |
| S6 finding 7 | S6 | high | Diagnostic selection and validity remain underspecified | reconciled-protocol-requirement | RCON-04; RCON-08 | DISCOVERED | High 7 | §10 (S6) |
| S6 finding 8 | S6 | medium | Scheme-relative input is liable to be conflated with schemes or browser repair | reconciled-protocol-requirement | RCON-05 | DISCOVERED | Medium 8 | §10 (S6) |
| S6 finding 9 | S6 | medium | FTPS/SFTP/default-port/fixer distinctions need explicit fixtures | reconciled-protocol-requirement | RCON-03; RCON-05 | DISCOVERED | Medium 9 | §10 (S6) |
| S6 finding 10 | S6 | medium | Point-in-time and stale documentation are not clearly quarantined | reconciled-protocol-requirement | RCON-05; RCON-10 | DISCOVERED | Medium 10 | §10 (S6) |
| S7 F1 | S7 | blocker | no normative host-state and semantic-axis matrix | reconciled-protocol-requirement | RCON-02; RCON-05; RCON-08 | DISCOVERED | S7-F1 | — |
| S7 F2 | S7 | blocker | `domain` vocabulary is overloaded across syntax, registrability, and public type | reconciled-protocol-requirement | RCON-02; RCON-08 | DISCOVERED | S7-F2 | — |
| S7 F3 | S7 | blocker | optional annotation outcomes and parse validity have no required state model | reconciled-protocol-requirement | RCON-02; RCON-04; RCON-08 | DISCOVERED | S7-F3 | — |
| S7 F4 | S7 | high | the IDNA contract is under-specified | reconciled-protocol-requirement | RCON-08 | DISCOVERED | S7-F4 | — |
| S7 F5 | S7 | high | the PSL contract lacks knowledge-source identity and reproducibility rules | reconciled-protocol-requirement | RCON-06; RCON-08 | DISCOVERED | S7-F5 | — |
| S7 F6 | S7 | high | DNS syntax, product policy, and resolution need separate contracts | reconciled-protocol-requirement | RCON-04; RCON-05; RCON-08 | DISCOVERED | S7-F6 | — |
| S7 F7 | S7 | high | IP source, identity, serialization, and annotation eligibility are not explicit enough | reconciled-protocol-requirement | RCON-02; RCON-05; RCON-08 | DISCOVERED | S7-F7 | — |
| S7 F8 | S7 | high | laziness, cost, cache keys, and dependency failures are not reconstructible | reconciled-protocol-requirement | RCON-04; RCON-05; RCON-06; RCON-08 | DISCOVERED | S7-F8 | — |
| S7 F9 | S7 | high | external oracles are named but their authority boundaries are not mandatory | reconciled-protocol-requirement | RCON-04; RCON-08; RCON-10 | DISCOVERED | S7-F9 | — |
| S7 F10 | S7 | medium | public identity fields and compatibility projections need ownership | reconciled-protocol-requirement | RCON-02; RCON-08 | DISCOVERED | S7-F10 | — |
| S8 finding 1 | S8 | blocker | The CRAN rule conflicts with the accepted epic’s latest release disposition | reconciled-protocol-requirement | RCON-10 | DISCOVERED | Blocker 1 | — |
| S8 finding 2 | S8 | blocker | “Complete curl removal” has no auditable closure artifact or negative gate | reconciled-protocol-requirement | RCON-09 | DISCOVERED | Blocker 2 | — |
| S8 finding 3 | S8 | blocker | Performance is a goal without a falsifiable contract | reconciled-protocol-requirement | RCON-09 | DISCOVERED | Blocker 3 | — |
| S8 finding 4 | S8 | high | Vectorization is not reconstructed across the public API | reconciled-protocol-requirement | RCON-05; RCON-07; RCON-09 | DISCOVERED | High 4 | — |
| S8 finding 5 | S8 | high | Cache behavior is under-specified and the current evidence base contradicts itself | reconciled-protocol-requirement | RCON-09 | DISCOVERED | High 5 | — |
| S8 finding 6 | S8 | high | Conformance provenance and claim scope are not mandatory enough to prevent a known failure mode | reconciled-protocol-requirement | RCON-10 | DISCOVERED | High 6 | — |
| S8 finding 7 | S8 | high | Determinism is evidence collection, not an acceptance gate | reconciled-protocol-requirement | RCON-10 | DISCOVERED | High 7 | — |
| S8 finding 8 | S8 | high | Migration coverage is chapter-shaped, not public-surface complete | reconciled-protocol-requirement | RCON-05; RCON-07; RCON-10 | DISCOVERED | High 8 | — |
| S8 finding 9 | S8 | medium | PSL engine, chunk, and worker determinism lack an explicit support boundary | reconciled-protocol-requirement | RCON-05; RCON-08; RCON-09 | DISCOVERED | Medium 9 | — |
| S8 finding 10 | S8 | medium | Optional annotation cost is named but not measurable | reconciled-protocol-requirement | RCON-05; RCON-08; RCON-09 | DISCOVERED | Medium 10 | — |
| S8 finding 11 | S8 | medium | Query performance and encoding parity are hidden inside the broad vector example | reconciled-protocol-requirement | RCON-09 | DISCOVERED | Medium 11 | — |
| S8 finding 12 | S8 | medium | Accepted Stage A/B and curl-shim architecture may be mistaken for immutable v3 design | reconciled-protocol-requirement | RCON-09 | DISCOVERED | Medium 12 | — |
| S8 finding 13 | S8 | low | Current-state documentation contradictions need an acceptance-time closure rule | reconciled-protocol-requirement | RCON-10 | DISCOVERED | Low 13 | — |
| S9 C1 | S9 | critical | The process control plane is ephemeral | reconciled-protocol-requirement | RCON-01 | DISCOVERED | C1 | — |
| S9 C2 | S9 | critical | Highest-precedence owner decisions are not durably identifiable | reconciled-protocol-requirement | RCON-01 | DISCOVERED | C2 | — |
| S9 C3 | S9 | critical | The named investigations and evidence baseline cannot be independently recovered | reconciled-protocol-requirement | RCON-01 | DISCOVERED | C3 | — |
| S9 H1 | S9 | high | Artifact schemas and transitions are underspecified | reconciled-protocol-requirement | RCON-01 | DISCOVERED | H1 | — |
| S9 H2 | S9 | high | There is no frozen source/tracker baseline or completeness closure | reconciled-protocol-requirement | RCON-01 | DISCOVERED | H2 | — |
| S9 H3 | S9 | high | Existing authority and supersession contradictions are not forced to closure | reconciled-protocol-requirement | RCON-01 | DISCOVERED | H3 | — |
| S9 H4 | S9 | high | Draft-only red-team review cannot validate evidence claims | reconciled-protocol-requirement | RCON-10 | DISCOVERED | H4 | — |
| S9 H5 | S9 | high | Approval and graduation gates are subjective | reconciled-protocol-requirement | RCON-10 | DISCOVERED | H5 | — |
| S9 H6 | S9 | high | Traceability and examples are not sufficient to prove testability | reconciled-protocol-requirement | RCON-10 | DISCOVERED | H6 | — |
| S9 H7 | S9 | high | Tracker scope and mutation rules are unresolved | reconciled-protocol-requirement | RCON-01 | DISCOVERED | H7 | — |
| S9 H8 | S9 | high | Source changes do not invalidate prior decisions or reviews | reconciled-protocol-requirement | RCON-01 | DISCOVERED | H8 | — |
| S9 H9 | S9 | high | Historical-draft preservation is asserted, not guaranteed | reconciled-protocol-requirement | RCON-01 | DISCOVERED | H9 | — |
| S9 H10 | S9 | high | Implementation delay has no enforcement mechanism | reconciled-protocol-requirement | RCON-10 | DISCOVERED | H10 | — |
| S9 H11 | S9 | high | The verification gate is not mapped to actual CI behavior | reconciled-protocol-requirement | RCON-10 | DISCOVERED | H11 | — |
| S9 M1 | S9 | medium | The compact run-state schema is too lossy for handoff | reconciled-protocol-requirement | RCON-01 | DISCOVERED | M1 | — |
| S9 M2 | S9 | medium | External fixture provenance is not revision-complete | reconciled-protocol-requirement | RCON-01 | DISCOVERED | M2 | — |
| S9 M3 | S9 | medium | Review findings have no disposition/closure lifecycle | reconciled-protocol-requirement | RCON-01 | DISCOVERED | M3 | — |
| S9 M4 | S9 | medium | Batch boundaries ignore cross-cutting dependencies | reconciled-protocol-requirement | RCON-01 | DISCOVERED | M4 | — |
| S9 M5 | S9 | medium | No restore drill or single-writer responsibility is defined | reconciled-protocol-requirement | RCON-01 | DISCOVERED | M5 | — |

