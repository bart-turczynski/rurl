# Semantic cache contract (§6 artifact 10 — cache slice)

<!-- Contract artifact (§6 artifact 10, cache slice). RCON-09 cache closure.
     This record PROJECTS the ACCEPTED owner decision P5.1 (authoritative cache
     contract) and frozen evidence S8 (performance / curl-conformance /
     migration) into normative cache-inventory, key-partition, bound/eviction,
     clearing, external-data-versioning, semantic-transparency, and
     cached/uncached-equivalence matrices. It makes NO new product decision:
     every SETTLED cell is transcribed from P5.1; cells P5.1 left as owner open
     questions are flagged OPEN, never invented. This is the SINGLE WRITER of
     the v3 cache semantics. G3.H (§6 artifact 10, host/annotation remainder)
     must NOT redefine cache semantics — it consumes this contract. The
     verification/executable side (determinism gate, transparency oracle policy,
     documentation-consistency + cache-correctness checks) is owned by P5.2,
     P5.3, and G4 respectively and is referenced here as a boundary, never
     redefined. Format follows the G3.3/G3.K precedent: Envelope, tamper-evident
     Inputs, pipe-table Rows, Scope boundaries, Open cells. The envelope remains
     lifecycle_state PROPOSED until the cp-snapshot-3 seal; validator coverage
     and the manifest present-flip ride that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-semantic-cache |
| name | semantic-cache-contract |
| artifact_number | 10 (cache slice; G3.H owns the host/annotation remainder of artifact 10) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/semantic-cache-contract.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of v3 cache semantics (caches that exist, keys, bounds, eviction, clearing, external-data versioning, semantic transparency, cached/uncached equivalence) |
| lifecycle_state | PROPOSED |
| dependencies | P5.1 (bound decision); S8 (bound evidence); contract-canonical-state (Stage-A axis field vocabulary only); P5.2 (determinism gate boundary); P5.3 (transparency oracle policy boundary); reconciliation §6 artifact 10, §4 RCON-09, §7 G3–G4 |
| bound_decision | P5.1 |
| bound_evidence | S8 |
| closes_finding | RCON-09 (cache-contract dimension) |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: the caches that exist, their keys/partitions, defaults, eviction, clearing, external-data versioning, the semantic-transparency invariant, cached/uncached equivalence, and the C-08 documentation-consistency disposition each carry a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line impact and owner-decision destination; cross-artifact field names agree with artifact 3; validate-records.R (semantic-cache section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (P5.1, S8), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | semantic-cache validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative v3 contract for rurl's own caches: which caches exist, how their
keys partition, their default bounds and eviction, how they are cleared, how
external (`pslr`) data participates in the key, and the load-bearing
semantic-transparency invariant that cached and uncached execution must produce
byte-identical results. It also records the disposition of contradiction **C-08**
(the 100,000-vs-unbounded documentation drift) and the documentation-consistency
gate that disposition mandates.

This record **projects** P5.1 + S8; it does not implement the caches and makes no
product decision. A `SETTLED` row cites `P5.1@d254ff1`. A cell P5.1 left as an
owner open question is `OPEN` with its exact impact and settlement destination.
The executable checks (determinism, oracle, documentation-consistency,
cache-correctness) are named as boundaries owned by P5.2 / P5.3 / G4.

## Inputs

The exact sources this contract projects, hashed at authoring.
`validate-records.R` recomputes both hashes at the cp-snapshot-3 seal. P5.1 is
already hash-enforced as an ACCEPTED decision by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P5.1-cache-contract.md | 8420e6829675d3259ce3414a2c34a030b73ea113f101d7108add4b102076afc5 |
| design/work/url-v3/evidence/S8-performance-curl-conformance-migration.md | f1d3f985f67d4b90382866a839ce1d6cc9155cfa53f395a144fb9278de75571b |

## Cache inventory rows

The v3 cache surface is exactly the three caches in `.CACHE_REGISTRY`; the Public
Suffix List query cache lives in `pslr`, not here (ADR 0001).

| cache | memoizes | key partition | default bound | eviction | configurable via | owner_decision_ref | status |
|---|---|---|---|---|---|---|---|
| `full_parse` | the option-independent Stage-A parse core (name kept for API stability) | URL plus every Stage-A-affecting axis (protocol/scheme handling, `www_handling`, `tld_source`, relative, standard, policy, acceptance, fix-up) plus the `pslr` engine snapshot identity | **bounded: 100,000 entries** | hard **reset-watermark** — a new key that would exceed the bound clears the entire cache, then stores the new key | `rurl_cache_config(full_parse=, max_full_parse=)`; `max_full_parse = Inf` opts into historical unbounded behavior | P5.1@d254ff1 (§1.1–1.3) | SETTLED |
| `puny_encode` | IDNA/Punycode host **encode** round-trips | host/label (bounded by unique hosts/labels seen, not URL×option cross-product) | **unbounded by design** ("small by design") | none | `rurl_cache_config(puny_encode=)` | P5.1@d254ff1 (§1.1–1.2) | SETTLED |
| `puny_decode` | Punycode host **decode** round-trips | host/label (as above) | **unbounded by design** | none | `rurl_cache_config(puny_decode=)` | P5.1@d254ff1 (§1.1–1.2) | SETTLED |

All three caches are **enabled by default**. rurl memoizes only its own work.

## Key-partition and external-data-versioning rows

| aspect | contract | invariants | owner_decision_ref | status |
|---|---|---|---|---|
| `full_parse` key completeness | the key is the URL plus every Stage-A-affecting axis, so a change on any axis is a distinct entry | a change on any keyed axis can never produce a cross-profile stale hit | P5.1@d254ff1 (§1.1) | SETTLED |
| external-data version in key | the `pslr` **engine snapshot identity** participates in the `full_parse` key | a `pslr` engine or default-snapshot change must never yield a stale hit; it is a distinct key | P5.1@d254ff1 (§1.1, §1.4) | SETTLED |
| Punycode key scope | `puny_encode`/`puny_decode` are keyed by host/label only | growth is bounded by the count of unique hosts/labels, not by option cross-product | P5.1@d254ff1 (§1.1) | SETTLED |

## Bound, eviction, and clearing rows

| surface | contract | invariants | owner_decision_ref | status |
|---|---|---|---|---|
| `full_parse` default bound | bounded at 100,000 entries by default (`.onLoad` initializes `full_parse_max = 100000L`) | the shipped memory-safety default; the value `rurl_cache_config()` reports for `max_full_parse` on a freshly loaded package equals the `.onLoad` literal | P5.1@d254ff1 (§1.2, §2.1) | SETTLED |
| `full_parse` eviction | hard reset-watermark: at the bound, a genuinely new key empties the entire cache and rebuilds from scratch | `max_full_parse` caps **peak memory**, not working-set size; it is not LRU/FIFO | P5.1@d254ff1 (§1.3) | SETTLED |
| `rurl_cache_info()` | exported introspection of the three caches (state, sizes, bound) | reports the live runtime state, not a documented aspiration | P5.1@d254ff1 (§1.1) | SETTLED |
| `rurl_cache_config()` | exported configuration (enable/disable each cache; set `max_full_parse`) | toggling a cache off must not change any observable result (see transparency invariant) | P5.1@d254ff1 (§1.1–1.2) | SETTLED |
| `rurl_clear_caches()` | exported clearing of the three caches | clearing is a pure performance/memory act; a subsequent call recomputes byte-identical output | P5.1@d254ff1 (§1.1, §1.4) | SETTLED |

## Semantic-transparency invariant (the load-bearing rule)

A cache is a pure performance/memory optimization and MUST NOT change any
observable result. For every rurl operation and input vector, the output must be
**byte-identical** across every cache state below. Only elapsed time and peak
memory may differ; **component values, `parse_status`, warnings, names, order,
and types may not.**

| equivalence axis | states that must produce byte-identical output | owner_decision_ref | status |
|---|---|---|---|
| cold vs warm | first call vs a repeated call whose keys are already stored | P5.1@d254ff1 (§1.4) | SETTLED |
| enabled vs disabled | the same call with the cache toggled off via `rurl_cache_config(<cache> = FALSE)` | P5.1@d254ff1 (§1.4) | SETTLED |
| bounded vs unbounded | `max_full_parse = 100000` vs `Inf` | P5.1@d254ff1 (§1.4) | SETTLED |
| pre- vs post-eviction | a call that straddles a reset-watermark clear vs one that does not | P5.1@d254ff1 (§1.4) | SETTLED |
| scalar-loop vs one vector call vs deterministic chunk recombination | a chunked/vectorized run must not diverge from a scalar loop | P5.1@d254ff1 (§1.4) | SETTLED |
| external-data stability | a `pslr` engine/default-snapshot change never yields a stale hit (it is a distinct key, not a silent divergence) | P5.1@d254ff1 (§1.1, §1.4) | SETTLED |

**Cached/uncached equivalence** is exactly this invariant: for any operation, the
result with caches warm/bounded/enabled equals the result with caches
cold/unbounded/disabled. This contract owns the invariant; **P5.3** owns the
`self-metamorphic` oracle that *claims* it and **G4** owns the executable check
that *exercises* it with positive and negative cases (see Scope boundaries).

## C-08 disposition and documentation-consistency rows

| cell | disposition | owner_decision_ref | status |
|---|---|---|---|
| C-08 authoritative side | the **runtime default of 100,000 entries is the truth**; the `README.Rmd` claim of an unbounded default is wrong and must be corrected (correctness-over-back-compat; an unbounded default is a latent OOM/DoS risk on all-unique-input workloads) | P5.1@d254ff1 (§2.1) | SETTLED |
| README correction | `README.Rmd` must state `full_parse` is bounded at 100,000 by default (a hard reset-watermark capping peak memory) and that `max_full_parse = Inf` is the opt-in to historical unbounded behavior; `README.md` is re-knit | P5.1@d254ff1 (§2.2) — edit is a G4-tier implementation task, out of scope here | SETTLED (decision); edit deferred to G4 |
| documentation-consistency gate | a deterministic, network-free check in the `verify` chain that reads the runtime default and every documented bound (README, `rurl_cache_config`/`rurl_cache_info` roxygen → `man/*.Rd`, the Caching vignette/section if it names a number, and any inline `R/**` cache-bound comment — incl. the stale `R/zzz.R` "unbounded" comment) and **fails** if any documented bound and the runtime default diverge (Inf compared as Inf) | P5.1@d254ff1 (§2.3) — the gate **becomes a G4 executable check**; this contract fixes its required behavior, G4 implements it | SETTLED (required behavior); implementation is G4 |

## Scope boundaries

This contract owns cache **semantics**. It deliberately does **not** define, and
must not be read as redefining:

- **Determinism tolerance / gate** — owned by **P5.2**. The transparency
  invariant here is a *semantic* equality requirement; the determinism
  acceptance gate and its tolerance policy are P5.2's.
- **Cache-transparency oracle / claim policy** — owned by **P5.3**. This contract
  states the invariant to be proved; P5.3 chooses the `self-metamorphic` oracle
  and the claim discipline over it.
- **Executable documentation-consistency and cache-correctness checks** — owned
  by **G4**. This contract fixes the *required behavior* of the C-08 doc gate and
  the byte-identical cache-correctness property; G4 implements them in the
  `verify` chain (positive and negative cases). The README edit itself is a
  G4-tier task.
- **Curl-removal sequence and closure gate** — a separate P5 cell. This contract
  references the `full_parse`/percent-cache lineage only as downstream of the
  parser backend; it does not sequence curl removal.
- **Host vocabulary, IDNA, PSL, DNS, IP, external provenance/cost, and failure
  states** — the non-cache remainder of §6 artifact 10, owned by **G3.H**. G3.H
  consumes this contract for anything cache-related and must not redefine it.
- **Stage-A axis field vocabulary** — consumed from artifact 3 (canonical-state
  contract) without renaming; this contract does not re-declare those field
  names.
- **Parallel/chunked cache *scope* and worker/process ownership** — deferred to
  the P5 parallel/chunk cell (see Open cell CACHE-O5). The transparency invariant
  is stated per-process here.

## Open cells

P5.1 accepted the cache contract with the following owner questions left open;
its body **retains the shipped defaults as the accepted contract** (reset-watermark
eviction, count-based bounds, unbounded Punycode caches), so each open cell below
concerns only whether v3 should **add an alternative/enhancement or extra
coverage** — none reopens a SETTLED default. Each is recorded rather than
invented.

- **CACHE-O1 — eviction-policy alternative.** P5.1 §1.3 retains the hard
  reset-watermark as the v3 default and treats an LRU/FIFO working-set eviction
  as a deferred enhancement, not a proposal. **Impact:** a steadier hit rate on
  long unique streams is not available without per-entry bookkeeping; the default
  eviction is SETTLED, only the alternative is open. **Settles at:** a future P5
  cache-performance decision.
- **CACHE-O2 — bound units.** The `full_parse` bound is an entry count (100,000),
  not a memory ceiling; on large parsed objects peak bytes vary with entry size
  (P5.1 §Open Q2). **Impact:** no memory-based ceiling exists; peak memory is
  bounded only indirectly by entry count. **Settles at:** a P5
  performance-budget decision.
- **CACHE-O3 — Punycode cache bound.** `puny_encode`/`puny_decode` are unbounded
  "small by design," but a massive all-unique-IDN stream could grow them
  without limit (P5.1 §Open Q4). **Impact:** the only relief today is the disable
  switch; no optional bound exists. **Settles at:** a P5 cache-performance
  decision.
- **CACHE-O4 — shipped-default + reset-watermark test coverage.** The cache-policy
  test helper forces `max_full_parse = Inf`, so the 100,000 default and one
  reset-watermark cycle are not tested as defaults (P5.1 §2.3, §Open Q3; S8
  High 5). **Impact:** the shipped default and its eviction are unverified until
  covered. **Settles at:** G4 verification (P5.1 proposes adding default-and-reset
  coverage alongside the §2.3 documentation gate).
- **CACHE-O5 — worker/process cache scope.** Caches are process-local and the
  `pslr` matcher is a non-serializable external pointer; the transparency
  invariant is stated per-process (P5.1 §Open Q5; S8 Medium 9). **Impact:**
  parallel/chunked cache scope is undecided; cross-worker cache behavior is not
  specified. **Settles at:** the P5 parallel/chunk cell (not re-decided here).
