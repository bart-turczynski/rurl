# Verification contracts — cache slice (§6 artifact 11 — cache slice)

<!-- Verification artifact (§6 artifact 11, cache slice). RCON-09 executable-
     evidence dimension for the cache surface. This record maps every SETTLED
     cell of the ACCEPTED semantic-cache contract (§6 artifact 10, cache slice)
     onto executable positive/negative evidence in the shipped package and the
     verify chain, per §7 G4. It makes NO product decision and re-decides NO
     cache semantics: it VERIFIES the contract, which remains the single writer
     of those semantics.

     First writer of §6 artifact 11. Following the artifact-10 precedent (cache
     vs host, two independently enveloped slices), artifact 11 is authored as
     slices with their own envelopes rather than one monolith. This record
     claims ownership ONLY of the cache slice. The determinism comparison and
     approved-exception ledger (G4.2, projecting P5.2), the owner-approved
     release rule (G4.3, projecting C-10/P0.4), and the remaining artifact-11
     areas the reconciliation names for the whole verification contract
     (migration, curl-removal closure, benchmark budgets, and the oracle
     taxonomy/traceability for non-cache surfaces) are named as boundaries owned
     by later G4 leaves and are NOT designed here — designing them off the cache
     slice would manufacture accidental policy.

     lifecycle_state PROPOSED until a future G4 control-plane snapshot; a
     verification-family validator section and the manifest present-flip ride
     that seal, exactly as the §6 contract family did at cp-snapshot-3. No
     current validator touches design/work/url-v3/verification/. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-cache-slice |
| name | verification-cache-slice |
| artifact_number | 11 (cache slice; determinism, release-rule, and the remaining verification areas are owned by later G4 leaves) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/cache-slice.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this record is the SINGLE WRITER of the cache-slice executable-evidence map; it verifies but never redefines cache semantics |
| lifecycle_state | PROPOSED |
| verifies | contract-semantic-cache (§6 artifact 10, cache slice); P5.1 (authoritative cache contract) |
| dependencies | contract-semantic-cache (the normative source); P5.1 (bound decision); S8 (bound evidence); reconciliation §6 artifact 11, §4 RCON-09, §7 G4 |
| closes_finding | RCON-09 (executable-evidence dimension, cache slice) |
| completion_rule | §7 G4 — every SETTLED cell of the semantic-cache contract maps to a test, fixture, or tool with positive AND negative coverage and an exact command; the six equivalence axes are each exercised against the caches-disabled oracle; the C-08 documentation-consistency gate exists in the verify chain and is self-tested; each map row cites concrete evidence (path :: test name); non-cache verification areas are named as boundaries, not left as unowned cells |
| content_hash | per-input sha256 under `## Inputs`, recomputed by the verification-family validator at the sealing G4 snapshot |
| approval_evidence | pending — seals at a future G4 control-plane snapshot (NOT an envelope flip) |
| validation_command | devtools::test() && Rscript tools/cache-doc-consistency.R |
| validator_note | a verification-family validator section stages with the sealing G4 snapshot; until then no validator globs design/work/url-v3/verification/ |

## Purpose

The executable-evidence contract for rurl's caches: it demonstrates that the
accepted semantic-cache contract's SETTLED cells actually hold at runtime, with
positive and negative coverage, and that the C-08 documentation drift is both
corrected and guarded against recurrence. It exercises the load-bearing
**semantic-transparency invariant** — cached and uncached execution produce
byte-identical output — across all six equivalence axes the contract names.

This record **verifies**; it does not decide. Every claim traces to a SETTLED
cell of `contract-semantic-cache`; nothing here alters a cache default, key,
bound, eviction rule, or the invariant itself. The verification acts (tests,
gate) live in the shipped package so they run on every PR, not only in the
control plane.

## Inputs

The exact normative sources this record verifies, hashed at authoring. A future
verification-family validator recomputes them at the sealing G4 snapshot. P5.1
is additionally hash-enforced as an ACCEPTED decision by `validate-manifest.R`,
and `contract-semantic-cache` is hash-pinned in the manifest as of
v3/cp-snapshot-3.

| path | sha256 |
|---|---|
| design/work/url-v3/contracts/semantic-cache-contract.md | a70acc712e3db5f1426d45106e925d445af048df03086a230e04a4545fc67001 |
| design/work/url-v3/decisions/P5.1-cache-contract.md | 8420e6829675d3259ce3414a2c34a030b73ea113f101d7108add4b102076afc5 |
| design/work/url-v3/evidence/S8-performance-curl-conformance-migration.md | f1d3f985f67d4b90382866a839ce1d6cc9155cfa53f395a144fb9278de75571b |

## The oracle (authority and claim boundary)

Every equivalence check compares against ONE oracle: the result computed with all
three caches **disabled** (`rurl_cache_config(full_parse = FALSE, puny_encode =
FALSE, puny_decode = FALSE)`). No memoization can influence a caches-disabled
run, so any state that reproduces it byte-for-byte is proven transparent.

- **Oracle class:** `self-metamorphic` — rurl's own uncached output is the
  reference for its cached output (P5.3 owns the oracle taxonomy and claim
  policy; this record consumes the classification, it does not define it).
- **Claim boundary:** the oracle proves **internal cache transparency only** —
  that caching changes nothing observable. It makes NO claim about conformance
  to any external standard; those oracles are labeled and owned elsewhere (S9 /
  artifact 11 non-cache slices). "byte-identical" here means equal component
  values, `parse_status`, warnings, names, order, and types; only time and peak
  memory may differ.

## Semantic-transparency invariant — equivalence-axis coverage

The six axes named by `contract-semantic-cache` (## Semantic-transparency
invariant), each exercised in `tests/testthat/test-cache-transparency.R` against
the disabled-cache oracle over a representative input vector (mixed case, ports,
userinfo, query/fragment, IDN + punycode, trailing dot, a malformed input, and a
scheme-relative input).

| axis | contract cell | test (tests/testthat/test-cache-transparency.R) | positive | negative |
|---|---|---|---|---|
| cold vs warm | cold vs warm | "cold and warm parses are byte-identical" | first vs repeated call identical, both == oracle | — (see cross-profile below for the discriminator) |
| enabled vs disabled | enabled vs disabled | "cache-enabled output equals cache-disabled output" | enabled == oracle | — |
| bounded vs unbounded | bounded vs unbounded | "bounded (100000) and unbounded (Inf) parses agree" | 100000 == Inf == oracle | — |
| pre- vs post-eviction | pre- vs post-eviction | "results straddling reset-watermark evictions match the oracle" | tiny bound (3) forces repeated resets; every result == oracle; peak size <= bound | — |
| scalar vs vector vs chunk | scalar-loop vs one vector call vs chunk recombination | "scalar, vectorized, and chunked parses recombine identically" | all three == each other and == oracle | — |
| external-data / key stability | external-data stability; `full_parse` key completeness | "a keyed-axis change never produces a stale cross-profile hit" | warm one profile, call another; result == that profile's oracle | the two profiles genuinely differ (`expect_false(identical(...))`), so a stale hit WOULD be observable — the equality is not vacuous |

Additional cells:

| cell | test | property |
|---|---|---|
| `rurl_clear_caches()` purity | "clearing caches is pure: recompute is byte-identical" | clear then recompute == pre-clear result |
| reset-watermark mechanics | "reset-watermark clears the whole cache then stores the new key" | at the bound a new key empties the cache then stores (size -> 1, never bound+1); re-storing an existing key never resets |
| `full_parse` default bound = 100000 | "the shipped full_parse default bound is the 100000 source of truth" | `.FULL_PARSE_MAX_DEFAULT == 100000L`; the doc gate below ties every documented bound to it |

**Negative coverage note (§7 G4).** The transparency axes are equalities against
an independent oracle; their *discriminating power* (that a divergence would be
caught) is proven by the cross-profile test asserting two profiles are NOT
identical, and by the doc-consistency gate's own negative self-test below.

## C-08 documentation-consistency gate

`contract-semantic-cache` (## C-08 disposition) requires (§2.3) a deterministic,
network-free check in the verify chain that fails if any documented `full_parse`
bound diverges from the runtime default, and (§2.1–2.2) the README correction.

- **Source of truth:** `.FULL_PARSE_MAX_DEFAULT <- 100000L` in `R/zzz.R`, which
  `.onLoad` installs as the runtime default.
- **Gate:** `tools/cache-doc-consistency.R` reads that literal by sourcing
  `R/zzz.R` (no package build, no network) and scans README, the cache roxygen →
  `man/rurl_cache_*.Rd`, the vignettes, and inline `R/` comments; it FAILS on any
  divergence. It distinguishes the forbidden "full_parse … unbounded by default"
  from the correct "puny … unbounded by design" and the "= Inf" opt-in.
- **Positive/negative coverage:** `Rscript tools/cache-doc-consistency.R
  --self-test` exercises 3 negative (must-flag) and 5 positive (must-pass) cases
  plus thousands-separator handling, and runs in CI before the live scan.
- **CI placement:** the `cache-docs` job in `.github/workflows/verify.yml`
  (`R-CMD-check` workflow) runs the self-test then the live scan.
- **README correction (§2.1–2.2):** `README.Rmd` now states `full_parse` is
  bounded at 100000 by default (a hard reset-watermark capping peak memory) with
  `max_full_parse = Inf` as the opt-in; `README.md` is re-knit; the stale
  `R/zzz.R` "unbounded" default comment is corrected.

## Exact CI commands

```sh
# Cache-transparency + bounds/eviction + default-bound tests (in the suite):
Rscript -e 'devtools::test(filter = "cache-transparency")'
Rscript -e 'devtools::test(filter = "cache-policy")'

# C-08 documentation-consistency gate (deterministic, network-free):
Rscript tools/cache-doc-consistency.R --self-test   # positive/negative unit coverage
Rscript tools/cache-doc-consistency.R               # live scan, exits 1 on drift
```

The transparency and bounds tests run inside the standard suite, so the verify
chain's `R CMD check`, `Tests (LC_ALL=C)`, and `Test coverage` jobs all execute
them; the doc gate runs as the dedicated `cache-docs` job.

## Scope boundaries

This record owns the **cache-slice executable evidence** and nothing else. It
does NOT define, and must not be read as redefining:

- **Cache semantics** — owned by `contract-semantic-cache` (§6 artifact 10 cache
  slice) / P5.1. This record verifies those semantics; it never changes a
  default, key, bound, eviction rule, or the invariant.
- **Determinism tolerance / the failing determinism comparison + approved
  exceptions** — owned by **G4.2** (projecting **P5.2**). The transparency
  invariant here is a *semantic* byte-identity requirement per process; the
  cross-OS/R/charset determinism gate and its tolerance policy are G4.2's.
- **Oracle taxonomy and claim policy** — owned by **P5.3**. This record consumes
  the `self-metamorphic` classification; it does not define the taxonomy.
- **Owner-approved release rule (C-10)** — owned by **G4.3** (projecting P0.4).
- **The remaining §6 artifact-11 areas** the reconciliation lists for the whole
  verification contract — migration, curl-removal closure, benchmark budgets,
  and the traceability/oracle map for non-cache surfaces — owned by later G4
  leaves, authored as their own artifact-11 slices (or a fan-in), NOT here.
- **Host/annotation, IDNA, PSL, DNS, IP verification** — the non-cache remainder
  of §6 artifact 10, owned by **G3.H** and its future G4 verification slice.

## Open cells

None. Every SETTLED cache cell maps to executable evidence above; no cell is left
unowned. Two coverage notes, neither an open decision:

- **pslr engine-snapshot key axis.** The contract's external-data-versioning row
  (the `pslr` engine snapshot participates in the `full_parse` key) is verified
  here via the general keyed-axis stale-hit property (a change on any keyed axis
  is a distinct entry, never a stale hit) plus clear-and-recompute purity. A
  dedicated engine-swap fixture that mutates the `pslr` snapshot mid-session is a
  determinism/reproducibility concern that rides **G4.2** (engine identity is
  part of the determinism surface), not an open cell of this slice.
- **100000-entry default at true fresh load.** The suite locks
  `.FULL_PARSE_MAX_DEFAULT == 100000L`; the doc gate asserts the fresh-load
  runtime default in a clean Rscript session (it sources `R/zzz.R`). No open
  question remains.
