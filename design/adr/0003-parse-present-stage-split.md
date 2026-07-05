# ADR 0003: Split parsing into a cacheable Stage A and a presentation Stage B

- **Status:** Accepted
- **Date:** 2026 (rurl performance refactor)
- **Tracking:** RURL-dkwrebdt, epic RURL-gvlqokul

## Context

`safe_parse_url()` had grown to interleave the expensive, option-independent
work (curl parse, IP detection, `pslr` domain/TLD derivation) with cheap,
option-dependent presentation work (case folding, host encoding, path/query
rendering, port/clean_url assembly). Memoizing the whole thing keyed on *every*
option meant each new editorial knob (`port_handling`, `query_handling`, …)
multiplied the cache key space, and the hot path re-did the expensive parse for
callers that differed only in presentation.

## Decision

Split the pipeline in two:

- **Stage A** (`._parse_stage_a_vec`): option-independent, cacheable, keyed by
  `.parse_cache_keys()` — the URL plus only the options that change *what is
  parsed* (notably `url_standard`, which is Stage-A-affecting; see ADR 0007).
- **Stage B** (`._parse_stage_b_vec`): presentation, recomputed on every call,
  never cached — case policy, host encoding, www/subdomain policy, path
  normalization/encoding, query filtering, port rendering, `clean_url`
  assembly.

## Consequences

- Adding a presentation-only knob is cheap and cannot bloat the cache: it lives
  in Stage B (as `port_handling` and `query_handling` do).
- Adding a knob that changes *what is parsed* must enter `.parse_cache_keys()`
  or the cache will return stale results across differing values. Decide Stage A
  vs Stage B per feature — do not copy a prior "no cache-key change" conclusion
  blindly.
- The libcurl-drift snapshot lives behind `.blank_to_na` so a curl version bump
  that changes empty-vs-NULL component behavior is normalized at the seam.
