# ADR 0007: `url_standard` as an opt-in coherent-profile selector (no default flip)

- **Status:** Accepted
- **Date:** 2026-07-05
- **Tracking:** epics RURL-eqzkkohm (v1) and RURL-uyjheruh (v2). Specs:
  [`../prd/url-standard-selector.md`](../prd/url-standard-selector.md),
  [`../prd/url-standard-selector-v2.md`](../prd/url-standard-selector-v2.md)

## Context

rurl exposes ~20 low-level normalization knobs. Consumers who want
"RFC 3986-conformant" or "WHATWG-conformant" behavior had to hand-assemble a
coherent set of those knobs and keep them consistent. Getting one wrong yields
subtly non-conformant output. rurl also had several genuinely standard-divergent
behaviors (path percent/dot handling, numeric-host parsing) with no single
switch to select a coherent profile.

## Decision

Add a single top-level `url_standard` selector: `NULL` (default — today's
behavior, exactly), `"rfc3986"`, or `"whatwg"`. When set, it selects a coherent
set of standard-conformant behaviors for the axes it governs and
conflict-checks any low-level knob the caller supplied against the value the
profile would pick (`.URL_STANDARD_PROFILES` + `.check_url_standard_conflicts()`).

Governed axes ship in two waves:

- **v1 (RURL-eqzkkohm):** path percent/dot-segment handling, the host
  IPv4/reg-name model (see ADR 0004), `case_handling`, and a diagnostics set
  (see ADR 0006).
- **v2 (RURL-uyjheruh):** default-port elision, WHATWG backslash-as-slash for
  special schemes, DNS-length/UTS-46 diagnostics, `get_scheme_class()`, and
  `resolve_url()` (RFC 3986 §5 reference resolution composed over
  `safe_parse_urls()` — a shared standard-agnostic base-merge plus delegation,
  not a new divergence axis).
- **Pre-benchmark hardening (RURL-moselrwp, rurl 2.3.0):** two host-acceptance
  axes added under `whatwg` (both Stage-A-affecting; both a no-op under `NULL`):
  ASCII tab/LF/CR **stripping** before parse (RURL-tyetpjym) and
  forbidden-host/domain-code-point **rejection** (RURL-jfuqpwvh — see ADR 0004,
  which this supersedes for that axis). Under `rfc3986` the control chars still
  reject and the forbidden code points stay permissive reg-names, so both remain
  genuine profile-divergence axes, not default changes.

## Consequences

- **No default flip.** `url_standard = NULL` remains byte-for-byte compatible;
  the selector is purely additive. A default change would be a future major.
- A behavior axis is either a standalone editorial knob (independent of
  `url_standard`, e.g. `port_handling`, `www_handling` — NOT in
  `.URL_STANDARD_PROFILES`) or fully governed (in the conflict matrix). Decide
  which *before* touching the profile machinery.
- Governed axes that change *what is parsed* (the host model) are
  Stage-A-affecting and enter the cache key (ADR 0003); presentation-only axes
  (ports) are Stage B.
- Explicitly out of scope: `ws`/`wss`/`file` scheme expansion, query handling
  (owned by the query epic), an Ada parser backend, `max_url_length`, and
  `explain_parse_url()`.
