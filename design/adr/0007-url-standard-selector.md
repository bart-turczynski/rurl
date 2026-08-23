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
  not a new divergence axis — **amended, see below: the merge is not agnostic**).
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

## Amendment: reference resolution IS a divergence axis (P2.7 D-B)

*Added RURL-fupsemxr T2.5, 2026-08-12. It amends the v2 bullet's parenthetical
only, and is appended rather than edited in place so no line citation into this
file moves.*

The v2 bullet calls `resolve_url()`'s base-merge "a shared standard-agnostic
base-merge … not a new divergence axis". That is falsified by measurement, not
by argument. Scored over the WHATWG's own base-carrying corpus — 274 WPT
base-relative success rows, `tests/testthat/test-wpt-base-relative.R` — the
standard-blind merge was exact on 218 and differed on 56. Decision **P2.7 D-B**
(`design/work/url-v3/decisions/P2.7-display-and-resolver-output.md`) retires the
claim, and PRD v2 D6, which was its authority, carries the same note.

Reference resolution is now selector-governed:

- **`"whatwg"`** — WHATWG's reference-*parsing* rules run before the RFC 3986 §5
  merge: a reference carrying the base's own special scheme is relative, `\`
  reads as `/`, an arbitrary leading run of `/` and `\` introduces an authority,
  and leading/trailing C0-or-space is stripped from the reference. All of these
  are additionally gated on the base's scheme being special.
- **`"rfc3986"`** — RFC 3986 §5.2–§5.3, unchanged.
- **Both named profiles** — the scheme production is
  `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )`, RFC 3986 §3.1's own grammar,
  replacing Appendix B's self-described non-validating `[^:/?#]+`. It is not a
  WHATWG import: the two standards agree here, so it is a conformance fix on
  each profile's own terms, and it is not gated on the base's scheme because a
  reference's scheme spelling has no base dependence.

**The `NULL` freeze in §Consequences is untouched, and was verified per rule
rather than assumed.** No resolver rule — including the scheme production, whose
tightening would otherwise be a strict improvement — is reachable from
`url_standard = NULL`; Appendix B's loose group is retained there precisely
because this ADR freezes the bytes, and the freeze governs whether output may
*move*, not whether it is right. The carve-out was measured adversarially: NULL
probes are byte-identical to the pre-change tree, and the same probe goes red
when the carve-out is removed.

Re-derived at T2.5, the corpus reads **247 exact / 27 differing**. That is a
known-differ set over base-carrying rows, never a conformance rate, and it is a
disjoint population from the base-null WPT headline — the two are never summed.

## Amendment: the `NULL` freeze does not extend to the companion helpers (ADR 0015)

*Added RURL-kbpyivuk, 2026-08-23. Appended rather than edited in place so no
line citation into this file moves.*

§Consequences promises `url_standard = NULL` stays byte-for-byte compatible.
**That promise is about output which predates the selector, and it stands
unchanged for every parse function.** It does not extend to the three companion
helpers this ADR's epic introduced — `get_host_type()`,
`get_url_diagnostics()`, `get_scheme_class()` — which shipped in the same
release (rurl 2.2.0) and therefore have no pre-selector output to preserve.

Measured on `848bd11`: under `url_standard = NULL` all three answer nothing for
every input, always. [ADR 0015](0015-require-url-standard-on-companion-helpers.md)
makes the selector **required** on those three, so omitting it is an error
rather than a mode that returns `NA`. The parse functions keep the `NULL`
profile, the freeze keeps governing it, and the Appendix-B carve-out recorded
in the amendment above is unaffected.

## Amendment: the freeze binds selector-*caused* drift (ADR 0016)

*Added RURL-bmxptxxz, 2026-08-23. Appended rather than edited in place so no
line citation into this file moves.*

§Consequences promises `url_standard = NULL` stays byte-for-byte compatible and
states no boundary. [ADR 0016](0016-null-freeze-binds-selector-caused-drift.md)
supplies one: **the promise binds drift caused by introducing, extending or
conforming the selector. It does not require a pre-existing defect in the
default parse path to survive.**

A fix may move `NULL` output when it carries a witness that the defect is in the
`NULL` path and a declared signature it stays inside. It may not move `NULL` on
the strength of the axis it touches — in particular, membership in
`.URL_STANDARD_PROFILES` is conflict-matrix ownership, not behavioral
independence, and ADR 0016 falsifies the reading that an "ungoverned" axis
manifests identically under all three selector values.

The Appendix-B carve-out in the amendment above is the model for the other half:
its retention under `NULL` is a *selector-caused* change held back, which this
boundary leaves exactly where it was.

This is a different boundary from the one
[ADR 0015](0015-require-url-standard-on-companion-helpers.md) states, and
neither implies the other. ADR 0015 says *which surface* the freeze
covers; ADR 0016 says *which kind of change* it forbids on that surface.

See also **[ADR 0011](0011-path-encoding-orthogonal-presentation.md)**, which
this ADR had never cited in the other direction.
