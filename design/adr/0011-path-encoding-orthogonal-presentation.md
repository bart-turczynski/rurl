# ADR 0011: `path_encoding` is an orthogonal presentation axis, not a governed knob

- **Status:** Accepted
- **Date:** 2026-07-08
- **Tracking:** RURL-sjnqhwtl (parent RURL-osrjtiwk, the HERO paper). Sibling:
  RURL-ndrgrwcz (Scope A — shipped the public `keep`/`encode`/`decode` knob).
  Amends the conflict matrix fixed under ADR 0007 (PRD §5 / D3).

## Context

`path_encoding` (public enum `keep`/`encode`/`decode`; RURL-ndrgrwcz) was
documented as "the path analog of `host_encoding`" — a readable-vs-browser
*presentation* choice. But unlike `host_encoding`, it was **governed** by
`url_standard`: `url_standard = "whatwg"` + any explicit `path_encoding` was an
error. `host_encoding` is orthogonal (`whatwg` + `host_encoding = "unicode"`
combine freely, ADR 0007); `path_encoding` was not. Closing that asymmetry
retires benchmark caveat #3 in `analysis/disagreement/README.md`.

The root cause was a **conflation of two axes onto one argument slot**:

1. **Path identity normalization** — the profile-internal modes
   `.rfc3986_unreserved` (decode unreserved octets only) and `.whatwg_preserve`
   (never decode; hex-case only). These decide *which percent-octets are
   canonicalized in the path a URL denotes*. A `url_standard` profile wrote its
   mode straight into `opts$path_encoding`.
2. **Path presentation** — the public `keep`/`encode`/`decode` rendering shown
   in `clean_url`.

Because both used the one slot, a profile occupying it collided with any
explicit public value — a purely mechanical conflict, not a semantic one.

Two facts (verified live, 2026-07-08) shaped the decision:

- The shipped `encode` does a full percent-decode **first**, so it collapses
  reserved octets: `/a%2Fb/c` → `/a/b/c`, exactly like `decode`. The ticket's
  original hypothesis ("`encode` is identity-preserving, so only it may layer")
  is **false** — `encode` and `decode` are in the same lossiness class on
  reserved octets, and `encode` cannot be redefined (byte-compat is a hard
  constraint). So the split is not "encode-safe / decode-unsafe"; it is
  "`keep` = identity presentation, `encode`/`decode` = lossy presentation,
  uniformly."
- The profile makes **no** presentation choice ("the profile never switches a
  readable path to the browser-encoded form"). Presentation therefore does not
  compete with any profile decision — it is genuinely orthogonal, exactly like
  `host_encoding`.

## Decision

**Split the slot. Make `path_encoding` a pure, orthogonal presentation knob and
remove it from the `url_standard` conflict matrix entirely** — mirroring
`host_encoding` and `port_handling`, which are standalone editorial/presentation
knobs, not governed axes.

- A new **internal** `opts$path_identity` (`"none"` | `".rfc3986_unreserved"` |
  `".whatwg_preserve"`) carries the profile's identity mode. It is never a
  public argument and never conflict-checked.
- `.URL_STANDARD_PROFILES` names its path entry `path_identity` (was
  `path_encoding`); `path_normalization` and `case_handling` remain governed.
- `.normalize_path_vec()` applies identity and presentation as **independent,
  composing** steps (was a mutually-exclusive `if/else-if`): the profile's
  identity mode runs, then the public presentation `decode`/`encode` transforms
  for display. All three presentation values layer on any profile.

Conflict rule for `path_encoding`: **none.** `url_standard = "whatwg"` +
`path_encoding = "encode"` now yields the WHATWG-parsed path in browser form
(`/école` → `/%C3%A9cole`), retiring caveat #3.

### Why not the stricter "only `keep` layers" alternative

The D5 precedent (ADR 0007 / PRD v2) refused `case_handling = "lower"` under a
selector because it silently alters the path. That reasoning does **not**
transfer: `case_handling` is a *governed identity* axis competing with the
profile's own case decision, whereas presentation competes with nothing the
profile decides. A stricter rule would also fail to retire the caveat, which
explicitly wants `encode` to work under `whatwg`. Presentation belongs with
`host_encoding` (zero conflicts), so full orthogonality is the coherent mirror.

## Consequences

- **Backward compatible.** `url_standard = NULL` is byte-for-byte unchanged; a
  profile with the default `path_encoding = "keep"` reproduces today's profile
  output exactly (identity mode runs, `keep` is a no-op). The only behavioral
  change is **error → success**: previously-rejected profile + explicit
  `path_encoding` combinations now compute. No working call changes output.
- `encode`/`decode` are **presentation, not identity** forms: they may
  re-encode or decode reserved octets (`%2F` ↔ `/`), so a profile's canonical
  identity path is not guaranteed to survive them. This is documented on the
  `path_encoding` parameter and is unchanged from the `url_standard = NULL`
  behavior. Consumers wanting the profile's conformant canonical key use the
  default `keep`.
- `path_identity` lives in Stage B (presentation; `.normalize_path_vec` is Stage
  B, ADR 0003) and is derived deterministically from `url_standard` at
  opts-build time, so it does **not** enter the Stage A cache key.
- The scaffold conflict tests that asserted profile + `path_encoding` errors
  flip to assert the layered output; the AC#1 "NULL is byte-for-byte inert"
  corpus gate is untouched.
