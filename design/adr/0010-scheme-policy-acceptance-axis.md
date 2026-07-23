# ADR 0010: `scheme_policy` — the input-acceptance axis for scheme-less input

- **Status:** Accepted
- **Date:** 2026-07-08 (rurl 2.4.0)
- **Tracking:** RURL-vzgeurae. Origin: RURL-dxwxeamq / ADR 0009 (ada-005
  scheme-inference divergence). Relates to ADR 0004 (host-shape gate), ADR 0007
  (`url_standard` governed axes). Prerequisite for the compliance-presets design
  (RURL-mpktqfto).

## Context

rurl unconditionally prepends `http://` to **scheme-less host-shaped** input so
libcurl can parse it (`add_http` in `.prepare_urls_for_curl_vec()`, Phase 1).
This scheme inference was **always on**, gated by nothing — not by `url_standard`
and not by `protocol_handling`.

Two facts made this a gap, both grep- and test-verified:

1. **`protocol_handling` does not gate acceptance.** All five values
   (`keep`/`none`/`strip`/`http`/`https`) only control how the scheme is
   *presented* in `clean_url`; `example.com` parses under every one of them.
   `none`/`strip` merely omit the scheme from the output, they do not reject the
   row. So there was **no way to be strict** — no way to say "reject input that
   lacks an explicit scheme."

2. **The inference is a browser-*omnibox* affordance, not a parser behavior.**
   The omnibox invents a scheme and hands an absolute URL to the parser; the
   WHATWG/RFC parser itself never invents one. So rurl's always-on inference is a
   leniency posture layered on top of whichever standard is selected — a
   posture the caller could not turn off.

This surfaced via the ada-005 corpus row (ADR 0009): under `url_standard =
"whatwg"` rurl accepts the scheme-less `example.com`+backtick host (the
host-charset shim keeps the backtick) where Ada rejects the scheme-less form for
want of a base URL. That divergence is purely rurl's scheme inference.

The D1 host-shape gate (ADR 0004) already rejects scheme-less input that is *not*
host-shaped (`asdfghjkl`, `hello world`); the gap was specifically scheme-less
input that **is** host-shaped.

## Decision

Add a new governed axis, **`scheme_policy`**, controlling input *acceptance* for
scheme-less host-shaped input. Two values:

- **`"infer"` (default)** — today's behavior, byte-for-byte. Fabricate `http://`
  for scheme-less host-shaped input.
- **`"require"`** — strict. Any row that *would* receive a fabricated scheme
  (the `add_http` set) is folded into the reject set instead, yielding
  `parse_status = "error"`.

> **v3.0 amendment (P2.1 / P2.4, epic RURL-dorofzmb).** `"infer"`-as-**default**
> above is the **2.x** rule. At the **3.0 major boundary** the public default
> flips to **`scheme_policy = "require"`** (the `strict` posture): scheme-less
> host-shaped input **rejects by default**, and `infer` becomes opt-in (via the
> `compatibility` posture, the `browser`/`repair` posture, or an explicit
> `scheme_policy = "infer"`). The "byte-for-byte / hard constraint" framing in
> the Decision and Consequences is scoped to the **2.x line** — a 3.0 major
> release is precisely where that constraint is lifted for this axis. The
> `infer`/`require` mechanism and the three-axis orthogonality are unchanged.
> See `design/work/url-v3/decisions/P2.1-*`, `P2.4-*`.

**Mechanism.** In `.prepare_urls_for_curl_vec()`, after `add_http` is computed
and the D1 gate has run, one line under `scheme_policy == "require"` adds the
`add_http` rows to `rejected`. It is placed *before* the `scheme_less_userinfo`
computation, so a rejected row never also acquires the `warning-userinfo`
classification. `scheme_policy` is validated in `.parse_options()` via
`match.arg()` (choices `infer`/`require`) and threaded into the vector engine.
Because it changes *what curl parses* (and whether a row survives), it enters the
Stage-A parse cache key alongside `protocol_handling` / `url_standard`.

**Orthogonality — the three axes.** `scheme_policy` is deliberately separate
from the two existing axes it is often conflated with:

| Axis | Question it answers |
| --- | --- |
| `scheme_policy` | Is scheme-less input **accepted**? |
| `protocol_handling` | How is the scheme **presented** in `clean_url`? |
| `url_standard` | Which standard **interprets** the URL? |

**Boundary — scheme-relative `//host` is out of scope.** A `//host` input is
also scheme-less, but it already has a dedicated axis
(`scheme_relative_handling`, with its own `"error"` mode). `scheme_policy`
governs **only** the `add_http` inference path (bare host / `host:port`). This
keeps `//host` single-governed and the two axes independent — verified: under
`scheme_policy = "require"` a `//host` input still follows
`scheme_relative_handling` exactly (`keep` → `ok-scheme-relative`, `error` →
`error`).

## Consequences

- **Default unchanged (2.x; flipped to `require` at 3.0 — see the v3.0
  amendment above).** `"infer"` is byte-for-byte the historical behavior; the
  full pre-existing suite stays green. Backward compatibility (a hard constraint
  for the CRAN-published package) is preserved on the 2.x line.
- **Strict users get real control** — the maintainer's original set-protocol
  intent, extended to "reject if absent."
- **Scheme-inference divergences are now opt-out-able.** With `scheme_policy =
  "require"`, ada-005 rejects, matching Ada exactly on the scheme-inference axis.
  This is a strong property for the paper: a divergence rurl chooses (leniency)
  is a *knob*, not a baked-in behavior. Recorded in ADR 0009's Consequences and
  the ada-005 corpus entry (`tests/testthat/fixtures/external-url-vectors.csv`).
- **Threaded across the public surface.** `scheme_policy` is exposed on
  `safe_parse_url` / `safe_parse_urls` and the parse-configuring accessors
  (`get_parse_status`, `get_clean_url`, `get_domain`, `get_host`, `get_path`,
  `get_subdomain`, `get_tld`, `get_host_type`, `get_url_diagnostics`), matching
  how `url_standard` was threaded (ADR 0007). Accessors that expose neither
  `url_standard` nor the parse knobs (`get_scheme`, `get_query`, `get_fragment`,
  `get_port`, `get_user`/`get_password`/`get_userinfo`, `get_scheme_class`) are
  unchanged.
- **Enables the presets work.** A compliance/intent preset can only bundle a
  leniency posture once that posture is a first-class knob. This ADR makes the
  leniency axis first-class; RURL-mpktqfto (presets) builds on it and must land
  after this.

**Standing rule.** Do not gate scheme-less *acceptance* on `protocol_handling`
or `url_standard`, and do not fold `//host` handling into `scheme_policy` —
acceptance, presentation, interpretation, and scheme-relative handling are four
independent axes. Revisit only if a future standard profile needs to *force* a
scheme_policy value (as `url_standard` forces `path_encoding`); that would be a
profile→knob mapping in `.parse_options()`, not a merge of the axes.
