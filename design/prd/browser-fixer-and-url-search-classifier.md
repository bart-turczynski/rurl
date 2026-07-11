# PRD: bounded browser fixer + URL-vs-search classifier

- **Status:** Accepted (graduated per ADR 0008). Layer 6 — the bounded browser
  fixer (`fixup_posture`) and `profile="browser"` — shipped in rurl 2.6.0. Layer 7
  (the URL-vs-search classifier specified below) remains a pending follow-up
  (RURL-zdwfkaxd).
- **Date:** 2026-07-10
- **Spun out of:** ADR 0012 (general URL parser) — this was Layer 6's fixer spec,
  Layer 7 (URL-vs-search classifier), and Appendix A.3 (Chromium omnibox
  research) in the full working draft. Split out so ADR 0012 is reviewable on its
  own.
- **Relationship to ADR 0012:**
  - The **bounded fixer** is a *prerequisite* for exposing the `browser` profile
    (ADR 0012 D6 / Layer 6). ADR 0012 owns the decision that `browser` is a
    fix-up posture (standing rule 2) and that the fixer must share the
    `scheme_policy=infer` seam (D4); this PRD owns the fixer's exact behavior.
  - The **URL-vs-search classifier** is ADR 0012's optional Layer 7 — fully
    non-blocking; a companion classifier adjacent to parsing, not parsing.
- **Tracking:** RURL-mpktqfto (presets/fixer), RURL-pbbkzccp (parent scope).

---

## Central insight

**"browser/omnibox" is not a peer of `whatwg`/`rfc`.** It is a *fix-up posture*
(guess intent, invent a scheme, repair the string) layered *in front of* a spec
parser that itself never guesses. rurl already has this seam:
`scheme_policy=infer` is the scheme-invention step. The `browser` profile adds a
**bounded, inspectable** fixer before the WHATWG parser. Standards-required
WHATWG preprocessing stays parser behavior, not mislabeled fixer policy.

The `browser` profile is **browser-*like*, not Chrome-faithful.** Chromium has
implementation-specific divergences we deliberately do not chase.

---

## Part 1 — the bounded fixer (Layer 6 prerequisite)

A non-classifying fixer, run before the WHATWG parser under the `browser` bundle:

- **Outer C0/space trim.**
- **The selected WHATWG preprocessing** (parser behavior, not fixer policy —
  named here only for ordering).
- **`;`→`:`** only before a **recognized-scheme** token (not every syntactically
  valid scheme).
- **Missing authority slashes** (`://` insertion) only for schemes in the
  **authority table**.
- **Fallback `http`** (NOT https — see Part 3) only on the existing
  scheme-inference path (`scheme_policy=infer`), so http-prepend lives in **one**
  place shared with the default posture (ADR 0012 D4).

### The two tables (acceptance criteria — enumerated, ordered, exampled)

The fixer's "bounded"-ness is only real if these tables are closed and checked in.
They are keyed to the **`browser` profile's own interpretation standard (WHATWG)**,
not to rurl's broader `.SUPPORTED_SCHEMES` — because a fix-up only makes sense for
a scheme the downstream parser treats as authority-based. Under WHATWG the
authority-based schemes are exactly the **special schemes**, so both fixer tables
resolve to the same set and there is no half-repair asymmetry:

- **Recognized-scheme set** (gates `;`→`:`) **= authority table** (gates `://`
  insertion) **= `.WHATWG_SPECIAL_SCHEMES` after Layer 1 adds `ws`/`wss`** →
  `{http, https, ftp, file, ws, wss}`. A `;` before, or a missing `//` after, any
  token **outside** this set is left verbatim.
- **`ftps` is excluded from both tables** — it is **non-special under WHATWG**, so
  the `browser` profile parses `ftps:host` as an opaque path regardless; inserting
  `://` would only manufacture a spurious non-special authority. (This is separate
  from `ftps` lacking a standards-backed default port, ADR 0012 D5. Note rurl
  *does* accept an explicitly-written `ftps://host` — the fixer simply does not
  *synthesize* the authority. Browsers removed the FTP family entirely in M95, so
  no browser-faithfulness is lost either way.)
- No "internal/chrome" schemes are recognized — rurl is not a browser.

**Ordering** (each step feeds the next; deterministic, single pass):
1. outer C0/space trim → 2. `;`→`:` (recognized-scheme set only) → 3. `://`
insertion (authority table only) → 4. `scheme_policy=infer` fallback `http`.

**Worked examples (acceptance cases):**

| Input | After fixer | Rule |
| --- | --- | --- |
| `http;//example.com` | `http://example.com` | step 2 (`http` recognized) |
| `mailto;x@y.com` | `mailto;x@y.com` (unchanged) | step 2 skipped (`mailto` not recognized) |
| `http:example.com` | `http://example.com` | step 3 (`http` in authority table) |
| `ftps;host/p` | `ftps;host/p` (unchanged) | step 2 skipped (`ftps` non-special, not in tables) |
| `ftps:host/p` | `ftps:host/p` (unchanged) | step 3 skipped (`ftps` non-special → opaque under WHATWG) |
| `foo:bar` | `foo:bar` (unchanged) | opaque; no step applies |
| `example.com` | `http://example.com` | step 4 (`scheme_policy=infer`) |

Any change to these tables is a versioned change with new acceptance rows, not an
inline tweak.

This pass **never** decides URL-vs-search and **never** rewrites an explicit
unknown scheme to HTTP. `fixup_posture` must be included in the Stage-A cache key
so a fixed parse never collides with an unfixed one.

**Profile wiring:** the `browser` profile is not available until its bounded
fixer is. Add the `profile` arg, missing-vs-explicit resolution, final-
combination validation, and `url_profile()` inspector (`customized` flag) — but
those are ADR 0012 D6 / Layer 6 mechanics; this PRD owns only the fixer behavior.

---

## Part 2 — the URL-vs-search classifier (optional, ADR 0012 Layer 7)

A companion classifier implementing a **documented subset inspired by** Chromium's
`AutocompleteInput::Parse`, built from the PSL + host-shape machinery rurl owns.
Facts (a verdict), not navigation.

**Do not claim Chrome mirroring** without its platform-file, `SchemeClassifier`,
JavaScript, GURL-validity, host-compliance, credential, whitespace, port, and
IPv4 branches plus Chromium fixtures. It **consumes Part 1's fixer**; it does not
own or silently expand repair behavior.

Verdict vocabulary (subset): URL / QUERY / UNKNOWN, with the documented
tie-breakers below.

---

## Part 3 — Chromium omnibox research (the model)

**Not a spec.** Chrome has no normative omnibox specification; this is
implementation research gathered against a recent Chromium `main`. The commit
`0fa6c98b660d6151f789297e6880161f594b923d` cited in earlier revisions could
**not be independently confirmed** — treat every file path, verbatim quote, and
line reference as **revision-fragile**; re-confirm at HEAD before relying on an
exact string. Cite files by role, not by a pinned SHA.

### Two sequential layers — only the first guesses

1. **Intent classify + fix-up** (`components/omnibox/browser/autocomplete_input.cc`
   `AutocompleteInput::Parse` → `components/url_formatter/url_fixer.cc`
   `FixupURL`/`SegmentURL` — the fixer is `url_fixer.cc` directly under
   `components/url_formatter/`, **not** a nested `url_fixer/url_fixer.cc`).
   Heuristic UI layer; the only place a scheme is invented.
2. **Chromium's URL library** (`url/`, GURL). Do NOT call GURL "the WHATWG
   parser." Chromium's `url/README` documents separate best-guess parsing,
   canonicalization, utility, and GURL layers; its low-level parser "can never
   fail." GURL is WHATWG-*aligned* canonicalization, not a drop-in WHATWG parser.
   Key invariant: the parse/canonicalize layer never invents a scheme; scheme
   invention is exclusively fix-up (layer 1).

### URL-vs-search classification (NOT strict "order as implemented")

- Canonicalize via `FixupURL`; nothing parseable → QUERY.
- Explicit scheme, per-scheme: `http`/`https` → continue host heuristics; `file:`
  → URL (QUERY on iOS/Android); `javascript:` → UNKNOWN if prose-shaped. A
  non-http explicit scheme is **first delegated to `SchemeClassifier`**; only an
  **unknown/unhandled** scheme falls through to UNKNOWN and the `http://`-prepend
  retry, which exists *only* to resolve the `username:password@host` ambiguity —
  not a general "unknown scheme becomes http".
- **Known-TLD check via the PSL** — uses the *public* registry only (excludes
  private PSL registries). Chromium *separately* recognizes `localhost` and
  `.example`/`.test`/`.local`/`.internal` — but those four require **at least one
  preceding label**; a bare `example` is NOT treated like `localhost`.
- **Ambiguous bare inputs** (no explicit scheme, no obvious authority): IPv6 →
  always URL; a **fully typed four-component IPv4** whose canonical first octet is
  nonzero — plus exactly `0.0.0.0` — immediately forces URL; other `0/8` forms
  become QUERY. A non-default port → URL; a trailing `/` or `\` → URL;
  `NumNonHostComponents() > 1` → URL. These are tie-breakers on the
  ambiguous-input path, not gates on every branch. Two- and three-component IPv4
  forms can be forced by an explicit HTTP(S) scheme or trailing slash before
  otherwise hitting a QUERY return; one-component forms can continue farther.
- Single bare word / unknown-TLD host → UNKNOWN (search now + retained navigable
  fallback). A space → almost always QUERY.

### Scheme selection when none typed (`SegmentURL`)

Recognized Windows drive/UNC path → `file:`; on POSIX a leading `/` or `~` →
`file:`; extractable valid scheme → use it; else **fallback `http`**
(`url::kHttpScheme`). `localhost`/bare IP/`host:port`/`user:pass@host` get
`http://`; credentials preserved with minimal fixing.

### String repair (scoped precisely)

Trim **outer** C0 controls/spaces; **remove interior CR/LF/TAB specifically** —
the canonicalizer's `IsRemovableURLWhitespace` matches exactly `\r`, `\n`, `\t`,
not "all control chars"; other controls are **percent-encoded component-wise** by
canonicalization, not stripped by the fixer. This CR/LF/TAB-removal step has
**no** `data:` exception — the `data:` whitespace-preservation behavior is a
**separate** mechanism in `net/base/data_url.cc`, not a carve-out inside the
whitespace remover. For special/`file` URLs, parsing treats slash and backslash
alike at post-scheme and authority boundaries and path canonicalization
normalizes path backslashes; not a blanket rewrite for non-special URLs. `;`→`:`
applies **only before a Chromium-recognized standard/internal scheme**; insert
`://` after a standard scheme missing its slashes. Note: `http:example.com` **is**
valid *generic* RFC 3986 syntax, but WHATWG raises
`special-scheme-missing-following-solidus` and it is **not** a valid HTTP URI (RFC
9110 §4.2.1) — so "insert the slashes" is a fix-up, not evidence of conformance.

### HTTP vs HTTPS

`FixupURL`'s fallback is **`http`**. The older scheme-less HTTPS-default feature
inside `AutocompleteInput` is now enabled by default **only on iOS**; on non-iOS
Chromium it is superseded by navigation-layer **HTTPS-Upgrades**
(`https_upgrades_interceptor.cc`). So "Chrome defaults to http" is precise for the
*fixer* layer; the whole-omnibox default depends on platform + the upgrade layer.
Explicit `http://` opts out of *ordinary* upgrades but not necessarily strict
HTTPS-First Mode.

**Decision (resolving the earlier contradiction): the `browser` profile's fixer
default is `http`, and there is NO `https_upgrade` flag on the parser profile.**
HTTPS-Upgrades / HTTPS-First are **navigation-layer** behavior (they depend on a
network round-trip and prior-visit/HSTS state), so they cannot be modeled
faithfully at parse time. An earlier draft both proposed a `browser`-profile
`https_upgrade` flag *and* declared HTTPS-Upgrades out of scope — the flag is
removed here. If https-upgrade *simulation* is ever wanted it is a **separate,
explicitly-named future navigation-simulation feature**, not a parser knob.

### Intranet single-word + "Did you mean"

An otherwise ambiguous single label → UNKNOWN → search now + background check +
"Did you mean http://word/" if it succeeds; `localhost` is an explicit URL
exception. TWO distinct mechanisms: (1) the per-candidate check
(`chrome_omnibox_navigation_observer.cc`) issues a separate `HEAD` and accepts
2xx/401/407 or a non-hijacked redirect — not DNS resolution and not "any
non-error response"; (2) the **Intranet Redirect Detector**
(`intranet_redirect_detector.cc`) issues three startup `HEAD`s to random
hostnames and treats ≥2 final origins under the same registrable domain/host
(`SameDomainOrHost`) as ISP interception. Separate mechanisms. **Both are
navigation-layer and out of scope for a parser** — noted for fidelity only.

### Additional Chromium behavior worth preserving (as facts, not features)

- **FTP was disabled in M88, fully removed in M95 (2021)** — a host merely
  *named* `ftp.example.com` is just an HTTP host. ("browser-like" ≠ "supports
  ftp.")
- **External-protocol dispatch is navigation policy, not parsing**: `mailto:`,
  `tel:`, custom, `magnet:` reach `ExternalProtocolHandler`. The fixer has
  distinct paths for `view-source`, `file`, `filesystem`, `about`/`chrome`,
  `devtools`; do not collapse `blob:`/`data:` into one class. A `browser` parser
  posture cares only that the selected parser can represent them.
- **`data:` payload whitespace preserved** — via `net/base/data_url.cc`
  (`DataURL::Parse`), landed around **M133 / early 2025** (earlier it kept
  whitespace only for `text/*`/`xml`); a `net/`-layer parse concern, distinct from
  the omnibox fixer's CR/LF/TAB removal. Illustrates `browser`-like ≠
  Chrome-faithful.
- **IDN display ≠ parsing**: Chrome parses IDN via UTS-46 but *displays*
  Unicode-vs-Punycode via `IDNSpoofChecker` (`url_formatter::FormatUrl`) — a
  presentation layer analogous to rurl's `host_encoding`, not parsing.
- **HSTS preload** forces https at navigation regardless of typed scheme —
  navigation-layer, not fix-up, not parsing.
- **Trailing-dot FQDN** (`example.com.`) preserved.
- Omnibox `SchemeClassifier` and URL-library application-specific scheme
  categorization are separately pluggable; only the low-level component splitter
  lacks scheme-selection policy — supports separating acceptance/classification
  from interpretation without claiming the whole parser core is closed.

---

## Scope / build order

- **Part 1 (bounded fixer)** blocks the `browser` profile (ADR 0012 Layer 6). It
  is the smaller, higher-priority slice.
- **Part 2 (classifier)** is optional and follows the fixer; it consumes the
  fixer and adds no repair behavior of its own.
- Both are **facts, not navigation**: none of the navigation-layer mechanisms
  (HTTPS-Upgrades, HSTS, Intranet Redirect Detector, ExternalProtocolHandler) are
  implemented — they are documented so the `browser` posture's limits are honest.

## Open questions

- Classifier verdict surface: a new helper vs an extension of
  `get_url_diagnostics`?
- How much of the ambiguous-IPv4 tie-breaker ladder is worth reproducing vs
  documenting as "browser-like, not Chrome-faithful"?
- Is a separate future https-upgrade *simulation* feature (navigation-layer,
  not a parser knob) ever worth building? (The parser profile does **not** carry
  an `https_upgrade` flag — see Part 3.)
