# ADR 0012: rurl becomes a general URL parser with web features layered on top

- **Status:** Accepted
- **Date:** 2026-07-10
- **Tracking:** RURL-pbbkzccp (additional-schemes scope), RURL-mpktqfto
  (presets-vs-knobs decision), RURL-vzgeurae / ADR 0010 (`scheme_policy`
  leniency axis, prerequisite). Supersedes the "keep the scheme set closed"
  standing rule of ADR 0004.
- **Depends on / relates to:** ADR 0002 (punycode helpers — untouched),
  ADR 0003 (parse/present Stage split), ADR 0006 (diagnostics as companion
  helpers only — hard constraint here), ADR 0007 (`url_standard` selector — the
  bundle-axis template), ADR 0009 (host-charset shim), ADR 0011 (`path_encoding`
  orthogonality).
- **Spun-out PRDs:** the email/userinfo diagnostic vocabulary
  ([`email-userinfo-diagnostics.md`](../prd/email-userinfo-diagnostics.md)) and
  the bounded browser fixer + URL-vs-search classifier, incl. the Chromium
  omnibox research
  ([`browser-fixer-and-url-search-classifier.md`](../prd/browser-fixer-and-url-search-classifier.md)).
  **Neither PRD blocks *accepting* this ADR** — they are follow-up slices. The
  fixer PRD *does* block **Layer 6** and exposing `profile="browser"` (D6); the
  email PRD blocks nothing here. This ADR owns only the decisions the PRDs depend
  on.


---

## TL;DR

rurl started life as a web-URL / domain-analysis tool. It has quietly grown the
machinery to be a *general* URL parser (a `whatwg` posture, a `scheme_policy`
acceptance axis, two hostless parse precedents already in-tree). This ADR makes
that ambition explicit and bounded:

1. **Core = a general, standard-grounded URL parser.** Under
   `scheme_acceptance="general"`, any *syntactically valid* scheme token is
   **admitted to parsing** (the parse itself may still fail on a malformed
   remainder — "admitted" ≠ "always succeeds"). Acceptance admits only the token;
   `url_standard` decides the interpretation, and `general` is invalid when
   `url_standard` is `NULL`. Under WHATWG the six special schemes get authority +
   host + default-port handling and **every other scheme is non-special** —
   parsed as one of *three* shapes (not two): an **opaque-path** URL (`mailto:x`,
   `foo:bar` — path is a string, no authority), a **non-special list-path without
   authority** (`foo:/bar` — list path, host absent), or a **non-special
   authority** URL (`foo://host`, `foo:///bar` empty-host, `foo://[::1]/bar`). No
   per-scheme allowlisting is needed for the general posture.
2. **Web/SEO features layer on top**, but are NOT safe merely because they are
   host-independent. SEO/web transforms (scheme, www, subdomain, host encoding,
   query sort/filter, case, index, trailing-slash) can corrupt opaque hosts,
   `mailto:` hfields, custom-scheme query semantics, and even non-special *list*
   paths — component semantics belong to the scheme (RFC 8820 §2), not to generic
   tooling. SEO transforms get a **separate HTTP(S)-only transform-eligibility
   policy** plus explicit profile opt-in; the default acceptance allowlist is not
   a transform allowlist. Only standards-required processing runs
   unconditionally. Opaque paths additionally receive no implicit
   hierarchical-path transform; ADR 0011's explicitly lossy presentation remains
   an opt-in escape hatch over the public rendering only.
3. **Named profiles** (RURL-mpktqfto) are the user-facing packaging: `browser`
   (the honest home for the historical http-prepending), `whatwg`, `rfc-syntax`,
   and `canonical`/`seo` (rurl's origin intent — currently nameless). Profiles
   are thin sugar over orthogonal knobs; explicit args win.
4. **Parse success is not validity.** Successful new branches receive an
   `ok`-family parse status, but are not thereby declared conformant; selected
   non-fatal WHATWG validation errors and scheme-specific RFC violations are
   reported as companion facts (ADR 0006), never new columns. Other WHATWG
   validation errors can accompany parse failure; diagnostics never turn a failed
   parse into a successful one.
5. **Build it foundation-first**, in layers that each ship value and preserve
   byte-for-byte backward compatibility for the CRAN-published default, and each
   pass an explicit default-byte-identity + conformance-parity gate.

The central insight (from studying Chromium — see the fixer PRD): **"browser/
omnibox" is not a peer of `whatwg`/`rfc`.** It is a *fix-up posture* (guess
intent, invent a scheme, repair the string) layered in *front* of a spec parser
that itself never guesses. rurl already has this exact seam —
`scheme_policy=infer` is the scheme-invention step; Layer 6 adds a bounded,
inspectable fixer before the WHATWG parser. Standards-required WHATWG
preprocessing remains parser behavior, not mislabeled fixer policy. This ADR
names the posture instead of leaving it as an un-named default leak.

---

## Context

### Where we are

- The scheme gate is a single point: `.SUPPORTED_SCHEMES`
  (`http/https/ftp/ftps/file`) plus a "scheme-bearing-but-not-allowlisted →
  `error`" rule. Everything else (`mailto:`, `data:`, `ws:`, …) hard-errors. The
  gate is literally `paste0(.SUPPORTED_SCHEMES, "://")` (`R/parse-phases.R`), so
  it also *requires* the `://`, which is why opaque schemes cannot pass today.
- The reject is **entangled with presentation**: it is only armed under
  `protocol_handling ∈ {keep, none}`. Scheme *acceptance* riding on
  `protocol_handling` is the same category error that `scheme_policy` (ADR 0010)
  fixed for scheme-*less* input, still present for scheme-*bearing* input.
- Two hostless parse precedents already ship: `file:` (dedicated WHATWG state
  machine; gets an OK-status **and** a `clean_url` exception) and the
  `rfc3986_path_rootless` slice (`http:example.com` — OK-status but NA
  clean_url). So "a URL with no authority" is not new to the pipeline.
- `url_standard` is already a *bundle* selector (ADR 0007): `whatwg` switches on
  control-char strip + backslash rewrite + forbidden-cp reject + UTS-46 dot
  mapping + numeric-host IPv4 model + host-charset shim. rurl's existing
  `rfc3986` selector is a **reg-name-permissive syntax posture** (keeps numeric
  hosts as reg-names, no WHATWG IDNA/IPv4 coercion) — *not* a full RFC 3986
  normalizer. That distinction is load-bearing for D1.

### What forced the decision

The cross-parser disagreement corpus (RURL-wncwfasl) found the closed scheme set
is **the single largest capability gap**: all 8 runnable false-rejects against
both the WHATWG and RFC oracles are opaque/arbitrary-scheme inputs
(`mailto:a@b.com`, `data:space?test#test`, `fs:/hello.eth`, `a:b#`,
`scheme:example.com`, `scheme:example.com/path`, `scheme://username@@@@example.com`,
`foo://///////bar.com/`).

**Corpus conclusion, corrected:** "accepted by both oracles" means **parser
acceptance, not standards conformance**. WHATWG *accepts*
`scheme://username@@@@example.com` while emitting `invalid-credentials`
validation errors and percent-escaping the extra `@`; RFC 3986 does **not** admit
a raw `@` inside `userinfo`/`reg-name`, so the string is not a valid RFC
authority even though a permissive parser yields components. Likewise WHATWG will
parse strings that violate a scheme's own RFC (a `data:` with no comma). So
closing this gap means rurl should *accept-and-diagnose*, not *accept-as-valid*.
This is the strongest remaining item in the paper's conformance story
(RURL-osrjtiwk): a divergence rurl chooses becomes a knob, and the validation
facts become reportable rather than hidden behind a hard reject.

ADR 0004's standing rule — *"keep the allowed-scheme set closed
(`http/https/ftp/ftps`); do not add `ws`/`wss`/`file`"* — is already **stale**:
`file` shipped (PR #134/#135). This ADR formally supersedes that rule.

---

## Decision

### D1 — rurl is a general URL parser; RFC and WHATWG postures are DISTINCT

Under general acceptance the parser admits any syntactically valid scheme token
(`^[A-Za-z][A-Za-z0-9+.-]*$`). WHATWG stores the scheme ASCII-lowercased; RFC
3986 treats lowercasing as *syntax-based normalization* (§6.2.2.1), so under the
`rfc-syntax` profile (which disclaims normalization) the **source spelling is
preserved** and the lowercased form is derived only where a caller asks for it
(scheme-class lookup, default-port matching). Under WHATWG, special-ness is a
lookup against its six special schemes; everything else is non-special. RFC 3986
has no corresponding classification. Thus **RFC 3986 and WHATWG are not one
interpretation core** — they differ materially, and a single "general" branch
cannot serve both faithfully:

| Concern | WHATWG | RFC 3986 |
| --- | --- | --- |
| scheme-specific payload (`mailto:x@y`) | **opaque path** (path is a *string*; record-level list-vs-opaque distinction) | `path-rootless`; no opaque/list distinction |
| non-special host | **opaque host** — preserve ASCII case, no IDNA, no IPv4 coercion, UTF-8 percent-encode non-ASCII; IPv6 + empty are additional forms | `reg-name`, `IPv4address`, **or** `IP-literal` (bracketed — includes IPv6 **and `IPvFuture`**; current syntax excludes zone identifiers, RFC 9844) |
| path normalization | `.`/`..` **and their percent-encoded forms** (`%2e`, `.%2e`, `%2e.`, `%2e%2e`, ASCII case-insensitive) are removed **during parsing** (path state) in every *list* path — a *parser* behavior; opaque paths are exempt | `remove_dot_segments` runs only in *reference resolution* (§5.2.4) and as syntax-based normalization (§6.2.2.3), NOT basic parsing |
| accepted raw chars | forbidden-host / forbidden-domain sets; validation errors | reg-name grammar (sub-delims); stricter about raw delimiters |

Note (precision): **only special-scheme *domain* hosts use the WHATWG domain
parser.** That algorithm percent-decodes the host and invokes UTS #46 with
WHATWG-defined strict/relaxed behavior and fixed parameters; a generic direct
UTS-46 call is not equivalent. Non-special authority hosts are *opaque hosts*
(ASCII case preserved, non-ASCII UTF-8 percent-encoded); `IPvFuture` is an
alternative *inside* the bracketed `IP-literal`, not part of `reg-name`.

**Decision:** implement **distinct RFC and WHATWG host/path semantics** where they
diverge (host model, opaque-body representation), sharing only the plumbing that
is genuinely posture-neutral (routing a no-authority input out of the authority
parser; carrying the body in the `path` column).

**Parsing vs normalization.** In RFC 3986, `remove_dot_segments` is *both*
reference resolution (§5.2.4) *and* syntax-based normalization (§6.2.2.3);
**scheme and host case lowercasing are together §6.2.2.1** (Case Normalization).
All of these are *normalization / resolution*, **not** basic parsing. (WHATWG
differs: it removes `.`/`..` — and the percent-encoded `%2e` forms,
case-insensitively — in the path state *while parsing* every list path.) A true
RFC *syntax* profile must **retain dot segments, host case, and scheme case
unless resolution/normalization is explicitly requested.** So the public profile
is named **`rfc-syntax`** and it does **not** force
`path_normalization = "dot_segments"`, unreserved-octet identity normalization,
or `case_handling = "lower_host"`. rurl's *existing* direct
`url_standard = "rfc3986"` bundle still forces those historical normalization
choices under ADR 0007; changing that direct call would be an unrelated
compatibility break. Instead, the named `rfc-syntax` profile is an authorized
profile-level exception that resolves to `path_normalization = "none"`,
`path_identity = "none"`, and `case_handling = "keep"`. Profile resolution runs
before the final options object is built, so these are intentional profile
defaults rather than caller-supplied conflicts. Explicit caller arguments still
win and may knowingly produce a custom, non-profile-conformant combination.

**Deterministic RFC-general failure boundary.** RFC 3986 supplies ABNF, not one
WHATWG-like parser algorithm, so "the standard's parser returned failure" is not
an executable RFC contract. For `url_standard = "rfc3986"` with
`scheme_acceptance = "general"` — including the `rfc-syntax` profile — rurl
therefore defines the following boundary:

- After recognizing the scheme and component delimiters, the ASCII portion of
  the input must match RFC 3986's generic `URI` grammar: every `%` is followed by
  two hex digits; authority/userinfo/host/port and bracketed IP-literals match
  their generic productions; and raw component delimiters cannot appear where
  only `pct-encoded` would admit them. A violation is a parse `error`.
- Directly-written non-ASCII scalar values are the one deliberate syntax
  extension described below. They are carried reversibly and diagnosed as
  `unicode-outside-rfc3986-uri`, not rejected. This exception does not relax the
  surrounding ASCII grammar.
- Scheme-specific restrictions are overlays, not generic parse gates. Thus a
  generically parseable `data:` without a comma or `mailto:` with a fragment is
  an `ok` parse plus a selected diagnostic. Conversely, repeated raw `@` inside
  an RFC authority is a generic-grammar failure even if a permissive component
  splitter can recover a host.

This boundary is the normative acceptance contract for the new RFC-general
branch; libcurl or another backend may assist with component extraction but may
neither widen nor narrow it accidentally — so it is an **independent** gate, not
a delegation to libcurl's permissiveness (see Layer 4a). Existing direct
`url_standard="rfc3986", scheme_acceptance="web"` behavior stays compatible;
`rfc-syntax` selects the new branch and removes the historical normalization
bundle as described above.

**Non-ASCII input posture.** Under `whatwg`, *domain* hosts go through the WHATWG
domain parser (UTS #46 is a dependency, not the public algorithm contract), and
non-ASCII in path/query/fragment is UTF-8 percent-encoded. Under `rfc-syntax`, a
strict URI reading would reject a directly-written non-ASCII *character* — RFC
3986 identifiers are drawn from a subset of US-ASCII, and non-ASCII *data* is
representable only as percent-encoded UTF-8 octets (RFC 3986 §2.5); it is
directly-written non-ASCII *characters* that are exclusively IRI territory (RFC
3987). rurl instead renders such input reversibly (ADR 0002) — but this is
**not RFC 3987 (IRI) conformance**: rurl does not implement IRI's allowed
repertoire, normalization, or IRI-to-URI mapping. Call it **"Unicode-tolerant,
RFC-shaped syntax,"** flagged as a fact, never advertised as RFC-3987-conformant.

### D2 — output contract frozen; richer internal state; SEO transforms are scheme-guarded

The 18-field `safe_parse_url` result does not grow. But richer internal-state
families are required (public `NA` mapping unchanged):

- The path model is posture-specific: WHATWG carries **`path_kind` ∈ {opaque,
  list}**; RFC carries **`rfc_path_form` ∈ {abempty, absolute, rootless, empty}**
  and never inherits WHATWG's opaque/list discriminator. Shared plumbing carries
  **`host_kind`/`authority_kind` ∈ {absent, empty, present}** (plus host *form*:
  domain / opaque / ipv4 / ipv6 / empty under WHATWG, **plus `reg-name` and
  `ipvfuture`** for the `rfc-syntax` posture — D1 and Layer 4 promise the RFC
  `reg-name` / bracketed `IP-literal` (`IPv6` / `IPvFuture`) cases, so the form
  enum must carry them rather than force-fit them into the WHATWG `opaque`/`ipv6`
  buckets). IPv6 zone identifiers are deliberately **not** another host form: RFC
  9844 obsoleted RFC 6874 and restored RFC 3986's original `IP-literal` grammar,
  while WHATWG also omits zone IDs. A single `opaque` boolean **cannot**
  round-trip the four WHATWG non-special shapes: `foo:bar` (opaque, host absent),
  `foo:/bar` (list, host absent, no authority), `foo:///bar` (list, host
  **empty**, authority present), `foo://[::1]/bar` (list, IPv6 host). (Empty host
  is a *non-special* / `file` privilege only — a **special** scheme with an empty
  host is a host-missing parse *failure*, so this four-shape enumeration is
  non-special-specific.) WHATWG serialization keys the `//` on the
  **null-vs-empty host** distinction and adds a `/.` guard for a null-host *list*
  path — precisely when the host is null, the path is **not** opaque, the path
  has **more than one segment**, and its **first segment is empty** (a single
  empty segment does not trigger it); RFC 3986 §5.3 likewise preserves
  undefined-vs-empty components. Both empty and absent hosts still surface as
  `NA` publicly — this is internal state only.

  > `host_kind` and `authority_kind` are both retained deliberately, not
  > redundantly: `authority_kind` records whether a `//` authority was present
  > (distinguishing `foo:/bar` from `foo:///bar`), while `host_kind` records
  > empty-vs-absent-vs-present *within* an authority. Neither derives the other
  > across all four shapes.
- Query and fragment each carry **presence state** in addition to their payload:
  **`query_kind` / `fragment_kind` ∈ {absent, empty, present}**. The public
  `query`/`fragment` fields continue mapping both absent and empty to `NA`, so
  the 18-field contract is unchanged, but the standard serializers retain a
  trailing `?` or `#` when the delimiter was present with an empty value.
  `clean_url` still excludes fragments by its longstanding product contract; when
  query presentation is enabled, an empty-but-present query can serialize as `?`.

Under WHATWG, the **opaque/list discriminator governs every Stage-B path
transform and the serializer**. Under RFC, `rfc_path_form` selects RFC-shaped
parsing/serialization and never triggers WHATWG opaque-path behavior. The guard
is record-wide rather than path-only:

| Operation | Eligible rows | Behavior elsewhere |
| --- | --- | --- |
| Standards-required parsing/serialization | Every row admitted by the selected posture | Run exactly the posture rule: WHATWG lowercases the scheme and domain hosts, preserves opaque-host ASCII case, removes dot segments only from list paths, and uses the component-specific encode sets; `rfc-syntax` preserves source case/dot segments and enforces D1's generic grammar. |
| DNS/PSL derivation (`domain`, `tld`, subdomain analysis) | A WHATWG `domain` host, or an RFC `reg-name` under a scheme explicitly declared DNS-host-eligible (the built-in web schemes) | Public DNS-derived fields are `NA`; an opaque host or arbitrary-scheme `reg-name` is not silently asserted to be a DNS name. A caller may request a separate `public-suffix-known` fact. |
| Automatic semantic transforms (scheme force, www/subdomain rewrite, case beyond the standard, index/trailing-slash, query filter/sort) | HTTP(S), plus an explicit `canonical`/`seo` opt-in; a future scheme may add its own policy | Skip the transform for that row. If the caller explicitly requested a globally applied but ineligible transform, report `transform-skipped-ineligible-scheme`; do not turn parse success into failure. |
| Host presentation (`host_encoding`) | WHATWG domain hosts or another explicitly IDNA-eligible DNS host | Keep the parsed host. Opaque hosts, IP literals, and arbitrary RFC reg-names never enter the IDNA/punycode helpers. |
| Hierarchical path transforms | HTTP(S) list/hierarchical paths under the semantic-transform gate | Never apply them to a WHATWG opaque path or to an arbitrary non-special list path. |
| Explicit `path_encoding=encode/decode` | Any successfully parsed row | Honor it only as ADR 0011's knowingly lossy **public presentation** escape hatch, after the internal identity record and diagnostics are fixed. It can make `path`/`clean_url` non-conformant; it never feeds parsing, scheme overlays, or the standard serializer used for identity comparisons. Profile defaults never select the lossy values for an opaque or non-HTTP path. |

This matrix explicitly amends how ADR 0007's `case_handling = "lower_host"`
bundle applies to the new host forms without changing existing direct-selector
results for the previously supported schemes: standard-required WHATWG scheme
and *domain-host* lowercasing still occurs, but a non-special opaque host is not
lowercased. The `rfc-syntax` named profile uses the D1 preservation overrides.
Generic `case_handling`, `www_handling`, subdomain trimming, and forced
`protocol_handling = "http"/"https"` are semantic transforms governed by the
table, not licenses to rewrite an arbitrary newly admitted scheme.

**Opaque paths receive no implicit rurl Stage-B semantic transforms.** This does
not mean byte-verbatim input: WHATWG's own preprocessing already stripped
tabs/newlines and outer C0/space, the opaque-path state special-cases spaces
before `?`/`#`, and the C0-control percent-encode set encodes all non-ASCII. rurl
applies that WHATWG set — never its hierarchical path encoder — and adds nothing
else unless the caller explicitly chooses ADR 0011's lossy presentation escape
hatch described in the matrix.

- `get_scheme()` must **return the literal scheme** for accepted opaque/
  non-special input (today it returns `NA` because the whole row errors). This is
  the pivotal cascade: `get_scheme_class()` then returns `special`/`non-special`
  correctly *for free* (vocabulary already exists).
- All new facts (validation errors D3/D5; scheme-specific facts; email facts —
  see the email PRD) surface only through companion helpers
  (`get_url_diagnostics`, `get_scheme_class`, `get_host_type`), never new columns.

This makes Layer 3 substantially larger than "reuse `path`": it is a guard
threaded through the entire Stage-B component pipeline (hence the 3a/3b/3c split).

### D3 — scheme acceptance is a governed axis; the allowlist is only the *default* posture's concern

A key consequence of D1: **general acceptance admits every syntactically valid
scheme token, without choosing its interpretation.** `url_standard=whatwg` then
classifies the six special schemes as special and every other scheme as
non-special; `url_standard=rfc3986` instead applies generic RFC component syntax.
The remainder can still make either parser fail. So there is no "add
`ws`/`wss`/`sftp` to make its scheme token admissible" under general acceptance.
The default allowlist decides only the **default posture's** curated set.

Decouple scheme *acceptance* from `protocol_handling`. The public argument is
**`scheme_acceptance = c("web", "general")`**. It is resolved before parsing and
enters the Stage-A cache key (like `scheme_policy`/`url_standard`):

- **`web`** (default; the "web-schemes-only" posture) — the curated allowlist
  (`http/https/ftp/ftps/file`). Scheme-bearing input outside it → `error`
  (today's behavior, minus the `protocol_handling` entanglement). Default-port
  metadata is interpretation/normalization data, not part of this acceptance set.
  `ws`/`wss` are not admitted here (D4/L1).
- **`general`** — admit any syntactically valid scheme token. This setting does
  not choose special/non-special, path kind, host model, encoding, or
  serialization; `url_standard` does. Admission does not guarantee that parsing
  the remainder succeeds.

**Composition rule:** `scheme_acceptance = "general"` requires an explicit
non-NULL `url_standard`. A direct call with `general` and `url_standard = NULL`
is an option-validation error before any row is parsed; rurl does not silently
choose RFC, WHATWG, or legacy/libcurl semantics. Named profiles may supply both
values. Explicit arguments override profile defaults, then the resolved
combination is validated, so explicitly removing the profile's standard while
retaining `general` also errors. `scheme_acceptance = "web"` remains freely
composable with `url_standard = NULL`, `rfc3986`, or `whatwg`.

**Fatal-vs-reportable boundary.** Once a scheme token is admitted, a row becomes
an `error` only when WHATWG parsing returns failure or when D1's explicit
RFC-general generic-grammar contract fails. Examples: WHATWG host-missing for a
special scheme, forbidden-host code points, invalid IP literals, malformed opaque
hosts, RFC malformed percent triplets or authority grammar. If the generic/WHATWG
parse succeeds but a scheme-specific RFC is violated — a `data:` with no comma, a
`mailto:` with a fragment — the row receives an `ok`-family status and the
violation is a companion diagnostic (D5), never a parse failure.

**The four separately owned axes** (extends ADR 0010's three):

| Axis | Question | Values |
| --- | --- | --- |
| `scheme_acceptance` — *new* | Which scheme tokens may enter parsing? | `web` / `general` |
| `scheme_policy` (ADR 0010) | Is scheme-*less* input accepted? | `infer` / `require` |
| `url_standard` (ADR 0007) | Which standard interprets it? | `rfc3986` / `whatwg` / none |
| `protocol_handling` | How is the scheme presented in `clean_url`? | `keep`/`none`/`strip`/`http`/`https` |

**"Independent" means separate questions and arguments, not that every Cartesian
combination is valid.** Acceptance and `url_standard` remain distinct caller
choices, but `general + NULL` is rejected by the composition rule above. The
`general` value also remains unavailable until Layers 3–4 can shape opaque paths,
non-special authority, and RFC reg-name/`IPvFuture`. Read the table as
"orthogonal policy ownership with explicit cross-axis validation," not four
unconstrained dimensions.

### D4 — default stays the web/SEO intent; generalization is gated

**The general parser is opt-in, not on-by-default.** `error → ok` is a widening,
but it still *changes output* for the affected inputs, and a crawl pipeline
should not suddenly emit `mailto:` rows. So:

- The **default** (`url_standard = NULL`, `scheme_policy = "infer"`,
  `scheme_acceptance = "web"`) stays byte-for-byte compatible (hard CRAN
  constraint).
- General acceptance is selected by the `rfc-syntax`, `whatwg`, and `browser`
  profiles, or directly through `scheme_acceptance = "general"` together with an
  explicit standard; there is no separate `general` profile in the candidate set.
- **`ws`/`wss` do NOT join the default acceptance allowlist.** An earlier draft
  added them eagerly to the *default* set while also promising byte-for-byte
  compatibility — a direct contradiction: `ws:…`/`wss:…` inputs that today emit
  `error` would silently become `ok` rows for existing default-posture callers.
  Given the hard CRAN backward-compat constraint, they stay **general-only**.
  With general acceptance plus WHATWG interpretation they parse as special URLs
  with ports 80/443 and the full host model, and require no default-allowlist
  widening. rurl still *knows* their special-ness and default ports
  (`.WHATWG_SPECIAL_SCHEMES` / `.SCHEME_DEFAULT_PORTS`); what it does **not** do
  is admit them under the default `scheme_acceptance = "web"` set. Promoting them
  to the default is a separate, deliberately versioned widening if ever wanted.

**Default-posture continuity note.** The historical http-prepending stays in the
default posture for byte-compat, *and* the `browser` profile is its "honest
home." To avoid maintaining two prepend implementations that can drift, Layer 6
must express the default's prepend and the `browser` profile's fix-up through the
**same** scheme-inference seam (`scheme_policy=infer`), not a parallel code path.
See the fixer PRD.

### D5 — parse success ≠ validity; SELECTED diagnostics, not a conformance oracle

New successful branches deliberately receive an `ok`-family parse status; this
means only that structural parsing succeeded, not that the URL conforms to every
scheme specification. The diagnostics below are explicitly **selected facts, not
complete validators** — and the ADR guarantees that **absence of a diagnostic
does NOT imply conformance**. Full per-standard conformance validation is out of
scope; if it is ever wanted it is a separate, named validator.

Report selected companion facts (WHATWG error names used verbatim):

- **WHATWG validation errors** — malformed `%` and non-URL code points are *both*
  the single `invalid-URL-unit` error (not separate names). `invalid-credentials`
  is raised for **any** credentials in a URL, **not** specifically the
  multiple-`@` case. These two are non-fatal in the selected parser contexts and
  are reportable. Other WHATWG validation errors can accompany paths that
  explicitly return failure. *("valid-with-validation-errors" is NOT a WHATWG
  term — see A.1.)*
- **RFC-general extension fact** — `unicode-outside-rfc3986-uri` fires when the
  sole D1 tolerance is used. Any other RFC generic-grammar violation remains a
  fatal parse error, so absence of this fact says nothing about scheme-specific
  conformance.
- **Scheme-specific facts** (parseable ≠ valid-for-the-scheme):
  - `ws`/`wss`: fragment present, or userinfo present (RFC 6455 forbids both).
  - `mailto`: fragment present (RFC 6068 §2: fragments **SHOULD NOT** be used).
  - `tel`: local number missing the required `;phone-context=` (RFC 3966).
  - `data`: missing the mandatory comma (RFC 2397).
  - `file` under `rfc-syntax`: non-absolute path; userinfo, port, query, or
    fragment present (outside RFC 8089's normative grammar). Under WHATWG,
    `file://localhost/...` maps `localhost` to the empty host.
  - Illustrative, not exhaustive: e.g. `http:example.com` is valid *generic* RFC
    3986 syntax but **not a valid HTTP URI** (RFC 9110 §4.2.1 requires `http://`
    + non-empty authority) — a case the selected diagnostics do not currently
    flag, underscoring that they are not a conformance oracle.

rurl follows the interpreting standard at the *parse* layer and never rejects on
a scheme-specific fact — it reports it. The de-facto schemes (`sftp`, `ftps`)
have **no URI-scheme RFC** (sftp = expired draft but IANA-*provisionally-
registered*; ftps = RFC 4217 is protocol-only and ftps is *absent* from the IANA
registry; ports 990/21 ambiguous). Any default port for them is convention,
flagged as such, never presented as standards-backed.

**Exact `parse_status` contract for new rows.** Status reports the parser/
domain-analysis outcome, never full conformance:

- A successfully parsed opaque or no-authority URL is `ok`.
- A successfully parsed non-special authority URL with an empty, opaque, IP, or
  arbitrary RFC reg-name host is `ok`; absence of a PSL result is not a warning
  because D2 does not presume such a host is DNS-backed.
- Existing `warning-no-tld`, `warning-invalid-tld`, and `warning-public-suffix`
  statuses apply only to D2's DNS/PSL-eligible rows. Existing `warning-userinfo`,
  `ok-ftp`, and `ok-scheme-relative` behavior keeps its documented scope.
- WHATWG validation errors that continue parsing, Unicode-tolerant RFC extension
  facts, scheme-overlay violations, and skipped ineligible transforms do not
  change `parse_status`; they live in `get_url_diagnostics()`.
- A generic/posture parse failure is `error` regardless of diagnostics. A
  diagnostic can never turn a failed parse into success.

### D6 — named profiles as inspectable sugar (RURL-mpktqfto: accept, layered)

Add a `profile` argument, **separate** from `url_standard` (merging them is the
`"browser"+"rfc"` category error `mpktqfto` reframe #2 warns about). A profile
resolves to a full `opts` bundle (acceptance + interpretation + leniency +
canonicalization). Iron rules: **explicit args override the profile**; orthogonal
knobs remain the escape hatch; profiles are **inspectable** (`url_profile("browser")`
returns the expanded knob list). Candidate set (≤4, names to confirm):

- **`browser`** — honest home for the historical http-prepending.
  `url_standard="whatwg"` + `scheme_acceptance="general"` +
  `scheme_policy="infer"` + `scheme_relative_handling="http"` + the bounded fixer
  (see fixer PRD) + fix-up default scheme `http` (NOT https) + minimal
  canonicalization. No `https_upgrade` parser knob — HTTPS-Upgrades is
  navigation-layer and out of scope (fixer PRD Part 3). Described as
  **browser-*like*, not Chrome-faithful**.
- **`whatwg`** — absolute-URL, no-base spec posture: `url_standard="whatwg"` +
  `scheme_acceptance="general"` + `scheme_policy="require"` +
  `scheme_relative_handling="error"`. A bare `//host` cannot parse without a
  supplied base, and this profile does not invent one.
  > **Foot-gun (intended):** `profile="whatwg"` sets `scheme_policy="require"`,
  > whereas a direct `url_standard="whatwg"` call inherits the default `infer`.
  > So `profile="whatwg"` **rejects** scheme-less `example.com` that the direct
  > selector accepts. This is deliberate faithfulness to "absolute-URL, no-base,"
  > but two things sharing the name "whatwg" diverge on acceptance — document it
  > loudly, and let the `customized` inspector surface the resolved knobs.
- **`rfc-syntax`** — Unicode-tolerant RFC-shaped generic syntax (D1):
  `url_standard="rfc3986"` + `scheme_acceptance="general"` +
  `scheme_policy="require"` + `scheme_relative_handling="keep"` +
  `path_identity="none"` + `path_normalization="none"` + `case_handling="keep"`;
  disclaims RFC normalization and IRI conformance.
- **`canonical`** / **`seo`** — rurl's ORIGIN intent, finally named: default
  `scheme_acceptance="web"`, but built-in semantic transforms are HTTP(S)-only;
  for eligible HTTP(S) inputs, strip www, force https, apply trailing-slash and
  index policies, and strip tracking parameters. `file` and FTP-family inputs may
  remain admissible for compatibility but receive no SEO transforms.
  > **Pin before graduation (Open Q1):** one bundle with an alias (both
  > reviewers agree), keeping the "≤4 profiles" claim true — not a distinct fifth
  > bundle. Still open: which name is primary. Claude leans `canonical` primary
  > (consistency with `clean_url`); Codex leans `seo` primary (`canonical` can
  > overpromise, since these transforms are policyful and need not preserve
  > semantic equivalence).

**Resolution and compatibility.** The implementation records which arguments were
explicitly supplied, expands the profile only into missing slots, then validates
the final combination (including D3's `general + NULL` prohibition).
Profile-supplied normalization exceptions such as `rfc-syntax` are authorized
defaults, not conflicts with ADR 0007. Direct calls using only
`url_standard="rfc3986"` or `"whatwg"` retain ADR 0007's existing bundle and
conflict behavior. If an explicit argument overrides a profile, the inspector
labels the resolved result `customized = TRUE`; rurl does not continue claiming
that the result exactly matches the named profile.

### D7 — mailto recipient parts are exposed through the standard accessors

An email address contains web-y structure: the `addr-spec` right-hand side is a
domain, and the left is the user. Rather than mint `mailto`/email-specific
extraction functions, the **existing** accessors are adapted: under
`scheme_acceptance = "general"`, a `mailto:` URL's first recipient decomposes so
that `get_host` / `get_domain` / `get_tld` / `get_subdomain` / `get_user` /
`get_userinfo` return the recipient's parts, decomposed by the **same PSL seam a
web host uses** (an email domain and an `http` host take identical branches).
This unifies with the pre-existing scheme-less `user@host` `warning-userinfo`
path, which already resolves host/user through these accessors — turning that
former convenience hack into a principled, spec-grounded feature.

Rules:

- **Extraction metadata only.** The recipient domain is populated as the
  internal `host` in Stage A; Stage B still serializes general rows from its own
  `gen_b` re-parse (host `NA` for `mailto`), so `clean_url` / round-trip is
  byte-for-byte unchanged. `get_host`(mailto) and `clean_url`(mailto) are
  independent.
- **Deliberate carve-out of D2.** D2 masks a general/opaque host out of the PSL
  decomposition because it is not asserted to be a DNS name. A `mailto` recipient
  domain *is* a domain, so it is exempted from that mask and does flow through
  pslr — the one place a general-routed host reaches the PSL seam.
- **Domain-form RHS only.** An `address-literal` or invalid RHS yields `NA` host
  (it is not a domain to decompose); the local-part still resolves as `user`.
- **First recipient.** The 1-URL→1-value accessors extract the *first*
  recipient; the full per-recipient breakdown (and the email/SMTP *facts*) is
  `get_mailto_recipients()`'s job (email PRD). "First" is a convenience
  selection, not a spec claim.
- **Gated to general acceptance.** Under the default `web` acceptance `mailto`
  does not parse, so every default-mode result is unchanged.

Authority accessors are **not** taught to reach into recipients for their own
sake — a `mailto` has no authority host and may carry N recipients; the domain
is a property of a recipient, surfaced through the same accessor only because it
is the same *kind* of value.

---

## Layered build plan (foundation first)

Each layer ships independently, keeps the default green, and is a clean fp slice.
Do **not** start the profile layer before the parser layers.

**Every layer passes the same two gates before merge:**
1. **Default byte-identity** — a frozen golden snapshot of `safe_parse_url` (+
   accessors) over a representative corpus is byte-identical for the default
   posture (`url_standard=NULL`, `scheme_policy="infer"`,
   `scheme_acceptance="web"`).
2. **Conformance parity — no per-case regression.** The WHATWG-WPT / RFC-3986
   parity harness (`inst/bench/standard-parity.R`, RURL-lyhcyvsa) is a
   **comparison ledger**, not a correctness oracle: it records what rurl does
   relative to each reference. The gate is **per-case**, not aggregate — **no
   previously passing case may regress** without an explicitly approved ledger
   change (a rising aggregate count must not be allowed to mask a swapped
   case). The *normative* oracle for the new RFC-general branch is the
   hand-authored **RFC 3986 ABNF fixture set** in Layer 4a, not the parity
   ledger — because a permissive RFC component splitter can "accept" strings D1
   deliberately rejects (e.g. repeated raw `@`), so parity agreement is not proof
   of grammar conformance.

**Layer 0 — Foundation / scope (this ADR + housekeeping).**
- Accept this ADR; supersede ADR 0004's closed-set standing rule.
- Centralize the **16** `identical(url_standard, "whatwg")` toggles (verified:
  1 in `R/parse.R`, 15 in `R/parse-phases.R`) into an internal standard-posture
  record (distinct from D6's public named profiles). Pure refactor, but larger
  than first scoped; it de-risks every later layer.

**Layer 1 — register ws/wss special-scheme metadata (mechanical, inert).**
- Add to `.WHATWG_SPECIAL_SCHEMES`, `.SPECIAL_AUTHORITY_SCHEMES`,
  `.SCHEME_DEFAULT_PORTS` (80/443). **Do NOT add them to `.SUPPORTED_SCHEMES`**
  (the default acceptance allowlist): keeping the default byte-for-byte
  compatible is the hard constraint (D4). Until the acceptance axis lands (L2)
  this metadata is inert. **Add a test asserting inertness**: a `ws:` input under
  default `web` acceptance still hard-errors at the prefix gate before any
  special-scheme lookup. Conformance fixtures exercise ws/wss only under general
  acceptance plus WHATWG interpretation.

**Layer 2 — the acceptance axis (D3).**
- Lift scheme acceptance out of `protocol_handling`; governed axis in the Stage-A
  cache key; public `scheme_acceptance = c("web", "general")`; validate that
  `general` has a non-NULL `url_standard` after profile expansion and explicit
  override resolution.
- **Build-order gate:** `general` admits scheme *tokens* that Layers 3–4 must be
  able to *shape*. Exposing `general` before those parsers land would admit
  inputs the pipeline cannot yet represent. So either (a) ship L2's plumbing but
  keep `general` **unavailable to callers** until L3–L4 merge, or (b) land L2–L4
  as one activation. Do **not** expose `general` on L2 alone.

**Layer 3 — posture-specific path state + Stage-B guards.** *(Decomposed — this
was the largest single item in the full draft; split so each sub-slice honors the
byte-identity gate atomically.)*

- **3a — State model + round-trip.** Add WHATWG `path_kind` {opaque,list}, RFC
  `rfc_path_form` {abempty,absolute,rootless,empty}, shared `host_kind`/
  `authority_kind` {absent,empty,present}, `query_kind`/`fragment_kind`
  {absent,empty,present}, and posture-specific host form. Prove the four WHATWG
  non-special shapes and empty query/fragment delimiters round-trip without
  imposing that model on RFC parsing (D2). Public `NA` mapping unchanged.
- **3b — Serializers.** WHATWG null-vs-empty-host serializer with the four-
  condition `/.` list-path guard; distinct RFC generic serializer. Routing
  selected by `url_standard`: WHATWG uses `prep` masks,
  `.parse_opaque_urls_vec()`, Stage-A fields, and parse-status + clean_url
  whitelist entries; RFC uses the `rfc3986_path_rootless` path. Serialize opaque
  paths with the WHATWG C0-control set **only under WHATWG**.
- **3c — The Stage-B eligibility matrix (the real work).** Implement D2's
  complete component matrix as a guarded pass threaded through the Stage-B
  pipeline: path-kind guard, HTTP(S)+profile semantic-transform eligibility,
  host-form casing/IDNA eligibility, DNS/PSL eligibility, and per-row
  skipped-transform facts. ADR 0011's explicit lossy `path_encoding` presentation
  remains an escape hatch but never mutates the internal identity record.
  `get_scheme()` returns literal scheme (D2 cascade). *(Note: ~25 `://`
  occurrences in `parse-phases.R` bake in the `scheme://` assumption — audit each
  before treating the count as fixed.)*

**Layer 4 — authority/host semantics per posture + file overlay.**
- **4a — The independent RFC generic-grammar gate (own slice).** Implement D1's
  RFC-3986 generic `URI` ABNF gate **independently of libcurl's permissiveness**
  (percent-triplet validity, `reg-name` sub-delims, bracketed `IP-literal`, raw-
  delimiter rejection incl. the repeated-`@` case). Ship with a dedicated fixture
  corpus (the RURL-wncwfasl false-rejects plus adversarial percent/authority
  cases). **This hand-authored ABNF fixture set is the normative oracle** for the
  branch; the parity ledger is only a comparison view — a reference component
  splitter may "accept" strings D1 rejects, so parity agreement is not proof of
  grammar conformance. This is easy to get subtly wrong and is deliberately
  budgeted as its own slice.
- **4b — Host parsers per posture.** WHATWG opaque-host parser (ASCII case
  preserved, no IDNA/IPv4; reject forbidden-host cps; UTF-8 percent-encode
  controls + non-ASCII). Distinct `rfc-syntax` host path (`reg-name` /
  `IPv4address` / bracketed `IP-literal` incl. IPv6 + `IPvFuture`). Zone
  identifiers are not URI host forms under current RFC 3986/RFC 9844 or WHATWG.
  Only special-scheme domain hosts use the WHATWG domain parser; implement that
  algorithm rather than a generic direct UTS-46 call. Do NOT route non-special
  hosts through the punycode/domain helpers (ADR 0002). Populate public
  domain/TLD fields only for D2's DNS-eligible hosts. Add an RFC 8089 overlay for
  `file:` under `rfc-syntax`; preserve WHATWG's `file://localhost/` → empty-host
  mapping.

**Layer 5 — SELECTED diagnostics (D5) — never a conformance oracle.**
- WHATWG `invalid-URL-unit` + `invalid-credentials` facts + per-scheme facts
  (ws/wss fragment+userinfo, mailto fragment, tel phone-context, data comma,
  RFC-8089 file shape) via `get_url_diagnostics`, plus `unicode-outside-rfc3986-uri`
  and `transform-skipped-ineligible-scheme`. Documented as selected, not
  complete: **absence of a diagnostic ≠ conformance**. Own slice so acceptance is
  never conflated with validity.

**Layer 6 — bounded browser fixer + profiles (RURL-mpktqfto).**
- Implement the non-classifying bounded fixer required by the `browser` bundle
  *before* exposing that profile — **specified in the fixer PRD**
  (`browser-fixer-and-url-search-classifier.md`). Per D4, the fixer must
  share the `scheme_policy=infer` seam that the default posture already uses, so
  http-prepend lives in one place.
- Add the `profile` arg, missing-vs-explicit resolution, final-combination
  validation, `url_profile()` inspector (`customized` flag included), and the ≤4
  bundles. The `browser` profile is not available until its bounded fixer is.

**Layer 7 (optional) — URL-vs-search verdict.** Spun out to the fixer PRD;
non-blocking, a classifier adjacent to parsing.

---

## Consequences

- **Closes the corpus's single largest gap** — the 8 runnable false-rejects
  become honest accept-and-diagnose rows under a profile; strong paper property.
- **Default unchanged, CRAN-safe** — generalization gated (D4), enforced by the
  per-layer byte-identity gate.
- **Punycode/domain helpers untouched** (ADR 0002) — opaque-host and `rfc-syntax`
  reg-name paths are separate and never route through IDNA.
- **Output contract stable** (ADR 0006) — no new columns; opaque path value in
  `path`; empty query/fragment delimiters retained internally; all facts in
  companion helpers.
- **Acceptance ≠ validity is preserved** — selected non-fatal WHATWG validation
  errors and scheme-specific RFC violations are reported, while fatal parser
  errors remain failures rather than being hidden behind an `OK` row.
- **The http-prepending is finally explained** — a browser fix-up posture, now
  named `browser` and opt-out-able, sharing one prepend seam with the default.
- **New surface to maintain** — an opaque branch with a component-wide guard, an
  independent RFC grammar gate, two posture-specific host parsers, a
  validation-diagnostics layer, a `profile` axis + inspector, and (optionally, per
  the PRDs) a URL-vs-search classifier and email facts.

**Standing rules this creates.**
1. Supersedes ADR 0004's closed-scheme rule. New rule: scheme acceptance is a
   governed `scheme_acceptance` axis (D3); the **default** `web` set stays
   web-focused (D4); `general` requires an explicit `url_standard`, admits any
   syntactically valid scheme token, and lets that posture validate the
   remainder. "Do not add `sftp`" means only "do not add it to the default
   acceptance allowlist or claim a standards-backed default port." `ftps` is the
   one exception, grandfathered into the default allowlist for CRAN
   byte-compatibility (it predates this epic) but with no standards-backed
   default port — see Open Q2 (RURL-lvrfrkfa).
2. `browser`/`omnibox` is a fix-up posture, never a `url_standard` value —
   acceptance, leniency, interpretation, and presentation remain separate policy
   axes with D3's explicit cross-axis validation.
3. Parse success is not validity, and the diagnostics are **selected, not
   complete**: absence of a diagnostic never implies conformance (D5).
4. Opaque paths receive no **implicit semantic** Stage-B transform, and
   **acceptance and SEO-transform eligibility are separate policies** — built-in
   semantic transforms run only for HTTP(S), under explicit profile opt-in
   (component semantics belong to the scheme, RFC 8820 §2). ADR 0011's explicit
   lossy path-presentation values remain a documented escape hatch that does not
   mutate the internal identity record (D2).
5. Internal state carries WHATWG `path_kind`, RFC `rfc_path_form`, and shared
   `host_kind`/`authority_kind` plus `query_kind`/`fragment_kind` (null-vs-empty),
   so all four WHATWG non-special shapes and empty delimiters round-trip; the
   public surface still maps empty/absent values to `NA` (D2).
6. `rfc-syntax` is parsing, not normalization: dot-segment removal and host
   lowercasing are opt-in, Unicode tolerance is NOT advertised as RFC 3987, and
   D1's explicit generic-grammar gate — not backend permissiveness — sets its
   fatal boundary (D1).
7. Email/userinfo facts (governed by the email PRD) stay structural-only,
   companion-helper only (ADR 0006), with each fact naming its grammar.

**Revisit if:** a future interpretation can shape arbitrary schemes without a
non-NULL `url_standard`, or a new scheme earns DNS/SEO-transform eligibility.
Either change requires an explicit profile→knob or scheme-policy mapping, not a
silent widening of `scheme_acceptance` or the transform set.

---

## Appendix A — Research findings (preserve)

Grounding for the decisions above. The Chromium omnibox research (former A.3)
moved to the fixer PRD. Standards claims here were verified against primary
sources (WHATWG URL Standard, RFC 3986 family, per-scheme RFCs) in the 2026-07-10
alignment session.

### A.1 WHATWG URL Standard (url.spec.whatwg.org)

- **Special schemes + default ports** (`#special-scheme`): `ftp`→21,
  `file`→**null**, `http`→80, `https`→443, **`ws`→80**, **`wss`→443**. "special"
  is one boolean. Every other scheme is non-special.
- **Opaque-path trigger** (`#scheme-state`, `#opaque-path-state`): after the `:`,
  a URL goes opaque iff the scheme is **non-special AND remaining input does not
  start with `/`**. `foo:bar` → opaque; `foo:/bar` → path-or-authority (host
  null, list path); `foo://bar/baz` → non-special authority (opaque host `bar`,
  list path `["baz"]`).
- **Opaque-path URL fields**: `username`/`password` = **empty string `""`**
  (never null); `host` = **null**; `port` = **null**; `path` = the opaque
  **string**; `query`/`fragment` still parse. (rurl may map absence to `NA`, but
  that is rurl's mapping, stated separately — not the WHATWG model.)
- **Serializer** (`#concept-url-serializer`): for **opaque paths**, host is null →
  no `//`. WHATWG inserts a `/.` guard for a null-host **list** path under **all
  four** conditions together: host is null, the path is **not** opaque, the path
  size is **greater than 1**, and `path[0]` is the empty string. (A single empty
  segment does not trigger it.)
- **Query percent-encode sets differ by special-ness**: a non-special URL's query
  uses the **query percent-encode set**; a **special** URL's query uses the
  **special-query set** — the query set **plus U+0027 (`'`)**. A general
  serializer must pick the set by special-ness. (Path, userinfo, fragment, and
  C0-control sets are likewise distinct.)
- **Host parsing, special vs non-special** (`#concept-host-parser`): special
  domain hosts run the **WHATWG domain parser** (percent-decode, WHATWG's
  strict/relaxed UTS #46 and fixed parameters), then the IPv4 parser, and reject
  *forbidden-domain* cps — not equivalent to a generic UTS-46 helper call.
  Non-special → **opaque-host parser**: reject only the 17 *forbidden-host* cps,
  then **UTF-8 percent-encode with the C0-control set**; malformed `%` and
  non-URL code points raise validation errors **without necessarily causing
  failure**; no IDNA, no IPv4 coercion; IPv6 literals still parsed.
- Forbidden-host set (17): NUL TAB LF CR SP `# / : < > ? @ [ \ ] ^ |`.
  Forbidden-*domain* = that plus all C0 controls, `%`, DEL.
- **Validation error does not by itself determine parser success** (`#writing`).
  ("valid-with-validation-errors" is NOT WHATWG terminology.) `invalid-URL-unit`
  is the **single** error type covering both a malformed `%` and any non-URL code
  point. `invalid-credentials` fires for **any** credentials, not specifically
  the multiple-`@` case.
- **ws/wss** carry no parser-level special rules beyond special-scheme
  membership. The parser does **not** enforce RFC 6455's no-fragment/no-userinfo.
- **Scheme grammar** (`#url-scheme-string`): `^[A-Za-z][A-Za-z0-9+.-]*$`, stored
  ASCII-lowercased.

### A.2 Per-scheme RFC grounding

- **RFC 3986 (STD 66)** — generic URI. Authority present **iff** hier-part starts
  `//`. Host is `reg-name` / `IPv4address` / `IP-literal` — where `IP-literal` is
  the **bracketed** `"[" (IPv6address / IPvFuture) "]"` (`IPvFuture` is INSIDE
  `IP-literal`, NOT part of `reg-name`). `reg-name = *(unreserved / pct-encoded /
  sub-delims)`, `sub-delims = !$&'()*+,;=`; reg-name case is normalized
  case-insensitively (§6.2.2.1 — a *normalization* behavior, not basic parsing).
  RFC 9844 obsoleted RFC 6874, so current `IP-literal` does **not** include an
  IPv6 zone identifier. Path can exist without authority (`path-absolute`,
  `path-rootless`) — the `mailto:` positional `to` is `path-rootless` here, *not*
  a WHATWG opaque path. No special-scheme concept and no universal default-port
  table — **but §3.2.3 explicitly allows a scheme to define a default port and
  recommends eliding a matching default** (so default-port normalization is NOT
  exclusively a WHATWG posture). ASCII-only (non-ASCII → IRI, RFC 3987).
- **RFC 8089 (file)** — narrower than generic RFC 3986: absolute path, optionally
  under `//` authority; normative authority is empty, `localhost`, or `host`, no
  userinfo/port, no query/fragment. WHATWG has a distinct state machine and maps
  `file://localhost/...` to an empty host.
- **Special-use names are resolution semantics, not URI syntax gates** —
  `.localhost`, `.invalid`, `.test`, `.example`, `.local` remain parseable (RFC
  6761/6762). Separate from PSL registrability and omnibox heuristics.
- **RFC 6455 §3 (ws/wss)** — authority-based; ports 80/443. Grammar **omits
  userinfo**; **fragment MUST NOT be used**. Protocol-layer narrowing; the URL
  parser does not enforce it.
- **RFC 6068 (mailto)** — WHATWG **opaque-path** / RFC 3986 **path-rootless**.
  `mailto:[to][hfields]`, `to` = comma-separated `addr-spec` list, `hfields` =
  `?name=value&...`. Percent-encoding is **component-specific** (RFC 6068 EID
  7919 deleted the old "`&`,`;`,`=` must be percent-encoded" sentence). Fragments
  **SHOULD NOT** be used. Detailed recipient/email grammar → email PRD.
- **RFC 3966 (tel)** — WHATWG opaque-path / RFC path-rootless, no port.
  `global-number` (leading `+`) vs `local-number` (MUST carry `;phone-context=`).
- **RFC 2397 (data)** — WHATWG opaque-path / RFC path-rootless.
  `data:[mediatype][;base64],data`; **comma mandatory**.
- **sftp** — no RFC (expired draft), IANA-provisionally-registered; de-facto port
  22. **ftps** — no URI-scheme RFC and absent from the IANA registry; RFC 4217 is
  protocol-only. rurl correctly assigns no default port to ftps.

### A.3 rurl codebase readiness

- **Single gate**, `protocol_handling`-entangled reject (see Context); it is
  `paste0(.SUPPORTED_SCHEMES, "://")` so it also requires `://`. Two hostless
  precedents (`file:` state machine + clean_url exception; `rfc3986_path_rootless`
  OK-status, NA clean_url).
- **Opaque branch cost = moderate for routing, but the component-wide Stage-B
  guard is the real work**: the `scheme://` assumption is baked into ~25 `://`
  occurrences in `parse-phases.R`; audit each. Every Stage-B path transform must
  learn the opaque discriminator, while host, DNS, scheme-presentation, and query
  transforms need D2's eligibility matrix.
- **`url_standard` = the bundle template**: declarative profile table
  (`.URL_STANDARD_PROFILES`) → validator → conflict-checker (+ `...`-seam variant
  for `canonical_join`) → expand into `opts` → single Stage-A cache-key entry.
  Its WHATWG behaviors are **16** scattered `identical(url_standard,"whatwg")`
  checks (1 in `parse.R`, 15 in `parse-phases.R`); centralize (Layer 0) first.
- **Stage-A cache key** contains `urls`, `protocol_handling`, `www_handling`,
  `tld_source`, `scheme_relative_handling`, `url_standard`, `scheme_policy`,
  `scheme_acceptance`, and `fixup_posture`. Key the **resolved Stage-A-affecting
  values**, not the public profile name: two profiles that expand identically
  share an entry, while the Layer 6 browser fixer must not collide with an
  unfixed parse.
- **Accessors degrade correctly for hostless input already**: PSL accessors
  return `NA`/`character(0)` for NA host. D2 must extend that eligibility guard to
  hostful opaque hosts and arbitrary-scheme RFC reg-names. `get_scheme_class`
  already speaks `special`/`non-special`/`missing-or-error` but is starved by
  `get_scheme` returning `NA`; fixing `get_scheme` (D2) feeds it.
- **Web features survive *for eligible HTTP(S) schemes only***: being
  host-independent is NOT sufficient — query sort/filter can rewrite `mailto:`
  hfields, and case/index/trailing can corrupt a non-special *list* path. The
  acceptance allowlist includes `file` and FTP-family, so it cannot be the
  transform allowlist. Built-in SEO transforms require HTTP(S) eligibility plus
  explicit profile opt-in.
- **Existing email posture**: `.STATUS_WARN_USERINFO` for scheme-less
  `user@example.com` (components resolve, `clean_url` NA). Foundation for the
  email PRD.

---

## Open questions (genuine architecture blockers only)

Implementation-level questions (email-fact surface, validation-error default-on,
classifier scope) have moved to the two PRDs; questions D1–D6 already resolve are
recorded in those decisions, not relisted here.

1. **Profile names + `seo`/`canonical` primary.** Set = `browser` / `whatwg` /
   `rfc-syntax` / (`canonical`|`seo`), one bundle with an alias to keep "≤4".
   Avoid `omnibox` (jargon) and verb names `parse`/`clean`/`validate`. Two leans
   on which is primary: **Claude** — `canonical` primary (consistency with
   `clean_url`); **Codex** — `seo` primary, `canonical` alias (these transforms
   are policyful and may not preserve semantic equivalence, so `canonical` can
   overpromise). Maintainer call. *(Blocks L6b bundle wiring.)*
2. **`sftp`/`ftps`.** *Resolved (RURL-lvrfrkfa).* The standing rule is: **`sftp`
   stays OUT of the default allowlist; `ftps` is grandfathered IN.** `ftps` has
   shipped in `.SUPPORTED_SCHEMES` (`R/utils.R`:
   `c('http','https','ftp','ftps','file')`) since before this epic, so removing
   it would break the hard CRAN byte-identity constraint — it is retained for
   backward compatibility, **not** because it has standards backing (no
   URI-scheme RFC, absent from the IANA registry; RFC 4217 is protocol-only).
   Consistently, `ftps` carries **no** default port (it is correctly absent from
   `.SCHEME_DEFAULT_PORTS`). `sftp` is correctly absent from both the allowlist
   and the port table. General acceptance admits both tokens. This standing rule
   supersedes ADR 0004 wording; the earlier Open-Q2 recommendation to keep
   `ftps` out is void.

*Not a blocker (roadmap, not architecture):* whether `canonical`/`seo` — rurl's
origin intent, nameless today — ships first among profiles even though the parser
layers (L2–L4) gate the spec ones. This is a shipping-order call for the layer
roadmap, not an acceptance question for this ADR.
