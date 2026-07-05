<!--
Graduated from _scratch/PRD-url-standard-selector.md on 2026-07-05 per ADR 0008.
This is the durable, accepted v1 spec for the url_standard selector (epic
RURL-eqzkkohm, shipped). It is a point-in-time document: where it and the code
disagree, the code and the relevant ADR (design/adr/0007) win. Companion
research (RESEARCH-*, whatwg-url-brainstorm) remains in the working-tree
_scratch/.
-->

# PRD - `url_standard` selector for rurl

**Status:** Draft for review (pre-ticket) - derived from
`RESEARCH-url-standard-selector.md`, with review corrections folded in
**Tracking issue:** RURL-eqzkkohm
**Target release:** TBD minor for opt-in selector; default flip only in a future
major release
**Author:** design session 2026-07-05
**Companion artifacts:** `_scratch/RESEARCH-url-standard-selector.md`,
`_scratch/whatwg-url-brainstorm.md`

---

## 1. Background & problem

rurl exposes many independent URL normalization controls:

- path structure: `path_normalization`
- path percent handling: `path_encoding`
- case: `case_handling`
- host presentation: `host_encoding`
- scheme inference and supported schemes
- current strict IP-literal gate

This gives callers power, but no standard-level intent. Consumers currently
assemble their own profiles and can easily land between RFC 3986 and WHATWG in
ways neither standard endorses.

Two concrete failures motivate this PRD:

1. **Path false joins.** Profiles using `path_encoding = "decode"` or
   `"encode"` decode reserved path bytes before normalization. That collapses
   `/a%2Fb` and `/a/b`, even though RFC 3986 and WHATWG both preserve `%2F` as
   data inside a path segment.
2. **Host false rejects.** rurl currently rejects numeric, hex, octal, and
   short-form hosts at parse time. Some are valid RFC 3986 `reg-name` values;
   under WHATWG they can parse to IPv4 addresses. Hard rejection conflates
   standard conformance with caller safety policy.

The product need is a small, explicit selector that says which standard model a
consumer wants for canonical URL keys, without requiring every consumer to know
the low-level knob matrix.

## 2. Goals

1. Add an opt-in top-level selector:

   ```r
   url_standard = c("rfc3986", "whatwg")
   ```

2. In v1, govern only the proven-broken axes:
   - path percent normalization;
   - dot-segment recognition/removal;
   - host IPv4/reg-name parsing model;
   - diagnostics for suspicious but parseable host forms.
3. Preserve current defaults when `url_standard = NULL`.
4. Keep standard conformance separate from safety policy: parse faithfully where
   the selected standard permits parsing, then attach diagnostics that callers
   can use to reject SSRF/allowlist bypass risks.
5. Provide enough fixture coverage that rurl can prove each profile's behavior
   against RFC-derived expectations and selected WHATWG/WPT cases.

## 3. Non-goals (v1)

- No full WHATWG URL object implementation.
- No new parser backend requirement. v1 can continue to use curl plus rurl-owned
  raw path extraction and host post-processing where needed.
- No default behavior change.
- No sealed profile objects.
- No promise that `url_standard` governs every existing rurl option.
- No port, backslash, IDNA, query, scheme table, or relative-URL changes in v1.
  These are future axes and must not be implied by the v1 selector.
- No caller safety policy. rurl emits parse results and diagnostics; consumers
  decide whether to reject.

## 4. Decision log

| # | Decision | Choice | Rationale |
|---|---|---|---|
| D1 | Selector shape | `url_standard = NULL` by default; opt-in values `"rfc3986"` / `"whatwg"` | Avoids calling current behavior a standard while preserving CRAN compatibility. |
| D2 | v1 coverage | Path percent, dot segments, host IPv4/reg-name model, diagnostics | These are the axes with demonstrated false joins/false rejects. |
| D3 | Override policy | Conflicting explicit low-level knobs error when `url_standard` is set | Keeps the selector coherent; avoids creating new accidental profiles. |
| D4 | RFC host model | Numeric/hex/octal/short hosts that are not RFC dotted quads parse as `reg-name` where the grammar allows | RFC 3986 host uses first-match-wins for IPv4, but non-IPv4 hosts can still be valid reg-names. |
| D5 | WHATWG host model | Numeric/hex/octal/short hosts parse according to the WHATWG IPv4 parser where WHATWG permits | Browser-faithful canonical keys should match where browsers actually navigate. |
| D6 | Diagnostics | Every non-canonical IPv4 spelling gets a diagnostic, including whole-host decimal | Whole-host decimal is standard under WHATWG but still a security-relevant footgun. |
| D7 | Port/backslash/case scope | Not governed by v1 selector | Prevents the PRD from promising a full standard profile before the behavior matrix exists. |
| D8 | Interim migration | `path_encoding = "keep"` is a narrow collision stopgap, not a full WHATWG profile | It preserves `%2F`, but also changes `%41` and `%2e%2e` behavior versus decode profiles. |

## 5. Public API

Add `url_standard` to:

- `safe_parse_url()`
- `safe_parse_urls()`
- `get_clean_url()`
- `get_path()` (the selector's path percent/dot policy applies to the returned
  path, so `get_path("…/%41%42", url_standard = "rfc3986")` -> `/AB`)
- `get_host()` (host IPv4/reg-name model is selector-governed; without this the
  regression assertions in §9.3 cannot be written)
- `get_parse_status()` (host parseability changes under the selector -
  `2130706433` moves from `error` to parseable, so the status accessor must see
  the selector to report the new outcome)
- `get_domain()` / `get_tld()` / `get_subdomain()` (yes: these ride on the
  post-parse host and the pslr seam, so a selector that changes what the host
  parses to changes their inputs; they must accept `url_standard` for symmetry)
- `canonical_join()` through forwarded parse options

Proposed signature shape — **`url_standard` is appended after the existing
options, not inserted after `url`:**

```r
safe_parse_url <- function(
  url,
  ... existing options (unchanged order) ...,
  url_standard = NULL
)
```

Argument-position compatibility is a hard constraint (CRAN backward-compat).
Inserting `url_standard` as the second positional parameter would silently
rebind existing positional calls: today `safe_parse_url(url, "none")` binds the
2nd arg to `protocol_handling` and `get_host(url, "keep")` binds it to
`host_encoding`. `url_standard` must therefore be added **last** in each
signature (after all current options, before `...` where one exists), so no
existing positional call changes meaning. All new tests and docs must pass it
**by name** (`url_standard = "rfc3986"`), never positionally.

Validation:

- `NULL` preserves today's behavior exactly.
- Non-NULL must be one of `"rfc3986"` or `"whatwg"`.
- Public wrappers must use `missing()` or equivalent argument capture to know
  whether governed low-level knobs were explicitly supplied.
- If `url_standard` is set and the caller explicitly supplies a governed knob
  with a value outside the selected profile, error.
- If the caller supplies the same value the profile would choose, accept it.
- `canonical_join()` forwards parse options through `...`, where `missing()`
  does not apply - a governed knob passed via `...` is indistinguishable from a
  default. Conflict detection across that seam must be explicit (e.g. capture
  the dots and check for governed names present in the call), or the error rule
  in the two bullets above silently will not fire for `canonical_join()`.

### 5.1 Caching interaction (Stage A vs Stage B)

`url_standard` is **Stage-A-affecting** and must be reflected in the parse cache
key, or the standard-dependent host work must move out of cached Stage A.

Today `.parse_cache_keys()` (`R/parse.R`) keys the Stage A parse core on
`url x protocol_handling x www_handling x tld_source x scheme_relative_handling`
only. Per the parse/present split (RURL-dkwrebdt), Stage A is the
option-independent core and explicitly includes **IP detection, host rejection,
final host, and the PSL decomposition** - exactly the layer this selector
changes (`2130706433` -> `error` vs `reg-name` vs `127.0.0.1`). With the current
key, a second call under a different `url_standard` would return a stale cached
host.

v1 must pick one:

- **(a)** Add `url_standard` to the Stage A cache key. Simplest; costs one extra
  cache entry per URL per standard actually used.
- **(b)** Keep host-standard work in Stage A but make it standard-independent
  (compute a superset once, select per standard in Stage B). Lower cache
  footprint; more implementation surface.

The path percent/dot policy is already a Stage-B presentation transform, so the
path axis needs no cache-key change; only the host axis forces this decision.

Governed low-level knobs in v1:

| Knob / behavior | Governed by v1? | Notes |
|---|---:|---|
| `path_encoding` | yes | Standard profiles require new internal modes, not just existing enum values. |
| `path_normalization` dot semantics | yes | Both profiles resolve dot segments, but differ in encoded-dot handling source. |
| host IPv4 parsing/coercion | yes | RFC mode distinguishes IPv4address from reg-name; WHATWG mode coerces valid numeric IPv4 forms. |
| diagnostics | yes | New parse metadata, not a cleaning knob. |
| `case_handling` | no | Existing default already lowercases scheme/host for clean URLs; full profile treatment deferred. |
| default ports | no | Future `port_handling` work. |
| backslash-as-slash | no | Future WHATWG profile axis. |
| IDNA/UTS-46 | no | Future host profile axis. |
| query handling | no | Covered by the query PRD, not this selector. |

## 6. Behavior matrix

### 6.1 Path percent handling

v1 needs two new internal path policies. They may be exposed later, but they do
not need to become public enum values if `url_standard` is the only entry point.

| Input path | `rfc3986` clean path | `whatwg` clean path | Notes |
|---|---|---|---|
| `/a/../b/./c` | `/b/c` | `/b/c` | Literal dot segments removed. |
| `/%2e%2e/a` | `/a` | `/a` | RFC decodes unreserved `%2E` to `.` before dot handling; WHATWG names encoded-dot segments. |
| `/%41%42` | `/AB` | `/%41%42` | Main canonical-key divergence. |
| `/%7euser` | `/~user` | `/%7Euser` | RFC unreserved fold; WHATWG preserves the encoded byte. Hex case canonicalization is acceptable for rurl keys. |
| `/%20foo` | `/%20foo` | `/%20foo` | Space is not unreserved; keep encoded. |
| `/caf%C3%A9` | `/caf%C3%A9` | `/caf%C3%A9` | Non-ASCII bytes stay percent-encoded. |
| `/a%2Fb` | `/a%2Fb` | `/a%2Fb` | Reserved slash stays data, not a segment separator. |
| `/a%3Fb` | `/a%3Fb` | `/a%3Fb` | Reserved question mark stays encoded in path. |
| `/a//b` | `/a//b` | `/a//b` | Slash collapsing is not part of v1 standard profiles. |

Required implementation properties:

- Never use full percent decode for standard profiles.
- Never decode reserved characters for standard profiles.
- RFC mode decodes only unreserved percent-encoded octets:
  `ALPHA`, `DIGIT`, `-`, `.`, `_`, `~`.
- WHATWG mode preserves percent-encoded unreserved bytes in path identity, while
  recognizing WHATWG single-dot and double-dot path segments, including encoded
  dot forms.
- **Ordering is normative, not incidental.** The current dot-segment remover
  (`._remove_dot_segments()`, `R/path-query.R`) matches **literal** `.` / `..`
  segments only. So:
  - **RFC mode** must decode unreserved octets (which includes `%2E` -> `.`)
    *before* running dot-segment removal, so `/%2e%2e/a` folds to `/..` then to
    `/a`. Decode-then-remove; never remove-then-decode.
  - **WHATWG mode** must run an **encoded-dot recognizer** as part of
    dot-segment removal - treating `.`, `%2e`, `%2E` as a single-dot segment and
    `..`, `.%2e`, `%2e.`, `%2e%2e` (all hex-case-insensitive) as a double-dot
    segment - *without* performing a general path percent-decode. This is a new
    recognizer, not a tweak to the existing literal matcher.
- Percent-triplet hex case should be canonicalized consistently in rurl keys so
  `%2f` and `%2F` compare equal. Any divergence from browser byte-for-byte href
  spelling must be documented as rurl canonical-key behavior.

### 6.2 Host parsing

| Input host | RFC 3986 mode | WHATWG mode | Diagnostics |
|---|---|---|---|
| `127.0.0.1` | IPv4 `127.0.0.1` | IPv4 `127.0.0.1` | none |
| `2130706433` | reg-name `2130706433` | IPv4 `127.0.0.1` | `ipv4-number-form`, `ipv4-non-dotted` |
| `0x7f000001` | reg-name `0x7f000001` | IPv4 `127.0.0.1` | `ipv4-number-form`, `ipv4-non-decimal` |
| `017700000001` | reg-name `017700000001` | IPv4 `127.0.0.1` | `ipv4-number-form`, `ipv4-non-decimal`, `ipv4-octal`, `ipv4-leading-zero` |
| `0177.0.0.1` | reg-name `0177.0.0.1` | IPv4 `127.0.0.1` | `ipv4-non-decimal`, `ipv4-octal`, `ipv4-leading-zero` |
| `192.168.010.1` | reg-name `192.168.010.1` | IPv4 `192.168.8.1` | `ipv4-non-decimal`, `ipv4-octal`, `ipv4-leading-zero` |
| `192.168` | reg-name `192.168` | IPv4 per WHATWG parser | `ipv4-short-form` |
| `0` | reg-name `0` | IPv4 `0.0.0.0` | `ipv4-number-form`, `ipv4-non-dotted` |
| `256.1.1.1` | reg-name `256.1.1.1` | parse failure (WHATWG IPv4 parser rejects the out-of-range last part) | RFC: `ipv4-out-of-range`; WHATWG: n/a (fatal) |

Important correction from the research note: under RFC 3986, many dotted or
numeric-looking hosts that are not valid `IPv4address` values are still valid
`reg-name` values. RFC mode must not reject them just because they look like
dangerous IP spellings. It must avoid coercing them to IPs.

**Diagnostic-token rule (resolves the two octal rows above; decided during
RURL-luwvkwhd, 2026-07-05).** The `ipv4-non-decimal` / `ipv4-octal` /
`ipv4-leading-zero` tokens are **independent facts, emitted whenever
applicable** - not a partition. A leading-zero octal part (e.g. `0177`, `010`)
is simultaneously octal, leading-zero, and non-decimal, so it carries all three
tokens. The earlier draft rows assigned each octal example a different
two-token subset; there is no principled rule that distinguishes `0177` from
`010`, so the table now shows the full applicable set for each. This is the
"facts not policy" principle applied to diagnostics: emit every observed fact
and let the consumer filter. Conformance fixtures (RURL-cuqafsif) must assert
the full sets shown above.

**Diagnostics apply in both modes, keyed to host shape - not to coercion
outcome.** The Diagnostics column above is not WHATWG-only. A numeric-looking
host such as `2130706433`, `0x7f000001`, or `256.1.1.1` carries its diagnostics
under `rfc3986` too, even though RFC mode keeps it a `reg-name` and never dials
it. This is the "facts not policy" principle (§2 goal 4, §5 of the research
note): an SSRF-sensitive consumer must be able to reject `http://2130706433/`
whether the caller selected RFC or WHATWG. Emitting diagnostics only in WHATWG
mode would leave RFC-mode consumers unable to see the footgun. Acceptance
criterion 5 is corrected accordingly (see §10).

### 6.3 Parse result fields

Two pieces of parse metadata are added:

| Field | Type | Purpose |
|---|---|---|
| `host_type` | `domain` / `ipv4` / `ipv6` / `reg-name` / `missing` | Lets callers distinguish RFC reg-name from coerced WHATWG IPv4. |
| `diagnostics` | character vector of diagnostic tokens per URL | Non-fatal validation/safety facts. |

`host_type` is a function of `(host, url_standard)`, **not** of the host string
alone: the same input (`2130706433`) is `reg-name` under `rfc3986` and `ipv4`
under `whatwg`. Callers reading `host_type` must therefore know which selector
produced it.

Return-shape contract (one URL can carry several tokens, so the helper is not a
plain scalar-per-URL):

- `get_url_diagnostics(url, url_standard = …)`: for a length-1 `url`, returns a
  character vector of zero or more tokens; for a length-n `url`, returns a
  **list of length n**, each element a character vector of that URL's tokens
  (`character(0)` when none). This mirrors how a variadic-per-row field is
  surfaced without becoming a `safe_parse_urls()` list-column.
- `get_host_type(url, url_standard = …)`: exactly one token per URL, so it
  returns a plain character vector of length n.

**v1 surface decision (resolves Open Question 1): companion helpers, not a
widened output shape.** v1 exposes this metadata through helpers -
`get_host_type(url, url_standard = …)` and
`get_url_diagnostics(url, url_standard = …)` - and does **not** add columns to
`safe_parse_urls()` or fields to the `safe_parse_url()` list in v1. This keeps
the `url_standard = NULL` compatibility story airtight: with no selector, the
output shape of every existing function is byte-for-byte unchanged (acceptance
criterion 1), because nothing was widened. Additive columns / list-columns on
`safe_parse_urls()` remain a candidate for a later minor once the metadata has
proven out, but are explicitly out of scope for v1.

`parse_status` remains coarse and backward-compatible:

- parseable-with-diagnostics stays `ok` or an existing warning status unless
  the current API already requires a warning;
- fatal parse failures stay `error`;
- diagnostics are not encoded into `parse_status` strings.

## 7. Diagnostics vocabulary

Initial diagnostics:

| Diagnostic | Meaning | Typical consumer action |
|---|---|---|
| `ipv4-number-form` | Host used a numeric IPv4 shorthand instead of dotted decimal | Security filters usually reject. |
| `ipv4-non-dotted` | Whole-host number parsed/coerced to IPv4 in WHATWG mode | Security filters usually reject. |
| `ipv4-short-form` | Fewer than four dotted IPv4 parts | Security filters usually reject. |
| `ipv4-non-decimal` | Hex or octal notation participated in IPv4 parsing | Security filters reject. |
| `ipv4-octal` | Octal interpretation changed apparent address value | Security filters reject. |
| `ipv4-leading-zero` | Dotted decimal-looking part had a leading zero | Security filters reject. |
| `ipv4-out-of-range` | A dotted part exceeds 255 (fatal for WHATWG; flags a numeric-looking `reg-name` in RFC mode, e.g. `256.1.1.1`) | Security filters reject. |
| `encoded-dot-segment` | Path contained an encoded-dot segment (`%2e` / `%2e%2e`, any hex case) that the profile's dot handling acted on | Audit; may affect traversal-like keys. |
| `encoded-reserved-path-byte` | Path contains an encoded reserved byte such as `%2F` / `%3F` / `%23` that the profile preserved as data | Audit; distinct from a literal separator. |

**Naming note (reconciliation with the research doc).** These `ipv4-*` /
`encoded-*` tokens are the authoritative v1 vocabulary and **supersede** the
provisional names used in `RESEARCH-url-standard-selector.md` §5
(`non-decimal-ipv4` -> `ipv4-non-decimal`; `ambiguous-octet` ->
`ipv4-octal` + `ipv4-leading-zero`; `decoded-reserved` ->
`encoded-reserved-path-byte`). Downstream consumers (pagerankr, sitemapr,
semantic) must gate on the names in this table, not the research-doc drafts.

**Path-diagnostic triggers (pin the two rows not exercised by any matrix
above).** `encoded-dot-segment` fires when a `%2e`/`%2e%2e` segment is
recognized and removed - i.e. on the `/%2e%2e/a` rows of §6.1, in both modes.
`encoded-reserved-path-byte` fires whenever the preserved clean path still
contains an encoded reserved byte (`%2F`, `%3F`, `%23`) - i.e. on the `/a%2Fb`
and `/a%3Fb` rows of §6.1, in both modes. Add explicit §9.1 fixtures asserting
each token's presence.

Diagnostics are facts, not policy. pagerankr can ignore them when building link
graph keys. sitemapr or any fetcher/SSRF guard can reject based on them.

## 8. Migration plan

### 8.1 rurl package

Phase 1 - opt-in:

- Add `url_standard = NULL`.
- Add `"rfc3986"` and `"whatwg"` profiles.
- Add diagnostics access.
- Keep all existing defaults and low-level option defaults unchanged.

Phase 2 - ecosystem adoption:

- pagerankr and semantic pin `url_standard = "whatwg"` if WHATWG-aligned link
  identity **for the governed axes** (path percent/dot handling, host
  IPv4/reg-name model) is the desired contract. This is deliberately narrower
  than "browser-faithful": v1 does not govern backslashes, default ports, IDNA,
  query handling, or relative-URL resolution (§3 non-goals), so `"whatwg"` is
  WHATWG-*closer*, not a complete browser URL model.
- Security-sensitive consumers inspect diagnostics before fetching.

Phase 3 - possible major default flip:

- After at least one release cycle, decide whether `get_clean_url()` should
  default to `url_standard = "whatwg"` or remain legacy-by-default.
- Any default flip requires a major release and a dedicated migration note.

### 8.2 Interim before selector lands

For pagerankr/semantic, `path_encoding = "keep"` is a useful stopgap because it
prevents `%2F` reserved-byte false joins.

It is not behavior-equivalent to `url_standard = "whatwg"`:

- `/%41%42` remains encoded under `keep`, which is WHATWG-like but differs from
  RFC mode.
- `/%2e%2e/a` remains encoded and does not resolve under current `keep` plus
  RFC literal-dot normalization, while WHATWG mode would remove it.
- It does not change host numeric parsing.

The interim change should be documented as a collision fix, not as a complete
standard profile.

## 9. Test strategy

### 9.1 Golden fixtures

Add a fixture table with columns:

```text
id
input
url_standard
expected_clean_url
expected_host
expected_host_type
expected_path
expected_diagnostics
source_reference
notes
```

Fixture groups:

- reserved path bytes: `%2F`, `%3F`, `%23`;
- unreserved path bytes: `%41`, `%7E`, mixed hex case;
- encoded dot segments: `%2e`, `%2e%2e`, mixed case;
- literal dot segments;
- non-ASCII and spaces in path;
- canonical IPv4;
- whole decimal IPv4;
- whole hex/octal IPv4;
- dotted octal/leading-zero IPv4;
- short-form IPv4;
- out-of-range IPv4-looking hosts (`256.1.1.1`: RFC `reg-name` +
  `ipv4-out-of-range`; WHATWG fatal parse failure);
- numeric-looking hosts under **both** modes, asserting the diagnostic fires in
  RFC mode too (not only WHATWG);
- path-diagnostic presence: `encoded-dot-segment` on `/%2e%2e/a`,
  `encoded-reserved-path-byte` on `/a%2Fb` and `/a%3Fb`.

### 9.2 Reference expectations

- RFC expectations should be derived from RFC 3986 grammar and normalization
  rules, documented in fixture notes.
- WHATWG expectations should be compared against selected WPT
  `urltestdata.json` cases or a pinned reference implementation. Do not rely on
  libcurl alone for WHATWG host behavior.
- Existing rurl behavior should remain covered by legacy tests with
  `url_standard = NULL`.

### 9.3 Regression assertions

Required high-signal assertions:

```r
expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
                url_standard = "whatwg"))
expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
                url_standard = "rfc3986"))

expect_true(eq("http://ex.com/%41%42", "http://ex.com/AB",
               url_standard = "rfc3986"))
expect_false(eq("http://ex.com/%41%42", "http://ex.com/AB",
                url_standard = "whatwg"))

expect_equal(get_host("http://2130706433/", url_standard = "rfc3986"),
             "2130706433")
expect_equal(get_host("http://2130706433/", url_standard = "whatwg"),
             "127.0.0.1")

# path accessor honors the profile percent policy directly
expect_equal(get_path("http://ex.com/%41%42", url_standard = "rfc3986"), "/AB")
expect_equal(get_path("http://ex.com/%41%42", url_standard = "whatwg"),
             "/%41%42")

# diagnostics fire in BOTH modes for numeric-looking hosts (facts, not policy)
expect_true("ipv4-non-dotted" %in%
            get_url_diagnostics("http://2130706433/", url_standard = "whatwg"))
expect_true("ipv4-number-form" %in%
            get_url_diagnostics("http://2130706433/", url_standard = "rfc3986"))

# host_type is (host, url_standard)-dependent
expect_equal(get_host_type("http://2130706433/", url_standard = "rfc3986"),
             "reg-name")
expect_equal(get_host_type("http://2130706433/", url_standard = "whatwg"),
             "ipv4")

# NULL selector leaves the output shape untouched (no new columns/fields in v1)
expect_identical(names(safe_parse_urls("http://ex.com/")),
                 names(safe_parse_urls("http://ex.com/", url_standard = NULL)))
```

The helper names (`get_url_diagnostics`, `get_host_type`) match the v1
companion-helper surface decided in §6.3.

## 10. Acceptance criteria

1. `url_standard = NULL` produces byte-for-byte identical outputs to current
   defaults on the existing test corpus, **and does not change the output shape
   of any function** (no new columns on `safe_parse_urls()`, no new list fields
   on `safe_parse_url()`) - metadata lives behind companion helpers in v1 (§6.3).
   This is enforced mechanically: snapshot the full existing corpus outputs and
   assert `NULL` reproduces them bit-for-bit in CI.
2. `url_standard = "rfc3986"` decodes only unreserved path bytes and never
   decodes reserved bytes such as `%2F`.
3. `url_standard = "whatwg"` preserves encoded unreserved path bytes but handles
   encoded dot segments according to the WHATWG path rules selected for v1.
4. RFC mode parses numeric-looking non-IPv4 hosts as `reg-name` when RFC grammar
   permits; it does not coerce them to IP addresses.
5. WHATWG mode coerces valid numeric IPv4 forms according to the pinned WHATWG
   reference. Diagnostics are attached to every non-canonical spelling in
   **both** RFC and WHATWG modes, keyed to host shape (a numeric-looking
   `reg-name` under RFC carries the same diagnostic it would under WHATWG).
6. Whole-host decimal IPv4 forms such as `2130706433` carry a diagnostic in both
   modes.
7. Conflicting explicit low-level knobs error when `url_standard` is set,
   including knobs forwarded through `canonical_join()`'s `...` (§5).
8. `get_path()`, `get_host()`, `get_parse_status()`, `get_domain()`,
   `get_tld()`, and `get_subdomain()` all honor the selector consistently with
   `get_clean_url()`.
9. Cache correctness: a URL parsed under one `url_standard` and then another
   returns the correct per-standard host, not a stale cached value (§5.1).
10. Documentation clearly states that v1 does not govern default ports,
    backslashes, IDNA, query handling, or relative URL resolution.

## 11. Open questions

1. ~~Diagnostics surface: list-column vs companion helper vs attribute.~~
   **Resolved (§6.3):** v1 uses companion helpers (`get_url_diagnostics()`,
   `get_host_type()`); no widened output shape. Additive columns deferred to a
   later minor.
2. Whether `host_type = "reg-name"` should be distinct from `"domain"` in the
   public helper output, or only internal. Distinction is useful for
   numeric-looking RFC hosts; leaning distinct, since it is the whole point of
   letting a consumer tell an RFC reg-name from a coerced WHATWG IPv4.
3. Whether the new RFC unreserved-only path normalizer should become a public
   `path_encoding` value, e.g. `"unreserved"`, or stay internal to
   `url_standard = "rfc3986"`.
4. Whether the WHATWG path mode should preserve original percent-triplet hex
   case byte-for-byte or continue rurl's canonical-key uppercasing. This PRD
   leans toward uppercasing for stable keys, with docs.
5. How much of WHATWG host parsing to implement directly in R versus delegating
   to a future Ada/reference backend.

## 12. Recommended next steps

1. Red-pen this PRD's behavior matrix against a pinned WHATWG reference corpus
   (in particular the `256.1.1.1` and short-form `192.168` rows).
2. ~~Choose the diagnostics API shape.~~ Decided (§6.3): companion helpers for
   v1. Remaining sub-decision: finalize helper names / signatures.
3. Decide the Stage-A cache strategy (§5.1 option (a) vs (b)) before touching
   host parsing - it constrains where the host-standard logic can live.
4. Implement only path profiles first behind internal helpers, then host
   profiles, then the public selector.
5. Add migration notes for pagerankr/semantic distinguishing interim
   `path_encoding = "keep"` from the final `url_standard = "whatwg"` pin.
