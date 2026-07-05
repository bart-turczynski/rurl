<!--
Graduated from _scratch/PRD-url-standard-selector-v2.md on 2026-07-05 per ADR
0008. This is the durable, accepted v2 spec for the url_standard selector (epic
RURL-uyjheruh, shipped T1-T7). It is a point-in-time document: where it and the
code disagree, the code and the relevant ADR (design/adr/0007) win. Companion
research/handoff notes remain in the working-tree _scratch/.
-->

# PRD - `url_standard` selector for rurl, v2

**Status:** Draft for review (pre-ticket) - derived from a design conversation
following v1's close-out, cross-checked against the actual `punycoder` /
`pslr` APIs (not assumed from docs)
**Predecessor:** RURL-eqzkkohm (v1 - shipped T1-T6, PR #94-#99; T7 docs/NEWS
remains before v1 formally closes)
**Tracking issue:** RURL-uyjheruh (parent tracker; child epics T1-T8 filed)
**Target release:** TBD minor(s); no default-flip implied
**Author:** design session 2026-07-05
**Companion artifacts:** `_scratch/PRD-url-standard-selector.md` (v1, shipped),
`_scratch/RESEARCH-url-standard-selector.md`,
`_scratch/whatwg-url-brainstorm.md`

---

## 1. Background

v1 (RURL-eqzkkohm) deliberately governed only three proven-broken axes: path
percent/dot-segment handling, host IPv4/reg-name parsing, and a narrow
diagnostics set. PRD v1 §3 named several axes as non-goals "that must not be
implied by the v1 selector": ports, backslashes, IDNA, query handling, the
scheme table, and relative-URL resolution. Query handling is out of scope
permanently (owned by the separate query PRD/epic). The rest were open gaps,
not a committed roadmap - this PRD closes that gap by scoping them.

Two inputs shaped this PRD beyond v1's own non-goals list:

- `whatwg-url-brainstorm.md`'s numbered ideas (#1-#12), which named several of
  these axes independently, plus a few v1 never mentioned (DNS length,
  `scheme_class`, `explain_parse_url()`, `max_url_length`, URLPattern-style
  grouping).
- A live check of the installed `punycoder` package (not just its brainstorm
  mention), which found `host_normalize()` and `validate_domain()` already
  implement most of what a naive "DNS length validation" ticket would have
  reimplemented, and confirmed `punycoder::parse_url()` / `url_encode()` /
  `url_decode()` (whole-URL, non-conformant helpers) are already `Deprecated`
  in punycoder's own docs in favor of rurl. That check is recorded here so
  nobody re-derives it: **rurl already correctly avoids calling those three
  deprecated functions** (grep-verified against `R/*.R` on 2026-07-05); no
  migration is needed, just a standing constraint not to introduce a call to
  them.

## 2. Goals

Scope the following axes for a `url_standard` v2, each as a self-contained
decision that can become its own ticket:

1. Default-port elision under `url_standard` (not full port policy).
2. Backslash-as-slash recognition under `url_standard = "whatwg"` for
   WHATWG-special schemes only.
3. Expanded diagnostics vocabulary riding along with (1), (2), and (4).
4. DNS length + UTS-46 conformance diagnostics, via a new `punycoder` seam,
   diagnostic-only (never rendering).
5. Close the `case_handling` conflict-matrix gap (no new normalization logic
   required - the current default is already conformant).
6. `resolve_url(relative, base, url_standard = ...)` - a new entry point that
   composes existing/planned axes rather than adding a new divergent one.
7. A `url_standard`-scoped special-scheme lookup + `scheme_class` companion
   helper.

## 3. Non-goals (v2)

- Query handling - permanently out of scope for this selector; owned by the
  query PRD/epic.
- Expanding rurl's allowed-scheme set to `ws`/`wss`/`file`. Not relevant to
  rurl's SEO/crawl-data audience; `http`/`https`/`ftp`/`ftps` remains the
  allowlist.
- `unsupported_scheme = "error"/"warn"/"opaque"` policy knob (brainstorm idea
  #8's second half). Current reject-on-disallowed-scheme behavior is
  sufficient; revisit only if a concrete consumer need appears.
- `max_url_length` / `max_clean_url_length` guards (brainstorm idea #6) and
  URLPattern-style path grouping (idea #12). Both are standalone features that
  do not read `url_standard` at all - track as independent tickets, not as
  part of this epic.
- `explain_parse_url()` debug/introspection helper (idea #10) - standalone,
  not selector-dependent; independent ticket if wanted.
- An Ada (or any alternate) parser backend. Explicitly left alone. The
  intended way to "beat Ada" is continuing to mine its WPT/fuzz fixture
  corpora as free conformance material (as T6 already did), not adopting it
  as a dependency.
- Full IDNA2008/UTS-46 *implementation* work. That already lives in
  `punycoder`; v2 only adds a diagnostic call into it, gated by
  `url_standard`. See §5.4.
- Any change to `.normalize_and_punycode()` / `.punycode_to_unicode()`
  rendering behavior. Per the standing RURL-ntdnoywx decision, these stay
  lenient/reversible/case-preserving; nothing here revisits that.

## 4. Decision log

| # | Decision | Choice | Rationale |
|---|---|---|---|
| D1 | Port scope | `url_standard` governs only default-port elision (WHATWG special schemes: elide 80/443/21/80/443 for http/https/ftp/ws/wss; `rfc3986` never elides, since it has no default-port concept). Non-default-port stripping is a **separate**, standard-independent `port_handling` knob. **Correction (Codex review, 2026-07-05):** the v1 draft of this row described `"keep"` as "current behavior." Verified false - `R/accessors.R:146-147` states port is **always excluded** from `clean_url` today, unconditionally, regardless of default/non-default. There is no existing "keep the port" behavior to preserve; adding any value that puts a port back into `clean_url` output is new behavior to the canonical-key contract, not a knob default. `port_handling` must default to `"exclude"` (today's only behavior) so `url_standard = NULL` / no `port_handling` set stays byte-identical to current output, matching the v1 acceptance-criterion-1 pattern. | Default-port elision is genuine spec behavior (conformance); stripping/keeping ports is rurl editorial policy for aggressive joins, same tier as `www_handling`/`trailing_slash_handling`/`index_page_handling` - it must not be smuggled into the standard selector, and must not silently change what `clean_url` returns today. |
| D2 | Backslash scope | Literal backslash only, only for schemes in the WHATWG special-scheme set actually in rurl's allowlist (`http`, `https`, `ftp` - **not** `ftps`, which WHATWG does not define). Treated identically to `/` everywhere the WHATWG state machine checks for a slash (scheme-relative `//` marker, authority/path boundary, path-segment separator). Query/fragment untouched. `%5C` never gets separator treatment (no general decode, matching the T5 encoded-dot precedent). `rfc3986` mode: backslash is always inert literal data. | Mirrors the T5 "recognizer without decode" pattern already built; scoping to rurl's actual allowlist avoids inventing behavior for schemes rurl doesn't support. |
| D3 | DNS length ownership | Delegate to `punycoder` via a new seam (e.g. `.punycoder_host_probe()`, mirroring the `.psl_*` seam pattern) rather than reimplementing RFC 1035 label/name length checks in rurl. **Correction 1 (Codex review, 2026-07-05, empirically re-verified):** the v1 draft claimed `validate_domain()` returns "independent, non-abortive" codes. **False - verified live.** A domain that is simultaneously too-long AND STD3-invalid (`strrep("y", 70) %+% "_z"`) returns **only** `"ascii_domain_characters"` from `validate_domain()`, not both facts; `strict = FALSE` doesn't recover the missing fact either (returns `character(0)`). **Correction 2 (second Codex review, same day, empirically re-verified):** the fix proposed for Correction 1 - toggling `host_normalize()`'s three flags one at a time from an all-strict baseline - is *itself* wrong. Verified live: a host failing 2 of 3 checks (`use_std3`+`verify_dns_length`) and a host failing all 3 checks produce the **identical** all-`NA` result pattern under that design, making them indistinguishable. **Correct design (verified live):** invert the direction - start from all three flags relaxed, then re-enable exactly one flag at a time (holding the other two relaxed); each such call isolates one check with zero ambiguity regardless of how many others fail. This correctly distinguished the 2-of-3 and 3-of-3 cases where the first fix could not. Also verified `domain-empty-label` isn't controlled by any of the three flags (`host_normalize("a..com", ...)` with all three relaxed still returns `NA`) and needs its own detector. See §5.4 for the full corrected design. | `punycoder` already implements the underlying checks (confirmed live); reimplementing DNS length / UTS-46 checks in rurl would duplicate an existing, tested dependency. The diagnostic *extraction* mechanism took two rounds of review to get right - worth noting as a reminder that "verified live" claims still need the actual failure-isolation logic checked, not just the raw API behavior. |
| D4 | IDNA/UTS-46 scope | Diagnostic-only, via the flag-toggling probe from D3 (not `validate_domain()` directly - see D3 correction), against `host_normalize()`. Never use its output to change what `get_host()` renders. **Correction (Codex review, 2026-07-05):** the v1 draft of this row said "under `rfc3986`, skip the UTS-46 gate entirely," which directly contradicted Open Question 3 below (leaning toward both modes, facts-not-policy). Resolved in favor of Open Question 3's position: **run the same diagnostic probe under both `rfc3986` and `whatwg`.** RFC 3986 has no UTS-46 *requirement*, but v1 already established the precedent (§6.2/§7 of the v1 PRD) that a footgun diagnostic fires regardless of which standard is selected, because the caller's safety decision shouldn't depend on which conformance profile they happened to pick. Open Question 3 is now resolved into this decision, not left open. | Reconciles the diagnostic need with the standing RURL-ntdnoywx decision that `host_normalize()` is unsuitable for rendering (force-lowercases, NA's out hosts rurl tolerates). Running it in parallel, probe-style, for facts-only output uses it for exactly what its own docs say it's for ("full WHATWG host policy... lives upstack in rurl"), without touching rendering, and matches v1's facts-not-policy precedent. |
| D5 | `case_handling` | Add to the v1 governed-knob conflict matrix (§5 D3 pattern in v1's PRD). Under a selector: **only `"lower_host"` passes; `"lower"`, `"upper"`, and `"keep"` all error.** No new casing logic. **Correction (Codex review, 2026-07-05):** the v1 draft of this row said `"lower"` should also pass, reasoning it was "a stricter superset." Wrong - verified in `R/parse-phases.R:887` that `case_handling = "lower"` lowercases the **path** in addition to scheme+host. Neither RFC 3986 nor WHATWG normalizes path case (path is case-sensitive, server-defined, left untouched by both specs), so `"lower"` is not a superset of conformant behavior, it's a divergent one - allowing it under a selector would let a caller believe they hold a conformant clean URL when the path has been silently altered. | Verified live in `R/parse-phases.R` (`.apply_case_policy_vec`) that `get_clean_url()`'s actual default (`case_handling = "lower_host"`) already lowercases scheme+host only, matching both RFC 3986 §6.2.2.1 and WHATWG - the brainstorm doc's claim that rurl "deliberately defaults to `keep`" is stale and should be corrected wherever it's cited next. The only real gap is missing conflict-checking, not missing normalization. |
| D6 | `resolve_url()` | New public entry point, not a new divergence axis. Base-path merge (RFC 3986 §5.3: query-only ref, fragment-only ref, empty ref, scheme-relative ref, path-merge) is identical in both standards. `url_standard` flows through to the existing/planned path (T3/T5), host (T4), backslash (D2), and port (D1) logic rurl already has or is building. | Decomposing the WHATWG base-URL algorithm against RFC 3986 §5.3 shows no genuinely new divergent behavior once the axes above exist - only a shared merge step plus composition of already-scoped axes. |
| D7 | Scheme table | Add an internal `url_standard`-scoped special-scheme lookup (`http`/`https`/`ftp` = special; `ftps` = not, under `whatwg`) that D2 and D1 consult. Expose the fact via a `scheme_class` companion helper alongside the existing `host_type` (T2 infra). Do not expand rurl's allowed-scheme set. | The lookup table is required machinery for D1/D2 regardless; exposing it costs little given T2's diagnostics/companion-helper infra already exists, and answers brainstorm idea #7 without new design. |
| D8 | Library boundaries | `punycoder` owns UTS-46/IDNA/DNS-length (`host_normalize`, `validate_domain`) and the Punycode transform (`puny_encode`/`puny_decode`) - rurl calls these, never reimplements them. `pslr` stays scoped to suffix/registrable-domain matching only - no length-validation surface exists there and none should be added. rurl remains the only layer that assembles a full URL parse; `punycoder::parse_url()`/`url_encode()`/`url_decode()` are already `Deprecated` upstream in favor of rurl and must never be called (grep-verified rurl doesn't, 2026-07-05). | Explicit boundary check requested during v2 scoping to avoid rurl duplicating dependency functionality or leaking URL-shaped logic into a package scoped to host/IDNA concerns. |

## 5. Axis notes (carried from decision log, expanded where useful)

### 5.1 Port handling

**Correction (Codex review, 2026-07-05):** verified `R/accessors.R:146-147`
states port is **always excluded** from `clean_url` today, unconditionally -
there is no existing "keep the port" behavior. The draft below is revised so
`port_handling` defaults to `"exclude"` (today's only behavior, preserving
byte-identical output when the option is unset) and any value that puts a
port into `clean_url` is new, opt-in behavior, not a default change.

Two independent behaviors, deliberately not one knob:

- **Standalone `port_handling` knob (editorial, not standard-governed):**
  `"exclude"` (**default** - today's only behavior, port never appears in
  `clean_url`) / `"keep"` (new: include the port in the output, **subject to
  whatever `url_standard`'s own default-port rule says** - see below, this is
  not a verbatim-preservation guarantee) / `"strip_default"` (keep only
  non-default ports) / `"strip_all"` (alias of `"exclude"` for callers who
  want to say so explicitly). Lives alongside
  `www_handling`/`trailing_slash_handling` in the cleanup-knob tier,
  independent of `url_standard`.
- **`url_standard`-governed (conformance), only meaningful when
  `port_handling = "keep"`:** `whatwg` elides a port that matches its scheme's
  default (`http`:80, `https`:443, `ftp`:21, `ws`:80, `wss`:443, `file`: none -
  though `file` isn't in rurl's allowlist) even under `"keep"` - so `"keep"`
  under `whatwg` means "keep, minus scheme-default noise," not "preserve every
  input port byte-for-byte." `rfc3986` never elides under `"keep"`; RFC 3986
  has no default-port concept, a port is always a literal authority
  component, so `"keep"` under `rfc3986` (or with no `url_standard` set) *is*
  verbatim. Under the standalone default `"exclude"`, this rule is moot -
  there is no port in the output for either standard to act on.
  **Wording correction (Codex review, 2026-07-05):** an earlier draft called
  `"keep"` "preserve the port verbatim" without qualifying that `whatwg` still
  elides default ports underneath it, which was self-contradictory against
  the elision rule stated two sentences later. Fixed above - keep the single
  `"keep"` value (no need for a separate verbatim-only mode), just document
  that its exact output depends on `url_standard`.

### 5.2 Backslash handling

See D2. Suggested diagnostic: `invalid-reverse-solidus`, firing whenever a
literal backslash was reinterpreted as a separator (i.e. under `whatwg` +
special scheme) - lets RFC-mode or non-special-scheme consumers still see
"a browser would have treated this backslash as a path separator."

### 5.3 Diagnostics vocabulary expansion

New tokens expected from this epic, grouped by originating axis (not a new
standalone axis of its own):

| Token | Axis | Source |
|---|---|---|
| `invalid-reverse-solidus` | backslash (D2) | new, rurl-native |
| `explicit-default-port` | port (D1) | new, rurl-native - port present in input and matches the scheme's default (noisy but harmless) |
| `non-default-port` | port (D1) | new, rurl-native - port present and differs from the scheme's default (join-relevant fact) |
| `domain-label-too-long` / `domain-name-too-long` | DNS length (D3/D4) | the `host_normalize()` flag-toggling probe detects "DNS length failed"; splitting label-vs-name length needs a separate rurl detector or a scoped `validate_domain()` classification call |
| `domain-empty-label` | DNS length (D3/D4) | separate structural detector; not derived from the `host_normalize()` flag-toggling probe |
| `domain-hyphen-violation` | UTS-46 CheckHyphens (D4) | derived from the probe (`check_hyphens` flag) |
| `domain-std3-violation` | UTS-46/STD3 (D4) | derived from the probe (`use_std3` flag); **not** assumed equivalent to WHATWG's forbidden-host-code-point set, see §7 open question 2 |

**Correction (Codex review, 2026-07-05):** the v1 draft of this row named
`non-default-port-explicit` as a single token for the port axis, but that
conflated two distinct facts - a port matching the scheme default (noise, not
a join risk) versus a genuinely non-default port (an actual join-relevant
signal). Split into two tokens above. It also proposed passing through raw
`punycoder::validate_domain()` error-code strings; per the D3 correction those
codes aren't independent per-check facts, so rurl must expose its own
diagnostic vocabulary and derive each token from an explicit mechanism: the
`host_normalize()` probe for hyphen/STD3/DNS-length failure, plus separate
structural/classification checks for empty labels and label-vs-name length.
The public token names use rurl's kebab-case diagnostic style (matching the
shipped `ipv4-*` convention), not punycoder's internal `snake_case` naming, so
the dependency's naming can change without breaking rurl's public diagnostics
contract.

### 5.4 DNS length / IDNA-UTS-46 diagnostics

See D3/D4 (corrected 2026-07-05 after live verification against `punycoder`;
**corrected a second time, same day, after a Codex review caught the first
probe design was itself wrong** - see below). Implementation shape:

- New seam function, e.g. `.punycoder_host_probe(host)`, mirroring `.psl_*`
  (R/domain.R). **Not** a thin wrapper around `validate_domain()` - verified
  live that its `error_codes` are not independent per-check facts (a domain
  that is both too-long and STD3-invalid returns only one code, and
  `strict = FALSE` doesn't recover the missing fact, it just returns none).
- **Corrected probe design (Codex review, 2026-07-05).** The first draft of
  this section proposed calling `host_normalize()` from an all-strict
  baseline, relaxing one flag at a time, and reading which calls flip
  NA-to-pass. **Verified live that this is wrong**: for a host failing both
  `use_std3` and `verify_dns_length` (2 of 3 checks), every single-flag-relax
  call still returns `NA` (relaxing one flag still leaves the other failing
  check active) - and this exact same all-`NA` pattern is *also* what a host
  failing **all three** checks produces. The two cases are indistinguishable
  under that design; relaxing one flag while the others stay enforced
  conflates "is this flag not the (sole) problem" with "is some other flag
  also a problem," which is not resolvable from an OR-style probe once 2+
  checks fail simultaneously.
  The correct design **inverts the direction**: start from all three flags
  relaxed (`check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length =
  FALSE`) as the baseline, then re-enable exactly one flag at a time (holding
  the other two relaxed) and test each in isolation - each such call has only
  one active constraint, so its NA/pass result is an unambiguous, independent
  read on that one check, regardless of how many other checks are failing.
  Verified live this correctly distinguishes the 2-of-3-failing host from the
  3-of-3-failing host, where the original design produced identical (and
  therefore useless) output for both.
- **Baseline guard:** only interpret the three isolated flag calls when the
  all-relaxed baseline itself returns non-`NA`. If the all-relaxed call still
  returns `NA`, the host has a structural problem outside the three toggled
  checks (for example an empty label), and comparing the isolated calls would
  misclassify that structural failure as hyphen/STD3/length failure. In that
  case, skip the flag-derived diagnostics and hand the host to the separate
  structural detectors below.
- **`domain-empty-label` needs a separate detector, not this probe.** Verified
  live that `host_normalize("a..com", check_hyphens = FALSE, use_std3 =
  FALSE, verify_dns_length = FALSE)` still returns `NA` even with all three
  optional checks relaxed - empty-label rejection is a structural check none
  of the three flags controls. This diagnostic needs its own detection (e.g.
  a direct check for zero-length labels in the split host, done in rurl, or a
  scoped `validate_domain()` call used only for this one fact) rather than
  being folded into the flag-toggling probe.
- **Length subtyping also needs a separate classifier.** The flag probe can
  prove that DNS length validation failed, but it cannot distinguish a
  too-long label from an overall too-long domain name. If rurl keeps separate
  `domain-label-too-long` and `domain-name-too-long` diagnostics, that split
  must come from either a small rurl-owned length classifier over labels/name
  length or a scoped `validate_domain()` classification call, not from the
  NA/pass flag probe alone.
- Runs under **both** `url_standard` values (per the resolved D4/Open
  Question 3 - facts fire regardless of selected standard), not
  `whatwg`-only.
- Must not affect `.normalize_and_punycode()` / `.punycode_to_unicode()` or
  any `host_encoding` output - probe results are diagnostics-only, never fed
  back into rendering.
- Performance note: the corrected design is still up to 4 `host_normalize()`
  calls per host (all-relaxed baseline + 3 single-flag-enabled calls) plus
  whatever the empty-label detector costs, all more expensive than a single
  `validate_domain()` call. Worth checking whether this needs to be gated
  behind the diagnostics helpers being explicitly invoked (already lazy -
  `get_url_diagnostics()`/`get_host_type()` are opt-in calls, not part of
  `safe_parse_urls()`'s default output per v1 §6.3) rather than assuming it's
  free to always compute.

### 5.5 `case_handling`

See D5 (corrected 2026-07-05: only `"lower_host"` passes under a selector,
not `"lower"` - `"lower"` also lowercases path per
`R/parse-phases.R:887`, which neither spec sanctions). Purely a
conflict-matrix addition to the existing v1 machinery
(`.URL_STANDARD_PROFILES` / conflict checkers in `R/parse.R`, per T1). No new
public option, no new normalization code.

### 5.6 `resolve_url()`

New function:

```r
resolve_url(relative_or_absolute, base_url, url_standard = NULL, ...)
```

- Shares the base-merge algorithm (standard-agnostic) across both profiles.
- Delegates dot-segment/percent handling, host model, backslash handling, and
  port elision to the same per-standard logic used elsewhere, keyed by the
  same `url_standard` value.
- Should land after D1/D2/D4 exist, since it composes them rather than
  introducing new divergent behavior itself.

### 5.7 Scheme table / `scheme_class`

See D7. Internal lookup only maps rurl's existing allowed schemes
(`http`/`https`/`ftp`/`ftps`) to special-vs-not under `whatwg`; does not grow
the allowlist. `scheme_class` companion helper follows the `get_host_type()` /
`get_url_diagnostics()` shape from T2.

## 6. Explicitly out of scope for this epic (see §3 for full non-goals)

Tracked here only as a reminder these were considered and deferred further,
not forgotten: `max_url_length` guards, URLPattern-style grouping,
`explain_parse_url()`, `unsupported_scheme` policy, Ada backend, `ws`/`wss`/
`file` scheme expansion.

## 7. Open questions

1. ~~`validate_domain()` code independence.~~ **Resolved (Codex review,
   2026-07-05, verified live): false.** `validate_domain()` does not emit
   independent per-check codes - a combined too-long+STD3-invalid domain
   returns only one code. Superseded by the `host_normalize()` flag-toggling
   probe design in D3/§5.4 - which itself needed a second round of correction
   (a second Codex review caught that the first replacement design was also
   ambiguous on 2+ simultaneous failures; the inverted all-relaxed-then-
   enable-one-at-a-time design in §5.4 is the one verified live to actually
   work).
2. **Forbidden-host-code-point equivalence.** Still open. Confirm whether the
   `use_std3` check inside `host_normalize()`'s probe (§5.4) covers WHATWG's
   specific forbidden-host-code-point set (control chars, space,
   `# % / : < > ? @ [ \ ] ^ |`) byte-for-byte, a subset, or a differently-scoped
   check (it may run post-processing on the domain rather than pre-processing
   on the raw host). Do not assume equivalence when writing fixtures.
3. ~~Should DNS-length/UTS-46 diagnostics be available under `rfc3986` too?~~
   **Resolved into D4:** yes, both modes, facts-not-policy.
4. Exact naming for the new port/backslash diagnostic tokens - **partially
   resolved** in §5.3 (`explicit-default-port`, `non-default-port`,
   `invalid-reverse-solidus`); DNS-length/UTS-46 token names in §5.3 are still
   provisional pending question 2 above.
5. Whether `port_handling` (D1's standalone knob) should be added now
   alongside the `url_standard`-governed default-port elision, or ticketed
   separately since it doesn't require `url_standard` at all.
6. **New (from the D3 correction):** is a 4x `host_normalize()` call per host
   (all-relaxed baseline + 3 single-flag-enabled calls), plus separate
   structural/classification checks for empty labels and label-vs-name length,
   an acceptable cost for the diagnostics helpers, or does the probe need a
   cheaper implementation? Needs a benchmark against realistic crawl-export
   volumes before D4 is ticketed.
7. **New (from the port wording fix):** is `port_handling = "keep"` producing
   different output depending on `url_standard` (verbatim under `rfc3986`/no
   selector, default-port-elided under `whatwg`) confusing enough that it
   needs a clearer name (e.g. splitting into `"keep"` = verbatim always vs.
   letting `url_standard` govern elision only through a different path), or
   is documenting the dependency sufficient? Leaning: documentation is
   sufficient, since the elision-under-`whatwg` behavior is the same kind of
   `url_standard`-conditional behavior every other governed knob already has -
   but flagging in case it reads as surprising during implementation review.

## 8. Recommended next steps

1. Resolve open questions 2 and 6 before scoping D3/D4 into a ticket - they
   determine the diagnostic probe's exact behavior and cost.
2. File child tickets under a new umbrella epic, roughly in the priority order
   from the design conversation: (a) port default-elision + standalone
   `port_handling`, (b) backslash-as-slash, (c) DNS-length/UTS-46 diagnostics
   seam, (d) `case_handling` conflict-matrix fix (smallest, could ship first
   as a quick win), (e) scheme table + `scheme_class`, (f) `resolve_url()`
   (depends on a-b-c-e existing first).
3. Keep `max_url_length`, URLPattern grouping, and `explain_parse_url()` as
   separate, independently-schedulable tickets, not children of this epic.
