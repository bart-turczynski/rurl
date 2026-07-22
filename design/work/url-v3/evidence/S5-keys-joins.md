# S5 — Comparison keys and URL joins

**Audit status:** protocol reconstruction is incomplete for this slice  
**Audit scope:** comparison-key and URL-join reconstruction only  
**Evidence date:** 2026-07-21  
**Protocol reviewed:** `_scratch/url-v3-spec-reconstruction-protocol.md`

## Executive verdict

The protocol finds the correct architectural fault line but does not yet cross it. It correctly says that `get_clean_url()` is a deliberately lossy SEO/cleaning convenience, that comparison must be independent of cleaning and display, that v3 needs a non-URL `get_url_key()` projection, and that `canonical_join()` must no longer inherit equality implicitly from `clean_url` (`§6.4`–`§6.5`, lines 153–175). Those are necessary conclusions.

They are not an implementable comparison or join specification. The protocol supplies names and a risk checklist, but it does not define:

- the key's type, version, field framing, missing/invalid representation, vector contract, or defaults;
- a component-by-component equivalence relation for scheme, authority/host, port, path, query, userinfo, and fragment;
- the exact meaning of the three named scheme policies;
- the interaction between scheme equivalence and default-port equivalence, especially for missing schemes;
- row retention, row order, cardinality, invalid/`NA` behavior, key visibility, suffix repair, and error behavior for the six proposed joins;
- how current `canonical_join(..., ...)` cleaning dials migrate, deprecate, or map to explicit key policy; or
- a compatibility boundary between shipped `canonical_join()` results and the proposed join family.

This is a release-blocking reconstruction gap, not merely missing prose. Two independent implementations can satisfy every bullet in protocol `§6.5` and disagree on both which URLs compare equal and which rows/columns a join returns.

There is also a concrete current-versus-accepted contradiction. Shipped `canonical_join()` uses `parsed$clean_url` as its key (`R/canonical_join.R:241-247`). Therefore every cleaning/profile/display dial forwarded through `...` is currently an equality dial. Accepted ADR 0011 calls `path_encoding = "encode"/"decode"` lossy presentation (`design/adr/0011-path-encoding-orthogonal-presentation.md:85-90`), and accepted ADR 0012 says that presentation can make `clean_url` non-conformant and must never feed the standard serializer used for identity comparisons (`design/adr/0012-general-url-parser-scope.md:262-282`). The protocol agrees (`§6.4`–`§6.5`). The source and tests still preserve the old coupling.

**Verdict:** the protocol is sufficient to direct the next design step, but insufficient to reconstruct or approve the v3 key/join API. Before implementation, an owner must accept a normative key-policy matrix, a normative join-semantics matrix, and a migration rule for `canonical_join()`. This report provides the required decision surface below without treating recommendations as already accepted requirements.

### Evidence classification used in this report

| Label | Meaning |
|---|---|
| **SHIPPED** | Current exported API, implementation, generated docs, and executable tests. |
| **ACCEPTED** | An accepted ADR decision. It may describe an intended invariant that the current join path does not yet satisfy. |
| **HISTORICAL** | A PRD, brainstorm, issue audit/import artifact, NEWS item, or older compatibility rationale. Useful evidence, but not automatically v3 normative. |
| **PROPOSED** | The v3 reconstruction protocol. The protocol explicitly describes itself as a reconstruction/review process rather than the product API specification (`protocol:3-5`). |
| **CONFLICT** | Two authority classes prescribe incompatible behavior or use the same term for different contracts. |

## Coverage map

| Required surface | Protocol coverage | Current evidence | Audit judgment |
|---|---|---|---|
| Cleaning versus comparison separation | Explicit in `§6.4`–`§6.5` | `canonical_join()` still keys on `clean_url` | Correct direction; unresolved shipped/accepted contradiction. |
| Public comparison projection | Names `get_url_key()` and says its value is not a URL | No export, implementation, manual page, or tests; absent from `NAMESPACE` | Name and principle only; no callable contract. |
| Key policy | Requires explicit scheme/port/path/query policies | Current policy is the full `get_clean_url()` option surface | No defaults, argument shape, state model, or equivalence tables. |
| Scheme equivalence | Names exact, HTTP/HTTPS, and HTTP/HTTPS/missing modes | Shipped parser has `scheme_policy = infer/require`; cleaning has protocol presentation controls | Comparison policy is not distinguished operationally from parse acceptance/inference/presentation. |
| Port equivalence | Says HTTP 80 and HTTPS 443 are equivalent to absent; valid non-default ports remain valid | Default `port_handling = "exclude"` removes every port from current key | Requirement exists, but scheme/missing/unknown-scheme truth table is absent. |
| Missing scheme | Says not to guess an effective port | Shipped `scheme_policy = "infer"` can manufacture `http` before key assembly | Critical interaction is recognized but not specified. |
| Host equivalence | Protocol asks broad host/state reconstruction elsewhere | Current key varies with host case, encoding, `www`, subdomain trimming, PSL source/engine, standard/profile | No comparison-specific host policy or host-kind matrix. |
| Path equivalence | Protocol prohibits display strings as equality | Current `clean_url` path is identity processing followed by selected presentation | Dedicated comparison serialization and reserved-percent rules absent. |
| Query equivalence | Protocol lists query as a policy dimension | Shipped cleaner can drop/filter/allow/keep, sort, decode plus, change empty handling, and collapse some states | No comparison defaults or presence/duplicate/order/bare-key contract. |
| Userinfo and fragment | Protocol says full URL-state review; display/cleaning section allows lossy behavior | Current `clean_url` excludes both, so joins ignore both | Equality policy not stated for either component. |
| Invalid/`NA` keys | Named as a join concern | Current keep mode gives invalid rows private sentinels, outputs `JoinKey = NA`, and never matches them | Protocol has no key eligibility or join policy matrix. |
| Duplicate/cardinality behavior | Names duplicate policy and relationship validation | Current `collision = first/all/error` is per-side preprocessing, not a relationship contract | Topic only; semantics and error timing absent. |
| Join family | Names inner/left/right/full/semi/anti and says “dplyr-shaped” | Only exported `canonical_join()`; base `merge()` implements four joins | Function signatures and row/column semantics absent. |
| Output schema | Names key visibility, suffix policy, ordering, and collisions | Current output is `JoinKey`, both URL columns, then suffixed payload; base/data-frame repair leaks through | No normative schema, order, name repair, or zero-row prototype. |
| Migration/compatibility | Says reassess `canonical_join()` | README, manual, architecture, tests, and historical PRDs call `clean_url` a join/canonical key | No deprecation period, compatibility mode, warning, or versioning plan. |
| Minimum conformance tests | Protocol includes useful fixture categories (`§8`) | Join tests cover only the current four-way helper and selected cleaning forwarding | No key conformance ledger or six-join contract suite. |

## Current `canonical_join()` dataflow and dial inventory

### Shipped dataflow

The current dataflow is:

```text
canonical_join(data_A, data_B, col_A, col_B, ...)
  -> match join/collision/on_parse_error/join_parse_status
  -> collect every `...` argument and run only the profile/standard conflict precheck
  -> validate data-frame inputs and URL columns (warning-and-empty on many failures)
  -> safe_parse_urls(data_A[[col_A]], ...)
  -> safe_parse_urls(data_B[[col_B]], ...)
  -> .cj_side_state(): key = parsed$clean_url; derive status eligibility
  -> .cj_resolve_sides(): error/drop invalids; first/all/error duplicate policy
  -> .cj_build_join_df(): replace retained invalid keys with side-private sentinels
  -> base::merge(..., by = ".cj_join_key", all.x/all.y, sort = FALSE)
  -> .cj_assemble_output(): coalesce visible `JoinKey`; bind URL and payload columns
```

Exact source evidence:

- Public formals are only the four base-style joins, per-side suffixes/names, `collision`, `on_parse_error`, `join_parse_status`, and `...` (`R/canonical_join.R:92-100`).
- Parse options are forwarded to `safe_parse_urls()` for both inputs (`R/canonical_join.R:108-142`).
- The match is a base `merge()` with `sort = FALSE` (`R/canonical_join.R:148-157`).
- The equality key is exactly `parsed$clean_url` and must be non-`NA`, non-empty, and status-eligible (`R/canonical_join.R:241-247`).
- `on_parse_error = "error"` errors for any row outside the chosen join-status set, so under the default `join_parse_status = "ok"` a warning status is treated as an “error” despite the argument name (`R/canonical_join.R:193-201`).
- `collision = "first"` retains the first occurrence independently on each side, while `"all"` permits the base-merge Cartesian product (`R/canonical_join.R:203-217,259-267`).
- Invalid rows retained by `on_parse_error = "keep"` receive unique side-tagged sentinel match keys so neither invalids nor `NA` values match, although the visible output key is `NA` (`R/canonical_join.R:269-289`).
- Output assembly always exposes `JoinKey`; it has no key-visibility policy (`R/canonical_join.R:291-320`).

The generated manual mirrors this implementation (`man/canonical_join.Rd:7-83`). `NAMESPACE` exports `canonical_join` but no `get_url_key` or URL join family.

### Equality dials inherited through `clean_url`

Because `.cj_side_state()` takes `parsed$clean_url`, every option capable of changing `clean_url` changes equality. This remains true even where docs call an option “presentation.” `canonical_join()`'s unrestricted `...` also means its comparison API is not discoverable from its own formals.

| Dial group | Current knobs reaching equality | Current effect/risk |
|---|---|---|
| Profile/standard | `profile`, `url_standard`, `scheme_acceptance` | A profile expands to a governed parse/cleaning bundle; standard selection changes host/path/port parsing and identity processing. Profile resolution and explicit-argument precedence happen before parse (`R/accessors.R:255-362`, `R/profiles.R`). |
| Scheme acceptance/inference | `scheme_policy = infer/require`, `scheme_acceptance` | Controls whether scheme-less host-shaped input is accepted/inferred and which schemes are joinable. This is parse eligibility, not the proposed scheme-equivalence policy. |
| Scheme presentation | `protocol_handling = keep/none/strip/http/https` | Can retain, remove, or force the scheme in `clean_url`, and therefore currently collapses or separates rows. It must not be mistaken for the three proposed comparison modes. |
| Host rewriting | `www_handling`, `subdomain_levels_to_keep`, `domain_source`, `engine`, `case_handling`, `host_encoding` | Can strip/add host spelling, remove subdomains using PSL-derived boundaries, fold case beyond host-only modes, or switch Unicode/punycode presentation. PSL source/engine becomes an equality dependency when subdomain trimming is used. |
| Path rewriting | `trailing_slash_handling`, `index_page_handling`, `path_normalization`, `case_handling`, `path_encoding` | Can remove slash/index, resolve dot segments, fold path case, and encode/decode presentation. `.normalize_path_vec()` explicitly runs internal identity then public presentation (`R/parse-phases.R:1389-1546`). |
| Query rewriting | `query_handling`, `params_keep`, `params_drop`, `params_case_sensitive`, `sort_params`, `empty_param_handling`, `decode_plus` | Default drops the query. Opt-in modes reparse and reserialize pairs, filter/sort keys, alter empty handling, and choose form-style plus decoding. Therefore all currently define equality (`R/parse-phases.R:2255-2290`; `R/path-query.R`). |
| Port rewriting | `port_handling = exclude/keep/strip_default`, plus `url_standard` | Default `exclude` removes every explicit port, including non-default ports. `strip_default` removes a port only when a known scheme table supplies the same default; `keep` emits it (`R/parse-phases.R:2331-2406`). |
| Scheme-relative handling | `scheme_relative_handling` | Can retain/interpret `//host` presentation differently and thereby affect whether a buildable clean string exists and what it contains. |
| Cleaning profile | `profile = browser/whatwg/rfc-syntax/seo/canonical` (as available in `R/profiles.R`) | Bundles several of the preceding axes. A profile change is therefore currently a wholesale equality-policy change. |
| Always omitted from clean key | userinfo, fragment | Two inputs differing only in credentials or fragment currently compare equal. This is not exposed as an explicit comparison decision (`R/parse-phases.R:2424-2433`; README:96-104). |

### Important present-state details

1. **Cleaning is not a faithful structural serializer.** `.build_clean_url_vec()` concatenates selected scheme, host, rendered port, rendered path, and optionally a non-empty query (`R/parse-phases.R:2375-2406`). It is doing its documented product job, not supplying a framed non-URL equality key.
2. **Absent and present-empty query are not safely distinguishable through the current key.** Accepted ADR 0012 requires internal `query_kind`/`fragment_kind` states (`:262-268`), but the public query path and current clean assembly still omit an empty query unless the richer state is preserved and deliberately serialized.
3. **Default port handling is currently unsafe for identity.** With the default `port_handling = "exclude"`, `http://example.com/` and `http://example.com:8080/` receive the same `clean_url`.
4. **Path presentation can change equality.** Under non-WHATWG `path_encoding = "encode"` or `"decode"`, full decoding precedes output encoding, so a reserved `%2F` can collapse into `/`; this is permitted for display by ADR 0011 but not for identity.
5. **Invalid rows are retained but intentionally never matched only in `keep` mode.** The visible `JoinKey = NA` hides private sentinels used for matching. There is no public representation that distinguishes invalid, missing, empty, and ineligible keys.
6. **Column repair is incidental.** Internal names, caller payload names such as `JoinKey`, duplicate input names, URL-column name collisions, and empty/equal suffixes are left to a mixture of `merge()`, `data.frame()`, `unique()`, and base name repair. The manual does not define the result.
7. **Ordering is not a contract.** `sort = FALSE` avoids lexical key sorting, but no documentation or test guarantees matched expansion order, unmatched placement, or stability across join modes.

### Minimal probes of shipped behavior

These were run against the loaded package during the audit; they are observations, not proposed v3 requirements.

| Inputs/options | Observed current key/join behavior |
|---|---|
| `http://u:p@example.com/a#x`, `http://example.com/a#y` | Same default clean key: userinfo and fragment ignored. |
| `http://example.com:8080/a`, `http://example.com/a` | Same default clean key because all ports are excluded. |
| `example.com/a`, `http://example.com/a` | Same default clean key under scheme inference. |
| `/a%2Fb`, `/a/b` with default path presentation | Distinct in the tested default path mode. |
| `/a%2Fb`, `/a/b` with `path_encoding = "encode"` | Collapsed in the tested non-WHATWG rendering path. |
| `?flag`, `?flag=` with query keep | Both serialize as `flag=` and compare equal. |
| Duplicate key on both sides with `collision = "all"` | Cartesian rows are returned. |
| Invalid or `NA` rows with `on_parse_error = "keep"` | Retained as unmatched outer rows, never matched to each other, visible key `NA`. |

## Severity-ordered findings

### BLOCKER 1 — `get_url_key()` has no normative data or vector contract

**Protocol section:** `§6.5` (`protocol:165-175`), supported by the state/serialization warnings in `§6.3`–`§6.4` (`:124-163`).

**Exact evidence:** the protocol states only that `get_url_key()` is “a non-URL comparison projection, independent of display and cleaning,” then lists scheme and port policy topics (`:167-172`). No subsequent section defines the function's formals, return type, key version, length/name rules, field encoding, eligibility states, error behavior, or default policy. `NAMESPACE` contains no such export. The protocol itself says it is not the future API specification (`:3-5`).

**Minimal example:** given

```r
u <- c("https://example.com/a?", "https://example.com/a")
get_url_key(u)
```

the protocol does not determine whether the two outputs are equal, distinct strings, opaque records, or `NA`; whether names are preserved; or whether a present-empty query participates by default.

**Consequence:** no fixture can assert a unique expected result, no ordinary data-frame join can depend safely on the output, and a delimiter-based implementation may itself collide when component values contain the delimiter. “Non-URL” prevents one category error but does not specify a safe key.

**Concrete correction:** add a normative comparison-key section that defines:

1. the exported signature and vector recycling/name rules;
2. a typed/versioned key representation or an injectively framed scalar representation;
3. an explicit eligibility result for missing, invalid, warning, and unsupported inputs;
4. all default component policies from the key-policy matrix below;
5. a guarantee that cleaning and presentation knobs cannot change a key unless supplied through an explicitly named comparison policy; and
6. golden examples plus collision tests proving distinct field tuples cannot produce the same key accidentally.

### BLOCKER 2 — “dplyr-shaped” does not specify the six joins

**Protocol section:** `§6.5` (`protocol:173-175`).

**Exact evidence:** the protocol names inner, left, right, full, semi, and anti joins and lists duplicate, relationship, invalid, unmatched, key visibility, suffix, ordering, collision, and error topics (`:173-175`). It supplies no function names/signatures, result schema, row multiplicity, order, relationship enum, unmatched behavior, `NA` behavior, or dependency/version target. Shipped code exposes one base-R-oriented `canonical_join()` with only four join modes (`R/canonical_join.R:92-100`; `NAMESPACE`).

**Minimal example:** A has two rows for key `k`; B has three. The protocol does not determine whether a proposed left join returns six rows, one row, an error, or a warning by default; whether `relationship = "one-to-one"` is available; which matching row is first; whether a semi join keeps one or two A rows; or whether the comparison key is visible.

**Consequence:** the join family cannot be implemented or tested consistently. Merely copying current `collision = "first"` would silently discard data and is not equivalent to relationship validation. Merely calling `merge(sort = FALSE)` would not create a documented dplyr-compatible contract.

**Concrete correction:** adopt a named public family and the join-semantics matrix below. Pin multiplicity, stable row order, column selection/order, `NA` and invalid policies, relationship validation and timing, unmatched policy, suffix/name repair, zero-row prototypes, and whether semantics track a specified dplyr version or are an independent rurl contract inspired by it.

### HIGH 3 — Shipped joins consume a lossy display/cleaning representation despite accepted identity rules

**Protocol section:** `§6.4`–`§6.5` (`protocol:142-175`).

**Exact evidence:**

- Protocol: display strings must not be serialization or equality substrates (`:142-151`); cleaning may be intentionally lossy (`:153-163`); comparison must be independent (`:165-175`).
- Accepted ADR 0011: path identity and path presentation are independent (`design/adr/0011-path-encoding-orthogonal-presentation.md:21-27,49-62`); encode/decode are lossy presentation (`:85-90`).
- Accepted ADR 0012: explicit path presentation may make `path`/`clean_url` non-conformant and “never feeds ... the standard serializer used for identity comparisons” (`design/adr/0012-general-url-parser-scope.md:262-282`).
- Shipped source: `.cj_side_state()` assigns `key <- parsed$clean_url` (`R/canonical_join.R:241-247`).
- Shipped test: `tests/testthat/test-url-standard-path-encoding-orthogonal.R:204-216` explicitly verifies that `canonical_join()` forwards `path_encoding` through `...`, thereby preserving presentation-driven equality.

**Minimal example:** `/a%2Fb` and `/a/b` can be distinct identity paths because `%2F` is data in one spelling and a separator in the other. A lossy full-decode/re-encode presentation can emit the same path for both. Current `canonical_join(..., path_encoding = "encode")` may then join them.

**Consequence:** a documented display choice can produce false joins. This violates the accepted identity/presentation split and makes join results change when a user asks only for more readable or encoded output.

**Concrete correction:** build `get_url_key()` from the internal structural/identity record after standard interpretation but before any cleaning/display transform. Add invariance tests showing that `path_encoding`, `host_encoding`, protocol presentation, case presentation, and other display-only settings do not change keys. Deprecate join-time forwarding of display arguments or accept them only for separately named output columns, never matching.

### HIGH 4 — Scheme and port equivalence is named but not closed under all input states

**Protocol section:** `§6.5` (`protocol:168-172`) and fixture requirements in `§8` (`:228-233`).

**Exact evidence:** the protocol names `exact`, HTTP/HTTPS equivalence, and HTTP/HTTPS/missing equivalence; states that HTTP 80 and HTTPS 443 should be equivalent to default absence; preserves valid non-default ports; and warns not to guess an effective port for a missing scheme (`:168-172`). It does not say:

- which policy is default;
- whether scheme matching uses parsed, original, inferred, or presentation scheme;
- whether case is folded before exact comparison;
- what `//example.com`, `example.com`, `http:example.com`, and an RFC relative reference mean;
- whether `http://x:443` or `https://x:80` is default (they are not, but the required algorithm is unstated);
- which defaults apply to `ftp`, `ws`, `wss`, custom schemes, or non-special URLs;
- how an explicitly written port is represented when a scheme is missing; or
- whether unsupported-but-valid schemes produce keys.

Shipped behavior is materially different: `scheme_policy = "infer"` can manufacture `http`, and default `port_handling = "exclude"` drops both default and non-default ports (`man/get_clean_url.Rd:244-257`; `R/parse-phases.R:2331-2406`).

**Minimal example:** compare `example.com:443/a`, `http://example.com:443/a`, and `https://example.com/a` under the proposed HTTP/HTTPS/missing mode. “Do not guess” rules out one shortcut, but the protocol does not specify the key fields or pairwise results.

**Consequence:** implementations will disagree at exactly the boundary intended to prevent false joins. A careless scheme collapse followed by default-port stripping can treat `http://x:443` as equivalent to HTTPS absent-port; a careless inference step can erase missing-scheme provenance.

**Concrete correction:** accept a scheme/port truth table covering explicit HTTP/HTTPS, missing, scheme-relative, inferred, unsupported/custom, absent/default/non-default port, and mismatched well-known ports. Define comparison on original presence plus parsed effective values, not on display strings. The key-policy matrix below identifies the required rows.

### HIGH 5 — Host, path, query, userinfo, and fragment equivalence defaults are absent

**Protocol section:** `§6.3`–`§6.5` and `§8` (`protocol:124-175,208-237`).

**Exact evidence:** the protocol correctly demands absent/present-empty/present state, prohibits display equality, and lists rich fixtures. It never states which distinctions `get_url_key()` preserves by default or which named policies are available. Current cleaner behavior cannot fill the gap:

- host equality may vary with Unicode/punycode presentation, case, trailing-dot handling in the parsing model, IP normalization, `www`, subdomain/PSL rewriting, and selected standard/profile;
- path equality may vary with dot segments, unreserved percent normalization, reserved separators, backslashes, repeated slashes, empty versus `/`, case, and invalid escapes;
- query equality may vary with absent versus present-empty, order, duplicate position, bare key versus empty value, percent spelling, plus semantics, empty pairs, filtering, and tracking parameters;
- userinfo and fragment are always excluded from current `clean_url`.

Accepted ADR 0012 explicitly retains internal query/fragment presence states (`:262-268`) and says semantic cleaning transforms are scheme-scoped (`:275-282`), reinforcing that the cleaning output is not a default comparison spec.

**Minimal example:** current query keep normalizes `?flag` and `?flag=` to the same rendered pair. The protocol does not say whether the v3 comparison policy preserves their structural difference, deliberately equates them, or ignores query entirely by default. Likewise it does not say whether `https://x/a#one` equals `https://x/a#two`.

**Consequence:** comparison semantics remain accidental. Adopting current cleaner defaults would preserve false joins (non-default port, query drop, display collapse); adopting strict byte equality would contradict intended standard normalization and scheme policies.

**Concrete correction:** decide every row in the key-policy matrix below, including default and opt-in policies. Require component-level fixtures and cross-product tests; do not define equality as “whatever the chosen profile emits.”

### HIGH 6 — There is no migration contract for the current equality-dial surface

**Protocol section:** `§6.4`–`§6.5` (`protocol:153-175`).

**Exact evidence:** the protocol says cleaning profiles should remain independently useful and calls for reassessment of `canonical_join()` (`:153-175`). Shipped docs promise the opposite coupling:

- README calls `clean_url` a “normalized canonical key” and says query filtering shapes `canonical_join()`'s join key (`README.md:96-104,187-216,249-258`).
- `man/get_clean_url.Rd:214-216` explicitly says case handling affects the `canonical_join` key; `:195-257` documents query and port rendering as clean-key controls.
- `ARCHITECTURE.md:151-179` calls `clean_url` the join/identity contract.
- `man/canonical_join.Rd:74-83` promises all parser/cleaning options, including presentation knobs, through `...`.
- Historical query PRD says query handling intentionally enters `canonical_join()` equality (`_scratch/PRD-query-param-handling.md:175-189,279-285`).

No protocol section says whether v3 `canonical_join()` changes in place, freezes as legacy, delegates to a new family, warns for cleaning arguments, or exposes an explicit legacy policy.

**Minimal example:** a user currently calls `canonical_join(A, B, query_handling = "filter", params_drop = "utm_source")`. Under v3, should that call keep its shipped result, warn and translate to a key query policy, error because cleaning dials are no longer comparison dials, or clean both input URL display columns but join on an independent default key? The protocol does not decide.

**Consequence:** even a correct new key API cannot be integrated without either silent behavior change or indefinite preservation of the architectural bug. Documentation and downstream expectations will conflict.

**Concrete correction:** add a migration table for every current `canonical_join()` formal and forwarded cleaning option. Recommended shape: keep `canonical_join()` for a documented deprecation window; make legacy equality explicit (for example a named legacy policy rather than implicit `...`); introduce the six new joins on `get_url_key()`; warn on comparison-irrelevant cleaning/display dials; and provide an audit helper or dual-key comparison so users can quantify changed matches before switching.

### HIGH 7 — Invalid, missing, warning, duplicate, and relationship policies are not a coherent model

**Protocol section:** `§6.5` (`protocol:173-175`).

**Exact evidence:** the protocol lists these concerns but supplies no state machine. Shipped behavior combines several unrelated concepts:

- `join_parse_status = "ok"/"ok_or_warning"` controls eligibility;
- `on_parse_error = keep/drop/error` controls ineligible row retention;
- `collision = first/all/error` preprocesses duplicate keys independently per side;
- invalid retained rows receive private sentinels and visible `JoinKey = NA`;
- `on_parse_error = "error"` can error on warnings under the default status policy (`R/canonical_join.R:193-217,241-289`).

There is no relationship argument, no unmatched policy, no distinction between missing input and invalid parse, and no explicit `NA` matching policy.

**Minimal example:** A contains one missing URL, one invalid URL, and two valid rows with key `k`; B contains one missing URL and three `k` rows. The protocol does not determine which policies validate 2×3 cardinality, whether missing values match, whether invalids are returned by left/full/anti joins, or whether errors are reported before or after unmatched filtering.

**Consequence:** row loss, Cartesian expansion, and diagnostics depend on incidental preprocessing. `collision = "first"` is especially dangerous because it resolves a many-to-many condition by discarding rows rather than validating the caller's assertion.

**Concrete correction:** separate five axes: key eligibility (`ok`, warnings allowed, invalid, missing); missing-match behavior; invalid-row behavior; multiplicity behavior; and declared relationship. Validate relationship on eligible keys before materializing results, define whether unmatched validation is per side, and make “first match” an explicit lossy `multiple` option if retained at all—not the duplicate-policy default.

### MEDIUM 8 — Result schema, order, name repair, suffix behavior, and zero-row prototypes are unspecified

**Protocol section:** `§6.5` (`protocol:173-175`).

**Exact evidence:** these topics appear in the checklist but nowhere receive normative values. Current implementation constructs an internal key, calls base `merge(sort = FALSE)`, and manually rebuilds output (`R/canonical_join.R:148-157,221-236,269-320`). Tests verify a few current names and one empty structure (`tests/testthat/test-canonical_join.R:202-271`), but not exact order across every join, suffix collisions, duplicate payload names, a caller column named `JoinKey`, equal/empty suffixes, internal-name collisions, or type-stable zero-row output.

**Minimal example:** both inputs contain payload columns `JoinKey`, `.cj_join_key`, and `value`, with `suffix_A = suffix_B = ""`. Current name repair is incidental; the protocol does not define error, repair, or output names.

**Consequence:** callers cannot select columns robustly or rely on stable row order. Implementations based on base R, dplyr, or custom indexing will disagree while all remain “dplyr-shaped.”

**Concrete correction:** pin a result prototype for each join, reserve or avoid internal names safely, specify visible-key placement, require unique nonempty suffixes when collisions exist (or define deterministic repair), define type preservation, and test zero-row/zero-column/vector-length edge cases. State the stable ordering algorithm rather than only `sort = FALSE`.

### MEDIUM 9 — Authority records do not state which older “canonical key” claims are superseded

**Protocol section:** authority and contradiction handling in `§2`–`§3`, then `§6.4`–`§6.5` (`protocol:38-54,153-175`).

**Exact evidence:** the protocol correctly asks reviewers to distinguish shipped, accepted, historical, and proposed material, but the product docs still use `clean_url` as canonical identity (`README.md:96-104`; `ARCHITECTURE.md:156-179`; `man/get_clean_url.Rd:214-216`; `man/canonical_join.Rd:74-83`). Historical standard-selector PRDs and query PRD also reason about `clean_url` as the key (`design/prd/url-standard-selector.md:40-41,94-116,368-384`; `_scratch/PRD-query-param-handling.md:175-189`). Accepted ADR 0011 itself says consumers wanting a conformant canonical key use the default keep presentation (`:85-90`), while later ADR 0012 says lossy presentation never feeds identity (`:262-282`).

**Minimal example:** a reviewer can cite `ARCHITECTURE.md` to require clean-key equality and cite accepted ADR 0012 plus the v3 protocol to prohibit it. Both are repository authority records; only the reconstruction classification resolves them, not the current docs.

**Consequence:** design and test reviews can accidentally preserve historical behavior as v3 normative or treat proposed v3 behavior as already shipped.

**Concrete correction:** the eventual v3 specification must contain an explicit supersession ledger: current documentation describes 2.x/3.0-preexisting behavior; ADR 0011's presentation wording is retained but its clean-key consumer advice is superseded for comparisons; ADR 0012's internal-identity invariant is carried forward; historical PRDs remain compatibility rationale only. Update public docs only in the implementation slice, not in this audit.

## Required key-policy matrix

The following is the minimum matrix the owner must turn into normative specification. “Required decision” is intentionally not presented as already accepted. The recommendation column is the auditor's proposed safe baseline.

| Dimension | States/cases that must be distinguished | Existing evidence | Required decision | Recommended v3 baseline |
|---|---|---|---|---|
| API | scalar/vector; names; length zero; classed inputs; factors; list columns | Current accessors are vectorized; join currently accepts character/factor columns | Signature, recycling, names, output length/class, errors | `get_url_key(url, policy = url_key_policy())`; length-preserving, names-preserving, character/factor accepted with explicit coercion rules; no silent recycling of policy vectors. |
| Key representation | component tuples containing arbitrary bytes/delimiters | Protocol requires a non-URL projection | String versus record, framing, versioning, stability | A classed, versioned key with injective length-prefix/binary framing internally; printable representation for diagnostics only. Never construct by ambiguous delimiter concatenation. |
| Eligibility | OK, warning, invalid, unsupported scheme, relative reference, missing input, empty input | Current status gate is `ok` or `ok_or_warning` | Which states get a key and how reason is exposed | Return key plus eligibility/status metadata internally; public vector uses `NA` only for non-keyable rows and exposes a companion diagnostic path. Do not conflate missing with invalid. |
| Standard interpretation | `rfc-syntax`, `whatwg`, legacy/no selector; parse fixups | Profiles currently mix interpretation and cleaning | Which standard is default for keys; whether fixups affect identity | Explicit key-policy field with a stable default selected by owner; interpretation may affect component identity, but cleaning profile never does. |
| Scheme source | explicit, inferred, missing, scheme-relative, relative, malformed | Parser has `scheme_policy`; protocol names comparison modes | Whether comparison sees original presence, parsed effective scheme, or both | Preserve original scheme presence separately from parsed scheme. Comparison policy consumes those states; never infer solely to erase “missing.” |
| Scheme equality | exact; HTTP≈HTTPS; HTTP≈HTTPS≈missing | Protocol names all three | Names, exact pairwise relation, default, unsupported schemes | Define enum exactly and truth-table it. Under relaxed modes collapse only the named web-scheme states; all other valid schemes remain exact. Owner must choose default, preferably `exact` for safety. |
| Scheme case | spelling/case of scheme | Standards treat scheme case-insensitively | Whether “exact” means byte exact or semantic exact | “Exact” should mean same normalized scheme identity, not original letter case; retain raw spelling only for round-trip. |
| Port presence | absent, explicit default, explicit non-default, invalid, empty delimiter | Protocol mandates HTTP 80/HTTPS 443 equivalence and valid non-default retention | Field encoding and invalid behavior | Preserve explicit presence internally; compare absent with explicit 80 only under HTTP and absent with explicit 443 only under HTTPS. Non-default is always key-significant. |
| Port × relaxed scheme | `http:80`, `https:443`, `http:443`, `https:80`, missing `:80/:443`, scheme-relative | Protocol warns against guessing missing-scheme effective port | Full pairwise table | Normalize default port before optional scheme collapsing, using each row's explicit recognized scheme. For missing scheme, retain literal port and missing-scheme marker; do not label it default. |
| Other scheme defaults | ftp/ws/wss/custom | ADR 0012 has parser scheme table; protocol only commits HTTP/S | Whether key uses other standard defaults | Do not extend comparison equivalence beyond HTTP/S without an explicit policy/version. Custom scheme ports remain literal. |
| Authority presence | absent, empty, present | Protocol `§6.3`; ADR 0012 requires `authority_kind` | Whether states can compare | Preserve and frame the state. Never collapse absent and empty accidentally. Define per-scheme keyability. |
| Host kind | domain, opaque host, IPv4, IPv6, empty, absent | Accepted general-parser design distinguishes host kinds | Which normalizer applies to each | Key includes host kind. Apply DNS/IDNA rules only to eligible domain hosts, canonical IP forms to IP kinds, and scheme-appropriate exact rules to opaque hosts. |
| Domain spelling | ASCII case, Unicode/punycode, UTS46 mappings, trailing dot, empty labels | Current `host_encoding` is presentation; standard/engine affect parsing | Semantic equivalence and default | Compare the internal normalized domain identity, not Unicode/punycode display. Explicitly decide trailing-root-dot equivalence and test deviation characters. |
| Host editing | `www`, subdomain levels, PSL source/engine | Current cleaning knobs can alter key | Whether any belongs in comparison | Exclude from default comparison. If offered, put in an explicitly lossy `host_scope` comparison policy with versioned PSL dependency and diagnostics. |
| IP host | IPv4 alternate spelling, numeric/hex/octal/short forms, IPv6 compression/zone | Standards may interpret spelling differently | Interpretation and equivalence | Use selected standard's parsed IP identity; preserve host kind; reject/diagnose unsupported zones rather than pass through display. |
| Path presence/root | empty path, `/`, rootless, opaque body, repeated slash | Different scheme/standard models | Pairwise equality | Frame path kind and segment structure. Only equate empty and `/` where the selected standard/scheme identity explicitly does so. |
| Path normalization | dot segments, unreserved percent octets, percent hex case | ADR 0011 has internal `path_identity` | Default and standard mapping | Compare post-standard identity path before presentation. Pin percent normalization and dot-segment behavior per standard. |
| Reserved path bytes | literal `/` versus `%2F`, `\\` versus `%5C`, delimiter-like bytes | Historical brainstorm identifies false-join risk (`:523-530`) | Exact preservation rules | Never turn encoded reserved data into structural separators for comparison unless the selected standard's parse algorithm does so before the internal record. Add explicit regression fixtures. |
| Path display/editing | encode/decode, case fold, index removal, trailing slash, SEO cleanup | Protocol and ADRs call these cleaning/presentation | Whether any can reach key | None reaches the default key. Optional lossy comparison policies, if product-required, must be separately named and never reuse display arguments. |
| Query presence | absent, present-empty, present-nonempty | Protocol state model; ADR 0012 `query_kind` | Default query participation and presence equality | Preserve all three states in key material whenever query participates. Owner must choose default participation; safest identity baseline is exact structural query, while an SEO-specific policy may ignore/filter. |
| Query pair grammar | empty pair, bare key, empty value, repeated `&`, first `=`, invalid percent | Current ordered-pair cleaner drops/normalizes some distinctions | Raw versus parsed-pair comparison | Define a key query mode. An `exact` mode should compare standard-normalized serialized query while retaining presence; a pair mode must specify grammar and cannot silently substitute for exact. |
| Query order/duplicates | `a=1&b=2`, reordered pairs, duplicate keys and positions | Historical query PRD preserves ordered pairs | Order significant? duplicate order? stable sort? | Exact default preserves order and duplicates. Offer sort/filter only as explicit comparison policy; stable sort must retain duplicate relative order. |
| Query encoding | hex case, unreserved percent, reserved delimiters, UTF-8, malformed escapes | Current cleaner canonicalizes tokens | Normalization and error handling | Pin per selected standard. Never whole-string decode before parsing delimiters. Malformed escapes yield diagnostics and a stated keyability result. |
| Plus semantics | literal `+` versus `%20` versus `%2B` | Current `decode_plus` is a cleaning option; brainstorm flags risk | URL-query versus form semantics | Literal URL-query mode keeps `+` distinct from space and `%2B` according to standard serialization. Form semantics must be an explicit key policy. |
| Query tracking/filtering | drop/allow/filter params, case sensitivity | Current SEO cleaner supports this | Whether available for comparison | Not part of default identity. If retained, expose a versioned, explicitly lossy query comparison policy separate from `get_clean_url()` configuration. |
| Fragment | absent, empty, present value | Current clean key always excludes it; protocol demands presence fidelity generally | Included or ignored by default; empty distinction | Owner decision required. If URL-resource identity deliberately ignores fragments, state that explicitly and still keep structural state outside the key for diagnostics. |
| Userinfo | absent, empty, username/password variants | Current clean key excludes it | Included, ignored, or makes row ineligible | Owner decision required. For web-resource joins, recommended ignore with an explicit diagnostic/security note; never let display stripping silently decide. |
| Cleaner/profile invariance | every `get_clean_url()` dial and profile | Current join inherits all | Formal non-interference property | For fixed parse/standard and key policy, changing cleaning/profile/display-only options must leave the key byte-identical. Test each dial. |
| Missing/invalid key scalar | `NA`, empty input, invalid parse, warning-only parse | Current private sentinels exist only inside join | Public value and equality | Non-keyable values produce `NA` key plus reason. Default data-frame joins use never-match for missing/non-keyable unless caller explicitly opts into missing equality. |
| Stability/versioning | policy/default changes across releases | Current plain clean strings are unversioned | Backward compatibility and stored-key use | Include policy/version in class metadata and documented serialization. A default change requires a new version, never silent reinterpretation of persisted keys. |

### Required scheme/port truth-table skeleton

The protocol must fill this table for each of its three scheme modes. `?` means an owner decision is currently missing, not that arbitrary behavior is acceptable.

| Left state | Right state | `exact` | `http_https` | `http_https_missing` | Required rule |
|---|---|---:|---:|---:|---|
| HTTP absent port | HTTP `:80` | equal per protocol | equal | equal | Normalize recognized per-row default port. |
| HTTPS absent port | HTTPS `:443` | equal per protocol | equal | equal | Normalize recognized per-row default port. |
| HTTP absent port | HTTP `:8080` | distinct | distinct | distinct | Valid non-default remains significant. |
| HTTPS absent port | HTTPS `:80` | distinct | distinct | distinct | Do not strip another scheme's default. |
| HTTP `:443` | HTTPS absent | distinct | ? | ? | Scheme collapsing must not erase mismatched explicit port. Recommended distinct. |
| HTTP `:80` | HTTPS `:443` | distinct by scheme | ? | ? | If both scheme and each own default collapse, likely equal; must be explicit. |
| HTTP absent | HTTPS absent | distinct | equal | equal | Named scheme policy. |
| Missing scheme/no port | HTTP/no port | distinct | distinct | equal | Named missing-scheme policy, preserving provenance before comparison. |
| Missing scheme `:80` | HTTP/no port | distinct | distinct | ? | No effective-scheme guessing. Recommended distinct unless a separate explicit assumption policy exists. |
| Missing scheme `:443` | HTTPS/no port | distinct | distinct | ? | Same; recommended distinct. |
| Scheme-relative/no port | missing host-shaped/no port | ? | ? | ? | Decide whether scheme-relative is “missing” or a distinct reference kind. Recommended distinct unless resolved against a base. |
| FTP `:21` | FTP/no port | ? | ? | ? | Protocol only commits HTTP/S; recommended exact literal until extended. |
| Custom scheme `:123` | custom/no port | distinct | distinct | distinct | No guessed default. |

## Required join-semantics matrix

The table below is the minimum proposed normative shape. Owner approval is required, especially for stable ordering and dependency compatibility. It intentionally separates row semantics from key generation.

| Join | Retained input rows | Match multiplicity | Recommended stable order | Output columns | Invalid/missing default |
|---|---|---|---|---|---|
| `url_inner_join(x, y, ...)` | Only eligible matching x/y pairs | Cartesian expansion for duplicate matches unless relationship/multiple policy rejects or narrows | Preserve x row order; for each x row preserve matching y order | x columns, then non-key y columns with suffix handling; optional visible comparison key | Non-keyable rows do not match and are omitted; diagnostics available separately. |
| `url_left_join(x, y, ...)` | Every x row; eligible matches expand; unmatched x gets missing y payload | Same as inner for matches | Preserve x row order; preserve y match order within each x row | Same family prototype | Invalid/missing x retained as unmatched by default; invalid/missing y cannot match. |
| `url_right_join(x, y, ...)` | Every y row; eligible matches expand; unmatched y gets missing x payload | Same as inner for matches | Either a documented y-primary mirror or an explicitly pinned dplyr-compatible order; owner must choose | x/y column policy must remain consistent with family | Invalid/missing y retained as unmatched; invalid/missing x cannot match. |
| `url_full_join(x, y, ...)` | All matched pairs plus all unmatched rows from both sides | Same as inner for matches | Recommended: left-join result in x order, followed by unmatched y in y order | Same family prototype | Invalid/missing rows from each side retained separately and never match by default. |
| `url_semi_join(x, y, ...)` | Each x row whose eligible key has at least one eligible y match | Never duplicates x because y has multiple matches | Preserve x order | x columns only; key visibility only if already in x or explicitly requested without changing family expectations | Invalid/missing x excluded; invalid/missing y provides no match. |
| `url_anti_join(x, y, ...)` | Each x row with no eligible y match | One output per retained x row | Preserve x order | x columns only | Invalid/missing x retained by default because it has no match; this must be owner-confirmed. |

### Cross-cutting join decisions that must be normative

| Axis | Required choices | Recommended contract |
|---|---|---|
| URL columns | `by`, `col_x`/`col_y`, tidy-select or names; one or many URL columns | Start with one named URL column per side and explicit `by = c(x = "url_a", y = "url_b")`; do not overload arbitrary expression semantics in the first release. |
| Key policy | object or individual arguments; side-specific policy? | One immutable `url_key_policy` applied symmetrically. Side-specific equivalence rules are prohibited because equality must be symmetric/transitive. |
| Parse policy | standard, acceptance, warnings | Explicit shared parse/key policy. Cleaning profiles are not accepted for matching. |
| Relationship | none, one-to-one, one-to-many, many-to-one, many-to-many | Follow a documented relationship enum; validate on eligible keys before result materialization. Error includes representative keys/counts without leaking credentials. |
| Multiple matches | all, any/first/last, error | Default `all`. If lossy narrowing is offered, name it separately and define stable first/last order; do not reuse `collision`. |
| Duplicate definition | duplicates per side among eligible keys; invalid/NA included? | Only eligible, nonmissing keys enter cardinality counts. Missing/invalid counts are reported separately. |
| `NA` matching | never, equal, controlled | Default never. If an `na_matches` option exists, specify whether it applies only to missing input, never to invalid parse. |
| Invalid rows | keep unmatched, drop, error; warning eligibility | Separate `invalid = keep/drop/error` and `warnings = allow/reject/error`; rename away from misleading `on_parse_error`. |
| Unmatched rows | allowed, error by side | `unmatched = drop/error` or side-specific equivalent with exact timing; for outer joins clarify that “unmatched” may still be retained even when diagnosed. |
| Key visibility | never, keep, position/name | Default hidden or a collision-proof explicit name; `keep = TRUE` exposes a classed non-URL key, never a URL-looking `JoinKey` string. |
| Original URL columns | keep both or merge | Keep both when names differ; define same-name behavior. Semi/anti keep x only. Never overwrite originals with cleaned display. |
| Suffix | defaults, empty/equal, collision repair | Require two distinct suffixes when overlapping non-key names exist, or use deterministic vctrs-style repair. Error early on irreparable/ambiguous requests. |
| Existing key-name collision | caller already has key output name | User chooses a nonconflicting name or the function errors; no silent `.x/.y` leak. |
| Duplicate input names | accepted/repaired/error | Decide and test. Recommended require uniquely named data frames for public join family unless a single documented repair strategy is adopted. |
| Row order | exact stable algorithm | Pin per matrix and test duplicates/unmatched rows. Do not describe only as “unsorted” or “dplyr-shaped.” |
| Type/attributes | data frame/tibble/subclass; factors; list columns | State restoration strategy and key column class. If no dplyr/vctrs dependency is desired, promise base data-frame behavior explicitly instead of implied tibble behavior. |
| Zero rows/no matches | output prototype | Preserve input column types/attributes and deterministic suffix schema for every join. |
| Errors/warnings | class, payload, timing | Define stable condition classes for invalid input, relationship, unmatched, suffix/name collision, and policy conflict. Validate before expensive Cartesian expansion. |
| Resource bounds | duplicate explosion | Preflight key counts and relationship; optionally add a documented maximum-output guard. Never materialize unbounded Cartesian results merely to discover an error. |
| Diagnostics | per-row key/parse facts | Provide a companion diagnostics/result mode rather than encoding diagnostics into a display key. |
| Dependency meaning | “dplyr-shaped” | Choose either compatibility with a pinned dplyr contract/version (and test against it) or an independent rurl contract with the matrix above. The phrase alone is non-normative. |

## Minimum required tests

The protocol's fixture families are useful, but the key/join slice needs its own executable conformance ledger. At minimum:

1. **Key vector contract:** scalar, length zero, `NA`, empty string, names, factors, list-column rejection, mixed valid/invalid/warning rows, deterministic version/class.
2. **Injective framing:** adversarial component values containing every separator/control candidate cannot collide; component-boundary tuples remain distinct.
3. **Display/cleaning invariance:** for a fixed parse/key policy, vary every `get_clean_url()` dial—protocol, `www`, PSL/subdomain, case, trailing/index, path normalization, scheme-relative display, host/path encoding, all query modes/options, port handling, profile—and prove the key is unchanged unless an explicitly corresponding comparison policy is changed.
4. **Scheme matrix:** explicit HTTP/HTTPS, case variants, missing, inferred, scheme-relative, relative reference, custom scheme, unsupported scheme, each under all three proposed modes.
5. **Port matrix:** absent/default/non-default/mismatched-default/invalid/empty ports crossed with explicit/missing/scheme-relative/custom scheme. Include HTTP 80 and HTTPS 443 equivalence and preserved `:8080`/`:8443` distinctions.
6. **Host matrix:** Unicode/punycode, case, trailing dot, empty labels, deviation characters, IPv4 alternate forms, IPv6 normalization, opaque host, absent/empty authority, and PSL rewrites proven comparison-inert by default.
7. **Path matrix:** empty versus `/`, rootless/opaque, dot segments, literal versus percent-encoded unreserved, `%2F` versus `/`, `%5C` versus `\\`, repeated slashes, encoded dot segments, invalid percent, UTF-8, and every presentation choice proven inert.
8. **Query matrix:** absent versus `?`, empty pairs, `a` versus `a=`, duplicate/order variants, `+`/`%20`/`%2B`, encoded delimiters, invalid percent, query mode policies, stable duplicate sorting, and tracking-filter policy version.
9. **Fragment/userinfo matrix:** explicit proof of the chosen default and opt-in behavior, including absent versus present-empty state.
10. **Eligibility matrix:** missing input, invalid input, warning-only parse, unsupported valid scheme, successful non-host URL, and explicit behavior for key value plus diagnostic reason.
11. **Join-family row ledger:** all six joins with 0×0, 0×n, n×0, no match, one-to-one, one-to-many, many-to-one, many-to-many, and matches mixed with unmatched rows; assert exact row order.
12. **Relationship/multiple matrix:** every declared relationship against valid and violating data; validation before materialization; stable condition class and representative counts.
13. **Invalid/`NA`/unmatched matrix:** each join crossed with keep/drop/error and missing-match policy; semi/anti behavior explicitly asserted.
14. **Schema/name matrix:** same/different URL column names, overlapping payload, caller key-name collision, duplicate names, equal/empty suffixes, internal-looking names, factors/list columns, and zero-row prototypes.
15. **Legacy migration:** freeze current `canonical_join()` behavior in a legacy ledger, then assert warnings/translations/new-family differences for non-default port, query handling, lossy path encoding, protocol stripping, profiles, invalids, and duplicate keys.

### Current tests checked and remaining gaps

Shipped tests establish only the old contract:

- `tests/testthat/test-canonical_join.R:1-304` covers clean-key matching, `first/all/error`, parse keep/drop/error, right/full, input warnings, empty results, output names, anonymous names, and status eligibility.
- `tests/testthat/test-url-standard-path-encoding-orthogonal.R:204-216` verifies path-presentation forwarding through the join.
- Query tests verify current clean-key filter/keep behavior, and profile tests verify forwarding, but do not define an independent key.
- Port tests cover parsing/clean rendering, not the complete join equivalence table.
- The parser characterization snapshot is not a join/key conformance suite.

Missing are semi/anti joins, a public key, key framing/version, cleaner-invariance, the full scheme/port/component truth tables, relationship validation, complete duplicate cardinalities, stable order, key visibility, suffix/name collision handling, exact invalid/`NA` policy, all zero-row prototypes, and migration behavior.

## Contradictions and open owner decisions

### Contradiction ledger

| ID | Authorities in tension | Classification | Required resolution |
|---|---|---|---|
| C1 | Protocol `§6.4`–`§6.5` says cleaning/display is not equality; `R/canonical_join.R:241-247` keys on `clean_url` | **PROPOSED/ACCEPTED vs SHIPPED** | New key path plus explicit legacy migration. Do not reinterpret current behavior as compliant. |
| C2 | ADR 0011 says encode/decode are lossy presentation; its closing consumer advice points key users to `keep`; ADR 0012 says lossy presentation never feeds the identity serializer | **ACCEPTED evolution** | Carry forward identity-before-presentation; supersede any reading that `clean_url` itself is the future comparison object. |
| C3 | README/manual/architecture call `clean_url` canonical join identity; protocol demotes it to cleaning convenience | **SHIPPED docs vs PROPOSED v3** | Preserve docs as evidence of legacy behavior, then update atomically with implementation/migration. |
| C4 | Historical query PRD intentionally makes query cleaning shape join equality; protocol requires an independent query key policy | **HISTORICAL vs PROPOSED** | Treat historical behavior as migration input, not the v3 default contract. |
| C5 | Historical port PRD documents `exclude` as compatibility default and new `keep/strip_default`; protocol requires safe default-port equivalence and preserved non-default ports | **SHIPPED compatibility vs PROPOSED comparison** | Keep cleaning port behavior independent; define comparison port behavior separately. |
| C6 | Protocol says “ordinary data-frame joins” and “dplyr-shaped”; source uses base `merge()` and package has no public six-function join family | **PROPOSED ambiguity vs SHIPPED** | Choose an independent pinned contract or explicit dplyr compatibility/dependency. |
| C7 | `on_parse_error = "error"` can reject warning rows under default status policy | **SHIPPED naming contradiction** | Separate warning eligibility from invalid/error policy in the new API; document legacy behavior. |
| C8 | Protocol requires absent/present-empty fidelity; current clean/query assembly can erase empty delimiters | **PROPOSED/ACCEPTED vs SHIPPED key substrate** | Build keys from the richer internal state, not public clean fields. |

### Open owner decisions

The following decisions block a normative v3 key/join specification:

1. What is the default comparison standard/policy, and how is it versioned?
2. Is `get_url_key()` a classed atomic vector, record/data frame, or another non-URL type? What persisted representation is supported?
3. Which of `exact`, HTTP/HTTPS, or HTTP/HTTPS/missing is the default scheme policy?
4. Does scheme-relative count as missing for the third scheme policy, or remain a distinct unresolved-reference state?
5. What are the precise port results in the scheme/port truth-table rows marked `?`?
6. Are FTP/WS/WSS default ports normalized in comparison v1, or is the commitment intentionally HTTP/S-only?
7. What is the default query policy: ignored resource-style identity, exact structural query, standard serialized query, or ordered-pair identity?
8. Are fragment and userinfo ignored, included, or key-ineligibility triggers by default?
9. Is a host trailing root dot equivalent? Which IDNA/UTS46 processing and version is part of the key contract?
10. Which relative, opaque, hostless, and unsupported-scheme URLs are keyable without a base?
11. Do missing keys ever match? If yes, can missing input match only missing input while invalid parses remain never-match?
12. What invalid/warning defaults apply to each join, especially semi and anti?
13. What relationship and multiple-match defaults replace or coexist with `collision`?
14. What exact stable order is promised for right and full joins?
15. Is the public family behavior pinned to a dplyr version/dependency, or independently specified by rurl?
16. Is the comparison key visible by default, and how are its class/name and collisions handled?
17. How are duplicate input names, equal/empty suffixes, and existing key-name collisions handled?
18. Does `canonical_join()` freeze as legacy, deprecate, or change in place? What happens to every current argument in `...`?
19. What resource guard prevents accidental many-to-many result explosion?
20. What diagnostic surface lets users compare legacy clean-key matches against v3 matches before migration?

## Files, symbols, and tests checked

### Primary protocol and repository instructions

- `_scratch/url-v3-spec-reconstruction-protocol.md` — entire protocol, especially authority (`§2`–`§3`), state/serialization/cleaning/comparison (`§6.3`–`§6.5`), fixtures (`§8`), and product structure/owner decisions.
- `CLAUDE.md`
- `FP_CLAUDE.md`
- `ARCHITECTURE.md`

### Public implementation and internal dataflow

- `R/canonical_join.R` — `canonical_join`, `.cj_validate_inputs`, `.cj_resolve_sides`, `.cj_empty_result`, `.cj_side_state`, `.cj_drop_bad`, `.cj_keep_first`, `.cj_build_join_df`, `.cj_assemble_output`.
- `R/accessors.R` — `get_clean_url`, accessor extraction/profile resolution, query accessors.
- `R/parse.R` — public parse entry points and option flow.
- `R/parse-phases.R` — scheme preprocessing/acceptance, raw components, path identity/presentation, query filtering, port rendering, clean assembly, opaque/general assembly.
- `R/path-query.R` — path/query parsing, normalization, pair processing, percent/plus behavior.
- `R/profiles.R` — profile definitions, governed options, explicit precedence/conflicts.
- `R/utils.R` — option validation/vector helpers and utility behavior relevant to key inputs.
- `R/domain.R` — domain/PSL derivation used by host/subdomain cleaning.
- `R/host-policy.R` — host kinds and host normalization/presentation rules.
- `R/status-constants.R` — status eligibility classes.
- `NAMESPACE` — exported join/accessor surface; no `get_url_key` or join family.

### Public documentation

- `README.md` — clean URL, query handling, profiles, canonical joins.
- `NEWS.md` — historical changes to cleaning, standard/profile, query, port, and join behavior.
- `man/canonical_join.Rd`
- `man/get_clean_url.Rd`
- Relevant parse/accessor/profile/host/path/query generated manuals identified through the public option flow.

### Accepted design records

- `design/adr/0003-parse-present-stage-split.md`
- `design/adr/0007-url-standard-selector.md`
- `design/adr/0011-path-encoding-orthogonal-presentation.md`
- `design/adr/0012-general-url-parser-scope.md`

### PRDs and historical evidence

- `design/prd/url-standard-selector.md`
- `design/prd/url-standard-selector-v2.md`
- `_scratch/PRD-query-param-handling.md`
- `_scratch/whatwg-url-brainstorm.md`
- `_scratch/fp-issue-audit.md`
- `_scratch/fp-issue-import.md`

These records were classified as accepted or historical according to their status; none was silently promoted above the protocol's future-v3 reconstruction needs.

### Tests

- `tests/testthat/test-canonical_join.R`
- `tests/testthat/test-url-standard-path-encoding-orthogonal.R`
- `tests/testthat/test-url-standard-port-handling.R`
- Query handling/filter/accessor tests located by `query_handling`, `sort_params`, duplicate, empty, and plus searches.
- Profile/conflict/forwarding tests located by `canonical_join`, `profile`, and `url_standard` searches.
- General-parser characterization test and snapshot, checked for whether they define key/join behavior (they do not).
- Repository-wide test inventory searched for `canonical_join`, `clean_url`, port, scheme-relative, duplicates, relationship, semi/anti, ordering, suffix, invalid, and `NA` behavior.

## Final audit conclusion

The protocol should retain its current central claim: `get_clean_url()` is a useful cleaning convenience, not the equality substrate, and display strings may legitimately differ without implying different comparison identity. To make that claim executable, the v3 specification now needs two owner-approved normative artifacts: the completed key-policy matrix (including the scheme/port truth table) and the completed six-join semantics matrix. It also needs an explicit `canonical_join()` migration ledger. Until those exist, comparison and join reconstruction remains blocked at design—not because implementation is difficult, but because the desired observable behavior is not uniquely specified.
