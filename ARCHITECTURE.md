# rurl architecture

This is the durable, tracked description of how `rurl` is put together: the
load order, the file/responsibility map, the parse data flow, the delegation
seams, and the caches. It is the reference `AGENTS.md` and new contributors
point at, so it survives on a fresh clone and in CI (unlike the working-tree
`_scratch/` notes).

For *why* a load-bearing decision was made — as opposed to *what* the structure
is — see the Architecture Decision Records under [`design/adr/`](design/adr/).
Accepted product specs live under [`design/prd/`](design/prd/). This file is
excluded from the package build (`.Rbuildignore`), so it never affects
`R CMD check` or the pkgdown site.

## Overview

`rurl` parses, normalizes, cleans, and joins URLs with vectorized,
pipe-friendly functions. Domain/TLD extraction is delegated to the `pslr`
package (Public Suffix List); Punycode/IDNA to `punycoder`. The underlying
syntactic parse is **in-tree** (`R/parse-web.R`) — it used to be `curl`'s, and
that dependency is gone. `rurl` owns the normalization policy, the canonical
`clean_url` key, the reversible host rendering, and the standards-profile
(`url_standard`) behavior on top of those libraries.

## Load order

The `Collate:` field in `DESCRIPTION` is authoritative. The load order is:

```
rurl-package.R → status-constants.R → utils.R → percent-coding.R →
parse-state.R → query-denylist.R → domain.R → path-query.R → parse-web.R →
parse-phases.R → parse.R → verdicts.R → profiles.R → diagnostics.R →
accessors.R → email-diagnostics.R → host-policy.R → canonical_join.R →
resolve.R → serialize.R → format.R → url-key.R → url-join.R → zzz.R
```

Later files depend on earlier ones (e.g. `resolve.R` composes `parse.R`'s
`safe_parse_urls()` and `path-query.R`'s `._remove_dot_segments()`, and
`format.R` reads the serializer-input record `serialize.R` builds). Keep
`Collate:` in sync when adding a file — it is hand-maintained, and
`devtools::load_all()` ignores it, so an omitted file passes the test suite and
fails `R CMD build`.

## File / responsibility map

- **R/parse.R** / **R/parse-phases.R** — the parsing engine. `parse.R` holds
  `safe_parse_url()` (scalar) and `safe_parse_urls()` (vector), option
  validation, the `url_standard` profile/conflict machinery
  (`.URL_STANDARD_PROFILES`, `.validate_url_standard()`,
  `.check_url_standard_conflicts()`), and the Stage-A/Stage-B split
  (`._parse_stage_a_vec` / `._parse_stage_b_vec`) plus the cache-key derivation
  (`.parse_cache_keys()`). `parse-phases.R` holds the decomposed per-phase
  helpers `.prepare_urls_for_curl_vec()` … `.assemble_parse_result()`, including
  the host model (`.apply_host_standard_model_vec()`), the WHATWG pre-parse
  transforms (`.rewrite_whatwg_backslashes_vec()`,
  `.strip_whatwg_control_chars_vec()`, `.map_whatwg_domain_separators_vec()`),
  and the `clean_url` assembler (`.build_clean_url_vec()` /
  `.build_port_part_vec()`). The libcurl **compensation layer** that used to sit
  in that family is **gone** (RURL-ezhzpkhg; ADR 0013 supersedes ADR 0009). Each
  of its five members decided a *parsing* question in front of the parser, and
  each is now a dial on `.parse_web_url_one()` (R/parse-web.R), mapped from
  `url_standard` by a `.web_*_policy()` function so the vectorized and scalar
  routes cannot drift: `host_charset` (which literal bytes a host may hold),
  `host_pct` (how a host that parsed is spelled back), `last_at_userinfo` (where
  the authority splits), `pqf_bytes` (an unwritable byte outside the authority)
  and `host_ipv4` (which tokens are IPv4 addresses). Phase 1 rewrites the input;
  it no longer decides acceptance. `.encode_userinfo_charset_vec()` is the one
  survivor — it writes the spelling WHATWG *stores*, which the parser cannot
  infer because `rfc3986` must stay source-preserving.
- **R/verdicts.R** — the layered validation verdicts: the L1 syntax / L2 policy
  / L3 annotation vocabularies, their derivation
  (`.derive_verdict_layers_vec()`), the projection back to the legacy
  `parse_status` (`.project_parse_status_vec()`, "π"), and the public companion
  `get_parse_verdicts()`. Phase 12 **is** π ∘ layer-derivation, so there is one
  status-deciding path and the companion cannot drift from the column.
- **R/accessors.R** — public `get_*()` accessors, all built on the shared
  `.extract_from_urls()` helper over `safe_parse_url()`.
- **R/domain.R** — Punycode helpers (`.normalize_and_punycode()`,
  `.punycode_to_unicode()`) and the `pslr` query seam
  (`.psl_registered_domain()`, `.psl_public_suffix()`, `.psl_suffix_extract()`,
  `.host_is_ace()`).
- **R/path-query.R** — low-level path normalization
  (`._collapse_path_slashes()`, `._remove_dot_segments()`,
  `._strip_index_page()`, `._encode_path_segments()`, and the per-standard
  `.rfc_unreserved_normalize()` / `.whatwg_preserve_normalize()`) and
  query-string parsing (`._parse_query_string()`).
- **R/diagnostics.R** — `url_standard` diagnostics + `host_type`
  infrastructure: the vocab constants (`.URL_DIAGNOSTICS`, `.HOST_TYPES`), the
  per-URL diagnostics accumulator (`.diag_new()` / `.diag_add()`), the single
  emit seam (`.derive_url_metadata_vec()`), and the companion-helper engine
  (`._url_metadata_vec()`). Metadata is surfaced ONLY through
  `get_host_type()` / `get_url_diagnostics()` / `get_scheme_class()` — never as
  widened parse columns/fields (see ADR 0006).
- **R/canonical_join.R** — dataset joining by canonicalized URL keys
  (`canonical_join()`).
- **R/resolve.R** — `resolve_url()`, reference resolution composed over
  `safe_parse_urls()` (ADR 0007); the merge is standard-*aware* (see Invariants).
  `output = "serialized"` routes the resolved absolute string to
  `serialize_url()` rather than to the cleaner.
- **R/serialize.R** — output surface **(b)**, the full-string standard
  serializer: `serialize_url()` and `.fsss_record_vec()`, the *lossless*
  serializer-input record the two spec-exact serializers
  (`.serialize_whatwg_full_vec()` / `.serialize_rfc_full_vec()`, in
  `parse-phases.R`) consume. The record is deliberately not the 18-field public
  projection, which `.blank_to_na()` has already collapsed.
- **R/format.R** — output surface **(d)**, safe human display: `format_url()`
  and its escape engine. The escaped code-point set is enumerated as static
  range matrices (`.FORMAT_IMMUTABLE_BLOCKS` / `.FORMAT_HAZARD_BLOCKS` /
  `.FORMAT_ESCAPE_BLOCKS`) with **no runtime Unicode general-category lookup**,
  so the rule is Unicode-version-invariant by construction; a test meta-guard
  deparses the namespace objects to enforce it. `.format_render()` decodes UTF-8
  itself instead of handing the string to `stringi`, because an invalid octet
  must be *emitted* as `%XX` rather than throw or be replaced. It reads
  `serialize.R`'s record, so it is loaded after it.
- **R/status-constants.R** — the `.STATUS_*` parse-status constants and the
  `.is_*_status()` predicates (incl. `.is_joinable_status()`).
- **R/utils.R** — the `%||%` operator, the scheme tables
  (`.WHATWG_SPECIAL_SCHEMES`, `.SCHEME_DEFAULT_PORTS`, `.SUPPORTED_SCHEMES`),
  and `.spu_result_fields` (the single source of truth for parse result
  columns).
- **R/query-denylist.R** — the built-in tracker-parameter denylist for
  `query_handling = "filter"`.
- **R/zzz.R** — package init (`.onLoad`), the cache registry
  (`.CACHE_REGISTRY`), the memoization caches, and the public cache API.

## Key internal functions

- `.normalize_and_punycode()` (R/domain.R) — IDNA/Punycode encoding with NFC
  normalization, for host reconstruction (`host_encoding = "idna"`).
- `.punycode_to_unicode()` (R/domain.R) — per-label Punycode decoding to
  Unicode (lenient `puny_decode` + `iconv` sanitization; `host_encoding =
  "unicode"` and `get_host()`). See ADR 0002 for why these two helpers are
  kept rather than replaced by `punycoder::host_normalize()`.
- `.psl_registered_domain()` / `.psl_public_suffix()` (R/domain.R) — thin
  wrappers over `pslr::registrable_domain()` / `pslr::public_suffix()`.
- `.psl_suffix_extract()` (R/domain.R) — full canonical decomposition
  (subdomain / domain / suffix / registrable_domain) via
  `pslr::suffix_extract()`, used for STRUCTURAL policy decisions (www-prefix,
  subdomain-trim) on one canonical spelling.
- `.host_is_ace()` (R/domain.R) — TRUE if any host label is an `xn--` A-label;
  drives the `host_encoding = "keep"` spelling choice.
- `.apply_host_standard_model_vec()` (R/parse-phases.R) — the `url_standard`
  host IPv4/reg-name model. No-op when `url_standard` is NULL; Stage-A-affecting
  under a selector (enters the parse cache key). See ADR 0007.

## PSL delegation contract (R/domain.R)

`rurl` no longer ships or matches the Public Suffix List; `pslr` owns it (see
ADR 0001). `rurl` calls `pslr` with a fixed contract:

- `source` `"all"` / `"icann"` / `"private"` maps 1:1 onto `pslr` `section`.
- `output = "unicode"` by default (preserves rurl's historical decoded-IDN
  output; pslr defaults to ASCII A-labels). Structural/decision callers keep
  this default; the emitted domain/TLD path (`.derive_domain_tld()`) instead
  selects the spelling from `host_encoding`, so
  `get_domain()`/`get_tld()`/`get_subdomain()` mirror `get_host()`.
- `unknown = "na"` so an unknown TLD yields `NA` rather than pslr's implicit
  `*`.
- pslr is queried on the **annotation candidate**, never on the identity host
  (`.psl_annotation_host_vec()`, R/parse-phases.R). The two differ only for a
  percent-encoded `rfc3986` reg-name, which keeps its source spelling in the
  host identity and so cannot be read by the PSL at all. The candidate is that
  host decoded **exactly once** as UTF-8 — legitimate because RFC 3986 §3.2.2
  admits percent-encoded UTF-8 in `reg-name` and requires IDNA transformation
  before a DNS lookup, while §6.2.2.2 authorizes only unreserved decoding for
  URI normalization. The decode reaches domain/TLD and nothing else: acceptance,
  `final_host` and serialization are untouched, and an invalid-UTF-8 or
  non-domain result is the `unknown` annotation. Because everything after the
  decode is the ordinary pslr path, an `rfc3986` host's annotation agrees with
  the one `whatwg` computes for the same decoded host (RURL-jhsbzmsj).
- `invalid = "na"` so malformed hosts yield `NA` instead of erroring.
- Never use pslr session-global list switching (`psl_use()`) for per-request
  behavior (pslr PRD §12). Per-request list selection instead flows through the
  optional `engine` argument (RURL-mhibnqbd): the public functions accept a
  `pslr::psl_engine()` snapshot, `.parse_options()` stores it on `opts`, and the
  three wrappers thread it into `pslr::*(..., engine = engine)`. `engine = NULL`
  (the default) OMITS the argument so pslr resolves against its session-global
  default — byte-identical to the pre-engine behavior. (A `psl_engine()` holds a
  C++ external pointer that does not serialize across sessions/workers; it is
  never cached or sent to a worker — rebuild per process.)

## Data flow: the parse pipeline

`safe_parse_url()` is the workhorse; every `get_*()` accessor, `canonical_join()`,
and `resolve_url()` is built on it. The pipeline is split into two stages
(ADR 0003):

- **Stage A (`._parse_stage_a_vec`)** — the option-*independent*, cacheable
  parse: prepare the URL (`.prepare_urls_for_curl_vec()`, named for the parser
  it no longer calls), run the in-tree `.parse_web_url_one()`, extract raw
  components, detect IP hosts, apply the `url_standard` host model, derive
  domain/TLD via `pslr`. Its output is keyed by `.parse_cache_keys()` (the URL
  plus the small set of options that change *what is parsed*, notably
  `url_standard` and `scheme_policy`, plus the per-request `engine` identity so
  two engines over different lists never share a memoized domain/TLD —
  `.engine_cache_token()`, `NULL` → `""`). `scheme_policy` (ADR 0010) is the
  input-*acceptance* axis: under `"require"` the scheme-less `add_http`
  inference in `.prepare_urls_for_curl_vec()` is suppressed and those rows join
  the reject set instead — orthogonal to `protocol_handling` (presentation) and
  `url_standard` (interpretation); `//host` stays governed by
  `scheme_relative_handling`.
- **Stage B (`._parse_stage_b_vec`)** — the presentation layer: case policy,
  host encoding, www/subdomain policy, path normalization/encoding, query
  filtering, port rendering, and the final `clean_url` assembly. Stage B is
  recomputed on every call and never cached, so editorial knobs
  (`port_handling`, `query_handling`, `case_handling`, …) do not multiply the
  cache key space.

The canonical `clean_url` is the join/identity key: it excludes fragment and
userinfo, includes the query only under `query_handling != "drop"` and the port
only under `port_handling != "exclude"`.

## Caches

- **Memoization**: `safe_parse_url()` (Stage A) and the Punycode encode/decode
  round-trips are cached in rurl; `pslr` caches its own PSL query results.
- **Registry**: caches are registered in `.CACHE_REGISTRY` (R/zzz.R) and
  initialized in `.onLoad`; they persist for the R session.
- **Public API**: `rurl_clear_caches()` frees memory; `rurl_cache_info()`
  inspects; `rurl_cache_config()` covers `full_parse`, `puny_encode`,
  `puny_decode`.

## Invariants worth knowing

- **Accessor symmetry**: every `get_*()` accessor is a thin wrapper over
  `safe_parse_url()` reading one field; a test oracle enforces that the two
  agree, so accessors cannot silently drift from the parse result.
- **Diagnostics are companion-only** (ADR 0006): host/scheme/validation
  metadata is exposed through `get_host_type()` / `get_scheme_class()` /
  `get_url_diagnostics()`, never as new parse columns/fields.
- **`clean_url` is the contract**: `canonical_join()` keys on it and
  `resolve_url()` returns it **by default** (`output = "clean"`), so any change
  to `clean_url` assembly is a change to the join/identity semantics.
  `resolve_url(..., output = "serialized")` is the opt-in escape hatch added by
  P2.7 D-A: it returns `serialize_url()`'s full standard string (surface (b) —
  fragment and credentials preserved) and requires an explicit `url_standard`.
  Surface (b) is the conformance substrate; surface (c) is a lossy SEO product
  and never carries a standards claim (P2.2 §1, P5.3 CLAIM-1).
- **No output surface substitutes for another** (P2.2 §5.1). The full-string
  surfaces are distinct products of the same parse and are not interchangeable:
  **(a)** source reproduction (`original_url`), **(b)** standard serialization
  (`serialize_url()`), **(c)** clean output (`get_clean_url()`), **(d)** safe
  display (`format_url()`), **(e)** the comparison key (`get_url_key()`, a
  non-URL projection). Credentials are reproduced only by (a)/(b), dropped by
  (c) and **redacted** by (d); a (d) string is display-only and must never
  re-enter serialization, a mutation baseline or a key. The rule that keeps this
  checkable is that (b) and (d) both consume the *lossless* serializer-input
  record, never a cleaned or formatted component (P2.2 §5.2).
- **Reference resolution is standard-aware** (P2.7 D-B, retiring PRD v2 D6):
  under `url_standard = "whatwg"` the WHATWG reference-*parsing* rules run
  before the RFC 3986 §5 merge — a reference carrying the base's own special
  scheme is relative, `\` reads as `/`, leading slash runs are skipped, and
  leading/trailing C0-or-space is stripped. Under `"rfc3986"` the merge is RFC
  3986 §5.2–§5.3. Under `NULL` it is byte-frozen (ADR 0007). Only the scheme
  production (RFC 3986 §3.1) is shared by both named profiles, because the two
  standards agree on it. Measured over the WHATWG's own base-carrying corpus
  (`tests/testthat/test-wpt-base-relative.R`): 247 of 274 rows exact, 27
  enumerated differences. That is a **separate** population from the base-null
  WPT headline (336/336, `tests/testthat/test-wpt-full-suite.R`); the two are
  never summed and the 247/274 split is not quoted as a conformance rate.

## Dependencies

- `stringi` — Unicode string manipulation (with deliberate base-R exceptions;
  see ADR 0005).
- `punycoder` (>= 1.2.0) — Punycode encoding/decoding.
- `pslr` (>= 1.1.0) — Public Suffix List matching.

The syntactic URL parse is in-tree (`R/parse-web.R`); `curl` is no longer a
dependency.
