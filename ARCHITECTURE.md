# rurl architecture

This is the durable, tracked description of how `rurl` is put together: the
load order, the file/responsibility map, the parse data flow, the delegation
seams, and the caches. It is the reference `CLAUDE.md` and new contributors
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
package (Public Suffix List); Punycode/IDNA to `punycoder`; the underlying
syntactic parse to `curl`. `rurl` owns the normalization policy, the canonical
`clean_url` key, the reversible host rendering, and the standards-profile
(`url_standard`) behavior on top of those libraries.

## Load order

The `Collate:` field in `DESCRIPTION` is authoritative. The load order is:

```
rurl-package.R → status-constants.R → utils.R → query-denylist.R → domain.R →
path-query.R → parse-phases.R → parse.R → diagnostics.R → accessors.R →
canonical_join.R → resolve.R → zzz.R
```

Later files depend on earlier ones (e.g. `resolve.R` composes `parse.R`'s
`safe_parse_urls()` and `path-query.R`'s `._remove_dot_segments()`). Keep
`Collate:` in sync when adding a file.

## File / responsibility map

- **R/parse.R** / **R/parse-phases.R** — the parsing engine. `parse.R` holds
  `safe_parse_url()` (scalar) and `safe_parse_urls()` (vector), option
  validation, the `url_standard` profile/conflict machinery
  (`.URL_STANDARD_PROFILES`, `.validate_url_standard()`,
  `.check_url_standard_conflicts()`), and the Stage-A/Stage-B split
  (`._parse_stage_a_vec` / `._parse_stage_b_vec`) plus the cache-key derivation
  (`.parse_cache_keys()`). `parse-phases.R` holds the decomposed per-phase
  helpers `.prepare_urls_for_curl_vec()` … `.assemble_parse_result()`, including
  the host model (`.apply_host_standard_model_vec()`), the WHATWG pre-curl
  transforms (`.rewrite_whatwg_backslashes_vec()`,
  `.strip_whatwg_control_chars_vec()`, `.map_whatwg_domain_separators_vec()`,
  and the host-charset shim `.shim_whatwg_host_charset_vec()` — ADR 0009), and
  the `clean_url` assembler (`.build_clean_url_vec()` / `.build_port_part_vec()`).
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
- **R/resolve.R** — `resolve_url()`, RFC 3986 §5 reference resolution composed
  over `safe_parse_urls()` (see ADR 0007).
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
- `invalid = "na"` so malformed hosts yield `NA` instead of erroring.
- Never use pslr session-global list switching (`psl_use()`) for per-request
  behavior (pslr PRD §12).

## Data flow: the parse pipeline

`safe_parse_url()` is the workhorse; every `get_*()` accessor, `canonical_join()`,
and `resolve_url()` is built on it. The pipeline is split into two stages
(ADR 0003):

- **Stage A (`._parse_stage_a_vec`)** — the option-*independent*, cacheable
  parse: prepare the URL for `curl`, run `curl::curl_parse_url()`, extract raw
  components, detect IP hosts, apply the `url_standard` host model, derive
  domain/TLD via `pslr`. Its output is keyed by `.parse_cache_keys()` (the URL
  plus the small set of options that change *what is parsed*, notably
  `url_standard` and `scheme_policy`). `scheme_policy` (ADR 0010) is the
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
  `resolve_url()` returns it, so any change to `clean_url` assembly is a change
  to the join/identity semantics.

## Dependencies

- `curl` — syntactic URL parse via `curl_parse_url()`.
- `stringi` — Unicode string manipulation (with deliberate base-R exceptions;
  see ADR 0005).
- `punycoder` (>= 1.2.0) — Punycode encoding/decoding.
- `pslr` (>= 1.0.2) — Public Suffix List matching.
