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
fails `R CMD build`. This block is checked against `Collate:`, order included,
by `tools/architecture-map-gate.R` — see [Gates on this
file](#gates-on-this-file).

## File / responsibility map

Every file in `Collate:` has an entry here, and `tools/architecture-map-gate.R`
checks that — see [Gates on this file](#gates-on-this-file) below.

- **R/rurl-package.R** — the roxygen `"_PACKAGE"` block only: the package-level
  help topic, its `@seealso` map of the public surface, and the runnable
  overview examples. No code, and first in `Collate:` so the block is in place
  before anything documents against it.
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
- **R/parse-web.R** — the in-tree syntactic parser for the web/special-scheme
  route (http/https/ftp/ftps, plus ws/wss under `whatwg`). `.parse_web_url_one()`
  is the drop-in replacement for the `curl::curl_parse_url()` call Phase 2 used
  to make: it takes ONE already-prepared URL and returns either `NULL` (parse
  error) or the nine components libcurl returned, so every downstream `%||%
  NA_character_` keeps working. Eight of the nine reproduce libcurl's spelling
  and were verified to; `$url` deliberately does not (it is the prepared input
  verbatim, not a re-serialization) and nothing in rurl reads it — the file
  header carries the measured table. This is also where the five ex-compensation
  dials live as arguments (`host_charset`, `host_pct`, `last_at_userinfo`,
  `pqf_bytes`, `host_ipv4`), each fed by its `.web_*_policy()` mapper, plus the
  host parser (`.web_parse_host()`) and the IPv4/IPv6 canonicalizers.
- **R/parse-state.R** — the non-web parse routes and the state model they share
  (ADR 0012 Layers 3a/3c/4a/4b). Five sections: the internal enum vocabularies
  (`.PATH_KIND`, `.HOST_KIND`, `.AUTHORITY_KIND`, `.WHATWG_HOST_FORM`,
  `.RFC_HOST_FORM`, …) and their pure classifiers; the Stage-B eligibility
  matrix (`.stage_b_eligibility()`); the RFC 3986 generic-URI grammar gate
  (`.RFC3986_*` regex constants, `.rfc3986_generic_uri_ok()`,
  `.rfc3986_uniform_gate_ok()`); the posture host/opaque parsers
  (`.parse_opaque_urls_vec()`, `.parse_rfc_file_urls_vec()`); and the
  general-acceptance router (`.general_parse_vec()`). Why the vocabulary is
  richer than one `opaque` flag: ADR 0012 D2 shows a single boolean cannot
  round-trip `foo:bar` / `foo:/bar` / `foo:///bar` / `foo://[::1]/bar`, so
  `authority_delimiter_present`, `authority_payload_kind` and `host_kind` are
  all retained. The vocabulary is internal state only — empty and absent hosts
  still both surface as `NA` publicly.
- **R/verdicts.R** — the layered validation verdicts: the L1 syntax / L2 policy
  / L3 annotation vocabularies, their derivation
  (`.derive_verdict_layers_vec()`), the projection back to the legacy
  `parse_status` (`.project_parse_status_vec()`, "π"), and the public companion
  `get_parse_verdicts()`. Phase 12 **is** π ∘ layer-derivation, so there is one
  status-deciding path and the companion cannot drift from the column.
- **R/profiles.R** — the exported profile inspector `url_profile()` and nothing
  else (ADR 0012 Layer 6 / D6). The resolution machinery (`.URL_PROFILES`,
  `.validate_profile()`, `.resolve_profile()`) stays in `parse.R` next to the
  knobs it bundles; the inspector calls that same `.resolve_profile()`, so
  "what does this profile do" and "what does the parse path do" cannot diverge.
  Explicit arguments always override a profile (the iron rule), and the result
  is flagged `customized = TRUE` when they do.
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
- **R/percent-coding.R** — the in-tree percent-coding primitives `.pct_escape()`
  / `.pct_unescape()`, byte-exact replacements for `curl::curl_escape()` /
  `curl::curl_unescape()` on every reachable input (two deviations, both
  strictly local improvements, are documented on each function). The escape set
  is a static 256-entry byte→chunk table, so it is not locale-dependent the way
  a naive `isalnum()` would be. These are **not** the WHATWG component
  serializers — `.whatwg_component_percent_encode()` (R/path-query.R) preserves
  existing spellings and encodes a per-component set; these two are the plain
  RFC 3986 unreserved-set escape and the permissive decode rurl's presentation
  paths have always used. Do not merge them.
- **R/diagnostics.R** — `url_standard` diagnostics + `host_type`
  infrastructure: the vocab constants (`.URL_DIAGNOSTICS`, `.HOST_TYPES`), the
  per-URL diagnostics accumulator (`.diag_new()` / `.diag_add()`), the single
  emit seam (`.derive_url_metadata_vec()`), and the companion-helper engine
  (`._url_metadata_vec()`). Metadata is surfaced ONLY through
  `get_host_type()` / `get_url_diagnostics()` / `get_scheme_class()` — never as
  widened parse columns/fields (see ADR 0006).
- **R/email-diagnostics.R** — the email/userinfo diagnostic vocabulary behind
  `get_mailto_recipients()` (ADR 0012 D7). Structural **facts** only, never a
  gate (D5) and never a new `safe_parse_url()` column (ADR 0006): each fact
  names the grammar it was judged against, and the same recipient is projected
  independently as RFC 6068 `mailto_*` vocabulary and as an RFC 5321 `smtp_*`
  wire candidate. The provenance rule is load-bearing — the positional `to`
  arrives still percent-encoded, so recipients are **tokenized before
  decoding** (a `%2C` is never a separator, an encoded quote or bracket still
  protects a raw comma) and each field is then decoded exactly once.
- **R/host-policy.R** — practical host-validation **policy** on top of
  standards-correct parsing: `is_valid_host()`, `check_hosts()`, and the
  `.HOST_POLICY_RULES` vocabulary (`url` / `dns` / `web` / `registrable` /
  `seo`). It never changes how a URL parses, never touches `parse_status`, and
  never widens the parse fields — it is a companion helper composed from
  `get_host_type()` / `get_url_diagnostics()` / `get_host()` (ADR 0006). It is
  also not a conformance oracle (ADR 0012 D5): `web = TRUE` means "no practical
  footgun rurl checks for", and the absence of a `reasons` token is not a
  conformance claim. The one fact the parser cannot express is the underscore
  split (`domain-std3-violation` bundles `_` with `+`), so the web/dns
  char-class checks run their own regex over the ASCII host form; everything
  else is read from facts the parser already surfaces.
- **R/canonical_join.R** — dataset joining by canonicalized URL keys
  (`canonical_join()`). Legacy: its key **is** the cleaned display string. The
  `url-key.R` / `url-join.R` family below exists because that conflation is a
  defect, and deliberately does not inherit it.
- **R/url-key.R** — output surface **(e)**, the comparison key: `get_url_key()`,
  `url_key_policy()`, their print/format/`as.character` methods, and the engine
  under them. The key is derived from the canonical **identity** state — after
  standard interpretation, before any cleaning or display transform — and never
  from `clean_url`, which is why it shares `.fsss_record_vec()` with
  `serialize.R` rather than building its own record. No presentation dial can
  reach it: `path_encoding`, `host_encoding`, `www_handling`, case, query
  cleaning, `port_handling`, index/trailing-slash and every profile bundle are
  pinned to their identity values, so they are structurally incapable of moving
  a key byte. Framing is injective by construction, not delimiter
  concatenation. The key is not a URL and must never be rendered as one.
- **R/url-join.R** — the identity-keyed join family: `url_inner_join()`,
  `url_left_join()`, `url_right_join()`, `url_full_join()`, `url_semi_join()`,
  `url_anti_join()`, and the shared engine. It consumes
  `.url_key_compute_vec()` and compares framed bytes — it never touches
  `clean_url` and never re-derives identity. One immutable, symmetric policy is
  applied to both sides (side-specific rules are prohibited, because equality
  must stay symmetric and transitive), and row order is pinned per join rather
  than inherited: the family builds index vectors instead of calling `merge()`,
  whose ordering `canonical_join()` relies on and whose docs call unspecified.
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

- `utils` — base-R helpers: `URLdecode()` on the host-decode path,
  `head()`/`tail()` for label slicing, `capture.output()` for engine identity.
- `stringi` — Unicode string manipulation (with deliberate base-R exceptions;
  see ADR 0005).
- `punycoder` (>= 1.2.0) — Punycode encoding/decoding.
- `pslr` (>= 1.1.0) — Public Suffix List matching.

The syntactic URL parse is in-tree (`R/parse-web.R`); `curl` is no longer a
dependency.

## Gates on this file

`tools/architecture-map-gate.R` checks the three sections above that are exact
enumerations of a DESCRIPTION field — the load-order block, the
file/responsibility map, and the dependency list. It asserts four things, with
DESCRIPTION as the authority throughout:

1. the load-order block lists exactly `Collate:`, **in the same order** — the
   block's whole subject is the order, so a set comparison would let it lie
   about the thing it is for;
2. every `Collate:` file has a `**R/<file>**` entry in the file map;
3. no map entry names a file that is not in `Collate:` — the direction a rename
   or a deletion breaks;
4. `## Dependencies` names exactly the packages in `Imports:`, each carrying the
   same version floor. Compared as a **set**, unlike the load order: that
   section groups by importance and has no order to be wrong about.

Nothing else in this file is gated, and deliberately so: the rest is prose that
a checker could only pretend to judge. These three sections are different in
kind, and all three had drifted. Measured 2026-08-12, the load-order block
listed 14 of 24 files, the map documented 15 of 24 — including no entry for
`R/parse-web.R`, which holds the parser this file exists to explain — and the
dependency list named three of the four imports, missing `utils`. Every gap
arrived the same way: a slice edited DESCRIPTION because `R CMD build` or `R CMD
check` forced it, and had no reason to open this document. That is also why the
gate's CI trigger is `DESCRIPTION` rather than `R/**`.

Property 4 gates the version floors rather than banning them (RURL-rnwfclja).
Forbidding version numbers in this prose would remove the drift surface instead
of policing it, and would be marginally cheaper to check — but it also removes
the answer to what the section is asked most, "which packages, at what minimum",
and the gate makes the transcription safe anyway. The coupling is the intended
one: a floor bump now touches DESCRIPTION and this file in one commit.

`## Key internal functions` is **not** gated and should stay that way. It is a
curated selection, not an enumeration; a bijection against `R/` would buy stub
entries for several hundred internal functions and nothing else.

The cost of the rule is one paragraph per new R file. That is the trade being
made knowingly: a stub entry is worse than honest silence in a package with
hundreds of files, and better than silence in one with twenty-four, where every
file is a subsystem with a header comment already explaining itself.

Run it directly, or let `tools/verify.R` run it:

```sh
Rscript tools/architecture-map-gate.R             # scan, exit 1 on a gap
Rscript tools/architecture-map-gate.R --self-test # 3 positive + 17 negative cases
```
