## rurl 2.3.0

### Bug fixes

- `url_standard = "whatwg"` now strips ASCII tab (`U+0009`), LF (`U+000A`), and
  CR (`U+000D`) from the input before parsing, matching the WHATWG URL
  Standard's first parse step. Previously rurl rejected a control character in
  the authority (libcurl errors), so adversarial hosts that browsers accept
  after stripping — `http://ex<TAB>ample.com/` → `example.com`,
  `https://n.pr<LF>e.gg` → `n.pre.gg`, and CRLF-injection shapes like
  `http://127.0.0.<CR><LF>1:6379…` → `127.0.0.1` — returned `error`. They now
  parse under `"whatwg"`. The strip is **not silent**: it fires a new
  `control-char-stripped` diagnostic (see `get_url_diagnostics()`), keeping with
  the facts-not-policy design. `url_standard = "rfc3986"` and the default
  (`NULL`) are unchanged — RFC 3986 has no strip step and requires such bytes to
  be percent-encoded, so they still reject.

- `url_standard = "whatwg"` now **rejects** WHATWG forbidden host/domain code
  points instead of accepting them as registered names with `warning-no-tld`. A
  special-scheme host is a domain, and WHATWG fails the host parse when
  domain-to-ASCII yields a forbidden code point (`|`, `^`, DEL, space, …) or when
  domain-to-ASCII itself fails (a disallowed code point such as U+FFFD/U+FFFF, or
  a UTS-46-ignored code point like the U+00AD soft hyphen collapsing a label to
  empty). These now return `parse_status = "error"` under `"whatwg"`, matching
  the web-platform-tests failure corpus. `url_standard = "rfc3986"` and the
  default (`NULL`) are unchanged — RFC 3986 has no forbidden-host-code-point
  concept, so these stay permissive registered names there. The reversible-host
  and Punycode helpers are untouched (ADR 0002); this is a separate reject gate.

### Diagnostics

- New `get_url_diagnostics()` token `control-char-stripped`, emitted under
  `"whatwg"` on any URL from which an ASCII tab/LF/CR was removed.

## rurl 2.2.2

### Bug fixes

- `url_standard = "whatwg"` now rejects obfuscated numeric hosts that libcurl
  leaves as registered names. WHATWG parses any host whose final label is a
  number (decimal, or a `0x` hex literal) as an IPv4 address and fails the whole
  host parse when that IPv4 parse is invalid. Previously rurl only applied this
  rule to forms libcurl had already coerced to an IPv4 literal, so mixed
  reg-name/number hosts (`http://foo.09`, `http://foo.0x4`), leading-zero /
  invalid-octal octets (`http://1.2.3.08`), and `>4`-part or trailing-dot forms
  (`http://0x1.2.3.4.5`, `http://1.2.3.08.`) slipped through as
  `warning-invalid-tld` instead of `error`. They now return
  `parse_status = "error"` under `"whatwg"`, matching the WHATWG URL Standard
  and the web-platform-tests failure corpus. `url_standard = "rfc3986"` and the
  default (`NULL`) are unchanged — RFC 3986 has no numeric-host rule, so these
  remain valid registered names there.

## rurl 2.2.1

### Packaging

- Pin the sibling `Remotes:` to release tags (`pslr@v1.0.2`,
  `punycoder@v1.2.0`) to match the `Imports` version floors. The Remotes
  previously tracked each sibling's default branch; once `pslr`'s development
  head began pinning `punycoder@v1.2.0`, `pak` saw two conflicting sources for
  `punycoder` and could not solve the dependency graph, breaking a fresh
  install / CI resolution of `rurl` and of any package depending on it. No
  user-facing code change.

## rurl 2.2.0

### New features

- New `url_standard` selector on `safe_parse_url()`, `safe_parse_urls()`, the
  `get_*()` accessors, and `canonical_join()`: `NULL` (default), `"rfc3986"`, or
  `"whatwg"`. It selects a coherent set of
  standard-conformant behaviors for the axes it governs — path percent/dot
  handling, the host IPv4/reg-name model, and `case_handling` — so callers no
  longer hand-assemble the low-level knobs to approximate a standard. Passing a
  governed low-level knob (`path_encoding`, `path_normalization`,
  `case_handling`) with a value the selected profile would not choose is an
  error (also across `canonical_join()`'s `...`). **`url_standard = NULL` is
  fully backward compatible** — byte-for-byte identical output and unchanged
  result shape; there is no default flip. Under `"rfc3986"` only unreserved path
  bytes are decoded (`%2F` stays encoded) and numeric-looking non-IPv4 hosts are
  parsed as `reg-name`; under `"whatwg"` encoded unreserved bytes are preserved
  and valid numeric IPv4 forms are coerced per the WHATWG host model.
- New standalone `port_handling` option controlling whether the port appears in
  `clean_url`: `"exclude"` (default, today's behavior), `"keep"`,
  `"strip_default"`, `"strip_all"`. It is editorial and
  standard-independent; under `url_standard = "whatwg"`, `"keep"` elides a port
  matching its special scheme's default (http:80, https:443, ftp:21).
- Under `url_standard = "whatwg"`, a literal backslash is recognized as a path
  separator for WHATWG-special schemes (`http`/`https`/`ftp`, not `ftps`), as
  browsers do. `%5C` is never treated as a separator, and
  `"rfc3986"` / no selector leave backslashes inert.
- New `resolve_url(relative_or_absolute, base_url, url_standard = NULL, ...)`:
  RFC 3986 §5 reference resolution (empty / fragment-only / query-only /
  scheme-relative / absolute-path / relative-path merge) composed over the same
  parsing machinery, returning the canonical `clean_url` of the resolved
  reference. Vectorized, with the base recycled.

### Diagnostics and classification helpers

- New companion helpers, gated on `url_standard` (return `NA` with no selector),
  surface metadata **without** widening the parse result shape:
  `get_host_type()` (domain / ipv4 / ipv6 / reg-name / missing),
  `get_scheme_class()` (WHATWG special / non-special / missing-or-error), and
  `get_url_diagnostics()`.
- Diagnostics vocabulary: `ipv4-*` numeric-host tokens, `encoded-dot-segment`,
  `encoded-reserved-path-byte`, `explicit-default-port` / `non-default-port`,
  `invalid-reverse-solidus`, and the DNS/UTS-46 tokens `domain-label-too-long`,
  `domain-name-too-long`, `domain-empty-label`, `domain-hyphen-violation`,
  `domain-std3-violation`. Diagnostics are **facts, not policy**:
  a token describing an input shape fires identically under both standards, so a
  link-graph builder can ignore them while an SSRF/allowlist guard rejects on
  them.

### Documentation

- New `url_standard` vignette walking through RFC 3986 vs WHATWG on the canonical
  cases (`%41%42`, `%2F`, `2130706433`), the diagnostics, and the migration
  notes (pin `url_standard = "whatwg"` for WHATWG-aligned link identity on the
  governed axes; the interim `path_encoding = "keep"` stopgap is a collision fix,
  not a full standard profile).

## rurl 2.1.0

### New features

- `safe_parse_url()` and `safe_parse_urls()` gain four additive result columns —
  `domain_ascii`, `domain_unicode`, `tld_ascii`, and `tld_unicode` — exposing
  the registrable domain and public suffix in **both** canonical spellings,
  independent of `host_encoding`. `host_encoding` is a
  *rendering* choice, so the existing `domain`/`tld` columns follow it (under
  the default `"keep"`, a Unicode host and its Punycode A-label render
  differently and do not compare equal). The new columns are stable *identity*
  keys instead: `http://münchen.de` and `http://xn--mnchen-3ya.de` share one
  `domain_ascii` (`"xn--mnchen-3ya.de"`) and one `domain_unicode`
  (`"münchen.de"`), so a consumer can build an encoding-independent key from a
  single parse rather than re-parsing with a forced `host_encoding`. For
  ASCII-only hosts the two spellings are equal; IP hosts and null rows yield
  `NA`. The values were already computed internally, so this is purely additive
  and existing `domain`/`tld` semantics are unchanged.

## rurl 2.0.0

### New features

- `safe_parse_url()` and `safe_parse_urls()` gain opt-in query-string handling
  for `clean_url`. New `query_handling` option:
  `"drop"` (default — `clean_url` stays query-free, exactly as before),
  `"filter"` (keep contentful params, drop known trackers such as `utm_*`,
  `fbclid`, `gclid` via a built-in denylist), `"allow"` (keep only names in
  `params_keep`), and `"keep"` (keep every param, canonicalized). Supporting
  options: `params_keep`, `params_drop` (glob-aware, `*`-only), `sort_params`,
  `empty_param_handling`, `params_case_sensitive`, and `decode_plus`. The raw
  `query` result field is untouched — it always reports the faithful original.
  All defaults preserve current output. Because `canonical_join()` forwards
  `...` to `safe_parse_urls()`, these options also flow into the join key, so a
  non-`"drop"` mode makes `?id=1`/`?id=2` stop collapsing while `utm`-only
  differences still collapse under `"filter"`.
- `get_clean_url()` gains the seven query-filter arguments (`query_handling`,
  `params_keep`, `params_drop`, `params_case_sensitive`, `sort_params`,
  `empty_param_handling`, `decode_plus`), reaching full parity with the parse
  engine. A filtered cleaned URL is now available directly —
  `get_clean_url(u, query_handling = "filter")` — instead of only via
  `safe_parse_url(u, query_handling = "filter")$clean_url`. Defaults are
  unchanged (`query_handling = "drop"`), so existing output is byte-identical.
- `get_query()` gains the same query-filter engine arguments
  (`query_handling`, `params_keep`, `params_drop`, `params_case_sensitive`,
  `sort_params`, `empty_param_handling`, `decode_plus`), so a cleaned query can
  be pulled directly without going through `clean_url`. It
  defaults to `query_handling = "keep"` (an accessor returns the query as found
  unless you ask it to filter), and the filter runs before rendering:
  `decode = TRUE` gives the readable decoded form, `decode = FALSE` the
  canonical re-encoded form. Every existing default is byte-for-byte unchanged.
- New `query_param_summary()` introspection function tabulates the query
  parameters across a set of URLs — which names appear, what values they take,
  `n`/`n_urls` counts, and a `would_drop` column previewing what
  `query_handling = "filter"` would remove. Returns a flat
  (long) `data.frame` at `level = "param"` or `level = "value"`. Param names
  are grouped faithfully (case-sensitively) while `would_drop` honours
  `params_case_sensitive`, so you can audit a URL set before choosing a policy.
- The `clean_url` query is deliberately **exempt from `case_handling`** (query
  values are case-sensitive — tokens, IDs, signatures). Under
  `case_handling = "lower"` or `"upper"` the scheme/host/path fold but the
  appended query keeps its original case, so `clean_url` is no longer uniformly
  cased in those modes.

### Breaking changes

- `path_normalization = "none"` (the default) is now genuinely lossless for
  path structure. The request path is read from the input
  verbatim rather than from libcurl's pre-normalized path, so `.`/`..` segments
  are preserved: `"http://ex.com/a/../b"` now yields `clean_url`
  `"http://ex.com/a/../b"` instead of `"http://ex.com/b"`. Because `clean_url`
  is a `canonical_join` key, dot-segment paths that previously collided no
  longer do — pass `path_normalization = "dot_segments"` (or `"both"`) to
  resolve them. rurl now owns dot-segment resolution (RFC 3986 §5.2.4, literal
  `.`/`..` only), so a percent-encoded `%2e` is treated as an ordinary path
  byte and is **never** resolved as traversal — closing the silent
  `"/a/%2e%2e/b"` → `"/b"` rewrite libcurl used to perform. Percent-hex case is
  still canonicalized to uppercase (`%2f` → `%2F`) under `"keep"`, so encoded
  paths remain join-equivalent.
- Non-compliant input handling is now consistent and strict.
  rurl no longer fabricates an `http://` URL for scheme-less input that is not
  host-shaped: nonsense tokens (`"asdfghjkl"`, `"example"`), free text
  (`"hello world"`), and path fragments (`"/relative/path"`) now return
  `parse_status = "error"` with `clean_url = NA`, matching the behavior of
  inputs that already errored. An **explicit** supported scheme is still
  trusted, so `"http://asdfghjkl/"` remains `warning-no-tld`. Scheme-less
  `localhost` is accepted (the one allowlisted single-label host).
- IP literals are validated strictly against the *input* rather than trusting
  libcurl's legacy `inet_aton` coercion. Integer, hexadecimal, octal, and
  short-form numbers (`"12345"` → `0.0.48.57`, `"0x7f000001"`, `"192.168"`),
  out-of-range or wrong-arity dotted numbers (`"256.1.1.1"`, `"1.2.3.4.5"`),
  and **leading-zero (octal) octets** (`"192.168.010.1"`, which silently means
  `192.168.8.1`) now return `"error"` instead of a coerced address. Canonical
  literals (`"1.2.3.4"`, `"[::1]"`) are unaffected. `.detect_ip_host_vec()` is
  correspondingly tightened to reject zero-padded octets.
- Only `http`, `https`, `ftp`, and `ftps` are supported schemes (now a single
  source of truth, `.SUPPORTED_SCHEMES`). Scheme-bearing input with any other
  scheme — opaque (`mailto:`, `tel:`, `data:`) or authority-based but
  unsupported (`ws://`, `ssh://`, `redis://`) — returns `"error"`.
- New `parse_status` value `"warning-userinfo"` for scheme-less input carrying
  userinfo (e.g. `"user@example.com"`): `host`/`domain`/`tld`/`user` still
  resolve, but `clean_url` is `NA` (rurl will not fabricate a canonical URL
  from an ambiguous, email-shaped, scheme-less string). Such rows are
  non-joinable in `canonical_join()`. Input with an explicit scheme
  (`"http://user@example.com"`) is unchanged. Scheme-less `user:pass@host`
  (indistinguishable from `scheme:opaque`) remains `"error"`; use the
  scheme-relative form `//user:pass@host` to parse it.

### Performance

- The parse pipeline is split into an option-independent core (Stage A: curl
  components, IP detection, the post-www host, and the PSL domain/TLD
  decomposition) and a presentation stage (Stage B: path handling, case,
  host-encoding spelling, subdomain trimming, clean-URL assembly, status). The
  `full_parse` cache now stores Stage A, keyed only by URL, protocol/scheme
  handling, `www_handling`, and `tld_source`. Calling several accessors with
  different presentation profiles on the same URLs (e.g. `get_host()`,
  `get_domain()`, `get_tld()`, `get_clean_url()`, `get_subdomain()`) now shares
  one cache entry per URL and re-runs only the cheap Stage B, so the expensive
  curl + PSL work happens once instead of once per profile. Output is unchanged.
  Cache memory per URL also drops to a single (option-independent) entry.

- `safe_parse_urls()` now de-duplicates its input, parsing each unique URL only
  once (with cross-call reuse via the `full_parse` cache) and expanding the
  results back with `match()`. Repeated / duplicate URLs cost only the match,
  so warm and duplicate-heavy inputs are dramatically faster. `safe_parse_url()`
  (scalar) shares the same cached code path.

- Query-string parsing (`get_query(format = "list")`) is now linear in the
  number of key/value pairs (previously quadratic from incremental list
  growth), so URLs with very long query strings parse faster. Output is
  unchanged.

### Behavior changes

- Accessor results (`get_*()`) are no longer named by the input URLs. The
  `get_*()` functions now parse their input in a single vectorized pass and
  return plain unnamed vectors (or lists), instead of vectors carrying a
  `names` attribute of the input URLs. Wrap in `stats::setNames(x, url)` if you
  relied on the old names.

- The `full_parse` memoization cache is now bounded by default at 100000 unique
  url × option combinations (previously `Inf`), so parsing millions of unique
  URLs can no longer grow the cache without limit. Override with
  `rurl_cache_config(max_full_parse = Inf)` to restore the previous unbounded
  behavior; the reset-watermark semantics are unchanged.

- A present-but-empty `query`, `fragment`, `user`, or `password` component
  (e.g. the query of `"https://example.com/?"`) is now reported as `NA`
  consistently. `curl::curl_parse_url()` returns such components as `NULL` on
  some libcurl versions and `""` on others; both now normalize to `NA`, so
  output no longer depends on the installed libcurl version. This matches the
  behavior already produced on platforms where curl returned `NULL`.

### Behavior changes

- `safe_parse_urls()` now accepts a factor input, coercing it to its character
  labels up front (matching `canonical_join()`), instead of returning an
  all-`error` row for every element.

### Bug fixes

- Scheme-less `host:port` input (e.g. `"example.com:8080/x"`) is no longer
  reported as `parse_status = "error"`. It parses correctly (valid host, path,
  and `clean_url`) but the status-derivation phase mistook `example.com:` for
  an unsupported scheme and demoted it, contradicting the emitted components.
  It now reports `"ok"`, restoring the invariant that a present `clean_url`
  implies a non-error status. Genuinely unsupported/opaque schemes
  (`mailto:`, `user:pass@host`) still return `"error"`.
- path/fragment/userinfo are no longer percent-decoded during parsing;
  `path_encoding = 'keep'` now honors its contract (leaves the path
  byte-for-byte); the raw query is preserved (`?flag` stays `flag`, not
  `flag=`). NOTE: `clean_url` values change for URLs containing
  percent-encoded path bytes — since `clean_url` is a `canonical_join` key,
  `/a%2Fb` and `/a/b` no longer collide.

### Documentation

- Clarified the `path_normalization` and `path_encoding` docs to describe the
  normalization the underlying parser (libcurl) applies before rurl sees the
  path: RFC 3986 dot-segment resolution (`.`/`..`, including `%2e`/`%2E`) is
  unconditional and cannot be disabled — so `path_normalization = "none"` still
  resolves `/a/../b` to `/b` — and percent-encoding hex digits are normalized to
  uppercase (`%2f` → `%2F`), an RFC 3986 §6.2.2.1 case canonicalization that
  makes such paths compare equal in `canonical_join()`. Behavior is unchanged;
  only the documentation now matches it.

## rurl 1.4.1

### Bug fixes

- `safe_parse_url()`/`safe_parse_urls()` now recognize IPv6 address literals
  that carry an embedded dotted-quad IPv4 tail (RFC 4291 §2.2 form 3 / §2.5.5,
  e.g. `[::ffff:127.0.0.1]`, `[64:ff9b::8.8.8.8]`). Previously these fell
  through to the registered-name path, returning `is_ip_host = FALSE` and a
  spurious `warning-invalid-tld` status; they now report `is_ip_host = TRUE`
  and `parse_status = "ok"`. Both the dotted and hex-hextet spellings of the
  same address (`[::ffff:0808:0808]` vs `[::ffff:7f00:1]`) now classify
  identically. A malformed embedded tail (octet out of range) is still
  rejected.

### Infrastructure

- Added a dependency vulnerability audit against the Sonatype OSS Index via
  `oysteR` (new `Suggests`). `tests/testthat/test-security.R` runs
  `oysteR::expect_secure("rurl")` and a dedicated `security-audit.yml` workflow
  (weekly + on demand) executes it with OSS Index credentials; the test skips
  cleanly without credentials, offline, or on CRAN.
- Added a second, token-free dependency vulnerability audit against the OSV
  database (<https://osv.dev>) via `rosv` (new `Suggests`).
  `tests/testthat/test-osv.R` checks the runtime dependency closure of rurl
  (recursive `Depends` + `Imports`) at installed versions, and a dedicated
  `osv-audit.yml` workflow (weekly + on demand) executes it with no secrets;
  the test skips cleanly offline or on CRAN.

## rurl 1.4.0

### Dependencies

- The `pslr` dependency floor is now `>= 1.0.2` and the `punycoder` floor is
  `>= 1.2.0`. Those releases form the coordinated `punycoder 1.2.0`
  host-normalization API pair, so a fresh install pulls a compatible set;
  `rurl` should be submitted after both dependency updates are on CRAN.

### Accessor improvements

- `get_path()` gains `path_normalization`, `index_page_handling`,
  `trailing_slash_handling`, and `path_encoding` arguments, matching
  the corresponding options of `safe_parse_url()`.
- `get_scheme()` gains `scheme_relative_handling`.
- `get_parse_status()` gains `source` (mapped to `tld_source`) so
  warning statuses can be queried under a specific PSL section.
- `get_clean_url()` and `get_host()` gain `source` (mapped to `tld_source`).
- `get_host()` gains `host_encoding`.
- `get_domain()`, `get_tld()`, and `get_subdomain()` gain `host_encoding`,
  mirroring `get_host()`.

All new arguments default to the same values as `safe_parse_url()`, so
existing calls are unaffected.

### Behavior change

- The domain-family accessors (`get_domain()`, `get_tld()`,
  `get_subdomain()`) now follow `host_encoding` (default `"keep"`) instead
  of always returning Unicode. Under `"keep"` the emitted domain/TLD/
  subdomain mirrors the input host's own spelling: an A-label (`xn--…`)
  host yields A-label parts, a Unicode host yields Unicode parts. Pass
  `host_encoding = "unicode"` for the previous always-decoded output, or
  `"idna"` to force A-labels. This makes the domain accessors consistent
  with `get_host()`, whose `host_encoding` already defaulted to `"keep"`.

### Internal

- Parse-status string literals replaced by named constants
  (`R/status-constants.R`) and predicates (`.is_ok_status()`,
  `.is_warning_status()`, `.is_joinable_status()`).
- Cache touchpoints in `R/zzz.R` now driven from a single `.CACHE_REGISTRY`
  instead of repeating cache names by hand.
- Cleared the `lintr`/`goodpractice` findings across `R/` and the tests
  (e.g. `fixed = TRUE` dot splits, condition-message construction, dropped
  unnecessary lambdas) with no behavior change.
- `.lintr` now mirrors `goodpractice`'s linter set, so a local
  `lintr::lint_package()` matches the `goodpractice` report; intentional
  test-idiom deviations are documented in the config header.
- Restored 100% line coverage: added targeted tests for the
  `.punycode_to_unicode("")`, `.host_is_ace()`, and `.cache_enabled()`
  guard branches and the `derive_parse_status()` NA-host-dot fallback
  (and fixed an over-escaped regex literal that left the scheme-slash NA
  guard untested). The two genuinely unreachable `www`-prefix
  regex-capture fallbacks are now marked `# nocov` with justification.
- Reduced the cyclomatic complexity of `canonical_join()` (47→7),
  `get_subdomain()` (26→6), `rurl_cache_config()` (23→5), and
  `safe_parse_urls()` (19→3) by extracting named sub-helpers (e.g.
  `.cj_validate_inputs()`/`.cj_resolve_sides()`/`.cj_build_join_df()`,
  `.subdomain_labels()`, `.validate_max_full_parse()`,
  `.spu_coerce_original()`). No behavior change; no function in the package
  now exceeds the `goodpractice` cyclocomp threshold of 15.

### Documentation & metadata

- Added package-level documentation (`?rurl` / `man/rurl-package.Rd`) via a
  `"_PACKAGE"` sentinel, so the maintainer ORCID, package URLs, and the
  cross-promotion of `pslr`/`punycoder` now render on a help/landing page.
- Enabled roxygen2 markdown (`Roxygen: list(markdown = TRUE)`), regenerating
  all `man/*.Rd` (inline backticks now render as `\code{}`).
- Fixed the stale `inst/CITATION`: it now reads the version from package
  metadata (was hardcoded `0.2.0`), uses the correct title, and carries the
  maintainer ORCID. Added a root `CITATION.cff`.
- Added `X-schema.org-keywords`, the r-universe URL, and a `codemeta.json`
  for discoverability.
- Maintainer email simplified to `bartek@turczynski.pl`.

## rurl 1.3.0

### Dependencies

- Public Suffix List matching is now delegated to the `pslr` package
  (`Imports: pslr (>= 1.0.1)`). `rurl` no longer ships its own processed copy of
  the list (`R/sysdata.rda`) or its embedded matcher, and `data-raw/update_psl.R`
  has been removed. `punycoder` is now required at `>= 1.1.0`.

### Behavior changes (PSL correctness)

The embedded matcher used through 1.2.0 was not fully spec-correct. Delegating
to `pslr` fixes the following; outputs change accordingly:

- **Wildcard rules (`*.`)** are now honored by TLD extraction. For example
  `get_tld("a.b.kobe.jp")` is now `"b.kobe.jp"` (was `"kobe.jp"`).
- **Exception rules (`!`)** are now honored by TLD extraction. For example
  `get_tld("www.ck")` is now `"ck"` (was `"www.ck"`), and `get_tld("foo.ck")`
  is now `"foo.ck"` (was `"ck"`).
- **IDN hosts** now resolve a registered domain in every section. For example
  `get_domain("example.рф")` is now `"example.рф"` (was `NA`).
- `safe_parse_url()` / `safe_parse_urls()` now derive the `domain` field using
  the requested `tld_source` rather than always using the combined list, so
  `domain` and `tld` are consistent within a parse. Under
  `tld_source = "private"` (or `"icann"`), a host with no suffix in that section
  now has `domain = NA`; consequently `subdomain_levels_to_keep` is a no-op for
  such hosts (there is no registered domain to trim toward). The default
  `tld_source = "all"` is unaffected.
- Hosts under an unknown TLD continue to return `NA` for both domain and TLD
  (`rurl` queries `pslr` with `unknown = "na"`), rather than treating an unknown
  single label as a public suffix.

### Cache changes

- The per-host `domain` and `tld` memoization caches have been removed; `pslr`
  caches its own query results. `rurl_cache_config()` and `rurl_cache_info()`
  now cover only `full_parse`, `puny_encode`, and `puny_decode`, and the
  `domain` / `tld` arguments to `rurl_cache_config()` no longer exist.

## rurl 1.2.0

### Dependencies

- `punycoder` (used for IDNA/Punycode encoding and decoding) is now on CRAN.
  `DESCRIPTION` requires `punycoder (>= 1.0.0)`.

### Behavior changes

- The package-wide default for `case_handling` is now `"lower_host"` (was
  `"keep"` for `safe_parse_url()`, `safe_parse_urls()`, `get_clean_url()`, and
  the `get_*()` accessors, and `"lower"` for `get_path()`). This is the
  RFC 3986 §6.2.2.1 normalization: the case-insensitive scheme and host fold to
  lowercase while the case-sensitive path is preserved. With the previous
  defaults, hosts such as `WWW.Example.COM` and `www.example.com` did not fold
  to one identity, and `get_path()` silently lowercased paths (two pages that
  differ only by path casing collapsed to one). Pass `case_handling = "keep"`
  to restore the previous reconstruction, or `"lower"` to lowercase the whole
  URL including the path.

## rurl 1.1.0

### New features

- `canonical_join()` gains `name_A` / `name_B` arguments to set the output
  original-URL column names explicitly. They default to `NULL`, preserving the
  previous `deparse(substitute())` behavior; supply them for stable names when
  piping or passing anonymous inputs (e.g. `canonical_join(df[df$x > 1, ],
  get_b())`), which otherwise produced unstable column names.
- `canonical_join()` gains a `join_parse_status` argument controlling which
  parse statuses yield joinable keys. The default `"ok"` preserves the previous
  behavior (only `ok*` statuses join); `"ok_or_warning"` additionally treats
  the parseable-but-suspicious `warning-*` statuses (`warning-no-tld`,
  `warning-invalid-tld`, `warning-public-suffix`) as joinable, at the cost of
  more potential false-positive matches.

- Cache introspection and configuration. `rurl_cache_info()` reports the entry
  count, enabled state, and any bound for each memoization cache
  (`full_parse`, `domain`, `tld`). `rurl_cache_config()` enables or disables
  individual caches and sets an optional `max_full_parse` bound on the
  full-parse cache (default `Inf`, preserving the previous unbounded
  behavior); when the bound is reached the cache is reset so peak memory stays
  bounded. The `domain` and `tld` caches remain unbounded by design — they
  grow with the number of unique hosts, not with URL/option combinations — and
  can be disabled for workloads with very many unique hosts.

### Bug fixes

- `safe_parse_url()` now returns `port` as an integer (or `NA_integer_`), and
  `safe_parse_urls()` no longer errors on URLs that contain an explicit port
  (e.g. `http://example.com:8080/path`). Previously the scalar parser returned
  the port as a character string and the vectorized parser aborted.
- Bracketed IPv6 hosts (e.g. `http://[2001:db8::1]/`) are now correctly detected
  as IP hosts: `is_ip_host` is `TRUE`, `parse_status` is `"ok"`, and no
  TLD/domain derivation is attempted — matching how IPv4 hosts were already
  handled. An over-escaped detection pattern previously prevented this.

### Behavior changes (potentially breaking)

- `subdomain_levels_to_keep = N` (for `N > 0`) now keeps the `N` rightmost
  subdomain labels as documented, instead of silently retaining all subdomains.
  For example, `safe_parse_url("http://deep.sub.domain.example.com",
  subdomain_levels_to_keep = 1)` now returns host `domain.example.com` (was
  `deep.sub.domain.example.com`). `N = 0` (strip all) is unchanged. Code that
  relied on the previous no-op behavior for `N > 0` will see different output.

### Documentation

- Documented `clean_url` composition: it is a normalized canonical key built
  from scheme, host, and path only. Port, query, fragment, and userinfo are
  intentionally excluded, and with `path_encoding = "decode"` the path is shown
  decoded (human-readable, not guaranteed URL-safe). This matches the existing
  behavior and the key used by `canonical_join()` — no behavior change.
  Corrected a `lower_host` description that implied userinfo could be retained
  in `clean_url`, and fixed a README example whose input contained a literal
  space (now percent-encoded) so it parses as documented.

---

## rurl v1 (GitHub Release) - 2026-02-16

- Published first stable GitHub release tag: `v1`.
- Release notes added in `RELEASE_NOTES_v1.md`.
- GitHub release page: <https://github.com/bart-turczynski/rurl/releases/tag/v1>
- Package version for this release is `1.0.0` (see `DESCRIPTION`).

---

## rurl 0.3.0

This release adds powerful capabilities for URL normalization and canonical dataset joining. It significantly improves robustness in handling malformed or inconsistent URLs.

### Highlights

- New `case_handling` and `trailing_slash_handling` parameters in `safe_parse_url()` and `get_clean_url()` provide greater control over URL formatting.
- Introduced `canonical_join()` for joining datasets on normalized URL keys.
- Improved handling of non-standard or malformed schemes like `htp://`.
- Fixed parsing for schemeless URLs with ports (e.g., `example.com:8080/path`).
- More reliable fallback when `curl::curl_parse_url()` fails internally.
- Corrected regular expressions for IPv6 parsing.

---

## rurl 0.2.0

* First version for a potential CRAN submission.
* Fully tested across macOS, Windows, and Linux.
* Achieved 100% unit test coverage.
* Improved README and documentation.

This release adds robust support for internationalized domain names (IDNs),
improves punycode handling, and ensures accurate extraction of TLDs and
registered domains.

### Highlights
- Accurate TLD extraction for both ASCII and Unicode domains
- Graceful fallback when `urltools` is unavailable
- NFC normalization with `stringi`
- 100% test coverage with edge cases and punycode validation
- Improved internal helpers and clearer test diagnostics

## rurl 0.1.3

### Improvements

- Removed the dependency on the `psl` package.
- Implemented an internal registered domain extraction using the Public Suffix List.
- Added internal `update_psl.R` script to fetch and process the PSL during development.
- Improved test coverage to 100%.
- Cleaned up exports and internal helpers.
- Updated ignores.
- Tested on macOS, Windows, and Linux via rhub and win-builder.  
- CRAN checks pass with 0 errors/warnings and only standard notes.

### Documentation

- README updated to reflect the use of the PSL and internal domain logic.
- LICENSE and attribution clarified for MIT + Mozilla Public Suffix List.

## rurl 0.1.2

### Stabilization & Coverage

- Achieved **100% test coverage**.
- Added examples to all exported functions.
- Improved documentation (`@param`, `@return`, etc.) for CRAN compliance.
- Cleaned up `NAMESPACE` and removed unnecessary functions like `hello()`.
- Refined URL parsing logic and improved output consistency.

## rurl 0.1.0

- All `get_*()` functions are now vectorized and work on character vectors.
- Deprecated scalar-only behavior.
- Internal parsing made more robust using `curl` and `psl`.
- Ready for use in `mutate()` and other tidy workflows.
