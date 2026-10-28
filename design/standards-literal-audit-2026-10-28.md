# Standards-literal audit of ADRs, rulings, tests and dials

| Field | Value |
|---|---|
| id | `audit-standards-literal` |
| tracked_location | `design/standards-literal-audit-2026-10-28.md` |
| purpose | ADR 0020 D1 sweep: find behavior an ADR, a ruling, a test or a dial pins that the selected standard calls invalid or that names a different URL; one numbered finding per ticket for the dispatcher |
| owning issue | `RURL-ongkcidw` |
| validation_command | none (a point-in-time audit; each finding names the probe that reproduces it) |

**Base commit.** Every probe below ran on `7764705f1ce463596e21c12845c8bbfa6b0538c3`
(`main` at "Merge branch 'feature/mailto-local-part-raw'"), loaded with
`devtools::load_all()`. The record is dated, not live (ADR 0014): it says what
was true at that commit.

**Home.** A dated top-level snapshot, like `stringi-inventory.md`, not
`work/url-v3/evidence/`, which holds the frozen G0.1 bundle. The tickets filed
from it cite it as dated observation on the base commit, not as current
authority: the ADRs and the shipped tests stay that. The numbered findings
stand in for a claims table; each is settled by its own ticket.

**Out of scope.** The `url_standard = NULL` arm: RURL-mbhuhwzk retires it and
makes `NULL` read as `"whatwg"` (ADR 0020 D4), so NULL-only behavior is not
audited. Its defaults for `scheme_policy`, `scheme_acceptance` and
`scheme_relative_handling` are audited under `"whatwg"`, as RURL-mbhuhwzk
defers them here. Also out: fixing, performance, library comparison.
`clean_url` is a lossy policy projection (ADR 0017): a cleaning transform it
documents is not a finding unless it leaks into a component, a
`safe_parse_urls()` column or a serialization.

## What was read

- AGENTS.md, design/README.md, design/AGENTS.md, design/posture-card.md,
  ARCHITECTURE.md (pipeline, invariants), the record template.
- ADR 0001 through 0020, every one in full except ADR 0012 (read: TL;DR,
  D1-D7, the build-plan gates and both 2026-10-25 amendments; skimmed:
  Appendix A research notes) and ADR 0019 (read: Decision, Consequences,
  Semantic exceptions, Close-out headings).
- `design/work/url-v3/registers/rulings.md`, RUL-001 through RUL-031.
- Standards text, fetched read-only from url.spec.whatwg.org and
  rfc-editor.org (RFC 3986), for every clause quoted below. RFC 6068 and
  RFC 8089 clauses are cited from the ADRs that already quote them and checked
  only where a finding rests on them.

## Method

1. **Corpus.** 3,015 unique inputs: every `url` of
   `tests/testthat/fixtures/parse-corpus.csv` (119), every decodable
   `input_json` of `external-url-vectors.csv` (396), every `input` of
   `url-standard-conformance.csv` (75) and `encoding-fixtures.csv` (16), and
   every double-quoted literal in `tests/testthat/test-*.R` that starts with a
   scheme token from a fixed list (http, https, ftp, ftps, file, ws, wss,
   mailto, foo, data, urn, tel, javascript, sc, blob) followed by `:` (2,571
   literals before de-duplication). Rows that are not valid UTF-8 or that carry
   an R escape spelling were dropped. This is the systematic test sample: it
   takes every URL a test file writes as a literal, not a hand-picked subset,
   and judges what rurl returns for it against the standard. Where a
   divergence turned up, the test that pins the divergent value was located by
   `grep` and is named in the finding.
2. **WHATWG cross-check.** Each input was run through Node 26.12.1's `URL`
   (no base) to get the record the WHATWG basic URL parser yields (href,
   scheme, host, port, path, query, fragment, username, password). Node is the
   cross-check, not the authority: every finding below rests on the quoted
   clause, and the Node value is only the place where the probe first showed
   the gap.
3. **RFC 3986 cross-check.** Appendix A transcribed as one byte-mode PCRE
   (`URI` production, with the full `IPv6address`, `IPvFuture` and
   `dec-octet` alternatives), run over the same 3,015 inputs plus four
   IP-literal probes.
4. **Dial matrix.** Every non-default value of every `safe_parse_urls()` dial
   (38 settings) layered on two bases, `profile = "whatwg"`'s bundle and
   `url_standard = "rfc3986", scheme_acceptance = "general", scheme_policy =
   "require", scheme_relative_handling = "keep"`, comparing the columns
   `parse_status`, `scheme`, `host`, `port`, `path`, `query`, `fragment`,
   `user`, `password`, `domain`, `tld` and `is_ip_host` with the base row by
   row. `clean_url` was excluded on purpose: a dial that moves only
   `clean_url` is cleaning policy.
5. **Accessors and resolution.** Targeted probes of `get_*()`, `serialize_url()`,
   `resolve_url(output = "serialized")` over the 274 WPT base-relative rows,
   and the 41 normal examples of RFC 3986 section 5.4.

Probe scripts and raw output: the unit's scratch directory (not committed);
each finding repeats its reproducing call and output.

## What was found clean

- **`serialize_url(standard = "whatwg")`.** Exact on all 2,526 corpus rows
  the WHATWG parser accepts, and it rejects exactly the 489 it rejects (0
  divergences either way). Query, fragment, username, password, scheme and
  port columns of the `profile = "whatwg"` parse also match the WHATWG record
  on every row both accept (RUL-026's triplet case included).
- **`resolve_url(output = "serialized", url_standard = "whatwg")`.** 274/274
  WPT base-relative rows exact through the public entry point (the
  ARCHITECTURE.md "247 of 274, 27 enumerated" sentence is stale; the test
  file says 274/274).
- **RFC 3986 section 5.4.** All 41 normal examples exact under
  `url_standard = "rfc3986"`, `form = "source"`. Under `form = "normalized"`
  only `//g` differs (`http://g/`), which section 6.2.3's empty-path rule for
  `http` sanctions.
- **RFC 3986 acceptance.** Of 2,394 ABNF-valid URIs in the corpus, rurl's
  `rfc3986` arm rejects 8 (findings 13 and 14); on the `scheme_policy =
  "require"` base of the method it accepts no string the ABNF rejects other
  than the two `//host` network-path references, which it
  reports as `ok-scheme-relative`, not `ok` (a relative reference, section
  4.2). Under the default `"infer"` it also accepts scheme-less input such as
  `example.com` (finding 1). Percent-encoded `reg-name` octets (`http://a%2Fb/`) parse, so the open
  question ADR 0013 left is closed in the tree.
- **Governed dials.** `case_handling` (`keep`, `lower`, `upper`) and
  `path_normalization` (`none`, `collapse_slashes`, `both`) are errors under
  both named standards; the selector's own value is a no-op. No component
  moves.
- **Clean-surface-only dials.** `query_handling`, `params_keep`,
  `sort_params`, `empty_param_handling`, `decode_plus`, `credential_handling`
  and `port_handling = "strip_default"` / `"strip_all"` move no column under
  either base. `protocol_handling = "none"` moves none either.
- **`tld_source`** moves `domain`, `tld` and one `parse_status`
  (`warning-public-suffix`), which are PSL facts no URL standard defines.
- **`get_mailto_recipients()`** reads only the positional `to` list and says
  so in its roxygen (hfield recipients are documented out of scope). The
  obs-qp NUL classification is RURL-mxsyzrho, not refiled.

### ADR verdicts

| ADR | Verdict |
|---|---|
| 0001 PSL seam | Clean. `domain` / `tld` are PSL facts; no standard defines them. |
| 0002 Punycode helpers | Clean for `whatwg` (RUL-002 routes it around the helpers). Under `rfc3986` the `idna` presentation decodes percent-triplets first: finding 10. |
| 0003 Stage split | Structural. It places www, subdomain, trailing-slash, index-page, scheme and port presentation in Stage B, which writes the same record the component columns read: the mechanism under findings 6-9. |
| 0004 Host-shape gate | Its closed allowlist and scheme inference are the defaults of findings 1 and 2. Its WHATWG forbidden-host rejection and ends-in-a-number rule are clean (corpus exact). |
| 0005, 0019 String operations | Clean; implementation semantics, no URL output pinned. |
| 0006 Companion helpers | Clean. |
| 0007 Selector | `NULL` parts out of scope. "Nulls default ports in parse output" holds, but `port_handling = "keep"` undoes it: finding 9. |
| 0008, 0014 Process | No behavior. |
| 0009, 0013 Host charset | Clean: the 15 WHATWG code points parse under `whatwg`, the 11 `sub-delims` under `rfc3986`. |
| 0010 `scheme_policy` | Its `"infer"` default: finding 1. `"require"` is clean. |
| 0011 `path_encoding` | `encode` fixed (RURL-dbyavcjt); `decode` is RURL-eokizmdi. Its "`keep` = identity presentation" leaves the `whatwg` path column unencoded: finding 5. |
| 0012 General parser | D3/D4 `web` default: finding 2. D7 mailto accessors: finding 11. D2's IPvFuture host form is unreachable on the web route under `rfc3986`: finding 13. D2's `/.` guard and D5 are clean. |
| 0015, 0016 | Superseded by ADR 0020; `NULL` only. |
| 0017 `clean_url` | The clean surface itself is clean by definition. Its rows 3-6 (https, www, trailing slash, index page) also rewrite component columns when passed as dials: findings 6-8. |
| 0018 IP literals | Clean (WHATWG IPv4 host exact on the corpus). |
| 0020 | Governing. |

### Ruling verdicts

| Ruling | Verdict |
|---|---|
| RUL-001 | Clean: `credential_handling` moves no column. |
| RUL-002 | Pins a Unicode host in the `whatwg` parse record under the default `host_encoding = "keep"`: finding 4. |
| RUL-003 | Clean: the RFC 9110 empty-host rejection sits on `scheme_acceptance = "web"`, a scheme overlay. |
| RUL-004 | Superseded in substance by ADR 0020 D5; carried by RURL-eokizmdi. |
| RUL-005, RUL-016, RUL-017 | Clean surface; clean. (`port_handling = "keep"` leaking into the `port` column is finding 9, not RUL-016's ruling.) |
| RUL-006, 008-013, 018-020 | Gate shape, registers, scope, process: no URL output. |
| RUL-007, RUL-015 | `form = "source"` reproduces scheme, query and fragment bytes, but not the port: finding 15. |
| RUL-014 | Clean (`example.com:8080/x` is scheme `example.com` under `require`, as both standards read it). |
| RUL-021, 022, 024 | Clean: `check_schemes()` is descriptive and moves no parse. |
| RUL-023, RUL-025 | Clean: diagnostics only. |
| RUL-026 | Clean (query and fragment columns exact on the corpus). |
| RUL-027, RUL-028 | Clean (section 5.4 examples; the section 3.3 `//` guard holds). |
| RUL-029 | `NULL` only: out of scope. |
| RUL-030 | Clean: strict US-ASCII is RFC 3986 section 2. |
| RUL-031 | (1) and the `unicode` display form are clean under ADR 0020 D5. Its claim that `"idna"` is a form that parses again is false under `rfc3986`: finding 10. |

### Dials x standards matrix

Each cell names what the dial moves beyond `clean_url`, on the 3,015-row
corpus. "-" means no column moved.

| Dial value | `whatwg` | `rfc3986` |
|---|---|---|
| `path_encoding = "encode"` | path 82 (to the WHATWG path; see finding 5) | - |
| `path_encoding = "decode"` | path 204 (RURL-eokizmdi) | path 183 (RURL-eokizmdi) |
| `host_encoding = "idna"` | host 48 (to the WHATWG host; see finding 4) | host 131: finding 10 |
| `host_encoding = "unicode"` | host 52 (documented display form) | host 181 (documented display form, RUL-031) |
| `decode_plus = TRUE` | - | - |
| `port_handling = "keep"` | port 48: finding 9 | - |
| `port_handling = "strip_default"`, `"strip_all"` | - | - |
| `case_handling` (non-default) | error (governed) | error (governed) |
| `path_normalization` (non-default) | error (governed) | error (governed) |
| `query_handling`, `sort_params`, `empty_param_handling`, `params_*` | - | - |
| `protocol_handling = "none"` | - | - |
| `protocol_handling = "strip"`, `"http"`, `"https"` | status, scheme, host, query, user, domain, tld: finding 6 | status, scheme, host, user, domain, tld: finding 6 |
| `www_handling = "strip"`, `"keep"`, `"if_no_subdomain"` | host (83 / 1,652 / 925), status: finding 7 | host (65 / 1,451 / 730), status: finding 7 |
| `subdomain_levels_to_keep = 0` | host 85: finding 7 | host 79: finding 7 |
| `trailing_slash_handling = "keep"`, `"strip"` | path 1,169 / 106: finding 8 | path 769 / 61: finding 8 |
| `index_page_handling = "strip"` | path 36: finding 8 | path 34: finding 8 |
| `credential_handling = "reject"` | - | - |
| `tld_source = "icann"` | domain, tld (PSL) | domain, tld (PSL) |
| `scheme_policy = "infer"` (default) | 12 rows error -> ok: finding 1 | 10 rows error -> ok: finding 1 |
| `scheme_acceptance = "web"` (default) | 454 rows ok -> error: finding 2 | 550 rows ok -> error: finding 2 |
| `scheme_relative_handling = "keep"` (default), `"http"`, `"https"` | `//host` error -> ok: finding 3 | `"keep"` is the base (a relative reference, section 4.2); `"http"`/`"https"` add a scheme the input lacks: finding 3 |

## Findings

Every call below ran on base `7764705` in a fresh `Rscript` process, twice
(identical output both times). Output is shown as R's `deparse()` prints it.
Severity uses three classes: **different resource** (the output names another
URL than the standard's processing of the input), **rejected string** (the
output is, or parses as, something the standard calls failure or invalid),
and **other** (stated per finding).

Known findings are cross-referenced, not refiled: RURL-dbyavcjt (`encode`
`%2F` fold, fixed), RURL-eokizmdi (`decode` folds reserved triplets),
RURL-mxsyzrho (mailto obs-qp NUL), RURL-rqibprtg (non-ASCII hosts with an
empty label), RURL-mbhuhwzk (the `NULL` default).

### 1. The default `scheme_policy = "infer"` accepts scheme-less input under both named standards

- **Clause.** WHATWG URL Standard, no scheme state: "If base is null, or base
  has an opaque path and c is not U+0023 (#), missing-scheme-non-relative-URL
  validation error, return failure." RFC 3986 section 4.2: "A relative
  reference that does not begin with a slash character is termed a
  relative-path reference"; `example.com` has no authority there.
- **Call.**
  `safe_parse_urls(c("example.com", "12345"), url_standard = "whatwg")[, c("parse_status", "scheme", "host", "port", "path")]`
  and `safe_parse_urls("example.com", url_standard = "rfc3986")`.
- **Observed.** whatwg: `parse_status = c("ok", "ok")`, `scheme = c("http",
  "http")`, `host = c("example.com", "0.0.48.57")`, `path = c("/", "/")`.
  rfc3986: `ok`, `http`, `example.com`, path `""`.
- **Expected.** whatwg: failure for both (no base). rfc3986: not a URI; as a
  relative reference its path is `example.com` and it has no host.
- **Control.** `profile = "whatwg"` gives `error` for `example.com` and `ok`
  for `http://example.com`.
- **Severity.** Rejected string under `whatwg` (an `ok` row for input the
  parser fails); different resource under `rfc3986` (a path becomes a host).
- **Pinned by.** ADR 0010 (`"infer"` default), ADR 0012 D6 "foot-gun
  (intended)"; `tests/testthat/test-rfc3986-grammar.R`
  (`get_parse_status("example.com/path", url_standard = "rfc3986")` is
  `"ok"`); the `scheme_policy` roxygen in `R/parse.R`. RURL-mbhuhwzk defers
  the default to this audit.

### 2. The default `scheme_acceptance = "web"` rejects URLs the WHATWG parser accepts

- **Clause.** WHATWG URL Standard, scheme state: a scheme is any
  `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )` followed by `:`; a non-special
  scheme without `/` goes to the opaque path state, and `ws`/`wss` are special
  schemes. The basic URL parser has no allowlist.
- **Call.**
  `safe_parse_urls(c("mailto:a@b.com", "ws://h/", "foo://h/p", "data:,x"), url_standard = "whatwg")$parse_status`
- **Observed.** `c("error", "error", "error", "error")`.
- **Expected.** Four parsed URLs; `serialize_url()` on the same strings gives
  `c("mailto:a@b.com", "ws://h/", "foo://h/p", "data:,x")`.
- **Control.** With `scheme_acceptance = "general"`:
  `c("ok", "warning-no-tld", "ok", "ok")`. Over the corpus the default turns
  454 accepted rows into `error` under `whatwg` and 550 under `rfc3986`.
- **Severity.** Other: rejects valid input (the `parse_status` claims a
  failure the standard does not report).
- **Pinned by.** ADR 0004 (closed set), ADR 0012 D3/D4 and its v3.0
  amendment ("`scheme_acceptance = "web"` ... remain the default admission
  axes"); `tests/testthat/test-scheme-acceptance.R` ("unsupported scheme
  errors under any protocol_handling"). ADR 0020 Consequences hands this to
  the audit.

### 3. The default `scheme_relative_handling = "keep"` (and `"http"`, `"https"`) parses `//host` under `whatwg`

- **Clause.** WHATWG URL Standard, no scheme state (quoted in finding 1):
  `//example.com/p` has no scheme and no base, so the parser returns failure.
- **Call.**
  `safe_parse_urls("//example.com/p", url_standard = "whatwg")` and the same
  with `scheme_relative_handling = "https"`.
- **Observed.** `keep`: `parse_status = "ok-scheme-relative"`, `scheme = NA`,
  `host = "example.com"`, `path = "/p"`. `https`: `ok`, `https`,
  `example.com`, `/p`.
- **Expected.** Failure.
- **Control.** `profile = "whatwg"` (`scheme_relative_handling = "error"`)
  gives `error`.
- **Severity.** Rejected string (an accepted row, and under `"http"` /
  `"https"` a fabricated scheme). Under `rfc3986` the `keep` reading is a
  valid network-path reference (section 4.2) and stays clean; `"http"` /
  `"https"` there still add a scheme the input lacks.
- **Pinned by.** ADR 0010 (`//host` single-governed by this dial, default
  `keep`); `tests/testthat/test-accessors.R` and `test-parse-verdicts.R`
  (`ok-scheme-relative` rows).

### 4. Under `whatwg` the default `host_encoding = "keep"` puts a Unicode host in the parse record

- **Clause.** WHATWG URL Standard, host parser: "Let asciiDomain be the
  result of running domain parser with domain and false." ... "Return
  asciiDomain." A URL's host is that ASCII domain.
- **Call.** `get_host("http://bücher.de/", url_standard = "whatwg")` and
  `safe_parse_urls("http://b%C3%BCcher.de/", profile = "whatwg")$host`.
- **Observed.** `c("bücher.de", "bücher.de")`;
  `serialize_url("http://bücher.de/", standard = "whatwg")` is
  `"http://xn--bcher-kva.de/"`.
- **Expected.** `"xn--bcher-kva.de"` in both.
- **Control.** `get_host("http://xn--bcher-kva.de/", url_standard = "whatwg")`
  and `host_encoding = "idna"` both give `"xn--bcher-kva.de"`, and an ASCII
  host (`http://Example.COM/`) gives `"example.com"`. The same host thus has
  two spellings in the record depending on how the input wrote it. 48 corpus
  rows differ from the WHATWG host.
- **Severity.** Other: same host, but a component value the standard's
  record never holds (the documented display form is `"unicode"`, not the
  default).
- **Pinned by.** RUL-002 ("Both render `ToUnicode(ToASCII(host))`");
  `tests/testthat/test-url-standard-host.R` ("WHATWG default and unicode
  presentations map the host": `safe_parse_urls(urls, profile = "whatwg")$host`
  is the Unicode `mapped`); the `host_encoding` roxygen in `R/parse.R`.

### 5. Under `whatwg` the path column is not percent-encoded with the path percent-encode set

- **Clause.** WHATWG URL Standard, path state: "UTF-8 percent-encode c using
  the path percent-encode set and append the result to buffer."
- **Call.** `get_path("http://ex.com/école", url_standard = "whatwg")` and
  `safe_parse_urls("https://example.com/a<b>c?q<r>", profile = "whatwg")$path`.
- **Observed.** `c("/école", "/a<b>c")`, while the same row's `query` is
  `"q%3Cr%3E"` and `serialize_url()` gives `"http://ex.com/%C3%A9cole"` and
  `"https://example.com/a%3Cb%3Ec?q%3Cr%3E"`.
- **Expected.** `c("/%C3%A9cole", "/a%3Cb%3Ec")`.
- **Control.** An already-encoded path stays as written
  (`get_path("http://ex.com/a%2Fb", url_standard = "whatwg")` is `"/a%2Fb"`),
  and `path_encoding = "encode"` gives `"/%C3%A9cole"`. 82 corpus rows differ
  from the WHATWG path; the query and fragment columns differ on none.
- **Severity.** Rejected string for `<`, `"` and the other non-URL code
  points, which the path state reports as an `invalid-URL-unit`; other for a
  non-ASCII code point such as `é`, which is a URL code point (U+00A0 to
  U+10FFFD) and valid input but is still not the path the standard's record
  holds, the percent-encoded one (same resource in both cases).
- **Pinned by.** ADR 0011 ("`keep` = identity presentation"); the
  `path_encoding` roxygen ("Under `url_standard = "whatwg"`, existing
  percent-triplet spelling is preserved byte-for-byte"); the `@return` `path`
  item, which unlike `query` states no encoding;
  `tests/testthat/test-encoding-fixtures.R` ("url_standard does not force path
  percent-encoding by default").

### 6. `protocol_handling` rewrites the scheme, status, host and authority columns

- **Clause.** WHATWG URL Standard, scheme state (the scheme is what the input
  spells); a non-special scheme with no `//` has an opaque path, and "A URL's
  host is null or a host". RFC 3986 section 3 (`hier-part`: an authority only
  after `//`).
- **Call.**
  `safe_parse_urls("mailto:user@example.com", url_standard = "whatwg", scheme_acceptance = "general", protocol_handling = "https")[, c("parse_status", "scheme", "host", "user", "path")]`;
  `get_scheme("ftp://h/x", protocol_handling = "https", url_standard = "whatwg")`;
  `safe_parse_urls(c("HTTP://EXAMPLE.COM/", "file://localhost/x"), url_standard = "whatwg", protocol_handling = "strip")`.
- **Observed.** `ok`, `https`, `example.com`, `user`, `user@example.com`;
  `"https"`;
  `strip`: scheme `NA` for both, and `file://localhost/x` becomes `error`.
- **Expected.** The parse of the input: `mailto`, host `NA`, user `NA`, path
  `user@example.com`; `get_scheme()` `"ftp"`; `file://localhost/x` parses
  (host empty, path `/x`).
- **Control.** Without the dial the mailto row is `ok`, `mailto`, `NA`, `NA`,
  `user@example.com`. Over the corpus `"https"` moves the scheme of 2,151
  rows, the status of 279, and the query encode set of 8 (a `foo:` row gains
  the special-query `'` escape).
- **Severity.** Different resource (`mailto:user@example.com` reads as an
  `https` URL with host `example.com`).
- **Pinned by.** ADR 0010's own table says `protocol_handling` answers only
  "How is the scheme presented in `clean_url`?", so the leak contradicts it;
  `tests/testthat/test-accessors.R` ("get_scheme respects protocol_handling")
  pins the accessor behavior.

### 7. `www_handling` and `subdomain_levels_to_keep` rewrite the host column

- **Clause.** WHATWG URL Standard, host parser (the host is the parsed input
  domain); RFC 3986 section 3.2.2 (`host` is the `reg-name` as written).
  `www.example.com` and `example.com` are different hosts in both.
- **Call.**
  `get_host("http://example.com/", www_handling = "keep", url_standard = "whatwg")`,
  `get_host("http://www.example.com/", www_handling = "strip", url_standard = "whatwg")`,
  `get_host("https://blog.example.com/", subdomain_levels_to_keep = 0, url_standard = "whatwg")`,
  `get_parse_status("http://www./", www_handling = "strip", url_standard = "whatwg")`.
- **Observed.** `c("www.example.com", "example.com", "example.com")`; the
  status is `"error"` (was `warning-invalid-tld`).
- **Expected.** `c("example.com", "www.example.com", "blog.example.com")` and
  the input's own status.
- **Control.** Without the dials `get_host("http://www.example.com/",
  url_standard = "whatwg")` is `"www.example.com"`, matching
  `serialize_url()`.
- **Severity.** Different resource.
- **Pinned by.** ADR 0003 (www/subdomain policy is Stage B, which writes the
  record); ADR 0017 D2 row 4 licenses the strip on the clean surface only;
  `tests/testthat/test-whatwg-host-policy-mapping.R` (pins `p$host` under
  every `www_handling` with `subdomain_levels_to_keep = 0`, both `whatwg`
  postures); `test-offsets-parse-phases.R` and `test-engine-per-request.R`
  (`subdomain_levels_to_keep = 0` on `$host` / `get_host()`).

### 8. `trailing_slash_handling` and `index_page_handling` rewrite the path column

- **Clause.** WHATWG URL Standard, path state (the path is the input's
  segments); RFC 3986 section 6.2.3 sanctions no trailing-slash or index-page
  equivalence; `/dir` and `/dir/` are different paths in both.
- **Call.**
  `get_path("https://example.com/dir/file.txt", trailing_slash_handling = "keep", url_standard = "whatwg")`,
  the same with `"https://example.com/dir/"` and `"strip"`, and
  `get_path("https://example.com/foo/index.html", index_page_handling = "strip", url_standard = "whatwg")`.
- **Observed.** `c("/dir/file.txt/", "/dir", "/foo")`.
- **Expected.** `c("/dir/file.txt", "/dir/", "/foo/index.html")`, which is
  what the same calls give without the dials.
- **Severity.** Different resource.
- **Pinned by.** ADR 0017 D2 rows 5-6 license both on the clean surface only;
  `tests/testthat/test-offsets-query-accessors.R`
  (`get_path(url, index_page_handling = "strip", ...)`).

### 9. `port_handling = "keep"` writes a default port into the `whatwg` port column

- **Clause.** WHATWG URL Standard, port state: "Set url's port to null, if
  port is url's scheme's default port; otherwise to port."
- **Call.**
  `c(safe_parse_urls("http://example.com:80/", url_standard = "whatwg", port_handling = "keep")$port, get_port("http://example.com:80/", url_standard = "whatwg"))`
- **Observed.** `c(80L, NA)`.
- **Expected.** `NA` in both.
- **Control.** Without the dial the column is `NA` for `:80` and `8080` for
  `:8080`. 48 corpus rows move.
- **Severity.** Other: equivalent URL, but the column holds a value the
  record nulls (`get_port()` has no `port_handling` dial, so it gives `NA`).
- **Pinned by.** The `port_handling` roxygen ("an explicit non-parity
  override for callers that need the input's port spelling"), which describes
  `clean_url`, not the column.

### 10. `host_encoding = "idna"` under `rfc3986` percent-decodes the reg-name into a different or invalid host

- **Clause.** RFC 3986 section 3.2.2: `reg-name = *( unreserved / pct-encoded
  / sub-delims )`; section 2.2: "URIs that differ in the replacement of a
  reserved character with its corresponding percent-encoded octet are not
  equivalent."
- **Call.**
  `get_host(c("http://%25/", "http://hello%00/", "http://a%2Fb/"), url_standard = "rfc3986", host_encoding = "idna")`
- **Observed.** `c("%", "hello", "a/b")`. Re-parsing `http://<that>/` under
  `rfc3986` gives host `c(NA, "hello", "a")`.
- **Expected.** A host that is the same reg-name (`"%25"`, `"hello%00"`,
  `"a%2Fb"`, which `host_encoding = "keep"` gives) or its IDNA form where one
  exists; never a lone `%`, a dropped `%00`, or a `/` that ends the authority.
- **Severity.** Different resource (`a%2Fb` becomes host `a` with path
  `/b/`) and rejected string (`http://%/`). 131 corpus rows move.
- **Pinned by.** RUL-031 (2): "`"idna"` and `"keep"` are the forms that parse
  again", which this contradicts; ADR 0002 (the helpers tolerate malformed
  hosts). Possibly the same root as RURL-rqibprtg's host helpers; filed
  separately because the input here is ASCII.

### 11. The mailto accessors report a host, user and host type for a URL with no host

- **Clause.** WHATWG URL Standard: "A URL's host is null or a host"; a
  non-special scheme not followed by `/` goes to the opaque path state, which
  sets no host. RFC 3986 section 3: an authority exists only after `//`.
- **Call.** For `std` in `"whatwg"`, `"rfc3986"`:
  `get_host("mailto:a@b.com", url_standard = std, scheme_acceptance = "general")`,
  likewise `get_user()`, `get_domain()`, `get_host_type()`, and
  `safe_parse_urls(...)$host`.
- **Observed.** host `"b.com"`, user `"a"`, domain `"b.com"`, host type
  `"domain"`, record host `NA`, under both standards.
- **Expected.** `NA` for host and user, and no host type, matching the parse
  record. `get_domain()` reads a PSL fact no URL standard defines; it follows
  whatever `get_host()` gives.
- **Control.** `get_host("foo:a@b.com", url_standard = "whatwg",
  scheme_acceptance = "general")` is `NA`.
- **Severity.** Other: a component accessor names a component the URL does
  not have. The recipient domain is real email metadata; the defect is that
  `get_host()` and `get_host_type()` present it as the URL's host.
- **Pinned by.** ADR 0012 D7 and its T1 amendment ("`safe_parse_url`(mailto)
  `$host` and `get_host`(mailto) now deliberately diverge"); RUL-022 (a)
  records the same divergence. An owner ruling may keep it as documented
  extraction metadata; ADR 0020 D5 holds a component accessor to the
  standard, so it is filed.

### 12. `get_query()`'s default `decode = TRUE` folds reserved triplets into delimiters

- **Clause.** RFC 3986 section 2.2 (quoted in finding 10); WHATWG URL
  Standard, query state (the query is the percent-encoded string; `%26` is
  data, `&` a separator in `application/x-www-form-urlencoded`).
- **Call.**
  `get_query(c("http://h/?a=%26b&c=1", "http://h/?k=%3D%23"), url_standard = "whatwg")`
- **Observed.** `c("a=&b&c=1", "k==#")`.
- **Expected.** A query string that is the URL's query:
  `c("a=%26b&c=1", "k=%3D%23")` (what `decode = FALSE` gives).
- **Control.** A non-reserved triplet decodes harmlessly:
  `get_query("http://h/?a=caf%C3%A9", url_standard = "whatwg")` is
  `"a=café"`; `format = "list"` splits before decoding and keeps the
  structure.
- **Severity.** Different resource: the returned string has three parameters
  where the URL has two. This is RURL-eokizmdi's question (is a decoding
  accessor a display form?) on another accessor, and here the decoding form is
  the default.
- **Pinned by.** `R/accessors.R` `get_query` roxygen ("still percent-decodes
  for readability (`decode = TRUE`)", and its security note).

### 13. `rfc3986` rejects an IPvFuture literal on the web route

- **Clause.** RFC 3986 section 3.2.2: `IP-literal = "[" ( IPv6address /
  IPvFuture ) "]"`, `IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims
  / ":" )`.
- **Call.**
  `safe_parse_urls(c("http://[v1.x]/", "http://[v7.aBc]/", "http://[v1.fe80::1]/"), url_standard = "rfc3986", scheme_acceptance = "general")$parse_status`
- **Observed.** `c("error", "error", "error")`.
- **Expected.** Parsed, with an IPvFuture host.
- **Control.** `foo://[v1.x]/` on the general route is `ok`, and
  `http://[::1]/` is `ok`. The grammar gate itself accepts
  `http://[v1.fe80::1]/` (the `ipvfuture` row of
  `tests/testthat/fixtures/rfc3986-abnf-fixtures.csv` expects `accept`), so the
  gate and the full parse disagree.
- **Severity.** Other: rejects a valid URI.
- **Pinned by.** `tests/testthat/test-parse-web.R`
  (`expect_null(p("http://[v1.x]/"))   # no IPvFuture`), a web-parser unit
  test that holds for `whatwg` but is reached by `rfc3986` too. ADR 0012 D2
  promises the `ipvfuture` host form for the RFC posture.

### 14. `rfc3986` rejects a port above 2^31 - 1 on the web route

- **Clause.** RFC 3986 section 3.2.3: `port = *DIGIT`; "The type of port
  designated by the port number (e.g., TCP, UDP, SCTP) is defined by the URI
  scheme." The posture card: the 16-bit ceiling is "never a syntax gate under
  `rfc3986`".
- **Call.**
  `safe_parse_urls(c("http://example.com:2147483648/", "foo://h:99999999999/"), url_standard = "rfc3986", scheme_acceptance = "general")$parse_status`
- **Observed.** `c("error", "ok")`.
- **Expected.** `ok` for both.
- **Control.** `http://example.com:99999/` and `:2147483647/` are `ok` with
  ports `99999L` and `2147483647L`; the general route accepts the 11-digit
  port.
- **Severity.** Other: rejects a valid URI. The cause is the record's
  `integer(1)` port type, as the pinning test says.
- **Pinned by.** `tests/testthat/test-parse-web.R`
  (`expect_null(pu("http://example.com:2147483648/"))`, "a representation
  limit, not the transport bound under another name").

### 15. `serialize_url(standard = "rfc3986", form = "source")` rewrites the port

- **Clause.** RFC 3986 section 5.3, component recomposition:
  `if defined(authority) then append "//" to result; append authority to
  result;`. Rewriting `080` as `80` or dropping an empty port's `:` is
  normalization (section 3.2.3 lets "URI producers and normalizers" omit an
  empty port's `:`), which `form = "source"` disclaims.
- **Call.**
  `serialize_url(c("http://example.com:080/", "http://example.com:/", "foo://host:/", "http://example.com:0000/"), standard = "rfc3986", form = "source")`
- **Observed.** `c("http://example.com:80/", "http://example.com/",
  "foo://host/", "http://example.com:0/")`.
- **Expected.** The input strings, byte for byte.
- **Control.** `http://example.com:8080/` and
  `HTTP://Example.COM/a/../%7e` come back exactly as written. 22 corpus rows
  move, all on the port.
- **Severity.** Other: an equivalent URI under the scheme's port semantics,
  but not the source form the dial claims.
- **Pinned by.** RUL-007 describes `source` as the form with no
  normalization (its "reproduces the input bytes" names the scheme, query and
  fragment, not the port), and this port rewrite is a normalization;
  `tests/testthat/test-parse-web.R`
  (`expect_identical(p("http://example.com:080/")$port, "80")` pins the
  normalized port string `"80"`, which is where the spelling is lost).

## Open cells

None: every finding has its own ticket (label `standards-audit`).

## Consequences

Each fix moves the tests named in its "Pinned by" line and adds a `NEWS.md`
bullet citing the clause (ADR 0020 D1). Findings 1-3 are RURL-mbhuhwzk's
deferred defaults and change default output. Findings 4, 5, 9 and 11 change
the `whatwg` record and so the `safe_parse_urls()` columns the URL key reads
(check `get_url_key()`'s key version). Findings 6-8 share one mechanism
(Stage B presentation writing the record, ADR 0003) and may land as one
change. ARCHITECTURE.md's "247 of 274 rows exact, 27 enumerated differences"
was stale against the 274/274 its own test pins; the MR that adds this record
corrects it.
