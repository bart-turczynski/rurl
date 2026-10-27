# Standards-literal audit of ADRs, rulings, tests and dials

| Field | Value |
|---|---|
| id | `audit-standards-literal` |
| tracked_location | `design/work/url-v3/evidence/2026-10-28-standards-literal-audit.md` |
| purpose | ADR 0020 D1 sweep: find behavior an ADR, a ruling, a test or a dial pins that the selected standard calls invalid or that names a different URL; one numbered finding per ticket for the dispatcher |
| owning issue | `RURL-ongkcidw` |
| validation_command | none (a point-in-time audit; each finding names the probe that reproduces it) |

**Base commit.** Every probe below ran on `7764705f1ce463596e21c12845c8bbfa6b0538c3`
(`main` at "Merge branch 'feature/mailto-local-part-raw'"), loaded with
`devtools::load_all()`. The record is dated, not live (ADR 0014): it says what
was true at that commit.

**Home.** `evidence/` is archival (design/README.md): nothing reads it and it
is never the current authority. That fits a point-in-time audit, whose
authority passes to the tickets filed from it. No gate globs `evidence/`
(`tools/release-rule-check.R` excludes it by name; `verification/` would
trip `traceability-gate.R`'s unregistered-file check), and the frozen
`SHA256SUMS.txt` lists only the eleven graduated files, so a new file here
moves no checksum.

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
  `rfc3986` arm rejects 8 (findings 13 and 14); it accepts no string the ABNF
  rejects other than the two `//host` network-path references, which it
  reports as `ok-scheme-relative`, not `ok` (a relative reference, section
  4.2). Percent-encoded `reg-name` octets (`http://a%2Fb/`) parse, so the open
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

(Filled in by the next checkpoint.)
