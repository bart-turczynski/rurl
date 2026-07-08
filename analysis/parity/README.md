# Standard-conformance parity — frozen run

Companion to the [disagreement study](../disagreement/). That one measures how
much parsers *diverge from each other*; **this one measures how close rurl is to
the STANDARDS THEMSELVES** — the number to close conformance issues against.

Harness: [`inst/bench/standard-parity.R`](../../inst/bench/standard-parity.R).
Regenerate into a scratch dir with

```r
devtools::load_all(); source("inst/bench/standard-parity.R")
```

or freeze here with `RURL_PARITY_OUT=analysis/parity Rscript …`.

## Oracles

| Standard | Oracle | Provenance |
|---|---|---|
| **WHATWG** | `inst/bench/wpt-url-cases.json` — 176 success (with expected components) + 202 failure cases | derived from **web-platform-tests** `url/resources/urltestdata.json` (BSD-3-Clause, "web-platform-tests contributors") by `make-wpt-fixture.py`. The spec's own conformance suite. |
| **RFC 3986** | `inst/bench/rfc3986-probes.csv` — 19 probes | hand-authored against the RFC's grammar/§6.2.2 rules (no official RFC test suite exists), each row tagged with its section. |

Success cases are limited to the schemes rurl supports (`http/https/ftp/file`) —
the *"additional protocols notwithstanding"* carve-out; non-special schemes
(`mailto:`, `data:`, `ws:`, …) are out of scope by design.

WHATWG success is scored in rurl's **canonical-output config**
(`scheme_policy="require", host_encoding="idna", path_encoding="encode"`) — the
settings that ask rurl for the WHATWG serialization. A residual mismatch there
is genuine: **the spec output cannot be reached by any knob.**

## Headline (rurl 2.5.0, 2026-07-08)

| Metric | Result |
|---|---|
| WHATWG — success **accepted** | 145/176 (82%) |
| WHATWG — success **full-component parity** | **120/176 (68%)** |
| WHATWG — failure **correctly rejected** | 202/202 (100%) |
| WHATWG — overall acceptance conformance | 347/378 (92%) |
| RFC 3986 — probes passed | 19/19 (100%) |

rurl **never accepts a URL WHATWG rejects** among these 202 failure cases
(100%). Every WHATWG shortfall is either **over-strict** (rejects/normalizes
something the spec accepts) or a **serialization** difference — not a dangerous
over-acceptance. The RFC probe set is now fully conformant for the covered
grammar and normalization cases.

## Where rurl falls short of the standard

### RFC 3986 — covered probes fully pass
rurl(rfc3986) now accepts reg-names containing the RFC 3986 §3.2.2
**sub-delims** (`! $ & ' ( ) * + , ; =`) and passes the normalization probes
(case folding, unreserved decode, `%2f`→`%2F` reserved-preserve, dot-segment
resolution).

### WHATWG — three buckets
Component non-conformances among accepted cases: **path 22, port 3**;
plus **31 over-strict rejections** (28 of them `file:`).

1. **`file:` scheme (28 rejects + 5 path rows).** rurl rejects `file:` URLs
   WHATWG accepts — drive letters (`file:C|/m/`, `file:///Y:`), bare/empty
   (`file:`, `file:.`, `file:?q=v`, `file:#frag`), backslash (`file:\\//`), and
   host+drive (`file://example.net/C:/`, `file://[1::8]/C:/`) — and mis-resolves
   several `file:` paths. Biggest single gap.
2. **Default-port elision (systematic).** `http://foo:80/` keeps `:80`; WHATWG
   nulls the default port (`.port === ""`). Every default-port URL diverges.
3. **Path percent-encoding (22 rows), unreachable by any knob.**
   `path_encoding="encode"` (a) over-encodes characters *outside* WHATWG's path
   encode-set (`|`→`%7C`, `@`→`%40`, `(`→`%28`, `:`→`%3A`); (b) double-encodes
   `%` (`foo%`→`/foo%25`, `%2`→`%252`); (c) decodes unreserved (`%41%7a`→`Az`,
   `%2E`→`.`); (d) uppercases hex (`%3a`→`%3A`, `RURL-dkaycxvp`); (e) drops/breaks
   on null and invalid-UTF-8 bytes (`%00%51`→dropped, `foo\t%91`→literal `NA`).

## Files

| File | Contents |
|---|---|
| `whatwg-success-scored.csv` | per success case: accepted + per-component `*_ok` + rurl vs expected |
| `whatwg-failure-scored.csv` | per failure case: rurl status + conformant (rejected) |
| `rfc-probes-scored.csv` | per RFC probe: pass + rurl vs expected + section |
| `run-console.txt` | verbatim console incl. the headline table |

Re-run and re-freeze whenever a conformance issue closes; the headline numbers
are the regression metric.
