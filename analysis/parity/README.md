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
| WHATWG — success **accepted** | 173/176 (98%) |
| WHATWG — success **full-component parity** | **173/176 (98%)** |
| WHATWG — failure **correctly rejected** | 202/202 (100%) |
| WHATWG — overall acceptance conformance | 375/378 (99%) |
| RFC 3986 — probes passed | 19/19 (100%) |

rurl **never accepts a URL WHATWG rejects** among these 202 failure cases
(100%). The remaining WHATWG shortfall is **over-strictness**: three WPT-valid
success rows are still rejected before component comparison. The RFC probe set
is now fully conformant for the covered grammar and normalization cases.

## Where rurl falls short of the standard

### RFC 3986 — covered probes fully pass
rurl(rfc3986) now accepts reg-names containing the RFC 3986 §3.2.2
**sub-delims** (`! $ & ' ( ) * + , ; =`) and passes the normalization probes
(case folding, unreserved decode, `%2f`→`%2F` reserved-preserve, dot-segment
resolution).

### WHATWG — remaining buckets
Component non-conformances among accepted cases: **0**; plus **3 over-strict
rejections**. Query and fragment are now scored in the full-component metric and
have no accepted-case mismatches.

1. **Over-strict rejected path/control rows.** Three WPT-valid success rows
   still reject before component comparison; accepted success rows now have no
   scheme, host, port, path, query, or fragment mismatches.
2. **Default-port elision closed.** Under `url_standard = "whatwg"`, default
   ports now serialize as absent in the parse result (`http://foo:80/` returns
   `port = NA`, matching WHATWG's empty `.port`).
3. **Path percent-encoding closed.** Under
   `url_standard="whatwg", path_encoding="encode"`, rurl now uses the WHATWG
   path encode set, preserves existing `%` spellings and hex case, avoids
   double-encoding malformed percent runs, and percent-encodes literal Unicode
   as UTF-8 bytes.
4. **Query/fragment serialization closed.** The WHATWG profile now serializes
   accepted query and fragment components with their component-specific encode
   sets, and the harness scores WPT `search`/`hash` alongside the existing
   scheme/host/port/path columns.

## Files

| File | Contents |
|---|---|
| `whatwg-success-scored.csv` | per success case: accepted + per-component `*_ok` + rurl vs expected |
| `whatwg-failure-scored.csv` | per failure case: rurl status + conformant (rejected) |
| `rfc-probes-scored.csv` | per RFC probe: pass + rurl vs expected + section |
| `run-console.txt` | verbatim console incl. the headline table |

Re-run and re-freeze whenever a conformance issue closes; the headline numbers
are the regression metric.
