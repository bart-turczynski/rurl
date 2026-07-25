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
| **RFC 3986** | `inst/bench/rfc3986-probes.csv` — 37 probes (19 accept + 18 reject) | hand-authored against the RFC's grammar/§6.2.2 rules (no official RFC test suite exists), each row tagged with its section. Reject rows added in `RURL-wlqhmbdw`, drawn from the audited conformance fixture and verified against both referees. |

Rows whose input contains a control byte carry a JSON spelling in `input_json`,
which is authoritative when present; the `input` cell is then a lossy rendering
kept only so the row stays legible in a diff.

Success cases are limited to the schemes rurl supports (`http/https/ftp/file`) —
the *"additional protocols notwithstanding"* carve-out; non-special schemes
(`mailto:`, `data:`, `ws:`, …) are out of scope by design.

WHATWG success is scored in rurl's **canonical-output config**
(`scheme_policy="require", host_encoding="idna", path_encoding="encode"`) — the
settings that ask rurl for the WHATWG serialization. A residual mismatch there
is genuine: **the spec output cannot be reached by any knob.**

## Headline (rurl 2.8.0, 2026-07-26)

| Metric | Result |
|---|---|
| WHATWG — success **accepted** | 176/176 (100%) |
| WHATWG — success **full-component parity** | **176/176 (100%)** |
| WHATWG — failure **correctly rejected** | 202/202 (100%) |
| WHATWG — overall acceptance conformance | **378/378 (100%)** |
| RFC 3986 — **accept**-conformance | 19/19 (100%) |
| RFC 3986 — **reject**-conformance | 13/13 (100%) |
| RFC 3986 — **two-sided** conformance | **32/32 (100%)** |
| RFC 3986 — documented departures pinned | 5/5 — *excluded from the figures above* |

rurl **never accepts a URL WHATWG rejects** among these 202 failure cases, and
the three over-strict rejections noted in the 2026-07-08 run (rurl 2.5.0:
173/176) are gone. Against this oracle the WHATWG profile is now fully
conformant.

### Reading the RFC rows

The probe set is now **two-sided** (`RURL-wlqhmbdw`): it contains rejection
cases, so it can finally detect over-permissiveness rather than only failing to
notice it. Two properties keep the number honest:

1. **Reject probes use `http`/`https`/`ftp`/`file` only.** A rejection of
   `sc://…` would be produced by the ADR 0004 closed scheme set, not by the
   grammar — scoring that as *grammar* conformance would credit rurl for the
   wrong reason.
2. **Deliberate strictness is pinned but never counted as conformance.** Five
   probes record inputs the RFC grammar **admits** and rurl declines by policy.
   They carry a `rurl_deviation` citing the owning ADR and are reported on their
   own line. Folding them into the conformance score would mean rurl could raise
   its own "RFC conformance" by rejecting *more* of what the RFC allows — a
   metric that rewards the opposite of conformance.

> **Still do not read 32/32 as "rurl is 100% RFC 3986 conformant."** 37
> hand-authored probes cannot cover the grammar. The honest whole-corpus figure
> comes from the audited conformance fixture (`RURL-nknytzxz`): of **257 rows
> carrying an RFC 3986 oracle, rurl matches the standard on 164 and departs on
> 93** — 81 where it rejects what the RFC admits (deliberate policy: the ADR
> 0004 host-shape gate and the closed scheme set) and 12 where it accepts what
> the RFC does not (ADR 0002 Unicode hosts, ADR 0011 readable paths). Each of
> the 93 cites the ADR or ticket that owns it.
>
> That was **158 / 99** before `RURL-qrfrvmkg` bound the generic-URI gate
> uniformly across schemes; closing the `RURL-pfewxbhb` coverage gap moved
> exactly 6 rows from over-permissive to conformant-reject (18 → 12).

## Where rurl falls short of the standard

### RFC 3986 — covered probes fully pass, both directions
rurl(rfc3986) accepts reg-names containing the RFC 3986 §3.2.2 **sub-delims**
(`! $ & ' ( ) * + , ; =`) and passes the normalization probes (case folding,
unreserved decode, `%2f`→`%2F` reserved-preserve, dot-segment resolution).

It also **rejects** what the grammar rejects, now covered by probe: `|` and raw
SP and C0 controls in a reg-name (§3.2.2), a reg-name inside `[...]` or a
non-IPv6 IP-literal (§3.2.2), truncated/non-hex percent triplets (§2.1),
non-numeric and negative ports (§3.2.3), `"` in a path (§3.3), and backslashes
in an authority (§3.2.1) — the last being the hostname-confusion shape from
*"yoU aRe a Liar"* (SecWeb 2022) and *"Equivocal URLs"* (ESORICS 2022), where
RFC 3986 has no backslash-correction rule and WHATWG does.

The five pinned departures are all ADR 0004: percent-encoded and empty and
dotless hosts, an empty authority, and a scheme outside the closed set.

### WHATWG — no remaining buckets against this oracle
Component non-conformances among accepted cases: **0**; over-strict rejections:
**0** (was 3). Query and fragment are scored in the full-component metric and
have no accepted-case mismatches. The buckets described below are retained as
the record of what was closed, not as open items.

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
| `rfc-probes-scored.csv` | per RFC probe: pass + `is_departure` + rurl vs expected + section + `rurl_deviation` |
| `run-console.txt` | verbatim console incl. the headline table |

Re-run and re-freeze whenever a conformance issue closes; the headline numbers
are the regression metric.
