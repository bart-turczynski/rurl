# Cross-parser URL-parsing disagreement — frozen run

This directory is the **committed, citable output** of the paper's headline
experiment (fp `RURL-odekavlr`, parent HERO paper `RURL-osrjtiwk`). The
*regenerable* source of truth is the harness at
[`inst/bench/parser-disagreement.R`](../../inst/bench/parser-disagreement.R)
(spec: `RURL-wncwfasl`); this snapshot is the specific run the paper cites.

Regenerate into a scratch dir with:

```r
devtools::load_all(); source("inst/bench/parser-disagreement.R")
```

or freeze a new committed run with `RURL_BENCH_OUT=analysis/disagreement Rscript …`.

## Provenance / version manifest

| | |
|---|---|
| Date | 2026-07-07 |
| rurl | **2.3.0** (dev tree via `load_all`; hardened per `RURL-moselrwp`) |
| R | 4.6.0 (aarch64-apple-darwin23) |
| libcurl | 8.14.1 (via `curl` 7.1.0) |
| adaR | 0.3.5 (WHATWG reference) |
| urltools | 1.7.3.1 |
| pslr | 1.0.2.9000 |
| punycoder | 1.2.0 |
| Corpus | 328 inputs across 28 axes |
| Participants | `rurl(rfc3986)`, `rurl(whatwg)`, `curl`, `adaR`, `urltools` |

Oracle: divergence is measured against the committed **dual RFC/WHATWG oracle**
(`tests/testthat/fixtures/url-standard-conformance.csv` +
`external-url-vectors.csv`, columns `rfc3986_expected` / `whatwg_expected` /
`oracle_ref` / `divergence_class`), **not** against rurl's own output.

Full console log (per-axis summary + the divergent matrix as printed):
[`run-console.txt`](run-console.txt).

## Files

| File | One row per | Contents |
|---|---|---|
| `disagreement-matrix.csv` | input | per-parser compact verdict `scheme\|host\|port\|path` (or `<error>`/`<reject>`), plus which components diverge — **the headline table** |
| `disagreement-long.csv` | (input × parser) | full normalized components + status |
| `per-axis-summary.csv` | axis | divergence count / ratio, and which components diverge |
| `run-console.txt` | — | verbatim console output incl. version manifest |

## Comparison model (kept honest)

Divergence is scored on four comparable components — **scheme, host, port,
path**. query/fragment are captured but **not scored** (query handling is
outside the `url_standard` selector's remit). Normalizers (see the harness
header for the *why*): scheme lower-cased + trailing `:` stripped; host
verbatim (case and trailing dot **are** axes); port preserved (a retained `80`
vs an elided default **is** the axis); path gets a leading `/` when missing but
dot-segment resolution is **not** normalized (it is itself an axis). A parser
that rejects contributes the sentinel `<error>` / `<reject>`, so
accept-vs-reject — the most consequential divergence — is captured naturally.

## Headline result

**303 of 328 inputs (92%) show at least one cross-parser divergence.** That
coarse figure is real but inflated by design: five parsers with distinct
policies rarely *all* agree, and two of the five columns are rurl's own
`rfc3986` and `whatwg` profiles, which are *built* to differ — that difference
is the paper's point, not noise.

The informative decomposition is pairwise. Full-tuple
(`scheme|host|port|path`) agreement rates:

|                | rurl(rfc) | rurl(wg) | curl | adaR | urltools |
|----------------|:---------:|:--------:|:----:|:----:|:--------:|
| rurl(rfc3986)  | 1.000 | 0.713 | 0.863 | 0.125 | 0.351 |
| rurl(whatwg)   | 0.713 | 1.000 | 0.771 | 0.256 | 0.113 |
| curl           | 0.863 | 0.771 | 1.000 | 0.204 | 0.241 |
| adaR           | 0.125 | 0.256 | 0.204 | 1.000 | 0.104 |
| urltools       | 0.351 | 0.113 | 0.241 | 0.104 | 1.000 |

The low **adaR** column is *not* host-shape disagreement — it is two single,
documented axes that fire on nearly every non-ASCII row: adaR percent-encodes
every non-ASCII **path** (`/école` → `/%C3%A9cole`; the readable-path default,
caveat 3 / `RURL-ndrgrwcz`) and renders punycode hosts back to **Unicode**
(`xn--mnchen-3ya.de` → `münchen.de`; a `host_encoding` presentation choice).
Once rejections and those two axes are set aside, rurl's WHATWG profile tracks
the WHATWG reference tightly:

* **rurl(whatwg) vs adaR** — accept/reject **concordance 0.970**; on the rows
  where both accept, per-component agreement is scheme 0.99, host 0.958, port
  0.958, path 0.938 (residual: 4 host, 4 port, 6 path mismatches — the path
  ones are the readable-path caveat).
* **rurl(rfc3986) vs curl** — scheme 1.00, port 1.00, path 0.956; host **0.757**
  — the gap is precisely the IPv4-obfuscation axis (curl runs the IPv4 host
  parser; the RFC profile keeps the literal reg-name).

Accept/reject posture over the corpus: `urltools` **never rejects** (328/328
accepted — its permissiveness is itself a finding); `rurl(whatwg)` is the
strictest (232 rejected), `adaR` next (222), `rurl(rfc3986)` 191, `curl` 187.

## Notable divergences — who follows which standard, and why the selector resolves it

Each cell below is `scheme|host|port|path` from the frozen matrix.

| Input | rurl(rfc3986) | rurl(whatwg) | curl | adaR | urltools | Reading |
|---|---|---|---|---|---|---|
| `http://ex.com/%41%42` | `…/AB` | `…/%41%42` | `…/AB` | `…/%41%42` | `…/%41%42` | RFC 3986 §2.3 normalizes unreserved `%41%42`→`AB`; WHATWG preserves. rurl's two profiles **bracket** the two standards; curl sides with RFC, adaR/urltools with WHATWG. |
| `http://ex.com/a%2Fb` | `…/a%2Fb` | `…/a%2Fb` | `…/a/b` | `…/a%2Fb` | `…/a%2Fb` | Reserved `%2F` must stay data (RFC §2.2). **curl alone** decodes it — a real correctness divergence; both rurl profiles are correct. |
| `http://2130706433/` | `2130706433` *(warning-no-tld)* | `127.0.0.1` | `127.0.0.1` | `127.0.0.1` | `2130706433` | Whole-decimal IPv4. WHATWG's host parser MUST read it as `127.0.0.1`; RFC 3986 has no IPv4 special-casing, so it is a (suspicious) reg-name. rurl's selector makes the choice explicit and **flags** the RFC reading. |
| `http://0x7f.0.0.1/` | `0x7f.0.0.1` *(warning-invalid-tld)* | `127.0.0.1` | `127.0.0.1` | `127.0.0.1` | `0x7f.0.0.1` | Hex-octet IPv4 — same split. The RFC profile surfaces a diagnostic rather than silently canonicalizing. |
| `http://999999999999/` | `999999999999` *(warning-no-tld)* | `<error>` | `999999999999` | `<reject>` | `999999999999` | Out-of-range integer host. WHATWG MUST **reject**; rurl(whatwg) and adaR do. curl, urltools and the RFC profile accept it as a reg-name — the SSRF-relevant accept-vs-reject split. |
| `http://ex.com:80/` | `…\|80\|` | `…\|80\|` | `…\|80\|` | `…\|\|` (elided) | `…\|80\|` | Default-port elision is a WHATWG **serializer** choice. **adaR alone** drops `:80`; rurl keeps it verbatim (elision is a separate normalization phase, not parsing). |
| `http://xn--mnchen-3ya.de/` | `xn--mnchen-3ya.de` | `xn--mnchen-3ya.de` | `xn--mnchen-3ya.de` | `münchen.de` | `xn--mnchen-3ya.de` | IDNA rendering. **adaR alone** presents Unicode; rurl keeps the ASCII/A-label and exposes Unicode via `host_encoding` (facts-not-policy, ADR 0002). |
| `https://lemire.me/école` | `/école` | `/école` | `/école` | `/%C3%A9cole` | `/école` | Readable vs percent-encoded **path**. **adaR alone** percent-encodes; rurl's readable *default* is user-pickable — `path_encoding = "encode"` opts into the browser form (`RURL-ndrgrwcz`, caveat 3). |
| `http://ex.com/a/./b/../c` | `/a/c` | `/a/c` | `/a/c` | `/a/c` | `/a/./b/../c` | Dot-segment resolution (RFC §5.2.4 / WHATWG). **urltools alone** leaves segments unresolved — a correctness divergence. |
| `http:\\example.com\a` | `<error>` | `example.com/a` | `<error>` | `example.com/a` | mangled | WHATWG special-scheme treats `\`→`/`; RFC rejects. rurl(whatwg) **matches the WHATWG reference (adaR)**; the RFC profile and curl reject. |
| `http://ex⇥ample.com/` (tab) | `<error>` | `example.com` | `<error>` | `example.com` | keeps tab | WHATWG strips ASCII tab/CR/LF before parsing. rurl(whatwg) **matches adaR**; the RFC profile rejects; urltools silently keeps the control char. |

The recurring pattern: for every input where a reasonable parser disagrees with
another, rurl **does not guess** — the `url_standard` selector routes the input
to the RFC or WHATWG reading deterministically, and the RFC profile emits a
diagnostic (`warning-no-tld`, `warning-invalid-tld`) rather than silently
canonicalizing an obfuscated host. That is the paper's thesis in one table.

## Carried caveats (provisional freeze — `RURL-moselrwp` Part 3b)

This is the **first** benchmark run and explicitly **not** the deciding one; the
tool stays in fix→test→repeat. Report these as boundaries — do not let the
table settle them:

1. **Biggest caveat:** the WHATWG profile is WHATWG on *governed axes only*
   (path percent/dot, host IPv4/reg-name, case, port elision, backslash,
   tab/CR/LF, forbidden host code points) — **not** a full UTS-46 host mapping.
   Ligatures / circled digits / zero-width code points pass through (ADR 0002).
2. **ada-008 / host allowed-set:** libcurl's host allowed-set is narrower than
   WHATWG's (e.g. curl rejects an apostrophe in a host) → `RURL-dxwxeamq`.
3. **Readable-path default (user-pickable, no longer a limitation):** by
   *default* rurl keeps non-ASCII paths readable (`/école`) where WHATWG / adaR
   percent-encode (`/%C3%A9cole`) — this default dominates the adaR
   path-agreement gap in the table above. It is a presentation choice, not a
   limitation: `path_encoding = "encode"` renders the browser/percent-encoded
   form and `"decode"` the readable form (the analog of `host_encoding`),
   shipped by `RURL-ndrgrwcz`. The one residual gap — combining an explicit
   `path_encoding` *with* a `url_standard` profile, which currently conflicts —
   is queued as `RURL-sjnqhwtl` (needs a PRD/ADR delta first).
4. **Parked, needs-investigation:** `eq-U8` (U+0130 fold), `yal-009` (scheme
   inference), `ipobf-019/020` (IPv6 re-serialization).

If any boundary is later closed, re-run (`_scratch/build-oracle-columns.R`
regenerates the oracle columns, then re-freeze this directory).
