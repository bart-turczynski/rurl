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
| Date | 2026-07-08 |
| rurl | **2.5.0** (dev tree via `load_all`; hardened per `RURL-moselrwp`) |
| R | 4.6.0 (aarch64-apple-darwin23) |
| libcurl | 8.14.1 (via `curl` 7.1.0) |
| adaR | 0.3.5 (WHATWG reference) |
| urltools | 1.7.3.1 |
| pslr | 1.0.2 |
| punycoder | 1.2.0 |
| Corpus | 334 inputs across 28 axes |
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
| `diverge-whatwg-vs-adaR.csv` | divergent input | exact per-input `rurl(whatwg)` vs `adaR` spots |
| `diverge-rfc-vs-curl.csv` | divergent input | exact per-input `rurl(rfc3986)` vs `curl` spots |
| `diverge-rfc-vs-python.csv` | divergent input | exact per-input `rurl(rfc3986)` vs CPython `urllib.parse` spots |
| `pairwise-divergences.R` / `cross-language-rfc.py` | — | regenerate the three `diverge-*.csv` tables (the Python file is an external, non-R RFC baseline) |

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

Two rules keep the study from counting non-disagreements (added 2026-07-08):

* **Held axis — `scheme_policy = "require"`.** Both rurl profiles are run with
  `scheme_policy = "require"` (ADR 0010). curl and adaR (given no base URL)
  also require an explicit scheme, so all five participants are compared at the
  same acceptance posture and rurl's `http://` **inference** — a *separate*
  axis — does not inflate accept-vs-reject divergence. This also removes the
  lone WHATWG false-accept (backtick host, `ada-005`), which is opt-out-able
  exactly here.
* **Path presentation is not a disagreement.** For **scoring only** (the
  displayed matrix cell keeps the raw path), raw non-ASCII bytes and the WHATWG
  forbidden path code points are percent-encoded uniformly across parsers, so
  `/école` ≡ `/%C3%A9cole` and `/"quoted"` ≡ `/%22quoted%22`. That is a
  presentation axis (`path_encoding`, ADR 0011), not a parse disagreement.
  Already-encoded octets (`%41%42` vs `AB`) and reserved delimiters (`%2F` vs
  `/`) are left **untouched**, so the genuine RFC-vs-WHATWG unreserved
  divergence and curl's `%2F`-decode bug survive.

## Headline result

**304 of 334 inputs (91%) show at least one cross-parser divergence.** That
coarse figure is real but inflated by design: five parsers with distinct
policies rarely *all* agree, and two of the five columns are rurl's own
`rfc3986` and `whatwg` profiles, which are *built* to differ — that difference
is the paper's point, not noise.

The informative decomposition is pairwise. Full-tuple
(`scheme|host|port|path`, path canonicalized) agreement rates:

|                | rurl(rfc) | rurl(wg) | curl | adaR | urltools |
|----------------|:---------:|:--------:|:----:|:----:|:--------:|
| rurl(rfc3986)  | 1.000 | 0.695 | 0.877 | 0.138 | 0.347 |
| rurl(whatwg)   | 0.695 | 1.000 | 0.760 | 0.290 | 0.126 |
| curl           | 0.877 | 0.760 | 1.000 | 0.216 | 0.243 |
| adaR           | 0.138 | 0.290 | 0.216 | 1.000 | 0.126 |
| urltools       | 0.347 | 0.126 | 0.243 | 0.126 | 1.000 |

The low **adaR** column is *not* host-shape disagreement — it is two single,
documented axes that fire on nearly every non-ASCII row: adaR renders punycode
hosts back to **Unicode** (`xn--mnchen-3ya.de` → `münchen.de`; a `host_encoding`
presentation choice, ADR 0002) and **elides** the default port
(`http://ex.com:80/` → `:80` dropped; a serializer choice). Once rejections and
those two axes are set aside, rurl's WHATWG profile tracks the WHATWG reference
tightly:

* **rurl(whatwg) vs adaR** — accept/reject **concordance 0.973**; on the rows
  where both accept, per-component agreement is scheme **1.00**, host **0.990**,
  port 0.971, path **0.981**. The residual path gap is now just **two** rows —
  the `%7e`→`%7E` percent-hex-case nit (`RURL-dkaycxvp`); école/quoted are no
  longer counted (presentation).
* **rurl(rfc3986) vs curl** — scheme **1.00**, port **1.00**, path 0.978; host
  **0.759** — the gap is precisely the IPv4-obfuscation axis (curl runs the
  IPv4 host parser; the RFC profile keeps the literal reg-name and flags it).

Accept/reject posture over the corpus: `urltools` **never rejects** (334/334
accepted — its permissiveness is itself a finding); `rurl(whatwg)` is the
strictest (231 rejected), `adaR` next (222), `curl` 193, `rurl(rfc3986)` 196.

## Focused pairwise divergences (exact spots)

The three `diverge-*.csv` files list every divergent input for a pairing, with
the differing component and both values. Highlights:

**`rurl(whatwg)` vs `adaR`** (Ada / C++ WHATWG reference) — 15 rows:
9 accept/reject (all the closed scheme set + `yal-009`), 3 port (default-port
elision), 2 path (`%7e`→`%7E` hex-case, `RURL-dkaycxvp`), 1 host
(punycode→Unicode). No host-shape or structural disagreement remains.

**`rurl(rfc3986)` vs `curl`** (libcurl / C) — 41 rows: **33 host** — every one
an IPv4-obfuscation form (`http://2130706433/`, `http://0x7f.0.0.1/`,
`http://0177.0.0.1/`, …) where curl canonicalizes to dotted-quad and the RFC
profile keeps the literal reg-name; 5 accept/reject; 3 path — including curl's
`%2F`/`%3F`/`%23`-decode (`/a%2Fb`→`/a/b`), a real curl correctness divergence.

**`rurl(rfc3986)` vs CPython `urllib.parse`** (non-R, separate ecosystem) — 192
rows: **171 accept/reject**. `urllib.parse.urlsplit` is a permissive RFC
*splitter* that **never rejects** — it accepts control characters in the host
(`http://a\x01b/`), forbidden host code points (`http://a'b.example.com/`),
empty authorities (`http://user:pass@/`), and mangled userinfo — where rurl's
strict host-shape gate (ADR 0004) rejects. The remaining rows are rurl's RFC
normalization that a raw splitter does not do: dot-segment resolution
(`/a/./b/../c`→`/a/c`, RFC §5.2.4), unreserved percent-decoding
(`%41%42`→`AB`, `%7e`→`~`, RFC §2.3), and host IDNA/percent handling
(`Yağız.com`→`yağız.com`). The cross-language check confirms rurl(rfc3986)'s
normalizations are real and its strictness is the deliberate, security-relevant
difference — not an artifact of comparing two R packages.

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
| `http://ex.com/a/./b/../c` | `/a/c` | `/a/c` | `/a/c` | `/a/c` | `/a/./b/../c` | Dot-segment resolution (RFC §5.2.4 / WHATWG). **urltools alone** leaves segments unresolved — a correctness divergence. |
| `http:\\example.com\a` | `<error>` | `example.com/a` | `<error>` | `example.com/a` | mangled | WHATWG special-scheme treats `\`→`/`; RFC rejects. rurl(whatwg) **matches the WHATWG reference (adaR)**; the RFC profile and curl reject. |
| `http://ex⇥ample.com/` (tab) | `<error>` | `example.com` | `<error>` | `example.com` | keeps tab | WHATWG strips ASCII tab/CR/LF before parsing. rurl(whatwg) **matches adaR**; the RFC profile rejects; urltools silently keeps the control char. |

The recurring pattern: for every input where a reasonable parser disagrees with
another, rurl **does not guess** — the `url_standard` selector routes the input
to the RFC or WHATWG reading deterministically, and the RFC profile emits a
diagnostic (`warning-no-tld`, `warning-invalid-tld`) rather than silently
canonicalizing an obfuscated host. That is the paper's thesis in one table.

**Presentation, not divergence.** `https://lemire.me/école` used to appear here
(adaR percent-encodes the path to `/%C3%A9cole`, rurl keeps it readable). It is
deliberately **no longer counted**: readable-vs-percent-encoded of the same
character is the `path_encoding` axis (ADR 0011) — `path_encoding = "encode"`
emits the browser form on any profile — so the harness canonicalizes it away in
scoring (see Comparison model).

## Carried caveats

The tool stays in fix→test→repeat. Report these as boundaries — do not let the
table settle them:

1. **Biggest caveat:** the WHATWG profile is WHATWG on *governed axes only*
   (path percent/dot, host IPv4/reg-name, case, port elision, backslash,
   tab/CR/LF, forbidden host code points) — **not** a full UTS-46 host mapping.
   Ligatures / circled digits / zero-width code points pass through (ADR 0002).
   Tracked: **`RURL-tvbvdjde`**.
2. **ada-008 / host allowed-set:** libcurl's host allowed-set is narrower than
   WHATWG's (e.g. curl rejects an apostrophe in a host) → **`RURL-dxwxeamq`**.
3. **`%7e`→`%7E` percent-hex case:** the WHATWG profile uppercases already-
   encoded hex where strict WHATWG preserves input case (cosmetic) →
   **`RURL-dkaycxvp`**. The only residual `rurl(whatwg)` vs adaR path gap.
4. **Closed by design / now controllable (no longer limitations):**
   *readable-path* rendering is the `path_encoding` axis (ADR 0011) and is no
   longer scored; the *backtick-host* false-accept, default *scheme inference*,
   and `yal-009` dotted-authority scheme-confusion row are controllable via
   `scheme_policy = "require"` (ADR 0010), which this run holds fixed.
5. **Resolved decision (`RURL-ajnnjzgs`):** `eq-U8` stays an intentional
   parser-boundary: `@` delimits userinfo and rurl drops userinfo rather than
   aligning to the paper's WHATWG-ref U+0130 host fold. `ipobf-019/020` stay an
   intentional spec-divergent profile split: RFC keeps the IPv6 literal, WHATWG
   re-serializes the embedded IPv4 pieces.

If any boundary is later closed, re-run (`_scratch/build-oracle-columns.R`
regenerates the oracle columns, then re-freeze this directory).
