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
| Date | 2026-07-26 (refreshed; previous runs 2026-07-20 at rurl 2.7.0, 2026-07-08 at 2.5.0) |
| rurl | **2.8.0** (dev tree via `load_all`) |
| R | 4.6.0 (aarch64-apple-darwin23) |
| libcurl | 8.14.1 (via `curl` 7.1.0) |
| adaR | 0.3.5 (WHATWG reference) |
| urltools | 1.7.3.1 |
| pslr | 1.1.1 |
| punycoder | 1.2.1.9000 |
| Corpus | 336 inputs across 28 axes |
| Participants | `rurl(rfc3986)`, `rurl(whatwg)`, `curl`, `adaR`, `urltools` |
| rurl posture | `scheme_policy = "require"`, `scheme_acceptance = "general"` (both held — see below) |

> **What changed in the 2026-07-26 run (`RURL-goeprkuf`, T6).** Both rurl
> profiles now run at `scheme_acceptance = "general"`. Previous runs used the
> default `"web"` allowlist, which scored ~19 opaque/non-special rows as rurl
> rejections against parsers that genuinely parse them — measuring rurl's
> *scheme-acceptance policy* rather than the `url_standard` interpretation this
> study is about. Do not compare this run's counts cell-by-cell against the
> 2026-07-20 run without reading the posture note below.

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
* **Held axis — `scheme_acceptance = "general"`** (added 2026-07-26,
  `RURL-goeprkuf`). Both rurl profiles are run at `general` (ADR 0012 D3).
  adaR and `urllib.parse` are **general** parsers — they parse `mailto:`,
  `data:`, `tel:` and other non-special schemes — while rurl's default `"web"`
  acceptance admits only the curated `http`/`https`/`ftp`/`ftps`/`file`
  allowlist (ADR 0004). Scoring `web` against them turns a *policy* choice into
  ~19 accept/reject "divergences" that say nothing about `url_standard`
  interpretation. Holding `general` isolates the axis this study measures.
  **Caveat — this cuts the other way for `curl`:** libcurl is a web-scheme
  parser and does not implement generic opaque-scheme parsing, so *its* pairing
  gains 15 accept/reject rows that are purely scheme-acceptance (`mailto:a@b.com`,
  `sc://:12/`, `urn://:443`, …). They are enumerated in
  `diverge-rfc-vs-curl.csv` and must not be read as RFC-interpretation
  divergence. The `web` posture is **not** unmeasured: it is exactly what the
  conformance fixture and `inst/bench/standard-parity.R` score, so the two
  harnesses cover the two postures between them.
* **Path presentation is not a disagreement.** For **scoring only** (the
  displayed matrix cell keeps the raw path), raw non-ASCII bytes and the WHATWG
  forbidden path code points are percent-encoded uniformly across parsers, so
  `/école` ≡ `/%C3%A9cole` and `/"quoted"` ≡ `/%22quoted%22`. That is a
  presentation axis (`path_encoding`, ADR 0011), not a parse disagreement.
  Already-encoded octets (`%41%42` vs `AB`) and reserved delimiters (`%2F` vs
  `/`) are left **untouched**, so the genuine RFC-vs-WHATWG unreserved
  divergence and curl's `%2F`-decode bug survive.

## Headline result

**307 of 336 inputs (91%) show at least one cross-parser divergence.** That
coarse figure is real but inflated by design: five parsers with distinct
policies rarely *all* agree, and two of the five columns are rurl's own
`rfc3986` and `whatwg` profiles, which are *built* to differ — that difference
is the paper's point, not noise.

The informative decomposition is pairwise. Full-tuple agreement rate — the
share of the 336 inputs on which two parsers produce the *identical*
`scheme|host|port|path` tuple, with `canon_path()` applied and a shared
`<reject>` sentinel so agreeing to reject counts as agreement:

|                | rurl(rfc) | rurl(wg) | curl | adaR | urltools |
|----------------|:---------:|:--------:|:----:|:----:|:--------:|
| rurl(rfc3986)  | 1.000 | 0.649 | 0.783 | 0.643 | 0.396 |
| rurl(whatwg)   | 0.649 | 1.000 | 0.732 | **0.994** | 0.128 |
| curl           | 0.783 | 0.732 | 1.000 | 0.726 | 0.247 |
| adaR           | 0.643 | 0.994 | 0.726 | 1.000 | 0.125 |
| urltools       | 0.396 | 0.128 | 0.247 | 0.125 | 1.000 |

> **Methodology note.** This table is computed with the definition stated above,
> which makes it *consistent with the focused pairwise section below*: 2
> divergent rows out of 336 is exactly 0.994, and 73 out of 336 is exactly
> 0.783. The only input is `disagreement-long.csv` plus that definition, so the
> table is re-derivable from the frozen artifacts alone. The 2026-07-08 table
> was produced ad hoc under a different, unstated rule and did **not** reconcile
> with its own `diverge-*.csv` counts. Prefer these numbers; do not compare them
> cell-by-cell against the older runs (and see the posture note above — the
> 2026-07-20 run scored rurl at `web`).

Against the WHATWG reference, rurl's WHATWG profile now differs on **two rows
out of 336**, and neither is a parsing disagreement:

* `http://xn--mnchen-3ya.de/` — adaR renders punycode hosts back to **Unicode**
  (`münchen.de`); rurl keeps the A-label and exposes Unicode through
  `host_encoding` (facts-not-policy, ADR 0002).
* `www.php.net:80/index.php?test=1` — the **held** `scheme_policy = "require"`
  axis: rurl declines to infer a scheme, adaR accepts.

Both are held axes of this study, not conformance gaps. On this corpus there is
**no accept/reject, host-shape, port or path disagreement left** between
`rurl(whatwg)` and the WHATWG reference:

* **rurl(whatwg) vs adaR** — accept/reject **concordance 0.997**; on the 113
  rows where both accept, per-component agreement is scheme **1.000**, port
  **1.000**, path **1.000**, host **0.991** (that one punycode row).
* **rurl(rfc3986) vs curl** — accept/reject concordance 0.893; on the 134 rows
  where both accept, scheme **1.000**, port **1.000**, path 0.970; host
  **0.746** — the gap is precisely the IPv4-obfuscation axis (curl runs the
  IPv4 host parser; the RFC profile keeps the literal reg-name and flags it).

Accept/reject posture over the corpus: `urltools` **never rejects** (336/336
accepted — its permissiveness is itself a finding); `rurl(whatwg)` is the
strictest (223 rejected), `adaR` next (222), `curl` 193, `rurl(rfc3986)` **175**
(rurl got *more permissive* by getting more correct — see below).

## Focused pairwise divergences (exact spots)

The three `diverge-*.csv` files list every divergent input for a pairing, with
the differing component and both values. Highlights:

**`rurl(whatwg)` vs `adaR`** (Ada / C++ WHATWG reference) — **2 rows** (was 10
at `web`): 1 host (punycode→Unicode, ADR 0002) and 1 accept/reject (the held
`scheme_policy` row). Both are held axes of this study. The 9 closed-scheme-set
accept/reject rows are gone because this run holds `scheme_acceptance =
"general"`, which is the posture adaR itself parses at; the `sc://@/` class that
`general` used to over-accept is gone because `RURL-jxvibxqq` landed. Against
the WHATWG reference there is now **no host-shape, port, path or accept/reject
disagreement at all** — the two profiles agree on every structural question this
corpus asks.

**`rurl(rfc3986)` vs `curl`** (libcurl / C) — **73 rows** (was 52 at `web`):
**33 host** — every one an IPv4-obfuscation form (`http://2130706433/`,
`http://0x7f.0.0.1/`, `http://0177.0.0.1/`, …) where curl canonicalizes to
dotted-quad and the RFC profile keeps the literal reg-name; **36
accept/reject**; 3 path — including curl's `%2F`/`%3F`/`%23`-decode
(`/a%2Fb`→`/a/b`), a real curl correctness divergence; 1 host+path.

**Read the +21 correctly — 15 of it is the held posture, not a rurl change.**
libcurl is a **web-scheme** parser with no generic opaque-scheme support, so
holding rurl at `scheme_acceptance = "general"` adds 15 accept/reject rows that
are purely scheme acceptance (`mailto:a@b.com`, `scheme:example.com`,
`sc://:12/`, `urn://:443`, `foo://///////bar.com/`, …). They are the mirror of
the inflation the `general` posture *removes* from the adaR pairing, and they
say nothing about RFC interpretation. The remaining movement is the older
finding that this pairing got worse because rurl got better: rurl(rfc3986)
accepts things RFC 3986 admits and libcurl still rejects — the sub-delim
reg-names of `RURL-dnddogce` (`http://a;b.example.com/`, `a!b`, `a'b`, `a+b`,
`a=b`; §3.2.2 lists all of them under `sub-delims`) and the percent-encoded
`file:` hosts unlocked by the two-gate model (`RURL-obsweger`; §3.2.2 does not
decode a reg-name for validity). Per the standing parity directive, libcurl is a
*heuristic toward* RFC 3986, not the standard itself, so divergence from it is
not evidence of a rurl defect. The independent check that this is movement
toward the RFC and not away from it is `tools/oracle-audit-rfc3986.R`, which
refereed these same constructs against the RFC ABNF and Ruby's
`URI::RFC3986_Parser` and found both accept them (`RURL-nknytzxz`).

**`rurl(rfc3986)` vs CPython `urllib.parse`** (non-R, separate ecosystem) —
**176 rows** (was 186 at `web`): **150 accept/reject**, 14 host, 11 path, 1
host+path. This pairing moves the *opposite* way from curl's under the `general`
posture, and for the reason that makes the posture the right call:
`urllib.parse` is itself a **general** RFC splitter, so rurl parsing opaque
schemes closes the gap rather than widening it. `urllib.parse.urlsplit` is a
permissive RFC
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
| `http://ex.com:80/` | `…\|80\|` | `…\|\|` (elided) | `…\|80\|` | `…\|\|` (elided) | `…\|80\|` | Default-port elision is a WHATWG **serializer** choice. `rurl(whatwg)` **matches adaR** and elides (`RURL-uvilvhnm`); the RFC profile, curl and urltools keep `:80` verbatim. *(This row previously read "adaR alone drops `:80`" — stale since `RURL-uvilvhnm` landed; corrected against the regenerated matrix on 2026-07-26.)* |
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
   Tracked: **`RURL-mumydwfh`** — which reopened `RURL-tvbvdjde`; cite the live
   one, the original is closed. This is the **only** residual on the WHATWG side
   that is a genuine gap rather than a dial; the attributed ledger of the rest is
   in [`../parity/README.md`](../parity/README.md#residual-deviations--the-attributed-ledger).
2. **ada-008 / host allowed-set:** libcurl's host allowed-set is narrower than
   WHATWG's (e.g. curl rejects an apostrophe in a host) → **`RURL-dxwxeamq`**.
3. **`%7e`→`%7E` percent-hex case — CLOSED (`RURL-dkaycxvp`).** Verified in the
   2026-07-26 run: on `http://ex.com/%7euser` both `rurl(whatwg)` and adaR emit
   `/%7euser`, and `rurl(whatwg)` vs adaR path agreement is **1.000**. That
   input still shows `path` in `divergent_components` because `rurl(rfc3986)`
   normalizes `%7e`→`~` (RFC §2.3) — the deliberate profile split, not a WHATWG
   gap.
4. **Closed by design / now controllable (no longer limitations):**
   *readable-path* rendering is the `path_encoding` axis (ADR 0011) and is no
   longer scored; the *backtick-host* false-accept, default *scheme inference*,
   and `yal-009` dotted-authority scheme-confusion row are controllable via
   `scheme_policy = "require"` (ADR 0010), which this run holds fixed.
5. **Resolved decision (`RURL-ajnnjzgs`):** `eq-U8` stays an intentional rurl
   deviation (recorded in the fixture's `rurl_deviation` column since
   RURL-nknytzxz, which retired the old `parser-boundary` class): `@` delimits
   userinfo and rurl drops userinfo rather than aligning to the paper's
   WHATWG-ref U+0130 host fold. `ipobf-019/020` stay an
   intentional spec-divergent profile split: RFC keeps the IPv6 literal, WHATWG
   re-serializes the embedded IPv4 pieces.

If any boundary is later closed, re-run (`_scratch/build-oracle-columns.R`
regenerates the oracle columns, then re-freeze this directory).
