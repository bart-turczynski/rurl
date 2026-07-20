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
| Date | 2026-07-20 (refreshed; previous run 2026-07-08 at rurl 2.5.0) |
| rurl | **2.7.0** (dev tree via `load_all`; hardened per `RURL-moselrwp`) |
| R | 4.6.0 (aarch64-apple-darwin23) |
| libcurl | 8.14.1 (via `curl` 7.1.0) |
| adaR | 0.3.5 (WHATWG reference) |
| urltools | 1.7.3.1 |
| pslr | 1.1.1 |
| punycoder | 1.2.1 |
| Corpus | 336 inputs across 28 axes |
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

**306 of 336 inputs (91%) show at least one cross-parser divergence.** That
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
| rurl(rfc3986)  | 1.000 | 0.685 | 0.845 | 0.655 | 0.384 |
| rurl(whatwg)   | 0.685 | 1.000 | 0.753 | **0.970** | 0.128 |
| curl           | 0.845 | 0.753 | 1.000 | 0.726 | 0.247 |
| adaR           | 0.655 | 0.970 | 0.726 | 1.000 | 0.125 |
| urltools       | 0.384 | 0.128 | 0.247 | 0.125 | 1.000 |

> **Methodology note (2026-07-20).** This table is now computed with the
> definition stated above, which makes it *consistent with the focused pairwise
> section below*: 10 divergent rows out of 336 is exactly 0.970, and 52 out of
> 336 is exactly 0.845. The 2026-07-08 table was produced ad hoc under a
> different, unstated rule and did **not** reconcile with its own
> `diverge-*.csv` counts (it reported rurl(whatwg) vs adaR as 0.290 while the
> companion file listed only 15 divergent rows, i.e. 0.955). Prefer these
> numbers; do not compare them cell-by-cell against the older run.

Where rurl's WHATWG profile still differs from the WHATWG reference, it is one
documented presentation axis plus the closed scheme set: adaR renders punycode
hosts back to **Unicode** (`xn--mnchen-3ya.de` → `münchen.de`; a `host_encoding`
choice, ADR 0002). The default-port axis called out in the previous run is gone
— rurl now elides it too (`RURL-uvilvhnm`). Once rejections and that one axis
are set aside, the profiles track each other tightly:

* **rurl(whatwg) vs adaR** — accept/reject **concordance 0.973**; on the rows
  where both accept, per-component agreement is scheme **1.00**, port **1.00**,
  host **0.990**, path **0.971**. Port is now exact: default-port elision
  (`RURL-uvilvhnm`) landed, and so did percent-hex case preservation
  (`RURL-dkaycxvp`), so the two nits called out in the 2026-07-08 run are gone.
* **rurl(rfc3986) vs curl** — scheme **1.00**, port **1.00**, path 0.964; host
  **0.763** — the gap is precisely the IPv4-obfuscation axis (curl runs the
  IPv4 host parser; the RFC profile keeps the literal reg-name and flags it).

Accept/reject posture over the corpus: `urltools` **never rejects** (336/336
accepted — its permissiveness is itself a finding); `rurl(whatwg)` is the
strictest (231 rejected), `adaR` next (222), `curl` 193, `rurl(rfc3986)` **185**
(was 196 — see below; rurl(rfc) got *more permissive* by getting more correct).

## Focused pairwise divergences (exact spots)

The three `diverge-*.csv` files list every divergent input for a pairing, with
the differing component and both values. Highlights:

**`rurl(whatwg)` vs `adaR`** (Ada / C++ WHATWG reference) — **10 rows** (was
15): 9 accept/reject (all the closed scheme set + `yal-009`), 1 host
(punycode→Unicode, ADR 0002). The 3 port rows and 2 path rows are **gone** —
`RURL-uvilvhnm` and `RURL-dkaycxvp` both landed. What remains against the
WHATWG reference is one deliberate presentation choice and the closed scheme
set; no host-shape or structural disagreement at all.

**`rurl(rfc3986)` vs `curl`** (libcurl / C) — **52 rows** (was 41): **33 host** —
every one an IPv4-obfuscation form (`http://2130706433/`, `http://0x7f.0.0.1/`,
`http://0177.0.0.1/`, …) where curl canonicalizes to dotted-quad and the RFC
profile keeps the literal reg-name; **16 accept/reject** (was 5); 3 path —
including curl's `%2F`/`%3F`/`%23`-decode (`/a%2Fb`→`/a/b`), a real curl
correctness divergence.

**Read the +11 correctly: this pairing got worse because rurl got better.**
Every added row is one where rurl(rfc3986) now *accepts* something RFC 3986
admits and libcurl still rejects — the sub-delim reg-names of `RURL-dnddogce`
(`http://a;b.example.com/`, `a!b`, `a'b`, `a+b`, `a=b`; §3.2.2 lists all of
them under `sub-delims`) and the percent-encoded `file:` hosts unlocked by the
two-gate model (`RURL-obsweger`; §3.2.2 does not decode a reg-name for
validity). Per the standing parity directive, libcurl is a *heuristic toward*
RFC 3986, not the standard itself, so divergence from it is not evidence of a
rurl defect. The independent check that this is movement toward the RFC and not
away from it is `tools/oracle-audit-rfc3986.R`, which refereed these same
constructs against the RFC ABNF and Ruby's `URI::RFC3986_Parser` and found both
accept them (`RURL-nknytzxz`).

**`rurl(rfc3986)` vs CPython `urllib.parse`** (non-R, separate ecosystem) —
**186 rows** (was 192): **160 accept/reject** (was 171). The drop is the same
movement as above seen from the other side: `urllib.parse` never rejects, so
rurl accepting more genuinely-valid RFC input closes the gap rather than
widening it. `urllib.parse.urlsplit` is a permissive RFC
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
5. **Resolved decision (`RURL-ajnnjzgs`):** `eq-U8` stays an intentional rurl
   deviation (recorded in the fixture's `rurl_deviation` column since
   RURL-nknytzxz, which retired the old `parser-boundary` class): `@` delimits
   userinfo and rurl drops userinfo rather than aligning to the paper's
   WHATWG-ref U+0130 host fold. `ipobf-019/020` stay an
   intentional spec-divergent profile split: RFC keeps the IPv6 literal, WHATWG
   re-serializes the embedded IPv4 pieces.

If any boundary is later closed, re-run (`_scratch/build-oracle-columns.R`
regenerates the oracle columns, then re-freeze this directory).
