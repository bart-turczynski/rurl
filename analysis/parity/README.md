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
| **WHATWG** | `inst/bench/wpt-url-cases.json` — 336 success (with expected components) + 202 failure cases | derived from **web-platform-tests** `url/resources/urltestdata.json` (BSD-3-Clause, "web-platform-tests contributors") by `make-wpt-fixture.py`. The spec's own conformance suite. |
| **RFC 3986** | `inst/bench/rfc3986-probes.csv` — 37 probes (19 accept + 18 reject) | hand-authored against the RFC's grammar/§6.2.2 rules (no official RFC test suite exists), each row tagged with its section. Reject rows added in `RURL-wlqhmbdw`, drawn from the audited conformance fixture and verified against both referees. |

Rows whose input contains a control byte carry a JSON spelling in `input_json`,
which is authoritative when present; the `input` cell is then a lossy rendering
kept only so the row stays legible in a diff.

WHATWG success is scored in rurl's **canonical-output config**
(`scheme_policy="require", host_encoding="idna", path_encoding="encode"`) — the
settings that ask rurl for the WHATWG serialization. A residual mismatch there
is genuine: **the spec output cannot be reached by any knob.**

### Both postures are scored — `scheme_acceptance` is no longer implicit

Until `RURL-ghdlrcjv` the harness never passed `scheme_acceptance` at all, so it
silently ran at the exported default (`"web"`) over a success fixture that had
itself been filtered to `http`/`https`/`ftp`/`file`. Both halves of that
carve-out are gone. The fixture now spans **every scheme WPT exercises**, and
every figure below names the posture it was measured at:

* **`general`** — no scheme is excluded. This is the **grammar** figure: a
  rejection or a component mismatch here is a real conformance gap with nowhere
  to hide. It is the number to read as "how conformant is rurl".
* **`web`** — the curated `http`/`https`/`ftp`/`ftps`/`file` allowlist
  (ADR 0004), which is the exported default. Of the 336 success rows it accepts
  **176**; the other **160** are refused by the allowlist before the grammar is
  consulted. That is the allowlist **working as designed**, not a conformance
  miss — the same 160 rows are accepted at full component parity under
  `general`. Never quote `176/336` as a conformance rate.

Scoring both postures also settles a question the earlier freeze had to farm out
to the companion study. Of the 202 failure rows, **158 carry a `web` scheme, 36
carry a non-web one** (`sc` 12, then `data`, `intent`, `javascript`, `mailto`,
`stun`, `turn`, `urn` at 3 each, `non-special` 2, `asdf` 1) and **8 carry no
scheme at all**. At `web` those 36 are rejected by the closed scheme set rather
than by the WHATWG grammar — correctly rejected, but for a reason the grammar
did not have to supply. That is now answerable **in band**: at `general` the
allowlist cannot fire, and all **202/202** still reject. The grammar rejects
them on its own.

`scheme_acceptance` is a **public argument**, not an internal flag: it is in the
formals of the exported parse and accessor surface, so both postures are things
users actually run, and both are now measured here.

### Held axis — this study is absolute URLs only

The one axis this study holds is **base resolution**. `make-wpt-fixture.py`
keeps only rows with a null base, so every figure below is scored with no base
in play. Two rows (`#x` and `#x:y` against `about:blank`) are excluded on
exactly that ground. Read the figures below as *complete on absolute-URL
parsing*, and as saying **nothing** about base-relative resolution.

**Corrected (RURL-fupsemxr T2.5, 2026-08-12).** This section used to give the
reason as "rurl parses absolute URLs and does not implement WHATWG's
relative-reference resolution against a base URL … which rurl does not claim to
do". That was already wrong when written — `resolve_url()` has shipped since
rurl 2.2.0 — and the base-relative axis is now measured. Its harness is
`tests/testthat/test-wpt-base-relative.R`, the exact complement of this import
at the same pinned upstream revision (OR-024): **247 of 274 base-carrying
success rows serialize to upstream's own `href`**, with 27 enumerated
differences in three families (Windows drive letters under `file:` 18, a
recomposition seam 5, absolute references whose deviation is in absolute
parsing 4). The scope held here is this *study's*, not the package's.

**The two populations are disjoint and are never summed.** The base-null
headline below and the 247/274 base-relative figure are different corpora
scored by different harnesses; a combined "rows passed / rows total" would be
two measurements reported as one. The base-relative split is a known-differ
set, not a conformance rate (P5.3 §2.2), and it is scored on the full standard
serialization (`serialize_url()`), never on `resolve_url()`'s default clean
output.

## Headline (rurl 2.8.0, 2026-07-26)

Both postures, same canonical-output dials
(`url_standard="whatwg", scheme_policy="require", host_encoding="idna",
path_encoding="encode"`).

| Metric | `scheme_acceptance = "general"` | `scheme_acceptance = "web"` |
|---|---|---|
| WHATWG — success **accepted** | **336/336 (100%)** | 176/336 (52%) — 160 ADR 0004 policy rejections |
| WHATWG — success **full-component parity** | **336/336 (100%)** | 176/176 of those accepted (100%) |
| WHATWG — failure **correctly rejected** | **202/202 (100%)** | 202/202 (100%) |
| WHATWG — overall acceptance conformance | **538/538 (100%)** | *not a meaningful single figure at this posture* |

| RFC 3986 metric (posture-independent) | Result |
|---|---|
| **accept**-conformance | 19/19 (100%) |
| **reject**-conformance | 13/13 (100%) |
| **two-sided** conformance | **32/32 (100%)** |
| documented departures pinned | 5/5 — *excluded from the figures above* |

At `general` — every scheme in scope, nothing filtered — rurl reproduces the
WHATWG serialization on **all 336** success rows across all **eight** scored
components (scheme, username, password, host, port, path, query, fragment), and
**never accepts a URL WHATWG rejects** among the 202 failure rows. The three
over-strict rejections noted in the 2026-07-08 run (rurl 2.5.0: 173/176) are
gone, and widening the corpus from 176 to 336 rows surfaced **no new component
mismatch**.

> **"Full component parity" now means all eight components (`RURL-nolcjgdb`).**
> Until this freeze the verdict was built from **six** — `username` and
> `password` were never compared, in either posture, so the headline was silent
> on credentials. That mattered concretely: the general/opaque route used to
> discard userinfo entirely (`RURL-ovpguvva`), a component-level
> non-conformance on the exact posture whose figure read 100%, and **no scored
> number would have moved** when it broke or when it was fixed. The oracle was
> re-extracted at the *same* pinned upstream revision to carry upstream's
> `username`/`password` (24 and 13 non-empty rows respectively), which
> `make-wpt-fixture.py` had been dropping. The headline is unchanged at
> **336/336** — but it is now a wider claim over a stricter denominator, not the
> same claim re-stated: credentials are checked, and they pass.

> **This freeze supersedes the `4ed6c14` re-verification.** That note recorded
> that the three CSVs reproduced byte-identically at `4ed6c14`; it is now stale
> by construction, because the oracle itself changed. These artifacts were
> regenerated from the tree carrying the widened oracle and the two-posture
> harness — both landed together in the `RURL-ghdlrcjv` change, so re-running
> `standard-parity.R` at any commit from that change onward reproduces them.
> The `RURL-nolcjgdb` credential-scoring change supersedes it again on the two
> success CSVs, which gained four columns; the failure and RFC CSVs reproduced
> byte-identically across that change.
> The `general` figures are
> **new measurements**, not carried-over ones — the 176→336 widening was
> measured, not assumed to hold.

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
> 93** — 81 where it rejects what the RFC admits and 12 where it accepts what
> the RFC does not. Each of the 93 cites the ADR or ticket that owns it.
>
> **Re-baselined onto `serialize_url()` (P5.4), that same 257-row scope reads
> 179 / 78** — fifteen rows stop being departures because surface (c) declined
> them by POLICY (the ADR 0004 closed scheme set, the ADR 0002 reversible
> Unicode host) where the standard serializer matches the grammar. Over the
> current 325-runnable corpus it is 235 / 90. This remains an ACCEPTANCE axis;
> the full-string serialization headline is separate, and is measured on the
> WHATWG's own suite rather than on a curated subset: **336 exact / 0
> deviations over the 336 success rows** of the imported web-platform-tests
> corpus, scored against upstream's recorded `href`, plus **202 / 202**
> must-fail rows rejected (OR-022, P5.4 §2.1). That figure read **326 / 10**
> when P5.4 measured it; all 10 were the single host-less-`file:` family of
> RURL-uhwivndf, which is now fixed in the parse record (P1.3), leaving no
> deviation family on this oracle.
>
> That was **158 / 99** before `RURL-qrfrvmkg` bound the generic-URI gate
> uniformly across schemes; closing the `RURL-pfewxbhb` coverage gap moved
> exactly 6 rows from over-permissive to conformant-reject (18 → 12).
>
> **Both halves are single-cause, which earlier prose got wrong.** This
> attribution used to read "the ADR 0004 host-shape gate *and the closed scheme
> set*"; re-deriving it from `oracle-audit-rows.csv` at `4ed6c14` shows the
> closed scheme set contributes **zero** rows. Every one of the 81 is the ADR
> 0004 host/authority gate — percent-encoded reg-names **48**, other reg-name
> shapes **11**, empty host **8**, userinfo **6**, absent authority **5**, port
> shape **3** — and all **12** over-lenient rows are a single family,
> `non-ascii-or-control` (ADR 0002 Unicode hosts, ADR 0011 readable paths). The
> corpus is 202/282 WPT-sourced and therefore almost entirely `http`/`https`/
> `file`, so the scheme set never had the opportunity to fire.

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
At `scheme_acceptance = "general"`, over the full 336-row success corpus:
component non-conformances among accepted cases **0**; rejections of WPT-valid
input **0**. Query and fragment are scored in the full-component metric and have
no accepted-case mismatches, as are `username` and `password` since
`RURL-nolcjgdb`. At `web` the component mismatch count is likewise
**0**; its 160 rejections are the ADR 0004 allowlist, listed as residual #3
below rather than as a shortfall. The buckets described below are retained as
the record of what was closed, not as open items.

1. **Over-strict rejected path/control rows — closed.** Three WPT-valid success
   rows once rejected before component comparison; at `general` no success row
   rejects, and accepted rows have no scheme, username, password, host, port,
   path, query or fragment mismatch.
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

## Residual deviations — the attributed ledger

What is left, with the owner of each. Nothing here is unattributed, and nothing
here is accidental — that is the standard this ledger is held to, not the count.

| # | Residual | Kind | Owner | Reachable by the user? |
|---|---|---|---|---|
| 1 | Host mapping is not full **UTS-46**: ligatures, circled digits and zero-width code points pass through unmapped | Deliberate | ADR 0002; tracked `RURL-mumydwfh` (which reopened the closed `RURL-tvbvdjde`) | No — open work, not a dial |
| 2 | Punycode hosts keep the **A-label** rather than rendering back to Unicode | Deliberate (facts-not-policy) | ADR 0002 | Yes — `host_encoding` |
| 3 | The default scheme set is **closed** to the `web` allowlist — 160 of the 336 WPT success rows are refused at `web` | Deliberate | ADR 0004 | Yes — `scheme_acceptance = "general"`, where all 336 are accepted at full parity |
| 4 | No scheme is **inferred** for scheme-less input under `require` | Deliberate | ADR 0010 | Yes — `scheme_policy` |
| 5 | RFC 3986: **81** rows rejected that the grammar admits — all of them the host/authority gate | Deliberate strictness | ADR 0004 | No — the gate is not a dial |
| 6 | RFC 3986: **12** rows accepted that the grammar does not admit — all `non-ascii-or-control` | Deliberate leniency | ADR 0002 (Unicode hosts), ADR 0011 (readable paths) | Yes — `host_encoding`, `path_encoding` |

Row 1 is the only entry that is a **gap** rather than a dial, and it is the one
the companion study also names as its biggest carried caveat. Rows 2–4 are the
design's actual claim: a deviation that is reachable from a documented argument
is a policy the caller chooses, not a defect they are stuck with.

**Measurement scope is still a limit, but a different one.** The opaque / `ws:`
/ `wss:` gap that used to be listed here is closed: `RURL-ghdlrcjv` widened the
success fixture to every scheme, and those rows are now **measured and
conformant** at `general` rather than silently unmeasured. What **this study**
does not measure is **base-relative resolution** (see the held-axis note above);
the two `about:blank`-based rows are out of its scope by construction. That is
no longer a repository-level gap: RURL-fupsemxr T2.1 imported the complementary
274-row corpus and `tests/testthat/test-wpt-base-relative.R` scores it (247
exact / 27 enumerated differences, OR-024). Keep the two figures apart — they
are disjoint populations and summing them would invent a rate neither harness
measured.

## Files

| File | Contents |
|---|---|
| `whatwg-success-scored.csv` | per success case at `scheme_acceptance = "web"`: accepted + per-component `*_ok` + rurl vs expected |
| `whatwg-success-scored-general.csv` | the same, at `scheme_acceptance = "general"` — the grammar figure |
| `whatwg-failure-scored.csv` | per failure case at `"web"`: rurl status + conformant (rejected) |
| `whatwg-failure-scored-general.csv` | the same at `"general"` — the in-band evidence that the grammar, not the allowlist, rejects the 36 non-web-scheme rows |
| `rfc-probes-scored.csv` | per RFC probe: pass + `is_departure` + rurl vs expected + section + `rurl_deviation` |
| `run-console.txt` | verbatim console incl. the headline table |

Re-run and re-freeze whenever a conformance issue closes; the headline numbers
are the regression metric.
