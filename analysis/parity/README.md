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

WHATWG success is scored in rurl's **canonical-output config**
(`scheme_policy="require", host_encoding="idna", path_encoding="encode"`) — the
settings that ask rurl for the WHATWG serialization. A residual mismatch there
is genuine: **the spec output cannot be reached by any knob.**

### Held axis — `scheme_acceptance = "web"` (the default)

Every figure below is scored at rurl's **default** scheme acceptance, the
curated `http`/`https`/`ftp`/`ftps`/`file` allowlist (ADR 0004). That is a
deliberate division of labour with the companion study, and it has to be read
carefully in both directions:

* The **success** fixture is itself limited to `http`/`https`/`ftp`/`file` —
  `make-wpt-fixture.py` hardcodes that set. So the success figures are scored at
  `web` over a corpus that never leaves `web`, and they say **nothing yet** about
  opaque, `ws:` or `wss:` serialization. Widening the carve-out to non-special
  schemes — and re-cutting the oracle-provenance records that key off the
  fixture digest — is tracked as **`RURL-ghdlrcjv`**. Until it lands, read
  176/176 as *"complete on the special schemes the spec suite covers"*, not as
  *"complete."*
* The **failure** fixture is not limited that way, so `202/202` is a real
  whole-corpus rejection result. Measured at `4ed6c14`: 158 of the 202 rows
  carry a `web` scheme, **36 carry a non-web one** (`sc` 12, then `data`,
  `intent`, `javascript`, `mailto`, `stun`, `turn`, `urn` at 3 each,
  `non-special` 2, `asdf` 1), and 8 carry no scheme at all. At this posture
  those 36 are rejected by the **closed scheme set**, not by the WHATWG
  grammar — so they are correctly rejected, but for a reason the grammar did
  not have to supply. The complementary evidence that the grammar itself rejects
  them comes from the disagreement study, which holds `scheme_acceptance =
  "general"`; that posture is exactly what exposed the `sc://@/` over-acceptance
  closed by `RURL-jxvibxqq`.

`scheme_acceptance` is a **public argument**, not an internal flag: it is in the
formals of the exported parse and accessor surface, so both postures are things
users actually run. Between them, the two frozen studies cover both —
`web` here, `general` in [`../disagreement/`](../disagreement/README.md).

## Headline (rurl 2.8.0, 2026-07-26, `scheme_acceptance = "web"`)

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
173/176) are gone. Against this oracle the WHATWG profile is fully conformant —
with "this oracle" carrying the scope stated above.

> **Re-verified at `4ed6c14`.** The general-mode parser fixes that landed after
> this run was frozen (`RURL-glphqenm` opaque-path masking, `RURL-jxvibxqq`
> host-missing authority) are `general`-posture edits, so they were not expected
> to move a `web`-posture score. That was **checked, not assumed**: re-running
> the harness at `4ed6c14` reproduces `whatwg-success-scored.csv`,
> `whatwg-failure-scored.csv` and `rfc-probes-scored.csv` **byte-identically**.
> The frozen figures are current.

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

## Residual deviations — the attributed ledger

What is left, with the owner of each. Nothing here is unattributed, and nothing
here is accidental — that is the standard this ledger is held to, not the count.

| # | Residual | Kind | Owner | Reachable by the user? |
|---|---|---|---|---|
| 1 | Host mapping is not full **UTS-46**: ligatures, circled digits and zero-width code points pass through unmapped | Deliberate | ADR 0002; tracked `RURL-mumydwfh` (which reopened the closed `RURL-tvbvdjde`) | No — open work, not a dial |
| 2 | Punycode hosts keep the **A-label** rather than rendering back to Unicode | Deliberate (facts-not-policy) | ADR 0002 | Yes — `host_encoding` |
| 3 | The default scheme set is **closed** to the `web` allowlist | Deliberate | ADR 0004 | Yes — `scheme_acceptance = "general"` |
| 4 | No scheme is **inferred** for scheme-less input under `require` | Deliberate | ADR 0010 | Yes — `scheme_policy` |
| 5 | RFC 3986: **81** rows rejected that the grammar admits — all of them the host/authority gate | Deliberate strictness | ADR 0004 | No — the gate is not a dial |
| 6 | RFC 3986: **12** rows accepted that the grammar does not admit — all `non-ascii-or-control` | Deliberate leniency | ADR 0002 (Unicode hosts), ADR 0011 (readable paths) | Yes — `host_encoding`, `path_encoding` |

Row 1 is the only entry that is a **gap** rather than a dial, and it is the one
the companion study also names as its biggest carried caveat. Rows 2–4 are the
design's actual claim: a deviation that is reachable from a documented argument
is a policy the caller chooses, not a defect they are stuck with.

**Measurement scope is not a deviation, but it is a limit.** The success
fixture's `http`/`https`/`ftp`/`file` carve-out (see the held-axis note above)
means opaque, `ws:` and `wss:` serialization is currently **unmeasured** against
WPT rather than measured-and-conformant. It is listed here so no reader mistakes
the silence for a pass. Tracked: `RURL-ghdlrcjv`.

## Files

| File | Contents |
|---|---|
| `whatwg-success-scored.csv` | per success case: accepted + per-component `*_ok` + rurl vs expected |
| `whatwg-failure-scored.csv` | per failure case: rurl status + conformant (rejected) |
| `rfc-probes-scored.csv` | per RFC probe: pass + `is_departure` + rurl vs expected + section + `rurl_deviation` |
| `run-console.txt` | verbatim console incl. the headline table |

Re-run and re-freeze whenever a conformance issue closes; the headline numbers
are the regression metric.
