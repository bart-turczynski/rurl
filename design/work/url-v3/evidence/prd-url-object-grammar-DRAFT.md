# PRD — The URL object, the render spectrum, and tiered parsing

- **Status:** DRAFT for owner review. Graduates to `design/prd/` once accepted (ADR 0008).
- **Drafted:** 2026-07-21, out of epic RURL-zxfqullq after audit RURL-mcrtedcp.
- **Supersedes:** the reactive "close the conformance gaps" framing of RURL-zxfqullq
  (T1–T7). The audit showed there are no conformance gaps to close; the real work is
  the library's grammar and its compute layering.
- **Related:** ADR 0006 (facts via companion helpers), ADR 0007 (`url_standard` selector),
  ADR 0011 (`path_encoding` orthogonal presentation), ADR 0012 (general parser / profiles);
  perf epics RURL-gvlqokul (vector core) + PSLR-bzqvsatk (columnar hot path);
  compliance-presets thread RURL-mpktqfto; punycoder/pslr CRAN release chain PSLR-hrpalwzo.

---

## 1. Background & motivation

The trigger was an epic to make rurl "best-in-class" against WHATWG + RFC 3986 by closing
live conformance gaps found in a 2026-07-21 adaR-0.3.5 audit.

A full re-audit (RURL-mcrtedcp) against **live adaR 0.3.5 + all committed oracles**,
sweeping every case across the meaningful dial bundles, settled it:

- **Genuine conformance gaps: ZERO.** 937 case×standard rows → 731 conform at a reachable
  bundle, 204 are documented-deliberate deviations, 246 are *phantom* (conform the instant
  you leave the default), 2 are the intentional mailto-D7 design.
- **Perfect paths exist.** RFC 3986: `url_standard="rfc3986"` → 100% (174/174). WHATWG:
  `url_standard="whatwg" + host_encoding="idna" + path_encoding="encode" +
  scheme_policy="require"` → 99.8% (532/533; the 1 residual is an infer/require trade-off,
  not a gap).
- **Robustness sweep clean:** 0 bugs over 192 dial combinations (0 identity-shifts from
  presentation knobs, 0 profile/manual divergence, 0 non-idempotence, 0 errors).

So the epic was chasing measurement artifacts: each "gap" had been measured at the *default*
dials, where the conformant behavior lives behind a non-default knob. The recurring failure
mode was running a check at the wrong setting, seeing surprising output, and concluding
"broken/missing" — when the behavior was one argument away.

**The real problem the exercise surfaced is the grammar of the library**: capabilities exist
but are entangled with the wrong dial, one whole output shape is missing, and the settings
are hard to access and reason about (25 knobs on one signature + a hidden conflict matrix).

---

## 2. The core model (the correction)

### 2.1 Original intent
1. A URL string exists.
2. `safe_parse_url()` runs.
3. The string is no longer a string — it is an **annotated object**: the input, decomposed
   into parts and annotated with derived notes (domain, tld, subdomain, is_ip, parse_status).
   It **round-trips to the input**; it does not normalize or opinionate. *"The input URL with
   additional notes."*
4. Users read the annotations (protocol, TLD, host, …).
5. Users get a URL string back **rendered with the settings they chose**.

The **annotated object is the parsed URL** — the noun. The `get_*` helpers are **producers**:
they *construct* a view (a rendered component, a rebuilt URL). They are not accessors reading
a canonical table.

### 2.2 The mistake
`clean_url` — a heavily-modified, opinionated **rendering** (it can strip the query, drop the
fragment, force https, remove www, kill tracking params) — got mistaken for *"the parsed
URL."* Two things caused the drift:
- it is the **only** rendering rurl offers, so it reads as "the output"; and
- it is **baked into `safe_parse_url`'s result as a column** (`clean_url`, one of 18),
  sitting next to the faithful annotations, so it looks like another annotation.

This is why we kept trying to make `clean_url` conformant — it never was, and never should be.

### 2.3 The correction: rendering is a spectrum
Rendering the annotated object back into a string is a **separate, parameterized step** — a
spectrum, all the same operation with different settings:

| render | meaning | exists today? |
|---|---|---|
| **identity** | the original string, round-tripped (zero change) | via raw components |
| **faithful / canonical** | standard-normalized, **zero opinion** — matches adaR `href` | **NO** |
| **clean** | opinions applied — `clean_url`, the high-opinion end | yes (`get_clean_url`) |

`clean_url` is the far end of the spectrum, not the truth. The **faithful render is the
missing point**, and it is the real conformance surface — which dissolves the whole
conformance confusion: `clean_url` stops needing to be conformant because it never was the
serialization.

---

## 3. Three jobs, three verbs

rurl does three separable jobs; only two are named well today.

| job | verb today | contract |
|---|---|---|
| **parse** → components | `safe_parse_url` + `get_*` accessors | readable, faithful parts |
| **serialize** → faithful standards URL | **MISSING** | exact reconstruction per standard, no editorializing (the adaR `href` analog) |
| **clean** → opinionated URL | `get_clean_url` | strip trackers, readable, "nice looking" |

Note rurl already has no `clean_url()` *verb* — the public function is `get_clean_url()`, and
`clean_url` is the *column*. So the faithful render must follow the same idiom:
a **`serialized_url` column + a `get_serialized_url()` accessor**, parallel to
`clean_url`/`get_clean_url` — not a differently-shaped `serialize_url()` verb.

---

## 4. Three setting axes (currently flat + entangled)

25 knobs sit on one signature with a hidden conflict matrix. They cluster into three axes the
API does not reflect:

- **A · Interpretation** — *what the input means*: `url_standard`, `scheme_acceptance`,
  `scheme_policy`, `scheme_relative_handling`.
- **B · Presentation** — *how to render*: `host_encoding`, `path_encoding`, `case_handling`,
  `path_normalization`, **`fragment_handling` (new)**.
- **C · Cleaning** — *editorial; `clean_url` only*: `www_handling`, `trailing_slash_handling`,
  `index_page_handling`, `query_handling`/`params_*`, `protocol_handling`, `sort_params`, …

### 4.1 The grammar bug
Axis B is exposed as loose knobs **and bundled by no preset**, so conformant output requires
hand-aligning B with A. Foot-guns:
- *"pick `whatwg` here, remember `idna` there"* — `url_standard="whatwg"` selects the parsing
  model but emits non-WHATWG serialization (97% vs the 99.8% four-knob bundle).
- `profile="whatwg"` ≠ `url_standard="whatwg"` — different `scheme_policy` (require vs infer).
- **No profile carries the presentation knobs** (`host_encoding`/`path_encoding`).

### 4.2 A hidden fourth axis: validation strictness
"How to treat a bad host" — **reject the whole URL** vs **return the raw host and flag it** —
is its own axis, but today it is *welded to `url_standard`*:

| input | `url_standard="whatwg"` (strict) | `url_standard=NULL` (lenient) |
|---|---|---|
| `http://münchen.de/` | ok · `münchen.de` | ok · `münchen.de` |
| `http://a�b.com/` (U+FFFD) | **error · host=NA** | warning · host=`a�b.com` |
| `http://­/` (soft-hyphen only) | **error · host=NA** | warning · host=`­` |

You **cannot** currently ask for "WHATWG's parsing model *but* hand me the raw host and flag
invalidity instead of rejecting the row" — a common data-cleaning need. Strictness should be
an explicit axis, not implied by the standard.

---

## 5. Compute tiering (do only the work the caller needs)

The internal architecture **already** separates the object from the render:
- **Stage A** (`._parse_stage_a_vec`, cached) = the annotated object: components + host
  normalization + domain/TLD via PSL.
- **Stage B** (`._parse_stage_b_vec`, uncached, per-call) = the render: case, www, trailing
  slash, query filtering, and the final `clean_url` assembly.

So the model is an **evolution of Stage A/B, not a rewrite.** The gap is that everything is
eager: Stage A always does PSL, and (for non-ASCII hosts) the UTS-46 validity gate, even when
the caller just wants a yes/no or a raw host.

### 5.1 The two heavyweights
- **PSL** (`pslr`): domain/tld/subdomain — runs eagerly in Stage A for every parse.
- **punycoder**: **already the more-tiered of the two** — every call is gated on non-ASCII
  hosts (an all-ASCII URL never touches it, e.g. `parse-phases.R:1863`), and its round-trips
  are memoized (`puny_encode`/`puny_decode` caches). Its one eager call is `host_normalize()`
  as the UTS-46 validity gate (non-ASCII only). It is also the **engine of the faithful host
  render** — the `xn--…` form is literally its output.

### 5.2 Proposed tiers
| tier | work | cost |
|---|---|---|
| **0 · validate** | structural validity (+ optional IDN validity) | cheapest; no PSL; punycode only if IDN-validating |
| **1 · components** | scheme/host/port/path/query/fragment/userinfo | no PSL; no punycode (raw host) |
| **2 · annotate** | domain/tld/subdomain/is_ip via PSL | **lazy** — pay only if asked |
| **3 · render** | identity / faithful / clean string | on demand |

A validator pays Tier 0; a host-extractor pays Tier 0–1; only domain/TLD work pays PSL; only
rendering pays reconstruction. This *is* the sharpened target for the perf epics
(RURL-gvlqokul / PSLR-bzqvsatk).

---

## 6. Proposed changes

1. **`get_serialized_url()` + `serialized_url` column** — the faithful render. Reconstructs
   exactly per `url_standard`, applies **no** cleaning (axis C), derives axis-B defaults from
   the standard (whatwg → idna host + spec path encoding; rfc3986 → RFC-canonical), and
   **retains the fragment**. Matches adaR `href` byte-for-byte. Zero dials for the common case.
2. **`fragment_handling` knob** (`keep`/`strip`) — the one genuinely missing knob; the
   faithful render needs it, and `clean_url` gains fragment control too. (Today even
   `get_clean_url(query_handling="keep")` drops `#frag` — there is no fragment knob.)
3. **`standard=` drives presentation defaults *per output*** — so the conformance case needs
   no manual B alignment, while components and `clean_url` stay readable by default.
4. **Validation-strictness axis** — decouple "reject vs return-and-flag a bad host" from
   `url_standard`, so WHATWG parsing + lenient host extraction becomes expressible.
5. **Lazy PSL tier + a validate/components entry** — skip PSL (and, for raw host, the UTS-46
   gate) when the caller doesn't need annotations.
6. **De-conflate `clean_url` from the parse table** (Phase 2) — make rendering an explicit,
   parameterized step rather than a column baked into `safe_parse_url`.
7. **Preset coherence** (Phase 2) — a profile bundles A(+B+C) coherently; resolve the
   `whatwg` foot-gun; ties into RURL-mpktqfto.

---

## 7. Compatibility stance

rurl is on CRAN; back-compat is a hard constraint, **narrowed** by the owner mandate that
spec-exactness beats preserving old *output values* (that governs values, not API shape).

- **Additive-first:** `get_serialized_url`, `serialized_url` column, `fragment_handling`, the
  validate/components entry, and the strictness axis are all additive — no breakage.
- **`clean_url` semantics preserved** through Phase 1. The Phase-2 de-conflation (removing the
  baked-in column) is done with deprecation + a transition window, never a hard break; any
  rename (`url_standard` → `standard`) is alias-with-deprecation.
- **Default byte-identity** (ADR 0012 hard constraint) preserved for the default posture
  throughout; the parity harness + the RURL-mcrtedcp audit sweep are the regression gates.
- **punycoder release chain:** everything needed (`host_normalize`, `puny_encode`,
  `puny_decode`) exists at the pinned `punycoder (>= 1.2.0)`. This work targets the current
  API and adds **no** new punycoder-version blocker; it sequences around the in-flight
  1.2.1 release rather than entangling with it.

---

## 8. Phasing

- **Phase 0 — fast, additive, no architecture change:** `fragment_handling` → then
  `get_serialized_url()` + `serialized_url` column with an adaR-`href` parity test. Ships the
  missing render point immediately.
- **Phase 1 — perf + axis:** lazy PSL tier, validate/components entry, validation-strictness
  axis. Big throughput win; additive API.
- **Phase 2 — grammar, compat-gated:** render spectrum made explicit, `clean_url`
  de-conflated from the parse, preset coherence, deprecations. The genuine "rethink," done
  once the model is proven and with a deprecation path.

---

## 9. Open decisions (owner)

1. `get_serialized_url` — is `url_standard`/`standard` **required**, or default `"whatwg"`?
2. `fragment_handling` default — retain (`keep`) by default in the faithful render?
3. How far to take the **validation-strictness** axis — a simple `on_invalid_host =
   reject|flag`, or a fuller strictness dial?
4. **Rename** `url_standard` → `standard` (aliased) — worth the churn?
5. **Profile unification** — one canonical meaning for `whatwg`, or layer presets and document
   the difference?
6. **Phase 2 reach** — does `safe_parse_url` stop carrying `clean_url` entirely, or keep it as
   a deprecated convenience column through a transition?

---

## 10. Success criteria

- The faithful render matches adaR `href` byte-for-byte across the committed corpus.
- Validate/extract paths are measurably faster (skip PSL; skip punycode on the raw-host path).
- There is **one obvious way** to get each of: the parts, the canonical URL, the clean URL.
- No conformance regression (parity harness + the RURL-mcrtedcp sweep as CI-adjacent gates).
- Existing calls behave identically through Phase 1.

---

## 11. Non-goals

- **Not** changing conformance behavior — the audit proved it is already complete.
- **Not** a breaking rewrite — this is an evolution of Stage A/B + additive verbs.
- **Not** resolving the mailto-D7 accessor decision — separate and parked (RURL-glphqenm).

---

## 12. Suggested decomposition (once accepted)

Phase 0
- U1 — `fragment_handling` (axis B): the smallest unit; unblocks a faithful whole-URL render.
- U2 — `get_serialized_url()` + `serialized_url` column; standard-driven B defaults;
  adaR-`href` parity test over the corpus.

Phase 1
- U3 — lazy PSL tier (make Stage-A domain/TLD pay-on-access).
- U4 — validate/components entry point (Tier 0/1) skipping PSL (+ raw-host tier skipping the
  UTS-46 gate).
- U5 — validation-strictness axis (`on_invalid_host` etc.), decoupled from `url_standard`.

Phase 2 (design-gated)
- U6 — make rendering an explicit step; de-conflate `clean_url` from `safe_parse_url` (with
  deprecation).
- U7 — preset coherence (profiles carry B; resolve the `whatwg` foot-gun); ties RURL-mpktqfto.
- U8 — docs/NEWS + graduate this PRD to `design/prd/` + an ADR on the object/render/tier model.

Ties: U3/U4 are the sharpened form of RURL-gvlqokul + PSLR-bzqvsatk; U7 subsumes
RURL-mpktqfto; this PRD supersedes the T1–T7 framing of RURL-zxfqullq (T2 already shipped as
the one real fix, #197).

---

## Appendix A — evidence (all verified 2026-07-21)

- Audit RURL-mcrtedcp: `_scratch/orchestrate/audit/` — `LEDGER.md`, `dials.md`,
  `conformance-ledger.csv`, `robustness-findings.md`, re-runnable `sweep.R`.
- Faithful render already reachable, matches adaR `href`:
  `get_clean_url(u, query_handling="keep", host_encoding="idna")` →
  `http://xn--hxajbheg2az3al.xn--jxalpdlp/café?q=1` (identical to adaR), **except** it drops
  the fragment (no `fragment_handling` knob — hence U1).
- adaR splits the representations: `href` = Punycode serialization; `hostname` = readable
  Unicode component. rurl's readable default already equals adaR's component; `host_encoding=
  "idna"` equals adaR's serialized host.
- Validation strictness welded to `url_standard`: whatwg rejects a bad IDN host (host=NA);
  NULL returns the raw host + a warning (§4.2 table).
- Stage A/B split + caches: `ARCHITECTURE.md` (`._parse_stage_a_vec` cached incl. PSL;
  `._parse_stage_b_vec` uncached render); punycode round-trips cached (`R/zzz.R`), non-ASCII
  gated (`R/parse-phases.R:1863`).
