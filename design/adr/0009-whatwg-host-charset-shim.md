# ADR 0009: Accept curl-rejected-but-WHATWG-valid host code points under `whatwg` (host-charset shim)

- **Status:** Accepted
- **Date:** 2026-07-07 (rurl 2.3.0)
- **Tracking:** RURL-dxwxeamq. Boundary-case reference: RURL-ffrkfdcq (ada-008,
  closed as documented boundary). Relates to ADR 0002 (reversible host), ADR
  0004 (host-shape gate), ADR 0007 (`url_standard` governed axes).

## Context

rurl delegates host parsing to libcurl (`curl_parse_url()`). libcurl enforces a
**narrower host allowed-set than the WHATWG URL Standard**: it rejects host code
points that WHATWG keeps in the host. When libcurl rejects, `curl_parse_url()`
errors and rurl drops the **entire row** — not just the host — so a WHATWG-valid
URL fails to parse under the `whatwg` profile.

This is the mirror image of ADR 0004's forbidden-code-point boundary (where
WHATWG is *stricter* than rurl's default): here WHATWG is *more permissive* than
libcurl.

### The delta (empirically enumerated, ada-cross-checked)

Swept printable ASCII (U+0021–U+007E) as a mid-host character through libcurl
and cross-checked each against **ada** (`adaR`, a WHATWG-conformant reference).
libcurl rejects exactly **15** code points that WHATWG keeps verbatim in the
host:

```
!  "  $  &  '  (  )  *  +  ,  ;  =  `  {  }
U+0021 22 24 26 27 28 29 2A 2B 2C 3B 3D 60 7B 7D
```

ada keeps all 15 literally in the domain (e.g. `http://a'b` → host `a'b`, href
`http://a'b/`); with UTS-46 `UseSTD3ASCIIRules=false` these are *valid*, not
mapped or percent-encoded.

Findings that shape the decision:

- **`%` (U+0025) is NOT in the set.** ada drops it too (forbidden *domain* code
  point unless a valid `%`-escape), so libcurl rejecting it is correct. The shim
  **excludes** `%`.
- **The gap spans both host types** — opaque hosts (`foo://a'b`) are rejected by
  libcurl for the same 15 points and kept by ada. Not special-scheme-only (but
  the first slice is scoped to special schemes — see Consequences / follow-up).
- **All 15 are non-structural** — none delimits userinfo (`@`), port (`:`), path
  (`/`), query (`?`), or fragment (`#`); those are forbidden-host points libcurl
  handles correctly. So the host span is unambiguously locatable *before* any
  substitution, using only ordinary delimiters.
- **Reverse case, out of scope:** libcurl *accepts* `|` (U+007C) in a host,
  which WHATWG *forbids* (ada → NA). That is libcurl being *broader* — the
  opposite of this ADR. ADR 0004's forbidden-code-point gate already covers the
  reject side for the ASCII forbidden set; the residual `|`-specific over-permit
  is tracked separately, not here.

## Decision

Add a fourth pre-curl `whatwg`-gated transform — a **host-charset shim**
(`.shim_whatwg_host_charset_vec()`) — alongside the existing family
(`.strip_whatwg_control_chars_vec`, `.rewrite_whatwg_backslashes_vec`,
`.map_whatwg_domain_separators_vec`) in `.prepare_urls_for_curl_vec()`. It runs
**only** under `url_standard == "whatwg"` and is a byte-for-byte no-op otherwise
(RFC 3986 profile and the default `NULL` are unchanged).

**Mechanism — sanitize the host span for curl, restore the true host after.**
libcurl's *only* job for these rows is confirming structure; it never
percent-decodes the host, and rurl owns the host string itself. So:

1. Locate the host span (unambiguous — gap chars are non-structural) and
   **capture the true host string**.
2. If the host contains any of the 15 gap code points, replace each 1:1 with a
   single benign filler letter. A 1:1 letter swap is **length- and
   delimiter-preserving**, so libcurl returns byte-identical scheme / userinfo /
   port / path / query / fragment; only its `$host` is a placeholder.
3. `curl_parse_url()` now succeeds; the row survives.
4. **Restore:** in `._parse_stage_a_vec()`, for shimmed rows, overwrite the
   extracted `raw_host` with the captured true host string *before* IP detection
   and the host model run. No per-character reversal — we carry the whole
   captured string.

Why an explicit restore is required (verified by tracing
`.apply_host_standard_model_vec()`): the model overwrites `host` with
`input_host` **only for IPv4 attempts**; an ordinary reg-name (`a'b.example.com`)
keeps `host = curl_host` in both the `rfc3986` and `whatwg` branches. So relying
on `input_host` alone would leak the filler placeholder for reg-name hosts. The
explicit post-curl restore lands the true host in the same `host` variable the
downstream gates read.

This is safe and *correct* by construction: once the true host is restored, the
`whatwg` forbidden-code-point gate (`.WHATWG_FORBIDDEN_HOST_CP`) runs on it and
**passes all 15** gap points (none is a forbidden host code point — verified)
while still rejecting the true forbidden set (`| ^ %` → fatal — verified). The
shim widens *only* libcurl's charset bottleneck, not any policy; `%` is excluded
from the shim set, so it continues to reject under `whatwg` exactly as before.

## Consequences

- **Default and `rfc3986` unchanged** — the shim is `whatwg`-only and a
  byte-for-byte no-op elsewhere. RFC 3986 keeps its reg-name permissiveness via
  the existing reversible-host path (ADR 0002) and still inherits libcurl's
  stricter charset (these hosts stay dropped there).
- **Stage-A-affecting** — it changes *what is parsed* (rows that were dropped now
  parse), so it enters the parse cache key like the other pre-curl transforms
  (ADR 0003).
- **ADR 0002 preserved** — the shim does not touch the punycode / reversible-host
  helpers; the true host is a captured string, rendered by the unchanged host
  path.
- **ADR 0004 preserved** — the host-shape gate and the forbidden-code-point
  reject gate still run on the recovered host; the shim only removes libcurl's
  charset veto, it does not bypass rurl's own host validation.
- **ADR 0007** — this makes "host allowed-set" a governed axis under `whatwg`,
  consistent with the profile model: `whatwg` accepts the 15 points, `rfc3986`
  is unaffected.
- **New diagnostic** — `host-charset-shimmed` (ADR 0006) fires on rows where the
  shim replaced a gap byte, mirroring `control-char-stripped` /
  `invalid-reverse-solidus`, so the mutation is surfaced, not silent.
- **Corpus impact (ada-008 / ada-005).** ada-008 (`http://///\'`) now conforms —
  rurl(whatwg) reproduces Ada's `http://'/` — and leaves the documented
  nonconformance set. ada-005 (`example.com` `` ` `` `x.example.com`, a
  scheme-less backtick host) flips from an *incidental* both-reject (rurl used to
  reject on the backtick after inferring `http://`) into a documented
  scheme-inference divergence: rurl infers a scheme (ADR 0004) and now keeps the
  backtick host, so it accepts where the scheme-less WHATWG oracle fails for want
  of a base URL. Both are recorded in `tests/testthat/fixtures/`. **Update (ADR
  0010, rurl 2.4.0):** this ada-005 scheme-inference divergence is now
  **opt-out-able** — under `scheme_policy = "require"` (RURL-vzgeurae) rurl
  rejects the scheme-less form, matching Ada exactly on that axis. The
  divergence documented here is therefore rurl's *default* leniency posture, not
  a fixed behavior.

## Follow-up (deliberately out of this slice)

1. **`rfc3986` scope.** RFC 3986 `reg-name` also permits sub-delims
   (`! $ & ' ( ) * + , ; =`) — a subset of the gap set — so libcurl is arguably
   too strict for `rfc3986` too. Scoped to `whatwg` only for now (the ticket's
   ask); widening to `rfc3986` is a separate decision.
2. **ftps and opaque hosts.** This slice is scoped to WHATWG special schemes
   (http/https/ftp). For opaque/non-special hosts WHATWG percent-encodes some
   code points rather than keeping them literal, so the recovered-token path
   would need to match WHATWG opaque-host serialization before widening there.

## Prototype

`_scratch/dxwxeamq-host-charset-shim-proto.R` (gitignored) proved the seam
end-to-end before wiring: all 15 gap chars parse with structure preserved and
the true host recovered; normal URLs are a no-op; `%` stays rejected; a gap char
outside the host is never touched.
