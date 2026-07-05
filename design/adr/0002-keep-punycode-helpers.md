# ADR 0002: Keep rurl's Punycode helpers; do not adopt `punycoder::host_normalize()`

- **Status:** Accepted
- **Date:** 2026-06-20
- **Tracking:** RURL-ntdnoywx

## Context

rurl renders the host reversibly through two helpers in `R/domain.R`:
`.normalize_and_punycode()` (host → A-label, for `host_encoding = "idna"`) and
`.punycode_to_unicode()` (A-label → Unicode, for `host_encoding = "unicode"`
and `get_host()`). `punycoder` also exposes `host_normalize()`, and a natural
question was whether rurl should collapse its two helpers onto it.

This was checked live (characterization diff recorded in the issue).
`host_normalize()` is purpose-built for canonical *comparison* form — which is
exactly why `pslr` applies it before PSL matching — not for rurl's reversible
host *rendering*.

## Decision

Keep both helpers. Do **not** replace them with `host_normalize()`.

## Consequences

- `host_normalize()` is not a drop-in for either helper: (a) it is
  one-directional with no inverse, so it cannot replace `.punycode_to_unicode()`
  at all; (b) on the encode path it force-lowercases (colliding with rurl's
  separate case policy — it would break `case_handling = "keep"`/`"upper"`) and
  returns `NA` for hosts rurl currently tolerates (STD3 `_`, leading/trailing
  hyphens, `--` in label positions 3–4 per CheckHyphens, over-DNS-length
  labels).
- The helpers render reversibly, *preserve case* (case policy is a separate
  later phase), and *tolerate* malformed-but-encodable hosts (lenient
  `strict = FALSE` fallback). They carry no per-TLD hardcoded workarounds (those
  were removed in the pslr migration; plain `puny_encode`/`puny_decode` handle
  `.ελ`/`.рф` correctly).
- **Standing rule:** do not alter these helpers to force-lowercase or to reject
  tolerated hosts. Revisit only if rurl gains a dedicated "canonical match key"
  surface where lowercasing + UTS-46 strictness are actually desired.
