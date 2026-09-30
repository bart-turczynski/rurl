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

## Amendment: the decode helper settles each label with rurl's own RFC 3492 decode

*Added RURL-oizpyvdz, 2026-09-30, with the owner's approval. Appended rather
than edited in place so no line citation into this file moves.*

punycoder 1.3.0 makes `puny_decode(strict = FALSE)` require letter-digit-hyphen
basic code points, so `.punycode_to_unicode_vec()` stopped decoding
`xn--a_-wia` ("a_" + U+00E4) and rendered the A-label under every arm.
RFC 3492 §6.2 accepts any basic code point, and UTS #46 §4 (ToUnicode,
UseSTD3ASCIIRules false) decodes the label.

The helper still decodes with punycoder first. `.ace_decode_settle()` then
retries an `xn--` label punycoder rejected with `.rfc3492_decode()`
(RURL-mfmgauos), and keeps the spelling of any `xn--` label whose payload holds
a URL delimiter (`#` `/` `:` `?` `@`), whichever decoder read it, so a rendered
host never gains one (RFC 3986 §3.2.2).

- **The standing rule holds.** The helper still preserves case (the basic
  string is copied as written) and still rejects nothing it tolerated: a label
  that fails every decode keeps its spelling, as before.
- **Measured on 36,149 fuzzed labels.** Under punycoder 1.3.0 the helper's
  output equals its output under a 1.2.1 build without libidn2 on every label.
  A 1.2.1 build linked against libidn2 also decodes 35 labels by reading `_`
  as a Punycode digit, which RFC 3492 §5 does not allow; 1.3.0 rejects them,
  and this change does not reach them. On either build, under 1.2.1 it moves
  only the 130 labels 1.2.1 decoded with a `:` after a prefix that is not
  scheme-shaped. Such a host reaches the helper only as a percent-decoded
  `%3A` under `url_standard = "rfc3986"`; `NULL` and `whatwg` reject it
  before rendering, so the `NULL` freeze (ADR 0007, ADR 0016) is untouched.
- The scalar `.punycode_to_unicode()` path with a non-default `decode_fn`
  exists for test doubles and does not settle labels, so a double's answer is
  what the caller sees.
