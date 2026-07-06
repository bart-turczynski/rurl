# ADR 0004: Strict host-shape gate on scheme-less input, relaxable under `url_standard`

- **Status:** Accepted
- **Date:** 2026 (rurl 1.5.0)
- **Tracking:** RURL-muwpjsmn; host model under RURL-luwvkwhd. WHATWG-conformance
  scope boundaries recorded from the WPT-failure triage (RURL-zjbzdmdr); the one
  known numeric-host gap was fixed under RURL-cdjnhnvf.

## Context

rurl infers a scheme for scheme-less input (`example.com` → `http://example.com`)
so crawl-export columns without protocols still parse. But that inference must
not fabricate a URL from a string that is not a host: `"asdfghjkl"`, `"12345"`,
`"/path"`, or a non-canonical IP literal (integer/hex/octal/short forms, or
leading-zero octets like `192.168.010.1`) are not authority-based URLs and
should be rejected, not coerced. libcurl, left to itself, coerces several of
these (e.g. numeric hosts to IPv4).

## Decision

Gate scheme inference behind a strict host-shape check and validate input IP
literals. A scheme-less non-host or non-canonical IP literal yields
`parse_status = "error"` rather than a fabricated URL. rurl's allowed scheme
set is closed to `http`/`https`/`ftp`/`ftps`; any other scheme
(`mailto:`/`tel:`/`ws:`/…) is an error. Scheme-less input carrying userinfo
(`user@example.com`) is a `warning-userinfo`: components resolve but `clean_url`
is `NA` (rurl will not mint a canonical URL from an ambiguous email-shaped
string).

The `url_standard` selector then governs the numeric-host model on top of this
gate (`.apply_host_standard_model_vec()`): under a selector the Phase-1 hard
reject of numeric hosts is lifted — RFC 3986 restores the original token as a
reg-name (undoing libcurl's coercion), WHATWG keeps libcurl's IPv4 coercion and
marks out-of-range / >4-part hosts fatal. The WHATWG fatal decision is driven by
WHATWG's own "ends in a number" trigger (`.host_ends_in_number_vec()`: the final
host label, after dropping one trailing dot, is a decimal integer or a hex
literal) — **not** by whether libcurl chose to coerce the host. Any such host
must parse to a canonical IPv4 or the whole host parse fails; there is no
reg-name fallback under `whatwg` (RURL-cdjnhnvf).

## Consequences

- Default (no selector) behavior stays strict and byte-for-byte compatible.
- The numeric-host relaxation is opt-in and standard-specific, and because it
  changes *what is parsed* it is Stage-A-affecting (enters the parse cache key;
  see ADR 0003).
- **Standing rule:** keep the allowed-scheme set closed
  (`http`/`https`/`ftp`/`ftps`); do not add `ws`/`wss`/`file` for the
  SEO/crawl audience.

## Documented scope boundaries (WHATWG conformance)

The WPT-failure triage (RURL-zjbzdmdr; the 202 R-representable `failure` cases
from WHATWG `url/resources/urltestdata.json`) established where rurl's `whatwg`
profile deliberately does **not** reproduce WHATWG's hard-reject behavior. These
are intentional boundaries, not defects — recorded here so the paper can cite
"known, documented limitation":

- **Forbidden host code points are surfaced, not rejected.** WHATWG's
  forbidden-host-code-point set (`|`, DEL, U+FFFD, U+00AD, …) triggers hard
  failure. rurl instead renders the host reversibly (ADR 0002) and flags it
  (`warning-no-tld` + diagnostics, ADR 0006) rather than refusing the URL.
  Forbidden-code-point *rejection* is not a governed axis (ADR 0007). The
  UTS-46-*ignored* code points among these (e.g. U+00AD soft hyphen) fall under
  the DNS/UTS-46 probe (RURL-rxbhotzu), which flags rather than maps-and-fails.
- **RFC 3986 reg-name permissiveness is by design.** Under `rfc3986`, numeric
  and near-out-of-charset hosts are kept as reg-names (RFC 3986 has no
  numeric-host special case). This is an expected profile divergence from
  `whatwg`, not a defect.
- **NUL bytes cannot round-trip.** R strings cannot hold a `U+0000`; a URL with
  an embedded NUL is truncated at the NUL before rurl (or the corpus tooling)
  sees it. Inherent R limitation — documented as a can't-represent case, not a
  parse decision.

**Resolved (RURL-cdjnhnvf):** the WHATWG numeric-host rule was previously gated
on libcurl's IPv4 coercion, so obfuscated forms libcurl leaves as reg-names (hex
`0x4`, octal/leading-zero `09`, mixed `foo.09`, short/>4-part dotted, trailing-dot
`1.2.3.08.`) slipped through as `warning-invalid-tld` instead of rejecting — a
conformance gap on a governed axis (ADR 0007), surfaced by the WPT-failure
import (all 18 bucket-A cases). The fix re-anchors the WHATWG fatal decision on
WHATWG's "ends in a number" trigger (see Decision above) rather than libcurl's
coercion choice, so every such host now rejects under `whatwg`; `rfc3986`/`NULL`
reg-name behavior is unchanged. Regression fixtures live in
`tests/testthat/fixtures/url-standard-conformance.csv`
(`host-ends-in-number-*`).
