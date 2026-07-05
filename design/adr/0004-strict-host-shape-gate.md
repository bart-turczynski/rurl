# ADR 0004: Strict host-shape gate on scheme-less input, relaxable under `url_standard`

- **Status:** Accepted
- **Date:** 2026 (rurl 1.5.0)
- **Tracking:** RURL-muwpjsmn; host model under RURL-luwvkwhd

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
marks out-of-range / >4-part hosts fatal.

## Consequences

- Default (no selector) behavior stays strict and byte-for-byte compatible.
- The numeric-host relaxation is opt-in and standard-specific, and because it
  changes *what is parsed* it is Stage-A-affecting (enters the parse cache key;
  see ADR 0003).
- **Standing rule:** keep the allowed-scheme set closed
  (`http`/`https`/`ftp`/`ftps`); do not add `ws`/`wss`/`file` for the
  SEO/crawl audience.
