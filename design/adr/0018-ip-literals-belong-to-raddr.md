# ADR 0018: IP literals belong to `raddr`, address policy to `ssrfr`

- **Status:** Accepted
- **Date:** 2026-08-26 (the ruling was taken earlier and is recorded here)
- **Tracking:** RURL-cbrfphfr (the scope ruling and the migration gate).
  RURL-dxwsksor (the missing `ipv6-*` diagnostics) and RURL-rhelasnq's
  loopback/private/link-local finding both resolve to this decision.
- **Relates to:** ADR 0004 (host-shape gate), ADR 0012 (general parser scope),
  ADR 0006 (diagnostics stay companion helpers).

## Context

`rurl` parses IP literals in the host position and classifies some of them, and
it has repeatedly been asked to do more: classify loopback, private and
link-local ranges; emit `ipv6-*` diagnostics to match the seven `ipv4-*` ones;
adopt a stricter IPv4 reading. Each request is reasonable on its own and none of
them has an owner inside this package.

`~/Projects/raddr` is the sibling that does own them. It already models the
multi-reading problem the same way `rurl` models `url_standard` — `addr_whatwg`,
`addr_curl`, `addr_pton`, `addr_aton`, `addr_getaddrinfo`, `addr_strict` — and
it carries IANA special-purpose classification (`addr_classify`, `addr_within`).
Its WHATWG reading of an address literal is an exact match to `rurl`'s.

The trap is that the two packages look like they overlap when they do not. The
boundary is by **layer**, not by parse-versus-serialize:

| Layer | Owner |
|---|---|
| address literal | `raddr` |
| URL host | `rurl` (`R/parse-web.R`) |
| resolver | `raddr` |
| allow/deny policy over an address | `ssrfr` |

## Decision

**D1 — IP-literal parsing, IANA special-purpose classification and resolver
semantics belong to `raddr`.** SSRF-style allow/deny policy over an address
belongs to `ssrfr`. **`rurl` holds no address ranges at all**, and the absence
of loopback/private/link-local classification here is this layering, not an
oversight.

**D2 — the host/address boundary stays in `rurl`,** because `raddr` has no
reg-name concept. `0xg`, `0x1p`, `...`, `.` and `..` are `rurl` correctly
returning a reg-name; that boundary is gated by `.host_ends_in_number_vec()` in
`R/parse-phases.R`, which gates the IPv4 attempt.

**D3 — no further IP work happens in `rurl`** until `raddr` ships a URL-host-layer
curl dialect. Requests for in-tree IPv6 classification are answered by waiting
for `raddr` and projecting its facts, never by new address code here.

## Consequences

**`raddr::addr_curl()` is not interchangeable with `rurl`'s URL-host parse, and
this is the counter-evidence that makes D3 binding rather than aspirational.**
`addr_curl()` is a **resolver**-layer reading — `aton` falling back to `pton` —
while `R/parse-web.R` is `rurl`'s reproduction of libcurl's *URL host* parse,
calibrated against real libcurl by per-octet sweep. Both are right about their
own layer. Delegating `rurl`'s web profile to `addr_curl()` regresses **7 of 18
sampled hosts** — `192.0.048.1`, `08.0.0.1`, `09.1.1.1`, `1.2.3.09`, `0Xff`,
`4294967296`, `0x100000000` — confirmed against Apple libc, where
`inet_aton("192.0.048.1")` rejects (`048` is not octal) and `inet_pton` accepts
it as `192.0.48.1`. Ungated delegation therefore turns valid hosts into parse
failures in a CRAN package.

The first assessment of this seam read "20 of 21 agree", scored on a corpus of
IPv4 candidates only — a population that structurally cannot surface either the
reg-name boundary or the layer confusion. Any future adapter is measured on a
corpus that contains reg-names.

The serializer question is **closed** and is not a blocker: the divergence is
exactly `::ffff:0:0/96`, and a short serializer over `raddr`'s exported
`addr_to_bytes()` reproduces `rurl`'s output on all 18 sampled rows including
first-run-wins compression. `rurl` does not need `raddr` to grow a WHATWG
renderer.

The whole `rurl` reference to `raddr` today is prose. There is no call site and
no `Remotes:` entry, so nothing in this repository breaks if the migration never
happens; what this ADR prevents is an adapter written before the question below
is answered.

## Open question — deliberately unresolved

**Does `raddr` grow a URL-host-layer curl dialect?** It decides whether
`R/parse-web.R` can ever be deleted, and it must be settled **before any adapter
is written** — an adapter built on `addr_curl()` is the regression measured
above. Carried by **RURL-cbrfphfr**. Until it is answered, D3 stands.

What would justify revisiting this ADR: `raddr` shipping that dialect, or
`raddr` reaching CRAN and a measured re-run of the 18-host sample showing no
regression.
