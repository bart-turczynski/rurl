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

## Amendment: projecting `raddr` facts is not IP work in `rurl`

*Added RURL-dxwsksor, 2026-10-07. Appended rather than edited in place so no
line citation into this file moves.*

**Owner ruling, 2026-09-30.** Projecting `raddr`'s address facts into `rurl`'s
diagnostic vocabulary is not the "further IP work" D3 holds back. It replaces
none of `rurl`'s host parsing and builds no adapter on `addr_curl()`, so it does
not touch the open question above. D2 stands: the host/address boundary stays
in `rurl`, and only a host `rurl` has already classified as an IPv6 literal is
handed to `raddr`.

**Owner ruling, 2026-10-07.** `raddr` is an `Imports` dependency, not
`Suggests`. The owner's reason is that the ecosystem's packages (`rurl`,
`raddr`, `ssrfr` and the rest) rely on each other rather than on
alternatives. The diagnostics' reason is that a fact computed only when a
suggested package happens to be installed would make a missing token mean
nothing, and the facts exist so that a guard can rely on them. The floor is
`raddr (>= 0.1.2)`, the current CRAN release on 2026-10-07. It carries
`addr_embedded_kind()` and `addr_embeddings()` with all eight embedding kinds.

What this changes in the text above:

- §Consequences says the whole `rurl` reference to `raddr` is prose, with no
  call site. From this amendment there is one: `.ipv6_host_diagnostics()` in
  `R/diagnostics.R` calls `raddr::addr_whatwg()` and
  `raddr::addr_embedded_kind()` to emit `ipv6-embedded-ipv4`.
- D1 still holds: `rurl` holds no address ranges. The second IPv6 token,
  `ipv6-non-canonical`, compares the literal with `rurl`'s existing WHATWG
  IPv6 serializer. That serializer is URL-host-layer code that predates this
  amendment, and no new address code was written for it.
- D3 still holds for parsing. An adapter that replaces `R/parse-web.R`'s
  host parse still waits on the open question.

The token definitions and their clauses are ruling RUL-025 in
`design/work/url-v3/registers/rulings.md`.

## Amendment: the open question is closed, and D3 is replaced

*Added RURL-cbrfphfr, 2026-10-09. Appended rather than edited in place so no
line citation into this file moves. The open question, D3 and the 2026-10-07
amendment's last bullet above stay as written; this section supersedes them.*

**Owner ruling, 2026-10-08: no.** `raddr` does not grow a URL-host-layer curl
dialect. The open question is closed. `R/parse-web.R` therefore keeps its
libcurl-style ("narrow") IPv4 reading for good: it is the reading for
`url_standard = NULL` and `"rfc3986"`, and no `raddr` function replaces it.
The counter-evidence in §Consequences still binds. `raddr::addr_curl()` is a
resolver-layer reading, and no adapter is ever built on it.

**D3, replaced.** The old D3 held back IP work "until `raddr` ships a
URL-host-layer curl dialect". Under the ruling above that wait would never end,
so the clause is replaced by this one:

> **D3 — new address code goes to `raddr`, never into `rurl`.** `rurl` may
> adapt one of `raddr`'s dialect readings at the URL layer where that reading
> is the standard's own. Under `url_standard = "whatwg"` the WHATWG IPv4 host
> reading is `raddr::addr_whatwg()`, called through an adapter in
> `R/parse-web.R`. `rurl` keeps everything around the reading that is URL
> layer and not address layer: the host/address gate
> (`.host_ends_in_number_vec()`, D2), tab and newline stripping,
> percent-decoding, bracket syntax, URL failure semantics, the narrow reading
> for `NULL` and `"rfc3986"`, and its own WHATWG IPv6 serializer. Requests for
> in-tree IPv6 classification are still answered by projecting `raddr`'s
> facts, never by new address code here.

What the adapter may and may not do:

- It is called only for a host that the ends-in-a-number gate has already
  sent to the IPv4 parser, and only under `whatwg`. `raddr` has no reg-name
  concept (D2), so the gate is what keeps `0xg`, `0x1p`, `.` and `..` names.
- An `addr_whatwg()` rejection at that point is a WHATWG IPv4 failure, which
  fails the URL, exactly as the in-tree reading it replaces did. An answer
  that is not an IPv4 address (`addr_whatwg()` also reads IPv6) is a
  rejection too.
- The in-tree WHATWG IPv4 number parser is deleted, not kept as a fallback or
  as a test oracle. The tests pin the standard's expected output
  (`tests/testthat/test-url-standard-whatwg-ipv4.R`).

The gain is single ownership of the WHATWG IPv4 reading, not a large deletion.
At `main` `da6becd` the in-tree WHATWG reading and `addr_whatwg()` agreed on
all 52 hosts that pass `rurl`'s ends-in-a-number gate, which is the corpus
§Consequences asks for: it contains the reg-names the gate keeps out.

The diagnostics in `R/diagnostics.R` keep their own per-part reader
(`.parse_ipv4_part_value()`). It computes shape facts identically under
`whatwg` and `rfc3986`, including an out-of-range value that `addr_whatwg()`
only reports as a rejection, so it is not the address reading this
amendment moves.
