# ADR 0017: `clean_url` is a lossy policy projection, and the SEO profile drops the whole query

- **Status:** Accepted
- **Date:** 2026-08-25
- **Tracking:** RURL-hcntbqku (this decision). **Supersedes** the "strip tracking
  parameters" clause of [ADR 0012](0012-general-url-parser-scope.md) §D5
  (`canonical`/`seo` bullet, line 528) — that clause only, not the bullet.
  Unblocks RURL-szvncnou (the capability row for the dual-role surface) and
  RURL-otwfjvnf (the bare-dot host under `profile = "seo"`).
- **Depends on / relates to:** ADR 0007 (`url_standard` selector — **untouched**:
  this is a profile-only change and the `NULL` arm does not move), ADR 0011
  (`path_encoding` orthogonality), ADR 0012 (the profile bundles themselves),
  ADR 0016 (the freeze boundary this decision deliberately stays outside of).

All code, test and measurement citations below were taken on `277a639` and are
dated, not live (the citation convention in ADR 0014's wake): they record what
was true when the ruling was made.

## Context

`clean_url` is rurl's most-used output and the one non-experts reach for first.
Until now, what it *is* had never been written down anywhere the package could
be judged against — only what its dials do, one dial at a time.

The owner stated a definition on 2026-08-03: a clean URL is the URL a working
SEO expects to see — human-readable, no cruft, safe to show a person — and
concretely "a WHATWG-parsed URL plus visual tweaks, never a separate, weaker
construction of its own", followed by eight items. Items 1, 2 and 6 (WHATWG
base, folders resolve, Unicode host) shipped into the `seo` bundle on 2026-08-03
(MR !14, `28f85fc`). Items 3, 4 and 8 were never gaps. Item 5 was resolved as
satisfied-as-written: force-https is the SEO rule, and "browser-like" protocol
fix-up is the separate `browser` profile's rule (ADR 0012:525-529; browser-fixer
PRD:213-221).

**Item 7 — "parameters are gone" — was the only survivor, and it was a
contradiction rather than an omission.** The definition asks for the whole query
to go. ADR 0012:528 specifies the SEO profile as stripping *tracking* parameters,
which is what shipped: `query_handling = "filter"`. And
[`cleaning-mutation-contracts.md`](../work/url-v3/contracts/cleaning-mutation-contracts.md)
rows 13-19 are SETTLED and rule out a destructive trim outright — the query dials
are filtering, sorting and encoding only, because "the query is an ordered
sequence, not a map". So the definition could not be delivered by editing a
bundle; something normative had to give.

Two questions had to be answered, and only the first is architectural.

### Q1 — is the definition a product spec or a contract?

Either the `seo` bundle *is* the definition (the eight items were illustrative,
and anything the bundle does is by construction clean), or the definition exists
independently and the bundle is judged against it.

The cheapest test is that **the bundle does three things the eight items never
mention**: `www_handling = "strip"`, `trailing_slash_handling = "strip"` and
`index_page_handling = "strip"`. Under a strict contract reading with a closed
list, all three are unauthorized and would have to be removed — which would gut
the profile and contradict ADR 0012:527-528, where strip-www and the
trailing-slash and index policies are named as SEO transforms. That reductio
settles it: the eight items are demonstrably **not a closed authorization list**.
The failure they expose is that the definition is *under-written*, not that the
bundle is over-reaching.

### Q2 — what should `query_handling` be under `profile = "seo"`?

Measured on `277a639`:

```r
get_clean_url("https://example.com/p?utm_source=nl&id=7&ref=x")
#> "https://example.com/p"                    # no profile: the query is gone
get_clean_url("https://example.com/p?utm_source=nl&id=7&ref=x", profile = "seo")
#> "https://example.com/p?id=7&ref=x"         # the SEO preset RETAINS it

get_clean_url("https://example.com/p?a=1&b=2", profile = "seo")
#> "https://example.com/p?a=1&b=2"            # no trackers present: untouched
```

Asking for the SEO preset yields **strictly less parameter cleaning than asking
for nothing**. That is an inconsistency between two of rurl's own defaults, and
it needs no agreement about what "clean" means to be a defect.

Two further facts belong in the record:

1. **`filter` reads as a category and behaves as a list.** It is name-matched
   against a deliberately small built-in denylist (`R/query-denylist.R`) plus the
   caller's `params_drop`. So `?utm=x` survives while `?utm_source=nl` does not —
   measured, and visible in this ADR's own worked example. "Removes tracking
   parameters" is therefore not a promise the implementation can keep; it removes
   *names on a list*.
2. **The bite is entirely non-trackers.** Over the 482-row union of the five
   fixture corpora, moving `seo` from `filter` to `drop` changes **25 rows**, and
   **all 25** lose a parameter that is not on the denylist. On this corpus the
   filter's only observable effect under the SEO profile is retaining ordinary
   parameters — the trackers were already gone either way.

## Decision

**D1 — the definition is a CONTRACT, and it is restated to be one.** A clean URL
is a **lossy policy projection of a WHATWG-parsed URL**, not a visually adjusted
one: forcing https, dropping the port, dropping the query and stripping `www`
can each change the addressed resource, and calling them "visual tweaks"
understates them. The surface makes **no claim of resource equivalence, no
security claim, and no HTML-canonical claim**. "Safe to show a person" means
structurally valid and display-oriented; it does not mean trustworthy or
navigation-equivalent (a Unicode host is more readable *and* more confusable).

**D2 — the contract's mutation list is exhaustive, and it is this table.** A
transform not on it is not part of the definition; adding one is an amendment to
this ADR, not a bundle edit.

| # | Mutation | Dial under `profile = "seo"` |
|---|---|---|
| 1 | WHATWG parse underneath (which also resolves `.`/`..` segments) | `url_standard = "whatwg"` |
| 2 | HTTP(S)-family input; other admitted schemes serialize but receive no SEO transforms | `scheme_acceptance = "web"` |
| 3 | the protocol is forced to https, and remains caller-pickable | `protocol_handling = "https"` |
| 4 | `www.` comes off the host | `www_handling = "strip"` |
| 5 | a trailing slash comes off | `trailing_slash_handling = "strip"` |
| 6 | a terminal index page comes off | `index_page_handling = "strip"` |
| 7 | the host renders in Unicode regardless of the input spelling | `host_encoding = "unicode"` |
| 8 | **the whole query is dropped** | `query_handling = "drop"` |
| 9 | the **default** port is dropped and a non-default one kept; default-ness is judged on the parsed scheme, never on the `https` row 3 renders (amended 2026-09-04, RUL-016) | `port_handling = "strip_default"` |
| 10 | the host case folds | `case_handling = "lower_host"` (surface default) |
| 11 | no fragment is ever carried | structural — `clean_url` has no fragment |
| 12 | no userinfo (credentials) is carried | structural today — `clean_url` has no userinfo; RUL-001 adds `credential_handling = c("strip", "reject")`, default `"strip"`, so a caller may ask for `NA` instead of a silently collapsed URL |

**Amendment 2026-09-03 (RUL-001, `design/work/url-v3/registers/rulings.md`).** Row 12 records a mutation that had shipped since the surface existed but was missing from this table: userinfo is always dropped, and the roxygen for `get_clean_url()` has said so throughout. It is added under D2's own rule that a transform not on the table is an amendment to this ADR.

**Amendment 2026-09-04 (RUL-016, `design/work/url-v3/registers/rulings.md`).** Row 9 was inherited from the surface default `port_handling = "exclude"` rather than decided, and it folded non-default origins: measured on `main` that day, `http://example.com:8080/a` under `profile = "seo"` cleaned to `https://example.com/a`. RFC 3986 §6.2.3 and the WHATWG URL Standard's port state sanction dropping a *default* port (it is equivalent to no port); nothing sanctions dropping a non-default one, which names a different origin (RFC 6454 §4). The bundle now pins `port_handling = "strip_default"`, and — the same ruling's second half — default-ness is judged on the scheme the input was parsed with, not on the `https` row 3 renders, so `http://example.com:443/a` keeps `:443` and `http://example.com:80/a` drops `:80`. The surface default stays `"exclude"`, byte-identical.

Rows 4, 5 and 6 are the three the eight items omitted; recording them here is
what closes Q1. **Item 8 of the owner's list — "users can define what a clean URL
means for their site" — is API policy, not a mutation**, and is deliberately not
in this table. It is restated as: an explicitly passed dial always beats the
profile (the iron rule), so any caller may define their own projection.

**D3 — `profile = "seo"` sets `query_handling = "drop"`.** This supersedes ADR
0012:528's "strip tracking parameters" for the SEO bundle only. `filter` survives
untouched as an explicitly selectable policy for callers who want tracker removal
while preserving semantic parameters — a dial does not have to be the preset to
earn its place.

**D4 — no destructive trim is introduced.** `drop` is an existing
`query_handling` value operating on the parsed query, so
`cleaning-mutation-contracts.md` rows 13-19 stand unamended: the query is still
an ordered sequence, and rurl still never trims raw bytes after `?`. The
definition's wording "up to and including the destructive trim of everything
after `?`" described an outcome, not a mechanism, and the outcome is reachable
without one.

## Consequences

- **Callers who pass `profile = "seo"` get different output.** 25 of 482 fixture
  rows move. Anyone wanting the shipped behavior passes `query_handling =
  "filter"` explicitly — one argument, and the iron rule guarantees it wins.
- **`url_standard = NULL` does not move.** The change lives entirely inside a
  profile bundle, so ADR 0007's byte-for-byte freeze is untouched and ADR 0016's
  boundary is not engaged. Re-measured after the change: 0 changed rows on the
  no-profile arm.
- **The built-in denylist loses its last default consumer.** Nothing in the
  package then defaults to `filter` — `get_clean_url` defaults to `drop`,
  `format_url` to `keep`. The denylist and its pluggable source registry become
  opt-in machinery only. That is accepted, not overlooked: it stays because
  `filter` stays.
- **`canonical_join`'s legacy dial seam converges on its own default.**
  `clean_url` doubles as the legacy join key, so this changes which rows match
  when `profile` is forwarded through `...`. The direction is toward the shipped
  default, not away from it — measured on `277a639`, `A = /p?id=7` and
  `B = /p?id=8` already join under no profile (key `https://example.com/p`), and
  today's `profile = "seo"` is the only thing that prevents it. The seam already
  warns (`rurl_legacy_join_dial_warning`, P3.1 D-E).
- **The `canonical` alias overpromises and is documented, not renamed.** These
  transforms are policyful and need not preserve semantic equivalence — the
  concern ADR 0012's Open Q1 recorded when `seo` was made primary. Under the
  3.0.0 freeze the remedy is the disclaimer in D1, carried into the alias's
  documentation.

## Alternatives rejected

- **Keep `filter`, amend the definition instead.** This was the cheaper option
  when the version could still move. It cannot now: 3.0.0 is frozen, so the
  anomaly would ship as final behavior, and the package would permanently answer
  "what is a clean URL?" with a rule its own bare default disagrees with.
- **Remove `www_handling = "strip"` as unauthorized.** The strict-closed-list
  reading of Q1, and the remedy an external review recommended when shown
  `www_handling` alone. It fails on the two dials that review was not shown:
  applied consistently it also deletes the trailing-slash and index-page
  policies, which ADR 0012 names explicitly. The list was under-written; the
  bundle was not over-reaching.
- **`allow` or `keep` for the SEO bundle.** More honest than a small denylist
  *if* preservation were the governing principle. It is not: this surface is
  declared lossy in D1 and item 7 chose removal.
- **A smarter parameter classifier.** Deciding which parameters are semantically
  disposable requires site knowledge that URL syntax does not carry. That is what
  `params_keep` / `params_drop` are for, and why item 8 exists.
