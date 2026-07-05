# ADR 0006: Diagnostics/metadata are companion helpers, never widened parse columns

- **Status:** Accepted
- **Date:** 2026 (url_standard v1)
- **Tracking:** RURL-csdrxdoj; PRD `url_standard` §6.3

## Context

The `url_standard` work introduced descriptive metadata about a URL that is not
part of its normalized form: `host_type` (ipv4 / reg-name / …), `scheme_class`
(WHATWG special vs not), and a diagnostics vocabulary (`ipv4-*`, port tokens,
`invalid-reverse-solidus`, DNS/UTS-46 tokens). The tempting move — adding these
as columns on `safe_parse_urls()` / fields on `safe_parse_url()` — would widen
the stable result shape that `canonical_join()` and every downstream consumer
depend on, and would force every parse to compute metadata most callers never
ask for.

## Decision

Surface metadata **only** through dedicated companion helpers —
`get_host_type()`, `get_scheme_class()`, `get_url_diagnostics()` — gated on an
explicit `url_standard` argument (NULL → `NA`, opt-in). Never widen the parse
result columns/fields.

## Consequences

- The parse result shape is stable; adding a new diagnostic never changes it.
  Each `url_standard` test file carries an "adds no columns/fields" guard test —
  copy that pattern for any new diagnostic axis.
- **Facts-not-policy:** a diagnostic token describing an input *shape* fact
  (`ipv4-*`, port tokens, DNS/UTS-46 tokens) fires identically under both
  `rfc3986` and `whatwg`; only `host_type` itself and actual rendering/elision
  behavior differ per standard.
- Metadata computation is opt-in and off the default parse path, so its cost
  (e.g. the DNS/UTS-46 probe) cannot regress the common case.
