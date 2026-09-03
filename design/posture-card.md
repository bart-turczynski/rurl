# Posture card — read before touching parse behavior or a conformance test

One page. Every recipe below was read from `R/parse.R` (`.URL_PROFILES`,
`.URL_STANDARD_PROFILES`) on 2026-09-03; when a value here disagrees with the
tree, the tree wins and this card is the file to fix.

## Three questions, in this order

1. **Which standard?** `url_standard = "whatwg"` | `"rfc3986"` | `NULL`.
   `NULL` is **not** "unspecified": it is the byte-frozen pre-selector profile
   ([ADR 0007](adr/0007-url-standard-selector.md)). It names no standard, so
   it can never be the posture for a conformance claim. On `get_host_type()`,
   `get_url_diagnostics()` and `get_scheme_class()` it is an error, not a mode
   ([ADR 0015](adr/0015-require-url-standard-on-companion-helpers.md)).
2. **Which acceptance?** `scheme_policy`: `"infer"` (omnibox affordance, the
   2.x default) vs `"require"` (what both standards' parsers actually do; the
   3.0 default, [ADR 0010](adr/0010-scheme-policy-acceptance-axis.md)).
   `scheme_acceptance`: `"web"` (five-scheme allowlist, ADR 0004) vs
   `"general"` (any scheme token; **errors under `url_standard = NULL`**,
   `R/parse.R` `.check_url_standard_conflicts` region).
3. **Which substrate?** A conformance claim is made on `serialize_url()`.
   Identity is made on `safe_parse_urls()` components or `get_url_key()`.
   **`clean_url` is never a conformance oracle** — it is a lossy policy
   projection ([ADR 0017](adr/0017-clean-url-is-a-lossy-policy-projection.md)).

## The recipes (copy them; do not paraphrase)

| Posture | Exact arguments | Serialize with |
|---|---|---|
| **WHATWG** | `profile = "whatwg"` — equals `url_standard = "whatwg", scheme_policy = "require", scheme_acceptance = "general", scheme_relative_handling = "error"` | `serialize_url(x, standard = "whatwg")` |
| **RFC 3986, normalizing** | `url_standard = "rfc3986", scheme_policy = "require", scheme_acceptance = "general"` (governed: `path_normalization = "dot_segments"`, `case_handling = "lower_host"`) | `serialize_url(x, standard = "rfc3986", form = "normalized")` |
| **RFC 3986, syntax only** | `profile = "rfc-syntax"` — `url_standard = "rfc3986"` with `path_normalization = "none"`, `case_handling = "keep"`, `scheme_relative_handling = "keep"` | `serialize_url(x, standard = "rfc3986", form = "source")` |
| **Legacy frozen** | `url_standard = NULL, scheme_policy = "infer", scheme_acceptance = "web"` | none — `serialize_url(standard = NULL)` is an error (RURL-ouorolhb): the frozen profile names no standard to serialize *as* |

`Rscript tools/posture-probe.R <url>` prints all four side by side with the
oracle's expected value where a fixture row exists. `url_profile("whatwg")`
prints exactly what a profile bundles.

## Where posture gets lost (each one has cost a ticket)

- **The default differs by function.** `safe_parse_url(s)`, `get_clean_url`,
  `get_parse_verdicts`, `resolve_url` default to `NULL`; the three companion
  helpers require it; `check_hosts`/`is_valid_host` default to `"whatwg"`;
  `get_mailto_recipients` defaults to `"rfc3986"`. Name the posture even when
  it equals the default.
- **The axis is spelled two ways.** `serialize_url()` and `url_key_policy()`
  take `standard = c("whatwg", "rfc3986")`, default `"whatwg"`, and both
  reject `NULL` (`serialize_url()` since RURL-ouorolhb) — the opposite of the
  parse surface, where `NULL` is a value.
- **A profile is not its `url_standard`.** `profile = "whatwg"` sets
  `scheme_policy = "require"` and rejects scheme-less input that
  `url_standard = "whatwg"` alone accepts. `profile = "rfc-syntax"` turns
  normalization off; `url_standard = "rfc3986"` turns it on.
- **Ungoverned is not selector-independent.** `port_handling` no-ops outside
  WHATWG; `host_encoding` decodes only under `rfc3986`; `path_encoding`
  switches encoder under WHATWG. [ADR 0016](adr/0016-null-freeze-binds-selector-caused-drift.md)
  exists because a test comment assumed otherwise.
- **The frame vs the payload.** A test that varies the input while holding
  `url_standard` fixed cannot reach code gated on the other arm. When a
  behavior is claimed invariant across arms, loop the selector.

## Which layer does the rule belong to?

Before fixing, name the layer; three tickets were reverted for mixing them.

| Layer | Example | Belongs in |
|---|---|---|
| Generic syntax | RFC 3986 §3 grammar, §6.2.2 normalization | the `rfc3986` arm |
| WHATWG parser | host parser, UTS-46, backslash, special schemes | the `whatwg` arm |
| Scheme overlay | RFC 8089 `file:` no-port, RFC 9110 non-empty host | a reportable fact / `web` acceptance, never the generic grammar |
| Transport | 16-bit port ceiling | never a syntax gate under `rfc3986` |
| Presentation | `host_encoding`, `path_encoding`, `format_url` | an orthogonal dial (ADR 0011), never the parse record |

## Definition of done for a conformance change

- The new test is shown **red at the pre-fix baseline** under the named posture.
- The named suite passes: `testthat::test_local(filter = "<suite>")` — WPT:
  `wpt-full-suite`, `wpt-base-relative`; RFC 3986: `rfc3986-grammar`,
  `rfc3986-appendix-b`, `url-standard-conformance`; vectors:
  `external-url-vectors`.
- Oracle fixture edits touch only the columns scoped to the posture
  ([oracle-fixtures.md](oracle-fixtures.md)); a flipped row re-pins the sha256
  in **both** `oracle-provenance.json` and the register row.
- A `NEWS.md` bullet cites the standard and the clause.
- `Rscript tools/verify.R` reports `0 failed`.
- No ADR, decision record or worklog unless the change flips a contract claim
  or changes a user-visible default with no governing ADR.
