<!--
Graduated from _scratch/PRD-host-validation-policy.md on 2026-07-11 per ADR
0008. This is the durable, accepted spec for the practical host-validation
policy helpers (`is_valid_host()` / `check_hosts()`), shipped in rurl 2.6.0
(RURL-clsmspfb, PR #164). It is a point-in-time document: where it and the code
disagree, the code and the relevant ADRs (0004, 0006, 0012) win. Companion
working notes remain in the working-tree _scratch/.
-->

# PRD — practical host validation policy helpers (web / DNS / registrable / SEO)

**Status:** Accepted and shipped (rurl 2.6.0, PR #164) — RURL-clsmspfb
**Parent epic:** RURL-kdzwamop (URL standards scope, parity, intent profiles)
**Depends on / relates to:** ADR 0006 (diagnostics are companion helpers only —
hard constraint), ADR 0004 (strict host-shape gate — the reason several
"malformed" hosts already fail registrability), ADR 0012 D2/D5 ("selected
diagnostics, never a conformance oracle"; DNS/PSL eligibility matrix), the
`url_standard` selector (ADR 0007), `get_host_type` / `get_url_diagnostics` /
`get_scheme_class` companion helpers.
**Author:** design session 2026-07-11
**Target release:** a minor (no default flip; purely additive helpers)

---

## 1. Why

The parity epic (RURL-lyhcyvsa) fixed rurl's rule: a selected `url_standard`
profile should match the URL *standard*, not impose practical web/SEO hygiene.
Being stricter than the standard is a parity miss. So `a+b.example` (valid RFC
3986 reg-name), `_dmarc.example.com` (valid DNS owner name), and `-example.com`
(a parseable URL host) all *parse* — they are not parse errors.

That leaves a real product need: developers and SEOs want to ask, of a
standards-correctly parsed host, **"is this usable as a practical web hostname /
DNS owner name / registrable site host / SEO-safe host?"** — a *policy* verdict,
not a parse error, and never a new column on `safe_parse_url` (ADR 0006).

## 2. What the existing facts already give us (grounding, verified 2026-07-11)

Probed via `devtools::load_all()` against the shipped 2.6.0 code. Every fact
below is already available through a companion helper or accessor; this layer
composes them, it does not re-parse.

| Input (`http://…/`)  | `host_type` | `get_domain` | host-hygiene diagnostics |
| --- | --- | --- | --- |
| `a+b.example`         | reg-name | NA | `domain-std3-violation` |
| `_dmarc.example.com`  | reg-name | NA | `domain-std3-violation` |
| `a_b.example.com`     | reg-name | NA | `domain-std3-violation` |
| `-example.com`        | reg-name | NA | `domain-hyphen-violation` |
| `example-.com`        | reg-name | NA | `domain-hyphen-violation` |
| `a..com`              | reg-name | NA | `domain-empty-label` |
| `verylong…64.com`     | reg-name | NA | `domain-label-too-long` |
| `localhost`           | reg-name | NA | *(none)* |
| `example`             | reg-name | NA | *(none)* |
| `münchen.de`          | domain   | münchen.de | *(none)* |
| `xn--nxasmq6b.example.com` | domain | example.com | *(none)* |
| `example.com.` (FQDN) | domain   | example.com. | *(none)* |
| `[::1]`               | ipv6     | NA | *(none)* |
| `192.168.0.1`         | ipv4     | NA | *(none)* |
| `2130706433` (whatwg) | ipv4     | NA | `ipv4-number-form,ipv4-non-dotted` |
| `0x7f.1` (whatwg)     | ipv4     | NA | `ipv4-short-form,ipv4-non-decimal` |

**The load-bearing finding — the strict host-shape gate (ADR 0004) already does
most of the "registrable" hygiene.** A host that carries a
`domain-hyphen-violation` / `domain-empty-label` / `domain-std3-violation` /
`domain-*-too-long` fact **never** gets a registrable domain: it stays
`host_type == "reg-name"` with `get_domain() == NA`. So in rurl:

> `host_type == "domain"`  ⟺  `get_domain()` is non-NA  ⟺  a clean LDH label
> host with a PSL-known public suffix.

The malformed-but-parseable cases the ticket worries about (`-example.com`,
`a..com`) are therefore *already* non-registrable and *already* fail any
LDH-based check. The one axis the parse facts **cannot** express today is the
**underscore split**: `domain-std3-violation` bundles `_` together with `+`,
so it cannot say "valid DNS owner name (underscore OK) but not a web hostname."
**That split is the genuinely new thing this layer adds.**

## 3. Non-goals

- **Not** a parse error. Practical-hostname failure never changes
  `parse_status`, never fails `safe_parse_url`, never widens its 18 fields
  (ADR 0006).
- **Not** a conformance oracle (ADR 0012 D5). "web-hostname = TRUE" means "no
  practical LDH/length/hygiene footgun rurl checks for," not "provably valid per
  every RFC."
- **Not** DNS resolution, reachability, or WHOIS/registration lookup.
  "registrable" = "has a PSL registrable domain," not "is registered."
- **Not** a new host parser or normalizer — the punycode/PSL seams (ADR
  0001/0002) are untouched; this reads their output.

## 4. Rule taxonomy (the decision)

Five named rules, each a pure predicate over the per-URL facts. Each rule is a
*policy* verdict; the descriptive name is documentation, the short token is the
argument value.

| token | descriptive name | TRUE when… | key examples |
| --- | --- | --- | --- |
| `url` | url-valid | host is present and the selected standard parsed it (`parse_status != "error"`, `host_type != "missing"`) — the loosest rule; anything the standard admits | `a+b.example` ✓, `_dmarc.…` ✓, `-example.com` ✓ |
| `dns` | dns-owner-name | LDH **plus underscore**; every label 1–63 octets, name ≤ 253, no empty interior label, no leading/trailing hyphen | `_dmarc.example.com` ✓, `a_b.…` ✓; `a+b.example` ✗, `-example.com` ✗ |
| `web` | web-hostname | usable HTTP-authority host: an IP literal (ipv4/ipv6), **or** a strict-LDH name host (labels 1–63, no leading/trailing hyphen except an `xn--` ACE label, name ≤ 253, no empty label, **no underscore**) | `münchen.de` ✓, `example.com` ✓, `[::1]` ✓, `192.168.0.1` ✓; `_dmarc.…` ✗ (underscore), `a+b.…` ✗ |
| `registrable` | registrable | `host_type == "domain"` (equivalently `get_domain()` non-NA): a clean host under a PSL public suffix | `example.com` ✓, `münchen.de` ✓; `localhost` ✗, `example` ✗, `192.168.0.1` ✗ |
| `seo` | seo-host | `web` **and** `registrable` **and** carries no host-shape footgun diagnostic (`domain-*`, `ipv4-*`) | `example.com` ✓; `2130706433` ✗ (ipv4 footgun, not registrable anyway), `_dmarc.…` ✗ |

Notes:
- `dns` vs `web` differ **only** by the underscore (and are computed by a
  policy-level char-class regex on the parsed host string, since the shipped
  `domain-std3-violation` fact does not split `_` from other non-LDH bytes).
  This is the one axis the existing facts can't express — the layer's core value.
- Because of the ADR-0004 gate, `registrable ⟹ web` in practice, and `seo` is
  `registrable` plus "no footgun." We keep `seo` as a distinct, self-documenting
  rule (SEO users ask for it by name and it guards the IPv4-shorthand footguns
  that `registrable` alone doesn't), while documenting that it collapses close to
  `registrable` under rurl's strict gate.
- IP literals: `web = TRUE` (a valid authority), `dns = FALSE`,
  `registrable = FALSE`, `seo = FALSE`.

## 5. Public surface (the decision)

Two exported helpers, mirroring rurl's existing idioms
(`get_*` vectors; `query_param_summary` tabular). **No** third
`get_host_diagnostics` — the "why did it fail" reasons ride as a list-column on
the summary, and `get_url_diagnostics` already exists for the raw facts.

### 5.1 `is_valid_host()` — the vectorized predicate

```r
is_valid_host(url,
              rule = c("web", "dns", "registrable", "seo", "url"),
              url_standard = "whatwg",
              scheme_policy = "infer",
              scheme_acceptance = "web")
# -> logical vector, same length as `url`
```

- One `rule` at a time (default `"web"`, the most-asked question). Vectorized,
  `get_*`-style. NA host → NA (not FALSE): "no host to judge."

### 5.2 `check_hosts()` — the tabular workhorse

```r
check_hosts(url,
            rules = c("url", "dns", "web", "registrable", "seo"),
            url_standard = "whatwg",
            scheme_policy = "infer",
            scheme_acceptance = "web")
# -> data.frame: url, host, host_type, <one logical col per rule>, reasons
```

- One row per input URL (input order preserved). `host` = the parsed host
  (`host_encoding = "keep"`), `host_type` = `get_host_type()` value, one logical
  column per requested rule, `reasons` = a list-column of the failing policy/
  diagnostic tokens per URL (`character(0)` when everything requested passed).
- `is_valid_host()` is a thin wrapper: `check_hosts(url, rules = rule)[[rule]]`.

### 5.3 `url_standard` default = `"whatwg"` (the decision)

Unlike the neutral companion helpers (which default `url_standard = NULL` →
all-NA so they never widen the default posture), a *policy* helper that returns
all-NA by default is useless. The practical-web/SEO intent **is** the living web
standard, so these helpers default to `url_standard = "whatwg"`. Callers wanting
RFC-3986 reg-name semantics (numeric host stays a reg-name, no IPv4 coercion)
pass `url_standard = "rfc3986"`. The result depends on the standard (e.g.
`2130706433` is `web = TRUE` ipv4 under whatwg, and a reg-name under rfc3986), so
the chosen standard is documented as part of each helper's contract.

## 6. Acceptance mapping (ticket criteria → this PRD)

1. **Decision recorded** — §4 (taxonomy), §5 (names `is_valid_host` /
   `check_hosts`, data.frame + logical-vector return shapes), §5.3 (standard).
2. **Tests** cover the RFC-reg-name / DNS-owner / web-hostname / registrable /
   SEO distinctions, in both `url_standard` modes.
3. **Examples**: `a+b.example`, `_dmarc.example.com`, `-example.com`,
   `example-.com`, `a..com`, single-label (`localhost`, `example`), IP literals
   (`[::1]`, `192.168.0.1`, `2130706433`), IDNs (`münchen.de`,
   `xn--nxasmq6b.example.com`).
4. **Docs** state plainly: policy layer, not parser conformance; absence of a
   failure is not a conformance claim (ADR 0012 D5).

## 7. Maintainer decisions (RESOLVED 2026-07-11)

All four resolved in favor of the recommendations:

- **Q1 — helper names.** `is_valid_host()` (vector predicate) + `check_hosts()`
  (tabular). No third `get_host_diagnostics`; "why" rides in the `reasons`
  list-column.
- **Q2 — rule vocabulary.** Five tokens: `url` / `dns` / `web` / `registrable` /
  `seo`. `url` (the loosest, "the standard admits this host") is kept as the
  baseline rung of the ladder.
- **Q3 — default `url_standard`.** `"whatwg"`; `"rfc3986"` opt-in.
- **Q4 — `dns` hyphen strictness.** Strict RFC 1035 preferred-name syntax:
  leading/trailing hyphen fails `dns`, so `dns` = `web` + underscore permitted.

## 8. Implementation notes (as built)

- `reasons` is host-fact **evidence** for the row, independent of which `rules`
  were requested: the host-shape diagnostics that fired (`domain-*`, `ipv4-*`,
  `host-charset-shimmed`) plus three policy tokens the raw diagnostics cannot
  express — `ip-literal`, `not-registrable`, `underscore-label` (the last is
  what distinguishes a `dns`-pass / `web`-fail host).
- All rule columns are `NA` for a row with no host to judge (host missing or the
  input did not parse); `is_valid_host()` returns `NA` there, never `FALSE`.
- Char-class checks run on the **idna (ASCII)** host form so a Unicode reg-name
  under a non-public suffix (`müller.example` → `xn--mller-kva.example`) is
  judged LDH-fairly while `_`/`+` stay visible. `registrable` is exactly
  `host_type == "domain"` (the ADR-0004 gate guarantees the equivalence).
