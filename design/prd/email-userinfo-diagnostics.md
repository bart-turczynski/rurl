# PRD: email / userinfo diagnostic vocabulary

- **Status:** Accepted as a design note (graduated per ADR 0008) — records the
  agreed email/userinfo fact vocabulary. **Implementation-ready as of
  2026-07-11:** the public result shape is resolved (RURL-hxmdzxkd) — see
  *Public result shape (RESOLVED)* below. The `indeterminate` value is still
  carried on every per-recipient enum; implementation is spun out as a follow-up
  ticket.
- **Date:** 2026-07-10 (result-shape decision recorded 2026-07-11)
- **Spun out of:** ADR 0012 (general URL parser) — this was §D7 + the email parts
  of Appendix A.2/A.4 in the full working draft. Split out so ADR 0012 is
  reviewable on its own; this thread is a **non-blocking follow-up slice** (does
  not gate ADR 0012 Layers 1–6).
- **Tracking:** RURL-pbbkzccp (parent scope). New fp issue TBD.
- **Hard constraints inherited from ADR 0012 / repo ADRs:** companion-helper only
  (ADR 0006 — no new `safe_parse_url` columns); facts, not gates (never rejects a
  parse); each fact **names the grammar and projection** it was judged against.

---

## Problem

rurl **already does non-standard email-shaped handling**: scheme-less
`user@example.com` is `warning-userinfo` (`.STATUS_WARN_USERINFO`) — components
resolve, `clean_url` suppressed to `NA`. In that inferred URI authority the text
before `@` is URI `userinfo`; in an RFC 6068 `mailto:` positional `addr-spec` it
is an email `local-part`. The user ask — *let users learn whether the thing
before `@something-else.com` is a valid potential local part* — is a separate
email-candidate projection on the scheme-less source, not a relabeling of the
URI parse.

The original ADR proposal conflated layers. This PRD specifies the corrected
model: **separate vocabularies, facts-not-gates, companion-helper only.**

## Non-goals

- No full RFC 5322 comment / folding-whitespace / address-list parser.
- No DNS-resolution verdict, no deliverability check.
- No new `safe_parse_url` columns; everything lands in `get_url_diagnostics()`.
- No rejection: an email-shaped fact never turns a parse into an `error`.

---

## Model (corrected — separate vocabularies)

- A `mailto:` positional `to` portion is an **optional comma-separated list of
  `addr-spec`s**, not a single address; recipients may also appear in
  `to`/`cc`/`bcc` **hfields** (RFC 6068 §2). Avoid calling this opaque path the
  "mailto body": `body` is RFC 6068's message-body pseudo-hfield.
- **Provenance-preserving tokenize-then-decode-then-classify.** Decoding the
  whole string first would turn `%2C`, `%3D`, and `%26` data into delimiters.
  Blindly splitting every raw comma is also incomplete: RFC 6068 imports RFC 5322
  `quoted-string`, whose `qtext` admits comma, and its liberal bracketed-domain
  grammar admits comma and raw `@`; the quote/bracket markers themselves can be
  percent-encoded (`mailto:%22a,b%22@example.org`, `mailto:user@%5Ba,b%5D`). The
  contract is:
  1. Split the raw `?` to/hfields boundary and raw hfield `&`/`=` delimiters
     before decoding. Preserve every token's raw-vs-percent-encoded provenance.
  2. Lex the positional `to` using RFC-6068-aware quote, quoted-pair, and
     bracketed-domain context. A `%2C` is never a separator; a raw comma
     separates recipients only outside those contexts. Locate the addr-spec `@`
     with the same grammar — not first/last-raw-`@`.
  3. Percent-decode each resulting field / `addr-spec` **exactly once**, then
     classify the decoded token against the named email grammar.
  If this lexer is not implemented, affected positional recipients are
  `indeterminate`; the parser must not invent recipients or emit a false verdict.
- **Per-recipient diagnostics are limited to positional `to`.** RFC 5322
  `To`/`Cc` field values use `address-list`; `Bcc` is `[address-list / CFWS]` and
  can be empty (these Destination Address Field productions are RFC 5322 **§3.6.3**;
  `address-list` itself is §3.4). Extracting recipients from these hfields needs a
  full RFC 5322 address-list parser — **out of scope**. Without one, report only
  hfield presence / syntax-indeterminate facts — never "recipients present" from a
  nonempty or malformed `Bcc` value.
- **Every fact names its grammar and projection.** In a decoded RFC 6068
  positional `addr-spec`, the left side is `local-part`; in inferred `user@host`,
  it is URI `userinfo`. The same scheme-less source may separately be projected
  as an SMTP or RFC 5322 email candidate. RFC 6068, RFC 5322, and SMTP RFC 5321
  have distinct local-part and right-hand-side grammars, so no unqualified
  `email-local-part-form` / `email-domain-form` spans them.
- Length limits are **SMTP transport** limits (RFC 5321 §4.5.3.1), **not** RFC
  5322 or `mailto` syntax limits → distinct `smtp-*` facts. RFC 5321 specifies a
  **64-octet local-part** and a **256-octet forward/reverse path**; the commonly
  cited **254** is the *derived* practical maximum for a bare mailbox after the
  path brackets (RFC 3696 EID 1690), not a standalone production. These octet
  facts require an actual serialized wire projection; counting decoded Unicode
  characters or a bare unprojected mailbox is invalid.
- **RFC 6531 (SMTPUTF8)** is an **envelope-address projection tied to wire
  serialization**, not `mailto` conformance: a **non-ASCII local-part** makes
  *that address* "internationalized" (definition: **RFC 6530 §4.2**, the
  framework 6531 imports), and a **U-label domain sent as Unicode** requires
  SMTPUTF8 too (RFC 6531 §3.4 trigger); the requirement is avoided only when the
  Unicode domain is **projected to its ASCII A-label** (RFC 6531 §3.2 carve-out).
  Scoped to a single address (`smtp-envelope-address-requires-smtputf8`);
  deliberately does **not** decide whether a whole message needs SMTPUTF8 (UTF-8
  message headers are an independent trigger).
- A **known PSL suffix is NOT mailbox validity**. Under SMTP the mailbox RHS is
  `Domain / address-literal`; an `address-literal` is not a kind of Domain. RFC
  5321 domains must be fully qualified and resolvable; an arbitrary dotless local
  alias does not become SMTP-valid merely because its characters fit the ABNF.
  RFC 6068's domain grammar is more liberal and is only `mailto` syntax. DNS
  resolution and PSL knowledge remain separate; PSL-known is explicitly
  non-validating.
- If "label" was meant as subaddressing (`user+detail`), RFC 5233 says the
  separator and splitting are **site-specific** — `+` cannot be assumed
  universal. Out of scope beyond noting it.

---

## Public result shape (RESOLVED — 2026-07-11, RURL-hxmdzxkd)

**Decision: a dedicated structured helper for the per-recipient facts, with a
hybrid split of the vocabulary.**

The decisive constraint is that `get_url_diagnostics()` is **not a free-form
token bag**: it is a *closed enum tested by set membership*. Every token it can
emit is a fixed string in `.URL_DIAGNOSTICS` (R/diagnostics.R); `.diag_add()`
rejects any token not on that list; and every downstream consumer (pagerankr,
sitemapr, semantic) gates with `token %in% diags`. No token in the vocabulary
carries a value or an index.

Two shapes were weighed:

- **(a) Indexed flat tokens** — `mailto-local-part-form[1]=…`,
  `mailto-recipient-count=N`, inside the existing helper. Rejected: it injects a
  `key[i]=value` micro-syntax into an otherwise bare-enum vocabulary, breaks the
  closed-enum contract `.diag_add()` enforces, and forces every membership-based
  consumer to string-parse tokens. This is what the earlier draft flagged as
  "awkward to consume programmatically," understated.
- **(b) Dedicated structured helper** — **chosen.** The per-recipient facts are
  inherently 2-D (URL × recipient × fact); that shape belongs in a data frame,
  not a membership set.

**Hybrid split (the resolved contract):**

- **URL-level (whole-URL) facts stay as flat enum tokens in
  `get_url_diagnostics()`** — they fit the existing idiom: `userinfo-form`,
  `public-suffix-known`, and the whole-URL `smtp-envelope-wire-mode`. These are
  added to `.URL_DIAGNOSTICS` as ordinary closed-enum tokens.
- **Per-recipient facts go to a new companion helper `get_mailto_recipients()`**
  returning a `data.frame` keyed by URL and recipient index (one row per
  positional-`to` `addr-spec`), one column per per-recipient enum
  (`mailto_local_part_form`, `mailto_domain_form`, `smtp_mailbox_rhs_syntax_form`,
  `smtp_domain_wire_form`, `smtp_local_part_length_ok`,
  `smtp_direct_forward_path_fits`, `smtp_envelope_address_requires_smtputf8`).
  Still companion-only (ADR 0006) — **no `safe_parse_url` / `safe_parse_urls`
  columns.** The token-vocabulary contract of `get_url_diagnostics()` is left
  byte-for-byte unchanged for its existing tokens.

**Defined edge cases (mandatory for the implementation ticket):**

- **Empty positional `to`** (`mailto:?subject=x`, or a bare `mailto:`): zero
  rows for that URL from `get_mailto_recipients()`.
- **All-`indeterminate`** (lexer not implemented / could not resolve): one row
  per recipient the lexer *could* delimit with every per-recipient enum set to
  `indeterminate`; if even the recipient count is unresolvable, zero rows plus a
  URL-level indeterminacy fact. The parser never invents a recipient or emits a
  false verdict (see the lexer contract).
- **Not a `mailto:` URL** (e.g. an inferred scheme-less `user@host`): zero rows;
  the `userinfo-form` / `smtp-*` candidate facts for that source surface as
  URL-level tokens in `get_url_diagnostics()`, not in the recipients frame.

## Proposed diagnostic vocabulary

Companion facts, no new columns, no rejects; **per-recipient for the positional
`to` only** (hfield address-lists are RFC 5322, out of scope); each names the
grammar it was judged against. Every **per-recipient** form carries an
`indeterminate` value for recipients the lexer could not resolve (see the lexer
contract) — the parser never invents a recipient or emits a false verdict.

**Routing (per the resolved result shape above):** facts tagged
*per-recipient* below become columns of `get_mailto_recipients()` (one row per
positional-`to` `addr-spec`); the *URL-level* facts (`userinfo-form`,
`smtp-envelope-wire-mode`, `public-suffix-known`) become closed-enum tokens in
`get_url_diagnostics()`. Column names in the recipients frame are the
snake_case forms of the kebab token names given here.

- `mailto-local-part-form = dot-atom-text | quoted-string | invalid |
  indeterminate` (RFC 6068, post-single-percent-decode; per-recipient)
- `userinfo-form` for the actual scheme-less inferred URI authority, plus an
  independent `smtp-local-part-form` for the same source tested as an email
  candidate. (An explicitly named `rfc5322-addr-spec-form` was considered and
  **dropped from the first slice** — see Open questions.)
- `mailto-domain-form = ascii-dot-atom-text | idna2008-domain | bracketed-domain
  | invalid | indeterminate` (RFC 6068 §2; per-recipient). `bracketed-domain` is
  mailto vocabulary here, not SMTP `address-literal`; a non-ASCII label is a
  U-label only after IDNA2008 validation, never merely because it is Unicode
- `smtp-mailbox-rhs-syntax-form = domain | address-literal | invalid |
  indeterminate` (per-recipient), explicitly separate from DNS resolvability and
  from the RFC 6068 domain grammar. Note `indeterminate` (lexer could not resolve
  the recipient) is distinct from the `unavailable`/`unknown` values below (a
  recipient resolved, but no wire projection could be made)
- `smtp-domain-wire-form = ascii-domain | a-label-domain | u-label-domain |
  address-literal | unavailable` plus `smtp-envelope-wire-mode = ascii | smtputf8
  | unavailable`; SMTPUTF8 can be required solely by a non-ASCII local-part even
  when the domain uses an A-label. Length and SMTPUTF8 facts are `unknown` when a
  wire projection cannot be made
- `smtp-local-part-length-ok`: compare the **serialized** local-part with the
  64-octet limit; `smtp-direct-forward-path-fits`: evaluate
  `octet_length("<" + serialized Mailbox + ">") <= 256`. The familiar 254-octet
  value is only the RFC 3696 EID 1690 derivation for that direct path
- `smtp-envelope-address-requires-smtputf8` (renamed from `smtputf8-required`): an
  SMTP-envelope address projection, tied to wire serialization; does **not**
  determine whether an entire *message* needs SMTPUTF8
- `public-suffix-known` (separate, non-validating)

Scope: **structural facts only** — no full RFC 5322 comment/folding-whitespace or
address-list parser, no DNS-resolution verdict, no deliverability.

---

## Standards grounding (email/SMTP)

- Three *distinct* local-part grammars: RFC 6068 §2 `dot-atom-text /
  quoted-string` on the percent-decoded `addr-spec` (obs excluded); RFC 5322
  §3.4.1 `dot-atom` (allowing surrounding CFWS) `/ quoted-string` plus
  `obs-local-part`; SMTP RFC 5321 §4.1.2 `Dot-string / Quoted-string` with
  stricter domains and specific address-literal forms.
- Transport limits (RFC 5321 §4.5.3.1): local-part ≤ 64 octets; forward/reverse
  **path** ≤ 256 octets; the widely cited **254** is the RFC 3696 EID 1690
  derivation for a bare mailbox after the path brackets — SMTP limits, not syntax
  limits, measured on the serialized wire projection.
- SMTPUTF8 (RFC 6531): §3.4 operational trigger; the definition that a non-ASCII
  local-part internationalizes a mailbox is **RFC 6530 §4.2**; RFC 6531 §3.2
  carves out the IDNA **A-label** domain form.
- RFC 5322 `To`/`Cc` bodies use `address-list`; `Bcc` is `[address-list / CFWS]`
  (can be empty) — these header-field productions are **§3.6.3 (Destination
  Address Fields)**, whereas `address-list`/`local-part` are §3.4/§3.4.1.
- RFC 6068's `addr-spec` is RFC 5322's `addr-spec` with `<comment>`/CFWS excluded
  (beyond the obs-form exclusion). RFC 5233 subaddressing (`user+detail`) is
  site-specific; `+` not universal.
- As of 2026-07-10 the emailcore 5321bis/5322bis documents are **not yet RFCs** —
  both cleared IESG review and sit in the RFC Editor queue (post-approval,
  awaiting publication), so RFC 5321/5322 remain the published baseline.

---

## Open questions

- ~~Surface via `get_url_diagnostics` extension vs a dedicated helper?~~
  **Resolved 2026-07-11 (RURL-hxmdzxkd):** hybrid — URL-level facts extend
  `get_url_diagnostics()`; per-recipient facts get a dedicated structured helper
  `get_mailto_recipients()`. See *Public result shape (RESOLVED)*.
- ~~Which facts are default-on vs opt-in, given the wire-projection cost?~~
  **Resolved 2026-07-11 (RURL-hxmdzxkd):** tiered on cost. Pure-syntax facts
  (no serialization) are **default-on**: `mailto_local_part_form`,
  `mailto_domain_form`, `smtp_mailbox_rhs_syntax_form`, `userinfo-form`,
  `public-suffix-known`. Facts requiring a **serialized wire projection** are
  **opt-in** behind a single `smtp_wire = FALSE` flag on
  `get_mailto_recipients()`: `smtp_local_part_length_ok`,
  `smtp_direct_forward_path_fits`, `smtp_domain_wire_form`,
  `smtp_envelope_address_requires_smtputf8` (and the URL-level
  `smtp-envelope-wire-mode`). When `smtp_wire` is not requested those columns
  take the `unavailable`/`unknown` sentinel — the default path never has to make
  a wire-projection decision it could get wrong.
- ~~Naming: settle the `smtp-*` prefix set and the `mailto-*` vs `rfc5322-*`
  split before any of these ships.~~ **Resolved 2026-07-11 (RURL-hxmdzxkd):**
  lock the `mailto-*`, `smtp-*`, and `userinfo-form` names as listed in the
  vocabulary. **Drop `rfc5322-addr-spec-form` from the first slice** — a
  standalone RFC 5322 `addr-spec` fact has no URL projection to anchor it and
  invites exactly the unqualified "email-valid" verdict this PRD forbids. Add it
  only if a downstream consumer asks.
