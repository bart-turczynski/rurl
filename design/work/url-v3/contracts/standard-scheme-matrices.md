# Standard / profile / scheme matrices (§6 artifact 5)

<!-- Contract artifact (§6 artifact 5). RCON-05 closure. This record PROJECTS
     the ACCEPTED owner decisions P2.4 (scheme-admission boundary) and P4.1
     (scheme-specialization posture) — the joint disposition of C-11 — plus P1.2
     (authority-state vocabulary, for the authority-emission cell) and frozen
     evidence S6 (schemes/userinfo/email/resolution) into normative expansion,
     precedence, parser-route, eligibility, credential, email, resolution, and
     diagnostics matrices. It makes NO new product decision: every SETTLED cell
     is transcribed from an accepted record; cells those records left as owner
     open questions / deferrals are flagged OPEN, never invented. This is the
     SINGLE WRITER of the v3 standard/profile/scheme admission+interpretation
     matrices. Host vocabulary/IDNA/PSL/DNS/IP detail is G3.H's (RCON-08);
     credential OUTPUT policy is P2.2's; cache is G3.9's; comparison keys are
     G3.K's — each referenced here as a boundary, never redefined. Format
     follows the G3.3/G3.K precedent: Envelope, tamper-evident Inputs, pipe-table
     Rows, Scope boundaries, Open cells. The envelope remains lifecycle_state
     PROPOSED until the cp-snapshot-3 seal; validator coverage and the manifest
     present-flip ride that seal. -->

## Envelope

| Field | Value |
|---|---|
| id | contract-standard-scheme |
| name | standard-scheme-matrices |
| artifact_number | 5 |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/contracts/standard-scheme-matrices.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 — this contract is the SINGLE WRITER of the v3 standard/profile/scheme admission and interpretation matrices (expansion, precedence, parser routes, eligibility, credentials, email, resolution, diagnostics) |
| lifecycle_state | PROPOSED |
| dependencies | P2.4 (admission bound decision); P4.1 (specialization bound decision); P1.2 (authority-state vocabulary, authority-emission cell); S6 (bound evidence); contract-canonical-state (field vocabulary only); P2.1/P2.2 (repair posture default + credential output boundary); reconciliation §6 artifact 5, §4 RCON-05, §7 G3 |
| bound_decision | P2.4 + P4.1 (joint C-11 disposition); P1.2 (authority-emission cell only) |
| bound_evidence | S6 |
| closes_finding | RCON-05 |
| completion_rule | §7 G3 — the artifact exists and contains no unowned cells: axis lattice + profile expansion, precedence, admission gate, interpretation/parser routes (incl. authority emission), scheme-family specialization/eligibility, credentials, email, resolution, and diagnostics each carry a non-placeholder owner_decision_ref with status SETTLED or an explicit status OPEN with a one-line impact and owner-decision destination; cross-artifact field names agree with artifact 3; validate-records.R (standard-scheme section, added at cp-snapshot-3) passes |
| content_hash | per-input sha256 under `## Inputs` (P2.4, P4.1, P1.2, S6), recomputed by validate-records.R at cp-snapshot-3 |
| approval_evidence | pending — seals at v3/cp-snapshot-3 (NOT an envelope flip) |
| validation_command | Rscript design/work/url-v3/tools/validate-records.R |
| validator_note | standard-scheme validator section stages with the cp-snapshot-3 seal |

## Purpose

The normative v3 contract for how standards, profiles, and schemes combine:
which scheme tokens are admitted under which axis settings (P2.4), what the
parser does with each admitted scheme under each interpretation standard (P4.1),
and how authority presence is emitted per route (P1.2). It fixes the axis
lattice and its defaults, profile expansion and precedence, the admission gate,
the interpretation/parser routes, per-family specialization and transform
eligibility, the credential and email surfaces, scheme-less/scheme-relative
resolution, and the diagnostics posture.

This record **projects** accepted P2.4 + P4.1 (+ P1.2 for authority emission) and
S6; it does not implement these surfaces and makes no product decision. A
`SETTLED` row cites `P2.4@b017e87`, `P4.1@b017e87`, or `P1.2@bb3346e`. A cell an
accepted record left open or deferred is `OPEN` with its exact impact and
settlement destination.

## Inputs

The exact sources this contract projects, hashed at authoring.
`validate-records.R` recomputes all hashes at the cp-snapshot-3 seal. P2.4, P4.1,
and P1.2 are already hash-enforced as ACCEPTED decisions by `validate-manifest.R`.

| path | sha256 |
|---|---|
| design/work/url-v3/decisions/P2.4-scheme-admission-boundary.md | 6f414f297094b46bef2a5e6c4f32938264e1026e4425955ec2a98f5b921d5cd6 |
| design/work/url-v3/decisions/P4.1-scheme-specialization-posture.md | 0d1e9a4d941b7398a935cbe66b91f174c7aecef768114964c6b1ee91b51b7cb2 |
| design/work/url-v3/decisions/P1.2-authority-state-vocabulary.md | 1c8f68528f834f96fefb0b2912661ea6ee33328b1d14652be3701810bcfaad2d |
| design/work/url-v3/evidence/S6-schemes-email.md | bc70876a6ec2af42411bc783ac30914893fa06a15cc275337ff982c2c0e05c99 |

## Axis lattice and profile-expansion rows

Admission and interpretation are decided by three orthogonal *configuration*
axes; orthogonal as separate arguments, not as a valid Cartesian product.

| axis | question | values | v3 default | owner_decision_ref | status |
|---|---|---|---|---|---|
| `scheme_acceptance` | which scheme *tokens* may enter parsing | `web` \| `general` | `web` (unchanged; CRAN byte-compat) | P2.4@b017e87 (D-B, D-C) | SETTLED |
| `scheme_policy` | is scheme-*less* host-shaped input accepted | `infer` \| `require` | `require` (strict default posture, P2.1/B1) | P2.4@b017e87 (D-C) | SETTLED |
| `url_standard` | which standard *interprets* an admitted token | `NULL` \| `rfc3986` \| `whatwg` | `NULL` (unchanged) | P2.4@b017e87 (D-B); ADR 0007 | SETTLED |
| composition rule | `general` requires a non-NULL `url_standard` | `general + NULL` → option-validation error before any row parses | n/a | P2.4@b017e87 (D-B) | SETTLED |

**Profile expansion** (named profile → axis settings; explicit args always win):

| profile | `scheme_acceptance` | notable coupled axes | owner_decision_ref | status |
|---|---|---|---|---|
| `browser` | `general` | scheme-relative → `http` | P2.4@b017e87 (D-B); P4.1@b017e87 (D-B) | SETTLED |
| `whatwg` | `general` | scheme-relative → `error`; sets `scheme_policy = require` | P2.4@b017e87 (D-B); P4.1@b017e87 (D-B) | SETTLED |
| `rfc-syntax` | `general` | scheme-relative → `keep` | P2.4@b017e87 (D-B); P4.1@b017e87 (D-B) | SETTLED |
| `seo` (alias `canonical`) | `web` | — | P2.4@b017e87 (D-B) | SETTLED |

## Precedence rows

| rule | contract | owner_decision_ref | status |
|---|---|---|---|
| explicit-args-win | an explicitly set axis overrides any profile/posture default (iron rule); an explicit `scheme_policy = "infer"` is honored even under the strict default posture | P2.4@b017e87 (D-B, D-C); ADR 0012 D6 | SETTLED |
| strict-override scoping | an explicit `infer` takes only the scheme-less axis out of strict for that call; the fabricated `http://…` is an inferred candidate reported through the P2.3 layered verdicts, never claimed as strict conformance of the original bytes | P2.4@b017e87 (D-C) | SETTLED |
| direct-vs-profile `scheme_policy` divergence | the `whatwg` profile sets `scheme_policy = require` while a direct `url_standard = "whatwg"` inherits the default — a scheme-less-acceptance foot-gun P2.4/P4.1 did not resolve | — (see Open cells SCHEME-O1) | OPEN |

## Admission gate (input class × axis setting)

Cells state **admitted / `error`** at the admission gate only; interpretation is
shown as a hint, owned by the parser-route rows below.

| input class | `web` (default) | `general` + `whatwg` | `general` + `rfc3986` | owner_decision_ref | status |
|---|---|---|---|---|---|
| `http`, `https` | admitted (allowlist) | admitted (special hint) | admitted | P2.4@b017e87 (D-B) | SETTLED |
| `ftp` | admitted (allowlist) | admitted (special hint) | admitted | P2.4@b017e87 (D-B) | SETTLED |
| `ftps` | admitted (grandfathered; no std default port) | admitted (non-special hint) | admitted | P2.4@b017e87 (D-A.3, D-B) | SETTLED |
| `file` | admitted (allowlist) | admitted (special hint) | admitted (RFC 8089 overlay) | P2.4@b017e87 (D-B) | SETTLED |
| `ws`, `wss` | **`error`** (not in allowlist; special-ness admission-inert) | admitted (special hint; ports 80/443) | admitted | P2.4@b017e87 (D-B) | SETTLED |
| `mailto`, `tel`, `data`, `sftp`, arbitrary `foo:` | **`error`** | admitted (non-special hint) | admitted | P2.4@b017e87 (D-B) | SETTLED |
| scheme-less host-shaped (`example.com`) | governed by `scheme_policy` (default `require` → `error`; `infer` → prepend `http://`) | same | same | P2.4@b017e87 (D-C) | SETTLED |
| scheme-relative (`//host`) | governed by `scheme_relative_handling` (independent axis) | same | same | P4.1@b017e87 (D-B); ADR 0010 | SETTLED |

Admission ≠ parse success: an admitted token may still fail on its remainder
(host-missing special scheme, RFC generic-grammar violation) — those boundaries
are the parser-route/specialization rows.

## Interpretation / parser-route rows

| route | contract | owner_decision_ref | status |
|---|---|---|---|
| WHATWG special set | exactly `http, https, ws, wss, ftp, file` are special under `url_standard = "whatwg"`; special ⇒ authority + host + WHATWG host model + default-port handling | P4.1@b017e87 (D-A.1) | SETTLED |
| special + missing host | a special scheme with a missing/empty host is a host-missing parse **`error`** | P4.1@b017e87 (D-A.1) | SETTLED |
| WHATWG non-special shapes | every other admitted scheme is non-special and parses as one of three shapes: opaque-path (`foo:bar`), list-path without authority (`foo:/bar`), or authority (`foo://host`, `foo:///bar`, `foo://[::1]/bar`) | P4.1@b017e87 (D-A.1, D-D) | SETTLED |
| RFC 3986 route | no special/non-special classification; an admitted scheme parses against RFC 3986's generic `URI` grammar via an independent deterministic RFC-general failure boundary (not libcurl permissiveness) | P4.1@b017e87 (D-A.2) | SETTLED |
| opaque-path trigger | under `whatwg`, after `:` a URL goes opaque **iff** the scheme is non-special AND the remainder does not start with `/` | P4.1@b017e87 (D-D) | SETTLED |
| authority emission (binds P1.2) | authority presence per route is recorded as `authority_delimiter_present: logical` + `authority_payload_kind: {empty, present}` with independent `host_kind`; serializers emit `//` iff `authority_delimiter_present`. Special ⇒ delimiter present + payload present + host present; `foo:///bar`/`file:///bar` ⇒ delimiter present + payload empty (they agree); `foo://@/bar`/`foo://:80/bar` ⇒ delimiter present + payload present; `foo:bar`/`foo:/bar` ⇒ delimiter absent | P1.2@bb3346e; P4.1@b017e87 (D-A) | SETTLED |

## Scheme-family specialization and eligibility rows

Four sections, one per property, not one table with four normative columns.

**Why the split, and what it does not change.** Until `RURL-fmkuunwj` these four
properties shared a single table keyed on `family`. Every cell below is
transcribed unchanged from that table, and each row keeps the exact
`owner_decision_ref` its composite row carried — no cell's meaning, status, or
attribution moves, and nothing here narrows a citation to a sub-property, which
would be a new attribution this record has no authority to make. What changes is
that a *row* now asserts one property instead of four.

That matters outside this file. `verification/traceability-map.md` makes a table
row the unit of a normative claim, so a row asserting special-ness *and* a
default port *and* host/PSL eligibility *and* semantic-transform eligibility
could not be assigned an owner: the map's own precedents send those to
`join-slice` (`KJ s3`), `host-slice` (`HA s2`/`HA s4`) and `mutation-slice`
(`CM s2`) respectively (P0.8 §4). Claim-level ownership overrides (P0.8 D-D)
made the *key* assignable but not the *row*, resolving exactly one of the nine —
`default-port data`, whose other three columns were `—`. Splitting the table is
the shape P0.8 D-D left to the owner and is what makes the remaining eight
expressible. `## Scheme-family default-port rows` is where the `default-port
data` row now lives: its payload sat in the `special-ness` column positionally
while stating the default-port table, which is why that row read as ambiguous
and why only its semantics — not its position — could resolve it.

## Scheme-family special-ness rows (whatwg)

| family | special-ness (whatwg) | owner_decision_ref | status |
|---|---|---|---|
| `http`, `https` | special | P4.1@b017e87 (D-A) | SETTLED |
| `ftp` | special | P4.1@b017e87 (D-A, D-B) | SETTLED |
| `ftps` | **non-special** (in-tree general parser; legacy curl RFC route) | P4.1@b017e87 (D-B) | SETTLED |
| `sftp` | non-special (general-only) | P4.1@b017e87 (D-B) | SETTLED |
| `file` | special | P4.1@b017e87 (D-A, D-E); ADR 0012 D5 | SETTLED |
| `ws`, `wss` | special (general-only) | P4.1@b017e87 (D-A, D-D) | SETTLED |
| `mailto` | non-special | P4.1@b017e87 (D-C); ADR 0012 D7 | SETTLED |
| `tel`, `data`, arbitrary `foo:` | non-special (opaque/list/authority) | P4.1@b017e87 (D-D) | SETTLED |

## Scheme-family default-port rows

| family | default port | owner_decision_ref | status |
|---|---|---|---|
| `http`, `https` | 80 / 443 | P4.1@b017e87 (D-A) | SETTLED |
| `ftp` | 21 | P4.1@b017e87 (D-A, D-B) | SETTLED |
| `ftps` | **none** (no standards-backed default port; grandfathered for CRAN, not standards) | P4.1@b017e87 (D-B) | SETTLED |
| `sftp` | none; **no browser-fixer inference** | P4.1@b017e87 (D-B) | SETTLED |
| `file` | none | P4.1@b017e87 (D-A, D-E); ADR 0012 D5 | SETTLED |
| `ws`, `wss` | 80 / 443 | P4.1@b017e87 (D-A, D-D) | SETTLED |
| `mailto` | none | P4.1@b017e87 (D-C); ADR 0012 D7 | SETTLED |
| `tel`, `data`, arbitrary `foo:` | none | P4.1@b017e87 (D-D) | SETTLED |
| default-port data | `.SCHEME_DEFAULT_PORTS` = `http 80, https 443, ftp 21, ws 80, wss 443`; deliberately no `ftps`/`sftp`; any asserted ftps/sftp port is convention, flagged, never standards-backed | P4.1@b017e87 (D-A.4, D-B) | SETTLED |

## Scheme-family host / PSL eligibility rows

| family | host / PSL eligibility | owner_decision_ref | status |
|---|---|---|---|
| `http`, `https` | DNS/PSL + host presentation eligible | P4.1@b017e87 (D-A) | SETTLED |
| `ftp` | DNS/PSL-eligible host (detail → G3.H) | P4.1@b017e87 (D-A, D-B) | SETTLED |
| `ftps` | host-eligible; detail → G3.H | P4.1@b017e87 (D-B) | SETTLED |
| `sftp` | host-eligible; detail → G3.H | P4.1@b017e87 (D-B) | SETTLED |
| `file` | localhost/empty-host mapping; drive/path per standard (detail → G3.H) | P4.1@b017e87 (D-A, D-E); ADR 0012 D5 | SETTLED |
| `ws`, `wss` | host model under `general`; inert under `web` | P4.1@b017e87 (D-A, D-D) | SETTLED |
| `mailto` | recipient-domain PSL carve-out (D-C) | P4.1@b017e87 (D-C); ADR 0012 D7 | SETTLED |
| `tel`, `data`, arbitrary `foo:` | none | P4.1@b017e87 (D-D) | SETTLED |

## Scheme-family semantic-transform eligibility rows

| family | semantic-transform eligibility | owner_decision_ref | status |
|---|---|---|---|
| `http`, `https` | **full HTTP(S) semantic/canonical transforms** | P4.1@b017e87 (D-A) | SETTLED |
| `ftp` | none (SEO transforms are HTTP(S)-only) | P4.1@b017e87 (D-A, D-B) | SETTLED |
| `ftps` | none | P4.1@b017e87 (D-B) | SETTLED |
| `sftp` | none | P4.1@b017e87 (D-B) | SETTLED |
| `file` | none | P4.1@b017e87 (D-A, D-E); ADR 0012 D5 | SETTLED |
| `ws`, `wss` | none | P4.1@b017e87 (D-A, D-D) | SETTLED |
| `mailto` | none | P4.1@b017e87 (D-C); ADR 0012 D7 | SETTLED |
| `tel`, `data`, arbitrary `foo:` | none | P4.1@b017e87 (D-D) | SETTLED |

## Credential rows

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| five distinct credential routes | authority credentials (`foo://u:p@host`), scheme-less `user@host` (`warning-userinfo`), RFC `file:` userinfo, `mailto:` local parts, and web-authority userinfo are never conflated | P4.1@b017e87 (D-C) | SETTLED |
| generic-authority credential preservation | parsed non-special authority userinfo is **preserved** in canonical state (`user`/`password`/`userinfo` populated, row reassemblable), fixing the shipped drop | P4.1@b017e87 (D-C, Q1/B9) | SETTLED |
| credential output policy (boundary) | `clean_url` never reconstructs credentials; safe display (`format_url`) redacts; only deliberate source-reproduction and exact standard-serialization surfaces may reproduce them — owned by **P2.2**, cross-referenced not redefined | P2.2 (#212) via P4.1@b017e87 (Q1/B9) | SETTLED (boundary) |
| `get_password()` selector parity | `get_password()` lacks the `scheme_acceptance`/`url_standard` selectors carried by `get_user()`/`get_userinfo()` | — (see Open cells SCHEME-O2) | OPEN |

## Email rows

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| recipient projection | under `general`, a `mailto:` URL's **first** recipient decomposes through the same web accessors (`get_host`/`get_domain`/`get_tld`/`get_subdomain`/`get_user`/`get_userinfo`) via the same PSL seam — the one place a general-routed host reaches the PSL, because a recipient RHS is a domain | P4.1@b017e87 (D-C); ADR 0012 D7 | SETTLED |
| extraction is metadata-only | recipient domain populates internal Stage-A `host`; Stage-B serializes from its own re-parse (host `NA` for mailto), so `clean_url`/round-trip is byte-unchanged; non-domain RHS → `NA` host, local part still resolves as `user` | P4.1@b017e87 (D-C); ADR 0012 D7 | SETTLED |
| first-local-part decode | decode **exactly once**, with corrected `get_user()` docs | P4.1@b017e87 (D-C, Q3) | SETTLED |
| email helper surface | `get_mailto_recipients()` is the **only** email-specific helper; rurl mints no other mailto/email extraction functions (reuses web accessors); `smtp_wire` is opt-in and **must never alter URL acceptance** | P4.1@b017e87 (D-C); ADR 0012 D7; [[rurl-email-mailto-d7]] | SETTLED |
| indeterminate lexer + URL-level email facts | the `indeterminate` unresolved-lexer outcome (unterminated quote / domain-literal) and the URL-level facts `userinfo-form` / `public-suffix-known` / `smtp-envelope-wire-mode` are accepted but not surfaced | — (see Open cells SCHEME-O3) | OPEN |

## Resolution rows

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| scheme-less input | governed by `scheme_policy`: default `require` rejects; `infer` fabricates `http://` for host-shaped input (an inferred candidate, not strict conformance) | P2.4@b017e87 (D-C) | SETTLED |
| scheme-relative `//host` | an independent input axis `scheme_relative_handling ∈ {keep, http, https, error}`, independent of `scheme_acceptance`/`scheme_policy`; per-profile defaults `browser → http`, `whatwg → error`, `rfc-syntax → keep` | P4.1@b017e87 (D-B); ADR 0010 | SETTLED |
| RFC relative-reference resolution algorithm | base eligibility, standard-specific reference-merging, arbitrary/opaque-scheme handling, credential and empty-component behavior for `R/resolve.R` reference resolution | — (see Open cells SCHEME-O4) | OPEN |

## Diagnostics rows

| aspect | contract | owner_decision_ref | status |
|---|---|---|---|
| `get_scheme_class()` | surfaces `special` / `non-special` / opaque as a **companion diagnostic**, fed by `get_scheme()` returning the literal scheme for accepted opaque input; metadata, never a parse gate | P4.1@b017e87 (D-A.3); ADR 0006 | SETTLED |
| selected scheme facts | scheme-specific violations (`data:` missing comma, `tel:` missing `;phone-context=`, `mailto:` carrying a fragment) are `ok`-family parses plus a diagnostic, **never** an `error`; a generic-grammar violation (RFC repeated raw `@`, malformed `%`-triplet) **is** fatal under `rfc-syntax` | P4.1@b017e87 (D-D); ADR 0012 D5 | SETTLED |
| absence-is-not-conformance | selected facts are not a validity oracle; a diagnostic's absence never asserts conformance | P4.1@b017e87 (D-A.3); ADR 0012 D5; ADR 0006 | SETTLED |
| email diagnostics | all email/SMTP facts are companion-only (ADR 0006); the `smtp_wire` tier is opt-in and never alters acceptance | P4.1@b017e87 (D-C) | SETTLED |

## Scope boundaries

This contract owns the **standard/profile/scheme admission + interpretation**
matrices. It does **not** define, and must not be read as redefining:

- **Host vocabulary, IDNA profile/order, PSL source identity and private-section
  policy, DNS boundary, IP identity** — RCON-08, owned by the P4 host record and
  **G3.H**. This contract states only the scheme-forced host *posture* (special ⇒
  host mandatory; `file` localhost/empty-host; ftp DNS-eligible; mailto recipient
  domain) and defers the vocabulary.
- **Credential output/serialization + security policy** — owned by **P2.2**. This
  contract records credential *preservation in canonical state* only; the
  clean/display/exact-serialize redaction rules are P2.2's.
- **Cache semantics** — owned by **G3.9**.
- **Comparison keys / URL joins** — owned by **G3.K / P3.1**; scheme/port
  *equivalence* for keys lives there, not here.
- **Cleaning / mutation semantics** — owned by P2.1/P2.2 and **G3.8**; this
  contract states transform *eligibility* per scheme, not the transforms.
- **Deep mailto recipient provenance** (tokenize-before-decode, `%2C`
  non-delimiter) and the email fixture table — the email PRD + P4-email slice.
- **Canonical-state field vocabulary** (`authority_delimiter_present`,
  `authority_payload_kind`, `host_kind`, scheme fields) — consumed from artifact 3
  / P1.2 without renaming.
- **Documentation hygiene** — re-statusing ADR 0004's closed-scheme rule (P2.4
  Q2) and correcting the stale `R/parse.R` "general is internal-only" comment
  (P2.4 Q4) are parked housekeeping/implementation-cleanup slices, not cells of
  this contract.

## Open cells

Each cell an accepted record left open or deferred is recorded rather than
invented. None reopens a SETTLED default.

- **SCHEME-O1 — direct-vs-profile `scheme_policy` precedence.** The `whatwg`
  profile sets `scheme_policy = require` while a direct `url_standard = "whatwg"`
  inherits the default, so the two diverge on scheme-less acceptance (S6 O2;
  P2.4 Q5; P4.1 Q5). **Impact:** identical-looking `whatwg` requests accept or
  reject scheme-less input differently depending on profile-vs-direct entry.
  **Settles at:** the standards/profile-precedence slice.
- **SCHEME-O2 — `get_password()` selector parity.** `get_password()` lacks the
  `scheme_acceptance`/`url_standard` selectors carried by `get_user()` /
  `get_userinfo()` (S6 O4; P4.1 Q2). **Impact:** password extraction cannot be
  scoped by admission/standard the way its sibling accessors can. **Settles at:**
  the credential-accessor implementation-cleanup slice.
- **SCHEME-O3 — mailto `indeterminate` lexer + URL-level email facts.** The
  accepted `indeterminate` unresolved-lexer outcome and the URL-level email facts
  `userinfo-form` / `public-suffix-known` / `smtp-envelope-wire-mode` are not
  surfaced by the shipped splitter/diagnostics (S6 O6/O7; P4.1 Q4). **Impact:**
  accepted email diagnostics remain unavailable; an unterminated recipient
  boundary has no defined non-`error` outcome surfaced. **Settles at:** the
  P4-email slice, paired with the email PRD.
- **SCHEME-O4 — RFC relative-reference resolution layer.** S6 rates resolution a
  **Missing** layer: `R/resolve.R` applies RFC-style reference merging then
  reparses/serializes, but base eligibility, standard-specific algorithms,
  arbitrary/opaque-scheme handling, and credential/empty-component behavior have
  no normative contract; P2.4/P4.1 settle only the scheme-less and scheme-relative
  *input* axes, not reference resolution. **Impact:** relative-reference
  resolution behavior is unspecified and untested against a contract. **Settles
  at:** a dedicated resolution-layer decision (S6 Resolution row; not owned by
  P2.4/P4.1).
