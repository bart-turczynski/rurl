# S6 — Schemes, userinfo, email, and resolution protocol review

## Executive verdict

**Verdict: not sufficient as written; amend the reconstruction protocol before using it to draft the v3 specification.**

The protocol identifies the right subject matter—scheme classes, credentials, `mailto:`, browser repair, diagnostics, and fixture-backed contracts—but it treats them as a list of topics rather than as intersecting behavioral axes. That is too weak for this repository. The shipped surface and accepted design depend on distinctions among admission, parser standard, URL shape, scheme class, repair posture, semantic transform eligibility, diagnostic selection, accessor behavior, and optional email wire analysis. A draft can satisfy every bullet in protocol §6.6 while still erasing those distinctions.

The protocol should require three concrete artifacts before drafting:

1. a normative scheme × axis matrix with a v3 disposition for every current behavior;
2. a userinfo/credential route matrix that separates authority credentials, scheme-less URI userinfo, `mailto:` local parts, and the RFC `file:` extension;
3. a complete `mailto:` recipient/diagnostic fixture table, including raw delimiter provenance, indeterminate lexer states, public-suffix/IDNA behavior, optional SMTP wire projections, and first-recipient-versus-all-recipient API boundaries.

This review also found shipped-versus-accepted gaps that the current protocol would not reliably surface: generic non-special authority userinfo is discarded on the in-tree parse path; accepted `mailto:` indeterminate lexer semantics are not implemented; accepted URL-level email diagnostics are absent; `get_password()` lacks the general-parser selectors carried by adjacent accessors; and resolution is not represented as its own reconstruction layer.

## Coverage map

| Area | Proposed protocol coverage | Durable/current evidence | Sufficiency | Required protocol amendment |
|---|---|---|---|---|
| Scheme admission | §6.6 lists web schemes, `file:`, WS, `mailto:`, `tel:`, `data:`, and arbitrary schemes; §7 asks whether scheme handling is generalized | ADR 0012 D3–D4 defines `scheme_acceptance` independently from parser standard and gives exact web/general families | **Insufficient** | Require an admission matrix by scheme/family, `web`/`general`, explicit/default standard, and profile/direct-call posture |
| Interpretation standard | §6.2 asks for a standards decision and parse/serialize separation | ADR 0012 D1 distinguishes WHATWG special/non-special parsing from RFC grammar and URL shapes | **Partial** | Require every scheme row to name the parse model/backend and serialization model under both standards |
| URL shape/state | §6.1 requires authority/path state, present-empty components, and reassemblable credentials; §6.3 requires empty/fragment preservation | ADR 0012 D2 and `R/parse-state.R` distinguish special, opaque, list-path, and authority forms | **Partial** | Add mandatory shape rows and exact round-trip fixtures for each non-special form, including empty query/fragment/authority |
| HTTP(S), FTP, FTPS | §6.6 says web schemes and non-web schemes, but has no per-scheme rules | `.SUPPORTED_SCHEMES`, `.WHATWG_SPECIAL_SCHEMES`, `.DEFAULT_PORTS`, ADR 0012 D4 and the scheme-class tests encode materially different treatment of FTP and FTPS | **Insufficient** | Require distinct rows for HTTP(S), FTP, and FTPS; do not use one “web schemes” row |
| `file:` | §6.6 names file URLs | ADR 0012 D5 and `R/parse-state.R` contain separate WHATWG state and RFC overlay behavior, including an RFC userinfo extension | **Insufficient** | Require host/localhost/drive/path/port/userinfo/diagnostic behavior under each standard and acceptance mode |
| WS/WSS | §6.6 names WebSocket URLs | ADR 0012 D4–D5 makes WS/WSS general-only, WHATWG-special, with scheme facts and default ports | **Insufficient** | Require admission, standard, credential/fragment diagnostics, default-port, and browser-fixer cells |
| Arbitrary schemes | §6.6 mentions arbitrary/unknown schemes | ADR 0012 D1–D2 and `test-general-acceptance.R` define opaque, list-path, authority, and empty-authority forms | **Partial** | Require a family matrix and exact serializer/accessor/PSL eligibility behavior, not a single arbitrary-scheme example |
| Scheme-relative input | §6.6 names scheme-relative inputs | ADR 0010 makes this independent from scheme admission; `scheme_relative_handling` is a separate option | **Partial** | Represent it as an input-shape/repair axis, not as a scheme row |
| Credentials/userinfo | §6.1 and §6.3 mention preservation and credentials | `R/parse-state.R`, `R/parse.R`, `R/accessors.R`, diagnostics, and the email PRD expose four distinct meanings/routes | **Insufficient** | Require a route matrix covering parse state, public fields, clean serialization, accessors, status, diagnostics, and security posture |
| `mailto:` parsing | §6.6 mentions multi-recipient forms, grammar facts, SMTP projections, and no deliverability checks | Accepted email/userinfo PRD specifies raw token provenance, positional recipients, grammar, IDNA/PSL, cardinality, and cost tiers | **Insufficient** | Promote those contracts into required table fields and fixtures; “multi-recipient forms” is not specific enough |
| Email diagnostics | §6.6 says acceptance versus diagnostics | Email PRD and `R/email-diagnostics.R` define lexical, domain, suffix, and wire vocabularies, but accepted and shipped contracts differ | **Insufficient** | Require vocabulary, cardinality, gating, cost, indeterminate-state, and shipped/tested/v3-disposition columns |
| Generic diagnostics | §6.6 and §7 distinguish parsing from diagnostics at a high level | ADR 0012 D5 says selected facts are not a validity oracle; `R/diagnostics.R` has a closed vocabulary | **Partial** | Require eligibility and cardinality per fact, plus explicit “absence is not conformance” language |
| Semantic transforms | §6.4 asks for mutation/cleaning rules | ADR 0012 D2 restricts semantic transforms to HTTP(S) while allowing only narrower presentation operations elsewhere | **Partial** | Cross every scheme row with semantic, host-presentation, path-presentation, query, and port eligibility |
| Browser repair | §6.2 names browser-style repair | Browser fixer PRD and profiles limit repair to a known sequence and scheme set; unknown schemes are not rewritten | **Partial** | Require repair eligibility and sequence as a separate axis; distinguish profile defaults from direct selectors |
| Resolution | No dedicated layer or fixture row | `R/resolve.R` applies RFC-style reference merging and then reparses/serializes; tests focus on web-era behavior | **Missing** | Add a resolution layer covering base eligibility, standard-specific algorithms, arbitrary/opaque schemes, credentials, and empty components |
| Compatibility/documentation | §9 and §11 cover compatibility and traceability generally | ADR 0007 is superseded by ADR 0012; several comments/vignette passages still describe pre-general behavior | **Partial** | Require point-in-time document classification and a stale-document cleanup ledger |

## Scheme-by-scheme current-state and evidence matrix

This table describes the current accepted/shipped posture that v3 must explicitly preserve, replace, or supersede. It is not itself a recommendation that every behavior remain unchanged.

| Scheme/family | Current web admission | Current general + WHATWG | Current general + RFC | Accessors, diagnostics, transforms, and notable evidence |
|---|---|---|---|---|
| `http`, `https` | Accepted by the default curated web set | WHATWG special; curl-backed parsed state | Legacy curl-backed path remains admitted | DNS/PSL and host presentation are eligible; credentials can be exposed; full semantic/canonical transforms are eligible; default ports 80/443. Evidence: `R/utils.R`, `R/parse-state.R::.general_parsed_mask()`, ADR 0012 D2/D4 |
| `ftp` | Accepted by the default curated web set | WHATWG special; curl-backed | Legacy curl-backed | DNS/PSL and host presentation are eligible; general semantic transforms are not HTTP(S)-eligible; default port 21. Evidence: `R/utils.R`, `R/parse-state.R::.stage_b_eligibility()`, ADR 0012 D2/D4 |
| `ftps` | Grandfathered in the curated web set | Non-special under WHATWG and therefore routed to the in-tree general parser | Kept on the legacy curl-backed RFC route | No default-port entry and no browser-fixer inference contract. The difference from `ftp` is load-bearing. Evidence: ADR 0012 D4 and resolved questions; `R/utils.R`; `test-url-standard-scheme-class.R` |
| `file` | Accepted by the curated web set | WHATWG special semantics represented by the in-tree state path | Separate RFC overlay, including repository-specific undivided userinfo support and port rejection | File-specific facts, localhost/empty-host handling, and standard-dependent behavior require their own row. Evidence: ADR 0012 D5; `R/parse-state.R::.parse_rfc_file_one()`; `test-url-standard-file.R` |
| `ws`, `wss` | Rejected by default web admission | WHATWG special; general-only | Parsed through the RFC general route | Default ports 80/443; selected WHATWG fragment/userinfo facts; not eligible for HTTP(S) semantic transforms. Evidence: ADR 0012 D4/D5; `R/utils.R`; `test-general-acceptance.R`; `test-ws-wss-inertness.R` |
| `mailto` | Rejected by default web admission | Non-special opaque-style parsing | RFC rootless/path-oriented parsing | First positional recipient is projected through standard accessors only in general mode; all positional recipients use `get_mailto_recipients()`; selected fragment and email facts; no deliverability checks. Evidence: ADR 0012 D5/D7; email/userinfo PRD; `R/email-diagnostics.R`; `test-email-diagnostics.R` |
| `tel` | Rejected by default web admission | Non-special opaque-style parsing | RFC rootless parsing | Selected scheme-specific facts only; no DNS/PSL or web semantic transforms. Evidence: ADR 0012 D5; `test-url-diagnostics.R` |
| `data` | Rejected by default web admission | Non-special opaque-style parsing | RFC rootless parsing | Selected media/base64 facts only; no host semantics or web transforms. Evidence: ADR 0012 D5; `test-url-diagnostics.R` |
| `sftp` | Rejected by default web admission | Non-special general parsing | RFC general parsing | No browser-fixer inference and no default-port rule. Accepted ADR resolution keeps it general-only. Evidence: ADR 0012 resolved questions and `R/utils.R` |
| Arbitrary opaque, e.g. `foo:bar` | Rejected by default web admission | Non-special opaque state | RFC rootless state | Exact shape is preservable; no generic DNS/PSL or host transform eligibility. Evidence: ADR 0012 D1/D2; `test-general-acceptance.R` |
| Arbitrary list-path, e.g. `foo:/a` | Rejected by default web admission | Non-special list-path state | RFC path state | Exact slash/path/query/fragment presence is part of the state contract. Evidence: `R/parse-state.R::.parse_opaque_url_one()` and general acceptance fixtures |
| Arbitrary authority, e.g. `foo://host/a` | Rejected by default web admission | Non-special authority state | RFC authority state | Generic DNS/PSL is deliberately not inferred. **Current gap:** `.parse_opaque_url_one()` calls `.split_authority()` but drops returned userinfo from its output; the public vector initializes userinfo as `NA`. Evidence: `R/parse-state.R` around `.split_authority()`, `.parse_opaque_url_one()`, and `.general_parse_vec()` |
| Scheme-relative, e.g. `//host/a` | Governed by scheme inference and scheme-relative handling, not by a scheme membership row | Same independent input posture | Same independent input posture | `keep`/`http`/`https`/`error` is independent from `scheme_acceptance`; profiles may set a different default. Evidence: ADR 0010; `R/parse.R` option matching and profile table |

### Current route diagram

```text
input
  ├─ admission: scheme_acceptance = web | general
  ├─ missing/relative scheme: scheme_policy + scheme_relative_handling
  ├─ interpretation: url_standard = whatwg | rfc
  │    ├─ WHATWG special → special/curl-or-file state route
  │    └─ non-special    → opaque/list/authority in-tree route
  ├─ optional repair: profile/fixup posture
  ├─ operation eligibility: semantic | host presentation | path presentation
  ├─ serialization/cleaning: protocol/query/path/port/etc.
  └─ annotations: generic diagnostics | scheme facts | mailto recipients/wire facts
```

The protocol currently names most boxes but does not require this routing relationship to be reconstructed.

## Scheme, userinfo, and email dial inventory

The v3 protocol should require every dial to be classified as **admission**, **interpretation**, **repair**, **serialization/presentation**, **annotation**, or **cost**. It should also record defaults for direct calls and each named profile, allowed combinations, rejection behavior, vectorization/recycling, and whether the dial changes parsing or only output.

| Dial / API | Category | Current values/posture | Load-bearing interaction the protocol must capture |
|---|---|---|---|
| `scheme_acceptance` | Admission | `web`, `general` | `general` requires a non-`NULL` standard; web admission is not a parser standard |
| `scheme_policy` | Admission/inference | `infer`, `require` | Controls missing schemes independently from whether an explicit scheme is admitted |
| `url_standard` | Interpretation | `NULL`, `whatwg`, `rfc` | Selects grammar/state; direct `url_standard = "whatwg"` does not imply all profile defaults |
| `scheme_relative_handling` | Relative-input posture | `keep`, `http`, `https`, `error` | Independent from `scheme_policy` and `scheme_acceptance` |
| `profile` | Coordinated policy | `browser`, `whatwg`, `rfc-syntax`, `seo`; `canonical` alias | Profiles bundle defaults; explicit arguments can customize them. The `whatwg` profile requires a scheme while the direct standard selector inherits ordinary inference defaults |
| `fixup_posture` | Repair, internal/profile-driven | `none`, `browser` | Browser repair is ordered and scheme-aware; it is not equivalent to standards parsing or Chrome navigation |
| `protocol_handling` | Serialization/presentation | `keep`, `none`, `strip`, `http`, `https` | Can remove or force visible protocol and therefore must not be conflated with admission or interpretation |
| `www_handling` | Host presentation | `none`, `strip`, `keep`, `if_no_subdomain` | Eligibility differs by parsed host kind/scheme |
| `case_conversion` | Presentation | `lower_host`, `keep`, `lower`, `upper` | Host-only versus whole-output semantics matter for non-web URLs |
| `trailing_slash`, `index`, `path_normalization`, `path_encoding` | Path presentation/semantic mutation | Existing enumerated policies in `R/parse.R` | ADR 0012 distinguishes explicit presentation from web-only semantic transforms |
| `host_encoding` | Host presentation | `keep`, `idna`, `unicode` | Generic arbitrary hosts are not automatically DNS; `mailto:` has an explicit recipient-domain carveout |
| `query`, `empty_param` | Query serialization | `drop`, `filter`, `allow`, `keep`; `keep`, `drop` | Generic-scheme behavior currently avoids web query semantics and passes raw state through |
| `port` | Serialization | `exclude`, `keep`, `strip_default`, `strip_all` | Default ports exist for HTTP(S), FTP, WS/WSS, but not FTPS/SFTP; port eligibility differs for file/generic forms |
| `smtp_wire` in `get_mailto_recipients()` | Optional cost/annotation | off by default; opt-in wire projection | Must never alter URL acceptance; requires stable disabled/not-applicable sentinel behavior |
| `get_user()`, `get_password()`, `get_userinfo()` | Accessor projections | User/userinfo accept general selectors; password currently only accepts protocol handling | Public parity and the meaning of “userinfo” differ among authority URLs and `mailto:` |
| `get_mailto_recipients()` | Dedicated scheme helper | Defaults to RFC + general; one row per positional recipient | All-recipient, grammar, PSL/IDNA, and optional wire diagnostics must remain separate from first-recipient generic accessors |
| `get_url_diagnostics()` | Annotation | Closed vocabulary of selected facts | Facts are not a conformance oracle; gating may follow parsed standard and scheme class rather than admission alone |

## Severity-ordered findings

### Blocker 1 — §6.6 does not require the scheme matrix needed to preserve current semantics

- **Protocol section:** §6.6 “Schemes, userinfo, and email” and §7 “Required layer-by-layer questions”.
- **Exact evidence:** §6.6 provides a flat checklist of web schemes, `file:`, WS, `mailto:`, `tel:`, `data:`, arbitrary schemes, helpers, and diagnostics (`_scratch/url-v3-spec-reconstruction-protocol.md:177`). ADR 0012 D1–D4 instead defines orthogonal scheme class, acceptance, standard, parser route, and operation eligibility. Shipped routing is encoded in `.general_parsed_mask()` and `.stage_b_eligibility()` (`R/parse-state.R`).
- **Example:** `ftps:` is accepted by the legacy web set, non-special under WHATWG general parsing, retained on the legacy RFC/curl route, has no default-port entry, and is excluded from browser-style scheme inference. `ws:` is WHATWG-special but general-only and has a default port. A single “web versus non-web schemes” paragraph cannot represent either case.
- **Consequence:** A v3 draft can generalize from HTTP(S), silently collapse parser routes, assign wrong transforms/default ports, or change backward-compatible admission while still appearing to satisfy the protocol.
- **Required correction:** Add a mandatory row-per-scheme/family matrix with columns for default admission, explicit web/general admission, WHATWG class/route, RFC class/route, state shape, serializer, transform eligibility, host/PSL/IDNA eligibility, port rules, repair eligibility, diagnostic facts, accessors/helpers, and v3 disposition (`preserve`, `change`, `remove`, `unresolved`).

### Blocker 2 — The protocol does not reconstruct the selector lattice or profile/direct-call differences

- **Protocol section:** §5 required artifacts, §6.2 standards, and §6.4 mutation/cleaning.
- **Exact evidence:** Protocol artifacts require a “Decision Ledger” and behavior matrix but do not require a complete public-dial inventory or combination table (`_scratch/url-v3-spec-reconstruction-protocol.md:82-116`). Current public signatures in `R/parse.R` expose separate selectors for acceptance, scheme inference, standard, relative handling, protocol, host/path/query/port policies, engine, and profiles. The profile table gives `profile = "whatwg"` a required-scheme posture while a direct `url_standard = "whatwg"` call inherits ordinary inference behavior.
- **Example:** Treating the `whatwg` profile as an alias for `url_standard = "whatwg"` changes scheme-less input behavior. Treating `scheme_acceptance = "general"` as synonymous with WHATWG parsing loses the RFC route and the explicit-standard requirement.
- **Consequence:** The spec may document named presets but leave invalid or ambiguous combinations undefined, making implementation and tests disagree on which knob owns a behavior.
- **Required correction:** Require the dial inventory above, a direct-call/profile defaults table, combination validation rules, and ownership statements for every effect: admission, interpretation, repair, presentation, semantic transform, annotation, or cost.

### Blocker 3 — Credentials/userinfo are named but not modeled as distinct routes

- **Protocol section:** §6.1 parsed state and §6.3 fragment/credential preservation.
- **Exact evidence:** The protocol requires credentials to be explicit and reassemblable (`_scratch/url-v3-spec-reconstruction-protocol.md:122-151`) but does not enumerate their meanings or public routes. The code has separate behavior for curl-backed authority credentials, RFC `file:` userinfo, scheme-less `user@host` warnings, and `mailto:` local parts. In the generic authority route, `.parse_opaque_url_one()` calls `.split_authority()` but omits the returned userinfo from its result, and `.general_parse_vec()` initializes userinfo without filling it (`R/parse-state.R`). `get_password()` does not accept `scheme_acceptance` or `url_standard`, unlike `get_user()`/`get_userinfo()` (`R/accessors.R`).
- **Example:** `foo://u:p@host/a`, `file://u@host/a`, `ws://u:p@host/a`, `user@host`, and `mailto:user@host` contain superficially similar text but represent different state, warnings, facts, accessors, and serialization/security behavior.
- **Consequence:** A draft may claim credential preservation while an implementation drops generic authority userinfo, mislabels an email local part as URI userinfo, or accidentally serializes secrets into cleaned output.
- **Required correction:** Add a credential route matrix for those five cases with columns for delimiter ownership, decoded/raw state, public `user`/`password`/`userinfo`, clean serialization, exact round-trip serialization, warnings/status, diagnostics, and allowed transforms. Require an explicit v3 security decision for credential-bearing clean output.

### Blocker 4 — The `mailto:` checklist is too shallow to recover the accepted contract

- **Protocol section:** §6.6 and §7 fixture matrix.
- **Exact evidence:** §6.6 mentions “multi-recipient forms, grammar facts, SMTP wire projections, and explicit non-goals” (`_scratch/url-v3-spec-reconstruction-protocol.md:177-184`), while the fixture list contains only a broad `mailto:` row (`:221-235`). The accepted email/userinfo PRD specifies tokenize-before-decode, `%2C` non-delimiter behavior, quote/bracket contexts, positional-only recipients, one-decode provenance, `indeterminate` lexer outcomes, IDNA/PSL projections, 64/256-octet checks, SMTPUTF8 classification, cardinality, and disabled sentinels (`design/prd/email-userinfo-diagnostics.md`).
- **Example:** `mailto:a%2Cb@example.com,c@example.com` has two positional recipients, not three. An unclosed quoted string or domain literal must not invent recipient boundaries; the accepted PRD calls for an indeterminate outcome. The shipped splitter returns tokens but does not expose unresolved quote/bracket state, so `.email_recipient_facts()` defaults such cases toward validity/invalidity rather than assigning `indeterminate` (`R/email-diagnostics.R`).
- **Consequence:** Implementations can pass a nominal multi-recipient test while violating raw provenance, producing fabricated recipients, or changing diagnostic cardinality and wire-cost behavior.
- **Required correction:** Make the email PRD dimensions mandatory protocol fields and add the fixture families listed below. Require an explicit accepted/shipped/v3 disposition for each email fact and lexer state.

### High 5 — Resolution is absent as a reconstruction layer

- **Protocol section:** §7 layer questions and §7 fixture matrix.
- **Exact evidence:** There is no resolution layer or resolution fixture category in the protocol. `resolve_url()` in `R/resolve.R` performs RFC-style reference merging, then sends the result through parsing and canonical cleaning. Existing `test-resolve-url.R` primarily protects web-era/reference behavior and standard pass-through rather than defining arbitrary-scheme or opaque-base semantics.
- **Example:** The protocol does not force a decision for resolving `../x` against `foo://host/a/b`, a relative reference against `foo:opaque`, a scheme-relative reference against a general base, or an absolute `g:h` reference under general admission. It also does not state whether credentials, empty query/fragment markers, or standard-specific base algorithms survive.
- **Consequence:** Parsing can be generalized while resolution remains implicitly web/RFC-shaped, yielding internally inconsistent v3 behavior.
- **Required correction:** Add a resolution layer with base/reference admission, WHATWG-versus-RFC algorithm ownership, opaque-base behavior, authority/credential preservation, scheme-relative behavior, exact empty-component behavior, serialization policy, and dedicated fixtures.

### High 6 — The evidence artifacts do not force accepted-versus-shipped disposition per contract

- **Protocol section:** §5 Evidence Catalog, Contradiction Register, and Decision Ledger.
- **Exact evidence:** The proposed fields classify sources and contradictions, but not each behavioral contract across `normative`, `shipped`, `tested`, `documented`, and `v3 disposition`. The accepted email PRD includes URL-level facts such as `userinfo-form`, `public-suffix-known`, and `smtp-envelope-wire-mode`; `.URL_DIAGNOSTICS` in `R/diagnostics.R` does not include them, and exact-vocabulary tests preserve the smaller shipped set. The same PRD requires `indeterminate` unresolved lexer semantics that the current splitter does not implement.
- **Example:** A contradiction register can note “PRD and code differ” without forcing an owner to decide whether v3 implements the PRD, preserves shipped behavior, or supersedes the PRD.
- **Consequence:** The draft may cite accepted design and shipped tests interchangeably, leaving implementation tickets without a normative target.
- **Required correction:** Extend each matrix/ledger row with `accepted design`, `shipped code`, `test coverage`, `public documentation`, `v3 disposition`, `owner`, and `required fixture`. No mismatch may be silently resolved by source ordering alone.

### High 7 — Diagnostic selection and validity remain underspecified

- **Protocol section:** §6.6 “acceptance versus diagnostics” and §7 diagnostics questions.
- **Exact evidence:** ADR 0012 D5 states that scheme-specific facts are selected annotations, not a comprehensive validity oracle. `R/diagnostics.R` implements a closed vocabulary with scheme/standard-dependent gating; WHATWG credential/invalid-URL facts can be attached independently from whether the curated web set admits a scheme. The protocol only asks which malformed inputs are preserved or repaired and which diagnostics are returned.
- **Example:** Absence of a `tel:` or `data:` fact does not certify conformance. A WHATWG-specific credential fact and a general-only scheme fact can follow different gates for the same input.
- **Consequence:** Consumers may treat facts as exhaustive validation, or an implementation may gate diagnostics on `scheme_acceptance` when the accepted design gates them on parsed standard and state.
- **Required correction:** Require a fact matrix with vocabulary, exact trigger, scheme/standard gate, cardinality, severity/status effect, cost tier, and explicit non-oracle statement.

### Medium 8 — Scheme-relative input is liable to be conflated with schemes or browser repair

- **Protocol section:** §6.6 scheme-relative inputs and §6.2 repair.
- **Exact evidence:** ADR 0010 makes scheme-relative handling independent from `scheme_policy`; `R/parse.R` exposes `scheme_relative_handling = keep/http/https/error`. Profiles may coordinate this dial with inference and repair, but it remains a separate decision.
- **Example:** `//example.com/a` can be kept, rejected, or assigned HTTP/HTTPS without changing whether `mailto:` or `foo:` is admitted.
- **Consequence:** A combined “scheme handling” rule can unintentionally make admission choices alter relative-input semantics.
- **Required correction:** Add a separate scheme-relative row to the public dial table and fixture matrix, crossed with direct calls and profiles.

### Medium 9 — FTPS/SFTP/default-port/fixer distinctions need explicit fixtures

- **Protocol section:** §6.6 scheme coverage and §7 fixture matrix.
- **Exact evidence:** ADR 0012 resolves FTPS as a grandfathered web scheme and SFTP as general-only, with neither gaining a default-port rule. `.DEFAULT_PORTS` in `R/utils.R` includes HTTP(S), FTP, WS/WSS but not FTPS/SFTP. Browser-fixer policy does not infer arbitrary/unknown schemes.
- **Example:** Generalizing from `ftp:` could cause `ftps:` to be treated as WHATWG-special or strip a guessed default port; generalizing from URI syntax could cause browser repair to rewrite `sftp:`.
- **Consequence:** Scheme-specific behavior changes without an explicit product decision.
- **Required correction:** Add exact FTPS/SFTP fixtures for web/general admission, both standards, default-port stripping, and fixer behavior.

### Medium 10 — Point-in-time and stale documentation are not clearly quarantined

- **Protocol section:** §2 authority ordering and §10 red-team review.
- **Exact evidence:** ADR 0007 describes a pre-general scope and is superseded by ADR 0012. URL-selector PRDs retain “Draft for review” headers despite prose describing durable/shipped decisions. `vignettes/url-standard.Rmd` still says the selector does not expand beyond the old web schemes, and several source/test comments call general selectors inert or internal although they are public and tested.
- **Example:** A drafter following prose without date/status classification could use the vignette or ADR 0007 to deny current general-scheme support.
- **Consequence:** The evidence synthesis can import stale constraints or present current behavior as accidental.
- **Required correction:** Require every evidence item to state temporal scope and supersession, and add stale public/source documentation to the implementation cleanup ledger.

## Contradictions and open owner decisions

These decisions must not be inferred by the drafting agent.

| ID | Contradiction or decision | Current evidence | Required owner decision |
|---|---|---|---|
| O1 | v3 default admission | Accepted/shipped v2 keeps the curated `web` set by default and makes `general` opt-in; the v3 protocol permits intentional compatibility breaks | Preserve the opt-in model, make general admission the v3 default, or introduce another default; state migration and diagnostics |
| O2 | Direct selectors versus profiles | Current direct `url_standard = "whatwg"` and `profile = "whatwg"` differ on scheme-less input; profiles are bundled defaults with explicit overrides | Preserve both surfaces and document the difference, tighten profiles, or collapse one surface |
| O3 | Generic authority credentials | Protocol demands explicit/reassemblable credential state; the general in-tree authority parser currently drops returned userinfo | Preserve full parsed credentials, reject them, or retain them only in an exact serializer; define public/security behavior |
| O4 | Public password accessor parity | `get_user()`/`get_userinfo()` accept standard/admission selectors; `get_password()` does not | Add selector parity, intentionally keep password web-only, or replace the credential accessor model |
| O5 | `mailto:` accessor rawness | `get_user()` documentation says first local-part output is raw/exact; `.mailto_first_recipient_parts()` percent-decodes it | Specify raw, decoded-once, or dual raw/decoded fields and align docs/tests |
| O6 | Unresolved email lexer contexts | Accepted PRD requires `indeterminate` and no invented recipients; shipped splitter does not expose unclosed quote/bracket state | Implement the accepted contract, replace it with a different recovery rule, or explicitly supersede the PRD |
| O7 | URL-level email facts | Accepted PRD names `userinfo-form`, `public-suffix-known`, and `smtp-envelope-wire-mode`; shipped closed vocabulary omits them | Add them, keep them only in the recipient helper under new names, or supersede that PRD section |
| O8 | Resolution standard | Current resolver uses RFC-style merge before parse/clean; protocol has no v3 position | Choose RFC resolution for all accepted bases, standard-specific resolution, or restrict supported bases/shapes |
| O9 | Clean URL versus exact serialization | Current clean output intentionally applies canonical policies and may omit credentials/fragment, while parsed state aims to preserve exact distinctions | Define separate exact serializer and clean presenter, their credential policy, and which public API exposes each |
| O10 | Diagnostic breadth | ADR 0012 says selected facts, not exhaustive scheme validation | Preserve selected facts, define a comprehensive validator, or create a separate conformance API; absence semantics must be explicit |
| O11 | Generic DNS/PSL eligibility | Current design avoids guessing that arbitrary authority reg-names are DNS but explicitly carves out `mailto:` recipient domains | Preserve the carveout, add declared DNS-like schemes, or add an explicit host-kind declaration API |
| O12 | FTPS and SFTP v3 posture | Accepted current decision is FTPS web-grandfathered and SFTP general-only, with no guessed default ports | Confirm preservation or explicitly revoke it as a v3 compatibility break |

## Required fixture additions

The following fixture families should become required protocol outputs. Each row needs expected parsed state, public fields, exact serialization, cleaned serialization, status/warnings, diagnostics, and accessor/helper results where applicable.

| Fixture family | Minimum cases and assertions |
|---|---|
| AXIS-01 admission × standard | Run `http`, `ftp`, `ftps`, `file`, `ws`, `mailto`, `sftp`, and `foo` through default web, explicit web, and general with WHATWG/RFC; assert admission and parser route independently |
| AXIS-02 direct call × profile | Same scheme-less and scheme-relative inputs under direct standard selectors and `browser`, `whatwg`, `rfc-syntax`, `seo`/`canonical`; assert bundled defaults and explicit overrides |
| STATE-01 non-special shapes | `foo:opaque`, `foo:/a`, `foo://host/a`, `foo:///a`, plus empty path/query/fragment variants; assert presence flags and exact reassembly |
| STATE-02 present-empty components | Distinguish absent, present-empty, and non-empty authority, port, path, query, fragment, username, and password where grammar permits |
| CRED-01 credential routes | `foo://u:p@host/a`, `file://u@host/a`, `ws://u:p@host/a`, `user@host`, `mailto:user@host`; assert parsing, accessor fields, warnings, facts, clean and exact serialization |
| CRED-02 delimiter recovery | Multiple raw `@`, encoded `%40`, empty username/password, username-only, colon in password, and malformed percent escape under each relevant standard |
| MAIL-01 positional splitting | Raw comma, `%2C`, quoted comma, domain-literal comma, encoded quotes/brackets, multiple recipients, empty positional elements, and `?to=`/`?cc=`/`?bcc=` hfields remaining out of positional scope |
| MAIL-02 unresolved contexts | Unterminated quoted string, unterminated domain literal, escaped final character, and ambiguous raw delimiter; assert no invented recipient and explicit indeterminate outcome |
| MAIL-03 addr-spec grammar | Missing `@`, repeated raw `@`, encoded `%40`, quoted local part, dot-atom edges, domain literal, U-label/A-label, Unicode local part, malformed percent sequences, and decode-exactly-once assertions |
| MAIL-04 recipient projections | First positional recipient through `get_host/domain/tld/subdomain/user/userinfo`; all recipients through `get_mailto_recipients()`; assert first-versus-all boundary and raw/decoded contract |
| MAIL-05 PSL/IDNA | Known/unknown suffixes, private/ICANN modes, A-label/U-label, domain literal, single-label domain, and non-DNS domain; assert the explicit `mailto:` carveout only |
| MAIL-06 SMTP wire tier | Disabled sentinel, ASCII envelope, Unicode local part requiring SMTPUTF8, A-label projection, 64/65-octet local part, 256/257-octet forward path, derived mailbox-length boundary, and not-applicable cases |
| MAIL-07 cardinality/vectorization | Non-`mailto:` zero rows, default-web rejected `mailto:` zero rows, empty recipient list, `NA`, vector inputs, duplicate recipients, row order, and input-index provenance |
| DIAG-01 selection versus validity | Exact closed vocabulary and fact cardinality under web/whatwg, general/whatwg, general/rfc; assert that missing facts do not certify validity |
| PORT-01 scheme ports | HTTP(S), FTP, WS/WSS default stripping; FTPS/SFTP no guessed default; file port rejection; arbitrary numeric port preservation or rejection by grammar |
| FIX-01 browser repair | Known special candidates, semicolon/slash repair, scheme inference, unknown/arbitrary schemes, FTPS/SFTP, and `mailto:`; assert ordered repair and no unintended rewrite |
| RESOLVE-01 general bases | Hierarchical arbitrary base, opaque base, WS/file/mailto bases, absolute arbitrary reference, scheme-relative reference, empty query/fragment, dot segments, credentials, and standard-specific expected algorithm/error |
| DOC-01 traceability | For every scheme/dial row, link accepted design, code symbol, test, public documentation, and explicit v3 disposition; stale/superseded sources must be marked |

## Files, symbols, and tests checked

### Proposed protocol and durable design evidence

- `_scratch/url-v3-spec-reconstruction-protocol.md` — especially §§2, 5, 6.1–6.6, 7, 9–12.
- `design/adr/0006-url-diagnostic-warnings.md` — diagnostics are companion facts, not rejection by default.
- `design/adr/0007-url-standard-selection.md` — historical opt-in selector scope; superseded/expanded by ADR 0012.
- `design/adr/0010-scheme-policy.md` — `scheme_policy` and scheme-relative handling are independent.
- `design/adr/0012-general-url-parser-scope.md` — D1 scheme classes, D2 operation eligibility, D3/D4 admission/defaults, D5 facts, D6 profiles, D7 `mailto:` projections, standing rules, resolved FTPS/SFTP questions.
- `design/prd/email-userinfo-diagnostics.md` — accepted mailto/userinfo lexer, grammar, PSL/IDNA, wire, cardinality, and public API contract.
- Browser fixer PRD under `design/prd/` — repair sequence, supported scheme posture, and unknown-scheme non-rewrite.
- URL selector PRDs under `design/prd/` — point-in-time selector/profile intent and status-label inconsistencies.
- `ARCHITECTURE.md`, `CLAUDE.md`, `FP_CLAUDE.md`, `NAMESPACE`, `NEWS.md`, `README.md`, and `vignettes/url-standard.Rmd`.

### Implementation symbols

- `R/utils.R`: `.SUPPORTED_SCHEMES`, `.WHATWG_SPECIAL_SCHEMES`, `.SPECIAL_AUTHORITY_SCHEMES`, `.DEFAULT_PORTS`, `.blank_to_na()`.
- `R/status-constants.R`: status/warning vocabulary, including scheme-less userinfo warning behavior.
- `R/profiles.R` and `R/parse.R`: public option matching, profile table, explicit-standard requirement for general admission, Stage A/B wiring, serializer dispatch, operation eligibility, scheme-less userinfo handling.
- `R/parse-state.R`: state enums, `.stage_b_eligibility()`, `.split_authority()`, `.parse_opaque_url_one()`, `.parse_rfc_file_one()`, `.general_parsed_mask()`, `.general_parse_vec()`.
- `R/accessors.R`: `get_scheme()`, `get_host()`, `get_domain()`, `get_tld()`, `get_subdomain()`, `get_user()`, `get_password()`, `get_userinfo()`.
- `R/email-diagnostics.R`: lexer/token roles, recipient and addr-spec splitters, first-recipient projection, wire tier, `get_mailto_recipients()`, `.email_recipient_facts()`.
- `R/diagnostics.R`: `.URL_DIAGNOSTICS`, generic and scheme-specific fact selection/gating.
- `R/resolve.R`: reference merge, parse/clean handoff, and public `resolve_url()` behavior.

### Tests

- `tests/testthat/test-general-acceptance.R` — explicit-standard rule, arbitrary shapes, WHATWG/RFC differences, mailto, generic PSL restrictions, helpers, WS/WSS.
- `tests/testthat/test-scheme-acceptance.R` — admission decoupling and current default behavior.
- `tests/testthat/test-url-standard-scheme-class.R` — special/non-special and FTPS behavior.
- `tests/testthat/test-url-standard-authority.R` — authority parsing behavior.
- `tests/testthat/test-url-standard-file.R` — WHATWG/RFC file behavior and RFC userinfo extension.
- `tests/testthat/test-ws-wss-inertness.R` — default rejection/general opt-in behavior, despite stale naming/comments.
- `tests/testthat/test-email-diagnostics.R` — delimiter provenance, grammar, IDNA/PSL, wire sentinels/limits, vectorization, and accessor projections; notably no unresolved-context indeterminate fixture.
- `tests/testthat/test-url-diagnostics.R` — exact closed vocabulary and selected WS/mailto/tel/data/file facts.
- `tests/testthat/test-resolve-url.R` — RFC-style reference merge and standard pass-through; limited general/opaque coverage.

## Required protocol disposition

Before the reconstruction proceeds to a normative draft, the protocol owner should add the scheme matrix, dial lattice, credential route matrix, email diagnostic contract table, and resolution layer; assign owners for O1–O12; and require the fixture families above. Without those amendments, schemes/email/userinfo remain a high-risk area where a coherent-looking v3 specification can contradict both accepted design and shipped behavior.
