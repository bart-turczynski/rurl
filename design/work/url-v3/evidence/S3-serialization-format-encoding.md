# S3 evidence review: serialization, formatting, source spelling, and encoding

## Executive verdict

**Verdict: the reconstruction protocol has the correct conceptual layers, but is not yet sufficient to reconstruct precise, separate contracts for all six requested outputs.** It explicitly separates parse records, standards serialization, formatting, cleaning, and keys (`_scratch/url-v3-spec-reconstruction-protocol.md:122-175,206-237`), and it requires conflicts to remain visible (`:48-54,98-106`). That is a sound outline. It does not, however, require the byte/state schema, operation definitions, component-level display matrix, or dial-to-output matrix needed to turn those headings into unambiguous normative contracts.

Two corrections are blockers before normative drafting:

1. Replace “lossless or faithful serialization” with separately named source reproduction and standards serialization contracts, including the admitted R string/byte model.
2. Require standards serializers to consume a lossless parse record containing authority, credential, delimiter-presence, source, and repaired-state distinctions. The shipped serializer seams cannot satisfy the protocol's stated fragment/credential requirements.

The evidence below distinguishes:

- **Shipped behavior**: current `R/` code and current tests.
- **Accepted design**: accepted ADRs, especially ADR 0011 and ADR 0012.
- **Proposed v3 direction**: the reconstruction protocol only; it is not treated as an accepted product decision.

## Coverage map

| Contract to reconstruct | Protocol coverage | Evidence-backed assessment |
|---|---|---|
| Parsing representation | Partial | Section 6.1 names original input, structural state, empty-vs-absent, and source spelling, but the durable artifacts do not require a field-by-field record schema or transform provenance. |
| Standards serialization | Partial, blocked on precision | Sections 6.2-6.3 require distinct RFC/WHATWG serialization and preservation of credentials/fragments/empty delimiters, but do not define the serializer's input record, operation/API, normalization boundary, or exact component encode sets. |
| Readable formatting | Insufficient | Section 6.3 names `format_url()` and hazard classes, but supplies no normative decode/escape matrix or exact output for hazards. No shipped `format_url()` exists (`NAMESPACE`; no implementation under `R/`). |
| Current `clean_url` | Partial | Section 6.4 correctly classifies it as intentionally lossy SEO output. It does not require a complete current-default and knob-effects matrix, which is necessary because the shipped value is also used by resolution and joining. |
| Source spelling | Insufficient | Section 6.1 names the distinction, but there is no separately named source-reproduction operation, byte/`Encoding()` guarantee, span/provenance schema, or rule for repaired inputs. |
| Equality/keying | Partial | Section 6.5 correctly proposes a non-URL `get_url_key()` independent of display/cleaning and calls out `canonical_join()`. Exact key representation, equivalence defaults, encoding, and migration from shipped `clean_url` keys remain open by design. No shipped `get_url_key()` exists. |

## Severity findings

### BLOCKER S3-F1 — “Lossless or faithful” conflates source reproduction with standard serialization

**Protocol section:** 6.1, 6.3, 7 (`url-v3-spec-reconstruction-protocol.md:124-129,142-151,208-237`).

**Exact evidence:**

- The proposed v3 text says “Lossless **or** faithful serialization guarantees by standard” (`:144`) while separately naming source spelling (`:127`). “Or” leaves two different operations under one contract.
- **Shipped:** `original_url` preserves the supplied string bytes, but all returned non-ASCII character components, including `original_url`, are declared UTF-8 without transcoding (`R/parse.R:765-805`; `tests/testthat/test-locale-invariance.R:3-18,44-65,98-121`). Thus byte preservation and preservation of R's `Encoding()` label are already different guarantees.
- **Shipped:** WHATWG preprocessing strips TAB/LF/CR before parsing (`tests/testthat/test-url-standard-control-chars.R:1-35`), and path extraction is from the **prepared** input (`R/parse-phases.R:1335-1380`). The original source and the parsed component therefore legitimately differ.
- **Accepted design:** ADR 0012 says an opaque path is not byte-verbatim input after WHATWG preprocessing (`design/adr/0012-general-url-parser-scope.md:293-299`).

**Byte/character example:** for `http://ex<TAB>ample.com/a%2fb?#`, the source includes byte `09`; the shipped WHATWG parse succeeds with host `example.com`, path `/a%2fb`, and `original_url` still contains `09`, while RFC parsing errors. Source reproduction, parsed state, and standards output cannot be the same string contract.

**Consequence:** a v3 author can satisfy the present checklist while using “serialization” either for exact source spelling or for a normalized standard output. That makes round-trip, mutation, equality, and repair guarantees unverifiable.

**Concrete protocol correction:** require three separately named operations and fixture columns:

1. `source_string(record)` (working name): reproduces admitted input bytes when the record is unmutated; state whether it preserves or normalizes the R `Encoding()` label.
2. `serialize_url(record, standard=...)`: emits the standard's serialization from parsed state; it is not promised to reproduce source spelling.
3. `format_url(record, ...)`: display-only output with no round-trip or identity guarantee.

Also require the v3 PRD to define the admitted input model for R character values (`Encoding()` values, directly written non-ASCII, percent-encoded octets, and malformed/invalid literal byte sequences) and define “byte-for-byte” as an explicit comparison operation, not informal prose.

### BLOCKER S3-F2 — The protocol does not require a serializer-ready lossless state record

**Protocol section:** 5.2, 6.1-6.3, 7 (`:82-96,122-151,208-237`).

**Exact evidence:**

- The ledger schema records decisions, but it does not require a normative per-field parse-record schema (`:84-94`).
- **Shipped:** the public 18-field result collapses empty query, fragment, user, and password to `NA` (`R/parse.R:292-307`; `.blank_to_na()` at `R/utils.R:199-207`). The cached Stage A fields contain raw values but no `authority_kind`, credential delimiter kinds, source spans, or repair provenance (`R/utils.R:243-285`). Stage B reparses original input to recover general-parser kinds because those kinds were not threaded through the cache (`R/parse.R:1932-1954`).
- **Shipped:** `.serialize_whatwg_vec()` and `.serialize_rfc_generic_vec()` accept scheme/host/path/query state and port, but no fragment, username, password, userinfo kind, or authority kind (`R/parse-phases.R:2445-2461,2548-2564`). Both explicitly implement a `clean_url` contract with fragments excluded (`:2422-2436,2578-2588`). Unit tests pin “FRAGMENT excluded” and “never a fragment” (`tests/testthat/test-parse-serializers.R:4-9,73-88`).
- **Accepted design:** ADR 0012 requires `authority_kind`, `query_kind`, and `fragment_kind`, and says standard serializers retain trailing `?` and `#` (`design/adr/0012-general-url-parser-scope.md:228-268,623-634`). This is not the complete shipped serializer state.

**Byte/character example:** a read-only current-code probe of `HTTP://user:pass@Ex.com:80/a/../b?#` under both explicit standards with `query_handling="keep"` returned `clean_url == "http://ex.com/b"`, `user == "user"`, `password == "pass"`, and `query`/`fragment == NA`. The credential bytes, `?`, and `#` are not available to the shipped serializer call.

**Consequence:** the required v3 fragment/credential/empty-delimiter serialization cannot be reconstructed from the shipped public record or the current serializer signature. An author could accidentally serialize Stage-B display/cleaning values, reparse `original_url`, or omit state, all with different results.

**Concrete protocol correction:** make a “serializer input record” artifact mandatory. At minimum require:

- source input and admitted-byte metadata;
- selected standard and parse/repair provenance;
- scheme payload plus source/normalized spelling;
- `authority_kind` and `host_kind` separately;
- host form and source/parsed/canonical host spellings;
- username, password, raw userinfo, `userinfo_kind`, and `password_delimiter_present`;
- port presence/source/value;
- posture-specific path state and source/parsed payload;
- `query_kind`, `fragment_kind`, and source payloads;
- mutation state.

Require each standard serializer to consume this record, never the public 18-field projection or formatted/cleaned components. Add full-string fixtures asserting both serialized bytes and parse→serialize→parse structural equivalence.

### HIGH S3-F3 — `format_url()` safety is a topic list, not an executable contract

**Protocol section:** 6.3 and 7 (`:142-151,208-237`).

**Exact evidence:**

- The protocol names safe decoding, structural delimiters, invalid UTF-8, controls, bidi controls, and invisible characters, but gives no required output for any of them (`:146-151`).
- **Shipped:** `path_encoding="decode"` full-decodes the path through `curl_unescape()` (`R/parse-phases.R:1396-1429`); `/a%2Fb/c` becomes `/a/b/c` under the tested WHATWG decode path (`tests/testthat/test-url-standard-path-encoding-orthogonal.R:181-193`).
- **Shipped:** `get_query()` defaults to whole-string percent-decoding for readability (`R/accessors.R:580-600,617-637`), so percent-encoded query delimiters can become visually structural even though the raw query is still distinct.
- No shipped `format_url()` implementation or tests exist.

**Byte/character examples:**

- Path `/a%2Fb` must not display as `/a/b` if the output claims to preserve visible structure.
- Query `?x=a%26b%3Dc` must not display as `?x=a&b=c` without an unambiguous escaping/annotation rule.
- `%E2%80%AE` (U+202E RIGHT-TO-LEFT OVERRIDE), `%0A`, `%00`, and `%FF` require exact visible outputs, not only a statement that they are “handled.”
- Host `xn--mnchen-3ya.de` versus `münchen.de` needs an exact ACE/Unicode and spoof-safety rule.

**Consequence:** independent implementations can all claim “safe display decoding” while producing different, structurally misleading, or non-copyable strings. Tests cannot be derived from the current prose.

**Concrete protocol correction:** require a component-by-component format matrix with columns: input bytes/source spelling, parsed code points, validity, decode eligibility, exact displayed string, hazard annotation/escaping, returned `Encoding()`, and whether copying the display is intended to be reparsable. Include separate policies for host, username/password, path segments, query keys/values, and fragment. State that display output is never fed into serialization, mutation baselines, or keys.

### HIGH S3-F4 — Query lexical spelling is not covered deeply enough for source or standards serialization

**Protocol section:** 6.1, 6.3, 6.4, 7 (`:124-129,142-158,221-235`).

**Exact evidence:**

- The protocol requires absent/present-empty query and encoded `%26`/`%3D`, but not the lexical distinctions inside a non-empty query.
- **Shipped:** the cleaning engine splits on literal `&`, skips empty segments, splits on the first `=`, and maps a bare key to an empty value (`R/path-query.R:351-379`). Its canonical rendering always emits `key=value` and escapes embedded delimiters (`:508-541`). Tests pin `?a` as equivalent to `?a=`, removal of `&&`, plus handling, and malformed-percent pass-through (`tests/testthat/test-query-engine.R:9-28,104-129`).
- A current-code probe observed `https://e/?a` and `https://e/?a=` both clean to `https://e/?a=`, and `https://e/?a=1&&b=2` cleans to `https://e/?a=1&b=2` when `query_handling="keep"`.

**Byte/character example:** `?a`, `?a=`, `?a=1&&b=2`, `?q=a+b`, `?x=a%26b%3Dc`, `?a=%`, and bare `?` are distinct source spellings even when a cleaning/query-pair policy intentionally equates some of them.

**Consequence:** if the query-pair engine becomes the substrate for standards serialization or source reproduction, lexical information is irreversibly lost. If the raw string is used, cleaning semantics are not represented. The present protocol does not explicitly prevent this cross-layer reuse.

**Concrete protocol correction:** require a query fixture grammar and state table covering delimiter presence, bare-key versus equals-present, empty pairs, ordering, duplicates, literal plus, percent triplet case, encoded delimiters, and malformed `%`. Normatively require standards/source serialization to use the lossless query state, while query pair parsing/filtering remains a cleaning or mutation view only.

### HIGH S3-F5 — Credential reassembly needs delimiter state, not only `user` and `password`

**Protocol section:** 6.1 and 6.3 (`:126-129,144-150`).

**Exact evidence:**

- The protocol asks for “credentials and reassemblable userinfo” but does not enumerate the states that make it reassemblable.
- **Shipped:** empty user/password values are collapsed through `.blank_to_na()` (`R/utils.R:199-207`; `R/parse.R:1688-1701`). `get_userinfo()` emits `user` alone when password is absent **or empty**, and returns `NA` when no non-empty user exists (`R/accessors.R:895-916`).
- **Shipped:** selector preprocessing uses the last `@` as authority delimiter and percent-encodes earlier `@` bytes (`R/parse-phases.R:294-323`); RFC and WHATWG differ on repeated raw `@` (`tests/testthat/test-general-acceptance.R:51-66`).
- Current serializers take no credential arguments (S3-F2).

**Byte/character example:** `https://u@e/`, `https://u:@e/`, `https://:p@e/`, `https://@e/`, `https://u:p:q@e/`, and `scheme://username@@@@example.com` cannot be reconstructed from non-empty `user`/`password` values alone. A current-code probe returned `get_userinfo("https://u:@e/") == "u"`, losing the colon.

**Consequence:** “reassemblable” is untestable, and a standards serializer could silently delete or invent credential delimiters.

**Concrete protocol correction:** require `authority_kind`, `userinfo_kind`, `username_kind`, `password_kind`, `password_delimiter_present`, raw userinfo spelling, parsed username/password values, and repair provenance. Add exact full-string cases for every example above under both standards and the explicit repair posture.

### HIGH S3-F6 — Current presentation/identity behavior contains an unregistered accepted-design-versus-code conflict

**Protocol section:** 5.3, 6.3-6.5, 9 (`:98-106,142-175,266-276`).

**Exact evidence:**

- **Accepted design:** ADR 0011 says both `path_encoding="encode"` and `"decode"` first collapse reserved `%2F` to `/`, calling both lossy (`design/adr/0011-path-encoding-orthogonal-presentation.md:32-41,78-90`).
- **Shipped:** `.normalize_path_vec()` has a WHATWG-specific `encode` branch that does **not** full-decode (`R/parse-phases.R:1396-1429`), and the WHATWG byte encoder preserves every existing `%xx` spelling (`R/path-query.R:206-269`). A current-code probe produced:

  - no selector: `encode` → `http://ex.com/a/b/c`;
  - RFC 3986: `encode` → `http://ex.com/a/b/c`;
  - WHATWG: `encode` → `http://ex.com/a%2Fb/c`;
  - WHATWG: `decode` → `http://ex.com/a/b/c`.

- **Accepted design:** ADR 0012 says lossy path presentation never feeds the standard serializer used for identity comparisons (`design/adr/0012-general-url-parser-scope.md:275-283`).
- **Shipped:** `path_encoding` feeds the public path and `clean_url` (`R/parse.R:1961-1974,2058-2063`), and `canonical_join()` uses `parsed$clean_url` as its key (`R/canonical_join.R:238-247`). Tests intentionally pass `path_encoding="encode"` through joining (`tests/testthat/test-url-standard-path-encoding-orthogonal.R:204-216`).

**Byte/character example:** `/a%2Fb/c` has four current outcomes across standard/presentation combinations above; `https://ex.com/école` and `https://ex.com/%C3%A9cole` join when the current presentation knob collapses their spellings.

**Consequence:** the reconstruction ledger could copy either accepted prose or shipped behavior and incorrectly call it current truth. This directly affects whether a value is display, standards output, or equality input.

**Concrete protocol correction:** seed a mandatory contradiction-register entry for ADR 0011 versus the shipped WHATWG encoder and require an owner decision. Require the final dial matrix to state, for every standard × `path_encoding` combination, whether the knob affects parsed identity, standards serialization, `format_url`, `clean_url`, and `get_url_key`. Preserve shipped `canonical_join` behavior as a migration fact, not as the v3 equality contract.

### MEDIUM S3-F7 — Output encoding marks and invalid-byte behavior are not part of the required artifact schema

**Protocol section:** 6.3, 6.8, 7 (`:142-151,221-237`; the protocol's vector/error scope also requires deterministic behavior but does not name R encoding metadata).

**Exact evidence:**

- **Shipped:** all returned character components are passed through `.mark_result_utf8()`; non-ASCII outputs have `Encoding() == "UTF-8"`, while pure ASCII remains `"unknown"` because R will not retain an encoding mark on ASCII (`R/parse.R:786-805`; `tests/testthat/test-locale-invariance.R:3-18,42-93`). Byte-level tests pin UTF-8 octets (`test-locale-invariance.R:96-121`).
- **Shipped:** the WHATWG component encoder declares bytes UTF-8, uses `charToRaw()`, encodes non-ASCII/C0 bytes, and preserves existing percent spellings (`R/path-query.R:206-260`).
- The protocol mentions invalid UTF-8 in display but does not require output `Encoding()` marks or distinguish literal invalid bytes from an ASCII percent spelling such as `%FF`.

**Byte/character example:** literal `é` is bytes `C3 A9` and returned UTF-8-marked; `%C3%A9` is seven ASCII bytes until decoded; `%FF` is three valid ASCII source bytes but denotes a non-UTF-8 octet if interpreted. These require distinct rules.

**Consequence:** implementations can agree visually while differing in `identical()`, joins, locale behavior, and raw bytes. The shipped locale-invariance contract could be lost during v3 separation.

**Concrete protocol correction:** add `raw_bytes`, Unicode scalar interpretation, `Encoding()` on input/output, invalid-sequence policy, and locale invariance to every serialization/format fixture. Require exact `charToRaw()` and `Encoding()` assertions for non-ASCII outputs and explicitly state the ASCII-mark rule.

## Contradictions and open decisions

| ID | Classification | Competing claims/evidence | Required decision |
|---|---|---|---|
| S3-C1 | **Accepted design vs shipped implementation** | ADR 0012 says standard serializers retain trailing `#` (`0012:262-268`); shipped serializers and their unit tests explicitly exclude fragments (`parse-phases.R:2432-2436,2578-2588`; `test-parse-serializers.R:4-9,73-88`). | Decide whether this is unfinished ADR implementation or superseded design. V3's proposed separate standards serializer should not silently inherit `clean_url`'s omission. |
| S3-C2 | **Accepted design vs shipped implementation** | ADR 0011 says WHATWG `path_encoding="encode"` is lossy for `%2F`; shipped WHATWG encode preserves `%2F` (S3-F6). | Choose the compatibility behavior for current `clean_url`; separately prohibit this knob from controlling the v3 standards serializer. |
| S3-C3 | **Accepted design / shipped naming fork** | ADR 0012 distinguishes source-preserving `rfc-syntax` from the historically normalized direct `url_standard="rfc3986"` (`0012:167-185`). The protocol says “RFC” without requiring which posture/name is the v3 serialization contract. | Name and define the RFC syntax serializer, RFC normalization operations, and migration behavior separately. |
| S3-C4 | **Shipped behavior vs proposed v3 separation** | `clean_url` excludes credentials/fragments and is allowed to be non-URL-safe under decoded presentation (`R/parse.R:325-336`); proposed v3 requires full standards serialization and standards-valid cleaning (`protocol:144-157`). | Define whether current `get_clean_url()` defaults remain byte-compatible, which lossy outputs remain legal, and whether a new cleaner replaces or wraps it. |
| S3-C5 | **Shipped behavior vs proposed v3 equality** | `canonical_join()` keys on `clean_url` (`R/canonical_join.R:238-247`); v3 proposes an independent non-URL `get_url_key()` (`protocol:165-175`). | Specify default equivalence, key encoding/type, invalid-row behavior, and a compatibility/migration path for existing joins. |
| S3-C6 | **Open v3 display policy** | Protocol lists controls/bidi/invisibles but no exact representation. No current `format_url()` exists. | Choose exact escaping/annotation rules, Unicode-host policy, and whether display is copyable/reparsable. |
| S3-C7 | **Open source contract** | `original_url` preserves bytes but parsing may use repaired/prepared text and returned encoding marks may change. | Decide whether v3 promises source-byte reproduction, R encoding-label reproduction, both, or a bounded subset; name the operation. |

## Output/encoding dial inventory

The protocol should require this inventory to be converted into a normative **function × dial × layer** matrix. The following is current shipped behavior, not a proposed v3 design.

| Current dial/output | Values/default | Shipped effect and seam |
|---|---|---|
| `url_standard` | `NULL`, `rfc3986`, `whatwg`; default `NULL` | Selects parser/normalization posture and some component encoders; currently also influences `path`, `query`, `fragment`, and `clean_url`. |
| `scheme_acceptance` | `web`, `general`; default `web` | Changes admitted shapes and serializer routing; `general` requires an explicit standard. |
| `scheme_policy`, `scheme_relative_handling`, `protocol_handling` | infer/require; keep/http/https/error; keep/none/strip/http/https | Mix parse admission, scheme inference/repair-like behavior, and cleaned scheme presentation. Must be assigned per v3 layer. |
| `host_encoding` | keep/idna/unicode; default keep | Presentation spelling used for returned host/derived names and `clean_url`; must not determine standards serialization or equality unless a separate policy says so. |
| `path_encoding` | keep/encode/decode; default keep | Presentation currently feeds returned `path`, `clean_url`, and therefore `canonical_join`; behavior differs for WHATWG encode (S3-F6). |
| `query_handling` plus `params_keep/drop`, `sort_params`, `empty_param_handling`, `params_case_sensitive`, `decode_plus` | drop/filter/allow/keep; parser/clean default drop | Filters and canonicalizes query for `clean_url`; raw result query is separate. Pair parsing loses bare-equals and empty-pair spelling. |
| `get_query(format, decode)` | string/list; decode default true | Accessor-only representation; the default can decode percent-encoded structural delimiters. It is not a safe model for future whole-URL formatting. |
| `port_handling` | exclude/keep/strip_default/strip_all; default exclude | Changes returned port/cleaned output and current join keys. |
| Cleaning dials | `www_handling`, `subdomain_levels_to_keep`, `case_handling`, `trailing_slash_handling`, `index_page_handling`, `path_normalization` | Change returned components and/or `clean_url`; several are intentionally lossy and currently flow into equality through `canonical_join`. |
| `profile` | bundle or `NULL` | Expands multiple settings and can override default interpretation. A v3 contract must show the resolved values, not only the profile name. |
| `tld_source`/accessor `source`, `engine` | PSL/engine selection | Annotation/backend controls, not serialization, but currently share parser entry points and cache seams. |
| `original_url` | result field | Source string bytes retained; non-ASCII returned value is UTF-8-marked with other character fields. Not a parsed-state serializer. |
| `clean_url` | result/accessor | Current lossy scheme/host/path product; query opt-in, port opt-in, fragment/userinfo always out; may be human-readable/non-URL-safe with decoded path. |
| `resolve_url()` | public output | Returns canonical `clean_url`, not faithful resolved recomposition; docs explicitly say userinfo/fragment are excluded (`R/resolve.R:160-203`). |
| `canonical_join()` | current key consumer | Uses `clean_url`; all applicable parse/presentation/cleaning dials forwarded through `...`. |
| `format_url()` | proposed only | No shipped API. Must receive its own decode/safety/host-display controls rather than inherit `path_encoding` or `get_query(decode=TRUE)` wholesale. |
| `get_url_key()` | proposed only | No shipped API. Must receive explicit equivalence policies and return a non-URL representation independent of serialization/display/cleaning. |
| R output encoding | implicit shipped rule | Non-ASCII returned character values are marked UTF-8; ASCII remains `Encoding() == "unknown"`; byte values are locale-invariant. This needs an explicit v3 invariant. |

Current formal inventory is visible at `R/parse.R:431-469`, `R/accessors.R:232-252,534-572,617-643`, and `R/canonical_join.R:45-60`.

## Minimum protocol amendments before drafting

1. Add mandatory artifacts for a lossless parse-record schema, transform/provenance trace, exact public-output/dial matrix, and component display-safety matrix.
2. Split source reproduction, standards serialization, readable formatting, SEO cleaning, and equality keying into separately named operations with separately testable invariants.
3. Require every example row to contain exact source bytes/mark, parsed state, repaired/prepared state, serialized bytes/mark, formatted text, cleaned text, and key projection—not only prose answers.
4. Seed contradictions S3-C1 through S3-C7 into the contradiction register rather than waiting for the successor PRD to expose them.
5. Add fixture families for credential delimiter state, query lexical state, reserved delimiters, invalid UTF-8/percent octets, controls/bidi/invisibles, Unicode/ACE hosts, and every standard × presentation combination.

With those amendments, the protocol can support precise reconstruction. Without them, it can support discovery and review but not an unambiguous normative specification.

## Files and symbols checked

### Required guidance and protocol

- `_scratch/url-v3-spec-reconstruction-protocol.md` (complete)
- `CLAUDE.md` (complete)
- `FP_CLAUDE.md` (complete)
- `ARCHITECTURE.md` (complete)

### Core implementation

- `R/parse.R`: `safe_parse_url()`, `safe_parse_urls()`, `._parse_stage_a_vec()`, `._parse_stage_b_vec()`, `.mark_host_utf8()`, `.mark_result_utf8()`
- `R/parse-state.R`: `.host_kind()`, `.authority_kind()`, `.presence_kind()`, `.rfc_path_form()`, general/opaque parse state
- `R/parse-phases.R`: WHATWG preprocessing/repairs, `.prepare_urls_for_curl_vec()`, `.parse_with_curl()`, `.extract_raw_path_vec()`, `.extract_raw_components()`, `.normalize_path_vec()`, `.build_clean_url_vec()`, `.serialize_whatwg_vec()`, `.serialize_rfc_generic_vec()`
- `R/path-query.R`: RFC percent normalization, WHATWG component/path/query/fragment encoders, query-pair/filter engine
- `R/accessors.R`: `get_clean_url()`, `get_path()`, `get_query()`, `get_fragment()`, `get_user()`, `get_password()`, `get_userinfo()`
- `R/domain.R`: Unicode/Punycode conversion and sanitation seams
- `R/utils.R`: `.blank_to_na`, `.spu_result_fields`, `.spu_stage_a_fields`
- `R/canonical_join.R`: `.cj_side_state()`
- `R/resolve.R`: component resolution and `clean_url` return contract
- `NAMESPACE`: confirmed absence of shipped `format_url()` and `get_url_key()`

### Tests and fixtures

- `tests/testthat/test-parse-serializers.R`
- `tests/testthat/test-parse-state.R`
- `tests/testthat/test-general-acceptance.R`
- `tests/testthat/test-url-standard-path-encoding-orthogonal.R`
- `tests/testthat/test-url-standard-conformance.R`
- `tests/testthat/test-url-standard-control-chars.R`
- `tests/testthat/test-query-engine.R`
- `tests/testthat/test-query-clean-url.R`
- `tests/testthat/test-encoding-fixtures.R`
- `tests/testthat/test-locale-invariance.R`
- `tests/testthat/test-raw-path.R`
- `tests/testthat/fixtures/encoding-fixtures.csv`
- `tests/testthat/fixtures/url-standard-conformance.csv`

### Accepted decisions and historical PRDs

- `design/adr/0003-parse-present-stage-split.md`
- `design/adr/0005-intentional-base-r-string-exceptions.md`
- `design/adr/0009-whatwg-host-charset-shim.md`
- `design/adr/0011-path-encoding-orthogonal-presentation.md`
- `design/adr/0012-general-url-parser-scope.md`
- `design/prd/url-standard-selector.md`
- `design/prd/url-standard-selector-v2.md`

### Empirical checks

Read-only `pkgload::load_all()` probes were used to confirm exact current output for the representative credential/empty-delimiter URL, query lexical variants, control-character preprocessing, path `%2F` across standard/presentation combinations, `get_userinfo()` empty-password behavior, and returned `Encoding()` marks. These observations agree with the cited code and tests; they are characterization evidence, not proposed v3 decisions.
