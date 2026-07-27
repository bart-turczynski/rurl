## rurl 2.8.0

### Breaking changes

- **`canonical_join()` now warns when a legacy presentation dial is forwarded
  through `...`.** `canonical_join()` matches on the canonicalized presentation
  string (`clean_url`), so a presentation or cleaning dial passed through `...`
  silently moves the comparison key and changes join cardinality — the same
  inputs match a different number of rows because of dials that were never meant
  to be identity inputs. Twenty-one such dials now emit one warning per call, of
  class `"rurl_legacy_join_dial_warning"`. The four input and interpretation
  axes — `url_standard`, `scheme_acceptance`, `scheme_policy` and
  `scheme_relative_handling` — are legitimate inputs to identity and stay
  silent.

  **Results are unchanged.** The warning is purely additive, and byte-identical
  output was verified across ten dial configurations, so no caller is silently
  re-matched. This is flagged breaking only because a new condition is
  *signalled*: code running under `options(warn = 2)`, or asserting with
  `expect_silent()`, will now see an error where it previously saw none. Because
  the condition is classed it can be silenced without hiding other warnings:

  ```r
  suppressWarnings(
    canonical_join(A, B, www_handling = "strip"),
    classes = "rurl_legacy_join_dial_warning"
  )
  ```

  The per-dial classification is taken from the settled `key-affecting?` column
  of the v3 cleaning-mutation contract rather than re-derived. Retaining
  `canonical_join()` with a deprecation window and warnings — rather than
  re-keying it in place — is the ratified disposition (P3.1 Q7/B7).

- **`safe_parse_url()` no longer reports an authority for a `mailto:` URL under
  `scheme_acceptance = "general"`.** A non-special scheme with no `//` is a
  WHATWG *opaque path*, which has no authority — but the `@` in a recipient
  address was re-surfacing as parsed authority, so
  `safe_parse_urls("mailto:a@b.com", url_standard = "whatwg", scheme_acceptance = "general")`
  reported `host = "b.com"`, `user = "a"`, `domain = "b.com"`, `tld = "com"`.
  Those five columns (plus `domain_ascii` / `domain_unicode` / `tld_ascii` /
  `tld_unicode`) are now `NA`, matching WHATWG and adaR. `path` and `clean_url`
  already carried the address verbatim and are unchanged, as is every
  non-`mailto:` opaque row (`tel:`, `data:`, `sc:`), which already reported no
  authority.

  **The accessors are deliberately unchanged.** `get_host()`, `get_domain()`,
  `get_tld()`, `get_subdomain()` and `get_user()` still decompose a `mailto:`
  recipient under `general` — through the same PSL seam a web host uses — as
  shipped in 2.6.0 (ADR 0012 D7). A recipient domain is *extraction metadata
  about an address*, not the URL's authority, so it is surfaced by the
  accessors and by `get_mailto_recipients()` while the parse table stays
  WHATWG-conformant. `safe_parse_url(u)$host` and `get_host(u)` therefore
  disagree for a `mailto:` under `general`, by design — the same independence
  D7 already declared between `get_host()` and `clean_url()`. Only the default
  `scheme_acceptance = "web"` posture is untouched in every respect, since it
  does not parse `mailto:` at all.

### Bug fixes

- **RFC 3986 §6.2.2.2 host normalization no longer depends on the scheme, and
  no longer leaks into the source-preserving form.** Percent-decoding of the
  host happened during the parse, inside a phase gated on rurl's supported
  scheme set, rather than in the RFC serializer's `normalized` branch alongside
  the other four components. Which decoding a host got therefore depended on
  *its scheme* instead of the `form` the caller asked for, and it broke in both
  directions at once:

  ```r
  # before                                                  # after
  serialize_url("foo://ho%2Dst/p", standard = "rfc3986",
                form = "normalized")   #> "foo://ho%2Dst/p" -> "foo://ho-st/p"
  serialize_url("http://ho%41st/p", standard = "rfc3986",
                form = "source")       #> "http://hoAst/p"  -> "http://ho%41st/p"
  ```

  A general-scheme host was the only position in a URL that skipped §6.2.2.2
  (all 132 unreserved triplet spellings survived normalization there, and none
  did anywhere else), while a special-scheme host was the only one that decoded
  in `form = "source"`, which is documented as byte-preserving. Both are fixed:
  §6.2.2.2 now runs in the serializer's `normalized` branch for every scheme,
  and the RFC record takes the host's *source* spelling, recovered lexically
  the way the undivided `userinfo` slice already was.

  Two further consequences. A host triplet that does **not** encode an
  unreserved octet now stays encoded, so `serialize_url("http://ho%7Cst/p",
  standard = "rfc3986")` keeps `%7C` instead of emitting a literal `|` — output
  that no RFC 3986 production admits and that did not re-parse. And `form =
  "source"` is now byte-preserving at the host under both scheme classes, which
  was one of the four places it was not.

  Acceptance is unchanged: the fix is confined to the serializer-input record,
  and an invariance sweep over every octet 0–255 in both hex cases at nine
  component positions across six schemes (36,104 inputs) shows identical
  accepted counts and identical per-row `parse_status` on all four parse
  profiles, plus byte-identical WHATWG serialization and an unchanged 336/336
  on the WHATWG web-platform-tests.

- **A host-less `file:` URL now carries an empty host, not a null one, so
  `serialize_url()` emits the authority the URL Standard requires.** WHATWG's
  *file state* gives every `file:` URL a non-null host — the empty string when
  no authority is written, and `localhost` maps to that same empty string — and
  the serializer therefore always emits `//`. rurl recorded a null host
  instead, so ten inputs serialized without it:

  ```r
  # before                             # after
  serialize_url("file:C|/m/")          #> "file:/C:/m/"   -> "file:///C:/m/"
  serialize_url("file:/example.com/")  #> "file:/example.com/"
                                       #>                 -> "file:///example.com/"
  serialize_url("file:?q=v")           #> "file:/?q=v"    -> "file:///?q=v"
  serialize_url("file:/.//p")          #> "file:/.//p"    -> "file:////p"
  ```

  With this, **`serialize_url(standard = "whatwg")` reproduces the WHATWG URL
  Standard's own recorded `href` on all 336 success rows of the full imported
  web-platform-tests suite (was 326), and still rejects all 202 must-fail
  rows.** No deviation family remains on that oracle. The ten inputs are
  enumerated in `tests/testthat/test-wpt-full-suite.R` rather than counted, so
  a regression has to name itself.

  The defect was in the **parse record**, not the serializer: the fix teaches
  the WHATWG `file:` parser to record the empty host, and the serializer's `//`
  condition moves with it, from the source delimiter fact to the standard's own
  rule — a non-null host (`#concept-url-serializer`). The RFC serializers keep
  the delimiter fact, where the source spelling *is* what they render:
  `file:/example.com/` has no authority under RFC 3986 and its `rfc3986`
  serialization is unchanged.

  **Nothing else moves.** Acceptance is byte-identical (859 accepted of 1460
  probed inputs under `whatwg`, 1045 under `rfc3986`, before and after), both
  RFC forms are unchanged on every row, and the public parse frame —
  `parse_status`, `host`, `path`, `clean_url`, `domain`, `port` — is
  byte-identical under all four profiles. In particular `get_host()` still
  reports `NA` for an empty host: that collapse is a general accessor rule
  (`foo:///bar` reports `NA` too), not part of this defect. (`RURL-uhwivndf`;
  disposition P1.3, superseding P1.2 D-C for the WHATWG serializer.)

- **Under `url_standard = "rfc3986"`, a percent-encoded DEL in the host no
  longer decodes into the parsed host, and host percent-triplets keep uppercase
  hex.** Two defects on the same RFC 3986 §6.2.2 rule, both invisible to the
  existing harnesses because the RFC profile is scored on an *acceptance* axis,
  which cannot see a bad output string on an accepted input.

  libcurl percent-decodes every host triplet, including octets §6.2.2.2 forbids
  decoding (only ALPHA / DIGIT / `-` / `.` / `_` / `~` may be decoded). DEL was
  the one such octet libcurl also *admitted*, so it reached the parsed host as a
  raw control byte:

  ```r
  # before
  serialize_url("http://ho%7Fst/", standard = "rfc3986")
  #> "http://ho\177st/"    <- raw DEL
  serialize_url("http://ho\177st/", standard = "rfc3986")
  #> NA                    <- the serializer's own output does not re-parse

  # after
  serialize_url("http://ho%7Fst/", standard = "rfc3986")
  #> "http://ho%7Fst/"     <- re-parses to itself
  ```

  Besides breaking round-trip and idempotence, decoding the control byte is what
  made an encoded host render as a clean-looking hostname.

  Separately, host case folding lowercased the hex digits of a *surviving*
  triplet, which §6.2.2.1 renders uppercase — the host was the only component
  whose triplets escaped the hex normalization the path, query, fragment and
  userinfo already got. `file://%43%7C` now normalizes to `file://%43%7C`
  rather than `file://%43%7c`.

  **Acceptance is unchanged.** Which URLs the RFC profile accepts or rejects is
  byte-identical, verified over the full 0–255 host-octet range: every other
  control octet is rejected once decoded, and the fix deliberately leaves those
  rows on that path rather than admitting them. Non-control triplets keep the
  established decode contract, so `host_encoding = "idna"` still sees real code
  points. Five conformance-fixture expectations were re-baselined; their
  `rfc3986_expected` cells are a *reading* of the RFC (the imported oracle
  records `failure` with no value for those rows) and had transcribed the
  retained triplets in lowercase, corrected here from the RFC text.

  One defect of the same class remains open and is **not** fixed here: a
  percent-encoded `|` (`%7C`) still decodes to a literal `|`, which is not an
  RFC 3986 reg-name character, so that output does not re-parse either. Its
  decoded spelling is pinned by existing fixtures, so changing it is a separate
  decision.

- **`serialize_url()` now reads the backslash-rewritten source, fixing two
  hostname-confusion defects.** Both shipped with the serializer and were
  invisible to every existing test, because `clean_url` drops userinfo so no
  harness could observe them. The parse record was correct throughout; only the
  serializer's lexical recovery was wrong.

  `.fsss_source_lex()` read the *raw* source where the parser reads the source
  after WHATWG's reverse-solidus rewrite, so a `\` the standard maps to the
  authority/path boundary stayed inside the authority slice. The text before the
  last `@` — which is **path** — was recovered as a userinfo the parser never
  found, and the host was duplicated into the credentials:

  ```r
  serialize_url("http://google.com:80\\@yahoo.com")
  #> was: "http://google.com:80%5C@google.com/@yahoo.com"
  #> now: "http://google.com/@yahoo.com"
  ```

  Separately, `.has_explicit_authority()` greps a literal `://`, which an input
  using WHATWG's special-authority-ignore-slashes state need not contain, so the
  serializer emitted no `//` and **dropped the host** the parser had resolved:
  `serialize_url("https:/\\/\\/\\github.com/foo/bar")` returned
  `"https:/foo/bar"` and now returns `"https://github.com/foo/bar"`.

  Both are confined to the serializer; `.has_explicit_authority()` itself is
  unchanged, so the `mailto:` and Stage-B callers keep their behavior. The
  rewrite is a no-op under `rfc3986`, which admits no raw `\` and rejects these
  inputs outright. Found by the conformance re-baseline (RURL-yeikpnan): every
  affected row is in the CVE-2020-26291 hostname-confusion family, and each now
  agrees with its published oracle.

- **`url_standard = "whatwg"` now applies the userinfo percent-encode set to the
  `user` and `password` columns.** WHATWG's authority state fills its username
  and password buffers by percent-encoding each code point with the userinfo
  percent-encode set — the path set (SP `"` `#` `<` `>` `?` `^` `` ` `` `{` `}`)
  plus `/` `:` `;` `=` `@` `[` `\` `]` `|` — so those are the parsed values the
  standard stores. rurl applied no encode set at all, reporting the raw source
  slice: `safe_parse_urls("http://a^b@host/", url_standard = "whatwg")$user` was
  `a^b` where WHATWG stores `a%5Eb`, and a `:` inside a password was reported
  literally instead of as `%3A`. **This is a user-visible output change under
  `whatwg`** (spec exactness over output stability). Because the encode set
  inherits the C0-control set, it also covers every C0 control, DEL and
  **non-ASCII** byte, so a non-ASCII userinfo now gains UTF-8 escapes too:
  `"http://éx@host/"` reports user `%C3%A9x`. Existing percent-triplets are
  re-emitted verbatim, never decoded or double-encoded (`u%40ser` stays
  `u%40ser`, `%25DOMAIN` stays `%25DOMAIN`), and a space already pre-encoded by
  the userinfo charset shim stays `%20`. The transform is gated on `whatwg`
  explicitly and applies only where the userinfo was actually split into a
  username and a password: `url_standard = "rfc3986"` and the no-selector
  default remain source-preserving and byte-for-byte unchanged, as do the
  undivided RFC 8089 `file:` overlay and a `mailto:` recipient local-part,
  which is not a URL userinfo.

- **The general/opaque route no longer discards credentials.** For a
  general-routed URL — any non-special scheme under
  `scheme_acceptance = "general"` — the opaque parser computed the authority's
  userinfo and then dropped it, so
  `safe_parse_urls("sc://u:p@h/x", scheme_acceptance = "general",
  url_standard = "rfc3986")` reported `user` and `password` as `NA` while the
  same credentials on an `http:` URL were reported exactly. The userinfo is now
  split at the **first** `:` per WHATWG's authority state, so `u:p:q` gives
  user `u` and password `p:q`, and the userinfo percent-encode set applies here
  on the same terms as the libcurl route (`p%3Aq` under `whatwg`, `p:q` under
  `rfc3986` and the no-selector default).

  Two producers deliberately stay undivided and unencoded: the RFC 8089
  `file:` overlay, whose Appendix E.1 production is `[ userinfo "@" ]` with no
  credentials split and which warns that a password there is "a serious
  security exposure"; and a `mailto:` `user`, which is a recipient local-part
  (ADR 0012 D7), not a URL userinfo at all.

  `clean_url` is unaffected: it carries credentials on no route, including the
  libcurl one. No scored conformance figure moved *at the time of this fix* —
  because the parity oracle then compared only scheme, host, port, path, query
  and fragment, so a credential fix and a credential regression were equally
  invisible to it. That measurement gap is closed separately below; credentials
  are now scored, and this fix passes on all 336 rows.

- **`url_standard = "whatwg"` no longer rejects a URL whose userinfo carries a
  space, a C0 control or DEL.** libcurl refuses an authority whose userinfo
  contains any of 30 ASCII code points — SPACE (U+0020), the C0 controls
  (U+0000–U+001F) and DEL (U+007F) — so rows the WHATWG parser accepts were
  errored out entirely: `"http://a b@host/"` was a parse error (now accepted,
  host `host`, user `a%20b`), as were the WPT punctuation-run rows
  ``"wss:// !\"$%&'()*+,-.;<=>@[]^_`{|}~@host/"`` and its `joe:`-password variant
  (both now host `host`, under `scheme_acceptance = "general"`). Those 30 code
  points are now percent-encoded in the userinfo span before curl parses the
  string. Every one of them is a member of the WHATWG userinfo percent-encode
  set, so the encoded form is the spelling WHATWG stores and no restore step is
  involved; `%` is not in the set, so an already-encoded userinfo
  (`%25DOMAIN`, `u%40ser:p%40ss`) is never double-encoded. All other userinfo
  bytes, including non-ASCII, already parsed and are untouched, as is the
  pre-existing repeated-`@` recovery (`"http://username@@@@example.com"` still
  reports user `username%40%40%40`). The rewrite is gated on `whatwg`
  explicitly: `url_standard = "rfc3986"` — which has no userinfo production for
  a space or a control byte — still rejects these inputs, and the no-selector
  default is byte-for-byte unaffected. Acceptance does not launder the row's
  facts: such rows report `invalid-credentials` and `invalid-URL-unit` from
  `get_url_diagnostics()` as before.

- **Leading and trailing C0-control-or-space is now stripped from the input
  under `url_standard = "whatwg"`.** WHATWG's basic URL parser step 1 has two
  halves — first remove any leading and trailing C0 control or space
  (U+0000–U+0020) from the input, *then* remove every ASCII tab/LF/CR anywhere
  in it. rurl implemented only the second half, so an input padded at either end
  was mis-parsed: `"http://example.com/a  "` reported path `/a%20%20` (now
  `/a`), `"  http://example.com/a"` was a parse error (now accepted, host
  `example.com`), and under `scheme_acceptance = "general"`
  `"non-special:opaque  "` reported path `opaque  ` (now `opaque`). Interior
  spaces are untouched, and the trim runs in the spec's order, so a *leading*
  tab is now removed by the first half rather than the second. The strip lives
  in the single seam every route shares, so the libcurl route, the general route
  and the Stage-B re-parse are fed identical input by construction.

  The mutation is surfaced, not silent: a new `get_url_diagnostics()` token
  **`leading-trailing-stripped`** fires exactly on the rows where a
  leading/trailing run was removed. It is deliberately a separate token from
  `control-char-stripped`, whose meaning is unchanged (an interior tab/LF/CR was
  removed); a row that had both reports both. `url_standard = "rfc3986"` and the
  default (no selector) are byte-for-byte unaffected — that profile has no strip
  step and requires such bytes to be percent-encoded, so those inputs stay
  errors. The frozen `analysis/parity` and `analysis/disagreement` studies are
  byte-identical; WPT full-row parity on the excluded rows improves by one and
  component mismatches go to zero. (RURL-yvxpanix.)

- **Opaque paths are now percent-encoded, and `^` joins the path
  percent-encode set.** Three related WHATWG encode-set gaps, all under
  `scheme_acceptance = "general"` unless noted:

  * **Opaque paths were carried verbatim.** WHATWG's opaque path state encodes
    each code point with the C0-control percent-encode set as it is consumed,
    so the stored path — what the `path` column reports — is already encoded.
    rurl encoded only `clean_url`, so `wow:<U+FFFF>` reported path
    `<U+FFFF>` where the serialized URL said `%EF%BF%BF`. The two now agree.
    The C0 set is *not* the path set: printable ASCII that a hierarchical path
    escapes (`^`, `{`, `}`, `<`, `>`) stays literal in an opaque path, and
    existing `%xx` spellings are preserved. WHATWG also encodes the single
    space immediately before the `?` or `#` that ends an opaque path — so that
    a trailing space survives a re-parse — and `non-special:opaque  ?hi` now
    yields path `opaque %20`, with interior spaces untouched.
  * **`^` (U+005E) was missing from the path percent-encode set.** It applies
    to *every* profile row under `url_standard = "whatwg"` with
    `path_encoding = "encode"`, not just general-routed ones:
    `http://ex.com/a^b` now presents `/a%5Eb`.
  * **U+000B (VT) and U+000C (FF) broke decomposition entirely.** ICU counts
    both as line terminators, so the `.`-based scheme/remainder split in the
    general parser never matched them and the whole row failed — `sc://a<VT>b/`
    was a parse error instead of host `a%0Bb`. WHATWG's step 1 strips only
    tab/LF/CR; VT and FF are kept and percent-encoded. This also unblocks the
    WPT C0-control host row, whose opaque host now encodes `%01`…`%1F%7F`
    rather than being rejected.

  `url_standard = "rfc3986"` is unaffected throughout — the `rfc-syntax`
  posture disclaims this normalization. One cell of the frozen
  `analysis/disagreement` study moves as a result, *converging* on the value
  the WHATWG reference parser already reported; no count or ratio changes.
  (RURL-qxpgcwie.)

- **IPv6 hosts are now WHATWG-serialized for non-special schemes too.** The
  WHATWG host parser stores an IPv6 literal as eight 16-bit pieces and
  re-serializes it — longest zero run compressed to `::`, lowercase hex, no
  dotted-quad tail — and that step is *scheme-independent*: the same host parser
  runs for any scheme carrying an authority. rurl wired the serializer on the
  special-scheme branch only, so under `scheme_acceptance = "general"` a
  non-special host kept its input spelling:
  `non-special://[1:2:0:0:5:0:0:0]/` reported `[1:2:0:0:5:0:0:0]` where
  `http://[1:2:0:0:5:0:0:0]/` correctly reported `[1:2:0:0:5::]`. The two
  branches now agree, and `[ABCD::1]` and `[::127.0.0.1]` render as `[abcd::1]`
  and `[::7f00:1]` for non-special schemes as well.

  The change is confined to the WHATWG opaque-host parse, so validation is
  untouched — a malformed literal such as `[1:2:3:4]` is still a host parse
  failure rather than a passthrough. `url_standard = "rfc3986"` is unaffected
  and keeps the input spelling: the `rfc-syntax` posture disclaims host
  normalization, which is the deliberate profile split. (RURL-cyxegfjs.)

- **ASCII tab, LF and CR are now stripped for non-special schemes too.** The
  WHATWG parser's very first step removes every ASCII tab (U+0009), LF
  (U+000A) and CR (U+000D) from the input, everywhere, before any component is
  parsed — and that step is *scheme-independent*. rurl applied it only on the
  libcurl preparation path, so rows routed to the general parser under
  `scheme_acceptance = "general"` were handed the raw string: `foo://ho<TAB>st/`
  percent-encoded the tab into the host as `ho%09st`, and `foo://ho<LF>st/` was
  rejected outright. All four spellings now agree with the clean input on host
  `host`.

  Only the strip is shared with the general route, deliberately not the rest of
  the preparation: browser fixup and special-scheme backslash rewriting are
  separate rules that must not begin firing on non-special schemes. Both parse
  stages apply it identically, since they disagree about routing otherwise.
  `url_standard = "rfc3986"` is unaffected and still rejects — RFC 3986 has no
  strip step and requires such bytes to be percent-encoded, which is the
  deliberate profile split. (RURL-lsgdeisl.)

- **An opaque URL whose payload ends in `:<digits>` now parses.** Under
  `scheme_acceptance = "general"`, `urn:ietf:rfc:2648` — the textbook URN form —
  was rejected outright, as were `urn:a:1` and `sc:x:80`. A non-numeric tail
  (`urn:ietf:rfc:abcd`) was fine, and so was a trailing query or fragment
  (`urn:a:1?q`), which made the failure look arbitrary.

  The cause is the carve-out that keeps the scheme-less `example.com:8080` form
  out of the opaque parser. It is needed because a dot is a legal scheme
  character, so `example.com:8080` also matches the scheme grammar — but its
  authority part was matched colon-greedily, so `urn:ietf:rfc:2648` read as
  "authority `urn:ietf:rfc`, port 2648". Such a row was withheld from the opaque
  parser and fell through to the web path, which rejects `urn:`. The trailing
  `?`/`#` cases escaped only because they broke the pattern's end anchor.

  The authority part must be colon-free, which is what the scheme-less form
  actually is. An opaque path has no authority, so a numeric tail in one is
  never a port: `urn:ietf:rfc:2648` now parses with path `ietf:rfc:2648` and no
  host or port. `example.com:8080` is unaffected and still reads as host plus
  port. Pre-existing since general acceptance shipped — **not** introduced by
  the host-missing-authority rule earlier in this cycle, verified against that
  commit's parent. (RURL-jnvtttfm.)

- **A `mailto:` URL carrying a real `//` authority no longer loses its host.**
  Under `scheme_acceptance = "general"`,
  `safe_parse_urls("mailto://example.com:8080/pathname")` reported
  `host = NA` while still reporting `port = 8080` — an authority presenting a
  port with no host to attach it to. WPT expects hostname `example.com`.

  The WHATWG opaque-path rule has two halves — a non-special scheme **and** no
  `//` — and only the first was being tested. ADR 0012 D7's recipient
  decomposition is about the opaque form (`mailto:jane@example.com`, where the
  payload is an `addr-spec`); running it over `mailto://host/pathname`
  overwrote the authority the general parser had already parsed correctly with
  the `NA` that decomposing `/pathname` yields, since a path is not an address.
  Both the Stage A recipient write and the T1 parse-table mask now require the
  `//` to be absent, so they agree on what "opaque" means.

  Nothing about the opaque form changes: `mailto:jane@example.com` still
  presents no authority in the parse table, and `get_host()` / `get_domain()`
  still resolve the recipient through D7. This narrows the recipient rule to
  the shape it was always specified for; it does not retire it. Introduced by
  the D7 slice earlier in this same unreleased cycle, so no released version
  carries it. (RURL-gmzipkyw.)

- **`url_standard = "whatwg"` now rejects a host-missing authority under
  `scheme_acceptance = "general"`.** `sc://@/`, `sc://te@s:t@/`, `sc://:/` and
  `data://:` parsed as `ok` even though all four are host-missing authorities
  that WHATWG requires be rejected — they are in the WPT must-fail set, and
  adaR 0.3.5 rejects every one. The default `web` acceptance already rejected
  them, so `general` was the *more* permissive route, which is backwards.

  The earlier fix in this cycle keyed the host-missing rule off the *port having
  content*, so an empty host followed by a bare `:` or `@` slipped through. The
  trigger is really the **delimiter**: WHATWG's host state fails on the `:`
  itself before any port is read, and its authority state fails when an `@` was
  seen and the host after the last one is empty. A `//` authority holding
  nothing else (`foo:///bar`) remains the one legal empty-host shape, and a
  non-empty host with an empty port (`sc://host:/`), an IPv6 literal
  (`sc://[::1]:/`) or userinfo (`sc://user@host/`) all stay legal.

  `url_standard = "rfc3986"` is deliberately unaffected: its `reg-name` and
  `port` productions are both `*`-quantified, so these are well-formed generic
  syntax under the RFC.

- **`url_standard = "rfc3986"` now applies RFC 3986's generic-URI grammar to
  every scheme, not just `file:`.** The grammar gate travelled with the RFC 8089
  `file:` overlay, so which parser happened to own a row decided whether the
  selected standard was enforced: `file://C|/x` was an error while
  `http://a|b/` parsed and reported `host = "a|b"` — though `"|"` is in none of
  `unreserved` / `pct-encoded` / `sub-delims` / `pchar`, so no RFC 3986
  production admits it either way. That is the wrong thing for a *selector* to
  mean. Asking for a standard now gets that standard's grammar on every route
  (libcurl, path-rootless, `file:`, general), which is also what a reference
  RFC 3986 parser such as Ruby's `URI::RFC3986_Parser` does with both strings.

  In practice this rejects raw bytes the grammar has no production for —
  `"|"`, `"\"`, `"""`, and a repeated raw `"@"` in an authority — where the
  rfc3986 profile previously carried them through or silently recovered a host
  from them. That last case matters most: `https://n.pr\@e.gg` used to resolve
  to host `e.gg` under rfc3986, reproducing what *permissive* RFC-style parsers
  do rather than what the standard says, on exactly the inputs security papers
  use to demonstrate host equivocation. The independent ABNF transcription of
  RFC 3986 Appendix A that referees the project's oracle rejects all of them.

  Deliberate acceptances are untouched: this adds only generic-*syntax*
  rejections. A directly-written non-ASCII host stays accepted and flagged
  (`http://exämple.com/`, ADR 0002/0011), a reg-name built from characters the
  RFC admits stays accepted even where WHATWG forbids it (`http://a%7Cb/`), and
  scheme inference remains the `scheme_policy` axis, so scheme-less input is
  unaffected. `url_standard = "whatwg"` and the no-selector default are
  byte-identical.

- **A list element of the wrong length no longer aborts the whole
  `safe_parse_urls()` call.** `safe_parse_urls(list("http://a.com/", c("b", "c")))`
  failed the entire call with base R's untyped `"values must be length 1"`. A
  list element of the wrong length is bad *data*, not a contract violation, so it
  now recovers row-locally as an error row (`original_url = NA`,
  `parse_status = "error"`) — matching what every other non-scalar shape (`NULL`,
  length 0, a non-character vector) already did. Call-level errors stay reserved
  for contract violations.

- **Input names no longer leak into `safe_parse_urls()` row names.**
  `safe_parse_urls(c(a = "http://example.com/", b = "http://ex.org/"))` promoted
  the input's names to the result frame's row names, while the vectorized
  accessors strip them. The public surface disagreed with itself, and leaked row
  names are a silent correctness hazard rather than a cosmetic one — they survive
  into joins and downstream frames as though they were a column. The frame now
  always has ordinary sequential row names, matching the accessors, which return
  unnamed vectors. Both halves are now pinned by test; the suite previously had
  no named-vector coverage at all.

- **`get_mailto_recipients()` no longer errors at its own documented
  defaults.** Every call that did not pass `scheme_acceptance` explicitly —
  including the plain `get_mailto_recipients("mailto:x@example.com")` — aborted
  with base R's untyped `"'arg' must be of length 1"`. The helper's
  `scheme_acceptance` formal deliberately lists `"general"` first, since mailto
  is a general-scheme context, and that unresolved length-2 default was
  forwarded to an internal whose own choices are ordered `"web"` first;
  `match.arg()` tolerates a length > 1 value only when it is `identical()` to
  the callee's choices, so the reversed order failed. The default is now
  resolved against the helper's own formal before forwarding, leaving the
  deliberate ordering intact. Broken since the helper shipped in 2.6.0; all
  three documented examples pass the argument explicitly, so `R CMD check`
  never exercised the default path.

### New features

- **`serialize_url()` renders a URL the way its standard would.** rurl's only
  full-string output was `get_clean_url()`, which is an SEO/canonicalization
  product: it drops credentials and the fragment by design, and it is driven by
  two dozen cleaning dials. That makes it the wrong thing to compare against a
  standard. `serialize_url()` is the other surface — the full string, credentials
  and fragment included, with no presentation dial at all:

  ```r
  serialize_url("http://user:pw@Example.COM:80/a/../b?q=1#frag")
  #> [1] "http://user:pw@example.com/b?q=1#frag"
  get_clean_url("http://user:pw@Example.COM:80/a/../b?q=1#frag")
  #> [1] "http://example.com/a/../b"
  ```

  A present-but-empty delimiter carries information and survives, which no
  cleaning surface can promise:

  ```r
  serialize_url(c("http://h/", "http://h/#", "http://h/?"))
  #> [1] "http://h/"  "http://h/#" "http://h/?"
  ```

  `standard = "rfc3986"` selects RFC 3986 §5.3 recomposition, in either of two
  postures: `form = "source"` (the default) normalizes nothing and emits the
  undivided `userinfo` verbatim, while `form = "normalized"` applies §6.2.2
  syntax-based normalization and §6.2.3 default-port elision.

  ```r
  serialize_url("HTTP://Example.COM:80/a/%7Euser/../x", standard = "rfc3986")
  #> [1] "http://Example.COM:80/a/%7Euser/../x"
  serialize_url("HTTP://Example.COM:80/a/%7Euser/../x", standard = "rfc3986",
                form = "normalized")
  #> [1] "http://example.com/a/x"
  ```

  Each standard is parsed under its own spec posture, so any scheme is accepted
  and a scheme is *required*: neither standard defines a base-URL-free parse of
  `example.com/x`, and rurl's `https://` prepend is browser-like fix-up that has
  no business inside a standard serialization. Scheme-less input returns `NA`.

  Two consequences are worth stating plainly. WHATWG credential serialization is
  spec-exact and therefore lossy in one direction — `http://@h/` serializes as
  `http://h/` and `http://u:@h/` as `http://u@h/`, both pinned by the Web
  Platform Tests — and the losslessness lives in the parse record, which the RFC
  `source` form renders verbatim. And `serialize_url()` emits WHATWG's ASCII
  (punycode) host where `get_clean_url()` keeps the Unicode spelling; both are
  correct for their surface.

- **`get_parse_verdicts()` reports the three verdicts `parse_status` collapses
  into one.** A single status value answers three independent questions at
  once — did the input present well-formed URL syntax (layer 1), was the parsed
  object admitted under the active policy (layer 2), and what did the Public
  Suffix List annotation find (layer 3) — so it is a *lossy* view of them. The
  guaranteed loss is that a structural failure and a policy rejection both
  report `"error"`:

  ```r
  get_parse_status(c("mailto:jane@example.com", "http://"))
  #> [1] "error" "error"

  get_parse_verdicts(c("mailto:jane@example.com", "http://"))
  #>   layer1_syntax_verdict layer2_policy_verdict layer3_annotation_state
  #> 1                  pass       rejected-scheme          not-applicable
  #> 2                  fail              admitted          not-applicable
  ```

  The first was declined at admission; the second did not parse. Layer 3 also
  makes the Public Suffix List result a *typed annotation* rather than a
  warning: a host with no public suffix is `"unknown"`, while an IP literal or
  a `file:` host is `"not-applicable"` (no registrable-domain concept at all),
  and neither is ever fatal.

  Like the other companion helpers it never widens the `safe_parse_url()`
  frame — that keeps its 18 columns. Unlike `get_host_type()` and
  `get_scheme_class()` it is fully defined without a `url_standard` selector,
  since layers 1 and 2 describe the parse that actually occurred.

  **`parse_status` is unchanged and is not deprecated.** It is now *derived* as
  the projection of the three layers rather than computed separately, so there
  is one status-deciding path and the two surfaces cannot drift apart.
  Byte-identity was verified over 74,700 (`parse_status`, `clean_url`) cells —
  the committed corpora under 60 option configurations — plus the whole suite.

- **`get_password()`, `get_query()`, `get_fragment()` and `get_port()` gain the
  standards axis (`url_standard`, `scheme_policy`, `scheme_acceptance`).** Each
  of the four could previously take only presentation dials, so none of them
  could return a value its own `safe_parse_url()` column carries:
  `get_password()` could not reach the WHATWG userinfo spelling the `password`
  column has carried since 2.8.0's userinfo encode set (`p:q` vs `p%3Aq`);
  `get_query()` and `get_fragment()` could not reach the query and fragment
  percent-encode-set spellings; and `get_port()` could not report the WHATWG
  default-port drop, where `http://example.com:80/` parses to `NA` rather than
  `80`. All fourteen accessors now expose all three axes.

  **Purely additive.** The new arguments default to the source-preserving
  behavior (`url_standard = NULL`), and output with the arguments omitted — or
  passed at their defaults — is byte-identical to before, which is pinned by a
  test.

  The accessor↔option coverage oracle
  (`tests/testthat/test-accessor-registry.R`) now covers these three axes, not
  just the eleven presentation dials. Their absence from it is precisely why
  all four gaps went unnoticed: no registry cell forced the arguments to exist.

### Documentation

- **RFC 3986 now has a serialization oracle; it had only an acceptance axis.**
  The conformance evidence has four quadrants — {WHATWG, RFC 3986} ×
  {acceptance, full-string serialization} — and three were covered. Nothing in
  the repository answered *what string does RFC 3986 require this URL to
  serialize to*. That gap was not theoretical: the host-decoding defect fixed
  earlier in this release produced output that does not re-parse, and passed
  every harness here, because an acceptance metric cannot see a bad output
  string on an input it accepts.

  The new harness is **property-based and transcribes no expected strings**.
  WHATWG ships a suite with recorded `href` values; RFC 3986 is prose plus
  ABNF, and hand-transcribing expected strings is the move that produced 75
  fixture rows where the oracle and the implementation confirmed each other and
  their shared disagreement with the RFC stayed invisible. So §6.2.2/§6.2.3 are
  stated as properties over a **generated** 6506-input population (256 octets ×
  2 hex cases × 12 component positions × both scheme classes, plus 47
  structural shapes): every output must be admitted by the RFC's own ABNF, must
  re-parse to itself, and under `form = "normalized"` must satisfy §6.2.2.1
  case, §6.2.2.2 unreserved decoding, §6.2.2.3 dot-segment removal and §6.2.3
  default-port elision. A property needs no oracle, so it cannot co-confirm
  with the implementation.

  The one external judge is the independent RFC 3986 ABNF transcription that
  already exists in the suite — normative grammar, sharing no code with rurl.

  **What it is not.** These are properties of the output *string*. An output
  can satisfy every one and still describe the wrong URL, because nothing here
  checks that a component was sliced from the input correctly. This quadrant is
  a necessary condition on RFC serialization, not a sufficient one, and the
  oracle register records it that way (OR-023).

  Running it found three defects with no oracle and no adjudication, each now
  enumerated by input in the harness so a fix must delete its entry rather than
  re-fit a total: the general-scheme host is the only component that skips
  §6.2.2.2 unreserved decoding; a general-scheme userinfo admits raw
  CR/LF/VT/FF and emits them verbatim, which no RFC 3986 production allows; and
  `form = "source"`, documented as preserving source bytes, does not — 419 of
  5668 accepted rows change, in four families. All three are filed, not fixed
  here: the host seam is the one where a change silently widens *acceptance*
  rather than only serialization, so it needs its own octet-invariance sweep.

- **All conformance and benchmark evidence is re-baselined onto
  `serialize_url()`; two headline figures now exist where one did.** Every
  harness in the repository scored `clean_url`, which is output surface (c) —
  "a policy-driven SEO/canonicalization product; **not** a serializer, identity,
  redirect target, or conformance oracle" — and which P5.3 CLAIM-1 does not
  admit as a claim substrate. Surface (b) did not exist when those harnesses
  were written, so there was nothing admissible to score against. It does now.

  **The previously published "158 conforming / 99 documented deviations" is an
  RFC-3986-grammar *acceptance* count and is not replaced by the new figure.**
  The two answer different questions:

  | | axis 1 (new) | axis 2 |
  |---|---|---|
  | question | does rurl emit the standard's **serialization**? | does rurl **accept/reject** what the grammar does? |
  | authority | `whatwg-wpt` | `rfc3986-grammar` |
  | figure | **336 exact / 0 deviations** over the 336 WPT success rows, plus **202/202** must-fail rows rejected | 164/93 → **179/78** (like-for-like, 257-row scope) |

  Axis 1 is new evidence: P5.3 §2.2 had already recorded that the FSSS
  full-string headline did not yet exist. It is measured on the **WHATWG's own
  test suite** — the imported web-platform-tests corpus in
  `inst/bench/wpt-url-cases.json` — and scored against upstream's recorded
  `href`, the standard's own serialization, rather than against a string
  re-assembled from the component getters. That distinction is not cosmetic:
  the component dump cannot tell a null host from an empty one, so a
  re-assembly must guess the `//` delimiter, and doing so reported 40
  differences where the authoritative oracle reports 13.

  It is reported **by substrate** — serialization and acceptance are never
  summed, because an aggregate would let must-fail rows inflate a
  *serialization* result. All four `standard` × `form` configurations were
  swept; under `whatwg` the two forms are identical on every row, and
  acceptance never depends on `form`.

  The measurement first reported 326 exact with 10 deviations — one family in
  one scheme, the host-less `file:` URL parsed to a null host. That family is
  **fixed in this same release** (see *Bug fixes*, `RURL-uhwivndf`), which is
  what moves the figure above to 336 / 336. Non-special schemes were exact on
  all 141 rows and special schemes with a host on all 159 before the fix as
  well; the family was traced to the parse record rather than the serializer,
  and was fixed there.

  Axis 2 moves because fifteen rows stop being deviations. Every one is a case
  where surface (c) declined **by policy** — the ADR 0004 closed scheme set, the
  ADR 0002 reversible Unicode host, the readable-path default — and surface (b)
  matches the grammar. rurl's parser never disagreed with the RFC on any of them.
  The same effect empties the Ada watch list entirely (11 documented divergences
  → 0): Ada is a conformant WHATWG parser and its `href` is a full serialization,
  so that comparison was like-for-like for the first time.

  **The corpus shape was distorted too, and is repaired.** It carried **zero**
  expected values with a fragment or credentials, and the conformance fixture
  contained **zero** `@` characters — because those are precisely the rows a
  `clean_url` comparison could never have passed, so the corpus had grown into
  the shape its harness could score. 43 rows are added from the committed WPT
  import, with expected values assembled from WPT's own recorded components by
  the WHATWG URL serializer (URL Standard §4.5). rurl matches all 43. The
  string-valued substrate nearly doubles, 49 → 92 rows.

  Two oracle repairs are not surface swaps. Host oracles now compare an
  **extracted host** rather than asking `grepl(host, clean_url)`; on a
  hostname-confusion corpus containment is the wrong question, since both
  hostnames appear in `http://letsencrypt.org%2F@malware.testing.google.test/`
  and it passes whether the parse is correct or inverted. And the conformance
  fixture's `divergence_class` is derived from the **normalized** serialization
  rather than the source form, which had misclassified 7 of 75 rows in both
  directions. Disposition and full numbers: P5.4. (RURL-yeikpnan.)


- **The diagnostics vocabulary now has one canonical, enforced enumeration.**
  `?get_url_diagnostics` gains a *Diagnostic vocabulary (canonical)* section
  listing all 32 tokens with their meanings and the postures they fire under.
  Previously the only enumeration was the v1 selector PRD's section 7 table,
  which — being a graduated, historical spec (ADR 0008) — had stopped tracking
  the code: it was missing `control-char-stripped`, `host-charset-shimmed`,
  `leading-trailing-stripped` and every Layer-5 token. A new CI gate,
  `tools/diagnostics-doc-consistency.R`, now holds that section and the
  `.URL_DIAGNOSTICS` registry to each other in both directions and rejects any
  documented diagnostic literal that no longer resolves to a real token, so the
  drift cannot recur silently. The test that claimed to check the PRD table was
  renamed to what it actually does — pin the closed set.

- **`safe_parse_url()`'s `query` and `fragment` columns are documented with the
  two-branch encoding contract they actually have.** Both said the value is
  returned "as written in the URL"; that stopped being true under
  `url_standard = "whatwg"`, where the query and fragment percent-encode sets
  are applied. `get_query()` and `get_fragment()` take no `url_standard` and so
  do always return the raw source spelling — their documentation now says so
  explicitly and points at the column for the WHATWG spelling, matching the
  wording already used by `get_password()`.

- **The conformance posture is now stated in one place, with its measurements
  and its limits.** `vignette("url-standard")` gains a *Conformance posture*
  section: against the WHATWG spec's own conformance suite the `"whatwg"`
  profile is 378/378 (176 success rows at full component parity, 202 rejections)
  and differs from the `adaR` reference on **two rows out of 336**, neither a
  parsing disagreement; against RFC 3986 the `"rfc3986"` profile matches on
  **164** of 257 oracled rows and departs on **93**, every departure attributed
  to an ADR. The two boundaries that keep this honest are stated alongside the
  numbers rather than buried: the WPT fixture covers **absolute** URLs only, so
  the figures say nothing about base-relative resolution; and the profile is
  WHATWG on its governed axes, **not** a full UTS-46 host mapping. (The success
  figure was later widened to 336 rows — see the entry below.)

- **"Full component parity" now scores credentials, so it means all eight
  components rather than six.** `inst/bench/standard-parity.R` built its
  per-row verdict from scheme, host, port, path, query and fragment;
  `username` and `password` were never compared, in either posture. The
  published headline — "336/336 accepted, 336/336 FULL component parity" — was
  therefore silent on credentials, and the omission was not hypothetical: the
  general/opaque route discarded userinfo entirely (fixed above), a
  component-level non-conformance on the exact posture whose figure read 100%,
  and **no scored number would have moved** either when it broke or when it was
  fixed. A measurement whose name claims more than it checks.

  The oracle was re-extracted at the **same** pinned upstream revision
  (`181476aa`, from a raw file whose sha256 still matched the recorded
  `raw_source_sha256` — a re-extraction, not a re-pin) so that
  `make-wpt-fixture.py` carries upstream's `username`/`password`, which it had
  been dropping: 24 rows carry a non-empty username and 13 a non-empty
  password. No row was added or removed, so the case counts stay 336/202.

  The headline is unchanged at **336/336**, but it is now a wider claim over a
  stricter denominator, not the same claim restated — credentials are checked,
  and they pass on every row at both postures. `analysis/parity/` was re-frozen:
  the two success CSVs gained four columns, and the failure and RFC CSVs
  reproduced byte-identically.

- **The WPT success oracle now spans every scheme, and both scheme-acceptance
  postures are scored.** The fixture generator used to keep only
  `http`/`https`/`ftp`/`file` success rows, so the headline "176/176 full
  component parity" was scored over a corpus that could not contain an opaque,
  `ws:` or `wss:` URL — the carve-out the previous entry had to disclose as
  *unmeasured*. Dropping it takes the success set from **176 to 336** rows (the
  202 failure rows are unchanged), and `inst/bench/standard-parity.R` now passes
  `scheme_acceptance` explicitly instead of inheriting the exported default,
  scoring both postures side by side. At `scheme_acceptance = "general"` rurl
  reaches **full component parity on all 336 rows** — across the six components
  scored at the time (scheme, host, port, path, query, fragment; credentials
  were added later in this release, see below) — with zero rejections of
  WPT-valid input, and still
  rejects **202/202** failure rows, for **538/538** overall. Widening the corpus
  by 160 rows surfaced no new mismatch, so opaque, `ws:` and `wss:`
  serialization is now measured-and-conformant rather than silently untested.
  At the default `"web"` posture 160 of the 336 are declined by the ADR 0004
  allowlist before the grammar is consulted; that is the allowlist working, and
  `176/336` is not a conformance rate. Scoring the failure rows at `general`
  also answers in band what previously needed the companion study: the 36
  non-web-scheme failure rows are rejected by the **grammar**, not by the closed
  scheme set. Only base-relative rows remain out of scope, because rurl parses
  absolute URLs. Frozen in `analysis/parity/`.

- **`scheme_acceptance` is documented as an axis in its own right.** The
  vignette's *What the selector does not govern* section previously said the
  selector "does not expand the allowed scheme set beyond
  `http`/`https`/`ftp`/`ftps`" — which omitted `file` from the actual allowlist
  and left readers with no way to discover that `scheme_acceptance = "general"`
  parses `mailto:`, `data:` and `tel:`. The two axes are now described as
  composing: `scheme_acceptance` decides what gets parsed, `url_standard`
  decides how the result is read. A worked `mailto:` example shows the
  opaque-path rule from the 2.8.0 breaking change — the parse table reports no
  authority, while `get_host()` still extracts the recipient's host.

- **`analysis/parity/README.md` now states its own posture and carries an
  attributed ledger of what is left.** Every figure in it is scored at the
  default `scheme_acceptance = "web"`, which was true but unstated; the success
  fixture's scheme carve-out is now recorded as a *measurement limit* so the
  silence on opaque schemes is not read as a pass. (Both of those were then
  closed within this same release — the README now scores **both** postures over
  a fixture with no scheme carve-out.) The residual deviations are
  tabulated with their owning ADR and, where one exists, the argument that
  reaches them — separating the one genuine gap (UTS-46 host mapping) from the
  four deviations that are dials the caller chooses.

- **A stale attribution of the RFC departures is corrected.** The 81 over-strict
  rows were described as coming from "the ADR 0004 host-shape gate and the
  closed scheme set". Re-derived from the audit rows, the closed scheme set
  contributes **zero** of them: all 81 are the host/authority gate
  (percent-encoded reg-names 48, other reg-name shapes 11, empty host 8,
  userinfo 6, absent authority 5, port shape 3), and all 12 over-lenient rows
  are the single `non-ascii-or-control` family. The corpus is 202/282
  WPT-sourced and so almost entirely `http`/`https`/`file`, leaving the scheme
  set no opportunity to fire. The 164/93 headline is unchanged. (RURL-vgovkcze.)

### Internal

- **Authority presence is now recorded as two independent facts instead of one
  ambiguous enum.** The internal state model carried a single three-valued
  `authority_kind`, which conflated *was a `//` delimiter present* with *did it
  carry anything* and left its `empty` value unreachable: the general parser
  called every `//` row authority-present, while the RFC 8089 `file:` overlay
  called the identical shape authority-empty, so `foo:///bar` and `file:///bar`
  disagreed. It is replaced by `authority_delimiter_present` (logical) and
  `authority_payload_kind` (`empty`/`present`, `NA` when no delimiter was
  present), with `host_kind` staying an independent axis — a payload can be
  present while the host is empty (`foo://@/bar`, `foo://:80/bar`). The legacy
  name survives only as a derived, read-only projection, whose `empty` value is
  now reachable and defined. Both posture serializers emit `//` from the
  recorded delimiter fact rather than re-deriving it from `host_kind`, which
  could not tell a delimiter-present empty authority from a delimiter-absent
  input.

  **No public output changes.** These are internal state fields; the general
  route's parsed columns and `clean_url` were verified byte-identical on both
  postures across the opaque, empty-authority, `file:`, IPv6 and credential
  shapes.

- **The committed WHATWG conformance oracle
  (`inst/bench/wpt-url-cases.json`) now covers every scheme, not four.** The
  success arm of the fixture was carved out to `http`/`https`/`ftp`/`file`,
  which silently dropped every non-special and opaque WPT success case — so
  the oracle could not see a whole category of behaviour that
  `scheme_acceptance = "general"` parses. The generator's scheme filter is
  removed entirely rather than extended with a list: WHATWG has exactly two
  scheme categories, so "success = any base-null non-failure case" is the
  selector that needs no maintenance. Success grows 176 → 336 across 54
  schemes; the failure arm is unchanged at 202, having never been filtered by
  scheme. Base-relative rows (the two `base = "about:blank"` fragment
  references) are now excluded as out of scope: rurl is an absolute-only
  parser and does no relative resolution, the same disposition
  `external-url-vectors.csv` already records for such rows. The applicability
  selector, counts and fixture hash in
  `tests/testthat/fixtures/oracle-provenance.json` are re-cut to match.

- **The cross-parser disagreement study now measures rurl at
  `scheme_acceptance = "general"`, and `analysis/disagreement/` is re-frozen.**
  Previous runs used the default `"web"` allowlist against adaR and
  `urllib.parse`, which are *general* parsers — scoring ~19 opaque/non-special
  rows as rurl rejections and measuring rurl's scheme-acceptance policy rather
  than the `url_standard` interpretation the study is about. Held as a
  documented axis alongside `scheme_policy = "require"`.

  `rurl(whatwg)` vs adaR falls from **10 divergent rows to 2** (full-tuple
  agreement 0.970 → 0.994), and neither remaining row is a parsing
  disagreement: one is punycode-vs-Unicode host rendering (a `host_encoding`
  choice, ADR 0002) and one is the held `scheme_policy` row. There is no
  accept/reject, host-shape, port or path disagreement left against the WHATWG
  reference on this corpus. The `rurl(rfc3986)` vs `curl` pairing moves the
  other way (52 → 73) because libcurl is a web-scheme parser: 15 of those rows
  are purely scheme acceptance and are enumerated as such in the README, so the
  count is not read as RFC-interpretation divergence.

  Two stale claims in the frozen README were corrected against the regenerated
  matrix: the `http://ex.com:80/` row still said "adaR alone drops `:80`"
  (`rurl(whatwg)` has elided since `RURL-uvilvhnm`), and the `%7e` percent-hex
  caveat still claimed a residual path gap against adaR (closed —
  path agreement is now 1.000). Analysis artifacts only; no package behavior
  changes.

- The documented `canonical_join()` example no longer passes presentation dials,
  so the package's own headline usage no longer demonstrates the pattern that now
  warns. Documentation and `README` only; no behavior change.

- The four tests that pinned presentation dials moving the `canonical_join()`
  comparison key now state that behavior as documented legacy that warns, rather
  than endorsing the collapse as correct. Every value assertion is unchanged; the
  rewrite to key invariance is deferred to the slice that introduces an explicit
  identity key.

- **The RFC 3986 probe set is now two-sided, and the published conformance
  figure has moved.** `inst/bench/rfc3986-probes.csv` grew from 19 rows to 37.
  Every one of the original 19 was an *accept* case, so the set could not detect
  over-permissiveness at all — it could only fail to notice it. The 18 new rows
  are rejection cases tagged by ABNF section, drawn from the audited conformance
  fixture rather than invented, and each verified against both referees (the
  transcribed RFC 3986 ABNF and Ruby's `URI::RFC3986_Parser`) before being
  recorded.

  Two properties keep the resulting number honest. Reject probes use only
  `http`/`https`/`ftp`/`file`, so a rejection is attributable to the **grammar**
  rather than to the ADR 0004 closed scheme set — otherwise the set would credit
  rurl for rejecting `sc://…` for entirely the wrong reason. And five probes
  record inputs the RFC grammar **admits** while rurl declines by policy; these
  carry a `rurl_deviation` naming the owning ADR and are reported on their own
  line, **excluded** from the conformance score. Counting them as conformance
  would let rurl raise its own "RFC conformance" by rejecting more of what the
  RFC allows — a metric that rewards the opposite of what it claims to measure.

  Updated picture on the 257 rows carrying an RFC oracle: rurl matches the
  standard on **164** and departs on **93** — 81 where it rejects what RFC 3986
  admits, 12 where it accepts what RFC 3986 does not. The 2.7.0 figure was
  158/99; binding the generic-URI gate uniformly (above) moved exactly six rows
  from over-permissive to conformant-reject. Analysis only — no behavior
  changed in this entry. (RURL-wlqhmbdw.)

## rurl 2.7.0

### Breaking changes

- **`file:` URLs are now parsed in rurl rather than by libcurl under
  `url_standard = "rfc3986"` and the default (`NULL`) selector**, and are
  decided by an explicit two-gate model. Previously these rows went to
  `curl::curl_parse_url()`, whose `file:` behavior is a property of the libcurl
  *build*: Windows builds enable drive-letter and `file://host` handling that
  Unix builds reject, so identical input returned `ok` on Windows and `error`
  on Linux/macOS. Parse output no longer depends on the operating system.
  (`url_standard = "whatwg"` already had its own in-tree `file:` parser and is
  unchanged.) The gates are:

  - **Gate 1 — the string must be a valid RFC 3986 URI.** RFC 8089's normative
    grammar is a strict *subset* of RFC 3986, but its Appendix E/F "nonstandard
    variations" partly escape it: `drive-letter = ALPHA ":" / ALPHA "|"` is not
    valid RFC 3986, because `|` is absent from `pchar`. So `file://C|/x`,
    `file:///path\to\file` (Appendix E.4 calls the backslash "forbidden by both
    [RFC1738] and [RFC3986]"), and a literal `file://[example]/` are now
    errors. The percent-encoded forms remain valid — `file://C%7C` parses,
    because percent-encoding is the legal way to carry `|` in a `reg-name`.
  - **Gate 2 — RFC 8089 §2's narrowing of the authority is enforced.** A port
    is now a parse error (`file://example.com:80/path`): §2's
    `file-auth = "localhost" / host` has no port and no appendix supplies a
    production for one. Userinfo is now *parsed* and surfaced in the `user`
    column rather than silently discarded, because Appendix E.1/F does supply
    `file-auth = "localhost" / [ userinfo "@" ] host`. Query and fragment are
    unaffected: RFC 8089 never mentions either, so both are inherited generic
    RFC 3986 components — and RFC 3986 §3.5 states fragment semantics "cannot
    be redefined by scheme specifications", so RFC 8089 could not have
    restricted the fragment even had it wanted to. `file:///doc.pdf#page=2`
    therefore keeps working, which RFC 8118 §3 (`application/pdf`) depends on.

  Verified against two independent implementations: Ruby's
  `URI::RFC3986_Parser` draws the same accept/reject line on every applicable
  case, and Node's WHATWG parser repairs exactly the forms the `whatwg` profile
  repairs. (RURL-obsweger.)

- The `file-forbidden-component` diagnostic is replaced by
  **`file-userinfo-extension`** (userinfo present, permitted by RFC 8089
  Appendix E.1's extended grammar) and **`file-component-outside-rfc8089`** (a
  query or fragment, inherited from RFC 3986). The old name asserted something
  the RFCs contradict: of the four components it covered, only `port` was ever
  forbidden, and that is now a parse error rather than a diagnostic.

- **Returned character components now carry an explicit `Encoding()` mark.**
  Every character component returned by `safe_parse_url()`, `safe_parse_urls()`
  and the accessors is declared UTF-8, so a value holding non-ASCII reports
  `Encoding() == "UTF-8"` where it previously reported `"unknown"`. The
  **bytes are unchanged** and `==` comparison is unaffected, but `identical()`
  against an unmarked literal — and anything else that inspects the mark — can
  change result. Pure-ASCII values still report `"unknown"`: that is simply how
  R represents an ASCII string, not a missing declaration. This is the one part
  of the locale-determinism fix below that is observable in a UTF-8 session;
  everything else about that fix is a no-op there.

### New features

- Domain, TLD, and subdomain extraction can now resolve against a caller-
  supplied Public Suffix List **per request** via a new `engine` argument on
  `safe_parse_url()`, `safe_parse_urls()`, `get_domain()`, `get_tld()`,
  `get_subdomain()`, `get_host()`, and `get_clean_url()` (and, through `...`,
  `canonical_join()`). Pass a `pslr::psl_engine()` snapshot to pin a specific
  list version or load an alternate list — for example
  `pslr::psl_engine(source = "path", path = ...)` — without mutating any global
  state (`pslr::psl_use()` is never involved). The default, `engine = NULL`,
  resolves against `pslr`'s session-global default list, exactly as before —
  every existing call path is byte-identical. The engine identity is folded
  into the parse cache key, so switching engines never reuses another engine's
  memoized domain/TLD. Requires `pslr` (>= 1.1.0). **Process-local:** a
  `psl_engine()` holds a C++ external pointer that does not serialize across R
  sessions or parallel workers — build one in the process that uses it; never
  cache it to disk or send it to a worker. (RURL-mhibnqbd; PSLR-onruvdfw.)

- Under `scheme_acceptance = "general"`, the standard component accessors now
  extract the web-y parts of a `mailto:` recipient: `get_host()`,
  `get_domain()`, `get_tld()`, `get_subdomain()`, `get_user()`, and
  `get_userinfo()` return the first recipient's domain / registrable domain /
  public suffix / subdomain / local-part, decomposed by the same Public Suffix
  List seam a web host uses (an email domain and an `http` host take identical
  branches). This reuses the existing accessors rather than adding
  email-specific equivalents, and unifies with the scheme-less `user@host`
  behaviour. `get_user()` / `get_userinfo()` gain the `scheme_policy` /
  `scheme_acceptance` / `url_standard` arguments to reach it. Extraction is
  metadata only — a `mailto:` `clean_url` and round-trip are unchanged — and is
  a strict no-op under the default `scheme_acceptance = "web"`. See ADR 0012 D7.

- New companion helper `get_mailto_recipients()` reports structural,
  per-recipient **facts** about the positional recipient list of a `mailto:`
  URL (the comma-separated `addr-spec`s before `?`, RFC 6068 §2). It returns a
  `data.frame` with one row per recipient, classifying each against three
  distinct, separately-named grammars — RFC 6068 `local-part` and domain form,
  and the SMTP (RFC 5321) mailbox right-hand side — plus a non-validating
  `public_suffix_known` flag. The positional list is tokenized on the raw
  source *before* percent-decoding, so `%2C` is never a recipient separator and
  an encoded quote/bracket (`%22`, `%5B`/`%5D`) still protects a raw comma.
  Facts only, never a gate, and no new `safe_parse_url` columns (ADR 0006 /
  0012 D7). An opt-in `smtp_wire = TRUE` argument adds the SMTP transport facts
  that require a serialized wire projection — domain wire form (ASCII / A-label
  / U-label / address-literal), the SMTPUTF8 envelope mode (RFC 6531/6530,
  triggered by a non-ASCII local-part *or* a U-label domain), and the RFC 5321
  64-octet local-part and 256-octet forward-path limits measured on UTF-8 wire
  bytes. hfield (`to`/`cc`/`bcc`) address-lists remain out of scope.

- New `scheme_acceptance` argument (`"web"` / `"general"`) on the parse and
  accessor functions, controlling **which URL shapes are accepted at all**.
  `"web"` (the default) keeps today's behavior byte-for-byte — only the
  historical special-scheme web set parses. `"general"` turns rurl into a
  general URL parser: opaque, non-special, RFC 3986-generic, and `file:` URLs
  parse and round-trip, and `ws:` / `wss:` are recognized as WHATWG-special
  schemes. This is a new axis, **orthogonal** to `url_standard` (which controls
  *interpretation*) and `scheme_policy` (which controls input *leniency*). See
  ADR 0012.

- `get_scheme()` and `get_scheme_class()` now carry the `scheme_acceptance`
  (and `scheme_policy` / `url_standard`) argument, completing the
  `get_scheme` → `get_scheme_class` cascade for opaque and non-special
  schemes. Under `scheme_acceptance = "general"`, `get_scheme("mailto:x",
  url_standard = "rfc3986", scheme_acceptance = "general")` returns `"mailto"`
  and `get_scheme_class()` classifies it as `"non-special"`; the default
  `"web"` acceptance is unchanged (opaque schemes remain `NA` /
  `"missing-or-error"`).

- New `profile` argument (`"browser"` / `"whatwg"` / `"rfc-syntax"` / `"seo"`,
  with `"canonical"` an alias of `"seo"`) on `safe_parse_url()`,
  `safe_parse_urls()`, and `get_clean_url()`, plus a companion inspector
  `url_profile()`. A profile is public sugar that bundles the acceptance,
  interpretation, leniency, and canonicalization knobs the lower layers expose
  under one **inspectable** name — `url_profile("browser")` returns the exact
  `knob = value` set it resolves to. Explicit arguments always override the
  profile (the "iron rule"), so a profile only fills slots you did not set
  yourself. `"browser"` gives a browser-*like* posture (WHATWG interpretation,
  general acceptance, scheme inference, and a bounded fixer for rurl's historical
  scheme prepending); `"whatwg"` is the strict absolute-URL spec posture and
  **rejects** scheme-less input; `"rfc-syntax"` parses RFC 3986 generic syntax as
  *parsing*, not normalization (case and dot-segments preserved); `"seo"` names
  rurl's origin-cleaning intent. `canonical_join()` also accepts `profile`,
  forwarded through its `...` (like `url_standard`); on the profile path its
  `url_standard` conflict check is skipped, matching `safe_parse_url()`. The
  default (`profile = NULL`) is byte-for-byte unchanged. See ADR 0012.

- `get_url_diagnostics()` gains 11 additional companion facts, surfaced only
  under `scheme_acceptance = "general"`, describing outcomes specific to
  general-mode parsing (opaque paths, non-special authorities, and the RFC 3986
  grammar gate). As with all diagnostics these are companion facts only —
  `safe_parse_url()`'s columns are unchanged (ADR 0006) — and under the default
  `scheme_acceptance = "web"` the diagnostics surface is byte-identical to
  before. See ADR 0012.

- New practical host-validation **policy** helpers `is_valid_host()` and
  `check_hosts()`. rurl's `url_standard` profiles deliberately match the URL
  *standards*, so hosts such as `a+b.example` (a valid RFC 3986 reg-name),
  `_dmarc.example.com` (a valid DNS owner name), and `-example.com` all parse
  successfully. These helpers answer the separate, product-level question of
  whether a parsed host is usable as a practical **web** hostname, **dns** owner
  name, **registrable** site host, or **seo**-safe host (plus the loosest
  **url** rule). `is_valid_host(url, rule = "web")` returns a logical vector;
  `check_hosts(url, rules = ...)` returns a tabular report with a logical column
  per rule and a `reasons` list-column of the host facts observed. This is a
  policy layer on top of parsing, not parser conformance and not a conformance
  oracle: it never changes `parse_status`, never widens `safe_parse_url()`
  (ADR 0006), and the absence of a `reasons` token is not a validity claim
  (ADR 0012 D5). They default to `url_standard = "whatwg"`.

### Bug fixes

- **rurl's output no longer depends on the R session's character set.**
  Parsing the same URL in a non-UTF-8 session — `LC_ALL=C`, or the non-UTF-8
  Windows locale that win-builder and many CRAN Windows checks run in —
  returned different, and in several cases simply wrong, results from the same
  call in a UTF-8 session. rurl handed strings to `pslr`, to `punycoder`, to
  its own percent-encoder and to its own cache layer without ever declaring
  their encoding, and the operations downstream (`enc2utf8()`, `utf8towcs()`,
  environment-name lookup, libcurl's own host handling) each re-read those
  bytes in whatever `LC_CTYPE` the session happened to have. The symptoms, all
  measured:

  - `domain`, `tld`, `domain_ascii`, `domain_unicode`, `tld_ascii` and
    `tld_unicode` came back **`NA` for every non-ASCII (IDN) host**:
    `get_domain("http://bücher.münchen.de/p")` returned `NA` under `LC_ALL=C`
    and `"münchen.de"` under UTF-8, because `pslr` received an undeclared
    string and re-decoded it in the session locale.
  - A non-ASCII path was **corrupted**, not merely marked differently:
    `/école` percent-encoded to `/%3Cc3%3E%3Ca9%3Ecole` instead of
    `/%C3%A9cole`, because the encoder transcoded from the session locale
    before reading the octets WHATWG's percent-encode set is defined over.
  - `get_mailto_recipients()` **errored** — `invalid input '<U+FFFF>' in
    'utf8towcs'` — on a recipient whose local part contained U+FFFF.
  - A host that libcurl percent-decodes to invalid UTF-8, such as
    `http://example.com%80/`, was **accepted** under `LC_ALL=C` and rejected in
    a UTF-8 session: `curl::curl_parse_url()` is itself locale-dependent on
    those bytes. rurl now pins curl's UTF-8-session outcome in every locale.
  - Some `file:` URLs with percent-encoded non-ASCII hosts, such as
    `file://a%C2%ADb/p`, were rejected under `LC_ALL=C` although they parse in
    a UTF-8 session, because the decoded host missed the UTS-46 mapping its
    literal form gets.

  Encoding is now **declared** — with `Encoding<-`, never with `enc2utf8()`,
  which transcodes from the session locale and was the defect in most of these
  paths — at the points where a string enters the pipeline and at the two
  points where a result is assembled. The cache layer additionally derives an
  ASCII-safe key one seam beneath its callers, since an environment name is
  rendered in the native encoding; that also removes 347 "unable to translate
  ... to native encoding" warnings from a non-UTF-8 check run, which is its own
  hazard on CRAN. The ADR 0002 Punycode helpers are untouched: every fix sits
  upstream or downstream of them.

  A UTF-8 session is byte-for-byte unaffected by the whole change set — every
  fix makes a non-UTF-8 session behave the way a UTF-8 session already did.
  Measured over the full test suite, `LC_ALL=C` moves from 99 failures and 348
  warnings to 0 failures and 1 warning; the one remaining warning is upstream
  in `pslr` and is filed there as PSLR-jzdhhugc. A new test file states the
  contract as executable assertions — `Encoding()` marks, `charToRaw()` byte
  equality, and each of the regressions above as a named case — and a
  `Tests (LC_ALL=C)` CI job, which asserts the character set it actually
  received before running anything, now runs the suite in a non-UTF-8 locale on
  every push. (RURL-vzqmwthu.)

- **Scheme and host normalization no longer depends on the R session's
  locale.** rurl lowercased URL syntax with `stringi::stri_trans_tolower()`
  without an explicit `locale=`, so it inherited ICU's locale-tailored case
  mapping. In a Turkish or Azeri session (`tr`, `az`; Lithuanian `lt` is a
  milder variant) ICU correctly maps `I` to `ı` (dotless i) — correct
  orthography, but wrong for protocol syntax. The visible symptoms: under the
  default `case_handling = "lower_host"`, `https://WIKI.example.com/p` returned
  the host `wıkı.example.com`, a **different domain** than the one requested;
  and `FILE:///tmp/x` returned `parse_status = "error"`, because scheme
  recognition folded `FILE` to `fıle`, which is not a supported scheme (`file`
  is the only supported scheme containing the letter `i`, so `HTTPS:` and
  friends were unaffected). Scheme and host case normalization is defined over
  an ASCII grammar — RFC 3986 §6.2.2.1 and the WHATWG URL Standard's "ASCII
  lowercase" — so every URL-syntax case-mapping site now uses ASCII-only
  mapping, which removes locale, ICU version, and Unicode version from that
  path entirely. The one place a caller explicitly asks for case transformation
  of free text, `case_handling` applied to the **path**, keeps full Unicode case
  mapping but now pins a non-tailoring locale, so its result is likewise stable
  across sessions. `case_handling = "upper"` applied to the host is a
  presentation transform rather than protocol syntax — no standard uppercases a
  host — so it too keeps full Unicode case mapping under the pinned locale, and
  its output is unchanged (`bücher.example` still uppercases to
  `BÜCHER.EXAMPLE`). (RURL-ugfpuotu.)

### Internal

- **The RFC 3986 conformance oracle has been audited and repaired.** No
  behavior changed — but a published claim did. The `rfc3986_expected` column
  of the external conformance fixture was transcribed from the WHATWG
  web-platform-tests, whose must-fail expectations answer "what does the
  *WHATWG* parser reject" — a different question from what RFC 3986 rejects. On
  75 rows it therefore asserted the RFC rejects strings the RFC plainly accepts
  (an empty `reg-name`, percent-encoded octets in a `reg-name`, which §3.2.2
  does not decode for validity, and `path-rootless` forms misread as a
  userinfo), and on 20 more it recorded rurl's tolerant *output* as though the
  RFC had required it. Because rurl also declines most of the first 75 — by
  **policy** (the ADR 0004 host-shape gate, the closed scheme set), not by
  standard — the oracle and the implementation confirmed each other and the
  test suite stayed green. Nothing was visibly wrong.

  The underlying defect was the schema, not the cells: one `divergence_class`
  column carried both how the two standards relate to each other *and* whether
  rurl follows them, which made a policy rejection indistinguishable from a
  conformance result. The two facts now sit on separate axes —
  `divergence_class` is purely standard-versus-standard, and a new
  `rurl_deviation` column names the ADR or ticket that owns each departure.
  A new test checks the RFC column against a transcription of the RFC 3986 ABNF
  itself, since a fixture cell cannot be validated by the parser it exists to
  validate; the derivation is `tools/oracle-audit-rfc3986.R`, which scores every
  row against two independent referees (that grammar and Ruby's
  `URI::RFC3986_Parser`). They agree on all 282 runnable rows.

  Honest picture on the 257 rows carrying an RFC oracle: rurl matches the
  standard on 158 and departs on 99 — 81 where it rejects what RFC 3986 admits,
  18 where it accepts what RFC 3986 does not. Each of the 99 now cites its
  owner. (RURL-nknytzxz.)

## rurl 2.5.0

### New features

- `path_encoding` (`"keep"` / `"encode"` / `"decode"`) is now an **orthogonal**
  presentation knob that layers on any `url_standard` profile, mirroring
  `host_encoding`. Previously, setting `url_standard` and an explicit
  `path_encoding` together was an error (the profile "governed" the path
  encoding). That asymmetry is gone: the profile now sets an internal path
  *identity* mode, and the public `path_encoding` *presentation* applies on top.
  So `get_path(u, url_standard = "whatwg", path_encoding = "encode")` emits the
  WHATWG-parsed path in browser form (`/école` → `/%C3%A9cole`). Only `"keep"`
  (the default) preserves a profile's canonical identity path verbatim;
  `"encode"`/`"decode"` are presentation forms that may re-encode or decode
  reserved octets (e.g. `%2F` ↔ `/`), independent of whether a profile is set.
  Fully backward compatible — `url_standard = NULL` and profile + default
  `"keep"` are byte-for-byte unchanged; only previously-rejected combinations
  now compute. See ADR 0011.

### Bug fixes

- `url_standard = "rfc3986"` now accepts literal RFC 3986 `reg-name`
  sub-delimiters in hosts (`! $ & ' ( ) * + , ; =`). These URLs previously
  inherited libcurl's narrower host character set and returned `error`; they now
  parse as RFC-legal registered names while `url_standard = NULL` keeps the
  historical curl behavior. This moves the RFC 3986 parity probe set from 9/19
  to 19/19.

- `url_standard = "whatwg"` now accepts WPT-valid IPv4 hosts with empty hex
  zero parts, such as `https://0x.0x.0` and `https://0x.0x.0x.0x`, and
  serializes them as `0.0.0.0`.

- `url_standard = "whatwg"` with `host_encoding = "idna"` now applies UTS-46
  ignored-code-point mappings during IDNA presentation, so
  `https://a%C2%ADb/` serializes canonically as `https://ab/` instead of
  punycoding the soft hyphen.

## rurl 2.4.0

### New features

- New `scheme_policy` argument (`"infer"` / `"require"`) on the parse and
  accessor functions, controlling whether scheme-less, host-shaped input is
  **accepted**. `"infer"` (default) keeps today's behavior byte-for-byte —
  scheme-less input like `example.com` gains a fabricated `http://` (a
  browser-omnibox-style affordance) and parses. `"require"` opts out of that
  inference: scheme-less host-shaped input becomes `parse_status = "error"`,
  giving a strict, pure-parser posture. This is a new axis, **orthogonal** to
  `protocol_handling` (which only controls how the scheme is *presented* in
  `clean_url`, not whether input is accepted) and to `url_standard` (which
  controls *interpretation*). Scheme-relative `//host` input keeps its own
  dedicated axis, `scheme_relative_handling`, and is **not** governed by
  `scheme_policy`. See ADR 0010. One practical consequence: rurl's
  scheme-inference divergences from a pure WHATWG parser (e.g. accepting a
  scheme-less backtick host under `url_standard = "whatwg"` where Ada rejects
  the scheme-less form for want of a base URL) are now opt-out-able — under
  `scheme_policy = "require"` rurl rejects them too, matching Ada on that axis.

## rurl 2.3.0

### Bug fixes

- `url_standard = "whatwg"` now strips ASCII tab (`U+0009`), LF (`U+000A`), and
  CR (`U+000D`) from the input before parsing, matching the WHATWG URL
  Standard's first parse step. Previously rurl rejected a control character in
  the authority (libcurl errors), so adversarial hosts that browsers accept
  after stripping — `http://ex<TAB>ample.com/` → `example.com`,
  `https://n.pr<LF>e.gg` → `n.pre.gg`, and CRLF-injection shapes like
  `http://127.0.0.<CR><LF>1:6379…` → `127.0.0.1` — returned `error`. They now
  parse under `"whatwg"`. The strip is **not silent**: it fires a new
  `control-char-stripped` diagnostic (see `get_url_diagnostics()`), keeping with
  the facts-not-policy design. `url_standard = "rfc3986"` and the default
  (`NULL`) are unchanged — RFC 3986 has no strip step and requires such bytes to
  be percent-encoded, so they still reject.

- `url_standard = "whatwg"` now **rejects** WHATWG forbidden host/domain code
  points instead of accepting them as registered names with `warning-no-tld`. A
  special-scheme host is a domain, and WHATWG fails the host parse when
  domain-to-ASCII yields a forbidden code point (`|`, `^`, DEL, space, …) or when
  domain-to-ASCII itself fails (a disallowed code point such as U+FFFD/U+FFFF, or
  a UTS-46-ignored code point like the U+00AD soft hyphen collapsing a label to
  empty). These now return `parse_status = "error"` under `"whatwg"`, matching
  the web-platform-tests failure corpus. `url_standard = "rfc3986"` and the
  default (`NULL`) are unchanged — RFC 3986 has no forbidden-host-code-point
  concept, so these stay permissive registered names there. The reversible-host
  and Punycode helpers are untouched (ADR 0002); this is a separate reject gate.

- `url_standard = "whatwg"` now maps the three UTS-46 alternative full-stop code
  points — U+3002 (ideographic), U+FF0E (fullwidth), and U+FF61 (halfwidth
  ideographic) — to ASCII `.` in the authority before parsing, matching
  WHATWG domain-to-ASCII. Previously a Unicode-dot host such as
  `http://127。0。0。1/` was kept as a literal registered name (`warning-no-tld`);
  it now coerces to the canonical dotted-quad `http://127.0.0.1/`, closing an
  SSRF-relevant loopback/metadata obfuscation that browsers resolve. IDN names
  have their separators normalized the same way (`例え。jp` → `例え.jp`). The
  mapping is scoped to the authority: a full-stop variant in the path, query, or
  fragment is left literal. `url_standard = "rfc3986"` and the default (`NULL`)
  are unchanged — RFC 3986 has no UTS-46 mapping, so these bytes stay literal.

- `url_standard = "whatwg"` now **accepts** the 15 ASCII host code points that
  libcurl rejects but the WHATWG URL Standard keeps in the host —
  `! " $ & ' ( ) * + , ; = ` { }`. rurl delegates host parsing to libcurl, whose
  host allowed-set is narrower than WHATWG's, so a host such as `http://a'b/`
  (the residual behind the ada-008 boundary case) previously returned `error`.
  A special-scheme URL whose host carries one of these code points now parses,
  with the host preserved byte-for-byte (`http://a'b.example.com/` keeps
  `a'b.example.com`). The fix is a pre-parse shim that lets libcurl read the URL
  structure and then restores the true host, so every downstream check —
  including the forbidden-host-code-point reject — still runs on the real host:
  `%` (a forbidden domain code point) and `|`/`^` stay rejected. It fires a new
  `host-charset-shimmed` diagnostic. `url_standard = "rfc3986"` and the default
  (`NULL`) are unchanged — they inherit libcurl's stricter charset and still
  reject. The reversible-host and Punycode helpers are untouched (ADR 0002); see
  ADR 0009. This first slice covers WHATWG special schemes (http/https/ftp);
  ftps and opaque hosts remain a documented follow-up.

### Diagnostics

- New `get_url_diagnostics()` token `control-char-stripped`, emitted under
  `"whatwg"` on any URL from which an ASCII tab/LF/CR was removed.

- New `get_url_diagnostics()` token `host-charset-shimmed`, emitted under
  `"whatwg"` on any URL whose host carried a libcurl-rejected-but-WHATWG-valid
  code point (`! " $ & ' ( ) * + , ; = ` { }`) that the shim accepted.

### Documentation

- Clarified `path_encoding` as the readable-vs-browser path *presentation*
  choice (the analog of `host_encoding`): the default `"keep"` is a faithful
  passthrough that forces neither form, `"encode"` renders the
  browser/percent-encoded path (`/école` → `/%C3%A9cole`, `/"path"` →
  `/%22path%22`), and `"decode"` renders the readable path. No behavior change —
  the knob already existed; the docs now make the choice discoverable and note
  that a `url_standard` profile does not switch the path to the browser-encoded
  rendering (a readable path stays readable).

## rurl 2.2.2

### Bug fixes

- `url_standard = "whatwg"` now rejects obfuscated numeric hosts that libcurl
  leaves as registered names. WHATWG parses any host whose final label is a
  number (decimal, or a `0x` hex literal) as an IPv4 address and fails the whole
  host parse when that IPv4 parse is invalid. Previously rurl only applied this
  rule to forms libcurl had already coerced to an IPv4 literal, so mixed
  reg-name/number hosts (`http://foo.09`, `http://foo.0x4`), leading-zero /
  invalid-octal octets (`http://1.2.3.08`), and `>4`-part or trailing-dot forms
  (`http://0x1.2.3.4.5`, `http://1.2.3.08.`) slipped through as
  `warning-invalid-tld` instead of `error`. They now return
  `parse_status = "error"` under `"whatwg"`, matching the WHATWG URL Standard
  and the web-platform-tests failure corpus. `url_standard = "rfc3986"` and the
  default (`NULL`) are unchanged — RFC 3986 has no numeric-host rule, so these
  remain valid registered names there.

## rurl 2.2.1

### Packaging

- Pin the sibling `Remotes:` to release tags (`pslr@v1.0.2`,
  `punycoder@v1.2.0`) to match the `Imports` version floors. The Remotes
  previously tracked each sibling's default branch; once `pslr`'s development
  head began pinning `punycoder@v1.2.0`, `pak` saw two conflicting sources for
  `punycoder` and could not solve the dependency graph, breaking a fresh
  install / CI resolution of `rurl` and of any package depending on it. No
  user-facing code change.

## rurl 2.2.0

### New features

- New `url_standard` selector on `safe_parse_url()`, `safe_parse_urls()`, the
  `get_*()` accessors, and `canonical_join()`: `NULL` (default), `"rfc3986"`, or
  `"whatwg"`. It selects a coherent set of
  standard-conformant behaviors for the axes it governs — path percent/dot
  handling, the host IPv4/reg-name model, and `case_handling` — so callers no
  longer hand-assemble the low-level knobs to approximate a standard. Passing a
  governed low-level knob (`path_encoding`, `path_normalization`,
  `case_handling`) with a value the selected profile would not choose is an
  error (also across `canonical_join()`'s `...`). **`url_standard = NULL` is
  fully backward compatible** — byte-for-byte identical output and unchanged
  result shape; there is no default flip. Under `"rfc3986"` only unreserved path
  bytes are decoded (`%2F` stays encoded) and numeric-looking non-IPv4 hosts are
  parsed as `reg-name`; under `"whatwg"` encoded unreserved bytes are preserved
  and valid numeric IPv4 forms are coerced per the WHATWG host model.
- New standalone `port_handling` option controlling whether the port appears in
  `clean_url`: `"exclude"` (default, today's behavior), `"keep"`,
  `"strip_default"`, `"strip_all"`. It is editorial and
  standard-independent; under `url_standard = "whatwg"`, `"keep"` elides a port
  matching its special scheme's default (http:80, https:443, ftp:21).
- Under `url_standard = "whatwg"`, a literal backslash is recognized as a path
  separator for WHATWG-special schemes (`http`/`https`/`ftp`, not `ftps`), as
  browsers do. `%5C` is never treated as a separator, and
  `"rfc3986"` / no selector leave backslashes inert.
- New `resolve_url(relative_or_absolute, base_url, url_standard = NULL, ...)`:
  RFC 3986 §5 reference resolution (empty / fragment-only / query-only /
  scheme-relative / absolute-path / relative-path merge) composed over the same
  parsing machinery, returning the canonical `clean_url` of the resolved
  reference. Vectorized, with the base recycled.

### Diagnostics and classification helpers

- New companion helpers, gated on `url_standard` (return `NA` with no selector),
  surface metadata **without** widening the parse result shape:
  `get_host_type()` (domain / ipv4 / ipv6 / reg-name / missing),
  `get_scheme_class()` (WHATWG special / non-special / missing-or-error), and
  `get_url_diagnostics()`.
- Diagnostics vocabulary: `ipv4-*` numeric-host tokens, `encoded-dot-segment`,
  `encoded-reserved-path-byte`, `explicit-default-port` / `non-default-port`,
  `invalid-reverse-solidus`, and the DNS/UTS-46 tokens `domain-label-too-long`,
  `domain-name-too-long`, `domain-empty-label`, `domain-hyphen-violation`,
  `domain-std3-violation`. Diagnostics are **facts, not policy**:
  a token describing an input shape fires identically under both standards, so a
  link-graph builder can ignore them while an SSRF/allowlist guard rejects on
  them.

### Documentation

- New `url_standard` vignette walking through RFC 3986 vs WHATWG on the canonical
  cases (`%41%42`, `%2F`, `2130706433`), the diagnostics, and the migration
  notes (pin `url_standard = "whatwg"` for WHATWG-aligned link identity on the
  governed axes; the interim `path_encoding = "keep"` stopgap is a collision fix,
  not a full standard profile).

## rurl 2.1.0

### New features

- `safe_parse_url()` and `safe_parse_urls()` gain four additive result columns —
  `domain_ascii`, `domain_unicode`, `tld_ascii`, and `tld_unicode` — exposing
  the registrable domain and public suffix in **both** canonical spellings,
  independent of `host_encoding`. `host_encoding` is a
  *rendering* choice, so the existing `domain`/`tld` columns follow it (under
  the default `"keep"`, a Unicode host and its Punycode A-label render
  differently and do not compare equal). The new columns are stable *identity*
  keys instead: `http://münchen.de` and `http://xn--mnchen-3ya.de` share one
  `domain_ascii` (`"xn--mnchen-3ya.de"`) and one `domain_unicode`
  (`"münchen.de"`), so a consumer can build an encoding-independent key from a
  single parse rather than re-parsing with a forced `host_encoding`. For
  ASCII-only hosts the two spellings are equal; IP hosts and null rows yield
  `NA`. The values were already computed internally, so this is purely additive
  and existing `domain`/`tld` semantics are unchanged.

## rurl 2.0.0

### New features

- `safe_parse_url()` and `safe_parse_urls()` gain opt-in query-string handling
  for `clean_url`. New `query_handling` option:
  `"drop"` (default — `clean_url` stays query-free, exactly as before),
  `"filter"` (keep contentful params, drop known trackers such as `utm_*`,
  `fbclid`, `gclid` via a built-in denylist), `"allow"` (keep only names in
  `params_keep`), and `"keep"` (keep every param, canonicalized). Supporting
  options: `params_keep`, `params_drop` (glob-aware, `*`-only), `sort_params`,
  `empty_param_handling`, `params_case_sensitive`, and `decode_plus`. The raw
  `query` result field is untouched — it always reports the faithful original.
  All defaults preserve current output. Because `canonical_join()` forwards
  `...` to `safe_parse_urls()`, these options also flow into the join key, so a
  non-`"drop"` mode makes `?id=1`/`?id=2` stop collapsing while `utm`-only
  differences still collapse under `"filter"`.
- `get_clean_url()` gains the seven query-filter arguments (`query_handling`,
  `params_keep`, `params_drop`, `params_case_sensitive`, `sort_params`,
  `empty_param_handling`, `decode_plus`), reaching full parity with the parse
  engine. A filtered cleaned URL is now available directly —
  `get_clean_url(u, query_handling = "filter")` — instead of only via
  `safe_parse_url(u, query_handling = "filter")$clean_url`. Defaults are
  unchanged (`query_handling = "drop"`), so existing output is byte-identical.
- `get_query()` gains the same query-filter engine arguments
  (`query_handling`, `params_keep`, `params_drop`, `params_case_sensitive`,
  `sort_params`, `empty_param_handling`, `decode_plus`), so a cleaned query can
  be pulled directly without going through `clean_url`. It
  defaults to `query_handling = "keep"` (an accessor returns the query as found
  unless you ask it to filter), and the filter runs before rendering:
  `decode = TRUE` gives the readable decoded form, `decode = FALSE` the
  canonical re-encoded form. Every existing default is byte-for-byte unchanged.
- New `query_param_summary()` introspection function tabulates the query
  parameters across a set of URLs — which names appear, what values they take,
  `n`/`n_urls` counts, and a `would_drop` column previewing what
  `query_handling = "filter"` would remove. Returns a flat
  (long) `data.frame` at `level = "param"` or `level = "value"`. Param names
  are grouped faithfully (case-sensitively) while `would_drop` honours
  `params_case_sensitive`, so you can audit a URL set before choosing a policy.
- The `clean_url` query is deliberately **exempt from `case_handling`** (query
  values are case-sensitive — tokens, IDs, signatures). Under
  `case_handling = "lower"` or `"upper"` the scheme/host/path fold but the
  appended query keeps its original case, so `clean_url` is no longer uniformly
  cased in those modes.

### Breaking changes

- `path_normalization = "none"` (the default) is now genuinely lossless for
  path structure. The request path is read from the input
  verbatim rather than from libcurl's pre-normalized path, so `.`/`..` segments
  are preserved: `"http://ex.com/a/../b"` now yields `clean_url`
  `"http://ex.com/a/../b"` instead of `"http://ex.com/b"`. Because `clean_url`
  is a `canonical_join` key, dot-segment paths that previously collided no
  longer do — pass `path_normalization = "dot_segments"` (or `"both"`) to
  resolve them. rurl now owns dot-segment resolution (RFC 3986 §5.2.4, literal
  `.`/`..` only), so a percent-encoded `%2e` is treated as an ordinary path
  byte and is **never** resolved as traversal — closing the silent
  `"/a/%2e%2e/b"` → `"/b"` rewrite libcurl used to perform. Percent-hex case is
  still canonicalized to uppercase (`%2f` → `%2F`) under `"keep"`, so encoded
  paths remain join-equivalent.
- Non-compliant input handling is now consistent and strict.
  rurl no longer fabricates an `http://` URL for scheme-less input that is not
  host-shaped: nonsense tokens (`"asdfghjkl"`, `"example"`), free text
  (`"hello world"`), and path fragments (`"/relative/path"`) now return
  `parse_status = "error"` with `clean_url = NA`, matching the behavior of
  inputs that already errored. An **explicit** supported scheme is still
  trusted, so `"http://asdfghjkl/"` remains `warning-no-tld`. Scheme-less
  `localhost` is accepted (the one allowlisted single-label host).
- IP literals are validated strictly against the *input* rather than trusting
  libcurl's legacy `inet_aton` coercion. Integer, hexadecimal, octal, and
  short-form numbers (`"12345"` → `0.0.48.57`, `"0x7f000001"`, `"192.168"`),
  out-of-range or wrong-arity dotted numbers (`"256.1.1.1"`, `"1.2.3.4.5"`),
  and **leading-zero (octal) octets** (`"192.168.010.1"`, which silently means
  `192.168.8.1`) now return `"error"` instead of a coerced address. Canonical
  literals (`"1.2.3.4"`, `"[::1]"`) are unaffected. `.detect_ip_host_vec()` is
  correspondingly tightened to reject zero-padded octets.
- Only `http`, `https`, `ftp`, and `ftps` are supported schemes (now a single
  source of truth, `.SUPPORTED_SCHEMES`). Scheme-bearing input with any other
  scheme — opaque (`mailto:`, `tel:`, `data:`) or authority-based but
  unsupported (`ws://`, `ssh://`, `redis://`) — returns `"error"`.
- New `parse_status` value `"warning-userinfo"` for scheme-less input carrying
  userinfo (e.g. `"user@example.com"`): `host`/`domain`/`tld`/`user` still
  resolve, but `clean_url` is `NA` (rurl will not fabricate a canonical URL
  from an ambiguous, email-shaped, scheme-less string). Such rows are
  non-joinable in `canonical_join()`. Input with an explicit scheme
  (`"http://user@example.com"`) is unchanged. Scheme-less `user:pass@host`
  (indistinguishable from `scheme:opaque`) remains `"error"`; use the
  scheme-relative form `//user:pass@host` to parse it.

### Performance

- The parse pipeline is split into an option-independent core (Stage A: curl
  components, IP detection, the post-www host, and the PSL domain/TLD
  decomposition) and a presentation stage (Stage B: path handling, case,
  host-encoding spelling, subdomain trimming, clean-URL assembly, status). The
  `full_parse` cache now stores Stage A, keyed only by URL, protocol/scheme
  handling, `www_handling`, and `tld_source`. Calling several accessors with
  different presentation profiles on the same URLs (e.g. `get_host()`,
  `get_domain()`, `get_tld()`, `get_clean_url()`, `get_subdomain()`) now shares
  one cache entry per URL and re-runs only the cheap Stage B, so the expensive
  curl + PSL work happens once instead of once per profile. Output is unchanged.
  Cache memory per URL also drops to a single (option-independent) entry.

- `safe_parse_urls()` now de-duplicates its input, parsing each unique URL only
  once (with cross-call reuse via the `full_parse` cache) and expanding the
  results back with `match()`. Repeated / duplicate URLs cost only the match,
  so warm and duplicate-heavy inputs are dramatically faster. `safe_parse_url()`
  (scalar) shares the same cached code path.

- Query-string parsing (`get_query(format = "list")`) is now linear in the
  number of key/value pairs (previously quadratic from incremental list
  growth), so URLs with very long query strings parse faster. Output is
  unchanged.

### Behavior changes

- Accessor results (`get_*()`) are no longer named by the input URLs. The
  `get_*()` functions now parse their input in a single vectorized pass and
  return plain unnamed vectors (or lists), instead of vectors carrying a
  `names` attribute of the input URLs. Wrap in `stats::setNames(x, url)` if you
  relied on the old names.

- The `full_parse` memoization cache is now bounded by default at 100000 unique
  url × option combinations (previously `Inf`), so parsing millions of unique
  URLs can no longer grow the cache without limit. Override with
  `rurl_cache_config(max_full_parse = Inf)` to restore the previous unbounded
  behavior; the reset-watermark semantics are unchanged.

- A present-but-empty `query`, `fragment`, `user`, or `password` component
  (e.g. the query of `"https://example.com/?"`) is now reported as `NA`
  consistently. `curl::curl_parse_url()` returns such components as `NULL` on
  some libcurl versions and `""` on others; both now normalize to `NA`, so
  output no longer depends on the installed libcurl version. This matches the
  behavior already produced on platforms where curl returned `NULL`.

### Behavior changes

- `safe_parse_urls()` now accepts a factor input, coercing it to its character
  labels up front (matching `canonical_join()`), instead of returning an
  all-`error` row for every element.

### Bug fixes

- Scheme-less `host:port` input (e.g. `"example.com:8080/x"`) is no longer
  reported as `parse_status = "error"`. It parses correctly (valid host, path,
  and `clean_url`) but the status-derivation phase mistook `example.com:` for
  an unsupported scheme and demoted it, contradicting the emitted components.
  It now reports `"ok"`, restoring the invariant that a present `clean_url`
  implies a non-error status. Genuinely unsupported/opaque schemes
  (`mailto:`, `user:pass@host`) still return `"error"`.
- path/fragment/userinfo are no longer percent-decoded during parsing;
  `path_encoding = 'keep'` now honors its contract (leaves the path
  byte-for-byte); the raw query is preserved (`?flag` stays `flag`, not
  `flag=`). NOTE: `clean_url` values change for URLs containing
  percent-encoded path bytes — since `clean_url` is a `canonical_join` key,
  `/a%2Fb` and `/a/b` no longer collide.

### Documentation

- Clarified the `path_normalization` and `path_encoding` docs to describe the
  normalization the underlying parser (libcurl) applies before rurl sees the
  path: RFC 3986 dot-segment resolution (`.`/`..`, including `%2e`/`%2E`) is
  unconditional and cannot be disabled — so `path_normalization = "none"` still
  resolves `/a/../b` to `/b` — and percent-encoding hex digits are normalized to
  uppercase (`%2f` → `%2F`), an RFC 3986 §6.2.2.1 case canonicalization that
  makes such paths compare equal in `canonical_join()`. Behavior is unchanged;
  only the documentation now matches it.

## rurl 1.4.1

### Bug fixes

- `safe_parse_url()`/`safe_parse_urls()` now recognize IPv6 address literals
  that carry an embedded dotted-quad IPv4 tail (RFC 4291 §2.2 form 3 / §2.5.5,
  e.g. `[::ffff:127.0.0.1]`, `[64:ff9b::8.8.8.8]`). Previously these fell
  through to the registered-name path, returning `is_ip_host = FALSE` and a
  spurious `warning-invalid-tld` status; they now report `is_ip_host = TRUE`
  and `parse_status = "ok"`. Both the dotted and hex-hextet spellings of the
  same address (`[::ffff:0808:0808]` vs `[::ffff:7f00:1]`) now classify
  identically. A malformed embedded tail (octet out of range) is still
  rejected.

### Infrastructure

- Added a dependency vulnerability audit against the Sonatype OSS Index via
  `oysteR` (new `Suggests`). `tests/testthat/test-security.R` runs
  `oysteR::expect_secure("rurl")` and a dedicated `security-audit.yml` workflow
  (weekly + on demand) executes it with OSS Index credentials; the test skips
  cleanly without credentials, offline, or on CRAN.
- Added a second, token-free dependency vulnerability audit against the OSV
  database (<https://osv.dev>) via `rosv` (new `Suggests`).
  `tests/testthat/test-osv.R` checks the runtime dependency closure of rurl
  (recursive `Depends` + `Imports`) at installed versions, and a dedicated
  `osv-audit.yml` workflow (weekly + on demand) executes it with no secrets;
  the test skips cleanly offline or on CRAN.

## rurl 1.4.0

### Dependencies

- The `pslr` dependency floor is now `>= 1.0.2` and the `punycoder` floor is
  `>= 1.2.0`. Those releases form the coordinated `punycoder 1.2.0`
  host-normalization API pair, so a fresh install pulls a compatible set;
  `rurl` should be submitted after both dependency updates are on CRAN.

### Accessor improvements

- `get_path()` gains `path_normalization`, `index_page_handling`,
  `trailing_slash_handling`, and `path_encoding` arguments, matching
  the corresponding options of `safe_parse_url()`.
- `get_scheme()` gains `scheme_relative_handling`.
- `get_parse_status()` gains `source` (mapped to `tld_source`) so
  warning statuses can be queried under a specific PSL section.
- `get_clean_url()` and `get_host()` gain `source` (mapped to `tld_source`).
- `get_host()` gains `host_encoding`.
- `get_domain()`, `get_tld()`, and `get_subdomain()` gain `host_encoding`,
  mirroring `get_host()`.

All new arguments default to the same values as `safe_parse_url()`, so
existing calls are unaffected.

### Behavior change

- The domain-family accessors (`get_domain()`, `get_tld()`,
  `get_subdomain()`) now follow `host_encoding` (default `"keep"`) instead
  of always returning Unicode. Under `"keep"` the emitted domain/TLD/
  subdomain mirrors the input host's own spelling: an A-label (`xn--…`)
  host yields A-label parts, a Unicode host yields Unicode parts. Pass
  `host_encoding = "unicode"` for the previous always-decoded output, or
  `"idna"` to force A-labels. This makes the domain accessors consistent
  with `get_host()`, whose `host_encoding` already defaulted to `"keep"`.

### Internal

- Parse-status string literals replaced by named constants
  (`R/status-constants.R`) and predicates (`.is_ok_status()`,
  `.is_warning_status()`, `.is_joinable_status()`).
- Cache touchpoints in `R/zzz.R` now driven from a single `.CACHE_REGISTRY`
  instead of repeating cache names by hand.
- Cleared the `lintr`/`goodpractice` findings across `R/` and the tests
  (e.g. `fixed = TRUE` dot splits, condition-message construction, dropped
  unnecessary lambdas) with no behavior change.
- `.lintr` now mirrors `goodpractice`'s linter set, so a local
  `lintr::lint_package()` matches the `goodpractice` report; intentional
  test-idiom deviations are documented in the config header.
- Restored 100% line coverage: added targeted tests for the
  `.punycode_to_unicode("")`, `.host_is_ace()`, and `.cache_enabled()`
  guard branches and the `derive_parse_status()` NA-host-dot fallback
  (and fixed an over-escaped regex literal that left the scheme-slash NA
  guard untested). The two genuinely unreachable `www`-prefix
  regex-capture fallbacks are now marked `# nocov` with justification.
- Reduced the cyclomatic complexity of `canonical_join()` (47→7),
  `get_subdomain()` (26→6), `rurl_cache_config()` (23→5), and
  `safe_parse_urls()` (19→3) by extracting named sub-helpers (e.g.
  `.cj_validate_inputs()`/`.cj_resolve_sides()`/`.cj_build_join_df()`,
  `.subdomain_labels()`, `.validate_max_full_parse()`,
  `.spu_coerce_original()`). No behavior change; no function in the package
  now exceeds the `goodpractice` cyclocomp threshold of 15.

### Documentation & metadata

- Added package-level documentation (`?rurl` / `man/rurl-package.Rd`) via a
  `"_PACKAGE"` sentinel, so the maintainer ORCID, package URLs, and the
  cross-promotion of `pslr`/`punycoder` now render on a help/landing page.
- Enabled roxygen2 markdown (`Roxygen: list(markdown = TRUE)`), regenerating
  all `man/*.Rd` (inline backticks now render as `\code{}`).
- Fixed the stale `inst/CITATION`: it now reads the version from package
  metadata (was hardcoded `0.2.0`), uses the correct title, and carries the
  maintainer ORCID. Added a root `CITATION.cff`.
- Added `X-schema.org-keywords`, the r-universe URL, and a `codemeta.json`
  for discoverability.
- Maintainer email simplified to `bartek@turczynski.pl`.

## rurl 1.3.0

### Dependencies

- Public Suffix List matching is now delegated to the `pslr` package
  (`Imports: pslr (>= 1.0.1)`). `rurl` no longer ships its own processed copy of
  the list (`R/sysdata.rda`) or its embedded matcher, and `data-raw/update_psl.R`
  has been removed. `punycoder` is now required at `>= 1.1.0`.

### Behavior changes (PSL correctness)

The embedded matcher used through 1.2.0 was not fully spec-correct. Delegating
to `pslr` fixes the following; outputs change accordingly:

- **Wildcard rules (`*.`)** are now honored by TLD extraction. For example
  `get_tld("a.b.kobe.jp")` is now `"b.kobe.jp"` (was `"kobe.jp"`).
- **Exception rules (`!`)** are now honored by TLD extraction. For example
  `get_tld("www.ck")` is now `"ck"` (was `"www.ck"`), and `get_tld("foo.ck")`
  is now `"foo.ck"` (was `"ck"`).
- **IDN hosts** now resolve a registered domain in every section. For example
  `get_domain("example.рф")` is now `"example.рф"` (was `NA`).
- `safe_parse_url()` / `safe_parse_urls()` now derive the `domain` field using
  the requested `tld_source` rather than always using the combined list, so
  `domain` and `tld` are consistent within a parse. Under
  `tld_source = "private"` (or `"icann"`), a host with no suffix in that section
  now has `domain = NA`; consequently `subdomain_levels_to_keep` is a no-op for
  such hosts (there is no registered domain to trim toward). The default
  `tld_source = "all"` is unaffected.
- Hosts under an unknown TLD continue to return `NA` for both domain and TLD
  (`rurl` queries `pslr` with `unknown = "na"`), rather than treating an unknown
  single label as a public suffix.

### Cache changes

- The per-host `domain` and `tld` memoization caches have been removed; `pslr`
  caches its own query results. `rurl_cache_config()` and `rurl_cache_info()`
  now cover only `full_parse`, `puny_encode`, and `puny_decode`, and the
  `domain` / `tld` arguments to `rurl_cache_config()` no longer exist.

## rurl 1.2.0

### Dependencies

- `punycoder` (used for IDNA/Punycode encoding and decoding) is now on CRAN.
  `DESCRIPTION` requires `punycoder (>= 1.0.0)`.

### Behavior changes

- The package-wide default for `case_handling` is now `"lower_host"` (was
  `"keep"` for `safe_parse_url()`, `safe_parse_urls()`, `get_clean_url()`, and
  the `get_*()` accessors, and `"lower"` for `get_path()`). This is the
  RFC 3986 §6.2.2.1 normalization: the case-insensitive scheme and host fold to
  lowercase while the case-sensitive path is preserved. With the previous
  defaults, hosts such as `WWW.Example.COM` and `www.example.com` did not fold
  to one identity, and `get_path()` silently lowercased paths (two pages that
  differ only by path casing collapsed to one). Pass `case_handling = "keep"`
  to restore the previous reconstruction, or `"lower"` to lowercase the whole
  URL including the path.

## rurl 1.1.0

### New features

- `canonical_join()` gains `name_A` / `name_B` arguments to set the output
  original-URL column names explicitly. They default to `NULL`, preserving the
  previous `deparse(substitute())` behavior; supply them for stable names when
  piping or passing anonymous inputs (e.g. `canonical_join(df[df$x > 1, ],
  get_b())`), which otherwise produced unstable column names.
- `canonical_join()` gains a `join_parse_status` argument controlling which
  parse statuses yield joinable keys. The default `"ok"` preserves the previous
  behavior (only `ok*` statuses join); `"ok_or_warning"` additionally treats
  the parseable-but-suspicious `warning-*` statuses (`warning-no-tld`,
  `warning-invalid-tld`, `warning-public-suffix`) as joinable, at the cost of
  more potential false-positive matches.

- Cache introspection and configuration. `rurl_cache_info()` reports the entry
  count, enabled state, and any bound for each memoization cache
  (`full_parse`, `domain`, `tld`). `rurl_cache_config()` enables or disables
  individual caches and sets an optional `max_full_parse` bound on the
  full-parse cache (default `Inf`, preserving the previous unbounded
  behavior); when the bound is reached the cache is reset so peak memory stays
  bounded. The `domain` and `tld` caches remain unbounded by design — they
  grow with the number of unique hosts, not with URL/option combinations — and
  can be disabled for workloads with very many unique hosts.

### Bug fixes

- `safe_parse_url()` now returns `port` as an integer (or `NA_integer_`), and
  `safe_parse_urls()` no longer errors on URLs that contain an explicit port
  (e.g. `http://example.com:8080/path`). Previously the scalar parser returned
  the port as a character string and the vectorized parser aborted.
- Bracketed IPv6 hosts (e.g. `http://[2001:db8::1]/`) are now correctly detected
  as IP hosts: `is_ip_host` is `TRUE`, `parse_status` is `"ok"`, and no
  TLD/domain derivation is attempted — matching how IPv4 hosts were already
  handled. An over-escaped detection pattern previously prevented this.

### Behavior changes (potentially breaking)

- `subdomain_levels_to_keep = N` (for `N > 0`) now keeps the `N` rightmost
  subdomain labels as documented, instead of silently retaining all subdomains.
  For example, `safe_parse_url("http://deep.sub.domain.example.com",
  subdomain_levels_to_keep = 1)` now returns host `domain.example.com` (was
  `deep.sub.domain.example.com`). `N = 0` (strip all) is unchanged. Code that
  relied on the previous no-op behavior for `N > 0` will see different output.

### Documentation

- Documented `clean_url` composition: it is a normalized canonical key built
  from scheme, host, and path only. Port, query, fragment, and userinfo are
  intentionally excluded, and with `path_encoding = "decode"` the path is shown
  decoded (human-readable, not guaranteed URL-safe). This matches the existing
  behavior and the key used by `canonical_join()` — no behavior change.
  Corrected a `lower_host` description that implied userinfo could be retained
  in `clean_url`, and fixed a README example whose input contained a literal
  space (now percent-encoded) so it parses as documented.

---

## rurl v1 (GitHub Release) - 2026-02-16

- Published first stable GitHub release tag: `v1`.
- Release notes added in `RELEASE_NOTES_v1.md`.
- GitHub release page: <https://github.com/bart-turczynski/rurl/releases/tag/v1>
- Package version for this release is `1.0.0` (see `DESCRIPTION`).

---

## rurl 0.3.0

This release adds powerful capabilities for URL normalization and canonical dataset joining. It significantly improves robustness in handling malformed or inconsistent URLs.

### Highlights

- New `case_handling` and `trailing_slash_handling` parameters in `safe_parse_url()` and `get_clean_url()` provide greater control over URL formatting.
- Introduced `canonical_join()` for joining datasets on normalized URL keys.
- Improved handling of non-standard or malformed schemes like `htp://`.
- Fixed parsing for schemeless URLs with ports (e.g., `example.com:8080/path`).
- More reliable fallback when `curl::curl_parse_url()` fails internally.
- Corrected regular expressions for IPv6 parsing.

---

## rurl 0.2.0

* First version for a potential CRAN submission.
* Fully tested across macOS, Windows, and Linux.
* Achieved 100% unit test coverage.
* Improved README and documentation.

This release adds robust support for internationalized domain names (IDNs),
improves punycode handling, and ensures accurate extraction of TLDs and
registered domains.

### Highlights
- Accurate TLD extraction for both ASCII and Unicode domains
- Graceful fallback when `urltools` is unavailable
- NFC normalization with `stringi`
- 100% test coverage with edge cases and punycode validation
- Improved internal helpers and clearer test diagnostics

## rurl 0.1.3

### Improvements

- Removed the dependency on the `psl` package.
- Implemented an internal registered domain extraction using the Public Suffix List.
- Added internal `update_psl.R` script to fetch and process the PSL during development.
- Improved test coverage to 100%.
- Cleaned up exports and internal helpers.
- Updated ignores.
- Tested on macOS, Windows, and Linux via rhub and win-builder.  
- CRAN checks pass with 0 errors/warnings and only standard notes.

### Documentation

- README updated to reflect the use of the PSL and internal domain logic.
- LICENSE and attribution clarified for MIT + Mozilla Public Suffix List.

## rurl 0.1.2

### Stabilization & Coverage

- Achieved **100% test coverage**.
- Added examples to all exported functions.
- Improved documentation (`@param`, `@return`, etc.) for CRAN compliance.
- Cleaned up `NAMESPACE` and removed unnecessary functions like `hello()`.
- Refined URL parsing logic and improved output consistency.

## rurl 0.1.0

- All `get_*()` functions are now vectorized and work on character vectors.
- Deprecated scalar-only behavior.
- Internal parsing made more robust using `curl` and `psl`.
- Ready for use in `mutate()` and other tidy workflows.
