# `stringi` call inventory

*Snapshot of `main` at `80d4f0e` (2026-10-12), taken for RURL-lbccaixb, the
foundation of the epic that moves string operations off `stringi`
(RURL-bjdcjeel). [ADR 0019](adr/0019-string-operations-run-in-base-r.md)
cites it. This file is a snapshot and is not kept in step with the code. The
live list is `tools/stringi-call-allowlist.csv`, which
`tools/stringi-call-gate.R` checks.*

## How it was counted

Every `stringi::` reference in `R/`, found by parsing the code with
`getParseData()`. Comments and strings are not counted. Each call counts once,
as does each function value (`.unicode_case_map_vec()` picks
`stringi::stri_trans_toupper` or `stringi::stri_trans_tolower` as a value),
and each `stringi::stri_opts_regex()` options call counts too. `R/` holds no
`stringi:::`, no bare `stri_*` call, no `library(stringi)` and no
`@importFrom stringi`. `NAMESPACE` imports nothing from `stringi`.

**Total: 190 references in 15 files.** That matches the ticket's count on
`5bf3791`, so the merges between `5bf3791` and `80d4f0e` added or removed
none.

**Changes happening while this was written.** Peer units were editing the
tree at the same time. One replaces the RFC 3986 grammar check, which removes
row 141 (`.rfc3986_match()` in `R/parse-state.R`). Another vectorizes the
WHATWG percent encoders in `R/path-query.R`, `R/parse-phases.R`, `R/parse.R`
and `R/url-key.R`, and may remove rows in those files. A removed row leaves
the gate green (it prints an "allowlist can shrink" line). A call that a
refactor moves into a new top-level function counts as a new call.

**Since the snapshot.** MR !235 (RURL-vewhvxnb, merge `f6b9b8d`) kept the
RFC 3986 class check in stringi, as ADR 0005 still requires until this ADR is
accepted, and moved it into a new function: `.rfc3986_class_ok()` in
`R/parse-state.R` now holds one `stri_detect_regex` (the linear inverted
search over the same `\x{0080}-\x{10FFFF}` class; slice oahhlnvf), and row
141 (`.rfc3986_match()`) now serves only the port regex. The allowlist was
regenerated on that base: 191 references under 123 keys.

Later merges moved four calls. MR !240 (RURL-fobwctml) removed the two
whatwg-only `stri_startswith_fixed` calls in `.apply_subdomain_policy_vec()`
and `.apply_www_if_no_subdomain_vec()`, and MR !241 (RURL-ozpxupgq) added one
in `.general_route_input_vec()` (R/parse-phases.R). The allowlist was
regenerated on main `6623cab`: 190 references under 122 keys. Two keys are
listed ahead of their code, so this MR and MR !242 (RURL-cwzickqy, parked)
pass the gate in either merge order: `stri_enc_mark` in `.prepare_url_input()`
(R/parse.R) and `stri_enc_toutf8` in `resolve_url()` (R/resolve.R). Until
MR !242 merges, the gate prints an "allowlist can shrink" line for each,
which passes.

On 2026-10-24 main `3c49b0a` was merged in. MR !246 (RURL-esztpdhx, the
whatwg full-stop variants in a percent-decoded host) added two calls in
`.apply_host_standard_model_vec()` (R/parse-phases.R): one more
`stri_detect_regex` (the non-ASCII host test `[^\u0001-\u007f]`; slice
oahhlnvf) and one `stri_replace_all_regex` (the UTS #46 full-stop variants to
"."; slice oahhlnvf, byte mode with the variants' UTF-8 byte sequences). The
allowlist was regenerated: 192 references under 123 keys, plus the two keys
still listed ahead of MR !242's code.
MR !242 merged the same day (`325f787`), so those two keys now match code:
194 references under 125 keys.

RURL-yyeohqah decided the semantic exceptions (ADR 0019, "Semantic
exceptions"). By row of the table below, and the key MR !242 added:

| Row(s) | Call | Disposition |
|---|---|---|
| 187 | `stri_enc_isutf8` in `.cache_key_ascii` | removed: the base-R key needs no validity test |
| 188-190 | `stri_escape_unicode` in `.cache_key_escape` | removed: octets hexed in base R |
| 168 | `stri_enc_toutf8` in `.latin1_input_to_utf8` | removed: exact ISO 8859-1 map |
| (MR !242) | `stri_enc_mark` in `.prepare_url_input` | removed: base `Encoding()` |
| 184 | `stri_replace_all_regex` in `.glob_to_regex` | removed: byte layer, ASCII punctuation only |
| 173 | `stri_detect_regex` in `.param_matches` | **stays**: case-insensitive arm, ICU full case folding; the case-sensitive arm moved to the byte layer |
| 181, 182 | `stri_enc_toutf8` in `.url_key_frame` | **stays**: U+FFFD decode, WHATWG maximal subpart, with its U+FEFF splice |
| 183 | `stri_numbytes` in `.url_key_frame` | not moved in this slice (left for a later one) |
| 12, 13 | `stri_trans_nfc` in the Punycode helpers | **stays** (ADR 0002); counts a leading U+FEFF, no guard |
| 136, 137 | `stri_trans_toupper` / `stri_trans_tolower` in `.unicode_case_map_vec` | **stays**, `"en_US_POSIX"` locale and `"0"` splice guard |

The gate then reports 187 references, with five "allowlist can shrink"
lines for the removed keys.

## Legend

- **Family**: what the call does. *detect* returns a logical. *count*
  returns a count. *replace* rewrites text. *locate* returns positions.
  *capture* returns a match and its groups. *cut* takes a substring by
  position. *length (offset)* is a length that feeds a cut. *split* returns
  fields. *options* builds a `stri_opts_regex()` list. *encoding*, *escape*,
  *normalization* and *case map* are the semantic exceptions.
- **Pattern**: the regex or literal as the engine sees it, after R's string
  escapes are undone. `<expr>` marks a pattern built at run time.
- **Unicode semantics**: the parts of the call where ICU and byte-mode PCRE
  behave differently. "none" means the pattern is ASCII-structural and byte
  mode gives the same answer on valid UTF-8. The traps are measured below.
- **Target**: the mode ADR 0019 expects the slice to use. *bytes* is
  `perl = TRUE, useBytes = TRUE` through `R/strings-base.R`. A *semantic
  exception* gets its own decision in RURL-yyeohqah. *stringi (stays)* keeps
  the call.
- **Slice**: the ticket that migrates the call.
  - `oahhlnvf`: RURL-oahhlnvf, boolean and replacement operations (detect,
    prefix test, count, replace).
  - `yqmoztoj`: RURL-yqmoztoj, offset-bearing operations (locate, capture,
    cut, length, split, and the `stri_opts_regex()` objects their consumers
    take). A position and the cut that uses it move together, so they always
    count the same unit.
  - `yyeohqah`: RURL-yyeohqah, the semantic exceptions (the cache-key escape,
    encoding conversion, the Unicode glob) and confining NFC and case mapping
    to `stringi`.
  - RURL-dddxutjf, the close-out, owns no call. The allowlist shrinks at
    every merge: each migration slice runs
    `Rscript tools/stringi-call-gate.R --write-allowlist` and commits the
    shrink in the same MR (ADR 0019, decision 4). The close-out confirms that
    the list holds only the calls that stay, removes the ADR 0005 U+FEFF
    guards that the migration made redundant, and settles `Imports:`.

## Totals

| Slice | References |
|---|---|
| RURL-oahhlnvf (boolean / replacement) | 112 |
| RURL-yqmoztoj (offset-bearing) | 64 |
| RURL-yyeohqah (semantic exceptions, NFC, case map) | 14 |
| **Total** | **190** |

| File | oahhlnvf | yqmoztoj | yyeohqah | Total |
|---|---|---|---|---|
| `R/accessors.R` | 0 | 3 | 0 | 3 |
| `R/diagnostics.R` | 7 | 1 | 0 | 8 |
| `R/domain.R` | 0 | 2 | 2 | 4 |
| `R/email-diagnostics.R` | 1 | 0 | 0 | 1 |
| `R/parse-phases.R` | 78 | 41 | 2 | 121 |
| `R/parse-state.R` | 15 | 11 | 0 | 26 |
| `R/parse-web.R` | 1 | 2 | 0 | 3 |
| `R/parse.R` | 1 | 1 | 1 | 3 |
| `R/path-query.R` | 2 | 1 | 1 | 4 |
| `R/resolve.R` | 0 | 1 | 0 | 1 |
| `R/serialize.R` | 2 | 1 | 0 | 3 |
| `R/url-key.R` | 3 | 0 | 3 | 6 |
| `R/utils.R` | 1 | 0 | 1 | 2 |
| `R/verdicts.R` | 1 | 0 | 0 | 1 |
| `R/zzz.R` | 0 | 0 | 4 | 4 |

The U+FEFF column below is the first character of a string that starts with
the mark, measured on stringi 1.8.9 / ICU 71.1 with a U+200B twin as the
control. "Skips" means the call reads the string as if the mark were not
there. Base R counts the mark in every mode.

| `stringi` function | Refs | Family | Leading U+FEFF | Slice |
|---|---|---|---|---|
| `stri_detect_regex` | 57 | detect | counts | oahhlnvf (56), yyeohqah (1) |
| `stri_replace_first_regex` | 22 | replace | counts | oahhlnvf |
| `stri_match_first_regex` | 21 | capture | skips | yqmoztoj |
| `stri_sub` | 17 | cut | skips | yqmoztoj |
| `stri_detect_fixed` | 15 | detect | skips | oahhlnvf |
| `stri_locate_first_regex` | 10 | locate | counts | yqmoztoj |
| `stri_replace_all_fixed` | 6 | replace | skips | oahhlnvf |
| `stri_length` | 5 | length (offset) | counts | yqmoztoj |
| `stri_replace_all_regex` | 5 | replace | counts | oahhlnvf (4), yyeohqah (1) |
| `stri_split_fixed` | 5 | split | skips | yqmoztoj |
| `stri_startswith_fixed` | 5 | detect (prefix) | skips | oahhlnvf |
| `stri_opts_regex` | 4 | options | n/a | yqmoztoj |
| `stri_enc_toutf8` | 3 | encoding | skips | yyeohqah |
| `stri_escape_unicode` | 3 | escape | skips | yyeohqah |
| `stri_count_fixed` | 2 | count | skips | oahhlnvf |
| `stri_replace_last_regex` | 2 | replace | counts | oahhlnvf |
| `stri_trans_nfc` | 2 | normalization | counts | yyeohqah (stays) |
| `stri_enc_isutf8` | 1 | encoding | n/a (validity only) | yyeohqah |
| `stri_locate_first_fixed` | 1 | locate | skips | yqmoztoj |
| `stri_locate_last_regex` | 1 | locate | counts | yqmoztoj |
| `stri_numbytes` | 1 | length (bytes) | counts | yyeohqah |
| `stri_trans_tolower` | 1 | case map | skips | yyeohqah (stays) |
| `stri_trans_toupper` | 1 | case map | skips | yyeohqah (stays) |

## Where the semantics differ

Each count is a number of rows in the table at the end of this file. Every
line was measured on R 4.6.0 (macOS) with PCRE2 10.47, and stringi 1.8.9
with ICU 71.1, under `en_US.UTF-8` and under `LC_ALL=C`.

- **ICU `$`, 37 rows.** ICU's `$` also matches before a final `\r`, U+0085,
  U+2028 or `\r\n`. PCRE's `$` matches only before a final `\n`. `\z` means
  the same thing in both. A migrated anchor is written `\z`, unless the slice
  shows the terminator case cannot occur.
- **ICU `.`, 17 rows.** ICU's `.` does not match `\n`, `\r`, U+0085, U+2028
  or U+2029. In byte mode `.` matches any byte except `\n`, so it matches
  `\r`, and it matches a multibyte character one byte at a time. Greedy runs
  such as `.*` that end on an ASCII delimiter come out the same. A `.` that
  counts characters does not.
- **`(?i)` on an ASCII pattern, 12 rows, and the ICU `case_insensitive` flag
  on an ASCII pattern, 4 rows (plus the 2 `stri_opts_regex()` rows that build
  it).** In byte mode, under C and UTF-8 locales, an
  ASCII letter folds only to its ASCII partner. A high-byte range does not:
  `(?i)[\xc0-\xc5]` also matches `\xe0`-`\xe5` under `en_US.UTF-8` and does
  not under C, and no single-byte locale was measured. Since ADR 0019's
  acceptance (decision 2), `R/strings-base.R` refuses case-insensitive
  matching in byte mode in every spelling, ASCII patterns included, and a
  slice spells each letter as a class (`[Hh][Tt][Tt][Pp]`). That rewrite is
  not meaning-preserving: ICU's caseless match also folds U+017F to `s` and
  U+212A to `k` (`stri_detect_regex("httpſ", "(?i)^https$")` is `TRUE`).
  Each such row is a behavior change, with its own NEWS entry and test.
- **Locale-table classes.** No current row uses one, apart from the `[\s\S]`
  idiom (3 rows). In byte mode R builds PCRE's character tables from the
  locale. `\s` matches the byte `0xA0`, `\w` matches 65 high bytes, and
  `[[:alpha:]]` matches `0xE9` under `en_US.UTF-8`, while none of them do
  under C. Byte mode is locale-invariant only without these classes, so
  `R/strings-base.R` refuses them: `\w \W \s \S \d \D \b \B`, the
  byte-level `\h \H \v \V \R` (which match `0xA0` or `0x85`, both possible
  continuation bytes), `\p`/`\P`, `\X`, `\C`, and POSIX `[:name:]` classes.
  `[\s\S]` becomes `(?s:.)`.
- **Non-ASCII code points, 3 rows** (`[。．｡]`, the WHATWG
  domain separators). Byte mode spells them as UTF-8 byte sequences, so
  `(?:\xe3\x80\x82|\xef\xbc\x8e|\xef\xbd\xa1)`. UTF-8 PCRE mode keeps the
  code points. Either works on valid UTF-8; the slice decides.
- **`\P{ASCII}` (1 row) and non-ASCII detectors `[^\u0000-\u007F]` /
  `[^\u0001-\u007f]` (3 rows).** In byte mode these are `[^\x00-\x7f]` /
  `[^\x01-\x7f]`, which give the same answer on valid UTF-8 and also flag an
  invalid row. Today that row's result depends on how `stringi` treats it.
- **A `\x{0080}-\x{10FFFF}` range (1 row, `.rfc3986_class_ok()` since MR !235; `.rfc3986_match()` in the snapshot).** Byte mode
  spells it as `\x80-\xff`, which matches each byte of a multibyte character
  and also the bytes of an invalid sequence. RURL-vewhvxnb kept it in stringi
  (MR !235); the oahhlnvf slice decides, relying on the gate's undecodable
  preflight.
- **Code-point offsets, 22 rows** (`stri_sub`, `stri_length`), together with
  the 12 locate rows and 21 capture rows. `stri_*` indexes code points and
  base R with `useBytes = TRUE` indexes bytes. A chain moves as a whole
  (ARCHITECTURE.md, "Cut by byte position at the host and authority seams").
- **Split.** `stri_split_fixed()` keeps a trailing empty field and maps `""`
  to `""`. `strsplit()` drops the field and maps `""` to `character(0)`.
  ADR 0005 keeps `strsplit()` on purpose at its sites. `.bytes_split_fixed()`
  makes the caller choose.
- **Acceptance at host seams.** ARCHITECTURE.md warns that `useBytes = TRUE`
  on a host-charset grep widens acceptance, because an ASCII pattern then
  matches inside an undecodable host. Byte mode gives an invalid row its own
  answer. That answer is a new input to every acceptance mask downstream, and
  the slice that moves a host-seam call has to show it rejects the same rows.

## Frozen areas these rows touch

- Rows 12 and 13 (`stri_trans_nfc` in `.normalize_and_punycode()` and
  `.normalize_and_punycode_vec()`) are in the Punycode helpers that ADR 0002
  freezes. They stay in `stringi` in any case.
- Rows 14 and 15 (`.split_host_labels()`, `R/domain.R`) are outside the PSL
  seam that ADR 0001 names (`.psl_*`), but they are in the same file. Read
  ADR 0001 before moving them.
- No row is in the base-R string calls that ADR 0005 retains, or in the
  `safe_parse_url()` columns that ADR 0006 fixes.

## Every reference

Rows are in file order, then source order.

| # | File | Enclosing function | `stringi` function | Family | Pattern | Unicode semantics | Target | Slice |
|---|---|---|---|---|---|---|---|---|
| 1 | `accessors.R` | `.subdomain_labels_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 2 | `accessors.R` | `.subdomain_labels_vec` | `stri_length` | length (offset) |  | indexes code points | bytes | yqmoztoj |
| 3 | `accessors.R` | `.subdomain_labels_vec` | `stri_length` | length (offset) |  | indexes code points | bytes | yqmoztoj |
| 4 | `diagnostics.R` | `.derive_url_metadata_vec` | `stri_detect_fixed` | detect | `:` | none (ASCII literal) | bytes | oahhlnvf |
| 5 | `diagnostics.R` | `.derive_url_metadata_vec` | `stri_detect_regex` | detect | `(?i)%(2f\|3f\|23)` | (?i) on an ASCII pattern | bytes | oahhlnvf |
| 6 | `diagnostics.R` | `.derive_url_metadata_vec` | `stri_detect_regex` | detect | `(?i)(^\|/)(%2e\|\.%2e\|%2e\.\|%2e%2e)(/\|$)` | (?i) on an ASCII pattern; ICU `$` | bytes | oahhlnvf |
| 7 | `diagnostics.R` | `.derive_url_metadata_vec` | `stri_detect_regex` | detect | `%(?![0-9A-Fa-f]{2})` | none (ASCII structural) | bytes | oahhlnvf |
| 8 | `diagnostics.R` | `.derive_url_metadata_vec` | `stri_detect_regex` | detect | `[ "<>\u0060{}\|^]` | none (ASCII structural) | bytes | oahhlnvf |
| 9 | `diagnostics.R` | `.derive_url_metadata_vec` | `stri_detect_fixed` | detect | `,` | none (ASCII literal) | bytes | oahhlnvf |
| 10 | `diagnostics.R` | `.derive_url_metadata_vec` | `stri_detect_regex` | detect | `(?i);phone-context=` | (?i) on an ASCII pattern | bytes | oahhlnvf |
| 11 | `diagnostics.R` | `.ipv6_host_diagnostics` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 12 | `domain.R` | `.normalize_and_punycode` | `stri_trans_nfc` | normalization |  | Unicode NFC (UCD tables): stays stringi | stringi (stays) | yyeohqah |
| 13 | `domain.R` | `.normalize_and_punycode_vec` | `stri_trans_nfc` | normalization |  | Unicode NFC (UCD tables): stays stringi | stringi (stays) | yyeohqah |
| 14 | `domain.R` | `.split_host_labels` | `stri_split_fixed` | split | `.` | keeps trailing empty field | bytes | yqmoztoj |
| 15 | `domain.R` | `.split_host_labels` | `stri_split_fixed` | split | `.` | keeps trailing empty field | bytes | yqmoztoj |
| 16 | `email-diagnostics.R` | `.email_has_non_ascii` | `stri_detect_regex` | detect | `[^\u0000-\u007F]` | non-ASCII detector | bytes (`[^\x01-\x7f]`) | oahhlnvf |
| 17 | `parse-phases.R` | `.classify_input_host_vec` | `stri_replace_first_regex` | replace | `^[a-zA-Z][a-zA-Z0-9+.-]*://` | none (ASCII structural) | bytes | oahhlnvf |
| 18 | `parse-phases.R` | `.classify_input_host_vec` | `stri_replace_first_regex` | replace | `^//` | none (ASCII structural) | bytes | oahhlnvf |
| 19 | `parse-phases.R` | `.classify_input_host_vec` | `stri_replace_first_regex` | replace | `[/?#].*$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | oahhlnvf |
| 20 | `parse-phases.R` | `.classify_input_host_vec` | `stri_detect_fixed` | detect | `@` | none (ASCII literal) | bytes | oahhlnvf |
| 21 | `parse-phases.R` | `.classify_input_host_vec` | `stri_replace_first_regex` | replace | `^.*@` | ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | oahhlnvf |
| 22 | `parse-phases.R` | `.classify_input_host_vec` | `stri_replace_first_regex` | replace | `^(\[[^\]]*\]).*$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | oahhlnvf |
| 23 | `parse-phases.R` | `.classify_input_host_vec` | `stri_replace_first_regex` | replace | `:[^:]*$` | ICU `$` | bytes | oahhlnvf |
| 24 | `parse-phases.R` | `.classify_input_host_vec` | `stri_detect_regex` | detect | `^[^.]+(\.[^.]+)+$` | ICU `$` | bytes | oahhlnvf |
| 25 | `parse-phases.R` | `.classify_input_host_vec` | `stri_detect_regex` | detect | `^(0[xX][0-9a-fA-F]*\|[0-9]+)(\.(0[xX][0-9a-fA-F]*\|[0-9]+))*$` | ICU `$` | bytes | oahhlnvf |
| 26 | `parse-phases.R` | `.classify_input_host_vec` | `stri_detect_fixed` | detect | `:` | none (ASCII literal) | bytes | oahhlnvf |
| 27 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_match_first_regex` | capture | `^([a-zA-Z][a-zA-Z0-9+.-]*):` | none (ASCII structural) | bytes | yqmoztoj |
| 28 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_length` | length (offset) |  | indexes code points | bytes | yqmoztoj |
| 29 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_locate_first_regex` | locate | `[?#]` | none (ASCII structural) | bytes | yqmoztoj |
| 30 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 31 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 32 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_match_first_regex` | capture | `^[^:]*:([/\\]+)` | none (ASCII structural) | bytes | yqmoztoj |
| 33 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_length` | length (offset) |  | indexes code points | bytes | yqmoztoj |
| 34 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 35 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_replace_all_fixed` | replace | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 36 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_detect_fixed` | detect | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 37 | `parse-phases.R` | `.rewrite_whatwg_backslashes_vec` | `stri_detect_fixed` | detect | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 38 | `parse-phases.R` | `.strip_whatwg_control_chars_vec` | `stri_replace_first_regex` | replace | `^[\u0000-\u0020]+` | none (ASCII structural) | bytes | oahhlnvf |
| 39 | `parse-phases.R` | `.strip_whatwg_control_chars_vec` | `stri_replace_first_regex` | replace | `[\u0000-\u0020]+\z` | none (ASCII structural) | bytes | oahhlnvf |
| 40 | `parse-phases.R` | `.strip_whatwg_control_chars_vec` | `stri_detect_regex` | detect | `^[\u0000-\u0020]\|[\u0000-\u0020]\z` | none (ASCII structural) | bytes | oahhlnvf |
| 41 | `parse-phases.R` | `.strip_whatwg_control_chars_vec` | `stri_detect_regex` | detect | `[\t\n\r]` | none (ASCII structural) | bytes | oahhlnvf |
| 42 | `parse-phases.R` | `.strip_whatwg_control_chars_vec` | `stri_replace_all_regex` | replace | `[\t\n\r]` | none (ASCII structural) | bytes | oahhlnvf |
| 43 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_match_first_regex` | capture | `^([a-zA-Z][a-zA-Z0-9+.-]*:)?(//)([^/?#]*)(.*)$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 44 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_detect_regex` | detect | `[\u3002\uFF0E\uFF61]` | non-ASCII code points (\u3002 \uFF0E \uFF61) | bytes (UTF-8 byte literals) or UTF-8 PCRE | oahhlnvf |
| 45 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_locate_first_regex` | locate | `<paste0(prefix, "[^/?#]*")>` | none (ASCII structural) | bytes | yqmoztoj |
| 46 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_locate_first_regex` | locate | `<paste0(prefix, "[^/?#]*@")>` | none (ASCII structural) | bytes | yqmoztoj |
| 47 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_locate_first_regex` | locate | `<prefix>` | none (ASCII structural) | bytes | yqmoztoj |
| 48 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 49 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 50 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 51 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_detect_regex` | detect | `[\u3002\uFF0E\uFF61]` | non-ASCII code points (\u3002 \uFF0E \uFF61) | bytes (UTF-8 byte literals) or UTF-8 PCRE | oahhlnvf |
| 52 | `parse-phases.R` | `.map_whatwg_domain_separators_vec` | `stri_replace_all_regex` | replace | `[\u3002\uFF0E\uFF61]` | non-ASCII code points (\u3002 \uFF0E \uFF61) | bytes (UTF-8 byte literals) or UTF-8 PCRE | oahhlnvf |
| 53 | `parse-phases.R` | `.encode_userinfo_charset_vec` | `stri_match_first_regex` | capture | `^([a-zA-Z][a-zA-Z0-9+.-]*:)(//)([^/?#]*)(.*)$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 54 | `parse-phases.R` | `.encode_userinfo_charset_vec` | `stri_count_fixed` | count | `@` | none (ASCII literal) | bytes | oahhlnvf |
| 55 | `parse-phases.R` | `.encode_userinfo_charset_vec` | `stri_replace_last_regex` | replace | `@[^@]*\z` | none (ASCII structural) | bytes | oahhlnvf |
| 56 | `parse-phases.R` | `.encode_userinfo_charset_vec` | `stri_detect_regex` | detect | `[\u0000-\u0020\u007F]` | none (ASCII structural) | bytes | oahhlnvf |
| 57 | `parse-phases.R` | `.percent_encode_userinfo_charset` | `stri_replace_all_fixed` | replace | `<chars>` | none (33 ASCII control bytes, `vectorize_all = FALSE`) | bytes | oahhlnvf |
| 58 | `parse-phases.R` | `.rfc3986_path_rootless_vec` | `stri_match_first_regex` | capture | `^([a-zA-Z][a-zA-Z0-9+.-]*):(.*)$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 59 | `parse-phases.R` | `.rfc3986_path_rootless_vec` | `stri_replace_first_regex` | replace | `[/?#].*$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | oahhlnvf |
| 60 | `parse-phases.R` | `.rfc3986_path_rootless_vec` | `stri_detect_regex` | detect | `^[A-Za-z0-9._~-]+(\.[A-Za-z0-9._~-]+)+$` | ICU `$` | bytes | oahhlnvf |
| 61 | `parse-phases.R` | `.rfc3986_path_rootless_vec` | `stri_startswith_fixed` | detect (prefix) | `/` | none (ASCII literal) | bytes | oahhlnvf |
| 62 | `parse-phases.R` | `.rfc3986_path_rootless_vec` | `stri_startswith_fixed` | detect (prefix) | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 63 | `parse-phases.R` | `.rfc3986_path_rootless_vec` | `stri_detect_fixed` | detect | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 64 | `parse-phases.R` | `.whatwg_file_normalize_host` | `stri_detect_regex` | detect | `<.WHATWG_FORBIDDEN_HOST_CP>` | none (ASCII class, `R/utils.R`) | bytes | oahhlnvf |
| 65 | `parse-phases.R` | `.whatwg_file_drive_path` | `stri_replace_first_regex` | replace | `^/([A-Za-z])\\|(?=/\|$)` | ICU `$` | bytes | oahhlnvf |
| 66 | `parse-phases.R` | `.whatwg_file_shorten_drive_path` | `stri_detect_regex` | detect | `^/[A-Za-z]:(?=/\|$)` | ICU `$` | bytes | oahhlnvf |
| 67 | `parse-phases.R` | `.whatwg_file_shorten_drive_path` | `stri_detect_regex` | detect | `(?i)/(\.\|%2e){1,2}(/\|$)` | (?i) on an ASCII pattern; ICU `$` | bytes | oahhlnvf |
| 68 | `parse-phases.R` | `.whatwg_file_shorten_drive_path` | `stri_replace_all_regex` | replace | `(?i)(?<=/)(\.\|%2e){2}(?=/\|$)` | (?i) on an ASCII pattern; ICU `$` | bytes | oahhlnvf |
| 69 | `parse-phases.R` | `.whatwg_file_shorten_drive_path` | `stri_replace_all_regex` | replace | `(?i)(?<=/)%2e(?=/\|$)` | (?i) on an ASCII pattern; ICU `$` | bytes | oahhlnvf |
| 70 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_match_first_regex` | capture | `^[Ff][Ii][Ll][Ee]:(.*)$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 71 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_replace_all_fixed` | replace | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 72 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_replace_all_fixed` | replace | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 73 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_detect_regex` | detect | `^[/\\]{2}` | none (ASCII structural) | bytes | oahhlnvf |
| 74 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_locate_first_regex` | locate | `^[/\\]{2}[^/\\]*` | none (ASCII structural) | bytes | yqmoztoj |
| 75 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_locate_first_regex` | locate | `^[/\\]{2}[^/\\]*[/\\]` | none (ASCII structural) | bytes | yqmoztoj |
| 76 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 77 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 78 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_replace_all_fixed` | replace | `\` | none (ASCII literal) | bytes | oahhlnvf |
| 79 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_detect_regex` | detect | `^[A-Za-z][:\|]$` | ICU `$` | bytes | oahhlnvf |
| 80 | `parse-phases.R` | `.parse_whatwg_file_urls_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 81 | `parse-phases.R` | `.apply_browser_fixup_vec` | `stri_replace_first_regex` | replace | `^[\x00-\x20]+` | none (ASCII structural) | bytes | oahhlnvf |
| 82 | `parse-phases.R` | `.apply_browser_fixup_vec` | `stri_replace_first_regex` | replace | `[\x00-\x20]+$` | ICU `$` | bytes | oahhlnvf |
| 83 | `parse-phases.R` | `.apply_browser_fixup_vec` | `stri_replace_first_regex` | replace | `<paste0("(?i)^(", scheme_alt, ");")>` | (?i) on an ASCII pattern (special-scheme names) | bytes | oahhlnvf |
| 84 | `parse-phases.R` | `.apply_browser_fixup_vec` | `stri_replace_first_regex` | replace | `<paste0("(?i)^(", authority_alt, "):/?(?!/)")>` | (?i) on an ASCII pattern (special-scheme names) | bytes | oahhlnvf |
| 85 | `parse-phases.R` | `.apply_browser_fixup_vec` | `stri_replace_first_regex` | replace | `^(?=/(?![\t\n\r]*[/\\]))` | none (ASCII structural) | bytes | oahhlnvf |
| 86 | `parse-phases.R` | `.prepare_urls_for_parse_vec` | `stri_detect_regex` | detect | `^[Ff][Ii][Ll][Ee]:` | none (ASCII structural) | bytes | oahhlnvf |
| 87 | `parse-phases.R` | `.prepare_urls_for_parse_vec` | `stri_match_first_regex` | capture | `^([a-zA-Z][a-zA-Z0-9+.-]*):` | none (ASCII structural) | bytes | yqmoztoj |
| 88 | `parse-phases.R` | `.prepare_urls_for_parse_vec` | `stri_detect_regex` | detect | `^([a-zA-Z][a-zA-Z0-9+.-]*):\/\/` | none (ASCII structural) | bytes | oahhlnvf |
| 89 | `parse-phases.R` | `.prepare_urls_for_parse_vec` | `stri_startswith_fixed` | detect (prefix) | `//` | none (ASCII literal) | bytes | oahhlnvf |
| 90 | `parse-phases.R` | `.prepare_urls_for_parse_vec` | `stri_detect_regex` | detect | `^[^/:]+:[0-9]+($\|/)` | ICU `$` | bytes | oahhlnvf |
| 91 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_locate_first_regex` | locate | `^[a-zA-Z][a-zA-Z0-9+.-]*://` | none (ASCII structural) | bytes | yqmoztoj |
| 92 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_replace_first_regex` | replace | `^[a-zA-Z][a-zA-Z0-9+.-]*://` | none (ASCII structural) | bytes | oahhlnvf |
| 93 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_locate_first_regex` | locate | `[/?#]` | none (ASCII structural) | bytes | yqmoztoj |
| 94 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 95 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 96 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 97 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_locate_first_regex` | locate | `[?#]` | none (ASCII structural) | bytes | yqmoztoj |
| 98 | `parse-phases.R` | `.extract_raw_path_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 99 | `parse-phases.R` | `.normalize_path_vec` | `stri_detect_fixed` | detect | `%` | none (ASCII literal) | bytes | oahhlnvf |
| 100 | `parse-phases.R` | `.normalize_path_vec` | `stri_detect_fixed` | detect | `%` | none (ASCII literal) | bytes | oahhlnvf |
| 101 | `parse-phases.R` | `.normalize_path_vec` | `stri_detect_fixed` | detect | `%` | none (ASCII literal) | bytes | oahhlnvf |
| 102 | `parse-phases.R` | `.normalize_path_vec` | `stri_detect_regex` | detect | `<detect_pattern>` | (?i) on an ASCII pattern; ICU `$` | bytes | oahhlnvf |
| 103 | `parse-phases.R` | `.normalize_path_vec` | `stri_detect_regex` | detect | `(?i)/(index\|default)\.[^/]+/?$` | (?i) on an ASCII pattern; ICU `$` | bytes | oahhlnvf |
| 104 | `parse-phases.R` | `.detect_ip_host_vec` | `stri_detect_regex` | detect | `<ipv4_re>` | ASCII dotted quad; ICU `$` | bytes | oahhlnvf |
| 105 | `parse-phases.R` | `.detect_ip_host_vec` | `stri_detect_fixed` | detect | `[` | none (ASCII literal) | bytes | oahhlnvf |
| 106 | `parse-phases.R` | `.detect_ip_host_vec` | `stri_detect_fixed` | detect | `]` | none (ASCII literal) | bytes | oahhlnvf |
| 107 | `parse-phases.R` | `.detect_ip_host_vec` | `stri_detect_regex` | detect | `<paste0("^\\[?[0-9a-fA-F:]+:(", oct, "(?:\\.", oct, "){3})\\]?$")>` | ASCII grammar; ICU `$` | bytes | oahhlnvf |
| 108 | `parse-phases.R` | `.detect_ip_host_vec` | `stri_detect_regex` | detect | `^\[?[0-9a-fA-F:]+\]?$` | ICU `$` | bytes | oahhlnvf |
| 109 | `parse-phases.R` | `.detect_ip_host_vec` | `stri_detect_fixed` | detect | `:` | none (ASCII literal) | bytes | oahhlnvf |
| 110 | `parse-phases.R` | `.host_ends_in_number_vec` | `stri_replace_first_regex` | replace | `\.$` | ICU `$` | bytes | oahhlnvf |
| 111 | `parse-phases.R` | `.host_ends_in_number_vec` | `stri_replace_first_regex` | replace | `^.*\.` | ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | oahhlnvf |
| 112 | `parse-phases.R` | `.host_ends_in_number_vec` | `stri_detect_regex` | detect | `^[0-9]+$` | ICU `$` | bytes | oahhlnvf |
| 113 | `parse-phases.R` | `.host_ends_in_number_vec` | `stri_detect_regex` | detect | `^0[xX][0-9a-fA-F]*$` | ICU `$` | bytes | oahhlnvf |
| 114 | `parse-phases.R` | `.serialize_whatwg_ipv6_host` | `stri_detect_regex` | detect | `^\[.*\]$` | ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | oahhlnvf |
| 115 | `parse-phases.R` | `.serialize_whatwg_ipv6_host` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 116 | `parse-phases.R` | `.serialize_whatwg_ipv6_host` | `stri_match_first_regex` | capture | `<paste0("^(.*:)(", oct, "\\.", oct, "\\.", oct, "\\.", oct, ")$")>` | ASCII grammar; ICU `$`; ICU `.` | bytes | yqmoztoj |
| 117 | `parse-phases.R` | `.serialize_whatwg_ipv6_host` | `stri_split_fixed` | split | `::` | keeps trailing empty field | bytes | yqmoztoj |
| 118 | `parse-phases.R` | `.serialize_whatwg_ipv6_host` | `stri_split_fixed` | split | `:` | keeps trailing empty field | bytes | yqmoztoj |
| 119 | `parse-phases.R` | `.serialize_whatwg_ipv6_hosts_vec` | `stri_detect_regex` | detect | `^\[[0-9A-Fa-f:.]+\]$` | ICU `$` | bytes | oahhlnvf |
| 120 | `parse-phases.R` | `.apply_host_standard_model_vec` | `stri_detect_regex` | detect | `[^\u0001-\u007f]` | non-ASCII detector | bytes (`[^\x01-\x7f]`) | oahhlnvf |
| 121 | `parse-phases.R` | `.apply_host_standard_model_vec` | `stri_detect_regex` | detect | `<.WHATWG_FORBIDDEN_HOST_CP>` | none (ASCII class, `R/utils.R`) | bytes | oahhlnvf |
| 122 | `parse-phases.R` | `.apply_host_standard_model_vec` | `stri_detect_regex` | detect | `[^\u0001-\u007f]` | non-ASCII detector | bytes (`[^\x01-\x7f]`) | oahhlnvf |
| 123 | `parse-phases.R` | `.apply_host_standard_model_vec` | `stri_detect_regex` | detect | `<.WHATWG_FORBIDDEN_HOST_CP>` | none (ASCII class, `R/utils.R`) | bytes | oahhlnvf |
| 124 | `parse-phases.R` | `.apply_www_policy_vec` | `stri_opts_regex` | options |  | ICU case-insensitive flag | dropped with its consumers | yqmoztoj |
| 125 | `parse-phases.R` | `.apply_www_policy_vec` | `stri_replace_first_regex` | replace | `^(www[0-9]*\.)(.*)` | ICU case-insensitive on an ASCII pattern; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | oahhlnvf |
| 126 | `parse-phases.R` | `.apply_www_policy_vec` | `stri_detect_regex` | detect | `^www[0-9]*\.` | none (ASCII structural) | bytes | oahhlnvf |
| 127 | `parse-phases.R` | `.apply_www_policy_vec` | `stri_match_first_regex` | capture | `^(www[0-9]*\.)(.*)` | ICU case-insensitive on an ASCII pattern; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 128 | `parse-phases.R` | `.apply_www_if_no_subdomain_vec` | `stri_opts_regex` | options |  | ICU case-insensitive flag | dropped with its consumers | yqmoztoj |
| 129 | `parse-phases.R` | `.apply_www_if_no_subdomain_vec` | `stri_detect_regex` | detect | `^www[0-9]*\.` | none (ASCII structural) | bytes | oahhlnvf |
| 130 | `parse-phases.R` | `.apply_www_if_no_subdomain_vec` | `stri_match_first_regex` | capture | `^(www[0-9]*\.)(.*)` | ICU case-insensitive on an ASCII pattern; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 131 | `parse-phases.R` | `.apply_www_if_no_subdomain_vec` | `stri_startswith_fixed` | detect (prefix) | `www.` | none (ASCII literal) | bytes | oahhlnvf |
| 132 | `parse-phases.R` | `.apply_www_if_no_subdomain_vec` | `stri_match_first_regex` | capture | `^www\.(.*)` | ICU case-insensitive on an ASCII pattern; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 133 | `parse-phases.R` | `.psl_annotation_host_vec` | `stri_detect_fixed` | detect | `%` | none (ASCII literal) | bytes | oahhlnvf |
| 134 | `parse-phases.R` | `.apply_subdomain_policy_vec` | `stri_startswith_fixed` | detect (prefix) | `www.` | none (ASCII literal) | bytes | oahhlnvf |
| 135 | `parse-phases.R` | `.apply_subdomain_policy_vec` | `stri_sub` | cut |  | indexes code points | bytes | yqmoztoj |
| 136 | `parse-phases.R` | `.unicode_case_map_vec` | `stri_trans_toupper` | case map |  | Unicode full case map: stays stringi | stringi (stays) | yyeohqah |
| 137 | `parse-phases.R` | `.unicode_case_map_vec` | `stri_trans_tolower` | case map |  | Unicode full case map: stays stringi | stringi (stays) | yyeohqah |
| 138 | `parse-state.R` | `.rfc_host_form` | `stri_detect_regex` | detect | `<.RFC3986_BRACKET_IPVFUTURE_RE>` | none (ASCII grammar, `\A`...`\z`) | bytes | oahhlnvf |
| 139 | `parse-state.R` | `.rfc3986_first_index` | `stri_locate_first_regex` | locate | `<ch>` | none (one ASCII delimiter, literal flag) | bytes | yqmoztoj |
| 140 | `parse-state.R` | `.rfc3986_first_index` | `stri_opts_regex` | options |  | literal flag | dropped with its consumers | yqmoztoj |
| 141 | `parse-state.R` | `.rfc3986_match` | `stri_detect_regex` | detect | `<re>` | port regex `\A[0-9]*\z` only since MR !235 (the class regexes moved to `.rfc3986_class_ok()`, see "Since the snapshot") | bytes | oahhlnvf |
| 142 | `parse-state.R` | `.rfc3986_valid_ip_literal` | `stri_detect_regex` | detect | `<.RFC3986_IPV6_RE>` | none (ASCII grammar, `\A`...`\z`) | bytes | oahhlnvf |
| 143 | `parse-state.R` | `.rfc3986_valid_ip_literal` | `stri_detect_regex` | detect | `<.RFC3986_IPVFUTURE_RE>` | none (ASCII grammar, `\A`...`\z`) | bytes | oahhlnvf |
| 144 | `parse-state.R` | `.rfc3986_valid_authority` | `stri_count_fixed` | count | `@` | none (ASCII literal) | bytes | oahhlnvf |
| 145 | `parse-state.R` | `.rfc3986_generic_uri_ok_one` | `stri_match_first_regex` | capture | `^([A-Za-z][A-Za-z0-9+.\-]*):` | none (ASCII structural) | bytes | yqmoztoj |
| 146 | `parse-state.R` | `.rfc3986_generic_uri_ok` | `stri_detect_regex` | detect | `\P{ASCII}` | Unicode property \P{ASCII} | bytes (UTF-8 byte literals) or UTF-8 PCRE | oahhlnvf |
| 147 | `parse-state.R` | `.split_authority` | `stri_locate_last_regex` | locate | `@` | none (ASCII structural) | bytes | yqmoztoj |
| 148 | `parse-state.R` | `.split_authority` | `stri_opts_regex` | options |  | literal flag | dropped with its consumers | yqmoztoj |
| 149 | `parse-state.R` | `.whatwg_opaque_host_one` | `stri_detect_regex` | detect | `<.RFC3986_IPV6_RE>` | none (ASCII grammar, `\A`...`\z`) | bytes | oahhlnvf |
| 150 | `parse-state.R` | `.whatwg_opaque_host_one` | `stri_detect_regex` | detect | `<.WHATWG_FORBIDDEN_HOST_ONLY_CP>` | none (ASCII class, `R/utils.R`) | bytes | oahhlnvf |
| 151 | `parse-state.R` | `.rfc_host_one` | `stri_detect_regex` | detect | `<.RFC3986_IPV6_RE>` | none (ASCII grammar, `\A`...`\z`) | bytes | oahhlnvf |
| 152 | `parse-state.R` | `.rfc_host_one` | `stri_detect_regex` | detect | `<.RFC3986_IPVFUTURE_RE>` | none (ASCII grammar, `\A`...`\z`) | bytes | oahhlnvf |
| 153 | `parse-state.R` | `.rfc_host_one` | `stri_detect_regex` | detect | `<paste0("\\A", .RFC3986_IPV4_QUAD, "\\z")>` | none (ASCII grammar, `\A`...`\z`) | bytes | oahhlnvf |
| 154 | `parse-state.R` | `.parse_opaque_url_one` | `stri_match_first_regex` | capture | `^([A-Za-z][A-Za-z0-9+.\-]*):([\s\S]*)$` | `[\s\S]` (byte mode: `(?s:.)`); ICU `$` | bytes | yqmoztoj |
| 155 | `parse-state.R` | `.parse_opaque_url_one` | `stri_detect_regex` | detect | `\A[0-9]+\z` | none (ASCII structural) | bytes | oahhlnvf |
| 156 | `parse-state.R` | `.parse_rfc_file_url_one` | `stri_match_first_regex` | capture | `^([Ff][Ii][Ll][Ee]):([\s\S]*)$` | `[\s\S]` (byte mode: `(?s:.)`); ICU `$` | bytes | yqmoztoj |
| 157 | `parse-state.R` | `.parse_rfc_file_url_one` | `stri_detect_regex` | detect | `\A[0-9]*\z` | none (ASCII structural) | bytes | oahhlnvf |
| 158 | `parse-state.R` | `.general_parsed_mask` | `stri_match_first_regex` | capture | `^([A-Za-z][A-Za-z0-9+.\-]*):` | none (ASCII structural) | bytes | yqmoztoj |
| 159 | `parse-state.R` | `.general_parsed_mask` | `stri_detect_regex` | detect | `^[^/:]+:[0-9]+($\|/)` | ICU `$` | bytes | oahhlnvf |
| 160 | `parse-state.R` | `.rfc_empty_authority_host` | `stri_detect_regex` | detect | `\A[A-Za-z][A-Za-z0-9+.\-]*://([^/?#@]*@)?(:[0-9]*)?([/?#][\s\S]*)?\z` | `[\s\S]` (byte mode: `(?s:.)`) | bytes | oahhlnvf |
| 161 | `parse-state.R` | `.rfc_odd_slash_run` | `stri_match_first_regex` | capture | `^[A-Za-z][A-Za-z0-9+.\-]*:(/*)` | none (ASCII structural) | bytes | yqmoztoj |
| 162 | `parse-state.R` | `.rfc_odd_slash_run` | `stri_length` | length (offset) |  | indexes code points | bytes | yqmoztoj |
| 163 | `parse-state.R` | `.general_parse_vec` | `stri_match_first_regex` | capture | `^([A-Za-z][A-Za-z0-9+.\-]*):` | none (ASCII structural) | bytes | yqmoztoj |
| 164 | `parse-web.R` | `.web_ipv6_serialize` | `stri_match_first_regex` | capture | `<paste0("\\A(.*:)(", oct, "\\.", oct, "\\.", oct, "\\.", oct, ")\\z")>` | ASCII grammar; ICU `.` | bytes | yqmoztoj |
| 165 | `parse-web.R` | `.web_ipv6_serialize` | `stri_locate_first_fixed` | locate | `::` | none (ASCII literal) | bytes | yqmoztoj |
| 166 | `parse-web.R` | `.parse_web_url_one` | `stri_detect_regex` | detect | `(?i)(\A\|/)(\.\|%2e){1,2}(/\|\z)` | (?i) on an ASCII pattern | bytes | oahhlnvf |
| 167 | `parse.R` | `.has_explicit_authority` | `stri_replace_first_regex` | replace | `^[\u0000-\u0020]+` | none (ASCII structural) | bytes | oahhlnvf |
| 168 | `parse.R` | `.latin1_input_to_utf8` | `stri_enc_toutf8` | encoding |  | encoding conversion/validation | semantic exception | yyeohqah |
| 169 | `parse.R` | `.source_scheme_spelling_vec` | `stri_match_first_regex` | capture | `^([A-Za-z][A-Za-z0-9+.\-]*):` | none (ASCII structural) | bytes | yqmoztoj |
| 170 | `path-query.R` | `._strip_index_page` | `stri_match_first_regex` | capture | `(?i)^(.*)/(index\|default)\.[^/]+/?$` | (?i) on an ASCII pattern; ICU `$`; ICU `.` (excludes \r, U+0085, U+2028/9) | bytes | yqmoztoj |
| 171 | `path-query.R` | `._strip_index_page` | `stri_detect_regex` | detect | `(^\|/)\.{1,2}$` | ICU `$` | bytes | oahhlnvf |
| 172 | `path-query.R` | `.token_is_opaque` | `stri_detect_regex` | detect | `%(?![0-9A-Fa-f]{2})` | none (ASCII structural) | bytes | oahhlnvf |
| 173 | `path-query.R` | `.param_matches` | `stri_detect_regex` | detect | `<matcher$regex>` | ICU case-insensitive (Unicode folding); user glob compiled by `.glob_to_regex()` | stringi or UTF-8 PCRE (open) | yyeohqah |
| 174 | `resolve.R` | `.whatwg_file_remove_dot_segments` | `stri_split_fixed` | split | `/` | keeps trailing empty field | bytes | yqmoztoj |
| 175 | `serialize.R` | `.fsss_source_lex` | `stri_replace_first_regex` | replace | `^[\u0000-\u0020]+` | none (ASCII structural) | bytes | oahhlnvf |
| 176 | `serialize.R` | `.fsss_source_lex` | `stri_replace_last_regex` | replace | `[\u0000-\u0020]+$` | ICU `$` | bytes | oahhlnvf |
| 177 | `serialize.R` | `.fsss_source_lex` | `stri_match_first_regex` | capture | `^[A-Za-z][A-Za-z0-9+.\-]*://([^/]*)` | none (ASCII structural) | bytes | yqmoztoj |
| 178 | `url-key.R` | `.url_key_scheme_presence` | `stri_replace_first_regex` | replace | `^[\u0000-\u0020]+` | none (ASCII structural) | bytes | oahhlnvf |
| 179 | `url-key.R` | `.url_key_scheme_presence` | `stri_detect_regex` | detect | `^//` | none (ASCII structural) | bytes | oahhlnvf |
| 180 | `url-key.R` | `.url_key_scheme_presence` | `stri_detect_regex` | detect | `^[A-Za-z][A-Za-z0-9+.\-]*:` | none (ASCII structural) | bytes | oahhlnvf |
| 181 | `url-key.R` | `.url_key_frame` | `stri_enc_toutf8` | encoding |  | encoding conversion/validation | semantic exception | yyeohqah |
| 182 | `url-key.R` | `.url_key_frame` | `stri_enc_toutf8` | encoding |  | encoding conversion/validation | semantic exception | yyeohqah |
| 183 | `url-key.R` | `.url_key_frame` | `stri_numbytes` | length (bytes) |  | none (byte count) | semantic exception | yyeohqah |
| 184 | `utils.R` | `.glob_to_regex` | `stri_replace_all_regex` | replace | `([^A-Za-z0-9_])` | `[^A-Za-z0-9_]` takes one code point; per-byte escaping breaks a multibyte glob | semantic exception | yyeohqah |
| 185 | `utils.R` | `.ascii_case_fallback` | `stri_replace_all_fixed` | replace | `<from>` | none (ASCII letters, `vectorize_all = FALSE`) | bytes | oahhlnvf |
| 186 | `verdicts.R` | `.derive_verdict_layers_vec` | `stri_detect_fixed` | detect | `.` | none (ASCII literal) | bytes | oahhlnvf |
| 187 | `zzz.R` | `.cache_key_ascii` | `stri_enc_isutf8` | encoding |  | UTF-8 validity test | semantic exception | yyeohqah |
| 188 | `zzz.R` | `.cache_key_escape` | `stri_escape_unicode` | escape |  | escapes every non-ASCII code point as \uXXXX/\UXXXXXXXX | semantic exception | yyeohqah |
| 189 | `zzz.R` | `.cache_key_escape` | `stri_escape_unicode` | escape |  | escapes every non-ASCII code point as \uXXXX/\UXXXXXXXX | semantic exception | yyeohqah |
| 190 | `zzz.R` | `.cache_key_escape` | `stri_escape_unicode` | escape |  | escapes every non-ASCII code point as \uXXXX/\UXXXXXXXX | semantic exception | yyeohqah |
