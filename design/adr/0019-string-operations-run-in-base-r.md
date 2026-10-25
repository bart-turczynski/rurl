# ADR 0019: String operations run in base R, each with declared semantics

- **Status:** Accepted (2026-10-24)
- **Date:** 2026-10-12
- **Tracking:** RURL-lbccaixb (this ADR and the migration foundation), in
  epic RURL-bjdcjeel. Slices: RURL-oahhlnvf, RURL-yqmoztoj, RURL-yyeohqah,
  and the close-out RURL-dddxutjf. Input boundary: RURL-cwzickqy.
  **Supersedes in part** [ADR 0005](0005-intentional-base-r-string-exceptions.md):
  its "prefer `stringi`" consequence.

## Context

ADR 0005 recorded a migration from base R to `stringi` for Unicode
correctness. It kept a few base-R calls on purpose and told new string code to
prefer `stringi`. Three findings since then turn that preference around.

**`stringi` drops a leading U+FEFF, and that cannot be turned off.** stringi's
UTF-8 container (`String8::initialize`) skips an EF BB BF that starts its
input, so many `stri_*` functions read a field as if its first code point were
not there. The upstream request to make this configurable was closed
(gagolews/stringi#516). ADR 0005's amendment measured which functions skip the
mark and which count it, and its 31-row table lists the helpers guarded
against the gap: each guard is a mask, a splice-and-cut, or a base-R call.
Each new `stringi` call that can see the start of a field needs another guard
or a stated reason, so the guard table grows with the code. In a URL the mark is data: the WHATWG URL Standard
does not strip it and RFC 3986 has no rule for it.

**`stringi` counts code points, and base R in byte mode counts bytes.** The
parser cuts at the host and authority seams by byte position
(ARCHITECTURE.md, "Cut by byte position at the host and authority seams")
because positions in two units mis-slice multibyte input. Every
`stri_locate_*()` → `stri_sub()` chain is a place where the two units meet.

**Base R gives the behavior the parser wants.** With `perl = TRUE,
useBytes = TRUE` (or `fixed = TRUE, useBytes = TRUE`) on input declared UTF-8,
measured on R 4.6.0 / PCRE2 10.47:

- a leading U+FEFF is matched, cut and split like any other code point;
- an invalid-UTF-8 row gets its own result and the vector does not throw;
- positions and cuts are both in bytes;
- the answer does not depend on the locale, **with one condition**. In byte
  mode R builds PCRE's character tables from `LC_CTYPE`. Under
  `en_US.UTF-8`, `\s` matches the byte 0xA0, `\w` matches 65 high bytes,
  `[[:alpha:]]` matches 0xE9, and `(?i)[\xc0-\xc5]` also matches 0xE0-0xE5.
  Under `LC_ALL=C` none of them do. So byte mode is locale-invariant only for
  patterns that avoid those constructs.

The rulings behind this ADR were made in the maintainer's session before this
ADR was drafted: the epic, the split by kind of operation, the rejected
alternatives, and the guard policy below. This ADR records them. The
inventory, [`design/stringi-inventory.md`](../stringi-inventory.md), lists
every `stringi::` reference in `R/` at `80d4f0e`: 190 in 15 files, each with
its operation family, the points where ICU and byte-mode PCRE differ for it,
and the slice that migrates it.

## Decision

**1. String operations run in base R, and each operation declares its
semantics.** There are three modes, and a call site uses the narrowest one
that is correct for it:

- **Byte mode**, `perl = TRUE, useBytes = TRUE` (or `fixed = TRUE`), is for
  ASCII-structural matching, cutting and splitting: patterns whose delimiters
  and anchors are ASCII. UTF-8 is self-synchronizing, so such a pattern
  matches the same text in bytes as in characters. It goes through
  `R/strings-base.R`: `.bytes_detect()`, `.bytes_replace()`,
  `.bytes_locate()`, `.bytes_capture()`, `.bytes_cut()` and
  `.bytes_split_fixed()`. Each declares its result UTF-8 again and refuses
  the locale-table constructs above (`\w \s \d \b`, their negations, `\h \v
  \R`, `\p \P \X \C`, POSIX classes and the `[[:<:]]` / `[[:>:]]` word
  boundaries, leading `(*...)` verbs, and case-insensitive matching in
  any spelling: `(?i)`, `(?i:...)`, `(?mi)`, `(?^i)`; a letter is spelled
  as a class, `[Hh][Tt][Tt][Pp]`). An end anchor is written `\z`: ICU's `$` also matches
  before a final `\r`, U+0085 or U+2028, and PCRE's matches only before a
  final `\n`. So the pattern check refuses a bare `$`; a dollar sign is
  written `\$` or `[$]`. A class of non-ASCII code points can be written in
  byte mode as its UTF-8 byte sequences (for example, the WHATWG domain
  separators as
  `(?:\xe3\x80\x82|\xef\xbc\x8e|\xef\xbd\xa1)`).
- **UTF-8 PCRE mode**, `perl = TRUE` without `useBytes`, on input declared
  UTF-8, is for a match that has to see code points and that byte literals
  express badly. In this mode `grepl()` warns and returns FALSE for an invalid
  row, and `gsub()` throws for the whole vector, so the caller masks undecodable
  rows first, as `.grepl_decodable()` and `.gsub_decodable()` do in
  `R/utils.R`.
- **`stringi`** stays only where Unicode tables are needed: NFC before
  Punycode (`stri_trans_nfc()` in the Punycode helpers that ADR 0002 freezes)
  and full case mapping (`.unicode_case_map_vec()`). `.param_matches()`, a
  case-insensitive match of user globs, may join them; RURL-yyeohqah decides.
  (It joined them, with the URL key's U+FFFD decode: see "Semantic
  exceptions" below.)

The semantic exceptions get their own decision in RURL-yyeohqah, because
their behavior is the operation: the cache-key escape (`stri_escape_unicode()`
and `stri_enc_isutf8()` in `R/zzz.R`), encoding conversion
(`stri_enc_toutf8()` in `.latin1_input_to_utf8()` and `.url_key_frame()`,
with `stri_numbytes()`), and the Unicode glob (`.glob_to_regex()`). The
decisions are recorded under "Semantic exceptions" below.

**2. Input reaches these operations declared UTF-8.** The input boundary
(RURL-cwzickqy) makes every entry point declare its input UTF-8 with
`Encoding<-`, never by converting from the session locale. Until it lands,
`R/strings-base.R` reads "unknown" input as UTF-8 bytes, takes "UTF-8" input
as declared, and stops on "latin1" or "bytes" input, because re-declaring
those bytes would change the text. The same check covers the pattern, the
replacement and the separator.

**3. The migration is split by kind of operation, not by file.** Offset-bearing
chains move together, so that a position and the cut that uses it always
count the same unit.

| Slice | Moves | References at `80d4f0e` |
|---|---|---|
| RURL-oahhlnvf | boolean and replacement operations: detect, prefix test, count, replace | 112 |
| RURL-yqmoztoj | offset-bearing operations: locate, capture, cut, length, split, and the `stri_opts_regex()` objects their consumers take | 64 |
| RURL-yyeohqah | the semantic exceptions, and confining NFC and case mapping to `stringi` | 14 |
| RURL-dddxutjf | close-out: confirm the allowlist holds only the calls that stay, remove the ADR 0005 guards that the migration made redundant, settle `Imports:` | none |

Rejected: a split by file (it separates a locate from the cut in another
helper that consumes its position, and it mixes kinds of risk in one review),
and a report-only gate (it lets the call count grow while the slices run).

**4. A gate blocks new `stringi` references.** `tools/stringi-call-gate.R`
parses `R/` and `NAMESPACE` and fails on any `stringi` reference beyond
`tools/stringi-call-allowlist.csv`. That covers `::` and `:::`, aliases, bare
`stri_*`, `library()`/`requireNamespace()`, and roxygen and NAMESPACE imports.
The allowlist is keyed on file, enclosing top-level function and reference,
with a count, so a moved or removed call never fails. A removal prints
"allowlist can shrink". Each migration slice runs
`Rscript tools/stringi-call-gate.R --write-allowlist` and commits the shrink
in the same MR, so the list shrinks at every merge; the close-out only
confirms what is left.

**5. Guard policy until the epic closes.** A newly found U+FEFF defect is
fixed through a migration slice, not with a new per-call guard. One exception:
a shipped conformance defect that the migration cannot fix promptly may get a
targeted fix, written so the migration can delete it. Such a fix uses base R
at the site rather than a mask and splice that a later slice would have to
unwind, and it cites this ADR.

## Relation to ADR 0005

This ADR **supersedes** ADR 0005's consequence "New string code should still
prefer `stringi` unless it hits one of these documented equivalence gaps".
Decision 1 replaces it. It **keeps** three of ADR 0005's contracts:

- **The split contract.** `strsplit(..., fixed = TRUE)` drops a trailing
  empty field and maps `""` to `character(0)`. `stri_split_fixed()` keeps the
  field and maps `""` to `""`. ADR 0005's retained `strsplit()` sites depend
  on the first reading and stay as they are. `.bytes_split_fixed()` has no
  default for `keep_trailing_empty`, so every migrated split states which
  reading it relies on.
- **The rejection.** ADR 0005 rejected finishing a migration for consistency
  alone. That holds in the other direction too: no call moves for uniformity.
  Each moved call either keeps its behavior or names the defect it fixes,
  with a standards citation in `NEWS.md` when the defect was shipped.
- **U+FEFF as data.** A leading U+FEFF is ordinary data. A fix's test pairs
  each U+FEFF row with a U+200B twin. The amendment's guard table stays in
  force for each helper until a slice moves that helper, and no guarded
  helper goes back to a skipping `stringi` call in the meantime.

## Consequences

- **Standing rule:** add no `stringi` reference outside the allowlist. New
  string code uses `R/strings-base.R`, or base R with its mode stated: byte
  mode for ASCII structure, UTF-8 PCRE mode with an undecodable-row mask for
  code points. `stringi` is for Unicode tables only.
- **Acceptance at host seams.** Byte mode answers for an invalid row, where
  `.grepl_decodable()` folds such a row to FALSE. ARCHITECTURE.md warns that
  `useBytes = TRUE` on a host-charset grep widens acceptance. A slice that
  moves a call feeding an acceptance mask keeps the decodable mask at that
  seam (`.grepl_decodable()`, or the input boundary's per-row verdict where
  the seam has it), and plain byte mode is used elsewhere. Combining masks
  never turns an undecodable row into an accepted one, and the row lands on
  the reject or `NA` side its component's status contract names.
- **Semantics a slice must check, per call** (the inventory lists them):
  ICU `$` (37 references), ICU `.` (17), `(?i)` and the ICU
  case-insensitive flag on ASCII patterns (rewritten as ASCII letter
  classes), `[\s\S]` (written `(?s:.)`),
  non-ASCII classes, and code-point offsets.
- **Easier:** a field that starts with U+FEFF needs no guard; offsets have
  one unit; invalid rows no longer abort a batch at a migrated site.
- **Harder:** a pattern that names non-ASCII code points has to spell them
  as bytes or use UTF-8 PCRE mode, and a byte-mode pattern cannot use the
  shorthand classes.
- **Revisit** if `stringi` gains a switch that keeps a leading U+FEFF (the
  unit and invalid-row arguments would still stand), or if R stops building
  byte-mode PCRE tables from the locale (the pattern check could then
  relax).

## Decisions at acceptance

The five questions the draft left open were settled by the maintainer on
2026-10-24. Each recommendation was also put to a separate second-opinion
consultation, and each consultation agreed:

1. **Refuse, do not rewrite.** `R/strings-base.R` refuses locale-table
   constructs in a byte-mode pattern, and a slice spells the class out
   (`[ \t\n\r\f\v]` for `\s`). A refusal is a prompt to review the call's
   semantics: a call that needs code points moves to UTF-8 PCRE mode instead
   of being expanded into byte classes.
2. **No case-insensitive matching in byte mode.** Byte-mode `(?i)` folds by
   the locale's tables, and that was measured under `C` and `en_US.UTF-8`
   only, not in a single-byte locale such as ISO-8859-9. Turkish-locale case
   folding broke `pslr` 1.2.1 (`tolower("ICANN")` gave `"ıcann"`), so the
   pattern check refuses every spelling that turns it on (`(?i)`,
   `(?i:...)`, `(?mi)`, `(?^i)`), on ASCII patterns too, and the wrappers
   take no `ignore.case` argument. A slice spells each letter as a class
   (`[Hh][Tt][Tt][Pp]`). The 12 `(?i)` patterns and 4 calls with the ICU
   flag are to be rewritten that way, in slices oahhlnvf and yqmoztoj. The
   rewrite changes behavior: ICU's caseless match also folds U+017F (ſ) to
   `s` and U+212A (Kelvin sign) to `k`, so `"httpſ"` matches `(?i)^https$`
   today and will not after the rewrite. The URL standards match schemes
   and these keywords ASCII case-insensitively (WHATWG URL Standard,
   "ASCII case-insensitive"; RFC 3986 section 3.1), so the change moves
   toward conformance, and each slice that makes it carries a NEWS entry
   and a test for the non-ASCII fold it drops (AGENTS.md).
3. **Keep the decodable mask at acceptance seams**, with no per-seam proof
   asked for instead (Consequences, "Acceptance at host seams").
4. **`stringi` stays in `Imports:`** for NFC before Punycode and full case
   mapping only, and for `.param_matches()` if RURL-yyeohqah keeps it. The
   close-out allowlist holds those calls and no others. Dropping the import
   is revisited only when `punycoder` or base R covers each of them with the
   same semantics. The case mapping that stays is tested in more than one
   locale.
5. **Fold the old byte helpers in at the close-out.** `.byte_substring()`,
   `.byte_substring_vec()` and `.first_byte_index*()` in `R/utils.R` move
   into `.bytes_cut()` and `.bytes_locate()` in RURL-dddxutjf, not earlier, so
   each slice's diff stays scoped. Before and after, the tests compare
   encoding marks as well as values, byte offsets, `NA` and empty input,
   and invalid rows. Declaring a result UTF-8 does not make a byte slice
   valid UTF-8.

## Semantic exceptions (RURL-yyeohqah)

The maintainer decided the four semantic exceptions on 2026-10-24, each after
a separate second-opinion consultation that agreed, with one amendment each.
`tests/testthat/test-stringi-semantic-exceptions.R` pins each one, and runs
under `LC_ALL=C` and a UTF-8 locale.

1. **Cache key (`.cache_key_ascii()`, `R/zzz.R`): base R.** Each octet is
   spelled on its own: an ASCII octet other than `\` is itself, `\` is `\\`,
   and an octet at or above 0x80, from valid UTF-8 or not, is `\x` and its
   two lowercase hex digits; `NA` is `\zNA`. A single `\` in a key's image is
   only ever followed by `\` or `x` and two hex digits, so the map is
   injective and `NA`'s spelling is disjoint from it. Escaping per octet, not
   per key, keeps a mostly-ASCII key about its own length: hexing a whole key
   once it held one non-ASCII octet doubled it, so a 5,000-byte URL with one
   `é` passed R's 10,000-byte variable-name cap and was never cached. A key
   of non-ASCII text grows to about four times its octets instead. An
   all-ASCII key only has its `\` doubled, in one vectorized `gsub()`; only a
   key holding an octet at or above 0x80 is taken apart into octets;
   deriving 1e5 such keys at once takes about 0.3 s, as the whole-key hex
   did. A latin1 key is first spelled in UTF-8 by the exact ISO 8859-1 map
   (amendment: raw octets alone collide, since latin1 `Ã©` holds the octets
   C3 A9 of UTF-8 `é`). The U+FEFF placeholder splice and the invalid-UTF-8
   fallback are gone, because octets are data. `validUTF8()` turned out not
   to be needed: valid and invalid non-ASCII keys take the same path, so the
   key needs no validity test. The caches live in memory only, so the change
   invalidates nothing persistent.
2. **Encoding calls.** (a) latin1 to UTF-8 by the exact map
   `intToUtf8(as.integer(charToRaw(s)))` per latin1 element
   (`.latin1_input_to_utf8()`), never `iconv()` or `enc2utf8()`, which read
   0x80-0x9F as CP1252 on Windows. (b) Validity: see 1. (c) Base
   `Encoding()` in `.prepare_url_input()` (amendment: `Encoding()` reports
   ASCII and unmarked non-ASCII alike as `"unknown"`, so a byte-mode scan for
   an octet at or above 0x80 tells them apart, and the ASCII fast path still
   returns the vector as it came). Byte-identical on the existing tests.
   (d) **Kept in `stringi`:** `stri_enc_toutf8(validate = TRUE)` in
   `.url_key_frame()` (`R/url-key.R`), for its U+FFFD decode. A fixture
   table (truncated, overlong, surrogate, above U+10FFFF, F5-FF leads) shows
   it replaces one U+FFFD per maximal subpart, the WHATWG Encoding Standard's
   UTF-8 decoder rule (amendment). It keeps its `"x"` splice, because it
   drops a leading U+FEFF and the key frames the mark. `stri_numbytes()` in
   the same function was left for a later slice.
3. **Parameter globs (`.glob_to_regex()`, `.param_matches()`).** The glob
   compiler and the case-sensitive arm move to base R in byte mode. The glob
   compiler escapes only ASCII punctuation (amendment: escaping each
   non-alphanumeric octet corrupts a multibyte literal), compiles `*` to
   `(?s:.*)` and anchors with `\z`, so the regex reads the same in PCRE and
   ICU. **Kept in `stringi`:** the case-insensitive arm,
   `stri_detect_regex(case_insensitive = TRUE)`, for ICU's caseless match,
   which uses Unicode full case folding (`SS` matches U+00DF, `k` matches
   U+212A) and does not follow the ICU default locale (tested with
   `stri_locale_set("tr_TR")`). Measured first, as the decision asked:
   `stri_detect_regex("﻿utm_source", "^utm_.*$")` is `FALSE`, so the
   regex engine counts a leading mark and the arm needs no guard. A leading
   U+FEFF is data on both sides: `utm_*` does not match it, `*utm_*` does,
   and a pattern may carry the mark. The move changes two outputs, both in
   `NEWS.md`: `*` now matches line terminators (ICU's `.` stopped at CR, LF,
   U+0085, U+2028 and U+2029, and its `$` matched before a final one), and
   in the case-sensitive arm an undecodable name no longer matches a pattern
   spelled with U+FFFD.
4. **NFC and case mapping stay in `stringi`.** The two `stri_trans_nfc()`
   calls stay in `R/domain.R`, frozen by ADR 0002; "confined" means the
   allowlist names exactly these reviewed calls (file, function, operation),
   with no file move. `stri_trans_nfc()` counts a leading U+FEFF, so it has
   no splice guard and needs none; a test shows NFC keeps the mark.
   `.unicode_case_map_vec()` keeps its `"0"` splice guard, with a test. Its
   locale stays `.ASCII_SAFE_ICU_LOCALE` (`"en_US_POSIX"`), not `"root"` as
   the decision proposed: measured on stringi 1.8.9 / ICU 71.1, after
   `stri_locale_set("tr_TR")` both `locale = "root"` and `locale = "und"`
   lowercase `WIKI` to `wıkı`, as `""` does, while `"en_US_POSIX"` gives
   `wiki`. The Turkish regression test sets and restores stringi's default
   with `stri_locale_set("tr_TR")` (amendment) and goes red when the locale
   is `"root"`.

## Close-out (RURL-dddxutjf)

The calls outside decision 4's kept set were settled on 2026-10-25. The
maintainer delegated the calls to an agent recommendation that a separate
second-opinion consultation had to agree with, and it did, on each one:

- `.split_host_labels()` (R/domain.R, ADR 0002's helpers): moved to
  `.bytes_split_fixed(keep_trailing_empty = TRUE)`, and its U+FEFF guard was
  removed. ADR 0002 carries a dated note.
- `.url_key_frame()`'s `stri_numbytes()`: moved to `nchar(type = "bytes")`.
- `resolve_url()`'s `stri_enc_toutf8()` decode: kept under semantic exception
  2(d), now through `.utf8_decode_replacing()` (R/parse-phases.R), which the
  whatwg input trim shares.
- `.strip_whatwg_control_chars_vec()`'s leading `stri_replace_first_regex()`:
  removed. Its ICU read was also the U+FFFD decode the whatwg route relies on,
  so that decode is now explicit (`.utf8_decode_replacing()` on the rows that
  are not valid UTF-8) and both trims run in byte mode. This adds no
  exception beyond 2(d). The two decodes write the same U+FFFD per maximal
  subpart on 2(d)'s fixture classes, and the merge was gated on a differential
  (WPT inputs, the determinism corpus and a byte fuzz; parse, clean,
  serialize, key and resolve; C and UTF-8 locales) that moved no value and no
  encoding mark.

The allowlist now holds the kept set and nothing else: 8 references under 7
keys.
