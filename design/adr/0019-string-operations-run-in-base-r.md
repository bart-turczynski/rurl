# ADR 0019: String operations run in base R, each with declared semantics

- **Status:** Proposed
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
  boundaries, leading `(*...)` verbs, and, under `(?i)`, high bytes and
  back-references). An end anchor is written `\z`: ICU's `$` also matches
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

The semantic exceptions get their own decision in RURL-yyeohqah, because
their behavior is the operation: the cache-key escape (`stri_escape_unicode()`
and `stri_enc_isutf8()` in `R/zzz.R`), encoding conversion
(`stri_enc_toutf8()` in `.latin1_input_to_utf8()` and `.url_key_frame()`,
with `stri_numbytes()`), and the Unicode glob (`.glob_to_regex()`).

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
  moves a call feeding an acceptance mask shows that the same rows are still
  rejected, or keeps the decodable mask at that seam.
- **Semantics a slice must check, per call** (the inventory lists them):
  ICU `$` (37 references), ICU `.` (17), `(?i)` and the ICU
  case-insensitive flag on ASCII patterns, `[\s\S]` (written `(?s:.)`),
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

## Open questions

For the maintainer at acceptance:

1. **Refuse or rewrite?** `R/strings-base.R` refuses locale-table constructs
   in a byte-mode pattern rather than rewriting them, so a slice spells
   classes out (`[ \t\n\r\f\v]` for `\s`). Is refusal the policy you want?
2. **`(?i)` in a single-byte locale.** Byte-mode `(?i)` relies on the
   locale's tables folding an ASCII letter only to its ASCII partner. That
   was measured true under `C` and `en_US.UTF-8`. It was not measured under a
   single-byte locale such as ISO-8859-9 (Turkish), where the C library may
   fold `i` to 0xDD, a UTF-8 lead byte. Accept the risk, or refuse `(?i)` in
   byte mode and spell `[Ii]`? Today 12 patterns use `(?i)` and 4 calls use
   the ICU flag.
3. **Undecodable rows at acceptance seams.** Proposed: slices keep the
   `.grepl_decodable()` mask wherever a migrated call feeds an acceptance
   mask, and use plain byte mode elsewhere. Confirm, or ask for a per-seam
   proof instead.
4. **`stringi` in `Imports:` after the close-out.** NFC and case mapping keep
   it unless `punycoder` or base R covers them; base R has no NFC. Proposed:
   `stringi` stays in `Imports:` for those calls only.
5. **The existing byte helpers.** `.byte_substring()`, `.byte_substring_vec()`
   and `.first_byte_index*()` in `R/utils.R` keep the input's encoding mark
   instead of declaring UTF-8. Fold them into `.bytes_cut()` and
   `.bytes_locate()` at the close-out, or keep both?
