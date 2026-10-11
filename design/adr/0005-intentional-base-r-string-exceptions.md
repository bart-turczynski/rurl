# ADR 0005: Intentional base-R string exceptions (the `stringi` migration is not total)

- **Status:** Accepted
- **Date:** 2026
- **Tracking:** n/a (recorded during the stringi migration)

## Context

rurl migrated most string manipulation from base R to `stringi` for Unicode
correctness. A blanket "finish the migration for consistency" would be wrong:
some base-R calls are retained because they are *not* behaviorally equivalent to
their `stringi` counterparts, and swapping them would silently change parsing.

## Decision

Keep the following base-R string calls deliberately:

- **`strsplit(..., fixed = TRUE)`** for host-label / subdomain splits on `"."`
  and the path splits in `path-query.R`. It is **not** equivalent to
  `stringi::stri_split_fixed()`: `strsplit("a.b.", ".", fixed = TRUE)` drops the
  trailing empty (`c("a","b")`) and returns `character(0)` for `""`, whereas
  `stri_split_fixed` keeps the trailing `""` and returns `""`. The dot splits
  use `fixed = TRUE` (verified identical to the regex `"\\."` for the
  trailing-empty behavior), which also keeps `fixed_regex_linter` quiet.
- **Genuine ASCII regexes** — `gsub("/+", …)`, `sub("/?[^/]*$", …)`, the
  `regexpr`/`regmatches` in `._remove_dot_segments`, `grepl("^www[0-9]*$", …)`,
  and the status-pattern `grepl` in `canonical_join.R` — operate on ASCII
  delimiters/anchors where `stringi` offers no correctness or Unicode benefit.

## Consequences

- **Standing rule:** do not "finish the migration" on these calls for
  consistency alone. A reviewer seeing a base-R string call in these locations
  should treat it as intentional.
- New string code should still prefer `stringi` unless it hits one of these
  documented equivalence gaps.

## Amendment: a leading U+FEFF is a third equivalence gap

*Added RURL-zxnxczsw, 2026-10-10. Appended rather than edited in place, so no
line citation into this file moves.*

**The gap.** Measured on stringi 1.8.9 (ICU 71.1): many `stringi` functions
read a U+FEFF that *starts* their input as a byte-order mark and skip it, in
the subject and in a fixed pattern alike. They include `stri_sub()`,
`stri_match_*_regex()`, `stri_extract_*()`, `stri_split_*()` (fixed and
regex), the `*_fixed()` family (`stri_startswith_fixed()`,
`stri_locate_*_fixed()`, `stri_replace_*_fixed()`, ...),
`stri_trans_tolower()` / `stri_trans_toupper()`, `stri_trim_*()`,
`stri_enc_toutf8()`, `stri_escape_unicode()` and `stri_paste()`. Others count
it: `stri_detect_regex()`, `stri_replace_*_regex()`, `stri_locate_*_regex()`,
`stri_length()`, `stri_numbytes()` and `stri_trans_nfc()`. Base R
(`startsWith()`, `endsWith()`, `substring()`, `nchar()`, `grepl()`, `sub()`,
`regexpr()`) counts it everywhere. A U+FEFF is ordinary data in a URL: the
WHATWG URL Standard does not strip it (it is not a C0 control or space) and
RFC 3986 has no rule for it. So a skipping call on a string that can start
with one drops the mark, reads a prefix past it (a scheme, `//`, `/`,
`www.`), or returns a position one off from a counting call's.
This list is the reference; the comment above `.starts_with_bom()` in
`R/parse-phases.R` quotes only part of it.

**Decision.** The helpers below are guarded against the gap on purpose. Each
uses base R, a `.starts_with_bom()` mask, a `stringi` call that counts the
mark (a `*_regex()` detector, replacer or locator), or a `stringi` call fed an
ASCII byte spliced on first and cut off as a byte. Do not migrate them back to the
skipping `stringi` call, and do not swap a counting `stringi` call there for
a skipping one (a `*_regex()` locator for a `*_fixed()` one):

| File | Helper | Guard | Ticket |
|---|---|---|---|
| `R/parse-phases.R` | `.starts_with_bom()` | the base-R byte test itself | RURL-vhionecz |
| `R/parse-phases.R` | `.rewrite_whatwg_backslashes_vec()` | mask; `//` spliced before the fixed rewrite | RURL-iiehsbfg |
| `R/parse-phases.R` | `.strip_whatwg_control_chars_vec()` | regex strip and flag, which count the mark | RURL-vhionecz, RURL-bnorcgbv (pinned in RURL-zxnxczsw) |
| `R/parse-phases.R` | `.map_whatwg_domain_separators_vec()` | mask; spans cut from the whole input | RURL-exdlurql |
| `R/parse-phases.R` | `.percent_encode_userinfo_charset()` | `@` spliced, cut as a byte | RURL-rfgbozdr |
| `R/parse-phases.R` | `.rfc3986_path_rootless_vec()` | mask | RURL-biunpazk |
| `R/parse-phases.R` | `.whatwg_file_normalize_host()` | `startsWith()` | RURL-azcvukyh |
| `R/parse-phases.R` | `.parse_whatwg_file_urls_vec()` | `/` spliced, cut as a byte; `startsWith()` | RURL-rjpljsui, RURL-azcvukyh |
| `R/parse-phases.R` | `.prepare_urls_for_parse_vec()` | mask | RURL-vhionecz |
| `R/parse-phases.R` | `.extract_raw_path_vec()` | regex locators, which count the mark | RURL-vhionecz |
| `R/parse-phases.R` | `.normalize_path_vec()`, trailing-slash branch | `endsWith()`; the slash cut as a byte | RURL-zxnxczsw |
| `R/parse-state.R` | `.rfc3986_first_index()`, `.split_authority()` | literal regex locators | RURL-biunpazk |
| `R/parse-state.R` | `.rfc3986_generic_uri_ok()`, `.general_parsed_mask()` | mask | RURL-biunpazk, RURL-vhionecz |
| `R/path-query.R` | `._remove_dot_segments()` | `startsWith()`, `substring()` with an explicit `last` | RURL-xrjtdnko, RURL-zxnxczsw |
| `R/path-query.R` | `._remove_dot_segments_whatwg()` | `substring()` with an explicit `last` | RURL-zxnxczsw |
| `R/path-query.R` | `._strip_index_page()` | `x` spliced, cut as a byte; `startsWith()` | RURL-zxnxczsw |
| `R/path-query.R` | `._encode_path_segments()` | `startsWith()`, `endsWith()` | RURL-zxnxczsw |
| `R/resolve.R` | `.whatwg_file_remove_dot_segments()` | `/` spliced before the split | RURL-dglgcwit |
| `R/zzz.R` | `.cache_key_escape()`, under `.cache_key_ascii()` | `startsWith()` mask; `x` spliced before `stri_escape_unicode()`, then cut | RURL-iiehsbfg |
| `R/serialize.R` | `.fsss_source_lex()` | mask on the stripped source | RURL-rynmvtwp |
| `R/accessors.R` | `.subdomain_labels_vec()` | `endsWith()`; mask, the prefix of a mark-led host cut as bytes (`.byte_substring_vec()`, `.byte_length()`) | RURL-fyiadctm |
| `R/url-key.R` | `.url_key_frame()` | mask; `x` spliced before `stri_enc_toutf8()`, then cut; `stri_numbytes()` counts the mark | RURL-dquaxcaz |
| `R/url-key.R` | `.url_key_scheme_presence()` | `stri_detect_regex()`, which counts the mark | RURL-dquaxcaz |

**Consequences.**

- **Standing rule:** a reviewer who sees base R, a splice or a mask in these
  helpers should treat it as intentional, like the calls above.
- New `stringi` code that can see the start of a whole input, host, userinfo,
  path, query or fragment uses a counting call or one of these guards, or
  states why its input cannot start with U+FEFF. A fix's test pairs each
  U+FEFF row with a U+200B twin, a zero-width code point `stringi` does not
  skip, so the test shows the defect is the mark's alone.
- The RURL-zxnxczsw audit found further unguarded sites (host case mapping,
  `host_encoding = "unicode"`, the subdomain trim, the URL key's framing, the
  ASCII case helpers' fallback). They are tracked separately and join the
  table when fixed.
- A base-R suffix cut passes `last` explicitly (`.Machine$integer.max`) or
  uses `.byte_substring()`: before R 4.6.0, `substring(x, first)` defaulted to
  `last = 1000000L`, and the package supports R 4.1.0 and later.
- The measured lists are stringi 1.8.9 behavior. A stringi release that stops
  skipping the mark would make these guards redundant, not wrong; the tests
  would still pass.
