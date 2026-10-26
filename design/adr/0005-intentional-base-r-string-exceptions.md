# ADR 0005: Intentional base-R string exceptions (the `stringi` migration is not total)

- **Status:** Accepted; superseded in part by [ADR 0019](0019-string-operations-run-in-base-r.md) (accepted 2026-10-24); see the note at the end
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
| `R/accessors.R` | `.subdomain_labels_vec()` | Retired 2026-10-25 by RURL-yqmoztoj, !256 (ADR 0019): every host's prefix is cut in bytes with `.bytes_cut()` after the base-R `endsWith()`, so a mark-led host needs no mask. Was: `endsWith()`; mask, the prefix of a mark-led host cut as bytes (`.byte_substring_vec()`, `.byte_length()`) | RURL-fyiadctm |
| `R/domain.R` | `.split_host_labels()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (maintainer delegation, second-opinion agreement): the split is `.bytes_split_fixed(keep_trailing_empty = TRUE)`, which keeps the trailing empty label and reads a leading U+FEFF as data. Outside ADR 0001's `.psl_*` seam; ADR 0002's contract unchanged (see its note). Was: mask; `x` spliced before the two `stri_split_fixed()` calls, cut off the first label as a byte | RURL-tbmnnauj |
| `R/parse-phases.R` | `.starts_with_bom()` | the base-R byte test itself | RURL-vhionecz |
| `R/parse-phases.R` | `.rewrite_whatwg_backslashes_vec()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the scheme capture and the `\` rewrite run in byte mode, which reads a leading U+FEFF as data, so a mark-led row is never eligible; `//` is now the collapsed run, joined after the rewrite. Was: mask; `//` spliced before the fixed rewrite | RURL-iiehsbfg |
| `R/parse-phases.R` | `.strip_whatwg_control_chars_vec()` | byte-mode strip and flag, which count the mark (RURL-dddxutjf, 2026-10-25: the leading strip's `stri_replace_first_regex()` is gone; its hidden U+FFFD decode is now `.utf8_decode_replacing()`, below) | RURL-vhionecz, RURL-bnorcgbv (pinned in RURL-zxnxczsw) |
| `R/parse-phases.R` | `.utf8_decode_replacing()`, for the whatwg input trim and `resolve_url()` | mask; `x` spliced before `stri_enc_toutf8()`, then cut. Kept under ADR 0019: the U+FFFD decode is its semantic exception 2(d), and `stri_enc_toutf8()` still drops a leading U+FEFF | RURL-tfttelwo, RURL-dddxutjf |
| `R/parse-phases.R` | `.map_whatwg_domain_separators_vec()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the captures and locators run in byte mode and find no scheme or `//` on a mark-led row. Was: mask; spans cut from the whole input | RURL-exdlurql |
| `R/parse-phases.R` | `.percent_encode_userinfo_charset()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the replaces run in byte mode, which keeps a leading U+FEFF. Was: `@` spliced, cut as a byte | RURL-rfgbozdr |
| `R/parse-phases.R` | `.rfc3986_path_rootless_vec()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the scheme capture runs in byte mode and finds none on a mark-led row. Was: mask | RURL-biunpazk |
| `R/parse-phases.R` | `.whatwg_file_normalize_host()` | `startsWith()` | RURL-azcvukyh |
| `R/parse-phases.R` | `.parse_whatwg_file_urls_vec()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the `\` rewrite of the path runs in byte mode, which keeps a leading U+FEFF; the prefix tests are base R or byte mode. Was: `/` spliced, cut as a byte. Still live: `startsWith()` | RURL-rjpljsui, RURL-azcvukyh |
| `R/parse-phases.R` | `.prepare_urls_for_parse_vec()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out, with the same mask in `.general_route_input_vec()`: every scheme and `//` test runs in byte mode or base R, which find no scheme and no `//` on a mark-led row. Was: mask | RURL-vhionecz |
| `R/parse-phases.R` | `.extract_raw_path_vec()` | Retired 2026-10-25 by RURL-yqmoztoj, !259 (ADR 0019): the locators and cuts run in byte mode, which counts the mark. Was: regex locators, which count the mark | RURL-vhionecz |
| `R/parse-phases.R` | `.normalize_path_vec()`, trailing-slash branch | `endsWith()`; the slash cut as a byte | RURL-zxnxczsw |
| `R/parse-phases.R` | `.apply_case_policy_vec()`, via `.unicode_case_map_vec()` | mask; `0` spliced before `stri_trans_toupper()` / `stri_trans_tolower()`, cut as a byte. Kept under ADR 0019: full case mapping stays in `stringi` (its "Semantic exceptions", 4), and those calls still drop a leading U+FEFF | RURL-opwjlnla |
| `R/parse-phases.R` | `.apply_subdomain_policy_vec()` | `startsWith()`, then `stri_sub()` on rows it passes; the stringi reading kept under `whatwg` | RURL-opwjlnla |
| `R/parse-phases.R` | `.apply_www_if_no_subdomain_vec()` | `startsWith()`; the stringi reading kept under `whatwg` | RURL-opwjlnla |
| `R/parse-phases.R` | `.detect_ip_host_vec()` | Retired 2026-10-25 by RURL-oahhlnvf, !254 (ADR 0019): the detectors run in byte mode, which counts the mark. Was: regex detector for the dotted-quad tail, which counts the mark | RURL-opwjlnla |
| `R/parse-phases.R` | `.classify_input_host_vec()`, `.apply_host_standard_model_vec()` | `startsWith()` | RURL-opwjlnla |
| `R/parse-phases.R` | `.encode_userinfo_charset_vec()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the capture runs in byte mode and finds no scheme on a mark-led row. Was: mask | RURL-opwjlnla |
| `R/parse-state.R` | `.rfc3986_first_index()`, `.split_authority()` | Retired 2026-10-25 by RURL-yqmoztoj, !258 (ADR 0019): the locators are fixed byte-mode `regexpr()` and `.last_byte_index()`, and the cuts `.bytes_cut()`, all counting the mark. Was: literal regex locators | RURL-biunpazk |
| `R/parse-state.R` | `.rfc3986_generic_uri_ok()`, `.general_parsed_mask()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the scheme capture (`.scheme_prefix_len()`, `.scheme_prefix()`) runs in byte mode and finds no scheme on a mark-led row. Was: mask | RURL-biunpazk, RURL-vhionecz |
| `R/path-query.R` | `._remove_dot_segments()` | `startsWith()`, `substring()` with an explicit `last` | RURL-xrjtdnko, RURL-zxnxczsw |
| `R/path-query.R` | `._remove_dot_segments_whatwg()` | `substring()` with an explicit `last` | RURL-zxnxczsw |
| `R/path-query.R` | `._strip_index_page()` | Retired 2026-10-25 by RURL-yqmoztoj, !256 (ADR 0019): the page search runs in byte mode, which reads a leading U+FEFF as data, and the kept directory is cut in bytes. Was: `x` spliced, cut as a byte. Still live: `startsWith()` | RURL-zxnxczsw |
| `R/path-query.R` | `._encode_path_segments()` | Retired 2026-10-26 by RURL-dbyavcjt: replaced by `.rfc3986_path_percent_encode()`, which maps each octet on its own and never splits or tests the path's ends, so a leading U+FEFF is data. Was: `startsWith()`, `endsWith()` | RURL-zxnxczsw |
| `R/resolve.R` | `.whatwg_file_remove_dot_segments()` | Retired 2026-10-25 by RURL-yqmoztoj, !260 (ADR 0019): the segments split in byte mode, so a U+FEFF that starts the body stays on its segment. Was: `/` spliced before the split | RURL-dglgcwit |
| `R/serialize.R` | `.fsss_source_lex()` | Retired 2026-10-25 by RURL-dddxutjf, the ADR 0019 close-out (guard removal): the authority capture runs in byte mode on the stripped source and finds no scheme on a mark-led row. Was: mask on the stripped source | RURL-rynmvtwp |
| `R/url-key.R` | `.url_key_frame()` | mask; `x` spliced before `stri_enc_toutf8()`, then cut; `nchar(type = "bytes")` counts the mark (was `stri_numbytes()`, replaced 2026-10-25 by RURL-dddxutjf). Kept under ADR 0019: `stri_enc_toutf8()` stays for its U+FFFD decode (its "Semantic exceptions", 2(d)) and still drops a leading U+FEFF | RURL-dquaxcaz |
| `R/url-key.R` | `.url_key_scheme_presence()` | Retired 2026-10-24 by RURL-oahhlnvf (ADR 0019): the scheme tests run in byte mode, which reads a leading U+FEFF as data. Was: `stri_detect_regex()`, which counts the mark | RURL-dquaxcaz |
| `R/utils.R` | `.ascii_case_fallback()`, under `.ascii_tolower()` / `.ascii_toupper()` | Retired 2026-10-24 by RURL-oahhlnvf (ADR 0019): the 26 fixed replacements run in byte mode, which keeps a leading U+FEFF; still pinned by `test-ascii-case-leading-bom.R`. Was: mask; `0` spliced before the fixed replace, cut as a byte | RURL-bqhfznkg |
| `R/zzz.R` | `.cache_key_escape()`, under `.cache_key_ascii()` | Retired 2026-10-24 by RURL-yyeohqah (ADR 0019): the key escapes each octet in base R, so a leading U+FEFF is data and needs no guard. Was a `startsWith()` mask, with `x` spliced before `stri_escape_unicode()`, then cut | RURL-iiehsbfg, RURL-yyeohqah |

**Consequences.**

- **Standing rule:** a reviewer who sees base R, a splice or a mask in these
  helpers should treat it as intentional, like the calls above.
- New `stringi` code that can see the start of a whole input, host, userinfo,
  path, query or fragment uses a counting call or one of these guards, or
  states why its input cannot start with U+FEFF. A fix's test pairs each
  U+FEFF row with a U+200B twin, a zero-width code point `stringi` does not
  skip, so the test shows the defect is the mark's alone.
- The RURL-zxnxczsw audit found further unguarded sites. All are now guarded
  and in the table: host case mapping (RURL-opwjlnla),
  `host_encoding = "unicode"` (RURL-tbmnnauj), the subdomain trim
  (RURL-fyiadctm), the URL key's framing (RURL-dquaxcaz) and the ASCII case
  helpers' fallback (RURL-bqhfznkg). A related follow-up is open: under
  `whatwg`, the www and subdomain policies read the host before UTS-46
  mapping (RURL-fobwctml).
- A base-R suffix cut passes `last` explicitly (`.Machine$integer.max`), as
  `.bytes_cut()` callers do (`.byte_substring()`, which defaulted it, was
  folded into `.bytes_cut()` by ADR 0019 decision 5): before R 4.6.0,
  `substring(x, first)` defaulted to `last = 1000000L`, and the package
  supports R 4.1.0 and later.
  `tools/substring-last-gate.R` now enforces this on every `substring()` call
  in `R/`.
- The measured lists are stringi 1.8.9 behavior. A stringi release that stops
  skipping the mark would make these guards redundant, not wrong; the tests
  would still pass.

## Note: superseded in part by ADR 0019

*Added RURL-lbccaixb, 2026-10-12. Appended, so no line citation into this file
moves. It took effect when ADR 0019 was accepted, on 2026-10-24.*

[ADR 0019](0019-string-operations-run-in-base-r.md) supersedes one
consequence: "New string code should still prefer `stringi` unless it hits one
of these documented equivalence gaps." String operations now run in base R,
and each declares its semantics. This ADR's other rules still apply: the
`strsplit()` split contract, the rejection of migrating a call for consistency
alone, the U+FEFF-as-data rule with its U+200B twin, the guard table (in force
for each helper until a migration slice moves it), and the explicit-`last`
rule for `substring()`.
