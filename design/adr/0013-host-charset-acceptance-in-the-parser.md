# ADR 0013: Host-charset acceptance belongs to the parser, on its own dial

- **Status:** Accepted
- **Date:** 2026-07-29
- **Tracking:** RURL-ezhzpkhg (deletion 1 of the libcurl compensation layer),
  parent epic RURL-dorofzmb. Supersedes **ADR 0009**. Relates to ADR 0002
  (reversible host), ADR 0004 (host-shape gate), ADR 0007 (`url_standard`
  governed axes), ADR 0011 (the same un-conflation, one axis over).

## Context

ADR 0009 was written when rurl delegated host parsing to libcurl. libcurl
enforces a narrower host allowed-set than the WHATWG URL Standard, and a
rejection dropped the **entire row**, so a WHATWG-valid URL failed to parse
under the `whatwg` profile. With the engine out of reach, the only available
lever was the input string: ADR 0009 added a pre-parse **shim** that substituted
filler bytes for the 15 gap code points, let libcurl read the structure, and
restored the true host afterwards.

**The engine is no longer out of reach.** `RURL-robgajml` brought the parser
in-tree (`R/parse-web.R`) and dropped `Imports: curl`; the allowed-set ADR 0009
worked around is now `.WEB_HOST_ALLOWED_BYTES`, a constant this package owns.
The shim's premise has expired.

It is not only obsolete, it is **wrong in a way only its removal exposes**. A
pre-parse rewrite needs an eligibility test, and the shim's was a regex over the
whole URL. So which literal bytes a host was allowed to contain came to depend
on things the host does not contain — measured, at HEAD, all three under
`url_standard = "whatwg"` or `"rfc3986"`:

| shape | at HEAD | why |
|---|---|---|
| `http:/a!b.com/p`, `http:///a!b.com/p` | rejected | the pattern hard-required a literal `//`; the parser accepts 1..3 slashes |
| `http://a!b.com/p<VT>q` | rejected | the post-authority remainder was matched with an ICU `.`, which excludes the Unicode line terminators (VT, FF, NEL, LS, PS) |
| `ftps://a!b.com/p` | rejected under `whatwg`, **accepted** under `rfc3986` | eligibility was scoped to a scheme set, and `ftps` is not a WHATWG special scheme |

Removing the `<VT>` from the second row makes it parse. That is the defect
class: **a regex-shaped eligibility gate silently narrows the set it claims to
cover**, and this is its fourth instance in this epic (after deletion 3's
required `//`, deletion 2's `^scheme://`, and deletion 5's ICU `.`).

The second force is a conflation. `host_pct` (RURL-rgjpcbuk) already carried
"which triplets are decoded, and therefore which byte set the result is judged
against" — two answers on one dial. Adding the literal set would have made three,
and the three profiles do not agree on the diagonal:

| profile | rendering | literal set | decoded set |
|---|---|---|---|
| `whatwg` | decoded | the 15 | the 15 |
| `rfc3986` | source-preserving (§6.2.2.2) | the **11** `sub-delims` | the 15 |
| no selector | decoded | none | none |

`rfc3986` pairs RFC rendering with the *narrower* literal set, `whatwg` pairs
decoded rendering with the *wider* one. One dial can only express the diagonal,
so a value named after percent-decoding would have been deciding whether a
literal `!` is a legal host byte.

## Decision

**Delete the shim. Move host-charset acceptance into `.parse_web_url_one()` as
its own dial, separate from rendering.**

1. `.shim_whatwg_host_charset_vec()` is gone, with its Phase-1 call, its
   `restore_host_shimmed` / `shimmed_true_host` plumbing, and the Stage-A
   restore step in `._parse_stage_a_vec()`. The two ICU classes it consumed
   (`.WHATWG_HOST_CHARSET_SHIM_CP`, `.RFC3986_REG_NAME_SUB_DELIM_CP`) are gone
   with it, replaced by BYTE sets in the parser that judges them
   (`.WEB_HOST_GAP_BYTES`, `.WEB_HOST_SUBDELIM_BYTES`). The change of unit is
   deliberate: an ICU class only matches a string `stringi` will accept, and a
   host token may be declared UTF-8 while holding invalid octets
   (RURL-kmpnbvdl).

2. **`host_charset`** is a new parser dial and owns ACCEPTANCE alone:
   `"narrow"` (libcurl's measured set — the no-selector default), `"whatwg"`
   (plus the 15, literally and percent-encoded), `"rfc3986"` (plus the 11
   `sub-delims` literally; the full 15 once decoded, because `%60` is a
   well-formed `pct-encoded` and `reg-name` admits it however it decodes).

3. **`host_pct`** narrows to RENDERING alone: `"decode"` / `"keep"`. Its
   `"narrow"`/`"wide"` values differed only in the judged set, which is now
   `host_charset`'s, so they collapsed into one.

4. Acceptance is **scheme-independent**. ADR 0009 deferred this
   ("ftps and opaque hosts") pending confirmation that WHATWG keeps the 15
   literal in an *opaque* host too. It does: the opaque-host parser
   percent-encodes with the **C0 control percent-encode set** (C0 controls plus
   everything above U+007E), and all 15 are printable ASCII at or below U+007E,
   so none is escaped. RFC 3986's `reg-name` is scheme-independent by
   construction. The scoping was an artifact of the rewrite, not a rule.

5. The `host-charset-shimmed` **diagnostic keeps its name and its meaning**,
   now derived from the PARSED host rather than from a Phase-1 rewrite flag. It
   always described the resulting host — `man/get_url_diagnostics.Rd` already
   words it as "the host carries one of the 15" — and that description is
   unchanged. The name is now a fossil; renaming it is a public-surface change
   and is left to a separate decision.

## Consequences

- **The no-selector profile does not move at all.** Measured: 0 differing rows
  across every corpus, `default accepted` unchanged in both the original and
  the extended sweep. `host_charset = "narrow"` is byte-for-byte the historical
  behaviour.
- **`whatwg` and `rfc3986` widen, by exactly the enumerated gate gaps.** On the
  original 5298-input octet corpus, all three sweeps report **0 differing rows**
  — the corpus varies host octets inside a fixed `scheme://` frame and so cannot
  see a structural gate. The corpus was extended with a literal-byte × structure
  block (`tools/octet-acceptance-sweep.R`), which reports **+90 `whatwg` / +44
  `rfc3986` / +0 default**, zero narrowings, and byte sets that are exactly the
  15 and exactly the 11 with no negative control moving.
- **Standing rule, now paid for four times: when a compensation is gated by a
  REGEX, enumerate what the regex CANNOT match before porting its semantics.**
  That set is part of the behaviour being replaced. ICU semantics are their own
  hazard — `.` is not `[^\n]`, and `$` also matches *before* a trailing line
  terminator.
- **Standing rule: a corpus that varies one axis inside a fixed frame cannot
  falsify a rule gated on the frame.** The three checked-in sweeps scored a
  truthful 0 here while the behaviour had genuinely moved. The instrument was
  proven red first (neutering the mask moved 405 octet-sweep rows), which is
  what made the 0 readable as "out of corpus reach" rather than "no change".
- **ADR 0002, ADR 0004 preserved.** The punycode/reversible-host helpers are
  untouched, and the host-shape and forbidden-code-point gates still run on the
  parsed host — they now run on a host the parser produced directly rather than
  one substituted back into it.
- **ADR 0007 refined.** "Host allowed-set" remains a governed axis, but it is
  governed *at the parser*, and it is governed separately from host rendering.
- **What would justify revisiting:** a standard whose host rendering and host
  charset do not covary the way these three do would need the dials kept
  separate (they now are). The open question ADR 0009 left — whether `rfc3986`
  should admit ANY `pct-encoded` octet in a `reg-name`, including `%2F` — is
  still open and still deliberately unimplemented; it was measured at +710
  accepted rows and backed out (S7-F4).

## Relationship to ADR 0009

ADR 0009 is **superseded, not reversed**. Its finding stands: WHATWG keeps 15
ASCII code points in a host that libcurl rejects, none of them forbidden or
structural, and rurl must accept them under `whatwg`. All 15 still parse, both
spellings, and the `host-charset-shimmed` diagnostic still fires. What changed
is *where the rule lives*: ADR 0009's mechanism was the only one available
against a third-party parser, and it expired when that parser did.
