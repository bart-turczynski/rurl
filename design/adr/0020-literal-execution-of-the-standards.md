# ADR 0020: rurl executes the standards literally, and the default is WHATWG

- **Status:** Accepted
- **Date:** 2026-10-26
- **Tracking:** RURL-njwxywsw (this decision). Supersedes **ADR 0015** and
  **ADR 0016** whole, and in part **ADR 0007** (§Consequences "No default flip"
  and the `NULL` byte freeze) and **ADR 0011** (the clause that `encode` cannot
  be redefined). Each carries an appended note. Follow-ups: RURL-mbhuhwzk
  (the default flip), RURL-ongkcidw (the audit), RURL-dbyavcjt (`encode`),
  RURL-eokizmdi (`decode`).

## Context

The owner ruled on 2026-10-26 that rurl's goal is the literal execution of the
standards it names: the WHATWG URL Standard, RFC 3986, and the scheme RFCs
(RFC 6068 for `mailto:`, RFC 8089 for `file:`). Where a reading is in doubt,
the stricter reading of the text wins.

The earlier ADRs were written around what the code could do at the time. Several
of them protect output that a standard calls invalid, or that names a different
resource from the input:

- **ADR 0007** kept `url_standard = NULL` as the default and froze its output
  byte for byte, "whether it is right" or not (its P2.7 D-B amendment). The
  posture card calls it "the byte-frozen pre-selector profile" that "names no
  standard".
- **ADR 0016** kept every change whose reason is a standard's rule unreachable
  from `NULL`.
- **ADR 0011** pinned `path_encoding = "encode"` folding `%2F` into a
  path-separating `/`, because "`encode` cannot be redefined (byte-compat is a
  hard constraint)". RFC 3986 §2.2 makes a reserved character and its
  percent-encoded octet non-equivalent.

AGENTS.md already said that conformant behavior is the default and that a
shipped behavior a standard calls invalid is a bug. ADR 0007 and ADR 0016 cut
that rule off at the default arm.

**How far the default is from either standard.** Measured on `24addbe` over
the 441 unique inputs of `tests/testthat/fixtures/parse-corpus.csv` and
`external-url-vectors.csv`, comparing the `safe_parse_urls()` columns
`parse_status`, `scheme`, `host`, `port`, `path`, `query` and `fragment`:

| `url_standard = NULL` against | rows that differ |
|---|---|
| `"whatwg"` | 93 |
| `"rfc3986"` | 200 |
| both | 68 |

So 68 inputs (15%) get an answer from the default that neither named arm gives
(both arms measured under the default `scheme_policy` and `scheme_acceptance`).

**A per-axis mix is not a standard.** One way out was to keep `NULL` as rurl's
own dialect, unfrozen, and to require only that each of its behaviors be
sanctioned by some standard. That fails. A mode that follows WHATWG on one axis
and RFC 3986 on another can give a whole result that conforms to neither, so it
cannot be the literal execution of anything.

## Decision

**D1: the standards outrank every ADR.** A behavior that a selected standard
calls invalid, or that names a different URL than the standard's processing of
the same input, is a bug. That holds when an ADR, a ruling, a test or the
`NULL` freeze pins it. Fix it, and cite the standard and clause in `NEWS.md`.
An ADR that pins such a behavior is superseded on that point by a note
appended to it, plus its Status line; its body is never edited, so line
citations hold. A ruling is superseded by a new row in
`design/work/url-v3/registers/rulings.md` that names it.

**D2: compatibility protects the API, not output.** What stays protected is
gratuitous API churn: renaming or removing an argument, a function, an enum
value or a column without cause. A change of output that makes rurl conform is
never held back for compatibility, in a minor release as much as in a major.
NEWS names every such change.

**D3: the default `url_standard` is `"whatwg"`.** rurl's users expect the URL
behavior they meet on the web every day, and the WHATWG URL Standard is the one
that defines it operationally, parse and serialization both. ADR 0017 already
defines `clean_url` as a projection of a WHATWG-parsed URL. This ships in the
next release, not held for 4.0.0.

**D4: `NULL` stays accepted and means `"whatwg"`.** Wherever `url_standard` is
a parameter, `NULL` is read as `"whatwg"`. Removing the value would be churn D2
forbids. The frozen pre-selector profile is retired: no code path keeps it, and
no test pins it. The three companion helpers of ADR 0015 (`get_host_type()`,
`get_url_diagnostics()`, `get_scheme_class()`) default to `"whatwg"` as well:
ADR 0015 required the argument because `NULL` could not answer, and `NULL` now
answers as WHATWG. A function that already defaults to a named standard
(`check_hosts()`, `is_valid_host()`: `"whatwg"`; `get_mailto_recipients()`:
`"rfc3986"`) keeps its default under this ADR, and there too `NULL` means
`"whatwg"`: `get_mailto_recipients(x, url_standard = NULL)` reads as WHATWG
while `get_mailto_recipients(x)` reads as RFC 3986.

**D4a: a governed knob under the default.** `path_normalization` and
`case_handling` are governed: today an explicit value that differs from the
selector's is an error, and dropping `url_standard` (falling back to `NULL`) was
the way out. That way out goes. Under the default `"whatwg"`, an explicit
governed knob may still shape `clean_url`, which is cleaning policy (ADR 0017),
but it never changes the parse record, the components or a serialization: those
stay WHATWG. Where a knob value cannot be confined to `clean_url`, the conflict
error stands and names `url_standard = "rfc3986"` (or `profile = "rfc-syntax"`)
when that arm offers the value. RURL-mbhuhwzk decides each value.

**D5: presentation and policy dials still answer to the standard.** `clean_url`
stays a lossy policy projection (ADR 0017): a documented cleaning transform is
not a conformance defect. A documented display form (`host_encoding =
"unicode"`, RUL-031) is not one either: it is labeled as a rendering for people
and need not re-parse. Any other dial that changes a component accessor, a
`safe_parse_urls()` column or a serialization is held to the standard, and so is
a dial that claims a standard's form and does not give it. The standing example
is `path_encoding = "encode"`, which claims the percent-encoded form and folds
`%2F` into a path-separating `/`. Whether `path_encoding = "decode"` is a
display form that may do the same is RURL-eokizmdi's question.

## Consequences

- **The default output moves.** About one input in five of the fixture corpora
  parses differently by default once D3 lands. RURL-mbhuhwzk implements it,
  with NEWS listing the visible moves (scheme-less input, host handling,
  percent-encoding, reference resolution).
- **Rules retired with the freeze.** "No fix moves `NULL` on the strength of an
  axis label" (ADR 0016) and the witness-plus-signature requirement no longer
  apply: there is no frozen arm to protect. The Appendix B scheme group in
  `.split_uri_ref()`, `NULL`'s use of the RFC 8089 `file:` overlay
  (`ascii_only = FALSE`) and the other `NULL`-only behaviors in ARCHITECTURE.md
  go with it.
- **Other default dials are audited, not assumed.** `scheme_policy`,
  `scheme_acceptance` (the ADR 0004 five-scheme allowlist) and
  `scheme_relative_handling` can make the default posture differ from the
  WHATWG parser even with `url_standard = "whatwg"`. RURL-ongkcidw decides each
  against D1.
- **Rulings stay rows.** RURL-mbhuhwzk adds the row that supersedes the
  rulings resting on the `NULL` freeze (RUL-014, RUL-015, RUL-026, RUL-029,
  RUL-030 at least) on their `NULL` clauses. Until it lands they describe the
  tree as it is.
- **What would justify revisiting.** A standard that leaves the behavior open
  (a "may", or a case it does not cover) is where an OWNER ruling still
  decides, as before. Nothing here makes a cleaning policy conformant or
  non-conformant by itself.
