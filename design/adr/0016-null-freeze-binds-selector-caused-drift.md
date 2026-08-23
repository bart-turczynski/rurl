# ADR 0016: the `NULL` freeze binds selector-caused drift, not default-path defects

- **Status:** Accepted
- **Date:** 2026-08-23
- **Tracking:** RURL-bmxptxxz (this decision). Bounds **ADR 0007** §Consequences
  on a second, different axis from [ADR 0015](0015-require-url-standard-on-companion-helpers.md);
  relates to **ADR 0011** and **ADR 0010**. Its first witness is RURL-eikgtrqf;
  its predicted next instance is RURL-dxwsksor.

All code and test citations below were measured on `ab97416` and are dated, not
live (see the citation convention in ADR 0014's wake): they record what was true
when the ruling was made.

## Context

[ADR 0007](0007-url-standard-selector.md) §Consequences promises that
`url_standard = NULL` stays **byte-for-byte** compatible. It states no boundary,
and the promise is permanent under the 3.0.0 freeze. So the boundary is
permanent too, and leaving it unstated has already cost a slice.

**The witness.** RURL-eikgtrqf shipped with an acceptance criterion of *"0
changed rows under `url_standard = NULL`"*. Measured during that fix: 9 rows
changed, **3 under `NULL`**, all root-dot hosts under
`host_encoding = "unicode"`. Zero changed `NULL` rows would have proven the fix
never reached the default path. Taken literally, the criterion demanded the bug
survive.

**The tempting discriminator, and why it is wrong.** The obvious repair is to
say the freeze binds only the axes the selector *governs* — the keys of
`.URL_STANDARD_PROFILES` (`R/parse.R:1054`), which are exactly
`path_identity`, `path_normalization` and `case_handling` — and that a defect on
any other axis "manifests identically under all three selector values", so
fixing it necessarily moves `NULL` rows and cannot be a freeze violation.

Both halves of that are false, and the tree falsifies them:

- **Ungoverned knobs are not selector-independent.**
  `.apply_host_encoding_vec()` takes `url_standard` as a parameter
  (`R/parse-phases.R:1990-1991`), percent-decodes the host only under
  `"rfc3986"` (`R/parse-phases.R:2014`), and takes a different IDNA path under
  WHATWG (`R/parse-phases.R:2019-2020`). `path_encoding = "encode"` selects a
  different encoder when the selector-derived identity is `.whatwg_preserve`
  (`R/parse-phases.R:1210`). `port_handling` is outside the table and its output
  policy is a no-op unless the selector is WHATWG
  (`R/parse-phases.R:2189`). `scheme_acceptance = "general"` is an error
  *without* a selector (`R/parse.R:1536`).
- **"Manifests identically under all three arms" is false in shipped code.**
  RURL-ogruzocw's index-page fix is an ungoverned-axis fix whose new branch both
  named profiles never reach, because their `path_normalization = "dot_segments"`
  resolves the dots at an earlier step — asserted as such in
  `tests/testthat/test-index-page-strip-dot-segments.R:95`.

`.URL_STANDARD_PROFILES` answers one narrow question: **does the selector own
this knob's value and reject a conflicting explicit one?** That is conflict-matrix
ownership (`.check_url_standard_conflicts()`, `R/parse.R:1355`). It is not a
statement about behavioral independence, and it is not a licence to move `NULL`.

**The asymmetry that hid this.** ADR 0011 cites ADR 0007; ADR 0007 has never
cited ADR 0011. The governed side of the rule is discoverable by tripping over
an error message; the ungoverned side was discoverable nowhere, so the wrong
version of it was reconstructed from scratch and written into a test file's
justification comment.

## Decision

**ADR 0007's `NULL` freeze prohibits drift *caused by* introducing, extending or
conforming the `url_standard` selector. It does not require a pre-existing
defect in the default parse path to survive.**

The discriminator is causation, not the axis a knob sits on:

- A change whose *reason* is a standard's rule — importing a WHATWG behavior,
  tightening a production to RFC 3986's own grammar, closing a conformance gap —
  is selector-caused. It must stay unreachable from `NULL`. ADR 0007's
  Appendix-B carve-out is the model: the loose scheme group is retained under
  `NULL` deliberately, because the freeze governs whether output may *move*, not
  whether it is right.
- A change whose *reason* is that the default path itself is broken — it emits
  something no profile, standard or documented contract sanctions — is a defect
  fix. It may move `NULL`, and an acceptance criterion must not forbid it.

**A `NULL`-moving fix carries its own evidence.** Two things, both of which
belong in the ordinary test file for the fix — this creates no record lifecycle,
no seal and no separate artifact (ADR 0014 retired all three, and this ADR does
not reintroduce them):

1. **A witness that the defect is in the `NULL` path.** A pre-fix probe under
   `url_standard = NULL` exhibiting the defect, kept afterwards as the
   regression test. Omitting the argument and passing `NULL` explicitly must
   agree.
2. **A declared signature the fix stays inside.** The inputs, option
   combination and output fields the fix is allowed to move, asserted so that
   nothing outside it changes. Do **not** assume the named-profile arms behave
   like `NULL`: state, per arm, whether the arm was already correct, and why.
   "The axis is outside `.URL_STANDARD_PROFILES`" is never the justification.

Stronger instruments — a frozen pre-fix fixture diffed against current output,
or a mutation that disables the fix and proves the regression test goes red —
are available and encouraged where the blast radius is wide. They are not
required.

**What `.URL_STANDARD_PROFILES` does determine.** For a direct selector call, a
knob named in the table is selector-owned: omitting it selects the profile's
value, supplying that same value explicitly is accepted, and supplying a
different value is an error (`R/parse.R:1355`;
`tests/testthat/test-url-standard-case-handling.R:24` pins the accepted case).
A named `profile` may authorize its own bundle, and explicit arguments still
override it (ADR 0012 D6). None of that speaks to `NULL` compatibility.

## Consequences

- **RURL-eikgtrqf's criterion was unsatisfiable for the right reason, stated
  wrongly.** The fix was legitimate because the defect was in the `NULL` path —
  not because `host_encoding` is outside the governed table. Same verdict,
  sound premise.
- **A shipped justification comment was corrected.**
  `tests/testthat/test-index-page-strip-dot-segments.R` argued the membership
  test in its header while its own line 95 test disproved it. The fix it
  defends stands; the reason is restated as a default-path defect.
- **The membership set is pinned.** `test-url-standard-scaffold.R` asserts the
  key set of `.URL_STANDARD_PROFILES` against the three names this ADR
  documents, so adding a fourth governed knob makes this prose fail rather than
  rot. That pin guards documentation, **not** `NULL` drift — the guard for drift
  is the evidence requirement above.
- **Governed-axis defects are not protected forever.** A genuine bug in the
  default path's casing behavior would be fixable under this rule even though
  `case_handling` is a governed knob. Knob ownership and "is there a bug here"
  are different questions, and conflating them is what this ADR exists to stop.
- **Standing rule: no fix moves `NULL` on the strength of an axis label.** The
  claim to make, and to review, is "the default path is defective, here is the
  witness, here is the signature". A reviewer who sees an axis-membership
  argument for `NULL` drift should reject it and ask for the witness.
- **What would justify revisiting.** A future ADR that makes some axis formally
  selector-independent — proven, not asserted — could narrow the evidence
  requirement for that axis. Nothing in the tree supports such a claim today.
