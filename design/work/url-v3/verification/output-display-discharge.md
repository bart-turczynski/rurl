# Verification — safe display (surface d) discharge (VD-003)

<!-- Verification artifact, discharge record. NARROW BY CONSTRUCTION: this file
     discharges exactly one deferral, VD-003, by mapping the display cells it
     deferred onto executable evidence that now ships. It is NOT the full-string
     family's verification slice — that slice, the artifact-11 map for
     contracts/output-contracts.md, remains a later G4 leaf — and it is not a
     second bite at VD-002's surface-(b) cells, which output-fsss-discharge.md
     already covers. Surfaces (a) and (c) are untouched here.

     Why this record exists at all: P0.5 failure condition 3 fires the moment a
     deferred surface ships. VD-003's surface_probe is `export:format_url`, and
     that export now resolves in NAMESPACE, so the row cannot stay ACCEPTED; and
     a DISCHARGED row must be claimed by a registered claimant (deferral-gate
     rule D2, reading its registry out of traceability-map.md) or the gate fails
     closed. This record is that claim, and nothing more. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-output-display-discharge |
| name | verification-output-display-discharge |
| artifact_number | 11 (discharge record; the full-string family slice proper is a later G4 leaf) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/output-display-discharge.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| verifies | VD-003 (registers/verification-deferrals.md); P2.2 and P2.7 (authoritative decisions); contracts/output-contracts.md — the safe-display (surface d) cells only |
| dependencies | P0.5 (the deferral register and its gate); P2.2 (the surface + its §5.5 credential guard); P2.7 D-D (the escape/redaction matrix and the public entry point); contracts/output-contracts.md (the normative source) |
| closes_finding | VD-003 |
| completion_rule | every cell named by VD-003 maps to shipped executable evidence, cited as `path :: test name`, with the evidence present in the tree rather than promised; `tools/deferral-gate.R` reports D2 and D3 PASS |
| approval_evidence | pending — rides the owner's merge of the PR carrying it |
| validation_command | Rscript tools/deferral-gate.R && devtools::test() |

## DISCHARGED[VD-003]

VD-003 deferred the display cells on an explicit, checkable premise — in the
register's own words, that **"output surface (d), the safe human-facing
formatter, is unbuilt"**.

That premise has expired. `format_url()` ships with this slice, so the
deferral's `surface_probe` (`export:format_url`) resolves present in `NAMESPACE`
— precisely the condition P0.5 failure condition 3 names. The register's rule is
that a row dies when its surface arrives, not when a date passes. The cells are
no longer excused, they are **covered**.

**The second half of VD-003's sentence was a different fact, and it expired
separately.** The row also says the "escape/redaction matrix detail is
separately OPEN as OUT-O4". P2.7 D-D *decides* that matrix, and P2.7 §7
projects the resulting cell moves onto `contracts/output-contracts.md` (the
surface-(d) row of §Five output surfaces, the `scope + escape/annotation
matrix` row of §Safe display, the `resolve_url()` coupling row of §Clean
output, and the OUT-O4 bullet of `## Open cells`). Those moves landed under
`RURL-irfmmoer`, a separate slice from this one: `OUT-O4` is CLOSED in the
contract and the `format_url` roster row in
`contracts/public-surface-disposition.md` reads SETTLED. The two slices were
kept apart on purpose — this record excused and then evidenced *behavior*; the
other rewrote the contract text to match the decision that governs it — and
neither claims the other's work.

## Cell → evidence map

All evidence is in one file, `tests/testthat/test-format-url.R`, and runs in the
standard `devtools::test()` chain. The file is organized by rule (E1…E5) with
S3-F3's five worked examples first and by name, because P2.7 §7 names those five
as *the* acceptance criteria D-D must be tested against — so the acceptance
criteria are locatable without reading the whole file.

| VD-003 cell | Evidence | Coverage |
|---|---|---|
| `SURF-d` — surface (d) exists as a public surface, distinct from every other | `test-format-url.R :: "the display surface is not the serialization surface"`; `:: "the vector contract matches the sibling full-string surface"` | the export exists and is demonstrably a *different product* from surface (b) on one input, while its vector/NA/factor/names contract is asserted to match its sibling — distinct in output, identical in shape, which is what "a fifth surface, not a variant" means |
| `DISP-1` — a human-readable rendering with hazard handling: controls, bidi overrides, invisibles, spoof-safe host display | `:: "S3-F3 example 3: U+202E, LF and NUL get exact visible outputs"`; `:: "E2a escapes C0, DEL, C1 and private-use code points"`; `:: "E2b escapes every bidi formatting character D-D names"`; `:: "E2b escapes the marks, joiners and invisibles D-D names"`; `:: "E1 escapes a literal angle bracket and keeps an encoded one"`; `:: "every angle bracket in the output was written by the formatter"`; `:: "S3-F3 example 5: both host spellings are shown when they differ"`; `:: "a punycode label decodes into the display spelling"`; `:: "IP literals are shown verbatim and never annotated"` | each hazard class D-D enumerates, asserted on the exact output bytes rather than on "is escaped". The angle-bracket corpus sweep is the one that makes the `<U+XXXX>`/`<redacted>` tokens *unambiguous* rather than merely present: it strips the formatter's own tokens and fails if any bracket survives |
| `DISP-2` — `format_url()` redacts credentials (the §5.5 secret-in-output guard) | `:: "userinfo is redacted whenever any was present, including a bare @"`; `:: "the redaction token is fixed-width and leaks no length"`; `:: "no userinfo emits no token and no delimiter"` | redaction is asserted on the *presence* fact, not on a non-empty username: `u:p@`, `u@`, `:p@` and a bare `@` all collapse to the same token. The fixed-width test is the one that closes the side channel — a token that grew with the secret would satisfy "redacts" and still leak its length |
| `CAP-4` — the capability row for `(d)`: not reparsable, not standards-valid, not identity- or routing-safe, **display-only** | `:: "S3-F3 example 1: /a%2Fb does not display as /a/b"`; `:: "S3-F3 example 2: ?x=a%26b%3Dc does not display as ?x=a&b=c"`; `:: "E4 keeps encoded grammar delimiters encoded in every component"`; `:: "E4 adds & = + to the delimiter set inside the query only"`; `:: "E4 preserves a percent sign so decoding happens exactly once"`; `:: "the display surface is not the serialization surface"`; `:: "format_url takes no presentation dial"` | E4 is what makes "display-only" a property of the *bytes* rather than a warning in a man page: a decoded `%2F` would fabricate a path segment the URL does not have, so the output would be a plausible-looking URL that resolves elsewhere. Keeping it encoded is simultaneously the hazard fix and the proof that the result is not an address |
| redaction leg of `CRED-3` — credentials are reproduced only by (a)/(b), dropped by (c), **redacted by (d)** | `:: "userinfo is redacted whenever any was present, including a bare @"`; `:: "the display surface is not the serialization surface"` | the (d) leg only. That the same input serializes with its credentials intact under `serialize_url()` and redacted under `format_url()` is asserted as one comparison, so the per-surface split cannot regress into a global policy in either direction |
| redaction leg of `INV-5` — the credential-output policy governs **all** preserved credentials, for web AND generic-authority credentials alike | `:: "a generic-authority scheme's credentials are redacted too"` | asserted on `foo://` and `git+ssh://` specifically. A rule reached only through the special-scheme branch would pass every other test in the file and leak the secret for exactly the schemes nobody writes a fixture for — which is why this row's evidence is a separate test rather than another `https://` case |

## Boundary: what this record does NOT claim

- It does **not** verify the rest of `contracts/output-contracts.md`. Only the
  cells VD-003 enumerated are discharged. Surface (a) source reproduction and
  surface (c) clean output keep whatever status they already had, and `OUT-O1`
  and `OUT-O5` remain open.
- It did **not** close `OUT-O4`. The matrix is *decided* (P2.7 D-D) and
  *implemented and tested* here; the contract cells P2.7 §7 projects were moved
  by `RURL-irfmmoer`, and that is what closed the cell. This record evidences the
  behavior, it does not rewrite the contract.
- It does **not** claim the display rule is verified against a Unicode *version*
  sweep. It is verified to be **version-independent by construction**: the E2
  blocks are static range matrices, and `:: "the escape decision performs no
  runtime category lookup"` deparses the namespace objects and fails if a general
  category ever appears in the escape path. The four E2c drift probes (U+2065,
  U+FFF8, U+110BD, U+E0001) are the non-vacuous complement — an implementation
  spelled "escape Cc/Cf/Cs/Co/Cn" passes every other test in this file and fails
  only that one.
- It makes **no product decision**. P2.2 remains the single writer of the output
  surfaces and P2.7 of this one's escape/redaction matrix, entry point and parse
  posture; this record only records where they are executably verified.

## Inputs

| Path | Role |
|---|---|
| design/work/url-v3/registers/verification-deferrals.md | the register carrying VD-003 |
| design/work/url-v3/decisions/P0.5-g4-exit-criterion-scope.md | the disposition defining discharge |
| design/work/url-v3/decisions/P2.2-serializer-clean-output.md | the bound decision (the surface and its §5.5 credential guard) |
| design/work/url-v3/decisions/P2.7-display-and-resolver-output.md | the bound decision (D-D: the escape/redaction matrix, the entry point, the parse posture) |
| design/work/url-v3/contracts/output-contracts.md | the normative source of the discharged cells |
| tests/testthat/test-format-url.R | the cited executable evidence |
