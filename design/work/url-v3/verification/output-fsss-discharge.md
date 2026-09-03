# Verification — FSSS (surface b) discharge (VD-002)

<!-- Verification artifact, discharge record. NARROW BY CONSTRUCTION: this file
     discharges exactly one deferral, VD-002, by mapping the full-string cells it
     deferred onto executable evidence that now ships. It is NOT the full-string
     family's verification slice; that slice — the artifact-11 map for
     contracts/output-contracts.md — remains a later G4 leaf, and surfaces (a),
     (c) and (d) are untouched here. Authoring the whole family slice to close one
     row would manufacture G4 scope, which is the failure cache-slice.md's
     envelope warns against.

     Why this record exists at all: P0.5 failure condition 3 fires the moment a
     deferred surface ships. VD-002's surface_probe is `export:serialize_url`, and
     that export now resolves in NAMESPACE, so the row cannot stay ACCEPTED; and a
     DISCHARGED row must be claimed by a verification slice (deferral-gate rule
     D2) or the gate fails. This record is that claim, and nothing more. -->

## Envelope

| Field | Value |
|---|---|
| id | verification-output-fsss-discharge |
| name | verification-output-fsss-discharge |
| artifact_number | 11 (discharge record; the full-string family slice proper is a later G4 leaf) |
| schema_version | 1.0.0 |
| tracked_location | design/work/url-v3/verification/output-fsss-discharge.md |
| owner | Bart Turczynski <bartek@turczynski.pl> |
| single_writer | repository owner (sole); P0.3 §5 |
| lifecycle_state | PROPOSED |
| verifies | VD-002 (registers/verification-deferrals.md); P2.2 and P2.5 (authoritative decisions); contracts/output-contracts.md — the FSSS (surface b) cells only |
| dependencies | P0.5 (the deferral register and its gate); P2.2 (the surface + contract); P2.5 (the public entry point, PROPOSED with this slice); P5.3 (the claim/oracle policy the idempotence oracle serves); contracts/output-contracts.md (the normative source) |
| closes_finding | VD-002 |
| completion_rule | every cell named by VD-002 maps to shipped executable evidence, cited as `path :: test name`, with the evidence present in the tree rather than promised; `tools/deferral-gate.R` reports D2 and D3 PASS |
| approval_evidence | pending — rides the owner's merge of the PR carrying it |
| validation_command | Rscript tools/deferral-gate.R && devtools::test() |

## DISCHARGED[VD-002]

VD-002 deferred the full-string cells on an explicit, checkable premise — in the
register's own words, that **"output surface (b) … is unbuilt"** and that
consequently **"parse→serialize→parse … cannot be exercised at all"**.

Both halves of that premise have expired. The serializers shipped in `df00da8`
(`.serialize_whatwg_full_vec` / `.serialize_rfc_full_vec`, `R/parse-phases.R`),
and the public entry point `serialize_url()` shipped with this slice, so the
deferral's `surface_probe` (`export:serialize_url`) resolves present in
`NAMESPACE` — precisely the condition P0.5 failure condition 3 names. The
parse→serialize→parse oracle, which the G4 audit recorded as having **zero
instances** anywhere in the suite, is now asserted at both levels.

The register's rule is that a row dies when its surface arrives, not when a date
passes. This record is that death certificate: the cells are no longer excused,
they are **covered**.

## Cell → evidence map

Evidence lives in two files, deliberately split by what can be wrong at each
level. `test-serialize-fsss.R` exercises the two serializers against hand-built
records — it can catch a serializer bug but not a record-build bug.
`test-serialize-url.R` exercises the export end-to-end — it is the only thing
that can catch a record built from the wrong parse or from a cleaned component.
Both run in the standard `devtools::test()` chain.

| VD-002 cell | Evidence | Coverage |
|---|---|---|
| `SURF-b` — surface (b) exists as a public surface | `test-serialize-url.R :: "serialize_url() emits the full string, credentials and fragment"`; `:: "serialize_url() is not clean_url"` | the export exists and is demonstrably a *different product* from surface (c) on one input — (b) keeps credentials, fragment and the ASCII host; (c) drops the first two and keeps the Unicode host |
| `FSSS-1` — pair of spec-exact serializers | `test-serialize-fsss.R :: "the WHATWG FSSS emits credentials and the fragment"`; `:: "the source posture normalizes nothing"`; `:: "the normalized posture applies section 6.2.2 and 6.2.3"` | both standards, each against its own specification rather than against the other |
| `FSSS-2` (fragment leg) / `REC-4` (fragment leg) — full-string, fragment-complete, empty delimiter preserved | `test-serialize-fsss.R :: "a present-but-empty query or fragment delimiter survives"`; `:: "an absent query or fragment emits no delimiter"`; `:: "the fragment is percent-encoded with the fragment set"`; `:: "the RFC serializer preserves an empty query or fragment delimiter"`; `test-serialize-url.R :: "a present-but-empty query or fragment delimiter survives"` | ADR 0012 D2's three-valued presence, asserted at BOTH levels on purpose: Stage A collapses `""` to `NA`, so the public-level assertion is the one that can fail when the serializer is right and the record build is wrong |
| `FSSS-3` — parse → serialize → parse idempotence (the P5.3 claim substrate) | `test-serialize-fsss.R :: "WHATWG parse -> serialize -> parse is idempotent"`; `:: "RFC parse -> serialize -> parse is idempotent"`; `:: "the RFC source posture reproduces its input for this corpus"`; `test-serialize-url.R :: "serialize_url() is idempotent on both standards"`; `:: "the RFC source form reproduces an already-conformant input"` | the audit's zero-instance gap, closed at both levels. Byte reproduction is asserted as its OWN test rather than folded into idempotence, so a future normalization change fails on the claim it actually breaks |
| `FSSS-7` / `CAP-2` — WHATWG credential spec-exactness | `test-serialize-fsss.R :: "WHATWG drops a userinfo whose halves are both empty"`; `:: "WHATWG drops an empty password but keeps its username"`; `:: "WHATWG keeps a password with an empty username"`; `:: "the userinfo split is at the FIRST colon only"`; `:: "credentials are percent-encoded with the userinfo set"`; `:: "the RFC serializer keeps the undivided userinfo in both postures"`; `test-serialize-url.R :: "credential serialization is spec-exact per standard"` | the WPT-pinned drops (`http://@h/` → `http://h/`, `http://u:@h/` → `http://u@h/`) and, next to them, the RFC posture where every spelling survives verbatim — the two together are what makes "the losslessness lives in the record" a checkable claim rather than an excuse |
| `INV-2` — independence from presentation (C-05) | `test-serialize-fsss.R :: "the FSSS takes no presentation dial"`; `:: "the identity port is emitted even when it is the scheme default"`; `:: "a lone trailing slash is never stripped"`; `test-serialize-url.R :: "serialize_url() takes no presentation argument"`; `:: "a default port is elided or kept per the standard, not per a dial"` | asserted structurally, not by example: both tests read `formals()` and fail if any presentation dial ever appears in a signature. C-05 cannot regress silently, at either level |

## Boundary: what this record does NOT claim

- It does **not** verify the rest of `contracts/output-contracts.md`. Only the
  cells VD-002 enumerated are discharged. Surface (a) source reproduction,
  surface (c) clean output and surface (d) safe display keep whatever status they
  already had, and `OUT-O1` and `OUT-O5` remain open (`OUT-O4` closed later, under `RURL-irfmmoer`).
- It does **not** discharge `VD-003`, the sibling row covering surface (d) and
  `format_url`. That surface is still unshipped, so VD-003 stays `ACCEPTED` and
  D3 correctly stays silent about it.
- It does **not** claim that rurl's conformance *evidence* now rides surface (b).
  It does not: every conformance and benchmark harness in the repo still scores
  against `clean_url`, which P5.3 CLAIM-1 does not admit as a claim substrate.
  This record makes an admissible substrate exist; re-pointing the evidence onto
  it is `RURL-yeikpnan`, and it is deliberately **not** claimed here.
- It makes **no product decision**. P2.2 remains the single writer of the output
  surfaces and P2.5 of this one's public spelling; this record only records where
  they are executably verified.

## Inputs

| Path | Role |
|---|---|
| design/work/url-v3/registers/verification-deferrals.md | the register carrying VD-002 |
| design/work/url-v3/decisions/P0.5-g4-exit-criterion-scope.md | the disposition defining discharge |
| design/work/url-v3/decisions/P2.2-serializer-clean-output.md | the bound decision (the surface and its contract) |
| design/work/url-v3/decisions/P2.5-standard-serializer-surface.md | the bound decision (the public entry point) |
| design/work/url-v3/contracts/output-contracts.md | the normative source of the discharged cells |
| tests/testthat/test-serialize-fsss.R | the cited executable evidence (serializer level) |
| tests/testthat/test-serialize-url.R | the cited executable evidence (public level) |
