# Measurement traps

How a green test, gate or harness returns a plausible **wrong** number.

Every trap recorded here produced a number, not an error. A green measurement
answers only the question its instrument can ask, and `0 differing rows` prints
identically to *the instrument cannot reach the seam*. That is why the entries
below are worth a document: none of them is visible in the output that reports
them.

**Scope.** Per-seam rules live in the ADRs and per-instrument rules in the
script headers — read
[`tools/octet-acceptance-sweep.R`](../tools/octet-acceptance-sweep.R) and
[`tools/oracle/README.md`](../tools/oracle/README.md) before extending either.
This file links to those rather than restating them; the pointer table in §8
records what is already documented elsewhere.

## 1. The one rule: prove the instrument red

Run the **finished** instrument at a baseline where the defect is present and
confirm it fails there. An instrument that has never gone red has not been
shown to work, and its zero is not evidence.

- Prefer a real pre-existing failure over a synthetic mutant. Running a finished
  gate against unmodified bytes has produced a genuine finding more than once,
  before any mutation was written.
- Apply any mutant to unmodified `main` as well. Otherwise you have shown the
  test passes, not that it bites.
- When several instruments run, record the **spread**, not just the totals. A
  change that moves 164 octet-sweep rows and 0 rows in two other sweeps has
  established which corpora reach the seam — and only then does a later 0 from
  one of the other two carry information.

**Corollary.** An instrument that cannot show a signal returns *absent* for both
"absent" and "unobservable". Nothing downstream can tell those apart, so the
distinction has to be established before the measurement, never after it.

## 2. What a corpus cannot reach

- **Conjunctions.** A corpus that varies one thing at a time cannot reach code
  guarded by a conjunction — a bad octet **and** a co-occurring sub-delim,
  percent-triplet or repeated `@`. Build the falsifying corpus on the *opposite*
  construction principle to the fix's own: hold the payload fixed and vary the
  co-trigger. A fix's author who also builds its corpus gives the corpus the
  same blind spots as the fix.
- **Spellings.** A corpus holding a code point only raw cannot see its encoded
  spelling: `%60` and a literal backtick are one code point decided by different
  rules, and `%C3%A9` needs a *second* triplet to be legal at all, so a
  single-triplet corpus cannot spell a non-ASCII host. Enumerate an input's
  spellings, not just its bytes.
- **The frame.** Already a standing rule —
  [ADR 0013](adr/0013-host-charset-acceptance-in-the-parser.md) records it and
  the evidence behind it. Do not restate it here.
- **The payload.** Blindness is not only in the frame. Every block of the octet
  corpus holds the host *token* non-numeric, so a rule firing on "the host ends
  in a number" is invisible to all 5562 inputs. Ask what the corpus holds
  constant at **both** levels, then extend the existing `lit_*` (literal byte ×
  frame) or `ip4_*` (address-grammar token × frame) blocks rather than inventing
  a corpus alongside them.

## 3. What an instrument declines to record

- **The moved column.** A sweep can falsify only what it records; a sweep
  missing the column returns a confident, partly-truthful number. Extend the
  columns **first**, rebuild the baseline with the **new** script — each sweep
  loads the package from its `<pkg-dir>` argument, so a new script scores an old
  tree fine — and add the identity guards in the same pass. The sweep's own
  header states which columns exist for which change and why.
- **Decode surfaces.** A raw-byte sweep cannot falsify a claim about decode
  surfaces. A `%0D%0A` input returns through `get_query()` (`decode = TRUE` is
  the default), through `get_query(format = "list")` and
  `query_param_summary()$example_value`, and through anything carrying
  `path_encoding = "decode"` — including `get_clean_url()`, which then yields a
  whole URL string with an embedded CRLF. The identity-preserving accessors are
  all clean, which is exactly what makes a raw-byte sweep look conclusive. When
  asking whether component X can hold byte B, vary the **spelling** of B *and*
  enumerate every accessor **argument** that decodes.
- **Consumer argument sets.** When re-deriving a recorded fixture cell, use the
  arguments the consuming test actually passes, not the ones more "correct" for
  the conformance question. Re-deriving `external-url-vectors.csv` cells with
  `scheme_acceptance = "general"` — normally the right choice — against cells
  recorded without it produced about ten false `moved` rows on empty-authority
  inputs.

## 4. Harness mechanics that lie

Four mechanical failures, each of which has produced a discarded run. The sweep
header carries the rationale for the first two.

1. **Never round-trip a corpus through `.rds`.** Under `LC_ALL=C`, `readRDS()`
   re-encodes `"unknown"`-marked non-ASCII strings on load, so the two locale
   runs are fed different inputs and the harness reports false agreement. Build
   the corpus from raw octets in-process and write results as plain text.
2. **Never wrap a whole vectorized call in one `tryCatch()`.** One throwing row
   collapses the column into a single sentinel, hiding every other row's value —
   and the comparison then reports 100% of rows changed, which reads as a
   catastrophe and means nothing. Fall back to one call per row.
3. **Never write corpus *strings* to a line-oriented diff file.** Raw CR/LF in
   the population splits rows and mis-columns everything downstream, which is
   how a comparison reported a confident `newly accepted: 0` while its own
   totals plainly differed. Write a positional mask — one character per row —
   plus a `digest::digest()` of the population, so both trees can be shown to
   enumerate it identically.
4. **Compare bytes *plus* `Encoding()` marks.** A transcoding regression moves
   neither the value nor the length, only the mark.

## 5. How gates die quietly

- **Discovery goes vacuous.** A gate has two halves: which text it treats as a
  claim (discovery) and what it asserts about that claim (the rule). The rule
  gets tested; discovery usually does not, and a reword, a backtick or a
  markdown reflow silently empties the claim set while the gate prints green. A
  gate that finds no claims and a gate whose claims all hold produce the same
  output. Key discovery on **meaning**: normalize first, then require a loose
  core phrase plus a corroborating token. Read paragraphs rather than lines
  wherever a count and its noun can wrap apart, but treat a table row as its own
  paragraph. Separate *if stated it must be right* from *it must be stated* —
  the first alone is dodged by deleting the number, the second alone
  false-positives on legitimate prose. Then run the **finished** gate against
  pre-fix bytes and count findings **per rule**; a rule contributing zero
  findings at baseline either had nothing to catch or cannot see, and which one
  is a decision to make out loud.
- **The environment rewrites the watched text.** `AGENTS.md` documents the
  `watch = <regex>` mechanism; what makes a watch pattern non-vacuous is where
  its bytes came from. Under `LC_ALL=C` — the locale the test step *forces* —
  cli and testthat degrade `══ Warnings` to plain `== Warnings`, so a pattern
  written from the output visible on a dev machine is vacuous in the only step
  that carries it. Capture bytes from the environment the gate actually runs in,
  and pin every spelling the renderer can emit: locale, TTY-versus-pipe,
  `NO_COLOR`, terminal width, CI-versus-local.
- **Regenerate-and-compare is a consistency test, not a correctness test.** It
  proves the committed block matches its generator and nothing else, so it is
  blind to a bug the two share and to an input the generator ignores. Add an
  invariant the generator must satisfy internally — `stop()` before emitting an
  unsound block, reported as a rule FAIL with a reason rather than as a
  traceback. Give two functions accepting "the same" vocabulary one predicate
  rather than two agreeing regexes. Make every assertion that matters read the
  **generated artifact**, not a rule verdict. Prove it with a no-op mutant: when
  the apply step is made to return its input unchanged, the structural tests
  stay green, which is the proof that structural tests alone certify nothing.
  This is load-bearing rather than academic — since
  [ADR 0014](adr/0014-retire-the-v3-control-plane.md), regeneration is the only
  drift detector left.
- **Structural anti-vacuity.** A row with neither a resolvable term nor a
  checkable count must **fail**, not pass; that is what defends a table against
  a future edit which quietly removes everything checkable. And prove each
  predicate red **separately** — a battery that can only ever fail through one
  rule is §2's conjunction trap wearing a different hat.
- **Parity oracles are blind to mechanism.** A "the scalar engine stays in sync"
  test compares outputs, so it stays truthfully green whenever both engines
  reach the same answer by different routes. When a fix names two assignment
  sites, verify both landed. To pin a *locus* rather than an answer, assert
  against the scalar wrapper, never the `_vec` form — the `_vec` form usually
  accepts the input already, so a `_vec` assertion is green on arrival.

## 6. Documents as instruments

- **Globs under-match.** A claim quantified by a pattern narrows its own
  subject. "The three `rurl_cache_*` rows" matched two, because the third
  surface is spelled `rurl_clear_caches` — the count was right and the pattern
  was wrong, which is the inverse of the drift people look for. Verify a
  pattern's **extension**, never the number beside it.
- **Census denominators.** Splitting a contract table creates claims without
  creating cells: one split moved the population 491 → 515 and the headline
  37% → 35% while unowned claims stayed flat at 180. Read the absolute unowned
  count, never the percentage. When restructuring a table, verify losslessness
  by (row-key, property) with a script, and account for an intentional
  relocation explicitly rather than loosening the check to accommodate it.
- **Stale pins.** A `sha256` or `verified-at` pin records the bytes a record
  asserts over. Recomputing it without re-verifying the assertion converts
  honest drift into a false claim of freshness, which is worse than the drift.
  Refresh only pins whose assertion you re-checked. Since ADR 0014 retired the
  comparison in `design/work/url-v3/tools/validate-records.R`, nothing enforces
  these — `grep` finds them and no gate does.
- **Evidence maps beat coverage.** A test count, a coverage percentage and "the
  suite is green" are all statements about the tests that *exist*. Only a
  cell → `path :: test name` map reveals a contract cell with no test at all:
  the key-policy `authority` cell had zero evidence behind 148 passing tests in
  the file that should have covered it. Write the enumeration before the claim,
  and re-derive cell counts from the contract's own tables rather than from a
  register's prose.

## 7. Oracles: name what you establish

Re-derivation, re-location and transcription integrity are three different
claims. A gate labelled *re-derivation* invites the reader to assume an
independent check happened, so the output banner has to say which one it is.
[`tools/oracle/README.md`](../tools/oracle/README.md) holds the taxonomy, the
per-group status table and the per-group findings; two rules generalize beyond
it and are recorded here:

- **Condition `fsss_host == oracle_value` on `rurl_deviation` being `NA`.**
  Unconditional, the equality forces a choice between a false oracle and a
  deleted deviation. Falsify all three directions: an undocumented difference
  must fail, a documented one must pass, and **deleting the deviation must
  fail**.
- **Carry deltas as exact ledgers, never as tolerances.** A ledger row that
  *stops* disagreeing has to fail too, or the gap gets closed by adopting
  upstream's current value.

## 8. Already recorded elsewhere

This table exists so the document cannot drift against its neighbours. Each row
is deliberately **not** restated above.

| Rule | Recorded in |
|---|---|
| `tools/verify.R` output policy, `--verbose`, `watch = <regex>` | [`AGENTS.md`](../AGENTS.md) |
| Enumerate what a compensation's regex cannot match before porting it; the fixed-frame corpus rule and its evidence | [ADR 0013](adr/0013-host-charset-acceptance-in-the-parser.md) |
| The acceptance-drift rule, and which columns the sweep records for which change | [`tools/octet-acceptance-sweep.R`](../tools/octet-acceptance-sweep.R) header |
| Oracle tier taxonomy, per-group status, cache keying | [`tools/oracle/README.md`](../tools/oracle/README.md) |
| Pin verification before the pin string | [`oracle-pinning.md`](oracle-pinning.md) |
| Which fixture columns are posture-scoped | [`oracle-fixtures.md`](oracle-fixtures.md) |
