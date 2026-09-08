# Windows, via win-builder — transcript

## 3.0.1 — 2026-09-06 — **RED on both queues**

Two runs of `rurl_3.0.1.tar.gz`, one per queue, notified by email at 17:37 and
17:43 CEST. Raw logs are preserved next to this file
(`win-builder-logs/3.0.1-devel-00check.log`,
`win-builder-logs/3.0.1-release-00check.log`) because win-builder deletes result
directories after roughly 72 hours.

| | R-devel | R-release |
| --- | --- | --- |
| result URL | `https://win-builder.r-project.org/hS7iqtREIv3f` | `https://win-builder.r-project.org/55298c3m74cl` |
| R | R Under development (unstable) (2026-09-04 r90492 ucrt) | R version 4.6.1 (2026-06-24 ucrt) |
| log directory | `d:/RCompile/CRANguest/R-devel/` | `d:/RCompile/CRANguest/R-release/` |
| check began | 2026-09-06 15:34:10 UTC | 2026-09-06 15:40:10 UTC |
| install time | 7s | 7s |
| check time | 203s | 197s |
| tests | `[121s] ERROR` | `[109s] ERROR` |
| vignettes | re-built OK | re-built OK |
| manual | PDF `[17s] OK`, HTML OK | PDF `[17s] OK`, HTML OK |
| **Status** | **1 ERROR, 1 NOTE** | **1 ERROR, 1 NOTE** |

Both ran on `x86_64-w64-mingw32`, Windows Server 2022 x64 (build 20348), R
compiled by gcc 14.3.0 / GNU Fortran 14.3.0, session charset UTF-8. Everything
outside `checking tests` passed on both.

The NOTE is the expected `checking CRAN incoming feasibility` note — maintainer
address, the `BugReports:` 404, and `IDNA`/`Punycode` flagged by `aspell`. It is
unchanged from 3.0.0 and is addressed in `cran-comments.md`.

### The ERROR: the locale pin did not work

`[ FAIL 11 | WARN 0 | SKIP 8 | PASS 7146 ]`, identical on both queues, every
failure in `tests/testthat/test-external-url-vectors.R`:

| line | expectation | actual | expected |
| --- | --- | --- | --- |
| 119 | `nrow(fx)` | **389** | 396 |
| 120 | `sum(fx$runnable == "yes")` | **316** | 325 |
| 121 | `sum(!is.na(fx$rurl_deviation))` | **43** | 41 |
| 123 | `sum(fx$source_class == "A")` | **367** | 375 |
| 129 | `sum(fx$divergence_class == "both-reject")` | **91** | 100 |
| 155 | `all(fx$source_class %in% c("A","B","C"))` | FALSE | TRUE |
| 156 | `all(fx$standard %in% c("rfc3986","whatwg","both"))` | FALSE | TRUE |
| 161 | `all(is.na(fx$rurl_whatwg_status[!runnable]))` | FALSE | TRUE |
| 190 | `all(...)` (dual-standard oracle) | FALSE | TRUE |
| 243 | `all(fx$runnable[dev] == "yes")` | FALSE | TRUE |
| 245 | `all(grepl("ADR [0-9]{4}\|RURL-[a-z]+", fx$rurl_deviation[dev]))` | FALSE | TRUE |

**The six failures that 3.0.0 reported are still here, unchanged.** They are
the same six tests — 3.0.0's lines 95/96/101 and 130/183/185 are this tree's
155/156/161 and 190/243/245, shifted by the block the fix inserted. The five new
lines are the corpus-shape pin the fix added. So the pin worked as designed —
it made the damage legible — and the `LC_CTYPE = "C"` reader **changed nothing
about the parse**.

`WARN 0` again, and `nrow` is short rather than the columns being NA, so this is
still rows merging into their neighbours, not truncation.

### What the transcript rules out

* **Not the tarball.** `tests/testthat/fixtures/external-url-vectors.csv` inside
  the checked tarball is byte-identical to the working tree: 280019 bytes, md5
  `d6ad52f244c2f45ada2caa560a93a5e6`, read back from
  `.../examples_and_tests/tests/testthat/fixtures/` on the R-devel result page.
  Nothing translated line endings in transit; the file has 0 CR bytes at both
  ends.
* **Not R-devel-only.** R-release 4.6.1 fails identically, so this is not a
  development-version regression.
* **Not reproducible by locale on macOS.** Reading the same file under
  `LC_CTYPE` = `C`, `en_US.UTF-8` and `UTF-8` on macOS aarch64 / R 4.6.0 returns
  `nrow = 396`, `runnable == "yes"` = 325 in all three. The pin is a no-op
  locally, which is why the local gate never had evidence for or against it.
* **Not `0x1A`-as-EOF.** The file's single `0x1A` sits at byte offset 45709,
  inside raw line 106 of 413. Text-mode EOF there would leave ~105 rows, not
  389.

### The unexamined premise

`read_vectors()`'s header comment calls `input_json` an "ASCII-only,
JSON-escaped" source of truth. Measured against the committed file, it is not:
`input_json` carries **17 non-ASCII cells and 1 raw control character**. The
column that was supposed to be insulated from the mis-parse is subject to the
same hazard as `input` (35 control characters, 22 non-ASCII), so reconstructing
`input` from it does not sidestep anything.

`input_json` *is* non-NA on all 396 rows, including the 71 non-runnable rows
where `input` is NA by invariant — so it does carry every row, and a genuinely
ASCII-escaped `input_json` would be a complete source.

## 3.0.1.9002 — 2026-09-07 — **GREEN on R-devel**

Probe build 2, uploaded to the R-devel queue only, notified by email at 23:39
CEST. Result URL `https://win-builder.r-project.org/R8WbZQ5MYFL3`; the logs are
preserved next to this file as `3.0.1.9002-devel-00check.log`,
`3.0.1.9002-devel-diagnose-windows-fixture.Rout` and
`3.0.1.9002-devel-testthat.Rout`.

`Status: 1 NOTE` — incoming feasibility only (large version components, the
maintainer address, `IDNA`, and the `BugReports:` 404). The suite:

```
[ FAIL 0 | WARN 0 | SKIP 8 | PASS 7157 ]
```

The eleven `test-external-url-vectors.R` failures are gone.

### The cause, measured

The probe carried a `tests/diagnose-windows-fixture.R` that ran ahead of
`testthat.R`, so the mechanism and the verdict are in the same run.

* **The fixture bytes are intact in the check tree.** 280019 bytes, md5
  `d6ad52f2...`, 413 LF, 0 CR, 17012 quotes, 460 bytes >= 0x80. A locale-free
  RFC 4180 record count over those bytes gives **396 on Windows**. The bytes
  describe 396 rows there, same as everywhere.
* **The `LC_CTYPE = "C"` pin takes, and is irrelevant.** Probe 1
  (`3.0.1.9001`, `https://win-builder.r-project.org/N17GS24jtbvX`) confirmed
  `l10n_info()` goes MBCS TRUE -> FALSE and codepage 65001 -> 0 under the pin,
  and the corpus still read 389. This kills the `mbrtowc()` theory 3.0.1 was
  built on.
* **It is the connection layer.** Same process, same locale, same `scan()`:
  through a file connection, 389 rows; from memory, 396.
  `read.csv(path)` 389, `textConnection(rawToChar(raw))` 396,
  `read.csv(text = rawToChar(raw))` 396.
* **Which bytes.** `readLines(file(fx, "rb"))` reconstructs all 280019 bytes
  across 413 lines. `readLines(file(fx, "r"))` returns **405 lines / 276576
  bytes — 3443 short**, and the first byte where the rejoined text differs from
  the file is offset **45710**, which holds the file's single **`0x1A`**.

### Correcting "What the transcript rules out"

The 2026-09-06 bullet **"Not `0x1A`-as-EOF"** reasoned that text-mode EOF at
that byte would leave ~105 rows, not 389. That arithmetic is right and the
conclusion it drew is wrong: the text-mode connection does not *truncate* at
the `0x1A`, it loses 3443 bytes there and resynchronizes further on. The byte
was the right suspect; "as-EOF" was the wrong model of what it does. Every
other bullet in that section stands.

The "unexamined premise" bullet also stands but is now moot: `input_json`
carrying 17 non-ASCII cells does not matter once the reader never decodes
through a connection at all.

### The fix

`read_vectors()` reads the file with `readBin()`, marks the string UTF-8 and
parses it with `read.csv(text = )`. No connection is opened, so no text-mode
connection can be met. Executable content of the green probe and of the
submission tree is byte-for-byte identical — the probe differed only in
`DESCRIPTION`'s version string and in the diagnostic script, both removed
before the fix landed on `main`.
