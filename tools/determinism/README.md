# Cross-platform parse-determinism harness

`rurl` parses via `curl::curl_parse_url()`, i.e. libcurl. libcurl's edge-case
URL behavior varies across versions and platform builds, so `rurl`'s **output**
is not (yet) deterministic across machines. win-builder r-devel exposed the
class: `file://` `parse_status` and U+FFFF host rendering differ from the
values recorded on macOS libcurl 8.14.1.

This directory is the probe that characterizes the **full** divergence
surface, so the class gets fixed rather than the two cases that happened to
fail. Once determinism is achieved it stays as the permanent regression
harness that proves it.

Not a test and not shipped: `^tools/determinism$` is in `.Rbuildignore`. CI
runs it only on demand and only as evidence collection — never as a gate; see
[GitHub Actions matrix](#github-actions-matrix).

## Files

| Path | Role |
| --- | --- |
| `corpus.R` | Generates the probe corpus. Base R only. |
| `corpus.csv` | The generated corpus. **Committed and regenerable** — same discipline as the `inst/bench` oracles. |
| `parse-dump.R` | Runs the corpus through `safe_parse_urls()` and dumps every output column. Base R + an installed `rurl` only. |
| `curl-probe.R` | Runs the corpus through **libcurl alone**. Base R + the `curl` package only. What the Docker matrix runs — see [Multi-libcurl matrix](#multi-libcurl-matrix). |
| `matrix/` | Docker runner that sweeps `curl-probe.R` across many libcurl versions, plus the divergence join. |
| `../../.github/workflows/determinism-probe.yml` | On-demand GHA sweep over OS × R version × locale/charset — the axes Docker cannot reach. See [GitHub Actions matrix](#github-actions-matrix). |
| `out/` | Per-platform dump artifacts. **Gitignored** (`out/.gitignore`) — evidence, not source. |
| `.gitattributes` | `*.csv -text`: never CRLF-translate the evidence. |

## Running

```sh
# regenerate the corpus (only when constructs change)
Rscript tools/determinism/corpus.R

# dump this platform
Rscript tools/determinism/parse-dump.R

# ...or against the working tree instead of the installed rurl
Rscript -e 'devtools::load_all(); source("tools/determinism/parse-dump.R")'

# probe libcurl alone on this platform (no rurl needed)
Rscript tools/determinism/curl-probe.R
```

Both scripts are run from the repo root and have **zero runtime dependencies
beyond base R and an installed `rurl`** — no jsonlite, no devtools, no
stringi — because they must run inside minimal Docker images. `stringi` is
probed with `requireNamespace()` for its ICU/Unicode versions and is never
required.

`parse-dump.R` writes `out/dump-<LABEL>.csv` and `out/env-<LABEL>.csv`.
`LABEL` comes from `$RURL_DETERMINISM_LABEL`; the default is a sanitized
`<sysname>-libcurl<version>` (e.g. `Darwin-libcurl8.14.1`). Set it explicitly
whenever two runs would otherwise collide (e.g. installed vs `load_all()`
`rurl` on the same machine).

## Encoding contract (read before touching either script)

The corpus deliberately contains C0 control characters, CR/LF, astral-plane
and non-character code points. **Those cannot survive a raw CSV cell** — that
exact bug produced a Windows CI failure and a silent CRLF corruption.
Therefore:

* Every value — corpus inputs *and* every dumped output column, `r_condition`,
  and every `env-*.csv` value — is stored as a **JSON string literal**:
  surrounded by `"`, ASCII-only, non-ASCII emitted as `\uXXXX` (astral code
  points as a surrogate pair). Both files are pure ASCII and LF-terminated.
* The escape/unescape pair is hand-rolled in base R and **deliberately
  duplicated** in `corpus.R` and `parse-dump.R` so each script is
  copy-one-file runnable in a container. `parse-dump.R` round-trips the whole
  corpus (plus a literal CRLF, NUL-free C0 controls, DEL, an astral char,
  U+FFFF, `""`, `"NA"` and `null`) through *its* copy before parsing anything
  and `stop()`s loudly on any mismatch — which also proves the two copies
  agree.

### NA sentinel

Downstream analysis must tell three things apart, so:

| R value | Written as | Bytes in the cell |
| --- | --- | --- |
| `NA_character_` | bare `null` | `null` (unquoted, 4 chars) |
| `""` | empty JSON string | `""` |
| `"NA"` | JSON string | `"NA"` |

`null` is never produced by the escaper for any string, so the mapping is
unambiguous. `r_condition` uses `""` (an empty JSON string) for "no condition
raised", never `null`.

## `corpus.csv` schema

`id, construct, input_json` — exactly, in that order.

* `id` — stable, unique, human-meaningful (`c0-host-1f`, `ipv4-octal-03`,
  `file-authority-02`, `ext-<fixture-id>`). Downstream diffs **join on it**,
  so ids are derived from the construct plus a stable key (the code point in
  hex, or the 1-based index within a hand-authored vector) — never from
  overall row order. **Only append** to the hand-authored vectors in
  `corpus.R`; never insert or reorder.
* `construct` — the bucket label the analysis groups divergences by.
* `input_json` — the JSON-escaped input.

### Constructs

| Bucket family | Coverage |
| --- | --- |
| `c0-control-{leading,trailing,scheme,host,path,query,fragment}` | every C0 control `0x01`–`0x1F` plus `0x7F`, swept by position (32 × 7). NUL is unrepresentable in an R string and is excluded by construction. |
| `c0-whatwg-strip` | tab/LF/CR specifically: repeated, paired as CRLF, and spanning delimiters — WHATWG strips these from input, libcurl historically does not. |
| `ipv4-{octal,hex,dword,mixed-radix,padded-octet,short-form,trailing-dot,out-of-range}` | IPv4 obfuscation forms. |
| `file-{authority,drive-letter,backslash-separator}` | `file://` variants including the confirmed divergent `file://example.com/path`, Windows drive forms and backslash separators. |
| `empty-component` | empty host/port/query/fragment/path/userinfo, bare `http://`, scheme-less `//`. |
| `idn-{noncharacter,astral-plane,invisible-codepoint,label-separator,punycoded-label,mixed-script,bidi,overlong-label}` | IDN/Unicode hosts. |
| `known-divergent` | the five rows win-builder actually caught, so the harness demonstrably reproduces the original failure. |
| `external-vector` | the runnable rows of `tests/testthat/fixtures/external-url-vectors.csv`, ids prefixed `ext-`. |

The external fixture is read via its **ASCII-only `input_json` column**, never
its raw `input` column (that column mis-decodes on a Windows non-UTF-8 locale
— one of the two root causes of the win-builder failure). The fixture itself
is never modified.

Unicode inputs are built with `intToUtf8()` rather than pasted as literals, so
`corpus.R` is itself pure ASCII and immune to source-encoding drift.

## `out/dump-<LABEL>.csv` schema

`id, construct, input_json, url_standard, r_condition,` then all 18
`safe_parse_urls()` output columns in order: `original_url, scheme, host,
port, path, query, fragment, user, password, domain, tld, domain_ascii,
domain_unicode, tld_ascii, tld_unicode, is_ip_host, clean_url, parse_status`.

Every row of the corpus appears three times, once per `url_standard`:
`default` (the argument omitted entirely — legacy behavior), `rfc3986`, and
`whatwg`. All other `safe_parse_urls()` arguments are left at their defaults.

`r_condition` records R-level conditions **as data** — a container that dies
mid-corpus produces no evidence. Format: `warning: <msg>` / `error: <msg>`,
multiple conditions joined with ` | `, `""` when clean. On an error the 18
value columns are the `null` sentinel.

Execution strategy: one **vectorized** `safe_parse_urls()` call per
`url_standard` (the fast path). If that call errors or raises any warning, the
whole standard is re-run **per row**, because a vectorized warning cannot be
attributed to the element that produced it. Per-row results are identical in
content; only the attribution differs.

## `out/env-<LABEL>.csv` schema

Tidy `key,value`, one row per key, values JSON-escaped like everything else:
`label`, `libcurl_version`, `libcurl_ssl_version`, `libcurl_ssl_backend`,
`libcurl_protocols`, `libcurl_libidn`, `libcurl_libssh`, `r_version`,
`r_platform`, `sysname`, `release`, `machine`, `locale`, `encoding`,
`icu_version`, `unicode_version`, `rurl_version`, `curl_version_pkg`,
`pslr_version`, `punycoder_version`, `stringi_version`, `run_utc`.

Absent values (e.g. no `libidn`, stringi not installed) are the `null`
sentinel. Under `devtools::load_all()`, `rurl_version` reports the working
tree's `DESCRIPTION` version.

## Adding a platform

1. Get an R with `rurl` (and its hard deps `curl`, `pslr`, `punycoder`)
   installed — a base image plus `install.packages()` is enough; nothing else
   is needed.
2. Make this directory available to it (bind-mount the repo, or copy
   `corpus.csv` + `parse-dump.R` — the script finds `corpus.csv` next to
   itself, in `tools/determinism/`, or in the working directory).
3. Run with an explicit label:

   ```sh
   RURL_DETERMINISM_LABEL=ubuntu2204-libcurl7.81 \
     Rscript tools/determinism/parse-dump.R
   ```

4. Copy `out/dump-<LABEL>.csv` and `out/env-<LABEL>.csv` back out. They are
   gitignored on purpose — collect them as artifacts, do not commit them.

Diffing two platforms is a plain join on `id` + `url_standard`; because every
cell is a canonical ASCII JSON literal, byte inequality *is* value inequality
(no encoding, locale or line-ending noise).

## Multi-libcurl matrix

### Why two probe scripts

| | `parse-dump.R` | `curl-probe.R` |
| --- | --- | --- |
| Calls | `safe_parse_urls()`, three `url_standard`s, 18 columns | `curl::curl_parse_url()` and nothing else |
| Needs | `rurl` + `curl` + `pslr` + `punycoder` (+ ICU via `stringi`) | base R ≥ 3.6 + the `curl` package |
| Answers | "what does a *user* see here?" | "what does *libcurl* do here?" |

The matrix runs `curl-probe.R`, not `parse-dump.R`. Building the full `rurl`
stack on a 2020-era distro means building `stringi` against that distro's ICU —
slow, fragile, and it would add ICU as a second varying input to an experiment
whose whole point is to isolate **one** variable. `curl-probe.R` needs one apt
transaction (`r-base-core r-cran-curl`) on every image in the table below.

The probe's call is byte-identical to `.parse_with_curl()` in
`R/parse-phases.R`:

```r
curl::curl_parse_url(input, decode = FALSE, params = FALSE)
```

Do not "improve" those arguments. The probe is evidence about `rurl` only
because it asks libcurl exactly what `rurl` asks it.

### Running it

```sh
# whole matrix (builds + runs every image, collects into out/)
tools/determinism/matrix/run-matrix.sh

# one or more images only
tools/determinism/matrix/run-matrix.sh ubuntu2204 debian12

# the local host, as the reference point, through the same script
Rscript tools/determinism/curl-probe.R

# join every collected label into the divergence table
Rscript tools/determinism/matrix/divergence.R
```

Needs Docker and network access to the distro archives — no secrets, no
registry auth beyond anonymous pulls. Re-running overwrites the same per-label
files, so the matrix is idempotent.

An image that refuses to provision is **reported and skipped, never fatal**.
The goal is version spread; one stubborn EOL distro must not block the sweep.
The end-of-run summary lists provisioned vs skipped.

### Images

`matrix/Dockerfile` is parameterized on `BASE_IMAGE`. The distro supplies
libcurl and R; the R `curl` package is **pinned** and built from a CRAN source
tarball in every image (see below).

| Name | Base image | libcurl | R | curl pkg |
| --- | --- | --- | --- | --- |
| `ubuntu2004` | `ubuntu:20.04` | 7.68.0 | 3.6.3 | 6.2.1 |
| `ubuntu2204` | `ubuntu:22.04` | 7.81.0 | 4.1.2 | 6.2.1 |
| `debian12` | `debian:12` | 7.88.1 | 4.2.2 | 6.2.1 |
| `ubuntu2404` | `ubuntu:24.04` | 8.5.0 | 4.3.3 | 6.2.1 |
| `debian13` | `debian:13` | 8.14.1 | 4.5.0 | 6.2.1 |
| — (host) | local macOS | 8.14.1 | 4.6.0 | 7.1.0 |

`curl_url()`, the API `curl_parse_url()` wraps, landed in libcurl 7.62.0
(2018), so the sample starts just above the floor and runs to current. The
image list is a means, not an end — substitute freely for more spread.
`ubuntu:20.04` is EOL and no longer resolves on its live archive host;
`run-matrix.sh` repoints its `sources.list` at old-releases via the `APT_PRE`
build arg. `debian:11` (7.74) and `ubuntu:23.10` (8.2) were tried and
**dropped**: their archive mirrors resolve but cannot satisfy
`build-essential` + `libcurl4-openssl-dev`. Dropping an image is the intended
outcome of the time-box, not a failure of the sweep.

#### Why the curl package is pinned, not taken from the distro

`r-cran-curl` is *not* usable here: `curl_parse_url()` was added in the R
`curl` package 6.0.0, and the distro packages across this matrix range 4.3 →
6.2.1 — only `debian:13`'s has the function at all. So the image installs
`build-essential` + `libcurl4-openssl-dev` and builds a single pinned CRAN
source tarball (`CURL_PKG_VERSION`, default 6.2.1) against whatever libcurl
the base image ships. 6.2.1 is chosen because it builds unmodified on both R
3.6.3 and R 4.5.

That pin is what makes the sweep an experiment: across the five containers
**libcurl is the only variable that moves**. R still varies (it does not
implement URL parsing) and is recorded per label.

**The macOS host row is confounded and must be treated as such.** It carries
both a different libcurl *build* and a different curl package (7.1.0), so a
macOS-only difference is not attributable to libcurl. Read the containers
against each other; use macOS as a reference point, not as a matrix cell. Both
versions are recorded in every `curlenv-<LABEL>.csv`: a divergence that tracks
`curl_version_pkg` rather than `libcurl_version` is a confound, not a libcurl
finding.

### Label convention

`<image-name>-libcurl<version>`, e.g. `ubuntu2204-libcurl7.81.0`. The
`RURL_DETERMINISM_LABEL_PREFIX` env var supplies the image name;
`matrix/entrypoint.sh` discovers the libcurl version **at run time** (never
assumed from the table above) and composes the full label, which
`curl-probe.R` then sanitizes to `[A-Za-z0-9._-]`. Setting
`RURL_DETERMINISM_LABEL` directly overrides the whole thing — that is how the
macOS host run is labelled.

### `out/curl-<LABEL>.csv` schema

`id, construct, input_json, curl_status,` then the 9 fields
`curl_parse_url()` returns, in curl's own order: `url, scheme, user,
password, host, port, path, query, fragment`.

`curl_status` is `ok`, or `error: <msg>` / `warning: <msg>` (multiple
conditions joined with ` | `). Conditions are captured as **data** — a
container that dies mid-corpus yields no evidence. On an error every value
column is the `null` sentinel. Fields an older `curl` package does not return
are also `null`.

Some libcurl builds return a field that is **not valid UTF-8** (libcurl
percent-decodes the host, and `%80` / `%A0` are not UTF-8). That is a finding,
not a harness bug, so it is neither fatal nor normalized away: the field is
written as `<non-utf8-bytes:XX…>` (the raw bytes in lowercase hex, a sentinel
no legitimate parse result can collide with) and `curl_status` becomes
`warning: non-utf8 field(s): <names>`.

### `out/curlenv-<LABEL>.csv` schema

Tidy `key,value`, JSON-escaped like everything else: `label`, `probe`,
`libcurl_version`, `libcurl_ssl_version`, `libcurl_ssl_backend`,
`libcurl_protocols`, `libcurl_libidn`, `libcurl_libssh`, `libcurl_libz`,
`r_version`, `r_platform`, `sysname`, `release`, `version`, `machine`,
`locale`, `encoding`, `curl_version_pkg`, `run_utc`.

### `out/curl-divergence.csv`

`matrix/divergence.R` joins every `curl-<LABEL>.csv` in `out/` on `id` and
keeps only the `(id, column)` pairs where **at least two labels disagree**:

`id, construct, input_json, column, n_distinct,` then one column per label
(labels sorted, names made syntactic). Sorted by `id` then by the fixed
column order, so re-runs are byte-stable.

It is deliberately raw — no bucketing, no interpretation, no prose. That is
the downstream analysis unit's job. All labels present in `out/` participate,
so drop or add a probe file to change the comparison set.

## GitHub Actions matrix

`.github/workflows/determinism-probe.yml`.

### Why, given the Docker matrix already exists

The Docker matrix varies **libcurl version on one OS**. It cannot reach the
axis that broke win-builder: every Linux libcurl in the table above rejects
`file://example.com/path`, so Windows accepting it is a property of the
Windows **build**, not of a libcurl version. Only a real Windows runner probes
that. R for Windows ships its own bundled libcurl, so there the R version *is*
the libcurl-build axis — which is why `release`/`devel`/`oldrel-1` are swept.

The second axis the containers do not carry is **locale**. A Turkish locale is
where a locale-sensitive `tolower()`/`toupper()` breaks scheme/host case
folding (RURL-ugfpuotu); running it on all three OSes makes that fix a
permanently guarded property rather than a one-platform assertion. It is run at
R `release` only — the fix is in `rurl`'s own code and is not R-version
dependent, so crossing it with the build axis would triple the cost for no new
information.

| | Docker matrix | GHA matrix |
| --- | --- | --- |
| Varies | libcurl version | OS build × R version × locale/charset |
| Runs | `curl-probe.R` | `curl-probe.R` **and** `parse-dump.R` |
| Windows | impossible | the point |

### Why there is a charset axis

The three runners **do not agree on a default character set**:

| Runner | as-shipped locale | charset |
| --- | --- | --- |
| `ubuntu-latest` | `LC_ALL=C` | US-ASCII |
| `macos-latest` | `en_US.UTF-8` | UTF-8 |
| `windows-latest` | `English_United States.utf8` | UTF-8 |

So a cross-OS comparison of `default` cells **confounds OS build with
charset**. Measured on the first two live sweeps: ubuntu `tr` vs ubuntu
`default` differed by 146 lines, and none of it was the Turkish hazard — it was
C vs UTF-8:

```
default (LC_ALL=C):  host = "<ef><bf><bf>y"   <- raw bytes, not UTF-8-marked
tr      (UTF-8):     host = "<U+FFFF>y"       <- the code point
```

Affected constructs: `external-vector` (44 rows), `idn-mixed-script` (18),
`idn-invisible-codepoint` (18), `idn-noncharacter` (15), `idn-bidi` (15),
`idn-label-separator` (12), `idn-astral-plane` (12), `idn-punycoded-label` (6),
`known-divergent` (3), `idn-overlong-label` (3). ASCII-only constructs —
including the `file://example.com/path` finding — are unaffected.

Without this axis the analysis would attribute a **charset** difference to a
**platform build**. Hence: the charset is made explicit and held constant
wherever the matrix compares platforms.

### Cells

A deliberate `include:` list, **not** a cross product. A full `os × r ×
locale` cross would be 32 cells and buy nothing: the R-version axis exists to
reach the Windows libcurl *build*, while locale/charset is a per-OS property.
They are crossed only at R `release`, where an interaction is plausible.
`macos-latest`/`devel` is absent throughout — `setup-r` publishes no R-devel
build for macOS. **17 jobs.**

| Axis | OS | R | locale | Label |
| --- | --- | --- | --- | --- |
| build | `ubuntu-latest` | `release` | `utf8` | `gha-ubuntu-latest-Rrelease-utf8` |
| build | `ubuntu-latest` | `devel` | `utf8` | `gha-ubuntu-latest-Rdevel-utf8` |
| build | `ubuntu-latest` | `oldrel-1` | `utf8` | `gha-ubuntu-latest-Roldrel-1-utf8` |
| build | `windows-latest` | `release` | `utf8` | `gha-windows-latest-Rrelease-utf8` |
| build | `windows-latest` | `devel` | `utf8` | `gha-windows-latest-Rdevel-utf8` |
| build | `windows-latest` | `oldrel-1` | `utf8` | `gha-windows-latest-Roldrel-1-utf8` |
| build | `macos-latest` | `release` | `utf8` | `gha-macos-latest-Rrelease-utf8` |
| build | `macos-latest` | `oldrel-1` | `utf8` | `gha-macos-latest-Roldrel-1-utf8` |
| charset | `ubuntu-latest` | `release` | `c` | `gha-ubuntu-latest-Rrelease-c` |
| charset | `windows-latest` | `release` | `c` | `gha-windows-latest-Rrelease-c` |
| charset | `macos-latest` | `release` | `c` | `gha-macos-latest-Rrelease-c` |
| charset | `ubuntu-latest` | `release` | `tr` | `gha-ubuntu-latest-Rrelease-tr` |
| charset | `windows-latest` | `release` | `tr` | `gha-windows-latest-Rrelease-tr` |
| charset | `macos-latest` | `release` | `tr` | `gha-macos-latest-Rrelease-tr` |
| baseline | `ubuntu-latest` | `release` | `default` | `gha-ubuntu-latest-Rrelease-default` |
| baseline | `windows-latest` | `release` | `default` | `gha-windows-latest-Rrelease-default` |
| baseline | `macos-latest` | `release` | `default` | `gha-macos-latest-Rrelease-default` |

* **build axis** — 3 OS × 3 R at a *fixed UTF-8 charset*. The only like-for-like
  cross-OS comparison: a divergence here is an OS/R build property.
* **charset axis** — 3 OS × {`c`, `tr`} at R `release`. Combined with the R
  `release` `utf8` cells above, this gives `c` / `utf8` / `tr` per OS with the
  build held constant. `tr` is also UTF-8, so `tr` vs `utf8` differs **only** in
  language (case mapping) and `c` vs `utf8` **only** in character set.
* **baseline** — the runner as shipped. Its divergence from the same OS's
  `utf8` cell is itself a finding ("what a user actually gets").

> **`default` cells are NOT cross-OS comparable.** Each runner ships a
> different charset, so diffing `gha-ubuntu-latest-Rrelease-default` against
> `gha-macos-latest-Rrelease-default` measures charset, not platform. Every
> cell records `cross_os_comparable` in `locale-<LABEL>.csv`; it is `false`
> exactly for these three. Use the `utf8` cells for cross-OS work.

`rurl` is pinned to the **checked-out source tree** (`local::.` via
`setup-r-dependencies`), never CRAN. An unpinned matrix measures `rurl` drift
and misattributes it to libcurl; `rurl_version` in `env-<LABEL>.csv` records
the pin but does not create it.

`curl-probe.R` is the hard requirement — if it cannot run, the cell fails. The
full stack is `continue-on-error`: a cell that cannot build `rurl` degrades to
libcurl-only and writes `out/DEGRADED-<LABEL>.txt` into its artifact, so
"no dump here" is distinguishable from "dump lost". A **divergence never fails
the build** — divergence is the product, not the defect.

### Setting the locale

The locale must be in place **before R starts**. A mid-session
`Sys.setlocale()` is not sufficient: ICU (via `stringi`) fixes its default
locale at initialization, so a late switch can leave `stri_trans_tolower()`
behaving as if in the default locale — the `tr` cells would then probe exactly
what the `default` cells probe, while looking green. `LANG`/`LC_ALL` are also
not portable here: R on Windows takes its locale from the OS and ignores them,
and the Linux runners ship neither a Turkish nor an `en_US.UTF-8` locale
generated. So, per `locale` value:

| locale | Linux | macOS | Windows |
| --- | --- | --- | --- |
| `default` | nothing set | nothing set | nothing set |
| `c` | `LANG`+`LC_ALL=C` | `LANG`+`LC_ALL=C` | `Sys.setlocale("LC_ALL", "C")` — sole candidate |
| `utf8` | `LANG`+`LC_ALL=en_US.UTF-8`, after `sudo locale-gen en_US.UTF-8` | `LANG`+`LC_ALL=en_US.UTF-8` — present in the base system | `English_United States.utf8`, `en-US.UTF-8`, `en_US.UTF-8`, then the fallbacks `English_United States.1252`, `English` |
| `tr` | `LANG`+`LC_ALL=tr_TR.UTF-8`, after `sudo locale-gen tr_TR.UTF-8` | `LANG`+`LC_ALL=tr_TR.UTF-8` — present in the base system | `Turkish_Türkiye.utf8`, `tr-TR.UTF-8`, `tr_TR.UTF-8`, then the fallbacks `Turkish_Türkiye.1254`, `tr-TR`, `Turkish_Turkey.1254`, `Turkish` |

Linux/macOS use `LANG`+`LC_ALL` **in the step env**, i.e. set before R is
exec'd. Windows uses `R_PROFILE_USER` → a generated `gha-locale.Rprofile`, the
earliest R-side hook (it runs at session start, before any package loads);
candidates are tried in order and the session **aborts** if none applies.

Every UTF-8 form comes first on purpose: on the `tr` axis the variable under
study is **case mapping**, so a non-UTF-8 codepage would be a second
uncontrolled variable — and reaching one of those fallbacks now trips the
charset assertion below rather than passing quietly. Conversely the `c` cells
ask for exactly `C` and nothing else: a silent upgrade to a UTF-8 locale would
delete the axis. `default` sets nothing — it is the runner's own locale, and
the honest baseline. The OS/locale `case` has a hard-failing fallback branch,
so an unhandled pair can never quietly probe the default locale under a
labelled cell.

#### Two Windows facts, both measured on PR #181

Neither is obvious and both cost a live run, so they are recorded here rather
than left to be re-derived:

1. **`Turkish_Turkey.*` does not exist.** Windows renamed the locale, so the
   name is `Turkish_Türkiye` (U+00FC). The original candidate list asked for
   `Turkish_Turkey.utf8`, got nothing, and fell through to bare `Turkish` —
   which resolves to `Turkish_Türkiye.**1254**`, precisely the codepage
   fallback the ordering was meant to avoid. The workflow file stays pure
   ASCII, so the name is assembled with `intToUtf8(0x00FC)` inside the
   generated Rprofile and referenced as the token `%TRTR%` in the candidate
   list.
2. **Setting the CRT locale is not enough — ICU ignores it.** With
   `LC_CTYPE=Turkish_Türkiye.1254` in force, `stringi` still reported ICU
   locale `en_US` and `stri_trans_tolower("WIKI") == "wiki"`. Unlike on Unix,
   `stringi` on Windows does **not** derive its ICU default locale from the
   R/CRT locale. Since `rurl` folds case through `stringi`/ICU, this is the
   fault that actually matters: fixing (1) alone still leaves the axis dead.

The lever for (2) is `stringi::stri_locale_set("tr_TR")` — it sets the default
locale that every `stri_trans_*()` call without an explicit `locale=` uses,
which is exactly the code path at risk. It is applied from a
`packageEvent("stringi", "onLoad")` hook registered by the Rprofile, **not**
inline in the Rprofile: the Rprofile runs before any package loads, so
`stringi` is not loadable there (and in a libcurl-only cell is not installed at
all). The hook fires when `stringi` appears, in whichever process loads it.

This is a faithful simulation, not a cheat: on a genuinely Turkish Windows
machine ICU reports `tr_TR` unprompted. The runner's OS locale cannot be
changed, so the cell sets explicitly what that machine would supply — and
**only** where the ambient mechanism did not already do it. Linux and macOS are
left alone, `default` cells are never touched, and `icu_locale_source` records
which happened.

### `out/locale-<LABEL>.csv` — are the axes actually armed?

Mechanism is the belt; this is the braces. Every cell measures whether the
Turkish case-mapping hazard is *live* and whether it got the character set it
asked for, and writes both verdicts next to the dumps, tidy `key,value` and
JSON-escaped like every other artifact:

`label`, `locale_requested`, `locale_effective`, `encoding`,
`charset_expected`, `charset_utf8`, `charset_as_requested`, `lc_ctype`,
`l10n_info`, `cross_os_comparable`, `base_tolower_WIKI`, `base_toupper_i`,
`stringi_available`, `stringi_locale`, `icu_locale_source`,
`stri_trans_tolower_WIKI`, `hazard_armed_base`, `hazard_armed_stringi`,
`hazard_armed`, `run_utc`.

#### The charset keys

| Key | Meaning |
| --- | --- |
| `charset_expected` | What the cell asked for: `utf8` (the `utf8` and `tr` cells), `non-utf8` (the `c` cells), `unspecified` (the `default` cells). |
| `charset_utf8` | The **effective** character set, from `l10n_info()[["UTF-8"]]`: `true`/`false`. Group cells by this — never by parsing locale names, since `en_US.UTF-8`, `English_United States.utf8` and `C.UTF-8` are all UTF-8 and none of them say so the same way. |
| `charset_as_requested` | `charset_utf8` vs `charset_expected`. `null` for `default` cells (they asked for nothing). `false` means the cell is **not** a like-for-like charset comparison. |
| `lc_ctype` / `l10n_info` | The raw evidence behind the verdict: `Sys.getlocale("LC_CTYPE")` and the whole `l10n_info()` list serialized `k=v; k=v` (`MBCS`, `UTF-8`, `Latin-1`, `codeset` on Unix / `codepage`, `system.codepage` on Windows). |
| `cross_os_comparable` | `false` exactly for the `default` cells, `true` otherwise. Do not diff a `false` cell against another OS. |

A cell whose `charset_as_requested` is `false` emits a `::warning::` and is
still collected — same discipline as `hazard_armed`: never fail the cell, never
let it pass as a clean comparison.

Armed means `tolower("WIKI")` yields `wıkı` (or `toupper("i")` yields
`İ`). Both engines are recorded because they **disagree by platform** —
macOS libc `tolower()` is not Turkish-aware while ICU is, so a macOS `tr` cell
reports `hazard_armed_base=false, hazard_armed_stringi=true`. `rurl`'s own case
folding goes through `stringi`/ICU, so `hazard_armed` is the disjunction.

`icu_locale_source` says **how** ICU got its locale, so a natural repro stays
distinguishable from a simulated one:

| Value | Meaning |
| --- | --- |
| `ambient` | ICU was already Turkish (or no override was requested) — the environment did it. Linux and macOS `tr` cells, and every `default` cell. |
| `explicit` | The `stringi` onLoad hook called `stri_locale_set()`. Expected on Windows `tr` cells. |
| `explicit-late` | The hook did not fire; the assertion step set it as a belt. Treat as a harness defect worth looking at, not as evidence. |
| `failed` | `stri_locale_set()` errored. The cell is unarmed. |
| `none` | `stringi` is not installed (libcurl-only cell). |

A `tr` cell that is **not** armed emits a `::warning::` and records
`hazard_armed=false`. It does not fail the cell — but U4 must not read it as a
clean negative, because that cell measured nothing the `default` cell did not.
`default` cells run the same assertion as the control.

### Label convention

`gha-<os>-R<rver>-<locale>`, e.g. `gha-windows-latest-Rdevel-utf8`. Unique by
construction (the label *is* the cell tuple and the `include:` list carries no
duplicate tuple) and already within the `[A-Za-z0-9._-]` set both probes
sanitize to, so no two cells can collide after sanitization and overwrite each
other's evidence. The full list is in [Cells](#cells).

### Running it, and collecting the results

```sh
# dispatch (only available once the workflow is on the default branch;
# before that, the path-filtered pull_request trigger is what runs it)
gh workflow run determinism-probe.yml

# libcurl-only sweep, skipping the full rurl stack
gh workflow run determinism-probe.yml -f run_parse_dump=false

# pull every cell's artifact into out/, then join
gh run download <run-id> --dir tools/determinism/out
find tools/determinism/out -mindepth 2 -name '*.csv' -exec mv -n {} tools/determinism/out/ \;
Rscript tools/determinism/matrix/divergence.R
```

Artifacts are one per cell, named `determinism-<LABEL>`, retained 90 days.
`gh run download` unpacks each into its own subdirectory, hence the flatten
step — `divergence.R` reads `out/curl-*.csv` at the top level. Flattening is
safe because filenames already carry the label.

Triggers: `workflow_dispatch`, plus a `pull_request` filtered to just
`tools/determinism/**` and the workflow file. The filter is deliberately narrow:
the sweep never fails on a divergence, so as a PR check it produces **no**
automatic signal — a human has to download artifacts — and firing 17 jobs on
every parse-pipeline PR would be latency and noise for no gate. Determinism
regressions on parse changes are caught by **dispatching this workflow
manually**, not by widening the filter. Deliberately **not** `push: [main]`
either — 17 jobs across Windows (2×) and macOS (10×) minute multipliers is an
on-demand probe, not a per-push gate.
