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

Not a test, not run by CI, not shipped: `^tools/determinism$` is in
`.Rbuildignore`.

## Files

| Path | Role |
| --- | --- |
| `corpus.R` | Generates the probe corpus. Base R only. |
| `corpus.csv` | The generated corpus. **Committed and regenerable** — same discipline as the `inst/bench` oracles. |
| `parse-dump.R` | Runs the corpus through `safe_parse_urls()` and dumps every output column. Base R + an installed `rurl` only. |
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
