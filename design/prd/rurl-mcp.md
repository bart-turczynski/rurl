# PRD — rurl-mcp: MCP server + Claude skill for URL cleaning/extraction

**Status:** Converged 2026-06-27; graduated per
[ADR 0008](../adr/0008-prd-graduation-and-design-docs-home.md) from
`_scratch/PRD-rurl-mcp.md`, which is gitignored and therefore not durable. It is
the sole design record for a repository that **does not exist yet**.
**Scope:** New sibling repo `rurl-mcp` (NOT part of the rurl package)
**Audience:** SEO practitioners with little/no R skill
**Author:** design session 2026-06-27
**v1 locks:** stdio transport only, never remote or Docker · `Rscript` front end
(unusual in an npx/uvx ecosystem, so hosted-runner registries list it as
local/self-hosted) · results inline at ten rows or fewer, CSV above that.
**Related fp issues:** deferred query-stripping decision `RURL-jtsyckxr`;
registries epic `RURL-nzlihhfz` (children: `RURL-unagtnwv` official registry,
`RURL-vffwztxs` Smithery, `RURL-wxopopjg` Glama, `RURL-mojqnzdx` mcp.so,
`RURL-btoqfwzy` PulseMCP, `RURL-uvvnpiwf` awesome-mcp-servers PR)
**Companion:** rurl 1.5.0 query-param work (PRD-query-param-handling.md) may
later supply selective query control that `clean_url` simply exposes.

---

## 1. Background & problem

`rurl` is a capable R package for parsing/normalizing/cleaning/joining URLs,
but its audience is gated behind R. SEO practitioners — the people who most
want bulk URL cleaning, domain/TLD extraction, and dedup — generally do not
write R. We want to put rurl's capabilities in front of them through two
agent-native front-ends that share the same backend:

1. **An MCP server** (`rurl-mcp`) — portable across every MCP client, listable
   on MCP directories.
2. **A Claude skill** — near-zero setup, Claude-only, drives rurl directly.

Both are thin wrappers over `rurl`; the package stays the single source of
truth for URL logic.

## 2. Goals

- Let non-R users clean, parse, and extract from URLs in bulk via natural
  language in their agent of choice.
- Be discoverable: SEO-keyword-rich listings on the major MCP directories.
- Keep the wrappers thin — no reimplementation of URL logic; `rurl` owns it.
- Identical behavior across the MCP and the skill (shared helper layer).
- Transparent output: every result row carries its input alongside its output.

## 3. Non-goals (v1)

- **No remote / hosted server. No Docker.** stdio only, ever.
- **No selective query-param stripping** in v1 — `clean_url` ships rurl's
  current behavior verbatim (see §6.1). Revisit via `RURL-jtsyckxr` once rurl
  1.5.0 lands query filtering.
- **No `canonical_join` in v1** — parked as the first post-v1 update (§6.6).
- **No `.mcpb` Desktop Extension bundle** in v1 — parked (§6.4).
- No config-file auto-writing logic of our own (agents / `claude mcp add` do it).

## 4. Decision log (resolved)

| # | Decision | Choice |
|---|---|---|
| D1 | Wrapping approach | R-native via `mcptools` (+ `ellmer`), no FFI/subprocess marshaling |
| D2 | Transport | stdio only; no remote, no Docker — permanent |
| D3 | Repo | standalone **plain-script** repo `rurl-mcp`, sibling to rurl |
| D4 | v1 tools | `clean_url`, `parse_url`, `extract` (intent-shaped, batch-capable) |
| D5 | `clean_url` semantics | ship rurl's native `get_clean_url` as-is (drops whole query + fragment); expose all 11 params. **Tool description MUST warn loudly** that query + fragment are dropped (review #1) |
| D6 | Tracking-param stripping | deferred → `RURL-jtsyckxr`; if added, lives rurl-side (1.5.0), wrapper just exposes |
| D7 | `parse_url` output | full `.spu_result_fields` set + `subdomain`, as structured JSON; **excludes `user`/`password` by default** (the two credential fields in the parse result — rurl has no `userinfo` field); default `include_credentials=false`, pass `true` to return `user`+`password` (review #7, R2#1) |
| D8 | `extract` components | PSL/structural only (no credentials, no clean_url/parse_status); array, default `["domain"]` |
| D9 | I/O modes | inline (array→stdout) vs CSV (file path→file); trigger = argument presence. **Exactly one of `urls`/`input_csv` required — both or neither is a tool-level error** (review #4) |
| D10 | Inline / output | If `output_csv` is supplied, write CSV **regardless of row count**. Else: ≤10 rows → inline; >10 → tool returns **structured** `status="needs_output_csv"` (not prose; tools are stateless) (review #5, R2#2, T1a) |
| D11 | CSV column detect | auto-detect `url`/`URL`/`address`/`Address` + `url_column` override; **multiple candidates → tool-level error requiring `url_column`** (review #8) |
| D12 | CSV output shape | preserve all columns (`check.names=FALSE`); insert result column **immediately right of source** |
| D13 | Result column naming | `<url_column>_clean`, `<url_column>_<component>`, `<url_column>_<field>` for parse (T3; `<url_column>` = the detected/overridden URL column — *not* the PSL `source` arg, R2#5); **existing target name → tool-level error** (review #6/#8) |
| D14 | Output path | **required** for CSV/overflow (no auto-naming); `output_csv == input_csv` → error (review #8) |
| D15 | Failure handling | **URL-parse** failures → row-level `NA`; **validation + I/O** failures (missing path, bad enum, unreadable/unwritable file, missing/ambiguous column) → tool-level errors (review #3) |
| D16 | Dedup | `rows="all"` (default, 1:1, transparency applies) vs `rows="distinct"` = **aggregation: distinct transformed values + `count`** (satisfies count-per-domain). Allowed only for `clean_url` and single-component `extract`; **rejected for `parse_url` and multi-component `extract`**. Unparseable/blank URLs collapse into one `NA` value row with its count, so `sum(count) == n_input` (review #2, R2#3/#4, T2a) |
| D17 | Transparency | `rows="all"`: every output row carries input + result. `rows="distinct"` is an aggregate (input passthrough N/A by design) (review #2) |
| D18 | Skill relationship | **stands alone** over `Rscript` (no MCP dependency); shares helper `.R` with `server.R` |
| D19 | Install | minimal `install.sh`/`install.ps1`: check R → install CRAN deps (with `repos=`) → print `server.R` path; detect-and-guide for R itself (review impl) |
| D20 | Client registration | one JSON `mcpServers` snippet (all JSON clients) + TOML variant (Codex); `claude mcp add`/`codex mcp add`/agent-driven |
| D21 | Docs | tool descriptions (in code) + README + `llms.txt`; `llms.txt` canonical, README mirrors, skill references |
| D22 | Scaffolding | **MIT (confirmed — `DESCRIPTION:14`)** · SemVer from 0.1.0 + tags · `testthat` on helpers + smoke test · GH Actions: mac + ubuntu, **+ Windows install/script smoke** (review impl) |
| D23 | Output substrate | **all tools return structured JSON** as canonical; inline ≤10 may add a readable rendering, but structured is primary (review #9) |
| D24 | Version pinning | `DESCRIPTION`/install pin `rurl (>= <current>)`; **smoke test asserts `get_clean_url`'s 11 formals** to catch CRAN drift (review #10) |
| D25 | stdout hygiene | **HARD REQUIREMENT:** `server.R` must emit ONLY MCP protocol on stdout — suppress package startup messages, route all diagnostics to stderr (review impl) |

## 5. Architecture

Two parallel thin front-ends over one backend:

```
                         rurl (CRAN) — URL logic, single source of truth
                                    ^
                    ┌───────────────┴────────────────┐
            shared helper .R (CSV column detect, insert-adjacent,
                rows dedup, inline-overflow, NA handling)
                    ^                                ^
            server.R (mcptools tools)         skill helper scripts
                    ^                                ^
            any MCP client (stdio)            Claude runs Rscript
```

- **Backend:** `rurl` (+ `pslr`/`punycoder`/`mcptools`/`ellmer`), all on CRAN.
- **Shared helper `.R`:** the Q3/T1/T2/T3 conventions live here ONCE, called by
  both `server.R` and the skill's helper scripts, so MCP and skill behave
  identically.
- **MCP front-end:** `server.R` registers the three tools via `ellmer::tool()`
  and serves over stdio (`mcptools::mcp_server()`).
- **Skill front-end:** `SKILL.md` + bundled helper scripts; Claude calls them
  via `Rscript`. No MCP in the loop.

## 6. Detailed spec

### 6.1 Tools

**`clean_url`** — normalize/clean one or many URLs. Exposes all 11
`get_clean_url` params (`protocol_handling`, `www_handling`, `source`,
`case_handling`, `trailing_slash_handling`, `index_page_handling`,
`path_normalization`, `scheme_relative_handling`, `subdomain_levels_to_keep`,
`host_encoding`, `path_encoding`). Ships rurl defaults verbatim. **Known v1
behavior:** rurl's `get_clean_url` drops the *entire* query string and fragment
unconditionally — e.g. `https://blog.com/?p=123` → `https://blog.com/`. This is
accepted for v1 (D5); selective handling tracked in `RURL-jtsyckxr`. **The tool
description MUST warn loudly** that query strings and fragments are removed, so
the model/user is not surprised when `?id=42`, pagination, faceted, or search
URLs collapse (review #1).

**`parse_url`** — full decomposition (D7): `original_url, scheme, host, port,
path, query, fragment, domain, tld, is_ip_host, clean_url, parse_status` +
`subdomain`. **`user` and `password` are excluded by default** (avoid leaking
credentials into the agent conversation); **default `include_credentials=false`;
pass `true` to return `user`+`password`** (review #7). Note: the parse result
has `user`/`password`, *not* a `userinfo` field (that is a separate rurl
accessor, not emitted here). Returned as structured JSON (D23).

**`extract`** — pull one or more components (D8). Allowed: `scheme, host,
subdomain, domain, tld, port, path, query, fragment`. `components` is an array,
default `["domain"]`. `source` (`all`/`icann`/`private`) applies to
`domain`/`tld`. (`domain` = registrable domain, the SEO grouping key.)

### 6.2 I/O contract (shared across all tools)

- **Input mode (D9):** `urls=[...]` → inline input; `input_csv="path"` → CSV
  input. **Exactly one is required**; supplying both, or neither, is a tool-level
  error (review #4). (`output_csv` is orthogonal — it controls *where output
  goes*, not which input mode.)
- **Output substrate (D23):** every tool returns **structured JSON** as the
  canonical result. Inline output (≤10 rows) MAY additionally include a
  human-readable rendering for the conversation, but agents consume the
  structured form.
- **Output destination (D10):** if **`output_csv` is supplied, write a CSV
  regardless of row count** (works for `urls` input too — useful for "clean
  these 5 and save them"). Else, for `urls` input: ≤10 rows → return structured
  `{input, result}` pairs; >10 rows → tool returns a **structured**
  `{status: "needs_output_csv", n_rows: N, message}` (it does NOT hold state;
  the agent re-calls with a path) (review #5, R2#2, T1a). `input_csv` always
  requires `output_csv`.
- **CSV (D11–D14):** read `input_csv` (`check.names=FALSE`); detect the URL
  column among `url`/`URL`/`address`/`Address` (override via `url_column`);
  preserve ALL columns; insert the result column(s) immediately to the right of
  the URL column; named `<url_column>_clean` / `<url_column>_<component>` /
  `<url_column>_<field>` (parse) — `<url_column>` is the detected/overridden URL
  column name, distinct from the PSL `source` arg (R2#5); write to the
  **required** `output_csv` path.
- **Transparency (D17):** for `rows="all"`, input value always present alongside
  output. `rows="distinct"` is an aggregate, so per-row input passthrough does
  not apply (by design).
- **Failures (D15):** *URL-parse* failures → `NA` in that row's output cell; the
  batch is not aborted. *Validation and I/O* failures are **tool-level errors**
  (not silent): missing/ambiguous URL column, missing `output_csv`,
  `output_csv == input_csv`, pre-existing target column name, invalid enum arg
  (`source`/`components`/`rows`), unreadable input, unwritable output, both/neither
  of `urls`/`input_csv` (review #3).
- **Dedup (D16):** `rows="all"` (default) is 1:1 with input, order preserved.
  `rows="distinct"` is an **aggregation**: it returns the distinct transformed
  values plus a `count` column (number of input rows mapping to each), which
  satisfies "count URLs per registrable domain". **Applicability:** allowed for
  `clean_url` and single-component `extract`; **rejected (tool-level error) for
  `parse_url` and multi-component `extract`**, and for multi-column CSV input
  (collapsing would discard the other columns). **`NA` handling:**
  unparseable/blank URLs collapse into a single `NA` value row carrying their
  count, so `sum(count)` equals the input row count (deterministic) (review #2,
  R2#3/#4).

### 6.2.1 CSV edge cases (review #8)

- **Multiple URL-column candidates** (e.g. both `url` and `Address` present) →
  tool-level error asking the caller to set `url_column`.
- **Encoding:** read/write UTF-8; strip a leading BOM if present.
- **`check.names=FALSE`** so columns like `Address` (Screaming Frog) survive
  unmangled.
- **`output_csv == input_csv`** → error (no in-place overwrite in v1).
- **Pre-existing target column** (`<url_column>_clean` etc. already in the file)
  → error (do not silently overwrite or proliferate `_2` suffixes).
- **Blank/`NA` URL cell** → `NA` result (row-level, per D15).
- **Relative paths** resolved against the process working directory; document
  this in the tool description.

### 6.3 Entrypoint & client config

- Single `server.R` registers the tools and calls `mcptools::mcp_server()`.
- **stdout hygiene (D25 — hard requirement, review impl):** stdio MCP is
  corrupted by ANY non-protocol byte on stdout. `server.R` must
  `suppressPackageStartupMessages()` all loads, route every `message()`/warning/
  diagnostic to **stderr**, and ensure no helper `print()`/`cat()` leaks to
  stdout. Add a smoke check that the server's stdout contains only valid MCP
  frames.
- JSON clients (Claude Desktop, Claude Code, Cursor, VS Code, Zed, Windsurf) —
  one snippet:

  ```json
  { "mcpServers": { "rurl": { "command": "Rscript", "args": ["/path/to/rurl-mcp/server.R"] } } }
  ```

- Codex CLI (TOML, `~/.codex/config.toml`):

  ```toml
  [mcp_servers.rurl]
  command = "Rscript"
  args = ["/path/to/rurl-mcp/server.R"]
  ```

- Or `claude mcp add rurl -- Rscript /path/to/server.R` / `codex mcp add …` /
  "point your agent at this repo and ask it to set up the MCP server."
- Docs must flag prominently: **command is `Rscript`; R + packages must be
  installed first** (unusual in an npx/uvx ecosystem).

### 6.4 Install

`install.sh` (mac/Linux) + `install.ps1` (Windows), each minimal:
1. Detect R; if absent, print the platform-appropriate way to get it and stop
   (do NOT silently install R).
2. `Rscript -e 'install.packages(c("rurl","mcptools","ellmer"), repos="https://cloud.r-project.org")'`
   — **`repos=` is required** or some R installs prompt interactively (review impl).
3. Print the absolute path to `server.R` (the one value the next step needs).

No client-config editing. Parked: a `.mcpb` Desktop Extension bundle (Claude
Desktop only; bundling an R runtime is unproven).

### 6.5 Skill

- `SKILL.md` (triggers + parameter suggestions + recipes) + bundled helper `.R`
  scripts (the shared §5 layer). Claude reads `SKILL.md`, runs the helper via
  `Rscript`, reports back. No MCP required.
- Recipes: (1) clean URLs → CSV; (2) dedup a list by registrable domain
  (`extract domain` + `rows="distinct"`); (3) normalize two lists for matching
  (lead-in to future `canonical_join`). Plus a "which tool for which ask"
  routing note.
- Triggers: SEO URL-wrangling phrases ("clean these URLs", "extract domains
  from this CSV", "dedup this list", "normalize these URLs for matching"),
  scoped tight.

### 6.6 Post-v1: `canonical_join`

File-output tool over `rurl::canonical_join()`: two CSV inputs + key columns,
canonicalized-URL join, writes a CSV. Designed but not in v1.

## 7. Use-case catalog (v1)

| # | Use case | Tool | Key args |
|---|---|---|---|
| 1 | Clean URLs | `clean_url` | the 11 rurl params |
| 2 | Extract registrable domains | `extract` | `components=["domain"]`, `source` |
| 3 | Extract hosts | `extract` | `components=["host"]` |
| 4 | Extract TLDs | `extract` | `components=["tld"]`, `source` |
| 5 | Extract subdomains | `extract` | `components=["subdomain"]` |
| 6 | Full parse | `parse_url` | — |
| 7 | Canonical join | `canonical_join` | two inputs + key cols *(post-v1)* |

"Count URLs per registrable domain" = #2 + `rows="distinct"` (distinct domains
**+ `count`** — the aggregation shape per D16). "Normalize two lists to match" =
#1 on both sides → lead-in to #7. No dedicated tools for these.

## 8. Repo scaffolding

- **License:** **MIT — confirmed** (`DESCRIPTION:14`); matches rurl.
- **Versioning:** SemVer from `0.1.0`; git tags so directories can pin.
- **Version pinning (D24):** declare a minimum `rurl (>= <current CRAN>)`; the
  smoke test **asserts `get_clean_url`'s 11 expected formals** so wrapper
  behavior can't silently drift when rurl updates on CRAN (review #10).
- **Tests:** `testthat` (standalone, run via `Rscript`) on the shared helper
  functions (CSV column detect, insert-adjacent, `rows` dedup/aggregation,
  inline overflow, validation/error paths, `NA`) — that's where bugs hide; thin
  wrappers and rurl itself are not re-tested. Plus a boot/smoke test that starts
  the server, checks stdout is protocol-clean (D25), and calls each tool.
- **CI:** GitHub Actions (install R + deps → tests → smoke) on **macOS +
  Ubuntu + a Windows install/script smoke** (since `install.ps1` is in scope —
  review impl). No `R CMD check`/matrix (not a package).

## 9. Distribution (registries)

Epic `RURL-nzlihhfz` with one child per destination: official MCP registry,
Smithery, Glama, mcp.so, PulseMCP, awesome-mcp-servers PR. A **base `server.json`
manifest** (SEO-keyword description + tags `seo`/`url`/`domain`/`csv`) is the
starting point, but **registry schemas differ** — expect per-destination
adaptation rather than a single reused file (review impl). **Caveat:**
hosted-runner registries (esp. Smithery) lack R — list as "local/self-hosted,
requires R"; do not depend on their runner.

## 10. Open questions / deferred

- **RURL-jtsyckxr** — selective query-param stripping (utm-only vs all);
  rurl-side vs wrapper-side. Likely resolved rurl-side by 1.5.0.
- `server.json` exact schema per each registry's current spec (verify at
  submission time; schemas differ across registries).
- Whether the skill ships in the `rurl-mcp` repo or its own (current: same repo).

**Resolved by review (2026-06-27, Codex pass):** license confirmed MIT;
`clean_url` query-drop accepted for v1 (description must warn); `rows="distinct"`
redefined as distinct+`count` aggregation; failure semantics split
(row-level `NA` for parse vs tool-level errors for validation/I/O); I/O-mode
conflict rules; structured-output substrate; `parse_url` credential exclusion;
CSV edge cases; stdout hygiene; `repos=`; version-formals assertion; Windows CI.

**Resolved by review pass 2 (R2):** `include_credentials` wording fixed
(default `false`; opt-in returns `user`+`password`; no `userinfo` field);
`output_csv` forces CSV regardless of row count and is orthogonal to input mode;
`rows="distinct"` applicability narrowed (allowed: `clean_url`, single-component
`extract`; rejected: `parse_url`, multi-component `extract`, multi-column CSV);
deterministic `NA` aggregation (single `NA` bucket row, counts sum to input);
result-column naming renamed `<url_column>_*` to disambiguate from PSL `source`;
ticket breakdown refreshed to inherit the new contract.

## 11. Proposed ticket breakdown (after review)

1. Repo scaffold: `rurl-mcp` skeleton, MIT license, `0.1.0`, CI stub.
2. Shared helper `.R` (inherits the full §6.2 contract): CSV column detect
   (incl. multi-candidate error), insert-adjacent write with `<url_column>_*`
   naming + collision error, `rows="all"` vs `distinct` **aggregation+`count`**
   (with applicability + `NA`-bucket rules), structured `needs_output_csv`
   overflow, `output_csv`-forces-CSV rule, validation/error paths, CSV edge
   cases (BOM/UTF-8, `check.names=FALSE`, `output==input`), row-level `NA` —
   with `testthat` tests covering each.
3. `server.R`: register `clean_url` / `parse_url` / `extract` via `ellmer::tool`
   (structured output; `clean_url` loud query-drop warning; `parse_url`
   `include_credentials=false` default); `mcptools::mcp_server()`; **stdout
   hygiene (D25)**; tool descriptions.
4. Install scripts (`install.sh` / `install.ps1`).
5. Docs: `llms.txt` (canonical) + README mirror; client-config snippets (JSON +
   Codex TOML); `Rscript` prerequisite callout.
6. Claude skill: `SKILL.md` + bundled helper scripts; 3 recipes + routing note.
7. Boot/smoke test.
8. `server.json` manifest.
9. Registries epic `RURL-nzlihhfz` (per-destination children).
10. (post-v1) `canonical_join` file tool.
