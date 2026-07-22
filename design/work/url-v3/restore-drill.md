# G0.4 — Fresh-clone restore drill (RURL-zyksymya)

The objective **G0 fan-in signal** (reconciliation §7 G0): from a clone with **no
local `_scratch` memory**, the durable control plane reconstructs and verifies
end-to-end. Depends on G0.1 (evidence), G0.2 (manifest + seal), G0.3 (schemas +
validators) — all complete.

## Run it

```sh
design/work/url-v3/tools/restore-drill.sh <EMPTY_WORKDIR> [REMOTE_URL]
# REMOTE_URL defaults to https://github.com/bart-turczynski/rurl.git
```

The script (`tools/restore-drill.sh`) clones fresh and asserts, exiting non-zero
on the first failure:

1. **Fresh clone** — no `_scratch` present (it is gitignored, so a clone cannot
   carry it); this proves the reconstruction uses tracked state only.
2. **Snapshot binding** — reads `registers/snapshots.log` line 1 from `main` (the
   log is written *after* the tag, so it lives on a descendant of C1, not at the
   tag) and verifies `git rev-parse v3/cp-snapshot-1^{commit}` equals the logged
   C1 SHA.
3. **Baseline** — `89be90b` is reachable and is **distinct** from the snapshot
   (the two are never conflated).
4. **Frozen sources** — the 11 review inputs pass `shasum -a 256 -c SHA256SUMS.txt`.
5. **Manifest** — `schema_version` + `single_writer` present; the tracker-snapshot
   issue count matches the manifest; `validate-manifest.R` passes (every recorded
   source/artifact/decision/tracker hash + the self-reference rule).
6. **Authority records** — `validate-records.R` passes (envelope + schemas +
   references + duplicate/unknown-state/hash-drift rejection classes).
7. **Lifecycle** — `validate-transitions.R` passes (transition legality +
   propagation across the five fixtures).

## Captured passing run

Executed from a genuine fresh clone of the GitHub remote (no local memory):

- **When:** 2026-07-22T19:31:19Z
- **Remote:** `https://github.com/bart-turczynski/rurl.git`
- **Clone HEAD:** `324eb1d` (main, post-G0.3)
- **Snapshot:** `v3/cp-snapshot-1` → `d139675281078d8bdc38fd40fcae034290fb75f9` (C1), matching `snapshots.log`
- **Baseline:** `89be90b` present, distinct from the snapshot
- **Sources:** 11 × `shasum -c` OK
- **Tracker snapshot:** 57 issues, matches the manifest
- **Validators:** `validate-manifest.R`, `validate-records.R`, `validate-transitions.R` — all `VALIDATION PASSED`

```
RESTORE DRILL PASSED (baseline 89be90b, snapshot v3/cp-snapshot-1 -> d139675281078d8bdc38fd40fcae034290fb75f9)
```

This satisfies the G0 restore acceptance: a fresh clone reconstructs baseline
`89be90b`, recovers the protocol / S1–S9 / predecessor / register state, and
verifies every hash, schema version, the tracker snapshot, all references, and
the single-writer declarations — without any local `_scratch` memory.
