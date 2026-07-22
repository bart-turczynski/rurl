#!/usr/bin/env bash
# export-tracker-snapshot.sh — deterministic fp tracker export for the rurl 3.0
# protocol-hardening effort. Packaged by G0.2 (RURL-qcduzpex); the primitives and
# the completeness rule are fixed by decision record P0.3 (§2).
#
# Why two primitives (not `fp issue list --format json`):
#   - `fp` has no generic `export`; `fp tree` does not accept `--format json`.
#   - `fp issue list --format json` can emit unescaped control characters that
#     break strict JSON parsers and omits per-issue comments.
# So we enumerate the subtree with `fp tree` and capture each issue (comments
# included) with `fp issue show ... --format json`, assembling one sorted array.
#
# Usage:  design/work/url-v3/tools/export-tracker-snapshot.sh [ROOT_ID] [OUT_PATH]
# Defaults: ROOT_ID=RURL-dorofzmb  OUT_PATH=design/work/url-v3/evidence/tracker-snapshot.json
#
# Tested on fp 0.24.0, jq 1.8.2. Deterministic: output is sorted by displayId.
set -euo pipefail

ROOT_ID="${1:-RURL-dorofzmb}"
OUT_PATH="${2:-design/work/url-v3/evidence/tracker-snapshot.json}"

ids_file="$(mktemp)"
trap 'rm -f "$ids_file"' EXIT

# 1. enumerate the full subtree (root + all descendants), unique + sorted
fp tree "$ROOT_ID" | grep -oE 'RURL-[a-z0-9]+' | sort -u > "$ids_file"

# 2. capture each issue with its comments; assemble one deterministic array
while read -r id; do
  fp issue show "$id" --format json
done < "$ids_file" \
  | jq -s 'sort_by(.displayId)' \
  > "$OUT_PATH"

count="$(jq 'length' "$OUT_PATH")"
echo "wrote $OUT_PATH ($count issues, root $ROOT_ID)" >&2
