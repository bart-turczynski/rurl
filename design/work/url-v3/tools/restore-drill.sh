#!/usr/bin/env bash
# restore-drill.sh — G0.4 fresh-clone restore drill (RURL-zyksymya). The objective
# G0 fan-in signal: from a clone with NO local _scratch memory, reconstruct the
# durable control plane and verify it end-to-end. Runs the manifest, record, and
# transition validators plus the snapshot/baseline/tracker/single-writer checks.
#
# Usage:  design/work/url-v3/tools/restore-drill.sh <WORKDIR> [REMOTE_URL]
#   WORKDIR    empty/nonexistent scratch dir to clone into (required)
#   REMOTE_URL git remote to clone (default: the canonical GitHub remote)
#
# Exits non-zero on the first failed check. Prints "RESTORE DRILL PASSED" on
# success. Needs: git, jq, shasum, Rscript (+ R packages yaml, digest).
set -euo pipefail

WORKDIR="${1:?usage: restore-drill.sh <WORKDIR> [REMOTE_URL]}"
REMOTE="${2:-https://github.com/bart-turczynski/rurl.git}"
WS="design/work/url-v3"
BASELINE="89be90b"
TAG="v3/cp-snapshot-1"

step() { printf '\n== %s\n' "$1"; }
ok()   { printf '   OK: %s\n' "$1"; }
die()  { printf '   FAIL: %s\n' "$1" >&2; exit 1; }

if [ -e "$WORKDIR" ] && [ -n "$(ls -A "$WORKDIR" 2>/dev/null || true)" ]; then
  die "WORKDIR '$WORKDIR' exists and is not empty"
fi

step "1. Fresh clone (no local _scratch memory) from $REMOTE"
git clone -q "$REMOTE" "$WORKDIR/rurl"
cd "$WORKDIR/rurl"
[ -d _scratch ] && die "_scratch present in clone (must be gitignored)" || ok "no _scratch in clone"
ok "cloned at HEAD $(git rev-parse --short HEAD)"

step "2. Obtain the tracked control-plane snapshot binding (from main, not the tag)"
LOG="$WS/registers/snapshots.log"
[ -f "$LOG" ] || die "missing $LOG"
line="$(grep -E '^1[[:space:]]' "$LOG")" || die "no snapshot-1 line in log"
C1="$(echo "$line" | awk '{print $3}')"
tagsha="$(git rev-parse "${TAG}^{commit}")"
[ "$tagsha" = "$C1" ] && ok "tag $TAG -> $C1 (matches log)" || die "tag/log mismatch: tag=$tagsha log=$C1"

step "3. Reconstruct source baseline $BASELINE (distinct from snapshot)"
git cat-file -t "$BASELINE" >/dev/null 2>&1 && ok "baseline $BASELINE present" || die "baseline $BASELINE unreachable"
[[ "$C1" != ${BASELINE}* ]] && ok "baseline != snapshot (not conflated)" || die "baseline conflated with snapshot"

step "4. Recover + verify frozen sources (protocol, S1-S9, predecessor)"
( cd "$WS/evidence" && shasum -a 256 -c SHA256SUMS.txt >/dev/null ) && ok "11 frozen sources shasum -c OK" || die "source checksum failure"

step "5. Follow the manifest: schema version, hashes, tracker snapshot, references, single-writer"
grep -qE '^schema_version:' "$WS/manifest.yaml" || die "manifest missing schema_version"
grep -qE '^single_writer:' "$WS/manifest.yaml" || die "manifest missing single_writer declaration"
ok "manifest schema_version + single_writer present"
# tracker snapshot count matches manifest
mcount="$(grep -E '^\s*issue_count:' "$WS/manifest.yaml" | grep -oE '[0-9]+' | head -1)"
tcount="$(jq 'length' "$WS/evidence/tracker-snapshot.json")"
[ "$mcount" = "$tcount" ] && ok "tracker snapshot count $tcount matches manifest" || die "tracker count mismatch: manifest=$mcount json=$tcount"
Rscript "$WS/tools/validate-manifest.R" "$WS/manifest.yaml" | tail -1 | grep -q 'VALIDATION PASSED' && ok "validate-manifest.R PASSED" || die "manifest validation failed"

step "6. Recover register + authority-record state (schema + references + single-writer)"
Rscript "$WS/tools/validate-records.R" | tail -1 | grep -q 'VALIDATION PASSED' && ok "validate-records.R PASSED" || die "record validation failed"

step "7. Lifecycle transition + gate-propagation semantics"
Rscript "$WS/tools/validate-transitions.R" | tail -1 | grep -q 'VALIDATION PASSED' && ok "validate-transitions.R PASSED" || die "transition validation failed"

printf '\nRESTORE DRILL PASSED (baseline %s, snapshot %s -> %s)\n' "$BASELINE" "$TAG" "$C1"
