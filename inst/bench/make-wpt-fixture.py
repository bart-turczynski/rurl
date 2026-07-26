#!/usr/bin/env python3
"""Derive the committed WHATWG conformance fixture from WPT urltestdata.json.

Source: web-platform-tests `url/resources/urltestdata.json`
(BSD-3-Clause, "web-platform-tests contributors"). This script extracts the
subset the standard-parity harness scores and writes it, with provenance, to
`inst/bench/wpt-url-cases.json`. It is committed so a WHATWG-conformance number
is reproducible without a network fetch; regenerate when the upstream suite is
refreshed.

Filter:
  * base = null only -- rurl is absolute-parse-only and does no relative
    resolution, so base-relative rows (including base "about:blank") are out
    of scope and excluded;
  * success cases: any base-null non-failure case, all schemes (special and
    non-special alike -- no scheme restriction);
  * failure cases: any base-null case (all schemes -- a reject is a reject);
  * inputs containing NUL are dropped (not round-trippable through R).

Provenance (P5.3 section 2.3): the emitted `_meta` block pins the upstream
project / revision / path, the retrieval date, the license, the raw-source
hash, the import + generation commands, the standard / version / section, the
claim kind, and the applicability selector. Revision and retrieval date are
inputs (flag or env var), never constants, so a re-pin edits no logic; the
raw-source hash is computed from the file given, not asserted. The
transformed-fixture hash is deliberately NOT emitted here -- it would hash the
file that carries it -- and is recorded by a sidecar instead.

RAW below is the fetched upstream `urltestdata.json`; SHA is the full 40-char
upstream commit it was fetched at; DATE is the ISO retrieval date, or `today`
for a genuinely new extraction (never for a plain regeneration of the pin):

    python3 inst/bench/make-wpt-fixture.py RAW --revision SHA --retrieved DATE

`--revision` / `--retrieved` fall back to `WPT_REVISION` / `WPT_RETRIEVED`.
"""
import argparse
import datetime
import hashlib
import json
import os
import re
import sys

PROJECT = "web-platform-tests/wpt"
UPSTREAM_PATH = "url/resources/urltestdata.json"
LICENSE = "BSD-3-Clause (web-platform-tests contributors)"
RAW_URL = "https://raw.githubusercontent.com/%s/%s/%s"
SELECTOR = ("base = null only -- base-relative rows (including base "
            "about:blank) are excluded because rurl is absolute-parse-only "
            "and does no relative resolution; success = any base-null "
            "non-failure case, all schemes, no scheme restriction; "
            "failure = any base-null failure case; NUL inputs dropped")
STANDARD = "WHATWG URL Standard"
STANDARD_VERSION = "Living Standard (unversioned); pinned by upstream_revision"
STANDARD_SECTION = ("URL parsing; URL serializing; URL class API getters "
                    "(protocol, hostname, port, pathname, search, hash)")
CLAIM_KIND = "conformance"
OUT_PATH = "inst/bench/wpt-url-cases.json"


def parse_args(argv):
    """Read the raw file plus the two pins that must not be hardcoded."""
    ap = argparse.ArgumentParser(
        description="Derive the committed WHATWG conformance fixture.")
    ap.add_argument("raw", help="fetched upstream urltestdata.json")
    ap.add_argument("--revision", default=os.environ.get("WPT_REVISION", ""),
                    help="full 40-char upstream commit the raw file came "
                         "from (env: WPT_REVISION)")
    ap.add_argument("--retrieved", default=os.environ.get("WPT_RETRIEVED", ""),
                    help="retrieval date as YYYY-MM-DD, or 'today' for a "
                         "genuinely new extraction (env: WPT_RETRIEVED)")
    args = ap.parse_args(argv)
    if not re.fullmatch("[0-9a-f]{40}", args.revision):
        ap.error("--revision (or WPT_REVISION) must be the full 40-char "
                 "upstream commit sha -- an immutable pin, not a branch")
    if not args.retrieved:
        ap.error("--retrieved (or WPT_RETRIEVED) is required: pass the "
                 "recorded ISO date to re-emit an existing pin, 'today' "
                 "only for a genuinely new extraction")
    if args.retrieved == "today":
        args.retrieved = datetime.date.today().isoformat()
    else:
        try:
            datetime.date.fromisoformat(args.retrieved)
        except ValueError:
            ap.error("--retrieved must be YYYY-MM-DD or 'today'")
    return args


args = parse_args(sys.argv[1:])
with open(args.raw, "rb") as fh:
    raw = fh.read()
raw_sha256 = hashlib.sha256(raw).hexdigest()
src = json.loads(raw.decode("utf-8"))

success, failure = [], []
for e in src:
    if not isinstance(e, dict):
        continue
    if e.get("base") is not None:
        continue
    if "\x00" in e.get("input", ""):
        continue
    if e.get("failure"):
        failure.append({"input": e["input"]})
    else:
        success.append({k: e.get(k, "") for k in (
            "input", "protocol", "hostname", "port", "pathname",
            "search", "hash")})

out = {
    "_meta": {
        "upstream_project": PROJECT,
        "upstream_revision": args.revision,
        "upstream_path": UPSTREAM_PATH,
        "retrieved": args.retrieved,
        "license": LICENSE,
        "raw_source_sha256": raw_sha256,
        "import_command": "curl -fsSL %s -o urltestdata.json" % (
            RAW_URL % (PROJECT, args.revision, UPSTREAM_PATH)),
        "generation_command": (
            "python3 inst/bench/make-wpt-fixture.py urltestdata.json "
            "--revision %s --retrieved %s" % (args.revision, args.retrieved)),
        "standard": STANDARD,
        "standard_version": STANDARD_VERSION,
        "standard_section": STANDARD_SECTION,
        "claim_kind": CLAIM_KIND,
        "applicability_selector": SELECTOR,
        "counts": {"success": len(success), "failure": len(failure)},
    },
    "success": success,
    "failure": failure,
}
with open(OUT_PATH, "w", encoding="utf-8") as fh:
    json.dump(out, fh, ensure_ascii=False, indent=1)
sys.stderr.write(f"success={len(success)} failure={len(failure)}\n")
