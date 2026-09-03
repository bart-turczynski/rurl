#!/usr/bin/env python3
"""Derive a committed WHATWG conformance fixture from WPT urltestdata.json.

Source: web-platform-tests `url/resources/urltestdata.json`
(BSD-3-Clause, "web-platform-tests contributors"). This script extracts the
subset a standard-parity harness scores and writes it, with provenance, to a
committed fixture. It is committed so a WHATWG-conformance number is
reproducible without a network fetch; regenerate when the upstream suite is
refreshed.

TWO MODES, one selector axis. Upstream's rows are partitioned by whether the
case carries a `base`, and the two halves answer different questions, so they
are extracted into different fixtures rather than one.

`--mode base-null` (the default; `inst/bench/wpt-url-cases.json`):
  * base = null only -- these are the absolute-parse rows;
  * success cases: any base-null non-failure case, all schemes (special and
    non-special alike -- no scheme restriction);
  * failure cases: any base-null case (all schemes -- a reject is a reject);
  * inputs containing NUL are dropped (not round-trippable through R).

`--mode base-relative` (`tests/testthat/fixtures/wpt-url-base-relative.json`):
  * base != null only -- the exact complement, so no upstream row is claimed by
    both fixtures and none is silently dropped by both;
  * success cases only: a base-carrying non-failure case scores REFERENCE
    RESOLUTION (resolve the reference against the base, then serialize), which
    is the property RURL-fupsemxr exists to measure. The must-fail half of the
    base-carrying rows is deliberately NOT imported here: nothing in this repo
    scores it yet, and a fixture arm no harness reads is a vacuous instrument;
  * rows whose input OR base contains NUL are dropped, same reason as above.
  * each row additionally carries upstream's `base`, without which the row is
    not scorable at all.

Success rows carry upstream's own `href` alongside the component getters.
`href` is the WHATWG *serialization* of the parsed URL -- the authoritative
full-string oracle, recorded by the standard's own suite. It must be preferred
over re-assembling a string from the components: the component dump cannot
distinguish a NULL host from an EMPTY one (both surface as `hostname: ""`),
nor an absent query/fragment from a present-but-empty one (both surface as
`""`), so any re-assembly has to GUESS the `//`, `?` and `#` delimiters and
will manufacture differences that belong to the guesser, not the parser.

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
`--out` overrides the destination, which otherwise follows `--mode`. Both flags
default to the base-null extraction, so the historical invocation above still
regenerates `inst/bench/wpt-url-cases.json` byte-for-byte.
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
SELECTOR = ("base = null only -- this fixture is the base-null half of the "
            "upstream suite; base-relative rows (including base about:blank) "
            "are imported separately as "
            "tests/testthat/fixtures/wpt-url-base-relative.json (OR-024, the "
            "exact complement on `base`), so every upstream row belongs to "
            "exactly one of the two; success = any base-null non-failure "
            "case, all schemes, no scheme restriction; failure = any "
            "base-null failure case; NUL inputs dropped")
RELATIVE_SELECTOR = (
    "base != null only -- the exact complement of the base-null import in "
    "inst/bench/wpt-url-cases.json, so every upstream row belongs to exactly "
    "one of the two and none is dropped by both; success = any base-carrying "
    "non-failure case, all schemes, no scheme restriction; the base-carrying "
    "FAILURE rows are deliberately not imported because no harness scores "
    "them; rows whose input or base contains NUL are dropped")
STANDARD = "WHATWG URL Standard"
STANDARD_VERSION = "Living Standard (unversioned); pinned by upstream_revision"
STANDARD_SECTION = ("URL parsing; URL serializing; URL class API getters "
                    "(protocol, username, password, hostname, port, "
                    "pathname, search, hash)")
RELATIVE_STANDARD_SECTION = (
    "URL parsing with a base URL (reference resolution); URL serializing; "
    "URL class API getters (protocol, username, password, hostname, port, "
    "pathname, search, hash)")
CLAIM_KIND = "conformance"
OUT_PATH = "inst/bench/wpt-url-cases.json"
RELATIVE_OUT_PATH = "tests/testthat/fixtures/wpt-url-base-relative.json"

# The per-row field set. `base` is emitted only in base-relative mode, where a
# row without it is not scorable; the base-null rows have no base to record and
# their shape is unchanged.
ROW_FIELDS = ("input", "protocol", "username", "password", "hostname", "port",
              "pathname", "search", "hash", "href")


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
    ap.add_argument("--mode", choices=("base-null", "base-relative"),
                    default="base-null",
                    help="which half of the upstream suite to extract; the "
                         "two are exact complements on `base`")
    ap.add_argument("--out", default=None,
                    help="destination path (default: follows --mode)")
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
    if args.out is None:
        args.out = (RELATIVE_OUT_PATH if args.mode == "base-relative"
                    else OUT_PATH)
    return args


args = parse_args(sys.argv[1:])
with open(args.raw, "rb") as fh:
    raw = fh.read()
raw_sha256 = hashlib.sha256(raw).hexdigest()
src = json.loads(raw.decode("utf-8"))

relative = args.mode == "base-relative"

success, failure = [], []
for e in src:
    if not isinstance(e, dict):
        continue
    if (e.get("base") is not None) != relative:
        continue
    if "\x00" in e.get("input", ""):
        continue
    if relative and "\x00" in (e.get("base") or ""):
        continue
    if e.get("failure"):
        # The base-relative import carries no failure arm; see the module
        # docstring. Skipping is stated here rather than left implicit.
        if relative:
            continue
        failure.append({"input": e["input"]})
    else:
        row = {k: e.get(k, "") for k in ROW_FIELDS}
        if relative:
            row["base"] = e["base"]
        success.append(row)

extra_flags = "" if not relative else " --mode base-relative"
counts = {"success": len(success)}
arrays = {"success": success}
if not relative:
    counts["failure"] = len(failure)
    arrays["failure"] = failure

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
            "--revision %s --retrieved %s%s" % (args.revision, args.retrieved,
                                                extra_flags)),
        "standard": STANDARD,
        "standard_version": STANDARD_VERSION,
        "standard_section": (RELATIVE_STANDARD_SECTION if relative
                             else STANDARD_SECTION),
        "claim_kind": CLAIM_KIND,
        "applicability_selector": RELATIVE_SELECTOR if relative else SELECTOR,
        "counts": counts,
    },
}
out.update(arrays)
with open(args.out, "w", encoding="utf-8") as fh:
    json.dump(out, fh, ensure_ascii=False, indent=1)
sys.stderr.write(f"success={len(success)} failure={len(failure)}\n")
