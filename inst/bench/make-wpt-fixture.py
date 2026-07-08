#!/usr/bin/env python3
"""Derive the committed WHATWG conformance fixture from WPT urltestdata.json.

Source: web-platform-tests `url/resources/urltestdata.json`
(BSD-3-Clause, "web-platform-tests contributors"). This script extracts the
subset the standard-parity harness scores and writes it, with provenance, to
`inst/bench/wpt-url-cases.json`. It is committed so a WHATWG-conformance number
is reproducible without a network fetch; regenerate when the upstream suite is
refreshed.

Filter:
  * base in {null, about:blank}  -- absolute parse, no relative resolution;
  * success cases limited to the schemes rurl supports (http/https/ftp/file) --
    the "additional protocols notwithstanding" carve-out;
  * failure cases: any base-null case (all schemes -- a reject is a reject);
  * inputs containing NUL are dropped (not round-trippable through R).

    python3 inst/bench/make-wpt-fixture.py _scratch/urltestdata.json
"""
import datetime
import json
import sys

SUPP = {"http:", "https:", "ftp:", "file:"}
src = json.load(open(sys.argv[1], encoding="utf-8"))

success, failure = [], []
for e in src:
    if not isinstance(e, dict):
        continue
    if e.get("base") not in (None, "about:blank"):
        continue
    if "\x00" in e.get("input", ""):
        continue
    if e.get("failure"):
        failure.append({"input": e["input"]})
    elif e.get("protocol") in SUPP:
        success.append({k: e.get(k, "") for k in (
            "input", "protocol", "hostname", "port", "pathname",
            "search", "hash")})

out = {
    "_meta": {
        "source": "web-platform-tests url/resources/urltestdata.json",
        "license": "BSD-3-Clause (web-platform-tests contributors)",
        "extracted": datetime.date.today().isoformat(),
        "filter": ("base in {null, about:blank}; success limited to "
                   "http/https/ftp/file; failure = any base-null case; "
                   "NUL inputs dropped"),
        "counts": {"success": len(success), "failure": len(failure)},
    },
    "success": success,
    "failure": failure,
}
with open("inst/bench/wpt-url-cases.json", "w", encoding="utf-8") as fh:
    json.dump(out, fh, ensure_ascii=False, indent=1)
sys.stderr.write(f"success={len(success)} failure={len(failure)}\n")
