#!/usr/bin/env python3
"""External, non-R RFC-3986 reference: CPython ``urllib.parse.urlsplit``.

A genuinely separate-ecosystem baseline for the disagreement study. urlsplit is
a pure RFC-3986 *splitter* with minimal normalization: it lowercases the scheme
and (via ``.hostname``) the host, strips IPv6 brackets, and splits the
authority, but it does NOT percent-decode, resolve dot-segments, or reject
malformed authorities. That permissiveness is itself the finding when diffed
against rurl(rfc3986)'s strict host-shape gate.

Usage (invoked by ``pairwise-divergences.R``; standalone for a quick look):

    python3 cross-language-rfc.py inputs.json > python-rfc.json

Reads a JSON array of URL strings, writes a JSON array of
``{input, scheme, host, port, path, query, fragment, status}`` records with the
components the R harness compares.
"""
import json
import sys
from urllib.parse import urlsplit

with open(sys.argv[1], encoding="utf-8") as fh:
    inputs = json.load(fh)

rows = []
for u in inputs:
    rec = {"input": u, "scheme": "", "host": "", "port": "",
           "path": "", "query": "", "fragment": "", "status": "ok"}
    try:
        sp = urlsplit(u)
        rec["scheme"] = sp.scheme or ""
        rec["host"] = sp.hostname or ""
        try:
            rec["port"] = "" if sp.port is None else str(sp.port)
        except ValueError:
            rec["port"] = "<badport>"
        rec["path"] = sp.path or ""
        rec["query"] = sp.query or ""
        rec["fragment"] = sp.fragment or ""
    except Exception:  # noqa: BLE001 - urlsplit is near-total; guard anyway
        rec["status"] = "error"
    rows.append(rec)

json.dump(rows, sys.stdout, ensure_ascii=False)
