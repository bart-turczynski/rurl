# bestpractices-url v2
"""Turn `.bestpractices.json` into bestpractices.dev edit links, and check it.

WHY THIS EXISTS. bestpractices.dev only reads `.bestpractices.json` when the
project's repository URL is on github.com, and it never lets the file replace
an answer that is already stored (file answers carry confidence 3.5; forcing
takes 4). A GitLab-hosted project therefore gets nothing from the file, and a
project first registered through its GitHub mirror keeps the mirror's answers.
seor hit both on 2026-09-26 (SEOR-oaqnafzs).

What the site does accept from any forge is an automation-proposal link:
`/en/projects/ID/SECTION/edit?FIELD=VALUE...&overrides=*`. It pre-fills the
edit form, marks every change for review, and saves nothing until a person
clicks Save. The site answers 414 above roughly 8 KB, so the proposals are
split across several links, each opened and saved on its own.

WHAT IT DOES.

    python3 scripts/bestpractices-url.py            # print links for passing
    python3 scripts/bestpractices-url.py --open     # ...and open them
    python3 scripts/bestpractices-url.py --check    # exit 1 if site != file
    python3 scripts/bestpractices-url.py --self-test

* Only fields whose stored value differs from the file are proposed, so a
  re-run after saving prints nothing. `--all` proposes every field.
* Before printing anything it refuses a file the site would not count: a
  "URL required" criterion marked Met without a link in its justification, or
  N/A on a criterion that does not allow N/A.
* `--section silver|gold` targets another level; each link only carries the
  fields of its own section, because the site ignores the rest.
* `--field description=...` (also `name`, `license`,
  `implementation_languages`) proposes a general field alongside.
* The project id comes from the README badge unless `--project` is given.

It reads the network, unlike the pre-push checks: the live project entry
from bestpractices.dev, and the criteria list (`criteria/criteria.yml`) from
the badge app's repository, which says which criteria belong to which level
and which need a URL. Pass `--criteria FILE` to read a local copy instead.
That file is YAML; it is read with a small reader for its fixed layout, so
the script stays stdlib-only like the other scripts here, and the reader
fails loudly if the layout changes rather than guessing.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import re
import sys
import urllib.parse
import urllib.request
import webbrowser
from pathlib import Path

SITE = "https://www.bestpractices.dev"
CRITERIA_URL = (
    "https://raw.githubusercontent.com/coreinfrastructure/"
    "best-practices-badge/main/criteria/criteria.yml"
)
SECTIONS = {"passing": "0", "silver": "1", "gold": "2"}
GENERAL_FIELDS = ("name", "description", "license", "implementation_languages")
MAX_LENGTH = 6000  # the site 414s a 12.7k link; 6k parts pass (2026-09-26)
BADGE_RE = re.compile(r"bestpractices\.dev/projects/(\d+)")
URL_RE = re.compile(r"https?://")

LEVEL_LINE = re.compile(r"^- '(\d)':")
CRITERION_LINE = re.compile(r"^ {6}- ([A-Za-z0-9_]+):\s*(#.*)?$")
PROPERTY_LINE = re.compile(r"^ {10}([a-z_]+):\s*(\S.*)?$")


# --- fleet sync --------------------------------------------------------------
#
# THIS FILE IS VENDORED into every repository that carries a
# `.bestpractices.json`, and into the boilerplate R template, for the same
# reason `check-citation.py` is: each repository has to work from a fresh
# clone on its own. The digest below covers the implementation -- every byte
# below the module docstring, minus this assignment -- so the prose may differ
# per repository while any change to behaviour is caught. It is verified on
# every run, including `--self-test`, which the pre-push hook runs whenever
# this file changes. Whether the copies agree is one grep:
#
#     grep -h '^IMPLEMENTATION_DIGEST' ~/Projects/*/scripts/bestpractices-url.py | sort -u
#
# One line out means every copy is in sync. Re-bless it in all copies in the
# same change, never in one. The mechanism is check-citation.py's (SEOR-tssbiedr).
IMPLEMENTATION_DIGEST = "8812f4578a0de769"


def module_docstring_end(source: str) -> int:
    """The 1-based line on which this module's docstring ends, or 0 if none."""
    body = ast.parse(source).body
    if body:
        first = body[0]
        if (
            isinstance(first, ast.Expr)
            and isinstance(first.value, ast.Constant)
            and isinstance(first.value.value, str)
        ):
            return first.end_lineno or 0
    return 0


def implementation_source(source: str) -> str:
    """This file's bytes below the docstring, minus the digest assignment."""
    lines = source.splitlines(keepends=True)
    start = module_docstring_end(source)
    return "".join(
        line
        for line in lines[start:]
        if not line.startswith("IMPLEMENTATION_DIGEST = ")
    )


def implementation_digest(source: str) -> str:
    return hashlib.sha256(implementation_source(source).encode()).hexdigest()[:16]


def check_vendored_copy() -> list[str]:
    """Fail when this copy's implementation is not the one it claims to be."""
    found = implementation_digest(Path(__file__).resolve().read_text(encoding="utf-8"))
    if found == IMPLEMENTATION_DIGEST:
        return []
    return [
        f"vendored copy drifted: implementation digest is {found}, "
        f"IMPLEMENTATION_DIGEST records {IMPLEMENTATION_DIGEST}. Either this "
        "copy was edited without re-blessing it, or it was re-blessed without "
        "the other copies. Fix every copy in one change."
    ]


# --- criteria.yml ------------------------------------------------------------


def parse_criteria(text: str) -> dict[str, dict[str, dict]]:
    """Map level ("0", "1", "2") -> criterion name -> {na_allowed, met_url_required}.

    A name can appear at more than one level with different rules (gold
    repeats `test_invocation`), so rules are kept per level. Future and
    obsolete criteria are dropped: the edit form does not show them.
    """
    levels: dict[str, dict[str, dict]] = {}
    level = None
    current = None
    for line in text.splitlines():
        if m := LEVEL_LINE.match(line):
            level, current = m.group(1), None
            levels.setdefault(level, {})
            continue
        if m := CRITERION_LINE.match(line):
            if level is None:
                raise ValueError(f"criterion before any level: {line!r}")
            current = {"na_allowed": False, "met_url_required": False, "skip": False}
            levels[level][m.group(1)] = current
            continue
        if current is not None and (m := PROPERTY_LINE.match(line)):
            key, value = m.group(1), (m.group(2) or "").strip()
            if key in ("na_allowed", "met_url_required"):
                current[key] = value == "true"
            elif key in ("future", "obsolete") and value == "true":
                current["skip"] = True
    criteria = {
        lvl: {name: rule for name, rule in rules.items() if not rule.pop("skip")}
        for lvl, rules in levels.items()
    }
    if len(criteria.get("0", {})) < 20:
        raise ValueError(
            f"criteria.yml layout not recognized ({len(criteria.get('0', {}))} "
            "passing criteria read); update parse_criteria()"
        )
    return criteria


# --- checks and proposals ----------------------------------------------------


def lint(answers: dict, criteria: dict, section: str) -> tuple[list[str], list[str]]:
    """(problems, warnings) for one section.

    Problems are answers the site would reject or not count there. Warnings
    name fields that are no criterion at any level; the site ignores them.
    """
    known = {name for rules in criteria.values() for name in rules}
    rules = criteria.get(SECTIONS[section], {})
    problems, warnings = [], []
    for key, status in answers.items():
        if not key.endswith("_status"):
            continue
        name = key[: -len("_status")]
        if name not in known:
            warnings.append(f"{name}: not a current criterion; the site ignores it")
            continue
        rule = rules.get(name)
        if rule is None:
            continue
        just = answers.get(f"{name}_justification", "")
        if status == "Met" and rule["met_url_required"] and not URL_RE.search(just):
            problems.append(f"{name}: Met needs a URL in the justification")
        if status == "N/A" and not rule["na_allowed"]:
            problems.append(f"{name}: N/A is not allowed")
    return problems, warnings


def proposals(answers: dict, live: dict, criteria: dict, section: str,
              everything: bool) -> list[tuple[str, str]]:
    """(field, value) pairs for one section, in criteria.yml order."""
    pairs = []
    for name in criteria.get(SECTIONS[section], {}):
        if f"{name}_status" not in answers:
            continue
        for field in (f"{name}_status", f"{name}_justification"):
            if field in answers and (everything or live.get(field) != answers[field]):
                pairs.append((field, answers[field]))
    return pairs


def links(project: str, section: str, pairs: list[tuple[str, str]],
          limit: int = MAX_LENGTH) -> list[str]:
    """Split proposals into edit links no longer than `limit`.

    A status and its justification always travel in the same link.
    """
    base = f"{SITE}/en/projects/{project}/{section}/edit?"

    def build(chunk):
        return base + urllib.parse.urlencode(chunk + [("overrides", "*")])

    groups: list[list[tuple[str, str]]] = []
    for field, value in pairs:
        if groups and field.endswith("_justification") and \
                groups[-1][-1][0] == field[: -len("_justification")] + "_status":
            groups[-1].append((field, value))
        else:
            groups.append([(field, value)])
    out, chunk = [], []
    for group in groups:
        if chunk and len(build(chunk + group)) > limit:
            out.append(build(chunk))
            chunk = []
        chunk += group
        if len(build(chunk)) > limit:
            raise SystemExit(f"one answer alone exceeds {limit} chars: {group[0][0]}")
    if chunk:
        out.append(build(chunk))
    return out


def differences(answers: dict, live: dict, criteria: dict, section: str) -> list[str]:
    rules = criteria.get(SECTIONS[section], {})
    out = []
    for field, value in answers.items():
        name = re.sub(r"_(status|justification)$", "", field)
        if name in rules and live.get(field) != value:
            out.append(f"{field}: site={live.get(field)!r} file={value!r}"[:160])
    return out


# --- I/O ---------------------------------------------------------------------


def fetch(url: str) -> str:
    request = urllib.request.Request(url, headers={"User-Agent": "seor-bestpractices-url"})
    with urllib.request.urlopen(request, timeout=30) as response:
        return response.read().decode("utf-8")


def project_id(explicit: str | None) -> str:
    if explicit:
        return explicit
    for readme in ("README.Rmd", "README.md"):
        path = Path(readme)
        if path.exists() and (m := BADGE_RE.search(path.read_text(encoding="utf-8"))):
            return m.group(1)
    raise SystemExit("no bestpractices.dev badge in README; pass --project ID")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--section", choices=SECTIONS, default="passing")
    parser.add_argument("--project")
    parser.add_argument("--file", default=".bestpractices.json")
    parser.add_argument("--criteria", help="local criteria.yml instead of fetching")
    parser.add_argument("--field", action="append", default=[], metavar="KEY=VALUE")
    parser.add_argument("--all", action="store_true", help="propose unchanged fields too")
    parser.add_argument("--check", action="store_true", help="exit 1 if the site differs")
    parser.add_argument("--open", action="store_true", help="open the links in a browser")
    parser.add_argument("--max-length", type=int, default=MAX_LENGTH)
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv)
    drift = check_vendored_copy()
    if drift:
        print("\n".join(["bestpractices-url failed:"] + drift), file=sys.stderr)
        return 1
    if args.self_test:
        return self_test()

    answers = json.loads(Path(args.file).read_text(encoding="utf-8"))
    criteria = parse_criteria(
        Path(args.criteria).read_text(encoding="utf-8") if args.criteria
        else fetch(CRITERIA_URL)
    )
    problems, warnings = lint(answers, criteria, args.section)
    for warning in warnings:
        print(f"warning: {warning}", file=sys.stderr)
    if problems:
        print("\n".join([f"{args.file} would not count on the site:"] + problems),
              file=sys.stderr)
        return 1
    project = project_id(args.project)
    live = json.loads(fetch(f"{SITE}/projects/{project}.json"))

    if args.check:
        diff = differences(answers, live, criteria, args.section)
        for line in diff:
            print(line)
        print(f"{len(diff)} {args.section} field(s) differ from project {project}.",
              file=sys.stderr)
        return 1 if diff else 0

    general = []
    for item in args.field:
        key, sep, value = item.partition("=")
        if not sep or key not in GENERAL_FIELDS:
            raise SystemExit(f"--field takes one of {GENERAL_FIELDS} as KEY=VALUE")
        if args.all or live.get(key) != value:
            general.append((key, value))
    pairs = general + proposals(answers, live, criteria, args.section, args.all)
    if not pairs:
        print(f"Project {project} {args.section}: already matches {args.file}.",
              file=sys.stderr)
        return 0
    urls = links(project, args.section, pairs, args.max_length)
    for url in urls:
        print(url)
    print(f"{len(pairs)} field(s) in {len(urls)} link(s). Open each, review the "
          "highlighted fields, click Save. Then re-run with --check.", file=sys.stderr)
    if args.open:
        for url in urls:
            webbrowser.open_new_tab(url)
    return 0


# --- self-test (offline) -----------------------------------------------------

FIXTURE_CRITERIA = """\
--- !!omap
- '0': !!omap
  - Basics: !!omap
    - Basic project website content: !!omap
      - description_good:
          category: MUST
          autofill: >
            prose that must be ignored
      - contribution:
          category: MUST
          met_url_required: true
      - crypto_call:
          category: SHOULD
          na_allowed: true
      - retired_one:
          category: MUST
          obsolete: true
{padding}- '1': !!omap
  - Basics: !!omap
    - Project oversight: !!omap
      - governance:
          category: MUST
      - require_2FA: # a trailing comment
          category: SUGGESTED
      - contribution:
          category: MUST
          na_allowed: true
"""


def self_test() -> int:
    padding = "".join(
        f"      - filler_{i}:\n          category: SUGGESTED\n" for i in range(20)
    )
    criteria = parse_criteria(FIXTURE_CRITERIA.format(padding=padding))

    def expect(tag: str, condition: bool) -> None:
        if not condition:
            raise SystemExit(f"self-test FAILED ({tag})")

    passing, silver = criteria["0"], criteria["1"]
    expect("levels", "description_good" in passing and "governance" in silver)
    expect("flags", passing["contribution"]["met_url_required"]
           and passing["crypto_call"]["na_allowed"]
           and not passing["description_good"]["na_allowed"])
    expect("obsolete dropped", "retired_one" not in passing)
    expect("comment and capitals", "require_2FA" in silver)
    expect("same name, per-level rules",
           not passing["contribution"]["na_allowed"] and silver["contribution"]["na_allowed"])
    try:
        parse_criteria("- '0': !!omap\n  - x: 1\n")
        expect("unrecognized layout refused", False)
    except ValueError:
        pass

    good = {
        "description_good_status": "Met", "description_good_justification": "Clear.",
        "contribution_status": "Met",
        "contribution_justification": "https://example.org/CONTRIBUTING.md",
        "crypto_call_status": "N/A", "crypto_call_justification": "No crypto.",
        "governance_status": "Met", "governance_justification": "One maintainer.",
    }
    expect("clean file passes lint", lint(good, criteria, "passing") == ([], []))
    bad = dict(good, contribution_justification="See CONTRIBUTING.md",
               description_good_status="N/A", unknown_status="Met")
    found, warned = lint(bad, criteria, "passing")
    problems = " ".join(found)
    expect("url-required flagged", "contribution: Met needs a URL" in problems)
    expect("N/A flagged", "description_good: N/A is not allowed" in problems)
    expect("unknown only warned", "unknown" not in problems
           and any(w.startswith("unknown:") for w in warned))
    expect("rules are per section",
           lint(dict(good, contribution_status="N/A"), criteria, "silver")[0] == [])

    live = dict(good, description_good_justification="Old text.")
    changed = proposals(good, live, criteria, "passing", everything=False)
    expect("only differences", changed == [("description_good_justification", "Clear.")])
    expect("section isolation",
           all(not f.startswith("governance") for f, _ in
               proposals(good, {}, criteria, "passing", everything=True)))
    expect("silver section", [f for f, _ in proposals(good, {}, criteria, "silver", True)]
           == ["governance_status", "governance_justification",
               "contribution_status", "contribution_justification"])

    many = [(f"filler_{i}_{kind}", "x" * 150) for i in range(20)
            for kind in ("status", "justification")]
    parts = links("1", "passing", many, limit=1200)
    expect("split respects limit", len(parts) > 1 and all(len(u) <= 1200 for u in parts))
    expect("pairs stay together",
           all(u.count("_status=") == u.count("_justification=") for u in parts))
    expect("overrides on every link", all(u.endswith("&overrides=%2A") for u in parts))
    expect("nothing lost",
           sum(u.count("_status=") + u.count("_justification=") for u in parts) == 40)

    expect("differences", differences(good, live, criteria, "passing")
           == ["description_good_justification: site='Old text.' file='Clear.'"])
    here = Path(__file__).resolve().read_text(encoding="utf-8")
    lines = here.splitlines(keepends=True)
    end = module_docstring_end(here)
    expect("vendor: docstring found", end > 0)
    prose = "".join(lines[: end - 1] + ["Inserted by the self-test.\n"] + lines[end - 1:])
    expect("vendor: prose edit keeps digest",
           implementation_digest(prose) == implementation_digest(here))
    expect("vendor: code edit moves digest",
           implementation_digest(here + "\n_ = None\n") != implementation_digest(here))
    reblessed = here.replace(
        'IMPLEMENTATION_DIGEST = "' + IMPLEMENTATION_DIGEST + '"',
        'IMPLEMENTATION_DIGEST = "ffffffffffffffff"',
    )
    expect("vendor: constant found", reblessed != here)
    expect("vendor: rebless ignores own value",
           implementation_digest(reblessed) == implementation_digest(here))
    print("bestpractices-url self-test: all checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
