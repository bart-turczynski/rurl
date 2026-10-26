# check-bugreports v2 (ported from seor main, SEOR-sptibjph; seor adapted
# pslr's v2, PSLR-cvhksjst). v2 requires /-/work_items in DESCRIPTION too, the
# opposite of v1: re-port only from seor's copy, never from an older one.
"""BugReports / tracker-link gate.

WHY THIS EXISTS. CRAN runs two checks over a GitLab `BugReports:` URL and,
on gitlab.com, they contradict each other. `tools::check_url_db()` actually
fetches the address; GitLab moved issue reporting to work items, so the
classic `.../-/issues` path 404s for a client that does not send
`Accept: text/html` (the exact client CRAN's check uses) while
`.../-/work_items` serves 200. `tools:::.check_package_CRAN_incoming()`
never fetches anything -- it string-tests the path and flags anything but
`/issues`, `/-/work_items` included, with a "which should likely be"
NOTE proposing an `/issues` path (R 4.6.1 proposes
`.../-/work_items/issues`). No URL satisfies both.

The fleet used to keep `/-/issues` in DESCRIPTION
and `/-/work_items` everywhere else (SEOR-ocbtrrnl). pslr's 1.2.2
pretest then drew the 404 NOTE, and on 2026-10-23 Uwe Ligges (CRAN) called
the "should likely be" NOTE a bug in CRAN's check for GitLab and invited the
working form: "Please feel free to use the working form such as
https://gitlab.com/bart-turczynski/pslr/-/work_items and we will let it pass
and fix our checks." So every file, DESCRIPTION included, now names
`/-/work_items`, and none names `/-/issues` (SEOR-sptibjph, ADR 0011).

`man/rurl-package.Rd`'s "Report bugs at" link is not a separate decision:
`R/rurl-package.R` uses roxygen2's `"_PACKAGE"` sentinel, which generates a
"Useful links" section verbatim from `DESCRIPTION`'s own `URL:`/`BugReports:`
fields. If that link is present it MUST agree with `DESCRIPTION`, or
`devtools::document()` was not re-run.

roxygen2 emits that generated section only when the package doc has no
`@seealso` of its own. `R/rurl-package.R` declares one (pslr's shape), so
`man/rurl-package.Rd` carries no such line. Its absence is accepted only
while that `@seealso` suppresses it; if the line comes back, it is compared.

WHAT IT CHECKS.

1. `DESCRIPTION`'s `BugReports:` is the `/-/work_items` address,
   optionally with a trailing slash, of the gitlab.com repository that
   `DESCRIPTION`'s `URL:` names, and `codemeta.json`'s `"issueTracker"`
   is the same address.
2. `man/rurl-package.Rd`'s "Report bugs at" link, when present, is
   IDENTICAL to `DESCRIPTION`'s `BugReports:` value; when absent,
   `R/rurl-package.R` declares the `@seealso` that suppresses it.
3. Every file that carries the tracker link names `/-/work_items`, and none
   of them carries a `/-/issues` link:
     - codemeta.json (`"issueTracker"`)
     - .bestpractices.json
     - CONTRIBUTING.md (where to report a bug)
     - SECURITY.md (the confidential-issue channel)
4. No other human-facing surface carries a `/-/issues` link. These files
   name no tracker URL today, so the check forbids the old form without
   requiring the new one; requiring it would force a link nobody asked for
   into them:
     - README.Rmd, README.md, SECURITY-INSIGHTS.yml, _pkgdown.yml,
       vignettes/*.Rmd, .github/**

WHAT IT DOES NOT CHECK, ON PURPOSE.

* `NEWS.md` is a point-in-time record of what was announced. A historical
  bullet that quotes an old `/-/issues` form, including one arguing FOR that
  form, is not drift, so it is unchecked.
* `cran-comments.md` is unchecked too. It records what each submission
  said, and it must quote the incoming check's NOTE, which proposes an
  `/issues` path, verbatim.
* Nothing here touches the network. Whether a URL currently resolves is a
  fact about the rest of the world; `R CMD check --as-cran` already fetches
  declared URLs, and a network call in a pre-push gate fails on a train.

Stdlib only, so it runs in any bare Python CI image, same shape as
`scripts/check-citation.py` (which this file does not import from or
modify -- that script is owned by a separate, fleet-wide dedup decision).

    python3 scripts/check-bugreports.py              # exit 1 on drift
    python3 scripts/check-bugreports.py --self-test  # positive/negative cases
"""

from __future__ import annotations

import re
import sys
import tempfile
from pathlib import Path

ISSUES_RE = re.compile(
    r"https://gitlab\.com/[\w.-]+/[\w.-]+/-/issues(?![\w-])"
)
WORK_ITEMS_RE = re.compile(r"https://gitlab\.com/[\w.-]+/[\w.-]+/-/work_items\b")

# DESCRIPTION's BugReports: value, in full, must match this.
BUGREPORTS_RE = re.compile(r"^(https://gitlab\.com/[\w.-]+/[\w.-]+)/-/work_items/?$")
# A gitlab.com repository address in DESCRIPTION's URL: field.
REPO_URL_RE = re.compile(r"(https://gitlab\.com/[\w.-]+/[\w.-]+?)/?(?=[\s,]|$)")
ISSUE_TRACKER_RE = re.compile(r'"issueTracker"\s*:\s*"([^"]*)"')

# Files beside DESCRIPTION that carry the tracker link; each must name
# /-/work_items.
HUMAN_FACING_FILES = (
    "codemeta.json",
    ".bestpractices.json",
    "CONTRIBUTING.md",
    "SECURITY.md",
)

# Human-facing surfaces that link no tracker today: /-/issues is forbidden,
# /-/work_items is not required. Globs, relative to the repository root.
NO_ISSUES_GLOBS = (
    "README.Rmd",
    "README.md",
    "SECURITY-INSIGHTS.yml",
    "_pkgdown.yml",
    "vignettes/*.Rmd",
    ".github/**/*",
    ".gitlab/**/*",
)

PACKAGE_RD = Path("man") / "rurl-package.Rd"
PACKAGE_R = Path("R") / "rurl-package.R"


def read_dcf(path: Path) -> dict[str, str]:
    """Flat DCF fields, joining RFC-822 style continuation lines."""
    fields: dict[str, str] = {}
    key: str | None = None
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            key = None
            continue
        if line[0].isspace():
            if key is not None:
                fields[key] += " " + line.strip()
            continue
        name, sep, value = line.partition(":")
        if not sep:
            key = None
            continue
        key = name.strip()
        fields[key] = value.strip()
    return fields


def check_repo(root: Path) -> list[str]:
    """Findings for one repository; empty when every tracker link agrees."""
    errors: list[str] = []

    description = root / "DESCRIPTION"
    if not description.exists():
        return ["DESCRIPTION is missing; nothing to check BugReports against"]
    desc = read_dcf(description)
    bugreports = desc.get("BugReports", "")
    if not bugreports:
        return []  # No BugReports: field is a separate question, not drift here.

    match = BUGREPORTS_RE.match(bugreports)
    if match is None:
        errors.append(
            f"DESCRIPTION BugReports: is '{bugreports}', which is not a "
            f"gitlab.com /-/work_items address. /-/issues 404s for CRAN's URL "
            f"check, and CRAN accepts /-/work_items (Uwe Ligges, 2026-10-23; "
            f"SEOR-sptibjph)."
        )
    elif match.group(1) not in REPO_URL_RE.findall(desc.get("URL", "")):
        errors.append(
            f"DESCRIPTION BugReports: is '{bugreports}', but URL: names no "
            f"repository '{match.group(1)}'. The tracker must be this "
            f"package's own."
        )

    codemeta = root / "codemeta.json"
    if codemeta.exists():
        tracker = ISSUE_TRACKER_RE.search(codemeta.read_text(encoding="utf-8"))
        if tracker is None or tracker.group(1).rstrip("/") != bugreports.rstrip("/"):
            found = tracker.group(1) if tracker else "nothing"
            errors.append(
                f"codemeta.json issueTracker is '{found}', but DESCRIPTION's "
                f"BugReports: is '{bugreports}'. codemeta.json is hand-"
                f"maintained; make it name the same address."
            )

    man_path = root / PACKAGE_RD
    if man_path.exists():
        text = man_path.read_text(encoding="utf-8")
        match = re.search(r"Report bugs at \\url\{([^}]*)\}", text)
        pkg_r = root / PACKAGE_R
        has_seealso = pkg_r.exists() and re.search(
            r"^#'\s*@seealso\b", pkg_r.read_text(encoding="utf-8"), re.MULTILINE
        )
        if match is None and not has_seealso:
            errors.append(
                "man/rurl-package.Rd has no 'Report bugs at' link, and "
                "R/rurl-package.R declares no @seealso that "
                "suppresses it -- the man page is stale. Re-run "
                "devtools::document()."
            )
        elif match is not None and match.group(1) != bugreports:
            errors.append(
                f"man/rurl-package.Rd's 'Report bugs at' link is "
                f"'{match.group(1)}', but DESCRIPTION's BugReports: is "
                f"'{bugreports}'. This link is generated verbatim from "
                f"DESCRIPTION by roxygen2's \"_PACKAGE\" sentinel -- re-run "
                f"devtools::document(), do not hand-edit the .Rd."
            )

    for name in HUMAN_FACING_FILES:
        path = root / name
        if not path.exists():
            continue
        text = path.read_text(encoding="utf-8")
        if ISSUES_RE.search(text):
            errors.append(
                f"{name} still links a /-/issues form, which 404s for a "
                f"non-browser client. Repoint it at /-/work_items."
            )
        if not WORK_ITEMS_RE.search(text):
            errors.append(
                f"{name} does not name the project's /-/work_items tracker "
                f"link anywhere -- expected the tracker URL."
            )

    # A file already checked above is not reported a second time by the
    # glob pass.
    seen: set[Path] = {root / name for name in HUMAN_FACING_FILES}
    for pattern in NO_ISSUES_GLOBS:
        for path in sorted(root.glob(pattern)):
            if path in seen or not path.is_file():
                continue
            seen.add(path)
            text = path.read_text(encoding="utf-8", errors="replace")
            if ISSUES_RE.search(text):
                errors.append(
                    f"{path.relative_to(root)} links a /-/issues form, which "
                    f"404s for a non-browser client. Use /-/work_items."
                )

    return errors


# --- self-test (positive + negative coverage, executable) --------------------


def _fixture(
    directory: Path,
    bugreports: str,
    man_url: str | None,
    human: dict[str, str],
    seealso: bool = True,
    url: str = "https://gitlab.com/bart-turczynski/fixture",
) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "DESCRIPTION").write_text(
        f"Package: fixture\nVersion: 0.1.0\nURL: {url}\nBugReports: {bugreports}\n",
        encoding="utf-8",
    )
    man_dir = directory / "man"
    man_dir.mkdir(parents=True, exist_ok=True)
    if man_url is not None:
        rd = f"\\seealso{{\nReport bugs at \\url{{{man_url}}}\n}}\n"
    else:
        rd = "\\seealso{\nMember list: seor_packages().\n}\n"
    (directory / PACKAGE_RD).write_text(rd, encoding="utf-8")
    r_dir = directory / "R"
    r_dir.mkdir(parents=True, exist_ok=True)
    tag = "#' @seealso\n#' Member list: [seor_packages()].\n" if seealso else ""
    (directory / PACKAGE_R).write_text(f"{tag}\"_PACKAGE\"\n", encoding="utf-8")
    for name, content in human.items():
        path = directory / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
    return directory


def self_test() -> None:
    org = "https://gitlab.com/bart-turczynski/fixture"
    issues = f"{org}/-/issues"
    work_items = f"{org}/-/work_items"

    def run(tag: str, **kwargs) -> list[str]:
        with tempfile.TemporaryDirectory() as tmp:
            return check_repo(_fixture(Path(tmp) / tag, **kwargs))

    def expect_clean(tag: str, **kwargs) -> None:
        found = run(tag, **kwargs)
        if found:
            raise SystemExit(f"self-test FAILED ({tag}): {found}")

    def expect_flagged(tag: str, needle: str, **kwargs) -> None:
        found = run(tag, **kwargs)
        if not any(needle in f for f in found):
            raise SystemExit(f"self-test FAILED ({tag}): expected {needle!r}, got {found}")

    human_target = {
        "codemeta.json": f'{{"issueTracker": "{work_items}"}}',
        ".bestpractices.json": f'{{"report_url_justification": "Bug tracker: {work_items}"}}',
        "CONTRIBUTING.md": f"Report bugs at <{work_items}>.",
        "SECURITY.md": f"Or open a confidential issue at <{work_items}>.",
        "README.Rmd": "Install from CRAN.",
        "README.md": "Install from CRAN.",
    }

    # POSITIVE: every tracker link names /-/work_items (seor's shape: the
    # generated "Report bugs at" line is present).
    expect_clean("all-work-items", bugreports=work_items, man_url=work_items, human=human_target)

    # POSITIVE: pslr's shape -- no "Report bugs at" line because the
    # package doc's own @seealso suppresses the generated section.
    expect_clean("man-suppressed-by-seealso", bugreports=work_items, man_url=None, human=human_target)

    # POSITIVE: a trailing slash is the same address.
    expect_clean("trailing-slash", bugreports=f"{work_items}/", man_url=None, human=human_target)

    # POSITIVE: so is a trailing slash on URL:'s repository address.
    expect_clean(
        "url-trailing-slash",
        bugreports=work_items,
        man_url=work_items,
        human=human_target,
        url="https://gitlab.com/bart-turczynski/fixture/",
    )

    # NEGATIVE: the line is absent and nothing suppresses it any more.
    expect_flagged(
        "man-missing-without-seealso",
        "declares no @seealso",
        bugreports=work_items,
        man_url=None,
        human=human_target,
        seealso=False,
    )

    # NEGATIVE: a tracker copied from another package.
    expect_flagged(
        "foreign-tracker",
        "URL: names no repository",
        bugreports="https://gitlab.com/someone/else/-/work_items",
        man_url=None,
        human=human_target,
    )

    # NEGATIVE: codemeta.json names another /-/work_items address than
    # DESCRIPTION's.
    drifted = dict(human_target)
    drifted["codemeta.json"] = '{"issueTracker": "https://gitlab.com/someone/else/-/work_items"}'
    expect_flagged(
        "codemeta-differs",
        "codemeta.json issueTracker is",
        bugreports=work_items,
        man_url=None,
        human=drifted,
    )

    # NEGATIVE: DESCRIPTION back on /-/issues, the form that drew the 1.2.2
    # pretest's 404 NOTE.
    expect_flagged(
        "description-issues",
        "not a gitlab.com /-/work_items address",
        bugreports=issues,
        man_url=issues,
        human=human_target,
    )

    # NEGATIVE: DESCRIPTION on /-/issues/new, the other form CRAN's incoming
    # check string-tests for.
    expect_flagged(
        "description-issues-new",
        "not a gitlab.com /-/work_items address",
        bugreports=f"{issues}/new",
        man_url=None,
        human=human_target,
    )

    # NEGATIVE: man page not regenerated after a DESCRIPTION edit.
    expect_flagged(
        "man-stale",
        "generated verbatim from",
        bugreports=work_items,
        man_url=issues,
        human=human_target,
    )

    # NEGATIVE: a human-facing file reverted to /-/issues (the "prove it
    # bites" case this gate exists for).
    reverted = dict(human_target)
    reverted["CONTRIBUTING.md"] = f"Report bugs at <{issues}/new>."
    expect_flagged(
        "contributing-reverted",
        "still links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human=reverted,
    )

    # NEGATIVE: a human-facing file never names /-/work_items at all.
    missing = dict(human_target)
    missing["codemeta.json"] = '{"issueTracker": ""}'
    expect_flagged(
        "codemeta-missing-work-items",
        "does not name the project's /-/work_items",
        bugreports=work_items,
        man_url=work_items,
        human=missing,
    )

    # NEGATIVE: SECURITY.md keeps its /-/work_items link but gains a
    # /-/issues one beside it.
    security = dict(human_target)
    security["SECURITY.md"] += f" Or [a confidential issue]({issues}/new)."
    expect_flagged(
        "security-issues-link",
        "SECURITY.md still links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human=security,
    )

    # NEGATIVE: a surface with no tracker link today (a vignette) gains a
    # /-/issues link.
    vignette = dict(human_target)
    vignette["vignettes/getting-started.Rmd"] = f"Report problems at <{issues}>."
    expect_flagged(
        "vignette-issues-link",
        "vignettes/getting-started.Rmd links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human=vignette,
    )

    # NEGATIVE: the link in backticks, before a full stop, and to one issue;
    # the old lookahead missed all three.
    for tag, text in (
        ("backticks", f"See `{issues}`."),
        ("full-stop", f"Report problems at {issues}."),
        ("one-issue", f"Fixed in {issues}/42 already."),
    ):
        inline = dict(human_target)
        inline["README.md"] = text
        expect_flagged(
            f"issues-link-{tag}",
            "README.md links a /-/issues form",
            bugreports=work_items,
            man_url=work_items,
            human=inline,
        )

    # NEGATIVE: a GitLab template under .gitlab/ gains a /-/issues link.
    template = dict(human_target)
    template[".gitlab/issue_templates/Bug.md"] = f"Search {issues} first."
    expect_flagged(
        "gitlab-template-issues-link",
        ".gitlab/issue_templates/Bug.md links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human=template,
    )

    print("check-bugreports self-test: PASS (4 positive + 14 negative cases)")


def main() -> int:
    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    root = Path(__file__).resolve().parent.parent
    errors = check_repo(root)
    if errors:
        print("check-bugreports failed:", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1
    print("check-bugreports: every tracker link names /-/work_items.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
