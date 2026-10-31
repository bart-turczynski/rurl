# check-bugreports v2 (seor adaptation of pslr's v2, PSLR-cvhksjst). v2 requires
# /-/work_items in DESCRIPTION too, the opposite of v1 and of the other
# packages' copies until SEOR-sptibjph reaches them: never re-port from those.
# seor's copy also carries the SEOR-jjblyiyd hardening (own tracker address,
# missing files, empty BugReports:, http:// and subgroup links, no seor-only
# paths, the package block's @seealso) and the SEOR-sgfvtzps one (the legacy
# /issues path only where it ends, any host case or www. in every pattern,
# no .git in a tracker link, non-UTF-8 files read); the members' ports
# predate both.
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

The package man page's "Report bugs at" link is not a separate decision.
`man/<pkg>-package.Rd` (`<pkg>` is `DESCRIPTION`'s `Package:`) is generated
from the R/ file holding roxygen2's `"_PACKAGE"` sentinel, which writes a
"Useful links" section verbatim from `DESCRIPTION`'s own `URL:`/`BugReports:`
fields. If that link is present it MUST agree with `DESCRIPTION`, or
`devtools::document()` was not re-run.

roxygen2 emits that generated section only when the package doc has no
`@seealso` of its own. seor's `R/seor-package.R` declares none, so
`man/seor-package.Rd` carries the line and it is compared. Its absence is
accepted only while an `@seealso` in the `"_PACKAGE"` block itself
suppresses it (pslr's shape).

WHAT IT CHECKS.

1. `DESCRIPTION`'s `BugReports:` is the `/-/work_items` address,
   optionally with a trailing slash, of the gitlab.com repository that
   `DESCRIPTION`'s `URL:` names, and `codemeta.json`'s `"issueTracker"`
   is the same address (the host's case aside). Repositories compare
   case-insensitively and the host may be spelt `GitLab.com`, but
   `BugReports:` and `URL:` may not use `www.gitlab.com`: it answers with
   a 308 permanent redirect, which CRAN's URL check reports as moved. A
   tracker link in another file may use it. `URL:` may name the repository
   with a `.git` suffix (a clone address); `BugReports:` and every tracker
   link may not, as `.../<project>.git/-/work_items` is not the canonical
   address.
2. `man/<pkg>-package.Rd`'s "Report bugs at" link, when present, is
   IDENTICAL to `DESCRIPTION`'s `BugReports:` value; when absent, the
   `"_PACKAGE"` file declares the `@seealso` that suppresses it.
3. Every file that carries the tracker link exists, names this package's own
   `/-/work_items` address (the repository `BugReports:` and `URL:` agree
   on; another project's tracker may appear beside it, never instead), and
   none of them carries a `/-/issues` link. That ban covers `http://` and
   `https://`, a subgroup project, any host case, `www.gitlab.com`, and the
   legacy path without `/-/` (`.../<project>/issues`, which redirects to
   `/-/issues`) wherever its `issues` segment ends (`/new`, `/42/notes`,
   `/service_desk`, a query, a fragment, a quote or punctuation). A subgroup
   project named `issues` (`.../group/sub/issues/-/...`) and a folder or
   file (`.../blob/main/issues.md`) in a `blob/`, `tree/`, `raw/` or
   `wikis/` path are not that path; a subgroup project named `issues`
   linked bare (`.../group/sub/issues`) is indistinguishable from it and is
   flagged:
     - codemeta.json (`"issueTracker"`)
     - .bestpractices.json
     - CONTRIBUTING.md (where to report a bug)
     - SECURITY.md (the confidential-issue channel)
4. No other human-facing surface carries a `/-/issues` link. These files
   name no tracker URL today, so the check forbids the old form without
   requiring the new one; requiring it would force a link nobody asked for
   into them:
     - README.Rmd, README.md, SECURITY-INSIGHTS.yml, _pkgdown.yml,
       vignettes/*.Rmd, .github/**, .gitlab/**

Every file is read as UTF-8 with an undecodable byte replaced, so a Latin-1
DESCRIPTION (R allows `Encoding: latin1`) or man page is checked, not a
traceback: every address compared is ASCII.

WHAT IT DOES NOT CHECK, ON PURPOSE.

* `NEWS.md` is a point-in-time record of what was announced. A historical
  bullet that quotes an old `/-/issues` form, including one arguing FOR that
  form, is not drift, so it is unchecked.
* `cran-comments.md` is unchecked too. seor's is the draft for its first
  submission, and it must quote the incoming check's NOTE, which proposes an
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

# A gitlab.com project path: a namespace, any subgroups, then the project.
# A GitLab path segment cannot start with "-", so "/-/" ends the path.
_SEGMENT = r"[\w.][\w.-]*"
_PATH = rf"/{_SEGMENT}(?:/{_SEGMENT})+"
# Hosts are case-insensitive, so every pattern takes any case; repositories
# are then compared by _repo_key(). www.gitlab.com answers with a 308
# permanent redirect to gitlab.com: a link a reader follows may use it, but
# CRAN's URL check reports a DESCRIPTION address on it as moved, so
# BugReports: and URL: may not.
_HOST = r"(?i:https://gitlab\.com)"
_LINK_HOST = r"(?i:https://(?:www\.)?gitlab\.com)"
WWW_RE = re.compile(r"https?://www\.gitlab\.com\b", re.IGNORECASE)

# A repository-content path (a file or folder in blob/, tree/ or raw/, a
# wiki page) is not the project path, so it stops the legacy /issues match.
_CONTENT = r"(?!(?:blob|tree|raw|wikis)/)"
_ISSUES_PATH = rf"/{_SEGMENT}(?:/{_CONTENT}{_SEGMENT})+"

# Over http:// too: it redirects to the same 404ing https:// address. The
# legacy path without "/-/" redirects to /-/issues; it counts wherever the
# "issues" segment ends (any character but a word character or "-"),
# except where it is a subgroup project named "issues" (issues/-/...) or a
# file or folder in the repository's content (blob/main/docs/issues.md).
ISSUES_RE = re.compile(
    rf"https?://(?:www\.)?gitlab\.com{_ISSUES_PATH}/"
    r"(?:-/issues(?![\w-])|issues(?![\w-]|/-/))",
    re.IGNORECASE,
)
# Group 1 is the repository the tracker belongs to.
WORK_ITEMS_RE = re.compile(rf"({_LINK_HOST}{_PATH})/-/work_items\b")

# DESCRIPTION's BugReports: value, in full, must match this.
BUGREPORTS_RE = re.compile(rf"^({_HOST}{_PATH})/-/work_items/?$")
# A gitlab.com repository address in DESCRIPTION's URL: field.
REPO_URL_RE = re.compile(rf"({_HOST}/{_SEGMENT}(?:/{_SEGMENT})+?)/?(?=[\s,]|$)")
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

# The line roxygen2 documents as the package itself, a trailing comment
# (`"_PACKAGE" # nolint`) allowed.
PACKAGE_SENTINEL_RE = re.compile(r"""^\s*["']_PACKAGE["']\s*(?:#.*)?$""", re.MULTILINE)


def _repo_key(repo: str, url_field: bool = False) -> str:
    """A repository address as compared and shown: case-folded, on
    gitlab.com without www., without a trailing "/". A ".git" suffix goes
    only from `URL:`'s repository (`url_field`), where a clone address is
    fine; on a tracker link it is not the canonical address."""
    repo = repo.rstrip("/").casefold().replace("://www.", "://", 1)
    if url_field and repo.endswith(".git"):
        repo = repo[: -len(".git")]
    return repo


def _address_key(address: str) -> str:
    """An address as codemeta.json is compared with BugReports:: the scheme
    and host case-folded, the path exact, without a trailing "/"."""
    origin, path = re.match(r"((?:[^/]*//)?[^/]*)(.*)", address.rstrip("/")).groups()
    return origin.casefold() + path


def _read(path: Path) -> str:
    """A file's text, a byte that is not UTF-8 replaced. R allows a Latin-1
    DESCRIPTION (`Encoding: latin1`) and man page; every address compared
    here is ASCII, so a replaced byte cannot change a finding, and a
    non-ASCII byte inside an address leaves a value that fails to match."""
    return path.read_text(encoding="utf-8", errors="replace")


def read_dcf(path: Path) -> dict[str, str]:
    """Flat DCF fields, joining RFC-822 style continuation lines."""
    fields: dict[str, str] = {}
    key: str | None = None
    for line in _read(path).splitlines():
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


def package_doc_source(root: Path) -> tuple[Path, str] | None:
    """The R/ file holding the "_PACKAGE" sentinel (R/<pkg>-package.R by
    usethis's convention, but any file name works for roxygen2), with its
    text as read, a byte that is not UTF-8 replaced."""
    for path in sorted((root / "R").glob("*.[Rr]")):
        text = _read(path)
        if PACKAGE_SENTINEL_RE.search(text):
            return path, text
    return None


def package_block(text: str) -> str:
    """The roxygen block documenting "_PACKAGE": the #' lines above the
    sentinel, climbing over blank lines and plain # comments and stopping at
    the first code line. Only an @seealso there suppresses the generated
    "Report bugs at" line; one on another object's block does not."""
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if PACKAGE_SENTINEL_RE.match(line):
            start = index
            while start > 0:
                above = lines[start - 1].strip()
                if above and not above.startswith("#"):
                    break
                start -= 1
            block = (line.lstrip() for line in lines[start:index])
            return "\n".join(line for line in block if line.startswith("#'"))
    return ""


def check_repo(root: Path) -> list[str]:
    """Findings for one repository; empty when every tracker link agrees."""
    errors: list[str] = []

    description = root / "DESCRIPTION"
    if not description.exists():
        return ["DESCRIPTION is missing; nothing to check BugReports against"]
    desc = read_dcf(description)
    # No BugReports: field is a separate question, not drift here; the checks
    # that compare against it are skipped, the /-/issues ban still runs.
    bugreports = desc.get("BugReports", "")

    # The package's own repository: BugReports:'s, when URL: names it too;
    # otherwise whatever gitlab.com repository URL: names. Repositories are
    # compared, and shown, by _repo_key().
    url_repos = {_repo_key(repo, url_field=True) for repo in REPO_URL_RE.findall(desc.get("URL", ""))}
    match = BUGREPORTS_RE.match(bugreports)
    bug_repo = _repo_key(match.group(1)) if match is not None else None
    own = {bug_repo} if bug_repo in url_repos else url_repos
    own_label = ", ".join(f"{repo}/-/work_items" for repo in sorted(own)) or "none in URL:"

    url_www = WWW_RE.search(desc.get("URL", ""))
    if url_www:
        errors.append(
            "DESCRIPTION URL: names a repository on www.gitlab.com, which "
            "answers with a 308 permanent redirect that CRAN's URL check "
            "reports as moved. Name it on gitlab.com."
        )
    # A www. URL: is reported above; that its repository is not derivable is
    # the same finding.
    if not own and not url_www:
        errors.append(
            "DESCRIPTION's URL: names no gitlab.com repository that BugReports: "
            "agrees with, so this package's own tracker can't be derived and no "
            "file's /-/work_items link can be checked against it. Add the "
            "repository to URL:."
        )

    if bugreports and match is None and WWW_RE.match(bugreports):
        errors.append(
            f"DESCRIPTION BugReports: is '{bugreports}', on www.gitlab.com, "
            f"which answers with a 308 permanent redirect that CRAN's URL "
            f"check reports as moved. Name it on gitlab.com."
        )
    elif bugreports and match is None:
        errors.append(
            f"DESCRIPTION BugReports: is '{bugreports}', which is not a "
            f"gitlab.com /-/work_items address. /-/issues 404s for CRAN's URL "
            f"check, and CRAN accepts /-/work_items (Uwe Ligges, 2026-10-23; "
            f"SEOR-sptibjph)."
        )
    # BugReports: alone shows a .git suffix, whatever URL: names.
    elif match is not None and bug_repo.endswith(".git"):
        errors.append(
            f"DESCRIPTION BugReports: is '{bugreports}', which names the "
            f"repository with a .git suffix. Name it as "
            f"'{_repo_key(match.group(1), url_field=True)}/-/work_items', the "
            f"canonical address."
        )
    # With no repository in URL: at all, a finding above already says so.
    elif match is not None and url_repos and bug_repo not in url_repos:
        errors.append(
            f"DESCRIPTION BugReports: is '{bugreports}', but URL: names no "
            f"repository '{match.group(1)}'. The tracker must be this "
            f"package's own."
        )

    codemeta = root / "codemeta.json"
    if bugreports and codemeta.exists():
        tracker = ISSUE_TRACKER_RE.search(_read(codemeta))
        if tracker is None or _address_key(tracker.group(1)) != _address_key(bugreports):
            found = tracker.group(1) if tracker else "nothing"
            errors.append(
                f"codemeta.json issueTracker is '{found}', but DESCRIPTION's "
                f"BugReports: is '{bugreports}'. codemeta.json is hand-"
                f"maintained; make it name the same address."
            )

    package = desc.get("Package", "")
    man_rel = f"man/{package}-package.Rd"
    man_path = root / man_rel
    if package and man_path.exists():
        text = _read(man_path)
        match = re.search(r"Report bugs at \\url\{([^}]*)\}", text)
        pkg_doc = package_doc_source(root)
        has_seealso = pkg_doc is not None and re.search(
            r"^#'\s*@seealso\b", package_block(pkg_doc[1]), re.MULTILINE
        )
        # roxygen2 generates the line only from a BugReports: value.
        if match is None and bugreports and pkg_doc is None:
            errors.append(
                f"{man_rel} has no 'Report bugs at' link, and no R/ file "
                f"holds the \"_PACKAGE\" sentinel whose @seealso could "
                f"suppress it -- document the package with \"_PACKAGE\" and "
                f"re-run devtools::document()."
            )
        elif match is None and bugreports and not has_seealso:
            errors.append(
                f"{man_rel} has no 'Report bugs at' link, and "
                f"{pkg_doc[0].relative_to(root).as_posix()} declares no "
                f"@seealso that suppresses it -- the man page is stale. Re-run "
                f"devtools::document()."
            )
        elif match is not None and match.group(1) != bugreports:
            errors.append(
                f"{man_rel}'s 'Report bugs at' link is "
                f"'{match.group(1)}', but DESCRIPTION's BugReports: is "
                f"'{bugreports}'. This link is generated verbatim from "
                f"DESCRIPTION by roxygen2's \"_PACKAGE\" sentinel -- re-run "
                f"devtools::document(), do not hand-edit the .Rd."
            )

    for name in HUMAN_FACING_FILES:
        path = root / name
        if not path.is_file():
            errors.append(
                f"{name} is missing; it must exist and name this package's "
                f"own tracker ({own_label})."
            )
            continue
        text = _read(path)
        if ISSUES_RE.search(text):
            errors.append(
                f"{name} still links a /-/issues form, which 404s for a "
                f"non-browser client. Repoint it at /-/work_items."
            )
        # Another project's tracker may appear beside this package's own,
        # never instead of it. With no own tracker known, that is reported
        # once above, not here per file.
        trackers = {_repo_key(repo) for repo in WORK_ITEMS_RE.findall(text)}
        if own and not trackers & own:
            errors.append(
                f"{name} does not name the project's /-/work_items tracker "
                f"link ({own_label}) anywhere -- expected this package's own "
                f"tracker URL."
            )

    # A file already checked above is not reported a second time by the
    # glob pass.
    seen: set[Path] = {root / name for name in HUMAN_FACING_FILES}
    for pattern in NO_ISSUES_GLOBS:
        for path in sorted(root.glob(pattern)):
            if path in seen or not path.is_file():
                continue
            seen.add(path)
            text = _read(path)
            if ISSUES_RE.search(text):
                errors.append(
                    f"{path.relative_to(root)} links a /-/issues form, which "
                    f"404s for a non-browser client. Use /-/work_items."
                )

    return errors


# --- self-test (positive + negative coverage, executable) --------------------
#
# The fixture is package-neutral: it names its own package ("fixture" unless a
# case says otherwise) and writes that package's own man/<pkg>-package.Rd and
# R/<pkg>-package.R, so a copy ported to another package tests the shape it
# runs on, not seor's.

# Cases that pin a known gap still open in check_repo(). A listed case must
# fail; one that passes fails the self-test, so a fix has to drop its tag.
XFAIL: frozenset[str] = frozenset()


def _fixture(
    directory: Path,
    bugreports: str,
    man_url: str | None,
    human: dict[str, str],
    seealso: str = "package",
    url: str = "https://gitlab.com/bart-turczynski/fixture",
    package: str = "fixture",
    r_encoding: str = "utf-8",
    latin1: tuple[str, ...] = (),
) -> Path:
    """A throwaway package; `seealso` picks the R/<pkg>-package.R shape.

    "package" puts an @seealso in the "_PACKAGE" block (it suppresses the
    generated "Report bugs at" line), "other" puts one only in another
    object's block (it suppresses nothing), "none" declares no @seealso.
    "package-blank" and "package-comment" separate the block from the
    sentinel by a blank line or a plain # comment, "package-trailing-comment"
    puts a # comment after the sentinel, "package-accented" carries a
    non-ASCII character (write it with `r_encoding` "latin-1" to get a byte
    that is not UTF-8), and "no-sentinel" documents no "_PACKAGE" at all.
    Each file named in `latin1` (DESCRIPTION, the man page as "man", or a
    human-facing file) gains a line with a Latin-1 byte that is not UTF-8.
    """
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "DESCRIPTION").write_text(
        f"Package: {package}\nVersion: 0.1.0\nURL: {url}\nBugReports: {bugreports}\n",
        encoding="utf-8",
    )
    # Spelled out here, not taken from check_repo()'s own path logic, so the
    # fixture states the roxygen2 file-name convention independently.
    man_dir = directory / "man"
    man_dir.mkdir(parents=True, exist_ok=True)
    if man_url is not None:
        rd = f"\\seealso{{\nReport bugs at \\url{{{man_url}}}\n}}\n"
    else:
        rd = "\\seealso{\nThe package website.\n}\n"
    (man_dir / f"{package}-package.Rd").write_text(rd, encoding="utf-8")
    r_dir = directory / "R"
    r_dir.mkdir(parents=True, exist_ok=True)
    package_block = {
        "package": "#' The package.\n#'\n#' @seealso The package website.\n\"_PACKAGE\"\n",
        "other": (
            "#' The package.\n\"_PACKAGE\"\n\n"
            "#' A helper.\n#' @seealso [base::print()]\nhelper <- function() NULL\n"
        ),
        "none": "#' The package.\n\"_PACKAGE\"\n",
        "package-blank": "#' The package.\n#'\n#' @seealso The package website.\n\n\"_PACKAGE\"\n",
        "package-comment": "#' The package.\n#'\n#' @seealso The package website.\n# nolint\n\"_PACKAGE\"\n",
        "package-trailing-comment": "#' The package.\n#'\n#' @seealso The package website.\n\"_PACKAGE\" # nolint\n",
        "package-accented": "#' The package, by Café Ltd.\n#'\n#' @seealso The package website.\n\"_PACKAGE\"\n",
        "no-sentinel": "#' A helper.\nhelper <- function() NULL\n",
    }[seealso]
    (r_dir / f"{package}-package.R").write_text(package_block, encoding=r_encoding)
    for name, content in human.items():
        path = directory / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")
    for name in latin1:
        path = man_dir / f"{package}-package.Rd" if name == "man" else directory / name
        with path.open("ab") as handle:
            handle.write("Maintainer-Note: Café Ltd.\n".encode("latin-1"))
    return directory


def self_test() -> None:
    org = "https://gitlab.com/bart-turczynski/fixture"
    issues = f"{org}/-/issues"
    work_items = f"{org}/-/work_items"
    foreign = "https://gitlab.com/someone/else/-/work_items"

    failures: list[str] = []
    xpassed: list[str] = []
    ran: set[str] = set()
    counts = {"positive": 0, "negative": 0, "xfail": 0}

    def run(tag: str, **kwargs) -> list[str]:
        ran.add(tag)
        with tempfile.TemporaryDirectory() as tmp:
            # A crash is a finding too, so one case's traceback cannot hide
            # every case after it.
            try:
                return check_repo(_fixture(Path(tmp) / tag, **kwargs))
            except Exception as exc:  # noqa: BLE001
                return [f"check_repo() raised {exc!r}"]

    def record(tag: str, ok: bool, detail: str) -> None:
        if tag in XFAIL:
            if ok:
                xpassed.append(tag)
            else:
                counts["xfail"] += 1
        elif not ok:
            failures.append(f"{tag}: {detail}")

    def expect_clean(tag: str, **kwargs) -> None:
        counts["positive"] += 1
        found = run(tag, **kwargs)
        record(tag, not found, f"expected clean, got {found}")

    def expect_flagged(tag: str, needle: str, only: bool = False, **kwargs) -> None:
        """`only`: the needle's finding must be the sole one."""
        counts["negative"] += 1
        found = run(tag, **kwargs)
        ok = any(needle in f for f in found) and (len(found) == 1 or not only)
        record(tag, ok, f"expected {'only ' if only else ''}{needle!r}, got {found}")

    human_target = {
        "codemeta.json": f'{{"issueTracker": "{work_items}"}}',
        ".bestpractices.json": f'{{"report_url_justification": "Bug tracker: {work_items}"}}',
        "CONTRIBUTING.md": f"Report bugs at <{work_items}>.",
        "SECURITY.md": f"Or open a confidential issue at <{work_items}>.",
        "README.Rmd": "Install from CRAN.",
        "README.md": "Install from CRAN.",
    }

    def with_file(name: str, text: str | None) -> dict[str, str]:
        """human_target with one file replaced, or dropped when `text` is None."""
        human = dict(human_target)
        if text is None:
            del human[name]
        else:
            human[name] = text
        return human

    # POSITIVE: every tracker link names /-/work_items (the "Report bugs at"
    # line generated into the package's man page is present).
    expect_clean("all-work-items", bugreports=work_items, man_url=work_items, human=human_target)

    # POSITIVE: no "Report bugs at" line because the package doc's own
    # @seealso suppresses the generated section.
    expect_clean("man-suppressed-by-seealso", bugreports=work_items, man_url=None, human=human_target)

    # POSITIVE: a trailing slash is the same address.
    expect_clean("trailing-slash", bugreports=f"{work_items}/", man_url=None, human=human_target)

    # POSITIVE: so is a trailing slash on URL:'s repository address.
    expect_clean(
        "url-trailing-slash",
        bugreports=work_items,
        man_url=work_items,
        human=human_target,
        url=f"{org}/",
    )

    # POSITIVE: a file may name another project's tracker beside this
    # package's own (a metapackage pointing at a member's, say).
    expect_clean(
        "own-plus-foreign-work-items",
        bugreports=work_items,
        man_url=work_items,
        human=with_file("CONTRIBUTING.md", f"Report bugs at <{work_items}>; member bugs at <{foreign}>."),
    )

    # POSITIVE: a package whose repository sits in a GitLab subgroup.
    sub = "https://gitlab.com/some-group/sub.group/fixture"
    expect_clean(
        "subgroup-package",
        bugreports=f"{sub}/-/work_items",
        man_url=f"{sub}/-/work_items",
        human={name: text.replace(org, sub) for name, text in human_target.items()},
        url=sub,
    )

    # POSITIVE: no BugReports: at all is a separate question, not drift here;
    # the own address then comes from URL:. With no @seealso either, the man
    # page rightly has no "Report bugs at" line: roxygen2 writes none.
    expect_clean("empty-bugreports-clean", bugreports="", man_url=None, human=human_target, seealso="none")

    # POSITIVE: the same checks hold for a package of any name.
    expect_clean(
        "other-package-name",
        bugreports=work_items,
        man_url=work_items,
        human=human_target,
        package="otherpkg",
    )

    # POSITIVE: the package block's @seealso still counts across a blank line
    # or a plain # comment before the sentinel, and with a comment after it.
    for shape in ("package-blank", "package-comment", "package-trailing-comment"):
        expect_clean(f"seealso-{shape}", bugreports=work_items, man_url=None, human=human_target, seealso=shape)

    # POSITIVE: an R/ file with a byte that is not UTF-8 (Latin-1 here).
    expect_clean(
        "latin1-package-file",
        bugreports=work_items,
        man_url=None,
        human=human_target,
        seealso="package-accented",
        r_encoding="latin-1",
    )

    # POSITIVE: URL: names the repository with an owner in another case, a
    # .git suffix and a trailing slash; it is still this package's own.
    expect_clean(
        "url-case-dotgit",
        bugreports=work_items,
        man_url=work_items,
        human=human_target,
        url="https://gitlab.com/Bart-Turczynski/fixture.git/",
    )

    # POSITIVE: an /issues path on another host, and a mixed-case host on the
    # /-/work_items form, are not /-/issues links.
    expect_clean(
        "issues-elsewhere-clean",
        bugreports=work_items,
        man_url=work_items,
        human=with_file(
            "README.md",
            "Upstream bugs: <https://github.com/someone/else/issues/new>; ours: "
            "<https://GitLab.com/bart-turczynski/fixture/-/work_items>.",
        ),
    )

    # NEGATIVE: the line is absent and nothing suppresses it any more.
    expect_flagged(
        "man-missing-without-seealso",
        "declares no @seealso",
        bugreports=work_items,
        man_url=None,
        human=human_target,
        seealso="none",
    )

    # NEGATIVE: an @seealso in another object's block suppresses nothing in
    # the package's own man page.
    expect_flagged(
        "seealso-in-other-block",
        "declares no @seealso",
        bugreports=work_items,
        man_url=None,
        human=human_target,
        seealso="other",
    )

    # NEGATIVE: no R/ file holds the "_PACKAGE" sentinel at all.
    expect_flagged(
        "no-sentinel",
        'no R/ file holds the "_PACKAGE" sentinel',
        bugreports=work_items,
        man_url=None,
        human=human_target,
        seealso="no-sentinel",
    )

    # NEGATIVE: no BugReports: and no gitlab.com repository in URL:, so the
    # package's own tracker is unknown; any project's must not pass for it.
    expect_flagged(
        "own-tracker-underivable",
        "own tracker can't be derived",
        bugreports="",
        man_url=None,
        human=human_target,
        seealso="none",
        url="https://example.org/fixture",
    )

    # NEGATIVE: a tracker copied from another package.
    expect_flagged(
        "foreign-tracker",
        "URL: names no repository",
        bugreports=foreign,
        man_url=None,
        human=human_target,
    )

    # NEGATIVE: codemeta.json names another /-/work_items address than
    # DESCRIPTION's.
    expect_flagged(
        "codemeta-differs",
        "codemeta.json issueTracker is",
        bugreports=work_items,
        man_url=None,
        human=with_file("codemeta.json", f'{{"issueTracker": "{foreign}"}}'),
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

    # NEGATIVE: the same, in a package not named "fixture": the man page
    # checked is that package's own.
    expect_flagged(
        "other-package-name-man-stale",
        "man/otherpkg-package.Rd's 'Report bugs at' link",
        bugreports=work_items,
        man_url=issues,
        human=human_target,
        package="otherpkg",
    )

    # NEGATIVE: a human-facing file reverted to /-/issues (the "prove it
    # bites" case this gate exists for).
    expect_flagged(
        "contributing-reverted",
        "still links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human=with_file("CONTRIBUTING.md", f"Report bugs at <{issues}/new>."),
    )

    # NEGATIVE: a human-facing file never names /-/work_items at all.
    expect_flagged(
        "codemeta-missing-work-items",
        "does not name the project's /-/work_items",
        bugreports=work_items,
        man_url=work_items,
        human=with_file("codemeta.json", '{"issueTracker": ""}'),
    )

    # NEGATIVE: a human-facing file names only another project's tracker.
    expect_flagged(
        "contributing-foreign-work-items",
        f"CONTRIBUTING.md does not name the project's /-/work_items tracker link ({work_items})",
        bugreports=work_items,
        man_url=work_items,
        human=with_file("CONTRIBUTING.md", f"Report bugs at <{foreign}>."),
    )

    # NEGATIVE: a required human-facing file is missing.
    expect_flagged(
        "contributing-missing",
        "CONTRIBUTING.md is missing",
        bugreports=work_items,
        man_url=work_items,
        human=with_file("CONTRIBUTING.md", None),
    )

    # NEGATIVE: SECURITY.md keeps its /-/work_items link but gains a
    # /-/issues one beside it.
    expect_flagged(
        "security-issues-link",
        "SECURITY.md still links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human=with_file("SECURITY.md", human_target["SECURITY.md"] + f" Or [a confidential issue]({issues}/new)."),
    )

    # NEGATIVE: with no BugReports:, the /-/issues ban still runs.
    expect_flagged(
        "empty-bugreports-issues-link",
        "README.md links a /-/issues form",
        bugreports="",
        man_url=None,
        human=with_file("README.md", f"Report problems at <{issues}>."),
    )

    # NEGATIVE: a surface with no tracker link today (a vignette) gains a
    # /-/issues link.
    expect_flagged(
        "vignette-issues-link",
        "vignettes/getting-started.Rmd links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human={**human_target, "vignettes/getting-started.Rmd": f"Report problems at <{issues}>."},
    )

    # NEGATIVE: the link in backticks, before a full stop, to one issue, over
    # plain http://, in a GitLab subgroup, on a mixed-case or www. host, and
    # on the legacy path without "/-/"; earlier patterns missed each.
    for tag, text in (
        ("backticks", f"See `{issues}`."),
        ("full-stop", f"Report problems at {issues}."),
        ("one-issue", f"Fixed in {issues}/42 already."),
        ("http", f"Report problems at <{issues.replace('https://', 'http://')}>."),
        ("subgroup", "Report problems at <https://gitlab.com/some-group/sub.group/fixture/-/issues>."),
        ("mixed-case-host", "Report problems at <https://GitLab.com/bart-turczynski/fixture/-/issues>."),
        ("www-host", "Report problems at <https://www.gitlab.com/bart-turczynski/fixture/-/issues>."),
        ("legacy-path", "Report problems at <https://gitlab.com/bart-turczynski/fixture/issues/new>."),
    ):
        expect_flagged(
            f"issues-link-{tag}",
            "README.md links a /-/issues form",
            bugreports=work_items,
            man_url=work_items,
            human=with_file("README.md", text),
        )

    # NEGATIVE: a GitLab template under .gitlab/ gains a /-/issues link.
    expect_flagged(
        "gitlab-template-issues-link",
        ".gitlab/issue_templates/Bug.md links a /-/issues form",
        bugreports=work_items,
        man_url=work_items,
        human={**human_target, ".gitlab/issue_templates/Bug.md": f"Search {issues} first."},
    )

    # NEGATIVE: the legacy path without "/-/" ending the URL, at a sentence's
    # full stop, or followed by one issue, a query or a fragment.
    for tag, suffix in (
        ("end", "issues."),
        ("trailing-slash", "issues/ today"),
        ("one-issue", "issues/42"),
        ("query", "issues?state=opened"),
        ("fragment", "issues#top"),
    ):
        expect_flagged(
            f"legacy-issues-{tag}",
            "README.md links a /-/issues form",
            bugreports=work_items,
            man_url=work_items,
            human=with_file("README.md", f"See https://gitlab.com/bart-turczynski/fixture/{suffix}"),
        )

    # NEGATIVE: the legacy path followed by a character no ASCII delimiter
    # list foresees: smart quotes, an ellipsis, an HTML entity, a note on an
    # issue, the service desk.
    for tag, text in (
        ("smart-quotes", "See “https://gitlab.com/bart-turczynski/fixture/issues”."),
        ("ellipsis", "See https://gitlab.com/bart-turczynski/fixture/issues..."),
        ("html-entity", 'See <a href="https://gitlab.com/bart-turczynski/fixture/issues&amp;x=1">'),
        ("issue-notes", "See https://gitlab.com/bart-turczynski/fixture/issues/42/notes"),
        ("service-desk", "See https://gitlab.com/bart-turczynski/fixture/issues/service_desk"),
    ):
        expect_flagged(
            f"legacy-issues-{tag}",
            "README.md links a /-/issues form",
            bugreports=work_items,
            man_url=work_items,
            human=with_file("README.md", text),
        )

    # POSITIVE: a folder or file named "issues" in a repository path, under
    # blob/, tree/, raw/ or wikis/, with or without "/-/", is not the legacy
    # /issues path.
    expect_clean(
        "legacy-issues-repo-paths",
        bugreports=work_items,
        man_url=work_items,
        human=with_file(
            "README.md",
            "\n".join(
                f"See <https://gitlab.com/bart-turczynski/fixture/{path}>."
                for path in (
                    "blob/main/issues",
                    "tree/main/issues/",
                    "raw/main/docs/issues",
                    "wikis/issues",
                    "-/blob/main/issues",
                    "-/tree/main/issues/",
                )
            ),
        ),
    )

    # POSITIVE: a subgroup project named "issues", and a repository file
    # named issues.md, are not the legacy /issues path.
    expect_clean(
        "legacy-issues-lookalikes",
        bugreports=work_items,
        man_url=work_items,
        human=with_file(
            "README.md",
            "Tracker of a sibling: <https://gitlab.com/some-group/sub/issues/-/work_items>. "
            "Notes: <https://gitlab.com/bart-turczynski/fixture/blob/main/docs/issues.md>.",
        ),
    )

    # POSITIVE: a work_items link on a mixed-case or www. host is the same
    # tracker, as it is for the /-/issues ban.
    for tag, host in (("mixed-case", "https://GitLab.com"), ("www", "https://www.gitlab.com")):
        expect_clean(
            f"work-items-{tag}-host",
            bugreports=work_items,
            man_url=work_items,
            human=with_file(
                "CONTRIBUTING.md",
                f"Report bugs at <{host}/bart-turczynski/fixture/-/work_items>.",
            ),
        )

    # POSITIVE: URL: names the repository on a mixed-case host.
    expect_clean(
        "url-mixed-case-host",
        bugreports=work_items,
        man_url=work_items,
        human=human_target,
        url="https://GitLab.com/bart-turczynski/fixture",
    )

    # NEGATIVE: URL: or BugReports: on www.gitlab.com, which answers with a
    # 308 permanent redirect that CRAN's URL check reports as moved.
    www = "https://www.gitlab.com/bart-turczynski/fixture"
    expect_flagged(
        "url-www-host",
        "www.gitlab.com",
        only=True,
        bugreports=work_items,
        man_url=work_items,
        human=human_target,
        url=www,
    )
    expect_flagged(
        "bugreports-www-host",
        "www.gitlab.com",
        only=True,
        bugreports=f"{www}/-/work_items",
        man_url=f"{www}/-/work_items",
        human=with_file("codemeta.json", f'{{"issueTracker": "{www}/-/work_items"}}'),
    )

    # POSITIVE: BugReports: on a mixed-case host. The man page repeats it
    # verbatim (roxygen2 copies it); codemeta.json may spell the host in
    # lower case.
    upper = "https://GitLab.com/bart-turczynski/fixture/-/work_items"
    expect_clean(
        "bugreports-mixed-case-host",
        bugreports=upper,
        man_url=upper,
        human=human_target,
    )

    # NEGATIVE: codemeta.json differing from BugReports: in the path's case
    # is another address.
    expect_flagged(
        "codemeta-path-case",
        "codemeta.json issueTracker is",
        bugreports=work_items,
        man_url=work_items,
        human=with_file(
            "codemeta.json",
            '{"issueTracker": "https://gitlab.com/Bart-Turczynski/fixture/-/work_items"}',
        ),
    )

    # NEGATIVE: BugReports: with a .git suffix is not the canonical address,
    # though URL: may carry one.
    dotgit = f"{org}.git/-/work_items"
    expect_flagged(
        "bugreports-dotgit",
        "a .git suffix",
        bugreports=dotgit,
        man_url=dotgit,
        human=with_file("codemeta.json", f'{{"issueTracker": "{dotgit}"}}'),
    )

    # NEGATIVE: the same, with no gitlab.com repository in URL: to compare it
    # against; BugReports: alone shows the suffix.
    expect_flagged(
        "bugreports-dotgit-no-gitlab-url",
        "a .git suffix",
        bugreports=dotgit,
        man_url=dotgit,
        human=with_file("codemeta.json", f'{{"issueTracker": "{dotgit}"}}'),
        url="https://example.org/fixture",
    )

    # NEGATIVE: a file naming the tracker only with a .git suffix.
    expect_flagged(
        "contributing-dotgit-work-items",
        "CONTRIBUTING.md does not name the project's /-/work_items",
        bugreports=work_items,
        man_url=work_items,
        human=with_file("CONTRIBUTING.md", f"Report bugs at <{dotgit}>."),
    )

    # POSITIVE: DESCRIPTION, the man page and a human-facing file with a byte
    # that is not UTF-8 (R allows `Encoding: latin1`).
    for name in ("DESCRIPTION", "man", "CONTRIBUTING.md"):
        expect_clean(
            f"latin1-{name.lower().replace('.', '-')}",
            bugreports=work_items,
            man_url=work_items,
            human=human_target,
            latin1=(name,),
        )

    # NEGATIVE: URL: names no gitlab.com repository; reported once, not
    # again as a BugReports: mismatch.
    expect_flagged(
        "own-tracker-underivable-once",
        "own tracker can't be derived",
        only=True,
        bugreports=work_items,
        man_url=work_items,
        human=human_target,
        url="https://example.org/fixture",
    )

    # NEGATIVE: the own tracker shown in a finding is the canonical address,
    # without URL:'s .git suffix or its owner's capitals.
    expect_flagged(
        "own-label-canonical",
        f"CONTRIBUTING.md does not name the project's /-/work_items tracker link ({work_items})",
        bugreports="",
        man_url=None,
        seealso="none",
        human=with_file("CONTRIBUTING.md", f"Report bugs at <{foreign}>."),
        url="https://gitlab.com/Bart-Turczynski/fixture.git",
    )

    unknown = sorted(XFAIL - ran)
    if failures or xpassed or unknown:
        lines = [f"  - {failure}" for failure in failures]
        lines += [f"  - {tag}: passes but is listed in XFAIL; drop it" for tag in xpassed]
        lines += [f"  - {tag}: listed in XFAIL but names no case; drop it" for tag in unknown]
        raise SystemExit("check-bugreports self-test FAILED:\n" + "\n".join(lines))
    pending = f", {counts['xfail']} expected failures pending" if counts["xfail"] else ""
    print(
        f"check-bugreports self-test: PASS ({counts['positive']} positive + "
        f"{counts['negative']} negative cases{pending})"
    )


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
