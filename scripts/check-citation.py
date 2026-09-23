# check-citation v3
"""Citation-metadata consistency gate.

WHY THIS EXISTS. `CITATION.cff` and `.zenodo.json` each duplicate two facts out
of `DESCRIPTION` - the version and the project URLs - and nothing asserted that
either copy was right. Measured across the fleet 2026-09-11, four of the seven
repositories carrying the files disagreed with `DESCRIPTION`, one of them with
itself (SEOR-lreejxat). seor's own `CITATION.cff` and `.zenodo.json` were still
pointing at `https://bart-turczynski.gitlab.io/seor/`, an address that belongs
to no project on this host and returns 403; `DESCRIPTION` and `_pkgdown.yml`
had been corrected and these two were missed, because nothing looked.

Same shape as `rurl`'s `tools/cran-comments-gate.R`, which exists because
`cran-comments.md` drifted eleven releases unnoticed. This is that check for
these two files.

WHAT IT CHECKS.

1. `CITATION.cff` `version:` equals the release the `DESCRIPTION` `Version:`
   names, per design/adr/0002-citation-urls-are-the-ones-about-this-package.md:
   `X.Y.Z.9000` names `X.Y.Z`; a package that has never released (the named
   release is `0.0.0`) carries the development version verbatim.
2. `.zenodo.json` `"version"`, same rule.
3. A package that has never released carries no `date-released` and no DOI -
   the honesty clause that makes the exception in (1) defensible.
4. Every http(s) URL those two files declare ABOUT THIS PACKAGE appears in
   `DESCRIPTION`'s `URL:`. For `.zenodo.json` that means the
   `related_identifiers` whose `relation` is self-referential and NOT the ones
   pointing at a dependency or an upstream source, which are not facts
   duplicated out of `DESCRIPTION` at all - see `SELF_REFERENTIAL_RELATIONS`
   and design/adr/0002-citation-urls-are-the-ones-about-this-package.md.

WHAT IT DOES NOT CHECK, ON PURPOSE.

* `codemeta.json` is not read here. It is generated, and its `issueTracker` is
  deliberately NOT required to equal `DESCRIPTION`'s `BugReports:`: the two
  differ by design (RURL-ahatycrd). `DESCRIPTION` keeps the legacy
  `/-/issues` form, which is what R's CRAN incoming check demands and what CRAN
  has accepted; every human-facing file, `codemeta.json` included, carries
  `/-/work_items`, the address GitLab actually serves. A gate equating the two
  would force one of them wrong.
* Nothing here touches the network. Whether a declared URL resolves is a fact
  about the rest of the world; `R CMD check --as-cran` already fetches declared
  URLs, and wiring a network call into a pre-push gate makes every push fail on
  a train.

Stdlib only, so it runs in any repository in the fleet without adding a
dependency, and in a bare Python CI image. `CITATION.cff` is read with a
deliberately small top-level-scalar reader rather than a YAML parser for the
same reason; it refuses to guess when the file does not have that shape.

    python3 scripts/check-citation.py              # exit 1 on drift
    python3 scripts/check-citation.py --self-test  # positive/negative cases
"""

from __future__ import annotations

import ast
import hashlib
import json
import re
import sys
import tempfile
from pathlib import Path

# --- fleet sync --------------------------------------------------------------
#
# THIS FILE IS VENDORED. A copy lives in all eight fleet repositories, and it is
# vendored rather than shared on purpose: each package has to stay
# self-contained, because a fresh clone's pre-commit hooks and a CRAN tarball
# cannot depend on a sibling checkout being present.
#
# The price of vendoring is silent drift, and the fleet has already paid it.
# Measured 2026-09-23, the eight copies had five distinct sha256 sums; nothing
# anywhere recorded which was correct. The digest below is what stops the next
# one being silent. It covers the IMPLEMENTATION -- every byte below the module
# docstring, minus this assignment -- so the prose above stays free to differ
# per repository, which it must (each repository's `codemeta.json` is a
# different situation, and four separate rewrites of that bullet are what
# produced the five sums), while any change to behaviour is caught.
#
# Bytes rather than a parse-tree hash on purpose: an `ast.dump()` digest would
# be hostage to the Python version running the gate, which is the same class of
# failure as SEOR-tcytizic and not one worth importing here.
#
# `main()` verifies it on every run, so the check is armed in all eight gates
# regardless of which of them pass `--self-test`. A logic change therefore
# cannot land in one repository without someone consciously re-blessing the
# digest, and because the constant is a literal, whether the fleet agrees is
# one grep:
#
#     grep -h '^IMPLEMENTATION_DIGEST' ~/Projects/*/scripts/check-citation.py | sort -u
#
# One line out means eight implementations in sync. Re-bless it in all eight
# repositories in the same change, never in one (SEOR-tssbiedr).
IMPLEMENTATION_DIGEST = "f6f0e5a8b5cb6218"


# A top-level `key: value` line in a CFF file: no leading whitespace, and not a
# block opener (`key:` with nothing after it, which starts a mapping or list).
CFF_SCALAR = re.compile(r"^([A-Za-z][A-Za-z0-9_-]*):[ \t]+(.+?)[ \t]*$")
CFF_KEY = re.compile(r"^([A-Za-z][A-Za-z0-9_-]*):")
URL_LIKE = re.compile(r"^https?://", re.IGNORECASE)

# Zenodo/DataCite `relation` values that assert something about THIS package,
# so the identifier is a fact duplicated out of DESCRIPTION and has to agree
# with it. Every other relation points at a DIFFERENT artifact -- a dependency,
# an upstream source -- which has no business in DESCRIPTION's `URL:` field.
#
# Measured across the seven repositories carrying `.zenodo.json` (2026-09-11):
# these three relations account for 10 identifiers, every one of them declared
# in DESCRIPTION; `requires`, `isRequiredBy` and `isDerivedFrom` account for 7,
# not one of them declared. 17 entries, no counterexample either way.
#
# It is an allowlist of what to CHECK rather than a denylist of what to skip,
# because the costs are not symmetric. A false positive reds this gate in every
# repository at once -- it did exactly that in five of seven, which is how this
# rule was found. A false negative only misses a duplicated URL that
# `CITATION.cff`'s own `url:` / `repository-code:` fields still cross-check.
# So an unrecognized relation is exempt rather than flagged. ADR 0002.
SELF_REFERENTIAL_RELATIONS = frozenset(
    {
        "isIdenticalTo",
        "isDocumentedBy",
        "isSupplementTo",
    }
)


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
    """The bytes of this file that define its behaviour.

    Everything up to and including the module docstring is prose and is
    excluded, as is the `IMPLEMENTATION_DIGEST` assignment itself -- otherwise
    the digest would be a hash of its own value. `ast` is used only to find
    where the docstring ends, never to hash anything, so the result does not
    move when the interpreter does.
    """
    lines = source.splitlines(keepends=True)
    start = module_docstring_end(source)
    return "".join(
        line
        for line in lines[start:]
        if not line.startswith("IMPLEMENTATION_DIGEST = ")
    )


def implementation_digest(source: str) -> str:
    """The recorded digest's counterpart: what this file actually is."""
    return hashlib.sha256(implementation_source(source).encode()).hexdigest()[:16]


def check_vendored_copy() -> list[str]:
    """Fail when this copy's implementation is not the one it claims to be."""
    here = Path(__file__).resolve()
    found = implementation_digest(here.read_text(encoding="utf-8"))
    if found == IMPLEMENTATION_DIGEST:
        return []
    return [
        f"vendored copy drifted: implementation digest is {found}, "
        f"IMPLEMENTATION_DIGEST records {IMPLEMENTATION_DIGEST}. Either this "
        f"copy was edited without re-blessing it, or it was re-blessed without "
        f"the other seven. Fix all eight in one change (SEOR-tssbiedr)."
    ]


def normalize_url(url: str) -> str:
    """Compare URLs without being defeated by a trailing slash."""
    return url.strip().rstrip("/").lower()


def release_named_by(version: str) -> str:
    """`X.Y.Z` for the development version `X.Y.Z.9000`, else the version."""
    parts = version.split(".")
    if len(parts) == 4 and parts[3].isdigit() and int(parts[3]) >= 9000:
        return ".".join(parts[:3])
    return version


def expected_citation_version(version: str) -> str:
    """The version CITATION.cff and .zenodo.json must carry. See ADR 0001."""
    release = release_named_by(version)
    if release == "0.0.0":
        # Never released: there is no release to name, so the files carry the
        # development version verbatim - and must claim no date and no DOI.
        return version
    return release


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


def read_cff(path: Path) -> tuple[dict[str, str], set[str]]:
    """Top-level scalars, plus the set of every top-level key present.

    The key set is what lets `date-released:` and `identifiers:` be detected
    whether they are scalars or block openers.
    """
    scalars: dict[str, str] = {}
    keys: set[str] = set()
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line or line[0].isspace() or line.lstrip().startswith("#"):
            continue
        key_match = CFF_KEY.match(line)
        if key_match:
            keys.add(key_match.group(1))
        scalar = CFF_SCALAR.match(line)
        if scalar:
            scalars[scalar.group(1)] = scalar.group(2).strip().strip("\"'")
    return scalars, keys


def zenodo_urls(data: dict) -> list[str]:
    """Http(s) identifiers .zenodo.json declares ABOUT THIS PACKAGE.

    A `related_identifier` whose `relation` points at a different artifact -- a
    dependency, an upstream source -- is not a fact duplicated out of
    DESCRIPTION, so it is not cross-checked against `URL:`. See
    `SELF_REFERENTIAL_RELATIONS` and ADR 0002.
    """
    out = []
    for entry in data.get("related_identifiers", []):
        if isinstance(entry, dict):
            if entry.get("relation") not in SELF_REFERENTIAL_RELATIONS:
                continue
            value = str(entry.get("identifier", ""))
            if URL_LIKE.match(value):
                out.append(value)
    return out


def check_repo(root: Path) -> list[str]:
    """Findings for one repository; empty when the metadata agrees."""
    errors: list[str] = []

    description = root / "DESCRIPTION"
    if not description.exists():
        return ["DESCRIPTION is missing; nothing to check citation data against"]
    desc = read_dcf(description)
    version = desc.get("Version")
    if not version:
        return ["DESCRIPTION has no `Version:` field"]
    expected = expected_citation_version(version)
    never_released = release_named_by(version) == "0.0.0"
    declared_urls = {
        normalize_url(u)
        for u in re.split(r"[,\s]+", desc.get("URL", ""))
        if URL_LIKE.match(u)
    }

    cff_path = root / "CITATION.cff"
    zenodo_path = root / ".zenodo.json"
    if not cff_path.exists() and not zenodo_path.exists():
        # Absence is a separate question (should this package have them at
        # all?), open on SEOR-lreejxat. It is not drift.
        return []

    found_urls: list[tuple[str, str]] = []

    if cff_path.exists():
        scalars, keys = read_cff(cff_path)
        if "version" not in scalars:
            errors.append(
                "CITATION.cff has no top-level `version:` scalar. Either it is "
                "absent or this gate's reader does not understand the file; "
                "both need a human."
            )
        elif scalars["version"] != expected:
            errors.append(
                f"CITATION.cff says version {scalars['version']}; DESCRIPTION "
                f"is {version}, which names release {expected} "
                f"(design/adr/0002-citation-urls-are-the-ones-about-this-package.md)."
            )
        if never_released:
            for claim in ("date-released", "doi", "identifiers"):
                if claim in keys:
                    errors.append(
                        f"CITATION.cff carries `{claim}:` while DESCRIPTION is "
                        f"{version}, a version that has never been released. "
                        f"There is no release date and no deposit to point at."
                    )
        for key in ("url", "repository-code", "repository", "repository-artifact"):
            if key in scalars and URL_LIKE.match(scalars[key]):
                found_urls.append((f"CITATION.cff `{key}:`", scalars[key]))

    if zenodo_path.exists():
        try:
            data = json.loads(zenodo_path.read_text(encoding="utf-8"))
        except json.JSONDecodeError as exc:
            errors.append(f".zenodo.json is not valid JSON: {exc}")
            data = None
        if isinstance(data, dict):
            if "version" not in data:
                errors.append(".zenodo.json has no `version` key.")
            elif data["version"] != expected:
                errors.append(
                    f".zenodo.json says version {data['version']}; DESCRIPTION "
                    f"is {version}, which names release {expected} "
                    f"(design/adr/0002-citation-urls-are-the-ones-about-this-package.md)."
                )
            if never_released and "doi" in data:
                errors.append(
                    f".zenodo.json carries a `doi` while DESCRIPTION is "
                    f"{version}, a version that has never been released."
                )
            for url in zenodo_urls(data):
                found_urls.append((".zenodo.json related identifier", url))

    for where, url in found_urls:
        if normalize_url(url) not in declared_urls:
            errors.append(
                f"{where} is {url}, which DESCRIPTION's `URL:` field does not "
                f"declare. One of the two is wrong, and only DESCRIPTION is "
                f"checked by `R CMD check --as-cran`."
            )

    return errors


# --- self-test (positive + negative coverage, executable) --------------------


def _fixture(
    directory: Path,
    version: str,
    cff: str | None,
    zenodo: dict | None,
    url: str = "https://example.org/docs, https://example.org/repo",
) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "DESCRIPTION").write_text(
        f"Package: fixture\nVersion: {version}\nURL: {url}\n", encoding="utf-8"
    )
    if cff is not None:
        (directory / "CITATION.cff").write_text(cff, encoding="utf-8")
    if zenodo is not None:
        (directory / ".zenodo.json").write_text(
            json.dumps(zenodo), encoding="utf-8"
        )
    return directory


def self_test() -> None:
    released_cff = (
        'cff-version: 1.2.0\ntype: software\nversion: 3.0.1\n'
        'date-released: "2026-09-09"\n'
        'url: "https://example.org/docs"\n'
        'repository-code: "https://example.org/repo"\n'
    )
    unreleased_cff = (
        'cff-version: 1.2.0\ntype: software\nversion: 0.0.0.9000\n'
        'url: "https://example.org/docs"\n'
    )

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
            raise SystemExit(
                f"self-test FAILED ({tag}): expected {needle!r}, got {found}"
            )

    # POSITIVE: a development cycle still cites the release it descends from.
    expect_clean(
        "dev-cycle",
        version="3.0.1.9000",
        cff=released_cff,
        zenodo={"version": "3.0.1"},
    )
    # POSITIVE: release preparation, DESCRIPTION already at the release.
    expect_clean(
        "at-release",
        version="3.0.1",
        cff=released_cff,
        zenodo={"version": "3.0.1"},
    )
    # POSITIVE: the never-released exception - verbatim, no date, no DOI.
    expect_clean(
        "never-released",
        version="0.0.0.9000",
        cff=unreleased_cff,
        zenodo={"version": "0.0.0.9000"},
    )
    # POSITIVE: a package with neither file is not in violation.
    expect_clean("absent", version="0.1.2.9000", cff=None, zenodo=None)
    # POSITIVE: a related identifier pointing at a DIFFERENT artifact is not a
    # fact duplicated out of DESCRIPTION, so `URL:` must not have to declare
    # it. This is the measured fleet defect that produced ADR 0002: `requires`,
    # `isRequiredBy` and `isDerivedFrom` pointers at dependencies and upstream
    # sources reddened this gate in five of seven repositories.
    expect_clean(
        "cross-artifact-relations",
        version="3.0.1.9000",
        cff=released_cff,
        zenodo={
            "version": "3.0.1",
            "related_identifiers": [
                {
                    "identifier": "https://CRAN.R-project.org/package=pslr",
                    "relation": "requires",
                },
                {
                    "identifier": "https://CRAN.R-project.org/package=rurl",
                    "relation": "isRequiredBy",
                },
                {
                    "identifier": "https://github.com/google/robotstxt",
                    "relation": "isDerivedFrom",
                },
            ],
        },
    )

    # NEGATIVE: the measured pslr/punycoder drift - files behind DESCRIPTION.
    expect_flagged(
        "stale-version",
        "CITATION.cff says version 3.0.1",
        version="4.0.0",
        cff=released_cff,
        zenodo={"version": "4.0.0"},
    )
    # NEGATIVE: the measured robotstxtr self-disagreement.
    expect_flagged(
        "internal-disagreement",
        ".zenodo.json says version 0.1.0",
        version="0.2.0.9000",
        cff='cff-version: 1.2.0\nversion: 0.2.0\n',
        zenodo={"version": "0.1.0"},
    )
    # NEGATIVE: mirroring DESCRIPTION verbatim, which is what cffr generates.
    expect_flagged(
        "cffr-verbatim",
        "names release 3.0.1",
        version="3.0.1.9000",
        cff='cff-version: 1.2.0\nversion: 3.0.1.9000\n',
        zenodo={"version": "3.0.1"},
    )
    # NEGATIVE: the honesty clause - a release date with no release.
    expect_flagged(
        "unreleased-date",
        "has never been released",
        version="0.0.0.9000",
        cff='cff-version: 1.2.0\nversion: 0.0.0.9000\ndate-released: "2026-01-01"\n',
        zenodo={"version": "0.0.0.9000"},
    )
    # NEGATIVE: the measured seor defect - a URL DESCRIPTION does not declare.
    expect_flagged(
        "undeclared-url",
        "does not declare",
        version="0.0.0.9000",
        cff='cff-version: 1.2.0\nversion: 0.0.0.9000\nurl: "https://elsewhere.example/"\n',
        zenodo={"version": "0.0.0.9000"},
    )
    # NEGATIVE: same, on the Zenodo side.
    expect_flagged(
        "undeclared-zenodo-url",
        "related identifier",
        version="0.0.0.9000",
        cff=unreleased_cff,
        zenodo={
            "version": "0.0.0.9000",
            "related_identifiers": [
                {"identifier": "https://elsewhere.example/", "relation": "isDocumentedBy"}
            ],
        },
    )
    # NEGATIVE: a CITATION.cff this reader cannot understand must not pass.
    expect_flagged(
        "unreadable-cff",
        "does not understand",
        version="1.0.0",
        cff="cff-version: 1.2.0\nversion:\n  - 1.0.0\n",
        zenodo={"version": "1.0.0"},
    )

    # The vendored-copy digest (see IMPLEMENTATION_DIGEST). These cases prove
    # what it is FOR -- that it ignores prose and catches code -- rather than
    # only asserting that this copy happens to match today, which main()
    # already does. The fixtures are built by position, not by replacing a
    # phrase: any phrase distinctive enough to find in the docstring also
    # appears in this test, so a replacement would edit the implementation too
    # and the prose case would fail for the wrong reason.
    here = Path(__file__).resolve().read_text(encoding="utf-8")
    lines = here.splitlines(keepends=True)
    end = module_docstring_end(here)

    if implementation_digest(here) != IMPLEMENTATION_DIGEST:
        raise SystemExit("self-test FAILED (vendor-untouched): digest mismatch")

    if end == 0:
        raise SystemExit("self-test FAILED (vendor-prose): no module docstring")
    prose = "".join(
        lines[: end - 1] + ["Inserted by the self-test.\n"] + lines[end - 1 :]
    )
    if implementation_digest(prose) != IMPLEMENTATION_DIGEST:
        raise SystemExit(
            "self-test FAILED (vendor-prose): a docstring edit moved the digest, "
            "so per-repo prose could not legitimately differ"
        )

    if implementation_digest(here + "\n_ = None\n") == IMPLEMENTATION_DIGEST:
        raise SystemExit(
            "self-test FAILED (vendor-logic): a code edit left the digest alone, "
            "so drift would go unnoticed"
        )

    reblessed = here.replace(
        'IMPLEMENTATION_DIGEST = "' + IMPLEMENTATION_DIGEST + '"',
        'IMPLEMENTATION_DIGEST = "0000000000000000"',
    )
    if reblessed == here:
        raise SystemExit("self-test FAILED (vendor-rebless): constant not found")
    if implementation_digest(reblessed) != IMPLEMENTATION_DIGEST:
        raise SystemExit(
            "self-test FAILED (vendor-rebless): the digest hashes its own value"
        )

    print(
        "check-citation self-test: PASS "
        "(5 positive + 6 negative cases, 4 vendor-digest cases)"
    )


def main() -> int:
    drift = check_vendored_copy()
    if drift:
        print("check-citation failed:", file=sys.stderr)
        for error in drift:
            print(f"  - {error}", file=sys.stderr)
        return 1

    if "--self-test" in sys.argv[1:]:
        self_test()
        return 0

    root = Path(__file__).resolve().parent.parent
    errors = check_repo(root)
    if errors:
        print("check-citation failed:", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1
    print("check-citation: CITATION.cff and .zenodo.json agree with DESCRIPTION.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
