# check-news v1
"""NEWS.md terseness gate.

WHY THIS EXISTS. A NEWS file is read by a package user deciding whether an
upgrade touches them, and the fleet's had grown into design notes: rurl's
NEWS.md reached 311 KiB, bullets running to paragraphs of rationale. The rule
(design/fleet-standard.md, "NEWS.md bullets") was written down, and agents kept
breaking it, so this refuses a bloated bullet at push time (SEOR-mjrpwnqz).

WHAT IT REFUSES, per top-level list item: a bullet (`*`, `-` or `+`) or an
ordered item (`1.`, `1)`), its marker indented at most three spaces
(CommonMark). A line indented to the item's content column belongs to the
item, so a nested item stays part of its parent.

1. [wrap] The bullet's markdown source spans more than one line: a
   continuation line, a nested bullet, a fenced code block under it, or an
   indented paragraph after a blank line. A bullet is one source line,
   whatever its rendered width.
2. [length] More than WORD_LIMIT words, not counting a trailing parenthesized
   reference such as `(SEOR-abcdefgh)` or `(!53)`. A reference (REF_RE) is an
   fp key (`SEOR-abcdefgh`), a forge reference (`#12`, `!53`, `rurl#12`,
   `owner/repo!53`), a gitlab.com issue, work item or MR URL, or an ADR
   (`ADR 0004`, `ADR-008`). A trailing parenthesis that holds anything else,
   `(UTF-8)` or `(see #12)` included, is counted.
3. [reference] The trailing reference group holds more than one reference:
   a bullet carries at most one. None is required.
4. [empty] The bullet holds nothing but its reference.
5. [housekeeping] The bullet's subject is housekeeping (CI, gates, lint,
   spelling, logos, badges, codemeta/CITATION, agent files, an `Internal:`
   label and the like) and it names no function (`name()` or `name(args)`).
   The test is deliberately conservative: it reads only the bullet's opening
   words (HOUSEKEEPING_RE). Known misses both ways: it refuses a user-facing
   change that opens on such a word, such as "lintr is no longer in
   Suggests"; it passes housekeeping phrased some other way ("Updated the
   README badges"), and housekeeping that names any function ("CI now runs
   `covr::package_coverage()`"), since a function is what a user notices.

And anywhere in the file:

6. [internal] A heading, ATX (`## Internal`) or setext (`Internal` over
   `---` or `===`), at any level, that opens on `Internal` or `Internals`,
   bold or not: housekeeping gets no bullet, so it gets no section either.

Fenced code blocks (``` or ~~~, closed only by a run of the same character
at least as long) and HTML comment lines (`<!-- ... -->`) are skipped. Version
headings and prose paragraphs are not judged here; the top heading is the
`news-version` gate's.

This is the reference copy every fleet package ports: it takes the NEWS.md
path as its one argument and knows nothing about any one repository. Stdlib
only.

    python3 scripts/check-news.py NEWS.md      # exit 1 on a refused bullet
    python3 scripts/check-news.py --self-test  # positive/negative cases

Exit status: 0 when every bullet passes, 1 on any refusal (or a failed
self-test), 2 when the file cannot be read or the arguments are wrong.
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path

# Calibrated on ssrfr's and sitemapr's NEWS.md (2026-10-28), the two already
# written one terse line per bullet: of their 82 bullets the longest run to 32
# words, the median to 18 or 19. 32 keeps both green; a bullet past it is
# carrying its rationale.
WORD_LIMIT = 32

# A fence opens on a run of three or more backticks or tildes; a backtick
# fence's info string holds no backtick, or the line is a code span.
FENCE_RE = re.compile(r"^[ \t]*(?P<run>`{3,}(?=[^`]*$)|~{3,})")
COMMENT_RE = re.compile(r"^ {0,3}<!--")
HEADING_RE = re.compile(r"^ {0,3}#{1,6}(?=[ \t]|$)")
SETEXT_RE = re.compile(r"^ {0,3}(?:=+|-+)[ \t]*$")
INTERNAL_RE = re.compile(r"^(?:\*\*|__|\*|_)?internals?\b", re.I)
# A top-level list item (CommonMark: the marker indented at most three spaces).
BULLET_RE = re.compile(r"^( {0,3}(?:[*+-]|\d{1,9}[.)]))([ \t]+)(.*)$")
WORD_RE = re.compile(r"\S*\w\S*")

# One reference: an fp key (SEOR-abcdefgh), a forge reference (#12, !53,
# rurl#12, owner/repo!53), a gitlab.com issue, work item or MR URL, or an ADR
# (ADR 0004, ADR-008, ADR 0020 D3). Backticks around it are allowed.
REF_RE = re.compile(
    r"`?(?:[A-Z]{2,}-[a-z]{8}"
    r"|(?:[\w.-]+/)?(?:[a-z][a-z0-9.]*)?[#!]\d+"
    r"|https://gitlab\.com/[\w.-]+(?:/[\w.-]+)*/-/(?:issues|work_items|merge_requests)/\d+"
    r"|ADR[- ]?\d+(?:\s+D\d+)*)`?"
)
TRAILING_PAREN_RE = re.compile(r"\s*\(([^()]*)\)\s*[.;]?\s*$")
REF_SPLIT_RE = re.compile(r"\s*(?:,|;|\band\b)\s*")

# The bullet's opening words, after an optional bold marker and article.
HOUSEKEEPING_RE = re.compile(
    r"""^(?:\*\*)?(?:(?:the|a|an|its|this\s+package's)\s+)?(?:new\s+)?
    (?:CI\b|continuous\s+integration|`?\.gitlab-ci\.ya?ml`?
      |`?pre-(?:push|commit)`?|(?:verify|local|cheap)\s+gates?\b|gates?\b
      |internals?(?:\*\*)?\s*:
      |lint(?:s|ing|r|\s+set)?\b|`?\.lintr`?|spelling\b|spell-?check|codespell
      |`?inst/WORDLIST`?|word\s*list\b|(?:hex\s+)?logos?\b|(?:README\s+)?badges?\b
      |badge\s+row|alt\s+text|`?codemeta\.json`?|`?CITATION\.cff`?|`?\.zenodo\.json`?
      |`?\.bestpractices\.json`?|`?(?:AGENTS(?:_LANG)?|CLAUDE)\.md`?
      |agent\s+(?:files?|instructions?))""",
    re.I | re.X,
)
FUNCTION_RE = re.compile(r"\b[\w.]+\([^)]*\)")


@dataclass
class Bullet:
    line: int  # 1-based source line of the list marker
    lines: list[str]  # its source lines, the first one included
    content: int  # the column its content starts at; a line indented this far belongs to it

    @property
    def text(self) -> str:
        first = BULLET_RE.match(self.lines[0])
        rest = [line.strip() for line in self.lines[1:] if line.strip()]
        return " ".join([first.group(3).strip() if first else self.lines[0].strip()] + rest)


def indent(line: str) -> int:
    expanded = line.expandtabs(4)
    return len(expanded) - len(expanded.lstrip(" "))


def content_column(m: re.Match) -> int:
    """CommonMark: one to four spaces after the marker belong to it; more mean one."""
    gap = len(m.group(2).expandtabs(4))
    return len(m.group(1)) + (gap if gap <= 4 else 1)


def closes_fence(line: str, fence: tuple[str, int]) -> bool:
    """A closing fence: a run of the opening character, at least as long, and nothing else."""
    char, length = fence
    stripped = line.strip()
    return len(stripped) >= length and set(stripped) == {char}


def heading_text(line: str) -> str:
    """An ATX heading's content, its opening and closing hashes dropped."""
    return re.sub(r"(?:^|[ \t]+)#+[ \t]*$", "", line.strip().lstrip("#").strip())


def split_reference(text: str) -> tuple[str, list[str]]:
    """`text` without a trailing parenthesized reference group, and that group's references."""
    m = TRAILING_PAREN_RE.search(text)
    if not m:
        return text, []
    parts = [p for p in REF_SPLIT_RE.split(m.group(1).strip()) if p]
    if parts and all(REF_RE.fullmatch(p) for p in parts):
        return text[: m.start()], parts
    return text, []


def trailing_reference(text: str) -> str:
    """`text` without a trailing parenthesized reference, when it ends in one."""
    return split_reference(text)[0]


def words(text: str) -> int:
    return len(WORD_RE.findall(trailing_reference(text)))


def is_housekeeping(text: str) -> bool:
    return bool(HOUSEKEEPING_RE.match(text)) and not FUNCTION_RE.search(text)


def parse(text: str) -> tuple[list[Bullet], list[tuple[int, str, str]]]:
    """The file's top-level bullets, and its headings as (line, source, content)."""
    bullets: list[Bullet] = []
    headings: list[tuple[int, str, str]] = []
    current: Bullet | None = None
    after_blank = False
    fence: tuple[str, int] | None = None
    in_comment = False
    paragraph: tuple[int, list[str]] | None = None  # an open prose paragraph, for a setext heading
    for number, line in enumerate(text.splitlines(), start=1):
        if fence is not None:
            if closes_fence(line, fence):
                fence = None
            continue
        if in_comment:
            in_comment = "-->" not in line
            continue
        if COMMENT_RE.match(line):  # neutral: neither a wrap nor the end of a bullet
            in_comment = "-->" not in line[line.index("<!--") + 4:]
            continue
        if not line.strip():
            after_blank, paragraph = True, None
            continue
        inside = current is not None and indent(line) >= current.content
        m = FENCE_RE.match(line)
        if m and (inside or indent(line) <= 3):
            fence = (m.group("run")[0], len(m.group("run")))
            if inside:
                current.lines.append(line)
            else:
                current = None
            after_blank, paragraph = False, None
            continue
        if inside:
            current.lines.append(line)
        elif HEADING_RE.match(line):
            headings.append((number, line.strip(), heading_text(line)))
            current, paragraph = None, None
        elif paragraph is not None and SETEXT_RE.match(line):
            start, texts = paragraph
            headings.append((start, f"{' '.join(texts)} / {line.strip()}", " ".join(texts)))
            paragraph = None
        elif b := BULLET_RE.match(line):
            current, paragraph = Bullet(number, [line], content_column(b)), None
            bullets.append(current)
        elif current is not None and not after_blank:
            current.lines.append(line)
        else:
            current = None
            if paragraph is None:
                paragraph = (number, [])
            paragraph[1].append(line.strip())
        after_blank = False
    return bullets, headings


def check_text(text: str) -> list[tuple[int, str, str]]:
    """Each refusal as (line, key, message); key is wrap, length, reference, empty, housekeeping or internal."""
    bullets, headings = parse(text)
    found: list[tuple[int, str, str]] = []
    for number, source, content in headings:
        if INTERNAL_RE.match(content):
            found.append((number, "internal", f"{source!r}: housekeeping gets no bullet and no section"))
    for bullet in bullets:
        body = bullet.text
        if len(bullet.lines) > 1:
            found.append((bullet.line, "wrap", f"the bullet spans {len(bullet.lines)} source lines; make it one"))
        rest, refs = split_reference(body)
        count = len(WORD_RE.findall(rest))
        if count > WORD_LIMIT:
            found.append((bullet.line, "length", f"{count} words, over {WORD_LIMIT} (the trailing reference not "
                                                  "counted); the reasoning belongs in the issue or ADR"))
        if len(refs) > 1:
            found.append((bullet.line, "reference", f"{len(refs)} references ({', '.join(refs)}); keep the one "
                                                     "most specific to the change"))
        if count == 0:
            found.append((bullet.line, "empty", "the bullet holds nothing but its reference"))
        if is_housekeeping(body):
            found.append((bullet.line, "housekeeping", f"housekeeping gets no NEWS bullet: {body[:60]!r}"))
    return sorted(found)


# --- self-test (positive + negative coverage, executable) --------------------


def self_test() -> None:
    limit_words = " ".join(["word"] * WORD_LIMIT)
    cases: list[tuple[str, str, set[str]]] = [
        # POSITIVE.
        ("terse", "# pkg 1.0.0\n\n* `f()` now accepts `NA` (PKG-abcdefgh).\n* `g()` is faster.\n", set()),
        ("dash and plus bullets", "- `f()` now accepts `NA` (!12).\n+ `g()` is faster.\n", set()),
        ("exactly the limit", f"* {limit_words}.\n", set()),
        ("standard clause cited", "* `parse()` rejects a bare `%` in the path (RFC 3986 §2.1) (PKG-abcdefgh).\n", set()),
        ("blank line ends the bullet", "* `f()` is faster.\n\nA prose paragraph after it.\n", set()),
        ("feature subheadings", "# pkg 1.0.0\n\n## Reading\n\n* `read()` reads.\n\n### Policy\n\n* `p()` builds.\n",
         set()),
        ("fenced code skipped", "* `f()` is faster.\n\n```\n## Internal\n* CI\n  wraps\n```\n", set()),
        ("housekeeping word later in the bullet", "* `f()` no longer fails when CI is set.\n", set()),
        ("housekeeping subject naming a function", "* Badges: `badge_url()` builds a badge URL.\n", set()),
        ("user-facing subject that starts like one", "* Gateway timeouts now retry once.\n", set()),
        ("an internal-sounding user change", "* Internal buffers are freed after each call.\n", set()),
        ("an Internal-led feature heading", "# pkg 1.0.0\n\n## International domains\n\n* `f()` reads IDNs.\n",
         set()),
        # Fences: a run closes only on the same character, at least as long, and nothing after it.
        ("a four-backtick fence holds an inner ```", "````\n```\n## Internal\n```\n````\n* ok.\n", set()),
        ("a shorter tilde run does not close", "~~~~\n~~~\n## Internal\n~~~~\n* ok.\n", set()),
        ("an info string does not close", "```\n```r\n## Internal\n```\n* ok.\n", set()),
        ("an unindented fence after a bullet ends it", "* `f()` is faster.\n```\ncode\n```\n", set()),
        # List markers: CommonMark top level, nested items stay with their parent.
        ("ordered items", "1. `f()` now accepts `NA` (!12).\n2) `g()` is faster.\n", set()),
        ("siblings indented one space", " * `f()` is faster.\n * `g()` too.\n", set()),
        # References: each accepted form, one at a time, is not counted.
        *[(f"the reference {ref} not counted", f"* {limit_words} ({ref}).\n", set())
          for ref in ("PKG-abcdefgh", "`PKG-abcdefgh`", "!53", "#12", "rurl#12", "seor!53", "bart-turczynski/seor#12",
                      "https://gitlab.com/bart-turczynski/seor/-/work_items/12",
                      "https://gitlab.com/bart-turczynski/seor/-/merge_requests/53", "ADR 0004", "ADR-008",
                      "ADR 0020 D3")],
        ("a bullet with no reference", "* `f()` is faster.\n", set()),
        ("a function written with arguments", "* CI: `use_ci(x)` sets up the pipeline.\n", set()),
        ("an HTML comment line between bullets", "* `f()` is faster.\n<!-- a note -->\n* `g()` too.\n", set()),
        ("a multi-line HTML comment", "* `f()` is faster.\n<!--\n## Internal\n  wraps\n-->\n* `g()` too.\n", set()),
        # NEGATIVE.
        ("continuation line", "* `f()` now accepts `NA`\n  and `NULL` (PKG-abcdefgh).\n", {"wrap"}),
        ("lazy continuation", "* `f()` now accepts `NA`\nand `NULL`.\n", {"wrap"}),
        ("nested bullet", "* `f()` changes:\n  - it accepts `NA`.\n", {"wrap"}),
        ("indented paragraph after a blank line", "* `f()` changes.\n\n  Here is why.\n", {"wrap"}),
        ("a code block under a bullet", "* `f()` takes a vector:\n\n  ```r\n  f(1:3)\n  ```\n", {"wrap"}),
        ("a code block straight under a bullet", "* `f()` takes a vector:\n  ```r\n  f(1:3)\n  ```\n* ok.\n", {"wrap"}),
        ("a tilde block under a bullet", "* `f()` takes a vector:\n\n  ~~~\n  f(1:3)\n  ~~~\n", {"wrap"}),
        ("a triple-backtick code span is no fence", "* `f()` errors:\n    ``` `x` must be one value. ```\n## Internal\n",
         {"wrap", "internal"}),
        ("an ordered item that wraps", "2) `f()` now accepts `NA`\n   and `NULL`.\n", {"wrap"}),
        ("a nested item under an ordered one", "1. `f()` changes:\n   - it accepts `NA`.\n", {"wrap"}),
        ("a marker indented three spaces", "   * CI now caches the package library.\n", {"housekeeping"}),
        ("an ordered housekeeping item", "1. CI now caches the package library.\n", {"housekeeping"}),
        ("one word over the limit", f"* {limit_words} more.\n", {"length"}),
        # Fixed lengths, so the limit cannot drift far without this failing.
        ("a 25-word bullet", f"* {' '.join(['word'] * 25)}.\n", set()),
        ("a 45-word bullet", f"* {' '.join(['word'] * 45)}.\n", {"length"}),
        ("prose in the trailing parenthesis is counted",
         f"* {' '.join(['word'] * (WORD_LIMIT - 2))} (because the old one was slow).\n", {"length"}),
        # Parentheticals that are not references are counted.
        *[(f"the parenthetical {p} counted", f"* {limit_words} ({p}).\n", {"length"})
          for p in ("UTF-8", "ISO-8859", "see #12", "fixes #12")],
        ("two references", "* `f()` is faster (PKG-abcdefgh, PKG-ijklmnop).\n", {"reference"}),
        ("many references", f"* {limit_words} (PKG-abcdefgh, owner/repo#12, !53; ADR 0004).\n", {"reference"}),
        ("nothing but a reference", "* (PKG-abcdefgh).\n", {"empty"}),
        ("## Internal", "# pkg 1.0.0\n\n## Internal\n", {"internal"}),
        ("### Internal changes", "# pkg 1.0.0\n\n### Internal changes\n", {"internal"}),
        ("## Internals", "# pkg 1.0.0\n\n## Internals\n", {"internal"}),
        ("a bold Internal heading", "# pkg 1.0.0\n\n## **Internal**\n", {"internal"}),
        ("an indented Internal heading", "# pkg 1.0.0\n\n   ## Internal ##\n", {"internal"}),
        ("a setext Internal heading, ---", "# pkg 1.0.0\n\nInternal\n--------\n\n* `f()` is faster.\n", {"internal"}),
        ("a setext Internal heading, ===", "Internal changes\n===\n", {"internal"}),
        ("an Internal: label", "* **Internal:** the test suite runs in parallel.\n", {"housekeeping"}),
        ("CI", "* CI now caches the package library (PKG-abcdefgh).\n", {"housekeeping"}),
        ("the gate", "* The pre-push gate runs `check-urls`.\n", {"housekeeping"}),
        ("the verify gate", "* The verify gate runs lintr first.\n", {"housekeeping"}),
        ("lint set", "* Lint set aligned with goodpractice.\n", {"housekeeping"}),
        ("spelling", "* Spelling fixed throughout the vignettes.\n", {"housekeeping"}),
        ("logo", "* The hex logo carries metadata.\n", {"housekeeping"}),
        ("badges", "* README badges follow the fleet standard.\n", {"housekeeping"}),
        ("codemeta", "* `codemeta.json` lists the keywords.\n", {"housekeeping"}),
        ("CITATION.cff", "* `CITATION.cff` names the release.\n", {"housekeeping"}),
        ("agent files", "* Agent instructions point at the house skills.\n", {"housekeeping"}),
        ("bold lead-in", "* **CI:** the coverage job fails below 95%.\n", {"housekeeping"}),
        ("every rule at once", f"## Internal\n\n* CI {limit_words}\n  wraps (PKG-abcdefgh, !53).\n",
         {"internal", "wrap", "length", "reference", "housekeeping"}),
    ]
    failures = []
    for tag, text, want in cases:
        got = {key for _, key, _ in check_text(text)}
        if got != want:
            failures.append(f"self-test FAILED ({tag}): expected {sorted(want)}, got {sorted(got)}")
    # The line a refusal names is the bullet's first line, and a setext heading's text line.
    lines = [line for line, _, _ in check_text("# pkg\n\n* ok.\n* wraps\n  here.\n\nInternal\n---\n")]
    if lines != [4, 7]:
        failures.append(f"self-test FAILED (line number): expected [4, 7], got {lines}")
    if failures:
        raise SystemExit("\n".join(failures))
    positive = sum(1 for _, _, want in cases if not want)
    print(f"check-news self-test: PASS ({positive} positive + {len(cases) - positive} negative cases)")


def main(argv: list[str]) -> int:
    if argv == ["--self-test"]:
        self_test()
        return 0
    if len(argv) != 1 or argv[0].startswith("-"):
        print("usage: check-news.py NEWS.md | --self-test", file=sys.stderr)
        return 2
    path = Path(argv[0])
    try:
        text = path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as error:
        print(f"check-news: cannot read {path}: {error}", file=sys.stderr)
        return 2
    found = check_text(text)
    if found:
        print(f"check-news failed: {len(found)} refusal(s) in {path} (design/fleet-standard.md, "
              "\"NEWS.md bullets\"):", file=sys.stderr)
        for line, key, message in found:
            print(f"  - {path}:{line}: [{key}] {message}", file=sys.stderr)
        return 1
    print(f"check-news: every bullet in {path} is one terse line.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
