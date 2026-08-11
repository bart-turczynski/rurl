# The BASE-RELATIVE half of the official WHATWG conformance suite
# (RURL-fupsemxr, epic RURL-dorofzmb).
#
# WHY THIS FILE EXISTS. Until this commit no harness in the repo read a single
# base-carrying WPT row: `inst/bench/make-wpt-fixture.py` filtered them out
# (`if e.get("base") is not None: continue`), on the since-outdated ground that
# rurl is absolute-parse-only. rurl HAS a resolver -- `resolve_url()` --  and
# its merge is RFC 3986 section 5 throughout, which is not what WHATWG
# specifies for a relative reference. So the repo was about to change
# resolution behaviour with zero regression signal over the standard's own
# relative-resolution corpus. This file is that signal.
#
# SCOPE. It is an INSTRUMENT, not a fix. At this commit rurl differs from
# upstream on 43 of the 274 rows, and those 43 are enumerated below. The test
# is green because the measured differing set equals the enumerated one --
# never because a count was tolerated.
#
# FIXTURE. `tests/testthat/fixtures/wpt-url-base-relative.json`, the exact
# complement of `inst/bench/wpt-url-cases.json` on `base`, generated from the
# same upstream artifact at the same pinned revision. It lives under
# `tests/testthat/fixtures/` rather than `inst/bench/` deliberately:
# `.Rbuildignore` excludes the `inst/bench/` import, so a harness reading it
# through `system.file()` silently SKIPS under `R CMD check` -- the environment
# closest to the shipped package (pre-existing defect, RURL-mifbbrez).
#
# ORACLE. Upstream's own `href`, the WHATWG serialization of the parsed URL --
# never a string re-assembled from the component getters, which collapse
# null-vs-empty host and absent-vs-empty query/fragment. Same choice, and the
# same reasoning, as `test-wpt-full-suite.R`.
#
# SUBSTRATE. `serialize_url(<resolved>, standard = "whatwg")` -- surface (b),
# the FSSS. The resolution step is reached through the INTERNAL
# `.resolve_one_raw()` rather than the public `resolve_url()` because
# `resolve_url()` returns `clean_url`, output surface (c): a policy-driven
# SEO/canonicalization product that drops the fragment and userinfo and is
# barred from carrying a conformance claim (P2.7 D-A,
# design/work/url-v3/decisions/P2.7-display-and-resolver-output.md; P5.3
# CLAIM-1). Scoring the public surface here would measure the canonicalizer.
#
# The resolution call passes `url_standard = "whatwg"` because that is the
# standard this file scores against. Reference resolution stopped being
# standard-agnostic with P2.7 D-B (R/resolve.R): the WHATWG reference-parsing
# rules are reachable ONLY through that selector, and the NULL selector stays
# byte-frozen under ADR 0007 / P2.7 D-C. Scoring with the NULL selector would
# hold rurl to WHATWG's oracle while denying it WHATWG's rules -- and would go
# silently stale as each rule lands.

wpt_base_relative_suite <- function() {
  skip_if_not_installed("jsonlite")
  path <- testthat::test_path("fixtures", "wpt-url-base-relative.json")
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

wpt_rel_field <- function(rows, key) {
  vapply(rows, function(x) {
    v <- x[[key]]
    if (is.null(v)) "" else v
  }, character(1))
}

# A row's identity. `input` alone is NOT unique here -- ".." and "/" each occur
# against several different bases -- so the key is the PAIR, spelled so a
# reader can see both halves. Uniqueness is asserted below rather than assumed.
wpt_rel_id <- function(base, input) paste0(base, " >> ", input)

# ---- the known-differ set ---------------------------------------------------
#
# 43 of 274 rows, grouped by the EARLIEST point at which rurl's resolution
# leaves the WHATWG algorithm, so a later unit can delete one group at a time.
# Every group is a real, currently-failing family: nothing here is speculative.
#
# Two notes on how this differs from the prior measurement recorded on
# RURL-fupsemxr, which counted 62 across a partly different family list:
#   * the count was 56 when this file landed, not 62 -- the `file:` empty-host
#     family (RURL-uhwivndf) was discharged in the meantime, and the base-null
#     suite now scores 336/336. It is 43 now: the SAME_SCHEME family (14 rows)
#     was discharged by P2.7 D-B, and 13 of its 14 rows left with it;
#   * "rows rurl rejects outright (NA)" is NOT a family here. A reject is a
#     symptom, not a cause: the NA rows are distributed across the groups below
#     by the defect that produced them, which is the axis a fix is organised
#     around.
#
# DISCHARGED: `WPT_REL_SAME_SCHEME` -- the reference carrying the base's OWN
# special scheme. WHATWG consumes it and continues relatively ("special relative
# or authority state"); rurl's RFC 3986 splitter saw a scheme and took the
# absolute branch, ignoring the base. `.split_after_scheme()` (R/resolve.R) now
# implements the state under `url_standard = "whatwg"`, and 13 of the family's
# 14 rows became exact. The fourteenth,
# `file:///tmp/mock/path >> file:c:\foo\bar.html`, is a CONJUNCTION: once the
# scheme is consumed relatively, what is left wrong about it is the Windows
# drive letter, so it moved into WPT_REL_DRIVE_LETTER below rather than staying
# behind under a family name that no longer explains it.

# Backslash-as-slash inside the REFERENCE. Under a special scheme WHATWG reads
# `\` exactly as `/`, including where a run of them introduces an authority;
# rurl's reference splitter and merge treat it as an ordinary path byte.
WPT_REL_BACKSLASH <- c(
  "http://example.org/foo/bar >> \\x",
  "http://example.org/foo/bar >> \\\\x\\hello",
  "file:///tmp/mock/path >> \\\\server\\file",
  "file:///tmp/mock/path >> /\\server/file",
  "file://lion/ >> \\//pig",
  "file://lion/ >> \\/localhost//pig",
  "http://example.org/ >> /\\/\\//example.org/../path",
  "file:/// >> /\\//\\/a/../"
)

# Leading/trailing C0-or-space is not stripped from the reference (and the
# stripped-to-empty case must then fall back to the base minus its fragment).
WPT_REL_C0_OR_SPACE <- c(
  "http://example.org/foo/bar >> \t   :foo.com   \n",
  "http://example.org/foo/bar >>  foo.com  ",
  "http://example.org/foo/bar >>   \t"
)

# Windows drive letters. WHATWG normalises `C|` to `C:`, refuses to shorten a
# path past a drive letter, and in the file-host state turns a drive-letter
# "host" into an empty host plus a path segment. rurl has none of that inside
# resolution, so the drive letter behaves like any other segment or host.
WPT_REL_DRIVE_LETTER <- c(
  "file:///tmp/mock/path >> C|/foo/bar",
  "file:///C:/ >> ..",
  "file:///C:/a/b >> /",
  "file://h/C:/a/b >> /",
  "file:///C:/a/b >> //d:",
  "file:///C:/a/b >> //d:/..",
  "file://host/dir/file >> C|",
  "file://host/D:/dir1/dir2/file >> C|",
  "file://host/dir/file >> C|#",
  "file://host/dir/file >> C|?",
  "file://host/dir/file >> C|/",
  "file://host/dir/file >> C|\n/",
  "file://host/dir/file >> C|\\",
  "file://x/C:/ >> ..",
  "file://host/ >> //C:/",
  "file://host/ >> file://C:/",
  # Ex-WPT_REL_SAME_SCHEME. The scheme is now consumed relatively, so the
  # resolved path merges against the base (`file:///tmp/mock/c:/foo/bar.html`)
  # instead of being rejected; the residual gap is WHATWG's file-state rule that
  # a remainder BEGINNING with a drive letter EMPTIES the path rather than
  # shortening it, which is this family's machinery and this family's unit.
  "file:///tmp/mock/path >> file:c:\\foo\\bar.html"
)

# "Special authority ignore slashes": after a special scheme WHATWG skips an
# arbitrary RUN of `/` and `\` before reading the host, so `///example.org/x`
# has host example.org. RFC 3986 stops at two and reads an EMPTY authority.
WPT_REL_SLASH_RUN <- c(
  "http://example.org/ >> ///example.org/../path",
  "http://example.org/ >> ///example.org/../../",
  "http://example.org/ >> ///example.org/../path/../../",
  "http://example.org/ >> ///example.org/../path/../../path"
)

# A resolved path whose first segment is empty must serialize with the `/.`
# guard or it re-reads as an authority. `serialize_url()` already emits the
# guard (see test-wpt-full-suite.R); it is LOST here because resolution hands
# it a recomposed STRING -- `.recompose_uri()` writes "non-spec://path" -- and
# the serializer then parses an authority that resolution never intended.
WPT_REL_PATH_AS_AUTHORITY <- c(
  "non-spec:/p >> /.//path",
  "non-spec:/p >> /..//path",
  "non-spec:/p >> ..//path",
  "non-spec:/p >> a/..//path",
  "non-spec:/..//p >> path"
)

# NOT a resolution defect at all: the reference is absolute, so the base is
# irrelevant and the deviation is in absolute parsing. Kept in the fixture
# because upstream files these rows with a base, and kept visible here so a
# resolution fix is not credited with -- or blamed for -- them.
WPT_REL_ABSOLUTE_REF <- c(
  "http://example.org/foo/bar >> tel:1234567890",
  "http://other.com/ >> http://%30%78%63%30%2e%30%32%35%30.01",
  "http://other.com/ >> http://%30%78%63%30%2e%30%32%35%30.01%2e",
  paste0("http://other.com/ >> http://",
         "\uff10\uff38\uff43\uff10\uff0e\uff10\uff12\uff15\uff10\uff0e",
         "\uff10\uff11")
)

# The scheme production is too permissive: RFC 3986's `[^:/?#]+` before a colon
# admits "10.0.0.7" and "[61", so a relative PATH that happens to contain a
# colon is misread as an absolute reference. WHATWG requires ALPHA *( ALPHA /
# DIGIT / "+" / "-" / "." ).
WPT_REL_SCHEME_PRODUCTION <- c(
  "http://example.org/foo/bar >> [61:24:74]:98",
  "file:///some/dir/bar.html >> 10.0.0.7:8080/foo.html"
)

WPT_REL_KNOWN_DIFFER <- c(
  WPT_REL_BACKSLASH, WPT_REL_C0_OR_SPACE,
  WPT_REL_DRIVE_LETTER, WPT_REL_SLASH_RUN, WPT_REL_PATH_AS_AUTHORITY,
  WPT_REL_ABSOLUTE_REF, WPT_REL_SCHEME_PRODUCTION
)

# Assert a property holds of every row except an enumerated deviation set.
# House style, copied from test-rfc3986-serialization-properties.R: the
# comparison is a SET EQUALITY, not a count. An unlisted violation fails, and
# so does a listed row that no longer violates -- so a fix cannot land without
# deleting its entry, and the list cannot rot into a permanent allowance.
expect_property <- function(violates, input, deviations = character(0)) {
  expect_setequal(input[violates], deviations)
}

test_that("the known-differ families are disjoint and sum to the whole", {
  # The constant is itself data, and a duplicated id across two families would
  # make the set-equality below pass while the families lie about ownership.
  expect_length(WPT_REL_KNOWN_DIFFER, 43L)
  expect_length(unique(WPT_REL_KNOWN_DIFFER), 43L)
})

test_that("WPT base-relative rows resolve to the standard's own `href`", {
  j <- wpt_base_relative_suite()
  s <- j$success
  # The population is PINNED, not read: a fixture that silently gained or lost
  # rows would otherwise re-baseline the claim without anyone noticing.
  expect_length(s, 274L)

  input <- wpt_rel_field(s, "input")
  base <- wpt_rel_field(s, "base")
  href <- wpt_rel_field(s, "href")
  # Every row carries a base and the oracle; a blank would silently pass below.
  expect_true(all(nzchar(base)))
  expect_true(all(nzchar(href)))

  id <- wpt_rel_id(base, input)
  expect_length(unique(id), 274L)

  # `.resolve_one_raw()` (R/resolve.R) is the raw RFC 3986 section 5 resolution
  # step, before `resolve_url()` canonicalizes it down to surface (c). See the
  # SUBSTRATE note at the top of this file for why the public entry point
  # cannot carry this claim.
  resolved <- vapply(
    seq_along(id),
    function(i) rurl:::.resolve_one_raw(input[[i]], base[[i]], "whatwg"),
    character(1)
  )
  got <- serialize_url(resolved, standard = "whatwg")

  # A rejected row (NA) differs just as much as a wrongly-spelled one.
  differs <- is.na(got) | got != href
  expect_property(differs, id, WPT_REL_KNOWN_DIFFER)
})
