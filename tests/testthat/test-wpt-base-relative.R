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
# SCOPE. It is an INSTRUMENT, not a fix. When it landed, rurl differed from
# upstream on 56 of the 274 rows, and every differing row was enumerated below
# by family. At this commit ONE row differs, and it is enumerated. The test is
# green because the measured differing set equals the enumerated one -- never
# because a count was tolerated -- so an unlisted deviation fails, and so does
# a listed row that stops deviating.
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
# 1 of 274 rows. The families below were grouped by the EARLIEST point at
# which rurl's resolution left the WHATWG algorithm, so each unit could delete
# one group at a time; the DISCHARGED notes keep the history of what each fix
# was, so a regression names its family instead of moving a count.
#
# Two notes on how this differed from the prior measurement recorded on
# RURL-fupsemxr, which counted 62 across a partly different family list:
#   * the count was 56 when this file landed, not 62 -- the `file:` empty-host
#     family (RURL-uhwivndf) was discharged in the meantime, and the base-null
#     suite now scores 336/336. It fell to 43 when the SAME_SCHEME family (14
#     rows) was discharged by P2.7 D-B, to 29 when the reference preprocessing
#     families below were, to 27 with the scheme production, and to 0 when the
#     last three families (drive letters, the `/.` guard, the absolute-parse
#     rows) were discharged together (RURL-ufsltsit, RURL-bedensww,
#     RURL-lxdwuacn), bar the one absolute-parse row below;
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
#
# DISCHARGED: `WPT_REL_BACKSLASH` (8 rows) and `WPT_REL_SLASH_RUN` (4 rows) --
# backslash-as-slash inside the reference, and the arbitrary-length leading run
# of `/` and `\` that introduces an authority. These were two family names for
# ONE state chain: WHATWG's "relative slash state" reads `\` exactly as `/` and
# hands a second slash-or-backslash to "special authority ignore slashes", which
# skips the whole run before reading a host. A scheme-LESS reference under a
# special base now enters that chain through the same `.split_after_scheme()`
# a consumed special scheme does (R/resolve.R), so neither family could be
# discharged without the other -- `///example.org/../path` and
# `/\/\//example.org/../path` differ only in which slash byte was typed.
#
# DISCHARGED: `WPT_REL_C0_OR_SPACE` (3 rows) -- the WHATWG parser's step 1
# strip (leading/trailing C0-control-or-SPACE, then every tab/LF/CR) now runs on
# the REFERENCE, through the same `.strip_whatwg_control_chars_vec()` seam the
# absolute-parse path uses. One row MOVED rather than passing:
# `file:///tmp/mock/path >>   File:c|////foo\bar.html` used to be exact by
# accident, because its leading spaces made `  File` an unrecognizable scheme
# and sent the whole reference down the absolute branch, where absolute parsing
# got it right. Stripped, its scheme IS the base's, so it now resolves
# relatively and lands on the drive-letter defect -- the same conjunction as the
# ex-SAME_SCHEME row above, and it sits in WPT_REL_DRIVE_LETTER for the same
# reason.
#
# DISCHARGED: `WPT_REL_SCHEME_PRODUCTION` (2 rows) -- the scheme production was
# Appendix B's self-described NON-validating `[^:/?#]+`, which admits "10.0.0.7"
# and "[61", so a relative PATH whose first segment merely contains a colon was
# read as an absolute reference and the base was discarded. `.split_uri_ref()`
# (R/resolve.R) now reads `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )` whenever
# a standard is selected. That production is RFC 3986 section 3.1's OWN grammar
# and byte-identical to WHATWG's scheme states, so unlike every other rule in
# this file it ships under `"rfc3986"` too -- and unlike them it is not gated on
# `.whatwg_special_base_scheme()`, because a reference's scheme production does
# not depend on the base at all. Only `url_standard = NULL` keeps the loose
# group, and only because ADR 0007 freezes it byte-for-byte (measured: zero
# NULL-path rows move).
#
# No row MOVED between families that time: both rows became exact, and the
# three families below were the residue unchanged (18 + 5 + 4).
#
# DISCHARGED: `WPT_REL_DRIVE_LETTER` (18 rows, RURL-ufsltsit) -- Windows drive
# letters. WHATWG normalises `C|` to `C:`, refuses to shorten a path past a
# drive letter, empties the base path when the remainder BEGINS with one, and
# in the file-host state turns a drive-letter "host" into an empty host plus a
# path segment. Resolution had none of that, so the drive letter behaved like
# any other segment or host. The first three rules now live in R/resolve.R
# (`.transform_reference()` under `whatwg_file`, gated on the base's scheme
# being `file` under `url_standard = "whatwg"`); the fourth was ALREADY in the
# absolute parser (`.parse_whatwg_file_urls_vec()`) for the `C|` spelling and
# missed `C:`, which is why `file://C:/` and `//d:` rejected outright. The two
# conjunction rows -- ex-SAME_SCHEME `file:c:\foo\bar.html` and
# ex-C0_OR_SPACE `  File:c|////foo\bar.html` -- discharged with the family, as
# predicted: once the scheme is consumed relatively, what was left wrong about
# them was exactly the file-state empty-the-path rule. The rows:
# `file:///tmp/mock/path >> C|/foo/bar`, `file:///C:/ >> ..`,
# `file:///C:/a/b >> /`, `file://h/C:/a/b >> /`, `file:///C:/a/b >> //d:`,
# `file:///C:/a/b >> //d:/..`, `file://host/dir/file >> C|` (and its `#`,
# `?`, `/`, `LF/` and `\` variants), `file://host/D:/dir1/dir2/file >> C|`,
# `file://x/C:/ >> ..`, `file://host/ >> //C:/`, `file://host/ >> file://C:/`.
# test-resolve-url.R pins each of them by hand.
#
# DISCHARGED: `WPT_REL_PATH_AS_AUTHORITY` (5 rows, RURL-bedensww) -- a resolved
# path whose first segment is empty must serialize with the `/.` guard or it
# re-reads as an authority. `serialize_url()` already emitted the guard (see
# test-wpt-full-suite.R); it was LOST here because resolution hands it a
# recomposed STRING, and `.recompose_uri()` wrote "non-spec://path".
# `.recompose_uri()` (R/resolve.R) now emits the guard whenever a standard is
# selected and the path begins with `//` after no authority -- RFC 3986
# section 3.3's own constraint under "rfc3986", the serializer's guard under
# "whatwg", and byte-frozen under NULL (ADR 0007). The rows:
# `non-spec:/p >> /.//path`, `>> /..//path`, `>> ..//path`, `>> a/..//path`,
# `non-spec:/..//p >> path`.
#
# DISCHARGED: `WPT_REL_ABSOLUTE_REF` (4 rows, RURL-lxdwuacn) -- NOT a
# resolution defect at all: the reference is absolute, so the base is
# irrelevant and the deviation was in absolute parsing. The three
# obfuscated-IPv4 rows went first: the WHATWG host model read "ends in a
# number" off the SOURCE token, so `%30%78%63%30%2e%30%32%35%30.01`
# (percent-encoded `0xc0.0250.01`) was rejected and the fullwidth spelling was
# accepted as a reg-name; `.apply_host_standard_model_vec()`
# (R/parse-phases.R) now reads it after percent-decoding and UTS-46 mapping,
# in the host parser's own order, and all three are `http://192.168.0.1/`.
# The last, `tel:1234567890`, matched the scheme-less `<host>:<port>`
# carve-out in `.general_parsed_mask()` (R/parse-state.R) and was diverted
# from the opaque parser to the web route, which rejected it, while WHATWG's
# scheme state reads `tel` as the scheme and `1234567890` as its opaque path.
# The carve-out is now off under `whatwg` whenever `scheme_policy =
# "require"` has switched inference off (RUL-014): the same gate `rfc3986`
# already had, because RFC 3986 section 3.1 and the WHATWG scheme state carry
# the identical production. That flipped external vector yal-009
# (`www.php.net:80/index.php?test=1`) from a FILED deviation (RURL-yeikpnan)
# to the WHATWG-correct string, and its `rurl_deviation` accounting and sha256
# pins were re-baselined in the same slice (design/oracle-fixtures.md).
#
# No family is left: the corpus is 274/274, and the `expect_property` below
# runs with an EMPTY deviation set so a regression re-lists its row.

# Assert a property holds of every row except an enumerated deviation set.
# House style, copied from test-rfc3986-serialization-properties.R: the
# comparison is a SET EQUALITY, not a count. An unlisted violation fails, and
# so does a listed row that no longer violates -- so a fix cannot land without
# deleting its entry, and the list cannot rot into a permanent allowance.
expect_property <- function(violates, input, deviations = character(0)) {
  expect_setequal(input[violates], deviations)
}

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
  expect_property(differs, id)
})
