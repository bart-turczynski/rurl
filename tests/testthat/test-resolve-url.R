# Tests for resolve_url() (RURL-wrfcildb, epic RURL-ehlircjt / parent
# RURL-uyjheruh; PRD v2 §5.6 D6). resolve_url() composes: standard-agnostic
# RFC 3986 §5.2.2 base-merge, then the same safe_parse_urls() machinery for
# host/path/port/query normalization and diagnostics. It adds NO per-standard
# behavior of its own. The return is the CANONICAL clean_url of the resolved
# reference (fragment/userinfo dropped, query per query_handling, port per
# port_handling), which is why several RFC §5.4 expectations below differ from
# a verbatim resolver on the query/fragment components only.

base <- "http://a/b/c/d;p?q"

# --- RFC 3986 §5.4.1 normal examples (path merge is the observable part) -----

test_that("RFC 3986 §5.4.1 normal examples resolve their path/authority", {
  # query/fragment are dropped by clean_url canonicalization, so we assert the
  # scheme+host+path portion the merge is responsible for.
  expect_identical(resolve_url("g", base), "http://a/b/c/g")
  expect_identical(resolve_url("./g", base), "http://a/b/c/g")
  expect_identical(resolve_url("g/", base), "http://a/b/c/g/")
  expect_identical(resolve_url("/g", base), "http://a/g")
  expect_identical(resolve_url("//g", base), "http://g/")
  expect_identical(resolve_url("g?y", base), "http://a/b/c/g")
  expect_identical(resolve_url("g#s", base), "http://a/b/c/g")
  expect_identical(resolve_url(".", base), "http://a/b/c/")
  expect_identical(resolve_url("./", base), "http://a/b/c/")
  expect_identical(resolve_url("..", base), "http://a/b/")
  expect_identical(resolve_url("../", base), "http://a/b/")
  expect_identical(resolve_url("../g", base), "http://a/b/g")
  expect_identical(resolve_url("../..", base), "http://a/")
  expect_identical(resolve_url("../../g", base), "http://a/g")
})

test_that("empty / fragment-only / query-only references resolve the base", {
  # Empty ref, fragment-only ref, and query-only ref all reduce to the base's
  # resource once the (dropped-by-default) fragment/query are set aside.
  expect_identical(resolve_url("", base), "http://a/b/c/d;p")
  expect_identical(resolve_url("#s", base), "http://a/b/c/d;p")
  expect_identical(resolve_url("?y", base), "http://a/b/c/d;p")
})

test_that("RFC 3986 §5.4.2 abnormal examples clamp excess ..", {
  expect_identical(resolve_url("../../../g", base), "http://a/g")
  expect_identical(resolve_url("../../../../g", base), "http://a/g")
  expect_identical(resolve_url("/./g", base), "http://a/g")
  expect_identical(resolve_url("/../g", base), "http://a/g")
  # A trailing dot segment resolves to the containing directory.
  expect_identical(resolve_url("g.", base), "http://a/b/c/g.")
  expect_identical(resolve_url(".g", base), "http://a/b/c/.g")
  expect_identical(resolve_url("g..", base), "http://a/b/c/g..")
  expect_identical(resolve_url("./../g", base), "http://a/b/g")
  expect_identical(resolve_url("./g/.", base), "http://a/b/c/g/")
  expect_identical(resolve_url("g/./h", base), "http://a/b/c/g/h")
  expect_identical(resolve_url("g/../h", base), "http://a/b/c/h")
})

# --- Absolute references ignore the base -------------------------------------

test_that("an absolute reference ignores the base entirely", {
  expect_identical(
    resolve_url("https://x.com/y/z", base), "https://x.com/y/z"
  )
  # Base may even be NA when the reference is absolute.
  expect_identical(
    resolve_url("http://x.com/y", NA_character_), "http://x.com/y"
  )
  # Dot segments in an absolute reference are still resolved.
  expect_identical(
    resolve_url("http://x.com/a/b/../c", "http://a/b"), "http://x.com/a/c"
  )
})

test_that("an unsupported-scheme absolute ref yields NA (rurl allowlist)", {
  # "g:h" resolves (per RFC) to the opaque "g:h", but rurl only canonicalizes
  # http/https/ftp/ftps/file, so clean_url is NA -- same as parsing "g:h"
  # directly.
  expect_true(is.na(resolve_url("g:h", base)))
  expect_identical(resolve_url("g:h", base), get_clean_url("g:h"))
})

# --- Base must be absolute for a relative reference --------------------------

test_that("a relative reference needs an absolute base", {
  expect_true(is.na(resolve_url("g", NA_character_)))
  expect_true(is.na(resolve_url("g", "not-a-url/path")))
  expect_true(is.na(resolve_url("../x", "//host/only")))
})

# --- Query / fragment are canonicalized like clean_url elsewhere -------------

test_that("query follows query_handling; merge is genuinely computed", {
  # With query_handling = "keep" the resolved query becomes observable, proving
  # the query-only and query-bearing merges compute the right query.
  expect_identical(
    resolve_url("?y", base, query_handling = "keep"), "http://a/b/c/d;p?y="
  )
  expect_identical(
    resolve_url("g?y", base, query_handling = "keep"), "http://a/b/c/g?y="
  )
  # An empty reference inherits the base query.
  expect_identical(
    resolve_url("", base, query_handling = "keep"), "http://a/b/c/d;p?q="
  )
  # A reference with its own query does NOT inherit the base query.
  expect_identical(
    resolve_url("g?a=1", "http://a/b/c?q", query_handling = "keep"),
    "http://a/b/g?a=1"
  )
})

# --- url_standard pass-through: no divergent behavior of its own -------------

test_that("port rendering flows through safe_parse_urls", {
  expect_identical(
    resolve_url("/p", "http://a:80/b", port_handling = "strip_default",
                url_standard = "whatwg"),
    "http://a/p"
  )
  expect_identical(
    resolve_url("/p", "http://a:80/b", port_handling = "keep",
                url_standard = "whatwg"),
    "http://a:80/p"
  )
  expect_identical(
    resolve_url("/p", "http://a:80/b", port_handling = "keep",
                url_standard = "rfc3986"),
    "http://a:80/p"
  )
  expect_identical(
    resolve_url("/p", "http://a:8080/b", port_handling = "keep",
                url_standard = "whatwg"),
    "http://a:8080/p"
  )
})

test_that("WHATWG backslash-as-slash flows from url_standard", {
  # Backslash recognition is applied by the downstream parser on the resolved
  # absolute URL, governed by url_standard exactly as in safe_parse_url.
  expect_identical(
    resolve_url("g\\h", "http://a/b/c/", url_standard = "whatwg"),
    "http://a/b/c/g/h"
  )
  # rfc3986 does NOT map "\" to "/" -- and, since RURL-qrfrvmkg, does not carry
  # the raw byte either: the resolved absolute URL `http://a/b/c/g\h` matches no
  # RFC 3986 production, so the downstream parse rejects it. The no-selector
  # default is un-governed and still returns it verbatim, which is what shows
  # the rewrite genuinely did not happen.
  expect_true(is.na(
    resolve_url("g\\h", "http://a/b/c/", url_standard = "rfc3986")
  ))
  expect_identical(
    resolve_url("g\\h", "http://a/b/c/"),
    "http://a/b/c/g\\h"
  )
})

# --- P2.7 D-B: a same-special-scheme reference is RELATIVE under whatwg -------
#
# WHATWG's "special relative or authority state" consumes a scheme equal to the
# base's when that scheme is special and keeps parsing against the base. RFC
# 3986 section 5.2.2 has no such rule -- any scheme makes the reference
# absolute -- so this is the first genuinely standard-divergent behavior in the
# MERGE, and it retires the standard-agnostic claim PRD v2 D6 made. The 274-row
# conformance measurement lives in test-wpt-base-relative.R; these are the
# axis-by-axis assertions.

test_that("whatwg reads the base's own special scheme as relative", {
  wbase <- "http://example.org/foo/bar"
  # The worked example from P2.7 D-B section 3.
  expect_identical(
    resolve_url("http:foo.com", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://example.org/foo/foo.com"
  )
  # An absolute-path remainder roots against the BASE's authority, not its own.
  expect_identical(
    resolve_url("http:/example.com/", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://example.org/example.com/"
  )
  # A bare "scheme:" is the empty reference: base path AND base query survive.
  expect_identical(
    resolve_url("http:", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://example.org/foo/bar"
  )
  # The scheme match is ASCII case-insensitive on both sides.
  expect_identical(
    resolve_url("HTTP:g", "http://example.org/foo/bar",
                url_standard = "whatwg", output = "serialized"),
    "http://example.org/foo/g"
  )
})

test_that("the consumed scheme is not re-read out of the remainder", {
  # `.split_after_scheme()` drops the scheme production deliberately: re-running
  # the full splitter would re-read "C:" / ":" as a scheme of the remainder.
  expect_identical(
    resolve_url("file:C:/", "file://host/", url_standard = "whatwg",
                output = "serialized"),
    "file://host/C:/"
  )
  expect_identical(
    resolve_url("http::@c:29", "http://example.org/foo/bar",
                url_standard = "whatwg", output = "serialized"),
    "http://example.org/foo/:@c:29"
  )
})

test_that("only the base's OWN special scheme is consumed", {
  wbase <- "http://example.org/foo/bar"
  # A DIFFERENT scheme stays absolute even when it is also special ...
  expect_identical(
    resolve_url("https:foo.com", wbase, url_standard = "whatwg",
                output = "serialized"),
    "https://foo.com/"
  )
  # ... and so does a matching scheme that is not special. `ftps` is rurl's own
  # addition and is NOT a WHATWG special scheme, so the rule must not fire.
  expect_identical(
    resolve_url("ftps:foo.com", "ftps://example.org/foo/bar",
                url_standard = "whatwg", output = "serialized"),
    "ftps:foo.com"
  )
})

test_that("file:// keeps its own two-slash authority entry", {
  # The five non-file special schemes reach "special authority ignore slashes"
  # and skip a run of any length; `file` has its own state machine that
  # consumes exactly two, so a third slash is an EMPTY host plus a path.
  expect_identical(
    resolve_url("file:///foo/bar.txt", "file:///tmp/mock/path",
                url_standard = "whatwg", output = "serialized"),
    "file:///foo/bar.txt"
  )
  expect_identical(
    resolve_url("http:///example.com/p", "http://example.org/foo/bar",
                url_standard = "whatwg", output = "serialized"),
    "http://example.com/p"
  )
})

test_that("rfc3986 keeps the same-scheme reference ABSOLUTE", {
  wbase <- "http://example.org/foo/bar"
  # The other half of the axis: under rfc3986 the reference is absolute and the
  # base is irrelevant, which is RFC 3986 section 5.2.2's first branch.
  expect_identical(
    resolve_url("http:foo.com", wbase, url_standard = "rfc3986",
                output = "serialized"),
    "http:foo.com"
  )
  # On the clean surface that same absolute `http:` reference carries no
  # authority, which rurl's parser rejects exactly as it does a direct parse --
  # pre-existing behavior, unchanged by this rule.
  expect_identical(
    resolve_url("http:foo.com", wbase, url_standard = "rfc3986"),
    get_clean_url("http:foo.com", url_standard = "rfc3986")
  )
  # ... and the raw merge itself, one layer below the serializer.
  expect_identical(
    rurl:::.resolve_one_raw("http:foo.com", wbase, "rfc3986"),
    "http:foo.com"
  )
  expect_identical(
    rurl:::.resolve_one_raw("http:foo.com", wbase, "whatwg"),
    "http://example.org/foo/foo.com"
  )
})

test_that("url_standard = NULL is byte-frozen against the new rule", {
  # ADR 0007 / P2.7 D-C: every reference-parsing rule is reachable ONLY through
  # url_standard = "whatwg". At NULL the same inputs keep RFC 3986's answer.
  wbase <- "http://example.org/foo/bar"
  # The raw merge -- the locus of the change -- is untouched at NULL, and the
  # internal's DEFAULT argument is that frozen path rather than the new one.
  expect_identical(
    rurl:::.resolve_one_raw("http:foo.com", wbase), "http:foo.com"
  )
  expect_identical(
    rurl:::.resolve_one_raw("http:foo.com", wbase),
    rurl:::.resolve_one_raw("http:foo.com", wbase, NULL)
  )
  expect_identical(
    rurl:::.resolve_one_raw("file:test", "file:///tmp/mock/path"), "file:test"
  )
  # ... and so is the clean surface it feeds. These references resolve to an
  # authority-less absolute URL that rurl's parser rejects, exactly as a direct
  # parse of it does; the NA is pre-existing behavior, not a new one.
  expect_identical(
    resolve_url("http:foo.com", wbase), get_clean_url("http:foo.com")
  )
  expect_true(is.na(resolve_url("http:foo.com", wbase)))
  expect_true(is.na(resolve_url("file:test", "file:///tmp/mock/path")))
  # The whatwg selector is the ONLY way to reach the new rule: same input, same
  # surface, and only there does it resolve against the base.
  expect_identical(
    resolve_url("http:foo.com", wbase, url_standard = "whatwg"),
    "http://example.org/foo/foo.com"
  )
  expect_identical(
    resolve_url("file:test", "file:///tmp/mock/path", url_standard = "whatwg"),
    "file:///tmp/mock/test"
  )
})

# --- P2.7 D-B: reference PREPROCESSING under whatwg (RURL-fupsemxr T2.4) ------
#
# Two more rules WHATWG applies while parsing the REFERENCE that RFC 3986
# section 5 has no equivalent for, both reachable through the "whatwg" selector
# only. The 274-row conformance measurement lives in test-wpt-base-relative.R;
# these are the axis-by-axis assertions.

test_that("under a special base a leading backslash is a slash", {
  wbase <- "http://example.org/foo/bar"
  # WHATWG "relative slash state": one leading `\` roots the path exactly as
  # `/` does -- it does NOT merge against the base path as a normal byte would.
  expect_identical(
    resolve_url("\\x", wbase, url_standard = "whatwg", output = "serialized"),
    "http://example.org/x"
  )
  # And it is the state, not a blanket byte rewrite: a `\` that is not in the
  # leading run stays a path byte here and is recognized downstream instead.
  expect_identical(
    resolve_url("\\?q", wbase, url_standard = "whatwg", output = "serialized"),
    "http://example.org/?q"
  )
  expect_identical(
    resolve_url("\\#f", wbase, url_standard = "whatwg", output = "serialized"),
    "http://example.org/#f"
  )
})

test_that("a leading run of slash-or-backslash introduces an authority", {
  wbase <- "http://example.org/foo/bar"
  # A SECOND slash-or-backslash enters the authority, and for the five
  # non-`file` special schemes "special authority ignore slashes" then skips the
  # whole run: these two references differ only in which byte was typed.
  expect_identical(
    resolve_url("\\\\x\\hello", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://x/hello"
  )
  expect_identical(
    resolve_url("/\\/\\//example.org/../path", "http://example.org/",
                url_standard = "whatwg", output = "serialized"),
    "http://example.org/path"
  )
  expect_identical(
    resolve_url("///example.org/../path", "http://example.org/",
                url_standard = "whatwg", output = "serialized"),
    "http://example.org/path"
  )
  # `file` consumes exactly TWO (its own file-slash chain), so a third slash is
  # an EMPTY host plus a path -- the same asymmetry `file:` references have.
  expect_identical(
    resolve_url("/\\server/file", "file:///tmp/mock/path",
                url_standard = "whatwg", output = "serialized"),
    "file://server/file"
  )
  expect_identical(
    resolve_url("///foo/bar", "file:///tmp/mock/path",
                url_standard = "whatwg", output = "serialized"),
    "file:///foo/bar"
  )
})

test_that("a NON-special base gets neither rule", {
  # Every rule here is a state WHATWG reaches only from a special scheme, so a
  # non-special base keeps RFC 3986's reading even under the whatwg selector.
  expect_identical(
    rurl:::.resolve_one_raw("\\x", "non-spec://h/p", "whatwg"),
    "non-spec://h/\\x"
  )
  expect_identical(
    rurl:::.resolve_one_raw("///a/b", "non-spec://h/p", "whatwg"),
    "non-spec:///a/b"
  )
})

test_that("whatwg strips C0-or-space from the reference before reading it", {
  wbase <- "http://example.org/foo/bar"
  # Step 1a: a leading/trailing C0-control-or-SPACE run is removed, so this
  # resolves exactly as the unpadded reference does.
  expect_identical(
    resolve_url("  foo.com  ", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://example.org/foo/foo.com"
  )
  # Step 1b: every ASCII tab/LF/CR, anywhere in the reference.
  expect_identical(
    resolve_url("\tg\nh\r", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://example.org/foo/gh"
  )
  # A reference that strips to EMPTY is the empty reference -- base minus its
  # fragment (RFC 3986 section 5.2.2), not a merge of the whitespace.
  expect_identical(
    resolve_url("   \t", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://example.org/foo/bar"
  )
  expect_identical(
    resolve_url("   \t", "http://example.org/foo/bar?q#old",
                url_standard = "whatwg", output = "serialized"),
    "http://example.org/foo/bar?q"
  )
  # The strip happens BEFORE the scheme is read, so a padded `:` is not one.
  expect_identical(
    resolve_url("\t   :foo.com   \n", wbase, url_standard = "whatwg",
                output = "serialized"),
    "http://example.org/foo/:foo.com"
  )
})

test_that("rfc3986 and NULL are byte-frozen against BOTH new rules", {
  # ADR 0007 / P2.7 D-C. Asserted on the raw merge, which is the locus of the
  # change: `\` stays an ordinary path byte, `//` is the whole authority
  # production, and no byte is stripped from the reference.
  wbase <- "http://example.org/foo/bar"
  frozen <- list(
    c("\\x", "http://example.org/foo/\\x"),
    c("\\\\x\\hello", "http://example.org/foo/\\\\x\\hello"),
    c("///example.org/../path", "http:///path"),
    c("  foo.com  ", "http://example.org/foo/  foo.com  "),
    c("   \t", "http://example.org/foo/   \t")
  )
  for (case in frozen) {
    expect_identical(
      rurl:::.resolve_one_raw(case[[1L]], wbase, "rfc3986"), case[[2L]]
    )
    # NULL is the DEFAULT argument as well as an explicit value; both are the
    # frozen path, and neither may drift from "rfc3986" here.
    expect_identical(rurl:::.resolve_one_raw(case[[1L]], wbase), case[[2L]])
    expect_identical(
      rurl:::.resolve_one_raw(case[[1L]], wbase, NULL), case[[2L]]
    )
  }
  # ... and the clean surface the NULL merge feeds: identical to a direct parse
  # of the un-stripped, un-rewritten recomposition, which is what shows neither
  # rule fired. (The padded reference resolves to a URL rurl's parser rejects at
  # NULL -- pre-existing behavior, and exactly what the direct parse gives.)
  expect_identical(
    resolve_url("  foo.com  ", wbase),
    get_clean_url("http://example.org/foo/  foo.com  ")
  )
  expect_true(is.na(resolve_url("  foo.com  ", wbase)))
  expect_identical(resolve_url("\\x", wbase), "http://example.org/foo/\\x")
})

test_that("resolved output equals a direct parse of the resolved URL", {
  # The composition contract: resolving then reading clean_url is identical to
  # parsing the RFC-recomposed URL directly, for every governed axis.
  expect_identical(
    resolve_url("../g?x=1", base, query_handling = "keep",
                url_standard = "rfc3986"),
    get_clean_url("http://a/b/g?x=1", query_handling = "keep",
                  url_standard = "rfc3986")
  )
})

# --- url_standard conflict check across the `...` seam -----------------------

test_that("a governed knob conflicting with url_standard errors", {
  expect_error(
    resolve_url("g", base, url_standard = "whatwg", case_handling = "upper"),
    "governs `case_handling`"
  )
  expect_error(
    resolve_url("g", base, url_standard = "rfc3986",
                path_normalization = "collapse_slashes"),
    "governs `path_normalization`"
  )
  # The value the profile would pick is accepted.
  expect_identical(
    resolve_url("g", base, url_standard = "rfc3986",
                path_normalization = "dot_segments"),
    "http://a/b/c/g"
  )
})

# --- Vectorization, recycling, and NA handling -------------------------------

test_that("resolve_url() is vectorized and recycles the base", {
  expect_identical(
    resolve_url(c("g", "../h", "/i", "//j/k"), "http://a/b/c/"),
    c("http://a/b/c/g", "http://a/b/h", "http://a/i", "http://j/k")
  )
  # A vector of bases, recycled against a scalar reference.
  expect_identical(
    resolve_url("g", c("http://a/b/", "http://x/y/")),
    c("http://a/b/g", "http://x/y/g")
  )
})

test_that("NA and empty inputs propagate to NA / empty output", {
  expect_identical(
    resolve_url(c("g", NA_character_), "http://a/b/c/"),
    c("http://a/b/c/g", NA_character_)
  )
  expect_identical(resolve_url(character(0), "http://a/b/"), character(0))
})

# --- output = "serialized": the standards surface (P2.7 D-A) -----------------
#
# `clean_url` (surface c) is intentionally lossy and is barred from carrying a
# conformance claim; `serialize_url()` (surface b) is the standards surface.
# `output = "serialized"` routes the RESOLVED ABSOLUTE STRING to the latter, so
# RFC 3986 §5.4's own expectations are reproducible verbatim -- which they are
# not on the clean surface, where the query and fragment are dropped.

test_that("RFC 3986 §5.4 query/fragment cases are exact under serialized", {
  # The three cases the clean surface cannot express. RFC 3986 §5.4.1 gives
  # "?y" -> http://a/b/c/d;p?y, "#s" -> http://a/b/c/d;p?q#s, and the empty
  # reference -> http://a/b/c/d;p?q.
  expect_identical(
    resolve_url("?y", base, url_standard = "rfc3986", output = "serialized"),
    "http://a/b/c/d;p?y"
  )
  expect_identical(
    resolve_url("#s", base, url_standard = "rfc3986", output = "serialized"),
    "http://a/b/c/d;p?q#s"
  )
  expect_identical(
    resolve_url("", base, url_standard = "rfc3986", output = "serialized"),
    "http://a/b/c/d;p?q"
  )
  # The same three under whatwg: this trio is merge-only, so both standards
  # serialize it identically.
  expect_identical(
    resolve_url(c("?y", "#s", ""), base, url_standard = "whatwg",
                output = "serialized"),
    c("http://a/b/c/d;p?y", "http://a/b/c/d;p?q#s", "http://a/b/c/d;p?q")
  )
})

test_that("output = \"serialized\" requires an explicit url_standard", {
  expect_error(
    resolve_url("g", base, output = "serialized"),
    "requires an explicit `url_standard`"
  )
  # ... and NULL is the only rejected value; both profiles are accepted.
  expect_identical(
    resolve_url("g", base, url_standard = "whatwg", output = "serialized"),
    "http://a/b/c/g"
  )
})

test_that("a presentation dial cannot be silently discarded by serialized", {
  # serialize_url() takes NO presentation arguments, so a `...` dial provably
  # cannot apply; accepting and dropping it would misreport the return.
  expect_error(
    resolve_url("g", base, url_standard = "whatwg", output = "serialized",
                port_handling = "keep"),
    "accepts no parse or presentation options"
  )
  expect_error(
    resolve_url("g", base, url_standard = "rfc3986", output = "serialized",
                query_handling = "keep"),
    "drop `query_handling`"
  )
  # The same dial is honored on the clean surface, which is what makes the
  # error above a refusal rather than a limitation.
  expect_identical(
    resolve_url("g?a=1", base, url_standard = "rfc3986",
                query_handling = "keep"),
    "http://a/b/c/g?a=1"
  )
})

test_that("form passes through for rfc3986 and is inert for whatwg", {
  # Source-preserving keeps the %7E triplet and the host's source case;
  # normalized applies RFC 3986 §6.2.2/§6.2.3 (case, unreserved decoding,
  # default-port elision).
  expect_identical(
    resolve_url("%7Euser/x", "HTTP://Example.COM:80/b/",
                url_standard = "rfc3986", output = "serialized"),
    "http://Example.COM:80/b/%7Euser/x"
  )
  expect_identical(
    resolve_url("%7Euser/x", "HTTP://Example.COM:80/b/",
                url_standard = "rfc3986", output = "serialized",
                form = "normalized"),
    "http://example.com/b/~user/x"
  )
  # The default is "source".
  expect_identical(
    resolve_url("%7Euser/x", "HTTP://Example.COM:80/b/",
                url_standard = "rfc3986", output = "serialized",
                form = "source"),
    resolve_url("%7Euser/x", "HTTP://Example.COM:80/b/",
                url_standard = "rfc3986", output = "serialized")
  )
  # whatwg has a single spec-defined serializer form, so `form` is inert --
  # exactly serialize_url()'s own contract.
  expect_identical(
    resolve_url("%7Euser/x", "HTTP://Example.COM:80/b/",
                url_standard = "whatwg", output = "serialized",
                form = "source"),
    resolve_url("%7Euser/x", "HTTP://Example.COM:80/b/",
                url_standard = "whatwg", output = "serialized",
                form = "normalized")
  )
})

test_that("serialized keeps the fragment and credentials clean_url drops", {
  # ONE input, both surfaces, side by side: this is the whole observable
  # difference between surface (b) and surface (c).
  cred_base <- "http://u:pw@example.com/a?q=1"
  expect_identical(
    resolve_url("#frag", cred_base, url_standard = "whatwg",
                output = "serialized"),
    "http://u:pw@example.com/a?q=1#frag"
  )
  expect_identical(
    resolve_url("#frag", cred_base),
    "http://example.com/a"
  )
})

test_that("serialized is vectorized, recycles, and propagates NA", {
  expect_identical(
    resolve_url(c("g", "../h", "/i", "//j/k"), "http://a/b/c/",
                url_standard = "whatwg", output = "serialized"),
    c("http://a/b/c/g", "http://a/b/h", "http://a/i", "http://j/k")
  )
  # Scalar reference recycled against a vector of bases.
  expect_identical(
    resolve_url("g", c("http://a/b/", "http://x/y/"),
                url_standard = "rfc3986", output = "serialized"),
    c("http://a/b/g", "http://x/y/g")
  )
  # NA reference, and a relative reference with no absolute base, both NA.
  expect_identical(
    resolve_url(c("g", NA_character_, "../x"),
                c("http://a/b/c/", "http://a/b/c/", "//host/only"),
                url_standard = "whatwg", output = "serialized"),
    c("http://a/b/c/g", NA_character_, NA_character_)
  )
  expect_identical(
    resolve_url(character(0), "http://a/b/", url_standard = "whatwg",
                output = "serialized"),
    character(0)
  )
  # Names are not data on this surface either (RURL-vhdsqaln).
  expect_null(names(
    resolve_url(c(a = "/p", b = "q.html"), "http://example.com/base/",
                url_standard = "whatwg", output = "serialized")
  ))
})

test_that("the default output surface is unchanged by the new argument", {
  refs <- c("g", "./g", "g/", "/g", "//g", "g?y", "g#s", ".", "..", "../g", "")
  expect_identical(
    resolve_url(refs, base),
    resolve_url(refs, base, output = "clean")
  )
  # `form` is inert on the clean surface.
  expect_identical(
    resolve_url(refs, base),
    resolve_url(refs, base, form = "normalized")
  )
  # And explicit "clean" changes nothing about the `...` seam or url_standard.
  expect_identical(
    resolve_url("/p", "http://a:80/b", port_handling = "keep",
                url_standard = "whatwg", output = "clean"),
    "http://a:80/p"
  )
})

test_that("url_standard = NULL stays byte-frozen under the new signature", {
  # ADR 0007 D-C: the NULL selector does not move. output = "serialized" is an
  # error at NULL rather than a second NULL rendering.
  expect_identical(resolve_url("g\\h", "http://a/b/c/"), "http://a/b/c/g\\h")
  expect_identical(
    resolve_url("g\\h", "http://a/b/c/", output = "clean"),
    "http://a/b/c/g\\h"
  )
  expect_error(
    resolve_url("g\\h", "http://a/b/c/", output = "serialized"),
    "requires an explicit `url_standard`"
  )
})

test_that("an invalid output value is rejected by match.arg", {
  expect_error(resolve_url("g", base, output = "raw"))
})

# --- IDN / host model pass-through -------------------------------------------

test_that("host encoding and IDN handling flow through to the resolved host", {
  # host_encoding governs the resolved host spelling as in safe_parse_url.
  expect_identical(
    resolve_url("/p", "http://münchen.de/x", host_encoding = "idna"),
    get_clean_url("http://münchen.de/p", host_encoding = "idna")
  )
  expect_identical(
    resolve_url("/p", "http://münchen.de/x", host_encoding = "unicode"),
    get_clean_url("http://münchen.de/p", host_encoding = "unicode")
  )
})
