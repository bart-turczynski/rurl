# The FULL official WHATWG conformance suite, scored against the standard's
# own recorded serialization (RURL-yeikpnan, epic RURL-dorofzmb).
#
# WHY THIS FILE EXISTS. Every other WHATWG-side harness in the repo scores a
# CURATED slice: the credential/fragment vectors in
# fixtures/external-url-vectors.csv (43 rows), the Ada `href` vectors (33), the
# paper host oracles (16). Those are adversarial subsets chosen to probe known
# hazards, and a headline drawn from them ("N of 92 rows conform") reads as a
# conformance RATE that the sample does not support -- the failure mode P5.3
# sec 2.2 exists to forbid. This file scores the whole imported suite instead:
# 336 success + 202 failure rows from web-platform-tests
# `url/resources/urltestdata.json`, the WHATWG's own test suite and the closest
# thing to an official conformance metric that exists.
#
# ORACLE. Upstream's `href` field -- the WHATWG serialization of the parsed
# URL, recorded by the standard's suite. NOT a string re-assembled from the
# component getters. The component dump collapses null-vs-empty host and
# absent-vs-present-but-empty query/fragment (all surface as ""), so any
# re-assembly must GUESS the `//`, `?` and `#` delimiters. Measured: scoring
# against a component re-assembly reported 40 differences here, of which 27
# belonged to the guess and not to rurl.
#
# SUBSTRATE. serialize_url() only -- surface (b), the full-string serializer.
# `clean_url` is surface (c), "a policy-driven SEO/canonicalization product;
# NOT a serializer, identity, redirect target, or conformance oracle"
# (output-contracts.md P2.2 sec 1c/5.1) and is barred as a claim substrate by
# P5.3 CLAIM-1. Acceptance and serialization are reported separately: an
# aggregate spanning both would let must-fail rejections inflate a
# serialization figure (P5.3 sec 2.2).

wpt_suite <- function() {
  skip_if_not_installed("jsonlite")
  path <- system.file("bench", "wpt-url-cases.json", package = "rurl")
  skip_if(!nzchar(path) || !file.exists(path), "WPT import not installed")
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

wpt_field <- function(rows, key) {
  vapply(rows, function(x) {
    v <- x[[key]]
    if (is.null(v)) "" else v
  }, character(1))
}

# The four (standard, form) configurations serialize_url() offers.
WPT_CONFIGS <- list(
  c("whatwg", "source"), c("whatwg", "normalized"),
  c("rfc3986", "source"), c("rfc3986", "normalized")
)

test_that("WPT must-fail rows are rejected under WHATWG, in every form", {
  j <- wpt_suite()
  inp <- wpt_field(j$failure, "input")
  expect_length(inp, 202L)

  # Acceptance, not serialization: the only thing a must-fail row can measure.
  for (cfg in WPT_CONFIGS[1:2]) {
    got <- serialize_url(inp, standard = cfg[1], form = cfg[2])
    expect_true(all(is.na(got)))
  }
})

test_that("acceptance does not depend on `form`", {
  # P2.5 OUT-O3: `form` is a PRESENTATION axis. It may change how an accepted
  # URL is rendered; it may never change whether one is accepted.
  j <- wpt_suite()
  for (arm in list(j$success, j$failure)) {
    inp <- wpt_field(arm, "input")
    for (std in c("whatwg", "rfc3986")) {
      src <- serialize_url(inp, standard = std, form = "source")
      nrm <- serialize_url(inp, standard = std, form = "normalized")
      expect_identical(is.na(src), is.na(nrm))
    }
  }
})

test_that("WHATWG has ONE serialization: source and normalized agree", {
  # Not an assumption -- WHATWG defines a single serializer, so the `form` dial
  # must be inert under `standard = "whatwg"`. Re-asserted over the full suite.
  j <- wpt_suite()
  inp <- wpt_field(j$success, "input")
  expect_identical(
    serialize_url(inp, standard = "whatwg", form = "source"),
    serialize_url(inp, standard = "whatwg", form = "normalized")
  )
})

test_that("WPT success rows serialize to the standard's own `href`", {
  j <- wpt_suite()
  s <- j$success
  expect_length(s, 336L)

  inp <- wpt_field(s, "input")
  href <- wpt_field(s, "href")
  # Every success row carries the oracle; a blank would silently pass below.
  expect_true(all(nzchar(href)))

  got <- serialize_url(inp, standard = "whatwg")
  # No success row is rejected: acceptance is total on this arm.
  expect_false(anyNA(got))

  agree <- got == href
  proto <- wpt_field(s, "protocol")
  host <- wpt_field(s, "hostname")
  special <- proto %in% c("http:", "https:", "ws:", "wss:", "ftp:", "file:")

  # Reported by substrate, one population per claim.
  expect_identical(sum(agree), 326L)
  expect_true(all(agree[!special]))                  # 141/141 non-special
  expect_true(all(agree[special & nzchar(host)]))    # 159/159 special + host

  # THE ONE DOCUMENTED DEVIATION FAMILY, enumerated rather than counted, so a
  # regression cannot hide inside a tolerance. rurl parses a host-less `file:`
  # URL to a NULL host, where WHATWG gives every special scheme a non-null host
  # -- the empty string here -- and therefore always serializes the `//`
  # delimiter. rurl emits `file:/x`, the standard `file:///x`.
  #
  # Traced to the Stage-A parse record (`final_host` is NA for these inputs, so
  # host_kind is "absent"), NOT to the serializer, which renders that record
  # faithfully. Per the standing precedent it is filed against the parser and
  # not compensated for downstream. RURL-uhwivndf.
  expect_identical(inp[!agree], c(
    "file:C|/m/", "file:C||/m/", "file:/example.com/", "file:.", "file:/C|/",
    "file:", "file:?q=v", "file:#frag", "file:.//p", "file:/.//p"
  ))
  expect_true(all(proto[!agree] == "file:"))
  expect_true(all(is.na(get_host(inp[!agree]))))
})

test_that("a rooted path starting `//` keeps its `/.` guard", {
  # Regression pin for the defect this suite exposed. The URL serializer must
  # prefix `/.` when the host is null and the path's first segment is empty,
  # or the output re-reads as an empty AUTHORITY instead of a path. The guard
  # derived its segments with strsplit(), which drops a single TRAILING "", so
  # a path of exactly "//" (WHATWG list ["", ""]) measured as one segment and
  # the guard missed -- emitting `non-spec://` for `non-spec:/.//`.
  inp <- c("non-spec:/.//", "non-spec:/..//", "non-spec:/a/..//")
  expect_identical(
    serialize_url(inp, standard = "whatwg"),
    rep("non-spec:/.//", 3L)
  )
  # The guarded output re-parses to itself; the unguarded one did not.
  expect_identical(
    serialize_url(serialize_url(inp, standard = "whatwg"), standard = "whatwg"),
    rep("non-spec:/.//", 3L)
  )
  # Still fires where it always did, and still stays out of the way otherwise.
  expect_identical(serialize_url("non-spec:/.//p", standard = "whatwg"),
                   "non-spec:/.//p")
  expect_identical(serialize_url("non-spec:/a/b", standard = "whatwg"),
                   "non-spec:/a/b")
})
