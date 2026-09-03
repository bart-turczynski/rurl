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
  # The fixture SHIPS in the tarball (RURL-mifbbrez: it was .Rbuildignore'd,
  # so under `R CMD check` every block below skipped and the package's most
  # load-bearing conformance claim was vacuous exactly where the built
  # artifact is checked). This skip now guards only a broken install -- an
  # empty path here means inst/bench/ did not install, never "not built in".
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

  # Reported by substrate, one population per claim. NO deviation family
  # remains: every success row serializes to the standard's own `href`.
  expect_identical(sum(agree), 336L)
  expect_true(all(agree[!special]))                  # 141/141 non-special
  expect_true(all(agree[special & nzchar(host)]))    # 159/159 special + host
  expect_true(all(agree))
})

test_that("a host-less `file:` URL carries an EMPTY, not a null, host", {
  # The former deviation family (RURL-uhwivndf), kept as its own enumerated pin
  # so a regression names itself instead of moving a count. WHATWG's file state
  # gives every `file:` URL a non-null host -- the empty string when no
  # authority is written -- and the serializer therefore always emits `//`.
  #
  # Fixed in the PARSE record (`.parse_whatwg_file_urls_vec()` now records the
  # empty host), not compensated for in the serializer, per the standing
  # precedent. The serializer's `//` condition moved with it, from the source
  # delimiter fact to the standard's own rule (a non-null host): P1.3,
  # superseding P1.2 D-C for the WHATWG serializer.
  inp <- c(
    "file:C|/m/", "file:C||/m/", "file:/example.com/", "file:.", "file:/C|/",
    "file:", "file:?q=v", "file:#frag", "file:.//p", "file:/.//p"
  )
  expect_identical(serialize_url(inp, standard = "whatwg"), c(
    "file:///C:/m/", "file:///C||/m/", "file:///example.com/", "file:///",
    "file:///C:/", "file:///", "file:///?q=v", "file:///#frag",
    "file:////p", "file:////p"
  ))
  # The parse RECORD is what moved: `host_kind` is now "empty" (a present,
  # zero-length host) where it read "absent". `localhost` maps to that same
  # empty host, and a real file host is untouched.
  expect_identical(
    .fsss_record_vec(inp, "whatwg")$host_kind, rep("empty", length(inp))
  )
  expect_identical(
    .fsss_record_vec(
      c("file:///p", "file://localhost/p", "file://h/p", "foo:/bar"), "whatwg"
    )$host_kind,
    c("empty", "empty", "present", "absent")
  )
  # NOT moved, and deliberately: the public `host` column still maps an empty
  # host to NA (`.assemble_parse_result_vec`). That collapse is a general
  # accessor rule -- `foo:///bar` reports NA too -- not the `file:` defect, so
  # widening it is a separate surface decision and is not taken here.
  expect_true(all(is.na(get_host(inp, url_standard = "whatwg"))))
  # RFC 3986 has no such rule: `file:/example.com/` genuinely carries no
  # authority there, and both the host and the serialization stay put.
  expect_identical(get_host("file:/example.com/", url_standard = "rfc3986"),
                   NA_character_)
  expect_identical(
    serialize_url("file:/example.com/", standard = "rfc3986"),
    "file:/example.com/"
  )
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
