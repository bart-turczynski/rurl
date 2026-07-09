# Conformance fixtures + WPT reference corpus + regression assertions
# (RURL-cuqafsif, epic RURL-eqzkkohm; PRD §9). The spec-conformance backstop:
# every row below is checked against RFC 3986 grammar/normalization rules or a
# pinned WHATWG reference (the web-platform-tests `url/resources/
# urltestdata.json` corpus, https://github.com/web-platform-tests/wpt), not
# just against rurl's own current output. WPT-sourced rows record the exact
# `input` string from that file in `source_reference` so a future re-pin can
# re-locate them.
#
# Percent-triplet hex case is canonicalized by the RFC profile where RFC 3986
# normalization applies. The WHATWG profile preserves existing percent spelling
# byte-for-byte, matching the URL Standard serializer.
#
# Most RFC-side rows are derived from RFC 3986 grammar/normalization prose
# (there is no RFC equivalent of WPT's machine-checkable corpus). The
# `path-rfc-5-4-*` rows are the exception: RFC 3986 §5.4's "abnormal examples"
# table gives worked reference-resolution vectors against a base URI, and two
# of them (`/./g`, `/../g`) use an absolute-path reference, which §5.3 routes
# straight to `remove_dot_segments()` with no base merge -- so those two are
# genuine RFC-text-verbatim, base-independent dot-segment vectors, not a
# hand-derivation.

# --- Golden fixture table (PRD §9.1) ----------------------------------------

test_that("url_standard conformance fixtures match pinned expectations", {
  path <- testthat::test_path("fixtures", "url-standard-conformance.csv")
  fx <- utils::read.csv(
    path, stringsAsFactors = FALSE, colClasses = "character", na.strings = "NA"
  )

  split_tokens <- function(x) {
    if (is.na(x)) character(0) else strsplit(x, "|", fixed = TRUE)[[1]]
  }

  for (i in seq_len(nrow(fx))) {
    row <- fx[i, ]
    label <- sprintf("[%s] %s (%s): %s", row$id, row$input, row$url_standard,
      row$notes)

    expect_identical(
      get_clean_url(row$input, url_standard = row$url_standard),
      row$expected_clean_url, label = paste("clean_url", label)
    )
    expect_identical(
      get_host(row$input, url_standard = row$url_standard),
      row$expected_host, label = paste("host", label)
    )
    expect_identical(
      get_host_type(row$input, url_standard = row$url_standard),
      row$expected_host_type, label = paste("host_type", label)
    )
    expect_identical(
      get_path(row$input, url_standard = row$url_standard),
      row$expected_path, label = paste("path", label)
    )
    expect_setequal(
      get_url_diagnostics(row$input, url_standard = row$url_standard),
      split_tokens(row$expected_diagnostics)
    )
  }
})

# --- Required regression assertions (PRD §9.3) ------------------------------

eq <- function(a, b, ...) {
  identical(get_clean_url(a, ...), get_clean_url(b, ...))
}

test_that("PRD §9.3 required regression assertions hold", {
  expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
    url_standard = "whatwg"))
  expect_false(eq("http://ex.com/a%2Fb", "http://ex.com/a/b",
    url_standard = "rfc3986"))

  expect_true(eq("http://ex.com/%41%42", "http://ex.com/AB",
    url_standard = "rfc3986"))
  expect_false(eq("http://ex.com/%41%42", "http://ex.com/AB",
    url_standard = "whatwg"))

  expect_equal(get_host("http://2130706433/", url_standard = "rfc3986"),
    "2130706433")
  expect_equal(get_host("http://2130706433/", url_standard = "whatwg"),
    "127.0.0.1")

  # path accessor honors the profile percent policy directly
  expect_equal(get_path("http://ex.com/%41%42", url_standard = "rfc3986"),
    "/AB")
  expect_equal(get_path("http://ex.com/%41%42", url_standard = "whatwg"),
    "/%41%42")

  # diagnostics fire in BOTH modes for numeric-looking hosts (facts, not policy)
  expect_true("ipv4-non-dotted" %in%
    get_url_diagnostics("http://2130706433/", url_standard = "whatwg"))
  expect_true("ipv4-number-form" %in%
    get_url_diagnostics("http://2130706433/", url_standard = "rfc3986"))

  # host_type is (host, url_standard)-dependent
  expect_equal(get_host_type("http://2130706433/", url_standard = "rfc3986"),
    "reg-name")
  expect_equal(get_host_type("http://2130706433/", url_standard = "whatwg"),
    "ipv4")

  # NULL selector leaves the output shape untouched (no new columns/fields)
  expect_named(
    safe_parse_urls("http://ex.com/", url_standard = NULL),
    names(safe_parse_urls("http://ex.com/"))
  )
})

# --- RURL-cdjnhnvf: WHATWG "ends in a number" hosts that libcurl leaves as
# reg-names (mixed reg-name/number, hex/octal final labels, trailing-dot forms,
# >4 parts) must reject under whatwg -- not slip through as warning-invalid-tld.
# rfc3986/NULL keep the reg-name (RFC 3986 has no numeric-host rule).

test_that("WHATWG rejects obfuscated numeric hosts libcurl leaves literal", {
  bucket_a <- c(
    "http://foo.09", "http://foo.0x4", "http://0x1.2.3.4.5",
    "http://0x1.2.3.4.5.", "http://0x100.2.3.4.", "http://1.2.3.08",
    "http://1.2.3.08.", "http://\U0001F4A9.123"
  )
  for (u in bucket_a) {
    expect_identical(
      get_parse_status(u, url_standard = "whatwg"), "error",
      label = paste("whatwg must reject", u)
    )
    expect_true(
      is.na(get_clean_url(u, url_standard = "whatwg")),
      label = paste("whatwg clean_url NA for", u)
    )
    # rfc3986 is unchanged: a numeric-looking reg-name with no valid TLD.
    expect_identical(
      get_parse_status(u, url_standard = "rfc3986"), "warning-invalid-tld",
      label = paste("rfc3986 keeps reg-name for", u)
    )
    expect_false(
      is.na(get_clean_url(u, url_standard = "rfc3986")),
      label = paste("rfc3986 clean_url non-NA for", u)
    )
  }

  # Guardrails: valid numeric hosts still parse, and reg-names whose final
  # label is NOT a number are untouched (no false rejections).
  expect_identical(get_parse_status("http://2130706433/",
    url_standard = "whatwg"), "ok")
  expect_identical(get_parse_status("http://192.168.010.1/",
    url_standard = "whatwg"), "ok")
  expect_identical(get_parse_status("http://foo.0x4g",
    url_standard = "whatwg"), "warning-invalid-tld")
})

# --- get_path()/get_host() honor the selector consistently with get_clean_url
# (AC #8): spot-check the remaining accessors on a representative fixture row.

test_that("get_parse_status/domain/tld/subdomain honor the standard (AC #8)", {
  # WHATWG-fatal numeric host: every accessor reports the failure consistently.
  u <- "http://256.1.1.1/"
  expect_identical(get_parse_status(u, url_standard = "whatwg"), "error")
  expect_true(is.na(get_domain(u, url_standard = "whatwg")))
  expect_true(is.na(get_tld(u, url_standard = "whatwg")))
  expect_true(is.na(get_subdomain(u, url_standard = "whatwg")))
  # Under RFC the reg-name parses (not fatal), but a numeric-looking reg-name
  # has no valid TLD, so it carries the existing invalid-TLD warning status.
  expect_identical(
    get_parse_status(u, url_standard = "rfc3986"), "warning-invalid-tld"
  )
})

# --- RURL-dxwxeamq / ADR 0009: WHATWG host-charset shim ----------------------
# libcurl's host allowed-set is narrower than WHATWG's; it rejects 15 ASCII
# code points WHATWG keeps in the host. Under whatwg the shim accepts them (curl
# parses a filler-substituted host for structure; rurl restores the true host).
# RFC 3986 uses the same restore seam for its literal reg-name sub-delims;
# NULL keeps curl's stricter charset.

test_that("host-charset shim accepts selector-valid literal host bytes", {
  gap <- c("!", "\"", "$", "&", "'", "(", ")", "*",
           "+", ",", ";", "=", "`", "{", "}")
  rfc_subdelims <- c("!", "$", "&", "'", "(", ")", "*", "+", ",", ";", "=")
  for (ch in gap) {
    u <- paste0("http://a", ch, "b.example.com/")
    # whatwg: parses, host preserved byte-for-byte, shim diagnostic fires.
    expect_identical(
      get_host(u, url_standard = "whatwg"),
      paste0("a", ch, "b.example.com"),
      label = paste("whatwg host preserved for", ch)
    )
    expect_identical(
      get_clean_url(u, url_standard = "whatwg"), u,
      label = paste("whatwg clean_url preserved for", ch)
    )
    expect_true(
      "host-charset-shimmed" %in%
        get_url_diagnostics(u, url_standard = "whatwg"),
      label = paste("host-charset-shimmed fires for", ch)
    )
    expect_identical(
      get_host_type(u, url_standard = "whatwg"), "reg-name",
      label = paste("whatwg host_type reg-name for", ch)
    )
    # rfc3986: only RFC 3986 reg-name sub-delims parse; the broader WHATWG-only
    # set still rejects.
    if (ch %in% rfc_subdelims) {
      expect_identical(
        get_host(u, url_standard = "rfc3986"),
        paste0("a", ch, "b.example.com"),
        label = paste("rfc3986 host preserved for", ch)
      )
      expect_identical(
        get_clean_url(u, url_standard = "rfc3986"), u,
        label = paste("rfc3986 clean_url preserved for", ch)
      )
      expect_identical(
        get_host_type(u, url_standard = "rfc3986"), "reg-name",
        label = paste("rfc3986 host_type reg-name for", ch)
      )
    } else {
      expect_true(
        is.na(get_clean_url(u, url_standard = "rfc3986")),
        label = paste("rfc3986 drops", ch)
      )
    }

    # NULL: curl's stricter charset is inherited unchanged.
    expect_true(
      is.na(get_clean_url(u, url_standard = NULL)),
      label = paste("NULL selector drops", ch)
    )
    expect_false(
      "host-charset-shimmed" %in%
        get_url_diagnostics(u, url_standard = "rfc3986"),
      label = paste("no shim diagnostic under rfc3986 for", ch)
    )
  }
})

test_that("host-charset shim excludes % and does not widen the forbidden set", {
  # % (U+0025) is a forbidden domain code point -- WHATWG drops it too, so it is
  # deliberately NOT in the shim set and stays rejected under whatwg.
  expect_true(is.na(get_clean_url("http://a%b.example.com/",
    url_standard = "whatwg")))
  # | and ^ are forbidden host code points -> still fatal under whatwg.
  expect_identical(get_parse_status("http://a|b.example.com/",
    url_standard = "whatwg"), "error")
  expect_identical(get_parse_status("http://a^b.example.com/",
    url_standard = "whatwg"), "error")
})

test_that("host-charset shim is scoped to the host, not path/query/fragment", {
  # A gap byte outside the authority is ordinary content: not shimmed, and the
  # host (clean, curl-accepted) parses normally with no shim diagnostic.
  for (u in c("http://example.com/a'b", "http://example.com/p?q=a'b",
              "http://example.com/p#a'b")) {
    expect_identical(get_host(u, url_standard = "whatwg"), "example.com",
      label = paste("host untouched for", u))
    expect_false(
      "host-charset-shimmed" %in%
        get_url_diagnostics(u, url_standard = "whatwg"),
      label = paste("no shim diagnostic for", u)
    )
  }
})

test_that("host-charset shim preserves userinfo and port structure", {
  # The shim substitutes only within the host span; userinfo and port survive.
  u <- "http://user:pw@a'b.example.com:8443/p"
  expect_identical(get_host(u, url_standard = "whatwg"), "a'b.example.com")
  p <- safe_parse_url(u, url_standard = "whatwg")
  expect_identical(p$user, "user")
  expect_identical(p$password, "pw")
  expect_identical(p$port, 8443L)
  expect_identical(p$path, "/p")
})

test_that("percent-encoded host gap bytes follow each standard", {
  u <- "http://ex.com%60x.example.com/"

  expect_identical(
    get_host(u, url_standard = "whatwg"), "ex.com`x.example.com"
  )
  expect_identical(
    get_clean_url(u, url_standard = "whatwg"),
    "http://ex.com`x.example.com/"
  )
  expect_true(
    "host-charset-shimmed" %in%
      get_url_diagnostics(u, url_standard = "whatwg")
  )

  expect_identical(
    get_host(u, url_standard = "rfc3986"), "ex.com%60x.example.com"
  )
  expect_identical(
    get_clean_url(u, url_standard = "rfc3986"),
    "http://ex.com%60x.example.com/"
  )
  expect_false(
    "host-charset-shimmed" %in%
      get_url_diagnostics(u, url_standard = "rfc3986")
  )
})

test_that("RFC host shim composes literal sub-delims with percent triplets", {
  ok <- "http://a'b%60c.example.com/"
  expect_identical(
    get_host(ok, url_standard = "rfc3986"), "a'b%60c.example.com"
  )
  expect_identical(get_clean_url(ok, url_standard = "rfc3986"), ok)
  expect_false(
    "host-charset-shimmed" %in%
      get_url_diagnostics(ok, url_standard = "rfc3986")
  )

  bad <- "http://a'b`c.example.com/"
  expect_true(is.na(get_clean_url(bad, url_standard = "rfc3986")))
  expect_identical(
    get_host(bad, url_standard = "whatwg"), "a'b`c.example.com"
  )
})

test_that("RFC host percent normalization decodes only unreserved octets", {
  expect_identical(
    get_host("http://ex.com%7Ex.example.com/", url_standard = "rfc3986"),
    "ex.com~x.example.com"
  )
  expect_identical(
    get_host("http://ex.com%2Dx.example.com/", url_standard = "rfc3986"),
    "ex.com-x.example.com"
  )
  expect_identical(
    get_host("http://ex%2Ecom.example.com/", url_standard = "rfc3986"),
    "ex.com.example.com"
  )
  expect_identical(
    get_host("http://ex.com%21x.example.com/", url_standard = "rfc3986"),
    "ex.com%21x.example.com"
  )
})

# --- Dual-standard divergence_class label (Part 3a, RURL-moselrwp) -----------
# The golden table already carries a per-standard oracle (one row per
# input x url_standard). `divergence_class` labels each row so both oracle
# fixtures (this one and external-url-vectors.csv) are queryable the same way.

test_that("conformance divergence_class is consistent with the paired oracle", {
  path <- testthat::test_path("fixtures", "url-standard-conformance.csv")
  fx <- utils::read.csv(
    path, stringsAsFactors = FALSE, colClasses = "character", na.strings = "NA"
  )
  expect_true("divergence_class" %in% names(fx))
  expect_false(anyNA(fx$divergence_class))
  expect_true(all(fx$divergence_class %in% c(
    "aligned", "spec-divergent", "whatwg-only", "rfc3986-only"
  )))

  # For every input, the class must agree with the actual paired expectations:
  # spec-divergent iff the rfc3986 and whatwg rows disagree; aligned iff they
  # agree; *-only iff the counterpart standard is absent.
  for (inp in unique(fx$input)) {
    rr <- fx$expected_clean_url[fx$input == inp & fx$url_standard == "rfc3986"]
    wr <- fx$expected_clean_url[fx$input == inp & fx$url_standard == "whatwg"]
    cls <- unique(fx$divergence_class[fx$input == inp])
    expect_length(cls, 1L)
    if (length(rr) == 0L) {
      expect_identical(cls, "whatwg-only")
    } else if (length(wr) == 0L) {
      expect_identical(cls, "rfc3986-only")
    } else if (identical(rr[1], wr[1])) {
      expect_identical(cls, "aligned")
    } else {
      expect_identical(cls, "spec-divergent")
    }
  }
})
