# Tests for the `domain-invalid-ace-label` diagnostic (RURL-vicyvlvh; ruling
# RUL-023). The token reports an `xn--` label that is not a genuine A-label:
# its Punycode decode fails, or the decoded label fails the UTS #46 Validity
# Criteria (section 4.1) under WHATWG's non-strict flags. The PARSE does not
# move: the WHATWG domain parser (beStrict = false) keeps such an ASCII host,
# lowercased, and the WPT corpus pins `https://xn--/` as a success.
#
# Unlike the vocabulary as a whole ("selected facts; absence never implies
# conformance", ADR 0012 D5), this token carries a COMPLETENESS guarantee that
# ssrfr relies on: it fires on every host meeting the predicate. The grid test
# below pins that against hand-classified labels, not against the probe.

# get_url_diagnostics() returns a bare vector for one URL and a list for
# several; wrap so both read the same.
diag_list <- function(url, standard) {
  d <- get_url_diagnostics(url, url_standard = standard)
  if (length(url) == 1L) list(d) else d
}

diag_has_ace <- function(url, standard) {
  vapply(
    diag_list(url, standard),
    function(d) "domain-invalid-ace-label" %in% d,
    logical(1)
  )
}

# Hand-classified labels. Every INVALID entry names the rule it breaks; every
# VALID entry is a genuine A-label or a plain LDH label.
ace_invalid <- c(
  "xn--a", #              Punycode decode fails
  "xn--", #               empty after the prefix
  "xn--ascii-", #         decodes to an all-ASCII label
  "xn--ASCII-", #         same, prefix case varies
  "XN--A", #              the prefix is ASCII case-insensitive
  "Xn--a-", #             decodes to "a"
  "xn--a-ccb", #          criterion 1: not NFC
  "xn--xn---ooa", #       criterion 4: decodes to "xn--" + U+00E4
  "xn--xn--_-kra", #      criterion 4, with a non-STD3 basic code point
  "XN--XN---OOA", #       criterion 4, case folded before decoding
  "xn--a-wbb", #          criterion 6: leading combining mark
  "xn--7ba", #            criterion 7: U+00C4 has status mapped
  "xn--a-ba", #           criterion 7: U+0080 is disallowed
  "xn--ab-m1t", #         criterion 8: ZWJ fails ContextJ
  "xn--a-zhc" #           criterion 9: Hebrew then Latin, in one label
)
ace_valid <- c(
  "xn--bcher-kva", "XN--BCHER-KVA", "xn--nxasmq6b", "xn--zca",
  "xn--4dbrk0ce", "xn--ls8h", "xn--1-0fa", "example", "xn", "axn--a",
  "x-n--a"
)

# --- the named acceptance cases ----------------------------------------------

test_that("the token fires on the three reported invalid ACE hosts", {
  urls <- c(
    "http://xn--a.example/", "http://xn--.example/",
    "http://xn--ASCII-.example/"
  )
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(
      get_url_diagnostics(urls, url_standard = s),
      rep(list("domain-invalid-ace-label"), 3L),
      info = s
    )
  }
})

test_that("the token does not fire on genuine A-labels, ASCII or IP hosts", {
  urls <- c(
    "http://xn--bcher-kva.example/", "http://xn--nxasmq6b.example/",
    "http://example.com/", "http://127.0.0.1/", "http://[::1]/",
    "http://[2001:db8::1]/"
  )
  for (s in c("whatwg", "rfc3986")) {
    expect_false(any(diag_has_ace(urls, s)), info = s)
  }
})

# --- the parse does not move -------------------------------------------------

test_that("an invalid ACE host still parses, lowercased, as WHATWG requires", {
  # WHATWG URL Standard, host parser: the domain parser runs with beStrict =
  # false and returns an ASCII domain lowercased "regardless of Unicode
  # ToASCII's outcome". The token is a fact, never a verdict.
  urls <- c(
    "http://xn--a.example/", "http://xn--.example/",
    "http://xn--ASCII-.example/"
  )
  hosts <- c("xn--a.example", "xn--.example", "xn--ascii-.example")
  for (s in c("whatwg", "rfc3986")) {
    v <- get_parse_verdicts(urls, url_standard = s)
    expect_identical(v$layer1_syntax_verdict, rep("pass", 3L), info = s)
    expect_identical(
      get_parse_status(urls, url_standard = s),
      rep("warning-invalid-tld", 3L),
      info = s
    )
    expect_identical(get_host(urls, url_standard = s), hosts, info = s)
    expect_identical(
      get_host(urls, url_standard = s, host_encoding = "idna"), hosts,
      info = s
    )
  }
})

test_that("the WPT success rows for an xn-- host stay successes", {
  urls <- c("https://xn--/", "file://xn--/p")
  v <- get_parse_verdicts(urls, url_standard = "whatwg")
  expect_identical(v$layer1_syntax_verdict, c("pass", "pass"))
  expect_identical(get_host(urls, url_standard = "whatwg"), c("xn--", "xn--"))
  expect_identical(diag_has_ace(urls, "whatwg"), c(TRUE, TRUE))
})

# --- completeness: fires on EVERY host meeting the predicate ------------------

test_that("the token fires exactly where a label is invalid, in any position", {
  # Every hand-classified label, in every position, beside every kind of
  # neighbor -- including an empty label, which must neither mask nor mimic
  # the fact, and a trailing root dot. Both directions: the token fires iff
  # the host holds an invalid label.
  shapes <- c(
    "%s.example", "www.%s.example", "a.b.%s", "%s", "%s.example.",
    "%s..example", "a..b.%s", "%s.xn--bcher-kva.example",
    "xn--a.%s.example"
  )
  for (s in c("whatwg", "rfc3986")) {
    for (shape in shapes) {
      host <- sprintf(shape, c(ace_invalid, ace_valid))
      expected <- c(
        rep(TRUE, length(ace_invalid)),
        # The one shape that carries a known-invalid neighbor fires regardless.
        rep(startsWith(shape, "xn--a."), length(ace_valid))
      )
      expect_identical(
        diag_has_ace(paste0("http://", host, "/"), s), expected,
        info = paste(s, shape)
      )
    }
  }
})

test_that("an empty label elsewhere neither masks nor mimics the token", {
  expect_setequal(
    get_url_diagnostics("http://xn--a..example/", url_standard = "whatwg"),
    c("domain-empty-label", "domain-invalid-ace-label")
  )
  expect_identical(
    get_url_diagnostics(
      "http://a..xn--bcher-kva.example/",
      url_standard = "whatwg"
    ),
    "domain-empty-label"
  )
})

test_that("a non-ASCII code point inside an xn-- label fires under rfc3986", {
  # Under whatwg such a host fails the parse (ToASCII fails on a non-ASCII
  # domain, which the non-strict carve-out does not cover), so there is no
  # host to report on.
  url <- "http://xn--bücher.example/"
  expect_true(diag_has_ace(url, "rfc3986"))
  expect_identical(get_parse_status(url, url_standard = "whatwg"), "error")
})

test_that("the probe folds ASCII case before testing criterion 4", {
  # The seam is fed the parsed host, which the parser has already lowercased;
  # the fold keeps the probe correct for any caller that has not.
  probe <- rurl:::.punycoder_host_probe(
    c("XN--XN---OOA.example", "Xn--Xn---ooa.example", "XN--BCHER-KVA.example")
  )
  expect_identical(probe$invalid_ace_label, c(TRUE, TRUE, FALSE))
})

# --- scope: what the token does not report ------------------------------------

test_that("hyphen, STD3 and length facts alone never fire the token", {
  long_ace <- paste0("xn--9ca", strrep("a", 57)) # valid, 64 octets
  urls <- c(
    "http://-a.example/", "http://a-.example/", "http://ab--c.example/",
    "http://a_b.example/", "http://xn--a_-wia.example/",
    paste0("http://", strrep("a", 64), ".example/"),
    paste0("http://", long_ace, ".example/")
  )
  for (s in c("whatwg", "rfc3986")) {
    d <- get_url_diagnostics(urls, url_standard = s)
    expect_false(any(diag_has_ace(urls, s)), info = s)
    expect_true(all(lengths(d) > 0L), info = s)
  }
})

test_that("a non-LDH basic code point in an A-label is judged by rurl's decode", {
  # "xn--a_-wia" is "a_" + U+00E4: RFC 3492 decodes it and UseSTD3ASCIIRules
  # is false under WHATWG, so it is a genuine A-label. punycoder 1.3.0's
  # decoder rejects it, which put the token on it and hid the STD3 fact until
  # rurl decoded ACE payloads itself (RURL-mfmgauos). Holds under either
  # punycoder version.
  url <- "http://xn--a_-wia.example/"
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(diag_list(url, s)[[1]], "domain-std3-violation", info = s)
  }
  probe <- rurl:::.punycoder_host_probe(
    c("xn--a_-wia.example", "xn--a_-wia.example.", "XN--A_-WIA.example")
  )
  expect_identical(probe$invalid_ace_label, c(FALSE, FALSE, FALSE))
  expect_identical(probe$std3_violation, c(TRUE, TRUE, TRUE))
})

test_that("a decoded label is validated as it stands, never after mapping", {
  # host_normalize() maps and NFC-normalizes Unicode input, so rurl requires
  # the decoded label to come back unchanged: U+00C4 (mapped), "a" + U+0308
  # (not NFC) and U+00AD (ignored) are not genuine A-labels even though their
  # mapped forms are valid.
  labels <- c("xn--7ba", "xn--a-ccb", "xn--ab-5da")
  expect_identical(
    vapply(substring(labels, 5L), rurl:::.rfc3492_decode, character(1),
           USE.NAMES = FALSE),
    c("Ä", "ä", "a­b")
  )
  expect_identical(
    rurl:::.ace_label_table(as.list(labels))$invalid, labels
  )
})

test_that("criterion 4 co-fires with the strict hyphen fact", {
  # "xn--" + U+00E4 breaks criterion 2 (hyphens in positions 3-4, a
  # CheckHyphens rule, reported as domain-hyphen-violation) and criterion 4
  # (its non-CheckHyphens counterpart, reported by this token).
  expect_setequal(
    get_url_diagnostics(
      "http://xn--xn---ooa.example/",
      url_standard = "whatwg"
    ),
    c("domain-hyphen-violation", "domain-invalid-ace-label")
  )
})

test_that("cross-label Bidi is out of the token's scope", {
  # "1" + U+00E4 is a valid label on its own and fails criterion 9 only
  # because the Hebrew label makes the name a Bidi domain name. The predicate
  # is evaluated per label, alone; ?get_url_diagnostics states the exclusion.
  expect_false(
    diag_has_ace("http://xn--1-0fa.xn--4dbrk0ce/", "whatwg")
  )
})
