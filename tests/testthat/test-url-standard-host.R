# Host IPv4/reg-name model per selector (RURL-luwvkwhd, epic RURL-eqzkkohm;
# PRD §6.2, §5.1). Under a url_standard selector rurl parses numeric hosts
# faithfully instead of hard-rejecting them: RFC 3986 keeps them as reg-names,
# WHATWG coerces valid numeric IPv4 forms (and rejects out-of-range / >4-part
# forms as fatal). Diagnostics fire in BOTH modes keyed to host shape ("facts
# not policy"). Diagnostic-token subsets follow the "emit every applicable fact"
# rule adopted for the two octal rows of §6.2 (a leading-zero octal part is at
# once octal, leading-zero, and non-decimal).

url_of <- function(host) paste0("http://", host, "/")

# --- get_host: coercion vs reg-name preservation ----------------------------

test_that("get_host keeps RFC reg-names and coerces WHATWG IPv4", {
  # DoD anchor.
  expect_identical(get_host(url_of("2130706433"), url_standard = "rfc3986"),
    "2130706433")
  expect_identical(get_host(url_of("2130706433"), url_standard = "whatwg"),
    "127.0.0.1")

  # Whole-host numeric forms coerce under WHATWG, stay literal under RFC.
  rfc_literal <- c("0x7f000001", "017700000001", "0177.0.0.1",
    "192.168.010.1", "192.168", "0")
  for (h in rfc_literal) {
    expect_identical(get_host(url_of(h), url_standard = "rfc3986"), h,
      info = h)
  }
  expect_identical(get_host(url_of("0x7f000001"), url_standard = "whatwg"),
    "127.0.0.1")
  expect_identical(get_host(url_of("017700000001"), url_standard = "whatwg"),
    "127.0.0.1")
  expect_identical(get_host(url_of("0177.0.0.1"), url_standard = "whatwg"),
    "127.0.0.1")
  expect_identical(get_host(url_of("192.168.010.1"), url_standard = "whatwg"),
    "192.168.8.1")
  expect_identical(get_host(url_of("0"), url_standard = "whatwg"), "0.0.0.0")
})

test_that("WHATWG accepts empty-hex zero IPv4 parts", {
  cases <- c(
    "https://0x.0x.0" = "0.0.0.0",
    "https://0x.0x.0x.0x" = "0.0.0.0"
  )

  for (u in names(cases)) {
    expected <- unname(cases[[u]])
    expect_identical(get_parse_status(u, url_standard = "whatwg"), "ok",
      info = u)
    expect_identical(get_host(u, url_standard = "whatwg"), expected,
      info = u)
    expect_identical(get_host_type(u, url_standard = "whatwg"), "ipv4",
      info = u)
    expect_identical(get_clean_url(u, url_standard = "whatwg"),
      paste0("https://", expected, "/"), info = u)
  }

  expect_identical(get_host("https://0x.0x.0", url_standard = "rfc3986"),
    "0x.0x.0")
  expect_identical(get_host_type("https://0x.0x.0",
    url_standard = "rfc3986"), "reg-name")
})

test_that("WHATWG IDNA presentation applies UTS-46 ignored mappings", {
  u <- "https://a%C2%ADb/"

  expect_identical(get_host(u, url_standard = "whatwg"), "a\u00adb")
  expect_identical(
    get_host(u, url_standard = "whatwg", host_encoding = "idna"), "ab"
  )
  expect_identical(
    get_clean_url(u, url_standard = "whatwg", host_encoding = "idna"),
    "https://ab/"
  )

  expect_identical(
    get_host(u, url_standard = "rfc3986", host_encoding = "idna"),
    "xn--ab-5da"
  )
})

test_that("WHATWG IDNA presentation applies UTS-46 compatibility mappings", {
  urls <- c(
    "https://①.com/",
    "https://ﬃ.com/",
    "https://Ａ.com/",
    paste0(
      "https://loC",
      "\U0001D400\U0001D40B\U0001D407\U0001D428\U0001D42C\U0001D42D",
      "/"
    )
  )
  expected <- c(
    "1.com",
    "ffi.com",
    "a.com",
    "localhost"
  )

  for (i in seq_along(urls)) {
    expect_identical(
      get_host(urls[i], url_standard = "whatwg", host_encoding = "idna"),
      expected[i],
      info = urls[i]
    )
  }

  expect_identical(
    get_clean_url(
      paste0(
        "https://loC",
        "\U0001D400\U0001D40B\U0001D407\U0001D428\U0001D42C\U0001D42D",
        "/"
      ),
      url_standard = "whatwg",
      host_encoding = "idna"
    ),
    "https://localhost/"
  )
})

test_that("WHATWG rejects UTS-46 join controls that cannot normalize", {
  urls <- c("https://\u200D.com/", "https://\u200C.com/")

  expect_identical(
    get_parse_status(urls, url_standard = "whatwg"),
    c("error", "error")
  )
  expect_true(all(is.na(get_host(urls, url_standard = "whatwg"))))
})

test_that("RFC IDNA presentation remains the reversible punycode path", {
  expect_identical(
    get_host("https://①.com/", url_standard = "rfc3986",
             host_encoding = "idna"),
    "xn--orh.com"
  )
  expect_identical(
    get_host("https://ﬃ.com/", url_standard = "rfc3986",
             host_encoding = "idna"),
    "xn--lm6c.com"
  )
})

test_that("a canonical dotted-quad is IPv4 in both modes", {
  expect_identical(get_host(url_of("127.0.0.1"), url_standard = "rfc3986"),
    "127.0.0.1")
  expect_identical(get_host(url_of("127.0.0.1"), url_standard = "whatwg"),
    "127.0.0.1")
  expect_identical(get_host_type(url_of("127.0.0.1"), url_standard = "rfc3986"),
    "ipv4")
  expect_identical(get_host_type(url_of("127.0.0.1"), url_standard = "whatwg"),
    "ipv4")
})

test_that("out-of-range and over-arity are RFC reg-names but WHATWG-fatal", {
  # 256.1.1.1: RFC reg-name + ipv4-out-of-range; WHATWG fatal.
  expect_identical(get_host(url_of("256.1.1.1"), url_standard = "rfc3986"),
    "256.1.1.1")
  expect_true(is.na(get_host(url_of("256.1.1.1"), url_standard = "whatwg")))
  expect_identical(
    get_parse_status(url_of("256.1.1.1"), url_standard = "whatwg"), "error"
  )

  # >4 numeric parts: RFC reg-name, WHATWG fatal.
  expect_identical(get_host(url_of("1.2.3.4.5"), url_standard = "rfc3986"),
    "1.2.3.4.5")
  expect_true(is.na(get_host(url_of("1.2.3.4.5"), url_standard = "whatwg")))
})

# --- host_type ---------------------------------------------------------------

test_that("host_type is an (host, url_standard) function", {
  numeric_hosts <- c("2130706433", "0x7f000001", "017700000001", "0177.0.0.1",
    "192.168.010.1", "192.168", "0")
  for (h in numeric_hosts) {
    expect_identical(get_host_type(url_of(h), url_standard = "rfc3986"),
      "reg-name", info = h)
    expect_identical(get_host_type(url_of(h), url_standard = "whatwg"),
      "ipv4", info = h)
  }

  # Ordinary registrable domain, IPv6 literal, and no-host cases.
  expect_identical(get_host_type(url_of("example.com"),
    url_standard = "rfc3986"), "domain")
  expect_identical(get_host_type("http://[::1]/", url_standard = "whatwg"),
    "ipv6")

  # WHATWG-fatal numeric host has no host_type (it did not parse).
  expect_true(
    is.na(get_host_type(url_of("256.1.1.1"), url_standard = "whatwg"))
  )
  # RFC keeps it as a reg-name.
  expect_identical(get_host_type(url_of("256.1.1.1"),
    url_standard = "rfc3986"), "reg-name")
})

test_that("WHATWG serializes embedded-IPv4 IPv6 literals", {
  cases <- c(
    "http://[::127.0.0.1]/" = "[::7f00:1]",
    "http://[::ffff:127.0.0.1]/" = "[::ffff:7f00:1]",
    "http://[0:0:0:0:0:0:0:1]/" = "[::1]"
  )

  for (u in names(cases)) {
    expected_host <- unname(cases[[u]])
    expect_identical(get_host(u, url_standard = "whatwg"), expected_host,
      info = u)
    expect_identical(get_clean_url(u, url_standard = "whatwg"),
      paste0("http://", expected_host, "/"), info = u)
    expect_identical(get_host_type(u, url_standard = "whatwg"), "ipv6",
      info = u)
  }
})

test_that("RFC 3986 keeps embedded-IPv4 IPv6 literal spelling", {
  for (u in c("http://[::127.0.0.1]/", "http://[::ffff:127.0.0.1]/")) {
    original_host <- sub("^http://(\\[[^]]+\\])/$", "\\1", u)
    expect_identical(get_host(u, url_standard = "rfc3986"), original_host,
      info = u)
    expect_identical(get_clean_url(u, url_standard = "rfc3986"), u,
      info = u)
    expect_identical(get_host(u), original_host, info = u)
  }
})

# --- Diagnostics: fire in BOTH modes keyed to shape -------------------------

test_that("get_url_diagnostics matches the shape table in both modes", {
  expected <- list(
    "2130706433"    = c("ipv4-number-form", "ipv4-non-dotted"),
    "0x7f000001"    = c("ipv4-number-form", "ipv4-non-decimal"),
    "017700000001"  = c("ipv4-number-form", "ipv4-non-decimal", "ipv4-octal",
      "ipv4-leading-zero"),
    "0177.0.0.1"    = c("ipv4-non-decimal", "ipv4-octal", "ipv4-leading-zero"),
    "192.168.010.1" = c("ipv4-non-decimal", "ipv4-octal", "ipv4-leading-zero"),
    "192.168"       = "ipv4-short-form",
    "0"             = c("ipv4-number-form", "ipv4-non-dotted")
  )
  # These hosts parse under both standards, so their diagnostics are identical
  # in RFC and WHATWG mode (facts keyed to shape, not coercion outcome).
  for (h in names(expected)) {
    for (std in c("rfc3986", "whatwg")) {
      expect_setequal(
        get_url_diagnostics(url_of(h), url_standard = std), expected[[h]]
      )
    }
  }
})

test_that("out-of-range fires under RFC, suppressed by WHATWG-fatal", {
  expect_setequal(
    get_url_diagnostics(url_of("256.1.1.1"), url_standard = "rfc3986"),
    "ipv4-out-of-range"
  )
  # WHATWG: fatal parse => error row => no diagnostics.
  expect_identical(
    get_url_diagnostics(url_of("256.1.1.1"), url_standard = "whatwg"),
    character(0)
  )
})

test_that("clean hosts and unparseable input carry no host diagnostics", {
  expect_identical(
    get_url_diagnostics(url_of("example.com"), url_standard = "whatwg"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics(url_of("127.0.0.1"), url_standard = "rfc3986"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics("not-a-url", url_standard = "rfc3986"), character(0)
  )
})

# --- AC #9: Stage-A cache is keyed on url_standard --------------------------

test_that("switching url_standard does not return a stale cached host", {
  rurl_clear_caches()
  u <- url_of("2130706433")
  # Parse under each standard, interleaved, to force cache reuse attempts.
  expect_identical(get_host(u, url_standard = "rfc3986"), "2130706433")
  expect_identical(get_host(u, url_standard = "whatwg"), "127.0.0.1")
  expect_identical(get_host(u, url_standard = "rfc3986"), "2130706433")
  expect_true(is.na(get_host(u)))
  expect_identical(get_host(u, url_standard = "whatwg"), "127.0.0.1")
})

# --- AC #1: NULL selector preserves the historical hard reject --------------

test_that("without a selector numeric hosts stay rejected (unchanged)", {
  for (h in c("2130706433", "0x7f000001", "192.168.010.1", "256.1.1.1",
    "1.2.3.4.5")) {
    expect_true(is.na(get_host(url_of(h))), info = h)
    expect_identical(get_parse_status(url_of(h)), "error", info = h)
  }
  # Canonical IPv4 is accepted with no selector, as always.
  expect_identical(get_host(url_of("127.0.0.1")), "127.0.0.1")
})

# --- RURL-crrgaiel: `pct-encoded` in a reg-name is unrestricted -------------

# RFC 3986 S3.2.2: `reg-name = *( unreserved / pct-encoded / sub-delims )` with
# `pct-encoded = "%" HEXDIG HEXDIG`. The production restricts WHICH LITERAL
# bytes a reg-name may hold; it places no restriction whatever on what a
# well-formed triplet DECODES to. S2.2 is the point of the escape: a reserved
# octet is percent-encoded precisely so it can be carried as DATA rather than as
# a delimiter. So `ho%2Fst` is a valid reg-name whose second-to-last character
# is a slash-as-data, and S6.2.2.2 forbids decoding it (only `unreserved` may be
# decoded), which `host_pct = "keep"` already renders correctly.
#
# rurl used to judge the DECODED octet against the literal set, so `%2F`, `%25`,
# `%40` and the C0 range rejected "whichever way they were written". That is the
# WHATWG host parser's rule -- decode first, then check forbidden host code
# points -- not RFC 3986's, and it scored 48 of the sweep's grammar-valid
# rejections.
test_that("rfc3986 admits any well-formed host triplet, kept encoded", {
  hosts <- c(
    sprintf("%%%02X", 0:31),                      # the C0 controls
    "%20", "%23", "%25", "%2F", "%3A", "%3C", "%3E", "%3F", "%40",
    "%5B", "%5C", "%5D"
  )
  urls <- paste0("http://ho", hosts, "st/")

  # The PARSE succeeds -- that is the conformance fact, read off the identity
  # record rather than off `parse_status`, which is a projection that also folds
  # in the L3 PSL annotation (R/verdicts.R).
  r <- rurl:::.fsss_record_vec(urls, "rfc3986", NULL)
  expect_identical(r$ok, rep(TRUE, length(urls)))
  # Kept ENCODED -- never decoded, since S6.2.2.2 permits decoding `unreserved`
  # only.
  expect_identical(r$host, paste0("ho", hosts, "st"))
  # Surface (b) round-trips the source spelling.
  expect_identical(
    serialize_url(urls, standard = "rfc3986", form = "source"),
    paste0("http://ho", hosts, "st/")
  )

  # `parse_status` is whatever a structurally IDENTICAL host with a literal
  # character in place of the triplet reports -- i.e. the percent-encoding is
  # not itself the reason for the verdict. `ho...st` is a single label, so every
  # one of these is the ordinary no-TLD annotation.
  expect_identical(
    get_parse_status(urls, url_standard = "rfc3986"),
    rep(get_parse_status("http://hoXst/", url_standard = "rfc3986"),
      length(urls))
  )

  # The two S6.2.2 normalizations are applied by the SERIALIZER under
  # `form = "normalized"`, never during the parse -- the same parse/normalize
  # line RURL-epoinamh drew for the empty path. So the identity record keeps the
  # source spelling and only `normalized` uppercases the hex (S6.2.2.1) and
  # decodes an `unreserved` triplet (S6.2.2.2), while a non-unreserved octet
  # stays encoded in BOTH forms.
  mixed <- c("http://ho%2fst/", "http://ho%6fst/", "http://ho%00st/")
  expect_identical(
    rurl:::.fsss_record_vec(mixed, "rfc3986", NULL)$host,
    c("ho%2fst", "ho%6fst", "ho%00st")
  )
  expect_identical(
    serialize_url(mixed, standard = "rfc3986", form = "source"), mixed
  )
  expect_identical(
    serialize_url(mixed, standard = "rfc3986", form = "normalized"),
    c("http://ho%2Fst/", "http://hoost/", "http://ho%00st/")
  )
  # S6.2.2.1 has a SECOND sentence -- the host is case-insensitive and
  # normalizes to lowercase -- so an `unreserved` triplet decoding to uppercase
  # normalizes twice: "%41" -> "A" -> "a". Both still leave `source` untouched.
  expect_identical(
    serialize_url("http://ho%41st/", standard = "rfc3986", form = "source"),
    "http://ho%41st/"
  )
  expect_identical(
    serialize_url("http://ho%41st/", standard = "rfc3986",
      form = "normalized"),
    "http://hoast/"
  )

  # High bytes are `pct-encoded` too, on the schemes the sweep exercised.
  hi <- c("ftp://example.com%80/", "ftp://example.com%A0/",
          "https://example.com%80/", "https://example.com%A0/")
  expect_identical(rurl:::.fsss_record_vec(hi, "rfc3986", NULL)$ok,
    rep(TRUE, 4L))
  expect_identical(
    rurl:::.fsss_record_vec(hi, "rfc3986", NULL)$host,
    c("example.com%80", "example.com%A0", "example.com%80", "example.com%A0")
  )
  expect_identical(
    get_parse_status(hi, url_standard = "rfc3986"),
    rep(get_parse_status("https://example.comX/", url_standard = "rfc3986"), 4L)
  )

  # A malformed "%" is still a PARSE ERROR -- widening which octets a triplet
  # may denote did not stop requiring `"%" HEXDIG HEXDIG`.
  bad <- c("http://ho%st/", "http://ho%0st/", "http://ho%zzst/", "http://ho%/")
  expect_identical(get_parse_status(bad, url_standard = "rfc3986"),
    rep("error", length(bad)))
})

test_that("whatwg and NULL keep rejecting host triplets they always did", {
  # WHATWG's forbidden host code points are judged on the DECODED host, so these
  # must keep failing there; the no-selector baseline is byte-frozen.
  urls <- paste0("http://ho", c("%00", "%1F", "%2F", "%25", "%40"), "st/")
  expect_identical(get_parse_status(urls, url_standard = "whatwg"),
    rep("error", length(urls)))
  expect_identical(get_parse_status(urls), rep("error", length(urls)))

  # A percent-encoded reg-name is not DNS-eligible, so the PSL annotation
  # declines it rather than guessing (`.psl_annotation_host_vec()`).
  expect_true(all(is.na(
    get_domain(paste0("http://ho", c("%00", "%2F"), "st/"),
      url_standard = "rfc3986")
  )))
})

# --- host_encoding = "unicode" preserves the label structure ----------------
# RURL-eikgtrqf. `unicode` is a RENDERING knob over a host identity, so it may
# change a label's SPELLING (A-label -> U-label) but never how many labels the
# host has. The decode seam used to rejoin `strsplit(host, ".", fixed = TRUE)`,
# which drops trailing empty labels (ADR 0005 documents that base-R gap), so a
# root-dot FQDN silently lost its root label and converged on the non-FQDN host
# that P3.2 (KJ-O1..O8) holds DISTINCT. The WHATWG host parser does not strip a
# root dot either, so `unicode` was also the only rendering diverging from the
# standard it sits under.

test_that("host_encoding = 'unicode' keeps a trailing root-dot label", {
  for (std in list(NULL, "whatwg", "rfc3986")) {
    expect_identical(
      unname(get_host("https://example.com./",
        url_standard = std, host_encoding = "unicode")),
      "example.com.",
      info = paste("url_standard =", if (is.null(std)) "NULL" else std)
    )
  }

  # The A-label still decodes; only the label COUNT is preserved.
  expect_identical(
    unname(get_host("https://xn--mnchen-3ya.de./", host_encoding = "unicode")),
    "münchen.de."
  )

  # And the distinction survives to the presentation surface.
  expect_false(identical(
    unname(get_host("https://example.com./", host_encoding = "unicode")),
    unname(get_host("https://example.com/", host_encoding = "unicode"))
  ))
})

test_that("the unicode decode seam is label-count preserving", {
  # Both implementations, since the scalar seam is a separate code path
  # (test doubles) that the parity oracle compares against the vector one.
  hosts <- c("example.com.", "a..b", "a..", ".", "..", "xn--p1ai.")
  expected <- c("example.com.", "a..b", "a..", ".", "..", "рф.")
  expect_identical(rurl:::.punycode_to_unicode_vec(hosts), expected)
  expect_identical(
    vapply(hosts, rurl:::.punycode_to_unicode, character(1), USE.NAMES = FALSE),
    expected
  )
})
