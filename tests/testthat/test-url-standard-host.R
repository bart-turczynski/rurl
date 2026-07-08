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
