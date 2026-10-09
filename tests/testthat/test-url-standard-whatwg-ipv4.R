# WHATWG IPv4 host reading under `url_standard = "whatwg"` (RURL-cbrfphfr).
#
# Every expected value here is read off the WHATWG URL Standard, not off an
# implementation: the IPv4 parser
# (https://url.spec.whatwg.org/#concept-ipv4-parser), the IPv4 number parser
# (#ipv4-number-parser), the ends-in-a-number checker
# (#ends-in-a-number-checker) and the basic URL parser's preprocessing. Rows
# that web-platform-tests also pins (url/resources/urltestdata.json) say so.
# No row is derived by comparing rurl with raddr, and no copy of the IPv4
# parser lives in this file: the pins are literal strings.
#
# The four pieces of the standard the rows below exercise:
#
#   number parser  "0x"/"0X" prefix -> radix 16; a leading "0" on a part of two
#                  or more code points -> radix 8; otherwise radix 10. An empty
#                  remainder after the prefix is 0. A code point outside the
#                  radix is failure.
#   IPv4 parser    one empty last part is dropped; more than four parts is
#                  failure; any part but the last > 255 is failure; the last
#                  part must be < 256^(5 - number of parts); the value is
#                  serialized as a dotted quad.
#   ends in a      the host goes to the IPv4 parser only when its last label
#   number         (after dropping one empty one) is all ASCII digits or parses
#                  as an IPv4 number. Otherwise it is a domain, never an
#                  address -- and once it does go there, IPv4 failure is host
#                  failure (host parser step 7), with no fallback to a domain.
#   preprocessing  the basic URL parser removes every ASCII tab or newline from
#                  the input before any state runs (step 3), so a tab inside
#                  the host is gone before the host parser sees it.

ipv4_url <- function(host) paste0("http://", host, "/")

whatwg_host <- function(host) {
  get_host(ipv4_url(host), url_standard = "whatwg")
}

# --- accepted IPv4 forms ------------------------------------------------------

test_that("whatwg reads every IPv4 number radix and short form as an address", {
  accepted <- c(
    # hex first part, two parts: 0x7f -> 127, last part fills three octets.
    "0x7f.1" = "127.0.0.1",
    # uppercase prefix: the standard's test is "0X" or "0x".
    "0Xff" = "0.0.0.255",
    "0xff" = "0.0.0.255",
    "0x7F000001" = "127.0.0.1",
    # one part: the whole 32-bit range. WPT: http://4294967295,
    # http://0xffffffff -> 255.255.255.255.
    "4294967295" = "255.255.255.255",
    "0xffffffff" = "255.255.255.255",
    # WPT: http://999999999 -> 59.154.201.255; http://256 -> 0.0.1.0.
    "999999999" = "59.154.201.255",
    "256" = "0.0.1.0",
    "0" = "0.0.0.0",
    # three parts: the last fills two octets. WPT: http://192.168.257 ->
    # 192.168.1.1.
    "1.2.3" = "1.2.0.3",
    "192.168.257" = "192.168.1.1",
    "1.256" = "1.0.1.0",
    # octal parts: a leading "0" selects radix 8.
    "0177.0.0.1" = "127.0.0.1",
    "192.168.010.1" = "192.168.8.1",
    "017700000001" = "127.0.0.1",
    # mixed radices in one host.
    "0xc0.0250.01" = "192.168.0.1",
    # an empty remainder after "0x" is the number 0. WPT: https://0x.0x.0 ->
    # 0.0.0.0.
    "0x" = "0.0.0.0",
    "0X" = "0.0.0.0",
    "0x.0x.0" = "0.0.0.0",
    # one empty last part is dropped. WPT: http://1.2.3.4./ -> 1.2.3.4,
    # http://192.168.257. -> 192.168.1.1.
    "1.2.3.4." = "1.2.3.4",
    "192.168.257." = "192.168.1.1",
    "1.2.3.4" = "1.2.3.4"
  )
  for (h in names(accepted)) {
    want <- unname(accepted[[h]])
    expect_identical(whatwg_host(h), want, info = h)
    expect_identical(
      get_parse_status(ipv4_url(h), url_standard = "whatwg"), "ok", info = h
    )
    expect_identical(
      get_host_type(ipv4_url(h), url_standard = "whatwg"), "ipv4", info = h
    )
    expect_identical(
      serialize_url(ipv4_url(h), standard = "whatwg"),
      paste0("http://", want, "/"), info = h
    )
  }
})

# --- rejected address candidates: WHATWG IPv4 failure is URL failure ----------

test_that("whatwg fails the URL when a number-ending host is no address", {
  rejected <- c(
    # above 2^32 - 1 in one part. WPT: https://0x100000000/test fails.
    "4294967296",
    "0x100000000",
    "0999999999999999999",
    # more than four parts. WPT: http://0x1.2.3.4.5, http://01.2.3.4.5 fail.
    "1.2.3.4.5",
    "0x1.2.3.4.5",
    "0x1.2.3.4.5.",
    # a non-last part above 255. WPT: https://256.0.0.1/test fails.
    "256.0.0.1",
    "256.1.1.1",
    # the last part too wide for the octets left: 3 parts -> < 2^16.
    "192.168.0x00A80001",
    "1.2.65536",
    # "08" / "09" / "048" are octal with a digit outside radix 8. WPT:
    # http://1.2.3.08, http://1.2.3.09, http://09.2.3.4 fail.
    "192.0.048.1",
    "1.2.3.08",
    "1.2.3.08.",
    "1.2.3.09",
    "09.2.3.4",
    "08",
    # a domain label in front of a number: the last label ends in a number,
    # so the IPv4 parser runs and fails on "foo". WPT: http://foo.09,
    # http://foo.1.2.3.4. fail.
    "foo.09",
    "foo.0x4",
    "foo.1.2.3.4."
  )
  for (h in rejected) {
    expect_identical(
      get_parse_status(ipv4_url(h), url_standard = "whatwg"), "error", info = h
    )
  }
})

test_that("a WHATWG IPv4 failure leaves no host, type, clean URL or href", {
  for (h in c("4294967296", "1.2.3.4.5", "192.0.048.1", "0x100000000")) {
    u <- ipv4_url(h)
    expect_true(is.na(get_host(u, url_standard = "whatwg")), info = h)
    expect_true(is.na(get_host_type(u, url_standard = "whatwg")), info = h)
    expect_true(is.na(get_clean_url(u, url_standard = "whatwg")), info = h)
    expect_true(is.na(serialize_url(u, standard = "whatwg")), info = h)
  }
})

test_that("a failing row does not disturb its neighbors in one call", {
  urls <- ipv4_url(c("0x7f.1", "4294967296", "1.2.3", "192.0.048.1", "0xg"))
  expect_identical(
    get_host(urls, url_standard = "whatwg"),
    c("127.0.0.1", NA, "1.2.0.3", NA, "0xg")
  )
  expect_identical(
    get_parse_status(urls, url_standard = "whatwg")[c(1L, 2L, 3L, 4L)],
    c("ok", "error", "ok", "error")
  )
})

# --- hosts that never reach the IPv4 parser stay names ------------------------

test_that("a host whose last label is not a number stays a name", {
  # "0xg" and "0x1p": "0x" then a code point outside radix 16, so the last
  # label is no IPv4 number and the ends-in-a-number checker says false.
  # "." and "..": after dropping one empty last part, the last label is empty.
  # WPT: http://0x7f.0.0.0x7g -> host 0x7f.0.0.0x7g; http://256.com,
  # http://999999999.com and http://192.168.257.com stay names.
  names_kept <- c(
    "0xg", "0x1p", ".", "..", "0x7f.0.0.0x7g", "256.com",
    "999999999.com", "192.168.257.com", "1.2.3.4.."
  )
  for (h in names_kept) {
    u <- ipv4_url(h)
    expect_identical(whatwg_host(h), h, info = h)
    expect_false(
      identical(get_parse_status(u, url_standard = "whatwg"), "error"),
      info = h
    )
    expect_false(
      identical(get_host_type(u, url_standard = "whatwg"), "ipv4"), info = h
    )
  }
})

# --- preprocessing before the IPv4 parser ------------------------------------

test_that("tab and newline are removed before the host is read as IPv4", {
  # Basic URL parser step 3 strips every ASCII tab or newline, wherever it is.
  stripped <- c(
    "1.2.3.4\t" = "1.2.3.4",
    "1.2.\t3.4" = "1.2.3.4",
    "\t0x7f.1" = "127.0.0.1",
    "1.2.3.4\n" = "1.2.3.4",
    "0x7f\r.1" = "127.0.0.1"
  )
  for (h in names(stripped)) {
    expect_identical(whatwg_host(h), unname(stripped[[h]]), info = h)
  }
})

test_that("a percent-encoded numeric host is decoded before the IPv4 parser", {
  # Host parser: percent-decode, domain to ASCII, then ends in a number. WPT:
  # http://%30%78%63%30%2e%30%32%35%30.01 and the same with a trailing %2e
  # -> 192.168.0.1.
  expect_identical(
    whatwg_host("%30%78%63%30%2e%30%32%35%30.01"), "192.168.0.1"
  )
  expect_identical(
    whatwg_host("%30%78%63%30%2e%30%32%35%30.01%2e"), "192.168.0.1"
  )
  expect_identical(whatwg_host("%31.2.3.4"), "1.2.3.4")
})

# --- the reading is whatwg-only ----------------------------------------------

test_that("rfc3986 keeps the same tokens as registered names", {
  # RFC 3986 section 3.2.2: a host that does not match IPv4address is a
  # reg-name. (Lowercase rows only: `url_standard = "rfc3986"` folds host
  # case, section 6.2.2.1, which is a separate rule.)
  for (h in c("0x7f.1", "0xff", "4294967295", "1.2.3", "4294967296",
              "192.0.048.1")) {
    expect_identical(
      get_host(ipv4_url(h), url_standard = "rfc3986"), h, info = h
    )
  }
})
