# Tests for non-compliant / non-URL input handling (RURL-muwpjsmn, D1-D8).
#
# Governing principle: an explicit supported scheme is the user asserting "this
# is a URL"; a missing scheme must be EARNED by the input being host-shaped.

# D1: scheme-less host-plausibility gate -------------------------------------

test_that("D1: scheme-less non-host-like input is error, not fabricated", {
  for (u in c("asdfghjkl", "example", "hello world", "/relative/path", "255")) {
    expect_identical(unname(get_parse_status(u)), "error", info = u)
    expect_identical(unname(get_clean_url(u)), NA_character_, info = u)
    expect_identical(unname(get_host(u)), NA_character_, info = u)
  }
})

test_that("D1: nonsense is consistent regardless of a space tripping curl", {
  # The historical inconsistency: "hello world" errored (curl rejects the
  # space) but "asdfghjkl" fabricated a URL. Now both are error.
  expect_identical(
    unname(get_parse_status(c("hello world", "asdfghjkl"))),
    c("error", "error")
  )
})

test_that("D1: an explicit scheme is trusted (single-label host kept)", {
  # With a scheme the user asserted a URL, so we keep it (warning, not error).
  expect_identical(
    unname(get_parse_status("http://asdfghjkl/")), "warning-no-tld"
  )
  expect_identical(
    unname(get_clean_url("http://asdfghjkl/")), "http://asdfghjkl/"
  )
})

test_that("D1: host-shaped scheme-less input still parses", {
  expect_identical(unname(get_clean_url("example.com")), "http://example.com/")
  expect_identical(unname(get_parse_status("example.com")), "ok")
  expect_identical(
    unname(get_parse_status("a.b.c.d.e")), "warning-invalid-tld"
  )
})

# D2/D3: strict IP validation on the input token -----------------------------

test_that("D2: inet_aton coercion forms are rejected, scheme or not", {
  coerced <- c(
    "12345", "http://12345/", "2130706433", "0x7f000001",
    "017700000001", "192.168", "1.1", "256.1.1.1", "1.2.3.4.5",
    "999.999.999.999"
  )
  for (u in coerced) {
    expect_identical(unname(get_parse_status(u)), "error", info = u)
    expect_identical(unname(get_clean_url(u)), NA_character_, info = u)
  }
})

test_that("D3: leading-zero (octal) octets are rejected, not rewritten", {
  # "192.168.010.1" is 192.168.8.1 under inet_aton -- a different host.
  for (u in c("192.168.010.1", "http://192.168.010.1/", "127.0.0.01")) {
    expect_identical(unname(get_parse_status(u)), "error", info = u)
  }
})

test_that("D2: canonical IP literals are still accepted", {
  expect_identical(unname(get_clean_url("1.2.3.4")), "http://1.2.3.4/")
  expect_identical(unname(get_parse_status("1.2.3.4")), "ok")
  expect_true(safe_parse_url("1.2.3.4")$is_ip_host)
  expect_identical(unname(get_clean_url("http://[::1]/")), "http://[::1]/")
  expect_identical(unname(get_parse_status("192.168.0.1")), "ok")
})

test_that("D3: .detect_ip_host_vec rejects zero-padded/out-of-range octets", {
  expect_true(rurl:::.detect_ip_host_vec("192.168.0.1"))
  expect_true(rurl:::.detect_ip_host_vec("0.0.0.0"))
  expect_true(rurl:::.detect_ip_host_vec("10.0.0.0"))
  expect_false(rurl:::.detect_ip_host_vec("192.168.007.1"))
  expect_false(rurl:::.detect_ip_host_vec("192.168.010.1"))
  expect_false(rurl:::.detect_ip_host_vec("256.1.1.1"))
  expect_false(rurl:::.detect_ip_host_vec("1.2.3.4.5"))
  expect_true(rurl:::.detect_ip_host_vec("[::1]"))
})

# D4: localhost allowlist -----------------------------------------------------

test_that("D4: scheme-less localhost is accepted (warning-no-tld)", {
  expect_identical(unname(get_clean_url("localhost")), "http://localhost/")
  expect_identical(unname(get_parse_status("localhost")), "warning-no-tld")
  expect_identical(unname(get_host("localhost")), "localhost")
})

# D5: scheme-less userinfo ----------------------------------------------------

test_that("D5: scheme-less user@host keeps components, suppresses clean_url", {
  expect_identical(
    unname(get_parse_status("user@example.com")), "warning-userinfo"
  )
  expect_identical(unname(get_clean_url("user@example.com")), NA_character_)
  expect_identical(unname(get_host("user@example.com")), "example.com")
  expect_identical(unname(get_domain("user@example.com")), "example.com")
  expect_identical(unname(get_tld("user@example.com")), "com")
  expect_identical(unname(get_user("user@example.com")), "user")
})

test_that("D5: with an explicit scheme, userinfo is dropped into a clean_url", {
  # Documented canonical-key behavior: userinfo excluded, scheme trusted.
  expect_identical(
    unname(get_clean_url("http://user@example.com")), "http://example.com/"
  )
  expect_identical(unname(get_parse_status("http://user@example.com")), "ok")
})

test_that("D5: warning-userinfo rows are non-joinable (NA key)", {
  a <- data.frame(URL = "user@example.com", x = 1L, stringsAsFactors = FALSE)
  b <- data.frame(URL = "user@example.com", y = 2L, stringsAsFactors = FALSE)
  joined <- canonical_join(
    a, b, join = "left", join_parse_status = "ok_or_warning"
  )
  expect_true(all(is.na(joined$y)))
})

# D6: user:pass@host stays error ---------------------------------------------

test_that("D6: user:pass@host (no //) is error, like scheme:opaque", {
  # Indistinguishable from scheme:opaque (mailto:a@b.com), so both are error.
  expect_identical(unname(get_parse_status("user:pass@example.com")), "error")
  expect_identical(unname(get_parse_status("mailto:a@b.com")), "error")
  # Escape hatch: scheme-relative authority still resolves.
  expect_identical(
    unname(get_host("//user:pass@example.com")), "example.com"
  )
})

# D7/D8: supported scheme set -------------------------------------------------

test_that("D7: only http/https/ftp/ftps/file are supported schemes", {
  expect_identical(
    rurl:::.SUPPORTED_SCHEMES, c("http", "https", "ftp", "ftps", "file")
  )
  expect_identical(unname(get_parse_status("http://example.com")), "ok")
  expect_identical(unname(get_parse_status("https://example.com")), "ok")
  expect_identical(unname(get_parse_status("ftp://example.com")), "ok-ftp")
  expect_identical(unname(get_parse_status("ftps://example.com")), "ok-ftp")
  expect_identical(unname(get_parse_status("file:///tmp/a.txt")), "ok")
})

test_that("D7: unsupported hierarchical + opaque schemes are error", {
  for (u in c("ws://example.com", "wss://example.com/s", "ssh://git@h.com/r",
              "redis://localhost:6379/0", "gopher://example.com",
              "tel:+15551234", "data:text/plain,hi", "javascript:void(0)")) {
    expect_identical(unname(get_parse_status(u)), "error", info = u)
  }
})
