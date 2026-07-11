# End-to-end tests for the public `scheme_acceptance = "general"` activation
# (ADR 0012 Layer 4b-2, RURL-qbnelzku). Layer 4b-1 (RURL-yutinyhb) unit-tested
# the pure building blocks; here `general` is publicly reachable, so these drive
# the PUBLIC API (safe_parse_urls + accessors) and assert the observable output.
#
# Byte-identity for the default `scheme_acceptance = "web"` posture is covered
# by test-characterization-snapshot.R + the parity harness; this file adds the
# general-posture behavior.

# --- composition rule (ADR 0012 D3) -----------------------------------------

test_that("general with url_standard = NULL is a validation error (D3)", {
  expect_error(
    safe_parse_urls("mailto:a@b.com", scheme_acceptance = "general"),
    "url_standard"
  )
  expect_error(
    safe_parse_url("mailto:a@b.com", scheme_acceptance = "general"),
    "url_standard"
  )
  # web needs no url_standard and never errors on composition.
  expect_silent(
    safe_parse_urls("http://example.com", scheme_acceptance = "web")
  )
})

# --- the 8 RURL-wncwfasl corpus false-rejects ------------------------------

test_that("corpus opaque/non-special inputs parse ok under whatwg general", {
  ok_inputs <- c(
    "mailto:a@b.com", "data:space?test#test", "fs:/hello.eth", "a:b#",
    "scheme:example.com", "scheme:example.com/path", "foo://///////bar.com/"
  )
  d <- safe_parse_urls(
    ok_inputs, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_false(any(d$parse_status == "error"))
  expect_true(all(d$parse_status %in% c("ok", "ok-ftp", "ok-scheme-relative")))
  # scheme is returned (today the whole row would error and scheme would be NA).
  expect_identical(
    d$scheme,
    c("mailto", "data", "fs", "a", "scheme", "scheme", "foo")
  )
  # parse_status is reachable through the accessor with scheme_acceptance too.
  st <- get_parse_status(
    ok_inputs, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_false(any(st == "error"))
})

test_that("repeated raw @ errors under rfc gate, escaped under whatwg", {
  bad <- "scheme://username@@@@example.com"
  # RFC 3986 generic-grammar gate (D1): a repeated raw @ in the authority fails.
  r <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(r$parse_status, "error")
  expect_true(is.na(r$clean_url))
  # WHATWG accepts and percent-escapes the excess credentials, recovering host.
  w <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_false(w$parse_status == "error")
  expect_identical(w$scheme, "scheme")
  expect_identical(w$host, "example.com")
})

# --- the four WHATWG non-special shapes (D2) --------------------------------

test_that("the four WHATWG non-special shapes parse and round-trip", {
  d <- safe_parse_urls(
    c("foo:bar", "foo:/bar", "foo:///bar", "foo://[::1]/bar"),
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_true(all(d$parse_status == "ok"))
  expect_identical(d$scheme, rep("foo", 4L))
  # opaque path (host absent), null-host list path, empty-host list path, IPv6.
  expect_identical(d$path, c("bar", "/bar", "/bar", "/bar"))
  expect_identical(d$host, c(NA, NA, NA, "[::1]"))
  # clean_url round-trips through the WHATWG serializer (null-vs-empty host).
  expect_identical(
    d$clean_url,
    c("foo:bar", "foo:/bar", "foo:///bar", "foo://[::1]/bar")
  )
})

test_that("opaque scheme payload is carried verbatim under whatwg general", {
  d <- safe_parse_urls(
    "mailto:a@b.com", scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(d$scheme, "mailto")
  # ADR 0012 D7: the recipient domain is extracted as host (see the D7 tests in
  # test-email-diagnostics.R), while the opaque payload is still carried
  # verbatim in path and clean_url.
  expect_identical(d$host, "b.com")
  expect_identical(d$path, "a@b.com")
  expect_identical(d$clean_url, "mailto:a@b.com")
})

# --- no DNS/PSL derivation and no punycode for opaque/non-special hosts ------

test_that("opaque and non-special hosts get no domain/tld and no punycode", {
  # A genuinely opaque / non-special reg-name host is NOT asserted to be a DNS
  # name (ADR 0012 D2): no PSL derivation, no punycode. (mailto is the explicit
  # D7 carve-out and is covered in test-email-diagnostics.R.)
  d <- safe_parse_urls(
    "foo://host.example/x",
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_true(is.na(d$domain))
  expect_true(is.na(d$tld))
  expect_true(is.na(d$domain_ascii))
  expect_true(is.na(d$tld_ascii))
  # A non-special reg-name host is preserved verbatim (never IDNA/punycode).
  expect_identical(d$host, "host.example")
  # via accessors as well
  expect_true(is.na(get_domain(
    "foo://host.example/x",
    scheme_acceptance = "general", url_standard = "whatwg"
  )))
})

# --- get_scheme_class / get_host_type companion helpers ---------------------

test_that("get_host_type is reachable with scheme_acceptance = general", {
  # A non-special reg-name host is not asserted to be a DNS name; the helper
  # returns a token without erroring under general acceptance.
  ht <- get_host_type(
    "foo://host.example/x",
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_length(ht, 1L)
  expect_false(is.na(ht))
})

test_that("get_scheme carries scheme_acceptance (opaque schemes resolve)", {
  # Default web acceptance: an opaque scheme is outside the allowlist => NA.
  expect_identical(get_scheme("mailto:jane@example.com"), NA_character_)
  expect_identical(get_scheme("tel:+15551234567"), NA_character_)
  # general acceptance admits the literal scheme (both standards).
  expect_identical(
    get_scheme("mailto:jane@example.com",
      url_standard = "rfc3986", scheme_acceptance = "general"),
    "mailto"
  )
  expect_identical(
    get_scheme("ws://example.com/s",
      url_standard = "whatwg", scheme_acceptance = "general"),
    "ws"
  )
  # general still requires an explicit url_standard.
  expect_error(
    get_scheme("mailto:x", scheme_acceptance = "general"),
    "requires an explicit url_standard"
  )
})

test_that("get_scheme_class cascade returns non-special under general", {
  # D2 cascade completed: with general acceptance the opaque scheme resolves,
  # so get_scheme_class classifies it as non-special rather than
  # missing-or-error (the reachable-today web default).
  expect_identical(
    get_scheme_class("mailto:jane@example.com", url_standard = "rfc3986"),
    "missing-or-error"
  )
  expect_identical(
    get_scheme_class("mailto:jane@example.com",
      url_standard = "rfc3986", scheme_acceptance = "general"),
    "non-special"
  )
  # Special schemes stay special under general acceptance.
  expect_identical(
    get_scheme_class("http://example.com/",
      url_standard = "whatwg", scheme_acceptance = "general"),
    "special"
  )
  expect_identical(
    get_scheme_class("ws://example.com/s",
      url_standard = "whatwg", scheme_acceptance = "general"),
    "special"
  )
})

# --- ws/wss special-scheme metadata activates under general (D4 / L1) --------

test_that("ws/wss parse as special (ports 80/443) under whatwg general", {
  d <- safe_parse_urls(
    c("ws://example.com/s", "wss://example.com/s"),
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_true(all(d$parse_status == "ok"))
  expect_identical(d$scheme, c("ws", "wss"))
  expect_identical(d$host, c("example.com", "example.com"))
  # The L1 default-port metadata (ws->80, wss->443) is live: an explicit default
  # port is elided under strip_default, proving special-ness.
  dd <- safe_parse_urls(
    c("ws://example.com:80/s", "wss://example.com:443/s"),
    scheme_acceptance = "general", url_standard = "whatwg",
    port_handling = "strip_default"
  )
  expect_false(any(grepl(":80|:443", dd$clean_url)))
})

test_that("ws is still rejected under the default web acceptance", {
  d <- safe_parse_urls("ws://example.com/s")
  expect_identical(d$parse_status, "error")
})

# --- RFC-general branch: the gate is the acceptance contract ----------------

test_that("rfc3986 general parses generic URIs and gates bad authorities", {
  ok <- safe_parse_urls(
    c("mailto:a@b.com", "scheme:example.com", "fs:/hello.eth"),
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_true(all(ok$parse_status != "error"))
  expect_identical(ok$scheme, c("mailto", "scheme", "fs"))
  # file under rfc3986 uses the RFC 8089 overlay.
  f <- safe_parse_urls(
    "file:///etc/hosts", scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(f$parse_status, "ok")
  expect_identical(f$path, "/etc/hosts")
})
