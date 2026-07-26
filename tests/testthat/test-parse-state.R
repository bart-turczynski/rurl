# Layer 3a state-model representability proof (ADR 0012 D2, RURL-tzkcbvvt).
#
# These tests prove the internal state model is expressive enough WITHOUT any
# public parse: the four WHATWG non-special shapes all error publicly today and
# the `general` acceptance axis is unexposed (Layer 2 gate). So the proof works
# at the STATE level -- feeding hand-decomposed pieces to the pure classifiers
# and asserting the four shapes map to four DISTINCT state tuples. Per ADR 0012
# D2 a true state->string->state round-trip is not yet possible (the L3b
# serializers and L4b host parsers do not exist and the current builder is
# lossy); D2 itself frames "round-trip" as REPRESENTABILITY -- "a single opaque
# boolean cannot round-trip the four WHATWG non-special shapes."

test_that("four WHATWG non-special shapes map to distinct state tuples", {
  # is_special = FALSE for the non-special scheme `foo`. Each row is the
  # hand-decomposition of one shape: the remainder after `foo:`, the authority
  # substring (NA when no `//` delimiter was present), and the isolated host.
  shapes <- list(
    list(
      url = "foo:bar", remainder = "bar", authority = NA, host = NA,
      exp_path = "opaque", exp_payload = NA_character_, exp_host = "absent"
    ),
    list(
      url = "foo:/bar", remainder = "/bar", authority = NA, host = NA,
      exp_path = "list", exp_payload = NA_character_, exp_host = "absent"
    ),
    list(
      # P1.2 D-A: `foo:///bar` is delimiter-PRESENT with an EMPTY payload and an
      # EMPTY host. The payload kind classifies the authority substring, the
      # host kind the host -- two independent axes (D-B), so the RFC `file:`
      # overlay cannot classify the identical shape differently (S1-F5).
      url = "foo:///bar", remainder = "///bar", authority = "", host = "",
      exp_path = "list", exp_payload = "empty", exp_host = "empty"
    ),
    list(
      url = "foo://[::1]/bar", remainder = "//[::1]/bar",
      authority = "[::1]", host = "[::1]",
      exp_path = "list", exp_payload = "present", exp_host = "present"
    )
  )

  tuples <- character(length(shapes))
  for (i in seq_along(shapes)) {
    s <- shapes[[i]]
    path_kind <- .whatwg_path_kind(FALSE, s$remainder)
    delimiter <- !is.na(s$authority)
    payload_kind <- .authority_payload_kind(s$authority)
    host_kind <- .host_kind(s$host)

    expect_identical(path_kind, s$exp_path, info = s$url)
    expect_identical(payload_kind, s$exp_payload, info = s$url)
    expect_identical(host_kind, s$exp_host, info = s$url)

    tuples[i] <- paste(delimiter, payload_kind, host_kind, path_kind, sep = "|")
  }

  # D2's core claim: a single opaque boolean is insufficient -- the four
  # (authority_delimiter_present, authority_payload_kind, host_kind, path_kind)
  # tuples are all DISTINCT.
  expect_length(unique(tuples), 4L)
})

test_that("presence_kind distinguishes empty from absent", {
  # Same classifier serves both query_kind and fragment_kind.
  expect_identical(.presence_kind(NA_character_), "absent")
  expect_identical(.presence_kind(""), "empty")
  expect_identical(.presence_kind("q=1"), "present")
  expect_identical(.presence_kind("section"), "present")
})

test_that("host_kind maps absent / empty / present", {
  expect_identical(.host_kind(NA_character_), "absent")
  expect_identical(.host_kind(""), "empty")
  expect_identical(.host_kind("example.com"), "present")
})

test_that("authority_payload_kind classifies the payload, not the host", {
  # P1.2 D-A.2. NA authority (no delimiter) -> not applicable; "" -> empty;
  # anything at all -> present, including userinfo-only and port-only payloads
  # whose HOST is empty. Host presence is host_kind's job (D-B).
  expect_identical(.authority_payload_kind(NA_character_), NA_character_)
  expect_identical(.authority_payload_kind(""), "empty")
  expect_identical(.authority_payload_kind("example.com"), "present")
  expect_identical(.authority_payload_kind("@"), "present")
  expect_identical(.authority_payload_kind(":80"), "present")
})

test_that("legacy authority_kind is derived from the two canonical fields", {
  # P1.2 D-D: the retired three-value enum survives ONLY as a read-only
  # projection. Its `empty` value is now REACHABLE -- as a payload state under
  # a present delimiter -- which is the unreachable-value defect S1-F5 named.
  expect_identical(.authority_kind(FALSE, NA_character_), "absent")
  expect_identical(.authority_kind(TRUE, "empty"), "empty")
  expect_identical(.authority_kind(TRUE, "present"), "present")

  # All three legacy values are produced by some canonical pair, so no value in
  # the legacy vocabulary is unreachable any more.
  expect_setequal(
    .authority_kind(c(FALSE, TRUE, TRUE), c(NA, "empty", "present")),
    .AUTHORITY_KIND
  )
})

test_that("whatwg_path_kind: non-special opaque trigger vs list", {
  expect_identical(.whatwg_path_kind(FALSE, "bar"), "opaque")
  expect_identical(.whatwg_path_kind(FALSE, "/bar"), "list")
  expect_identical(.whatwg_path_kind(FALSE, "///bar"), "list")
  # Missing remainder is treated as not starting with `/` -> opaque.
  expect_identical(.whatwg_path_kind(FALSE, NA_character_), "opaque")
})

test_that("a special scheme is always path_kind = list", {
  expect_identical(.whatwg_path_kind(TRUE, "bar"), "list")
  expect_identical(.whatwg_path_kind(TRUE, "/bar"), "list")
  expect_identical(.whatwg_path_kind(TRUE, ""), "list")
  expect_identical(.whatwg_path_kind(TRUE, NA_character_), "list")
})

test_that("rfc_path_form implements RFC 3986 section 3.3 disambiguation", {
  # With authority -> abempty (empty or begins `/`).
  expect_identical(.rfc_path_form(TRUE, ""), "abempty")
  expect_identical(.rfc_path_form(TRUE, "/a/b"), "abempty")
  expect_identical(.rfc_path_form(TRUE, NA_character_), "abempty")
  # Without authority.
  expect_identical(.rfc_path_form(FALSE, NA_character_), "empty")
  expect_identical(.rfc_path_form(FALSE, ""), "empty")
  expect_identical(.rfc_path_form(FALSE, "/a/b"), "absolute")
  expect_identical(.rfc_path_form(FALSE, "a/b"), "rootless")
  expect_identical(.rfc_path_form(FALSE, "a"), "rootless")
})

test_that("host-form thin mappers resolve unambiguous cases; defer rest", {
  # NA host -> NA (no form for an absent host); IPv6/IPv4/empty are resolved;
  # a present non-IP host is deferred (NA) -- L4b populates domain/reg-name/
  # opaque/ipvfuture.
  na_chr <- NA_character_
  expect_identical(.whatwg_host_form(na_chr, FALSE, FALSE), na_chr)
  expect_identical(.whatwg_host_form("[::1]", TRUE, FALSE), "ipv6")
  expect_identical(.whatwg_host_form("1.2.3.4", FALSE, TRUE), "ipv4")
  expect_identical(.whatwg_host_form("", FALSE, FALSE), "empty")
  expect_identical(.whatwg_host_form("example.com", FALSE, FALSE), na_chr)

  expect_identical(.rfc_host_form(na_chr, FALSE, FALSE), na_chr)
  expect_identical(.rfc_host_form("[::1]", TRUE, FALSE), "ipv6")
  expect_identical(.rfc_host_form("1.2.3.4", FALSE, TRUE), "ipv4")
  expect_identical(.rfc_host_form("", FALSE, FALSE), "empty")
  expect_identical(.rfc_host_form("example.com", FALSE, FALSE), na_chr)
})

test_that("classifiers are vectorized and stay within vocabulary", {
  is_special <- c(FALSE, FALSE, TRUE, FALSE)
  remainder  <- c("bar", "/bar", "bar", "///bar")
  pk <- .whatwg_path_kind(is_special, remainder)
  expect_length(pk, 4L)
  expect_true(all(pk %in% .PATH_KIND))
  expect_identical(pk, c("opaque", "list", "list", "list"))

  hosts <- c(NA, "", "example.com", "[::1]")
  hk <- .host_kind(hosts)
  expect_length(hk, 4L)
  expect_true(all(hk %in% .HOST_KIND))

  qk <- .presence_kind(c(NA, "", "q=1"))
  expect_length(qk, 3L)
  expect_true(all(qk %in% .PRESENCE_KIND))

  apk <- .authority_payload_kind(c(NA, "", "example.com"))
  expect_length(apk, 3L)
  expect_true(all(apk[!is.na(apk)] %in% .AUTHORITY_PAYLOAD_KIND))

  ak <- .authority_kind(c(FALSE, TRUE, TRUE), c(NA, "empty", "present"))
  expect_length(ak, 3L)
  expect_true(all(ak %in% .AUTHORITY_KIND))

  rf <- .rfc_path_form(c(TRUE, FALSE, FALSE, FALSE), c("/a", "", "/a", "a"))
  expect_length(rf, 4L)
  expect_true(all(rf %in% .RFC_PATH_FORM))
})
