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
  # hand-decomposition of one shape: the remainder after `foo:`, whether a `//`
  # authority is present, and the isolated host.
  shapes <- list(
    list(
      url = "foo:bar", remainder = "bar", has_dbl_slash = FALSE, host = NA,
      exp_path = "opaque", exp_auth = "absent", exp_host = "absent"
    ),
    list(
      url = "foo:/bar", remainder = "/bar", has_dbl_slash = FALSE, host = NA,
      exp_path = "list", exp_auth = "absent", exp_host = "absent"
    ),
    list(
      # ADR 0012 D2 (lines 257-258, 270-271) is dispositive: `foo:///bar` is
      # authority PRESENT with host EMPTY -- authority_kind records only WHETHER
      # a `//` authority was present, and host emptiness is carried by host_kind
      # (not by authority_kind). The vocab's `empty` is reserved for a genuinely
      # empty authority *component* (L4b, once userinfo/port are modeled).
      url = "foo:///bar", remainder = "///bar", has_dbl_slash = TRUE, host = "",
      exp_path = "list", exp_auth = "present", exp_host = "empty"
    ),
    list(
      url = "foo://[::1]/bar", remainder = "//[::1]/bar",
      has_dbl_slash = TRUE, host = "[::1]",
      exp_path = "list", exp_auth = "present", exp_host = "present"
    )
  )

  tuples <- character(length(shapes))
  for (i in seq_along(shapes)) {
    s <- shapes[[i]]
    path_kind <- .whatwg_path_kind(FALSE, s$remainder)
    auth_kind <- .authority_kind(s$has_dbl_slash)
    host_kind <- .host_kind(s$host)

    expect_identical(path_kind, s$exp_path, info = s$url)
    expect_identical(auth_kind, s$exp_auth, info = s$url)
    expect_identical(host_kind, s$exp_host, info = s$url)

    tuples[i] <- paste(auth_kind, host_kind, path_kind, sep = "|")
  }

  # D2's core claim: a single opaque boolean is insufficient -- the four
  # (authority_kind, host_kind, path_kind) tuples are all DISTINCT.
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

test_that("authority_kind records only whether a // authority was present", {
  # ADR 0012 D2: authority_kind records WHETHER `//` was present; host
  # emptiness is host_kind's job, not authority_kind's. No `//` -> absent;
  # `//` present -> present (regardless of what host it carries).
  expect_identical(.authority_kind(FALSE), "absent")
  expect_identical(.authority_kind(TRUE), "present")
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

  ak <- .authority_kind(c(FALSE, TRUE, TRUE))
  expect_length(ak, 3L)
  expect_true(all(ak %in% .AUTHORITY_KIND))

  rf <- .rfc_path_form(c(TRUE, FALSE, FALSE, FALSE), c("/a", "", "/a", "a"))
  expect_length(rf, 4L)
  expect_true(all(rf %in% .RFC_PATH_FORM))
})
