# Layer 4b posture host/opaque parsers (ADR 0012 D1/D2 + Appendix A.1/A.2,
# RURL-yutinyhb).
#
# These tests drive the PARSER FUNCTIONS directly at the STATE level. The
# `general` acceptance axis is unexposed and opaque inputs still error through
# the public parse path, so -- as with test-parse-state.R -- the proof works by
# feeding inputs to the additive functions and asserting the decomposed
# components + internal state kinds. Nothing here exercises a public parse.

# --- WHATWG: the four non-special shapes + case preservation -----------------

test_that("four WHATWG non-special shapes decompose to distinct state", {
  r <- .parse_opaque_urls_vec(
    c("foo:bar", "foo:/bar", "foo:///bar", "foo://[::1]/bar"), "whatwg"
  )

  expect_true(all(r$ok))
  expect_identical(r$scheme, rep("foo", 4L))

  # foo:bar -- opaque path, host ABSENT, no authority.
  expect_identical(r$path_kind[[1L]], "opaque")
  expect_identical(r$path[[1L]], "bar")
  expect_identical(r$host_kind[[1L]], "absent")
  expect_identical(r$authority_kind[[1L]], "absent")
  expect_true(is.na(r$host[[1L]]))

  # foo:/bar -- list path, host ABSENT, no authority.
  expect_identical(r$path_kind[[2L]], "list")
  expect_identical(r$path[[2L]], "/bar")
  expect_identical(r$host_kind[[2L]], "absent")
  expect_identical(r$authority_kind[[2L]], "absent")

  # foo:///bar -- list path, authority PRESENT (any `//`), host EMPTY. Matches
  # the L3a `.authority_kind()` classifier + ADR 0012 D2 (lines 257-258,
  # 270-271): host emptiness is host_kind's job, not authority_kind's.
  expect_identical(r$path_kind[[3L]], "list")
  expect_identical(r$path[[3L]], "/bar")
  expect_identical(r$host[[3L]], "")
  expect_identical(r$host_kind[[3L]], "empty")
  expect_identical(r$authority_kind[[3L]], "present")
  expect_identical(r$host_form[[3L]], "empty")

  # foo://[::1]/bar -- list path, IPv6 host.
  expect_identical(r$path_kind[[4L]], "list")
  expect_identical(r$host[[4L]], "[::1]")
  expect_identical(r$host_kind[[4L]], "present")
  expect_identical(r$authority_kind[[4L]], "present")
  expect_identical(r$host_form[[4L]], "ipv6")

  # The four (authority_kind, host_kind, path_kind) tuples are all DISTINCT.
  tuples <- paste(r$authority_kind, r$host_kind, r$path_kind, sep = "|")
  expect_length(unique(tuples), 4L)
})

test_that("WHATWG opaque host preserves ASCII case and resolves form opaque", {
  r <- .parse_opaque_urls_vec("foo://Example.COM/p", "whatwg")
  expect_true(r$ok)
  expect_identical(r$host, "Example.COM") # NOT lowercased
  expect_identical(r$host_form, "opaque")
  expect_identical(r$path, "/p")
  expect_identical(r$host_kind, "present")
})

# --- WHATWG: forbidden-host reject contract ----------------------------------

test_that("WHATWG opaque host rejects forbidden-host code points", {
  # Reject is signalled by ok = FALSE (the host parse fails). Space and `<` are
  # forbidden-host code points; the raw host is retained on the failed row.
  space <- .parse_opaque_urls_vec("foo://ex ample.com", "whatwg")
  expect_false(space$ok)

  lt <- .parse_opaque_urls_vec("foo://a<b", "whatwg")
  expect_false(lt$ok)

  # `%` is NOT a forbidden-host code point (it is forbidden only for DOMAIN
  # hosts). An opaque host carrying `%` parses ok; a valid pct-triple is kept.
  pct <- .parse_opaque_urls_vec("foo://ex%20ample.com", "whatwg")
  expect_true(pct$ok)
  expect_identical(pct$host, "ex%20ample.com")
})

test_that("WHATWG opaque host UTF-8 percent-encodes non-ASCII, not punycode", {
  r <- .parse_opaque_urls_vec("foo://exämple", "whatwg")
  expect_true(r$ok)
  expect_identical(r$host, "ex%C3%A4mple") # UTF-8 %-encoded, C0 encoder
  expect_false(grepl("xn--", r$host, fixed = TRUE)) # NOT routed through IDNA
  expect_identical(r$host_form, "opaque")
})

# --- RFC 3986 host forms -----------------------------------------------------

test_that("RFC host resolves reg-name / IPv6 / IPvFuture forms", {
  reg <- .parse_opaque_urls_vec("foo://a.b.c/p", "rfc3986")
  expect_true(reg$ok)
  expect_identical(reg$host, "a.b.c") # source-preserving
  expect_identical(reg$host_form, "reg-name")
  expect_identical(reg$rfc_path_form, "abempty")
  expect_true(is.na(reg$path_kind)) # no opaque/list discriminator under RFC

  v6 <- .parse_opaque_urls_vec("foo://[::1]/p", "rfc3986")
  expect_true(v6$ok)
  expect_identical(v6$host, "[::1]")
  expect_identical(v6$host_form, "ipv6")

  vf <- .parse_opaque_urls_vec("foo://[v1.x]/p", "rfc3986")
  expect_true(vf$ok)
  expect_identical(vf$host, "[v1.x]")
  expect_identical(vf$host_form, "ipvfuture")
})

test_that("RFC host preserves case and is not routed through punycode", {
  r <- .parse_opaque_urls_vec("foo://Example.COM/p", "rfc3986")
  expect_true(r$ok)
  expect_identical(r$host, "Example.COM")
  expect_false(grepl("xn--", r$host, fixed = TRUE))
})

# --- RFC 8089 file overlay ---------------------------------------------------

test_that("RFC file overlay parses absolute path and localhost -> empty host", {
  abs <- .parse_rfc_file_urls_vec("file:/abs/path")
  expect_true(abs$ok)
  expect_identical(abs$scheme, "file")
  expect_identical(abs$path, "/abs/path")
  expect_identical(abs$rfc_path_form, "absolute")
  expect_identical(abs$authority_kind, "absent")
  expect_true(is.na(abs$host)) # no authority -> host absent

  loc <- .parse_rfc_file_urls_vec("file://localhost/x")
  expect_true(loc$ok)
  expect_identical(loc$host, "") # localhost collapses to empty host
  expect_identical(loc$host_kind, "empty")
  expect_identical(loc$path, "/x")

  named <- .parse_rfc_file_urls_vec("file://example.com/x")
  expect_true(named$ok)
  expect_identical(named$host, "example.com")
  expect_identical(named$host_form, "reg-name")
})

# --- host-FORM mappers: populated forms + L3a back-compat ---------------------

test_that("host-form mappers populate present non-IP forms when asked", {
  na_chr <- NA_character_

  # WHATWG: is_special resolves domain vs opaque; default NA leaves it deferred
  # (L3a back-compat -- the L3a representability tests must be unaffected).
  expect_identical(
    .whatwg_host_form("example.com", FALSE, FALSE, is_special = TRUE), "domain"
  )
  expect_identical(
    .whatwg_host_form("example.com", FALSE, FALSE, is_special = FALSE), "opaque"
  )
  expect_identical(.whatwg_host_form("example.com", FALSE, FALSE), na_chr)

  # RFC: resolve = TRUE splits reg-name vs bracketed ipvfuture; default defers.
  expect_identical(
    .rfc_host_form("a.b.c", FALSE, FALSE, resolve = TRUE), "reg-name"
  )
  expect_identical(
    .rfc_host_form("[v1.x]", FALSE, FALSE, resolve = TRUE), "ipvfuture"
  )
  expect_identical(.rfc_host_form("example.com", FALSE, FALSE), na_chr)
})

test_that("opaque parser handles empty and NA vectors", {
  empty <- .parse_opaque_urls_vec(character(0), "whatwg")
  expect_identical(empty$ok, logical(0))
  expect_identical(empty$host, character(0))

  na_row <- .parse_opaque_urls_vec(NA_character_, "whatwg")
  expect_false(na_row$ok)
  expect_true(is.na(na_row$host))
})
