# Tests for get_mailto_recipients() (ADR 0012 D7, RURL-zyoahfpv).

test_that("a single simple recipient classifies across all three grammars", {
  r <- get_mailto_recipients("mailto:jane@example.com",
    scheme_acceptance = "general")
  expect_s3_class(r, "data.frame")
  expect_identical(nrow(r), 1L)
  expect_identical(r$recipient_index, 1L)
  expect_identical(r$mailto_local_part_form, "dot-atom-text")
  expect_identical(r$mailto_domain_form, "ascii-dot-atom-text")
  expect_identical(r$smtp_mailbox_rhs_syntax_form, "domain")
  expect_true(r$public_suffix_known)
})

test_that("the positional list splits into one row per recipient", {
  r <- get_mailto_recipients("mailto:a@example.com,b@example.org",
    scheme_acceptance = "general")
  expect_identical(nrow(r), 2L)
  expect_identical(r$recipient_index, c(1L, 2L))
  expect_identical(r$url, rep("mailto:a@example.com,b@example.org", 2L))
})

test_that("a raw comma inside a quoted-string does NOT split recipients", {
  r <- get_mailto_recipients("mailto:a@example.com,\"b,c\"@example.org",
    scheme_acceptance = "general")
  expect_identical(nrow(r), 2L)
  expect_identical(
    r$mailto_local_part_form,
    c("dot-atom-text", "quoted-string")
  )
})

test_that("an ENCODED quote (%22) still protects an inner comma", {
  # mailto:%22a,b%22@example.org -- one recipient, quoted-string local-part.
  r <- get_mailto_recipients("mailto:%22a,b%22@example.org",
    scheme_acceptance = "general")
  expect_identical(nrow(r), 1L)
  expect_identical(r$mailto_local_part_form, "quoted-string")
})

test_that("an ENCODED bracket (%5B/%5D) protects a comma and yields a mailto
           bracketed-domain that is NOT a valid SMTP address-literal", {
  r <- get_mailto_recipients("mailto:user@%5Ba,b%5D",
    scheme_acceptance = "general")
  expect_identical(nrow(r), 1L)
  expect_identical(r$mailto_domain_form, "bracketed-domain")
  expect_identical(r$smtp_mailbox_rhs_syntax_form, "invalid")
  expect_true(is.na(r$public_suffix_known))
})

test_that("%2C is data, never a separator", {
  r <- get_mailto_recipients("mailto:a%2Cb@example.com",
    scheme_acceptance = "general")
  expect_identical(nrow(r), 1L)
  # decoded local-part "a,b" is not a valid dot-atom (comma is not atext).
  expect_identical(r$mailto_local_part_form, "invalid")
})

test_that("%40 is data, never the addr-spec separator", {
  # No structural '@' -> malformed addr-spec, all invalid.
  r <- get_mailto_recipients("mailto:user%40example.com",
    scheme_acceptance = "general")
  expect_identical(nrow(r), 1L)
  expect_identical(r$mailto_local_part_form, "invalid")
  expect_identical(r$mailto_domain_form, "invalid")
  expect_true(is.na(r$public_suffix_known))
})

test_that("mailto and SMTP domain vocabularies diverge (IDNA vs base SMTP)", {
  r <- get_mailto_recipients("mailto:a@über.de",
    scheme_acceptance = "general")
  expect_identical(r$mailto_domain_form, "idna2008-domain")
  expect_identical(r$smtp_mailbox_rhs_syntax_form, "invalid")
  expect_true(r$public_suffix_known)
})

test_that("a bracketed IPv4 literal is a valid SMTP address-literal", {
  r <- get_mailto_recipients("mailto:x@[192.168.0.1]",
    scheme_acceptance = "general")
  expect_identical(r$mailto_domain_form, "bracketed-domain")
  expect_identical(r$smtp_mailbox_rhs_syntax_form, "address-literal")
})

test_that("a recipient with no structural @ is invalid, not indeterminate", {
  r <- get_mailto_recipients("mailto:noatsign", scheme_acceptance = "general")
  expect_identical(nrow(r), 1L)
  expect_identical(r$mailto_local_part_form, "invalid")
})

test_that("public-suffix-known is non-validating and domain-form only", {
  # dotless dot-atom is a valid mailto domain form but not PSL-known.
  r <- get_mailto_recipients("mailto:a@localhost",
    scheme_acceptance = "general")
  expect_identical(r$mailto_domain_form, "ascii-dot-atom-text")
  expect_false(r$public_suffix_known)
})

test_that("empty positional to yields zero rows", {
  expect_identical(
    nrow(get_mailto_recipients("mailto:?subject=x",
      scheme_acceptance = "general")),
    0L
  )
  expect_identical(
    nrow(get_mailto_recipients("mailto:", scheme_acceptance = "general")),
    0L
  )
})

test_that("non-mailto URLs contribute no rows", {
  expect_identical(
    nrow(get_mailto_recipients("http://example.com/",
      scheme_acceptance = "general")),
    0L
  )
})

test_that("web acceptance never parses mailto (zero rows)", {
  expect_identical(
    nrow(get_mailto_recipients("mailto:a@b.com", scheme_acceptance = "web")),
    0L
  )
})

test_that("the return shape is a stable data.frame for degenerate input", {
  z <- get_mailto_recipients(character(0), scheme_acceptance = "general")
  expect_s3_class(z, "data.frame")
  expect_identical(nrow(z), 0L)
  expect_named(
    z,
    c("url", "recipient_index", "mailto_local_part_form",
      "mailto_domain_form", "smtp_mailbox_rhs_syntax_form",
      "public_suffix_known")
  )
})

test_that("mixed vectors keep per-URL provenance and skip non-mailto rows", {
  r <- get_mailto_recipients(
    c("http://x.com/", "mailto:a@example.com,b@example.org", "mailto:?to=x"),
    scheme_acceptance = "general"
  )
  expect_identical(nrow(r), 2L)
  expect_identical(unique(r$url), "mailto:a@example.com,b@example.org")
})

test_that("non-character input is rejected", {
  expect_error(get_mailto_recipients(42), "must be a character vector")
})
