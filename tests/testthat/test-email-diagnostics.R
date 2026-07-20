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
      "public_suffix_known", "smtp_domain_wire_form",
      "smtp_envelope_wire_mode", "smtp_envelope_address_requires_smtputf8",
      "smtp_local_part_length_ok", "smtp_direct_forward_path_fits")
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

# --- SMTP wire-projection tier (opt-in, RURL-nivxvldd) -----------------------

test_that("wire columns are always present but sentinel by default", {
  r <- get_mailto_recipients("mailto:a@example.com",
    scheme_acceptance = "general")
  expect_identical(ncol(r), 11L)
  expect_identical(r$smtp_domain_wire_form, "unavailable")
  expect_identical(r$smtp_envelope_wire_mode, "unavailable")
  expect_true(is.na(r$smtp_envelope_address_requires_smtputf8))
  expect_true(is.na(r$smtp_local_part_length_ok))
  expect_true(is.na(r$smtp_direct_forward_path_fits))
})

test_that("an all-ASCII address needs no SMTPUTF8", {
  r <- get_mailto_recipients("mailto:jane@example.com",
    scheme_acceptance = "general", smtp_wire = TRUE)
  expect_identical(r$smtp_domain_wire_form, "ascii-domain")
  expect_identical(r$smtp_envelope_wire_mode, "ascii")
  expect_false(r$smtp_envelope_address_requires_smtputf8)
})

test_that("a U-label domain requires SMTPUTF8; its A-label does not", {
  u <- get_mailto_recipients("mailto:a@münchen.de",
    scheme_acceptance = "general", smtp_wire = TRUE)
  expect_identical(u$smtp_domain_wire_form, "u-label-domain")
  expect_identical(u$smtp_envelope_wire_mode, "smtputf8")
  expect_true(u$smtp_envelope_address_requires_smtputf8)

  a <- get_mailto_recipients("mailto:a@xn--mnchen-3ya.de",
    scheme_acceptance = "general", smtp_wire = TRUE)
  expect_identical(a$smtp_domain_wire_form, "a-label-domain")
  expect_false(a$smtp_envelope_address_requires_smtputf8)
})

test_that("a non-ASCII local-part alone requires SMTPUTF8, independent of the
           (invalid) RFC 6068 verdict", {
  r <- get_mailto_recipients("mailto:ünïcöde@example.com",
    scheme_acceptance = "general", smtp_wire = TRUE)
  expect_identical(r$mailto_local_part_form, "invalid")
  expect_identical(r$smtp_domain_wire_form, "ascii-domain")
  expect_true(r$smtp_envelope_address_requires_smtputf8)
})

test_that("a bracketed IPv4 literal projects as an ASCII address-literal", {
  r <- get_mailto_recipients("mailto:x@[192.168.0.1]",
    scheme_acceptance = "general", smtp_wire = TRUE)
  expect_identical(r$smtp_domain_wire_form, "address-literal")
  expect_identical(r$smtp_envelope_wire_mode, "ascii")
})

test_that("the 64-octet local-part limit is measured on wire octets", {
  ok <- get_mailto_recipients(
    paste0("mailto:", strrep("a", 64L), "@example.com"),
    scheme_acceptance = "general", smtp_wire = TRUE
  )
  expect_true(ok$smtp_local_part_length_ok)
  over <- get_mailto_recipients(
    paste0("mailto:", strrep("a", 65L), "@example.com"),
    scheme_acceptance = "general", smtp_wire = TRUE
  )
  expect_false(over$smtp_local_part_length_ok)
})

test_that("an unprojectable (mailto bracketed, non-literal) RHS is unavailable
           but a valid local-part still gets its length", {
  r <- get_mailto_recipients("mailto:user@%5Ba,b%5D",
    scheme_acceptance = "general", smtp_wire = TRUE)
  expect_identical(r$smtp_domain_wire_form, "unavailable")
  expect_identical(r$smtp_envelope_wire_mode, "unavailable")
  expect_true(is.na(r$smtp_envelope_address_requires_smtputf8))
  expect_true(r$smtp_local_part_length_ok)
  expect_true(is.na(r$smtp_direct_forward_path_fits))
})

test_that("smtp_wire must be a valid flag", {
  expect_error(
    get_mailto_recipients("mailto:a@b.com", scheme_acceptance = "general",
      smtp_wire = "yes")
  )
})

# --- recipient extraction via the standard accessors (ADR 0012 D7) -----------

test_that("mailto recipient parts extract through the standard accessors", {
  ga <- list(scheme_acceptance = "general", url_standard = "rfc3986")
  expect_identical(
    do.call(get_host, c(list("mailto:jane@example.com"), ga)), "example.com"
  )
  expect_identical(
    do.call(get_domain, c(list("mailto:jane@example.com"), ga)), "example.com"
  )
  expect_identical(
    do.call(get_tld, c(list("mailto:jane@example.com"), ga)), "com"
  )
  expect_identical(
    do.call(get_user, c(list("mailto:jane@example.com"), ga)), "jane"
  )
})

test_that("a mailto recipient decomposes exactly like a web host (PSL)", {
  ga <- list(scheme_acceptance = "general", url_standard = "rfc3986")
  u <- "mailto:bob@mail.example.co.uk"
  expect_identical(do.call(get_host, c(list(u), ga)), "mail.example.co.uk")
  expect_identical(do.call(get_subdomain, c(list(u), ga)), "mail")
  expect_identical(do.call(get_domain, c(list(u), ga)), "example.co.uk")
  expect_identical(do.call(get_tld, c(list(u), ga)), "co.uk")
})

test_that("multi-recipient extraction uses the first recipient", {
  ga <- list(scheme_acceptance = "general", url_standard = "rfc3986")
  u <- "mailto:a@example.com,b@other.org"
  expect_identical(do.call(get_host, c(list(u), ga)), "example.com")
  expect_identical(do.call(get_user, c(list(u), ga)), "a")
})

test_that("extraction is a strict no-op under the default web acceptance", {
  expect_true(is.na(get_host("mailto:jane@example.com")))
  expect_true(is.na(get_user("mailto:jane@example.com")))
  expect_true(is.na(get_domain("mailto:jane@example.com")))
})

test_that("extraction never disturbs the mailto clean_url / round-trip", {
  ga <- list(scheme_acceptance = "general", url_standard = "rfc3986")
  for (u in c("mailto:jane@example.com", "mailto:a@x.com,b@y.com",
    "mailto:a@example.com?subject=hi")) {
    expect_identical(do.call(get_clean_url, c(list(u), ga)), u)
    expect_identical(do.call(get_parse_status, c(list(u), ga)), "ok")
  }
})

# --- Locale-invariance of email unit-role case mapping (RURL-hckljpwh) ------
# `.email_unit_role()` used base `toupper()` to fold a `%XX` triplet before
# comparing it to the encoded delimiters (`%22` / `%5B` / `%5D` / `%5C`). That
# is the same locale-tailored case mapping RURL-ugfpuotu removed from the URL
# syntax path, and it carried a second, portable defect: base `toupper()`
# routes through `utf8towcs()`, which REJECTS the code points the hostile-input
# corpora carry (the U+FFFF noncharacter), so a mailto containing one raised
# "invalid input in 'utf8towcs'" out of `get_mailto_recipients()`. The ASCII
# helper's `stringi` fallback survives those rows, which is exactly why it
# exists.
#
# Local twin of the same-named helper in test-parse-phases.R (testthat gives
# each test file its own environment, so a helper cannot be shared between
# them). The locale is forced through `stringi::stri_locale_set()` rather than
# the system locale so the tailoring is available everywhere, including CI.
.with_turkish_stri_locale <- function(code) {
  old <- suppressMessages(stringi::stri_locale_set("tr_TR"))
  on.exit(
    {
      suppressMessages(stringi::stri_locale_set(old))
      rurl_clear_caches()
    },
    add = TRUE
  )
  rurl_clear_caches()
  force(code)
}

test_that("a U+FFFF unit classifies as data instead of erroring", {
  expect_identical(rurl:::.email_unit_role("￿"), "other")
})

test_that("get_mailto_recipients() survives a U+FFFF local part", {
  r <- get_mailto_recipients("mailto:a￿b@example.com",
    scheme_acceptance = "general")
  expect_identical(nrow(r), 1L)
  expect_identical(r$mailto_local_part_form, "invalid")
})

test_that("unit roles are stable under a Turkish session locale", {
  .with_turkish_stri_locale({
    # Guard: the tailoring is actually armed in this session.
    expect_identical(stringi::stri_trans_tolower("WIKI"), "wıkı")

    # Encoded delimiters fold ASCII-only, in either input case.
    expect_identical(rurl:::.email_unit_role("%22"), "quote")
    expect_identical(rurl:::.email_unit_role("%5b"), "bopen")
    expect_identical(rurl:::.email_unit_role("%5d"), "bclose")
    expect_identical(rurl:::.email_unit_role("%5c"), "bslash")
    # `%2C` / `%40` stay DATA; raw delimiters keep their roles.
    expect_identical(rurl:::.email_unit_role("%2c"), "other")
    expect_identical(rurl:::.email_unit_role("%40"), "other")
    expect_identical(rurl:::.email_unit_role(","), "comma")
    expect_identical(rurl:::.email_unit_role("@"), "at")
    # The dotless/dotted i pair is data whichever way a locale maps it.
    expect_identical(rurl:::.email_unit_role("ı"), "other")
    expect_identical(rurl:::.email_unit_role("İ"), "other")

    r <- get_mailto_recipients(
      "mailto:%22a,b%22@example.com,c@example.org",
      scheme_acceptance = "general"
    )
    expect_identical(nrow(r), 2L)
  })
})
