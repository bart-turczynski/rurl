# RURL-pntbujzc, maintainer ruling 2026-10-25 (RUL-031): get_mailto_recipients()
# keeps classifying the recipients of a mailto: URL the selected standard
# rejects, and every recipient row carries that URL's parse verdict
# (`parse_status`) and its diagnostics (`url_diagnostics`). The classification
# is a diagnostic fact; the verdict columns say whether the URL itself parsed.
# Under rfc3986 a raw non-ASCII character is invalid (RFC 3986 sections 2, 2.5;
# RFC 6068 section 2 percent-encodes it as UTF-8), its percent-encoded twin is
# valid.

mv_facts <- function(urls, ...) {
  get_mailto_recipients(urls, scheme_acceptance = "general", ...)
}

test_that("a rejected rfc3986 mailto row keeps its recipients and says it failed", {
  r <- mv_facts("mailto:a@bücher.de", url_standard = "rfc3986")
  expect_identical(nrow(r), 1L)
  expect_identical(r$mailto_domain_form, "idna2008-domain")
  expect_identical(r$parse_status, "error")
  expect_identical(r$url_diagnostics, list("unicode-outside-rfc3986-uri"))
  expect_identical(
    safe_parse_urls("mailto:a@bücher.de", url_standard = "rfc3986",
                    scheme_acceptance = "general")$parse_status,
    "error"
  )
})

test_that("the percent-encoded twin parses and classifies the same domain", {
  r <- mv_facts("mailto:a@b%C3%BCcher.de", url_standard = "rfc3986")
  expect_identical(r$mailto_domain_form, "idna2008-domain")
  expect_identical(r$parse_status, "ok")
  expect_false("unicode-outside-rfc3986-uri" %in% r$url_diagnostics[[1L]])
})

test_that("an ASCII grammar failure keeps its recipients too", {
  r <- mv_facts("mailto:a b@x.de", url_standard = "rfc3986")
  expect_identical(nrow(r), 1L)
  expect_identical(r$parse_status, "error")
  expect_identical(r$url_diagnostics, list(character(0)))
})

test_that("every recipient of a URL carries that URL's verdict", {
  urls <- c("mailto:ok@x.de,two@y.de", "mailto:ä@x.de,b@y.de")
  r <- mv_facts(urls, url_standard = "rfc3986")
  expect_identical(r$url, rep(urls, each = 2L))
  expect_identical(r$recipient_index, c(1L, 2L, 1L, 2L))
  expect_identical(r$parse_status, c("ok", "ok", "error", "error"))
  status <- safe_parse_urls(urls, url_standard = "rfc3986",
                            scheme_acceptance = "general")$parse_status
  expect_identical(r$parse_status, rep(status, each = 2L))
  diags <- get_url_diagnostics(urls, url_standard = "rfc3986",
                               scheme_acceptance = "general")
  expect_identical(r$url_diagnostics, rep(diags, each = 2L))
})

test_that("whatwg accepts the raw row, and the columns follow that verdict", {
  r <- mv_facts("mailto:a@bücher.de", url_standard = "whatwg")
  expect_identical(r$mailto_domain_form, "idna2008-domain")
  expect_identical(r$parse_status, "ok")
  expect_identical(
    r$url_diagnostics,
    list(get_url_diagnostics("mailto:a@bücher.de",
                             url_standard = "whatwg",
                             scheme_acceptance = "general"))
  )
})

test_that("the recipient classification does not move with the verdict", {
  facts <- c("mailto_local_part_form", "mailto_domain_form",
             "smtp_mailbox_rhs_syntax_form", "public_suffix_known")
  urls <- c("mailto:a@bücher.de", "mailto:ä@x.de")
  expect_identical(
    mv_facts(urls, url_standard = "rfc3986")[facts],
    mv_facts(urls, url_standard = "whatwg")[facts]
  )
})
