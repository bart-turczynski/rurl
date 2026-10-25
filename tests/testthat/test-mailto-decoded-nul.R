# RURL-rujjbahq: a decoded %00 in a mailto addr-spec half. Every case runs
# under LC_CTYPE C and under a UTF-8 LC_CTYPE, alone and between good
# neighbors, and the batch must agree with the solo call.

dn_neighbors <- c("mailto:one@good.example.com",
                  "mailto:two@good.example.com?subject=hi")

dn_locales <- function() {
  utf8 <- hi_utf8_locale()
  c("C", if (!is.na(utf8)) utf8)
}

dn_in_ctype <- function(locale, code) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  withr::with_locale(c(LC_CTYPE = locale), force(code))
}

dn_cols <- c("mailto_local_part_form", "mailto_domain_form",
             "smtp_mailbox_rhs_syntax_form", "public_suffix_known",
             "smtp_domain_wire_form", "smtp_envelope_wire_mode",
             "smtp_envelope_address_requires_smtputf8",
             "smtp_local_part_length_ok", "smtp_direct_forward_path_fits")

# The single recipient row of `url` under smtp_wire = TRUE, as a plain list of
# the fact columns, checked identical across locales and against a batch with
# the neighbors (whose own rows must stay the plain ascii projection).
dn_row <- function(url) {
  vals <- list()
  for (loc in dn_locales()) {
    vals[[loc]] <- dn_in_ctype(loc, {
      batch <- get_mailto_recipients(c(dn_neighbors[1L], url, dn_neighbors[2L]),
                                     smtp_wire = TRUE)
      solo <- get_mailto_recipients(url, smtp_wire = TRUE)
      expect_identical(nrow(solo), 1L, label = paste(url, loc))
      nb <- batch[batch$url %in% dn_neighbors, , drop = FALSE]
      expect_identical(nb$mailto_local_part_form, rep("dot-atom-text", 2L))
      expect_identical(nb$mailto_domain_form, rep("ascii-dot-atom-text", 2L))
      expect_identical(nb$smtp_envelope_wire_mode, rep("ascii", 2L))
      mine <- batch[batch$url == url, , drop = FALSE]
      expect_identical(as.list(mine[dn_cols]), as.list(solo[dn_cols]),
                       label = paste(url, loc))
      as.list(solo[dn_cols])
    })
  }
  for (k in names(vals)[-1L]) {
    expect_identical(vals[[k]], vals[[1L]], label = paste(url, k))
  }
  vals[[1L]]
}

# get_user() / get_host() of `url` (scheme_acceptance = "general"), checked
# identical across locales and both standards, solo and in a batch.
dn_parts <- function(url) {
  vals <- list()
  for (loc in dn_locales()) {
    for (std in c("rfc3986", "whatwg")) {
      vals[[paste(loc, std)]] <- dn_in_ctype(loc, {
        urls <- c(dn_neighbors[1L], url, dn_neighbors[2L])
        user <- get_user(urls, url_standard = std,
                         scheme_acceptance = "general")
        host <- get_host(urls, url_standard = std,
                         scheme_acceptance = "general")
        expect_identical(user[c(1L, 3L)], c("one", "two"))
        expect_identical(host[c(1L, 3L)], rep("good.example.com", 2L))
        solo <- list(
          user = get_user(url, url_standard = std,
                          scheme_acceptance = "general"),
          host = get_host(url, url_standard = std,
                          scheme_acceptance = "general")
        )
        expect_identical(list(user = user[2L], host = host[2L]), solo,
                         label = paste(url, loc, std))
        solo
      })
    }
  }
  for (k in names(vals)[-1L]) {
    expect_identical(vals[[k]], vals[[1L]], label = paste(url, k))
  }
  vals[[1L]]
}

dn_invalid_local <- function(row) {
  expect_identical(row$mailto_local_part_form, "invalid")
  expect_identical(row$smtp_envelope_wire_mode, "unavailable")
  expect_identical(row$smtp_envelope_address_requires_smtputf8, NA)
  expect_identical(row$smtp_local_part_length_ok, NA)
  expect_identical(row$smtp_direct_forward_path_fits, NA)
}

dn_invalid_domain <- function(row) {
  expect_identical(row$mailto_domain_form, "invalid")
  expect_identical(row$smtp_mailbox_rhs_syntax_form, "invalid")
  expect_identical(row$public_suffix_known, NA)
  expect_identical(row$smtp_domain_wire_form, "unavailable")
  expect_identical(row$smtp_envelope_wire_mode, "unavailable")
  expect_identical(row$smtp_envelope_address_requires_smtputf8, NA)
  expect_identical(row$smtp_direct_forward_path_fits, NA)
}

# --- Pins: behavior the fix must leave alone ---------------------------------

test_that("a plain recipient projects to the ascii wire", {
  row <- dn_row("mailto:jane@b.com")
  expect_identical(row$mailto_local_part_form, "dot-atom-text")
  expect_identical(row$mailto_domain_form, "ascii-dot-atom-text")
  expect_identical(row$smtp_envelope_wire_mode, "ascii")
  expect_true(row$smtp_local_part_length_ok)
  expect_true(row$smtp_direct_forward_path_fits)
  expect_identical(dn_parts("mailto:jane@b.com"),
                   list(user = "jane", host = "b.com"))
})

test_that("a decoded %01 or %20 local part is invalid, the user kept", {
  for (u in c("mailto:a%01b@b.com", "mailto:a%20b@b.com")) {
    row <- dn_row(u)
    dn_invalid_local(row)
    expect_identical(row$mailto_domain_form, "ascii-dot-atom-text",
                     label = u)
    expect_true(row$public_suffix_known, label = u)
  }
  expect_identical(dn_parts("mailto:a%01b@b.com"),
                   list(user = "a\001b", host = "b.com"))
  expect_identical(dn_parts("mailto:a%20b@b.com"),
                   list(user = "a b", host = "b.com"))
})

test_that("a decoded %01 or %20 domain is invalid, the host NA", {
  for (u in c("mailto:a@b%01c.com", "mailto:a@b%20c.com")) {
    row <- dn_row(u)
    dn_invalid_domain(row)
    expect_identical(row$mailto_local_part_form, "dot-atom-text", label = u)
    expect_identical(dn_parts(u), list(user = "a", host = NA_character_),
                     label = u)
  }
})

test_that("an undecodable UTF-8 half stays NA (RURL-gndbrpam)", {
  dn_invalid_local(dn_row("mailto:a%E9@b.com"))
  dn_invalid_domain(dn_row("mailto:a@b%E9.com"))
  expect_identical(dn_parts("mailto:a%E9@b.com"),
                   list(user = NA_character_, host = "b.com"))
  expect_identical(dn_parts("mailto:a@b%E9.com"),
                   list(user = "a", host = NA_character_))
})
