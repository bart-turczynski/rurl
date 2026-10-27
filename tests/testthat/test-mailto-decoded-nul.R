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

# --- The defect: .pct_unescape() stops at a decoded NUL, so the prefix before
# it was classified and returned as if it were the whole field. NUL is neither
# atext nor dtext (RFC 5322 sections 3.2.3 and 3.4.1; RFC 6068 section 2). ---

test_that("a decoded NUL in the local part makes it invalid, the user NA", {
  for (u in c("mailto:a%00zz@b.com", "mailto:%00@b.com", "mailto:a%00@b.com",
              "mailto:a%000@b.com", "mailto:a%00%C3%A9@b.com",
              "mailto:%22a%00b%22@b.com", "mailto:a%00zz@b.com?subject=hi")) {
    row <- dn_row(u)
    dn_invalid_local(row)
    expect_identical(row$mailto_domain_form, "ascii-dot-atom-text",
                     label = u)
    expect_identical(row$smtp_domain_wire_form, "ascii-domain", label = u)
    expect_true(row$public_suffix_known, label = u)
    expect_identical(dn_parts(u), list(user = NA_character_, host = "b.com"),
                     label = u)
  }
})

test_that("a decoded NUL in the domain makes it invalid, the host NA", {
  for (u in c("mailto:a@b.com%00evil", "mailto:a@evil.com%00.good.com",
              "mailto:a@%00b.com", "mailto:a@b.com%00")) {
    row <- dn_row(u)
    dn_invalid_domain(row)
    expect_identical(row$mailto_local_part_form, "dot-atom-text", label = u)
    expect_true(row$smtp_local_part_length_ok, label = u)
    expect_identical(dn_parts(u), list(user = "a", host = NA_character_),
                     label = u)
  }
})

test_that("a decoded NUL in both halves leaves nothing valid", {
  row <- dn_row("mailto:a%00@b%00.com")
  dn_invalid_local(row)
  dn_invalid_domain(row)
  expect_identical(dn_parts("mailto:a%00@b%00.com"),
                   list(user = NA_character_, host = NA_character_))
})

test_that("only the recipient holding the NUL is invalid", {
  r <- get_mailto_recipients("mailto:a%00@b.com,c@d.com", smtp_wire = TRUE)
  expect_identical(r$mailto_local_part_form, c("invalid", "dot-atom-text"))
  expect_identical(r$smtp_envelope_wire_mode, c("unavailable", "ascii"))
  expect_identical(dn_parts("mailto:a%00@b.com,c@d.com"),
                   list(user = NA_character_, host = "b.com"))
})

test_that("an encoded percent before 00 is a literal, not a NUL", {
  row <- dn_row("mailto:a%2500@b.com")
  expect_identical(row$mailto_local_part_form, "dot-atom-text")
  expect_identical(row$smtp_envelope_wire_mode, "ascii")
  expect_identical(dn_parts("mailto:a%2500@b.com"),
                   list(user = "a%00", host = "b.com"))
})

test_that("every accessor of a NUL half is NA, the other half kept", {
  for (std in c("rfc3986", "whatwg")) {
    acc <- function(f, u) {
      f(u, url_standard = std, scheme_acceptance = "general")
    }
    d <- "mailto:a@evil.com%00.good.com"
    for (f in list(get_host, get_domain, get_tld, get_subdomain)) {
      expect_identical(acc(f, d), NA_character_, info = std)
    }
    expect_identical(acc(get_user, d), "a", info = std)
    l <- "mailto:a%00zz@b.com"
    expect_identical(acc(get_userinfo, l), NA_character_, info = std)
    expect_identical(acc(get_domain, l), "b.com", info = std)
  }
})

# --- RURL-mxsyzrho: RFC 6068 section 2 takes `local-part = dot-atom-text /
# quoted-string` from RFC 5322 but forbids <obs-local-part> and <NO-WS-CTL>
# (item 2) and whitespace (item 3). Of RFC 5322 section 4.1's obsolete
# quoted forms, obs-qtext (obs-NO-WS-CTL: %d1-8, %d11, %d12, %d14-31, %d127)
# and obs-qp ("\" followed by %d0, obs-NO-WS-CTL, LF or CR), only "\" before
# %d0 survives both bans. ---

# A quoted local part whose only fault, if any, is an obsolete form: the domain
# is a plain ascii one, and the SMTP wire has no projection of the local part
# (RFC 5321 section 4.1.2 has no obsolete forms).
dn_obs_local <- function(u, form) {
  row <- dn_row(u)
  expect_identical(row$mailto_local_part_form, form, label = u)
  expect_identical(row$mailto_domain_form, "ascii-dot-atom-text", label = u)
  expect_identical(row$smtp_mailbox_rhs_syntax_form, "domain", label = u)
  expect_true(row$public_suffix_known, label = u)
  expect_identical(row$smtp_domain_wire_form, "ascii-domain", label = u)
  expect_identical(row$smtp_envelope_wire_mode, "unavailable", label = u)
  expect_identical(row$smtp_envelope_address_requires_smtputf8, NA, label = u)
  expect_identical(row$smtp_local_part_length_ok, NA, label = u)
  expect_identical(row$smtp_direct_forward_path_fits, NA, label = u)
  for (std in c("rfc3986", "whatwg")) {
    r <- get_mailto_recipients(u, url_standard = std)
    expect_identical(r$parse_status, "ok", label = paste(u, std))
    expect_identical(r$mailto_local_part_form, form, label = paste(u, std))
  }
}

test_that("an obs-qp NUL in a quoted local part is a quoted-string", {
  # The decoded local part holds a NUL, which an R string cannot hold: the
  # user stays NA though the form is valid.
  for (u in c("mailto:%22%5C%00%22@b.com", "mailto:%22a%5C%00b%22@b.com",
              "mailto:%22%5C%00%5C%00%22@b.com")) {
    dn_obs_local(u, "quoted-string")
    expect_identical(dn_parts(u), list(user = NA_character_, host = "b.com"),
                     label = u)
  }
})

test_that("an obsolete form RFC 6068 forbids stays invalid", {
  # NO-WS-CTL (item 2): obs-qtext, or obs-qp escaping a control or DEL.
  # Whitespace (item 3): obs-qp escaping a CR or LF.
  for (u in c("mailto:%22%5C%0D%22@b.com", "mailto:%22%5C%0A%22@b.com",
              "mailto:%22%5C%01%22@b.com", "mailto:%22%5C%7F%22@b.com",
              "mailto:%22%01%22@b.com", "mailto:%22a%1Fb%22@b.com",
              "mailto:%22%7F%22@b.com")) {
    dn_obs_local(u, "invalid")
  }
})

test_that("no obsolete form admits these: they stay invalid", {
  # A bare NUL is not obs-qtext (obs-NO-WS-CTL starts at %d1), a bare CR or LF
  # is neither obs-qtext nor FWS, an obs form outside quotes is no
  # dot-atom-text, and a NUL after the closing quote is outside it.
  for (u in c("mailto:a%00zz@b.com", "mailto:%22%00%22@b.com",
              "mailto:%22a%00b%22@b.com", "mailto:%22%0D%22@b.com",
              "mailto:%22%0A%22@b.com", "mailto:a%01@b.com",
              "mailto:%5C%00@b.com", "mailto:%22%5C%00%22%00@b.com")) {
    row <- dn_row(u)
    dn_invalid_local(row)
    expect_identical(row$mailto_domain_form, "ascii-dot-atom-text", label = u)
  }
})

test_that("an obs-qp NUL leaves the domain half's NUL invalid", {
  row <- dn_row("mailto:%22%5C%00%22@b%00.com")
  dn_invalid_domain(row)
  expect_identical(row$mailto_local_part_form, "quoted-string")
})

test_that("only the obs-qp recipient changes form in a list", {
  r <- get_mailto_recipients("mailto:%22%5C%00%22@b.com,c@d.com",
                             smtp_wire = TRUE)
  expect_identical(r$mailto_local_part_form,
                   c("quoted-string", "dot-atom-text"))
  expect_identical(r$smtp_envelope_wire_mode, c("unavailable", "ascii"))
})
