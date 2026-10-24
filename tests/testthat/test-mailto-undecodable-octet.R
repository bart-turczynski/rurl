# RURL-lotjvpnt: a percent-decoded octet that is not valid UTF-8 in a mailto
# recipient. RFC 6068 section 2 has every non-ASCII character of a mailto URI
# encoded as UTF-8 and then percent-encoded octet by octet, so a decoded
# recipient that is not valid UTF-8 is not an RFC 6068 addr-spec: it is that
# recipient's own `invalid` verdict, never a throw that loses every other row
# of the vectorized call.
#
# Each case runs alone and between two good neighbors, under LC_CTYPE C and
# under a UTF-8 LC_CTYPE: every row equals its solo value, and both locales
# agree.

mu_neighbors <- c("mailto:one@good.example.com",
                  "mailto:two@good.example.com?subject=hi")

mu_facts <- function(urls, smtp_wire = FALSE) {
  get_mailto_recipients(urls, scheme_acceptance = "general",
                        smtp_wire = smtp_wire)
}

mu_in_ctype <- function(locale, code) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  withr::with_locale(c(LC_CTYPE = locale), force(code))
}

mu_locales <- function() {
  utf8 <- hi_utf8_locale()
  c("C", if (!is.na(utf8)) utf8)
}

# Cut a batch's rows into the segments its inputs contributed, given each
# input's solo row count; row names reset.
mu_segments <- function(out, n) {
  ends <- cumsum(n)
  lapply(seq_along(n), function(k) {
    r <- out[seq_len(n[k]) + ends[k] - n[k], , drop = FALSE]
    rownames(r) <- NULL
    r
  })
}

# `url`'s solo result in every locale, after checking that it is the same in
# each, that a batch with the neighbors keeps it and keeps the neighbors' own
# solo rows.
mu_isolated <- function(url, smtp_wire = FALSE) {
  vals <- lapply(mu_locales(), function(loc) {
    mu_in_ctype(loc, {
      urls <- c(mu_neighbors[1L], url, mu_neighbors[2L])
      solos <- lapply(urls, mu_facts, smtp_wire = smtp_wire)
      batch <- mu_facts(urls, smtp_wire)
      expect_identical(nrow(batch), sum(vapply(solos, nrow, 1L)),
                       label = paste(url, loc, "batch rows"))
      seg <- mu_segments(batch, vapply(solos, nrow, 1L))
      for (k in seq_along(urls)) {
        expect_identical(seg[[k]], solos[[k]],
                         label = paste(url, loc, "batch row", k))
      }
      solos[[2L]]
    })
  })
  for (v in vals[-1L]) {
    expect_identical(v, vals[[1L]], label = paste(url, "C vs UTF-8"))
  }
  vals[[1L]]
}

form_cols <- c("mailto_local_part_form", "mailto_domain_form",
               "smtp_mailbox_rhs_syntax_form")

test_that("the good neighbors keep their facts", {
  r <- mu_isolated(mu_neighbors[1L], smtp_wire = TRUE)
  expect_identical(unlist(r[1L, form_cols], use.names = FALSE),
                   c("dot-atom-text", "ascii-dot-atom-text", "domain"))
  expect_true(r$public_suffix_known)
  expect_identical(r$smtp_envelope_wire_mode, "ascii")
})

test_that("a well-formed UTF-8 local part stays invalid RFC 6068 syntax", {
  # %C3%A9 is U+00E9: valid UTF-8, outside atext.
  r <- mu_isolated("mailto:z%C3%A9@b.com", smtp_wire = TRUE)
  expect_identical(unlist(r[1L, form_cols], use.names = FALSE),
                   c("invalid", "ascii-dot-atom-text", "domain"))
  expect_identical(r$smtp_envelope_wire_mode, "smtputf8")
})

test_that("an undecodable octet in a dot-atom domain is the row's invalid", {
  r <- mu_isolated("mailto:a@b%E9.com", smtp_wire = TRUE)
  expect_identical(unlist(r[1L, form_cols], use.names = FALSE),
                   c("dot-atom-text", "invalid", "invalid"))
  expect_identical(r$public_suffix_known, NA)
  expect_identical(r$smtp_domain_wire_form, "unavailable")
  expect_identical(r$smtp_envelope_wire_mode, "unavailable")
  expect_identical(r$smtp_direct_forward_path_fits, NA)
})

test_that("a truncated UTF-8 sequence in the domain is invalid too", {
  r <- mu_isolated("mailto:a@b%C3.com")
  expect_identical(unlist(r[1L, form_cols], use.names = FALSE),
                   c("dot-atom-text", "invalid", "invalid"))
})

test_that("one undecodable recipient leaves its list neighbor alone", {
  r <- mu_isolated("mailto:x@y.com,a@b%E9.com")
  expect_identical(r$recipient_index, 1:2)
  expect_identical(r$mailto_domain_form, c("ascii-dot-atom-text", "invalid"))
  expect_identical(r$public_suffix_known, c(TRUE, NA))
})

# --- The defect: these rows threw "invalid multibyte string" -----------------

# The address has no wire projection; the intact half keeps its own fact
# (the domain's wire form, the local part's length), as for any other invalid
# half.
no_wire <- function(r, bad = c("local", "domain")) {
  bad <- match.arg(bad)
  expect_identical(r$smtp_domain_wire_form,
                   if (bad == "local") "ascii-domain" else "unavailable")
  expect_identical(r$smtp_local_part_length_ok,
                   if (bad == "local") NA else TRUE)
  expect_identical(r$smtp_envelope_wire_mode, "unavailable")
  expect_identical(r$smtp_envelope_address_requires_smtputf8, NA)
  expect_identical(r$smtp_direct_forward_path_fits, NA)
}

test_that("an undecodable octet in the local part is the row's invalid", {
  for (w in c(FALSE, TRUE)) {
    r <- mu_isolated("mailto:a%E9@b.com", smtp_wire = w)
    expect_identical(nrow(r), 1L)
    expect_identical(unlist(r[1L, form_cols], use.names = FALSE),
                     c("invalid", "ascii-dot-atom-text", "domain"))
    expect_true(r$public_suffix_known)
    if (w) no_wire(r, "local")
  }
})

test_that("a truncated UTF-8 sequence in the local part is invalid", {
  r <- mu_isolated("mailto:a%C3@b.com", smtp_wire = TRUE)
  expect_identical(r$mailto_local_part_form, "invalid")
  no_wire(r, "local")
})

test_that("an undecodable octet in a quoted local part is invalid", {
  r <- mu_isolated("mailto:%22a%E9%22@b.com", smtp_wire = TRUE)
  expect_identical(r$mailto_local_part_form, "invalid")
  expect_identical(r$mailto_domain_form, "ascii-dot-atom-text")
  no_wire(r, "local")
})

test_that("an undecodable octet in a bracketed domain is invalid", {
  for (u in c("mailto:a@[%E9]", "mailto:a@[1.2.3.%E9]",
              "mailto:a@[IPv6:%E9:1]")) {
    r <- mu_isolated(u, smtp_wire = TRUE)
    expect_identical(unlist(r[1L, form_cols], use.names = FALSE),
                     c("dot-atom-text", "invalid", "invalid"), label = u)
    expect_identical(r$public_suffix_known, NA, label = u)
    no_wire(r, "domain")
  }
})

test_that("the undecodable local part leaves its list neighbors alone", {
  r <- mu_isolated("mailto:x@y.com,a%E9@b.com,z@w.com")
  expect_identical(r$recipient_index, 1:3)
  expect_identical(r$mailto_local_part_form,
                   c("dot-atom-text", "invalid", "dot-atom-text"))
})

test_that("the ticket batch returns every row", {
  urls <- c(mu_neighbors[1L], "mailto:a%E9@b.com", "mailto:a@b%E9.com",
            mu_neighbors[2L])
  for (loc in mu_locales()) {
    r <- mu_in_ctype(loc, mu_facts(urls, smtp_wire = TRUE))
    expect_identical(r$url, urls, label = loc)
    expect_identical(r$mailto_local_part_form,
                     c("dot-atom-text", "invalid", "dot-atom-text",
                       "dot-atom-text"), label = loc)
    expect_identical(r$mailto_domain_form,
                     c("ascii-dot-atom-text", "ascii-dot-atom-text",
                       "invalid", "ascii-dot-atom-text"), label = loc)
  }
})

test_that("hostile scale: a long undecodable run and many bad recipients", {
  big <- paste0("mailto:", strrep("%E9", 1e4), "@b.com")
  many <- paste0("mailto:", paste(rep("a%E9@b.com", 500L), collapse = ","))
  urls <- c(big, many, mu_neighbors[1L])
  for (loc in mu_locales()) {
    r <- mu_in_ctype(loc, mu_facts(urls, smtp_wire = TRUE))
    expect_identical(nrow(r), 502L, label = loc)
    expect_identical(r$mailto_local_part_form,
                     c(rep("invalid", 501L), "dot-atom-text"), label = loc)
  }
})
