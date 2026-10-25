# RURL-gndbrpam: the mailto first-recipient extraction behind get_user() /
# get_host(). RFC 6068 section 2 has every non-ASCII character of a mailto URI
# encoded as UTF-8 and then percent-encoded, so a recipient half whose decoded
# octets are not valid UTF-8 is no addr-spec part: that half is NA, and no
# undecodable octet leaves the parser (the RURL-otrpfhmq contract). Every case
# runs under both standards that accept a general scheme, alone and between
# good neighbors, under LC_CTYPE C and under a UTF-8 LC_CTYPE.

uu_standards <- c("rfc3986", "whatwg")

uu_neighbors <- c("mailto:one@good.example.com",
                  "mailto:two@good.example.com?subject=hi")

uu_locales <- function() {
  utf8 <- hi_utf8_locale()
  c("C", if (!is.na(utf8)) utf8)
}

uu_in_ctype <- function(locale, code) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  withr::with_locale(c(LC_CTYPE = locale), force(code))
}

# `url`'s solo user and host, after checking that they are the same in every
# locale and standard, and that a batch with the neighbors keeps them and the
# neighbors' own values.
uu_parts <- function(url) {
  vals <- list()
  for (loc in uu_locales()) {
    for (std in uu_standards) {
      vals[[paste(loc, std)]] <- uu_in_ctype(loc, {
        urls <- c(uu_neighbors[1L], url, uu_neighbors[2L])
        user <- get_user(urls, url_standard = std,
                         scheme_acceptance = "general")
        host <- get_host(urls, url_standard = std,
                         scheme_acceptance = "general")
        label <- paste(url, loc, std)
        expect_identical(user[c(1L, 3L)], c("one", "two"), label = label)
        expect_identical(host[c(1L, 3L)], rep("good.example.com", 2L),
                         label = label)
        solo_user <- get_user(url, url_standard = std,
                              scheme_acceptance = "general")
        expect_identical(user[2L], solo_user, label = label)
        expect_true(is.na(solo_user) || validUTF8(solo_user), label = label)
        list(user = solo_user,
             host = get_host(url, url_standard = std,
                             scheme_acceptance = "general"))
      })
    }
  }
  for (k in names(vals)[-1L]) {
    expect_identical(vals[[k]], vals[[1L]], label = paste(url, k))
  }
  vals[[1L]]
}

test_that("a plain recipient decomposes into user and host", {
  expect_identical(uu_parts("mailto:jane@b.com"),
                   list(user = "jane", host = "b.com"))
})

test_that("a percent-encoded UTF-8 local part decodes", {
  r <- uu_parts("mailto:a%C3%A9@b.com")
  expect_identical(r, list(user = "aé", host = "b.com"))
  expect_identical(Encoding(r$user), "UTF-8")
})

test_that("an empty local part is the empty user", {
  expect_identical(uu_parts("mailto:@b.com"), list(user = "", host = "b.com"))
})

test_that("only the first recipient decomposes", {
  expect_identical(uu_parts("mailto:x@y.com,a%E9@b.com"),
                   list(user = "x", host = "y.com"))
})

test_that("an undecodable domain is the NA host", {
  for (u in c("mailto:a@b%E9.com", "mailto:a@b%C3.com")) {
    expect_identical(uu_parts(u), list(user = "a", host = NA_character_),
                     label = u)
  }
})

# --- The defect: get_user() handed back the raw octets ("a\xe9") -------------

test_that("an undecodable local part is the NA user", {
  for (u in c("mailto:a%E9@b.com", "mailto:a%C3@b.com",
              "mailto:%22a%E9%22@b.com", "mailto:a%E9@b.com?subject=hi")) {
    expect_identical(uu_parts(u), list(user = NA_character_, host = "b.com"),
                     label = u)
  }
})

test_that("both halves undecodable is NA user and NA host", {
  expect_identical(uu_parts("mailto:a%E9@b%E9.com"),
                   list(user = NA_character_, host = NA_character_))
})

test_that("an undecodable second recipient leaves the first alone", {
  expect_identical(uu_parts("mailto:x@y.com,a%E9@b.com"),
                   list(user = "x", host = "y.com"))
})

test_that("the reported row's user no longer throws in nchar()", {
  for (std in uu_standards) {
    u <- get_user("mailto:a%E9@b.com", url_standard = std,
                  scheme_acceptance = "general")
    expect_no_error(nchar(u, type = "chars"))
    expect_identical(u, NA_character_, label = std)
  }
})
