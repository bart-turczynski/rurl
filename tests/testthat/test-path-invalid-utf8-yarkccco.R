# A web-route URL whose PATH holds an octet that is not valid UTF-8. The
# default `url_standard = "whatwg"` reads the octet as U+FFFD, as its parser
# reads a scalar-value string (UTF-8 decode with replacement), and the row
# parses; `rfc3986` rejects the row at its grammar gate (RFC 3986 sec 2: a URI
# is a sequence of characters).
#
# RURL-yarkccco (ruling RUL-029) percent-encoded such an octet under the
# frozen `url_standard = NULL` profile; ADR 0020 retired that profile, and
# RUL-032 supersedes RUL-029, so `NULL` now reads the octet as WHATWG does.
#
# Every bad input is built from raw octets with `rawToChar()`, so it carries
# the "unknown" mark real user input has; each is also run marked "UTF-8".

oct <- function(prefix, octets, suffix = "") {
  paste0(prefix, rawToChar(as.raw(octets)), suffix)
}
mark_utf8 <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}
marks <- list(unknown = identity, `UTF-8` = mark_utf8)

in_ctype <- function(locale, utf8, code) {
  active <- suppressWarnings(
    withr::with_locale(c(LC_CTYPE = locale), Sys.getlocale("LC_CTYPE"))
  )
  skip_if_not(identical(active, locale), paste(locale, "is not available"))
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  withr::with_locale(c(LC_CTYPE = locale), {
    skip_if_not(
      identical(l10n_info()[["UTF-8"]], utf8),
      paste("LC_CTYPE", locale, "did not set the charset")
    )
    force(code)
  })
}
ctypes <- list(C = FALSE, `en_US.UTF-8` = TRUE)

# Each bad path row and the row that spells its ill-formed octets as U+FFFD.
fffd <- "�"
path_twins <- list(
  end = list(oct("http://example.com/caf", 0xE9),
             paste0("http://example.com/caf", fffd)),
  query = list(oct("http://example.com/caf", 0xE9, "?q=1"),
               paste0("http://example.com/caf", fffd, "?q=1")),
  fragment = list(oct("http://example.com/caf", 0xE9, "#f"),
                  paste0("http://example.com/caf", fffd, "#f")),
  dots = list(oct("http://example.com/a/../caf", 0xE9, "/./b"),
              paste0("http://example.com/a/../caf", fffd, "/./b")),
  mixed = list(oct("http://example.com/caf", c(0xC3, 0xA9, 0xE9)),
               paste0("http://example.com/café", fffd)),
  schemeless = list(oct("example.com/caf", 0xE9),
                    paste0("example.com/caf", fffd)),
  ftp = list(oct("ftp://example.com/caf", 0xE9),
             paste0("ftp://example.com/caf", fffd))
)
drop_original <- function(df) df[setdiff(names(df), "original_url")]

for (loc in names(ctypes)) {
  test_that(paste("the default reads a path octet as U+FFFD under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        x <- marks[[mk]](c(oct("http://example.com/caf", 0xE9),
                           oct("http://example.com/caf", 0xE9, "?q=1"),
                           oct("file:///caf", 0xE9)))
        for (std in list(NULL, "whatwg")) {
          rurl_clear_caches()
          r <- safe_parse_urls(x, url_standard = std)
          expect_identical(r$parse_status, c("ok", "ok", "ok"), info = mk)
          expect_identical(r$path, rep(paste0("/caf", fffd), 3L), info = mk)
          expect_identical(r$query, c(NA, "q=1", NA), info = mk)
        }
        rurl_clear_caches()
        expect_identical(safe_parse_urls(x)$path, rep(paste0("/caf", fffd), 3L),
                         info = mk)
      }
    })
  })

  test_that(paste("a path octet row equals its U+FFFD twin under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      opts <- list(
        list(),
        list(index_page_handling = "strip"),
        list(trailing_slash_handling = "strip"),
        list(path_encoding = "encode"),
        list(path_encoding = "decode")
      )
      for (mk in names(marks)) {
        bad <- marks[[mk]](vapply(path_twins, `[[`, "", 1L))
        twin <- vapply(path_twins, `[[`, "", 2L)
        for (opt in opts) {
          what <- paste(mk, paste(names(opt), opt, collapse = " "))
          rurl_clear_caches()
          expect_no_warning(rb <- do.call(safe_parse_urls, c(list(bad), opt)))
          expect_no_warning(rt <- do.call(safe_parse_urls, c(list(twin), opt)))
          expect_identical(drop_original(rb), drop_original(rt), info = what)
          expect_identical(rb$original_url, unname(bad), info = what)
          expect_true(all(validUTF8(rb$clean_url[!is.na(rb$clean_url)])),
                      info = what)
        }
      }
    })
  })

  test_that(paste("get_path and get_clean_url follow under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      x <- c(oct("http://example.com/caf", 0xE9),
             oct("http://example.com/caf", 0xE9, "?q=1"))
      expect_identical(get_path(x), rep(paste0("/caf", fffd), 2L))
      expect_identical(get_parse_status(x), c("ok", "ok"))
      expect_identical(get_clean_url(x),
                       rep(paste0("http://example.com/caf", fffd), 2L))
    })
  })

  test_that(paste("an existing %E9 and a valid UTF-8 path are unchanged under",
                  loc), {
    in_ctype(loc, ctypes[[loc]], {
      r <- safe_parse_urls(c("http://example.com/caf%E9",
                             "http://example.com/caf%e9",
                             "http://example.com/café"))
      expect_identical(r$parse_status, c("ok", "ok", "ok"))
      expect_identical(r$path, c("/caf%E9", "/caf%e9", "/caf\u00e9"))
    })
  })

  test_that(paste("rfc3986 rejects every octet row under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        bad <- marks[[mk]](c(vapply(path_twins, `[[`, "", 1L),
                             oct("http://e.com/a?q=caf", 0xE9),
                             oct("http://e.com/a#caf", 0xE9),
                             oct("file:///caf", 0xE9)))
        rurl_clear_caches()
        r <- safe_parse_urls(bad, url_standard = "rfc3986")
        expect_identical(r$parse_status, rep("error", length(bad)), info = mk)
        expect_identical(r$path, rep(NA_character_, length(bad)), info = mk)
      }
    })
  })
}

test_that("every lone high octet in a path reads as U+FFFD, row ok", {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  octets <- 0x80:0xFF
  bad <- vapply(octets, function(o) oct("http://example.com/x", o), "")
  r <- safe_parse_urls(bad)
  expect_identical(r$parse_status, rep("ok", length(bad)))
  expect_identical(r$path, rep(paste0("/x", fffd), length(bad)))
})
