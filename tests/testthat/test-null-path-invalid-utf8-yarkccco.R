# Under `url_standard = NULL`, an octet that is not valid UTF-8 in a web-route
# PATH is percent-encoded, as the same octet in the query and fragment already
# is (RURL-yarkccco, ruling RUL-029). RURL-otrpfhmq made such a row fail
# (`parse_status = "error"`, every component NA) to stop it aborting the
# batch; the owner ruled instead that the path keeps the octet as its "%XX"
# triplet: RFC 3986 sec 2.1 allows `%E9` in a path and forbids only the raw
# octet. `file:` rows and the `rfc3986` and `whatwg` profiles do not move.
#
# Not byte-for-byte CRAN 3.1.0: 3.1.0 read the path octet as U+FFFD.
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

# Each bad path row and the row that spells its octets as "%XX" triplets.
path_twins <- list(
  end = list(oct("http://example.com/caf", 0xE9), "http://example.com/caf%E9"),
  query = list(oct("http://example.com/caf", 0xE9, "?q=1"),
               "http://example.com/caf%E9?q=1"),
  fragment = list(oct("http://example.com/caf", 0xE9, "#f"),
                  "http://example.com/caf%E9#f"),
  dots = list(oct("http://example.com/a/../caf", 0xE9, "/./b"),
              "http://example.com/a/../caf%E9/./b"),
  # A truncated 2-byte sequence: the lead octet alone is escaped.
  pair = list(oct("http://example.com/", 0xC3, "("),
              "http://example.com/%C3("),
  # A valid "é" beside an invalid octet: only the invalid one is escaped.
  mixed = list(oct("http://example.com/caf", c(0xC3, 0xA9, 0xE9)),
               "http://example.com/café%E9"),
  schemeless = list(oct("example.com/caf", 0xE9), "example.com/caf%E9"),
  ftp = list(oct("ftp://example.com/caf", 0xE9), "ftp://example.com/caf%E9")
)

null_arm <- function(x, ...) safe_parse_urls(x, url_standard = NULL, ...)
drop_original <- function(df) df[setdiff(names(df), "original_url")]

for (loc in names(ctypes)) {
  test_that(paste("a NULL path octet is percent-encoded, row ok, under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        rurl_clear_caches()
        r <- null_arm(marks[[mk]](c(
          oct("http://example.com/caf", 0xE9),
          oct("http://example.com/caf", 0xE9, "?q=1")
        )))
        expect_identical(r$parse_status, c("ok", "ok"), info = mk)
        expect_identical(r$path, c("/caf%E9", "/caf%E9"), info = mk)
        expect_identical(r$host, c("example.com", "example.com"), info = mk)
        expect_identical(r$query, c(NA, "q=1"), info = mk)
        expect_identical(
          r$clean_url,
          c("http://example.com/caf%E9", "http://example.com/caf%E9"),
          info = mk
        )
        # Omitting url_standard is the same NULL profile.
        rurl_clear_caches()
        expect_identical(
          safe_parse_urls(marks[[mk]](oct("http://example.com/caf", 0xE9)))$path,
          "/caf%E9", info = mk
        )
      }
    })
  })

  test_that(paste("a NULL path octet row equals its %XX twin under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      opts <- list(
        list(),
        list(path_normalization = "dot_segments"),
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
          expect_no_error(
            rb <- suppressWarnings(do.call(null_arm, c(list(bad), opt)))
          )
          rt <- suppressWarnings(do.call(null_arm, c(list(twin), opt)))
          expect_identical(drop_original(rb), drop_original(rt), info = what)
          expect_identical(rb$original_url, unname(bad), info = what)
        }
      }
    })
  })

  test_that(paste("get_path and get_clean_url follow under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      x <- c(oct("http://example.com/caf", 0xE9),
             oct("http://example.com/caf", 0xE9, "?q=1"))
      expect_identical(get_path(x), c("/caf%E9", "/caf%E9"))
      expect_identical(get_parse_status(x), c("ok", "ok"))
      expect_identical(
        get_clean_url(x),
        c("http://example.com/caf%E9", "http://example.com/caf%E9")
      )
    })
  })

  # --- Pins of what does not move -----------------------------------------

  test_that(paste("a NULL query or fragment octet is unchanged under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        rurl_clear_caches()
        r <- null_arm(marks[[mk]](c(
          oct("http://e.com/a?q=caf", 0xE9),
          oct("http://e.com/a#caf", 0xE9)
        )))
        expect_identical(r$parse_status, c("ok", "ok"), info = mk)
        expect_identical(r$path, c("/a", "/a"), info = mk)
        expect_identical(r$query, c("q=caf%E9", NA), info = mk)
        expect_identical(r$fragment, c(NA, "caf%E9"), info = mk)
        expect_identical(r$clean_url, c("http://e.com/a", "http://e.com/a"),
                         info = mk)
      }
    })
  })

  test_that(paste("an existing %E9 and a valid UTF-8 path are unchanged under",
                  loc), {
    in_ctype(loc, ctypes[[loc]], {
      r <- null_arm(c("http://example.com/caf%E9", "http://example.com/caf%e9",
                      "http://example.com/café"))
      expect_identical(r$parse_status, c("ok", "ok", "ok"))
      expect_identical(r$path, c("/caf%E9", "/caf%E9", "/café"))
      expect_identical(
        r$clean_url,
        c("http://example.com/caf%E9", "http://example.com/caf%E9",
          "http://example.com/café")
      )
    })
  })

  test_that(paste("a file: octet row still fails under NULL, under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        rurl_clear_caches()
        r <- null_arm(marks[[mk]](c(oct("file:///caf", 0xE9),
                                    oct("file:///a?q=caf", 0xE9),
                                    oct("file:///a#caf", 0xE9))))
        expect_identical(r$parse_status, rep("error", 3L), info = mk)
        expect_identical(r$path, rep(NA_character_, 3L), info = mk)
      }
    })
  })

  test_that(paste("rfc3986 still rejects every octet row under", loc), {
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

  test_that(paste("whatwg still reads the octet as U+FFFD under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        rurl_clear_caches()
        r <- safe_parse_urls(
          marks[[mk]](c(oct("http://example.com/caf", 0xE9),
                        oct("http://example.com/caf", 0xE9, "?q=1"),
                        oct("file:///caf", 0xE9))),
          url_standard = "whatwg"
        )
        expect_identical(r$parse_status, c("ok", "ok", "ok"), info = mk)
        expect_identical(r$path, rep("/caf�", 3L), info = mk)
        expect_identical(r$query, c(NA, "q=1", NA), info = mk)
      }
    })
  })
}

test_that("every lone high octet in a NULL path is escaped, row ok", {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  octets <- 0x80:0xFF
  bad <- vapply(octets, function(o) oct("http://example.com/x", o), "")
  r <- null_arm(bad)
  expect_identical(r$parse_status, rep("ok", length(bad)))
  expect_identical(r$path, sprintf("/x%%%02X", octets))
})
