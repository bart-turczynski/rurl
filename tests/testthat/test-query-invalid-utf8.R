# RURL-kzbqvxpk: under `url_standard = "rfc3986"`, a row whose query holds an
# octet that is not valid UTF-8 stopped the whole vectorized call. The query
# engine split the query with a character-mode `strsplit()`, which warned
# "input string 1 is invalid UTF-8" and returned NA, and a later `if ()` threw
# "missing value where TRUE/FALSE needed", so every good row was lost. RFC
# 3986 section 2 admits no such octet, and the parse contract gives a bad row
# its own verdict: that row is the rfc3986 parse error, and its neighbors are
# what they are alone. The query split now runs in bytes (ADR 0019, ADR 0005's
# split contract).
#
# Every bad input is built from raw octets, so it carries the "unknown" mark
# real input has; each is also run marked "UTF-8".

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

good <- "http://www.example.com/?k=v&utm_source=x"
bad_rows <- list(
  # The ticket's row: one lone 0xE9 in a query value.
  value = oct("http://h.example/p?q=", 0xE9),
  # An undecodable query and an undecodable fragment (dispatcher's repro).
  frag = oct("http://x.com/?", 0x80, oct("#", 0xFF)),
  # An undecodable key, and a truncated sequence beside valid pairs.
  key = oct("http://h.example/?", 0xE9, "=1&r=2"),
  pair = oct("http://h.example/?a=1&b=", c(0xC3, 0x28), "&c=3")
)

option_sets <- list(
  list(query_handling = "keep"),
  list(query_handling = "filter"),
  list(query_handling = "allow"),
  list(query_handling = "drop"),
  list(query_handling = "filter", params_keep = "utm_*", params_drop = "k",
       sort_params = TRUE, empty_param_handling = "drop", decode_plus = TRUE),
  list(query_handling = "keep", params_drop = "q*",
       params_case_sensitive = TRUE),
  list(query_handling = "allow", params_keep = c("q", "k*")),
  list(query_handling = "keep", path_normalization = "dot_segments"),
  list(query_handling = "keep", index_page_handling = "strip")
)
label <- function(opt) paste(names(opt), opt, sep = "=", collapse = " ")

for (loc in names(ctypes)) {
  test_that(paste("an rfc3986 query octet row is its own parse error under",
                  loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        for (bn in names(bad_rows)) {
          bad <- marks[[mk]](bad_rows[[bn]])
          for (opt in option_sets) {
            what <- paste(mk, bn, label(opt))
            run <- function(x) {
              do.call(safe_parse_urls,
                      c(list(x, url_standard = "rfc3986"), opt))
            }
            rurl_clear_caches()
            both <- run(c(good, bad))
            rurl_clear_caches()
            alone_good <- run(good)
            rurl_clear_caches()
            alone_bad <- run(bad)
            expect_identical(both$parse_status, c("ok", "error"), info = what)
            expect_identical(both$clean_url[2L], NA_character_, info = what)
            expect_identical(both[1L, ], alone_good, info = what)
            rownames(alone_bad) <- 2L
            expect_identical(both[2L, ], alone_bad, info = what)
          }
        }
      }
    })
  })

  test_that(paste("get_clean_url() and get_query() keep the good row under",
                  loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        bad <- marks[[mk]](unlist(bad_rows, use.names = FALSE))
        for (opt in option_sets) {
          what <- paste(mk, label(opt))
          rurl_clear_caches()
          cu <- do.call(get_clean_url,
                        c(list(c(good, bad), url_standard = "rfc3986"), opt))
          rurl_clear_caches()
          solo <- do.call(get_clean_url,
                          c(list(good, url_standard = "rfc3986"), opt))
          expect_identical(cu, c(solo, rep(NA_character_, length(bad))),
                           info = what)
        }
        for (qh in c("keep", "filter", "allow")) {
          rurl_clear_caches()
          q <- get_query(c(good, bad), url_standard = "rfc3986",
                         query_handling = qh, format = "list")
          expect_identical(q[[1L]], get_query(good, url_standard = "rfc3986",
                                              query_handling = qh,
                                              format = "list")[[1L]],
                           info = paste(mk, qh))
          expect_length(q, 1L + length(bad))
        }
      }
    })
  })

  test_that(paste("the query engine splits an undecodable query in bytes",
                  "under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      for (mk in names(marks)) {
        q <- marks[[mk]](oct("a=1&q=", 0xE9, oct("&", 0xE9, "=x&&b")))
        expect_no_warning(p <- rurl:::._parse_query_pairs(q))
        expect_identical(p$key, mark_utf8(c("a", "q", oct("", 0xE9), "b")),
                         info = mk)
        expect_identical(p$value, mark_utf8(c("1", oct("", 0xE9), "x", "")),
                         info = mk)
        expect_identical(Encoding(p$key[3L]), "UTF-8", info = mk)
        expect_no_warning(
          out <- rurl:::._filter_query_params(q, query_handling = "keep")
        )
        expect_identical(out, "a=1&q=%E9&%E9=x&b=", info = mk)
        expect_no_warning(g <- rurl:::._parse_query_string(q, decode = FALSE))
        # identical(), not expect_named(): the marks are part of the pin.
        keys <- names(g)
        expect_identical(keys, mark_utf8(c("a", "q", oct("", 0xE9), "b")),
                         info = mk)
        expect_identical(unname(unlist(g)),
                         mark_utf8(c("1", oct("", 0xE9), "x", "")), info = mk)
      }
    })
  })
}
