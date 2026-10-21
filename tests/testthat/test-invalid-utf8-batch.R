# Invalid UTF-8 after the authority must not abort the batch (RURL-otrpfhmq).
#
# Under `url_standard = NULL` (the default) and `"rfc3986"`, one row holding an
# octet sequence that is not valid UTF-8 in its path, query or fragment made
# the whole `safe_parse_urls()` call throw `invalid UTF-8 byte sequence
# detected`, in every locale, so every good row in the same vector was lost.
# The throw came from `stringi::stri_length()` in `.extract_raw_path_vec()`,
# reached from `._parse_stage_a_vec()`. It is the same broken promise
# RURL-kmpnbvdl fixed for the rfc3986 AUTHORITY (test-locale-invariance.R,
# section 4b), at a different site.
#
# The trigger is a conjunction. `stri_length()` sat in the `no` branch of an
# `ifelse()`, which R evaluates over EVERY row as soon as one row has a path
# with no "?" or "#" after it. So a bad row whose path does end that way threw
# on its own, while a bad row followed by "?" or "#" -- including one whose
# bad octet is in the query or fragment -- threw only beside such a row, and
# only when neither was already in the parse cache.
#
# Every bad input is built from RAW OCTETS with `rawToChar()`, never from a
# source literal, so it carries the "unknown" mark real user input has; each
# is also run with a declared "UTF-8" mark. Each locale is switched
# in-process, and the parse cache is cleared on both sides: the cache key does
# not carry the locale, and whether a row threw depended on what was cached.

oct <- function(prefix, octets, suffix = "") {
  paste0(prefix, rawToChar(as.raw(octets)), suffix)
}
utf8_marked <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}

# Web-route rows holding an undecodable octet after the authority.
bad_rows <- c(
  end = oct("http://example.com/caf", 0xE9),
  query = oct("http://example.com/caf", 0xE9, "?q=1"),
  dots = oct("http://example.com/a/../caf", 0xE9),
  # A truncated 2-byte sequence whose trailing octet is "(" (a sub-delim).
  pair = oct("http://example.com/", 0xC3, "("),
  schemeless = oct("example.com/caf", 0xE9),
  ftp = oct("ftp://example.com/caf", 0xE9),
  in_query = oct("http://example.com/a?q=caf", 0xE9),
  in_fragment = oct("http://example.com/a#caf", 0xE9)
)
in_path <- c("end", "query", "dots", "pair", "schemeless", "ftp")
# Under NULL a path octet is percent-encoded and the row parses
# (RURL-yarkccco, RUL-029), with the path the "%XX" spelling gives.
null_path <- c(end = "/caf%E9", query = "/caf%E9", dots = "/a/../caf%E9",
               pair = "/%C3(", schemeless = "/caf%E9", ftp = "/caf%E9")

# Rows holding an undecodable octet IN the authority. The parser rejects them
# before the path is cut (whatwg reads the userinfo octet as U+FFFD instead),
# so they never reached the throwing site. Outside the fix's signature.
authority_rows <- c(
  userinfo = oct("http://us", 0xE9, "r:pw@example.com/a"),
  password = oct("http://user:p", 0xE9, "@example.com/a"),
  host = oct("http://ex", 0xE9, "mple.com/a")
)

# Valid controls. Row 1 has a path with no "?" or "#", which is what makes
# every other bad shape throw beside it.
valid <- c(
  "https://www.example.com/a",
  "http://example.com/caf\u00e9",
  "http://example.com/a/../b?q=caf%C3%A9#f",
  "ftp://user@example.com/x"
)

arms <- list(omitted = "omitted", null = NULL, rfc3986 = "rfc3986",
             whatwg = "whatwg")
parse_arm <- function(x, arm) {
  if (identical(arm, "omitted")) {
    safe_parse_urls(x)
  } else {
    safe_parse_urls(x, url_standard = arm)
  }
}

with_ctype <- function(locale, utf8, code) {
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
marked <- function(x, mark) if (mark == "UTF-8") utf8_marked(x) else x

# Today's values for the valid controls. These rows hold no invalid octet, so
# the fix must leave every one of them byte-identical.
expected_valid <- list(
  null = list(
    path = c("/a", "/caf\u00e9", "/a/../b", "/x"),
    clean_url = c("https://www.example.com/a", "http://example.com/caf\u00e9",
                  "http://example.com/a/../b", "ftp://example.com/x")
  ),
  rfc3986 = list(
    path = c("/a", "/caf\u00e9", "/b", "/x"),
    clean_url = c("https://www.example.com/a", "http://example.com/caf\u00e9",
                  "http://example.com/b", "ftp://example.com/x")
  )
)
expected_valid$omitted <- expected_valid$null
expected_valid$whatwg <- expected_valid$rfc3986

for (loc in names(ctypes)) {
  test_that(paste("the valid controls parse to today's values under", loc), {
    with_ctype(loc, ctypes[[loc]], {
      for (arm in names(arms)) {
        res <- parse_arm(valid, arms[[arm]])
        expect_identical(res$path, expected_valid[[arm]]$path, info = arm)
        expect_identical(
          res$clean_url, expected_valid[[arm]]$clean_url, info = arm
        )
        expect_identical(
          res$host,
          c("www.example.com", "example.com", "example.com", "example.com"),
          info = arm
        )
        expect_identical(res$query, c(NA, NA, "q=caf%C3%A9", NA), info = arm)
        expect_identical(
          res$parse_status, c("ok", "ok", "ok", "ok-ftp"), info = arm
        )
      }
    })
  })

  test_that(paste("whatwg already returns one row per input under", loc), {
    # WHATWG parses a scalar-value string: the undecodable octet is read as
    # U+FFFD. Already correct; the fix must not move it.
    with_ctype(loc, ctypes[[loc]], {
      ref <- safe_parse_urls(valid, url_standard = "whatwg")
      for (mark in c("unknown", "UTF-8")) {
        for (shape in names(bad_rows)) {
          rurl_clear_caches()
          res <- safe_parse_urls(
            c(valid, marked(bad_rows[[shape]], mark)), url_standard = "whatwg"
          )
          expect_identical(nrow(res), 5L, info = paste(mark, shape))
          expect_identical(res[1:4, ], ref, info = paste(mark, shape))
        }
        res <- safe_parse_urls(
          c(valid, marked(bad_rows[["end"]], mark)), url_standard = "whatwg"
        )
        expect_identical(res$parse_status[5L], "ok", info = mark)
        expect_identical(res$path[5L], "/caf\ufffd", info = mark)
        expect_identical(
          res$clean_url[5L], "http://example.com/caf\ufffd", info = mark
        )
      }
    })
  })

  test_that(paste("a lone query or fragment octet parses under", loc), {
    # Parsed ALONE, an invalid octet in the query or fragment never threw: the
    # no-selector default percent-encodes it, and rfc3986 rejects it through
    # its uniform grammar gate (RURL-kmpnbvdl). Those verdicts must survive.
    with_ctype(loc, ctypes[[loc]], {
      qf <- unname(bad_rows[c("in_query", "in_fragment")])
      for (arm in c("omitted", "null")) {
        rurl_clear_caches()
        res <- parse_arm(qf, arms[[arm]])
        expect_identical(res$parse_status, c("ok", "ok"), info = arm)
        expect_identical(res$path, c("/a", "/a"), info = arm)
        expect_identical(res$query, c("q=caf%E9", NA), info = arm)
        expect_identical(res$fragment, c(NA, "caf%E9"), info = arm)
      }
      rurl_clear_caches()
      res <- safe_parse_urls(qf, url_standard = "rfc3986")
      expect_identical(res$parse_status, c("error", "error"))
    })
  })

  test_that(paste("an authority octet never threw the batch under", loc), {
    with_ctype(loc, ctypes[[loc]], {
      expected <- list(
        omitted = c("error", "error", "error"),
        null = c("error", "error", "error"),
        rfc3986 = c("error", "error", "error"),
        whatwg = c("ok", "ok", "error")
      )
      for (arm in names(arms)) {
        ref <- parse_arm(valid, arms[[arm]])
        rurl_clear_caches()
        res <- parse_arm(c(valid, authority_rows), arms[[arm]])
        expect_identical(res[1:4, ], ref, info = arm)
        expect_identical(res$parse_status[5:7], expected[[arm]], info = arm)
      }
    })
  })

  # The ADR 0016 WITNESS, kept as the regression test. Before the fix every
  # call below threw `invalid UTF-8 byte sequence detected` -- under the
  # no-selector default (argument omitted and passed as NULL alike) and under
  # rfc3986, in both locales and for both marks.
  #
  # DECLARED SIGNATURE, per arm. What may move: rows holding an undecodable
  # octet after the authority, and the batch, which no longer throws. Nothing
  # else: the valid rows are asserted byte-identical to the batch parsed
  # without the bad row.
  #   NULL     A web-route row whose PATH holds the octet parses `ok` with
  #            the octet percent-encoded (`/caf%E9`), as its QUERY or
  #            FRAGMENT octet already was (RURL-yarkccco, RUL-029; RURL-otrpfhmq
  #            failed the row instead). The `?q=1` shape, which parsed ALONE
  #            was `ok` with the octet raw in `path`, now has `/caf%E9` too. A
  #            `file:` row fails wherever the octet is (see the file: test).
  #   rfc3986  Already correct: the uniform grammar gate rejects every one of
  #            these rows (RURL-kmpnbvdl). Only the throw moves.
  #   whatwg   Already correct and unmoved (pinned above): the octet is read
  #            as U+FFFD.
  test_that(paste("an undecodable octet fails its row, not the batch, under",
                  loc), {
    with_ctype(loc, ctypes[[loc]], {
      for (mark in c("unknown", "UTF-8")) {
        for (shape in names(bad_rows)) {
          bad <- marked(bad_rows[[shape]], mark)
          for (arm in c("omitted", "null", "rfc3986")) {
            what <- paste(mark, shape, arm)
            ref <- parse_arm(valid, arms[[arm]])
            rurl_clear_caches()
            expect_no_error(res <- parse_arm(c(valid, bad), arms[[arm]]))
            expect_identical(nrow(res), 5L, info = what)
            expect_identical(res[1:4, ], ref, info = what)
            expect_identical(res$original_url[5L], bad, info = what)
            if (arm == "rfc3986") {
              expect_identical(res$parse_status[5L], "error", info = what)
              expect_identical(res$path[5L], NA_character_, info = what)
              expect_identical(res$host[5L], NA_character_, info = what)
              expect_identical(res$clean_url[5L], NA_character_, info = what)
            } else if (shape %in% in_path) {
              expect_identical(
                res$parse_status[5L], if (shape == "ftp") "ok-ftp" else "ok",
                info = what
              )
              expect_identical(res$path[5L], null_path[[shape]], info = what)
              expect_identical(res$host[5L], "example.com", info = what)
            } else {
              expect_identical(res$parse_status[5L], "ok", info = what)
              expect_identical(res$path[5L], "/a", info = what)
            }
          }
        }
      }
    })
  })

  test_that(paste("omitting url_standard equals passing NULL under", loc), {
    with_ctype(loc, ctypes[[loc]], {
      for (mark in c("unknown", "UTF-8")) {
        batch <- c(valid, marked(unname(bad_rows), mark))
        rurl_clear_caches()
        omitted <- safe_parse_urls(batch)
        rurl_clear_caches()
        expect_identical(safe_parse_urls(batch, url_standard = NULL), omitted)
      }
    })
  })

  test_that(paste("the NULL encoded path holds under every path option, under",
                  loc), {
    # The options that run over the path in Stage B. Each bad row gives what
    # its "%E9" spelling gives (RURL-yarkccco).
    with_ctype(loc, ctypes[[loc]], {
      batch <- c(valid, unname(bad_rows[c("query", "dots")]))
      twins <- c(valid, "http://example.com/caf%E9?q=1",
                 "http://example.com/a/../caf%E9")
      for (opt in list(list(path_encoding = "encode"),
                       list(path_encoding = "decode"),
                       list(path_normalization = "dot_segments"),
                       list(index_page_handling = "strip"),
                       list(trailing_slash_handling = "strip"))) {
        rurl_clear_caches()
        expect_no_warning(
          res <- do.call(safe_parse_urls, c(list(batch), opt))
        )
        expect_no_warning(
          ref <- do.call(safe_parse_urls, c(list(twins), opt))
        )
        expect_identical(res$parse_status[5:6], c("ok", "ok"))
        expect_identical(res$path, ref$path)
        expect_identical(res$clean_url, ref$clean_url)
      }
    })
  })

  test_that(paste("an undecodable path octet survives encode and decode, under",
                  loc), {
    # `path_encoding` decodes before it presents. A decode that yields an
    # octet that is not valid UTF-8 (the raw row, and a valid `%E9` row, under
    # NULL and rfc3986 alike) gave the literal path "/NA" under "encode" (a
    # strsplit() warning) and wrote the raw octet into `path` and `clean_url`
    # under "decode". Both now keep that octet as its "%XX" triplet.
    with_ctype(loc, ctypes[[loc]], {
      rows <- list(
        list(x = bad_rows[["end"]], std = list(NULL)),
        list(x = "http://example.com/caf%E9", std = list(NULL, "rfc3986")),
        list(x = "http://example.com/caf%e9", std = list(NULL, "rfc3986"))
      )
      for (row in rows) {
        for (std in row$std) {
          for (pe in c("encode", "decode")) {
            what <- paste(pe, format(std))
            rurl_clear_caches()
            expect_no_warning(
              r <- safe_parse_urls(row$x, path_encoding = pe,
                                   url_standard = std)
            )
            expect_identical(r$parse_status, "ok", info = what)
            expect_identical(r$path, "/caf%E9", info = what)
            expect_identical(r$clean_url, "http://example.com/caf%E9",
                             info = what)
          }
        }
      }
      # Decode still decodes every triplet that is valid UTF-8 beside it;
      # encode re-encodes the whole path.
      rurl_clear_caches()
      expect_no_warning(
        d <- safe_parse_urls("http://example.com/a%20%C3%A9%E9",
                             path_encoding = "decode")
      )
      expect_identical(d$path, "/a é%E9")
      expect_no_warning(
        e <- safe_parse_urls("http://example.com/a%20%C3%A9%E9/b",
                             path_encoding = "encode")
      )
      expect_identical(e$path, "/a%20%C3%A9%E9/b")
    })
  })

  test_that(paste("an rfc3986 path-rootless octet fails its row under", loc), {
    # `http:example.com/...` is RFC 3986 path-rootless, split by
    # `.rfc3986_path_rootless_vec()` rather than the web parser. Its
    # `nchar()` threw `invalid multibyte string` out of the batch.
    with_ctype(loc, ctypes[[loc]], {
      rootless <- c(oct("http:example.com/caf", 0xE9),
                    oct("http:example.com/a?q=caf", 0xE9),
                    oct("http:example.com/a#caf", 0xE9))
      ref <- safe_parse_urls(valid, url_standard = "rfc3986")
      for (mark in c("unknown", "UTF-8")) {
        rurl_clear_caches()
        expect_no_error(
          res <- safe_parse_urls(
            c(valid, marked(rootless, mark)), url_standard = "rfc3986"
          )
        )
        expect_identical(res[1:4, ], ref, info = mark)
        expect_identical(res$parse_status[5:7], rep("error", 3L), info = mark)
      }
    })
  })

  test_that(paste("the accessors survive the bad row too, under", loc), {
    with_ctype(loc, ctypes[[loc]], {
      batch <- c(valid, bad_rows[["end"]])
      rurl_clear_caches()
      expect_identical(
        get_parse_status(batch), c("ok", "ok", "ok", "ok-ftp", "ok")
      )
      expect_identical(get_path(batch)[5L], "/caf%E9")
      expect_identical(get_clean_url(batch)[5L], "http://example.com/caf%E9")
      # The row parses, so its NULL syntax verdict is a pass (RURL-yarkccco).
      verdicts <- get_parse_verdicts(batch)
      expect_identical(verdicts$layer1_syntax_verdict, rep("pass", 5L))
      expect_identical(verdicts$layer2_policy_verdict[5L], "admitted")
      rurl_clear_caches()
      std <- "rfc3986"
      expect_identical(
        get_parse_status(batch, url_standard = std),
        c("ok", "ok", "ok", "ok-ftp", "error")
      )
      expect_identical(get_path(batch, url_standard = std)[5L], NA_character_)
      expect_identical(
        get_clean_url(batch, url_standard = std)[5L], NA_character_
      )
      verdicts <- get_parse_verdicts(batch, url_standard = std)
      expect_identical(verdicts$layer1_syntax_verdict[5L], "fail")
      rurl_clear_caches()
      expect_no_error(get_url_diagnostics(batch, url_standard = "rfc3986"))
      expect_identical(
        serialize_url(batch, standard = "rfc3986")[5L], NA_character_
      )
    })
  })
}

for (loc in names(ctypes)) {
  test_that(paste("a file: row with an undecodable octet fails alone, under",
                  loc), {
    # The RFC 8089 overlay threw `invalid multibyte string` for the batch
    # under NULL and rfc3986 (review of RURL-otrpfhmq); it is withheld like
    # the opaque route now. The withholding reads the whole row, so a file:
    # row fails wherever the octet is -- path, query or fragment -- unlike a
    # web-route row, whose query or fragment octet is percent-encoded.
    bad <- c(
      path = oct("file:///caf", 0xE9),
      query = oct("file:///a?q=caf", 0xE9),
      fragment = oct("file:///a#caf", 0xE9)
    )
    bad <- c(bad, setNames(utf8_marked(bad), paste0(names(bad), "_utf8")))
    ok_row <- "file:///a/b"
    with_ctype(loc, ctypes[[loc]], {
      for (std in list(NULL, "rfc3986")) {
        alone <- safe_parse_urls(ok_row, url_standard = std)
        for (nm in names(bad)) {
          lab <- paste(loc, if (is.null(std)) "NULL" else std, nm)
          r <- safe_parse_urls(c(ok_row, bad[[nm]]), url_standard = std)
          expect_identical(nrow(r), 2L, label = lab)
          expect_identical(r$parse_status[2], "error", label = lab)
          expect_identical(r[1, ], alone[1, ], label = lab,
                           ignore_attr = TRUE)
        }
      }
    })
  })
}

test_that(".extract_raw_path_vec() cuts an undecodable path without throwing", {
  # ... and percent-encodes its undecodable octet (RURL-yarkccco).
  bad <- utf8_marked(bad_rows[["end"]])
  out <- rurl:::.extract_raw_path_vec(
    c("http://example.com/a", "http://example.com/b?q", bad),
    c("/a", "/b", "/x")
  )
  expect_identical(out, c("/a", "/b", "/caf%E9"))
})

test_that(".rfc3986_path_rootless_vec() splits in bytes, totally", {
  bad <- utf8_marked(oct("http:example.com/caf", 0xE9, "?q#f"))
  out <- rurl:::.rfc3986_path_rootless_vec(
    c("http:example.com/a?q=1#f", "http:example.com/caf\u00e9?#", bad),
    "rfc3986"
  )
  expect_identical(out$is_path_rootless, c(TRUE, TRUE, TRUE))
  expect_identical(out$path[1:2], c("example.com/a", "example.com/caf\u00e9"))
  expect_identical(out$query, c("q=1", NA, "q"))
  expect_identical(out$fragment, c("f", NA, "f"))
  expect_identical(
    charToRaw(out$path[3L]),
    c(charToRaw("example.com/caf"), as.raw(0xE9))
  )
})
