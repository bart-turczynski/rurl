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

  # DEFECT WITNESS (ADR 0016), pre-fix. Under the no-selector default -- with
  # the argument omitted and passed as NULL alike -- and under rfc3986, one
  # undecodable octet after the authority aborts the whole call.
  test_that(paste("DEFECT: an undecodable octet throws the batch under", loc), {
    with_ctype(loc, ctypes[[loc]], {
      for (mark in c("unknown", "UTF-8")) {
        for (shape in names(bad_rows)) {
          bad <- marked(bad_rows[[shape]], mark)
          for (arm in c("omitted", "null", "rfc3986")) {
            rurl_clear_caches()
            expect_error(
              parse_arm(c(valid, bad), arms[[arm]]),
              "invalid UTF-8 byte sequence",
              info = paste(mark, shape, arm)
            )
          }
        }
      }
    })
  })
}
