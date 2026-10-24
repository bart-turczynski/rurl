# RURL-oahhlnvf (ADR 0019): the boolean and replacement `stringi` calls in
# R/url-key.R, R/parse.R, R/path-query.R and R/utils.R run in base R in byte
# mode. Each pattern is ASCII, so on valid UTF-8 every answer is the one
# stringi gave; byte mode also counts a leading U+FEFF (each U+FEFF row has a
# U+200B twin), gives a row that is not valid UTF-8 its own answer without a
# warning, and answers alike under LC_CTYPE C and a UTF-8 locale. A latin1
# element is read as its UTF-8 twin, as stringi read it.

u8 <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}
oct <- function(prefix, octets, suffix = "") {
  u8(paste0(prefix, rawToChar(as.raw(octets)), suffix))
}
latin1 <- function(x) {
  Encoding(x) <- "latin1"
  x
}

in_ctype <- function(locale, utf8, code) {
  active <- suppressWarnings(
    withr::with_locale(c(LC_CTYPE = locale), Sys.getlocale("LC_CTYPE"))
  )
  skip_if_not(identical(active, locale), paste(locale, "is not available"))
  withr::with_locale(c(LC_CTYPE = locale), {
    skip_if_not(
      identical(l10n_info()[["UTF-8"]], utf8),
      paste("LC_CTYPE", locale, "did not set the charset")
    )
    force(code)
  })
}
ctypes <- list(C = FALSE, `en_US.UTF-8` = TRUE)

for (loc in names(ctypes)) {
  test_that(paste(".has_explicit_authority() answers in bytes under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      f <- rurl:::.has_explicit_authority
      x <- c("mailto://e.example/", "mailto:x@y", " \u0001mailto://e/",
             "\ufeffmailto://e/", "\u200bmailto://e/", NA, "",
             oct("mailto://", 0xE9), oct("mailto:", 0xE9),
             oct("", 0xE9, "mailto://e/"))
      want <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
                TRUE, FALSE, FALSE)
      for (std in list(NULL, "rfc3986", "whatwg")) {
        expect_no_warning(got <- f(x, std))
        expect_identical(got, want, info = paste(std))
      }
      # whatwg removes tab/LF/CR first.
      expect_true(f("ma\tilto:/\n/e/", "whatwg"))
      expect_false(f("ma\tilto:/\n/e/", "rfc3986"))
      # latin1 reads as its UTF-8 twin.
      expect_true(f(latin1("mailto://\xe9/"), "whatwg"))
      expect_identical(f(character(0), "whatwg"), logical(0))
    })
  })

  test_that(paste(".url_key_scheme_presence() answers in bytes under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      f <- function(x, std) {
        rurl:::.url_key_scheme_presence(x, std, rep(FALSE, length(x)))
      }
      x <- c("//h/p", "\ufeff//h/p", "\u200b//h/p", "http://h/", "h.com:80/",
             "\u0001 //h/p", "1x://h/", NA, oct("//h/", 0xE9),
             oct("h", 0xE9, "ttp://h/"), latin1("//h\xe9/"))
      expect_no_warning(got <- f(x, "whatwg"))
      expect_identical(got, c(
        "scheme-relative", "inferred", "inferred", "explicit", "explicit",
        "scheme-relative", "inferred", "inferred", "scheme-relative",
        "inferred", "scheme-relative"
      ))
      # rfc3986 does not trim a leading C0-or-space run.
      expect_no_warning(got <- f(x, "rfc3986"))
      expect_identical(got[6L], "inferred")
      expect_identical(got[-6L], c(
        "scheme-relative", "inferred", "inferred", "explicit", "explicit",
        "inferred", "inferred", "scheme-relative", "inferred",
        "scheme-relative"
      ))
      # The host:port flag still overrides the lexical scheme test.
      expect_identical(
        rurl:::.url_key_scheme_presence("h.com:80/", "whatwg", TRUE),
        "inferred"
      )
    })
  })

  test_that(paste(".token_is_opaque() answers in bytes under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      x <- c("%", "%2", "%2z", "%41", "a%zz", "%%41", "plain", "", NA,
             "\ufeff%", "\u200b%41", oct("%", 0xE9), oct("", 0xE9, "%41"),
             u8("%41\u00e9"), "%4\n")
      expect_no_warning(got <- rurl:::.token_is_opaque(x))
      expect_identical(got, c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
                              FALSE, NA, TRUE, FALSE, TRUE, FALSE, FALSE,
                              TRUE))
    })
  })

  test_that(paste("the index strip resolves an exposed dot segment under",
                  loc), {
    in_ctype(loc, ctypes[[loc]], {
      expect_true(rurl:::.bytes_check_pattern(rurl:::.DOT_SEGMENT_TAIL_RE,
                                              fixed = FALSE))
      s <- function(p) rurl:::._strip_index_page(p)
      expect_identical(s("/a/../index.html"), "/")
      expect_identical(s("/a/b/../index.html"), "/a/")
      expect_identical(s("/a/./index.html"), "/a/")
      expect_identical(s("/../index.html"), "/")
      expect_identical(s("/a/b./index.html"), "/a/b.")
      expect_identical(s("/a/.../index.html"), "/a/...")
      expect_identical(s("/a/b/index.html"), "/a/b")
      # A line terminator stops the match before the dot test is reached.
      expect_identical(s("/a/..\n/index.html"), "/a/..\n/index.html")
      expect_identical(s(u8("/a/..\u2028/index.html")),
                       u8("/a/..\u2028/index.html"))
      # The mark is data before a dot segment, as its twin is.
      expect_identical(s(u8("/\ufeff../index.html")), u8("/\ufeff.."))
      expect_identical(s(u8("/\u200b../index.html")), u8("/\u200b.."))
    })
  })

  test_that(paste("the ASCII case fallback maps in bytes under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      fb <- rurl:::.ascii_case_fallback
      x <- c(a = u8("\ufeffABC"), b = u8("A\uffffB"), c = u8("\u200bXYZ"),
             d = u8("caf\u00c9"), e = "ABC", f = NA, g = "",
             h = oct("A", 0xFF, "B"), i = oct("", c(0xEF, 0xBB, 0xBF), "AZ"))
      expect_no_warning(lo <- fb(x, LETTERS, letters))
      expect_identical(lo, c(u8("\ufeffabc"), u8("a\uffffb"), u8("\u200bxyz"),
                             u8("caf\u00c9"), "abc", NA, "",
                             oct("a", 0xFF, "b"),
                             oct("", c(0xEF, 0xBB, 0xBF), "az")))
      expect_null(names(lo))
      expect_identical(Encoding(lo[8L]), "UTF-8")
      expect_identical(fb(lo, letters, LETTERS)[c(1L, 4L, 8L)],
                       c(u8("\ufeffABC"), u8("CAF\u00c9"),
                         oct("A", 0xFF, "B")))
      # A latin1 element is spelled in UTF-8, as stringi spelled it.
      l1 <- fb(latin1(c("Caf\xe9", "H\xc9")), LETTERS, letters)
      expect_identical(l1, c(u8("caf\u00e9"), u8("h\u00c9")))
      expect_identical(Encoding(l1), c("UTF-8", "UTF-8"))
      expect_identical(fb(character(0), LETTERS, letters), character(0))
    })
  })
}

test_that("the migrated files call no boolean or replacement stringi call", {
  # The case-insensitive arm of .param_matches() stays in stringi (ADR 0019,
  # "Semantic exceptions"); nothing else of this family may come back.
  root <- testthat::test_path("..", "..")
  files <- file.path(root, "R", c("url-key.R", "parse.R", "path-query.R",
                                  "utils.R", "zzz.R", "accessors.R",
                                  "resolve.R"))
  skip_if_not(all(file.exists(files)), "package sources not available")
  family <- "^stri_(detect|startswith|endswith|count|replace)_"
  found <- character(0)
  for (f in files) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    calls <- pd$text[pd$token == "SYMBOL_FUNCTION_CALL" &
                       grepl(family, pd$text)]
    found <- c(found, paste(basename(f), calls))
  }
  found <- found[!grepl(" $", found)]
  expect_identical(found, "path-query.R stri_detect_regex")
})
