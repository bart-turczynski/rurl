# The byte-mode string layer, R/strings-base.R (RURL-lbccaixb, ADR 0019).
#
# Each behavior is checked under LC_CTYPE=C and under a UTF-8 locale, so the
# layer's claim to be locale-invariant is tested rather than assumed. A
# leading U+FEFF row is paired with a U+200B twin, a zero-width code point
# `stringi` does not skip: equal answers show the mark is read as data.
#
# Non-ASCII literals are \u escapes, which parse to UTF-8-marked strings in
# any locale.

bom <- "\ufeff"
zwsp <- "\u200b"
e_acute <- "\u00e9"
bad <- "a\x80b"
Encoding(bad) <- "UTF-8"

ctypes <- list(C = FALSE, `en_US.UTF-8` = TRUE)

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

bytes_of <- function(x) lapply(x, charToRaw)

for (loc in names(ctypes)) {
  utf8 <- ctypes[[loc]]

  test_that(sprintf("detect reads a leading U+FEFF as data [%s]", loc), {
    in_ctype(loc, utf8, {
      x <- c(paste0(bom, "http://h"), paste0(zwsp, "http://h"), "http://h")
      expect_identical(.bytes_detect(x, "^http"), c(FALSE, FALSE, TRUE))
      expect_identical(.bytes_detect(x, "http", fixed = TRUE),
                       c(TRUE, TRUE, TRUE))
      expect_true(.bytes_detect(paste0(bom, "a"), paste0("^", bom)))
    })
  })

  test_that(sprintf("detect: multibyte prefix, NA, empty, no match [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      x <- "\u00e9/a"
      expect_false(.bytes_detect(x, "^/"))
      expect_true(.bytes_detect(x, "/a\\z"))
      expect_identical(.bytes_detect(c(NA, "a", "", "b"), "a"),
                       c(NA, TRUE, FALSE, FALSE))
      expect_true(.bytes_detect("", "^\\z"))
      expect_identical(.bytes_detect(character(0), "a"), logical(0))
      expect_identical(.bytes_detect(c("a.b", "ab"), ".", fixed = TRUE),
                       c(TRUE, FALSE))
    })
  })

  test_that(sprintf("detect gives an invalid-UTF-8 row its own answer [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      expect_no_warning(res <- .bytes_detect(c(bad, "ab", "zz"), "b"))
      expect_identical(res, c(TRUE, TRUE, FALSE))
      expect_identical(.bytes_detect(c(bad, "ab"), "[^\\x01-\\x7f]"),
                       c(TRUE, FALSE))
    })
  })

  test_that(sprintf("replace keeps a leading U+FEFF and declares UTF-8 [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      expect_identical(.bytes_replace(paste0(bom, "ab"), "^", "X"),
                       paste0("X", bom, "ab"))
      expect_identical(.bytes_replace(paste0(zwsp, "ab"), "^", "X"),
                       paste0("X", zwsp, "ab"))
      out <- .bytes_replace("\u00e9/p?q#f", "[?#].*\\z", "")
      expect_identical(out, "\u00e9/p")
      expect_identical(Encoding(out), "UTF-8")
      out <- .bytes_replace("\u00e9/p", "zzz", "")
      expect_identical(Encoding(out), "UTF-8")
      expect_identical(Encoding(.bytes_replace("abc", "b", "")), "unknown")
    })
  })

  test_that(sprintf("replace: groups, all, fixed, NA, invalid row [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      expect_identical(.bytes_replace("www.example.com", "^www\\.(.*)", "\\1"),
                       "example.com")
      expect_identical(
        .bytes_replace("a\\b\\c", "\\", "/", all = TRUE, fixed = TRUE),
        "a/b/c"
      )
      expect_identical(.bytes_replace(c(NA, "aa"), "a", "b", all = TRUE),
                       c(NA, "bb"))
      expect_identical(.bytes_replace("", "^\\z", "x"), "x")
      expect_no_warning(out <- .bytes_replace(c(bad, "ab"), "b", "B"))
      expect_identical(bytes_of(out), list(charToRaw("a\x80B"),
                                           charToRaw("aB")))
      expect_identical(Encoding(out[1L]), "UTF-8")
    })
  })

  test_that(sprintf("locate reports byte offsets [%s]", loc), {
    in_ctype(loc, utf8, {
      x <- c("\u00e9/a?b", paste0(bom, "x?y"), paste0(zwsp, "x?y"),
             "none", NA)
      expect_identical(
        .bytes_locate(x, "[?#]"),
        matrix(c(5L, 5L, 5L, NA, NA, 5L, 5L, 5L, NA, NA), ncol = 2L,
               dimnames = list(NULL, c("start", "end")))
      )
      expect_identical(.bytes_locate("abc", "(?=b)")[1L, ],
                       c(start = 2L, end = 1L))
      expect_identical(dim(.bytes_locate(character(0), "a")), c(0L, 2L))
      expect_no_warning(res <- .bytes_locate(c(bad, "zb"), "b"))
      expect_identical(res[, "start"], c(3L, 2L))
    })
  })

  test_that(sprintf("locate last finds the last match [%s]", loc), {
    in_ctype(loc, utf8, {
      res <- .bytes_locate(c("u@v@h", paste0(e_acute, "@h"), "h", NA, ""),
                           "@", fixed = TRUE, last = TRUE)
      expect_identical(res[, "start"], c(4L, 3L, NA, NA, NA))
      expect_identical(res[, "end"], c(4L, 3L, NA, NA, NA))
      expect_identical(.bytes_locate("aXbXc", "X[a-z]", last = TRUE)[1L, ],
                       c(start = 4L, end = 5L))
    })
  })

  test_that(sprintf("capture reads groups from regexpr attributes [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      x <- c(paste0("http://", e_acute, "/p"), paste0(bom, "http://h/p"),
             paste0(zwsp, "http://h/p"), "nope", NA)
      res <- .bytes_capture(x, "^([a-z]+)://([^/]*)")
      expect_identical(res[1L, ],
                       c(paste0("http://", e_acute), "http", e_acute))
      expect_identical(Encoding(res[1L, 3L]), "UTF-8")
      expect_true(all(is.na(res[2:5, ])))
      expect_identical(dim(res), c(5L, 3L))
    })
  })

  test_that(sprintf("capture: empty and unset groups, names, no groups [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      res <- .bytes_capture(c("x", "ab", ""), "^(a)?(b?)")
      expect_identical(res[1L, ], c("", NA, ""))
      expect_identical(res[2L, ], c("ab", "a", "b"))
      expect_identical(res[3L, ], c("", NA, ""))
      named <- .bytes_capture("ab", "(?<first>a)(?<second>b)")
      expect_identical(colnames(named), c("", "first", "second"))
      expect_identical(.bytes_capture(c("ab", "x"), "b"),
                       matrix(c("b", NA), ncol = 1L))
      expect_identical(dim(.bytes_capture(character(0), "(a)")), c(0L, 2L))
      expect_no_warning(res <- .bytes_capture(c(bad, "ab"), "(b)"))
      expect_identical(res[, 2L], c("b", "b"))
    })
  })

  test_that(sprintf("cut slices bytes and declares UTF-8 [%s]", loc), {
    in_ctype(loc, utf8, {
      x <- paste0(e_acute, "abc")
      expect_identical(.bytes_cut(x, 3L, .Machine$integer.max), "abc")
      out <- .bytes_cut(x, 1L, 2L)
      expect_identical(out, e_acute)
      expect_identical(Encoding(out), "UTF-8")
      expect_identical(.bytes_cut(paste0(bom, "ab"), 4L, 4L), "a")
      expect_identical(.bytes_cut(paste0(bom, "ab"), 1L, 3L), bom)
      expect_identical(.bytes_cut(paste0(zwsp, "ab"), 1L, 3L), zwsp)
      expect_identical(.bytes_cut(c("abc", "de", NA, ""), 2L,
                                  c(10L, 1L, 2L, 5L)),
                       c("bc", "", NA, ""))
      expect_identical(.bytes_cut("abc", NA, 2L), NA_character_)
      expect_identical(.bytes_cut(character(0), 1L, 2L), character(0))
      expect_no_warning(out <- .bytes_cut(c(bad, "xyz"), 2L, 3L))
      expect_identical(bytes_of(out), list(charToRaw("\x80b"),
                                           charToRaw("yz")))
      expect_identical(Encoding(out[1L]), "UTF-8")
    })
  })

  test_that(sprintf("split states its trailing-field rule [%s]", loc), {
    in_ctype(loc, utf8, {
      x <- c("a.b.", "", ".", "a..b", NA, "a..")
      expect_identical(
        .bytes_split_fixed(x, ".", keep_trailing_empty = TRUE),
        list(c("a", "b", ""), "", c("", ""), c("a", "", "b"), NA_character_,
             c("a", "", ""))
      )
      expect_identical(
        .bytes_split_fixed(x, ".", keep_trailing_empty = FALSE),
        list(c("a", "b"), character(0), "", c("a", "", "b"), NA_character_,
             c("a", ""))
      )
      expect_identical(.bytes_split_fixed(character(0), ".", TRUE), list())
      expect_identical(.bytes_split_fixed(NA_character_, ".", TRUE),
                       list(NA_character_))
    })
  })

  test_that(sprintf("split keeps a leading U+FEFF and multibyte text [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      expect_identical(.bytes_split_fixed(paste0(bom, ".a"), ".", TRUE),
                       list(c(bom, "a")))
      expect_identical(.bytes_split_fixed(paste0(zwsp, ".a"), ".", TRUE),
                       list(c(zwsp, "a")))
      out <- .bytes_split_fixed(paste0("a", e_acute, "b.", e_acute), ".",
                                TRUE)[[1L]]
      expect_identical(out, c(paste0("a", e_acute, "b"), e_acute))
      expect_identical(Encoding(out), c("UTF-8", "UTF-8"))
      expect_identical(.bytes_split_fixed(paste0("a", e_acute, "b"), e_acute,
                                          TRUE),
                       list(c("a", "b")))
      expect_no_warning(out <- .bytes_split_fixed(c(bad, "x.y"), ".", TRUE))
      expect_identical(bytes_of(out[[1L]]), list(charToRaw("a\x80b")))
      expect_identical(out[[2L]], c("x", "y"))
    })
  })

  test_that(sprintf("split matches stri_split_fixed() off the mark [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      # A separator that overlaps itself is where a "does it end in `sep`"
      # shortcut would go wrong: "aaa" on "aa" is "", "a".
      x <- c("aaa", "a", "aa", "aaaa", "", "baab", "aab", paste0(e_acute, "aa"))
      for (sep in c("aa", "a", "b")) {
        expect_identical(
          .bytes_split_fixed(x, sep, keep_trailing_empty = TRUE),
          stringi::stri_split_fixed(x, sep),
          label = sep
        )
      }
    })
  })

  test_that(sprintf("the pattern check refuses locale-table constructs [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      refused <- c("\\s", "a\\S", "\\w+", "\\W", "\\d", "\\D", "a\\b",
                   "\\B", "\\h", "\\v", "\\R", "\\p{L}", "\\P{ASCII}", "\\X",
                   "\\C", "[[:alpha:]]", "[^[:space:]]", "(*UTF)a",
                   "(*UCP)a", "(?i)\\xe9", "(?i)[\\x{c0}-\\x{c5}]",
                   "(?i:a)\\x80", "(?i)(a)\\1", "(?i)\\o{351}",
                   paste0("(?i)", e_acute), "[\\s\\S]",
                   # Word boundaries read the locale's tables too.
                   "x[[:>:]]", "[[:<:]]x",
                   # A back-reference under (?i) folds what its group took.
                   "(?i)(?P<n>.)(?P=n)", "(?i)(?P<n>.)(?P>n)",
                   "(?i)(?<n>.)\\k<n>", "(?i)(.)\\g1",
                   # The end anchor is \z; `$` also matches before a final \n.
                   "a$", "^a$", "(a|b$)", "[a]$")
      for (p in refused) {
        expect_error(.bytes_detect("a", p), "ADR 0019", label = p)
      }
      allowed <- c("\\\\s", "(?i)abc", "[\\x00-\\x20]+", "(?s:.)", "\\A\\z",
                   "%(?![0-9A-Fa-f]{2})", "(?-i)a", "\\x80", "(a)\\1",
                   "[\\t\\n\\r]", "a\\.b", "\\[", "(?i)%2e",
                   "(?P<n>.)(?P=n)", "\\$", "a\\$\\z", "[$]", "[^$]",
                   "[]$]", "[\\]$]", "\\\\[$]")
      for (p in allowed) {
        expect_no_error(.bytes_detect("a", p))
      }
    })
  })

  test_that(sprintf("the layer refuses bad arguments [%s]", loc), {
    in_ctype(loc, utf8, {
      latin1 <- "caf\xe9"
      Encoding(latin1) <- "latin1"
      expect_error(.bytes_detect(latin1, "a"), "latin1")
      raw_bytes <- "\xe9"
      Encoding(raw_bytes) <- "bytes"
      expect_error(.bytes_cut(raw_bytes, 1L, 1L), "bytes")
      expect_error(.bytes_detect(1L, "a"), "character vector")
      expect_error(.bytes_detect("a", NA_character_), "one non-empty string")
      expect_error(.bytes_detect("a", ""), "one non-empty string")
      expect_error(.bytes_detect("a", c("a", "b")), "one non-empty string")
      expect_error(.bytes_split_fixed("a.b", "", TRUE), "`sep`")
      expect_error(.bytes_split_fixed("a.b", "."), "keep_trailing_empty")
      expect_error(.bytes_split_fixed("a.b", ".", NA), "keep_trailing_empty")
      expect_error(.bytes_replace("ab", "(a)", "\\U\\1"), "locale")
      expect_identical(.bytes_replace("ab", "a", "\\U", fixed = TRUE), "\\Ub")
      expect_error(.bytes_replace("ab", "a", NA_character_), "one string")
      # The pattern, replacement and separator are bytes re-declared UTF-8
      # too, so a latin1 or bytes mark on them stops as on the subject.
      expect_error(.bytes_detect("café", latin1), "latin1")
      expect_error(.bytes_detect("a", raw_bytes, fixed = TRUE), "bytes")
      expect_error(.bytes_replace("a", "a", latin1), "latin1")
      expect_error(.bytes_split_fixed("a", raw_bytes, TRUE), "bytes")
      # `first` and `last` recycle from length 1 only, never truncate.
      expect_error(.bytes_cut(c("abc", "def", "ghi"), c(1L, 2L), 3L),
                   "`first`")
      expect_error(.bytes_cut("abc", 1L, c(2L, 3L)), "`last`")
      expect_identical(.bytes_cut(c("abc", "def"), c(1L, 2L), 2L),
                       c("ab", "e"))
    })
  })

  test_that(sprintf("replace refuses \\0, which base R writes as 0 [%s]",
                    loc), {
    in_ctype(loc, utf8, {
      # `sub()` has no whole-match reference: "[\\0]" would give "a[0]c".
      expect_error(.bytes_replace("abc", "b", "[\\0]"), "\\\\0")
      expect_error(.bytes_replace("abc", "b", "\\0", all = TRUE), "\\\\0")
      expect_identical(.bytes_replace("abc", "(b)", "[\\1]"), "a[b]c")
      # An escaped backslash before 0 is a literal backslash, then "0".
      expect_identical(.bytes_replace("abc", "b", "\\\\0"), "a\\0c")
      expect_identical(.bytes_replace("abc", "b", "\\0", fixed = TRUE),
                       "a\\0c")
    })
  })
}
