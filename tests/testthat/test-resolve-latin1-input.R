# RURL-ummmdiwu: resolve_url() and a latin1-MARKED reference or base.
#
# A latin1 mark declares what each octet means: octet 0xNN is U+00NN
# (ISO 8859-1), so a latin1 "\xfc" and "ü" are the same URL
# (test-latin1-input.R, RURL-buemdwcq). resolve_url() reshapes the reference
# and the base as STRINGS in .resolve_one_raw() -- split, merge, recompose --
# before any parse seam reads the mark, so R's own string machinery decided
# what the octets meant, and it decides by LOCALE:
#
#   - in a UTF-8 locale R translates latin1 as CP1252, so 0x80-0x9F became
#     CP1252's characters (0x9F -> U+0178, bytes c5 b8), not U+0080-U+009F;
#   - in the C locale R translated the latin1 element to native ASCII, and a
#     non-ASCII octet became the literal escape text "<fc>".
#
# Every test below switches LC_CTYPE in-process (with_ctype(), the pattern of
# test-locale-invariance.R) and skips a locale the machine lacks. Inputs are
# built from raw octets; expected values are spelled with \u escapes. The parse
# cache keys the twins alike, so every resolution runs on a cold cache.

latin1 <- function(...) {
  out <- vapply(list(...), function(octets) rawToChar(as.raw(octets)),
                character(1))
  Encoding(out) <- "latin1"
  out
}

# Compare character values octet by octet (`identical()` on strings can match
# a latin1 "\xfc" against "ü"); NA stays NA.
expect_bytes <- function(actual, expected, label) {
  as_bytes <- function(v) {
    lapply(v, function(s) if (is.na(s)) NA else charToRaw(s))
  }
  expect_identical(as_bytes(actual), as_bytes(expected), label = label)
}

with_ctype <- function(locale, utf8, code) {
  active <- suppressWarnings(
    withr::with_locale(c(LC_CTYPE = locale), Sys.getlocale("LC_CTYPE"))
  )
  skip_if_not(identical(active, locale), paste(locale, "is not available"))
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  withr::with_locale(c(LC_CTYPE = locale), {
    # A switch that did not reach R's charset would test nothing.
    skip_if_not(
      identical(l10n_info()[["UTF-8"]], utf8),
      paste("LC_CTYPE", locale, "did not set the charset")
    )
    force(code)
  })
}
ctypes <- list(C = FALSE, `en_US.UTF-8` = TRUE)

cold_resolve <- function(ref, base, args) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  do.call(resolve_url, c(list(ref, base), args))
}

# Rows: an absolute reference carrying 0x9F (the CP1252 split); a relative
# path with 0xFC; a query and fragment with 0xFC; an ASCII reference against a
# base with 0xFC in host and path.
latin1_refs <- latin1(
  c(utf8ToInt("http://h.com/"), 0x9f),
  c(utf8ToInt("h"), 0xfc, utf8ToInt(".com/p")),
  c(utf8ToInt("?q="), 0xfc, 0x23, 0xfc),
  utf8ToInt("p")
)
utf8_refs <- c("http://h.com/\u009f", "hü.com/p", "?q=ü#ü", "p")
utf8_bases <- c(rep("http://b.com/a", 3L), "http://bü.com/ü/")
latin1_bases <- c(
  rep("http://b.com/a", 3L),
  latin1(c(utf8ToInt("http://b"), 0xfc, utf8ToInt(".com/"), 0xfc, 0x2f))
)

# The UTF-8 twin's value on every arm. `query_handling = "keep"` so the clean
# surface shows the query row; output = "serialized" needs a named standard.
clean_twin <- c(
  "http://h.com/\u009f", "http://b.com/hü.com/p",
  "http://b.com/a?q=%C3%BC", "http://bü.com/ü/p"
)
arms <- list(
  "NULL clean" = list(
    args = list(url_standard = NULL, query_handling = "keep"),
    twin = clean_twin
  ),
  "rfc3986 clean" = list(
    args = list(url_standard = "rfc3986", query_handling = "keep"),
    twin = clean_twin
  ),
  "rfc3986 serialized" = list(
    args = list(url_standard = "rfc3986", output = "serialized"),
    twin = c(
      "http://h.com/\u009f", "http://b.com/hü.com/p",
      "http://b.com/a?q=ü#ü", "http://bü.com/ü/p"
    )
  ),
  "whatwg clean" = list(
    args = list(url_standard = "whatwg", query_handling = "keep"),
    twin = clean_twin
  ),
  "whatwg serialized" = list(
    args = list(url_standard = "whatwg", output = "serialized"),
    twin = c(
      "http://h.com/%C2%9F", "http://b.com/h%C3%BC.com/p",
      "http://b.com/a?q=%C3%BC#%C3%BC", "http://xn--b-eha.com/%C3%BC/p"
    )
  )
)

# --- Pins: behavior the fix must not move ------------------------------------

test_that("a UTF-8 twin resolves to these values on every arm and locale", {
  for (loc in names(ctypes)) {
    with_ctype(loc, ctypes[[loc]], {
      for (arm in names(arms)) {
        expect_bytes(
          cold_resolve(utf8_refs, utf8_bases, arms[[arm]]$args),
          arms[[arm]]$twin, paste(loc, arm)
        )
      }
    })
  }
})

test_that("an ASCII reference and base resolve as before on every arm", {
  ascii <- list(
    "NULL clean" = "http://a/b/g?y=", "rfc3986 clean" = "http://a/b/g?y=",
    "rfc3986 serialized" = "http://a/b/g?y#s",
    "whatwg clean" = "http://a/b/g?y=", "whatwg serialized" = "http://a/b/g?y#s"
  )
  for (loc in names(ctypes)) {
    with_ctype(loc, ctypes[[loc]], {
      for (arm in names(arms)) {
        expect_identical(
          cold_resolve(c("../g?y#s", NA), "http://a/b/c/d;p?q",
                       arms[[arm]]$args),
          c(ascii[[arm]], NA_character_), label = paste(loc, arm)
        )
      }
    })
  }
})

test_that("whatwg in a UTF-8 locale already reads a latin1 row as its twin", {
  with_ctype("en_US.UTF-8", TRUE, {
    for (arm in c("whatwg clean", "whatwg serialized")) {
      expect_bytes(
        cold_resolve(latin1_refs, latin1_bases, arms[[arm]]$args),
        arms[[arm]]$twin, arm
      )
    }
  })
})

# --- The defect (pre-fix witness) --------------------------------------------
#
# DEFECT: these assert today's WRONG values. They are the witness ADR 0016
# asks for, and the fix flips them to the twin's value.

test_that("DEFECT: UTF-8 locale reads latin1 0x9F as CP1252 U+0178", {
  with_ctype("en_US.UTF-8", TRUE, {
    for (args in list(list(), list(url_standard = NULL))) {
      expect_bytes(
        cold_resolve(latin1_refs[1], latin1_bases[1], args),
        "http://h.com/Ÿ", "NULL"
      )
    }
    for (out in c("clean", "serialized")) {
      expect_bytes(
        cold_resolve(latin1_refs[1], latin1_bases[1],
                     list(url_standard = "rfc3986", output = out)),
        "http://h.com/Ÿ", paste("rfc3986", out)
      )
    }
  })
})

test_that("DEFECT: C locale merges the escape text of a latin1 octet", {
  with_ctype("C", FALSE, {
    for (args in list(list(), list(url_standard = NULL),
                      list(url_standard = "whatwg"))) {
      expect_bytes(
        cold_resolve(latin1_refs[2], latin1_bases[2], args),
        "http://b.com/h<fc>.com/p", paste(args, collapse = " ")
      )
    }
    expect_identical(
      cold_resolve(latin1_refs[2], latin1_bases[2],
                   list(url_standard = "rfc3986")),
      NA_character_
    )
  })
})
