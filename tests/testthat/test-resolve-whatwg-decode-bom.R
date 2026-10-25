# RURL-tfttelwo: resolve_url() and an undecodable operand led by U+FEFF.
#
# Under whatwg, resolve_url() reads an undecodable reference or base as its
# parse seam does, with U+FFFD for each invalid sequence (RURL-cwzickqy). The
# decode is stringi::stri_enc_toutf8(validate = TRUE), which reads a U+FEFF
# that starts its input as a byte-order mark and drops it (ADR 0019 RC2). In
# a URL the code point is data: the URL Standard's parser does not strip it
# (only C0 controls and space), and it percent-encodes it in a path. So a
# row led by the mark must resolve as its U+200B twin does, with the mark
# swapped back.
#
# Every row runs under the C and a UTF-8 LC_CTYPE (the pattern of
# test-resolve-latin1-input.R) on a cold cache. Inputs are built from raw
# octets; expected values are spelled with \u escapes.

octets <- function(...) {
  out <- rawToChar(as.raw(c(...)))
  Encoding(out) <- "UTF-8"
  out
}
bom <- c(0xef, 0xbb, 0xbf)
zwsp <- c(0xe2, 0x80, 0x8b)

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
    skip_if_not(
      identical(l10n_info()[["UTF-8"]], utf8),
      paste("LC_CTYPE", locale, "did not set the charset")
    )
    force(code)
  })
}
ctypes <- list(C = FALSE, `en_US.UTF-8` = TRUE)

cold_resolve <- function(ref, base, url_standard) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  resolve_url(ref, base, url_standard = url_standard)
}

# Swap U+200B for U+FEFF, octet for octet; NA stays NA.
swap_mark <- function(x) {
  gsub("​", "﻿", x, fixed = TRUE, useBytes = TRUE)
}

base <- "http://h/d/"

for (ct in names(ctypes)) {
  test_that(paste("a decodable operand led by either mark resolves alike,", ct), {
    with_ctype(ct, ctypes[[ct]], {
      for (std in list("whatwg", "rfc3986", NULL)) {
        label <- paste(ct, if (is.null(std)) "NULL" else std)
        expect_bytes(
          cold_resolve(c("﻿a", "​a"), base, std),
          c("http://h/d/﻿a", "http://h/d/​a"),
          label = paste(label, "reference")
        )
        # A base led by either mark has no scheme, so it is no base.
        expect_bytes(
          cold_resolve("a", c("﻿http://h/d/", "​http://h/d/"), std),
          c(NA_character_, NA_character_),
          label = paste(label, "base")
        )
      }
    })
  })

  test_that(paste("an undecodable operand not led by U+FEFF keeps its reading,", ct), {
    with_ctype(ct, ctypes[[ct]], {
      zw_ref <- octets(zwsp, 0x61, 0xff)
      mid_ref <- octets(0x61, bom, 0xff)
      zw_abs <- octets(zwsp, utf8ToInt("http://x/"), 0xff)
      zw_base <- octets(zwsp, utf8ToInt(base), 0xff)
      mid_base <- octets(utf8ToInt("http://h/"), bom, utf8ToInt("d/"), 0xff)

      expect_bytes(
        cold_resolve(c(zw_ref, mid_ref, zw_abs), base, "whatwg"),
        c("http://h/d/​a�", "http://h/d/a﻿�",
          "http://h/d/​http://x/�"),
        label = paste(ct, "whatwg references")
      )
      expect_bytes(
        cold_resolve("a", c(zw_base, mid_base), "whatwg"),
        c(NA_character_, "http://h/﻿d/a"),
        label = paste(ct, "whatwg bases")
      )
      expect_bytes(
        cold_resolve(c(zw_ref, mid_ref), base, "rfc3986"),
        c(NA_character_, NA_character_),
        label = paste(ct, "rfc3986 references")
      )
      expect_bytes(
        cold_resolve("a", c(zw_base, mid_base), "rfc3986"),
        c(NA_character_, NA_character_),
        label = paste(ct, "rfc3986 bases")
      )
      expect_bytes(
        cold_resolve(c(zw_ref, mid_ref), base, NULL),
        c("http://h/d/​a%FF", "http://h/d/a﻿%FF"),
        label = paste(ct, "NULL references")
      )
      expect_bytes(
        cold_resolve("a", c(zw_base, mid_base), NULL),
        c(NA_character_, "http://h/﻿d/a"),
        label = paste(ct, "NULL bases")
      )
    })
  })

  test_that(paste("rfc3986 and NULL keep U+FEFF on an undecodable operand,", ct), {
    with_ctype(ct, ctypes[[ct]], {
      bom_ref <- octets(bom, 0x61, 0xff)
      bom_base <- octets(bom, utf8ToInt(base), 0xff)
      expect_bytes(
        cold_resolve(bom_ref, base, "rfc3986"), NA_character_,
        label = paste(ct, "rfc3986 reference")
      )
      expect_bytes(
        cold_resolve("a", bom_base, "rfc3986"), NA_character_,
        label = paste(ct, "rfc3986 base")
      )
      expect_bytes(
        cold_resolve(bom_ref, base, NULL), "http://h/d/﻿a%FF",
        label = paste(ct, "NULL reference")
      )
      expect_bytes(
        cold_resolve("a", bom_base, NULL), NA_character_,
        label = paste(ct, "NULL base")
      )
    })
  })
}
