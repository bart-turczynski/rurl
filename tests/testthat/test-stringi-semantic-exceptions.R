# RURL-yyeohqah: the semantic exceptions of ADR 0019, one decision each.
#
# Each section pins the behavior the decision must keep. The suite also runs
# under LC_ALL=C, and every non-ASCII value here is spelled with \u escapes or
# built from raw octets, so the same assertions show both locales agree. Each
# U+FEFF row has a U+200B twin (ADR 0005): the twin is a zero-width code point
# that stringi does not skip.

bom <- "﻿"
zwsp <- "​"

octets <- function(..., enc = "unknown") {
  out <- rawToChar(as.raw(c(...)))
  Encoding(out) <- enc
  out
}

# Each string's octets (NA stays NA), so a comparison does not depend on how
# the session reads an invalid or latin1 string.
as_bytes <- function(x) {
  lapply(x, function(s) if (is.na(s)) NA else charToRaw(s))
}

is_ascii <- function(x) {
  !grepl("[\\x80-\\xff]", x, perl = TRUE, useBytes = TRUE)
}

# --- 1. The cache key (R/zzz.R) -----------------------------------------------
#
# A cache key is a variable name, so `.cache_key_ascii()` must be total onto
# ASCII and injective, and must read a string by its declaration, not the
# locale. Keys that spell the same text (a latin1 "\xe9" and a UTF-8 "é")
# share a key; any two different texts or byte strings do not.

ck_inputs <- function() {
  list(
    # Each entry is one text; its elements must share a key.
    list("a.com"),
    list(paste0(bom, "a.com")),
    list(paste0(bom, bom, "a.com")),
    list(paste0(zwsp, "a.com")),
    list(bom),
    list(zwsp),
    list(paste0("a", bom, ".com")),
    list("é", octets(0xe9, enc = "latin1"), octets(0xc3, 0xa9)),
    # latin1 "Ã©" has the octets of UTF-8 "é", but not its text.
    list("Ã©", octets(0xc3, 0xa9, enc = "latin1")),
    list(octets(0xe9)),
    list(octets(0x61, 0xff, 0x62)),
    list(octets(0x61, 0xfe, 0x62)),
    list(octets(0xef, 0xbb, 0xbf, 0x61, 0xff)),
    list("\\zz61ff62"),
    list("\\zzc3a9"),
    list("\\u00e9"),
    list("\\ufeffa.com"),
    list("x\\"),
    list("x\\\\"),
    list("中文.cn"),
    list("bücher.example"),
    list("")
  )
}

test_that("cache keys are ASCII, and distinct texts get distinct keys", {
  groups <- ck_inputs()
  keys <- lapply(groups, function(g) .cache_key_ascii(unlist(g)))
  for (i in seq_along(groups)) {
    expect_true(all(is_ascii(keys[[i]])), label = paste("group", i))
    expect_length(unique(keys[[i]]), 1L)
  }
  firsts <- vapply(keys, `[`, character(1), 1L)
  expect_length(unique(firsts), length(groups))
  # The whole vector at once derives what each element derives alone.
  expect_identical(.cache_key_ascii(unlist(groups)), unlist(keys))
})

test_that("an ASCII key without a backslash is its own cache key", {
  x <- c("a.com", "https://h/p?q=1#f", "", "%7E")
  expect_identical(.cache_key_ascii(x), x)
})

test_that("a BOM host is a cache key of its own end to end", {
  rurl_clear_caches()
  withr::defer(rurl_clear_caches())
  .cache_set("puny_encode", "a.com", "plain")
  .cache_set("puny_encode", paste0(bom, "a.com"), "marked")
  .cache_set("puny_encode", paste0(zwsp, "a.com"), "zwsp")
  expect_identical(.cache_get("puny_encode", "a.com"), "plain")
  expect_identical(.cache_get("puny_encode", paste0(bom, "a.com")), "marked")
  expect_identical(.cache_get("puny_encode", paste0(zwsp, "a.com")), "zwsp")
  expect_identical(
    .cache_get_many("puny_encode", c(paste0(bom, "a.com"), "a.com")),
    list("marked", "plain"),
    ignore_attr = TRUE
  )
})

# --- 2. Encoding conversion (R/parse.R, R/url-key.R) --------------------------

test_that("latin1 input is read as ISO 8859-1, octet by octet (0x80-0xFF)", {
  for (b in 0x80:0xff) {
    l1 <- octets(0x61, b, enc = "latin1")
    out <- .latin1_input_to_utf8(l1)
    expect_identical(out, paste0("a", intToUtf8(b)), label = sprintf("%02X", b))
    expect_identical(Encoding(out), "UTF-8")
    expect_identical(charToRaw(out), charToRaw(enc2utf8(paste0("a", intToUtf8(b)))))
  }
  # 0x80-0x9F are the C1 controls U+0080-U+009F, not CP1252's punctuation.
  expect_identical(utf8ToInt(.latin1_input_to_utf8(octets(0x80, enc = "latin1"))),
                   0x80L)
  expect_identical(utf8ToInt(.latin1_input_to_utf8(octets(0x9f, enc = "latin1"))),
                   0x9fL)
  all_high <- octets(0x80:0xff, enc = "latin1")
  expect_identical(utf8ToInt(.latin1_input_to_utf8(all_high)), 0x80:0xff)
})

test_that("only latin1 elements are converted", {
  x <- c("a", NA, "é", octets(0xe9), octets(0xfc, enc = "latin1"), "")
  out <- .latin1_input_to_utf8(x)
  expect_identical(out[-5L], x[-5L])
  expect_identical(Encoding(out), c("unknown", "unknown", "UTF-8", "unknown",
                                    "UTF-8", "unknown"))
  expect_identical(out[5L], "ü")
  expect_identical(.latin1_input_to_utf8(character(0)), character(0))
})

test_that(".prepare_url_input keeps its outputs and marks", {
  bytes_row <- octets(0x68, 0xff, enc = "bytes")
  native <- octets(0x68, 0xc3, 0xa9)
  bad <- octets(0x68, 0xff)
  x <- c("http://a/", NA, "http://é/", native, bad,
         octets(0x68, 0xfc, enc = "latin1"), bytes_row)
  out <- .prepare_url_input(x)
  expect_identical(as_bytes(out$url),
                   as_bytes(c("http://a/", NA, "http://é/", "hé", bad,
                              "hü", NA)))
  expect_identical(Encoding(out$url), c("unknown", "unknown", "UTF-8", "UTF-8",
                                        "UTF-8", "UTF-8", "unknown"))
  expect_identical(out$decodable, c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))

  # The ASCII path returns the vector as it came, names included, and an
  # unnamed verdict; a non-ASCII vector keeps names on both.
  ascii <- c(a = "http://y/", b = NA)
  expect_identical(.prepare_url_input(ascii),
                   list(url = ascii, decodable = c(TRUE, TRUE)))
  named <- c(a = "http://x/é", b = "http://y/")
  expect_identical(.prepare_url_input(named),
                   list(url = named, decodable = c(a = TRUE, b = TRUE)))
  expect_identical(.prepare_url_input(character(0)),
                   list(url = character(0), decodable = logical(0)))
  expect_identical(.prepare_url_input(NULL),
                   list(url = NULL, decodable = logical(0)))
  expect_identical(.prepare_url_input(NA),
                   list(url = NA, decodable = TRUE))
  # Idempotent.
  expect_identical(.prepare_url_input(out$url)$url, out$url)
})

# The URL key's replacement decode stays in stringi (an ADR 0019 exception).
# Each malformed sequence is replaced the WHATWG way (Encoding Standard, UTF-8
# decoder: one U+FFFD per maximal subpart of an ill-formed sequence).
fffd <- "�"
malformed <- list(
  list("lone continuation", c(0x61, 0x80, 0x62), paste0("a", fffd, "b")),
  list("truncated 2-byte", c(0x61, 0xc3), paste0("a", fffd)),
  list("truncated 3-byte", c(0x61, 0xe2, 0x82), paste0("a", fffd)),
  list("truncated 3-byte mid", c(0x61, 0xe2, 0x82, 0x62),
       paste0("a", fffd, "b")),
  list("truncated 4-byte", c(0xf0, 0x9f, 0x98, 0x62), paste0(fffd, "b")),
  list("overlong 2-byte", c(0xc0, 0xaf), strrep(fffd, 2L)),
  list("overlong C1", c(0xc1, 0xbf), strrep(fffd, 2L)),
  list("overlong 3-byte", c(0xe0, 0x80, 0xaf), strrep(fffd, 3L)),
  list("overlong 4-byte", c(0xf0, 0x80, 0x80, 0xaf), strrep(fffd, 4L)),
  list("surrogate", c(0xed, 0xa0, 0x80), strrep(fffd, 3L)),
  list("above U+10FFFF", c(0xf4, 0x90, 0x80, 0x80), strrep(fffd, 4L)),
  list("F5 lead", c(0xf5, 0x80), strrep(fffd, 2L)),
  list("FF", 0xff, fffd),
  list("mark then FF", c(0xef, 0xbb, 0xbf, 0xff), paste0(bom, fffd)),
  list("U+200B then FF", c(0xe2, 0x80, 0x8b, 0xff), paste0(zwsp, fffd))
)

test_that("the URL key frames malformed UTF-8 one U+FFFD per maximal subpart", {
  for (row in malformed) {
    want <- paste0(nchar(row[[3L]], type = "bytes"), ":", row[[3L]])
    for (enc in c("unknown", "UTF-8")) {
      x <- octets(row[[2L]], enc = enc)
      got <- suppressWarnings(.url_key_frame(list(x)))
      expect_identical(got, want, label = paste(row[[1L]], enc))
    }
  }
})

test_that("the URL key frames byte counts, NA and latin1 fields", {
  expect_identical(.url_key_frame(list(c("ab", NA, "", "é", "\U0001F600"))),
                   c("2:ab", "-:", "0:", "2:é", "4:\U0001F600"))
  expect_identical(.url_key_frame(list(octets(0xe9, enc = "latin1"))),
                   "2:é")
  expect_identical(.url_key_frame(list(c(paste0(bom, "a"), paste0(zwsp, "a")))),
                   c(paste0("4:", bom, "a"), paste0("4:", zwsp, "a")))
})

# --- 3. Parameter-name globs (R/path-query.R, R/utils.R) ----------------------

pm <- function(names, patterns, case_sensitive = FALSE) {
  .param_matches(names, .compile_param_matcher(patterns, case_sensitive))
}

test_that("the case-insensitive arm folds by Unicode full case folding", {
  # Each row: name, pattern, whether they match ignoring case.
  pairs <- list(
    list("straße", "STRASSE", TRUE),
    list("STRASSE", "straße", TRUE),
    list("strasse", "straße", TRUE),
    list("K", "k", TRUE),           # KELVIN SIGN folds to k
    list("k", "K", TRUE),
    list("ſ", "s", TRUE),           # LATIN SMALL LETTER LONG S
    list("Σ", "ς", TRUE),      # capital sigma and final sigma
    list("σ", "ς", TRUE),
    list("É", "é", TRUE),
    list("İ", "i", FALSE),          # I WITH DOT ABOVE: not a simple i
    list("ı", "I", FALSE),          # DOTLESS I: not folded to i
    list("I", "i", TRUE),
    list("UTM_Source", "utm_*", TRUE)
  )
  for (p in pairs) {
    expect_identical(pm(p[[1L]], p[[2L]]), p[[3L]],
                     label = paste(p[[1L]], "~", p[[2L]]))
  }
})

test_that("the case-insensitive arm does not follow the ICU default locale", {
  old <- suppressMessages(stringi::stri_locale_set("tr_TR"))
  withr::defer(suppressMessages(stringi::stri_locale_set(old)))
  expect_true(pm("I", "i"))
  expect_true(pm("i", "I"))
  expect_false(pm("ı", "I"))
  expect_true(pm("K", "k"))
})

test_that("the case-sensitive arm compares code points exactly", {
  expect_false(pm("K", "k", TRUE))
  expect_false(pm("UTM_SOURCE", "utm_*", TRUE))
  expect_true(pm("utm_source", "utm_*", TRUE))
  expect_false(pm("straße", "strasse", TRUE))
  expect_true(pm("straße", "straße", TRUE))
})

test_that("only '*' is special in a glob", {
  for (cs in c(FALSE, TRUE)) {
    expect_identical(pm(c("a.b", "axb"), "a.b", cs), c(TRUE, FALSE))
    expect_identical(pm(c("a[0]", "a0"), "a[0]", cs), c(TRUE, FALSE))
    expect_identical(pm(c("a\\b", "ab"), "a\\b", cs), c(TRUE, FALSE))
    expect_identical(pm(c("a+b", "aab"), "a+b", cs), c(TRUE, FALSE))
    expect_identical(pm(c("a|b", "a", "b"), "a|b", cs), c(TRUE, FALSE, FALSE))
    expect_identical(pm(c("(x)", "x"), "(x)", cs), c(TRUE, FALSE))
    expect_identical(pm(c("$x", "x"), "$x", cs), c(TRUE, FALSE))
    expect_identical(pm(c("^x", "x"), "^x", cs), c(TRUE, FALSE))
    expect_identical(pm(c("a{1}", "a"), "a{1}", cs), c(TRUE, FALSE))
    expect_identical(pm(c("a?", "a", "ab"), "a?", cs), c(TRUE, FALSE, FALSE))
    expect_identical(pm(c("a b", "ab"), "a b", cs), c(TRUE, FALSE))
    expect_identical(pm(c("a-b", "a~b"), "a-b", cs), c(TRUE, FALSE))
    expect_identical(pm(c("abc", "ac", "xabc"), "a*c", cs),
                     c(TRUE, TRUE, FALSE))
    expect_identical(pm(c("x", ""), "*", cs), c(TRUE, TRUE))
  }
})

test_that("a multibyte literal in a glob matches itself", {
  for (cs in c(FALSE, TRUE)) {
    expect_identical(pm(c("cafébar", "cafebar"), "café*", cs),
                     c(TRUE, FALSE))
    expect_identical(pm(c("中文_x", "中_x"), "中文_*", cs),
                     c(TRUE, FALSE))
    expect_true(pm("\U0001F600", "\U0001F600", cs))
  }
})

test_that("a leading U+FEFF is data on both sides of the match", {
  for (cs in c(FALSE, TRUE)) {
    for (mark in c(bom, zwsp)) {
      lbl <- sprintf("cs=%s mark=%04X", cs, utf8ToInt(mark))
      name <- paste0(mark, "utm_source")
      expect_false(pm(name, "utm_*", cs), label = lbl)
      expect_true(pm(name, "*utm_*", cs), label = lbl)
      expect_true(pm(name, paste0(mark, "utm_*"), cs), label = lbl)
      expect_false(pm("utm_source", paste0(mark, "utm_*"), cs), label = lbl)
      expect_true(pm(mark, mark, cs), label = lbl)
      expect_false(pm(mark, "x", cs), label = lbl)
      expect_false(pm("", mark, cs), label = lbl)
    }
  }
})

test_that("an undecodable name still matches '*' and nothing narrower", {
  bad <- octets(0x61, 0xff)
  for (cs in c(FALSE, TRUE)) {
    expect_true(pm(bad, "*", cs))
    expect_true(pm(bad, "a*", cs))
    expect_false(pm(bad, "b*", cs))
  }
})

test_that("the matcher handles NULL, empty and NA patterns", {
  expect_null(.compile_param_matcher(c(NA, ""), FALSE))
  expect_identical(.param_matches(c("a", "b"), NULL), c(FALSE, FALSE))
  expect_identical(pm(character(0), "a"), logical(0))
  expect_identical(pm(c("a", "b"), c(NA, "", "b")), c(FALSE, TRUE))
})

test_that("a latin1 glob matches its UTF-8 name in both arms", {
  l1 <- octets(0x63, 0x61, 0x66, 0xe9, enc = "latin1")
  for (cs in c(FALSE, TRUE)) {
    expect_identical(pm(c("café", "cafe"), l1, cs), c(TRUE, FALSE))
  }
})

test_that("query filtering through the globs end to end", {
  f <- ._filter_query_params
  expect_identical(f("utm_source=1&a=2", "filter"), "a=2")
  expect_identical(f("%EF%BB%BFutm_source=1&a=2", "filter"),
                   "%EF%BB%BFutm_source=1&a=2")
  expect_identical(f("%EF%BB%BFx=1&x=2", "filter", params_drop = paste0(bom, "x")),
                   "x=2")
  expect_identical(f("stra%C3%9Fe=1&a=2", "filter", params_drop = "STRASSE"),
                   "a=2")
  expect_identical(f("%E2%84%AA=1&a=2", "filter", params_drop = "k"), "a=2")
  expect_identical(
    f("%E2%84%AA=1&a=2", "filter", params_drop = "k",
      params_case_sensitive = TRUE),
    "%E2%84%AA=1&a=2"
  )
  expect_identical(f("%FF=1&a=2", "filter", params_drop = "*"), "")
})

# --- 4. NFC and case mapping stay in stringi ----------------------------------

test_that("case mapping does not follow the ICU default locale", {
  old <- suppressMessages(stringi::stri_locale_set("tr_TR"))
  withr::defer(suppressMessages(stringi::stri_locale_set(old)))
  expect_identical(.unicode_case_map_vec("WIKI", upper = FALSE), "wiki")
  expect_identical(.unicode_case_map_vec("wiki", upper = TRUE), "WIKI")
})

test_that("the ICU default locale is restored after the Turkish case", {
  expect_false(identical(stringi::stri_locale_get(), "tr_TR"))
})

test_that("case mapping is full Unicode mapping and keeps a leading mark", {
  expect_identical(.unicode_case_map_vec("straße", upper = TRUE),
                   "STRASSE")
  expect_identical(
    .unicode_case_map_vec("ΟΔΟΣ", upper = FALSE),
    "οδος"
  )
  for (mark in c(bom, zwsp)) {
    expect_identical(
      .unicode_case_map_vec(c(paste0(mark, "ab"), "cd", NA), upper = TRUE),
      c(paste0(mark, "AB"), "CD", NA)
    )
    expect_identical(
      .unicode_case_map_vec(paste0(mark, "Σ"), upper = FALSE),
      paste0(mark, "σ")
    )
  }
})

test_that("NFC before Punycode keeps a leading mark", {
  rurl_clear_caches()
  withr::defer(rurl_clear_caches())
  for (mark in c(bom, zwsp)) {
    composed <- .normalize_and_punycode_vec(paste0(mark, "é.example"))
    rurl_clear_caches()
    decomposed <- .normalize_and_punycode_vec(paste0(mark, "é.example"))
    rurl_clear_caches()
    plain <- .normalize_and_punycode_vec("é.example")
    expect_identical(decomposed, composed)
    expect_false(identical(composed, plain))
  }
})
