# Value pins for the WHATWG component percent encoders (RURL-khcicsca).
#
# The encoders (R/path-query.R) emit, byte for byte, one token per input octet:
# C0 controls, DEL and every non-ASCII octet become an uppercase `%XX`
# triplet, the component's encode set does too, and every other printable
# ASCII octet -- `%` and the hex digits included -- passes through. These pins
# hold that output fixed across the move from a per-byte R loop to one
# vectorized table pass, so every case here is one the two implementations
# could plausibly disagree on: invalid UTF-8, a leading U+FEFF, `%` tails,
# NA / empty, the opaque-path trailing space, and vectors mixing latin1-,
# bytes-, UTF-8- and native-marked strings, in a C and a UTF-8 locale.

enc_path <- function(x) rurl:::.whatwg_path_percent_encode(x)
enc_query <- function(x, scheme = NA_character_) {
  rurl:::.whatwg_query_percent_encode(x, scheme)
}
enc_userinfo <- function(x) rurl:::.whatwg_userinfo_percent_encode(x)
enc_fragment <- function(x) rurl:::.whatwg_fragment_percent_encode(x)
enc_c0 <- function(x) rurl:::.whatwg_component_percent_encode(x, integer(0))

# Apply a scalar-contract encoder element by element, so the same assertion
# holds for the per-row implementation and the vectorized one.
each <- function(f, x, ...) {
  vapply(seq_along(x), function(i) f(x[i], ...), character(1))
}

octets <- function(...) rawToChar(as.raw(c(...)))
marked <- function(x, enc) {
  Encoding(x) <- enc
  x
}
hex_of <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) NA_character_ else paste(charToRaw(s), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# One vector holding every encoding mark R has, with the same e-acute spelled
# three ways plus an undecodable octet: latin1 0xE9, UTF-8 C3 A9, and 0xFF.
mixed_marks <- c(
  marked("caf\xe9 x", "latin1"),
  marked(octets(0x63, 0xff), "bytes"),
  marked(octets(0xc3, 0xa9, 0x20), "UTF-8"),
  octets(0xe2, 0x80, 0x8b, 0x27),
  NA, "", "plain", "%zz%41%"
)

with_ctype <- function(locale, utf8, code) {
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

test_that("each encode set escapes exactly its own printable ASCII", {
  printable <- rawToChar(as.raw(0x20:0x7e))
  expect_identical(
    enc_path(printable),
    paste0(
      "%20!%22%23$%&'()*+,-./0123456789:;%3C=%3E%3F@",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]%5E_%60",
      "abcdefghijklmnopqrstuvwxyz%7B|%7D~"
    )
  )
  expect_identical(
    enc_query(printable),
    paste0(
      "%20!%22%23$%&'()*+,-./0123456789:;%3C=%3E?@",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`",
      "abcdefghijklmnopqrstuvwxyz{|}~"
    )
  )
  expect_identical(
    enc_query(printable, "HTTPS"),
    paste0(
      "%20!%22%23$%&%27()*+,-./0123456789:;%3C=%3E?@",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`",
      "abcdefghijklmnopqrstuvwxyz{|}~"
    )
  )
  expect_identical(
    enc_userinfo(printable),
    paste0(
      "%20!%22%23$%&'()*+,-.%2F0123456789%3A%3B%3C%3D%3E%3F%40",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_%60",
      "abcdefghijklmnopqrstuvwxyz%7B%7C%7D~"
    )
  )
  expect_identical(
    enc_fragment(printable),
    paste0(
      "%20!%22#$%&'()*+,-./0123456789:;%3C=%3E?@",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_%60",
      "abcdefghijklmnopqrstuvwxyz{|}~"
    )
  )
  expect_identical(enc_c0(printable), printable)
})

test_that("C0 controls, DEL and every non-ASCII octet become triplets", {
  controls <- octets(0x01, 0x09, 0x0a, 0x0d, 0x1f, 0x7f)
  for (f in list(enc_path, enc_query, enc_userinfo, enc_fragment, enc_c0)) {
    expect_identical(f(controls), "%01%09%0A%0D%1F%7F")
  }
  high <- marked(rawToChar(as.raw(0x80:0xff)), "bytes")
  expect_identical(
    enc_c0(high), paste(sprintf("%%%02X", 0x80:0xff), collapse = "")
  )
})

test_that("percent signs and their tails pass through unchanged", {
  tails <- c("%", "%4", "%zz", "%%", "%41", "%2z", "%2e%2E", "a%", "%%41",
             "%4%41", "%E9", "%e9")
  for (f in list(enc_path, enc_query, enc_userinfo, enc_fragment, enc_c0)) {
    expect_identical(each(f, tails), tails)
  }
  # A triplet whose tail is non-ASCII keeps the `%` and escapes the octets.
  expect_identical(enc_path(octets(0x25, 0xc3, 0xa9)), "%%C3%A9")
})

test_that("invalid UTF-8 and a leading U+FEFF encode octet by octet", {
  expect_identical(enc_path(octets(0x61, 0xff, 0x62)), "a%FFb")
  expect_identical(enc_query(octets(0xc3)), "%C3")
  expect_identical(enc_fragment(octets(0xe2, 0x80)), "%E2%80")
  expect_identical(enc_userinfo(octets(0x61, 0x20, 0xff, 0x20)), "a%20%FF%20")
  expect_identical(enc_path("\ufeff/a"), "%EF%BB%BF/a")
  expect_identical(enc_c0("\ufeff"), "%EF%BB%BF")
  expect_identical(enc_query("a\ufeff b", "http"), "a%EF%BB%BF%20b")
})

test_that("NA and the empty string come back as they went in", {
  for (f in list(enc_path, enc_query, enc_userinfo, enc_fragment, enc_c0)) {
    expect_identical(each(f, c(NA, "")), c(NA_character_, ""))
  }
})

test_that("an encoding mark is declared over, never transcoded", {
  # latin1 0xE9 stays the octet E9: the encoder reads bytes, it does not turn
  # latin1 into UTF-8 first. The outputs are ASCII, so they carry no mark.
  got <- each(enc_query, mixed_marks, "http")
  expect_identical(
    got,
    c("caf%E9%20x", "c%FF", "%C3%A9%20", "%E2%80%8B%27", NA, "", "plain",
      "%zz%41%")
  )
  expect_true(all(Encoding(got[!is.na(got)]) == "unknown"))
  expect_identical(
    each(enc_path, mixed_marks),
    c("caf%E9%20x", "c%FF", "%C3%A9%20", "%E2%80%8B'", NA, "", "plain",
      "%zz%41%")
  )
})

test_that("the opaque-path trailing space is encoded only before a delimiter", {
  op <- function(p, d) rurl:::.whatwg_opaque_path_encode(p, d)
  expect_identical(op("opaque  ", TRUE), "opaque %20")
  expect_identical(op("opaque  ", FALSE), "opaque  ")
  expect_identical(op("opaque  ", NA), "opaque  ")
  expect_identical(op("a^{} ", TRUE), "a^{}%20")
  expect_identical(op(octets(0xc3, 0xa9, 0x20), TRUE), "%C3%A9%20")
  expect_identical(op(marked("\xe9 ", "latin1"), TRUE), "%E9%20")
  expect_identical(op(" ", TRUE), "%20")
  expect_identical(op("", TRUE), "")
  expect_identical(op(NA_character_, TRUE), NA_character_)
})

test_that("the vector callers render mixed-mark rows like single rows", {
  n <- length(mixed_marks)
  kinds <- ifelse(is.na(mixed_marks), "absent", "present")
  kinds[mixed_marks %in% ""] <- "empty"
  schemes <- rep_len(c("http", "foo"), n)
  expect_identical(
    rurl:::.whatwg_query_suffix_vec(mixed_marks, kinds, schemes),
    c("?caf%E9%20x", "?c%FF", "?%C3%A9%20", "?%E2%80%8B'", "", "?",
      "?plain", "?%zz%41%")
  )
  expect_identical(
    rurl:::.whatwg_fragment_suffix_vec(mixed_marks, kinds),
    c("#caf%E9%20x", "#c%FF", "#%C3%A9%20", "#%E2%80%8B'", "", "#",
      "#plain", "#%zz%41%")
  )
  expect_identical(
    rurl:::.url_key_query(mixed_marks, schemes, "whatwg"),
    c("caf%E9%20x", "c%FF", "%C3%A9%20", "%E2%80%8B'", NA, "", "plain",
      "%zz%41%")
  )
})

test_that("the full serializer encodes userinfo, path and fragment per row", {
  got <- rurl:::.serialize_whatwg_full_vec(
    scheme = c("http", "foo", "foo", "http"),
    userinfo = c(marked("u\xe9:p w", "latin1"), NA, NA, "a@b:"),
    host = c("h", NA, NA, "h"),
    host_kind = c("present", "absent", "absent", "present"),
    authority_delimiter_present = c(TRUE, FALSE, FALSE, TRUE),
    path = c(
      marked(octets(0x2f, 0xc3, 0xa9, 0x20), "UTF-8"), "op aque ",
      "//x y", "/\ufeff"
    ),
    path_kind = c("list", "opaque", "list", "list"),
    query = c("q'", NA, "a b", NA),
    query_kind = c("present", "absent", "present", "absent"),
    fragment = c(NA, "f`", NA, octets(0xff)),
    fragment_kind = c("absent", "present", "absent", "present"),
    port = c("8080", NA, NA, NA)
  )
  expect_identical(got, c(
    "http://u%E9:p%20w@h:8080/%C3%A9%20?q%27",
    "foo:op aque%20#f%60",
    "foo:/.//x%20y?a%20b",
    "http://a%40b@h/%EF%BB%BF#%FF"
  ))
})

test_that("the clean serializer encodes list and opaque paths per row", {
  got <- rurl:::.serialize_whatwg_vec(
    scheme = c("foo", "foo", "foo", "foo"),
    host = c(".", "h", NA, NA),
    host_kind = c("present", "present", "absent", "absent"),
    authority_delimiter_present = c(TRUE, TRUE, FALSE, FALSE),
    path = c("/", "/", "a b ", "//\ufeff"),
    path_kind = c("list", "list", "opaque", "list"),
    query = c(NA, NA, "x'", NA),
    query_kind = c("absent", "absent", "present", "absent"),
    port = NULL, port_handling = "keep", trailing_slash_handling = "strip"
  )
  expect_identical(got, c("foo://./", "foo://h", "foo:a b ?x'",
                          "foo:/.//%EF%BB%BF"))
})

test_that("encoder output does not depend on the session charset", {
  run <- function() {
    list(
      path = each(enc_path, mixed_marks),
      query = each(enc_query, mixed_marks, "ws"),
      userinfo = each(enc_userinfo, mixed_marks),
      fragment = each(enc_fragment, mixed_marks),
      c0 = each(enc_c0, mixed_marks),
      suffix = rurl:::.whatwg_query_suffix_vec(
        mixed_marks, rep("present", length(mixed_marks)), "http"
      )
    )
  }
  ref <- run()
  for (loc in names(ctypes)) {
    with_ctype(loc, ctypes[[loc]], {
      got <- run()
      for (nm in names(ref)) {
        expect_identical(hex_of(got[[nm]]), hex_of(ref[[nm]]), info = nm)
      }
    })
  }
})

# --- the vector contract (RURL-khcicsca) -------------------------------------

# Every encoder takes a whole vector in one call. A mixed-mark vector is the
# hard case: `paste()` across rows would transcode a latin1 row if the rows did
# not share one declaration first.
pool <- c(
  mixed_marks,
  octets(0x61, 0xff, 0x62), "\ufeff/a b", "%", "%4", "%%", "a\"b#c<d>e'f",
  marked(rawToChar(as.raw(0x80:0xff)), "bytes"),
  marked("x\xe9:y@z", "latin1"), octets(0x01, 0x7f, 0x20)
)

test_that("a whole-vector call matches the row-by-row result", {
  run <- function() {
    schemes <- rep_len(c("http", "foo", NA, "WSS"), length(pool))
    list(
      path = list(enc_path(pool), each(enc_path, pool)),
      userinfo = list(enc_userinfo(pool), each(enc_userinfo, pool)),
      fragment = list(enc_fragment(pool), each(enc_fragment, pool)),
      c0 = list(enc_c0(pool), each(enc_c0, pool)),
      query = list(
        enc_query(pool, schemes),
        vapply(seq_along(pool), function(i) enc_query(pool[i], schemes[i]),
               character(1))
      )
    )
  }
  for (loc in names(ctypes)) {
    with_ctype(loc, ctypes[[loc]], {
      for (pair in run()) {
        expect_identical(hex_of(pair[[1]]), hex_of(pair[[2]]))
      }
    })
  }
  expect_identical(
    enc_query(c("'", "'", "'"), c("http", "foo", NA)), c("%27", "'", "'")
  )
  expect_identical(enc_query(c("'", "'"), "https"), c("%27", "%27"))
})

test_that("several batches stitch back into the same rows", {
  one <- rurl:::.whatwg_component_percent_encode(
    pool, rurl:::.WHATWG_PATH_ENCODE_SET
  )
  many <- rurl:::.whatwg_component_percent_encode(
    pool, rurl:::.WHATWG_PATH_ENCODE_SET, batch_bytes = 5
  )
  expect_identical(hex_of(many), hex_of(one))
})

test_that("the encoders keep length, names and the empty vector", {
  expect_identical(enc_path(character(0)), character(0))
  expect_identical(enc_query(character(0), character(0)), character(0))
  expect_identical(
    rurl:::.whatwg_opaque_path_encode(character(0), logical(0)), character(0)
  )
  named <- c(a = "x y", b = NA, c = "z")
  expect_identical(enc_path(named), c(a = "x%20y", b = NA, c = "z"))
  # The opaque-host parser's scalar call (R/parse-state.R) is unchanged.
  expect_identical(
    rurl:::.whatwg_component_percent_encode("h\u00e9", integer(0)), "h%C3%A9"
  )
})

test_that("opaque-path trailing space works across a vector", {
  got <- rurl:::.whatwg_opaque_path_encode(
    c("a ", "a ", "a ", NA, "", octets(0xff, 0x20), "b  "),
    c(TRUE, FALSE, NA, TRUE, TRUE, TRUE, TRUE)
  )
  expect_identical(got, c("a%20", "a ", "a ", NA, "", "%FF%20", "b %20"))
})

test_that("an encode set naming `%` or a hex digit is refused", {
  # The table lookup is only the old re-emit rule because `%` and the hex
  # digits pass through; a set that encoded them would silently diverge.
  expect_error(
    rurl:::.whatwg_component_percent_encode("%41", 0x25L), "internal error"
  )
  expect_error(
    rurl:::.whatwg_component_percent_encode("a", 0x61L), "internal error"
  )
})
