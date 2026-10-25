# The ICU-emulating cuts and tests of R/parse-phases.R stay linear in the row
# and under PCRE's match limit (RURL-atlidhyh). Each block pins the helper's
# reading on short rows first, then a row long enough to have hit the limit.

test_that(".icu_cut_through_last_at() cuts through the last \"@\" of line 1", {
  cut <- .icu_cut_through_last_at
  # The last "@" wins; with none, nothing is cut.
  expect_identical(cut("u@h"), "h")
  expect_identical(cut("a@b@c"), "c")
  expect_identical(cut("@@@h"), "h")
  expect_identical(cut("u@"), "")
  expect_identical(cut("@"), "")
  expect_identical(cut("h"), "h")
  expect_identical(cut(""), "")
  expect_identical(cut(NA_character_), NA_character_)
  expect_identical(cut(c("a@b", NA, "c", "d@e@f")), c("b", NA, "c", "f"))
  expect_identical(cut(character()), character())
  # ICU's `.` stops at the first line terminator, so an "@" after one is not
  # reached; the terminator and all after it stay.
  expect_identical(cut("a@b\nc@d"), "b\nc@d")
  expect_identical(cut("a\n@b"), "a\n@b")
  expect_identical(cut("a@b\r\nc@d"), "b\r\nc@d")
  expect_identical(cut("a@b\u2028c@d"), "b\u2028c@d")
  expect_identical(cut("a@b\u0085c@d"), "b\u0085c@d")
  expect_identical(cut("a@b\vc@d"), "b\vc@d")
  # U+3002 and other multibyte code points are data; positions are bytes.
  expect_identical(cut("u\u3002i@h\u3002b"), "h\u3002b")
  expect_identical(cut("\u00fc@\u00df@\u00e9"), "\u00e9")
  # A leading U+FEFF is data, cut with the userinfo or kept with the host.
  expect_identical(cut("\ufeffu@h"), "h")
  expect_identical(cut("\ufeff@h"), "h")
  expect_identical(cut("\ufeffh"), "\ufeffh")
  expect_identical(cut("u@\ufeffh"), "\ufeffh")
  expect_identical(Encoding(cut("u@h\u00e9")), "UTF-8")
})

test_that(".classify_input_host_vec() reads the host after the last \"@\"", {
  cls <- .classify_input_host_vec(c(
    "http://u\u3002i@ha\u3002b/p", "http://a@b@1.2.3.4:80/", "//x@[::1]:8/"
  ))
  expect_identical(cls$host_token, c("ha\u3002b", "1.2.3.4", "[::1]"))
  expect_identical(cls$has_userinfo, c(TRUE, TRUE, TRUE))
  expect_identical(cls$is_canonical_ip, c(FALSE, TRUE, TRUE))
})

# PCRE2's default match limit is 1e7; a row a little past it. The long rows
# below run through the helper that cuts or tests them, not a full parse, so
# the file stays fast.
long_n <- 10500000L

test_that("the last-\"@\" cut stays under the match limit on a long row", {
  # `\A(?s:.)*@` ran to the end and gave the row back byte by byte to the
  # last "@": past ~1e7 bytes `regexpr()` warned and the row went uncut, so
  # the userinfo read as part of the host token.
  host <- paste0("h", strrep("a", long_n), "\u3002b")
  expect_no_warning(out <- .icu_cut_through_last_at(paste0("u\u3002i@", host)))
  # `expect_true(identical())`: a failure must not print the row.
  expect_true(identical(out, host))
  expect_no_warning(out <- .icu_cut_through_last_at(strrep("@", long_n)))
  expect_identical(out, "")
})

test_that("the host classifier drops only the last \":port\"", {
  tok <- function(x) .classify_input_host_vec(x)$host_token
  expect_identical(
    tok(c("http://h:80/", "http://h:1:2/", "http://h/", "http://h:/",
          "u@h:8", "http://a:b@h:9/", "h:1\v2", NA)),
    c("h", "h:1", "h", "h", "h", "h", "h", NA)
  )
})

test_that("the port cut stays under the match limit on a long row", {
  # `:[^:]*\z` gave back the run after each earlier ":" byte by byte: with
  # ~1e7 bytes before the last ":" it warned and the port stayed on.
  host <- paste0("h:", strrep("a", long_n))
  expect_no_warning(
    cls <- .classify_input_host_vec(paste0("http://", host, ":80/p"))
  )
  expect_true(identical(cls$host_token, host))
})

test_that("the userinfo encoder reads the span before the last \"@\"", {
  enc <- function(x) .encode_userinfo_charset_vec(x, "whatwg")$url
  expect_identical(
    enc(c("http://u v@h/", "http://u@h v/", "http://a b@c d@h/", "http://h/",
          "https://u\x01@h@x/", "http://u@h\x01/", NA)),
    c("http://u%20v@h/", "http://u@h v/", "http://a%20b@c%20d@h/", "http://h/",
      "https://u%01@h@x/", "http://u@h\x01/", NA)
  )
})

test_that("the userinfo span stays under the match limit on a long row", {
  # `@[^@]*\z` gave back the run after each earlier "@" byte by byte: with
  # ~1e7 bytes between two "@" it warned and read the host as userinfo.
  url <- paste0("http://u@", strrep("a", long_n), "@h\x01/")
  expect_no_warning(out <- .encode_userinfo_charset_vec(url, "whatwg")$url)
  expect_true(identical(out, url))
})
