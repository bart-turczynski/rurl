# The percent-escape and hex-uppercase helpers of R/parse-web.R work over a
# byte table, not one R call per byte (RURL-iyjdljln). Each block pins the
# helper's reading on short inputs first, then a row long enough that a
# per-byte loop dominated a full parse.

bytes <- function(s) as.integer(charToRaw(s))
chr <- function(b) rawToChar(as.raw(b))

test_that(".web_uppercase_pct() uppercases each claimed pair, in order", {
  up <- function(s) chr(.web_uppercase_pct(bytes(s)))
  expect_identical(up("%2e%c3"), "%2E%C3")
  # The pair is uppercased whether or not it is hex.
  expect_identical(up("%zz"), "%ZZ")
  # A "%" with fewer than two bytes after it is left alone.
  expect_identical(up("a%b"), "a%b")
  expect_identical(up("%a"), "%a")
  # A sequential scan: the first "%" claims "%2", and the scan resumes at "e".
  expect_identical(up("%%2e"), "%%2e")
  expect_identical(up("%%%2e"), "%%%2e")
  expect_identical(up("%%%%2e"), "%%%%2E")
  expect_identical(up("%2%c3%82z"), "%2%c3%82z")
  expect_identical(.web_uppercase_pct(integer()), integer())
})

test_that(".web_escape_bytes() writes a selected byte as a lowercase triplet", {
  esc <- function(s, sel) chr(.web_escape_bytes(bytes(s), sel))
  expect_identical(esc("a b", c(FALSE, TRUE, FALSE)), "a%20b")
  expect_identical(esc("\xc3\xa9", c(TRUE, TRUE)), "%c3%a9")
  expect_identical(esc(" ", TRUE), "%20")
  expect_identical(esc("ab", c(FALSE, FALSE)), "ab")
})

test_that("the uppercase-triplet helpers keep everything else as written", {
  expect_identical(.web_escape_pqf_bytes("/a b%2e\x7f"), "/a%20b%2e%7F")
  expect_identical(.web_escape_high_bytes("q=é%2e"), "q=%C3%A9%2e")
  expect_identical(.web_escape_pqf_bytes("/ab"), "/ab")
  expect_identical(.web_escape_high_bytes(NA_character_), NA_character_)
  expect_identical(chr(.web_encode_at(bytes("u@x@"))), "u%40x%40")
})

test_that("a long inner whitespace run parses without a per-byte loop", {
  # A ~1 MB row: not on CRAN.
  skip_on_cran()
  n <- 1000000L
  u <- paste0("http://e.com/a", strrep(" ", n), "b?q #f")
  rurl_clear_caches()
  elapsed <- system.time(
    p <- safe_parse_url(u, url_standard = "whatwg")
  )[["elapsed"]]
  # `expect_true(identical())`: a failure must not print the row.
  expect_true(identical(
    p$path, paste0("/a", strrep("%20", n), "b")
  ))
  expect_identical(p$query, "q%20")
  # The per-byte loops took about 6 s here; the byte table about 1 s. The
  # bound is loose so a busy machine does not fail it.
  expect_lt(elapsed, 4)
  rurl_clear_caches()
})
