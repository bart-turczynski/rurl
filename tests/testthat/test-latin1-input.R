# RURL-buemdwcq: a latin1-MARKED input element and its UTF-8 twin.
#
# A latin1 mark declares what each octet means: octet 0xNN is the character
# U+00NN (ISO 8859-1). So `"http://h/?\xfc"` marked latin1 and
# `"http://h/?ü"` are the same URL, and R agrees -- they compare equal,
# `unique()` folds them, and the parse cache keys them alike. The tests below
# hold every arm to that reading. The whole suite also runs under LC_ALL=C (the
# gate's locale stage), and every expected value here is spelled with \u
# escapes or raw octets, so the same assertions prove both locales agree.
#
# Each latin1 element is BUILT from raw octets, never from a "\xNN" literal,
# so the test does not depend on how the parser reads one in this locale.

latin1 <- function(...) {
  out <- vapply(list(...), function(octets) rawToChar(as.raw(octets)),
                character(1))
  Encoding(out) <- "latin1"
  out
}

# http://h/?<fc>, http://h/<fc>?q=<fc>#<fc>, http://h<fc>.com/, foo://h/<fc>?<fc>
latin1_urls <- latin1(
  c(utf8ToInt("http://h/?"), 0xfc),
  c(utf8ToInt("http://h/"), 0xfc, utf8ToInt("?q="), 0xfc, 0x23, 0xfc),
  c(utf8ToInt("http://h"), 0xfc, utf8ToInt(".com/")),
  c(utf8ToInt("foo://h/"), 0xfc, 0x3f, 0xfc)
)
utf8_urls <- c(
  "http://h/?ü", "http://h/ü?q=ü#ü", "http://hü.com/",
  "foo://h/ü?ü"
)

# Parse on a cold cache: the twins share a cache key, so a warm cache would
# hand one of them the other's row.
cold_parse <- function(urls, ...) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  safe_parse_urls(urls, ...)
}

expect_bytes <- function(actual, expected, label) {
  as_bytes <- function(v) {
    lapply(v, function(s) if (is.na(s)) NA else charToRaw(s))
  }
  expect_identical(as_bytes(actual), as_bytes(expected), label = label)
}

# The UTF-8 twin's value under each arm. `scheme_acceptance = "general"` is an
# error under url_standard = NULL.
twin <- list(
  "NULL web" = list(
    args = list(url_standard = NULL),
    host = c("h", "h", "hü.com", NA),
    path = c("/", "/ü", "/", NA),
    query = c("%C3%BC", "q=%C3%BC", NA, NA),
    fragment = c(NA, "%C3%BC", NA, NA),
    parse_status = c("warning-no-tld", "warning-no-tld", "ok", "error")
  ),
  "rfc3986 web" = list(
    args = list(url_standard = "rfc3986"),
    host = c("h", "h", "hü.com", NA),
    path = c("/", "/ü", "/", NA),
    query = c("ü", "q=ü", NA, NA),
    fragment = c(NA, "ü", NA, NA),
    parse_status = c("warning-no-tld", "warning-no-tld", "ok", "error")
  ),
  "rfc3986 general" = list(
    args = list(url_standard = "rfc3986", scheme_acceptance = "general"),
    host = c("h", "h", "hü.com", "h"),
    path = c("/", "/ü", "/", "/ü"),
    query = c("ü", "q=ü", NA, "ü"),
    fragment = c(NA, "ü", NA, NA),
    parse_status = c("warning-no-tld", "warning-no-tld", "ok", "ok")
  ),
  "whatwg web" = list(
    args = list(url_standard = "whatwg"),
    host = c("h", "h", "hü.com", NA),
    path = c("/", "/ü", "/", NA),
    query = c("%C3%BC", "q=%C3%BC", NA, NA),
    fragment = c(NA, "%C3%BC", NA, NA),
    parse_status = c("warning-no-tld", "warning-no-tld", "ok", "error")
  ),
  "whatwg general" = list(
    args = list(url_standard = "whatwg", scheme_acceptance = "general"),
    host = c("h", "h", "hü.com", "h"),
    path = c("/", "/ü", "/", "/ü"),
    query = c("%C3%BC", "q=%C3%BC", NA, "%C3%BC"),
    fragment = c(NA, "%C3%BC", NA, NA),
    parse_status = c("warning-no-tld", "warning-no-tld", "ok", "ok")
  )
)
twin_cols <- c("host", "path", "query", "fragment", "parse_status")

# --- Pins: behavior the fix must not move ------------------------------------

test_that("a UTF-8 input parses to these values under every arm", {
  for (arm in names(twin)) {
    r <- do.call(cold_parse, c(list(utf8_urls), twin[[arm]]$args))
    for (col in twin_cols) {
      expect_bytes(r[[col]], twin[[arm]][[col]], paste(arm, col))
    }
  }
})

test_that("whatwg web already reads a latin1 element as its UTF-8 twin", {
  r <- cold_parse(latin1_urls, url_standard = "whatwg")
  for (col in twin_cols) {
    expect_bytes(r[[col]], twin[["whatwg web"]][[col]], col)
  }
})

test_that("original_url echoes a latin1 element untouched", {
  for (arm in names(twin)) {
    r <- do.call(cold_parse, c(list(latin1_urls), twin[[arm]]$args))
    expect_identical(Encoding(r$original_url), rep("latin1", 4L), label = arm)
    expect_bytes(r$original_url, latin1_urls, arm)
  }
})

test_that("bytes-marked and undecodable UTF-8 rows still fail, not throw", {
  bytes_row <- latin1(c(utf8ToInt("http://h/?"), 0xfc))
  Encoding(bytes_row) <- "bytes"
  bad_utf8 <- rawToChar(as.raw(c(utf8ToInt("http://hé.com/"), 0x80)))
  Encoding(bad_utf8) <- "UTF-8"
  for (arm in names(twin)) {
    r <- do.call(cold_parse,
                 c(list(c(bytes_row, "http://example.com/")), twin[[arm]]$args))
    expect_identical(r$parse_status[1], "error", label = arm)
    expect_identical(r$host[2], "example.com", label = arm)
  }
  r <- cold_parse(c(bad_utf8, "http://example.com/"), url_standard = "rfc3986")
  expect_identical(r$parse_status, c("error", "ok"))
})
