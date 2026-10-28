# RURL-buemdwcq: a latin1-MARKED input element and its UTF-8 twin.
#
# A latin1 mark declares what each octet means: octet 0xNN is the character
# U+00NN (ISO 8859-1). So `"http://h/?\xfc"` marked latin1 and
# `"http://h/?\u00fc"` are the same URL, and R agrees -- they compare equal,
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

# http://h/?<fc>, http://h/<fc>?q=<fc>#<fc>, http://h<fc>.com/ and
# foo://h/<fc>?<fc>
latin1_urls <- latin1(
  c(utf8ToInt("http://h/?"), 0xfc),
  c(utf8ToInt("http://h/"), 0xfc, utf8ToInt("?q="), 0xfc, 0x23, 0xfc),
  c(utf8ToInt("http://h"), 0xfc, utf8ToInt(".com/")),
  c(utf8ToInt("foo://h/"), 0xfc, 0x3f, 0xfc)
)
utf8_urls <- c(
  "http://h/?\u00fc", "http://h/\u00fc?q=\u00fc#\u00fc", "http://h\u00fc.com/",
  "foo://h/\u00fc?\u00fc"
)

# Parse on a cold cache: the twins share a cache key, so a warm cache would
# hand one of them the other's row.
cold_parse <- function(urls, ...) {
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  safe_parse_urls(urls, ...)
}

# Compare character values octet by octet (`identical()` on strings would
# match a latin1 "\xfc" against "\u00fc"); anything else as it is.
expect_bytes <- function(actual, expected, label) {
  as_bytes <- function(v) {
    if (!is.character(v)) {
      return(v)
    }
    lapply(v, function(s) if (is.na(s)) NA else charToRaw(s))
  }
  expect_identical(as_bytes(actual), as_bytes(expected), label = label)
}

# The UTF-8 twin's value under each arm. `scheme_acceptance = "general"` is an
# error under url_standard = NULL.
twin <- list(
  "NULL web" = list(
    args = list(url_standard = NULL),
    host = c("h", "h", "h\u00fc.com", NA),
    path = c("/", "/\u00fc", "/", NA),
    query = c("%C3%BC", "q=%C3%BC", NA, NA),
    fragment = c(NA, "%C3%BC", NA, NA),
    parse_status = c("warning-no-tld", "warning-no-tld", "ok", "error")
  ),
  # Every twin writes U+00FC raw, so under rfc3986 every row is a parse
  # error on both routes: a URI is US-ASCII and non-ASCII data is written
  # as percent-encoded UTF-8 (RFC 3986 sections 2, 2.5, 3.2.2; RUL-030,
  # RURL-pntbujzc). A latin1 element still reads as its twin, so it fails
  # the same way.
  "rfc3986 web" = list(
    args = list(url_standard = "rfc3986"),
    host = rep(NA_character_, 4L),
    path = rep(NA_character_, 4L),
    query = rep(NA_character_, 4L),
    fragment = rep(NA_character_, 4L),
    parse_status = rep("error", 4L)
  ),
  "rfc3986 general" = list(
    args = list(url_standard = "rfc3986", scheme_acceptance = "general"),
    host = rep(NA_character_, 4L),
    path = rep(NA_character_, 4L),
    query = rep(NA_character_, 4L),
    fragment = rep(NA_character_, 4L),
    parse_status = rep("error", 4L)
  ),
  "whatwg web" = list(
    args = list(url_standard = "whatwg"),
    host = c("h", "h", "h\u00fc.com", NA),
    path = c("/", "/\u00fc", "/", NA),
    query = c("%C3%BC", "q=%C3%BC", NA, NA),
    fragment = c(NA, "%C3%BC", NA, NA),
    parse_status = c("warning-no-tld", "warning-no-tld", "ok", "error")
  ),
  "whatwg general" = list(
    args = list(url_standard = "whatwg", scheme_acceptance = "general"),
    host = c("h", "h", "h\u00fc.com", "h"),
    path = c("/", "/\u00fc", "/", "/\u00fc"),
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
  bad_utf8 <- rawToChar(as.raw(c(utf8ToInt("http://h\u00e9.com/"), 0x80)))
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

# --- The fix -----------------------------------------------------------------
#
# ADR 0016: this fix MOVES `url_standard = NULL` rows. The freeze binds drift
# a standard causes; this is a defect in the default path itself -- no
# standard, profile or contract makes a latin1 "\xfc" mean anything but U+00FC,
# and the NULL arm gave its twin `%C3%BC` while giving it `%FC`.
#
#   1. WITNESS: the first test runs under the default selector, omitted and
#      explicit NULL alike. Before the fix, `query` was "%FC" and
#      "q=%FC", `fragment` "%FC", and the latin1 host row was an "error".
#   2. SIGNATURE: inputs = a latin1-MARKED element with an octet >= 0x80
#      (a UTF-8, ASCII, NA, bytes-marked or undecodable element is untouched,
#      pinned above); options = any; what moves is every parse column of such
#      a row, which now equals its UTF-8 twin's, and so do the accessors and
#      serializers built on the parse. Per arm, measured before the fix:
#        NULL web       -- query and fragment ("%FC"), the non-ASCII host row
#                          (error).
#        rfc3986 web    -- query and fragment (UTF-8-marked lone 0xFC, invalid
#                          UTF-8), the non-ASCII host row (error).
#        rfc3986 general and whatwg general -- the same, plus every
#                          general-routed row (error).
#        whatwg web     -- already correct (pinned above): its encoders read
#                          the latin1 mark.
#      `original_url` keeps the caller's latin1 element (pinned above).

test_that("NULL witness: a latin1 element parses as its UTF-8 twin", {
  for (r in list(cold_parse(latin1_urls),
                 cold_parse(latin1_urls, url_standard = NULL))) {
    for (col in twin_cols) {
      expect_bytes(r[[col]], twin[["NULL web"]][[col]], col)
    }
    expect_identical(r$query[1:2], c("%C3%BC", "q=%C3%BC"))
  }
})

test_that("every arm parses a latin1 element as its UTF-8 twin", {
  for (arm in names(twin)) {
    a <- do.call(cold_parse, c(list(latin1_urls), twin[[arm]]$args))
    b <- do.call(cold_parse, c(list(utf8_urls), twin[[arm]]$args))
    for (col in setdiff(names(b), "original_url")) {
      expect_bytes(a[[col]], b[[col]], paste(arm, col))
    }
    for (col in twin_cols) {
      expect_bytes(a[[col]], twin[[arm]][[col]], paste(arm, col))
    }
  }
})

test_that("the shared cache key no longer makes the result order-dependent", {
  for (arm in names(twin)) {
    args <- twin[[arm]]$args
    rurl_clear_caches()
    first_latin1 <- do.call(safe_parse_urls, c(list(latin1_urls), args))
    then_utf8 <- do.call(safe_parse_urls, c(list(utf8_urls), args))
    rurl_clear_caches()
    first_utf8 <- do.call(safe_parse_urls, c(list(utf8_urls), args))
    then_latin1 <- do.call(safe_parse_urls, c(list(latin1_urls), args))
    rurl_clear_caches()
    for (col in twin_cols) {
      expected <- twin[[arm]][[col]]
      expect_bytes(first_latin1[[col]], expected, paste(arm, col, 1))
      expect_bytes(then_utf8[[col]], expected, paste(arm, col, 2))
      expect_bytes(first_utf8[[col]], expected, paste(arm, col, 3))
      expect_bytes(then_latin1[[col]], expected, paste(arm, col, 4))
    }
  }
})

test_that("latin1 0x80-0x9F reads as U+0080-U+009F, as whatwg read it", {
  # ISO 8859-1, the reading of the cache key and the whatwg route; R's own
  # latin1 translation would read 0x80 as CP1252's U+20AC.
  x <- latin1(c(utf8ToInt("http://h/?"), 0x80))
  expect_identical(cold_parse(x)$query, "%C2%80")
  expect_identical(cold_parse(x, url_standard = "whatwg")$query, "%C2%80")
  # rfc3986 reads the same U+0080 and rejects it as raw non-ASCII (RFC 3986
  # sections 2, 2.5; RUL-030); the grammar's component walk still admits it
  # as one scalar value, the reading the cache key shares.
  expect_identical(cold_parse(x, url_standard = "rfc3986")$query,
                   NA_character_)
  expect_identical(
    get_url_diagnostics(x, url_standard = "rfc3986"),
    "unicode-outside-rfc3986-uri"
  )
  expect_true(.rfc3986_generic_uri_ok_walk(.latin1_input_to_utf8(x)))
})

test_that("get_mailto_recipients() reads a latin1 recipient as its twin", {
  x <- latin1(c(utf8ToInt("mailto:"), 0xfc, utf8ToInt("@h.com")))
  for (std in c("whatwg", "rfc3986")) {
    rurl_clear_caches()
    a <- get_mailto_recipients(x, url_standard = std)
    b <- get_mailto_recipients("mailto:\u00fc@h.com", url_standard = std)
    expect_identical(nrow(a), 1L, label = std)
    for (col in setdiff(names(b), "url")) {
      expect_bytes(a[[col]], b[[col]], paste(std, col))
    }
    expect_bytes(a$url, x, paste(std, "url echo"))
  }
})

test_that("the accessors and serializers follow the parse", {
  # serialize_url() and the host helpers read the input themselves rather than
  # through safe_parse_urls(), so each needs the same entry conversion.
  rurl_clear_caches()
  on.exit(rurl_clear_caches(), add = TRUE)
  expect_bytes(get_host(latin1_urls[3]), "h\u00fc.com", "get_host")
  for (std in c("whatwg", "rfc3986")) {
    for (form in c("source", "normalized")) {
      expect_bytes(serialize_url(latin1_urls, standard = std, form = form),
                   serialize_url(utf8_urls, standard = std, form = form),
                   paste(std, form))
    }
    expect_identical(get_host_type(latin1_urls, url_standard = std),
                     get_host_type(utf8_urls, url_standard = std), label = std)
    expect_identical(is_valid_host(latin1_urls[3], url_standard = std),
                     is_valid_host(utf8_urls[3], url_standard = std),
                     label = std)
    a <- check_hosts(latin1_urls, url_standard = std)
    b <- check_hosts(utf8_urls, url_standard = std)
    for (col in setdiff(names(b), "url")) {
      expect_bytes(a[[col]], b[[col]], paste(std, col))
    }
  }
})
