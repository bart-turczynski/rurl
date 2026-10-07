# Tests for the WHATWG / UTS-46 alternative full-stop mapping vertical slice
# (RURL-odsmwsxu, epic RURL-moselrwp). UTS-46 domain-to-ASCII maps three
# alternative full-stop code points to ASCII "." before a host is split into
# labels: U+3002 (ideographic), U+FF0E (fullwidth), U+FF61 (halfwidth
# ideographic). rurl hands the raw string to the web parser, which does not
# apply
# UTS-46, so under url_standard = "whatwg" rurl maps these to "." in the
# HOST pre-parse (RURL-mkbfseqs narrowed it from the whole
# authority). That lets a Unicode-dot host coerce through the IPv4
# parser (an SSRF-relevant loopback/metadata obfuscation) and normalizes IDN
# separators. RFC 3986 has no UTS-46 mapping, so rfc3986 / no selector keep the
# bytes literal.

U3002 <- "。" # ideographic full stop
UFF0E <- "．" # fullwidth full stop
UFF61 <- "｡" # halfwidth ideographic full stop

# --- whatwg maps the separator and coerces the IPv4 host ---------------------

test_that("whatwg maps U+3002 in an IPv4 host and coerces to dotted-quad", {
  u <- paste0("http://127", U3002, "0", U3002, "0", U3002, "1/")
  expect_identical(get_clean_url(u, url_standard = "whatwg"),
                   "http://127.0.0.1/")
  expect_identical(get_host(u, url_standard = "whatwg"), "127.0.0.1")
})

test_that("whatwg maps U+FF0E in an IPv4 host and coerces to dotted-quad", {
  u <- paste0("http://127", UFF0E, "0", UFF0E, "0", UFF0E, "1/")
  expect_identical(get_clean_url(u, url_standard = "whatwg"),
                   "http://127.0.0.1/")
})

test_that("whatwg maps U+FF61 in an IPv4 host and coerces to dotted-quad", {
  u <- paste0("http://127", UFF61, "0", UFF61, "0", UFF61, "1/")
  expect_identical(get_clean_url(u, url_standard = "whatwg"),
                   "http://127.0.0.1/")
})

test_that("whatwg maps the separator in an IDN name host", {
  # The reversible Unicode host is kept (ADR 0002); only the separator is
  # normalized to ASCII ".".
  u <- paste0("https://例え", U3002, "jp/path")
  expect_identical(get_host(u, url_standard = "whatwg"), "例え.jp")
})

# --- host-only scope: path/query/fragment must NOT be mapped -----------------

test_that("a full-stop variant in the path is left untouched", {
  u <- paste0("http://example.com/文書", U3002, "pdf")
  cu <- get_clean_url(u, url_standard = "whatwg")
  # The path retains the literal ideographic full stop (mapping is host-only).
  expect_true(grepl(paste0("文書", U3002, "pdf"), cu, fixed = TRUE))
})

test_that("a full-stop variant in the query is not mapped to a dot", {
  u <- paste0("http://example.com/a?x=1", U3002, "2")
  q <- safe_parse_urls(u, url_standard = "whatwg")$query
  # The query byte is percent-encoded (UTF-8 of U+3002), never turned into ".".
  expect_false(grepl(".", q, fixed = TRUE))
})

# --- rfc3986 / no selector keep the literal separators -----------------------

test_that("rfc3986 leaves the Unicode separators literal (no UTS-46 mapping)", {
  u <- paste0("http://127", U3002, "0", U3002, "0", U3002, "1/")
  expect_identical(get_clean_url(u, url_standard = "rfc3986"),
                   paste0("http://127", U3002, "0", U3002, "0", U3002, "1/"))
})

test_that("no selector keeps the strict default (separators left literal)", {
  u <- paste0("http://127", U3002, "0", U3002, "0", U3002, "1/")
  expect_identical(get_host(u),
                   paste0("127", U3002, "0", U3002, "0", U3002, "1"))
})

# --- no-op + vectorization ---------------------------------------------------

test_that("a separator-free URL is unchanged under whatwg", {
  u <- "http://Example.com/a/b?q=1#f"
  expect_identical(get_clean_url(u, url_standard = "whatwg"),
                   get_clean_url(u, url_standard = "rfc3986"))
})

test_that("mapping is vectorized and per-row", {
  us <- c(
    paste0("http://127", U3002, "0", U3002, "0", U3002, "1/"), # mapped
    "http://clean.com/",                                        # untouched
    paste0("http://169", UFF0E, "254", UFF0E, "169", UFF0E, "254/") # mapped
  )
  hosts <- get_host(us, url_standard = "whatwg")
  expect_identical(hosts, c("127.0.0.1", "clean.com", "169.254.169.254"))
})

# --- host-only scope: IPv6 literals and userinfo are not domains -------------
# RURL-mkbfseqs. The WHATWG host parser sends a host that starts with "[" to the
# IPv6 parser, which has no UTS-46 step and fails on any code point other than
# an ASCII hex digit, ":" or "." (IPv6-invalid-code-point,
# IPv4-in-IPv6-invalid-code-point). Userinfo never reaches domain to ASCII
# either: it is percent-encoded with the userinfo percent-encode set.

test_that("whatwg rejects a full-stop variant inside an IPv6 literal", {
  us <- c(
    paste0("http://[::ffff:127", U3002, "0", U3002, "0", U3002, "1]/"),
    paste0("http://[::1", UFF0E, "2.3.4]/"),
    paste0("http://[::1.2", UFF61, "3.4]/")
  )
  expect_identical(get_parse_status(us, url_standard = "whatwg"),
                   rep("error", 3L))
  expect_identical(get_host(us, url_standard = "whatwg"),
                   rep(NA_character_, 3L))
})

test_that("an IPv6 literal with a full-stop variant fails under every arm", {
  # ADR 0016 witness: NULL and rfc3986 already reject it and must not move.
  u <- paste0("http://[::ffff:127", U3002, "0", U3002, "0", U3002, "1]/")
  for (std in list(NULL, "rfc3986", "whatwg")) {
    expect_identical(get_parse_status(u, url_standard = std), "error")
  }
})

test_that("whatwg percent-encodes a full-stop variant in userinfo", {
  u <- paste0("http://a", U3002, "b:p", UFF0E, "w@example.com/")
  expect_identical(get_user(u, url_standard = "whatwg"), "a%E3%80%82b")
  expect_identical(get_password(u, url_standard = "whatwg"), "p%EF%BC%8Ew")
  expect_identical(get_host(u, url_standard = "whatwg"), "example.com")
})

test_that("whatwg still maps the host after userinfo", {
  u <- paste0("http://u", U3002, "x@127", U3002, "0", U3002, "0", U3002, "1/")
  expect_identical(get_host(u, url_standard = "whatwg"), "127.0.0.1")
  expect_identical(get_user(u, url_standard = "whatwg"), "u%E3%80%82x")
})

test_that("a line terminator in userinfo does not unsplit the host", {
  # ICU `.` stops at U+000C; the split must still find the last "@", so the
  # host is mapped here and the parse status agrees with it.
  u <- paste0("http://a\fb@ex", UFF0E, "com/")
  expect_identical(get_host(u, url_standard = "whatwg"), "ex.com")
  expect_identical(get_parse_status(u, url_standard = "whatwg"), "ok")
})
