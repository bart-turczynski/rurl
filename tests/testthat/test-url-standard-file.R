# Tests for file:// support (RURL-rutsdflg, epic RURL-apxhgjhf). WHATWG treats
# file as a special scheme; RFC 3986 admits it as an ordinary registered
# hierarchical scheme. rurl supports the hostless forms libcurl can parse:
# file:///... and file://localhost/... (localhost collapses to empty host).

test_that("file URLs parse under both standard profiles", {
  urls <- c(
    "file://localhost/path/to/file.txt",
    "file:///path/to/file.txt"
  )

  for (std in c("whatwg", "rfc3986")) {
    parsed <- safe_parse_urls(urls, url_standard = std)

    expect_identical(parsed$scheme, c("file", "file"), info = std)
    expect_true(all(is.na(parsed$host)), info = std)
    expect_identical(
      parsed$path, c("/path/to/file.txt", "/path/to/file.txt"), info = std
    )
    expect_identical(
      parsed$clean_url,
      c("file:///path/to/file.txt", "file:///path/to/file.txt"),
      info = std
    )
    expect_identical(parsed$parse_status, c("ok", "ok"), info = std)
    expect_identical(parsed$port, c(NA_integer_, NA_integer_), info = std)
  }
})

test_that("file URLs compose with accessors and scheme classification", {
  u <- "file://localhost/path/to/file.txt"

  expect_identical(get_clean_url(u, url_standard = "whatwg"),
    "file:///path/to/file.txt")
  expect_identical(get_path(u, url_standard = "whatwg"), "/path/to/file.txt")
  expect_true(is.na(get_host(u, url_standard = "whatwg")))
  expect_identical(get_scheme_class(u, url_standard = "whatwg"), "special")
})

test_that("legacy and RFC file forms are parsed in-tree, not by libcurl", {
  # These two inputs are the epic's known-divergent pair (RURL-gxqdmpcp): they
  # returned "error" on Linux/macOS and "ok" on Windows from the SAME libcurl
  # call, because libcurl's file: handling is a BUILD property. They are now
  # decided by the in-tree RFC 8089 overlay, so the answer is the same on every
  # platform. This test previously asserted the macOS half of that divergence.
  urls <- c("file://example.com/path", "file:///c:/Windows/System32")

  expect_identical(get_parse_status(urls), c("ok", "ok"))
  expect_identical(
    get_parse_status(urls, url_standard = "rfc3986"), c("ok", "ok")
  )
  # The NULL selector and "rfc3986" agree by construction: both route file: to
  # the same overlay (RURL-obsweger decision (a)).
  expect_identical(
    get_clean_url(urls), get_clean_url(urls, url_standard = "rfc3986")
  )
  expect_identical(
    get_clean_url(urls),
    c("file://example.com/path", "file:///c:/Windows/System32")
  )
  # The drive-letter path keeps its leading slash. Windows libcurl returned
  # path="c:/Windows/System32", which re-parses with host=c: -- one reason not
  # to adopt the Windows answer wholesale.
  expect_identical(get_path(urls)[2L], "/c:/Windows/System32")
})

test_that("file: Gate 1 rejects forms that are not valid RFC 3986", {
  # RFC 8089 App. F gives ABNF to nonstandard forms that ESCAPE RFC 3986:
  # `drive-letter = ALPHA ":" / ALPHA "|"`, but "|" is absent from `pchar`.
  # Gate 1 rejects those; the whatwg profile repairs them instead.
  bs <- paste0("file:///path", rawToChar(as.raw(92L)), "to")
  for (std in list(NULL, "rfc3986")) {
    status <- function(x) {
      if (is.null(std)) {
        get_parse_status(x)
      } else {
        get_parse_status(x, url_standard = std)
      }
    }
    expect_identical(status("file://C|/x"), "error")
    expect_identical(status(bs), "error")
    expect_identical(status("file://[example]/"), "error")
  }
  # Percent-encoding is the LEGAL way to carry "|" in a reg-name, so the
  # encoded form is accepted where the bare form is not. Confirmed against
  # Ruby's URI::RFC3986_Parser, which draws the same line.
  expect_identical(get_parse_status("file://C%7C"), "ok")
  # WHATWG repairs rather than rejects (matches adaR / Node `new URL()`).
  expect_identical(
    get_path("file://C|/x", url_standard = "whatwg"), "/C:/x"
  )
})

test_that("file: Gate 2 applies RFC 8089 S2's narrowing of the authority", {
  # No port: S2's `file-auth = "localhost" / host` has none, and no appendix
  # supplies a production for one -> parse failure.
  expect_identical(get_parse_status("file://example.com:80/path"), "error")
  expect_identical(
    get_parse_status("file://example.com:80/path", url_standard = "rfc3986"),
    "error"
  )
  # userinfo IS admitted, by App. E.1/F's production, and is surfaced as a
  # fact rather than silently discarded.
  u <- "file://user@example.com/path"
  expect_identical(get_parse_status(u), "ok")
  expect_identical(get_user(u), "user")
  expect_identical(get_host(u), "example.com")
  # query/fragment survive: RFC 8089 never mentions either, and RFC 3986 S3.5
  # forbids scheme specs from restricting the fragment at all. RFC 8118 S3
  # depends on this working (application/pdf `#page=`).
  expect_identical(get_parse_status("file:///doc.pdf#page=2"), "ok")
  expect_identical(get_fragment("file:///doc.pdf#page=2"), "page=2")
  expect_identical(get_query("file:///data.csv?v=2"), "v=2")
})

test_that("WHATWG file parser accepts drive-letter and bare path forms", {
  cases <- data.frame(
    input = c(
      "file:C|/m/",
      "file:C||/m/",
      "file:/C|/",
      "file://C|/",
      "file:///Y:",
      "file:///Y:/",
      "file:",
      "file:.",
      "file:/example.com/",
      "file:.//p",
      "file:/.//p"
    ),
    path = c(
      "/C:/m/",
      "/C||/m/",
      "/C:/",
      "/C:/",
      "/Y:",
      "/Y:/",
      "/",
      "/",
      "/example.com/",
      "//p",
      "//p"
    ),
    stringsAsFactors = FALSE
  )

  parsed <- safe_parse_urls(cases$input, url_standard = "whatwg")

  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_identical(parsed$scheme, rep("file", nrow(cases)))
  expect_true(all(is.na(parsed$host)))
  expect_identical(parsed$path, cases$path)
})

test_that("WHATWG file parser preserves query and fragment on empty paths", {
  parsed <- safe_parse_urls(
    c("file:?q=v", "file:#frag"),
    url_standard = "whatwg",
    query_handling = "keep"
  )

  expect_identical(parsed$parse_status, c("ok", "ok"))
  expect_identical(parsed$path, c("/", "/"))
  expect_identical(parsed$query, c("q=v", NA_character_))
  expect_identical(parsed$fragment, c(NA_character_, "frag"))
  expect_identical(parsed$clean_url[1L], "file:///?q=v")
})

test_that("WHATWG file parser handles reverse solidus file states", {
  cases <- data.frame(
    input = c(
      r"(file:\\//)",
      r"(file:\\\\)",
      r"(file:\\\\?fox)",
      r"(file:\\\\#guppy)",
      r"(file:\\localhost//)",
      r"(file://\/localhost//cat)"
    ),
    path = c("//", "//", "//", "//", "//", "//localhost//cat"),
    query = c(NA_character_, NA_character_, "fox", NA_character_, NA_character_,
              NA_character_),
    fragment = c(NA_character_, NA_character_, NA_character_, "guppy",
                 NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  parsed <- safe_parse_urls(
    cases$input, url_standard = "whatwg", query_handling = "keep"
  )

  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_true(all(is.na(parsed$host)))
  expect_identical(parsed$path, cases$path)
  expect_identical(parsed$query, cases$query)
  expect_identical(parsed$fragment, cases$fragment)
})

test_that("WHATWG file parser accepts non-local and UTS-mapped hosts", {
  cases <- data.frame(
    input = c(
      "file://spider///",
      "file://example.net/C:/",
      "file://1.2.3.4/C:/",
      "file://[1::8]/C:/",
      "file://a\u00adb/p",
      "file://a%C2%ADb/p",
      paste0(
        "file://loC",
        "\U0001D400\U0001D40B\U0001D407\U0001D428\U0001D42C\U0001D42D",
        "/usr/bin"
      ),
      "file://xn--/p"
    ),
    host = c(
      "spider",
      "example.net",
      "1.2.3.4",
      "[1::8]",
      "ab",
      "ab",
      NA_character_,
      "xn--"
    ),
    path = c("///", "/C:/", "/C:/", "/C:/", "/p", "/p", "/usr/bin", "/p"),
    stringsAsFactors = FALSE
  )

  parsed <- safe_parse_urls(cases$input, url_standard = "whatwg")

  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_identical(parsed$host, cases$host)
  expect_identical(parsed$path, cases$path)
})

test_that("WHATWG file parser rejects forbidden decoded file hosts", {
  expect_identical(
    get_parse_status("file://%43%3A", url_standard = "whatwg"),
    "error"
  )
  expect_true(is.na(get_clean_url("file://%43%3A", url_standard = "whatwg")))
})
