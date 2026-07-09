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

test_that("legacy and RFC file forms remain libcurl-scoped", {
  urls <- c("file://example.com/path", "file:///c:/Windows/System32")

  expect_identical(get_parse_status(urls), c("error", "error"))
  expect_identical(
    get_parse_status(urls, url_standard = "rfc3986"), c("error", "error")
  )
  expect_true(all(is.na(get_clean_url(urls))))
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
