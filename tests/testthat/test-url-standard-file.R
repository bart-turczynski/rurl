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

test_that("unsupported file forms remain outside this slice", {
  # libcurl rejects non-local file hosts and Windows drive-letter paths in this
  # build. RURL-rutsdflg intentionally ships the authority/empty-host forms
  # needed by the cross-parser benchmark and leaves a full file-state machine
  # for a later scoped change.
  urls <- c("file://example.com/path", "file:///c:/Windows/System32")

  expect_identical(
    get_parse_status(urls, url_standard = "whatwg"), c("error", "error")
  )
  expect_true(all(is.na(get_clean_url(urls, url_standard = "whatwg"))))
})
