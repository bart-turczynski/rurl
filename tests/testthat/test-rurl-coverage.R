test_that("safe_parse_urls handles empty and non-character inputs", {
  res_empty <- safe_parse_urls(character(0))
  expect_s3_class(res_empty, "data.frame")
  expect_equal(nrow(res_empty), 0)

  res_num <- safe_parse_urls(1)
  expect_equal(res_num$original_url, "1")
  expect_equal(res_num$parse_status, "error")
})

test_that("safe_parse_urls validates subdomain_levels_to_keep", {
  expect_error(
    safe_parse_urls("example.com", subdomain_levels_to_keep = -1),
    "non-negative"
  )
})

test_that("internal parse handles NA regex results", {
  ns <- asNamespace("stringi")
  orig <- get("stri_detect_regex", envir = ns)
  was_locked <- bindingIsLocked("stri_detect_regex", ns)
  if (was_locked) unlockBinding("stri_detect_regex", ns)
  withr::defer({
    assign("stri_detect_regex", orig, envir = ns)
    if (was_locked) lockBinding("stri_detect_regex", ns)
  }, testthat::teardown_env())

  assign("stri_detect_regex", function(string, pattern, ...) {
    if (identical(pattern, "^([a-zA-Z][a-zA-Z0-9+.-]*):\\\\/\\\\/")) {
      return(NA)
    }
    orig(string, pattern, ...)
  }, envir = ns)

  res <- rurl:::._safe_parse_url_impl(
    url = "example.com",
    protocol_handling = "keep",
    www_handling = "none",
    tld_source = "all",
    case_handling = "keep",
    trailing_slash_handling = "none",
    index_page_handling = "keep",
    path_normalization = "none",
    scheme_relative_handling = "keep",
    subdomain_levels_to_keep = NULL,
    host_encoding = "keep",
    path_encoding = "keep"
  )
  expect_true(is.list(res))
})

test_that("safe_parse_url handles NA from stringi ip detection", {
  ns <- asNamespace("stringi")
  orig <- get("stri_detect_regex", envir = ns)
  was_locked <- bindingIsLocked("stri_detect_regex", ns)
  if (was_locked) unlockBinding("stri_detect_regex", ns)
  withr::defer({
    assign("stri_detect_regex", orig, envir = ns)
    if (was_locked) lockBinding("stri_detect_regex", ns)
  }, testthat::teardown_env())

  assign("stri_detect_regex", function(string, pattern, ...) {
    if (identical(string, "127.0.0.1") &&
        pattern %in% c("^\\d{1,3}(\\.\\d{1,3}){3}$", "^\\[?[0-9a-fA-F:]+\\]?$")) {
      return(NA)
    }
    orig(string, pattern, ...)
  }, envir = ns)

  parsed <- safe_parse_url("http://127.0.0.1", www_handling = "none")
  expect_true(is.list(parsed))
})

test_that("safe_parse_url handles empty host from curl_parse_url", {
  ns <- asNamespace("curl")
  orig <- get("curl_parse_url", envir = ns)
  was_locked <- bindingIsLocked("curl_parse_url", ns)
  if (was_locked) unlockBinding("curl_parse_url", ns)
  withr::defer({
    assign("curl_parse_url", orig, envir = ns)
    if (was_locked) lockBinding("curl_parse_url", ns)
  }, testthat::teardown_env())

  assign("curl_parse_url", function(url, ...) {
    if (identical(url, "http://empty-host")) {
      return(list(scheme = "http", host = "", path = ""))
    }
    orig(url, ...)
  }, envir = ns)

  res_keep <- safe_parse_url("http://empty-host", www_handling = "keep")
  res_if_no <- safe_parse_url("http://empty-host", www_handling = "if_no_subdomain")
  expect_true(is.list(res_keep))
  expect_true(is.list(res_if_no))
})

test_that("safe_parse_url www_handling if_no_subdomain branches", {
  res_www <- safe_parse_url("http://www2.example.com", www_handling = "if_no_subdomain")
  expect_equal(res_www$host, "www.example.com")

  res_sub <- safe_parse_url("http://blog.example.com", www_handling = "if_no_subdomain")
  expect_equal(res_sub$host, "blog.example.com")

  res_unknown <- safe_parse_url("http://example.invalidtld", www_handling = "if_no_subdomain")
  expect_equal(res_unknown$host, "www.example.invalidtld")
})

test_that("internal path helpers cover edge cases", {
  expect_equal(rurl:::._collapse_path_slashes(NA_character_), NA_character_)
  expect_equal(rurl:::._collapse_path_slashes(""), "")
  expect_equal(rurl:::._collapse_path_slashes("/a//b///c"), "/a/b/c")

  expect_equal(rurl:::._remove_dot_segments("../a"), "a")
  expect_equal(rurl:::._remove_dot_segments("./a"), "a")
  expect_equal(rurl:::._remove_dot_segments("/./a"), "/a")
  expect_equal(rurl:::._remove_dot_segments("/."), "/")
  expect_equal(rurl:::._remove_dot_segments("/../a"), "/a")
  expect_equal(rurl:::._remove_dot_segments("/.."), "/")
  expect_equal(rurl:::._remove_dot_segments("."), "")
  expect_equal(rurl:::._remove_dot_segments(".."), "")

  expect_equal(rurl:::._strip_index_page(NA_character_), NA_character_)
  expect_equal(rurl:::._strip_index_page("foo/index.html"), "/foo")

  expect_equal(rurl:::._encode_path_segments(NA_character_), NA_character_)
  expect_equal(rurl:::._encode_path_segments("/a b/"), "/a%20b/")
})

test_that("punycode_to_unicode handles invalid decode", {
  ns <- asNamespace("urltools")
  orig <- get("puny_decode", envir = ns)
  was_locked <- bindingIsLocked("puny_decode", ns)
  if (was_locked) unlockBinding("puny_decode", ns)
  withr::defer({
    assign("puny_decode", orig, envir = ns)
    if (was_locked) lockBinding("puny_decode", ns)
  }, testthat::teardown_env())

  assign("puny_decode", function(x) NA_character_, envir = ns)

  res <- rurl:::.punycode_to_unicode("xn--invalid")
  expect_equal(res, "")
})

test_that("domain helpers handle edge cases", {
  expect_true(is.na(rurl:::._derive_domain_from_tld(NA_character_, "com")))
  expect_true(is.na(rurl:::._derive_domain_from_tld("example.com", NA_character_)))
  expect_true(is.na(rurl:::._derive_domain_from_tld("com", "com")))
  expect_true(is.na(rurl:::._derive_domain_from_tld("example.net", "com")))
  expect_true(is.na(rurl:::._derive_domain_from_tld(".com", "com")))
  expect_equal(rurl:::._derive_domain_from_tld("sub.example.com", "com"), "example.com")

  expect_true(is.na(rurl:::._extract_tld_impl(NA_character_, rurl:::.tld_all_set)))
})

test_that("query parser handles empty input", {
  expect_equal(rurl:::._parse_query_string(NA_character_), list())
})

test_that("getters return NA on parse failures", {
  expect_true(is.na(get_domain("http://127.0.0.1", source = "icann")))
  expect_true(is.na(get_query("mailto:x@example.com")))
  expect_equal(get_query("mailto:x@example.com", format = "list")[[1]], list())
  expect_true(is.na(get_fragment("mailto:x@example.com")))
  expect_true(is.na(get_port("mailto:x@example.com")))
  expect_true(is.na(get_port("http://example.com")))
  expect_true(is.na(get_user("mailto:x@example.com")))
  expect_true(is.na(get_password("mailto:x@example.com")))
  expect_true(is.na(get_userinfo("mailto:x@example.com")))
  expect_true(is.na(get_userinfo("http://example.com")))
  expect_true(is.na(unname(get_userinfo("http://user@example.com"))))
})

test_that("get_subdomain handles edge cases and formats", {
  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) {
      if (identical(url, "http://example.com")) {
        return(list(host = "example.com", domain = "example.com", tld = "com", is_ip_host = FALSE))
      }
      if (identical(url, "http://127.0.0.1")) {
        return(list(host = "127.0.0.1", domain = NA_character_, tld = NA_character_, is_ip_host = TRUE))
      }
      if (identical(url, "http://www.blog.example.co.uk")) {
        return(list(host = "www.blog.example.co.uk", domain = "example.co.uk", tld = "co.uk", is_ip_host = FALSE))
      }
      list(host = NA_character_, domain = NA_character_, tld = NA_character_, is_ip_host = FALSE)
    },
    .punycode_to_unicode = function(x) x,
    .env = asNamespace("rurl")
  )

  expect_true(is.na(get_subdomain("http://example.com")))
  expect_true(is.na(get_subdomain("http://127.0.0.1")))

  res_labels <- get_subdomain("http://www.blog.example.co.uk", source = "all", format = "labels")
  expect_true(is.list(res_labels))
  expect_equal(res_labels[[1]], "blog")
})

test_that("get_subdomain with non-all source uses derived domain", {
  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) {
      list(host = "blog.example.co.uk", domain = NA_character_, tld = "co.uk", is_ip_host = FALSE)
    },
    .punycode_to_unicode = function(x) x,
    ._derive_domain_from_tld = function(host_unicode, tld_unicode) "example.co.uk",
    .env = asNamespace("rurl")
  )

  res_labels <- get_subdomain("http://blog.example.co.uk", source = "icann", format = "labels")
  expect_true(is.list(res_labels))
  expect_equal(res_labels[[1]], "blog")
})

test_that("get_subdomain uses safe_parse_url values defensively", {
  ns <- asNamespace("rurl")
  orig <- get("safe_parse_url", envir = ns)
  was_locked <- bindingIsLocked("safe_parse_url", ns)
  if (was_locked) unlockBinding("safe_parse_url", ns)
  withr::defer({
    assign("safe_parse_url", orig, envir = ns)
    if (was_locked) lockBinding("safe_parse_url", ns)
  }, testthat::teardown_env())

  assign("safe_parse_url", function(url, ...) {
    list(host = NA_character_, domain = NA_character_, tld = NA_character_, is_ip_host = FALSE)
  }, envir = ns)
  expect_true(is.na(get_subdomain("http://example.com")))

  assign("safe_parse_url", function(url, ...) {
    list(host = ".example.com", domain = "example.com", tld = "com", is_ip_host = FALSE)
  }, envir = ns)
  expect_true(is.na(get_subdomain("http://example.com")))

  assign("safe_parse_url", function(url, ...) {
    list(host = "example.com", domain = NA_character_, tld = NA_character_, is_ip_host = FALSE)
  }, envir = ns)
  expect_true(is.na(get_subdomain("http://example.com", source = "icann")))
})
