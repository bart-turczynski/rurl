test_that("safe_parse_urls handles empty and non-character inputs", {
  res_empty <- safe_parse_urls(character(0))
  expect_s3_class(res_empty, "data.frame")
  expect_equal(nrow(res_empty), 0)

  res_num <- safe_parse_urls(1)
  expect_equal(res_num$original_url, "1")
  expect_equal(res_num$parse_status, "error")
})

test_that("safe_parse_urls accepts factor input by coercing to labels", {
  # Factor input is coerced to its character labels up front, so it parses
  # identically to the equivalent character vector rather than yielding an
  # all-error row (regression: factors previously fell through as non-char).
  f <- factor(c("http://example.com", "https://www.example.com/path"))
  res_factor <- safe_parse_urls(f)
  res_char <- safe_parse_urls(as.character(f))
  expect_equal(res_factor, res_char)
  expect_equal(res_factor$parse_status, c("ok", "ok"))
  expect_equal(res_factor$original_url, as.character(f))
})

test_that("safe_parse_urls validates subdomain_levels_to_keep", {
  expect_error(
    safe_parse_urls("example.com", subdomain_levels_to_keep = -1),
    "non-negative"
  )
  expect_error(
    safe_parse_urls("example.com", subdomain_levels_to_keep = c(1, 2)),
    "non-negative integer"
  )
  expect_error(
    safe_parse_urls("example.com", subdomain_levels_to_keep = NA_real_),
    "non-negative integer"
  )
  expect_error(
    safe_parse_urls("example.com", subdomain_levels_to_keep = 1.5),
    "non-negative integer"
  )
  expect_no_error(
    safe_parse_urls("example.com", subdomain_levels_to_keep = 1)
  )
})

test_that("internal parse handles NA regex results", {
  ns <- asNamespace("stringi")
  orig <- get("stri_detect_regex", envir = ns)
  was_locked <- bindingIsLocked("stri_detect_regex", ns)
  if (was_locked) unlockBinding("stri_detect_regex", ns)
  withr::defer(
    {
      assign("stri_detect_regex", orig, envir = ns)
      if (was_locked) lockBinding("stri_detect_regex", ns)
    }
  )

  assign("stri_detect_regex", function(string, pattern, ...) {
    if (identical(pattern, "^([a-zA-Z][a-zA-Z0-9+.-]*):\\/\\/")) {
      return(NA)
    }
    if (identical(pattern, "^[^/]+:[0-9]+($|/)")) {
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
  expect_type(res, "list")

  res2 <- rurl:::._safe_parse_url_impl(
    url = "foo:123",
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
  expect_true(is.null(res2) || is.list(res2))
})

test_that("safe_parse_url handles NA from stringi ip detection", {
  ns <- asNamespace("stringi")
  orig <- get("stri_detect_regex", envir = ns)
  was_locked <- bindingIsLocked("stri_detect_regex", ns)
  if (was_locked) unlockBinding("stri_detect_regex", ns)
  withr::defer(
    {
      assign("stri_detect_regex", orig, envir = ns)
      if (was_locked) lockBinding("stri_detect_regex", ns)
    }
  )

  assign("stri_detect_regex", function(string, pattern, ...) {
    if (identical(string, "127.0.0.1") &&
      pattern %in% c(
        "^\\d{1,3}(\\.\\d{1,3}){3}$",
        "^\\[?[0-9a-fA-F:]+\\]?$",
        ":"
      )) {
      return(NA)
    }
    orig(string, pattern, ...)
  }, envir = ns)

  parsed <- safe_parse_url("http://127.0.0.1", www_handling = "none")
  expect_type(parsed, "list")
})

test_that("safe_parse_url handles empty host from curl_parse_url", {
  ns <- asNamespace("curl")
  orig <- get("curl_parse_url", envir = ns)
  was_locked <- bindingIsLocked("curl_parse_url", ns)
  if (was_locked) unlockBinding("curl_parse_url", ns)
  withr::defer(
    {
      assign("curl_parse_url", orig, envir = ns)
      if (was_locked) lockBinding("curl_parse_url", ns)
    }
  )

  assign("curl_parse_url", function(url, ...) {
    if (identical(url, "http://empty-host")) {
      return(list(scheme = "http", host = "", path = ""))
    }
    orig(url, ...)
  }, envir = ns)

  res_keep <- safe_parse_url("http://empty-host", www_handling = "keep")
  res_if_no <- safe_parse_url(
    "http://empty-host",
    www_handling = "if_no_subdomain"
  )
  expect_type(res_keep, "list")
  expect_type(res_if_no, "list")
})

test_that("safe_parse_url www_handling if_no_subdomain branches", {
  res_www <- safe_parse_url(
    "http://www2.example.com",
    www_handling = "if_no_subdomain"
  )
  expect_equal(res_www$host, "www.example.com")

  res_sub <- safe_parse_url(
    "http://blog.example.com",
    www_handling = "if_no_subdomain"
  )
  expect_equal(res_sub$host, "blog.example.com")

  res_unknown <- safe_parse_url(
    "http://example.invalidtld",
    www_handling = "if_no_subdomain"
  )
  expect_equal(res_unknown$host, "example.invalidtld")
})

test_that(
  "safe_parse_url if_no_subdomain keeps candidate host when domain is unknown",
  {
    testthat::local_mocked_bindings(
      .psl_suffix_extract = function(host, section = "all", engine = NULL) {
        data.frame(
          host = host,
          subdomain = NA_character_,
          domain = NA_character_,
          suffix = NA_character_,
          registrable_domain = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    )

    # The full-parse cache may already hold "http://example.com" from an earlier
    # test (resolved with the real PSL); clear it so the mock is exercised.
    rurl_clear_caches()
    res <- safe_parse_url(
      "http://example.com",
      www_handling = "if_no_subdomain"
    )
    expect_equal(res$host, "example.com")
  }
)

test_that("internal path helpers cover edge cases", {
  expect_equal(rurl:::._collapse_path_slashes(NA_character_), NA_character_)
  expect_equal(rurl:::._collapse_path_slashes(""), "")
  expect_equal(rurl:::._collapse_path_slashes("/a//b///c"), "/a/b/c")

  expect_equal(rurl:::._remove_dot_segments(NA_character_), NA_character_)
  expect_equal(rurl:::._remove_dot_segments(""), "")
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

test_that("punycode_to_unicode never errors on malformed A-labels", {
  # The helper is contractually total: it returns a single string for any
  # input, falling back to the original label when a part cannot be decoded.
  res <- rurl:::.punycode_to_unicode("xn--invalid")
  expect_type(res, "character")
  expect_length(res, 1L)
  expect_false(is.na(res))
})

test_that("punycode_to_unicode returns empty string for empty input", {
  # Empty (but non-NA) input short-circuits to "" before any decode attempt.
  expect_identical(rurl:::.punycode_to_unicode(""), "")
})

test_that(".host_is_ace returns FALSE for NA, empty, and non-scalar hosts", {
  expect_false(rurl:::.host_is_ace(NA_character_))
  expect_false(rurl:::.host_is_ace(""))
  expect_false(rurl:::.host_is_ace(character(0)))
  expect_false(rurl:::.host_is_ace(c("xn--a", "xn--b")))
  # Sanity: a genuine A-label host still reports TRUE.
  expect_true(rurl:::.host_is_ace("xn--mnchen-3ya.de"))
})

test_that(".cache_enabled returns FALSE for an unknown cache name", {
  expect_false(rurl:::.cache_enabled("not-a-real-cache"))
})

test_that("derive_parse_status coerces NA host-has-dot to no-TLD warning", {
  # stri_detect_fixed(final_host, ".") can in principle return NA; the
  # defensive guard coerces that to FALSE so the host is treated as dotless
  # (warning-no-tld) instead of erroring on `if (NA)`.
  ns <- asNamespace("stringi")
  orig <- get("stri_detect_fixed", envir = ns)
  was_locked <- bindingIsLocked("stri_detect_fixed", ns)
  if (was_locked) unlockBinding("stri_detect_fixed", ns)
  withr::defer(
    {
      assign("stri_detect_fixed", orig, envir = ns)
      if (was_locked) lockBinding("stri_detect_fixed", ns)
    }
  )

  assign("stri_detect_fixed", function(str, pattern, ...) {
    if (identical(str, "example.com") && identical(pattern, ".")) {
      return(NA)
    }
    orig(str, pattern, ...)
  }, envir = ns)

  rurl_clear_caches()
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
  expect_equal(res$parse_status, rurl:::.STATUS_WARN_NO_TLD)
})

# Note: .psl_registered_domain / .psl_public_suffix edge cases are covered in
# test-accessors.R. They are deliberately NOT tested here because the
# if_no_subdomain mock above invokes the mocked binding through safe_parse_url,
# which leaves the mock installed for the rest of this file (a known testthat
# local_mocked_bindings teardown quirk with indirect invocation).

test_that("query parser handles empty input", {
  expect_equal(rurl:::._parse_query_string(NA_character_), list())
  parsed <- rurl:::._parse_query_string("a=1&&b=2")
  expect_equal(parsed$a, "1")
  expect_equal(parsed$b, "2")
})

test_that("query parser groups repeated keys in first-seen order (linear)", {
  # Repeated keys collect their values in occurrence order.
  expect_equal(rurl:::._parse_query_string("x=1&x=2"), list(x = c("1", "2")))
  # first-seen key order preserved even when a repeat is interleaved.
  expect_equal(
    rurl:::._parse_query_string("b=1&a=2&b=3"),
    list(b = c("1", "3"), a = "2")
  )
})

test_that("query parser preserves '=' split and empty-value semantics", {
  # Value re-joins the tail with "=" (strsplit fixed=TRUE trailing-empty drop).
  expect_equal(rurl:::._parse_query_string("a=b=c"), list(a = "b=c"))
  expect_equal(rurl:::._parse_query_string("a==b"), list(a = "=b"))
  # A bare flag and a trailing "=" both yield an empty value.
  expect_equal(rurl:::._parse_query_string("flag"), list(flag = ""))
  expect_equal(rurl:::._parse_query_string("x="), list(x = ""))
  expect_equal(rurl:::._parse_query_string("x=="), list(x = ""))
  expect_equal(rurl:::._parse_query_string(""), list())
})

test_that("query parser percent-decodes both sides unless decode = FALSE", {
  decoded <- rurl:::._parse_query_string("%20=%3D", decode = TRUE)
  expect_named(decoded, " ")
  expect_equal(decoded[[1]], "=")
  raw <- rurl:::._parse_query_string("%20=%3D", decode = FALSE)
  expect_named(raw, "%20")
  expect_equal(raw[[1]], "%3D")
  # A lone "%" is tolerated (not an error) and left as-is.
  expect_equal(rurl:::._parse_query_string("%"), list(`%` = ""))
})

test_that("encode_path_segments handles empty and multi-segment paths", {
  # Empty-segment behavior: "" stays "", character(0) recomposes to "".
  expect_equal(rurl:::._encode_path_segments(""), "")
  expect_equal(rurl:::._encode_path_segments("/"), "/")
  # Every segment is escaped by the single vectorized curl_escape() call.
  expect_equal(rurl:::._encode_path_segments("/a b/c d"), "/a%20b/c%20d")
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
  user_only <- unname(get_userinfo("http://user@example.com"))
  expect_true(is.na(user_only) || identical(user_only, "user"))
})

test_that("getters return NA when safe_parse_url is NULL", {
  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) NULL
  )

  expect_true(is.na(get_query("http://example.com")))
  expect_equal(get_query("http://example.com", format = "list")[[1]], list())
  expect_true(is.na(get_fragment("http://example.com")))
  expect_true(is.na(get_port("http://example.com")))
  expect_true(is.na(get_user("http://example.com")))
  expect_true(is.na(get_password("http://example.com")))
  expect_true(is.na(get_userinfo("http://example.com")))
})

test_that("get_userinfo returns user when password is missing", {
  # Real userinfo with a user and no password. (Previously mocked
  # safe_parse_url; accessors now route through the vector engine.)
  expect_equal(
    get_userinfo("ftp://alice@ftp.example.com/file.txt"),
    "alice"
  )
})

test_that("get_domain returns NA for ip hosts in non-all source", {
  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) list(is_ip_host = TRUE)
  )
  expect_true(is.na(get_domain("http://127.0.0.1", source = "icann")))
})

test_that("get_subdomain handles edge cases and formats", {
  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) {
      if (identical(url, "http://example.com")) {
        return(list(
          host = "example.com",
          domain = "example.com",
          tld = "com",
          is_ip_host = FALSE
        ))
      }
      if (identical(url, "http://127.0.0.1")) {
        return(list(
          host = "127.0.0.1",
          domain = NA_character_,
          tld = NA_character_,
          is_ip_host = TRUE
        ))
      }
      if (identical(url, "http://www.blog.example.co.uk")) {
        return(list(
          host = "www.blog.example.co.uk",
          domain = "example.co.uk",
          tld = "co.uk",
          is_ip_host = FALSE
        ))
      }
      list(
        host = NA_character_,
        domain = NA_character_,
        tld = NA_character_,
        is_ip_host = FALSE
      )
    },
    .punycode_to_unicode = function(x) x
  )

  expect_true(is.na(get_subdomain("http://example.com")))
  expect_true(is.na(get_subdomain("http://127.0.0.1")))

  res_labels <- get_subdomain(
    "http://www.blog.example.co.uk",
    source = "all",
    format = "labels"
  )
  expect_type(res_labels, "list")
  expect_equal(res_labels[[1]], c("www", "blog"))
})

test_that("get_subdomain with non-all source uses the parsed domain", {
  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) {
      list(
        host = "blog.example.co.uk",
        domain = "example.co.uk",
        tld = "co.uk",
        is_ip_host = FALSE
      )
    },
    .punycode_to_unicode = function(x) x
  )

  res_labels <- get_subdomain(
    "http://blog.example.co.uk",
    source = "icann",
    format = "labels"
  )
  expect_type(res_labels, "list")
  expect_equal(res_labels[[1]], "blog")
})

test_that("get_subdomain returns NA when domain is missing", {
  # An unknown TLD yields a NA registered domain (pslr unknown = "na"), so the
  # suffix strip has nothing to match and the subdomain is NA. (Previously
  # mocked safe_parse_url; accessors now route through the vector engine.)
  expect_true(
    is.na(get_subdomain("http://foo.example.invalidtldxyz", source = "all"))
  )
})

test_that("get_subdomain uses safe_parse_url values defensively", {
  # Use local_mocked_bindings (like the tests above) rather than a manual
  # assign()/defer into the namespace: the manual mock previously deferred its
  # restore to testthat::teardown_env(), which leaves the stub installed for
  # every LATER test file. Any file that calls safe_parse_url() directly (e.g.
  # test-url-standard-scaffold.R) then saw the stub instead of the real engine.
  # local_mocked_bindings restores at the end of THIS test_that. Re-calling it
  # re-points the binding for the remaining assertions.
  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) {
      list(
        host = NA_character_, domain = NA_character_,
        tld = NA_character_, is_ip_host = FALSE
      )
    }
  )
  expect_true(is.na(get_subdomain("http://example.com")))

  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) {
      list(
        host = ".example.com", domain = "example.com",
        tld = "com", is_ip_host = FALSE
      )
    }
  )
  expect_true(is.na(get_subdomain("http://example.com")))

  testthat::local_mocked_bindings(
    safe_parse_url = function(url, ...) {
      list(
        host = "example.com", domain = NA_character_,
        tld = NA_character_, is_ip_host = FALSE
      )
    }
  )
  expect_true(is.na(get_subdomain("http://example.com", source = "icann")))
})
