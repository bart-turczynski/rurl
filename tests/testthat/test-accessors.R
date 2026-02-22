test_that("get_clean_url returns expected values", {
  urls <- c("example.com", "http://test.com/page")
  expect_equal(
    unname(get_clean_url(urls)),
    c("http://example.com/", "http://test.com/page")
  )
})

test_that("get_clean_url preserves host casing by default", {
  expect_equal(
    unname(get_clean_url("Http://Example.Com/MyPath/")),
    "http://Example.Com/MyPath/"
  )
})

test_that("get_clean_url handles host:port without explicit scheme", {
  expect_equal(
    unname(get_clean_url(
      "example.com:8080/path",
      trailing_slash_handling = "keep"
    )),
    "http://example.com/path/"
  )
})

test_that("get_domain works with subdomains", {
  expect_equal(unname(get_domain("https://sub.example.co.uk")), "example.co.uk")
  expect_equal(unname(get_domain("http://localhost")), NA_character_)
})

test_that("get_scheme respects protocol_handling", {
  expect_equal(unname(get_scheme("https://x.com")), "https")
  expect_equal(unname(get_scheme("x.com", "http")), "http")
  expect_equal(unname(get_scheme("x.com", "none")), NA_character_)
})

test_that("get_parse_status returns 'ok' or 'error'", {
  expect_equal(unname(get_parse_status("example.com")), "ok")
  expect_equal(unname(get_parse_status("not a url")), "error")
})

test_that("get_parse_status flags ftp correctly", {
  expect_equal(unname(get_parse_status("ftp://example.com")), "ok-ftp")
})

test_that("get_parse_status validates allowed schemes only", {
  expect_equal(unname(get_parse_status("http://example.com")), "ok")
  expect_equal(unname(get_parse_status("https://example.com")), "ok")
  expect_equal(unname(get_parse_status("ftp://example.com")), "ok-ftp")
  expect_equal(unname(get_parse_status("ftps://example.com")), "ok-ftp")
})

test_that("get_parse_status rejects unsupported or malformed schemes", {
  expect_equal(unname(get_parse_status("mailto:x@example.com")), "error")
  expect_equal(unname(get_parse_status("file:///home/user/file.txt")), "error")
  expect_equal(unname(get_parse_status("ftp:example.com")), "error")
  expect_equal(unname(get_parse_status("s3://bucket-name")), "error")
  expect_equal(unname(get_parse_status("data:text/plain,hello")), "error")
  expect_equal(unname(get_parse_status("htp://fake.com")), "error")
})

test_that("safe_parse_url returns NULL for NA, non-character, or empty input", {
  expect_null(safe_parse_url(NA_character_))
  expect_null(safe_parse_url(""))
  expect_null(safe_parse_url(12345))
})

test_that("safe_parse_url enforces scalar input", {
  expect_error(
    safe_parse_url(c("example.com", "test.com")),
    "single URL"
  )
})

test_that("safe_parse_urls returns a data.frame with expected fields", {
  res <- safe_parse_urls(c("example.com", "//example.org/path", NA_character_))
  expect_s3_class(res, "data.frame")
  expect_true(all(c(
    "original_url", "host", "clean_url", "parse_status"
  ) %in% names(res)))
  expect_equal(res$host[1], "example.com")
  expect_equal(res$clean_url[2], "http://example.org/path")
  expect_equal(res$parse_status[3], "error")
})

test_that("safe_parse_url handles protocol-relative URLs", {
  res <- safe_parse_url("//example.com/path")
  expect_equal(res$clean_url, "http://example.com/path")
  expect_true(is.na(res$scheme))
  expect_equal(res$parse_status, "ok-scheme-relative")
})

test_that(
  "get_parse_status returns error for parseable but unsupported schemes",
  {
  expect_equal(unname(get_parse_status("ws://example.com")), "error")
})

test_that("get_parse_status detects no-TLD hosts", {
  expect_equal(unname(get_parse_status("http://just-a-path")), "warning-no-tld")
  expect_equal(unname(get_parse_status("http://localhost")), "warning-no-tld")
  expect_equal(unname(get_parse_status("http://example.com")), "ok")
  expect_equal(
    unname(get_parse_status("http://example.invalidtld")),
    "warning-invalid-tld"
  )
  expect_equal(
    unname(get_parse_status("http://co.uk")),
    "warning-public-suffix"
  )
})

test_that(
  "get_clean_url returns NA if parsed is NULL or host/path is missing",
  {
  expect_true(is.na(get_clean_url("mailto:user@example.com")))
  expect_true(is.na(get_clean_url("")))
})

test_that("get_domain returns NA when parsed or host is missing", {
  expect_true(is.na(get_domain("mailto:user@example.com")))
  expect_true(is.na(get_domain("http:///nohost")))
  expect_true(is.na(get_domain("not a url")))
})

test_that("get_scheme returns NA when parsing fails", {
  expect_true(is.na(get_scheme("mailto:user@example.com")))
  expect_true(is.na(get_scheme("not a url")))
})

test_that("get_host extracts host or returns NA", {
  expect_equal(unname(get_host("http://example.com/path")), "example.com")
  expect_equal(unname(get_host("https://sub.domain.org/")), "sub.domain.org")
  expect_equal(
    unname(get_host("Http://User:Pass@MyHost.Com:8080/SomeWhere")),
    "myhost.com"
  )
  expect_true(is.na(get_host("mailto:user@example.com")))
  expect_true(is.na(get_host("not a url")))
})

test_that("get_host respects case_handling", {
  expect_equal(
    unname(get_host("http://Example.Com", case_handling = "upper")),
    "EXAMPLE.COM"
  )
})

test_that("get_path extracts path or returns NA", {
  expect_equal(unname(get_path("http://example.com/test")), "/test")
  expect_equal(unname(get_path("https://x.org/hello/world")), "/hello/world")
  expect_equal(unname(get_path("HTTP://EXAMPLE.NET/A/B/C/?p=1")), "/a/b/c/")
  expect_true(is.na(get_path("mailto:user@example.com")))
  expect_true(is.na(get_path("not a url")))
})

test_that("get_path respects case_handling", {
  expect_equal(
    unname(get_path("http://example.com/MyPath", case_handling = "keep")),
    "/MyPath"
  )
})

test_that("get_query returns string or parsed list", {
  expect_equal(unname(get_query("http://example.com/path?a=1&b=2")), "a=1&b=2")
  parsed <- get_query("http://example.com/path?a=1&a=2&b=3", format = "list")
  expect_true(is.list(parsed))
  expect_equal(parsed[[1]]$a, c("1", "2"))
  expect_equal(parsed[[1]]$b, "3")
})

test_that("get_fragment, get_port, get_user, get_password, get_userinfo work", {
  expect_equal(unname(get_fragment("http://example.com/path#frag")), "frag")
  expect_equal(unname(get_port("http://example.com:8080/path")), 8080L)
  expect_equal(unname(get_user("http://user:pass@example.com")), "user")
  expect_equal(unname(get_password("http://user:pass@example.com")), "pass")
  expect_equal(
    unname(get_userinfo("http://user:pass@example.com")),
    "user:pass"
  )
})

test_that("get_domain respects PSL source options", {
  expect_equal(
    unname(get_domain("http://sub.blogspot.com", source = "all")),
    "sub.blogspot.com"
  )
  expect_equal(
    unname(get_domain("http://sub.blogspot.com", source = "private")),
    "sub.blogspot.com"
  )
  expect_equal(
    unname(get_domain("http://sub.blogspot.com", source = "icann")),
    "blogspot.com"
  )
})

test_that("get_subdomain returns expected values", {
  expect_equal(
    unname(get_subdomain("http://www.blog.example.co.uk")),
    "www.blog"
  )
  expect_equal(
    unname(get_subdomain("http://www.blog.example.co.uk", include_www = TRUE)),
    "www.blog"
  )
  expect_true(is.na(unname(get_subdomain("http://www.example.co.uk"))))
  expect_equal(
    unname(get_subdomain("http://www.example.co.uk", include_www = TRUE)),
    "www"
  )
  labels <- get_subdomain("http://www.blog.example.co.uk", format = "labels")
  expect_equal(labels[[1]], c("www", "blog"))
})

test_that(".get_registered_domain handles known cases correctly", {
  expect_equal(rurl:::.get_registered_domain("example.com"), "example.com")
  expect_equal(
    rurl:::.get_registered_domain("sub.example.co.uk"),
    "example.co.uk"
  )
  expect_equal(
    rurl:::.get_registered_domain("sub.dev-builder.code.com"),
    NA_character_
  )
  expect_equal(
    rurl:::.get_registered_domain("city.kawasaki.jp"),
    "city.kawasaki.jp"
  )
  expect_equal(
    rurl:::.get_registered_domain("foo.bar.city.kawasaki.jp"),
    "city.kawasaki.jp"
  )
  expect_equal(rurl:::.get_registered_domain("unknown.tld"), NA_character_)
  expect_equal(rurl:::.get_registered_domain("localhost"), NA_character_)
})

test_that(".get_registered_domain handles suffix-only domains", {
  expect_true(is.na(rurl:::.get_registered_domain("com")))
  expect_true(is.na(rurl:::.get_registered_domain("co.uk")))
})

test_that("get_parse_status falls through to final error", {
  mock_parse <- function(...) list(scheme = "gopher", host = "example.com")
  ns <- asNamespace("rurl")
  original <- get("safe_parse_url", envir = ns)
  unlockBinding("safe_parse_url", ns)
  assign("safe_parse_url", mock_parse, envir = ns)
  lockBinding("safe_parse_url", ns)
  on.exit({
    unlockBinding("safe_parse_url", ns)
    assign("safe_parse_url", original, envir = ns)
    lockBinding("safe_parse_url", ns)
  })
  expect_identical(unname(get_parse_status("whatever")), "error")
})

test_that("get_tld matches correct TLDs across sources", {
  # Simple domains
  expect_identical(unname(get_tld("sub.pl")), "pl")
  expect_identical(unname(get_tld("sub.com.pl")), "com.pl")
  expect_identical(unname(get_tld("sub.google")), "google")

  # Blogspot (private)
  expect_identical(
    unname(get_tld("sub.blogspot.com", source = "all")),
    "blogspot.com"
  )
  expect_identical(
    unname(get_tld("sub.blogspot.com", source = "private")),
    "blogspot.com"
  )
  expect_identical(unname(get_tld("sub.blogspot.com", source = "icann")), "com")

  # Wordpress (not a PSL private domain)
  expect_identical(unname(get_tld("sub.wordpress.com", source = "all")), "com")
  expect_identical(
    unname(get_tld("sub.wordpress.com", source = "private")),
    NA_character_
  )
  expect_identical(
    unname(get_tld("sub.wordpress.com", source = "icann")),
    "com"
  )

  # Warszawa.pl (ICANN only)
  expect_identical(
    unname(get_tld("sub.warszawa.pl", source = "all")),
    "warszawa.pl"
  )
  expect_identical(
    unname(get_tld("sub.warszawa.pl", source = "private")),
    NA_character_
  )
  expect_identical(
    unname(get_tld("sub.warszawa.pl", source = "icann")),
    "warszawa.pl"
  )

  # Deep subdomains
  expect_identical(
    unname(get_tld("sub.sub.warszawa.pl", source = "all")),
    "warszawa.pl"
  )
  expect_identical(
    unname(get_tld("sub.sub.warszawa.pl", source = "private")),
    NA_character_
  )
  expect_identical(
    unname(get_tld("sub.sub.warszawa.pl", source = "icann")),
    "warszawa.pl"
  )
})

test_that("get_tld handles NA and edge cases", {
  expect_identical(unname(get_tld(NA_character_)), NA_character_)
  expect_identical(unname(get_tld("not a url")), NA_character_)
  expect_identical(unname(get_tld("http://localhost")), NA_character_)
  expect_identical(unname(get_tld("")), NA_character_)
  expect_identical(
    unname(get_tld("ftp://example.com")),
    "com"
  ) # Non-http scheme
  expect_identical(
    unname(get_tld("http://192.168.1.1")),
    NA_character_
  ) # IP address
  expect_identical(
    unname(get_tld("http://example.")),
    NA_character_
  ) # Trailing dot
})

test_that("get_tld handles internationalized domain names (IDNs)", {
  expect_identical(unname(get_tld("石川.jp")), "石川.jp")
  expect_identical(unname(get_tld("münchen.de")), "de")
  expect_identical(unname(get_tld("παράδειγμα.ελ")), "ελ")
  expect_identical(unname(get_tld("xn--hxajbheg2az3al.xn--qxam")), "ελ")
  expect_identical(unname(get_tld("中国.中国")), "中国")
  expect_identical(unname(get_tld("xn--fiqs8s.xn--fiqs8s")), "中国")
})

test_that("get_tld handles edge cases and unexpected inputs gracefully", {
  expect_identical(unname(get_tld(NA_character_)), NA_character_)
  expect_identical(unname(get_tld("")), NA_character_)
  expect_identical(unname(get_tld("not a url")), NA_character_)
  expect_identical(unname(get_tld("http://localhost")), NA_character_)
  expect_identical(unname(get_tld("http://example.")), NA_character_)
  expect_identical(unname(get_tld("http://192.168.1.1")), NA_character_)
  expect_identical(unname(get_tld("ftp://example.com")), "com")

  # IDN edge cases
  expect_identical(unname(get_tld("xn--fiqs8s.xn--fiqs8s")), "中国")
  expect_identical(unname(get_tld("中国.中国")), "中国")
  expect_identical(unname(get_tld("xn--hxajbheg2az3al.xn--qxam")), "ελ")
  expect_identical(unname(get_tld("παράδειγμα.ελ")), "ελ")

  # Weird but valid cases
  expect_identical(unname(get_tld(".com")), NA_character_)
  expect_true(is.na(rurl:::.get_registered_domain(".com")))
  expect_identical(unname(get_tld("example")), NA_character_)
  expect_identical(unname(get_tld("example..com")), NA_character_)
})

test_that(
  ".normalize_and_punycode handles Unicode normalization and punycode encoding",
  {
  expect_match(
    unname(rurl:::.normalize_and_punycode("παράδειγμα.ελ")),
    "^xn--"
  )
  expect_identical(
    unname(rurl:::.normalize_and_punycode("ascii-only.com")),
    "ascii-only.com"
  )
})

test_that(".normalize_and_punycode returns NA_character_ on error", {
  fail_encode <- function(x) stop("fail")

  result <- rurl:::.normalize_and_punycode(
    "παράδειγμα.ελ",
    encode_fn = fail_encode
  )
  expect_identical(unname(result), NA_character_)
})

test_that(".normalize_and_punycode handles NA and empty input", {
  expect_identical(
    unname(rurl:::.normalize_and_punycode(NA_character_)),
    NA_character_
  )
  expect_identical(unname(rurl:::.normalize_and_punycode("")), "")
})

test_that("safe_parse_url handles www_handling options correctly", {
  # www_handling = "strip"
  expect_equal(
    safe_parse_url("http://www.example.com", www_handling = "strip")$host,
    "example.com"
  )
  expect_equal(
    safe_parse_url("http://www123.example.com", www_handling = "strip")$host,
    "example.com"
  )
  expect_equal(
    safe_parse_url("http://example.com", www_handling = "strip")$host,
    "example.com"
  ) # No www to strip
  # Test with IP, strip should not apply
  expect_equal(
    safe_parse_url("http://1.2.3.4", www_handling = "strip")$host,
    "1.2.3.4"
  )

  # www_handling = "keep"
  expect_equal(
    safe_parse_url("http://example.com", www_handling = "keep")$host,
    "www.example.com"
  )
  expect_equal(
    safe_parse_url("http://www123.example.com", www_handling = "keep")$host,
    "www.example.com"
  )
  expect_equal(
    safe_parse_url("http://www.example.com", www_handling = "keep")$host,
    "www.example.com"
  )
  # Test with empty raw_host for keep - should remain empty or NA
  # safe_parse_url with an effectively empty host after protocol strip might be
  # tricky to set up directly
  # Let's test the helper directly if possible, or ensure this path is hit.
  # For safe_parse_url, if the input is just "http://", parsed_curl$host might
  # be NA or empty.
  # If input is "", it returns NULL early. If input is "http://", host is NA.
  expect_true(is.null(safe_parse_url("http://", www_handling = "keep")$host))
  # Test with IP, keep should not apply
  expect_equal(
    safe_parse_url("http://1.2.3.4", www_handling = "keep")$host,
    "1.2.3.4"
  )

  # www_handling = "none" (default, for completeness)
  expect_equal(
    safe_parse_url("http://www.example.com", www_handling = "none")$host,
    "www.example.com"
  )
  expect_equal(
    safe_parse_url("http://www123.example.com", www_handling = "none")$host,
    "www123.example.com"
  )
  expect_equal(
    safe_parse_url("http://example.com", www_handling = "none")$host,
    "example.com"
  )
})

test_that("case_handling lower_host keeps path casing", {
  res <- safe_parse_url(
    "http://Example.COM/Some/Path/",
    case_handling = "lower_host",
    path_encoding = "keep"
  )
  expect_equal(res$clean_url, "http://example.com/Some/Path/")
})

test_that("path_encoding decode and encode work as expected", {
  res_decode <- safe_parse_url(
    "http://example.com/a%20b",
    path_encoding = "decode",
    case_handling = "keep"
  )
  expect_equal(res_decode$path, "/a b")
  expect_equal(res_decode$clean_url, "http://example.com/a b")

  res_encode <- safe_parse_url(
    "http://example.com/a%20b",
    path_encoding = "encode",
    case_handling = "keep"
  )
  expect_equal(res_encode$path, "/a%20b")
  expect_equal(res_encode$clean_url, "http://example.com/a%20b")
})

test_that("path_normalization collapses slashes and resolves dot segments", {
  res_slashes <- safe_parse_url(
    "http://example.com//a///b",
    path_normalization = "collapse_slashes",
    case_handling = "keep"
  )
  expect_equal(res_slashes$path, "/a/b")

  res_dot <- safe_parse_url(
    "http://example.com/a/b/../c",
    path_normalization = "dot_segments",
    case_handling = "keep"
  )
  expect_equal(res_dot$path, "/a/c")
})

test_that("index_page_handling strips index/default pages", {
  res_strip <- safe_parse_url(
    "http://example.com/index.html",
    index_page_handling = "strip",
    trailing_slash_handling = "strip"
  )
  expect_equal(res_strip$path, "/")
  expect_equal(res_strip$clean_url, "http://example.com")

  res_strip_keep <- safe_parse_url(
    "http://example.com/index.html/",
    index_page_handling = "strip",
    trailing_slash_handling = "keep"
  )
  expect_equal(res_strip_keep$path, "/")
  expect_equal(res_strip_keep$clean_url, "http://example.com/")
})

test_that("trailing_slash_handling strips root slash in clean_url", {
  res_strip <- safe_parse_url(
    "http://example.com",
    trailing_slash_handling = "strip"
  )
  expect_equal(res_strip$path, "/")
  expect_equal(res_strip$clean_url, "http://example.com")
})

test_that("scheme_relative_handling https forces https output", {
  res_https <- safe_parse_url(
    "//example.com/path",
    scheme_relative_handling = "https"
  )
  expect_equal(res_https$scheme, "https")
  expect_equal(res_https$clean_url, "https://example.com/path")
})

test_that("scheme_relative_handling error rejects scheme-relative URLs", {
  expect_null(safe_parse_url(
    "//example.com/path",
    scheme_relative_handling = "error"
  ))
})

test_that("host_encoding handles IDNA round-trips", {
  res_idna <- safe_parse_url(
    "http://münich.com/path",
    host_encoding = "idna",
    case_handling = "keep"
  )
  expect_match(res_idna$clean_url, "xn--")

  res_unicode <- safe_parse_url(
    "http://xn--mnich-kva.com/path",
    host_encoding = "unicode",
    case_handling = "keep"
  )
  expect_equal(res_unicode$clean_url, "http://münich.com/path")
})

test_that("safe_parse_url handles specific error conditions", {
  # Test for line 187: original_looks_like_protocol &&
  # !original_has_allowed_scheme
  # This condition should lead to parse_status = "error" (and NULL return from
  # safe_parse_url itself)
  # Assuming gopher is not in allowed_prefixes.
  expect_null(unname(safe_parse_url("gopher://example.com")))
  # To be more robust, let's check parse_status via get_parse_status for this
  # case
  expect_equal((unname(get_parse_status("gopher://example.com"))), "error")
})

test_that(".punycode_to_unicode handles various inputs and known TLDs", {
  # Test NA input (line 278)
  expect_equal(rurl:::.punycode_to_unicode(NA_character_), NA_character_)

  # Test specific Punycode TLDs that have workarounds
  # .ελ (xn--qxam) is already covered by existing get_tld tests indirectly if
  # they pass.
  # Let's ensure direct test too for the function itself.
  expect_equal(rurl:::.punycode_to_unicode("xn--qxam"), "ελ") # .ελ

  # .рф (xn--p1ai) - Line 289
  expect_equal(rurl:::.punycode_to_unicode("xn--p1ai"), "рф")
  expect_equal(
    rurl:::.punycode_to_unicode("сайт.xn--p1ai"),
    "сайт.рф"
  )

  # .مصر (xn--wgbh1c) - Line 291
  expect_equal(rurl:::.punycode_to_unicode("xn--wgbh1c"), "مصر")
  expect_equal(
    rurl:::.punycode_to_unicode("موقع.xn--wgbh1c"),
    "موقع.مصر"
  )

  # .გე (xn--node) - Line 293
  expect_equal(rurl:::.punycode_to_unicode("xn--node"), "გე")
  expect_equal(
    rurl:::.punycode_to_unicode("საიტი.xn--node"),
    "საიტი.გე"
  )

  # Test standard punycode decoding via the default punycode backend
  expect_equal(rurl:::.punycode_to_unicode("xn--mnchen-3ya.de"), "münchen.de")
})

test_that("Internal TLD helpers handle NA/empty/error conditions", {
  # ._extract_tld_original_logic handles NA and empty input
  expect_equal(
    rurl:::._extract_tld_original_logic(
      NA_character_, rurl:::.tld_all_set, "all"
    ),
    NA_character_
  )
  expect_equal(
    rurl:::._extract_tld_original_logic("", rurl:::.tld_all_set, "all"),
    NA_character_
  )

  # get_tld returns NA for edge cases (uses safe_parse_url internally now)
  expect_equal(unname(get_tld("")), NA_character_)
  expect_equal(unname(get_tld(NA_character_)), NA_character_)
})

test_that("permute_url handles specific error/edge conditions from parsing", {
  # Lines 49-54: is.null(parsed_curl) or is.na(raw_host)
  # curl_parse_url("http://") gives host=NA
  res_http_only <- permute_url("http://")
  expect_equal(nrow(res_http_only), 1)
  expect_true(is.na(res_http_only$Permutation))
  expect_equal(res_http_only$URL, "http://")

  # What if curl_parse_url itself returns NULL?
  # This would happen if url_to_parse is truly unparseable by curl's C library.
  # e.g. a very bad string. safe_parse_url would return NULL from the start.
  # permute_url prepends "http://" if no scheme, so it's harder to make
  # curl_parse_url return NULL
  # unless the original string + "http://" is still malformed for curl.
  # Example: very long string, or control characters.
  # For now, the http:// case covers is.na(raw_host).

  # Lines 60-65: !nzchar(trimws(stripped_bare_host))
  # Input "www." -> stripped_bare_host becomes ""
  res_www_only <- permute_url("www.")
  expect_equal(nrow(res_www_only), 1)
  expect_true(is.na(res_www_only$Permutation))
  expect_equal(res_www_only$URL, "www.")

  res_www_num_only <- permute_url("www123.")
  expect_equal(nrow(res_www_num_only), 1)
  expect_true(is.na(res_www_num_only$Permutation))
  expect_equal(res_www_num_only$URL, "www123.")

  # Lines 125-129: length(unique_permutations) == 0 (else branch)
  # Input "a" (after filters, might become empty)
  # The filters in permute_url are quite strict:
  # unique_permutations <-
  # unique_permutations[nzchar(trimws(gsub("^(https?://)?(www[.])?", "",
  # unique_permutations, perl = TRUE)))]
  # unique_permutations <- unique_permutations[!unique_permutations %in%
  # c("http://", "https://", "http://www.", "https://www.", "www.", "")]
  # unique_permutations <-
  # unique_permutations[nzchar(trimws(unique_permutations))]
  # For input "a", permutations like "a", "a/", "http://a", etc. are generated.
  # gsub removes scheme/www -> "a" or "a/". These pass nzchar.
  # They don't match the specific remnants.
  # So for "a", it will likely produce results.
  # To hit this, we need all generated permutations to be filtered out.
  # Consider an input that *only* produces things like "http://" or "www." or
  # empty after initial processing.
  # This should be caught by earlier NA returns if host becomes empty.
  # Let's re-check permute_url("http://") - it results in raw_host=NA, so
  # returns NA perm early.
  # Let's try an input that is just a scheme after prepending, e.g. if
  # original_url_input was very odd.
  # This branch is hard to hit if the initial host checks are robust.
  # The current NA returns for invalid hosts (like from "http://" or "www.")
  # cover most scenarios
  # where permutations would be empty or invalid.
  # The example permute_url("") already covers one path to this (via early
  # next).
  # The else branch implies permutations were generated but *all* got filtered.
  # This means stripped_bare_host was valid, but all scheme/www/path combos
  # were filtered.
  # Example: If stripped_bare_host was such that all its permutations like
  # "www.host" or "scheme://host"
  # were *exactly* the strings in the filter list. (e.g. host = "")
  # If stripped_bare_host is "", it's caught by `if
  # (!nzchar(trimws(stripped_bare_host)))`
  # So this specific `else` branch on line 125 seems very hard to reach if
  # prior checks are done.
  # It's a defensive catch-all. We can accept it might not be hit if logic is
  # sound.
})

test_that("permute_url generates expected permutations for various URL types", {
  # Case 1: Root domain
  input_root <- "test.com"
  perm_args <- list(
    protocol_handling = c("strip", "http", "https"),
    www_handling = c("strip", "keep"),
    case_handling = "keep",
    trailing_slash_handling = c("none", "keep"),
    subdomain_levels_to_keep = list(NULL),
    host_encoding = "keep",
    path_encoding = "keep"
  )
  expected_perms_root <- sort(c(
    "test.com", "test.com/",
    "http://test.com", "http://test.com/",
    "https://test.com", "https://test.com/",
    "www.test.com", "www.test.com/",
    "http://www.test.com", "http://www.test.com/",
    "https://www.test.com", "https://www.test.com/"
  ))
  result_root <- do.call(permute_url, c(list(input_root), perm_args))
  expect_equal(nrow(result_root), 12)
  expect_equal(sort(result_root$Permutation), expected_perms_root)
  expect_true(all(result_root$URL == input_root))

  # Case 2: Domain with a specific path and query parameters
  input_path <- "test.com/folder/subfolder/path?parameter=value"
  expected_perms_path <- sort(c(
    "test.com/folder/subfolder/path?parameter=value",
    "test.com/folder/subfolder/path/?parameter=value",
    "http://test.com/folder/subfolder/path?parameter=value",
    "http://test.com/folder/subfolder/path/?parameter=value",
    "https://test.com/folder/subfolder/path?parameter=value",
    "https://test.com/folder/subfolder/path/?parameter=value",
    "www.test.com/folder/subfolder/path?parameter=value",
    "www.test.com/folder/subfolder/path/?parameter=value",
    "http://www.test.com/folder/subfolder/path?parameter=value",
    "http://www.test.com/folder/subfolder/path/?parameter=value",
    "https://www.test.com/folder/subfolder/path?parameter=value",
    "https://www.test.com/folder/subfolder/path/?parameter=value"
  ))
  result_path <- do.call(permute_url, c(list(input_path), perm_args))
  expect_equal(nrow(result_path), 12)
  expect_equal(sort(result_path$Permutation), expected_perms_path)
  expect_true(all(result_path$URL == input_path))

  # Case 3: Internationalized Domain Name (IDN)
  testthat::skip_if_not(
    l10n_info()[["UTF-8"]],
    "IDN permutations require UTF-8 locale"
  )
  input_idn <- "παράδειγμα.ελ"
  expected_perms_idn <- sort(c(
    "παράδειγμα.ελ", "παράδειγμα.ελ/",
    "http://παράδειγμα.ελ", "http://παράδειγμα.ελ/",
    "https://παράδειγμα.ελ", "https://παράδειγμα.ελ/",
    "www.παράδειγμα.ελ", "www.παράδειγμα.ελ/",
    "http://www.παράδειγμα.ελ",
    "http://www.παράδειγμα.ελ/",
    "https://www.παράδειγμα.ελ",
    "https://www.παράδειγμα.ελ/"
  ))
  result_idn <- do.call(permute_url, c(list(input_idn), perm_args))
  expect_equal(nrow(result_idn), 12)
  expect_equal(sort(result_idn$Permutation), expected_perms_idn)
  expect_true(all(result_idn$URL == input_idn))

  # Case 4: URL with existing scheme and www
  input_full <- "http://www.another.com/test/page.html"
  expected_perms_full <- sort(c(
    "another.com/test/page.html", "another.com/test/page.html/",
    "http://another.com/test/page.html", "http://another.com/test/page.html/",
    "https://another.com/test/page.html", "https://another.com/test/page.html/",
    "www.another.com/test/page.html", "www.another.com/test/page.html/",
    "http://www.another.com/test/page.html",
    "http://www.another.com/test/page.html/",
    "https://www.another.com/test/page.html",
    "https://www.another.com/test/page.html/"
  ))
  result_full <- do.call(permute_url, c(list(input_full), perm_args))
  expect_equal(nrow(result_full), 12)
  expect_equal(sort(result_full$Permutation), expected_perms_full)
  expect_true(all(result_full$URL == input_full))

  # Case 5: Empty input vector
  expect_equal(nrow(permute_url(character(0))), 0)
  expect_equal(colnames(permute_url(character(0))), c("URL", "Permutation"))

  # Case 6: NA input
  result_na <- permute_url(NA_character_)
  expect_equal(nrow(result_na), 1)
  expect_true(is.na(result_na$Permutation))
  expect_true(is.na(result_na$URL))

  # Case 7: Empty string input
  result_empty_str <- permute_url("")
  expect_equal(nrow(result_empty_str), 1)
  expect_true(is.na(result_empty_str$Permutation))
  expect_equal(result_empty_str$URL, "")

  # Case 8: Multiple URLs including one that would result in NA
  input_multiple <- c("ok.com", "")
  result_multiple <- do.call(permute_url, c(list(input_multiple), perm_args))
  expect_equal(
    nrow(result_multiple),
    12 + 1
  ) # 12 for ok.com, 1 for empty string
  expect_equal(sum(result_multiple$URL == "ok.com"), 12)
  expect_true(is.na(result_multiple$Permutation[result_multiple$URL == ""]))
})

test_that("permute_url validates option values strictly", {
  expect_error(
    permute_url("example.com", protocol_handling = "bogus"),
    "Invalid values"
  )
})

test_that("permute_url errors on mixed valid and invalid option values", {
  expect_error(
    permute_url("example.com", protocol_handling = c("http", "bogus")),
    "Invalid values"
  )
})

test_that("permute_url can include permutation rank", {
  res <- permute_url(
    "example.com",
    protocol_handling = "http",
    include_rank = TRUE
  )
  expect_true("PermutationRank" %in% names(res))
  expect_true(
    is.integer(res$PermutationRank) || is.numeric(res$PermutationRank)
  )
})

test_that("safe_parse_url validates and applies subdomain_levels_to_keep", {
  # Validation errors
  expect_error(
    safe_parse_url("example.com", subdomain_levels_to_keep = -1),
    "subdomain_levels_to_keep must be NULL or a non-negative integer"
  )
  expect_error(
    safe_parse_url("example.com", subdomain_levels_to_keep = 1.5),
    "subdomain_levels_to_keep must be NULL or a non-negative integer"
  )
  expect_error(
    safe_parse_url("example.com", subdomain_levels_to_keep = "one"),
    "subdomain_levels_to_keep must be NULL or a non-negative integer"
  )

  # Functional behavior: strip all subdomains
  res_strip <- safe_parse_url(
    "http://deep.sub.domain.example.com",
    subdomain_levels_to_keep = 0
  )
  expect_equal(res_strip$host, "example.com")
  expect_equal(res_strip$domain, "example.com")

  # Functional behavior: keep subdomains when level > 0 (current implementation
  # retains all)
  res_keep1 <- safe_parse_url(
    "http://deep.sub.domain.example.com",
    subdomain_levels_to_keep = 1
  )
  expect_equal(res_keep1$host, "deep.sub.domain.example.com")
  expect_equal(res_keep1$domain, "example.com")

  # Interaction with www_handling = keep
  res_www <- safe_parse_url(
    "http://www1.deep.example.com",
    www_handling = "keep",
    subdomain_levels_to_keep = 0
  )
  expect_equal(res_www$host, "www.example.com")

  # Cache hit path: second call should return cached result
  res_first <- safe_parse_url("example.com", subdomain_levels_to_keep = 1)
  res_second <- safe_parse_url("example.com", subdomain_levels_to_keep = 1)
  expect_identical(res_first, res_second)
})
