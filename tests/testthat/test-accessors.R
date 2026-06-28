test_that("get_clean_url returns expected values", {
  urls <- c("example.com", "http://test.com/page")
  expect_equal(
    unname(get_clean_url(urls)),
    c("http://example.com/", "http://test.com/page")
  )
})

test_that("get_clean_url lowercases scheme + host but keeps path by default", {
  # Default is "lower_host" (RFC 3986 6.2.2.1): scheme and host fold to
  # lowercase, the case-sensitive path is preserved.
  expect_equal(
    unname(get_clean_url("Http://Example.Com/MyPath/")),
    "http://example.com/MyPath/"
  )
})

test_that("get_clean_url preserves all casing with case_handling = 'keep'", {
  expect_equal(
    unname(get_clean_url("Http://Example.Com/MyPath/", case_handling = "keep")),
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
  }
)

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
  }
)

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
  # Default "lower_host" preserves path casing (paths are case-sensitive).
  expect_equal(unname(get_path("HTTP://EXAMPLE.NET/A/B/C/?p=1")), "/A/B/C/")
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
  expect_type(parsed, "list")
  expect_equal(parsed[[1]]$a, c("1", "2"))
  expect_equal(parsed[[1]]$b, "3")
})

test_that("get_query(format = 'list') preserves encoded '&' (%26) in values", {
  # Regression: an encoded ampersand inside a value must not be treated as a
  # pair delimiter when splitting the query string.
  expect_equal(
    get_query("http://example.com/?a=1%262", format = "list")[[1]],
    list(a = "1&2")
  )
  # Scalar (string) form must be unaffected by the fix.
  expect_equal(
    unname(get_query("http://example.com/?a=1%262")),
    "a=1&2"
  )
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

test_that(".psl_registered_domain handles known cases correctly", {
  expect_equal(rurl:::.psl_registered_domain("example.com"), "example.com")
  expect_equal(
    rurl:::.psl_registered_domain("sub.example.co.uk"),
    "example.co.uk"
  )
  expect_equal(
    rurl:::.psl_registered_domain("sub.dev-builder.code.com"),
    NA_character_
  )
  expect_equal(
    rurl:::.psl_registered_domain("city.kawasaki.jp"),
    "city.kawasaki.jp"
  )
  expect_equal(
    rurl:::.psl_registered_domain("foo.bar.city.kawasaki.jp"),
    "city.kawasaki.jp"
  )
  expect_equal(rurl:::.psl_registered_domain("unknown.tld"), NA_character_)
  expect_equal(rurl:::.psl_registered_domain("localhost"), NA_character_)
})

test_that(".psl_registered_domain handles suffix-only domains", {
  expect_true(is.na(rurl:::.psl_registered_domain("com")))
  expect_true(is.na(rurl:::.psl_registered_domain("co.uk")))
})

test_that("PSL seam helpers handle NA/empty/section edge cases", {
  expect_true(is.na(rurl:::.psl_registered_domain(NA_character_, "all")))
  expect_true(is.na(rurl:::.psl_registered_domain("", "all")))
  expect_equal(
    rurl:::.psl_registered_domain("sub.example.com", "all"),
    "example.com"
  )
  # example.net is ICANN-only, so under the private section it has no suffix.
  expect_true(is.na(rurl:::.psl_registered_domain("example.net", "private")))

  expect_true(is.na(rurl:::.psl_public_suffix(NA_character_, "all")))
  expect_true(is.na(rurl:::.psl_public_suffix("", "all")))
  expect_equal(rurl:::.psl_public_suffix("sub.example.com", "all"), "com")
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
  expect_identical(unname(get_tld("中国.中国")), "中国")
  # Default host_encoding = "keep" mirrors the input spelling: an A-label host
  # yields an A-label TLD.
  expect_identical(
    unname(get_tld("xn--hxajbheg2az3al.xn--qxam")), "xn--qxam"
  )
  expect_identical(unname(get_tld("xn--fiqs8s.xn--fiqs8s")), "xn--fiqs8s")
  # host_encoding = "unicode" decodes A-label hosts back to Unicode.
  expect_identical(
    unname(get_tld("xn--hxajbheg2az3al.xn--qxam", host_encoding = "unicode")),
    "ελ"
  )
  expect_identical(
    unname(get_tld("xn--fiqs8s.xn--fiqs8s", host_encoding = "unicode")), "中国"
  )
})

test_that("domain accessors honor host_encoding (keep/idna/unicode)", {
  uni <- "https://café.münchen.de/x"
  ace <- "https://xn--caf-dma.xn--mnchen-3ya.de/x"

  # keep: mirror the input spelling on both directions.
  expect_identical(unname(get_domain(uni)), "münchen.de")
  expect_identical(unname(get_domain(ace)), "xn--mnchen-3ya.de")
  expect_identical(unname(get_subdomain(uni)), "café")
  expect_identical(unname(get_subdomain(ace)), "xn--caf-dma")

  # idna: force A-labels regardless of input spelling.
  expect_identical(
    unname(get_domain(uni, host_encoding = "idna")), "xn--mnchen-3ya.de"
  )
  expect_identical(
    unname(get_subdomain(uni, host_encoding = "idna")), "xn--caf-dma"
  )
  expect_identical(
    unname(get_tld(ace, host_encoding = "idna")), "de"
  )

  # unicode: decode regardless of input spelling.
  expect_identical(
    unname(get_domain(ace, host_encoding = "unicode")), "münchen.de"
  )
  expect_identical(
    unname(get_subdomain(ace, host_encoding = "unicode")), "café"
  )

  # A pure-ASCII host is identical across all three encodings.
  for (enc in c("keep", "idna", "unicode")) {
    expect_identical(
      unname(get_domain("https://sub.example.co.uk/", host_encoding = enc)),
      "example.co.uk"
    )
  }
})

test_that("get_tld handles edge cases and unexpected inputs gracefully", {
  expect_identical(unname(get_tld(NA_character_)), NA_character_)
  expect_identical(unname(get_tld("")), NA_character_)
  expect_identical(unname(get_tld("not a url")), NA_character_)
  expect_identical(unname(get_tld("http://localhost")), NA_character_)
  expect_identical(unname(get_tld("http://example.")), NA_character_)
  expect_identical(unname(get_tld("http://192.168.1.1")), NA_character_)
  expect_identical(unname(get_tld("ftp://example.com")), "com")

  # IDN edge cases (host_encoding = "unicode" decodes A-labels)
  expect_identical(
    unname(get_tld("xn--fiqs8s.xn--fiqs8s", host_encoding = "unicode")), "中国"
  )
  expect_identical(unname(get_tld("中国.中国")), "中国")
  expect_identical(
    unname(get_tld("xn--hxajbheg2az3al.xn--qxam", host_encoding = "unicode")),
    "ελ"
  )
  expect_identical(unname(get_tld("παράδειγμα.ελ")), "ελ")

  # Weird but valid cases
  expect_identical(unname(get_tld(".com")), NA_character_)
  expect_true(is.na(rurl:::.psl_registered_domain(".com")))
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
  }
)

test_that(".normalize_and_punycode returns NA_character_ on error", {
  fail_encode <- function(x) stop("fail")

  result <- rurl:::.normalize_and_punycode(
    "παράδειγμα.ελ",
    encode_fn = fail_encode
  )
  expect_identical(unname(result), NA_character_)
})

test_that(".normalize_and_punycode recovers via non-strict retry", {
  # Force the strict encode to fail, then succeed on the non-strict retry,
  # exercising the inner tryCatch recovery branch (strict error -> non-strict
  # success). A non-default encode_fn bypasses the shared cache by design.
  strict_fail_encode <- function(x, strict = TRUE) {
    if (isTRUE(strict)) {
      stop("strict encode failed")
    }
    "recovered-host.example"
  }

  result <- rurl:::.normalize_and_punycode(
    "παράδειγμα.ελ",
    encode_fn = strict_fail_encode
  )
  expect_identical(unname(result), "recovered-host.example")
})

test_that(".normalize_and_punycode handles NA and empty input", {
  expect_identical(
    unname(rurl:::.normalize_and_punycode(NA_character_)),
    NA_character_
  )
  expect_identical(unname(rurl:::.normalize_and_punycode("")), "")
})

test_that("safe_parse_url handles www_handling options correctly", {
  # www_handling strip mode
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

  # www_handling keep mode
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
  expect_null(safe_parse_url("http://", www_handling = "keep")$host)
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
  # A URL that looks like it has a protocol but whose scheme is not allowed
  # should yield parse_status "error" (and a NULL return from safe_parse_url
  # itself), assuming gopher is not in allowed_prefixes.
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
  # .psl_public_suffix handles NA and empty input
  expect_equal(
    rurl:::.psl_public_suffix(NA_character_, "all"),
    NA_character_
  )
  expect_equal(
    rurl:::.psl_public_suffix("", "all"),
    NA_character_
  )

  # get_tld returns NA for edge cases (uses safe_parse_url internally now)
  expect_equal(unname(get_tld("")), NA_character_)
  expect_equal(unname(get_tld(NA_character_)), NA_character_)
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

  # Functional behavior: keep N rightmost subdomain labels (counted from the
  # registered domain outward), per documented behavior.
  res_keep1 <- safe_parse_url(
    "http://deep.sub.domain.example.com",
    subdomain_levels_to_keep = 1
  )
  expect_equal(res_keep1$host, "domain.example.com")
  expect_equal(res_keep1$domain, "example.com")

  res_keep2 <- safe_parse_url(
    "http://deep.sub.domain.example.com",
    subdomain_levels_to_keep = 2
  )
  expect_equal(res_keep2$host, "sub.domain.example.com")

  # Requesting more levels than available keeps all subdomains (capped).
  res_keep_over <- safe_parse_url(
    "http://deep.sub.domain.example.com",
    subdomain_levels_to_keep = 9
  )
  expect_equal(res_keep_over$host, "deep.sub.domain.example.com")

  # www is preserved by www_handling = "keep" and is orthogonal to the
  # subdomain level count.
  res_www_keep1 <- safe_parse_url(
    "http://www.deep.sub.domain.example.com",
    www_handling = "keep",
    subdomain_levels_to_keep = 1
  )
  expect_equal(res_www_keep1$host, "www.domain.example.com")

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

test_that("port is returned as integer and vectorizes without error", {
  # Regression: curl returns port as character; the parser must coerce so that
  # scalar, vector, and accessor paths all yield integer / NA_integer_.
  res <- safe_parse_url("http://example.com:8080/path")
  expect_identical(res$port, 8080L)

  vec <- safe_parse_urls(c("http://example.com", "http://example.com:8080"))
  expect_identical(vec$port, c(NA_integer_, 8080L))

  expect_identical(unname(get_port("http://example.com:8080/path")), 8080L)
  expect_identical(unname(get_port("http://example.com/path")), NA_integer_)
})

test_that("bracketed IPv6 hosts are detected as IP hosts", {
  # Regression: the IPv6 detector regex was over-escaped and never matched, so
  # IPv6 hosts fell through to TLD derivation and got a spurious warning.
  res <- safe_parse_url("http://[2001:db8::1]/x")
  expect_true(res$is_ip_host)
  expect_equal(res$parse_status, "ok")
  expect_true(is.na(res$domain))
  expect_true(is.na(res$tld))
  expect_identical(unname(get_domain("http://[2001:db8::1]/x")), NA_character_)
  expect_identical(unname(get_tld("http://[2001:db8::1]/x")), NA_character_)
  # IPv4 parity (already worked; guards against regressions).
  expect_true(safe_parse_url("http://192.168.1.1/x")$is_ip_host)
})

test_that("IPv6 literals with embedded dotted-quad IPv4 are detected", {
  # RURL-tvfpeocg: ::ffff:127.0.0.1 and friends used to fall through to the
  # TLD path (is_ip_host = FALSE, warning-invalid-tld). Both the dotted and
  # hex spellings of the same address must now classify identically.
  for (u in c(
    "http://[::ffff:127.0.0.1]/", "http://[::ffff:7f00:1]/",
    "http://[::ffff:0808:0808]/", "http://[64:ff9b::8.8.8.8]/",
    "http://[::127.0.0.1]/"
  )) {
    res <- safe_parse_url(u)
    expect_true(res$is_ip_host, info = u)
    expect_equal(res$parse_status, "ok", info = u)
    expect_true(is.na(res$tld), info = u)
  }
})

test_that("get_*() accessors error if passed a parsed object, not a string", {
  # Use a hand-constructed list so the test does not depend on safe_parse_url()
  # being callable in the current R environment.
  p <- list(
    scheme = "https", host = "example.com", port = NA_integer_,
    path = "/x", query = NA_character_, fragment = NA_character_,
    user = NA_character_, password = NA_character_, domain = "example.com",
    tld = "com", is_ip_host = FALSE, clean_url = "https://example.com/x",
    parse_status = "ok", original_url = "https://example.com/x"
  )

  # .extract_from_urls path
  expect_error(get_host(p), "character vector")
  expect_error(get_clean_url(p), "character vector")
  expect_error(get_domain(p), "character vector")
  expect_error(get_scheme(p), "character vector")
  expect_error(get_path(p), "character vector")
  expect_error(get_fragment(p), "character vector")
  expect_error(get_port(p), "character vector")
  expect_error(get_user(p), "character vector")
  expect_error(get_password(p), "character vector")
  expect_error(get_userinfo(p), "character vector")
  expect_error(get_tld(p), "character vector")
  expect_error(get_parse_status(p), "character vector")

  # lapply paths
  expect_error(get_query(p), "character vector")
  expect_error(get_subdomain(p), "character vector")
})
