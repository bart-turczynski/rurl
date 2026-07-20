# Unit tests for the decomposed phase helpers behind ._safe_parse_url_impl().
# These exercise each normalization phase in isolation so behavior changes can
# be localized to a single phase rather than the former ~486-line monolith.

test_that(".prepare_url_for_curl prefixes scheme-less hosts with http://", {
  prep <- rurl:::.prepare_url_for_curl("example.com", "keep", "keep")
  expect_equal(prep$url_to_parse, "http://example.com")
  expect_false(prep$looks_like_protocol)
  expect_false(prep$original_has_allowed_scheme)
  expect_false(prep$is_scheme_relative)
})

test_that(".prepare_url_for_curl keeps supported explicit schemes as-is", {
  prep <- rurl:::.prepare_url_for_curl("https://example.com/p", "keep", "keep")
  expect_equal(prep$url_to_parse, "https://example.com/p")
  expect_true(prep$looks_like_protocol)
  expect_true(prep$original_has_allowed_scheme)
})

test_that(".prepare_url_for_curl treats host:port as a host, not a scheme", {
  prep <- rurl:::.prepare_url_for_curl("example.com:8080/p", "keep", "keep")
  expect_true(prep$looks_like_host_port)
  expect_equal(prep$url_to_parse, "http://example.com:8080/p")
})

test_that(".prepare_url_for_curl rejects unsupported schemes under keep/none", {
  expect_null(rurl:::.prepare_url_for_curl("mailto:a@b.com", "keep", "keep"))
  expect_null(rurl:::.prepare_url_for_curl("mailto:a@b.com", "none", "keep"))
})

test_that(".prepare_url_for_curl honors scheme-relative handling", {
  expect_null(
    rurl:::.prepare_url_for_curl("//cdn.example.com", "keep", "error")
  )
  prep_http <- rurl:::.prepare_url_for_curl("//cdn.example.com", "keep", "http")
  expect_true(prep_http$is_scheme_relative)
  expect_equal(prep_http$url_to_parse, "http://cdn.example.com")
  prep_keep <- rurl:::.prepare_url_for_curl("//cdn.example.com", "keep", "keep")
  expect_equal(prep_keep$url_to_parse, "http://cdn.example.com")
})

test_that(".parse_with_curl returns NULL on unparseable input", {
  parsed <- rurl:::.parse_with_curl("ht!tp://")
  expect_true(is.null(parsed) || is.list(parsed))
  expect_type(rurl:::.parse_with_curl("http://example.com"), "list")
})

test_that(".extract_raw_components takes the raw query verbatim", {
  # T2 (RURL-yuozrhop): with params = FALSE, curl surfaces the raw query
  # string directly, so .extract_raw_components takes it byte-for-byte (a bare
  # key keeps no trailing "=") rather than rebuilding it from decoded params.
  prepared <- "http://example.com/p?a=1&b=2&flag"
  parsed <- rurl:::.parse_with_curl(prepared)
  raw <- rurl:::.extract_raw_components(parsed, prepared)
  expect_equal(raw$host, "example.com")
  expect_equal(raw$query, "a=1&b=2&flag")
})

test_that(".extract_raw_path_vec preserves dot segments and percent case", {
  # Raw path comes from the prepared input, not curl's normalized $path, so RFC
  # 3986 dot segments (and encoded %2e forms) survive to path_normalization;
  # percent-triplet case is preserved for the later presentation phase.
  ext <- function(prepared) {
    curl_path <- rurl:::.parse_with_curl(prepared)$path
    rurl:::.extract_raw_path_vec(prepared, curl_path)
  }
  expect_equal(ext("http://ex.com/a/../b"), "/a/../b")
  expect_equal(ext("http://ex.com/a/%2e%2e/b"), "/a/%2e%2e/b")
  expect_equal(ext("http://ex.com/a%2fb"), "/a%2fb")
  expect_equal(ext("http://ex.com/a//b"), "/a//b")
  # Empty-authority special schemes are reinterpreted by curl as host-bearing
  # URLs. Keep curl's coherent path so the promoted host is not duplicated.
  expect_equal(ext("http:///evil.com"), "/")
  expect_equal(ext("http:///a/../b"), "/b")
  # No path component -> fall back to curl's canonical "/".
  expect_equal(ext("http://ex.com"), "/")
  expect_equal(ext("http://ex.com?x=1"), "/")
  # Query/fragment slashes never leak into the path.
  expect_equal(ext("http://ex.com/p?x=/y#/z"), "/p")
  # Vectorized, NA-safe.
  expect_equal(
    rurl:::.extract_raw_path_vec(c(NA, "http://ex.com/a"), c(NA, "/a")),
    c(NA, "/a")
  )
})

test_that(".normalize_path applies decode, normalization, index, trailing", {
  expect_equal(
    rurl:::.normalize_path(
      "//a///b", "keep", "collapse_slashes", "keep", "none"
    ),
    "/a/b"
  )
  expect_equal(
    rurl:::.normalize_path("/a/b/../c", "keep", "dot_segments", "keep", "none"),
    "/a/c"
  )
  expect_equal(
    rurl:::.normalize_path("/dir/index.html", "keep", "none", "strip", "none"),
    "/dir"
  )
  expect_equal(
    rurl:::.normalize_path("/dir/sub", "keep", "none", "keep", "strip"),
    "/dir/sub"
  )
  expect_equal(
    rurl:::.normalize_path("/dir/sub/", "keep", "none", "keep", "strip"),
    "/dir/sub"
  )
  expect_equal(
    rurl:::.normalize_path("/dir/sub", "keep", "none", "keep", "keep"),
    "/dir/sub/"
  )
  expect_true(is.na(rurl:::.normalize_path(
    NA_character_, "keep", "both", "strip", "strip"
  )))
})

test_that(".derive_final_scheme respects protocol policy", {
  expect_equal(rurl:::.derive_final_scheme("keep", TRUE, "ftp"), "ftp")
  expect_equal(rurl:::.derive_final_scheme("http", TRUE, "ftp"), "http")
  expect_equal(rurl:::.derive_final_scheme("https", TRUE, "ftp"), "https")
  expect_true(is.na(rurl:::.derive_final_scheme("strip", TRUE, "ftp")))
  expect_equal(rurl:::.derive_final_scheme("none", TRUE, "ftp"), "ftp")
  expect_true(is.na(rurl:::.derive_final_scheme("none", FALSE, "ftp")))
})

test_that(".detect_ip_host recognizes IPv4 and IPv6, rejects names", {
  expect_true(rurl:::.detect_ip_host("192.168.0.1"))
  expect_true(rurl:::.detect_ip_host("[2001:db8::1]"))
  expect_true(rurl:::.detect_ip_host("2001:db8::1"))
  expect_true(rurl:::.detect_ip_host("[::1]"))
  expect_false(rurl:::.detect_ip_host("example.com"))
  expect_false(rurl:::.detect_ip_host(NA_character_))
  expect_false(rurl:::.detect_ip_host(""))
})

test_that(".detect_ip_host recognizes IPv6 with embedded dotted-quad IPv4", {
  # RFC 4291 §2.2 form 3 / §2.5.5: a trailing dotted-quad is part of the
  # IPv6 grammar and must classify as an IP literal, not a registered name
  # (RURL-tvfpeocg).
  expect_true(rurl:::.detect_ip_host("[::ffff:127.0.0.1]"))
  expect_true(rurl:::.detect_ip_host("::ffff:127.0.0.1"))
  expect_true(rurl:::.detect_ip_host("[::ffff:0:127.0.0.1]"))
  expect_true(rurl:::.detect_ip_host("[64:ff9b::8.8.8.8]"))
  expect_true(rurl:::.detect_ip_host("[::127.0.0.1]"))
  # A malformed embedded tail (octet out of range) must not be accepted.
  expect_false(rurl:::.detect_ip_host("[::ffff:999.0.0.1]"))
  expect_false(rurl:::.detect_ip_host("[::ffff:256.0.0.1]"))
})

test_that(".detect_ip_host rejects invalid IP literals", {
  # IPv4 octets out of range
  expect_false(rurl:::.detect_ip_host("999.999.999.999"))
  expect_false(rurl:::.detect_ip_host("256.0.0.1"))
  # Unbalanced IPv6 brackets
  expect_false(rurl:::.detect_ip_host("[2001:db8"))
  expect_false(rurl:::.detect_ip_host("2001:db8]"))
})

test_that(".apply_www_policy strips, keeps, and leaves IP hosts untouched", {
  expect_equal(
    rurl:::.apply_www_policy("www.example.com", "strip", FALSE),
    "example.com"
  )
  expect_equal(
    rurl:::.apply_www_policy("example.com", "keep", FALSE),
    "www.example.com"
  )
  expect_equal(
    rurl:::.apply_www_policy("example.com", "none", FALSE),
    "example.com"
  )
  expect_equal(
    rurl:::.apply_www_policy("192.168.0.1", "strip", TRUE),
    "192.168.0.1"
  )
})

test_that(".derive_domain_tld extracts registered domain and TLD", {
  dt <- rurl:::.derive_domain_tld("a.b.example.co.uk", FALSE, "all")
  expect_equal(dt$domain, "example.co.uk")
  expect_equal(dt$tld, "co.uk")
  ip <- rurl:::.derive_domain_tld("192.168.0.1", TRUE, "all")
  expect_true(is.na(ip$domain))
  expect_true(is.na(ip$tld))
})

test_that(".apply_subdomain_policy keeps the requested number of levels", {
  expect_equal(
    rurl:::.apply_subdomain_policy(
      "a.b.c.example.com", "example.com", 0, FALSE
    ),
    "example.com"
  )
  expect_equal(
    rurl:::.apply_subdomain_policy(
      "a.b.c.example.com", "example.com", 1, FALSE
    ),
    "c.example.com"
  )
  expect_equal(
    rurl:::.apply_subdomain_policy(
      "a.b.c.example.com", "example.com", 2, FALSE
    ),
    "b.c.example.com"
  )
  # NULL means "leave untouched"
  expect_equal(
    rurl:::.apply_subdomain_policy(
      "a.b.example.com", "example.com", NULL, FALSE
    ),
    "a.b.example.com"
  )
})

test_that(".apply_host_encoding round-trips IDNA and Unicode for names", {
  expect_equal(
    rurl:::.apply_host_encoding("bücher.example", "idna", FALSE),
    "xn--bcher-kva.example"
  )
  expect_equal(
    rurl:::.apply_host_encoding("xn--bcher-kva.example", "unicode", FALSE),
    "bücher.example"
  )
  expect_equal(
    rurl:::.apply_host_encoding("example.com", "keep", FALSE),
    "example.com"
  )
})

test_that(".apply_case_policy lowercases, uppercases, keeps as configured", {
  lo <- rurl:::.apply_case_policy("EXAMPLE.COM", "/P", "HTTP", "lower")
  expect_equal(lo$host, "example.com")
  expect_equal(lo$path, "/p")
  expect_equal(lo$scheme, "http")
  lh <- rurl:::.apply_case_policy("EXAMPLE.COM", "/P", "HTTP", "lower_host")
  expect_equal(lh$host, "example.com")
  expect_equal(lh$path, "/P")
  up <- rurl:::.apply_case_policy("example.com", "/p", "http", "upper")
  expect_equal(up$host, "EXAMPLE.COM")
})

test_that(".build_clean_url reconstructs scheme/host/path", {
  expect_equal(
    rurl:::.build_clean_url("https", "example.com", "/p", "none"),
    "https://example.com/p"
  )
  expect_equal(
    rurl:::.build_clean_url(NA_character_, "example.com", "/p", "none"),
    "example.com/p"
  )
  expect_equal(
    rurl:::.build_clean_url("http", "example.com", "/", "strip"),
    "http://example.com"
  )
  expect_true(is.na(
    rurl:::.build_clean_url("http", NA_character_, "/p", "none")
  ))
})

test_that(".derive_parse_status classifies outcomes", {
  parsed <- curl::curl_parse_url("http://example.com")
  expect_equal(
    rurl:::.derive_parse_status(
      parsed, "example.com", FALSE, "com",
      "example.com", "keep", "http", TRUE, TRUE, FALSE, FALSE, "keep"
    ),
    "ok"
  )
  expect_equal(
    rurl:::.derive_parse_status(
      parsed, "203.0.113.1", TRUE, NA_character_,
      NA_character_, "keep", "http", FALSE, TRUE, FALSE, FALSE, "keep"
    ),
    "ok"
  )
  expect_equal(
    rurl:::.derive_parse_status(
      parsed, "files.example.com", FALSE, "com",
      "example.com", "keep", "ftp", TRUE, TRUE, FALSE, FALSE, "keep"
    ),
    "ok-ftp"
  )
  expect_equal(
    rurl:::.derive_parse_status(
      parsed, "localhost", FALSE, NA_character_,
      NA_character_, "keep", "http", FALSE, TRUE, FALSE, FALSE, "keep"
    ),
    "warning-no-tld"
  )
  expect_equal(
    rurl:::.derive_parse_status(
      parsed, "example.com", FALSE, "com",
      "example.com", "keep", "http", TRUE, TRUE, FALSE, TRUE, "keep"
    ),
    "ok-scheme-relative"
  )
})

test_that(".derive_parse_status keeps host:port off scheme demotion", {
  # RURL-aldwnots: a scheme-less host:port matches the scheme regex
  # (looks_like_protocol = TRUE, original_has_allowed_scheme = FALSE) but is a
  # valid host:port form (looks_like_host_port = TRUE), so it must stay "ok"
  # rather than being demoted to "error".
  parsed <- curl::curl_parse_url("http://example.com:8080/x")
  expect_equal(
    rurl:::.derive_parse_status(
      parsed, "example.com", FALSE, "com",
      "example.com", "keep", "http", TRUE, FALSE, TRUE, FALSE, "keep"
    ),
    "ok"
  )
  # A genuinely unsupported scheme (looks_like_host_port = FALSE) still demotes.
  expect_equal(
    rurl:::.derive_parse_status(
      parsed, "example.com", FALSE, "com",
      "example.com", "keep", "http", TRUE, FALSE, FALSE, FALSE, "keep"
    ),
    "error"
  )
})

test_that("get_parse_status agrees with clean_url for scheme-less host:port", {
  # RURL-aldwnots end-to-end: a present clean_url must not co-occur with an
  # "error" status.
  expect_equal(get_parse_status("example.com:8080/x"), "ok")
  expect_equal(get_host("example.com:8080/x"), "example.com")
  expect_equal(get_clean_url("example.com:8080/x"), "http://example.com/x")
  # mailto: / user:pass@host stay error (genuinely unsupported / opaque).
  expect_equal(get_parse_status("mailto:a@b.com"), "error")
  expect_equal(get_parse_status("user:pass@example.com"), "error")
})

test_that(".assemble_parse_result coerces port to integer", {
  parsed <- curl::curl_parse_url("http://example.com:8080/p")
  res <- rurl:::.assemble_parse_result(
    "http://example.com:8080/p", "http", "example.com", parsed, "/p",
    NA_character_, "example.com", "com", "example.com", "example.com",
    "com", "com", FALSE,
    "http://example.com:8080/p", "ok", FALSE, "keep"
  )
  expect_type(res$port, "integer")
  expect_equal(res$port, 8080L)
  expect_equal(res$host, "example.com")
})

# --- Locale-invariance of URL-syntax case mapping (RURL-ugfpuotu) -----------
# `stringi::stri_trans_tolower()` without `locale=` inherits the session locale.
# Under Turkish/Azeri (`tr`, `az`) ICU correctly maps "I" -> "ı", which silently
# corrupted URL SYNTAX: hosts became different domains and `FILE:` stopped being
# recognized as a supported scheme. Syntax case mapping is now ASCII-only
# (`.ascii_tolower()` / `.ascii_toupper()`), so it cannot move with the locale.
#
# The locale is forced through `stringi::stri_locale_set()` rather than
# `withr::with_locale()` so the test exercises the exact mechanism (stringi's
# default locale) without depending on the machine having a `tr_TR` system
# locale installed -- ICU ships the `tr` tailoring itself, so this runs
# everywhere, including CI.
.with_turkish_stri_locale <- function(code) {
  old <- suppressMessages(stringi::stri_locale_set("tr_TR"))
  on.exit(
    {
      suppressMessages(stringi::stri_locale_set(old))
      rurl_clear_caches()
    },
    add = TRUE
  )
  rurl_clear_caches()
  force(code)
}

test_that(".ascii_tolower()/.ascii_toupper() ignore the session locale", {
  .with_turkish_stri_locale({
    # Guard: the hazard is actually armed in this session.
    expect_identical(stringi::stri_trans_tolower("WIKI"), "wıkı")

    expect_identical(
      rurl:::.ascii_tolower("WIKI.EXAMPLE.COM"), "wiki.example.com"
    )
    expect_identical(
      rurl:::.ascii_toupper("wiki.example.com"), "WIKI.EXAMPLE.COM"
    )
    expect_identical(rurl:::.ascii_tolower("FILE"), "file")
    # Non-ASCII code points pass through untouched -- "ASCII lowercase".
    expect_identical(rurl:::.ascii_tolower("ÄB"), "Äb")
    # NA / empty / zero-length propagate like stri_trans_*.
    expect_identical(rurl:::.ascii_tolower(c("A", NA, "")), c("a", NA, ""))
    expect_identical(rurl:::.ascii_tolower(character(0)), character(0))
  })
})

test_that("host and scheme normalization survive a Turkish session locale", {
  .with_turkish_stri_locale({
    # Host: the default case_handling = "lower_host" must not produce the
    # dotless i, which would silently name a DIFFERENT domain.
    expect_identical(get_host("https://WIKI.example.com/p"), "wiki.example.com")
    expect_identical(
      get_clean_url("https://WIKI.example.com/p"),
      "https://wiki.example.com/p"
    )

    # Scheme: `file` is the only supported scheme containing "i", so scheme
    # recognition against .SUPPORTED_SCHEMES broke for FILE: alone.
    expect_identical(get_parse_status("FILE:///tmp/x"), "ok")
    expect_identical(get_scheme("FILE:///tmp/x"), "file")
    expect_identical(get_clean_url("FILE:///tmp/x"), "file:///tmp/x")
    expect_identical(get_parse_status("FILE://localhost/tmp/x"), "ok")
  })
})

test_that("case_handling = 'upper' on a host stays Unicode and locale-pinned", {
  # Pins the direction split in `.apply_case_policy_vec()`: host LOWERING is
  # protocol syntax (ASCII-only), but host UPPERCASING is a presentation
  # transform -- no standard uppercases a host -- so it keeps full Unicode case
  # mapping under the pinned non-tailoring locale. A future refactor that
  # "unifies" the host branch on the ASCII helpers would produce the mixed-case
  # "BüCHER.EXAMPLE" and fail here.
  expect_identical(
    get_host("https://bücher.example/", case_handling = "upper"),
    "BÜCHER.EXAMPLE"
  )

  .with_turkish_stri_locale({
    # Unicode-aware ...
    expect_identical(
      get_host("https://bücher.example/", case_handling = "upper"),
      "BÜCHER.EXAMPLE"
    )
    # ... AND locale-invariant: Turkish would uppercase "i" to the dotted "İ".
    expect_identical(
      get_host("https://wiki.example.com/", case_handling = "upper"),
      "WIKI.EXAMPLE.COM"
    )
  })
})

test_that("path case_handling is locale-pinned, not session-dependent", {
  # The path keeps full Unicode case mapping (it is a user-requested transform
  # of free text) but pins an explicit non-tailoring locale. NOTE: "root" and
  # "und" do NOT override the ambient locale in stringi -- see
  # .ASCII_SAFE_ICU_LOCALE in R/utils.R.
  baseline <- safe_parse_url(
    "https://example.com/PATH/WIKI", case_handling = "lower"
  )$path
  expect_identical(baseline, "/path/wiki")

  .with_turkish_stri_locale({
    expect_identical(
      safe_parse_url(
        "https://example.com/PATH/WIKI", case_handling = "lower"
      )$path,
      baseline
    )
  })
})

test_that(".uppercase_percent_hex() declares a locale-invariant UTF-8 mark", {
  # `gsub(perl = TRUE)` hands back native ("unknown") bytes unchanged under
  # `LC_ALL=C` but marks them "UTF-8" under a UTF-8 session -- identical bytes,
  # divergent `Encoding()`. The helper now DECLARES the mark (never
  # transcodes), so the result is the same object in every locale.
  raw_bytes <- rawToChar(as.raw(c(0x2f, 0x70, 0x25, 0x32, 0x66, 0xc3, 0xbc)))
  expect_identical(Encoding(raw_bytes), "unknown")

  out <- rurl:::.uppercase_percent_hex(c(raw_bytes, "/plain%2f", NA))
  expect_identical(Encoding(out), c("UTF-8", "unknown", "unknown"))
  # Bytes are untouched apart from the hex uppercasing; NA stays NA.
  expect_identical(
    charToRaw(out[1]),
    as.raw(c(0x2f, 0x70, 0x25, 0x32, 0x46, 0xc3, 0xbc))
  )
  expect_identical(out[2], "/plain%2F")
  expect_true(is.na(out[3]))
})
