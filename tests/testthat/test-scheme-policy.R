# check_schemes(): the scheme-axis policy companion (RUL-021, RURL-zfycisur).

test_that("check_schemes reports the scheme facts for a mixed set", {
  urls <- c(
    "https://example.com/a",
    "ftp://example.com/a",
    "scp://host/a",
    "smb://server/share",
    "mailto:someone@example.com",
    "javascript:alert(1)",
    "notaurl"
  )
  r <- check_schemes(urls)

  expect_s3_class(r, "data.frame")
  expect_identical(nrow(r), length(urls))
  expect_identical(r$url, urls)
  expect_named(r, c("url", "scheme", "scheme_class", "web_scheme", "reasons"))
  expect_identical(
    r$scheme,
    c("https", "ftp", "scp", "smb", "mailto", "javascript", NA_character_)
  )
  expect_identical(
    r$scheme_class,
    c("special", "special", "non-special", "non-special", "non-special",
      "non-special", "missing-or-error")
  )
  expect_identical(
    r$web_scheme,
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_type(r$reasons, "list")
  expect_identical(r$reasons[[1]], "special-scheme")
  expect_identical(
    r$reasons[[3]], c("non-special-scheme", "outside-web-acceptance")
  )
  expect_identical(r$reasons[[7]], "no-scheme")
})

test_that("no allowed column is emitted without an allowlist", {
  r <- check_schemes("https://example.com/")
  expect_false("allowed" %in% names(r))
  # The absence is the point: with no allowlist there is no judgment to make,
  # so the helper reports facts and stops.
  expect_false(any(vapply(
    r$reasons, function(x) "not-in-allowlist" %in% x, logical(1)
  )))
})

test_that("allowed_schemes scores the set and adds the token", {
  urls <- c("https://example.com/a", "scp://host/a", "notaurl")
  r <- check_schemes(urls, allowed_schemes = c("https", "http"))

  expect_true("allowed" %in% names(r))
  expect_identical(r$allowed, c(TRUE, FALSE, FALSE))
  expect_false("not-in-allowlist" %in% r$reasons[[1]])
  expect_true("not-in-allowlist" %in% r$reasons[[2]])
  # A URL with no parsed scheme cannot satisfy an allowlist -- FALSE, not NA,
  # because "may I act on this?" has a definite answer.
  expect_true("not-in-allowlist" %in% r$reasons[[3]])
  expect_false(is.na(r$allowed[3]))
})

test_that("allowed_schemes is matched case-insensitively and de-duplicated", {
  r <- check_schemes(
    c("HTTPS://example.com/", "https://example.com/"),
    allowed_schemes = c("HTTPS", "https", " https ")
  )
  expect_identical(r$allowed, c(TRUE, TRUE))
})

test_that("an empty allowlist admits nothing but still reports the facts", {
  r <- check_schemes("https://example.com/", allowed_schemes = character(0))
  expect_false(r$allowed)
  expect_true("not-in-allowlist" %in% r$reasons[[1]])
  expect_identical(r$scheme, "https")
  expect_identical(r$scheme_class, "special")
})

test_that("check_schemes is vectorized, order-preserving, and handles edges", {
  urls <- c("https://b.example/", NA_character_, "", "ftp://a.example/")
  r <- check_schemes(urls)
  expect_identical(nrow(r), 4L)
  expect_identical(r$url, urls)
  expect_identical(r$scheme[c(1, 4)], c("https", "ftp"))
  expect_true(all(is.na(r$scheme[2:3])))
  expect_identical(check_schemes(character(0))$url, character(0))
})

test_that("check_schemes rejects bad input rather than guessing", {
  expect_error(check_schemes(123), "must be a character vector")
  expect_error(
    check_schemes(list("https://example.com/")), "must be a character vector"
  )
  expect_error(
    check_schemes("https://example.com/", allowed_schemes = 1),
    "`allowed_schemes` must be a character vector"
  )
  expect_error(
    check_schemes("https://example.com/", allowed_schemes = c("https", NA)),
    "`allowed_schemes` must be a character vector"
  )
})

test_that("check_schemes never changes how a URL parses", {
  urls <- c("scp://host/a", "javascript:alert(1)", "https://example.com/")
  before <- safe_parse_urls(
    urls, url_standard = "whatwg", scheme_acceptance = "general"
  )
  invisible(check_schemes(urls, allowed_schemes = "https"))
  after <- safe_parse_urls(
    urls, url_standard = "whatwg", scheme_acceptance = "general"
  )
  expect_identical(before, after)
  # Scoring a scheme FALSE does not make its URL fail to parse: that is the
  # whole reason this is a companion rather than a parse-time argument.
  expect_identical(after$parse_status, rep("ok", 3L))
})

test_that("both named standards work and scheme_acceptance is honored", {
  expect_identical(
    check_schemes("scp://host/a", url_standard = "rfc3986")$scheme, "scp"
  )
  # Under scheme_acceptance = "web" a non-web scheme is a parse error, which
  # is exactly why "general" is this helper's default -- see ?check_schemes.
  r_web <- check_schemes("scp://host/a", scheme_acceptance = "web")
  expect_identical(r_web$scheme_class, "missing-or-error")
  expect_identical(r_web$reasons[[1]], "no-scheme")
})

# The authority question (RURL-ktpjscne). These rows pin what the parse record
# says about each URL's authority, so the reasons vocabulary can be checked
# against it: a scheme fact must never contradict the parse.
authority_urls <- c(
  "mailto:someone@example.com", # non-special, opaque path, no authority
  "javascript:alert(1)",        # non-special, opaque path, no authority
  "foo:bar",                    # non-special, opaque path, no authority
  "scp://host/a",               # non-special, `//` authority
  "foo://host/x",               # non-special, `//` authority
  "http:example.com",           # special, no `//` in the input
  "http:/example.com",          # special, one `/` in the input
  "https:host",                 # special, no `//` in the input
  "notaurl"                     # no scheme
)

test_that("the parse record's authority facts are pinned (whatwg)", {
  p <- safe_parse_urls(
    authority_urls, url_standard = "whatwg", scheme_acceptance = "general"
  )
  expect_identical(
    p$parse_status,
    c(rep("ok", 7L), "warning-no-tld", "error")
  )
  # WHATWG: a non-special scheme with no `//` never enters the authority
  # states, so host is null and `@` stays in the opaque path. A special scheme
  # gets a host with or without `//` (the special-authority states).
  expect_identical(
    p$host,
    c(NA, NA, NA, "host", "host", "example.com", "example.com", "host", NA)
  )
  expect_identical(p$path[1:3], c("someone@example.com", "alert(1)", "bar"))
  # get_host() on a mailto: is ADR 0012 D7 recipient-extraction metadata, not
  # an authority parse; it deliberately diverges from the parse record's host.
  expect_identical(
    get_host("mailto:someone@example.com",
      url_standard = "whatwg", scheme_acceptance = "general"
    ),
    "example.com"
  )
  expect_true(is.na(get_host("foo:bar@baz",
    url_standard = "whatwg", scheme_acceptance = "general"
  )))
})

test_that("the parse record's authority facts are pinned (rfc3986)", {
  p <- safe_parse_urls(
    authority_urls, url_standard = "rfc3986", scheme_acceptance = "general"
  )
  expect_identical(p$parse_status, c(rep("ok", 8L), "error"))
  # RFC 3986 section 3: an authority exists only after `//`, special scheme
  # or not.
  expect_identical(
    p$host,
    c(NA, NA, NA, "host", "host", NA, NA, NA, NA)
  )
})

test_that("check_schemes reasons on the authority rows (pinned)", {
  nsp <- c("non-special-scheme", "outside-web-acceptance")
  for (std in c("whatwg", "rfc3986")) {
    r <- check_schemes(authority_urls, url_standard = std)
    expect_identical(r$reasons[[1]], nsp)
    expect_identical(r$reasons[[2]], nsp)
    expect_identical(r$reasons[[3]], nsp)
    expect_identical(r$reasons[[4]], nsp)
    expect_identical(r$reasons[[5]], nsp)
    expect_identical(r$reasons[[6]], "special-scheme")
    expect_identical(r$reasons[[7]], "special-scheme")
    expect_identical(r$reasons[[8]], "special-scheme")
    expect_identical(r$reasons[[9]], "no-scheme")
  }
})

test_that("check_schemes leaves the authority rows' parse untouched", {
  for (std in c("whatwg", "rfc3986")) {
    before <- safe_parse_urls(
      authority_urls, url_standard = std, scheme_acceptance = "general"
    )
    invisible(check_schemes(authority_urls, url_standard = std))
    after <- safe_parse_urls(
      authority_urls, url_standard = std, scheme_acceptance = "general"
    )
    expect_identical(before, after)
  }
})
