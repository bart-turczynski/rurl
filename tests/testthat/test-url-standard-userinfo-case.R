# Hex-digit case of a percent-triplet already present in the userinfo
# (RURL-jzwshyqb).
#
# The WHATWG URL Standard's authority state percent-encodes each userinfo code
# point with the userinfo percent-encode set. "%" is not in that set, so an
# existing triplet is copied as written: `new URL("http://a%7fb:c%c3d@a.com/")`
# has username `a%7fb` and password `c%c3d` (Node 26.3.1). The `rfc3986` arm
# keeps the triplet as written too since RURL-bxrbpzet (RFC 3986 sec 6.2.2.1,
# RUL-015), and the `NULL` profile is frozen (ADR 0007, ADR 0016).
#
# The `whatwg_*` columns are the Node `username` / `password` (a `""` password
# is reported as NA by the parse record). Red at the pre-fix baseline (main @
# 9f9df4a): every lowercase triplet came back uppercased, and `%g1` as `%G1`.

userinfo_case_cases <- data.frame(
  url = c(
    "http://a%7fb:c%c3d@a.com/",
    "http://a%7fb@a.com/",
    "http://u:p%7f@a.com/",
    "http://a%7Fb:c%C3d@a.com/",
    "http://a%7f%3ab:c%3a%7f@a.com/",
    "ftp://a%7fb:c%c3d@h.com/",
    # a lone "%" and a "%" before a non-hex pair
    "http://a%:b%g1@a.com/",
    # several "@": the earlier ones become "%40" (last_at_userinfo)
    "http://a%7f@b:c%7f@a.com/",
    "http://a%7f@b@c%7fd:e%7f@a.com/"
  ),
  whatwg_user = c(
    "a%7fb", "a%7fb", "u", "a%7Fb", "a%7f%3ab", "a%7fb",
    "a%", "a%7f%40b", "a%7f%40b%40c%7fd"
  ),
  whatwg_password = c(
    "c%c3d", NA, "p%7f", "c%C3d", "c%3a%7f", "c%c3d",
    "b%g1", "c%7f", "e%7f"
  ),
  stringsAsFactors = FALSE
)

test_that("whatwg: an existing userinfo triplet keeps its hex case", {
  u <- userinfo_case_cases$url
  for (args in list(
    list(url_standard = "whatwg"),
    list(profile = "whatwg")
  )) {
    r <- do.call(safe_parse_urls, c(list(u), args))
    expect_false(anyNA(r$host))
    expect_identical(r$user, userinfo_case_cases$whatwg_user)
    expect_identical(r$password, userinfo_case_cases$whatwg_password)
  }
  for (i in seq_along(u)) {
    s <- safe_parse_url(u[i], url_standard = "whatwg")
    expect_identical(s$user, userinfo_case_cases$whatwg_user[i], info = u[i])
    expect_identical(
      s$password, userinfo_case_cases$whatwg_password[i], info = u[i]
    )
  }
  expect_identical(
    get_user(u, url_standard = "whatwg"), userinfo_case_cases$whatwg_user
  )
  expect_identical(
    get_password(u, url_standard = "whatwg"),
    userinfo_case_cases$whatwg_password
  )
})

test_that("whatwg: the general route already keeps the triplet as written", {
  r <- safe_parse_urls("sc://a%7fb:c%c3d@h/", profile = "whatwg")
  expect_identical(r$user, "a%7fb")
  expect_identical(r$password, "c%c3d")
})

test_that("whatwg: serialize_url() keeps the triplet as written", {
  u <- userinfo_case_cases$url
  expect_identical(
    serialize_url(u, standard = "whatwg"),
    c(
      "http://a%7fb:c%c3d@a.com/", "http://a%7fb@a.com/",
      "http://u:p%7f@a.com/", "http://a%7Fb:c%C3d@a.com/",
      "http://a%7f%3ab:c%3a%7f@a.com/", "ftp://a%7fb:c%c3d@h.com/",
      "http://a%:b%g1@a.com/", "http://a%7f%40b:c%7f@a.com/",
      "http://a%7f%40b%40c%7fd:e%7f@a.com/"
    )
  )
})

test_that("whatwg: clean_url and get_url_key() never carry the userinfo", {
  u <- userinfo_case_cases$url
  r <- safe_parse_urls(u, url_standard = "whatwg")
  expect_false(any(grepl("@", r$clean_url, fixed = TRUE)))
  # Userinfo is excluded from the identity tuple, so the triplet's case cannot
  # move a key: the lowercase and uppercase spellings share one.
  k <- get_url_key(
    c("http://a%7fb:c%c3d@a.com/", "http://a%7Fb:c%C3d@a.com/",
      "http://a.com/"),
    url_key_policy("whatwg")
  )
  expect_identical(k[[1]], k[[2]])
  expect_identical(k[[1]], k[[3]])
})

test_that("rfc3986: the web route keeps the triplet as written too", {
  # RURL-bxrbpzet: RFC 3986 sec 6.2.2.1's fold belongs to `form =
  # "normalized"` (RUL-007, RUL-015); see test-rfc3986-userinfo-triplet-case.R.
  u <- userinfo_case_cases$url[1:6]
  r <- safe_parse_urls(u, url_standard = "rfc3986")
  expect_identical(r$user, userinfo_case_cases$whatwg_user[1:6])
  expect_identical(r$password, userinfo_case_cases$whatwg_password[1:6])
  s <- safe_parse_url(u[1], url_standard = "rfc3986")
  expect_identical(c(s$user, s$password), c("a%7fb", "c%c3d"))
  # a malformed triplet or a second "@" is no userinfo under RFC 3986
  expect_identical(
    safe_parse_urls(userinfo_case_cases$url[7:9],
                    url_standard = "rfc3986")$parse_status,
    rep("error", 3L)
  )
})

test_that("url_standard = NULL is frozen: the fold stays", {
  u <- userinfo_case_cases$url[1:7]
  r <- safe_parse_urls(u)
  expect_identical(
    r$user, c("a%7Fb", "a%7Fb", "u", "a%7Fb", "a%7F%3Ab", "a%7Fb", "a%")
  )
  expect_identical(
    r$password, c("c%C3d", NA, "p%7F", "c%C3d", "c%3A%7F", "c%C3d", "b%G1")
  )
  s <- safe_parse_url(u[1])
  expect_identical(c(s$user, s$password), c("a%7Fb", "c%C3d"))
  expect_null(safe_parse_url(userinfo_case_cases$url[8]))
})
