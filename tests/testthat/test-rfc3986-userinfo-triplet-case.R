# Hex-digit case of a percent-triplet already present in the userinfo, under
# `url_standard = "rfc3986"` (RURL-bxrbpzet).
#
# RFC 3986 sec 6.2.2.1 makes the hex-digit case fold a NORMALIZATION, so it
# belongs to `serialize_url(standard = "rfc3986", form = "normalized")` and not
# to the parse record or the `source` form (RUL-007), and the `rfc3986` parse
# record follows the source serializer (RUL-015). The query and fragment
# already do (`pqf_source = "preserve"`).
#
# This file first pins what must NOT move: the frozen `NULL` profile (ADR 0007,
# ADR 0016), the `whatwg` record (RURL-jzwshyqb), the `rfc3986` general route,
# both `rfc3986` serializer forms, and the userinfo-free surfaces (`clean_url`,
# `get_url_key()`).

rfc_ui_url <- "http://a%7fb:c%c3d@h/"

test_that("NULL is frozen: the web route still folds the userinfo", {
  r <- safe_parse_urls(c(rfc_ui_url, "http://a%:b%g1@a.com/"))
  expect_identical(r$user, c("a%7Fb", "a%"))
  expect_identical(r$password, c("c%C3d", "b%G1"))
  s <- safe_parse_url(rfc_ui_url)
  expect_identical(c(s$user, s$password), c("a%7Fb", "c%C3d"))
  expect_identical(get_user(rfc_ui_url), "a%7Fb")
  expect_identical(get_password(rfc_ui_url), "c%C3d")
})

test_that("whatwg keeps the userinfo as written", {
  r <- safe_parse_urls(rfc_ui_url, url_standard = "whatwg")
  expect_identical(c(r$user, r$password), c("a%7fb", "c%c3d"))
  s <- safe_parse_url(rfc_ui_url, url_standard = "whatwg")
  expect_identical(c(s$user, s$password), c("a%7fb", "c%c3d"))
})

test_that("rfc3986: the general route keeps the userinfo as written", {
  u <- c("ws://a%7fb:c%c3d@h/", "sc://a%7fb:c%c3d@h/")
  for (args in list(
    list(url_standard = "rfc3986", scheme_acceptance = "general"),
    list(url_standard = "rfc3986", scheme_acceptance = "general",
         scheme_policy = "require")
  )) {
    r <- do.call(safe_parse_urls, c(list(u), args))
    expect_identical(r$user, c("a%7fb", "a%7fb"))
    expect_identical(r$password, c("c%c3d", "c%c3d"))
  }
})

test_that("rfc3986: the source form writes the userinfo as written", {
  expect_identical(
    serialize_url(
      c(rfc_ui_url, "ftp://a%7fb:c%c3d@h/", "sc://a%7fb:c%c3d@h/"),
      standard = "rfc3986"
    ),
    c("http://a%7fb:c%c3d@h/", "ftp://a%7fb:c%c3d@h/", "sc://a%7fb:c%c3d@h/")
  )
})

test_that("rfc3986: the normalized form still folds the userinfo", {
  expect_identical(
    serialize_url(
      c(rfc_ui_url, "ftp://a%7fb:c%c3d@h/", "sc://a%7fb:c%c3d@h/"),
      standard = "rfc3986", form = "normalized"
    ),
    c("http://a%7Fb:c%C3d@h/", "ftp://a%7Fb:c%C3d@h/", "sc://a%7Fb:c%C3d@h/")
  )
})

test_that("rfc3986: clean_url and get_url_key() never carry the userinfo", {
  u <- c(rfc_ui_url, "http://a%7Fb:c%C3d@h/", "http://h/")
  r <- safe_parse_urls(u, url_standard = "rfc3986")
  expect_identical(r$clean_url, rep("http://h/", 3L))
  k <- get_url_key(u, url_key_policy("rfc3986"))
  expect_identical(k[[1]], k[[3]])
  expect_identical(k[[2]], k[[3]])
})

test_that("rfc3986: a % that starts no triplet is no userinfo", {
  # RFC 3986 sec 3.2.1: `userinfo = *( unreserved / pct-encoded / sub-delims
  # / ":" )`, so "%" and "%g1" fail the uniform grammar gate before the record
  # is written; the fold never sees them.
  r <- safe_parse_urls("http://a%:b%g1@a.com/", url_standard = "rfc3986")
  expect_identical(r$parse_status, "error")
  expect_null(safe_parse_url("http://a%:b%g1@a.com/", url_standard = "rfc3986"))
})

# The fix. Red at the pre-fix baseline (main @ 6051635): the web route
# uppercased every lowercase triplet on both sides of the ":".
rfc_ui_cases <- data.frame(
  url = c(
    "http://a%7fb:c%c3d@h/",
    "http://a%7fb@a.com/",
    "http://u:p%7f@a.com/",
    "http://a%7Fb:c%C3d@a.com/",
    "http://a%7f%3ab:c%3a%7f@a.com/",
    "https://a%7fb:c%c3d@h.com/",
    "ftp://a%7fb:c%c3d@h.com/",
    "ftps://a%7fb:c%c3d@h.com/"
  ),
  user = c(
    "a%7fb", "a%7fb", "u", "a%7Fb", "a%7f%3ab", "a%7fb", "a%7fb", "a%7fb"
  ),
  password = c(
    "c%c3d", NA, "p%7f", "c%C3d", "c%3a%7f", "c%c3d", "c%c3d", "c%c3d"
  ),
  stringsAsFactors = FALSE
)

test_that("rfc3986: the web route keeps an existing triplet's hex case", {
  u <- rfc_ui_cases$url
  for (args in list(
    list(url_standard = "rfc3986"),
    list(url_standard = "rfc3986", scheme_acceptance = "general",
         scheme_policy = "require"),
    list(profile = "rfc-syntax")
  )) {
    r <- do.call(safe_parse_urls, c(list(u), args))
    expect_false(anyNA(r$host))
    expect_identical(r$user, rfc_ui_cases$user)
    expect_identical(r$password, rfc_ui_cases$password)
  }
  for (i in seq_along(u)) {
    s <- safe_parse_url(u[i], url_standard = "rfc3986")
    expect_identical(s$user, rfc_ui_cases$user[i], info = u[i])
    expect_identical(s$password, rfc_ui_cases$password[i], info = u[i])
  }
  expect_identical(get_user(u, url_standard = "rfc3986"), rfc_ui_cases$user)
  expect_identical(
    get_password(u, url_standard = "rfc3986"), rfc_ui_cases$password
  )
})

test_that("rfc3986: the record agrees with the general route and source", {
  web <- safe_parse_urls(rfc_ui_url, url_standard = "rfc3986")
  gen <- safe_parse_urls(
    c("ws://a%7fb:c%c3d@h/", "sc://a%7fb:c%c3d@h/"),
    url_standard = "rfc3986", scheme_acceptance = "general"
  )
  expect_identical(rep(web$user, 2L), gen$user)
  expect_identical(rep(web$password, 2L), gen$password)
  src <- serialize_url(rfc_ui_url, standard = "rfc3986")
  expect_identical(
    src, paste0("http://", web$user, ":", web$password, "@h/")
  )
})
