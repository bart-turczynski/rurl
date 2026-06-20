# Tests for the shared accessor extraction path (.extract_from_urls) and the
# public return types/names it must preserve across all get_* accessors.

test_that(".extract_from_urls pulls a single field and preserves names", {
  urls <- c("http://example.com/p", "https://sub.example.org")
  res <- rurl:::.extract_from_urls(urls, "host")
  expect_type(res, "character")
  expect_equal(unname(res), c("example.com", "sub.example.org"))
  expect_named(res, urls)
})

test_that(".extract_from_urls returns null_value for unparseable URLs", {
  expect_true(is.na(rurl:::.extract_from_urls("mailto:a@b.com", "host")))
  expect_equal(
    rurl:::.extract_from_urls("mailto:a@b.com", "parse_status",
      null_value = "error"
    ),
    c("mailto:a@b.com" = "error")
  )
})

test_that(".extract_from_urls honors fun_value and transform (port path)", {
  res <- rurl:::.extract_from_urls("http://example.com:8080/", "port",
    null_value = NA_integer_, fun_value = integer(1), transform = as.integer
  )
  expect_type(res, "integer")
  expect_equal(unname(res), 8080L)
})

test_that(".extract_from_urls with field=NULL hands over the parsed list", {
  res <- rurl:::.extract_from_urls("http://u:p@example.com", NULL,
    transform = function(parsed) parsed$user %||% NA_character_
  )
  expect_equal(unname(res), "u")
})

test_that("character accessors keep character type and input names", {
  urls <- c("http://www.example.com/Path", "ftp://example.org")
  for (fn in list(
    get_scheme, get_host, get_path, get_query, get_fragment,
    get_user, get_password, get_userinfo, get_domain, get_tld,
    get_clean_url, get_parse_status
  )) {
    out <- fn(urls)
    expect_type(out, "character")
    expect_named(out, urls)
    expect_length(out, 2)
  }
})

test_that("get_port returns a named integer vector", {
  out <- get_port(c("http://example.com:8080", "http://example.com"))
  expect_type(out, "integer")
  expect_equal(unname(out), c(8080L, NA_integer_))
  expect_equal(names(out)[1], "http://example.com:8080")
})

test_that("get_query(format='list') still returns a list", {
  out <- get_query("http://example.com/p?a=1&b=2", format = "list")
  expect_type(out, "list")
  expect_equal(out[[1]]$a, "1")
  expect_equal(out[[1]]$b, "2")
})

test_that("get_subdomain preserves string and labels return shapes", {
  expect_type(get_subdomain("http://www.blog.example.co.uk"), "character")
  labels <- get_subdomain("http://www.blog.example.co.uk", format = "labels")
  expect_type(labels, "list")
  expect_equal(labels[[1]], c("www", "blog"))
})

test_that("get_domain source variants route through the shared helper", {
  expect_equal(
    unname(get_domain("http://a.b.example.co.uk", source = "all")),
    "example.co.uk"
  )
  expect_true(is.na(get_domain("http://192.168.0.1", source = "icann")))
})
