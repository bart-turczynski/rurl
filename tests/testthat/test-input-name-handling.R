# Input NAMES are not data. A named input vector must not leak its names into
# the public return surface: result frames get ordinary sequential row names and
# accessors return unnamed vectors. Both halves are pinned here (RURL-vhdsqaln).
#
# The pre-existing assertions in test-accessor-helper.R (:9, :46, :55) check
# `expect_null(names(out))` but pass UNNAMED input, so they are vacuous for this
# property -- they hold whether or not names are stripped. Every test below
# passes a NAMED input on purpose; that is the whole point of the file.
#
# Leaked row names are a silent correctness hazard rather than a cosmetic one:
# they survive into joins and downstream frames as though they were a column.

named_urls <- c(
  a = "http://example.com/p?x=1",
  b = "http://sub.example.org/i.html"
)
plain_urls <- unname(named_urls)

test_that("safe_parse_urls() gives sequential row names for named input", {
  res <- safe_parse_urls(named_urls)
  expect_identical(rownames(res), c("1", "2"))
  # The automatic (compact) integer form, not character row names that merely
  # happen to read as digits.
  expect_identical(attr(res, "row.names"), 1:2)
})

test_that("safe_parse_urls() is identical for named and unnamed input", {
  expect_identical(safe_parse_urls(named_urls), safe_parse_urls(plain_urls))
})

test_that("safe_parse_urls() columns carry no names for named input", {
  res <- safe_parse_urls(named_urls)
  has_names <- vapply(res, function(x) !is.null(names(x)), logical(1))
  expect_identical(names(res)[has_names], character(0))
})

test_that("safe_parse_urls() strips names from a named list input too", {
  res <- safe_parse_urls(as.list(named_urls))
  expect_identical(rownames(res), c("1", "2"))
  expect_identical(attr(res, "row.names"), 1:2)
  expect_identical(res, safe_parse_urls(as.list(plain_urls)))
})

test_that("safe_parse_urls() strips names from a named factor input", {
  res <- safe_parse_urls(factor(named_urls))
  expect_identical(attr(res, "row.names"), 1:2)
  expect_identical(res, safe_parse_urls(plain_urls))
})

test_that("safe_parse_url() carries no names into the scalar field list", {
  res <- safe_parse_url(c(a = "http://example.com/p?x=1"))
  inner <- unique(unlist(lapply(res, names)))
  expect_null(inner)
  expect_identical(res, safe_parse_url("http://example.com/p?x=1"))
})

test_that("character accessors return unnamed vectors for NAMED input", {
  for (fn in list(
    get_scheme, get_host, get_path, get_query, get_fragment,
    get_user, get_password, get_userinfo, get_domain, get_tld,
    get_clean_url, get_parse_status, get_subdomain
  )) {
    out <- fn(named_urls)
    expect_null(names(out))
    expect_identical(out, fn(plain_urls))
  }
})

test_that("selector-gated accessors return unnamed vectors for NAMED input", {
  # Same property, separate loop: these two require url_standard (ADR 0015),
  # so they cannot be called with the single positional argument above.
  for (fn in list(get_host_type, get_scheme_class)) {
    out <- fn(named_urls, url_standard = "whatwg")
    expect_null(names(out))
    expect_identical(out, fn(plain_urls, url_standard = "whatwg"))
  }
})

test_that("get_port() returns an unnamed integer vector for NAMED input", {
  out <- get_port(c(a = "http://example.com:8080", b = "http://example.com"))
  expect_null(names(out))
  expect_identical(out, c(8080L, NA_integer_))
})

test_that("resolve_url() is unnamed for named relative and named base input", {
  base <- "http://example.com/base/"
  out <- resolve_url(c(a = "/p", b = "q.html"), base)
  expect_null(names(out))
  expect_identical(out, resolve_url(c("/p", "q.html"), base))
  expect_null(names(resolve_url("/p", c(z = base))))
})

test_that("companion frame helpers give sequential row names for named input", {
  hosts <- c(a = "example.com", b = "sub.example.org")
  expect_identical(attr(check_hosts(hosts), "row.names"), 1:2)
  expect_identical(check_hosts(hosts), check_hosts(unname(hosts)))

  # query_param_summary() aggregates one row per PARAMETER, not per URL, so the
  # row count is asserted off nrow() rather than the input length.
  qs <- query_param_summary(named_urls)
  expect_identical(attr(qs, "row.names"), seq_len(nrow(qs)))
  expect_identical(qs, query_param_summary(plain_urls))
})

test_that("get_url_diagnostics() carries no names for named input", {
  diag <- get_url_diagnostics(named_urls, url_standard = "whatwg")
  expect_null(unique(unlist(lapply(diag, names))))
  expect_identical(
    diag, get_url_diagnostics(plain_urls, url_standard = "whatwg")
  )
})
