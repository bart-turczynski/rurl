# Tests for query_param_summary(): the query-introspection accessor.

test_that("level = 'param' tabulates distinct params with n vs n_urls", {
  urls <- c(
    "http://example.com/?utm_source=nl&id=42",
    "http://example.com/watch?v=abc&utm_source=x",
    "http://example.com/?id=99"
  )
  res <- query_param_summary(urls)
  expect_s3_class(res, "data.frame")
  expect_named(
    res,
    c("param", "n", "n_urls", "example_value", "example_url", "would_drop")
  )
  # First-seen order of the distinct param names.
  expect_identical(res$param, c("utm_source", "id", "v"))
  # utm_source: two occurrences across two URLs.
  utm <- res[res$param == "utm_source", ]
  expect_identical(utm$n, 2L)
  expect_identical(utm$n_urls, 2L)
  expect_identical(utm$example_value, "nl")
  expect_identical(utm$example_url, "http://example.com/?utm_source=nl&id=42")
})

test_that("n counts occurrences while n_urls counts distinct URLs", {
  # id appears twice in ONE URL -> n = 2 but n_urls = 1.
  res <- query_param_summary("http://e.com/?id=1&id=2")
  expect_identical(res$n, 2L)
  expect_identical(res$n_urls, 1L)
})

test_that("level = 'value' emits one row per distinct (param, value)", {
  urls <- c("http://e.com/?a=1&a=2&b=9", "http://e.com/?a=1")
  res <- query_param_summary(urls, level = "value")
  expect_named(
    res,
    c("param", "value", "n", "n_urls", "example_url", "would_drop")
  )
  a1 <- res[res$param == "a" & res$value == "1", ]
  expect_identical(a1$n, 2L) # a=1 in both URLs
  expect_identical(a1$n_urls, 2L)
  a2 <- res[res$param == "a" & res$value == "2", ]
  expect_identical(a2$n, 1L)
  # b appears once.
  expect_identical(res[res$param == "b", ]$value, "9")
})

test_that("param names are grouped faithfully (case-sensitively)", {
  res <- query_param_summary("http://e.com/?utm_source=a&UTM_SOURCE=b")
  # Both spellings survive as distinct rows (first-seen order).
  expect_identical(res$param, c("utm_source", "UTM_SOURCE"))
})

test_that("would_drop previews FILTER mode honouring params_case_sensitive", {
  urls <- "http://e.com/?utm_source=a&UTM_SOURCE=b&id=1"
  # Case-insensitive (default): both spellings match the denylist.
  res <- query_param_summary(urls)
  expect_true(res[res$param == "utm_source", ]$would_drop)
  expect_true(res[res$param == "UTM_SOURCE", ]$would_drop)
  expect_false(res[res$param == "id", ]$would_drop)
  # Case-sensitive: the upper-case spelling no longer matches.
  res_cs <- query_param_summary(urls, params_case_sensitive = TRUE)
  expect_true(res_cs[res_cs$param == "utm_source", ]$would_drop)
  expect_false(res_cs[res_cs$param == "UTM_SOURCE", ]$would_drop)
})

test_that("would_drop honours params_drop and params_keep", {
  urls <- "http://e.com/?keepme=1&junk=2&utm_source=x"
  res <- query_param_summary(
    urls, params_drop = "junk", params_keep = "utm_source"
  )
  expect_true(res[res$param == "junk", ]$would_drop) # added to denylist
  expect_false(res[res$param == "utm_source", ]$would_drop) # rescued
  expect_false(res[res$param == "keepme", ]$would_drop)
})

test_that("empty_param_handling = 'drop' flags empty-valued params", {
  urls <- "http://e.com/?ref=&id=1"
  res <- query_param_summary(urls, empty_param_handling = "drop")
  expect_true(res[res$param == "ref", ]$would_drop)
  expect_false(res[res$param == "id", ]$would_drop)
  # params_keep rescues an empty param over empty-dropping.
  res_keep <- query_param_summary(
    urls, empty_param_handling = "drop", params_keep = "ref"
  )
  expect_false(res_keep[res_keep$param == "ref", ]$would_drop)
})

test_that("values are decoded before grouping; decode_plus honoured", {
  # %26 decodes to '&' and groups by the decoded form.
  res <- query_param_summary("http://e.com/?a=x%26y", level = "value")
  expect_identical(res$value, "x&y")
  # decode_plus converts '+' to space in values only.
  res_plus <- query_param_summary(
    "http://e.com/?q=a+b", level = "value", decode_plus = TRUE
  )
  expect_identical(res_plus$value, "a b")
  res_noplus <- query_param_summary(
    "http://e.com/?q=a+b", level = "value", decode_plus = FALSE
  )
  expect_identical(res_noplus$value, "a+b")
})

test_that("opaque tokens group by their raw form", {
  # Malformed %zz (curl upper-cases to %ZZ) is opaque and grouped raw.
  res <- query_param_summary(
    c("http://e.com/?a=%zz", "http://e.com/?a=%zz"),
    level = "value"
  )
  expect_identical(res$value, "%ZZ")
  expect_identical(res$n, 2L)
})

test_that("multi-valued params expand to multiple value-level rows", {
  res <- query_param_summary("http://e.com/?tag=x&tag=y&tag=z", level = "value")
  expect_identical(res$value, c("x", "y", "z"))
  # ... but collapse to a single param-level row.
  res_p <- query_param_summary("http://e.com/?tag=x&tag=y&tag=z")
  expect_identical(nrow(res_p), 1L)
  expect_identical(res_p$n, 3L)
})

test_that("example_* fields are the first-seen occurrence", {
  urls <- c("http://e.com/?p=first", "http://e.com/?p=second")
  res <- query_param_summary(urls)
  expect_identical(res$example_value, "first")
  expect_identical(res$example_url, "http://e.com/?p=first")
})

test_that("URLs without a query contribute no rows", {
  urls <- c("http://e.com/nopath", "http://e.com/?id=1", "notaurl")
  res <- query_param_summary(urls)
  expect_identical(res$param, "id")
  expect_identical(res$n_urls, 1L)
})

test_that("an empty result keeps the level's column shape", {
  res_p <- query_param_summary(c("http://e.com/nopath", "x"))
  expect_identical(nrow(res_p), 0L)
  expect_named(
    res_p,
    c("param", "n", "n_urls", "example_value", "example_url", "would_drop")
  )
  res_v <- query_param_summary(character(0), level = "value")
  expect_identical(nrow(res_v), 0L)
  expect_named(
    res_v,
    c("param", "value", "n", "n_urls", "example_url", "would_drop")
  )
  expect_type(res_v$n, "integer")
  expect_type(res_v$would_drop, "logical")
})

test_that("query_param_summary rejects non-character input", {
  expect_error(query_param_summary(42), "character vector")
})
