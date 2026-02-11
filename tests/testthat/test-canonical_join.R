test_that("canonical_join matches on clean_url", {
  A <- data.frame(
    URL = c("http://Example.com/Page", "http://example.com/Other"),
    ValA = 1:2,
    stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = c("https://www.example.com/Page/", "http://example.com/Miss"),
    ValB = c("x", "y"),
    stringsAsFactors = FALSE
  )

  res <- canonical_join(
    A, B,
    protocol_handling = "strip",
    www_handling = "strip",
    case_handling = "lower_host",
    trailing_slash_handling = "strip",
    join = "inner"
  )

  expect_equal(nrow(res), 1)
  expect_equal(res$A[1], "http://Example.com/Page")
  expect_equal(res$B[1], "https://www.example.com/Page/")
  expect_equal(res$JoinKey[1], "example.com/Page")
  expect_equal(res$ValA_A[1], 1)
  expect_equal(res$ValB_B[1], "x")
})

test_that("canonical_join handles collisions via first/all", {
  A <- data.frame(
    URL = c("http://example.com/a", "https://example.com/a/"),
    Id = 1:2,
    stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "http://example.com/a",
    Val = "x",
    stringsAsFactors = FALSE
  )

  res_first <- canonical_join(
    A, B,
    protocol_handling = "strip",
    trailing_slash_handling = "strip",
    collision = "first",
    join = "inner"
  )
  expect_equal(nrow(res_first), 1)
  expect_equal(res_first$Id_A[1], 1)

  res_all <- canonical_join(
    A, B,
    protocol_handling = "strip",
    trailing_slash_handling = "strip",
    collision = "all",
    join = "inner"
  )
  expect_equal(nrow(res_all), 2)
  expect_equal(sort(res_all$Id_A), c(1, 2))
})

test_that("canonical_join keeps parse errors when requested", {
  A <- data.frame(
    URL = c("http://example.com/a", "mailto:x@example.com"),
    ValA = c(1, 2),
    stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "http://example.com/a",
    ValB = "x",
    stringsAsFactors = FALSE
  )

  res <- canonical_join(
    A, B,
    protocol_handling = "strip",
    trailing_slash_handling = "strip",
    on_parse_error = "keep",
    join = "left"
  )

  expect_equal(nrow(res), 2)
  expect_true(any(is.na(res$JoinKey)))
  expect_true(any(is.na(res$B)))
})

test_that("canonical_join supports right/full joins and parse-error handling", {
  A <- data.frame(
    URL = c("mailto:x@example.com", "http://example.com/a"),
    ValA = c(1, 2),
    stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = c("http://example.com/a", "http://example.com/b"),
    ValB = c("x", "y"),
    stringsAsFactors = FALSE
  )

  res_right <- canonical_join(
    A, B,
    protocol_handling = "strip",
    trailing_slash_handling = "strip",
    on_parse_error = "keep",
    join = "right"
  )

  expect_equal(nrow(res_right), 2)
  expect_true(any(res_right$JoinKey == "example.com/b"))
  expect_true(any(is.na(res_right$A)))

  res_drop <- canonical_join(
    A, B,
    protocol_handling = "strip",
    trailing_slash_handling = "strip",
    on_parse_error = "drop",
    join = "full"
  )

  expect_equal(nrow(res_drop), 2)
  expect_false(any(is.na(res_drop$A) & is.na(res_drop$B)))

  expect_error(
    canonical_join(A, B, on_parse_error = "error"),
    "parsing errors"
  )
})

test_that("canonical_join collision = error stops on duplicates", {
  A <- data.frame(
    URL = c("http://example.com/a", "https://example.com/a/"),
    stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "http://example.com/a",
    stringsAsFactors = FALSE
  )

  expect_error(
    canonical_join(
      A, B,
      protocol_handling = "strip",
      trailing_slash_handling = "strip",
      collision = "error"
    ),
    "duplicate canonical keys"
  )
})

test_that("canonical_join validates inputs and columns", {
  expect_warning(
    res <- canonical_join(list(), data.frame(URL = "a", stringsAsFactors = FALSE)),
    "data frames"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)

  expect_warning(
    res2 <- canonical_join(data.frame(X = "a", stringsAsFactors = FALSE),
                           data.frame(URL = "b", stringsAsFactors = FALSE),
                           col_A = "URL"),
    "not found"
  )
  expect_s3_class(res2, "data.frame")
  expect_equal(nrow(res2), 0)

  expect_warning(
    res3 <- canonical_join(data.frame(URL = 1, stringsAsFactors = FALSE),
                           data.frame(URL = "b", stringsAsFactors = FALSE)),
    "must be character"
  )
  expect_s3_class(res3, "data.frame")
  expect_equal(nrow(res3), 0)

  expect_warning(
    res4 <- canonical_join(data.frame(URL = "a", stringsAsFactors = FALSE),
                           data.frame(X = "b", stringsAsFactors = FALSE),
                           col_B = "URL"),
    "not found"
  )
  expect_s3_class(res4, "data.frame")
  expect_equal(nrow(res4), 0)

  expect_warning(
    res5 <- canonical_join(data.frame(URL = "a", stringsAsFactors = FALSE),
                           data.frame(URL = 1, stringsAsFactors = FALSE)),
    "must be character"
  )
  expect_s3_class(res5, "data.frame")
  expect_equal(nrow(res5), 0)
})

test_that("canonical_join returns empty structure on no matches", {
  A <- data.frame(URL = "http://example.com/a", ValA = 1, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://example.com/b", ValB = "x", stringsAsFactors = FALSE)

  res <- canonical_join(
    A, B,
    protocol_handling = "strip",
    trailing_slash_handling = "strip",
    join = "inner"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_true(all(c("A", "B", "JoinKey") %in% names(res)))
})
