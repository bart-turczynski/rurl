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
    res <- canonical_join(
      list(),
      data.frame(URL = "a", stringsAsFactors = FALSE)
    ),
    "data frames"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)

  expect_warning(
    res2 <- canonical_join(data.frame(X = "a", stringsAsFactors = FALSE),
      data.frame(URL = "b", stringsAsFactors = FALSE),
      col_A = "URL"
    ),
    "not found"
  )
  expect_s3_class(res2, "data.frame")
  expect_equal(nrow(res2), 0)

  expect_warning(
    res3 <- canonical_join(
      data.frame(URL = 1, stringsAsFactors = FALSE),
      data.frame(URL = "b", stringsAsFactors = FALSE)
    ),
    "must be character"
  )
  expect_s3_class(res3, "data.frame")
  expect_equal(nrow(res3), 0)

  expect_warning(
    res4 <- canonical_join(data.frame(URL = "a", stringsAsFactors = FALSE),
      data.frame(X = "b", stringsAsFactors = FALSE),
      col_B = "URL"
    ),
    "not found"
  )
  expect_s3_class(res4, "data.frame")
  expect_equal(nrow(res4), 0)

  expect_warning(
    res5 <- canonical_join(
      data.frame(URL = "a", stringsAsFactors = FALSE),
      data.frame(URL = 1, stringsAsFactors = FALSE)
    ),
    "must be character"
  )
  expect_s3_class(res5, "data.frame")
  expect_equal(nrow(res5), 0)
})

test_that("canonical_join returns empty structure on no matches", {
  A <- data.frame(
    URL = "http://example.com/a",
    ValA = 1,
    stringsAsFactors = FALSE
  )
  B <- data.frame(
    URL = "http://example.com/b",
    ValB = "x",
    stringsAsFactors = FALSE
  )

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

test_that("canonical_join uses explicit name_A / name_B for output columns", {
  A <- data.frame(URL = "http://example.com/a", ValA = 1, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://example.com/a", ValB = "x", stringsAsFactors = FALSE)

  res <- canonical_join(
    A, B,
    name_A = "left_url", name_B = "right_url",
    protocol_handling = "strip"
  )

  expect_equal(nrow(res), 1)
  expect_true(all(c("left_url", "right_url", "JoinKey") %in% names(res)))
  expect_equal(res$left_url[1], "http://example.com/a")
  expect_equal(res$right_url[1], "http://example.com/a")
})

test_that("canonical_join yields stable names for piped / anonymous inputs", {
  make_a <- function() {
    data.frame(URL = "http://example.com/a", ValA = 1, stringsAsFactors = FALSE)
  }
  make_b <- function() {
    data.frame(URL = "http://example.com/a", ValB = "x", stringsAsFactors = FALSE)
  }

  # Without explicit names, deparse(substitute()) yields the call expression,
  # which data.frame() then mangles into a non-syntactic name — unstable.
  res_default <- canonical_join(make_a(), make_b(), protocol_handling = "strip")
  expect_true("make_a.." %in% names(res_default))
  expect_true("make_b.." %in% names(res_default))

  # Explicit names are stable regardless of the input expression.
  res_named <- canonical_join(
    make_a(), make_b(),
    name_A = "A", name_B = "B",
    protocol_handling = "strip"
  )
  expect_true(all(c("A", "B") %in% names(res_named)))
})

test_that("canonical_join join_parse_status defaults to ok-only", {
  A <- data.frame(URL = "http://internalhost/path", ValA = 1, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://internalhost/path", ValB = "x", stringsAsFactors = FALSE)

  # warning-no-tld hosts are not joinable by default.
  res <- canonical_join(A, B, name_A = "A", name_B = "B", join = "inner")
  expect_equal(nrow(res), 0)
})

test_that("canonical_join join_parse_status = 'ok_or_warning' joins warning statuses", {
  A <- data.frame(URL = "http://internalhost/path", ValA = 1, stringsAsFactors = FALSE)
  B <- data.frame(URL = "http://internalhost/path", ValB = "x", stringsAsFactors = FALSE)

  res <- canonical_join(
    A, B,
    name_A = "A", name_B = "B",
    join = "inner",
    join_parse_status = "ok_or_warning"
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$JoinKey[1], "http://internalhost/path")
  expect_equal(res$ValA[1], 1)
  expect_equal(res$ValB[1], "x")
})
