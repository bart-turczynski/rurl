# --- Helper Data for Tests ---
# Consistent helper data names
A_data_match <- data.frame(
  URL = c("http://example.com/page1", "http://sample.org/another"),
  OtherColA = c("valA1", "valA2"),
  stringsAsFactors = FALSE
)

B_data_match <- data.frame(
  URL = c("http://example.com/page1/", "http://different.net/something"), # One match with A_data_match
  OtherColB = c("valB1", "valB2"),
  stringsAsFactors = FALSE
)

A_data_no_match <- data.frame(
  URL = c("http://unique-a.com"),
  OtherColA = "valA_unique",
  stringsAsFactors = FALSE
)

B_data_no_match <- data.frame(
  URL = c("http://unique-b.com"),
  OtherColB = "valB_unique",
  stringsAsFactors = FALSE
)

empty_df <- data.frame(URL = character(0), OtherCol = character(0), stringsAsFactors = FALSE) # Ensure OtherCol for structure

# --- Test Cases ---

test_that("permutation_join works with a basic match", {
  res <- permutation_join(A_data_match, B_data_match)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("A_data_match", "B_data_match", "JoinKey", "OtherColA_A", "OtherColB_B"), ignore.order = TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(res$A_data_match[1], "http://example.com/page1")
  expect_equal(res$B_data_match[1], "http://example.com/page1/")
  expect_equal(res$JoinKey[1], "http://example.com/page1")
  expect_equal(res$OtherColA_A[1], "valA1")
  expect_equal(res$OtherColB_B[1], "valB1")
})

test_that("permutation_join handles no matches", {
  res <- permutation_join(A_data_no_match, B_data_no_match)

  expect_s3_class(res, "data.frame")
  # Expect specific names even for 0 rows, based on permutation_join_empty_result_structure
  expect_named(res, c("A_data_no_match", "B_data_no_match", "JoinKey", "OtherColA_A", "OtherColB_B"), ignore.order = TRUE)
  expect_equal(nrow(res), 0)
})

test_that("permutation_join handles empty data_A input", {
  res <- permutation_join(empty_df, B_data_match, col_A = "URL", col_B = "URL") # Specify cols for empty_df

  expect_s3_class(res, "data.frame")
  expect_named(res, c("empty_df", "B_data_match", "JoinKey", "OtherCol_A", "OtherColB_B"), ignore.order = TRUE)
  expect_equal(nrow(res), 0)
})

test_that("permutation_join handles empty data_B input", {
  res <- permutation_join(A_data_match, empty_df, col_A = "URL", col_B = "URL")

  expect_s3_class(res, "data.frame")
  expect_named(res, c("A_data_match", "empty_df", "JoinKey", "OtherColA_A", "OtherCol_B"), ignore.order = TRUE)
  expect_equal(nrow(res), 0)
})

test_that("permutation_join handles both inputs empty", {
  res <- permutation_join(empty_df, empty_df, col_A = "URL", col_B = "URL")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("empty_df", "JoinKey", "OtherCol_A", "OtherCol_B"), ignore.order = TRUE)
  expect_equal(nrow(res), 0)
})

test_that("permutation_join warns and returns empty structure for non-data.frame inputs", {
  # Test with data_A as non-df
  expect_warning(
    res_A_invalid <- permutation_join(list(), B_data_match),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_A_invalid, "data.frame")
  expect_equal(length(names(res_A_invalid)), 0)
  expect_equal(nrow(res_A_invalid), 0)
  
  # Test with data_B as non-df
  expect_warning(
    res_B_invalid <- permutation_join(A_data_match, "not_a_df"),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_B_invalid, "data.frame")
  expect_equal(length(names(res_B_invalid)), 0)
  expect_equal(nrow(res_B_invalid), 0)
})

test_that("permutation_join warns for missing URL columns", {
  A_no_URL <- data.frame(X = "a")
  B_no_URL <- data.frame(Y = "b")

  # A missing URL col
  expect_warning(
    res_A <- permutation_join(A_no_URL, B_data_match, col_A = "URL"), # col_A="URL" is default
    regexp = "Column 'URL' not found in data_A."
  )
  expect_s3_class(res_A, "data.frame")
  expect_equal(length(names(res_A)), 0)
  expect_equal(nrow(res_A), 0)

  # B missing URL col
  expect_warning(
    res_B <- permutation_join(A_data_match, B_no_URL, col_B = "URL"), # col_B="URL" is default
    regexp = "Column 'URL' not found in data_B."
  )
  expect_s3_class(res_B, "data.frame")
  expect_equal(length(names(res_B)), 0)
  expect_equal(nrow(res_B), 0)
})

test_that("permutation_join warns for URL columns of incorrect type", {
  A_URL_not_char <- data.frame(URL = 123, OtherColA = "val", stringsAsFactors = FALSE)

  expect_warning(
    res <- permutation_join(A_URL_not_char, B_data_match),
    regexp = "Column 'URL' in data_A must be character or factor."
  )
  expect_s3_class(res, "data.frame")
  expect_equal(length(names(res)), 0) # Expect character(0) for names due to early return
  expect_equal(nrow(res), 0)
})

test_that("permutation_join handles custom URL column names", {
  A_custom_col <- data.frame(MyURL_A = "http://example.com/custom_a", OtherColA = "valA", stringsAsFactors = FALSE)
  B_custom_col <- data.frame(MyURL_B = "http://example.com/custom_a", OtherColB = "valB", stringsAsFactors = FALSE)

  res <- permutation_join(A_custom_col, B_custom_col, col_A = "MyURL_A", col_B = "MyURL_B")

  expect_s3_class(res, "data.frame")
  expect_named(res, c("A_custom_col", "B_custom_col", "JoinKey", "OtherColA_A", "OtherColB_B"), ignore.order = TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(res$A_custom_col[1], "http://example.com/custom_a")
  expect_equal(res$JoinKey[1], "http://example.com/custom_a")
  expect_equal(res$OtherColA_A[1], "valA")
})

test_that("permutation_join handles custom suffixes", {
  res <- permutation_join(A_data_match, B_data_match, suffix_A = ".left", suffix_B = ".right")
  expect_named(res, c("A_data_match", "B_data_match", "JoinKey", "OtherColA.left", "OtherColB.right"), ignore.order = TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(res$"OtherColA.left"[1], "valA1")
  expect_equal(res$"OtherColB.right"[1], "valB1")
})

# Test for when input data.frames have columns that would lead to name clashes after deparse(substitute())
# For example, if data_A is named 'df' and data_B is named 'df1'.
test_that("permutation_join handles tricky deparse(substitute()) names for empty results", {
  df <- data.frame(URL=character(), stringsAsFactors = FALSE)
  df1 <- data.frame(URL=character(), stringsAsFactors = FALSE)
  res <- permutation_join(df, df1)
  expect_named(res, c("df", "df1", "JoinKey"), ignore.order = TRUE)
  expect_equal(nrow(res), 0)

  # Case for non-dataframe inputs that result in identical deparsed names
  # This scenario is complex as deparse(substitute(list())) gives "list()"
  # If both were list(), the internal empty structure might de-duplicate or error.
  # The current empty structure function doesn't explicitly handle name_A == name_B.
  # Let's assume for now distinct deparsed names for simplicity in this test edge case.
})

# Test with a direct pipe to ensure deparse(substitute()) works
test_that("permutation_join works with piped inputs", {
  A_pipe <- A_data_match
  B_pipe <- B_data_match

  result <- A_pipe %>% permutation_join(B_pipe)

  # Note: deparse(substitute(A_pipe)) might become "." or the variable name
  # depending on pipe implementation and environment.
  # We expect the variable names if not directly piped as first arg.
  # If A_pipe is the first arg to the pipe, its name might be "."
  # This test is more about ensuring it runs; exact name checking for "." is tricky.
  # Let's check for the presence of JoinKey and original values.
  
  expect_s3_class(result, "data.frame")
  expect_true("JoinKey" %in% names(result))
  expect_equal(nrow(result), 1)

  # Check that original values are there, regardless of exact 'A_pipe'/'B_pipe' column name
  # This requires knowing which column corresponds to A_pipe's URL
  # This is complex to assert generally for piped inputs without knowing the exact deparse result
  # For now, ensuring it runs and finds a match is a good step.
  # If first arg to pipe, name_A can be "."
   if ("." %in% names(result)) {
     expect_equal(result[[".", exact=FALSE]][1], "http://example.com/page1")
     expect_true("B_pipe" %in% names(result))
     expect_equal(result$B_pipe[1], "http://example.com/page1/")
   } else { # Assuming standard variable names were captured
     expect_true("A_pipe" %in% names(result))
     expect_true("B_pipe" %in% names(result))
     expect_equal(result$A_pipe[1], "http://example.com/page1")
     expect_equal(result$B_pipe[1], "http://example.com/page1/")
   }
  expect_equal(result$JoinKey[1], "http://example.com/page1")
  # Also check for other columns with expected suffixes
  # (e.g. OtherColA_A or OtherColA_., OtherColB_B)
  expect_true(any(grepl("OtherColA", names(result))))
  expect_true(any(grepl("OtherColB", names(result))))
}) 