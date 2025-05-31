# --- Helper Data for Tests ---

# Basic valid data for A
perms_df_a1_valid <- data.frame(Permutations = c("http://ex.com/p1", "http://ex.com/p2"),
                                OtherColA = 1:2, stringsAsFactors = FALSE)
perms_df_a2_valid <- data.frame(Permutations = c("http://samp.org/a", "http://samp.org/b"),
                                OtherColA = 3:4, stringsAsFactors = FALSE)
A_valid <- data.frame(URL = c("http://example.com", "http://sample.org"),
                    stringsAsFactors = FALSE)
A_valid$Permutation <- list(perms_df_a1_valid, perms_df_a2_valid)

# Basic valid data for B (with a different "other" column)
perms_df_b1_valid <- data.frame(Permutations = c("http://test.net/x", "http://test.net/y"),
                                OtherColB = c("foo", "bar"), stringsAsFactors = FALSE)
B_valid <- data.frame(URL = c("http://test.net"), stringsAsFactors = FALSE)
B_valid$Permutation <- list(perms_df_b1_valid)

# Empty data frame structure (matching expected output of flatten_perms for empty inputs)
empty_df_input <- data.frame(URL = character(0), stringsAsFactors = FALSE)
empty_df_input$Permutation <- vector("list", 0)


# --- Test Cases ---

test_that("permutation_join works with valid basic inputs", {
  result <- permutation_join(A_valid, B_valid)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("A_valid", "B_valid", "JoinKey", "OtherColA", "OtherColB"), ignore.order = TRUE)
  expect_equal(nrow(result), 6) # 4 from A, 2 from B
  # Check specific values if necessary, e.g. first row from A
  expect_equal(result$A_valid[1], "http://example.com")
  expect_true(is.na(result$B_valid[1]))
  expect_equal(result$JoinKey[1], "http://example.com")
  expect_equal(result$OtherColA[1], 1)
  expect_true(is.na(result$OtherColB[1]))
  # Check specific values from B
  expect_true(is.na(result$A_valid[5]))
  expect_equal(result$B_valid[5], "http://test.net")
  expect_equal(result$JoinKey[5], "http://test.net")
  expect_equal(result$OtherColB[5], "foo")
  expect_true(is.na(result$OtherColA[5]))
})

test_that("permutation_join handles one empty input correctly (A empty)", {
  result <- permutation_join(empty_df_input, B_valid)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("empty_df_input", "B_valid", "JoinKey", "OtherColB"), ignore.order = TRUE)
  expect_equal(nrow(result), 2) # 0 from A, 2 from B
  expect_true(all(is.na(result$empty_df_input)))
  expect_equal(result$B_valid, rep("http://test.net", 2))
  expect_equal(result$JoinKey, rep("http://test.net", 2))
  expect_equal(result$OtherColB, c("foo", "bar"))
})

test_that("permutation_join handles one empty input correctly (B empty)", {
  result <- permutation_join(A_valid, empty_df_input)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("A_valid", "empty_df_input", "JoinKey", "OtherColA"), ignore.order = TRUE)
  expect_equal(nrow(result), 4) # 4 from A, 0 from B
  expect_equal(result$A_valid, rep(c("http://example.com", "http://example.com", "http://sample.org", "http://sample.org"), length.out = 4))
  expect_true(all(is.na(result$empty_df_input)))
  expect_equal(result$JoinKey, result$A_valid)
  expect_equal(result$OtherColA, c(1, 2, 3, 4))
})

test_that("permutation_join handles both inputs empty", {
  result <- permutation_join(empty_df_input, empty_df_input)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("empty_df_input", "empty_df_input.1", "JoinKey"), ignore.order = TRUE)
  expect_equal(nrow(result), 0)
  expect_true(is.character(result$JoinKey))
  expect_true(is.character(result$empty_df_input))
  expect_true(is.character(result$empty_df_input.1))
})

# --- Input Validation Tests ---

test_that("permutation_join returns empty data.frame and warns for NULL inputs", {
  expect_warning(
    res_A_null <- permutation_join(NULL, B_valid),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_A_null, "data.frame")
  expect_equal(nrow(res_A_null), 0)
  expect_warning(
    res_B_null <- permutation_join(A_valid, NULL),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_B_null, "data.frame")
  expect_equal(nrow(res_B_null), 0)
  expect_warning(
    res_both_null <- permutation_join(NULL, NULL),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_both_null, "data.frame")
  expect_equal(nrow(res_both_null), 0)
})

test_that("permutation_join returns NULL and warns for non-data.frame inputs", {
  expect_warning(
    res_A_invalid <- permutation_join(list(), B_valid),
    "Input 'A' is not a data.frame or tibble.", fixed = TRUE
  )
  expect_null(res_A_invalid)
  
  expect_warning(
    res_B_invalid <- permutation_join(A_valid, "not_a_df"),
    "Input 'B' is not a data.frame or tibble.", fixed = TRUE
  )
  expect_null(res_B_invalid)
})

test_that("permutation_join returns NULL and warns for missing required columns", {
  A_no_URL <- A_valid[, "Permutation", drop = FALSE]
  expect_warning(
    res_A_no_URL <- permutation_join(A_no_URL, B_valid),
    "Input 'A' is missing required column(s): URL", fixed = TRUE
  )
  expect_null(res_A_no_URL)
  
  A_no_Permutation <- A_valid[, "URL", drop = FALSE]
  expect_warning(
    res_A_no_Perm <- permutation_join(A_no_Permutation, B_valid),
    "Input 'A' is missing required column(s): Permutation", fixed = TRUE
  )
  expect_null(res_A_no_Perm)
})

test_that("permutation_join returns NULL and warns for incorrect column types", {
  A_URL_not_char <- A_valid
  A_URL_not_char$URL <- seq_len(nrow(A_URL_not_char))
  expect_warning(
    res_A_URL_type <- permutation_join(A_URL_not_char, B_valid),
    "Column 'URL' in input 'A' must be character.", fixed = TRUE
  )
  expect_null(res_A_URL_type)
  
  A_Perm_not_list <- A_valid
  A_Perm_not_list$Permutation <- "not_a_list"
  expect_warning(
    res_A_Perm_type <- permutation_join(A_Perm_not_list, B_valid),
    "Column 'Permutation' in input 'A' must be a list.", fixed = TRUE
  )
  expect_null(res_A_Perm_type)
})

test_that("permutation_join returns NULL for invalid Permutation list elements", {
  A_perm_elem_not_df <- A_valid
  A_perm_elem_not_df$Permutation[[1]] <- "not_a_df"
  expect_warning(
    res_A_perm_elem_type <- permutation_join(A_perm_elem_not_df, B_valid),
    "Element 1 of 'Permutation' in 'A' is not a data.frame.", fixed = TRUE
  )
  expect_null(res_A_perm_elem_type)

  A_perm_elem_missing_col <- A_valid
  df_no_perms_col <- data.frame(NotPermutations = "abc", stringsAsFactors = FALSE)
  A_perm_elem_missing_col$Permutation[[1]] <- df_no_perms_col
  expect_warning(
    res_A_perm_elem_missing <- permutation_join(A_perm_elem_missing_col, B_valid),
    paste0("Data.frame at element 1 of 'Permutation' in 'A'",
           " is non-empty and must have a 'Permutations' column."), fixed = TRUE
  )
  expect_null(res_A_perm_elem_missing)
})

# --- Tests for flatten_perms (via permutation_join) ---

test_that("flatten_perms handles Permutation list with NULL elements", {
  # Create a fresh copy for this test to avoid side effects
  A_data_for_null_test <- data.frame(URL = c("http://example.com", "http://sample.org"),
                                     stringsAsFactors = FALSE)
  # Explicitly create the list for Permutation column
  A_data_for_null_test$Permutation <- list(
    data.frame(Permutations = c("http://ex.com/p1", "http://ex.com/p2"), OtherColA = 1:2, stringsAsFactors = FALSE), # Valid first element
    NULL # Second element is NULL
  )
  
  result <- permutation_join(A_data_for_null_test, empty_df_input)
  expect_false(is.null(result))
  expect_equal(nrow(result), 2) # Should only process the first element
  expect_equal(result$Source, rep("http://example.com", 2)) # Source should be from the first URL
  expect_named(result, c("Perm", "OtherColA", "Source", "SourceSet"), ignore.order = TRUE)
})

test_that("flatten_perms handles Permutation list with empty data.frames", {
  A_with_empty_df_perm <- A_valid
  empty_inner_df <- data.frame(Permutations = character(0), OtherColA = character(0), stringsAsFactors = FALSE)
  A_with_empty_df_perm$Permutation[[1]] <- empty_inner_df # Modify first element of the copied A_valid
  
  result <- permutation_join(A_with_empty_df_perm, empty_df_input) # B is empty for simplicity
  expect_false(is.null(result))
  expect_equal(nrow(result), 2) # Processes perms_df_a2_valid (2 rows) from the second element of A_with_empty_df_perm
  expect_equal(result$Source, rep("http://sample.org", 2)) # Source corresponds to the second URL
})

test_that("flatten_perms handles inner DFs with varying columns (becomes NA)", {
  # A_valid already has OtherColA
  # B_valid has OtherColB
  result <- permutation_join(A_valid, B_valid)
  
  # For rows from A, OtherColB should be NA
  expect_true(all(is.na(result[result$SourceSet == "SetA", "OtherColB"])))
  expect_false(any(is.na(result[result$SourceSet == "SetA", "OtherColA"])))
  
  # For rows from B, OtherColA should be NA
  expect_true(all(is.na(result[result$SourceSet == "SetB", "OtherColA"])))
  expect_false(any(is.na(result[result$SourceSet == "SetB", "OtherColB"])))
})

test_that("validate_input catches inner DF missing 'Permutations' column", {
  A_inner_missing_perm_col <- data.frame(URL = "http://test.com", stringsAsFactors = FALSE)
  bad_perm_df <- data.frame(NotTheRightName = "a.com", stringsAsFactors = FALSE) # Non-empty
  A_inner_missing_perm_col$Permutation <- list(bad_perm_df)
 
  # Exact warning message expected from validate_input
  expected_literal_msg <- "Data.frame at element 1 of 'Permutation' in 'A' is non-empty and must have a 'Permutations' column."

  # Test that the warning is thrown AND that the function returns NULL
  expect_warning(
    res <- permutation_join(A_inner_missing_perm_col, empty_df_input),
    regexp = expected_literal_msg,
    fixed = TRUE # Use exact literal string matching
  )
  # After expect_warning, 'res' should hold the value returned by permutation_join if no error,
  # which is NULL in this case because validate_input returns FALSE.
  expect_null(res) 
})

test_that("permutation_join handles data.frames with zero rows correctly in lists", {
  A_zero_row_list_element <- data.frame(URL = "http://example.com", stringsAsFactors = FALSE)
  # Permutation for this one URL is a 0-row data.frame but with a ColX
  A_zero_row_list_element$Permutation <- list(data.frame(Permutations = character(0), ColX = integer(0), stringsAsFactors = FALSE))
  
  B_data <- B_valid # Use a valid B
  
  result <- permutation_join(A_zero_row_list_element, B_data)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  # Should only contain rows from B_data, as A's entry was an empty permutation df
  expect_equal(nrow(result), nrow(B_valid$Permutation[[1]])) # 2 rows from B
  expect_true(all(result$SourceSet == "SetB"))
  # ColX from A should exist (as all NA) and ColOtherB from B should exist
  expect_named(result, c("Perm", "OtherColB", "Source", "SourceSet", "ColX"), ignore.order = TRUE)
  expect_true(all(is.na(result$ColX[result$SourceSet == "SetB"]))) # ColX is NA for rows from B
})

test_that("permutation_join handles 0-row inputs with differing unique columns correctly", {
  # Test for lines 242-243: ensures types are taken from flat_A/flat_B if a col is unique to one of them
  # Main dataframes A and B have 1 row each. Their *Permutation* list element is a 0-row data.frame.
  A_df <- data.frame(URL = "urlA_0row_inner", stringsAsFactors = FALSE) # 1 row
  A_df$Permutation <- list( # List of 1 element for the 1 row in A_df
    data.frame(Permutations = character(0), ColA = integer(0), stringsAsFactors = FALSE) # The 0-row permutation df
  )
  
  B_df <- data.frame(URL = "urlB_0row_inner", stringsAsFactors = FALSE) # 1 row
  B_df$Permutation <- list( # List of 1 element for the 1 row in B_df
    data.frame(Permutations = character(0), ColB = numeric(0), stringsAsFactors = FALSE) # The 0-row permutation df
  )

  result <- permutation_join(A_df, B_df)
  
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0) # Resulting data.frame has 0 rows because inner perms were 0-row
  expect_named(result, c("Perm", "Source", "SourceSet", "ColA", "ColB"), ignore.order = TRUE)
  
  # Check types of the 0-length columns in the 0-row result data.frame
  expect_true(is.character(result$Perm))
  expect_true(is.character(result$Source))
  if ("SourceSet" %in% names(result)) { # SourceSet is factor due to line 298
      expect_true(is.factor(result$SourceSet), label = "result$SourceSet should be a factor in 0-row unique cols test")
      # No explicit level check here, but if it's a factor(character(0)), levels length is 0.
  }
  expect_true(is.integer(result$ColA)) # Preserved from A_df$Permutation[[1]]
  expect_true(is.numeric(result$ColB) && !is.integer(result$ColB)) # Preserved from B_df$Permutation[[1]]
})

test_that("permutation_join works when Permutation column is I(list())", {
  A_I_list <- data.frame(URL = "http://example.com")
  A_I_list$Permutation <- I(list(perms_df_a1_valid)) # Using I() to create list column

  result <- permutation_join(A_I_list, empty_df_input)
  expect_false(is.null(result))
  expect_equal(nrow(result), 2)
  expect_true(all(result$SourceSet == "SetA"))
})

test_that("validate_input catches non-empty inner DF missing 'Permutations' column", {
  A_malformed_inner <- data.frame(URL = "http://example.com/malformed", stringsAsFactors = FALSE)
  # This inner DF is non-empty but lacks the 'Permutations' column.
  malformed_df <- data.frame(NotTheRightColumnName = "p1", OtherData = "abc", stringsAsFactors = FALSE)
  A_malformed_inner$Permutation <- list(malformed_df)

  # A valid B (not strictly needed as A causes NULL return, but good for consistency)
  B_data <- B_valid 

  expected_warning_regexp <- paste0("Data.frame at element 1 of 'Permutation' in 'A'",
                                  " is non-empty and must have a 'Permutations' column.")

  expect_warning(
    result <- permutation_join(A_malformed_inner, B_data),
    regexp = expected_warning_regexp,
    fixed = TRUE
  )
  
  expect_null(result) # permutation_join returns NULL due to validate_input failure
})

test_that("flatten_perms handles 0-row inner DF missing 'Permutations' column by adding empty Perm", {
  A_0row_no_perm_col <- data.frame(URL = "http://example.com/0row_no_perm", stringsAsFactors = FALSE)
  # This inner DF is 0-row and lacks the 'Permutations' column.
  zero_row_missing_perm_df <- data.frame(SomeOtherCol = character(0), stringsAsFactors = FALSE)
  A_0row_no_perm_col$Permutation <- list(zero_row_missing_perm_df)

  # B is empty for simplicity, focusing on how A is processed
  result <- permutation_join(A_0row_no_perm_col, empty_df_input)

  expect_false(is.null(result))
  expect_equal(nrow(result), 0) # 0-row inner df results in 0 rows in output for this source
  # Key: 'Perm' column should exist, along with 'Source', 'SourceSet', and 'SomeOtherCol'.
  expect_named(result, c("Perm", "Source", "SourceSet", "SomeOtherCol"), ignore.order = TRUE)
  expect_true(is.character(result$Perm)) # Should be character(0)
  expect_true(is.character(result$Source))
  expect_true(is.character(result$SomeOtherCol)) # Type preserved from the 0-row df
  if ("SourceSet" %in% names(result)) {
    expect_true(is.factor(result$SourceSet)) # Should be factor(character(0))
  }
})

test_that("flatten_perms returns empty_base_df if all inner perms are NULL or become NULL", {
  A_all_perms_become_null <- data.frame(URL = c("url1", "url2"), stringsAsFactors = FALSE)
  A_all_perms_become_null$Permutation <- list(NULL, NULL) # All are NULL

  result_all_null <- permutation_join(A_all_perms_become_null, empty_df_input)
  expect_false(is.null(result_all_null))
  expect_s3_class(result_all_null, "data.frame")
  expect_named(result_all_null, c("Perm", "Source", "SourceSet"), ignore.order = TRUE) # from empty_base_df
  expect_equal(nrow(result_all_null), 0)

  # Case: Inner DFs are valid but result in no processable data due to missing 'Permutations'
  # This case is actually caught by validate_input before flatten_perms is meaningfully called with such data.
  # So, the primary way to hit line 182 is via all NULLs in the Permutation list for a non-empty main DF.
})

test_that("permutation_join handles rows with heterogeneous permutation columns", {
  # Test case where different rows within ONE input table (A) have permutation data.frames
  # that contain different "other" columns. This tests the NA-padding within flatten_perms.
  A_data_hetero_rows <- data.frame(URL = c("urlA_row1", "urlA_row2"), stringsAsFactors = FALSE) # 2 rows
  perms_A_row1 <- data.frame(Permutations = "pA.1", ColExtra1 = "val1_A", stringsAsFactors = FALSE)
  perms_A_row2 <- data.frame(Permutations = "pA.2", ColExtra2 = "val2_A", stringsAsFactors = FALSE)
  A_data_hetero_rows$Permutation <- list(perms_A_row1, perms_A_row2) # List of 2 DFs, one for each URL in A

  # Combine with a simple B to make the structure clear and ensure B's columns are also handled
  perms_df_b_simple <- data.frame(Permutations = "pB.1", ColExtraB = "valB_B", stringsAsFactors = FALSE)
  B_simple <- data.frame(URL = "url_B_simple", stringsAsFactors = FALSE) # 1 row
  B_simple$Permutation <- list(perms_df_b_simple)

  result <- permutation_join(A_data_hetero_rows, B_simple)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  # Expected columns: Perm, Source, SourceSet (core), ColExtra1 (from A), ColExtra2 (from A), ColExtraB (from B)
  expect_named(result, c("Perm", "Source", "SourceSet", "ColExtra1", "ColExtra2", "ColExtraB"), ignore.order = TRUE)
  expect_equal(nrow(result), 3) # 1 from A_row1, 1 from A_row2, 1 from B_simple

  # Check A's first row contribution (urlA_row1)
  row_A1 <- result[result$Source == "urlA_row1", ]
  expect_equal(row_A1$Perm, "pA.1")
  expect_equal(row_A1$ColExtra1, "val1_A")
  expect_true(is.na(row_A1$ColExtra2))
  expect_true(is.na(row_A1$ColExtraB))

  # Check A's second row contribution (urlA_row2)
  row_A2 <- result[result$Source == "urlA_row2", ]
  expect_equal(row_A2$Perm, "pA.2")
  expect_true(is.na(row_A2$ColExtra1))
  expect_equal(row_A2$ColExtra2, "val2_A")
  expect_true(is.na(row_A2$ColExtraB))

  # Check B's contribution (url_B_simple)
  row_B1 <- result[result$Source == "url_B_simple", ]
  expect_equal(row_B1$Perm, "pB.1")
  expect_true(is.na(row_B1$ColExtra1))
  expect_true(is.na(row_B1$ColExtra2))
  expect_equal(row_B1$ColExtraB, "valB_B")
}) 