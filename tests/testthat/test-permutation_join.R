
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
  expect_named(result, c("Perm", "OtherColA", "Source", "SourceSet", "OtherColB"), ignore.order = TRUE)
  expect_equal(nrow(result), 6) # 4 from A, 2 from B
  expect_s3_class(result$SourceSet, "factor")
  expect_equal(levels(result$SourceSet), c("SetA", "SetB"))
  expect_equal(sum(result$SourceSet == "SetA"), 4)
  expect_equal(sum(result$SourceSet == "SetB"), 2)

  # Check specific values if necessary, e.g. first perm from A
  expect_equal(result$Perm[1], "http://ex.com/p1")
  expect_equal(result$Source[1], "http://example.com")
  expect_equal(result$OtherColA[1], 1)
  expect_true(is.na(result$OtherColB[1]))

  # Check specific values from B
  expect_equal(result$Perm[5], "http://test.net/x")
  expect_equal(result$Source[5], "http://test.net")
  expect_equal(result$OtherColB[5], "foo")
  expect_true(is.na(result$OtherColA[5]))
})

test_that("permutation_join handles one empty input correctly (A empty)", {
  result <- permutation_join(empty_df_input, B_valid)

  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Perm", "OtherColB", "Source", "SourceSet"), ignore.order = TRUE)
  expect_equal(nrow(result), 2) # 0 from A, 2 from B
  expect_s3_class(result$SourceSet, "factor")
  expect_equal(levels(result$SourceSet), c("SetB"))
  expect_equal(sum(result$SourceSet == "SetB"), 2)
})

test_that("permutation_join handles one empty input correctly (B empty)", {
  result <- permutation_join(A_valid, empty_df_input)

  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Perm", "OtherColA", "Source", "SourceSet"), ignore.order = TRUE)
  expect_equal(nrow(result), 4) # 4 from A, 0 from B
  expect_s3_class(result$SourceSet, "factor")
  expect_equal(levels(result$SourceSet), c("SetA"))
  expect_equal(sum(result$SourceSet == "SetA"), 4)
})

test_that("permutation_join handles both inputs empty", {
  result <- permutation_join(empty_df_input, empty_df_input)
  
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  # Expecting canonical columns from empty_base_df in flatten_perms
  expect_named(result, c("Perm", "Source", "SourceSet"), ignore.order = TRUE)
  expect_equal(nrow(result), 0)
  # Check column types for empty result
  expect_true(is.character(result$Perm))
  expect_true(is.character(result$Source))
  expect_true(is.character(result$SourceSet)) # Before factor conversion for non-empty
})

# --- Input Validation Tests ---

test_that("permutation_join returns NULL and warns for NULL inputs", {
  expect_warning(
    res_A_null <- permutation_join(NULL, B_valid),
    "Input 'A' is NULL."
  )
  expect_null(res_A_null)
  
  expect_warning(
    res_B_null <- permutation_join(A_valid, NULL),
    "Input 'B' is NULL."
  )
  expect_null(res_B_null)
  
  expect_warning(
    res_both_null <- permutation_join(NULL, NULL), # First validation (for A) will trigger
    "Input 'A' is NULL."
  )
  expect_null(res_both_null)
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
  A_URL_not_char$URL <- 1:nrow(A_URL_not_char)
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
  A_with_empty_df_perm$Permutation[[1]] <- empty_inner_df
  
  result <- permutation_join(A_with_empty_df_perm, empty_df_input) # B is empty for simplicity
  expect_false(is.null(result))
  expect_equal(nrow(result), 2) # Processes perms_df_a2_valid (2 rows)
  expect_equal(result$Source, rep("http://sample.org", 2))
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

test_that("flatten_perms warns when inner DF is missing 'Permutations' column", {
 A_inner_missing_perm_col <- data.frame(URL = "http://test.com", stringsAsFactors = FALSE)
 bad_perm_df <- data.frame(NotTheRightName = "a.com", stringsAsFactors = FALSE)
 A_inner_missing_perm_col$Permutation <- list(bad_perm_df)
 
 expect_warning(
   result <- permutation_join(A_inner_missing_perm_col, empty_df_input),
   # The warning message comes from paste() so it might be complex, using a partial match and fixed = FALSE (default) is safer
   # However, the original code uses paste0 which suggests no complex regex from the warning itself.
   # Let's try fixed=TRUE for the specific part, if it fails, we might need more precise matching or allow regex.
   "Column 'Permutations' not found in list element  1  of 'Permutation' for URL: 'http://test.com' in SourceSet: 'SetA'. Skipping this element.",
   fixed = TRUE 
 )
 expect_false(is.null(result)) 
 expect_equal(nrow(result), 0) 
})

test_that("validate_input catches inner DF missing 'Permutations' column", {
 A_inner_missing_perm_col <- data.frame(URL = "http://test.com", stringsAsFactors = FALSE)
 bad_perm_df <- data.frame(NotTheRightName = "a.com", stringsAsFactors = FALSE)
 A_inner_missing_perm_col$Permutation <- list(bad_perm_df)
 
 expect_warning(
   res <- permutation_join(A_inner_missing_perm_col, empty_df_input),
   "must have a 'Permutations' column" # Partial match, regex by default
 )
 expect_null(res) 
})

test_that("permutation_join handles data.frames with zero rows correctly in lists", {
  A_zero_row_list_element <- data.frame(URL = "http://example.com", stringsAsFactors = FALSE)
  A_zero_row_list_element$Permutation <- list(data.frame(Permutations = character(0), ColX = integer(0)))
  
  B_data <- B_valid # Use a valid B
  
  result <- permutation_join(A_zero_row_list_element, B_data)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  # Should only contain rows from B_data, as A's entry was an empty permutation df
  expect_equal(nrow(result), nrow(B_valid$Permutation[[1]]))
  expect_true(all(result$SourceSet == "SetB"))
  expect_named(result, c("Perm", "OtherColB", "Source", "SourceSet", "ColX"), ignore.order = TRUE)
  expect_true(all(is.na(result$ColX)))
})

test_that("permutation_join works when Permutation column is I(list())", {
  A_I_list <- data.frame(URL = "http://example.com")
  A_I_list$Permutation <- I(list(perms_df_a1_valid)) # Using I() to create list column

  result <- permutation_join(A_I_list, empty_df_input)
  expect_false(is.null(result))
  expect_equal(nrow(result), 2)
  expect_true(all(result$SourceSet == "SetA"))
}) 