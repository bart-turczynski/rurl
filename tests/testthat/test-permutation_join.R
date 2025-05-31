# --- Helper Data for Tests ---

# Basic valid data for A
perms_df_a1_valid <- data.frame(Permutations = c("http://ex.com/p1", "http://ex.com/p2"),
                                OtherColA = 1:2, stringsAsFactors = FALSE)
perms_df_a2_valid <- data.frame(Permutations = c("http://samp.org/a", "http://samp.org/b"),
                                OtherColA = 3:4, stringsAsFactors = FALSE)
A_valid <- data.frame(URL = c("http://example.com", "http://sample.org"), 
                    OtherColA = c("valA1", "valA2"), 
                    stringsAsFactors = FALSE)

# Basic valid data for B (with a different "other" column)
perms_df_b1_valid <- data.frame(Permutations = c("http://test.net/x", "http://test.net/y"),
                                OtherColB = c("foo", "bar"), stringsAsFactors = FALSE)
B_valid <- data.frame(URL = c("http://test.net", "http://example.com/"), # One matching, one not
                    OtherColB = c("valB1", "valB2"), 
                    stringsAsFactors = FALSE)

# Empty data frame structure (matching expected output of flatten_perms for empty inputs)
empty_df_input <- data.frame(URL = character(0), stringsAsFactors = FALSE)
empty_df_input$Permutation <- vector("list", 0)


# --- Test Cases ---

test_that("permutation_join works with valid basic inputs and finds a match", {
  # Ensure permute_url is available (mock if not from package)
  if (!exists("permute_url", mode = "function")) {
    permute_url <- function(urls) {
      if (!is.character(urls)) urls <- as.character(urls)
      if (length(urls) == 0) return(data.frame(URL=character(), Permutation=character(), stringsAsFactors = FALSE))
      all_perms_list <- lapply(urls, function(u) {
        if (is.na(u) || u == "") return(data.frame(URL=u, Permutation=NA_character_, stringsAsFactors = FALSE))
        stripped_u <- gsub("^(https?://)?(www\\.)?", "", u)
        stripped_u_slash <- gsub("/$", "", stripped_u)
        # Ensure a trailing slash variant is included for matching
        perms <- unique(c(stripped_u_slash, paste0(stripped_u_slash, "/")))
        # Ensure the permutation itself doesn't have http/www for basic matching
        perms <- gsub("^(https?://)?(www\\.)?", "", perms)
        return(data.frame(URL = rep(u, length(perms)), Permutation = perms, stringsAsFactors = FALSE))
      })
      return(do.call(rbind, all_perms_list))
    }
  }

  result <- permutation_join(A_valid, B_valid)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  # Expect names from input data.frames and JoinKey, plus other columns from original dfs
  expect_named(result, c("A_valid", "B_valid", "JoinKey", "OtherColA", "OtherColB"), ignore.order = TRUE)
  # A_valid[1] ("http://example.com") should match B_valid[2] ("http://example.com/")
  expect_equal(nrow(result), 1)
  if(nrow(result) == 1){
    expect_equal(result$A_valid[1], "http://example.com")
    expect_equal(result$B_valid[1], "http://example.com/")
    expect_equal(result$JoinKey[1], "http://example.com") # Original URL from A
    expect_equal(result$OtherColA[1], "valA1") # From A_valid
    expect_equal(result$OtherColB[1], "valB2") # From B_valid corresponding to the match
  }
})

test_that("permutation_join returns empty for no matches", {
  A_no_match <- data.frame(URL = "http://nomatchA.com", OtherColA = "valX", stringsAsFactors = FALSE)
  B_no_match <- data.frame(URL = "http://nomatchB.com", OtherColB = "valY", stringsAsFactors = FALSE)
  result <- permutation_join(A_no_match, B_no_match)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("A_no_match", "B_no_match", "JoinKey", "OtherColA", "OtherColB"), ignore.order = TRUE)
  expect_equal(nrow(result), 0)
})

test_that("permutation_join handles one empty input correctly (A empty)", {
  result <- permutation_join(empty_df_input, B_valid)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("empty_df_input", "B_valid", "JoinKey", "OtherColB"), ignore.order = TRUE) # OtherColA not present as empty_df_input has no other cols
  expect_equal(nrow(result), 0)
})

test_that("permutation_join handles one empty input correctly (B empty)", {
  result <- permutation_join(A_valid, empty_df_input)
  expect_false(is.null(result))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("A_valid", "empty_df_input", "JoinKey", "OtherColA"), ignore.order = TRUE) # OtherColB not present
  expect_equal(nrow(result), 0)
})

test_that("permutation_join handles both inputs empty", {
  # Define names for empty inputs to make deparse(substitute()) predictable
  left_empty <- data.frame(URL = character(0), stringsAsFactors = FALSE)
  right_empty <- data.frame(URL = character(0), stringsAsFactors = FALSE)
  result <- permutation_join(left_empty, right_empty)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("left_empty", "right_empty", "JoinKey"), ignore.order = TRUE)
  expect_equal(nrow(result), 0)
})

test_that("permutation_join returns empty data.frame and warns for NULL inputs", {
  expect_warning(
    res_A_null <- permutation_join(NULL, B_valid),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_A_null, "data.frame")
  # Expect specific column names even for empty data.frame from warning path
  expect_named(res_A_null, c("B_valid", "JoinKey", "OtherColB"), ignore.order = TRUE)
  expect_equal(nrow(res_A_null), 0)

  expect_warning(
    res_B_null <- permutation_join(A_valid, NULL),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_B_null, "data.frame")
  expect_named(res_B_null, c("A_valid", "JoinKey", "OtherColA"), ignore.order = TRUE)
  expect_equal(nrow(res_B_null), 0)

  expect_warning(
    res_both_null <- permutation_join(NULL, NULL),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_both_null, "data.frame")
  expect_named(res_both_null, character(0)) # No column info if both NULL
  expect_equal(nrow(res_both_null), 0)
})

test_that("permutation_join warns for non-data.frame inputs", {
  expect_warning(
    res_A_invalid <- permutation_join(list(), B_valid),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_A_invalid, "data.frame")
  expect_named(res_A_invalid, c("B_valid", "JoinKey", "OtherColB"), ignore.order = TRUE)
  expect_equal(nrow(res_A_invalid), 0)
  
  expect_warning(
    res_B_invalid <- permutation_join(A_valid, "not_a_df"),
    "Inputs 'data_A' and 'data_B' must be data frames."
  )
  expect_s3_class(res_B_invalid, "data.frame")
  expect_named(res_B_invalid, c("A_valid", "JoinKey", "OtherColA"), ignore.order = TRUE)
  expect_equal(nrow(res_B_invalid), 0)
})

test_that("permutation_join warns for missing URL columns in inputs", {
  A_no_URL <- data.frame(SomeCol = "val1", stringsAsFactors = FALSE)
  # Correct deparsed name for A_no_URL
  name_A_no_URL <- deparse(substitute(A_no_URL))

  expect_warning(
    res_A_no_URL <- permutation_join(A_no_URL, B_valid),
    "Column 'URL' not found in data_A.", fixed = TRUE
  )
  expect_s3_class(res_A_no_URL, "data.frame")
  # Column names should include B_valid's details and the attempted name for A
  expect_named(res_A_no_URL, c(name_A_no_URL, "B_valid", "JoinKey", "OtherColB"), ignore.order = TRUE)
  expect_equal(nrow(res_A_no_URL), 0)

  B_no_URL <- data.frame(SomeOtherCol = "val2", stringsAsFactors = FALSE)
  name_B_no_URL <- deparse(substitute(B_no_URL))
  expect_warning(
    res_B_no_URL <- permutation_join(A_valid, B_no_URL),
    "Column 'URL' not found in data_B.", fixed = TRUE
  )
  expect_s3_class(res_B_no_URL, "data.frame")
  expect_named(res_B_no_URL, c("A_valid", name_B_no_URL, "JoinKey", "OtherColA"), ignore.order = TRUE)
  expect_equal(nrow(res_B_no_URL), 0)
})

test_that("permutation_join handles custom URL column names", {
  A_custom_col <- data.frame(MyURL = "http://custom.com", ValA = 10, stringsAsFactors = FALSE)
  B_custom_col <- data.frame(Link = "http://custom.com/", ValB = 20, stringsAsFactors = FALSE)
  
  result <- permutation_join(A_custom_col, B_custom_col, col_A = "MyURL", col_B = "Link")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("A_custom_col", "B_custom_col", "JoinKey", "ValA", "ValB"), ignore.order = TRUE)
  expect_equal(nrow(result), 1)
  if(nrow(result) == 1){
    expect_equal(result$A_custom_col[1], "http://custom.com")
    expect_equal(result$B_custom_col[1], "http://custom.com/")
    expect_equal(result$JoinKey[1], "http://custom.com")
    expect_equal(result$ValA[1], 10)
    expect_equal(result$ValB[1], 20)
  }
})

# --- Input Validation Tests ---

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