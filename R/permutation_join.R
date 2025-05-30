#' Permutation Join of Two URL Sets
#'
#' This function takes two datasets, A and B, each containing URLs and their
#' permutations, flattens them, and then joins them into a single data frame.
#' Each permutation is augmented with information about its original URL (Source)
#' and a label indicating whether it came from dataset A or B (SourceSet).
#'
#' @param A A data frame. It must contain at least two columns:
#'   \code{URL} (character, the original URL) and \code{Permutation} (list,
#'   where each element is a data frame of permutations). Each data frame
#'   within the \code{Permutation} list must have a column named \code{Permutations}
#'   (which will be renamed to \code{Perm}).
#' @param B A data frame, with the same structure as \code{A}.
#'
#' @return A single data frame combining the processed permutations from both
#'   A and B. The resulting data frame will have columns including \code{Perm}
#'   (the permutation), \code{Source} (the original URL from which the
#'   permutation was derived), \code{SourceSet} (a factor indicating origin,
#'   e.g., "SetA" or "SetB"), and any other columns originally present in the
#'   innermost permutation data frames. Returns \code{NULL} if inputs are invalid.
#'
#' @export
#' @examples
#' # Example Data for A
#' perms_df_a1 <- data.frame(Permutations = c("http://ex.com/p1", "http://ex.com/p2"),
#'                           OtherCol = 1:2, stringsAsFactors = FALSE)
#' perms_df_a2 <- data.frame(Permutations = c("http://samp.org/a", "http://samp.org/b"),
#'                           OtherCol = 3:4, stringsAsFactors = FALSE)
#' A_data <- data.frame(URL = c("http://example.com", "http://sample.org"),
#'                      stringsAsFactors = FALSE)
#' A_data$Permutation <- list(perms_df_a1, perms_df_a2)
#'
#' # Example Data for B
#' perms_df_b1 <- data.frame(Permutations = c("http://test.net/x", "http://test.net/y"),
#'                           AnotherCol = c("foo", "bar"), stringsAsFactors = FALSE)
#' B_data <- data.frame(URL = c("http://test.net"), stringsAsFactors = FALSE)
#' B_data$Permutation <- list(perms_df_b1)
#'
#' # Perform the permutation join using base data.frames
#' joined_data_df <- permutation_join(A_data, B_data)
#' if (!is.null(joined_data_df)) {
#'   print(head(joined_data_df))
#'   print(table(joined_data_df$SourceSet))
#' }
#'
#' # Example with empty input for B
#' empty_df_B <- data.frame(URL = character(0), stringsAsFactors = FALSE)
#' empty_df_B$Permutation <- vector("list", 0)
#'
#' joined_with_empty_B <- permutation_join(A_data, empty_df_B)
#' if (!is.null(joined_with_empty_B)) {
#'  print(head(joined_with_empty_B))
#'  print(table(joined_with_empty_B$SourceSet))
#' }
#'
#' # Example with both inputs empty
#' empty_df_A <- data.frame(URL = character(0), stringsAsFactors = FALSE)
#' empty_df_A$Permutation <- vector("list", 0)
#'
#' joined_empty_both <- permutation_join(empty_df_A, empty_df_B)
#' if (!is.null(joined_empty_both)) {
#'  print(head(joined_empty_both)) # Should be an empty data frame with defined columns
#'  print(dim(joined_empty_both))
#' }
#'
permutation_join <- function(A, B) {

  # --- Input Validation Helper ---
  validate_input <- function(tbl, tbl_name) {
    if (is.null(tbl)) {
      warning(paste0("Input '", tbl_name, "' is NULL."))
      return(FALSE)
    }
    if (!is.data.frame(tbl)) {
      warning(paste0("Input '", tbl_name, "' is not a data.frame or tibble."))
      return(FALSE)
    }
    required_cols <- c("URL", "Permutation")
    if (!all(required_cols %in% names(tbl))) {
      missing_cols <- setdiff(required_cols, names(tbl))
      warning(paste0("Input '", tbl_name, "' is missing required column(s): ",
                     paste(missing_cols, collapse = ", ")))
      return(FALSE)
    }
    if (!is.character(tbl$URL)) {
        warning(paste0("Column 'URL' in input '", tbl_name, "' must be character."))
        return(FALSE)
    }
    if (!is.list(tbl$Permutation)) {
      warning(paste0("Column 'Permutation' in input '", tbl_name, "' must be a list."))
      return(FALSE)
    }
    if (nrow(tbl) > 0) {
        for (i in seq_len(nrow(tbl))) {
            perm_element <- tbl$Permutation[[i]]
            if (is.null(perm_element)) { # Allow NULL elements if they are skipped later
                 next # Or treat as error: return(FALSE) with warning
            }
            if (!is.data.frame(perm_element)) {
                warning(paste0("Element ", i, " of 'Permutation' in '", tbl_name, "' is not a data.frame."))
                return(FALSE)
            }
            # Allow empty data.frames in the list, flatten_perms handles them
            if (nrow(perm_element) > 0 && !("Permutations" %in% names(perm_element))) {
                warning(paste0("Data.frame at element ", i, " of 'Permutation' in '", tbl_name,
                               "' is non-empty and must have a 'Permutations' column."))
                return(FALSE)
            }
        }
    }
    return(TRUE)
  }

  if (!validate_input(A, "A") || !validate_input(B, "B")) {
    return(NULL)
  }

  # --- Helper to flatten a tibble of permuted URLs ---
  flatten_perms <- function(tbl, source_name) {
    # Define a canonical empty data frame structure for this helper's output
    # This is used if tbl is empty or results in no processable data.
    # Other columns will be added by rbind if they exist in actual data.
    empty_base_df <- data.frame(Perm = character(0),
                                Source = character(0),
                                SourceSet = character(0),
                                stringsAsFactors = FALSE)

    if (nrow(tbl) == 0) {
      return(empty_base_df)
    }

    list_of_dfs <- lapply(seq_len(nrow(tbl)), function(i) {
      inner_df_orig <- tbl$Permutation[[i]]
      
      if (is.null(inner_df_orig)) { # Only skip truly NULL elements
        return(NULL)
      }
      
      current_df <- as.data.frame(inner_df_orig) # Ensure it's a data.frame
      
      if (!("Permutations" %in% names(current_df))) {
        warning(paste("Column 'Permutations' not found in list element ", i ," of 'Permutation' for URL: '",
                      tbl$URL[i], "' in SourceSet: '", source_name,
                      "'. Skipping this element."), call. = FALSE)
        return(NULL) # Return NULL if 'Permutations' column is missing, to be filtered
      }

      # If inner_df_orig is a 0-row data.frame but HAS the 'Permutations' column:
      if (nrow(current_df) == 0) {
        # It's a 0-row df, its column structure (including 'Permutations' and others like 'ColX') is valuable.
        # Add Source, SourceSet as 0-length character vectors.
        current_df$Source <- character(0) 
        current_df$SourceSet <- character(0) # source_name is a single string, so use character(0)
        
        # Rename Permutations to Perm. Already checked 'Permutations' exists.
        current_df_names <- names(current_df)
        current_df_names[current_df_names == "Permutations"] <- "Perm"
        names(current_df) <- current_df_names
        
        # Ensure standard columns are present and are 0-length characters of the correct type.
        # This is important if 'Permutations' was the only column, it becomes 'Perm'.
        if("Perm" %in% names(current_df)) {
            current_df$Perm <- as.character(current_df$Perm) # Ensure it is character
        } else {
            current_df$Perm <- character(0) # Add if somehow missing after rename logic
        }
        # Source and SourceSet already set to character(0).

        return(current_df) # Return the 0-row df with correct columns and types
      }
      
      # For non-empty data frames (original logic)
      current_df$Source <- tbl$URL[i]
      current_df$SourceSet <- source_name
      names(current_df)[names(current_df) == "Permutations"] <- "Perm"
      return(current_df)
    })
    
    list_of_dfs <- Filter(Negate(is.null), list_of_dfs)

    if (length(list_of_dfs) == 0) {
      return(empty_base_df)
    }
    
    # Robust rbind: find all unique column names and add missing ones with NAs
    all_names <- unique(unlist(lapply(list_of_dfs, names)))
    
    processed_list_of_dfs <- lapply(list_of_dfs, function(df_item) {
      missing_cols <- setdiff(all_names, names(df_item))
      if (length(missing_cols) > 0) {
          for (col_name in missing_cols) {
            df_item[[col_name]] <- NA # Use logical NA, rbind will coerce
          }
      }
      # Ensure column order for rbind, and select only existing columns from all_names
      # This handles cases where a df_item might have extra cols not in all_names (should not happen here)
      df_item[, intersect(all_names, names(df_item)), drop = FALSE]
    })
    
    # Re-align all dataframes to have all columns from all_names, then rbind
    # This is the robust part for base R rbind.
    final_processed_list <- lapply(processed_list_of_dfs, function(df_item) {
        current_item_names <- names(df_item)
        # Add columns from all_names that are missing in df_item
        for(col_to_add in setdiff(all_names, current_item_names)) {
            df_item[[col_to_add]] <- NA
        }
        # Ensure order
        df_item[, all_names, drop = FALSE]
    })

    result_df <- do.call(rbind, final_processed_list)
    
    # Ensure core columns have consistent types if they became all NAs
    if ("Perm" %in% names(result_df)) result_df$Perm <- as.character(result_df$Perm)
    if ("Source" %in% names(result_df)) result_df$Source <- as.character(result_df$Source)
    if ("SourceSet" %in% names(result_df)) result_df$SourceSet <- as.character(result_df$SourceSet)
    
    return(result_df)
  }

  # Process A and B
  flat_A <- flatten_perms(A, "SetA")
  flat_B <- flatten_perms(B, "SetB")

  # Combine the results
  # If one is empty (0 rows), return the other.
  # The flatten_perms ensures even empty results have a base structure.
  if (nrow(flat_A) == 0 && nrow(flat_B) == 0) {
    # Both are empty. Determine combined column structure.
    # Default to core columns if no other columns are present in inputs.
    # This typically means they returned the empty_base_df from flatten_perms.
    # rbind of two identical empty_base_df is fine.
    # If they had different "other" columns that became all NA, names() would reflect them.
    all_cols <- union(names(flat_A), names(flat_B))
    if (length(all_cols) == 0) all_cols <- names(data.frame(Perm=character(),Source=character(),SourceSet=character()))

    empty_res <- data.frame(matrix(ncol = length(all_cols), nrow = 0))
    colnames(empty_res) <- all_cols
    # Ensure types for key columns for consistency
    if("Perm" %in% all_cols) empty_res$Perm <- character(0)
    if("Source" %in% all_cols) empty_res$Source <- character(0)
    if("SourceSet" %in% all_cols) empty_res$SourceSet <- character(0)
    return(empty_res)

  } else if (nrow(flat_A) == 0) {
    if ("SourceSet" %in% names(flat_B)) flat_B$SourceSet <- as.factor(flat_B$SourceSet)
    return(flat_B)
  } else if (nrow(flat_B) == 0) {
    if ("SourceSet" %in% names(flat_A)) flat_A$SourceSet <- as.factor(flat_A$SourceSet)
    return(flat_A)
  }

  # Both flat_A and flat_B have rows.
  # Align columns before final rbind.
  all_final_names <- union(names(flat_A), names(flat_B))
  
  align_df_for_final_rbind <- function(df, target_names) {
    # Add missing columns
    for (col_name in setdiff(target_names, names(df))) {
      df[[col_name]] <- NA 
    }
    # Select and order columns
    df[, target_names, drop = FALSE]
  }

  flat_A_aligned <- align_df_for_final_rbind(flat_A, all_final_names)
  flat_B_aligned <- align_df_for_final_rbind(flat_B, all_final_names)
  
  result <- rbind(flat_A_aligned, flat_B_aligned)
  
  if ("SourceSet" %in% names(result)) {
    result$SourceSet <- as.factor(result$SourceSet)
  }

  return(result)
} 