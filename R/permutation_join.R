#' Permutation Join of Two URL Sets (Base R Version)
#'
#' Performs a join between two data frames based on URL permutations.
#' It identifies rows in `data_A` and `data_B` that can be linked because
#' a URL in one is a permutation of a URL in the other (considering variations
#' in scheme, www prefix, and trailing slashes).
#' This version aims to use primarily base R functions.
#'
#' @param data_A A data frame containing URLs for the left side of the join.
#' @param data_B A data frame containing URLs for the right side of the join.
#' @param col_A Character string, the name of the column in `data_A` that
#'   contains URLs. Defaults to "URL".
#' @param col_B Character string, the name of the column in `data_B` that
#'   contains URLs. Defaults to "URL".
#' @param suffix_A Character string, suffix to add to `data_A` column names
#'   in the output if they conflict with `data_B` column names (excluding
#'   join keys and specially created columns like OriginalURL_A). Defaults to "_A".
#' @param suffix_B Character string, suffix to add to `data_B` column names
#'   in the output if they conflict with `data_A` column names. Defaults to "_B".
#'
#' @return A data frame representing the join. Each row signifies a unique pair of
#'   rows (one from `data_A`, one from `data_B`) linked by a common URL
#'   permutation. The output includes:
#'   \itemize{
#'     \item `OriginalURL_A`: The URL from the linking row in `data_A`.
#'     \item `OriginalURL_B`: The URL from the linking row in `data_B`.
#'     \item `JoinKey`: An example of the common permuted URL string that
#'           established the link.
#'     \item All other original columns from `data_A` and `data_B`, with
#'           suffixes applied by `base::merge` to resolve name conflicts
#'           if necessary.
#'   }
#'   Returns an empty data frame with the expected structure if no matches are found
#'   or if inputs are invalid/empty.
#'
#' @export
#' @examples
#' # Create dummy data for permute_url if not loaded (for example context)
#' if (!exists("permute_url", mode = "function")) {
#'   permute_url <- function(urls) {
#'     if (!is.character(urls)) urls <- as.character(urls)
#'     if (length(urls) == 0) return(data.frame(URL=character(), Permutation=character(),
#'                                              stringsAsFactors = FALSE))
#'     all_perms_list <- lapply(urls, function(u) {
#'       if (is.na(u) || u == "") {
#'         return(data.frame(URL=u, Permutation=NA_character_,
#'                           stringsAsFactors = FALSE))
#'       }
#'       # Simplified mock for example purposes
#'       stripped_u <- gsub("^(https?://)?(www\\.)?", "", u)
#'       perms <- unique(c(stripped_u, paste0(stripped_u, "/")))
#'       return(data.frame(URL = rep(u, length(perms)), Permutation = perms,
#'                         stringsAsFactors = FALSE))
#'     })
#'     return(do.call(rbind, all_perms_list))
#'   }
#' }
#'
#' dfA <- data.frame(
#'   ID_A = 1:2,
#'   URL_A_col = c("http://example.com/path", "www.test.com/another/"),
#'   DataA = LETTERS[1:2],
#'   SharedCol = c("val1", "val2"),
#'   stringsAsFactors = FALSE
#' )
#' dfB <- data.frame(
#'   ID_B = 1:3,
#'   URL_B_col = c("example.com/path/", "test.com/another", "unrelated.org"),
#'   DataB = letters[24:26], # x, y, z
#'   SharedCol = c("val_x", "val_y", "val_z"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # To run example (make sure permute_url is defined as above or from package)
#' # joined_result <- permutation_join(dfA, dfB, col_A = "URL_A_col", col_B = "URL_B_col",
#' #                                 suffix_A = ".dfA", suffix_B = ".dfB")
#' # print(joined_result)
#'
#' # Example with default "URL" column name
#' dfA_std <- data.frame(URL = "site.com/page", ValA = 10, stringsAsFactors = FALSE)
#' dfB_std <- data.frame(URL = "https://www.site.com/page/", ValB = 20, stringsAsFactors = FALSE)
#' # joined_std <- permutation_join(dfA_std, dfB_std)
#' # print(joined_std)
permutation_join <- function(data_A, data_B,
                             col_A = "URL", col_B = "URL",
                             suffix_A = "_A", suffix_B = "_B") {

  # --- Input Validation ---
  if (!is.data.frame(data_A) || !is.data.frame(data_B)) {
    warning("Inputs 'data_A' and 'data_B' must be data frames.", call. = FALSE)
    return(data.frame()) # Return empty data.frame
  }
  if (!col_A %in% names(data_A)) {
    warning(paste0("Column '", col_A, "' not found in data_A."), call. = FALSE)
    return(data.frame())
  }
  if (!col_B %in% names(data_B)) {
    warning(paste0("Column '", col_B, "' not found in data_B."), call. = FALSE)
    return(data.frame())
  }

  # Define expected output structure for empty results
  empty_output_template <- data.frame(
    OriginalURL_A = character(0),
    OriginalURL_B = character(0),
    JoinKey = character(0),
    stringsAsFactors = FALSE
  )
  # Attempt to add other columns from inputs to the template for empty results
  # This is best-effort for a more informative empty return.
  # Names from data_A (excluding col_A, adding suffix if needed later)
  other_cols_A <- setdiff(names(data_A), col_A)
  for(oca in other_cols_A) empty_output_template[[paste0(oca, suffix_A)]] <- character(0)
  # Names from data_B (excluding col_B, adding suffix if needed later)
  other_cols_B <- setdiff(names(data_B), col_B)
  for(ocb in other_cols_B) empty_output_template[[paste0(ocb, suffix_B)]] <- character(0)
  # Ensure unique columns in template in case of original name clashes resolved by suffix
  empty_output_template <- empty_output_template[, unique(names(empty_output_template)), drop = FALSE]


  if (nrow(data_A) == 0 || nrow(data_B) == 0) {
    return(empty_output_template)
  }

  # --- Internal Helper Function to Prepare Data (Base R) ---
  .prepare_data_for_join_base <- function(df, url_col_name_str,
                                          desired_original_url_col_name_str,
                                          internal_id_col_name_str) {
    df_copy <- df # Work on a copy
    df_copy[[internal_id_col_name_str]] <- seq_len(nrow(df_copy))

    all_perms_list <- vector("list", nrow(df_copy))

    for (i in seq_len(nrow(df_copy))) {
      current_row_url <- df_copy[[url_col_name_str]][i]
      
      # permute_url should return a data.frame with columns "URL" (original input to it)
      # and "Permutation" (the permuted string).
      perms_df_for_row <- permute_url(current_row_url)

      if (is.data.frame(perms_df_for_row) && nrow(perms_df_for_row) > 0) {
        # Keep the original URL from *this specific row* of df_copy,
        # not the one permute_url might return in its 'URL' column if it processed a vector.
        perms_df_for_row[[desired_original_url_col_name_str]] <- current_row_url
        
        # Rename the column containing the actual permutation strings to "JoinKey"
        names(perms_df_for_row)[names(perms_df_for_row) == "Permutation"] <- "JoinKey"
        
        # Add the internal ID of the original row from df_copy
        perms_df_for_row[[internal_id_col_name_str]] <- df_copy[[internal_id_col_name_str]][i]
        
        # Select only necessary columns for the expanded part
        # The original 'URL' column from permute_url output is not strictly needed here
        # as we have desired_original_url_col_name_str
        all_perms_list[[i]] <- perms_df_for_row[, c(internal_id_col_name_str, desired_original_url_col_name_str, "JoinKey"), drop = FALSE]
      } else {
        # Handle case where permute_url returns empty/NA or not a proper data.frame
        # Create a 0-row df with expected columns to avoid rbind errors
        temp_empty_df <- data.frame(
            stringsAsFactors = FALSE
        )
        temp_empty_df[[internal_id_col_name_str]] <- df_copy[[internal_id_col_name_str]][i] # or integer(0) if not linking to row
        temp_empty_df[[desired_original_url_col_name_str]] <- current_row_url # or character(0)
        temp_empty_df[["JoinKey"]] <- NA_character_ # or character(0)
        all_perms_list[[i]] <- temp_empty_df[0, , drop = FALSE] # ensure 0 rows
      }
    }

    expanded_data <- do.call(rbind, all_perms_list)
    if (is.null(expanded_data) || nrow(expanded_data) == 0) {
        # Create an empty data frame with the correct column names if all_perms_list was empty or all elements were NULL/0-row
        empty_df <- data.frame(stringsAsFactors = FALSE)
        empty_df[[internal_id_col_name_str]] <- integer(0)
        empty_df[[desired_original_url_col_name_str]] <- character(0)
        empty_df[["JoinKey"]] <- character(0)
        return(empty_df)
    }
    
    # Merge back with original df to get all original columns
    # Ensure df_copy has the internal_id_col_name_str with unique values for merge
    # The internal_id_col_name_str in expanded_data links to rows in df_copy
    
    # Select all columns from df_copy *except* the original url_col_name_str IF it is not also an ID or other wanted col.
    # This is to avoid duplicating the URL column if it's already captured as desired_original_url_col_name_str.
    # However, merge will handle suffixes if url_col_name_str is still present.
    # Simpler: let merge handle all columns from df_copy.
    
    # Add a suffix to df_copy columns (except the ID) to prevent clashes with expanded_data before merge
    # This is not standard for merge; merge itself handles suffixes on output.
    # So, directly merge.
    
    # df_copy should only contain the id and its original columns.
    # expanded_data has id, OriginalURL_X, JoinKey.
    # We want to attach all original columns from df_copy to expanded_data.
    
    # Prepare df_copy for merge: only id and its other columns
    df_original_cols_with_id <- df_copy # df_copy has id and all original cols
    
    # Merge expanded_data (id, OriginalURL_X, JoinKey) with all original columns from df_original_cols_with_id
    # common column for merge is internal_id_col_name_str
    final_expanded_df <- merge(
      expanded_data, # x: has internal_id_col_name_str, desired_original_url_col_name_str, JoinKey
      df_original_cols_with_id, # y: has internal_id_col_name_str and all original columns of df
      by = internal_id_col_name_str,
      all.x = TRUE, # Keep all permutation rows
      all.y = FALSE  # Don't keep original rows that had no valid permutations (already handled by expanded_data construction)
    )
    
    return(final_expanded_df)
  }

  # --- Process data_A ---
  id_col_A <- ".id_A_rurl_join"
  data_A_expanded <- .prepare_data_for_join_base(
    df = data_A, url_col_name_str = col_A,
    desired_original_url_col_name_str = "OriginalURL_A",
    internal_id_col_name_str = id_col_A
  )

  # --- Process data_B ---
  id_col_B <- ".id_B_rurl_join"
  data_B_expanded <- .prepare_data_for_join_base(
    df = data_B, url_col_name_str = col_B,
    desired_original_url_col_name_str = "OriginalURL_B",
    internal_id_col_name_str = id_col_B
  )
  
  if (nrow(data_A_expanded) == 0 || nrow(data_B_expanded) == 0) {
    return(empty_output_template)
  }

  # --- Join Expanded DataFrames ---
  joined_data_raw <- merge(
    data_A_expanded, data_B_expanded,
    by = "JoinKey",
    suffixes = c(suffix_A, suffix_B),
    all = FALSE # Inner join behavior
  )

  # --- Deduplicate Connections ---
  if (nrow(joined_data_raw) > 0) {
    # Create a composite key for deduplication based on original row IDs
    dedup_key <- paste(joined_data_raw[[id_col_A]], joined_data_raw[[id_col_B]], sep = "_rurl_sep_")
    final_joined_data <- joined_data_raw[!duplicated(dedup_key), , drop = FALSE]
  } else {
    final_joined_data <- joined_data_raw
  }

  # --- Select and Arrange Output Columns ---
  if (nrow(final_joined_data) > 0) {
    # Define core columns in desired order
    core_output_cols <- c("OriginalURL_A", "OriginalURL_B", "JoinKey")
    
    # Get all other columns, excluding the internal ID columns
    all_current_names <- names(final_joined_data)
    other_cols <- setdiff(all_current_names, c(core_output_cols, id_col_A, id_col_B))
    
    # Combine and ensure unique names (though suffixes from merge should handle most)
    final_col_order <- unique(c(core_output_cols, other_cols))
    
    # Ensure all selected columns actually exist to prevent errors
    final_col_order_existing <- final_col_order[final_col_order %in% all_current_names]
    
    result <- final_joined_data[, final_col_order_existing, drop = FALSE]
    
  } else {
    result <- empty_output_template # Use the template for 0-row results
  }

  return(result)
} 