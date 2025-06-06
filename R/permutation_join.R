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
#'     \item `JoinKey`: The specific permutation derived from `data_A`'s URL
#'           that matched a permutation from `data_B`'s URL.
#'     \item All other original columns from `data_A` and `data_B`, with
#'           suffixes applied by `base::merge` to resolve name conflicts
#'           if necessary.
#'   }
#'   Returns an empty data frame with the expected structure if no matches are found
#'   or if inputs are invalid/empty.
#'
#' @importFrom stats setNames
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

  # --- Capture argument names for output columns ---
  name_A <- deparse(substitute(data_A))
  name_B <- deparse(substitute(data_B))

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
  # Check URL column types
  if (!is.character(data_A[[col_A]]) && !is.factor(data_A[[col_A]])) {
    warning(paste0("Column '", col_A, "' in data_A must be character or factor."), call. = FALSE)
    return(data.frame())
  }
  if (!is.character(data_B[[col_B]]) && !is.factor(data_B[[col_B]])) {
    warning(paste0("Column '", col_B, "' in data_B must be character or factor."), call. = FALSE)
    return(data.frame())
  }

  # Define expected output structure for empty results
  empty_output_template <- data.frame(
    stringsAsFactors = FALSE
  )
  empty_output_template[[name_A]] <- character(0)
  empty_output_template[[name_B]] <- character(0)
  empty_output_template[["JoinKey"]] <- character(0)
  other_cols_A <- setdiff(names(data_A), col_A)
  for(oca in other_cols_A) empty_output_template[[paste0(oca, suffix_A)]] <- character(0)
  other_cols_B <- setdiff(names(data_B), col_B)
  for(ocb in other_cols_B) empty_output_template[[paste0(ocb, suffix_B)]] <- character(0)
  empty_output_template <- empty_output_template[, unique(names(empty_output_template)), drop = FALSE]

  if (nrow(data_A) == 0 || nrow(data_B) == 0) {
    return(empty_output_template)
  }

  # --- Internal Helper Function to Prepare Data (Base R) ---
  .prepare_data_for_join_base <- function(df, url_col_name_str,
                                          desired_original_url_col_name_str,
                                          internal_id_col_name_str,
                                          permutation_key_col_name_str) {
    df_copy <- df 
    df_copy[[internal_id_col_name_str]] <- seq_len(nrow(df_copy))

    all_perms_list <- vector("list", nrow(df_copy))

    for (i in seq_len(nrow(df_copy))) {
      current_row_url <- df_copy[[url_col_name_str]][i]
      perms_df_for_row <- permute_url(current_row_url)

      if (is.data.frame(perms_df_for_row) && nrow(perms_df_for_row) > 0) {
        perms_df_for_row[[desired_original_url_col_name_str]] <- current_row_url
        names(perms_df_for_row)[names(perms_df_for_row) == "Permutation"] <- permutation_key_col_name_str
        perms_df_for_row[[internal_id_col_name_str]] <- df_copy[[internal_id_col_name_str]][i]
        all_perms_list[[i]] <- perms_df_for_row[, c(internal_id_col_name_str, desired_original_url_col_name_str, permutation_key_col_name_str), drop = FALSE]
      } else {
        temp_empty_df <- data.frame(stringsAsFactors = FALSE)
        temp_empty_df[[internal_id_col_name_str]] <- df_copy[[internal_id_col_name_str]][i] 
        temp_empty_df[[desired_original_url_col_name_str]] <- current_row_url 
        temp_empty_df[[permutation_key_col_name_str]] <- NA_character_
        all_perms_list[[i]] <- temp_empty_df[0, , drop = FALSE] 
      }
    }

    expanded_data <- do.call(rbind, all_perms_list)
    if (is.null(expanded_data) || nrow(expanded_data) == 0) {
        empty_df <- data.frame(stringsAsFactors = FALSE)
        empty_df[[internal_id_col_name_str]] <- integer(0)
        empty_df[[desired_original_url_col_name_str]] <- character(0)
        empty_df[[permutation_key_col_name_str]] <- character(0)
        return(empty_df)
    }
    
    cols_to_keep_from_original <- setdiff(names(df_copy), url_col_name_str)
    if (!internal_id_col_name_str %in% cols_to_keep_from_original) {
        cols_to_keep_from_original <- c(internal_id_col_name_str, cols_to_keep_from_original)
    }
    cols_to_keep_from_original <- unique(cols_to_keep_from_original)

    if(length(cols_to_keep_from_original) == 0 && internal_id_col_name_str %in% names(df_copy)){
        df_original_other_cols_with_id <- df_copy[, internal_id_col_name_str, drop = FALSE]
    } else if (length(cols_to_keep_from_original) > 0) {
        df_original_other_cols_with_id <- df_copy[, cols_to_keep_from_original, drop = FALSE]
    } else {
        df_original_other_cols_with_id <- data.frame(stringsAsFactors = FALSE)
        df_original_other_cols_with_id[[internal_id_col_name_str]] <- df_copy[[internal_id_col_name_str]]
    }
        
    final_expanded_df <- merge(
      expanded_data, 
      df_original_other_cols_with_id,
      by = internal_id_col_name_str,
      all.x = TRUE,
      all.y = FALSE 
    )
    return(final_expanded_df)
  }

  # --- Process data_A ---
  id_col_A <- ".id_A_rurl_join"
  perm_key_A <- ".PermKey_A_temp_rurl"
  data_A_expanded <- .prepare_data_for_join_base(
    df = data_A, url_col_name_str = col_A,
    desired_original_url_col_name_str = "OriginalURL_A",
    internal_id_col_name_str = id_col_A,
    permutation_key_col_name_str = perm_key_A
  )

  # --- Process data_B ---
  id_col_B <- ".id_B_rurl_join"
  perm_key_B <- ".PermKey_B_temp_rurl"
  data_B_expanded <- .prepare_data_for_join_base(
    df = data_B, url_col_name_str = col_B,
    desired_original_url_col_name_str = "OriginalURL_B",
    internal_id_col_name_str = id_col_B,
    permutation_key_col_name_str = perm_key_B
  )
  
  if (nrow(data_A_expanded) == 0 || nrow(data_B_expanded) == 0) {
    return(empty_output_template)
  }

  # --- Join Expanded DataFrames ---
  joined_data_raw <- merge(
    data_A_expanded, data_B_expanded,
    by.x = perm_key_A,
    by.y = perm_key_B,
    suffixes = c(suffix_A, suffix_B),
    all = FALSE # Inner join behavior
  )

  # --- Deduplicate Connections ---
  if (nrow(joined_data_raw) > 0) {
    # Rename perm_key_A to JoinKey for clarity in the output
    names(joined_data_raw)[names(joined_data_raw) == perm_key_A] <- "JoinKey"
    
    dedup_key <- paste(joined_data_raw[[id_col_A]], joined_data_raw[[id_col_B]], sep = "_rurl_sep_")
    final_joined_data <- joined_data_raw[!duplicated(dedup_key), , drop = FALSE]
  } else {
    final_joined_data <- joined_data_raw
  }

  # --- Select and Arrange Output Columns ---
  if (nrow(final_joined_data) > 0) {
    # Initialize result with core columns directly from final_joined_data
    result <- data.frame(
      setNames(list(final_joined_data$OriginalURL_A), name_A),
      setNames(list(final_joined_data$OriginalURL_B), name_B),
      JoinKey = final_joined_data$OriginalURL_A, # JoinKey is Original URL from A
      stringsAsFactors = FALSE
    )

    # Add any other columns from A and B, applying user-defined suffixes
    original_other_cols_A <- setdiff(names(data_A), col_A)
    for (col_name_orig_A in original_other_cols_A) {
      col_in_merged_A <- NULL
      if (col_name_orig_A %in% names(final_joined_data)) {
        col_in_merged_A <- col_name_orig_A
      } else if (paste0(col_name_orig_A, ".x") %in% names(final_joined_data)) {
        col_in_merged_A <- paste0(col_name_orig_A, ".x")
      }
      
      if (!is.null(col_in_merged_A)) {
        result[[paste0(col_name_orig_A, suffix_A)]] <- final_joined_data[[col_in_merged_A]]
      }
    }

    original_other_cols_B <- setdiff(names(data_B), col_B)
    for (col_name_orig_B in original_other_cols_B) {
      col_in_merged_B <- NULL
      if (col_name_orig_B %in% names(final_joined_data)) {
        col_in_merged_B <- col_name_orig_B
      } else if (paste0(col_name_orig_B, ".y") %in% names(final_joined_data)) {
        col_in_merged_B <- paste0(col_name_orig_B, ".y")
      }
      
      if (!is.null(col_in_merged_B)) {
        result[[paste0(col_name_orig_B, suffix_B)]] <- final_joined_data[[col_in_merged_B]]
      }
    }
    
    # Ensure column order is consistent if possible, starting with defined ones
    # This is a basic reordering, might need refinement for perfect match with template
    # preferred_order <- names(empty_output_template)
    # result <- result[, intersect(preferred_order, names(result)), drop = FALSE]

  } else {
    result <- empty_output_template
  }
  return(result)
} 