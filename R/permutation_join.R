#' Permutation Join of Two URL Sets
#'
#' Performs a join between two data frames based on URL permutations.
#' It identifies rows in `data_A` and `data_B` that can be linked because
#' a URL in one is a permutation of a URL in the other (considering variations
#' in scheme, www prefix, and trailing slashes).
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
#' @return A tibble representing the join. Each row signifies a unique pair of
#'   rows (one from `data_A`, one from `data_B`) linked by a common URL
#'   permutation. The output includes:
#'   \itemize{
#'     \item `OriginalURL_A`: The URL from the linking row in `data_A`.
#'     \item `OriginalURL_B`: The URL from the linking row in `data_B`.
#'     \item `JoinKey`: An example of the common permuted URL string that
#'           established the link.
#'     \item All other original columns from `data_A` and `data_B`, with
#'           suffixes applied by `dplyr::inner_join` to resolve name conflicts
#'           if necessary.
#'   }
#'   Returns an empty tibble with the expected structure if no matches are found
#'   or if inputs are invalid/empty.
#'
#' @export
#' @importFrom dplyr mutate row_number select rename inner_join distinct everything
#' @importFrom tidyr unnest
#' @importFrom rlang := .data
#'
#' @examples
#' # Create dummy data for permute_url if not loaded (for example context)
#' if (!exists("permute_url")) {
#'   permute_url <- function(urls) {
#'     if (length(urls) == 0) return(data.frame(URL=character(), Permutation=character()))
#'     # Simplified mock: returns original and original with/without trailing slash
#'     res <- lapply(urls, function(u) {
#'       if (is.na(u) || u == "") return(data.frame(URL=u, Permutation=NA_character_))
#'       p1 <- u
#'       p2 <- if (grepl("/$", u)) sub("/$", "", u) else paste0(u, "/")
#'       unique_perms <- unique(c(p1,p2))
#'       # permute_url actually returns scheme/www variants too
#'       # For this mock, just a few for structure.
#'       # True permute_url returns a data.frame with 'URL' (original)
#'       # and 'Permutation' (the permutation string)
#'       # Let's mock 2 permutations: original and stripped protocol/www
#'       stripped_u <- gsub("^(https?://)?(www\\\\.)?", "", u)
#'       return(data.frame(URL = rep(u, 2), Permutation = c(stripped_u, paste0(stripped_u,"/"))))
#'     })
#'     do.call(rbind, res)
#'   }
#' }
#'
#' dfA <- data.frame(
#'   ID_A = 1:2,
#'   URL_A_col = c("http://example.com/path", "www.test.com/another/"),
#'   DataA = LETTERS[1:2],
#'   stringsAsFactors = FALSE
#' )
#' dfB <- data.frame(
#'   ID_B = 1:3,
#'   URL_B_col = c("example.com/path/", "test.com/another", "unrelated.org"),
#'   DataB = letters[24:26], # x, y, z
#'   stringsAsFactors = FALSE
#' )
#'
#' # joined_result <- permutation_join(dfA, dfB, col_A = "URL_A_col", col_B = "URL_B_col")
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
    return(dplyr::tibble()) # Return empty tibble
  }
  if (!col_A %in% names(data_A)) {
    warning(paste0("Column '", col_A, "' not found in data_A."), call. = FALSE)
    return(dplyr::tibble())
  }
  if (!col_B %in% names(data_B)) {
    warning(paste0("Column '", col_B, "' not found in data_B."), call. = FALSE)
    return(dplyr::tibble())
  }
  if (nrow(data_A) == 0 || nrow(data_B) == 0) {
    # Define expected output columns for empty result
    # This is tricky without knowing actual column names from data_A/data_B
    # For now, return a generic empty tibble or one with core expected names
    return(dplyr::tibble(
      OriginalURL_A = character(0),
      OriginalURL_B = character(0),
      JoinKey = character(0)
      # ... and potentially other columns if we could infer them
    ))
  }


  # --- Internal Helper Function to Prepare Data ---
  .prepare_data_for_join <- function(df, url_col_name,
                                     desired_original_url_col_name,
                                     temp_id_col_name_str,
                                     final_id_col_name_str) {

    # Ensure rlang := syntax works with string names for new columns
    temp_id_sym <- rlang::sym(temp_id_col_name_str)
    final_id_sym <- rlang::sym(final_id_col_name_str)
    desired_orig_url_sym <- rlang::sym(desired_original_url_col_name)

    df_with_id <- df %>%
      dplyr::mutate({{temp_id_sym}} := dplyr::row_number())

    # Generate permutations and link back to original row
    # .data[[url_col_name]] correctly accesses the url column by string name
    prepared_df <- df_with_id %>%
      dplyr::select({{temp_id_sym}}, .current_url_for_permute = .data[[url_col_name]]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(.list_of_perms_df = list(permute_url(.current_url_for_permute))) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(.list_of_perms_df) %>% # Columns: temp_id_sym, .current_url_for_permute, URL (from permute_url), Permutation
      dplyr::rename(
        JoinKey = Permutation,
        {{desired_orig_url_sym}} := .current_url_for_permute # This is the original URL from this specific row
      ) %>%
      dplyr::select({{temp_id_sym}}, {{desired_orig_url_sym}}, JoinKey) %>%
      dplyr::left_join(df_with_id, by = temp_id_col_name_str) %>% # Join by string name
      dplyr::rename({{final_id_sym}} := {{temp_id_sym}}) # Final rename for the ID column

    return(prepared_df)
  }

  # --- Process data_A ---
  data_A_expanded <- .prepare_data_for_join(
    df = data_A,
    url_col_name = col_A,
    desired_original_url_col_name = "OriginalURL_A",
    temp_id_col_name_str = ".id_A_temp_rurl", # Use distinct temp names
    final_id_col_name_str = ".row_id_A_rurl"
  )

  # --- Process data_B ---
  data_B_expanded <- .prepare_data_for_join(
    df = data_B,
    url_col_name = col_B,
    desired_original_url_col_name = "OriginalURL_B",
    temp_id_col_name_str = ".id_B_temp_rurl",
    final_id_col_name_str = ".row_id_B_rurl"
  )
  
  # Handle cases where one or both expansions are empty
  if (nrow(data_A_expanded) == 0 || nrow(data_B_expanded) == 0) {
    return(dplyr::tibble(
      OriginalURL_A = character(0),
      OriginalURL_B = character(0),
      JoinKey = character(0)
      # Ideally, add other columns from data_A and data_B with NA values
      # This is complex to do generically for all possible columns.
      # A simpler approach for now is to return the core columns empty.
    ))
  }


  # --- Join Expanded DataFrames ---
  # Suffixes are applied to non-join columns that are present in both
  # data_A_expanded and data_B_expanded and have the same name.
  # OriginalURL_A, OriginalURL_B, .row_id_A_rurl, .row_id_B_rurl are unique.
  joined_data_raw <- dplyr::inner_join(
    data_A_expanded,
    data_B_expanded,
    by = "JoinKey",
    suffix = c(suffix_A, suffix_B)
  )

  # --- Deduplicate Connections ---
  # Keep one row per unique pair of original input rows that connected.
  if (nrow(joined_data_raw) > 0) {
    final_joined_data <- joined_data_raw %>%
      dplyr::distinct(.row_id_A_rurl, .row_id_B_rurl, .keep_all = TRUE)
  } else {
    final_joined_data <- joined_data_raw # Empty, but keep structure
  }


  # --- Select and Arrange Output Columns ---
  if (nrow(final_joined_data) > 0) {
    # Define core columns that must exist if join was successful
    core_cols <- c("OriginalURL_A", "OriginalURL_B", "JoinKey")
    
    # Ensure core columns are present (should be if nrow > 0)
    # This also helps in selecting them first.
    # Other columns are all remaining columns from the join.
    # We also remove the internal row ID columns.
    
    # Get all names, then arrange
    all_names <- names(final_joined_data)
    
    # Identify original A columns (those not from B and not core/id)
    # This is complex due to suffixes. The `everything()` selector
    # along with explicit ordering of known columns is usually best.

    result <- final_joined_data %>%
      dplyr::select(
        OriginalURL_A,
        OriginalURL_B,
        JoinKey,
        dplyr::everything(), # Puts all other columns after the specified ones
        -.row_id_A_rurl,
        -.row_id_B_rurl
      )
  } else {
    # Return an empty tibble with the expected core column structure
    # and try to infer other columns if possible (difficult generically)
    # For now, a defined empty structure.
    result <- dplyr::tibble(
      OriginalURL_A = character(0),
      OriginalURL_B = character(0),
      JoinKey = character(0)
      # Add other common columns if schema is fixed or known, otherwise this is safest.
    )
  }

  return(dplyr::as_tibble(result)) # Ensure it's a tibble
} 