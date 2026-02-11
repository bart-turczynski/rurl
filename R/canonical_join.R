#' Canonical Join of Two URL Sets (Base R Version)
#'
#' Performs a join between two data frames by canonicalizing URLs to a shared
#' "clean" format using \code{\link{safe_parse_urls}} and then matching on that key.
#' This avoids permutation expansion and is suitable for large crawl exports.
#'
#' @param data_A A data frame containing URLs for the left side of the join.
#' @param data_B A data frame containing URLs for the right side of the join.
#' @param col_A Character string, the name of the column in \code{data_A} that
#'   contains URLs. Defaults to "URL".
#' @param col_B Character string, the name of the column in \code{data_B} that
#'   contains URLs. Defaults to "URL".
#' @param suffix_A Character string, suffix to append to \code{data_A} columns
#'   (excluding the URL column) in the output. Defaults to "_A".
#' @param suffix_B Character string, suffix to append to \code{data_B} columns
#'   (excluding the URL column) in the output. Defaults to "_B".
#' @param join Join type: \code{"inner"}, \code{"left"}, \code{"right"}, or
#'   \code{"full"}. Defaults to \code{"inner"}.
#' @param collision How to handle duplicate canonical keys within inputs.
#'   \code{"first"} keeps the first row per key, \code{"all"} keeps all rows
#'   (many-to-many), and \code{"error"} stops on duplicates. Defaults to \code{"first"}.
#' @param on_parse_error How to handle URLs that fail canonicalization.
#'   \code{"keep"} retains them as unmatched rows (for left/right/full joins),
#'   \code{"drop"} removes them before joining, and \code{"error"} stops.
#'   Defaults to \code{"keep"}.
#' @param ... Additional arguments forwarded to \code{\link{safe_parse_urls}},
#'   controlling canonicalization (e.g., \code{protocol_handling},
#'   \code{www_handling}, \code{trailing_slash_handling}, \code{index_page_handling},
#'   \code{path_normalization}, \code{scheme_relative_handling}, \code{host_encoding},
#'   \code{path_encoding}).
#'
#' @return A data frame representing the join. The output includes:
#'   \itemize{
#'     \item The original URL columns (named after the input objects).
#'     \item \code{JoinKey}: the canonicalized URL used for matching.
#'     \item All other columns from \code{data_A} and \code{data_B} with
#'           suffixes applied.
#'   }
#'   Returns an empty data frame with the expected structure if no matches are found
#'   or if inputs are invalid.
#'
#' @export
#' @examples
#' A <- data.frame(URL = c("http://Example.com/Page", "http://example.com/Other"),
#'                 ValA = 1:2, stringsAsFactors = FALSE)
#' B <- data.frame(URL = c("https://www.example.com/Page/", "http://example.com/Miss"),
#'                 ValB = c("x", "y"), stringsAsFactors = FALSE)
#'
#' canonical_join(
#'   A, B,
#'   protocol_handling = "strip",
#'   www_handling = "strip",
#'   case_handling = "lower_host",
#'   trailing_slash_handling = "strip"
#' )
canonical_join <- function(data_A, data_B,
                           col_A = "URL", col_B = "URL",
                           suffix_A = "_A", suffix_B = "_B",
                           join = c("inner", "left", "right", "full"),
                           collision = c("first", "all", "error"),
                           on_parse_error = c("keep", "drop", "error"),
                           ...) {
  name_A <- deparse(substitute(data_A))
  name_B <- deparse(substitute(data_B))
  join <- match.arg(join)
  collision <- match.arg(collision)
  on_parse_error <- match.arg(on_parse_error)

  if (!is.data.frame(data_A) || !is.data.frame(data_B)) {
    warning("Inputs 'data_A' and 'data_B' must be data frames.", call. = FALSE)
    return(data.frame())
  }
  if (!col_A %in% names(data_A)) {
    warning(paste0("Column '", col_A, "' not found in data_A."), call. = FALSE)
    return(data.frame())
  }
  if (!col_B %in% names(data_B)) {
    warning(paste0("Column '", col_B, "' not found in data_B."), call. = FALSE)
    return(data.frame())
  }
  if (!is.character(data_A[[col_A]]) && !is.factor(data_A[[col_A]])) {
    warning(paste0("Column '", col_A, "' in data_A must be character or factor."), call. = FALSE)
    return(data.frame())
  }
  if (!is.character(data_B[[col_B]]) && !is.factor(data_B[[col_B]])) {
    warning(paste0("Column '", col_B, "' in data_B must be character or factor."), call. = FALSE)
    return(data.frame())
  }

  # Expected output structure for empty results, preserving column types
  empty_cols <- list()
  empty_cols[[name_A]] <- data_A[0, col_A, drop = TRUE]
  empty_cols[[name_B]] <- data_B[0, col_B, drop = TRUE]
  empty_cols[["JoinKey"]] <- character(0)
  other_cols_A <- setdiff(names(data_A), col_A)
  for (oca in other_cols_A) empty_cols[[paste0(oca, suffix_A)]] <- data_A[0, oca, drop = TRUE]
  other_cols_B <- setdiff(names(data_B), col_B)
  for (ocb in other_cols_B) empty_cols[[paste0(ocb, suffix_B)]] <- data_B[0, ocb, drop = TRUE]
  empty_output_template <- data.frame(empty_cols, stringsAsFactors = FALSE)
  empty_output_template <- empty_output_template[, unique(names(empty_output_template)), drop = FALSE]

  # Parse URLs into canonical keys
  parsed_A <- safe_parse_urls(as.character(data_A[[col_A]]), ...)
  parsed_B <- safe_parse_urls(as.character(data_B[[col_B]]), ...)

  key_A <- parsed_A$clean_url %||% rep(NA_character_, nrow(data_A))
  key_B <- parsed_B$clean_url %||% rep(NA_character_, nrow(data_B))
  status_A <- parsed_A$parse_status %||% rep("error", nrow(data_A))
  status_B <- parsed_B$parse_status %||% rep("error", nrow(data_B))

  ok_A <- !is.na(key_A) & nzchar(key_A) & grepl("^ok", status_A)
  ok_B <- !is.na(key_B) & nzchar(key_B) & grepl("^ok", status_B)

  if (on_parse_error == "error" && (any(!ok_A) || any(!ok_B))) {
    stop("canonical_join() encountered URL parsing errors.", call. = FALSE)
  }

  # Optionally drop rows that failed to parse
  data_A_work <- data_A
  data_B_work <- data_B
  if (on_parse_error == "drop") {
    data_A_work <- data_A_work[ok_A, , drop = FALSE]
    data_B_work <- data_B_work[ok_B, , drop = FALSE]
    key_A <- key_A[ok_A]
    key_B <- key_B[ok_B]
    ok_A <- ok_A[ok_A]
    ok_B <- ok_B[ok_B]
  }

  # Handle duplicate keys
  if (collision != "all") {
    dup_A <- duplicated(key_A) & ok_A
    dup_B <- duplicated(key_B) & ok_B
    if (collision == "error" && (any(dup_A) || any(dup_B))) {
      stop("canonical_join() found duplicate canonical keys. Use collision = \"all\" or \"first\".", call. = FALSE)
    }
    if (collision == "first") {
      keep_A <- !ok_A | !duplicated(key_A)
      keep_B <- !ok_B | !duplicated(key_B)
      data_A_work <- data_A_work[keep_A, , drop = FALSE]
      data_B_work <- data_B_work[keep_B, , drop = FALSE]
      key_A <- key_A[keep_A]
      key_B <- key_B[keep_B]
      ok_A <- ok_A[keep_A]
      ok_B <- ok_B[keep_B]
    }
  }

  # Internal join keys (avoid matching NA rows when on_parse_error = "keep")
  join_key_A <- key_A
  join_key_B <- key_B
  if (on_parse_error == "keep") {
    join_key_A <- ifelse(ok_A, key_A, paste0(".__rurl_na_A__", seq_len(nrow(data_A_work))))
    join_key_B <- ifelse(ok_B, key_B, paste0(".__rurl_na_B__", seq_len(nrow(data_B_work))))
  }

  df_A_join <- data.frame(
    .join_key = join_key_A,
    .join_key_out_A = ifelse(ok_A, key_A, NA_character_),
    .orig_url_A = data_A_work[[col_A]],
    stringsAsFactors = FALSE
  )
  other_cols_A <- setdiff(names(data_A_work), col_A)
  for (oca in other_cols_A) {
    df_A_join[[paste0(oca, suffix_A)]] <- data_A_work[[oca]]
  }

  df_B_join <- data.frame(
    .join_key = join_key_B,
    .join_key_out_B = ifelse(ok_B, key_B, NA_character_),
    .orig_url_B = data_B_work[[col_B]],
    stringsAsFactors = FALSE
  )
  other_cols_B <- setdiff(names(data_B_work), col_B)
  for (ocb in other_cols_B) {
    df_B_join[[paste0(ocb, suffix_B)]] <- data_B_work[[ocb]]
  }

  all_x <- join %in% c("left", "full")
  all_y <- join %in% c("right", "full")
  joined <- merge(df_A_join, df_B_join, by = ".join_key", all.x = all_x, all.y = all_y, sort = FALSE)

  if (nrow(joined) == 0) {
    return(empty_output_template)
  }

  join_key_out <- joined$.join_key_out_A
  if (!is.null(joined$.join_key_out_B)) {
    na_idx <- is.na(join_key_out)
    if (any(na_idx)) {
      join_key_out[na_idx] <- joined$.join_key_out_B[na_idx]
    }
  }

  result <- data.frame(
    setNames(list(joined$.orig_url_A), name_A),
    setNames(list(joined$.orig_url_B), name_B),
    JoinKey = join_key_out,
    stringsAsFactors = FALSE
  )

  drop_cols <- c(".join_key", ".join_key_out_A", ".join_key_out_B", ".orig_url_A", ".orig_url_B")
  other_cols <- setdiff(names(joined), drop_cols)
  for (col_name in other_cols) {
    result[[col_name]] <- joined[[col_name]]
  }

  result
}
