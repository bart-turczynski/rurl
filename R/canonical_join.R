#' Canonical Join of Two URL Sets (Base R Version)
#'
#' Performs a join between two data frames by canonicalizing URLs to a shared
#' "clean" format using \code{\link{safe_parse_urls}} and then matching on
#' that key.
#' This is suitable for large crawl exports.
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
#' @param name_A Character string, the name of the output column holding the
#'   original \code{data_A} URLs. Defaults to \code{NULL}, in which case the
#'   name is derived from the \code{data_A} argument expression via
#'   \code{deparse(substitute())}. Supply an explicit value for stable output
#'   names when piping or passing anonymous inputs (e.g.
#'   \code{canonical_join(df[df$x > 1, ], get_b())}).
#' @param name_B Character string, the name of the output column holding the
#'   original \code{data_B} URLs. Defaults to \code{NULL}; behaves like
#'   \code{name_A} for \code{data_B}.
#' @param join Join type: \code{"inner"}, \code{"left"}, \code{"right"}, or
#'   \code{"full"}. Defaults to \code{"inner"}.
#' @param collision How to handle duplicate canonical keys within inputs.
#'   \code{"first"} keeps the first row per key, \code{"all"} keeps all rows
#'   (many-to-many), and \code{"error"} stops on duplicates. Defaults to
#'   \code{"first"}.
#' @param on_parse_error How to handle URLs that fail canonicalization.
#'   \code{"keep"} retains them as unmatched rows (for left/right/full joins),
#'   \code{"drop"} removes them before joining, and \code{"error"} stops.
#'   Defaults to \code{"keep"}.
#' @param join_parse_status Which parse statuses yield joinable canonical keys.
#'   \code{"ok"} (default) joins only rows whose \code{parse_status} begins with
#'   \code{"ok"} (\code{"ok"}, \code{"ok-ftp"}, \code{"ok-scheme-relative"}).
#'   \code{"ok_or_warning"} additionally treats parseable-but-suspicious
#'   \code{warning-*} statuses (\code{"warning-no-tld"},
#'   \code{"warning-invalid-tld"}, \code{"warning-public-suffix"}) as joinable.
#'   Joining on warning statuses can increase false-positive matches between
#'   distinct hosts that both fail TLD derivation.
#' @param ... Additional arguments forwarded to \code{\link{safe_parse_urls}},
#'   controlling canonicalization (e.g., \code{protocol_handling},
#'   \code{www_handling}, \code{trailing_slash_handling},
#'   \code{index_page_handling}, \code{path_normalization},
#'   \code{scheme_relative_handling}, \code{host_encoding},
#'   \code{path_encoding}, and the \code{url_standard} selector). When
#'   \code{url_standard} is set, forwarding a governed low-level knob it would
#'   override (e.g. \code{path_encoding}) is an error, exactly as in
#'   \code{\link{safe_parse_url}}.
#'
#' @return A data frame representing the join. The output includes:
#'   \itemize{
#'     \item The original URL columns (named via \code{name_A} / \code{name_B},
#'           or after the input expressions when those are \code{NULL}).
#'     \item \code{JoinKey}: the canonicalized URL used for matching.
#'     \item All other columns from \code{data_A} and \code{data_B} with
#'           suffixes applied.
#'   }
#'   Returns an empty data frame with the expected structure if no matches
#'   are found or if inputs are invalid.
#'
#' @importFrom stats setNames
#' @export
#' @examples
#' A <- data.frame(
#'   URL = c("http://Example.com/Page", "http://example.com/Other"),
#'   ValA = 1:2, stringsAsFactors = FALSE
#' )
#' B <- data.frame(
#'   URL = c("https://www.example.com/Page/", "http://example.com/Miss"),
#'   ValB = c("x", "y"), stringsAsFactors = FALSE
#' )
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
                           name_A = NULL, name_B = NULL,
                           join = c("inner", "left", "right", "full"),
                           collision = c("first", "all", "error"),
                           on_parse_error = c("keep", "drop", "error"),
                           join_parse_status = c("ok", "ok_or_warning"),
                           ...) {
  if (is.null(name_A)) name_A <- deparse(substitute(data_A))
  if (is.null(name_B)) name_B <- deparse(substitute(data_B))
  join <- match.arg(join)
  collision <- match.arg(collision)
  on_parse_error <- match.arg(on_parse_error)
  join_parse_status <- match.arg(join_parse_status)

  # url_standard (RURL-eqzkkohm) is forwarded to safe_parse_urls() via `...`,
  # where missing() cannot see which governed knobs the caller supplied. Detect
  # them by name in the captured dots and run the same conflict check up front,
  # so a conflicting profile + explicit knob fails fast here.
  .check_url_standard_conflicts_dots(list(...))

  if (!.cj_validate_inputs(data_A, data_B, col_A, col_B)) {
    return(data.frame())
  }

  empty_output_template <- .cj_empty_template(
    data_A, data_B, col_A, col_B, name_A, name_B, suffix_A, suffix_B
  )

  # Parse URLs into canonical keys, then resolve drop/collision policy.
  side_A <- .cj_side_state(
    safe_parse_urls(as.character(data_A[[col_A]]), ...),
    data_A, join_parse_status
  )
  side_B <- .cj_side_state(
    safe_parse_urls(as.character(data_B[[col_B]]), ...),
    data_B, join_parse_status
  )

  sides <- .cj_resolve_sides(side_A, side_B, on_parse_error, collision)
  side_A <- sides$A
  side_B <- sides$B

  df_A_join <- .cj_build_join_df(side_A, col_A, suffix_A, on_parse_error, "A")
  df_B_join <- .cj_build_join_df(side_B, col_B, suffix_B, on_parse_error, "B")

  joined <- merge(
    df_A_join, df_B_join,
    by = ".join_key",
    all.x = join %in% c("left", "full"),
    all.y = join %in% c("right", "full"),
    sort = FALSE
  )

  if (nrow(joined) == 0) {
    return(empty_output_template)
  }

  .cj_assemble_result(joined, name_A, name_B)
}

# Validate canonical_join() inputs, emitting the same warnings as before and
# returning FALSE (so the caller returns an empty data.frame) on any failure.
.cj_validate_inputs <- function(data_A, data_B, col_A, col_B) {
  if (!is.data.frame(data_A) || !is.data.frame(data_B)) {
    warning("Inputs 'data_A' and 'data_B' must be data frames.", call. = FALSE)
    return(FALSE)
  }
  .cj_validate_column(data_A, col_A, "data_A") &&
    .cj_validate_column(data_B, col_B, "data_B")
}

# Validate one side's URL column: it must exist and be character or factor.
.cj_validate_column <- function(data, col, which) {
  if (!col %in% names(data)) {
    warning("Column '", col, "' not found in ", which, ".", call. = FALSE)
    return(FALSE)
  }
  if (!is.character(data[[col]]) && !is.factor(data[[col]])) {
    warning(
      "Column '", col, "' in ", which, " must be character or factor.",
      call. = FALSE
    )
    return(FALSE)
  }
  TRUE
}

# Apply the on_parse_error and collision policies to both sides, stopping on
# the "error" variants and returning the (possibly filtered) sides.
.cj_resolve_sides <- function(side_A, side_B, on_parse_error, collision) {
  if (on_parse_error == "error" && (!all(side_A$ok) || !all(side_B$ok))) {
    stop("canonical_join() encountered URL parsing errors.", call. = FALSE)
  }
  if (on_parse_error == "drop") {
    side_A <- .cj_drop_unparsed(side_A)
    side_B <- .cj_drop_unparsed(side_B)
  }
  if (collision != "all") {
    has_dup <- any(duplicated(side_A$key) & side_A$ok) ||
      any(duplicated(side_B$key) & side_B$ok)
    if (collision == "error" && has_dup) {
      stop(
        "canonical_join() found duplicate canonical keys. ",
        "Use collision = \"all\" or \"first\".",
        call. = FALSE
      )
    }
    if (collision == "first") {
      side_A <- .cj_keep_first(side_A)
      side_B <- .cj_keep_first(side_B)
    }
  }
  list(A = side_A, B = side_B)
}

# Expected output structure for empty results, preserving column types.
.cj_empty_template <- function(data_A, data_B, col_A, col_B,
                               name_A, name_B, suffix_A, suffix_B) {
  empty_cols <- list()
  empty_cols[[name_A]] <- data_A[0, col_A, drop = TRUE]
  empty_cols[[name_B]] <- data_B[0, col_B, drop = TRUE]
  empty_cols[["JoinKey"]] <- character(0)
  for (oca in setdiff(names(data_A), col_A)) {
    empty_cols[[paste0(oca, suffix_A)]] <- data_A[0, oca, drop = TRUE]
  }
  for (ocb in setdiff(names(data_B), col_B)) {
    empty_cols[[paste0(ocb, suffix_B)]] <- data_B[0, ocb, drop = TRUE]
  }
  template <- data.frame(empty_cols, stringsAsFactors = FALSE)
  template[, unique(names(template)), drop = FALSE]
}

# Per-side parse state: the working data frame plus the canonical key vector
# and the joinable-row mask. Bundling these keeps the drop/collision steps to
# a single object per side.
.cj_side_state <- function(parsed, data, join_parse_status) {
  n <- nrow(data)
  key <- parsed$clean_url %||% rep(NA_character_, n)
  status <- parsed$parse_status %||% rep("error", n)
  ok <- !is.na(key) & nzchar(key) &
    .is_joinable_status(status, join_parse_status)
  list(data = data, key = key, ok = ok)
}

# Drop rows that failed to parse (on_parse_error = "drop").
.cj_drop_unparsed <- function(side) {
  keep <- side$ok
  side$data <- side$data[keep, , drop = FALSE]
  side$key <- side$key[keep]
  side$ok <- side$ok[keep]
  side
}

# Keep the first row per canonical key (collision = "first"); NA-key rows are
# all retained.
.cj_keep_first <- function(side) {
  keep <- !side$ok | !duplicated(side$key)
  side$data <- side$data[keep, , drop = FALSE]
  side$key <- side$key[keep]
  side$ok <- side$ok[keep]
  side
}

# Build one side's join data frame: the internal match key, the emitted key,
# the original URL, and the suffixed payload columns. NA-key rows get a unique
# sentinel match key when on_parse_error = "keep" so they never match.
.cj_build_join_df <- function(side, col, suffix, on_parse_error, side_tag) {
  data_work <- side$data
  join_key <- side$key
  if (on_parse_error == "keep") {
    join_key <- ifelse(
      side$ok, side$key,
      paste0(".__rurl_na_", side_tag, "__", seq_len(nrow(data_work)))
    )
  }
  df <- data.frame(.join_key = join_key, stringsAsFactors = FALSE)
  df[[paste0(".join_key_out_", side_tag)]] <-
    ifelse(side$ok, side$key, NA_character_)
  df[[paste0(".orig_url_", side_tag)]] <- data_work[[col]]
  for (oc in setdiff(names(data_work), col)) {
    df[[paste0(oc, suffix)]] <- data_work[[oc]]
  }
  df
}

# Assemble the public result from the merged frame: coalesce the emitted key
# across sides, restore the original URL column names, and drop internals.
.cj_assemble_result <- function(joined, name_A, name_B) {
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

  drop_cols <- c(
    ".join_key",
    ".join_key_out_A",
    ".join_key_out_B",
    ".orig_url_A",
    ".orig_url_B"
  )
  for (col_name in setdiff(names(joined), drop_cols)) {
    result[[col_name]] <- joined[[col_name]]
  }

  result
}
