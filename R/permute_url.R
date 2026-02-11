#' Permute URL Variants
#'
#' Generates permutations of a URL across all supported cleaning options
#' (scheme handling, www handling, case handling, trailing slashes,
#' index/default handling, path normalization, scheme-relative handling,
#' subdomain stripping, host encoding, and path encoding).
#'
#' Each argument can be supplied as a single value or a vector of values; the
#' Cartesian product of all supplied values is evaluated, and duplicates are
#' removed from the final result. Use narrower argument sets to reduce the
#' number of generated variants for large datasets.
#'
#' @param urls A character vector of URLs.
#' @param protocol_handling Vector of protocol options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param www_handling Vector of www-handling options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param case_handling Vector of case-handling options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param trailing_slash_handling Vector of trailing-slash options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param index_page_handling Vector of index/default handling options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param path_normalization Vector of path normalization options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param scheme_relative_handling Vector of scheme-relative handling options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param subdomain_levels_to_keep Vector or list of values for
#'   `subdomain_levels_to_keep`. Use `NULL` to keep all subdomains (default).
#'   Values must be non-negative integers or `NULL`.
#' @param host_encoding Vector of host-encoding options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param path_encoding Vector of path-encoding options passed to
#'   [safe_parse_url()]. Defaults to all supported values.
#' @param include_rank Logical. If TRUE, includes `PermutationRank` in the
#'   output, indicating priority based on the order of option values.
#' @return A data.frame with columns `URL` (the input URL) and
#'   `Permutation` (the generated URL variant). If `include_rank = TRUE`,
#'   an additional `PermutationRank` column is included.
#' @export
#' @examples
#' # Generate a compact set of permutations
#' permute_url(
#'   "Example.com/path",
#'   protocol_handling = c("http", "https"),
#'   www_handling = c("none", "keep"),
#'   trailing_slash_handling = c("none", "keep"),
#'   case_handling = c("keep", "lower")
#' )
#'
#' # Include punycode/unicode host variants (ASCII punycode input)
#' permute_url(
#'   "xn--hxajbheg2az3al.xn--qxam", # punycode for a Greek IDN
#'   host_encoding = c("keep", "idna", "unicode"),
#'   path_encoding = c("keep", "encode")
#' )
permute_url <- function(urls,
                        protocol_handling = eval(formals(safe_parse_url)$protocol_handling),
                        www_handling = eval(formals(safe_parse_url)$www_handling),
                        case_handling = eval(formals(safe_parse_url)$case_handling),
                        trailing_slash_handling = eval(formals(safe_parse_url)$trailing_slash_handling),
                        index_page_handling = eval(formals(safe_parse_url)$index_page_handling),
                        path_normalization = eval(formals(safe_parse_url)$path_normalization),
                        scheme_relative_handling = eval(formals(safe_parse_url)$scheme_relative_handling),
                        subdomain_levels_to_keep = list(NULL),
                        host_encoding = eval(formals(safe_parse_url)$host_encoding),
                        path_encoding = eval(formals(safe_parse_url)$path_encoding),
                        include_rank = FALSE) {

  if (length(urls) == 0) {
    empty_df <- data.frame(URL = character(0), Permutation = character(0), stringsAsFactors = FALSE)
    if (isTRUE(include_rank)) {
      empty_df$PermutationRank <- integer(0)
    }
    return(empty_df)
  }

  urls <- as.character(urls)

  # Helper: validate character options against safe_parse_url choices
  validate_choice <- function(arg_values, allowed, arg_name) {
    if (length(arg_values) == 0) {
      stop(paste0("Argument '", arg_name, "' must have at least one value."), call. = FALSE)
    }
    invalid <- setdiff(arg_values, allowed)
    if (length(invalid) > 0) {
      stop(paste0(
        "Argument '", arg_name, "' must be a subset of: ",
        paste(allowed, collapse = ", "), ". Invalid values: ",
        paste(invalid, collapse = ", "), "."
      ), call. = FALSE)
    }
    resolved <- arg_values[arg_values %in% allowed]
    resolved[!duplicated(resolved)]
  }

  # Helper: normalise subdomain options to a list while validating values
  normalize_subdomains <- function(values) {
    if (is.null(values)) return(list(NULL))
    if (!is.list(values)) values <- as.list(values)
    lapply(values, function(v) {
      if (is.null(v)) return(NULL)
      if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0 || v %% 1 != 0) {
        stop("subdomain_levels_to_keep must contain NULL or non-negative integer values.", call. = FALSE)
      }
      as.integer(v)
    })
  }

  safe_defaults <- formals(safe_parse_url)
  protocol_handling <- validate_choice(protocol_handling, eval(safe_defaults$protocol_handling), "protocol_handling")
  www_handling <- validate_choice(www_handling, eval(safe_defaults$www_handling), "www_handling")
  case_handling <- validate_choice(case_handling, eval(safe_defaults$case_handling), "case_handling")
  trailing_slash_handling <- validate_choice(trailing_slash_handling, eval(safe_defaults$trailing_slash_handling), "trailing_slash_handling")
  index_page_handling <- validate_choice(index_page_handling, eval(safe_defaults$index_page_handling), "index_page_handling")
  path_normalization <- validate_choice(path_normalization, eval(safe_defaults$path_normalization), "path_normalization")
  scheme_relative_handling <- validate_choice(scheme_relative_handling, eval(safe_defaults$scheme_relative_handling), "scheme_relative_handling")
  host_encoding <- validate_choice(host_encoding, eval(safe_defaults$host_encoding), "host_encoding")
  path_encoding <- validate_choice(path_encoding, eval(safe_defaults$path_encoding), "path_encoding")
  subdomain_levels_to_keep <- normalize_subdomains(subdomain_levels_to_keep)

  base_grid <- expand.grid(
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    case_handling = case_handling,
    trailing_slash_handling = trailing_slash_handling,
    index_page_handling = index_page_handling,
    path_normalization = path_normalization,
    scheme_relative_handling = scheme_relative_handling,
    host_encoding = host_encoding,
    path_encoding = path_encoding,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  arg_combinations <- vector("list", nrow(base_grid) * max(1, length(subdomain_levels_to_keep)))
  idx <- 1L
  for (i in seq_len(nrow(base_grid))) {
    for (sd in subdomain_levels_to_keep) {
      arg_combinations[[idx]] <- list(
        protocol_handling = base_grid$protocol_handling[[i]],
        www_handling = base_grid$www_handling[[i]],
        case_handling = base_grid$case_handling[[i]],
        trailing_slash_handling = base_grid$trailing_slash_handling[[i]],
        index_page_handling = base_grid$index_page_handling[[i]],
        path_normalization = base_grid$path_normalization[[i]],
        scheme_relative_handling = base_grid$scheme_relative_handling[[i]],
        subdomain_levels_to_keep = sd,
        host_encoding = base_grid$host_encoding[[i]],
        path_encoding = base_grid$path_encoding[[i]]
      )
      idx <- idx + 1L
    }
  }

  all_permutations <- lapply(urls, function(original_url_raw) {
    original_url_input <- enc2utf8(original_url_raw)

    if (is.na(original_url_input) || !nzchar(trimws(original_url_input))) {
      return(data.frame(URL = original_url_raw, Permutation = NA_character_, stringsAsFactors = FALSE))
    }

    baseline_parsed <- safe_parse_url(
      original_url_input,
      protocol_handling = "keep",
      www_handling = "none",
      tld_source = "all",
      case_handling = "keep",
      trailing_slash_handling = "none",
      subdomain_levels_to_keep = NULL,
      host_encoding = "keep",
      path_encoding = "keep"
    )

    if (is.null(baseline_parsed) || is.na(baseline_parsed$host) || !nzchar(trimws(baseline_parsed$host))) {
      return(data.frame(URL = original_url_raw, Permutation = NA_character_, stringsAsFactors = FALSE))
    }

    bare_host_baseline <- sub("^(www[0-9]*\\.)", "", baseline_parsed$host, ignore.case = TRUE)
    if (!nzchar(trimws(bare_host_baseline))) {
      return(data.frame(URL = original_url_raw, Permutation = NA_character_, stringsAsFactors = FALSE))
    }

    raw_path_baseline <- baseline_parsed$path %||% ""
    is_root_path <- raw_path_baseline %in% c("", "/")

    perms <- vapply(seq_along(arg_combinations), function(i) {
      arg_set <- arg_combinations[[i]]
      parsed <- do.call(
        safe_parse_url,
        c(list(url = original_url_input),
          arg_set[c(
            "protocol_handling", "www_handling", "case_handling",
            "trailing_slash_handling", "subdomain_levels_to_keep",
            "index_page_handling", "path_normalization", "scheme_relative_handling",
            "host_encoding", "path_encoding"
          )])
      )
      if (is.null(parsed)) return(NA_character_)
      clean <- parsed$clean_url %||% NA_character_
      if (is.na(clean) || !nzchar(trimws(clean))) return(NA_character_)
      query_part <- if (!is.null(parsed$query) && !is.na(parsed$query) && nzchar(parsed$query)) paste0("?", parsed$query) else ""
      fragment_part <- if (!is.null(parsed$fragment) && !is.na(parsed$fragment) && nzchar(parsed$fragment)) paste0("#", parsed$fragment) else ""
      paste0(clean, query_part, fragment_part)
    }, character(1), USE.NAMES = FALSE)

    perm_ranks <- seq_along(arg_combinations)
    perms_df <- data.frame(
      Permutation = perms,
      PermutationRank = perm_ranks,
      stringsAsFactors = FALSE
    )

    perms_df <- perms_df[!is.na(perms_df$Permutation), , drop = FALSE]
    perms_df <- perms_df[nzchar(trimws(perms_df$Permutation)), , drop = FALSE]
    perms_df <- perms_df[nzchar(trimws(gsub("^(https?://)?(www[0-9]*\\.)?", "", perms_df$Permutation, perl = TRUE))), , drop = FALSE]
    invalid_strings <- c("http://", "https://", "http://www.", "https://www.", "www.", "http://www/", "https://www/", "www/", "/", "")
    perms_df <- perms_df[!perms_df$Permutation %in% invalid_strings, , drop = FALSE]
    if (nrow(perms_df) > 0) {
      perms_df <- perms_df[!duplicated(perms_df$Permutation), , drop = FALSE]
    }

    if (is_root_path && nrow(perms_df) > 0) {
      toggled <- ifelse(endsWith(perms_df$Permutation, "/"),
                        sub("/+$", "", perms_df$Permutation),
                        paste0(perms_df$Permutation, "/"))
      toggled_df <- data.frame(
        Permutation = toggled,
        PermutationRank = length(arg_combinations) + seq_along(toggled),
        stringsAsFactors = FALSE
      )
      toggled_df <- toggled_df[!toggled_df$Permutation %in% perms_df$Permutation, , drop = FALSE]
      if (nrow(toggled_df) > 0) {
        perms_df <- rbind(perms_df, toggled_df)
      }
    }
    perms_df <- perms_df[nzchar(trimws(perms_df$Permutation)), , drop = FALSE]

    if (nrow(perms_df) == 0) {
      perms_df <- data.frame(
        Permutation = NA_character_,
        PermutationRank = NA_integer_,
        stringsAsFactors = FALSE
      )
    }

    perms_df$URL <- original_url_raw
    perms_df <- perms_df[, c("URL", "Permutation", "PermutationRank"), drop = FALSE]

    if (!isTRUE(include_rank)) {
      perms_df$PermutationRank <- NULL
    }

    perms_df
  })

  final_df <- unique(do.call(rbind, all_permutations))
  rownames(final_df) <- NULL
  final_df
}
