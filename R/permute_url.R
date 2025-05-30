#' Permute URL Variants
#'
#' Generates various permutations of a URL, altering scheme, www prefix, and trailing slash.
#'
#' As an SEO expert I need to create altenative variants of URLs for joining data.frames,
#' comparing reports, etc. Various tools may report URLs in various formats:
#' with or without the protocol, with or without www, and with http or https
#' as the protocol and with or without the trailing slash.
#'
#' @param urls A character vector of URLs.
#' @return A data.frame with two columns: `OriginalURL` (the input URL) and
#'   `Permutation` (the generated URL variant).
#' @importFrom curl curl_parse_url
#' @export
#' @examples
#' permute_url("example.com/path?query=1")
#' permute_url(c("http://www.test.co.uk/one", "another.com/two/"))
permute_url <- function(urls) {
  all_permutations <- list()

  # %||% is defined globally in R/rurl.R

  for (original_url_input in urls) {
    if (is.na(original_url_input) || !nzchar(trimws(original_url_input))) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        OriginalURL = original_url_input,
        Permutation = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    has_scheme <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", original_url_input)
    url_to_parse <- original_url_input
    if (!has_scheme) {
      url_to_parse <- paste0("http://", original_url_input)
    }

    parsed_curl <- tryCatch(
      curl::curl_parse_url(url_to_parse),
      error = function(e) NULL
    )

    # Use .GlobalEnv$`%||%` or ensure it's available if this file were sourced alone
    # For package context, it's fine as it is defined in rurl.R
    raw_host <- parsed_curl$host %||% NA_character_

    if (is.null(parsed_curl) || is.na(raw_host) || !nzchar(trimws(raw_host))) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        OriginalURL = original_url_input,
        Permutation = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    stripped_bare_host <- sub("^(www[0-9]*\\.)", "", raw_host, ignore.case = TRUE)
    
    if (!nzchar(trimws(stripped_bare_host))) {
        all_permutations[[length(all_permutations) + 1]] <- data.frame(
            OriginalURL = original_url_input,
            Permutation = NA_character_,
            stringsAsFactors = FALSE
        )
        next
    }

    raw_path <- parsed_curl$path %||% ""
    query_part <- if (!is.na(parsed_curl$query) && nzchar(parsed_curl$query)) paste0("?", parsed_curl$query) else ""
    fragment_part <- if (!is.na(parsed_curl$fragment) && nzchar(parsed_curl$fragment)) paste0("#", parsed_curl$fragment) else ""

    host_prefixes <- c("", "www.") 
    schemes <- c("", "http://", "https://")

    current_url_perms_set <- character()

    for (hp in host_prefixes) {
      current_perm_host <- paste0(hp, stripped_bare_host)
      if (hp == "www." && grepl("^www\\.", stripped_bare_host, ignore.case = TRUE)){
          current_perm_host <- stripped_bare_host
      }
      if (!nzchar(current_perm_host)) next
      
      for (s in schemes) {
        url_variant1 <- paste0(s, current_perm_host, raw_path, query_part, fragment_part)
        current_url_perms_set <- c(current_url_perms_set, url_variant1)

        if (raw_path == "") {
          url_variant2 <- paste0(s, current_perm_host, "/", query_part, fragment_part)
          current_url_perms_set <- c(current_url_perms_set, url_variant2)
        } else if (substr(raw_path, nchar(raw_path), nchar(raw_path)) != "/") {
          url_variant2 <- paste0(s, current_perm_host, raw_path, "/", query_part, fragment_part)
          current_url_perms_set <- c(current_url_perms_set, url_variant2)
        }
      }
    }

    unique_permutations <- unique(current_url_perms_set)
    unique_permutations <- unique_permutations[nzchar(trimws(gsub("^(https?://)?(www[.])?", "", unique_permutations, perl = TRUE)))]
    unique_permutations <- unique_permutations[!unique_permutations %in% c("http://", "https://", "http://www.", "https://www.", "www.", "")]
    unique_permutations <- unique_permutations[nzchar(trimws(unique_permutations))]

    if (length(unique_permutations) > 0) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        OriginalURL = original_url_input,
        Permutation = unique_permutations,
        stringsAsFactors = FALSE
      )
    } else {
       all_permutations[[length(all_permutations) + 1]] <- data.frame(
        OriginalURL = original_url_input,
        Permutation = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(all_permutations) == 0) {
    return(data.frame(OriginalURL = character(0), Permutation = character(0), stringsAsFactors = FALSE))
  }

  final_df <- do.call(rbind, all_permutations)
  final_df <- unique(final_df) 
  rownames(final_df) <- NULL   
  return(final_df)
} 