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
#' @return A data.frame with two columns: `URL` (the input URL) and
#'   `Permutation` (the generated URL variant).
#' @importFrom curl curl_parse_url
#' @export
#' @examples
#' permute_url("example.com/path?query=1")
#' permute_url(c("http://www.test.co.uk/one", "another.com/two/"))
#' permute_url("example.com")
permute_url <- function(urls) {
  all_permutations <- list()

  # %||% is defined globally in R/rurl.R

  for (original_url_input in urls) {
    if (is.na(original_url_input) || !nzchar(trimws(original_url_input))) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_input,
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

    raw_host <- parsed_curl$host %||% NA_character_

    if (is.null(parsed_curl) || is.na(raw_host) || !nzchar(trimws(raw_host))) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_input,
        Permutation = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    stripped_bare_host <- sub("^(www[0-9]*\\.)", "", raw_host, ignore.case = TRUE)
    
    if (!nzchar(trimws(stripped_bare_host))) {
        all_permutations[[length(all_permutations) + 1]] <- data.frame(
            URL = original_url_input,
            Permutation = NA_character_,
            stringsAsFactors = FALSE
        )
        next
    }

    # path from curl_parse_url can be "" or "/" for root, or e.g. "/foo"
    raw_path_from_curl <- parsed_curl$path %||% ""
    safe_query <- parsed_curl$query %||% NA_character_
    safe_fragment <- parsed_curl$fragment %||% NA_character_

    query_part <- if (!is.na(safe_query) && nzchar(safe_query)) paste0("?", safe_query) else ""
    fragment_part <- if (!is.na(safe_fragment) && nzchar(safe_fragment)) paste0("#", safe_fragment) else ""
    query_fragment_suffix <- paste0(query_part, fragment_part)

    host_prefixes <- c("", "www.") 
    schemes <- c("", "http://", "https://")

    current_url_perms_set <- character()

    for (hp in host_prefixes) {
      current_perm_host <- paste0(hp, stripped_bare_host)
      if (hp == "www." && grepl("^www\\.", stripped_bare_host, ignore.case = TRUE)){
          current_perm_host <- stripped_bare_host # Should not happen if sub() is correct
      }
      if (!nzchar(current_perm_host)) next # Should be caught by earlier checks
      
      base_for_permutations <- paste0(s, current_perm_host) # scheme + www.host or host

      for (s in schemes) {
        current_base_schemed_host <- paste0(s, current_perm_host)

        # 1. Permutation with NO path component (just host, then query/fragment)
        # e.g., example.com?query=1
        current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, query_fragment_suffix))

        # 2. Permutation with a SINGLE SLASH path component, then query/fragment
        # e.g., example.com/?query=1
        current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, "/", query_fragment_suffix))

        # 3. If raw_path_from_curl was something more specific than "" or "/", include it and its slashed version.
        # This handles inputs like "example.com/page" or "example.com/page/".
        # The unique() later will remove redundancies if raw_path_from_curl was "" or "/".
        if (nzchar(raw_path_from_curl) && raw_path_from_curl != "/") {
          # 3a. Original path as is
          current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, raw_path_from_curl, query_fragment_suffix))
          
          # 3b. Original path with a trailing slash (if it doesn't already have one)
          if (substr(raw_path_from_curl, nchar(raw_path_from_curl), nchar(raw_path_from_curl)) != "/") {
            current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, raw_path_from_curl, "/", query_fragment_suffix))
          }
        }
      }
    }

    unique_permutations <- unique(current_url_perms_set)
    # Filters:
    unique_permutations <- unique_permutations[nzchar(trimws(gsub("^(https?://)?(www[.])?", "", unique_permutations, perl = TRUE)))]
    unique_permutations <- unique_permutations[!unique_permutations %in% c("http://", "https://", "http://www.", "https://www.", "www.", "")]
    unique_permutations <- unique_permutations[nzchar(trimws(unique_permutations))]

    if (length(unique_permutations) > 0) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_input,
        Permutation = unique_permutations,
        stringsAsFactors = FALSE
      )
    } else {
       all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_input,
        Permutation = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(all_permutations) == 0) {
    return(data.frame(URL = character(0), Permutation = character(0), stringsAsFactors = FALSE))
  }

  final_df <- do.call(rbind, all_permutations)
  final_df <- unique(final_df) 
  rownames(final_df) <- NULL   
  return(final_df)
} 