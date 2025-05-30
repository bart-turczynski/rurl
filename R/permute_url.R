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

    raw_path <- parsed_curl$path %||% ""
    safe_query <- parsed_curl$query %||% NA_character_
    safe_fragment <- parsed_curl$fragment %||% NA_character_

    query_part <- if (!is.na(safe_query) && nzchar(safe_query)) paste0("?", safe_query) else ""
    fragment_part <- if (!is.na(safe_fragment) && nzchar(safe_fragment)) paste0("#", safe_fragment) else ""
    
    # Construct the full path + query + fragment string
    path_query_fragment <- paste0(raw_path, query_part, fragment_part)

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
        # Variant 1: No added slash (respects original path structure)
        # If raw_path is "/", it remains "/". If "/path", remains "/path". If "", remains "".
        url_no_added_slash <- paste0(s, current_perm_host, path_query_fragment)
        current_url_perms_set <- c(current_url_perms_set, url_no_added_slash)

        # Variant 2: With an added trailing slash (if applicable)
        # Logic: 
        #   - If raw_path is empty (e.g. "example.com"), add "/" -> "example.com/"
        #   - If raw_path is not empty and does not end with "/" (e.g. "/path"), add "/" -> "/path/"
        #   - If raw_path already ends with "/" (e.g. "/path/"), this variant is covered by url_no_added_slash
        
        path_for_slash_variant <- raw_path
        added_slash_path_query_fragment <- ""

        if (path_for_slash_variant == "") {
          added_slash_path_query_fragment <- paste0("/", query_part, fragment_part)
          url_with_added_slash <- paste0(s, current_perm_host, added_slash_path_query_fragment)
          current_url_perms_set <- c(current_url_perms_set, url_with_added_slash)
        } else if (substr(path_for_slash_variant, nchar(path_for_slash_variant), nchar(path_for_slash_variant)) != "/") {
          added_slash_path_query_fragment <- paste0(path_for_slash_variant, "/", query_part, fragment_part)
          url_with_added_slash <- paste0(s, current_perm_host, added_slash_path_query_fragment)
          current_url_perms_set <- c(current_url_perms_set, url_with_added_slash)
        }
        # If path_for_slash_variant already ends with "/", url_no_added_slash is sufficient.
      }
    }

    unique_permutations <- unique(current_url_perms_set)
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