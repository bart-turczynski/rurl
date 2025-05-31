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
#' permute_url("test.com/folder/subfolder/path?parameter=value")
permute_url <- function(urls) {
  all_permutations <- list()

  # %||% is defined globally in R/rurl.R

  for (original_url_raw in urls) {
    # Sanitize the input URL immediately
    original_url_input <- iconv(original_url_raw, from = "", to = "UTF-8", sub = "byte")

    if (is.na(original_url_input) || !nzchar(trimws(original_url_input))) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_raw, # Store raw original URL
        Permutation = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    # original_url_input is now the sanitized version for further processing

    has_scheme <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", original_url_input)
    url_to_parse <- original_url_input
    if (!has_scheme) {
      url_to_parse <- paste0("http://", original_url_input)
    }

    parsed_curl <- tryCatch(
      curl::curl_parse_url(url_to_parse),
      error = function(e) NULL
    )

    # Sanitize components from curl_parse_url
    raw_host_from_curl <- parsed_curl$host %||% NA_character_
    raw_host <- iconv(raw_host_from_curl, from = "", to = "UTF-8", sub = "byte")

    if (is.null(parsed_curl) || is.na(raw_host) || !nzchar(trimws(raw_host))) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_raw, # Store raw original URL
        Permutation = NA_character_,
        stringsAsFactors = FALSE
      )
      next
    }

    stripped_bare_host <- sub("^(www[0-9]*\\.)", "", raw_host, ignore.case = TRUE)
    # No need to sanitize stripped_bare_host again if raw_host is clean and sub() is safe.
    
    if (!nzchar(trimws(stripped_bare_host))) {
        all_permutations[[length(all_permutations) + 1]] <- data.frame(
            URL = original_url_raw, # Store raw original URL
            Permutation = NA_character_,
            stringsAsFactors = FALSE
        )
        next
    }

    # Sanitize other curl components
    raw_path_from_curl_orig <- parsed_curl$path %||% ""
    raw_path_from_curl <- iconv(raw_path_from_curl_orig, from = "", to = "UTF-8", sub = "byte")

    safe_query_orig <- parsed_curl$query %||% NA_character_
    safe_query <- iconv(safe_query_orig, from = "", to = "UTF-8", sub = "byte")

    safe_fragment_orig <- parsed_curl$fragment %||% NA_character_
    safe_fragment <- iconv(safe_fragment_orig, from = "", to = "UTF-8", sub = "byte")

    query_part <- if (!is.na(safe_query) && nzchar(safe_query)) paste0("?", safe_query) else ""
    fragment_part <- if (!is.na(safe_fragment) && nzchar(safe_fragment)) paste0("#", safe_fragment) else ""
    query_fragment_suffix <- paste0(query_part, fragment_part)

    host_prefixes <- c("", "www.") 
    schemes <- c("", "http://", "https://")

    current_url_perms_set <- character()

    for (hp in host_prefixes) {
      current_perm_host <- paste0(hp, stripped_bare_host)
      if (!nzchar(current_perm_host)) next 
      
      for (s in schemes) {
        current_base_schemed_host <- paste0(s, current_perm_host)

        # Check if the path from curl indicates a root domain or a specific path
        is_root_domain_path <- (raw_path_from_curl == "" || raw_path_from_curl == "/")

        if (is_root_domain_path) {
          # Case 1: Input was effectively a root domain
          # Permutation with NO path component (host only, then query/fragment)
          current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, query_fragment_suffix))
          
          # Permutation with a SINGLE SLASH path component, then query/fragment
          current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, "/", query_fragment_suffix))
        } else {
          # Case 2: Input had a specific path (e.g., "test.com/folder/subfolder")
          # This means raw_path_from_curl is not "" and not "/"

          # Version 1: The path as it is (e.g., /path or /path/)
          current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, raw_path_from_curl, query_fragment_suffix))

          # Version 2: The path with the alternative trailing slash state
          if (endsWith(raw_path_from_curl, "/")) {
            # Original path ended with a slash, so create version without it
            path_alternative <- sub("/$", "", raw_path_from_curl)
          } else {
            # Original path did not end with a slash, so create version with it
            path_alternative <- paste0(raw_path_from_curl, "/")
          }
          current_url_perms_set <- c(current_url_perms_set, paste0(current_base_schemed_host, path_alternative, query_fragment_suffix))
        }
      }
    }

    unique_permutations <- unique(current_url_perms_set)
    unique_permutations <- unique_permutations[nzchar(trimws(gsub("^(https?://)?(www[.])?", "", unique_permutations, perl = TRUE)))]
    unique_permutations <- unique_permutations[!unique_permutations %in% c("http://", "https://", "http://www.", "https://www.", "www.", "")]
    unique_permutations <- unique_permutations[nzchar(trimws(unique_permutations))]

    if (length(unique_permutations) > 0) {
      all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_raw, # Store raw original URL
        Permutation = unique_permutations,
        stringsAsFactors = FALSE
      )
    } else {
       all_permutations[[length(all_permutations) + 1]] <- data.frame(
        URL = original_url_raw, # Store raw original URL
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