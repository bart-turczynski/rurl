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
  if (length(urls) == 0) {
    return(data.frame(URL = character(0), Permutation = character(0), stringsAsFactors = FALSE))
  }

  all_permutations <- lapply(urls, function(original_url_raw) {
    original_url_input <- enc2utf8(original_url_raw)

    if (is.na(original_url_input) || !nzchar(trimws(original_url_input))) {
      return(data.frame(URL = original_url_raw, Permutation = NA_character_, stringsAsFactors = FALSE))
    }

    parsed <- ._safe_parse_url_impl(
      url = original_url_input,
      protocol_handling = "keep",
      www_handling = "none",
      tld_source = "all",
      case_handling = "keep",
      trailing_slash_handling = "none",
      subdomain_levels_to_keep = NULL
    )

    if (is.null(parsed) || is.na(parsed$host) || !nzchar(trimws(parsed$host))) {
      return(data.frame(URL = original_url_raw, Permutation = NA_character_, stringsAsFactors = FALSE))
    }

    host_base <- parsed$host
    bare_host <- sub("^(www[0-9]*\\.)", "", host_base, ignore.case = TRUE)
    if (!nzchar(trimws(bare_host))) {
      return(data.frame(URL = original_url_raw, Permutation = NA_character_, stringsAsFactors = FALSE))
    }
    host_variants <- c(bare_host, paste0("www.", bare_host))
    host_variants <- unique(host_variants[nzchar(trimws(host_variants))])

    raw_path <- parsed$path %||% ""
    query_part <- if (!is.na(parsed$query %||% NA_character_) && nzchar(parsed$query)) paste0("?", parsed$query) else ""
    fragment_part <- if (!is.na(parsed$fragment %||% NA_character_) && nzchar(parsed$fragment)) paste0("#", parsed$fragment) else ""
    query_fragment_suffix <- paste0(query_part, fragment_part)

    schemes <- c("", "http://", "https://")
    is_root_domain_path <- raw_path %in% c("", "/")

    current_perms <- character()
    for (h in host_variants) {
      for (s in schemes) {
        base <- paste0(s, h)
        if (is_root_domain_path) {
          current_perms <- c(current_perms, paste0(base, query_fragment_suffix))
          current_perms <- c(current_perms, paste0(base, "/", query_fragment_suffix))
        } else {
          current_perms <- c(current_perms, paste0(base, raw_path, query_fragment_suffix))
          alt_path <- if (endsWith(raw_path, "/")) substr(raw_path, 1, nchar(raw_path) - 1) else paste0(raw_path, "/")
          current_perms <- c(current_perms, paste0(base, alt_path, query_fragment_suffix))
        }
      }
    }

    unique_permutations <- unique(current_perms)
    unique_permutations <- unique_permutations[nzchar(trimws(gsub("^(https?://)?(www[.])?", "", unique_permutations, perl = TRUE)))]
    unique_permutations <- unique_permutations[!unique_permutations %in% c("http://", "https://", "http://www.", "https://www.", "www.", "")]
    unique_permutations <- unique_permutations[nzchar(trimws(unique_permutations))]

    if (length(unique_permutations) == 0) {
      unique_permutations <- NA_character_
    }

    data.frame(
      URL = original_url_raw,
      Permutation = unique_permutations,
      stringsAsFactors = FALSE
    )
  })

  final_df <- unique(do.call(rbind, all_permutations))
  rownames(final_df) <- NULL
  final_df
}
