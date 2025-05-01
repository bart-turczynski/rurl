# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Parse a URL safely with scheme handling
#'
#' @param url A character vector containing one or more URLs to be parsed.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          Can be one of "keep", "none", "strip", "http", "https".
#'                          The protocol is preserved if it exists, and "http://" is
#'                          added if missing. If "none", no protocol is added. If
#'                          "http://" or "https://" the given protocol is added
#'                          or changed to the one indicated.
#' @return A named list with components such as `scheme`, `host`, `path`, or `NULL` if parsing fails.
#' @keywords internal
#' @description
#' This function parses a URL safely, handling different protocols such as http(s), ftp, and ftps.
#' If no scheme is present, it assumes "http://". Other schemes will return NULL.
#' @export
#' @examples
#' safe_parse_url("http://example.com", "http")
#' safe_parse_url("example.com", "keep")
safe_parse_url <- function(url, protocol_handling = c("keep", "none", "strip", "http", "https")) {
  protocol_handling <- match.arg(protocol_handling)

  if (is.na(url) || !is.character(url) || url == "") return(NULL)

  # Note: ws:// allowed here for test purposes only to trigger fallback error path
  allowed_prefixes <- c("http://", "https://", "ftp://", "ftps://", "ws://")

  # Detect if URL starts with an allowed scheme
  has_valid_prefix <- any(startsWith(tolower(url), allowed_prefixes))

  # Detect if it looks like it has any scheme (valid or not)
  looks_like_protocol <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:", url)

  # ❌ Reject if a scheme exists but it's not in our whitelist
  if (looks_like_protocol && !has_valid_prefix) return(NULL)

  # ✅ Only prepend http:// if no scheme at all
  if (!looks_like_protocol) {
    url <- paste0("http://", url)
  }

  result <- tryCatch(curl::curl_parse_url(url), error = function(e) NULL)
  if (is.null(result)) return(NULL)

  result$scheme <- switch(protocol_handling,
                          none  = if (!has_valid_prefix) NA_character_ else result$scheme,
                          strip = NA_character_,
                          http  = "http",
                          https = "https",
                          keep  = result$scheme
  )

  result
}

#' Get the parse status of URLs
#'
#' @param url A character vector of URLs to be parsed.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          Can be one of "keep", "none", "strip", "http", "https".
#'                          The protocol is preserved if it exists, and "http://" is
#'                          added if missing. If "none", no protocol is added. If
#'                          "http://" or "https://" the given protocol is added
#'                          or changed to the one indicated.
#' @return A character vector with the parse status of each URL:
#'   - "ok" for http(s) URLs.
#'   - "ok-ftp" for ftp and ftps URLs.
#'   - "error" for unsupported schemes (mailto, file, etc.) or invalid URLs.
#' @export
#' @examples
#' get_parse_status(c("http://example.com", "ftp://example.com", "mailto:user@example.com"))
#' get_parse_status(c("http://example.com", "not-a-url"))
get_parse_status <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u, protocol_handling)
    if (is.null(parsed)) return("error")

    scheme <- tolower(parsed$scheme %||% "")

    host <- parsed$host %||% ""

    if (!is.na(scheme) && scheme %in% c("ftp", "ftps")) return("ok-ftp")

    if (!is.na(scheme) && scheme %in% c("http", "https")) {
      if (!grepl("\\.", host)) return("warning-no-tld")
      return("ok")
    }

    return("error")

  }, character(1))
}

#' Get cleaned URLs
#'
#' This function returns the cleaned version of the URLs by ensuring that the
#' URLs are valid and, if necessary, prepends "http://" when the protocol is missing.
#'
#' @param url A character vector containing URLs to be parsed. This can include URLs
#'            without a scheme (e.g., "example.com") or URLs with a scheme
#'            (e.g., "http://example.com").
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          Can be one of "keep", "none", "strip", "http", "https".
#'                          The protocol is preserved if it exists, and "http://" is
#'                          added if missing. If "none", no protocol is added. If
#'                          "http://" or "https://" the given protocol is added
#'                          or changed to the one indicated.
#' @return A character vector of cleaned URLs.
#' @export
#' @examples
#' get_clean_url("example.com")
#' get_clean_url("http://example.com")
#' get_clean_url("https://example.com")
#' get_clean_url("ftp://example.com")
get_clean_url <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u, protocol_handling)
    # NOTE: This block is unreachable in practice due to curl_parse_url always
    # returning valid host/path on successful parse. Kept for safety.
    if (
      is.null(parsed) ||
      is.null(parsed$host) || parsed$host == "" ||
      is.null(parsed$path) || parsed$path == ""
    ) {
      return(NA_character_)
    }
    if (!is.null(parsed$scheme)) {
      paste0(parsed$scheme, "://", parsed$host, parsed$path)
    } else {
      # Defensive fallback: scheme is NULL but host/path exist.
      # curl::curl_parse_url() rarely produces this; may be unreachable.
      # nocov start
      paste0(parsed$host, parsed$path)
      # nocov end
    }
  }, character(1))
}

# Internal helper to extract the registered domain from a hostname
# Uses the public suffix list to remove known suffixes and return the main domain.
#' @importFrom utils tail
.get_registered_domain <- function(hostname) {
  parts <- strsplit(hostname, "\\.")[[1]]
  n <- length(parts)
  if (n < 2) return(NA_character_)

  # Extract rule types from PSL
  exception_rules <- sub("^!", "", grep("^!", psl_clean, value = TRUE))
  wildcard_rules  <- sub("^\\*\\.", "", grep("^\\*\\.", psl_clean, value = TRUE))
  normal_rules    <- setdiff(psl_clean, c(paste0("!", exception_rules), paste0("*.", wildcard_rules)))

  # 1. Exception rules (take precedence)
  for (i in seq_len(n)) {
    candidate <- paste(parts[i:n], collapse = ".")
    exception_rule <- paste0("!", candidate)

    if (exception_rule %in% psl_clean) {
      # Exception match: treat the exception domain as *not* a suffix
      # So return one label above it
      return(candidate)
    }
  }

  # 2. Track best match length
  best_match_len <- 0

  for (i in seq_len(n)) {
    candidate <- paste(parts[i:n], collapse = ".")

    if (candidate %in% normal_rules && n - i + 1 > best_match_len) {
      best_match_len <- n - i + 1
    }

    # Wildcard match: candidate must match a known *.domain suffix
    if (i < n) {
      wildcard_candidate <- paste(parts[i:n], collapse = ".")
      if (wildcard_candidate %in% wildcard_rules && n - i > best_match_len) {
        best_match_len <- n - i
      }
    }
  }

  if (best_match_len == 0) {
    return(paste(tail(parts, 2), collapse = "."))
  }

  if (n <= best_match_len) return(NA_character_)

  # Return: label before suffix + suffix itself
  return(paste(parts[(n - best_match_len):n], collapse = "."))
}

#' Get domain names
#'
#' This function extracts the domain name from a given URL. It returns only the
#' domain part of the URL (e.g., "example.com" from "http://example.com").
#' #' Note the domain is determined based on Public Suffix List at
#' https://publicsuffix.org/list/public_suffix_list.dat Which may not give intuitive results
#' sometimes. For example, blogspot.com is treated as a TLD but wordpress.org is not.
#'
#' For example:
#'
#' get_domain("https://test.wordpress.org")
#' https://test.wordpress.org
#' "wordpress.org"
#'
#' But:
#'
#' get_domain("https://test.blogspot.com")
#' https://test.blogspot.com
#' "test.blogspot.com"
#'
#' Deciding what is a "proper" domain name is an ambitious yet futile task.
#' I gave up and decided to use something that already exists and is respected.
#'
#' @param url A character vector containing URLs from which to extract the domain.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          Can be one of "keep", "none", "strip", "http", "https".
#'                          The protocol is preserved if it exists, and "http://" is
#'                          added if missing. If "none", no protocol is added. If
#'                          "http://" or "https://" the given protocol is added
#'                          or changed to the one indicated.
#' @return A character vector with domain names extracted from the given URLs.
#' @export
#' @examples
#' get_domain("http://example.com/path")
#' get_domain("https://sub.domain.org/")
#' get_domain("ftp://ftp.example.com")
get_domain <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u, protocol_handling)
    if (is.null(parsed) || is.null(parsed$host)) return(NA_character_)
    .get_registered_domain(parsed$host)
  }, character(1))
}

#' Get URL schemes
#'
#' This function extracts the scheme (protocol) of a given URL. It returns the
#' scheme (e.g., "http", "https", "ftp", etc.) of the URL.
#'
#' @param url A character vector containing URLs from which to extract the scheme.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          Can be one of "keep", "none", "strip", "http", "https".
#'                          The protocol is preserved if it exists, and "http://" is
#'                          added if missing. If "none", no protocol is added. If
#'                          "http://" or "https://" the given protocol is added
#'                          or changed to the one indicated.
#' @return A character vector with the scheme (e.g., "http", "https", "ftp") of each URL.
#' @export
#' @examples
#' get_scheme("http://example.com")
#' get_scheme("ftp://example.com")
#' get_scheme("https://example.com")
get_scheme <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u, protocol_handling)
    if (is.null(parsed)) return(NA_character_)
    parsed$scheme %||% NA_character_
  }, character(1))
}

#' Get URL hosts
#'
#' This function extracts the host (domain) of a given URL. It returns the host
#' name (e.g., "example.com") of the URL.
#'
#' @param url A character vector containing URLs from which to extract the host.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          Can be one of "keep", "none", "strip", "http", "https".
#'                          The protocol is preserved if it exists, and "http://" is
#'                          added if missing. If "none", no protocol is added. If
#'                          "http://" or "https://" the given protocol is added
#'                          or changed to the one indicated.
#' @return A character vector with the host of each URL.
#' In layman's terms, the host
#' being the part of the address "between the protocol and first slash / end
#' of the string if no slash is present, e.g., test.wordpress.org, www.r-project.org.
#' Note the host and the domain may be the same thing but for different reasons.
#' @export
#' @examples
#' get_host("http://example.com")
#' get_host("ftp://example.com")
#' get_host("https://sub.domain.com")
get_host <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u, protocol_handling)
    if (is.null(parsed)) return(NA_character_)
    parsed$host %||% NA_character_
  }, character(1))
}

#' Get URL paths
#'
#' This function extracts the path component of a given URL. The path refers to the
#' part of the URL that follows the domain, such as "/path/to/resource".
#'
#' @param url A character vector containing URLs from which to extract the path.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          Can be one of "keep", "none", "strip", "http", "https".
#'                          The protocol is preserved if it exists, and "http://" is
#'                          added if missing. If "none", no protocol is added. If
#'                          "http://" or "https://" the given protocol is added
#'                          or changed to the one indicated.
#' @return A character vector with the path of each URL. If no path exists, it will
#'         return an empty string.
#' @export
#' @examples
#' get_path("http://example.com/path/to/resource")
#' get_path("ftp://example.com/another/path")
#' get_path("https://example.com")
get_path <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u, protocol_handling)
    if (is.null(parsed)) return(NA_character_)
    parsed$path %||% NA_character_
  }, character(1))
}
