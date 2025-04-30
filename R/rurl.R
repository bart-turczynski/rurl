#' Parse a URL safely with scheme handling
#'
#' @param url A character string
#' @param protocol_handling One of "keep", "none", "strip", "http", or "https"
#' @return A named list or NULL on error
#' @keywords internal
safe_parse_url <- function(url, protocol_handling = c("keep", "none", "strip", "http", "https")) {
  protocol_handling <- match.arg(protocol_handling)
  if (is.na(url) || !is.character(url) || url == "") return(NULL)

  has_protocol <- grepl("^https?://", url, ignore.case = TRUE)

  parse_url <- switch(protocol_handling,
                      keep  = if (!has_protocol) paste0("http://", url) else url,
                      none  = if (!has_protocol) paste0("http://", url) else url,
                      strip = paste0("http://", sub("^https?://", "", url, ignore.case = TRUE)),
                      http  = paste0("http://", sub("^https?://", "", url, ignore.case = TRUE)),
                      https = paste0("https://", sub("^https?://", "", url, ignore.case = TRUE))
  )

  result <- tryCatch(curl::curl_parse_url(parse_url), error = function(e) NULL)
  if (is.null(result)) return(NULL)

  result$scheme <- switch(protocol_handling,
                          none  = if (!has_protocol) NA_character_ else result$scheme,
                          strip = NA_character_,
                          http  = "http",
                          https = "https",
                          keep  = result$scheme
  )

  result
}

#' Get the parse status of a URL
#' @param url URL string
#' @param protocol_handling Protocol handling mode
#' @return "ok" or "error"
#' @export
get_parse_status <- function(url, protocol_handling = "keep") {
  parsed <- safe_parse_url(url, protocol_handling)
  if (is.null(parsed)) "error" else "ok"
}

#' Get the cleaned URL (scheme://host/path)
#' @param url URL string
#' @param protocol_handling Protocol handling mode
#' @return A cleaned URL or NA
#' @export
get_clean_url <- function(url, protocol_handling = "keep") {
  parsed <- safe_parse_url(url, protocol_handling)
  if (is.null(parsed) || is.null(parsed$host) || is.null(parsed$path)) return(NA_character_)
  if (!is.null(parsed$scheme)) {
    paste0(parsed$scheme, "://", parsed$host, parsed$path)
  } else {
    paste0(parsed$host, parsed$path)
  }
}

#' Get the domain (e.g., example.co.uk)
#' @param url URL string
#' @param protocol_handling Protocol handling mode
#' @return Apex domain or NA
#' @export
get_domain <- function(url, protocol_handling = "keep") {
  parsed <- safe_parse_url(url, protocol_handling)
  if (is.null(parsed) || is.null(parsed$host)) return(NA_character_)
  psl::apex_domain(parsed$host)
}

#' Get the scheme of the URL
#' @param url URL string
#' @param protocol_handling Protocol handling mode
#' @return Scheme or NA
#' @export
get_scheme <- function(url, protocol_handling = "keep") {
  parsed <- safe_parse_url(url, protocol_handling)
  if (is.null(parsed)) return(NA_character_)
  parsed$scheme %||% NA_character_
}

#' Get the host of the URL
#' @param url URL string
#' @param protocol_handling Protocol handling mode
#' @return Host or NA
#' @export
get_host <- function(url, protocol_handling = "keep") {
  parsed <- safe_parse_url(url, protocol_handling)
  if (is.null(parsed)) return(NA_character_)
  parsed$host %||% NA_character_
}

#' Get the path of the URL
#' @param url URL string
#' @param protocol_handling Protocol handling mode
#' @return Path or NA
#' @export
get_path <- function(url, protocol_handling = "keep") {
  parsed <- safe_parse_url(url, protocol_handling)
  if (is.null(parsed)) return(NA_character_)
  parsed$path %||% NA_character_
}

# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y
