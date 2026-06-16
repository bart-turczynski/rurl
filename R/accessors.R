# Accessors: public get_*() functions and the shared
# .extract_from_urls() helper.

# Shared extraction path for the get_* accessors.
#
# Parses each URL once via the memoized safe_parse_url() and pulls a single
# field out of the result, collapsing the identical vapply/null-guard loop
# that every simple accessor used to carry. The parse-option arguments mirror
# safe_parse_url()'s own defaults so callers override only what differs.
#
# - field: name of the result element to return, or NULL to hand the whole
#   parsed list to `transform` (used by the multi-field accessors).
# - null_value: returned when the URL fails to parse (and used as the default
#   when the field itself is absent).
# - fun_value: the vapply() template controlling the output type.
# - transform: applied to the extracted value (or parsed list when field=NULL).
.extract_from_urls <- function(url,
                               field,
                               null_value = NA_character_,
                               fun_value = character(1),
                               transform = identity,
                               protocol_handling = "keep",
                               www_handling = "none",
                               tld_source = "all",
                               case_handling = "lower_host",
                               trailing_slash_handling = "none",
                               index_page_handling = "keep",
                               path_normalization = "none",
                               scheme_relative_handling = "keep",
                               subdomain_levels_to_keep = NULL,
                               host_encoding = "keep",
                               path_encoding = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
      protocol_handling = protocol_handling,
      www_handling = www_handling,
      tld_source = tld_source,
      case_handling = case_handling,
      trailing_slash_handling = trailing_slash_handling,
      index_page_handling = index_page_handling,
      path_normalization = path_normalization,
      scheme_relative_handling = scheme_relative_handling,
      subdomain_levels_to_keep = subdomain_levels_to_keep,
      host_encoding = host_encoding,
      path_encoding = path_encoding
    )
    if (is.null(parsed)) {
      return(null_value)
    }
    if (is.null(field)) {
      transform(parsed)
    } else {
      transform(parsed[[field]] %||% null_value)
    }
  }, fun_value)
}

#' Get the parse status of URLs
#'
#' @param url A character vector of URLs to be parsed.
#' @inheritParams safe_parse_url
#' @return A character vector with the parse status of each URL.
#' @export
#' @examples
#' get_parse_status(
#'   c("http://example.com", "ftp://example.com", "mailto:user@example.com")
#' )
#' get_parse_status(c("http://example.com", "not-a-url"))
get_parse_status <- function(url,
                             protocol_handling = "keep",
                             www_handling = "none",
                             subdomain_levels_to_keep = NULL) {
  .extract_from_urls(url, "parse_status",
    null_value = "error",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    case_handling = "lower",
    subdomain_levels_to_keep = subdomain_levels_to_keep
  )
}

#' Get cleaned URLs
#'
#' This function returns the cleaned version of the URLs after applying
#' protocol, www, case, and trailing slash handling rules. The result is a
#' normalized canonical key composed of scheme, host, and path only; port,
#' query, fragment, and userinfo are intentionally excluded (use
#' \code{\link{get_port}}, \code{\link{get_query}}, \code{\link{get_fragment}},
#' or \code{\link{get_userinfo}} for those).
#'
#' @param url A character vector containing URLs to be parsed.
#' @inheritParams safe_parse_url
#' @return A character vector of cleaned URLs.
#' @export
#' @examples
#' get_clean_url("Example.COM/Path") # Default lower_host: host folds, path kept
#' get_clean_url(
#'   "Example.COM/Path",
#'   case_handling = "keep",
#'   trailing_slash_handling = "keep"
#' )
#' get_clean_url(
#'   "Example.COM/Path/",
#'   case_handling = "upper",
#'   trailing_slash_handling = "strip"
#' )
#' get_clean_url("http://example.com", www_handling = "strip")
#' get_clean_url(
#'   "http://deep.sub.domain.example.com/path",
#'   subdomain_levels_to_keep = 0
#' )
#' # -> "http://example.com/path"
#' get_clean_url(
#'   "http://www.deep.sub.domain.example.com/path",
#'   subdomain_levels_to_keep = 1,
#'   www_handling = "strip"
#' )
#' # -> "http://domain.example.com/path"
#' get_clean_url(
#'   "http://www.deep.sub.domain.example.com/path",
#'   subdomain_levels_to_keep = 1,
#'   www_handling = "keep"
#' )
#' # -> "http://www.domain.example.com/path"
get_clean_url <- function(url,
                          protocol_handling = "keep",
                          www_handling = "none",
                          case_handling = "lower_host",
                          trailing_slash_handling = "none",
                          index_page_handling = "keep",
                          path_normalization = "none",
                          scheme_relative_handling = "keep",
                          subdomain_levels_to_keep = NULL,
                          host_encoding = "keep",
                          path_encoding = "keep") {
  .extract_from_urls(url, "clean_url",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    case_handling = case_handling,
    trailing_slash_handling = trailing_slash_handling,
    index_page_handling = index_page_handling,
    path_normalization = path_normalization,
    scheme_relative_handling = scheme_relative_handling,
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding,
    path_encoding = path_encoding
  )
}

#' Get domain names
#'
#' Extracts the registered domain name from a URL (e.g., "example.com").
#' Relies on the Public Suffix List.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param source Which PSL source to use: "all", "private", or "icann".
#' @return A character vector of domain names.
#' @export
#' @examples
#' get_domain("http://www.example.co.uk/path")
get_domain <- function(url,
                       protocol_handling = "keep",
                       www_handling = "none",
                       subdomain_levels_to_keep = NULL,
                       source = c("all", "private", "icann")) {
  source <- match.arg(source)
  # parsed$domain is the registered domain for the requested section (pslr
  # resolves it consistently with the TLD), so every source reads one field.
  .extract_from_urls(url, "domain",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
    case_handling = "lower",
    subdomain_levels_to_keep = subdomain_levels_to_keep
  )
}

#' Get URL schemes
#'
#' Extracts the scheme (protocol) of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of URL schemes.
#' @export
#' @examples
#' get_scheme("https://example.com")
get_scheme <- function(url, protocol_handling = "keep") {
  # Scheme is unaffected by www/subdomain handling.
  .extract_from_urls(url, "scheme",
    protocol_handling = protocol_handling,
    case_handling = "lower"
  )
}

#' Get URL hosts
#'
#' Extracts the host component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param case_handling How to handle casing of the returned host. Defaults to
#' "lower".
#' @return A character vector of URL hosts.
#' @export
#' @examples
#' get_host("http://sub.example.com:8080")
#' get_host(
#'   "http://www.two.one.example.com",
#'   subdomain_levels_to_keep = 1
#' ) # Result: "www.one.example.com"
#' get_host(
#'   "http://www.two.one.example.com",
#'   www_handling = "strip",
#'   subdomain_levels_to_keep = 1
#' ) # Result: "one.example.com"
#' get_host(
#'   "http://www.two.one.example.com",
#'   www_handling = "keep",
#'   subdomain_levels_to_keep = 1
#' ) # Result: "www.one.example.com"
#' get_host(
#'   "http://three.two.one.example.com",
#'   subdomain_levels_to_keep = 0
#' ) # Result: "example.com"
#' get_host(
#'   "http://www.three.two.one.example.com",
#'   subdomain_levels_to_keep = 0
#' ) # Result: "www.example.com"
get_host <- function(url,
                     protocol_handling = "keep",
                     www_handling = "none",
                     subdomain_levels_to_keep = NULL,
                     case_handling = c(
                       "lower", "keep", "upper", "lower_host"
                     )) {
  case_handling <- match.arg(case_handling)
  .extract_from_urls(url, "host",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    case_handling = case_handling,
    subdomain_levels_to_keep = subdomain_levels_to_keep
  )
}

#' Get URL paths
#'
#' Extracts the path component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param case_handling How to handle casing of the returned path. Defaults to
#' "lower_host", which preserves the path's original casing (paths are
#' case-sensitive per RFC 3986 §6.2.2.1). Use "lower"/"upper" to force a case.
#' @return A character vector of URL paths.
#' @export
#' @examples
#' get_path("http://example.com/some/path?query=1")
get_path <- function(
  url,
  protocol_handling = "keep",
  case_handling = c("lower_host", "keep", "lower", "upper")
) {
  case_handling <- match.arg(case_handling)
  # Path is unaffected by www/subdomain handling.
  .extract_from_urls(url, "path",
    protocol_handling = protocol_handling,
    case_handling = case_handling
  )
}

#' Get URL query strings
#'
#' Extracts the query component of a URL, optionally parsing it into a list.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param format Return format: "string" (default) or "list" for parsed
#' elements.
#' @param decode Logical; if TRUE and format="list", percent-decodes
#' keys/values.
#' @return A character vector (format="string") or list (format="list").
#' @export
#' @examples
#' get_query("http://example.com/path?a=1&b=2")
#' get_query("http://example.com/path?a=1&b=2", format = "list")
get_query <- function(url,
                      protocol_handling = "keep",
                      format = c("string", "list"),
                      decode = TRUE) {
  format <- match.arg(format)

  if (format == "string") {
    return(.extract_from_urls(url, "query",
      protocol_handling = protocol_handling
    ))
  }

  lapply(url, function(u) {
    parsed <- safe_parse_url(u,
      protocol_handling = protocol_handling,
      www_handling = "none",
      tld_source = "all",
      case_handling = "keep",
      trailing_slash_handling = "none",
      subdomain_levels_to_keep = NULL
    )
    if (is.null(parsed)) {
      return(list())
    }
    ._parse_query_string(parsed$query %||% NA_character_, decode = decode)
  })
}

#' Get URL fragments
#'
#' Extracts the fragment component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of fragments.
#' @export
#' @examples
#' get_fragment("http://example.com/path#section")
get_fragment <- function(url, protocol_handling = "keep") {
  .extract_from_urls(url, "fragment", protocol_handling = protocol_handling)
}

#' Get URL ports
#'
#' Extracts the port component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return An integer vector of ports.
#' @export
#' @examples
#' get_port("http://example.com:8080/path")
get_port <- function(url, protocol_handling = "keep") {
  .extract_from_urls(url, "port",
    null_value = NA_integer_,
    fun_value = integer(1),
    transform = as.integer,
    protocol_handling = protocol_handling
  )
}

#' Get URL user names
#'
#' Extracts the user component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of user names.
#' @export
get_user <- function(url, protocol_handling = "keep") {
  .extract_from_urls(url, "user", protocol_handling = protocol_handling)
}

#' Get URL passwords
#'
#' Extracts the password component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of passwords.
#' @export
get_password <- function(url, protocol_handling = "keep") {
  .extract_from_urls(url, "password", protocol_handling = protocol_handling)
}

#' Get URL userinfo
#'
#' Extracts the userinfo component of a URL (user or user:password).
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of userinfo values.
#' @export
get_userinfo <- function(url, protocol_handling = "keep") {
  .extract_from_urls(url, NULL,
    transform = function(parsed) {
      user <- parsed$user %||% NA_character_
      password <- parsed$password %||% NA_character_
      if (is.na(user) || !nzchar(user)) {
        return(NA_character_)
      }
      if (is.na(password) || !nzchar(password)) {
        return(user)
      }
      paste0(user, ":", password)
    },
    protocol_handling = protocol_handling
  )
}

#' Get URL subdomains
#'
#' Extracts the subdomain component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param source Which PSL source to use: "all", "private", or "icann".
#' @param include_www Logical; if FALSE (default), removes a leading
#'   www/www[0-9]* label only when it is the sole subdomain label.
#' @param format Return format: "string" (default) or "labels" for a character
#' vector of labels.
#' @return A character vector (format="string") or list of label vectors
#' (format="labels").
#' @export
#' @examples
#' get_subdomain("http://www.blog.example.co.uk")
#' get_subdomain("http://www.blog.example.co.uk", format = "labels")
get_subdomain <- function(url,
                          protocol_handling = "keep",
                          www_handling = "none",
                          source = c("all", "private", "icann"),
                          include_www = FALSE,
                          format = c("string", "labels")) {
  source <- match.arg(source)
  format <- match.arg(format)

  results <- lapply(url, function(u) {
    parsed <- safe_parse_url(u,
      protocol_handling = protocol_handling,
      www_handling = www_handling,
      tld_source = source,
      case_handling = "lower",
      trailing_slash_handling = "none",
      subdomain_levels_to_keep = NULL
    )
    if (is.null(parsed) || isTRUE(parsed$is_ip_host)) {
      return(character(0))
    }

    host_unicode <- .punycode_to_unicode(parsed$host %||% NA_character_)
    if (is.na(host_unicode) || !nzchar(host_unicode)) {
      return(character(0))
    }

    # parsed$domain already reflects the requested section (see get_domain()).
    domain_val <- parsed$domain %||% NA_character_

    if (is.na(domain_val) || !nzchar(domain_val)) {
      return(character(0))
    }

    host_lc <- stringi::stri_trans_tolower(host_unicode)
    domain_lc <- stringi::stri_trans_tolower(domain_val)
    suffix <- paste0(".", domain_lc)
    if (!stringi::stri_endswith_fixed(host_lc, suffix)) {
      return(character(0))
    }

    sub_part <- stringi::stri_sub(
      host_lc,
      1,
      stringi::stri_length(host_lc) - stringi::stri_length(suffix)
    )
    if (!nzchar(sub_part)) {
      return(character(0))
    }

    labels <- strsplit(sub_part, "\\.")[[1]]
    drop_www_label <- !include_www &&
      length(labels) == 1 &&
      grepl("^www[0-9]*$", labels[1])
    if (drop_www_label) {
      labels <- labels[-1]
    }
    labels
  })

  if (format == "labels") {
    return(results)
  }

  vapply(results, function(labels) {
    if (length(labels) == 0) {
      return(NA_character_)
    }
    paste(labels, collapse = ".")
  }, character(1))
}

#' Extract the top-level domain (TLD) from a URL
#'
#' Uses safe_parse_url internally to extract the TLD, benefiting from
#' all memoization layers for improved performance.
#'
#' @param url A character vector of URLs.
#' @param source Which TLD source to use: "all", "icann", or "private".
#' @return A character vector of TLDs.
#' @export
#' @examples
#' get_tld("example.com")
get_tld <- function(url, source = c("all", "private", "icann")) {
  source <- match.arg(source)
  .extract_from_urls(url, "tld",
    tld_source = source,
    case_handling = "lower"
  )
}
