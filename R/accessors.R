# Accessors: public get_*() functions and the shared
# .extract_from_urls() helper.

# Shared extraction path for the get_* accessors.
#
# Validates `url` once, parses the whole vector in a single pass through the
# cached vector engine (`._parse_urls_cached()`, the same unique+match+memoized
# path `safe_parse_urls()` uses), then pulls one column out. This replaces the
# old per-element `vapply(url, safe_parse_url, ...)` loop, which re-ran option
# validation for every element and returned a vector NAMED by the input URLs.
# Results are now UNNAMED (a deliberate behavior change; see NEWS).
#
# The parse-option arguments mirror safe_parse_url()'s own defaults so callers
# override only what differs.
#
# - field: name of the result column to return, or NULL to hand the whole
#   column list to `transform` (used by the multi-column accessors, which
#   combine several columns vectorized and are responsible for their own
#   null-row handling).
# - null_value: written into the rows that failed to parse (the engine already
#   fills those with each field's default; this pins the accessor's contract
#   exactly regardless of the column default).
# - fun_value: retained for the helper's documented signature; the output type
#   now follows `transform`/the extracted column rather than a vapply template.
# - transform: applied (vectorized) to the extracted column, or to the whole
#   column list when field=NULL.
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
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  # Validate + normalize the option profile once (match.arg + subdomain check),
  # then parse the entire vector through the shared cached engine in one call.
  opts <- .parse_options(
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
  cols <- ._parse_urls_cached(url, opts)

  if (is.null(field)) {
    # Multi-column accessors receive the column list and return a full vector.
    return(unname(transform(cols)))
  }

  # Single-column accessors: apply the (vectorized) transform, then pin the
  # null rows to null_value so the accessor's absent/unparseable contract holds
  # exactly even if a column default ever diverged from it.
  result <- transform(cols[[field]])
  result[attr(cols, "null_row")] <- null_value
  unname(result)
}

#' Get the parse status of URLs
#'
#' @param url A character vector of URLs to be parsed.
#' @inheritParams safe_parse_url
#' @param source Which PSL source to use: "all", "private", or "icann".
#'   Warning statuses such as \code{warning-no-tld}, \code{warning-invalid-tld},
#'   and \code{warning-public-suffix} depend on which PSL section is consulted,
#'   so pass \code{source = "icann"} to use only ICANN-managed TLDs.
#' @return A character vector with the parse status of each URL.
#' @export
#' @examples
#' get_parse_status(
#'   c("http://example.com", "ftp://example.com", "mailto:user@example.com")
#' )
#' get_parse_status(c("http://example.com", "not-a-url"))
#' get_parse_status("http://example.com", source = "icann")
get_parse_status <- function(url,
                             protocol_handling = "keep",
                             www_handling = "none",
                             subdomain_levels_to_keep = NULL,
                             source = c("all", "private", "icann")) {
  source <- match.arg(source)
  # case_handling does not affect the parse_status output (it is derived from
  # curl success, host, domain and TLD, none of which depend on the clean_url
  # case policy). "lower" is kept here purely as an explicit, stable profile;
  # it is intentionally NOT aligned to .extract_from_urls()'s "lower_host"
  # default, so this accessor keeps its own memoization key rather than risk a
  # cache-key/output perturbation for a micro-optimization (RURL-actrnerd).
  .extract_from_urls(url, "parse_status",
    null_value = "error",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
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
#' @param source Which PSL source to use: "all", "private", or "icann".
#'   Subdomain trimming depends on which section is consulted, so pass
#'   \code{source = "icann"} to exclude private suffixes (e.g. github.io).
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
                          source = c("all", "private", "icann"),
                          case_handling = "lower_host",
                          trailing_slash_handling = "none",
                          index_page_handling = "keep",
                          path_normalization = "none",
                          scheme_relative_handling = "keep",
                          subdomain_levels_to_keep = NULL,
                          host_encoding = "keep",
                          path_encoding = "keep") {
  source <- match.arg(source)
  .extract_from_urls(url, "clean_url",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
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
                       source = c("all", "private", "icann"),
                       host_encoding = c("keep", "idna", "unicode")) {
  source <- match.arg(source)
  host_encoding <- match.arg(host_encoding)
  # parsed$domain is the registered domain for the requested section (pslr
  # resolves it consistently with the TLD), so every source reads one field.
  # case_handling is immaterial to the domain output (the registered domain is
  # derived from the normalized host, independent of the clean_url case policy);
  # "lower" is retained as an explicit, stable profile rather than aligned to
  # the "lower_host" default to avoid a cache-key/output change (RURL-actrnerd).
  .extract_from_urls(url, "domain",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
    case_handling = "lower",
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding
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
get_scheme <- function(url, protocol_handling = "keep",
                       scheme_relative_handling = "keep") {
  # Scheme is unaffected by www/subdomain handling.
  .extract_from_urls(url, "scheme",
    protocol_handling = protocol_handling,
    scheme_relative_handling = scheme_relative_handling,
    case_handling = "lower"
  )
}

#' Get URL hosts
#'
#' Extracts the host component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param source Which PSL source to use: "all", "private", or "icann".
#'   Subdomain trimming depends on which section is consulted, so pass
#'   \code{source = "icann"} to exclude private suffixes (e.g. github.io).
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
                     source = c("all", "private", "icann"),
                     subdomain_levels_to_keep = NULL,
                     case_handling = c(
                       "lower", "keep", "upper", "lower_host"
                     ),
                     host_encoding = c("keep", "idna", "unicode")) {
  source <- match.arg(source)
  case_handling <- match.arg(case_handling)
  host_encoding <- match.arg(host_encoding)
  .extract_from_urls(url, "host",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
    case_handling = case_handling,
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding
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
  case_handling = c("lower_host", "keep", "lower", "upper"),
  trailing_slash_handling = c("none", "keep", "strip"),
  index_page_handling = c("keep", "strip"),
  path_normalization = c("none", "collapse_slashes", "dot_segments", "both"),
  path_encoding = c("keep", "encode", "decode")
) {
  case_handling <- match.arg(case_handling)
  trailing_slash_handling <- match.arg(trailing_slash_handling)
  index_page_handling <- match.arg(index_page_handling)
  path_normalization <- match.arg(path_normalization)
  path_encoding <- match.arg(path_encoding)
  # Path is unaffected by www/subdomain handling.
  .extract_from_urls(url, "path",
    protocol_handling = protocol_handling,
    case_handling = case_handling,
    trailing_slash_handling = trailing_slash_handling,
    index_page_handling = index_page_handling,
    path_normalization = path_normalization,
    path_encoding = path_encoding
  )
}

#' Get URL query strings
#'
#' Extracts the query component of a URL, optionally parsing it into a list.
#'
#' The underlying parse preserves the raw query string byte-for-byte (a bare
#' key such as `?flag` stays `flag`, not `flag=`). By default this accessor
#' still percent-decodes for readability (`decode = TRUE`); pass
#' `decode = FALSE` to obtain the raw query exactly as written in the URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param format Return format: "string" (default) or "list" for parsed
#' elements.
#' @param decode Logical; if TRUE (default), percent-decodes the query
#' (the whole string for format="string", keys/values for format="list").
#' Set FALSE to return the raw query as written in the URL.
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

  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  if (format == "string") {
    raw <- .extract_from_urls(url, "query",
      protocol_handling = protocol_handling
    )
    # The parse result now carries a RAW (percent-encoded) query so the list
    # form can split safely; the string form historically returns the decoded
    # query, so decode here to keep that output unchanged.
    if (decode) {
      # Per-element decode keeps the historical byte-for-byte fallback (a query
      # that fails to unescape is returned raw); USE.NAMES = FALSE so the result
      # stays unnamed like every other accessor.
      raw <- vapply(
        raw,
        function(q) {
          if (is.na(q)) {
            NA_character_
          } else {
            tryCatch(curl::curl_unescape(q), error = function(e) q)
          }
        },
        character(1),
        USE.NAMES = FALSE
      )
    }
    return(raw)
  }

  # format = "list": pull the raw query column in one engine pass (case = "keep"
  # matches the historical per-element profile so cache keys are unchanged),
  # then parse each element. Null rows carry NA, and ._parse_query_string(NA)
  # returns list() -- exactly what the old per-element NULL branch returned.
  raw <- .extract_from_urls(url, "query",
    protocol_handling = protocol_handling,
    case_handling = "keep"
  )
  lapply(raw, ._parse_query_string, decode = decode)
}

#' Get URL fragments
#'
#' Extracts the fragment component of a URL. The value is returned raw, exactly
#' as written in the URL (not percent-decoded).
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
#' Extracts the user component of a URL. The value is returned raw, exactly as
#' written in the URL (not percent-decoded).
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of user names.
#' @export
#' @examples
#' get_user("ftp://alice:secret@ftp.example.com/file.txt")
get_user <- function(url, protocol_handling = "keep") {
  .extract_from_urls(url, "user", protocol_handling = protocol_handling)
}

#' Get URL passwords
#'
#' Extracts the password component of a URL. The value is returned raw, exactly
#' as written in the URL (not percent-decoded).
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of passwords.
#' @export
#' @examples
#' get_password("ftp://alice:secret@ftp.example.com/file.txt")
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
#' @examples
#' get_userinfo("ftp://alice:secret@ftp.example.com/file.txt")
#' get_userinfo("ftp://alice@ftp.example.com/file.txt")
get_userinfo <- function(url, protocol_handling = "keep") {
  .extract_from_urls(url, NULL,
    transform = function(cols) {
      # Vectorized user[:password]: NA when there is no user (covers null rows,
      # whose user column is NA), user alone when the password is absent/empty,
      # and user:password otherwise -- matching the old scalar branch row-wise.
      user <- cols$user
      password <- cols$password
      has_user <- !is.na(user) & nzchar(user)
      has_password <- !is.na(password) & nzchar(password)
      out <- rep(NA_character_, length(user))
      out[has_user] <- user[has_user]
      combine <- has_user & has_password
      out[combine] <- paste0(user[combine], ":", password[combine])
      out
    },
    protocol_handling = protocol_handling
  )
}

# Derive the subdomain labels for a vector of parsed rows, from the engine's
# host + domain + is_ip_host columns. For each row: lowercase host and domain,
# strip the ".<domain>" suffix, split the remaining prefix into labels, and
# (unless include_www) drop a lone leading www/www[0-9]* label. A row yields
# character(0) when there is no subdomain -- IP host, empty/NA host or domain
# (which includes null/unparseable rows, whose columns are NA), suffix
# mismatch, or an empty prefix. host and domain honor host_encoding upstream,
# so they share one spelling and the suffix strip matches directly (no forced
# Unicode decode); domain already reflects the requested section.
.subdomain_labels_vec <- function(host, domain, is_ip_host, include_www) {
  n <- length(host)
  result <- rep(list(character(0)), n)

  host_l <- stringi::stri_trans_tolower(host)
  domain_l <- stringi::stri_trans_tolower(domain)
  # Candidate rows: real host + domain and not an IP host. is_ip_host is TRUE
  # only for genuine IPs; FALSE or NA (null rows) is treated as non-IP, and the
  # host/domain NA checks then exclude the null rows anyway.
  not_ip <- is.na(is_ip_host) | !is_ip_host
  usable <- not_ip &
    !is.na(host_l) & nzchar(host_l) &
    !is.na(domain_l) & nzchar(domain_l)
  if (!any(usable)) {
    return(result)
  }

  suffix <- character(n)
  suffix[usable] <- paste0(".", domain_l[usable])
  ends <- logical(n)
  ends[usable] <- stringi::stri_endswith_fixed(host_l[usable], suffix[usable])

  take <- which(ends)
  sub_part <- stringi::stri_sub(
    host_l[take],
    1,
    stringi::stri_length(host_l[take]) - stringi::stri_length(suffix[take])
  )
  have_sub <- take[nzchar(sub_part)]
  if (length(have_sub) == 0L) {
    return(result)
  }

  # One list-vectorized split over the rows that actually carry a prefix.
  labels_list <- strsplit(sub_part[nzchar(sub_part)], ".", fixed = TRUE)
  if (!include_www) {
    lone_www <- vapply(
      labels_list,
      function(labels) length(labels) == 1L && grepl("^www[0-9]*$", labels[1]),
      logical(1)
    )
    labels_list[lone_www] <- lapply(labels_list[lone_www], `[`, -1L)
  }
  result[have_sub] <- labels_list
  result
}

#' Get URL subdomains
#'
#' Extracts the subdomain component of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @param source Which PSL source to use: "all", "private", or "icann".
#' @param include_www Logical; if FALSE (default), removes a leading
#'   `www`/`www[0-9]*` label only when it is the sole subdomain label.
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
                          format = c("string", "labels"),
                          host_encoding = c("keep", "idna", "unicode")) {
  source <- match.arg(source)
  format <- match.arg(format)
  host_encoding <- match.arg(host_encoding)

  # One engine pass with the deliberate subdomain profile (case = "lower" so the
  # suffix comparison is case-insensitive; host_encoding shared by host+domain),
  # then vectorized label derivation. field = NULL hands the column list to the
  # transform, which returns the per-row label list.
  results <- .extract_from_urls(url, NULL,
    transform = function(cols) {
      .subdomain_labels_vec(
        cols$host, cols$domain, cols$is_ip_host, include_www
      )
    },
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
    case_handling = "lower",
    host_encoding = host_encoding
  )

  if (format == "labels") {
    return(results)
  }

  vapply(results, function(labels) {
    if (length(labels) == 0) {
      return(NA_character_)
    }
    paste(labels, collapse = ".")
  }, character(1), USE.NAMES = FALSE)
}

#' Extract the top-level domain (TLD) from a URL
#'
#' Uses safe_parse_url internally to extract the TLD, benefiting from
#' all memoization layers for improved performance.
#'
#' @param url A character vector of URLs.
#' @param source Which TLD source to use: "all", "icann", or "private".
#' @inheritParams safe_parse_url
#' @return A character vector of TLDs.
#' @export
#' @examples
#' get_tld("example.com")
get_tld <- function(url, source = c("all", "private", "icann"),
                    host_encoding = c("keep", "idna", "unicode")) {
  source <- match.arg(source)
  host_encoding <- match.arg(host_encoding)
  # case_handling is immaterial to the tld output (the TLD is derived from the
  # normalized host, independent of the clean_url case policy); "lower" is
  # retained as an explicit, stable profile rather than aligned to the
  # "lower_host" default to avoid a cache-key/output change (RURL-actrnerd).
  .extract_from_urls(url, "tld",
    tld_source = source,
    case_handling = "lower",
    host_encoding = host_encoding
  )
}
