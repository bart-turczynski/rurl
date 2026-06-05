# Null coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y
utils::globalVariables(c(
  "psl_clean",
  "tld_icann",
  "tld_private",
  "tld_all"
))
#' Parse a URL comprehensively, extracting and deriving all relevant components.
#'
#' This function serves as the core URL processing engine. It parses a URL,
#' handles protocol and www prefix modifications, detects IP addresses,
#' and derives components like the registered domain and top-level domain (TLD).
#' Results are memoized for performance when processing large datasets.
#'
#' @param url A single URL string to be parsed. For vectors, use
#'   \code{\link{safe_parse_urls}}.
#' @param protocol_handling A character string specifying how to handle
#'   protocols. Defaults to "keep".
#'   \itemize{
#'     \item{"keep": If a scheme exists (http, https, ftp, ftps), it's used. If
#'     no scheme, "http://" is added.}
#'     \item{"none": If a scheme exists, it's used. If no scheme, then no
#'     scheme is used (scheme component will be NA).}
#'     \item{"strip": Any existing scheme is removed (scheme component will be
#'     NA).}
#'     \item{"http": The scheme is forced to be "http".}
#'     \item{"https": The scheme is forced to be "https".}
#'   }
#' @param www_handling A character string specifying how to handle "www"
#'   and "www[number]" prefixes in the host. Defaults to "none".
#'   \itemize{
#'     \item{"none": (Default) Leaves the host's www prefix (or lack thereof)
#'     untouched.}
#'     \item{"strip": Removes any "www." or "www[number]." prefix.}
#'     \item{"keep": Ensures the host starts with "www.". If it has
#'     "www[number].", it's normalized to "www.". If no www prefix, "www." is
#'     added. An empty input host remains empty.}
#'     \item{"if_no_subdomain": If the host is a bare registered domain (e.g.,
#'     "example.com"), "www." is added. If the host already has a "www." or
#'     "www[number]." prefix, it is normalized to "www." (e.g.,
#'     "www1.example.com" becomes "www.example.com"; "www1.sub.example.com"
#'     becomes "www.sub.example.com"). If a non-www subdomain exists (e.g.,
#'     "sub.example.com" or the normalized "www.sub.example.com"), the host is
#'     not further altered. An empty input host remains empty.}
#'   }
#' @param tld_source Which TLD source to use for TLD extraction: "all", "icann",
#'   or "private". Defaults to "all".
#' @param case_handling A character string specifying how to handle the case of
#'                      the cleaned URL. Defaults to "keep".
#'   \itemize{
#'     \item{"keep": (Default) Preserves casing of the reconstructed URL.}
#'     \item{"lower": Converts the cleaned URL to lowercase.}
#'     \item{"upper": Converts the cleaned URL to uppercase.}
#'     \item{"lower_host": Lowercases scheme and host only; the path keeps its
#'     original casing.}
#'   }
#' @param trailing_slash_handling A character string specifying how to handle
#'   trailing slashes in the path component of the cleaned URL. Defaults to
#'   "none".
#'   \itemize{
#'     \item{"none": (Default) No specific handling is applied. Path remains as
#'     is after initial parsing.}
#'     \item{"keep": Ensures a trailing slash. If a path exists and doesn't end
#'     with one, it's added. If path is just "/", it's kept.}
#'     \item{"strip": Removes a trailing slash if present, unless the path is
#'     solely "/".}
#'   }
#' @param index_page_handling A character string specifying how to handle
#' index/default pages. Defaults to "keep".
#'   \itemize{
#'     \item{"keep": (Default) Leave index/default page segments untouched.}
#'     \item{"strip": Remove a trailing index.* or default.* segment
#'     (case-insensitive).}
#'   }
#' @param path_normalization How to normalize path structure. Defaults to
#' "none".
#'   \itemize{
#'     \item{"none": (Default) No normalization.}
#'     \item{"collapse_slashes": Collapse duplicate slashes in the path.}
#'     \item{"dot_segments": Resolve . and .. segments per RFC 3986.}
#'     \item{"both": Apply both collapse_slashes and dot_segments.}
#'   }
#' @param scheme_relative_handling How to handle URLs starting with "//".
#' Defaults to "keep".
#'   \itemize{
#'     \item{"keep": Parse using http but return scheme as NA and set status to
#'     "ok-scheme-relative".}
#'     \item{"http": Assume http for parsing and output.}
#'     \item{"https": Assume https for parsing and output.}
#'     \item{"error": Treat scheme-relative URLs as invalid.}
#'   }
#' @param host_encoding How to present the host in `clean_url`. Defaults to
#' "keep".
#'   \itemize{
#'     \item{"keep": Leave host as parsed by curl (may preserve original case).}
#'     \item{"idna": Convert Unicode host labels to Punycode (IDNA) for the
#'     cleaned URL.}
#'     \item{"unicode": Decode Punycode labels to Unicode for the cleaned URL.}
#'   }
#' @param path_encoding How to handle percent-encoding in the path for
#' `clean_url`. Defaults to "keep".
#'   \itemize{
#'     \item{"keep": Leave the path percent-encoding untouched.}
#'     \item{"encode": Normalize by decoding first, then percent-encoding each
#'     segment (slashes preserved).}
#'     \item{"decode": Percent-decode UTF-8 sequences in the path.}
#'   }
#' @param subdomain_levels_to_keep An integer or NULL. Determines how many
#' levels of subdomains are kept,
#'   in addition to any 'www.' prefix handled by `www_handling`.
#'   \itemize{
#'     \item{`NULL`: (Default) No specific subdomain stripping is performed
#'     beyond `www_handling`.}
#'     \item{`0`: All subdomains are stripped. If `www_handling` preserved or
#'     added 'www.',
#'          it remains (e.g., 'www.sub.example.com' becomes 'www.example.com';
#'          'sub.example.com' becomes 'example.com').}
#'     \item{`N > 0`: Keeps up to N levels of subdomains, counted from
#'     right-to-left (closest to the registered domain),
#'          in addition to any 'www.' prefix. E.g., if N=1,
#'          'three.two.one.example.com' becomes 'one.example.com';
#'          'www.three.two.one.example.com' (post www_handling) becomes
#'          'www.one.example.com'.}
#'   }
#' @return A named list with the following components:
#'   \itemize{
#'     \item `original_url`: The original URL string provided.
#'     \item `scheme`: The scheme (e.g., "http", "https").
#'     \item `host`: The host (e.g., "www.example.com"). NA if the host becomes
#'     empty after processing.
#'     \item `port`: The port number.
#'     \item `path`: The path component (e.g., "/path/to/resource").
#'     \item `query`: The query string (e.g., "name=value").
#'     \item `fragment`: The fragment identifier (e.g., "section").
#'     \item `user`: The user name for authentication.
#'     \item `password`: The password for authentication.
#'     \item `domain`: The registered domain name (e.g., "example.com"). NA if
#'     host is an IP, empty, or derivation fails.
#'     \item `tld`: The top-level domain (e.g., "com"). NA if host is an IP,
#'     empty, or derivation fails.
#'     \item `is_ip_host`: Logical, TRUE if the host is an IP address.
#'     \item `clean_url`: A normalized canonical key reconstructed from
#'     scheme, host, and path only, after processing and with case handling
#'     applied. Port, query, fragment, and userinfo are intentionally
#'     excluded (use the dedicated components above to retrieve them). With
#'     `path_encoding = "decode"` the path is shown decoded, so `clean_url`
#'     is human-readable rather than guaranteed URL-safe. NA if host is
#'     empty/NA.
#'     \item `parse_status`: Character string indicating parsing outcome
#'       ("ok", "ok-ftp", "ok-scheme-relative", "error", "warning-no-tld",
#'       "warning-invalid-tld", "warning-public-suffix").
#'   }
#'   Returns `NULL` if the URL is fundamentally unparseable (e.g., NA, empty)
#'   or uses a disallowed scheme.
#' @seealso \code{\link{safe_parse_urls}}
#' @importFrom utils tail
#' @importFrom curl curl_parse_url curl_escape curl_unescape
#' @keywords internal
#' @export
#' @examples
#' safe_parse_url(
#'   "http://www.Example.com/Path?q=1#Frag",
#'   protocol_handling = "keep",
#'   case_handling = "lower"
#' )
#' safe_parse_url(
#'   "Example.com/Another",
#'   protocol_handling = "none",
#'   www_handling = "keep",
#'   case_handling = "upper",
#'   trailing_slash_handling = "keep"
#' )
#' safe_parse_url(
#'   "example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> www.example.com
#' safe_parse_url(
#'   "sub.example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> sub.example.com
#' safe_parse_url(
#'   "www1.example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> www.example.com
#' safe_parse_url(
#'   "www1.sub.example.com",
#'   www_handling = "if_no_subdomain"
#' ) # -> www.sub.example.com
#' safe_parse_url(
#'   "http://www.example.com/path/",
#'   trailing_slash_handling = "strip"
#' )
#' safe_parse_url("192.168.1.1/test")
#' safe_parse_url("ftp://user:pass@ftp.example.co.uk:21/file.txt")
#' safe_parse_url(
#'   "http://deep.sub.domain.example.com",
#'   subdomain_levels_to_keep = 0
#' )
#' safe_parse_url(
#'   "http://deep.sub.domain.example.com",
#'   subdomain_levels_to_keep = 1
#' )
#' safe_parse_url(
#'   "http://www.deep.sub.domain.example.com",
#'   www_handling = "keep",
#'   subdomain_levels_to_keep = 0
#' )
#' safe_parse_url(
#'   "http://www.deep.sub.domain.example.com",
#'   www_handling = "keep",
#'   subdomain_levels_to_keep = 1
#' )
safe_parse_url <- function(url,
                           protocol_handling = c(
                             "keep", "none", "strip", "http", "https"
                           ),
                           www_handling = c(
                             "none", "strip", "keep", "if_no_subdomain"
                           ),
                           tld_source = c("all", "private", "icann"),
                           case_handling = c(
                             "keep", "lower", "upper", "lower_host"
                           ),
                           trailing_slash_handling = c("none", "keep", "strip"),
                           index_page_handling = c("keep", "strip"),
                           path_normalization = c(
                             "none", "collapse_slashes", "dot_segments", "both"
                           ),
                           scheme_relative_handling = c(
                             "keep", "http", "https", "error"
                           ),
                           subdomain_levels_to_keep = NULL,
                           host_encoding = c("keep", "idna", "unicode"),
                           path_encoding = c("keep", "encode", "decode")) {
  # Enforce scalar input to keep behavior explicit and predictable
  if (length(url) != 1) {
    stop(
      "safe_parse_url() expects a single URL. Use safe_parse_urls() for",
      " vectors.",
      call. = FALSE
    )
  }

  # Match arguments early for cache key generation
  protocol_handling <- match.arg(protocol_handling)
  www_handling <- match.arg(www_handling)
  tld_source <- match.arg(tld_source)
  case_handling <- match.arg(case_handling)
  trailing_slash_handling <- match.arg(trailing_slash_handling)
  index_page_handling <- match.arg(index_page_handling)
  path_normalization <- match.arg(path_normalization)
  scheme_relative_handling <- match.arg(scheme_relative_handling)
  host_encoding <- match.arg(host_encoding)
  path_encoding <- match.arg(path_encoding)

  # Validate subdomain_levels_to_keep
  if (!is.null(subdomain_levels_to_keep) &&
    (!is.numeric(subdomain_levels_to_keep) ||
      subdomain_levels_to_keep < 0 ||
      subdomain_levels_to_keep %% 1 != 0)) {
    stop(
      "subdomain_levels_to_keep must be NULL or a non-negative integer.",
      call. = FALSE
    )
  }

  ._safe_parse_url_scalar(
    url = url,
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
}

# Vectorized wrapper for safe_parse_url
# Returns a data.frame for easy downstream use
#' Parse multiple URLs and return a data.frame of components
#'
#' Vectorized wrapper around \code{\link{safe_parse_url}} that returns a
#' data.frame with one row per input URL.
#'
#' @param url A character vector of URLs to be parsed.
#' @inheritParams safe_parse_url
#' @return A data.frame with one row per URL and the same fields returned by
#'   \code{\link{safe_parse_url}}. Invalid inputs return NA fields with
#'   \code{parse_status = "error"}.
#' @export
#' @examples
#' safe_parse_urls(c("example.com", "https://www.example.com/path"))
safe_parse_urls <- function(url,
                            protocol_handling = c(
                              "keep", "none", "strip", "http", "https"
                            ),
                            www_handling = c(
                              "none", "strip", "keep", "if_no_subdomain"
                            ),
                            tld_source = c("all", "private", "icann"),
                            case_handling = c(
                              "keep", "lower", "upper", "lower_host"
                            ),
                            trailing_slash_handling = c(
                              "none", "keep", "strip"
                            ),
                            index_page_handling = c("keep", "strip"),
                            path_normalization = c(
                              "none", "collapse_slashes", "dot_segments", "both"
                            ),
                            scheme_relative_handling = c(
                              "keep", "http", "https", "error"
                            ),
                            subdomain_levels_to_keep = NULL,
                            host_encoding = c("keep", "idna", "unicode"),
                            path_encoding = c("keep", "encode", "decode")) {
  # Match arguments once for performance
  protocol_handling <- match.arg(protocol_handling)
  www_handling <- match.arg(www_handling)
  tld_source <- match.arg(tld_source)
  case_handling <- match.arg(case_handling)
  trailing_slash_handling <- match.arg(trailing_slash_handling)
  index_page_handling <- match.arg(index_page_handling)
  path_normalization <- match.arg(path_normalization)
  scheme_relative_handling <- match.arg(scheme_relative_handling)
  host_encoding <- match.arg(host_encoding)
  path_encoding <- match.arg(path_encoding)

  # Validate subdomain_levels_to_keep
  if (!is.null(subdomain_levels_to_keep) &&
    (!is.numeric(subdomain_levels_to_keep) ||
      subdomain_levels_to_keep < 0 ||
      subdomain_levels_to_keep %% 1 != 0)) {
    stop(
      "subdomain_levels_to_keep must be NULL or a non-negative integer.",
      call. = FALSE
    )
  }

  urls <- url
  if (length(urls) == 0) {
    return(data.frame(
      original_url = character(0),
      scheme = character(0),
      host = character(0),
      port = integer(0),
      path = character(0),
      query = character(0),
      fragment = character(0),
      user = character(0),
      password = character(0),
      domain = character(0),
      tld = character(0),
      is_ip_host = logical(0),
      clean_url = character(0),
      parse_status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  urls_list <- as.list(urls)
  parsed_list <- lapply(urls_list, function(u) {
    ._safe_parse_url_scalar(
      url = u,
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
  })

  # Build a rectangular data.frame, filling NULLs with error rows
  original_url_vec <- vapply(urls_list, function(u) {
    if (is.null(u) ||
      length(u) == 0 ||
      (is.atomic(u) && length(u) == 1 && is.na(u))) {
      return(NA_character_)
    }
    if (is.character(u)) {
      return(u)
    }
    if (is.atomic(u) && length(u) == 1) {
      return(as.character(u))
    }
    NA_character_
  }, character(1))

  empty_row <- function(orig) {
    list(
      original_url = orig,
      scheme = NA_character_,
      host = NA_character_,
      port = NA_integer_,
      path = NA_character_,
      query = NA_character_,
      fragment = NA_character_,
      user = NA_character_,
      password = NA_character_,
      domain = NA_character_,
      tld = NA_character_,
      is_ip_host = NA,
      clean_url = NA_character_,
      parse_status = "error"
    )
  }

  normalized_list <- lapply(seq_along(parsed_list), function(i) {
    if (is.null(parsed_list[[i]])) {
      return(empty_row(original_url_vec[[i]]))
    }
    parsed_list[[i]]
  })

  data.frame(
    original_url = vapply(
      normalized_list,
      function(x) x$original_url %||% NA_character_,
      character(1)
    ),
    scheme = vapply(
      normalized_list, function(x) x$scheme %||% NA_character_, character(1)
    ),
    host = vapply(
      normalized_list, function(x) x$host %||% NA_character_, character(1)
    ),
    port = vapply(
      normalized_list, function(x) x$port %||% NA_integer_, integer(1)
    ),
    path = vapply(
      normalized_list, function(x) x$path %||% NA_character_, character(1)
    ),
    query = vapply(
      normalized_list, function(x) x$query %||% NA_character_, character(1)
    ),
    fragment = vapply(
      normalized_list, function(x) x$fragment %||% NA_character_, character(1)
    ),
    user = vapply(
      normalized_list, function(x) x$user %||% NA_character_, character(1)
    ),
    password = vapply(
      normalized_list, function(x) x$password %||% NA_character_, character(1)
    ),
    domain = vapply(
      normalized_list, function(x) x$domain %||% NA_character_, character(1)
    ),
    tld = vapply(
      normalized_list, function(x) x$tld %||% NA_character_, character(1)
    ),
    is_ip_host = vapply(
      normalized_list, function(x) x$is_ip_host %||% NA, logical(1)
    ),
    clean_url = vapply(
      normalized_list, function(x) x$clean_url %||% NA_character_, character(1)
    ),
    parse_status = vapply(
      normalized_list, function(x) x$parse_status %||% "error", character(1)
    ),
    stringsAsFactors = FALSE
  )
}

# Internal scalar helper that handles caching and calls the implementation
._safe_parse_url_scalar <- function(url,
                                    protocol_handling,
                                    www_handling,
                                    tld_source,
                                    case_handling,
                                    trailing_slash_handling,
                                    index_page_handling,
                                    path_normalization,
                                    scheme_relative_handling,
                                    subdomain_levels_to_keep,
                                    host_encoding,
                                    path_encoding) {
  # Early return for invalid input
  if (is.na(url) || !is.character(url) || url == "") {
    return(NULL)
  }

  # Generate cache key
  subdomain_key <- if (is.null(subdomain_levels_to_keep)) {
    "NULL"
  } else {
    as.character(subdomain_levels_to_keep)
  }
  cache_key <- paste(url, protocol_handling, www_handling, tld_source,
    case_handling, trailing_slash_handling, index_page_handling,
    path_normalization, scheme_relative_handling, subdomain_key,
    host_encoding, path_encoding,
    sep = "\x1F"
  )
  cache_key <- stringi::stri_escape_unicode(enc2utf8(cache_key))

  # Check cache
  cached <- .cache_get("full_parse", cache_key)
  if (!identical(cached, .rurl_cache_sentinel)) {
    return(cached)
  }

  # Call the implementation
  result <- ._safe_parse_url_impl(
    url = url,
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

  # Cache the result
  .cache_set("full_parse", cache_key, result)

  result
}

# Internal implementation of safe_parse_url (not memoized)
# Arguments are already validated by the memoizing wrapper
._safe_parse_url_impl <- function(url,
                                  protocol_handling,
                                  www_handling,
                                  tld_source,
                                  case_handling,
                                  trailing_slash_handling,
                                  index_page_handling,
                                  path_normalization,
                                  scheme_relative_handling,
                                  subdomain_levels_to_keep,
                                  host_encoding = "keep",
                                  path_encoding = "keep") {
  original_input_url <- url

  host_encoding <- match.arg(host_encoding, c("keep", "idna", "unicode"))
  path_encoding <- match.arg(path_encoding, c("keep", "encode", "decode"))

  # Phase 1: scheme detection and input preparation for curl
  prep <- .prepare_url_for_curl(
    url, protocol_handling, scheme_relative_handling
  )
  if (is.null(prep)) {
    return(NULL)
  }

  # Phase 2: parse with curl, then pull out raw components
  parsed_curl <- .parse_with_curl(prep$url_to_parse)
  if (is.null(parsed_curl)) {
    return(NULL)
  }
  raw <- .extract_raw_components(parsed_curl)
  raw_host <- raw$host
  raw_query <- raw$query

  # Phase 3: path normalization (decode, slashes/dots, index, trailing, encode)
  path_final <- .normalize_path(
    raw$path,
    path_encoding = path_encoding,
    path_normalization = path_normalization,
    index_page_handling = index_page_handling,
    trailing_slash_handling = trailing_slash_handling
  )

  # Phase 4: final scheme per protocol policy
  final_scheme <- .derive_final_scheme(
    protocol_handling, prep$original_looks_like_protocol, raw$scheme
  )

  # Phase 5: IP host detection
  is_ip_host <- .detect_ip_host(raw_host)

  # Phase 6: www prefix policy
  final_host <- .apply_www_policy(raw_host, www_handling, is_ip_host)

  # Phase 7: registered-domain and TLD derivation
  domain_tld <- .derive_domain_tld(final_host, is_ip_host, tld_source)
  domain <- domain_tld$domain
  tld <- domain_tld$tld

  # Phase 8: subdomain-level policy
  final_host <- .apply_subdomain_policy(
    final_host, domain, subdomain_levels_to_keep, is_ip_host
  )

  # Phase 9: host encoding (idna / unicode)
  host_for_clean <- .apply_host_encoding(final_host, host_encoding, is_ip_host)

  # Phase 10: case policy applied to host, path, and scheme
  cased <- .apply_case_policy(
    host_for_clean, path_final, final_scheme, case_handling
  )
  host_output <- cased$host
  path_output <- cased$path
  scheme_output <- cased$scheme

  # Phase 11: clean URL reconstruction
  clean_url <- .build_clean_url(
    scheme_output, host_output, path_output, trailing_slash_handling
  )

  # Phase 12: parse-status assignment
  parse_status <- .derive_parse_status(
    parsed_curl = parsed_curl,
    final_host = final_host,
    is_ip_host = is_ip_host,
    tld = tld,
    domain = domain,
    protocol_handling = protocol_handling,
    final_scheme = final_scheme,
    original_looks_like_protocol = prep$original_looks_like_protocol,
    original_has_allowed_scheme = prep$original_has_allowed_scheme,
    is_scheme_relative = prep$is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )

  # Phase 13: assemble the typed result list
  .assemble_parse_result(
    original_input_url = original_input_url,
    scheme_output = scheme_output,
    host_output = host_output,
    parsed_curl = parsed_curl,
    path_output = path_output,
    raw_query = raw_query,
    domain = domain,
    tld = tld,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status,
    is_scheme_relative = prep$is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )
}

# ---------------------------------------------------------------------------
# Decomposed phase helpers for ._safe_parse_url_impl().
#
# Each helper owns one normalization phase and can be reasoned about and
# tested independently. The code inside is moved verbatim from the original
# monolithic implementation (the only behavior changes are the separately
# tracked port/IPv6/subdomain bug fixes). None of these are memoized; the
# memoizing wrapper ._safe_parse_url_scalar() caches the whole result.
# ---------------------------------------------------------------------------

# Phase 1: scheme detection, supported-scheme policy, and building the string
# handed to curl. Returns NULL when the URL must be rejected (scheme-relative
# with "error" handling, or an unsupported explicit scheme under keep/none).
.prepare_url_for_curl <- function(url,
                                  protocol_handling,
                                  scheme_relative_handling) {
  allowed_prefixes <- c("http://", "https://", "ftp://", "ftps://")
  original_url_lower <- stringi::stri_trans_tolower(url)
  original_has_allowed_scheme <- any(
    startsWith(original_url_lower, allowed_prefixes)
  )

  scheme_match <- stringi::stri_match_first_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):"
  )
  scheme_candidate <- if (!is.na(scheme_match[1, 2])) {
    scheme_match[1, 2]
  } else {
    NA_character_
  }
  original_looks_like_protocol <- !is.na(scheme_candidate)
  has_explicit_scheme_with_slashes <- stringi::stri_detect_regex(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*):\\/\\/"
  )
  if (is.na(has_explicit_scheme_with_slashes)) {
    has_explicit_scheme_with_slashes <- FALSE
  }

  is_scheme_relative <- stringi::stri_startswith_fixed(url, "//")
  if (is_scheme_relative && scheme_relative_handling == "error") {
    return(NULL)
  }
  if (is_scheme_relative && scheme_relative_handling %in% c("http", "https")) {
    # Treat scheme-relative URLs as having an inferred scheme for handling logic
    original_looks_like_protocol <- TRUE
    original_has_allowed_scheme <- TRUE
  }

  looks_like_host_port <- FALSE
  if (original_looks_like_protocol &&
    !original_has_allowed_scheme &&
    !has_explicit_scheme_with_slashes) {
    looks_like_host_port <- stringi::stri_detect_regex(
      url, "^[^/]+:[0-9]+($|/)"
    )
    if (is.na(looks_like_host_port)) {
      looks_like_host_port <- FALSE
    }
  }

  if ((protocol_handling == "keep" || protocol_handling == "none") &&
    original_looks_like_protocol &&
    !original_has_allowed_scheme &&
    !looks_like_host_port) {
    return(NULL)
  }

  url_to_parse <- url
  if (is_scheme_relative) {
    if (scheme_relative_handling %in% c("http", "https")) {
      url_to_parse <- paste0(scheme_relative_handling, ":", url)
    } else {
      url_to_parse <- paste0("http:", url)
    }
  } else if (!original_looks_like_protocol || looks_like_host_port) {
    url_to_parse <- paste0("http://", url)
  }

  list(
    url_to_parse = url_to_parse,
    original_looks_like_protocol = original_looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    is_scheme_relative = is_scheme_relative,
    looks_like_host_port = looks_like_host_port
  )
}

# Phase 2a: parse the prepared URL with curl, returning NULL on failure.
.parse_with_curl <- function(url_to_parse) {
  tryCatch(
    curl::curl_parse_url(url_to_parse),
    error = function(e) NULL
  )
}

# Phase 2b: pull the raw components used downstream out of the curl result,
# reconstructing the query string from params when curl did not surface one.
.extract_raw_components <- function(parsed_curl) {
  raw_query <- parsed_curl$query %||% NA_character_
  if (is.na(raw_query) &&
    !is.null(parsed_curl$params) &&
    length(parsed_curl$params) > 0) {
    raw_query <- paste(
      names(parsed_curl$params),
      parsed_curl$params,
      sep = "=",
      collapse = "&"
    )
  }
  list(
    scheme = parsed_curl$scheme %||% NA_character_,
    host = parsed_curl$host %||% NA_character_,
    path = parsed_curl$path %||% NA_character_,
    query = raw_query
  )
}

# Phase 3: path decoding, slash/dot normalization, index stripping, trailing
# slash policy, and optional percent-encoding.
.normalize_path <- function(raw_path,
                            path_encoding,
                            path_normalization,
                            index_page_handling,
                            trailing_slash_handling) {
  path_work <- raw_path

  # Decode path if requested, before normalization/index handling
  if (!is.na(path_work) && path_encoding %in% c("decode", "encode")) {
    path_work <- tryCatch(
      curl::curl_unescape(path_work),
      error = function(e) path_work
    )
  }

  # Path normalization (slashes and dot segments)
  if (!is.na(path_work)) {
    if (path_normalization %in% c("collapse_slashes", "both")) {
      path_work <- ._collapse_path_slashes(path_work)
    }
    if (path_normalization %in% c("dot_segments", "both")) {
      path_work <- ._remove_dot_segments(path_work)
    }
  }

  # Index/default page handling
  if (!is.na(path_work) && index_page_handling == "strip") {
    path_work <- ._strip_index_page(path_work)
  }

  # Trailing slash handling (after normalization/index handling)
  if (!is.na(path_work) && nzchar(path_work)) {
    if (trailing_slash_handling == "strip") {
      if (path_work != "/" && stringi::stri_endswith_fixed(path_work, "/")) {
        path_work <- stringi::stri_sub(
          path_work, 1, stringi::stri_length(path_work) - 1
        )
      }
    } else if (trailing_slash_handling == "keep") {
      if (path_work != "/" && !stringi::stri_endswith_fixed(path_work, "/")) {
        path_work <- paste0(path_work, "/")
      }
    }
  }

  # Path percent-encoding handling (performed after normalization/index logic)
  if (!is.na(path_work) && path_encoding == "encode") {
    path_work <- ._encode_path_segments(path_work)
  }

  path_work
}

# Phase 4: resolve the final scheme according to protocol policy.
.derive_final_scheme <- function(protocol_handling,
                                 original_looks_like_protocol,
                                 raw_scheme) {
  switch(protocol_handling,
    none = if (original_looks_like_protocol) raw_scheme else NA_character_,
    strip = NA_character_,
    http = "http",
    https = "https",
    keep = raw_scheme
  )
}

# Phase 5: detect whether the host is an IP literal (IPv4 or IPv6).
.detect_ip_host <- function(raw_host) {
  if (is.na(raw_host) || raw_host == "") {
    return(FALSE)
  }
  # IPv4: 1.2.3.4
  ipv4 <- stringi::stri_detect_regex(raw_host, "^\\d{1,3}(\\.\\d{1,3}){3}$")
  # IPv6: [2001:db8::1] or 2001:db8::1
  ipv6 <- stringi::stri_detect_regex(
    raw_host, "^\\[?[0-9a-fA-F:]+\\]?$"
  ) && stringi::stri_detect_regex(raw_host, ":")
  if (is.na(ipv4)) ipv4 <- FALSE # Add NA check
  if (is.na(ipv6)) ipv6 <- FALSE # Add NA check
  ipv4 || ipv6
}

# Phase 6: apply the www-prefix policy to a (non-IP) host.
.apply_www_policy <- function(raw_host, www_handling, is_ip_host) {
  final_host <- raw_host
  if (!is_ip_host && !is.na(raw_host) && raw_host != "") {
    if (www_handling == "strip") {
      final_host <- stringi::stri_replace_first_regex(
        raw_host,
        "^(www[0-9]*\\.)(.*)",
        "$2",
        opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
      )
    } else if (www_handling == "keep") {
      current_host_lower <- stringi::stri_trans_tolower(raw_host)
      if (stringi::stri_detect_regex(current_host_lower, "^www[0-9]*\\.")) {
        match_res <- stringi::stri_match_first_regex(
          raw_host,
          "^(www[0-9]*\\.)(.*)",
          opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )
        bare_host_part <- if (!is.na(match_res[1, 3])) {
          match_res[1, 3]
        } else {
          raw_host
        }
        final_host <- paste0("www.", bare_host_part)
      } else {
        final_host <- paste0("www.", raw_host)
      }
    } else if (www_handling == "if_no_subdomain") {
      candidate_host <- raw_host
      if (stringi::stri_detect_regex(
        stringi::stri_trans_tolower(raw_host), "^www[0-9]*\\."
      )) {
        match_res <- stringi::stri_match_first_regex(
          raw_host,
          "^(www[0-9]*\\.)(.*)",
          opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )
        bare_part <- if (!is.na(match_res[1, 3])) match_res[1, 3] else raw_host
        candidate_host <- paste0("www.", bare_part)
      }
      host_for_domain_check <- candidate_host
      if (stringi::stri_startswith_fixed(
        stringi::stri_trans_tolower(candidate_host), "www."
      )) {
        match_res_bare <- stringi::stri_match_first_regex(
          candidate_host,
          "^www\\.(.*)",
          opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
        )
        host_for_domain_check <- if (!is.na(match_res_bare[1, 2])) {
          match_res_bare[1, 2]
        } else {
          candidate_host
        }
      }

      temp_host_nfc_lc <- stringi::stri_trans_nfc(
        stringi::stri_trans_tolower(host_for_domain_check)
      )
      temp_host_puny <- .normalize_and_punycode(temp_host_nfc_lc)

      temp_derived_domain_puny <- NA_character_
      if (!is.na(temp_host_puny) && temp_host_puny != "") {
        temp_derived_domain_puny <- .get_registered_domain(temp_host_puny)
      }
      temp_derived_domain_unicode <- .punycode_to_unicode(
        temp_derived_domain_puny
      )

      if (is.na(temp_derived_domain_unicode) ||
        temp_derived_domain_unicode == "") {
        final_host <- candidate_host
      } else {
        if (stringi::stri_trans_tolower(host_for_domain_check) ==
          temp_derived_domain_unicode) {
          if (!stringi::stri_startswith_fixed(
            stringi::stri_trans_tolower(candidate_host), "www."
          )) {
            final_host <- paste0("www.", candidate_host)
          } else {
            final_host <- candidate_host
          }
        } else {
          final_host <- candidate_host
        }
      }
    }
  }
  final_host
}

# Phase 7: derive the registered domain (Unicode) and TLD from the host using
# the Public Suffix List.
.derive_domain_tld <- function(final_host, is_ip_host, tld_source) {
  domain <- NA_character_
  tld <- NA_character_

  if (!is_ip_host && !is.na(final_host) && final_host != "") {
    host_for_derivation <- stringi::stri_trans_nfc(
      stringi::stri_trans_tolower(final_host)
    )
    encoded_host_for_derivation <- .normalize_and_punycode(host_for_derivation)

    if (!is.na(encoded_host_for_derivation) &&
      encoded_host_for_derivation != "") {
      derived_domain_encoded <- .get_registered_domain(
        encoded_host_for_derivation
      )
      if (!is.na(derived_domain_encoded)) {
        domain <- .punycode_to_unicode(derived_domain_encoded)
      }
      # Use pre-computed hash sets for O(1) lookup
      selected_tld_set <- switch(tld_source,
        all = .tld_all_set,
        private = .tld_private_set,
        icann = .tld_icann_set
      )
      tld <- ._extract_tld_original_logic(
        final_host, selected_tld_set, tld_source
      )
    }
  }

  list(domain = domain, tld = tld)
}

# Phase 8: keep only the requested number of subdomain levels.
.apply_subdomain_policy <- function(final_host, domain,
                                    subdomain_levels_to_keep, is_ip_host) {
  if (!is.null(subdomain_levels_to_keep) &&
    !is_ip_host &&
    !is.na(domain) &&
    domain != "" &&
    !is.na(final_host) &&
    final_host != "") {
    current_host_lower <- stringi::stri_trans_tolower(final_host)
    domain_lower <- stringi::stri_trans_tolower(domain)
    www_prefix_str <- "www."

    has_www_prefix <- stringi::stri_startswith_fixed(
      current_host_lower, www_prefix_str
    )

    host_part_to_analyze <- final_host
    if (has_www_prefix) {
      host_part_to_analyze <- stringi::stri_sub(
        final_host, stringi::stri_length(www_prefix_str) + 1
      )
    }

    host_part_to_analyze_lower <- stringi::stri_trans_tolower(
      host_part_to_analyze
    )

    if (host_part_to_analyze_lower == domain_lower) {
      # no-op
    } else if (stringi::stri_endswith_fixed(
      host_part_to_analyze_lower, paste0(".", domain_lower)
    )) {
      subdomain_component_string <- stringi::stri_sub(
        host_part_to_analyze,
        1,
        stringi::stri_length(host_part_to_analyze) -
          stringi::stri_length(domain_lower) - 1
      )

      sub_labels <- strsplit(subdomain_component_string, "\\.")[[1]]

      kept_sub_labels_string <- ""
      if (subdomain_levels_to_keep > 0) {
        num_sub_labels_to_keep <- min(
          length(sub_labels), subdomain_levels_to_keep
        )
        if (num_sub_labels_to_keep > 0) {
          actual_kept_labels <- tail(sub_labels, num_sub_labels_to_keep)
          kept_sub_labels_string <- paste(actual_kept_labels, collapse = ".")
        }
      }

      reconstructed_host_part <- domain_lower
      if (nzchar(kept_sub_labels_string)) {
        reconstructed_host_part <- paste0(
          kept_sub_labels_string, ".", domain_lower
        )
      }

      if (has_www_prefix) {
        final_host <- paste0(www_prefix_str, reconstructed_host_part)
      } else {
        final_host <- reconstructed_host_part
      }
    }
  }
  final_host
}

# Phase 9: re-encode the (non-IP) host to IDNA/Punycode or Unicode on request.
.apply_host_encoding <- function(final_host, host_encoding, is_ip_host) {
  host_for_clean <- final_host
  if (!is.na(host_for_clean) && host_for_clean != "" && !is_ip_host) {
    if (host_encoding == "idna") {
      encoded_host <- .normalize_and_punycode(host_for_clean)
      if (!is.na(encoded_host)) host_for_clean <- encoded_host
    } else if (host_encoding == "unicode") {
      decoded_host <- .punycode_to_unicode(host_for_clean)
      if (!is.na(decoded_host) && decoded_host != "") {
        host_for_clean <- decoded_host
      }
    }
  }
  host_for_clean
}

# Phase 10: apply the case policy to host, path, and scheme.
.apply_case_policy <- function(host_for_clean, path_final, final_scheme,
                               case_handling) {
  host_output <- host_for_clean
  path_output <- path_final
  scheme_output <- final_scheme

  if (!is.na(host_output) && host_output != "") {
    host_output <- switch(case_handling,
      lower = stringi::stri_trans_tolower(host_output),
      upper = stringi::stri_trans_toupper(host_output),
      lower_host = stringi::stri_trans_tolower(host_output),
      keep = host_output
    )
  }

  if (!is.na(path_output)) {
    path_output <- switch(case_handling,
      lower = stringi::stri_trans_tolower(path_output),
      upper = stringi::stri_trans_toupper(path_output),
      lower_host = path_output,
      keep = path_output
    )
  }

  if (!is.na(scheme_output)) {
    scheme_output <- switch(case_handling,
      lower = stringi::stri_trans_tolower(scheme_output),
      upper = stringi::stri_trans_toupper(scheme_output),
      lower_host = stringi::stri_trans_tolower(scheme_output),
      keep = scheme_output
    )
  }

  list(host = host_output, path = path_output, scheme = scheme_output)
}

# Phase 11: reconstruct the canonical "clean" URL from cased components.
.build_clean_url <- function(scheme_output, host_output, path_output,
                             trailing_slash_handling) {
  clean_url <- NA_character_
  if (!is.na(host_output) && host_output != "") {
    scheme_part <- if (!is.na(scheme_output)) {
      paste0(scheme_output, "://")
    } else {
      ""
    }
    path_part <- if (!is.na(path_output)) path_output else ""
    if (trailing_slash_handling == "strip" && identical(path_part, "/")) {
      path_part <- ""
    }
    clean_url <- paste0(scheme_part, host_output, path_part)
  }
  clean_url
}

# Phase 12: classify the parse outcome (ok / ok-ftp / warning-* / error /
# ok-scheme-relative).
.derive_parse_status <- function(parsed_curl, final_host, is_ip_host, tld,
                                 domain, protocol_handling, final_scheme,
                                 original_looks_like_protocol,
                                 original_has_allowed_scheme,
                                 is_scheme_relative,
                                 scheme_relative_handling) {
  parse_status <- "error"

  if (!is.null(parsed_curl)) {
    host_is_present <- !is.na(final_host) && final_host != ""

    if (host_is_present) {
      if (is_ip_host) {
        parse_status <- "ok"
      } else {
        host_has_dot <- stringi::stri_detect_fixed(final_host, ".")
        if (is.na(host_has_dot)) host_has_dot <- FALSE

        if (!host_has_dot) {
          parse_status <- "warning-no-tld"
        } else if (is.na(tld) || !nzchar(tld)) {
          parse_status <- "warning-invalid-tld"
        } else if (is.na(domain) || !nzchar(domain)) {
          parse_status <- "warning-public-suffix"
        } else {
          parse_status <- "ok"
        }
      }

      if (parse_status == "ok" &&
        protocol_handling != "strip" &&
        !is.na(final_scheme)) {
        current_scheme_lower <- stringi::stri_trans_tolower(final_scheme)
        if (current_scheme_lower %in% c("ftp", "ftps")) {
          parse_status <- "ok-ftp"
        }
      }
    }
  }

  if (is.null(parsed_curl) ||
    ((protocol_handling == "keep" || protocol_handling == "none") &&
      original_looks_like_protocol &&
      !original_has_allowed_scheme)) {
    parse_status <- "error"
  }

  if (is_scheme_relative &&
    scheme_relative_handling == "keep" &&
    parse_status == "ok") {
    parse_status <- "ok-scheme-relative"
  }

  parse_status
}

# Phase 13: coerce types and assemble the result list returned to callers.
.assemble_parse_result <- function(original_input_url, scheme_output,
                                   host_output, parsed_curl, path_output,
                                   raw_query, domain, tld, is_ip_host,
                                   clean_url, parse_status, is_scheme_relative,
                                   scheme_relative_handling) {
  scheme_return <- scheme_output
  if (is_scheme_relative && scheme_relative_handling == "keep") {
    scheme_return <- NA_character_
  }

  list(
    original_url = original_input_url,
    scheme = scheme_return,
    host = if (is.na(host_output) || host_output == "") {
      NA_character_
    } else {
      host_output
    },
    port = suppressWarnings(as.integer(parsed_curl$port %||% NA_integer_)),
    path = path_output,
    query = raw_query %||% NA_character_,
    fragment = parsed_curl$fragment %||% NA_character_,
    user = parsed_curl$user %||% NA_character_,
    password = parsed_curl$password %||% NA_character_,
    domain = domain,
    tld = tld,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status
  )
}

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
                               case_handling = "keep",
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
#' get_clean_url("Example.COM/Path") # Default keep, default no slash change
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
                          case_handling = "keep",
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

# Internal helper to encode hostnames using IDNA (Punycode)
# Accepts encode_fn for testability and fallback
.normalize_and_punycode <- function(host, encode_fn = punycoder::puny_encode) {
  if (is.na(host) || !nzchar(host)) {
    return(host)
  }
  host <- stringi::stri_trans_nfc(host) # Normalize Unicode

  encoded <- tryCatch(
    encode_fn(host, strict = TRUE),
    error = function(e) {
      tryCatch(encode_fn(host), error = function(e2) NA_character_)
    }
  )

  if (!is.character(encoded) || length(encoded) != 1L) {
    # nocov start
    return(NA_character_)
    # nocov end
  }
  encoded
}

# Internal helper to decode Punycode domain parts to Unicode
.punycode_to_unicode <- function(
  domain_puny,
  decode_fn = punycoder::puny_decode
) {
  if (is.na(domain_puny)) {
    return(NA_character_)
  }
  if (!nzchar(domain_puny)) {
    return("")
  }
  parts_puny <- strsplit(domain_puny, "\\.")[[1]]

  decoded_labels <- tryCatch(
    decode_fn(parts_puny, strict = FALSE),
    error = function(e) {
      # nocov start
      tryCatch(
        decode_fn(parts_puny),
        error = function(e2) rep(NA_character_, length(parts_puny))
      )
      # nocov end
    }
  )

  if (!is.character(decoded_labels) ||
    length(decoded_labels) != length(parts_puny)) {
    # nocov start
    decoded_labels <- rep(NA_character_, length(parts_puny))
    # nocov end
  }

  decoded_labels[is.na(decoded_labels)] <- parts_puny[is.na(decoded_labels)]

  # Ensure labels are valid UTF-8 and drop irrecoverable bytes.
  sane_labels <- iconv(decoded_labels, from = "UTF-8", to = "UTF-8", sub = "")
  sane_labels[is.na(sane_labels)] <- ""

  paste(sane_labels, collapse = ".")
}

# Internal helper to collapse duplicate slashes in paths
._collapse_path_slashes <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  gsub("/+", "/", path, perl = TRUE)
}

# Internal helper to remove dot segments per RFC 3986
._remove_dot_segments <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }

  input <- path
  output <- ""

  while (nzchar(input)) {
    if (stringi::stri_startswith_fixed(input, "../")) {
      input <- stringi::stri_sub(input, 4)
    } else if (stringi::stri_startswith_fixed(input, "./")) {
      input <- stringi::stri_sub(input, 3)
    } else if (stringi::stri_startswith_fixed(input, "/./")) {
      input <- paste0("/", stringi::stri_sub(input, 4))
    } else if (identical(input, "/.")) {
      input <- "/"
    } else if (stringi::stri_startswith_fixed(input, "/../")) {
      input <- paste0("/", stringi::stri_sub(input, 5))
      output <- sub("/?[^/]*$", "", output)
    } else if (identical(input, "/..")) {
      input <- "/"
      output <- sub("/?[^/]*$", "", output)
    } else if (identical(input, ".") || identical(input, "..")) {
      input <- ""
    } else {
      match <- regexpr("^(/?[^/]*)", input, perl = TRUE)
      segment <- regmatches(input, match)
      output <- paste0(output, segment)
      input <- substring(input, attr(match, "match.length") + 1)
    }
  }

  output
}

# Internal helper to strip index/default pages from the end of a path
._strip_index_page <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  match <- stringi::stri_match_first_regex(
    path,
    "(?i)^(.*)/(index|default)\\.[^/]+/?$"
  )
  if (is.na(match[1, 1])) {
    return(path)
  }
  base <- match[1, 2]
  if (is.na(base) || base == "") {
    return("/")
  }
  if (!stringi::stri_startswith_fixed(base, "/")) {
    base <- paste0("/", base)
  }
  base
}

# Internal helper to percent-encode path segments
._encode_path_segments <- function(path) {
  if (is.na(path)) {
    return(path)
  }
  has_leading <- stringi::stri_startswith_fixed(path, "/")
  has_trailing <- stringi::stri_endswith_fixed(path, "/")
  segments <- strsplit(path, "/", fixed = TRUE)[[1]]
  encoded_segments <- vapply(
    segments,
    function(seg) curl::curl_escape(seg),
    character(1)
  )
  recomposed <- paste(encoded_segments, collapse = "/")
  if (has_leading && !stringi::stri_startswith_fixed(recomposed, "/")) {
    recomposed <- paste0("/", recomposed)
  }
  if (has_trailing && !stringi::stri_endswith_fixed(recomposed, "/")) {
    recomposed <- paste0(recomposed, "/")
  }
  recomposed
}

# Internal helper to derive a registered domain from host + TLD
._derive_domain_from_tld <- function(host_unicode, tld_unicode) {
  if (is.na(host_unicode) || !nzchar(host_unicode)) {
    return(NA_character_)
  }
  if (is.na(tld_unicode) || !nzchar(tld_unicode)) {
    return(NA_character_)
  }

  host_lc <- stringi::stri_trans_tolower(host_unicode)
  tld_lc <- stringi::stri_trans_tolower(tld_unicode)

  if (host_lc == tld_lc) {
    return(NA_character_)
  }
  suffix <- paste0(".", tld_lc)
  if (!stringi::stri_endswith_fixed(host_lc, suffix)) {
    return(NA_character_)
  }

  host_left <- stringi::stri_sub(
    host_lc,
    1,
    stringi::stri_length(host_lc) - stringi::stri_length(suffix)
  )
  if (!nzchar(host_left)) {
    return(NA_character_)
  }

  labels <- strsplit(host_left, "\\.")[[1]]
  paste0(utils::tail(labels, 1), ".", tld_lc)
}

# Internal helper to parse query strings into a list
._parse_query_string <- function(query, decode = TRUE) {
  if (is.na(query) || !nzchar(query)) {
    return(list())
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  result <- list()

  for (part in parts) {
    if (!nzchar(part)) next
    kv <- strsplit(part, "=", fixed = TRUE)[[1]]
    key <- kv[1]
    value <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else ""

    if (decode) {
      key <- tryCatch(curl::curl_unescape(key), error = function(e) key)
      value <- tryCatch(curl::curl_unescape(value), error = function(e) value)
    }

    if (key %in% names(result)) {
      result[[key]] <- c(result[[key]], value)
    } else {
      result[[key]] <- value
    }
  }

  result
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
  .extract_from_urls(url, NULL,
    transform = function(parsed) {
      if (source == "all") {
        return(parsed$domain %||% NA_character_)
      }
      if (isTRUE(parsed$is_ip_host)) {
        return(NA_character_)
      }
      host_unicode <- .punycode_to_unicode(parsed$host %||% NA_character_)
      ._derive_domain_from_tld(host_unicode, parsed$tld %||% NA_character_)
    },
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
#' "lower".
#' @return A character vector of URL paths.
#' @export
#' @examples
#' get_path("http://example.com/some/path?query=1")
get_path <- function(
  url,
  protocol_handling = "keep",
  case_handling = c("lower", "keep", "upper", "lower_host")
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

    domain_val <- if (source == "all") {
      parsed$domain %||% NA_character_
    } else {
      ._derive_domain_from_tld(host_unicode, parsed$tld %||% NA_character_)
    }

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
    if (!include_www &&
      length(labels) == 1 &&
      grepl("^www[0-9]*$", labels[1])) {
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

# Internal helper to derive registered domain using Public Suffix List
# Expects hostname_encoded to already be NFC-normalized, lowercased,
# and Punycode-encoded if non-ASCII.
# Uses pre-computed hash sets from .onLoad for O(1) lookups
# instead of linear %in% searches.
# Results are memoized for performance.
.get_registered_domain <- function(hostname) {
  # Check cache first
  cached <- .cache_get("domain", hostname)
  if (!identical(cached, .rurl_cache_sentinel)) {
    return(cached)
  }

  result <- ._get_registered_domain_impl(hostname)

  # Cache the result
  .cache_set("domain", hostname, result)
  result
}

# Internal implementation of .get_registered_domain (not memoized)
._get_registered_domain_impl <- function(hostname) {
  if (is.na(hostname) || !nzchar(hostname)) {
    return(NA_character_)
  }

  hostname_core <- if (stringi::stri_endswith_fixed(hostname, ".")) {
    stringi::stri_sub(hostname, 1, stringi::stri_length(hostname) - 1)
  } else {
    hostname
  }

  if (!nzchar(hostname_core)) {
    return(NA_character_)
  }

  parts <- strsplit(hostname_core, "\\.")[[1]]
  if (any(!nzchar(parts))) {
    return(NA_character_)
  }

  n <- length(parts)
  if (n < 2) {
    return(NA_character_)
  }

  # 1. Exception rules (take precedence)
  # Uses .psl_exception_set (environment) for O(1) lookup
  for (i in seq_len(n)) {
    candidate <- paste(parts[i:n], collapse = ".")

    if (.in_set(candidate, .psl_exception_set)) {
      # Exception match: treat the exception domain as *not* a suffix
      # So return one label above it
      return(candidate)
    }
  }

  # 2. Track best match length
  best_match_len <- 0L

  for (i in seq_len(n)) { # i is the start index of a suffix candidate in parts
    candidate_suffix_str <- paste(parts[i:n], collapse = ".")
    num_parts_in_candidate_suffix <- n - i + 1L

    # Check if candidate_suffix_str is an exact match in normal_rules
    # Uses .psl_normal_set (environment) for O(1) lookup
    if (.in_set(candidate_suffix_str, .psl_normal_set)) {
      if (num_parts_in_candidate_suffix > best_match_len) {
        best_match_len <- num_parts_in_candidate_suffix
      }
    }

    # Check if candidate_suffix_str matches a wildcard rule.
    # A wildcard rule means "*." + some_suffix_in_wildcard_rules.
    # Uses .psl_wildcard_set (environment) for O(1) lookup
    # Must have at least "label.wildcard_part"
    if (num_parts_in_candidate_suffix > 1L) {
      potential_wildcard_match_part <- paste(parts[(i + 1L):n], collapse = ".")
      if (.in_set(potential_wildcard_match_part, .psl_wildcard_set)) {
        if (num_parts_in_candidate_suffix > best_match_len) {
          best_match_len <- num_parts_in_candidate_suffix
        }
      }
    }
  }

  if (best_match_len == 0L) {
    return(NA_character_)
  }

  # If hostname is a public suffix itself
  # (or shorter than the matched public suffix),
  # it cannot be a "registered domain" by the eTLD+1 definition.
  if (n <= best_match_len) {
    return(NA_character_)
  }

  # Standard case: n > best_match_len
  # The registered domain is the public suffix (best_match_len parts)
  # plus one additional label to the left.
  return(paste(parts[(n - best_match_len):n], collapse = "."))
}

# Internal helper using the exact original get_tld logic for TLD extraction
# Uses hash set (environment) for O(1) lookup instead of linear %in% search.
# Results are memoized for performance.
._extract_tld_original_logic <- function(
  host_to_process,
  current_tld_set,
  tld_source_id = "all"
) {
  if (is.na(host_to_process) || !nzchar(host_to_process)) {
    return(NA_character_)
  }

  # Generate cache key including both host and source
  cache_key <- paste(host_to_process, tld_source_id, sep = "\x1F")
  cache_key <- stringi::stri_escape_unicode(enc2utf8(cache_key))

  # Check cache
  cached <- .cache_get("tld", cache_key)
  if (!identical(cached, .rurl_cache_sentinel)) {
    return(cached)
  }

  result <- ._extract_tld_impl(host_to_process, current_tld_set)

  # Cache the result
  .cache_set("tld", cache_key, result)
  result
}

# Internal implementation of TLD extraction (not memoized)
._extract_tld_impl <- function(host_to_process, current_tld_set) {
  normalized_host <- stringi::stri_trans_nfc(
    stringi::stri_trans_tolower(host_to_process)
  )
  encoded_host <- .normalize_and_punycode(normalized_host)

  if (is.na(encoded_host) || !nzchar(encoded_host)) {
    return(NA_character_)
  }

  encoded_host_core <- if (stringi::stri_endswith_fixed(encoded_host, ".")) {
    stringi::stri_sub(encoded_host, 1, stringi::stri_length(encoded_host) - 1)
  } else {
    encoded_host
  }

  if (!nzchar(encoded_host_core)) {
    return(NA_character_)
  }

  parts <- strsplit(encoded_host_core, "\\.")[[1]]
  if (any(!nzchar(parts))) {
    return(NA_character_)
  }

  n <- length(parts)
  if (n < 2L) {
    return(NA_character_)
  }

  if (n > 1L) {
    for (i in seq_len(n - 1L)) { # Checks suffixes of length n down to 2
      candidate <- paste(parts[i:n], collapse = ".") # Candidate is Punycode
      if (.in_set(candidate, current_tld_set)) { # O(1) lookup
        return(.punycode_to_unicode(candidate)) # Decodes matched Punycode TLD
      }
    }
  }

  if (n > 0L) { # Fallback to last part
    last_candidate <- parts[n] # Punycode
    if (.in_set(last_candidate, current_tld_set)) { # O(1) lookup
      return(.punycode_to_unicode(last_candidate))
    }
  }

  return(NA_character_)
}
