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
#'
#' @param url A character vector containing one or more URLs to be parsed.
#' @param protocol_handling A character string specifying how to handle
#'   protocols. Defaults to "keep".
#'   \itemize{
#'     \item{"keep": If a scheme exists (http, https, ftp, ftps), it's used. If no scheme, "http://" is added.}
#'     \item{"none": If a scheme exists, it's used. If no scheme, then no scheme is used (scheme component will be NA).}
#'     \item{"strip": Any existing scheme is removed (scheme component will be NA).}
#'     \item{"http": The scheme is forced to be "http".}
#'     \item{"https": The scheme is forced to be "https".}
#'   }
#' @param www_handling A character string specifying how to handle "www"
#'   and "www[number]" prefixes in the host. Defaults to "none".
#'   \itemize{
#'     \item{"none": (Default) Leaves the host's www prefix (or lack thereof) untouched.}
#'     \item{"strip": Removes any "www." or "www[number]." prefix.}
#'     \item{"keep": Ensures the host starts with "www.". If it has "www[number].", it's normalized to "www.". If no www prefix, "www." is added. An empty input host remains empty.}
#'     \item{"if_no_subdomain": If the host is a bare registered domain (e.g., "example.com"), "www." is added. If the host already has a "www." or "www[number]." prefix, it is normalized to "www." (e.g., "www1.example.com" becomes "www.example.com"; "www1.sub.example.com" becomes "www.sub.example.com"). If a non-www subdomain exists (e.g., "sub.example.com" or the normalized "www.sub.example.com"), the host is not further altered. An empty input host remains empty.}
#'   }
#' @param tld_source Which TLD source to use for TLD extraction: "all", "icann",
#'   or "private". Defaults to "all".
#' @param case_handling A character string specifying how to handle the case of
#'                      the cleaned URL. Defaults to "lower".
#'   \itemize{
#'     \item{"keep": Preserves the original casing.}
#'     \item{"lower": (Default) Converts the cleaned URL to lowercase.}
#'     \item{"upper": Converts the cleaned URL to uppercase.}
#'   }
#' @param trailing_slash_handling A character string specifying how to handle
#'   trailing slashes in the path component of the cleaned URL. Defaults to "none".
#'   \itemize{
#'     \item{"none": (Default) No specific handling is applied. Path remains as is after initial parsing.}
#'     \item{"keep": Ensures a trailing slash. If a path exists and doesn't end with one, it's added. If path is just "/", it's kept.}
#'     \item{"strip": Removes a trailing slash if present, unless the path is solely "/".}
#'   }
#' @param subdomain_levels_to_keep An integer or NULL. Determines how many levels of subdomains are kept,
#'   in addition to any 'www.' prefix handled by `www_handling`.
#'   \itemize{
#'     \item{`NULL`: (Default) No specific subdomain stripping is performed beyond `www_handling`.}
#'     \item{`0`: All subdomains are stripped. If `www_handling` preserved or added 'www.',
#'          it remains (e.g., 'www.sub.example.com' becomes 'www.example.com'; 'sub.example.com' becomes 'example.com').}
#'     \item{`N > 0`: Keeps up to N levels of subdomains, counted from right-to-left (closest to the registered domain),
#'          in addition to any 'www.' prefix. E.g., if N=1, 'three.two.one.example.com' becomes 'one.example.com';
#'          'www.three.two.one.example.com' (post www_handling) becomes 'www.one.example.com'.}
#'   }
#' @return A named list with the following components:
#'   \itemize{
#'     \item `original_url`: The original URL string provided.
#'     \item `scheme`: The scheme (e.g., "http", "https").
#'     \item `host`: The host (e.g., "www.example.com"). NA if the host becomes empty after processing.
#'     \item `port`: The port number.
#'     \item `path`: The path component (e.g., "/path/to/resource").
#'     \item `query`: The query string (e.g., "name=value").
#'     \item `fragment`: The fragment identifier (e.g., "section").
#'     \item `user`: The user name for authentication.
#'     \item `password`: The password for authentication.
#'     \item `domain`: The registered domain name (e.g., "example.com"). NA if host is an IP, empty, or derivation fails.
#'     \item `tld`: The top-level domain (e.g., "com"). NA if host is an IP, empty, or derivation fails.
#'     \item `is_ip_host`: Logical, TRUE if the host is an IP address.
#'     \item `clean_url`: The URL reconstructed from scheme, host, and path after processing, with case handling applied. NA if host is empty/NA.
#'     \item `parse_status`: Character string indicating parsing outcome ("ok", "ok-ftp", "error", "warning-no-tld").
#'   }
#'   Returns `NULL` if the URL is fundamentally unparseable (e.g., NA, empty) or uses a disallowed scheme.
#' @keywords internal
#' @export
#' @examples
#' safe_parse_url("http://www.Example.com/Path?q=1#Frag", protocol_handling = "keep", case_handling = "lower")
#' safe_parse_url("Example.com/Another", protocol_handling = "none", www_handling = "keep", case_handling = "upper", trailing_slash_handling = "keep")
#' safe_parse_url("example.com", www_handling = "if_no_subdomain") # -> www.example.com
#' safe_parse_url("sub.example.com", www_handling = "if_no_subdomain") # -> sub.example.com
#' safe_parse_url("www1.example.com", www_handling = "if_no_subdomain") # -> www.example.com
#' safe_parse_url("www1.sub.example.com", www_handling = "if_no_subdomain") # -> www.sub.example.com
#' safe_parse_url("http://www.example.com/path/", trailing_slash_handling = "strip")
#' safe_parse_url("192.168.1.1/test")
#' safe_parse_url("ftp://user:pass@ftp.example.co.uk:21/file.txt")
safe_parse_url <- function(url,
                           protocol_handling = c("keep", "none", "strip", "http", "https"),
                           www_handling = c("none", "strip", "keep", "if_no_subdomain"),
                           tld_source = c("all", "private", "icann"),
                           case_handling = c("lower", "keep", "upper"),
                           trailing_slash_handling = c("none", "keep", "strip"),
                           subdomain_levels_to_keep = NULL) {
  protocol_handling <- match.arg(protocol_handling)
  www_handling <- match.arg(www_handling)
  tld_source <- match.arg(tld_source)
  case_handling <- match.arg(case_handling)
  trailing_slash_handling <- match.arg(trailing_slash_handling)

  if (!is.null(subdomain_levels_to_keep) && (!is.numeric(subdomain_levels_to_keep) || subdomain_levels_to_keep < 0 || subdomain_levels_to_keep %% 1 != 0)) {
    stop("subdomain_levels_to_keep must be NULL or a non-negative integer.", call. = FALSE)
  }

  original_input_url <- url

  if (is.na(url) || !is.character(url) || url == "") {
    return(NULL)
  }

  allowed_prefixes <- c("http://", "https://", "ftp://", "ftps://")
  original_url_lower <- stringi::stri_trans_tolower(url)
  original_has_allowed_scheme <- any(startsWith(original_url_lower, allowed_prefixes))
  original_looks_like_protocol <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:", url)

  # Only return NULL for unallowed schemes if we intend to keep/use the original scheme.
  # If protocol_handling is 'strip', 'http', or 'https', we'll attempt to fix it.
  if ((protocol_handling == "keep" || protocol_handling == "none") &&
      original_looks_like_protocol &&
      !original_has_allowed_scheme) {
    return(NULL)
  }

  url_to_parse <- url
  if (!original_looks_like_protocol) {
    url_to_parse <- paste0("http://", url)
  }

  parsed_curl <- tryCatch(curl::curl_parse_url(url_to_parse), error = function(e) NULL)

  # If curl_parse_url failed, parsed_curl will be NULL. Return NULL immediately.
  if (is.null(parsed_curl)) {
    return(NULL)
  }

  raw_scheme <- parsed_curl$scheme %||% NA_character_
  raw_host <- parsed_curl$host %||% NA_character_
  raw_path <- parsed_curl$path %||% NA_character_

  # Apply trailing slash handling to raw_path
  if (!is.na(raw_path) && nzchar(raw_path)) { # Only process if path is not NA and not empty
    if (trailing_slash_handling == "strip") {
      if (raw_path != "/" && stringi::stri_endswith_fixed(raw_path, "/")) {
        raw_path <- stringi::stri_sub(raw_path, 1, stringi::stri_length(raw_path) - 1)
      }
    } else if (trailing_slash_handling == "keep") {
      if (raw_path != "/" && !stringi::stri_endswith_fixed(raw_path, "/")) {
        raw_path <- paste0(raw_path, "/")
      }
      # If raw_path is just "/", stringi::stri_endswith_fixed(raw_path, "/") is true, so it's kept.
      # If curl_parse_url returns empty string for path (should be "/" for root),
      # this logic might need adjustment, but current assumption is it's at least "/" or non-empty.
      # For an initially empty path (e.g. urltools::url_parse("http://a.com")$path is "")
      # we should add a slash if not already there.
      # However, curl::curl_parse gives "/" for "http://a.com"
    }
    # If trailing_slash_handling == "none", raw_path is unchanged.
  }

  final_scheme <- switch(
    protocol_handling,
    none = if (original_looks_like_protocol) raw_scheme else NA_character_,
    strip = NA_character_,
    http = "http",
    https = "https",
    keep = raw_scheme
  )

  # Use regex for IP detection (IPv4 and IPv6)
  is_ip_host <- if (is.na(raw_host) || raw_host == "") FALSE else {
    # IPv4: 1.2.3.4
    ipv4 <- grepl("^\\d{1,3}(\\.\\d{1,3}){3}$", raw_host)
    # IPv6: [2001:db8::1] or 2001:db8::1
    ipv6 <- grepl("^\\\\[?[0-9a-fA-F:]+\\\\]?$", raw_host) && grepl(":", raw_host)
    ipv4 || ipv6
  }
  final_host <- raw_host

  if (!is_ip_host && !is.na(raw_host) && raw_host != "") {
    if (www_handling == "strip") {
      final_host <- stringi::stri_replace_first_regex(raw_host, "^(www[0-9]*\\.)(.*)", "$2", opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
    } else if (www_handling == "keep") {
      # Ensure www. prefix, normalizing www[n]. -> www., adding if absent. Empty host stays empty.
      if (raw_host == "") {
        final_host <- ""
      } else {
        current_host_lower <- stringi::stri_trans_tolower(raw_host)
        if (grepl("^www[0-9]*\\.", current_host_lower)) { # grepl is fine for this pattern
          bare_host_part <- stringi::stri_replace_first_regex(raw_host, "^www[0-9]*\\.(.*)", "$1", opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
          final_host <- paste0("www.", bare_host_part)
        } else { # Does not start with any www prefix
          final_host <- paste0("www.", raw_host)
        }
      }
    } else if (www_handling == "if_no_subdomain") {
      if (raw_host == "") { # Handle empty host case explicitly similar to "keep"
        final_host <- ""
      } else {
        # Step 1: Tentatively normalize any existing www[n]. prefix to www.
        candidate_host <- raw_host
        if (grepl("^www[0-9]*\\.", stringi::stri_trans_tolower(raw_host))) {
          bare_part <- stringi::stri_replace_first_regex(raw_host, "^www[0-9]*\\.(.*)", "$1", opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
          candidate_host <- paste0("www.", bare_part)
        }
        # Now, candidate_host is raw_host (if no www[n]. originally) or www.rest_of_host (if www[n]. was normalized).

        # Step 2: Check if candidate_host is effectively a "bare domain"
        host_for_domain_check <- candidate_host
        if (stringi::stri_startswith_fixed(stringi::stri_trans_tolower(candidate_host), "www.")) {
          host_for_domain_check <- stringi::stri_replace_first_regex(candidate_host, "^www\\.(.*)", "$1", opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
        }

        temp_host_nfc_lc <- stringi::stri_trans_nfc(stringi::stri_trans_tolower(host_for_domain_check))
        temp_host_puny <- .normalize_and_punycode(temp_host_nfc_lc)

        temp_derived_domain_puny <- NA_character_
        if (!is.na(temp_host_puny) && temp_host_puny != "") {
          temp_derived_domain_puny <- .get_registered_domain(temp_host_puny)
        }
        temp_derived_domain_unicode <- .punycode_to_unicode(temp_derived_domain_puny)

        if (is.na(temp_derived_domain_unicode) || temp_derived_domain_unicode == "") {
          final_host <- candidate_host # Default to (potentially www-normalized) host
        } else {
          if (stringi::stri_trans_tolower(host_for_domain_check) == temp_derived_domain_unicode) {
            # It's a bare domain structure. Ensure it has "www.".
            if (!stringi::stri_startswith_fixed(stringi::stri_trans_tolower(candidate_host), "www.")) {
              final_host <- paste0("www.", candidate_host)
            } else {
              final_host <- candidate_host # Already has www.
            }
          } else {
            # It's a subdomain structure, or something like "co.uk". Use the normalized candidate_host.
            final_host <- candidate_host
          }
        }
      }
    }
  }

  domain <- NA_character_
  tld <- NA_character_

  if (!is_ip_host && !is.na(final_host) && final_host != "") {
    host_for_derivation <- stringi::stri_trans_nfc(stringi::stri_trans_tolower(final_host))
    encoded_host_for_derivation <- .normalize_and_punycode(host_for_derivation)

    if (!is.na(encoded_host_for_derivation) && encoded_host_for_derivation != "") {
      derived_domain_encoded <- .get_registered_domain(encoded_host_for_derivation)
      if (!is.na(derived_domain_encoded)) {
        domain <- .punycode_to_unicode(derived_domain_encoded)
      }
      # TLD derivation using the original logic helper
      selected_tlds_list <- switch(
        tld_source, # tld_source is the arg to safe_parse_url
        all = tld_all,
        private = tld_private,
        icann = tld_icann
      )
      tld <- ._extract_tld_original_logic(final_host, selected_tlds_list)
    }
  }

  # Apply subdomain_levels_to_keep logic
  if (!is.null(subdomain_levels_to_keep) && !is_ip_host && !is.na(domain) && domain != "" && !is.na(final_host) && final_host != "") {
    current_host_lower <- stringi::stri_trans_tolower(final_host)
    domain_lower <- stringi::stri_trans_tolower(domain)
    www_prefix_str <- "www."

    has_www_prefix <- stringi::stri_startswith_fixed(current_host_lower, www_prefix_str)
    
    host_part_to_analyze <- final_host
    if (has_www_prefix) {
      # Get the part of the host after "www."
      host_part_to_analyze <- stringi::stri_sub(final_host, stringi::stri_length(www_prefix_str) + 1)
    }
    
    host_part_to_analyze_lower <- stringi::stri_trans_tolower(host_part_to_analyze)

    if (host_part_to_analyze_lower == domain_lower) {
      # No subdomains other than potentially www, or host is just the domain.
      # final_host is already correct (it's original final_host after www_handling)
    } else if (stringi::stri_endswith_fixed(host_part_to_analyze_lower, paste0(".", domain_lower))) {
      # There are subdomains in host_part_to_analyze
      subdomain_component_string <- stringi::stri_sub(
        host_part_to_analyze, 
        1, 
        stringi::stri_length(host_part_to_analyze) - stringi::stri_length(domain_lower) - 1
      )
      
      sub_labels <- strsplit(subdomain_component_string, "\\\\.")[[1]]
      
      kept_sub_labels_string <- ""
      if (subdomain_levels_to_keep > 0) {
        num_sub_labels_to_keep <- min(length(sub_labels), subdomain_levels_to_keep)
        if (num_sub_labels_to_keep > 0) {
          actual_kept_labels <- tail(sub_labels, num_sub_labels_to_keep)
          kept_sub_labels_string <- paste(actual_kept_labels, collapse = ".")
        }
      } # if subdomain_levels_to_keep is 0, kept_sub_labels_string remains ""

      reconstructed_host_part <- domain_lower # Start with domain
      if (nzchar(kept_sub_labels_string)) {
        reconstructed_host_part <- paste0(kept_sub_labels_string, ".", domain_lower)
      }
      
      if (has_www_prefix) {
        final_host <- paste0(www_prefix_str, reconstructed_host_part)
      } else {
        final_host <- reconstructed_host_part
      }
    }
    # If host_part_to_analyze_lower does not end with .domain_lower (e.g. host is "com" and domain is "example.com")
    # or other edge cases, final_host remains as it was after www_handling. This ensures robustness.
  }

  clean_url <- NA_character_
  if (!is.na(final_host) && final_host != "") {
    scheme_part <- if (!is.na(final_scheme)) paste0(final_scheme, "://") else ""
    clean_url <- paste0(scheme_part, final_host, raw_path)
  }

  # Apply case handling to clean_url
  if (!is.na(clean_url)) {
    clean_url <- switch(
      case_handling,
      lower = stringi::stri_trans_tolower(clean_url),
      upper = stringi::stri_trans_toupper(clean_url),
      keep = clean_url # Default is to keep original casing
    )
  }

  # Initialize parse_status
  parse_status <- "error" # Default to error

  if (!is.null(parsed_curl)) { # Ensure basic parsing by curl was successful
    host_is_structurally_ok <- !is.na(final_host) && final_host != "" &&
                               (is_ip_host || grepl("\\.", final_host))

    if (host_is_structurally_ok) {
      parse_status <- "ok"

      if (protocol_handling != "strip" && !is.na(final_scheme)) {
        current_scheme_lower <- stringi::stri_trans_tolower(final_scheme)
        if (current_scheme_lower %in% c("ftp", "ftps")) {
          parse_status <- "ok-ftp"
        }
      }
    } else if (!is.na(final_host) && final_host != "" && !is_ip_host && !grepl("\\.", final_host)) {
      parse_status <- "warning-no-tld"
    }
  }

  # Override to "error" for fundamentally unparseable original schemes
  # or if the original scheme was disallowed AND we weren't trying to fix it.
  if (is.null(parsed_curl) ||
      ((protocol_handling == "keep" || protocol_handling == "none") &&
       original_looks_like_protocol &&
       !original_has_allowed_scheme)) {
    parse_status <- "error"
  }

  list(
    original_url = original_input_url,
    scheme = final_scheme,
    host = if (is.na(final_host) || final_host == "") NA_character_ else final_host,
    port = parsed_curl$port %||% NA_integer_,
    path = raw_path,
    query = parsed_curl$query %||% NA_character_,
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

#' Get the parse status of URLs
#'
#' @param url A character vector of URLs to be parsed.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          See \code{\link{safe_parse_url}} for details. Defaults to "keep".
#' @param www_handling A character string specifying how to handle "www" prefixes.
#'                     See \code{\link{safe_parse_url}} for details. Defaults to "none".
#' @param subdomain_levels_to_keep An integer or NULL. Determines how many levels of subdomains are kept,
#'   in addition to any 'www.' prefix handled by `www_handling`.
#'   \itemize{
#'     \item{`NULL`: (Default) No specific subdomain stripping is performed beyond `www_handling`.}
#'     \item{`0`: All subdomains are stripped. If `www_handling` preserved or added 'www.',
#'          it remains (e.g., 'www.sub.example.com' becomes 'www.example.com'; 'sub.example.com' becomes 'example.com').}
#'     \item{`N > 0`: Keeps up to N levels of subdomains, counted from right-to-left (closest to the registered domain),
#'          in addition to any 'www.' prefix. E.g., if N=1, 'three.two.one.example.com' becomes 'one.example.com';
#'          'www.three.two.one.example.com' (post www_handling) becomes 'www.one.example.com'.}
#'   }
#' @return A character vector with the parse status of each URL.
#' @export
#' @examples
#' get_parse_status(c("http://example.com", "ftp://example.com", "mailto:user@example.com"))
#' get_parse_status(c("http://example.com", "not-a-url"))
get_parse_status <- function(url,
                             protocol_handling = "keep",
                             www_handling = "none",
                             subdomain_levels_to_keep = NULL) {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all",
                             case_handling = "lower",
                             trailing_slash_handling = "none",
                             subdomain_levels_to_keep = subdomain_levels_to_keep)
    if (is.null(parsed)) return("error")
    parsed$parse_status %||% "error"
  }, character(1))
}

#' Get cleaned URLs
#'
#' This function returns the cleaned version of the URLs after applying
#' protocol, www, case, and trailing slash handling rules.
#'
#' @param url A character vector containing URLs to be parsed.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          See \code{\link{safe_parse_url}} for details. Defaults to "keep".
#' @param www_handling A character string specifying how to handle "www" prefixes.
#'                     See \code{\link{safe_parse_url}} for details. Defaults to "none".
#' @param case_handling A character string specifying how to handle the case of
#'                      the cleaned URL. Defaults to "lower".
#'   \itemize{
#'     \item{"keep": Preserves the original casing.}
#'     \item{"lower": (Default) Converts the cleaned URL to lowercase.}
#'     \item{"upper": Converts the cleaned URL to uppercase.}
#'   }
#' @param trailing_slash_handling A character string specifying how to handle
#'   trailing slashes in the path component of the cleaned URL. Defaults to "none".
#'   \itemize{
#'     \item{"none": (Default) No specific handling is applied. Path remains as is after initial parsing.}
#'     \item{"keep": Ensures a trailing slash. If a path exists and doesn't end with one, it's added. If path is just "/", it's kept.}
#'     \item{"strip": Removes a trailing slash if present, unless the path is solely "/".}
#'   }
#' @param subdomain_levels_to_keep An integer or NULL. Determines how many levels of subdomains are kept,
#'   in addition to any 'www.' prefix handled by `www_handling`.
#'   \itemize{
#'     \item{`NULL`: (Default) No specific subdomain stripping is performed beyond `www_handling`.}
#'     \item{`0`: All subdomains are stripped. If `www_handling` preserved or added 'www.',
#'          it remains (e.g., 'www.sub.example.com' becomes 'www.example.com'; 'sub.example.com' becomes 'example.com').}
#'     \item{`N > 0`: Keeps up to N levels of subdomains, counted from right-to-left (closest to the registered domain),
#'          in addition to any 'www.' prefix. E.g., if N=1, 'three.two.one.example.com' becomes 'one.example.com';
#'          'www.three.two.one.example.com' (post www_handling) becomes 'www.one.example.com'.}
#'   }
#' @return A character vector of cleaned URLs.
#' @export
#' @examples
#' get_clean_url("Example.COM/Path") # Default lower, default no slash change
#' get_clean_url("Example.COM/Path", case_handling = "keep", trailing_slash_handling = "keep")
#' get_clean_url("Example.COM/Path/", case_handling = "upper", trailing_slash_handling = "strip")
#' get_clean_url("http://example.com", www_handling = "strip")
get_clean_url <- function(url,
                          protocol_handling = "keep",
                          www_handling = "none",
                          case_handling = "lower",
                          trailing_slash_handling = "none",
                          subdomain_levels_to_keep = NULL) {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all",
                             case_handling = case_handling,
                             trailing_slash_handling = trailing_slash_handling,
                             subdomain_levels_to_keep = subdomain_levels_to_keep)
    if (is.null(parsed)) return(NA_character_)
    parsed$clean_url %||% NA_character_
  }, character(1))
}

# Internal helper to encode hostnames using IDNA (Punycode)
# Accepts encode_fn for testability and fallback
.normalize_and_punycode <- function(host, encode_fn = urltools::puny_encode) {
  if (is.na(host) || !nzchar(host)) return(host)
  host <- stringi::stri_trans_nfc(host)  # Normalize Unicode

  # Skip punycode encoding if ASCII-only
  # Using stri_enc_isascii which is more direct for this check
  if (stringi::stri_enc_isascii(host)) return(host)

  tryCatch(
    encode_fn(host),
    error = function(e) NA_character_
  )
}

# Internal helper to decode Punycode domain parts to Unicode
.punycode_to_unicode <- function(domain_puny) {
  if (is.na(domain_puny)) return(NA_character_)
  # cat(paste0("DEBUG .punycode_to_unicode input: ", domain_puny, "\n")) # Debug Line 1
  parts_puny <- strsplit(domain_puny, "\\.")[[1]]

  decoded_labels_unicode <- vapply(parts_puny, function(part_puny) {
    # cat(paste0("  DEBUG part_puny: ", part_puny, "\n")) # Debug Line 2
    decoded_label <- NULL
    # Known workarounds for urltools::puny_decode issues with specific Punycode TLDs
    if (part_puny == "xn--qxam") { # Punycode for .ελ
      decoded_label <- "\u03b5\u03bb"  # "ελ"
    } else if (part_puny == "xn--p1ai") { # Punycode for .рф
      decoded_label <- "\u0440\u0444"  # "рф"
    } else if (part_puny == "xn--wgbh1c") { # Punycode for .مصر
      decoded_label <- "\u0645\u0635\u0631"  # "مصر"
    } else if (part_puny == "xn--node") { # Punycode for .გე
      decoded_label <- "\u10d2\u10d4"  # "გე"
      # Add other problematic Punycode TLDs here if discovered
      # For example, if xn--o3cw4h (.ไทย) was problematic in isolation:
      # } else if (part_puny == "xn--o3cw4h") {
      #   decoded_label <- "\u0e44\u0e17\u0e22"  # "ไทย"
    } else {
      # Default to urltools::puny_decode for other cases
      decoded_label_from_tool <- tryCatch(urltools::puny_decode(part_puny), error = function(e) part_puny)
      # cat(paste0("    DEBUG decoded_label_from_tool ('", part_puny, "'): ", decoded_label_from_tool, "\n")) # Debug Line 3
      decoded_label <- decoded_label_from_tool
    }

    # Ensure the string is valid UTF-8.
    # iconv will attempt to convert from UTF-8 to UTF-8.
    # sub="" will try to discard non-translatable characters/invalid byte sequences.
    sane_label <- iconv(decoded_label, from = "UTF-8", to = "UTF-8", sub = "")
    # cat(paste0("    DEBUG sane_label for ('", part_puny, "') (original decoded: '", decoded_label, "'): ", sane_label, "\n")) # Debug Line 4

    # If iconv failed and returned NA (e.g., for a completely malformed string)
    # and the original decoded_label wasn't NA, we might fallback or return a placeholder.
    # For now, if sane_label is NA, it means iconv found it irreparable.
    if (is.na(sane_label)) {
      # cat(paste0("    DEBUG sane_label was NA for part_puny: ", part_puny, " (decoded_label was: ", decoded_label,") - returning empty string\n")) # Debug Line 5
      return("")
    }

    return(sane_label)
  }, character(1))

  result_unicode <- paste(decoded_labels_unicode, collapse = ".")
  # cat(paste0("  DEBUG .punycode_to_unicode result: ", result_unicode, "\n")) # Debug Line 6
  return(result_unicode)
}

# Internal helper to convert host to ASCII using Punycode, fallback if urltools is unavailable
.to_ascii <- function(host) {
  if (is.na(host) || !nzchar(host)) return(host)
  if (all(charToRaw(host) <= as.raw(127))) return(host)  # ASCII-only shortcut
  if (requireNamespace("urltools", quietly = TRUE)) {
    tryCatch(urltools::puny_encode(host), error = function(e) host)
  } else {
    host
  }
}

#' Get domain names
#'
#' Extracts the registered domain name from a URL (e.g., "example.com").
#' Relies on the Public Suffix List.
#'
#' @param url A character vector of URLs.
#' @param protocol_handling See \code{\link{safe_parse_url}}. Defaults to "keep".
#' @param www_handling See \code{\link{safe_parse_url}}. Defaults to "none".
#' @param subdomain_levels_to_keep An integer or NULL. Determines how many levels of subdomains are kept,
#'   in addition to any 'www.' prefix handled by `www_handling`.
#'   \itemize{
#'     \item{`NULL`: (Default) No specific subdomain stripping is performed beyond `www_handling`.}
#'     \item{`0`: All subdomains are stripped. If `www_handling` preserved or added 'www.',
#'          it remains (e.g., 'www.sub.example.com' becomes 'www.example.com'; 'sub.example.com' becomes 'example.com').}
#'     \item{`N > 0`: Keeps up to N levels of subdomains, counted from right-to-left (closest to the registered domain),
#'          in addition to any 'www.' prefix. E.g., if N=1, 'three.two.one.example.com' becomes 'one.example.com';
#'          'www.three.two.one.example.com' (post www_handling) becomes 'www.one.example.com'.}
#'   }
#' @return A character vector of domain names.
#' @export
#' @examples
#' get_domain("http://www.example.co.uk/path")
get_domain <- function(url,
                       protocol_handling = "keep",
                       www_handling = "none",
                       subdomain_levels_to_keep = NULL) {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all",
                             case_handling = "lower",
                             trailing_slash_handling = "none",
                             subdomain_levels_to_keep = subdomain_levels_to_keep)
    if (is.null(parsed)) return(NA_character_)
    parsed$domain %||% NA_character_
  }, character(1))
}

#' Get URL schemes
#'
#' Extracts the scheme (protocol) of a URL.
#'
#' @param url A character vector of URLs.
#' @param protocol_handling See \code{\link{safe_parse_url}}. Defaults to "keep".
#' @return A character vector of URL schemes.
#' @export
#' @examples
#' get_scheme("https://example.com")
get_scheme <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = "none", # Consistent with other get_* funcs
                             tld_source = "all",
                             case_handling = "lower",
                             trailing_slash_handling = "none",
                             subdomain_levels_to_keep = NULL) # Scheme not affected by subdomain levels
    if (is.null(parsed)) return(NA_character_)
    parsed$scheme %||% NA_character_
  }, character(1))
}

#' Get URL hosts
#'
#' Extracts the host component of a URL.
#'
#' @param url A character vector of URLs.
#' @param protocol_handling See \code{\link{safe_parse_url}}. Defaults to "keep".
#' @param www_handling See \code{\link{safe_parse_url}}. Defaults to "none".
#' @param subdomain_levels_to_keep An integer or NULL. Determines how many levels of subdomains are kept,
#'   in addition to any 'www.' prefix handled by `www_handling`.
#'   \itemize{
#'     \item{`NULL`: (Default) No specific subdomain stripping is performed beyond `www_handling`.}
#'     \item{`0`: All subdomains are stripped. If `www_handling` preserved or added 'www.',
#'          it remains (e.g., 'www.sub.example.com' becomes 'www.example.com'; 'sub.example.com' becomes 'example.com').}
#'     \item{`N > 0`: Keeps up to N levels of subdomains, counted from right-to-left (closest to the registered domain),
#'          in addition to any 'www.' prefix. E.g., if N=1, 'three.two.one.example.com' becomes 'one.example.com';
#'          'www.three.two.one.example.com' (post www_handling) becomes 'www.one.example.com'.}
#'   }
#' @return A character vector of URL hosts.
#' @export
#' @examples
#' get_host("http://sub.example.com:8080")
get_host <- function(url,
                     protocol_handling = "keep",
                     www_handling = "none",
                     subdomain_levels_to_keep = NULL) {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all",
                             case_handling = "lower", # host is case-sensitive by spec, but often normalized
                             trailing_slash_handling = "none",
                             subdomain_levels_to_keep = subdomain_levels_to_keep)
    if (is.null(parsed)) return(NA_character_)
    parsed$host %||% NA_character_
  }, character(1))
}

#' Get URL paths
#'
#' Extracts the path component of a URL.
#'
#' @param url A character vector of URLs.
#' @param protocol_handling See \code{\link{safe_parse_url}}. Defaults to "keep".
#' @return A character vector of URL paths.
#' @export
#' @examples
#' get_path("http://example.com/some/path?query=1")
get_path <- function(url, protocol_handling = "keep") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = "none", # Consistent with other get_* funcs
                             tld_source = "all",
                             case_handling = "lower",
                             trailing_slash_handling = "none",
                             subdomain_levels_to_keep = NULL) # Path not affected by subdomain levels
    if (is.null(parsed)) return(NA_character_)
    parsed$path %||% NA_character_
  }, character(1))
}

#' Extract the top-level domain (TLD) from a URL
#'
#' @param url A character vector of URLs.
#' @param source Which TLD source to use: "all", "icann", or "private".
#' @return A character vector of TLDs.
#' @export
#' @examples
#' get_tld("example.com")
get_tld <- function(url, source = c("all", "private", "icann")) {
  source <- match.arg(source)
  tlds <- switch(
    source,
    all     = tld_all,
    private = tld_private,
    icann   = tld_icann
  )

  vapply(url, function(u) {
    if (is.na(u) || !nzchar(u)) return(NA_character_)

    # MODIFIED: Call the dedicated legacy host extraction path
    host <- ._legacy_get_host_for_get_tld_only(u)
    if (is.na(host) || !nzchar(host)) return(NA_character_)

    # Normalize to NFC and lowercase
    host_normalized <- stringi::stri_trans_nfc(host)
    host_lower <- stringi::stri_trans_tolower(host_normalized)

    # Puny-encode only if non-ASCII (original logic's conditional punycode)
    # Using .to_ascii which encapsulates this conditional punycode logic robustly
    encoded_host <- .to_ascii(host_lower) # .to_ascii handles NA and no-op for ASCII

    if (is.na(encoded_host) || !nzchar(encoded_host)) return(NA_character_) # Added safety check

    # Split domain parts
    parts <- strsplit(encoded_host, "\\.")[[1]]
    n <- length(parts)

    # Match from longest candidate to shortest (original loop)
    if (n > 1) { # seq_len(n - 1) is meaningful only if n > 1
        for (i in seq_len(n - 1)) {
            candidate <- paste(parts[i:n], collapse = ".")
            if (candidate %in% tlds) {
                # Original used iconv, .punycode_to_unicode should suffice and handle encoding.
                return(.punycode_to_unicode(candidate))
            }
        }
    }

    # Fallback: try last part only
    if (n > 0) { # Ensure there's at least one part
        last <- parts[n]
        if (last %in% tlds) {
            return(.punycode_to_unicode(last))
        }
    }

    NA_character_
  }, character(1))
}

# Internal helper to derive registered domain using Public Suffix List
# Expects hostname_encoded to be already NFC-normalized, lowercased, and Punycode-encoded if non-ASCII.
.get_registered_domain <- function(hostname) {
  parts <- strsplit(hostname, "\\.")[[1]]
  parts <- parts[nzchar(parts)]  # Remove empty labels (e.g., from ".com")
  n <- length(parts)
  if (n < 2)
    return(NA_character_)

  # Extract rule types from PSL
  exception_rules <- sub("^!", "", grep("^!", psl_clean, value = TRUE))
  wildcard_rules  <- sub("^\\*\\.", "", grep("^\\*\\.", psl_clean, value = TRUE))
  normal_rules    <- setdiff(psl_clean, c(paste0("!", exception_rules),
                                          paste0("*.", wildcard_rules)))

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

  for (i in seq_len(n)) { # i is the start index of a suffix candidate in parts
    candidate_suffix_str <- paste(parts[i:n], collapse = ".")
    num_parts_in_candidate_suffix <- n - i + 1

    # 1. Check if candidate_suffix_str is an exact match in normal_rules
    if (candidate_suffix_str %in% normal_rules) {
      if (num_parts_in_candidate_suffix > best_match_len) {
        best_match_len <- num_parts_in_candidate_suffix
      }
    }

    # 2. Check if candidate_suffix_str matches a wildcard rule.
    #    A wildcard rule means "*." + some_suffix_in_wildcard_rules.
    #    So, candidate_suffix_str must be of form "label." + some_suffix_in_wildcard_rules.
    #    e.g., candidate_suffix_str = "somerset.sch.uk", and "sch.uk" is in wildcard_rules.
    if (num_parts_in_candidate_suffix > 1) { # Must have at least "label.wildcard_part"
      # The part that would be in wildcard_rules is parts[(i+1):n]
      potential_wildcard_match_part <- paste(parts[(i+1):n], collapse=".")
      if (potential_wildcard_match_part %in% wildcard_rules) {
        # If it matches, then the current candidate_suffix_str (parts[i:n]) is the public suffix.
        if (num_parts_in_candidate_suffix > best_match_len) {
          best_match_len <- num_parts_in_candidate_suffix
        }
      }
    }
  }

  if (best_match_len == 0) {
    # No PSL rule matched. Fallback to TLD being last part, domain is penultimate + last.
    # Assumes n >= 2 at this point due to the function's initial `if (n < 2)` check.
    return(paste(utils::tail(parts, 2), collapse = "."))
  }

  # If hostname is a public suffix itself (or shorter than the matched public suffix),
  # it cannot be a "registered domain" by the eTLD+1 definition.
  if (n <= best_match_len) {
    return(NA_character_)
  }

  # Standard case: n > best_match_len
  # The registered domain is the public suffix (best_match_len parts)
  # plus one additional label to the left.
  # So, we take (best_match_len + 1) parts from the end of the hostname.
  # The starting index for these parts is (n - (best_match_len + 1) + 1) = (n - best_match_len).
  return(paste(parts[(n - best_match_len):n], collapse = "."))
}

# Internal helper using the exact original get_tld logic for TLD extraction
._extract_tld_original_logic <- function(host_to_process, current_tld_list) {
  if (is.na(host_to_process) || !nzchar(host_to_process)) {
      return(NA_character_)
  }

  normalized_host <- stringi::stri_trans_nfc(stringi::stri_trans_tolower(host_to_process))
  encoded_host <- .normalize_and_punycode(normalized_host)

  if (is.na(encoded_host) || !nzchar(encoded_host)) {
      return(NA_character_)
  }

    parts <- strsplit(encoded_host, "\\.")[[1]]
    n <- length(parts)

  if (n > 1) {
    for (i in seq_len(n - 1)) { # Checks suffixes of length n down to 2
      candidate <- paste(parts[i:n], collapse = ".") # Candidate is Punycode
      if (candidate %in% current_tld_list) { # current_tld_list should also be Punycode
        return(.punycode_to_unicode(candidate)) # Decodes matched Punycode TLD
      }
    }
  }

  if (n > 0) { # Fallback to last part
    last_candidate <- parts[n] # Punycode
    if (last_candidate %in% current_tld_list) { # Check against Punycode TLD list
      return(.punycode_to_unicode(last_candidate))
    }
  }

  return(NA_character_)
}

# Original safe_parse_url, renamed for use by legacy get_host
._legacy_safe_parse_url_for_get_tld_only <- function(url,
                           protocol_handling = c("keep", "none", "strip", "http", "https")) {
  protocol_handling <- match.arg(protocol_handling)

  if (is.na(url) || !is.character(url) || url == "")
    return(NULL)

  allowed_prefixes <- c("http://", "https://", "ftp://", "ftps://")
  has_valid_prefix <- any(startsWith(tolower(url), allowed_prefixes))
  looks_like_protocol <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:", url)

  if (looks_like_protocol && !has_valid_prefix)
    return(NULL)

  if (!looks_like_protocol) {
    url <- paste0("http://", url)
  }

  result <- tryCatch(curl::curl_parse_url(url), error = function(e) NULL)

  if (is.null(result))
    return(NULL)

  result$scheme <- switch(
    protocol_handling,
    none  = if (!has_valid_prefix) NA_character_ else result$scheme,
    strip = NA_character_,
    http  = "http",
    https = "https",
    keep  = result$scheme
  )
  result
}

# Original get_host, renamed and modified to call legacy safe_parse_url
._legacy_get_host_for_get_tld_only <- function(url, protocol_handling = "keep") {
  # protocol_handling is part of the original signature, match.arg if needed by legacy_safe_parse_url
  # Forcing "keep" as it was the implicit original behavior for get_host feeding get_tld
  protocol_handling <- "keep"
  parsed <- ._legacy_safe_parse_url_for_get_tld_only(url, protocol_handling)
  if (is.null(parsed))
    return(NA_character_)
  parsed$host %||% NA_character_
}

