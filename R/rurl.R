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
#'   }
#' @param tld_source Which TLD source to use for TLD extraction: "all", "icann",
#'   or "private". Defaults to "all".
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
#'     \item `clean_url`: The URL reconstructed from scheme, host, and path after processing. NA if host is empty/NA.
#'     \item `parse_status`: Character string indicating parsing outcome ("ok", "ok-ftp", "error", "warning-no-tld").
#'   }
#'   Returns `NULL` if the URL is fundamentally unparseable (e.g., NA, empty) or uses a disallowed scheme.
#' @keywords internal
#' @export
#' @examples
#' safe_parse_url("http://www.example.com/path?q=1#frag", protocol_handling = "keep")
#' safe_parse_url("example.com", protocol_handling = "none", www_handling = "keep")
#' safe_parse_url("192.168.1.1/test")
#' safe_parse_url("ftp://user:pass@ftp.example.co.uk:21/file.txt")
safe_parse_url <- function(url,
                           protocol_handling = c("keep", "none", "strip", "http", "https"),
                           www_handling = c("none", "strip", "keep"),
                           tld_source = c("all", "private", "icann")) {
  protocol_handling <- match.arg(protocol_handling)
  www_handling <- match.arg(www_handling)
  tld_source <- match.arg(tld_source)

  original_input_url <- url

  if (is.na(url) || !is.character(url) || url == "") {
    return(NULL)
  }

  allowed_prefixes <- c("http://", "https://", "ftp://", "ftps://")
  original_url_lower <- tolower(url)
  original_has_allowed_scheme <- any(startsWith(original_url_lower, allowed_prefixes))
  original_looks_like_protocol <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*:", url)

  if (original_looks_like_protocol && !original_has_allowed_scheme) {
    return(NULL)
  }

  url_to_parse <- url
  if (!original_looks_like_protocol) {
    url_to_parse <- paste0("http://", url)
  }

  parsed_curl <- tryCatch(curl::curl_parse_url(url_to_parse), error = function(e) NULL)

  if (is.null(parsed_curl)) {
    return(NULL)
  }

  raw_scheme <- parsed_curl$scheme %||% NA_character_
  raw_host <- parsed_curl$host %||% NA_character_
  raw_path <- parsed_curl$path %||% NA_character_

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
    ipv6 <- grepl("^\\[?[0-9a-fA-F:]+\\]?$", raw_host) && grepl(":", raw_host)
    ipv4 || ipv6
  }
  final_host <- raw_host

  if (!is_ip_host && !is.na(raw_host) && raw_host != "") {
    if (www_handling == "strip") {
      final_host <- sub("^(www[0-9]*\\.)(.*)", "\\2", raw_host, ignore.case = TRUE)
    } else if (www_handling == "keep") {
      # Ensure www. prefix, normalizing www[n]. -> www., adding if absent. Empty host stays empty.
      if (raw_host == "") { 
        final_host <- ""
      } else {
        current_host_lower <- tolower(raw_host)
        if (grepl("^www[0-9]*\\.", current_host_lower)) { # Starts with www. or www[n].
          bare_host_part <- sub("^www[0-9]*\\.(.*)", "\\1", raw_host, ignore.case = TRUE)
          final_host <- paste0("www.", bare_host_part)
        } else { # Does not start with any www prefix
          final_host <- paste0("www.", raw_host)
        }
      }
    }
  }

  domain <- NA_character_
  tld <- NA_character_

  if (!is_ip_host && !is.na(final_host) && final_host != "") {
    host_for_derivation <- stringi::stri_trans_nfc(tolower(final_host))
    encoded_host_for_derivation <- .normalize_and_punycode(host_for_derivation)

    if (!is.na(encoded_host_for_derivation) && encoded_host_for_derivation != "") {
      derived_domain_encoded <- .get_registered_domain(encoded_host_for_derivation)
      if (!is.na(derived_domain_encoded)) {
        domain <- .punycode_to_unicode(derived_domain_encoded)
      }

      selected_tlds <- switch(
        tld_source,
        all = tld_all,
        private = tld_private,
        icann = tld_icann
      )
      host_parts <- strsplit(encoded_host_for_derivation, "\\.")[[1]]
      n_parts <- length(host_parts)
      for (i in seq_len(n_parts)) { 
        candidate_tld <- paste(host_parts[i:n_parts], collapse = ".")
        if (candidate_tld %in% selected_tlds) {
          tld <- .punycode_to_unicode(candidate_tld)
          break 
        }
      }
    }
  }

  clean_url <- NA_character_
  if (!is.na(final_host) && final_host != "") {
    scheme_part <- if (!is.na(final_scheme)) paste0(final_scheme, "://") else ""
    clean_url <- paste0(scheme_part, final_host, raw_path)
  }

  parse_status <- "error" 
  if (!is.null(parsed_curl)) {
    current_scheme_lower <- tolower(final_scheme %||% "")
    if (current_scheme_lower %in% c("ftp", "ftps")) {
      parse_status <- "ok-ftp"
    } else if (current_scheme_lower %in% c("http", "https")) {
      if (is.na(final_host) || final_host == "" || (!is_ip_host && !grepl("\\.", final_host))) {
        parse_status <- "warning-no-tld"
      } else {
        parse_status <- "ok"
      }
    }
  }
  if(is.null(parsed_curl) || (original_looks_like_protocol && !original_has_allowed_scheme)) {
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
#' @return A character vector with the parse status of each URL.
#' @export
#' @examples
#' get_parse_status(c("http://example.com", "ftp://example.com", "mailto:user@example.com"))
#' get_parse_status(c("http://example.com", "not-a-url"))
get_parse_status <- function(url,
                             protocol_handling = "keep",
                             www_handling = "none") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all") 
    if (is.null(parsed)) return("error")
    parsed$parse_status %||% "error"
  }, character(1))
}

#' Get cleaned URLs
#'
#' This function returns the cleaned version of the URLs after applying
#' protocol and www handling rules.
#'
#' @param url A character vector containing URLs to be parsed.
#' @param protocol_handling A character string specifying how to handle protocols.
#'                          See \code{\link{safe_parse_url}} for details. Defaults to "keep".
#' @param www_handling A character string specifying how to handle "www" prefixes.
#'                     See \code{\link{safe_parse_url}} for details. Defaults to "none".
#' @return A character vector of cleaned URLs.
#' @export
#' @examples
#' get_clean_url("example.com")
#' get_clean_url("http://example.com", www_handling = "strip")
get_clean_url <- function(url,
                          protocol_handling = "keep",
                          www_handling = "none") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all") 
    if (is.null(parsed)) return(NA_character_)
    parsed$clean_url %||% NA_character_
  }, character(1))
}

# Internal helper to derive registered domain using Public Suffix List
# Expects hostname_encoded to be already NFC-normalized, lowercased, and Punycode-encoded if non-ASCII.
# This function's logic needs to be robust and match standard PSL algorithm.
# The psl_clean data should contain Punycode for IDN rules.
.get_registered_domain <- function(hostname_encoded) {
  if (is.na(hostname_encoded) || hostname_encoded == "") return(NA_character_)

  parts <- strsplit(hostname_encoded, "\\.")[[1]]
  n <- length(parts)
  if (n < 1) return(NA_character_) 

  exception_rules <- sub("^!", "", grep("^!", psl_clean, value = TRUE))
  wildcard_rules  <- sub("^\\*\\.", "", grep("^\\*\\.", psl_clean, value = TRUE)) 
  normal_rules    <- setdiff(psl_clean, c(paste0("!", exception_rules), paste0("*.", wildcard_rules)))

  # PSL matching logic according to the algorithm:
  # 1. If the hostname matches an exception rule (e.g., !example.com), then the registrable part is example.com itself.
  for (i in seq_len(n)) {
    candidate_domain <- paste(parts[i:n], collapse = ".")
    if (candidate_domain %in% exception_rules) {
      # This means candidate_domain is a registrable domain, not a public suffix.
      # The public suffix is one level above this, if such a level exists and is in PSL.
      # For www.parliament.uk, if !parliament.uk is the rule, parliament.uk is registrable.
      return(candidate_domain) 
    }
  }

  # 2. Match against the normal and wildcard rules to find the longest public suffix.
  longest_public_suffix_match <- ""
  for (i in seq_len(n)) {
    candidate_suffix <- paste(parts[i:n], collapse = ".")
    
    # Check normal rules
    if (candidate_suffix %in% normal_rules) {
      if (nchar(candidate_suffix) > nchar(longest_public_suffix_match)) {
        longest_public_suffix_match <- candidate_suffix
      }
    }
    
    # Check wildcard rules (e.g. *.ck means anything.ck -> .ck is public suffix, so ck is public suffix base)
    # If host is a.b.c, and candidate_suffix is b.c, and c is a wildcard_rule (*.c)
    # then b.c is the public suffix.
    if (i > 1) { # Wildcard must have something before it.
        # Current candidate_suffix is parts[i:n]. Base for wildcard is parts[(i+1):n]
        possible_wildcard_base <- paste(parts[(i+1):n], collapse=".")
        if (possible_wildcard_base %in% wildcard_rules) {
            # The candidate_suffix (parts[i:n]) is the actual public suffix under this rule.
            if (nchar(candidate_suffix) > nchar(longest_public_suffix_match)) {
                longest_public_suffix_match <- candidate_suffix
            }
        }
    }
  }

  if (longest_public_suffix_match == "") {
    if (n >= 2) return(paste(parts[(n - 1):n], collapse = "."))
    return(NA_character_)
  }

  if (longest_public_suffix_match == hostname_encoded) {
    return(NA_character_)
  }

  num_public_suffix_parts <- length(strsplit(longest_public_suffix_match, "\\.")[[1]])
  num_host_parts <- n
  
  # The registered domain is the public suffix plus one preceding label.
  # Index of that label is (num_host_parts - num_public_suffix_parts)
  required_label_index <- num_host_parts - num_public_suffix_parts
  
  if (required_label_index < 1) {
      # This implies the public suffix is as long or longer than the host, which shouldn't happen if longest_public_suffix_match != hostname_encoded
      return(NA_character_)
  }
  
  return(paste(parts[required_label_index:num_host_parts], collapse = "."))
}


#' Get domain names
#'
#' Extracts the registered domain name from a URL (e.g., "example.com").
#' Relies on the Public Suffix List.
#'
#' @param url A character vector of URLs.
#' @param protocol_handling See \code{\link{safe_parse_url}}. Defaults to "keep".
#' @param www_handling See \code{\link{safe_parse_url}}. Defaults to "none".
#' @return A character vector of domain names.
#' @export
#' @examples
#' get_domain("http://www.example.co.uk/path")
get_domain <- function(url,
                       protocol_handling = "keep",
                       www_handling = "none") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all") 
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
                             www_handling = "none", 
                             tld_source = "all")   
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
#' @return A character vector of URL hosts.
#' @export
#' @examples
#' get_host("http://sub.example.com:8080")
get_host <- function(url,
                     protocol_handling = "keep",
                     www_handling = "none") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = "all") 
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
                             www_handling = "none", 
                             tld_source = "all")   
    if (is.null(parsed)) return(NA_character_)
    parsed$path %||% NA_character_
  }, character(1))
}

# Internal helper to encode hostnames using IDNA (Punycode)
# Input host should be already lowercased. Normalizes to NFC first, then Punycodes if non-ASCII.
.normalize_and_punycode <- function(host) {
  if (is.na(host) || !nzchar(host)) return(host)
  host_nfc <- stringi::stri_trans_nfc(host)

  if (all(charToRaw(host_nfc) <= as.raw(127))) return(host_nfc)

  tryCatch(
    urltools::puny_encode(host_nfc),
    error = function(e) NA_character_
  )
}

# Internal helper to decode Punycode domain parts to Unicode
.punycode_to_unicode <- function(domain) {
  if (is.na(domain)) return(NA_character_)
  parts <- strsplit(domain, "\\.")[[1]]
  decoded_parts <- vapply(parts, function(part) {
    if (startsWith(tolower(part), "xn--")) {
      tryCatch(urltools::puny_decode(part), error = function(e) part)
    } else {
      part
    }
  }, character(1))
  paste(decoded_parts, collapse = ".")
}


#' Extract the top-level domain (TLD) from a URL
#'
#' @param url A character vector of URLs.
#' @param source Which TLD source to use: "all", "icann", or "private". Defaults to "all".
#' @param protocol_handling See \code{\link{safe_parse_url}}. Defaults to "keep".
#' @param www_handling See \code{\link{safe_parse_url}}. Defaults to "none".
#' @return A character vector of TLDs.
#' @export
#' @examples
#' get_tld("www.example.co.uk", source = "all")
get_tld <- function(url,
                    source = c("all", "private", "icann"),
                    protocol_handling = "keep",
                    www_handling = "none") {
  vapply(url, function(u) {
    parsed <- safe_parse_url(u,
                             protocol_handling = protocol_handling,
                             www_handling = www_handling,
                             tld_source = source) 
    if (is.null(parsed)) return(NA_character_)
    parsed$tld %||% NA_character_
  }, character(1))
}
