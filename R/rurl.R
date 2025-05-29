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

# Internal helper to encode hostnames using IDNA (Punycode)
# Accepts encode_fn for testability and fallback
.normalize_and_punycode <- function(host, encode_fn = urltools::puny_encode) {
  if (is.na(host) || !nzchar(host)) return(host)
  host <- stringi::stri_trans_nfc(host)  # Normalize Unicode

  # Skip punycode encoding if ASCII-only
  if (all(charToRaw(host) <= as.raw(127))) return(host)

  tryCatch(
    encode_fn(host),
    error = function(e) NA_character_
  )
}

# Internal helper to decode Punycode domain parts to Unicode
.punycode_to_unicode <- function(domain) {
  if (is.na(domain)) return(NA_character_)
  parts <- strsplit(domain, "\\.")[[1]]
  decoded_parts <- vapply(parts, function(part) {
    raw_decoded_label <- tryCatch(urltools::puny_decode(part), error = function(e) part)
    
    # Step 1: Immediately try to ensure the raw decoded label is valid UTF-8
    utf8_label <- iconv(raw_decoded_label, from = "", to = "UTF-8", sub = "byte")

    # Step 2: Clean the (now hopefully) UTF-8 label
    cleaned_label <- gsub("[^\\p{L}\\p{N}-]", "", utf8_label, perl = TRUE)
    
    return(cleaned_label)
  }, character(1))
  paste(decoded_parts, collapse = ".")
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
    host_lower <- tolower(host_normalized)

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

  for (i in seq_len(n)) {
    candidate <- paste(parts[i:n], collapse = ".")

    if (candidate %in% normal_rules && n - i + 1 > best_match_len) {
      best_match_len <- n - i + 1
    }

    # Wildcard match: candidate must match a known *.domain suffix
    if (i < n) {
      wildcard_candidate <- paste(parts[i:n], collapse = ".")
      if (wildcard_candidate %in% wildcard_rules &&
            n - i > best_match_len) {
        best_match_len <- n - i
      }
    }
  }

  if (best_match_len == 0) {
    return(paste(utils::tail(parts, 2), collapse = "."))
  }

  if (n <= best_match_len)
    return(NA_character_)

  # Return: label before suffix + suffix itself
  paste(parts[(n - best_match_len):n], collapse = ".")
}

# Internal helper using the exact original get_tld logic for TLD extraction
._extract_tld_original_logic <- function(host_to_process, current_tld_list) {
  if (is.na(host_to_process) || !nzchar(host_to_process)) {
    return(NA_character_)
  }

  normalized_host <- stringi::stri_trans_nfc(tolower(host_to_process))
  encoded_host <- .normalize_and_punycode(normalized_host)

  if (is.na(encoded_host) || !nzchar(encoded_host)) {
    return(NA_character_)
  }

  parts <- strsplit(encoded_host, "\\.")[[1]]
  n <- length(parts)

  if (n > 1) {
    for (i in seq_len(n - 1)) {
      candidate <- paste(parts[i:n], collapse = ".")
      if (candidate %in% current_tld_list) {
        decoded_tld <- .punycode_to_unicode(candidate)
        return(iconv(decoded_tld, from = "", to = "UTF-8", sub = "")) 
      }
    }
  }

  if (n > 0) {
    last_candidate <- parts[n]
    if (last_candidate %in% current_tld_list) {
      decoded_tld <- .punycode_to_unicode(last_candidate)
      return(iconv(decoded_tld, from = "", to = "UTF-8", sub = ""))
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

