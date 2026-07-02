# URL parsing: public parse functions, scalar wrapper, and impl orchestration.

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
#'   and `www[number]` prefixes in the host. Defaults to "none".
#'   \itemize{
#'     \item{"none": (Default) Leaves the host's www prefix (or lack thereof)
#'     untouched.}
#'     \item{"strip": Removes any "www." or `www[number].` prefix.}
#'     \item{"keep": Ensures the host starts with "www.". If it has
#'     `www[number].`, it's normalized to "www.". If no www prefix, "www." is
#'     added. An empty input host remains empty.}
#'     \item{"if_no_subdomain": If the host is a bare registered domain (e.g.,
#'     "example.com"), "www." is added. If the host already has a "www." or
#'     `www[number].` prefix, it is normalized to "www." (e.g.,
#'     "www1.example.com" becomes "www.example.com"; "www1.sub.example.com"
#'     becomes "www.sub.example.com"). If a non-www subdomain exists (e.g.,
#'     "sub.example.com" or the normalized "www.sub.example.com"), the host is
#'     not further altered. An empty input host remains empty.}
#'   }
#' @param tld_source Which TLD source to use for TLD extraction: "all", "icann",
#'   or "private". Defaults to "all".
#' @param case_handling A character string specifying how to handle the case of
#'                      the cleaned URL. Defaults to "lower_host", the
#'                      RFC 3986 §6.2.2.1 normalization (scheme and host are
#'                      case-insensitive and folded to lowercase; the path is
#'                      case-sensitive and preserved).
#'   \itemize{
#'     \item{"lower_host": (Default) Lowercases scheme and host only; the path
#'     keeps its original casing.}
#'     \item{"keep": Preserves casing of the reconstructed URL.}
#'     \item{"lower": Converts the cleaned URL to lowercase.}
#'     \item{"upper": Converts the cleaned URL to uppercase.}
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
#'     \item{"keep": Leave the path percent-encoding untouched (the path is
#'     preserved byte-for-byte as written in the URL, so `%2F` stays `%2F`
#'     rather than decoding into a path-separating `/`).}
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
#'     \item `query`: The raw query string as written in the URL, preserved
#'     byte-for-byte (e.g., "name=value"); not percent-decoded.
#'     \item `fragment`: The fragment identifier as written in the URL
#'     (e.g., "section"); not percent-decoded.
#'     \item `user`: The user name for authentication, as written in the URL;
#'     not percent-decoded.
#'     \item `password`: The password for authentication, as written in the
#'     URL; not percent-decoded.
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
                             "lower_host", "keep", "lower", "upper"
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

  # Validate and normalize options once (match.arg + subdomain check)
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

  ._safe_parse_url_scalar(url, opts)
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
                              "lower_host", "keep", "lower", "upper"
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
  # Validate and normalize options once (match.arg + subdomain check)
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

  if (length(url) == 0) {
    return(.spu_empty_result())
  }

  # Coerce to a character vector to parse and, separately, the original_url
  # column. A non-character element (or a non-scalar list element) is not
  # parseable -- it becomes NA for the engine (an "error" row), exactly as the
  # scalar pipeline returned NULL for it -- but its original_url is still the
  # coerced input string. This preserves the historical column semantics without
  # a per-URL loop.
  if (is.character(url)) {
    # Fast path: a plain character vector needs no per-element coercion --
    # every element is already a length-1 character (NA included), so both the
    # original_url column and the parse input equal the input verbatim. This is
    # the overwhelmingly common shape and keeps the warm/duplicate path O(1) per
    # element instead of two vapply() closures over the whole vector.
    original_url_vec <- url
    parse_input <- url
  } else {
    urls_list <- as.list(url)
    original_url_vec <- vapply(urls_list, .spu_coerce_original, character(1))
    parse_input <- vapply(
      urls_list,
      function(u) if (is.character(u) && length(u) == 1L) u else NA_character_,
      character(1)
    )
  }

  # De-duplicate + memoize at the unique-URL level: unique parse inputs are
  # parsed once (with cross-call reuse via the full_parse cache), then expanded
  # back to every row. Duplicates cost only the match() inside
  # ._parse_urls_cached(). original_url is restored per row afterwards so it
  # reflects the input element even for duplicates / NA / non-character rows.
  field_names <- vapply(.spu_result_fields, function(f) f$name, character(1))
  cols <- ._parse_urls_cached(parse_input, opts)[field_names]
  cols$original_url <- original_url_vec
  cols$stringsAsFactors <- FALSE
  do.call(data.frame, cols)
}

# Empty (zero-row) result data.frame with the canonical column set/types.
.spu_empty_result <- function() {
  cols <- lapply(.spu_result_fields, function(f) f$template[0])
  names(cols) <- vapply(.spu_result_fields, function(f) f$name, character(1))
  cols$stringsAsFactors <- FALSE
  do.call(data.frame, cols)
}

# Coerce one input element to the scalar `original_url` value: NA for missing
# or non-scalar inputs, the string itself for character, else as.character().
.spu_coerce_original <- function(u) {
  is_missing_url <- is.null(u) ||
    length(u) == 0 ||
    (is.atomic(u) && length(u) == 1 && is.na(u))
  if (is_missing_url) {
    return(NA_character_)
  }
  if (is.character(u)) {
    return(u)
  }
  if (is.atomic(u) && length(u) == 1) {
    return(as.character(u))
  }
  NA_character_
}

# Validate and normalize the parsing options shared by safe_parse_url() and
# safe_parse_urls(). Runs every match.arg() once and validates
# subdomain_levels_to_keep, returning a list keyed by option name. The formal
# defaults below are the single source of the allowed choices, so match.arg()
# resolves them here rather than in each public function.
# Allowed choices for the parsing options. Hoisted to constants so the
# .parse_options() signature does not need multi-line default vectors;
# match.arg() resolves each formal's default to the matching constant.
.opt_protocol_handling <- c("keep", "none", "strip", "http", "https")
.opt_www_handling <- c("none", "strip", "keep", "if_no_subdomain")
.opt_tld_source <- c("all", "private", "icann")
.opt_case_handling <- c("lower_host", "keep", "lower", "upper")
.opt_trailing_slash_handling <- c("none", "keep", "strip")
.opt_index_page_handling <- c("keep", "strip")
.opt_path_normalization <- c("none", "collapse_slashes", "dot_segments", "both")
.opt_scheme_relative_handling <- c("keep", "http", "https", "error")
.opt_host_encoding <- c("keep", "idna", "unicode")
.opt_path_encoding <- c("keep", "encode", "decode")

.parse_options <- function(protocol_handling = .opt_protocol_handling,
                           www_handling = .opt_www_handling,
                           tld_source = .opt_tld_source,
                           case_handling = .opt_case_handling,
                           trailing_slash_handling =
                             .opt_trailing_slash_handling,
                           index_page_handling = .opt_index_page_handling,
                           path_normalization = .opt_path_normalization,
                           scheme_relative_handling =
                             .opt_scheme_relative_handling,
                           subdomain_levels_to_keep = NULL,
                           host_encoding = .opt_host_encoding,
                           path_encoding = .opt_path_encoding) {
  # match.arg first (matches the original error precedence), then validate
  # subdomain_levels_to_keep.
  opts <- list(
    protocol_handling = match.arg(protocol_handling),
    www_handling = match.arg(www_handling),
    tld_source = match.arg(tld_source),
    case_handling = match.arg(case_handling),
    trailing_slash_handling = match.arg(trailing_slash_handling),
    index_page_handling = match.arg(index_page_handling),
    path_normalization = match.arg(path_normalization),
    scheme_relative_handling = match.arg(scheme_relative_handling),
    host_encoding = match.arg(host_encoding),
    path_encoding = match.arg(path_encoding)
  )

  valid_subdomain_value <- is.numeric(subdomain_levels_to_keep) &&
    length(subdomain_levels_to_keep) == 1L &&
    !is.na(subdomain_levels_to_keep) &&
    subdomain_levels_to_keep >= 0 &&
    subdomain_levels_to_keep %% 1 == 0
  invalid_subdomain_levels <-
    !is.null(subdomain_levels_to_keep) && !valid_subdomain_value
  if (invalid_subdomain_levels) {
    stop(
      "subdomain_levels_to_keep must be NULL or a non-negative integer.",
      call. = FALSE
    )
  }
  # Single-bracket list() assignment keeps the element when the value is NULL
  # ($<- NULL would drop it).
  opts["subdomain_levels_to_keep"] <- list(subdomain_levels_to_keep)
  opts
}

# Build memoization cache keys for a vector of URLs from validated options.
# Single source of the field set, order, separator, and Unicode escaping so the
# key format cannot drift across call sites. The scalar opts recycle across the
# `urls` vector in one paste(), and one stri_escape_unicode() escapes them all.
.parse_cache_keys <- function(urls, opts) {
  subdomain_key <- if (is.null(opts$subdomain_levels_to_keep)) {
    "NULL"
  } else {
    as.character(opts$subdomain_levels_to_keep)
  }
  cache_key <- paste(urls, opts$protocol_handling, opts$www_handling,
    opts$tld_source, opts$case_handling, opts$trailing_slash_handling,
    opts$index_page_handling, opts$path_normalization,
    opts$scheme_relative_handling, subdomain_key,
    opts$host_encoding, opts$path_encoding,
    sep = "\x1F"
  )
  stringi::stri_escape_unicode(enc2utf8(cache_key))
}

# Scalar cache key: the n = 1 case of .parse_cache_keys(), retained for
# compatibility with existing call sites and tests.
.parse_cache_key <- function(url, opts) {
  .parse_cache_keys(url, opts)
}

# Internal scalar helper that handles caching and calls the shared cached
# vector path (n = 1). Receives the validated `opts` list (from
# .parse_options()) and reuses it directly. Returns NULL for a NULL-equivalent
# row (invalid / rejected / curl failure) or the row as a named list
# (byte-identical to the historical .assemble_parse_result() output).
._safe_parse_url_scalar <- function(url, opts) {
  # Early return for invalid input (never cached, matching the historical
  # scalar pipeline that returned NULL for these before touching the cache).
  if (is.na(url) || !is.character(url) || url == "") {
    return(NULL)
  }

  # Scalar parse is the n = 1 case of the shared cached vector path, so the
  # per-URL cache behavior comes from the same code as safe_parse_urls().
  cols <- ._parse_urls_cached(url, opts)
  if (attr(cols, "null_row")[1L]) {
    return(NULL)
  }
  field_names <- vapply(.spu_result_fields, function(f) f$name, character(1))
  lapply(cols[field_names], function(column) column[[1L]])
}

# Shared cached vector parse path for safe_parse_url() (n = 1) and
# safe_parse_urls(). `parse_input` is a character vector (NA marks elements the
# caller could not coerce to a parseable string). Returns the list of 14 columns
# named by .spu_result_fields (canonical order) plus a `null_row` logical
# attribute -- the rows the scalar pipeline would have returned NULL for.
#
# The full_parse cache is keyed and stored at the UNIQUE-URL level: unique parse
# inputs are looked up in one batch, misses parsed once by the vector engine and
# stored, then all rows are expanded via match(). Duplicate rows cost only the
# match(). Each cache value is either an unnamed length-14 list of the row's
# fields (canonical .spu_result_fields order) or NULL for a null row, so
# NULL-caching semantics are preserved and hit reconstruction stays a builtin
# `[[` gather (no per-element closure). NA / "" inputs are treated as null rows
# and are NEVER cached (mirroring the scalar early return), so cache-entry
# counts reflect real unique URLs only.
._parse_urls_cached <- function(parse_input, opts) {
  field_names <- vapply(.spu_result_fields, function(f) f$name, character(1))
  n_fields <- length(.spu_result_fields)
  uniq <- unique(parse_input)
  m <- length(uniq)

  # Only a real URL string (non-NA, non-empty) participates in the cache.
  cacheable <- !is.na(uniq) & nzchar(uniq)
  keys <- rep(NA_character_, m)
  if (any(cacheable)) {
    keys[cacheable] <- .parse_cache_keys(uniq[cacheable], opts)
  }

  # Batch cache lookup over the cacheable keys.
  cached <- rep(list(.rurl_cache_sentinel), m)
  if (any(cacheable)) {
    cached[cacheable] <- .cache_get_many("full_parse", keys[cacheable])
  }
  hit <- !vapply(cached, identical, logical(1), .rurl_cache_sentinel)

  # Unique-level columns, initialized to the null-row defaults.
  cols_uniq <- lapply(.spu_result_fields, function(f) rep(f$default, m))
  names(cols_uniq) <- field_names
  null_uniq <- rep(TRUE, m)

  # Fill cache hits. A hit value is an unnamed length-14 list (populated row) or
  # NULL (cached null row -- keep the defaults). Non-null rows are gathered a
  # field at a time with the builtin `[[`, so reconstruction avoids a closure
  # per hit.
  if (any(hit)) {
    hit_idx <- which(hit)
    is_null_hit <- vapply(cached[hit_idx], is.null, logical(1))
    null_uniq[hit_idx] <- is_null_hit
    nn_idx <- hit_idx[!is_null_hit]
    if (length(nn_idx) > 0L) {
      nn_vals <- cached[nn_idx]
      for (j in seq_len(n_fields)) {
        cols_uniq[[j]][nn_idx] <- vapply(
          nn_vals, `[[`, .spu_result_fields[[j]]$template, j
        )
      }
    }
  }

  # Parse the misses (includes NA / "" inputs, which the engine turns into null
  # rows) and populate their columns.
  need_idx <- which(!hit)
  if (length(need_idx) > 0L) {
    engine_cols <- ._parse_urls_vec(uniq[need_idx], opts)
    null_uniq[need_idx] <- attr(engine_cols, "null_row")
    for (nm in field_names) {
      cols_uniq[[nm]][need_idx] <- engine_cols[[nm]]
    }

    # Store the freshly parsed, cacheable rows: the unnamed length-14 field
    # list, or NULL for a null row. NA / "" are never cached.
    store_idx <- need_idx[cacheable[need_idx]]
    if (length(store_idx) > 0L) {
      store_vals <- lapply(store_idx, function(i) {
        if (null_uniq[i]) {
          NULL
        } else {
          unname(lapply(cols_uniq, `[[`, i))
        }
      })
      .cache_set_many("full_parse", keys[store_idx], store_vals)
    }
  }

  # Expand the unique rows back to every input position.
  pos <- match(parse_input, uniq)
  cols <- lapply(cols_uniq, function(col) col[pos])
  names(cols) <- field_names
  attr(cols, "null_row") <- null_uniq[pos]
  cols
}

# Internal vector engine: parse a character vector of URLs into a list of the
# 14 columns named by .spu_result_fields, plus a `null_row` attribute marking
# the rows the scalar pipeline would have returned NULL for (invalid input,
# phase-1 rejection, or curl failure). Options are the validated `opts` list
# (from .parse_options()); every per-phase switch/if is selected once here and
# the phase helpers run over whole vectors. All pslr/punycode traffic is batched
# to one call per phase (over unique hosts). This is the single production parse
# path for both safe_parse_url() and safe_parse_urls(); ._safe_parse_url_impl()
# below is the equivalent scalar orchestrator kept for its unit coverage.
._parse_urls_vec <- function(urls, opts) {
  n <- length(urls)

  # Input-level validity: parseable only if a non-NA, non-empty character
  # scalar (callers map non-character input to NA before the engine).
  valid <- !is.na(urls) & nzchar(urls)

  # Phase 1: scheme detection and curl-input preparation.
  prep <- .prepare_urls_for_curl_vec(
    urls, opts$protocol_handling, opts$scheme_relative_handling
  )

  # Phase 2: parse with curl (the only per-URL loop) over the surviving rows.
  parseable <- valid & !prep$rejected
  parsed_list <- vector("list", n)
  parse_idx <- which(parseable)
  if (length(parse_idx) > 0L) {
    parsed_list[parse_idx] <- lapply(
      prep$url_to_parse[parse_idx], .parse_with_curl
    )
  }
  curl_ok <- parseable & !vapply(parsed_list, is.null, logical(1))
  null_row <- !curl_ok

  # Pull raw components into columns (mirrors .extract_raw_components() and the
  # port/fragment/user/password fields of .assemble_parse_result(), using `$`
  # exactly as the scalar code does).
  raw_scheme <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$scheme %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_host <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$host %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_path <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$path %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_query <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$query %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_fragment <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$fragment %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_user <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$user %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_password <- vapply(parsed_list, function(p) {
    if (is.null(p)) NA_character_ else p$password %||% NA_character_
  }, character(1), USE.NAMES = FALSE)
  raw_port <- vapply(parsed_list, function(p) {
    if (is.null(p)) {
      NA_integer_
    } else {
      suppressWarnings(as.integer(p$port %||% NA_integer_))
    }
  }, integer(1), USE.NAMES = FALSE)

  # Phase 3: path normalization.
  path_final <- .normalize_path_vec(
    raw_path, opts$path_encoding, opts$path_normalization,
    opts$index_page_handling, opts$trailing_slash_handling
  )

  # Phase 4: final scheme per protocol policy.
  final_scheme <- .derive_final_scheme_vec(
    opts$protocol_handling, prep$looks_like_protocol, raw_scheme
  )

  # Phase 5: IP host detection.
  is_ip_host <- .detect_ip_host_vec(raw_host)

  # Phase 6: www-prefix policy.
  final_host <- .apply_www_policy_vec(raw_host, opts$www_handling, is_ip_host)

  # Phase 7: registered-domain and TLD derivation (batched pslr calls).
  domain_tld <- .derive_domain_tld_vec(
    final_host, is_ip_host, opts$tld_source, opts$host_encoding
  )
  domain <- domain_tld$domain
  tld <- domain_tld$tld

  # Phase 8: subdomain-level policy.
  final_host <- .apply_subdomain_policy_vec(
    final_host, domain, opts$subdomain_levels_to_keep, is_ip_host
  )

  # Phase 9: host encoding (idna / unicode).
  host_for_clean <- .apply_host_encoding_vec(
    final_host, opts$host_encoding, is_ip_host
  )

  # Phase 10: case policy applied to host, path, and scheme.
  cased <- .apply_case_policy_vec(
    host_for_clean, path_final, final_scheme, opts$case_handling
  )

  # Phase 11: clean URL reconstruction.
  clean_url <- .build_clean_url_vec(
    cased$scheme, cased$host, cased$path, opts$trailing_slash_handling
  )

  # Phase 12: parse-status assignment.
  parse_status <- .derive_parse_status_vec(
    curl_ok = curl_ok,
    final_host = final_host,
    is_ip_host = is_ip_host,
    tld = tld,
    domain = domain,
    protocol_handling = opts$protocol_handling,
    final_scheme = final_scheme,
    looks_like_protocol = prep$looks_like_protocol,
    original_has_allowed_scheme = prep$original_has_allowed_scheme,
    is_scheme_relative = prep$is_scheme_relative,
    scheme_relative_handling = opts$scheme_relative_handling
  )

  # Phase 13: assemble the 14 typed columns.
  cols <- .assemble_parse_result_vec(
    original_url = urls,
    scheme_output = cased$scheme,
    host_output = cased$host,
    port = raw_port,
    path_output = cased$path,
    raw_query = raw_query,
    fragment = raw_fragment,
    user = raw_user,
    password = raw_password,
    domain = domain,
    tld = tld,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status,
    is_scheme_relative = prep$is_scheme_relative,
    scheme_relative_handling = opts$scheme_relative_handling
  )

  # Rows the scalar pipeline returned NULL for become all-default "error" rows
  # (like .spu_empty_row()); original_url is left as the input. Non-null rows
  # keep every computed column -- including the unsupported-scheme rows whose
  # parse_status is "error" but whose other components are populated.
  if (any(null_row)) {
    for (f in .spu_result_fields) {
      if (f$name == "original_url") {
        next
      }
      cols[[f$name]][null_row] <- f$default
    }
  }

  attr(cols, "null_row") <- null_row
  cols
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

  # host_encoding/path_encoding are already validated upstream by
  # .parse_options(); no re-validation here.

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
    protocol_handling, prep$looks_like_protocol, raw$scheme
  )

  # Phase 5: IP host detection
  is_ip_host <- .detect_ip_host(raw_host)

  # Phase 6: www prefix policy
  final_host <- .apply_www_policy(raw_host, www_handling, is_ip_host)

  # Phase 7: registered-domain and TLD derivation
  domain_tld <- .derive_domain_tld(
    final_host, is_ip_host, tld_source, host_encoding
  )
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
    looks_like_protocol = prep$looks_like_protocol,
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
