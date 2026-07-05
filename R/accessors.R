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
                               path_encoding = "keep",
                               query_handling = "drop",
                               params_keep = NULL,
                               params_drop = NULL,
                               params_case_sensitive = FALSE,
                               sort_params = FALSE,
                               empty_param_handling = "keep",
                               decode_plus = FALSE,
                               port_handling = "exclude",
                               url_standard = NULL) {
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
    path_encoding = path_encoding,
    query_handling = query_handling,
    params_keep = params_keep,
    params_drop = params_drop,
    params_case_sensitive = params_case_sensitive,
    sort_params = sort_params,
    empty_param_handling = empty_param_handling,
    decode_plus = decode_plus,
    port_handling = port_handling,
    url_standard = url_standard
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
#' @return A character vector with the parse status of each URL: one of
#'   \code{"ok"}, \code{"ok-ftp"}, \code{"ok-scheme-relative"},
#'   \code{"warning-no-tld"}, \code{"warning-invalid-tld"},
#'   \code{"warning-public-suffix"}, \code{"warning-userinfo"} (a scheme-less
#'   input carrying userinfo, e.g. \code{"user@example.com"}), or
#'   \code{"error"}. See \code{\link{safe_parse_url}} for the full semantics.
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
                             source = c("all", "private", "icann"),
                             url_standard = NULL) {
  source <- match.arg(source)
  url_standard <- .validate_url_standard(url_standard)
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
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    url_standard = url_standard
  )
}

#' Get cleaned URLs
#'
#' This function returns the cleaned version of the URLs after applying
#' protocol, www, case, and trailing slash handling rules. By default the result
#' is a normalized canonical key composed of scheme, host, and path only; port
#' is dropped (\code{port_handling = "exclude"}), and fragment/userinfo are
#' always excluded (use \code{\link{get_port}}, \code{\link{get_fragment}}, or
#' \code{\link{get_userinfo}} for those).
#'
#' The query string is dropped by default (\code{query_handling = "drop"}), so
#' the historical scheme/host/path output is byte-identical. Pass
#' \code{query_handling = "keep"}, \code{"filter"}, or \code{"allow"} (with the
#' companion \code{params_*} / \code{sort_params} / \code{empty_param_handling}
#' / \code{decode_plus} arguments) to retain a shaped query on the cleaned URL;
#' the engine is the same one \code{\link{safe_parse_url}} and
#' \code{\link{get_query}} use, so
#' \code{get_clean_url(u, query_handling = "filter")} equals
#' \code{safe_parse_url(u, query_handling = "filter")$clean_url}.
#'
#' The port is included only when \code{port_handling != "exclude"}; see
#' \code{\link{safe_parse_url}} for the full \code{port_handling} semantics,
#' including how \code{url_standard = "whatwg"} elides a default port even
#' under \code{port_handling = "keep"}.
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
#' # Query dropped by default (byte-identical to earlier releases):
#' get_clean_url("http://example.com/p?utm_source=nl&id=42")
#' # -> "http://example.com/p"
#' # Strip trackers, keep contentful params:
#' get_clean_url(
#'   "http://example.com/p?utm_source=nl&id=42",
#'   query_handling = "filter"
#' )
#' # -> "http://example.com/p?id=42"
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
                          path_encoding = "keep",
                          query_handling = c("drop", "filter", "allow", "keep"),
                          params_keep = NULL,
                          params_drop = NULL,
                          params_case_sensitive = FALSE,
                          sort_params = FALSE,
                          empty_param_handling = c("keep", "drop"),
                          decode_plus = FALSE,
                          port_handling = c(
                            "exclude", "keep", "strip_default", "strip_all"
                          ),
                          url_standard = NULL) {
  source <- match.arg(source)
  query_handling <- match.arg(query_handling)
  empty_param_handling <- match.arg(empty_param_handling)
  port_handling <- match.arg(port_handling)
  url_standard <- .validate_url_standard(url_standard)
  # get_clean_url()'s governed formals default to scalars, so match.arg() needs
  # the explicit choice sets to resolve/validate a supplied value.
  .check_url_standard_conflicts(url_standard, .governed_supplied(
    path_encoding = if (missing(path_encoding)) {
      NULL
    } else {
      match.arg(path_encoding, .opt_path_encoding)
    },
    path_normalization = if (missing(path_normalization)) {
      NULL
    } else {
      match.arg(path_normalization, .opt_path_normalization)
    },
    case_handling = if (missing(case_handling)) {
      NULL
    } else {
      match.arg(case_handling, .opt_case_handling)
    }
  ))
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
    path_encoding = path_encoding,
    query_handling = query_handling,
    params_keep = params_keep,
    params_drop = params_drop,
    params_case_sensitive = params_case_sensitive,
    sort_params = sort_params,
    empty_param_handling = empty_param_handling,
    decode_plus = decode_plus,
    port_handling = port_handling,
    url_standard = url_standard
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
                       host_encoding = c("keep", "idna", "unicode"),
                       url_standard = NULL) {
  source <- match.arg(source)
  host_encoding <- match.arg(host_encoding)
  url_standard <- .validate_url_standard(url_standard)
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
    host_encoding = host_encoding,
    url_standard = url_standard
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
                     host_encoding = c("keep", "idna", "unicode"),
                     url_standard = NULL) {
  source <- match.arg(source)
  host_encoding <- match.arg(host_encoding)
  url_standard <- .validate_url_standard(url_standard)
  # missing() must be read before match.arg() reassigns case_handling below.
  .check_url_standard_conflicts(url_standard, .governed_supplied(
    case_handling =
      if (missing(case_handling)) NULL else match.arg(case_handling)
  ))
  case_handling <- match.arg(case_handling)
  .extract_from_urls(url, "host",
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
    case_handling = case_handling,
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding,
    url_standard = url_standard
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
  path_encoding = c("keep", "encode", "decode"),
  url_standard = NULL
) {
  # url_standard validation + conflict check must read missing() BEFORE the
  # match.arg() reassignments below (assignment can make missing() FALSE).
  url_standard <- .validate_url_standard(url_standard)
  .check_url_standard_conflicts(url_standard, .governed_supplied(
    path_encoding =
      if (missing(path_encoding)) NULL else match.arg(path_encoding),
    path_normalization =
      if (missing(path_normalization)) NULL else match.arg(path_normalization),
    case_handling =
      if (missing(case_handling)) NULL else match.arg(case_handling)
  ))
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
    path_encoding = path_encoding,
    url_standard = url_standard
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
#' Set FALSE to obtain the query as written: the raw query for the default
#' `query_handling = "keep"`, or the canonical re-encoded form (uppercase hex,
#' `%20`, `%26`/`%3D`) once any filtering is requested.
#' @details
#' The filtering arguments (`query_handling`, `params_keep`, `params_drop`,
#' `params_case_sensitive`, `sort_params`, `empty_param_handling`,
#' `decode_plus`) share the engine used by \code{\link{get_clean_url}}, but
#' default to `query_handling = "keep"` here: an accessor returns the query as
#' found unless you ask it to filter. When no filtering or reordering is
#' requested (the default profile), the output is byte-for-byte identical to
#' earlier releases; once you opt in, the surviving params are selected first
#' and only then rendered per `format`/`decode`.
#' @return A character vector (format="string") or list (format="list").
#' @export
#' @examples
#' get_query("http://example.com/path?a=1&b=2")
#' get_query("http://example.com/path?a=1&b=2", format = "list")
#' # Drop trackers, keep contentful params:
#' get_query(
#'   "http://example.com/?utm_source=nl&id=42",
#'   query_handling = "filter"
#' )
#' # Canonical (re-encoded) form:
#' get_query(
#'   "http://example.com/?a=1%262",
#'   query_handling = "keep", decode = FALSE
#' )
get_query <- function(url,
                      protocol_handling = "keep",
                      format = c("string", "list"),
                      decode = TRUE,
                      query_handling = c("keep", "drop", "filter", "allow"),
                      params_keep = NULL,
                      params_drop = NULL,
                      params_case_sensitive = FALSE,
                      sort_params = FALSE,
                      empty_param_handling = c("keep", "drop"),
                      decode_plus = FALSE) {
  format <- match.arg(format)
  query_handling <- match.arg(query_handling)
  empty_param_handling <- match.arg(empty_param_handling)

  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }

  # Fast path: when no filtering or reordering is requested the engine is an
  # identity transform, so reproduce the historical output byte-for-byte. This
  # is the only path that preserves the faithful raw-query edges the canonical
  # engine deliberately normalizes away (a bare `?flag` staying `flag` not
  # `flag=`, a trailing `?a=b&` keeping its `&`). params_keep/params_drop are
  # ignored under query_handling = "keep", so they do not disqualify it.
  engine_active <- !(identical(query_handling, "keep") &&
    !sort_params &&
    identical(empty_param_handling, "keep") &&
    !decode_plus)

  if (!engine_active) {
    if (format == "string") {
      raw <- .extract_from_urls(url, "query",
        protocol_handling = protocol_handling
      )
      # The parse result carries a RAW (percent-encoded) query so the list form
      # can split safely; the string form historically returns the decoded
      # query, so decode here to keep that output unchanged.
      if (decode) {
        # Per-element decode keeps the historical byte-for-byte fallback (a
        # query that fails to unescape is returned raw); USE.NAMES = FALSE so
        # the result stays unnamed like every other accessor.
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

    # format = "list": pull the raw query column in one engine pass (case =
    # "keep" matches the historical per-element profile so cache keys are
    # unchanged), then parse each element. Null rows carry NA, and
    # ._parse_query_string(NA) returns list() -- exactly what the old
    # per-element NULL branch returned.
    raw <- .extract_from_urls(url, "query",
      protocol_handling = protocol_handling,
      case_handling = "keep"
    )
    return(lapply(raw, ._parse_query_string, decode = decode))
  }

  # Engine path: filtering and/or reordering is in effect. Pull the faithful raw
  # query column (the `query` field is never mutated by the engine), then select
  # the surviving pairs before rendering. NA rows (no query) render as NA /
  # list(); a present query whose params are all filtered out renders as "" /
  # list(), matching the empty-query contract.
  raw <- .extract_from_urls(url, "query", protocol_handling = protocol_handling)
  na_row <- is.na(raw)

  surviving <- function(query) {
    ._query_surviving_pairs(
      query, query_handling, params_keep, params_drop, params_case_sensitive,
      sort_params, empty_param_handling, decode_plus, "builtin"
    )
  }

  if (format == "string") {
    out <- vapply(seq_along(raw), function(i) {
      if (na_row[i]) {
        return(NA_character_)
      }
      if (!decode) {
        # Canonical re-encoded form -- exactly the clean_url query renderer.
        return(._filter_query_params(
          raw[i],
          query_handling = query_handling,
          params_keep = params_keep,
          params_drop = params_drop,
          params_case_sensitive = params_case_sensitive,
          sort_params = sort_params,
          empty_param_handling = empty_param_handling,
          decode_plus = decode_plus
        ))
      }
      if (identical(query_handling, "drop")) {
        return("")
      }
      sp <- surviving(raw[i])
      if (is.null(sp)) {
        return("")
      }
      # Decoded, readable form: opaque tokens are already passed through raw by
      # .decode_query_tokens(), so `%zz` stays `%zz`.
      paste(paste0(sp$dec_key, "=", sp$dec_value), collapse = "&")
    }, character(1), USE.NAMES = FALSE)
    return(out)
  }

  # format = "list": grouped named list of the surviving pairs. decode selects
  # the decoded or raw spelling; grouping matches ._parse_query_string exactly.
  lapply(seq_along(raw), function(i) {
    if (na_row[i] || identical(query_handling, "drop")) {
      return(list())
    }
    sp <- surviving(raw[i])
    if (is.null(sp)) {
      return(list())
    }
    if (decode) {
      .group_query_pairs(sp$dec_key, sp$dec_value)
    } else {
      .group_query_pairs(sp$raw_key, sp$raw_value)
    }
  })
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
                          host_encoding = c("keep", "idna", "unicode"),
                          url_standard = NULL) {
  source <- match.arg(source)
  format <- match.arg(format)
  host_encoding <- match.arg(host_encoding)
  url_standard <- .validate_url_standard(url_standard)

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
    host_encoding = host_encoding,
    url_standard = url_standard
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
                    host_encoding = c("keep", "idna", "unicode"),
                    url_standard = NULL) {
  source <- match.arg(source)
  host_encoding <- match.arg(host_encoding)
  url_standard <- .validate_url_standard(url_standard)
  # case_handling is immaterial to the tld output (the TLD is derived from the
  # normalized host, independent of the clean_url case policy); "lower" is
  # retained as an explicit, stable profile rather than aligned to the
  # "lower_host" default to avoid a cache-key/output change (RURL-actrnerd).
  .extract_from_urls(url, "tld",
    tld_source = source,
    case_handling = "lower",
    host_encoding = host_encoding,
    url_standard = url_standard
  )
}

#' Classify the host of each URL under a standard profile
#'
#' Companion helper for the \code{url_standard} selector: reports the host
#' \emph{type} of each URL as exactly one of \code{"domain"}, \code{"ipv4"},
#' \code{"ipv6"}, \code{"reg-name"}, or \code{"missing"}. Unlike a raw host
#' string, \code{host_type} is a function of \emph{both} the host and the
#' selected standard: the numeric host \code{2130706433} is a \code{"reg-name"}
#' under \code{"rfc3986"} but an \code{"ipv4"} address under \code{"whatwg"}.
#' Callers reading the result must therefore know which selector produced it.
#'
#' The metadata is intentionally exposed through this helper rather than as a
#' column on \code{\link{safe_parse_urls}} or a field on
#' \code{\link{safe_parse_url}}, so that passing no selector leaves every
#' existing function's output shape unchanged.
#'
#' @param url A character vector of URLs.
#' @param url_standard Standard profile governing host interpretation:
#'   \code{NULL} (default; no classification, returns \code{NA}),
#'   \code{"rfc3986"}, or \code{"whatwg"}.
#' @return A character vector the same length as \code{url}, each element one of
#'   the \code{host_type} tokens above, or \code{NA} when no selector is given.
#' @seealso \code{\link{get_url_diagnostics}}, \code{\link{safe_parse_url}}
#' @export
#' @examples
#' get_host_type("http://example.com/", url_standard = "rfc3986")
#' get_host_type("http://2130706433/", url_standard = "whatwg")
get_host_type <- function(url, url_standard = NULL) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  opts <- .parse_options(url_standard = url_standard)
  ._url_metadata_vec(url, opts)$host_type
}

#' Report non-fatal diagnostics for each URL under a standard profile
#'
#' Companion helper for the \code{url_standard} selector: reports the
#' non-fatal validation/safety \emph{facts} rurl observed while parsing each
#' URL (for example an IPv4 host written in a numeric or non-decimal shorthand,
#' or a path segment carrying an encoded reserved byte). Diagnostics are facts,
#' not policy: they are emitted keyed to host/path \emph{shape} in both
#' standard modes so a security-sensitive consumer can reject a footgun URL
#' regardless of which selector it chose, while a link-graph consumer can ignore
#' them. See \code{vignette} / the package NEWS for the full token vocabulary.
#'
#' A single URL can carry several diagnostics, so the return shape is not a
#' plain scalar-per-URL vector (see \emph{Value}). \code{parse_status} stays
#' coarse; diagnostics are never encoded into it.
#'
#' @param url A character vector of URLs.
#' @param url_standard Standard profile governing interpretation: \code{NULL}
#'   (default; no diagnostics), \code{"rfc3986"}, or \code{"whatwg"}.
#' @return For a length-1 \code{url}, a character vector of zero or more
#'   diagnostic tokens for that URL. For a length-n \code{url} (including
#'   \code{n == 0}), a list of length n whose i-th element is the character
#'   vector of that URL's tokens (\code{character(0)} when it has none).
#' @seealso \code{\link{get_host_type}}, \code{\link{safe_parse_url}}
#' @export
#' @examples
#' get_url_diagnostics("http://example.com/", url_standard = "rfc3986")
#' get_url_diagnostics(
#'   c("http://example.com/", "http://2130706433/"),
#'   url_standard = "whatwg"
#' )
get_url_diagnostics <- function(url, url_standard = NULL) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  opts <- .parse_options(url_standard = url_standard)
  diagnostics <- ._url_metadata_vec(url, opts)$diagnostics
  # length-1 url -> the bare token vector; length-n (incl. 0) -> list of n.
  if (length(url) == 1L) {
    return(diagnostics[[1L]])
  }
  diagnostics
}

#' Classify the scheme of each URL as WHATWG special or not
#'
#' Companion helper for the \code{url_standard} selector: reports whether each
#' URL's resolved scheme is a WHATWG \dQuote{special scheme} (\code{"special"}),
#' one rurl supports but WHATWG does not treat specially (\code{"non-special"}),
#' or absent/unparseable (\code{"missing-or-error"} -- an unsupported scheme, a
#' scheme-relative URL under the default \code{scheme_relative_handling =
#' "keep"}, or an input that failed to parse at all).
#'
#' Unlike \code{\link{get_host_type}}, the classification itself does not vary
#' between \code{"rfc3986"} and \code{"whatwg"} -- \dQuote{special scheme} is a
#' WHATWG concept describing a fixed property of the scheme string, not
#' something RFC 3986 redefines. \code{url_standard} instead gates whether the
#' metadata is exposed at all, mirroring \code{get_host_type()}'s contract:
#' pass \code{NULL} (the default) and every element is \code{NA}.
#'
#' Within rurl's allowlist (\code{http}/\code{https}/\code{ftp}/\code{ftps}),
#' \code{http}, \code{https}, and \code{ftp} are WHATWG special schemes;
#' \code{ftps} (FTP-over-TLS, rurl's own addition) is not. This is metadata
#' only -- it does not add \code{ws}/\code{wss}/\code{file} to rurl's allowed
#' schemes and does not change what \code{\link{safe_parse_url}} accepts.
#'
#' @param url A character vector of URLs.
#' @param url_standard Standard profile gating the classification: \code{NULL}
#'   (default; no classification, returns \code{NA}), \code{"rfc3986"}, or
#'   \code{"whatwg"}.
#' @return A character vector the same length as \code{url}, each element one
#'   of \code{"special"}, \code{"non-special"}, or \code{"missing-or-error"},
#'   or \code{NA} when no selector is given.
#' @seealso \code{\link{get_host_type}}, \code{\link{get_scheme}}
#' @export
#' @examples
#' get_scheme_class("http://example.com/", url_standard = "whatwg")
#' get_scheme_class("ftps://example.com/", url_standard = "whatwg")
#' get_scheme_class("//example.com/path", url_standard = "whatwg")
get_scheme_class <- function(url, url_standard = NULL) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  url_standard <- .validate_url_standard(url_standard)
  if (is.null(url_standard)) {
    return(rep(NA_character_, length(url)))
  }
  scheme <- get_scheme(url)
  out <- rep("missing-or-error", length(url))
  out[!is.na(scheme) & !(scheme %in% .WHATWG_SPECIAL_SCHEMES)] <- "non-special"
  out[scheme %in% .WHATWG_SPECIAL_SCHEMES] <- "special"
  out
}

#' Summarize query parameters across a set of URLs
#'
#' Tabulates which query parameters appear across a vector of URLs and what
#' values they take, with a `would_drop` column previewing what
#' \code{query_handling = "filter"} would remove. Useful for auditing a URL set
#' before choosing a cleaning policy: see the trackers before you strip them.
#'
#' Parameter names are grouped \emph{faithfully} (case-sensitively and by their
#' decoded spelling), so `utm_source` and `UTM_SOURCE` are reported as separate
#' rows. The `would_drop` preview, by contrast, honours `params_case_sensitive`:
#' with the default `params_case_sensitive = FALSE`, `UTM_SOURCE` matches the
#' built-in denylist and shows `would_drop = TRUE`; set it to `TRUE` and the
#' upper-case spelling no longer matches. The raw `query` field is only read,
#' never mutated.
#'
#' @param urls A character vector of URLs.
#' @param level One of "param" (default) for one row per distinct parameter
#'   name, or "value" for one row per distinct (parameter, value) pair.
#' @inheritParams safe_parse_url
#' @return A flat (long) `data.frame`. For `level = "param"`: `param`, `n`
#'   (total occurrences), `n_urls` (distinct URLs containing the param),
#'   `example_value`, `example_url`, `would_drop`. For `level = "value"`:
#'   `param`, `value`, `n`, `n_urls`, `example_url`, `would_drop`. The
#'   `example_*` columns and the param-level `would_drop` reflect the
#'   first-seen occurrence (deterministic given input order). Returns a
#'   zero-row `data.frame` with the level's columns when no URL carries a query.
#' @export
#' @examples
#' urls <- c(
#'   "http://example.com/?utm_source=nl&id=42",
#'   "http://example.com/watch?v=abc&utm_source=x",
#'   "http://example.com/?id=99"
#' )
#' query_param_summary(urls)
#' query_param_summary(urls, level = "value")
#' # Preview a custom policy:
#' query_param_summary(urls, params_drop = "id")
query_param_summary <- function(urls,
                                level = c("param", "value"),
                                params_keep = NULL,
                                params_drop = NULL,
                                params_case_sensitive = FALSE,
                                empty_param_handling = c("keep", "drop"),
                                decode_plus = FALSE) {
  level <- match.arg(level)
  empty_param_handling <- match.arg(empty_param_handling)

  if (!is.character(urls)) {
    stop(
      "`urls` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }

  # Read the faithful raw query for every URL in one engine pass, then decompose
  # each into decoded ordered pairs. would_drop is a FILTER-mode preview: the
  # same ._select_params() the cleaner uses, so denylist u params_drop minus
  # params_keep, plus empty-dropping, all honouring params_case_sensitive.
  raw <- .extract_from_urls(urls, "query", protocol_handling = "keep")

  per_url <- lapply(seq_along(raw), function(i) {
    query <- raw[i]
    if (is.na(query)) {
      return(NULL)
    }
    pairs <- ._parse_query_pairs(query)
    if (length(pairs$key) == 0L) {
      return(NULL)
    }
    key_opaque <- .token_is_opaque(pairs$key)
    val_opaque <- .token_is_opaque(pairs$value)
    dec_key <- .decode_query_tokens(pairs$key, key_opaque, FALSE, decode_plus)
    dec_val <- .decode_query_tokens(pairs$value, val_opaque, TRUE, decode_plus)
    surv <- ._select_params(
      dec_key, dec_val, "filter", params_keep, params_drop,
      params_case_sensitive, empty_param_handling, "builtin"
    )
    list(
      url_idx = rep.int(i, length(dec_key)),
      param = dec_key, value = dec_val, would_drop = !surv
    )
  })
  per_url <- per_url[!vapply(per_url, is.null, logical(1))]

  cols_param <- c("param", "n", "n_urls", "example_value", "example_url",
    "would_drop")
  cols_value <- c("param", "value", "n", "n_urls", "example_url", "would_drop")
  if (length(per_url) == 0L) {
    empty_cols <- if (level == "param") cols_param else cols_value
    return(.empty_query_summary(empty_cols))
  }

  url_idx <- unlist(lapply(per_url, `[[`, "url_idx"), use.names = FALSE)
  param <- unlist(lapply(per_url, `[[`, "param"), use.names = FALSE)
  value <- unlist(lapply(per_url, `[[`, "value"), use.names = FALSE)
  would_drop <- unlist(lapply(per_url, `[[`, "would_drop"), use.names = FALSE)

  # Group occurrences preserving first-seen order. At value level the group key
  # combines the param and value FACTOR CODES (pure integers, so the "."
  # separator can never collide) to key each distinct (param, value) pair.
  if (level == "param") {
    group_key <- param
  } else {
    group_key <- paste(
      as.integer(factor(param)), as.integer(factor(value)),
      sep = "."
    )
  }
  levels_seen <- group_key[!duplicated(group_key)]
  groups <- split(seq_along(group_key), factor(group_key, levels = levels_seen))

  first <- vapply(groups, `[`, integer(1), 1L, USE.NAMES = FALSE)
  n <- unname(lengths(groups))
  n_urls <- vapply(
    groups, function(ix) length(unique(url_idx[ix])), integer(1),
    USE.NAMES = FALSE
  )

  out <- data.frame(
    param = param[first],
    n = as.integer(n),
    n_urls = n_urls,
    example_url = urls[url_idx[first]],
    would_drop = would_drop[first],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  if (level == "param") {
    out$example_value <- value[first]
    return(out[, cols_param])
  }
  out$value <- value[first]
  out[, cols_value]
}

# A zero-row query-summary data.frame with the given columns typed to match a
# populated result (character/integer/logical), so callers get a stable shape
# even when no URL carries a query.
.empty_query_summary <- function(cols) {
  proto <- list(
    param = character(0), value = character(0), n = integer(0),
    n_urls = integer(0), example_value = character(0),
    example_url = character(0), would_drop = logical(0)
  )
  data.frame(proto[cols], stringsAsFactors = FALSE)
}
