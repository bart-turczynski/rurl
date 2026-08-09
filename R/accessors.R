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
                               scheme_policy = "infer",
                               scheme_acceptance = "web",
                               url_standard = NULL,
                               fixup_posture = "none",
                               engine = NULL,
                               profile_authorized = NULL) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  # Validate + normalize the option profile once (match.arg + subdomain check),
  # then parse the entire vector through the shared cached engine in one call.
  # `fixup_posture` / `profile_authorized` default to inert values and are only
  # non-default when a public named profile (ADR 0012 D6) sets them.
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
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard,
    fixup_posture = fixup_posture,
    engine = engine,
    profile_authorized = profile_authorized
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
#' The status is a single value collapsed from three independent facts --- URL
#' syntax, admission policy, and the Public Suffix List annotation --- so it is
#' a \emph{lossy} view of them. The guaranteed loss: a structural syntax
#' failure and a policy rejection both report \code{"error"}. Call
#' \code{\link{get_parse_verdicts}} when you need to tell those apart, or to
#' read the PSL result as a typed annotation state rather than as a warning.
#' Nothing here is deprecated --- the layered accessor is purely additive.
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
#' @seealso \code{\link{get_parse_verdicts}} for the unprojected layers
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
                             scheme_policy = c("infer", "require"),
                             scheme_acceptance = c("web", "general"),
                             url_standard = NULL) {
  source <- match.arg(source)
  url_standard <- .validate_url_standard(url_standard)
  # case_handling does not affect the parse_status output (it is derived from
  # parse success, host, domain and TLD, none of which depend on the clean_url
  # case policy). "lower" is kept here purely as an explicit, stable profile;
  # it is intentionally NOT aligned to .extract_from_urls()'s "lower_host"
  # default, so this accessor keeps its own memoization key rather than risk a
  # cache-key/output perturbation for a micro-optimization (RURL-actrnerd).
  .extract_from_urls(url, "parse_status",
    null_value = "error",
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
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
#' \code{\link{safe_parse_url}} for the full \code{port_handling} semantics.
#'
#' @section Cleaning is lossy by design:
#'
#' A cleaned URL is an SEO and deduplication product, not an identity function
#' and not a standard's serialization. It is deliberately \emph{lossy}: the
#' fragment and userinfo are always dropped, the query is dropped by default,
#' and the cleaning policies (\code{www_handling},
#' \code{trailing_slash_handling}, \code{index_page_handling}, subdomain
#' trimming, \code{port_handling}) exist precisely to collapse URLs that differ.
#' Distinct URLs therefore map to the same cleaned string, so this is never an
#' identity function and never round-trips back to its input.
#'
#' It is also \strong{not} the surface rurl's standards-conformance claims are
#' measured on. When identity or conformance is the goal, use a different
#' surface:
#'
#' \itemize{
#'   \item \code{\link{serialize_url}} --- the selected standard's own
#'   full-string serialization, credentials and fragment included. This is the
#'   substrate the conformance claims are measured on.
#'   \item \code{\link{get_url_key}} (with \code{\link{url_key_policy}}) and the
#'   \code{\link{url_join}} family --- resource identity for deduplicating,
#'   grouping, matching and joining. No cleaning option can reach a key byte.
#' }
#'
#' @section Path percent-encoding under a standard:
#'
#' \code{path_encoding} is a presentation knob and does not select a standard's
#' path identity; \code{url_standard} does. On the path axis the selector means:
#'
#' \itemize{
#'   \item \code{url_standard = "rfc3986"} applies RFC 3986 section 6.2.2.2
#'   percent-encoding normalization: a triplet encoding an \emph{unreserved}
#'   byte (\code{A-Z}, \code{a-z}, \code{0-9}, \code{-}, \code{.}, \code{_},
#'   \code{~}) is decoded to that byte, and every other triplet --- reserved
#'   bytes such as \code{\%2F}, \code{\%3F}, \code{\%23}, and non-ASCII bytes
#'   such as \code{\%C3} --- is left encoded with its hex digits uppercased.
#'   It is applied \emph{before} dot-segment removal, so an encoded dot segment
#'   (\code{\%2E} / \code{\%2E\%2E}) resolves like a literal one.
#'   \item \code{url_standard = "whatwg"} preserves existing percent-triplets
#'   byte-for-byte, hex case included, and resolves encoded dot segments
#'   without any decode.
#'   \item \code{url_standard = NULL} decodes nothing and canonicalizes triplet
#'   hex case to uppercase.
#' }
#'
#' That normalization is part of the path a URL denotes, and only
#' \code{path_encoding = "keep"} (the default) preserves it verbatim ---
#' \code{"encode"} and \code{"decode"} layer a presentation form on top and may
#' fold a reserved octet such as \code{\%2F} into a path-separating \code{/}.
#' See \code{vignette("url-standard")}.
#'
#' @param url A character vector containing URLs to be parsed.
#' @inheritParams safe_parse_url
#' @param source Which PSL source to use: "all", "private", or "icann".
#'   Subdomain trimming depends on which section is consulted, so pass
#'   \code{source = "icann"} to exclude private suffixes (e.g. github.io).
#' @return A character vector of cleaned URLs.
#' @seealso \code{\link{serialize_url}} for a standard's full-string
#'   serialization (the conformance-bearing surface), \code{\link{get_url_key}}
#'   and \code{\link{url_join}} for resource identity and joining, and
#'   \code{\link{safe_parse_url}} for the parsed components.
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
#' # Lossy by design: different URLs clean to the same string, and the cleaned
#' # string is not what a standard would serialize.
#' u <- c("http://u:pw@example.com/a#frag", "http://example.com/a?q=1")
#' get_clean_url(u)
#' # -> both "http://example.com/a"
#' serialize_url(u[1])
#' # The identity surface keeps them apart: the query is identity, the
#' # fragment and userinfo are not.
#' k <- get_url_key(u)
#' k[1] == k[2]
#' # -> FALSE
#' # RFC 3986 section 6.2.2.2 on the path: unreserved triplets decode,
#' # reserved ones stay encoded.
#' get_clean_url("http://example.com/a%7Eb%2Fc", url_standard = "rfc3986")
#' # -> "http://example.com/a~b%2Fc"
#' get_clean_url("http://example.com/a%7Eb%2Fc", url_standard = "whatwg")
#' # -> "http://example.com/a%7Eb%2Fc"
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
                          scheme_policy = c("infer", "require"),
                          scheme_acceptance = c("web", "general"),
                          url_standard = NULL,
                          engine = NULL,
                          profile = NULL) {
  # Capture query_handling's supplied-ness BEFORE match.arg() reassigns it (an
  # assignment to a formal clears its missing() status), so profile resolution
  # can tell an explicit query_handling from the default (seo governs it).
  query_handling_supplied <- !missing(query_handling)
  source <- match.arg(source)
  query_handling <- match.arg(query_handling)
  empty_param_handling <- match.arg(empty_param_handling)
  port_handling <- match.arg(port_handling)
  url_standard <- .validate_url_standard(url_standard)
  profile <- .validate_profile(profile)
  # get_clean_url()'s governed formals default to scalars, so match.arg() needs
  # the explicit choice sets to resolve/validate a supplied value.
  # `path_encoding` is orthogonal (ADR 0011): not passed here, never conflicts.
  # The conflict matrix is skipped on the profile path (see safe_parse_url()).
  if (is.null(profile)) {
    .check_url_standard_conflicts(url_standard, .governed_supplied(
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
  }
  extract_args <- list(url, "clean_url",
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
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard,
    engine = engine
  )
  if (!is.null(profile)) {
    extract_args <- .merge_profile_args(extract_args, .resolve_profile(
      profile,
      list(
        url_standard = url_standard,
        scheme_acceptance = if (missing(scheme_acceptance)) {
          NULL
        } else {
          match.arg(scheme_acceptance, .opt_scheme_acceptance)
        },
        scheme_policy = if (missing(scheme_policy)) {
          NULL
        } else {
          match.arg(scheme_policy, .opt_scheme_policy)
        },
        scheme_relative_handling = if (missing(scheme_relative_handling)) {
          NULL
        } else {
          match.arg(scheme_relative_handling, .opt_scheme_relative_handling)
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
        },
        protocol_handling = if (missing(protocol_handling)) {
          NULL
        } else {
          match.arg(protocol_handling, .opt_protocol_handling)
        },
        www_handling = if (missing(www_handling)) {
          NULL
        } else {
          match.arg(www_handling, .opt_www_handling)
        },
        trailing_slash_handling = if (missing(trailing_slash_handling)) {
          NULL
        } else {
          match.arg(trailing_slash_handling, .opt_trailing_slash_handling)
        },
        index_page_handling = if (missing(index_page_handling)) {
          NULL
        } else {
          match.arg(index_page_handling, .opt_index_page_handling)
        },
        host_encoding = if (missing(host_encoding)) {
          NULL
        } else {
          match.arg(host_encoding, .opt_host_encoding)
        },
        query_handling = if (query_handling_supplied) query_handling else NULL
      )
    ))
  }
  do.call(.extract_from_urls, extract_args)
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
                       scheme_policy = c("infer", "require"),
                       scheme_acceptance = c("web", "general"),
                       url_standard = NULL,
                       engine = NULL) {
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
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
    case_handling = "lower",
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding,
    url_standard = url_standard,
    engine = engine
  )
}

#' Get URL schemes
#'
#' Extracts the scheme (protocol) of a URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of URL schemes.
#' @details
#' By default (\code{scheme_acceptance = "web"}) only rurl's web-scheme
#' allowlist parses, so an opaque scheme such as \code{mailto:} or \code{tel:}
#' yields \code{NA}. Pass \code{scheme_acceptance = "general"} (which requires
#' an explicit \code{url_standard}) to run the general parser, under which those
#' schemes resolve and their scheme string is returned.
#' @export
#' @examples
#' get_scheme("https://example.com")
#' get_scheme(
#'   "mailto:jane@example.com",
#'   url_standard = "rfc3986", scheme_acceptance = "general"
#' )
get_scheme <- function(url, protocol_handling = "keep",
                       scheme_relative_handling = "keep",
                       scheme_policy = c("infer", "require"),
                       scheme_acceptance = c("web", "general"),
                       url_standard = NULL) {
  url_standard <- .validate_url_standard(url_standard)
  # Scheme is unaffected by www/subdomain/case handling, so "lower" is pinned
  # here as a stable profile and never conflicts with `url_standard`.
  .extract_from_urls(url, "scheme",
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    protocol_handling = protocol_handling,
    scheme_relative_handling = scheme_relative_handling,
    case_handling = "lower",
    url_standard = url_standard
  )
}

#' Get URL hosts
#'
#' Extracts the host component of a URL.
#'
#' Under \code{scheme_acceptance = "general"} a \code{mailto:} URL's first
#' recipient domain is returned, decomposed through the same PSL seam a web host
#' uses, so \code{\link{get_domain}} / \code{\link{get_tld}} /
#' \code{\link{get_subdomain}} work on it too (ADR 0012 D7). This deliberately
#' diverges from \code{\link{safe_parse_url}}, whose \code{host} column is
#' \code{NA} for a \code{mailto:} URL: a \code{mailto:} is a WHATWG opaque path
#' and has no authority, so the recipient domain is surfaced here as extraction
#' metadata rather than presented as a parsed authority. Under the default
#' \code{"web"} acceptance a \code{mailto:} URL is not parsed and this returns
#' \code{NA}.
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
                     scheme_policy = c("infer", "require"),
                     scheme_acceptance = c("web", "general"),
                     url_standard = NULL,
                     engine = NULL) {
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
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    protocol_handling = protocol_handling,
    www_handling = www_handling,
    tld_source = source,
    case_handling = case_handling,
    subdomain_levels_to_keep = subdomain_levels_to_keep,
    host_encoding = host_encoding,
    url_standard = url_standard,
    engine = engine
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
  scheme_policy = c("infer", "require"),
  scheme_acceptance = c("web", "general"),
  url_standard = NULL
) {
  # url_standard validation + conflict check must read missing() BEFORE the
  # match.arg() reassignments below (assignment can make missing() FALSE).
  url_standard <- .validate_url_standard(url_standard)
  # `path_encoding` is orthogonal (ADR 0011): not passed here, never conflicts.
  .check_url_standard_conflicts(url_standard, .governed_supplied(
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
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
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
#' Under `url_standard = "whatwg"` the underlying query carries the standard's
#' percent-encoded spelling (the query percent-encode set is applied, so a
#' literal space becomes `%20`); under `url_standard = "rfc3986"` or no
#' selector it is the raw source spelling. That distinction is only visible
#' with `decode = FALSE`, since decoding collapses both spellings.
#'
#' @section Security note:
#' Because `decode = TRUE` is the **default**, this accessor can return
#' characters the URL itself never contained literally — including control
#' characters such as CR and LF, which `%0D`/`%0A` decode to. Treat the result
#' as untrusted input: do not interpolate it into a header, a log line, a
#' shell command, or a SQL statement without escaping it for that sink, and do
#' not assume it is single-line. Pass `decode = FALSE` when you want the query
#' exactly as written, with no decoding step at all.
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
                      decode_plus = FALSE,
                      scheme_policy = c("infer", "require"),
                      scheme_acceptance = c("web", "general"),
                      url_standard = NULL) {
  format <- match.arg(format)
  query_handling <- match.arg(query_handling)
  empty_param_handling <- match.arg(empty_param_handling)
  url_standard <- .validate_url_standard(url_standard)

  .check_character_url(url)

  if (!.query_engine_active(query_handling, sort_params, empty_param_handling,
    decode_plus)) {
    return(.get_query_fast_path(url, protocol_handling, format, decode,
      scheme_policy, scheme_acceptance, url_standard))
  }

  .get_query_engine_path(
    url, protocol_handling, format, decode, query_handling, params_keep,
    params_drop, params_case_sensitive, sort_params, empty_param_handling,
    decode_plus, scheme_policy, scheme_acceptance, url_standard
  )
}

.check_character_url <- function(url) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
}

.query_engine_active <- function(query_handling, sort_params,
                                 empty_param_handling, decode_plus) {
  !(identical(query_handling, "keep") &&
    !sort_params &&
    identical(empty_param_handling, "keep") &&
    !decode_plus)
}

.get_query_fast_path <- function(url, protocol_handling, format, decode,
                                 scheme_policy, scheme_acceptance,
                                 url_standard) {
  if (identical(format, "string")) {
    raw <- .extract_from_urls(url, "query",
      protocol_handling = protocol_handling,
      scheme_policy = scheme_policy,
      scheme_acceptance = scheme_acceptance,
      url_standard = url_standard
    )
    if (decode) {
      return(.decode_raw_query_strings(raw))
    }
    return(raw)
  }

  raw <- .extract_from_urls(url, "query",
    protocol_handling = protocol_handling,
    case_handling = "keep",
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
  lapply(raw, ._parse_query_string, decode = decode)
}

.decode_raw_query_strings <- function(raw) {
  vapply(
    raw,
    function(q) {
      if (is.na(q)) {
        NA_character_
      } else {
        tryCatch(.pct_unescape(q), error = function(e) q)
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.get_query_engine_path <- function(url, protocol_handling, format, decode,
                                   query_handling, params_keep, params_drop,
                                   params_case_sensitive, sort_params,
                                   empty_param_handling, decode_plus,
                                   scheme_policy, scheme_acceptance,
                                   url_standard) {
  raw <- .extract_from_urls(url, "query",
    protocol_handling = protocol_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
  if (identical(format, "string")) {
    return(.get_query_engine_string(
      raw, decode, query_handling, params_keep, params_drop,
      params_case_sensitive, sort_params, empty_param_handling, decode_plus
    ))
  }

  .get_query_engine_list(
    raw, decode, query_handling, params_keep, params_drop,
    params_case_sensitive, sort_params, empty_param_handling, decode_plus
  )
}

.query_surviving_pairs <- function(query, query_handling, params_keep,
                                   params_drop, params_case_sensitive,
                                   sort_params, empty_param_handling,
                                   decode_plus) {
  ._query_surviving_pairs(
    query, query_handling, params_keep, params_drop, params_case_sensitive,
    sort_params, empty_param_handling, decode_plus, "builtin"
  )
}

.get_query_engine_string <- function(raw, decode, query_handling, params_keep,
                                     params_drop, params_case_sensitive,
                                     sort_params, empty_param_handling,
                                     decode_plus) {
  vapply(seq_along(raw), function(i) {
    if (is.na(raw[i])) {
      return(NA_character_)
    }
    .get_query_engine_string_one(
      raw[i], decode, query_handling, params_keep, params_drop,
      params_case_sensitive, sort_params, empty_param_handling, decode_plus
    )
  }, character(1), USE.NAMES = FALSE)
}

.get_query_engine_string_one <- function(query, decode, query_handling,
                                         params_keep, params_drop,
                                         params_case_sensitive, sort_params,
                                         empty_param_handling, decode_plus) {
  if (!decode) {
    return(._filter_query_params(
      query,
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
  sp <- .query_surviving_pairs(
    query, query_handling, params_keep, params_drop, params_case_sensitive,
    sort_params, empty_param_handling, decode_plus
  )
  if (is.null(sp)) {
    return("")
  }
  paste(paste0(sp$dec_key, "=", sp$dec_value), collapse = "&")
}

.get_query_engine_list <- function(raw, decode, query_handling, params_keep,
                                   params_drop, params_case_sensitive,
                                   sort_params, empty_param_handling,
                                   decode_plus) {
  lapply(raw, function(query) {
    if (is.na(query) || identical(query_handling, "drop")) {
      return(list())
    }
    .get_query_engine_list_one(
      query, decode, query_handling, params_keep, params_drop,
      params_case_sensitive, sort_params, empty_param_handling, decode_plus
    )
  })
}

.get_query_engine_list_one <- function(query, decode, query_handling,
                                       params_keep, params_drop,
                                       params_case_sensitive, sort_params,
                                       empty_param_handling, decode_plus) {
  sp <- .query_surviving_pairs(
    query, query_handling, params_keep, params_drop, params_case_sensitive,
    sort_params, empty_param_handling, decode_plus
  )
  if (is.null(sp)) {
    return(list())
  }
  if (decode) {
    .group_query_pairs(sp$dec_key, sp$dec_value)
  } else {
    .group_query_pairs(sp$raw_key, sp$raw_value)
  }
}

#' Get URL fragments
#'
#' Extracts the fragment component of a URL. The value is never
#' percent-decoded. Under \code{url_standard = "whatwg"} it carries the
#' standard's percent-encoded spelling (the fragment percent-encode set is
#' applied, so a double-quote inside the fragment becomes \code{\%22}); under
#' \code{url_standard = "rfc3986"} or no selector it is the raw source
#' spelling, exactly as written in the URL.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of fragments.
#' @export
#' @examples
#' get_fragment("http://example.com/path#section")
#' get_fragment("http://example.com/p#a\"b", url_standard = "whatwg")
get_fragment <- function(url, protocol_handling = "keep",
                         scheme_policy = c("infer", "require"),
                         scheme_acceptance = c("web", "general"),
                         url_standard = NULL) {
  url_standard <- .validate_url_standard(url_standard)
  .extract_from_urls(url, "fragment",
    protocol_handling = protocol_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
}

#' Get URL ports
#'
#' Extracts the port component of a URL.
#'
#' Under \code{url_standard = "whatwg"} a port equal to the scheme's default is
#' \emph{not part of the parsed URL} (the standard discards it during parsing),
#' so \code{"http://example.com:80/"} reports \code{NA} rather than \code{80}.
#' Under \code{url_standard = "rfc3986"} or no selector the written port is
#' reported as-is. This is distinct from \code{port_handling}, a presentation
#' dial that governs whether a port is rendered into \code{clean_url}; the two
#' are independent.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return An integer vector of ports.
#' @export
#' @examples
#' get_port("http://example.com:8080/path")
#' get_port("http://example.com:80/path", url_standard = "whatwg")
get_port <- function(url, protocol_handling = "keep",
                     scheme_policy = c("infer", "require"),
                     scheme_acceptance = c("web", "general"),
                     url_standard = NULL) {
  url_standard <- .validate_url_standard(url_standard)
  .extract_from_urls(url, "port",
    null_value = NA_integer_,
    fun_value = integer(1),
    transform = as.integer,
    protocol_handling = protocol_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
}

#' Get URL user names
#'
#' Extracts the user component of a URL. The value is never percent-decoded.
#' Under \code{url_standard = "whatwg"} it carries the standard's
#' percent-encoded spelling (WHATWG stores the username buffer encoded with the
#' userinfo percent-encode set, so \code{"http://a^b@host/"} yields
#' \code{"a\%5Eb"}); under \code{url_standard = "rfc3986"} or no selector it is
#' the raw source spelling, exactly as written in the URL.
#'
#' Under \code{scheme_acceptance = "general"} the user of a \code{mailto:} URL's
#' first recipient (its \code{addr-spec} local-part) is returned, mirroring how
#' \code{\link{get_host}} / \code{\link{get_domain}} extract that recipient's
#' domain (ADR 0012 D7). As with \code{\link{get_host}}, this deliberately
#' diverges from \code{\link{safe_parse_url}}, whose \code{user} column is
#' \code{NA} for a \code{mailto:} URL (an opaque path carries no authority).
#' Under the default \code{"web"} acceptance a \code{mailto:} URL is not parsed
#' and this returns \code{NA}.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of user names.
#' @seealso \code{\link{get_mailto_recipients}} for the full per-recipient list.
#' @export
#' @examples
#' get_user("ftp://user:password@ftp.example.com/file.txt")
#' get_user("mailto:jane@example.com",
#'   scheme_acceptance = "general", url_standard = "rfc3986")
get_user <- function(url, protocol_handling = "keep",
                     scheme_policy = c("infer", "require"),
                     scheme_acceptance = c("web", "general"),
                     url_standard = NULL) {
  url_standard <- .validate_url_standard(url_standard)
  .extract_from_urls(url, "user",
    protocol_handling = protocol_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
}

#' Get URL passwords
#'
#' Extracts the password component of a URL. The value is never
#' percent-decoded. Under \code{url_standard = "whatwg"} it carries the
#' standard's percent-encoded spelling (the userinfo percent-encode set is
#' applied, so a ":" inside the password becomes \code{\%3A}); under
#' \code{url_standard = "rfc3986"} or no selector it is the raw source
#' spelling, exactly as written in the URL. This is the same contract as
#' \code{\link{get_user}}.
#'
#' @param url A character vector of URLs.
#' @inheritParams safe_parse_url
#' @return A character vector of passwords.
#' @seealso \code{\link{get_user}}, \code{\link{get_userinfo}}.
#' @export
#' @examples
#' get_password("ftp://user:password@ftp.example.com/file.txt")
#' get_password("http://u:p:q@example.com/", url_standard = "whatwg")
get_password <- function(url, protocol_handling = "keep",
                         scheme_policy = c("infer", "require"),
                         scheme_acceptance = c("web", "general"),
                         url_standard = NULL) {
  url_standard <- .validate_url_standard(url_standard)
  .extract_from_urls(url, "password",
    protocol_handling = protocol_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
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
#' get_userinfo("ftp://user:password@ftp.example.com/file.txt")
#' get_userinfo("ftp://user@ftp.example.com/file.txt")
get_userinfo <- function(url, protocol_handling = "keep",
                         scheme_policy = c("infer", "require"),
                         scheme_acceptance = c("web", "general"),
                         url_standard = NULL) {
  url_standard <- .validate_url_standard(url_standard)
  .extract_from_urls(url, NULL,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard,
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

  host_l <- .ascii_tolower(host)
  domain_l <- .ascii_tolower(domain)
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
                          scheme_policy = c("infer", "require"),
                          scheme_acceptance = c("web", "general"),
                          url_standard = NULL,
                          engine = NULL) {
  source <- match.arg(source)
  format <- match.arg(format)
  host_encoding <- match.arg(host_encoding)
  url_standard <- .validate_url_standard(url_standard)

  # One engine pass with the deliberate subdomain profile (case = "lower" so the
  # suffix comparison is case-insensitive; host_encoding shared by host+domain),
  # then vectorized label derivation. field = NULL hands the column list to the
  # transform, which returns the per-row label list.
  results <- .extract_from_urls(url, NULL,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
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
    url_standard = url_standard,
    engine = engine
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
                    scheme_policy = c("infer", "require"),
                    scheme_acceptance = c("web", "general"),
                    url_standard = NULL,
                    engine = NULL) {
  source <- match.arg(source)
  host_encoding <- match.arg(host_encoding)
  url_standard <- .validate_url_standard(url_standard)
  # case_handling is immaterial to the tld output (the TLD is derived from the
  # normalized host, independent of the clean_url case policy); "lower" is
  # retained as an explicit, stable profile rather than aligned to the
  # "lower_host" default to avoid a cache-key/output change (RURL-actrnerd).
  .extract_from_urls(url, "tld",
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    tld_source = source,
    case_handling = "lower",
    host_encoding = host_encoding,
    url_standard = url_standard,
    engine = engine
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
#'   \code{"rfc3986"}, or \code{"whatwg"}. The gate is semantic, not stylistic:
#'   whether a host is an IPv4 literal or a registered name is a question only
#'   a standard answers, so with no selector there is no fact to report.
#'   \code{\link{get_parse_verdicts}} is deliberately \emph{not} gated this way
#'   --- its layers describe the parse that actually ran, which is defined with
#'   or without a selector.
#' @inheritParams safe_parse_url
#' @return A character vector the same length as \code{url}, each element one of
#'   the \code{host_type} tokens above, or \code{NA}.
#'
#'   \strong{\code{NA} is ambiguous, and knowingly so.} It means either that no
#'   selector was passed --- in which case nothing was classified at all --- or
#'   that a selector \emph{was} passed and this row could not be classified
#'   under it. The two are indistinguishable in the returned value:
#'
#'   \preformatted{identical(
#'   get_host_type(c("/relative/path", ""), url_standard = "rfc3986"),
#'   get_host_type(c("http://example.com/", "http://x.test/"))
#' )
#' #> TRUE}
#'
#'   A caller that must tell them apart has to retain its own knowledge of
#'   whether it supplied \code{url_standard}; the return value does not carry
#'   it. In particular, an all-\code{NA} result is \emph{not} evidence that the
#'   URLs are unclassifiable --- it is the same answer a selector-less call
#'   gives for perfectly well-formed input.
#'
#'   \code{\link{get_scheme_class}} imposes no such burden: see the guarantee in
#'   its own \code{Value} section.
#' @seealso \code{\link{get_url_diagnostics}}, \code{\link{get_parse_verdicts}},
#'   \code{\link{safe_parse_url}}
#' @export
#' @examples
#' get_host_type("http://example.com/", url_standard = "rfc3986")
#' get_host_type("http://2130706433/", url_standard = "whatwg")
get_host_type <- function(url, url_standard = NULL,
                          scheme_policy = c("infer", "require"),
                          scheme_acceptance = c("web", "general")) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  opts <- .parse_options(url_standard = url_standard,
    scheme_policy = scheme_policy, scheme_acceptance = scheme_acceptance)
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
#' them. The complete token vocabulary is enumerated below under
#' \emph{Diagnostic vocabulary (canonical)}.
#'
#' A single URL can carry several diagnostics, so the return shape is not a
#' plain scalar-per-URL vector (see \emph{Value}). \code{parse_status} stays
#' coarse; diagnostics are never encoded into it.
#'
#' @section Selected facts, not a conformance oracle: The diagnostics are
#'   deliberately a \strong{selected} set of facts, \strong{not} a complete
#'   validator. The \emph{absence} of a diagnostic never implies the URL
#'   conforms to its scheme's specification or to WHATWG/RFC 3986. Full
#'   per-standard conformance validation is out of scope (ADR 0012 D5).
#'
#'   Two WHATWG-generic facts gate on the \emph{interpreting standard}, not the
#'   acceptance axis, so they are reported whenever \code{url_standard =
#'   "whatwg"} --- including the default \code{"web"} acceptance path (they are
#'   route-independent, string-level facts; RURL-sgjzbqzk):
#'   \itemize{
#'     \item \code{invalid-URL-unit} / \code{invalid-credentials} --- WHATWG
#'       validation errors (WHATWG-verbatim names): a malformed \code{\%}-escape
#'       or a non-URL code point, and any credentials (userinfo) present.
#'       Bounded detection.
#'   }
#'   The default combination (\code{"web"} acceptance with \code{url_standard =
#'   NULL}) emits no diagnostics at all, so it is unaffected.
#'
#'   With \code{scheme_acceptance = "general"} (the general-parser posture) a
#'   further set of selected facts is reported. These fire \emph{only} under
#'   \code{"general"}; the default \code{"web"} acceptance never emits them:
#'   \itemize{
#'     \item \code{unicode-outside-rfc3986-uri} --- under \code{"rfc3986"}, a
#'       directly-written non-ASCII scalar value accepted by the sole RFC 3986
#'       generic-grammar tolerance (not RFC 3987/IRI conformance).
#'     \item \code{transform-skipped-ineligible-scheme} --- the scheme is
#'       non-HTTP(S) and so ineligible for the SEO/semantic Stage-B transforms.
#'     \item scheme-specific facts: \code{ws-fragment-forbidden} /
#'       \code{ws-userinfo-forbidden} (RFC 6455),
#'       \code{mailto-fragment-discouraged} (RFC 6068),
#'       \code{tel-missing-phone-context} (RFC 3966),
#'       \code{data-missing-comma} (RFC 2397), and, under \code{"rfc3986"},
#'       \code{file-non-absolute-path},
#'       \code{file-userinfo-extension} (userinfo, permitted by RFC 8089
#'       Appendix E.1's non-normative extended grammar), and
#'       \code{file-component-outside-rfc8089} (a port, query or fragment,
#'       which RFC 8089's grammar does not mention and which are therefore
#'       inherited generic RFC 3986 components).
#'   }
#'
#' @section Diagnostic vocabulary (canonical): This section is the
#'   \strong{single authoritative enumeration} of the diagnostics vocabulary.
#'   It is held to the runtime registry (\code{.URL_DIAGNOSTICS}) in both
#'   directions by \code{tools/diagnostics-doc-consistency.R}, a CI gate: a
#'   token cannot be added, renamed, or removed without this list moving with
#'   it. Earlier design documents (including the v1 selector PRD's section 7
#'   table) are historical records of what the vocabulary was when they were
#'   accepted --- they are not registries and do not track it.
#'
#'   Every token below is emitted only when \code{url_standard} is not
#'   \code{NULL}. Tokens marked \emph{general} additionally require
#'   \code{scheme_acceptance = "general"}; the rest fire under both acceptance
#'   postures and, unless noted, under both \code{"rfc3986"} and
#'   \code{"whatwg"}.
#'
#'   \strong{Host --- IPv4 shape.} Facts about a host written as, or coerced
#'   to, an IPv4 address; security filters typically reject all of them.
#'   \itemize{
#'     \item \code{ipv4-number-form} --- numeric IPv4 shorthand instead of
#'       dotted decimal.
#'     \item \code{ipv4-non-dotted} --- a whole-host number parsed/coerced to
#'       IPv4 in WHATWG mode.
#'     \item \code{ipv4-short-form} --- fewer than four dotted parts.
#'     \item \code{ipv4-non-decimal} --- hex or octal notation participated in
#'       IPv4 parsing.
#'     \item \code{ipv4-octal} --- octal interpretation changed the apparent
#'       address value.
#'     \item \code{ipv4-leading-zero} --- a dotted decimal-looking part had a
#'       leading zero.
#'     \item \code{ipv4-out-of-range} --- a dotted part exceeds 255 (fatal
#'       under \code{"whatwg"}; flags a numeric-looking \code{reg-name} under
#'       \code{"rfc3986"}, e.g. \code{256.1.1.1}).
#'   }
#'
#'   \strong{Host --- DNS length, UTS-46 and charset.} Probed against the
#'   resolved host; IP literals are excluded.
#'   \itemize{
#'     \item \code{domain-label-too-long} --- a label exceeds the DNS 63-byte
#'       limit.
#'     \item \code{domain-name-too-long} --- the whole name exceeds the DNS
#'       253-byte limit.
#'     \item \code{domain-empty-label} --- the host contains an empty label
#'       (a \code{".."} run, or a leading dot).
#'     \item \code{domain-hyphen-violation} --- a label breaks the UTS-46
#'       hyphen rules (leading/trailing hyphen, or \code{"--"} in positions
#'       3--4 of a non-\code{xn--} label).
#'     \item \code{domain-std3-violation} --- a label carries a code point
#'       outside the STD3 LDH set.
#'     \item \code{host-charset-shimmed} --- the host carries one of the 15
#'       code points WHATWG keeps but the historical parser rejected,
#'       accepted by the shim (ADR 0009: \code{! $ & ( ) * + , ; =}, plus the
#'       ASCII quotation mark, apostrophe, grave accent, and the two braces).
#'       \code{"whatwg"} only.
#'   }
#'
#'   \strong{Path.}
#'   \itemize{
#'     \item \code{encoded-dot-segment} --- an encoded-dot segment
#'       (\code{\%2e} / \code{\%2e\%2e}, any hex case) that the profile's dot
#'       handling acted on.
#'     \item \code{encoded-reserved-path-byte} --- the preserved path still
#'       carries an encoded reserved byte (\code{\%2F}, \code{\%3F},
#'       \code{\%23}) held as data rather than as a separator.
#'   }
#'
#'   \strong{Port.} Facts about the raw port versus the resolved scheme's
#'   WHATWG default, independent of the \code{port_handling} knob.
#'   \itemize{
#'     \item \code{explicit-default-port} --- the port was written out and
#'       equals the scheme's default.
#'     \item \code{non-default-port} --- a port is present and is not the
#'       scheme's default (including any port on a scheme with no defined
#'       default).
#'   }
#'
#'   \strong{Input shape --- WHATWG cleanup.} All three are \code{"whatwg"}
#'   only; \code{"rfc3986"} has no strip or rewrite step.
#'   \itemize{
#'     \item \code{invalid-reverse-solidus} --- a literal \code{\\} was
#'       reinterpreted as \code{/} (special schemes only).
#'     \item \code{control-char-stripped} --- an ASCII tab/LF/CR was removed
#'       from the interior of the input (step 1, second half).
#'     \item \code{leading-trailing-stripped} --- a leading and/or trailing run
#'       of C0-control-or-SPACE was removed (step 1, first half).
#'   }
#'
#'   \strong{Layer 5 --- selected per-standard and per-scheme facts}
#'   (ADR 0012 D5). Described in full under \emph{Selected facts, not a
#'   conformance oracle} above.
#'   \itemize{
#'     \item \code{invalid-URL-unit} --- WHATWG validation error: a malformed
#'       \code{\%}-escape or a non-URL code point. \code{"whatwg"} only.
#'     \item \code{invalid-credentials} --- WHATWG validation error:
#'       credentials (userinfo) are present. \code{"whatwg"} only.
#'     \item \code{unicode-outside-rfc3986-uri} --- \emph{general}; a
#'       directly-written non-ASCII scalar value under \code{"rfc3986"}.
#'     \item \code{transform-skipped-ineligible-scheme} --- \emph{general};
#'       a non-HTTP(S) scheme, ineligible for the Stage-B transforms.
#'     \item \code{ws-fragment-forbidden} --- \emph{general}; a fragment on a
#'       \code{ws:}/\code{wss:} URL (RFC 6455).
#'     \item \code{ws-userinfo-forbidden} --- \emph{general}; userinfo on a
#'       \code{ws:}/\code{wss:} URL (RFC 6455).
#'     \item \code{mailto-fragment-discouraged} --- \emph{general}; a fragment
#'       on a \code{mailto:} URL (RFC 6068).
#'     \item \code{tel-missing-phone-context} --- \emph{general}; a local
#'       \code{tel:} number with no \code{phone-context} (RFC 3966).
#'     \item \code{data-missing-comma} --- \emph{general}; a \code{data:} URL
#'       with no \code{","} separator (RFC 2397).
#'     \item \code{file-non-absolute-path} --- \emph{general}; a
#'       non-absolute \code{file:} path under \code{"rfc3986"}.
#'     \item \code{file-userinfo-extension} --- \emph{general}; userinfo on a
#'       \code{file:} URL, permitted by RFC 8089 Appendix E.1's non-normative
#'       extended grammar.
#'     \item \code{file-component-outside-rfc8089} --- \emph{general}; a port,
#'       query or fragment on a \code{file:} URL, inherited from generic
#'       RFC 3986.
#'   }
#'
#' @param url A character vector of URLs.
#' @param url_standard Standard profile governing interpretation: \code{NULL}
#'   (default; no diagnostics), \code{"rfc3986"}, or \code{"whatwg"}.
#' @inheritParams safe_parse_url
#' @return For a length-1 \code{url}, a character vector of zero or more
#'   diagnostic tokens for that URL. For a length-n \code{url} (including
#'   \code{n == 0}), a list of length n whose i-th element is the character
#'   vector of that URL's tokens (\code{character(0)} when it has none).
#'
#'   \strong{An empty result is ambiguous, and knowingly so.} Per row,
#'   \code{character(0)} means either that no selector was passed --- in which
#'   case no diagnostics were evaluated for any row --- or that a selector
#'   \emph{was} passed and that URL raised none. An empty result is therefore
#'   never evidence that a URL is clean unless the caller knows it supplied
#'   \code{url_standard}. Under \code{NULL} every row is \code{character(0)},
#'   whatever the input.
#' @seealso \code{\link{get_host_type}}, \code{\link{safe_parse_url}}
#' @export
#' @examples
#' get_url_diagnostics("http://example.com/", url_standard = "rfc3986")
#' get_url_diagnostics(
#'   c("http://example.com/", "http://2130706433/"),
#'   url_standard = "whatwg"
#' )
get_url_diagnostics <- function(url, url_standard = NULL,
                                scheme_policy = c("infer", "require"),
                                scheme_acceptance = c("web", "general")) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  opts <- .parse_options(url_standard = url_standard,
    scheme_policy = scheme_policy, scheme_acceptance = scheme_acceptance)
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
#' Within rurl's allowlist (\code{http}/\code{https}/\code{ftp}/\code{ftps}/
#' \code{file}), \code{http}, \code{https}, \code{ftp}, and \code{file} are
#' WHATWG special schemes; \code{ftps} (FTP-over-TLS, rurl's own addition) is
#' not. This is metadata only -- it does not add \code{ws}/\code{wss} to
#' rurl's allowed schemes and does not change what
#' \code{\link{safe_parse_url}} accepts.
#'
#' @param url A character vector of URLs.
#' @param url_standard Standard profile gating the classification: \code{NULL}
#'   (default; no classification, returns \code{NA}), \code{"rfc3986"}, or
#'   \code{"whatwg"}. The gate is semantic, not stylistic: "special" is a
#'   WHATWG notion, so without a selector there is no fact to report.
#'   \code{\link{get_parse_verdicts}} is deliberately \emph{not} gated this way
#'   --- its layers describe the parse that actually ran, which is defined with
#'   or without a selector.
#' @inheritParams safe_parse_url
#' @return A character vector the same length as \code{url}, each element one
#'   of \code{"special"}, \code{"non-special"}, or \code{"missing-or-error"},
#'   or \code{NA} when no selector is given.
#'
#'   \strong{Here \code{NA} is unambiguous}, unlike
#'   \code{\link{get_host_type}}'s. Given a selector, every element receives one
#'   of the three tokens above --- input that is unparseable, scheme-less, empty
#'   or \code{NA} classifies as \code{"missing-or-error"} rather than falling
#'   through to \code{NA}. So \code{NA} occurs if and only if
#'   \code{url_standard} was \code{NULL}, and \code{is.na()} on this result is a
#'   reliable test for the selector-less call.
#' @details
#' Under the default \code{scheme_acceptance = "web"} an opaque scheme such as
#' \code{mailto:} is outside rurl's web allowlist and classifies as
#' \code{"missing-or-error"}. Pass \code{scheme_acceptance = "general"} to run
#' the general parser, under which such a scheme resolves and classifies as
#' \code{"non-special"} (it is not a WHATWG special scheme).
#' @seealso \code{\link{get_host_type}}, \code{\link{get_scheme}}
#' @export
#' @examples
#' get_scheme_class("http://example.com/", url_standard = "whatwg")
#' get_scheme_class("ftps://example.com/", url_standard = "whatwg")
#' get_scheme_class("//example.com/path", url_standard = "whatwg")
#' get_scheme_class(
#'   "mailto:jane@example.com",
#'   url_standard = "rfc3986", scheme_acceptance = "general"
#' )
get_scheme_class <- function(url, url_standard = NULL,
                             scheme_policy = c("infer", "require"),
                             scheme_acceptance = c("web", "general")) {
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
  scheme <- get_scheme(url,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
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
