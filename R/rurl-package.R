#' @keywords internal
#' @seealso
#' Parsing: [safe_parse_url()], [safe_parse_urls()].
#' Accessors: [get_host()], [get_domain()], [get_tld()], [get_subdomain()],
#' [get_path()], [get_query()].
#' Cleaning and joining: [get_clean_url()], [canonical_join()].
#' Query introspection: [query_param_summary()].
#' Cache management: [rurl_clear_caches()], [rurl_cache_info()],
#' [rurl_cache_config()].
#'
#' Domain and public-suffix extraction is delegated to the \pkg{pslr} package;
#' Punycode/IDNA encoding is handled by the \pkg{punycoder} package.
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
