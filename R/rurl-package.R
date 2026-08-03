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
#'
#' @examples
#' # Parse a vector of URLs into one row each.
#' urls <- c(
#'   "https://www.Example.co.uk/Blog/index.html?utm_source=nl&id=7#top",
#'   "http://sub.example.com:8080/a/./b/../c"
#' )
#' safe_parse_urls(urls)[, c("scheme", "host", "domain", "tld", "path")]
#'
#' # Reach a single component without materializing the frame.
#' get_domain(urls)
#' get_subdomain(urls)
#'
#' # Clean for SEO: a WHATWG parse plus visual tweaks. Dot segments resolve,
#' # the host renders in Unicode, www/index/tracking params come off.
#' get_clean_url(urls, profile = "seo")
#'
#' # Profiles are inspectable sugar over the low-level knobs, and an explicit
#' # argument always overrides the bundle.
#' url_profile("seo")
#' get_clean_url("https://xn--mnchen-3ya.de/a", profile = "seo")
#' get_clean_url("https://xn--mnchen-3ya.de/a", profile = "seo",
#'   host_encoding = "keep")
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
