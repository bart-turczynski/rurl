# Public named-profile inspector (ADR 0012 Layer 6 / D6, RURL-djmgzjmr).
# The resolution machinery (.URL_PROFILES, .validate_profile, .resolve_profile)
# lives in R/parse.R alongside the parser knobs it bundles; this file only holds
# the exported inspector, which runs the SAME .resolve_profile() the parse
# wrappers use, so the inspector and the parse path cannot diverge.

#' Inspect a named parsing profile
#'
#' Expands a named \code{profile} into the resolved bundle of low-level parser
#' knobs it sets, running the exact same resolution the parse functions
#' (\code{\link{safe_parse_url}}, \code{\link{safe_parse_urls}},
#' \code{\link{get_clean_url}}) apply when you pass \code{profile}. It never
#' parses a URL; it answers \dQuote{what does this profile actually do, and did
#' my explicit overrides change it?}.
#'
#' Profiles are inspectable sugar that bundle the parser's acceptance,
#' interpretation, leniency, and canonicalization axes under one name. Explicit
#' arguments always override the profile (the iron rule); when any override is
#' supplied, the resolved result is flagged \code{customized = TRUE} and rurl no
#' longer claims the result matches the named profile exactly.
#'
#' The recognized profiles are \code{"browser"} (browser-\emph{like}
#' http-prepending fix-up posture; not Chrome-faithful), \code{"whatwg"}
#' (absolute-URL, no-base spec posture that \emph{rejects} scheme-less input),
#' \code{"rfc-syntax"} (RFC 3986 generic syntax as \emph{parsing}, not
#' normalization: case and dot-segments are preserved), and \code{"seo"}
#' (rurl's origin-cleaning intent; \code{"canonical"} is an alias resolving
#' identically to \code{"seo"}).
#'
#' \code{"seo"} delivers rurl's definition of a \emph{clean URL}: a
#' \strong{lossy policy projection} of a WHATWG-parsed URL (ADR 0017), never a
#' separate, weaker construction. It selects \code{url_standard = "whatwg"} as
#' the identity underneath — which is also what resolves \code{.} and \code{..}
#' folder segments — and \code{host_encoding = "unicode"}, so the host is
#' canonical in one direction regardless of whether the input spelled it in
#' Unicode or Punycode. The remaining knobs are the projection: https, strip
#' www, trailing slash and index page, and \strong{drop the whole query}. The
#' result makes no claim of resource equivalence and is not an HTML canonical
#' URL — forcing https, dropping the port and dropping the query can each
#' change the resource addressed. As always, an explicit argument overrides the
#' bundle, so \code{query_handling = "filter"} still buys tracker-only removal.
#'
#' @param profile A single profile name: one of \code{"browser"},
#'   \code{"whatwg"}, \code{"rfc-syntax"}, \code{"seo"}, or the \code{"seo"}
#'   alias \code{"canonical"}.
#' @param ... Optional explicit knob overrides (e.g. \code{scheme_policy =
#'   "require"}), named as in \code{\link{safe_parse_url}}. Each override that
#'   the profile also sets replaces the profile's value and marks the result
#'   \code{customized}.
#' @return A named list of the resolved knob \eqn{\rightarrow} value pairs the
#'   profile sets (the same shape as an internal profile bundle), plus a
#'   trailing logical \code{customized} element.
#' @seealso \code{\link{safe_parse_url}}, \code{\link{get_scheme_class}}
#' @export
#' @examples
#' url_profile("browser")
#' url_profile("seo")
#' # canonical is an alias of seo:
#' identical(url_profile("canonical"), url_profile("seo"))
#' # explicit overrides win and flag the result customized:
#' url_profile("browser", scheme_policy = "require")
url_profile <- function(profile = NULL, ...) {
  profile <- .validate_profile(profile)
  if (is.null(profile)) {
    stop(
      "url_profile() requires a profile name (one of: ",
      toString(.url_profile_choices),
      ").",
      call. = FALSE
    )
  }
  supplied <- list(...)
  if (length(supplied) > 0L) {
    nms <- names(supplied)
    if (is.null(nms) || !all(nzchar(nms))) {
      stop(
        "url_profile() overrides must be supplied as named `knob = value` ",
        "arguments.",
        call. = FALSE
      )
    }
    unknown <- setdiff(nms, names(.profile_knob_choices))
    if (length(unknown) > 0L) {
      stop(
        "url_profile() does not recognize override(s): ",
        toString(unknown),
        ". Recognized knobs: ",
        toString(names(.profile_knob_choices)),
        ".",
        call. = FALSE
      )
    }
    for (knob in nms) {
      supplied[[knob]] <- match.arg(
        supplied[[knob]], .profile_knob_choices[[knob]]
      )
    }
  }
  res <- .resolve_profile(profile, supplied)
  c(res$opts, list(customized = res$customized))
}
