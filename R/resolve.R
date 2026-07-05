# Reference resolution: resolve a relative or absolute URL reference against a
# base URL (RFC 3986 section 5), then canonicalize the result through the same
# url_standard-governed machinery as safe_parse_url(s). This file adds NO new
# per-standard divergent behavior of its own (PRD v2 D6): the base-merge
# (section 5.2.2) is identical under both standards, and everything downstream
# (path percent/dot handling, host IPv4/reg-name model, port elision, WHATWG
# backslash recognition, diagnostics) is delegated to safe_parse_urls().

# Split a URI reference into its five components using the RFC 3986 Appendix B
# regular expression. Each of scheme / authority / query / fragment is either a
# string (the component was PRESENT, possibly empty) or NA (ABSENT -- e.g. no
# "//" means authority is NA, distinct from "" for "http:///"). `path` is always
# a string (RFC 3986 always matches a, possibly empty, path). Returns NA fields
# for an NA input. Purely syntactic and standard-agnostic.
.split_uri_ref <- function(ref) {
  na_ref <- list(
    scheme = NA_character_, authority = NA_character_, path = NA_character_,
    query = NA_character_, fragment = NA_character_
  )
  if (is.na(ref)) {
    return(na_ref)
  }
  # ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
  # Groups: 2 scheme, 4 authority, 5 path, 7 query, 9 fragment.
  m <- regmatches(
    ref,
    regexec(
      "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$",
      ref,
      perl = TRUE
    )
  )[[1]]
  if (length(m) < 10L) {
    # This regex (all groups optional, anchored) matches every string, so a
    # short vector is unreachable in practice; stay defensive and treat the
    # whole input as an opaque path.
    na_ref$path <- ref
    return(na_ref)
  }
  # m[[1]] is the full match; capture groups follow. A component counts as
  # PRESENT only when its delimiter group matched: m[[3]] scheme (via the
  # "scheme:" group m[[2]]), m[[4]] = "//authority" gating m[[5]] authority,
  # m[[6]] path (always present), m[[7]] = "?query" gating m[[8]] query,
  # m[[9]] = "#fragment" gating m[[10]] fragment.
  scheme <- if (nzchar(m[[2L]])) m[[3L]] else NA_character_
  authority <- if (startsWith(m[[4L]], "//")) m[[5L]] else NA_character_
  path <- m[[6L]]
  query <- if (startsWith(m[[7L]], "?")) m[[8L]] else NA_character_
  fragment <- if (startsWith(m[[9L]], "#")) m[[10L]] else NA_character_
  list(
    scheme = scheme, authority = authority, path = path,
    query = query, fragment = fragment
  )
}

# RFC 3986 section 5.2.3: merge a relative-reference path with the base path.
# When the base has an authority and an empty path, the merged path is the
# reference path prefixed with "/"; otherwise it is the base path up to and
# including its last "/", followed by the reference path.
.merge_ref_path <- function(base_authority, base_path, ref_path) {
  if (!is.na(base_authority) && !nzchar(base_path)) {
    return(paste0("/", ref_path))
  }
  slash <- regexpr("/[^/]*$", base_path, perl = TRUE)
  if (slash == -1L) {
    # Base path has no "/": the "everything up to the last /" prefix is empty.
    return(ref_path)
  }
  paste0(substr(base_path, 1L, slash), ref_path)
}

# RFC 3986 section 5.2.2: transform a parsed reference `r` against a parsed base
# `b` into the target components. Returns a component list (scheme / authority /
# path / query / fragment). Standard-agnostic -- the same algorithm under both
# rfc3986 and whatwg (PRD v2 D6). `._remove_dot_segments()` (R/path-query.R) is
# reused for the mandated dot-segment removal.
.transform_reference <- function(r, b) {
  if (!is.na(r$scheme)) {
    return(list(
      scheme = r$scheme,
      authority = r$authority,
      path = ._remove_dot_segments(r$path),
      query = r$query,
      fragment = r$fragment
    ))
  }
  if (!is.na(r$authority)) {
    authority <- r$authority
    path <- ._remove_dot_segments(r$path)
    query <- r$query
  } else {
    authority <- b$authority
    if (nzchar(r$path)) {
      if (startsWith(r$path, "/")) {
        path <- ._remove_dot_segments(r$path)
      } else {
        path <- ._remove_dot_segments(
          .merge_ref_path(b$authority, b$path, r$path)
        )
      }
      query <- r$query
    } else {
      path <- b$path
      query <- if (is.na(r$query)) b$query else r$query
    }
  }
  list(
    scheme = b$scheme,
    authority = authority,
    path = path,
    query = query,
    fragment = r$fragment
  )
}

# RFC 3986 section 5.3: recompose target components into a URI string. A
# component contributes its delimiter only when it is present (non-NA).
.recompose_uri <- function(t) {
  out <- ""
  if (!is.na(t$scheme)) {
    out <- paste0(out, t$scheme, ":")
  }
  if (!is.na(t$authority)) {
    out <- paste0(out, "//", t$authority)
  }
  out <- paste0(out, if (!is.na(t$path)) t$path else "")
  if (!is.na(t$query)) {
    out <- paste0(out, "?", t$query)
  }
  if (!is.na(t$fragment)) {
    out <- paste0(out, "#", t$fragment)
  }
  out
}

# Resolve ONE (reference, base) pair to a raw absolute URI string, or NA when
# resolution cannot yield an absolute URL (base not absolute and reference not
# absolute either). ._remove_dot_segments happens inside .transform_reference().
.resolve_one_raw <- function(ref, base) {
  if (is.na(ref)) {
    return(NA_character_)
  }
  r <- .split_uri_ref(ref)
  if (!is.na(r$scheme)) {
    # Absolute reference: base is irrelevant (section 5.2.2 first branch).
    empty_base <- .split_uri_ref(NA_character_)
    return(.recompose_uri(.transform_reference(r, empty_base)))
  }
  # Relative reference: the base must be an absolute URL (have a scheme).
  if (is.na(base)) {
    return(NA_character_)
  }
  b <- .split_uri_ref(base)
  if (is.na(b$scheme)) {
    return(NA_character_)
  }
  .recompose_uri(.transform_reference(r, b))
}

#' Resolve a URL reference against a base URL
#'
#' Resolves a relative or absolute URL reference against a base URL following
#' the RFC 3986 section 5 reference-resolution algorithm, then canonicalizes the
#' result with the same machinery as \code{\link{safe_parse_url}}. The
#' base-merge step (empty reference, fragment-only, query-only, scheme-relative
#' \code{//host} reference, absolute-path reference, and relative-path merge) is
#' identical under both standards; \code{url_standard} and any \code{...}
#' options flow straight through to the parse so the host IPv4/reg-name model,
#' path percent/dot-segment handling, default-port elision, WHATWG
#' backslash-as-slash recognition, and diagnostics are exactly those of a direct
#' \code{safe_parse_url()} call on the resolved URL. \code{resolve_url()}
#' introduces no per-standard behavior of its own.
#'
#' The return value is the \emph{canonical} \code{clean_url} of the resolved
#' reference, not a verbatim RFC 3986 recomposition: as everywhere else in rurl,
#' the fragment and userinfo are excluded from \code{clean_url}, the query is
#' included only when \code{query_handling != "drop"} (the default drops it),
#' and the port only when \code{port_handling != "exclude"}. This differs from a
#' generic resolver such as \code{xml2::url_absolute()} or Python's
#' \code{urljoin}, which preserve every component verbatim; \code{resolve_url()}
#' resolves \emph{and} canonicalizes. To inspect individual resolved components
#' (including the fragment), resolve first and pass the result to
#' \code{\link{safe_parse_url}}.
#'
#' @param relative_or_absolute A character vector of URL references to resolve.
#'   Each may be relative (\code{"../b"}, \code{"?q=1"}, \code{"#frag"},
#'   \code{"//host/p"}) or already absolute (\code{"https://host/p"}); an
#'   absolute reference ignores \code{base_url}.
#' @param base_url A character vector of base URLs, recycled against
#'   \code{relative_or_absolute}. Each base must itself be an absolute URL
#'   (carry a scheme); a relative reference resolved against a scheme-less or
#'   \code{NA} base yields \code{NA}.
#' @param url_standard Optional standard profile forwarded to the parse:
#'   \code{NULL} (default), \code{"rfc3986"}, or \code{"whatwg"}. See
#'   \code{\link{safe_parse_url}} for the axes it governs. The reference-
#'   resolution merge itself does not vary between the two profiles.
#' @param ... Additional arguments forwarded to \code{\link{safe_parse_urls}}
#'   (e.g. \code{port_handling}, \code{query_handling}, \code{host_encoding}).
#'   Passing a governed low-level knob that conflicts with \code{url_standard}
#'   errors, exactly as it does for \code{\link{safe_parse_url}}.
#' @return A character vector the same length as the recycled inputs: the
#'   canonical \code{clean_url} of each resolved reference, or \code{NA} where
#'   resolution cannot produce an absolute URL or the resolved URL is
#'   unparseable.
#' @seealso \code{\link{safe_parse_url}}, \code{\link{get_clean_url}}
#' @export
#' @examples
#' resolve_url("../g", "http://a/b/c/d;p?q") # -> "http://a/b/g"
#' resolve_url("g", "http://a/b/c/d;p?q") # -> "http://a/b/c/g"
#' resolve_url("//example.org/p", "http://a/b/c") # -> "http://example.org/p"
#' resolve_url("https://x.com/y", "http://a/b/c") # absolute ref, base ignored
#' resolve_url(c("g", "../h"), "http://a/b/c/") # vectorized
resolve_url <- function(relative_or_absolute, base_url, url_standard = NULL,
                        ...) {
  # url_standard conflict check across the `...` seam (same contract as
  # canonical_join(): missing() cannot see through `...`, so read the governed
  # knobs straight from the captured dots).
  .check_url_standard_conflicts_dots(
    c(list(url_standard = url_standard), list(...))
  )
  url_standard <- .validate_url_standard(url_standard)

  ref <- as.character(relative_or_absolute)
  base <- as.character(base_url)

  # A zero-length operand yields a zero-length result, as R vectorized ops do.
  if (length(ref) == 0L || length(base) == 0L) {
    return(character(0))
  }
  # Recycle both inputs to the common length (base_url is commonly scalar).
  n <- max(length(ref), length(base))
  ref <- rep_len(ref, n)
  base <- rep_len(base, n)

  resolved_raw <- vapply(
    seq_len(n),
    function(i) .resolve_one_raw(ref[[i]], base[[i]]),
    character(1)
  )

  # Delegate ALL normalization/rendering/diagnostics to the shared parser so
  # resolve_url() adds no divergent behavior. safe_parse_urls() memoizes, so
  # duplicate resolved URLs cost only a match().
  parsed <- safe_parse_urls(resolved_raw, url_standard = url_standard, ...)
  out <- parsed$clean_url
  # A reference that could not resolve to an absolute URL (NA raw) is NA here
  # anyway, since safe_parse_urls() returns NA clean_url for an NA input.
  out
}
