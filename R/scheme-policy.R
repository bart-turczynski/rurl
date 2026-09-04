# Scheme-side policy companion (RUL-021, carrier RURL-zfycisur).
#
# WHY THIS EXISTS. rurl already ships an allowlist: `scheme_acceptance = "web"`
# admits exactly `.SUPPORTED_SCHEMES` and rejects everything else at parse time.
# What it does not ship is a PARAMETERIZABLE one. The moment a caller needs one
# scheme outside those five they must move to `scheme_acceptance = "general"`,
# which admits every syntactically valid scheme token at once -- an
# all-or-nothing dial where the established transfer libraries offer a set. See
# NEWS and RUL-021 for the prior art this mirrors.
#
# WHY A COMPANION RATHER THAN A PARSE ARGUMENT. Under `general` the parser is
# telling the caller what the standard says about the input. Turning a
# conformant scheme into `parse_status = "error"` because of a caller's local
# policy would make the parse result a function of that policy rather than of
# the standard. So the facts are reported here and the caller filters, exactly
# as `check_hosts()` does on the host axis (ADR 0006: companion helpers surface
# metadata; parse results are never widened).
#
# WHY THE VOCABULARY IS DESCRIPTIVE. Every token below names something
# observable about the parse -- which scheme, which class, whether an authority
# was present, whether it falls inside the built-in web acceptance set. None of
# them is a risk label. "Dangerous" is not a property of a scheme in the
# abstract; it is a property of what the caller does with the URL, which this
# package cannot see. The same reasoning `check_hosts()` applies with
# `ip-literal` / `not-registrable` / `underscore-label`.

#' Report scheme facts for a set of URLs, and score them against an allowlist
#'
#' Tabular companion to \code{\link{get_scheme}} and
#' \code{\link{get_scheme_class}}, and the scheme-axis counterpart of
#' \code{\link{check_hosts}}: for each URL, reports the parsed scheme, its
#' special/non-special classification, whether it falls inside the built-in
#' web-acceptance set, an optional \code{allowed} column scored against a
#' caller-supplied allowlist, and a \code{reasons} list-column naming the
#' scheme facts observed.
#'
#' Like \code{\link{check_hosts}} this is a \strong{policy} layer, not parser
#' conformance: it never changes how a URL parses. Restricting which schemes
#' your application will act on is the caller's decision, and this helper
#' supplies the facts to make it --- it does not make it for you.
#'
#' \subsection{Why the reasons are descriptive}{
#' Every token names something observable about the parse. None is a risk
#' label, and none is intended as one: whether a scheme is safe depends on what
#' the caller does with the URL, which this package cannot observe. A
#' \code{scp:} URL is unremarkable to a mirroring tool and unacceptable in an
#' HTTP fetcher, and no property of the string distinguishes those cases.
#' }
#'
#' @param url A character vector of URLs.
#' @param allowed_schemes Optional character vector of schemes the caller will
#'   act on, compared case-insensitively. When supplied, the result gains a
#'   logical \code{allowed} column (\code{FALSE} for a URL with no parsed
#'   scheme) and the token \code{"not-in-allowlist"} where it is \code{FALSE}.
#'   When \code{NULL} (default) no \code{allowed} column is emitted and no
#'   allowlist judgement is made.
#' @param url_standard Standard profile governing scheme interpretation:
#'   \code{"whatwg"} (default) or \code{"rfc3986"}. Unlike most of the package
#'   this argument has a non-\code{NULL} default, because a scheme
#'   classification is undefined without a named standard.
#' @inheritParams safe_parse_url
#' @return A \code{data.frame} with one row per input URL (input order
#'   preserved): \code{url}, \code{scheme} (the parsed scheme, lowercased, or
#'   \code{NA}), \code{scheme_class} (\code{"special"}, \code{"non-special"} or
#'   \code{"missing-or-error"}), \code{web_scheme} (is the scheme one of the
#'   five \code{scheme_acceptance = "web"} admits), \code{allowed} (only when
#'   \code{allowed_schemes} is supplied), and \code{reasons} --- a list-column
#'   whose i-th element is a character vector of the scheme facts observed
#'   (\code{character(0)} when none). The tokens are \code{"no-scheme"},
#'   \code{"special-scheme"}, \code{"non-special-scheme"},
#'   \code{"outside-web-acceptance"} and \code{"not-in-allowlist"}.
#'
#'   There is deliberately no token for \dQuote{carries no authority}. It is
#'   the natural fact to want for \code{mailto:} and \code{javascript:}, but
#'   it cannot be computed from the public parse record:
#'   \code{get_host("mailto:someone@example.com")} returns
#'   \code{"example.com"}, reading the \code{@} as a userinfo delimiter, so a
#'   token derived from host presence would be wrong for exactly the schemes it
#'   is most wanted for. Reporting a fact this package cannot compute correctly
#'   would be worse than not reporting it.
#' @note \code{scheme_acceptance} defaults to \code{"general"} here, not to the
#'   package-wide \code{"web"}. Auditing which schemes a URL set carries is
#'   pointless under an acceptance mode that has already collapsed every
#'   non-web scheme into a parse error.
#' @seealso \code{\link{check_hosts}} for the host axis,
#'   \code{\link{get_scheme_class}}, \code{\link{get_url_diagnostics}}.
#' @export
#' @examples
#' urls <- c(
#'   "https://example.com/a",   # special, inside web acceptance
#'   "ftp://example.com/a",     # special, inside web acceptance
#'   "scp://host/a",            # non-special, outside web acceptance
#'   "smb://server/share",      # non-special, outside web acceptance
#'   "mailto:someone@example.com", # non-special, opaque path
#'   "javascript:alert(1)",     # non-special, opaque path
#'   "notaurl"                  # no scheme
#' )
#' check_schemes(urls)
#' # Score against the schemes an application will actually act on:
#' check_schemes(urls, allowed_schemes = c("https", "http"))
check_schemes <- function(url,
                          allowed_schemes = NULL,
                          url_standard = "whatwg",
                          scheme_policy = c("infer", "require"),
                          scheme_acceptance = c("general", "web")) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  if (!is.null(allowed_schemes)) {
    if (!is.character(allowed_schemes) || anyNA(allowed_schemes)) {
      stop(
        "`allowed_schemes` must be a character vector of scheme names ",
        "with no NA, or NULL.",
        call. = FALSE
      )
    }
    allowed_schemes <- unique(tolower(trimws(allowed_schemes)))
  }
  scheme_policy <- match.arg(scheme_policy)
  scheme_acceptance <- match.arg(scheme_acceptance)
  url_standard <- .validate_url_standard(url_standard)

  scheme <- get_scheme(url,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    url_standard = url_standard
  )
  scheme_class <- get_scheme_class(url,
    url_standard = url_standard,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance
  )
  have <- !is.na(scheme)
  web_scheme <- have & scheme %in% .SUPPORTED_SCHEMES

  out <- data.frame(
    url = url,
    scheme = scheme,
    scheme_class = scheme_class,
    web_scheme = web_scheme,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  allowed <- NULL
  if (!is.null(allowed_schemes)) {
    # A URL with no parsed scheme cannot satisfy an allowlist: there is nothing
    # to match. FALSE rather than NA, because the caller's question ("may I act
    # on this?") has a definite answer.
    allowed <- have & scheme %in% allowed_schemes
    out$allowed <- allowed
  }

  out$reasons <- lapply(seq_along(url), function(i) {
    toks <- character(0)
    if (have[i]) {
      toks <- c(toks, if (identical(scheme_class[i], "special")) {
        "special-scheme"
      } else {
        "non-special-scheme"
      })
      if (!web_scheme[i]) {
        toks <- c(toks, "outside-web-acceptance")
      }
    } else {
      toks <- c(toks, "no-scheme")
    }
    if (!is.null(allowed) && !allowed[i]) {
      toks <- c(toks, "not-in-allowlist")
    }
    unique(toks)
  })
  out
}
