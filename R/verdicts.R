# Layered validation verdicts (P1.1 §2 + P2.3 §1-§5, projected by G3.6's
# validation-intervention contract; RURL-glkuulyr).
#
# The single `parse_status` column conflates three INDEPENDENT questions. This
# file separates them and then projects them back:
#
#   L1 syntax   -- did the input present a well-formed URL under the selected
#                  standard? Fatality: `fail` is structural.
#   L2 policy   -- is an already-parsed object admitted, and with what note,
#                  under the active acceptance/relative/userinfo policy?
#                  Fatality: `rejected-scheme` rejects; `warn-userinfo` accepts
#                  with a note.
#   L3 annotation -- the PSL registrability fact, as a TYPED state rather than a
#                  bare NA. NEVER fatal, and it MUST NOT change L1 or L2.
#
# `parse_status` is retained unchanged as the total, pure projection
# pi(L1, L2, L3) below -- the sole locus at which a PSL fact reaches a
# status-shaped output. The projection is LOSSY by contract: an L1 structural
# failure and an L2 admission rejection both collapse to `error`, and only the
# companion `get_parse_verdicts()` recovers which one occurred.
#
# BYTE-IDENTITY IS STRUCTURAL, NOT PARALLEL. `.derive_parse_status_vec()` is
# implemented AS `.project_parse_status_vec(.derive_verdict_layers_vec(...))`,
# so there is exactly one status-deciding code path. A second implementation
# kept "in sync by testing" is the duplication this epic keeps paying for.

# --- vocabularies ------------------------------------------------------------

# L1 (P2.3 §1). `fail` is FATAL (structural).
.LAYER1_SYNTAX_VERDICT <- c("pass", "fail")

# L2 (P2.3 §1). `rejected-scheme` is FATAL (admission reject); `warn-userinfo`
# is accept-with-note; the `admitted*` values are non-fatal accepts.
.LAYER2_POLICY_VERDICT <- c(
  "admitted", "admitted-scheme-relative", "admitted-ftp",
  "rejected-scheme", "warn-userinfo"
)

# L3 (P2.3 §2, the S7-F3 typed states). Never fatal.
#
# The shipped engine produces three of these six. `not-requested` needs a lazy
# annotation the always-on pipeline does not have; `invalid-input` needs the
# annotation to reject a host URL syntax accepted; `dependency-error` needs a
# trapped `pslr` failure, and P2.3 §4 Q3 explicitly scopes the v2 byte-identity
# guarantee to inputs whose lookup RUNS, which is why no shipped path emits it.
# The three states are enumerated here because the enum is SETTLED, and the
# gap is asserted rather than left silent (see `test-parse-verdicts.R`).
.LAYER3_ANNOTATION_STATE <- c(
  "not-requested", "not-applicable", "known", "unknown",
  "invalid-input", "dependency-error"
)

# INTERNAL L3 refinement. pi's rows 4-6 need to know WHICH `unknown` and WHICH
# `known` occurred; carrying that as a private detail column keeps pi a pure
# function of the verdict state instead of re-deriving from raw host facts.
.LAYER3_DETAIL <- c(
  "not-applicable", "unknown-no-dot", "unknown-dot",
  "known-suffix-only", "known-registrable"
)

.LAYER3_DETAIL_TO_STATE <- c(
  "not-applicable"    = "not-applicable",
  "unknown-no-dot"    = "unknown",
  "unknown-dot"       = "unknown",
  "known-suffix-only" = "known",
  "known-registrable" = "known"
)

# --- layer derivation --------------------------------------------------------

# Derive the three independent verdict layers for a vector of already-parsed
# rows. Arguments are the same facts Phase 12 has always consumed, plus
# `scheme_less_userinfo` (the D5 flag that used to be applied as a post-hoc
# override of the finished status -- see `.project_parse_status_vec`).
#
# Returns a columnar list: `layer1_syntax_verdict`, `layer2_policy_verdict`,
# `layer3_annotation_state`, and the private `layer3_detail`.
.derive_verdict_layers_vec <- function(curl_ok, final_host, is_ip_host, tld,
                                       domain, protocol_handling, final_scheme,
                                       looks_like_protocol,
                                       original_has_allowed_scheme,
                                       looks_like_host_port,
                                       is_scheme_relative,
                                       scheme_relative_handling,
                                       rfc3986_path_rootless = NULL,
                                       scheme_acceptance = "web",
                                       is_general = NULL,
                                       scheme_less_userinfo = NULL) {
  n <- length(final_host)
  if (is.null(rfc3986_path_rootless)) {
    rfc3986_path_rootless <- rep(FALSE, n)
  }
  if (is.null(is_general)) {
    is_general <- rep(FALSE, n)
  }
  if (is.null(scheme_less_userinfo)) {
    scheme_less_userinfo <- rep(FALSE, n)
  }
  curl_ok <- rep_len(curl_ok, n)

  scheme_lower <- .ascii_tolower(final_scheme)
  is_file <- curl_ok & !is.na(scheme_lower) & scheme_lower == "file"
  is_rootless <- curl_ok & rfc3986_path_rootless
  is_general_ok <- curl_ok & is_general
  host_present <- curl_ok & !is.na(final_host) & final_host != ""
  # The three shapes that legitimately carry no host: a `file:` URL, an RFC
  # rootless path, and a general-routed opaque / non-special-authority row.
  host_optional <- is_file | is_rootless | is_general_ok

  # --- L2 policy ------------------------------------------------------------
  # Computed BEFORE L1 because an admission rejection explains an absent parse
  # (see L1 below). Assigned in ascending precedence so the fatal value wins.
  layer2 <- rep("admitted", n)

  # FTP-family retention (shipped `ok-ftp`). Deliberately NOT conditioned on the
  # PSL outcome the way the shipped cascade was: L3 must not decide L2 (P1.1
  # §2). The shipped behavior -- an FTP host with no registrable domain reports
  # the PSL warning, not `ok-ftp` -- is reproduced by pi's row ORDER instead,
  # which considers the L3 warnings (rows 4-6) before `admitted-ftp` (row 8).
  if (!identical(protocol_handling, "strip")) {
    is_ftp <- host_present & !is.na(scheme_lower) &
      scheme_lower %in% c("ftp", "ftps")
    layer2[is_ftp] <- "admitted-ftp"
  }

  # Scheme-relative retention (shipped `ok-scheme-relative`). Same treatment:
  # unconditional here, ordered in pi (row 7).
  if (identical(scheme_relative_handling, "keep")) {
    rel <- is_scheme_relative & !is.na(is_scheme_relative)
    layer2[rel] <- "admitted-scheme-relative"
  }

  # D5 scheme-less userinfo (`user@example.com`): accepted with a note.
  slu <- scheme_less_userinfo & !is.na(scheme_less_userinfo) & curl_ok
  layer2[slu] <- "warn-userinfo"

  # ADR 0012 D3: under `web` an unsupported scheme-bearing token is demoted.
  # This is a POLICY rejection, not a syntax failure -- the distinction the
  # single `error` value destroys and this companion surface exists to recover.
  if (identical(scheme_acceptance, "web")) {
    unsupported <- looks_like_protocol &
      !original_has_allowed_scheme &
      !looks_like_host_port
    unsupported[is.na(unsupported)] <- FALSE
    layer2[unsupported] <- "rejected-scheme"
  }
  rejected <- layer2 == "rejected-scheme"

  # --- L1 syntax ------------------------------------------------------------
  # `fail` reports an OBSERVED syntax failure, never a proof of well-formedness
  # in the other direction (the S2-01 absence-is-not-conformance rule): a row
  # the admission gate rejected before any parse ran carries its rejection in
  # L2 and is not additionally reported as a syntax failure, since rurl never
  # evaluated its syntax.
  layer1 <- rep("pass", n)
  layer1[!curl_ok & !rejected] <- "fail"
  # A row curl accepted but which resolved no host, in a shape where a host is
  # not optional, did not yield a well-formed URL either. Empirically this cell
  # is unreached across the committed corpora; it is handled so pi stays TOTAL.
  layer1[curl_ok & !host_present & !host_optional & !rejected] <- "fail"

  # --- L3 annotation --------------------------------------------------------
  # The registrability cascade, verbatim from the shipped one. Everything with
  # no registrable-domain CONCEPT -- IP literal, `file:` host, general/opaque
  # authority, and any row with no host to annotate -- is `not-applicable`,
  # never `unknown`: a lookup that does not apply is not a lookup that failed.
  host_has_dot <- stringi::stri_detect_fixed(final_host, ".")
  host_has_dot[is.na(host_has_dot)] <- FALSE
  tld_empty <- is.na(tld) | !nzchar(tld)
  domain_empty <- is.na(domain) | !nzchar(domain)

  detail <- rep("not-applicable", n)
  non_ip <- host_present & !is_ip_host & !is_file & !is_general_ok
  detail[non_ip & !host_has_dot] <- "unknown-no-dot"
  detail[non_ip & host_has_dot & tld_empty] <- "unknown-dot"
  detail[non_ip & host_has_dot & !tld_empty & domain_empty] <-
    "known-suffix-only"
  detail[non_ip & host_has_dot & !tld_empty & !domain_empty] <-
    "known-registrable"

  list(
    layer1_syntax_verdict = layer1,
    layer2_policy_verdict = layer2,
    layer3_annotation_state = unname(.LAYER3_DETAIL_TO_STATE[detail]),
    layer3_detail = detail
  )
}

# --- pi: the legacy `parse_status` projection --------------------------------

# pi(L1, L2, L3) -> the eight-value `parse_status` vocabulary. TOTAL and PURE:
# every combination projects, and nothing outside the verdict state is read.
# Rows are evaluated top to bottom, first match wins (P2.3 §4):
#
#   1  L1 fail                          -> error            (structural)
#   2  L2 rejected-scheme               -> error            (admission)
#   3  L2 warn-userinfo                 -> warning-userinfo
#   4  L3 unknown, host has no dot      -> warning-no-tld
#   5  L3 unknown, host has a dot       -> warning-invalid-tld
#   6  L3 known, suffix-only            -> warning-public-suffix
#   7  L2 admitted-scheme-relative      -> ok-scheme-relative
#   8  L2 admitted-ftp                  -> ok-ftp
#   9  otherwise                        -> ok
#
# Written as descending-precedence mask assignment (row 9 first, row 1 last) so
# the earlier rows overwrite the later ones -- the repo's vectorized idiom, and
# the same shape the status derivation has always had.
.project_parse_status_vec <- function(v) {
  l1 <- v$layer1_syntax_verdict
  l2 <- v$layer2_policy_verdict
  detail <- v$layer3_detail
  status <- rep(.STATUS_OK, length(l1))

  status[l2 == "admitted-ftp"] <- .STATUS_OK_FTP                       # row 8
  status[l2 == "admitted-scheme-relative"] <- .STATUS_OK_SCHEME_REL    # row 7
  status[detail == "known-suffix-only"] <- .STATUS_WARN_PUBLIC_SUFFIX  # row 6
  status[detail == "unknown-dot"] <- .STATUS_WARN_INVALID_TLD          # row 5
  status[detail == "unknown-no-dot"] <- .STATUS_WARN_NO_TLD            # row 4
  status[l2 == "warn-userinfo"] <- .STATUS_WARN_USERINFO               # row 3
  status[l2 == "rejected-scheme"] <- .STATUS_ERROR                     # row 2
  status[l1 == "fail"] <- .STATUS_ERROR                                # row 1
  status
}

# --- public companion --------------------------------------------------------

#' Report the layered validation verdicts for each URL
#'
#' Companion helper that separates the three independent questions
#' \code{\link{get_parse_status}} collapses into one value: did the input
#' present well-formed URL syntax (layer 1), was the parsed object admitted
#' under the active policy (layer 2), and what did the Public Suffix List
#' annotation find (layer 3).
#'
#' The single \code{parse_status} value is a \emph{lossy} projection of these
#' three. The guaranteed loss is that a structural syntax failure and a policy
#' rejection both surface as \code{"error"}: under the default
#' \code{scheme_acceptance = "web"}, \code{"mailto:jane@example.com"} and
#' \code{"http://"} are both \code{"error"}, but the first is
#' \code{layer2_policy_verdict = "rejected-scheme"} (rurl declined to accept the
#' scheme) while the second is \code{layer1_syntax_verdict = "fail"} (it did not
#' parse). Recovering that distinction is what this helper is for.
#'
#' Like the other companion helpers this never widens the
#' \code{\link{safe_parse_url}} frame --- the parse table keeps its 18 columns
#' and the verdicts live here. Unlike \code{\link{get_host_type}} and
#' \code{\link{get_scheme_class}}, it is fully defined at
#' \code{url_standard = NULL}: layers 1 and 2 describe the parse that actually
#' occurred, which happens with or without a standard selector.
#'
#' @section Layers:
#'   \strong{Layer 1 --- syntax} (\code{"pass"} / \code{"fail"}). Whether a
#'   syntax failure was \emph{observed}. \code{"pass"} is the absence of an
#'   observed failure, not a proof of conformance --- a row the admission gate
#'   rejected before any parse ran reports its rejection in layer 2 and is not
#'   additionally reported as a syntax failure.
#'
#'   \strong{Layer 2 --- policy} (\code{"admitted"},
#'   \code{"admitted-scheme-relative"}, \code{"admitted-ftp"},
#'   \code{"rejected-scheme"}, \code{"warn-userinfo"}). Whether the object is
#'   admitted under the active \code{scheme_acceptance} /
#'   \code{scheme_relative_handling} / userinfo policy, and with what note.
#'   \code{"rejected-scheme"} rejects; \code{"warn-userinfo"} accepts with a
#'   note; the \code{admitted*} values accept.
#'
#'   \strong{Layer 3 --- annotation} (\code{"not-applicable"},
#'   \code{"known"}, \code{"unknown"}, and the currently unproduced
#'   \code{"not-requested"}, \code{"invalid-input"}, \code{"dependency-error"}).
#'   The PSL registrability fact as a typed state rather than a bare
#'   \code{NA}, and \strong{never fatal}: a host with no public suffix is a
#'   \code{"unknown"} annotation, not a parse failure. A host form with no
#'   registrable-domain concept at all --- an IP literal, a \code{file:} host,
#'   an opaque authority --- is \code{"not-applicable"}, never \code{"unknown"}.
#'
#' @param url A character vector of URLs.
#' @param url_standard Standard profile governing interpretation: \code{NULL}
#'   (default), \code{"rfc3986"}, or \code{"whatwg"}. Unlike the other
#'   companion helpers this argument does not gate the result.
#' @inheritParams safe_parse_url
#' @return A data frame with one row per element of \code{url} and three
#'   character columns: \code{layer1_syntax_verdict},
#'   \code{layer2_policy_verdict}, and \code{layer3_annotation_state}.
#' @seealso \code{\link{get_parse_status}}, \code{\link{get_url_diagnostics}},
#'   \code{\link{safe_parse_url}}
#' @export
#' @examples
#' # Both are parse_status "error" -- for entirely different reasons.
#' get_parse_verdicts(c("mailto:jane@example.com", "http://"))
#'
#' # A PSL miss is an annotation state, never a fatal verdict.
#' get_parse_verdicts("http://example.invalidtld/")
get_parse_verdicts <- function(url, url_standard = NULL,
                               protocol_handling = c(
                                 "keep", "none", "strip", "http", "https"
                               ),
                               scheme_relative_handling = c(
                                 "keep", "http", "https", "error"
                               ),
                               scheme_policy = c("infer", "require"),
                               scheme_acceptance = c("web", "general"),
                               tld_source = c("all", "private", "icann")) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  opts <- .parse_options(
    url_standard = .validate_url_standard(url_standard),
    protocol_handling = protocol_handling,
    scheme_relative_handling = scheme_relative_handling,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance,
    tld_source = tld_source
  )
  if (length(url) == 0L) {
    return(data.frame(
      layer1_syntax_verdict = character(0),
      layer2_policy_verdict = character(0),
      layer3_annotation_state = character(0),
      stringsAsFactors = FALSE
    ))
  }
  # The verdicts ride the SAME engine pass the status does -- read off the
  # internal attribute rather than recomputed here, so the companion can never
  # drift from the projection (see the byte-identity note at the top).
  v <- attr(._parse_urls_cached(unname(url), opts), "verdicts")
  data.frame(
    layer1_syntax_verdict = v$layer1_syntax_verdict,
    layer2_policy_verdict = v$layer2_policy_verdict,
    layer3_annotation_state = v$layer3_annotation_state,
    stringsAsFactors = FALSE
  )
}
