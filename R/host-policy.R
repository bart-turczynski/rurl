# Practical host validation POLICY helpers (RURL-clsmspfb; PRD
# _scratch/PRD-host-validation-policy.md, epic RURL-kdzwamop).
#
# A policy layer that sits ON TOP of standards-correct parsing. It answers the
# product question "is this parsed host usable as a practical web hostname / DNS
# owner name / registrable site host / SEO-safe host?" WITHOUT ever changing how
# a URL parses:
#
#   * It never turns a practical-hostname failure into a parse error, never
#     touches parse_status, and never widens safe_parse_url()'s fields -- it is
#     a companion helper composed from get_host_type() / get_url_diagnostics() /
#     get_host(), exactly the ADR 0006 contract.
#   * It is NOT a conformance oracle (ADR 0012 D5): "web = TRUE" means "no
#     practical LDH/length/hygiene footgun rurl checks for", not "provably valid
#     per every RFC". Absence of a `reasons` token is not a conformance claim.
#
# The one axis the raw parse facts cannot express is the underscore split
# (`domain-std3-violation` bundles `_` with `+`), so the web/dns char-class
# checks below run their own regex on the ASCII (idna) host form. Everything
# else -- registrability, hyphen/empty-label/length hygiene, IP-vs-name -- is
# already a fact the parser surfaces and this layer only reads it.

# The rule vocabulary (PRD S4). Each token is a policy predicate over the
# per-URL facts; the descriptive name is documentation.
.HOST_POLICY_RULES <- c("url", "dns", "web", "registrable", "seo")

# Host-shape diagnostic tokens surfaced verbatim as `reasons` evidence. A subset
# of .URL_DIAGNOSTICS restricted to the host facts (domain hygiene + IPv4
# shorthand + the whatwg host-charset shim); path/port/scheme facts are not host
# policy and are left to get_url_diagnostics().
.HOST_SHAPE_DIAGNOSTICS <- c(
  "domain-label-too-long", "domain-name-too-long", "domain-empty-label",
  "domain-hyphen-violation", "domain-std3-violation", "host-charset-shimmed",
  "ipv4-number-form", "ipv4-non-dotted", "ipv4-short-form",
  "ipv4-non-decimal", "ipv4-octal", "ipv4-leading-zero", "ipv4-out-of-range"
)

# Footgun tokens that disqualify an otherwise-registrable host from `seo`: the
# domain hygiene facts plus the IPv4-shorthand facts (an IPv4 host is not
# registrable, but the guard is kept explicit and future-proof). The whatwg
# host-charset shim is deliberately excluded -- a shimmed host is already caught
# by the `web` LDH check.
.HOST_SEO_FOOTGUNS <- c(
  "domain-label-too-long", "domain-name-too-long", "domain-empty-label",
  "domain-hyphen-violation", "domain-std3-violation",
  "ipv4-number-form", "ipv4-non-dotted", "ipv4-short-form",
  "ipv4-non-decimal", "ipv4-octal", "ipv4-leading-zero", "ipv4-out-of-range"
)

# Per-label char classes, anchored, case-insensitive. `web` is strict LDH (no
# leading/trailing hyphen); `dns` additionally permits the underscore anywhere
# (RFC 1035 preferred-name syntax + the underscore that SRV/DMARC/DKIM owner
# names rely on).
.RE_HOST_WEB_LABEL <- "^[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?$"
.RE_HOST_DNS_LABEL <- "^[A-Za-z0-9_]([A-Za-z0-9_-]*[A-Za-z0-9_])?$"

# Char-class + length verdict for one ASCII host string (the idna form). Returns
# c(web=, dns=) logicals; both FALSE for an absent/empty host. A single trailing
# root dot is stripped first (a legal FQDN spelling). Length rules: each label
# 1-63 octets, no empty label, and the whole name (labels + separating dots)
# <= 253.
.host_label_shape <- function(h) {
  if (is.na(h) || !nzchar(h)) {
    return(c(web = FALSE, dns = FALSE))
  }
  h <- sub("\\.$", "", h)
  if (!nzchar(h)) {
    return(c(web = FALSE, dns = FALSE))
  }
  labels <- strsplit(h, ".", fixed = TRUE)[[1]]
  lens <- nchar(labels)
  len_ok <- length(labels) >= 1L &&
    all(lens >= 1L & lens <= 63L) &&
    sum(lens) + length(labels) - 1L <= 253L
  if (!len_ok) {
    return(c(web = FALSE, dns = FALSE))
  }
  c(
    web = all(grepl(.RE_HOST_WEB_LABEL, labels)),
    dns = all(grepl(.RE_HOST_DNS_LABEL, labels))
  )
}

# Gather every per-URL fact the two exported helpers need, in one shared pass so
# is_valid_host() and check_hosts() agree by construction. Returns a list with
# `host` (parsed, keep form -- for display), `host_type`, the named list of
# per-rule logical vectors (NA where there is no host to judge), and the
# per-URL `reasons` evidence list.
.gather_host_policy <- function(url, url_standard, scheme_policy,
                                scheme_acceptance) {
  n <- length(url)
  opts <- .parse_options(
    url_standard = url_standard,
    scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance
  )
  meta <- ._url_metadata_vec(url, opts)
  host_type <- meta$host_type
  diag <- meta$diagnostics

  host_keep <- get_host(url,
    url_standard = url_standard, scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance, host_encoding = "keep"
  )
  host_idna <- get_host(url,
    url_standard = url_standard, scheme_policy = scheme_policy,
    scheme_acceptance = scheme_acceptance, host_encoding = "idna"
  )
  # idna can be NA for a host it cannot A-label-encode; fall back to the parsed
  # host so the char-class check still runs (and conservatively fails on a
  # genuinely non-LDH byte) rather than short-circuiting to NA.
  ascii_host <- ifelse(is.na(host_idna), host_keep, host_idna)

  present <- host_type %in% c("domain", "ipv4", "ipv6", "reg-name")
  is_ip <- host_type %in% c("ipv4", "ipv6")
  is_domain <- !is.na(host_type) & host_type == "domain"
  is_name <- host_type %in% c("domain", "reg-name")

  shape <- lapply(seq_len(n), function(i) {
    if (isTRUE(is_name[i])) {
      .host_label_shape(ascii_host[i])
    } else {
      c(web = FALSE, dns = FALSE)
    }
  })
  web_ldh <- vapply(shape, `[[`, logical(1), "web")
  dns_ldh <- vapply(shape, `[[`, logical(1), "dns")

  has_underscore <- is_name & !is.na(ascii_host) &
    grepl("_", ascii_host, fixed = TRUE)
  has_footgun <- vapply(
    diag, function(toks) any(toks %in% .HOST_SEO_FOOTGUNS), logical(1)
  )

  web_r <- present & (is_ip | (is_name & web_ldh))
  reg_r <- present & is_domain
  rules <- list(
    url = present,
    dns = present & is_name & dns_ldh,
    web = web_r,
    registrable = reg_r,
    seo = web_r & reg_r & !has_footgun
  )
  # No host to judge -> NA (never FALSE): the rule simply does not apply.
  no_host <- !present
  rules <- lapply(rules, function(x) {
    x[no_host] <- NA
    x
  })

  reasons <- lapply(seq_len(n), function(i) {
    if (!present[i]) {
      return(character(0))
    }
    toks <- diag[[i]][diag[[i]] %in% .HOST_SHAPE_DIAGNOSTICS]
    if (isTRUE(is_ip[i])) {
      toks <- c(toks, "ip-literal")
    }
    if (isTRUE(is_name[i]) && !isTRUE(is_domain[i])) {
      toks <- c(toks, "not-registrable")
    }
    if (isTRUE(has_underscore[i])) {
      toks <- c(toks, "underscore-label")
    }
    unique(toks)
  })

  list(
    host = host_keep, host_type = host_type,
    rules = rules, reasons = reasons
  )
}

#' Test whether each URL's host satisfies a practical validation rule
#'
#' A policy predicate layered on top of standards-correct parsing. rurl's
#' \code{url_standard} profiles deliberately match the URL \emph{standards}
#' rather than impose practical web/SEO hygiene, so hosts such as
#' \code{a+b.example} (a valid RFC 3986 reg-name), \code{_dmarc.example.com} (a
#' valid DNS owner name), or \code{-example.com} all parse successfully.
#' \code{is_valid_host()} answers the separate, product-level question of
#' whether such a host is usable as a practical web hostname, DNS owner name,
#' registrable site host, or SEO-safe host.
#'
#' @section A policy layer, not parser conformance: This never changes how a URL
#'   parses: it does not turn a hostname-policy failure into a parse error, does
#'   not affect \code{\link{get_parse_status}}, and adds no columns to
#'   \code{\link{safe_parse_url}}. It is also \strong{not} a conformance oracle
#'   (see \code{\link{get_url_diagnostics}} and ADR 0012): a \code{TRUE} verdict
#'   means only that rurl found no practical footgun it checks for, never that
#'   the host is provably valid per every specification.
#'
#' @param url A character vector of URLs.
#' @param rule A single rule to test. One of:
#'   \describe{
#'     \item{\code{"web"}}{(default) usable as an HTTP-authority host: an IP
#'       literal, or a strict letter-digit-hyphen (LDH) name host (labels 1--63
#'       octets, no leading/trailing hyphen, no underscore, name
#'       \eqn{\le} 253).}
#'     \item{\code{"dns"}}{a legal DNS owner-name shape --- the \code{"web"}
#'       rule but the underscore is permitted anywhere (as \code{_dmarc},
#'       \code{_dkim}, SRV owner names require).}
#'     \item{\code{"registrable"}}{the host has a Public Suffix List registrable
#'       domain (equivalently \code{\link{get_host_type}} is \code{"domain"});
#'       IP literals and single-label / unknown-suffix hosts are \code{FALSE}.}
#'     \item{\code{"seo"}}{SEO-safe: \code{"web"} and \code{"registrable"} and
#'       carrying no host-shape footgun diagnostic (e.g. an IPv4 written in a
#'       numeric or non-decimal shorthand).}
#'     \item{\code{"url"}}{the loosest rule: the selected standard admitted a
#'       host at all.}
#'   }
#' @param url_standard Standard profile governing host interpretation:
#'   \code{"whatwg"} (default; the living web standard) or \code{"rfc3986"}. The
#'   verdict can depend on the standard --- \code{2130706433} is an
#'   \code{"ipv4"} host (\code{web = TRUE}) under \code{"whatwg"} but a reg-name
#'   under \code{"rfc3986"}.
#' @inheritParams safe_parse_url
#' @return A logical vector the same length as \code{url}. \code{NA} for a URL
#'   with no host to judge (a host-less scheme, or an input that did not parse).
#' @seealso \code{\link{check_hosts}} for a tabular report over several rules at
#'   once, \code{\link{get_host_type}}, \code{\link{get_url_diagnostics}}.
#' @export
#' @examples
#' is_valid_host(c("http://example.com", "http://_dmarc.example.com"))
#' # web: FALSE for the underscore host; dns: TRUE for it
#' is_valid_host("http://_dmarc.example.com", rule = "dns")
#' is_valid_host(
#'   c("http://a+b.example", "http://-example.com", "http://a..com"),
#'   rule = "web"
#' )
#' is_valid_host(
#'   c("http://example.com", "http://localhost", "http://192.168.0.1"),
#'   rule = "registrable"
#' )
is_valid_host <- function(url,
                          rule = c("web", "dns", "registrable", "seo", "url"),
                          url_standard = "whatwg",
                          scheme_policy = c("infer", "require"),
                          scheme_acceptance = c("web", "general")) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  rule <- match.arg(rule)
  scheme_policy <- match.arg(scheme_policy)
  scheme_acceptance <- match.arg(scheme_acceptance)
  url_standard <- .validate_url_standard(url_standard)
  .gather_host_policy(url, url_standard, scheme_policy, scheme_acceptance)$
    rules[[rule]]
}

#' Report practical host validation verdicts for a set of URLs
#'
#' Tabular companion to \code{\link{is_valid_host}}: for each URL, reports the
#' parsed host, its \code{\link{get_host_type}} classification, a logical column
#' for each requested policy rule, and a \code{reasons} list-column naming the
#' host facts observed. Useful for auditing a URL set before choosing what to
#' keep --- see \emph{why} a host is not a practical web/SEO host, not just that
#' it is not.
#'
#' Like \code{\link{is_valid_host}} this is a \strong{policy} layer, not parser
#' conformance and not a conformance oracle: it never changes how a URL parses
#' and the absence of a \code{reasons} token is not a validity guarantee (see
#' \code{\link{is_valid_host}}'s \dQuote{A policy layer} section).
#'
#' @param url A character vector of URLs.
#' @param rules A character vector of one or more rules to score, drawn from
#'   \code{"url"}, \code{"dns"}, \code{"web"}, \code{"registrable"},
#'   \code{"seo"} (all five by default). See \code{\link{is_valid_host}} for
#'   each rule's meaning.
#' @param url_standard Standard profile governing host interpretation:
#'   \code{"whatwg"} (default) or \code{"rfc3986"}.
#' @inheritParams safe_parse_url
#' @return A \code{data.frame} with one row per input URL (input order
#'   preserved): \code{url}, \code{host} (the parsed host, or \code{NA}),
#'   \code{host_type}, one logical column per requested rule (\code{NA} when the
#'   URL has no host to judge; the \code{"url"} rule's column is named
#'   \code{url_valid} so it does not shadow the input \code{url} column), and
#'   \code{reasons} --- a list-column whose i-th
#'   element is a character vector of the host facts observed for that URL
#'   (\code{character(0)} when none). The \code{reasons} evidence is reported
#'   for the row as a whole, independent of which \code{rules} were requested,
#'   and combines the host-shape diagnostics that fired (see
#'   \code{\link{get_url_diagnostics}}) with the policy tokens
#'   \code{"ip-literal"}, \code{"not-registrable"}, and
#'   \code{"underscore-label"}.
#' @seealso \code{\link{is_valid_host}}, \code{\link{get_host_type}},
#'   \code{\link{get_url_diagnostics}}.
#' @export
#' @examples
#' urls <- c(
#'   "http://example.com",         # registrable domain
#'   "http://_dmarc.example.com",  # valid DNS owner name, not a web hostname
#'   "http://a+b.example",         # valid RFC reg-name, neither web nor dns
#'   "http://-example.com",        # hyphen hygiene failure
#'   "http://a..com",              # empty label
#'   "http://localhost",           # web hostname, but not registrable
#'   "http://192.168.0.1",         # IP literal
#'   "http://xn--nxasmq6b.example.com" # IDN (A-label)
#' )
#' check_hosts(urls)
#' # Score a single rule, or a subset:
#' check_hosts(urls, rules = c("web", "dns"))
#' # RFC 3986 reg-name semantics instead of whatwg:
#' check_hosts("http://2130706433", url_standard = "rfc3986")
check_hosts <- function(url,
                        rules = c("url", "dns", "web", "registrable", "seo"),
                        url_standard = "whatwg",
                        scheme_policy = c("infer", "require"),
                        scheme_acceptance = c("web", "general")) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  rules <- match.arg(rules, .HOST_POLICY_RULES, several.ok = TRUE)
  scheme_policy <- match.arg(scheme_policy)
  scheme_acceptance <- match.arg(scheme_acceptance)
  url_standard <- .validate_url_standard(url_standard)

  g <- .gather_host_policy(url, url_standard, scheme_policy, scheme_acceptance)
  out <- data.frame(
    url = url,
    host = g$host,
    host_type = g$host_type,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  for (rule in rules) {
    # The `url` rule's column is emitted under its descriptive name `url_valid`
    # so it never shadows the input `url` column; the other tokens are unique.
    col <- if (identical(rule, "url")) "url_valid" else rule
    out[[col]] <- g$rules[[rule]]
  }
  # Assigning a length-n list makes `reasons` a proper list-column.
  out$reasons <- g$reasons
  out
}
