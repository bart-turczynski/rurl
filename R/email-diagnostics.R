# Email / userinfo diagnostic vocabulary (ADR 0012 D7; PRD
# design/prd/email-userinfo-diagnostics.md; RURL-zyoahfpv).
#
# First slice: the per-recipient companion helper get_mailto_recipients() for
# the positional `to` of a mailto: URL. Structural FACTS only, never a gate
# (ADR 0012 D5) and never a new safe_parse_url column (ADR 0006). Each fact
# names the grammar it was judged against; the same source is projected as
# RFC 6068 mailto vocabulary (mailto_*) AND, independently, as an SMTP RFC 5321
# candidate (smtp_*). PSL knowledge is separate and explicitly non-validating.
#
# The SMTP wire-projection tier (octet lengths, SMTPUTF8, wire domain form) and
# the scheme-less `userinfo-form` URL-level fact are DEFERRED follow-up slices
# per the PRD's default-on/opt-in tiering.
#
# Provenance-preserving contract (PRD "Model"): the positional `to` arrives RAW
# (still percent-encoded) as the general-parser path; we tokenize BEFORE
# decoding so a `%2C` is never a recipient separator and an encoded quote/
# bracket (`%22`/`%5B`/`%5D`) still protects a raw comma, then percent-decode
# each field EXACTLY ONCE and classify the decoded token. We never decode the
# whole string first (that would turn `%2C` data into delimiters).

# --- Grammar character sets --------------------------------------------------

# RFC 5322 atext (obs excluded), shared by RFC 6068 dot-atom-text.
.EMAIL_ATEXT_CHARS <- c(
  LETTERS, letters, as.character(0:9),
  "!", "#", "$", "%", "&", "'", "*", "+", "-", "/",
  "=", "?", "^", "_", "`", "{", "|", "}", "~"
)

# --- Small structural predicates ---------------------------------------------

.email_has_non_ascii <- function(x) {
  stringi::stri_detect_regex(x, "[^\\u0000-\\u007F]")
}

# dot-atom-text = 1*atext *("." 1*atext): no leading/trailing/double dot, every
# non-dot character an atext.
.email_valid_dot_atom_text <- function(x) {
  if (!nzchar(x) || startsWith(x, ".") || endsWith(x, ".") ||
    grepl("..", x, fixed = TRUE)) {
    return(FALSE)
  }
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  all(ch %in% c(".", .EMAIL_ATEXT_CHARS)) && all(ch[ch != "."] %in%
    .EMAIL_ATEXT_CHARS)
}

# RFC 5322 quoted-string content (surrounding quotes already stripped):
# qtext (%d33 / %d35-91 / %d93-126) or quoted-pair ("\" followed by VCHAR/WSP).
# With allow_utf8 = TRUE (the SMTPUTF8 projection), non-ASCII scalar values are
# additionally admitted as qtext.
.email_valid_quoted_content <- function(inner, allow_utf8 = FALSE) {
  cs <- strsplit(inner, "", fixed = TRUE)[[1L]]
  i <- 1L
  n <- length(cs)
  while (i <= n) {
    ch <- cs[i]
    if (identical(ch, "\\")) {
      if (i == n) {
        return(FALSE) # dangling escape
      }
      code <- utf8ToInt(enc2utf8(cs[i + 1L]))[1L]
      if (!((code >= 33L && code <= 126L) || code == 32L || code == 9L)) {
        return(FALSE)
      }
      i <- i + 2L
    } else if (identical(ch, "\"")) {
      return(FALSE) # bare quote inside content
    } else {
      code <- utf8ToInt(enc2utf8(ch))[1L]
      ok <- code == 33L || (code >= 35L && code <= 91L) ||
        (code >= 93L && code <= 126L) || (allow_utf8 && code > 126L)
      if (!ok) {
        return(FALSE)
      }
      i <- i + 1L
    }
  }
  TRUE
}

# SMTPUTF8 dot-atom: like dot-atom-text but non-ASCII scalar values are admitted
# alongside ASCII atext (RFC 6531 extends atext with UTF8-non-ascii).
.email_valid_smtputf8_dot_atom <- function(x) {
  if (!nzchar(x) || startsWith(x, ".") || endsWith(x, ".") ||
    grepl("..", x, fixed = TRUE)) {
    return(FALSE)
  }
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  non_dot <- ch[ch != "."]
  all(non_dot %in% .EMAIL_ATEXT_CHARS | .email_has_non_ascii(non_dot))
}

# RFC 5321 sub-domain labels: Let-dig [*Ldh-str Let-dig], ASCII, no
# leading/trailing hyphen, dot-separated.
.email_valid_smtp_domain <- function(x) {
  if (!nzchar(x) || isTRUE(.email_has_non_ascii(x)) || startsWith(x, ".") ||
    endsWith(x, ".") || grepl("..", x, fixed = TRUE)) {
    return(FALSE)
  }
  labels <- strsplit(x, ".", fixed = TRUE)[[1L]]
  all(grepl("^[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?$", labels))
}

# RFC 5321 address-literal body (surrounding brackets already stripped): IPv4
# dotted-quad or an "IPv6:"-tagged literal. Bounded, not a full validator.
.email_valid_address_literal <- function(inner) {
  if (startsWith(inner, "IPv6:")) {
    v6 <- substring(inner, 6L)
    return(grepl(":", v6, fixed = TRUE) && grepl("^[0-9A-Fa-f:.]+$", v6))
  }
  oct <- strsplit(inner, ".", fixed = TRUE)[[1L]]
  length(oct) == 4L && all(grepl("^[0-9]{1,3}$", oct)) &&
    all(suppressWarnings(as.integer(oct)) <= 255L)
}

# --- The provenance-preserving lexer -----------------------------------------

# Explode a raw string into UNITS, where a `%XX` percent-triplet is one unit and
# every other byte is its own unit. Classification and separator detection then
# operate on units so an encoded delimiter is a single, recognizable token.
.email_units <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  n <- length(ch)
  units <- character(0)
  i <- 1L
  while (i <= n) {
    hex_next <- i + 2L <= n && grepl("^[0-9A-Fa-f]$", ch[i + 1L]) &&
      grepl("^[0-9A-Fa-f]$", ch[i + 2L])
    if (identical(ch[i], "%") && hex_next) {
      units <- c(units, paste0("%", ch[i + 1L], ch[i + 2L]))
      i <- i + 3L
    } else {
      units <- c(units, ch[i])
      i <- i + 1L
    }
  }
  units
}

# Structural role of a unit. A RAW delimiter and its percent-encoded spelling
# share a role (so `%22`/`%5B`/`%5D`/`%5C` protect commas just like raw ones);
# `%2C`/`%40` are DATA (role "other") and never act as separators.
.email_unit_role <- function(u) {
  up <- toupper(u)
  if (identical(u, "\"") || identical(up, "%22")) {
    "quote"
  } else if (identical(u, "[") || identical(up, "%5B")) {
    "bopen"
  } else if (identical(u, "]") || identical(up, "%5D")) {
    "bclose"
  } else if (identical(u, "\\") || identical(up, "%5C")) {
    "bslash"
  } else if (identical(u, ",")) {
    "comma"
  } else if (identical(u, "@")) {
    "at"
  } else {
    "other"
  }
}

# Split a raw positional `to` into raw recipient tokens on separator commas
# (raw comma outside quote and bracket context). Returns character(0) for an
# empty/NA `to`. A trailing separator yields a trailing empty token (an empty
# addr-spec, later classified invalid) -- we never silently drop recipients.
.email_split_recipients <- function(raw_to) {
  if (is.na(raw_to) || !nzchar(raw_to)) {
    return(character(0))
  }
  units <- .email_units(raw_to)
  in_quote <- FALSE
  in_bracket <- FALSE
  tokens <- character(0)
  cur <- character(0)
  i <- 1L
  n <- length(units)
  while (i <= n) {
    role <- .email_unit_role(units[i])
    if (in_quote && identical(role, "bslash")) {
      cur <- c(cur, units[i], if (i < n) units[i + 1L] else NULL)
      i <- i + 2L
      next
    }
    if (identical(role, "quote") && !in_bracket) {
      in_quote <- !in_quote
    } else if (identical(role, "bopen") && !in_quote) {
      in_bracket <- TRUE
    } else if (identical(role, "bclose") && !in_quote) {
      in_bracket <- FALSE
    } else if (identical(role, "comma") && !in_quote && !in_bracket) {
      tokens <- c(tokens, paste(cur, collapse = ""))
      cur <- character(0)
      i <- i + 1L
      next
    }
    cur <- c(cur, units[i])
    i <- i + 1L
  }
  c(tokens, paste(cur, collapse = ""))
}

# Split a raw recipient into (raw local-part, raw domain) at the STRUCTURAL `@`
# (a raw `@` outside quote and bracket context). Exactly one such `@` is a
# well-formed addr-spec; zero or many -> $ok = FALSE (malformed, still a
# resolved recipient). Never splits at an encoded `%40` or a quoted `@`.
.email_split_addr_spec <- function(raw_recipient) {
  units <- .email_units(raw_recipient)
  in_quote <- FALSE
  in_bracket <- FALSE
  at_positions <- integer(0)
  i <- 1L
  n <- length(units)
  while (i <= n) {
    role <- .email_unit_role(units[i])
    if (in_quote && identical(role, "bslash")) {
      i <- i + 2L
      next
    }
    if (identical(role, "quote") && !in_bracket) {
      in_quote <- !in_quote
    } else if (identical(role, "bopen") && !in_quote) {
      in_bracket <- TRUE
    } else if (identical(role, "bclose") && !in_quote) {
      in_bracket <- FALSE
    } else if (identical(role, "at") && !in_quote && !in_bracket) {
      at_positions <- c(at_positions, i)
    }
    i <- i + 1L
  }
  if (length(at_positions) != 1L) {
    return(list(ok = FALSE, local = NA_character_, domain = NA_character_))
  }
  p <- at_positions[1L]
  list(
    ok = TRUE,
    local = paste(units[seq_len(p - 1L)], collapse = ""),
    domain = paste(units[seq.int(p + 1L, n)], collapse = "")
  )
}

# --- Classifiers (operate on the ONCE-decoded field) -------------------------

.email_classify_local <- function(dec) {
  if (is.na(dec) || !nzchar(dec)) {
    return("invalid")
  }
  if (nchar(dec) >= 2L && startsWith(dec, "\"") && endsWith(dec, "\"")) {
    inner <- substring(dec, 2L, nchar(dec) - 1L)
    return(if (.email_valid_quoted_content(inner)) "quoted-string" else
      "invalid")
  }
  if (.email_valid_dot_atom_text(dec)) "dot-atom-text" else "invalid"
}

.email_classify_mailto_domain <- function(dec) {
  if (is.na(dec) || !nzchar(dec)) {
    return("invalid")
  }
  if (startsWith(dec, "[") && endsWith(dec, "]")) {
    return("bracketed-domain")
  }
  if (isTRUE(.email_has_non_ascii(dec))) {
    return(if (isTRUE(.validate_idna_domain_vec(dec))) "idna2008-domain" else
      "invalid")
  }
  if (.email_valid_dot_atom_text(dec)) "ascii-dot-atom-text" else "invalid"
}

.email_classify_smtp_rhs <- function(dec) {
  if (is.na(dec) || !nzchar(dec)) {
    return("invalid")
  }
  if (startsWith(dec, "[") && endsWith(dec, "]")) {
    inner <- substring(dec, 2L, nchar(dec) - 1L)
    return(if (.email_valid_address_literal(inner)) "address-literal" else
      "invalid")
  }
  if (.email_valid_smtp_domain(dec)) "domain" else "invalid"
}

# --- SMTP wire-projection facts (opt-in tier) --------------------------------
#
# These require an ACTUAL serialized wire projection of the address (RFC 5321
# transport, RFC 6531 SMTPUTF8), not just syntax. Octet counts are measured on
# the UTF-8 wire bytes, never on decoded character length. All are computed only
# when the caller opts in with `smtp_wire = TRUE`; otherwise the columns carry
# the "unavailable"/NA sentinel (see the helper).

# The domain's wire form. `dd` is the once-decoded domain; `dform`/`sform` are
# the already-computed mailto/SMTP-syntax classifications.
.email_smtp_domain_wire_form <- function(dd, dform, sform) {
  if (identical(sform, "address-literal")) {
    "address-literal"
  } else if (identical(dform, "idna2008-domain")) {
    # A non-ASCII IDNA2008-valid domain projects to Unicode U-labels on the
    # wire (which requires SMTPUTF8); its A-label projection would avoid it.
    "u-label-domain"
  } else if (identical(dform, "ascii-dot-atom-text") &&
    identical(sform, "domain")) {
    if (.host_is_ace(dd)) "a-label-domain" else "ascii-domain"
  } else {
    "unavailable"
  }
}

# SMTP local-part wire mode, judged as an RFC 5321 / RFC 6531 mailbox local-part
# (Dot-string / Quoted-string, optionally UTF8-extended) -- DELIBERATELY
# independent of the RFC 6068 `mailto_local_part_form` verdict, since a bare
# non-ASCII local-part is invalid RFC 6068 syntax yet is precisely the SMTPUTF8
# trigger. Returns "ascii", "smtputf8", or "unavailable".
.email_smtp_localpart_mode <- function(ld) {
  if (is.na(ld) || !nzchar(ld)) {
    return("unavailable")
  }
  ok <- if (nchar(ld) >= 2L && startsWith(ld, "\"") && endsWith(ld, "\"")) {
    .email_valid_quoted_content(substring(ld, 2L, nchar(ld) - 1L),
      allow_utf8 = TRUE)
  } else {
    .email_valid_smtputf8_dot_atom(ld)
  }
  if (!isTRUE(ok)) {
    "unavailable"
  } else if (isTRUE(.email_has_non_ascii(ld))) {
    "smtputf8"
  } else {
    "ascii"
  }
}

# Envelope wire mode for the whole address. SMTPUTF8 is required by a non-ASCII
# local-part alone, OR by a domain sent as U-labels (RFC 6531 sections 3.4/3.2;
# RFC 6530 section 4.2).
.email_smtp_envelope_wire_mode <- function(local_mode, domain_wire_form) {
  if (identical(domain_wire_form, "unavailable") ||
    identical(local_mode, "unavailable")) {
    "unavailable"
  } else if (identical(local_mode, "smtputf8") ||
    identical(domain_wire_form, "u-label-domain")) {
    "smtputf8"
  } else {
    "ascii"
  }
}

.email_octet_length <- function(x) {
  sum(nchar(x, type = "bytes"))
}

# --- Recipient extraction for the standard accessors (ADR 0012 D7) -----------

# First-recipient web-y extraction for a mailto: positional `to`. Returns, per
# input path, the recipient DOMAIN (decoded; only for a domain-form RHS -- NA
# for an address-literal / invalid / absent RHS, which is not a domain to
# decompose) as `host`, and the decoded local-part as `user` (whenever the
# addr-spec is well-formed, independent of the RHS form). Multi-recipient: the
# FIRST recipient (the full list is get_mailto_recipients()'s job). This feeds
# the standard accessors (get_host/get_domain/get_tld/get_subdomain/get_user)
# so a mailto recipient decomposes exactly like a web host; it is EXTRACTION
# metadata only and never touches clean_url / round-trip serialization.
.mailto_first_recipient_parts <- function(paths) {
  n <- length(paths)
  host <- rep(NA_character_, n)
  user <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    recips <- .email_split_recipients(paths[i])
    if (length(recips) == 0L) {
      next
    }
    split <- .email_split_addr_spec(recips[1L])
    if (!split$ok) {
      next
    }
    user[i] <- curl::curl_unescape(split$local)
    dd <- curl::curl_unescape(split$domain)
    if (.email_classify_mailto_domain(dd) %in%
      c("ascii-dot-atom-text", "idna2008-domain")) {
      host[i] <- dd
    }
  }
  list(host = host, user = user)
}

# --- Public helper -----------------------------------------------------------

#' Per-recipient email diagnostics for the mailto: positional recipient list
#'
#' Companion helper (ADR 0006) that reports structural, per-recipient
#' \emph{facts} about the recipients in the positional \code{to} of a
#' \code{mailto:} URL --- the comma-separated \code{addr-spec} list before the
#' \code{?} (RFC 6068 section 2). Recipients carried in \code{to}/\code{cc}/
#' \code{bcc} \emph{hfields} are RFC 5322 address-lists and are deliberately
#' \strong{out of scope}; only the positional list is analysed.
#'
#' Each fact \strong{names the grammar it was judged against}. The left of the
#' \code{addr-spec} is classified as an RFC 6068 \code{local-part} and,
#' independently, the right is classified both as RFC 6068 \code{mailto} domain
#' vocabulary and as an SMTP (RFC 5321) mailbox right-hand side --- these are
#' distinct grammars, so no single column spans them. Public-suffix knowledge is
#' reported separately and is explicitly \strong{non-validating}: a known suffix
#' is not mailbox validity.
#'
#' @section Facts, not a gate: These are \strong{selected structural facts}, not
#'   a conformance oracle and never a validator. A recipient's classification
#'   never turns a parse into an error, and the absence of an \code{invalid}
#'   value does not imply the address is deliverable or fully RFC-conformant.
#'   No DNS resolution or deliverability check is performed.
#'
#' @section SMTP wire-projection facts (opt-in): Set \code{smtp_wire = TRUE} to
#'   additionally compute the SMTP transport facts that require an actual
#'   serialized wire projection of the address (octet-length limits per RFC 5321
#'   section 4.5.3.1, and the SMTPUTF8 envelope mode per RFC 6531/6530). These
#'   are octet facts on the UTF-8 wire bytes, distinct from the syntax
#'   classifications above and from DNS. When \code{smtp_wire = FALSE} (the
#'   default) the five \code{smtp_*} wire columns are still present but carry
#'   the \code{"unavailable"}/\code{NA} sentinel; the same sentinel is used for
#'   a recipient whose address cannot be projected (an invalid mailbox).
#'
#' @section Provenance-preserving parse: The positional list is tokenized on the
#'   \strong{raw} (still percent-encoded) source before decoding, so an encoded
#'   comma (\code{\%2C}) is never a recipient separator and an encoded quote or
#'   bracket (\code{\%22}, \code{\%5B}/\code{\%5D}) still protects a raw comma;
#'   each field is then percent-decoded exactly once and classified.
#'
#' @param url A character vector of URLs. Non-\code{mailto:} URLs (and, under
#'   \code{scheme_acceptance = "web"}, all \code{mailto:} URLs, which the web
#'   allowlist does not accept) contribute no rows.
#' @param url_standard Standard profile passed to the general parser. A
#'   \code{mailto:} path is opaque, so this does not affect the classification;
#'   it defaults to \code{"rfc3986"} because the general parser requires a
#'   selector.
#' @param smtp_wire Logical; when \code{TRUE}, compute the opt-in SMTP
#'   wire-projection columns (see the corresponding section). Defaults to
#'   \code{FALSE}.
#' @inheritParams safe_parse_url
#' @return A \code{data.frame} (always, including for length-1 or all-empty
#'   input) with one row per positional-\code{to} recipient and columns:
#'   \describe{
#'     \item{\code{url}}{the source URL the recipient came from.}
#'     \item{\code{recipient_index}}{1-based index of the recipient within that
#'       URL's positional list.}
#'     \item{\code{mailto_local_part_form}}{RFC 6068 local-part form:
#'       \code{"dot-atom-text"}, \code{"quoted-string"}, \code{"invalid"}, or
#'       \code{"indeterminate"}.}
#'     \item{\code{mailto_domain_form}}{RFC 6068 domain form:
#'       \code{"ascii-dot-atom-text"}, \code{"idna2008-domain"},
#'       \code{"bracketed-domain"}, \code{"invalid"}, or
#'       \code{"indeterminate"}.}
#'     \item{\code{smtp_mailbox_rhs_syntax_form}}{RFC 5321 mailbox RHS form,
#'       independent of the mailto grammar and of DNS: \code{"domain"},
#'       \code{"address-literal"}, \code{"invalid"}, or
#'       \code{"indeterminate"}.}
#'     \item{\code{public_suffix_known}}{\code{TRUE}/\code{FALSE} whether the
#'       domain's public suffix is known to the PSL (non-validating);
#'       \code{NA} when the RHS is not a domain form.}
#'     \item{\code{smtp_domain_wire_form}}{(opt-in) \code{"ascii-domain"},
#'       \code{"a-label-domain"}, \code{"u-label-domain"},
#'       \code{"address-literal"}, or \code{"unavailable"}.}
#'     \item{\code{smtp_envelope_wire_mode}}{(opt-in) \code{"ascii"},
#'       \code{"smtputf8"}, or \code{"unavailable"}.}
#'     \item{\code{smtp_envelope_address_requires_smtputf8}}{(opt-in) logical;
#'       \code{NA} when no wire projection could be made.}
#'     \item{\code{smtp_local_part_length_ok}}{(opt-in) logical, serialized
#'       local-part at most 64 octets; \code{NA} when unavailable.}
#'     \item{\code{smtp_direct_forward_path_fits}}{(opt-in) logical, the octet
#'       length of \code{"<" + Mailbox + ">"} is at most 256; \code{NA} when
#'       unavailable. The familiar 254 is the RFC 3696 EID 1690 derivation of
#'       this path limit, not a standalone production.}
#'   }
#' @seealso \code{\link{get_url_diagnostics}}, \code{\link{safe_parse_url}}
#' @export
#' @examples
#' get_mailto_recipients("mailto:jane@example.com",
#'   scheme_acceptance = "general")
#' get_mailto_recipients(
#'   "mailto:a@example.com,\"b,c\"@example.org",
#'   scheme_acceptance = "general"
#' )
#' # opt-in SMTP wire-projection facts
#' get_mailto_recipients("mailto:a@xn--mnchen-3ya.de",
#'   scheme_acceptance = "general", smtp_wire = TRUE)
get_mailto_recipients <- function(url, url_standard = "rfc3986",
                                  scheme_policy = c("infer", "require"),
                                  scheme_acceptance = c("general", "web"),
                                  smtp_wire = FALSE) {
  if (!is.character(url)) {
    stop(
      "`url` must be a character vector of URL strings; ",
      "pass the URL, not a parsed object.",
      call. = FALSE
    )
  }
  smtp_wire <- .validate_flag(smtp_wire, "smtp_wire")
  opts <- .parse_options(url_standard = url_standard,
    scheme_policy = scheme_policy, scheme_acceptance = scheme_acceptance)

  empty <- data.frame(
    url = character(0),
    recipient_index = integer(0),
    mailto_local_part_form = character(0),
    mailto_domain_form = character(0),
    smtp_mailbox_rhs_syntax_form = character(0),
    public_suffix_known = logical(0),
    smtp_domain_wire_form = character(0),
    smtp_envelope_wire_mode = character(0),
    smtp_envelope_address_requires_smtputf8 = logical(0),
    smtp_local_part_length_ok = logical(0),
    smtp_direct_forward_path_fits = logical(0),
    stringsAsFactors = FALSE
  )
  if (length(url) == 0L) {
    return(empty)
  }

  gen <- .general_parse_vec(url, opts$url_standard, opts$scheme_acceptance)
  is_mailto <- gen$general_parsed &
    !is.na(gen$scheme) &
    stringi::stri_trans_tolower(gen$scheme) == "mailto"

  rows <- vector("list", length(url))
  for (i in which(is_mailto)) {
    recips <- .email_split_recipients(gen$path[i])
    if (length(recips) == 0L) {
      next
    }
    per <- lapply(recips, .email_recipient_facts, smtp_wire = smtp_wire)
    rows[[i]] <- data.frame(
      url = url[i],
      recipient_index = seq_along(recips),
      do.call(rbind.data.frame, c(per, list(
        stringsAsFactors = FALSE, make.row.names = FALSE
      ))),
      stringsAsFactors = FALSE
    )
  }
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(empty)
  }
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}

# Classify one raw recipient token into the per-recipient fact columns (without
# `url`/`recipient_index`, which the caller adds). Returns a one-row-ready named
# list. The five smtp_* wire facts carry the "unavailable"/NA sentinel unless
# `smtp_wire` is TRUE and the address projects.
.email_recipient_facts <- function(raw_recipient, smtp_wire) {
  out <- list(
    mailto_local_part_form = "invalid",
    mailto_domain_form = "invalid",
    smtp_mailbox_rhs_syntax_form = "invalid",
    public_suffix_known = NA,
    smtp_domain_wire_form = "unavailable",
    smtp_envelope_wire_mode = "unavailable",
    smtp_envelope_address_requires_smtputf8 = NA,
    smtp_local_part_length_ok = NA,
    smtp_direct_forward_path_fits = NA
  )
  split <- .email_split_addr_spec(raw_recipient)
  if (!split$ok) {
    return(out)
  }
  ld <- curl::curl_unescape(split$local)
  dd <- curl::curl_unescape(split$domain)
  out$mailto_local_part_form <- .email_classify_local(ld)
  out$mailto_domain_form <- .email_classify_mailto_domain(dd)
  out$smtp_mailbox_rhs_syntax_form <- .email_classify_smtp_rhs(dd)
  out$public_suffix_known <- if (out$mailto_domain_form %in%
    c("ascii-dot-atom-text", "idna2008-domain")) {
    !is.na(.psl_public_suffix(dd))
  } else {
    NA
  }
  if (!smtp_wire) {
    return(out)
  }

  local_mode <- .email_smtp_localpart_mode(ld)
  dwf <- .email_smtp_domain_wire_form(
    dd, out$mailto_domain_form, out$smtp_mailbox_rhs_syntax_form
  )
  wire_mode <- .email_smtp_envelope_wire_mode(local_mode, dwf)
  out$smtp_domain_wire_form <- dwf
  out$smtp_envelope_wire_mode <- wire_mode
  out$smtp_envelope_address_requires_smtputf8 <- if (
    identical(wire_mode, "unavailable")) {
    NA
  } else {
    identical(wire_mode, "smtputf8")
  }
  if (!identical(local_mode, "unavailable")) {
    out$smtp_local_part_length_ok <- .email_octet_length(ld) <= 64L
  }
  if (!identical(wire_mode, "unavailable")) {
    path <- paste0("<", ld, "@", dd, ">")
    out$smtp_direct_forward_path_fits <- .email_octet_length(path) <= 256L
  }
  out
}
