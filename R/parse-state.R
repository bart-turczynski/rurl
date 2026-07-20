# Internal parse STATE-MODEL vocabulary + pure classifiers (ADR 0012 Layer 3a,
# RURL-tzkcbvvt).
#
# This file is the foundation of Layer 3: it defines the internal state-model
# vocabulary and the pure, vectorized classifier functions that later layers
# CONSUME -- L3b (serializers), L3c (the Stage-B eligibility matrix), and L4b
# (host parsers). It is deliberately ADDITIVE: nothing here is wired into the
# live parse pipeline yet, so byte-identity of every existing output is
# trivially preserved. The classifiers are PURE: they take already-decomposed
# pieces (scheme special-ness, the remainder after the scheme, an isolated host,
# a `//` flag, a raw component value) and never parse, never touch curl, and
# never route through the punycode/domain helpers (ADR 0002).
#
# Why a richer state model at all: ADR 0012 D2 shows a single `opaque` boolean
# CANNOT round-trip the four WHATWG non-special shapes -- `foo:bar` (opaque,
# host absent), `foo:/bar` (list, host absent, no authority), `foo:///bar`
# (list, host EMPTY, authority present), `foo://[::1]/bar` (list, IPv6 host).
# `authority_kind` records whether a `//` authority was present (distinguishing
# `foo:/bar` from `foo:///bar`); `host_kind` records empty-vs-absent-vs-present
# WITHIN an authority. Neither derives the other across all four shapes, so both
# are retained. Public `NA` mapping is unchanged: both empty and absent hosts,
# and both empty and absent query/fragment, still surface as `NA` publicly --
# this vocabulary is internal state only.

# --- enum vocabularies (first value = the natural default) ------------------

# WHATWG path discriminator (ADR 0012 D2): every Stage-B path transform and the
# WHATWG serializer key off this. RFC parsing NEVER inherits it.
.PATH_KIND <- c("list", "opaque")

# RFC 3986 section 3.3 path forms (ADR 0012 D2): the RFC posture carries this
# instead of the WHATWG opaque/list discriminator.
.RFC_PATH_FORM <- c("abempty", "absolute", "rootless", "empty")

# Host emptiness/absence within an authority (ADR 0012 D2). Both `empty` and
# `absent` map to a public NA; only internal state distinguishes them.
.HOST_KIND <- c("absent", "empty", "present")

# Whether a `//` authority was present, and (if so) whether it carried a host
# (ADR 0012 D2). Distinguishes `foo:/bar` (absent) from `foo:///bar` (empty).
.AUTHORITY_KIND <- c("absent", "empty", "present")

# Delimiter-presence state for query and fragment (ADR 0012 D2): `query_kind` /
# `fragment_kind`. The standard serializers retain a trailing `?`/`#` when the
# delimiter was present with an empty value, so `empty` MUST be distinguishable
# from `absent` even though both map to a public NA.
.PRESENCE_KIND <- c("absent", "empty", "present")

# WHATWG host FORM (ADR 0012 D2). Full derivation (domain-vs-opaque needs the
# scheme special-ness and the host parse) is L4b's job; the thin mapper below
# resolves only the unambiguous IP/empty cases and defers the rest.
.WHATWG_HOST_FORM <- c("domain", "opaque", "ipv4", "ipv6", "empty")

# RFC 3986 host FORM (ADR 0012 D2): adds `reg-name` and `ipvfuture` (bracketed
# IPvFuture) which the WHATWG enum lacks. As above, L4b populates the
# reg-name-vs-ipvfuture split; the thin mapper defers it.
.RFC_HOST_FORM <- c("reg-name", "ipv4", "ipv6", "ipvfuture", "empty")

# --- pure, vectorized classifiers -------------------------------------------

# All classifiers below are written as pre-allocate + logical-mask assignment
# (the repo's vectorized idiom) rather than nested `ifelse()`: it avoids
# `ifelse`'s NA-propagation footguns, keeps each branch an explicit mask, and
# stays lint-clean (no nested_ifelse). Scalar/vector args are recycled to a
# common length so mixed scalar-and-vector calls (as the tests use) work.

# WHATWG opaque-path trigger (ADR 0012 D2 / Appendix A.1, WHATWG
# #opaque-path-state). A URL's path is `opaque` iff the scheme is NON-special
# AND the remainder after the scheme `:` does NOT start with `/`; otherwise
# `list`. `remainder_after_scheme` is the input after the scheme `:` (e.g. for
# `foo:bar` it is "bar", for `foo:/bar` it is "/bar", for `foo:///bar` it is
# "///bar"). Special schemes are always `list`. A missing (NA) remainder is
# treated as not starting with `/`.
.whatwg_path_kind <- function(is_special, remainder_after_scheme) {
  n <- max(length(is_special), length(remainder_after_scheme))
  is_special <- rep_len(is_special, n)
  rem <- rep_len(remainder_after_scheme, n)
  starts_slash <- !is.na(rem) & startsWith(rem, "/")
  out <- rep("list", n)
  out[!is_special & !starts_slash] <- "opaque"
  out
}

# host_kind (ADR 0012 D2): NA -> absent; "" -> empty; else present.
.host_kind <- function(host) {
  out <- rep("present", length(host))
  out[is.na(host)] <- "absent"
  out[!is.na(host) & host == ""] <- "empty"
  out
}

# authority_kind (ADR 0012 D2, lines 257-258 + 270-271): records WHETHER a `//`
# authority was present, distinguishing `foo:/bar` (absent) from `foo:///bar`
# (present). No `//` -> absent; `//` present -> present. It deliberately does
# NOT key off host emptiness: D2 labels `foo:///bar` as authority PRESENT with
# host EMPTY, so an empty host under a present `//` is (authority_kind
# "present", host_kind "empty"). Keying off the host would also misclassify a
# `//user@/path` authority (empty host but a non-empty authority via userinfo).
#
# The vocab's `empty` value is RESERVED for a genuinely empty authority
# *component* (a present `//` with no userinfo, host, AND port); this pure
# classifier cannot emit it because userinfo/port are not modeled here -- L4b
# populates `empty` once they are. host_kind is the seam the L3b serializer
# keys `//`-vs-no-`//` emission off (host null-vs-empty), so dropping the host
# arg here keeps the signature minimal for L3b/L3c.
.authority_kind <- function(has_double_slash) {
  out <- rep("present", length(has_double_slash))
  out[!has_double_slash] <- "absent"
  out
}

# presence_kind (ADR 0012 D2): NA -> absent; "" -> empty; else present. Used for
# query_kind AND fragment_kind. The raw value MUST be captured BEFORE
# `.blank_to_na` collapses ""->NA for this to distinguish empty from absent --
# but that capture is L3b's wiring job, not this pure classifier's.
.presence_kind <- function(value) {
  out <- rep("present", length(value))
  out[is.na(value)] <- "absent"
  out[!is.na(value) & value == ""] <- "empty"
  out
}

# RFC 3986 section 3.3 path form (ADR 0012 D2). With an authority the hier-part
# grammar admits only `path-abempty` (empty or begins `/`). Without an
# authority: `empty` ("" or NA), `absolute` (begins `/`), else `rootless`
# (first segment non-empty, no leading `/`).
#
# RFC section 3.3 disambiguation note: `path-absolute` is "/" [ segment-nz ... ]
# so it can never begin with "//" -- a leading "//" without an authority is not
# a valid hier-part path (it would be read as an authority). A pure classifier
# given (has_authority = FALSE, path = "//x") is therefore fed an input the
# grammar cannot produce; we classify it `absolute` (it begins with a single
# `/`) rather than invent a fifth form. Detecting/handling that malformed case
# is the authority splitter's job upstream, not this classifier's.
.rfc_path_form <- function(has_authority, path) {
  n <- max(length(has_authority), length(path))
  has_authority <- rep_len(has_authority, n)
  path <- rep_len(path, n)
  is_empty <- is.na(path) | path == ""
  begins_slash <- !is.na(path) & startsWith(path, "/")
  out <- rep("rootless", n)
  out[!has_authority & is_empty] <- "empty"
  out[!has_authority & !is_empty & begins_slash] <- "absolute"
  out[has_authority] <- "abempty"
  out
}

# WHATWG host FORM mapper (ADR 0012 D2, L4b POPULATED by RURL-yutinyhb).
# Unambiguous cases: NA host -> NA (no form for an absent host), IPv6 -> "ipv6",
# IPv4 -> "ipv4", "" -> "empty". The domain-vs-opaque split for a present non-IP
# host needs the scheme special-ness, which the L3a signature did not carry; L4b
# threads it through the OPTIONAL `is_special` argument. When `is_special` is
# supplied (the L4b parser passes it), a present non-IP host resolves to
# "domain" (special scheme) or "opaque" (non-special). When it is left at its
# NA default (the L3a call shape) the present non-IP host stays NA -- so the
# L3a representability tests are byte-for-byte unaffected. A "domain" here does
# NOT imply the domain parser ran; the opaque parser (RURL-yutinyhb) only ever
# passes is_special = FALSE, so it always yields "opaque".
.whatwg_host_form <- function(host, is_ip_host_v6, is_ip_host_v4,
                              is_special = NA) {
  n <- max(length(host), length(is_ip_host_v6), length(is_ip_host_v4))
  host <- rep_len(host, n)
  v6 <- rep_len(is_ip_host_v6, n)
  v4 <- rep_len(is_ip_host_v4, n)
  is_special <- rep_len(is_special, n)
  v6 <- !is.na(v6) & v6
  v4 <- !is.na(v4) & v4
  out <- rep(NA_character_, n) # NA host, and unresolved non-IP host, stay NA
  out[!is.na(host) & host == ""] <- "empty"
  present_non_ip <- !is.na(host) & host != "" & !v4 & !v6
  resolvable <- present_non_ip & !is.na(is_special)
  out[resolvable & is_special] <- "domain"
  out[resolvable & !is_special] <- "opaque"
  out[v4] <- "ipv4"
  out[v6] <- "ipv6"
  out
}

# RFC 3986 host FORM mapper (ADR 0012 D2, L4b POPULATED by RURL-yutinyhb).
# Unambiguous cases as above (NA -> NA, IPv6 -> "ipv6", IPv4 -> "ipv4",
# "" -> "empty"). The reg-name-vs-ipvfuture split needs bracketed-IPvFuture
# detection; L4b enables it with the OPTIONAL `resolve` flag. When `resolve` is
# TRUE (the L4b parser passes it) a present non-IP host resolves to "ipvfuture"
# iff it is a bracketed `[v...]` IP-literal, else "reg-name". When it is left at
# its FALSE default (the L3a call shape) a present non-IP host stays NA -- so
# the L3a representability tests are byte-for-byte unaffected. IPvFuture is
# detected structurally from the host string (bracketed), NOT via punycode/UTS
# #46: the RFC host path never routes through those (ADR 0002).
.rfc_host_form <- function(host, is_ip_host_v6, is_ip_host_v4,
                           resolve = FALSE) {
  n <- max(length(host), length(is_ip_host_v6), length(is_ip_host_v4))
  host <- rep_len(host, n)
  v6 <- rep_len(is_ip_host_v6, n)
  v4 <- rep_len(is_ip_host_v4, n)
  v6 <- !is.na(v6) & v6
  v4 <- !is.na(v4) & v4
  out <- rep(NA_character_, n) # NA host, and unresolved non-IP host, stay NA
  out[!is.na(host) & host == ""] <- "empty"
  present_non_ip <- !is.na(host) & host != "" & !v4 & !v6
  if (isTRUE(resolve)) {
    is_ipvf <- present_non_ip &
      stringi::stri_detect_regex(host, .RFC3986_BRACKET_IPVFUTURE_RE)
    is_ipvf[is.na(is_ipvf)] <- FALSE
    out[present_non_ip & !is_ipvf] <- "reg-name"
    out[is_ipvf] <- "ipvfuture"
  }
  out[v4] <- "ipv4"
  out[v6] <- "ipv6"
  out
}

# --- Stage-B eligibility matrix (ADR 0012 D2 / Layer 3c, RURL-jqlnnaiw) ------
#
# Per-row eligibility masks that gate the Stage-B semantic-transform pipeline
# (._parse_stage_b_vec, R/parse.R). This is a PURE, vectorized classifier: it
# takes already-classified per-row state (scheme string, WHATWG path_kind via
# `.whatwg_path_kind`, host_kind via `.host_kind`, is_ip_host) plus the resolved
# `scheme_acceptance`/`url_standard`, and returns three logical masks. It never
# parses, never touches curl, and never routes through the punycode/domain
# helpers (ADR 0002).
#
# CRITICAL BYTE-IDENTITY DESIGN: the ENTIRE restriction is gated on
# `scheme_acceptance == "general"`. Under any other value (notably "web", the
# only publicly reachable value and the default), ALL masks are TRUE -- the
# previously supported web schemes (http/https/ftp/ftps/file) are GRANDFATHERED
# and keep exactly today's Stage-B behavior (D2: "without changing existing
# direct-selector results for the previously supported schemes"). This mirrors
# L2's reject decoupling (`if (scheme_acceptance == "web")`, parse-phases.R):
# byte-identity for web is BY CONSTRUCTION, not by hoping masks come out TRUE.
#
# Returns a list of three masks, each length-n:
#   - path_eligible: hierarchical/path transforms (dot-segment normalization,
#     index page, trailing slash, and the path case fold). D2 row "Hierarchical
#     path transforms": never applied to a WHATWG opaque path; TRUE for list
#     paths.
#   - host_transform_eligible: host presentation (host_encoding IDNA/unicode),
#     subdomain trimming, and the host case fold. D2 rows "Host presentation
#     (host_encoding)" + "DNS/PSL derivation": a WHATWG *domain* host only. A
#     domain host arises only under a special scheme; opaque hosts (non-special
#     scheme), IP literals, and empty/absent hosts are NOT domain hosts.
#   - semantic_transform_eligible: automatic semantic transforms beyond the
#     standard (query filter/sort here; www/subdomain/scheme-force are Stage A
#     or covered by the host mask). D2 row "Automatic semantic transforms":
#     HTTP(S) only (ftp/file are special but not HTTP(S)). This FALSE case is
#     the `transform-skipped-ineligible-scheme` signal L5 will surface.
.stage_b_eligibility <- function(scheme_acceptance, scheme, url_standard,
                                 path_kind, host_kind, is_ip_host) {
  n <- max(
    length(scheme), length(path_kind), length(host_kind), length(is_ip_host)
  )
  scheme <- rep_len(scheme, n)
  path_kind <- rep_len(path_kind, n)
  host_kind <- rep_len(host_kind, n)
  is_ip_host <- rep_len(is_ip_host, n)

  # Grandfather clause: any non-"general" acceptance (incl. "web", the default
  # and only publicly reachable value) imposes NO restriction. All masks TRUE.
  if (!identical(scheme_acceptance, "general")) {
    all_true <- rep(TRUE, n)
    return(list(
      path_eligible = all_true,
      host_transform_eligible = all_true,
      semantic_transform_eligible = all_true
    ))
  }

  # url_standard is part of the classifier signature (D2 keys eligibility off
  # the selected posture); the D2 rows implemented below are posture-agnostic in
  # `general` today, so it is accepted for forward-compatibility and to keep
  # the signature stable for L4b/L5. `force` documents the deliberate non-use.
  force(url_standard)

  scheme_lc <- .ascii_tolower(scheme)
  is_special <- !is.na(scheme_lc) & scheme_lc %in% .WHATWG_SPECIAL_SCHEMES
  is_http <- !is.na(scheme_lc) & scheme_lc %in% c("http", "https")
  ip <- !is.na(is_ip_host) & is_ip_host

  # D2 "Hierarchical path transforms": eligible for list paths, never for a
  # WHATWG opaque path.
  path_eligible <- path_kind != "opaque"
  # D2 "Automatic semantic transforms": HTTP(S) only.
  semantic_transform_eligible <- is_http
  # D2 "Host presentation" + "DNS/PSL derivation": a WHATWG domain host only --
  # present, non-IP host under a special scheme. Opaque hosts, IP literals, and
  # empty/absent hosts are ineligible.
  host_transform_eligible <- is_special & host_kind == "present" & !ip

  list(
    path_eligible = path_eligible,
    host_transform_eligible = host_transform_eligible,
    semantic_transform_eligible = semantic_transform_eligible
  )
}

# --- RFC 3986 generic-URI grammar gate (ADR 0012 Layer 4a, RURL-sxssynfu) ----
#
# `.rfc3986_generic_uri_ok()` is the INDEPENDENT normative acceptance contract
# for the new RFC-general branch (ADR 0012 D1, lines 199-226). After scheme +
# component-delimiter recognition it validates that the ASCII portion of the
# input matches RFC 3986's generic `URI` grammar (RFC 3986 section 3). It is a
# PURE, vectorized validator: it NEVER calls curl and never delegates to
# libcurl's permissiveness (D1: "an INDEPENDENT gate, not a delegation to
# libcurl"). It is deliberately ADDITIVE -- nothing here is wired into the live
# parse pipeline (L4b does that), so byte-identity of every existing output is
# trivially preserved.
#
# It is NOT wired to the punycode/domain helpers (ADR 0002) and does NOT touch
# them. It is NOT an RFC 3987 (IRI) validator: directly-written non-ASCII scalar
# values are the ONE tolerated syntax extension (ADR 0002 reversibility) --
# carried through and flagged `unicode-outside-rfc3986-uri`, never advertised as
# IRI conformance. That tolerance does NOT relax the surrounding ASCII grammar.
#
# Return value: list(ok = <logical>, diagnostic = <character>), each length-n.
#   - ok        : TRUE = accepted by the generic URI grammar; FALSE = a generic
#                 grammar violation; NA = NA input.
#   - diagnostic: "unicode-outside-rfc3986-uri" iff the row is accepted AND
#                 carries a directly-written non-ASCII scalar value; NA
#                 otherwise (including every rejected row).
#
# DELIBERATE ABNF SIMPLIFICATIONS (documented per the task):
#   * Component validation is per-component character-class + pct-encoding regex
#     plus explicit delimiter checks, not a byte-perfect ABNF derivation. Every
#     RURL-wncwfasl false-reject and every adversarial fixture is covered, and
#     the adversarial rejects are rejected.
#   * IPv4address is subsumed by `reg-name` for ACCEPTANCE (a dotted quad is a
#     valid reg-name: digits + "." are unreserved), so a distinct IPv4 grammar
#     is not needed for the gate's verdict.
#   * IPv6 uses the canonical fully-expanded alternation (RFC 4291 textual
#     forms incl. `::` compression + trailing embedded IPv4); zone identifiers
#     are unsupported (RFC 9844 restored RFC 3986's zone-less `IP-literal`).
#   * Scheme-specific restrictions are NOT generic gates (D1 rule 6): a
#     comma-less `data:` or a `mailto:` with a fragment is an `ok` generic parse
#     here; those are L5 scheme diagnostics, not gate failures.

# character-class fragments (contents for a `[...]`), RFC 3986 section 2.2/2.3.
.RFC3986_UNRESERVED <- "A-Za-z0-9._~\\-"
.RFC3986_SUBDELIMS <- "!$&'()*+,;="
# The ONE tolerated extension (ADR 0002 / D1 rule 5): directly-written non-ASCII
# scalar values are admitted wherever a data character is admitted, then flagged
# separately. Admitting them here does NOT relax the ASCII grammar -- an
# otherwise-invalid ASCII portion still fails even with non-ASCII present.
.RFC3986_NONASCII <- "\\x{0080}-\\x{10FFFF}"
.RFC3986_PCT <- "%[0-9A-Fa-f]{2}"

# Build an anchored "*(data / pct-encoded)" matcher whose data class is the
# unreserved + sub-delims + non-ASCII set plus the component-specific `extra`
# characters (e.g. ":" for userinfo, ":@/" for a path).
.rfc3986_class_re <- function(extra) {
  paste0(
    "^(?:[", .RFC3986_UNRESERVED, .RFC3986_SUBDELIMS, extra,
    .RFC3986_NONASCII, "]|", .RFC3986_PCT, ")*$"
  )
}

# reg-name = *( unreserved / pct-encoded / sub-delims )                (S3.2.2)
.RFC3986_HOST_RE <- .rfc3986_class_re("")
# userinfo = *( unreserved / pct-encoded / sub-delims / ":" )          (S3.2.1)
.RFC3986_USERINFO_RE <- .rfc3986_class_re(":")
# path segments: pchar = unreserved / pct-encoded / sub-delims / ":" / "@",
# joined by "/". A raw "@"/":" is a LEGAL pchar (why `mailto:a@b.com` accepts).
.RFC3986_PATH_RE <- .rfc3986_class_re(":@/")
# query / fragment = *( pchar / "/" / "?" )                        (S3.4/S3.5)
.RFC3986_QF_RE <- .rfc3986_class_re(":@/?")
# port = *DIGIT (empty port is legal); non-ASCII is NOT tolerated here (S3.2.3).
.RFC3986_PORT_RE <- "^[0-9]*$"

# IPv4address, and the full dotted quad, for embedded-IPv4 IPv6 forms (S3.2.2).
.RFC3986_IPV4 <- "(25[0-5]|(2[0-4]|1?[0-9])?[0-9])"
.RFC3986_IPV4_QUAD <- paste0("(", .RFC3986_IPV4, "\\.){3}", .RFC3986_IPV4)

# IPv6address (RFC 4291) -- the canonical fully-expanded alternation. ASCII-only
# (no non-ASCII tolerance inside brackets); zone identifiers unsupported.
.RFC3986_IPV6_RE <- paste0(
  "^(",
  "([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}|",
  "([0-9A-Fa-f]{1,4}:){1,7}:|",
  "([0-9A-Fa-f]{1,4}:){1,6}:[0-9A-Fa-f]{1,4}|",
  "([0-9A-Fa-f]{1,4}:){1,5}(:[0-9A-Fa-f]{1,4}){1,2}|",
  "([0-9A-Fa-f]{1,4}:){1,4}(:[0-9A-Fa-f]{1,4}){1,3}|",
  "([0-9A-Fa-f]{1,4}:){1,3}(:[0-9A-Fa-f]{1,4}){1,4}|",
  "([0-9A-Fa-f]{1,4}:){1,2}(:[0-9A-Fa-f]{1,4}){1,5}|",
  "[0-9A-Fa-f]{1,4}:(:[0-9A-Fa-f]{1,4}){1,6}|",
  ":((:[0-9A-Fa-f]{1,4}){1,7}|:)|",
  "::([Ff]{4}(:0{1,4})?:)?", .RFC3986_IPV4_QUAD, "|",
  "([0-9A-Fa-f]{1,4}:){1,4}:", .RFC3986_IPV4_QUAD,
  ")$"
)

# IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )     (S3.2.2)
.RFC3986_IPVFUTURE_RE <- paste0(
  "^v[0-9A-Fa-f]+\\.[", .RFC3986_UNRESERVED, .RFC3986_SUBDELIMS, ":]+$"
)

# Bracketed IPvFuture IP-literal `"[" IPvFuture "]"` (RURL-yutinyhb). Used by
# the `.rfc_host_form` mapper to distinguish an `ipvfuture` host FORM from
# `reg-name` once L4b resolves a present non-IP RFC host (the bracket is part of
# the stored host value then). Same inner grammar as `.RFC3986_IPVFUTURE_RE`.
.RFC3986_BRACKET_IPVFUTURE_RE <- paste0(
  "^\\[v[0-9A-Fa-f]+\\.[", .RFC3986_UNRESERVED, .RFC3986_SUBDELIMS, ":]+\\]$"
)

# First 1-based index of the literal `ch` in `s`, or 0L when absent.
.rfc3986_first_index <- function(s, ch) {
  pos <- stringi::stri_locate_first_fixed(s, ch)[1L, 1L]
  if (is.na(pos)) 0L else as.integer(pos)
}

# Whole-string match of a pre-built component regex (empty string matches the
# "*(...)" productions -- an empty reg-name/userinfo/path/query/fragment/port is
# legal). Returns a single TRUE/FALSE.
.rfc3986_match <- function(s, re) {
  isTRUE(stringi::stri_detect_regex(s, re))
}

# IP-literal inner form: IPv6address / IPvFuture (S3.2.2). Empty inner (`[]`) or
# any malformation (non-hex, missing groups) -> FALSE.
.rfc3986_valid_ip_literal <- function(inner) {
  isTRUE(stringi::stri_detect_regex(inner, .RFC3986_IPV6_RE)) ||
    isTRUE(stringi::stri_detect_regex(inner, .RFC3986_IPVFUTURE_RE))
}

# host [ ":" port ] (S3.2.2/S3.2.3). A bracketed IP-literal owns any ":" inside
# it; only a trailing ":port" after "]" is a port. A non-bracketed host is a
# reg-name (which forbids ":"), so the first ":" is the port delimiter.
.rfc3986_valid_hostport <- function(hp) {
  if (startsWith(hp, "[")) {
    rb <- .rfc3986_first_index(hp, "]")
    if (rb == 0L) {
      return(FALSE) # bracket opened but never closed
    }
    if (!.rfc3986_valid_ip_literal(substring(hp, 2L, rb - 1L))) {
      return(FALSE)
    }
    after <- substring(hp, rb + 1L)
    if (!nzchar(after)) {
      return(TRUE)
    }
    if (!startsWith(after, ":")) {
      return(FALSE) # junk after the "]" that is not a port
    }
    return(.rfc3986_match(substring(after, 2L), .RFC3986_PORT_RE))
  }
  cpos <- .rfc3986_first_index(hp, ":")
  if (cpos > 0L) {
    if (!.rfc3986_match(substring(hp, cpos + 1L), .RFC3986_PORT_RE)) {
      return(FALSE)
    }
    return(.rfc3986_match(substring(hp, 1L, cpos - 1L), .RFC3986_HOST_RE))
  }
  .rfc3986_match(hp, .RFC3986_HOST_RE)
}

# authority = [ userinfo "@" ] host [ ":" port ] (S3.2). userinfo and reg-name
# both FORBID a raw "@" (it must be %40), so a valid authority carries AT MOST
# ONE "@" -- its single userinfo/host separator. D1's headline reject: a
# repeated raw "@" (`scheme://username@@@@example.com`) is a generic-grammar
# FAILURE even though a permissive splitter can recover a host.
.rfc3986_valid_authority <- function(authority) {
  n_at <- stringi::stri_count_fixed(authority, "@")
  if (n_at > 1L) {
    return(FALSE)
  }
  if (n_at == 1L) {
    apos <- .rfc3986_first_index(authority, "@")
    userinfo <- substring(authority, 1L, apos - 1L)
    if (!.rfc3986_match(userinfo, .RFC3986_USERINFO_RE)) {
      return(FALSE)
    }
    return(.rfc3986_valid_hostport(substring(authority, apos + 1L)))
  }
  .rfc3986_valid_hostport(authority)
}

# hier-part (S3.3). Authority is present IFF the hier-part starts "//"; then the
# rest is path-abempty. Otherwise it is path-absolute / path-rootless /
# path-empty, all validated by the shared path matcher (segment ":" and "@" are
# legal pchar, so a scheme-less rootless path like `example.com/path` is fine).
.rfc3986_valid_hier_part <- function(hp) {
  if (!startsWith(hp, "//")) {
    return(.rfc3986_match(hp, .RFC3986_PATH_RE))
  }
  ap <- substring(hp, 3L)
  spos <- .rfc3986_first_index(ap, "/")
  if (spos > 0L) {
    authority <- substring(ap, 1L, spos - 1L)
    path <- substring(ap, spos)
  } else {
    authority <- ap
    path <- ""
  }
  if (!.rfc3986_valid_authority(authority)) {
    return(FALSE)
  }
  .rfc3986_match(path, .RFC3986_PATH_RE)
}

# Scalar core: TRUE / FALSE / NA for one input (see the wrapper's contract).
.rfc3986_generic_uri_ok_one <- function(x) {
  if (is.na(x)) {
    return(NA)
  }
  # scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) ":" (S3.1). A missing
  # scheme means the string is not a generic URI at all -> reject. Non-ASCII in
  # the scheme position fails this ASCII-only match (scheme is never tolerant).
  m <- stringi::stri_match_first_regex(x, "^([A-Za-z][A-Za-z0-9+.\\-]*):")
  if (is.na(m[1L, 1L])) {
    return(FALSE)
  }
  rest <- substring(x, nchar(m[1L, 2L]) + 2L)
  # fragment = everything after the FIRST "#" (S3.5).
  hpos <- .rfc3986_first_index(rest, "#")
  if (hpos > 0L) {
    if (!.rfc3986_match(substring(rest, hpos + 1L), .RFC3986_QF_RE)) {
      return(FALSE)
    }
    rest <- substring(rest, 1L, hpos - 1L)
  }
  # query = everything after the FIRST "?" in what remains (S3.4).
  qpos <- .rfc3986_first_index(rest, "?")
  if (qpos > 0L) {
    if (!.rfc3986_match(substring(rest, qpos + 1L), .RFC3986_QF_RE)) {
      return(FALSE)
    }
    rest <- substring(rest, 1L, qpos - 1L)
  }
  .rfc3986_valid_hier_part(rest)
}

# Vectorized public-internal gate. `diagnostic` fires ONLY on an accepted row
# that carries a non-ASCII scalar value; a rejected row is never flagged (D1:
# the Unicode tolerance never rescues an otherwise-invalid ASCII portion).
.rfc3986_generic_uri_ok <- function(url) {
  n <- length(url)
  ok <- vapply(url, .rfc3986_generic_uri_ok_one, logical(1L), USE.NAMES = FALSE)
  has_non_ascii <- stringi::stri_detect_regex(url, "\\P{ASCII}")
  has_non_ascii[is.na(has_non_ascii)] <- FALSE
  diagnostic <- rep(NA_character_, n)
  flagged <- !is.na(ok) & ok & has_non_ascii
  diagnostic[flagged] <- "unicode-outside-rfc3986-uri"
  list(ok = ok, diagnostic = diagnostic)
}

# --- Posture host/opaque parsers (ADR 0012 Layer 4b, RURL-yutinyhb) ----------
#
# The HOST/OPAQUE decomposition functions the (still-unexposed) `general`
# acceptance will call. They are PURELY ADDITIVE: nothing here is wired into the
# live Stage-A routing, public signatures, `.derive_parse_status_vec`, or the
# Phase-11 serializer dispatch (that is the SEPARATE activation unit,
# RURL-qbnelzku). Byte-identity of every existing output is therefore trivially
# preserved. They NEVER route a non-special / opaque / reg-name host through the
# punycode or domain.R helpers (ADR 0002 -- hard): the opaque host is UTF-8
# percent-encoded, the RFC host is source-preserving, and ASCII case is kept in
# both (case policy is a separate phase).
#
# `.parse_opaque_urls_vec(url, url_standard)` returns a COLUMNAR list (each
# element length-n): decomposed components plus the internal STATE kinds. The
# activation unit reads the posture-appropriate columns:
#   - ok            : logical; FALSE = a host parse failure (forbidden-host cp,
#                     or a malformed bracketed IP literal) OR an un-decomposable
#                     input (missing scheme / NA). The four legal WHATWG shapes
#                     and every legal RFC host are ok = TRUE.
#   - scheme        : the source scheme spelling (case preserved; posture case
#                     policy is applied later).
#   - host          : NA (absent) / "" (empty) / value. WHATWG opaque hosts are
#                     UTF-8 %-encoded; RFC hosts are source-preserving.
#   - port          : NA (absent) / "" / digits (source-preserving string).
#   - path          : the path body (opaque path is the whole remainder string).
#   - query/fragment: NA (absent) / "" (empty-but-present) / value.
#   - path_kind     : WHATWG list/opaque (`.whatwg_path_kind`); NA under RFC.
#   - rfc_path_form : RFC abempty/absolute/rootless/empty (`.rfc_path_form`); NA
#                     under WHATWG.
#   - host_kind     : `.host_kind` (absent/empty/present).
#   - authority_kind: absent (no `//`) / empty (a `//` whose authority component
#                     is genuinely empty -- no userinfo, host, AND port) /
#                     present (a `//` carrying content). This POPULATES the
#                     `.authority_kind` vocab's reserved `empty` value, which
#                     the pure L3a classifier could not emit (it did not model
#                     userinfo/port). The L3a classifier is unchanged.
#   - host_form     : via the `.whatwg_host_form` / `.rfc_host_form` mappers.

# Split an authority component into userinfo / host / port (ADR 0012 Layer 4b).
# userinfo is everything before the LAST `@` (WHATWG credentials rule; a valid
# RFC authority carries at most one `@`, enforced separately by the L4a gate).
# A bracketed IP-literal owns any `:` inside it -- only a trailing `:port` after
# `]` is the port; a non-bracketed host's first `:` is the port delimiter.
# authority == "" yields host == "" (empty), userinfo/port NA.
.split_authority <- function(authority) {
  userinfo <- NA_character_
  hostport <- authority
  at <- stringi::stri_locate_last_fixed(authority, "@")[1L, 1L]
  if (!is.na(at)) {
    userinfo <- substring(authority, 1L, at - 1L)
    hostport <- substring(authority, at + 1L)
  }
  host <- hostport
  port <- NA_character_
  if (startsWith(hostport, "[")) {
    rb <- stringi::stri_locate_first_fixed(hostport, "]")[1L, 1L]
    if (!is.na(rb)) {
      host <- substring(hostport, 1L, rb)
      after <- substring(hostport, rb + 1L)
      if (startsWith(after, ":")) {
        port <- substring(after, 2L)
      }
    }
  } else {
    cpos <- stringi::stri_locate_first_fixed(hostport, ":")[1L, 1L]
    if (!is.na(cpos)) {
      host <- substring(hostport, 1L, cpos - 1L)
      port <- substring(hostport, cpos + 1L)
    }
  }
  list(userinfo = userinfo, host = host, port = port)
}

# WHATWG opaque-HOST parse of ONE non-special authority host (ADR 0012 D1 / A.1;
# WHATWG #concept-opaque-host-parser). Preserve ASCII case, NO IDNA, NO IPv4
# coercion, NO punycode/domain routing (ADR 0002). A bracketed host is an IPv6
# literal -- the forbidden-host reject does NOT apply to it, but its inner form
# must be a valid IPv6 address (WHATWG has no IPvFuture). A non-bracketed host
# rejects the forbidden-HOST code points (`.WHATWG_FORBIDDEN_HOST_ONLY_CP`, NOT
# the stricter forbidden-DOMAIN set), then is UTF-8 percent-encoded with the
# C0-control set. `%` is NOT forbidden (a malformed `%` is an L5
# validation-error fact, not a failure here). Returns list(ok, host, v6, v4).
.whatwg_opaque_host_one <- function(host) {
  if (startsWith(host, "[")) {
    if (!endsWith(host, "]")) {
      return(list(ok = FALSE, host = host, is_v6 = FALSE, is_v4 = FALSE))
    }
    inner <- substring(host, 2L, nchar(host) - 1L)
    if (!isTRUE(stringi::stri_detect_regex(inner, .RFC3986_IPV6_RE))) {
      return(list(ok = FALSE, host = host, is_v6 = FALSE, is_v4 = FALSE))
    }
    return(list(ok = TRUE, host = host, is_v6 = TRUE, is_v4 = FALSE))
  }
  forbidden <- stringi::stri_detect_regex(host, .WHATWG_FORBIDDEN_HOST_ONLY_CP)
  if (isTRUE(forbidden)) {
    return(list(ok = FALSE, host = host, is_v6 = FALSE, is_v4 = FALSE))
  }
  encoded <- .whatwg_component_percent_encode(host, integer(0))
  list(ok = TRUE, host = encoded, is_v6 = FALSE, is_v4 = FALSE)
}

# RFC-3986 host parse of ONE authority host (ADR 0012 D1 / A.2). Source
# preserving (the `rfc-syntax` posture disclaims normalization): preserve case,
# NO punycode, NO percent-encoding. A bracketed host is an IP-literal -- inner
# IPv6 or IPvFuture; a malformed inner is a host parse failure. A non-bracketed
# host is `reg-name` or `IPv4address` (both accepted verbatim; the FORM split is
# left to the mapper). Returns list(ok, host, is_v6, is_v4).
.rfc_host_one <- function(host) {
  if (startsWith(host, "[")) {
    if (!endsWith(host, "]")) {
      return(list(ok = FALSE, host = host, is_v6 = FALSE, is_v4 = FALSE))
    }
    inner <- substring(host, 2L, nchar(host) - 1L)
    if (isTRUE(stringi::stri_detect_regex(inner, .RFC3986_IPV6_RE))) {
      return(list(ok = TRUE, host = host, is_v6 = TRUE, is_v4 = FALSE))
    }
    if (isTRUE(stringi::stri_detect_regex(inner, .RFC3986_IPVFUTURE_RE))) {
      return(list(ok = TRUE, host = host, is_v6 = FALSE, is_v4 = FALSE))
    }
    return(list(ok = FALSE, host = host, is_v6 = FALSE, is_v4 = FALSE))
  }
  is_v4 <- isTRUE(stringi::stri_detect_regex(
    host, paste0("^", .RFC3986_IPV4_QUAD, "$")
  ))
  list(ok = TRUE, host = host, is_v6 = FALSE, is_v4 = is_v4)
}

# Decompose ONE scheme-bearing non-special / general input (ADR 0012 D1/D2,
# Appendix A.1). See `.parse_opaque_urls_vec` for the column contract.
.parse_opaque_url_one <- function(url, url_standard) {
  na <- NA_character_
  blank <- list(
    ok = FALSE, scheme = na, host = na, port = na, path = na, query = na,
    fragment = na, path_kind = na, rfc_path_form = na, host_kind = "absent",
    authority_kind = "absent", query_kind = "absent",
    fragment_kind = "absent", host_form = na
  )
  if (is.na(url)) {
    return(blank)
  }
  m <- stringi::stri_match_first_regex(
    url, "^([A-Za-z][A-Za-z0-9+.\\-]*):(.*)$"
  )
  if (is.na(m[1L, 1L])) {
    return(blank) # not a scheme-bearing input -> cannot decompose
  }
  scheme <- m[1L, 2L]
  remainder <- m[1L, 3L]
  is_special <- .ascii_tolower(scheme) %in% .WHATWG_SPECIAL_SCHEMES
  is_whatwg <- identical(url_standard, "whatwg")

  # Query and fragment split on the FIRST `#`, then the FIRST `?` in what
  # remains -- captured empty-but-present so `.presence_kind` can mark "empty".
  query <- na
  fragment <- na
  rest <- remainder
  hpos <- .rfc3986_first_index(rest, "#")
  if (hpos > 0L) {
    fragment <- substring(rest, hpos + 1L)
    rest <- substring(rest, 1L, hpos - 1L)
  }
  qpos <- .rfc3986_first_index(rest, "?")
  if (qpos > 0L) {
    query <- substring(rest, qpos + 1L)
    rest <- substring(rest, 1L, qpos - 1L)
  }

  path_kind <- na
  rfc_path_form <- na
  host <- na
  port <- na
  is_v6 <- FALSE
  is_v4 <- FALSE
  ok <- TRUE

  # WHATWG opaque-path trigger: a non-special scheme whose remainder does not
  # begin `/` (checked on the pre-split remainder, per WHATWG). Opaque paths
  # carry no authority and host is absent. RFC has no opaque/list distinction.
  if (is_whatwg) {
    path_kind <- .whatwg_path_kind(is_special, remainder)
  }

  if (is_whatwg && identical(path_kind, "opaque")) {
    path <- rest
    authority_kind <- "absent"
  } else if (startsWith(rest, "//")) {
    after <- substring(rest, 3L)
    spos <- .rfc3986_first_index(after, "/")
    if (spos > 0L) {
      authority <- substring(after, 1L, spos - 1L)
      path <- substring(after, spos)
    } else {
      authority <- after
      path <- ""
    }
    # Consistent with the L3a `.authority_kind()` classifier and ADR 0012 D2
    # (lines 257-258, 270-271): any `//` is authority_kind "present" regardless
    # of host emptiness. `foo:///bar` is (present, host_kind "empty"). The vocab
    # `empty` value stays vestigial -- an empty host under a present `//` is
    # host_kind's job, NOT authority_kind's, so the parser never emits it.
    authority_kind <- .authority_kind(TRUE)
    parts <- .split_authority(authority)
    host <- parts$host
    port <- parts$port
    if (nzchar(host)) {
      hp <- if (is_whatwg) {
        .whatwg_opaque_host_one(host)
      } else {
        .rfc_host_one(host)
      }
      ok <- hp$ok
      host <- hp$host
      is_v6 <- hp$is_v6
      is_v4 <- hp$is_v4
    }
    if (!is_whatwg) {
      rfc_path_form <- .rfc_path_form(TRUE, path)
    }
  } else {
    authority_kind <- "absent"
    path <- rest
    if (!is_whatwg) {
      rfc_path_form <- .rfc_path_form(FALSE, path)
    }
  }

  host_form <- if (is_whatwg) {
    .whatwg_host_form(host, is_v6, is_v4, is_special = is_special)
  } else {
    .rfc_host_form(host, is_v6, is_v4, resolve = TRUE)
  }

  list(
    ok = ok, scheme = scheme, host = host, port = port, path = path,
    query = query, fragment = fragment, path_kind = path_kind,
    rfc_path_form = rfc_path_form, host_kind = .host_kind(host),
    authority_kind = authority_kind, query_kind = .presence_kind(query),
    fragment_kind = .presence_kind(fragment), host_form = host_form
  )
}

# Vectorized posture opaque/host parser (ADR 0012 Layer 4b). See the column
# contract above. Pure: never touches curl, never routes through punycode /
# domain.R (ADR 0002).
.parse_opaque_urls_vec <- function(url, url_standard) {
  n <- length(url)
  chr_fields <- c(
    "scheme", "host", "port", "path", "query", "fragment", "path_kind",
    "rfc_path_form", "host_kind", "authority_kind", "query_kind",
    "fragment_kind", "host_form"
  )
  if (n == 0L) {
    out <- list(ok = logical(0))
    for (f in chr_fields) {
      out[[f]] <- character(0)
    }
    return(out)
  }
  rows <- lapply(url, .parse_opaque_url_one, url_standard = url_standard)
  out <- list(ok = vapply(rows, `[[`, logical(1L), "ok", USE.NAMES = FALSE))
  for (f in chr_fields) {
    out[[f]] <- vapply(rows, `[[`, character(1L), f, USE.NAMES = FALSE)
  }
  out
}

# RFC 8089 `file:` overlay (ADR 0012 D1 / A.2, RURL-yutinyhb; two-gate model
# RURL-obsweger). A THIN overlay used wherever `file:` is routed out of libcurl
# under the RFC model (`url_standard = "rfc3986"` and the NULL selector) -- the
# WHATWG `file:` path (`.parse_whatwg_file_urls_vec`) is a SEPARATE state
# machine and is left verbatim.
#
# RFC 8089 is a SPECIALIZATION of RFC 3986, not a parallel model: its normative
# Section 2 grammar is built from RFC 3986's own productions and is a strict
# SUBSET of them. Its Appendix E/F "nonstandard variations" are the opposite --
# they partly escape the generic envelope (App. F's `drive-letter = ALPHA ":" /
# ALPHA "|"` is not valid RFC 3986: `|` is absent from `pchar`). So acceptance
# is decided by TWO gates, in order:
#
#   Gate 1 -- is the string a valid RFC 3986 URI at all? No -> parse failure.
#     Rejects the backslash separator (App. E.4 states it is "forbidden by both
#     [RFC1738] and [RFC3986]" and offers only a repair heuristic, never a
#     production), the `ALPHA "|"` drive letter, and a literal `[example]` host.
#     Verified against independent implementations: Ruby's URI::RFC3986_Parser
#     rejects all three; Node/WHATWG repairs them (which is the `whatwg`
#     profile's job here, not ours).
#
#   Gate 2 -- does RFC 8089 Section 2 narrow it further? Enforce that narrowing,
#     UNLESS an Appendix F production restores the form AND the result is still
#     RFC 3986-valid.
#       port      -- REJECTED. Section 2's `file-auth = "localhost" / host`
#                    has no port and NO appendix supplies one.
#       userinfo  -- PARSED and surfaced as a fact. App. E.1/F supply
#                    `file-auth = "localhost" / [ userinfo "@" ] host`, and
#                    `userinfo` is an RFC 3986 production, so it stays inside
#                    the envelope. Reported via `file-userinfo-extension`.
#       query /   -- PARSED. RFC 8089 never mentions either (zero occurrences
#       fragment     in the document), so they are inherited generic RFC 3986
#                    components. For the fragment this is not merely silence:
#                    RFC 3986 Section 3.5 states that fragment semantics "are
#                    independent of the URI scheme and thus cannot be redefined
#                    by scheme specifications" -- RFC 8089 could not have
#                    restricted it. Real usage depends on this (RFC 8118
#                    Section 3 defines `page=`/`nameddest=` fragments for
#                    application/pdf, resolved by MEDIA TYPE exactly as 3986
#                    Section 3.5 describes). Reported via
#                    `file-component-outside-rfc8089`.
#
# Like WHATWG, `file://localhost/...` maps to an empty host (RFC 8089 App. B
# treats `localhost` and the empty authority as the local machine). No
# drive-letter or backslash rewriting (those are WHATWG-specific).
.parse_rfc_file_url_one <- function(url) {
  na <- NA_character_
  blank <- list(
    ok = FALSE, scheme = na, host = na, port = na, path = na, query = na,
    fragment = na, userinfo = na, rfc_path_form = na, host_kind = "absent",
    authority_kind = "absent", query_kind = "absent",
    fragment_kind = "absent", host_form = na
  )
  m <- stringi::stri_match_first_regex(url, "^([Ff][Ii][Ll][Ee]):(.*)$")
  if (is.na(m[1L, 1L])) {
    return(blank)
  }
  scheme <- m[1L, 2L]
  rest <- m[1L, 3L]

  query <- na
  fragment <- na
  hpos <- .rfc3986_first_index(rest, "#")
  if (hpos > 0L) {
    fragment <- substring(rest, hpos + 1L)
    rest <- substring(rest, 1L, hpos - 1L)
  }
  qpos <- .rfc3986_first_index(rest, "?")
  if (qpos > 0L) {
    query <- substring(rest, qpos + 1L)
    rest <- substring(rest, 1L, qpos - 1L)
  }

  host <- na
  port <- na
  userinfo <- na
  is_v6 <- FALSE
  is_v4 <- FALSE
  if (startsWith(rest, "//")) {
    after <- substring(rest, 3L)
    spos <- .rfc3986_first_index(after, "/")
    if (spos > 0L) {
      authority <- substring(after, 1L, spos - 1L)
      path <- substring(after, spos)
    } else {
      authority <- after
      path <- ""
    }
    authority_kind <- if (nzchar(authority)) "present" else "empty"
    parts <- .split_authority(authority)
    host <- parts$host
    port <- parts$port
    userinfo <- parts$userinfo
    # Gate 2: RFC 8089 Section 2 admits no port, and no appendix supplies a
    # production for one. A port is therefore a parse FAILURE, not a fact.
    # (Contrast userinfo, which App. E.1/F does supply a production for.)
    if (!is.na(port)) {
      return(blank)
    }
    # localhost (case-insensitive) collapses to an empty host, matching WHATWG.
    if (!is.na(host) &&
        identical(.ascii_tolower(host), "localhost")) {
      host <- ""
    } else if (nzchar(host)) {
      hp <- .rfc_host_one(host)
      is_v6 <- hp$is_v6
      is_v4 <- hp$is_v4
      host <- hp$host
    }
    rfc_path_form <- .rfc_path_form(TRUE, path)
  } else {
    authority_kind <- "absent"
    path <- rest
    rfc_path_form <- .rfc_path_form(FALSE, path)
  }

  list(
    ok = TRUE, scheme = scheme, host = host, port = port, path = path,
    query = query, fragment = fragment, userinfo = userinfo,
    rfc_path_form = rfc_path_form,
    host_kind = .host_kind(host), authority_kind = authority_kind,
    query_kind = .presence_kind(query),
    fragment_kind = .presence_kind(fragment),
    host_form = .rfc_host_form(host, is_v6, is_v4, resolve = TRUE)
  )
}

# Vectorized RFC 8089 `file:` overlay (ADR 0012 Layer 4b).
#
# Gate 1 lives HERE rather than in the caller so the overlay is self-contained:
# it carries its own RFC 3986 validity contract wherever it is routed (general
# acceptance, `rfc3986`, or the NULL selector), instead of depending on a caller
# to have applied the generic gate first. Under `rfc3986` the caller's gate runs
# too; that is idempotent, not a conflict.
.parse_rfc_file_urls_vec <- function(url) {
  n <- length(url)
  chr_fields <- c(
    "scheme", "host", "port", "path", "query", "fragment", "userinfo",
    "rfc_path_form",
    "host_kind", "authority_kind", "query_kind", "fragment_kind", "host_form"
  )
  if (n == 0L) {
    out <- list(ok = logical(0))
    for (f in chr_fields) {
      out[[f]] <- character(0)
    }
    return(out)
  }
  rows <- lapply(url, .parse_rfc_file_url_one)
  out <- list(ok = vapply(rows, `[[`, logical(1L), "ok", USE.NAMES = FALSE))
  for (f in chr_fields) {
    out[[f]] <- vapply(rows, `[[`, character(1L), f, USE.NAMES = FALSE)
  }
  # Gate 1: a `file:` string that is not a valid RFC 3986 URI is a parse
  # failure, whatever RFC 8089's appendices tolerate in the wild.
  gate <- .rfc3986_generic_uri_ok(url)$ok
  gate[is.na(gate)] <- FALSE
  out$ok <- out$ok & gate
  out
}

# --- General-acceptance routing + parse (ADR 0012 Layer 4b-2, RURL-qbnelzku) --
#
# The ACTIVATION seam that wires the L3a/L3b/L3c/L4a/L4b-1 building blocks into
# the live pipeline. `.general_parsed_mask` decides WHICH rows the `general`
# posture routes OUT of libcurl to the posture opaque/RFC/file parser;
# `.general_parse_vec` performs that parse and reports the components, the parse
# `ok` verdict (including D1's RFC generic-grammar gate), and the internal state
# kinds. BOTH Stage A and Stage B call `.general_parse_vec` on the same URL
# string, so the routing mask and state kinds are guaranteed consistent between
# the two stages WITHOUT threading every state kind through the Stage-A cache
# (the cache stores only `.spu_stage_a_fields`; these functions are pure and
# re-run cheaply in Stage B for the routed rows only).
#
# CRITICAL BYTE-IDENTITY DESIGN: both are a pure no-op unless
# `scheme_acceptance == "general"`. Under any other value (notably "web", the
# default and, until this unit, the only publicly reachable value) the mask is
# all-FALSE and the parse returns empty/NA columns, so the `general_route`
# masks in Stage A/B are EMPTY, libcurl-path vectors are bit-identical, and no
# new behavior runs. Byte-identity for web is BY CONSTRUCTION.
#
# ROUTING RULE (posture-keyed). A row is general-routed iff it is scheme-bearing
# (`^scheme:`), is NOT a host:port form (`example.com:8080`, which Phase 1
# parses as host:port), and its scheme is NOT one libcurl + the existing web
# machinery
# already handle for the posture:
#   - whatwg  : keep the six WHATWG special schemes (http/https/ftp/ws/wss/file)
#               on libcurl; route every other (non-special) scheme to the opaque
#               parser. ws/wss stay on libcurl and parse as special (L1 default
#               ports 80/443); ftps is non-special under WHATWG and routes here.
#   - rfc3986 : keep http/https/ftp/ftps on libcurl (existing rfc3986 host model
#               + path-rootless slice); route file to the RFC 8089 overlay and
#               every other scheme to the RFC generic host parser.
.general_parsed_mask <- function(url, url_standard, scheme_acceptance) {
  n <- length(url)
  if (!identical(scheme_acceptance, "general") || n == 0L) {
    return(rep(FALSE, n))
  }
  m <- stringi::stri_match_first_regex(url, "^([A-Za-z][A-Za-z0-9+.\\-]*):")
  scheme_lc <- .ascii_tolower(m[, 2L])
  has_scheme <- !is.na(scheme_lc)
  host_port <- stringi::stri_detect_regex(url, "^[^/]+:[0-9]+($|/)")
  host_port[is.na(host_port)] <- FALSE
  curl_scheme <- if (.is_whatwg(url_standard)) {
    .WHATWG_SPECIAL_SCHEMES
  } else {
    c("http", "https", "ftp", "ftps")
  }
  gp <- has_scheme & !host_port & !(scheme_lc %in% curl_scheme)
  gp[is.na(gp)] <- FALSE
  gp
}

# Vectorized general-acceptance parse. Returns a columnar list, length-n:
#   general_parsed : the routing mask (`.general_parsed_mask`).
#   ok             : TRUE = successfully parsed AND (under rfc3986) accepted by
#                    D1's generic-URI grammar gate; FALSE otherwise. Only
#                    meaningful where `general_parsed` is TRUE.
#   scheme/host/port/path/query/fragment : decomposed components (raw; the
#                    public NA mapping and `.blank_to_na` are applied by the
#                    caller). host is UTF-8 %-encoded (WHATWG opaque host) or
#                    source-preserving (RFC), never routed through punycode /
#                    domain.R (ADR 0002).
#   path_kind/rfc_path_form/host_kind/authority_kind/query_kind/fragment_kind/
#   host_form : the internal state kinds the L3b serializers and the
#                    parse-status promotion consume.
.general_parse_vec <- function(url, url_standard, scheme_acceptance) {
  n <- length(url)
  na <- rep(NA_character_, n)
  out <- list(
    general_parsed = rep(FALSE, n), ok = rep(FALSE, n),
    scheme = na, host = na, port = na, path = na, query = na, fragment = na,
    userinfo = na,
    path_kind = na, rfc_path_form = na, host_kind = rep("absent", n),
    authority_kind = rep("absent", n), query_kind = rep("absent", n),
    fragment_kind = rep("absent", n), host_form = na
  )
  gp <- .general_parsed_mask(url, url_standard, scheme_acceptance)
  out$general_parsed <- gp
  if (!any(gp)) {
    return(out)
  }
  is_whatwg <- .is_whatwg(url_standard)
  scheme_lc <- .ascii_tolower(
    stringi::stri_match_first_regex(url, "^([A-Za-z][A-Za-z0-9+.\\-]*):")[, 2L]
  )

  # D1 RFC generic-grammar gate: a general-routed row under rfc3986 must pass or
  # it is a parse error (the tolerated non-ASCII extension does NOT cause
  # failure; its diagnostic surfacing is L5, out of scope here).
  gate_ok <- rep(TRUE, n)
  if (identical(url_standard, "rfc3986")) {
    g <- .rfc3986_generic_uri_ok(url[gp])
    gk <- g$ok
    gk[is.na(gk)] <- FALSE
    gate_ok[gp] <- gk
  }

  # file under rfc3986 -> RFC 8089 overlay; everything else -> posture parser.
  # (whatwg `file` is a special scheme and never reaches here -- it stays on the
  # existing WHATWG file state machine.)
  is_file <- gp & !is_whatwg & !is.na(scheme_lc) & scheme_lc == "file"
  reg <- gp & !is_file

  opaque_fields <- c(
    "scheme", "host", "port", "path", "query", "fragment", "path_kind",
    "rfc_path_form", "host_kind", "authority_kind", "query_kind",
    "fragment_kind", "host_form"
  )
  if (any(reg)) {
    p <- .parse_opaque_urls_vec(url[reg], url_standard)
    for (f in opaque_fields) {
      out[[f]][reg] <- p[[f]]
    }
    out$ok[reg] <- p$ok
  }
  if (any(is_file)) {
    p <- .parse_rfc_file_urls_vec(url[is_file])
    # `userinfo` is file-only: RFC 8089 App. E.1/F supplies a production for it,
    # so the overlay surfaces it as a fact. The opaque parser has no such column
    # and its rows keep the NA initialized above, so opaque output is unchanged.
    for (f in c(setdiff(opaque_fields, "path_kind"), "userinfo")) {
      out[[f]][is_file] <- p[[f]]
    }
    out$ok[is_file] <- p$ok
  }

  out$ok <- out$ok & gate_ok
  out
}
