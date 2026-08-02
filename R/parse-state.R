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
# a `//` flag, a raw component value) and never parse, never touch the web
# parser, and
# never route through the punycode/domain helpers (ADR 0002).
#
# Why a richer state model at all: ADR 0012 D2 shows a single `opaque` boolean
# CANNOT round-trip the four WHATWG non-special shapes -- `foo:bar` (opaque,
# host absent), `foo:/bar` (list, host absent, no authority), `foo:///bar`
# (list, host EMPTY, authority present), `foo://[::1]/bar` (list, IPv6 host).
# `authority_delimiter_present` records whether a `//` authority delimiter was
# present (distinguishing `foo:/bar` from `foo:///bar`) and
# `authority_payload_kind` whether that delimiter carried anything;
# `host_kind` records empty-vs-absent-vs-present WITHIN an authority. Neither
# derives the other across all four shapes, so both are retained. Public `NA`
# mapping is unchanged: both empty and absent hosts, and both empty and absent
# query/fragment, still surface as `NA` publicly -- this vocabulary is internal
# state only.

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

# Payload state of a PRESENT `//` authority delimiter (P1.2 D-A): `empty` when
# the authority substring between `//` and the path start is zero-length,
# `present` when it carries anything at all (userinfo-only, port-only, or an
# ordinary host). Not applicable -- and projected NA -- when no delimiter was
# present. It is a payload state UNDER a delimiter, never a third sibling of
# absent/present.
.AUTHORITY_PAYLOAD_KIND <- c("empty", "present")

# LEGACY authority vocabulary (retired from the canonical schema by P1.2 D-D).
# Retained only as the value set of the derived, read-only `.authority_kind()`
# compatibility projection below -- never as canonical state, never as a
# serializer input.
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

# authority_payload_kind (P1.2 D-A.2). `authority` is the substring between the
# `//` delimiter and the path start (`/`, `?`, `#`, or end), or NA when no
# delimiter was present. NA -> NA (not applicable: the payload of a delimiter
# that does not exist is never forced to a substantive value); "" -> empty;
# anything else -> present.
#
# Payload-`present` does NOT assert that a host exists: `foo://@/bar`
# (userinfo-only) and `foo://:80/bar` (port-only) are both payload-present with
# an empty host. Host presence is `host_kind`'s job and is decided
# independently (P1.2 D-B).
.authority_payload_kind <- function(authority) {
  out <- rep("present", length(authority))
  out[is.na(authority)] <- NA_character_
  out[!is.na(authority) & authority == ""] <- "empty"
  out
}

# LEGACY authority_kind -- a DERIVED, READ-ONLY compatibility projection over
# the two canonical fields (P1.2 D-D). It is never canonical state and never a
# serializer input; the serializers consume `authority_delimiter_present`
# directly (P1.2 D-C), because inferring `//` from a derived component cannot
# distinguish a delimiter-present empty authority from a delimiter-absent input.
#
# The projection makes the legacy `empty` value REACHABLE for the first time --
# as a payload state under a present delimiter, which is precisely the
# unreachable-value contradiction S1-F5 recorded:
#   delimiter absent                   -> "absent"
#   delimiter present, payload empty   -> "empty"
#   delimiter present, payload present -> "present"
.authority_kind <- function(delimiter_present, payload_kind) {
  n <- max(length(delimiter_present), length(payload_kind))
  delimiter_present <- rep_len(delimiter_present, n)
  payload_kind <- rep_len(payload_kind, n)
  out <- rep("absent", n)
  present <- !is.na(delimiter_present) & delimiter_present
  out[present & !is.na(payload_kind) & payload_kind == "empty"] <- "empty"
  out[present & !is.na(payload_kind) & payload_kind == "present"] <- "present"
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
# parses, never touches the web parser, and never routes through the
# punycode/domain
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
# PURE, vectorized validator: it NEVER runs the web parser and never delegates
# to its permissiveness (D1: "an INDEPENDENT gate, not a delegation to the
# parser"). It is deliberately ADDITIVE -- nothing here is wired into the live
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

# Every anchor in this transcription is `\A`/`\z` (true start/end of input), NOT
# `^`/`$`. ICU's `$` also matches BEFORE a trailing line terminator, so an
# `^...$` grammar silently admits a component ending in LF, VT, FF, CR, NEL,
# LS or PS -- none of which any RFC 3986 production allows, since none is in
# `unreserved` / `sub-delims` / `pchar`. That leak made a general-scheme
# userinfo accept `u\n` and emit the raw control verbatim (RURL-dergzwku); RFC
# 3986 has no removal step, so the grammar REJECTS these rather than stripping
# them the way the WHATWG parser does. The same `$`-before-terminator trap is
# recorded at `R/parse-phases.R:251`.
.RFC3986_ANCHOR_START <- "\\A"
.RFC3986_ANCHOR_END <- "\\z"

# Build an anchored "*(data / pct-encoded)" matcher whose data class is the
# unreserved + sub-delims + non-ASCII set plus the component-specific `extra`
# characters (e.g. ":" for userinfo, ":@/" for a path).
.rfc3986_class_re <- function(extra) {
  paste0(
    .RFC3986_ANCHOR_START, "(?:[", .RFC3986_UNRESERVED, .RFC3986_SUBDELIMS,
    extra, .RFC3986_NONASCII, "]|", .RFC3986_PCT, ")*", .RFC3986_ANCHOR_END
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
.RFC3986_PORT_RE <- "\\A[0-9]*\\z"

# IPv4address, and the full dotted quad, for embedded-IPv4 IPv6 forms (S3.2.2).
.RFC3986_IPV4 <- "(25[0-5]|(2[0-4]|1?[0-9])?[0-9])"
.RFC3986_IPV4_QUAD <- paste0("(", .RFC3986_IPV4, "\\.){3}", .RFC3986_IPV4)

# IPv6address (RFC 4291) -- the canonical fully-expanded alternation. ASCII-only
# (no non-ASCII tolerance inside brackets); zone identifiers unsupported.
.RFC3986_IPV6_RE <- paste0(
  "\\A(",
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
  ")\\z"
)

# IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )     (S3.2.2)
.RFC3986_IPVFUTURE_RE <- paste0(
  "\\Av[0-9A-Fa-f]+\\.[", .RFC3986_UNRESERVED, .RFC3986_SUBDELIMS, ":]+\\z"
)

# Bracketed IPvFuture IP-literal `"[" IPvFuture "]"` (RURL-yutinyhb). Used by
# the `.rfc_host_form` mapper to distinguish an `ipvfuture` host FORM from
# `reg-name` once L4b resolves a present non-IP RFC host (the bracket is part of
# the stored host value then). Same inner grammar as `.RFC3986_IPVFUTURE_RE`.
.RFC3986_BRACKET_IPVFUTURE_RE <- paste0(
  "\\A\\[v[0-9A-Fa-f]+\\.[", .RFC3986_UNRESERVED, .RFC3986_SUBDELIMS,
  ":]+\\]\\z"
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

# Pin the byte->code-point reading this grammar walks, so the VERDICT does not
# depend on the session locale (RURL-kmpnbvdl).
#
# The walk mixes two indexing schemes: `stri_locate_first_fixed()` /
# `stri_detect_regex()` count CODE POINTS (stringi decodes first), while
# `substring()` / `nchar()` count NATIVE characters. For a string marked
# "unknown" -- which is what `rawToChar()` and most user input produce -- native
# means the session encoding, so `<C3><A9>` is ONE character under a UTF-8
# locale and TWO under `LC_ALL=C`. The two schemes then disagree and the
# grammar slices the authority at the wrong offset: `ftp://é:é@example.com/p`
# was admitted (`ok-ftp`) under a UTF-8 session and REJECTED (`error`) under
# `LC_ALL=C`, on byte-identical input.
#
# `\x{0080}-\x{10FFFF}` in the data class admits directly-written non-ASCII
# SCALAR VALUES, and a scalar value only exists once an encoding is fixed. UTF-8
# is that encoding everywhere else in the package (`.mark_host_utf8()`,
# `.web_high_bytes_ok()`), so declaring it here makes the gate agree with the
# parser it gates instead of with whoever set LC_CTYPE.
#
# `Encoding<-`, never `enc2utf8()`: the bytes must not move, only their reading
# be fixed -- `enc2utf8()` TRANSCODES from the session locale, which is the very
# sensitivity being removed. Only "unknown" (native) elements are touched: an
# explicit "UTF-8"/"latin1"/"bytes" declaration is already locale-invariant and
# is the caller stating what the bytes mean, so it stands. Invalid octets stay
# invalid -- a declared-UTF-8 string holding `<80>` makes `stri_detect_regex()`
# return NA, `isTRUE()` folds that to FALSE, and the row is rejected in EVERY
# locale, which is the correct answer: a lone continuation byte is not a scalar
# value and no RFC 3986 production admits it.
.rfc3986_declare_native_utf8 <- function(url) {
  native <- !is.na(url) & Encoding(url) == "unknown"
  if (any(native)) {
    Encoding(url[native]) <- "UTF-8"
  }
  url
}

# Vectorized public-internal gate. `diagnostic` fires ONLY on an accepted row
# that carries a non-ASCII scalar value; a rejected row is never flagged (D1:
# the Unicode tolerance never rescues an otherwise-invalid ASCII portion).
.rfc3986_generic_uri_ok <- function(url) {
  n <- length(url)
  url <- .rfc3986_declare_native_utf8(url)
  # Bytes that do not decode are rejected WITHOUT walking the grammar, and that
  # is a shortcut for a proof, not a convenience: the walk covers the whole
  # string (scheme / authority / path / query / fragment leave no byte
  # unclassified), every production is ASCII apart from the
  # `\x{0080}-\x{10FFFF}` scalar-value tolerance, and an octet sequence that is
  # not valid UTF-8 denotes no scalar value. So an undecodable element must
  # fail whichever component it lands in -- FALSE is the answer the walk would
  # have reached had it been able to run.
  #
  # It cannot run: the walk slices with `substring()`/`nchar()`, which THROW
  # `invalid multibyte string` on a declared-UTF-8 string holding invalid
  # octets. That throw aborted the entire vectorized call -- `get_host()` over
  # 1000 URLs lost all 1000 because one carried a stray `<80>` (RURL-kmpnbvdl).
  # `safe_parse_urls()` is named for the promise this broke.
  #
  # Scoped to elements DECLARED UTF-8 (including the ones just declared above).
  # A "latin1" element decodes by definition -- every octet is a latin1
  # character -- so it walks normally and is never caught here.
  undecodable <- !is.na(url) & Encoding(url) == "UTF-8" & !validUTF8(url)
  walkable <- !undecodable
  ok <- rep(NA, n)
  ok[undecodable] <- FALSE
  if (any(walkable)) {
    ok[walkable] <- vapply(
      url[walkable], .rfc3986_generic_uri_ok_one, logical(1L),
      USE.NAMES = FALSE
    )
  }
  has_non_ascii <- stringi::stri_detect_regex(url, "\\P{ASCII}")
  has_non_ascii[is.na(has_non_ascii)] <- FALSE
  diagnostic <- rep(NA_character_, n)
  flagged <- !is.na(ok) & ok & has_non_ascii
  diagnostic[flagged] <- "unicode-outside-rfc3986-uri"
  list(ok = ok, diagnostic = diagnostic)
}

# UNIFORM profile gate (RURL-qrfrvmkg / RURL-pfewxbhb Option (a)).
#
# `.rfc3986_generic_uri_ok()` above is the grammar; this is the one place that
# says WHEN it binds. Until this unit it bound only where rurl happened to own
# the parser -- the RFC 8089 `file:` overlay (Gate 1) and the general-routed
# opaque/RFC rows -- so `file://C|/x` was an error while `http://a|b/` parsed,
# with '|' admitted by no RFC 3986 production either way. That made the profile
# a property of the ROUTE rather than of the selected standard. Under
# `url_standard = "rfc3986"` the gate now binds on EVERY row, whichever route
# it takes (web, path-rootless, `file:`, general): selecting a standard
# selects its grammar, uniformly.
#
# Byte-identity elsewhere is by construction: any other selector (including the
# NULL no-selector default) returns an all-TRUE mask, so `parse_ok` is
# unchanged bit for bit.
#
# NA input yields NA from the grammar; it is folded to FALSE here because such
# rows are already non-parseable, and a mask must be a plain logical.
.rfc3986_uniform_gate_ok <- function(url, url_standard) {
  if (!identical(url_standard, "rfc3986")) {
    return(rep(TRUE, length(url)))
  }
  ok <- .rfc3986_generic_uri_ok(url)$ok
  ok[is.na(ok)] <- FALSE
  ok
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
#   - authority_delimiter_present : logical; was a `//` authority delimiter
#                     present after the scheme `:` (P1.2 D-A.1). This is the
#                     fact the L3b serializers consume to decide `//` emission.
#   - authority_payload_kind : empty (the authority substring under a present
#                     `//` is zero-length) / present (it carries anything at
#                     all) / NA when no delimiter was present (P1.2 D-A.2).
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
# must be a valid IPv6 address (WHATWG has no IPvFuture) and, once parsed, is
# re-serialized by the scheme-independent WHATWG IPv6 serializer (zero-run
# compression, lowercase hex, no dotted-quad tail) exactly as the special-scheme
# branch does in Phase 5b (RURL-cyxegfjs). A non-bracketed host
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
    return(list(
      ok = TRUE, host = .serialize_whatwg_ipv6_host(host),
      is_v6 = TRUE, is_v4 = FALSE
    ))
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
    host, paste0("\\A", .RFC3986_IPV4_QUAD, "\\z")
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
    authority_delimiter_present = FALSE, authority_payload_kind = na,
    query_kind = "absent",
    fragment_kind = "absent", host_form = na,
    userinfo = na, userinfo_kind = na
  )
  if (is.na(url)) {
    return(blank)
  }
  # `[\s\S]`, not `.`: ICU counts U+000B (VT) and U+000C (FF) as line
  # terminators, so `.` does not match them and any input carrying one failed to
  # decompose at all (`sc://a<VT>b/` was a parse error rather than a host with a
  # percent-encoded control). Tab/LF/CR are stripped upstream, but VT/FF are not
  # -- WHATWG keeps them and the C0 encoder handles them (RURL-qxpgcwie).
  m <- stringi::stri_match_first_regex(
    url, "^([A-Za-z][A-Za-z0-9+.\\-]*):([\\s\\S]*)$"
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
  # userinfo stays NA unless an authority actually supplies one. An opaque path
  # and a scheme with no `//` have no authority to carry credentials at all.
  userinfo <- na
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
    # WHATWG stores an opaque path ALREADY percent-encoded (opaque path state
    # encodes each code point with the C0-control set as it is consumed), so
    # this is parse-time identity, not `path_encoding` presentation: the
    # `pathname` getter returns the encoded spelling. `delimiter_follows` is the
    # `?`/`#` that ended the path, which decides the trailing-space rule.
    path <- .whatwg_opaque_path_encode(rest, hpos > 0L || qpos > 0L)
    authority <- na
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
    # P1.2 D-A: the delimiter fact is recorded from the source string
    # (`authority` is non-NA exactly when `//` was seen) and the payload kind is
    # classified from the authority substring, NOT from host emptiness.
    # `foo:///bar` is delimiter-present + payload-empty, and so is the RFC
    # `file:` overlay's `file:///bar` -- the two routes can no longer disagree
    # about the same shape (S1-F5).
    parts <- .split_authority(authority)
    host <- parts$host
    port <- parts$port
    # `.split_authority()` has always computed this -- the WHATWG host-missing
    # rule below reads it -- but the opaque parser used to drop it on the floor,
    # so every general-routed row reported NA credentials while the web
    # route reported them exactly (RURL-ovpguvva). Surfaced RAW here; the
    # username/password split and the WHATWG userinfo encode set are applied
    # downstream in R/parse.R, which is where the file:-overlay exception lives.
    userinfo <- parts$userinfo
    if (is_whatwg) {
      # WHATWG authority validation (ADR 0012 D2; #host-parser / #port-state).
      # A non-null port (content after `:`) must be ASCII digits only and
      # <= 65535; an empty port (`:` then end/`/`/`?`/`#`) is null -> legal.
      # A non-digit (`-`, `+`, letters) or an out-of-range integer is failure.
      # `\A`/`\z`, not `^`/`$`, for the reason recorded at the RFC 3986 grammar
      # transcription above: ICU's `$` matches before a trailing line
      # terminator, so `^[0-9]+$` admitted `80\v`, which `as.numeric()` then
      # coerced to 80 -- a silent STRIP of a code point the WHATWG port state
      # requires to be a failure (RURL-dergzwku). Tab/LF/CR never reach here;
      # step 1 removes them.
      has_port <- !is.na(port) && nzchar(port)
      if (has_port &&
        (!isTRUE(stringi::stri_detect_regex(port, "\\A[0-9]+\\z")) ||
          suppressWarnings(as.numeric(port)) > 65535)) {
        ok <- FALSE
      }
      # host-missing (RURL-jxvibxqq). A bare empty host is legal for a
      # non-special scheme (`foo:///bar`), but only when the authority holds
      # NOTHING ELSE. Two spec rules make an empty host a failure, and both key
      # off a DELIMITER being present rather than off the port having content:
      #   * authority state -- "if atSignSeen is true and buffer is the empty
      #     string, host-missing validation error, return failure". So `sc://@/`
      #     and `sc://te@s:t@/` fail: `@` was seen and the host after the LAST
      #     `@` is empty.
      #   * host state -- "if c is U+003A (:) and insideBrackets is false: if
      #     buffer is the empty string, host-missing validation error, return
      #     failure". So `sc://:/` fails on the `:` alone, BEFORE the port is
      #     read -- which is why this tests `!is.na(port)` (a `:` was present)
      #     and not `has_port` (the `:` was followed by digits). That
      #     distinction is the bug: an empty port after an empty host passed.
      # RFC 3986 is untouched: its `reg-name` and `port` are both
      # `*`-quantified, so these are well-formed under the generic syntax.
      if (!nzchar(host) && (!is.na(parts$userinfo) || !is.na(port))) {
        ok <- FALSE
      }
    }
    if (ok && nzchar(host)) {
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
    authority <- na
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
    authority_delimiter_present = !is.na(authority),
    authority_payload_kind = .authority_payload_kind(authority),
    query_kind = .presence_kind(query),
    fragment_kind = .presence_kind(fragment), host_form = host_form,
    userinfo = userinfo,
    # WHATWG authority userinfo: splittable at the first ":" into
    # username/password. Distinguished from the RFC 8089 overlay's UNDIVIDED
    # `[ userinfo "@" ]` so the consumer never has to re-derive which parser a
    # row came from (RURL-ovpguvva).
    userinfo_kind = if (is.na(userinfo)) na else "authority"
  )
}

# Vectorized posture opaque/host parser (ADR 0012 Layer 4b). See the column
# contract above. Pure: never runs the web parser, never routes through
# punycode /
# domain.R (ADR 0002).
.parse_opaque_urls_vec <- function(url, url_standard) {
  n <- length(url)
  chr_fields <- c(
    "scheme", "host", "port", "path", "query", "fragment", "path_kind",
    "rfc_path_form", "host_kind", "authority_payload_kind", "query_kind",
    "fragment_kind", "host_form", "userinfo", "userinfo_kind"
  )
  # `authority_delimiter_present` is the one LOGICAL state column (P1.2 D-A.1),
  # so it collects alongside `ok` rather than through the character loop.
  lgl_fields <- c("ok", "authority_delimiter_present")
  if (n == 0L) {
    out <- list()
    for (f in lgl_fields) {
      out[[f]] <- logical(0)
    }
    for (f in chr_fields) {
      out[[f]] <- character(0)
    }
    return(out)
  }
  rows <- lapply(url, .parse_opaque_url_one, url_standard = url_standard)
  out <- list()
  for (f in lgl_fields) {
    out[[f]] <- vapply(rows, `[[`, logical(1L), f, USE.NAMES = FALSE)
  }
  for (f in chr_fields) {
    out[[f]] <- vapply(rows, `[[`, character(1L), f, USE.NAMES = FALSE)
  }
  out
}

# RFC 8089 `file:` overlay (ADR 0012 D1 / A.2, RURL-yutinyhb; two-gate model
# RURL-obsweger). A THIN overlay used wherever `file:` is routed off the web
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
# No drive-letter or backslash rewriting (those are WHATWG-specific).
#
# What a `localhost` authority DECOMPOSES to, keyed on `url_standard` -- the
# same one-place shape as the `.web_*_policy()` mappers in R/parse-web.R, so
# the selector cannot mean two different grammars on two routes (ADR 0007).
#
#   "empty"     `localhost` collapses to an empty host. WHATWG's file host state
#               says "if host is localhost, set host to the empty string", and
#               the no-selector baseline reproduces it (byte-frozen, ADR 0012
#               D4).
#   "reg-name"  `localhost` is reported as itself, on the ordinary reg-name
#               seam. RFC 3986 S3.2.2 gives `reg-name` no special names, and
#               RFC 8089 S2's `file-auth = "localhost" / host` lists empty,
#               `localhost` and `host` as three legal authority FORMS -- it
#               never rewrites one into another. App. B makes
#               `file://localhost/p` EQUIVALENT to `file:///p`; equivalence is
#               not identical decomposition, and ADR 0007 puts `file` expansion
#               out of the selector's scope.
#
# ADR 0012 D5 scopes the collapse to WHATWG explicitly, by contrast with the
# `rfc-syntax` clause in the same bullet ("`file` under `rfc-syntax`:
# non-absolute path; userinfo, port, query, or fragment present ... Under
# WHATWG, `file://localhost/...` maps `localhost` to the empty host"). Layer
# 4b's "preserve WHATWG's `file://localhost/` -> empty-host mapping" is
# attributive -- it names the mapping's owner, and is not a mandate to run it
# under `rfc-syntax`. Emptying it there scored as the last `host`-field
# divergence in `tools/rfc3986-conformance-sweep.R` (RURL-zyytztdd).
.rfc_file_localhost_policy <- function(url_standard) {
  if (identical(url_standard, "rfc3986")) "reg-name" else "empty"
}

# What a port in a `file:` authority MEANS -- the fourth of ADR 0012 D5's four
# "scheme-specific facts (parseable != valid-for-the-scheme)" items for `file`
# under rfc-syntax. Keyed on the selector, exactly like
# `.rfc_file_localhost_policy()` above and for the same reason.
#
#   "reject"  a port is a parse FAILURE. RFC 8089 sec 2's
#             `file-auth = "localhost" / host` has no port production and no
#             appendix supplies one. This is the NULL default, byte-frozen by
#             ADR 0012 D4.
#   "fact"    the port parses and is surfaced as a diagnostic. `rfc3986` is the
#             scheme-AGNOSTIC generic syntax, where `port = *DIGIT` and the
#             authority is well-formed, so an RFC 8089 narrowing may not gate
#             the parse: ADR 0012 (owner-ruled) "Scheme-specific restrictions
#             are overlays, not generic parse gates", and ADR 0012:63 has
#             scheme-specific RFC violations surface as companion facts
#             (ADR 0006).
#
# D5 lists userinfo, port, query and fragment TOGETHER. Three of the four were
# already facts -- `file://u@example.com/p` parses and reports
# `file-userinfo-extension`, `file:///p?q=1` reports
# `file-component-outside-rfc8089` -- and port was the lone exception, gating
# the parse instead (RURL-uhkofhjf). Same defect class as RURL-zyytztdd, where
# WHATWG's localhost emptying had leaked into the grammar selector.
.rfc_file_port_policy <- function(url_standard) {
  if (identical(url_standard, "rfc3986")) "fact" else "reject"
}

.parse_rfc_file_url_one <- function(url, localhost_policy = "empty",
                                    port_policy = "reject") {
  na <- NA_character_
  blank <- list(
    ok = FALSE, scheme = na, host = na, port = na, path = na, query = na,
    fragment = na, userinfo = na, userinfo_kind = na, rfc_path_form = na,
    host_kind = "absent",
    authority_delimiter_present = FALSE, authority_payload_kind = na,
    query_kind = "absent",
    fragment_kind = "absent", host_form = na
  )
  # `[\s\S]`, not `.` -- the same ICU VT/FF line-terminator trap the opaque
  # decomposer above documents.
  m <- stringi::stri_match_first_regex(
    url, "^([Ff][Ii][Ll][Ee]):([\\s\\S]*)$"
  )
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
    parts <- .split_authority(authority)
    host <- parts$host
    port <- parts$port
    userinfo <- parts$userinfo
    # Gate 2: RFC 8089 Section 2 admits no port, and no appendix supplies a
    # production for one. Under `port_policy = "reject"` -- the NULL default,
    # byte-frozen by ADR 0012 D4 -- a port is therefore a parse FAILURE.
    # (Contrast userinfo, which App. E.1/F does supply a production for.)
    #
    # Under `"fact"` the RFC 8089 narrowing stops gating the parse and the port
    # is surfaced as a diagnostic instead; see `.rfc_file_port_policy()`. The
    # GENERIC production still applies, though -- RFC 3986 sec 3.2.3 is
    # `port = *DIGIT`, so a non-digit port is a grammar failure on this route
    # just as it is on every other, and only the SCHEME-specific narrowing is
    # lifted. `\A`/`\z`, not `^`/`$`: ICU's `$` matches before a trailing line
    # terminator, so `^[0-9]*$` would admit `1\v` (RURL-dergzwku).
    if (!is.na(port)) {
      if (identical(port_policy, "reject")) {
        return(blank)
      }
      if (!isTRUE(stringi::stri_detect_regex(port, "\\A[0-9]*\\z"))) {
        return(blank)
      }
    }
    # localhost (case-insensitive) collapses to an empty host where the selected
    # standard asks for it; under `rfc3986` it falls through to the reg-name arm
    # below and is reported as itself (see `.rfc_file_localhost_policy()`).
    if (identical(localhost_policy, "empty") && !is.na(host) &&
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
    authority <- na
    path <- rest
    rfc_path_form <- .rfc_path_form(FALSE, path)
  }

  list(
    ok = TRUE, scheme = scheme, host = host, port = port, path = path,
    query = query, fragment = fragment, userinfo = userinfo,
    # RFC 8089 App. E.1/F give `[ userinfo "@" ]` UNDIVIDED, and warn that a
    # password there is "a serious security exposure". rurl does not
    # manufacture a credentials split the RFC never draws (RURL-ovpguvva).
    userinfo_kind = if (is.na(userinfo)) na else "rfc8089",
    rfc_path_form = rfc_path_form,
    host_kind = .host_kind(host),
    # Same classifiers as the opaque parser, on the same substring: this is
    # WHERE the S1-F5 route disagreement is removed. The overlay used to emit
    # its own two-value authority_kind ("present"/"empty" keyed off payload
    # content), so `file:///bar` reported authority-empty while `foo:///bar`
    # reported authority-present for the identical shape.
    authority_delimiter_present = !is.na(authority),
    authority_payload_kind = .authority_payload_kind(authority),
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
.parse_rfc_file_urls_vec <- function(url, url_standard = NULL) {
  localhost_policy <- .rfc_file_localhost_policy(url_standard)
  port_policy <- .rfc_file_port_policy(url_standard)
  n <- length(url)
  chr_fields <- c(
    "scheme", "host", "port", "path", "query", "fragment", "userinfo",
    "userinfo_kind", "rfc_path_form",
    "host_kind", "authority_payload_kind", "query_kind", "fragment_kind",
    "host_form"
  )
  lgl_fields <- c("ok", "authority_delimiter_present")
  if (n == 0L) {
    out <- list()
    for (f in lgl_fields) {
      out[[f]] <- logical(0)
    }
    for (f in chr_fields) {
      out[[f]] <- character(0)
    }
    return(out)
  }
  rows <- lapply(
    url, .parse_rfc_file_url_one, localhost_policy = localhost_policy,
    port_policy = port_policy
  )
  out <- list()
  for (f in lgl_fields) {
    out[[f]] <- vapply(rows, `[[`, logical(1L), f, USE.NAMES = FALSE)
  }
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
# posture routes OFF the web route to the posture opaque/RFC/file parser;
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
# masks in Stage A/B are EMPTY, web-route vectors are bit-identical, and no
# new behavior runs. Byte-identity for web is BY CONSTRUCTION.
#
# ROUTING RULE (posture-keyed). A row is general-routed iff it is scheme-bearing
# (`^scheme:`), is NOT a host:port form (`example.com:8080`, which Phase 1
# parses as host:port), and its scheme is NOT one the web route and the
# existing machinery already handle for the posture:
#   - whatwg  : keep the six WHATWG special schemes (http/https/ftp/ws/wss/file)
#               on the web route; route every other (non-special) scheme to
#               the opaque parser. ws/wss stay on the web route and parse as
#               special (L1 default
#               ports 80/443); ftps is non-special under WHATWG and routes here.
#   - rfc3986 : keep http/https/ftp/ftps on the web route (existing host model
#               + path-rootless slice); route file to the RFC 8089 overlay and
#               every other scheme to the RFC generic host parser.
.general_parsed_mask <- function(url, url_standard, scheme_acceptance) {
  n <- length(url)
  if (n == 0L) {
    return(rep(FALSE, n))
  }
  m <- stringi::stri_match_first_regex(url, "^([A-Za-z][A-Za-z0-9+.\\-]*):")
  scheme_lc <- .ascii_tolower(m[, 2L])
  has_scheme <- !is.na(scheme_lc)
  # The host:port carve-out exists for the SCHEME-LESS `example.com:8080` form,
  # which the scheme regex above also matches (a dot is a legal scheme char, so
  # `example.com` reads as a scheme). The authority-part must therefore be
  # colon-free: `[^/]+` was greedy across colons, so `urn:ietf:rfc:2648` matched
  # as "authority `urn:ietf:rfc`, port 2648", was withheld from the opaque
  # parser, and fell through to the web path that rejects `urn:`. Any opaque
  # payload ending in `:<digits>` was unparseable -- `urn:a:1`, `sc:x:80` -- and
  # only a trailing `?`/`#` saved it, by breaking the `($|/)` anchor
  # (RURL-jnvtttfm).
  host_port <- stringi::stri_detect_regex(url, "^[^/:]+:[0-9]+($|/)")
  host_port[is.na(host_port)] <- FALSE
  # ...and the carve-out does not apply under `rfc3986` AT ALL (RURL-kkuirsnz).
  # It encodes rurl's browser-omnibox affordance -- "a bare `example.com:8080`
  # means host `example.com`, port 8080" -- which is a FIX-UP, not a production
  # in the generic syntax. Under the scheme-AGNOSTIC grammar selector the answer
  # is settled by the grammar and there is nothing to infer: `scheme = ALPHA
  # *( ALPHA / DIGIT / "+" / "-" / "." )` admits dots, so `example.com` IS a
  # scheme, and RFC 3986 sec 3.3 makes `8080/x` a `path-rootless`. The
  # Appendix B referee agrees, for `example.com:8080/x` and
  # `www.php.net:80/index.php?test=1` both.
  #
  # This does NOT touch the Stage-A `looks_like_host_port` flag, which is a
  # different question with a different consumer: the key/join surface reads it
  # to classify `h.com:80/` as "missing scheme `:80`"
  # (`contracts/key-join-contracts.md`, P3.1 ratification Q8), and it overrides
  # the same lexical ambiguity there deliberately. Only the rfc3986 ROUTING
  # stops consulting it.
  if (identical(url_standard, "rfc3986")) {
    host_port <- rep(FALSE, n)
  }

  # RFC-model `file:` leaves the web route on EVERY acceptance posture
  # (RURL-obsweger, Tier 1 of the determinism epic). The external engine's
  # `file:` handling was a BUILD
  # property, not a version property: Windows builds enable drive-letter and
  # `file://host` handling that Unix builds reject, so identical input yields
  # `ok` on Windows and `error` on Linux/macOS. That is a DEREFERENCING concern
  # ("can I open this on this machine") leaking into a PARSE, and a parser's
  # output must not depend on the OS it runs on. Routing these rows to the
  # in-tree RFC 8089 overlay -- which already shipped for `general` -- makes the
  # answer platform-invariant by construction. The `whatwg` profile already had
  # its own in-tree `file:` state machine and is untouched here.
  rfc_file <- has_scheme & !host_port & !.is_whatwg(url_standard) &
    !is.na(scheme_lc) & scheme_lc == "file"
  rfc_file[is.na(rfc_file)] <- FALSE

  web_route_scheme <- if (.is_whatwg(url_standard)) {
    .WHATWG_SPECIAL_SCHEMES
  } else {
    c("http", "https", "ftp", "ftps")
  }
  # Keyed on `url_standard`, NOT on the acceptance posture -- like `rfc_file`
  # above, and for the same reason. Which GRAMMAR a string is read under is
  # `url_standard`'s question; which SCHEMES are admitted is
  # `scheme_acceptance`'s. Gating this on the posture made one selector mean two
  # different grammars: `url_standard = "rfc3986"` reported host `evil.com` for
  # `https:///evil.com` under the default posture and the correct empty
  # authority
  # under `general`. ADR 0007 requires one axis per question, so it is answered
  # here once, for every posture.
  odd_slash <- .rfc_odd_slash_run(url, url_standard, scheme_lc, has_scheme,
                                  host_port, web_route_scheme)
  empty_auth <- .rfc_empty_authority_host(url, url_standard, scheme_lc,
                                          has_scheme, host_port,
                                          web_route_scheme)

  if (!identical(scheme_acceptance, "general")) {
    return(rfc_file | odd_slash | empty_auth)
  }
  gp <- has_scheme & !host_port & !(scheme_lc %in% web_route_scheme)
  gp[is.na(gp)] <- FALSE
  gp | rfc_file | odd_slash | empty_auth
}

# RURL-ajikcwkh. The companion to `.rfc_odd_slash_run()` below for the one shape
# it cannot cover: a slash run of EXACTLY 2 whose authority holds no host.
#
#   http://                        authority present + EMPTY, path empty
#   http://?                       ditto, query empty
#   http://#                       ditto, fragment empty
#   http://user@/www.example.com   authority `user@`  -> host empty
#   http://a:b@/www.example.com    authority `a:b@`   -> host empty
#
# RFC 3986 sec 3.2: `authority = [ userinfo "@" ] host [ ":" port ]` and
# `host = IP-literal / IPv4address / reg-name`, where `reg-name = *( ... )` --
# `*`-quantified, so the EMPTY host is a well-formed authority. Appendix B reads
# every row above as authority-present with an empty host, and rurl already
# reports exactly that for the non-special twins (`foo://`,
# `foo://user@/www.example.com`) because those route to the general parser.
#
# These did not, so they hit the web route, which rejects an empty authority
# outright -- `.parse_web_url_one()` returns NULL. That is correct for the model
# the web route implements (a special-scheme authority always has a host) and
# wrong for RFC 3986's generic syntax, so it is the same ROUTING defect as the
# slash-run family and takes the same fix: send the row to the parser that is
# already right.
#
# The slash run is checked as EXACTLY 2 and the host-empty test is lexical,
# because a 2-slash run WITH a host is the ordinary web shape and must keep
# going to the web route untouched. `\A`/`\z`, not `^`/`$`, for the ICU
# trailing-line-terminator reason recorded throughout this file.
#
# This deliberately does NOT answer RURL-mugcdtrv, which asks whether the `web`
# POSTURE should admit such a row. That question is about ADR 0004's host-shape
# gate applied to rows that already parse; this is about rows that never reached
# a parser at all. After this change these 7 behave exactly like their
# already-shipped odd-slash siblings -- record `ok`, `general` posture `ok`,
# `web` posture still rejected -- so the posture question is left open, with
# these rows added to the set it governs rather than decided.
.rfc_empty_authority_host <- function(url, url_standard, scheme_lc, has_scheme,
                                      host_port, web_route_scheme) {
  if (!identical(url_standard, "rfc3986")) {
    return(rep(FALSE, length(url)))
  }
  # Authority = what sits between the `//` and the first `/`, `?` or `#`. It has
  # no host when it is empty, or holds only a userinfo (`...@`) and/or an empty
  # `:port`.
  hostless <- stringi::stri_detect_regex(
    url,
    "\\A[A-Za-z][A-Za-z0-9+.\\-]*://([^/?#@]*@)?(:[0-9]*)?([/?#][\\s\\S]*)?\\z"
  )
  hostless[is.na(hostless)] <- FALSE
  out <- has_scheme & !host_port & scheme_lc %in% web_route_scheme & hostless
  out[is.na(out)] <- FALSE
  out
}

# RURL-xfbzkico. Under `rfc3986`, a web-route scheme whose post-scheme slash run
# is not EXACTLY 2 has no web-route shape at all, and the web route -- which
# models the special-scheme authority, not RFC 3986's generic syntax -- gets
# none of these right:
#
#   run  RFC 3986 sec 3 hier-part          web route reports
#   1    path-absolute, NO authority       host = first segment AND path = /seg
#   3    "//" empty-authority path-abempty host = first path segment (promotion)
#   >=4  "//" empty-authority path-abempty rejected outright
#
# The 1-slash case is the sharpest: the record claims
# `authority_delimiter_present = FALSE` while reporting a host, and leaves the
# same text in the path -- an internally incoherent record, and a decomposition
# no standard and no other engine produces.
#
# The fix is a ROUTING change rather than a second authority parser, because the
# general (RFC generic) parser already produces the exactly correct answer for
# every one of these shapes -- measured on the non-special twins, which differ
# only in scheme:
#
#   foo:/a           -> authority absent, path /a,          form `absolute`
#   foo:///a/b       -> authority EMPTY,  path /a/b,         form `abempty`
#   foo:////evil.com -> authority EMPTY,  path //evil.com,   form `abempty`
#
# So this routes to code that is already right and already covered, instead of
# teaching the web route a grammar it never modelled. It also fixes
# `rfc_path_form` for free (RURL-clgbpwla): the general route already reports
# `absolute` where the web route said `abempty`.
#
# A 0-slash run is INCLUDED as of RURL-kkuirsnz. `eb8ba1a` deliberately left it
# out -- the shape was nominally owned by the `prep$rfc3986_path_rootless` slice
# (`.rfc3986_path_rootless_vec()`, R/parse-phases.R) and widening it was a
# separate acceptance question. It is answered the same way as the rest of the
# family, and for the same reason: with no `//` there is no authority, RFC 3986
# sec 3.3's `hier-part` is `path-rootless`, and `@` and `:` are both `pchar`, so
# the whole remainder is a PATH.
#
# That slice stays, but it only ever claimed a NARROW subset -- a
# `.SPECIAL_AUTHORITY_SCHEMES` scheme whose first path segment is a DOTTED name
# -- so `http:example.com` worked while `http:@www.example.com`, `http:a:b@h`
# and `http::b@h` were rejected. The general parser is already right about all
# of them, and it is installed AFTER the slice in `.parse_urls_vec()`, so
# widening the routing lets ONE parser answer the whole family instead of a
# regex deciding which rootless paths are allowed to exist. Verified: on every
# shape the slice claims, the general parser returns the identical
# scheme/path/query/fragment.
#
# `whatwg` and the no-selector default are untouched -- the mask is gated on
# `url_standard == "rfc3986"` and returns all-FALSE for anything else, so both
# stay byte-identical by construction.
#
# It fires on EVERY acceptance posture, deliberately. Restricting it to
# `scheme_acceptance == "general"` (the first attempt) preserved `web`-posture
# bytes but split one selector across two grammars: `serialize_url(standard =
# "rfc3986")` gave the RFC answer while `safe_parse_urls(url_standard =
# "rfc3986")` still promoted the path segment into the host, and which you got
# depended on an unrelated axis. That is the "behaviour changes with the
# settings" failure mode, and it is worse than a characterization diff.
.rfc_odd_slash_run <- function(url, url_standard, scheme_lc, has_scheme,
                               host_port, web_route_scheme) {
  if (!identical(url_standard, "rfc3986")) {
    return(rep(FALSE, length(url)))
  }
  run <- stringi::stri_match_first_regex(
    url, "^[A-Za-z][A-Za-z0-9+.\\-]*:(/*)"
  )[, 2L]
  n_slash <- stringi::stri_length(run)
  out <- has_scheme & !host_port & scheme_lc %in% web_route_scheme &
    !is.na(n_slash) & n_slash != 2L
  out[is.na(out)] <- FALSE
  out
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
#   path_kind/rfc_path_form/host_kind/authority_delimiter_present/
#   authority_payload_kind/query_kind/fragment_kind/host_form : the internal
#                    state kinds the L3b serializers and the parse-status
#                    promotion consume.
.general_parse_vec <- function(url, url_standard, scheme_acceptance) {
  n <- length(url)
  na <- rep(NA_character_, n)
  out <- list(
    general_parsed = rep(FALSE, n), ok = rep(FALSE, n),
    scheme = na, host = na, port = na, path = na, query = na, fragment = na,
    userinfo = na, userinfo_kind = na,
    path_kind = na, rfc_path_form = na, host_kind = rep("absent", n),
    authority_delimiter_present = rep(FALSE, n), authority_payload_kind = na,
    query_kind = rep("absent", n),
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
  # A row whose BYTES are not valid UTF-8 is withheld from the opaque parser.
  # `.parse_opaque_urls_vec()` reaches `substring()`, which THROWS "invalid
  # multibyte string" on such input, so the row escaped as an error CONDITION
  # rather than the `error` verdict it is owed -- measured as 8 THROW rows in
  # the octet sweep's conjunction block (`http://<80>@/p` and friends). They
  # reached the general route for the first time only when RURL-ajikcwkh's
  # hostless 2-slash routing started sending them here; previously they went to
  # the web route, which rejects them without ever decoding them.
  #
  # The test is `validUTF8()` and NOT `gate_ok`, though the generic gate does
  # reject every one of these too. Short-circuiting on the gate was the first
  # attempt and is WRONG: `out$ok & gate_ok` (below) masks the VERDICT, but the
  # email/mailto diagnostics read this parser's component fields WITHOUT
  # consulting `ok`, so skipping the parse blanked them -- 6 failures in
  # test-email-diagnostics.R, and only under `LC_ALL=C`, because whether the
  # gate rejects those rows is itself locale-dependent. `validUTF8()` is a byte
  # test: it answers the same way in every locale, and it is exactly the
  # condition that makes the parser throw rather than a proxy for it.
  #
  # Withholding costs nothing that was available: RFC 3986's grammar is ASCII,
  # so a string that is not even valid UTF-8 has no decomposition to report.
  decodable <- validUTF8(url)
  decodable[is.na(decodable)] <- FALSE
  reg <- gp & !is_file & decodable

  opaque_fields <- c(
    "scheme", "host", "port", "path", "query", "fragment", "path_kind",
    "rfc_path_form", "host_kind", "authority_delimiter_present",
    "authority_payload_kind", "query_kind",
    "fragment_kind", "host_form", "userinfo", "userinfo_kind"
  )
  if (any(reg)) {
    p <- .parse_opaque_urls_vec(url[reg], url_standard)
    for (f in opaque_fields) {
      out[[f]][reg] <- p[[f]]
    }
    out$ok[reg] <- p$ok
  }
  if (any(is_file)) {
    p <- .parse_rfc_file_urls_vec(url[is_file], url_standard)
    # Both parsers now supply `userinfo`, but they mean different things by it,
    # and the difference is honoured downstream in R/parse.R rather than here:
    # the opaque parser's is a WHATWG authority userinfo (split at the first
    # ":" into username/password), while RFC 8089's App. E.1/F production is
    # `[ userinfo "@" ]` UNDIVIDED -- the appendix warns a password there is
    # "a serious security exposure", so rurl does not manufacture a split the
    # RFC never draws.
    for (f in setdiff(opaque_fields, "path_kind")) {
      out[[f]][is_file] <- p[[f]]
    }
    out$ok[is_file] <- p$ok
  }

  out$ok <- out$ok & gate_ok
  out
}
