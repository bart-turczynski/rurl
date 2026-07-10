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

# WHATWG host FORM thin mapper (ADR 0012 D2). Resolves only the unambiguous
# cases: NA host -> NA (no form for an absent host), IPv6 -> "ipv6", IPv4 ->
# "ipv4", "" -> "empty". The domain-vs-opaque split needs the scheme
# special-ness and the WHATWG/opaque host parse, so L4b POPULATES that; this
# returns NA_character_ for a present non-IP host and L4b refines it.
.whatwg_host_form <- function(host, is_ip_host_v6, is_ip_host_v4) {
  n <- max(length(host), length(is_ip_host_v6), length(is_ip_host_v4))
  host <- rep_len(host, n)
  v6 <- rep_len(is_ip_host_v6, n)
  v4 <- rep_len(is_ip_host_v4, n)
  v6 <- !is.na(v6) & v6
  v4 <- !is.na(v4) & v4
  out <- rep(NA_character_, n) # NA host, and present non-IP host (L4b), stay NA
  out[!is.na(host) & host == ""] <- "empty"
  out[v4] <- "ipv4"
  out[v6] <- "ipv6"
  out
}

# RFC 3986 host FORM thin mapper (ADR 0012 D2). As with the WHATWG mapper this
# resolves only the unambiguous cases (NA -> NA, IPv6 -> "ipv6", IPv4 ->
# "ipv4", "" -> "empty"); the reg-name-vs-ipvfuture split (bracketed IPvFuture
# detection) is L4b's job, so a present non-IP host returns NA_character_ here
# and L4b POPULATES the final form.
.rfc_host_form <- function(host, is_ip_host_v6, is_ip_host_v4) {
  n <- max(length(host), length(is_ip_host_v6), length(is_ip_host_v4))
  host <- rep_len(host, n)
  v6 <- rep_len(is_ip_host_v6, n)
  v4 <- rep_len(is_ip_host_v4, n)
  v6 <- !is.na(v6) & v6
  v4 <- !is.na(v4) & v4
  out <- rep(NA_character_, n) # NA host, and present non-IP host (L4b), stay NA
  out[!is.na(host) & host == ""] <- "empty"
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

  scheme_lc <- stringi::stri_trans_tolower(scheme)
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
