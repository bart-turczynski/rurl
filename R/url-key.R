# Output surface (e) -- the v3 comparison key: the engine, plus the two exported
# wrappers at the foot of the file.
#
# `registers/verification-deferrals.md` VD-001 probes this surface with
# `export:get_url_key;...;export:url_anti_join`, and deferral-gate D3 fires the
# moment ANY of those eight names appears in NAMESPACE -- at which point all 51
# of VD-001's cells must be covered in the same change (P0.5 failure condition
# 3). That is why the engine landed unexported first and why the eight exports
# then landed together, with `verification/key-join-discharge.md` claiming
# `DISCHARGED[VD-001]` in the same change. Output surface (b) did the same:
# VD-002 records `.serialize_whatwg_full_vec` / `.serialize_rfc_full_vec`
# shipping in `df00da8` while the public `serialize_url()` waited for P2.5, and
# D3 stayed green throughout because the probe named only the export.
#
# Surface discipline (design/work/url-v3/contracts/key-join-contracts.md):
#
#   * The key is derived from the CANONICAL IDENTITY STATE -- after standard
#     interpretation, before any cleaning or display transform -- and NEVER from
#     `clean_url` or any other presentation string (P3.1 D-A, C-06). It shares
#     `.fsss_record_vec()` with surface (b) for exactly that reason: that
#     builder is the only place the identity record is assembled.
#   * NO PRESENTATION DIAL reaches it (P3.1 D-A.3). The posture below pins every
#     cleaning/display knob to its identity value, so `path_encoding`,
#     `host_encoding`, `www_handling`, case, query cleaning, `port_handling`,
#     index/trailing-slash and every profile bundle are structurally incapable
#     of moving a key byte. This is the load-bearing non-interference
#     invariant.
#   * Framing is INJECTIVE, not delimiter concatenation (P3.1 D-A.2). Component
#     distinctness is guaranteed by construction, never hoped for by escaping.
#   * The key is NOT a URL and must never be rendered as one.

# Bumped when the key's byte layout changes; bumped when any default or rule
# changes what two URLs compare equal (P3.1 D-B). Both travel INSIDE the framed
# bytes, so a key minted under other semantics can never compare equal to one
# minted here -- "no release may silently reinterpret a persisted older key".
.URL_KEY_SCHEMA_VERSION <- 1L
.URL_KEY_POLICY_VERSION <- 1L

# Typed keyability reasons (P3.1 D-C). `NA` alone is forbidden: it would
# conflate missing input with an invalid parse, so every non-keyable row carries
# one of these instead.
.URL_KEY_REASONS <- c(
  "ok", "missing-input", "empty-input", "invalid-parse"
)

.URL_KEY_STANDARDS <- c("whatwg", "rfc3986")
.URL_KEY_SCHEME_EQUALITY <- c("exact", "http_https", "http_https_missing")

# The scheme family the relaxed modes collapse, and the token they collapse it
# to. The token is deliberately UNSPELLABLE as a scheme: `/` is outside RFC
# 3986's `scheme` production (`[A-Za-z][A-Za-z0-9+.-]*`), so no input can
# present a real scheme that forges membership in the collapsed class. A token
# like `http+https` would be forgeable, since `+` IS in the production.
.URL_KEY_COLLAPSED_SCHEMES <- c("http", "https")
.URL_KEY_COLLAPSED_SCHEME_TOKEN <- "http/https"

# The comparison-key policy. ONE immutable, symmetric, versioned object shared
# by both sides of any comparison or join -- side-specific rules are prohibited
# because equality must stay symmetric and transitive
# (`contracts/key-join-contracts.md:66`).
#
# `standard` defaults to "whatwg" per P3.2 D-A: the key surface is new in v3
# with no persisted keys and no back-compat debt, so it anchors to a standard's
# identity semantics rather than to `parse_url()`'s `NULL` infer-a-scheme
# heuristic. `NULL` is deliberately NOT accepted -- an unnamed standard cannot
# freeze key bytes, which is the whole point of KJ-O1.
#
# `url_key_policy()` at the foot of this file is the exported wrapper.
.url_key_policy_spec <- function(standard = "whatwg",
                                 scheme_equality = "exact") {
  standard <- match.arg(standard, .URL_KEY_STANDARDS)
  scheme_equality <- match.arg(scheme_equality, .URL_KEY_SCHEME_EQUALITY)

  # `http_https` was blocked with `http_https_missing` until the owner ruled on
  # RURL-ixlultql. The clash was real: rows 1, 2 and 7 of the scheme/port truth
  # table force `HTTP:80 ~ HTTPS:443` under transitive closure, while row 6
  # declared that pair `distinct` in both relaxed columns, and equality is
  # contractually an equivalence relation (`:66`). The ruling amends row 6's
  # relaxed cells to `equal`, which is exactly the closure, so the mode became
  # implementable without picking a side. The contract TEXT still reads
  # `distinct` on purpose -- that edit rides the cp-snapshot-3 seal
  # (RURL-isbsbrry); this code ships against a PROPOSED record, as
  # `serialize_url()` did against P2.5.
  #
  # `http_https_missing` stays refused for a DIFFERENT reason, so the citation
  # moves with it. Row 8 declares `missing scheme/no port` == `HTTP/no port`,
  # which collapsing scheme + presence cannot deliver: the pair also differs on
  # `authority_delimiter_present`, which P1.2 D-A frames as independent
  # identity. Satisfying row 8 needs a third collapse reaching into a SETTLED
  # contract, and row 11's scheme-relative branch is defective on top of that
  # (RURL-kmkyicpt). Tracked as RURL-ixxvjjwj.
  if (identical(scheme_equality, "http_https_missing")) {
    stop(
      "scheme_equality = \"http_https_missing\" is not implemented: ",
      "truth-table row 8 also requires collapsing ",
      "`authority_delimiter_present`, which P1.2 D-A frames as independent ",
      "identity, and row 11's scheme-relative branch is unsettled. ",
      "See RURL-ixxvjjwj. Use \"exact\" or \"http_https\".",
      call. = FALSE
    )
  }

  structure(
    list(
      standard = standard,
      scheme_equality = scheme_equality,
      key_version = .URL_KEY_POLICY_VERSION,
      schema_version = .URL_KEY_SCHEMA_VERSION
    ),
    class = "rurl_url_key_policy"
  )
}

# The key's own parse posture. Two axes differ from `.fsss_parse_options()` and
# both are forced by the contract, not by preference:
#
#   * `scheme_policy = "infer"` -- the FSSS uses "require", under which every
#     scheme-less input is a parse failure. The key's truth table needs
#     missing-scheme rows to be KEYABLE and distinguishable ("missing scheme/no
#     port" vs "HTTP/no port", `:112-115`), and `http_https_missing` exists only
#     to collapse them, which a never-match NA could not express. Presence is
#     framed separately (`.url_key_scheme_presence()`), so inference informs the
#     effective scheme WITHOUT erasing the fact that none was written -- exactly
#     what `:80` requires: "inference never exists only to erase missing".
#   * `scheme_acceptance = "general"` -- the key must cover opaque and
#     non-web schemes, not just the web set.
#
# Every remaining knob is the standard's own identity posture, inherited from
# the same profile surface (b) uses. Nothing here is a caller-visible dial.
.url_key_parse_options <- function(policy, engine = NULL) {
  bundle <- .URL_PROFILES[[.FSSS_PROFILE[[policy$standard]]]]
  authorized <- bundle[intersect(
    names(bundle), c("path_normalization", "path_identity")
  )]
  args <- bundle[setdiff(names(bundle), "path_identity")]
  args$engine <- engine
  args$scheme_policy <- "infer"
  args$scheme_acceptance <- "general"
  if (length(authorized) > 0L) {
    args$profile_authorized <- authorized
  }
  do.call(.parse_options, args)
}

# Scheme SOURCE state, framed independently of the effective scheme (`:80`).
# Read lexically off the source rather than from a parse column, because no
# Stage-A flag answers this question: `original_has_allowed_scheme` is FALSE for
# `mailto:x@y.com`, which carries a perfectly explicit scheme, so it reports
# web-set membership rather than presence.
#
# Order matters. `//host` is scheme-relative, an unresolved-reference kind that
# ratification Q4 pins as DISTINCT from "missing" and therefore never collapsed
# by `http_https_missing`.
#
# `looks_like_host_port` is REQUIRED, not an optimization. The lexical regex
# below cannot tell a scheme from a host:port: `h.com` satisfies RFC 3986's
# `scheme` production, so `h.com:80/` matches `^[A-Za-z][A-Za-z0-9+.-]*:` and
# reads as an explicit scheme. The truth table settles what it actually is --
# row 9's left state is labelled literally "missing scheme `:80`"
# (`contracts/key-join-contracts.md:113`) -- so the Stage-A flag that exists for
# exactly this shape (RURL-aldwnots) overrides the regex.
#
# Getting this wrong was NOT harmless. Misreading presence as `explicit` also
# makes `.url_key_port()` eligible to normalize the row's `:80` away under the
# INFERRED http scheme, which ratification Q4 forbids. Under `exact` the two
# errors cancel -- the wrong presence keeps `h.com:80/` distinct from `h.com/`,
# masking the wrong port -- and a relaxed mode that collapses presence removes
# the compensation, making `h.com:80/` == `h.com/`. Verified by measurement, and
# pinned by tests over both modes.
# `looks_like_host_port` has NO default on purpose: the compensation described
# above is re-introducible by any caller that omits it, so omitting it is made
# impossible rather than merely discouraged.
.url_key_scheme_presence <- function(url, url_standard, looks_like_host_port) {
  u <- ifelse(is.na(url), "", as.character(url))
  if (.is_whatwg(url_standard)) {
    u <- .strip_whatwg_control_chars_vec(u, url_standard)$url
    u <- stringi::stri_replace_first_regex(u, "^[\\u0000-\\u0020]+", "")
  }
  out <- rep("inferred", length(u))
  out[stringi::stri_startswith_fixed(u, "//")] <- "scheme-relative"
  explicit <- stringi::stri_detect_regex(u, "^[A-Za-z][A-Za-z0-9+.\\-]*:")
  out[!is.na(explicit) & explicit] <- "explicit"
  hp <- !is.na(looks_like_host_port) & looks_like_host_port
  out[hp & out == "explicit"] <- "inferred"
  out
}

# The key's port rule (key-policy v1, `:83`). Normalize absent vs explicit
# default for HTTP 80 and HTTPS 443 ONLY, and only under the row's OWN EXPLICIT
# recognized scheme. Everything else stays literal:
#
#   * ftp/ws/wss keep their defaults, so `ftp://h:21/` != `ftp://h/`
#     (ratification Q8) -- which is why this reads `syntactic_port` and not the
#     record's `port`, whose WHATWG elision already collapsed all five.
#   * an INFERRED scheme normalizes nothing, so `h.com:80/x` != `http://h.com/x`
#     (ratification Q4: no effective-scheme guessing).
#   * another scheme's default stays significant, so `https://h:80/` keeps `80`.
.url_key_port <- function(scheme, syntactic_port, presence) {
  out <- syntactic_port
  eligible <- !is.na(scheme) & !is.na(out) & presence == "explicit"
  default <- (scheme == "http" & out == "80") |
    (scheme == "https" & out == "443")
  out[eligible & default] <- NA_character_
  out
}

# The relaxed scheme collapse (key-policy `:81`, truth-table rows 6-7 as
# amended). Applied to the framed scheme field ONLY, and only for the two named
# web schemes -- "every other valid scheme remains exact", so the ws/wss pair,
# which shares http/https's default ports, is untouched.
#
# MUST run after `.url_key_port()`, and must never be fed to it. The contract
# opens the truth table with the sequencing rule (`:99`): default-port
# normalization happens BEFORE collapsing and uses "each row's own explicit
# recognized scheme". Feed the collapsed token to the port rule and neither
# `scheme == "http"` nor `scheme == "https"` matches, so rows 1-2 stop
# normalizing; collapse the scheme first and instead `https://h.com:80/` would
# lose its `:80`, breaking row 4. Only this order satisfies both.
#
# Transitivity is structural rather than argued: the collapse is a FUNCTION of
# the field, so key equality stays equality of a projection -- an equivalence
# relation by construction, whatever the table says.
.url_key_scheme_collapse <- function(scheme, scheme_equality) {
  if (identical(scheme_equality, "exact")) {
    return(scheme)
  }
  out <- scheme
  hit <- !is.na(out) & out %in% .URL_KEY_COLLAPSED_SCHEMES
  out[hit] <- .URL_KEY_COLLAPSED_SCHEME_TOKEN
  out
}

# Injective length-prefixed framing (P3.1 D-A.2). Each field emits
# `<nbytes>:<utf8 bytes>`, and `NA` emits the digit-free sentinel `-:`. A
# decoder reads the length, then exactly that many bytes, so no component value
# -- however many separators, control bytes or `:`s it carries -- can be
# mistaken for a field boundary. That is what makes collision a structural
# impossibility rather than an escaping problem.
#
# Byte counts come from `stringi::stri_numbytes()` over an explicitly UTF-8
# transcoded value, so the frame is identical under every locale. Lengths are
# only ever COUNTED here, never used to slice, so the code-point/native-index
# does not arise.
.url_key_frame <- function(fields) {
  n <- unique(lengths(fields))
  stopifnot(length(n) == 1L)
  if (n == 0L) {
    return(character(0))
  }
  pieces <- lapply(fields, function(x) {
    x <- as.character(x)
    enc <- stringi::stri_enc_toutf8(x, validate = TRUE)
    out <- paste0(stringi::stri_numbytes(enc), ":", enc)
    out[is.na(x)] <- "-:"
    out
  })
  do.call(paste0, pieces)
}

# The identity tuple, in a FIXED order. `fragment` and `userinfo` are absent by
# contract, not by omission: ratification Q5 makes both invisible to
# web-resource identity, and their structural state stays available for
# diagnostics on the parse side.
#
# `authority_delimiter_present` + `host_kind` carry the authority state (P1.2
# D-A). `authority_payload_kind` is deliberately NOT framed: the only
# distinction it adds over `host_kind` for a keyable row is a userinfo-only
# (`//user@/p` against `///p`), and that is precisely a userinfo distinction,
# which Q5 excludes from identity.
.url_key_state_vec <- function(url, policy, engine = NULL) {
  opts <- .url_key_parse_options(policy, engine)
  rec <- .fsss_record_vec(url, policy$standard, engine, opts = opts)
  # Stage A under the SAME options object `.fsss_record_vec()` just used, so
  # this is a cache hit rather than a second parse, and the flag cannot come
  # from a posture the record did not see.
  presence <- .url_key_scheme_presence(
    url, opts$url_standard,
    looks_like_host_port = ._parse_stage_a_vec(url, opts)$looks_like_host_port
  )
  # Order is load-bearing, see `.url_key_scheme_collapse()`: the port rule reads
  # the row's OWN scheme off the record, and only then is the framed scheme
  # field collapsed. `rec$scheme` is never overwritten, so no later reader can
  # pick up the token by accident.
  port <- .url_key_port(rec$scheme, rec$syntactic_port, presence)
  scheme <- .url_key_scheme_collapse(rec$scheme, policy$scheme_equality)

  list(
    ok = rec$ok,
    fields = list(
      # Policy identity leads the tuple: a key minted under a different version,
      # standard or equality mode can then never collide with one minted here.
      key_version = rep(as.character(policy$key_version), length(url)),
      schema_version = rep(as.character(policy$schema_version), length(url)),
      standard = rep(policy$standard, length(url)),
      scheme_equality = rep(policy$scheme_equality, length(url)),
      scheme_presence = presence,
      scheme = scheme,
      authority_delimiter_present = ifelse(
        rec$authority_delimiter_present, "T", "F"
      ),
      host_kind = rec$host_kind,
      host = rec$host,
      port = port,
      path_kind = rec$path_kind,
      rfc_path_form = rec$rfc_path_form,
      path = rec$path,
      query_kind = rec$query_kind,
      query = rec$query
    )
  )
}

# Typed keyability, in precedence order (P3.1 D-C). Missing input and an invalid
# parse are separated by construction, which is the one thing D-C forbids
# conflating.
.url_key_reason_vec <- function(url, ok) {
  out <- rep("ok", length(url))
  out[!ok] <- "invalid-parse"
  empty <- !is.na(url) & !nzchar(url)
  out[empty & !ok] <- "empty-input"
  out[is.na(url)] <- "missing-input"
  out
}

# Constructor for the classed key. A classed CHARACTER vector, so `==`, `match`,
# `duplicated` and `%in%` -- everything the join family needs -- work on the
# framed bytes natively and cannot disagree with each other. Versions ride in
# attributes as the contract's "class metadata", and also inside the bytes.
.new_url_key <- function(value, policy, reason) {
  structure(
    value,
    key_version = policy$key_version,
    schema_version = policy$schema_version,
    standard = policy$standard,
    scheme_equality = policy$scheme_equality,
    keyability = reason,
    class = "rurl_url_key"
  )
}

# The comparison projection. Length-preserving and names-preserving; the policy
# is scalar and is never silently recycled.
.url_key_compute_vec <- function(url, policy = .url_key_policy_spec(),
                                 engine = NULL) {
  if (!inherits(policy, "rurl_url_key_policy")) {
    stop("`policy` must be a rurl_url_key_policy object.", call. = FALSE)
  }
  nms <- names(url)
  if (is.factor(url)) {
    url <- as.character(url)
  }
  url <- unname(url)
  n <- length(url)
  if (n == 0L) {
    return(.new_url_key(character(0), policy, character(0)))
  }
  if (!is.character(url)) {
    url <- vapply(
      as.list(url),
      function(u) if (is.character(u) && length(u) == 1L) u else NA_character_,
      character(1)
    )
  }

  st <- .url_key_state_vec(url, policy, engine)
  reason <- .url_key_reason_vec(url, st$ok)
  value <- rep(NA_character_, n)
  keep <- which(st$ok)
  if (length(keep) > 0L) {
    value[keep] <- .url_key_frame(lapply(st$fields, function(f) f[keep]))
  }
  names(value) <- nms
  .new_url_key(value, policy, reason)
}

# --- classed-key methods -----------------------------------------------------

# The policy prints as what it IS -- the four fields that decide equality --
# rather than as the bare list `str()` would show. Both versions are displayed
# because they are the fields that make a persisted key re-readable.
#' @export
print.rurl_url_key_policy <- function(x, ...) {
  cat(sprintf(
    paste0("<rurl_url_key_policy> standard=%s  scheme_equality=%s  ",
           "key_version=%d  schema_version=%d\n"),
    x$standard, x$scheme_equality, x$key_version, x$schema_version
  ))
  invisible(x)
}

# The printable form is DIAGNOSTICS ONLY (P3.1 D-A.2). It never round-trips and
# is never a URL: the framed bytes are shown truncated, with the policy identity
# that produced them, so a key can be recognized in output without inviting
# anyone to parse or persist the display string.
#' @export
format.rurl_url_key <- function(x, ...) {
  v <- unclass(x)
  out <- ifelse(
    is.na(v), paste0("<non-keyable: ", attr(x, "keyability"), ">"),
    paste0("<url_key ", substr(v, 1L, 24L), "...>")
  )
  names(out) <- names(v)
  out
}

#' @export
print.rurl_url_key <- function(x, ...) {
  cat(sprintf(
    "<rurl_url_key> n=%d  standard=%s  scheme_equality=%s  key_version=%d\n",
    length(x), attr(x, "standard"), attr(x, "scheme_equality"),
    attr(x, "key_version")
  ))
  print(format(x), quote = FALSE)
  invisible(x)
}

# Subsetting keeps the class, the policy metadata AND the per-row keyability
# reasons aligned with the rows that survived. Dropping the attributes here
# would make `key[i]` a bare character vector and silently lose the version the
# contract requires the key to carry.
#' @export
`[.rurl_url_key` <- function(x, i, ...) {
  structure(
    unclass(x)[i],
    key_version = attr(x, "key_version"),
    schema_version = attr(x, "schema_version"),
    standard = attr(x, "standard"),
    scheme_equality = attr(x, "scheme_equality"),
    keyability = attr(x, "keyability")[i],
    class = "rurl_url_key"
  )
}

#' @export
as.character.rurl_url_key <- function(x, ...) {
  v <- unclass(x)
  attributes(v) <- NULL
  names(v) <- names(unclass(x))
  v
}

# --- the exported surface ----------------------------------------------------
#
# Two thin wrappers. They add no behavior on purpose: the engine above is what
# `tests/testthat/test-url-key.R` pins, and a wrapper that re-implemented any of
# it would give the public surface a second, unpinned notion of identity.
#
# NO `engine` ARGUMENT, deliberately, and it is not an oversight. The key frames
# no PSL-derived component, so a `pslr` engine cannot move a key byte -- that is
# asserted, not assumed (`test-url-key.R`). A public `engine =` here would
# therefore be a dial that provably does nothing. The six joins DO take one,
# because `warnings = "reject"` reads the L3 PSL annotation and a divergent
# suffix list can move eligibility: identity is engine-independent, eligibility
# is not, and the two signatures say so.

#' Comparison-key policy
#'
#' Builds the immutable, versioned policy object that governs URL *identity*
#' for [get_url_key()] and the [url_join] family. One policy is applied
#' symmetrically to both sides of every comparison, because equality has to
#' stay symmetric and transitive.
#'
#' @section Identity is not presentation:
#'
#' A comparison key is derived from the URL's canonical identity state -- after
#' the selected standard has interpreted it, and *before* any cleaning or
#' display transform. No cleaning option can reach it. `www_handling`,
#' `case_handling`, `trailing_slash_handling`, `index_page_handling`,
#' `path_encoding`, `host_encoding`, `port_handling`, query cleaning and every
#' [url_profile()] bundle are structurally incapable of changing a key byte.
#' That is the point: two URLs that a cleaning profile happens to render alike
#' are not thereby the same resource.
#'
#' @section What the key does and does not distinguish:
#'
#' Framed as identity: the scheme (and, separately, whether one was written at
#' all), the authority delimiter, the host and its kind, the port, the path and
#' its kind, and the query -- order and duplicates significant.
#'
#' Excluded by contract: the fragment and any userinfo. Neither identifies a
#' web resource, so `http://u:pw@h/p#frag` and `http://h/p` mint the same key.
#' Their structural state is still available from [safe_parse_url()] and the
#' diagnostics helpers.
#'
#' Ports normalize only where the standard makes them redundant: an explicit
#' `:80` under `http` and `:443` under `https` compare equal to no port at all.
#' Every other default stays literal, so `ftp://h:21/` and `ftp://h/` are
#' distinct, and an inferred scheme normalizes nothing (`h.com:80/` is not
#' `http://h.com/`).
#'
#' @section Versioning:
#'
#' The policy carries a key version and a schema version, and both travel
#' *inside* the framed key bytes. A key minted under different semantics can
#' therefore never compare equal to one minted here, so no release can silently
#' reinterpret a persisted key.
#'
#' @param standard The standard whose identity semantics apply: `"whatwg"`
#'   (default) or `"rfc3986"`. Unlike the parse surface, `NULL` is not accepted
#'   -- an unnamed standard cannot freeze key bytes.
#' @param scheme_equality How strictly schemes compare. `"exact"` (default)
#'   compares the normalized scheme identity. `"http_https"` additionally
#'   collapses `http` and `https` into one class, so `http://h/` and
#'   `https://h/` compare equal; every other scheme stays exact, including the
#'   `ws`/`wss` pair. `"http_https_missing"` is accepted by the vocabulary but
#'   not implemented, and errors -- see Details.
#'
#' @details
#' `scheme_equality = "http_https_missing"` would additionally collapse "no
#' scheme written" into the `http`/`https` class. It errors rather than
#' guessing, because the pair it would have to equate also differs on whether
#' an authority delimiter (`//`) was present, which rurl frames as independent
#' identity. Collapsing that too is a contract change, not an implementation
#' detail, so the mode refuses instead of silently picking a side.
#'
#' @return An object of class `rurl_url_key_policy`.
#'
#' @seealso [get_url_key()] for the key itself, and [url_join] for the joins
#'   that consume it.
#'
#' @examples
#' url_key_policy()
#'
#' # Identity under one policy ...
#' get_url_key(c("http://example.com/", "https://example.com/"))
#'
#' # ... and under a relaxed scheme mode.
#' p <- url_key_policy(scheme_equality = "http_https")
#' k <- get_url_key(c("http://example.com/", "https://example.com/"), p)
#' k[1] == k[2]
#'
#' @export
url_key_policy <- function(standard = c("whatwg", "rfc3986"),
                           scheme_equality = c("exact", "http_https",
                                               "http_https_missing")) {
  # `NULL` is rejected rather than absorbed. `match.arg(NULL, choices)` quietly
  # returns `choices[[1]]`, which would make `standard = NULL` a silent
  # "whatwg" -- and on the parse surface `url_standard = NULL` means the
  # OPPOSITE, "infer per input". A key cannot be minted under an unnamed
  # standard, so the mistake is refused at the edge instead of guessed.
  if (is.null(standard) || is.null(scheme_equality)) {
    stop(
      "`standard` and `scheme_equality` must be named; NULL is not accepted. ",
      "A key minted under an unnamed standard could not be frozen or ",
      "re-read. Pass standard = \"whatwg\" or \"rfc3986\".",
      call. = FALSE
    )
  }
  .url_key_policy_spec(
    standard = match.arg(standard),
    scheme_equality = match.arg(scheme_equality)
  )
}

#' URL comparison key
#'
#' Projects each URL onto a versioned, non-URL comparison key: the value rurl
#' uses to decide whether two URLs identify the same web resource. Use it to
#' deduplicate, group, or match URLs without relying on a cleaned display
#' string.
#'
#' @section The key is not a URL:
#'
#' The returned object is a classed character vector whose contents are
#' injectively framed component bytes, not a URL. Never parse it, never render
#' it to users, and never reconstruct a URL from it. `print()` deliberately
#' shows a truncated diagnostic form for that reason. What it *is* good for is
#' comparison: `==`, [match()], [duplicated()], `%in%` and the [url_join]
#' family all work on it directly.
#'
#' Framing is length-prefixed, so component boundaries cannot be forged. A host
#' or path containing separators, control bytes or colons can never make two
#' different URLs collide.
#'
#' @section Non-keyable input:
#'
#' A URL the selected standard cannot parse has no identity, so its key is
#' `NA` and never matches anything -- not even another `NA`. The reason is kept
#' alongside rather than collapsed into the `NA`, and is readable with
#' `attr(key, "keyability")`: one of `"ok"`, `"missing-input"`, `"empty-input"`
#' or `"invalid-parse"`. Missing input is never conflated with an invalid
#' parse.
#'
#' @param url A character vector of URLs. Factors are coerced.
#' @param policy A `rurl_url_key_policy` object from [url_key_policy()],
#'   which is also the default. The policy is scalar and is never recycled.
#'
#' @return A classed character vector (`rurl_url_key`) the same length as
#'   `url`, preserving its names. `NA` for a non-keyable element. The policy
#'   version, schema version, standard, scheme-equality mode and the per-element
#'   `keyability` reasons ride along as attributes.
#'
#' @seealso [url_key_policy()] for the dials, [url_join] for joining on the
#'   key, and [serialize_url()] for a standard's full-string serialization
#'   (which *is* a URL, unlike this).
#'
#' @examples
#' # Presentation differences that are not identity differences.
#' get_url_key(c("http://example.com:80/a", "http://example.com/a"))
#'
#' # The fragment and userinfo are excluded from web-resource identity.
#' k <- get_url_key(c("http://u:pw@example.com/a#top", "http://example.com/a"))
#' k[1] == k[2]
#'
#' # Query order and duplicates are significant.
#' k <- get_url_key(c("http://example.com/?a=1&b=2",
#'                    "http://example.com/?b=2&a=1"))
#' k[1] == k[2]
#'
#' # Deduplicate by identity rather than by string.
#' u <- c("HTTP://Example.com/a", "http://example.com/a",
#'        "http://example.com/b")
#' u[!duplicated(get_url_key(u))]
#'
#' # Non-keyable input carries a typed reason.
#' k <- get_url_key(c("http://example.com/", NA, "", ":::"))
#' attr(k, "keyability")
#'
#' @export
get_url_key <- function(url, policy = url_key_policy()) {
  .url_key_compute_vec(url, policy)
}
