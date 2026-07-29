# Output surface (e) -- the v3 comparison key. THE ENGINE ONLY: nothing here is
# exported yet, deliberately.
#
# `registers/verification-deferrals.md` VD-001 probes this surface with
# `export:get_url_key;...;export:url_anti_join`, and deferral-gate D3 fires the
# moment ANY of those eight names appears in NAMESPACE -- at which point all 51
# of VD-001's cells must be covered in the same change (P0.5 failure condition
# 3). The six joins do not exist yet, so exporting the key half alone would trip
# D3 while leaving 14 cells unverifiable. The engine therefore lands
# unexported first, exactly as output surface (b) did: VD-002 records that
# `.serialize_whatwg_full_vec` / `.serialize_rfc_full_vec` shipped in `df00da8`
# while the public `serialize_url()` waited for P2.5, and D3 stayed green
# throughout because the probe named only the export.
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
# The public `url_key_policy()` wrapper ships with the join family.
.url_key_policy_spec <- function(standard = "whatwg",
                                 scheme_equality = "exact") {
  standard <- match.arg(standard, .URL_KEY_STANDARDS)
  scheme_equality <- match.arg(scheme_equality, .URL_KEY_SCHEME_EQUALITY)

  # The two relaxed modes are SPECIFIED but not implementable as written: rows
  # 1, 2 and 7 of the scheme/port truth table force `HTTP:80 ~ HTTPS:443` under
  # transitive closure, while row 6 of the same table declares that pair
  # `distinct` in both relaxed columns. Equality is contractually an equivalence
  # relation (`:66`), so no implementation can satisfy both. Refusing is the
  # only honest option -- picking a side would silently install one reading of a
  # SETTLED contract as fact. Tracked as RURL-ixlultql.
  if (!identical(scheme_equality, "exact")) {
    stop(
      "scheme_equality = \"", scheme_equality, "\" is not implemented: the ",
      "scheme/port truth table is non-transitive in the relaxed columns ",
      "(HTTP:80 vs HTTPS:443), so no equivalence relation satisfies it. ",
      "See RURL-ixlultql. Use scheme_equality = \"exact\" (the default).",
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
.url_key_scheme_presence <- function(url, url_standard) {
  u <- ifelse(is.na(url), "", as.character(url))
  if (.is_whatwg(url_standard)) {
    u <- .strip_whatwg_control_chars_vec(u, url_standard)$url
    u <- stringi::stri_replace_first_regex(u, "^[\\u0000-\\u0020]+", "")
  }
  out <- rep("inferred", length(u))
  out[stringi::stri_startswith_fixed(u, "//")] <- "scheme-relative"
  explicit <- stringi::stri_detect_regex(u, "^[A-Za-z][A-Za-z0-9+.\\-]*:")
  out[!is.na(explicit) & explicit] <- "explicit"
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
  presence <- .url_key_scheme_presence(url, opts$url_standard)
  port <- .url_key_port(rec$scheme, rec$syntactic_port, presence)

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
      scheme = rec$scheme,
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
