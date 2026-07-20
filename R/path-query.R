# Low-level path normalization and query-string parsing helpers.

# Internal helper to collapse duplicate slashes in paths
._collapse_path_slashes <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  gsub("/+", "/", path, perl = TRUE)
}

# Internal helper to remove dot segments per RFC 3986
._remove_dot_segments <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }

  input <- path
  output <- ""

  while (nzchar(input)) {
    if (stringi::stri_startswith_fixed(input, "../")) {
      input <- stringi::stri_sub(input, 4)
    } else if (stringi::stri_startswith_fixed(input, "./")) {
      input <- stringi::stri_sub(input, 3)
    } else if (stringi::stri_startswith_fixed(input, "/./")) {
      input <- paste0("/", stringi::stri_sub(input, 4))
    } else if (identical(input, "/.")) {
      input <- "/"
    } else if (stringi::stri_startswith_fixed(input, "/../")) {
      input <- paste0("/", stringi::stri_sub(input, 5))
      output <- sub("/?[^/]*$", "", output)
    } else if (identical(input, "/..")) {
      input <- "/"
      output <- sub("/?[^/]*$", "", output)
    } else if (identical(input, ".") || identical(input, "..")) {
      input <- ""
    } else {
      match <- regexpr("^(/?[^/]*)", input, perl = TRUE)
      segment <- regmatches(input, match)
      output <- paste0(output, segment)
      input <- substring(input, attr(match, "match.length") + 1)
    }
  }

  output
}

# Internal helper: decode ONLY unreserved percent-encoded octets (RFC 3986
# section 6.2.2.2: ALPHA / DIGIT / "-" / "." / "_" / "~"), used by the
# `url_standard = "rfc3986"` path profile (RURL-gjltzwmp). Reserved and
# non-ASCII percent-triplets (`%2F`, `%3F`, `%23`, `%C3`, ...) are left
# encoded -- this is deliberately not a full percent-decode. Any triplet left
# encoded has its hex digits canonicalized to uppercase, so `%2f`/`%2F`
# compare equal in canonical keys.
.rfc_unreserved_normalize <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  m <- gregexpr("%[0-9A-Fa-f]{2}", path, perl = TRUE)
  matches <- regmatches(path, m)[[1]]
  if (length(matches) == 0L) {
    return(path)
  }
  codes <- strtoi(substring(matches, 2L), base = 16L)
  is_unreserved <- (codes >= 65L & codes <= 90L) | # A-Z
    (codes >= 97L & codes <= 122L) | # a-z
    (codes >= 48L & codes <= 57L) | # 0-9
    codes %in% c(45L, 46L, 95L, 126L) # - . _ ~
  replacement <- ifelse(
    is_unreserved,
    vapply(codes, function(code) rawToChar(as.raw(code)), character(1)),
    .ascii_toupper(matches)
  )
  regmatches(path, m) <- list(replacement)
  path
}

# Internal helper: an atom-aware dot-segment remover for the
# `url_standard = "whatwg"` path profile (RURL-bbmuehsx, PRD S6.1). WHATWG
# preserves percent-encoded unreserved bytes in path identity -- it must NOT
# run a general percent-decode before dot-segment removal (unlike RFC mode).
# Instead this recognizes a WHATWG dot atom (literal "." or encoded "%2e"/
# "%2E") wherever the RFC 3986 S5.2.4 algorithm matches a literal "." or "..",
# so "%2e", ".%2e", "%2e.", and "%2e%2e" resolve exactly like their literal
# counterparts while any percent-triplet that is NOT part of a whole dot
# segment (e.g. "%2eb") is left untouched as ordinary path data.
._remove_dot_segments_whatwg <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }

  dot <- "(?:\\.|%2[eE])"
  dotdot <- paste0(dot, "{2}")
  match_len <- function(pattern, x) {
    m <- regexpr(pattern, x, perl = TRUE)
    if (m == -1L) NA_integer_ else attr(m, "match.length")
  }

  input <- path
  output <- ""

  while (nzchar(input)) {
    len <- match_len(paste0("^", dotdot, "/"), input)
    if (!is.na(len)) {
      input <- stringi::stri_sub(input, len + 1)
      next
    }
    len <- match_len(paste0("^", dot, "/"), input)
    if (!is.na(len)) {
      input <- stringi::stri_sub(input, len + 1)
      next
    }
    len <- match_len(paste0("^/", dot, "/"), input)
    if (!is.na(len)) {
      input <- paste0("/", stringi::stri_sub(input, len + 1))
      next
    }
    len <- match_len(paste0("^/", dot, "$"), input)
    if (!is.na(len)) {
      input <- "/"
      next
    }
    len <- match_len(paste0("^/", dotdot, "/"), input)
    if (!is.na(len)) {
      input <- paste0("/", stringi::stri_sub(input, len + 1))
      output <- sub("/?[^/]*$", "", output)
      next
    }
    len <- match_len(paste0("^/", dotdot, "$"), input)
    if (!is.na(len)) {
      input <- "/"
      output <- sub("/?[^/]*$", "", output)
      next
    }
    len <- match_len(paste0("^", dot, "$"), input)
    if (!is.na(len)) {
      input <- ""
      next
    }
    len <- match_len(paste0("^", dotdot, "$"), input)
    if (!is.na(len)) {
      input <- ""
      next
    }
    match <- regexpr("^(/?[^/]*)", input, perl = TRUE)
    segment <- regmatches(input, match)
    output <- paste0(output, segment)
    input <- substring(input, attr(match, "match.length") + 1)
  }

  output
}

# Internal helper for the `url_standard = "whatwg"` path profile (RURL-bbmuehsx,
# PRD S6.1). Unlike the RFC mode's `.rfc_unreserved_normalize()`, this never
# decodes or canonicalizes percent-triplets: WHATWG preserves existing percent
# spellings in the path identity (`/%7e` stays `/%7e`).
.whatwg_preserve_normalize <- function(path) {
  path
}

# Internal helper to strip index/default pages from the end of a path
._strip_index_page <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  match <- stringi::stri_match_first_regex(
    path,
    "(?i)^(.*)/(index|default)\\.[^/]+/?$"
  )
  if (is.na(match[1, 1])) {
    return(path)
  }
  base <- match[1, 2]
  if (is.na(base) || base == "") {
    return("/")
  }
  if (!stringi::stri_startswith_fixed(base, "/")) {
    base <- paste0("/", base)
  }
  base
}

# Internal helper to percent-encode path segments
._encode_path_segments <- function(path) {
  if (is.na(path)) {
    return(path)
  }
  has_leading <- stringi::stri_startswith_fixed(path, "/")
  has_trailing <- stringi::stri_endswith_fixed(path, "/")
  segments <- strsplit(path, "/", fixed = TRUE)[[1]]
  # curl_escape() is vectorized: one call escapes every segment (and returns
  # character(0) for a character(0) input), so paste() recomposes identically
  # to the former per-segment vapply().
  encoded_segments <- curl::curl_escape(segments)
  recomposed <- paste(encoded_segments, collapse = "/")
  if (has_leading && !stringi::stri_startswith_fixed(recomposed, "/")) {
    recomposed <- paste0("/", recomposed)
  }
  if (has_trailing && !stringi::stri_endswith_fixed(recomposed, "/")) {
    recomposed <- paste0(recomposed, "/")
  }
  recomposed
}

# WHATWG component serializer core: encode C0 controls, non-ASCII UTF-8 bytes,
# and the component-specific encode set, while preserving every existing
# percent sign spelling so `%2e`, `%2E`, `%`, and `%2z` do not get decoded,
# hex-normalized, or double-encoded.
.whatwg_component_percent_encode <- function(x, encode_set) {
  if (is.na(x) || !nzchar(x)) {
    return(x)
  }

  bytes <- as.integer(charToRaw(enc2utf8(x)))
  out <- character(length(bytes))
  j <- 1L
  i <- 1L
  hex <- as.integer(charToRaw("0123456789ABCDEFabcdef"))

  while (i <= length(bytes)) {
    byte <- bytes[i]
    if (byte == 0x25L) {
      out[j] <- "%"
      j <- j + 1L
      if (i + 2L <= length(bytes) &&
          bytes[i + 1L] %in% hex &&
          bytes[i + 2L] %in% hex) {
        out[j] <- rawToChar(as.raw(bytes[i + 1L]))
        out[j + 1L] <- rawToChar(as.raw(bytes[i + 2L]))
        j <- j + 2L
        i <- i + 3L
      } else {
        i <- i + 1L
      }
      next
    }

    if (byte <= 0x1FL || byte >= 0x7FL || byte %in% encode_set) {
      out[j] <- sprintf("%%%02X", byte)
    } else {
      out[j] <- rawToChar(as.raw(byte))
    }
    j <- j + 1L
    i <- i + 1L
  }

  paste(out[seq_len(j - 1L)], collapse = "")
}

# WHATWG path serializer for `url_standard = "whatwg"` +
# `path_encoding = "encode"`: uses the path percent-encode set
# (`" # < > ? ` { }`).
.whatwg_path_percent_encode <- function(path) {
  .whatwg_component_percent_encode(
    path,
    c(0x20L, 0x22L, 0x23L, 0x3CL, 0x3EL, 0x3FL, 0x60L, 0x7BL, 0x7DL)
  )
}

# WHATWG query serializer for special schemes: uses the special-query
# percent-encode set (`" # < > '` plus C0/non-ASCII). rurl's WHATWG profile is
# scoped to special schemes for WPT parity; non-special additions keep the
# regular query set and therefore leave apostrophe literal.
.whatwg_query_percent_encode <- function(query, scheme = NA_character_) {
  encode_set <- c(0x20L, 0x22L, 0x23L, 0x3CL, 0x3EL)
  if (!is.na(scheme) &&
      .ascii_tolower(scheme) %in% .WHATWG_SPECIAL_SCHEMES) {
    encode_set <- c(encode_set, 0x27L)
  }
  .whatwg_component_percent_encode(query, encode_set)
}

# WHATWG fragment serializer: uses the fragment percent-encode set
# (`" < > ` plus C0/non-ASCII).
.whatwg_fragment_percent_encode <- function(fragment) {
  .whatwg_component_percent_encode(
    fragment, c(0x20L, 0x22L, 0x3CL, 0x3EL, 0x60L)
  )
}

# Internal helper to parse query strings into a list.
#
# Linear in the number of pairs: the former version grew the result list with a
# per-pair `result[[key]] <- c(result[[key]], value)` plus an `%in% names()`
# membership scan, which is O(k^2) in the pair count. This splits once, decodes
# each side with a single vectorized curl_unescape(), and groups by key with
# split(), preserving first-seen key order and per-key value order.
#
# Byte-for-byte compatible with the old parser: the "=" split keeps
# strsplit(fixed = TRUE) semantics (trailing empties dropped, so "x=" and "x=="
# both yield an empty value; "a=b=c" and "a==b" re-join the tail with "="), and
# empty inter-"&" segments are skipped.
._parse_query_string <- function(query, decode = TRUE) {
  if (is.na(query) || !nzchar(query)) {
    return(list())
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(list())
  }

  kv <- strsplit(parts, "=", fixed = TRUE)
  keys <- vapply(kv, function(p) p[1L], character(1), USE.NAMES = FALSE)
  values <- vapply(
    kv,
    function(p) if (length(p) > 1L) paste(p[-1L], collapse = "=") else "",
    character(1),
    USE.NAMES = FALSE
  )

  if (decode) {
    keys <- .query_unescape(keys)
    values <- .query_unescape(values)
  }

  .group_query_pairs(keys, values)
}

# Group parallel key/value vectors into a named list, one entry per distinct
# key (first-seen order), each holding its values in occurrence order. split()
# over a factor with first-seen levels keeps each group's original order, so
# repeated keys collect their values exactly as a c() accumulation would.
# Shared by ._parse_query_string() and get_query()'s filtered list output.
.group_query_pairs <- function(keys, values) {
  first_seen <- keys[!duplicated(keys)]
  grouped <- split(values, factor(keys, levels = first_seen))
  lapply(grouped, unname)
}

# Percent-decode a character vector in one vectorized curl_unescape() call,
# falling back to the raw input if the call errors (mirrors the former
# per-element tryCatch guard).
.query_unescape <- function(x) {
  tryCatch(curl::curl_unescape(x), error = function(e) x)
}

# --- Query-filter engine (query_handling) -----------------------------------
# Ordered (key, value) substrate + the filter/canonicalization engine feeding
# clean_url and get_query(). Unlike ._parse_query_string() (grouped and
# order-lossy) this keeps original order and duplicate positions, which the
# filter and stable-sort contracts require. The raw `query` result field is
# never touched by any of this -- it stays the faithful original; filtering is a
# derived view.

# Split a raw query string into ordered (key, value) pairs: on '&' (literal),
# then each pair on the FIRST '=' (literal). Empty inter-'&' segments are
# skipped (matching ._parse_query_string). Keys/values are RAW (percent-encoded)
# here; decoding happens in the engine. A bare "a" (no '=') yields value "" --
# v1 does not distinguish "?a" from "?a=". Returns list(key=<chr>, value=<chr>).
._parse_query_pairs <- function(query) {
  empty <- list(key = character(0), value = character(0))
  if (is.na(query) || !nzchar(query)) {
    return(empty)
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(empty)
  }
  eq <- regexpr("=", parts, fixed = TRUE)
  has_eq <- eq > 0L
  key <- ifelse(has_eq, substr(parts, 1L, eq - 1L), parts)
  value <- ifelse(has_eq, substr(parts, eq + 1L, nchar(parts)), "")
  list(key = key, value = value)
}

# TRUE where a token is malformed-opaque: it contains a '%' NOT followed by two
# hex digits. Opaque tokens are used as-is for matching and emitted byte-for-
# byte on output (their '%' is never turned into '%25'). Vectorized.
.token_is_opaque <- function(x) {
  stringi::stri_detect_regex(x, "%(?![0-9A-Fa-f]{2})")
}

# Decode non-opaque tokens (opaque ones pass through unchanged). decode_plus
# applies to VALUES only: '+' -> space before percent-decoding.
.decode_query_tokens <- function(tokens, opaque, is_value, decode_plus) {
  out <- tokens
  live <- !opaque
  if (any(live)) {
    x <- tokens[live]
    if (is_value && decode_plus) {
      x <- gsub("+", " ", x, fixed = TRUE)
    }
    out[live] <- .query_unescape(x)
  }
  out
}

# Compile a set of glob patterns into a single case-aware matcher, or NULL when
# there is nothing to match. Each pattern is anchored via .glob_to_regex() and
# the anchored alternatives are OR-joined.
.compile_param_matcher <- function(patterns, case_sensitive) {
  patterns <- patterns[!is.na(patterns) & nzchar(patterns)]
  if (length(patterns) == 0L) {
    return(NULL)
  }
  regexes <- vapply(patterns, .glob_to_regex, character(1), USE.NAMES = FALSE)
  list(regex = paste(regexes, collapse = "|"), case_sensitive = case_sensitive)
}

# Apply a compiled matcher to decoded param names. NULL matcher matches nothing.
.param_matches <- function(names, matcher) {
  if (is.null(matcher) || length(names) == 0L) {
    return(logical(length(names)))
  }
  stringi::stri_detect_regex(
    names, matcher$regex,
    case_insensitive = !matcher$case_sensitive
  )
}

# Decide, per decoded pair, which survive under the given mode. Returns a
# logical over the pairs. filter precedence (highest first): params_keep rescue
# > empty-drop > params_drop/built-in denylist > keep.
._select_params <- function(dec_key, dec_val, query_handling, params_keep,
                            params_drop, params_case_sensitive,
                            empty_param_handling, denylist_source) {
  n <- length(dec_key)
  is_empty <- !nzchar(dec_val)
  drop_empty <- identical(empty_param_handling, "drop")
  keep_hit <- .param_matches(
    dec_key, .compile_param_matcher(params_keep, params_case_sensitive)
  )

  if (identical(query_handling, "keep")) {
    surv <- rep(TRUE, n)
    if (drop_empty) surv <- surv & !is_empty
    return(surv)
  }
  if (identical(query_handling, "allow")) {
    # params_keep is the inclusion criterion only, NOT an empty-rescue: empty
    # allowed params still drop under empty_param_handling = "drop".
    surv <- keep_hit
    if (drop_empty) surv <- surv & !is_empty
    return(surv)
  }
  # filter: drop built-in denylist + params_drop, minus params_keep rescues.
  drop_patterns <- c(.resolve_query_denylist(denylist_source), params_drop)
  drop_hit <- .param_matches(
    dec_key, .compile_param_matcher(drop_patterns, params_case_sensitive)
  )
  surv <- !drop_hit
  if (drop_empty) surv <- surv & !is_empty
  # params_keep rescues over BOTH the denylist and empty-drop.
  surv | keep_hit
}

# Tokenize a raw query, decode for matching, select the surviving pairs per the
# filter args, and (optionally) stably sort by decoded key. Returns the
# surviving pairs in output order -- both the raw and decoded key/value plus the
# opacity flags -- or NULL when there is no query or nothing survives. Shared by
# ._filter_query_params() (canonical render) and get_query() (decoded / list
# render) so the selection + ordering logic lives in exactly one place. Callers
# must handle query_handling = "drop" themselves (it never reaches here).
._query_surviving_pairs <- function(query,
                                    query_handling,
                                    params_keep,
                                    params_drop,
                                    params_case_sensitive,
                                    sort_params,
                                    empty_param_handling,
                                    decode_plus,
                                    denylist_source) {
  pairs <- ._parse_query_pairs(query)
  if (length(pairs$key) == 0L) {
    return(NULL)
  }

  key_opaque <- .token_is_opaque(pairs$key)
  val_opaque <- .token_is_opaque(pairs$value)
  dec_key <- .decode_query_tokens(pairs$key, key_opaque, FALSE, decode_plus)
  dec_val <- .decode_query_tokens(pairs$value, val_opaque, TRUE, decode_plus)

  idx <- which(._select_params(
    dec_key, dec_val, query_handling, params_keep, params_drop,
    params_case_sensitive, empty_param_handling, denylist_source
  ))
  if (length(idx) == 0L) {
    return(NULL)
  }
  if (sort_params) {
    # Stable sort by decoded key (radix is stable, so repeated keys keep their
    # original relative order).
    idx <- idx[order(dec_key[idx], method = "radix")]
  }

  list(
    raw_key = pairs$key[idx], raw_value = pairs$value[idx],
    dec_key = dec_key[idx], dec_value = dec_val[idx],
    key_opaque = key_opaque[idx], val_opaque = val_opaque[idx]
  )
}

# Filter + canonicalize one raw query string's pairs per query_handling and
# return a canonical (re-encoded) query WITHOUT the leading '?', or "" when
# nothing survives. Scalar (one query in, one query out); the vector engine
# maps it over the queries of the unique URLs. The canonical form always
# re-encodes (uppercase hex, %20 for space, %26/%3D for embedded delimiters,
# empty value -> "key="); opaque tokens pass through byte-for-byte. This is NOT
# the faithful original -- that lives untouched on the `query` result field.
._filter_query_params <- function(query,
                                  query_handling = "keep",
                                  params_keep = NULL,
                                  params_drop = NULL,
                                  params_case_sensitive = FALSE,
                                  sort_params = FALSE,
                                  empty_param_handling = "keep",
                                  decode_plus = FALSE,
                                  denylist_source = "builtin") {
  if (identical(query_handling, "drop")) {
    return("")
  }
  sp <- ._query_surviving_pairs(
    query, query_handling, params_keep, params_drop, params_case_sensitive,
    sort_params, empty_param_handling, decode_plus, denylist_source
  )
  if (is.null(sp)) {
    return("")
  }

  # Re-encode key and value separately. Opaque tokens emit their raw bytes; the
  # curl_escape() of an opaque token is computed but discarded by ifelse().
  enc_key <- ifelse(sp$key_opaque, sp$raw_key, curl::curl_escape(sp$dec_key))
  enc_val <- ifelse(
    sp$val_opaque, sp$raw_value, curl::curl_escape(sp$dec_value)
  )
  paste(paste0(enc_key, "=", enc_val), collapse = "&")
}
