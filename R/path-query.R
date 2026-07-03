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
