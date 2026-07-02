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

  # Group values by decoded key, preserving first-seen key order. split()
  # keeps each group's original value order, so repeated keys collect their
  # values in occurrence order exactly as the former c() accumulation did.
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
