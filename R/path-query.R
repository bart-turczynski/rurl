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
  encoded_segments <- vapply(
    segments,
    function(seg) curl::curl_escape(seg),
    character(1)
  )
  recomposed <- paste(encoded_segments, collapse = "/")
  if (has_leading && !stringi::stri_startswith_fixed(recomposed, "/")) {
    recomposed <- paste0("/", recomposed)
  }
  if (has_trailing && !stringi::stri_endswith_fixed(recomposed, "/")) {
    recomposed <- paste0(recomposed, "/")
  }
  recomposed
}

# Internal helper to parse query strings into a list
._parse_query_string <- function(query, decode = TRUE) {
  if (is.na(query) || !nzchar(query)) {
    return(list())
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1]]
  result <- list()

  for (part in parts) {
    if (!nzchar(part)) next
    kv <- strsplit(part, "=", fixed = TRUE)[[1]]
    key <- kv[1]
    value <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else ""

    if (decode) {
      key <- tryCatch(curl::curl_unescape(key), error = function(e) key)
      value <- tryCatch(curl::curl_unescape(value), error = function(e) value)
    }

    if (key %in% names(result)) {
      result[[key]] <- c(result[[key]], value)
    } else {
      result[[key]] <- value
    }
  }

  result
}
