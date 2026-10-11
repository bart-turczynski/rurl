# Low-level path normalization and query-string parsing helpers.

# Internal helper to collapse duplicate slashes in paths
._collapse_path_slashes <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  gsub("/+", "/", path, perl = TRUE)
}

# Internal helper to remove dot segments per RFC 3986 section 5.2.4. The
# prefix tests and cuts are base R: stri_startswith_fixed() and stri_sub() read
# past a U+FEFF that starts their input (see .starts_with_bom()), so a first
# segment such as `\ufeff..` or `\ufeff` followed by `/./` was read as a dot
# segment and the mark was dropped (RURL-xrjtdnko).
._remove_dot_segments <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }

  input <- path
  output <- ""

  while (nzchar(input)) {
    if (startsWith(input, "../")) {
      input <- substring(input, 4L, .Machine$integer.max)
    } else if (startsWith(input, "./")) {
      input <- substring(input, 3L, .Machine$integer.max)
    } else if (startsWith(input, "/./")) {
      input <- paste0("/", substring(input, 4L, .Machine$integer.max))
    } else if (identical(input, "/.")) {
      input <- "/"
    } else if (startsWith(input, "/../")) {
      input <- paste0("/", substring(input, 5L, .Machine$integer.max))
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
      input <- substring(
        input, attr(match, "match.length") + 1, .Machine$integer.max
      )
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
  codes <- strtoi(substring(matches, 2L, .Machine$integer.max), base = 16L)
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
#
# The cuts are base R `substring()`, in the unit `regexpr()` measured the match
# in, like `._remove_dot_segments()`, with an explicit `last`: before R 4.6.0
# `substring()` defaulted to `last = 1000000L` and cut a longer path short.
# Each cut starts on the `.`, `%` or `/` an anchored pattern matched, so
# `stri_sub()`, which reads past a U+FEFF that starts its input (see
# .starts_with_bom()), gave the same answer here; the cut is base R so that a
# later pattern cannot make it read past one (RURL-zxnxczsw).
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
      input <- substring(input, len + 1L, .Machine$integer.max)
      next
    }
    len <- match_len(paste0("^", dot, "/"), input)
    if (!is.na(len)) {
      input <- substring(input, len + 1L, .Machine$integer.max)
      next
    }
    len <- match_len(paste0("^/", dot, "/"), input)
    if (!is.na(len)) {
      input <- paste0("/", substring(input, len + 1L, .Machine$integer.max))
      next
    }
    len <- match_len(paste0("^/", dot, "$"), input)
    if (!is.na(len)) {
      input <- "/"
      next
    }
    len <- match_len(paste0("^/", dotdot, "/"), input)
    if (!is.na(len)) {
      input <- paste0("/", substring(input, len + 1L, .Machine$integer.max))
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
    input <- substring(
      input, attr(match, "match.length") + 1, .Machine$integer.max
    )
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

# Internal helper to strip index/default pages from the end of a path.
# stri_match_first_regex() and stri_startswith_fixed() read past a U+FEFF that
# starts their input (see .starts_with_bom()), so the directory kept from
# `\ufeff/a/index.html` was `/a`, not `/\ufeff/a` (RURL-zxnxczsw). The match
# runs on the path behind a spliced "x", which the pattern's `^(.*)` takes
# into the kept directory and the byte cut removes; the regex itself, and so
# which paths match, is unchanged. The root test is base R.
._strip_index_page <- function(path) {
  if (is.na(path) || !nzchar(path)) {
    return(path)
  }
  match <- stringi::stri_match_first_regex(
    paste0("x", path),
    "(?i)^(.*)/(index|default)\\.[^/]+/?$"
  )
  if (is.na(match[1, 1])) {
    return(path)
  }
  base <- .byte_substring(match[1, 2], 2L)
  if (is.na(base) || base == "") {
    return("/")
  }
  if (!startsWith(base, "/")) {
    base <- paste0("/", base)
  }
  # Dropping the terminal page can EXPOSE a dot segment the page name was
  # hiding -- `/a/../index.html` would otherwise emit `/a/..`, and
  # `/../index.html` a path that escapes above the document root. Resolve the
  # base when, and only when, the strip left a dot segment in final position:
  # dot segments that were already visible beforehand are the caller's
  # `path_normalization` choice to make, and "none" has to keep meaning none.
  # Under a selector this branch is unreachable -- both standard profiles set
  # `path_normalization = "dot_segments"`, which runs first (processing-order
  # step 2) and leaves no dot segment for the strip to expose. RURL-ogruzocw.
  if (stringi::stri_detect_regex(base, "(^|/)\\.{1,2}$")) {
    base <- ._remove_dot_segments(base)
    if (!nzchar(base)) {
      return("/")
    }
  }
  base
}

# Internal helper to percent-encode path segments. The slash tests are base R:
# stri_startswith_fixed() reads past a U+FEFF that starts its input (see
# .starts_with_bom()), so the rootless `\ufeff/a` read as rooted and came back
# as `/%EF%BB%BF/a` (RURL-zxnxczsw).
._encode_path_segments <- function(path) {
  if (is.na(path)) {
    return(path)
  }
  has_leading <- startsWith(path, "/")
  has_trailing <- endsWith(path, "/")
  segments <- strsplit(path, "/", fixed = TRUE)[[1]]
  # .pct_escape() is vectorized: one call escapes every segment (and returns
  # character(0) for a character(0) input), so paste() recomposes identically
  # to the former per-segment vapply().
  encoded_segments <- .pct_escape(segments)
  recomposed <- paste(encoded_segments, collapse = "/")
  if (has_leading && !startsWith(recomposed, "/")) {
    recomposed <- paste0("/", recomposed)
  }
  if (has_trailing && !endsWith(recomposed, "/")) {
    recomposed <- paste0(recomposed, "/")
  }
  recomposed
}

# WHATWG component serializer core: encode C0 controls, non-ASCII UTF-8 bytes,
# and the component-specific encode set, while preserving every existing
# percent sign spelling so `%2e`, `%2E`, `%`, and `%2z` do not get decoded,
# hex-normalized, or double-encoded.
#
# ONE TOKEN PER OCTET. The output is a pure function of each input octet on
# its own: C0 controls (0x00-0x1F), DEL and every octet >= 0x80 become an
# uppercase `%XX` triplet, so do the members of `encode_set`, and every other
# printable ASCII octet passes through. `%` and the hex digits are printable
# ASCII and no WHATWG encode set names them, so an existing triplet (`%2e`) and
# a malformed one (`%`, `%2z`) both pass through verbatim -- which is what the
# former per-byte loop's special `%XX` re-emit branch did, and why that branch
# never changed a byte. `.whatwg_encode_table()` refuses a set that would break
# this (one naming `%` or a hex digit), because the re-emit rule would then
# disagree with a plain lookup.
#
# That makes the encoder a table lookup, and the lookup vectorizes: one
# `grepl()` finds the rows holding an octet that changes, and those rows go
# through ONE `paste()` / `charToRaw()` / table index / `rawToChar()` /
# `substring()` pass together, instead of an R-level loop per octet per row
# (RURL-khcicsca: about 1 us per byte before). Rows with nothing to change, NA
# and "" come back as they went in.
#
# The byte extraction is the INPUT-side counterpart of the host chokepoint's
# `.mark_host_utf8()`. This used to read `charToRaw(enc2utf8(x))`, and that
# `enc2utf8()` was the locale sensitivity: a path/query/fragment carrying
# non-ASCII arrives here marked "unknown" (native bytes -- the host chokepoint
# covers the host only), so under `LC_ALL=C` `enc2utf8()` RE-DECODES those UTF-8
# bytes as single native characters and `/école` serialized as
# `/%3Cc3%3E%3Ca9%3Ecole` instead of `/%C3%A9cole`. `.mark_host_utf8()` DECLARES
# instead (`Encoding<-`, bytes untouched) and `charToRaw()` then reads the exact
# UTF-8 octets WHATWG's percent-encode set is defined over, in every locale. In
# a UTF-8 session this is byte-for-byte the old behavior (`enc2utf8()` on
# already-UTF-8 bytes is the identity).
#
# The declaration is also what makes the batched `paste()` safe: it gives
# every non-ASCII row the SAME mark (ASCII rows carry none), so `paste()` has
# nothing to translate and concatenates the octets as they are. With mixed
# marks it would transcode a latin1-marked row to UTF-8 first and change its
# octets. The output is ASCII by construction, so it carries no mark.
#
# Vectorized over `x`; the scalar call `(x, integer(0))` the opaque-host parser
# makes (R/parse-state.R) is the length-one case. `batch_bytes` exists so a
# test can force several batches; callers leave it alone.
.whatwg_component_percent_encode <- function(x, encode_set,
                                             batch_bytes = 4194304) {
  out <- x
  todo <- which(!is.na(x) & grepl(
    .whatwg_encode_pattern(encode_set), x, perl = TRUE, useBytes = TRUE
  ))
  if (length(todo) == 0L) {
    return(out)
  }
  tab <- .whatwg_encode_table(encode_set)
  s <- .mark_host_utf8(x[todo])
  n_bytes <- nchar(s, type = "bytes")
  # Batches of about `batch_bytes` input octets bound the transient index
  # vectors (two integers per output octet). Ordinary input is one batch.
  batch <- cumsum(as.numeric(n_bytes)) %/% batch_bytes
  for (b in unique(batch)) {
    rows <- which(batch == b)
    out[todo[rows]] <- .whatwg_encode_batch(s[rows], n_bytes[rows], tab)
  }
  out
}

# Encode a batch of non-empty, same-mark rows: one octet pass, then cut the
# concatenated ASCII output back into rows. Octet `b` emits the first
# `tab$width[b + 1]` octets of column `b + 1` of `tab$octets`, so a row's
# output length is its octet count plus two per octet that became a triplet.
.whatwg_encode_batch <- function(s, n_bytes, tab) {
  bytes <- as.integer(charToRaw(paste(s, collapse = "")))
  width <- tab$width[bytes + 1L]
  octets <- tab$octets[rep.int(bytes * 3L, width) + sequence(width)]
  out_end <- cumsum(width)[cumsum(n_bytes)]
  out_start <- c(1L, out_end[-length(out_end)] + 1L)
  substring(rawToChar(octets), out_start, out_end)
}

# Encode tables and prefilter patterns, built once per distinct encode set.
.whatwg_encode_cache <- new.env(parent = emptyenv())

.whatwg_encode_key <- function(encode_set) {
  paste(sort(unique(as.integer(encode_set))), collapse = ",")
}

# The octet -> output table for one encode set: `octets` is a 3 x 256 raw
# matrix whose column `b + 1` holds octet `b`'s output -- `%` and two uppercase
# hex digits, or the octet itself in row 1 -- and `width` says how many of
# those three rows it uses (3, or 1 for a pass-through octet).
.whatwg_encode_table <- function(encode_set) {
  key <- paste0("tab:", .whatwg_encode_key(encode_set))
  tab <- .whatwg_encode_cache[[key]]
  if (!is.null(tab)) {
    return(tab)
  }
  passthrough <- c(0x25L, 0x30:0x39, 0x41:0x46, 0x61:0x66)
  if (any(encode_set %in% passthrough)) {
    stop(
      "internal error: a WHATWG encode set may not name `%` or a hex digit; ",
      "the table lookup relies on both passing through.",
      call. = FALSE
    )
  }
  hex <- charToRaw("0123456789ABCDEF")
  octets <- rbind(
    rep(charToRaw("%"), 256L), hex[0:255 %/% 16L + 1L], hex[0:255 %% 16L + 1L]
  )
  keep <- setdiff(0x20:0x7e, encode_set)
  octets[1L, keep + 1L] <- as.raw(keep)
  width <- rep(3L, 256L)
  width[keep + 1L] <- 1L
  tab <- list(octets = octets, width = width)
  assign(key, tab, envir = .whatwg_encode_cache)
  tab
}

# PCRE byte-mode class matching any octet the table changes: C0, DEL, every
# octet >= 0x80, and the encode set.
.whatwg_encode_pattern <- function(encode_set) {
  key <- paste0("pat:", .whatwg_encode_key(encode_set))
  pat <- .whatwg_encode_cache[[key]]
  if (!is.null(pat)) {
    return(pat)
  }
  pat <- paste0(
    "[\\x00-\\x1f\\x7f-\\xff",
    paste(sprintf("\\x%02x", unique(as.integer(encode_set))), collapse = ""),
    "]"
  )
  assign(key, pat, envir = .whatwg_encode_cache)
  pat
}

# WHATWG percent-encode sets, as octet values beyond the C0-control set that
# every one of them shares. The query set is SP `"` `#` `<` `>`; the
# special-query set adds `'`; the path set adds `?` `^` `` ` `` `{` `}` to the
# query set; the userinfo set adds `/` `:` `;` `=` `@` `[` `\` `]` `|` to the
# path set; the fragment set is SP `"` `<` `>` `` ` ``.
.WHATWG_QUERY_ENCODE_SET <- c(0x20L, 0x22L, 0x23L, 0x3CL, 0x3EL)
.WHATWG_SPECIAL_QUERY_ENCODE_SET <- c(.WHATWG_QUERY_ENCODE_SET, 0x27L)
.WHATWG_PATH_ENCODE_SET <- c(
  .WHATWG_QUERY_ENCODE_SET, 0x3FL, 0x5EL, 0x60L, 0x7BL, 0x7DL
)
.WHATWG_USERINFO_ENCODE_SET <- c(
  .WHATWG_PATH_ENCODE_SET,
  0x2FL, 0x3AL, 0x3BL, 0x3DL, 0x40L, 0x5BL, 0x5CL, 0x5DL, 0x7CL
)
.WHATWG_FRAGMENT_ENCODE_SET <- c(0x20L, 0x22L, 0x3CL, 0x3EL, 0x60L)

# WHATWG path serializer for `url_standard = "whatwg"` +
# `path_encoding = "encode"`: uses the path percent-encode set -- the query set
# (SP `"` `#` `<` `>`) plus `?` `^` `` ` `` `{` `}`. `^` (U+005E) is a member:
# the set is the query set and "U+003F (?), U+005E (^), U+0060 (`), U+007B ({),
# and U+007D (})", which WPT pins (`foo://host/...^...` -> `%5E`). It is NOT a
# member of the C0-control set an OPAQUE path uses, so `^` stays literal there
# -- see `.whatwg_opaque_path_encode()`. Vectorized.
.whatwg_path_percent_encode <- function(path) {
  .whatwg_component_percent_encode(path, .WHATWG_PATH_ENCODE_SET)
}

# WHATWG opaque-path serializer (#opaque-path-state). An opaque path is
# percent-encoded with the C0-control set only -- C0 controls, DEL and every
# non-ASCII byte -- so the printable ASCII a list path escapes (`^` `{` `}` ...)
# stays literal.
#
# `delimiter_follows` carries the one context the encode set cannot: WHATWG
# encodes a SPACE that sits immediately before the `?` or `#` that ends an
# opaque path, and leaves every other space literal. WPT pins all three cases:
# `non-special:opaque  x?hi` keeps both interior spaces,
# `non-special:opaque  ?hi` yields `opaque %20` (only the LAST space encoded),
# and a space at the very end of the input
# is removed by the parser's leading/trailing strip instead. The reason is
# round-tripping: a trailing space would otherwise be stripped when the
# serialized URL is re-parsed.
#
# Vectorized over `path`; `delimiter_follows` recycles, and only TRUE counts.
# The C0-control set leaves a space literal and the encoder maps octets one to
# one, so the encoded row still ends in that space; rewriting the final output
# character is the same as encoding all but the last octet and appending `%20`.
.whatwg_opaque_path_encode <- function(path, delimiter_follows) {
  out <- .whatwg_component_percent_encode(path, integer(0))
  delimiter_follows <- rep_len(as.logical(delimiter_follows), length(path))
  trailing <- which(
    !is.na(out) & delimiter_follows %in% TRUE & endsWith(out, " ")
  )
  if (length(trailing) > 0L) {
    out[trailing] <- paste0(
      substr(out[trailing], 1L, nchar(out[trailing]) - 1L), "%20"
    )
  }
  out
}

# WHATWG query serializer: the special-query percent-encode set (`" # < > '`
# plus C0/non-ASCII) under a special scheme, the query set otherwise -- a
# non-special scheme leaves the apostrophe literal. Vectorized over `query`;
# `scheme` recycles, NA reads as non-special.
.whatwg_query_percent_encode <- function(query, scheme = NA_character_) {
  scheme <- rep_len(as.character(scheme), length(query))
  special <- !is.na(scheme) &
    .ascii_tolower(scheme) %in% .WHATWG_SPECIAL_SCHEMES
  out <- query
  if (any(special)) {
    out[special] <- .whatwg_component_percent_encode(
      query[special], .WHATWG_SPECIAL_QUERY_ENCODE_SET
    )
  }
  if (!all(special)) {
    out[!special] <- .whatwg_component_percent_encode(
      query[!special], .WHATWG_QUERY_ENCODE_SET
    )
  }
  out
}

# WHATWG userinfo serializer (#userinfo-percent-encode-set): the path
# percent-encode set (SP `"` `#` `<` `>` `?` `^` `` ` `` `{` `}`) plus `/` `:`
# `;` `=` `@` `[` `\` `]` `|`. WHATWG's host parser fills the username and
# password buffers by percent-encoding each code point with this set, so the
# PARSED credential values it stores are already escaped -- unlike rurl's
# `raw_user` / `raw_password`, which keep the source spelling.
#
# Reusing `.whatwg_component_percent_encode()` is what makes this idempotent:
# `%` and the hex digits pass through, so an existing percent triplet is
# re-emitted verbatim -- `u%40ser` stays `u%40ser` and `%25DOMAIN` stays
# `%25DOMAIN` rather than becoming `%2525DOMAIN`. It also encodes every C0
# control, DEL and non-ASCII byte, so a non-ASCII userinfo gains escapes too
# (WHATWG-correct). Vectorized.
.whatwg_userinfo_percent_encode <- function(x) {
  .whatwg_component_percent_encode(x, .WHATWG_USERINFO_ENCODE_SET)
}

# WHATWG fragment serializer: uses the fragment percent-encode set
# (`" < > ` plus C0/non-ASCII). Vectorized.
.whatwg_fragment_percent_encode <- function(fragment) {
  .whatwg_component_percent_encode(fragment, .WHATWG_FRAGMENT_ENCODE_SET)
}

# Internal helper to parse query strings into a list.
#
# Linear in the number of pairs: the former version grew the result list with a
# per-pair `result[[key]] <- c(result[[key]], value)` plus an `%in% names()`
# membership scan, which is O(k^2) in the pair count. This splits once, decodes
# each side with a single vectorized .pct_unescape(), and groups by key with
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

# Percent-decode a character vector in one vectorized .pct_unescape() call,
# falling back to the raw input if the call errors (mirrors the former
# per-element tryCatch guard).
.query_unescape <- function(x) {
  tryCatch(.pct_unescape(x), error = function(e) x)
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
  # .pct_escape() of an opaque token is computed but discarded by ifelse().
  enc_key <- ifelse(sp$key_opaque, sp$raw_key, .pct_escape(sp$dec_key))
  enc_val <- ifelse(
    sp$val_opaque, sp$raw_value, .pct_escape(sp$dec_value)
  )
  paste(paste0(enc_key, "=", enc_val), collapse = "&")
}
