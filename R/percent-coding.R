# --- In-tree percent-coding primitives ---------------------------------------
#
# `.pct_escape()` / `.pct_unescape()` are the in-tree replacements for
# `curl::curl_escape()` / `curl::curl_unescape()` (RURL-robgajml). They are
# deliberately BYTE-EXACT with libcurl's `curl_easy_escape()` /
# `curl_easy_unescape()` on every reachable input, so swapping them in is a
# no-op on output -- the point of this seam is to shrink the libcurl surface,
# not to change what rurl emits. Two documented deviations, both of them
# strictly local improvements, are listed on each function.
#
# They are NOT the WHATWG component serializers. `.whatwg_component_percent_
# encode()` (R/path-query.R) preserves existing percent spellings and encodes a
# per-component set; these two are the plain RFC 3986 unreserved-set escape and
# the permissive decode that rurl's non-WHATWG presentation paths have always
# used. Do not merge them.

# Byte -> output-chunk lookup, one entry per octet value (index = byte + 1).
# Unreserved octets (RFC 3986 section 2.3: ALPHA / DIGIT / "-" / "." / "_" /
# "~") map to themselves; every other octet maps to its uppercase triplet.
# libcurl's escape set is exactly this -- it is NOT locale-dependent, unlike a
# naive `isalnum()`.
.pct_escape_table <- local({
  out <- sprintf("%%%02X", 0:255)
  keep <- c(0x30:0x39, 0x41:0x5A, 0x61:0x7A, 0x2DL, 0x2EL, 0x5FL, 0x7EL)
  out[keep + 1L] <- vapply(
    keep, function(b) rawToChar(as.raw(b)), character(1), USE.NAMES = FALSE
  )
  out
})

# Render one string as the UTF-8 octets the percent-coding is defined over.
#
# libcurl's R wrappers run `enc2utf8()` first. That is the locale sensitivity
# this codebase has been bitten by before (see
# `.whatwg_component_percent_encode()`): a string marked "unknown" carries
# native bytes, and under a non-UTF-8 locale `enc2utf8()` RE-DECODES already-
# UTF-8 octets into mojibake. Declaring instead (`Encoding<-`, bytes untouched)
# reads the exact octets in every locale, and is identical to `enc2utf8()` in a
# UTF-8 session.
#
# The declaration is UNCONDITIONAL, including for a latin1-MARKED input -- the
# one case where libcurl's transcode would differ. `enc2utf8()` is barred from
# the whole namespace by the locale-invariance meta-guard
# (tests/testthat/test-locale-invariance.R), and rurl declares UTF-8 on the way
# in and out (`.mark_host_utf8()` / `.mark_result_utf8()`), so a latin1-marked
# string does not reach this seam.
.pct_as_utf8 <- function(s) {
  Encoding(s) <- "UTF-8"
  s
}

.pct_escape_one <- function(s) {
  if (is.na(s)) {
    return(NA_character_)
  }
  if (!nzchar(s)) {
    return("")
  }
  bytes <- as.integer(charToRaw(.pct_as_utf8(s)))
  paste(.pct_escape_table[bytes + 1L], collapse = "")
}

# Percent-encode every octet outside the RFC 3986 unreserved set, uppercase hex.
# Vectorized; `character(0)` in, `character(0)` out. Output is ASCII by
# construction, so it carries no encoding mark.
#
# Deviation from `curl::curl_escape()`: NA propagates as NA instead of being
# stringified to the literal `"NA"` (which is what `as.character()` on the C
# side yields). Every call site guards or masks NA before reaching here, so the
# deviation is unreachable today; propagating is the behavior a caller that
# stops guarding would want.
.pct_escape <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L) {
    return(character(0))
  }
  out <- x
  ok <- !is.na(x)
  # A string made only of unreserved octets escapes to itself; skipping the
  # byte walk for it is what keeps the common path (ordinary path segments,
  # ordinary query keys) cheap.
  todo <- ok & grepl("[^A-Za-z0-9._~-]", x, useBytes = TRUE)
  if (any(todo)) {
    out[todo] <- vapply(
      x[todo], .pct_escape_one, character(1), USE.NAMES = FALSE
    )
  }
  out
}

.pct_unescape_one <- function(s) {
  if (is.na(s)) {
    return(NA_character_)
  }
  s <- .pct_as_utf8(s)
  # `useBytes` on both scans: a declared-UTF-8 string can hold invalid octets
  # (a `%FF` decode, or invalid input passed straight through), and a validity-
  # checking scan would warn on it where libcurl silently does not.
  if (!grepl("%", s, fixed = TRUE, useBytes = TRUE)) {
    return(s)
  }
  raw_in <- charToRaw(s)
  n <- length(raw_in)
  # Byte offsets, not character offsets: a literal run between two triplets can
  # hold multibyte UTF-8, and it is sliced out of `raw_in` by position.
  starts <- as.integer(
    gregexpr("%[0-9A-Fa-f]{2}", s, perl = TRUE, useBytes = TRUE)[[1]]
  )
  if (starts[1L] == -1L) {
    return(s)
  }

  chunks <- vector("list", 2L * length(starts) + 1L)
  k <- 0L
  pos <- 1L
  truncated <- FALSE
  for (st in starts) {
    if (st > pos) {
      k <- k + 1L
      chunks[[k]] <- raw_in[pos:(st - 1L)]
    }
    value <- strtoi(rawToChar(raw_in[(st + 1L):(st + 2L)]), base = 16L)
    if (value == 0L) {
      # libcurl hands back a NUL-terminated C string, so a decoded %00 ends the
      # result and discards the remainder. Preserved deliberately: R strings
      # cannot carry an embedded NUL either, and the alternative (dropping the
      # NUL but keeping the tail) would silently smuggle the tail past a caller
      # that today never sees it.
      truncated <- TRUE
      break
    }
    k <- k + 1L
    chunks[[k]] <- as.raw(value)
    pos <- st + 3L
  }
  if (!truncated && pos <= n) {
    k <- k + 1L
    chunks[[k]] <- raw_in[pos:n]
  }

  if (k == 0L) {
    return("")
  }
  out <- rawToChar(unlist(chunks[seq_len(k)], use.names = FALSE))
  # libcurl's wrapper marks its result UTF-8 unconditionally; R ignores the mark
  # on a pure-ASCII string, so this matches for both.
  Encoding(out) <- "UTF-8"
  out
}

# Percent-decode every `%XX` triplet, leaving a malformed `%` (not followed by
# two hex digits) byte-for-byte alone. Lowercase hex decodes. `+` is NOT treated
# as a space -- callers that want form decoding do that themselves. Vectorized.
#
# Deviation from `curl::curl_unescape()`: NA propagates as NA rather than
# decoding the literal `"NA"`. As with `.pct_escape()`, no live call site can
# reach it.
.pct_unescape <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L) {
    return(character(0))
  }
  out <- x
  ok <- !is.na(x)
  todo <- ok & grepl("%", x, fixed = TRUE, useBytes = TRUE)
  # Nothing to decode: the value is the input, but the UTF-8 declaration still
  # has to happen (libcurl's wrapper marks unconditionally). One vectorized
  # assignment is what keeps the no-triplet path cheap.
  pass <- ok & !todo
  if (any(pass)) {
    Encoding(out[pass]) <- "UTF-8"
  }
  if (any(todo)) {
    out[todo] <- vapply(
      x[todo], .pct_unescape_one, character(1), USE.NAMES = FALSE
    )
  }
  out
}
