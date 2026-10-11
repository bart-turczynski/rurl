# Base-R string layer in byte mode (RURL-lbccaixb, ADR 0019).
#
# WHY. `stringi` reads a U+FEFF that starts its UTF-8 input as a byte-order
# mark and drops it (stringi's `String8::initialize`, which cannot be turned
# off: gagolews/stringi#516). In a URL that code point is data (ADR 0005,
# amendment), so ADR 0019 moves string operations to base R. With
# `perl = TRUE, useBytes = TRUE` on input declared UTF-8, base R treats the
# mark as data, gives each invalid-UTF-8 row its own result rather than
# throwing for the whole vector, and indexes in bytes, so a position from
# `.bytes_locate()` or `.bytes_capture()` is the unit `.bytes_cut()` cuts in.
#
# WHAT IT IS FOR: ASCII-structural matching, cutting and splitting. These are
# patterns whose delimiters and anchors are ASCII, run over strings that may
# hold any bytes. UTF-8 is self-synchronizing, so an ASCII byte never occurs
# inside a multibyte sequence and an ASCII pattern matches the same text in
# byte mode as in character mode. Where Unicode semantics are the point (NFC,
# case mapping, a class of non-ASCII code points), a caller uses UTF-8 PCRE
# mode or `stringi`, as ADR 0019 says. That is not this file.
#
# BYTE MODE IS LOCALE-INVARIANT ONLY FOR SOME PATTERNS. In byte mode R builds
# PCRE's character tables from the session locale. Measured on R 4.6.0 /
# PCRE2 10.47: under en_US.UTF-8, `\s` matches the byte 0xA0, `\w` matches 65
# high bytes, `[[:alpha:]]` matches 0xE9, and `(?i)[\xc0-\xc5]` also matches
# 0xE0-0xE5. Under LC_ALL=C none of them do. `.bytes_check_pattern()`
# therefore refuses those constructs, so a pattern this layer runs gives the
# same answer in every locale.
#
# OUTPUT ENCODING. Byte-mode results come back "unknown" (or "bytes"). Every
# character result here is declared UTF-8 again: these helpers move bytes and
# do not reinterpret them. ASCII results stay "unknown", as R always marks
# ASCII. A declared-UTF-8 input that holds invalid octets comes back declared
# UTF-8 holding the same octets, as it went in.
#
# INPUT. The input boundary (RURL-cwzickqy) declares every entry point's input
# UTF-8. Here "unknown" input counts as UTF-8 bytes and "UTF-8" input is taken
# as declared. Input marked "latin1" or "bytes" is a programming error and
# stops: re-declaring those bytes as UTF-8 would change the text. The same
# rule covers the pattern, the replacement and the separator, whose bytes
# reach the result or decide the match just as the subject's do.
#
# NA in, NA out, per row. A zero-length input gives a zero-length result.

#' Declare a byte-mode result UTF-8
#'
#' Moves no bytes. ASCII elements stay "unknown", as R never marks ASCII.
#' @param x A character vector.
#' @return `x`, declared UTF-8.
#' @noRd
.bytes_utf8 <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}

#' Check a subject vector, pattern, replacement or separator for the byte layer
#'
#' Stops on a non-character vector, or on elements marked "latin1" or
#' "bytes". See INPUT in the file header.
#' @param x The vector.
#' @param what The argument's name, for the error message.
#' @return `TRUE`, invisibly.
#' @noRd
.bytes_check_input <- function(x, what = "input") {
  if (!is.character(x)) {
    stop("internal: the byte string layer takes a character vector",
         call. = FALSE)
  }
  enc <- Encoding(x)
  if (any(enc == "latin1") || any(enc == "bytes")) {
    stop(sprintf(paste(
      "internal: the byte string layer takes UTF-8 or unknown-marked %s,",
      "not latin1 or bytes; declare it UTF-8 at the boundary"
    ), what), call. = FALSE)
  }
  invisible(TRUE)
}

# Escapes whose meaning in byte mode comes from the locale's character tables
# (`\w \s \d \b` and their negations), or that match a single high byte which
# may be a UTF-8 continuation byte (`\h \v \R`: 0xA0 and 0x85), or that read a
# byte as a Latin-1 code point (`\p \P \X`), or that switch units (`\C`).
.BYTES_REFUSED_ESCAPE_CODES <- utf8ToInt("wWsSdDbBhHvVRpPXC")

# The byte after each unescaped backslash in `pattern`, as integer codes. Read
# from the raw bytes, so a pattern holding a high byte is never decoded.
.bytes_escape_codes <- function(pattern) {
  b <- as.integer(charToRaw(pattern))
  out <- integer(0)
  i <- 1L
  while (i < length(b)) {
    if (b[i] == 92L) {
      out <- c(out, b[i + 1L])
      i <- i + 2L
    } else {
      i <- i + 1L
    }
  }
  out
}

# Whether `pattern` holds a `$` that is neither escaped nor inside a
# character class. Read from the raw bytes, like `.bytes_escape_codes()`. A
# `]` right after `[` or `[^` is a literal member, as PCRE reads it.
.bytes_has_bare_dollar <- function(pattern) {
  b <- as.integer(charToRaw(pattern))
  n <- length(b)
  in_class <- FALSE
  i <- 1L
  while (i <= n) {
    if (b[i] == 92L) {
      i <- i + 2L
      next
    }
    if (in_class) {
      if (b[i] == 93L) {
        in_class <- FALSE
      }
    } else if (b[i] == 36L) {
      return(TRUE)
    } else if (b[i] == 91L) {
      in_class <- TRUE
      if (i < n && b[i + 1L] == 94L) {
        i <- i + 1L
      }
      if (i < n && b[i + 1L] == 93L) {
        i <- i + 1L
      }
    }
    i <- i + 1L
  }
  FALSE
}

#' Check a pattern for the byte layer
#'
#' A pattern is one non-empty string, not marked "latin1" or "bytes". A regex
#' must not use a construct whose byte-mode meaning depends on the locale
#' (see the file header): the escapes in `.BYTES_REFUSED_ESCAPE_CODES`, a
#' POSIX `[:name:]` class or `[[:<:]]` / `[[:>:]]` word boundary, or a
#' leading `(*...)` option verb. Under `(?i)` it must not name a byte at or
#' above 0x80, as a literal, a `\x` or `\o` escape or an octal escape, and
#' must not use a back-reference in any form (`\1`, `\g`, `\k`, `(?P=name)`,
#' `(?P>name)`), because the locale's tables fold those bytes and a
#' back-reference matches whatever bytes its group took. A bare `$` is
#' refused: the end anchor is written `\z` (ADR 0019); `\$` and `[$]` match a
#' dollar sign. `[\s\S]` is written `(?s:.)`.
#' @param pattern The pattern.
#' @param fixed Whether `pattern` is a literal.
#' @param what The argument's name, for the error message.
#' @return `TRUE`, invisibly.
#' @noRd
.bytes_check_pattern <- function(pattern, fixed, what = "pattern") {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern) ||
        !nzchar(pattern)) {
    stop(sprintf("internal: `%s` must be one non-empty string", what),
         call. = FALSE)
  }
  .bytes_check_input(pattern, sprintf("`%s`", what))
  if (fixed) {
    return(invisible(TRUE))
  }
  refuse <- function(why) {
    stop(sprintf("internal: byte-mode pattern %s %s (ADR 0019)",
                 encodeString(pattern, quote = "\""), why), call. = FALSE)
  }
  esc <- .bytes_escape_codes(pattern)
  bad <- esc[esc %in% .BYTES_REFUSED_ESCAPE_CODES]
  if (length(bad)) {
    refuse(sprintf(paste(
      "uses \\%s, whose byte-mode meaning depends on the locale or splits a",
      "UTF-8 sequence; spell the ASCII class out"
    ), intToUtf8(bad[1L])))
  }
  if (grepl("\\[:\\^?([A-Za-z]+|<|>):\\]", pattern, perl = TRUE,
            useBytes = TRUE)) {
    refuse(paste(
      "uses a POSIX class or word boundary, which reads the locale;",
      "spell it out"
    ))
  }
  if (grepl("^\\(\\*[A-Z_]", pattern, perl = TRUE, useBytes = TRUE)) {
    refuse("starts with a (*...) option verb, which changes the matching mode")
  }
  if (.bytes_has_bare_dollar(pattern)) {
    refuse(paste(
      "uses a bare $, which also matches before a final newline; write the",
      "end anchor \\z, and a dollar sign \\$ or [$]"
    ))
  }
  if (grepl("\\(\\?\\^?[A-Za-z]*i", pattern, perl = TRUE, useBytes = TRUE)) {
    high <- grepl("[^\\x01-\\x7f]", pattern, perl = TRUE, useBytes = TRUE)
    hex <- regmatches(pattern, gregexpr(
      "\\\\x(\\{[0-9A-Fa-f]+\\}|[0-9A-Fa-f]{1,2})", pattern,
      perl = TRUE, useBytes = TRUE
    ))[[1L]]
    hex <- gsub("[^0-9A-Fa-f]", "", substring(hex, 3L, nchar(hex)))
    high <- high || any(strtoi(hex, 16L) >= 128L)
    backref <- any(esc %in% utf8ToInt("gk0123456789")) ||
      grepl("\\(\\?P[=>]", pattern, perl = TRUE, useBytes = TRUE)
    if (high || backref || any(esc == utf8ToInt("o"))) {
      refuse(paste(
        "is case-insensitive and names a byte at or above 0x80 (or a",
        "back-reference or escape that can), which the locale's tables fold"
      ))
    }
  }
  invisible(TRUE)
}

#' Detect a pattern in each element, in bytes
#'
#' @param x A character vector.
#' @param pattern A PCRE pattern, or a literal when `fixed = TRUE`.
#' @param fixed Whether `pattern` is a literal.
#' @return A logical vector, NA where `x` is NA.
#' @noRd
.bytes_detect <- function(x, pattern, fixed = FALSE) {
  .bytes_check_input(x)
  .bytes_check_pattern(pattern, fixed)
  out <- grepl(pattern, x, perl = !fixed, fixed = fixed, useBytes = TRUE)
  out[is.na(x)] <- NA
  out
}

#' Replace the first (or every) match in each element, in bytes
#'
#' `replacement` refers to groups as `\\1` ... `\\9` (base-R syntax, not
#' stringi's `$1`). Base R has no whole-match reference: `sub()` writes `\\0`
#' as a literal "0", so `\\0` is refused; wrap the pattern in a group and use
#' `\\1`. The `\\U`, `\\L` and `\\E` case conversions are refused: they follow
#' the locale. With `fixed = TRUE` the replacement is a literal and none of
#' this applies.
#' @param x A character vector.
#' @param pattern A PCRE pattern, or a literal when `fixed = TRUE`.
#' @param replacement One string.
#' @param all Whether to replace every match (`gsub()`) rather than the first.
#' @param fixed Whether `pattern` and `replacement` are literals.
#' @return A character vector declared UTF-8, NA where `x` is NA.
#' @noRd
.bytes_replace <- function(x, pattern, replacement, all = FALSE,
                           fixed = FALSE) {
  .bytes_check_input(x)
  .bytes_check_pattern(pattern, fixed)
  if (!is.character(replacement) || length(replacement) != 1L ||
        is.na(replacement)) {
    stop("internal: `replacement` must be one string", call. = FALSE)
  }
  .bytes_check_input(replacement, "`replacement`")
  if (!fixed && grepl("\\\\[ULE]", replacement, perl = TRUE,
                      useBytes = TRUE)) {
    stop("internal: `replacement` uses \\U, \\L or \\E, which follow the ",
         "locale (ADR 0019)", call. = FALSE)
  }
  if (!fixed && any(.bytes_escape_codes(replacement) == utf8ToInt("0"))) {
    stop("internal: `replacement` uses \\0, which base R writes as a ",
         "literal 0; wrap the pattern in a group and use \\1 (ADR 0019)",
         call. = FALSE)
  }
  out <- if (all) {
    gsub(pattern, replacement, x, perl = !fixed, fixed = fixed,
         useBytes = TRUE)
  } else {
    sub(pattern, replacement, x, perl = !fixed, fixed = fixed,
        useBytes = TRUE)
  }
  .bytes_utf8(out)
}

#' Locate the first (or last) match in each element, in bytes
#'
#' Positions are 1-based byte offsets, inclusive. An empty match at byte `i`
#' gives `start = i`, `end = i - 1`. Rows where `x` is NA or nothing matches
#' are NA. "Last" is the last of the non-overlapping matches found scanning
#' left to right. It departs from `stringi::stri_locate_last_regex()` on a
#' pattern that can match empty: after an earlier match, base R's scan stops
#' at the end of the string and never reports an empty match there, so
#' `"ab"` with `"b*"` gives `start = 2, end = 2`, where stringi gives the
#' empty match `start = 3, end = 2`. Whether a regex can match empty is not
#' decidable from its text by a simple check, so such a pattern is not
#' refused; a caller that needs the end-of-string empty match tests for it.
#' @param x A character vector.
#' @param pattern A PCRE pattern, or a literal when `fixed = TRUE`.
#' @param fixed Whether `pattern` is a literal.
#' @param last Whether to report the last match rather than the first.
#' @return An integer matrix with columns `start` and `end`, one row per
#'   element of `x`.
#' @noRd
.bytes_locate <- function(x, pattern, fixed = FALSE, last = FALSE) {
  .bytes_check_input(x)
  .bytes_check_pattern(pattern, fixed)
  n <- length(x)
  out <- matrix(NA_integer_, n, 2L, dimnames = list(NULL, c("start", "end")))
  if (n == 0L) {
    return(out)
  }
  if (last) {
    g <- gregexpr(pattern, x, perl = !fixed, fixed = fixed, useBytes = TRUE)
    for (i in which(!is.na(x))) {
      s <- g[[i]]
      k <- length(s)
      if (s[k] > 0L) {
        out[i, ] <- c(s[k], s[k] + attr(s, "match.length")[k] - 1L)
      }
    }
    return(out)
  }
  m <- regexpr(pattern, x, perl = !fixed, fixed = fixed, useBytes = TRUE)
  len <- attr(m, "match.length")
  hit <- !is.na(m) & m > 0L
  out[hit, "start"] <- m[hit]
  out[hit, "end"] <- m[hit] + len[hit] - 1L
  out
}

#' Capture the first match and its groups in each element, in bytes
#'
#' Read from the capture attributes of one `regexpr(perl = TRUE)` call, never
#' `regmatches(regexec())`. Column 1 is the whole match, column `j + 1` group
#' `j`. A group that took part with an empty match is `""`; a group that did
#' not take part is NA, as is every column of a row where `x` is NA or nothing
#' matches. Named groups name their columns.
#' @param x A character vector.
#' @param pattern A PCRE pattern.
#' @return A character matrix declared UTF-8, one row per element of `x`.
#' @noRd
.bytes_capture <- function(x, pattern) {
  .bytes_check_input(x)
  .bytes_check_pattern(pattern, fixed = FALSE)
  m <- regexpr(pattern, x, perl = TRUE, useBytes = TRUE)
  cs <- attr(m, "capture.start")
  cl <- attr(m, "capture.length")
  groups <- if (is.null(cs)) 0L else ncol(cs)
  out <- matrix(NA_character_, length(x), groups + 1L)
  hit <- !is.na(m) & m > 0L
  if (any(hit)) {
    s <- m[hit]
    out[hit, 1L] <- .bytes_cut(x[hit], s, s + attr(m, "match.length")[hit] - 1L)
  }
  # In a matching row, R reports a group that did not take part with start 0
  # (PCRE's unset offset plus one); a group that matched empty has start >= 1.
  for (j in seq_len(groups)) {
    set <- hit & cs[, j] > 0L
    if (any(set)) {
      out[set, j + 1L] <- .bytes_cut(
        x[set], cs[set, j], cs[set, j] + cl[set, j] - 1L
      )
    }
  }
  nm <- attr(m, "capture.names")
  if (!is.null(nm) && any(nzchar(nm))) {
    colnames(out) <- c("", nm)
  }
  out
}

#' Cut each element by byte position
#'
#' Bytes `first` to `last` of each element, inclusive and 1-based. `first`
#' and `last` each have length 1, which recycles, or `length(x)`; any other
#' length stops rather than being truncated. As with `substring()`, an index
#' past either end is clamped, `first > last` gives `""`, and an NA index
#' gives NA. Pass `.Machine$integer.max` as `last` for a suffix. A cut may
#' split a multibyte
#' character; offsets from `.bytes_locate()` or `.bytes_capture()` on an
#' ASCII pattern never do.
#' @param x A character vector.
#' @param first,last Integer byte positions.
#' @return A character vector declared UTF-8, NA where `x` is NA.
#' @noRd
.bytes_cut <- function(x, first, last) {
  .bytes_check_input(x)
  n <- length(x)
  check_length <- function(v, arg) {
    if (!length(v) %in% c(1L, n)) {
      stop(sprintf("internal: `%s` must have length 1 or length(x)", arg),
           call. = FALSE)
    }
  }
  check_length(first, "first")
  check_length(last, "last")
  first <- rep_len(as.integer(first), n)
  last <- rep_len(as.integer(last), n)
  # Marked "bytes", `substring()` counts bytes and never decodes, so an
  # invalid-UTF-8 element cuts like any other. ASCII elements keep "unknown",
  # where bytes and characters coincide.
  Encoding(x) <- "bytes"
  .bytes_utf8(substring(x, first, last))
}

#' Split each element on a literal separator, in bytes
#'
#' The caller states the trailing-field rule, because the two readings in use
#' differ (ADR 0005). With `keep_trailing_empty = TRUE`, a trailing separator
#' yields a trailing `""` and `""` splits to `""`, as
#' `stringi::stri_split_fixed()` does. With `FALSE`, the trailing `""` is
#' dropped and `""` splits to `character(0)`, as `strsplit()` does.
#' @param x A character vector.
#' @param sep One non-empty literal string.
#' @param keep_trailing_empty `TRUE` or `FALSE`, with no default.
#' @return A list with one character vector per element of `x`, declared
#'   UTF-8; `NA_character_` where `x` is NA.
#' @noRd
.bytes_split_fixed <- function(x, sep, keep_trailing_empty) {
  .bytes_check_input(x)
  .bytes_check_pattern(sep, fixed = TRUE, what = "sep")
  if (!is.logical(keep_trailing_empty) || length(keep_trailing_empty) != 1L ||
        is.na(keep_trailing_empty)) {
    stop("internal: `keep_trailing_empty` must be TRUE or FALSE",
         call. = FALSE)
  }
  out <- vector("list", length(x))
  na <- is.na(x)
  out[na] <- list(NA_character_)
  if (all(na)) {
    return(out)
  }
  src <- x[!na]
  parts <- strsplit(src, sep, fixed = TRUE, useBytes = TRUE)
  if (keep_trailing_empty) {
    # `strsplit()` drops exactly one trailing empty field. It dropped one when
    # the fields and the separators between and after them account for every
    # byte of the input. Comparing lengths, not testing for a trailing `sep`,
    # stays right when `sep` overlaps itself ("aaa" on "aa" is "", "a").
    k <- lengths(parts)
    used <- vapply(parts, function(p) sum(nchar(p, type = "bytes")),
                   integer(1))
    dropped <- nchar(src, type = "bytes") == used + k * nchar(sep,
                                                              type = "bytes")
    parts[dropped] <- lapply(parts[dropped], c, "")
  }
  out[!na] <- lapply(parts, .bytes_utf8)
  out
}
