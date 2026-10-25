# Parsing phases: the decomposed helpers behind the parse engine.
#
# ---------------------------------------------------------------------------
# Each phase owns one normalization step. The `*_vec` functions are the real,
# vectorized implementation: they accept and return length-n column vectors
# (options are validated scalars per call, so every switch()/if hoists OUTSIDE
# the vector ops -- each phase is straight-line vectorized code selected once).
# The vector engine chains them in two stages (parse.R): ._parse_stage_a_vec()
# runs the option-independent, cacheable phases and ._parse_stage_b_vec() runs
# the presentation phases over the cached Stage A columns.
#
# The scalar helpers of the same base name are kept as thin wrappers that
# delegate to their `*_vec` counterpart on a length-1 input, so the per-phase
# unit tests (test-parse-phases.R) still pin behavior through them and the
# scalar orchestrator ._safe_parse_url_impl() (parse.R) keeps working.
# .parse_web_url_one()/.extract_raw_components() stay scalar because the web
# parser is scalar; the engine reuses it in its single per-URL loop
# and extracts the raw columns itself.
# ---------------------------------------------------------------------------

# Byte-mode spellings of the two ICU regex behaviors the boolean and
# replacement calls in this file had, and keep, since they moved off stringi
# (RURL-oahhlnvf, ADR 0019). R/strings-base.R refuses a bare `$`, so a
# pattern names the end it means.
#
# `.ICU_END` is ICU's `$` outside multiline mode: the true end of the input,
# or the point before a final line terminator (LF, VT, FF, CR, CRLF, U+0085,
# U+2028 or U+2029, spelled as UTF-8 bytes). `\z` is the true end only. A
# site whose class before the anchor absorbs every terminator writes `\z`,
# which is then the same; the rest write `.ICU_END`, so a row such as
# "1.2.3.4\n" keeps the reading it had (the `NULL` profile is byte-frozen,
# ADR 0016). The lookbehind refuses the point between the CR and LF of a
# final CRLF, which ICU's `$` does not take: CRLF is one terminator.
.ICU_END <- paste0(
  "(?=(?:\\r\\n|[\\n\\x0b\\x0c\\r]|\\xc2\\x85|\\xe2\\x80[\\xa8\\xa9])?\\z)",
  "(?!(?<=\\r)\\n)"
)

# ICU's `.` matches any code point but the seven terminators above (LF, VT,
# FF, CR, U+0085, U+2028, U+2029; measured against stringi), so its `.*`
# stops at the first one. Byte mode's `.` is any byte but LF, and ICU's `.`
# cannot be spelled in bytes without a group repeated per multibyte
# character, which hits PCRE's match limit on a multi-megabyte row (`sub()`
# then only warns and leaves the row uncut). So no pattern here spells it.
# The sites that cut with ICU's `.*` first split the row at its terminators
# (`.icu_split_lines()`), and then cut a part that holds none, where ICU's
# `.` is byte mode's `(?s:.)`. PCRE's match limit also counts each retry of
# a group or an assertion after a repeat gives back a byte, so a repeat here
# is possessive, or is followed by a literal its class excludes (PCRE then
# makes it possessive), or ends the match; a repeat that may give back must
# not be retried at every byte. Each iteration of a repeated group counts
# too, so a group is not repeated once per delimiter of the row. Every cut and
# test below is linear in the row and stays under the limit at any length
# (RURL-atlidhyh).

# A line terminator, and the same at the true end.
.ICU_TERMINATOR <- paste0(
  "(?:\\r\\n|[\\n\\x0b\\x0c\\r]|\\xc2\\x85|\\xe2\\x80[\\xa8\\xa9])"
)
.ICU_FINAL_TERMINATOR <- paste0(.ICU_TERMINATOR, "\\z")

# A terminator that is not the final one: one not followed by the true end.
# The atomic group takes a CRLF whole, so its CR is never read as a
# terminator of its own.
.ICU_INNER_TERMINATOR <- paste0(
  "(?>\\r\\n?|[\\n\\x0b\\x0c]|\\xc2\\x85|\\xe2\\x80[\\xa8\\xa9])(?!\\z)"
)

# On a part with no terminator: from the first "/", "?" or "#" to the end;
# through the last "@"; a leading "[" through the first "]". The cut through
# the last "@" ends where this match starts: the only "@" from which "@"-free
# bytes run to the end is the last one. The anchored `\A(?s:.)*@` ran to the
# end and gave the row back byte by byte to reach that "@", and past ~1e7
# bytes hit PCRE's match limit (RURL-atlidhyh); unanchored, each start is a
# fresh match and the possessive run gives nothing back.
.ICU_FROM_PATH_DELIM <- "(?s)[/?#].*\\z"
.ICU_THROUGH_LAST_AT <- "@[^@]*+\\z"
.ICU_BRACKET_HEAD <- "\\A\\[[^\\]]*+\\]"

# A whole run of C0 controls and spaces, for a trailing trim: the pattern
# appends the end it means. The lookbehind lets the run start only where a
# run starts, and the run is possessive. A shorter run from the same start
# ends before a run byte, which no end follows: every ASCII line terminator
# is a run byte, and U+0085, U+2028 and U+2029 can only follow the whole run.
# So the match is the one `[\x00-\x20]+` found, but where that pattern was
# tried at each byte of an inner run and gave the rest back byte by byte,
# quadratic in the run and over PCRE's match limit past ~1e7 bytes
# (RURL-atlidhyh), this is tried once per run and gives nothing back.
.C0_SPACE_RUN <- "(?<![\\x00-\\x20])[\\x00-\\x20]++"

# Split each row into `head` (through its last inner terminator, or ""),
# `line` (the rest, without the final terminator; it holds no terminator)
# and `final` (the final terminator, or ""). For non-NA rows. Every
# terminator is found in one left-to-right scan, a CRLF taken whole; the
# last one is final when it ends the row.
.icu_split_lines <- function(x) {
  nb <- nchar(x, type = "bytes")
  g <- gregexpr(.ICU_TERMINATOR, x, perl = TRUE, useBytes = TRUE)
  body_end <- nb
  head_end <- integer(length(x))
  for (i in seq_along(x)) {
    s <- g[[i]]
    if (s[1L] < 0L) {
      next
    }
    e <- s + attr(s, "match.length") - 1L
    k <- length(s)
    if (e[k] == nb[i]) {
      body_end[i] <- s[k] - 1L
      k <- k - 1L
    }
    if (k > 0L) {
      head_end[i] <- e[k]
    }
  }
  list(
    head = .bytes_cut(x, 1L, head_end),
    line = .bytes_cut(x, head_end + 1L, body_end),
    final = .bytes_cut(x, body_end + 1L, nb)
  )
}

# The ICU `sub("[/?#].*$", "", x)`: the path, query and fragment cut. ICU's
# `.*` cannot cross a terminator and its `$` needs the true end or the final
# terminator, so the cut can only start on the last line: there it is the
# first `[/?#]`, and the final terminator stays after it. A row with no
# terminator is all last line. Finding the last line first, rather than
# retrying the cut at every earlier delimiter, keeps this linear.
.icu_cut_tail <- function(x) {
  .bytes_check_input(x)
  out <- .bytes_utf8(sub(.ICU_FROM_PATH_DELIM, "", x, perl = TRUE,
                         useBytes = TRUE))
  multi <- grepl(.ICU_TERMINATOR, x, perl = TRUE, useBytes = TRUE)
  if (any(multi)) {
    p <- .icu_split_lines(x[multi])
    line <- .bytes_utf8(sub(.ICU_FROM_PATH_DELIM, "", p$line, perl = TRUE,
                            useBytes = TRUE))
    out[multi] <- .bytes_utf8(paste0(p$head, line, p$final))
  }
  out
}

# The ICU `sub("^.*@", "", x)`: drop everything through the last "@" before
# the first terminator, the userinfo of an authority.
.icu_cut_through_last_at <- function(x) {
  .bytes_check_input(x)
  out <- x
  todo <- which(!is.na(x))
  if (!length(todo)) {
    return(out)
  }
  y <- x[todo]
  first <- regexpr(.ICU_TERMINATOR, y, perl = TRUE, useBytes = TRUE)
  line1_end <- ifelse(first > 0L, first - 1L, nchar(y, type = "bytes"))
  line1 <- .bytes_cut(y, 1L, line1_end)
  # `m` is the byte of the last "@" in line 1, so the cut keeps what follows.
  m <- regexpr(.ICU_THROUGH_LAST_AT, line1, perl = TRUE, useBytes = TRUE)
  hit <- m > 0L
  y[hit] <- .bytes_cut(y[hit], m[hit] + 1L, .Machine$integer.max)
  out[todo] <- y
  .bytes_utf8(out)
}

# The ICU `sub("^(\\[[^\\]]*\\]).*$", "\\1", x)`: a bracketed host, without
# what follows it on its line. `[^\]]*` may cross a terminator; `.*$` then
# needs the rest to be one line, and the final terminator stays.
.icu_cut_after_bracket <- function(x) {
  .bytes_check_input(x)
  out <- x
  m <- regexpr(.ICU_BRACKET_HEAD, x, perl = TRUE, useBytes = TRUE)
  hit <- !is.na(m) & m > 0L
  if (any(hit)) {
    len <- attr(m, "match.length")[hit]
    xs <- x[hit]
    rest <- .bytes_cut(xs, len + 1L, .Machine$integer.max)
    ok <- !grepl(.ICU_INNER_TERMINATOR, rest, perl = TRUE, useBytes = TRUE)
    fin <- regexpr(.ICU_FINAL_TERMINATOR, rest, perl = TRUE, useBytes = TRUE)
    final <- ifelse(fin > 0L, .bytes_cut(rest, fin, .Machine$integer.max), "")
    xs[ok] <- paste0(.bytes_cut(xs[ok], 1L, len[ok]), final[ok])
    out[hit] <- xs
  }
  .bytes_utf8(out)
}

# The ICU capture `stri_match_first_regex(x, paste0(prefix, "(.*)$"))`, in
# bytes (RURL-yqmoztoj). `prefix` is start-anchored and holds its own groups;
# the last column is the `(.*)` group. Byte mode runs `prefix` then the rest
# of the row to the true end, so `prefix` takes the span ICU's first attempt
# gave it. ICU's `.*$` then needs that rest to hold no terminator but a final
# one, which it leaves out of the group and the match. Giving `prefix` back a
# byte only lengthens the rest, so a row that fails there fails everywhere:
# it is no match, all NA. The rest is split once at its terminators, so the
# check is linear. A row led by U+FEFF is read as data, where stringi read
# past the mark, so the prefix does not match there. No caller masks such a
# row: byte mode reads the mark as data, so the prefix finds no scheme on it
# (RURL-dddxutjf removed the masks). `.parse_whatwg_file_urls_vec()`'s only
# caller hands it no such row, since the `^[Ff][Ii][Ll][Ee]:` test that
# selects its rows is byte mode too.
.icu_capture_to_end <- function(x, prefix) {
  m <- .bytes_capture(x, paste0(prefix, "((?s:.*))\\z"))
  last <- ncol(m)
  multi <- !is.na(m[, last]) &
    grepl(.ICU_TERMINATOR, m[, last], perl = TRUE, useBytes = TRUE)
  if (any(multi)) {
    p <- .icu_split_lines(m[multi, last])
    whole <- m[multi, 1L]
    keep <- nchar(whole, type = "bytes") - nchar(p$final, type = "bytes")
    rows <- m[multi, , drop = FALSE]
    rows[, 1L] <- .bytes_cut(whole, 1L, keep)
    rows[, last] <- p$line
    rows[p$head != "", ] <- NA_character_
    m[multi, ] <- rows
  }
  m
}

# The WHATWG forbidden domain code points (`.WHATWG_FORBIDDEN_HOST_CP`,
# R/utils.R) as a byte-mode class: the same ASCII set, with the ICU `\u`
# escapes PCRE does not read written as `\x`.
.WHATWG_FORBIDDEN_HOST_CP_BYTES <- "[\\x01-\\x1f\\x7f #%/:<>?@\\[\\]\\\\^|]"

# A byte outside 0x01-0x7F: the byte-mode form of the ICU class
# `[^\u0001-\u007f]`. On valid UTF-8 the two find the same rows; ICU read an
# undecodable octet as U+FFFD, which is non-ASCII too.
.NON_ASCII_BYTE <- "[^\\x01-\\x7f]"

# One part of a host that is not an IPv4-attempt number: at the start or
# after a dot, a part that is not `0x` + hex digits or decimal digits
# followed by a dot or ICU's end. A host is an IPv4 attempt when no such part
# exists (`.classify_input_host_vec()`, RURL-rrumztrf).
.IPV4ISH_BAD_PART <- paste0(
  "(?:^|\\.)(?!(?:0[xX][0-9a-fA-F]*+|[0-9]++)(?:\\.|", .ICU_END, "))"
)

# A last path segment `index.*` or `default.*`, any ASCII case, with an
# optional trailing "/": the rows `index_page_handling = "strip"` hands to
# `._strip_index_page()`. Was the ICU `(?i)/(index|default)\.[^/]+/?$`; ICU
# folds no non-ASCII code point to these letters, so the classes lose no
# match, and `$` stays ICU's.
.INDEX_PAGE_TAIL <- paste0(
  "/(?:[Ii][Nn][Dd][Ee][Xx]|[Dd][Ee][Ff][Aa][Uu][Ll][Tt])\\.[^/]++/?", .ICU_END
)

# A host bracketed end to end: the ICU `^\[.*\]$`, with ICU's `.` and `$`
# (`.serialize_whatwg_ipv6_host()`). ICU's `.*` holds no terminator, so a
# row has that shape when it starts with "[", this closing "]" matches, and
# `.ICU_INNER_TERMINATOR` does not.
.IPV6_BRACKETED_SHAPE <- paste0("\\]", .ICU_END)

# The per-host patterns of `.detect_ip_host_vec()`,
# `.host_ends_in_number_vec()` and `.serialize_whatwg_ipv6_hosts_vec()`, each
# ending at ICU's `$`. A canonical IPv4 octet is 0..255 with no leading zero
# (rejects 00, 01, 007, 010).
.ICU_IPV4_OCTET <- "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])"
.ICU_IPV4_CANONICAL <- paste0(
  "^", .ICU_IPV4_OCTET, "(?:\\.", .ICU_IPV4_OCTET, "){3}", .ICU_END
)
# `.ICU_IPV6_IPV4_TAIL` was the ICU `^\[?([0-9a-fA-F:]+:)<quad>\]?$`. The
# dotted quad holds no ":", so its first octet is the digits after the last
# ":" of the hex-and-colon run, up to the first "."; the run is taken whole,
# possessively, and a lookbehind checks that it ends in ":" and a canonical
# octet, with a run byte before the ":". Retrying the quad after every
# earlier ":" hit PCRE's match limit on a host of millions of groups, and an
# atomic `(?>[0-9a-fA-F:]+:)` still gave a long last group back byte by byte
# (RURL-atlidhyh). Each lookbehind branch is fixed-length, which every PCRE
# release accepts.
.ICU_IPV6_RUN_BYTE <- "[0-9a-fA-F:]"
.ICU_IPV6_RUN_ENDS_IN_OCTET <- paste0(
  "(?<=",
  paste0(.ICU_IPV6_RUN_BYTE, ":",
         c("25[0-5]", "2[0-4][0-9]", "1[0-9][0-9]", "[1-9][0-9]", "[0-9]"),
         collapse = "|"),
  ")"
)
.ICU_IPV6_IPV4_TAIL <- paste0(
  "^\\[?", .ICU_IPV6_RUN_BYTE, "++", .ICU_IPV6_RUN_ENDS_IN_OCTET,
  "(?:\\.", .ICU_IPV4_OCTET, "){3}\\]?", .ICU_END
)
.ICU_IPV6_HEX_SHAPE <- paste0("^\\[?[0-9a-fA-F:]++\\]?", .ICU_END)
.ICU_IPV6_SERIALIZABLE <- paste0("^\\[[0-9A-Fa-f:.]++\\]", .ICU_END)
.ICU_TRAILING_DOT <- paste0("\\.", .ICU_END)
# The dotted-quad tail of a bracketed IPv6 host, matched whole. The ICU
# capture it replaces was `^(.*:)(<quad>)$`; the quad holds no ":", so it can
# only follow the last ":". `.serialize_whatwg_ipv6_host()` cuts at the last
# ":" and tests the rest against this pattern. In one regex, an atomic
# `(?>.*:)` gave bytes back to the last ":" one by one (RURL-yqmoztoj), and
# possessive `(?:[^:]*+:)++` counted a step per ":", so both hit PCRE's
# match limit on a host of ~1e7 bytes (RURL-rweowcfz).
.IPV6_QUAD_TAIL <- paste0(
  "\\A", .ICU_IPV4_OCTET, "\\.", .ICU_IPV4_OCTET, "\\.",
  .ICU_IPV4_OCTET, "\\.", .ICU_IPV4_OCTET, "\\z"
)
.ICU_NUMBER_LAST_LABEL <- paste0(
  "(?:\\A|\\.)(?:[0-9]++|0[xX][0-9a-fA-F]*+)", .ICU_END
)

# `.bytes_detect()` and `.bytes_replace()` for a pattern that is a constant of
# this file. They skip `.bytes_check_pattern()`, whose walk over the bytes of
# a long pattern such as `.ICU_IPV6_IPV4_TAIL` cost more than the match in the
# per-host helpers; tests/testthat/test-stringi-bool-parse-phases.R runs each
# constant passed here through that check instead. The rest is the wrappers'
# own handling: the input check, NA in and NA out, and a result declared
# UTF-8.
.const_detect <- function(x, re) {
  .bytes_check_input(x)
  out <- grepl(re, x, perl = TRUE, useBytes = TRUE)
  out[is.na(x)] <- NA
  out
}
.const_replace <- function(x, re, replacement) {
  .bytes_check_input(x)
  .bytes_utf8(sub(re, replacement, x, perl = TRUE, useBytes = TRUE))
}
# `.bytes_split_fixed(s, sep, keep_trailing_empty = TRUE)[[1L]]` for one
# non-NA string and a constant separator, without the separator check, for
# the per-host IPv6 serializer. The trailing-field rule is the wrapper's: a
# trailing empty field and `""` -> `""` survive, as `stri_split_fixed()`
# gave (ADR 0005), and the dropped field is found by counting bytes.
.const_split_keep_trailing <- function(s, sep) {
  .bytes_check_input(s)
  p <- strsplit(s, sep, fixed = TRUE, useBytes = TRUE)[[1L]]
  used <- sum(nchar(p, type = "bytes")) + length(p) * nchar(sep, type = "bytes")
  if (nchar(s, type = "bytes") == used) {
    p <- c(p, "")
  }
  .bytes_utf8(p)
}

# Each ASCII letter of `x` as a two-case class, `http` -> `[Hh][Tt][Tt][Pp]`.
# Byte mode does no case-insensitive matching (ADR 0019, decision 2). `x` is
# ASCII letters and digits only, here the special-scheme names.
.ascii_letter_classes <- function(x) {
  vapply(x, function(w) {
    ch <- strsplit(w, "", fixed = TRUE)[[1L]]
    up <- chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", ch)
    lo <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", ch)
    paste(ifelse(up == lo, ch, paste0("[", up, lo, "]")), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# Phase 1 helper (vector): classify the host token of each raw input for the
# host-shape gate. rurl only fabricates a scheme for URL-shaped input, so we
# must isolate and inspect the host before the web parse. Returns, per element:
#   has_userinfo    - an '@' appears in the authority (user[:pass]@host)
#   is_localhost    - the bare host is an allowlisted single-label host
#   is_dotted_name  - >= 2 non-empty dot-separated labels (example.com, a.b.c)
#   is_ipish        - the host is an IP *attempt*: all-decimal / 0x-hex / octal
#                     / short-form groups, or anything containing ':' (IPv6)
#   is_canonical_ip - a valid IPv4 dotted-quad or bracketed IPv6 per the strict
#                     detector (rejects leading-zero octets and coerced forms)
# Purely a function of the input string; used to drive rejection in Phase 1.
.classify_input_host_vec <- function(url) {
  # Strip a leading "scheme://" or scheme-relative "//", then the path/query/
  # fragment, to isolate the authority (userinfo + host + port).
  # Byte mode (ADR 0019). ICU's `.` and `$` are kept (`.icu_cut_tail()`,
  # `.icu_cut_through_last_at()`), so an interior line terminator still ends the
  # cut as before.
  s <- .bytes_replace(url, "^[a-zA-Z][a-zA-Z0-9+.-]*://", "")
  s <- .bytes_replace(s, "^//", "")
  authority <- .icu_cut_tail(s)

  has_userinfo <- .bytes_detect(authority, "@", fixed = TRUE)
  has_userinfo[is.na(has_userinfo)] <- FALSE

  # Host = authority after any userinfo, minus the port. Bracketed IPv6 keeps
  # its "[...]"; otherwise drop a single trailing ":port". `startsWith()`, not
  # stri_startswith_fixed(), which reads past a U+FEFF that starts its input
  # (see .starts_with_bom()): a host led by the mark is not bracketed
  # (RURL-opwjlnla).
  host_ui <- .icu_cut_through_last_at(authority)
  bracketed <- startsWith(host_ui, "[")
  bracketed[is.na(bracketed)] <- FALSE
  # `[^:]*` takes every line terminator, so `\z` reads as ICU's `$` did. The
  # run is possessive: a shorter run never reaches `\z`, and giving it back
  # byte by byte after each earlier ":" hit PCRE's match limit on a long row
  # (RURL-atlidhyh).
  host_token <- ifelse(
    bracketed,
    .icu_cut_after_bracket(host_ui),
    .bytes_replace(host_ui, ":[^:]*+\\z", "")
  )

  host_lower <- .ascii_tolower(host_token)
  is_localhost <- host_lower %in% .SPECIAL_SINGLE_LABEL_HOSTS

  # Two or more non-empty dot-separated labels. A linear check, not the
  # repeated group `^[^.]+(\.[^.]+)+$`: ICU's backtrack stack overflowed on a
  # host of about 4e5 labels and threw for the whole vector (RURL-rrumztrf).
  # A dot, and no empty label: none first, last or doubled. `[^.]` took a line
  # terminator too, so ICU's `$` added nothing there.
  is_dotted_name <- .bytes_detect(host_token, ".", fixed = TRUE) &
    !.bytes_detect(host_token, "^\\.|\\.\\.|\\.\\z")
  is_dotted_name[is.na(is_dotted_name)] <- FALSE

  # inet_aton / WHATWG IPv4 attempts: dotted groups each all-decimal or 0x-hex
  # (12345, 0x7f000001, 0x, 017700000001, 192.168, 1.2.3.4, 256.1.1.1,
  # 1.2.3.4.5), plus any ':' host (IPv6, incl. bracketed). WHATWG's IPv4
  # number parser treats a bare "0x" prefix as zero after stripping the prefix.
  # Read as the inverted search for one bad part, linear in the host, where
  # the repeated group `^(...)(\.(...))*$` overflowed ICU's backtrack stack on
  # a many-label host (RURL-rrumztrf). A part starts at the start or after a
  # dot, and must be a number followed by a dot or ICU's end (`.ICU_END`).
  ipv4ish <- !.bytes_detect(host_token, .IPV4ISH_BAD_PART)
  ipv4ish[is.na(ipv4ish)] <- FALSE
  ipv6ish <- .bytes_detect(host_token, ":", fixed = TRUE)
  ipv6ish[is.na(ipv6ish)] <- FALSE
  is_ipish <- ipv4ish | ipv6ish

  # Canonical per the strict detector (Phase 5): rejects leading-zero octets,
  # out-of-range/wrong-arity, and integer/hex/octal coercion; accepts 1.2.3.4
  # and [::1].
  is_canonical_ip <- .detect_ip_host_vec(host_token)

  list(
    has_userinfo = has_userinfo,
    is_localhost = is_localhost,
    is_dotted_name = is_dotted_name,
    is_ipish = is_ipish,
    is_canonical_ip = is_canonical_ip,
    # Original (pre-parse) host token and the IPv4-attempt flag, surfaced for
    # the url_standard host model (RURL-luwvkwhd): the web parser coerces
    # numeric IPv4
    # forms itself, so the model reads the original token to keep an RFC 3986
    # reg-name uncoerced and to compute shape-keyed diagnostics.
    host_token = host_token,
    is_ipv4ish = ipv4ish
  )
}

# Phase 1 helper (vector): WHATWG literal backslash-as-slash recognition
# (RURL-ledntyab, PRD v2 D2, §5.2). Under url_standard = "whatwg", for schemes
# in .WHATWG_SPECIAL_SCHEMES (http/https/ftp/file -- NOT ftps, which WHATWG
# does not define as special), a literal backslash is treated identically to a
# forward slash everywhere the WHATWG state machine checks for one: the
# scheme-relative "//" marker, the authority/path boundary, and path-segment
# separators. For rurl's authority-based special schemes (http/https/ftp), the
# same pre-parse step also implements the WHATWG special-authority-slashes
# state for inputs with no slash run at all: `http:example.com` is handed to the
# parser as `http://example.com`. This must run BEFORE the parse (the parser
# never treats "\" as a separator and rejects missing `//` authority forms), so
# it rewrites the raw string here in Phase 1, ahead of every other scheme/host
# regex in this function -- once rewritten, the existing "://"/"//" detection,
# host classification, and the parser handoff need no further changes.
#
# The leading run right after "scheme:" is handled as its own case (mirroring
# the WHATWG "special authority slashes"/"special authority ignore slashes"
# states) rather than a blind 1:1 "\" -> "/" substitution: that run -- of ANY
# length and ANY mix of "/" and "\" -- is collapsed to exactly "//" before
# authority parsing continues, so a single "\" (RURL-ledntyab's own acceptance
# case "http:\host\path"), a double "\\" ("http:\\host\path"), and an
# already-canonical "//" all normalize the same way. A run of length 0 (no
# separator at all, e.g. "http:host/path") is authority-introducing for the
# http/https/ftp subset, but not for file (left to the file-state slice). AFTER
# the authority marker, a literal "\" remains a plain 1:1 "\" -> "/" rewrite
# (path-segment separators, and the authority/path boundary when it isn't part
# of the run).
#
# Recognition only, no decoding: `%5C` (a percent-encoded backslash) is inert
# literal text here, never treated as a separator -- it contains no actual
# backslash byte. The query and fragment are never touched: only the span from
# just after "scheme:" up to the first literal "?"/"#" is eligible, so a
# backslash inside `?q=a\b#frag\c` stays untouched even on a rewritten row.
#
# Returns `url` (rewritten for eligible rows, unchanged otherwise) and
# `backslash_rewritten`, a logical mask marking rows where a literal "\" byte
# actually participated in a rewrite (in the leading run or the remainder) --
# used to emit the `invalid-reverse-solidus` diagnostic. A leading run that was
# ALREADY all forward slashes (a plain "http://", or the spec-accurate
# multi-slash collapse of e.g. "http:///host") changes no backslash, so it does
# NOT set this flag even though the string may still be rewritten.
.rewrite_whatwg_backslashes_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(url = url, backslash_rewritten = rep(FALSE, n))
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }

  # Byte mode (ADR 0019): a row led by U+FEFF has no scheme, so it is never
  # eligible. The stringi match read past the mark and needed a mask
  # (RURL-iiehsbfg); the close-out removed it (RURL-dddxutjf).
  scheme_match <- .bytes_capture(url, "^([a-zA-Z][a-zA-Z0-9+.-]*):")
  scheme_lower <- .ascii_tolower(scheme_match[, 2L])
  eligible <- !is.na(scheme_lower) &
    scheme_lower %in% .WHATWG_SPECIAL_SCHEMES
  if (!any(eligible)) {
    return(no_op)
  }

  # `nchar(NA, type = "bytes")` is 2 by default; `keepNA = TRUE` keeps a row
  # with no scheme NA.
  colon_len <- nchar(scheme_match[, 1L], type = "bytes", keepNA = TRUE)

  # Every span below is located in and cut from `url` itself, which starts with
  # the scheme, in bytes from locator to cut (ADR 0019). Some stringi
  # functions read past a U+FEFF that starts their input, so cutting or
  # rewriting a span that starts with one ("http://\ufeff/p" leaves
  # "\ufeff/p") once lost it, and "http:///p" then read "p" as its host
  # (RURL-iiehsbfg).
  #
  # The query/fragment boundary is the first literal '?' or '#' -- an
  # unencoded delimiter byte, so it is safe to locate before any rewriting. The
  # scheme holds neither, so it always falls after the colon.
  qf_start <- .bytes_locate(url, "[?#]")[, "start"]
  before_end <- ifelse(is.na(qf_start), .Machine$integer.max, qf_start - 1L)
  before <- .bytes_cut(url, colon_len + 1L, before_end)
  after <- ifelse(
    is.na(qf_start), "", .bytes_cut(url, qf_start, .Machine$integer.max)
  )

  # Leading run of one or more '/'/'\' right after "scheme:" -- the
  # authority-introducing marker, of any length/composition. Neither character
  # is '?' or '#', so the run always lies inside `before`.
  run <- .bytes_capture(url, "^[^:]*:([/\\\\]+)")[, 2L]
  has_run <- !is.na(run)

  run_len <- ifelse(has_run, nchar(run, type = "bytes"), 0L)
  remainder <- .bytes_cut(url, colon_len + run_len + 1L, before_end)

  # Beyond the leading run, a literal backslash is a plain separator: rewrite
  # it 1:1 to '/' (path segments, and the authority/path boundary when the
  # authority had no run of its own, e.g. "http://host\path"), behind the
  # collapsed "//" marker. The byte-mode replace reads a U+FEFF that starts
  # `remainder` as data.
  rewritten_remainder <- paste0(
    "//", .bytes_replace(remainder, "\\", "/", all = TRUE, fixed = TRUE)
  )

  no_run_authority <- eligible & !has_run &
    scheme_lower %in% .SPECIAL_AUTHORITY_SCHEMES &
    !is.na(before) & before != ""

  # No run at all is usually left untouched. For http/https/ftp under WHATWG,
  # synthesize the missing authority marker. Otherwise: collapse an existing
  # run to exactly "//" and splice the (possibly rewritten) remainder back on.
  rewritten_before <- ifelse(
    has_run | no_run_authority, rewritten_remainder, before
  )

  run_had_backslash <- has_run & .bytes_detect(run, "\\", fixed = TRUE)
  run_had_backslash[is.na(run_had_backslash)] <- FALSE
  remainder_had_backslash <- (has_run | no_run_authority) &
    .bytes_detect(remainder, "\\", fixed = TRUE)
  remainder_had_backslash[is.na(remainder_had_backslash)] <- FALSE
  # file:///path carries an empty file authority. Collapsing an all-forward-
  # slash run from "///" to "//" would turn it into file://path, which the
  # parser
  # treats as a non-local file host and rejects. Keep file rows byte-for-byte
  # when no literal backslash is present; actual backslash repair still uses the
  # shared special-scheme path above.
  file_no_backslash <- scheme_lower == "file" & has_run &
    !run_had_backslash & !remainder_had_backslash
  file_no_backslash[is.na(file_no_backslash)] <- FALSE
  rewritten_before[file_no_backslash] <- before[file_no_backslash]

  changed <- eligible & (run_had_backslash | remainder_had_backslash)
  changed[is.na(changed)] <- FALSE

  url_out <- url
  url_out[eligible] <- paste0(
    scheme_match[eligible, 1L], rewritten_before[eligible], after[eligible]
  )

  list(url = url_out, backslash_rewritten = changed)
}

# Whether each string starts with U+FEFF, matched as bytes with base R. Some
# stringi functions read past a U+FEFF that starts their input, subject and
# pattern alike, as a byte-order mark: on stringi 1.8.9 (ICU 71.1),
# stri_match_*_regex(), the *_fixed() family, stri_sub() and
# stri_escape_unicode() do (stri_locate_*_fixed() included), while
# stri_detect_regex(), stri_replace_*_regex() and stri_locate_*_regex() count
# it. A start-anchored stringi match can therefore succeed on a row led by
# U+FEFF where none of its patterns could, since U+FEFF is not an ASCII alpha,
# a C0 control, a space or a slash; callers masked such rows with this, under
# every url_standard (RURL-vhionecz, RURL-biunpazk). Byte mode reads the mark
# as data (ADR 0019), so only the guards that still front a stringi call use
# this: ADR 0005's table, and the whatwg `stri_enc_toutf8()` splice in
# resolve_url() (ADR 0019 decision 4, still open). NA reads FALSE.
.starts_with_bom <- function(x) {
  grepl("^\ufeff", x, useBytes = TRUE)
}

# WHATWG input stripping -- the basic URL parser's step 1, BOTH halves
# (RURL-tyetpjym, RURL-yvxpanix). Step 1 says, in this order:
#   1a. Remove any leading and trailing C0 control or SPACE (U+0000..U+0020)
#       from the input.
#   1b. Remove ALL ASCII tab (U+0009), LF (U+000A) and CR (U+000D) from the
#       input, everywhere in the string.
# Both run before any component is parsed, so every downstream scheme/host
# regex and the backslash recognizer see the already-stripped string. rurl
# otherwise rejects a control char in the authority (the parser errors) and
# percent-encodes a trailing space into the path -- correct under RFC 3986,
# which has no strip step and requires such bytes to be percent-encoded -- so
# this runs ONLY under url_standard == "whatwg" and is a byte-for-byte no-op
# otherwise. Note the spec order matters for fact attribution: a LEADING tab is
# removed by 1a, not 1b.
#
# Neither strip is silent. Two SEPARATE masks are returned, because they are two
# different facts (ADR 0006 -- surface the mutation as a FACT; ADR 0009's
# precedent of a new token for a new fact):
#   - `leading_trailing_stripped` -> the `leading-trailing-stripped` diagnostic
#     (rows where 1a removed a leading/trailing C0-or-space run).
#   - `control_char_stripped` -> the `control-char-stripped` diagnostic (rows
#     where 1b removed an interior tab/LF/CR). Only tab/LF/CR are removed by
#     1b; other interior C0 controls are left to the web parser.
# A row can carry both. Resolves the control-char-in-authority family: c0-tab
# probe, eq-U6 (LF), yal-002 (TAB), yal-003 (CR/LF), which WHATWG
# strips-and-accepts.
#
# This function is deliberately the single seam for step 1, shared by prep, the
# Stage-A general route, the Stage-B general re-parse and
# `.has_explicit_authority()`: putting step 1 here (never at a call site) is
# what keeps every stage's strip byte-identical by construction. It is not the
# whole general-route input, though: prep, Stage A and Stage B run the browser
# fixer first, Stage A, Stage B and `.fsss_record_vec()` through
# .general_route_input_vec() (RURL-vmsmlflr). A caller that runs this alone
# sees that input only where the fixer is a no-op or cannot change its answer:
# the url-key, resolve and scheme-policy surfaces never run under
# `fixup_posture = "browser"`, and inside the parse `.has_explicit_authority()`
# trims a leading C0-or-space run itself and is asked only about `mailto:`,
# which the fixer otherwise leaves alone.
.strip_whatwg_control_chars_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(
    url = url,
    control_char_stripped = rep(FALSE, n),
    leading_trailing_stripped = rep(FALSE, n)
  )
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }

  # Step 1a: leading/trailing C0-control-or-SPACE. `\z` (true end of input), not
  # `$`: ICU's `$` also matches before a final line terminator, and LF/CR/VT/FF
  # all live inside this class. The regex calls here count a leading U+FEFF,
  # so a row led by one keeps it (see .starts_with_bom()).
  #
  # The leading trim STAYS ON STRINGI (RURL-oahhlnvf): its ICU read is also
  # the decode this route relies on. stringi writes U+FFFD for each octet that
  # is not UTF-8, as the URL Standard's input does (its parser reads scalar
  # values, and a decoder writes U+FFFD for an undecodable sequence), and
  # every later step of the `whatwg` route, the `file:` parser among them,
  # sees only that decoded copy (`differs` below). Byte mode would hand the
  # raw octets on. Replacing it is an encoding decision, the semantic
  # exceptions' slice (RURL-yyeohqah, ADR 0019), not a byte-mode rewrite.
  url_out <- url
  trimmed <- stringi::stri_replace_first_regex(
    url_out, "^[\\u0000-\\u0020]+", ""
  )
  trimmed <- .bytes_replace(trimmed, paste0(.C0_SPACE_RUN, "\\z"), "")
  # `differs` picks the rows that flow on as stringi's copy; it is NOT the strip
  # fact. `!=` compares a native input with stringi's UTF-8 output, and R reads
  # the native side in the session charset: under LC_ALL=C every non-ASCII row
  # differs although nothing was removed (RURL-bnorcgbv). Taking the copy there
  # hands on the same octets, so no value moves. It also differs, in every
  # locale, where stringi substituted U+FFFD for an undecodable octet, and that
  # copy is the one the parse must see.
  differs <- !is.na(trimmed) & !is.na(url_out) & trimmed != url_out
  differs[is.na(differs)] <- FALSE
  if (any(differs)) {
    url_out[differs] <- trimmed[differs]
  }
  # The fact: a C0-or-space run at either end, matched by the same classes and
  # anchors as the strip, so it reads code points and no encoding mark or
  # session charset can move it.
  lt <- .bytes_detect(url, "^[\\x00-\\x20]|[\\x00-\\x20]\\z")
  lt[is.na(lt)] <- FALSE

  # Step 1b: every remaining tab/LF/CR, anywhere.
  had <- .bytes_detect(url_out, "[\\t\\n\\r]")
  had[is.na(had)] <- FALSE
  if (any(had)) {
    url_out[had] <- .bytes_replace(url_out[had], "[\\t\\n\\r]", "", all = TRUE)
  }

  list(
    url = url_out,
    control_char_stripped = had,
    leading_trailing_stripped = lt
  )
}

# The three code points UTS #46 maps to ASCII "." (section 4, step 1): U+3002,
# U+FF0E and U+FF61, as a byte-mode alternation of their UTF-8 sequences
# (ADR 0019): E3 80 82, EF BC 8E and EF BD A1. UTF-8 is self-synchronizing, so
# each sequence matches only the code point it spells.
.UTS46_FULL_STOP_VARIANTS <-
  "(?:\\xe3\\x80\\x82|\\xef\\xbc\\x8e|\\xef\\xbd\\xa1)"

# WHATWG / UTS-46 alternative full-stop mapping (RURL-odsmwsxu). UTS-46
# domain-to-ASCII -- the "map" step the WHATWG host parser runs for special
# schemes -- maps three alternative full-stop code points to ASCII "." before a
# host is split into labels: U+3002 (ideographic full stop), U+FF0E (fullwidth
# full stop), and U+FF61 (halfwidth ideographic full stop). rurl hands the raw
# string to the web parser, which does NOT apply UTS-46, so a Unicode-dot host
# like "127。0。0。1" reaches it with its separators intact, is never split into
# numeric labels, and so never coerces to the canonical dotted-quad (an
# SSRF-relevant loopback/metadata obfuscation -- browsers coerce). The upstream
# host-shape gate is no help either: .classify_input_host_vec()'s ipv4-attempt
# test splits on ASCII "." only, so a literal-Unicode-dot host is not even seen
# as an IPv4 attempt. Mapping the three code points to "." here, BEFORE the
# parse,
# lets the existing IPv4 coercion and label handling see "127.0.0.1" (and, for
# names, "例え。jp" -> "例え.jp").
#
# SCOPED TO THE HOST ONLY. A full-stop variant in the path/query/fragment
# is legitimate content (e.g. a path "/文書。pdf") and MUST NOT be rewritten, so
# only the authority span -- between the "//" that introduces it and the first
# "/", "?" or "#" -- is considered, and within it only the host-and-port part
# after the LAST "@" (where the WHATWG authority state splits userinfo off).
# Userinfo is not a domain: WHATWG percent-encodes it with the userinfo
# percent-encode set, so "a。b@h" keeps "a%E3%80%82b". A host that starts with
# "[" goes to the IPv6 parser, which has no UTS-46 step and fails on any code
# point but an ASCII hex digit, ":" or ".", so a bracketed literal is never
# mapped either (RURL-mkbfseqs). A variant full stop in the port fails the port
# state whether mapped or not. Opaque "scheme:foo" inputs (no "//") carry no
# authority and are left untouched; a backslash run has already been collapsed
# to "//" upstream, so this sees the normalized form. Runs ONLY under
# url_standard == "whatwg" (RFC 3986 has no UTS-46 mapping -- these bytes stay
# literal), and is a byte-for-byte no-op otherwise. Recognition only: a
# percent-encoded form ("%E3%80%82") is left as written here; the WHATWG host
# model maps it once the host is decoded (.apply_host_standard_model_vec()). No
# diagnostic is emitted -- this is ordinary WHATWG host normalization (like
# host-lowercasing), not a lossy repair of malformed input.
.map_whatwg_domain_separators_vec <- function(url, url_standard) {
  no_op <- list(url = url)
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }
  # A row led by U+FEFF has neither a scheme nor "//": the byte-mode capture
  # reads the mark as data and finds neither (ADR 0019), so the mask the
  # stringi match needed is gone (RURL-dddxutjf).
  m <- .icu_capture_to_end(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*:)?(//)([^/?#]*)"
  )
  authority <- m[, 4L]
  eligible <- !is.na(authority) &
    .bytes_detect(authority, .UTS46_FULL_STOP_VARIANTS)
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }
  # Every span below is located in and cut from `url` itself, which starts with
  # the scheme or "//", in bytes from locator to cut (ADR 0019): the stringi
  # fixed locators and stri_sub() read past a U+FEFF that starts their input,
  # so splitting `authority` once dropped a mark that starts the userinfo
  # (RURL-exdlurql). The authority ends at the first "/", "?" or "#"; userinfo
  # runs through the LAST "@" in it, where `.ICU_THROUGH_LAST_AT` starts in
  # the authority's bytes. A greedy `[^/?#]*@` backtracked byte by byte from
  # the end of the authority and hit PCRE's match limit on a long one
  # (RURL-yqmoztoj); the possessive `(?:[^/?#@]*+@)++` that replaced it still
  # counted a step per "@" and hit it on ~1e7 of them, and a fixed
  # `gregexpr()` is quadratic in the number of matches (RURL-atlidhyh). A
  # negated class, not `.*`: ICU `.` stops at U+000B, U+000C, U+0085, U+2028
  # and U+2029, which would leave the host unsplit for a later step to map.
  prefix <- "^(?:[a-zA-Z][a-zA-Z0-9+.-]*:)?//"
  prefix_end <- .bytes_locate(url, prefix)[, "end"]
  authority_end <- .bytes_locate(url, paste0(prefix, "[^/?#]*+"))[, "end"]
  userinfo_at <- .bytes_locate(
    .bytes_cut(url, prefix_end + 1L, authority_end), .ICU_THROUGH_LAST_AT
  )[, "start"]
  host_start <- prefix_end + ifelse(is.na(userinfo_at), 0L, userinfo_at) + 1L
  before_host <- .bytes_cut(url, 1L, host_start - 1L)
  host_port <- .bytes_cut(url, host_start, authority_end)
  after_authority <- .bytes_cut(url, authority_end + 1L, .Machine$integer.max)
  eligible <- eligible & !startsWith(host_port, "[") &
    .bytes_detect(host_port, .UTS46_FULL_STOP_VARIANTS)
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }
  mapped_host_port <- .bytes_replace(
    host_port, .UTS46_FULL_STOP_VARIANTS, ".", all = TRUE
  )
  url_out <- url
  url_out[eligible] <- paste0(
    before_host[eligible], mapped_host_port[eligible], after_authority[eligible]
  )
  no_op$url <- url_out
  no_op
}

# WHATWG userinfo charset acceptance (RURL-micalqvh, half (a)). The parser
# refuses an authority whose userinfo carries any of 30 ASCII code points --
# SPACE (0x20), the C0 controls (0x00-0x1F) and DEL (0x7F) -- so
# `http://a b@host/` errors even though WHATWG parses it and keeps the host.
# Every other userinfo byte the parser already accepts, including non-ASCII and
# existing percent-triplets, so the set is exactly those 30 and no wider. Each
# is a member of the WHATWG userinfo percent-encode set, i.e. the
# percent-encoded form written here IS the spelling WHATWG stores; the parser
# is asked
# to parse with `decode = FALSE`, so no restore step is needed (contrast the ADR
# 0009 host shim, which substitutes non-spec filler and must restore). "%" is
# not in the set, so an already-encoded userinfo (`%25DOMAIN`, `u%40ser`) is
# never double-encoded.
#
# THIS IS NOT COMPENSATION, which is why it survived RURL-ezhzpkhg deletion 3
# while the repeated-"@" repair that used to share this function did not. That
# repair (RURL-zqhgezuq) compensated for a parse seam that rejected a second
# "@" -- the C-03 disposition in design/work/url-v3/contracts/
# validation-intervention-contract.md owns that history. Splitting at the LAST
# "@" is what the WHATWG authority state does, so the behavior moved into the
# parser as `.parse_web_url_one(last_at_userinfo = TRUE)`. What is left
# here changes the SPELLING of a userinfo the parser would otherwise refuse, and
# the spelling it writes is the one WHATWG stores -- a normalization the parser
# cannot infer, because `rfc3986` must keep the same bytes source-preserving.
#
# GATED ON `.is_whatwg()` EXPLICITLY, for exactly that reason: `rfc3986` has no
# userinfo production for a space or a control byte.
.encode_userinfo_charset_vec <- function(url, url_standard) {
  no_op <- list(url = url)
  if (!.is_whatwg(url_standard)) {
    return(no_op)
  }

  m <- .icu_capture_to_end(
    url, "^([a-zA-Z][a-zA-Z0-9+.-]*:)(//)([^/?#]*)"
  )
  authority <- m[, 4L]
  # A row led by U+FEFF has no scheme, so the byte-mode capture finds none
  # and the row is left as written (ADR 0019); the mask the stringi match
  # needed (RURL-opwjlnla) is gone (RURL-dddxutjf). The pipeline never hands
  # this helper such a row anyway: Phase 1 prefixes "http://" to every row
  # led by the mark (`add_http`).
  has_at <- !is.na(authority) &
    .bytes_detect(authority, "@", fixed = TRUE)
  has_at[is.na(has_at)] <- FALSE

  # Any of the 30 parser-refused code points inside the userinfo span
  # (everything before the LAST "@"). The first match of `@[^@]*+\z` starts at
  # the last "@", so `sub()` cuts what stringi's replace-last did. The run is
  # possessive: a greedy one gave back the bytes after each earlier "@" one by
  # one and hit PCRE's match limit on a long row (RURL-atlidhyh).
  eligible <- rep(FALSE, length(url))
  if (any(has_at)) {
    userinfo_span <- .bytes_replace(authority[has_at], "@[^@]*+\\z", "")
    hit <- .bytes_detect(userinfo_span, "[\\x00-\\x20\\x7f]")
    hit[is.na(hit)] <- FALSE
    eligible[has_at] <- hit
  }

  if (!any(eligible)) {
    return(no_op)
  }

  repaired <- vapply(which(eligible), function(i) {
    a <- authority[i]
    # Byte-indexed for the same reason as the shim's slice (RURL-kmpnbvdl):
    # `authority` is a capture, so it is declared UTF-8 no matter what
    # octets it holds, and `substr()` threw on `http://a@@<80>b/`. The last
    # "@" is where `.ICU_THROUGH_LAST_AT` starts. (`.last_byte_index()` is
    # linear too since RURL-rweowcfz; this one regex was the RURL-atlidhyh
    # fix and stays.)
    last <- regexpr(.ICU_THROUGH_LAST_AT, a, perl = TRUE, useBytes = TRUE)[1L]
    paste0(
      .percent_encode_userinfo_charset(.byte_substring(a, 1L, last - 1L)),
      .byte_substring(a, last)
    )
  }, character(1), USE.NAMES = FALSE)

  url_out <- url
  url_out[eligible] <- paste0(
    m[eligible, 2L], m[eligible, 3L], repaired, m[eligible, 5L]
  )
  no_op$url <- url_out
  no_op
}

# Percent-encode the 30 ASCII code points refused in a userinfo: SPACE,
# the C0 controls and DEL. U+0000 cannot occur in an R string (and
# `rawToChar(as.raw(0L))` is ""), so the literal table covers 0x01-0x20 and
# 0x7F; the regex above still names the full 0x00-0x20 range. "%" is absent from
# the table, so the substitution is idempotent over already-encoded input.
# The replaces run in byte mode (ADR 0019), so a userinfo led by U+FEFF keeps
# the mark. The stringi fixed replace read past it and lost it
# (RURL-rfgbozdr), so an "@" was spliced on first and cut off as a byte; the
# close-out removed the splice (RURL-dddxutjf).
.percent_encode_userinfo_charset <- function(userinfo) {
  codes <- c(seq.int(1L, 32L), 127L)
  chars <- vapply(codes, function(i) rawToChar(as.raw(i)), character(1))
  # One literal at a time, in table order, as `vectorize_all = FALSE` did. No
  # replacement holds a byte of the table, so the order cannot matter.
  out <- userinfo
  for (k in seq_along(codes)) {
    out <- .bytes_replace(
      out, chars[k], sprintf("%%%02X", codes[k]), all = TRUE, fixed = TRUE
    )
  }
  out[is.na(userinfo)] <- NA_character_
  out
}

# The WHATWG IPv4 canonicalization that used to run here is gone
# (RURL-ezhzpkhg deletion 4, the last of the five). It rewrote a WHATWG-valid
# IPv4 host inside the URL STRING before the parse, because the old engine
# refused WPT-valid forms such as `0x.0x.0`; the in-tree parser owns the address
# grammar directly, as `host_ipv4` (R/parse-web.R). Its companion
# `.parse_whatwg_ipv4_host()` / `.parse_whatwg_ipv4_number()` went with it,
# reconciled into `.web_ipv4_normalize(host, ipv4)` -- one function with a flag,
# so the three forms the two flavors disagree about are stated once instead of
# having to be rediscovered by diffing two near-identical normalizers. The
# WHATWG flavor has since left the tree altogether: it is
# `raddr::addr_whatwg()` behind `.whatwg_ipv4_read()` (RURL-cbrfphfr, ADR 0018's
# 2026-10-09 amendment).
#
# `.host_ends_in_number_vec()` STAYS: it is also the WHATWG host model's trigger
# in `.apply_host_standard_model_vec()`, which is where "a host that ends in a
# number must parse as an address" belongs.

# Decode percent-triplets in a WHATWG host just far enough for the host model to
# see the real code points after the parse has resolved the URL's structure. A
# malformed or NUL-containing sequence maps to a C0 sentinel so the WHATWG
# forbidden-host gate rejects the row; no successful output depends on that
# placeholder.
.whatwg_percent_decode_host <- function(host) {
  if (is.na(host) || !nzchar(host) ||
      !grepl("%[0-9A-Fa-f]{2}", host, perl = TRUE)) {
    return(host)
  }
  tryCatch(utils::URLdecode(host), error = function(e) "\u0001")
}

# Uppercase the hex digits of every percent-triplet, leaving all other
# characters alone (RFC 3986 section 6.2.2.1). Unlike
# `.rfc_unreserved_normalize` this does NOT decode unreserved triplets -- it is
# the case rule only, for callers that must not change which octets stay
# encoded.
.pct_hex_upper <- function(x) {
  .gsub_decodable("%([0-9a-f]{2})", "%\\U\\1", x, perl = TRUE)
}

# RFC 3986 scheme + path-rootless support for special schemes without `//`
# (RURL-pwsacxvo). In RFC 3986 section 3, an authority is present only when the
# scheme-specific part starts with a literal `//`; otherwise `http:example.com`
# is `scheme = "http"`, no authority, `path = "example.com"`. The web parser
# rejects
# these as malformed HTTP URLs, so selector mode records the components here and
# Stage A installs them directly instead of going through the web parser.
#
# This slice is intentionally limited to host-shaped path-rootless
# (`scheme:example.com[/...]`) for the http/https/ftp family that WHATWG treats
# as recoverable authority URLs. Other RFC-valid path-rootless strings (for
# example ones starting with `@` or `:`), single-slash (`scheme:/path`) forms,
# and file-state variants are left outside this change. Literal backslash
# remains inert/rejected under RFC 3986.
.rfc3986_path_rootless_vec <- function(url, url_standard) {
  n <- length(url)
  no_op <- list(
    is_path_rootless = rep(FALSE, n),
    scheme = rep(NA_character_, n),
    path = rep(NA_character_, n),
    query = rep(NA_character_, n),
    fragment = rep(NA_character_, n)
  )
  if (!identical(url_standard, "rfc3986")) {
    return(no_op)
  }

  m <- .icu_capture_to_end(url, "^([a-zA-Z][a-zA-Z0-9+.-]*):")
  scheme <- m[, 2L]
  rest <- m[, 3L]
  scheme_lower <- .ascii_tolower(scheme)
  first_segment <- .icu_cut_tail(rest)
  # `^[A-Za-z0-9._~-]+(\.[A-Za-z0-9._~-]+)+$`, read without its repeated
  # group, whose backtracking a many-label segment overflowed (RURL-rrumztrf):
  # every byte in the class up to ICU's end, and a dot with a class byte on
  # either side. The class holds ".", so one such dot is all the group asked.
  seg_class <- "[A-Za-z0-9._~-]"
  host_shaped_first_segment <-
    .bytes_detect(first_segment, paste0("^", seg_class, "++", .ICU_END)) &
    .bytes_detect(first_segment, paste0("(?<=", seg_class, ")\\.", seg_class))
  host_shaped_first_segment[is.na(host_shaped_first_segment)] <- FALSE
  # A row led by U+FEFF has no scheme (RURL-biunpazk): the byte-mode capture
  # finds none, so `rest` is NA there and the mask the stringi match needed is
  # gone (RURL-dddxutjf). A host-shaped first segment starts with a class
  # byte, so `rest` cannot start with "/", "\" or the mark on an eligible
  # row; the two prefix tests are base R.
  eligible <- !is.na(rest) &
    scheme_lower %in% .SPECIAL_AUTHORITY_SCHEMES &
    host_shaped_first_segment &
    rest != "" &
    !startsWith(rest, "/") &
    !startsWith(rest, "\\") &
    !.bytes_detect(rest, "\\", fixed = TRUE)
  eligible[is.na(eligible)] <- FALSE
  if (!any(eligible)) {
    return(no_op)
  }

  # Located and cut in BYTES (RURL-otrpfhmq). `rest` is a capture, marked
  # UTF-8 whatever its octets, and `nchar()`/`substr()` THROW `invalid
  # multibyte string` on one holding
  # invalid octets, which aborted the whole batch. The delimiters are ASCII, so
  # a byte cut yields the same substrings as a character cut on every decodable
  # element; an undecodable one is now split too, and the uniform RFC 3986
  # gate rejects it (RURL-kmpnbvdl).
  split <- lapply(rest[eligible], function(x) {
    hash <- .first_byte_index(x, "#")
    qmark <- .first_byte_index(x, "?")
    n_bytes <- nchar(x, type = "bytes")

    end_path <- n_bytes
    if (qmark > 0L) {
      end_path <- min(end_path, qmark - 1L)
    }
    if (hash > 0L) {
      end_path <- min(end_path, hash - 1L)
    }
    path <- if (end_path > 0L) .byte_substring(x, 1L, end_path) else ""

    query <- NA_character_
    if (qmark > 0L && (hash == 0L || qmark < hash)) {
      query_end <- if (hash > 0L) hash - 1L else n_bytes
      query <- .byte_substring(x, qmark + 1L, query_end)
    }

    fragment <- NA_character_
    if (hash > 0L) {
      fragment <- .byte_substring(x, hash + 1L, n_bytes)
    }

    c(path = path, query = query, fragment = fragment)
  })
  parts <- do.call(rbind, split)

  no_op$is_path_rootless[eligible] <- TRUE
  no_op$scheme[eligible] <- scheme[eligible]
  no_op$path[eligible] <- parts[, "path"]
  no_op$query[eligible] <- .blank_to_na(parts[, "query"])
  no_op$fragment[eligible] <- .blank_to_na(parts[, "fragment"])
  no_op
}

# Split one `file:` rest into path, query and fragment, located and cut in
# BYTES (ADR 0019), as the RFC 3986 rootless split is. `rest` is a byte-mode
# capture, declared UTF-8 whatever its octets, and `nchar()`/`substr()` threw
# `invalid multibyte string` on one holding invalid octets, which aborted the
# whole vector (RURL-yqmoztoj). The delimiters are ASCII, so a byte cut gives
# the same substrings as a character cut on every decodable rest.
.whatwg_file_split_rest <- function(rest) {
  hash <- .first_byte_index(rest, "#")
  qmark <- .first_byte_index(rest, "?")
  n_bytes <- nchar(rest, type = "bytes")

  end_path <- n_bytes
  if (qmark > 0L) {
    end_path <- min(end_path, qmark - 1L)
  }
  if (hash > 0L) {
    end_path <- min(end_path, hash - 1L)
  }
  path <- if (end_path > 0L) .bytes_cut(rest, 1L, end_path) else ""

  query <- NA_character_
  if (qmark > 0L && (hash == 0L || qmark < hash)) {
    query_end <- if (hash > 0L) hash - 1L else n_bytes
    query <- .bytes_cut(rest, qmark + 1L, query_end)
  }

  fragment <- NA_character_
  if (hash > 0L) {
    fragment <- .bytes_cut(rest, hash + 1L, n_bytes)
  }

  c(path = path, query = query, fragment = fragment)
}

# WHATWG domain to ASCII (UTS #46 ToASCII with CheckHyphens, UseSTD3ASCIIRules
# and VerifyDnsLength false), NA on failure. The standard fails it twice over:
# when UTS #46 reports an error, and when "result is the empty string". Up to
# punycoder 1.3.0 a host that maps to nothing (a soft hyphen, U+00AD) came back
# NA, because the all-relaxed call rejected the empty label. Later punycoder
# keeps an empty label without VerifyDnsLength, as UTS #46 section 4.2 step 4
# says, and returns "", so the empty result is folded into NA here and every
# caller keeps one failure test (RURL-tsmevksk). A non-empty result with empty
# labels, such as ".", is not the empty string and passes through.
.whatwg_domain_to_ascii <- function(host) {
  out <- punycoder::host_normalize(
    host, check_hyphens = FALSE, use_std3 = FALSE,
    verify_dns_length = FALSE
  )
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

# The WHATWG `file:` host, which is NEVER null (RURL-uhwivndf). WHATWG's "file
# state" sets url's host to the EMPTY STRING before any authority is read, and
# nothing in the file host state can put it back to null: `localhost` is
# explicitly mapped to the empty string, not dropped. So this returns "" -- an
# empty, PRESENT host -- where it used to return NA, and NA is now reserved for
# "this row did not parse". That distinction is what lets the serializer emit
# the `//` the standard requires for every `file:` URL.
.whatwg_file_normalize_host <- function(host) {
  if (is.na(host) || host == "") {
    return("")
  }
  # Base R, not stri_startswith_fixed(), which reads past a U+FEFF that
  # starts `host`: "<U+FEFF>[::1]" is a domain, and fails (RURL-azcvukyh).
  if (startsWith(host, "[")) {
    return(.whatwg_file_ipv6_host(host))
  }

  # The host parser percent-decodes the host, and a "%" not followed by two hex
  # digits survives as a literal "%", a forbidden domain code point, so any
  # such "%" fails the host (RURL-zkirywwb). `.web_host_percent_decode()`
  # returns NULL for exactly those hosts, and for a decoded NUL, which is
  # forbidden too. `utils::URLdecode()`, used here before, dropped a "%" at the
  # end of the host, so `file://a%41%/p` parsed as host `aa`.
  #
  # The decoder rebuilds the string with `rawToChar()`, so the decoded octets
  # come back marked "unknown" (native) -- an INPUT-side gap the host
  # chokepoint in `.parse_urls_vec()` cannot cover, because the WHATWG `file:`
  # parser is in-tree and produces this host itself. Under `LC_ALL=C` those
  # bytes are then read as native characters, so `file://a%C2%ADb/p` (soft
  # hyphen) missed the UTS-46 mapping that `file://a<U+00AD>b/p` gets and the
  # row was rejected. `.mark_host_utf8()` DECLARES the decoded octets UTF-8
  # (`Encoding<-`; never `enc2utf8()`, which would transcode from the session
  # locale) so the forbidden-code-point gate and `host_normalize()` below see
  # the same host in every locale.
  decoded <- host
  if (grepl("%", host, fixed = TRUE)) {
    decoded <- .web_host_percent_decode(host)
    if (is.null(decoded)) {
      return("\u0001")
    }
    decoded <- .mark_host_utf8(decoded)
  }
  # Byte mode, `perl = TRUE, useBytes = TRUE` (ADR 0019), base R rather than
  # `.bytes_detect()`: this runs once per `file:` host, inside a vapply, and
  # the pattern is a constant ASCII class. `decoded` is declared UTF-8 above.
  if (grepl(.WHATWG_FORBIDDEN_HOST_CP_BYTES, decoded, perl = TRUE,
            useBytes = TRUE)) {
    return("\u0001")
  }
  # On failure the decoded host is kept; the host model
  # (`.apply_host_standard_model_vec()`) maps it again and fails the row.
  normalized <- .whatwg_domain_to_ascii(decoded)
  if (!is.na(normalized)) {
    decoded <- normalized
  }
  if (identical(.ascii_tolower(decoded), "localhost")) {
    return("")
  }
  decoded
}

# A `file:` host that starts with "[" (RURL-ohtwkdgi). The host parser sends
# it to the IPv6 parser: "If input does not end with U+005D (]), IPv6-unclosed
# validation error, return failure", and the text between the brackets must
# pass the IPv6 parser (WHATWG URL Standard, host parsing, IPv6 parser).
# `.whatwg_file_normalize_host()` used to return such a host unchecked, and the
# host model behind it reads any bracketed hex-and-colon run as an IP literal
# and only reserializes it, so `[1:]`, `[1::2::3]`, `[:::]` and `[12345::]`
# parsed as written. The check is the one the `http:` route runs,
# `.web_ipv6_serialize()` (NULL on an invalid literal). A failure returns the
# "\u0001" marker the normalizer's other failures use, which the host model
# rejects. A valid host goes on as written, not in the libcurl spelling that
# function returns, and the host model's WHATWG IPv6 serializer writes it.
.whatwg_file_ipv6_host <- function(host) {
  hb <- .web_bytes(host)
  if (hb[length(hb)] != 0x5DL ||
      is.null(.web_ipv6_serialize(.web_chr(hb[-c(1L, length(hb))])))) {
    return("\u0001")
  }
  host
}

.whatwg_file_drive_path <- function(path) {
  .bytes_replace(
    path, paste0("^/([A-Za-z])\\|(?=/|", .ICU_END, ")"), "/\\1:"
  )
}

# WHATWG "path state" for a `file:` path rooted at a normalized Windows drive
# letter (RURL-msefniuz). The standard's `..` step calls "shorten a URL's
# path", which returns WITHOUT removing anything when "url's scheme is 'file',
# path's size is 1, and path[0] is a normalized Windows drive letter" -- so
# `file:///C:/../` is `file:///C:/`, and `file:///C:/a/../..` is `file:///C:/`.
# The dot-segment resolution every other row gets, Phase 3's
# `._remove_dot_segments_whatwg()` (`.normalize_path_vec()` below), is RFC 3986
# section 5.2.4's algorithm and knows no drive letter: it shortened past `C:`
# and produced `file:///`.
#
# Phase 3 receives the path but not the scheme, and `http://h/C:/..` MUST
# still shorten to `/` -- the drive-letter clause is `file:`-only -- so the
# rule lives here, in the one parser that only ever sees `file:` rows, and is
# scoped to the rows where section 5.2.4 gives the wrong answer: a path whose
# FIRST segment is a normalized drive letter and which carries a dot segment.
# Every other `file:` path still reaches Phase 3 with its dots intact, exactly
# as before. The resolver's `.whatwg_file_remove_dot_segments()` (R/resolve.R,
# the same "shorten" rule for a relative reference) does the walk; WHATWG's
# dot segments are ATOMS (`.`, `%2e`, case-insensitive; a double-dot is any two
# of them), and that routine reads only the literal spellings, so whole
# encoded dot segments are folded to literals first. Phase 3 then finds no dot
# segment in these rows and leaves them untouched.
.whatwg_file_shorten_drive_path <- function(path) {
  # Byte mode (ADR 0019): `%2e` either case is spelled `%2[eE]`, which loses
  # no fold (ICU folds nothing else to "e"), and `$` is ICU's (`.ICU_END`).
  end <- paste0("(?:/|", .ICU_END, ")")
  mask <- !is.na(path) &
    .bytes_detect(path, paste0("^/[A-Za-z]:(?=/|", .ICU_END, ")")) &
    .bytes_detect(path, paste0("/(?:\\.|%2[eE]){1,2}", end))
  if (!any(mask)) {
    return(path)
  }
  p <- path[mask]
  p <- .bytes_replace(p, paste0("(?<=/)(?:\\.|%2[eE]){2}(?=", end, ")"), "..",
                      all = TRUE)
  p <- .bytes_replace(p, paste0("(?<=/)%2[eE](?=", end, ")"), ".", all = TRUE)
  path[mask] <- vapply(
    p, .whatwg_file_remove_dot_segments, character(1), USE.NAMES = FALSE
  )
  path
}

.parse_whatwg_file_urls_vec <- function(url) {
  n <- length(url)
  out <- list(
    ok = rep(FALSE, n),
    host = rep(NA_character_, n),
    path = rep(NA_character_, n),
    query = rep(NA_character_, n),
    fragment = rep(NA_character_, n)
  )
  if (n == 0L) {
    return(out)
  }

  m <- .icu_capture_to_end(url, "^[Ff][Ii][Ll][Ee]:")
  rest <- m[, 2L]
  ok <- !is.na(rest)
  if (!any(ok)) {
    return(out)
  }

  split <- lapply(rest[ok], .whatwg_file_split_rest)
  parts <- do.call(rbind, split)
  file_path_raw <- parts[, "path"]
  # Byte mode (ADR 0019): a host-less path led by U+FEFF keeps the mark. The
  # stringi fixed replace read past it and lost it (RURL-rjpljsui), so those
  # rows took a spliced "/", which the close-out removed (RURL-dddxutjf).
  file_path <- .bytes_replace(file_path_raw, "\\", "/", all = TRUE,
                              fixed = TRUE)

  # Empty, not NA: WHATWG's file state gives every `file:` URL a non-null host,
  # and the host-less forms (`file:`, `file:/p`, `file:C|/m/`) carry the empty
  # string. See .whatwg_file_normalize_host().
  host <- rep("", length(file_path))
  path <- file_path
  has_authority <- .bytes_detect(file_path_raw, "^[/\\\\]{2}")
  has_authority[is.na(has_authority)] <- FALSE
  if (any(has_authority)) {
    # Every span is located in and cut from the raw path, which starts with
    # the two-slash marker, in bytes from locator to cut (ADR 0019). Some
    # stringi functions read past a U+FEFF that starts their input, so cutting
    # the authority from the text after the marker, which can start with one,
    # once put the slash in the host and lost the path's first character
    # (RURL-azcvukyh). The authority holds no slash or backslash, so it needs
    # no rewrite.
    raw <- file_path_raw[has_authority]
    authority_end <- .bytes_locate(raw, "^[/\\\\]{2}[^/\\\\]*")[, "end"]
    slash <- .bytes_locate(raw, "^[/\\\\]{2}[^/\\\\]*[/\\\\]")[, "end"]
    authority <- .bytes_cut(raw, 3L, authority_end)
    auth_path <- ifelse(
      is.na(slash), "/", .bytes_cut(raw, slash, .Machine$integer.max)
    )
    auth_path <- .bytes_replace(auth_path, "\\", "/", all = TRUE,
                                fixed = TRUE)

    # WHATWG "file host state": a buffer that is a Windows drive letter -- an
    # ASCII alpha followed by `:` OR `|` -- is not a host at all; the state
    # falls through to "path state" with the buffer intact, and the host stays
    # the empty string the file state set. `file://C:/` is `file:///C:/`
    # exactly as `file://C|/` is (RURL-ufsltsit: the `:` spelling was missed,
    # so `file://C:/` was rejected outright).
    drive_authority <- .bytes_detect(
      authority, paste0("^[A-Za-z][:|]", .ICU_END)
    )
    drive_authority[is.na(drive_authority)] <- FALSE
    if (any(drive_authority)) {
      # The buffer IS the first path segment, so a slash-less `file://d:` has
      # the path `/d:` -- the "/" a real host would get from the path start
      # state is not appended, because the path state consumed the buffer.
      drive_tail <- ifelse(
        is.na(slash[drive_authority]), "", auth_path[drive_authority]
      )
      auth_path[drive_authority] <- paste0(
        "/",
        .bytes_cut(authority[drive_authority], 1L, 1L),
        ":",
        drive_tail
      )
      authority[drive_authority] <- ""
    }

    host[has_authority] <- vapply(
      authority, .whatwg_file_normalize_host, character(1), USE.NAMES = FALSE
    )
    path[has_authority] <- auth_path
  }

  no_authority <- !has_authority
  if (any(no_authority)) {
    no_auth_idx <- which(no_authority)
    path[no_auth_idx] <- ifelse(path[no_auth_idx] == "", "/", path[no_auth_idx])
    # Base startsWith(): stri_startswith_fixed() reads past a leading U+FEFF.
    needs_leading <- !startsWith(path[no_auth_idx], "/")
    needs_leading[is.na(needs_leading)] <- FALSE
    lead_idx <- no_auth_idx[needs_leading]
    path[lead_idx] <- paste0("/", path[lead_idx])
  }

  path <- .whatwg_file_drive_path(path)
  path <- .whatwg_file_shorten_drive_path(path)
  # No rule for an empty authority introduced by a backslash (RURL-vhuozous):
  # the path above is cut from the raw input, so it already counts the
  # separators after the authority, and the WHATWG path start state consumes
  # one of them whichever spelling it has. `file:\\` is `file:///` like
  # `file://`, and `file:\\\\` is `file:////` like `file:////`.

  out$ok[ok] <- TRUE
  out$host[ok] <- host
  out$path[ok] <- path
  out$query[ok] <- .blank_to_na(parts[, "query"])
  out$fragment[ok] <- .blank_to_na(parts[, "fragment"])
  out
}

# Bounded browser string fixer (RURL-jynceqrj, ADR 0012 Layer 6a; PRD Part 1).
# A deterministic single pass, gated on fixup_posture == "browser", that repairs
# the raw input string before any parsing. A byte-for-byte no-op for every other
# posture (returns `url` untouched), so the default parse path is unchanged.
#
# Steps (each feeds the next):
#   1. Outer C0/space trim -- strip leading/trailing U+0000..U+0020 from the
#      whole input (WHATWG "C0 control or space"). This trim has a whatwg-gated
#      sibling: WHATWG step 1's first half, in
#      .strip_whatwg_control_chars_vec() (RURL-yvxpanix), does the same trim for
#      every posture when url_standard == "whatwg". This one stays load-bearing
#      because it is the ONLY outer trim under url_standard = "rfc3986" / no
#      selector, where step 1 does not run at all.
#   2. `;`->`:` -- rewrite a leading `scheme;` to `scheme:` ONLY when the scheme
#      token is in the recognized-scheme set. A `;` after any other token is
#      left verbatim.
#   3. `://` insertion -- insert `://` after `scheme:` for a scheme in the
#      authority table that is missing its authority slashes, or one of them
#      (e.g. `http:example.com` and `http:/example.com` ->
#      `http://example.com`). Outside the set: verbatim.
#   3b. File-path guess -- scheme-less input that starts with exactly one `/`
#      becomes `file://` + input (`/Users/me/a.pdf` ->
#      `file:///Users/me/a.pdf`), as Chromium's SegmentURLInternal()
#      (components/url_formatter/url_fixer.cc) picks `file:` for a leading
#      separator on POSIX. `//` stays scheme-relative, counting a `/` or `\`
#      behind ASCII tab/LF/CR, which WHATWG step 1 removes next; `~` is not
#      expanded, as that needs the caller's home directory (RURL-anubcmpd).
#      This step invents a scheme, so it runs only where the infer seam would:
#      `scheme_policy = "infer"`, and only under `url_standard = "whatwg"`,
#      whose file state the fixed row is written for. It sits here, not at the
#      add_http seam, because the `file:` row detection below reads the string
#      before that seam runs.
#
# The recognized-scheme set is .WHATWG_SPECIAL_SCHEMES (referenced directly,
# not copied); the authority table is that set minus `file`. `ftps` is
# naturally excluded (it is not special under WHATWG), so `ftps;host` /
# `ftps:host` stay verbatim -- no special-casing. `file` is left out of step 3
# because the WHATWG file state sends a host-less `file:p` to the path state
# (`file:///p`); inserting `//` made `p` a host. Chromium agrees on POSIX:
# FixupURLInternal() hands an explicit `file:` to GURL unchanged, and
# DoParseFileUrl() (url/url_parse_file.cc) reads zero slashes as a local path
# (RURL-anubcmpd). For every other special scheme step 3 changes nothing the
# WHATWG special authority slashes state would not already do.
# Step 4 (fallback `http`) is NOT here: it is the existing scheme_policy =
# "infer" prepend seam (ADR 0012 D4).
.apply_browser_fixup_vec <- function(url, fixup_posture,
                                     url_standard = "whatwg",
                                     scheme_policy = "infer") {
  if (!identical(fixup_posture, "browser")) {
    return(url)
  }

  # Step 1: outer C0/space trim (U+0000..U+0020 at either end). NA passes
  # through unchanged. Byte mode (ADR 0019). The end is ICU's `$`, which the
  # trim has always used: the class takes every ASCII line terminator, but not
  # a final U+0085, U+2028 or U+2029, before which the run is still trimmed.
  url <- .bytes_replace(url, "^[\\x00-\\x20]+", "")
  url <- .bytes_replace(url, paste0(.C0_SPACE_RUN, .ICU_END), "")

  # Recognized-scheme set = .WHATWG_SPECIAL_SCHEMES; authority table =
  # .SPECIAL_AUTHORITY_SCHEMES, the same set minus `file`. Schemes are ASCII
  # case-insensitive (WHATWG URL Standard, scheme state; RFC 3986 section
  # 3.1), so each letter is a two-case class: byte mode does no
  # case-insensitive matching (ADR 0019, decision 2). The ICU `(?i)` this
  # replaced also folded U+017F, U+00DF and U+FB01 (`httpſ;`, `wß;`,
  # `ﬁle;`), which are not ASCII case variants of a scheme. The ordered
  # `;`/`:` delimiter after the alternation keeps a longer scheme (https) from
  # being truncated to a shorter prefix (http) and keeps non-scheme tokens
  # (httpx;) from matching.
  scheme_alt <- paste(
    .ascii_letter_classes(.WHATWG_SPECIAL_SCHEMES), collapse = "|"
  )
  authority_alt <- paste(
    .ascii_letter_classes(.SPECIAL_AUTHORITY_SCHEMES), collapse = "|"
  )

  # Step 2: leading `scheme;` -> `scheme:`, recognized schemes only. \1 keeps
  # the matched scheme's original case.
  url <- .bytes_replace(url, paste0("^(", scheme_alt, ");"), "\\1:")

  # Step 3: `scheme:` with fewer than two authority slashes -> `scheme://`,
  # authority-table schemes only. A single slash is COMPLETED to two, not
  # prefixed with two more (`http:/a` -> `http://a`, host `a`, as a browser
  # reads it): writing `http:///a` invented an empty authority, which the
  # `rfc3986` arm reads as such (RFC 3986 sec 3.2), so the row lost its host
  # (RURL-vmsmlflr). The `(?!/)` lookahead after the optional slash leaves an
  # already-slashed `scheme://` (incl. the step-2 output) and a longer run
  # (`scheme:///`) untouched. Under `whatwg` both spellings were the same URL
  # (the special authority slashes states skip every slash), so that arm does
  # not move.
  url <- .bytes_replace(
    url, paste0("^(", authority_alt, "):/?(?!/)"), "\\1://"
  )

  # Step 3b: a leading single `/` (not `//`, not `/\`, with tab/LF/CR between
  # them ignored) -> `file://` + input.
  if (identical(url_standard, "whatwg") && identical(scheme_policy, "infer")) {
    url <- .bytes_replace(
      url, "^(?=/(?![\\t\\n\\r]*[/\\\\]))", "file://"
    )
  }

  url
}

# The string the general route parses: the browser fixer's output, then WHATWG
# step 1's strip (RURL-lsgdeisl), the same two passes in the same order as at
# the top of .prepare_urls_for_parse_vec(). Stage A and Stage B both call this,
# so they agree on which rows are general-routed.
#
# The general route read the raw input until RURL-vmsmlflr, so a row the fixer
# rewrote was claimed on its raw spelling while the web route parsed the fixed
# one. Under `rfc3986`, where the general route owns `file:` (RFC 8089) and the
# odd-slash `http:` rows, `  file:///a/b` and `file;///a/b` fell to the web
# route, which read `a` as the host, and `file;p` failed; `http:a/b` kept its
# raw rootless path although step 3 had written `http://a/b`. A fixed row now
# parses as its fixed spelling typed directly. That includes `ws:`/`wss:`,
# which are not web-route schemes under `rfc3986` (`.general_parsed_mask()`):
# `  wss://h/x` now reads like `wss://h/x`, a generic URI whose reg-name gets
# no PSL split (ADR 0012 D2), where the web route used to parse the trimmed
# string as a special-scheme URL. The rules that must not reach a
# non-special scheme stay out: steps 2, 3 and 3b match special schemes only,
# step 1's trim is scheme-blind by definition (and under `whatwg` the strip
# already does it), and the special-scheme backslash rewrite is not applied.
# A byte-for-byte no-op unless fixup_posture == "browser", or a `//` row under
# `rfc3986` with "keep", "http" or "https" (below).
#
# Under `rfc3986`, a `//`-led row is also handed the prefix Phase 1 gives the
# web route: the `http:` stand-in under `scheme_relative_handling = "keep"`
# (RURL-ozpxupgq), and the assumed `http:` or `https:` under "http" or "https"
# (RURL-vubwgoxo). The general route's own rules then decide which of these
# rows the web route cannot read: `.rfc_odd_slash_run()` claims `///p` and
# `////p`, `.rfc_empty_authority_host()` claims `//`, `//u@/p` and `//:80/x`,
# and every `//host` row stays on the web route, byte-identical. The web route
# models the special-scheme authority, whose state skips a slash run, so
# `///p` read as host `p`, the reading of `//p`. RFC 3986 section 4.2 makes a
# network-path reference `"//" authority path-abempty`, and section 3.2's
# `reg-name` may be empty, so `///p` is an empty authority and the path `/p`.
# The general parser reads it that way: under `rfc3986` it is the generic
# syntax, in which the prefix is only a scheme name, never a special scheme.
# Under "keep" the row reports no scheme, as every "keep" row does (Phase 13);
# under "http" or "https" it reports the assumed scheme, so the row parses as
# its spelled `http:///p` or `https:///p` twin does. "error" rejects the row
# in Phase 1, and `whatwg` and the no-selector default read the slash run the
# special-scheme way, so none of them gets the prefix here.
.general_route_input_vec <- function(url, opts) {
  url <- .apply_browser_fixup_vec(
    url, opts$fixup_posture, opts$url_standard, opts$scheme_policy
  )
  url <- .strip_whatwg_control_chars_vec(url, opts$url_standard)$url
  srh <- opts$scheme_relative_handling
  if (identical(opts$url_standard, "rfc3986") &&
        isTRUE(srh %in% c("keep", "http", "https"))) {
    # The same test Phase 1 draws: a U+FEFF-led row is never scheme-relative.
    # Base `startsWith()` reads the mark as data, so it needs no mask.
    rel <- startsWith(url, "//")
    rel <- !is.na(rel) & rel
    # The prefix Phase 1 writes: "keep" parses behind `http:`.
    prefix <- if (identical(srh, "https")) "https:" else "http:"
    url[rel] <- paste0(prefix, url[rel])
  }
  url
}

# Phase 1 (vector): scheme detection, supported-scheme policy, the host-shape
# gate, and building the string handed to the parser. Returns the per-URL
# columns plus
# a logical `rejected` column marking rows the scalar pipeline returned NULL for
# (scheme-relative under "error" handling; a bare unsupported scheme under
# keep/none; a scheme-less input that is not host-shaped (D1); or an IP attempt
# that is not a canonical literal (D2/D3)) and a `scheme_less_userinfo` flag
# (D5). An input's supported scheme is decided against .SUPPORTED_SCHEMES.
.prepare_urls_for_parse_vec <- function(url,
                                       protocol_handling,
                                       scheme_relative_handling,
                                       url_standard = NULL,
                                       scheme_policy = "infer",
                                       scheme_acceptance = "web",
                                       fixup_posture = "none") {
  n <- length(url)

  # Bounded browser fixer (RURL-jynceqrj, ADR 0012 Layer 6a) runs BEFORE the
  # standards-required WHATWG preprocessing below. It is a deterministic single
  # pass that repairs the input STRING (outer C0/space trim, `;`->`:` and `://`
  # insertion for recognized special schemes, `file://` before a leading single
  # `/`) so the fixed string flows through the ordinary parse path. A
  # byte-for-byte no-op unless fixup_posture == "browser". The step-4 fallback
  # `http` prepend is NOT here: it is the existing scheme_policy = "infer" seam
  # at the add_http mask below, shared with the default posture (ADR 0012 D4 --
  # one prepend impl).
  url <- .apply_browser_fixup_vec(
    url, fixup_posture, url_standard, scheme_policy
  )

  # WHATWG input stripping (RURL-tyetpjym, RURL-yvxpanix) runs FIRST -- it is
  # the WHATWG parser's step 1 (trim leading/trailing C0-or-space, then remove
  # all ASCII tab/LF/CR), so every scheme/host
  # regex and the backslash recognizer below see the already-stripped string.
  # A no-op (byte-for-byte `url` unchanged) unless url_standard == "whatwg".
  cc <- .strip_whatwg_control_chars_vec(url, url_standard)
  url <- cc$url
  whatwg_file_input <- url

  # A row led by U+FEFF (after step 1 under WHATWG) has no scheme, so it is
  # never a scheme or scheme-relative row, and the rewrites below leave it
  # alone: WHATWG's scheme start state and RFC 3986 S3.1 both need an ASCII
  # alpha first, and NULL reads such a row like one led by any other code point
  # that cannot start a scheme. Every scheme and "//" test below runs in byte
  # mode or base R, which reads the mark as data (ADR 0019), so the
  # `.starts_with_bom()` mask the stringi matches needed (RURL-vhionecz,
  # RURL-biunpazk) is gone (RURL-dddxutjf).

  # WHATWG literal backslash recognition (RURL-ledntyab) runs next: for
  # eligible rows it rewrites "\" to "/" ahead of every scheme/host regex
  # below, so the rest of this function sees an already-normalized string.
  # A no-op (byte-for-byte `url` unchanged) unless url_standard == "whatwg".
  bs <- .rewrite_whatwg_backslashes_vec(url, url_standard)
  url <- bs$url

  # WHATWG UTS-46 alternative full-stop mapping (RURL-odsmwsxu) runs next: for
  # eligible rows it maps U+3002/U+FF0E/U+FF61 to ASCII "." in the HOST only
  # (not userinfo, not a bracketed IPv6 literal), so a Unicode-dot host coerces
  # through the existing IPv4/label handling (and IDN names normalize their
  # separators) instead of reaching the parser as an un-splittable literal. A
  # no-op unless url_standard == "whatwg".
  sep <- .map_whatwg_domain_separators_vec(url, url_standard)
  url <- sep$url

  url_lower <- .ascii_tolower(url)
  is_whatwg_file <- .is_whatwg(url_standard) &
    .bytes_detect(whatwg_file_input, "^[Ff][Ii][Ll][Ee]:")
  is_whatwg_file[is.na(is_whatwg_file)] <- FALSE

  # A scheme-bearing input is "allowed" only if its scheme is one rurl supports
  # (.SUPPORTED_SCHEMES). any(startsWith(., "<scheme>://")) per row, no loop.
  allowed_prefixes <- paste0(.SUPPORTED_SCHEMES, "://")
  original_has_allowed_scheme <- Reduce(
    `|`, lapply(allowed_prefixes, function(p) startsWith(url_lower, p))
  )
  original_has_allowed_scheme[is.na(original_has_allowed_scheme)] <- FALSE
  original_has_allowed_scheme <- original_has_allowed_scheme | is_whatwg_file

  # Only whether the scheme capture matched was read, so this is a detect.
  looks_like_protocol <- .bytes_detect(url, "^[a-zA-Z][a-zA-Z0-9+.-]*:")
  looks_like_protocol[is.na(looks_like_protocol)] <- FALSE
  rfc_rootless <- .rfc3986_path_rootless_vec(url, url_standard)
  original_has_allowed_scheme <-
    original_has_allowed_scheme | rfc_rootless$is_path_rootless

  has_scheme_slashes <- .bytes_detect(url, "^[a-zA-Z][a-zA-Z0-9+.-]*://")
  has_scheme_slashes[is.na(has_scheme_slashes)] <- FALSE

  is_scheme_relative <- startsWith(url, "//")
  is_scheme_relative[is.na(is_scheme_relative)] <- FALSE

  rejected <- rep(FALSE, n)
  if (scheme_relative_handling == "error") {
    rejected <- rejected | is_scheme_relative
  }
  if (scheme_relative_handling %in% c("http", "https")) {
    # Treat scheme-relative URLs as having an inferred scheme for the logic.
    looks_like_protocol[is_scheme_relative] <- TRUE
    original_has_allowed_scheme[is_scheme_relative] <- TRUE
  }

  looks_like_host_port <- rep(FALSE, n)
  maybe_host_port <- looks_like_protocol &
    !original_has_allowed_scheme &
    !has_scheme_slashes
  if (any(maybe_host_port)) {
    # The authority-part must be COLON-FREE. This is the same regex as
    # `.general_parsed_mask()`'s (R/parse-state.R), and the two must stay in
    # step: the carve-out exists for the scheme-LESS `example.com:8080` form,
    # which the scheme regex also matches (a dot is a legal scheme char). With
    # `[^/]+` the class ran greedily across colons, so `urn:ietf:rfc:2648` read
    # as "authority `urn:ietf:rfc`, port 2648", was flagged host:port, and was
    # diverted from the opaque parser to the web route that rejects `urn:`. Any
    # opaque payload ending in `:<digits>` was unparseable -- `urn:a:1`,
    # `sc:x:80`, `urn:isbn:0451450523` -- and only a trailing `?`/`#` rescued
    # it, by breaking the `($|/)` anchor. RURL-jnvtttfm repaired the sibling
    # site only; this is the second one (RURL-uafjkaas).
    #
    # Byte mode (ADR 0019), ending at the true end `\z`, the same anchor as
    # its twin in `.general_parsed_mask()`: ICU's `$` also took the point
    # before a final U+0085, U+2028 or U+2029, so `example.com:8080<U+2028>`
    # read as a host:port where `example.com:8080x` reads as a scheme and an
    # opaque path. A port is digits only (WHATWG URL Standard, port state;
    # RFC 3986 section 3.2.3), so those rows now read like the `x` row. The
    # digit run is possessive, the same rows as the twin's `[0-9]+`: a shorter
    # run is followed by a digit, neither end nor "/", and giving a long run
    # back byte by byte hit PCRE's match limit (RURL-atlidhyh).
    lhp <- .bytes_detect(url, "^[^/:]+:[0-9]++(?:\\z|/)")
    lhp[is.na(lhp)] <- FALSE
    looks_like_host_port[maybe_host_port] <- lhp[maybe_host_port]
  }

  # ADR 0012 D3 (Option B): the unsupported-scheme reject is armed on the
  # scheme-ACCEPTANCE axis, NOT on protocol_handling. Under "web" a
  # scheme-bearing token outside the curated allowlist is an error regardless
  # of how the scheme would be presented (keep/none/strip/http/https). Under
  # "general" this reject is suppressed -- general admits any valid scheme token
  # and defers shaping to L3/L4 (unimplemented here). The
  # `!looks_like_host_port` term stays so host:port inputs (`example.com:8080`,
  # which match the scheme regex) are never demoted -- Phase 1 recognizes and
  # parses them as host:port.
  if (scheme_acceptance == "web") {
    unsupported <- looks_like_protocol &
      !original_has_allowed_scheme &
      !looks_like_host_port
    rejected <- rejected | unsupported
  }

  # Host-shape classification (D1/D2/D5).
  cls <- .classify_input_host_vec(url)

  # D2/D3: an IP attempt that is not a canonical literal is a coerced/malformed
  # IP. Applies to every row (scheme-bearing too), so http://12345 and
  # http://192.168.010.1 are rejected as well as their scheme-less forms.
  #
  # url_standard host model (RURL-luwvkwhd): under a selector, a numeric IPv4
  # attempt is parsed faithfully instead of rejected -- RFC 3986 keeps it as a
  # reg-name, WHATWG coerces it (and only the post-parse model, not this gate,
  # decides WHATWG-fatal cases like 256.1.1.1 / 1.2.3.4.5). So the reject is
  # suppressed for the ipv4-ish attempts; non-canonical IPv6 (ipv6ish) stays
  # rejected in both modes. NULL selector keeps the historical hard reject.
  bad_ip <- cls$is_ipish & !cls$is_canonical_ip
  if (!is.null(url_standard)) {
    bad_ip <- bad_ip & !cls$is_ipv4ish
  }
  # A WHATWG `file:` row is never judged here (RURL-otfaotzq). The token above
  # is cut from text after "scheme://", so a host-less `file:C:/x` keeps its
  # scheme and reads as the IPv6 attempt "file:C", and the row failed before
  # the file state ran. That state sends a code point other than "/" or "\" to
  # the path state, where a colon is path data (`file:///C:/x`,
  # `file:///ab:cd`). A real authority goes to .parse_whatwg_file_urls_vec()
  # and the host model after it instead, which fail `file://[::1x]/` and
  # `file://1:2/` as the standard does; that parser does not yet validate
  # every bracketed literal (`file://[1::2::3]/` parses, RURL-ohtwkdgi).
  bad_ip <- bad_ip & !is_whatwg_file
  rejected <- rejected | bad_ip

  # Rows that get an inferred http:// (scheme-less, non-scheme-relative).
  add_http <- !is_scheme_relative &
    (!looks_like_protocol | looks_like_host_port)

  # D1: only fabricate a scheme when the token is host-shaped -- a canonical IP,
  # localhost, a dotted name, or an explicit host:port. Otherwise the input is
  # not a URL (asdfghjkl, "hello world", /relative/path, bare 12345). Under a
  # selector, a numeric IPv4 attempt (bare 2130706433, 0x7f000001) is host-like
  # too, so scheme-less numeric hosts reach the host model rather than D1.
  host_like <- cls$is_canonical_ip |
    cls$is_localhost |
    cls$is_dotted_name |
    looks_like_host_port
  if (!is.null(url_standard)) {
    host_like <- host_like | cls$is_ipv4ish
  }
  rejected <- rejected | (add_http & !host_like)

  # scheme_policy == "require" (RURL-vzgeurae): strict acceptance. Scheme
  # inference (the `add_http` path -- fabricating "http://" for host-shaped
  # scheme-less input) is rurl's browser-omnibox-style affordance, NOT a
  # WHATWG/RFC parser behavior. Under "require" the user opts out of it: any row
  # that WOULD receive a fabricated scheme is folded into the reject set instead
  # (a parse_status = "error"), so rurl behaves like a pure parser on this axis.
  # This is orthogonal to protocol_handling (which governs scheme
  # *presentation*, not *acceptance*) and url_standard (which governs
  # *interpretation*).
  # Scheme-relative //host input is deliberately NOT governed here -- it has its
  # own dedicated axis (scheme_relative_handling, incl. an "error" mode); this
  # covers only the add_http inference path. Default "infer" leaves `rejected`
  # untouched, so the historical behavior is byte-for-byte unchanged.
  if (scheme_policy == "require") {
    rejected <- rejected | add_http
  }

  # D5: scheme-less input carrying userinfo (user@example.com). Not rejected --
  # host/domain/tld/user still resolve -- but Stage B suppresses clean_url and
  # sets warning-userinfo. Only flagged for otherwise-parseable host-like rows.
  scheme_less_userinfo <- add_http & host_like & cls$has_userinfo & !rejected

  url_to_parse <- url
  if (scheme_relative_handling %in% c("http", "https")) {
    url_to_parse[is_scheme_relative] <- paste0(
      scheme_relative_handling, ":", url[is_scheme_relative]
    )
  } else {
    url_to_parse[is_scheme_relative] <- paste0(
      "http:", url[is_scheme_relative]
    )
  }
  url_to_parse[add_http] <- paste0("http://", url[add_http])

  # The string the uniform RFC 3986 gate judges (RURL-qrfrvmkg). Snapshotted
  # HERE, between the two kinds of rewrite this function performs:
  #   * BEFORE it, scheme inference (`add_http`) and scheme-relative expansion
  #     have run. Those are rurl's INPUT-ACCEPTANCE affordances, governed by
  #     `scheme_policy` / `scheme_relative_handling` (ADR 0010), not by
  #     `url_standard` -- so the gate must judge the input as accepted, or
  #     every scheme-less row would fail RFC 3986's mandatory `scheme ":"`
  #     and the grammar gate would silently re-implement scheme_policy.
  #   * AFTER it comes the one PARSER-COMPAT rewrite still standing (WHATWG
  #     userinfo charset acceptance). It exists to get a string past the web
  #     parser; judging its OUTPUT would
  #     let a
  #     repair launder an input the RFC has no production for -- exactly the
  #     "gated where rurl owns the parser" asymmetry this gate removes. The
  #     general/`file:` routes gate the source string, and this keeps the
  #     web route's subject identical to theirs.
  rfc_gate_input <- url_to_parse

  # WHATWG userinfo charset acceptance (RURL-micalqvh): rewrite the SPACE / C0 /
  # DEL bytes the parser refuses into the percent-encoded spelling WHATWG
  # stores. The repeated-"@" recovery that used to run here is now parser
  # behavior (RURL-ezhzpkhg deletion 3).
  at <- .encode_userinfo_charset_vec(url_to_parse, url_standard)
  url_to_parse <- at$url

  # `.encode_userinfo_charset_vec()` is now the ONLY rewrite left after the gate
  # snapshot, and it is a survivor rather than a leftover: it writes the
  # spelling WHATWG STORES for a userinfo byte, which the parser cannot infer
  # because `rfc3986` must stay source-preserving. The other four are gone --
  # the host-charset shim and the host percent-decode order (deletions 1 and 2,
  # ADR 0013 superseding ADR 0009), the excess-"@" authority split (3), the pqf
  # fallback (5) and the WHATWG IPv4 canonicalization (4). Each was a parser
  # property being decided in front of the parser, and each was gated by a
  # regex over the whole URL that narrowed the set it claimed to cover.

  list(
    url_to_parse = url_to_parse,
    # Subject of the uniform RFC 3986 gate (RURL-qrfrvmkg); see above.
    rfc_gate_input = rfc_gate_input,
    looks_like_protocol = looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    is_scheme_relative = is_scheme_relative,
    looks_like_host_port = looks_like_host_port,
    scheme_less_userinfo = scheme_less_userinfo,
    rejected = rejected,
    # Original host token + IPv4-attempt flag for the url_standard host model
    # (RURL-luwvkwhd), consumed by ._parse_stage_a_vec()'s model phase.
    input_host = cls$host_token,
    is_ipv4_attempt = cls$is_ipv4ish,
    # WHATWG backslash recognition (RURL-ledntyab): TRUE where a literal "\"
    # was actually reinterpreted as "/", consumed by the url_standard
    # diagnostics seam to emit `invalid-reverse-solidus`.
    backslash_rewritten = bs$backslash_rewritten,
    # WHATWG control-char strip (RURL-tyetpjym): TRUE where a tab/LF/CR was
    # removed, consumed by the diagnostics seam to emit `control-char-stripped`.
    control_char_stripped = cc$control_char_stripped,
    # WHATWG leading/trailing strip (RURL-yvxpanix, step 1's first half): TRUE
    # where a leading/trailing C0-control-or-space run was removed, consumed by
    # the same seam to emit `leading-trailing-stripped`.
    leading_trailing_stripped = cc$leading_trailing_stripped,
    rfc3986_path_rootless = rfc_rootless$is_path_rootless,
    rfc3986_path_rootless_scheme = rfc_rootless$scheme,
    rfc3986_path_rootless_path = rfc_rootless$path,
    rfc3986_path_rootless_query = rfc_rootless$query,
    rfc3986_path_rootless_fragment = rfc_rootless$fragment,
    whatwg_file = is_whatwg_file,
    whatwg_file_input = whatwg_file_input
  )
}

# Phase 1 (scalar wrapper): returns the per-URL list, or NULL when the URL must
# be rejected. Delegates to .prepare_urls_for_parse_vec().
.prepare_url_for_parse <- function(url,
                                  protocol_handling,
                                  scheme_relative_handling) {
  cols <- .prepare_urls_for_parse_vec(
    url, protocol_handling, scheme_relative_handling
  )
  if (cols$rejected[1L]) {
    return(NULL)
  }
  list(
    url_to_parse = cols$url_to_parse[1L],
    looks_like_protocol = cols$looks_like_protocol[1L],
    original_has_allowed_scheme = cols$original_has_allowed_scheme[1L],
    is_scheme_relative = cols$is_scheme_relative[1L],
    looks_like_host_port = cols$looks_like_host_port[1L],
    scheme_less_userinfo = cols$scheme_less_userinfo[1L],
    rfc3986_path_rootless = cols$rfc3986_path_rootless[1L]
  )
}


# Uppercase the two hex digits of every %XX percent-triplet, leaving the rest of
# the string untouched (`%2f` -> `%2F`). This keeps the historical no-selector
# `path_encoding = "keep"` behavior and the RFC 3986 section 6.2.2.1 case
# canonicalization path. Malformed `%` (not followed by two hex digits) is left
# as-is. `\U\1` (perl) uppercases just the captured pair.
#
# The `\U` case mapping is locale-proof here (the captured pair is hex by
# construction, and no locale case-maps `[0-9a-f]` outside `[0-9A-F]`), but the
# ENCODING MARK of `gsub(perl = TRUE)` is not: handed native (`"unknown"`)
# bytes it returns them still `"unknown"` under `LC_ALL=C` and marked `"UTF-8"`
# under a UTF-8 session -- identical bytes, divergent mark, on every path/query
# value that carries a non-ASCII byte. `.mark_host_utf8()` pins the mark the
# same way the input and output chokepoints do: it DECLARES with `Encoding<-`
# and never transcodes (`enc2utf8()` would re-decode in the session locale,
# which is the locale sensitivity being removed). Pure-ASCII values are
# unaffected -- R ignores a mark on them -- so this only touches the rows that
# diverged.
.uppercase_percent_hex <- function(x) {
  na <- is.na(x)
  if (all(na)) {
    return(x)
  }
  x[!na] <- .mark_host_utf8(
    gsub("%([0-9a-fA-F]{2})", "%\\U\\1", x[!na], perl = TRUE)
  )
  x
}

# Recover the raw request path from the prepared URL string (the exact bytes
# the parser was handed), rather than `parsed_web$path`. Its `$path` applies
# two normalizations even under `decode = FALSE`: it uppercases percent-hex and
# resolves RFC 3986 dot segments, INCLUDING percent-encoded ones
# (`/a/%2e%2e/b` -> `/b`). Both are profile/presentation decisions, so raw
# extraction stays byte-faithful and later phases apply the selected rules.
# Extracting from the input lets rurl own dot-segment resolution
# (`._remove_dot_segments`, literal `.`/`..` only, per RFC 3986 section 5.2.4 --
# an encoded `%2e` is NOT a dot segment), gated by `path_normalization`.
#
# Extraction (every parseable prepared row carries an explicit `scheme://`
# authority -- opaque/unsupported schemes are rejected upstream): strip the
# scheme, then the path is the run from the first literal `/` (after the
# authority, which cannot contain one) up to the first `?`/`#`. If the body
# starts with `/`, the prepared URL had an empty authority (`scheme:///...`) and
# the parser may have promoted the following segment into `$host`; in that shape
# the path from the prepared string is no longer the fetched path, so keep its
# coherent `$path` instead of duplicating the promoted host into the path. When
# the authority is followed directly by `?`, `#`, or end-of-string there is no
# path, so fall back to the parser's `$path` (the canonical "/" trailing-slash
# expects).
.extract_raw_path_vec <- function(prepared, engine_path,
                                  pqf_bytes = "reject") {
  out <- engine_path
  ok <- !is.na(prepared) & !is.na(engine_path)
  if (!any(ok)) {
    return(out)
  }
  # Positions are found in `body` but every cut is made in `p`, which starts
  # with the scheme, all in bytes (ADR 0019). Under stringi, stri_locate_*()
  # counted a U+FEFF that starts its input while stri_sub() read past it, so
  # cutting a body led by one (a host that starts with U+FEFF) took the path
  # one character off (RURL-vhionecz).
  p <- prepared[ok]
  prefix <- .bytes_locate(p, "^[a-zA-Z][a-zA-Z0-9+.-]*://")[, "end"]
  prefix[is.na(prefix)] <- 0L
  body <- .bytes_replace(p, "^[a-zA-Z][a-zA-Z0-9+.-]*://", "")
  first <- .bytes_locate(body, "[/?#]")[, "start"]
  empty_authority <- .bytes_cut(p, prefix + 1L, prefix + 1L) == "/"
  empty_authority[is.na(empty_authority)] <- FALSE
  has_path <- !is.na(first) &
    .bytes_cut(p, prefix + first, prefix + first) == "/" &
    !empty_authority
  has_path[is.na(has_path)] <- FALSE
  raw <- engine_path[ok]
  if (any(has_path)) {
    pp <- p[has_path]
    start <- prefix[has_path] + first[has_path]
    tail <- .bytes_cut(pp, start, .Machine$integer.max)
    stop_rel <- .bytes_locate(tail, "[?#]")[, "start"]
    # A path running to the end of the input is cut to the last byte. Under
    # stringi the bound was `to = -1L`, not `stri_length()`, which THROWS on an
    # element holding invalid octets and aborted the whole batch
    # (RURL-otrpfhmq); the byte layer is total over such elements.
    end <- start + stop_rel - 2L
    end[is.na(stop_rel)] <- .Machine$integer.max
    raw[has_path] <- .bytes_cut(pp, start, end)
  }
  # An octet that is not valid UTF-8 is percent-encoded here (RURL-yarkccco,
  # RUL-029), as the parser's component pass already encodes one in the query
  # and fragment, so under the no-selector default the row parses with
  # `/caf%E9` instead of failing (RURL-otrpfhmq). Stage B then reads a valid
  # string, the same one the input `/caf%E9` gives. `rfc3986` does not move:
  # its uniform grammar gate has already rejected every such row. `whatwg`
  # never has one: its input already reads the octet as U+FFFD. Rows off the
  # web route (`file:` and the general route) keep the undecodable-path
  # failure in `._parse_stage_a_vec()`.
  # The escape runs for every caller, ungated: no profile or `pqf_source` is in
  # scope here, and none is needed. It is observable ONLY under the NULL
  # profile, because the rfc3986 grammar gate rejects these rows before their
  # path is read and whatwg input already holds U+FFFD, never a raw ill-formed
  # octet (RUL-029). A profile that let such a row reach here would inherit
  # the escape; gate it here first.
  undecodable <- !is.na(raw) & !validUTF8(raw)
  if (any(undecodable)) {
    raw[undecodable] <- vapply(
      raw[undecodable], .web_escape_undecodable_bytes, character(1),
      USE.NAMES = FALSE
    )
  }
  # This slice bypasses the parser's component pass by design (dot segments must
  # survive to `path_normalization`), so under `pqf_bytes = "encode"` it is also
  # the one place a C0/SP/DEL byte could reach a public surface UNESCAPED --
  # while `query` and `fragment`, which come straight off the parser, are
  # escaped. That asymmetry is not a spelling preference; it would put a literal
  # space in `clean_url()` and a raw control byte in `path`.
  if (identical(pqf_bytes, "encode")) {
    raw <- vapply(raw, .web_escape_pqf_bytes, character(1), USE.NAMES = FALSE)
  }
  out[ok] <- raw
  out
}

# Phase 2b: pull the raw components used downstream out of the parse result.
# The web parser never decodes, so `parsed_web$query` is already
# the raw (percent-encoded) query string, so it is taken verbatim; downstream
# parsers split on raw "&"/"=" then decode per-pair. scheme/host as-is. The path
# is re-derived from the prepared input by .extract_raw_path_vec() (see there)
# so dot segments survive to `path_normalization`.
.extract_raw_components <- function(parsed_web, prepared,
                                    pqf_bytes = "reject") {
  list(
    scheme = .if_null(parsed_web$scheme, NA_character_),
    host = .if_null(parsed_web$host, NA_character_),
    path = .extract_raw_path_vec(
      prepared, .if_null(parsed_web$path, NA_character_), pqf_bytes
    ),
    # .blank_to_na(): present-but-empty query "" -> NA.
    query = .blank_to_na(.if_null(parsed_web$query, NA_character_))
  )
}

# Phase 3 (vector): path decoding, slash/dot normalization, index stripping,
# trailing-slash policy, and optional percent-encoding. The genuinely scalar
# steps (RFC 3986 dot-segment resolution, index stripping, segment encoding) run
# via vapply only over the rows that can change (a stri_detect mask), so real
# data pays for them on ~0 rows.
.normalize_path_vec <- function(raw_path,
                                path_encoding,
                                path_normalization,
                                index_page_handling,
                                trailing_slash_handling,
                                path_identity = "none") {
  path_work <- raw_path

  # Two ORTHOGONAL axes (ADR 0011), applied as independent, composing steps:
  #   1. path IDENTITY (`path_identity`) -- the url_standard profile's
  #      normalization of which percent-octets are canonicalized in the path a
  #      URL denotes (".rfc3986_unreserved" / ".whatwg_preserve"; "none" =
  #      default). Profile-internal; never a public argument.
  #   2. path PRESENTATION (`path_encoding`) -- the public keep/encode/decode
  #      readable-vs-browser rendering, which LAYERS on any identity mode.
  # Presentation is terminal: identity normalization and the cleaning policies
  # below must inspect the pre-presentation path, where an encoded separator is
  # data rather than structure. WHATWG `encode` is deliberately different from
  # the legacy encoder: the standard's path serializer preserves existing
  # percent spellings, so it never full-decodes before encoding.
  whatwg_encode <- path_encoding == "encode" &&
    identical(path_identity, ".whatwg_preserve")

  # Identity: apply the profile's path-identity normalization (independent of
  # the presentation step above).
  if (path_identity == ".rfc3986_unreserved") {
    # url_standard = "rfc3986" (RURL-gjltzwmp, PRD S6.1): decode ONLY
    # unreserved percent-encoded octets, and do it BEFORE dot-segment removal
    # -- ordering is normative, not incidental. Decoding first folds an
    # encoded dot (%2E / %2E%2E) to a literal "."/".." segment, which the
    # dot_segments step below then resolves via the existing literal matcher.
    # Reserved bytes (%2F, %3F, %23, ...) are never decoded here.
    mask <- !is.na(path_work) & .bytes_detect(path_work, "%", fixed = TRUE)
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], .rfc_unreserved_normalize, character(1),
        USE.NAMES = FALSE
      )
    }
  } else if (path_identity == ".whatwg_preserve" && !whatwg_encode) {
    # url_standard = "whatwg" (RURL-bbmuehsx, PRD S6.1): never decode or
    # canonicalize existing percent-triplets. Dot-segment resolution below uses
    # the encoded-dot-aware remover, so encoded dot segments (%2e/%2e%2e) still
    # resolve without a general decode.
    mask <- !is.na(path_work) & .bytes_detect(path_work, "%", fixed = TRUE)
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], .whatwg_preserve_normalize, character(1),
        USE.NAMES = FALSE
      )
    }
  } else if (path_identity == "none" && path_encoding == "keep") {
    # Preserve the historical no-selector `keep` contract after Stage A became
    # byte-faithful: percent-triplet hex case is canonicalized here instead of
    # during raw extraction.
    mask <- !is.na(path_work) & .bytes_detect(path_work, "%", fixed = TRUE)
    if (any(mask)) {
      path_work[mask] <- .uppercase_percent_hex(path_work[mask])
    }
  }

  # Slash collapsing.
  if (path_normalization %in% c("collapse_slashes", "both")) {
    mask <- !is.na(path_work) & nzchar(path_work)
    path_work[mask] <- gsub("/+", "/", path_work[mask], perl = TRUE)
  }

  # Dot-segment resolution (RFC 3986 S5.2.4), or -- under the "whatwg" path
  # profile (RURL-bbmuehsx) -- the encoded-dot-aware variant that recognizes
  # "%2e"/"%2E" alongside literal "." without a general path decode. Both
  # removers are identity for paths without a matching dot segment, so only
  # candidate rows are processed.
  if (path_normalization %in% c("dot_segments", "both")) {
    is_whatwg <- identical(path_identity, ".whatwg_preserve")
    dot_remover <- if (is_whatwg) {
      ._remove_dot_segments_whatwg
    } else {
      ._remove_dot_segments
    }
    # Byte mode (ADR 0019): `%2e` either case is `%2[eE]`, which loses no fold
    # (ICU folds nothing else to "e"), and `$` is ICU's (`.ICU_END`).
    detect_pattern <- if (is_whatwg) {
      paste0("(?:^|/)(?:\\.|%2[eE]){1,2}(?:/|", .ICU_END, ")")
    } else {
      paste0("(?:^|/)\\.{1,2}(?:/|", .ICU_END, ")")
    }
    mask <- !is.na(path_work) & nzchar(path_work) &
      .bytes_detect(path_work, detect_pattern)
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], dot_remover, character(1), USE.NAMES = FALSE
      )
    }
  }

  # Index/default page stripping (only where such a trailing segment appears).
  if (index_page_handling == "strip") {
    mask <- !is.na(path_work) & nzchar(path_work) &
      .bytes_detect(path_work, .INDEX_PAGE_TAIL)
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], ._strip_index_page, character(1), USE.NAMES = FALSE
      )
    }
  }

  # Trailing-slash policy (after normalization/index handling). Base R, and the
  # strip cuts the final "/" as a byte: stri_sub() reads past a U+FEFF that
  # starts its input while stri_length() counts it (see .starts_with_bom()),
  # so `\ufeff/a/` lost the mark and kept the slash, `/a/` (RURL-zxnxczsw).
  if (trailing_slash_handling == "strip") {
    mask <- !is.na(path_work) & nzchar(path_work) & path_work != "/" &
      endsWith(path_work, "/")
    path_work[mask] <- .byte_substring_vec(
      path_work[mask], 1L, .byte_length(path_work[mask]) - 1L
    )
  } else if (trailing_slash_handling == "keep") {
    mask <- !is.na(path_work) & nzchar(path_work) & path_work != "/" &
      !endsWith(path_work, "/")
    path_work[mask] <- paste0(path_work[mask], "/")
  }

  # Terminal path presentation. Decode only after identity normalization and
  # cleaning have decided which structural rules fire. Legacy `encode` still
  # means decode then segment-encode; the difference is that both presentation
  # operations now happen after index/trailing-slash handling.
  nul_kept <- logical(length(path_work))
  if (path_encoding %in% c("decode", "encode") && !whatwg_encode) {
    mask <- !is.na(path_work)
    if (any(mask)) {
      decoded <- tryCatch(
        .pct_unescape(path_work[mask]),
        error = function(e) NULL
      )
      if (!is.character(decoded) || length(decoded) != sum(mask)) {
        decoded <- vapply(
          path_work[mask],
          function(p) tryCatch(.pct_unescape(p), error = function(e) p),
          character(1),
          USE.NAMES = FALSE
        )
      }
      # A `%XX` that decodes to an octet outside well-formed UTF-8 (`%E9`, or
      # the triplet the raw-path cut writes for an undecodable octet under the
      # no-selector default, RUL-029) has no character to present. "decode"
      # wrote the raw octet into `path` and `clean_url`, a string stringi
      # downstream cannot read, so it keeps that octet as its uppercase
      # triplet; every other triplet in the path still decodes (RURL-yarkccco).
      # "encode" needs no mask: `._encode_path_segments()` splits such a path
      # in bytes and re-encodes the octet as the same triplet.
      if (path_encoding == "decode") {
        bad <- !is.na(decoded) & !validUTF8(decoded)
        if (any(bad)) {
          decoded[bad] <- vapply(
            decoded[bad], .web_escape_undecodable_bytes, character(1),
            USE.NAMES = FALSE
          )
        }
      } else {
        # The decode keeps a `%00` triplet as written (RURL-fpujlvoq), and
        # the segment encode below would escape its `%` to `%2500`. So a
        # path holding the triplet is decoded and encoded between its `%00`s,
        # and the triplets are put back between the pieces. `%00` has no hex
        # letter, so the fixed split finds every spelling of it.
        nul_kept[mask] <- grepl("%00", path_work[mask], fixed = TRUE,
                                useBytes = TRUE)
        if (any(nul_kept)) {
          decoded[nul_kept[mask]] <- vapply(
            path_work[nul_kept],
            function(p) {
              pieces <- strsplit(paste0(p, "/"), "%00", fixed = TRUE,
                                 useBytes = TRUE)[[1]]
              pieces[length(pieces)] <- sub("/$", "", pieces[length(pieces)],
                                            useBytes = TRUE)
              paste(
                vapply(.pct_unescape(pieces), ._encode_path_segments,
                       character(1), USE.NAMES = FALSE),
                collapse = "%00"
              )
            },
            character(1),
            USE.NAMES = FALSE
          )
        }
      }
      path_work[mask] <- decoded
    }
  }

  if (whatwg_encode) {
    path_work <- .whatwg_path_percent_encode(path_work)
  } else if (path_encoding == "encode") {
    mask <- !is.na(path_work) & !nul_kept
    if (any(mask)) {
      path_work[mask] <- vapply(
        path_work[mask], ._encode_path_segments, character(1), USE.NAMES = FALSE
      )
    }
  }

  path_work
}

# Phase 3 (scalar wrapper): delegates to .normalize_path_vec().
.normalize_path <- function(raw_path,
                            path_encoding,
                            path_normalization,
                            index_page_handling,
                            trailing_slash_handling,
                            path_identity = "none") {
  .normalize_path_vec(
    raw_path, path_encoding, path_normalization,
    index_page_handling, trailing_slash_handling,
    path_identity = path_identity
  )
}

# Phase 4 (vector): resolve the final scheme according to protocol policy.
.derive_final_scheme_vec <- function(protocol_handling,
                                     looks_like_protocol,
                                     raw_scheme) {
  n <- length(looks_like_protocol)
  switch(protocol_handling,
    none = ifelse(looks_like_protocol, raw_scheme, NA_character_),
    strip = rep(NA_character_, n),
    http = rep("http", n),
    https = rep("https", n),
    keep = raw_scheme
  )
}

# Phase 4 (scalar wrapper): delegates to .derive_final_scheme_vec().
.derive_final_scheme <- function(protocol_handling,
                                 looks_like_protocol,
                                 raw_scheme) {
  .derive_final_scheme_vec(protocol_handling, looks_like_protocol, raw_scheme)
}

# Phase 5 (vector): detect whether each host is an IP literal (IPv4 or IPv6).
# NA/"" hosts are FALSE; IPv4 requires four dot-separated CANONICAL octets --
# 0..255 with NO leading zeros (D3: a leading zero is octal in inet_aton, so
# "192.168.010.1" silently means "192.168.8.1"; rurl refuses to guess and treats
# zero-padded octets as non-IP); IPv6 requires balanced brackets and either a
# valid embedded canonical dotted-quad tail or a conservative hex/colon match.
# This is the single strict IP validator, reused by the Phase-1 host-shape gate
# (.classify_input_host_vec) against the input token.
.detect_ip_host_vec <- function(raw_host) {
  n <- length(raw_host)
  valid_h <- !is.na(raw_host) & raw_host != ""

  # IPv4: four canonical octets (`.ICU_IPV4_CANONICAL`).
  ipv4 <- rep(FALSE, n)
  # Byte mode (ADR 0019) with ICU's `$` (`.ICU_END`), so a host that ends in
  # a final line terminator reads as it did.
  match4 <- .const_detect(raw_host, .ICU_IPV4_CANONICAL)
  match4[is.na(match4)] <- FALSE
  ipv4 <- valid_h & match4

  # IPv6.
  ipv6 <- rep(FALSE, n)
  has_open <- .bytes_detect(raw_host, "[", fixed = TRUE)
  has_close <- .bytes_detect(raw_host, "]", fixed = TRUE)
  balanced <- valid_h & (has_open == has_close)
  balanced[is.na(balanced)] <- FALSE

  # A detector, not stri_match_first_regex(), which reads past a U+FEFF that
  # starts its input (see .starts_with_bom()) and so read an address in
  # a mark-led host (RURL-opwjlnla); byte mode counts the mark.
  tail_match <- .const_detect(raw_host, .ICU_IPV6_IPV4_TAIL)
  tail_match[is.na(tail_match)] <- FALSE
  # The tail regex already enforces canonical octets, so a match is sufficient.
  has_tail <- balanced & tail_match
  ipv6[has_tail] <- TRUE

  # Conservative check for balanced hosts without an embedded dotted-quad tail.
  no_tail <- balanced & !has_tail
  if (any(no_tail)) {
    hex_shape <- .const_detect(raw_host, .ICU_IPV6_HEX_SHAPE)
    hex_shape[is.na(hex_shape)] <- FALSE
    has_colon <- .bytes_detect(raw_host, ":", fixed = TRUE)
    has_colon[is.na(has_colon)] <- FALSE
    ipv6[no_tail] <- (hex_shape & has_colon)[no_tail]
  }

  ipv4 | ipv6
}

# Phase 5 (scalar wrapper): delegates to .detect_ip_host_vec().
.detect_ip_host <- function(raw_host) {
  .detect_ip_host_vec(raw_host)
}

# Phase 5b helper (vector): the WHATWG "ends in a number" checker
# (https://url.spec.whatwg.org/#ends-in-a-number-checker) -- the trigger that
# forces a host through the WHATWG IPv4 parser. Drops a single trailing dot,
# then returns TRUE when the FINAL label is a decimal integer (all ASCII digits,
# so leading-zero "09"/"08" count) or a hex literal ("0x"/"0X" + hex digits).
# Those are exactly the labels the IPv4 number parser accepts, so a TRUE host
# MUST parse to a valid IPv4 address or the whole host parse fails -- there is
# no reg-name fallback. This is broader than the all-numeric-parts is_ipv4ish
# flag: it also fires on mixed reg-name/number hosts (foo.09), hex/octal final
# labels (foo.0x4), trailing-dot forms (1.2.3.08.), and >4-part hosts the parser
# leaves literal (0x1.2.3.4.5.). NA/"" hosts are FALSE.
.host_ends_in_number_vec <- function(host) {
  n <- length(host)
  ok <- !is.na(host) & host != ""
  if (!any(ok)) {
    return(rep(FALSE, n))
  }
  # Drop one trailing "." (a single empty final part), then test the last
  # label. Byte mode (ADR 0019), keeping ICU's `$` and `.`: a final line
  # terminator stays after the trimmed dot. This was the ICU cut `^.*\.`
  # (through the last dot before the first terminator) and then the label
  # test `^[0-9]+$` or `^0[xX][0-9a-fA-F]*$`. Neither class takes a
  # terminator, so a host with an inner one was never a number; on any other
  # host the label after its last dot, or the whole host, is the one cut.
  # Read so, it is two linear tests, with no repeated group to overflow on a
  # host of millions of labels.
  trimmed <- .const_replace(host, .ICU_TRAILING_DOT, "")
  num <- .const_detect(trimmed, .ICU_NUMBER_LAST_LABEL)
  num[is.na(num)] <- FALSE
  if (any(num)) {
    num[num] <- !.const_detect(trimmed[num], .ICU_INNER_TERMINATOR)
  }
  ok & num
}

# Whether each host is the ICU `^\[.*\]$`: a "[", then no terminator but a
# final one, with a "]" before it. Byte mode, `perl = TRUE, useBytes = TRUE`
# (ADR 0019), with ICU's `.` and `$`. Base R rather than `.bytes_detect()`:
# this runs once per bracketed host, inside a vapply. NA is FALSE.
.ipv6_bracketed_shape <- function(host) {
  !is.na(host) & startsWith(host, "[") &
    grepl(.IPV6_BRACKETED_SHAPE, host, perl = TRUE, useBytes = TRUE) &
    !grepl(.ICU_INNER_TERMINATOR, host, perl = TRUE, useBytes = TRUE)
}

# Phase 5b helper (scalar): WHATWG IPv6 serializer for bracketed literals. The
# WHATWG host parser stores IPv6 as eight 16-bit pieces; dotted-quad tails are
# folded into two pieces before serialization, and the longest zero run is
# compressed (`[::127.0.0.1]` -> `[::7f00:1]`). Invalid inputs return unchanged,
# and the host model behind this makes no IPv6 validity decision: a route that
# builds a bracketed host must validate it first, as `.web_parse_host()` and
# `.whatwg_file_ipv6_host()` do (RURL-ohtwkdgi).
.serialize_whatwg_ipv6_host <- function(host) {
  if (!.ipv6_bracketed_shape(host)) {
    return(host)
  }

  # The shape test lets one final line terminator follow the "]". The stringi
  # cut `stri_sub(host, 2L, -2L)` then dropped only the terminator, `inner`
  # kept the "]", no hextet parsed and the host came back unchanged, as it
  # does here directly. Every other row ends in "]" and holds no terminator,
  # so `inner` is bytes 2 to n - 1 (ADR 0019).
  if (!endsWith(host, "]")) {
    return(host)
  }
  inner <- .bytes_cut(host, 2L, nchar(host, type = "bytes") - 1L)
  # The test runs base R on a constant of this file, as `.const_detect()`
  # does. `colon` is the byte length of the head through the last ":".
  colon <- .last_byte_index(inner, ":")
  quad <- if (colon > 0L) {
    .bytes_cut(inner, colon + 1L, nchar(inner, type = "bytes"))
  } else {
    ""
  }
  if (colon > 0L &&
        grepl(.IPV6_QUAD_TAIL, quad, perl = TRUE, useBytes = TRUE)) {
    quad <- strsplit(quad, ".", fixed = TRUE)[[1L]]
    octets <- suppressWarnings(as.integer(quad))
    if (length(octets) != 4L || anyNA(octets) ||
        any(octets < 0L | octets > 255L)) {
      return(host)
    }
    inner <- paste0(
      .bytes_cut(inner, 1L, colon),
      sprintf("%x", octets[1L] * 256L + octets[2L]), ":",
      sprintf("%x", octets[3L] * 256L + octets[4L])
    )
  }

  # Both splits keep a trailing empty field (the stri_split_fixed() reading,
  # ADR 0005), not strsplit()'s: strsplit() drops an empty last field, so
  # `"1::"` split to "1" and `[1:2:3:4:5:6:7::]` went down the no-"::" branch
  # unserialized (RURL-sqmhtldq), and `"8:"` split to "8", quietly repairing
  # an invalid trailing ":". Empty fields now survive and fail.
  parts <- .const_split_keep_trailing(inner, "::")
  if (length(parts) > 2L) {
    return(host)
  }

  split_side <- function(x) {
    if (is.na(x) || x == "") {
      return(character(0))
    }
    .const_split_keep_trailing(x, ":")
  }
  if (length(parts) == 1L) {
    hextets <- split_side(parts[1L])
    if (length(hextets) != 8L) {
      return(host)
    }
  } else {
    left <- split_side(parts[1L])
    right <- split_side(parts[2L])
    zero_count <- 8L - length(left) - length(right)
    if (zero_count < 1L) {
      return(host)
    }
    hextets <- c(left, rep("0", zero_count), right)
  }
  if (length(hextets) != 8L ||
      !all(grepl("^[0-9A-Fa-f]{1,4}$", hextets))) {
    return(host)
  }

  pieces <- suppressWarnings(strtoi(hextets, base = 16L))
  if (anyNA(pieces) || any(pieces < 0L | pieces > 65535L)) {
    return(host)
  }

  is_zero <- pieces == 0L
  run <- rle(is_zero)
  ends <- cumsum(run$lengths)
  starts <- ends - run$lengths + 1L
  zero_runs <- which(run$values & run$lengths > 1L)
  compress_start <- NA_integer_
  compress_len <- 0L
  if (length(zero_runs) > 0L) {
    best <- zero_runs[which.max(run$lengths[zero_runs])]
    compress_start <- starts[best]
    compress_len <- run$lengths[best]
  }

  # sprintf(), not as.character(as.hexmode()): before R 4.2.0 the hexmode
  # method zero-padded every element to the widest one, so a vector holding
  # 0x7f00 rendered 1 as "0001" and broke WHATWG's shortest-hex form.
  rendered <- sprintf("%x", pieces)
  if (is.na(compress_start)) {
    return(paste0("[", paste(rendered, collapse = ":"), "]"))
  }

  compress_end <- compress_start + compress_len - 1L
  before <- if (compress_start > 1L) {
    rendered[seq_len(compress_start - 1L)]
  } else {
    character(0)
  }
  after <- if (compress_end < 8L) {
    rendered[(compress_end + 1L):8L]
  } else {
    character(0)
  }

  serialized <- if (length(before) == 0L && length(after) == 0L) {
    "::"
  } else if (length(before) == 0L) {
    paste0("::", paste(after, collapse = ":"))
  } else if (length(after) == 0L) {
    paste0(paste(before, collapse = ":"), "::")
  } else {
    paste0(paste(before, collapse = ":"), "::", paste(after, collapse = ":"))
  }
  paste0("[", serialized, "]")
}

# Phase 5b helper (vector): delegates IPv6 WHATWG serialization to the scalar
# parser only for bracketed rows that can change.
.serialize_whatwg_ipv6_hosts_vec <- function(host) {
  out <- host
  elig <- !is.na(host) &
    .const_detect(host, .ICU_IPV6_SERIALIZABLE)
  elig[is.na(elig)] <- FALSE
  if (any(elig)) {
    out[elig] <- vapply(
      host[elig], .serialize_whatwg_ipv6_host, character(1),
      USE.NAMES = FALSE
    )
  }
  out
}

# Phase 5b (vector): the url_standard host IPv4/reg-name model (RURL-luwvkwhd,
# PRD §6.2). A no-op when url_standard is NULL (returns the parsed host / IP
# flag
# unchanged, never fatal), so the default pipeline is byte-for-byte unaffected
# (AC #1).
#
# The web parser already coerces numeric IPv4 forms itself (2130706433 ->
# 127.0.0.1,
# 192.168.010.1 -> 192.168.8.1) and keeps out-of-range / over-arity forms
# literal (256.1.1.1, 1.2.3.4.5). So `engine_host` is the coerced spelling and
# `input_host` (the original pre-parse token) is the un-coerced one. For an
# IPv4 attempt (`is_attempt`):
#   - rfc3986: parse faithfully as a reg-name -- restore the original token and
#     treat it as an IP only if the ORIGINAL was already a canonical quad.
#     Never fatal (every such token is a valid RFC 3986 reg-name).
#   - whatwg: every host that ends in a number is read by the WHATWG IPv4
#     parser (`.whatwg_ipv4_read()`, i.e. `raddr::addr_whatwg()`). An address
#     replaces the host; a failure (out-of-range, > 4 parts, a digit outside
#     its radix) makes the row fatal.
# Non-attempt hosts (ordinary names, IPv6, missing) pass through untouched.
# Returns updated `host`, `is_ip`, and a `fatal` mask the caller folds into the
# null-row set.
.apply_host_standard_model_vec <- function(input_host, engine_host,
                                           is_ip_engine, url_standard,
                                           is_attempt) {
  n <- length(engine_host)
  host <- engine_host
  is_ip <- is_ip_engine
  fatal <- rep(FALSE, n)
  if (is.null(url_standard)) {
    return(list(host = host, is_ip = is_ip, fatal = fatal))
  }

  if (identical(url_standard, "rfc3986")) {
    # Reg-name: keep the original token; IP only if it was already canonical.
    # RFC 3986 has no numeric-host special case, so the all-numeric is_ipv4ish
    # attempt set is the right (and unchanged) trigger here.
    att <- is_attempt & !is.na(is_attempt)
    if (any(att)) {
      input_canonical <- .detect_ip_host_vec(input_host)
      host[att] <- input_host[att]
      is_ip[att] <- input_canonical[att]
    }
  } else {
    # WHATWG: a host that "ends in a number" MUST parse as IPv4 or the whole
    # host parse fails -- independent of whether the web parser coerced it.
    # This trigger (not is_ipv4ish) closes the gap where obfuscated/mixed forms
    # the web parser leaves as reg-names (foo.09, foo.0x4, 1.2.3.08.,
    # 0x1.2.3.4.5.)
    # bypassed the gate and were wrongly accepted with warning-invalid-tld.
    #
    # The trigger is read in the HOST PARSER's own order (RURL-lxdwuacn):
    # percent-decode, then domain-to-ASCII, and only THEN "ends in a number"
    # (#concept-host-parser steps 3-6). `engine_host` is already
    # percent-decoded under this profile (`host_pct`, R/parse-web.R), and the
    # UTS-46 mapping is applied here for the test only -- the recorded host
    # keeps its Unicode spelling, since `host_encoding` selects the rendering
    # later. Reading the trigger off the SOURCE token missed both spellings
    # WPT pins: `%30%78%63%30%2e%30%32%35%30.01` decodes to `0xc0.0250.01` and
    # the fullwidth `０Ｘｃ０．０２５０．０１` maps to it, and each is
    # `192.168.0.1`. The web parser itself never takes the IPv4 reading of a
    # token holding a "%", and that stays so: it reproduces the engine it was
    # calibrated against (ADR 0018); this is the standard's host MODEL, layered
    # over it, and the address reading it applies is the standard's IPv4
    # parser as `raddr` owns it (`.whatwg_ipv4_read()`, ADR 0018's
    # 2026-10-09 amendment). An NA there is the IPv4 parser's failure, which
    # fails the host and so the URL.
    # `startsWith()`: stri_startswith_fixed() reads past a U+FEFF that starts
    # its input (see .starts_with_bom()), and a host led by the mark is not
    # bracketed (RURL-opwjlnla).
    reg_like <- !is.na(host) & host != "" & !startsWith(host, "[")
    reg_like[is.na(reg_like)] <- FALSE
    # A percent-encoded U+3002, U+FF0E or U+FF61 is a full stop once decoded,
    # as a raw one is (RURL-esztpdhx): UTS #46 maps all three to "." before it
    # splits labels. The raw spelling was mapped before the parse
    # (.map_whatwg_domain_separators_vec()), so the decoded one is mapped here,
    # and both spellings reach the PSL and the status as the same host. Left
    # unmapped, a trailing one failed the row: punycoder 1.3.0's domain to
    # ASCII rejects the empty last label a MAPPED full stop leaves (`a。`
    # gives NA where `a.` gives `a.`), so `http://example.com%E3%80%82/` was
    # an error where WHATWG gives the host `example.com.`. Only a non-ASCII
    # host can hold one, and a host that held nothing else is ASCII after it.
    nonascii_all <- reg_like & .bytes_detect(host, .NON_ASCII_BYTE)
    nonascii_all[is.na(nonascii_all)] <- FALSE
    if (any(nonascii_all)) {
      host[nonascii_all] <- .bytes_replace(
        host[nonascii_all], .UTS46_FULL_STOP_VARIANTS, ".", all = TRUE
      )
      nonascii_all[nonascii_all] <-
        .bytes_detect(host[nonascii_all], .NON_ASCII_BYTE)
    }
    candidate <- host
    mapped <- rep(NA_character_, n)
    if (any(nonascii_all)) {
      mapped[nonascii_all] <- .whatwg_domain_to_ascii(host[nonascii_all])
      use_mapped <- nonascii_all & !is.na(mapped)
      candidate[use_mapped] <- mapped[use_mapped]
    }
    att <- reg_like & .host_ends_in_number_vec(candidate)
    if (any(att)) {
      ipv4 <- .whatwg_ipv4_read(candidate[att])
      canonical <- !is.na(ipv4)
      host[att] <- ifelse(canonical, ipv4, candidate[att])
      is_ip[att] <- canonical
      fatal[att] <- !canonical
    }

    # WHATWG forbidden host/domain code points (RURL-jfuqpwvh). A special-scheme
    # host is a DOMAIN: WHATWG fails the host parse if domain-to-ASCII produces
    # a forbidden domain code point, or if domain-to-ASCII itself fails (a
    # disallowed / ignored-to-empty code point). rurl previously accepted these
    # as reg-names with warning-no-tld; the whatwg profile now rejects them
    # (flips the ADR 0004 boundary; governed axis per ADR 0007). Two narrow
    # signals on the resolved reg-name host (IP literals excluded):
    #   (B) an ASCII forbidden code point survives to the host (| ^ DEL space %
    #       and structural bytes) -- a cheap charclass, catches what UTS-46
    #       leaves intact (or, for DEL, silently drops);
    #   (A) a NON-ASCII host fails UTS-46 domain-to-ASCII (U+FFFD/U+FFFF
    #       noncharacters, a soft-hyphen-only host collapsing to empty) -- one
    #       vectorized .whatwg_domain_to_ascii() over just the non-ASCII rows;
    #       or its domain-to-ASCII RESULT holds a forbidden code point, which is
    #       where WHATWG tests for one: U+FF03, U+FF0F, U+FF1F, U+FF1A, U+FF05,
    #       U+00A0 and U+3000 map to # / ? : % and space (RURL-crsrkcoh).
    reg <- !is_ip & !is.na(host) & host != ""
    if (any(reg)) {
      bad_cp <- reg & .bytes_detect(host, .WHATWG_FORBIDDEN_HOST_CP_BYTES)
      bad_cp[is.na(bad_cp)] <- FALSE
      fatal <- fatal | bad_cp

      # `mapped` is the one domain-to-ASCII pass above, computed over every
      # non-ASCII reg-name; a row still non-ASCII here was not an address.
      nonascii <- reg & !bad_cp & .bytes_detect(host, .NON_ASCII_BYTE)
      nonascii[is.na(nonascii)] <- FALSE
      bad_mapped <- nonascii & !is.na(mapped) &
        .bytes_detect(mapped, .WHATWG_FORBIDDEN_HOST_CP_BYTES)
      bad_mapped[is.na(bad_mapped)] <- FALSE
      fatal <- fatal | (nonascii & is.na(mapped)) | bad_mapped
    }

    # WHATWG IPv6 serializer (RURL-thjmzaam): bracketed IPv6 literals serialize
    # as eight 16-bit pieces with zero compression; an embedded dotted-quad IPv4
    # tail never remains dotted.
    host <- .serialize_whatwg_ipv6_hosts_vec(host)
  }

  list(host = host, is_ip = is_ip, fatal = fatal)
}

# The host the www and subdomain policies read (RURL-fobwctml). Under
# `whatwg` that is the host the WHATWG host parser defines: its domain to
# ASCII step runs UTS #46 processing, whose first step maps the input (UTS #46
# section 4, step 1), and the mapping removes the code points UTS-46 ignores,
# U+200B, U+00AD and U+FEFF among them. Phase 9 renders that same mapped form
# (`.whatwg_host_presentation_vec()`, RUL-002), so a policy reading the source
# spelling policed `<U+200B>www.com` as a host whose first label is not
# `www`, a different host from the `www.com` WHATWG records. The policies now
# run after the mapping instead: they read the `keep` rendering, which Phase 9
# maps to itself under every `host_encoding`. `rfc3986` and the `NULL` arm
# have no mapping step and read the host as written.
.policy_host_vec <- function(host, elig, url_standard) {
  if (.is_whatwg(url_standard) && any(elig)) {
    host[elig] <- .whatwg_host_presentation_vec(host[elig], "keep")
  }
  host
}

# The host after its www prefix: the ICU caseless capture
# `^(www[0-9]*\.)(.*)`, group 2, in bytes (RURL-yqmoztoj). For rows that
# start with the prefix (the callers test it first). The prefix is spelled
# as ASCII letter classes (ADR 0019); ICU folds no other code point to "w".
# ICU's `.*` stops at the first line terminator, so the capture is cut there.
# Both steps are single scans, linear in the host.
.www_bare_host <- function(x) {
  pre_end <- .bytes_locate(x, "^[Ww][Ww][Ww][0-9]*\\.")[, "end"]
  rest <- .bytes_cut(x, pre_end + 1L, .Machine$integer.max)
  stop <- .bytes_locate(rest, .ICU_TERMINATOR)[, "start"]
  cut <- !is.na(stop)
  rest[cut] <- .bytes_cut(rest[cut], 1L, stop[cut] - 1L)
  rest
}

# Phase 6 (vector): apply the www-prefix policy to (non-IP) hosts.
# Under `whatwg` every policy but "none" reads the UTS-46-mapped host
# (`.policy_host_vec()`, RURL-fobwctml).
.apply_www_policy_vec <- function(raw_host, www_handling, is_ip_host,
                                  engine = NULL, url_standard = NULL) {
  final_host <- raw_host
  elig <- !is_ip_host & !is.na(raw_host) & raw_host != ""
  if (!any(elig) || www_handling == "none") {
    return(final_host)
  }
  raw_host <- .policy_host_vec(raw_host, elig, url_standard)
  final_host <- raw_host

  if (www_handling == "strip") {
    # Was the ICU caseless `^(www[0-9]*\.)(.*)` -> `$2`, which drops the
    # prefix and keeps the rest whatever ICU's `.` stopped at. Byte mode
    # spells the case (ADR 0019); ICU folds no non-ASCII code point to "w".
    final_host[elig] <- .bytes_replace(
      raw_host[elig], "^[Ww][Ww][Ww][0-9]*\\.", ""
    )
  } else if (www_handling == "keep") {
    lower <- .ascii_tolower(raw_host)
    has_www <- .bytes_detect(lower, "^www[0-9]*\\.")
    has_www[is.na(has_www)] <- FALSE
    hw <- elig & has_www
    no_www <- elig & !has_www
    if (any(hw)) {
      final_host[hw] <- paste0("www.", .www_bare_host(raw_host[hw]))
    }
    if (any(no_www)) {
      final_host[no_www] <- paste0("www.", raw_host[no_www])
    }
  } else if (www_handling == "if_no_subdomain") {
    final_host[elig] <- .apply_www_if_no_subdomain_vec(raw_host[elig], engine)
  }
  final_host
}

# Phase 6 (www_handling = "if_no_subdomain", vector): add a leading "www." only
# when the host is itself an apex (registrable domain with no subdomain labels).
# The STRUCTURAL decision uses pslr's canonical decomposition (batched over the
# eligible hosts) so an A-label host and its Unicode equivalent take the same
# branch; the emitted host keeps the input spelling (candidate_host). Under
# `whatwg` that input is already the UTS-46-mapped host (`.policy_host_vec()`,
# applied by the caller, RURL-fobwctml), so this helper takes no selector.
.apply_www_if_no_subdomain_vec <- function(raw_host, engine = NULL) {
  lower <- .ascii_tolower(raw_host)

  candidate_host <- raw_host
  has_www <- .bytes_detect(lower, "^www[0-9]*\\.")
  has_www[is.na(has_www)] <- FALSE
  if (any(has_www)) {
    candidate_host[has_www] <- paste0(
      "www.", .www_bare_host(raw_host[has_www])
    )
  }

  # `startsWith()`, not stri_startswith_fixed(), which reads past a U+FEFF
  # that starts its input (see .starts_with_bom()): the mark-led apex
  # "<U+FEFF>www.com" is not www-prefixed, so it gets "www." like its U+200B
  # twin (RURL-opwjlnla). Under `whatwg` the caller hands over the
  # UTS-46-mapped host, which no longer holds the mark (RURL-fobwctml).
  host_for_domain_check <- candidate_host
  cand_lower <- .ascii_tolower(candidate_host)
  cw <- startsWith(cand_lower, "www.")
  cw[is.na(cw)] <- FALSE
  if (any(cw)) {
    host_for_domain_check[cw] <- .www_bare_host(candidate_host[cw])
  }

  decomp <- .psl_suffix_extract(host_for_domain_check, "all", engine)
  derived_domain <- decomp$registrable_domain
  derived_subdomain <- decomp$subdomain

  no_derived_domain <- is.na(derived_domain) | derived_domain == ""
  # Apex iff the canonical decomposition has no subdomain labels.
  host_equals_domain <- !is.na(derived_subdomain) & derived_subdomain == ""

  cand_has_www <- cw

  result <- candidate_host
  add_www <- !no_derived_domain & host_equals_domain & !cand_has_www
  result[add_www] <- paste0("www.", candidate_host[add_www])
  result
}

# Phase 6 (scalar wrapper): delegates to .apply_www_policy_vec().
.apply_www_policy <- function(raw_host, www_handling, is_ip_host,
                              engine = NULL, url_standard = NULL) {
  .apply_www_policy_vec(
    raw_host, www_handling, is_ip_host, engine, url_standard
  )
}

# Phase 7 (vector, RURL-jhsbzmsj): the PSL ANNOTATION candidate for a host.
#
# `rfc3986` keeps a reg-name's percent-encoding in the host IDENTITY (host_pct =
# "keep"), because the RFC profile has to be source-preserving. The PSL cannot
# read a label containing "%", so a percent-encoded IDN host would carry no
# domain/TLD at all. RFC 3986 §3.2.2 explicitly admits percent-encoded UTF-8
# non-ASCII names in reg-name and requires IDNA transformation before a DNS
# lookup, while §6.2.2.2 authorizes only unreserved decoding for URI
# normalization -- so the decoded view is a legitimate basis for a DNS-facing
# ANNOTATION even though it must never become the identity.
#
# Hence: decode EXACTLY ONCE as UTF-8, solely to build the candidate. This must
# not affect acceptance, `final_host`, serialization, or identity -- it is
# reachable only from this function, which returns domain/TLD and nothing else.
#
# "Exactly once" is well defined here because no ACCEPTED host can carry a
# literal "%": an input spelling one (`%25`) is rejected under every profile, so
# a "%" in `final_host` is always an undecoded triplet. Verified across the
# whole octet sweep, not assumed.
#
# A failed decode (a malformed triplet, a decoded NUL) or a decode that is not
# valid UTF-8 yields NA -- the `unknown` annotation. `validUTF8()` is the
# validator on purpose: it is byte-based and locale-independent (the
# RURL-kmpnbvdl defect class), and strict enough to reject overlong forms and
# encoded surrogates, which are not legitimate spellings of a code point.
#
# Everything after the decode is the EXISTING pslr path, deliberately: the
# annotation for an `rfc3986` host then agrees with the one `whatwg` computes
# for the same decoded host, including its rejections (U+FFFD stays unknown, a
# soft hyphen is mapped away by IDNA).
.psl_annotation_host_vec <- function(hosts) {
  candidate <- hosts
  pct <- .bytes_detect(hosts, "%", fixed = TRUE)
  pct[is.na(pct)] <- FALSE
  if (!any(pct)) {
    return(candidate)
  }
  candidate[pct] <- vapply(hosts[pct], function(one) {
    decoded <- .web_host_percent_decode(one)
    if (is.null(decoded) || !validUTF8(decoded)) {
      return(NA_character_)
    }
    Encoding(decoded) <- "UTF-8"
    decoded
  }, character(1), USE.NAMES = FALSE)
  candidate
}

# Phase 7 (vector): derive the registered domain and TLD from each host using
# the Public Suffix List. This is the hot path: pslr is queried ONCE per output
# spelling over the UNIQUE non-IP hosts (host-level de-dup; many URLs share a
# host) rather than once per URL. `host_encoding` selects the emitted spelling,
# mirroring get_host(): "unicode" decodes IDNs, "idna" emits ASCII A-labels, and
# "keep" follows each input host's own spelling (ASCII if it is an A-label).
#
# The PSL is queried on the ANNOTATION candidate (above), never on the identity
# host -- the two differ only for a percent-encoded `rfc3986` reg-name.
.derive_domain_tld_vec <- function(final_host, is_ip_host, tld_source,
                                   host_encoding = "keep", engine = NULL) {
  n <- length(final_host)
  domain <- rep(NA_character_, n)
  tld <- rep(NA_character_, n)
  annot_host <- .psl_annotation_host_vec(final_host)
  elig <- !is_ip_host & !is.na(annot_host) & annot_host != ""
  if (!any(elig)) {
    return(list(domain = domain, tld = tld))
  }

  hosts <- annot_host[elig]
  uniq_hosts <- unique(hosts)

  spelling <- if (host_encoding == "idna") {
    rep("ascii", length(uniq_hosts))
  } else if (host_encoding == "unicode") {
    rep("unicode", length(uniq_hosts))
  } else {
    ifelse(.host_is_ace_vec(uniq_hosts), "ascii", "unicode")
  }

  dom_uniq <- rep(NA_character_, length(uniq_hosts))
  tld_uniq <- rep(NA_character_, length(uniq_hosts))

  ascii_mask <- spelling == "ascii"
  if (any(ascii_mask)) {
    dom_uniq[ascii_mask] <- .psl_registered_domain(
      uniq_hosts[ascii_mask], tld_source, "ascii", engine
    )
    tld_uniq[ascii_mask] <- .psl_public_suffix(
      uniq_hosts[ascii_mask], tld_source, "ascii", engine
    )
  }
  uni_mask <- !ascii_mask
  if (any(uni_mask)) {
    dom_uniq[uni_mask] <- .psl_registered_domain(
      uniq_hosts[uni_mask], tld_source, "unicode", engine
    )
    tld_uniq[uni_mask] <- .psl_public_suffix(
      uniq_hosts[uni_mask], tld_source, "unicode", engine
    )
  }

  pos <- match(hosts, uniq_hosts)
  domain[elig] <- dom_uniq[pos]
  tld[elig] <- tld_uniq[pos]
  list(domain = domain, tld = tld)
}

# Phase 7 (scalar wrapper): delegates to .derive_domain_tld_vec().
.derive_domain_tld <- function(final_host, is_ip_host, tld_source,
                               host_encoding = "keep", engine = NULL) {
  .derive_domain_tld_vec(
    final_host, is_ip_host, tld_source, host_encoding, engine
  )
}

# Phase 8 (vector): keep only the requested number of subdomain levels. Only
# runs when subdomain_levels_to_keep is non-NULL. The STRUCTURAL decomposition
# is batched over the eligible hosts; the (variable-length) label reconstruction
# runs via vapply on the affected hosts only.
.apply_subdomain_policy_vec <- function(final_host, domain,
                                        subdomain_levels_to_keep, is_ip_host,
                                        engine = NULL, url_standard = NULL) {
  if (is.null(subdomain_levels_to_keep)) {
    return(final_host)
  }
  can_trim <- !is_ip_host & !is.na(domain) & domain != "" &
    !is.na(final_host) & final_host != ""
  if (!any(can_trim)) {
    return(final_host)
  }

  idx <- which(can_trim)
  # Under `whatwg` the trim reads the UTS-46-mapped host (RURL-fobwctml).
  fh <- .policy_host_vec(final_host[idx], rep(TRUE, length(idx)), url_standard)
  lower <- .ascii_tolower(fh)
  # `startsWith()`, not stri_startswith_fixed(), which reads past a U+FEFF
  # that starts its input (see .starts_with_bom()): "<U+FEFF>www." read as the
  # www prefix, and stri_sub() then cut the mark off with it. The mark-led
  # label is a subdomain label like its U+200B twin's (RURL-opwjlnla). A row
  # that passes starts with "www.", four bytes, which the byte cut drops.
  has_www_prefix <- startsWith(lower, "www.")
  has_www_prefix[is.na(has_www_prefix)] <- FALSE
  # The rows the stringi reading took for www-prefixed: one mark, then "www.".
  # When the policy trims none of their labels they stay as written, rather
  # than take the rebuild below, which lowercases the registrable labels; the
  # stringi reading left "<U+FEFF>WWW.Example.com" as written too. A mapped
  # `whatwg` host holds no mark, so this only reaches `rfc3986` and `NULL`.
  bom_www <- .starts_with_bom(fh)
  if (any(bom_www)) {
    bom_www[bom_www] <- startsWith(
      .byte_substring_vec(lower[bom_www], 4L), "www."
    )
  }

  host_part <- fh
  if (any(has_www_prefix)) {
    host_part[has_www_prefix] <- .bytes_cut(
      fh[has_www_prefix], 5L, .Machine$integer.max
    )
  }

  decomp <- .psl_suffix_extract(host_part, "all", engine)
  derived_subdomain <- decomp$subdomain
  has_subdomain <- !is.na(derived_subdomain) & nzchar(derived_subdomain)

  # base strsplit keeps the documented trailing-empty behavior (see ADR 0005).
  sub_split <- strsplit(derived_subdomain, ".", fixed = TRUE)
  raw_split <- strsplit(host_part, ".", fixed = TRUE)
  num_sub_labels <- lengths(sub_split)
  num_raw_labels <- lengths(raw_split)

  act <- has_subdomain & (num_raw_labels > num_sub_labels) &
    !(bom_www & subdomain_levels_to_keep >= num_sub_labels)
  act_pos <- which(act)
  if (length(act_pos) > 0L) {
    slk <- subdomain_levels_to_keep
    recon <- vapply(act_pos, function(j) {
      raw_labels <- raw_split[[j]]
      ns <- num_sub_labels[j]
      # The registrable-domain portion is lowercased (matching the historical
      # reconstruction); the kept subdomain labels preserve the input spelling.
      registrable_labels <- .ascii_tolower(
        utils::tail(raw_labels, length(raw_labels) - ns)
      )
      sub_labels <- utils::head(raw_labels, ns)

      kept_sub_labels <- character(0)
      if (slk > 0) {
        num_keep <- min(length(sub_labels), slk)
        if (num_keep > 0) {
          kept_sub_labels <- utils::tail(sub_labels, num_keep)
        }
      }
      reconstructed <- paste(
        c(kept_sub_labels, registrable_labels), collapse = "."
      )
      if (has_www_prefix[j]) {
        paste0("www.", reconstructed)
      } else {
        reconstructed
      }
    }, character(1), USE.NAMES = FALSE)
    fh[act_pos] <- recon
  }

  final_host[idx] <- fh
  final_host
}

# Phase 8 (scalar wrapper): delegates to .apply_subdomain_policy_vec().
.apply_subdomain_policy <- function(final_host, domain,
                                    subdomain_levels_to_keep, is_ip_host,
                                    engine = NULL, url_standard = NULL) {
  .apply_subdomain_policy_vec(
    final_host, domain, subdomain_levels_to_keep, is_ip_host, engine,
    url_standard = url_standard
  )
}

# Phase 9 (vector): re-encode (non-IP) hosts to IDNA/Punycode or Unicode on
# request. The Punycode round-trip semantics are owned by the DO-NOT-ALTER
# helpers .normalize_and_punycode_vec()/.punycode_to_unicode_vec() (domain.R);
# this phase only selects the eligible rows and applies the scalar fallback
# rule (keep the pre-encode host when the encode returns NA, or when the decode
# returns NA / ""). Under `whatwg` the default ("keep") presentation is NOT a
# no-op: it renders the UTS-46-mapped host (RUL-002; see
# .whatwg_host_presentation_vec()).
.apply_host_encoding_vec <- function(final_host, host_encoding, is_ip_host,
                                     url_standard = NULL) {
  host_for_clean <- final_host
  if (host_encoding == "keep" && !.is_whatwg(url_standard)) {
    return(host_for_clean)
  }
  elig <- !is.na(final_host) & final_host != "" & !is_ip_host
  if (!any(elig)) {
    return(host_for_clean)
  }

  subset <- final_host[elig]
  # Under `rfc3986` the host IDENTITY preserves its source spelling: section
  # 6.2.2.2 lets only unreserved triplets decode, so `a%C2%ADb` stays written
  # that way (`host_pct = "keep"`, R/parse-web.R). IDNA and Unicode are
  # PRESENTATIONS of that identity, and both need real code points -- UTS-46
  # has nothing to say about the characters "%", "C", "2". So decode here,
  # where the rendering is chosen, rather than in the parse, where it would
  # change what the host IS (the ADR 0011 separation).
  #
  # Scoped to `rfc3986` precisely because that is the only profile that leaves
  # a triplet standing; `whatwg` and the no-selector default have already
  # decoded the host once, and decoding again would strip a LEVEL of encoding
  # (`a%2560b` -> `a%60b` -> "a`b") rather than reveal one.
  if (identical(url_standard, "rfc3986")) {
    subset <- vapply(subset, .whatwg_percent_decode_host, character(1),
      USE.NAMES = FALSE
    )
  }
  if (.is_whatwg(url_standard)) {
    host_for_clean[elig] <- .whatwg_host_presentation_vec(subset, host_encoding)
  } else if (host_encoding == "idna") {
    encoded <- .normalize_and_punycode_vec(subset)
    keep_orig <- is.na(encoded)
    encoded[keep_orig] <- subset[keep_orig]
    host_for_clean[elig] <- encoded
  } else {
    decoded <- .punycode_to_unicode_vec(subset)
    keep_orig <- is.na(decoded) | decoded == ""
    decoded[keep_orig] <- subset[keep_orig]
    host_for_clean[elig] <- decoded
  }
  host_for_clean
}

# WHATWG host presentation (RUL-002). The WHATWG host parser runs UTS-46
# "domain to ASCII" (Transitional_Processing false, CheckHyphens false,
# UseSTD3ASCIIRules false, VerifyDnsLength false) and the URL record stores that
# ASCII host; "domain to Unicode" is UTS-46 ToUnicode of it (UTS #46 section 4,
# 4.2, 4.3). So under `whatwg` every spelling is a rendering of ONE mapped form:
#   idna    -> ToASCII(host)
#   unicode -> ToUnicode(ToASCII(host))
#   keep    -> ToUnicode(ToASCII(host)) for a host written in Unicode, and
#              ToASCII(host) for a host already carrying an ACE (`xn--`) label,
#              mirroring how Stage B picks the `domain`/`tld` spelling for
#              "keep" (`host_is_ace`, R/parse.R) so host and domain agree.
# The mapped form is computed once via `punycoder::host_normalize()`
# (`.whatwg_domain_to_ascii()`) -- the call the `idna` branch already made,
# never the ADR 0002 helpers, which stay the reversible (unmapped) renderers of
# the `rfc3986` and `NULL` arms. When domain-to-ASCII fails the pre-encode host
# is kept, as before.
.whatwg_host_presentation_vec <- function(subset, host_encoding) {
  mapped <- .whatwg_domain_to_ascii(subset)
  retry <- is.na(mapped)
  if (any(retry)) {
    mapped[retry] <- .normalize_and_punycode_vec(subset[retry])
  }
  keep_orig <- is.na(mapped)
  mapped[keep_orig] <- subset[keep_orig]
  if (host_encoding == "idna") {
    return(mapped)
  }
  # Only an ACE label decodes to anything other than itself; skip the
  # Punycode pass (and its cache) for the all-ASCII majority.
  decoded <- mapped
  ace_mapped <- .host_is_ace_vec(mapped)
  if (any(ace_mapped)) {
    dec <- .punycode_to_unicode_vec(mapped[ace_mapped])
    keep_mapped <- is.na(dec) | dec == ""
    dec[keep_mapped] <- mapped[ace_mapped][keep_mapped]
    decoded[ace_mapped] <- dec
  }
  if (host_encoding == "unicode") {
    return(decoded)
  }
  ace_source <- .host_is_ace_vec(subset)
  decoded[ace_source] <- mapped[ace_source]
  decoded
}

# Phase 9 (scalar wrapper): delegates to .apply_host_encoding_vec().
.apply_host_encoding <- function(final_host, host_encoding, is_ip_host,
                                 url_standard = NULL) {
  .apply_host_encoding_vec(final_host, host_encoding, is_ip_host, url_standard)
}

# Unicode case mapping, pinned to .ASCII_SAFE_ICU_LOCALE, that keeps a leading
# U+FEFF. stri_trans_toupper() and stri_trans_tolower() drop a U+FEFF that
# starts their input (see .starts_with_bom()), so `case_handling` lost the mark
# from a host or path led by it (RURL-opwjlnla). A row led by the mark is mapped
# with an ASCII "0" spliced on first and cut off as a byte: "0" maps to itself
# in both directions and is neither cased nor case-ignorable, so it ends a
# Final_Sigma context exactly as the start of the string does. Other rows take
# the plain call, so they cannot move.
.unicode_case_map_vec <- function(x, upper) {
  map <- if (upper) stringi::stri_trans_toupper else stringi::stri_trans_tolower
  out <- map(x, locale = .ASCII_SAFE_ICU_LOCALE)
  bom_led <- .starts_with_bom(x)
  if (any(bom_led)) {
    out[bom_led] <- .byte_substring_vec(
      map(paste0("0", x[bom_led]), locale = .ASCII_SAFE_ICU_LOCALE), 2L
    )
  }
  out
}

# Phase 10 (vector): apply the case policy to host, path, and scheme.
.apply_case_policy_vec <- function(host_output, path_output, scheme_output,
                                   case_handling) {
  # The HOST branch splits by DIRECTION (RURL-ugfpuotu).
  #
  # LOWERING a host is protocol syntax: RFC 3986 section 6.2.2.1 and the WHATWG
  # URL Standard both normalize hosts to lowercase, and WHATWG spells it "ASCII
  # lowercase". So `lower` / `lower_host` use the ASCII-only helper, which takes
  # locale, ICU version, and Unicode version out of the path entirely -- a
  # Turkish/Azeri session can no longer map "I" -> "ı" and silently name a
  # DIFFERENT domain. That is the bug being fixed here, and it is a LOWERING
  # bug.
  #
  # UPPERCASING a host is not protocol syntax at all: no standard ever
  # uppercases a host. `case_handling = "upper"` is a user-facing presentation
  # transform rurl offers as a convenience, in the same category as the path
  # branch below -- so the "syntax => ASCII-only" rule does not reach it, and
  # full Unicode case mapping is kept (pinned to the same non-tailoring locale
  # as the path branch, so it is still session-invariant). ASCII-only
  # uppercasing would mangle a non-ASCII host into mixed case
  # ("bücher.example" -> "BüCHER.EXAMPLE"), which serves nobody.
  h_mask <- !is.na(host_output) & host_output != ""
  if (any(h_mask)) {
    if (case_handling == "lower" || case_handling == "lower_host") {
      # Case folding applies to the reg-name, not to the hex digits of a
      # surviving percent-triplet, which RFC 3986 section 6.2.2.1 renders
      # uppercase (RURL-savatsuc).
      host_output[h_mask] <- .pct_hex_upper(
        .ascii_tolower(host_output[h_mask])
      )
    } else if (case_handling == "upper") {
      host_output[h_mask] <- .unicode_case_map_vec(
        host_output[h_mask], upper = TRUE
      )
    }
  }

  # The PATH is the one component where `case_handling` is a USER-REQUESTED
  # transformation of free text that may legitimately be non-ASCII, so full
  # Unicode case mapping is kept here (unlike host LOWERING above and the scheme
  # branch below, which are protocol SYNTAX and use ASCII-only mapping;
  # RURL-ugfpuotu).
  # The locale is pinned explicitly so the result does not vary with the R
  # session's locale; see .ASCII_SAFE_ICU_LOCALE in R/utils.R for why "root" and
  # "und" do NOT work here.
  p_mask <- !is.na(path_output)
  if (any(p_mask)) {
    if (case_handling == "lower") {
      path_output[p_mask] <- .unicode_case_map_vec(
        path_output[p_mask], upper = FALSE
      )
    } else if (case_handling == "upper") {
      path_output[p_mask] <- .unicode_case_map_vec(
        path_output[p_mask], upper = TRUE
      )
    }
  }

  # The SCHEME stays ASCII-only in BOTH directions: a scheme is ASCII by grammar
  # (RFC 3986 `ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )`), so there is no
  # non-ASCII scheme for ASCII-only mapping to mangle, and both directions are
  # syntax.
  s_mask <- !is.na(scheme_output)
  if (any(s_mask)) {
    if (case_handling == "lower" || case_handling == "lower_host") {
      scheme_output[s_mask] <-
        .ascii_tolower(scheme_output[s_mask])
    } else if (case_handling == "upper") {
      scheme_output[s_mask] <-
        .ascii_toupper(scheme_output[s_mask])
    }
  }

  list(host = host_output, path = path_output, scheme = scheme_output)
}

# Phase 10 (scalar wrapper): delegates to .apply_case_policy_vec().
.apply_case_policy <- function(host_for_clean, path_final, final_scheme,
                               case_handling) {
  .apply_case_policy_vec(
    host_for_clean, path_final, final_scheme, case_handling
  )
}

# Query-filter (vector): map the scalar query engine (._filter_query_params,
# path-query.R) over each row's raw query, returning the canonical, re-encoded
# query WITHOUT a leading "?" per row -- or "" where nothing survives or the
# query is absent. "drop" (the default) short-circuits to "" for every row,
# preserving the historical query-free clean_url. Only non-NA/non-empty raw
# queries reach the scalar engine, so the common (query-less) URL pays nothing.
#
# This is a Stage B (presentation) transform: it depends only on the raw query
# (a Stage A column) and the query options, never on the expensive parse core.
# Like clean_url itself it is recomputed on every call and never cached, which
# is exactly why the query options are excluded from the full_parse cache key
# (see .parse_cache_keys, parse.R).
.filter_query_vec <- function(raw_query, opts) {
  n <- length(raw_query)
  out <- rep("", n)
  if (identical(opts$query_handling, "drop")) {
    return(out)
  }
  idx <- which(!is.na(raw_query) & nzchar(raw_query))
  if (length(idx) == 0L) {
    return(out)
  }
  # vapply passes the query options straight through to ._filter_query_params()
  # as trailing named args (the raw query is its first positional argument).
  out[idx] <- vapply(
    raw_query[idx], ._filter_query_params, character(1),
    query_handling = opts$query_handling,
    params_keep = opts$params_keep,
    params_drop = opts$params_drop,
    params_case_sensitive = opts$params_case_sensitive,
    sort_params = opts$sort_params,
    empty_param_handling = opts$empty_param_handling,
    decode_plus = opts$decode_plus,
    USE.NAMES = FALSE
  )
  out
}

# Look up the WHATWG default port for a (possibly mixed-case) scheme vector
# (PRD v2 D1, RURL-qdlvldts). Case-folds before the table lookup since `scheme`
# may carry the caller's original casing (case_handling need not be
# "lower_host") -- mirrors the ftp-status scheme fold in
# .derive_parse_status_vec(). Returns NA for a scheme with no WHATWG default
# (ftps, NA, or anything outside .SCHEME_DEFAULT_PORTS).
.scheme_default_port_vec <- function(scheme) {
  unname(.SCHEME_DEFAULT_PORTS[.ascii_tolower(scheme)])
}

# Phase 13 port output: WHATWG parsing nulls a port that equals the special
# scheme's default, so the public parse-result `port` column must use that
# effective value for parity. `port_handling = "keep"` is the explicit
# non-parity escape hatch for callers that need to retain the syntactic port.
.apply_port_output_policy_vec <- function(scheme, port, port_handling,
                                          url_standard) {
  if (is.null(port) ||
      !.is_whatwg(url_standard) ||
      identical(port_handling, "keep")) {
    return(port)
  }

  out <- port
  has_port <- !is.na(out)
  if (!any(has_port)) {
    return(out)
  }

  default_port <- .scheme_default_port_vec(scheme)
  scheme_lc <- .ascii_tolower(scheme)
  elide <- has_port &
    !is.na(default_port) &
    out == default_port &
    scheme_lc %in% .WHATWG_SPECIAL_SCHEMES
  out[elide] <- NA_integer_
  out
}

# Phase 11 port component (PRD v2 D1, RURL-qdlvldts): "" (excluded) or
# ":<port>" per the standalone `port_handling` knob. `strip_default` is the
# WHATWG/RFC-style default-port elision renderer. `keep` is literal, including
# under `url_standard = "whatwg"`; callers use it as an explicit non-parity
# override when they need to retain a syntactic default port. `scheme` keys the
# default-port table only -- it is never rendered here (the caller already
# embeds the cased scheme in `scheme_part`). Callers pass the PARSED scheme
# (RUL-016): under `protocol_handling = "https"` the rendered scheme is not
# the one whose default port the input carried (RFC 3986 sec 6.2.3; WHATWG
# port state), and judging on it folded `http://host:443` into
# `https://host` while resurrecting `http://host:80` as `https://host:80`.
.build_port_part_vec <- function(scheme, port, port_handling, url_standard) {
  # Retained for call-site stability; port rendering is selected entirely by
  # `port_handling` now that `keep` is the literal override in every profile.
  force(url_standard)
  n <- length(scheme)
  port_part <- rep("", n)
  if (is.null(port) ||
        port_handling %in% c("exclude", "strip_all")) {
    return(port_part)
  }
  has_port <- !is.na(port)
  if (!any(has_port)) {
    return(port_part)
  }

  default_port <- .scheme_default_port_vec(scheme)
  is_default <- has_port & !is.na(default_port) & port == default_port

  if (identical(port_handling, "strip_default")) {
    keep <- has_port & !is_default
  } else {
    keep <- has_port
  }
  port_part[keep] <- paste0(":", port[keep])
  port_part
}

# Phase 11 (vector): reconstruct the canonical "clean" URL from cased
# components, then append the filtered query. NA/empty host yields NA.
# `query` is a per-row canonical query WITHOUT a leading "?" (from
# .filter_query_vec); "" means no query for that row. It is appended AFTER the
# cased scheme/host/path because query values are case-exempt (see
# ._parse_stage_b_vec). NULL `query` (the scalar wrapper and the phase unit
# tests) appends nothing, so clean_url stays scheme+host+path only. `port` is
# the raw (unfiltered) port column; NULL (the scalar wrapper and phase unit
# tests) or `port_handling = "exclude"` (the default) keeps clean_url
# port-free, exactly as before. `port_scheme` is the scheme whose default-port
# table `strip_default` consults (RUL-016): the PARSED scheme, which differs
# from `scheme_output` once `protocol_handling` has rewritten it. It defaults
# to `scheme_output` so the scalar wrapper and the phase unit tests, which
# never upgrade the scheme, are byte-identical.
.build_clean_url_vec <- function(scheme_output, host_output, path_output,
                                 trailing_slash_handling, query = NULL,
                                 port = NULL, port_handling = "exclude",
                                 url_standard = NULL,
                                 port_scheme = scheme_output) {
  n <- length(host_output)
  clean_url <- rep(NA_character_, n)
  has_host <- !is.na(host_output) & host_output != ""
  scheme_lc <- .ascii_tolower(scheme_output)
  is_file <- !is.na(scheme_lc) & scheme_lc == "file"
  buildable <- has_host | is_file
  if (!any(buildable)) {
    return(clean_url)
  }

  scheme_part <- ifelse(!is.na(scheme_output), paste0(scheme_output, "://"), "")
  port_part <- .build_port_part_vec(
    port_scheme, port, port_handling, url_standard
  )
  path_part <- ifelse(!is.na(path_output), path_output, "")
  # RUL-005 (ADR 0017 D1): the strip must not leave an output whose authority
  # is only dots. "http://./" would otherwise clean to "http://." -- a string
  # that is structurally valid but not display-oriented, so D2 row 5 does not
  # apply to a dots-only host. The separator is kept for `.`, `..`, `...` and
  # any run of dots; root-dot FQDNs ("a.", "example.com.") strip as before,
  # and a dots-only host with a non-empty path ("http://./x/") still strips.
  dots_only_host <- has_host & grepl("^\\.+$", host_output)
  if (trailing_slash_handling == "strip") {
    path_part[path_part == "/" & !dots_only_host] <- ""
  }
  host_part <- ifelse(has_host, host_output, "")

  # A hostless row renders as `scheme://` + path, so a non-empty path that does
  # not begin with "/" lands in the AUTHORITY position and fabricates a host:
  # "file:C:/W" measured as clean_url "file://C:/W" (authority "C:" -- an SMB
  # fetch on Windows), "file:etc/passwd" as "file://etc/passwd", "file:." as
  # "file://.". Only `file:` reaches here hostless (`buildable` above), and
  # such row is `parse_status = "error"`, so there is no canonical spelling to
  # emit: rurl does not fabricate one (RURL-hnddjptl). Same defect class the
  # WHATWG serializer already guards with `/.` for an absent host and a
  # "//"-leading path (see .serialize_whatwg_vec below) -- but that guard
  # rewrites a path it can still spell; an error row has no spelling at all.
  fabricates_authority <- !has_host & nzchar(path_part) &
    !startsWith(path_part, "/")
  buildable <- buildable & !fabricates_authority
  if (!any(buildable)) {
    return(clean_url)
  }

  clean_url[buildable] <- paste0(
    scheme_part[buildable], host_part[buildable], port_part[buildable],
    path_part[buildable]
  )
  if (!is.null(query)) {
    q_present <- buildable & !is.na(query) & nzchar(query)
    clean_url[q_present] <- paste0(clean_url[q_present], "?", query[q_present])
  }
  clean_url
}

# Phase 11 (scalar wrapper): delegates to .build_clean_url_vec(). Keeps the
# historical query-free signature (no `query` arg) as the default, so the
# scalar orchestrator and phase unit tests reconstruct scheme+host+path only
# unless a caller explicitly opts into the port arguments.
.build_clean_url <- function(scheme_output, host_output, path_output,
                             trailing_slash_handling, port = NULL,
                             port_handling = "exclude", url_standard = NULL) {
  .build_clean_url_vec(
    scheme_output, host_output, path_output, trailing_slash_handling,
    port = port, port_handling = port_handling, url_standard = url_standard
  )
}

# --- Layer 3b posture serializers (ADR 0012 D2 / Appendix A.1, RURL-mgmviuta) -
#
# These render a clean_url-style string for the non-special / opaque /
# empty-host / RFC-generic URL shapes that the (not-yet-public) `general`
# acceptance will produce. They are PURELY ADDITIVE: L4b activates `general`
# and wires them into the Phase-11 dispatch. Nothing here is called from the
# live pipeline, so byte-identity of every existing output is automatic.
# `.build_clean_url_vec` above is left verbatim as the special/host-present
# serializer L4b keeps using.
#
# Both follow rurl's clean_url product contract: the FRAGMENT is EXCLUDED, and
# an empty-but-PRESENT query serializes as a trailing "?". They take
# already-parsed state as explicit args (the state vocabulary from
# R/parse-state.R: host_kind, path_kind, query_kind, rfc_path_form) and use the
# repo's pre-allocate + logical-mask assignment idiom.

# Does the WHATWG serializer emit the `//` authority introducer for this row?
#
# The standard names ONE condition -- "if url's host is non-null, append //"
# (#concept-url-serializer step 2) -- so this is `host_kind != "absent"` and
# nothing else. The `authority_delimiter_present` syntactic fact is NOT
# consulted: it is a property of the SOURCE string, and WHATWG serializes the
# parsed URL, not the source.
#
# THIS REPLACES P1.2 D-C, which keyed the introducer off
# `authority_delimiter_present` because `host_kind` "cannot tell a
# delimiter-present empty authority from a delimiter-absent input". That
# premise was itself the defect (RURL-uhwivndf): the only rows where the two
# collapsed were `file:` rows whose host the parser recorded as NULL where
# WHATWG gives every special scheme a non-null -- here empty -- host. With the
# `file:` host model repaired in `.parse_whatwg_file_urls_vec()`, host_kind
# distinguishes them, and the two conditions agree on every WPT success row
# EXCEPT the ten this fixes. `authority_delimiter_present` stays in the record
# and stays load-bearing for the RFC serializers, where the source spelling IS
# the fact being rendered.
.whatwg_authority_emitted <- function(host_kind) {
  !is.na(host_kind) & host_kind != "absent"
}

# WHATWG serializer (ADR 0012 A.1 #concept-url-serializer + D2). Emits the `//`
# authority introducer per `.whatwg_authority_emitted()` -- the standard's own
# condition, a NON-NULL host. The CONTENT of the authority (host, port) is
# driven by the respective component states, and the four-condition `/.` guard
# keys off a null HOST, which is what the WHATWG serializer's own condition
# names.
#
# Reuses the existing byte-level encoders
# (R/path-query.R): opaque paths take the C0-control set only
# (`.whatwg_component_percent_encode(path, integer(0))`), list paths take the
# path percent-encode set (`.whatwg_path_percent_encode`), and both already
# preserve existing `%xx` spellings. `port` may be NULL (no port) or a vector.
.serialize_whatwg_vec <- function(scheme, host, host_kind,
                                  authority_delimiter_present, path, path_kind,
                                  query, query_kind, port, port_handling,
                                  trailing_slash_handling,
                                  port_scheme = scheme) {
  # Part of the state the caller hands the serializer, but not consulted: the
  # `//` introducer follows the host, not the source spelling. force() marks it
  # deliberately consumed, as .serialize_rfc_generic_vec() does for host_kind.
  force(authority_delimiter_present)
  n <- max(
    length(scheme), length(host), length(host_kind),
    length(authority_delimiter_present), length(path),
    length(path_kind), length(query), length(query_kind)
  )
  scheme <- rep_len(scheme, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
  path <- rep_len(path, n)
  path_kind <- rep_len(path_kind, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  if (!is.null(port)) {
    port <- rep_len(port, n)
  }
  port_scheme <- rep_len(port_scheme, n)

  scheme_prefix <- paste0(scheme, ":")
  # `port_scheme` (RUL-016): the parsed scheme keys the default-port table.
  port_part <- .build_port_part_vec(port_scheme, port, port_handling, "whatwg")
  is_opaque <- path_kind == "opaque"

  # Body encoding + the `/.` guard, by mask: the byte encoders are vectorized
  # (R/path-query.R), so each path kind is one encoder call.
  path_body <- character(n)
  guard <- rep("", n)
  # Opaque path: WHATWG C0-control percent-encode set (empty extra set ->
  # encodes only C0/DEL/non-ASCII). No `/.` guard for opaque paths.
  opaque_rows <- !is.na(path) & !is.na(is_opaque) & is_opaque
  path_body[opaque_rows] <- .whatwg_component_percent_encode(
    path[opaque_rows], integer(0)
  )
  # List path. Optional trailing-slash strip mirrors .build_clean_url_vec
  # (a lone "/" -> ""); it only ever touches a size-1 path, never the guard.
  # It also mirrors the RUL-005 exception: a dots-only host ("foo://./",
  # host ".") keeps its separator, so the output never reads "foo://.".
  list_rows <- !is.na(path) & !is.na(is_opaque) & !is_opaque
  p_render <- path[list_rows]
  if (identical(trailing_slash_handling, "strip")) {
    host_list <- host[list_rows]
    dots_only_host <- !is.na(host_list) &
      grepl("^\\.+$", host_list, useBytes = TRUE)
    p_render[p_render == "/" & !dots_only_host] <- ""
  }
  path_body[list_rows] <- .whatwg_path_percent_encode(p_render)

  # Four-condition `/.` guard (ADR 0012 A.1 #concept-url-serializer): fires
  # when host is null (host_kind "absent"), the path is a list (not opaque),
  # its size is > 1, and its FIRST segment is empty -- i.e. the serialized
  # path would begin with "//" and be misread as an authority.
  #
  # SEGMENT DERIVATION: rurl holds a list path as its serialized STRING (each
  # WHATWG path segment rendered as "/" + segment, so a rooted list path
  # always begins with "/"). On that rendering the two path conditions
  # collapse into ONE string test: the first segment is empty exactly when
  # the string begins with "//", and any such string carries at least two
  # "/" -- hence at least two segments -- so `size > 1` is implied and need
  # not be tested separately. "/bar" -> ["bar"]; "//bar" -> ["","bar"];
  # "//" -> ["",""]; "/" -> [""].
  #
  # Do NOT re-derive this by splitting on "/": `strsplit` drops a single
  # TRAILING "", so a path of exactly "//" (WHATWG list ["",""], size 2)
  # measures as size 1 and the guard misses -- emitting `non-spec://`, which
  # re-reads as an empty AUTHORITY rather than a path. That was a real defect
  # against WPT rows `non-spec:/.//`, `non-spec:/..//` and `non-spec:/a/..//`.
  # Every parsed record carries a host kind; an NA here would silently skip
  # the guard and emit a `//` path as an authority, so fail loudly instead.
  stopifnot(!anyNA(host_kind))
  null_host <- host_kind == "absent"
  guard[list_rows & null_host & startsWith(path, "//")] <- "/."

  query_suffix <- .whatwg_query_suffix_vec(query, query_kind, scheme)

  out <- character(n)
  out[is_opaque] <- paste0(scheme_prefix[is_opaque], path_body[is_opaque])

  # List path under a NON-NULL host: emit `//` + host (host may be "") + port.
  # `foo:///bar` = "foo://" + "" + "/bar".
  delim <- .whatwg_authority_emitted(host_kind)
  auth <- !is_opaque & delim
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", host_str[auth], port_part[auth], path_body[auth]
  )

  # List path with no delimiter: NO `//`; the `/.` guard, if it fired, sits
  # between the scheme and the path. `foo:/bar` -> "foo:/bar".
  noauth <- !is_opaque & !delim
  out[noauth] <- paste0(
    scheme_prefix[noauth], guard[noauth], path_body[noauth]
  )

  paste0(out, query_suffix)
}

# RFC 3986 generic serializer (ADR 0012 D1/D2). NO normalization, NO
# opaque/list distinction, NO `/.` guard, NO dot-segment removal, NO case
# folding: it is a faithful generic serialization of the path it is handed.
# Under rfc-syntax that is the source path; under the `rfc3986` bundle the
# caller hands it the normalized one (RUL-027). `//` is emitted iff
# `authority_delimiter_present` (P1.2 D-C), so `host_kind` -- like
# `rfc_path_form` -- is now informational under this posture: the path string
# is already in its final source-preserving shape and host presence no longer
# decides the authority introducer, so nothing structural keys off either here
# (force() marks them deliberately consumed, mirroring .build_port_part_vec's
# force(url_standard)). Both stay in the signature because they are part of the
# state the caller hands the serializer. The query is
# preserved verbatim (no percent-encoder) under rfc-syntax's "preserve source"
# disclaimer. `port` may be NULL or a vector.
.serialize_rfc_generic_vec <- function(scheme, host, host_kind,
                                       authority_delimiter_present, path,
                                       rfc_path_form, query, query_kind,
                                       port, port_handling,
                                       port_scheme = scheme) {
  force(rfc_path_form)
  force(host_kind)
  n <- max(
    length(scheme), length(host), length(host_kind),
    length(authority_delimiter_present), length(path),
    length(query), length(query_kind)
  )
  scheme <- rep_len(scheme, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
  path <- rep_len(path, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  if (!is.null(port)) {
    port <- rep_len(port, n)
  }
  port_scheme <- rep_len(port_scheme, n)

  scheme_prefix <- paste0(scheme, ":")
  # `port_scheme` (RUL-016): the parsed scheme keys the default-port table.
  port_part <- .build_port_part_vec(
    port_scheme, port, port_handling, "rfc3986"
  )
  path_body <- ifelse(is.na(path), "", path) # source-preserving; no encoding

  out <- character(n)
  auth <- !is.na(authority_delimiter_present) & authority_delimiter_present
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", host_str[auth], port_part[auth], path_body[auth]
  )
  out[!auth] <- paste0(scheme_prefix[!auth], path_body[!auth])

  # rfc-syntax preserves source bytes: append the query delimiter/value
  # verbatim rather than through a WHATWG percent-encoder. present -> "?query";
  # empty -> bare "?"; absent -> nothing. Fragment always excluded.
  query_suffix <- rep("", n)
  is_present <- query_kind == "present"
  query_suffix[is_present] <- paste0("?", ifelse(
    is.na(query[is_present]), "", query[is_present]
  ))
  query_suffix[query_kind == "empty"] <- "?"

  paste0(out, query_suffix)
}

# Shared WHATWG query suffix (ADR 0012 D2): present -> "?" + special-ness-keyed
# percent-encode; empty -> bare "?"; absent -> nothing. Fragment excluded.
.whatwg_query_suffix_vec <- function(query, query_kind, scheme) {
  n <- length(query_kind)
  suffix <- rep("", n)
  suffix[!is.na(query_kind) & query_kind == "empty"] <- "?"
  present <- which(query_kind == "present")
  if (length(present) > 0L) {
    encoded <- .whatwg_query_percent_encode(query[present], scheme[present])
    encoded[is.na(encoded)] <- ""
    suffix[present] <- paste0("?", encoded)
  }
  suffix
}

# ---------------------------------------------------------------------------
# FSSS -- the full-string standard serializers (output surface (b)).
#
# These are the STANDARD serializers. The `.serialize_*_vec()` pair above are
# the CLEAN serializers (surface c): they implement rurl's `clean_url` product
# contract, which excludes the fragment and credentials by design. The two are
# separate surfaces, not versions of one thing (output-contracts.md, C-04).
#
# Three properties distinguish the FSSS, all contractual:
#
#   1. FULL-STRING. Credentials and the fragment are emitted, and a delimiter
#      that was present with an empty value survives as a trailing `?` / `#`
#      (ADR 0012 D2).
#   2. IDENTITY, NOT PRESENTATION. They take NO presentation dial -- no
#      `port_handling`, no `trailing_slash_handling`, no `path_encoding`. The
#      path is rendered by the selected standard's own percent-encode set and
#      `path_encoding` is never consulted (output-contracts.md, C-05; ADR 0011).
#      Passing one would be a category error, so there is no argument to pass.
#   3. LOSSLESS RECORD IN. They consume the lossless serializer-input record
#      (the R/parse-state.R vocabulary), never the 18-field public projection
#      and never a cleaned or formatted component (S3-F2).
#
# `port` is the identity port and is emitted whenever it is non-NA. The
# public projection's default-port nulling is a projection policy applied
# downstream; it is not this surface's business.

# WHATWG full-string serializer (#concept-url-serializer, ADR 0012 A.1 + D2).
#
# CREDENTIALS ARE SPEC-EXACT, WHICH MEANS LOSSY, AND THAT IS CORRECT HERE.
# WHATWG appends credentials iff the URL "includes credentials" -- username or
# password non-empty -- so a bare `http://@h/` serializes as `http://h/` and
# `http://u:@h/` as `http://u@h/`, both pinned by WPT. The undivided source
# userinfo is NOT used: this serializer emits the standard's own serialization
# of the parsed state, and WHATWG's state is the username/password split. The
# LOSSLESSNESS the contract requires lives in the RECORD (which keeps the
# undivided slice), not in this serializer's output; the RFC source-preserving
# form below is where the undivided spelling is rendered verbatim.
#
# Round-tripping is unaffected: the identity oracle is idempotence of
# parse -> serialize -- serialize(parse(serialize(parse(x)))) equals
# serialize(parse(x)) -- not recovery of the input bytes, which is surface (a).
.serialize_whatwg_full_vec <- function(scheme, userinfo, host, host_kind,
                                       authority_delimiter_present, path,
                                       path_kind, query, query_kind,
                                       fragment, fragment_kind, port) {
  # Not consulted; see .serialize_whatwg_vec(). The `//` introducer follows the
  # parsed host, which is what #concept-url-serializer names.
  force(authority_delimiter_present)
  n <- max(
    length(scheme), length(userinfo), length(host), length(host_kind),
    length(authority_delimiter_present), length(path), length(path_kind),
    length(query), length(query_kind), length(fragment), length(fragment_kind)
  )
  scheme <- rep_len(scheme, n)
  userinfo <- rep_len(userinfo, n)
  host <- rep_len(host, n)
  host_kind <- rep_len(host_kind, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
  path <- rep_len(path, n)
  path_kind <- rep_len(path_kind, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  fragment <- rep_len(fragment, n)
  fragment_kind <- rep_len(fragment_kind, n)
  port <- if (is.null(port)) rep(NA_character_, n) else rep_len(port, n)

  scheme_prefix <- paste0(scheme, ":")
  is_opaque <- path_kind == "opaque"

  # Identity port: emitted whenever present. No `port_handling`.
  port_part <- rep("", n)
  has_port <- !is.na(port)
  port_part[has_port] <- paste0(":", port[has_port])

  # Credentials: split the recorded userinfo at the FIRST ":" into the WHATWG
  # username/password pair, then apply #concept-url-serializer. The split is
  # on bytes: `:` is ASCII, so it never sits inside a multi-byte sequence and
  # the first `:` octet is the first `:` code point.
  cred_part <- character(n)
  has_ui <- !is.na(userinfo)
  ui <- userinfo[has_ui]
  has_colon <- grepl(":", ui, fixed = TRUE, useBytes = TRUE)
  username <- sub("(?s):.*", "", ui, perl = TRUE, useBytes = TRUE)
  password <- rep("", length(ui))
  password[has_colon] <- sub(
    "(?s)^[^:]*:", "", ui[has_colon], perl = TRUE, useBytes = TRUE
  )
  # "includes credentials" is false when both halves are empty: WHATWG then
  # drops the whole userinfo, delimiter included.
  includes <- nzchar(username) | nzchar(password)
  out_cred <- .whatwg_userinfo_percent_encode(username)
  with_pw <- nzchar(password)
  out_cred[with_pw] <- paste0(
    out_cred[with_pw], ":", .whatwg_userinfo_percent_encode(password[with_pw])
  )
  cred_ui <- rep("", length(ui))
  cred_ui[includes] <- paste0(out_cred[includes], "@")
  cred_part[has_ui] <- cred_ui

  path_body <- character(n)
  guard <- rep("", n)
  # An opaque path's trailing space is encoded only when a `?`/`#` actually
  # follows it. Unlike the clean serializer, the FSSS emits the fragment too,
  # so the fragment counts as a following delimiter here.
  opaque_rows <- !is.na(path) & !is.na(is_opaque) & is_opaque
  delimiter_follows <- query_kind != "absent" | fragment_kind != "absent"
  path_body[opaque_rows] <- .whatwg_opaque_path_encode(
    path[opaque_rows], delimiter_follows[opaque_rows]
  )
  list_rows <- !is.na(path) & !is.na(is_opaque) & !is_opaque
  path_body[list_rows] <- .whatwg_path_percent_encode(path[list_rows])

  # Four-condition `/.` guard -- identical to the clean serializer's, but
  # with no trailing-slash strip ahead of it, since that is a presentation
  # dial this surface does not take. See .serialize_whatwg_vec for the
  # segment-derivation reasoning, including why this must NOT be re-derived
  # by splitting on "/".
  # Every parsed record carries a host kind; an NA here would silently skip
  # the guard and emit a `//` path as an authority, so fail loudly instead.
  stopifnot(!anyNA(host_kind))
  null_host <- host_kind == "absent"
  guard[list_rows & null_host & startsWith(path, "//")] <- "/."

  out <- character(n)
  out[is_opaque] <- paste0(scheme_prefix[is_opaque], path_body[is_opaque])

  delim <- .whatwg_authority_emitted(host_kind)
  auth <- !is_opaque & delim
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", cred_part[auth], host_str[auth],
    port_part[auth], path_body[auth]
  )

  noauth <- !is_opaque & !delim
  out[noauth] <- paste0(
    scheme_prefix[noauth], guard[noauth], path_body[noauth]
  )

  paste0(
    out,
    .whatwg_query_suffix_vec(query, query_kind, scheme),
    .whatwg_fragment_suffix_vec(fragment, fragment_kind)
  )
}

# WHATWG fragment suffix (ADR 0012 D2), mirroring .whatwg_query_suffix_vec:
# present -> "#" + fragment-set encode; empty -> bare "#"; absent -> nothing.
# The empty case is the whole point -- `http://h/#` must not collapse to
# `http://h/`.
.whatwg_fragment_suffix_vec <- function(fragment, fragment_kind) {
  n <- length(fragment_kind)
  suffix <- rep("", n)
  suffix[!is.na(fragment_kind) & fragment_kind == "empty"] <- "#"
  present <- which(fragment_kind == "present")
  if (length(present) > 0L) {
    encoded <- .whatwg_fragment_percent_encode(fragment[present])
    encoded[is.na(encoded)] <- ""
    suffix[present] <- paste0("#", encoded)
  }
  suffix
}

# RFC 3986 full-string serializer (section 5.3 component recomposition), in
# BOTH postures the contract leaves open (OUT-O3):
#
#   form = "source"     -- rfc-syntax. No normalization of any kind: source
#                          bytes are preserved, the undivided userinfo slice is
#                          emitted verbatim (RFC 3986 has no credential concept
#                          to be spec-exact about, so nothing is dropped), and
#                          the query/fragment are appended without an encoder.
#                          This is the posture round-trip fidelity needs.
#   form = "normalized" -- section 6.2.2 syntax-based normalization plus the
#                          section 6.2.3 default-port removal: case
#                          normalization of scheme and host, percent-encoding
#                          normalization (triplets upper-cased, unreserved
#                          octets decoded), and path segment normalization.
#                          This is the posture a conformance claim needs.
#
# Both are exposed rather than one being chosen, because choosing forfeits
# either the round-trip oracle or the claim substrate.
.serialize_rfc_full_vec <- function(scheme, userinfo, host, host_kind,
                                    authority_delimiter_present, path,
                                    rfc_path_form, query, query_kind,
                                    fragment, fragment_kind, port,
                                    form = "source") {
  force(rfc_path_form)
  force(host_kind)
  n <- max(
    length(scheme), length(userinfo), length(host),
    length(authority_delimiter_present), length(path), length(query),
    length(query_kind), length(fragment), length(fragment_kind)
  )
  scheme <- rep_len(scheme, n)
  userinfo <- rep_len(userinfo, n)
  host <- rep_len(host, n)
  authority_delimiter_present <- rep_len(authority_delimiter_present, n)
  path <- rep_len(path, n)
  query <- rep_len(query, n)
  query_kind <- rep_len(query_kind, n)
  fragment <- rep_len(fragment, n)
  fragment_kind <- rep_len(fragment_kind, n)
  port <- if (is.null(port)) rep(NA_character_, n) else rep_len(port, n)

  normalized <- identical(form, "normalized")

  if (normalized) {
    scheme <- .ascii_tolower(scheme)
    # The host takes all three rules, in the order the section states them:
    # 6.2.2.2 decodes the triplets encoding an unreserved octet, 6.2.2.1
    # case-folds the reg-name (including any octet that decoding just
    # revealed), and the hex digits of every triplet that survived are rendered
    # uppercase. The last step is separate because case folding lowercases the
    # triplet too, so the host was the one component whose triplets escaped the
    # hex normalization the path/query/fragment below already get
    # (RURL-savatsuc); the first is here rather than in the parse because
    # 6.2.2.2 is normalization, and applying it during the parse made the
    # decoding depend on the SCHEME instead of the requested form
    # (RURL-xkhbhaje).
    host <- .pct_hex_upper(.ascii_tolower(.rfc_pct_normalize(host)))
    path <- .rfc_pct_normalize(path)
    # A raw byte >= 0x80 in the query or fragment is percent-encoded HERE, not
    # on the parse record (RUL-015): section 2.1 makes the triplet a
    # representation of the octet, so the `source` form hands the octet back
    # as written and the `normalized` form renders it as the ASCII grammar
    # requires. Before RUL-015 the web-route record already carried the
    # encoding, so for that route this is byte-identical; the general route
    # (`foo://h/p?q=<C3><BC>`) never encoded, and now agrees with it. The path
    # is deliberately not touched: its raw byte has always been rendered raw
    # in both forms, and moving it is not this ruling's question.
    query <- .rfc_pct_normalize(.rfc_pct_encode_high(query))
    fragment <- .rfc_pct_normalize(.rfc_pct_encode_high(fragment))
    userinfo <- .rfc_pct_normalize(userinfo)
    # Path segment normalization (section 6.2.2.3) applies to every non-empty
    # path, rootless included (RUL-028): section 5.2.2 runs remove_dot_segments
    # on any reference that has a scheme, and Appendix D.2 applies it "to all
    # URI reference paths". So `foo:a/../b` -> `foo:/b`, as resolve_url() and
    # the rfc3986 bundle's general-route clean_url (RUL-027) render it. That is
    # generic equivalence only; a scheme may disagree (section 6.1).
    #
    # Only a path holding a dot segment is handed to the scalar algorithm: on
    # any other path it is the identity, and it is quadratic in the segment
    # count, which a large rootless `data:` URI would otherwise pay. A `%2E`
    # spelling was decoded to "." by the 6.2.2.2 step above.
    seg_norm <- !is.na(path) & grepl("(^|/)\\.\\.?(/|$)", path)
    if (any(seg_norm)) {
      dotless <- vapply(
        path[seg_norm], ._remove_dot_segments, character(1), USE.NAMES = FALSE
      )
      # Section 3.3: "When authority is not present, the path cannot begin
      # with two slash characters". Removing dots can produce one
      # (`foo:/a/..//p` -> "//p"), and RFC 3986 has no escape for it, so
      # `foo://p` came out, naming a host the URI never had (RURL-dsjqseap).
      # Such a path keeps its dot segments, as the clean_url does
      # (RURL-rwlrcvnh).
      adp <- authority_delimiter_present[seg_norm]
      hostless <- is.na(adp) | !adp
      names_a_host <- hostless & startsWith(dotless, "//")
      dotless[names_a_host] <- path[seg_norm][names_a_host]
      path[seg_norm] <- dotless
    }
    # Section 6.2.3, verbatim: "a URI that uses the generic syntax for
    # authority with an empty path should be normalized to a path of '/'".
    # This is the ONLY home for that "/" -- the parse leaves `path-abempty`'s
    # empty match empty (RURL-epoinamh), so without this step the normalized
    # form would lose a normalization the RFC states.
    #
    # Keyed on the AUTHORITY delimiter rather than on the scheme, because that
    # is what the sentence is keyed on: it fires on a non-special scheme too,
    # and not at all on a URI with no authority, which has no `path-abempty`
    # and so no sentence to apply. `urn://:443` therefore normalizes to
    # `urn://:443/`, and that is deliberate -- ADR 0012 rules that
    # scheme-specific restrictions are overlays, not generic parse gates, so a
    # string being an invalid URN (RFC 8141) does not change its RFC 3986
    # generic-normalization spelling. Reporting that violation is the
    # RURL-eqrpggvz reader's job, not this dial's.
    empty_abempty <- !is.na(authority_delimiter_present) &
      authority_delimiter_present & (is.na(path) | !nzchar(path))
    path[empty_abempty] <- "/"
  }

  scheme_prefix <- paste0(scheme, ":")

  port_part <- rep("", n)
  has_port <- !is.na(port)
  if (normalized) {
    # Section 6.2.3: a port equal to the scheme's default is elided.
    default_port <- .scheme_default_port_vec(scheme)
    has_port <- has_port & !(!is.na(default_port) & port == default_port)
  }
  port_part[has_port] <- paste0(":", port[has_port])

  # RFC has no username/password split: the undivided slice is the component,
  # so every delimiter state (`u@`, `u:@`, `:p@`, `@`) survives verbatim.
  cred_part <- rep("", n)
  has_cred <- !is.na(userinfo)
  cred_part[has_cred] <- paste0(userinfo[has_cred], "@")

  path_body <- ifelse(is.na(path), "", path)

  out <- character(n)
  auth <- !is.na(authority_delimiter_present) & authority_delimiter_present
  host_str <- ifelse(is.na(host), "", host)
  out[auth] <- paste0(
    scheme_prefix[auth], "//", cred_part[auth], host_str[auth],
    port_part[auth], path_body[auth]
  )
  out[!auth] <- paste0(scheme_prefix[!auth], path_body[!auth])

  query_suffix <- rep("", n)
  is_present <- query_kind == "present"
  query_suffix[is_present] <- paste0("?", ifelse(
    is.na(query[is_present]), "", query[is_present]
  ))
  query_suffix[query_kind == "empty"] <- "?"

  fragment_suffix <- rep("", n)
  frag_present <- fragment_kind == "present"
  fragment_suffix[frag_present] <- paste0("#", ifelse(
    is.na(fragment[frag_present]), "", fragment[frag_present]
  ))
  fragment_suffix[fragment_kind == "empty"] <- "#"

  paste0(out, query_suffix, fragment_suffix)
}

# RFC 3986 section 6.2.2.1 + 6.2.2.2 percent-encoding normalization, vectorized
# over the scalar .rfc_unreserved_normalize() (R/path-query.R), which already
# does both halves: it decodes triplets encoding an unreserved octet and
# upper-cases the hex digits of every triplet it leaves encoded.
.rfc_pct_normalize <- function(x) {
  out <- x
  keep <- !is.na(x)
  if (!any(keep)) {
    return(x)
  }
  out[keep] <- vapply(
    x[keep], .rfc_unreserved_normalize, character(1), USE.NAMES = FALSE
  )
  out
}

# RFC 3986 section 2.1 rendering of a raw octet >= 0x80 as its uppercase
# triplet, vectorized over the scalar `.web_escape_high_bytes()`
# (R/parse-web.R). Every other byte -- an existing triplet included -- is left
# as written; the hex-case fold is `.rfc_pct_normalize()`'s job, applied after.
.rfc_pct_encode_high <- function(x) {
  out <- x
  keep <- !is.na(x)
  if (!any(keep)) {
    return(x)
  }
  out[keep] <- vapply(
    x[keep], .web_escape_high_bytes, character(1), USE.NAMES = FALSE
  )
  out
}

# Phase 12 (vector): classify the parse outcome (ok / ok-ftp / warning-* /
# error / ok-scheme-relative). `web_ok` is TRUE for rows the web parser read
# (the
# scalar wrapper passes !is.null(parsed_web)).
#
# The status is now DERIVED, not decided here: Phase 12 computes the three
# independent verdict layers (R/verdicts.R) and projects them through pi. That
# projection reproduces the historical cascade exactly -- including the two
# places the old code let a later fact overwrite an earlier one (an FTP scheme
# only reached `ok-ftp` when the PSL cascade had left the row `ok`; a
# scheme-relative row likewise) -- because pi considers the L3 annotation rows
# BEFORE the L2 accept sub-states. Keeping one status-deciding path is what
# makes `get_parse_verdicts()` incapable of drifting from `parse_status`.
#
# ADR 0012 D5 (RURL-qbnelzku): a general-routed opaque / non-special-authority
# row is `ok` and EXEMPT from the reg-name host_has_dot / tld / domain cascade
# -- an opaque or arbitrary-scheme host has no PSL domain, so it must NOT
# become warning-no-tld. Empty under "web" (no general row exists there), so
# this is a pure no-op for the default posture.
.derive_parse_status_vec <- function(web_ok, final_host, is_ip_host, tld,
                                     domain, protocol_handling, final_scheme,
                                     looks_like_protocol,
                                     original_has_allowed_scheme,
                                     looks_like_host_port,
                                     is_scheme_relative,
                                     scheme_relative_handling,
                                     rfc3986_path_rootless = NULL,
                                     scheme_acceptance = "web",
                                     is_general = NULL,
                                     scheme_less_userinfo = NULL) {
  .project_parse_status_vec(.derive_verdict_layers_vec(
    web_ok = web_ok,
    final_host = final_host,
    is_ip_host = is_ip_host,
    tld = tld,
    domain = domain,
    protocol_handling = protocol_handling,
    final_scheme = final_scheme,
    looks_like_protocol = looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    looks_like_host_port = looks_like_host_port,
    is_scheme_relative = is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling,
    rfc3986_path_rootless = rfc3986_path_rootless,
    scheme_acceptance = scheme_acceptance,
    is_general = is_general,
    scheme_less_userinfo = scheme_less_userinfo
  ))
}

# Phase 12 (scalar wrapper): delegates to .derive_parse_status_vec().
.derive_parse_status <- function(parsed_web, final_host, is_ip_host, tld,
                                 domain, protocol_handling, final_scheme,
                                 looks_like_protocol,
                                 original_has_allowed_scheme,
                                 looks_like_host_port,
                                 is_scheme_relative,
                                 scheme_relative_handling,
                                 rfc3986_path_rootless = NULL,
                                 scheme_less_userinfo = NULL) {
  .derive_parse_status_vec(
    web_ok = !is.null(parsed_web),
    final_host = final_host,
    is_ip_host = is_ip_host,
    tld = tld,
    domain = domain,
    protocol_handling = protocol_handling,
    final_scheme = final_scheme,
    looks_like_protocol = looks_like_protocol,
    original_has_allowed_scheme = original_has_allowed_scheme,
    looks_like_host_port = looks_like_host_port,
    is_scheme_relative = is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling,
    rfc3986_path_rootless = rfc3986_path_rootless,
    scheme_less_userinfo = scheme_less_userinfo
  )
}

# Phase 13 (vector): build the 14 result columns. Coerces the scheme-relative
# "keep" scheme to NA and an empty host to NA, matching the scalar assembler.
# The engine applies error-row defaults for NULL-equivalent rows afterward.
.assemble_parse_result_vec <- function(original_url, scheme_output, host_output,
                                       port, path_output, raw_query, fragment,
                                       user, password, domain, tld,
                                       domain_ascii, domain_unicode,
                                       tld_ascii, tld_unicode, is_ip_host,
                                       clean_url, parse_status,
                                       is_scheme_relative,
                                       scheme_relative_handling) {
  scheme_return <- scheme_output
  if (scheme_relative_handling == "keep") {
    scheme_return[is_scheme_relative] <- NA_character_
  }

  host_return <- host_output
  host_return[is.na(host_output) | host_output == ""] <- NA_character_

  list(
    original_url = original_url,
    scheme = scheme_return,
    host = host_return,
    port = port,
    path = path_output,
    query = raw_query,
    # fragment/user/password are never percent-DECODED: with `decode = FALSE`
    # the web parser never decodes them, keeping them
    # consistent with the raw path/query. Under `url_standard = "whatwg"` the
    # caller has already re-encoded these three with their WHATWG percent-encode
    # sets (fragment / userinfo) before they reach this assembler; under
    # `rfc3986` or no selector they are the raw source spelling.
    fragment = fragment,
    user = user,
    password = password,
    domain = domain,
    tld = tld,
    domain_ascii = domain_ascii,
    domain_unicode = domain_unicode,
    tld_ascii = tld_ascii,
    tld_unicode = tld_unicode,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status
  )
}

# Phase 13 (scalar wrapper): extracts port/fragment/user/password from the parse
# object, then delegates to .assemble_parse_result_vec().
.assemble_parse_result <- function(original_input_url, scheme_output,
                                   host_output, parsed_web, path_output,
                                   raw_query, domain, tld,
                                   domain_ascii, domain_unicode,
                                   tld_ascii, tld_unicode, is_ip_host,
                                   clean_url, parse_status, is_scheme_relative,
                                   scheme_relative_handling) {
  .assemble_parse_result_vec(
    original_url = original_input_url,
    scheme_output = scheme_output,
    host_output = host_output,
    port = suppressWarnings(as.integer(.if_null(parsed_web$port, NA_integer_))),
    path_output = path_output,
    # .blank_to_na(): present-but-empty raw components "" -> NA (see utils.R).
    raw_query = .blank_to_na(.if_null(raw_query, NA_character_)),
    fragment = .blank_to_na(.if_null(parsed_web$fragment, NA_character_)),
    user = .blank_to_na(.if_null(parsed_web$user, NA_character_)),
    password = .blank_to_na(.if_null(parsed_web$password, NA_character_)),
    domain = domain,
    tld = tld,
    domain_ascii = domain_ascii,
    domain_unicode = domain_unicode,
    tld_ascii = tld_ascii,
    tld_unicode = tld_unicode,
    is_ip_host = is_ip_host,
    clean_url = clean_url,
    parse_status = parse_status,
    is_scheme_relative = is_scheme_relative,
    scheme_relative_handling = scheme_relative_handling
  )
}
