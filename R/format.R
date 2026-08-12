# Output surface (d) -- SAFE DISPLAY.
#
# The human-facing formatter. It renders a URL for a PERSON to read: userinfo
# redacted, spoofable and invisible code points made visible, and both host
# spellings shown whenever they differ.
#
# Surface discipline (design/work/url-v3/contracts/output-contracts.md, and
# decisions/P2.7-display-and-resolver-output.md D-D, which is this file's
# executable specification):
#
#   * The result is NOT a URL. `output-contracts.md:209` SETTLED surface (d) as
#     NOT REPARSABLE -- it "never re-enters serialization" and carries no
#     round-trip and no identity guarantee. That cap is what buys the `<U+XXXX>`
#     escape tokens and the trailing annotation block below; without it every
#     one of them would be a round-trip violation. Nothing here may be fed to
#     `serialize_url()`, to a mutation baseline, or to `url_key()` (P2.2 §5(1)).
#   * NO PRESENTATION DIAL reaches this surface. Not `path_encoding`, not
#     `host_encoding`, not the query dials. The escape matrix below IS the
#     presentation rule, and accepting `path_encoding = "decode"` would hand a
#     caller back the exact `/a%2Fb` -> `/a/b` hazard rule E4 exists to prevent.
#   * The record is built from the LOSSLESS SERIALIZER-INPUT RECORD
#     (`.fsss_record_vec()`, R/serialize.R), not from the 18-field public
#     projection: `.blank_to_na()` collapses "" to NA on the way out of Stage A,
#     which destroys the empty-vs-absent distinction a full-string surface needs
#     (S3-F2). Surface (d) is a member of the full-string family (VD-003).
#
# PARSE POSTURE. This surface parses under WHATWG, and takes no `standard`
# argument. Two reasons, both structural rather than aesthetic:
#
#   * Every rule in D-D's matrix is standard-independent -- the escape set, the
#     redaction token and the annotation block say nothing about which standard
#     was asked for -- so a `standard` dial would vary only the parse, not the
#     display contract it selects.
#   * The host rule needs ONE direction. Under WHATWG the parse's host IS the
#     A-label (`.fsss_record_vec()` applies `host_encoding = "idna"` because
#     that is the standard's own host rendering), so the Unicode spelling is a
#     DECODE of a host rurl already decided on, and P2.7 §5's "no host, IDNA or
#     PSL semantics" non-scope holds literally: this file decides no spelling,
#     it decodes the one G3.H chose. Under `rfc3986` the record keeps the SOURCE
#     host spelling, so the same rule would have to ENCODE, i.e. pick an A-label
#     that no parse committed to.
#
# Adding `standard =` later is additive and breaks nothing; guessing at its
# semantics now would put an IDNA decision in the one file whose governing
# record forbids making one.

# --- E2: the escape enumerations ---------------------------------------------
#
# E2 performs NO runtime general-category lookup, and must not be "simplified"
# into one (P2.7 D-D). The obvious spelling -- escape `Cc`, `Cf`, `Cs`, `Co`,
# `Cn` -- makes this surface's output a function of the installed UNICODE DATA
# VERSION rather than of the URL: `Cn` (unassigned) shrinks with every Unicode
# release and `Cf` grows to match, so the same URL would format differently
# after a dependency bump, with no code change and no diff to notice it. The
# drift vector is live, not hypothetical: `DESCRIPTION` pins `punycoder
# (>= 1.2.0)` -- a lower bound, not a Unicode-version pin -- and
# `punycoder::unicode_versions()` already reports more than one version.
#
# So the two enumerations below are the whole of E2, and extending hazard
# coverage for a future Unicode release is a deliberate code change carrying a
# NEWS entry and a fixture diff -- a reviewable event, which a silent data bump
# is not.

# E2a -- immutable blocks. Membership of these ranges can never change under the
# Unicode Stability Policy (`Cc`, `Cs` and `Co` are fixed by it).
#
# The surrogate range is BELT-AND-BRACES and is unreachable through the byte
# walk below: a lone surrogate cannot occur in valid UTF-8, so it arrives as
# invalid bytes and E3 emits it as `%XX` before any code point exists. It is
# enumerated because D-D enumerates it -- an R string built from a `\uD800`
# escape is the case it names.
.FORMAT_IMMUTABLE_BLOCKS <- rbind(
  c(0x0000L, 0x001FL),   # C0 controls, incl. NUL (U+0000) and LF (U+000A)
  c(0x007FL, 0x009FL),   # DEL and the C1 controls
  c(0xD800L, 0xDFFFL),   # surrogates -- see E3
  c(0xE000L, 0xF8FFL),   # Private_Use
  c(0xF0000L, 0xFFFFDL), # Private_Use, plane 15
  c(0x100000L, 0x10FFFDL) # Private_Use, plane 16
)

# E2b -- the hazard list, a closed enumeration rurl owns. It includes the bidi
# FORMATTING characters U+202A-U+202E (LRE, RLE, PDF, LRO, RLO) and
# U+2066-U+2069 (LRI, RLI, FSI, PDI), the bidi MARKS U+200E/U+200F and U+061C,
# the zero-width joiners U+200B-U+200D, the soft hyphen and the byte-order mark.
#
# U+2065 is DELIBERATELY absent from the U+2060-U+206F sweep: it is unassigned,
# and E2c says an unassigned code point is not escaped for being unassigned. The
# gap is the enumeration's proof that no category lookup happens here.
.FORMAT_HAZARD_BLOCKS <- rbind(
  c(0x00ADL, 0x00ADL),   # SOFT HYPHEN
  c(0x061CL, 0x061CL),   # ARABIC LETTER MARK
  c(0x180EL, 0x180EL),   # MONGOLIAN VOWEL SEPARATOR
  c(0x200BL, 0x200FL),   # ZWSP, ZWNJ, ZWJ, LRM, RLM
  c(0x202AL, 0x202EL),   # LRE, RLE, PDF, LRO, RLO
  c(0x2060L, 0x2064L),   # WORD JOINER .. INVISIBLE PLUS
  c(0x2066L, 0x206FL),   # LRI, RLI, FSI, PDI, and the deprecated overrides
  c(0xFEFFL, 0xFEFFL),   # ZERO WIDTH NO-BREAK SPACE / BOM
  c(0xFFF9L, 0xFFFBL)    # interlinear annotation anchors
)

.FORMAT_ESCAPE_BLOCKS <- rbind(
  .FORMAT_IMMUTABLE_BLOCKS, .FORMAT_HAZARD_BLOCKS
)

# E4 -- delimiters that stay percent-encoded, so decoding never fabricates
# structure. The base set is the URL grammar's own; the query adds the three
# characters that are structural INSIDE a query.
.FORMAT_DELIMITERS <- utf8ToInt("/?#@:[]%\\")
.FORMAT_QUERY_DELIMITERS <- c(.FORMAT_DELIMITERS, utf8ToInt("&=+"))

.FORMAT_LT <- 0x3CL
.FORMAT_GT <- 0x3EL

# --- Byte-level tables -------------------------------------------------------

# Hex-digit lookup, indexed by byte + 1.
.FORMAT_IS_HEX <- local({
  v <- rep(FALSE, 256L)
  v[c(0x30:0x39, 0x41:0x46, 0x61:0x66) + 1L] <- TRUE
  v
})

# UTF-8 lead-byte tables, indexed by byte + 1: sequence length (0 = not a legal
# lead) and the bounds of the FIRST continuation byte. The bounds are what
# excludes overlong encodings (U+E0/U+F0), the surrogate block (U+ED) and
# everything above U+10FFFF (U+F4), so a sequence accepted here decodes to a
# scalar value and nothing else does.
.FORMAT_UTF8_LEN <- local({
  v <- integer(256L)
  v[(0x00:0x7FL) + 1L] <- 1L
  v[(0xC2:0xDFL) + 1L] <- 2L
  v[(0xE0:0xEFL) + 1L] <- 3L
  v[(0xF0:0xF4L) + 1L] <- 4L
  v
})

.FORMAT_CONT1_LO <- local({
  v <- rep(0x80L, 256L)
  v[0xE0L + 1L] <- 0xA0L
  v[0xF0L + 1L] <- 0x90L
  v
})

.FORMAT_CONT1_HI <- local({
  v <- rep(0xBFL, 256L)
  v[0xEDL + 1L] <- 0x9FL
  v[0xF4L + 1L] <- 0x8FL
  v
})

# A component needs the byte walk only if it holds a byte OTHER than printable
# ASCII minus `%`, `<` and `>`. Everything in that set is its own display form
# under every rule, so the ordinary path segment / query key skips the walk.
#
# Stated as a NEGATED class with no anchors on purpose. The anchored spelling
# `^[...]*$` reads as the same predicate and is not: PCRE's `$` also matches
# BEFORE a string-final newline, so a component ending in a raw LF would be
# judged inert and the control character would reach the output unescaped.
.FORMAT_NEEDS_WALK <- "[^\\x20-\\x24\\x26-\\x3B\\x3D\\x3F-\\x7E]"

# --- The escape engine -------------------------------------------------------

.format_cp_escaped <- function(cp) {
  any(
    cp >= .FORMAT_ESCAPE_BLOCKS[, 1L] & cp <= .FORMAT_ESCAPE_BLOCKS[, 2L]
  )
}

# One byte as an uppercase triplet. Called only with values below 256: E3 hands
# it a raw byte, and E1/E4 hand it a code point that is ASCII by construction
# (`<`, `>`, and the delimiter sets, all of which are single-byte in UTF-8).
.format_pct <- function(b) sprintf("%%%02X", b)

.format_token <- function(cp) sprintf("<U+%04X>", cp)

# Render one already-tokenized byte sequence. `vals` are byte values in source
# order and `from_pct` marks the ones that arrived as a `%XX` triplet -- the
# distinction E1/E4/E5 turn on, since a LITERAL delimiter is structure and an
# ENCODED one is data that must not become structure.
#
# The walk decodes UTF-8 itself rather than handing the string to `stringi`,
# because it has to survive input `stringi` would reject: a component may hold
# invalid octets (an undecoded `%FF`, or invalid bytes passed straight through),
# and E3 requires those to be EMITTED as `%XX`, not to throw and not to be
# replaced. Doing it byte-wise also keeps the result identical under `LC_ALL=C`.
.format_render <- function(vals, from_pct, delims) {
  n <- length(vals)
  out <- character(n)
  k <- 0L
  i <- 1L
  while (i <= n) {
    b <- vals[[i]]
    len <- .FORMAT_UTF8_LEN[[b + 1L]]
    cp <- NA_integer_
    if (len == 1L) {
      cp <- b
    } else if (len > 1L && i + len - 1L <= n) {
      cont <- vals[(i + 1L):(i + len - 1L)]
      lead_ok <- cont[[1L]] >= .FORMAT_CONT1_LO[[b + 1L]] &&
        cont[[1L]] <= .FORMAT_CONT1_HI[[b + 1L]]
      tail_ok <- all(cont[-1L] >= 0x80L & cont[-1L] <= 0xBFL)
      if (lead_ok && tail_ok) {
        cp <- bitwAnd(b, c(0L, 0x1FL, 0x0FL, 0x07L)[[len]])
        for (c1 in cont) {
          cp <- bitwOr(bitwShiftL(cp, 6L), bitwAnd(c1, 0x3FL))
        }
      }
    }

    k <- k + 1L
    if (is.na(cp)) {
      # E3: not valid UTF-8. The byte is emitted, never decoded, never dropped.
      out[[k]] <- .format_pct(b)
      i <- i + 1L
      next
    }

    encoded <- any(from_pct[i:(i + len - 1L)])
    out[[k]] <- if (cp == .FORMAT_LT || cp == .FORMAT_GT) {
      # E1: `<` and `>` are never emitted from data, in either direction. This
      # is what makes every `<...>` in the output the FORMATTER's, and so what
      # makes `<redacted>` unmistakable for a real credential.
      if (encoded) .format_pct(cp) else .format_token(cp)
    } else if (encoded && cp %in% delims) {
      .format_pct(cp) # E4: decoding never fabricates structure.
    } else if (.format_cp_escaped(cp)) {
      .format_token(cp) # E2.
    } else {
      intToUtf8(cp) # E5: the only place decoding happens.
    }
    i <- i + len
  }
  res <- paste(out[seq_len(k)], collapse = "")
  Encoding(res) <- "UTF-8"
  res
}

# Tokenize one component into bytes, then render it. `decode = TRUE` recognizes
# `%XX` triplets (path, query, fragment); `decode = FALSE` treats every byte as
# literal, which is what the host needs -- the parse already decoded it, and
# decoding a second time would strip a LEVEL of encoding rather than reveal one.
.format_escape_one <- function(s, delims, decode) {
  if (is.na(s)) {
    return(NA_character_)
  }
  if (!nzchar(s)) {
    return("")
  }
  # Declare, never convert. `enc2utf8()` re-decodes already-UTF-8 octets into
  # mojibake under a non-UTF-8 locale and is barred from the namespace by the
  # locale-invariance meta-guard; declaring reads the exact octets in every
  # locale (see `.pct_as_utf8()`).
  Encoding(s) <- "UTF-8"
  raw_in <- charToRaw(s)
  n <- length(raw_in)
  vals <- integer(n)
  from_pct <- logical(n)
  k <- 0L
  i <- 1L
  while (i <= n) {
    b <- as.integer(raw_in[[i]])
    triplet <- decode && b == 0x25L && i + 2L <= n &&
      .FORMAT_IS_HEX[[as.integer(raw_in[[i + 1L]]) + 1L]] &&
      .FORMAT_IS_HEX[[as.integer(raw_in[[i + 2L]]) + 1L]]
    k <- k + 1L
    if (triplet) {
      vals[[k]] <- strtoi(rawToChar(raw_in[(i + 1L):(i + 2L)]), base = 16L)
      from_pct[[k]] <- TRUE
      i <- i + 3L
    } else {
      vals[[k]] <- b
      i <- i + 1L
    }
  }
  .format_render(vals[seq_len(k)], from_pct[seq_len(k)], delims)
}

.format_escape <- function(x, delims = .FORMAT_DELIMITERS, decode = TRUE) {
  if (length(x) == 0L) {
    return(character(0))
  }
  out <- x
  todo <- !is.na(x) &
    grepl(.FORMAT_NEEDS_WALK, x, perl = TRUE, useBytes = TRUE)
  if (any(todo)) {
    out[todo] <- vapply(
      x[todo], .format_escape_one, character(1),
      delims = delims, decode = decode, USE.NAMES = FALSE
    )
  }
  .mark_host_utf8(out)
}

# --- Host: both spellings ----------------------------------------------------

# The display spelling is the Unicode (IDNA) form of the host the WHATWG parse
# committed to, which is the A-label. `.punycode_to_unicode_vec()` is a no-op on
# a host with no `xn--` label, so an ordinary ASCII host yields display == ascii
# and no annotation is produced.
#
# IP literals are excluded: a bracketed IPv6 literal is not a domain and is
# never routed through domain.R (ADR 0002), and a dotted-quad has no A-label.
.format_host_pair <- function(host, host_kind) {
  ascii <- host
  display <- host
  cand <- host_kind == "present" & !is.na(host) & !startsWith(host, "[") &
    grepl("xn--", .ascii_tolower(host), fixed = TRUE)
  if (any(cand)) {
    display[cand] <- .punycode_to_unicode_vec(host[cand])
  }
  list(
    # E1 and E2 apply to the host too. E1 is a SURFACE-wide invariant, not a
    # per-component rule: a stray `<` from an opaque general host would make
    # `<redacted>` ambiguous, which is the one thing D-D's token rule buys.
    display = .format_escape(display, decode = FALSE),
    ascii = .format_escape(ascii, decode = FALSE)
  )
}

# --- The formatter -----------------------------------------------------------

# Query/fragment suffix, three-valued like the serializers': present -> the
# delimiter plus the escaped body, empty -> the bare delimiter, absent ->
# nothing. The empty case is the whole point -- `http://h/#` must not collapse
# to `http://h/` on a surface whose job is showing what is really there.
.format_suffix <- function(delimiter, value, kind) {
  out <- rep("", length(kind))
  out[kind == "empty"] <- delimiter
  present <- kind == "present"
  out[present] <- paste0(delimiter, ifelse(is.na(value), "", value)[present])
  out
}

#' Format a URL for safe human display
#'
#' Renders each URL as a string a **person** can read safely: credentials are
#' redacted, invisible and bidirectional-override code points are made visible
#' as `<U+XXXX>` tokens, percent-encoded delimiters are left encoded so decoding
#' cannot fabricate structure, and an internationalized host is shown in both
#' its Unicode and its ASCII (punycode) spelling whenever the two differ.
#'
#' @section The result is not a URL:
#'
#' `format_url()` output is **display only**. It is not reparsable, has no
#' round-trip guarantee, and must never be fed back into [serialize_url()], into
#' a comparison key, or into anything that treats it as an address. Use
#' [serialize_url()] for a standard-exact full string and [get_clean_url()] for
#' the cleaning surface.
#'
#' @param url A character vector of URLs.
#' @param engine Optional `psl_engine` object from `pslr::psl_engine()` for
#'   per-request Public Suffix List resolution. `NULL` (default) uses the
#'   session-global engine.
#'
#' @return A character vector the same length as `url`. `NA_character_` for
#'   input the WHATWG parser does not accept.
#'
#' @seealso [serialize_url()] for the standard-exact full string,
#'   [get_clean_url()] for the cleaning surface, and [safe_parse_url()] for the
#'   parsed components.
#'
#' @examples
#' # Credentials are redacted, never shown.
#' format_url("https://user:pw@example.com/a")
#'
#' # Percent-encoded delimiters stay encoded: decoding them would fabricate
#' # structure that the URL does not have.
#' format_url("https://example.com/a%2Fb?x=a%26b%3Dc")
#'
#' # An internationalized host is shown in both spellings when they differ.
#' format_url("https://xn--mnchen-3ya.de/p")
#'
#' # Invisible and bidirectional-override code points are made visible.
#' format_url("https://example.com/p?q=x#%E2%80%AE%E2%80%8Bevil")
#'
#' # A byte no valid UTF-8 sequence can contain is emitted, not decoded.
#' format_url("https://example.com/%FF%00")
#'
#' # Input the WHATWG parser rejects is NA, not a guess.
#' format_url("example.com/x")
#'
#' @export
format_url <- function(url, engine = NULL) {
  engine <- .validate_engine(engine)

  # Input coercion matches `serialize_url()`: factors format as their labels,
  # names are not data (RURL-vhdsqaln), and a non-character (or non-scalar
  # list) element is simply not parseable and becomes NA.
  if (is.factor(url)) {
    url <- as.character(url)
  }
  if (!is.null(names(url))) {
    url <- unname(url)
  }
  n <- length(url)
  if (n == 0L) {
    return(character(0))
  }
  if (!is.character(url)) {
    url <- vapply(
      as.list(url),
      function(u) if (is.character(u) && length(u) == 1L) u else NA_character_,
      character(1)
    )
  }

  rec <- .fsss_record_vec(url, "whatwg", engine)
  out <- rep(NA_character_, n)
  keep <- which(rec$ok)
  if (length(keep) == 0L) {
    return(out)
  }

  slice <- function(x) x[keep]
  scheme <- slice(rec$scheme)
  host_kind <- slice(rec$host_kind)
  path <- slice(rec$path)
  path_kind <- slice(rec$path_kind)

  # Userinfo is emitted IFF any was present, including a bare `@`, and is never
  # split into user/password. The token replaces the component and is never
  # derived from it, so it is fixed-width whatever the input held and leaks
  # neither the password's presence nor any length.
  userinfo <- slice(rec$userinfo)
  cred <- ifelse(is.na(userinfo), "", "<redacted>@")

  # `syntactic_port`, not `port`: the record's `port` has already had WHATWG's
  # default-port elision applied, and elision is NORMALIZATION -- surface (b)'s
  # business, not a display fact. D-D requires the port as parsed, verbatim.
  port <- slice(rec$syntactic_port)
  port_part <- ifelse(is.na(port), "", paste0(":", port))

  hosts <- .format_host_pair(slice(rec$host), host_kind)

  body <- .format_escape(path)
  body[is.na(body)] <- ""
  query <- .format_suffix(
    "?", .format_escape(slice(rec$query), .FORMAT_QUERY_DELIMITERS),
    slice(rec$query_kind)
  )
  fragment <- .format_suffix(
    "#", .format_escape(slice(rec$fragment)), slice(rec$fragment_kind)
  )

  # Assembly mirrors `.serialize_whatwg_full_vec()`'s three branches, so a URL
  # with an opaque path or no authority formats with the same structure the
  # serializer would give it -- only the component bodies differ.
  auth <- path_kind != "opaque" & .whatwg_authority_emitted(host_kind)
  opaque <- path_kind == "opaque"
  guard <- ifelse(
    host_kind == "absent" & !is.na(path) & startsWith(path, "//"), "/.", ""
  )

  host_str <- ifelse(is.na(hosts$display), "", hosts$display)
  noauth <- !opaque & !auth
  shown <- character(length(keep))
  shown[opaque] <- paste0(scheme[opaque], ":", body[opaque])
  shown[auth] <- paste0(
    scheme[auth], "://", cred[auth], host_str[auth], port_part[auth],
    body[auth]
  )
  shown[noauth] <- paste0(
    scheme[noauth], ":", guard[noauth], body[noauth]
  )

  # A homograph is never invisible: the reader always sees the A-label whenever
  # it is not simply the host they already read.
  differs <- auth & !is.na(hosts$display) & !is.na(hosts$ascii) &
    hosts$display != hosts$ascii
  annotation <- ifelse(differs, paste0("  [host: ", hosts$ascii, "]"), "")

  out[keep] <- paste0(shown, query, fragment, annotation)
  .mark_host_utf8(out)
}
