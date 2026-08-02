# RFC 3986 Appendix B -- the parser the STANDARD ITSELF specifies.
#
# WHY THIS EXISTS (RURL-xfbzkico). `helper-rfc3986-abnf.R` beside this file
# answers "does the generic URI grammar admit this string". It cannot answer
# "and what are its components", because an acceptance matcher throws the
# decomposition away. Every defect in the rfc3986 authority-slash family is a
# decomposition defect: `http:/evil.com` is ACCEPTED by both rurl and the ABNF
# matcher, and is still wrong, because rurl splits it into a record that says
# "no authority was written" while simultaneously reporting `host = evil.com`.
# No acceptance oracle can see that. This one can.
#
# WHY IT IS AUTHORITATIVE. RFC 3986 does not merely define a grammar and leave
# parsing to the reader. Appendix B ("Parsing a URI Reference with a Regular
# Expression") gives a regular expression and states that it is "the regular
# expression for breaking-down a well-formed URI reference into its
# components". The decomposition is therefore part of the standard, not a
# reading of it -- so this oracle needs no consensus-of-implementations and no
# external runtime, which is the same reason the ABNF matcher is transcribed
# rather than shelled out to Ruby.
#
# Cross-checked over the authority-slash family against five independent
# implementations -- Go `net/url`, Python `urlsplit`, Ruby `URI`, libxml2 and
# PHP `parse_url` -- which agree with it on 10/10 rows.
#
# WHAT IT DELIBERATELY IS NOT.
#
#   * NOT a validator. The Appendix B expression matches EVERY string: each of
#     its groups is optional or `*`-quantified. `split_rfc3986_appendix_b("")`
#     succeeds. Validity is a separate question -- pair this with
#     `rfc3986_abnf_accepts()`. Asking this function whether something is a URI
#     is a category error, and treating a successful split as acceptance would
#     make the instrument green on garbage.
#   * NOT scheme-aware. It knows nothing of RFC 8089, of default ports, of
#     registrable domains, or of rurl's policy layer. It reports what the
#     generic syntax says was written.
#   * NOT normalizing. No case folding, no percent-decoding, no dot-segment
#     removal. Those are RFC 3986 section 6 operations, downstream of the split.
#
# THE DISTINCTION THAT MATTERS: absent vs empty. Appendix B's authority group
# is `(//([^/?#]*))?` -- an OPTIONAL group wrapping a possibly-EMPTY capture. So
# the expression natively separates three states, and this function reports them
# as `NA` / `""` / non-empty:
#
#     http:/x     -- group absent      -> authority NA, path "/x"
#     http://x    -- group present     -> authority "x", path ""
#     http:///x   -- present, empty    -> authority "",  path "/x"
#     http:////x  -- present, empty    -> authority "",  path "//x"
#
# That is exactly the tri-state rurl already models as
# `authority_delimiter_present` plus `host_kind` in {absent, empty, present},
# which is why conforming to the RFC here needs no canonical-state change: the
# state space is already right and only the parse disagrees.

# The expression is transcribed from Appendix B literally, including the ABSENCE
# of a `$` anchor -- the RFC's form has none, relying on the groups to consume a
# well-formed reference. `(?s)` is the one addition: without it a `.` would stop
# at a line terminator and leave a tail of the fragment unconsumed, silently
# truncating rather than reporting. It cannot change a verdict for any valid
# URI, because every RFC 3986 terminal is printable ASCII and
# `rfc3986_abnf_accepts()` already refuses anything outside `\x21-\x7e`; it only
# makes the split total.
# `(?s)` also forces `perl = TRUE` everywhere below -- TRE rejects the inline
# flag outright ("regcomp error: 'Invalid regexp'"), so there is no silent
# fallback to worry about.
.rfc3986_appendix_b_re <- paste0(
  "(?s)",
  "^(([^:/?#]+):)?", # 1, 2  scheme
  "(//([^/?#]*))?",  # 3, 4  authority
  "([^?#]*)",        # 5     path (always participates; may be empty)
  "(\\?([^#]*))?",   # 6, 7  query
  "(#(.*))?"         # 8, 9  fragment
)

# Group indices, named so the extraction below reads as the RFC's own structure
# rather than as magic numbers. Index 1 is the whole match.
.RFC3986_AB_GROUPS <- c(
  scheme = 3L, authority = 5L, path = 6L, query = 8L, fragment = 10L
)

# Split one string. Returns a one-row data.frame; `NA` means the component was
# NOT WRITTEN, `""` means it was written and is empty. `*_present` columns are
# redundant with `!is.na()` and exist so a test can assert presence without
# depending on that convention holding.
#
# Byte discipline: `regexec(useBytes = TRUE)` reports BYTE offsets, so the
# components are cut out of `charToRaw()` bytes rather than with `substr()`,
# which counts characters. Mixing the two is the index-unit mismatch that
# produced locale-dependent verdicts elsewhere in this package; here the units
# agree by construction, and the result is locale-independent.
#
# PARTICIPATION IS `start <= 0`, NOT `start < 0`. R's `regexpr()` documents -1
# for "no match", but `regexec(perl = TRUE)` reports a NON-PARTICIPATING capture
# group as start **0**. Measured, not assumed:
#
#     http:/evil.com  authority group -> start  0, len 0   (absent)
#     http:///x       authority group -> start  8, len 0   (present, empty)
#     http://x?#      query     group -> start 10, len 0   (present, empty)
#
# A participating group always reports the position it matched at, which is >= 1
# even when it matched zero bytes, so the two states never collide. This is the
# single most dangerous line in the file: written as `< 0` -- as it first was --
# every absent component silently becomes an EMPTY one, and the oracle then
# agrees with exactly the rurl bug it was built to expose (`http:/evil.com`
# reported as "authority written, and empty"). A green from that oracle would
# have been worthless.
# `rawToChar()` returns the right BYTES with no declared encoding, and losing
# the declaration is not cosmetic: a slice of a UTF-8 string comes back marked
# "unknown", which means "native". In a UTF-8 session native IS UTF-8, so the
# loss is invisible; under `LC_ALL=C` the identical bytes then denote different
# characters, and a decompose/recompose round-trip stops being `identical()` to
# the string it started from. That is what the locale cell of verify.yml caught:
# two non-ASCII corpus rows failed there and nowhere else.
#
# So the byte discipline above is kept exactly -- the bytes are never touched --
# and only the input's own declaration is put back on each slice. This is what
# makes the header's claim that the split is locale-independent true, rather
# than true-in-a-UTF-8-session. It is the encoding-mark half of the same
# index-unit family recorded in RURL-kmpnbvdl.
.ab_keep_encoding <- function(x, from) {
  enc <- Encoding(from)
  if (!identical(enc, "unknown")) {
    Encoding(x) <- enc
  }
  x
}

split_rfc3986_appendix_b_one <- function(s) {
  na_row <- function() {
    data.frame(
      scheme = NA_character_, authority = NA_character_, path = NA_character_,
      query = NA_character_, fragment = NA_character_,
      scheme_present = NA, authority_present = NA, query_present = NA,
      fragment_present = NA, stringsAsFactors = FALSE
    )
  }
  if (is.na(s)) {
    return(na_row())
  }
  m <- regexec(.rfc3986_appendix_b_re, s, perl = TRUE, useBytes = TRUE)[[1L]]
  if (m[1L] < 0L) {
    # Unreachable: every group is optional or `*`-quantified, so the expression
    # matches any string. Reported rather than silently returned as NA, because
    # if it ever fires the transcription is wrong and the oracle must not be
    # trusted through it.
    stop("Appendix B expression failed to match -- transcription is wrong: ", s)
  }
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  bytes <- charToRaw(s)
  participates <- function(i) starts[i] > 0L
  grab <- function(i) {
    if (!participates(i)) {
      return(NA_character_)
    }
    if (lens[i] == 0L) {
      return("")
    }
    .ab_keep_encoding(rawToChar(bytes[starts[i] - 1L + seq_len(lens[i])]), s)
  }
  g <- .RFC3986_AB_GROUPS
  out <- data.frame(
    scheme = grab(g[["scheme"]]),
    authority = grab(g[["authority"]]),
    path = grab(g[["path"]]),
    query = grab(g[["query"]]),
    fragment = grab(g[["fragment"]]),
    stringsAsFactors = FALSE
  )
  out$scheme_present <- participates(g[["scheme"]])
  out$authority_present <- participates(g[["authority"]])
  out$query_present <- participates(g[["query"]])
  out$fragment_present <- participates(g[["fragment"]])
  out
}

# Vectorized form. Kept as an rbind of the scalar rather than a vectorized
# regexec so that the scalar stays the single definition of the split.
split_rfc3986_appendix_b <- function(x) {
  if (length(x) == 0L) {
    return(split_rfc3986_appendix_b_one(NA_character_)[0L, , drop = FALSE])
  }
  do.call(rbind, lapply(x, split_rfc3986_appendix_b_one))
}

# Appendix B stops at the authority; RFC 3986 section 3.2 splits it further:
#
#     authority = [ userinfo "@" ] host [ ":" port ]
#
# The split is unambiguous for any authority the ABNF admits, and this function
# is only meaningful for those:
#
#   * `userinfo = *( unreserved / pct-encoded / sub-delims / ":" )` excludes
#     "@", so a VALID authority holds at most one "@" and it is the separator.
#     (For an invalid authority carrying several, first-"@" is a convention, not
#     a reading of the grammar -- ask `rfc3986_abnf_accepts()` first. rurl's own
#     last-"@" WHATWG rule is a different standard's answer, not a candidate
#     here.)
#   * `reg-name` cannot contain ":" -- sub-delims does not include it -- so in a
#     valid authority every colon is either inside an `IP-literal`'s brackets or
#     the port separator. Hence: scan for the separator AFTER "]" when the host
#     is bracketed, and the first colon otherwise.
#   * `port = *DIGIT`, so a trailing ":" with no digits is legal and means port
#     WRITTEN-BUT-EMPTY -- again `NA` vs `""`.
split_rfc3986_authority_one <- function(authority) {
  out <- data.frame(
    userinfo = NA_character_, host = NA_character_, port = NA_character_,
    userinfo_present = NA, port_present = NA, stringsAsFactors = FALSE
  )
  if (is.na(authority)) {
    return(out)
  }
  rest <- authority
  at <- which(charToRaw(rest) == as.raw(0x40L))
  if (length(at) > 0L) {
    cut <- at[1L]
    b <- charToRaw(rest)
    out$userinfo <- if (cut == 1L) "" else
      .ab_keep_encoding(rawToChar(b[seq_len(cut - 1L)]), authority)
    rest <- if (cut == length(b)) {
      ""
    } else {
      .ab_keep_encoding(rawToChar(b[seq.int(cut + 1L, length(b))]), authority)
    }
  }
  b <- charToRaw(rest)
  colon <- 0L
  if (length(b) > 0L && b[1L] == as.raw(0x5BL)) {
    close_br <- which(b == as.raw(0x5DL))
    if (length(close_br) > 0L) {
      after <- close_br[length(close_br)]
      hit <- which(b == as.raw(0x3AL) & seq_along(b) > after)
      if (length(hit) > 0L) colon <- hit[1L]
    }
  } else {
    hit <- which(b == as.raw(0x3AL))
    if (length(hit) > 0L) colon <- hit[1L]
  }
  if (colon > 0L) {
    out$host <- if (colon == 1L) "" else
      .ab_keep_encoding(rawToChar(b[seq_len(colon - 1L)]), authority)
    out$port <- if (colon == length(b)) {
      ""
    } else {
      .ab_keep_encoding(rawToChar(b[seq.int(colon + 1L, length(b))]), authority)
    }
  } else {
    out$host <- rest
  }
  out$userinfo_present <- !is.na(out$userinfo)
  out$port_present <- !is.na(out$port)
  out
}

split_rfc3986_authority <- function(x) {
  if (length(x) == 0L) {
    return(split_rfc3986_authority_one(NA_character_)[0L, , drop = FALSE])
  }
  do.call(rbind, lapply(x, split_rfc3986_authority_one))
}

# RFC 3986 section 5.3 (Component Recomposition), transcribed. Its pseudocode
# tests each component for being DEFINED, which is why the split above must
# report absent as `NA` and empty as `""`: `if defined(authority)` is the line
# that turns `http:/x` into `http:/x` and `http:///x` back into `http:///x`.
#
# This is the oracle's TOTAL self-check. Appendix B splits any string, and
# section 5.3 must then rebuild it byte-for-byte -- for every input, not curated
# ones. So `recompose_rfc3986(split(s)) == s` is a property assertable over an
# entire generated corpus, and it fails loudly the moment the split loses or
# reorders a byte. An oracle that cannot round-trip has no business judging a
# parser that must.
recompose_rfc3986 <- function(parts) {
  vapply(seq_len(nrow(parts)), function(i) {
    p <- parts[i, , drop = FALSE]
    if (is.na(p$path)) {
      return(NA_character_)
    }
    out <- ""
    if (!is.na(p$scheme)) out <- paste0(out, p$scheme, ":")
    if (!is.na(p$authority)) out <- paste0(out, "//", p$authority)
    out <- paste0(out, p$path)
    if (!is.na(p$query)) out <- paste0(out, "?", p$query)
    if (!is.na(p$fragment)) out <- paste0(out, "#", p$fragment)
    out
  }, character(1), USE.NAMES = FALSE)
}

# The two joined: the full RFC 3986 answer for a string, in one frame. `valid`
# is the ABNF matcher's verdict and is reported ALONGSIDE the split rather than
# gating it, so a test can assert "invalid" and "decomposes thus" independently
# -- and so that a divergence report can say which of the two questions rurl got
# wrong.
rfc3986_reference_parse <- function(x) {
  ab <- split_rfc3986_appendix_b(x)
  auth <- split_rfc3986_authority(ab$authority)
  out <- cbind(
    data.frame(input = x, valid = rfc3986_abnf_accepts(x),
               stringsAsFactors = FALSE),
    ab, auth
  )
  rownames(out) <- NULL
  out
}
