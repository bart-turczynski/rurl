# In-tree parser for the WEB/special-scheme route (RURL-robgajml, step 3).
#
# This file replaces the last remaining libcurl seam. Every other route already
# owns its parser in-tree: `.parse_opaque_urls_vec()` (posture opaque/RFC),
# `.parse_rfc_file_urls_vec()` (RFC 8089 overlay) and
# `.parse_whatwg_file_urls_vec()` (WHATWG `file:` state machine). What was left
# was the http/https/ftp/ftps (+ ws/wss under `whatwg`) slice, which Phase 2
# handed to `curl::curl_parse_url(decode = FALSE, params = FALSE)`.
#
# CONTRACT. `.parse_web_url_one()` is a drop-in for that call: it takes ONE
# fully PREPARED URL string (Phase 1 has already fabricated the scheme, stripped
# tab/LF/CR, rewritten backslashes, canonicalized WHATWG IPv4 and applied the
# host-charset shim) and returns either `NULL` (the row is a parse error) or a
# list of components: `url`, `scheme`, `host`,
# `port`, `path`, `query`, `fragment`, `user`, `password`. Absent components are
# `NULL`, exactly as libcurl reported them, so every `%||% NA_character_` and
# `.blank_to_na()` downstream keeps working unchanged.
#
# EIGHT of those nine reproduce libcurl's spelling and were verified to. `url`
# DOES NOT, and never did -- read this before reaching for it:
#
#   input                     libcurl $url             this $url
#   HTTP://example.com/p      http://example.com/p     HTTP://example.com/p
#   http://example.com        http://example.com/      http://example.com
#   http://example.com/a%2fb  http://example.com/a%2Fb http://example.com/a%2fb
#
# libcurl RE-SERIALIZES (ASCII-lowercases the scheme, supplies "/" for an empty
# path, uppercases every %XX); this returns the PREPARED INPUT VERBATIM. Over a
# 53,407-input corpus 10,614 rows (~20%) differ on `url`, against 3 on the eight
# consumed fields. Nothing in rurl reads `url` -- `.extract_raw_components()`
# and the vectorized path take scheme/host/port/path/query/fragment/user/
# password and nothing else -- so the divergence is inert. It stops being inert
# the moment someone treats `url` as a re-serialization; the FSSS
# (`serialize_url()`) is what renders a URL, not this field.
#
# WHY IT REPRODUCES LIBCURL RATHER THAN THE SPEC DIRECTLY. rurl's accept/reject
# verdict at this seam is load-bearing for `parse_status`, and the profile
# corrections that surround it (the ADR 0009 host-charset shim, the WHATWG IPv4
# rewrite, the excess-"@" repair) were all calibrated against libcurl's exact
# behaviour. Swapping the engine and the semantics in one step would make any
# conformance movement unattributable. So step 3 is a BEHAVIOUR-PRESERVING
# engine swap, verified by differential sweep; step 4 then deletes the
# compensation layer and moves the semantics deliberately, scored against the
# FSSS/WPT harness.
#
# STEP 4 IS UNDER WAY, so the list above is already out of date in two places.
# The excess-"@" repair is GONE (deletion 3): splitting the authority at the
# LAST "@" is what the WHATWG authority state does, so it is parser behaviour
# -- see `last_at_userinfo` below. The host-charset shim's PERCENT-TRIPLET half
# is gone too (deletion 2): which host triplets get decoded is decode ORDER,
# which only a parser can own -- see `host_pct`. What survives of that shim is
# the LITERAL gap-character mask, until ADR 0009 is superseded (deletion 1).
#
# Every rule below was derived by MEASUREMENT against
# libcurl (per-octet acceptance sweeps over host/userinfo/path/query/fragment,
# plus targeted probes for IPv4/IPv6/authority/port/dot-segment behaviour), not
# from reading the RFC -- because the thing being reproduced is libcurl, and
# where libcurl departs from the RFC that departure is the fact of record.
#
# The measured model, in one place (see tests/testthat/test-parse-web.R for the
# literal-oracle encoding of every clause):
#
#   scheme    ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) ":" -- any scheme is
#             accepted here (the allowed set is gated upstream), and the
#             reported spelling is ASCII-lowercased.
#   slashes   1..3 "/" may follow the scheme; 0 or >=4 is a parse error. Three
#             slashes means an EMPTY authority, and libcurl then promotes the
#             first path segment into the host (`http:///a/b` -> host "a",
#             path "/b") -- the shape `.extract_raw_path_vec()` documents.
#   authority up to the first "/" of what remains (after "#" and "?" are cut).
#             An empty authority is a parse error.
#   userinfo  split from the host at the LAST "@"; every earlier "@" is
#             percent-encoded into the userinfo as "%40". Under
#             `last_at_userinfo = FALSE` (the default, and what libcurl did)
#             only ONE "@" is admitted and a second is a parse error. Either
#             way: split into user/password at the FIRST ":"; bytes restricted
#             to 0x21-0x7E minus "@"; NEVER percent-decoded.
#   host      percent-DECODED first (a "%" not followed by two hex digits is a
#             parse error), then every decoded byte must be in
#             [A-Za-z0-9._~|-]; then libcurl's IPv4 normalization. A bracketed
#             host is an IPv6 literal, validated and re-serialized.
#             This is `host_pct = "narrow"`, the no-selector default; the other
#             two settings change the decode order and the set together, and
#             are documented at `.parse_web_url_one()` below.
#   port      ":" then ASCII digits only, value <= 65535, leading zeros
#             stripped; an EMPTY port (":" then end/"/"/"?"/"#") is no port.
#   path      "/" when absent; C0/space/DEL are a parse error; bytes >= 0x80 are
#             percent-encoded; the two characters after every "%" are
#             ASCII-uppercased; then dot segments are removed, INCLUDING
#             percent-encoded ones ("/a/%2E%2E/b" -> "/b").
#   query/    absent or empty -> NULL; otherwise the same C0/space/DEL
#   fragment  rejection and the same high-byte encoding + "%XX" uppercasing.

# Bytes libcurl refuses outright in path/query/fragment: C0 controls, SP, DEL.
# (NUL cannot reach here -- R strings cannot hold it.)
.WEB_FORBIDDEN_BYTES <- c(seq.int(1L, 32L), 127L)

# Decoded-host allowed ASCII set, as MEASURED: alphanumerics plus "-", ".",
# "_", "~" and "|". Note "|" -- libcurl keeps it although WHATWG forbids it in a
# host; that asymmetry is a fact of the engine being reproduced, not an
# oversight.
.WEB_HOST_ALLOWED_BYTES <- c(
  0x2DL, 0x2EL, seq.int(0x30L, 0x39L), seq.int(0x41L, 0x5AL), 0x5FL,
  seq.int(0x61L, 0x7AL), 0x7CL, 0x7EL
)

# The 15 ASCII code points WHATWG keeps in a host that the set above rejects:
# ! " $ & ' ( ) * + , ; = ` { }. The byte spelling of
# `.WHATWG_HOST_CHARSET_SHIM_CP` (R/utils.R), which states the provenance --
# none is a forbidden host or domain code point (ADR 0009, ada-confirmed).
# Admitted here only under `host_pct = "wide"`, and only on the DECODED side:
# the raw-token check stays narrow, so this closes the ENCODED spelling
# (`%60`) and leaves the literal one to the pre-parse shim until ADR 0009 is
# superseded.
.WEB_HOST_GAP_BYTES <- c(
  0x21L, 0x22L, 0x24L, 0x26L, 0x27L, 0x28L, 0x29L, 0x2AL, 0x2BL, 0x2CL,
  0x3BL, 0x3DL, 0x60L, 0x7BL, 0x7DL
)

# RFC 3986 section 2.3 `unreserved`: ALPHA / DIGIT / "-" / "." / "_" / "~".
# Section 6.2.2.2 permits decoding these and ONLY these.
.WEB_HOST_UNRESERVED_BYTES <- c(
  0x2DL, 0x2EL, seq.int(0x30L, 0x39L), seq.int(0x41L, 0x5AL), 0x5FL,
  seq.int(0x61L, 0x7AL), 0x7EL
)

# Userinfo allowed ASCII set: printable ASCII except "@" (0x40).
.WEB_USERINFO_ALLOWED_BYTES <- setdiff(seq.int(0x21L, 0x7EL), 0x40L)

# Host and userinfo are the only components that can carry RAW bytes >= 0x80 out
# of this parser (path/query/fragment get them percent-encoded), and there they
# are accepted only as WELL-FORMED UTF-8: `http://aéb/` parses, a lone 0x80 does
# not. That boundary is not libcurl's -- it is the R binding's, whose
# percent-hex `gsub(perl = TRUE)` pass over the decoded host THROWS on invalid
# UTF-8 in a UTF-8 session while returning raw bytes under `LC_ALL=C`. The old
# seam pinned the UTF-8-session outcome for every locale with an explicit
# `validUTF8()` reject; enforcing it here keeps that pin and makes it
# locale-invariant BY CONSTRUCTION rather than by compensation.
.web_high_bytes_ok <- function(s, allowed) {
  b <- .web_bytes(s)
  high <- b >= 0x80L
  if (!all(b[!high] %in% allowed)) {
    return(FALSE)
  }
  !any(high) || validUTF8(s)
}

# Byte view of one string. `charToRaw()` is deliberate: the prepared URL may be
# DECLARED UTF-8 while holding invalid octets, and `grepl`/`gregexpr` warn on
# exactly that input unless `useBytes = TRUE` (a trap this codebase has hit
# twice). Working in raw sidesteps the question entirely.
.web_bytes <- function(s) as.integer(charToRaw(s))

.web_chr <- function(b) {
  if (length(b) == 0L) {
    return("")
  }
  rawToChar(as.raw(b))
}

# ASCII-uppercase the two characters following each "%". Three measured details
# make this fussier than it looks, and all three are load-bearing:
#
#   1. libcurl uppercases the PAIR, not just valid hex -- "%zz" comes back
#      "%ZZ".
#   2. A "%" with fewer than two bytes after it is left alone -- "a%b" stays
#      "a%b".
#   3. It is a SEQUENTIAL SCAN that consumes three bytes per "%", not an
#      independent rewrite around every "%". In "%%2e" the first "%" claims the
#      pair "%2"; the scan then resumes at "e", which is never a pair member, so
#      the result is "%%2e" -- NOT the "%%2E" an every-"%" rewrite produces.
#
# The caller guarantees `b` is pure ASCII by the time this runs, so no encoding
# mark can diverge by locale here.
.web_uppercase_pct <- function(b) {
  n <- length(b)
  i <- 1L
  while (i <= n) {
    if (b[i] == 0x25L && i + 2L <= n) {
      for (j in c(i + 1L, i + 2L)) {
        if (b[j] >= 0x61L && b[j] <= 0x7AL) {
          b[j] <- b[j] - 32L
        }
      }
      i <- i + 3L
    } else {
      i <- i + 1L
    }
  }
  b
}

# Percent-encode every "@" in a userinfo byte run. Deliberately NOT expressed as
# a string substitution: `ub` is a raw byte slice that may hold invalid UTF-8,
# which is exactly the input `gsub`/`stringi` mishandle here (the pre-parse
# repair this replaces had to be rewritten onto byte-indexed helpers for
# RURL-kmpnbvdl for the same reason). Uppercase "%40" needs no later fix-up from
# `.web_uppercase_pct()`, which only touches hex letters.
.web_encode_at <- function(b) {
  if (!any(b == 0x40L)) {
    return(b)
  }
  unlist(lapply(b, function(x) {
    if (x == 0x40L) c(0x25L, 0x34L, 0x30L) else x
  }), use.names = FALSE)
}

# path/query/fragment normalization: reject the forbidden bytes, percent-encode
# every byte >= 0x80, then uppercase the "%XX" pairs. Returns NULL on rejection.
.web_normalize_component <- function(s) {
  b <- .web_bytes(s)
  if (any(b %in% .WEB_FORBIDDEN_BYTES)) {
    return(NULL)
  }
  high <- b >= 0x80L
  if (any(high)) {
    # LOWERCASE "%xx" here, deliberately. The uppercase pass runs AFTER, and it
    # is a sequential scan -- so a "%" already in the source can swallow the "%"
    # that introduces a freshly encoded octet and leave that octet's hex
    # lowercase. `/foo%2<C3><82>z` really does come back `/foo%2%c3%82z` from
    # libcurl. Encoding uppercase here would hide that interaction.
    out <- vector("list", length(b))
    out[!high] <- lapply(b[!high], identity)
    out[high] <- lapply(b[high], function(x) {
      c(0x25L, .web_bytes(sprintf("%02x", x)))
    })
    b <- unlist(out, use.names = FALSE)
  }
  .web_chr(.web_uppercase_pct(b))
}

# Strict percent-decode for the host. Unlike `.pct_unescape()` (which tolerates
# a malformed "%"), libcurl treats a "%" not followed by two hex digits in the
# host as a PARSE ERROR. Returns NULL in that case.
.web_hexdig <- function(x) {
  (x >= 0x30L & x <= 0x39L) | (x >= 0x41L & x <= 0x46L) |
    (x >= 0x61L & x <= 0x66L)
}

.web_host_percent_decode <- function(host) {
  b <- .web_bytes(host)
  hits <- which(b == 0x25L)
  if (length(hits) == 0L) {
    return(host)
  }
  hexd <- .web_hexdig
  n <- length(b)
  out <- integer(0)
  i <- 1L
  while (i <= n) {
    if (b[i] == 0x25L) {
      if (i + 2L > n || !hexd(b[i + 1L]) || !hexd(b[i + 2L])) {
        return(NULL)
      }
      out <- c(out, strtoi(.web_chr(b[(i + 1L):(i + 2L)]), base = 16L))
      i <- i + 3L
    } else {
      out <- c(out, b[i])
      i <- i + 1L
    }
  }
  # A decoded NUL cannot be carried in an R string, and libcurl rejects it in a
  # host anyway (0x00 is outside the allowed set), so reject rather than
  # truncate.
  if (any(out == 0L)) {
    return(NULL)
  }
  .web_chr(out)
}

# The `host_pct` setting each selected standard asks for. One place, because
# both the vectorized and the scalar route have to agree on it, and because a
# mapping that lives at the call sites is a mapping that drifts between them.
# The no-selector default stays `"narrow"` -- the historical behaviour.
.web_host_pct_policy <- function(url_standard) {
  if (.is_whatwg(url_standard)) {
    "wide"
  } else if (identical(url_standard, "rfc3986")) {
    "keep"
  } else {
    "narrow"
  }
}

# RFC 3986 host rendering: decode the triplets section 6.2.2.2 permits decoding
# (unreserved only) and leave every other one ENCODED, hex uppercased per
# section 6.2.2.1. Malformed "%" is still a parse error, exactly as in
# `.web_host_percent_decode()`, so the two agree on WHICH hosts parse and
# differ only on how the ones that do are spelled.
#
# This is the byte-level twin of `.rfc_unreserved_normalize()` (R/path-query.R),
# and it is a separate function rather than a call to it on purpose: that one
# runs `gregexpr(perl = TRUE)` over a string that here may be DECLARED UTF-8
# while holding invalid octets, which warns and returns NA on exactly the input
# this seam must judge (RURL-kmpnbvdl).
.web_host_pct_unreserved <- function(host) {
  b <- .web_bytes(host)
  if (!any(b == 0x25L)) {
    return(host)
  }
  n <- length(b)
  out <- integer(0)
  i <- 1L
  while (i <= n) {
    if (b[i] == 0x25L) {
      if (i + 2L > n || !.web_hexdig(b[i + 1L]) || !.web_hexdig(b[i + 2L])) {
        return(NULL)
      }
      pair <- b[(i + 1L):(i + 2L)]
      code <- strtoi(.web_chr(pair), base = 16L)
      out <- if (code %in% .WEB_HOST_UNRESERVED_BYTES) {
        c(out, code)
      } else {
        lower <- pair >= 0x61L & pair <= 0x7AL
        pair[lower] <- pair[lower] - 32L
        c(out, 0x25L, pair)
      }
      i <- i + 3L
    } else {
      out <- c(out, b[i])
      i <- i + 1L
    }
  }
  .web_chr(out)
}

# libcurl's IPv4 normalization. Deliberately NOT `.parse_whatwg_ipv4_host()`:
# the two disagree on the forms that decide whether a token is an ADDRESS at
# all. libcurl treats an empty numeric part as "not a number" (`0x` stays the
# name "0x", where WHATWG reads it as 0) and does not strip a trailing dot
# (`1.2.3.4.` stays a name). A token that is not an address is returned
# UNCHANGED -- it is simply a registered name -- never rejected.
.web_ipv4_number <- function(part) {
  if (!nzchar(part)) {
    return(NA_real_)
  }
  # LOWERCASE "0x" only. libcurl does not accept "0X", so `0Xff` is a registered
  # name while `0xff` is the address 0.0.0.255. (`.parse_whatwg_ipv4_number()`
  # accepts both, per WHATWG -- another reason these two normalizers stay
  # separate.)
  if (grepl("^0x", part, useBytes = TRUE)) {
    digits <- substring(part, 3L)
    base <- 16
  } else if (grepl("^0[0-9]+$", part, useBytes = TRUE)) {
    digits <- substring(part, 2L)
    base <- 8
  } else {
    digits <- part
    base <- 10
  }
  if (!nzchar(digits)) {
    return(NA_real_)
  }
  chars <- strsplit(digits, "", fixed = TRUE)[[1L]]
  vals <- match(.ascii_toupper(chars), c(0:9, "A", "B", "C", "D", "E", "F")) - 1
  if (anyNA(vals) || any(vals >= base)) {
    return(NA_real_)
  }
  Reduce(function(acc, d) acc * base + d, vals, 0)
}

.web_ipv4_normalize <- function(host) {
  parts <- strsplit(host, ".", fixed = TRUE)[[1L]]
  # `strsplit` drops a trailing empty field, so a trailing dot is detected from
  # the string, not from `parts` -- `1.2.3.4.` must stay a name.
  if (length(parts) == 0L || length(parts) > 4L ||
      endsWith(host, ".")) {
    return(host)
  }
  numbers <- vapply(parts, .web_ipv4_number, numeric(1), USE.NAMES = FALSE)
  if (anyNA(numbers)) {
    return(host)
  }
  k <- length(numbers)
  if (k > 1L && any(numbers[-k] > 255)) {
    return(host)
  }
  if (numbers[k] > 256^(5L - k) - 1) {
    return(host)
  }
  value <- numbers[k]
  if (k > 1L) {
    for (i in seq_len(k - 1L)) {
      value <- value + numbers[i] * 256^(4L - i)
    }
  }
  octets <- vapply(3L:0L, function(pow) {
    floor(value / 256^pow) %% 256
  }, numeric(1), USE.NAMES = FALSE)
  paste(octets, collapse = ".")
}

# IPv6 literal: validate, then reproduce libcurl's spelling. This is NOT
# `.serialize_whatwg_ipv6_host()` -- that one ALWAYS rewrites, folds a
# dotted-quad tail into two hextets and lowercases, per the WHATWG serializer.
# libcurl instead normalizes through inet_ntop and then keeps the result only
# if it came out SHORTER than the source text; otherwise the source stands
# verbatim, case and dotted quad intact. Returns NULL when the literal is
# invalid.
.web_ipv6_serialize <- function(inner) {
  # ASCII guard before any ICU call: an IPv6 literal is ASCII by construction,
  # and `stringi` refuses a declared-UTF-8 string holding invalid octets.
  if (any(.web_bytes(inner) >= 0x80L)) {
    return(NULL)
  }
  # A "%" anywhere inside the brackets REJECTS (RURL-ezhzpkhg). This seam used
  # to strip a "%zone" suffix and carry on, which silently ACCEPTED literals
  # every other part of rurl refuses: `ftp://[::1%]/x` parsed here with host
  # `[::1]` though libcurl rejected it outright. Rejecting agrees with both
  # host models rurl actually ships -- WHATWG forbids "%" in an IPv6 address,
  # and `.RFC3986_IPV6_RE` has no zone production either (RFC 9844 restored
  # RFC 3986's zone-less `IP-literal`) -- and it agrees with the downstream
  # host gate, which was already rejecting every one of these rows. That gate
  # is what made the widening invisible; a seam whose acceptance is wider than
  # every gate behind it is one refactor away from becoming user-visible.
  if (any(.web_bytes(inner) == 0x25L)) {
    return(NULL)
  }
  # The source text, kept for the length comparison and the verbatim return at
  # the end -- `inner` itself is consumed by the quad split below.
  src_inner <- inner
  quad <- NA_character_
  oct <- "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])"
  m <- stringi::stri_match_first_regex(
    inner, paste0("\\A(.*:)(", oct, "\\.", oct, "\\.", oct, "\\.", oct, ")\\z")
  )
  if (!is.na(m[1L, 1L])) {
    quad <- m[1L, 3L]
    prefix <- m[1L, 2L]
    # The prefix always ends in the ":" that separated the quad, and that colon
    # is dropped -- UNLESS it is the second colon of a "::", which belongs to
    # the compression marker and must survive. Without this, `[::127.0.0.1]` had
    # its
    # "::" cut down to a lone ":" and was rejected as malformed.
    if (!endsWith(prefix, "::")) {
      prefix <- substring(prefix, 1L, nchar(prefix) - 1L)
    }
    inner <- prefix
  }
  sides <- strsplit(inner, "::", fixed = TRUE)
  # `strsplit("::", "::")` yields character(0); `strsplit("a::", "::")` yields
  # "a". Detect the "::" and the empty sides from the string itself.
  has_dc <- grepl("::", inner, fixed = TRUE, useBytes = TRUE)
  if (has_dc) {
    dc <- stringi::stri_locate_first_fixed(inner, "::")[1L, 1L]
    left_s <- substring(inner, 1L, dc - 1L)
    right_s <- substring(inner, dc + 2L)
    if (grepl("::", right_s, fixed = TRUE, useBytes = TRUE)) {
      return(NULL) # more than one "::"
    }
  } else {
    left_s <- inner
    right_s <- ""
  }
  split_side <- function(x) {
    if (!nzchar(x)) {
      return(character(0))
    }
    if (startsWith(x, ":") || endsWith(x, ":")) {
      return(NULL)
    }
    strsplit(x, ":", fixed = TRUE)[[1L]]
  }
  left <- split_side(left_s)
  right <- split_side(right_s)
  if (is.null(left) || is.null(right)) {
    return(NULL)
  }
  quad_len <- if (is.na(quad)) 0L else 2L
  total <- length(left) + length(right) + quad_len
  if (has_dc) {
    if (total >= 8L) {
      return(NULL)
    }
  } else if (total != 8L) {
    return(NULL)
  }
  pieces <- c(left, right)
  if (length(pieces) > 0L &&
      !all(grepl("\\A[0-9A-Fa-f]{1,4}\\z", pieces, perl = TRUE))) {
    return(NULL)
  }
  fill <- if (has_dc) 8L - total else 0L
  # The eight 16-bit words. A dotted-quad tail contributes the last two.
  words <- c(
    strtoi(left, base = 16L), rep(0L, fill), strtoi(right, base = 16L)
  )
  if (!is.na(quad)) {
    o <- as.integer(strsplit(quad, ".", fixed = TRUE)[[1L]])
    words <- c(words, o[1L] * 256L + o[2L], o[3L] * 256L + o[4L])
  }
  if (length(words) != 8L || anyNA(words)) {
    return(NULL)
  }

  # Leftmost-LONGEST run of zero words, minimum length 2 (a single zero is never
  # compressed). Returns c(start, length), or c(0, 0) when there is none.
  r <- rle(words == 0L)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  best <- c(0L, 1L)
  for (i in seq_along(r$values)) {
    if (r$values[i] && r$lengths[i] > best[2L]) {
      best <- c(starts[i], r$lengths[i])
    }
  }

  # libcurl's inet_ntop. LOWERCASE, and with a dotted-quad rule NARROWER than
  # the BSD/glibc original it derives from: only
  # `best.base == 0 &&
  #  (best.len == 6 || (best.len == 5 && words[5] == 0xffff))`.
  # The `best.len == 7` clause is absent, which is why `[::0:2]` comes back
  # `[::2]` and not `[::0.0.0.2]`.
  norm <- if (best[1L] == 1L &&
                (best[2L] == 6L ||
                   (best[2L] == 5L && words[6L] == 0xffffL))) {
    tail4 <- c(
      words[7L] %/% 256L, words[7L] %% 256L,
      words[8L] %/% 256L, words[8L] %% 256L
    )
    head_toks <- if (best[2L] < 6L) {
      sprintf("%x", words[(best[2L] + 1L):6L])
    } else {
      character(0)
    }
    paste0(
      "::", paste(c(head_toks, paste(tail4, collapse = ".")), collapse = ":")
    )
  } else {
    toks <- sprintf("%x", words)
    if (best[1L] == 0L) {
      paste(toks, collapse = ":")
    } else {
      head_toks <- if (best[1L] > 1L) {
        toks[seq_len(best[1L] - 1L)]
      } else {
        character(0)
      }
      tail_idx <- best[1L] + best[2L]
      tail_toks <- if (tail_idx <= 8L) toks[tail_idx:8L] else character(0)
      paste0(
        paste(head_toks, collapse = ":"), "::",
        paste(tail_toks, collapse = ":")
      )
    }
  }

  # THE decisive clause, and the reason libcurl's IPv6 output looks
  # value-dependent when it is not: libcurl adopts the normalized spelling only
  # when it is STRICTLY SHORTER than what was written --
  #   `if(Curl_inet_ntop(...) && (strlen(norm) < hlen)) strcpy(hostname, norm);`
  # -- otherwise the source text stands, case and all. So `[::0001:0002]`
  # becomes `[::0.1.0.2]` (11 chars -> 9) while the SAME ADDRESS written
  # `[::1:2]` is left alone (5 chars; the dotted form would be longer), and
  # `[AB::CD]` keeps its uppercase because `ab::cd` is exactly as long.
  if (nchar(norm) < nchar(src_inner)) {
    return(paste0("[", norm, "]"))
  }
  paste0("[", src_inner, "]")
}

# Host parse: bracketed IPv6 literal, or a percent-decoded registered name /
# IPv4 address. Returns NULL on rejection.
#
# `host_pct` is the host's percent-decode POLICY -- see `.parse_web_url_one()`.
# It changes two things together, and they have to move together: which
# triplets are decoded, and therefore which byte set the result is judged
# against.
.web_parse_host <- function(host, host_pct = "narrow") {
  # Byte-based throughout: `nchar()`/`substring()`/`endsWith()` all throw
  # "invalid multibyte string" on a declared-UTF-8 host token holding invalid
  # octets -- and such a token must REJECT, not error.
  hb <- .web_bytes(host)
  if (length(hb) == 0L) {
    return(NULL)
  }
  if (hb[1L] == 0x5BL) {
    if (hb[length(hb)] != 0x5DL) {
      return(NULL)
    }
    return(.web_ipv6_serialize(.web_chr(hb[-c(1L, length(hb))])))
  }
  if (any(hb == 0x5DL)) {
    return(NULL)
  }
  # The RAW token is validated too, not just the decoded result, and the two
  # sets are NOT the same. A literal DEL is rejected, but "%7F" decodes to one
  # and is KEPT -- the only byte where the pre- and post-decode verdicts differ.
  # Every other forbidden byte fails whichever way it is written ("%2F" -> "/",
  # "%25" -> "%", "%40" -> "@" all reject).
  if (!.web_high_bytes_ok(host, c(.WEB_HOST_ALLOWED_BYTES, 0x25L))) {
    return(NULL)
  }
  # VALIDATION always runs on the fully decoded host, whatever the spelling
  # rule -- `%2F` is a "/" in a host however it is written, and a policy that
  # renders it encoded must not thereby stop judging it. That separation is the
  # whole point: the pre-parse mask this replaces blanked every triplet, so it
  # widened ACCEPTANCE as a side effect of wanting a different SPELLING.
  decoded <- .web_host_percent_decode(host)
  if (is.null(decoded) || !nzchar(decoded)) {
    return(NULL)
  }
  post_allowed <- c(.WEB_HOST_ALLOWED_BYTES, 0x7FL)
  if (!identical(host_pct, "narrow")) {
    post_allowed <- c(post_allowed, .WEB_HOST_GAP_BYTES)
  }
  if (!.web_high_bytes_ok(decoded, post_allowed)) {
    return(NULL)
  }
  # RENDERING then differs. "keep" re-reads the RAW token and decodes only the
  # unreserved triplets; it cannot fail, because the decode above has already
  # proved every triplet well-formed.
  if (identical(host_pct, "keep")) {
    decoded <- .web_host_pct_unreserved(host)
  }
  # IPv4 normalization reads the host AS WRITTEN, before percent-decoding: a
  # host spelled "%30%78%63%30%2e%30%32%35%30.01" decodes to the numeric form
  # "0xc0.0250.01" and yet libcurl leaves it a registered name. Percent-escapes
  # therefore SUPPRESS the numeric reading entirely.
  if (any(hb == 0x25L)) {
    return(decoded)
  }
  .web_ipv4_normalize(decoded)
}

# The seam. One prepared URL in; libcurl's field list out, or NULL.
#
# The whole structural scan runs on the BYTE vector and cuts BY POSITION. That
# is not a style choice: the prepared string can be declared UTF-8 while holding
# invalid octets, and `stringi` refuses such input outright (returning NA from
# `stri_match_first_regex`, which would read here as "not a URL" and reject
# every high-byte row libcurl happily parses). Cutting by position also
# sidesteps the ICU line-terminator trap -- `.` not matching VT/FF/NEL/LS/PS,
# and `$`
# matching BEFORE a trailing one -- that this codebase has lost two sessions to.
#
# TWO policy dials, both here rather than in front of the parser because both
# describe *parsing*, not repair. Each defaults to the historical no-selector
# behaviour, and callers opt in per selected standard.
#
# `last_at_userinfo` -- split the authority at the LAST "@". WHATWG's authority
# state buffers until the final "@" and prepends "%40" for each earlier one. It
# stays a dial because the P2.1 C-03 disposition binds repeated-"@" recovery to
# the `compatibility`/`repair` postures and requires a `strict` parse to REJECT
# a repeated raw "@" -- RFC 3986 admits no unescaped "@" in either `userinfo`
# or `reg-name`.
#
# `host_pct` -- the host's percent-decode ORDER and spelling, which is a
# property of the SELECTED STANDARD and cannot be inferred from the input:
#
#   "narrow"  decode every triplet, then judge the result against libcurl's
#             host set, and report it decoded. The no-selector default.
#   "wide"    the same, but the judged set also holds the 15 code points
#             WHATWG keeps in a host (`.WEB_HOST_GAP_BYTES`). WHATWG's host
#             parser percent-decodes FIRST and only then checks forbidden
#             domain code points, and "`" is not one of them -- so `%60` must
#             parse exactly as the literal "`" does.
#   "keep"    judged like "wide", but REPORTED with only the unreserved
#             triplets decoded and the rest left encoded, hex uppercased
#             (RFC 3986 sections 6.2.2.2 and 6.2.2.1). The RFC posture
#             preserves a reg-name's source spelling instead of inventing a
#             decoded one.
#
# Note what "keep" does NOT do: it does not stop VALIDATING the decoded host.
# `reg-name` grammar would admit any well-formed triplet whatever it decodes
# to, and that reading is a much larger acceptance question than this seam --
# it is left open deliberately. Rendering and acceptance are separate axes
# here, which is precisely what the mask could not express.
#
# Until this dial existed the difference was compensated for OUTSIDE the
# parser: Phase 1 masked every host triplet as filler so the decode could not
# happen, then substituted the profile-correct host back afterwards. That
# masking also hid the rest of the host from every check the parser makes
# (RURL-rgjpcbuk / RURL-ezhzpkhg deletion 2).
.parse_web_url_one <- function(url, last_at_userinfo = FALSE,
                               host_pct = "narrow") {
  if (is.na(url)) {
    return(NULL)
  }
  b <- .web_bytes(url)
  n <- length(b)

  is_alpha <- function(x) {
    (x >= 0x41L & x <= 0x5AL) | (x >= 0x61L & x <= 0x7AL)
  }
  is_digit <- function(x) x >= 0x30L & x <= 0x39L

  # scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) ":"
  if (n < 2L || !is_alpha(b[1L])) {
    return(NULL)
  }
  i <- 2L
  while (i <= n && (is_alpha(b[i]) || is_digit(b[i]) ||
                      b[i] == 0x2BL || b[i] == 0x2DL || b[i] == 0x2EL)) {
    i <- i + 1L
  }
  if (i > n || b[i] != 0x3AL) {
    return(NULL)
  }
  scheme <- .ascii_tolower(.web_chr(b[seq_len(i - 1L)]))
  pos <- i + 1L

  # 1..3 slashes. Three means an EMPTY authority, and libcurl then promotes the
  # first path segment into the host.
  slashes <- 0L
  while (pos <= n && b[pos] == 0x2FL) {
    slashes <- slashes + 1L
    pos <- pos + 1L
  }
  if (slashes < 1L || slashes > 3L) {
    return(NULL)
  }

  # Remaining byte range [pos, end]; "#" then "?" trim it from the right.
  end <- n
  fragment <- NULL
  query <- NULL
  cut_at <- function(byte) {
    if (pos > end) {
      return(0L)
    }
    hit <- which(b[pos:end] == byte)
    if (length(hit) == 0L) 0L else pos + hit[1L] - 1L
  }
  h <- cut_at(0x23L)
  if (h > 0L) {
    fragment <- .web_chr(b[seq_len(end - h) + h])
    end <- h - 1L
  }
  q <- cut_at(0x3FL)
  if (q > 0L) {
    query <- .web_chr(b[seq_len(end - q) + q])
    end <- q - 1L
  }

  s <- cut_at(0x2FL)
  if (s > 0L) {
    auth_end <- s - 1L
    path <- .web_chr(b[s:end])
  } else {
    auth_end <- end
    path <- ""
  }
  if (auth_end < pos) {
    return(NULL) # empty authority
  }
  ab <- b[pos:auth_end]

  user <- NULL
  password <- NULL
  ats <- which(ab == 0x40L)
  if (length(ats) > 1L && !last_at_userinfo) {
    return(NULL) # no recovery: a repeated raw "@" is not a valid authority
  }
  hb <- ab
  if (length(ats) >= 1L) {
    # The LAST "@" is the delimiter; everything before it is userinfo, and the
    # earlier "@" bytes become "%40" there -- the spelling WHATWG stores, and
    # the same bytes the deleted pre-parse repair used to write.
    at <- ats[length(ats)]
    ub <- if (at > 1L) ab[seq_len(at - 1L)] else integer(0)
    hb <- if (at < length(ab)) {
      ab[(at + 1L):length(ab)]
    } else {
      integer(0)
    }
    # Before the allowed-byte check, because "@" is not in the allowed set and
    # before the ":" split, because "%40" can neither create nor destroy a ":".
    if (length(ats) > 1L) {
      ub <- .web_encode_at(ub)
    }
    if (length(ub) > 0L &&
          !.web_high_bytes_ok(.web_chr(ub), .WEB_USERINFO_ALLOWED_BYTES)) {
      return(NULL)
    }
    # Userinfo is never percent-DECODED, but it IS "%XX"-uppercased, on each
    # side of the ":" independently (measured; the split happens first, so a
    # "%" straddling the colon cannot pair across it).
    c1 <- which(ub == 0x3AL)
    if (length(c1) > 0L) {
      user <- .web_chr(.web_uppercase_pct(ub[seq_len(c1[1L] - 1L)]))
      password <- .web_chr(
        .web_uppercase_pct(ub[seq_len(length(ub) - c1[1L]) + c1[1L]])
      )
    } else {
      user <- .web_chr(.web_uppercase_pct(ub))
    }
  }

  port <- NULL
  if (length(hb) > 0L && hb[1L] == 0x5BL) {
    rb <- which(hb == 0x5DL)
    if (length(rb) == 0L) {
      return(NULL)
    }
    rb <- rb[1L]
    host_token <- .web_chr(hb[seq_len(rb)])
    after <- hb[seq_len(length(hb) - rb) + rb]
    if (length(after) > 0L) {
      if (after[1L] != 0x3AL) {
        return(NULL)
      }
      port <- .web_chr(after[-1L])
    }
  } else {
    c2 <- which(hb == 0x3AL)
    if (length(c2) > 0L) {
      host_token <- .web_chr(hb[seq_len(c2[1L] - 1L)])
      port <- .web_chr(hb[seq_len(length(hb) - c2[1L]) + c2[1L]])
    } else {
      host_token <- .web_chr(hb)
    }
  }
  if (!is.null(port)) {
    pbytes <- .web_bytes(port)
    if (length(pbytes) == 0L) {
      port <- NULL
    } else if (!all(is_digit(pbytes)) ||
                 suppressWarnings(as.numeric(port)) > 65535) {
      return(NULL)
    } else {
      port <- format(as.numeric(port), scientific = FALSE) # strips zeros
    }
  }

  host <- .web_parse_host(host_token, host_pct)
  if (is.null(host)) {
    return(NULL)
  }

  if (nzchar(path)) {
    path <- .web_normalize_component(path)
    if (is.null(path)) {
      return(NULL)
    }
    # Safe to hand to stringi/ICU now: `.web_normalize_component()` has
    # percent-encoded every byte >= 0x80, so `path` is pure ASCII.
    if (stringi::stri_detect_regex(path, "(?i)(\\A|/)(\\.|%2e){1,2}(/|\\z)")) {
      path <- ._remove_dot_segments_whatwg(path)
    }
  }
  # Covers both an absent path and one that dot-segment resolution emptied.
  if (!nzchar(path)) {
    path <- "/"
  }

  norm_opt <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      return(list(ok = TRUE, value = NULL))
    }
    v <- .web_normalize_component(x)
    if (is.null(v)) {
      list(ok = FALSE, value = NULL)
    } else {
      list(ok = TRUE, value = v)
    }
  }
  qn <- norm_opt(query)
  if (!qn$ok) {
    return(NULL)
  }
  fn <- norm_opt(fragment)
  if (!fn$ok) {
    return(NULL)
  }

  list(
    url = url, scheme = scheme, host = host, port = port, path = path,
    query = qn$value, fragment = fn$value, user = user, password = password
  )
}
