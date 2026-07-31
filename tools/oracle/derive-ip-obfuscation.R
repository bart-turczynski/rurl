#!/usr/bin/env Rscript
# RURL-ozdejfzl -- the `ip-obfuscation` oracle, as tracked code.
#
# Ported from `_scratch/build-ipobf-vectors.R` (RURL-dbazixkr slice 3), which
# produced the 24 rows but was never in the repository: `_scratch/` is
# gitignored.
#
# WHAT THIS FILE IS. The DERIVATION only -- the oracle. It answers "what does
# the WHATWG URL Standard say this host parses and serializes to", and it
# answers it WITHOUT LOADING rurl. An oracle that calls the implementation it
# grades is a characterization test wearing an oracle's label, so the split is
# structural: rurl is never on the search path, so this file cannot reach
# rurl's answer even by accident. The `rurl_*` columns are a separate concern
# and are not computed here.
#
# WHAT CLAIM THIS DISCHARGES. Unlike the other six groups, `ip-obfuscation`
# records NO `generation_command` -- it carries `section_2_3_applies = false`
# because nothing was imported. What it does record, in prose, is a much
# stronger claim:
#
#     out_of_scope_reason: "each row is a decimal/octal/hex re-encoding of an
#       IPv4 literal, derivable from arithmetic alone"
#     relocation_note:     "the rows are re-derivable by arithmetic instead"
#
# That is the entire provenance argument for the group: there is no upstream
# artifact to re-locate against, so re-derivability is what stands in for one.
# Nothing tested it. This file makes it executable.
#
# THE TWO HALVES OF THE GROUP.
#
#   1. The 24 INPUTS are the irreducible data, and they travel with this file
#      as the roster below. They are hand-generated encodings, deliberately not
#      vendored from the technique references (neither cujanovic/SSRF-Testing
#      nor JorianWoltjer/ipobf ships a LICENSE), which is exactly why they
#      cannot be re-fetched from anywhere.
#   2. Every EXPECTED value is computed, never quoted -- by a transcription of
#      the WHATWG host parser (#concept-host-parser), its IPv4 parser
#      (#concept-ipv4-parser), its IPv6 parser (#concept-ipv6-parser), and the
#      two host serializers (#concept-ipv4-serializer and
#      #concept-ipv6-serializer). The first four live in URL Standard section
#      3.5 "Host parsing"; the two serializers in section 3.6 "Host
#      serializing".
#
# HOW THIS FILE CITES THE SPEC. Anchor first, section number second. Section
# NUMBERS are presentation metadata and they demonstrably drift: every citation
# in this file used to carry a number that does not resolve, and two of them
# pointed at subsections of "Host parsing" that have never existed in ANY
# revision of the standard -- it has no subsections. The anchor id (the
# fragment on https://url.spec.whatwg.org/) is what a future reader can
# actually resolve, so the anchor is the durable key and the number is the
# secondary hint. The spec revision these sections were verified against is
# pinned in tests/testthat/fixtures/oracle-provenance.json, under this group's
# `normative_dependencies`.
#
# The roster additionally declares, per IPv4 row, `denotes`: the 32-bit integer
# the encoding is INTENDED to carry, written in a notation the input does not
# use. That is a second, independent statement -- human intent -- and the
# spec arithmetic either agrees with it or does not. `ipobf-005` is the row that
# proves the check has teeth: `0x7f.256` looks like a 127.0.0.1 encoding and is
# not one, because the WHATWG 2-part form spreads 256 over the low three
# octets. It declares 2130706688 (127.0.1.0), not 2130706433.
#
# FAIL CLOSED, NEVER "failure". Where an input uses a construction this file
# does not model, it MUST abort rather than return failure. Returning failure
# for something unmodeled would coincidentally agree with the four rows whose
# expectation IS failure, and manufacture confidence out of a gap. The two
# unmodeled constructions are named at their guards below.
#
# Sourced by `tools/oracle/verify-ip-obfuscation.R`. Running it directly prints
# the derived block and writes nothing.

# The literal prefix every row's `source_reference` carries, ahead of its note.
# Kept here because it is part of what the derivation asserts about the group:
# the encodings are facts, and the upstream projects are cited for the
# TECHNIQUE only, with no bytes taken from either.
IPOBF_SOURCE_PREFIX <- paste0(
  "IPv4-obfuscation technique per cujanovic/SSRF-Testing ip.py & ",
  "JorianWoltjer/ipobf (no LICENSE; encodings hand-generated as arithmetic ",
  "facts, not vendored) -- "
)

# ---- IPv4 number parser ----------------------------------------------------
# WHATWG URL Standard section 3.5 "Host parsing", algorithm #ipv4-number-parser
#
# Returns list(ok = TRUE, value = <double>) or list(ok = FALSE).
#
# Accumulates in a double on purpose. `strtoi("ffffffff", 16L)` returns NA --
# the value exceeds .Machine$integer.max -- which would turn `ipobf-022`'s
# 4294967295 into a parse failure for the WRONG reason and still agree with the
# fixture's "failure". Doubles are exact to 2^53; anything that could exceed
# that range aborts instead.
ipobf_ipv4_number_parser <- function(part) {
  radix <- 10
  if (!nzchar(part)) {
    return(list(ok = FALSE))
  }
  if (nchar(part) >= 2L && substr(part, 1L, 2L) %in% c("0x", "0X")) {
    part <- substring(part, 3L)
    radix <- 16
  } else if (nchar(part) >= 2L && substr(part, 1L, 1L) == "0") {
    part <- substring(part, 2L)
    radix <- 8
  }
  # Spec step 3: an input that was nothing but the prefix is the number zero,
  # so "0x" and "0" are both 0 rather than failures.
  if (!nzchar(part)) {
    return(list(ok = TRUE, value = 0))
  }
  if (nchar(part) * log2(radix) > 53) {
    stop("NOT MODELED: numeric part '", part, "' in radix ", radix,
         " cannot be held exactly in a double; refusing to answer rather ",
         "than compare an inexact value.", call. = FALSE)
  }
  digits <- strsplit(part, "", fixed = TRUE)[[1]]
  lowered <- chartr("ABCDEF", "abcdef", digits)
  idx <- match(lowered, c(as.character(0:9), letters[1:6]))
  if (anyNA(idx) || any(idx - 1L >= radix)) {
    return(list(ok = FALSE))
  }
  value <- 0
  for (d in idx) {
    value <- value * radix + (d - 1L)
  }
  list(ok = TRUE, value = value)
}

# The spec's "strictly split on U+002E": every field is kept, including empty
# ones. Base `strsplit()` drops TRAILING empty fields, so "1.2." would come
# back as two fields instead of three and the trailing-dot rule below would
# never fire. Appending a guard field and dropping it restores the spec shape.
ipobf_strict_split <- function(s) {
  guard <- "\u0001"
  if (grepl(guard, s, fixed = TRUE)) {
    stop("NOT MODELED: the split guard code point occurs in the input.",
         call. = FALSE)
  }
  parts <- strsplit(paste0(s, ".", guard), ".", fixed = TRUE)[[1]]
  parts[-length(parts)]
}

# A single trailing empty field is dropped (spec step 2 of the IPv4 parser and
# of "ends in a number"), which is what makes "127.0.0.1." an IPv4 address.
ipobf_drop_trailing_empty <- function(parts) {
  if (length(parts) > 1L && !nzchar(parts[length(parts)])) {
    return(parts[-length(parts)])
  }
  parts
}

# ---- IPv4 parser -----------------------------------------------------------
# WHATWG URL Standard section 3.5 "Host parsing", algorithm #concept-ipv4-parser
ipobf_ipv4_parser <- function(input) {
  parts <- ipobf_drop_trailing_empty(ipobf_strict_split(input))
  if (length(parts) > 4L) {
    return(list(ok = FALSE))
  }
  numbers <- numeric(0)
  for (p in parts) {
    r <- ipobf_ipv4_number_parser(p)
    if (!r$ok) {
      return(list(ok = FALSE))
    }
    numbers <- c(numbers, r$value)
  }
  n <- length(numbers)
  # Step 6: any item but the LAST above 255 is fatal. This is the step that
  # rejects `0xffffffff.0` -- not an overflow of the address, but of the single
  # octet that first part is allotted.
  if (n > 1L && any(numbers[-n] > 255)) {
    return(list(ok = FALSE))
  }
  # Step 7: the last item absorbs the remaining octets, so its ceiling depends
  # on how many parts were written. One part may carry the whole 32 bits.
  if (numbers[n] >= 256^(5L - n)) {
    return(list(ok = FALSE))
  }
  ipv4 <- numbers[n]
  rest <- if (n > 1L) numbers[-n] else numeric(0)
  for (i in seq_along(rest)) {
    ipv4 <- ipv4 + rest[i] * 256^(4L - i)
  }
  list(ok = TRUE, value = ipv4)
}

# ---- "ends in a number" checker ---------------------------------------------
# WHATWG URL Standard section 3.5 "Host parsing", algorithm
# #ends-in-a-number-checker
ipobf_ends_in_number <- function(domain) {
  parts <- ipobf_drop_trailing_empty(ipobf_strict_split(domain))
  last <- parts[length(parts)]
  if (nzchar(last) && grepl("^[0-9]+$", last)) {
    return(TRUE)
  }
  ipobf_ipv4_number_parser(last)$ok
}

# ---- IPv4 serializer --------------------------------------------------------
# WHATWG URL Standard section 3.6 "Host serializing", algorithm
# #concept-ipv4-serializer
ipobf_ipv4_serialize <- function(value) {
  octets <- character(4)
  n <- value
  for (i in 4:1) {
    octets[i] <- as.character(as.integer(n %% 256))
    n <- n %/% 256
  }
  paste(octets, collapse = ".")
}

# ---- IPv6 parser -----------------------------------------------------------
# WHATWG URL Standard section 3.5 "Host parsing", algorithm #concept-ipv6-parser
#
# A direct transcription, pointer and all, over CODE POINTS rather than native
# characters. Returns list(ok = TRUE, address = <8 doubles>) or
# list(ok = FALSE). The embedded-IPv4 branch is what `[::ffff:127.0.0.1]` needs
# and is also what rejects `[::ffff:127.0.0.1%2523]`: after the fourth octet
# the loop sees "%" where it requires "." or end of input.
ipobf_ipv6_parser <- function(input) {
  cp <- if (nzchar(input)) utf8ToInt(enc2utf8(input)) else integer(0)
  len_cp <- length(cp)
  at <- function(i) if (i >= 1L && i <= len_cp) cp[i] else -1L
  is_hex <- function(c) {
    (c >= 48L && c <= 57L) || (c >= 97L && c <= 102L) || (c >= 65L && c <= 70L)
  }
  is_digit <- function(c) c >= 48L && c <= 57L
  hex_value <- function(c) {
    if (c <= 57L) c - 48L else if (c >= 97L) c - 87L else c - 55L
  }
  colon <- 58L
  dot <- 46L

  address <- rep(0, 8)
  piece_index <- 0L
  compress <- NA_integer_
  ptr <- 1L

  if (at(ptr) == colon) {
    if (at(ptr + 1L) != colon) {
      return(list(ok = FALSE))
    }
    ptr <- ptr + 2L
    piece_index <- piece_index + 1L
    compress <- piece_index
  }

  while (at(ptr) != -1L) {
    if (piece_index == 8L) {
      return(list(ok = FALSE))
    }
    if (at(ptr) == colon) {
      if (!is.na(compress)) {
        return(list(ok = FALSE))
      }
      ptr <- ptr + 1L
      piece_index <- piece_index + 1L
      compress <- piece_index
      next
    }
    value <- 0
    length_seen <- 0L
    while (length_seen < 4L && is_hex(at(ptr))) {
      value <- value * 16 + hex_value(at(ptr))
      ptr <- ptr + 1L
      length_seen <- length_seen + 1L
    }
    if (at(ptr) == dot) {
      if (length_seen == 0L) {
        return(list(ok = FALSE))
      }
      ptr <- ptr - length_seen
      if (piece_index > 6L) {
        return(list(ok = FALSE))
      }
      numbers_seen <- 0L
      while (at(ptr) != -1L) {
        ipv4_piece <- NA_real_
        if (numbers_seen > 0L) {
          if (at(ptr) == dot && numbers_seen < 4L) {
            ptr <- ptr + 1L
          } else {
            return(list(ok = FALSE))
          }
        }
        if (!is_digit(at(ptr))) {
          return(list(ok = FALSE))
        }
        while (is_digit(at(ptr))) {
          number <- at(ptr) - 48L
          if (is.na(ipv4_piece)) {
            ipv4_piece <- number
          } else if (ipv4_piece == 0) {
            # A leading zero is fatal here, unlike in the IPv4 parser.
            return(list(ok = FALSE))
          } else {
            ipv4_piece <- ipv4_piece * 10 + number
          }
          if (ipv4_piece > 255) {
            return(list(ok = FALSE))
          }
          ptr <- ptr + 1L
        }
        address[piece_index + 1L] <-
          address[piece_index + 1L] * 256 + ipv4_piece
        numbers_seen <- numbers_seen + 1L
        if (numbers_seen == 2L || numbers_seen == 4L) {
          piece_index <- piece_index + 1L
        }
      }
      if (numbers_seen != 4L) {
        return(list(ok = FALSE))
      }
      break
    } else if (at(ptr) == colon) {
      ptr <- ptr + 1L
      if (at(ptr) == -1L) {
        return(list(ok = FALSE))
      }
    } else if (at(ptr) != -1L) {
      return(list(ok = FALSE))
    }
    address[piece_index + 1L] <- value
    piece_index <- piece_index + 1L
  }

  if (is.na(compress)) {
    if (piece_index != 8L) {
      return(list(ok = FALSE))
    }
  } else {
    swaps <- piece_index - compress
    piece_index <- 7L
    while (piece_index != 0L && swaps > 0L) {
      tmp <- address[piece_index + 1L]
      address[piece_index + 1L] <- address[compress + swaps]
      address[compress + swaps] <- tmp
      piece_index <- piece_index - 1L
      swaps <- swaps - 1L
    }
  }
  list(ok = TRUE, address = address)
}

# ---- IPv6 serializer --------------------------------------------------------
# WHATWG URL Standard section 3.6 "Host serializing", algorithm
# #concept-ipv6-serializer
#
# Compression applies to the FIRST longest run of zero pieces, and only when
# that run is longer than one piece -- which is why [::ffff:7f00:1] compresses
# five leading zeroes but a lone interior zero stays written out.
ipobf_ipv6_serialize <- function(address) {
  best_start <- NA_integer_
  best_len <- 0L
  i <- 1L
  while (i <= 8L) {
    if (address[i] == 0) {
      j <- i
      while (j <= 8L && address[j] == 0) {
        j <- j + 1L
      }
      run <- j - i
      if (run > best_len) {
        best_len <- run
        best_start <- i
      }
      i <- j
    } else {
      i <- i + 1L
    }
  }
  compress <- if (best_len > 1L) best_start - 1L else NA_integer_

  out <- ""
  ignore_zero <- FALSE
  for (piece_index in 0:7) {
    if (ignore_zero && address[piece_index + 1L] == 0) {
      next
    }
    if (ignore_zero) {
      ignore_zero <- FALSE
    }
    if (!is.na(compress) && compress == piece_index) {
      out <- paste0(out, if (piece_index == 0L) "::" else ":")
      ignore_zero <- TRUE
      next
    }
    out <- paste0(out, sprintf("%x", as.integer(address[piece_index + 1L])))
    if (piece_index != 7L) {
      out <- paste0(out, ":")
    }
  }
  out
}

# ---- UTS-46, the narrow slice this group exercises -------------------------
#
# WHATWG host parser step 4 runs "domain to ASCII", i.e. UTS-46 ToASCII. Three
# of the 24 rows exist solely to exercise one fact from it: the IDNA mapping
# table maps the full-stop variants U+3002, U+FF0E and U+FF61 to U+002E, so a
# host written with them is the same domain as one written with ASCII dots and
# must then be coerced by the IPv4 parser.
#
# Only that slice is transcribed, plus the lowercase mapping. Any OTHER
# non-ASCII code point aborts -- a full UTS-46 table is not something to
# approximate, and a wrong answer here would silently move an expectation.
# `chartr()` rather than `tolower()` because `tolower()` is locale-sensitive
# and this gate runs under LC_ALL=C as well as a UTF-8 locale.
ipobf_uts46_to_ascii <- function(host) {
  host <- gsub("\u3002", ".", host, fixed = TRUE)
  host <- gsub("\uff0e", ".", host, fixed = TRUE)
  host <- gsub("\uff61", ".", host, fixed = TRUE)
  host <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                 "abcdefghijklmnopqrstuvwxyz", host)
  if (nzchar(host) && any(utf8ToInt(enc2utf8(host)) > 127L)) {
    stop("NOT MODELED: host '", host, "' carries a non-ASCII code point ",
         "outside the three full-stop variants this file transcribes from ",
         "the UTS-46 mapping table. Refusing to guess a ToASCII result.",
         call. = FALSE)
  }
  host
}

# Forbidden domain code point (WHATWG host parser step 5): a forbidden HOST
# code point, plus C0 controls, U+007F and "%".
ipobf_has_forbidden_domain_cp <- function(host) {
  if (!nzchar(host)) {
    return(FALSE)
  }
  cp <- utf8ToInt(enc2utf8(host))
  forbidden <- utf8ToInt(" #/:<>?@[\\]^|%")
  any(cp <= 31L) || any(cp == 127L) || any(cp %in% forbidden)
}

# ---- host parser ------------------------------------------------------------
# WHATWG URL Standard section 3.5 "Host parsing", algorithm #concept-host-parser
#
# Returns list(ok = TRUE, host = <serialized host>) or list(ok = FALSE).
# `is_opaque` is not a parameter: every row here has a special scheme, which is
# the branch that coerces numeric hosts at all.
ipobf_host_parse <- function(host) {
  if (startsWith(host, "[")) {
    if (!endsWith(host, "]")) {
      return(list(ok = FALSE))
    }
    inner <- substring(host, 2L, nchar(host) - 1L)
    r <- ipobf_ipv6_parser(inner)
    if (!r$ok) {
      return(list(ok = FALSE))
    }
    return(list(ok = TRUE,
                host = paste0("[", ipobf_ipv6_serialize(r$address), "]")))
  }
  # A special scheme with an empty host is a parse failure, and no row here
  # has one; keep the branch so an empty host can never fall through to the
  # domain path and be reported as an accepted empty domain.
  if (!nzchar(host)) {
    return(list(ok = FALSE))
  }
  # Step 4 percent-decodes before ToASCII. No row in the roster carries a
  # percent-encoding in its host, and a half-right decoder would change which
  # code points reach the forbidden-code-point check, so this aborts.
  if (grepl("%", host, fixed = TRUE)) {
    stop("NOT MODELED: host '", host, "' carries a percent sign; host-parser ",
         "step 4's percent-decode is not transcribed here.", call. = FALSE)
  }
  ascii_domain <- ipobf_uts46_to_ascii(host)
  if (ipobf_has_forbidden_domain_cp(ascii_domain)) {
    return(list(ok = FALSE))
  }
  if (ipobf_ends_in_number(ascii_domain)) {
    r <- ipobf_ipv4_parser(ascii_domain)
    if (!r$ok) {
      return(list(ok = FALSE))
    }
    return(list(ok = TRUE, host = ipobf_ipv4_serialize(r$value),
                ipv4 = r$value))
  }
  list(ok = TRUE, host = ascii_domain)
}

# ---- the roster ------------------------------------------------------------
#
# The 24 encodings, and for each IPv4 row the 32-bit integer it is INTENDED to
# denote. Non-ASCII inputs are written as \u escapes so this file stays pure
# ASCII and the strings are unambiguous regardless of source encoding.
#
# `denotes` is NA for the two bracketed IPv6 rows (their input is already in
# IPv6 notation, so there is no separate arithmetic intent to cross-check) and
# for the four must-fail rows (an encoding that does not denote an address).
# The `note` text is the fixture's own `notes` cell and the tail of its
# `source_reference`; it travels here so a row cannot be silently re-paired
# with another row's description.
ip_obfuscation_roster <- function() {
  row <- function(input, denotes, note) {
    data.frame(input = input, denotes = denotes, note = note,
               stringsAsFactors = FALSE)
  }
  u3002 <- "\u3002"
  uff0e <- "\uff0e"
  uff61 <- "\uff61"
  loopback <- 2130706433
  metadata <- 2852039166

  rbind(
    # --- 127.0.0.1 family (loopback) ---------------------------------------
    row("http://0x7f.0x0.0x0.0x1/", loopback,
        "dotted hexadecimal, all four octets in hex"),
    row("http://0x7f.0x00.0x00.0x01/", loopback,
        "dotted hexadecimal with zero-padding"),
    row("http://0x7f.1/", loopback,
        paste0("mixed radix: hex first part, short 2-part form fills last 3 ",
               "octets")),
    row("http://0x7f.00.0.1/", loopback,
        "mixed radix hex + octal + decimal parts"),
    # The counter-example, and the row that gives `denotes` its teeth.
    row("http://0x7f.256/", 2130706688,
        paste0("hex + short 2-part: 256 spans the last 3 octets -> a ",
               "DIFFERENT ip; the conformant WHATWG result is 127.0.1.0, ",
               "demonstrating the parser rule, not an alias of 127.0.0.1")),
    row("http://0177.00.00.01/", loopback,
        "dotted octal, per-part octal radix"),
    row("http://0177.000.0000.00001/", loopback,
        paste0("dotted octal with heavy zero-padding (padding does not ",
               "change value)")),
    row("http://0177.0x0.1/", loopback,
        "mixed radix octal + hex + decimal parts"),
    row("http://127.1/", loopback,
        "short 2-part form: last part fills the low 3 octets"),
    row("http://127.0.1/", loopback,
        "short 3-part form: last part fills the low 2 octets"),
    row(paste0("http://127", u3002, "0", u3002, "0", u3002, "1/"), loopback,
        paste0("U+3002 ideographic full stop as separator; UTS-46 maps to ",
               "'.' -> should coerce to 127.0.0.1")),
    row(paste0("http://127", uff0e, "0", uff0e, "0", uff0e, "1/"), loopback,
        paste0("U+FF0E fullwidth full stop as separator; UTS-46 maps to '.' ",
               "-> should coerce to 127.0.0.1")),
    row(paste0("http://127", uff61, "0", uff61, "0", uff61, "1/"), loopback,
        paste0("U+FF61 halfwidth ideographic full stop as separator; UTS-46 ",
               "maps to '.' -> should coerce to 127.0.0.1")),

    # --- 169.254.169.254 family (cloud link-local metadata) ----------------
    row("http://0xa9fea9fe/", metadata, "dotless hexadecimal whole number"),
    row("http://0xa9.0xfe.0xa9.0xfe/", metadata, "dotted hexadecimal"),
    row("http://2852039166/", metadata,
        "dotless decimal (dword) whole number"),
    row("http://0251.0376.0251.0376/", metadata, "dotted octal"),
    row("http://0xa9.254.0xa9.0376/", metadata,
        "mixed radix hex + decimal + hex + octal"),

    # --- bracketed IPv6 (legitimate hosts, NOT IPv4 coercion) --------------
    row("http://[::ffff:127.0.0.1]/", NA_real_,
        paste0("IPv4-mapped IPv6; a bracketed IPv6 host, not an ",
               "IPv4-coercion candidate (recorded for provenance)")),
    row("http://[::127.0.0.1]/", NA_real_,
        "IPv4-compatible IPv6; bracketed IPv6 host (recorded for provenance)"),

    # --- must-fail forms ---------------------------------------------------
    row("http://256.256.256.1/", NA_real_,
        paste0("4-part dotted-decimal with parts > 255 -> WHATWG IPv4 parse ",
               "fails")),
    row("http://0xffffffff.0/", NA_real_,
        paste0("2-part form, first part 0xffffffff overflows its allotted ",
               "octet range")),
    row("http://6442450945/", NA_real_,
        "2130706433 + 2^32: dword overflow trick, still > 2^32-1 -> fatal"),
    row("http://[::ffff:127.0.0.1%2523]/", NA_real_,
        paste0("IPv6 with a percent-encoded zone-id bypass ('%2523' -> ",
               "'%23') -> forbidden, fatal"))
  )
}

# Split `http://<host>/` and refuse anything else. The oracle here is about
# HOST parsing; accepting a richer URL shape would mean transcribing the URL
# parser too, so a row that is not this shape aborts rather than being parsed
# by guesswork.
ipobf_split_host <- function(url) {
  m <- regmatches(url, regexec("^http://([^/]*)/$", url))[[1]]
  if (length(m) != 2L) {
    stop("NOT MODELED: '", url, "' is not of the form http://<host>/ , which ",
         "is the only shape this derivation parses.", call. = FALSE)
  }
  m[2]
}

# Derive the ip-obfuscation oracle.
#
# Returns a data.frame with one row per roster entry:
#   input   -- the encoding
#   kind    -- "exact" if the WHATWG host parser accepts, "failure" if it
#              rejects. Mirrors the fixture's `oracle_kind`.
#   host    -- the serialized host, or NA on failure
#   href    -- the URL serialization (URL Standard section 4.5 "URL
#              serializing", algorithm #concept-url-serializer), or NA on
#              failure
#   ipv4    -- the 32-bit value when the host was IPv4-coerced, else NA
#   denotes -- the roster's declared intent, for the caller to cross-check
#   note    -- the roster's description
#   source_reference -- prefix + note, as the fixture records it
derive_ip_obfuscation <- function(roster = ip_obfuscation_roster()) {
  if (!nrow(roster)) {
    stop("FATAL: the roster is empty; there is nothing to derive.",
         call. = FALSE)
  }
  n <- nrow(roster)
  kind <- character(n)
  host <- rep(NA_character_, n)
  href <- rep(NA_character_, n)
  ipv4 <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    parsed <- ipobf_host_parse(ipobf_split_host(roster$input[i]))
    if (parsed$ok) {
      kind[i] <- "exact"
      host[i] <- parsed$host
      # WHATWG URL Standard section 4.5 "URL serializing", algorithm
      # #concept-url-serializer, for this shape: no credentials, no port,
      # path "/", no query and no fragment.
      href[i] <- paste0("http://", parsed$host, "/")
      if (!is.null(parsed$ipv4)) {
        ipv4[i] <- parsed$ipv4
      }
    } else {
      kind[i] <- "failure"
    }
  }

  data.frame(
    input = roster$input, kind = kind, host = host, href = href, ipv4 = ipv4,
    denotes = roster$denotes, note = roster$note,
    source_reference = paste0(IPOBF_SOURCE_PREFIX, roster$note),
    stringsAsFactors = FALSE
  )
}

# Runs only when this file is executed as a script: `sys.nframe()` is 0 at the
# top level of an Rscript invocation and non-zero inside the verifier's
# `source()` call. The earlier form also tested `!is.null(sys.frames())`, which
# is FALSE at top level -- so the block never fired and the header's "running it
# directly prints ..." was untrue of all three modules.
if (sys.nframe() == 0L) {
  d <- derive_ip_obfuscation()
  cat("derived rows:", nrow(d), "\n")
  print(table(d$kind))
  for (i in seq_len(nrow(d))) {
    cat(sprintf("  %-34s -> %s\n", encodeString(d$input[i]),
                if (is.na(d$href[i])) "failure" else d$href[i]))
  }
}
