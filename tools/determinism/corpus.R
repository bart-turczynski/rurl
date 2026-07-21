#!/usr/bin/env Rscript
# Cross-platform parse-determinism corpus generator (RURL-ttrcfneq).
#
# NOT a test and NOT shipped in the package (.Rbuildignore'd). Generates the
# probe corpus used by parse-dump.R to characterize how libcurl's URL parser
# -- and therefore rurl's OUTPUT -- varies across libcurl versions and
# platforms. win-builder r-devel exposed the class (file:// parse_status and
# U+FFFF rendering differ from the macOS-recorded values); this corpus sweeps
# the whole suspected surface rather than the two cases that happened to fail.
#
#   Rscript tools/determinism/corpus.R
#
# Output (committed, regenerable -- same discipline as the inst/bench oracles):
#   tools/determinism/corpus.csv   columns: id, construct, input_json
#
# ZERO package dependencies: base R only. The scripts must run inside minimal
# Docker images with nothing but R + rurl installed.
#
# TRANSPORT DISCIPLINE. The corpus deliberately contains C0 controls, CR/LF,
# astral-plane and non-character code points. Those CANNOT survive a raw CSV
# cell (that exact bug cost a Windows CI failure and a silent CRLF
# corruption), so every input is stored as a JSON string literal -- quoted,
# ASCII-only, non-ASCII as \uXXXX. corpus.csv is pure ASCII and LF-terminated.
#
# ID STABILITY. Downstream diffs join on `id`, so ids must not shift when a
# construct is added. Ids are derived from the construct plus a stable key
# (code point hex, or the index within a hand-authored vector). Therefore:
# ONLY APPEND to the hand-authored vectors below -- never insert or reorder.
# ----------------------------------------------------------------------------

# ---- JSON escape (base R) --------------------------------------------------
# Deliberately duplicated in parse-dump.R so each script is copy-one-file
# runnable inside a container. The two copies must stay in sync; parse-dump.R
# guards this by round-tripping every corpus.csv row through ITS copy.

# LOCALE-INVARIANT code points (RURL-cupshtnh). enc2utf8() must NEVER appear
# here: it TRANSCODES from the session locale, so the SAME bytes rendered
# under LC_ALL=C and under en_US.UTF-8 produced different text and the
# harness ended up measuring itself instead of rurl (it turned an unmarked
# UTF-8 host into "<d0><b0>pple.com" under C, and threw on a
# marked-but-invalid-UTF-8 value). This reads the BYTES and the declared
# Encoding() mark only:
#
#   * bytes that form well-formed UTF-8 decode to their code points --
#     whatever the session locale, and whatever mark the string carries;
#   * every other byte becomes the lone low surrogate 0xDC00 + byte (the
#     PEP-383 "surrogateescape" convention): deterministic, byte-reversible,
#     written as ASCII \udcXX, and unreachable by decoding valid UTF-8, so it
#     can never be confused with a genuine code point.
#
# The escaper is therefore TOTAL: invalid UTF-8 is RENDERED, never an error.
# The corpus deliberately produces such values, and a dump that abandons a
# row is worse evidence than one that records the bytes it saw.

utf8_codepoints <- function(s) {
  s8 <- s
  Encoding(s8) <- "UTF-8"                # declare; never transcode
  cps <- tryCatch(suppressWarnings(utf8ToInt(s8)),
                  error = function(e) NA_integer_)
  if (!anyNA(cps)) return(cps)
  raw_codepoints(charToRaw(s))
}

# Strict UTF-8 decode of a raw vector. Truncated sequences, bad continuation
# bytes, overlong forms, surrogates and anything above U+10FFFF are rejected
# one byte at a time into the surrogateescape range.
raw_codepoints <- function(b) {
  n <- length(b)
  if (n == 0L) return(integer(0))
  v <- as.integer(b)
  cps <- integer(n)
  k <- 0L
  i <- 1L
  while (i <= n) {
    b0 <- v[i]
    len <- if (b0 <= 0x7fL) {
      1L
    } else if (b0 >= 0xc2L && b0 <= 0xdfL) {
      2L
    } else if (b0 >= 0xe0L && b0 <= 0xefL) {
      3L
    } else if (b0 >= 0xf0L && b0 <= 0xf4L) {
      4L
    } else {
      0L
    }
    cp <- NA_integer_
    if (len > 0L && i + len - 1L <= n) {
      acc <- bitwAnd(b0, c(0x7fL, 0x1fL, 0x0fL, 0x07L)[len])
      ok <- TRUE
      for (j in seq_len(len - 1L)) {
        cont <- v[i + j]
        if (cont < 0x80L || cont > 0xbfL) {
          ok <- FALSE
          break
        }
        acc <- acc * 64L + (cont - 0x80L)
      }
      lo <- c(0L, 0x80L, 0x800L, 0x10000L)[len]
      if (ok && acc >= lo && acc <= 0x10ffffL &&
            (acc < 0xd800L || acc > 0xdfffL)) {
        cp <- acc
      }
    }
    k <- k + 1L
    if (is.na(cp)) {
      cps[k] <- 0xdc00L + b0
      i <- i + 1L
    } else {
      cps[k] <- cp
      i <- i + len
    }
  }
  cps[seq_len(k)]
}

json_escape_one <- function(s) {
  if (is.na(s)) return("null")
  cps <- utf8_codepoints(s)
  if (length(cps) == 0L) return("\"\"")
  out <- character(length(cps))
  for (i in seq_along(cps)) {
    cp <- cps[i]
    out[i] <- if (cp == 34L) {
      "\\\""
    } else if (cp == 92L) {
      "\\\\"
    } else if (cp == 8L) {
      "\\b"
    } else if (cp == 9L) {
      "\\t"
    } else if (cp == 10L) {
      "\\n"
    } else if (cp == 12L) {
      "\\f"
    } else if (cp == 13L) {
      "\\r"
    } else if (cp >= 32L && cp <= 126L) {
      intToUtf8(cp)
    } else if (cp <= 0xFFFFL) {
      sprintf("\\u%04x", cp)
    } else {
      u <- cp - 0x10000L
      sprintf("\\u%04x\\u%04x",
              0xD800L + u %/% 0x400L, 0xDC00L + u %% 0x400L)
    }
  }
  paste0("\"", paste(out, collapse = ""), "\"")
}

json_escape <- function(x) {
  vapply(x, json_escape_one, character(1), USE.NAMES = FALSE)
}

json_unescape_one <- function(s) {
  if (is.na(s) || identical(s, "null")) return(NA_character_)
  n <- nchar(s)
  if (n < 2L || substr(s, 1L, 1L) != "\"" || substr(s, n, n) != "\"") {
    stop("not a JSON string literal: ", s)
  }
  ch <- strsplit(substr(s, 2L, n - 1L), "", fixed = TRUE)[[1]]
  cps <- integer(0)
  i <- 1L
  m <- length(ch)
  while (i <= m) {
    if (ch[i] != "\\") {
      cps <- c(cps, utf8ToInt(ch[i]))
      i <- i + 1L
      next
    }
    e <- ch[i + 1L]
    if (e == "u") {
      hi <- strtoi(paste(ch[(i + 2L):(i + 5L)], collapse = ""), 16L)
      i <- i + 6L
      if (hi >= 0xD800L && hi <= 0xDBFFL && i + 5L <= m + 0L &&
            identical(ch[i], "\\") && identical(ch[i + 1L], "u")) {
        lo <- strtoi(paste(ch[(i + 2L):(i + 5L)], collapse = ""), 16L)
        i <- i + 6L
        hi <- 0x10000L + (hi - 0xD800L) * 0x400L + (lo - 0xDC00L)
      }
      cps <- c(cps, hi)
    } else {
      simple <- c("\"" = 34L, "\\" = 92L, "/" = 47L, "b" = 8L,
                  "f" = 12L, "n" = 10L, "r" = 13L, "t" = 9L)[e]
      if (is.na(simple)) stop("bad JSON escape: \\", e)
      cps <- c(cps, unname(simple))
      i <- i + 2L
    }
  }
  if (length(cps) == 0L) return("")
  intToUtf8(cps)
}

json_unescape <- function(x) {
  vapply(x, json_unescape_one, character(1), USE.NAMES = FALSE)
}

# ---- corpus accumulator ----------------------------------------------------

ROWS <- new.env(parent = emptyenv())
ROWS$id <- character(0)
ROWS$construct <- character(0)
ROWS$input <- character(0)

add <- function(id, construct, input) {
  ROWS$id <- c(ROWS$id, id)
  ROWS$construct <- c(ROWS$construct, construct)
  ROWS$input <- c(ROWS$input, input)
  invisible(NULL)
}

# Append-only vector -> one row per element, id = <prefix>-NN (1-based index).
add_seq <- function(prefix, construct, inputs) {
  for (i in seq_along(inputs)) {
    add(sprintf("%s-%02d", prefix, i), construct, inputs[i])
  }
}

u <- function(...) intToUtf8(c(...))

# ---- 1. C0 controls (and DEL) by position ----------------------------------
# 0x01-0x1F plus 0x7F. NUL (0x00) is unrepresentable in an R string and is
# excluded by construction. Tab/LF/CR are part of this sweep (WHATWG strips
# them from input; libcurl historically does not) and get extra dedicated
# rows in the c0-whatwg-strip bucket below.

c0_cps <- c(0x01:0x1F, 0x7F)
c0_slots <- list(
  leading   = function(c) paste0(c, "http://example.com/p?q=1#f"),
  trailing  = function(c) paste0("http://example.com/p?q=1#f", c),
  scheme    = function(c) paste0("ht", c, "tp://example.com/p?q=1#f"),
  host      = function(c) paste0("http://exam", c, "ple.com/p?q=1#f"),
  path      = function(c) paste0("http://example.com/p", c, "x?q=1#f"),
  query     = function(c) paste0("http://example.com/p?q", c, "=1#f"),
  fragment  = function(c) paste0("http://example.com/p?q=1#f", c, "g")
)
for (slot in names(c0_slots)) {
  mk <- c0_slots[[slot]]
  for (cp in c0_cps) {
    add(sprintf("c0-%s-%02x", slot, cp),
        paste0("c0-control-", slot),
        mk(u(cp)))
  }
}

# Tab/LF/CR specifics: repeated, paired (CRLF), and spanning delimiters.
add_seq("c0-strip", "c0-whatwg-strip", c(
  paste0("http://exa", u(0x09), "mple.com/", u(0x0A), "p"),
  paste0("http://example.com/", u(0x0D, 0x0A), "p"),
  paste0("http://example.com/p", u(0x0D, 0x0A)),
  paste0(u(0x0D, 0x0A), "http://example.com/p"),
  paste0("http", u(0x0A), "://example.com/p"),
  paste0("http:", u(0x09), "//example.com/p"),
  paste0("http://example.com", u(0x09), ":80/p"),
  paste0("http://example.com:", u(0x0A), "80/p"),
  paste0("http://us", u(0x0D), "er:pw@example.com/p"),
  paste0("http://example.com/p?a=", u(0x09), "1&b=", u(0x0A), "2"),
  paste0("http://example.com/p#fr", u(0x0D), "ag"),
  paste0("http://ex", u(0x09), u(0x0A), u(0x0D), "ample.com/p")
))

# ---- 2. IPv4 obfuscation ---------------------------------------------------

add_seq("ipv4-octal", "ipv4-octal", c(
  "http://0177.0.0.1/", "http://0177.00.00.01/",
  "http://0177.000.0000.00001/", "http://0251.0376.0251.0376/",
  "http://010.010.010.010/", "http://0300.0250.0.01/"
))
add_seq("ipv4-hex", "ipv4-hex", c(
  "http://0x7f.0x0.0x0.0x1/", "http://0x7f.0x00.0x00.0x01/",
  "http://0xa9.0xfe.0xa9.0xfe/", "http://0X7F.0X0.0X0.0X1/",
  "http://0x7f000001/", "http://0xa9fea9fe/"
))
add_seq("ipv4-dword", "ipv4-dword", c(
  "http://2130706433/", "http://2852039166/", "http://3232235777/",
  "http://4294967295/", "http://4294967296/", "http://0/"
))
add_seq("ipv4-mixed", "ipv4-mixed-radix", c(
  "http://0x7f.00.0.1/", "http://0177.0x0.1/", "http://0xa9.254.0xa9.0376/",
  "http://0x7f.1/", "http://127.0x1/", "http://0177.1.0x1/"
))
add_seq("ipv4-padded", "ipv4-padded-octet", c(
  "http://127.000.000.001/", "http://0000127.0.0.1/",
  "http://127.0.0.0000001/", "http://00000000000000000001.0.0.1/"
))
add_seq("ipv4-short", "ipv4-short-form", c(
  "http://127.1/", "http://127.0.1/", "http://192.168.1/",
  "http://1/", "http://1.1/"
))
add_seq("ipv4-dot", "ipv4-trailing-dot", c(
  "http://127.0.0.1./", "http://127.0.0.1../", "http://.127.0.0.1/",
  "http://example.com./", "http://127.0.0.1.:80/"
))
add_seq("ipv4-range", "ipv4-out-of-range", c(
  "http://127.0.0.256/", "http://256.256.256.256/", "http://999.999.999.999/",
  "http://127.0.0.1.1/", "http://0xffffffff.0/",
  "http://0xffffffff1/", "http://-1.0.0.1/", "http://127.0.0.0x100/"
))

# ---- 3. file:// variants ---------------------------------------------------

add_seq("file-authority", "file-authority", c(
  "file://example.com/path",      # confirmed divergent (macOS error / Win ok)
  "file:///path",
  "file://localhost/path",
  "file://./path",
  "file://../path",
  "file:///",
  "file://",
  "file:/path",
  "file:path",
  "file://user@example.com/path",
  "file://example.com:80/path",
  "file://LOCALHOST/path"
))
add_seq("file-drive", "file-drive-letter", c(
  "file:C:/x", "file:///C:/x", "file://C|/x", "file:C|/x", "file:C||/x",
  "file:/C|/", "file:///c:/Windows/System32", "file://localhost/C:/x",
  "file:///C:", "file:///C:/"
))
add_seq("file-backslash", "file-backslash-separator", c(
  "file://\\\\server\\share", "file:///C:\\x\\y", "file:\\\\C:\\x",
  "file://example.com\\path", "file:///path\\to\\file"
))

# ---- 4. empty components ---------------------------------------------------

add_seq("empty", "empty-component", c(
  "http://", "http:///path", "http://:80/", "http://example.com:",
  "http://example.com:/path", "http://example.com?", "http://example.com#",
  "http://example.com/?#", "http://example.com", "//example.com/path",
  "//", "///path", "http://@example.com/", "http://:@example.com/",
  "http://user@example.com/", "http://:pw@example.com/", "http:",
  "", " ", "http://example.com/?a=", "http://example.com/#",
  "http://example.com/p?#f"
))

# ---- 5. IDN / Unicode hosts ------------------------------------------------

add_seq("idn-nonchar", "idn-noncharacter", c(
  paste0("https://", u(0xFFFF), "y"),
  paste0("https://", u(0xFFFE), "y"),
  paste0("https://ex", u(0xFFFF), "ample.com/"),
  paste0("https://example.com/p", u(0xFFFF)),
  paste0("https://example.com/?q=", u(0xFFFF)),
  paste0("https://", u(0xFDD0), "example.com/"),
  paste0("https://exam", u(0x1FFFE), "ple.com/")
))
add_seq("idn-astral", "idn-astral-plane", c(
  paste0("https://", u(0x1F600), ".com/"),
  paste0("https://ex", u(0x1D11E), "ample.com/"),
  paste0("https://example.com/", u(0x1F600)),
  paste0("https://example.com/?q=", u(0x2070E)),
  paste0("https://", u(0x10FFFF), ".com/"),
  paste0("https://", u(0x1F600), u(0x1F601), ".example.com/")
))
add_seq("idn-invisible", "idn-invisible-codepoint", c(
  paste0("https://exam", u(0x00AD), "ple.com/"),      # soft hyphen
  paste0("https://exam", u(0x200B), "ple.com/"),      # zero width space
  paste0("https://exam", u(0x200D), "ple.com/"),      # ZWJ
  paste0("https://exam", u(0xFEFF), "ple.com/"),      # BOM / ZWNBSP
  paste0("https://exam", u(0x00A0), "ple.com/"),      # NBSP
  paste0("https://", u(0x00AD), "example.com/")
))
add_seq("idn-separator", "idn-label-separator", c(
  paste0("http://127", u(0x3002), "0", u(0x3002), "0", u(0x3002), "1/"),
  paste0("http://127", u(0xFF0E), "0", u(0xFF0E), "0", u(0xFF0E), "1/"),
  paste0("http://127", u(0xFF61), "0", u(0xFF61), "0", u(0xFF61), "1/"),
  paste0("https://example", u(0x3002), "com/"),
  paste0("https://example", u(0xFF0E), "com/"),
  paste0("https://example", u(0xFF61), "com/")
))
add_seq("idn-puny", "idn-punycoded-label", c(
  "https://xn--bcher-kva.example/",
  "https://xn--e1afmkfd.xn--p1ai/",
  "https://xn--/",
  "https://xn--a.com/",
  "https://xn--0.com/",
  paste0("https://xn--bcher-kva.", u(0x043F, 0x0440), ".com/")
))
add_seq("idn-mixed", "idn-mixed-script", c(
  paste0("https://", u(0x0430), "pple.com/"),          # Cyrillic a
  paste0("https://p", u(0x0430), "ypal.com/"),
  paste0("https://", u(0x4F8B), u(0x5B50), ".test/"),  # Han
  paste0("https://", u(0x03BF), "range.com/"),         # Greek omicron
  paste0("https://caf", u(0x00E9), ".example/"),
  paste0("https://cafe", u(0x0301), ".example/")       # NFC vs NFD
))
add_seq("idn-bidi", "idn-bidi", c(
  paste0("https://", u(0x05D0, 0x05D1, 0x05D2), ".example/"),
  paste0("https://", u(0x0627, 0x0628, 0x062C), ".example/"),
  paste0("https://a", u(0x05D0), "1.example/"),
  paste0("https://", u(0x202E), "example.com/"),
  paste0("https://exa", u(0x200F), "mple.com/")
))
add_seq("idn-long", "idn-overlong-label", c(
  paste0("https://", strrep("a", 63L), ".com/"),
  paste0("https://", strrep("a", 64L), ".com/"),
  paste0("https://", strrep("a", 255L), ".com/"),
  paste0("https://", strrep(paste0(strrep("a", 63L), "."), 4L), "com/"),
  paste0("https://", strrep(u(0x00E9), 40L), ".com/")
))

# ---- 6. known-divergent seeds ----------------------------------------------
# The five rows win-builder r-devel actually caught. Sources:
#   tests/testthat/test-url-standard-file.R:41-47   (file forms)
#   tests/testthat/fixtures/external-url-vectors.csv rows wpt-fail-242,
#   wpt-fail-239, ipobf-022 (U+FFFF host + obfuscated numeric hosts)
# The harness must reproduce the original failure signal, so these are carried
# in their own bucket even though the same strings also appear elsewhere.

add_seq("known-div", "known-divergent", c(
  "file://example.com/path",
  "file:///c:/Windows/System32",
  paste0("https://", u(0xFFFF), "y"),
  "http://foo.0XFfFfFfFfFfFfFfFfFfAcE123",
  "http://0xffffffff.0/"
))

# ---- 7. the existing 350-vector external fixture ---------------------------
# Read the ASCII-only input_json column (NOT the raw `input` column, which is
# UTF-8 and mis-decodes on a Windows non-UTF-8 locale -- the very bug that
# produced part of the win-builder failure). The fixture is never modified.
#
# `encoding = "UTF-8"`, NOT `fileEncoding = "UTF-8"`: the latter *transcodes*
# the file from UTF-8 to the session's native encoding on read, and under a
# non-UTF-8 locale (LC_ALL=C, the non-UTF-8 Windows default) that conversion
# hits the raw UTF-8 `input` column, warns "invalid input found on input
# connection", and SILENTLY TRUNCATES the read at the first offending row --
# 282 runnable rows collapse to 19. `encoding = "UTF-8"` marks the bytes
# instead of converting them, so the whole file is read identically in every
# locale (RURL-qpuphdrz). The row-count assertion below turns any remaining
# short read into a hard error rather than a silent shrink of the corpus that
# is the denominator for every divergence figure the harness reports.

# The corpus is a fixed, byte-identically-regenerable denominator: this count
# must change only by a deliberate edit here, never by an unnoticed short read.
EXPECTED_FIXTURE_RUNNABLE <- 282L

fixture_path <- function() {
  cand <- c(
    file.path("tests", "testthat", "fixtures", "external-url-vectors.csv"),
    file.path("..", "..", "tests", "testthat", "fixtures",
              "external-url-vectors.csv")
  )
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot locate external-url-vectors.csv (run from ",
                       "the repo root).")
  hit
}

fx <- utils::read.csv(fixture_path(), colClasses = "character",
                      encoding = "UTF-8", stringsAsFactors = FALSE)
fx <- fx[fx$runnable == "yes", , drop = FALSE]
if (nrow(fx) != EXPECTED_FIXTURE_RUNNABLE) {
  stop(sprintf(paste0("external fixture read %d runnable rows, expected %d. ",
                      "A short read (e.g. a non-UTF-8 locale truncating the ",
                      "UTF-8 fixture) would silently shrink the corpus; if ",
                      "the fixture genuinely changed, update ",
                      "EXPECTED_FIXTURE_RUNNABLE."),
              nrow(fx), EXPECTED_FIXTURE_RUNNABLE))
}
for (i in seq_len(nrow(fx))) {
  add(paste0("ext-", fx$id[i]), "external-vector",
      json_unescape_one(fx$input_json[i]))
}

# ---- write -----------------------------------------------------------------

out <- data.frame(
  id = ROWS$id,
  construct = ROWS$construct,
  input_json = json_escape(ROWS$input),
  stringsAsFactors = FALSE
)

if (anyDuplicated(out$id) > 0L) {
  stop("duplicate corpus ids: ",
       toString(unique(out$id[duplicated(out$id)])))
}
non_ascii <- grep("[^\x20-\x7e]", out$input_json)
if (length(non_ascii) > 0L) {
  stop("non-ASCII input_json rows: ", toString(out$id[non_ascii]))
}
rt <- json_unescape(out$input_json)
if (!identical(rt, ROWS$input)) {
  stop("JSON round-trip failed for ",
       toString(head(out$id[rt != ROWS$input], 5L)))
}

dest <- if (dir.exists("tools/determinism")) {
  "tools/determinism/corpus.csv"
} else {
  "corpus.csv"
}
con <- file(dest, open = "wb")          # binary: force LF, never CRLF
utils::write.csv(out, con, row.names = FALSE, quote = TRUE, eol = "\n")
close(con)

cat(sprintf("wrote %s: %d rows, %d constructs\n",
            dest, nrow(out), length(unique(out$construct))))
print(table(out$construct))
