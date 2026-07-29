#!/usr/bin/env Rscript
#
# 0-255 octet acceptance + round-trip sweep.
#
# The standing rule at a host seam: any change there requires accepted counts
# AND per-row `parse_status`, per profile, plus byte-identical serialization --
# because masking a host bypasses rejection and silently WIDENS acceptance,
# which has happened in this package before (RURL-dxwxeamq, RURL-savatsuc,
# RURL-ezhzpkhg). This script is that instrument. It lived in a session
# scratchpad through the in-tree parser work and was rebuilt from scratch twice;
# it is checked in so the next host-seam change scores against the same
# corpus rather than a freshly-invented one.
#
# Usage:
#   Rscript tools/octet-acceptance-sweep.R <pkg-dir> <out.tsv>
#
# Compare two runs with plain `diff`. The three questions it answers:
#
#   locale invariance   run under LC_ALL=C and a UTF-8 locale, diff the two
#                       outputs -- must be 0 differing rows
#   behaviour delta     run at a baseline worktree and at HEAD, diff -- every
#                       differing row must be explainable
#   acceptance drift    count rows whose status is neither `error` nor NA, per
#                       profile; a RISE is a widening and needs justifying
#
# TWO TRAPS this harness exists to avoid, both of which produced a WRONG answer
# on a first attempt during RURL-kmpnbvdl:
#
#   1. Never round-trip the corpus through `.rds`. Under `LC_ALL=C`,
#      `readRDS()` re-encodes "unknown"-marked non-ASCII strings to UTF-8 on
#      load, so the two locale runs are silently fed DIFFERENT inputs. The
#      corpus is therefore built from raw octets in-process, and the output is
#      plain hex text.
#   2. Never wrap the whole vectorized call in one `tryCatch`. One throwing row
#      collapses the entire column into a single "THROW" and hides every other
#      row's real value -- which is precisely the defect class being measured.
#      `each()` below falls back to one call per row.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("usage: octet-acceptance-sweep.R <pkg-dir> <out.tsv>", call. = FALSE)
}
pkg <- args[[1L]]
out_path <- args[[2L]]
suppressMessages(devtools::load_all(pkg, quiet = TRUE))

bstr <- function(bytes) rawToChar(as.raw(bytes))
asc <- function(s) as.integer(charToRaw(s))

# Every octet 1-255 (0x00 cannot live in an R string) dropped into each
# structural position of an otherwise-valid URL.
positions <- list(
  host      = function(o) c(asc("http://a"), o, asc("b.com/p")),
  host_only = function(o) c(asc("http://"), o, asc("/p")),
  hostport  = function(o) c(asc("http://a"), o, asc("b.com:8080/p")),
  user      = function(o) c(asc("http://u"), o, asc("x@h.com/p")),
  userpass  = function(o) c(asc("http://u:p"), o, asc("w@h.com/p")),
  path      = function(o) c(asc("http://h.com/a"), o, asc("b")),
  query     = function(o) c(asc("http://h.com/p?a"), o, asc("=1")),
  fragment  = function(o) c(asc("http://h.com/p#f"), o, asc("g")),
  port      = function(o) c(asc("http://h.com:8"), o, asc("0/p")),
  ipv6      = function(o) c(asc("http://[::1"), o, asc("]/p")),
  ftp_user  = function(o) c(asc("ftp://u"), o, asc("@h.com/p")),
  at_excess = function(o) c(asc("http://a@@"), o, asc("b/p"))
)

corpus <- character(0)
labels <- character(0)
for (pn in names(positions)) {
  f <- positions[[pn]]
  for (o in 1:255) {
    corpus <- c(corpus, bstr(f(o)))
    labels <- c(labels, sprintf("%s:%03d", pn, o))
  }
}

# Multi-octet UTF-8 classes, valid and not. The invalid ones are the
# RURL-kmpnbvdl trigger: a declared-UTF-8 string holding octets that decode to
# no scalar value.
seqs <- list(
  "lone-80"      = 0x80,
  "lone-C3"      = 0xC3,
  "trunc-E4BD"   = c(0xE4, 0xBD),
  "valid-C3A9"   = c(0xC3, 0xA9),
  "valid-E4B8AD" = c(0xE4, 0xB8, 0xAD),
  "overlong"     = c(0xC0, 0xAF),
  "surrogate"    = c(0xED, 0xA0, 0x80),
  "ff"           = 0xFF
)
shapes <- list(
  host      = function(s) c(asc("http://"), s, asc("/p")),
  host_mid  = function(s) c(asc("http://a"), s, asc("b.com/p")),
  hostport  = function(s) c(asc("http://"), s, asc(":80/p")),
  user      = function(s) c(asc("http://u"), s, asc("@h.com/p")),
  userpass  = function(s) {
    c(asc("ftp://"), s, asc(":"), s, asc("@example.com/p"))
  },
  ipv6_user = function(s) c(asc("http://"), s, asc("@[::0.1.0.2]/p")),
  at_excess = function(s) c(asc("http://a@@"), s, asc("b/p")),
  frag      = function(s) c(asc("http://h.com/p#"), s),
  query     = function(s) c(asc("http://h.com/p?q="), s)
)
for (sn in names(seqs)) {
  for (shn in names(shapes)) {
    corpus <- c(corpus, bstr(shapes[[shn]](seqs[[sn]])))
    labels <- c(labels, sprintf("seq:%s:%s", sn, shn))
  }
}

# CONJUNCTIONS -- the blind spot that let a PARTIAL RURL-kmpnbvdl fix score as
# complete. Both blocks above vary ONE thing at a time, and every mask past the
# authority split is a conjunction: reaching it needs the bad octet AND a
# second character that makes the row eligible. So the sweep reported "0
# throwing rows" truthfully over a corpus that could not reach the three sites
# still throwing (the shim's reassembly, `.pct_hex_upper()`, the excess-"@"
# repair). This block pairs them explicitly.
#
# `paren` looks redundant next to `bad-pair` and is not: `C3 28` is an invalid
# sequence whose SECOND octet is "(", a sub-delim -- it is its own conjunction,
# which is why a lone <80> never threw but `http://<C3>(/p` did. Keeping both
# spellings distinguishes "the pair matters" from "the octet matters".
triggers <- list(
  none   = integer(0),
  bang   = asc("!"),
  paren  = asc("("),
  comma  = asc(","),
  semi   = asc(";"),
  pct_lc = asc("%7f"),
  pct_uc = asc("%7F"),
  pct_c0 = asc("%01"),
  pct_az = asc("%41"),
  pct_no = asc("%zz"),
  upper  = asc("AB"),
  xn     = asc("xn--a"),
  at     = asc("@")
)
bad_seqs <- list(
  "lone-80"  = 0x80,
  "bad-pair" = c(0xC3, 0x28),
  "trunc-E2" = c(0xE2, 0x82),
  "ff"       = 0xFF
)
conj <- list(
  host     = function(s, t) c(asc("http://"), s, t, asc("/p")),
  host_rev = function(s, t) c(asc("http://"), t, s, asc("/p")),
  host_dot = function(s, t) c(asc("http://"), s, t, asc(".com/p")),
  host_prt = function(s, t) c(asc("http://"), s, t, asc(":80/p")),
  userinfo = function(s, t) c(asc("http://"), s, t, asc("@e.com/p")),
  ui_at2   = function(s, t) c(asc("http://"), s, asc("@"), t, asc("@e.com/p"))
)
for (bn in names(bad_seqs)) {
  for (tn in names(triggers)) {
    for (cn in names(conj)) {
      corpus <- c(corpus, bstr(conj[[cn]](bad_seqs[[bn]], triggers[[tn]])))
      labels <- c(labels, sprintf("conj:%s:%s:%s", bn, tn, cn))
    }
  }
}

# PERCENT-TRIPLET HOSTS (RURL-rgjpcbuk / RURL-ezhzpkhg deletion 2). The two
# blocks above put RAW octets in the host; neither can reach the ENCODED
# spelling, which is decided by a different rule -- the host's percent-DECODE
# ORDER. `%60` and a literal "`" are the same host code point written two ways,
# and until this block existed the sweep could score "0 differing rows" over a
# corpus that never varied one of them.
#
# `%HH` for every HH, in the shapes whose gates historically differed:
#
#   pct_host    the plain authority -- the shape the pre-parse shim covered
#   pct_3slash  THREE slashes, which that shim's `^scheme://` regex could not
#               match, so the parser judged it alone (the deletion-3 shape)
#   pct_ftps    `ftps`, a rurl scheme that is not WHATWG-special, which the
#               shim's `whatwg` eligibility set therefore excluded
#   pct_port    a triplet followed by a real ":port" delimiter
#   pct_ui      a triplet in USERINFO, which is never percent-decoded and must
#               not move when the host rule changes
#
# and two conjunctions, because admitting a triplet is exactly the kind of
# conjunction-guarded behaviour a one-at-a-time corpus cannot falsify: a
# masked host bypasses the rejection of everything ELSE in it, so the second
# triplet / the literal gap char is what makes the row reachable.
pct_shapes <- list(
  pct_host   = function(t) c(asc("http://a"), t, asc("b.com/p")),
  pct_3slash = function(t) c(asc("http:///a"), t, asc("b.com/p")),
  pct_ftps   = function(t) c(asc("ftps://a"), t, asc("b.com/p")),
  pct_port   = function(t) c(asc("http://a"), t, asc("b.com:8080/p")),
  pct_ui     = function(t) c(asc("http://u"), t, asc("v@h.com/p")),
  pct_gap_x  = function(t) c(asc("http://a%60b"), t, asc("c.com/p")),
  pct_lit_x  = function(t) c(asc("http://a!b"), t, asc("c.com/p"))
)
for (sn in names(pct_shapes)) {
  f <- pct_shapes[[sn]]
  for (o in 0:255) {
    corpus <- c(corpus, bstr(f(asc(sprintf("%%%02X", o)))))
    labels <- c(labels, sprintf("%s:%03d", sn, o))
  }
}

# MULTI-TRIPLET SEQUENCES. The block above varies ONE triplet, which cannot
# express a valid non-ASCII code point at all -- `%C3` alone is invalid UTF-8
# and rejects for that reason, so a corpus of single triplets says nothing
# about `%C3%A9`. That is the same conjunction blind spot as the raw-octet
# block, one level up: here the SECOND triplet is what makes the first one
# legal. Percent-encoded IDN hosts are the common real-world shape, and they
# live only in this block.
pct_seqs <- list(
  "e-acute"   = "%C3%A9",
  "cjk"       = "%E4%B8%AD",
  "astral"    = "%F0%9F%98%80",
  "lone-c3"   = "%C3",
  "overlong"  = "%C0%AF",
  "surrogate" = "%ED%A0%80",
  "shy"       = "%C2%AD",
  "replace"   = "%EF%BF%BD",
  "mixed"     = "%C3%A9%60",
  "pct-pct"   = "%25%36%30"
)
pct_seq_shapes <- list(
  sq_host   = function(s) paste0("http://a", s, "b.com/p"),
  sq_only   = function(s) paste0("http://", s, "/p"),
  sq_3slash = function(s) paste0("http:///a", s, "b.com/p"),
  sq_port   = function(s) paste0("http://a", s, "b.com:8080/p"),
  sq_label  = function(s) paste0("http://", s, ".com/p")
)
for (qn in names(pct_seqs)) {
  for (shn in names(pct_seq_shapes)) {
    corpus <- c(corpus, pct_seq_shapes[[shn]](pct_seqs[[qn]]))
    labels <- c(labels, sprintf("pctseq:%s:%s", qn, shn))
  }
}

# Malformed and case-varied triplets: a "%" that is not followed by two hex
# digits is a host PARSE ERROR (not a literal "%"), and the hex case must not
# decide acceptance.
pct_odd <- c(
  "http://a%6b.com/p", "http://a%b.com/p", "http://a%zzb.com/p",
  "http://a%2b.com/p", "http://a%2B.com/p", "http://a%60b.com/p",
  "http://a%60b.com/p", "http://a%6-b.com/p", "http://a%%60b.com/p",
  "http://a%25%36%30b.com/p", "http://a%.com/p", "http://%60/p"
)
corpus <- c(corpus, pct_odd)
labels <- c(labels, sprintf("pct_odd:%02d", seq_along(pct_odd)))

# LITERAL HOST BYTE x STRUCTURE (RURL-ezhzpkhg deletion 1, ADR 0013). Every
# block above varies the host OCTET inside a FIXED frame -- `http://` + two
# slashes + a plain remainder -- so none of them can see a rule whose gate is
# the structure AROUND the host. That was exactly the shim's shape: its
# eligibility was a regex over the whole URL, and which literal host bytes were
# admitted depended on things the host does not contain.
#
# The three frames that mattered, each of which used to REJECT a host the
# profile plainly admits, and none of which the corpus could reach:
#
#   slashes     the pattern hard-required a literal "//", so a 1- or 3-slash
#               authority never qualified (the encoded half already has
#               `pct_3slash`; the literal half had nothing)
#   terminator  the post-authority remainder was matched with an ICU ".", which
#               excludes the Unicode line terminators -- so a raw VT, FF, NEL,
#               LS or PS ANYWHERE after the authority made the row ineligible
#               and took the host down with it. This is the third instrument
#               this trap has evaded; ICU "." is not "[^\n]".
#   scheme      eligibility was scoped to a scheme SET, so `ftps` -- not a
#               WHATWG special scheme -- was excluded under `whatwg` while
#               `rfc3986` allowed it
#
# `lit_ui` and `lit_port` are the controls: userinfo and port are governed by
# other sets entirely and must not move when the host rule changes.
lit_bytes <- c(
  # the 15 WHATWG keeps in a host
  0x21L, 0x22L, 0x24L, 0x26L, 0x27L, 0x28L, 0x29L, 0x2AL, 0x2BL, 0x2CL,
  0x3BL, 0x3DL, 0x60L, 0x7BL, 0x7DL,
  # negative controls: forbidden host code points, and one ordinary byte
  0x3CL, 0x3EL, 0x5EL, 0x2FL, 0x25L, 0x7CL, 0x61L
)
lit_frames <- list(
  lit_1slash = function(o) c(asc("http:/a"), o, asc("b.com/p")),
  lit_2slash = function(o) c(asc("http://a"), o, asc("b.com/p")),
  lit_3slash = function(o) c(asc("http:///a"), o, asc("b.com/p")),
  lit_ftps   = function(o) c(asc("ftps://a"), o, asc("b.com/p")),
  lit_ftp    = function(o) c(asc("ftp://a"), o, asc("b.com/p")),
  lit_vt     = function(o) c(asc("http://a"), o, asc("b.com/p"), 0x0BL, asc("q")),
  lit_ff     = function(o) c(asc("http://a"), o, asc("b.com/p"), 0x0CL, asc("q")),
  lit_nel    = function(o) {
    c(asc("http://a"), o, asc("b.com/p"), 0xC2L, 0x85L, asc("q"))
  },
  lit_ls     = function(o) {
    c(asc("http://a"), o, asc("b.com/p"), 0xE2L, 0x80L, 0xA8L, asc("q"))
  },
  lit_ps     = function(o) {
    c(asc("http://a"), o, asc("b.com/p"), 0xE2L, 0x80L, 0xA9L, asc("q"))
  },
  lit_ui     = function(o) c(asc("http://u"), o, asc("v@h.com/p")),
  lit_port   = function(o) c(asc("http://a"), o, asc("b.com:8080/p"))
)
for (fn in names(lit_frames)) {
  f <- lit_frames[[fn]]
  for (o in lit_bytes) {
    corpus <- c(corpus, bstr(f(o)))
    labels <- c(labels, sprintf("%s:%03d", fn, o))
  }
}

# Bytes AND the encoding mark: a transcoding regression changes neither value
# nor length, only the mark, and would otherwise pass unnoticed.
hex <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) {
      return("NA")
    }
    octets <- paste(sprintf("%02x", as.integer(charToRaw(s))), collapse = "")
    sprintf("%s/%s", octets, Encoding(s))
  }, character(1), USE.NAMES = FALSE)
}

# See trap 2 in the header.
each <- function(f, x, ...) {
  whole <- tryCatch(suppressWarnings(f(x, ...)), error = function(e) NULL)
  if (!is.null(whole) && length(whole) == length(x)) {
    return(hex(whole))
  }
  vapply(x, function(one) {
    tryCatch(hex(suppressWarnings(f(one, ...))),
             error = function(e) paste0("THROW:", conditionMessage(e)))
  }, character(1), USE.NAMES = FALSE)
}

profiles <- list(
  whatwg = "whatwg", rfc3986 = "rfc3986", default = NULL
)

lines <- character(0)
for (pn in names(profiles)) {
  us <- profiles[[pn]]
  lines <- c(lines, sprintf(
    "%s\t%s\t%s\t%s\t%s", pn, labels,
    each(get_parse_status, corpus, url_standard = us),
    each(get_host, corpus, url_standard = us),
    each(serialize_url, corpus, standard = us)
  ))
}
writeLines(lines, out_path)

error_hex <- paste(sprintf("%02x", asc("error")), collapse = "")
accepted <- function(prof) {
  sel <- lines[startsWith(lines, paste0(prof, "\t"))]
  st <- vapply(strsplit(sel, "\t", fixed = TRUE), `[`, character(1), 3L)
  sum(st != "NA" & !startsWith(st, "THROW") &
        !startsWith(st, sprintf("%s/", error_hex)))
}
cat(sprintf("corpus=%d rows=%d locale=%s -> %s\n",
            length(corpus), length(lines), Sys.getlocale("LC_CTYPE"), out_path))
for (pn in names(profiles)) {
  cat(sprintf("  %-8s accepted=%d\n", pn, accepted(pn)))
}
cat(sprintf("  THROW rows: %d\n", sum(grepl("THROW", lines, fixed = TRUE))))
