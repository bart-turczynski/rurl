#!/usr/bin/env Rscript
#
# Path / query / fragment component sweep.
#
# The third instrument, alongside tools/octet-acceptance-sweep.R (a HOST-seam
# corpus) and tools/authority-at-sweep.R (an AUTHORITY corpus). Neither of those
# varies the POST-authority components systematically: the octet sweep carries
# exactly one `path`, one `query` and one `fragment` shape, each a single octet
# dropped into a fixed skeleton, and the at-sweep varies "@" placement. So a
# change at the path/query/fragment seam could score "0 differing rows"
# truthfully over a corpus that never reached it.
#
# WHY IT IS CHECKED IN. Written for RURL-ezhzpkhg deletion 5, which removes
# `.sanitize_whatwg_pqf_vec()` -- a pre-parse rewrite that respelled the
# post-authority components with the WHATWG percent-encode sets and re-parsed
# after the first attempt failed. Two properties of that compensation are
# invisible to a one-component-at-a-time corpus and are what this file exists
# to expose:
#
#   1. IT IS A CONJUNCTION. The rewrite fires only when the FIRST parse FAILED,
#      and it then respells ALL THREE components at once. So a byte that is
#      perfectly acceptable on its own changes spelling because a DIFFERENT
#      component, possibly a different one entirely, held a rejected byte. A
#      corpus that varies one component at a time cannot falsify that: it can
#      never build the pair (offending byte in X) AND (encode-set byte in Y).
#      Block 4 pairs them explicitly.
#   2. IT IS A SPELLING AXIS, NOT AN ACCEPTANCE ONE. `%7F` and a raw DEL are the
#      same code point written two ways and are decided by different rules --
#      the same lesson deletion 2 paid for twice at the host seam. Blocks 2 and
#      5 carry both spellings of every octet.
#
# The surfaces matter as much as the corpus. `path_encoding` (ADR 0011) is the
# orthogonal presentation axis on which a respelling becomes user-visible, and
# the deleted compensation's own comment claimed it preserved
# `path_encoding = "keep"`. So "keep"/"encode"/"decode" are all pulled, not just
# the default.
#
# Same two traps as its two companions: no .rds round-trip (the corpus is built
# from raw octets in-process and the output is hex text), and no whole-vector
# tryCatch (one throwing row must not collapse the column). Compare two runs
# with plain `diff`; run under LC_ALL=C and a UTF-8 locale for the
# locale-invariance question, and at a baseline worktree vs HEAD for the
# behaviour-delta one. A RISE in an accepted count is a widening and needs
# justifying.
#
# Usage: Rscript tools/pqf-component-sweep.R <pkg-dir> <out.tsv>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("usage: pqf-component-sweep.R <pkg-dir> <out.tsv>", call. = FALSE)
}
pkg <- args[[1L]]
out_path <- args[[2L]]
suppressMessages(devtools::load_all(pkg, quiet = TRUE))

bstr <- function(bytes) rawToChar(as.raw(bytes))
asc <- function(s) as.integer(charToRaw(s))

corpus <- character(0)
labels <- character(0)
add <- function(label, bytes) {
  corpus <<- c(corpus, bstr(bytes))
  labels <<- c(labels, label)
}

# ---- 1. Every octet, in every post-authority position -----------------------
#
# The parser refuses C0/SP/DEL outside the authority and percent-encodes >=0x80;
# WHATWG percent-encodes a wider set still, per component, and the sets DIFFER
# between path, query and fragment (the query set alone varies by scheme). So
# the same octet has three different correct answers depending on where it sits,
# and position within the component matters too -- an octet in the first segment
# is reached by different code than one in the last, and dot-segment removal
# runs after normalization.
pos <- list(
  path_mid   = function(o) c(asc("http://h.com/a"), o, asc("b")),
  path_head  = function(o) c(asc("http://h.com/"), o, asc("ab")),
  path_tail  = function(o) c(asc("http://h.com/ab"), o),
  path_seg2  = function(o) c(asc("http://h.com/a/"), o, asc("/b")),
  path_only  = function(o) c(asc("http://h.com/"), o),
  query_mid  = function(o) c(asc("http://h.com/p?a"), o, asc("=1")),
  query_val  = function(o) c(asc("http://h.com/p?a=1"), o, asc("2")),
  query_amp  = function(o) c(asc("http://h.com/p?a=1&b"), o, asc("=2")),
  query_only = function(o) c(asc("http://h.com/p?"), o),
  frag_mid   = function(o) c(asc("http://h.com/p#f"), o, asc("g")),
  frag_only  = function(o) c(asc("http://h.com/p#"), o),
  frag_hash  = function(o) c(asc("http://h.com/p#f#"), o),
  qf_both    = function(o) c(asc("http://h.com/p?q="), o, asc("#f"))
)
for (pn in names(pos)) {
  f <- pos[[pn]]
  for (o in 1:255) {
    add(sprintf("oct:%s:%03d", pn, o), f(o))
  }
}

# ---- 2. The same octets, ENCODED ------------------------------------------
#
# `%7F` and a raw DEL are one code point written two ways, decided by different
# rules: the raw one meets the parser's forbidden-byte check, the triplet meets
# the "%XX" uppercase pass and never decodes at all. Block 1 cannot see the
# second spelling. (This is the gap that cost deletion 2 two extra commits at
# the host seam.)
pct_pos <- list(
  path  = function(t) c(asc("http://h.com/a"), t, asc("b")),
  query = function(t) c(asc("http://h.com/p?a"), t, asc("=1")),
  frag  = function(t) c(asc("http://h.com/p#f"), t, asc("g"))
)
for (pn in names(pct_pos)) {
  f <- pct_pos[[pn]]
  for (o in 0:255) {
    add(sprintf("pct:%s:%03d", pn, o), f(asc(sprintf("%%%02X", o))))
  }
}

# ---- 3. The WHATWG percent-encode sets, member by member --------------------
#
# The bytes that separate the parser's set (C0/SP/DEL) from WHATWG's per
# component. These are the rows on which "reject" and "percent-encode" give
# different answers, and the rows whose SPELLING the deleted rewrite changed
# wholesale. Carried in every component and in every scheme class, because the
# query set is scheme-sensitive (special schemes add "'") and the eligibility
# gate of the deleted rewrite was scheme-shaped.
set_bytes <- list(
  sp = 0x20L, dquote = 0x22L, hash = 0x23L, pct = 0x25L, squote = 0x27L,
  lt = 0x3CL, gt = 0x3EL, qmark = 0x3FL, backtick = 0x60L, lbrace = 0x7BL,
  rbrace = 0x7DL, pipe = 0x7CL, caret = 0x5EL, bslash = 0x5CL,
  lbracket = 0x5BL, rbracket = 0x5DL, del = 0x7FL, tab = 0x09L, lf = 0x0AL,
  cr = 0x0DL, bel = 0x07L, esc = 0x1BL, us = 0x1FL
)
set_schemes <- c("http://", "https://", "ftp://", "ftps://", "ws://",
                 "wss://", "http:///")
set_shapes <- list(
  in_path  = function(s, o) c(asc(s), asc("h.com/a"), o, asc("b")),
  in_query = function(s, o) c(asc(s), asc("h.com/p?k="), o, asc("v")),
  in_frag  = function(s, o) c(asc(s), asc("h.com/p#f"), o, asc("g"))
)
for (bn in names(set_bytes)) {
  for (s in set_schemes) {
    for (shn in names(set_shapes)) {
      add(sprintf("set:%s:%s:%s", bn, s, shn),
          set_shapes[[shn]](s, set_bytes[[bn]]))
    }
  }
}

# ---- 4. CONJUNCTIONS across components -- the blind spot ---------------------
#
# THE block this file exists for. The deleted rewrite was gated on the first
# parse having FAILED and then respelled path AND query AND fragment together.
# So the observable is: put a REJECTED byte in one component and a merely
# ENCODE-SET byte in another, and the second component's spelling changes
# because of the first. No amount of one-component-at-a-time corpus reaches
# that; blocks 1-3 above would score it 0 while every row moved.
#
# `trigger` is the byte that decides whether the first parse fails at all;
# `witness` is the byte whose spelling is then observed. `witness = none` is the
# control -- it isolates acceptance from respelling.
triggers <- list(
  none = integer(0), sp = 0x20L, del = 0x7FL, c0 = 0x01L, lf = 0x0AL,
  high = 0xC3L, bad_pct = asc("%zz")
)
witnesses <- list(
  none = integer(0), lt = 0x3CL, backtick = 0x60L, dquote = 0x22L,
  squote = 0x27L, lbrace = 0x7BL, pct_uc = asc("%2F"), pct_lc = asc("%2f"),
  dots = asc("/../"), high = c(0xC3L, 0xA9L)
)
# Which component carries the trigger, and which carries the witness.
conj <- list(
  t_path_w_query = function(t, w) {
    c(asc("http://h.com/a"), t, asc("b?k="), w, asc("v"))
  },
  t_path_w_frag = function(t, w) {
    c(asc("http://h.com/a"), t, asc("b#f"), w, asc("g"))
  },
  t_query_w_path = function(t, w) {
    c(asc("http://h.com/a"), w, asc("b?k="), t, asc("v"))
  },
  t_frag_w_path = function(t, w) {
    c(asc("http://h.com/a"), w, asc("b#f"), t, asc("g"))
  },
  t_frag_w_query = function(t, w) {
    c(asc("http://h.com/p?k="), w, asc("v#f"), t, asc("g"))
  },
  t_path_w_path = function(t, w) {
    c(asc("http://h.com/a"), t, asc("b/c"), w, asc("d"))
  },
  # The trigger sits in the HOST, so the first parse fails for a reason the
  # rewrite cannot repair. The witness must NOT be respelled -- the row is an
  # error either way, and a rescue here would be a widening.
  t_host_w_path = function(t, w) {
    c(asc("http://h"), t, asc("x.com/a"), w, asc("b"))
  },
  # The trigger sits in the USERINFO, same argument, different gate.
  t_user_w_query = function(t, w) {
    c(asc("http://u"), t, asc("v@h.com/p?k="), w, asc("x"))
  }
)
for (tn in names(triggers)) {
  for (wn in names(witnesses)) {
    for (cn in names(conj)) {
      add(sprintf("conj:%s:%s:%s", tn, wn, cn),
          conj[[cn]](triggers[[tn]], witnesses[[wn]]))
    }
  }
}

# ---- 5. Multi-octet sequences, raw and encoded ------------------------------
#
# One triplet cannot spell a non-ASCII code point (`%C3` alone is invalid
# UTF-8), and one raw octet cannot either. Both spellings of the same scalar
# value belong in the corpus, valid and not: the parser percent-encodes >=0x80
# in these components without validating UTF-8, so the invalid ones are where a
# transcoding regression would surface.
seqs <- list(
  "e-acute"   = list(raw = c(0xC3L, 0xA9L), pct = "%C3%A9"),
  "cjk"       = list(raw = c(0xE4L, 0xB8L, 0xADL), pct = "%E4%B8%AD"),
  "astral"    = list(raw = c(0xF0L, 0x9FL, 0x98L, 0x80L), pct = "%F0%9F%98%80"),
  "lone-c3"   = list(raw = 0xC3L, pct = "%C3"),
  "bad-pair"  = list(raw = c(0xC3L, 0x28L), pct = "%C3%28"),
  "overlong"  = list(raw = c(0xC0L, 0xAFL), pct = "%C0%AF"),
  "surrogate" = list(raw = c(0xEDL, 0xA0L, 0x80L), pct = "%ED%A0%80"),
  "ff"        = list(raw = 0xFFL, pct = "%FF"),
  "pct-pct"   = list(raw = asc("%25%37%46"), pct = "%2525"),
  "nul-ish"   = list(raw = asc("%00"), pct = "%00")
)
seq_shapes <- list(
  s_path  = function(s) c(asc("http://h.com/a"), s, asc("b")),
  s_query = function(s) c(asc("http://h.com/p?k="), s),
  s_frag  = function(s) c(asc("http://h.com/p#"), s),
  s_all   = function(s) {
    c(asc("http://h.com/a"), s, asc("?k="), s, asc("#f"), s)
  }
)
for (qn in names(seqs)) {
  for (shn in names(seq_shapes)) {
    add(sprintf("seqraw:%s:%s", qn, shn), seq_shapes[[shn]](seqs[[qn]]$raw))
    add(sprintf("seqpct:%s:%s", qn, shn),
        seq_shapes[[shn]](asc(seqs[[qn]]$pct)))
  }
}

# ---- 6. Structural shapes ---------------------------------------------------
#
# The deleted rewrite matched `^scheme://[^/?#]*` and split the remainder by
# hand. Every shape that regex could mis-slice, or never match, belongs here:
# the three-slash authority (the blind spot that cost deletion 3 a 165-row
# widening and deletion 2 another 15 rows), an empty authority, no path at all,
# a "?"/"#" as the very first byte, repeated delimiters, and dot segments --
# which the parser resolves AFTER normalization, so a respelling upstream of it
# can change which segments are even seen.
structural <- c(
  "http://h.com", "http://h.com/", "http://h.com?", "http://h.com#",
  "http://h.com?#", "http://h.com#?", "http://h.com/?#",
  "http://h.com/p?", "http://h.com/p#", "http://h.com/p?#f",
  "http://h.com/p?q#", "http://h.com/p??q", "http://h.com/p##f",
  "http://h.com/p?a=1?b=2", "http://h.com/p#a#b",
  "http:///h.com/p", "http:///h.com/p?q=1#f", "http:///h.com/a b",
  "http://h.com/a/../b", "http://h.com/a/%2E%2E/b", "http://h.com/a/.%2E/b",
  "http://h.com/a/../b?q=../x#../y", "http://h.com/../..",
  "http://h.com/a b/../c", "http://h.com/a\tb/../c",
  "http://h.com/%2e%2e/a b", "http://h.com/./a b/.",
  "ftps://h.com/a b?q=1#f", "ws://h.com/a b?q=1#f", "wss://h.com/a b?q=1#f",
  "ftp://h.com/a b?q=1#f", "custom://h.com/a b?q=1#f",
  "mailto:u@h.com?subject=a b", "http://h.com/p?q=a b#f g",
  "http://u:p@h.com/a b?q=1#f", "http://h.com:8080/a b?q=1#f",
  "http://[::1]/a b?q=1#f", "http://127.0.0.1/a b?q=1#f",
  "http://h.com/a%20b", "http://h.com/a+b", "http://h.com/a%2Bb",
  "http://h.com/p?q=a%20b", "http://h.com/p#f%20g",
  "http://h.com/a%b", "http://h.com/p?q=%zz", "http://h.com/p#%",
  "http://h.com/%", "http://h.com/p?%", "http://h.com/a%%20b"
)
for (i in seq_along(structural)) {
  add(sprintf("struct:%02d", i), asc(structural[[i]]))
}

# ---- Output -----------------------------------------------------------------

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

profiles <- list(whatwg = "whatwg", rfc3986 = "rfc3986", default = NULL)
cols <- c("path", "query", "fragment", "clean_url", "parse_status")

# One safe_parse_urls call PER ROW (trap 2), all columns pulled at once.
frame_row <- function(one, us) {
  tryCatch({
    d <- suppressWarnings(safe_parse_urls(one, url_standard = us))
    if (is.null(d)) {
      rep("NA", length(cols))
    } else {
      vapply(cols, function(cl) {
        if (!cl %in% names(d)) "NA" else hex(d[[cl]])[1L]
      }, character(1), USE.NAMES = FALSE)
    }
  }, error = function(e) {
    rep(paste0("THROW:", conditionMessage(e)), length(cols))
  })
}

lines <- character(0)
for (pn in names(profiles)) {
  us <- profiles[[pn]]
  frame <- vapply(corpus, frame_row, character(length(cols)), us = us,
                  USE.NAMES = FALSE)
  lines <- c(lines, paste(
    pn, labels,
    each(get_parse_status, corpus, url_standard = us),
    each(serialize_url, corpus, standard = us),
    each(serialize_url, corpus, standard = us, form = "normalized"),
    # `path_encoding` is the orthogonal presentation axis (ADR 0011) on which a
    # respelling of the stored path becomes user-visible. The deleted
    # compensation claimed in its own comment to preserve "keep"; that claim is
    # only checkable if all three settings are pulled.
    each(get_path, corpus, url_standard = us, path_encoding = "keep"),
    each(get_path, corpus, url_standard = us, path_encoding = "encode"),
    each(get_path, corpus, url_standard = us, path_encoding = "decode"),
    apply(frame, 2L, paste, collapse = "\t"),
    sep = "\t"
  ))
}
writeLines(lines, out_path)

error_hex <- paste(sprintf("%02x", asc("error")), collapse = "")
accepted <- function(prof) {
  sel <- lines[startsWith(lines, paste0(prof, "\t"))]
  st <- vapply(strsplit(sel, "\t", fixed = TRUE), `[`, character(1), 3L)
  sum(!startsWith(st, "NA") & !startsWith(st, "THROW") &
        !startsWith(st, sprintf("%s/", error_hex)))
}
cat(sprintf("corpus=%d rows=%d locale=%s -> %s\n",
            length(corpus), length(lines), Sys.getlocale("LC_CTYPE"), out_path))
for (pn in names(profiles)) {
  cat(sprintf("  %-8s accepted=%d\n", pn, accepted(pn)))
}
cat(sprintf("  THROW rows: %d\n", sum(grepl("THROW", lines, fixed = TRUE))))
