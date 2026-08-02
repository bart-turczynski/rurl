#!/usr/bin/env Rscript
#
# Authority / userinfo "@" sweep.
#
# The companion to tools/octet-acceptance-sweep.R, which is a HOST-seam
# instrument and carries only two "@" shapes (`at_excess`, `ui_at2`). Anything
# that touches the userinfo/host split needs a corpus that varies the AUTHORITY
# systematically instead, so that is what this builds: "@" count x position x
# userinfo shape x host shape, "@" scattered rather than run together, "@" past
# the authority (which must never move), an octet sweep either side of a
# doubled "@", the conjunction block, and the degenerate authorities.
#
# WHY IT IS CHECKED IN. Written for RURL-ezhzpkhg deletion 3 (moving the
# repeated-"@" recovery out of the pre-parse `.encode_excess_authority_at_vec()`
# repair and into `.parse_web_url_one(last_at_userinfo = TRUE)`), where it paid
# for itself immediately: the obvious port of the old eligibility gate
# (`!is.null(url_standard)`) scored as a 165-row WIDENING of `rfc3986`, because
# the deleted repair's regex required a literal "//" and so had never fired on a
# THREE-slash URL. Nothing else in the suite caught that. The remaining
# compensation-layer deletions all touch this same seam.
#
# Same two traps as tools/octet-acceptance-sweep.R: no .rds round-trip (corpus
# built from raw octets in-process, output is hex text), and no whole-vector
# tryCatch (one throwing row must not collapse the column). Compare two runs
# with plain `diff`; run it under LC_ALL=C and a UTF-8 locale for the
# locale-invariance question, and at a baseline worktree vs HEAD for the
# behaviour-delta one. A RISE in an accepted count is a widening and needs
# justifying.
#
# Usage: Rscript tools/authority-at-sweep.R <pkg-dir> <out.tsv>

args <- commandArgs(trailingOnly = TRUE)
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

# ---- 1. "@" count x position, over several scheme/authority shapes ----------
schemes <- c("http://", "https://", "ftp://", "ws://", "wss://",
             "mailto:", "custom://", "http:///")
auth_tails <- c("example.com", "example.com:8080", "[::1]", "[::1]:80",
                "127.0.0.1", "0x7f.1", "ex%41mple.com", "")
userinfos <- c("", "u", "u:p", "u:", ":p", ":", "%40", "u%40v", "u%4a",
               "u%zz", "a.b", "u:p:q")
at_runs <- c("@", "@@", "@@@", "@@@@")

for (s in schemes) {
  for (ui in userinfos) {
    for (at in at_runs) {
      for (tail in auth_tails) {
        add(sprintf("shape:%s|%s|%s|%s", s, ui, at, tail),
            asc(paste0(s, ui, at, tail, "/p")))
      }
    }
  }
}

# ---- 2. "@" scattered through the userinfo, not just as a run --------------
scatter <- c("a@b@c", "a@b@c@d", "@a@b", "a@b@", "@@a", "a@@b", "a:b@c:d@e",
             "a@b:c@d", ":@:@:", "%40@%40", "a@:@b", "@:@")
for (s in c("http://", "ftp://", "ws://")) {
  for (sc in scatter) {
    for (tail in c("h.com", "h.com:99", "[::1]")) {
      add(sprintf("scatter:%s|%s|%s", s, sc, tail),
          asc(paste0(s, sc, "@", tail, "/p")))
      add(sprintf("scatter-bare:%s|%s|%s", s, sc, tail),
          asc(paste0(s, sc, tail, "/p")))
    }
  }
}

# ---- 3. "@" outside the authority must stay untouched ----------------------
outside <- c("http://h.com/a@b", "http://h.com/a@@b", "http://h.com/?a@@b",
             "http://h.com/#a@@b", "http://h.com?u@@v", "http://h.com#u@@v",
             "http://u@@h.com/a@@b?c@@d#e@@f",
             "http://h.com/p?q=a@@b#f@@g", "http://@h.com/@/@?@#@")
for (o in outside) add(sprintf("outside:%s", o), asc(o))

# ---- 4. CONJUNCTIONS: a second "@" AND something that changes the split ----
# The blind spot recorded in tools/octet-acceptance-sweep.R: this repair sits
# behind an at_count > 1 guard, so every defect in it needs the extra "@" AND a
# second ingredient. Vary the ingredient explicitly.
bad_seqs <- list(none = integer(0), "lone-80" = 0x80, "bad-pair" = c(0xC3, 0x28),
                 "trunc-E2" = c(0xE2, 0x82), ff = 0xFF,
                 "valid-C3A9" = c(0xC3, 0xA9))
ctl <- list(none = integer(0), sp = 0x20L, tab = 0x09L, vt = 0x0BL, ff2 = 0x0CL,
            del = 0x7FL, soh = 0x01L, esc = 0x1BL)
for (bn in names(bad_seqs)) {
  for (cn in names(ctl)) {
    b <- bad_seqs[[bn]]
    c0 <- ctl[[cn]]
    add(sprintf("conj:pre:%s:%s", bn, cn),
        c(asc("http://"), b, c0, asc("@x@h.com/p")))
    add(sprintf("conj:mid:%s:%s", bn, cn),
        c(asc("http://u@"), b, c0, asc("@h.com/p")))
    add(sprintf("conj:post:%s:%s", bn, cn),
        c(asc("http://u@x@"), b, c0, asc("h.com/p")))
    add(sprintf("conj:pass:%s:%s", bn, cn),
        c(asc("http://u:"), b, c0, asc("@v@h.com/p")))
    add(sprintf("conj:run:%s:%s", bn, cn),
        c(asc("http://"), b, asc("@@"), c0, asc("@h.com/p")))
    add(sprintf("conj:ftp:%s:%s", bn, cn),
        c(asc("ftp://u"), c0, b, asc("@@h.com/p")))
  }
}

# ---- 5. every octet 1-255 dropped either side of a doubled "@" -------------
for (o in 1:255) {
  add(sprintf("oct:pre:%03d", o), c(asc("http://a"), o, asc("@@h.com/p")))
  add(sprintf("oct:mid:%03d", o), c(asc("http://a@"), o, asc("@h.com/p")))
  add(sprintf("oct:post:%03d", o), c(asc("http://a@@"), o, asc("h.com/p")))
  add(sprintf("oct:one:%03d", o), c(asc("http://a"), o, asc("@h.com/p")))
}

# ---- 6. degenerate authorities --------------------------------------------
degenerate <- c("http://@", "http://@@", "http://@@@", "http://@/", "http://@@/",
                "http://@?q", "http://@#f", "http://u@@", "http://@@h.com",
                "http:////u@@h.com/p", "http:/u@@h.com", "http:u@@h.com",
                "//u@@h.com/p", "u@@h.com/p", "http://u@@h.com",
                "http://u@@h.com:", "http://u@@h.com:0", "http://u@@:80",
                "http://u@@[::1", "http://u@@[::1]x", "http://u@@h.com/",
                "HTTP://U@@H.COM/P", "http://u%40@h.com/p", "http://u@%40h.com/p")
for (d in degenerate) add(sprintf("degen:%s", d), asc(d))

# `port` comes back integer, so coerce -- but TAG the class, because a type
# change is exactly the kind of silent drift this sweep is meant to catch.
hex <- function(x) {
  cls <- class(x)[1L]
  if (!is.character(x)) x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s)) return(paste0("NA<", cls, ">"))
    sprintf("%s/%s<%s>",
            paste(sprintf("%02x", as.integer(charToRaw(s))), collapse = ""),
            Encoding(s), cls)
  }, character(1), USE.NAMES = FALSE)
}

each <- function(f, x, ...) {
  whole <- tryCatch(suppressWarnings(f(x, ...)), error = function(e) NULL)
  if (!is.null(whole) && length(whole) == length(x)) return(hex(whole))
  vapply(x, function(one) {
    tryCatch(hex(suppressWarnings(f(one, ...))),
             error = function(e) paste0("THROW:", conditionMessage(e)))
  }, character(1), USE.NAMES = FALSE)
}

profiles <- list(whatwg = "whatwg", rfc3986 = "rfc3986", default = NULL)
cols <- c("host", "user", "password", "port", "path", "query", "fragment",
          "clean_url", "parse_status")

# One safe_parse_urls call PER ROW (trap 2: a whole-vector call lets one
# throwing row collapse every other row's value), all columns pulled at once.
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
  }, error = function(e) rep(paste0("THROW:", conditionMessage(e)), length(cols)))
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
