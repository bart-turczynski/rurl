# RURL-cwzickqy: the rows and the canonical form behind
# test-input-boundary.R. Every name here starts with `ib_`: helpers share one
# environment with every test file.
#
# Every non-ASCII row is built from raw octets with rawToChar(), never from a
# source literal, and then given the mark its name says.

ib_str <- function(..., mark = "unknown") {
  parts <- list(...)
  octets <- unlist(lapply(parts, function(p) {
    if (is.character(p)) charToRaw(p) else as.raw(p)
  }))
  out <- rawToChar(octets)
  if (mark != "unknown") {
    Encoding(out) <- mark
  }
  out
}

# One row per input representation a caller can hand rurl: plain ASCII, UTF-8
# marked, the same UTF-8 bytes unmarked ("native"), latin1, bytes, invalid
# UTF-8 unmarked and invalid UTF-8 marked UTF-8, plus NA and "".
ib_parse_rows <- function() {
  c(
    ascii = "http://u@h.example.com/p?q=v#f",
    utf8 = ib_str("http://h.example.com/p", 0xc3, 0xbc, "?q=", 0xc3, 0xbc,
                  "#", 0xc3, 0xbc, mark = "UTF-8"),
    utf8_host = ib_str("http://b", 0xc3, 0xbc, "cher.example.com/",
                       mark = "UTF-8"),
    native = ib_str("http://h.example.com/p", 0xc3, 0xbc, "?q=", 0xc3, 0xbc,
                    "#", 0xc3, 0xbc),
    native_host = ib_str("http://b", 0xc3, 0xbc, "cher.example.com/"),
    latin1 = ib_str("http://h.example.com/p", 0xfc, "?q=", 0xfc, "#", 0xfc,
                    mark = "latin1"),
    latin1_host = ib_str("http://b", 0xfc, "cher.example.com/",
                         mark = "latin1"),
    bytes = ib_str("http://h.example.com/p", 0xfc, mark = "bytes"),
    bytes_host = ib_str("http://b", 0xfc, "cher.example.com/",
                        mark = "bytes"),
    invalid = ib_str("http://h.example.com/caf", 0xe9),
    invalid_query = ib_str("http://h.example.com/p?q=", 0xe9),
    invalid_host = ib_str("http://h", 0xe9, ".example.com/"),
    invalid_marked = ib_str("http://h.example.com/p", 0x80, mark = "UTF-8"),
    mailto_latin1 = ib_str("mailto:a", 0xfc, "@b.example.com",
                           mark = "latin1"),
    na = NA_character_,
    empty = ""
  )
}

# The parse postures the seam pins cross: the frozen default and both named
# standards, each named one under both acceptances.
ib_postures <- list(
  null_web = list(url_standard = NULL),
  whatwg_web = list(url_standard = "whatwg"),
  rfc3986_web = list(url_standard = "rfc3986"),
  whatwg_general = list(url_standard = "whatwg",
                        scheme_acceptance = "general"),
  rfc3986_general = list(url_standard = "rfc3986",
                         scheme_acceptance = "general")
)

# An output as one ASCII string: every character element as its octets in
# hex, so a pin compares bytes and nothing else, whatever the locale prints.
# The encoding mark is not part of it: two spellings of the same bytes that
# differ only by an "unknown" or "UTF-8" mark are the same output.
ib_canon <- function(v) {
  if (is.null(v)) {
    return("NULL")
  }
  if (is.data.frame(v)) {
    return(paste0("df(", ib_canon(as.list(v)), ")"))
  }
  if (is.list(v)) {
    nm <- names(v)
    if (is.null(nm)) {
      nm <- rep("", length(v))
    }
    a <- attributes(v)
    a <- a[setdiff(names(a), c("names", "row.names", "class"))]
    body <- paste(nm, vapply(v, ib_canon, ""), sep = "=", collapse = ",")
    extra <- if (length(a)) ib_canon(a) else ""
    return(paste0("list(", body, ")", extra))
  }
  a <- attributes(v)
  a <- a[setdiff(names(a), "names")]
  extra <- if (length(a)) paste0("@", ib_canon(a)) else ""
  if (is.character(v)) {
    body <- vapply(v, function(s) {
      if (is.na(s)) "NA" else paste(as.character(charToRaw(s)), collapse = "")
    }, "", USE.NAMES = FALSE)
    return(paste0("chr[", paste(body, collapse = "|"), "]", extra))
  }
  paste0(typeof(v), "[", paste(format(unclass(v)), collapse = "|"), "]",
         extra)
}

# The md5 of a canonical string. tools::md5sum() reads a file; its `bytes`
# argument is too recent to rely on.
ib_md5 <- function(s) {
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  con <- file(f, open = "wb")
  writeBin(charToRaw(s), con)
  close(con)
  unname(tools::md5sum(f))
}

# The export pins run each manifest export over these rows: the plain template
# row, its UTF-8 twins at every position (marked UTF-8), and the same twins
# with the mark removed.
ib_export_rows <- function(entry) {
  tpl <- entry$template
  twins <- hi_class_rows(tpl, "utf8_twin", entry$positions)
  native <- vapply(twins, function(s) rawToChar(charToRaw(s)), "")
  names(native) <- paste0("native_", names(twins))
  unname(c(plain = hi_plain(tpl), twins, native))
}

# One manifest export's output over `rows`, through the hostile-input runner
# (helper-hostile-inputs.R), so every operand and join is called the way the
# contracts call it. A scalar export is called once per row.
ib_run_export <- function(fn, entry, std, opts, operand, rows) {
  rn <- hi_runner(fn, entry, std, opts, operand)
  if (rn$scalar) {
    return(lapply(rows, rn$run))
  }
  rn$run(rows)
}

# Every (export, standard, option set, operand) of the capability manifest,
# keyed as the export pins spell it (the option set by its position in the
# manifest entry), with the md5 of its canonical output over the export rows.
ib_export_digests <- function() {
  out <- character(0)
  for (fn in names(capability_manifest)) {
    e <- capability_manifest[[fn]]
    rows <- ib_export_rows(e)
    for (std in e$standards) {
      for (oi in seq_along(e$options)) {
        o <- e$options[[oi]]
        if (!hi_options_apply(o, std)) {
          next
        }
        for (op in e$operands) {
          v <- suppressWarnings(ib_run_export(fn, e, std, o, op, rows))
          key <- paste(fn, std, oi, op)
          out[[key]] <- ib_md5(ib_canon(v))
        }
      }
    }
  }
  out
}
