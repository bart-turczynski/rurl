#!/usr/bin/env Rscript

# posture-probe: one URL, every conformance posture, the oracle beside it
# (RURL-cxkwbqhc).
#
# WHY THIS EXISTS. Answering "what does rurl do with THIS string under the
# WHATWG posture, and what does the standard say" used to take four things at
# once: remembering which argument set IS the posture (the `whatwg` profile is
# not `url_standard = "whatwg"` -- it also requires a scheme and errors on
# scheme-relative input), remembering that the conformance claim goes through
# `serialize_url()` and never `get_clean_url()`, opening the WPT fixture to
# find the row, and sourcing the RFC 3986 test helpers by hand. Each step is a
# place to pick the wrong instrument and read a plausible number off it
# (design/measurement-traps.md section 3, "consumer argument sets"). This
# script does all four, prints the argument set it actually passed, and prints
# `oracle: none` rather than an expectation it does not have.
#
# THE FOUR POSTURES, and where each argument set comes from:
#   whatwg      profile = "whatwg"        -- expanded from `.URL_PROFILES` at
#                                            run time, so the printout cannot
#                                            drift from R/parse.R
#   rfc3986     url_standard = "rfc3986", scheme_policy = "require",
#               scheme_acceptance = "general"   (the normalizing RFC posture;
#                                            no profile bundles exactly this)
#   rfc-syntax  profile = "rfc-syntax"    -- expanded from `.URL_PROFILES`
#   null        url_standard = NULL       -- the frozen legacy profile (ADR
#                                            0007 / 0016). NOT a conformance
#                                            posture: it names no standard,
#                                            so nothing is an oracle for it.
#
# SERIALIZATION. The WHATWG and RFC 3986 claims are `serialize_url(x,
# standard = ...)`; `rfc-syntax` parses under RFC 3986 and is shown with the
# same serializer. The null posture gets no serialize line: the null profile
# names no standard, and `serialize_url(standard = NULL)` is an error
# (RURL-ouorolhb) -- so the probe never hands NULL through; `serialize = NA`
# in `posture_args()` skips the call outright.
#
# ORACLES.
#   WHATWG    inst/bench/wpt-url-cases.json (base = null rows; `href` or
#             failure) and, with --base, tests/testthat/fixtures/
#             wpt-url-base-relative.json keyed by the (base, input) PAIR.
#   RFC 3986  the Appendix B decomposition and the ABNF acceptance verdict
#             from tests/testthat/helper-rfc3986-appendix-b.R and
#             helper-rfc3986-abnf.R, sourced from the tree. Appendix B splits
#             the SOURCE and never normalizes, so under the normalizing
#             rfc3986 posture it is the oracle for what was written, not for
#             the normalized output. It also reports absent (NA) and empty ("")
#             separately, which rurl's public frame does not (both are NA).
#   With --base under an RFC posture there is no section 5.2 resolver in the
#   tree, so the split shown is of the REFERENCE, before resolution.
#
# Deterministic and network-free. Needs `pkgload` and `jsonlite` (both in
# Suggests / the dev toolchain); reads only tracked files.
#
# Usage:
#   Rscript tools/posture-probe.R <url> [--posture whatwg|rfc3986|rfc-syntax|null] [--base <base-url>]
#   Rscript tools/posture-probe.R --self-test

# --- locate the tree ---------------------------------------------------------

script_path <- function() {
  f <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(f) == 0L) return(NA_character_)
  normalizePath(sub("^--file=", "", f[[1L]]), mustWork = FALSE)
}

repo_root <- function() {
  sp <- script_path()
  cand <- c(if (!is.na(sp)) dirname(dirname(sp)), getwd())
  for (d in cand) {
    if (file.exists(file.path(d, "DESCRIPTION")) &&
          file.exists(file.path(d, "R", "parse.R"))) {
      return(normalizePath(d))
    }
  }
  stop("cannot find the rurl tree (run from the repository root)", call. = FALSE)
}

need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("posture-probe needs the '%s' package", pkg), call. = FALSE)
  }
}

# --- the postures ------------------------------------------------------------

POSTURES <- c("whatwg", "rfc3986", "rfc-syntax", "null")

# The argument set for a posture, as a list ready for do.call(), plus the
# serializer standard the posture admits (NA for none). Profile postures are
# expanded from the package's own `.URL_PROFILES` so the printout is the
# bundle that actually applies, not a transcription of it.
posture_args <- function(posture, ns) {
  switch(
    posture,
    whatwg = list(args = list(profile = "whatwg"), serialize = "whatwg",
                  expands = ns$.URL_PROFILES[["whatwg"]]),
    rfc3986 = list(args = list(url_standard = "rfc3986",
                               scheme_policy = "require",
                               scheme_acceptance = "general"),
                   serialize = "rfc3986", expands = NULL),
    `rfc-syntax` = list(args = list(profile = "rfc-syntax"),
                        serialize = "rfc3986",
                        expands = ns$.URL_PROFILES[["rfc-syntax"]]),
    null = list(args = list(url_standard = NULL), serialize = NA_character_,
                expands = NULL),
    stop("unknown posture: ", posture, call. = FALSE)
  )
}

fmt_args <- function(x) {
  if (length(x) == 0L) return("")
  toString(vapply(names(x), function(n) {
    v <- x[[n]]
    sprintf("%s = %s", n, if (is.null(v)) "NULL" else deparse(v))
  }, character(1)))
}

# safe_parse_url() with the posture's arguments, warnings collected rather
# than printed mid-table. The scalar function returns NULL on a parse
# failure; that is reported as such rather than smoothed into an NA row that
# looks like a successful parse of nothing.
probe_parse <- function(url, args, ns) {
  acc <- new.env()
  acc$warns <- character(0)
  res <- withCallingHandlers(
    do.call(ns$safe_parse_url, c(list(url = url), args)),
    warning = function(w) {
      acc$warns <- c(acc$warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  warns <- acc$warns
  if (is.null(res)) {
    comp <- list(scheme = NA, userinfo = NA, host = NA, port = NA, path = NA,
                 query = NA, fragment = NA, parse_status = "error")
    warns <- c(warns, "safe_parse_url() returned NULL (parse failure)")
  } else {
    ui <- if (is.na(res$user) && is.na(res$password)) {
      NA_character_
    } else {
      paste0(if (is.na(res$user)) "" else res$user,
             if (is.na(res$password)) "" else paste0(":", res$password))
    }
    comp <- list(scheme = res$scheme, userinfo = ui, host = res$host,
                 port = res$port, path = res$path, query = res$query,
                 fragment = res$fragment, parse_status = res$parse_status)
  }
  list(components = comp, warnings = unique(warns))
}

probe_serialize <- function(url, standard, ns) {
  if (is.na(standard)) return(NULL)
  list(
    source = ns$serialize_url(url, standard = standard, form = "source"),
    normalized = ns$serialize_url(url, standard = standard, form = "normalized")
  )
}

# Reference resolution for --base. `resolve_url(output = "serialized")` is the
# standard's own answer and needs an explicit standard; the null posture only
# has the clean output.
probe_resolve <- function(url, base, posture, ns) {
  std <- switch(posture, whatwg = "whatwg", rfc3986 = , `rfc-syntax` = "rfc3986",
                null = NULL)
  if (is.null(std)) {
    ns$resolve_url(url, base)
  } else {
    ns$resolve_url(url, base, url_standard = std, output = "serialized")
  }
}

# --- oracles -----------------------------------------------------------------

read_json <- function(path) {
  need("jsonlite")
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

# WPT lookup. Without a base: the base-null import, success rows (href +
# getters) and failure rows. With a base: the base-relative fixture, keyed by
# the PAIR -- `input` alone is not unique there. Returns NULL for no row.
wpt_lookup <- function(root, input, base = NULL) {
  if (is.null(base)) {
    d <- read_json(file.path(root, "inst", "bench", "wpt-url-cases.json"))
    for (r in d$success) {
      if (identical(r$input, input)) return(list(kind = "success", row = r))
    }
    for (r in d$failure) {
      if (identical(r$input, input)) return(list(kind = "failure", row = r))
    }
    return(NULL)
  }
  d <- read_json(file.path(root, "tests", "testthat", "fixtures",
                           "wpt-url-base-relative.json"))
  for (r in d$success) {
    if (identical(r$input, input) && identical(r$base, base)) {
      return(list(kind = "success", row = r))
    }
  }
  NULL
}

# The WPT getters restated in rurl's component vocabulary, so the two columns
# can be marked equal or not. The mapping is the URL API's: `protocol` carries
# a trailing ":", `search`/`hash` their leading "?"/"#", and an empty getter
# means "absent" for host/port/query/fragment. Anything lost in that mapping
# is visible in the raw `href`, which is compared verbatim.
wpt_components <- function(row) {
  empty_na <- function(x) if (is.null(x) || identical(x, "")) NA_character_ else x
  ui <- if (identical(row$username, "") && identical(row$password, "")) {
    NA_character_
  } else {
    paste0(row$username, if (!identical(row$password, "")) paste0(":", row$password))
  }
  list(
    scheme = sub(":$", "", row$protocol),
    userinfo = ui,
    host = empty_na(row$hostname),
    port = empty_na(row$port),
    path = empty_na(row$pathname),
    query = empty_na(sub("^\\?", "", row$search)),
    fragment = empty_na(sub("^#", "", row$hash)),
    parse_status = "ok"
  )
}

# The RFC 3986 reference split, from the test helpers, sourced into their own
# environment (Appendix B calls the ABNF matcher by name, so both go in one).
rfc_oracle_env <- function(root) {
  env <- new.env(parent = globalenv())
  for (f in c("helper-rfc3986-abnf.R", "helper-rfc3986-appendix-b.R")) {
    sys.source(file.path(root, "tests", "testthat", f), envir = env)
  }
  env
}

rfc_components <- function(url, env) {
  p <- env$rfc3986_reference_parse(url)
  list(
    scheme = p$scheme, userinfo = p$userinfo, host = p$host, port = p$port,
    path = p$path, query = p$query, fragment = p$fragment,
    parse_status = if (isTRUE(p$valid)) "ok" else "error",
    abnf = isTRUE(p$valid)
  )
}

# --- rendering ---------------------------------------------------------------

show <- function(x) {
  if (is.null(x)) return("-")
  if (length(x) != 1L) return(deparse1(x, collapse = ""))
  if (is.na(x)) return("NA")
  if (identical(x, "")) return('""')
  encodeString(as.character(x), quote = "")
}

# "=" when both sides say the same thing, "~" when the only difference is the
# split's absent/empty distinction rurl's frame does not carry, "≠" otherwise.
mark <- function(a, b) {
  sa <- show(a)
  sb <- show(b)
  if (identical(sa, sb)) return("=")
  if ((sa == "NA" && sb == "\"\"") || (sa == "\"\"" && sb == "NA")) return("~")
  "≠"
}

render_table <- function(rows, header) {
  all <- c(list(header), rows)
  n <- length(header)
  width <- vapply(seq_len(n), function(j) {
    max(nchar(vapply(all, `[[`, character(1), j), type = "width"))
  }, integer(1))
  line <- function(r) {
    cells <- vapply(seq_len(n), function(j) {
      formatC(r[[j]], width = width[[j]], flag = "-")
    }, character(1))
    sub("\\s+$", "", paste0("   ", paste(cells, collapse = "  ")))
  }
  vapply(all, line, character(1))
}

COMPONENT_NAMES <- c("scheme", "userinfo", "host", "port", "path", "query",
                     "fragment", "parse_status")

# One posture, one URL: the block of text a reader sees. Returns the block
# (character vector) and the facts it was rendered from, so --self-test can
# assert on the facts and on the text.
probe_posture <- function(url, posture, ns, root, base = NULL, oracle_env = NULL) {
  spec <- posture_args(posture, ns)
  out <- c(sprintf("== %s", posture))
  if (identical(posture, "null")) {
    out <- c(out, paste0("   note: url_standard = NULL is the frozen legacy ",
                         "profile (ADR 0007/0016), not a conformance posture"))
  }
  out <- c(out, sprintf("   call: safe_parse_url(url, %s)", fmt_args(spec$args)))
  if (!is.null(spec$expands)) {
    out <- c(out, sprintf("         profile expands to: %s", fmt_args(spec$expands)))
  }

  subject <- url
  resolved <- NULL
  if (!is.null(base)) {
    resolved <- probe_resolve(url, base, posture, ns)
    out <- c(out, sprintf("   resolve_url(url, base, %s): %s",
                          if (identical(posture, "null")) "url_standard = NULL" else
                            sprintf("url_standard = %s, output = \"serialized\"",
                                    deparse(if (posture == "whatwg") "whatwg" else "rfc3986")),
                          show(resolved)))
    subject <- if (is.na(resolved)) url else resolved
    out <- c(out, sprintf("   components below are of the resolved string%s",
                          if (is.na(resolved)) " (resolution failed; probing the reference)" else ""))
  }

  parsed <- probe_parse(subject, spec$args, ns)
  ser <- probe_serialize(subject, spec$serialize, ns)
  if (!is.null(ser)) {
    out <- c(out, sprintf("   serialize_url(x, standard = \"%s\"): %s", spec$serialize,
                          show(ser$source)))
    if (!identical(ser$source, ser$normalized)) {
      out <- c(out, sprintf("   serialize_url(x, standard = \"%s\", form = \"normalized\"): %s",
                            spec$serialize, show(ser$normalized)))
    }
  }

  # oracle
  oracle <- NULL
  oracle_label <- "oracle: none"
  expected_href <- NULL
  if (posture == "whatwg") {
    hit <- wpt_lookup(root, url, base)
    if (!is.null(hit) && hit$kind == "success") {
      oracle <- wpt_components(hit$row)
      expected_href <- hit$row$href
      oracle_label <- sprintf("oracle: WPT %s row (href: %s)",
                              if (is.null(base)) "base-null" else "base-relative",
                              show(expected_href))
    } else if (!is.null(hit)) {
      oracle <- as.list(stats::setNames(rep(NA_character_, length(COMPONENT_NAMES)),
                                        COMPONENT_NAMES))
      oracle$parse_status <- "error"
      expected_href <- NA_character_
      oracle_label <- "oracle: WPT base-null FAILURE row (the parser must reject this input)"
    } else {
      oracle_label <- sprintf("oracle: none (no WPT %s row for this input)",
                              if (is.null(base)) "base-null" else "base-relative")
    }
  } else if (posture %in% c("rfc3986", "rfc-syntax")) {
    if (is.null(oracle_env)) oracle_env <- rfc_oracle_env(root)
    oracle <- rfc_components(url, oracle_env)
    oracle_label <- paste0(
      "oracle: RFC 3986 Appendix B split of the ", if (is.null(base)) "source" else
        "REFERENCE (pre-resolution; no section 5.2 resolver in tree)",
      "; ABNF ", if (oracle$abnf) "accepts" else "REJECTS",
      " (split is non-normalizing; NA = absent, \"\" = written empty)"
    )
  }
  out <- c(out, paste0("   ", oracle_label))

  rows <- lapply(COMPONENT_NAMES, function(n) {
    r <- c(n, show(parsed$components[[n]]))
    if (!is.null(oracle)) r <- c(r, show(oracle[[n]]), mark(parsed$components[[n]], oracle[[n]]))
    r
  })
  if (!is.null(expected_href)) {
    got <- if (!is.null(ser)) ser$source else NA_character_
    rows <- c(rows, list(c("href", show(got), show(expected_href), mark(got, expected_href))))
  }
  header <- c("field", "rurl", if (!is.null(oracle)) c("oracle", ""))
  out <- c(out, render_table(rows, header))
  for (w in parsed$warnings) out <- c(out, paste0("   warning: ", encodeString(w, quote = "")))

  list(text = out, components = parsed$components, serialized = ser,
       resolved = resolved, oracle = oracle, expected_href = expected_href,
       oracle_kind = if (is.null(oracle)) "none" else if (posture == "whatwg") "wpt" else "rfc")
}

# --- self-test ---------------------------------------------------------------

self_test <- function(root, ns) {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)
  env <- rfc_oracle_env(root)

  # A WPT success row: the oracle is found, and rurl's serialization is the
  # upstream href (the same claim tests/testthat/test-wpt-full-suite.R makes).
  r <- probe_posture("https://test:@test", "whatwg", ns, root)
  if (!identical(r$expected_href, "https://test@test/")) {
    fail("did not find the WPT success row for https://test:@test")
  }
  if (!identical(r$serialized$source, r$expected_href)) {
    fail(sprintf("serialize_url() disagrees with the WPT href: %s vs %s",
                 show(r$serialized$source), show(r$expected_href)))
  }
  if (!any(grepl("^   href ", r$text)) || !any(grepl(" = *$", grep("^   href ", r$text, value = TRUE)))) {
    fail("the href row is not rendered as equal")
  }
  if (!any(grepl("profile expands to: .*scheme_policy = \"require\"", r$text))) {
    fail("the whatwg argument set was not expanded from .URL_PROFILES in the output")
  }

  # A WPT failure row: the oracle says reject; rurl must serialize to NA.
  r <- probe_posture("file://example:1/", "whatwg", ns, root)
  if (!identical(r$oracle$parse_status, "error") || !is.na(r$expected_href)) {
    fail("did not find the WPT failure row for file://example:1/")
  }
  if (!is.na(r$serialized$source)) {
    fail("serialize_url() produced output for a WPT failure row")
  }
  if (!any(grepl("FAILURE row", r$text))) fail("failure row not labelled")

  # The RFC decomposition: absent authority (NA) with the path carrying the
  # host-looking text -- the case Appendix B exists to make visible.
  r <- probe_posture("http:/evil.com", "rfc3986", ns, root, oracle_env = env)
  o <- r$oracle
  if (!identical(o$scheme, "http") || !is.na(o$host) || !identical(o$path, "/evil.com") ||
        !identical(o$parse_status, "ok") || !isTRUE(o$abnf)) {
    fail(sprintf("Appendix B split is wrong: scheme=%s host=%s path=%s status=%s",
                 show(o$scheme), show(o$host), show(o$path), show(o$parse_status)))
  }
  if (!identical(r$oracle_kind, "rfc") || !any(grepl("Appendix B", r$text))) {
    fail("rfc3986 posture did not present the Appendix B oracle")
  }
  # rfc-syntax uses the same split.
  r2 <- probe_posture("http:/evil.com", "rfc-syntax", ns, root, oracle_env = env)
  if (!identical(r2$oracle, o) || !any(grepl("profile expands to: .*path_normalization = \"none\"", r2$text))) {
    fail("rfc-syntax posture did not reuse the split or expand its profile")
  }

  # No oracle: say so, invent nothing.
  r <- probe_posture("http://no-such-row.invalid/zzz", "whatwg", ns, root)
  if (!identical(r$oracle_kind, "none") || !is.null(r$expected_href) ||
        !any(grepl("oracle: none", r$text, fixed = TRUE))) {
    fail("an input with no fixture row did not print `oracle: none`")
  }
  r <- probe_posture("http://no-such-row.invalid/zzz", "null", ns, root)
  if (!identical(r$oracle_kind, "none") || !is.null(r$serialized) ||
        !any(grepl("not a conformance posture", r$text, fixed = TRUE)) ||
        any(grepl("serialize_url", r$text, fixed = TRUE))) {
    fail("the null posture printed an oracle or a serialization")
  }

  # --base: the base-relative fixture is keyed by the pair, and rurl's
  # resolution is the upstream href.
  r <- probe_posture("/a/b/c", "whatwg", ns, root, base = "http://example.org/foo/bar")
  if (!identical(r$expected_href, "http://example.org/a/b/c")) {
    fail("did not find the base-relative row for /a/b/c against http://example.org/foo/bar")
  }
  if (!identical(r$resolved, r$expected_href) || !identical(r$serialized$source, r$expected_href)) {
    fail(sprintf("resolution disagrees with the WPT href: %s vs %s",
                 show(r$resolved), show(r$expected_href)))
  }
  r <- probe_posture("/a/b/c", "rfc3986", ns, root, base = "http://example.org/foo/bar",
                     oracle_env = env)
  if (!identical(r$resolved, "http://example.org/a/b/c") || !any(grepl("pre-resolution", r$text))) {
    fail("rfc3986 --base did not resolve, or did not label its oracle as the reference split")
  }

  # The comparison mark must be able to go red: a fabricated expectation is
  # flagged, so an "=" column is evidence and not decoration.
  if (!identical(mark("http://a/", "http://b/"), "≠") || !identical(mark(NA_character_, ""), "~") ||
        !identical(mark("x", "x"), "=")) {
    fail("mark() does not distinguish equal, absent-vs-empty and different")
  }

  # Every posture renders for one ordinary input without error.
  for (p in POSTURES) probe_posture("http://Example.COM/a/../b?q=1#f", p, ns, root, oracle_env = env)

  cat("posture-probe self-test: PASS (WPT success + failure, RFC split x2, no-oracle x2, --base x2, mark)\n")
  invisible(TRUE)
}

# --- main --------------------------------------------------------------------

usage <- function() {
  cat(paste0(
    "usage: Rscript tools/posture-probe.R <url> [--posture ",
    paste(POSTURES, collapse = "|"), "] [--base <base-url>]\n",
    "       Rscript tools/posture-probe.R --self-test\n"
  ))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  root <- repo_root()
  need("pkgload")
  ns <- pkgload::load_all(root, quiet = TRUE)$env

  if ("--self-test" %in% args) {
    self_test(root, ns)
    return(invisible(TRUE))
  }

  posture <- NULL
  base <- NULL
  url <- NULL
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a == "--posture") {
      posture <- args[[i + 1L]]
      i <- i + 2L
    } else if (a == "--base") {
      base <- args[[i + 1L]]
      i <- i + 2L
    } else if (startsWith(a, "--")) {
      usage()
      stop("unknown flag: ", a, call. = FALSE)
    } else {
      url <- a
      i <- i + 1L
    }
  }
  if (is.null(url)) {
    usage()
    stop("no URL given", call. = FALSE)
  }
  if (!is.null(posture) && !(posture %in% POSTURES)) {
    stop("--posture must be one of: ", paste(POSTURES, collapse = ", "), call. = FALSE)
  }
  postures <- if (is.null(posture)) POSTURES else posture

  cat(sprintf("posture-probe: %s%s\n", show(url),
              if (is.null(base)) "" else sprintf("   base: %s", show(base))))
  env <- if (any(postures %in% c("rfc3986", "rfc-syntax"))) rfc_oracle_env(root) else NULL
  for (p in postures) {
    r <- probe_posture(url, p, ns, root, base = base, oracle_env = env)
    cat(r$text, sep = "\n")
  }
  cat("   legend: = same; ~ absent-vs-empty only (rurl's frame reports both as NA); ≠ differs\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  if (sys.nframe() == 0L) {
    main()
  }
}
