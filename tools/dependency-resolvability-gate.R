#!/usr/bin/env Rscript

# Dependency-resolvability gate: one script, seven repos.
#
# WHY THIS EXISTS. The seven packages in this family depend on each other, and
# every one of them declares those dependencies twice -- once as a version floor
# in `Imports:`, and again, implicitly, as the set of `dep::symbol` call sites in
# `R/` and `tests/`. Nothing checked that either declaration described a package
# that a stranger could actually install. The failure mode is not theoretical and
# not uniform: measured 2026-08-12 across the seven trees, FOUR repos fail this
# gate on FOUR different defects, and each one is invisible to a fully-populated
# local library because the local library holds development trees.
#
#   rurl       tests/ calls `punycoder::unicode_versions()`, which is exported by
#              punycoder's HEAD and by NO released version -- not CRAN 1.2.1, not
#              tag v1.2.1, not tag v1.2.0 (RURL-ovgasmea). `devtools::test()` is
#              green because the local punycoder IS the development tree.
#   pslr       `Remotes: gitlab::bart-turczynski/punycoder` names no ref, so the
#              tree builds against whatever punycoder HEAD happens to be that day.
#   pagerankr  floors `rurl (>= 3.0.0)`, a version that exists nowhere -- not on
#              CRAN (1.2.0), not as a git tag. And it carries a `Remotes:` field
#              while declaring a RELEASE version (0.1.0), which is the one state
#              in which CRAN rejects the field outright.
#   sitemapr   pins its `Remotes:` at `bart-turczynski/rurl@v2.2.1` with no host
#              prefix, which resolves to GitHub -- the suspended account. Pinned,
#              precise, and unreachable.
#
# WHAT MAKES THIS DIFFERENT FROM `R CMD check`. `R CMD check` verifies the
# package against THE LIBRARY ON THIS MACHINE. Every defect above survives it,
# because every one of them is a disagreement between what DESCRIPTION promises
# and what the wider world can serve. This gate never reads the local library.
# It reads released artifacts: CRAN tarballs and sibling git tags.
#
# THE FIVE PROPERTIES
#   P1  every floor in `Depends:`/`Imports:` is SATISFIABLE by a version that
#       exists -- on CRAN, or as a git tag in the sibling repo;
#   P2  every `dep::symbol` in `R/` is exported by that dependency at the
#       resolution artifact, read from the RELEASED NAMESPACE;
#   P3  every `dep::symbol` in `tests/` satisfies P2 or sits behind a skip guard;
#   P4  no `Remotes:` on a release tree; on a development tree every entry is
#       pinned to a ref, and that ref resolves;
#   P5  unresolvable inputs ABORT rather than score zero findings.
#
# WHY P1 IS SATISFIABILITY AND NOT EXISTENCE. The tempting rule -- "the floor
# names a version that was released" -- produces a false positive on the very
# first case it meets. rurl floors `punycoder (>= 1.2.0)`; punycoder 1.2.0 was
# never on CRAN (the index goes 1.1.0 then 1.2.1), yet the floor is perfectly
# satisfiable by 1.2.1 and a CRAN user installs without noticing. A floor is a
# lower bound, so the question it answers is whether the bound admits anything,
# not whether the bound is itself a shipped artifact. The version-never-released
# fact is still reported, as a note rather than a finding.
#
# WHY P2 CHECKS TWO ARTIFACTS, NOT ONE. A floor promises the package works at
# the floor; CRAN delivers the current release. Those are different tarballs and
# a symbol can be missing from either. `punycoder (>= 1.2.0)` resolves to tag
# v1.2.0 for the floor and to CRAN 1.2.1 for the current release, and
# `unicode_versions` is absent from both -- but a package that used a symbol
# added in 1.2.1 would pass a current-release check and fail its own floor. Both
# are checked, and the message names which artifact was consulted.
#
# WHY IT PARSES AND DOES NOT GREP. Written first as a grep, this gate reported
# `punycoder::unicode_versions` as a RUNTIME call in `R/format.R`. It is a
# mention in a comment. A regex cannot tell a call site from prose about a call
# site, and this codebase deliberately writes prose about its dependencies. Call
# sites therefore come from `parse()` and a walk of the resulting expressions:
# comments never reach the parse tree, and a package name inside a string
# literal is not a `::` call. Same instrument as checks C3/C4 of the
# zero-reference gate next door, for the same reason.
#
# NON-VACUITY (P5). A gate that scores zero findings because it found no
# population passes for the wrong reason, which is the worst outcome available
# to it. Each of these aborts instead: a DESCRIPTION that will not parse; a
# dependency whose version universe comes back empty; a downloaded artifact that
# is not a gzip archive (a CRAN 404 serves an HTML body with status 200 to a
# careless fetch -- this gate hit exactly that and cached a 992-byte error page
# as a tarball); a NAMESPACE with no exports; and, before anything else, a
# reachability probe -- if the network is down, every remote is "unreachable"
# and the gate would report a page of confident nonsense.
#
# NETWORK. This gate is deliberately NOT network-free, unlike the other gates in
# `tools/`. Its whole subject is what the outside world serves, so a cached
# transcript would answer a question about the past. Artifacts are cached under
# `$RURL_DEPGATE_CACHE` (default `~/.cache/rurl-dep-gate`) so repeat runs are
# cheap, and `--offline` scores from the cache alone and aborts on a miss rather
# than guessing. It is a release-readiness gate, not a per-push gate.
#
# Usage:
#   Rscript tools/dependency-resolvability-gate.R              # this repo
#   Rscript tools/dependency-resolvability-gate.R --repo ../pslr
#   Rscript tools/dependency-resolvability-gate.R --all        # every sibling
#   Rscript tools/dependency-resolvability-gate.R --offline    # cache only
#   Rscript tools/dependency-resolvability-gate.R --self-test  # unit checks

# --- constants ---------------------------------------------------------------

# Shipped with R itself, so their "version" is R's and there is no artifact to
# resolve. Recommended packages are NOT here: they live on CRAN and resolve
# normally, which is the correct treatment -- a floor on Matrix is a real floor.
BASE_PACKAGES <- c(
  "base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods",
  "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"
)

CRAN <- "https://cloud.r-project.org"

# The probe that separates "the network is down" from "this host does not serve
# that ref". Without it, an offline laptop reports every `Remotes:` entry as
# broken and the run looks like a catastrophe instead of a disconnection.
REACHABILITY_PROBE <- paste0(CRAN, "/src/contrib/PACKAGES.gz")

cache_dir <- function() {
  d <- Sys.getenv("RURL_DEPGATE_CACHE", unset = "")
  if (!nzchar(d)) d <- file.path(path.expand("~"), ".cache", "rurl-dep-gate")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# --- DESCRIPTION ---------------------------------------------------------------

# One field, unfolded. R's DESCRIPTION is RFC-822-ish: a field name, a colon, and
# continuation lines indented by whitespace.
dcf_field <- function(lines, field) {
  start <- grep(sprintf("^%s:", field), lines)
  if (length(start) == 0L) return(NA_character_)
  start <- start[[1L]]
  body <- sub(sprintf("^%s:", field), "", lines[start])
  i <- start + 1L
  while (i <= length(lines) && grepl("^[[:space:]]", lines[i])) {
    body <- c(body, lines[i])
    i <- i + 1L
  }
  trimws(gsub("[[:space:]]+", " ", paste(body, collapse = " ")))
}

# `pkg (>= 1.2.0)` -> name, operator, version. A requirement with no constraint
# yields NA for both, which is a legitimate state and not a parse failure.
split_requirement <- function(tok) {
  tok <- trimws(tok)
  name <- trimws(sub("[(].*$", "", tok))
  op <- NA_character_
  ver <- NA_character_
  if (grepl("[(]", tok)) {
    inner <- trimws(sub("^[^(]*[(]([^)]*)[)].*$", "\\1", tok))
    m <- regmatches(inner, regexec("^([<>=!]+)[[:space:]]*(.+)$", inner))[[1L]]
    if (length(m) == 3L) {
      op <- m[[2L]]
      ver <- trimws(m[[3L]])
    }
  }
  list(name = name, op = op, version = ver)
}

parse_requirements <- function(value) {
  if (is.na(value) || !nzchar(value)) return(list())
  toks <- trimws(unlist(strsplit(value, ",")))
  toks <- toks[nzchar(toks)]
  lapply(toks, split_requirement)
}

# `gitlab::owner/repo@ref` and its many abbreviations. A bare `owner/repo`
# resolves to GitHub, which is not a neutral default in this family: GitHub is
# the suspended account, so the abbreviation that looks like a shorthand is in
# fact a choice of an unreachable host.
parse_remote <- function(tok) {
  tok <- trimws(tok)
  host <- "github"
  spec <- tok
  if (grepl("^[A-Za-z]+::", spec)) {
    host <- sub("::.*$", "", spec)
    spec <- sub("^[A-Za-z]+::", "", spec)
  }
  ref <- NA_character_
  if (grepl("@", spec)) {
    ref <- sub("^[^@]*@", "", spec)
    spec <- sub("@.*$", "", spec)
  }
  spec <- sub("/$", "", spec)
  list(raw = tok, host = tolower(host), repo = spec, ref = ref)
}

read_description <- function(root) {
  path <- file.path(root, "DESCRIPTION")
  if (!file.exists(path)) {
    stop("no DESCRIPTION at: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  pkg <- dcf_field(lines, "Package")
  version <- dcf_field(lines, "Version")
  if (is.na(pkg) || is.na(version)) {
    stop(sprintf("DESCRIPTION at %s has no Package/Version -- refusing to score",
                 path), call. = FALSE)
  }
  # The honest vacuity guard. A package with NO dependencies is a legitimate
  # state and must pass, so "zero requirements" is not the trigger; a field that
  # is PRESENT and yields nothing is, because that is a parse failure wearing an
  # empty set as a disguise.
  for (f in c("Depends", "Imports")) {
    raw <- dcf_field(lines, f)
    if (!is.na(raw) && nzchar(raw) && length(parse_requirements(raw)) == 0L) {
      stop(sprintf("%s has a `%s:` field this gate cannot parse: %s",
                   path, f, raw), call. = FALSE)
    }
  }

  remotes_raw <- dcf_field(lines, "Remotes")
  remotes <- if (is.na(remotes_raw)) {
    list()
  } else {
    lapply(trimws(unlist(strsplit(remotes_raw, ","))), parse_remote)
  }
  list(
    package = pkg,
    version = version,
    depends = parse_requirements(dcf_field(lines, "Depends")),
    imports = parse_requirements(dcf_field(lines, "Imports")),
    remotes = remotes,
    has_remotes_field = !is.na(remotes_raw)
  )
}

# R's own convention: three components is a release, a fourth is a development
# state. `0.1.0` is a release tree; `0.1.1.9000` is not. This is the predicate
# P4 turns on, so it gets its own name rather than an inline expression.
is_development_version <- function(v) {
  length(unlist(strsplit(v, "[.-]"))) >= 4L
}

# --- call sites (parsed, never grepped) ---------------------------------------

# A NOTE ON R'S EMPTY ARGUMENT, because both walkers below are written around
# it and the natural spelling is the broken one. The hole in `x[, 1]` is a real
# element of the call -- a symbol whose name is "". Subscripting it is safe, but
# BINDING it and then looking the binding up is not: `part <- e[[i]]` succeeds
# and the very next mention of `part`, including `is.null(part)`, raises
# `argument "part" is missing, with no default`. `tryCatch` around the subscript
# does not help, because the subscript was never what failed. So the loops below
# test and recurse on `e[[i]]` directly and never assign it to a name;
# `is.call(e[[i]])` answers FALSE for the empty symbol without forcing it, which
# is exactly the guard needed. Written the natural way, this gate aborted on the
# first subscript in `R/`.

# Every `pkg::symbol` and `pkg:::symbol` in one expression tree. Returns a
# character matrix-free list of pkg/symbol pairs; comments are already gone
# because `parse()` dropped them.
qualified_uses_in_expr <- function(e, out) {
  if (is.call(e)) {
    head_ <- e[[1L]]
    if (is.name(head_) && as.character(head_) %in% c("::", ":::") &&
        length(e) == 3L) {
      pkg <- tryCatch(as.character(e[[2L]]), error = function(...) NA_character_)
      sym <- tryCatch(as.character(e[[3L]]), error = function(...) NA_character_)
      if (!is.na(pkg) && !is.na(sym)) {
        out[[length(out) + 1L]] <- list(package = pkg, symbol = sym)
      }
    }
    for (i in seq_along(e)) {
      if (is.call(e[[i]])) out <- qualified_uses_in_expr(e[[i]], out)
    }
  }
  out
}

r_files <- function(dir) {
  if (!dir.exists(dir)) return(character(0))
  list.files(dir, pattern = "[.][RrSsq]$|[.][Rr]md$", recursive = TRUE,
             full.names = TRUE)
}

# All `pkg::symbol` uses under one directory, each tagged with the file it came
# from -- P3 needs the file, because a skip guard is scoped to its file.
qualified_uses <- function(dir) {
  out <- list()
  for (f in r_files(dir)) {
    if (grepl("[.][Rr]md$", f)) next
    exprs <- tryCatch(parse(f, keep.source = FALSE),
                      error = function(e) NULL)
    if (is.null(exprs)) {
      stop(sprintf("cannot parse %s -- refusing to score a directory it is in",
                   f), call. = FALSE)
    }
    for (e in exprs) {
      for (u in qualified_uses_in_expr(e, list())) {
        u$file <- f
        out[[length(out) + 1L]] <- u
      }
    }
  }
  out
}

# The skip guards in one test file, as a named vector of version floors:
# `skip_if_not_installed("punycoder", "1.3.0")` -> c(punycoder = "1.3.0").
# A guard with no version is recorded as NA and does NOT excuse a missing
# symbol: the dependency IS installed, so the guard never fires. That is the
# distinction that makes the exemption meaningful rather than a formality.
skip_guards <- function(file) {
  exprs <- tryCatch(parse(file, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  found <- character(0)
  walk <- function(e) {
    if (!is.call(e)) return(invisible(NULL))
    head_ <- e[[1L]]
    nm <- if (is.name(head_)) {
      as.character(head_)
    } else if (is.call(head_) && length(head_) == 3L &&
               is.name(head_[[1L]]) &&
               as.character(head_[[1L]]) %in% c("::", ":::")) {
      as.character(head_[[3L]])
    } else {
      ""
    }
    if (identical(nm, "skip_if_not_installed") && length(e) >= 2L) {
      pkg <- tryCatch(as.character(e[[2L]]), error = function(...) NA_character_)
      ver <- if (length(e) >= 3L) {
        tryCatch(as.character(e[[3L]]), error = function(...) NA_character_)
      } else {
        NA_character_
      }
      if (!is.na(pkg) && is.character(pkg) && length(pkg) == 1L) {
        found[[pkg]] <<- ver
      }
    }
    for (i in seq_along(e)) {
      if (is.call(e[[i]])) walk(e[[i]])
    }
    invisible(NULL)
  }
  for (e in exprs) walk(e)
  found
}

# --- the version universe -----------------------------------------------------

# CRAN's current release for every package, fetched once. `available.packages()`
# is used rather than a hand-rolled index parse so the gate inherits R's own
# understanding of the repository layout.
cran_index <- local({
  cached <- NULL
  function(offline) {
    if (!is.null(cached)) return(cached)
    path <- file.path(cache_dir(), "cran-index.rds")
    if (offline) {
      if (!file.exists(path)) {
        stop("--offline was requested but no CRAN index is cached at ", path,
             call. = FALSE)
      }
      cached <<- readRDS(path)
      return(cached)
    }
    ap <- tryCatch(
      utils::available.packages(repos = CRAN, filters = "duplicates"),
      error = function(e) NULL
    )
    if (is.null(ap) || nrow(ap) == 0L) {
      stop("could not read the CRAN index -- refusing to score", call. = FALSE)
    }
    cached <<- stats::setNames(as.character(ap[, "Version"]), rownames(ap))
    saveRDS(cached, path)
    cached
  }
})

# Everything CRAN has ever served for one package: the current release plus the
# Archive directory. Archive is scraped from the directory index because CRAN
# publishes no machine-readable history of it.
cran_versions <- function(pkg, offline) {
  idx <- cran_index(offline)
  current <- if (pkg %in% names(idx)) unname(idx[[pkg]]) else character(0)

  path <- file.path(cache_dir(), sprintf("archive-%s.rds", pkg))
  archived <- if (file.exists(path)) {
    readRDS(path)
  } else if (offline) {
    stop(sprintf("--offline: no cached Archive listing for %s", pkg),
         call. = FALSE)
  } else {
    # A package with no Archive directory 404s, and that is the NORMAL state for
    # one that has only ever had a single release. Silenced, not because errors
    # do not matter here, but because this one is not an error -- an empty
    # listing and a missing listing mean the same thing to the caller.
    url <- sprintf("%s/src/contrib/Archive/%s/", CRAN, pkg)
    html <- suppressWarnings(tryCatch(
      paste(readLines(url, warn = FALSE), collapse = "\n"),
      error = function(e) ""
    ))
    hits <- unlist(regmatches(
      html,
      gregexpr(sprintf("%s_[0-9][0-9A-Za-z._-]*\\.tar\\.gz",
                       gsub("([.\\\\^$])", "\\\\\\1", pkg)), html)
    ))
    v <- unique(sub("[.]tar[.]gz$", "", sub(sprintf("^%s_", pkg), "", hits)))
    saveRDS(v, path)
    v
  }
  unique(c(current, archived))
}

# Tags in a sibling checkout that name a version: `v1.2.0` or `1.2.0`. Anything
# else -- `abandoned/...`, `v3/cp-snapshot-1`, `prerebase-.../main` -- is a
# working label, not a release, and admitting it would let the gate resolve a
# floor against a branch snapshot.
sibling_tag_versions <- function(sibling_root) {
  if (is.null(sibling_root) || !dir.exists(file.path(sibling_root, ".git"))) {
    return(character(0))
  }
  tags <- suppressWarnings(system2(
    "git", c("-C", shQuote(sibling_root), "tag"),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(tags) == 0L) return(character(0))
  keep <- grepl("^v?[0-9]+([.][0-9]+)+$", tags)
  stats::setNames(sub("^v", "", tags[keep]), tags[keep])
}

# Where a sibling package's checkout lives, if it is one of ours. Siblings are
# discovered by layout -- a directory next to this repo whose DESCRIPTION names
# that package -- rather than from a hardcoded list of seven, so the gate does
# not have to be edited when the family changes.
find_sibling <- local({
  cached <- new.env(parent = emptyenv())
  function(pkg, parent_dir) {
    key <- paste0(parent_dir, "|", pkg)
    if (!is.null(cached[[key]])) {
      v <- cached[[key]]
      return(if (identical(v, "")) NULL else v)
    }
    hit <- ""
    candidate <- file.path(parent_dir, pkg)
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      nm <- dcf_field(readLines(file.path(candidate, "DESCRIPTION"),
                                warn = FALSE), "Package")
      if (identical(nm, pkg)) hit <- candidate
    }
    cached[[key]] <- hit
    if (identical(hit, "")) NULL else hit
  }
})

# --- released NAMESPACE exports -----------------------------------------------

# TRUE only for a real gzip member. This is P5's sharpest edge: a CRAN 404 can
# arrive as an HTML body, and a fetch that only checks "did I get bytes" will
# cache that page under a tarball's name and then report every symbol in the
# package as unexported.
is_gzip <- function(path) {
  if (!file.exists(path) || file.info(path)$size < 32) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  magic <- readBin(con, "raw", n = 2L)
  length(magic) == 2L && magic[[1L]] == as.raw(0x1f) && magic[[2L]] == as.raw(0x8b)
}

download_tarball <- function(pkg, version, offline) {
  fname <- sprintf("%s_%s.tar.gz", pkg, version)
  dest <- file.path(cache_dir(), fname)
  if (is_gzip(dest)) return(dest)
  if (file.exists(dest)) unlink(dest)
  if (offline) {
    stop(sprintf("--offline: %s is not in the cache", fname), call. = FALSE)
  }
  urls <- c(
    sprintf("%s/src/contrib/%s", CRAN, fname),
    sprintf("%s/src/contrib/Archive/%s/%s", CRAN, pkg, fname)
  )
  for (u in urls) {
    ok <- tryCatch({
      utils::download.file(u, dest, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok && is_gzip(dest)) return(dest)
    if (file.exists(dest)) unlink(dest)
  }
  NULL
}

# The `export()`/`exportPattern()` surface of one released artifact. Parsed, not
# grepped, for the same reason call sites are.
namespace_exports <- function(text) {
  exprs <- tryCatch(parse(text = text, keep.source = FALSE),
                    error = function(e) NULL)
  if (is.null(exprs)) return(NULL)
  exact <- character(0)
  patterns <- character(0)
  for (e in exprs) {
    if (!is.call(e) || !is.name(e[[1L]])) next
    fn <- as.character(e[[1L]])
    if (fn %in% c("export", "exportClasses", "exportMethods")) {
      for (i in seq_along(e)[-1L]) {
        exact <- c(exact, as.character(e[[i]]))
      }
    } else if (identical(fn, "exportPattern")) {
      for (i in seq_along(e)[-1L]) {
        patterns <- c(patterns, as.character(e[[i]]))
      }
    } else if (identical(fn, "S3method") && length(e) >= 3L) {
      # An S3 method is reachable through its generic, never as `pkg::name`,
      # so it is deliberately NOT an export for this gate's purposes.
      next
    }
  }
  list(exact = unique(exact), patterns = unique(patterns))
}

exports_from_tarball <- function(pkg, version, offline) {
  tarball <- download_tarball(pkg, version, offline)
  if (is.null(tarball)) return(NULL)
  member <- file.path(pkg, "NAMESPACE")
  tmp <- tempfile("depgate-untar-")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({
    utils::untar(tarball, files = member, exdir = tmp)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  path <- file.path(tmp, member)
  if (!ok || !file.exists(path)) return(NULL)
  namespace_exports(paste(readLines(path, warn = FALSE), collapse = "\n"))
}

exports_from_tag <- function(sibling_root, tag) {
  if (is.null(sibling_root)) return(NULL)
  txt <- suppressWarnings(system2(
    "git", c("-C", shQuote(sibling_root), "show",
             shQuote(paste0(tag, ":NAMESPACE"))),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(txt) == 0L) return(NULL)
  namespace_exports(paste(txt, collapse = "\n"))
}

is_exported <- function(exports, symbol) {
  if (is.null(exports)) return(NA)
  if (symbol %in% exports$exact) return(TRUE)
  for (p in exports$patterns) {
    if (grepl(p, symbol)) return(TRUE)
  }
  FALSE
}

# --- resolution ---------------------------------------------------------------

# Everything known to serve one dependency, as a data frame of version + origin.
# Both origins are kept because P2's message has to say WHICH artifact it read.
version_universe <- function(pkg, parent_dir, offline) {
  sibling <- find_sibling(pkg, parent_dir)
  tags <- sibling_tag_versions(sibling)
  cran <- cran_versions(pkg, offline)
  rows <- c(
    lapply(cran, function(v) list(version = v, origin = "cran", ref = NA_character_)),
    lapply(seq_along(tags), function(i) {
      list(version = unname(tags[[i]]), origin = "tag", ref = names(tags)[[i]])
    })
  )
  list(sibling = sibling, rows = rows,
       versions = unique(vapply(rows, `[[`, character(1), "version")))
}

# The artifacts P2/P3 must consult for one dependency: the floor (exactly, or
# the lowest release above it) and CRAN's current offering. Deduplicated, since
# they are usually the same tarball.
resolution_points <- function(pkg, floor, universe, offline) {
  out <- list()
  cran_current <- {
    idx <- cran_index(offline)
    if (pkg %in% names(idx)) unname(idx[[pkg]]) else NA_character_
  }
  if (!is.na(floor)) {
    satisfying <- Filter(
      function(r) utils::compareVersion(r$version, floor) >= 0, universe$rows
    )
    if (length(satisfying) > 0L) {
      vs <- numeric_version(vapply(satisfying, `[[`, character(1), "version"))
      lowest <- satisfying[[order(vs)[[1L]]]]
      out[[length(out) + 1L]] <- c(lowest, list(role = "floor"))
    }
  }
  if (!is.na(cran_current)) {
    already <- any(vapply(out, function(r) identical(r$version, cran_current),
                          logical(1)))
    if (!already) {
      out[[length(out) + 1L]] <- list(version = cran_current, origin = "cran",
                                      ref = NA_character_, role = "cran-current")
    }
  }
  out
}

exports_at <- local({
  cached <- new.env(parent = emptyenv())
  function(pkg, point, sibling, offline) {
    key <- paste(pkg, point$version, point$origin, sep = "|")
    if (!is.null(cached[[key]])) {
      v <- cached[[key]]
      return(if (identical(v, "NONE")) NULL else v)
    }
    ex <- if (identical(point$origin, "tag")) {
      exports_from_tag(sibling, point$ref)
    } else {
      exports_from_tarball(pkg, point$version, offline)
    }
    # A tag that is not on CRAN can still be readable as a tarball, and the
    # reverse: fall back rather than abort, and let P5 abort only if BOTH fail.
    if (is.null(ex) && identical(point$origin, "tag")) {
      ex <- exports_from_tarball(pkg, point$version, offline)
    }
    cached[[key]] <- if (is.null(ex)) "NONE" else ex
    ex
  }
})

# --- the checks ---------------------------------------------------------------

finding <- function(prop, text) list(prop = prop, text = text)

check_floors <- function(desc, parent_dir, offline) {
  out <- list()
  notes <- character(0)
  reqs <- c(desc$depends, desc$imports)
  reqs <- Filter(function(r) !identical(r$name, "R"), reqs)
  universes <- list()
  for (r in reqs) {
    if (r$name %in% BASE_PACKAGES) next
    u <- version_universe(r$name, parent_dir, offline)
    # An EMPTY universe is a finding, not an abort. The distinction P5 draws is
    # between "I could not find out" (network down, cache miss -- abort) and "I
    # found out, and the answer is nothing" -- which is the most severe
    # resolvability defect there is, and reporting it as an abort would hide it
    # behind the noise of a broken run. pagerankr is exactly this: it declares
    # Version 0.1.0 with no git tag of any kind and is not on CRAN, so anything
    # depending on it depends on a package that exists on one laptop.
    if (length(u$versions) == 0L) {
      out[[length(out) + 1L]] <- finding("P1", sprintf(
        "`%s` is served by NOTHING: not on CRAN, and %s -- no version of it can be resolved",
        r$name,
        if (is.null(u$sibling)) {
          "there is no sibling checkout next to this repo"
        } else {
          sprintf("the checkout at %s has no release tag", u$sibling)
        }
      ))
      next
    }
    universes[[r$name]] <- u
    if (is.na(r$version)) next

    satisfied <- Filter(
      function(v) utils::compareVersion(v, r$version) >= 0, u$versions
    )
    if (length(satisfied) == 0L) {
      best <- u$versions[[order(numeric_version(u$versions),
                                decreasing = TRUE)[[1L]]]]
      out[[length(out) + 1L]] <- finding("P1", sprintf(
        "`%s (%s %s)` is UNSATISFIABLE: the newest %s that exists anywhere is %s (%s)",
        r$name, r$op, r$version, r$name, best,
        if (is.null(u$sibling)) "CRAN only" else "CRAN + sibling tags"
      ))
      next
    }
    if (!r$version %in% u$versions) {
      notes <- c(notes, sprintf(
        "`%s %s` names a version that was never released; satisfiable by %s",
        r$name, r$version,
        as.character(sort(numeric_version(satisfied))[[1L]])
      ))
    }
  }
  list(findings = out, notes = notes, universes = universes)
}

check_symbols <- function(desc, dir, prop, universes, parent_dir, offline,
                          allow_guards) {
  out <- list()
  uses <- qualified_uses(dir)
  declared <- unique(c(
    vapply(desc$imports, `[[`, character(1), "name"),
    vapply(desc$depends, `[[`, character(1), "name")
  ))
  floors <- stats::setNames(
    vapply(c(desc$imports, desc$depends), `[[`, character(1), "version"),
    vapply(c(desc$imports, desc$depends), `[[`, character(1), "name")
  )

  checked <- 0L
  seen <- character(0)
  for (u in uses) {
    pkg <- u$package
    if (pkg %in% BASE_PACKAGES) next
    if (identical(pkg, desc$package)) next
    if (!pkg %in% declared) next
    key <- paste(pkg, u$symbol, sep = "::")
    universe <- universes[[pkg]]
    if (is.null(universe)) {
      universe <- version_universe(pkg, parent_dir, offline)
      if (length(universe$versions) == 0L) next  # already a P1 finding
      universes[[pkg]] <- universe
    }
    floor <- if (pkg %in% names(floors)) floors[[pkg]] else NA_character_
    points <- resolution_points(pkg, floor, universe, offline)
    if (length(points) == 0L) {
      stop(sprintf("cannot resolve any artifact for `%s` -- refusing to score %s",
                   pkg, dir), call. = FALSE)
    }
    for (p in points) {
      ex <- exports_at(pkg, p, universe$sibling, offline)
      if (is.null(ex)) {
        stop(sprintf(
          "could not read a released NAMESPACE for %s %s (%s) -- refusing to score",
          pkg, p$version, p$origin), call. = FALSE)
      }
      if (length(ex$exact) == 0L && length(ex$patterns) == 0L) {
        stop(sprintf("%s %s exports nothing -- that is not a real NAMESPACE",
                     pkg, p$version), call. = FALSE)
      }
      checked <- checked + 1L
      if (isTRUE(is_exported(ex, u$symbol))) next

      excuse <- NULL
      if (allow_guards) {
        guards <- skip_guards(u$file)
        if (pkg %in% names(guards)) {
          g <- guards[[pkg]]
          if (!is.na(g) && utils::compareVersion(g, p$version) > 0) {
            excuse <- sprintf("skip guard requires %s >= %s", pkg, g)
          }
        }
      }
      if (!is.null(excuse)) next

      rel <- sub(paste0("^", normalizePath(dirname(dir), mustWork = FALSE), "/"),
                 "", normalizePath(u$file, mustWork = FALSE))
      msg <- sprintf(
        "`%s()` is called in %s but %s %s (%s%s) does not export it",
        key, rel, pkg, p$version, p$role,
        if (identical(p$origin, "tag")) sprintf(", tag %s", p$ref) else ", CRAN"
      )
      if (allow_guards) {
        guards <- skip_guards(u$file)
        msg <- if (pkg %in% names(guards) && is.na(guards[[pkg]])) {
          paste0(msg, " -- the file's `skip_if_not_installed(\"", pkg,
                 "\")` states no version, so it never fires")
        } else {
          paste0(msg, " -- and no skip guard names a version above ", p$version)
        }
      }
      if (!msg %in% seen) {
        seen <- c(seen, msg)
        out[[length(out) + 1L]] <- finding(prop, msg)
      }
    }
  }
  list(findings = out, uses = length(uses), checked = checked,
       universes = universes)
}

check_remotes <- function(desc, parent_dir, offline, reachable) {
  out <- list()
  dev <- is_development_version(desc$version)

  if (!dev && desc$has_remotes_field) {
    out[[length(out) + 1L]] <- finding("P4", sprintf(
      "Version is %s -- a RELEASE version -- and `Remotes:` is present (%d entry/entries); CRAN rejects the field",
      desc$version, length(desc$remotes)
    ))
  }
  if (length(desc$remotes) == 0L) return(out)

  floors <- stats::setNames(
    vapply(desc$imports, `[[`, character(1), "version"),
    vapply(desc$imports, `[[`, character(1), "name")
  )

  for (rm in desc$remotes) {
    pkg <- basename(rm$repo)
    if (is.na(rm$ref)) {
      out[[length(out) + 1L]] <- finding("P4", sprintf(
        "`Remotes: %s` is UNPINNED -- it builds against whatever %s HEAD is that day",
        rm$raw, pkg
      ))
    }
    host_url <- switch(
      rm$host,
      github = sprintf("https://github.com/%s.git", rm$repo),
      gitlab = sprintf("https://gitlab.com/%s.git", rm$repo),
      NA_character_
    )
    if (is.na(host_url)) {
      out[[length(out) + 1L]] <- finding("P4", sprintf(
        "`Remotes: %s` names host `%s`, which this gate cannot resolve",
        rm$raw, rm$host
      ))
      next
    }
    if (!reachable || offline) next
    args <- c("ls-remote", "--exit-code", shQuote(host_url))
    if (!is.na(rm$ref)) args <- c(args, shQuote(rm$ref))
    code <- suppressWarnings(system2("git", args, stdout = FALSE, stderr = FALSE))
    if (!identical(as.integer(code), 0L)) {
      out[[length(out) + 1L]] <- finding("P4", sprintf(
        "`Remotes: %s` does not resolve: %s%s is not served (host default was %s)",
        rm$raw, host_url,
        if (is.na(rm$ref)) "" else sprintf(" at %s", rm$ref),
        if (grepl("::", rm$raw)) "explicit" else "GitHub, by omission"
      ))
      next
    }
    if (!is.na(rm$ref) && pkg %in% names(floors) && !is.na(floors[[pkg]])) {
      pinned <- sub("^v", "", rm$ref)
      if (grepl("^[0-9]+([.][0-9]+)+$", pinned) &&
          utils::compareVersion(pinned, floors[[pkg]]) < 0) {
        out[[length(out) + 1L]] <- finding("P4", sprintf(
          "`Remotes: %s` pins %s, below the `Imports:` floor of %s",
          rm$raw, pinned, floors[[pkg]]
        ))
      }
    }
  }
  out
}

network_reachable <- function() {
  con <- NULL
  ok <- tryCatch({
    con <- url(REACHABILITY_PROBE, open = "rb")
    length(readBin(con, "raw", n = 4L)) == 4L
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!is.null(con)) try(close(con), silent = TRUE)
  ok
}

check_repo <- function(root, offline = FALSE, reachable = NA) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  parent_dir <- dirname(root)
  desc <- read_description(root)
  if (is.na(reachable)) reachable <- offline || network_reachable()
  if (!reachable && !offline) {
    stop("the network is unreachable -- refusing to score, because every ",
         "remote would look broken", call. = FALSE)
  }

  floors <- check_floors(desc, parent_dir, offline)
  runtime <- check_symbols(desc, file.path(root, "R"), "P2", floors$universes,
                           parent_dir, offline, allow_guards = FALSE)
  tests <- check_symbols(desc, file.path(root, "tests"), "P3",
                         runtime$universes, parent_dir, offline,
                         allow_guards = TRUE)
  remotes <- check_remotes(desc, parent_dir, offline, reachable)

  list(
    package = desc$package,
    version = desc$version,
    development = is_development_version(desc$version),
    notes = floors$notes,
    counts = list(
      requirements = length(desc$imports) + length(desc$depends),
      runtime_uses = runtime$uses, test_uses = tests$uses,
      artifacts = runtime$checked + tests$checked,
      remotes = length(desc$remotes)
    ),
    findings = c(floors$findings, runtime$findings, tests$findings, remotes)
  )
}

# --- self-test ----------------------------------------------------------------

# The parsers and the comparisons are tested against real files and real
# expressions. Anything needing the network is tested through the pure function
# it feeds, so the self-test stays runnable on a disconnected machine.
self_test <- function() {
  fail <- function(msg) stop("self-test FAILED: ", msg, call. = FALSE)

  # POSITIVE: DESCRIPTION fields unfold across continuation lines.
  base <- tempfile("depgate-selftest-")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "Package: fixture", "Version: 1.1.1.9000",
    "Depends:", "    R (>= 4.1.0)",
    "Imports:", "    digest,", "    punycoder (>= 1.1.0),", "    utils",
    "Remotes:", "    gitlab::bart-turczynski/punycoder"
  ), file.path(base, "DESCRIPTION"))
  d <- read_description(base)
  if (!identical(d$package, "fixture")) fail("did not read Package")
  if (length(d$imports) != 3L) {
    fail(sprintf("read %d imports, expected 3", length(d$imports)))
  }
  if (!identical(d$imports[[2L]]$name, "punycoder") ||
      !identical(d$imports[[2L]]$version, "1.1.0")) {
    fail("did not split a floored requirement into name + version")
  }
  if (!is.na(d$imports[[1L]]$version)) fail("invented a floor for `digest`")

  # POSITIVE: the release/development predicate, which P4 turns on.
  if (!is_development_version("1.1.1.9000")) fail("read 1.1.1.9000 as a release")
  if (is_development_version("0.1.0")) fail("read 0.1.0 as a development tree")
  if (is_development_version("3.0.0")) fail("read 3.0.0 as a development tree")

  # POSITIVE + NEGATIVE: remote specs, including the default that is not neutral.
  r <- parse_remote("gitlab::bart-turczynski/punycoder")
  if (!identical(r$host, "gitlab") || !is.na(r$ref)) fail("misread a gitlab remote")
  r <- parse_remote("bart-turczynski/rurl@v2.2.1")
  if (!identical(r$host, "github")) fail("a bare owner/repo must default to GitHub")
  if (!identical(r$ref, "v2.2.1")) fail("did not read the pinned ref")

  # THE MEASURED FALSE POSITIVE. Written as a grep, this gate reported a comment
  # as a call site. The fixture keeps the exact shape that fooled it.
  f <- file.path(base, "prose.R")
  writeLines(c(
    "# `punycoder::unicode_versions()` already reports more than one version.",
    "x <- \"punycoder::not_a_call\"",
    "y <- punycoder::puny_encode(\"a\")"
  ), f)
  uses <- qualified_uses(base)
  syms <- vapply(uses, `[[`, character(1), "symbol")
  if (!identical(syms, "puny_encode")) {
    fail(sprintf("parsed call sites as: %s", paste(syms, collapse = ", ")))
  }

  # POSITIVE: a use nested inside a function body and an argument is still found.
  writeLines(c(
    "f <- function() { g(h(pslr::registrable_domain(x))) }"
  ), f)
  syms <- vapply(qualified_uses(base), `[[`, character(1), "symbol")
  if (!identical(syms, "registrable_domain")) {
    fail("missed a `::` call nested inside a function body")
  }

  # NEGATIVE, and a defect this gate actually had: R's EMPTY ARGUMENT. The hole
  # in `x[, 1]` is a symbol whose name is "", and asking `is.call()` about it
  # forces it and throws. The walker aborted on the first such line in `R/`, so
  # the live run reported "0 findings" plus an abort -- the exact vacuous shape
  # P5 exists to prevent. The original fixtures had no subscript in them.
  writeLines(c(
    "f <- function(x) x[, 1]",
    "g <- function(x) x[1, , drop = FALSE]",
    "h <- function(x) punycoder::puny_decode(x)"
  ), f)
  syms <- vapply(qualified_uses(base), `[[`, character(1), "symbol")
  if (!identical(syms, "puny_decode")) {
    fail(sprintf("empty-argument walk: parsed %s", paste(syms, collapse = ", ")))
  }
  unlink(f)

  # POSITIVE + NEGATIVE: NAMESPACE export surface, including exportPattern and
  # the deliberate exclusion of S3method.
  ns <- namespace_exports(paste(c(
    "export(puny_encode)", "export(host_normalize)",
    "S3method(print,thing)", "exportPattern(\"^ip_\")"
  ), collapse = "\n"))
  if (!isTRUE(is_exported(ns, "puny_encode"))) fail("missed a plain export")
  if (!isTRUE(is_exported(ns, "ip_parse"))) fail("missed an exportPattern match")
  if (!isFALSE(is_exported(ns, "print.thing"))) {
    fail("counted an S3 method as a `pkg::` export")
  }
  if (!isFALSE(is_exported(ns, "unicode_versions"))) {
    fail("reported an absent symbol as exported")
  }

  # NON-VACUITY: an HTML error page cached under a tarball's name. This is the
  # trap that was actually sprung while building the gate.
  html <- file.path(base, "fake.tar.gz")
  writeLines("<!DOCTYPE html><html><body>404 Not Found</body></html>", html)
  if (is_gzip(html)) fail("accepted an HTML error page as a gzip archive")

  # NEGATIVE: skip guards. A guard with no version never fires, so it must not
  # excuse anything; a guard above the artifact version must.
  g <- file.path(base, "test-guard.R")
  writeLines(c(
    "skip_if_not_installed(\"punycoder\", \"1.3.0\")",
    "testthat::skip_if_not_installed(\"pslr\")",
    "z <- punycoder::unicode_versions()"
  ), g)
  guards <- skip_guards(g)
  if (!identical(unname(guards[["punycoder"]]), "1.3.0")) {
    fail("did not read a versioned skip guard")
  }
  if (!is.na(guards[["pslr"]])) fail("invented a version for an unversioned guard")
  unlink(g)

  # NEGATIVE: floor satisfiability, exercised through the comparison rather than
  # the network. 3.0.0 against a universe topping out at 2.2.1 is pagerankr's
  # live defect; 1.2.0 against {1.1.0, 1.2.1} is rurl's non-defect.
  universe <- c("1.2.0", "2.2.0", "2.2.1")
  if (length(Filter(function(v) utils::compareVersion(v, "3.0.0") >= 0,
                    universe)) != 0L) {
    fail("found a satisfying version for an unsatisfiable floor")
  }
  universe <- c("1.1.0", "1.2.1")
  if (length(Filter(function(v) utils::compareVersion(v, "1.2.0") >= 0,
                    universe)) != 1L) {
    fail("did not satisfy a floor whose exact version was never released")
  }

  # NON-VACUITY: a DESCRIPTION with no Package/Version aborts.
  writeLines(c("Title: nothing"), file.path(base, "DESCRIPTION"))
  if (!inherits(try(read_description(base), silent = TRUE), "try-error")) {
    fail("scored a DESCRIPTION with no Package/Version instead of aborting")
  }

  # NON-VACUITY: an unparseable R file aborts rather than contributing no uses.
  writeLines(c("Package: fixture", "Version: 1.0.0"),
             file.path(base, "DESCRIPTION"))
  writeLines("f <- function( {", file.path(base, "broken.R"))
  if (!inherits(try(qualified_uses(base), silent = TRUE), "try-error")) {
    fail("scored a directory containing an unparseable R file")
  }

  unlink(base, recursive = TRUE)
  cat("dependency-resolvability-gate self-test: PASS",
      "(12 positive + 10 negative cases)\n")
  invisible(TRUE)
}

# --- main ---------------------------------------------------------------------

report <- function(res) {
  cat(sprintf("\n%s %s (%s tree)\n", res$package, res$version,
              if (res$development) "development" else "release"))
  cat(sprintf(
    "  %d requirement(s), %d `::` use(s) in R/, %d in tests/, %d released artifact(s) read, %d remote(s)\n",
    res$counts$requirements, res$counts$runtime_uses, res$counts$test_uses,
    res$counts$artifacts, res$counts$remotes
  ))
  for (n in res$notes) cat("  note: ", n, "\n", sep = "")
  if (length(res$findings) == 0L) {
    # A green with nothing read is a green about nothing. Say which one it is,
    # so a package whose dependencies are all `importFrom`/`LinkingTo` is not
    # mistaken for one whose released symbols were checked and found present.
    if (res$counts$artifacts == 0L) {
      cat("  PASS (weak: no `pkg::` call site resolved to a released",
          "artifact, so P2/P3 had no population here)\n")
    } else {
      cat("  PASS\n")
    }
    return(invisible(TRUE))
  }
  for (f in res$findings) cat(sprintf("  %s  %s\n", f$prop, f$text))
  invisible(FALSE)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
    return(invisible(TRUE))
  }
  offline <- "--offline" %in% args

  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if ("--repo" %in% args) {
    i <- which(args == "--repo")[[1L]]
    if (i == length(args)) stop("--repo needs a path", call. = FALSE)
    root <- normalizePath(args[[i + 1L]], winslash = "/", mustWork = TRUE)
  }

  roots <- root
  if ("--all" %in% args) {
    parent_dir <- dirname(root)
    candidates <- list.dirs(parent_dir, recursive = FALSE)
    roots <- Filter(function(d) file.exists(file.path(d, "DESCRIPTION")),
                    candidates)
    if (length(roots) < 2L) {
      stop(sprintf("--all found %d package(s) next to %s -- refusing to score",
                   length(roots), root), call. = FALSE)
    }
  }

  reachable <- offline || network_reachable()
  if (!reachable && !offline) {
    stop("the network is unreachable -- refusing to score, because every ",
         "remote would look broken", call. = FALSE)
  }

  cat("Dependency-resolvability gate\n")
  total <- 0L
  failed <- character(0)
  for (r in roots) {
    res <- tryCatch(check_repo(r, offline = offline, reachable = reachable),
                    error = function(e) e)
    if (inherits(res, "error")) {
      cat(sprintf("\n%s\n  ABORT: %s\n", basename(r), conditionMessage(res)))
      failed <- c(failed, basename(r))
      next
    }
    report(res)
    total <- total + length(res$findings)
    if (length(res$findings) > 0L) failed <- c(failed, res$package)
  }

  cat(sprintf("\n%d finding(s) across %d package(s)\n", total, length(roots)))
  if (length(failed) > 0L) {
    stop(sprintf("dependency resolvability fails in: %s",
                 paste(unique(failed), collapse = ", ")), call. = FALSE)
  }
  cat("PASS: every floor is satisfiable, every `::` symbol is released, and\n",
      "      every `Remotes:` entry is pinned and resolves.\n", sep = "")
  invisible(TRUE)
}

if (identical(environment(), globalenv()) && !interactive()) {
  if (sys.nframe() == 0L) {
    main()
  }
}
