# bin/gate-run.R STEP: the part of bin/gate that runs inside the fleet image.
#
# bin/gate starts it as `fleet-tools Rscript /fleet/gate-run.R STEP` with the
# pushed tree exported (blob by blob) at /src, an output directory at /out
# holding config.json (the validated .fleet-ci.yml), and the shared pak
# download cache at R_PKG_CACHE_DIR. It never sees a .git directory, an
# untracked file or the maintainer's R library. Steps:
#
#   resolve  resolve the package's dependencies into /out/pkg.lock
#   docs     the docs tier: .fleet-ci.yml's docs_gates, then the pages job's
#            destination rule and check_pkgdown()
#   full     the full tier: locales, the dependency install from
#            /out/pkg.lock into a private run library, the docs tier, the
#            cheap gates, one R CMD check through fleet_check(), and, as
#            t2-locale: one more check per `locale_legs` entry (default C
#            when there are locale assertions) under LC_ALL=<leg>, the
#            locale assertions against it
#
# It runs as the invoking user (docker run --user), not root.
#
# Every gate runs and its result is recorded in /out/status.json; bin/gate
# reads that file and the check directory (/out/check) and decides. This
# script exits 0 when it recorded a result, whatever the result, and non-zero
# only when it could not run.

args <- commandArgs(trailingOnly = TRUE)
step <- if (length(args) == 1L) args[[1L]] else ""
if (!step %in% c("resolve", "docs", "full")) {
  stop("usage: gate-run.R resolve|docs|full", call. = FALSE)
}
if (!exists("fleet_check")) {
  stop("fleet_check() is not attached: run under fleet-tools", call. = FALSE)
}

src <- "/src"
out <- "/out"
cfg <- jsonlite::read_json(file.path(out, "config.json"), simplifyVector = TRUE)
status <- list(step = step, gates = list(), check = NULL)
started <- Sys.time()

say <- function(...) cat("gate-run: ", ..., "\n", sep = "")

save_status <- function() {
  status$seconds <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
  jsonlite::write_json(status, file.path(out, "status.json"), auto_unbox = TRUE, null = "null", pretty = TRUE)
}

# A gate command: sh -c in the export, with the tooling library reachable
# through R's profile the way the fleet-tools wrapper gives it, and run_lib
# (when there is one) on R_LIBS. Records name, exit status and seconds.
run_gate <- function(kind, cmd, run_lib = NULL, extra = character()) {
  say(kind, ": ", cmd, if (length(extra)) paste0(" (", paste(extra, collapse = " "), ")"))
  t0 <- Sys.time()
  env <- c(
    "R_PROFILE_USER=/opt/fleet-tools/Rprofile",
    "FLEET_TOOLS_LIB=/opt/fleet-tools/library",
    if (!is.null(run_lib)) paste0("R_LIBS=", run_lib),
    shQuote(extra)
  )
  code <- system2("env", c(env, "sh", "-c", shQuote(cmd)), stdin = "/dev/null")
  status$gates[[length(status$gates) + 1L]] <<- list(
    kind = kind, cmd = cmd, exit = code,
    seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  )
  invisible(code == 0L)
}

setwd(src)

if (step == "resolve") {
  # Resolved against an empty library: with the default, every package the
  # tooling library holds would be recorded as already installed, and the
  # run library would never get it.
  empty <- tempfile("empty-lib-")
  dir.create(empty)
  pak::lockfile_create(
    "deps::/src", lockfile = file.path(out, "pkg.lock"), lib = empty, dependencies = TRUE
  )
  say("dependencies resolved into ", file.path(out, "pkg.lock"))
  save_status()
  quit(status = 0L)
}

# The directory pkgdown builds the site into, relative to the package root:
# the top-level `destination` of the first pkgdown config file pkgdown 2.x
# reads (_pkgdown.yml, _pkgdown.yaml, then each under pkgdown/ and inst/),
# default docs. It stops on an absolute destination, one outside the package
# (by .. or a symbolic link, a dangling one included), the package root
# itself, or public or .fleet-agent-md or inside either (lexically or
# through a link). templates/
# r-package.yml's pages job has the same functions (fleet_pkgdown_config(),
# fleet_pages_dst(), fleet_pages_devdst()) and publishes that directory;
# bin/selftest checks each pair is one function and runs them.
#
# The first pkgdown config file pkgdown 2.x reads, as list(yml, cfg): its
# name (default _pkgdown.yml) and its parsed contents (list() without one).
fleet_pkgdown_config <- function() {
  cfgs <- c("_pkgdown.yml", "_pkgdown.yaml", "pkgdown/_pkgdown.yml", "pkgdown/_pkgdown.yaml",
            "inst/_pkgdown.yml", "inst/_pkgdown.yaml")
  cfgs <- cfgs[file.exists(cfgs)]
  yml <- if (length(cfgs)) cfgs[[1L]] else "_pkgdown.yml"
  list(yml = yml, cfg = if (length(cfgs)) yaml::read_yaml(yml) else list())
}

fleet_pages_dst <- function() {
  pc <- fleet_pkgdown_config()
  yml <- pc$yml
  cfg <- pc$cfg
  dst <- if (is.null(cfg$destination)) "docs" else cfg$destination
  if (!is.character(dst) || length(dst) != 1L || is.na(dst) || !nzchar(dst)) {
    stop("fleet-ci: ", yml, " destination must be one non-empty path", call. = FALSE)
  }
  if (startsWith(path.expand(dst), "/")) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " is an absolute path; give it relative to the package root", call. = FALSE)
  }
  root <- normalizePath(".", mustWork = TRUE)
  parts <- character()
  for (p in strsplit(file.path(root, dst), "/", fixed = TRUE)[[1L]]) {
    if (p == "..") parts <- head(parts, -1L) else if (nzchar(p) && p != ".") parts <- c(parts, p)
  }
  root_parts <- Filter(nzchar, strsplit(root, "/", fixed = TRUE)[[1L]])
  n <- length(root_parts)
  if (length(parts) <= n || !identical(parts[seq_len(n)], root_parts)) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " is not a directory inside the package", call. = FALSE)
  }
  public <- file.path(root, "public")
  in_public <- function(p) p == public || startsWith(p, paste0(public, "/"))
  rel <- paste(parts[-seq_len(n)], collapse = "/")
  if (in_public(file.path(root, rel))) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " is public or inside it, the directory the pages job publishes", call. = FALSE)
  }
  moved <- file.path(root, ".fleet-agent-md")
  in_moved <- function(p) p == moved || startsWith(p, paste0(moved, "/"))
  if (in_moved(file.path(root, rel))) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " is .fleet-agent-md or inside it, where the pages job moves unpublished .md files", call. = FALSE)
  }
  # The deepest component that exists, a dangling symbolic link included,
  # resolved; the components below it do not exist yet, so they cannot be links.
  # Sys.readlink gives "" for a path that is not a link, NA for a missing one.
  seen <- rel
  while (seen != "." && !file.exists(seen) && Sys.readlink(seen) %in% c("", NA)) seen <- dirname(seen)
  if (seen != "." && !file.exists(seen)) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " goes through a symbolic link that does not resolve", call. = FALSE)
  }
  rest <- if (seen == ".") rel else substring(rel, nchar(seen) + 2L)
  real <- if (seen == ".") root else normalizePath(seen, mustWork = TRUE)
  if (nzchar(rest)) real <- file.path(real, rest)
  if (!startsWith(real, paste0(root, "/"))) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " resolves through a symbolic link outside the package or to its root", call. = FALSE)
  }
  if (in_public(real)) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " resolves through a symbolic link to public or inside it", call. = FALSE)
  }
  if (in_moved(real)) {
    stop("fleet-ci: ", yml, " destination ", dQuote(dst, FALSE),
         " resolves through a symbolic link to .fleet-agent-md or inside it", call. = FALSE)
  }
  rel
}

# The development.destination of the same config file, where pkgdown puts
# the site in its devel and unreleased modes, relative to the destination,
# default dev; it must stay inside the destination. The mode is
# PKGDOWN_DEV_MODE when set, as pkgdown reads it, else development.mode. In
# any other mode (release, or none given) pkgdown never uses the value, so
# it is not checked and dev is returned.
fleet_pages_devdst <- function() {
  pc <- fleet_pkgdown_config()
  yml <- pc$yml
  devcfg <- if (is.list(pc$cfg$development)) pc$cfg$development else list()
  mode <- Sys.getenv("PKGDOWN_DEV_MODE")
  if (!nzchar(mode) && is.character(devcfg$mode) && length(devcfg$mode) == 1L) mode <- devcfg$mode
  if (!mode %in% c("devel", "unreleased")) return("dev")
  dev <- devcfg$destination
  if (is.null(dev)) dev <- "dev"
  if (!is.character(dev) || length(dev) != 1L || is.na(dev) || !nzchar(dev)) {
    stop("fleet-ci: ", yml, " development destination must be one non-empty path", call. = FALSE)
  }
  if (startsWith(path.expand(dev), "/")) {
    stop("fleet-ci: ", yml, " development destination ", dQuote(dev, FALSE),
         " is an absolute path; give it relative to the destination", call. = FALSE)
  }
  parts <- character()
  escaped <- FALSE
  for (p in strsplit(dev, "/", fixed = TRUE)[[1L]]) {
    if (p == "..") {
      if (!length(parts)) escaped <- TRUE
      parts <- head(parts, -1L)
    } else if (nzchar(p) && p != ".") {
      parts <- c(parts, p)
    }
  }
  if (escaped || !length(parts)) {
    stop("fleet-ci: ", yml, " development destination ", dQuote(dev, FALSE),
         " is not a directory inside the destination", call. = FALSE)
  }
  paste(parts, collapse = "/")
}

# The pages job's destination rules, as docs gates that only warn: a
# refused destination or development destination is recorded with exit 0
# and a WARNING, so a package on an earlier fleet-ci keeps passing its gate.
# The pages job itself refuses.
check_pages_dst <- function() {
  ok <- TRUE
  for (what in c("destination", "development destination")) {
    say("docs: pkgdown ", what)
    t0 <- Sys.time()
    fn <- if (what == "destination") fleet_pages_dst else fleet_pages_devdst
    res <- tryCatch(fn(), error = function(e) e)
    msg <- if (inherits(res, "error")) {
      paste0("WARNING: ", conditionMessage(res), "; the pages job will refuse it")
    } else {
      paste0("pkgdown ", what, " ", res)
    }
    say("docs: ", msg)
    status$gates[[length(status$gates) + 1L]] <<- list(
      kind = "docs", cmd = msg, exit = 0L,
      seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    )
    ok <- ok && !inherits(res, "error")
  }
  invisible(ok)
}

# The pkgdown config files pkgdown 2.x reads, as fleet_pkgdown_config() lists them.
pkgdown_cfgs <- c("_pkgdown.yml", "_pkgdown.yaml", "pkgdown/_pkgdown.yml", "pkgdown/_pkgdown.yaml",
                  "inst/_pkgdown.yml", "inst/_pkgdown.yaml")

if (step == "docs") {
  for (cmd in cfg$docs_gates) run_gate("docs", cmd)
  if (any(file.exists(pkgdown_cfgs))) {
    check_pages_dst()
    run_gate("docs", "Rscript -e 'pkgdown::check_pkgdown()'")
  }
  save_status()
  quit(status = 0L)
}

# --- full ---------------------------------------------------------------------

# The glibc source localedef -i reads for locale l: the name before the dot
# with l's @modifier (sr_RS.UTF-8@latin: sr_RS@latin).
fleet_source <- function(l) {
  s <- sub("[.].*$", "", sub("@.*$", "", l))
  if (grepl("@", l, fixed = TRUE)) s <- paste0(s, "@", sub("^[^@]*@", "", l))
  s
}

# The charmap localedef -f reads for locale l: the codeset between the dot
# and any @modifier under its glibc charmap name. ISO8859-N and iso8859N (any
# case, dash or not) are ISO-8859-N, utf8 and UTF8 are UTF-8, eucJP and
# EUC-JP are EUC-JP, anything else is passed as written; localedef given a
# name it has no charmap for still exits 0, with an ASCII locale.
# templates/r-package.yml's t2-locale has the same mappings as shell
# functions, fleet_source and fleet_charmap; bin/selftest runs both over
# tests/charmap-table.txt.
fleet_charmap <- function(l) {
  cs <- sub("^[^.]*[.]", "", sub("@.*$", "", l))
  if (grepl("^utf-?8$", cs, ignore.case = TRUE)) return("UTF-8")
  if (grepl("^euc-?jp$", cs, ignore.case = TRUE)) return("EUC-JP")
  if (grepl("^iso8859-?[0-9]{1,2}$", cs, ignore.case = TRUE)) {
    return(sub("^iso8859-?", "ISO-8859-", cs, ignore.case = TRUE))
  }
  cs
}

# The name glibc also looks a locale up under: the codeset normalized as
# _nl_normalize_codeset() does it (letters lowered, punctuation dropped, a
# digits-only codeset prefixed with "iso"). setlocale() asked for
# tr_TR.ISO8859-9 tries the directories tr_TR.ISO8859-9 and tr_TR.iso88599,
# but asked for tr_TR.iso88599 only the latter; the locale archive files a
# locale under both names, a LOCPATH directory only under the one it was
# built as.
fleet_locale_alias <- function(l) {
  m <- regmatches(l, regexec("^([^.@]+)[.]([^@]+)(@.*)?$", l))[[1L]]
  if (!length(m)) return(l)
  cs <- tolower(gsub("[^[:alnum:]]", "", m[[3L]]))
  if (grepl("^[0-9]+$", cs)) cs <- paste0("iso", cs)
  paste0(m[[2L]], ".", cs, m[[4L]])
}

# The container runs as the invoking user, not root, so the locales are
# compiled into a directory of their own and found through LOCPATH, which
# every process started from here inherits. glibc then reads compiled
# locales from LOCPATH and the default directory (C.UTF-8 among them), not
# the system's locale archive: a locale the package's checks need belongs in
# `locales`.
if (length(cfg$locales)) {
  locpath <- "/tmp/fleet-locales"
  dir.create(locpath, showWarnings = FALSE)
  for (l in cfg$locales) {
    code <- system2("localedef", c("-c", "-i", fleet_source(l), "-f", fleet_charmap(l), file.path(locpath, l)))
    # localedef -c exits 1 when it wrote the locale despite a warning.
    if (code > 1L) stop("localedef could not build ", l, " (exit ", code, ")", call. = FALSE)
  }
  # Built under the name the package wrote, each also answers to glibc's
  # normalized name, as it does from the archive in t2-locale.
  for (l in cfg$locales) {
    alias <- fleet_locale_alias(l)
    if (!file.exists(file.path(locpath, alias))) file.symlink(l, file.path(locpath, alias))
  }
  Sys.setenv(LOCPATH = locpath)
}

run_lib <- "/tmp/fleet-run-lib"
dir.create(run_lib)
# With the tooling library on the path pak would take its packages as
# installed and leave them out of run_lib. pak's worker process needs pak
# itself (self-contained: it bundles its own dependencies), so the install
# runs with run_lib and a library holding pak alone.
pak_only <- tempfile("pak-only-")
dir.create(pak_only)
file.symlink(file.path(Sys.getenv("FLEET_TOOLS_LIB", "/opt/fleet-tools/library"), "pak"), file.path(pak_only, "pak"))
tools_paths <- .libPaths()
.libPaths(c(run_lib, pak_only))
pak::lockfile_install(file.path(out, "pkg.lock"), lib = run_lib, update = TRUE)
.libPaths(tools_paths)
say("dependencies installed into the private run library ", run_lib)

for (cmd in cfg$docs_gates) run_gate("docs", cmd, run_lib)
if (any(file.exists(pkgdown_cfgs))) {
  check_pages_dst()
  run_gate("docs", "Rscript -e 'pkgdown::check_pkgdown()'", run_lib)
}
for (cmd in cfg$cheap_gates) run_gate("cheap", cmd, run_lib)

manual <- isTRUE(cfg$manual)
check_dir <- file.path(out, "check")
say(
  "R CMD check: NOT_CRAN=", Sys.getenv("NOT_CRAN"),
  " _R_CHECK_CRAN_INCOMING_=", Sys.getenv("_R_CHECK_CRAN_INCOMING_"),
  " _R_CHECK_CRAN_INCOMING_REMOTE_=", Sys.getenv("_R_CHECK_CRAN_INCOMING_REMOTE_"),
  " manual=", manual
)
t0 <- Sys.time()
res <- tryCatch(
  fleet_check(
    src, run_lib,
    args = c(if (!manual) "--no-manual", "--as-cran"),
    build_args = c(if (!manual) "--no-manual", "--compact-vignettes=gs+qpdf"),
    check_dir = check_dir, error_on = "warning",
    env = c(R_LIBS = run_lib, R_LIBS_SITE = "NULL", R_LIBS_USER = "NULL")
  ),
  error = function(e) e
)
check <- list(seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1))
if (inherits(res, "error")) {
  check$error <- conditionMessage(res)
  say("R CMD check: ", check$error)
} else {
  check$exit <- as.integer(res$status)
  check$errors <- length(res$errors)
  check$warnings <- length(res$warnings)
  check$notes <- length(res$notes)
  # Each NOTE's text, for allowed_notes; I() keeps a single one an array.
  check$note_texts <- I(as.character(res$notes))
  print(res)
}
status$check <- check

legs <- unlist(cfg$locale_legs)
# Without `locale_legs` the assertions run where t2-locale runs them: one
# LC_ALL=C leg with its own check, never against the main C.UTF-8 check.
if (!length(legs) && length(cfg$locale_assertions)) legs <- "C"
if (length(legs)) {
  # As t2-locale does: one check per leg under LC_ALL=<leg>, into its own
  # directory, then every locale assertion against that leg's .Rcheck, with
  # the same LC_ALL. bin/gate requires a complete, clean check per leg.
  status$legs <- list()
  old_lc <- Sys.getenv("LC_ALL")
  for (leg in legs) {
    say("locale leg LC_ALL=", leg)
    leg_dir <- file.path(out, "legs", leg)
    Sys.setenv(LC_ALL = leg)
    lres <- tryCatch(
      fleet_check(
        src, run_lib,
        args = c("--no-manual", "--ignore-vignettes"),
        build_args = c("--no-manual", "--no-build-vignettes"),
        check_dir = leg_dir, error_on = "never",
        env = c(R_LIBS = run_lib, R_LIBS_SITE = "NULL", R_LIBS_USER = "NULL")
      ),
      error = function(e) e
    )
    Sys.setenv(LC_ALL = old_lc)
    rec <- list()
    if (inherits(lres, "error")) {
      rec$error <- conditionMessage(lres)
    } else {
      rec$exit <- as.integer(lres$status)
      rec$errors <- length(lres$errors)
      rec$warnings <- length(lres$warnings)
    }
    leg_rcheck <- Sys.glob(file.path(leg_dir, "*.Rcheck"))
    leg_log <- file.path(leg_rcheck, "00check.log")
    rec$complete <- length(leg_rcheck) == 1L && file.exists(leg_log) &&
      any(grepl("^Status:", readLines(leg_log, warn = FALSE)))
    rec$assertions <- 0L
    for (cmd in cfg$locale_assertions) {
      run_gate("locale", cmd, run_lib, c(
        paste0("LC_ALL=", leg),
        paste0("FLEET_RCHECK_DIR=", if (length(leg_rcheck) == 1L) leg_rcheck else "")
      ))
      rec$assertions <- rec$assertions + 1L
    }
    status$legs[[leg]] <- rec
  }
}
save_status()
