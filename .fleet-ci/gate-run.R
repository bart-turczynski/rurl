# bin/gate-run.R STEP: the part of bin/gate that runs inside the fleet image.
#
# bin/gate starts it as `fleet-tools Rscript /fleet/gate-run.R STEP` with the
# pushed tree exported (blob by blob) at /src, an output directory at /out
# holding config.json (the validated .fleet-ci.yml), and the shared pak
# download cache at R_PKG_CACHE_DIR. It never sees a .git directory, an
# untracked file or the maintainer's R library. Steps:
#
#   resolve  resolve the package's dependencies into /out/pkg.lock
#   docs     the docs tier: .fleet-ci.yml's docs_gates, then check_pkgdown()
#   full     the full tier: locales, the dependency install from
#            /out/pkg.lock into a private run library, the docs gates, the
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

if (step == "docs") {
  for (cmd in cfg$docs_gates) run_gate("docs", cmd)
  if (file.exists("_pkgdown.yml")) {
    run_gate("docs", "Rscript -e 'pkgdown::check_pkgdown()'")
  }
  save_status()
  quit(status = 0L)
}

# --- full ---------------------------------------------------------------------

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
    name <- sub("[.].*$", "", l)
    charmap <- sub("^[^.]*[.]", "", l)
    code <- system2("localedef", c("-c", "-i", name, "-f", charmap, file.path(locpath, l)))
    # localedef -c exits 1 when it wrote the locale despite a warning.
    if (code > 1L) stop("localedef could not build ", l, " (exit ", code, ")", call. = FALSE)
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
if (file.exists("_pkgdown.yml")) {
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
