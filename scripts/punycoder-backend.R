#!/usr/bin/env Rscript
#
# Rebuild punycoder from source into a run library, then assert which backend
# it was built with (SEOR-hxxxkmws, RURL-woljcpfu).
#
#   Rscript scripts/punycoder-backend.R LIB libidn2    # the local gate
#   Rscript scripts/punycoder-backend.R LIB fallback   # x-check-no-idn2 in CI
#
# punycoder's configure builds its libidn2 backend when pkg-config finds
# libidn2, as on CRAN's Debian flavors, and its in-tree fallback otherwise, as
# on CRAN's Windows flavors. The two builds can decode differently: rurl
# 3.1.0's first upload failed CRAN's Debian pre-test on a probe test that only
# the libidn2 build broke. So rurl checks against both builds.
#
# WHY A REBUILD. The fleet images resolve dependencies from Posit Package
# Manager binaries, and its punycoder binary is built without libidn2, so the
# dependency install alone gives the fallback build even in fleet-r-idn2,
# which carries libidn2-dev for exactly this. This replaces the run library's
# punycoder with a source build of the same version, from the same dated
# snapshot's source repository, so the image decides the backend: fleet-r-idn2
# builds libidn2, fleet-r the fallback.
#
# The assertion is what keeps the cell honest: it fails when the rebuilt
# punycoder reports the other backend, or when the rebuild changed its
# version.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !args[[2L]] %in% c("libidn2", "fallback")) {
  stop("usage: Rscript scripts/punycoder-backend.R LIB libidn2|fallback",
       call. = FALSE)
}
lib <- args[[1L]]
want <- args[[2L]]
if (!nzchar(lib) || !dir.exists(lib)) {
  stop("run library '", lib, "' does not exist", call. = FALSE)
}
# The run library first, so the build finds Rcpp's headers and the load finds
# punycoder's imports there, under fleet-tools in CI as in the gate.
.libPaths(c(lib, .libPaths()))

before <- as.character(utils::packageVersion("punycoder", lib.loc = lib))
binary_repo <- getOption("repos")[["CRAN"]]
source_repo <- sub("/__linux__/[^/]+/", "/", binary_repo)
cat(sprintf("punycoder-backend: rebuilding punycoder %s from source (%s)\n",
            before, source_repo))
utils::install.packages("punycoder", lib = lib, repos = source_repo,
                        type = "source", dependencies = FALSE)

after <- as.character(utils::packageVersion("punycoder", lib.loc = lib))
if (!identical(after, before)) {
  stop(sprintf("the source build is punycoder %s, not the resolved %s",
               after, before), call. = FALSE)
}
b <- utils::getFromNamespace(".backend_info", loadNamespace("punycoder", lib.loc = lib))()
print(b)
got <- if (isTRUE(b$has_libidn2)) "libidn2" else "fallback"
if (!identical(got, want)) {
  stop(sprintf(paste("punycoder %s was built with the %s backend, so this",
                     "is not the %s cell (SEOR-hxxxkmws)"), after, got, want),
       call. = FALSE)
}
cat(sprintf("punycoder-backend: punycoder %s, %s backend, as wanted\n",
            after, got))
