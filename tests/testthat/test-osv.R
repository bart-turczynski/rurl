# OSV (Open Source Vulnerabilities) audit of rurl's runtime dependencies.
#
# Companion to the OSS Index audit (test-security.R), but with no account or
# token: OSV is free, unauthenticated, and has first-class CRAN coverage (the
# RSEC / R Consortium advisory feed). This checks rurl's *runtime* dependency
# closure -- the recursive Depends + Imports that actually ship to users, not
# dev/Suggests packages -- at their installed versions, via {rosv}.
#
# `rosv::osv_query()` is version-aware: for a given (package, version) it
# returns zero rows when that version is unaffected, so a non-empty result is
# a genuine advisory against the installed version. It is a network test, so
# it skips on CRAN, offline, or when rosv is not installed. A dedicated
# GitHub workflow (osv-audit.yml) used to run it weekly and on demand to
# drive the README badge; that workflow was deleted with RURL-vunvxusf (the
# account is suspended).
#
# It is EXCLUDED from the pre-push gate's test_local() cell (SEOR-fftbjnpl):
# test_local() sets NOT_CRAN=true, so skip_on_cran() does not fire there, and
# a new upstream advisory would block an unrelated push. `R CMD check` leaves
# NOT_CRAN unset and skips it. So nothing runs it automatically: run it
# deliberately with `testthat::test_local(filter = "osv")`.

test_that("runtime dependencies have no known OSV vulnerabilities", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("rosv")

  # installed.packages() is the standard dependency database for recursive
  # resolution; its "slow" caveat is irrelevant in a weekly CI audit, and it
  # works offline (unlike available.packages()).
  db <- utils::installed.packages() # nolint: installed_packages_linter.
  priority <- db[, "Priority"]
  base_pkgs <- rownames(db)[!is.na(priority) & priority == "base"]
  closure <- tools::package_dependencies(
    "rurl",
    db = db,
    which = c("Depends", "Imports"),
    recursive = TRUE
  )[[1]]
  deps <- setdiff(unique(closure), c(base_pkgs, "R"))
  skip_if(length(deps) == 0, "no resolvable runtime dependencies")

  advisories <- character()
  for (pkg in deps) {
    version <- unname(db[pkg, "Version"])
    hits <- rosv::osv_query(pkg, version = version, ecosystem = "CRAN")
    if (nrow(hits) > 0) {
      advisories <- c(
        advisories,
        sprintf("%s %s: %s", pkg, version, toString(unique(hits$id)))
      )
    }
  }

  expect_identical(
    advisories,
    character(),
    info = paste0(
      "OSV advisories against installed runtime dependencies:\n",
      paste(advisories, collapse = "\n")
    )
  )
})
