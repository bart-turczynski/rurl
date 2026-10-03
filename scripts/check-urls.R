#!/usr/bin/env Rscript

# scripts/check-urls.R -- the `urls` stage of the pre-push gate.
#
#   Rscript scripts/check-urls.R              # fetch every declared URL
#   Rscript scripts/check-urls.R --self-test  # the offline classifier test only
#
# Fetches every URL the package declares and fails on a dead one, which
# nothing else in the gate does. The `verify` hook's `R CMD check --as-cran`
# does fetch them -- `checking CRAN incoming feasibility` calls this same
# check_url_db() -- but a dead link there is only a NOTE, and the gate fails
# on warnings, not notes (`error_on = "warning"`). The fleet standard adds this
# stage to every package (seor design/fleet-standard.md, "Local pre-push
# gate"). Copied from punycoder's scripts/check-urls.R, itself ported from
# sitemapr's `urls` stage in tools/verify.R; the three functions below are
# sitemapr's verify_classify_urls(), verify_url_self_test() and verify_urls(),
# unchanged apart from these comments.
#
# It runs as its own pre-push hook (`check-urls` in .pre-commit-config.yaml)
# ahead of `verify`, so a dead link is reported in seconds rather than after
# the full check.

# Sort the rows `tools:::check_url_db()` returns into what they mean for the
# gate: "red" (a real defect: fail), "warn" (no answer at all: report, pass) or
# "exempt" (the one known, deliberate 404). Pure and offline, so
# verify_url_self_test() can pin it with a constructed data frame.
#
# check_url_db() returns only the URLs it objects to, one row each. Its
# `Status` column holds the HTTP status as a string when the server answered,
# and the literal "Error" when no HTTP exchange happened at all -- DNS failure,
# refused connection, timeout -- with libcurl's message in `Message`. A row can
# also carry no status and a static complaint instead (`Message` "Empty URL" or
# "Invalid URI scheme", or a non-empty `New`/`CRAN`/`Spaces`/`R` column: moved
# permanently, a non-canonical CRAN link, a space, an http:// r-project link).
# Those are defects in the text the package declares, and R CMD check reports
# them the same way it reports a 404.
#
# So only "Error" rows with no static complaint are downgraded to a warning: a
# transient network blip must never reject a push. Everything else
# check_url_db() returns is red, except the exemption below. That includes 403,
# which R CMD check's incoming step drops by default
# (`_R_CHECK_URLS_TAKE_403_STATUS_AS_OK_`) because bot-shy hosts answer it;
# GitLab answers 403, not 404, for a project that does not exist, so here it
# stays a failure.
#
# The exemption. DESCRIPTION's BugReports keeps GitLab's `/-/issues` form,
# which 404s for a signed-out client since GitLab moved issues to
# `/-/work_items`. That is a fleet decision, not a defect: CRAN's incoming
# check string-tests BugReports for `/issues` (SEOR-ocbtrrnl;
# scripts/check-bugreports.py holds the split). So exactly that URL, in exactly
# that form, answering exactly 404, is exempt. Nothing else is.
verify_classify_urls <- function(bad, bugreports = NA_character_) {
  absent <- setdiff(c("URL", "Status", "Message"), names(bad))
  if (length(absent)) {
    stop(
      sprintf(
        "tools:::check_url_db() returned no %s column(s); its result shape ",
        toString(absent)
      ),
      "changed in this R release, so the URL check cannot be read.",
      call. = FALSE
    )
  }

  n <- nrow(bad)
  complaint <- function(col) {
    if (col %in% names(bad)) nzchar(bad[[col]]) else logical(n)
  }
  static <- complaint("New") |
    complaint("CRAN") |
    complaint("Spaces") |
    complaint("R")

  issues <- bugreports[
    !is.na(bugreports) & grepl("/-/issues/?$", bugreports)
  ]

  kind <- rep("red", n)
  kind[bad$Status == "Error" & !static] <- "warn"
  kind[bad$Status == "404" & bad$URL %in% issues & !static] <- "exempt"
  kind
}

# Pins verify_classify_urls() offline. verify_urls() runs it before it touches
# the network, so a classifier edit that would wave a dead link through fails
# the gate on the spot.
verify_url_self_test <- function() {
  br <- "https://gitlab.com/o/p/-/issues"
  wi <- "https://gitlab.com/o/p/-/work_items"
  row <- function(url, status, message = "", new = "") {
    data.frame(
      URL = url,
      Status = status,
      Message = message,
      New = new,
      CRAN = "",
      Spaces = "",
      R = "",
      stringsAsFactors = FALSE
    )
  }
  cases <- list(
    list(row(br, "404", "Not Found"), br, "exempt"),
    list(row(br, "403", "Forbidden"), br, "red"), # the exemption is 404 only
    list(row(wi, "404", "Not Found"), wi, "red"), # and /-/issues form only
    list(row(br, "404", "Not Found"), NA_character_, "red"), # not BugReports
    list(row("https://x.example/", "404", "Not Found"), br, "red"),
    list(row("https://x.example/", "500", "Server Error"), br, "red"),
    list(row("https://nonexistent.invalid/", "Error", "error 6"), br, "warn"),
    list(row("https://10.255.255.1/", "Error", "error 28"), br, "warn"),
    list(
      row("https://x.example/a", "200", new = "https://x.example/b"),
      br,
      "red"
    ),
    list(row("", "", "Empty URL"), br, "red")
  )
  for (case in cases) {
    got <- verify_classify_urls(case[[1L]], case[[2L]])
    if (!identical(got, case[[3L]])) {
      stop(
        sprintf(
          "URL classifier self-test: %s (status %s, BugReports %s) gave %s, ",
          case[[1L]]$URL,
          case[[1L]]$Status,
          case[[2L]],
          toString(got)
        ),
        sprintf("expected %s.", case[[3L]]),
        call. = FALSE
      )
    }
  }
  if (length(verify_classify_urls(row(br, "404")[0L, ], br))) {
    stop(
      "URL classifier self-test: an empty result was not empty.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Fetch every URL the package declares and fail on a dead one.
#
# It reuses base R's own implementation -- the two unexported `tools`
# functions R CMD check calls -- rather than urlchecker, which wraps the same
# logic but would be one more dev dependency. The price is that unexported
# functions may change without notice, so their absence, or a changed result
# shape, stops the stage with a message naming the fix instead of passing it
# silently.
#
# It has to prove it RAN: an empty URL db would mean the lookup read nothing,
# not that every URL is fine, so it fails, and the stage prints how many URLs
# it checked. `url_db_from_package_sources()` reads DESCRIPTION, man/,
# inst/CITATION, NEWS and README.md; vignettes count only once built to
# inst/doc, which a source tree lacks.
verify_urls <- function(dir = ".") {
  verify_url_self_test()

  fns <- c("url_db_from_package_sources", "check_url_db")
  ns <- asNamespace("tools")
  gone <- fns[!vapply(fns, exists, NA, envir = ns, inherits = FALSE)]
  if (length(gone)) {
    stop(
      sprintf(
        "%s no longer exist(s) in R %s, so the `urls` stage cannot ",
        toString(sprintf("tools:::%s()", gone)),
        getRversion()
      ),
      "run. Port it to urlchecker::url_check(), which wraps the same logic.",
      call. = FALSE
    )
  }
  url_db <- get(fns[[1L]], envir = ns)
  check_url_db <- get(fns[[2L]], envir = ns)

  db <- url_db(dir)
  urls <- unique(db$URL)
  if (!length(urls)) {
    stop(
      "found no URLs to check, yet DESCRIPTION declares several. The ",
      "lookup read nothing, so this stage verified nothing.",
      call. = FALSE
    )
  }

  # Each unanswered URL costs one timeout, sequentially; R's 60s default would
  # let a dead network hold a push for minutes only to warn at the end.
  old <- options(timeout = 30)
  on.exit(options(old), add = TRUE)
  bad <- check_url_db(db)

  bugreports <- read.dcf(file.path(dir, "DESCRIPTION"), "BugReports")[[1L]]
  kind <- verify_classify_urls(bad, bugreports)

  cat(sprintf(
    "  checked %d URL(s) from %s\n",
    length(urls),
    toString(unique(db$Parent))
  ))
  show <- function(which, heading) {
    rows <- bad[kind == which, , drop = FALSE]
    if (!nrow(rows)) {
      return(invisible())
    }
    cat(heading, "\n", sep = "")
    for (i in seq_len(nrow(rows))) {
      cat(sprintf(
        "    %s\n      status %s: %s (from %s)\n",
        rows$URL[[i]],
        if (nzchar(rows$Status[[i]])) rows$Status[[i]] else "-",
        gsub("[[:space:]]+", " ", rows$Message[[i]]),
        toString(unlist(rows$From[i]))
      ))
      if ("New" %in% names(rows) && nzchar(rows$New[[i]])) {
        cat(sprintf("      moved permanently to %s\n", rows$New[[i]]))
      }
    }
  }
  show(
    "exempt",
    paste(
      "  exempted (DESCRIPTION's BugReports keeps the CRAN-incoming /-/issues",
      "form by fleet decision, SEOR-ocbtrrnl):"
    )
  )
  show(
    "warn",
    "  WARNING, not reached (no HTTP status; network, not the URL):"
  )
  show("red", "  BROKEN:")

  red <- sum(kind == "red")
  if (red) {
    stop(sprintf("%d broken URL(s)", red), call. = FALSE)
  }
  invisible(bad)
}

if (!interactive()) {
  if ("--self-test" %in% commandArgs(trailingOnly = TRUE)) {
    verify_url_self_test()
    cat("URL classifier self-test: PASS\n")
  } else {
    verify_urls()
    cat("urls: PASS\n")
  }
}
