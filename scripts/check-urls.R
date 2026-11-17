#!/usr/bin/env Rscript

# scripts/check-urls.R -- the URL check, a cheap gate of the fleet-ci local
# gate (.fleet-ci.yml's cheap_gates).
#
#   Rscript scripts/check-urls.R              # fetch every declared URL
#   Rscript scripts/check-urls.R --self-test  # the offline classifier test only
#
# Fetches every URL the package declares and fails on a dead one, which
# nothing else in the gate does. The gate's `R CMD check` runs with CRAN
# incoming feasibility off, so it never calls check_url_db(); and even where
# `R CMD check --as-cran` does (T2's t2-ascran), a dead link is only a NOTE,
# and the checks fail on warnings, not notes. The fleet standard adds this
# stage to every package (seor design/fleet-standard.md, "Local pre-push
# gate"). Ported from sitemapr's `urls` stage in tools/verify.R; the three
# functions below are its verify_classify_urls(), verify_url_self_test() and
# verify_urls(), unchanged apart from these comments.
# The r-universe helpers below come from ssrfr's copy (SEOR-uzqwlehl).
#
# It runs in the gate's full tier ahead of R CMD check, so a dead link is
# reported in seconds rather than after the full check.

# Sort the rows `tools:::check_url_db()` returns into what they mean for the
# gate: "red" (a real defect: fail), "warn" (no answer at all: report,
# pass), "limited" (gitlab.com refused to answer this machine for now:
# report loudly, pass) or "challenged" (an r-universe page behind
# Cloudflare's bot challenge, its package confirmed through the API: report
# loudly, pass). Pure and offline, so verify_url_self_test() can pin it with
# a constructed data frame; `confirmed` carries the API's answers, one per
# row, which verify_urls() fetches.
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
# So only "Error" rows with no static complaint are downgraded to a
# warning: a transient network blip must never reject a push. Nor must
# gitlab.com's rate limit: it answers 429 per IP, shared by every session
# on the machine, for half an hour at a time, whatever the URL
# (SEOR-bgvkdxyw). Its 429s with no static complaint are "limited"; a 429
# from any other host stays red, as 403 does. Everything else
# check_url_db() returns is red. That includes 403,
# which R CMD check's incoming step drops by default
# (`_R_CHECK_URLS_TAKE_403_STATUS_AS_OK_`) because bot-shy hosts answer it;
# GitLab answers 403, not 404, for a project that does not exist, so here it
# stays a failure.
#
# Except on r-universe (SEOR-uzqwlehl). Since 2026-11-16 every
# `*.r-universe.dev` page answers a non-browser client 403 with
# `cf-mitigated: challenge`, Cloudflare's Turnstile, whether or not the page
# exists. Its JSON API sits outside the challenge and answers 200 for a
# package and 404 for none, so a 403 from a universe page or root is checked
# there instead: "challenged" when the page's 403 carries the challenge header
# and the API confirms the package, red otherwise. Only the page forms
# r_universe_api_url() recognizes get this; any other r-universe URL, a plain
# 403, and a 403 from any other host stay red.
#
# There is no other exemption. Until SEOR-sptibjph, DESCRIPTION's BugReports
# kept GitLab's `/-/issues` form, which 404s for a non-browser client, and its
# 404 was waved through (SEOR-ocbtrrnl). BugReports now names
# `/-/work_items`, as CRAN invited on 2026-10-23 (seor ADR 0011), so a
# `/-/issues` 404 is red like any other.
verify_classify_urls <- function(bad, confirmed = logical(nrow(bad))) {
  stopifnot(is.logical(confirmed), length(confirmed) == nrow(bad))
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

  kind <- rep("red", n)
  kind[bad$Status == "Error" & !static] <- "warn"
  gitlab <- grepl("^https?://gitlab\\.com([/:?#]|$)", bad$URL)
  kind[bad$Status == "429" & gitlab & !static] <- "limited"
  universe <- !is.na(r_universe_api_url(bad$URL))
  kind[bad$Status == "403" & universe & confirmed & !static] <- "challenged"
  kind
}

# The r-universe API URL that confirms a universe page, or NA for any URL
# that is not one: `https://<owner>.r-universe.dev` (the universe) or
# `https://<owner>.r-universe.dev/<package>`, with an optional trailing
# slash and nothing else.
r_universe_api_url <- function(url) {
  re <- paste0(
    "^https://([a-z0-9][a-z0-9-]*)\\.r-universe\\.dev",
    "(/([A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]))?/?$"
  )
  m <- regmatches(url, regexec(re, url))
  vapply(
    m,
    function(x) {
      if (!length(x)) {
        return(NA_character_)
      }
      base <- sprintf("https://%s.r-universe.dev/api/packages", x[[2L]])
      if (nzchar(x[[4L]])) paste0(base, "/", x[[4L]]) else base
    },
    ""
  )
}

# TRUE for each URL whose page is behind Cloudflare's challenge (403 with
# `cf-mitigated: challenge`) and whose r-universe API answers 200 itself, with
# no redirect followed; FALSE for anything else, a failed request or a URL
# r_universe_api_url() does not recognize included. A plain 403 on a page
# that exists is a real refusal, so it stays red. `headers` is
# curlGetHeaders() in the gate, a fake in the self-test.
r_universe_confirms <- function(url, headers = curlGetHeaders) {
  status_of <- function(u) {
    h <- tryCatch(headers(u, redirect = FALSE), error = function(e) NULL)
    if (is.null(h)) {
      return(list(status = NA_integer_, challenge = FALSE))
    }
    list(
      status = as.integer(attr(h, "status")),
      challenge = any(grepl(
        "^cf-mitigated:[ \t]*challenge\\b",
        h,
        ignore.case = TRUE
      ))
    )
  }
  api <- r_universe_api_url(url)
  verdict <- vapply(
    unique(api[!is.na(api)]),
    function(a) {
      identical(status_of(a)$status, 200L)
    },
    NA
  )
  vapply(
    seq_along(url),
    function(i) {
      if (is.na(api[[i]]) || !verdict[[api[[i]]]]) {
        return(FALSE)
      }
      page <- status_of(url[[i]])
      identical(page$status, 403L) && page$challenge
    },
    NA
  )
}

# Pins verify_classify_urls() and the r-universe helpers offline.
# verify_urls() runs it before it touches the network, so a classifier edit
# that would wave a dead link through fails the gate on the spot.
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
    list(row(br, "404", "Not Found"), "red"), # the old exemption is gone
    list(row(wi, "404", "Not Found"), "red"),
    list(row(br, "403", "Forbidden"), "red"),
    list(row("https://x.example/", "404", "Not Found"), "red"),
    list(row("https://x.example/", "500", "Server Error"), "red"),
    list(row("https://nonexistent.invalid/", "Error", "error 6"), "warn"),
    list(row("https://10.255.255.1/", "Error", "error 28"), "warn"),
    list(row(wi, "429", "Too Many Requests"), "limited"),
    list(row("https://x.example/", "429", "Too Many Requests"), "red"),
    list(row("https://gitlab.com.x.example/", "429", ""), "red"),
    list(
      rbind(row(wi, "429"), row("https://x.example/", "404")),
      c("limited", "red")
    ),
    list(
      row(wi, "429", "Too Many Requests", new = "https://x.example/"),
      "red"
    ),
    list(
      row("https://x.example/a", "200", new = "https://x.example/b"),
      "red"
    ),
    list(row("", "", "Empty URL"), "red")
  )
  # r-universe pages: the third element is the API's answer per row.
  ru <- "https://o.r-universe.dev/pkg"
  cases <- c(
    cases,
    list(
      list(row(ru, "403", "Forbidden"), "challenged", TRUE),
      list(row(paste0(ru, "/"), "403", "Forbidden"), "challenged", TRUE),
      list(row("https://o.r-universe.dev", "403"), "challenged", TRUE),
      list(row(ru, "403", "Forbidden"), "red", FALSE), # a missing package
      list(row(ru, "404", "Not Found"), "red", TRUE),
      list(row(paste0(ru, "/doc/x.html"), "403"), "red", TRUE),
      list(row("https://o.r-universe.dev.x.example/pkg", "403"), "red", TRUE),
      list(row(br, "403", "Forbidden"), "red", TRUE), # GitLab stays red
      list(row(ru, "403", new = "https://x.example/"), "red", TRUE),
      list(
        rbind(row(ru, "403"), row(ru, "403")),
        c("challenged", "red"),
        c(TRUE, FALSE)
      )
    )
  )
  api_cases <- list(
    c(ru, "https://o.r-universe.dev/api/packages/pkg"),
    c("https://o.r-universe.dev/", "https://o.r-universe.dev/api/packages"),
    c(
      "https://o.r-universe.dev/a.b2",
      "https://o.r-universe.dev/api/packages/a.b2"
    ),
    c("http://o.r-universe.dev/pkg", NA),
    c("https://o.r-universe.dev/pkg/x", NA),
    c("https://evil.example/o.r-universe.dev/pkg", NA)
  )
  for (case in api_cases) {
    if (!identical(r_universe_api_url(case[[1L]]), case[[2L]])) {
      stop(
        sprintf(
          "URL classifier self-test: r_universe_api_url(%s) gave %s, ",
          case[[1L]],
          r_universe_api_url(case[[1L]])
        ),
        sprintf("expected %s.", case[[2L]]),
        call. = FALSE
      )
    }
  }
  # r_universe_confirms() on a fake fetch: the page must carry the challenge
  # and the API must answer 200 without a redirect.
  fake <- function(u, redirect = TRUE) {
    if (redirect) {
      stop("redirect must be FALSE")
    }
    challenge <- "cf-mitigated: challenge\r\n"
    answer <- switch(
      u,
      "https://o.r-universe.dev/pkg" = list(403L, challenge),
      "https://o.r-universe.dev/plain" = list(403L, "server: x\r\n"),
      "https://o.r-universe.dev/gone" = list(403L, challenge),
      "https://o.r-universe.dev/moved" = list(403L, challenge),
      "https://o.r-universe.dev/api/packages/pkg" = list(200L, ""),
      "https://o.r-universe.dev/api/packages/plain" = list(200L, ""),
      "https://o.r-universe.dev/api/packages/gone" = list(404L, ""),
      "https://o.r-universe.dev/api/packages/moved" = list(301L, ""),
      stop("unreachable")
    )
    structure(answer[[2L]], status = answer[[1L]])
  }
  got <- r_universe_confirms(
    c(
      ru,
      "https://o.r-universe.dev/plain",
      "https://o.r-universe.dev/gone",
      "https://o.r-universe.dev/moved",
      "https://o.r-universe.dev/down",
      "https://gitlab.com/o/p"
    ),
    headers = fake
  )
  if (!identical(got, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))) {
    stop(
      "URL classifier self-test: r_universe_confirms() gave ",
      toString(got),
      ", expected TRUE then five FALSE.",
      call. = FALSE
    )
  }
  for (case in cases) {
    confirmed <- if (length(case) > 2L) case[[3L]] else FALSE
    confirmed <- rep_len(confirmed, nrow(case[[1L]]))
    got <- verify_classify_urls(case[[1L]], confirmed)
    if (!identical(got, case[[2L]])) {
      stop(
        sprintf(
          "URL classifier self-test: %s (status %s) gave %s, ",
          case[[1L]]$URL,
          case[[1L]]$Status,
          toString(got)
        ),
        sprintf("expected %s.", case[[2L]]),
        call. = FALSE
      )
    }
  }
  if (length(verify_classify_urls(row(br, "404")[0L, ]))) {
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

  # Ask r-universe's API only about its 403s; every other row stays as is.
  confirmed <- logical(nrow(bad))
  ask <- bad$Status == "403" & !is.na(r_universe_api_url(bad$URL))
  confirmed[ask] <- r_universe_confirms(bad$URL[ask])
  kind <- verify_classify_urls(bad, confirmed)

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
    "warn",
    "  WARNING, not reached (no HTTP status; network, not the URL):"
  )
  show(
    "limited",
    paste0(
      "  WARNING, NOT VERIFIED: gitlab.com rate-limited this machine ",
      "(429); push again once it clears to check these:"
    )
  )
  show(
    "challenged",
    paste0(
      "  WARNING, PAGE NOT FETCHED: r-universe answered with Cloudflare's ",
      "bot challenge (403); its API confirms each of these:"
    )
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
