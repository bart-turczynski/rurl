# canonical_join() deprecation (RURL-atrvocqe).
#
# Owner decision (2026-10-08): canonical_join() is deprecated in the next rurl
# release and removed in 4.0.0. Every call emits one warning of class
# "rurl_canonical_join_deprecated", first. A call that also forwards a legacy
# presentation dial gets the separate "rurl_legacy_join_dial_warning" as well,
# unchanged, so existing class-selective suppressions keep working.
#
# The results pin below was captured from the shipped implementation before the
# deprecation landed. The deprecation is a warning only: these values must not
# move.

cjd_a <- function() {
  data.frame(
    URL = c(
      "https://Example.com/page", "https://example.com/other",
      "mailto:x@example.com"
    ),
    ValA = 1:3, stringsAsFactors = FALSE
  )
}
cjd_b <- function() {
  data.frame(
    URL = c(
      "https://example.com/page?utm_source=nl",
      "https://example.com/missing"
    ),
    ValB = c("x", "y"), stringsAsFactors = FALSE
  )
}

test_that("canonical_join() results are pinned", {
  A <- cjd_a()
  B <- cjd_b()
  expect_identical(
    cj_deprecated(canonical_join(A, B)),
    data.frame(
      A = "https://Example.com/page",
      B = "https://example.com/page?utm_source=nl",
      JoinKey = "https://example.com/page",
      ValA_A = 1L, ValB_B = "x",
      stringsAsFactors = FALSE
    )
  )
  expect_identical(
    cj_deprecated(canonical_join(A, B, join = "full")),
    data.frame(
      A = c(
        "https://Example.com/page", "https://example.com/other",
        "mailto:x@example.com", NA
      ),
      B = c("https://example.com/page?utm_source=nl", NA, NA,
            "https://example.com/missing"),
      JoinKey = c(
        "https://example.com/page", "https://example.com/other", NA,
        "https://example.com/missing"
      ),
      ValA_A = c(1L, 2L, 3L, NA), ValB_B = c("x", NA, NA, "y"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("a plain canonical_join() call warns that it is deprecated", {
  expect_warning(
    canonical_join(cjd_a(), cjd_b()),
    class = "rurl_canonical_join_deprecated"
  )
})

test_that("the deprecation fires exactly once and names both replacements", {
  seen <- testthat::capture_warnings(canonical_join(cjd_a(), cjd_b()))
  expect_length(seen, 1L)
  expect_match(seen, "deprecated", fixed = TRUE)
  expect_match(seen, "4.0.0", fixed = TRUE)
  expect_match(seen, "url_*_join()", fixed = TRUE)
  expect_match(seen, "get_clean_url(A$URL)", fixed = TRUE)
  expect_match(
    seen, "merge(A, B, by = \"k\", incomparables = NA)", fixed = TRUE
  )
})

test_that("the recipe the warning gives needs incomparables = NA", {
  # get_clean_url() is NA for a URL with no clean form, and merge() pairs NA
  # keys with each other unless told not to. canonical_join() never matches
  # such rows, so the recipe has to say incomparables = NA.
  A <- data.frame(URL = c("https://Example.com/page", "mailto:x@example.com"),
                  ValA = 1:2, stringsAsFactors = FALSE)
  B <- data.frame(URL = c("https://example.com/page?utm_source=nl", "nope"),
                  ValB = c("x", "y"), stringsAsFactors = FALSE)
  A$k <- get_clean_url(A$URL)
  B$k <- get_clean_url(B$URL)
  expect_identical(nrow(merge(A, B, by = "k")), 2L)
  recipe <- merge(A, B, by = "k", incomparables = NA)
  legacy <- cj_deprecated(canonical_join(A[, 1:2], B[, 1:2]))
  expect_identical(recipe$k, legacy$JoinKey)
  expect_identical(recipe$ValA, legacy$ValA_A)
  expect_identical(recipe$ValB, legacy$ValB_B)
})

test_that("the deprecation can be muted by class alone, results unchanged", {
  A <- cjd_a()
  B <- cjd_b()
  expect_silent(
    muted <- suppressWarnings(
      canonical_join(A, B),
      classes = "rurl_canonical_join_deprecated"
    )
  )
  # Same value whether the condition is muted, or caught and resumed.
  caught <- withCallingHandlers(
    canonical_join(A, B),
    rurl_canonical_join_deprecated = function(w) {
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(muted, caught)
})

test_that("a legacy dial gets two warnings: deprecation first, then the dial", {
  seen <- new.env()
  seen$conds <- list()
  withCallingHandlers(
    canonical_join(cjd_a(), cjd_b(), www_handling = "strip"),
    warning = function(w) {
      seen$conds[[length(seen$conds) + 1L]] <- w
      invokeRestart("muffleWarning")
    }
  )
  conds <- seen$conds
  expect_length(conds, 2L)
  expect_s3_class(conds[[1L]], "rurl_canonical_join_deprecated")
  expect_s3_class(conds[[2L]], "rurl_legacy_join_dial_warning")
  expect_false(inherits(conds[[1L]], "rurl_legacy_join_dial_warning"))
  expect_false(inherits(conds[[2L]], "rurl_canonical_join_deprecated"))
})

test_that("muting the dial class does not mute the deprecation", {
  expect_warning(
    suppressWarnings(
      canonical_join(cjd_a(), cjd_b(), www_handling = "strip"),
      classes = "rurl_legacy_join_dial_warning"
    ),
    class = "rurl_canonical_join_deprecated"
  )
})

test_that("the deprecation fires even when the call then errors", {
  expect_warning(
    expect_error(
      canonical_join(cjd_a(), cjd_b(), on_parse_error = "error"),
      "parsing errors"
    ),
    class = "rurl_canonical_join_deprecated"
  )
})
