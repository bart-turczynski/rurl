# canonical_join() deprecation (RURL-atrvocqe).
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
    canonical_join(A, B),
    data.frame(
      A = "https://Example.com/page",
      B = "https://example.com/page?utm_source=nl",
      JoinKey = "https://example.com/page",
      ValA_A = 1L, ValB_B = "x",
      stringsAsFactors = FALSE
    )
  )
  expect_identical(
    canonical_join(A, B, join = "full"),
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

test_that("a plain canonical_join() call is silent", {
  expect_silent(canonical_join(cjd_a(), cjd_b()))
})
