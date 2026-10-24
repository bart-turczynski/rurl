# A host of very many dot-separated labels gets its own row (RURL-rrumztrf).
# The host classifiers were ICU regexes with a repeated group
# (`^[^.]+(\.[^.]+)+$`, and the IPv4-attempt `(\.(...))*$`), whose backtrack
# stack overflowed near 4e5 labels: U_REGEX_STACK_OVERFLOW threw for the whole
# vectorized call, so its good neighbors were lost too. They are linear checks
# now.

long_name <- paste0("http://", strrep("a.", 4e5), "com/p")
long_ipish <- paste0("http://", strrep("1.", 4e5), "1/")

test_that("a 4e5-label host keeps its neighbors under every url_standard", {
  for (std in list(NULL, "rfc3986", "whatwg")) {
    label <- if (is.null(std)) "NULL" else std
    out <- safe_parse_urls(
      c("http://ok.example/", long_name, long_ipish, "http://ok2.example/x"),
      url_standard = std
    )
    expect_identical(nrow(out), 4L, label = label)
    expect_identical(out$host[c(1L, 4L)], c("ok.example", "ok2.example"),
                     label = label)
  }
})

test_that("the linear classifiers give the old answers on many labels", {
  token <- paste0(strrep("a.", 4e5), "com")
  cls <- rurl:::.classify_input_host_vec(c(
    paste0("http://", token, "/p"),
    paste0("http://", strrep("1.", 4e5), "1/"),
    paste0("http://", strrep("0x1.", 4e5), "0x/"),
    paste0("http://", strrep("1.", 4e5), "/"),
    paste0("http://", strrep("a.", 4e5), "/"),
    paste0("http://", strrep("1.", 4e5), "g/")
  ))
  expect_identical(cls$is_dotted_name, c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_identical(cls$is_ipv4ish, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("the rootless-path carve-out reads a many-label segment", {
  # `^[A-Za-z0-9._~-]+(\.[A-Za-z0-9._~-]+)+$` had the same repeated group.
  r <- rurl:::.rfc3986_path_rootless_vec(c(
    paste0("http:", strrep("a.", 4e5), "b/c"),
    paste0("http:", strrep("a.", 4e5), "b!/c"),
    "http:a.b/c"
  ), "rfc3986")
  expect_identical(r$is_path_rootless, c(TRUE, FALSE, TRUE))
})

test_that("the IPv4-shape diagnostics read a many-label host", {
  expect_identical(
    rurl:::.ipv4_host_diagnostics(paste0(strrep("a.", 4e5), "com")),
    character(0)
  )
  # More than four parts is not an IPv4 shape these diagnostics report.
  long <- paste0(strrep("1.", 4e5), "1")
  expect_identical(rurl:::.ipv4_host_diagnostics(long), character(0))
  # The shape test itself: `$` is the true end here (a base-R regex), so a
  # final LF is not an IPv4 attempt, unlike the Phase-1 classifier's ICU `$`.
  expect_identical(rurl:::.ipv4_host_diagnostics("1.2"), "ipv4-short-form")
  expect_identical(
    rurl:::.ipv4_host_diagnostics("0x"),
    c("ipv4-number-form", "ipv4-non-decimal")
  )
  expect_identical(rurl:::.ipv4_host_diagnostics("0x1.010.1"), c(
    "ipv4-short-form", "ipv4-non-decimal", "ipv4-octal", "ipv4-leading-zero"
  ))
  expect_identical(rurl:::.ipv4_host_diagnostics("256.1.1.1"),
                   "ipv4-out-of-range")
  for (t in c("1.2\n", "1..2", "a.1", ".1", "1.", "")) {
    expect_identical(rurl:::.ipv4_host_diagnostics(t), character(0), label = t)
  }
  for (std in c("rfc3986", "whatwg")) {
    d <- get_url_diagnostics(
      c("http://ok.example/", long_name, long_ipish), url_standard = std
    )
    expect_length(d, 3L)
  }
})
