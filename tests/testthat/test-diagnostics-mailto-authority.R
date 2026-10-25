# RURL-cjxdaqym: get_url_diagnostics() reports authority facts only where the
# parse produced an authority. ADR 0012 D7 installs a mailto recipient's local
# part and domain as the row's `user` / `host` for the accessors; that is
# extraction metadata, not a URL authority. WHATWG parses `mailto:a@x.de` into
# an opaque path (URL Standard, basic URL parser, "opaque path state"), so it
# has no username and raises no invalid-credentials; RFC 3986 section 3 reads
# it as a path-rootless hier-part with no authority.

ma_diag <- function(url, std = "whatwg", acc = "general") {
  get_url_diagnostics(url, url_standard = std, scheme_acceptance = acc)
}

ma_gen <- list(url_standard = "whatwg", scheme_acceptance = "general")

# --- Pins: authority rows keep their facts --------------------------------

test_that("a web authority's credentials stay invalid-credentials (whatwg)", {
  expect_true("invalid-credentials" %in% ma_diag("http://u@h/"))
  expect_true("invalid-credentials" %in% ma_diag("http://u@h/", acc = "web"))
  expect_true("invalid-credentials" %in% ma_diag("http://u:p@h/"))
})

test_that("a non-special `//` authority keeps credentials and host shape", {
  d <- ma_diag("sc://u@1.2.3.999/")
  expect_true("invalid-credentials" %in% d)
  expect_true("ipv4-out-of-range" %in% d)
  expect_true("ipv4-out-of-range" %in% ma_diag("sc://u@1.2.3.999/", "rfc3986"))
  # mailto with a real `//` authority is an authority row too (RURL-gmzipkyw).
  expect_true("invalid-credentials" %in% ma_diag("mailto://u@h/"))
})

test_that("ws userinfo stays two facts", {
  d <- ma_diag("ws://u@h/")
  expect_true("invalid-credentials" %in% d)
  expect_true("ws-userinfo-forbidden" %in% d)
})

test_that("web host-shape facts are untouched", {
  expect_true(
    "ipv4-out-of-range" %in% ma_diag("http://u:p@256.256.256.256/", "rfc3986")
  )
  expect_true("ipv4-short-form" %in% ma_diag("http://0x7f.1/", "rfc3986"))
  expect_true("ipv6-non-canonical" %in% ma_diag("http://[0:0::1]/"))
  expect_true("domain-hyphen-violation" %in% ma_diag("http://ex--ample.com/"))
  expect_true(
    "domain-hyphen-violation" %in% ma_diag("http://ex--ample.com/", "rfc3986")
  )
})

test_that("D7 extraction on mailto does not move", {
  u <- c("mailto:a@x.de", "mailto:a@256.256.256.256")
  expect_identical(do.call(get_user, c(list(u), ma_gen)), c("a", "a"))
  expect_identical(
    do.call(get_host, c(list(u), ma_gen)), c("x.de", "256.256.256.256")
  )
  expect_identical(do.call(get_domain, c(list(u[1L]), ma_gen)), "x.de")
  expect_identical(
    get_host_type("mailto:a@x.de", url_standard = "whatwg",
                  scheme_acceptance = "general"),
    "domain"
  )
})
