# `credential_handling` on the clean surface (RUL-001, ADR 0017 row 12;
# RURL-kjrjbhef).
#
# The clean surface always drops userinfo. The dial decides what a caller gets
# back for a row whose parsed authority carried a userinfo DELIMITER: "strip"
# (default) is the historical, silently collapsed URL; "reject" is NA. Nothing
# else moves -- components, `parse_status`, diagnostics, `serialize_url()` and
# `get_url_key()` are pinned unchanged, under BOTH named standards and, for the
# default, byte-identical under `url_standard = NULL` (ADR 0007).
#
# Red at baseline (2026-09-04, main @ 359840a): every call below that names the
# argument failed with `unused argument (credential_handling = "reject")`, and
# `url_profile("seo")$credential_handling` was NULL.
#
# Detection lives in Stage A (R/parse.R, `authority_userinfo`): read off the
# parser's non-NULL `user` for the web route and off `gen$userinfo` for the
# general route, BEFORE `.blank_to_na()` empties the bare-`@` shapes.

# One row per delimiter shape the ruling names, plus the eq-U8 Unicode-confusion
# shape (tests/testthat/fixtures/external-url-vectors.csv, row eq-U8) and two
# credential-free controls. The parse surface strips input names
# (RURL-vhdsqaln), so rows are addressed by position through `at()`.
cred_shapes <- c(
  user_only = "http://user@example.com/a",
  user_password = "http://user:pw@example.com/a",
  empty_at = "http://@example.com/a",
  empty_user_empty_password = "http://:@example.com/a",
  repeated_at = "http://a@b@example.com/a",
  host_looking_userinfo = "https://example.com@evil.example/a",
  eq_u8 = "https://n.prİ@e.gg",
  control = "http://example.com/a",
  control_query = "http://example.com/a?q=1"
)
cred_free <- c("control", "control_query")
has_cred <- !(names(cred_shapes) %in% cred_free)
at <- function(x, ...) x[match(c(...), names(cred_shapes))]
urls <- unname(cred_shapes)

named_standards <- c("whatwg", "rfc3986")
posture <- function(std, ...) {
  c(
    list(url_standard = std, scheme_policy = "require",
         scheme_acceptance = "general"),
    list(...)
  )
}
parse_with <- function(u, std, ...) {
  do.call(safe_parse_urls, c(list(u), posture(std, ...)))
}

test_that("strip is the default and is today's output under both standards", {
  for (std in named_standards) {
    default <- parse_with(urls, std)
    strip <- parse_with(urls, std, credential_handling = "strip")
    expect_identical(strip, default, info = std)
    # the collapsed spellings the surface has always emitted
    expect_identical(
      at(strip$clean_url, "user_only", "user_password", "empty_at",
         "empty_user_empty_password", "host_looking_userinfo", "control"),
      c(rep("http://example.com/a", 4L), "https://evil.example/a",
        "http://example.com/a"),
      info = std
    )
  }
})

test_that("reject sets ONLY clean_url to NA where the authority had userinfo", {
  for (std in named_standards) {
    strip <- parse_with(urls, std, credential_handling = "strip")
    reject <- parse_with(urls, std, credential_handling = "reject")

    # every credential-bearing row is NA; a row that was already an error
    # (rfc3986 rejects the repeated `@`) is NA under both values
    expect_true(all(is.na(reject$clean_url[has_cred])), info = std)
    # the controls are untouched
    expect_identical(
      reject$clean_url[!has_cred], strip$clean_url[!has_cred], info = std
    )
    # and NOTHING else moved: drop the one column the dial owns and compare
    strip$clean_url <- NULL
    reject$clean_url <- NULL
    expect_identical(reject, strip, info = std)
  }
})

test_that("reject covers every delimiter shape, including the empty ones", {
  # The bare `@` and `:@` shapes are the reason detection reads the parser's
  # delimiter rather than the `user`/`password` columns, which are NA there.
  for (std in named_standards) {
    reject <- parse_with(urls, std, credential_handling = "reject")
    expect_true(is.na(at(reject$user, "empty_at")), info = std)
    expect_true(is.na(at(reject$password, "empty_at")), info = std)
    expect_true(is.na(at(reject$clean_url, "empty_at")), info = std)
    expect_true(is.na(at(reject$clean_url, "empty_user_empty_password")),
                info = std)
    expect_identical(at(reject$parse_status, "empty_at"), "ok", info = std)
    # eq-U8: the `@` is preceded by host-looking text; rurl's parse keeps the
    # userinfo interpretation (host e.gg), so it is a credential row here too
    expect_identical(at(reject$host, "eq_u8"), "e.gg", info = std)
    expect_true(is.na(at(reject$clean_url, "eq_u8")), info = std)
  }
  # the repeated `@` parses under WHATWG (last-`@` split) and is rejected
  whatwg <- parse_with(urls, "whatwg", credential_handling = "reject")
  expect_identical(at(whatwg$parse_status, "repeated_at"), "ok")
  expect_identical(at(whatwg$user, "repeated_at"), "a%40b")
  expect_true(is.na(at(whatwg$clean_url, "repeated_at")))
})

test_that("reject reaches the general-routed (non-special) authorities too", {
  gen <- c("sc://u:p@h/x", "sc://@h/x", "sc://h/x", "mailto:a@b.com")
  for (std in named_standards) {
    strip <- parse_with(gen, std, credential_handling = "strip")
    reject <- parse_with(gen, std, credential_handling = "reject")
    expect_identical(strip$clean_url[1:3], rep("sc://h/x", 3L), info = std)
    expect_true(all(is.na(reject$clean_url[1:2])), info = std)
    expect_identical(reject$clean_url[3:4], strip$clean_url[3:4], info = std)
    # an opaque path has no authority, so its recipient `@` is not userinfo
    expect_identical(reject$clean_url[[4L]], "mailto:a@b.com", info = std)
  }
})

test_that("the identity surfaces do not move under reject", {
  # serialize_url() and get_url_key() take no credential dial at all; pin that
  # the credentials are still carried / still excluded from identity exactly
  # as before, on the same rows the clean surface now refuses.
  cred_rows <- at(urls, "user_only", "user_password", "host_looking_userinfo")
  for (std in named_standards) {
    ser <- serialize_url(cred_rows, standard = std)
    expect_true(all(grepl("@", ser, fixed = TRUE)), info = std)
  }
  # userinfo is excluded from web-resource identity: the key of a credential
  # row equals the key of its credential-free twin, and neither is NA
  twins <- c("http://example.com/a", "http://example.com/a",
             "https://evil.example/a")
  keys <- get_url_key(cred_rows)
  expect_false(anyNA(keys))
  expect_identical(keys, get_url_key(twins))
})

test_that("get_clean_url() carries the dial, with and without a profile", {
  u <- at(urls, "user_password", "empty_at", "control")
  for (std in named_standards) {
    strip <- do.call(get_clean_url, c(list(u), posture(std)))
    reject <- do.call(
      get_clean_url, c(list(u), posture(std, credential_handling = "reject"))
    )
    expect_identical(strip, parse_with(u, std)$clean_url, info = std)
    expect_identical(reject, c(NA, NA, strip[[3L]]), info = std)
  }
  # a bundle keeps "strip"; the iron rule lets the caller override it
  expect_identical(
    get_clean_url(u, profile = "seo"),
    rep("https://example.com/a", 3L)
  )
  expect_identical(
    get_clean_url(u, profile = "seo", credential_handling = "reject"),
    c(NA, NA, "https://example.com/a")
  )
  # scalar surface
  expect_true(is.na(
    safe_parse_url(u[[1L]], credential_handling = "reject")$clean_url
  ))
  expect_identical(
    safe_parse_url(u[[1L]], credential_handling = "reject")$user, "user"
  )
})

test_that("reject reads the delimiter through a warm Stage-A cache", {
  # The dial is a Stage-B option, so a row parsed once under "strip" is served
  # from the Stage-A cache when re-asked under "reject". The delimiter flag is
  # a cached Stage-A field precisely so this round trip cannot lose it.
  u <- "http://@cache-probe.example/a"
  expect_identical(get_clean_url(u), "http://cache-probe.example/a")
  expect_true(is.na(get_clean_url(u, credential_handling = "reject")))
  expect_identical(get_clean_url(u), "http://cache-probe.example/a")
})

test_that("every profile bundle carries credential_handling = \"strip\"", {
  for (p in c("browser", "whatwg", "rfc-syntax", "seo", "canonical")) {
    expect_identical(url_profile(p)$credential_handling, "strip", info = p)
  }
  res <- url_profile("seo", credential_handling = "reject")
  expect_identical(res$credential_handling, "reject")
  expect_true(res$customized)
})

test_that("the dial validates like every other match.arg() option", {
  u <- at(urls, "user_only")
  expect_error(get_clean_url(u, credential_handling = "keep"), "strip")
  expect_error(safe_parse_url(u, credential_handling = "keep"), "strip")
  expect_error(safe_parse_urls(u, credential_handling = "keep"), "strip")
  expect_error(url_profile("seo", credential_handling = "keep"), "strip")
  # partial matching resolves like the other options
  expect_true(is.na(get_clean_url(u, credential_handling = "rej")))
})

test_that("url_standard = NULL: strip is byte-identical; reject composes", {
  corpus <- c(
    urls,
    read.csv(
      testthat::test_path("fixtures", "parse-corpus.csv"),
      stringsAsFactors = FALSE
    )$url
  )
  expect_identical(
    safe_parse_urls(corpus, credential_handling = "strip"),
    safe_parse_urls(corpus)
  )
  expect_identical(
    get_clean_url(corpus, credential_handling = "strip"),
    get_clean_url(corpus)
  )
  # the dial is policy, not a standards axis, so the frozen arm accepts it
  strip <- safe_parse_urls(urls)
  reject <- safe_parse_urls(urls, credential_handling = "reject")
  expect_true(all(is.na(reject$clean_url[has_cred])))
  expect_identical(reject$clean_url[!has_cred], strip$clean_url[!has_cred])
  strip$clean_url <- NULL
  reject$clean_url <- NULL
  expect_identical(reject, strip)
})
