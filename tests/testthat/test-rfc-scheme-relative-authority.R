# RURL-ozpxupgq: a `//`-led row under `scheme_relative_handling = "keep"`.
#
# RFC 3986 section 4.2 reads a network-path reference as
# `relative-part = "//" authority path-abempty`, and section 3.2's `reg-name`
# is `*`-quantified, so `///p` is an EMPTY authority followed by the path `/p`,
# while `//p` is the authority `p` with an empty path. Stage A parsed every
# `//` row behind a stand-in `http:` prefix on the web route, which models the
# special-scheme authority and skips the extra slash: `///p` read as host `p`,
# and the two references keyed as one.
#
# This file pins, first, what the fix must NOT move: `//host` rows under every
# posture, the other postures' reading of the empty-authority shapes, and the
# serializer's NA for every `//` row.

srel_hosts <- c("//p", "//p/", "//example.com/a?q#f", "//u@h.com:8080/x",
                "//[::1]/x")
srel_empty <- c("///p", "////p", "///", "//", "//u@/p", "//:80/x")
srel_cols <- c("scheme", "host", "port", "path", "query", "clean_url",
               "parse_status")

srel_expect <- function(x, args, expected, label) {
  r <- do.call(safe_parse_urls, c(list(x), args))
  for (cl in names(expected)) {
    want <- expected[[cl]]
    got <- r[[cl]]
    if (cl == "port") {
      want <- as.integer(want)
    } else {
      want <- as.character(want)
    }
    expect_identical(got, want, label = paste(label, cl))
  }
}

# The `//host` shapes, whose authority the web route already reads right.
srel_host_rows <- list(
  fields = function(scheme, path, clean_url, parse_status) {
    list(
      scheme = scheme,
      host = c("p", "p", "example.com", "h.com", "[::1]"),
      port = c(NA, NA, NA, 8080L, NA),
      path = path,
      query = c(NA, NA, "q", NA, NA),
      clean_url = clean_url,
      parse_status = parse_status
    )
  }
)
srel_status_keep <- c("warning-no-tld", "warning-no-tld", "ok-scheme-relative",
                      "ok-scheme-relative", "ok-scheme-relative")
srel_status_assumed <- c("warning-no-tld", "warning-no-tld", "ok", "ok", "ok")
srel_path_rfc <- c("", "/", "/a", "/x", "/x")
srel_path_web <- c("/", "/", "/a", "/x", "/x")
srel_na5 <- rep(NA, 5L)

test_that("//host rows keep their parse under the rfc3986 keep posture", {
  want <- srel_host_rows$fields(
    srel_na5, srel_path_rfc,
    c("http://p", "http://p/", "http://example.com/a", "http://h.com/x",
      "http://[::1]/x"),
    srel_status_keep
  )
  srel_expect(srel_hosts, list(profile = "rfc-syntax"), want, "rfc-syntax")
  srel_expect(srel_hosts, list(url_standard = "rfc3986"), want, "rfc3986")
})

test_that("//host rows keep their parse under the postures that assume http", {
  srel_expect(
    srel_hosts,
    list(url_standard = "rfc3986", scheme_relative_handling = "http"),
    srel_host_rows$fields(
      rep("http", 5L), srel_path_rfc,
      c("http://p", "http://p/", "http://example.com/a", "http://h.com/x",
        "http://[::1]/x"),
      srel_status_assumed
    ),
    "rfc3986 http"
  )
  srel_expect(
    srel_hosts,
    list(url_standard = "rfc3986", scheme_relative_handling = "https"),
    srel_host_rows$fields(
      rep("https", 5L), srel_path_rfc,
      c("https://p", "https://p/", "https://example.com/a", "https://h.com/x",
        "https://[::1]/x"),
      srel_status_assumed
    ),
    "rfc3986 https"
  )
  srel_expect(
    srel_hosts, list(scheme_relative_handling = "http"),
    srel_host_rows$fields(
      rep("http", 5L), srel_path_web,
      c("http://p/", "http://p/", "http://example.com/a", "http://h.com/x",
        "http://[::1]/x"),
      srel_status_assumed
    ),
    "NULL http"
  )
})

test_that("//host rows keep their parse under whatwg, NULL and the SEO bundle", {
  web_keep <- srel_host_rows$fields(
    srel_na5, srel_path_web,
    c("http://p/", "http://p/", "http://example.com/a", "http://h.com/x",
      "http://[::1]/x"),
    srel_status_keep
  )
  srel_expect(srel_hosts, list(), web_keep, "NULL")
  srel_expect(srel_hosts, list(url_standard = "whatwg"), web_keep, "whatwg")
  seo <- srel_host_rows$fields(
    srel_na5, srel_path_web,
    c("https://p", "https://p", "https://example.com/a",
      "https://h.com:8080/x", "https://[::1]/x"),
    srel_status_keep
  )
  srel_expect(srel_hosts, list(profile = "seo"), seo, "seo")
  srel_expect(srel_hosts, list(profile = "canonical"), seo, "canonical")
})

test_that("the whatwg profile rejects every // row", {
  r <- safe_parse_urls(c(srel_hosts, srel_empty), profile = "whatwg")
  expect_identical(r$parse_status, rep("error", 11L))
  expect_identical(r$host, rep(NA_character_, 11L))
})

# The empty-authority shapes under every posture but the rfc3986 keep one. A
# posture other than "keep" assumes a scheme, and `whatwg` and `NULL` read the
# leading slashes the WHATWG way: the special authority state skips the slash
# run, so `///p` reads host `p`, and `////p` has no WHATWG answer rurl
# reproduces (rejected). None of them is moved by RURL-ozpxupgq.
srel_empty_fields <- function(scheme, path, clean_url) {
  list(
    scheme = c(scheme, rep(NA, 5L)),
    host = c("p", rep(NA, 5L)),
    port = rep(NA, 6L),
    path = c(path, rep(NA, 5L)),
    query = rep(NA, 6L),
    clean_url = c(clean_url, rep(NA, 5L)),
    parse_status = c("warning-no-tld", rep("error", 5L))
  )
}

test_that("other postures keep reading the empty-authority shapes as before", {
  srel_expect(srel_empty, list(), srel_empty_fields(NA, "/", "http://p/"),
              "NULL")
  srel_expect(srel_empty, list(url_standard = "whatwg"),
              srel_empty_fields(NA, "/", "http://p/"), "whatwg")
  srel_expect(srel_empty, list(profile = "seo"),
              srel_empty_fields(NA, "/", "https://p"), "seo")
  srel_expect(srel_empty, list(profile = "canonical"),
              srel_empty_fields(NA, "/", "https://p"), "canonical")
  srel_expect(srel_empty, list(scheme_relative_handling = "http"),
              srel_empty_fields("http", "/", "http://p/"), "NULL http")
  srel_expect(
    srel_empty,
    list(url_standard = "rfc3986", scheme_relative_handling = "http"),
    srel_empty_fields("http", "", "http://p"), "rfc3986 http"
  )
  srel_expect(
    srel_empty,
    list(url_standard = "rfc3986", scheme_relative_handling = "https"),
    srel_empty_fields("https", "", "https://p"), "rfc3986 https"
  )
  for (std in list(NULL, "rfc3986", "whatwg")) {
    r <- safe_parse_urls(srel_empty, url_standard = std,
                         scheme_relative_handling = "error")
    expect_identical(r$parse_status, rep("error", 6L),
                     label = paste("error", toString(std)))
  }
})

# The comparison key. Its rfc3986 posture is the `rfc-syntax` bundle, so it
# reads `//` rows under "keep"; its whatwg posture rejects them.
srel_key <- function(x, standard) {
  get_url_key(x, policy = url_key_policy(standard = standard))
}
srel_same <- function(a, b, standard = "rfc3986") {
  k <- as.character(unclass(srel_key(c(a, b), standard)))
  identical(k[[1L]], k[[2L]])
}

test_that("rfc3986 keys of //host rows keep their identity relations", {
  k <- srel_key(srel_hosts, "rfc3986")
  expect_identical(attr(k, "keyability"), rep("ok", 5L))
  expect_identical(anyDuplicated(as.character(unclass(k))), 0L)
  # The fragment and the userinfo are in no key (ratification Q5).
  expect_true(srel_same("//h.com/a", "//h.com/a#f"))
  expect_true(srel_same("//h.com/a", "//u:pw@h.com/a"))
  # Path, query and port stay significant.
  expect_false(srel_same("//p", "//p/"))
  expect_false(srel_same("//h.com/a", "//h.com/b"))
  expect_false(srel_same("//h.com/a", "//h.com/a?"))
  expect_false(srel_same("//h.com/a", "//h.com:80/a"))
  # Scheme presence: a network-path reference is neither an explicit-scheme
  # URI nor a scheme-less host-shaped input.
  expect_false(srel_same("//h.com/a", "http://h.com/a"))
  expect_false(srel_same("//h.com/a", "https://h.com/a"))
  expect_false(srel_same("//h.com/a", "h.com/a"))
  expect_false(srel_same("//h.com/a", "/h.com/a"))
})

test_that("whatwg keys refuse every // row", {
  k <- srel_key(c(srel_hosts, srel_empty), "whatwg")
  expect_identical(attr(k, "keyability"), rep("invalid-parse", 11L))
})

test_that("serialize_url() gives NA for every // row, both standards", {
  x <- c(srel_hosts, srel_empty)
  na <- rep(NA_character_, length(x))
  expect_identical(serialize_url(x, "rfc3986"), na)
  expect_identical(serialize_url(x, "rfc3986", form = "normalized"), na)
  expect_identical(serialize_url(x, "whatwg"), na)
})
