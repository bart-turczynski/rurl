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

test_that("//host rows keep their parse under whatwg, NULL and seo", {
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

# The empty-authority shapes under `whatwg`, `NULL` and the profiles built on
# them. They read the leading slashes the WHATWG way: the special authority
# state skips the slash run, so `///p` reads host `p`, and `////p` has no
# WHATWG answer rurl reproduces (rejected). Neither RURL-ozpxupgq nor
# RURL-vubwgoxo moves them; rfc3986 with "http"/"https" is pinned below.
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
  srel_expect(srel_empty, list(scheme_relative_handling = "https"),
              srel_empty_fields("https", "/", "https://p/"), "NULL https")
  srel_expect(srel_empty,
              list(url_standard = "whatwg", scheme_relative_handling = "http"),
              srel_empty_fields("http", "/", "http://p/"), "whatwg http")
  srel_expect(srel_empty,
              list(url_standard = "whatwg", scheme_relative_handling = "https"),
              srel_empty_fields("https", "/", "https://p/"), "whatwg https")
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

# --- the fix ----------------------------------------------------------------
#
# Under the rfc3986 "keep" posture the empty-authority shapes go to the RFC
# generic parser, which reads them as RFC 3986 sections 3.2 and 4.2 do.

test_that("///p is an empty authority and the path /p under rfc-syntax", {
  r <- safe_parse_urls(c("///p", "//p"), profile = "rfc-syntax")
  expect_identical(r$scheme, c(NA_character_, NA_character_))
  expect_identical(r$host, c(NA_character_, "p"))
  expect_identical(r$path, c("/p", ""))
  # "keep" parses using http and reports no scheme, so clean_url keeps the
  # stand-in, in the shape the corrected parse gives.
  expect_identical(r$clean_url, c("http:///p", "http://p"))
  expect_identical(r$parse_status, c("ok-scheme-relative", "warning-no-tld"))
})

test_that("every empty-authority shape reads per RFC 3986 under rfc-syntax", {
  x <- c("///p", "////p", "///", "//", "//u@/p", "//:80/x",
         "///example.com/x", "//?q")
  r <- safe_parse_urls(x, profile = "rfc-syntax")
  expect_identical(r$scheme, rep(NA_character_, 8L))
  expect_identical(r$host, rep(NA_character_, 8L))
  expect_identical(r$path, c("/p", "//p", "/", "", "/p", "/x",
                             "/example.com/x", ""))
  expect_identical(r$port, c(rep(NA_integer_, 5L), 80L, NA, NA))
  expect_identical(r$user, c(rep(NA_character_, 4L), "u", rep(NA, 3L)))
  expect_identical(r$query, c(rep(NA_character_, 7L), "q"))
  expect_identical(r$parse_status, rep("ok-scheme-relative", 8L))
  # The same components the explicit `http:` twin gets under this profile.
  twin <- safe_parse_urls(paste0("http:", x), profile = "rfc-syntax")
  for (cl in c("host", "port", "path", "query", "user", "clean_url")) {
    expect_identical(r[[cl]], twin[[cl]], label = cl)
  }
})

test_that("url_standard = 'rfc3986' reads ///p as its http: twin does", {
  # The default `web` acceptance rejects an empty host on a web-route scheme
  # (RURL-mugcdtrv), as it rejects `http:///p` here.
  x <- c("///p", "///example.com/x")
  r <- safe_parse_urls(x, url_standard = "rfc3986")
  twin <- safe_parse_urls(paste0("http:", x), url_standard = "rfc3986")
  expect_identical(r$parse_status, c("error", "error"))
  expect_identical(r$parse_status, twin$parse_status)
  expect_identical(r$host, twin$host)
  r <- safe_parse_urls(x, url_standard = "rfc3986",
                       scheme_acceptance = "general",
                       scheme_policy = "require")
  expect_identical(r$host, c(NA_character_, NA_character_))
  expect_identical(r$path, c("/p", "/example.com/x"))
})

test_that("///p and //p key apart under the rfc3986 key policy", {
  expect_false(srel_same("///p", "//p"))
  expect_false(srel_same("///example.com/x", "//example.com/x"))
  expect_false(srel_same("///p", "/p"))
  expect_false(srel_same("///p", "http:///p"))
  expect_false(srel_same("//", "///"))
  k <- srel_key(c("//p", "///p", "//"), "rfc3986")
  expect_identical(attr(k, "keyability"), rep("ok", 3L))
  expect_identical(
    as.character(unclass(k)),
    c(
      paste0("1:21:17:rfc39865:exact15:scheme-relative-:1:T7:present1:p",
             "-:4:list7:abempty0:6:absent-:"),
      paste0("1:21:17:rfc39865:exact15:scheme-relative-:1:T5:empty0:",
             "-:4:list7:abempty2:/p6:absent-:"),
      paste0("1:21:17:rfc39865:exact15:scheme-relative-:1:T5:empty0:",
             "-:4:list7:abempty0:6:absent-:")
    )
  )
})

test_that("the record gives a // row no scheme and its leading // delimiter", {
  x <- c("//p", "///p", "//h.com/a?q", "//")
  key_opts <- rurl:::.url_key_parse_options(
    url_key_policy(standard = "rfc3986")
  )
  for (opts in list(NULL, key_opts)) {
    rec <- rurl:::.fsss_record_vec(x, "rfc3986", NULL, opts = opts)
    expect_identical(rec$ok, rep(TRUE, 4L))
    expect_identical(rec$scheme, rep(NA_character_, 4L))
    expect_identical(rec$source_scheme, rep(NA_character_, 4L))
    expect_identical(rec$authority_delimiter_present, rep(TRUE, 4L))
    expect_identical(rec$host_kind, c("present", "empty", "present", "empty"))
    expect_identical(rec$path, c("", "/p", "/a", ""))
    expect_identical(rec$path_kind, rep("list", 4L))
    expect_identical(rec$rfc_path_form, rep("abempty", 4L))
    expect_identical(rec$scheme_relative, rep(TRUE, 4L))
  }
  # An absolute URI in the same call keeps its own scheme and delimiter.
  rec <- rurl:::.fsss_record_vec(c("//p", "http://p", "http:/p"), "rfc3986")
  expect_identical(rec$scheme, c(NA, "http", "http"))
  expect_identical(rec$authority_delimiter_present, c(TRUE, TRUE, FALSE))
})

# --- RURL-vubwgoxo: rfc3986 with "http" / "https" ----------------------------
#
# These handlings read a `//` row as if it began with that scheme, so under
# `rfc3986` the row parses as its spelled `http:` / `https:` twin does
# (RFC 3986 sections 3.2 and 4.2; the RURL-kmkyicpt ruling). Before the fix
# the web route read the stand-in with the special-scheme authority, which
# skips a slash run, so `///p` read as host `p` and the other empty-authority
# shapes failed where their twins parse under general acceptance.

srel_twin_rows <- c(srel_empty, "///example.com/x", "//?q")
srel_twin_cols <- c("scheme", "host", "port", "path", "query", "fragment",
                    "user", "password", "domain", "tld", "domain_ascii",
                    "domain_unicode", "tld_ascii", "tld_unicode", "is_ip_host",
                    "clean_url", "parse_status")

srel_twin_check <- function(x, handling, acceptance, policy) {
  args <- list(url_standard = "rfc3986", scheme_relative_handling = handling,
               scheme_acceptance = acceptance, scheme_policy = policy)
  r <- do.call(safe_parse_urls, c(list(x), args))
  twin <- do.call(safe_parse_urls, c(list(paste0(handling, ":", x)), args))
  for (cl in srel_twin_cols) {
    expect_identical(r[[cl]], twin[[cl]],
                     label = paste(handling, acceptance, policy, cl))
  }
}

test_that("rfc3986 http/https: empty-authority rows read as their twins", {
  for (h in c("http", "https")) {
    for (acc in c("web", "general")) {
      for (sp in c("infer", "require")) {
        srel_twin_check(srel_twin_rows, h, acc, sp)
      }
    }
  }
})

test_that("rfc3986 http/https: //host rows still read as their twins", {
  for (h in c("http", "https")) {
    for (acc in c("web", "general")) {
      srel_twin_check(srel_hosts, h, acc, "infer")
    }
  }
})

test_that("rfc3986 http: ///p is an empty authority and the path /p", {
  r <- safe_parse_urls(c("///p", "//p", "////p", "//u@/p", "//:80/x"),
                       url_standard = "rfc3986",
                       scheme_relative_handling = "http",
                       scheme_acceptance = "general")
  expect_identical(r$scheme, rep("http", 5L))
  expect_identical(r$host, c(NA, "p", NA, NA, NA))
  expect_identical(r$path, c("/p", "", "//p", "/p", "/x"))
  expect_identical(r$port, c(NA, NA, NA, NA, 80L))
  expect_identical(r$user, c(NA, NA, NA, "u", NA))
  expect_identical(r$clean_url, c("http:///p", "http://p", "http:////p",
                                  "http:///p", "http:///x"))
  expect_identical(r$parse_status,
                   c("ok", "warning-no-tld", "ok", "ok", "ok"))
  r <- safe_parse_urls(c("///p", "///"), url_standard = "rfc3986",
                       scheme_relative_handling = "https",
                       scheme_acceptance = "general")
  expect_identical(r$scheme, c("https", "https"))
  expect_identical(r$host, c(NA_character_, NA_character_))
  expect_identical(r$path, c("/p", "/"))
  expect_identical(r$clean_url, c("https:///p", "https:///"))
  # Under the default web acceptance the empty host fails, as it does for
  # `http:///p` (RURL-mugcdtrv), where `///p` had parsed as host `p`.
  r <- safe_parse_urls("///p", url_standard = "rfc3986",
                       scheme_relative_handling = "http")
  expect_identical(r$parse_status, "error")
  expect_identical(r$host, NA_character_)
})

test_that("the rfc3986 key and serializer read // rows under keep", {
  # The key's rfc3986 posture is the rfc-syntax bundle, so a caller's
  # "http"/"https" choice never reaches it.
  key_opts <- rurl:::.url_key_parse_options(
    url_key_policy(standard = "rfc3986")
  )
  expect_identical(key_opts$scheme_relative_handling, "keep")
  k <- srel_key(c("///p", "//p", "////p", "//u@/p", "//:80/x"), "rfc3986")
  expect_identical(attr(k, "keyability"), rep("ok", 5L))
  # The userinfo is in no key (ratification Q5), so `//u@/p` keys as `///p`.
  pre <- "1:21:17:rfc39865:exact15:scheme-relative-:1:T"
  expect_identical(
    as.character(unclass(k)),
    paste0(pre, c(
      "5:empty0:-:4:list7:abempty2:/p6:absent-:",
      "7:present1:p-:4:list7:abempty0:6:absent-:",
      "5:empty0:-:4:list7:abempty3://p6:absent-:",
      "5:empty0:-:4:list7:abempty2:/p6:absent-:",
      "5:empty0:2:804:list7:abempty2:/x6:absent-:"
    ))
  )
  x <- c(srel_hosts, srel_empty)
  expect_identical(serialize_url(x, "rfc3986"), rep(NA_character_, 11L))
})
