# RURL-pntbujzc: under `url_standard = "rfc3986"` a directly written
# non-ASCII code point is a parse error (owner ruling 2026-10-12, RUL-030).
#
# RFC 3986 section 2: a URI is "a sequence of characters chosen from a limited
# subset of the repertoire of US-ASCII characters". Section 2.5: non-ASCII
# data is written as percent-encoded UTF-8 octets. Section 3.2.2:
# `reg-name = *( unreserved / pct-encoded / sub-delims )`, all ASCII. Unicode
# input belongs to `url_standard = "whatwg"`; no RFC 3987 (IRI) mode exists.
#
# The first block pins what must NOT move: the percent-encoded twins and ASCII
# rows under `rfc3986`, and every raw non-ASCII row under `whatwg` and under
# the frozen `NULL` default (ADR 0007, ADR 0016).

cols <- c("parse_status", "host", "path", "query", "fragment", "clean_url")
frame <- function(url, ...) {
  df <- safe_parse_urls(url, ...)
  lapply(df[cols], identity)
}

a_uml <- "ä"
bom <- "\ufeff"
raw_rows <- c(
  paste0("http://b", "ü", "cher.de/"),
  paste0("http://h.de/", a_uml),
  paste0("http://h.de/?q=", a_uml),
  paste0("http://h.de/#", a_uml),
  paste0("http://", a_uml, "@h.de/"),
  paste0("http://", bom, "h.de/"),
  paste0("foo:", a_uml),
  paste0("foo://", a_uml, "/x")
)
twin_rows <- c(
  "http://h.de/%C3%A4", "http://b%C3%BCcher.de/", "http://h.de/?q=%C3%A4",
  "http://h.de/#%C3%A4", "http://%C3%A4@h.de/", "foo:%C3%A4",
  "foo://%C3%A4/x", "http://example.com/a?b#c", "mailto:a@b.com",
  "file:///x"
)

# --- Pins: behavior the strictness change must not move ----------------------

test_that("rfc3986: percent-encoded twins and ASCII rows keep their parse", {
  web <- list(
    parse_status = c("ok", "ok", "ok", "ok", "ok", "error", "error", "ok",
                     "error", "ok"),
    host = c("h.de", "b%C3%BCcher.de", "h.de", "h.de", "h.de", NA, NA,
             "example.com", NA, NA),
    path = c("/%C3%A4", "/", "/", "/", "/", NA, NA, "/a", NA, "/x"),
    query = c(NA, NA, "q=%C3%A4", NA, NA, NA, NA, "b", NA, NA),
    fragment = c(NA, NA, NA, "%C3%A4", NA, NA, NA, "c", NA, NA),
    clean_url = c("http://h.de/%C3%A4", "http://b%C3%BCcher.de/",
                  "http://h.de/", "http://h.de/", "http://h.de/", NA, NA,
                  "http://example.com/a", NA, "file:///x")
  )
  expect_identical(frame(twin_rows, url_standard = "rfc3986"), web)

  general <- list(
    parse_status = rep("ok", 10L),
    host = c("h.de", "b%C3%BCcher.de", "h.de", "h.de", "h.de", NA, "%C3%A4",
             "example.com", NA, NA),
    path = c("/%C3%A4", "/", "/", "/", "/", "%C3%A4", "/x", "/a", "a@b.com",
             "/x"),
    query = c(NA, NA, "q=%C3%A4", NA, NA, NA, NA, "b", NA, NA),
    fragment = c(NA, NA, NA, "%C3%A4", NA, NA, NA, "c", NA, NA),
    clean_url = c("http://h.de/%C3%A4", "http://b%C3%BCcher.de/",
                  "http://h.de/", "http://h.de/", "http://h.de/",
                  "foo:%C3%A4", "foo://%C3%A4/x", "http://example.com/a",
                  "mailto:a@b.com", "file:///x")
  )
  expect_identical(
    frame(twin_rows, url_standard = "rfc3986", scheme_acceptance = "general"),
    general
  )
  expect_identical(frame(twin_rows, profile = "rfc-syntax"), general)

  expect_identical(serialize_url(twin_rows, standard = "rfc3986"), c(
    "http://h.de/%C3%A4", "http://b%C3%BCcher.de/", "http://h.de/?q=%C3%A4",
    "http://h.de/#%C3%A4", "http://%C3%A4@h.de/", "foo:%C3%A4",
    "foo://%C3%A4/x", "http://example.com/a?b#c", "mailto:a@b.com",
    "file:///x"
  ))
  expect_identical(
    serialize_url(twin_rows, standard = "rfc3986", form = "normalized"),
    serialize_url(twin_rows, standard = "rfc3986")
  )
  key <- get_url_key(twin_rows, url_key_policy("rfc3986"))
  expect_identical(attr(key, "keyability"), rep("ok", 10L))
  expect_identical(
    get_url_diagnostics(twin_rows, "rfc3986", scheme_acceptance = "general"),
    list(character(0), "domain-std3-violation", character(0), character(0),
         character(0), "transform-skipped-ineligible-scheme",
         c("domain-std3-violation", "transform-skipped-ineligible-scheme"),
         character(0), "transform-skipped-ineligible-scheme",
         "transform-skipped-ineligible-scheme")
  )
})

test_that("whatwg: raw non-ASCII rows keep their parse on both routes", {
  hb <- paste0("b", "ü", "cher.de")
  web <- list(
    parse_status = c("ok", "ok", "ok", "ok", "ok", "ok", "error", "error"),
    host = c(hb, "h.de", "h.de", "h.de", "h.de", "h.de", NA, NA),
    path = c("/", paste0("/", a_uml), "/", "/", "/", "/", NA, NA),
    query = c(NA, NA, "q=%C3%A4", NA, NA, NA, NA, NA),
    fragment = c(NA, NA, NA, "%C3%A4", NA, NA, NA, NA),
    clean_url = c(paste0("http://", hb, "/"), paste0("http://h.de/", a_uml),
                  "http://h.de/", "http://h.de/", "http://h.de/",
                  "http://h.de/", NA, NA)
  )
  expect_identical(frame(raw_rows, url_standard = "whatwg"), web)

  general <- web
  general$parse_status <- rep("ok", 8L)
  general$host[8L] <- "%C3%A4"
  general$path[7:8] <- c("%C3%A4", "/x")
  general$clean_url[7:8] <- c("foo:%C3%A4", "foo://%C3%A4/x")
  expect_identical(
    frame(raw_rows, url_standard = "whatwg", scheme_acceptance = "general"),
    general
  )
  expect_identical(serialize_url(raw_rows, standard = "whatwg"), c(
    "http://xn--bcher-kva.de/", "http://h.de/%C3%A4", "http://h.de/?q=%C3%A4",
    "http://h.de/#%C3%A4", "http://%C3%A4@h.de/", "http://h.de/",
    "foo:%C3%A4", "foo://%C3%A4/x"
  ))
  expect_identical(attr(get_url_key(raw_rows), "keyability"), rep("ok", 8L))
})

test_that("NULL: raw non-ASCII rows keep their parse (frozen, ADR 0016)", {
  hb <- paste0("b", "ü", "cher.de")
  expect_identical(frame(raw_rows), list(
    parse_status = c("ok", "ok", "ok", "ok", "ok", "ok", "error", "error"),
    host = c(hb, "h.de", "h.de", "h.de", "h.de", paste0(bom, "h.de"), NA, NA),
    path = c("/", paste0("/", a_uml), "/", "/", "/", "/", NA, NA),
    query = c(NA, NA, "q=%C3%A4", NA, NA, NA, NA, NA),
    fragment = c(NA, NA, NA, "%C3%A4", NA, NA, NA, NA),
    clean_url = c(paste0("http://", hb, "/"), paste0("http://h.de/", a_uml),
                  "http://h.de/", "http://h.de/", "http://h.de/",
                  paste0("http://", bom, "h.de/"), NA, NA)
  ))
  # The `file:` overlay runs the RFC 3986 grammar under NULL too; its
  # non-ASCII tolerance there is the frozen NULL behavior.
  expect_identical(
    get_parse_status(c(paste0("file:///", a_uml), file.path(hb, "p"))),
    c("ok", "ok")
  )
})

# --- The change: raw non-ASCII is a parse error under rfc3986 ----------------

test_that("rfc3986: a raw non-ASCII code point fails in every component", {
  # raw_rows covers the host, path, query, fragment, userinfo, a mark that
  # starts the host, and (general route) an opaque path and a non-special
  # authority. RFC 3986 S2, S2.5, S3.2.2.
  expect_identical(
    get_parse_status(raw_rows[1:6], url_standard = "rfc3986"),
    rep("error", 6L)
  )
  for (acc in c("web", "general")) {
    p <- safe_parse_urls(raw_rows, url_standard = "rfc3986",
                         scheme_acceptance = acc)
    expect_identical(p$parse_status, rep("error", 8L), label = acc)
    expect_true(all(is.na(p$clean_url)), label = acc)
    expect_true(all(is.na(p$host)), label = acc)
  }
  p <- safe_parse_urls(raw_rows, profile = "rfc-syntax")
  expect_identical(p$parse_status, rep("error", 8L))
  # Scheme inference does not launder it: a scheme-less Unicode host fails.
  expect_identical(
    get_parse_status(paste0("b", "ü", "cher.de"),
                     url_standard = "rfc3986"),
    "error"
  )
  # A latin1-marked twin fails like its UTF-8 spelling.
  l1 <- "http://h.de/\xe4"
  Encoding(l1) <- "latin1"
  expect_identical(get_parse_status(l1, url_standard = "rfc3986"), "error")
})

test_that("every rfc3986 surface follows the parse error", {
  x <- c(raw_rows[c(1L, 2L, 5L)], "http://h.de/%C3%A4")
  want <- c(NA, NA, NA, "http://h.de/%C3%A4")
  expect_identical(serialize_url(x, standard = "rfc3986"), want)
  expect_identical(
    serialize_url(x, standard = "rfc3986", form = "normalized"), want
  )
  key <- get_url_key(x, url_key_policy("rfc3986"))
  expect_identical(attr(key, "keyability"),
                   c(rep("invalid-parse", 3L), "ok"))
  expect_identical(
    resolve_url(c(a_uml, "x"), "http://h.de/", url_standard = "rfc3986"),
    c(NA, "http://h.de/x")
  )
  # A refused base leaves a relative reference unresolved (RFC 3986 S5.1).
  expect_identical(
    resolve_url("x", paste0("http://h.de/", a_uml), url_standard = "rfc3986"),
    NA_character_
  )
  expect_identical(
    check_schemes(x, url_standard = "rfc3986")$scheme,
    c(NA, NA, NA, "http")
  )
  expect_identical(unname(get_host(x, url_standard = "rfc3986")),
                   c(NA, NA, NA, "h.de"))
  expect_identical(unname(get_clean_url(x, url_standard = "rfc3986")), want)
  expect_identical(get_host_type(x, "rfc3986"), c(NA, NA, NA, "domain"))
  expect_identical(
    get_parse_verdicts(x, url_standard = "rfc3986")$layer1_syntax_verdict,
    c("fail", "fail", "fail", "pass")
  )
  expect_null(safe_parse_url(raw_rows[2L], url_standard = "rfc3986"))
})

test_that("unicode-outside-rfc3986-uri is the rejection reason", {
  for (acc in c("web", "general")) {
    d <- get_url_diagnostics(c(raw_rows, "http://h.de/%C3%A4"), "rfc3986",
                             scheme_acceptance = acc)
    expect_identical(d[1:8], rep(list("unicode-outside-rfc3986-uri"), 8L),
                     label = acc)
    expect_false("unicode-outside-rfc3986-uri" %in% d[[9L]], label = acc)
  }
  # whatwg never reports it.
  d <- get_url_diagnostics(raw_rows, "whatwg", scheme_acceptance = "general")
  expect_false(any(vapply(d, function(t) {
    "unicode-outside-rfc3986-uri" %in% t
  }, logical(1L))))
})
