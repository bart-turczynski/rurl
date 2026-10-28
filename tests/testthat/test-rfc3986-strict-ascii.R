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
# rows under `rfc3986`, and every raw non-ASCII row under `whatwg`, the
# default (`NULL` reads as `whatwg`, ADR 0020).

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

test_that("NULL and the default read raw non-ASCII rows as whatwg", {
  # ADR 0020 D4: the frozen profile's tolerance is retired; NULL is whatwg.
  hb <- paste0("b", "ü", "cher.de")
  expect_identical(frame(raw_rows), frame(raw_rows, url_standard = "whatwg"))
  expect_identical(frame(raw_rows, url_standard = NULL),
                   frame(raw_rows, url_standard = "whatwg"))
  # WHATWG has its own `file:` state machine; the RFC 8089 overlay is the
  # rfc3986 arm's alone.
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
  tok <- "unicode-outside-rfc3986-uri"
  # Under `general` every raw row's percent-encoded twin parses, so each
  # failed row names the non-ASCII as its reason.
  d <- get_url_diagnostics(c(raw_rows, "http://h.de/%C3%A4"), "rfc3986",
                           scheme_acceptance = "general")
  expect_identical(d[1:8], rep(list(tok), 8L))
  expect_false(tok %in% d[[9L]])
  # Under `web` the two `foo:` rows fail on their scheme too: their twins
  # (`foo:%C3%A4`, `foo://%C3%A4/x`) are errors there, so percent-encoding
  # would not rescue them and the token does not claim it would.
  d <- get_url_diagnostics(c(raw_rows, "http://h.de/%C3%A4"), "rfc3986",
                           scheme_acceptance = "web")
  expect_identical(d[1:6], rep(list(tok), 6L))
  expect_identical(d[7:9], rep(list(character(0)), 3L))
  # whatwg never reports it.
  d <- get_url_diagnostics(raw_rows, "whatwg", scheme_acceptance = "general")
  expect_false(any(vapply(d, function(t) {
    "unicode-outside-rfc3986-uri" %in% t
  }, logical(1L))))
})

test_that("the token is withheld where percent-encoding would not rescue", {
  # The token's advice is "percent-encode it as UTF-8": it fires only on a
  # failed row whose percent-encoded twin parses, so a row that also fails
  # for another reason (a D1 not-host-like input, `scheme_policy =
  # "require"`, an ASCII grammar fault) carries no token.
  tok <- "unicode-outside-rfc3986-uri"
  rows <- c(paste0("h", a_uml, "llo w"), paste0("http://h.de/", a_uml, "|"))
  twins <- c("h%C3%A4llo w", "http://h.de/%C3%A4|")
  expect_identical(get_parse_status(rows, url_standard = "rfc3986"),
                   c("error", "error"))
  expect_identical(get_parse_status(twins, url_standard = "rfc3986"),
                   c("error", "error"))
  expect_identical(get_url_diagnostics(rows, url_standard = "rfc3986"),
                   list(character(0), character(0)))
  sl <- paste0("b", "ü", "cher.de/p")
  expect_identical(
    get_parse_status(c(sl, "b%C3%BCcher.de/p"), url_standard = "rfc3986",
                     scheme_policy = "require"),
    c("error", "error")
  )
  expect_identical(
    get_url_diagnostics(sl, url_standard = "rfc3986",
                        scheme_policy = "require"),
    character(0)
  )
  # Without `require` the twin infers its scheme and parses, so the raw row
  # names its non-ASCII.
  expect_identical(get_parse_status("b%C3%BCcher.de/p",
                                    url_standard = "rfc3986"), "ok")
  expect_identical(get_url_diagnostics(sl, url_standard = "rfc3986"),
                   tok)
})

# --- resolve_url(): the reference itself must be an RFC 3986 URI-reference --

test_that("rfc3986 resolve_url() refuses a reference outside the grammar", {
  # RFC 3986 section 4.1: URI-reference = URI / relative-ref. A reference
  # outside that grammar is no reference to resolve, even where section
  # 5.2.4's dot-segment removal would delete the offending segment.
  base <- "http://b.de/a/"
  bad <- c(
    "https://h.de/a/ä/../b",
    "ä/../b",
    "https://h.de/a/|/../b",
    "/%zz/../b",
    "foo:/a/ä/../b",
    # section 4.2: a first segment holding a colon cannot start a
    # relative-path reference (path-noscheme, segment-nz-nc).
    "1a:b"
  )
  good <- c("g", "../g", "?q", "#f", "//h/p", "%C3%A4/../b", "", "./1a:b")
  good_want <- c(
    "http://b.de/a/g", "http://b.de/g", "http://b.de/a/?q",
    "http://b.de/a/#f", "http://h/p", "http://b.de/a/b", "http://b.de/a/",
    "http://b.de/a/1a:b"
  )
  # Interleaved, so one refused row moves no other row.
  refs <- c(rbind(bad, good[seq_along(bad)]), good[-seq_along(bad)])
  want <- c(rbind(rep(NA_character_, length(bad)),
                  good_want[seq_along(bad)]),
            good_want[-seq_along(bad)])
  expect_identical(
    resolve_url(refs, base, url_standard = "rfc3986", output = "serialized"),
    want
  )
  expect_identical(
    resolve_url(bad, base, url_standard = "rfc3986"),
    rep(NA_character_, length(bad))
  )
  # whatwg, the default, does not check the reference.
  expect_identical(
    resolve_url(bad, base, url_standard = "whatwg", output = "serialized"),
    c("https://h.de/a/b", "http://b.de/a/b", "https://h.de/a/b",
      "http://b.de/b", "foo:/a/b", "http://b.de/a/1a:b")
  )
  expect_identical(resolve_url(bad, base),
                   resolve_url(bad, base, url_standard = "whatwg"))
})
