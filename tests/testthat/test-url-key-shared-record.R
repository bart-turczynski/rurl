# The URL key and the WHATWG serializer read one encoded record
# (RURL-ommuyaqi).
#
# The WHATWG URL Standard percent-encodes the path and the query while it
# parses (path state: the path percent-encode set; opaque path state: the
# C0-control set; query state: the query or special-query set), so the URL
# record holds them encoded, and two URLs whose serializations are equal are
# equal. `.fsss_record_vec()` keeps both as written and left the encoding to
# the serializer, so `get_url_key()` under the `whatwg` policy framed a list
# path the serializer would have escaped: `http://h/p{` and `http://h/p%7B`
# keyed apart, though both serialize as `http://h/p%7B`. RURL-pjrqnqvb had
# patched the query alone, inside R/url-key.R.
#
# Pins first: the `rfc3986` keys, the `whatwg` query keys and the output of
# `serialize_url()` under every standard and form must not move.

sr_key <- function(u, std = "whatwg") {
  as.character(get_url_key(u, url_key_policy(std)))
}

sr_fields <- function(u, std = "whatwg") {
  .url_key_state_vec(u, url_key_policy(std))$fields
}

sr_same <- function(u, std = "whatwg") {
  k <- sr_key(u, std)
  identical(k[[1L]], k[[2L]])
}

# --- Pins: behavior the change must not move ----------------------------------

test_that("serialize_url(standard = \"whatwg\") output is pinned", {
  u <- c(
    "HTTP://u:p w@Example.COM:80/a/../b{?q'x#f`",
    "foo://H/p q^?x y'#z",
    "foo:op aque ?q",
    "mailto:a@b.com?subject=hi there",
    "file:///C:/a/../b{",
    "http://[2001:DB8::1]:8080/p\u200b",
    "http://h/p\u200b?\u200b#\u200b",
    "foo:/.//p",
    "wss://h:443/%7e{}?%7e"
  )
  expect_identical(serialize_url(u, "whatwg"), c(
    "http://u:p%20w@example.com/b%7B?q%27x#f%60",
    "foo://H/p%20q%5E?x%20y'#z",
    "foo:op aque%20?q",
    "mailto:a@b.com?subject=hi%20there",
    "file:///C:/b%7B",
    "http://[2001:db8::1]:8080/p%E2%80%8B",
    "http://h/p%E2%80%8B?%E2%80%8B#%E2%80%8B",
    "foo:/.//p",
    "wss://h/%7e%7B%7D?%7e"
  ))
})

sr_rfc_rows <- c(
  "HTTP://u:pw@Example.COM:80/a/../b?q=1#f",
  "foo://H/p%7e?x",
  "foo:op/aque?q",
  "file:///C:/a/../b",
  "http://[2001:DB8::1]:8080/p\u200b",
  "http://h/p\u200b?\u200b#\u200b",
  "foo:/.//p",
  "foo:/p\u200b"
)

test_that("serialize_url(standard = \"rfc3986\") output is pinned", {
  expect_identical(serialize_url(sr_rfc_rows, "rfc3986", "source"), c(
    "HTTP://u:pw@Example.COM:80/a/../b?q=1#f",
    "foo://H/p%7e?x",
    "foo:op/aque?q",
    "file:///C:/a/../b",
    "http://[2001:DB8::1]:8080/p\u200b",
    "http://h/p\u200b?\u200b#\u200b",
    "foo:/.//p",
    "foo:/p\u200b"
  ))
  expect_identical(serialize_url(sr_rfc_rows, "rfc3986", "normalized"), c(
    "http://u:pw@example.com/b?q=1#f",
    "foo://h/p~?x",
    "foo:op/aque?q",
    "file:///C:/b",
    "http://[2001:db8::1]:8080/p\u200b",
    "http://h/p\u200b?%E2%80%8B#%E2%80%8B",
    "foo:/.//p",
    "foo:/p\u200b"
  ))
})

test_that("rfc3986 keys are pinned byte for byte", {
  expect_identical(sr_key(sr_rfc_rows, "rfc3986"), c(
    paste0("1:21:17:rfc39865:exact8:explicit4:http1:T7:present11:Example.COM",
           "-:4:list7:abempty7:/a/../b7:present3:q=1"),
    paste0("1:21:17:rfc39865:exact8:explicit3:foo1:T7:present1:H-:4:list",
           "7:abempty5:/p%7e7:present1:x"),
    paste0("1:21:17:rfc39865:exact8:explicit3:foo1:F6:absent-:-:4:list",
           "8:rootless7:op/aque7:present1:q"),
    paste0("1:21:17:rfc39865:exact8:explicit4:file1:T5:empty0:-:4:list",
           "7:abempty10:/C:/a/../b6:absent-:"),
    paste0("1:21:17:rfc39865:exact8:explicit4:http1:T7:present",
           "13:[2001:DB8::1]4:80804:list7:abempty5:/p\u200b6:absent-:"),
    paste0("1:21:17:rfc39865:exact8:explicit4:http1:T7:present1:h-:4:list",
           "7:abempty5:/p\u200b7:present3:\u200b"),
    paste0("1:21:17:rfc39865:exact8:explicit3:foo1:F6:absent-:-:4:list",
           "8:absolute5:/.//p6:absent-:"),
    paste0("1:21:17:rfc39865:exact8:explicit3:foo1:F6:absent-:-:4:list",
           "8:absolute5:/p\u200b6:absent-:")
  ))
})

test_that("rfc3986 keeps raw and encoded twins apart, path and query", {
  # RFC 3986 section 6.2.2 normalization is a comparison ladder the key does
  # not climb: the rfc3986 key frames the source spelling (section 6.2.2.2
  # would not equate `{` and `%7B` anyway, since `{` is not a URI character).
  twins <- list(
    c("foo:/p\u200b", "foo:/p%E2%80%8B"),
    c("http://h/p\u200b", "http://h/p%E2%80%8B"),
    c("file:///p\u200b", "file:///p%E2%80%8B"),
    c("foo:a\u200b", "foo:a%E2%80%8B"),
    c("http://h/?\u200b", "http://h/?%E2%80%8B")
  )
  for (tw in twins) {
    expect_false(sr_same(tw, "rfc3986"), label = tw[[1L]])
  }
  f <- sr_fields(c("foo:/p\u200b", "foo:a\u200b"), "rfc3986")
  expect_identical(f$path, c("/p\u200b", "a\u200b"))
})

test_that("whatwg: the query keys as the standard serializes it", {
  # RURL-pjrqnqvb's cases: twins the standard serializes alike key alike, and
  # twins it serializes apart stay apart.
  expect_true(sr_same(c("foo:/p?\u200bq", "foo:/p?%E2%80%8Bq")))
  expect_true(sr_same(c("http://h/p?'q", "http://h/p?%27q")))
  expect_true(sr_same(c("h.com/p?'q", "h.com/p?%27q")))
  expect_false(sr_same(c("foo:/p?'q", "foo:/p?%27q")))
  expect_false(sr_same(c("http://h/?q%7f", "http://h/?q%7F")))
  # `{` is in no query percent-encode set, so WHATWG serializes these two
  # apart and the key must too.
  expect_identical(serialize_url(c("http://h/?q{", "http://h/?q%7B")),
                   c("http://h/?q{", "http://h/?q%7B"))
  expect_false(sr_same(c("http://h/?q{", "http://h/?q%7B")))
  expect_identical(sr_fields(c("foo:/p?\u200bq", "http://h/?q{"))$query,
                   c("%E2%80%8Bq", "q{"))
})

test_that("whatwg: an opaque path already keys as the standard holds it", {
  # The opaque path state encodes as it parses, and the record already holds
  # that: a trailing space before `?` or `#` is `%20`, and `^` stays literal.
  expect_true(sr_same(c("foo:a\u200b", "foo:a%E2%80%8B")))
  expect_true(sr_same(c("foo:a #f", "foo:a%20")))
  expect_false(sr_same(c("foo:a^", "foo:a%5E")))
  expect_identical(sr_fields(c("foo:op aque ?q", "foo:a^"))$path,
                   c("op aque%20", "a^"))
})

test_that("the fragment and userinfo stay out of the key", {
  for (std in c("whatwg", "rfc3986")) {
    expect_true(sr_same(c("http://u\u200b:p@h/p#\u200b", "http://h/p"), std),
                label = std)
  }
})

# --- Fix: the whatwg key frames the components the serializer writes ----------

# Twins: a raw spelling and its WHATWG serialization, across the special,
# non-special, `file:` and inferred-scheme list-path routes.
sr_path_twins <- list(
  c("foo:/p\u200b", "foo:/p%E2%80%8B"),
  c("http://h/p\u200b", "http://h/p%E2%80%8B"),
  c("http://h/p{", "http://h/p%7B"),
  c("http://h/p\"q\"", "http://h/p%22q%22"),
  c("https://h/a b", "https://h/a%20b"),
  c("foo://h/p^`{}", "foo://h/p%5E%60%7B%7D"),
  c("file:///p\u200b", "file:///p%E2%80%8B"),
  c("file:///C:/a{", "file:///C:/a%7B"),
  c("http://[::1]/p{", "http://[::1]/p%7B")
)

test_that("whatwg keys a list-path twin pair alike", {
  for (tw in sr_path_twins) {
    expect_identical(serialize_url(tw, "whatwg"), c(tw[[2L]], tw[[2L]]),
                     label = tw[[2L]])
    expect_true(sr_same(tw), label = tw[[1L]])
  }
  # A row whose scheme the key infers reads its path as the `http` it parses
  # under, so its twins key alike too.
  expect_true(sr_same(c("h.com/p{", "h.com/p%7B")))
  expect_identical(
    sr_fields(c("http://h/p{", "foo:/p\u200b", "file:///C:/a{"))$path,
    c("/p%7B", "/p%E2%80%8B", "/C:/a%7B")
  )
  expect_identical(
    duplicated(get_url_key(c("http://h/p{", "http://h/p%7B"))),
    c(FALSE, TRUE)
  )
})

test_that("whatwg: twins the standard serializes apart still key apart", {
  # A triplet keeps its hex case (RUL-026), `{` is in no query set, and an
  # opaque path leaves `^` literal.
  expect_false(sr_same(c("http://h/p%7b", "http://h/p%7B")))
  expect_false(sr_same(c("http://h/?q{", "http://h/?q%7B")))
  expect_false(sr_same(c("foo:a^", "foo:a%5E")))
  expect_false(sr_same(c("http://h/p/", "http://h/p%2F")))
})

test_that("the shared step leaves rfc3986 and the non-shared fields alone", {
  rec <- .fsss_record_vec(sr_rfc_rows, "rfc3986")
  expect_identical(.fsss_encode_record_vec(rec, "rfc3986"), rec)
  u <- c("http://u\u200b:p w@h/p{?q'#f\u200b", "foo:a ?q", "foo:/p")
  raw <- .fsss_record_vec(u, "whatwg")
  enc <- .fsss_encode_record_vec(raw, "whatwg")
  moved <- names(raw)[!vapply(names(raw), function(f) {
    identical(raw[[f]], enc[[f]])
  }, logical(1))]
  expect_identical(moved, c("path", "query"))
  expect_identical(enc$path, c("/p%7B", "a%20", "/p"))
  expect_identical(enc$query, c("q%27", "q", NA))
  expect_identical(enc$userinfo, raw$userinfo)
  expect_identical(enc$fragment, raw$fragment)
  # Idempotent: the serializer's own encoders change no byte of it.
  expect_identical(.fsss_encode_record_vec(enc, "whatwg"), enc)
})

test_that("serialize_url() and the key both read the shared step", {
  # The serializer's own encoders (R/parse-phases.R) still run over the step's
  # output and change no byte of it, so skipping the step in serialize_url()
  # would not move its output. The wiring is asserted directly instead.
  calls <- new.env()
  calls$seen <- character(0)
  real <- .fsss_encode_record_vec
  local_mocked_bindings(.fsss_encode_record_vec = function(rec, standard) {
    calls$seen <- c(calls$seen, standard)
    real(rec, standard)
  })
  serialize_url("http://h/p{", "whatwg")
  serialize_url("http://h/p", "rfc3986")
  get_url_key("http://h/p{", url_key_policy("whatwg"))
  get_url_key("http://h/p", url_key_policy("rfc3986"))
  expect_identical(calls$seen, c("whatwg", "rfc3986", "whatwg", "rfc3986"))
})

test_that("whatwg: the key's path and query are the serialization's", {
  # Re-keying a URL's WHATWG serialization must reproduce the path and query
  # fields its key framed: they are the shared identity. Other fields may
  # move: authority-delimiter presence (`file:/p` against `file:///p`) and
  # the ftp/ws/wss default ports are key-policy decisions, not serializer
  # ones.
  skip_if_not_installed("jsonlite")
  path <- system.file("bench", "wpt-url-cases.json", package = "rurl")
  skip_if(!nzchar(path) || !file.exists(path), "WPT import not installed")
  wpt <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  u <- c(
    vapply(wpt$success, function(r) r$input, character(1)),
    unlist(sr_path_twins),
    "foo:op aque ?q", "foo:a #f", "http://h/p\u200b?\u200b#\u200b"
  )
  s <- serialize_url(u, "whatwg")
  u <- u[!is.na(s)]
  s <- s[!is.na(s)]
  expect_gt(length(u), 300L)
  fu <- sr_fields(u)
  fs <- sr_fields(s)
  for (f in c("path_kind", "path", "query_kind", "query")) {
    expect_identical(fu[[f]], fs[[f]], label = f)
  }
  # And the whole key agrees wherever no key-policy field differs.
  policy_same <- fu$authority_delimiter_present ==
    fs$authority_delimiter_present &
    ((is.na(fu$port) & is.na(fs$port)) |
       (!is.na(fu$port) & !is.na(fs$port) & fu$port == fs$port))
  expect_gt(sum(policy_same), 300L)
  expect_identical(sr_key(u)[policy_same], sr_key(s)[policy_same])
})
