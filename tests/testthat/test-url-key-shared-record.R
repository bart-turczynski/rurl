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
