# RURL-xjyrhqin: serialize_url(standard = "rfc3986") and a `//host`
# network-path reference.
#
# serialize_url() documents that both standards REQUIRE a scheme, so
# scheme-less input returns NA rather than being upgraded (roxygen, "Parse
# posture"). A network-path reference (`//h/p`) has no scheme: it is a
# relative reference (RFC 3986 section 4.2), not a URI (section 3, `scheme ":"`
# is mandatory), and with no base there is nothing to resolve it against
# (section 5). The `rfc-syntax` posture the RFC arm parses with sets
# `scheme_relative_handling = "keep"`, which parses such a row behind a
# stand-in `http:` prefix and reports no scheme on the parse surface. The
# serializer record read that stand-in as the row's scheme while its authority
# test, run on the source, found no `://`, so the serializer emitted the
# stand-in scheme and the path with no authority: the host was dropped.

network_path <- c("//h/a/../b", "//h/p")

test_that("controls: absolute URIs serialize under both RFC forms", {
  expect_identical(
    serialize_url(c("http://h/a/../b", "foo://h/p"), standard = "rfc3986"),
    c("http://h/a/../b", "foo://h/p")
  )
  expect_identical(
    serialize_url(c("http://h/a/../b", "foo://h/p"), standard = "rfc3986",
                  form = "normalized"),
    c("http://h/b", "foo://h/p")
  )
})

test_that("controls: other scheme-less input is NA under both RFC forms", {
  x <- c("a/../b", "h/p", "/p")
  for (form in c("source", "normalized")) {
    expect_identical(
      serialize_url(x, standard = "rfc3986", form = form),
      rep(NA_character_, 3L),
      label = form
    )
  }
})

test_that("controls: whatwg gives NA for a network-path reference", {
  expect_identical(
    serialize_url(network_path, standard = "whatwg"),
    c(NA_character_, NA_character_)
  )
})

test_that("controls: the parse surface reads the host, with no scheme", {
  p <- safe_parse_url("//h/p", profile = "rfc-syntax")
  expect_identical(p$scheme, NA_character_)
  expect_identical(p$host, "h")
  expect_identical(p$path, "/p")
})

test_that("a //host input is NA under both RFC forms, as scheme-less input", {
  # It gave the stand-in `http` scheme with no authority: `http:/a/../b` and
  # `http:/p` (source), `http:/b` and `http:/p` (normalized).
  for (form in c("source", "normalized")) {
    expect_identical(
      serialize_url(network_path, standard = "rfc3986", form = form),
      c(NA_character_, NA_character_),
      label = form
    )
  }
})

test_that("no RFC form drops the authority of any //host input", {
  # Every network-path shape: userinfo, port, query and fragment (which gave
  # `http:/p?q#f`), an empty path (`http:`), an empty authority (`///p`, also
  # `http:`), and a path the normalized form rewrites.
  x <- c(network_path, "//u@h:8/p?q#f", "//h", "///p", "//H/%7e/a/./b")
  for (form in c("source", "normalized")) {
    out <- serialize_url(x, standard = "rfc3986", form = form)
    expect_identical(out, rep(NA_character_, length(x)), label = form)
  }
  expect_identical(
    serialize_url(x, standard = "whatwg"), rep(NA_character_, length(x))
  )
})

test_that("an absolute URI in the same call is unaffected", {
  x <- c("//h/p", "http://h/p", NA, "", "foo://u@h:8/a/../b?q#f")
  expect_identical(
    serialize_url(x, standard = "rfc3986"),
    c(NA, "http://h/p", NA, NA, "foo://u@h:8/a/../b?q#f")
  )
  expect_identical(
    serialize_url(x, standard = "rfc3986", form = "normalized"),
    c(NA, "http://h/p", NA, NA, "foo://u@h:8/b?q#f")
  )
})
