# RURL-mvjgpthk: resolve_url() and a base carrying C0 controls or spaces.
#
# The URL Standard's basic URL parser strips a leading/trailing C0 control or
# space run from its input and removes every ASCII tab or newline from it
# (step 1). The API URL parser runs the basic URL parser on the base too, so
# under `whatwg` a padded base is the base it pads. RFC 3986 strips nothing
# (a space or control is no URI character, section 2), and the NULL selector
# is frozen (ADR 0007).
#
# This file opens with PINS: the rfc3986 and NULL rows, and the unpadded
# whatwg row, must not move when the whatwg base is stripped.

padded_bases <- c(
  " http://a/b/c ",
  "\thttp://a/b/c\r\n",
  "\x01http://a/b/c\x1f",
  "http:/\n/a/b/c",
  "ht\ttp://a/b/c"
)

test_that("pin: rfc3986 refuses a padded base for a relative reference", {
  for (b in padded_bases) {
    expect_identical(
      resolve_url("g", b, url_standard = "rfc3986", output = "serialized"),
      NA_character_, info = b
    )
    expect_identical(
      resolve_url("g", b, url_standard = "rfc3986"), NA_character_, info = b
    )
    expect_identical(
      resolve_url("g", b, profile = "rfc-syntax"), NA_character_, info = b
    )
  }
})

test_that("pin: the NULL selector leaves a padded base unresolved", {
  for (b in padded_bases) {
    expect_identical(resolve_url("g", b), NA_character_, info = b)
  }
})

test_that("pin: whatwg resolves against an unpadded base", {
  expect_identical(
    resolve_url("g", "http://a/b/c", url_standard = "whatwg",
                output = "serialized"),
    "http://a/b/g"
  )
  expect_identical(
    resolve_url("g", "http://a/b/c", url_standard = "whatwg"), "http://a/b/g"
  )
  expect_identical(
    resolve_url("g", "http://a/b/c", profile = "whatwg"), "http://a/b/g"
  )
})

test_that("whatwg strips a padded base before resolving against it", {
  for (b in padded_bases) {
    expect_identical(
      resolve_url("g", b, url_standard = "whatwg", output = "serialized"),
      "http://a/b/g", info = b
    )
    expect_identical(
      resolve_url("g", b, url_standard = "whatwg"), "http://a/b/g", info = b
    )
    expect_identical(
      resolve_url("g", b, profile = "whatwg"), "http://a/b/g", info = b
    )
  }
})

test_that("whatwg removes an interior tab or newline from the base", {
  # Control, not a pin: the final serialize_url() strips the merged string,
  # so this row gave http://a/b/g before the base strip too.
  expect_identical(
    resolve_url("g", "http://a/b\n/c", url_standard = "whatwg",
                output = "serialized"),
    "http://a/b/g"
  )
  # A tab or newline inside the scheme or the `//` is visible to the split of
  # the base: only a base stripped before the split has a scheme and an
  # authority to merge with.
  expect_identical(
    resolve_url("g", "h\nttp://a/b/c", url_standard = "whatwg",
                output = "serialized"),
    "http://a/b/g"
  )
  expect_identical(
    resolve_url("/g", "http:/\r/a/b/c", url_standard = "whatwg",
                output = "serialized"),
    "http://a/g"
  )
})

test_that("whatwg agrees with its own parser on a padded base", {
  b <- " http://a/b/c "
  expect_identical(
    serialize_url(b, standard = "whatwg"),
    serialize_url("http://a/b/c", standard = "whatwg")
  )
  expect_identical(
    resolve_url(c("g", "../x", "?y", "#z"), b, url_standard = "whatwg",
                output = "serialized"),
    resolve_url(c("g", "../x", "?y", "#z"), "http://a/b/c",
                url_standard = "whatwg", output = "serialized")
  )
})

test_that("an NA or all-padding base stays no base under whatwg", {
  expect_identical(
    resolve_url("g", NA_character_, url_standard = "whatwg"), NA_character_
  )
  expect_identical(
    resolve_url("g", " \t ", url_standard = "whatwg"), NA_character_
  )
})

test_that("an absolute reference ignores a padded whatwg base", {
  expect_identical(
    resolve_url("https://z.com/q", " http://a/ ", url_standard = "whatwg",
                output = "serialized"),
    "https://z.com/q"
  )
})

test_that("a padded undecodable base decodes, then strips, under whatwg", {
  expect_identical(
    resolve_url("g", " http://a/\xff/c ", url_standard = "whatwg",
                output = "serialized"),
    "http://a/%EF%BF%BD/g"
  )
})

# RURL-vuczsypb: the URL Standard's no scheme state fails a reference that
# does not start with "#" against a base with an opaque path. The base strip
# made a padded opaque base reach the merge, so both spellings are pinned.
test_that("an opaque-path base takes only a fragment under whatwg", {
  for (b in c("mailto:x", " mailto:x", "javascript:alert(1)",
              " javascript:alert(1)", " data:,x", "sc:")) {
    for (ref in c("g", "?q", "/p", "../x", "")) {
      expect_identical(
        resolve_url(ref, b, url_standard = "whatwg", output = "serialized"),
        NA_character_, info = paste(ref, b)
      )
      expect_identical(
        resolve_url(ref, b, profile = "whatwg"), NA_character_,
        info = paste(ref, b)
      )
    }
  }
  expect_identical(
    resolve_url("#f", " mailto:x", url_standard = "whatwg",
                output = "serialized"),
    "mailto:x#f"
  )
  expect_identical(
    resolve_url("#f", "mailto:x", url_standard = "whatwg",
                output = "serialized"),
    "mailto:x#f"
  )
  # A non-special base with a path that starts with "/" is no opaque path.
  expect_identical(
    resolve_url("g", "sc:/a/b", url_standard = "whatwg",
                output = "serialized"),
    "sc:/a/g"
  )
})
