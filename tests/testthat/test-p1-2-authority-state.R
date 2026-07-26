# P1.2 authority-state vocabulary (RURL-bewtdlua) -- the required-fixture set.
#
# P1.2 (ACCEPTED, bb3346e) retires the single three-valued `authority_kind` for
# `authority_delimiter_present: logical` + `authority_payload_kind:
# {empty, present}`, keeps `host_kind` independent, and binds `//` emission to
# the delimiter fact. Its "Required fixtures (implementation slice)" section
# names seven shapes and four things each must assert: the two canonical
# fields, the independent `host_kind`, the derived legacy projection, and
# serializer `//` emission driven by `authority_delimiter_present`.
#
# This file is that fixture set, driven through the real parsers rather than
# hand-built state, so it also pins the wire between parser and serializer.

# ---- the seven required fixtures, at the parser -----------------------------

test_that("P1.2 fixtures: canonical fields, host_kind, legacy projection", {
  # Columns per P1.2 D-B's table. `foo://@/bar` and `foo://:80/bar` are parsed
  # under rfc3986: WHATWG's host-missing rule rejects both (`ok = FALSE`), and
  # the point of these two rows is the PAYLOAD-present-with-empty-HOST cell,
  # which needs a route that accepts them.
  cases <- list(
    list(url = "foo:/bar", std = "rfc3986",
         delim = FALSE, payload = NA_character_, host = "absent",
         legacy = "absent"),
    list(url = "foo:bar", std = "whatwg",
         delim = FALSE, payload = NA_character_, host = "absent",
         legacy = "absent"),
    list(url = "foo:///bar", std = "whatwg",
         delim = TRUE, payload = "empty", host = "empty", legacy = "empty"),
    list(url = "foo://@/bar", std = "rfc3986",
         delim = TRUE, payload = "present", host = "empty",
         legacy = "present"),
    list(url = "foo://:80/bar", std = "rfc3986",
         delim = TRUE, payload = "present", host = "empty",
         legacy = "present"),
    list(url = "http://example.com/", std = "whatwg",
         delim = TRUE, payload = "present", host = "present",
         legacy = "present")
  )

  for (case in cases) {
    r <- .parse_opaque_urls_vec(case$url, case$std)
    expect_identical(
      r$authority_delimiter_present, case$delim, info = case$url
    )
    expect_identical(r$authority_payload_kind, case$payload, info = case$url)
    expect_identical(r$host_kind, case$host, info = case$url)
    expect_identical(
      .authority_kind(r$authority_delimiter_present, r$authority_payload_kind),
      case$legacy,
      info = case$url
    )
  }

  # The seventh fixture is the RFC 8089 `file:` overlay, a SEPARATE parser --
  # see the agreement test below.
  f <- .parse_rfc_file_urls_vec("file:///bar")
  expect_true(f$authority_delimiter_present)
  expect_identical(f$authority_payload_kind, "empty")
  expect_identical(f$host_kind, "empty")
})

# ---- S1-F5: the two routes agree about the same shape -----------------------

test_that("foo:///bar and file:///bar agree on authority state", {
  # The minimal contradiction S1-F5 recorded: both inputs are `//` followed by
  # an empty authority substring, yet the generic classifier called the first
  # authority-PRESENT while the RFC `file:` overlay called the second
  # authority-EMPTY. P1.2 D-A makes the two axes decide it identically.
  generic <- .parse_opaque_urls_vec("foo:///bar", "rfc3986")
  overlay <- .parse_rfc_file_urls_vec("file:///bar")

  expect_identical(
    generic$authority_delimiter_present, overlay$authority_delimiter_present
  )
  expect_identical(
    generic$authority_payload_kind, overlay$authority_payload_kind
  )
  expect_identical(generic$host_kind, overlay$host_kind)
})

# ---- D-B: the two axes are independent --------------------------------------

test_that("payload state and host state vary independently", {
  # A payload can be present while the host is empty (userinfo-only, port-only)
  # -- so `authority_payload_kind` is not a restatement of `host_kind`, and
  # neither can be derived from the other.
  r <- .parse_opaque_urls_vec(
    c("foo:///bar", "foo://@/bar", "foo://:80/bar", "foo://h/bar"), "rfc3986"
  )
  expect_identical(
    r$authority_payload_kind, c("empty", "present", "present", "present")
  )
  expect_identical(r$host_kind, c("empty", "empty", "empty", "present"))

  # Delimiter present is constant across all four, so it is not doing the work
  # either: three distinct (payload, host) pairs under one delimiter value.
  expect_true(all(r$authority_delimiter_present))
  expect_length(unique(paste(r$authority_payload_kind, r$host_kind)), 3L)
})

# ---- D-C: serializer `//` emission, end to end ------------------------------

test_that("a delimiter-present empty authority survives serialization", {
  # The consequence P1.2 names: `foo:///bar` is no longer indistinguishable
  # from `foo:/bar` on the way back out. Both postures, through the public
  # parse.
  for (std in c("whatwg", "rfc3986")) {
    d <- safe_parse_urls(
      c("foo:/bar", "foo:///bar"),
      scheme_acceptance = "general", url_standard = std
    )
    expect_identical(d$clean_url, c("foo:/bar", "foo:///bar"), info = std)
  }

  # And `file:///bar`, which the overlay used to classify differently, keeps
  # its delimiter too.
  d <- safe_parse_urls(
    "file:///bar", scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(d$clean_url, "file:///bar")
})
