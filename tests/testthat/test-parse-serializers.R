# Layer 3b posture-serializer unit tests (ADR 0012 D2 / Appendix A.1,
# RURL-mgmviuta).
#
# These exercise .serialize_whatwg_vec() and .serialize_rfc_generic_vec()
# purely at the STATE level: `general` acceptance is unexposed and opaque
# inputs still error before serialization, so there is no public parse to feed
# from. Each test hand-builds the already-parsed state args (host_kind via
# .host_kind, path_kind via .whatwg_path_kind) and asserts the exact clean_url
# string (FRAGMENT excluded; empty-but-present query -> trailing "?").

test_that("WHATWG serializes the four non-special shapes", {
  # Helper: build the state and serialize with a fragment-free, port-free,
  # keep-trailing-slash configuration.
  ser <- function(host, path, remainder) {
    .serialize_whatwg_vec(
      scheme = "foo",
      host = host,
      host_kind = .host_kind(host),
      path = path,
      path_kind = .whatwg_path_kind(FALSE, remainder),
      query = NA_character_,
      query_kind = "absent",
      port = NULL,
      port_handling = "exclude",
      trailing_slash_handling = "keep"
    )
  }

  # foo:bar   -> opaque path, host absent (remainder "bar" has no leading "/").
  expect_identical(ser(NA_character_, "bar", "bar"), "foo:bar")
  # foo:/bar  -> list path, host absent, no authority (no `//`).
  expect_identical(ser(NA_character_, "/bar", "/bar"), "foo:/bar")
  # foo:///bar -> list path, host EMPTY -> `//` + "" + "/bar".
  expect_identical(ser("", "/bar", "///bar"), "foo:///bar")
  # foo://[::1]/bar -> list path, host PRESENT.
  expect_identical(ser("[::1]", "/bar", "//[::1]/bar"), "foo://[::1]/bar")
})

test_that("WHATWG `/.` guard fires only for a null-host empty-lead list path", {
  ser <- function(path) {
    .serialize_whatwg_vec(
      scheme = "foo", host = NA_character_, host_kind = "absent",
      path = path, path_kind = "list", query = NA_character_,
      query_kind = "absent", port = NULL, port_handling = "exclude",
      trailing_slash_handling = "keep"
    )
  }

  # Triggering: path "//bar" -> WHATWG list c("", "bar") (size 2, first empty).
  # The guard prepends "/." so the serialization is not read as an authority.
  guarded <- ser("//bar")
  expect_true(grepl("/.", guarded, fixed = TRUE))
  expect_identical(guarded, "foo:/.//bar")

  # Non-triggering: a normal single-segment path.
  expect_identical(ser("/bar"), "foo:/bar")
  # Non-triggering: a lone "/" -> WHATWG list character(0) (a single empty
  # segment view / size 0), no guard.
  expect_identical(ser("/"), "foo:/")
})

test_that("WHATWG guard does NOT fire when the host is empty-but-present", {
  # host_kind "empty" is NOT null: authority emits `//`, so no guard.
  expect_identical(
    .serialize_whatwg_vec(
      "foo", "", "empty", "//bar", "list", NA_character_, "absent",
      NULL, "exclude", "keep"
    ),
    "foo:////bar"
  )
})

test_that("WHATWG query rule: empty-but-present -> `?`; never a fragment", {
  ser <- function(query, query_kind) {
    .serialize_whatwg_vec(
      "foo", NA_character_, "absent", "/p", "list", query, query_kind,
      NULL, "exclude", "keep"
    )
  }

  expect_identical(ser("", "present"), "foo:/p?")   # present with "" -> "?"
  expect_identical(ser(NA_character_, "empty"), "foo:/p?") # empty -> "?"
  expect_identical(ser(NA_character_, "absent"), "foo:/p") # absent -> none
  expect_identical(ser("a=1", "present"), "foo:/p?a=1")    # real query

  # A "#frag" that might live elsewhere is NEVER appended by the serializer.
  expect_false(grepl("#", ser("a=1", "present"), fixed = TRUE))
})

test_that("RFC generic serializes authority / rootless / absolute forms", {
  # Authority form: foo://h/p.
  expect_identical(
    .serialize_rfc_generic_vec(
      "foo", "h", "present", "/p", "abempty", NA_character_, "absent",
      NULL, "exclude"
    ),
    "foo://h/p"
  )
  # Rootless form: foo:p (no authority, no leading `/`).
  expect_identical(
    .serialize_rfc_generic_vec(
      "foo", NA_character_, "absent", "p", "rootless", NA_character_,
      "absent", NULL, "exclude"
    ),
    "foo:p"
  )
  # Absolute form: foo:/p (no authority, leading `/`).
  expect_identical(
    .serialize_rfc_generic_vec(
      "foo", NA_character_, "absent", "/p", "absolute", NA_character_,
      "absent", NULL, "exclude"
    ),
    "foo:/p"
  )
})

test_that("RFC generic preserves dot segments and applies no `/.` guard", {
  # Dot / dot-dot segments and a would-be-guard leading `//` are all preserved
  # verbatim: rfc-syntax does NOT remove dot segments or add the `/.` guard.
  expect_identical(
    .serialize_rfc_generic_vec(
      "foo", "h", "present", "/a/./b/../c", "abempty", NA_character_,
      "absent", NULL, "exclude"
    ),
    "foo://h/a/./b/../c"
  )
  expect_identical(
    .serialize_rfc_generic_vec(
      "foo", NA_character_, "absent", "//bar", "absolute", NA_character_,
      "absent", NULL, "exclude"
    ),
    "foo://bar"
  )
})

test_that("RFC generic query rule mirrors WHATWG presence handling", {
  ser <- function(query, query_kind) {
    .serialize_rfc_generic_vec(
      "foo", "h", "present", "/p", "abempty", query, query_kind,
      NULL, "exclude"
    )
  }
  expect_identical(ser("a=1", "present"), "foo://h/p?a=1")
  expect_identical(ser("", "present"), "foo://h/p?")
  expect_identical(ser(NA_character_, "empty"), "foo://h/p?")
  expect_identical(ser(NA_character_, "absent"), "foo://h/p")
})

test_that("WHATWG special-scheme host-present list path matches hierarchical", {
  # L4b relies on parity with .build_clean_url_vec for the special/host-present
  # case: a normal http list path serializes identically.
  expect_identical(
    .serialize_whatwg_vec(
      "http", "example.com", "present", "/a/b", "list", NA_character_,
      "absent", NULL, "exclude", "keep"
    ),
    "http://example.com/a/b"
  )
})
