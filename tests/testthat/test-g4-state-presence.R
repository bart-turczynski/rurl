# G4 state-family coverage: the presence tri-state and the reparse leg.
#
# The canonical state contract requires an absent / present-but-empty / nonempty
# distinction for query and fragment, and a lifetime invariant that state
# survives parse -> ... -> reparse. The audit found the tri-state pinned at the
# CLASSIFIER level (test-parse-state.R:61-73) and at the SERIALIZER level
# (test-parse-serializers.R:73-88), but not at the two levels in between:
#
#   * PARSER POPULATION -- no test asserts a real URL string produces the right
#     query_kind/fragment_kind. The identifiers appear in the suite only as
#     hand-supplied serializer arguments, so the wire between parser and
#     serializer was untested in both directions.
#   * END TO END -- test-general-acceptance.R:70-85 proves the tri-state for
#     HOST (foo:/bar vs foo:///bar stay distinct through clean_url while the
#     compat `host` column is NA for both). Nothing did the analogous thing for
#     query, which is the same property on a different component.
#
# And the reparse leg of the survival invariant had no instance anywhere.
#
# Every assertion pins measured, existing behavior.

# ---- parser population of the presence kinds --------------------------------

test_that("the opaque parser populates query_kind and fragment_kind", {
  # Four shapes covering the full cross-product of present-empty vs absent for
  # the two components: "?#" both empty, neither, query only, fragment only.
  r <- rurl:::.parse_opaque_urls_vec(
    c("foo:///x?#", "foo:///x", "foo:///x?", "foo:///x#"), "whatwg"
  )
  expect_identical(r$query_kind, c("empty", "absent", "empty", "absent"))
  expect_identical(r$fragment_kind, c("empty", "absent", "absent", "empty"))
})

# ---- the tri-state end to end, for query ------------------------------------

test_that("a present-empty query stays distinct from an absent one", {
  d <- safe_parse_urls(
    c("foo:///x?#", "foo:///x", "foo:///x?"),
    scheme_acceptance = "general", url_standard = "whatwg"
  )
  # The canonical record keeps the distinction: a present-but-empty query
  # serializes its "?", an absent one emits nothing.
  expect_identical(d$clean_url, c("foo:///x?", "foo:///x", "foo:///x?"))

  # ...while the public `query` column is a LOSSY compat projection that cannot
  # express it -- NA for all three. Asserting both halves together is the point:
  # it proves the information survives in the record even though the compat
  # column drops it, which is exactly the state contract's claim.
  expect_true(all(is.na(d$query)))
})

# ---- the reparse leg of the survival invariant ------------------------------

test_that("parsing a serialized general-acceptance URL reproduces its state", {
  # The four WHATWG non-special shapes (ADR 0012 D2): opaque path, null-host
  # list path, empty-host list path, IPv6 host.
  shapes <- c("foo:bar", "foo:/bar", "foo:///bar", "foo://[::1]/bar")
  once <- safe_parse_urls(shapes, scheme_acceptance = "general",
                          url_standard = "whatwg")
  twice <- safe_parse_urls(once$clean_url, scheme_acceptance = "general",
                           url_standard = "whatwg")

  for (field in c("scheme", "host", "path", "clean_url", "parse_status")) {
    expect_identical(twice[[field]], once[[field]], info = field)
  }

  # Idempotence is only meaningful if the serialization was non-trivial, so
  # pin that the four shapes really did round-trip to themselves rather than
  # collapsing into one another.
  expect_identical(once$clean_url, shapes)
})
