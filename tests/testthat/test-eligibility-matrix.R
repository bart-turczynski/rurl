# Layer 3c Stage-B eligibility matrix (ADR 0012 D2, RURL-jqlnnaiw).
#
# `.stage_b_eligibility` is a PURE classifier: it maps hand-built per-row state
# to three transform-eligibility masks. These tests exercise it at the STATE
# level (feeding hand-decomposed pieces), NOT end-to-end: `general` acceptance
# is unexposed and opaque non-special inputs still error pre-Stage-B until L4b,
# so end-to-end `general` coverage is L4b's slice, not this one. The critical
# property proven here is BYTE-IDENTITY BY CONSTRUCTION: under any non-"general"
# acceptance (notably "web", the default and only publicly reachable value)
# every mask is TRUE, so no Stage-B revert ever fires.

# --- grandfather clause: non-"general" => all masks TRUE ---------------------

test_that("web (and any non-general) acceptance leaves every mask TRUE", {
  # Every scheme, path kind, and host kind must be all-TRUE under "web" -- the
  # previously supported web schemes (incl. ftp/file) are grandfathered.
  schemes <- c("http", "https", "ftp", "ftps", "file", "mailto", "foo", NA)
  path_kinds <- c("list", "opaque")
  host_kinds <- c("present", "empty", "absent")
  for (acc in c("web", "rfc3986-ish-nonsense", "anything-but-general")) {
    for (sch in schemes) {
      for (pk in path_kinds) {
        for (hk in host_kinds) {
          for (ip in c(TRUE, FALSE, NA)) {
            elig <- rurl:::.stage_b_eligibility(
              acc, sch, "whatwg",
              path_kind = pk, host_kind = hk, is_ip_host = ip
            )
            expect_true(elig$path_eligible)
            expect_true(elig$host_transform_eligible)
            expect_true(elig$semantic_transform_eligible)
          }
        }
      }
    }
  }
})

test_that("web masks are all-TRUE and vectorized to input length", {
  sch <- c("http", "ftp", "mailto", "foo")
  elig <- rurl:::.stage_b_eligibility(
    "web", sch, NULL,
    path_kind = c("list", "list", "opaque", "opaque"),
    host_kind = c("present", "present", "absent", "empty"),
    is_ip_host = c(FALSE, FALSE, FALSE, TRUE)
  )
  expect_identical(elig$path_eligible, rep(TRUE, 4))
  expect_identical(elig$host_transform_eligible, rep(TRUE, 4))
  expect_identical(elig$semantic_transform_eligible, rep(TRUE, 4))
})

# --- general acceptance: D2 restriction per component ------------------------

test_that("general path_eligible: FALSE for opaque path, TRUE for list", {
  elig <- rurl:::.stage_b_eligibility(
    "general", c("foo", "foo", "http"), "whatwg",
    path_kind = c("opaque", "list", "list"),
    host_kind = c("absent", "present", "present"),
    is_ip_host = c(FALSE, FALSE, FALSE)
  )
  expect_identical(elig$path_eligible, c(FALSE, TRUE, TRUE))
})

test_that("general semantic_transform_eligible: HTTP(S) only", {
  # http/https eligible; ftp/file (special but not HTTP(S)) and non-special
  # schemes ineligible -- the transform-skipped-ineligible-scheme case.
  elig <- rurl:::.stage_b_eligibility(
    "general",
    c("http", "https", "HTTP", "ftp", "ftps", "file", "mailto", "foo", NA),
    "whatwg",
    path_kind = "list", host_kind = "present", is_ip_host = FALSE
  )
  expect_identical(
    elig$semantic_transform_eligible,
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("general host_transform_eligible: WHATWG domain host only", {
  # A domain host arises only under a special scheme with a present, non-IP
  # host. Opaque hosts (non-special scheme), IP literals, and empty/absent
  # hosts are ineligible.
  elig <- rurl:::.stage_b_eligibility(
    "general",
    scheme = c("http", "https", "ftp", "file", "http", "http", "http", "foo"),
    "whatwg",
    path_kind = "list",
    host_kind = c(
      "present", "present", "present", "present",
      "empty", "absent", "present", "present"
    ),
    is_ip_host = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
  expect_identical(
    elig$host_transform_eligible,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("general: HTTP(S) + domain host => all masks TRUE", {
  elig <- rurl:::.stage_b_eligibility(
    "general", c("http", "https"), "whatwg",
    path_kind = "list", host_kind = "present", is_ip_host = FALSE
  )
  expect_identical(elig$path_eligible, c(TRUE, TRUE))
  expect_identical(elig$host_transform_eligible, c(TRUE, TRUE))
  expect_identical(elig$semantic_transform_eligible, c(TRUE, TRUE))
})

test_that("general host mask tolerates NA is_ip_host (treated as non-IP)", {
  elig <- rurl:::.stage_b_eligibility(
    "general", "http", "whatwg",
    path_kind = "list", host_kind = "present", is_ip_host = NA
  )
  expect_true(elig$host_transform_eligible)
})

# --- get_scheme -> get_scheme_class cascade (D2) -----------------------------

test_that("get_scheme_class classifies special/non-special/missing", {
  # DELIVERABLE 3 cascade: get_scheme_class consumes get_scheme's literal
  # scheme. For today's reachable web rows get_scheme already returns the
  # lowercased literal scheme, so no get_scheme change was required; this
  # asserts the classifier works off that output for special, and for a
  # missing/errored row.
  expect_identical(
    get_scheme_class("http://example.com/", url_standard = "whatwg"),
    "special"
  )
  expect_identical(
    get_scheme_class("ftps://example.com/", url_standard = "whatwg"),
    "non-special"
  )
  # An unsupported scheme errors under the default web acceptance => get_scheme
  # NA => missing-or-error. (Once L4b exposes general and admits the literal
  # scheme, the same classifier returns non-special for free.)
  expect_identical(
    get_scheme_class("mailto:a@b.com", url_standard = "whatwg"),
    "missing-or-error"
  )
})
