# IDN policy symmetry (RURL-yeumdmmf).
#
# An A-label host (e.g. "xn--mnich-kva.com") and its Unicode equivalent
# ("münich.com") are the SAME logical host. The STRUCTURAL normalization
# decisions -- does the host have a subdomain? is it an apex eTLD+1 eligible for
# a leading "www."? how many subdomain labels exist? -- must therefore be
# IDENTICAL for both spellings, independent of how the URL was written. Only the
# requested output encoding (keep / idna / unicode) should differ.
#
# Before the fix these decisions were made by string-comparing the RAW host
# (A-label or Unicode, per input) against pslr's Unicode registered domain, so
# the branch taken depended on the input encoding. These tests pin the
# symmetry across the full option matrix.

# The same logical host in both spellings.
apex_unicode <- "münich.com"
apex_alabel <- "xn--mnich-kva.com"
deep_unicode <- "a.b.münich.com"
deep_alabel <- "a.b.xn--mnich-kva.com"

# Decode any host spelling to a single common (Unicode) encoding so that a
# "keep" A-label output and a "keep" Unicode output compare equal once the
# A-label-vs-Unicode spelling difference is removed. We re-parse the bare host
# under host_encoding = "unicode" with all structural policies neutral so this
# helper only ever changes ENCODING, never structure.
to_common <- function(host) {
  if (is.na(host) || !nzchar(host)) {
    return(host)
  }
  safe_parse_url(
    paste0("http://", host),
    www_handling = "none",
    subdomain_levels_to_keep = NULL,
    host_encoding = "unicode",
    case_handling = "keep"
  )$host
}

parse_host <- function(host, ...) {
  safe_parse_url(paste0("http://", host), ...)$host
}

# ---------------------------------------------------------------------------
# The two exact reproductions from the bug report.
# ---------------------------------------------------------------------------

test_that("www if_no_subdomain is symmetric across IDN spellings (repro 1)", {
  uni <- parse_host(
    apex_unicode,
    www_handling = "if_no_subdomain", host_encoding = "unicode"
  )
  ascii <- parse_host(
    apex_alabel,
    www_handling = "if_no_subdomain", host_encoding = "unicode"
  )
  expect_equal(uni, "www.münich.com")
  expect_equal(ascii, "www.münich.com")
})

test_that("subdomain trim is symmetric across IDN spellings (repro 2)", {
  uni <- parse_host(
    deep_unicode,
    subdomain_levels_to_keep = 1, host_encoding = "unicode"
  )
  ascii <- parse_host(
    deep_alabel,
    subdomain_levels_to_keep = 1, host_encoding = "unicode"
  )
  expect_equal(uni, "b.münich.com")
  expect_equal(ascii, "b.münich.com")
})

# ---------------------------------------------------------------------------
# Full matrix: host_encoding x www_handling x subdomain_levels_to_keep, each
# evaluated for both spellings of the same logical host. The two spellings must
# decode to the SAME common-encoding host.
# ---------------------------------------------------------------------------

host_encodings <- c("keep", "idna", "unicode")
www_handlings <- c("strip", "keep", "if_no_subdomain")
sub_levels <- list(NULL, 0, 1)

run_matrix <- function(host_uni, host_ascii) {
  for (enc in host_encodings) {
    for (www in www_handlings) {
      for (lvl in sub_levels) {
        out_uni <- parse_host(
          host_uni,
          host_encoding = enc, www_handling = www,
          subdomain_levels_to_keep = lvl
        )
        out_ascii <- parse_host(
          host_ascii,
          host_encoding = enc, www_handling = www,
          subdomain_levels_to_keep = lvl
        )
        label <- sprintf(
          "enc=%s www=%s sub=%s", enc, www,
          if (is.null(lvl)) "NULL" else lvl
        )
        # Same logical structure: equal after decoding both to a common
        # encoding (collapses the keep A-label vs keep Unicode spelling gap).
        expect_equal(to_common(out_uni), to_common(out_ascii), info = label)
      }
    }
  }
}

test_that("apex IDN host: A-label and Unicode are structurally identical", {
  run_matrix(apex_unicode, apex_alabel)
})

test_that("multi-subdomain IDN: A-label and Unicode are structural twins", {
  run_matrix(deep_unicode, deep_alabel)
})

# ---------------------------------------------------------------------------
# host_encoding = "keep" must preserve the INPUT spelling in the OUTPUT while
# still applying the structural trim. (Constraint #2.)
# ---------------------------------------------------------------------------

test_that("keep encoding preserves A-label spelling under subdomain trim", {
  expect_equal(
    parse_host(
      deep_alabel,
      subdomain_levels_to_keep = 1, host_encoding = "keep"
    ),
    "b.xn--mnich-kva.com"
  )
})

test_that("keep encoding preserves Unicode spelling under subdomain trim", {
  expect_equal(
    parse_host(
      deep_unicode,
      subdomain_levels_to_keep = 1, host_encoding = "keep"
    ),
    "b.münich.com"
  )
})

test_that("keep encoding preserves spelling under www if_no_subdomain", {
  expect_equal(
    parse_host(
      apex_alabel,
      www_handling = "if_no_subdomain", host_encoding = "keep"
    ),
    "www.xn--mnich-kva.com"
  )
  expect_equal(
    parse_host(
      apex_unicode,
      www_handling = "if_no_subdomain", host_encoding = "keep"
    ),
    "www.münich.com"
  )
})

test_that("idna encoding yields A-label for both input spellings", {
  expect_equal(
    parse_host(
      deep_alabel,
      subdomain_levels_to_keep = 1, host_encoding = "idna"
    ),
    "b.xn--mnich-kva.com"
  )
  expect_equal(
    parse_host(
      deep_unicode,
      subdomain_levels_to_keep = 1, host_encoding = "idna"
    ),
    "b.xn--mnich-kva.com"
  )
})

# ---------------------------------------------------------------------------
# Non-IDN hosts must be COMPLETELY unaffected. (Constraint #3.)
# ---------------------------------------------------------------------------

test_that("non-IDN www if_no_subdomain unchanged", {
  expect_equal(
    parse_host("example.com", www_handling = "if_no_subdomain"),
    "www.example.com"
  )
  expect_equal(
    parse_host("sub.example.com", www_handling = "if_no_subdomain"),
    "sub.example.com"
  )
})

test_that("non-IDN subdomain trim unchanged", {
  expect_equal(
    parse_host("a.b.example.com", subdomain_levels_to_keep = 1),
    "b.example.com"
  )
  expect_equal(
    parse_host("a.b.example.com", subdomain_levels_to_keep = 0),
    "example.com"
  )
  expect_equal(
    parse_host("a.b.c.d.example.co.uk", subdomain_levels_to_keep = 2),
    "c.d.example.co.uk"
  )
})

test_that("non-IDN mixed-case keep lowercases domain, keeps subdomain case", {
  expect_equal(
    parse_host(
      "A.B.Example.COM",
      subdomain_levels_to_keep = 1, host_encoding = "keep",
      case_handling = "keep"
    ),
    "B.example.com"
  )
})
