# RURL-vewhvxnb: the RFC 3986 component grammar check (`.rfc3986_generic_uri_ok`
# in R/parse-state.R) must answer in linear time and never throw. The anchored
# matcher `\A(?:[class]|%XX)*\z` kept ICU backtrack state per repetition, so a
# component of about 5e5 characters stopped the WHOLE vectorized call with
# "Regular expression backtrack stack overflow" -- every other row lost with it.
#
# The grammar is RFC 3986 section 3 and does not change here: host (S3.2.2),
# userinfo (S3.2.1), path (S3.3), query (S3.4) and fragment (S3.5) are each
# `*( data / pct-encoded )` over their own data class, plus the ADR 0002 / ADR
# 0012 D1 rule 5 tolerance for directly written non-ASCII scalar values.

# --- the component matcher ----------------------------------------------------

# One verdict per element for `s` checked against component `comp`.
.lin_comp_ok <- function(s, comp) {
  bad <- switch(comp,
    host = .RFC3986_HOST_BAD_RE,
    userinfo = .RFC3986_USERINFO_BAD_RE,
    path = .RFC3986_PATH_BAD_RE,
    qf = .RFC3986_QF_BAD_RE
  )
  .rfc3986_class_ok(s, bad)
}

# The component-specific characters each production adds to
# unreserved + sub-delims (RFC 3986 S2.2, S2.3).
.lin_extra <- c(host = "", userinfo = ":", path = ":@/", qf = ":@/?")

# The pre-RURL-vewhvxnb anchored matcher, restricted to ASCII input (its
# non-ASCII range cannot fire there, and PCRE rejects `\x{10FFFF}` in byte
# mode). It is the oracle for the short ASCII cases below.
.lin_oracle_ascii <- function(s, comp) {
  re <- paste0(
    "\\A(?:[A-Za-z0-9._~\\-!$&'()*+,;=", .lin_extra[[comp]],
    "]|%[0-9A-Fa-f]{2})*\\z"
  )
  grepl(re, s, perl = TRUE)
}

# Short ASCII cases: malformed and valid percent tails, every component
# delimiter, controls and line terminators, empty.
.lin_ascii_cases <- c(
  "", "a", "a%2", "%zz", "%", "%4", "%41", "a%20b", "%41%", "%%41", "%4g",
  "a b", "a\n", "\n", "a\r\n", "\t", "\x7f", "a:b", "a@b", "a/b", "a?b",
  "a#b", "[", "]", "|", "\\", "^", "`", "{", "}", "<", ">", "\"",
  "A-Z._~0", "!$&'()*+,;="
)

test_that("the component matcher agrees with the anchored oracle on ASCII", {
  # Expected disagreements between the old anchored matcher and the current
  # one over these ASCII cases: none. The grammar is unchanged.
  for (comp in names(.lin_extra)) {
    expect_identical(
      .lin_comp_ok(.lin_ascii_cases, comp),
      .lin_oracle_ascii(.lin_ascii_cases, comp),
      info = comp
    )
  }
})

test_that("malformed percent tails fail and valid triplets pass", {
  for (comp in names(.lin_extra)) {
    expect_identical(
      .lin_comp_ok(c("a%2", "%zz", "%", "%4", "%41%"), comp),
      rep(FALSE, 5L),
      info = comp
    )
    expect_identical(
      .lin_comp_ok(c("%41", "a%20b", "", "%7e%7E"), comp),
      rep(TRUE, 4L),
      info = comp
    )
  }
})

test_that("each component admits exactly its own delimiters", {
  # rows: host, userinfo, path, query/fragment; columns ":", "@", "/", "?".
  want <- rbind(
    host = c(FALSE, FALSE, FALSE, FALSE),
    userinfo = c(TRUE, FALSE, FALSE, FALSE),
    path = c(TRUE, TRUE, TRUE, FALSE),
    qf = c(TRUE, TRUE, TRUE, TRUE)
  )
  for (comp in rownames(want)) {
    expect_identical(
      .lin_comp_ok(c("a:b", "a@b", "a/b", "a?b"), comp),
      unname(want[comp, ]),
      info = comp
    )
  }
})

test_that("non-ASCII scalars keep the ADR 0002 tolerance at every boundary", {
  # Directly written non-ASCII is admitted wherever a data character is (ADR
  # 0012 D1 rule 5); it never rescues an otherwise-invalid ASCII portion.
  # U+FEFF and U+200B are checked as twins, leading and trailing.
  # Expected disagreements between the old and the current matcher here: none.
  # A leading U+FEFF is a non-ASCII scalar, admitted by both.
  ok <- c(
    "\u0080", "\U0010FFFF", "a\u0080b", "é", "﻿", "﻿a",
    "a﻿", "​", "​a", "a​", "﻿%41"
  )
  bad <- c(
    "﻿%zz", "​%zz", "﻿a b", "​a b", "é%", "a\u0080%4",
    "﻿\n", "a \n"
  )
  for (comp in names(.lin_extra)) {
    expect_identical(.lin_comp_ok(ok, comp), rep(TRUE, length(ok)), info = comp)
    expect_identical(
      .lin_comp_ok(bad, comp), rep(FALSE, length(bad)),
      info = comp
    )
    # The full anchored oracle, non-ASCII range included, run by PCRE in UTF
    # mode (every element here holds a non-ASCII scalar).
    oracle <- paste0(
      "\\A(?:[A-Za-z0-9._~\\-!$&'()*+,;=", .lin_extra[[comp]],
      "\\x{0080}-\\x{10FFFF}]|%[0-9A-Fa-f]{2})*\\z"
    )
    expect_identical(
      .lin_comp_ok(c(ok, bad), comp),
      grepl(oracle, c(ok, bad), perl = TRUE),
      info = comp
    )
  }
  # U+2028 (LS) and U+0085 (NEL) are non-ASCII scalars, so the tolerance admits
  # them as data, at the end of a component too; only the ASCII LF above fails.
  expect_identical(
    .lin_comp_ok(c(" ", "a ", "\u0085", "a\u0085"), "path"),
    rep(TRUE, 4L)
  )
})

test_that("a missing input is rejected", {
  expect_false(.lin_comp_ok(NA_character_, "path"))
})

test_that("a mixed vector gets one verdict per element, NA rejected", {
  # `stri_detect_regex()` answers NA for an NA element, so the matcher must
  # reject NA itself rather than let it through as "no offending character".
  s <- c("a", NA, "%zz", "", "﻿a", "a%2", "%41", NA, "a b")
  want <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  for (comp in names(.lin_extra)) {
    expect_identical(.lin_comp_ok(s, comp), want, info = comp)
  }
})

test_that("the matcher is total on bytes that do not decode", {
  # Never reached through the gate (`.rfc3986_generic_uri_ok()` rejects an
  # undecodable element before walking), but the matcher must not throw. It
  # does not judge decodability itself (stringi reads the stray octet as a
  # non-ASCII character), so the rejection comes from the gate's preflight.
  x <- "a\x80b"
  Encoding(x) <- "UTF-8"
  expect_no_error(suppressWarnings(.lin_comp_ok(x, "path")))
  expect_false(.rfc3986_generic_uri_ok(sprintf("foo:/%s", x))$ok)
})

# --- long components through the parse surface --------------------------------

.lin_n <- 1.5e6
.lin_long <- strrep("a", .lin_n)

# One good and one tail-invalid twin per RFC 3986 component, between two good
# short neighbors.
.lin_long_rows <- function() {
  c(
    neighbor_a = "http://h.example/x",
    userinfo = paste0("http://", .lin_long, "@h.example/p"),
    userinfo_bad = paste0("http://", .lin_long, "%@h.example/p"),
    host = paste0("http://", .lin_long, ".example/p"),
    host_bad = paste0("http://", .lin_long, ".example%/p"),
    path = paste0("http://h.example/", .lin_long),
    path_bad = paste0("http://h.example/", .lin_long, "%"),
    query = paste0("http://h.example/p?", .lin_long),
    query_bad = paste0("http://h.example/p?", .lin_long, "%"),
    fragment = paste0("http://h.example/p#", .lin_long),
    fragment_bad = paste0("http://h.example/p#", .lin_long, "%"),
    file = paste0("file:///", .lin_long),
    file_bad = paste0("file:///", .lin_long, "%"),
    neighbor_z = "http://h.example/z"
  )
}

test_that("a 1.5e6-character component under rfc3986 gets its own verdict", {
  skip_on_cran() # about 5 s and tens of MB; the defect is not R-version bound
  rows <- .lin_long_rows()
  expect_no_error(res <- safe_parse_urls(rows, url_standard = "rfc3986"))
  expect_identical(nrow(res), length(rows))
  status <- stats::setNames(res$parse_status, names(rows))
  bad <- grepl("_bad$", names(rows))
  # Each good row parses, each tail-invalid twin fails alone, and the short
  # neighbors are untouched.
  expect_true(all(status[!bad] != "error"), info = toString(status[!bad]))
  expect_true(all(status[bad] == "error"), info = toString(status[bad]))
})

test_that("a 1.5e6-character file: path gets its own verdict by default", {
  skip_on_cran() # about 5 s and tens of MB; the defect is not R-version bound
  # The default is whatwg (ADR 0020). Its file state machine reads a `%` that
  # begins no triplet as path data (a validation error, not a failure), so
  # `file_bad` parses too; the RFC 8089 overlay is the rfc3986 arm's alone.
  rows <- .lin_long_rows()[c("neighbor_a", "file", "file_bad", "neighbor_z")]
  expect_no_error(res <- safe_parse_urls(rows, url_standard = NULL))
  expect_identical(
    res$parse_status,
    c("warning-invalid-tld", "ok", "ok", "warning-invalid-tld")
  )
})

test_that("a 1.5e6-character component under whatwg returns a row each", {
  skip_on_cran() # about 5 s and tens of MB; the defect is not R-version bound
  # The RFC 3986 grammar does not bind under whatwg; this only proves no
  # throw. The cheap rows suffice (the long userinfo, host, query and fragment
  # rows cost seconds each in the WHATWG parser, outside the grammar check).
  rows <- .lin_long_rows()[c("neighbor_a", "path", "file", "neighbor_z")]
  expect_no_error(res <- safe_parse_urls(rows, url_standard = "whatwg"))
  expect_identical(nrow(res), length(rows))
})
