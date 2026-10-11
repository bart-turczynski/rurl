# The `whatwg` URL key frames the query as the WHATWG URL Standard serializes
# it (RURL-pjrqnqvb).
#
# The query state percent-encodes each code point with the query percent-encode
# set (a non-special scheme) or the special-query percent-encode set (a special
# scheme), so the URL record holds the query already encoded, and WHATWG URL
# equality is equality of serializations. The key's contract frames "the
# selected standard's normalized identity, not source-byte equality"
# (contracts/key-join-contracts.md, Key-policy rows), so two URLs the standard
# serializes alike must key alike.
#
# The serializer-input record (`.fsss_record_vec()`) is lossless: it keeps a
# general-routed query as written and leaves the encoding to the serializer.
# The key framed that record field as it stood, so `foo:/p?<U+200B>q` and
# `foo:/p?%E2%80%8Bq` keyed apart although `serialize_url()` gives
# `foo:/p?%E2%80%8Bq` for both. A special query kept its apostrophe and double
# quote as written, so `http://h/p?'q` and `http://h/p?%27q` keyed apart too.
#
# Pins first: the `rfc3986` key, the special query's non-ASCII bytes, the
# non-special apostrophe, the triplet case, query presence, the fragment and
# the path field must not move.

zwsp <- "\u200b"

wq_key <- function(u, std = "whatwg") {
  as.character(get_url_key(u, url_key_policy(std)))
}

wq_fields <- function(u, std = "whatwg") {
  .url_key_state_vec(u, url_key_policy(std))$fields
}

wq_same <- function(u, std = "whatwg") {
  k <- wq_key(u, std)
  identical(k[[1L]], k[[2L]])
}

# How a present field is framed: its UTF-8 byte count, `:`, its bytes.
wq_framed <- function(x) {
  paste0(nchar(x, type = "bytes"), ":", x)
}

wq_has_frame <- function(k, x) {
  grepl(wq_framed(x), k, fixed = TRUE, useBytes = TRUE)
}

# Twins: the raw spelling and its WHATWG serialization. `serialize_url()` gives
# the second for both.
wq_twins <- list(
  list("foo:/p?\u200bq", "foo:/p?%E2%80%8Bq"),
  list("foo:p?\u200bq", "foo:p?%E2%80%8Bq"),
  list("foo://h/p?\u200bq", "foo://h/p?%E2%80%8Bq"),
  list("foo:/p? q", "foo:/p?%20q"),
  list("foo:/p?<q\"", "foo:/p?%3Cq%22"),
  list("http://h/p?'q", "http://h/p?%27q"),
  list("http://h/p?\"q", "http://h/p?%22q"),
  list("file:///p?'q", "file:///p?%27q")
)

# --- Pins: behavior the fix must not move -------------------------------------

test_that("the WHATWG serialization of each twin pair is one string", {
  for (tw in wq_twins) {
    s <- serialize_url(c(tw[[1L]], tw[[2L]]), standard = "whatwg")
    expect_identical(s, c(tw[[2L]], tw[[2L]]), label = tw[[2L]])
  }
})

test_that("rfc3986: the key frames the query as written", {
  # RFC 3986 section 6.2.2.2 normalization is a separate question; the
  # rfc3986 key keeps the source spelling and these twins key apart.
  k <- wq_key(c("foo:/p?\u200bq", "foo:/p?%E2%80%8Bq", "http://h/p?'q",
                "http://h/p?%27q"), "rfc3986")
  expect_true(wq_has_frame(k[[1L]], paste0(zwsp, "q")))
  expect_true(wq_has_frame(k[[2L]], "%E2%80%8Bq"))
  expect_true(wq_has_frame(k[[3L]], "'q"))
  expect_true(wq_has_frame(k[[4L]], "%27q"))
  expect_false(k[[1L]] == k[[2L]])
  expect_false(k[[3L]] == k[[4L]])
  expect_identical(wq_fields("foo:/p?\u200bq", "rfc3986")$query,
                   paste0(zwsp, "q"))
})

test_that("whatwg: a special query's non-ASCII bytes already key alike", {
  expect_true(wq_same(c("http://h/p?\u200bq", "http://h/p?%E2%80%8Bq")))
  expect_identical(wq_fields("http://h/p?\u200bq")$query, "%E2%80%8Bq")
})

test_that("whatwg: twins the standard serializes apart stay apart", {
  # The query set leaves `'` literal for a non-special scheme, and an existing
  # triplet keeps its hex case (RUL-026).
  expect_false(wq_same(c("foo:/p?'q", "foo:/p?%27q")))
  expect_false(wq_same(c("foo:/p?q%7f", "foo:/p?q%7F")))
  expect_false(wq_same(c("http://h/?q%7f", "http://h/?q%7F")))
  expect_false(wq_same(c("foo:/p?a%zz", "foo:/p?a%ZZ")))
  expect_identical(wq_fields("foo:/p?'q")$query, "'q")
})

test_that("whatwg: query presence stays three-valued", {
  k <- wq_key(c("foo:/p", "foo:/p?", "foo:/p?q"))
  expect_length(unique(k), 3L)
  f <- wq_fields(c("foo:/p", "foo:/p?", "foo:/p?q"))
  expect_identical(f$query_kind, c("absent", "empty", "present"))
  expect_identical(f$query, c(NA_character_, NA_character_, "q"))
})

test_that("whatwg: the fragment stays out of the key", {
  expect_true(wq_same(c("foo:/p?q#\u200b", "foo:/p?q")))
  expect_true(wq_same(c("foo:/p?q#a", "foo:/p?q#b")))
})

test_that("whatwg: the path and host fields are not touched", {
  f <- wq_fields(c("foo:/p?\u200bq", "foo:p?\u200bq", "foo://h/p?\u200bq"))
  expect_identical(f$path, c("/p", "p", "/p"))
  expect_identical(f$host, c(NA_character_, NA_character_, "h"))
  expect_identical(f$scheme, c("foo", "foo", "foo"))
})

test_that("the key version is not bumped again", {
  expect_identical(url_key_policy()$key_version, 2L)
  expect_identical(url_key_policy("rfc3986")$key_version, 2L)
  expect_identical(url_key_policy()$schema_version, 1L)
})

# --- Defect: the whatwg key frames the query as written -----------------------

test_that("DEFECT: whatwg keys a query twin pair apart", {
  # The twins serialize alike (pinned above) but frame their raw spelling.
  for (tw in wq_twins) {
    expect_false(wq_same(c(tw[[1L]], tw[[2L]])), label = tw[[1L]])
  }
  k <- wq_key(c("foo:/p?\u200bq", "foo:/p?%E2%80%8Bq"))
  expect_true(wq_has_frame(k[[1L]], paste0(zwsp, "q")))
  expect_true(wq_has_frame(k[[2L]], "%E2%80%8Bq"))
  expect_identical(
    duplicated(get_url_key(c("foo:/p?\u200bq", "foo:/p?%E2%80%8Bq"),
                           url_key_policy("whatwg"))),
    c(FALSE, FALSE)
  )
  # An inferred scheme reads as `http`, whose parse record encodes `'`.
  expect_false(wq_same(c("h.com/p?'q", "h.com/p?%27q")))
})
