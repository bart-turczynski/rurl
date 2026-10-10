# Hex-digit case of a percent-triplet already present in the query and the
# fragment under `url_standard = "whatwg"` (RURL-djvqopjk).
#
# The WHATWG URL Standard's query state and fragment state percent-encode each
# code point with the (special-)query and fragment percent-encode sets. "%" is
# in neither set, so an existing triplet is copied as written:
# `new URL("http://h/p%7f?q%7f#f%7f").href` is `http://h/p%7f?q%7f#f%7f` (Node
# 26.3.1). Every `node` value below was read off Node 26.3.1's `URL`.
#
# This file first pins what must NOT move: the `rfc3986` arm (which already
# keeps the source spelling, RUL-007 and RUL-015), the frozen `NULL` profile
# (ADR 0007, ADR 0016), the WHATWG path, the uppercase spelling of a FRESH
# escape, and the special vs non-special query percent-encode sets.

pqf_case_urls <- c(
  # 1. the ticket's row: a lowercase triplet in path, query and fragment
  "http://h/p%7f?q%7f#f%7f",
  # 2. a raw non-ASCII byte beside an existing lowercase triplet
  "http://h/p?a%c3%bcü#b%c3%bcü",
  # 3. a "%" that starts no triplet, right before a raw non-ASCII byte
  "http://h/p?a%2ü#b%2ü",
  # 4. the special-query set's apostrophe beside a lowercase triplet
  "http://h/p?'%7e'#'%7e'",
  # 5. a space (fresh escape) beside a lowercase triplet
  "http://h/p?a b%7f#c d%7f"
)

# The general-routed non-special twin of rows 1, 2 and 4: `foo:` takes the
# (non-special) query percent-encode set, which leaves "'" literal.
pqf_case_foo <- c(
  "foo://h/p%7f?q%7f'#f%7f",
  "foo://h/p?a%c3ü'#b%c3ü"
)
pqf_case_foo_node <- c(
  "foo://h/p%7f?q%7f'#f%7f",
  "foo://h/p?a%c3%C3%BC'#b%c3%C3%BC"
)

pqf_whatwg_args <- list(
  url_standard = "whatwg", scheme_policy = "require",
  scheme_acceptance = "general"
)

test_that("rfc3986 keeps the query and fragment as written (does not move)", {
  rfc <- list(url_standard = "rfc3986", scheme_policy = "require",
              scheme_acceptance = "general")
  r <- do.call(safe_parse_urls, c(list(pqf_case_urls[c(1L, 2L, 4L)]), rfc))
  expect_identical(r$query, c("q%7f", "a%c3%bcü", "'%7e'"))
  expect_identical(r$fragment, c("f%7f", "b%c3%bcü", "'%7e'"))
  expect_identical(
    serialize_url(pqf_case_urls[c(1L, 2L, 4L)], standard = "rfc3986"),
    c("http://h/p%7f?q%7f#f%7f",
      "http://h/p?a%c3%bcü#b%c3%bcü",
      "http://h/p?'%7e'#'%7e'")
  )
  # The rfc3986 key frames the exact structural query, case included.
  pol <- url_key_policy(standard = "rfc3986")
  k <- get_url_key(c("http://h/?q%7f", "http://h/?q%7F"), pol)
  expect_false(identical(k[[1L]], k[[2L]]))
})

test_that("the frozen NULL profile keeps its normalized spelling", {
  n <- safe_parse_urls(pqf_case_urls[1:4], url_standard = NULL,
                       scheme_policy = "infer", scheme_acceptance = "web")
  # Byte-pinned: every triplet uppercased, a raw >= 0x80 byte encoded, and
  # the swallowed-"%" quirk of the component pass kept (row 3's `%c3`).
  expect_identical(
    n$query, c("q%7F", "a%C3%BC%C3%BC", "a%2%c3%BC", "'%7E'")
  )
  expect_identical(
    n$fragment, c("f%7F", "b%C3%BC%C3%BC", "b%2%c3%BC", "'%7E'")
  )
  expect_identical(n$path[1L], "/p%7F")
  expect_identical(
    get_query(pqf_case_urls[1:2], decode = FALSE),
    c("q%7F", "a%C3%BC%C3%BC")
  )
  expect_identical(rurl:::.web_pqf_source_policy(NULL), "normalize")
})

test_that("whatwg: the path keeps its spelling (does not move)", {
  w <- do.call(safe_parse_urls, c(list(pqf_case_urls[1L]), pqf_whatwg_args))
  expect_identical(w$path, "/p%7f")
  s <- safe_parse_url(pqf_case_urls[1L], url_standard = "whatwg")
  expect_identical(s$path, "/p%7f")
  expect_true(startsWith(
    serialize_url(pqf_case_urls[1L], standard = "whatwg"), "http://h/p%7f?"
  ))
})

test_that("whatwg: a fresh escape is uppercase (does not move)", {
  u <- c("http://h/p?ü#ü", "http://h/p?a b#c d",
         "http://h/p?ü=%C3%BC")
  w <- do.call(safe_parse_urls, c(list(u), pqf_whatwg_args))
  expect_identical(w$query, c("%C3%BC", "a%20b", "%C3%BC=%C3%BC"))
  expect_identical(w$fragment, c("%C3%BC", "c%20d", NA))
  # Node 26.3.1 `href`.
  expect_identical(
    serialize_url(u, standard = "whatwg"),
    c("http://h/p?%C3%BC#%C3%BC", "http://h/p?a%20b#c%20d",
      "http://h/p?%C3%BC=%C3%BC")
  )
  # The key frames the record's query, which spells the octet one way: the
  # raw byte and its uppercase escape still share a whatwg key.
  k <- get_url_key(c("http://h/?ü", "http://h/?%C3%BC"))
  expect_identical(k[[1L]], k[[2L]])
  # So does the cleaning surface's retained query (a lossy projection, ADR
  # 0017, but not one that may start carrying raw bytes).
  keep <- do.call(get_clean_url, c(
    list("http://h/p?ü", query_handling = "keep"), pqf_whatwg_args
  ))
  expect_identical(keep, "http://h/p?%C3%BC=")
})

test_that("whatwg: special vs non-special query encode sets (does not move)", {
  # Special: the special-query set holds "'"; the fragment set does not.
  expect_identical(
    serialize_url(c("http://h/p?'#'", "foo://h/p?'#'"), standard = "whatwg"),
    c("http://h/p?%27#'", "foo://h/p?'#'")
  )
  # The general-routed non-special rows already keep a triplet's case.
  f <- do.call(safe_parse_urls, c(list(pqf_case_foo), pqf_whatwg_args))
  expect_identical(f$query, c("q%7f'", "a%c3%C3%BC'"))
  expect_identical(f$fragment, c("f%7f", "b%c3%C3%BC"))
  expect_identical(
    serialize_url(pqf_case_foo, standard = "whatwg"), pqf_case_foo_node
  )
})
