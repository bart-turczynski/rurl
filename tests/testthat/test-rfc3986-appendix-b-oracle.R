# The Appendix B oracle judges rurl's decomposition, so it is pinned here before
# anything is allowed to cite it. Same discipline as
# "the RFC 3986 grammar transcription is itself sound" in
# test-external-url-vectors.R: an oracle nobody checks is a second opinion with
# no standing.

test_that("Appendix B separates authority ABSENT from authority EMPTY", {
  # THE regression test for this file. `regexec(perl = TRUE)` reports a
  # non-participating group as start 0, not -1, so a participation test written
  # as `start < 0` collapses absent into empty -- and the oracle then reports
  # `http:/evil.com` as "authority written, and empty", which is precisely the
  # incoherent record it exists to expose. This assertion fails on that bug.
  absent <- split_rfc3986_appendix_b(c("http:/x", "http:", "mailto:a@b.c",
                                       "urn:isbn:0451450523"))
  expect_false(any(absent$authority_present))
  expect_true(all(is.na(absent$authority)))

  # Written and empty: the `//` is there, the capture is zero-length.
  empty <- split_rfc3986_appendix_b(c("http://", "http:///", "http:///x",
                                      "http:////x", "foo:///x"))
  expect_true(all(empty$authority_present))
  expect_identical(empty$authority, rep("", 5L))

  # Written and non-empty, so the three states are all distinguished, not two.
  present <- split_rfc3986_appendix_b(c("http://x", "http://h.com:8080/p"))
  expect_true(all(present$authority_present))
  expect_identical(present$authority, c("x", "h.com:8080"))

  # The same tri-state for query and fragment, where `?`/`#` with nothing after
  # them is written-but-empty and their absence is NA.
  qf <- split_rfc3986_appendix_b(c("http://x", "http://x?", "http://x#",
                                   "http://x?q#f"))
  expect_identical(qf$query, c(NA, "", NA, "q"))
  expect_identical(qf$fragment, c(NA, NA, "", "f"))
})

test_that("Appendix B reproduces the standard's own component table", {
  # RFC 3986 Appendix B works its expression over this exact example and lists
  # the results; reproducing the RFC's own table is the closest thing to a
  # conformance test the oracle can have.
  p <- split_rfc3986_appendix_b("http://www.ics.uci.edu/pub/ietf/uri/#Related")
  expect_identical(p$scheme, "http")
  expect_identical(p$authority, "www.ics.uci.edu")
  expect_identical(p$path, "/pub/ietf/uri/")
  expect_identical(p$query, NA_character_) # the RFC notes query is undefined
  expect_identical(p$fragment, "Related")
})

test_that("Appendix B agrees with five independent implementations", {
  # The authority-slash family, measured against Go net/url, Python urlsplit,
  # Ruby URI, libxml2 and PHP parse_url -- all five agree with these rows
  # (RURL-kmkyicpt triage). Transcribed as the oracle's acceptance criteria, so
  # a later "simplification" of the splitter has to keep agreeing with them.
  fam <- c("http:/evil.com", "http://evil.com", "http:///evil.com",
           "http:///a/b", "http:////evil.com")
  p <- split_rfc3986_appendix_b(fam)
  expect_identical(p$authority, c(NA, "evil.com", "", "", ""))
  expect_identical(p$path, c("/evil.com", "", "/evil.com", "/a/b",
                             "//evil.com"))
})

test_that("section 3.2 splits the authority the grammar's way", {
  a <- split_rfc3986_authority(c(
    "u:p@h.com:8080", # userinfo may hold ":" (S3.2.1)
    "h.com",          # no userinfo, no port
    "@h.com",         # empty userinfo, written
    "h.com:",         # port = *DIGIT, so empty port is legal (S3.2.3)
    ":8080",          # reg-name may be empty (S3.2.2)
    "",               # wholly empty authority
    "[::1]:80",       # IP-literal: the port colon is the one AFTER "]"
    "[::1]",          # colons inside brackets are not port separators
    "[v7.aBc]:9"      # IPvFuture
  ))
  expect_identical(a$userinfo, c("u:p", NA, "", NA, NA, NA, NA, NA, NA))
  expect_identical(a$host, c("h.com", "h.com", "h.com", "h.com", "", "",
                             "[::1]", "[::1]", "[v7.aBc]"))
  expect_identical(a$port, c("8080", NA, NA, "", "8080", NA, "80", NA, "9"))

  # A VALID authority holds at most one "@" -- userinfo's production excludes it
  # -- so every row above splits identically under a first-"@" or a last-"@"
  # rule, and neither is pinned by them. Multi-"@" input is invalid, but the
  # helper still documents first-"@" as its convention, and an undefended
  # comment drifts from its code. This is the row that defends it, and it also
  # states plainly that rurl's own last-"@" WHATWG rule is a DIFFERENT
  # standard's answer that must never leak into the RFC oracle.
  expect_false(rfc3986_abnf_accepts("a@b@h.com"))
  multi <- split_rfc3986_authority("a@b@h.com")
  expect_identical(multi$userinfo, "a")
  expect_identical(multi$host, "b@h.com")
})

test_that("Appendix B + section 5.3 round-trip every string byte-for-byte", {
  # The oracle's total self-check. Appendix B is a splitter, not a validator: it
  # matches EVERY string. So recomposition must return every string unchanged --
  # valid URIs, invalid ones, relative references, and pure garbage alike. Any
  # byte lost, gained or reordered in the split shows up here.
  corpus <- c(
    "http://www.ics.uci.edu/pub/ietf/uri/#Related",
    "http:/x", "http://x", "http:///x", "http:////x", "http:", "http://",
    "urn:isbn:0451450523", "mailto:a@b.c", "foo:///x",
    "http://u:p@h.com:8080/a/b?q=1&r=2#f", "http://[::1]:80/p",
    # not URIs at all -- the splitter is still total over them
    "", "/", "//", "///", "?q", "#f", "?", "#", "//h.com/p?q#f",
    "example.com/x", "1http://x", "a b c", "??q##f", ":", ":/",
    # the delimiters in unusual orders, where a greedy group could steal bytes
    "http://x#f?notquery", "http://x?q#f#g", "s://a?#", "s:#f", "s:?q",
    # a raw line terminator: the reason `(?s)` is in the expression at all
    "http://x/a\nb", "http://x#f\n", "\n",
    # non-ASCII, to confirm the byte discipline holds outside ASCII
    "http://\u00e9.com/p", "http://x/\u00e9?\u00e9#\u00e9"
  )
  round_tripped <- recompose_rfc3986(split_rfc3986_appendix_b(corpus))
  expect_identical(round_tripped, corpus)
})

test_that("the oracle reports validity and decomposition independently", {
  # A successful split is NOT acceptance -- treating it as such would make the
  # instrument green on garbage. `rfc3986_reference_parse()` must therefore
  # carry a `valid` column that can be FALSE on a row that still decomposes.
  r <- rfc3986_reference_parse(c("http://a|b/", "example.com/x", "http://x/"))
  expect_identical(r$valid, c(FALSE, FALSE, TRUE))
  # ...and the invalid rows still decomposed, rather than coming back NA.
  expect_identical(r$path, c("/", "example.com/x", "/"))
  expect_identical(r$authority, c("a|b", NA, "x"))
})

test_that("the oracle propagates NA instead of inventing components", {
  r <- rfc3986_reference_parse(NA_character_)
  expect_identical(nrow(r), 1L)
  expect_true(is.na(r$valid))
  expect_true(all(is.na(r[c("scheme", "authority", "path", "query",
                            "fragment", "host")])))
  # A zero-length input must give a zero-row frame with the columns intact, so
  # a differential harness can rbind it without special-casing.
  z <- rfc3986_reference_parse(character(0))
  expect_identical(nrow(z), 0L)
  expect_true(all(c("valid", "authority", "path", "host") %in% names(z)))
})
