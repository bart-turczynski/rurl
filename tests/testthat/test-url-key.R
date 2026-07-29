# Output surface (e) -- the v3 comparison key engine (R/url-key.R).
#
# These cover the cells VD-001 lists under "key surface" and "key-policy rows",
# plus the whole 14-row scheme/port truth table, for the `exact` scheme-equality
# mode. They are the verification half of the engine slice; the join cells stay
# deferred until the six joins exist.
#
# Every reference is to design/work/url-v3/contracts/key-join-contracts.md.

key <- function(...) rurl:::.url_key_compute_vec(...)
policy <- function(...) rurl:::.url_key_policy_spec(...)

same <- function(a, b, policy = rurl:::.url_key_policy_spec()) {
  k <- key(c(a, b), policy)
  identical(as.character(k)[[1L]], as.character(k)[[2L]])
}

# --- key surface rows (:63-71) -----------------------------------------------

test_that("the key is length-preserving and names-preserving", {
  u <- c(a = "http://x.com/", b = "not a url at all", c = NA_character_)
  k <- key(u)
  expect_length(k, 3L)
  expect_named(as.character(k), c("a", "b", "c"))
})

test_that("a zero-length input yields a zero-length key, not an error", {
  k <- key(character(0))
  expect_length(k, 0L)
  expect_s3_class(k, "rurl_url_key")
})

test_that("factor input is accepted under explicit coercion", {
  expect_identical(
    as.character(key(factor("http://x.com/"))),
    as.character(key("http://x.com/"))
  )
})

test_that("the key is a classed non-URL object carrying both versions", {
  k <- key("http://x.com/")
  expect_s3_class(k, "rurl_url_key")
  expect_identical(attr(k, "key_version"), 1L)
  expect_identical(attr(k, "schema_version"), 1L)
  # Never a URL, and never mistakable for one.
  expect_false(grepl("^https?://", as.character(k)[[1L]]))
})

test_that("the policy is not silently recycled and must be a policy object", {
  expect_error(key("http://x.com/", policy = "whatwg"), "rurl_url_key_policy")
})

test_that("subsetting preserves class, versions and the aligned reasons", {
  k <- key(c("http://x.com/", NA_character_, "http://y.com/"))
  s <- k[2:3]
  expect_s3_class(s, "rurl_url_key")
  expect_identical(attr(s, "key_version"), 1L)
  expect_identical(attr(s, "keyability"), c("missing-input", "ok"))
})

test_that("the printable form is diagnostics-only and never a URL", {
  f <- format(key(c("http://x.com/", NA_character_)))
  expect_match(f[[1L]], "^<url_key ")
  expect_identical(f[[2L]], "<non-keyable: missing-input>")
})

# --- eligibility / non-keyable rows (:122-130, D-C) -------------------------

test_that("non-keyable rows are NA plus a typed reason", {
  k <- key(c("http://x.com/", NA_character_, ""))
  expect_true(is.na(as.character(k)[[2L]]))
  expect_true(is.na(as.character(k)[[3L]]))
  expect_identical(
    attr(k, "keyability"), c("ok", "missing-input", "empty-input")
  )
})

test_that("missing input is never conflated with an invalid parse", {
  k <- key(c(NA_character_, ""))
  expect_false(identical(
    attr(k, "keyability")[[1L]], attr(k, "keyability")[[2L]]
  ))
})

test_that("non-keyable rows never match each other (never-match default)", {
  k <- key(c(NA_character_, NA_character_))
  # Both NA: NA == NA is not a match, which is what the join family relies on.
  expect_true(all(is.na(as.character(k))))
})

# --- scheme / port truth table (:103-118), `exact` column --------------------

test_that("HTTP and HTTPS default ports normalize against absent", {
  expect_true(same("http://h.com/", "http://h.com:80/"))
  expect_true(same("https://h.com/", "https://h.com:443/"))
})

test_that("a non-default port stays significant", {
  expect_false(same("http://h.com/", "http://h.com:8080/"))
})

test_that("another scheme's default port stays significant", {
  expect_false(same("https://h.com/", "https://h.com:80/"))
  expect_false(same("http://h.com:443/", "https://h.com/"))
})

test_that("HTTP and HTTPS are distinct under exact scheme equality", {
  expect_false(same("http://h.com/", "https://h.com/"))
  expect_false(same("http://h.com:80/", "https://h.com:443/"))
})

test_that("a missing scheme is distinct from an explicit one", {
  expect_false(same("h.com/", "http://h.com/"))
})

test_that("a missing scheme does not guess an effective scheme for its port", {
  # Ratification Q4: no effective-scheme guessing, so `:80`/`:443` on
  # scheme-less input are NOT elided.
  expect_false(same("h.com:80/", "http://h.com/"))
  expect_false(same("h.com:443/", "https://h.com/"))
})

test_that("scheme-relative is its own kind, not 'missing'", {
  expect_false(same("//h.com/", "h.com/"))
})

test_that("ftp, ws, wss and custom default ports stay literal in v1", {
  # Ratification Q8: comparison-port equivalence is HTTP/S only in key-policy
  # v1, even though WHATWG's own parser elides all five special defaults.
  expect_false(same("ftp://h.com:21/", "ftp://h.com/"))
  expect_false(same("ws://h.com:80/", "ws://h.com/"))
  expect_false(same("wss://h.com:443/", "wss://h.com/"))
  expect_false(same("cust://h.com:123/", "cust://h.com/"))
})

# --- key-policy rows (:78-95) ------------------------------------------------

test_that("scheme and host case are normalized case-insensitively", {
  expect_true(same("HTTP://EXAMPLE.COM/p", "http://example.com/p"))
})

test_that("a Unicode host and its A-label share one key", {
  expect_true(same("http://münchen.de/", "http://xn--mnchen-3ya.de/"))
})

test_that("the trailing root dot is key-significant (P3.2 D-B)", {
  expect_false(same("http://example.com./", "http://example.com/"))
})

test_that("host editing is excluded from the key", {
  # `www` stripping and subdomain/PSL trimming are cleaning, so they must NOT
  # have collapsed these.
  expect_false(same("http://www.h.com/", "http://h.com/"))
  expect_false(same("http://a.h.com/", "http://h.com/"))
})

test_that("path display and editing are excluded from the key", {
  expect_false(same("http://h.com/a/", "http://h.com/a"))
  expect_false(same("http://h.com/index.html", "http://h.com/"))
})

test_that("reserved encoded path bytes remain data", {
  # `%2F` never becomes `/` merely for comparison.
  expect_false(same("http://h.com/a%2Fb", "http://h.com/a/b"))
})

test_that("query presence is three-valued", {
  expect_false(same("http://h.com/p", "http://h.com/p?"))
  expect_false(same("http://h.com/p?", "http://h.com/p?a=1"))
})

test_that("query order and duplicates are significant", {
  expect_false(same("http://h.com/?a=1&b=2", "http://h.com/?b=2&a=1"))
  expect_false(same("http://h.com/?a=1", "http://h.com/?a=1&a=1"))
})

test_that("the fragment is ignored for web-resource identity", {
  expect_true(same("http://h.com/p#one", "http://h.com/p#two"))
  expect_true(same("http://h.com/p#", "http://h.com/p"))
})

test_that("userinfo is ignored for web-resource identity", {
  expect_true(same("http://u:pw@h.com/p", "http://h.com/p"))
  expect_true(same("http://other@h.com/p", "http://h.com/p"))
})

test_that("a credential never appears in the key or its display", {
  k <- key("http://alice:s3cret@h.com/p")
  expect_false(grepl("s3cret", as.character(k)[[1L]], fixed = TRUE))
  expect_false(grepl("alice", as.character(k)[[1L]], fixed = TRUE))
  expect_false(grepl("s3cret", format(k)[[1L]], fixed = TRUE))
})

# --- collision / injective framing (:128, P3.1 D-A.2) -----------------------

test_that("adversarial component payloads cannot collide", {
  # Each pair puts the framing separators INSIDE component values. Ambiguous
  # delimiter concatenation would collapse at least one pair.
  adversarial <- list(
    c("http://h.com/a?b", "http://h.com/a%3Fb"),
    c("http://h.com/a:1/b", "http://h.com/a/1:b"),
    c("http://h.com/3:x", "http://h.com/x"),
    c("http://h.com/-:", "http://h.com/"),
    c("http://h.com/a?x=1:2", "http://h.com/a?x=1%3A2")
  )
  for (p in adversarial) {
    expect_false(same(p[[1L]], p[[2L]]), info = paste(p, collapse = " vs "))
  }
})

test_that("distinct component tuples do not collide across a large sample", {
  u <- c(
    # No fragment-only or userinfo-only variant belongs here: Q5 makes those
    # legitimately EQUAL, so including one would assert a collision as a defect.
    "http://h.com/", "http://h.com/a", "http://h.com/a/", "http://h.com/a?",
    "http://h.com/a?q", "http://h.com/a?q=1",
    "https://h.com/", "https://h.com/a", "//h.com/", "//h.com/a",
    "h.com/", "h.com/a", "ftp://h.com/", "ftp://h.com:21/",
    "http://h.com:1/", "http://h.com:2/", "http://other.com/",
    "http://h.com./", "http://www.h.com/", "mailto:a@b.com", "urn:x:y"
  )
  k <- as.character(key(u))
  expect_identical(anyDuplicated(k[!is.na(k)]), 0L)
})

# --- non-interference invariant (:69, P3.1 D-A.3, load-bearing) -------------

test_that("no presentation dial can reach the key surface", {
  # The invariant is structural: the engine accepts only `policy` and `engine`,
  # so no cleaning/profile/display option exists to change key bytes. Guard the
  # formals so adding one becomes a deliberate, visible act.
  expect_named(
    formals(rurl:::.url_key_compute_vec), c("url", "policy", "engine")
  )
  expect_named(
    formals(rurl:::.url_key_policy_spec), c("standard", "scheme_equality")
  )
})

test_that("the key posture differs from surface (b)'s in exactly one axis", {
  # The strongest available statement of non-interference: the key inherits the
  # SAME identity posture the FSSS conformance oracle uses, and the only knob it
  # moves is `scheme_policy` -- which it must, or every missing-scheme row of
  # the truth table would be a parse failure instead of a key. If a cleaning or
  # display knob ever diverges here, it shows up as a second name in this diff.
  fsss <- rurl:::.fsss_parse_options("whatwg")
  key_opts <- rurl:::.url_key_parse_options(policy())
  nm <- union(names(fsss), names(key_opts))
  differing <- nm[!vapply(
    nm, function(k) identical(fsss[[k]], key_opts[[k]]), logical(1)
  )]
  expect_identical(differing, "scheme_policy")
  expect_identical(key_opts$scheme_policy, "infer")
  expect_identical(key_opts$scheme_acceptance, "general")
})

test_that("the key is not derived from clean_url", {
  # clean_url collapses these; identity must not. If the key were ever rebuilt
  # on surface (c) this is the assertion that breaks.
  a <- "http://www.h.com/index.html"
  b <- "https://h.com/"
  expect_identical(
    get_clean_url(
      a, protocol_handling = "https", www_handling = "strip",
      index_page_handling = "strip"
    ),
    get_clean_url(b)
  )
  expect_false(same(a, b))
})

# --- policy object (:66) ----------------------------------------------------

test_that("the policy default standard is whatwg (P3.2 D-A)", {
  expect_identical(policy()$standard, "whatwg")
  expect_identical(policy()$scheme_equality, "exact")
})

test_that("an unnamed standard is rejected -- key bytes must be freezable", {
  expect_error(policy(standard = "definitely-not-a-standard"))
})

test_that("selecting a different standard changes the key", {
  expect_false(same(
    "http://h.com/a/../b", "http://h.com/b",
    policy = policy(standard = "rfc3986")
  ))
  # WHATWG resolves dot segments during parsing, so the same pair collapses.
  expect_true(same("http://h.com/a/../b", "http://h.com/b"))
})

test_that("keys minted under different policies never compare equal", {
  a <- as.character(key("http://h.com/p", policy(standard = "whatwg")))
  b <- as.character(key("http://h.com/p", policy(standard = "rfc3986")))
  expect_false(identical(a, b))
})

test_that("the non-transitive relaxed scheme modes are refused, not guessed", {
  # RURL-ixlultql: rows 1/2/7 of the truth table force HTTP:80 ~ HTTPS:443
  # under transitive closure, while row 6 declares that pair distinct. No
  # equivalence relation satisfies both, so the engine refuses rather than
  # installing one reading of a SETTLED contract as fact.
  expect_error(policy(scheme_equality = "http_https"), "RURL-ixlultql")
  expect_error(policy(scheme_equality = "http_https_missing"), "RURL-ixlultql")
})

# --- determinism -------------------------------------------------------------

test_that("the key is stable across repeated computation", {
  u <- c("http://h.com/a?q=1", "münchen.de/p", NA_character_, "")
  expect_identical(as.character(key(u)), as.character(key(u)))
})

test_that("vectorized and element-wise computation agree", {
  u <- c("http://h.com/a", "ftp://h.com:21/", NA_character_, "//h.com/b", "")
  one <- vapply(u, function(x) as.character(key(x))[[1L]], character(1),
                USE.NAMES = FALSE)
  expect_identical(as.character(key(u)), one)
})
