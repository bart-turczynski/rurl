# The EXPORTED half of output surface (e) -- the eight public wrappers over the
# key and join engines (R/url-key.R, R/url-join.R).
#
# The engines are already pinned by test-url-key.R (the key surface, the
# key-policy rows and all 14 truth-table rows) and test-url-join.R (the six
# operations and the cross-cutting axes). This file pins the only thing a thin
# wrapper can get wrong, which is precisely what a thin wrapper is most likely
# to get wrong: the SIGNATURE. A dropped argument, a defaulted-away axis or a
# silently-added dial would leave every engine test green.
#
# Every reference is to design/work/url-v3/contracts/key-join-contracts.md; the
# cell -> evidence map is design/work/url-v3/verification/key-join-discharge.md.

impl <- function(...) rurl:::.url_join_impl(...)
spec <- function(...) rurl:::.url_key_policy_spec(...)

x_df <- data.frame(
  URL = c("http://example.com:80/a", "https://example.com/b",
          "http://example.com/c", "not a url", NA_character_),
  n = 1:5,
  stringsAsFactors = FALSE
)
y_df <- data.frame(
  page = c("http://example.com/a", "http://example.com/a",
           "http://example.com/b", "http://example.com/c"),
  tag = c("a1", "a2", "b", "c"),
  # Deliberately collides with `x_df$n`, so `suffix` has something to do:
  # against a non-overlapping schema the axis is inert and a wrapper that
  # dropped it would pass unnoticed.
  n = c(10L, 20L, 30L, 40L),
  stringsAsFactors = FALSE
)

# --- the export slice itself -------------------------------------------------

test_that("all eight VD-001 surfaces are exported together", {
  # VD-001's surface_probe names exactly these eight. The register's failure
  # condition 3 fires on ANY one of them, so the set is asserted as a set: a
  # future change that exports seven of the eight is the shape the deferral
  # gate exists to catch, and it should fail here first.
  eight <- c("get_url_key", "url_key_policy", "url_inner_join",
             "url_left_join", "url_right_join", "url_full_join",
             "url_semi_join", "url_anti_join")
  expect_true(all(eight %in% getNamespaceExports("rurl")))
})

test_that("the exported wrappers add no behavior over the engines", {
  # Byte-identity with the engine, under a NON-DEFAULT setting of every axis,
  # so a wrapper that forwards nothing cannot pass by accident.
  p <- spec(scheme_equality = "http_https")
  args <- list(
    x = x_df, y = y_df, by = c(URL = "page"), policy = p,
    suffix = c("_L", "_R"), key_name = "k", relationship = "many-to-many",
    multiple = "last", invalid = "drop", warnings = "reject"
  )
  six <- list(
    inner = url_inner_join, left = url_left_join, right = url_right_join,
    full = url_full_join
  )
  for (nm in names(six)) {
    expect_identical(
      do.call(six[[nm]], args),
      do.call(impl, c(args, list(type = nm))),
      info = nm
    )
  }
  # semi/anti take no suffix or multiple: they emit x columns only, and each x
  # row at most once, so neither axis can arise.
  narrow <- args[setdiff(names(args), c("suffix", "multiple"))]
  for (nm in c("semi", "anti")) {
    f <- if (nm == "semi") url_semi_join else url_anti_join
    expect_identical(do.call(f, narrow),
                     do.call(impl, c(narrow, list(type = nm))), info = nm)
  }
  expect_identical(
    as.character(get_url_key(x_df$URL, p)),
    as.character(rurl:::.url_key_compute_vec(x_df$URL, p))
  )
})

test_that("every join axis actually reaches the engine", {
  # The companion to the delegation test above: each axis is shown to CHANGE
  # the result, so "identical to the engine" is not identical-to-the-default.
  base <- url_left_join(x_df, y_df, by = c(URL = "page"))
  expect_false(identical(
    base, url_left_join(x_df, y_df, by = c(URL = "page"), invalid = "drop")))
  expect_false(identical(
    base, url_left_join(x_df, y_df, by = c(URL = "page"), multiple = "last")))
  expect_false(identical(
    base, url_left_join(x_df, y_df, by = c(URL = "page"), key_name = "k")))
  expect_false(identical(
    base, url_left_join(x_df, y_df, by = c(URL = "page"),
                        suffix = c("_L", "_R"))))
  expect_false(identical(
    base, url_left_join(x_df, y_df, by = c(URL = "page"),
                        policy = spec(scheme_equality = "http_https"))))
  expect_error(
    url_left_join(x_df, y_df, by = c(URL = "page"),
                  relationship = "one-to-one"),
    class = "rurl_url_join_relationship_error"
  )
  expect_error(
    url_left_join(x_df, y_df, by = c(URL = "page"), invalid = "error"),
    class = "rurl_url_join_invalid_error"
  )
})

# --- the signature asymmetry, pinned -----------------------------------------

test_that("get_url_key exposes no engine dial and the joins all do", {
  # Not a style preference. The key frames no PSL-derived component, so a
  # `pslr` engine cannot move a key byte -- an `engine =` argument on
  # get_url_key() would be a dial that provably does nothing. `warnings =
  # "reject"` DOES read the L3 public-suffix annotation, so the joins expose
  # one. Identity is engine-independent; eligibility is not, and a later
  # "make the signatures consistent" refactor in either direction should fail
  # here.
  expect_named(formals(get_url_key), c("url", "policy"))
  for (f in list(url_inner_join, url_left_join, url_right_join, url_full_join,
                 url_semi_join, url_anti_join)) {
    expect_true("engine" %in% names(formals(f)))
  }
})

test_that("semi and anti expose no suffix or multiple axis", {
  for (f in list(url_semi_join, url_anti_join)) {
    expect_false(any(c("suffix", "multiple") %in% names(formals(f))))
  }
})

test_that("the four pair-emitting joins share one signature", {
  sigs <- lapply(
    list(url_inner_join, url_left_join, url_right_join, url_full_join),
    function(f) names(formals(f))
  )
  expect_identical(unique(sigs), list(sigs[[1L]]))
  expect_identical(
    sigs[[1L]],
    c("x", "y", "by", "policy", "suffix", "key_name", "relationship",
      "multiple", "invalid", "warnings", "engine")
  )
})

# --- url_key_policy() ---------------------------------------------------------

test_that("url_key_policy validates its vocabulary at the public edge", {
  expect_s3_class(url_key_policy(), "rurl_url_key_policy")
  expect_identical(url_key_policy(), spec())
  expect_identical(url_key_policy("rfc3986"), spec("rfc3986"))
  expect_error(url_key_policy("ada"))
  expect_error(url_key_policy(scheme_equality = "loose"))
  expect_error(url_key_policy(standard = NULL))
})

test_that("http_https_missing refuses at the public edge, with its reason", {
  # The mode is in the vocabulary and errors rather than guessing: the pair it
  # would collapse also differs on authority_delimiter_present, which P1.2 D-A
  # frames as independent identity (RURL-ixxvjjwj).
  expect_error(url_key_policy(scheme_equality = "http_https_missing"),
               "not implemented")
  expect_error(url_key_policy(scheme_equality = "http_https_missing"),
               "RURL-ixxvjjwj")
})

test_that("the policy prints its four equality-deciding fields", {
  out <- utils::capture.output(print(url_key_policy()))
  expect_match(out, "rurl_url_key_policy")
  expect_match(out, "standard=whatwg")
  expect_match(out, "scheme_equality=exact")
  expect_match(out, "key_version=1")
  expect_match(out, "schema_version=1")
})

# --- the documented public claims --------------------------------------------

test_that("no public cleaning or profile dial can reach the exported key", {
  # The non-interference invariant (P3.1 D-A.3), asserted through the EXPORTED
  # surface rather than the engine: get_url_key() takes no cleaning argument at
  # all, so the only way a presentation choice could reach it is via a global,
  # and the key of a URL is invariant to every dial get_clean_url() exposes.
  u <- "HTTP://WWW.Example.com:80/A/index.html?b=2&a=1#frag"
  k <- as.character(get_url_key(u))
  expect_identical(as.character(get_url_key(u)), k)
  expect_false(any(c("profile", "www_handling", "path_encoding",
                     "case_handling", "port_handling", "query_handling") %in%
                     names(formals(get_url_key))))
  # The cleaned display of the same URL varies wildly; the key does not.
  cleaned <- c(
    get_clean_url(u), get_clean_url(u, www_handling = "strip"),
    get_clean_url(u, trailing_slash_handling = "strip"),
    get_clean_url(u, index_page_handling = "strip"),
    get_clean_url(u, port_handling = "keep")
  )
  expect_gt(length(unique(cleaned)), 1L)
  expect_length(unique(as.character(get_url_key(rep(u, 5L)))), 1L)
})

test_that("the exported key never renders as a URL", {
  k <- get_url_key("http://user:pw@example.com/a")
  expect_false(grepl("://", format(k), fixed = TRUE))
  expect_false(grepl("user", format(k), fixed = TRUE))
  expect_false(grepl("pw", format(k), fixed = TRUE))
})

test_that("the documented join examples hold", {
  pages <- data.frame(
    URL = c("http://example.com:80/a", "https://example.com/b",
            "http://example.com/c?", "not a url"),
    clicks = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  meta <- data.frame(
    URL = c("http://example.com/a", "http://example.com/b",
            "http://example.com/c"),
    title = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  inner <- url_inner_join(pages, meta, by = "URL")
  expect_identical(nrow(inner), 1L)
  expect_identical(inner$title, "A")
  expect_named(inner, c("URL.x", "clicks", "URL.y", "title"))

  expect_identical(nrow(url_left_join(pages, meta, by = "URL")), 4L)
  expect_identical(url_anti_join(pages, meta, by = "URL")$clicks,
                   c(20, 30, 40))
  expect_named(url_anti_join(pages, meta, by = "URL"), c("URL", "clicks"))

  relaxed <- url_inner_join(
    pages, meta, by = "URL",
    policy = url_key_policy(scheme_equality = "http_https")
  )
  expect_identical(nrow(relaxed), 2L)
  expect_identical(relaxed$title, c("A", "B"))

  keyed <- url_inner_join(pages, meta, by = "URL", key_name = "key")
  expect_s3_class(keyed$key, "rurl_url_key")
})

test_that("the documented key examples hold", {
  expect_true(identical(
    as.character(get_url_key("http://example.com:80/a")),
    as.character(get_url_key("http://example.com/a"))
  ))
  k <- get_url_key(c("http://u:pw@example.com/a#top", "http://example.com/a"))
  eq <- k[1] == k[2]
  expect_true(eq)
  k <- get_url_key(c("http://example.com/?a=1&b=2",
                     "http://example.com/?b=2&a=1"))
  expect_false(isTRUE(k[1] == k[2]))
  u <- c("HTTP://Example.com/a", "http://example.com/a", "http://example.com/b")
  expect_identical(u[!duplicated(get_url_key(u))],
                   c("HTTP://Example.com/a", "http://example.com/b"))
  expect_identical(
    attr(get_url_key(c("http://example.com/", NA, "", ":::")), "keyability"),
    c("ok", "missing-input", "empty-input", "invalid-parse")
  )
})
