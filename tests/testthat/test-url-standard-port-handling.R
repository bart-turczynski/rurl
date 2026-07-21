# Tests for the port_handling vertical slice (RURL-qdlvldts, epic
# RURL-umnmmkce / parent RURL-uyjheruh; PRD v2 §5 D1) and WHATWG parse-level
# default-port elision (RURL-uvilvhnm). port_handling is a standalone,
# standard-INDEPENDENT editorial knob (like www_handling): "strip_default"
# gives spec-style clean URL rendering, while "keep" is the explicit literal
# port override.

# --- Defaults: byte-identical to pre-feature behavior ------------------------

test_that("no port_handling / url_standard NULL is byte-identical to today", {
  urls <- c(
    "http://example.com:8080/path", "http://example.com:80/path",
    "https://example.com:443/path", "ftp://example.com:21/path",
    "ftps://example.com:990/path", "http://example.com/path"
  )
  expect_identical(
    get_clean_url(urls),
    c(
      "http://example.com/path", "http://example.com/path",
      "https://example.com/path", "ftp://example.com/path",
      "ftps://example.com/path", "http://example.com/path"
    )
  )
  # Explicit default value matches the implicit default.
  expect_identical(
    get_clean_url(urls, port_handling = "exclude"), get_clean_url(urls)
  )
  # safe_parse_url()/safe_parse_urls() default the same way.
  expect_identical(
    safe_parse_urls(urls)$clean_url, get_clean_url(urls)
  )
})

test_that("strip_all is an explicit alias of exclude", {
  u <- "http://example.com:8080/path"
  expect_identical(
    get_clean_url(u, port_handling = "strip_all"),
    get_clean_url(u, port_handling = "exclude")
  )
})

# --- get_port() stays faithful regardless of port_handling -------------------

test_that("get_port is raw and unaffected by port_handling without selector", {
  u <- c("http://example.com:8080/path", "http://example.com/path")
  expected <- c(8080L, NA_integer_)
  expect_identical(get_port(u), expected)
  for (ph in c("exclude", "keep", "strip_default", "strip_all")) {
    expect_identical(
      safe_parse_urls(u, port_handling = ph)$port, expected
    )
  }
})

test_that("whatwg parse output nulls default ports unless explicitly kept", {
  u <- c(
    "http://example.com:80/path",
    "https://example.com:443/path",
    "ftp://example.com:21/path",
    "http://example.com:8080/path",
    "ftps://example.com:990/path"
  )

  expect_identical(
    safe_parse_urls(u, url_standard = "whatwg")$port,
    c(NA_integer_, NA_integer_, NA_integer_, 8080L, 990L)
  )
  expect_identical(
    safe_parse_urls(u, url_standard = "whatwg", port_handling = "keep")$port,
    c(80L, 443L, 21L, 8080L, 990L)
  )
  expect_identical(
    safe_parse_urls(u, url_standard = "rfc3986")$port,
    c(80L, 443L, 21L, 8080L, 990L)
  )
})

# --- port_handling = "keep": verbatim under rfc3986 / no selector ------------

test_that("keep includes the port verbatim with no url_standard", {
  expect_identical(
    get_clean_url("http://example.com:80/path", port_handling = "keep"),
    "http://example.com:80/path"
  )
  expect_identical(
    get_clean_url("http://example.com:8080/path", port_handling = "keep"),
    "http://example.com:8080/path"
  )
})

test_that("keep includes the port verbatim under rfc3986 (no elision)", {
  expect_identical(
    get_clean_url(
      "http://example.com:80/path",
      port_handling = "keep", url_standard = "rfc3986"
    ),
    "http://example.com:80/path"
  )
  expect_identical(
    get_clean_url(
      "https://example.com:443/path",
      port_handling = "keep", url_standard = "rfc3986"
    ),
    "https://example.com:443/path"
  )
  expect_identical(
    get_clean_url(
      "ftp://example.com:21/path",
      port_handling = "keep", url_standard = "rfc3986"
    ),
    "ftp://example.com:21/path"
  )
})

# --- port_handling = "strip_default": spec-style default-port elision --------

test_that("strip_default elides default ports for each special scheme", {
  expect_identical(
    get_clean_url(
      "http://example.com:80/path",
      port_handling = "strip_default", url_standard = "whatwg"
    ),
    "http://example.com/path"
  )
  expect_identical(
    get_clean_url(
      "https://example.com:443/path",
      port_handling = "strip_default", url_standard = "whatwg"
    ),
    "https://example.com/path"
  )
  expect_identical(
    get_clean_url(
      "ftp://example.com:21/path",
      port_handling = "strip_default", url_standard = "whatwg"
    ),
    "ftp://example.com/path"
  )
})

test_that("keep includes the port verbatim under whatwg as an override", {
  expect_identical(
    get_clean_url(
      "http://example.com:80/path",
      port_handling = "keep", url_standard = "whatwg"
    ),
    "http://example.com:80/path"
  )
  expect_identical(
    get_clean_url(
      "https://example.com:443/path",
      port_handling = "keep", url_standard = "whatwg"
    ),
    "https://example.com:443/path"
  )
  expect_identical(
    get_clean_url(
      "ftp://example.com:21/path",
      port_handling = "keep", url_standard = "whatwg"
    ),
    "ftp://example.com:21/path"
  )
  expect_identical(
    get_clean_url(
      "http://example.com:8080/path",
      port_handling = "keep", url_standard = "whatwg"
    ),
    "http://example.com:8080/path"
  )
  expect_identical(
    get_clean_url(
      "https://example.com:8443/path",
      port_handling = "keep", url_standard = "whatwg"
    ),
    "https://example.com:8443/path"
  )
  expect_identical(
    get_clean_url(
      "ftp://example.com:2121/path",
      port_handling = "keep", url_standard = "whatwg"
    ),
    "ftp://example.com:2121/path"
  )
})

test_that("ftps is not WHATWG-special so its port is never elided", {
  expect_identical(
    get_clean_url(
      "ftps://example.com:990/path",
      port_handling = "keep", url_standard = "whatwg"
    ),
    "ftps://example.com:990/path"
  )
  expect_identical(
    get_clean_url(
      "ftps://example.com:990/path",
      port_handling = "keep", url_standard = "rfc3986"
    ),
    "ftps://example.com:990/path"
  )
})

# --- port_handling = "strip_default" -----------------------------------------

test_that("strip_default keeps non-default ports, indep of url_standard", {
  expect_identical(
    get_clean_url(
      "http://example.com:80/path", port_handling = "strip_default"
    ),
    "http://example.com/path"
  )
  expect_identical(
    get_clean_url(
      "http://example.com:8080/path", port_handling = "strip_default"
    ),
    "http://example.com:8080/path"
  )
  # Same result whether or not a standard is selected (D1: standard-agnostic).
  expect_identical(
    get_clean_url(
      "http://example.com:80/path",
      port_handling = "strip_default", url_standard = "rfc3986"
    ),
    "http://example.com/path"
  )
  expect_identical(
    get_clean_url(
      "http://example.com:80/path",
      port_handling = "strip_default", url_standard = "whatwg"
    ),
    "http://example.com/path"
  )
  # ftps has no defined default, so an ftps port is never stripped.
  expect_identical(
    get_clean_url(
      "ftps://example.com:990/path", port_handling = "strip_default"
    ),
    "ftps://example.com:990/path"
  )
})

# --- port_handling validation -------------------------------------------------

test_that("port_handling rejects an unknown value", {
  expect_error(
    get_clean_url("http://example.com/", port_handling = "bogus"),
    "arg"
  )
  expect_error(
    safe_parse_url("http://example.com/", port_handling = "bogus"),
    "arg"
  )
})

# --- Diagnostics: explicit-default-port / non-default-port -------------------

test_that("port diagnostics fire as facts, independent of port_handling", {
  expect_identical(
    get_url_diagnostics("http://example.com:80/path", url_standard = "whatwg"),
    "explicit-default-port"
  )
  expect_identical(
    get_url_diagnostics(
      "http://example.com:80/path", url_standard = "rfc3986"
    ),
    "explicit-default-port"
  )
  expect_identical(
    get_url_diagnostics(
      "http://example.com:8080/path", url_standard = "whatwg"
    ),
    "non-default-port"
  )
  expect_identical(
    get_url_diagnostics(
      "http://example.com:8080/path", url_standard = "rfc3986"
    ),
    "non-default-port"
  )
})

test_that("port diagnostics cover https/ftp defaults and ftps (no default)", {
  expect_identical(
    get_url_diagnostics(
      "https://example.com:443/path", url_standard = "whatwg"
    ),
    "explicit-default-port"
  )
  expect_identical(
    get_url_diagnostics("ftp://example.com:21/path", url_standard = "whatwg"),
    "explicit-default-port"
  )
  # ftps has no WHATWG default -- any explicit port is "non-default".
  expect_identical(
    get_url_diagnostics(
      "ftps://example.com:990/path", url_standard = "whatwg"
    ),
    "non-default-port"
  )
})

test_that("no port diagnostics fire when no port/no selector is present", {
  expect_identical(
    get_url_diagnostics("http://example.com/path", url_standard = "whatwg"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics("http://example.com:80/path"), character(0)
  )
})

test_that("port diagnostics don't perturb parse_status", {
  expect_identical(
    get_parse_status(
      "http://example.com:80/path", url_standard = "whatwg"
    ),
    "ok"
  )
  expect_identical(
    get_parse_status(
      "http://example.com:8080/path", url_standard = "whatwg"
    ),
    "ok"
  )
})

# --- Cache correctness: clean_url is Stage-B, never stale across calls -------

test_that("switching port_handling on one URL never returns stale clean_url", {
  rurl_clear_caches()
  u <- "http://example.com:80/path"
  expect_identical(get_clean_url(u), "http://example.com/path")
  expect_identical(
    get_clean_url(u, port_handling = "keep"), "http://example.com:80/path"
  )
  expect_identical(get_clean_url(u), "http://example.com/path")
  expect_identical(
    get_clean_url(u, port_handling = "keep", url_standard = "whatwg"),
    "http://example.com:80/path"
  )
  expect_identical(
    get_clean_url(u, port_handling = "keep", url_standard = "rfc3986"),
    "http://example.com:80/path"
  )
  expect_identical(
    get_clean_url(u, port_handling = "keep"), "http://example.com:80/path"
  )
})

# --- Accessor parity ----------------------------------------------------------

test_that("get_clean_url matches safe_parse_url()$clean_url on port_handling", {
  u <- "http://example.com:8080/path"
  for (ph in c("exclude", "keep", "strip_default", "strip_all")) {
    expect_identical(
      get_clean_url(u, port_handling = ph),
      safe_parse_url(u, port_handling = ph)$clean_url
    )
  }
  expect_identical(
    get_clean_url(u, port_handling = "keep", url_standard = "whatwg"),
    safe_parse_url(u, port_handling = "keep", url_standard = "whatwg")$clean_url
  )
})

# --- No new columns/fields on the parse result --------------------------------

test_that("port_handling adds no new columns/fields to the parse result", {
  cols_before <- names(safe_parse_urls("http://example.com:8080/"))
  invisible(safe_parse_urls(
    "http://example.com:8080/", port_handling = "keep"
  ))
  cols_after <- names(safe_parse_urls("http://example.com:8080/"))
  expect_identical(cols_after, cols_before)
})

# --- WHATWG non-special port validation (RURL-kknambrz, T2) ------------------
# Under scheme_acceptance="general", url_standard="whatwg", a non-null port must
# be ASCII digits only and <= 65535; an empty port (`:` then end/`/`/`?`/`#`) is
# null and legal. Non-digit (`-`, `+`, letters) or overflow is a parse failure.
# The RFC profile owns its own port grammar (sibling T4/T5) and is untouched.

test_that("whatwg general rejects non-numeric / signed / overflow ports", {
  bad <- c(
    "data://test:test", "scheme://example.com:-1",
    "scheme://example.com:+1", "scheme://example.com:99999"
  )
  res <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, rep("error", length(bad)))
})

test_that("whatwg general accepts valid, empty, and boundary ports", {
  ok <- c("foo://host:80/", "foo://host:/", "foo://host:65535/")
  res <- safe_parse_urls(
    ok, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, c("ok", "ok", "ok"))
  expect_identical(res$host, c("host", "host", "host"))
  # An empty port after `:` is null; a valid one is carried as an integer.
  expect_identical(res$port, c(80L, NA_integer_, 65535L))
})
