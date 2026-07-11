# Per-request pslr engine (RURL-mhibnqbd): domain / TLD / subdomain extraction
# can resolve against a caller-supplied `pslr::psl_engine()` snapshot instead of
# pslr's session-global default, with `engine = NULL` (the default) preserving
# the historical behavior byte-for-byte.

# A minimal, well-formed PSL that makes `example.com` itself a public suffix
# (the real list treats it as a registrable domain). Domain/TLD extraction over
# this engine is therefore VISIBLY different from the default list, which is
# what lets these tests prove the engine is actually consulted. pslr requires
# both an ICANN and a PRIVATE section with the official markers.
.make_custom_engine <- function(env = parent.frame()) {
  skip_if_not_installed("pslr", minimum_version = "1.1.0")
  path <- withr::local_tempfile(fileext = ".dat", .local_envir = env)
  writeLines(
    c(
      "// ===BEGIN ICANN DOMAINS===",
      "com",
      "example.com",
      "// ===END ICANN DOMAINS===",
      "// ===BEGIN PRIVATE DOMAINS===",
      "priv.example",
      "// ===END PRIVATE DOMAINS==="
    ),
    path
  )
  pslr::psl_engine(source = "path", path = path)
}

test_that("engine = NULL is byte-identical to omitting the argument", {
  skip_if_not_installed("pslr", minimum_version = "1.1.0")
  urls <- c(
    "https://a.blog.example.co.uk/x",
    "http://www.example.com",
    "https://sub.example.org:8080/p?q=1",
    "not a url"
  )
  # The default path (no engine) and an explicit engine = NULL must agree on
  # every domain-derived output, and on the full parsed frame.
  expect_equal(get_domain(urls), get_domain(urls, engine = NULL))
  expect_equal(get_tld(urls), get_tld(urls, engine = NULL))
  expect_equal(get_subdomain(urls), get_subdomain(urls, engine = NULL))
  expect_equal(
    safe_parse_urls(urls),
    safe_parse_urls(urls, engine = NULL)
  )
})

test_that("a default psl_engine() reproduces the session-global default", {
  skip_if_not_installed("pslr", minimum_version = "1.1.0")
  e_default <- pslr::psl_engine()
  urls <- c("https://a.b.co.uk/x", "http://www.example.com", "https://x.dev")
  # The bundled engine IS the default list, so passing it explicitly must not
  # change any output relative to engine = NULL.
  expect_equal(get_domain(urls, engine = e_default), get_domain(urls))
  expect_equal(get_tld(urls, engine = e_default), get_tld(urls))
  expect_equal(
    safe_parse_urls(urls, engine = e_default)$domain,
    safe_parse_urls(urls)$domain
  )
})

test_that("a custom-list engine changes domain / TLD / subdomain extraction", {
  e <- .make_custom_engine()
  u <- "https://a.example.com/p"
  # Default list: example.com is registrable, so a.example.com has domain
  # example.com, TLD com, subdomain "a".
  expect_equal(get_domain(u), "example.com")
  expect_equal(get_tld(u), "com")
  expect_equal(get_subdomain(u), "a")
  # Custom list: example.com is a public suffix, so a.example.com is itself the
  # registrable domain, TLD example.com, and there is no subdomain.
  expect_equal(get_domain(u, engine = e), "a.example.com")
  expect_equal(get_tld(u, engine = e), "example.com")
  expect_true(is.na(get_subdomain(u, engine = e)))
  # The engine also drives safe_parse_urls() and the subdomain-trimmed host.
  expect_equal(safe_parse_url(u, engine = e)$domain, "a.example.com")
  expect_equal(
    get_host(u, subdomain_levels_to_keep = 0, engine = e),
    "a.example.com"
  )
})

test_that("switching engines does not return a stale cached domain/TLD", {
  e <- .make_custom_engine()
  u <- "https://a.example.com/p"
  rurl_clear_caches()
  # Prime the cache under the default engine, then the custom one, then the
  # default again: the engine identity is part of the Stage-A cache key, so the
  # final default read must NOT reuse the custom engine's memoized result.
  d_default_1 <- get_domain(u)
  d_custom <- get_domain(u, engine = e)
  d_default_2 <- get_domain(u)
  expect_equal(d_default_1, "example.com")
  expect_equal(d_custom, "a.example.com")
  expect_equal(d_default_2, "example.com")
})

test_that("engine is validated: non-engine input errors, NULL is accepted", {
  skip_if_not_installed("pslr", minimum_version = "1.1.0")
  expect_error(
    get_domain("https://example.com", engine = "not-an-engine"),
    "psl_engine"
  )
  expect_error(
    safe_parse_url("https://example.com", engine = list()),
    "psl_engine"
  )
  expect_null(rurl:::.validate_engine(NULL))
  e <- pslr::psl_engine()
  expect_identical(rurl:::.validate_engine(e), e)
})

test_that(".engine_cache_token: NULL -> \"\", engine -> stable identity", {
  skip_if_not_installed("pslr", minimum_version = "1.1.0")
  expect_identical(rurl:::.engine_cache_token(NULL), "")
  e <- pslr::psl_engine()
  tok <- rurl:::.engine_cache_token(e)
  expect_type(tok, "character")
  expect_length(tok, 1L)
  expect_true(nzchar(tok))
  # Two engines built from the SAME list share an identity, so they share a
  # token (and hence a cache entry) -- correct, since their PSL output is equal.
  expect_identical(rurl:::.engine_cache_token(pslr::psl_engine()), tok)
})
