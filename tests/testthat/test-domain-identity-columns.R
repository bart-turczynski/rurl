# Encoding-independent identity columns (RURL-owrdsivt).
#
# safe_parse_urls()/safe_parse_url() expose the registrable domain and public
# suffix in BOTH canonical spellings -- domain_ascii/domain_unicode and
# tld_ascii/tld_unicode -- regardless of host_encoding. These are stable
# identity keys: a Unicode host and its A-label share one domain_ascii (and one
# domain_unicode), so an identity-key consumer never has to re-parse. The
# host_encoding-following `domain`/`tld` columns are unchanged (pinned below).

# The same logical registrable domain in both spellings.
unicode_url <- "http://münchen.de"
alabel_url <- "http://xn--mnchen-3ya.de"

test_that("both spellings are exposed as columns", {
  df <- safe_parse_urls(c(unicode_url, alabel_url))
  expect_true(all(
    c("domain_ascii", "domain_unicode", "tld_ascii", "tld_unicode") %in%
      names(df)
  ))
})

test_that("identity columns are encoding-independent across host_encoding", {
  for (enc in c("keep", "idna", "unicode")) {
    df <- safe_parse_urls(c(unicode_url, alabel_url), host_encoding = enc)
    # A Unicode host and its A-label collapse to one identity in each spelling.
    expect_identical(
      df$domain_ascii, c("xn--mnchen-3ya.de", "xn--mnchen-3ya.de")
    )
    expect_identical(df$domain_unicode, c("münchen.de", "münchen.de"))
    expect_identical(df$tld_ascii, c("de", "de"))
    expect_identical(df$tld_unicode, c("de", "de"))
  }
})

test_that("host_encoding still only selects the rendering `domain`/`tld`", {
  # keep: domain mirrors each input host's own spelling.
  keep <- safe_parse_urls(c(unicode_url, alabel_url), host_encoding = "keep")
  expect_identical(keep$domain, c("münchen.de", "xn--mnchen-3ya.de"))
  # idna: both render as the A-label; unicode: both render decoded.
  idna <- safe_parse_urls(c(unicode_url, alabel_url), host_encoding = "idna")
  expect_identical(idna$domain, c("xn--mnchen-3ya.de", "xn--mnchen-3ya.de"))
  uni <- safe_parse_urls(c(unicode_url, alabel_url), host_encoding = "unicode")
  expect_identical(uni$domain, c("münchen.de", "münchen.de"))
})

test_that("ASCII-only hosts have ascii == unicode spellings", {
  df <- safe_parse_urls("http://sub.example.co.uk/path")
  expect_identical(df$domain_ascii, df$domain_unicode)
  expect_identical(df$domain_ascii, "example.co.uk")
  expect_identical(df$tld_ascii, df$tld_unicode)
  expect_identical(df$tld_ascii, "co.uk")
})

test_that("IP hosts, null rows, and no-TLD hosts yield NA identity columns", {
  df <- safe_parse_urls(c(
    "http://192.168.1.1", "http://[::1]", "not a url", "http://localhost"
  ))
  na_cols <- c("domain_ascii", "domain_unicode", "tld_ascii", "tld_unicode")
  for (col in na_cols) {
    expect_true(all(is.na(df[[col]])), info = col)
  }
})

test_that("identity columns match host_encoding-selected domain/tld per row", {
  # Under keep, domain equals whichever spelling matches the input host: the
  # A-label input picks domain_ascii, the Unicode input picks domain_unicode.
  df <- safe_parse_urls(c(alabel_url, unicode_url), host_encoding = "keep")
  expect_identical(df$domain[1], df$domain_ascii[1])
  expect_identical(df$domain[2], df$domain_unicode[2])
  expect_identical(df$tld[1], df$tld_ascii[1])
})

test_that("scalar safe_parse_url() exposes the identity columns too", {
  res <- safe_parse_url(unicode_url, host_encoding = "idna")
  expect_identical(res$domain_ascii, "xn--mnchen-3ya.de")
  expect_identical(res$domain_unicode, "münchen.de")
  expect_identical(res$tld_ascii, "de")
  expect_identical(res$tld_unicode, "de")
})

test_that("scalar engine (._safe_parse_url_impl) stays in sync", {
  res <- rurl:::._safe_parse_url_impl(
    url = alabel_url, protocol_handling = "keep", www_handling = "none",
    tld_source = "all", case_handling = "keep",
    trailing_slash_handling = "none", index_page_handling = "keep",
    path_normalization = "none", scheme_relative_handling = "keep",
    subdomain_levels_to_keep = NULL, host_encoding = "keep",
    path_encoding = "keep"
  )
  expect_identical(res$domain_ascii, "xn--mnchen-3ya.de")
  expect_identical(res$domain_unicode, "münchen.de")
  expect_identical(res$tld_ascii, "de")
  expect_identical(res$tld_unicode, "de")
})

test_that("empty-input frame carries the identity columns with correct types", {
  empty <- safe_parse_urls(character(0))
  for (col in c("domain_ascii", "domain_unicode", "tld_ascii", "tld_unicode")) {
    expect_true(col %in% names(empty), info = col)
    expect_type(empty[[col]], "character")
  }
})
