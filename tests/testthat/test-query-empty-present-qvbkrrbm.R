# get_query() on a present but empty query (RURL-qvbkrrbm). The WHATWG URL
# Standard's query state sets the query to "" (not null) on `?`, and RFC 3986
# sections 3.4 and 5.3 define the component whenever `?` is present, so
# `https://example.com/?` has an empty query while `https://example.com/` has
# none. The `query` column of safe_parse_urls() keeps NA for both (ADR 0012).

standards <- c("whatwg", "rfc3986")

test_that("a bare `?` gives an empty query; no `?` gives NA", {
  for (std in standards) {
    for (dec in c(TRUE, FALSE)) {
      expect_identical(
        get_query(
          c("https://example.com/?", "https://example.com/",
            "https://example.com/?#f", "https://example.com/#?",
            "https://example.com/?a"),
          decode = dec, url_standard = std
        ),
        c("", NA, "", NA, "a"),
        info = paste(std, dec)
      )
    }
  }
})

test_that("missing, empty and unparseable inputs stay NA", {
  for (std in standards) {
    expect_identical(
      get_query(c(NA_character_, "", "http://[::1/?"), url_standard = std),
      rep(NA_character_, 3L),
      info = std
    )
  }
})

test_that("presence follows each standard's preprocessing", {
  # WHATWG strips tab, LF and CR before parsing, so `?\t` is still empty.
  expect_identical(
    get_query("https://example.com/?\t", url_standard = "whatwg"), ""
  )
  # A scheme-less web input and a general-scheme URL.
  expect_identical(get_query("example.com/?"), "")
  expect_identical(
    get_query("mailto:x?", scheme_acceptance = "general",
              url_standard = "rfc3986"),
    ""
  )
})

test_that("the filtering path reads an empty query as empty", {
  u <- c("https://example.com/?", "https://example.com/")
  for (std in standards) {
    for (dec in c(TRUE, FALSE)) {
      expect_identical(
        get_query(u, sort_params = TRUE, decode = dec, url_standard = std),
        c("", NA), info = paste(std, dec)
      )
      expect_identical(
        get_query(u, query_handling = "filter", decode = dec,
                  url_standard = std),
        c("", NA), info = paste(std, dec)
      )
    }
    # drop gives "" for any present query, a bare `?` included, and NA for
    # an absent one.
    for (dec in c(TRUE, FALSE)) {
      expect_identical(
        get_query(c("https://example.com/?a", u), query_handling = "drop",
                  decode = dec, url_standard = std),
        c("", "", NA), info = paste(std, dec)
      )
    }
    # A query emptied by filtering keeps its earlier result.
    expect_identical(
      get_query("https://example.com/?utm_source=x",
                query_handling = "filter", url_standard = std),
      "", info = std
    )
  }
})

test_that("the list form is unchanged: an empty list for both", {
  u <- c("https://example.com/?", "https://example.com/")
  for (std in standards) {
    expect_identical(
      get_query(u, format = "list", url_standard = std), list(list(), list()),
      info = std
    )
    expect_identical(
      get_query(u, format = "list", query_handling = "filter",
                url_standard = std),
      list(list(), list()), info = std
    )
  }
})

test_that("the record column and the serializer are unchanged", {
  u <- c("https://example.com/?", "https://example.com/")
  for (std in standards) {
    expect_identical(
      safe_parse_urls(u, url_standard = std)$query, c(NA_character_, NA),
      info = std
    )
  }
  expect_identical(serialize_url(u), u)
})
