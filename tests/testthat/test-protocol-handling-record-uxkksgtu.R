# RURL-uxkksgtu (standards-literal audit finding 6): `protocol_handling`
# answers one question only, "how is the scheme presented in `clean_url`?"
# (ADR 0010's axis table). Every other column is the parse of the input under
# the selected standard: the WHATWG URL Standard's scheme state keeps the
# scheme the input spells, a non-special scheme with no `//` has an opaque path
# and no host, and RFC 3986 section 3 puts an authority only after `//`.

test_that("a mailto: row under protocol_handling = \"https\" stays mailto:", {
  out <- safe_parse_urls(
    "mailto:user@example.com",
    url_standard = "whatwg", scheme_acceptance = "general",
    protocol_handling = "https"
  )
  expect_identical(out$parse_status, "ok")
  expect_identical(out$scheme, "mailto")
  expect_identical(out$host, NA_character_)
  expect_identical(out$user, NA_character_)
  expect_identical(out$path, "user@example.com")
  # Only http/https rows take the presented scheme: a mailto: URL is never
  # rewritten into an https: one.
  expect_identical(out$clean_url, "mailto:user@example.com")
})

test_that("get_scheme() returns the parsed scheme under every protocol_handling", {
  expect_identical(
    get_scheme("ftp://h/x", protocol_handling = "https",
               url_standard = "whatwg"),
    "ftp"
  )
  for (ph in c("keep", "none", "strip", "http", "https")) {
    expect_identical(
      get_scheme("HTTPS://example.com/", protocol_handling = ph),
      "https",
      info = ph
    )
  }
  # A scheme-less host parses as http (scheme inference), whatever the dial.
  expect_identical(get_scheme("x.com", protocol_handling = "none"), "http")
  expect_identical(get_scheme("x.com", protocol_handling = "strip"), "http")
  expect_identical(get_scheme("x.com", protocol_handling = "http"), "http")
})

test_that("protocol_handling = \"strip\" keeps the scheme and the file: host rule", {
  out <- safe_parse_urls(
    c("HTTP://EXAMPLE.COM/", "file://localhost/x"),
    url_standard = "whatwg", protocol_handling = "strip"
  )
  expect_identical(out$scheme, c("http", "file"))
  expect_identical(out$parse_status, c("ok", "ok"))
  # `localhost` is the empty host of a file: URL; the column shows it as NA.
  expect_identical(out$host, c("example.com", NA_character_))
  expect_identical(out$path, c("/", "/x"))
  # clean_url still drops the scheme, which is all the dial may do.
  expect_identical(out$clean_url[1], "example.com/")
})

test_that("an ftp: row keeps ok-ftp under protocol_handling = \"strip\"", {
  expect_identical(
    get_parse_status("ftp://example.com", protocol_handling = "strip"),
    "ok-ftp"
  )
  v <- get_parse_verdicts("ftp://example.com", protocol_handling = "strip")
  expect_identical(v$layer2_policy_verdict, "admitted-ftp")
})

test_that("a non-special query keeps its special-query bytes under \"https\"", {
  out <- safe_parse_urls(
    "foo:/a?b'c",
    scheme_acceptance = "general", protocol_handling = "https"
  )
  expect_identical(out$scheme, "foo")
  expect_identical(out$query, "b'c")
  expect_identical(out$clean_url, "foo:/a?b'c")
})

test_that("general acceptance folds an http host case under \"strip\"", {
  out <- safe_parse_urls(
    "HTTP://Example.com/a",
    scheme_acceptance = "general", protocol_handling = "strip"
  )
  expect_identical(out$scheme, "http")
  expect_identical(out$host, "example.com")
  expect_identical(out$clean_url, "example.com/a")
})

test_that("http/https presentation still applies to http and https rows", {
  x <- c("http://example.com/a", "https://example.com/a", "example.com/a",
         "ftp://example.com/a", "mailto://h.example:8080/p")
  https <- safe_parse_urls(x, protocol_handling = "https",
                           scheme_acceptance = "general")
  expect_identical(
    https$clean_url,
    c("https://example.com/a", "https://example.com/a",
      "https://example.com/a", "ftp://example.com/a",
      "mailto://h.example:8080/p")
  )
  expect_identical(https$scheme, c("http", "https", "http", "ftp", "mailto"))
  # The explicit `//` authority of a mailto: row is a real host, not masked.
  expect_identical(https$host[5], "h.example")
  http <- safe_parse_urls(x[1:2], protocol_handling = "http")
  expect_identical(http$clean_url,
                   c("http://example.com/a", "http://example.com/a"))
  expect_identical(http$scheme, c("http", "https"))
})

test_that("the seo bundle's clean_url is unchanged for http and https input", {
  x <- c("http://www.Example.com/a/", "HTTPS://example.com:443/index.html?q=1",
         "http://example.com:443/a", "http://example.com:80/a",
         "http://bücher.example/p/", "example.com/x", "//host.example/p")
  expect_identical(
    safe_parse_urls(x, profile = "seo")$clean_url,
    c("https://example.com/a", "https://example.com",
      "https://example.com:443/a", "https://example.com/a",
      "https://bücher.example/p", "https://example.com/x",
      "https://host.example/p")
  )
})

test_that("edge inputs keep the keep-row record under every protocol_handling", {
  x <- c("example.com", "//host.example/p", "HTTP://Example.com/a",
         "mailto://h.example:8080/p", NA, "", "ws://example.com/")
  for (sa in c("web", "general")) {
    for (srh in c("keep", "http", "https", "error")) {
      ref <- safe_parse_urls(x, protocol_handling = "keep",
                             scheme_relative_handling = srh,
                             scheme_acceptance = sa)
      ref$clean_url <- NULL
      for (ph in c("none", "strip", "http", "https")) {
        got <- safe_parse_urls(x, protocol_handling = ph,
                               scheme_relative_handling = srh,
                               scheme_acceptance = sa)
        got$clean_url <- NULL
        expect_identical(got, ref, info = paste(sa, srh, ph))
      }
    }
  }
  # ws: stays outside the default web allowlist.
  expect_identical(
    get_parse_status("ws://example.com/", protocol_handling = "https"),
    "error"
  )
})

# The invariant behind the ticket: over the fixture corpora, a non-default
# protocol_handling may move `clean_url` and nothing else. Re-introducing any
# read of the presented scheme in a classifier turns this red.
test_that("only clean_url moves with protocol_handling over the corpora", {
  skip_if_not_installed("jsonlite")
  ext_path <- testthat::test_path("fixtures", "external-url-vectors.csv")
  txt <- rawToChar(readBin(ext_path, "raw", file.size(ext_path)))
  Encoding(txt) <- "UTF-8"
  fx <- utils::read.csv(
    text = txt, stringsAsFactors = FALSE, colClasses = "character",
    na.strings = "NA", encoding = "UTF-8"
  )
  ext <- vapply(fx$input_json[fx$runnable == "yes"], jsonlite::fromJSON,
                character(1), USE.NAMES = FALSE)
  x <- unique(c(parse_corpus_urls(), ext))
  for (std in c("whatwg", "rfc3986")) {
    for (sa in c("web", "general")) {
      ref <- safe_parse_urls(x, url_standard = std, scheme_acceptance = sa,
                             protocol_handling = "keep")
      ref_v <- get_parse_verdicts(x, url_standard = std,
                                  scheme_acceptance = sa,
                                  protocol_handling = "keep")
      ref$clean_url <- NULL
      for (ph in c("none", "strip", "http", "https")) {
        got <- safe_parse_urls(x, url_standard = std, scheme_acceptance = sa,
                               protocol_handling = ph)
        got$clean_url <- NULL
        expect_identical(got, ref, info = paste(std, sa, ph))
        expect_identical(
          get_parse_verdicts(x, url_standard = std, scheme_acceptance = sa,
                             protocol_handling = ph),
          ref_v,
          info = paste(std, sa, ph)
        )
      }
    }
  }
})
