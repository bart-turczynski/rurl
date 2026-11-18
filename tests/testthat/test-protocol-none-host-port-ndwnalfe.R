# RURL-ndwnalfe: under `protocol_handling = "none"` a scheme-less input is
# presented without a scheme. A scheme-less `host:port` input
# (`example.com:8080` reads as a scheme token, since the scheme grammar admits a
# dot) parses as http, as every other scheme-less host does, and must present
# scheme-less too.
# The `scheme` column keeps the inferred "http" (RURL-uxkksgtu).

host_port_inputs <- c("example.com:8080/a", "example.com:8080",
                      "localhost:3000", "EXAMPLE.com:80/")

test_that("a scheme-less host:port input gains no scheme under \"none\"", {
  expected <- list(
    whatwg = list(
      keep = c("example.com:8080/a", "example.com:8080/", "localhost:3000/",
               "example.com:80/"),
      exclude = c("example.com/a", "example.com/", "localhost/",
                  "example.com/")
    ),
    rfc3986 = list(
      keep = c("example.com:8080/a", "example.com:8080", "localhost:3000",
               "example.com:80/"),
      exclude = c("example.com/a", "example.com", "localhost", "example.com/")
    )
  )
  for (std in c("whatwg", "rfc3986")) {
    for (pt in c("keep", "exclude")) {
      none <- safe_parse_urls(host_port_inputs, url_standard = std,
                              protocol_handling = "none", port_handling = pt)
      keep <- safe_parse_urls(host_port_inputs, url_standard = std,
                              protocol_handling = "keep", port_handling = pt)
      info <- paste(std, pt)
      expect_identical(none$clean_url, expected[[std]][[pt]], info = info)
      # Exactly the inferred `http://` comes off the keep presentation.
      expect_identical(paste0("http://", none$clean_url), keep$clean_url,
                       info = info)
      # The record is the inferred parse, as before.
      expect_identical(none$scheme, rep("http", 4L), info = info)
      expect_identical(none$parse_status, keep$parse_status, info = info)
    }
  }
})

test_that("the ticket's two rows both present scheme-less", {
  expect_identical(
    safe_parse_urls(c("example.com:8080/a", "example.com/a"),
                    protocol_handling = "none")$clean_url,
    c("example.com/a", "example.com/a")
  )
  expect_identical(
    safe_parse_urls(c("example.com:8080/a", "example.com/a"),
                    protocol_handling = "none",
                    port_handling = "keep")$clean_url,
    c("example.com:8080/a", "example.com/a")
  )
})

test_that("the scalar engine agrees with the vector engine under \"none\"", {
  for (std in c("whatwg", "rfc3986")) {
    vec <- safe_parse_urls(host_port_inputs, url_standard = std,
                           protocol_handling = "none")$clean_url
    sca <- vapply(host_port_inputs, function(u) {
      rurl:::._safe_parse_url_impl(
        url = u, protocol_handling = "none", www_handling = "none",
        tld_source = "all", case_handling = "keep",
        trailing_slash_handling = "none", index_page_handling = "keep",
        path_normalization = "none", scheme_relative_handling = "keep",
        subdomain_levels_to_keep = NULL, url_standard = std
      )$clean_url
    }, character(1), USE.NAMES = FALSE)
    expect_false(any(startsWith(sca, "http:")), info = std)
    expect_identical(safe_parse_url(host_port_inputs[1], url_standard = std,
                                    protocol_handling = "none")$clean_url,
                     vec[1], info = std)
  }
})

test_that("inputs that spell a scheme keep it under \"none\"", {
  x <- c("http://example.com:8080/a", "https://example.com:8443/a",
         "ftp://h:21/")
  for (std in c("whatwg", "rfc3986")) {
    for (pt in c("keep", "exclude")) {
      none <- safe_parse_urls(x, url_standard = std, port_handling = pt,
                              protocol_handling = "none")
      keep <- safe_parse_urls(x, url_standard = std, port_handling = pt,
                              protocol_handling = "keep")
      expect_identical(none$clean_url, keep$clean_url, info = paste(std, pt))
      # A scheme-less host without a port was already presented scheme-less.
      bare <- safe_parse_urls("example.com/a", url_standard = std,
                              port_handling = pt, protocol_handling = "none")
      expect_identical(bare$clean_url, "example.com/a", info = paste(std, pt))
    }
  }
  # Under RFC 3986 general acceptance `example.com:` IS the scheme (section
  # 3.1): the row is scheme-bearing and keeps it.
  expect_identical(
    safe_parse_urls(host_port_inputs[1:2], url_standard = "rfc3986",
                    scheme_acceptance = "general",
                    protocol_handling = "none")$clean_url,
    c("example.com:8080/a", "example.com:8080")
  )
  # An opaque general-acceptance row is not a host:port and keeps its scheme.
  expect_identical(
    safe_parse_urls("urn:a:1", scheme_acceptance = "general",
                    protocol_handling = "none")$clean_url,
    "urn:a:1"
  )
})

# Over the fixture corpora, a row whose input spells its parsed scheme presents
# exactly its keep form under "none", and no other row presents a scheme.
test_that("\"none\" presents a scheme only where the input spells one", {
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
  x <- unique(c(parse_corpus_urls(), ext, host_port_inputs))
  # The WHATWG basic URL parser drops ASCII tab/newline anywhere and C0 or
  # space at either end before it reads the scheme.
  bare <- gsub("[\t\n\r]", "", x, useBytes = TRUE)
  bare <- sub("^[\001-\040]+", "", bare, useBytes = TRUE)
  for (std in c("whatwg", "rfc3986")) {
    for (sa in c("web", "general")) {
      keep <- safe_parse_urls(x, url_standard = std, scheme_acceptance = sa)
      none <- safe_parse_urls(x, url_standard = std, scheme_acceptance = sa,
                              protocol_handling = "none")
      pre <- paste0(keep$scheme, ":")
      spelled <- !is.na(keep$scheme) & !is.na(bare) &
        rurl:::.ascii_tolower(substring(bare, 1L, nchar(pre))) == pre
      info <- paste(std, sa)
      expect_identical(none$clean_url[spelled], keep$clean_url[spelled],
                       info = info)
      web_present <- !is.na(none$clean_url) &
        grepl("^https?://", none$clean_url)
      expect_false(any(web_present & !spelled), info = info)
    }
  }
})
