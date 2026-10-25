# RURL-pntbujzc, maintainer ruling 2026-10-25 (RUL-031): under
# url_standard = "rfc3986", host_encoding = "unicode" stays a display form. It
# presents the decoded host even though RFC 3986 writes a non-ASCII host only
# percent-encoded as UTF-8 (sections 2.5, 3.2.2), so that clean_url does not
# parse again under rfc3986; "idna" and "keep" do.

he_clean <- function(url, host_encoding) {
  get_clean_url(url, url_standard = "rfc3986", host_encoding = host_encoding)
}

he_status <- function(url) {
  safe_parse_urls(url, url_standard = "rfc3986")$parse_status
}

test_that("unicode presents the decoded host, which rfc3986 rejects on re-parse", {
  out <- he_clean("http://xn--bcher-kva.de/", "unicode")
  expect_identical(out, "http://bücher.de/")
  expect_identical(he_status(out), "error")
})

test_that("idna and keep give a clean_url that parses again", {
  for (enc in c("idna", "keep")) {
    out <- he_clean("http://xn--bcher-kva.de/", enc)
    expect_identical(out, "http://xn--bcher-kva.de/")
    expect_identical(he_status(out), "ok")
  }
})
