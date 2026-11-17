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

test_that("unicode presents a decoded host that rfc3986 rejects on re-parse", {
  out <- he_clean("http://xn--bcher-kva.de/", "unicode")
  expect_identical(out, "http://b\u00fccher.de/")
  expect_identical(he_status(out), "error")
})

test_that("idna and keep give a clean_url that parses again", {
  for (enc in c("idna", "keep")) {
    out <- he_clean("http://xn--bcher-kva.de/", enc)
    expect_identical(out, "http://xn--bcher-kva.de/")
    expect_identical(he_status(out), "ok")
  }
})

# RURL-rhwcrezl. A reserved character and its triplet name different URIs (RFC
# 3986 section 2.2), and only an unreserved triplet may decode (section
# 6.2.2.2). So the IDNA presentation decodes only what IDNA needs as code
# points, a run of non-ASCII UTF-8 triplets; a reserved, `%25` or control
# triplet stays as written, and such a host keeps the "keep" spelling. It used
# to decode every triplet: `a%2Fb` became host `a`, path `/b/`.
he_host <- function(url, host_encoding) {
  get_host(url, url_standard = "rfc3986", host_encoding = host_encoding)
}

test_that("idna keeps reserved, %25 and control host triplets encoded", {
  urls <- c("http://%25/", "http://hello%00/", "http://a%2Fb/")
  keep <- c("%25", "hello%00", "a%2Fb")
  expect_identical(he_host(urls, "keep"), keep)
  expect_identical(he_host(urls, "idna"), keep)
  out <- he_clean(urls, "idna")
  expect_identical(out, urls)
  # The clean_url parses again, to the same host.
  expect_identical(he_host(out, "keep"), keep)
  expect_identical(get_path(out, url_standard = "rfc3986"), rep("/", 3L))
})

test_that("a host that keeps a triplet keeps its source spelling", {
  # Lowercase hex (uppercased by the parse), a non-ASCII run beside a reserved
  # triplet, a run that is not well-formed UTF-8, and a lone control.
  urls <- c(
    "http://a%2fb/", "http://b%C3%BCcher%2F.de/", "http://caf%C3%28.com/",
    "http://%00/"
  )
  keep <- he_host(urls, "keep")
  expect_identical(
    keep, c("a%2Fb", "b%C3%BCcher%2F.de", "caf%C3%28.com", "%00")
  )
  expect_identical(he_host(urls, "idna"), keep)
  expect_identical(he_clean(urls, "idna"), paste0("http://", keep, "/"))
})

test_that("idna still renders non-ASCII host triplets", {
  # The `%61` row is a control for the whole pipeline: the rfc3986 parse has
  # already decoded that unreserved triplet, so it never reaches the
  # presentation decode.
  urls <- c("https://a%C2%ADb/", "http://b%C3%BCcher.de/", "http://%61b.com/")
  expect_identical(
    he_host(urls, "idna"), c("xn--ab-5da", "xn--bcher-kva.de", "ab.com")
  )
  # IP literals, a missing host and NA are not eligible and pass through.
  edge <- c("http://[::1]/", "http://1.2.3.4/", "http:///p", NA)
  expect_identical(he_host(edge, "idna"), he_host(edge, "keep"))
})

test_that("unicode keeps a reserved or control triplet encoded too", {
  urls <- c("http://%25/", "http://hello%00/", "http://a%2Fb/")
  expect_identical(he_host(urls, "unicode"), c("%25", "hello%00", "a%2Fb"))
  expect_identical(
    he_host("http://b%C3%BCcher.de/", "unicode"), "b\u00fccher.de"
  )
})

test_that("a decoded label that already reads as an A-label is held", {
  # Decoding `xn--zz%C3%BC` gives a label with the ACE prefix and a non-ASCII
  # code point: it is no A-label, and IDNA would prefix it a second time
  # ("xn--xn--zz-8ya"). Both presentations keep the source spelling.
  urls <- c("http://xn--zz%C3%BC.com/", "http://XN--zz%C3%BC.com/")
  keep <- he_host(urls, "keep")
  expect_identical(keep, c("xn--zz%C3%BC.com", "xn--zz%C3%BC.com"))
  expect_identical(he_host(urls, "idna"), keep)
  expect_identical(he_host(urls, "unicode"), keep)
  # An A-label beside a separately encoded label still renders.
  expect_identical(
    he_host("http://xn--bcher-kva.b%C3%BC.de/", "idna"),
    "xn--bcher-kva.xn--b-eha.de"
  )
})

test_that("a host whose IDNA encode fails keeps its source spelling", {
  # `xn--zz` is not a valid A-label, so the encode returns NA; the host falls
  # back to the source spelling, not to the decoded `b\u00fccher.xn--zz`,
  # which rfc3986 rejects on re-parse (section 2.5).
  expect_identical(
    he_host("http://b%C3%BCcher.xn--zz/", "idna"), "b%C3%BCcher.xn--zz"
  )
})
