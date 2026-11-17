# get_query() with the default decode = TRUE and reserved percent-triplets in
# the query (RURL-utkkuuaz). RFC 3986 section 2.2 and the WHATWG URL Standard's
# query state: `%26` is data, `&` a separator in application/x-www-form-
# urlencoded, so decoding a reserved triplet names a different query
# (`a=%26b&c=1` has two parameters; `a=&b&c=1` reads as three). decode = TRUE
# keeps every reserved triplet as written and decodes the unreserved and
# well-formed UTF-8 ones, the RURL-eokizmdi rule. The default path, the
# filtering path and both standards follow that one rule.

standards <- c("whatwg", "rfc3986")

test_that("decode keeps reserved triplets in the query (the audit call)", {
  for (std in standards) {
    expect_identical(
      get_query(c("http://h/?a=%26b&c=1", "http://h/?k=%3D%23"),
                url_standard = std),
      c("a=%26b&c=1", "k=%3D%23"),
      info = std
    )
  }
})

test_that("decode keeps reserved triplets on the filtering path", {
  expect_identical(
    get_query("http://h/?a=%26b&c=1", query_handling = "filter"),
    "a=%26b&c=1"
  )
  expect_identical(
    get_query("http://h/?b=%26&a=1", sort_params = TRUE),
    "a=1&b=%26"
  )
  # decode_plus still turns '+' into a space; %2B stays encoded.
  expect_identical(
    get_query("http://h/?a=x+y%2Bz", query_handling = "filter",
              decode_plus = TRUE),
    "a=x y%2Bz"
  )
})

test_that("default and filtering paths agree on a reserved triplet", {
  u <- "http://h/?a=%26b&c=%3D"
  expect_identical(
    get_query(u),
    get_query(u, query_handling = "filter", sort_params = TRUE)
  )
  expect_identical(get_query(u), "a=%26b&c=%3D")
})

test_that("decode still decodes unreserved and UTF-8 triplets", {
  expect_identical(
    get_query("http://h/?a=caf%C3%A9", url_standard = "whatwg"), "a=café"
  )
  expect_identical(
    get_query("http://h/?a=%41%7E%2D", url_standard = "whatwg"), "a=A~-"
  )
  expect_identical(
    get_query("http://h/?b=%41&a=%C3%A9", sort_params = TRUE), "a=é&b=A"
  )
  # format = "list" splits before it decodes, so the structure is unchanged.
  expect_identical(
    get_query("http://example.com/?a=1%262", format = "list")[[1]],
    list(a = "1&2")
  )
})

test_that("decode keeps NUL, CRLF, percent and non-UTF-8 triplets as written", {
  for (std in standards) {
    expect_identical(
      get_query("http://h/?a=%0D%0A", url_standard = std), "a=%0D%0A"
    )
    expect_identical(get_query("http://h/?a=%00", url_standard = std), "a=%00")
    expect_identical(
      get_query("http://h/?a=%2541", url_standard = std), "a=%2541"
    )
    expect_identical(get_query("http://h/?a=%E9", url_standard = std), "a=%E9")
  }
  # Lowercase hex on a reserved triplet keeps its case under decode = TRUE.
  expect_identical(
    get_query("http://h/?a=%2f", url_standard = "whatwg"), "a=%2f"
  )
})

test_that("decode leaves malformed and empty queries as they are", {
  expect_identical(
    get_query("http://h/?a=%zz", url_standard = "whatwg"), "a=%zz"
  )
  expect_identical(
    get_query("http://h/?a=50%", url_standard = "whatwg"), "a=50%"
  )
  expect_identical(get_query("http://h/?"), "")
  expect_true(is.na(get_query("http://h/nopath")))
})
