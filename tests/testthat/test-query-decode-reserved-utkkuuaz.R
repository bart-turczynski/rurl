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

test_that("decode leaves malformed queries as they are", {
  expect_identical(
    get_query("http://h/?a=%zz", url_standard = "whatwg"), "a=%zz"
  )
  expect_identical(
    get_query("http://h/?a=50%", url_standard = "whatwg"), "a=50%"
  )
})

test_that("an absent or empty query stays NA, as before", {
  # Unchanged by the decode rule: main reads `?` alone as no query.
  expect_true(is.na(get_query("http://h/nopath")))
  expect_true(is.na(get_query("http://h/?")))
  expect_true(is.na(get_query(NA_character_)))
})

test_that("an opaque token renders like any other on every path", {
  # %00 and a malformed % are opaque to the matcher, but the readable rule
  # decodes %41 and leaves them as written on the default and sort paths.
  expect_identical(get_query("http://h/?a=%00%41&b=1"), "a=%00A&b=1")
  expect_identical(
    get_query("http://h/?a=%00%41&b=1", sort_params = TRUE), "a=%00A&b=1"
  )
  expect_identical(get_query("http://h/?a=%zz%41"), "a=%zzA")
  expect_identical(
    get_query("http://h/?a=%zz%41", sort_params = TRUE), "a=%zzA"
  )
})

test_that("the string form keeps every ASCII triplet outside unreserved", {
  expect_identical(
    get_query("http://h/?a=%20%22%27%7C%7F%0D%0A%00"),
    "a=%20%22%27%7C%7F%0D%0A%00"
  )
  # Non-ASCII UTF-8 still decodes, U+2028 among it.
  expect_identical(get_query("http://h/?a=%E2%80%A8"), "a= ")
})

test_that("format = 'list' still decodes every triplet but %00", {
  expect_identical(
    get_query("http://h/?a=%0D%0A", format = "list")[[1]], list(a = "\r\n")
  )
  expect_identical(
    get_query("http://h/?a=%00", format = "list")[[1]], list(a = "%00")
  )
  expect_identical(
    get_query("http://h/?a=1%262", format = "list")[[1]], list(a = "1&2")
  )
})

test_that("decode_plus turns '+' into a space and keeps %20", {
  expect_identical(
    get_query("http://h/?a=x+y%20z", query_handling = "filter",
              decode_plus = TRUE),
    "a=x y%20z"
  )
})

test_that("a literal apostrophe shows the standard's spelling", {
  # whatwg encodes ' in an http query; rfc3986 keeps the sub-delimiter raw.
  expect_identical(
    get_query("http://h/?a=it's", url_standard = "whatwg"), "a=it%27s"
  )
  expect_identical(
    get_query("http://h/?a=it's", url_standard = "rfc3986"), "a=it's"
  )
})
