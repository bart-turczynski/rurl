# `path_encoding = "decode"` and reserved percent-triplets in the path
# (RURL-eokizmdi). RFC 3986 section 2.2: URIs that differ by replacing a
# reserved character with its percent-encoded octet are not equivalent, so
# decoding `%2F` to `/` names a different path. The dial reaches the `path`
# column of `safe_parse_urls()` and `get_path()`, not only `clean_url`, so
# every pin below runs on all three surfaces under every `url_standard`.

standards <- list(`NULL` = NULL, rfc3986 = "rfc3986", whatwg = "whatwg")

# The path of `url` on the three surfaces under `std`, with "decode".
decode_surfaces <- function(url, std) {
  rurl_clear_caches()
  list(
    path = safe_parse_urls(url, path_encoding = "decode",
                           url_standard = std)$path,
    get_path = get_path(url, path_encoding = "decode", url_standard = std),
    clean_url = get_clean_url(url, path_encoding = "decode",
                              url_standard = std)
  )
}

expect_decoded_path <- function(url, want_path, host = "http://h.de") {
  for (nm in names(standards)) {
    got <- decode_surfaces(url, standards[[nm]])
    expect_identical(got$path, want_path, info = paste(nm, "path", url))
    expect_identical(got$get_path, want_path, info = paste(nm, "get_path", url))
    expect_identical(got$clean_url, paste0(host, want_path),
                     info = paste(nm, "clean_url", url))
  }
}

test_that("decode keeps reserved and %25 triplets, decodes the rest", {
  # `%2F`, `%3F`, `%23` and `%25` stay; `%41` (unreserved) and the UTF-8 run
  # `%C3%A4` decode. It used to give "/a/b/c?d#e%fAä".
  expect_decoded_path(
    "http://h.de/a%2Fb/c%3Fd%23e%25f%41%C3%A4",
    "/a%2Fb/c%3Fd%23e%25fAä"
  )
})

test_that("decode never makes a new triplet out of %25", {
  # It used to give "/a%41", a different triplet the input did not hold.
  expect_decoded_path("http://h.de/a%2541", "/a%2541")
})

test_that("decode decodes unreserved and UTF-8 triplets as before", {
  expect_decoded_path("http://h.de/%7Ex", "/~x")
  expect_decoded_path("http://h.de/%C3%A4", "/ä")
  expect_decoded_path("http://h.de/%2D%2E%5F%30%7a", "/-._0z")
})

test_that("decode keeps a non-UTF-8 octet as its uppercase triplet", {
  expect_decoded_path("http://h.de/a%E9b", "/a%E9b")
  expect_decoded_path("http://h.de/a%C3%A4%E9", "/aä%E9")
  expect_decoded_path("http://h.de/%C3(", "/%C3(")
})

test_that("decode keeps every ASCII triplet outside unreserved", {
  reserved <- c("21", "23", "24", "26", "27", "28", "29", "2A", "2B", "2C",
                "2F", "3A", "3B", "3D", "3F", "40", "5B", "5D")
  others <- c("00", "09", "0A", "1F", "20", "22", "25", "3C", "3E", "5C",
              "5E", "60", "7B", "7C", "7D", "7F")
  for (hex in c(reserved, others)) {
    expect_decoded_path(paste0("http://h.de/a%", hex, "b"),
                        paste0("/a%", hex, "b"))
  }
})

test_that("a kept triplet keeps the hex case the standard left it", {
  # No new case rule: rfc3986 uppercases every kept triplet (section 6.2.2.1),
  # whatwg preserves the spelling, and NULL under "decode" does too.
  want <- list(`NULL` = "/a%2fb%3a", rfc3986 = "/a%2Fb%3A",
               whatwg = "/a%2fb%3a")
  for (nm in names(standards)) {
    got <- decode_surfaces("http://h.de/a%2fb%3a", standards[[nm]])
    expect_identical(got$path, want[[nm]], info = nm)
    expect_identical(got$get_path, want[[nm]], info = nm)
    expect_identical(got$clean_url, paste0("http://h.de", want[[nm]]),
                     info = nm)
  }
})
