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

test_that("decode folds reserved triplets (pre-fix pin)", {
  expect_decoded_path(
    "http://h.de/a%2Fb/c%3Fd%23e%25f%41%C3%A4",
    "/a/b/c?d#e%fAä"
  )
  expect_decoded_path("http://h.de/a%2541", "/a%41")
})
