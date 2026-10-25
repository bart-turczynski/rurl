# RURL-nwujwmft: resolve_url() and a standard selected through `profile`.
#
# A named profile bundles a `url_standard` (`.URL_PROFILES`, R/parse.R:
# "browser", "whatwg", "seo"/"canonical" carry "whatwg", "rfc-syntax" carries
# "rfc3986"), and an explicit `url_standard` overrides the bundle's (the iron
# rule, `.resolve_profile()`). safe_parse_url() reads the standard that way;
# the resolution rules in resolve_url() must read the same one.
#
# This file opens with PINS: the `url_standard =` paths already follow the
# named standard, and they must not move when the profile path is fixed.

frag_bad <- "https://example.org/##link" # RFC 3986 section 3.5: one `#` only
ws_bad <- "http://exa mple.com/a" # U+0020 is a forbidden host code point

# The posture card's WHATWG recipe, spelled as knobs: what `profile =
# "whatwg"` equals (design/posture-card.md).
whatwg_knobs <- list(
  url_standard = "whatwg", scheme_policy = "require",
  scheme_acceptance = "general", scheme_relative_handling = "error"
)

wpt_rel_rows <- function() {
  skip_if_not_installed("jsonlite")
  rows <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "wpt-url-base-relative.json"),
    simplifyVector = FALSE
  )$success
  field <- function(k) vapply(rows, function(x) x[[k]], character(1))
  list(input = field("input"), base = field("base"), href = field("href"))
}

test_that("pin: url_standard follows the named standard on the two rows", {
  expect_identical(
    resolve_url("g", frag_bad, url_standard = "rfc3986"), NA_character_
  )
  expect_identical(
    resolve_url("https://z.com/q", ws_bad, url_standard = "whatwg"),
    NA_character_
  )
})

test_that("pin: an explicit url_standard wins over the profile's", {
  # Same value as the bundle: the explicit argument carries the standard.
  expect_identical(
    resolve_url("g", frag_bad, url_standard = "rfc3986",
                profile = "rfc-syntax"),
    NA_character_
  )
  expect_identical(
    resolve_url("https://z.com/q", ws_bad, url_standard = "whatwg",
                profile = "whatwg"),
    NA_character_
  )
  # A conflicting value: the explicit one is the standard (iron rule), as in
  # safe_parse_url(). WHATWG keeps `##link` as a fragment, so it is a base.
  expect_identical(
    resolve_url("g", frag_bad, url_standard = "rfc3986", profile = "whatwg"),
    NA_character_
  )
  expect_identical(
    resolve_url("g", frag_bad, url_standard = "whatwg",
                profile = "rfc-syntax"),
    "https://example.org/g"
  )
})

test_that("pin: WPT base-relative rows resolve under the WHATWG recipe", {
  # Resolving then cleaning gives what cleaning the standard's own `href`
  # gives. The three listed rows differ only in SPELLING: clean_url echoes
  # the resolved string's raw Unicode / ASCII bytes where `href` is
  # percent- or ACE-encoded (host_encoding = "keep", the path as written),
  # not in what was resolved.
  w <- wpt_rel_rows()
  got <- do.call(resolve_url, c(list(w$input, w$base), whatwg_knobs))
  want <- do.call(safe_parse_urls, c(list(w$href), whatwg_knobs))$clean_url
  differs <- !mapply(identical, got, want, USE.NAMES = FALSE)
  expect_setequal(
    w$href[differs],
    c(
      "http://www.example.com/%E4%B8%AD/test.txt",
      "http://xn--6qqa088eba/",
      "http://%60%7B%7D:%60%7B%7D@h/%60%7B%7D?`{}"
    )
  )
  # url_standard plus the bundle that carries it is the same call.
  expect_identical(
    resolve_url(w$input, w$base, url_standard = "whatwg", profile = "whatwg"),
    got
  )
})
