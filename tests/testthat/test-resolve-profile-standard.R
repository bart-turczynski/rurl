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

# ---- the rule (RURL-nwujwmft) ------------------------------------------------
#
# The resolution reads the standard the profile bundles, through the same
# `.resolve_profile()` the parse uses.

test_that("profile = 'rfc-syntax' refuses an RFC 3986 base with a second #", {
  # RFC 3986 section 3.5: `fragment = *( pchar / "/" / "?" )`, so `##link`
  # is no URI, and section 5.1 requires the base to be one.
  expect_identical(
    resolve_url("g", frag_bad, profile = "rfc-syntax"), NA_character_
  )
  # An absolute reference does not read the base (section 5.2.2).
  expect_identical(
    resolve_url("https://z.com/q", frag_bad, profile = "rfc-syntax"),
    "https://z.com/q"
  )
})

test_that("a whatwg-bundling profile fails every row on a failing base", {
  # The URL Standard's API URL parser parses the base first and returns
  # failure when that fails, before it reads the input.
  for (p in c("whatwg", "browser", "seo", "canonical")) {
    expect_identical(
      resolve_url("https://z.com/q", ws_bad, profile = p), NA_character_,
      label = p
    )
  }
  # WHATWG keeps `##link` as a fragment, so under it that base is a base.
  expect_identical(
    resolve_url("g", frag_bad, profile = "whatwg"), "https://example.org/g"
  )
})

test_that("a profile reaches the WHATWG reference-parsing rules", {
  # "special relative or authority state": the base's own special scheme is
  # consumed, so the reference is relative (P2.7 D-B).
  expect_identical(
    resolve_url("http:foo.com", "http://example.org/foo/bar",
                profile = "whatwg"),
    "http://example.org/foo/foo.com"
  )
})

test_that("profile and url_standard + profile agree on WPT base-relative", {
  w <- wpt_rel_rows()
  std <- c(
    whatwg = "whatwg", browser = "whatwg", seo = "whatwg",
    canonical = "whatwg", `rfc-syntax` = "rfc3986"
  )
  for (p in names(std)) {
    expect_identical(
      resolve_url(w$input, w$base, profile = p),
      resolve_url(w$input, w$base, url_standard = std[[p]], profile = p),
      label = p
    )
  }
  # ...and `profile = "whatwg"` is the posture card's recipe.
  expect_identical(
    resolve_url(w$input, w$base, profile = "whatwg"),
    do.call(resolve_url, c(list(w$input, w$base), whatwg_knobs))
  )
})

test_that("no profile and an invalid profile behave as before", {
  # No profile: the NULL selector, frozen (ADR 0007), resolves against the
  # base its parser calls an error.
  expect_identical(resolve_url("g", frag_bad), "https://example.org/g")
  expect_error(
    resolve_url("g", "http://a/b", profile = "nope"),
    "profile must be NULL or one of"
  )
})

test_that("an abbreviated profile argument selects the standard too", {
  # safe_parse_urls() matches `prof =` to `profile`, so the resolution must.
  expect_identical(
    resolve_url("g", "https://example.org/##link", prof = "rfc-syntax"),
    NA_character_
  )
})

test_that("a whatwg-bundling profile refuses a scheme-less base", {
  # The URL Standard's API URL parser fails on a base that does not parse,
  # before it reads the input, even an absolute one; the browser profile's
  # own parse fixing the base up does not make it a base.
  for (p in c("browser", "seo", "canonical", "whatwg")) {
    expect_identical(
      resolve_url("https://x.com/y", "example.com/a/b", profile = p),
      NA_character_, label = p
    )
  }
  expect_identical(resolve_url("https://x.com/y", "example.com/a/b"),
                   "https://x.com/y")
})

# RURL-frwyfdhp: the url_standard conflict check is skipped on the profile
# path, as in safe_parse_url() and canonical_join(). A profile authorizes its
# own knob combination, and an explicit knob overrides it.
test_that("pin: without a profile a governed knob still conflicts", {
  for (std in c("rfc3986", "whatwg")) {
    expect_error(
      resolve_url("b", "https://example.org/a/", url_standard = std,
                  path_normalization = "none"),
      "governs `path_normalization`", info = std
    )
  }
})

test_that("a profile plus a governed knob resolves as safe_parse_url() parses", {
  cases <- list(
    list(std = "rfc3986", profile = "rfc-syntax"),
    list(std = "whatwg", profile = "whatwg")
  )
  for (cs in cases) {
    expect_identical(
      resolve_url("b", "https://example.org/a/", url_standard = cs$std,
                  profile = cs$profile, path_normalization = "none"),
      safe_parse_url("https://example.org/a/b", url_standard = cs$std,
                     profile = cs$profile,
                     path_normalization = "none")$clean_url,
      info = cs$profile
    )
  }
  expect_identical(
    resolve_url("b", "https://example.org/a/", url_standard = "rfc3986",
                prof = "rfc-syntax", path_normalization = "none"),
    "https://example.org/a/b"
  )
})

test_that("with a profile an explicit governed knob overrides the profile's", {
  cases <- list(
    list(std = "rfc3986", profile = "rfc-syntax"),
    list(std = "whatwg", profile = "whatwg")
  )
  for (cs in cases) {
    for (knob in list(list(path_normalization = "collapse_slashes"),
                      list(case_handling = "lower"))) {
      got <- do.call(resolve_url, c(
        list("a//B", "https://e.org/", url_standard = cs$std,
             profile = cs$profile),
        knob
      ))
      want <- do.call(safe_parse_url, c(
        list("https://e.org/a//B", url_standard = cs$std,
             profile = cs$profile),
        knob
      ))$clean_url
      expect_identical(got, want, info = paste(cs$profile, names(knob)))
      expect_false(
        identical(got, resolve_url("a//B", "https://e.org/",
                                   url_standard = cs$std,
                                   profile = cs$profile)),
        info = paste(cs$profile, names(knob))
      )
    }
  }
})
