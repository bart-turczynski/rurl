# Exported names are US English; a British spelling is accepted as an alias
# (SEOR-qwomlgjd, SEOR-oytkybis).
#
# The first block pins the `path_normalization` behavior of the four
# functions that take it, by name and by position, so a signature change
# cannot silently shift an existing call.

.pn_url <- "http://example.com//a/./b/../c"

# Each mode's expected path and clean URL for `.pn_url`.
.pn_expected <- list(
  none = list(path = "//a/./b/../c", clean = "http://example.com//a/./b/../c"),
  collapse_slashes = list(
    path = "/a/./b/../c", clean = "http://example.com/a/./b/../c"
  ),
  dot_segments = list(path = "//a/c", clean = "http://example.com//a/c"),
  both = list(path = "/a/c", clean = "http://example.com/a/c")
)

# The formals as they stood before the alias was added. A new formal may only
# be appended after these, never inserted among them.
.pn_formals_before <- list(
  get_clean_url = c(
    "url", "protocol_handling", "www_handling", "source", "case_handling",
    "trailing_slash_handling", "index_page_handling", "path_normalization",
    "scheme_relative_handling", "subdomain_levels_to_keep", "host_encoding",
    "path_encoding", "query_handling", "params_keep", "params_drop",
    "params_case_sensitive", "sort_params", "empty_param_handling",
    "decode_plus", "port_handling", "scheme_policy", "scheme_acceptance",
    "url_standard", "engine", "profile", "credential_handling"
  ),
  get_path = c(
    "url", "protocol_handling", "case_handling", "trailing_slash_handling",
    "index_page_handling", "path_normalization", "path_encoding",
    "scheme_policy", "scheme_acceptance", "url_standard"
  ),
  safe_parse_url = c(
    "url", "protocol_handling", "www_handling", "tld_source", "case_handling",
    "trailing_slash_handling", "index_page_handling", "path_normalization",
    "scheme_relative_handling", "subdomain_levels_to_keep", "host_encoding",
    "path_encoding", "query_handling", "params_keep", "params_drop",
    "sort_params", "empty_param_handling", "params_case_sensitive",
    "decode_plus", "port_handling", "scheme_policy", "scheme_acceptance",
    "url_standard", "engine", "profile", "credential_handling"
  )
)
.pn_formals_before$safe_parse_urls <- .pn_formals_before$safe_parse_url

# Calls `fn` positionally with every pre-alias formal after `url` at its
# default, except those overridden in `set` (a named list).
.pn_call_positional <- function(fn_name, url, set = list()) {
  before <- .pn_formals_before[[fn_name]][-1]
  fn <- get(fn_name, envir = asNamespace("rurl"))
  args <- lapply(before, function(nm) eval(formals(fn)[[nm]]))
  names(args) <- before
  args[names(set)] <- set
  do.call(fn, c(list(url), unname(args)))
}

test_that("pin: the pre-alias formals keep their positions", {
  for (fn_name in names(.pn_formals_before)) {
    before <- .pn_formals_before[[fn_name]]
    now <- names(formals(get(fn_name, envir = asNamespace("rurl"))))
    expect_identical(now[seq_along(before)], before, label = fn_name)
  }
})

test_that("pin: path_normalization by name", {
  for (mode in names(.pn_expected)) {
    want <- .pn_expected[[mode]]
    expect_identical(
      get_clean_url(.pn_url, path_normalization = mode), want$clean
    )
    expect_identical(get_path(.pn_url, path_normalization = mode), want$path)
    expect_identical(
      safe_parse_url(.pn_url, path_normalization = mode)$path, want$path
    )
    expect_identical(
      safe_parse_urls(.pn_url, path_normalization = mode)$path, want$path
    )
  }
})

test_that("pin: path_normalization by position", {
  # get_clean_url(), safe_parse_url() and safe_parse_urls() take it eighth,
  # get_path() sixth.
  expect_identical(
    get_clean_url(
      .pn_url, "keep", "none", "all", "lower_host", "none", "keep", "both"
    ),
    .pn_expected$both$clean
  )
  expect_identical(
    get_path(.pn_url, "keep", "lower_host", "none", "keep", "dot_segments"),
    .pn_expected$dot_segments$path
  )
  expect_identical(
    safe_parse_url(
      .pn_url, "keep", "none", "all", "lower_host", "none", "keep",
      "collapse_slashes"
    )$path,
    .pn_expected$collapse_slashes$path
  )
  expect_identical(
    safe_parse_urls(
      .pn_url, "keep", "none", "all", "lower_host", "none", "keep", "both"
    )$path,
    .pn_expected$both$path
  )
})

test_that("pin: a fully positional call reaches the last pre-alias formal", {
  cred <- "http://u:pw@example.com//a/./b/../c"
  # credential_handling is the last formal of three of the four.
  expect_identical(
    .pn_call_positional("get_clean_url", cred, list(
      path_normalization = "both", credential_handling = "reject"
    )),
    NA_character_
  )
  expect_identical(
    .pn_call_positional("get_clean_url", cred, list(
      path_normalization = "both", credential_handling = "strip"
    )),
    .pn_expected$both$clean
  )
  for (fn_name in c("safe_parse_url", "safe_parse_urls")) {
    rejected <- .pn_call_positional(fn_name, cred, list(
      path_normalization = "both", credential_handling = "reject"
    ))
    expect_identical(rejected$clean_url, NA_character_, label = fn_name)
    kept <- .pn_call_positional(fn_name, cred, list(
      path_normalization = "both", credential_handling = "strip"
    ))
    expect_identical(kept$path, .pn_expected$both$path, label = fn_name)
    expect_identical(kept$clean_url, .pn_expected$both$clean, label = fn_name)
  }
  # get_path() ends in url_standard: a positional "whatwg" there must still
  # reach it, so a conflicting positional path_normalization errors.
  expect_identical(
    .pn_call_positional("get_path", .pn_url, list(
      path_normalization = "dot_segments", url_standard = "whatwg"
    )),
    .pn_expected$dot_segments$path
  )
  expect_error(
    .pn_call_positional("get_path", .pn_url, list(
      path_normalization = "both", url_standard = "whatwg"
    )),
    "governs `path_normalization`"
  )
})

# --- serialise_url() --------------------------------------------------------

test_that("serialise_url() is serialize_url()", {
  expect_identical(serialise_url, serialize_url)
  expect_true("serialise_url" %in% getNamespaceExports("rurl"))
})

# --- path_normalisation -----------------------------------------------------

# Calls `fn_name` with `url` and the extra named arguments in `args`.
.pn_call <- function(fn_name, url, args) {
  do.call(get(fn_name, envir = asNamespace("rurl")), c(list(url), args))
}

.pn_functions <- names(.pn_formals_before)

test_that("path_normalisation is the last formal, defaulting to NULL", {
  for (fn_name in .pn_functions) {
    fml <- formals(get(fn_name, envir = asNamespace("rurl")))
    expect_named(fml, c(
      .pn_formals_before[[fn_name]], "path_normalisation"
    ), label = fn_name)
    expect_null(fml$path_normalisation, label = fn_name)
  }
})

test_that("path_normalisation alone equals path_normalization alone", {
  for (fn_name in .pn_functions) {
    for (mode in names(.pn_expected)) {
      expect_identical(
        .pn_call(fn_name, .pn_url, list(path_normalisation = mode)),
        .pn_call(fn_name, .pn_url, list(path_normalization = mode)),
        label = paste(fn_name, mode)
      )
    }
  }
  # And it takes effect: a non-default mode changes the path.
  expect_identical(
    get_path(.pn_url, path_normalisation = "both"), .pn_expected$both$path
  )
})

test_that("an abbreviated alias value resolves like the US spelling", {
  for (fn_name in .pn_functions) {
    expect_identical(
      .pn_call(fn_name, .pn_url, list(path_normalisation = "dot")),
      .pn_call(fn_name, .pn_url, list(path_normalization = "dot")),
      label = fn_name
    )
  }
})

test_that("supplying both spellings is an error naming both", {
  for (fn_name in .pn_functions) {
    for (pair in list(c("both", "both"), c("none", "dot_segments"))) {
      expect_error(
        .pn_call(fn_name, .pn_url, list(
          path_normalization = pair[[1]], path_normalisation = pair[[2]]
        )),
        "`path_normalization`.*`path_normalisation`",
        label = paste(fn_name, toString(pair))
      )
    }
  }
})

test_that("the alias counts as supplied in the url_standard conflict check", {
  # "none" conflicts with whatwg (which resolves dot segments); the US
  # spelling errors, so the alias must too rather than slip past as missing.
  for (fn_name in .pn_functions) {
    expect_error(
      .pn_call(fn_name, .pn_url, list(
        path_normalization = "none", url_standard = "whatwg"
      )),
      "governs `path_normalization`",
      label = paste(fn_name, "US")
    )
    expect_error(
      .pn_call(fn_name, .pn_url, list(
        path_normalisation = "none", url_standard = "whatwg"
      )),
      "governs `path_normalization`",
      label = paste(fn_name, "alias")
    )
    expect_identical(
      .pn_call(fn_name, .pn_url, list(
        path_normalisation = "dot_segments", url_standard = "whatwg"
      )),
      .pn_call(fn_name, .pn_url, list(
        path_normalization = "dot_segments", url_standard = "whatwg"
      )),
      label = paste(fn_name, "compatible")
    )
  }
})

test_that("the alias overrides a profile like the US spelling", {
  # rfc-syntax preserves dot segments; an explicit knob overrides it.
  for (fn_name in c("get_clean_url", "safe_parse_url", "safe_parse_urls")) {
    via_alias <- .pn_call(fn_name, .pn_url, list(
      profile = "rfc-syntax", path_normalisation = "both"
    ))
    expect_identical(
      via_alias,
      .pn_call(fn_name, .pn_url, list(
        profile = "rfc-syntax", path_normalization = "both"
      )),
      label = fn_name
    )
    expect_false(identical(
      via_alias,
      .pn_call(fn_name, .pn_url, list(profile = "rfc-syntax"))
    ), label = fn_name)
  }
})

test_that("url_profile() accepts the alias as a knob override", {
  # rfc-syntax is the profile that lets an explicit path_normalization through.
  via_alias <- url_profile("rfc-syntax", path_normalisation = "both")
  expect_identical(
    via_alias, url_profile("rfc-syntax", path_normalization = "both")
  )
  expect_identical(via_alias$path_normalization, "both")
  expect_true(via_alias$customized)
  expect_error(
    url_profile(
      "seo", path_normalization = "both", path_normalisation = "both"
    ),
    "`path_normalization`.*`path_normalisation`"
  )
})

test_that("canonical_join() forwards the alias and warns on it", {
  a <- data.frame(URL = "http://example.com//a/./b/../c")
  b <- data.frame(URL = "http://example.com/a/c")
  us <- cj_legacy(canonical_join(a, b, path_normalization = "both"))
  expect_identical(nrow(us), 1L)
  # The legacy-dial warning (P3.1 D-E.1) names the alias it received.
  expect_warning(
    via_alias <- cj_deprecated(
      canonical_join(a, b, path_normalisation = "both")
    ),
    "`path_normalisation`",
    class = "rurl_legacy_join_dial_warning"
  )
  expect_identical(via_alias, us)
  expect_error(
    cj_deprecated(canonical_join(
      a, b, path_normalization = "both", path_normalisation = "both"
    )),
    "`path_normalization`.*`path_normalisation`"
  )
  # The `...` seam's url_standard conflict check sees the alias too, so the
  # call fails there, before the legacy-dial warning.
  expect_error(
    cj_deprecated(withCallingHandlers(
      canonical_join(
        a, b, url_standard = "whatwg", path_normalisation = "none"
      ),
      rurl_legacy_join_dial_warning = function(w) {
        stop("warned before the conflict check")
      }
    )),
    "governs `path_normalization`"
  )
  expect_error(
    rurl:::.check_url_standard_conflicts_dots(
      list(url_standard = "whatwg", path_normalisation = "none")
    ),
    "governs `path_normalization`"
  )
})

test_that("resolve_url() forwards the alias", {
  expect_identical(
    resolve_url("x/./y", "http://example.com//a/", path_normalisation = "both"),
    resolve_url("x/./y", "http://example.com//a/", path_normalization = "both")
  )
  expect_error(
    resolve_url(
      "x", "http://example.com/", url_standard = "whatwg",
      path_normalisation = "none"
    ),
    "governs `path_normalization`"
  )
})
