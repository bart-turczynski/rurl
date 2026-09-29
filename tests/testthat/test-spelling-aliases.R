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
