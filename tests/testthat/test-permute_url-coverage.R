test_that("permute_url handles empty input and include_rank", {
  res <- permute_url(character(0), include_rank = TRUE)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("URL", "Permutation", "PermutationRank"))
  expect_equal(nrow(res), 0)
})

test_that("permute_url validates option vectors and subdomain settings", {
  expect_error(
    permute_url("example.com", protocol_handling = character(0)),
    "must have at least one value"
  )

  expect_error(
    permute_url("example.com", subdomain_levels_to_keep = list(-1)),
    "non-negative"
  )

  res_null <- permute_url(
    "example.com",
    protocol_handling = "http",
    www_handling = "none",
    case_handling = "keep",
    trailing_slash_handling = "none",
    index_page_handling = "keep",
    path_normalization = "none",
    scheme_relative_handling = "keep",
    subdomain_levels_to_keep = NULL,
    host_encoding = "keep",
    path_encoding = "keep"
  )
  expect_true(nrow(res_null) > 0)

  res_sd <- permute_url(
    "example.com",
    protocol_handling = "http",
    www_handling = "none",
    case_handling = "keep",
    trailing_slash_handling = "none",
    index_page_handling = "keep",
    path_normalization = "none",
    scheme_relative_handling = "keep",
    subdomain_levels_to_keep = 1,
    host_encoding = "keep",
    path_encoding = "keep"
  )
  expect_true(nrow(res_sd) > 0)
})

test_that("permute_url adds root-path toggled variant", {
  res <- permute_url(
    "example.com",
    protocol_handling = "http",
    www_handling = "none",
    case_handling = "keep",
    trailing_slash_handling = "keep",
    index_page_handling = "keep",
    path_normalization = "none",
    scheme_relative_handling = "keep",
    host_encoding = "keep",
    path_encoding = "keep",
    include_rank = TRUE
  )

  perms <- res$Permutation
  expect_true("http://example.com/" %in% perms)
  expect_true("http://example.com" %in% perms)
})

test_that("permute_url handles scheme-relative errors in permutations", {
  res <- permute_url(
    "//example.com/path",
    protocol_handling = "keep",
    www_handling = "none",
    case_handling = "keep",
    trailing_slash_handling = "none",
    index_page_handling = "keep",
    path_normalization = "none",
    scheme_relative_handling = c("keep", "error"),
    host_encoding = "keep",
    path_encoding = "keep"
  )
  expect_true(any(!is.na(res$Permutation)))
})

test_that("permute_url drops permutations when clean_url is missing", {
  ns <- asNamespace("rurl")
  orig <- get("safe_parse_url", envir = ns)
  was_locked <- bindingIsLocked("safe_parse_url", ns)
  if (was_locked) unlockBinding("safe_parse_url", ns)
  withr::defer({
    assign("safe_parse_url", orig, envir = ns)
    if (was_locked) lockBinding("safe_parse_url", ns)
  }, testthat::teardown_env())

  assign("safe_parse_url", function(url,
                                    protocol_handling = c("keep", "none", "strip", "http", "https"),
                                    www_handling = c("none", "strip", "keep", "if_no_subdomain"),
                                    tld_source = c("all", "private", "icann"),
                                    case_handling = c("keep", "lower", "upper", "lower_host"),
                                    trailing_slash_handling = c("none", "keep", "strip"),
                                    index_page_handling = c("keep", "strip"),
                                    path_normalization = c("none", "collapse_slashes", "dot_segments", "both"),
                                    scheme_relative_handling = c("keep", "http", "https", "error"),
                                    subdomain_levels_to_keep = NULL,
                                    host_encoding = c("keep", "idna", "unicode"),
                                    path_encoding = c("keep", "encode", "decode")) {
    if (identical(protocol_handling, "keep")) {
      return(list(host = "example.com", path = "/", clean_url = "http://example.com", query = NA_character_, fragment = NA_character_))
    }
    list(host = "example.com", path = "/", clean_url = NA_character_, query = NA_character_, fragment = NA_character_)
  }, envir = ns)

  res <- permute_url(
    "example.com",
    protocol_handling = "strip",
    www_handling = "none",
    case_handling = "keep",
    trailing_slash_handling = "none",
    index_page_handling = "keep",
    path_normalization = "none",
    scheme_relative_handling = "keep",
    host_encoding = "keep",
    path_encoding = "keep"
  )
  expect_true(all(is.na(res$Permutation)))
})
