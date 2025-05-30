# Test cases for specific line coverage using mockery

# Save original base functions before any mocking starts
.real_base_gsub <- base::gsub
.real_base_nzchar <- base::nzchar
.real_base_iconv <- base::iconv

test_that("permute_url hits line ~122 (else for empty unique_permutations)", {
  # Target: R/permute_url.R lines ~121-124
  input_url <- "example.com"

  mock_gsub <- function(pattern, replacement, x, ...) {
    if (identical(pattern, "^(https?://)?(www[.])?$") && identical(replacement, "")) {
      return(rep("", length(x)))
    }
    # Retrieve and call the true base::gsub for the passthrough case
    true_base_gsub <- get("gsub", envir = baseenv(), inherits = FALSE)
    return(true_base_gsub(pattern, replacement, x, ...))
  }

  testthat::local_mock(gsub = mock_gsub)
  
  result <- permute_url(input_url)
  
  expect_equal(nrow(result), 1, info = "Expected 1 row for NA permutation")
  expect_equal(result$URL, input_url, info = "URL should match input")
  expect_true(is.na(result$Permutation), info = "Permutation should be NA")
})

test_that("permute_url hits line ~82 (next if !nzchar(current_perm_host))", {
  # Target: R/permute_url.R line ~82
  input_url <- "domain.com"

  mock_nzchar <- function(x) {
    if (identical(x, "domain.com")) { 
      return(FALSE)
    }
    # Retrieve and call the true base::nzchar for the passthrough case
    true_base_nzchar <- get("nzchar", envir = baseenv(), inherits = FALSE)
    return(true_base_nzchar(x))
  }

  # If local_mock for nzchar continues to fail with "old_fun", this test may need to be skipped
  # or nzchar's mockability re-evaluated for this environment.
  testthat::local_mock(nzchar = mock_nzchar)
  
  result <- permute_url(input_url)

  expect_equal(nrow(result), 6, info = "Expected 6 permutations for www-only variants")
  expect_false(any(grepl(paste0("^http(s)?://domain\\.com"), result$Permutation)), info = "No non-www permutations with scheme")
  expect_false(any(grepl("^domain\\.com", result$Permutation)), info = "No raw non-www permutations")
  expect_true(all(grepl("www\\.domain\\.com", result$Permutation)), info = "All permutations should contain www.domain.com")
})

# For rurl.R specific lines:

# Regarding R/rurl.R line ~122 (in safe_parse_url): `final_host <- \"\"`
# This line appears to be unreachable. It's within:
# } else if (www_handling == \"keep\") {
#   if (raw_host == \"\") { // Condition for the target line
#     final_host <- \"\"
#   } ... }
# The entire `www_handling` block is conditional on `!is_ip_host && !is.na(raw_host) && raw_host != \"\"`.
# If `raw_host == \"\"`, then `raw_host != \"\"` is FALSE, so the outer block (and thus the target line) is skipped.
# No direct test is provided as it seems to be dead code.

# Regarding R/rurl.R line ~187 (in safe_parse_url): `parse_status <- \"error\"`
# This line appears to be unreachable. It's within:
# if(is.null(parsed_curl) || (original_looks_like_protocol && !original_has_allowed_scheme)) {
#   parse_status <- \"error\"
# }
# Conditions that make the above 'if' true (e.g. `is.null(parsed_curl)` or disallowed scheme)
# are handled by `return(NULL)` statements earlier in `safe_parse_url`, preventing this line from being reached.
# No direct test is provided as it seems to be dead code.

test_that(".punycode_to_unicode hits line ~316 (return '' if iconv returns NA)", {
  # Target: R/rurl.R line ~316
  input_domain_part <- "test"
  
  mock_iconv <- function(x, from, to, sub) {
    if (identical(x, input_domain_part)) {
      return(NA_character_)
    }
    # Retrieve and call the true base::iconv for the passthrough case
    true_base_iconv <- get("iconv", envir = baseenv(), inherits = FALSE)
    return(true_base_iconv(x, from, to, sub))
  }
  
  testthat::local_mock(iconv = mock_iconv)
  
  result <- rurl:::.punycode_to_unicode(input_domain_part)
  expect_equal(result, "", info = "Expected empty string when iconv returns NA")
})

test_that("._extract_tld_original_logic hits line ~577 (return NA_character_)", {
  # Target: R/rurl.R line ~577
  
  original_normalize_and_punycode <- rurl:::.normalize_and_punycode

  # Strategy 1: Mock .normalize_and_punycode to return NA_character_
  host_na_case <- "na-trigger.com"
  mock_norm_puny_na <- function(host, encode_fn = NULL) {
    if (identical(host, host_na_case)) {
      return(NA_character_)
    }
    return(original_normalize_and_punycode(host, encode_fn = encode_fn))
  }
  
  assignInNamespace(".normalize_and_punycode", mock_norm_puny_na, ns = "rurl")
  
  result_na <- rurl:::._extract_tld_original_logic(host_na_case, rurl:::tld_all)
  expect_equal(result_na, NA_character_, info = "Expected NA when .normalize_and_punycode returns NA")
  
  # Strategy 2: Mock .normalize_and_punycode to return \"\" (empty string)
  host_empty_case <- "empty-trigger.com"
  mock_norm_puny_empty <- function(host, encode_fn = NULL) {
    if (identical(host, host_empty_case)) {
      return("")
    }
    return(original_normalize_and_punycode(host, encode_fn = encode_fn))
  }
  
  assignInNamespace(".normalize_and_punycode", mock_norm_puny_empty, ns = "rurl")

  result_empty <- rurl:::._extract_tld_original_logic(host_empty_case, rurl:::tld_all)
  expect_equal(result_empty, NA_character_, info = "Expected NA when .normalize_and_punycode returns empty string")

  assignInNamespace(".normalize_and_punycode", original_normalize_and_punycode, ns = "rurl")
}) 