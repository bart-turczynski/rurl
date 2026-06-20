# Accessor <-> parser-option coverage oracle (RURL-lipzdisu)
#
# Registry states:
#   "exposed"            - accessor takes the option (or an alias)
#   "by-design-omitted"  - option cannot affect this component, or is an
#                          intentional override; must carry a non-empty reason
#   "gap"                - option SHOULD be exposed but isn't today; carries
#                          fill_slice referencing the issue ID
#
# The test asserts:
#   1. Every "exposed" cell exists in the accessor's formals (or via an alias).
#   2. Every "by-design-omitted" cell has a non-empty reason string.
#   3. No registry cell references an option absent from safe_parse_url().
#   4. "gap" and "by-design-omitted" cells are allowed without checking
#      the accessor signature.

# ---------------------------------------------------------------------------
# Registry helpers
# ---------------------------------------------------------------------------

.reg_exposed <- function(accessor, option, alias = NULL) {
  list(
    accessor = accessor,
    option = option,
    state = "exposed",
    alias = alias,
    reason = NULL,
    fill_slice = NULL
  )
}

.reg_omitted <- function(accessor, option, reason) {
  stopifnot(nzchar(reason))
  list(
    accessor = accessor,
    option = option,
    state = "by-design-omitted",
    alias = NULL,
    reason = reason,
    fill_slice = NULL
  )
}

.reg_gap <- function(accessor, option, fill_slice) {
  list(
    accessor = accessor,
    option = option,
    state = "gap",
    alias = NULL,
    reason = NULL,
    fill_slice = fill_slice
  )
}

# ---------------------------------------------------------------------------
# Known safe_parse_url() options (ground truth from R/parse.R)
# ---------------------------------------------------------------------------

.spu_options <- c(
  "protocol_handling",
  "www_handling",
  "tld_source",
  "case_handling",
  "trailing_slash_handling",
  "index_page_handling",
  "path_normalization",
  "scheme_relative_handling",
  "subdomain_levels_to_keep",
  "host_encoding",
  "path_encoding"
)

# ---------------------------------------------------------------------------
# The registry
# ---------------------------------------------------------------------------

.accessor_registry <- list(

  # ---- get_parse_status --------------------------------------------------
  .reg_exposed("get_parse_status", "protocol_handling"),
  .reg_exposed("get_parse_status", "www_handling"),
  .reg_exposed("get_parse_status", "subdomain_levels_to_keep"),
  .reg_omitted(
    "get_parse_status", "tld_source",
    "gap: tld_source not yet threaded through get_parse_status"
  ),
  .reg_omitted(
    "get_parse_status", "case_handling",
    "forced to 'lower' internally; not user-controllable for parse_status"
  ),
  .reg_omitted(
    "get_parse_status", "trailing_slash_handling",
    "trailing_slash has no effect on the parse_status field"
  ),
  .reg_omitted(
    "get_parse_status", "index_page_handling",
    "index_page_handling does not affect parse_status"
  ),
  .reg_omitted(
    "get_parse_status", "path_normalization",
    "path_normalization does not affect parse_status"
  ),
  .reg_omitted(
    "get_parse_status", "scheme_relative_handling",
    "scheme_relative_handling affects the scheme field, not parse_status"
  ),
  .reg_omitted(
    "get_parse_status", "host_encoding",
    "host_encoding does not affect parse_status"
  ),
  .reg_omitted(
    "get_parse_status", "path_encoding",
    "path_encoding does not affect parse_status"
  ),

  # ---- get_clean_url -----------------------------------------------------
  .reg_exposed("get_clean_url", "protocol_handling"),
  .reg_exposed("get_clean_url", "www_handling"),
  .reg_omitted(
    "get_clean_url", "tld_source",
    "gap: tld_source not yet threaded through get_clean_url"
  ),
  .reg_exposed("get_clean_url", "case_handling"),
  .reg_exposed("get_clean_url", "trailing_slash_handling"),
  .reg_exposed("get_clean_url", "index_page_handling"),
  .reg_exposed("get_clean_url", "path_normalization"),
  .reg_exposed("get_clean_url", "scheme_relative_handling"),
  .reg_exposed("get_clean_url", "subdomain_levels_to_keep"),
  .reg_exposed("get_clean_url", "host_encoding"),
  .reg_exposed("get_clean_url", "path_encoding"),

  # ---- get_domain --------------------------------------------------------
  .reg_exposed("get_domain", "protocol_handling"),
  .reg_exposed("get_domain", "www_handling"),
  .reg_exposed("get_domain", "tld_source", alias = "source"),
  .reg_omitted(
    "get_domain", "case_handling",
    "forced to 'lower' internally; domain is always lowercased"
  ),
  .reg_omitted(
    "get_domain", "trailing_slash_handling",
    "trailing_slash_handling does not affect the domain component"
  ),
  .reg_omitted(
    "get_domain", "index_page_handling",
    "index_page_handling does not affect the domain component"
  ),
  .reg_omitted(
    "get_domain", "path_normalization",
    "path_normalization does not affect the domain component"
  ),
  .reg_omitted(
    "get_domain", "scheme_relative_handling",
    "scheme_relative_handling does not affect the domain component"
  ),
  .reg_exposed("get_domain", "subdomain_levels_to_keep"),
  .reg_omitted(
    "get_domain", "host_encoding",
    "host_encoding affects clean_url reconstruction only, not the domain field"
  ),
  .reg_omitted(
    "get_domain", "path_encoding",
    "path_encoding does not affect the domain component"
  ),

  # ---- get_scheme --------------------------------------------------------
  .reg_exposed("get_scheme", "protocol_handling"),
  .reg_omitted(
    "get_scheme", "www_handling",
    "www_handling only modifies the host; scheme is unaffected"
  ),
  .reg_omitted(
    "get_scheme", "tld_source",
    "tld_source does not affect the scheme component"
  ),
  .reg_omitted(
    "get_scheme", "case_handling",
    "forced to 'lower' internally; scheme is always lowercased"
  ),
  .reg_omitted(
    "get_scheme", "trailing_slash_handling",
    "trailing_slash_handling does not affect the scheme component"
  ),
  .reg_omitted(
    "get_scheme", "index_page_handling",
    "index_page_handling does not affect the scheme component"
  ),
  .reg_omitted(
    "get_scheme", "path_normalization",
    "path_normalization does not affect the scheme component"
  ),
  .reg_gap(
    "get_scheme", "scheme_relative_handling",
    fill_slice = "RURL-lipzdisu"
  ),
  .reg_omitted(
    "get_scheme", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; scheme is unaffected"
  ),
  .reg_omitted(
    "get_scheme", "host_encoding",
    "host_encoding does not affect the scheme component"
  ),
  .reg_omitted(
    "get_scheme", "path_encoding",
    "path_encoding does not affect the scheme component"
  ),

  # ---- get_host ----------------------------------------------------------
  .reg_exposed("get_host", "protocol_handling"),
  .reg_exposed("get_host", "www_handling"),
  .reg_gap(
    "get_host", "tld_source",
    fill_slice = "RURL-lipzdisu"
  ),
  .reg_exposed("get_host", "case_handling"),
  .reg_omitted(
    "get_host", "trailing_slash_handling",
    "trailing_slash_handling does not affect the host component"
  ),
  .reg_omitted(
    "get_host", "index_page_handling",
    "index_page_handling does not affect the host component"
  ),
  .reg_omitted(
    "get_host", "path_normalization",
    "path_normalization does not affect the host component"
  ),
  .reg_omitted(
    "get_host", "scheme_relative_handling",
    "scheme_relative_handling does not affect the host component"
  ),
  .reg_exposed("get_host", "subdomain_levels_to_keep"),
  .reg_gap(
    "get_host", "host_encoding",
    fill_slice = "RURL-lipzdisu"
  ),
  .reg_omitted(
    "get_host", "path_encoding",
    "path_encoding does not affect the host component"
  ),

  # ---- get_path ----------------------------------------------------------
  .reg_exposed("get_path", "protocol_handling"),
  .reg_omitted(
    "get_path", "www_handling",
    "www_handling only modifies the host; path is unaffected"
  ),
  .reg_omitted(
    "get_path", "tld_source",
    "tld_source does not affect the path component"
  ),
  .reg_exposed("get_path", "case_handling"),
  .reg_exposed("get_path", "trailing_slash_handling"),
  .reg_exposed("get_path", "index_page_handling"),
  .reg_exposed("get_path", "path_normalization"),
  .reg_omitted(
    "get_path", "scheme_relative_handling",
    "scheme_relative_handling does not affect the path component"
  ),
  .reg_omitted(
    "get_path", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; path is unaffected"
  ),
  .reg_omitted(
    "get_path", "host_encoding",
    "host_encoding does not affect the path component"
  ),
  .reg_exposed("get_path", "path_encoding"),

  # ---- get_query ---------------------------------------------------------
  .reg_exposed("get_query", "protocol_handling"),
  .reg_omitted(
    "get_query", "www_handling",
    "www_handling only modifies the host; query is unaffected"
  ),
  .reg_omitted(
    "get_query", "tld_source",
    "tld_source does not affect the query component"
  ),
  .reg_omitted(
    "get_query", "case_handling",
    "query strings are case-sensitive; case_handling is not meaningful here"
  ),
  .reg_omitted(
    "get_query", "trailing_slash_handling",
    "trailing_slash_handling does not affect the query component"
  ),
  .reg_omitted(
    "get_query", "index_page_handling",
    "index_page_handling does not affect the query component"
  ),
  .reg_omitted(
    "get_query", "path_normalization",
    "path_normalization does not affect the query component"
  ),
  .reg_omitted(
    "get_query", "scheme_relative_handling",
    "scheme_relative_handling does not affect the query component"
  ),
  .reg_omitted(
    "get_query", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; query is unaffected"
  ),
  .reg_omitted(
    "get_query", "host_encoding",
    "host_encoding does not affect the query component"
  ),
  .reg_omitted(
    "get_query", "path_encoding",
    "path_encoding does not affect the query component"
  ),

  # ---- get_fragment ------------------------------------------------------
  .reg_exposed("get_fragment", "protocol_handling"),
  .reg_omitted(
    "get_fragment", "www_handling",
    "www_handling only modifies the host; fragment is unaffected"
  ),
  .reg_omitted(
    "get_fragment", "tld_source",
    "tld_source does not affect the fragment component"
  ),
  .reg_omitted(
    "get_fragment", "case_handling",
    "fragment identifiers are case-sensitive; case_handling is not meaningful"
  ),
  .reg_omitted(
    "get_fragment", "trailing_slash_handling",
    "trailing_slash_handling does not affect the fragment component"
  ),
  .reg_omitted(
    "get_fragment", "index_page_handling",
    "index_page_handling does not affect the fragment component"
  ),
  .reg_omitted(
    "get_fragment", "path_normalization",
    "path_normalization does not affect the fragment component"
  ),
  .reg_omitted(
    "get_fragment", "scheme_relative_handling",
    "scheme_relative_handling does not affect the fragment component"
  ),
  .reg_omitted(
    "get_fragment", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; fragment is unaffected"
  ),
  .reg_omitted(
    "get_fragment", "host_encoding",
    "host_encoding does not affect the fragment component"
  ),
  .reg_omitted(
    "get_fragment", "path_encoding",
    "path_encoding does not affect the fragment component"
  ),

  # ---- get_port ----------------------------------------------------------
  .reg_exposed("get_port", "protocol_handling"),
  .reg_omitted(
    "get_port", "www_handling",
    "www_handling only modifies the host prefix; port is unaffected"
  ),
  .reg_omitted(
    "get_port", "tld_source",
    "tld_source does not affect the port component"
  ),
  .reg_omitted(
    "get_port", "case_handling",
    "port is numeric; case_handling is not meaningful"
  ),
  .reg_omitted(
    "get_port", "trailing_slash_handling",
    "trailing_slash_handling does not affect the port component"
  ),
  .reg_omitted(
    "get_port", "index_page_handling",
    "index_page_handling does not affect the port component"
  ),
  .reg_omitted(
    "get_port", "path_normalization",
    "path_normalization does not affect the port component"
  ),
  .reg_omitted(
    "get_port", "scheme_relative_handling",
    "scheme_relative_handling does not affect the port component"
  ),
  .reg_omitted(
    "get_port", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; port is unaffected"
  ),
  .reg_omitted(
    "get_port", "host_encoding",
    "host_encoding does not affect the port component"
  ),
  .reg_omitted(
    "get_port", "path_encoding",
    "path_encoding does not affect the port component"
  ),

  # ---- get_user ----------------------------------------------------------
  .reg_exposed("get_user", "protocol_handling"),
  .reg_omitted(
    "get_user", "www_handling",
    "www_handling only modifies the host; userinfo is unaffected"
  ),
  .reg_omitted(
    "get_user", "tld_source",
    "tld_source does not affect the user component"
  ),
  .reg_omitted(
    "get_user", "case_handling",
    "usernames are case-sensitive; case_handling is not meaningful here"
  ),
  .reg_omitted(
    "get_user", "trailing_slash_handling",
    "trailing_slash_handling does not affect the user component"
  ),
  .reg_omitted(
    "get_user", "index_page_handling",
    "index_page_handling does not affect the user component"
  ),
  .reg_omitted(
    "get_user", "path_normalization",
    "path_normalization does not affect the user component"
  ),
  .reg_omitted(
    "get_user", "scheme_relative_handling",
    "scheme_relative_handling does not affect the user component"
  ),
  .reg_omitted(
    "get_user", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; userinfo is unaffected"
  ),
  .reg_omitted(
    "get_user", "host_encoding",
    "host_encoding does not affect the user component"
  ),
  .reg_omitted(
    "get_user", "path_encoding",
    "path_encoding does not affect the user component"
  ),

  # ---- get_password ------------------------------------------------------
  .reg_exposed("get_password", "protocol_handling"),
  .reg_omitted(
    "get_password", "www_handling",
    "www_handling only modifies the host; userinfo is unaffected"
  ),
  .reg_omitted(
    "get_password", "tld_source",
    "tld_source does not affect the password component"
  ),
  .reg_omitted(
    "get_password", "case_handling",
    "passwords are case-sensitive; case_handling is not meaningful here"
  ),
  .reg_omitted(
    "get_password", "trailing_slash_handling",
    "trailing_slash_handling does not affect the password component"
  ),
  .reg_omitted(
    "get_password", "index_page_handling",
    "index_page_handling does not affect the password component"
  ),
  .reg_omitted(
    "get_password", "path_normalization",
    "path_normalization does not affect the password component"
  ),
  .reg_omitted(
    "get_password", "scheme_relative_handling",
    "scheme_relative_handling does not affect the password component"
  ),
  .reg_omitted(
    "get_password", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; userinfo is unaffected"
  ),
  .reg_omitted(
    "get_password", "host_encoding",
    "host_encoding does not affect the password component"
  ),
  .reg_omitted(
    "get_password", "path_encoding",
    "path_encoding does not affect the password component"
  ),

  # ---- get_userinfo ------------------------------------------------------
  .reg_exposed("get_userinfo", "protocol_handling"),
  .reg_omitted(
    "get_userinfo", "www_handling",
    "www_handling only modifies the host; userinfo is unaffected"
  ),
  .reg_omitted(
    "get_userinfo", "tld_source",
    "tld_source does not affect the userinfo component"
  ),
  .reg_omitted(
    "get_userinfo", "case_handling",
    "userinfo is case-sensitive; case_handling is not meaningful here"
  ),
  .reg_omitted(
    "get_userinfo", "trailing_slash_handling",
    "trailing_slash_handling does not affect the userinfo component"
  ),
  .reg_omitted(
    "get_userinfo", "index_page_handling",
    "index_page_handling does not affect the userinfo component"
  ),
  .reg_omitted(
    "get_userinfo", "path_normalization",
    "path_normalization does not affect the userinfo component"
  ),
  .reg_omitted(
    "get_userinfo", "scheme_relative_handling",
    "scheme_relative_handling does not affect the userinfo component"
  ),
  .reg_omitted(
    "get_userinfo", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep only modifies the host; userinfo is unaffected"
  ),
  .reg_omitted(
    "get_userinfo", "host_encoding",
    "host_encoding does not affect the userinfo component"
  ),
  .reg_omitted(
    "get_userinfo", "path_encoding",
    "path_encoding does not affect the userinfo component"
  ),

  # ---- get_subdomain -----------------------------------------------------
  .reg_exposed("get_subdomain", "protocol_handling"),
  .reg_exposed("get_subdomain", "www_handling"),
  .reg_exposed("get_subdomain", "tld_source", alias = "source"),
  .reg_omitted(
    "get_subdomain", "case_handling",
    "forced to 'lower' internally; subdomain is always lowercased"
  ),
  .reg_omitted(
    "get_subdomain", "trailing_slash_handling",
    "trailing_slash_handling does not affect the subdomain component"
  ),
  .reg_omitted(
    "get_subdomain", "index_page_handling",
    "index_page_handling does not affect the subdomain component"
  ),
  .reg_omitted(
    "get_subdomain", "path_normalization",
    "path_normalization does not affect the subdomain component"
  ),
  .reg_omitted(
    "get_subdomain", "scheme_relative_handling",
    "scheme_relative_handling does not affect the subdomain component"
  ),
  .reg_omitted(
    "get_subdomain", "subdomain_levels_to_keep",
    paste0(
      "subdomain_levels_to_keep is not threaded through get_subdomain; ",
      "subdomain extraction uses www_handling and include_www instead"
    )
  ),
  .reg_omitted(
    "get_subdomain", "host_encoding",
    "host_encoding does not affect the subdomain extraction"
  ),
  .reg_omitted(
    "get_subdomain", "path_encoding",
    "path_encoding does not affect the subdomain component"
  ),

  # ---- get_tld -----------------------------------------------------------
  .reg_omitted(
    "get_tld", "protocol_handling",
    paste0(
      "get_tld omits protocol_handling as a deliberate simplification; ",
      "TLD extraction is scheme-independent"
    )
  ),
  .reg_omitted(
    "get_tld", "www_handling",
    "www_handling only modifies the host prefix; TLD is unaffected"
  ),
  .reg_exposed("get_tld", "tld_source", alias = "source"),
  .reg_omitted(
    "get_tld", "case_handling",
    "forced to 'lower' internally; TLD is always lowercased"
  ),
  .reg_omitted(
    "get_tld", "trailing_slash_handling",
    "trailing_slash_handling does not affect the TLD component"
  ),
  .reg_omitted(
    "get_tld", "index_page_handling",
    "index_page_handling does not affect the TLD component"
  ),
  .reg_omitted(
    "get_tld", "path_normalization",
    "path_normalization does not affect the TLD component"
  ),
  .reg_omitted(
    "get_tld", "scheme_relative_handling",
    "scheme_relative_handling does not affect the TLD component"
  ),
  .reg_omitted(
    "get_tld", "subdomain_levels_to_keep",
    "subdomain_levels_to_keep does not affect the TLD component"
  ),
  .reg_omitted(
    "get_tld", "host_encoding",
    "host_encoding does not affect the TLD component"
  ),
  .reg_omitted(
    "get_tld", "path_encoding",
    "path_encoding does not affect the TLD component"
  )
)

# ---------------------------------------------------------------------------
# Testthat assertions
# ---------------------------------------------------------------------------

test_that("registry: every exposed cell exists in the accessor's formals", {
  exposed_cells <- Filter(function(x) x$state == "exposed", .accessor_registry)
  for (cell in exposed_cells) {
    accessor_fn <- get(cell$accessor, envir = asNamespace("rurl"))
    fn_formals <- names(formals(accessor_fn))
    # Use alias if provided; otherwise use the canonical option name
    param_to_check <- if (!is.null(cell$alias)) cell$alias else cell$option
    expect_true(
      param_to_check %in% fn_formals,
      label = paste0(
        cell$accessor, "() should have formal '", param_to_check,
        "' (option: ", cell$option, ")"
      )
    )
  }
})

test_that("registry: every by-design-omitted cell has a non-empty reason", {
  omitted_cells <- Filter(
    function(x) x$state == "by-design-omitted",
    .accessor_registry
  )
  for (cell in omitted_cells) {
    expect_true(
      !is.null(cell$reason) && nzchar(cell$reason),
      label = paste0(
        cell$accessor, "/", cell$option,
        " by-design-omitted entry must have a non-empty reason"
      )
    )
  }
})

test_that("registry: no cell references an unknown safe_parse_url() option", {
  for (cell in .accessor_registry) {
    expect_true(
      cell$option %in% .spu_options,
      label = paste0(
        "Option '", cell$option, "' in registry for ", cell$accessor,
        " is not a known safe_parse_url() parameter"
      )
    )
  }
})

test_that("registry: all accessor x option cells are present", {
  accessors <- c(
    "get_parse_status", "get_clean_url", "get_domain", "get_scheme",
    "get_host", "get_path", "get_query", "get_fragment", "get_port",
    "get_user", "get_password", "get_userinfo", "get_subdomain", "get_tld"
  )
  for (acc in accessors) {
    for (opt in .spu_options) {
      matching <- Filter(
        function(x) x$accessor == acc && x$option == opt,
        .accessor_registry
      )
      expect_true(
        length(matching) == 1L,
        label = paste0(
          "Registry must have exactly one entry for ", acc, " x ", opt
        )
      )
    }
  }
})
