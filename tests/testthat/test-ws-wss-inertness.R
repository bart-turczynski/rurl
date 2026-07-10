# Inertness tests for the ws/wss special-scheme metadata (RURL-qluqkdwl,
# ADR 0012 Layer 1). ws/wss are now registered as WHATWG-special metadata
# (.WHATWG_SPECIAL_SCHEMES / .SPECIAL_AUTHORITY_SCHEMES / .SCHEME_DEFAULT_PORTS)
# but DELIBERATELY absent from .SUPPORTED_SCHEMES, so under default options the
# prefix gate (R/parse-phases.R, allowed_prefixes <- paste0(.SUPPORTED_SCHEMES,
# "://")) rejects them before any special-scheme lookup. These tests assert the
# metadata is inert: ws/wss inputs hard-error exactly like a peer unsupported
# scheme (ssh) until the Layer 2 acceptance axis lands. If the byte-for-byte
# behavior ever diverges here, the metadata was not inert.

# Peer unsupported scheme: establishes the observed error contract (do not
# hardcode a guessed status -- read it from a real unsupported scheme).
ssh_row <- safe_parse_urls("ssh://example.com")
ssh_status <- ssh_row$parse_status

test_that("peer unsupported scheme (ssh) hard-errors with NA clean_url", {
  expect_identical(ssh_status, "error")
  expect_true(is.na(ssh_row$clean_url))
})

test_that("ws/wss hard-error identically to a peer unsupported scheme", {
  for (u in c("ws://example.com", "wss://example.com/chat")) {
    d <- safe_parse_urls(u)
    expect_identical(d$parse_status, ssh_status, info = u)
    expect_true(is.na(d$clean_url), info = u)
  }
})

test_that("ws/wss are absent from the default-acceptance allowlist", {
  # The byte-compat invariant: .SUPPORTED_SCHEMES must not grow ws/wss.
  expect_identical(
    rurl:::.SUPPORTED_SCHEMES, c("http", "https", "ftp", "ftps", "file")
  )
  expect_false("ws" %in% rurl:::.SUPPORTED_SCHEMES)
  expect_false("wss" %in% rurl:::.SUPPORTED_SCHEMES)
})

test_that("ws/wss are registered as WHATWG-special metadata (but inert)", {
  # Registration landed (Layer 1) even though acceptance has not (Layer 2).
  expect_true("ws" %in% rurl:::.WHATWG_SPECIAL_SCHEMES)
  expect_true("wss" %in% rurl:::.WHATWG_SPECIAL_SCHEMES)
  expect_true("ws" %in% rurl:::.SPECIAL_AUTHORITY_SCHEMES)
  expect_true("wss" %in% rurl:::.SPECIAL_AUTHORITY_SCHEMES)
  expect_identical(rurl:::.SCHEME_DEFAULT_PORTS[["ws"]], 80L)
  expect_identical(rurl:::.SCHEME_DEFAULT_PORTS[["wss"]], 443L)
})

test_that("singular safe_parse_url() returns NULL for ws/wss, like ssh", {
  # The scalar entry point drops an unsupported-scheme input to NULL; ws/wss
  # behave identically to the peer unsupported scheme.
  expect_null(safe_parse_url("ssh://example.com"))
  expect_null(safe_parse_url("ws://example.com"))
  expect_null(safe_parse_url("wss://example.com/chat"))
})
