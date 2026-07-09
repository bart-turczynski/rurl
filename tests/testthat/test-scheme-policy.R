# Tests for the scheme_policy axis (RURL-vzgeurae): the input-*acceptance*
# knob for scheme-less host-shaped input. "infer" (default) fabricates
# "http://"; "require" rejects. Orthogonal to protocol_handling (presentation)
# and url_standard (interpretation). Covers the protocol_handling x
# scheme_policy matrix, the scheme_relative_handling interaction, validation,
# the cache, the accessor surface, and the ada-005 opt-out.

# The five protocol_handling values, exercised against scheme_policy.
ph_values <- c("keep", "none", "strip", "http", "https")

test_that("default 'infer' is byte-for-byte unchanged for scheme-less host", {
  # No scheme_policy argument -> historical behavior.
  d_default <- safe_parse_urls("example.com")
  d_infer <- safe_parse_urls("example.com", scheme_policy = "infer")
  expect_identical(d_default, d_infer)
  expect_identical(d_default$parse_status, "ok")
  expect_identical(d_default$clean_url, "http://example.com/")
})

test_that("require rejects scheme-less host under every protocol_handling", {
  for (ph in ph_values) {
    d <- safe_parse_urls(
      "example.com", protocol_handling = ph, scheme_policy = "require"
    )
    expect_identical(
      d$parse_status, "error",
      info = paste("protocol_handling =", ph)
    )
    expect_true(is.na(d$clean_url), info = paste("protocol_handling =", ph))
  }
})

test_that("require rejects scheme-less host:port under every protocol", {
  for (ph in ph_values) {
    d <- safe_parse_urls(
      "example.com:8080/p", protocol_handling = ph, scheme_policy = "require"
    )
    expect_identical(
      d$parse_status, "error",
      info = paste("protocol_handling =", ph)
    )
  }
})

test_that("require rejects scheme-less numeric IPv4 under a selector", {
  # Under url_standard the bare numeric host is host-shaped (add_http path), so
  # require folds it into the reject set instead of coercing it.
  for (std in c("rfc3986", "whatwg")) {
    d <- safe_parse_urls(
      "2130706433", url_standard = std, scheme_policy = "require"
    )
    expect_identical(d$parse_status, "error", info = std)
  }
})

test_that("require keeps input that carries an explicit supported scheme", {
  explicit <- c(
    "http://example.com/", "https://example.com/a?b=1",
    "ftp://files.example.com/x"
  )
  for (ph in ph_values) {
    d <- safe_parse_urls(
      explicit, protocol_handling = ph, scheme_policy = "require"
    )
    expect_false(
      any(d$parse_status == "error"),
      info = paste("protocol_handling =", ph)
    )
  }
})

test_that("require and infer agree on already-scheme-bearing input", {
  # For input that never touches the add_http path, the two policies must
  # produce byte-identical output.
  scheme_bearing <- c(
    "http://example.com/path?q=1#f",
    "https://sub.example.co.uk/a/b/",
    "ftp://user@files.example.org:21/x"
  )
  for (ph in ph_values) {
    d_infer <- safe_parse_urls(scheme_bearing, protocol_handling = ph)
    d_req <- safe_parse_urls(
      scheme_bearing, protocol_handling = ph, scheme_policy = "require"
    )
    expect_identical(d_infer, d_req, info = paste("protocol_handling =", ph))
  }
})

test_that("scheme-relative //host is governed by scheme_relative_handling", {
  # //host is scheme-less but has its own axis; scheme_policy = "require" must
  # NOT reject it. The dedicated scheme_relative_handling knob still decides.
  srh_keep <- safe_parse_urls(
    "//cdn.example.com", scheme_policy = "require",
    scheme_relative_handling = "keep"
  )
  expect_identical(srh_keep$parse_status, "ok-scheme-relative")
  expect_identical(srh_keep$clean_url, "http://cdn.example.com/")

  srh_http <- safe_parse_urls(
    "//cdn.example.com", scheme_policy = "require",
    scheme_relative_handling = "http"
  )
  expect_identical(srh_http$parse_status, "ok")

  # Rejecting //host remains the job of scheme_relative_handling = "error".
  srh_err <- safe_parse_urls(
    "//cdn.example.com", scheme_policy = "require",
    scheme_relative_handling = "error"
  )
  expect_identical(srh_err$parse_status, "error")

  # And with scheme_policy = "infer", //host under "error" is still rejected --
  # confirming the two axes are independent.
  srh_err_infer <- safe_parse_urls(
    "//cdn.example.com", scheme_policy = "infer",
    scheme_relative_handling = "error"
  )
  expect_identical(srh_err_infer$parse_status, "error")
})

test_that("non-host-shaped scheme-less input (D1) errors under both policies", {
  junk <- c("asdfghjkl", "hello world", "/relative/path")
  for (sp in c("infer", "require")) {
    d <- safe_parse_urls(junk, scheme_policy = sp)
    expect_true(all(d$parse_status == "error"), info = sp)
  }
})

test_that("require folds scheme-less userinfo into error", {
  # Under infer this is warning-userinfo (host still resolves); under require
  # the whole row is rejected before that classification.
  infer <- safe_parse_urls("user@example.com", scheme_policy = "infer")
  expect_identical(infer$parse_status, "warning-userinfo")

  req <- safe_parse_urls("user@example.com", scheme_policy = "require")
  expect_identical(req$parse_status, "error")
})

test_that("scheme_policy is validated via match.arg", {
  expect_error(
    safe_parse_urls("example.com", scheme_policy = "strict"),
    "should be one of"
  )
  expect_error(
    safe_parse_url("example.com", scheme_policy = "nope"),
    "should be one of"
  )
  # Partial matching is accepted (match.arg semantics).
  expect_identical(
    safe_parse_urls("example.com", scheme_policy = "req")$parse_status,
    "error"
  )
})

test_that("scheme_policy is in the parse cache key (no stale reuse)", {
  # Same URL under both policies within one session must not collide: infer
  # parses, require rejects.
  u <- "cache-probe.example.com"
  infer_status <- function() {
    safe_parse_urls(u, scheme_policy = "infer")$parse_status
  }
  expect_identical(infer_status(), "ok")
  expect_identical(
    safe_parse_urls(u, scheme_policy = "require")$parse_status, "error"
  )
  # And back again, to catch a one-directional cache poisoning.
  expect_identical(infer_status(), "ok")
})

test_that("safe_parse_url (scalar) honors scheme_policy", {
  expect_null(safe_parse_url("example.com", scheme_policy = "require"))
  kept <- safe_parse_url("http://example.com", scheme_policy = "require")
  expect_identical(kept$parse_status, "ok")
})

test_that("accessors honor scheme_policy for scheme-less input", {
  # Under require the scheme-less row is rejected, so host/domain/tld/clean_url
  # accessors fall back to their null_value and parse_status is "error".
  expect_true(is.na(get_host("example.com", scheme_policy = "require")))
  expect_true(is.na(get_domain("example.com", scheme_policy = "require")))
  expect_true(is.na(get_clean_url("example.com", scheme_policy = "require")))
  expect_identical(
    get_parse_status("example.com", scheme_policy = "require"), "error"
  )
  # infer keeps the historical resolution.
  expect_identical(
    get_host("example.com", scheme_policy = "infer"), "example.com"
  )
  expect_identical(
    get_parse_status("example.com", scheme_policy = "infer"), "ok"
  )
})

test_that("get_host_type honors scheme_policy under a selector", {
  # A scheme-less host under a selector is a parseable reg-name with infer, but
  # a rejected row with require -> host_type NA.
  ht_infer <- get_host_type(
    "example.com", url_standard = "whatwg", scheme_policy = "infer"
  )
  ht_require <- get_host_type(
    "example.com", url_standard = "whatwg", scheme_policy = "require"
  )
  expect_false(is.na(ht_infer))
  expect_true(is.na(ht_require))
})

test_that("ada-005 scheme-inference divergence is opt-out-able (matches Ada)", {
  # The ada-005 corpus row: scheme-less "example.com`x.example.com". Under the
  # whatwg profile with the default infer policy rurl accepts it (host-charset
  # shim keeps the backtick) where Ada rejects the scheme-less form. With
  # scheme_policy = "require" rurl rejects it too, restoring conformance on the
  # scheme-inference axis.
  ada005 <- "example.com`x.example.com"

  accepted <- safe_parse_urls(ada005, url_standard = "whatwg")
  expect_false(accepted$parse_status == "error")

  rejected <- safe_parse_urls(
    ada005, url_standard = "whatwg", scheme_policy = "require"
  )
  expect_identical(rejected$parse_status, "error")
})

test_that("yal-009 dotted host-port inference is opt-out-able", {
  # The yal-009 corpus row: WHATWG can read the dotted token before ":" as a
  # scheme. rurl's default usability policy instead reads it as host:port and
  # infers http, while strict parser mode rejects the scheme-less form.
  yal009 <- "www.php.net:80/index.php?test=1"

  inferred <- safe_parse_urls(yal009, url_standard = "whatwg")
  expect_identical(inferred$parse_status, "ok")
  expect_identical(inferred$scheme, "http")
  expect_identical(inferred$host, "www.php.net")
  expect_identical(inferred$path, "/index.php")
  expect_identical(inferred$query, "test=1")

  rejected <- safe_parse_urls(
    yal009, url_standard = "whatwg", scheme_policy = "require"
  )
  expect_identical(rejected$parse_status, "error")
  expect_true(is.na(rejected$clean_url))
})
