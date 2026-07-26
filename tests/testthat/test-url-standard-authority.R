# Tests for selector-profile authority recovery slices.

test_that("whatwg uses last at-sign as authority host delimiter", {
  u <- "http://username@@@@example.com"
  res <- safe_parse_url(u, url_standard = "whatwg")

  expect_identical(res$host, "example.com")
  expect_identical(res$user, "username%40%40%40")
  expect_true(is.na(res$password))
  expect_identical(res$clean_url, "http://example.com/")
  expect_identical(res$parse_status, "ok")
})

test_that("rfc3986 rejects a repeated raw at-sign instead of recovering", {
  # RE-POINTED by RURL-qrfrvmkg. This used to assert that the rfc3986 selector
  # recovers host=example.com / user=username%40%40%40 -- the RURL-zqhgezuq
  # last-"@" repair, which encodes the excess "@" bytes so libcurl can parse
  # the authority. That repair is CORRECT under whatwg (the WHATWG parser
  # genuinely takes the last "@") and is asserted above; under rfc3986 it was
  # laundering an input the grammar has no production for. Both `userinfo` and
  # `reg-name` forbid a raw "@", so a valid authority carries AT MOST ONE, and
  # the audited RFC oracle records `failure` for this exact string (fixture row
  # ada-018: "the RFC oracle ALSO rejects here -- repeated '@' leaves a userinfo
  # that is not well-formed"). The uniform gate judges the input as accepted,
  # BEFORE the parser-compat repairs, so the repair can no longer rescue it.
  u <- "http://username@@@@example.com"
  expect_null(safe_parse_url(u, url_standard = "rfc3986"))
  expect_identical(get_parse_status(u, url_standard = "rfc3986"), "error")
  # the grammar, asserted directly and without any backend, agrees.
  expect_false(isTRUE(.rfc3986_generic_uri_ok(u)$ok))
})

test_that("no selector leaves repeated at-sign authority baseline unchanged", {
  expect_null(safe_parse_url("http://username@@@@example.com"))
})

test_that("whatwg recovers authority for special schemes without slashes", {
  res <- safe_parse_urls(
    c("http:example.com", "https:example.com/path"),
    url_standard = "whatwg"
  )

  expect_identical(res$scheme, c("http", "https"))
  expect_identical(res$host, c("example.com", "example.com"))
  expect_identical(res$path, c("/", "/path"))
  expect_identical(
    res$clean_url, c("http://example.com/", "https://example.com/path")
  )
  expect_identical(res$parse_status, c("ok", "ok"))
})

test_that("rfc3986 keeps missing-slash special schemes as path-rootless", {
  res <- safe_parse_urls(
    c("http:example.com", "https:example.com/path"),
    url_standard = "rfc3986"
  )

  expect_identical(res$scheme, c("http", "https"))
  expect_true(all(is.na(res$host)))
  expect_identical(res$path, c("example.com", "example.com/path"))
  expect_true(all(is.na(res$clean_url)))
  expect_identical(res$parse_status, c("ok", "ok"))
})

test_that("no selector keeps special schemes without slashes as errors", {
  urls <- c("http:example.com", "https:example.com/path")

  expect_true(all(is.na(get_clean_url(urls))))
  expect_identical(get_parse_status(urls), c("error", "error"))
})

test_that("rfc3986 empty authority does not duplicate host into path", {
  urls <- c("https:///evil.com", "http:///evil.com")
  res <- safe_parse_urls(urls, url_standard = "rfc3986")

  expect_identical(res$host, c("evil.com", "evil.com"))
  expect_identical(res$path, c("/", "/"))
  expect_identical(
    res$clean_url, c("https://evil.com/", "http://evil.com/")
  )
  expect_identical(res$parse_status, c("ok", "ok"))

  for (i in seq_along(urls)) {
    occurrences <- sum(
      c(res$host[[i]], res$path[[i]]) == "evil.com" |
        c(res$host[[i]], res$path[[i]]) == "/evil.com",
      na.rm = TRUE
    )
    expect_identical(occurrences, 1L)
  }
})

test_that("whatwg empty-authority special schemes stay host/path coherent", {
  urls <- c("https:///evil.com", "https:////evil.com", "http:///evil.com")
  res <- safe_parse_urls(urls, url_standard = "whatwg")

  expect_identical(res$host, c("evil.com", "evil.com", "evil.com"))
  expect_identical(res$path, c("/", "/", "/"))
  expect_identical(
    res$clean_url,
    c("https://evil.com/", "https://evil.com/", "http://evil.com/")
  )
  expect_identical(res$parse_status, c("ok", "ok", "ok"))
})

test_that("rfc3986 rejects unsupported excess-slash empty authority", {
  res <- safe_parse_url("https:////evil.com", url_standard = "rfc3986")

  expect_null(res)
  expect_identical(
    get_parse_status("https:////evil.com", url_standard = "rfc3986"), "error"
  )
})

# --- WHATWG non-special empty-host validation (RURL-kknambrz T2, then ---------
# --- RURL-jxvibxqq) ----------------------------------------------------------
# Under scheme_acceptance="general", url_standard="whatwg", the non-special
# authority parser must reject a host-missing authority. T2 implemented that as
# "empty host carrying a non-null PORT"; RURL-jxvibxqq corrected the trigger to
# the DELIMITER, since WHATWG fails on the `:` or `@` itself, before/without any
# port content. The single legal empty-host shape is a `//` authority holding
# nothing else at all (`foo:///bar`). RFC-profile authority/port rules are a
# separate path (sibling T4/T5) and must stay untouched.

test_that("whatwg general rejects empty host with a port", {
  bad <- c("data://:443", "sc://:12/")
  res <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, c("error", "error"))
})

test_that("whatwg general keeps empty host with no port accepted", {
  # foo:///bar -- empty host, and the authority holds NOTHING else (no `@`, no
  # `:`) -- is the one legal empty-host shape for a non-special scheme.
  res <- safe_parse_urls(
    "foo:///bar", scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, "ok")
  expect_true(is.na(res$host))
  expect_true(is.na(res$port))
})

test_that("whatwg general rejects host-missing authority shapes", {
  # RURL-jxvibxqq. WHATWG makes an empty host a failure as soon as the
  # authority carries a delimiter, and the trigger is the DELIMITER, not the
  # port having content:
  #   * host state -- "if c is U+003A (:) ... if buffer is the empty string,
  #     host-missing validation error, return failure" (fires on `:` alone,
  #     before any port is read).
  #   * authority state -- "if atSignSeen is true and buffer is the empty
  #     string, host-missing validation error, return failure".
  # `data://:` was previously pinned here as legal on the reading that an empty
  # port is a null port and therefore harmless. That reading was wrong: the
  # host-state rule fires on the `:` itself. adaR 0.3.5 rejects it, and
  # `data://:443` / `sc://:/` / `sc://@/` / `sc://te@s:t@/` are all in the WPT
  # must-fail set (inst/bench/wpt-url-cases.json).
  bad <- c("sc://@/", "sc://te@s:t@/", "sc://:/", "data://:")
  res <- safe_parse_urls(
    bad, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, rep("error", length(bad)))
  # Rejected at `web` acceptance too -- general must not be the lenient one.
  web <- safe_parse_urls(
    bad, scheme_acceptance = "web", url_standard = "whatwg"
  )
  expect_identical(web$parse_status, rep("error", length(bad)))
})

test_that("whatwg general keeps legal authority shapes with delimiters", {
  # The mirror of the rejection test: a delimiter is only fatal when the host it
  # delimits is EMPTY. A non-empty host with an empty port, an IPv6 literal, or
  # userinfo all stay legal, so the host-missing rule cannot over-reject.
  ok <- c("sc://host:/", "sc://[::1]:/", "sc://user@host/", "sc://[::1]:80/")
  res <- safe_parse_urls(
    ok, scheme_acceptance = "general", url_standard = "whatwg"
  )
  expect_identical(res$parse_status, rep("ok", length(ok)))
})

test_that("rfc3986 general still accepts host-missing authority shapes", {
  # WHATWG-only, exactly like T2: RFC 3986's `reg-name` and `port` are both
  # `*`-quantified, so an empty host (with or without an empty port) is
  # well-formed generic syntax. `sc://te@s:t@/` is excluded -- it is rejected
  # under RFC for an unrelated reason (`@` is not in the `userinfo` production).
  res <- safe_parse_urls(
    c("sc://@/", "sc://:/", "data://:"),
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(res$parse_status, c("ok", "ok", "ok"))
})

test_that("rfc3986 general empty-host-with-port behavior is unchanged", {
  # T2 is WHATWG-only: the RFC profile still accepts these (its reg-name/port
  # rules are owned by sibling tickets), so the fix must not perturb it.
  res <- safe_parse_urls(
    c("data://:443", "sc://:12/"),
    scheme_acceptance = "general", url_standard = "rfc3986"
  )
  expect_identical(res$parse_status, c("ok", "ok"))
  expect_identical(res$port, c(443L, 12L))
})
