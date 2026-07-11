# Practical host validation policy helpers (RURL-clsmspfb;
# PRD _scratch/PRD-host-validation-policy.md). These exercise the POLICY layer:
# the distinction between RFC reg-name validity, DNS owner-name validity,
# web-hostname validity, registrability, and SEO host hygiene -- in both
# url_standard modes -- and prove the layer never changes how a URL parses.

# --- the ticket's acceptance examples, all five rules -----------------------

test_that("check_hosts scores the acceptance-criteria host examples", {
  df <- check_hosts(c(
    "http://example.com",
    "http://_dmarc.example.com",
    "http://a+b.example",
    "http://-example.com",
    "http://example-.com",
    "http://a..com",
    "http://localhost",
    "http://example",
    "http://192.168.0.1",
    "http://[::1]",
    "http://xn--nxasmq6b.example.com"
  ))

  # url-valid: the standard admitted a host for every one of these.
  expect_true(all(df$url_valid))

  # dns owner-name (LDH + underscore): the underscore host passes, the "+",
  # hyphen-hygiene and empty-label hosts fail.
  expect_identical(
    df$dns,
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )

  # web hostname (strict LDH; IP literals count; underscore does NOT).
  expect_identical(
    df$web,
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )

  # registrable == host_type "domain" (a PSL registrable domain).
  expect_identical(
    df$registrable,
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )

  # seo host: web AND registrable AND no footgun.
  expect_identical(
    df$seo,
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

# --- the core new axis: DNS owner name vs web hostname (underscore) ----------

test_that("the underscore is the only difference between dns and web", {
  # _dmarc is a valid DNS owner name but not a web hostname (STD3/LDH).
  expect_true(is_valid_host("http://_dmarc.example.com", rule = "dns"))
  expect_false(is_valid_host("http://_dmarc.example.com", rule = "web"))
  # a "+" is neither: it is not an LDH byte and not a DNS owner-name byte.
  expect_false(is_valid_host("http://a+b.example", rule = "dns"))
  expect_false(is_valid_host("http://a+b.example", rule = "web"))
})

test_that("RFC reg-name validity is broader than dns/web policy", {
  # All of these are accepted reg-names by the standard (url == TRUE) yet fail
  # the practical hostname policies -- the parity-vs-policy distinction.
  reg_ok <- c("http://a+b.example", "http://-example.com", "http://a..com")
  expect_true(all(is_valid_host(reg_ok, rule = "url")))
  expect_false(any(is_valid_host(reg_ok, rule = "web")))
})

# --- registrability and single-label / IP hosts -----------------------------

test_that("registrable requires a PSL domain; localhost and IPs are not", {
  expect_false(is_valid_host("http://localhost", rule = "registrable"))
  expect_false(is_valid_host("http://example", rule = "registrable"))
  expect_false(is_valid_host("http://192.168.0.1", rule = "registrable"))
  expect_true(is_valid_host("http://example.co.uk", rule = "registrable"))
  # ...but localhost is still a usable web hostname.
  expect_true(is_valid_host("http://localhost", rule = "web"))
})

test_that("IP literals are web hosts but never dns/registrable/seo", {
  ips <- c("http://192.168.0.1", "http://[::1]")
  expect_true(all(is_valid_host(ips, rule = "web")))
  expect_false(any(is_valid_host(ips, rule = "dns")))
  expect_false(any(is_valid_host(ips, rule = "registrable")))
  expect_false(any(is_valid_host(ips, rule = "seo")))
})

# --- IDN hosts ---------------------------------------------------------------

test_that("IDN hosts are judged on their A-label (ASCII) form", {
  # Unicode and its A-label spelling agree, and both are web + registrable.
  idns <- c("http://münchen.de", "http://xn--mnchen-3ya.de")
  for (u in idns) {
    expect_true(is_valid_host(u, rule = "web"))
    expect_true(is_valid_host(u, rule = "dns"))
    expect_true(is_valid_host(u, rule = "registrable"))
    expect_true(is_valid_host(u, rule = "seo"))
  }
  # A Unicode reg-name under a non-public suffix is still a web hostname (its
  # A-label form is LDH) but is not registrable.
  expect_true(is_valid_host("http://müller.example", rule = "web"))
  expect_false(is_valid_host("http://müller.example", rule = "registrable"))
})

test_that("a trailing root dot (FQDN) does not fail the hostname rules", {
  expect_true(is_valid_host("http://example.com.", rule = "web"))
  expect_true(is_valid_host("http://example.com.", rule = "registrable"))
})

# --- host_type is (host, standard)-dependent; so is the verdict --------------

test_that("verdict depends on url_standard for a numeric host", {
  # whatwg coerces 2130706433 to an IPv4 host: a web host, not registrable.
  w <- check_hosts("http://2130706433", url_standard = "whatwg")
  expect_identical(w$host_type, "ipv4")
  expect_true(w$web)
  expect_false(w$registrable)
  # rfc3986 keeps it a reg-name: all-digit labels are LDH, so web is TRUE, but
  # it is still not registrable and not seo-safe (numeric-shorthand footgun).
  r <- check_hosts("http://2130706433", url_standard = "rfc3986")
  expect_identical(r$host_type, "reg-name")
  expect_true(r$web)
  expect_false(r$registrable)
  expect_false(r$seo)
})

# --- return shapes -----------------------------------------------------------

test_that("check_hosts returns the documented data.frame shape", {
  df <- check_hosts(c("http://example.com", "http://_dmarc.example.com"))
  expect_s3_class(df, "data.frame")
  # The `url` rule's column is `url_valid` so it never shadows the input `url`.
  expect_named(
    df,
    c("url", "host", "host_type", "url_valid", "dns", "web", "registrable",
      "seo", "reasons")
  )
  expect_identical(df$url, c("http://example.com", "http://_dmarc.example.com"))
  expect_true(all(df$url_valid))
  # reasons is a list-column.
  expect_type(df$reasons, "list")
  expect_length(df$reasons, 2L)
})

test_that("check_hosts scores only the requested rules", {
  df <- check_hosts("http://example.com", rules = c("web", "dns"))
  expect_named(
    df,
    c("url", "host", "host_type", "web", "dns", "reasons")
  )
})

test_that("is_valid_host equals the matching check_hosts column", {
  urls <- c(
    "http://example.com", "http://_dmarc.example.com",
    "http://192.168.0.1", "mailto:x@y.com"
  )
  for (rule in c("url", "dns", "web", "registrable", "seo")) {
    col <- if (rule == "url") "url_valid" else rule
    expect_identical(
      is_valid_host(urls, rule = rule),
      check_hosts(urls, rules = rule)[[col]]
    )
  }
})

# --- NA (no host to judge) contract ------------------------------------------

test_that("rules are NA (never FALSE) when there is no host to judge", {
  # A host-less scheme under the default web acceptance errors; an unparseable
  # string does not parse. Both have no host -> NA.
  df <- check_hosts(c("mailto:jane@example.com", "not a url"))
  expect_true(all(is.na(df$url_valid)))
  expect_true(all(is.na(df$web)))
  expect_length(df$reasons[[1]], 0L) # no host -> no evidence
  expect_length(df$reasons[[2]], 0L)
  expect_true(is.na(is_valid_host("mailto:jane@example.com")))
})

# --- reasons evidence --------------------------------------------------------

test_that("reasons combines host-shape diagnostics with policy tokens", {
  df <- check_hosts(c(
    "http://_dmarc.example.com",
    "http://192.168.0.1",
    "http://2130706433",
    "http://example.com"
  ))
  expect_true(all(
    c("underscore-label", "not-registrable") %in% df$reasons[[1]]
  ))
  expect_true("domain-std3-violation" %in% df$reasons[[1]])
  expect_identical(df$reasons[[2]], "ip-literal")
  expect_true(all(c("ip-literal", "ipv4-number-form") %in% df$reasons[[3]]))
  # A clean registrable domain has nothing to report.
  expect_length(df$reasons[[4]], 0L)
})

# --- policy layer never changes how a URL parses (ADR 0006) ------------------

test_that("host-policy failures do not become parse errors", {
  # -example.com fails every hostname rule but still PARSES (a non-error
  # status): the policy verdict is independent of parse success.
  expect_false(is_valid_host("http://-example.com", rule = "web"))
  expect_false(identical(
    unname(get_parse_status("http://-example.com", url_standard = "whatwg")),
    "error"
  ))
})

# --- input handling ----------------------------------------------------------

test_that("non-character input is rejected with a clear message", {
  expect_error(is_valid_host(42), "must be a character vector")
  expect_error(check_hosts(list("a")), "must be a character vector")
})

test_that("empty input yields an empty result of the right shape", {
  expect_length(is_valid_host(character(0)), 0L)
  df <- check_hosts(character(0))
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 0L)
})
