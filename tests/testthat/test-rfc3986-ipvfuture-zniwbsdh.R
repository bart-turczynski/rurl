# RURL-zniwbsdh: under url_standard = "rfc3986" the web route parses an
# IPvFuture IP-literal. RFC 3986 sec 3.2.2: `IP-literal = "[" ( IPv6address /
# IPvFuture ) "]"`, `IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims
# / ":" )`. The host is stored as written and is an IP literal: no www, PSL,
# domain or TLD, and no IDNA under any `host_encoding`. WHATWG's host parser
# reads a "[" host as IPv6 or fails, so `whatwg` keeps rejecting it.

ipvf_urls <- c("http://[v1.x]/", "http://[v7.aBc]/", "http://[v1.fe80::1]/")
ipvf_hosts <- c("[v1.x]", "[v7.aBc]", "[v1.fe80::1]")
# The parser stores the literal as written (test-parse-web.R), and the general
# route reports it so. The rfc3986 profile's web-route case handling then
# lowercases a bracketed host exactly as it does an IPv6 one (`[AB::CD]` is
# `[ab::cd]` there too); the profile refuses an explicit `case_handling`.
ipvf_lower <- c("[v1.x]", "[v7.abc]", "[v1.fe80::1]")
ipvf_by_route <- list(general = ipvf_hosts, web = ipvf_lower)

test_that("rfc3986 parses an IPvFuture host on the web route, as written", {
  for (acc in c("general", "web")) {
    r <- safe_parse_urls(ipvf_urls, url_standard = "rfc3986",
                         scheme_acceptance = acc)
    expect_identical(r$parse_status, rep("ok", 3L))
    expect_identical(r$host, ipvf_by_route[[acc]])
    expect_identical(r$is_ip_host, rep(TRUE, 3L))
    expect_identical(r$domain, rep(NA_character_, 3L))
    expect_identical(r$tld, rep(NA_character_, 3L))
  }
  expect_identical(get_host(ipvf_urls, url_standard = "rfc3986"), ipvf_lower)
  for (acc in c("general", "web")) {
    expect_identical(
      get_host_type(ipvf_urls, url_standard = "rfc3986",
                    scheme_acceptance = acc),
      rep("ipvfuture", 3L)
    )
  }
  r <- safe_parse_urls("http://[v1.x]:8080/p?q#f", url_standard = "rfc3986")
  expect_identical(
    c(r$parse_status, r$host, as.character(r$port), r$path),
    c("ok", "[v1.x]", "8080", "/p")
  )
})

test_that("an IPvFuture host is kept as written under every host_encoding", {
  # No IDNA or Unicode mapping touches an IP literal: each encoding gives the
  # `keep` answer on both routes.
  for (enc in c("keep", "idna", "unicode")) {
    for (acc in c("general", "web")) {
      expect_identical(
        safe_parse_urls(ipvf_urls, url_standard = "rfc3986",
                        scheme_acceptance = acc, host_encoding = enc)$host,
        ipvf_by_route[[acc]]
      )
    }
    expect_identical(
      get_host(ipvf_urls, url_standard = "rfc3986", host_encoding = enc),
      ipvf_lower
    )
  }
})

test_that("the scalar orchestrator agrees with the vector path", {
  scalar_host <- function(x, std) {
    r <- rurl:::._safe_parse_url_impl(
      url = x, protocol_handling = "keep", www_handling = "none",
      tld_source = "all", case_handling = "keep",
      trailing_slash_handling = "none", index_page_handling = "keep",
      path_normalization = "none", scheme_relative_handling = "keep",
      subdomain_levels_to_keep = NULL, host_encoding = "idna",
      url_standard = std
    )
    if (is.null(r)) NA_character_ else c(r$host, r$is_ip_host)
  }
  for (i in seq_along(ipvf_urls)) {
    expect_identical(
      scalar_host(ipvf_urls[i], "rfc3986"), c(ipvf_hosts[i], "TRUE")
    )
    expect_identical(scalar_host(ipvf_urls[i], "whatwg"), NA_character_)
  }
})

test_that("whatwg still rejects an IPvFuture host; the controls hold", {
  for (acc in c("general", "web")) {
    expect_identical(
      safe_parse_urls(ipvf_urls, url_standard = "whatwg",
                      scheme_acceptance = acc)$parse_status,
      rep("error", 3L)
    )
  }
  for (std in c("rfc3986", "whatwg")) {
    r <- safe_parse_urls("http://[::1]/", url_standard = std)
    expect_identical(c(r$parse_status, r$host), c("ok", "[::1]"))
    expect_true(r$is_ip_host)
  }
  r <- safe_parse_urls("foo://[v1.x]/", url_standard = "rfc3986",
                       scheme_acceptance = "general")
  expect_identical(c(r$parse_status, r$host), c("ok", "[v1.x]"))
  # ...and is now an IP literal of the ipvfuture type, not a reg-name.
  expect_true(r$is_ip_host)
  expect_identical(
    get_host_type("foo://[v1.x]/", url_standard = "rfc3986",
                  scheme_acceptance = "general"),
    "ipvfuture"
  )
})

test_that("no ipv6-* diagnostic fires on an IPvFuture literal", {
  d <- get_url_diagnostics(ipvf_urls, url_standard = "rfc3986")
  expect_false(any(grepl("^ipv6-", unlist(d))))
  expect_identical(lengths(d), c(0L, 0L, 0L))
  # The bite: the ipv6-* emitter is gated on this internal host_type, which
  # must not read the colon-bearing `[v1.fe80::1]` as "ipv6".
  meta <- rurl:::._url_metadata_vec(
    ipvf_urls, rurl:::.parse_options(url_standard = "rfc3986")
  )
  expect_identical(meta$host_type, rep("ipvfuture", 3L))
})

test_that("a scheme-less or scheme-relative IPvFuture host parses too", {
  # The strict IP detector makes the bracketed literal host-shaped, so the
  # input gets the usual scheme inference; whatwg fails it, as on main.
  u <- c("[v1.x]", "//[v1.x]/")
  r <- safe_parse_urls(u, url_standard = "rfc3986")
  expect_identical(r$parse_status, c("ok", "ok-scheme-relative"))
  expect_identical(r$host, c("[v1.x]", "[v1.x]"))
  expect_identical(
    safe_parse_urls(u, url_standard = "whatwg")$parse_status,
    c("error", "error")
  )
})

test_that("host policy judges an IPvFuture host as an IP literal", {
  expect_identical(
    is_valid_host(ipvf_urls, url_standard = "rfc3986"), rep(TRUE, 3L)
  )
  expect_identical(
    is_valid_host(ipvf_urls, rule = "url", url_standard = "rfc3986"),
    rep(TRUE, 3L)
  )
  ch <- check_hosts(c(ipvf_urls, "foo://[v1.x]/"), url_standard = "rfc3986",
                    scheme_acceptance = "general")
  expect_identical(ch$host_type, rep("ipvfuture", 4L))
  expect_identical(ch$web, rep(TRUE, 4L))
  expect_identical(ch$registrable, rep(FALSE, 4L))
  expect_true(all(vapply(ch$reasons, identical, logical(1), "ip-literal")))
})

test_that("an input outside the IPvFuture production stays an error", {
  bad <- c("http://[v1]/", "http://[v.x]/", "http://[vZ.x]/",
           "http://[v1.]/", "http://[v1.x%41]/", "http://[]/",
           "http://[v1.é]/", NA)
  expect_identical(
    safe_parse_urls(bad, url_standard = "rfc3986")$parse_status,
    rep("error", length(bad))
  )
  # A latin1-marked byte inside the brackets is not in the production.
  latin1 <- "http://[v1.\xe9]/"
  Encoding(latin1) <- "latin1"
  expect_identical(
    safe_parse_urls(latin1, url_standard = "rfc3986")$parse_status, "error"
  )
  # A raw non-ASCII byte in the PATH is an rfc3986 error whatever the host
  # (no `pchar` holds it); its triplet spelling parses.
  expect_identical(
    safe_parse_urls("http://[v1.x]/p%E9", url_standard = "rfc3986")$host,
    "[v1.x]"
  )
})
