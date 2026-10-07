# RURL-dxwsksor. Seven `ipv4-*` diagnostics existed and no `ipv6-*` one, so an
# SSRF/allowlist guard built on get_url_diagnostics() -- the use the
# url-standard vignette sells -- saw every alternate IPv4 spelling and no
# alternate IPv6 one. curl's security page treats the two as one hazard class.
#
# Two facts close it, both keyed to the IPv6 literal as written and identical
# under both standards, like the `ipv4-*` family:
#   - `ipv6-non-canonical`: the source text is not the WHATWG URL Standard's
#     IPv6 serialization of the address (rurl's own serializer; RFC 5952 §4).
#   - `ipv6-embedded-ipv4`: raddr::addr_embedded_kind() names an embedding.
# rurl holds no address ranges (ADR 0018 D1); the second fact is raddr's,
# projected (ADR 0018, amendment of 2026-10-07).
#
# ADR 0016 evidence. (1) The NULL arm cannot carry this defect:
# get_url_diagnostics() is an error under url_standard = NULL (ADR 0015), and
# the frozen parse surface reports no diagnostics at all; the last test pins
# both. (2) Signature: the fix moves get_url_diagnostics() tokens and
# check_hosts() `reasons` on rows whose host_type is "ipv6", and nothing else;
# the host, host_type and policy verdicts are pinned below. (3) Arms: both
# named arms were wrong in the same way -- each reported no `ipv6-*` token,
# because the vocabulary had none -- and both move identically.

ipv6_tokens <- function(url, url_standard, scheme_acceptance = "web") {
  toks <- get_url_diagnostics(url, url_standard,
    scheme_acceptance = scheme_acceptance
  )
  if (!is.list(toks)) toks <- list(toks)
  lapply(toks, function(t) sort(t[startsWith(t, "ipv6-")]))
}

test_that("the ticket's six alternate spellings are all reported", {
  urls <- c(
    "http://[0:00::0:1]/",
    "http://[::ffff:127.0.0.1]/",
    "http://[::ffff:7f00:1]/",
    "http://[64:ff9b::7f00:1]/",
    "http://[2002:7f00:1::]/",
    "http://[2001:0:0:0:0:0:0:1]/"
  )
  expected <- list(
    "ipv6-non-canonical",
    c("ipv6-embedded-ipv4", "ipv6-non-canonical"),
    "ipv6-embedded-ipv4",
    "ipv6-embedded-ipv4",
    "ipv6-embedded-ipv4",
    c("ipv6-embedded-ipv4", "ipv6-non-canonical")
  )
  for (std in c("whatwg", "rfc3986")) {
    for (acc in c("web", "general")) {
      expect_identical(ipv6_tokens(urls, std, acc), expected,
        label = paste(std, acc)
      )
    }
  }
})

test_that("a canonical literal with no embedding carries no ipv6 fact", {
  urls <- c(
    "http://[::1]/", "http://[2001:db8::1]/", "http://[fe80::1]/",
    "http://[2001:db8:0:1:1:1:1:1]/", "http://[1:0:0:2::3]/",
    "http://[2001:db8::1]:8080/"
  )
  for (std in c("whatwg", "rfc3986")) {
    expect_identical(
      ipv6_tokens(urls, std), rep(list(character(0)), length(urls)),
      label = std
    )
  }
})

test_that("ipv6-non-canonical follows each RFC 5952 section 4 rule", {
  # Each source breaks one rule; the comment names the canonical text.
  urls <- c(
    "http://[2001:0db8::1]/", # 4.1 leading zero -> 2001:db8::1
    "http://[2001:db8:0:0:0:0:0:1]/", # 4.2.1 uncompressed -> 2001:db8::1
    "http://[2001:db8::1:1:1:1:1]/", # 4.2.2 `::` for one field
    "http://[1::2:0:0:0:3]/", # 4.2.3 not the longest run -> 1:0:0:2::3
    "http://[1:0:0:2::3:4]/", # 4.2.3 not the first equal run -> 1::2:0:0:3:4
    "http://[2001:DB8::1]/", # 4.3 uppercase -> 2001:db8::1
    "http://[::ffff:127.0.0.1]/" # dotted tail -> ::ffff:7f00:1
  )
  for (std in c("whatwg", "rfc3986")) {
    got <- ipv6_tokens(urls, std)
    expect_true(
      all(vapply(got, function(t) "ipv6-non-canonical" %in% t, logical(1))),
      label = std
    )
  }
})

test_that("ipv6-non-canonical sees a literal that ends in `::`", {
  # The serializer gave up on a trailing "::" until RURL-sqmhtldq, so every
  # non-canonical literal of this shape was missed. Canonical ones stay quiet.
  noncanonical <- c(
    "[2001:DB8::]", "[2001:0db8::]", "[2001:db8:0:0::]", "[0::]",
    "[1:0::]", "[1:2:3:4:5:6:7::]"
  )
  canonical <- c("[2001:db8::]", "[1::]", "[::]", "[1:2:3:4:5:6::]")
  for (std in c("whatwg", "rfc3986")) {
    got <- ipv6_tokens(paste0("http://", noncanonical, "/"), std)
    expect_true(
      all(vapply(got, function(t) "ipv6-non-canonical" %in% t, logical(1))),
      label = std
    )
    expect_identical(
      ipv6_tokens(paste0("http://", canonical, "/"), std),
      rep(list(character(0)), length(canonical)),
      label = std
    )
  }
})

test_that("ipv6-embedded-ipv4 fires on every raddr embedding kind", {
  # One literal per kind raddr 0.1.2 names; the kinds are raddr's, not rurl's.
  hosts <- c(
    ipv4_mapped = "::ffff:7f00:1",
    ipv4_compatible = "::7f00:1",
    ipv4_translated = "::ffff:0:7f00:1",
    `6to4` = "2002:7f00:1::",
    teredo = "2001:0:4136:e378:8000:63bf:3fff:fdd2",
    nat64_wk = "64:ff9b::7f00:1",
    nat64_local = "64:ff9b:1::a9fe:a9fe",
    isatap = "fe80::5efe:a9fe:a9fe"
  )
  expect_identical(
    as.character(raddr::addr_embedded_kind(raddr::addr_whatwg(hosts))),
    names(hosts)
  )
  urls <- paste0("http://[", hosts, "]/")
  for (std in c("whatwg", "rfc3986")) {
    got <- ipv6_tokens(urls, std)
    expect_true(
      all(vapply(got, function(t) "ipv6-embedded-ipv4" %in% t, logical(1))),
      label = std
    )
  }
})

test_that("ipv6-embedded-ipv4 is exactly raddr's embedding fact", {
  # Completeness: across a sweep of literals, the token is present if and only
  # if raddr names an embedding, so its absence can be relied on. The ISATAP
  # rows are spelled in hex: the rfc3986 arm still rejects their dotted
  # spelling (RURL-escneidz), so it would never reach the diagnostics.
  hosts <- c(
    "::", "::1", "::2", "::ffff:0", "::1:0:0", "::ffff:1:0:0",
    "::ffff:0:0", "::ffff:255.255.255.255", "::0.0.0.1", "::1.2.3.4",
    "64:ff9b::", "64:ff9b::1:2", "64:ff9b:1::", "64:ff9a::1",
    "2002::", "2002:c000:0201::1", "2001::", "2001:1::1", "2001:db8::",
    "fe80::5efe:102:304", "fe80::200:5efe:102:304", "fe80::1",
    "ff02::1", "fc00::1", "1:2:3:4:5:6:7:8"
  )
  kind <- raddr::addr_embedded_kind(raddr::addr_whatwg(hosts))
  urls <- paste0("http://[", hosts, "]/")
  for (std in c("whatwg", "rfc3986")) {
    got <- vapply(
      ipv6_tokens(urls, std),
      function(t) "ipv6-embedded-ipv4" %in% t, logical(1)
    )
    expect_identical(got, !is.na(kind), label = std)
  }
})

test_that("ipv6 facts fire only on IPv6 hosts", {
  urls <- c(
    "http://127.0.0.1/", "http://0x7f.1/", "http://example.com/",
    "http://[fe80::1%25eth0]/", "http://[v1.x]/", "mailto:a@b.example"
  )
  for (std in c("whatwg", "rfc3986")) {
    expect_identical(
      ipv6_tokens(urls, std, "general"),
      rep(list(character(0)), length(urls)),
      label = std
    )
  }
  # A non-special scheme with an IPv6 host reaches the same facts.
  expect_identical(
    ipv6_tokens("foo://[::FFFF:7F00:1]/p", "whatwg", "general"),
    list(c("ipv6-embedded-ipv4", "ipv6-non-canonical"))
  )
})

test_that("check_hosts() reports the ipv6 facts and no verdict moves", {
  urls <- c("http://[0:00::0:1]/", "http://[::ffff:7f00:1]/", "http://[::1]/")
  for (std in c("whatwg", "rfc3986")) {
    df <- check_hosts(urls, url_standard = std)
    expect_identical(df$reasons, list(
      c("ipv6-non-canonical", "ip-literal"),
      c("ipv6-embedded-ipv4", "ip-literal"),
      "ip-literal"
    ), label = std)
    # Signature (ADR 0016): the verdicts are those measured before the fix.
    expect_identical(df$url_valid, rep(TRUE, 3L))
    expect_identical(df$web, rep(TRUE, 3L))
    expect_identical(df$dns, rep(FALSE, 3L))
    expect_identical(df$registrable, rep(FALSE, 3L))
    expect_identical(df$seo, rep(FALSE, 3L))
  }
})

test_that("the parse record and the NULL arm do not move", {
  urls <- c(
    "http://[0:00::0:1]/", "http://[::ffff:127.0.0.1]/",
    "http://[2001:0:0:0:0:0:0:1]/"
  )
  # Hosts as measured before the fix, per arm.
  expect_identical(
    get_host(urls, url_standard = "whatwg"),
    c("[::1]", "[::ffff:7f00:1]", "[2001::1]")
  )
  expect_identical(
    get_host(urls, url_standard = "rfc3986"),
    c("[::1]", "[::ffff:127.0.0.1]", "[2001::1]")
  )
  for (std in c("whatwg", "rfc3986")) {
    expect_identical(get_host_type(urls, std), rep("ipv6", 3L))
  }
  # (1) of the ADR 0016 evidence: no diagnostics surface exists under NULL.
  expect_error(get_url_diagnostics(urls), "url_standard")
  expect_error(get_url_diagnostics(urls, url_standard = NULL), "url_standard")
  expect_identical(
    get_host(urls),
    c("[::1]", "[::ffff:127.0.0.1]", "[2001::1]")
  )
  expect_identical(get_host(urls), get_host(urls, url_standard = NULL))
})
