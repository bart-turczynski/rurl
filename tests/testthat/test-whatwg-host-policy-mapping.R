# RURL-fobwctml: under `url_standard = "whatwg"` the www and subdomain
# policies police the host the WHATWG host parser defines, which is the host
# AFTER domain to ASCII (UTS #46 processing step 1, "Map") has removed the
# code points UTS-46 ignores: U+200B (zero-width space), U+00AD (soft hyphen)
# and U+FEFF (zero-width no-break space). RFC 3986 has no mapping step, so the
# `rfc3986` and `NULL` arms keep the code point as data.
#
# Every source here spells the marks as `\u` escapes, never as raw invisible
# characters.

marks <- c(zwsp = "​", shy = "­", bom = "﻿")

# The hosts and the policies each test walks: every www_handling value and two
# subdomain levels.
policy_hosts <- c("www.com", "www.example.com", "www.a.example.com",
                  "example.com", "a.b.example.com", "WWW.Example.com")
policies <- list(
  none = list(www_handling = "none"),
  strip = list(www_handling = "strip"),
  keep = list(www_handling = "keep"),
  if_no_subdomain = list(www_handling = "if_no_subdomain"),
  sub0 = list(subdomain_levels_to_keep = 0),
  sub1 = list(subdomain_levels_to_keep = 1)
)

# One parse of `url` under `posture` with the named policy's options.
policy_parse <- function(url, posture, policy) {
  args <- c(list(url), policies[[policy]])
  if (identical(posture, "whatwg")) {
    args$url_standard <- "whatwg"
  } else if (identical(posture, "whatwg-profile")) {
    args$profile <- "whatwg"
  } else if (identical(posture, "rfc3986")) {
    args$url_standard <- "rfc3986"
  } else if (identical(posture, "NULL-explicit")) {
    args["url_standard"] <- list(NULL)
  }
  do.call(safe_parse_urls, args)
}

# --- Pins: what the fix must not move ----------------------------------------

test_that("whatwg: the mapped twins keep their policy output", {
  # The host each policy gives the twin with no mark, in `policy_hosts` order.
  # These are the values the mark-led rows must reach; the twins themselves
  # must not move.
  expected <- list(
    none = c("www.com", "www.example.com", "www.a.example.com",
             "example.com", "a.b.example.com", "www.example.com"),
    strip = c("com", "example.com", "a.example.com", "example.com",
              "a.b.example.com", "example.com"),
    keep = c("www.com", "www.example.com", "www.a.example.com",
             "www.example.com", "www.a.b.example.com", "www.example.com"),
    if_no_subdomain = c("www.com", "www.example.com", "www.a.example.com",
                        "www.example.com", "a.b.example.com",
                        "www.example.com"),
    sub0 = c("www.com", "www.example.com", "www.example.com", "example.com",
             "example.com", "www.example.com"),
    sub1 = c("www.com", "www.example.com", "www.a.example.com",
             "example.com", "b.example.com", "www.example.com")
  )
  urls <- paste0("http://", policy_hosts, "/")
  for (posture in c("whatwg", "whatwg-profile")) {
    for (policy in names(policies)) {
      p <- policy_parse(urls, posture, policy)
      lab <- paste(posture, policy)
      expect_identical(p$host, expected[[policy]], label = lab)
      expect_identical(p$clean_url, paste0("http://", expected[[policy]], "/"),
                       label = lab)
      # Stripping `www.` from `www.com` leaves the bare suffix `com`.
      status <- rep("ok", length(urls))
      status[expected[[policy]] == "com"] <- "warning-no-tld"
      expect_identical(p$parse_status, status, label = lab)
    }
  }
})

test_that("whatwg: IDN and ACE twins keep their policy output", {
  # Hosts UTS-46 maps without an ignorable code point (case, an IDN, an ACE
  # label, a fullwidth letter): the policies already read them the WHATWG
  # way, and the fix must not move them.
  urls <- c("http://www.bücher.de/", "http://BÜCHER.de/",
            "http://www.xn--bcher-kva.de/", "http://a.b.bücher.de/",
            "http://www.ｅxample.com/")
  host <- function(policy) policy_parse(urls, "whatwg", policy)$host
  expect_identical(host("none"), c(
    "www.bücher.de", "bücher.de", "www.xn--bcher-kva.de",
    "a.b.bücher.de", "www.example.com"
  ))
  expect_identical(host("strip"), c(
    "bücher.de", "bücher.de", "xn--bcher-kva.de",
    "a.b.bücher.de", "example.com"
  ))
  expect_identical(host("keep"), c(
    "www.bücher.de", "www.bücher.de", "www.xn--bcher-kva.de",
    "www.a.b.bücher.de", "www.example.com"
  ))
  expect_identical(host("if_no_subdomain"), c(
    "www.bücher.de", "www.bücher.de", "www.xn--bcher-kva.de",
    "a.b.bücher.de", "www.example.com"
  ))
  expect_identical(host("sub0"), c(
    "www.bücher.de", "bücher.de", "www.xn--bcher-kva.de",
    "bücher.de", "www.example.com"
  ))
  expect_identical(host("sub1"), c(
    "www.bücher.de", "bücher.de", "www.xn--bcher-kva.de",
    "b.bücher.de", "www.example.com"
  ))
  expect_identical(
    policy_parse(urls, "whatwg", "strip")$domain,
    c("bücher.de", "bücher.de", "xn--bcher-kva.de",
      "bücher.de", "example.com")
  )
})

test_that("rfc3986 and NULL: an ignorable-led host keeps the code point", {
  # RFC 3986 has no mapping step: the mark is data in the first label, so the
  # www prefix never matches and the mark-led label counts as a subdomain
  # label. "{m}" stands for the mark. Both arms lowercase the host here.
  expected <- list(
    none = c("{m}www.com", "{m}www.example.com", "{m}www.a.example.com",
             "{m}example.com", "{m}a.b.example.com", "{m}www.example.com"),
    strip = c("{m}www.com", "{m}www.example.com", "{m}www.a.example.com",
              "{m}example.com", "{m}a.b.example.com", "{m}www.example.com"),
    keep = c("www.{m}www.com", "www.{m}www.example.com",
             "www.{m}www.a.example.com", "www.{m}example.com",
             "www.{m}a.b.example.com", "www.{m}www.example.com"),
    if_no_subdomain = c("www.{m}www.com", "{m}www.example.com",
                        "{m}www.a.example.com", "www.{m}example.com",
                        "{m}a.b.example.com", "{m}www.example.com"),
    sub0 = c("{m}www.com", "example.com", "example.com", "{m}example.com",
             "example.com", "example.com"),
    sub1 = c("{m}www.com", "{m}www.example.com", "a.example.com",
             "{m}example.com", "b.example.com", "{m}www.example.com")
  )
  for (mk in names(marks)) {
    m <- marks[[mk]]
    urls <- paste0("http://", m, policy_hosts, "/")
    for (posture in c("rfc3986", "NULL", "NULL-explicit")) {
      for (policy in names(policies)) {
        want <- gsub("{m}", m, expected[[policy]], fixed = TRUE)
        p <- policy_parse(urls, posture, policy)
        lab <- paste(mk, posture, policy)
        expect_identical(p$host, want, label = lab)
        expect_identical(p$clean_url, paste0("http://", want, "/"), label = lab)
        expect_identical(p$parse_status, rep("ok", length(urls)), label = lab)
      }
    }
  }
})
