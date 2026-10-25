# RURL-fobwctml: under `url_standard = "whatwg"` the www and subdomain
# policies police the host the WHATWG host parser defines, which is the host
# AFTER domain to ASCII (UTS #46 processing step 1, "Map") has removed the
# code points UTS-46 ignores: U+200B (zero-width space), U+00AD (soft hyphen)
# and U+FEFF (zero-width no-break space). RFC 3986 has no mapping step, so the
# `rfc3986` and `NULL` arms keep the code point as data.
#
# Every source here spells the marks as `\u` escapes, never as raw invisible
# characters.

marks <- c(zwsp = "\u200b", shy = "\u00ad", bom = "\ufeff")

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
  # label, a fullwidth letter outside the www label): the policies already
  # read them the WHATWG way, and the fix must not move them. A www label
  # UTS-46 maps to `www` does move; see the next test.
  urls <- c("http://www.b\u00fccher.de/", "http://B\u00dcCHER.de/",
            "http://www.xn--bcher-kva.de/", "http://a.b.b\u00fccher.de/",
            "http://www.\uff45xample.com/")
  host <- function(policy) policy_parse(urls, "whatwg", policy)$host
  expect_identical(host("none"), c(
    "www.b\u00fccher.de", "b\u00fccher.de", "www.xn--bcher-kva.de",
    "a.b.b\u00fccher.de", "www.example.com"
  ))
  expect_identical(host("strip"), c(
    "b\u00fccher.de", "b\u00fccher.de", "xn--bcher-kva.de",
    "a.b.b\u00fccher.de", "example.com"
  ))
  expect_identical(host("keep"), c(
    "www.b\u00fccher.de", "www.b\u00fccher.de", "www.xn--bcher-kva.de",
    "www.a.b.b\u00fccher.de", "www.example.com"
  ))
  expect_identical(host("if_no_subdomain"), c(
    "www.b\u00fccher.de", "www.b\u00fccher.de", "www.xn--bcher-kva.de",
    "a.b.b\u00fccher.de", "www.example.com"
  ))
  expect_identical(host("sub0"), c(
    "www.b\u00fccher.de", "b\u00fccher.de", "www.xn--bcher-kva.de",
    "b\u00fccher.de", "www.example.com"
  ))
  expect_identical(host("sub1"), c(
    "www.b\u00fccher.de", "b\u00fccher.de", "www.xn--bcher-kva.de",
    "b.b\u00fccher.de", "www.example.com"
  ))
  expect_identical(
    policy_parse(urls, "whatwg", "strip")$domain,
    c("b\u00fccher.de", "b\u00fccher.de", "xn--bcher-kva.de",
      "b\u00fccher.de", "example.com")
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
  # rfc3986 no longer reaches the policies: the raw mark is not RFC 3986
  # syntax (sections 2, 2.5, 3.2.2), so every row is a parse error with no
  # host (RUL-030, owner ruling 2026-10-12, RURL-pntbujzc). NULL is frozen.
  for (mk in names(marks)) {
    m <- marks[[mk]]
    urls <- paste0("http://", m, policy_hosts, "/")
    for (posture in c("NULL", "NULL-explicit")) {
      for (policy in names(policies)) {
        want <- gsub("{m}", m, expected[[policy]], fixed = TRUE)
        p <- policy_parse(urls, posture, policy)
        lab <- paste(mk, posture, policy)
        expect_identical(p$host, want, label = lab)
        expect_identical(p$clean_url, paste0("http://", want, "/"), label = lab)
        expect_identical(p$parse_status, rep("ok", length(urls)), label = lab)
      }
    }
    for (policy in names(policies)) {
      p <- policy_parse(urls, "rfc3986", policy)
      lab <- paste(mk, "rfc3986", policy)
      expect_identical(p$host, rep(NA_character_, length(urls)), label = lab)
      expect_identical(p$clean_url, rep(NA_character_, length(urls)),
                       label = lab)
      expect_identical(p$parse_status, rep("error", length(urls)),
                       label = lab)
    }
  }
})

# --- The fix: the policies read the mapped host ------------------------------

test_that("whatwg: an ignorable-led host gets its twin's policy output", {
  # The policies used to read the host before domain to ASCII: `\u200bwww.com`
  # with `if_no_subdomain` became `www.www.com`, `strip` and `keep` never saw
  # the `www.` prefix, and the trim counted `\u200bwww` as a subdomain label.
  # Every column but the input matches the twin's, under every www mode and
  # both trim levels, for the url_standard and the profile.
  twin_urls <- paste0("http://", policy_hosts, "/")
  for (posture in c("whatwg", "whatwg-profile")) {
    for (policy in names(policies)) {
      twin <- policy_parse(twin_urls, posture, policy)
      twin$original_url <- NULL
      for (mk in names(marks)) {
        p <- policy_parse(paste0("http://", marks[[mk]], policy_hosts, "/"),
                          posture, policy)
        p$original_url <- NULL
        expect_identical(p, twin, label = paste(mk, posture, policy))
      }
    }
  }
})

test_that("whatwg: a mixed ACE host trims to the spelling of its domain", {
  # The `keep` rendering of a host that carries an ACE label is ToASCII of
  # the whole host, which is the spelling `domain` takes, so that host and
  # domain agree (`.whatwg_host_presentation_vec()`, RUL-002). The trim used
  # to drop the ACE label from the source spelling and leave the Unicode
  # `<U+043F><U+0440>.com`, a spelling neither column holds; it now trims the
  # mapped host. `www_handling = "keep"` puts `www.` back in front.
  urls <- c("https://xn--bcher-kva.\u043f\u0440.com/",
            "http://xn--bcher-kva.m\u00fcnchen.de/")
  for (posture in c("whatwg", "whatwg-profile")) {
    whole <- policy_parse(urls, posture, "none")
    expect_identical(whole$host, c("xn--bcher-kva.xn--o1ac.com",
                                   "xn--bcher-kva.xn--mnchen-3ya.de"),
                     label = posture)
    for (www in c("none", "strip", "keep", "if_no_subdomain")) {
      p <- if (posture == "whatwg") {
        safe_parse_urls(urls, url_standard = "whatwg", www_handling = www,
                        subdomain_levels_to_keep = 0)
      } else {
        safe_parse_urls(urls, profile = "whatwg", www_handling = www,
                        subdomain_levels_to_keep = 0)
      }
      lab <- paste(posture, www)
      trimmed <- c("xn--o1ac.com", "xn--mnchen-3ya.de")
      expect_identical(p$domain, trimmed, label = lab)
      expect_identical(
        p$host, if (www == "keep") paste0("www.", trimmed) else trimmed,
        label = lab
      )
      expect_true(all(endsWith(whole$host, paste0(".", trimmed))), label = lab)
    }
    expect_identical(policy_parse(urls, posture, "sub1")$host, whole$host,
                     label = posture)
  }
  # NULL reads the host as written and does not move. rfc3986 rejects the raw
  # non-ASCII label (RFC 3986 sections 2, 2.5, 3.2.2; RUL-030, owner ruling
  # 2026-10-12, RURL-pntbujzc).
  for (posture in c("NULL", "NULL-explicit")) {
    expect_identical(policy_parse(urls, posture, "sub0")$host,
                     c("\u043f\u0440.com", "m\u00fcnchen.de"), label = posture)
  }
  expect_identical(policy_parse(urls, "rfc3986", "sub0")$host,
                   c(NA_character_, NA_character_))
})

test_that("whatwg: pure ACE and pure Unicode hosts keep their spelling", {
  urls <- c("http://www.xn--bcher-kva.de/", "http://a.b.xn--bcher-kva.de/",
            "http://www.b\u00fccher.de/", "http://a.b.b\u00fccher.de/")
  for (posture in c("whatwg", "whatwg-profile")) {
    expect_identical(policy_parse(urls, posture, "sub0")$host, c(
      "www.xn--bcher-kva.de", "xn--bcher-kva.de", "www.b\u00fccher.de",
      "b\u00fccher.de"
    ), label = posture)
    expect_identical(policy_parse(urls, posture, "strip")$host, c(
      "xn--bcher-kva.de", "a.b.xn--bcher-kva.de", "b\u00fccher.de",
      "a.b.b\u00fccher.de"
    ), label = posture)
  }
})

test_that("whatwg: a first label UTS-46 maps to www is policed as www", {
  # Fullwidth, circled, a mark inside the label, a percent-encoded mark: each
  # maps to `www`, so each gets its plain `www.example.com` twin's output under
  # every policy, as the WHATWG host parser records the same host.
  marked <- c("http://ＷＷＷ.example.com/",
              "http://ⓦⓦⓦ.example.com/",
              "http://w​ww.example.com/",
              "http://%E2%80%8Bwww.example.com/")
  twin <- "http://www.example.com/"
  for (posture in c("whatwg", "whatwg-profile")) {
    for (policy in names(policies)) {
      want <- policy_parse(twin, posture, policy)$host
      got <- policy_parse(marked, posture, policy)$host
      expect_identical(got, rep(want, length(marked)),
        label = paste(posture, policy))
    }
  }
  # rfc3986 has no mapping step, and the raw fullwidth label is not RFC 3986
  # syntax (sections 2, 2.5, 3.2.2): a parse error, never policed as www
  # (RUL-030, owner ruling 2026-10-12, RURL-pntbujzc). The percent-encoded
  # mark is RFC syntax and, unmapped, is not www either.
  expect_identical(
    policy_parse(marked[[1]], "rfc3986", "strip")$host,
    NA_character_
  )
  expect_identical(
    policy_parse(marked[[4]], "rfc3986", "strip")$host,
    "%E2%80%8Bwww.example.com"
  )
})

test_that("whatwg: the scalar engine polices the mapped host too", {
  scalar <- function(www, levels) {
    rurl:::._safe_parse_url_impl(
      url = "http://​www.example.com/", protocol_handling = "keep",
      www_handling = www, tld_source = "all", case_handling = "keep",
      trailing_slash_handling = "none", index_page_handling = "keep",
      path_normalization = "none", scheme_relative_handling = "keep",
      subdomain_levels_to_keep = levels, host_encoding = "keep",
      path_encoding = "keep", url_standard = "whatwg"
    )$host
  }
  expect_identical(scalar("strip", NULL), "example.com")
  expect_identical(scalar("keep", NULL), "www.example.com")
  expect_identical(scalar("none", 0), "www.example.com")
})
