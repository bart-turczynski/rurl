# PSL parity / regression fixtures for the pslr migration (1.3.0).
#
# Domain and public-suffix extraction now delegate to pslr, which is
# spec-correct for wildcard (`*.`) and exception (`!`) rules and IDN hosts. The
# embedded matcher rurl shipped through 1.2.0 was NOT: its TLD path did
# exact-match only (ignoring wildcards/exceptions) and disagreed with its own
# domain path, and get_domain() returned NA for some Unicode hosts.
#
# These cases lock in the corrected behavior. Each block notes what 1.2.0
# returned so the intentional change is documented at the point of test.

test_that("wildcard rules (*.kobe.jp) are honored for both domain and TLD", {
  # 1.2.0: get_tld -> "kobe.jp" (wildcard ignored).
  expect_identical(unname(get_tld("https://a.b.kobe.jp/x")), "b.kobe.jp")
  expect_identical(
    unname(get_domain("https://a.b.kobe.jp/x")), "a.b.kobe.jp"
  )
})

test_that("exception rules (!www.ck under *.ck) are honored", {
  # 1.2.0: get_tld("www.ck") -> "www.ck"; get_tld("foo.ck") -> "ck".
  expect_identical(unname(get_tld("https://www.ck/x")), "ck")
  expect_identical(unname(get_domain("https://www.ck/x")), "www.ck")

  # foo.ck has no exception, so *.ck makes the whole host a public suffix.
  expect_identical(unname(get_tld("https://foo.ck/x")), "foo.ck")
  expect_true(is.na(unname(get_domain("https://foo.ck/x"))))
})

test_that("IDN hosts resolve a registered domain (Unicode output)", {
  # 1.2.0: get_domain("example.рф", source = "all") -> NA (bug).
  expect_identical(unname(get_domain("https://example.рф/x")), "example.рф")
  expect_identical(unname(get_tld("https://example.рф/x")), "рф")
})

test_that("unknown TLDs yield NA (pslr unknown = 'na', not the implicit *)", {
  expect_true(is.na(unname(get_domain("https://example.madeuptld/x"))))
  expect_true(is.na(unname(get_tld("https://example.madeuptld/x"))))
})

test_that("source selection maps to the pslr section", {
  # github.io is a PRIVATE suffix; io is the ICANN suffix.
  expect_identical(unname(get_tld("https://user.github.io/x")), "github.io")
  expect_identical(
    unname(get_tld("https://user.github.io/x", source = "icann")), "io"
  )
  expect_identical(
    unname(get_tld("https://user.github.io/x", source = "private")),
    "github.io"
  )

  expect_identical(
    unname(get_domain("https://user.github.io/x")), "user.github.io"
  )
  expect_identical(
    unname(get_domain("https://user.github.io/x", source = "icann")),
    "github.io"
  )
})

test_that("domain/TLD spelling follows host_encoding (default keep)", {
  # keep (default) mirrors the input spelling: an A-label host stays A-label.
  expect_identical(
    unname(get_tld("https://example.xn--p1ai/x")), "xn--p1ai"
  )
  expect_identical(
    unname(get_domain("https://shop.example.xn--p1ai/x")), "example.xn--p1ai"
  )
  # unicode decodes; idna forces A-labels.
  expect_identical(
    unname(get_tld("https://example.xn--p1ai/x", host_encoding = "unicode")),
    "рф"
  )
  expect_identical(
    unname(get_domain(
      "https://shop.example.xn--p1ai/x", host_encoding = "unicode"
    )),
    "example.рф"
  )
  # A Unicode host keeps Unicode under keep, and idna re-encodes it.
  expect_identical(unname(get_tld("https://example.рф/x")), "рф")
  expect_identical(
    unname(get_tld("https://example.рф/x", host_encoding = "idna")), "xn--p1ai"
  )
})

test_that("plain ICANN domains are unchanged from 1.2.0", {
  expect_identical(
    unname(get_domain("http://www.example.co.uk/p")), "example.co.uk"
  )
  expect_identical(unname(get_tld("http://www.example.co.uk/p")), "co.uk")
  expect_identical(unname(get_domain("https://example.com")), "example.com")
})
