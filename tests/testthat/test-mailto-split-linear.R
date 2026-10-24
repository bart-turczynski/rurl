# The mailto recipient lexer runs in linear time (RURL-tnlbvcpl).
#
# `.email_units()`, `.email_split_recipients()` and `.email_split_addr_spec()`
# grew their result one element at a time with `c()`, so a recipient of n
# characters cost O(n^2): a 2e4-character local part took about 5 s and a
# 8e4-character one over a minute. These tests pin what the lexer returns on
# edge and long inputs, so a rewrite keeps the output, and bound the time a
# 2e5-character local part takes.

sr <- rurl:::.email_split_recipients
sa <- rurl:::.email_split_addr_spec

test_that("recipient split is pinned on edge inputs", {
  cases <- list(
    list(NA_character_, character(0)),
    list("", character(0)),
    list("a@x.com", "a@x.com"),
    list("a@x.com,b@y.com", c("a@x.com", "b@y.com")),
    # Empty fields are kept, never dropped.
    list("a@x.com,", c("a@x.com", "")),
    list(",a@x.com", c("", "a@x.com")),
    list(",,", c("", "", "")),
    # A raw or encoded quote protects a raw comma.
    list("\"b,c\"@x.org,d@y.org", c("\"b,c\"@x.org", "d@y.org")),
    list("%22b,c%22@x.org,d@y.org", c("%22b,c%22@x.org", "d@y.org")),
    # An encoded comma is data, in either case.
    list("a%2Cb@x.com,c@y", c("a%2Cb@x.com", "c@y")),
    list("a%2cb@x.com", "a%2cb@x.com"),
    # Raw and encoded brackets protect a raw comma.
    list("a@[1,2],b@y", c("a@[1,2]", "b@y")),
    list("a@%5B1,2%5D,b@y", c("a@%5B1,2%5D", "b@y")),
    list("a@%5b1,2%5d,b@y", c("a@%5b1,2%5d", "b@y")),
    # A backslash inside quotes escapes the next unit, raw or encoded.
    list("\"a\\\",b\"@x,c@y", c("\"a\\\",b\"@x", "c@y")),
    list("\"a%5C\",b\"@x,c@y", c("\"a%5C\",b\"@x", "c@y")),
    list("\"a%5c\",b\"@x,c@y", c("\"a%5c\",b\"@x", "c@y")),
    list("\"a\\", "\"a\\"),
    list("\"a\\,", "\"a\\,"),
    # Outside quotes a backslash is ordinary.
    list("a\\,b", c("a\\", "b")),
    list("%5C,a", c("%5C", "a")),
    # Partial and non-hex percent sequences are single-byte units.
    list("%2", "%2"),
    list("%2,", c("%2", "")),
    list("%zz,a", c("%zz", "a")),
    list("%%22a,b%22,c", c("%%22a,b%22", "c")),
    list("%2%22a,b%22,c", c("%2%22a,b%22", "c")),
    # Quote and bracket contexts exclude each other.
    list("\"[a,b\",c", c("\"[a,b\"", "c")),
    list("[\"a,b],c", c("[\"a,b]", "c")),
    # An unclosed context runs to the end.
    list("[a,b,c", "[a,b,c"),
    list("\"a,b,c", "\"a,b,c"),
    list("%22a,b", "%22a,b"),
    list("a]b,c", c("a]b", "c")),
    # Non-ASCII units pass through.
    list("jäne@exämple.com,b@y", c("jäne@exämple.com", "b@y")),
    list("ä,ä%22,ä%22,b", c("ä", "ä%22,ä%22", "b"))
  )
  for (cs in cases) {
    expect_identical(sr(cs[[1L]]), cs[[2L]], info = deparse(cs[[1L]]))
  }
})

test_that("addr-spec split is pinned on edge inputs", {
  bad <- list(ok = FALSE, local = NA_character_, domain = NA_character_)
  cases <- list(
    list("a@b", list(ok = TRUE, local = "a", domain = "b")),
    list("a", bad),
    list("", bad),
    list("a@b@c", bad),
    list("\"a@b\"@c", list(ok = TRUE, local = "\"a@b\"", domain = "c")),
    list("a%40b@c", list(ok = TRUE, local = "a%40b", domain = "c")),
    list("@b", list(ok = TRUE, local = "", domain = "b")),
    list("[a@b]@c", list(ok = TRUE, local = "[a@b]", domain = "c")),
    list("a@[b@c]", list(ok = TRUE, local = "a", domain = "[b@c]")),
    list("\"a\\\"@b\"@c", list(ok = TRUE, local = "\"a\\\"@b\"", domain = "c")),
    list("%22a@b%22@c", list(ok = TRUE, local = "%22a@b%22", domain = "c")),
    list("a@b%40c", list(ok = TRUE, local = "a", domain = "b%40c")),
    list("ä@ä.de", list(ok = TRUE, local = "ä", domain = "ä.de"))
  )
  for (cs in cases) {
    expect_identical(sa(cs[[1L]]), cs[[2L]], info = deparse(cs[[1L]]))
  }
  # A trailing `@` still splits; the empty domain classifies invalid below.
  for (x in c("a@", "@")) {
    got <- sa(x)
    expect_true(got$ok, info = x)
    expect_identical(got$local, sub("@$", "", x), info = x)
  }
})

test_that("recipient facts are pinned on edge inputs", {
  u <- c(
    "mailto:a@example.com,,\"b,c\"@example.org,d%2Ce@example.net,",
    "mailto:%22x,y%22@example.com,[1,2]@h,z@[127.0.0.1]",
    "mailto:a@", "mailto:@b.com", "mailto:a@b@c.com"
  )
  r <- get_mailto_recipients(u, scheme_acceptance = "general")
  expect_identical(r$url, u[c(1, 1, 1, 1, 1, 2, 2, 2, 3, 4, 5)])
  expect_identical(r$recipient_index, c(1:5, 1:3, 1L, 1L, 1L))
  expect_identical(r$mailto_local_part_form, c(
    "dot-atom-text", "invalid", "quoted-string", "invalid", "invalid",
    "quoted-string", "invalid", "dot-atom-text", "dot-atom-text", "invalid",
    "invalid"
  ))
  expect_identical(r$mailto_domain_form, c(
    "ascii-dot-atom-text", "invalid", "ascii-dot-atom-text",
    "ascii-dot-atom-text", "invalid", "ascii-dot-atom-text",
    "ascii-dot-atom-text", "bracketed-domain", "invalid",
    "ascii-dot-atom-text", "invalid"
  ))
  expect_identical(r$smtp_mailbox_rhs_syntax_form, c(
    "domain", "invalid", "domain", "domain", "invalid", "domain", "domain",
    "address-literal", "invalid", "domain", "invalid"
  ))
  expect_identical(r$public_suffix_known,
    c(TRUE, NA, TRUE, TRUE, NA, TRUE, FALSE, NA, NA, TRUE, NA))
})

test_that("long inputs split the same way", {
  local <- strrep("a", 3000L)
  expect_identical(sr(paste0(local, "@example.com")),
    paste0(local, "@example.com"))
  expect_identical(sa(paste0(local, "@example.com")),
    list(ok = TRUE, local = local, domain = "example.com"))

  # Many recipients, with protected and encoded commas mixed in.
  one <- c("a@x.com", "\"b,c\"@y.org", "d%2Ce@z.net", "f@[1,2]", "")
  many <- rep(one, 100L)
  expect_identical(sr(paste(many, collapse = ",")), many)

  # A long run of percent-triplets and quotes.
  enc <- paste(rep("%41%22%2C%22", 500L), collapse = "")
  expect_identical(sr(paste0(enc, ",b")), c(enc, "b"))
})

test_that("a 2e5-character local part is split in linear time", {
  skip_on_cran()
  local <- strrep("a", 2e5)
  u <- paste0("mailto:", local, "@example.com")
  # The quadratic lexer would run for minutes; stop it well before that.
  setTimeLimit(elapsed = 20, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  elapsed <- system.time(
    r <- get_mailto_recipients(u, scheme_acceptance = "general")
  )[["elapsed"]]
  setTimeLimit(elapsed = Inf)
  expect_lt(elapsed, 2)
  expect_identical(nrow(r), 1L)
  expect_identical(r$mailto_local_part_form, "dot-atom-text")
  expect_identical(r$mailto_domain_form, "ascii-dot-atom-text")
})
