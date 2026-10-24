# RURL-vrvxxfvm: resolve_url() and a base the selected standard's parser
# refuses.
#
# This file opens with PINS: behavior that must not move when the refused-base
# rule lands. Valid bases resolve the same under all three selectors, a base
# that is valid under the selected standard but refused by rurl's web
# acceptance policy (`non-spec:/p`) is still a base, an RFC 3986 base with a
# port above 65535 is still a base under "rfc3986" (RFC 3986 section 3.2.3:
# `port = *DIGIT`; the 16-bit ceiling is transport, not syntax), and the
# frozen NULL selector keeps resolving against bases its parser calls an error
# (ADR 0007; ADR 0016 needs a witness and a declared signature to move it).

refs <- c("g", "//o.com/y", "/p", "https://z.com/q")

valid_base <- "http://example.com/a/b"
valid_out <- c(
  "http://example.com/a/g", "http://o.com/y", "http://example.com/p",
  "https://z.com/q"
)

test_that("a valid base resolves every reference kind under every selector", {
  for (s in list(NULL, "whatwg", "rfc3986")) {
    label <- if (is.null(s)) "NULL" else s
    expect_identical(
      resolve_url(refs, valid_base, url_standard = s), valid_out,
      label = paste("clean", label)
    )
    if (!is.null(s)) {
      expect_identical(
        resolve_url(refs, valid_base, url_standard = s, output = "serialized"),
        valid_out,
        label = paste("serialized", label)
      )
    }
  }
})

test_that("a non-ASCII valid base resolves under every selector", {
  base <- "http://example.com/café"
  out <- c(
    "http://example.com/g", "http://o.com/y", "http://example.com/p",
    "https://z.com/q"
  )
  for (s in list(NULL, "whatwg", "rfc3986")) {
    expect_identical(
      resolve_url(refs, base, url_standard = s), out,
      label = if (is.null(s)) "NULL" else s
    )
  }
  expect_identical(
    resolve_url("", base, url_standard = "whatwg", output = "serialized"),
    "http://example.com/caf%C3%A9"
  )
})

test_that("a base the standard accepts is a base even when `web` refuses it", {
  # `non-spec:/p` is a valid URL under both standards; rurl's default
  # scheme_acceptance = "web" refuses it, but that is acceptance policy
  # (ADR 0004), not a standard's parse failure, so it stays a base.
  out <- c("non-spec:/g", "non-spec://o.com/y", "non-spec:/p", "https://z.com/q")
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(
      resolve_url(refs, "non-spec:/p", url_standard = s, output = "serialized"),
      out,
      label = s
    )
  }
})

test_that("rfc3986: a port above 65535 does not refuse the base", {
  base <- "http://example.com:99999/cafe"
  expect_identical(
    resolve_url(refs, base, url_standard = "rfc3986", output = "serialized"),
    c(
      "http://example.com:99999/g", "http://o.com/y",
      "http://example.com:99999/p", "https://z.com/q"
    )
  )
})

test_that("NULL keeps resolving against a base its parser calls an error", {
  # Frozen (ADR 0007). ADR 0016 lets a fix move NULL only with a witness that
  # the default path is defective and a declared signature; this unit has
  # neither, so these rows are pinned as they are.
  null_out <- c(NA, "http://o.com/y", NA, "https://z.com/q")
  for (base in c("http://exa mple.com/a", "http://example.com:99999/cafe")) {
    expect_identical(get_parse_status(base), "error")
    expect_identical(resolve_url(refs, base), null_out, label = base)
    expect_identical(
      resolve_url(refs, base, url_standard = NULL), null_out, label = base
    )
  }
})

test_that("recycling: a scalar base over refs, a base vector over a scalar ref", {
  for (s in list(NULL, "whatwg", "rfc3986")) {
    label <- if (is.null(s)) "NULL" else s
    expect_identical(
      resolve_url(refs, valid_base, url_standard = s), valid_out,
      label = label
    )
    expect_identical(
      resolve_url("g", c(valid_base, "http://h.org/x/y", valid_base),
                  url_standard = s),
      c("http://example.com/a/g", "http://h.org/x/g", "http://example.com/a/g"),
      label = label
    )
  }
  # NULL, mixed vector: the refused base fails only its own relative row.
  expect_identical(
    resolve_url("g", c(valid_base, "http://exa mple.com/a", valid_base)),
    c("http://example.com/a/g", NA, "http://example.com/a/g")
  )
  expect_identical(
    resolve_url("//o.com/y", c(valid_base, "http://exa mple.com/a")),
    c("http://o.com/y", "http://o.com/y")
  )
})

test_that("every WPT base-relative row resolves through the public surface", {
  # test-wpt-base-relative.R scores the INTERNAL `.resolve_one_raw()`, so it
  # cannot see a rule placed in resolve_url() itself. Every WPT base is a
  # valid WHATWG URL, so the public serialized surface must still give the
  # standard's own `href` on all 274 rows.
  skip_if_not_installed("jsonlite")
  rows <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "wpt-url-base-relative.json"),
    simplifyVector = FALSE
  )$success
  field <- function(k) vapply(rows, function(x) x[[k]], character(1))
  got <- resolve_url(
    field("input"), field("base"),
    url_standard = "whatwg", output = "serialized"
  )
  expect_identical(got, field("href"))
})
