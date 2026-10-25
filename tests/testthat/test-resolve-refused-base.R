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
  out <- c(
    "non-spec:/g", "non-spec://o.com/y", "non-spec:/p", "https://z.com/q"
  )
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

test_that("recycling: scalar base over refs, base vector over scalar ref", {
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

# ---- the rule (RURL-vrvxxfvm) ------------------------------------------------
#
# A base the selected standard's parser refuses is no base.
#
#   * "whatwg": the URL Standard's API URL parser (used by `new URL(url,
#     base)`) parses the base first and returns failure when that fails,
#     before it reads the input, so every row with such a base is NA -- an
#     absolute reference included.
#   * "rfc3986": section 5.1 requires a base URI to conform to <absolute-URI>,
#     and section 5.2.2 reads the base only when the reference has no scheme,
#     so a relative reference is NA and an absolute one still resolves.
#
# "Refuses" means the standard's own parser, `serialize_url()` NA -- not
# get_parse_status(), whose default `web` acceptance refuses valid URLs such
# as `non-spec:/p` (pinned above).

ws_bad <- "http://exa mple.com/a" # U+0020 is a forbidden host code point
port_bad <- "http://example.com:99999/cafe" # WHATWG: port > 65535 fails
frag_bad <- "https://example.org/##link" # RFC 3986: `#` is not in fragment

test_that("whatwg: a refused base makes every row NA, absolute included", {
  for (base in c(ws_bad, port_bad, "http://exa mple.com/café")) {
    expect_true(is.na(serialize_url(base, standard = "whatwg")))
    expect_identical(
      resolve_url(refs, base, url_standard = "whatwg"),
      rep(NA_character_, 4L),
      label = base
    )
    expect_identical(
      resolve_url(refs, base, url_standard = "whatwg", output = "serialized"),
      rep(NA_character_, 4L),
      label = base
    )
  }
})

test_that("rfc3986: a refused base fails relative rows, not absolute ones", {
  out <- c(NA, NA, NA, "https://z.com/q")
  for (base in c(ws_bad, frag_bad, "http://exa mple.com/café")) {
    expect_true(is.na(serialize_url(base, standard = "rfc3986")))
    expect_identical(
      resolve_url(refs, base, url_standard = "rfc3986"), out, label = base
    )
    expect_identical(
      resolve_url(refs, base, url_standard = "rfc3986", output = "serialized"),
      out,
      label = base
    )
  }
  for (r in c("?q", "#f", "")) {
    expect_identical(
      resolve_url(r, ws_bad, url_standard = "rfc3986", output = "serialized"),
      NA_character_,
      label = r
    )
  }
})

test_that("the standards disagree on the base, so the rows do", {
  # `##link` is a WHATWG base (the fragment state keeps the second `#` as a
  # fragment code point) and WPT resolves `#link` against it; port 99999 is an
  # RFC 3986 base (`port = *DIGIT`). Each stays a base where its standard says
  # so.
  expect_identical(
    resolve_url("#link", frag_bad, url_standard = "whatwg",
                output = "serialized"),
    "https://example.org/#link"
  )
  expect_identical(
    resolve_url("g", port_bad, url_standard = "rfc3986",
                output = "serialized"),
    "http://example.com:99999/g"
  )
  expect_identical(
    resolve_url("g", port_bad, url_standard = "whatwg", output = "serialized"),
    NA_character_
  )
})

test_that("a refused base fails its own row only (vector base)", {
  bases <- c(valid_base, ws_bad, valid_base, port_bad)
  expect_identical(
    resolve_url("g", bases, url_standard = "whatwg"),
    c("http://example.com/a/g", NA, "http://example.com/a/g", NA)
  )
  expect_identical(
    resolve_url("https://z.com/q", bases, url_standard = "whatwg"),
    c("https://z.com/q", NA, "https://z.com/q", NA)
  )
  expect_identical(
    resolve_url("g", bases, url_standard = "rfc3986"),
    c("http://example.com/a/g", NA, "http://example.com/a/g",
      "http://example.com/g")
  )
  expect_identical(
    resolve_url("https://z.com/q", bases, url_standard = "rfc3986"),
    rep("https://z.com/q", 4L)
  )
  # Paired vectors: each row is judged on its own base.
  expect_identical(
    resolve_url(refs, c(ws_bad, valid_base, valid_base, ws_bad),
                url_standard = "rfc3986"),
    c(NA, "http://o.com/y", "http://example.com/p", "https://z.com/q")
  )
  expect_identical(
    resolve_url(refs, c(ws_bad, valid_base, valid_base, ws_bad),
                url_standard = "whatwg"),
    c(NA, "http://o.com/y", "http://example.com/p", NA)
  )
})

test_that("a recycled scalar refused base fails every row it reaches", {
  expect_identical(
    resolve_url(refs, ws_bad, url_standard = "whatwg"), rep(NA_character_, 4L)
  )
  expect_identical(
    resolve_url(refs, ws_bad, url_standard = "rfc3986"),
    c(NA, NA, NA, "https://z.com/q")
  )
  # ...and a scalar valid base over the same refs is untouched.
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(
      resolve_url(refs, valid_base, url_standard = s), valid_out, label = s
    )
  }
})

test_that("an NA base is no base, not a failing one", {
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(
      resolve_url(c("https://z.com/q", "g"), NA_character_, url_standard = s),
      c("https://z.com/q", NA),
      label = s
    )
  }
})

test_that("`...` acceptance options do not change which base is refused", {
  for (s in c("whatwg", "rfc3986")) {
    expect_identical(
      resolve_url(refs, ws_bad, url_standard = s,
                  scheme_acceptance = "general"),
      resolve_url(refs, ws_bad, url_standard = s),
      label = s
    )
  }
})

test_that("each distinct base is parsed once, not once per row", {
  seen <- new.env()
  seen$calls <- list()
  real <- serialize_url
  local_mocked_bindings(serialize_url = function(url, ...) {
    seen$calls[[length(seen$calls) + 1L]] <- url
    real(url, ...)
  })
  bases <- rep_len(c(valid_base, ws_bad), 1000L)
  resolve_url(rep_len(c("g", "https://z.com/q"), 1000L), bases,
              url_standard = "whatwg")
  expect_length(seen$calls, 1L)
  expect_identical(seen$calls[[1L]], c(valid_base, ws_bad))
})

test_that("whatwg: an empty or scheme-less base fails, so an absolute ref is NA", {
  # `new URL("https://z.com/q", "")` throws: the base is parsed first and an
  # empty or scheme-less string fails the basic URL parser. An NA base is no
  # base, so the absolute reference still resolves against it.
  for (b in c("", "example.com", "/a", "//h/p")) {
    expect_identical(
      resolve_url("https://z.com/q", b, url_standard = "whatwg"),
      NA_character_, label = b
    )
  }
  expect_identical(
    resolve_url("https://z.com/q", NA_character_, url_standard = "whatwg"),
    "https://z.com/q"
  )
  expect_identical(
    resolve_url("https://z.com/q", "", url_standard = "rfc3986"),
    "https://z.com/q"
  )
})
