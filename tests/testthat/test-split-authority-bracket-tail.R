# Text after the `]` of a bracketed host in a non-special authority
# (RURL-rrqzdahp). `.split_authority()` cut a hostport led by "[" at its first
# "]" and kept the tail only when it was a `:port`. The WHATWG host state
# instead hands the whole buffer -- up to a `:` outside the brackets, `/`, `?`,
# `#` or the end -- to the host parser, whose IPv6 branch fails an input that
# does not end with "]" (IPv6-unclosed), or, as `[::1]]` and `[::1][::2]` do,
# holds a code point the IPv6 parser rejects (WHATWG URL Standard, host state;
# host parsing, IPv6 parser). Every WHATWG expectation below was read from
# Node v26.3.1's WHATWG `URL` on 2026-10-10 (`new URL(input).href`, or a
# thrown ERR_INVALID_URL).

# The posture recipes of design/posture-card.md.
.sabt_rfc_args <- list(
  rfc_syntax = list(profile = "rfc-syntax"),
  rfc_general = list(
    url_standard = "rfc3986", scheme_policy = "require",
    scheme_acceptance = "general"
  )
)

test_that("whatwg: a bracketed host with no tail or a :port tail parses", {
  cases <- data.frame(
    input = c(
      "foo://[::1]/", "foo://[::1]", "foo://[::1]:80/", "foo://[::1]:/",
      "foo://[::1]?q", "foo://[::1]#f", "foo://u@[::1]/p",
      "foo://[::1]/p?q#f", "http://[::1]\\x"
    ),
    href = c(
      "foo://[::1]/", "foo://[::1]", "foo://[::1]:80/", "foo://[::1]/",
      "foo://[::1]?q", "foo://[::1]#f", "foo://u@[::1]/p",
      "foo://[::1]/p?q#f", "http://[::1]/x"
    ),
    port = c(NA, NA, 80L, NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  for (args in list(
    list(profile = "whatwg"),
    list(url_standard = "whatwg", scheme_acceptance = "general")
  )) {
    lab <- paste(names(args), unlist(args), collapse = ", ")
    p <- do.call(safe_parse_urls, c(list(cases$input), args))
    expect_identical(p$parse_status, rep("ok", nrow(cases)), info = lab)
    expect_identical(p$host, rep("[::1]", nrow(cases)), info = lab)
    expect_identical(p$port, cases$port, info = lab)
    expect_true(all(p$is_ip_host), info = lab)
  }
  expect_identical(serialize_url(cases$input, standard = "whatwg"), cases$href)
  expect_identical(
    get_host_type(
      cases$input, url_standard = "whatwg", scheme_acceptance = "general"
    ),
    rep("ipv6", nrow(cases))
  )
})

test_that("whatwg: bracketed hosts that already fail stay failed", {
  # A special scheme runs the web host parser, which already failed these.
  # The foo: rows fail inside the IPv6 parser (`[1:]`, `[v1.x]` -- WHATWG has
  # no IPvFuture), on a missing "]" (`[::1`), or in the port state (`::80`).
  urls <- c(
    "http://[::1]x/", "http://[::1]]/", "foo://[1:]/", "foo://[::1/",
    "foo://[v1.x]/", "foo://[::1]::80/"
  )
  p <- safe_parse_urls(urls, profile = "whatwg")
  expect_identical(p$parse_status, rep("error", length(urls)))
  expect_true(all(is.na(p$host)))
  expect_true(all(is.na(p$clean_url)))
  expect_identical(
    serialize_url(urls, standard = "whatwg"), rep(NA_character_, length(urls))
  )
})

test_that("rfc3986: a tail after the IP-literal is a grammar failure", {
  # RFC 3986 sec 3.2.2: `host = IP-literal / IPv4address / reg-name`, and an
  # IP-literal is followed only by `[ ":" port ]`. The generic-grammar gate
  # already rejected every one of these, so the arm does not move.
  bad <- c(
    "foo://[::1]x/", "foo://[::1]]/", "foo://[::1]\\x", "foo://u@[::1]x/",
    "foo://[::1]x", "foo://[::1]x?q", "foo://[::1]x:80/"
  )
  good <- c("foo://[::1]:80/", "foo://[::1]:/", "foo://[::1]/", "foo://[::1]")
  for (nm in names(.sabt_rfc_args)) {
    args <- .sabt_rfc_args[[nm]]
    b <- do.call(safe_parse_urls, c(list(bad), args))
    expect_identical(b$parse_status, rep("error", length(bad)), info = nm)
    expect_true(all(is.na(b$host)), info = nm)
    g <- do.call(safe_parse_urls, c(list(good), args))
    expect_identical(g$parse_status, rep("ok", length(good)), info = nm)
    expect_identical(g$host, rep("[::1]", length(good)), info = nm)
    expect_identical(g$port, c(80L, NA, NA, NA), info = nm)
  }
  for (form in c("source", "normalized")) {
    expect_identical(
      serialize_url(bad, standard = "rfc3986", form = form),
      rep(NA_character_, length(bad)),
      info = form
    )
  }
  expect_identical(
    serialize_url(good, standard = "rfc3986"),
    c("foo://[::1]:80/", "foo://[::1]/", "foo://[::1]/", "foo://[::1]")
  )
})

test_that("file: the RFC 8089 overlay's bracket tails do not move", {
  # `.parse_rfc_file_urls_vec()` is the other caller of `.split_authority()`,
  # and the only one `url_standard = NULL` reaches. Every tailed row already
  # failed under all three selectors. Under WHATWG `file:` is special: the
  # file host state ends the host at "\", so `file://[::1]\x` is the host
  # `[::1]` and the path `/x` there, as Node gives it.
  bad <- c("file://[::1]x/", "file://[::1]]/", "file://u@[::1]x/")
  for (std in list(NULL, "whatwg", "rfc3986")) {
    lab <- if (is.null(std)) "NULL" else std
    b <- safe_parse_urls(bad, url_standard = std)
    expect_identical(b$parse_status, rep("error", length(bad)), info = lab)
    expect_true(all(is.na(b$host)), info = lab)
    expect_true(all(is.na(b$clean_url)), info = lab)
    ok <- safe_parse_urls("file://[::1]/", url_standard = std)
    expect_identical(ok$parse_status, "ok", info = lab)
    expect_identical(ok$host, "[::1]", info = lab)
  }
  expect_identical(
    safe_parse_urls(bad)$parse_status, rep("error", length(bad))
  )
  for (std in c("whatwg", "rfc3986")) {
    expect_identical(
      serialize_url(bad, standard = std), rep(NA_character_, length(bad)),
      info = std
    )
  }

  bs <- "file://[::1]\\x"
  w <- safe_parse_urls(bs, url_standard = "whatwg")
  expect_identical(
    c(w$parse_status, w$host, w$path), c("ok", "[::1]", "/x")
  )
  expect_identical(serialize_url(bs, standard = "whatwg"), "file://[::1]/x")
  for (std in list(NULL, "rfc3986")) {
    lab <- if (is.null(std)) "NULL" else std
    expect_identical(
      safe_parse_urls(bs, url_standard = std)$parse_status, "error",
      info = lab
    )
  }
  expect_identical(serialize_url(bs, standard = "rfc3986"), NA_character_)
})

test_that("NULL: a non-special bracket tail never reaches the general parser", {
  # `scheme_acceptance = "general"` is an error without a selector, so the
  # frozen profile reads `foo:` through the web route, which rejects it
  # outright. Omitting `url_standard` and passing NULL agree.
  urls <- c(
    "foo://[::1]x/", "foo://[::1]]/", "foo://[::1]\\x", "foo://u@[::1]x/",
    "foo://[::1]x", "foo://[::1]/", "foo://[::1]:80/"
  )
  omitted <- safe_parse_urls(urls)
  explicit <- safe_parse_urls(urls, url_standard = NULL)
  expect_identical(omitted, explicit)
  expect_identical(omitted$parse_status, rep("error", length(urls)))
  expect_true(all(is.na(omitted$host)))
})

test_that("whatwg: text after the ] of a non-special host fails the parse", {
  # Each tail stays in the host buffer -- `\` is no delimiter for a non-special
  # scheme, and in `[::1]x:80` the buffer ends at the `:` after `x` -- so the
  # host parser fails it: IPv6-unclosed, or the IPv6 parser itself. These
  # parsed as host `[::1]`, the tail silently dropped: `foo://[::1]x/`
  # serialized as `foo://[::1]/` and `foo://[::1]\x` as `foo://[::1]`.
  urls <- c(
    "foo://[::1]x/", "foo://[::1]]/", "foo://[::1]\\x", "foo://u@[::1]x/",
    "foo://u:p@[::1]x/", "foo://[::1]x", "foo://[::1]x?q", "foo://[::1]x#f",
    "foo://[::1]x:80/", "foo://[::1][::2]/", "foo://[::1]%41/",
    "foo://[::1]./", "mailto://[::1]x/"
  )
  # An input that already failed in the IPv6 parser, one per row, so the two
  # data frames line up row for row.
  twin <- rep("foo://[1:]/", length(urls))
  for (args in list(
    list(profile = "whatwg"),
    list(url_standard = "whatwg", scheme_acceptance = "general")
  )) {
    lab <- paste(names(args), unlist(args), collapse = ", ")
    p <- do.call(safe_parse_urls, c(list(urls), args))
    expect_identical(p$parse_status, rep("error", length(urls)), info = lab)
    expect_true(all(is.na(p$host)), info = lab)
    expect_true(all(is.na(p$clean_url)), info = lab)
    # The failure has the shape every other host-parser failure has.
    t <- do.call(safe_parse_urls, c(list(twin), args))
    expect_identical(p[-1L], t[-1L], info = lab)
  }
  expect_identical(
    serialize_url(urls, standard = "whatwg"), rep(NA_character_, length(urls))
  )
  expect_identical(
    get_host_type(urls, url_standard = "whatwg", scheme_acceptance = "general"),
    rep(NA_character_, length(urls))
  )
  policy <- url_key_policy(standard = "whatwg")
  expect_identical(
    unclass(get_url_key(urls, policy)), unclass(get_url_key(twin, policy))
  )
})

test_that("whatwg: a tailed row fails alone; its neighbors keep their values", {
  mixed <- c("foo://[::1]/a", "foo://[::1]x/", "foo://[::1]:8080/b", "foo://h/")
  p <- safe_parse_urls(mixed, profile = "whatwg")
  expect_identical(p$parse_status, c("ok", "error", "ok", "ok"))
  expect_identical(p$host, c("[::1]", NA, "[::1]", "h"))
  expect_identical(
    serialize_url(mixed, standard = "whatwg"),
    c("foo://[::1]/a", NA_character_, "foo://[::1]:8080/b", "foo://h/")
  )
})
