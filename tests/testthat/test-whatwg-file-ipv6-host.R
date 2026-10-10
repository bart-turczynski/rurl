# A bracketed IPv6 literal in a WHATWG `file:` authority (RURL-ohtwkdgi). The
# file host state hands its buffer to the host parser, which runs the IPv6
# parser on a host that starts with "[" (WHATWG URL Standard, host parsing,
# IPv6 parser). Every expected value below was read from Node 26's WHATWG
# `URL` on 2026-10-10 (`new URL(input).href`, or a thrown ERR_INVALID_URL).

test_that("whatwg file: a valid IPv6 literal serializes in WHATWG form", {
  cases <- data.frame(
    input = c(
      "file://[::1]/",
      "file://[1:0::]/",
      "file://[::1]",
      "file://[::1]/p",
      "file://[0:0:0:0:0:0:0:1]/",
      "file://[::127.0.0.1]/",
      "file://[A:B::]/",
      "file://[1:2:3:4:5:6:7::]/",
      "file://[::2:3:4:5:6:7:8]/",
      "file://[1:2:3:4:5:6:1.2.3.4]/"
    ),
    host = c(
      "[::1]",
      "[1::]",
      "[::1]",
      "[::1]",
      "[::1]",
      "[::7f00:1]",
      "[a:b::]",
      "[1:2:3:4:5:6:7:0]",
      "[0:2:3:4:5:6:7:8]",
      "[1:2:3:4:5:6:102:304]"
    ),
    href = c(
      "file://[::1]/",
      "file://[1::]/",
      "file://[::1]/",
      "file://[::1]/p",
      "file://[::1]/",
      "file://[::7f00:1]/",
      "file://[a:b::]/",
      "file://[1:2:3:4:5:6:7:0]/",
      "file://[0:2:3:4:5:6:7:8]/",
      "file://[1:2:3:4:5:6:102:304]/"
    ),
    stringsAsFactors = FALSE
  )
  parsed <- safe_parse_urls(cases$input, url_standard = "whatwg")
  expect_identical(parsed$parse_status, rep("ok", nrow(cases)))
  expect_identical(parsed$host, cases$host)
  expect_true(all(parsed$is_ip_host))
  expect_identical(serialize_url(cases$input, standard = "whatwg"), cases$href)
})

test_that("whatwg http: rejects the invalid IPv6 literals", {
  urls <- c(
    "http://[1:]/", "http://[1::2::3]/", "http://[:::]/", "http://[12345::]/",
    "http://[::1/"
  )
  parsed <- safe_parse_urls(urls, url_standard = "whatwg")
  expect_identical(parsed$parse_status, rep("error", length(urls)))
  expect_true(all(is.na(parsed$host)))
  expect_true(all(is.na(parsed$clean_url)))
  expect_identical(
    serialize_url(urls, standard = "whatwg"), rep(NA_character_, length(urls))
  )
})

test_that("whatwg file: malformed bracketed hosts that already fail stay so", {
  # `file://[::1/` has no closing "]": the host parser's IPv6 branch requires
  # one ("If input does not end with U+005D (]), IPv6-unclosed validation
  # error, return failure"). The others fail inside the IPv6 parser, or, for
  # `[::1]:80`, because the file host state has no port.
  urls <- c(
    "file://[::1/",
    "file://[]/",
    "file://[v1.x]/",
    "file://[::1%25eth0]/",
    "file://[::1.2.3.04]/",
    "file://[::1]:80/",
    "file://[::1]x/"
  )
  parsed <- safe_parse_urls(urls, url_standard = "whatwg")
  expect_identical(parsed$parse_status, rep("error", length(urls)))
  expect_true(all(is.na(parsed$host)))
  expect_true(all(is.na(parsed$clean_url)))
  expect_identical(
    serialize_url(urls, standard = "whatwg"), rep(NA_character_, length(urls))
  )
})

test_that("NULL and rfc3986 do not move for file: IPv6 literals", {
  bad <- c(
    "file://[1:]/", "file://[1::2::3]/", "file://[:::]/", "file://[12345::]/"
  )
  good <- c("file://[::1]/", "file://[1:0::]/")
  for (std in list(NULL, "rfc3986")) {
    lab <- if (is.null(std)) "NULL" else std
    b <- safe_parse_urls(bad, url_standard = std)
    expect_identical(b$parse_status, rep("error", length(bad)), info = lab)
    expect_true(all(is.na(b$host)), info = lab)
    expect_true(all(is.na(b$clean_url)), info = lab)

    # Neither arm runs the WHATWG IPv6 serializer: the literal keeps its
    # spelling.
    g <- safe_parse_urls(good, url_standard = std)
    expect_identical(g$parse_status, c("ok", "ok"), info = lab)
    expect_identical(g$host, c("[::1]", "[1:0::]"), info = lab)
    expect_identical(g$clean_url, good, info = lab)
  }
  expect_identical(
    serialize_url(bad, standard = "rfc3986"), rep(NA_character_, length(bad))
  )
  expect_identical(serialize_url(good, standard = "rfc3986"), good)
})
