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

test_that("whatwg file: an invalid IPv6 literal fails the parse", {
  # Each inner text fails the IPv6 parser: a trailing lone ":", two "::", a
  # piece of five hex digits, nine pieces, eight pieces plus "::", a dotted
  # IPv4 part after seven pieces, too few pieces without "::".
  inner <- c(
    "1:", "1::2::3", ":::", "12345::",
    "1:2:3:4:5:6:7:8:9", "1::2:3:4:5:6:7:8", ":1::", "::1:", "00001::",
    "a::b::", "1:2:3:4:5:6:7", "1:2:3:4:5:6:7:1.2.3.4"
  )
  file_urls <- paste0("file://[", inner, "]/")
  http_urls <- paste0("http://[", inner, "]/")
  cols <- c("parse_status", "host", "path", "clean_url", "is_ip_host")
  for (args in list(
    list(url_standard = "whatwg"),
    list(profile = "whatwg")
  )) {
    lab <- paste(names(args), unlist(args))
    f <- do.call(safe_parse_urls, c(list(file_urls), args))
    expect_identical(f$parse_status, rep("error", length(inner)), info = lab)
    expect_true(all(is.na(f$host)), info = lab)
    expect_true(all(is.na(f$clean_url)), info = lab)
    # The failure has the shape the http: route gives the same host.
    h <- do.call(safe_parse_urls, c(list(http_urls), args))
    expect_identical(f[cols], h[cols], info = lab)
  }
  expect_identical(
    serialize_url(file_urls, standard = "whatwg"),
    rep(NA_character_, length(inner))
  )
  # One bad row fails alone; its neighbors keep their values.
  mixed <- c("file://[::1]/a", "file://[1:]/", "file:///b")
  m <- safe_parse_urls(mixed, url_standard = "whatwg")
  expect_identical(m$parse_status, c("ok", "error", "ok"))
  expect_identical(
    serialize_url(mixed, standard = "whatwg"),
    c("file://[::1]/a", NA_character_, "file:///b")
  )
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
