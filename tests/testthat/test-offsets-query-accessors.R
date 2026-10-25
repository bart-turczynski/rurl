# RURL-yqmoztoj (ADR 0019): the offset-bearing `stringi` calls in
# R/accessors.R, R/diagnostics.R, R/path-query.R and R/parse.R run in base R
# in byte mode, every offset counted in bytes from the call that measures it
# to the cut that uses it. These pins hold the answers the stringi calls gave,
# on multibyte prefixes and suffixes, a leading U+FEFF with its U+200B twin,
# invalid UTF-8, empty and unmatched captures, no match and NA, and run under
# LC_CTYPE C and a UTF-8 locale.

u8 <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}

in_ctype <- function(locale, utf8, code) {
  active <- suppressWarnings(
    withr::with_locale(c(LC_CTYPE = locale), Sys.getlocale("LC_CTYPE"))
  )
  skip_if_not(identical(active, locale), paste(locale, "is not available"))
  withr::with_locale(c(LC_CTYPE = locale), {
    skip_if_not(
      identical(l10n_info()[["UTF-8"]], utf8),
      paste("LC_CTYPE", locale, "did not set the charset")
    )
    force(code)
  })
}
ctypes <- list(C = FALSE, `en_US.UTF-8` = TRUE)

bom <- "\ufeff"
zwsp <- "\u200b"

for (loc in names(ctypes)) {
  # .subdomain_labels_vec(): the host minus "." + its registrable domain.
  test_that(paste("get_subdomain() cuts multibyte hosts in one unit under",
                  loc), {
    in_ctype(loc, ctypes[[loc]], {
      url <- c(
        "http://b\u00fccher.co.uk/", "http://www.b\u00fccher.co.uk/",
        "http://\u00fc.\u00fc.example.com/", "http://a.b\u00fccher.co.uk/",
        "http://x.y.b\u00fccher.co.uk/", "http://\u00e9.\u00e9.github.io/",
        paste0("http://", bom, "a.b\u00fccher.co.uk/"),
        paste0("http://", zwsp, "a.b\u00fccher.co.uk/"),
        paste0("http://", bom, "\u00fc.example.com/"),
        paste0("http://", zwsp, "\u00fc.example.com/"),
        "http://A.xn--bcher-kva.co.uk/", "http://WWW.Sub.EXAMPLE.COM/",
        "http://[::1]/", "http://1.2.3.4/", NA, "", "http://example.com/"
      )
      bad <- u8(c("http://a.ex\xe9.co.uk/", "http://\xe9.example.com/"))
      want_keep <- list(
        character(0), character(0), c("\u00fc", "\u00fc"), "a", c("x", "y"),
        "\u00e9", paste0(bom, "a"), paste0(zwsp, "a"), paste0(bom, "\u00fc"),
        paste0(zwsp, "\u00fc"), "a", c("www", "sub"), character(0),
        character(0),
        character(0), character(0), character(0)
      )
      got <- get_subdomain(url, format = "labels")
      expect_identical(got, want_keep)
      expect_identical(get_subdomain(url, format = "labels",
                                     host_encoding = "unicode"), want_keep)
      expect_identical(Encoding(got[[3]]), c("UTF-8", "UTF-8"))
      expect_identical(
        get_subdomain(url, format = "labels", host_encoding = "idna"),
        list(character(0), character(0), c("xn--tda", "xn--tda"), "a",
             c("x", "y"), "xn--9ca", "xn--a-6m0i", "xn--a-pgn",
             "xn--tda7611k", "xn--tda510n", "a", c("www", "sub"), character(0),
             character(0), character(0), character(0), character(0))
      )
      expect_identical(
        get_subdomain(url, include_www = TRUE),
        c(NA, "www", "\u00fc.\u00fc", "a", "x.y", "\u00e9",
          paste0(bom, "a"), paste0(zwsp, "a"), paste0(bom, "\u00fc"),
          paste0(zwsp, "\u00fc"), "a", "www.sub", NA, NA, NA, NA, NA)
      )
      expect_identical(
        get_subdomain(url, url_standard = "whatwg")[c(1:6, 11:17)],
        c(NA, NA, "\u00fc.\u00fc", "a", "x.y", "\u00e9", "a", "www.sub",
          NA, NA, NA, NA, NA)
      )
      # Rows 1-10 write their host with raw non-ASCII, a parse error under
      # rfc3986: a reg-name is ASCII and non-ASCII data is percent-encoded
      # UTF-8 (RFC 3986 sections 2, 2.5, 3.2.2; RUL-030, RURL-pntbujzc). The
      # multibyte cuts stay covered by the NULL and whatwg calls above.
      expect_identical(
        get_subdomain(url, url_standard = "rfc3986")[c(1:6, 11:17)],
        c(NA, NA, NA, NA, NA, NA, "a", "www.sub", NA, NA, NA, NA, NA)
      )
      expect_identical(
        get_subdomain(url[7:10], url_standard = "rfc3986"),
        rep(NA_character_, 4L)
      )
      expect_no_error(got_bad <- get_subdomain(bad, format = "labels"))
      expect_identical(got_bad, list(character(0), character(0)))
      expect_identical(get_subdomain(character(0)), character(0))
      expect_identical(get_subdomain(NA_character_), NA_character_)
    })
  })

  # The internal helper sees the host and domain columns directly, so a host
  # the parser never emits (a multibyte suffix led by U+FEFF, an invalid row)
  # is pinned here.
  test_that(paste(".subdomain_labels_vec() measures host and suffix alike",
                  "under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      f <- rurl:::.subdomain_labels_vec
      host <- c(
        "b\u00fccher.co.uk", "\u00fc.\u00fc.example.com",
        paste0(bom, "\u00fc.b\u00fccher.co.uk"),
        paste0(zwsp, "\u00fc.b\u00fccher.co.uk"),
        "\u00fc\u00fc\u00fc.b\u00fccher.co.uk", "www.b\u00fccher.co.uk",
        "www7.b\u00fccher.co.uk", "a.www.b\u00fccher.co.uk",
        "a..b\u00fccher.co.uk", ".b\u00fccher.co.uk", "x.other.co.uk",
        NA, "", "a.b\u00fccher.co.uk"
      )
      dom <- c(rep("b\u00fccher.co.uk", 1L), "example.com",
               rep("b\u00fccher.co.uk", 9L), NA, "b\u00fccher.co.uk", NA)
      ip <- c(rep(FALSE, 13L), FALSE)
      expect_identical(
        f(host, dom, ip, FALSE),
        list(character(0), c("\u00fc", "\u00fc"), paste0(bom, "\u00fc"),
             paste0(zwsp, "\u00fc"), "\u00fc\u00fc\u00fc", character(0),
             character(0), c("a", "www"), "a", character(0),
             character(0), character(0), character(0), character(0))
      )
      expect_identical(f(host[6:7], dom[6:7], ip[6:7], TRUE),
                       list("www", "www7"))
      expect_identical(f("a.b\u00fccher.co.uk", "b\u00fccher.co.uk", TRUE,
                         FALSE), list(character(0)))
      expect_identical(f(character(0), character(0), logical(0), FALSE),
                       list())
    })
  })

  # ._strip_index_page(): the ICU capture `(?i)^(.*)/(index|default)\.[^/]+/?$`.
  test_that(paste("index_page_handling = 'strip' keeps the same directory",
                  "under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      url <- c(
        "http://e.com/a/index.html", "http://e.com/\u00fc/index.html",
        paste0("http://e.com/", bom, "/a/index.html"),
        paste0("http://e.com/", zwsp, "/a/index.html"),
        "http://e.com/a/INDEX.PHP", "http://e.com/a/default.htm/",
        "http://e.com/a/index.html/x", "http://e.com/index.html",
        "http://e.com/a/index.\u00fc", "http://e.com/a/indexhtml",
        "http://e.com/\u00fc\u00fc/b/Default.aspx", NA,
        "http://e.com/a/../index.html", "http://e.com/a/%C3%BC/index.html"
      )
      want <- c("/a", "/\u00fc", paste0("/", bom, "/a"),
                paste0("/", zwsp, "/a"), "/a", "/a", "/a/index.html/x", "/",
                "/a", "/a/indexhtml", "/\u00fc\u00fc/b", NA, "/",
                "/a/%C3%BC")
      # Rows 2, 3, 4, 9 and 11 write a raw non-ASCII code point (U+00FC,
      # U+FEFF, U+200B): parse errors under rfc3986, so their path is NA
      # there (RFC 3986 sections 2, 2.5; RUL-030, RURL-pntbujzc). The byte
      # cuts on those rows stay covered by NULL and whatwg.
      want_rfc <- want
      want_rfc[c(2L, 3L, 4L, 9L, 11L)] <- NA_character_
      for (std in list(NULL, "whatwg", "rfc3986")) {
        w <- if (identical(std, "rfc3986")) want_rfc else want
        expect_identical(get_path(url, index_page_handling = "strip",
                                  url_standard = std), w,
                         info = format(std))
        expect_identical(
          safe_parse_urls(url, index_page_handling = "strip",
                          url_standard = std)$path,
          w, info = format(std)
        )
      }
    })
  })

  test_that(paste("._strip_index_page() keeps ICU's `.` and `$` under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      s <- rurl:::._strip_index_page
      # ICU's `.*` stops at a line terminator, so a directory holding one
      # never matched; `[^/]+` crosses one; `$` also matches before a final
      # terminator (CRLF as one).
      expect_identical(s("/a\nb/index.html"), "/a\nb/index.html")
      expect_identical(s("/a\rb/index.html"), "/a\rb/index.html")
      expect_identical(s("/a\vb/index.html"), "/a\vb/index.html")
      expect_identical(s("/a\fb/index.html"), "/a\fb/index.html")
      expect_identical(s("/a\u0085/index.html"), "/a\u0085/index.html")
      expect_identical(s("/a\u2028b/index.html"), "/a\u2028b/index.html")
      expect_identical(s("/a\u2029b/index.html"), "/a\u2029b/index.html")
      expect_identical(s("/a/index.html\n"), "/a")
      expect_identical(s("/a/index.html\r"), "/a")
      expect_identical(s("/a/index.ht\nml"), "/a")
      expect_identical(s("/a/index.html/\r\n"), "/a")
      expect_identical(s("/a/index.\u2029"), "/a")
      expect_identical(s("/a/index.html/\n\n"), "/a/index.html/\n\n")
      expect_identical(s("/a/index.html/\r\r"), "/a/index.html/\r\r")
      expect_identical(s("/\u00fc\u00fc/index.\u00fc/"), "/\u00fc\u00fc")
      # ASCII case only: ICU folds no non-ASCII code point to these letters.
      expect_identical(s("/a/\u0130ndex.html"), "/a/\u0130ndex.html")
      expect_identical(s("/a/\u0131ndex.html"), "/a/\u0131ndex.html")
      expect_identical(s("/a/InDeX.x"), "/a")
      expect_identical(s("/a/DeFaUlT.x/"), "/a")
      # Empty capture, unmatched, NA, the empty string, a leading mark.
      expect_identical(s("/index.html"), "/")
      expect_identical(s("index.html"), "index.html")
      expect_identical(s("a/index.html"), "/a")
      expect_identical(s("/a/index."), "/a/index.")
      expect_identical(s(NA_character_), NA_character_)
      expect_identical(s(""), "")
      expect_identical(s("\ufeff/a/index.html"), "/\ufeff/a")
      expect_identical(s("\u200b/a/index.html"), "/\u200b/a")
      expect_identical(s(paste0(bom, "index.html")), paste0(bom, "index.html"))
      # An invalid-UTF-8 directory is cut by its bytes.
      bad <- u8("/\xe9/index.html")
      expect_no_warning(got <- s(bad))
      expect_identical(charToRaw(got), charToRaw("/\xe9"))
      expect_identical(Encoding(s("/\u00fc/index.html")), "UTF-8")
      # Linear: a multi-megabyte path with a million candidate pages cuts
      # without a PCRE match-limit warning.
      long <- sprintf("%s/index.html", strrep("/index.a", 1e6))
      expect_no_warning(got <- s(long))
      expect_identical(nchar(got), nchar(long) - 11L)
      expect_no_warning(got <- s(sprintf("%s/x", strrep("/index.a", 1e6))))
      expect_identical(nchar(got), 8000002L)
    })
  })

  test_that(paste("._strip_index_page()'s byte patterns pass the byte-mode",
                  "check under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      expect_true(rurl:::.bytes_check_pattern(rurl:::.INDEX_PAGE_TAIL, FALSE))
      expect_true(rurl:::.bytes_check_pattern(rurl:::.ICU_TERMINATOR, FALSE))
    })
  })

  # .source_scheme_spelling_vec(): the scheme as the input spells it.
  test_that(paste("serialize_url(form = 'source') keeps the scheme's",
                  "spelling under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      url <- c("HTTP://Example.COM/a", "HtTpS://e.com/", "Foo+X.y://h/x",
               "mailto:a@b.c", "MAILTO:a@b.c", paste0(bom, "HTTP://e.com/"),
               paste0(zwsp, "HTTP://e.com/"), "e.com/x", NA,
               "\u017Fhttp://x/", "HTTP://b\u00fccher.co.uk/")
      # The last row's raw U+00FC host is a parse error under rfc3986, so
      # no source form exists (RFC 3986 sections 2, 3.2.2; RUL-030,
      # RURL-pntbujzc). The percent-encoded twin keeps the scheme spelling.
      expect_identical(
        serialize_url(url, standard = "rfc3986"),
        c("HTTP://Example.COM/a", "HtTpS://e.com/", "Foo+X.y://h/x",
          "mailto:a@b.c", "MAILTO:a@b.c", NA, NA, NA, NA, NA, NA)
      )
      expect_identical(
        serialize_url("HTTP://b%C3%BCcher.co.uk/", standard = "rfc3986"),
        "HTTP://b%C3%BCcher.co.uk/"
      )
      expect_identical(
        serialize_url(url, standard = "whatwg"),
        c("http://example.com/a", "https://e.com/", "foo+x.y://h/x",
          "mailto:a@b.c", "mailto:a@b.c", NA, NA, NA, NA, NA,
          "http://xn--bcher-kva.co.uk/")
      )
      f <- rurl:::.source_scheme_spelling_vec
      bad <- u8("HT\xe9TP://x")
      expect_identical(
        f(c("HTTP://x", paste0(zwsp, "HTTP://x"), NA, "Ab:", "1a:", "HTTP:",
            "Ftp://x", ":x", bad),
          c("http", "http", "http", "ab", "1a", NA, "http", "http", "http")),
        c("HTTP", "http", "http", "Ab", "1a", NA, "http", "http", "http")
      )
      # A leading U+FEFF is data that no scheme starts with: stringi read past
      # it and spelled `HTTP` here. No public surface reaches this row (the
      # parse settles no scheme for it, above), so nothing visible moves.
      expect_identical(f(paste0(bom, "HTTP://x"), "http"), "http")
      expect_identical(f(c("HTTP://x", "x"), c("http", "x")), c("HTTP", "x"))
      expect_identical(f(character(0), character(0)), character(0))
    })
  })

  # .ipv6_host_diagnostics(): the brackets stripped off the canonical text.
  test_that(paste("the ipv6 diagnostics read the address inside the brackets",
                  "under", loc), {
    in_ctype(loc, ctypes[[loc]], {
      url <- c("http://[::FFFF:1.2.3.4]/", "http://[::ffff:102:304]/",
               "http://[64:ff9b::1.2.3.4]/", "http://[2001:DB8::1]/",
               "http://[::1]/", "http://[0:0::1]/", "http://example.com/",
               NA, "http://[::1.2.3.4]/", "http://[::1]:8080/a/index.html")
      want <- list(
        c("ipv6-non-canonical", "ipv6-embedded-ipv4"), "ipv6-embedded-ipv4",
        c("ipv6-non-canonical", "ipv6-embedded-ipv4"), "ipv6-non-canonical",
        character(0), "ipv6-non-canonical", character(0), character(0),
        c("ipv6-non-canonical", "ipv6-embedded-ipv4"), "non-default-port"
      )
      for (std in c("whatwg", "rfc3986")) {
        expect_identical(get_url_diagnostics(url, url_standard = std), want,
                         info = std)
      }
    })
  })
}
