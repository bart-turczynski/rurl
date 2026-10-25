# The ICU-emulating cuts and tests of R/parse-phases.R stay linear in the row
# and under PCRE's match limit (RURL-atlidhyh). Each block pins the helper's
# reading on short rows first, then a row long enough to have hit the limit.

test_that(".icu_cut_through_last_at() cuts through the last \"@\" of line 1", {
  cut <- .icu_cut_through_last_at
  # The last "@" wins; with none, nothing is cut.
  expect_identical(cut("u@h"), "h")
  expect_identical(cut("a@b@c"), "c")
  expect_identical(cut("@@@h"), "h")
  expect_identical(cut("u@"), "")
  expect_identical(cut("@"), "")
  expect_identical(cut("h"), "h")
  expect_identical(cut(""), "")
  expect_identical(cut(NA_character_), NA_character_)
  expect_identical(cut(c("a@b", NA, "c", "d@e@f")), c("b", NA, "c", "f"))
  expect_identical(cut(character()), character())
  # ICU's `.` stops at the first line terminator, so an "@" after one is not
  # reached; the terminator and all after it stay.
  expect_identical(cut("a@b\nc@d"), "b\nc@d")
  expect_identical(cut("a\n@b"), "a\n@b")
  expect_identical(cut("a@b\r\nc@d"), "b\r\nc@d")
  expect_identical(cut("a@b\u2028c@d"), "b\u2028c@d")
  expect_identical(cut("a@b\u0085c@d"), "b\u0085c@d")
  expect_identical(cut("a@b\vc@d"), "b\vc@d")
  # U+3002 and other multibyte code points are data; positions are bytes.
  expect_identical(cut("u\u3002i@h\u3002b"), "h\u3002b")
  expect_identical(cut("\u00fc@\u00df@\u00e9"), "\u00e9")
  # A leading U+FEFF is data, cut with the userinfo or kept with the host.
  expect_identical(cut("\ufeffu@h"), "h")
  expect_identical(cut("\ufeff@h"), "h")
  expect_identical(cut("\ufeffh"), "\ufeffh")
  expect_identical(cut("u@\ufeffh"), "\ufeffh")
  expect_identical(Encoding(cut("u@h\u00e9")), "UTF-8")
})

test_that(".classify_input_host_vec() reads the host after the last \"@\"", {
  cls <- .classify_input_host_vec(c(
    "http://u\u3002i@ha\u3002b/p", "http://a@b@1.2.3.4:80/", "//x@[::1]:8/"
  ))
  expect_identical(cls$host_token, c("ha\u3002b", "1.2.3.4", "[::1]"))
  expect_identical(cls$has_userinfo, c(TRUE, TRUE, TRUE))
  expect_identical(cls$is_canonical_ip, c(FALSE, TRUE, TRUE))
})

# PCRE2's default match limit is 1e7; a row a little past it. The long rows
# below run through the helper that cuts or tests them, not a full parse, so
# the file stays fast.
long_n <- 10500000L

test_that("the last-\"@\" cut stays under the match limit on a long row", {
  # `\A(?s:.)*@` ran to the end and gave the row back byte by byte to the
  # last "@": past ~1e7 bytes `regexpr()` warned and the row went uncut, so
  # the userinfo read as part of the host token.
  host <- paste0("h", strrep("a", long_n), "\u3002b")
  expect_no_warning(out <- .icu_cut_through_last_at(paste0("u\u3002i@", host)))
  # `expect_true(identical())`: a failure must not print the row.
  expect_true(identical(out, host))
  expect_no_warning(out <- .icu_cut_through_last_at(strrep("@", long_n)))
  expect_identical(out, "")
})

test_that("the host classifier drops only the last \":port\"", {
  tok <- function(x) .classify_input_host_vec(x)$host_token
  expect_identical(
    tok(c("http://h:80/", "http://h:1:2/", "http://h/", "http://h:/",
          "u@h:8", "http://a:b@h:9/", "h:1\v2", NA)),
    c("h", "h:1", "h", "h", "h", "h", "h", NA)
  )
})

test_that("the port cut stays under the match limit on a long row", {
  # `:[^:]*\z` gave back the run after each earlier ":" byte by byte: with
  # ~1e7 bytes before the last ":" it warned and the port stayed on.
  host <- paste0("h:", strrep("a", long_n))
  expect_no_warning(
    cls <- .classify_input_host_vec(paste0("http://", host, ":80/p"))
  )
  expect_true(identical(cls$host_token, host))
})

test_that("the userinfo encoder reads the span before the last \"@\"", {
  enc <- function(x) .encode_userinfo_charset_vec(x, "whatwg")$url
  expect_identical(
    enc(c("http://u v@h/", "http://u@h v/", "http://a b@c d@h/", "http://h/",
          "https://u\x01@h@x/", "http://u@h\x01/", NA)),
    c("http://u%20v@h/", "http://u@h v/", "http://a%20b@c%20d@h/", "http://h/",
      "https://u%01@h@x/", "http://u@h\x01/", NA)
  )
})

test_that("the userinfo span stays under the match limit on a long row", {
  # `@[^@]*\z` gave back the run after each earlier "@" byte by byte: with
  # ~1e7 bytes between two "@" it warned and read the host as userinfo.
  url <- paste0("http://u@", strrep("a", long_n), "@h\x01/")
  expect_no_warning(out <- .encode_userinfo_charset_vec(url, "whatwg")$url)
  expect_true(identical(out, url))
})

prep_host_port <- function(x) {
  .prepare_urls_for_parse_vec(x, "keep", "keep")$looks_like_host_port
}

test_that("Phase 1 reads a scheme-less host:port only before \"/\" or the end", {
  expect_identical(
    prep_host_port(c("example.com:8080", "example.com:8080/p", "h:1x",
                     "urn:a:1", "h:80?q", "h:", "h:8080\u2028")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("the host:port test stays under the match limit on a long port", {
  # `[0-9]+(?:\z|/)` gave a digit run back byte by byte before failing: with
  # ~1e7 digits and no "/" or end after them it warned.
  x <- paste0("h:", strrep("1", long_n), "x")
  expect_no_warning(out <- prep_host_port(x))
  expect_false(out)
})

test_that("the IPv6 dotted-quad tail follows the last \":\" of a run", {
  x <- c("[::ffff:1.2.3.4]", "::1.2.3.4", "[:1.2.3.4]", "[1::01.2.3.4]",
         "[a:b:255.255.255.255]\n", "[a:b:255.255.255.255]\r\n", "[::1]",
         "1.2.3.4", "[::ffff:1.2.3.256]", "[::1a.2.3.4]", "[g::1.2.3.4]",
         "[::1.2.3.4]]", "[::1.2.3.4]\n\n", NA)
  expect_identical(
    .const_detect(x, .ICU_IPV6_IPV4_TAIL),
    c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, NA)
  )
})

test_that("the IPv6 tail test stays under the match limit on a long run", {
  # The atomic `(?>[0-9a-fA-F:]+:)` still ran to the end of the hex run and
  # gave it back byte by byte to the last ":": with ~1e7 hex digits after
  # it, it warned.
  x <- c(paste0("[::", strrep("a", long_n), "]"),
         paste0("[", strrep("a:", long_n %/% 2L), "1.2.3.4]"))
  expect_no_warning(out <- .const_detect(x, .ICU_IPV6_IPV4_TAIL))
  expect_identical(out, c(FALSE, TRUE))
})

trim_rows <- c(
  "http://a/ \t", "http://a/ b ", "  http://a/  ", "http://a/\r\n",
  "http://a/ \u2028", "http://a/ \r\n\n", " ", "", "http://a/\u2028 ", NA
)
strip_trim <- function(x) .strip_whatwg_control_chars_vec(x, "whatwg")$url
fixup_trim <- function(x) .apply_browser_fixup_vec(x, "browser", "whatwg")

test_that("the trailing C0/space trims cut the final run only", {
  # The WHATWG strip ends at the true end; the browser fixer at ICU's `$`,
  # so it also trims a run before a final U+2028.
  expect_identical(strip_trim(trim_rows), c(
    "http://a/", "http://a/ b", "http://a/", "http://a/", "http://a/ \u2028",
    "http://a/", "", "", "http://a/\u2028", NA
  ))
  expect_identical(fixup_trim(trim_rows), c(
    "http://a/", "http://a/ b", "http://a/", "http://a/", "http://a/\u2028",
    "http://a/", "", "", "http://a/\u2028", NA
  ))
})

test_that("the trailing trims stay under the match limit on a long run", {
  # Tried at each byte of an inner run, the greedy run gave the rest of it
  # back byte by byte: with ~1e7 bytes in one run it warned.
  x <- paste0("http://a/", strrep(" ", long_n), "b")
  expect_no_warning(out <- strip_trim(x))
  expect_true(identical(out, x))
  expect_no_warning(out <- fixup_trim(x))
  expect_true(identical(out, x))
})

test_that("the trailing trims are linear in an inner run", {
  # Below the limit the same retries were quadratic: an inner run of 2e4
  # spaces took about 2 s in the strip and 14 s in the browser fixer.
  skip_on_cran()
  x <- paste0("http://a/", strrep(" ", 20000L), "b")
  expect_lt(system.time(strip_trim(x))[["elapsed"]], 0.5)
  expect_lt(system.time(fixup_trim(x))[["elapsed"]], 0.5)
})

map_sep <- function(x) .map_whatwg_domain_separators_vec(x, "whatwg")$url

test_that("the full-stop map starts the host after the authority's last \"@\"", {
  expect_identical(
    map_sep(c("http://u\u3002i@h\u3002b/p", "http://a@b@h\uff0eb/c@d",
              "//h\uff61b?x@y", "http://@h\u3002b", "http://h/\u3002@x",
              "http://u\u3002i@[::1]/", "\ufeffu@h\u3002b", NA)),
    c("http://u\u3002i@h.b/p", "http://a@b@h.b/c@d", "//h.b?x@y",
      "http://@h.b", "http://h/\u3002@x", "http://u\u3002i@[::1]/",
      "\ufeffu@h\u3002b", NA)
  )
})

test_that("the full-stop map's last-\"@\" locate stays under the match limit", {
  # Possessive runs repeated once per "@" still counted one step each: an
  # authority of ~1e7 "@" warned, the "@" went unfound, and the userinfo
  # was mapped as host.
  url <- paste0("http://", strrep("@", long_n), "u\u3002i@h\u3002b/p")
  expect_no_warning(out <- map_sep(url))
  expect_true(identical(
    out, paste0("http://", strrep("@", long_n), "u\u3002i@h.b/p")
  ))
})

test_that("the userinfo repair finds its last \"@\" in linear time", {
  # `.last_byte_index()` lists every "@" with a fixed `gregexpr()`, which is
  # quadratic in their number: 4e5 of them took about 2.7 s.
  skip_on_cran()
  url <- paste0("http://\x01", strrep("@", 400000L), "h/")
  elapsed <- system.time(
    out <- .encode_userinfo_charset_vec(url, "whatwg")$url
  )[["elapsed"]]
  expect_true(identical(out, paste0("http://%01", strrep("@", 400000L), "h/")))
  expect_lt(elapsed, 0.5)
})
