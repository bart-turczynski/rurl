# Pins for the ADR 0005 U+FEFF guards the ADR 0019 close-out removes
# (RURL-dddxutjf). Each guard was a `.starts_with_bom()` mask or a spliced
# ASCII byte that kept a skipping stringi call from reading past a leading
# U+FEFF. The calls behind them now run in byte mode, which reads the mark as
# data, so the guards change nothing. These rows hold that: each U+FEFF row
# reads exactly as its U+200B twin, a zero-width code point stringi never
# skipped, with the mark swapped back in.

bom <- "\ufeff"
zwsp <- "\u200b"

# Swap U+200B for U+FEFF, raw and percent-encoded, in every character vector
# of `x`, recursively, so a U+200B result can be compared with a U+FEFF one.
swap_mark <- function(x) {
  if (is.list(x)) {
    for (i in seq_along(x)) {
      if (!is.null(x[[i]])) x[[i]] <- swap_mark(x[[i]])
    }
    return(x)
  }
  if (!is.character(x)) {
    return(x)
  }
  out <- gsub(zwsp, bom, x, fixed = TRUE, useBytes = TRUE)
  out <- gsub("%E2%80%8B", "%EF%BB%BF", out, fixed = TRUE, useBytes = TRUE)
  out <- gsub("%e2%80%8b", "%ef%bb%bf", out, fixed = TRUE, useBytes = TRUE)
  Encoding(out) <- "UTF-8"
  out
}

# The bytes of each element, so the comparison does not depend on how R
# compares strings whose encoding marks differ.
as_bytes <- function(x) {
  if (is.list(x)) {
    return(lapply(x, as_bytes))
  }
  if (!is.character(x)) {
    return(x)
  }
  lapply(x, function(s) if (is.na(s)) NA else charToRaw(s))
}

# `f` applied to the U+FEFF rows of `template` ("{M}" marks the code point)
# reads as `f` applied to their U+200B twins.
expect_twins <- function(template, f, label) {
  b <- sub("{M}", bom, template, fixed = TRUE)
  z <- sub("{M}", zwsp, template, fixed = TRUE)
  Encoding(b) <- "UTF-8"
  Encoding(z) <- "UTF-8"
  expect_identical(as_bytes(f(b)), as_bytes(swap_mark(f(z))), label = label)
}

lead_rows <- c(
  "{M}http:\\\\h\\p", "{M}file:\\\\h\\x", "{M}http://a。b/",
  "{M}//a。b/", "{M}http://a b@h/", "{M}http:ex.com/p", "{M}http:foo",
  "{M}mailto:x@y", "{M}//h/p", "{M}file:/a", "{M}h.example.com"
)
arms <- list(whatwg = "whatwg", rfc3986 = "rfc3986", "NULL" = NULL)

test_that("Phase 1 reads a U+FEFF-led row as its U+200B twin", {
  # Covers the masks in .rewrite_whatwg_backslashes_vec(),
  # .map_whatwg_domain_separators_vec(), .encode_userinfo_charset_vec(),
  # .rfc3986_path_rootless_vec(), and Phase 1's looks_like_protocol and
  # is_scheme_relative tests: a led row has no scheme and is not
  # scheme-relative, and no rewrite touches it.
  for (an in names(arms)) {
    for (srh in c("keep", "error", "http")) {
      expect_twins(lead_rows, function(u) {
        rurl:::.prepare_urls_for_parse_vec(
          u, "keep", srh, url_standard = arms[[an]]
        )
      }, label = paste(an, srh))
    }
  }
})

test_that("each Phase 1 rewrite leaves a U+FEFF-led row as written", {
  b <- paste0(bom, c("http:\\\\h\\p", "file:\\\\h\\x"))
  bs <- rurl:::.rewrite_whatwg_backslashes_vec(b, "whatwg")
  expect_identical(bs$url, b)
  expect_identical(bs$backslash_rewritten, c(FALSE, FALSE))
  b <- paste0(bom, c("http://a。b/", "//a。b/"))
  expect_identical(
    rurl:::.map_whatwg_domain_separators_vec(b, "whatwg")$url, b
  )
  b <- paste0(bom, "http://a b@h/")
  expect_identical(rurl:::.encode_userinfo_charset_vec(b, "whatwg")$url, b)
  r <- rurl:::.rfc3986_path_rootless_vec(paste0(bom, "http:ex.com/p"),
                                         "rfc3986")
  expect_false(r$is_path_rootless)
  expect_identical(r$scheme, NA_character_)
  expect_twins(c("{M}http:\\\\h\\p", "{M}file:\\\\h\\x", "{M}http:h"),
               function(u) rurl:::.rewrite_whatwg_backslashes_vec(u, "whatwg"),
               label = "backslashes")
  expect_twins(c("{M}http://a。b/", "{M}//a。b/"),
               function(u) {
                 rurl:::.map_whatwg_domain_separators_vec(u, "whatwg")
               }, label = "separators")
  expect_twins("{M}http://a b@h/",
               function(u) rurl:::.encode_userinfo_charset_vec(u, "whatwg"),
               label = "userinfo")
  expect_twins(c("{M}http:ex.com/p", "{M}https:a.b"),
               function(u) rurl:::.rfc3986_path_rootless_vec(u, "rfc3986"),
               label = "rootless")
})

test_that("the rfc3986 scheme-relative prefix skips a U+FEFF-led row", {
  for (srh in c("keep", "http", "https")) {
    opts <- list(
      fixup_posture = "none", url_standard = "rfc3986",
      scheme_policy = "infer", scheme_relative_handling = srh
    )
    f <- function(u) rurl:::.general_route_input_vec(u, opts)
    expect_identical(f(paste0(bom, "//h/p")), paste0(bom, "//h/p"))
    expect_twins(c("{M}//h/p", "{M}//h"), f, label = srh)
  }
})

test_that("a U+FEFF that leads a userinfo stays in it", {
  # .percent_encode_userinfo_charset(): the byte-mode replaces keep the mark,
  # so the spliced "@" did nothing.
  expect_identical(
    rurl:::.percent_encode_userinfo_charset(c(paste0(bom, "a b"), NA)),
    c(paste0(bom, "a%20b"), NA)
  )
  expect_identical(
    get_user(paste0("http://", bom, "a b@h/"), url_standard = "whatwg"),
    "%EF%BB%BFa%20b"
  )
  expect_twins(c("http://{M}a b@h/", "http://{M}a\tb@h/"),
               function(u) get_user(u, url_standard = "whatwg"),
               label = "get_user")
})

test_that("a WHATWG file path led by U+FEFF keeps the mark", {
  # .parse_whatwg_file_urls_vec(): the raw path `<U+FEFF>\a\b` starts with the
  # mark, and the byte-mode "\" rewrite keeps it, so the spliced "/" did
  # nothing. The file state then roots the path.
  expect_identical(
    rurl:::.parse_whatwg_file_urls_vec(paste0("file:", bom, "\\a\\b"))$path,
    paste0("/", bom, "/a/b")
  )
  expect_identical(
    safe_parse_url(paste0("file:", bom, "\\a"), url_standard = "whatwg")$path,
    paste0("/", bom, "/a")
  )
  expect_twins(
    c("file:{M}\\a\\b", "file:{M}/a", "file:{M}", "file:{M}\\"),
    function(u) rurl:::.parse_whatwg_file_urls_vec(u),
    label = "file parser"
  )
})

test_that("the public API reads U+FEFF-led inputs as their U+200B twins", {
  rows <- c(lead_rows, "file:{M}\\a\\b", "file:{M}/a", "http://{M}a b@h/")
  for (an in names(arms)) {
    for (srh in c("keep", "error", "http")) {
      std <- arms[[an]]
      expect_twins(rows, function(u) {
        list(
          safe_parse_urls(u, url_standard = std,
                          scheme_relative_handling = srh),
          if (!is.null(std)) get_url_diagnostics(u, url_standard = std)
        )
      }, label = paste(an, srh))
    }
  }
})

# The second close-out slice: the masks in .rfc3986_generic_uri_ok(),
# .general_parsed_mask() (R/parse-state.R) and .fsss_source_lex()
# (R/serialize.R). Each sat in front of a scheme test that now runs in byte
# mode, where `\A[A-Za-z]` cannot match the mark's first byte.
scheme_rows <- c(
  "{M}http://h/", "{M}foo:x", "{M}urn:a:1", "{M}file:///p", "{M}http:///h",
  "{M}http:/h", "{M}https://h/", "{M}ws://h/", "{M}mailto:x@y",
  "{M}example.com:8080", "{M}http://u@h:1/p?q#f"
)

test_that("the RFC 3986 grammar reads a U+FEFF-led row as its U+200B twin", {
  # A mark-led row has no scheme (S3.1), so the walk rejects it; the
  # whole-string ASCII test flags it as well.
  b <- paste0(bom, sub("{M}", "", scheme_rows, fixed = TRUE))
  Encoding(b) <- "UTF-8"
  g <- rurl:::.rfc3986_generic_uri_ok(b)
  expect_identical(g$ok, rep(FALSE, length(b)), label = "ok")
  expect_identical(g$diagnostic,
                   rep("unicode-outside-rfc3986-uri", length(b)),
                   label = "diagnostic")
  # The component walk alone rejects the row too: no scheme.
  expect_identical(rurl:::.rfc3986_generic_uri_ok_walk(b),
                   rep(FALSE, length(b)), label = "walk")
  expect_twins(scheme_rows, function(u) {
    rurl:::.rfc3986_generic_uri_ok(u)
  }, label = "grammar")
  expect_twins(scheme_rows, function(u) {
    rurl:::.rfc3986_generic_uri_ok_walk(u)
  }, label = "walk")
})

test_that("a U+FEFF-led row is never general-routed, as its U+200B twin", {
  b <- paste0(bom, sub("{M}", "", scheme_rows, fixed = TRUE))
  Encoding(b) <- "UTF-8"
  for (an in names(arms)) {
    for (acc in c("web", "general")) {
      for (pol in c("infer", "require")) {
        f <- function(u) {
          rurl:::.general_parsed_mask(u, arms[[an]], acc, scheme_policy = pol)
        }
        expect_identical(f(b), rep(FALSE, length(b)),
                         label = paste(an, acc, pol))
        expect_twins(scheme_rows, f, label = paste(an, acc, pol))
      }
    }
  }
})

test_that("the serializer's source lex reads a U+FEFF-led row as its twin", {
  rows <- c(scheme_rows, " {M}http://u@h/", "\t{M}http://u@h/")
  for (an in names(arms)) {
    expect_twins(rows, function(u) rurl:::.fsss_source_lex(u, arms[[an]]),
                 label = an)
  }
})

test_that("a native-marked U+FEFF-led row finds no scheme either", {
  # The old masks were byte tests, so they caught the mark under any encoding
  # mark; the byte-mode scheme capture must too.
  b <- paste0("\xef\xbb\xbf", sub("{M}", "", scheme_rows, fixed = TRUE))
  Encoding(b) <- "unknown"
  g <- rurl:::.rfc3986_generic_uri_ok(b)
  expect_identical(g$ok, rep(FALSE, length(b)), label = "ok")
  for (an in names(arms)) {
    expect_identical(
      rurl:::.general_parsed_mask(b, arms[[an]], "general"),
      rep(FALSE, length(b)), label = an
    )
    lex <- rurl:::.fsss_source_lex(b, arms[[an]])
    expect_identical(lex$host, rep(NA_character_, length(b)), label = an)
  }
})
