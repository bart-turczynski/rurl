# Characterization (golden) snapshot for safe_parse_urls().
#
# Purpose: lock the CURRENT public output of the parser before the
# option-threading refactor (RURL-sptpujmn) touches it. A fixed corpus of URLs
# is run through a spread of option combinations and the combined result is
# snapshotted with style = "json2", which compares deserialized VALUES (not just
# printed text) -- so any change to observable output fails the test with a
# readable diff.
#
# If a later step intentionally changes output, regenerate with
# testthat::snapshot_accept("characterization-snapshot") AFTER confirming the
# diff is the intended behavior change.

# A corpus spanning the parser's interesting branches: plain hosts, www, deep
# subdomains + multi-part TLD, userinfo + port + query + fragment, mixed case +
# index page, scheme-relative, ftp, IPv4, IPv6, punycode + IDN hosts, malformed
# input, dot-segment / double-slash paths, an unsupported scheme, empty, and NA.
.characterization_corpus <- c(
  "https://example.com/",
  "http://www.example.com/path/",
  "https://user:pass@example.com:8443/p?q=1#frag",
  "HTTP://WWW.Example.COM/Path/Index.html",
  "//cdn.example.com/lib.js",
  "ftp://files.example.org/pub/",
  "https://a.b.c.d.example.co.uk/",
  "http://192.168.0.1:8080/admin",
  "http://[2001:db8::1]/path",
  "https://xn--bcher-kva.example/",
  "https://bücher.example/",
  "https://例え.テスト/パス",
  "not a url",
  "https://example.com/a/./b/../c//d",
  "https://example.com/index.html",
  "mailto:user@example.com",
  "",
  NA_character_
)

# A curated spread of option combinations. The first entry is all-defaults; the
# rest each exercise the non-default values of one or more options so every
# option's branches are represented at least once.
.characterization_combos <- list(
  defaults = list(),
  strip_all = list(
    protocol_handling = "strip", www_handling = "strip",
    trailing_slash_handling = "strip", index_page_handling = "strip"
  ),
  lower_unicode = list(case_handling = "lower", host_encoding = "unicode"),
  idna_encode_path = list(host_encoding = "idna", path_encoding = "encode"),
  pathnorm_both_decode = list(
    path_normalization = "both", path_encoding = "decode"
  ),
  force_https_scheme_rel = list(
    protocol_handling = "https", scheme_relative_handling = "https",
    www_handling = "if_no_subdomain"
  ),
  icann_subdomain1 = list(tld_source = "icann", subdomain_levels_to_keep = 1),
  upper_keep = list(
    case_handling = "upper", trailing_slash_handling = "keep",
    index_page_handling = "keep"
  ),
  private_subdomain0_lowerhost = list(
    tld_source = "private", subdomain_levels_to_keep = 0,
    case_handling = "lower_host"
  )
)

# Build one combined data.frame: every combo applied to the full corpus, tagged
# with the combo name and a stable row index so the snapshot order is fixed.
.build_characterization_frame <- function() {
  parts <- lapply(names(.characterization_combos), function(combo_name) {
    args <- c(
      list(url = .characterization_corpus),
      .characterization_combos[[combo_name]]
    )
    res <- do.call(safe_parse_urls, args)
    cbind(
      combo = combo_name,
      url_index = seq_len(nrow(res)),
      res,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, parts)
}

test_that("safe_parse_urls() output matches characterization snapshot", {
  result <- .build_characterization_frame()

  # Sanity: shape is what the corpus x combos should produce, so an accidental
  # corpus/combo edit is caught explicitly rather than silently re-snapshotted.
  expect_equal(
    nrow(result),
    length(.characterization_corpus) * length(.characterization_combos)
  )

  expect_snapshot_value(result, style = "json2")
})
