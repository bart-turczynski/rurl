# Shared loader for the characterization corpus fixture
# (tests/testthat/fixtures/parse-corpus.csv). The CSV is the single, inspectable
# source of the URL list used by the characterization snapshot and the
# scalar/vector equivalence oracle. Two sentinels round-trip inputs the CSV
# cannot store literally:
#   "@@NA@@"    -> NA_character_ (missing input)
#   "@@EMPTY@@" -> ""            (empty-string input)
.corpus_decode_url <- function(x) {
  x[x == "@@NA@@"] <- NA_character_
  x[x == "@@EMPTY@@"] <- ""
  x
}

# Return the decoded corpus URL vector (order matches the CSV `id` column).
parse_corpus_urls <- function() {
  path <- testthat::test_path("fixtures", "parse-corpus.csv")
  df <- utils::read.csv(
    path,
    colClasses = "character",
    na.strings = character(0),
    encoding = "UTF-8"
  )
  .corpus_decode_url(df$url)
}

# The characterization option matrix. First entry is all-defaults; the rest each
# push one or more options off their default so every option's branches are
# represented at least once, plus two combined "profile" rows (strip_all,
# force_https_scheme_rel) exercising realistic multi-option combinations.
parse_corpus_combos <- function() {
  list(
    defaults = list(),
    proto_none = list(protocol_handling = "none"),
    proto_strip_www_strip = list(
      protocol_handling = "strip", www_handling = "strip"
    ),
    www_if_no_subdomain = list(www_handling = "if_no_subdomain"),
    tld_icann_sub1 = list(tld_source = "icann", subdomain_levels_to_keep = 1),
    tld_private_sub0 = list(
      tld_source = "private", subdomain_levels_to_keep = 0
    ),
    case_upper_keepslash = list(
      case_handling = "upper", trailing_slash_handling = "keep",
      index_page_handling = "keep"
    ),
    case_lower_unicode = list(
      case_handling = "lower", host_encoding = "unicode"
    ),
    idna_encode_path = list(host_encoding = "idna", path_encoding = "encode"),
    pathnorm_both_decode = list(
      path_normalization = "both", path_encoding = "decode"
    ),
    strip_all = list(
      protocol_handling = "strip", www_handling = "strip",
      trailing_slash_handling = "strip", index_page_handling = "strip",
      path_normalization = "both"
    ),
    force_https_scheme_rel = list(
      protocol_handling = "https", scheme_relative_handling = "https",
      www_handling = "if_no_subdomain"
    )
  )
}
