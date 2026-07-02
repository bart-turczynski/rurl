# Characterization (golden) oracle for the URL parser.
#
# Purpose: lock the CURRENT public output of safe_parse_urls() and
# safe_parse_url()
# before the vector-core refactor (RURL-gvlqokul and its children) touches it.
# The corpus lives in tests/testthat/fixtures/parse-corpus.csv (loaded via
# helper-parse-corpus.R) and spans every interesting parser branch: scheme
# variants, apex/subdomain/www/IDN/IP/private-suffix/unknown-TLD/malformed
# hosts, path normalization + percent-encoding, query/fragment/userinfo/port
# shapes, and NA / "" / garbage inputs. It is run through the option matrix in
# parse_corpus_combos() and the combined result is snapshotted with
# style = "json2", which compares deserialized VALUES (not just printed text) --
# so any change to observable output fails with a readable diff.
#
# These snapshots codify CURRENT behavior, known bugs included. When a later
# step intentionally changes output, regenerate with
# testthat::snapshot_accept("characterization-snapshot") AFTER confirming the
# diff is the intended behavior change.

# Build one combined data.frame: every combo applied to the full corpus, tagged
# with the combo name and a stable row index so the snapshot order is fixed.
.build_characterization_frame <- function(urls, combos) {
  parts <- lapply(names(combos), function(combo_name) {
    args <- c(list(url = urls), combos[[combo_name]])
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
  urls <- parse_corpus_urls()
  combos <- parse_corpus_combos()
  result <- .build_characterization_frame(urls, combos)

  # Sanity: shape is what corpus x combos should produce, so an accidental
  # corpus/combo edit is caught explicitly rather than silently re-snapshotted.
  expect_equal(nrow(result), length(urls) * length(combos))

  expect_snapshot_value(result, style = "json2")
})

test_that("safe_parse_urls() agrees row-for-row with safe_parse_url()", {
  urls <- parse_corpus_urls()
  combos <- parse_corpus_combos()
  fields <- rurl:::.spu_result_fields

  for (combo_name in names(combos)) {
    combo <- combos[[combo_name]]
    vframe <- do.call(safe_parse_urls, c(list(url = urls), combo))

    # original_url must round-trip the input on every row (NA stays NA).
    expect_equal(vframe$original_url, urls, info = combo_name)

    scalars <- lapply(urls, function(u) {
      do.call(safe_parse_url, c(list(url = u), combo))
    })

    for (f in fields) {
      if (f$name == "original_url") {
        next
      }
      sca_col <- vapply(
        scalars,
        function(s) {
          val <- if (is.null(s)) NULL else s[[f$name]]
          if (is.null(val)) f$default else val
        },
        f$template
      )
      expect_equal(
        vframe[[f$name]], sca_col,
        info = paste0(combo_name, ": ", f$name)
      )
    }
  }
})
