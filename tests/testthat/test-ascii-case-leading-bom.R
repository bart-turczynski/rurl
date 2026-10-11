# RURL-bqhfznkg: the ASCII case helpers' fallback is a cross-row path.
#
# `.ascii_tolower()` / `.ascii_toupper()` map case with `chartr()`, which
# throws on a code point `utf8towcs()` refuses (the U+FFFF noncharacter on this
# build). The throw is for the whole vector, so one such row sends every row
# through the `stri_replace_all_fixed()` fallback, and that call reads past a
# U+FEFF that starts its input (see `.starts_with_bom()`). One row could drop
# the leading mark of every other row. Each U+FEFF row has a U+200B twin: a
# zero-width code point stringi does not skip, so a twin that already gives the
# right answer shows the defect is the mark's alone.

bom <- "﻿"
zwsp <- "​"
# A row `chartr()` rejects, so its vector takes the fallback.
poison_chr <- "￿A"
poison_url <- "http://￿.example/"

chartr_rejects <- function() {
  inherits(try(chartr("a", "a", "￿"), silent = TRUE), "try-error")
}

mark_url <- "http://﻿H.example/"
twin_url <- "http://​H.example/"

# --- Pins: behavior the fix must not move -------------------------------------

test_that("the U+200B twins already keep their code point in the fallback", {
  expect_identical(
    .ascii_tolower(c(poison_chr, paste0(zwsp, "AB"))),
    c("￿a", paste0(zwsp, "ab"))
  )
  expect_identical(
    .ascii_toupper(c("￿a", paste0(zwsp, "ab"))),
    c("￿A", paste0(zwsp, "AB"))
  )
  expect_identical(
    get_clean_url(c(twin_url, poison_url))[[1L]],
    paste0("http://", zwsp, "h.example/")
  )
  expect_identical(
    serialize_url(c(twin_url, poison_url), standard = "rfc3986",
                  form = "normalized")[[1L]],
    paste0("http://", zwsp, "h.example/")
  )
})

test_that("negative controls: no fallback, or a mark that is not first", {
  # Alone, a mark-led row takes the chartr() path, which counts the mark.
  expect_identical(.ascii_tolower(paste0(bom, "AB")), paste0(bom, "ab"))
  expect_identical(.ascii_toupper(paste0(bom, "ab")), paste0(bom, "AB"))
  # In the fallback, a mark after the first code point is kept.
  expect_identical(
    .ascii_tolower(c(poison_chr, paste0("A", bom, "B"))),
    c("￿a", paste0("a", bom, "b"))
  )
  # NA, "" and non-ASCII letters pass through the fallback unchanged.
  expect_identical(
    .ascii_tolower(c(poison_chr, NA, "", "ÄB")),
    c("￿a", NA, "", "Äb")
  )
  expect_identical(.ascii_tolower(character(0)), character(0))
  # The public rows alone, and a vector with no rejected row.
  expect_identical(get_clean_url(mark_url),
                   paste0("http://", bom, "h.example/"))
  expect_identical(get_clean_url(c(mark_url, "http://B.example/")),
                   c(paste0("http://", bom, "h.example/"),
                     "http://b.example/"))
  expect_identical(
    serialize_url(mark_url, standard = "rfc3986", form = "normalized"),
    paste0("http://", bom, "h.example/")
  )
  expect_identical(get_clean_url(poison_url), poison_url)
})

test_that("whatwg: the host parser drops the mark before the case map", {
  # UTS-46 maps U+FEFF (and U+200B) to nothing, so this arm never hands the
  # case helpers a mark-led host and was already row-independent.
  for (u in list(mark_url, twin_url)) {
    alone <- safe_parse_urls(u, url_standard = "whatwg")
    mixed <- safe_parse_urls(c(u, poison_url), url_standard = "whatwg")
    expect_identical(alone$host, "h.example")
    expect_identical(mixed[1L, ], alone)
  }
})

# --- Defect: one rejected row drops every other row's leading mark ------------

test_that("the fallback keeps every element's leading mark", {
  skip_if_not(chartr_rejects(), "chartr() accepts U+FFFF on this build")
  # DEFECT: the mark of the second element is dropped because the first
  # element sent the whole vector to stri_replace_all_fixed().
  expect_identical(.ascii_tolower(c(poison_chr, paste0(bom, "A"))),
                   c("￿a", "a"))
  expect_identical(.ascii_toupper(c("￿a", paste0(bom, "a"))),
                   c("￿A", "A"))
  # DEFECT: two leading marks lose one.
  expect_identical(.ascii_tolower(c(poison_chr, paste0(bom, bom, "A"))),
                   c("￿a", paste0(bom, "a")))
})

test_that("one rejected row changes another row's public output", {
  skip_if_not(chartr_rejects(), "chartr() accepts U+FFFF on this build")
  # DEFECT: alone, the row keeps its mark (pinned above); next to a row
  # chartr() rejects it loses it.
  expect_identical(get_clean_url(c(mark_url, poison_url))[[1L]],
                   "http://h.example/")
  expect_identical(
    serialize_url(c(mark_url, poison_url), standard = "rfc3986",
                  form = "normalized")[[1L]],
    "http://h.example/"
  )
  expect_identical(
    safe_parse_urls(c(mark_url, poison_url), url_standard = "rfc3986")$host,
    c("h.example", "￿.example")
  )
})

test_that("NULL: the frozen selector reaches the fallback (ADR 0016)", {
  skip_if_not(chartr_rejects(), "chartr() accepts U+FFFF on this build")
  # Witness: under the frozen selector, the default call and an explicit NULL
  # agree, and both drop the mark of a row that shares its vector with a row
  # chartr() rejects. DEFECT: the row's own value is `http://﻿h.example/`.
  for (p in list(
    safe_parse_urls(c(mark_url, poison_url)),
    safe_parse_urls(c(mark_url, poison_url), url_standard = NULL)
  )) {
    expect_identical(p$host[[1L]], "h.example")
    expect_identical(p$clean_url[[1L]], "http://h.example/")
  }
  # Signature: only `host` and `clean_url` of the mark-led row move, and only
  # when the vector holds a row chartr() rejects. Every other column of that
  # row, and the rejected row itself, match the rows parsed alone.
  alone <- safe_parse_urls(mark_url)
  mixed <- safe_parse_urls(c(mark_url, poison_url))
  for (col in setdiff(names(alone), c("host", "clean_url"))) {
    expect_identical(mixed[[col]][[1L]], alone[[col]], label = col)
  }
  poison_alone <- safe_parse_urls(poison_url)
  for (col in names(poison_alone)) {
    expect_identical(mixed[[col]][[2L]], poison_alone[[col]], label = col)
  }
  expect_identical(alone$parse_status, "warning-invalid-tld")
})
