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

bom <- "\ufeff"
zwsp <- "\u200b"
# A row `chartr()` rejects, so its vector takes the fallback.
poison_chr <- "\uffffA"
poison_url <- "http://\uffff.example/"

chartr_rejects <- function() {
  inherits(try(chartr("a", "a", "\uffff"), silent = TRUE), "try-error")
}

mark_url <- "http://\ufeffH.example/"
twin_url <- "http://\u200bH.example/"

# --- Pins: behavior the fix must not move -------------------------------------

test_that("the U+200B twins already keep their code point in the fallback", {
  expect_identical(
    .ascii_tolower(c(poison_chr, paste0(zwsp, "AB"))),
    c("\uffffa", paste0(zwsp, "ab"))
  )
  expect_identical(
    .ascii_toupper(c("\uffffa", paste0(zwsp, "ab"))),
    c("\uffffA", paste0(zwsp, "AB"))
  )
  # RUL-030 (owner ruling 2026-10-12, RURL-pntbujzc): rfc3986 rejects a raw
  # non-ASCII code point (RFC 3986 sections 2, 2.5, 3.2.2), so the rfc3986
  # serializer has no record here. The pipeline reached the fallback with a
  # mark-led host only under the frozen NULL profile, which ADR 0020 retired;
  # the default whatwg maps the mark away first (pinned below), so the helper
  # pins carry the coverage.
  expect_identical(
    serialize_url(c(twin_url, poison_url), standard = "rfc3986",
                  form = "normalized")[[1L]],
    NA_character_
  )
})

test_that("negative controls: no fallback, or a mark that is not first", {
  # Alone, a mark-led row takes the chartr() path, which counts the mark.
  expect_identical(.ascii_tolower(paste0(bom, "AB")), paste0(bom, "ab"))
  expect_identical(.ascii_toupper(paste0(bom, "ab")), paste0(bom, "AB"))
  # In the fallback, a mark after the first code point is kept.
  expect_identical(
    .ascii_tolower(c(poison_chr, paste0("A", bom, "B"))),
    c("\uffffa", paste0("a", bom, "b"))
  )
  # NA, "" and non-ASCII letters pass through the fallback unchanged.
  expect_identical(
    .ascii_tolower(c(poison_chr, NA, "", "\u00c4B")),
    c("\uffffa", NA, "", "\u00c4b")
  )
  expect_identical(.ascii_tolower(character(0)), character(0))
  # The public rows alone, and a vector with no rejected row: the default
  # whatwg host parser maps the mark to nothing (UTS-46).
  expect_identical(get_clean_url(mark_url), "http://h.example/")
  expect_identical(get_clean_url(c(mark_url, "http://B.example/")),
                   c("http://h.example/", "http://b.example/"))
  # RUL-030 (owner ruling 2026-10-12, RURL-pntbujzc): rfc3986 rejects a raw
  # non-ASCII code point (RFC 3986 sections 2, 2.5, 3.2.2), so the rfc3986
  # serializer has no record here. The pipeline reached the fallback with a
  # mark-led host only under the frozen NULL profile, which ADR 0020 retired;
  # the default whatwg maps the mark away first (pinned below), so the helper
  # pins carry the coverage.
  expect_identical(
    serialize_url(mark_url, standard = "rfc3986", form = "normalized"),
    NA_character_
  )
  # U+FFFF is disallowed by UTS-46, so the WHATWG host parse fails.
  expect_identical(get_clean_url(poison_url), NA_character_)
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

# --- Fixed: one rejected row no longer drops another row's leading mark -------

test_that("the fallback keeps every element's leading mark", {
  # Called directly, so this holds on a build whose chartr() takes U+FFFF.
  # Before RURL-bqhfznkg stri_replace_all_fixed() read past the first mark:
  # "\ufeffA" came back as "a" and "\ufeff\ufeffA" as "\ufeffa".
  x <- c("\uffffA", "\ufeffA", "\ufeff\ufeffA", NA, "", "\u200bA", "A\ufeffB")
  expect_identical(
    .ascii_case_fallback(x, LETTERS, letters),
    c("\uffffa", "\ufeffa", "\ufeff\ufeffa", NA, "", "\u200ba", "a\ufeffb")
  )
  expect_identical(
    .ascii_case_fallback(c("\ufeffa", "\ufeff0z"), letters, LETTERS),
    c("\ufeffA", "\ufeff0Z")
  )
  expect_identical(.ascii_case_fallback(character(0), LETTERS, letters),
                   character(0))
})

test_that("one rejected element does not change another element's result", {
  skip_if_not(chartr_rejects(), "chartr() accepts U+FFFF on this build")
  expect_identical(.ascii_tolower(c(poison_chr, paste0(bom, "A"))),
                   c("\uffffa", paste0(bom, "a")))
  expect_identical(.ascii_toupper(c("\uffffa", paste0(bom, "a"))),
                   c("\uffffA", paste0(bom, "A")))
  expect_identical(.ascii_tolower(c(poison_chr, paste0(bom, bom, "A"))),
                   c("\uffffa", paste0(bom, bom, "a")))
  # Each element as if it were alone in the vector.
  x <- c(poison_chr, paste0(bom, "AB"), paste0(zwsp, "AB"), "A\ufeffB", NA)
  expect_identical(.ascii_tolower(x),
                   vapply(x, .ascii_tolower, character(1L),
                          USE.NAMES = FALSE))
})

test_that("one rejected row no longer changes another row's public output", {
  skip_if_not(chartr_rejects(), "chartr() accepts U+FFFF on this build")
  # Each row matches its value alone (pinned above).
  expect_identical(get_clean_url(c(mark_url, poison_url)),
                   c("http://h.example/", NA_character_))
  # RUL-030 (owner ruling 2026-10-12, RURL-pntbujzc): rfc3986 rejects a raw
  # non-ASCII code point (RFC 3986 sections 2, 2.5, 3.2.2), so the rfc3986
  # serializer has no record here. The pipeline reached the fallback with a
  # mark-led host only under the frozen NULL profile, which ADR 0020 retired;
  # the default whatwg maps the mark away first (pinned below), so the helper
  # pins carry the coverage.
  expect_identical(
    serialize_url(c(mark_url, poison_url), standard = "rfc3986",
                  form = "normalized")[[1L]],
    NA_character_
  )
  expect_identical(
    safe_parse_urls(c(mark_url, poison_url), url_standard = "rfc3986")$host,
    c(NA_character_, NA_character_)
  )
})
