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
    c("\uffffa", paste0("a", bom, "b"))
  )
  # NA, "" and non-ASCII letters pass through the fallback unchanged.
  expect_identical(
    .ascii_tolower(c(poison_chr, NA, "", "\u00c4B")),
    c("\uffffa", NA, "", "\u00c4b")
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
  # Each row matches its value alone (pinned above). Before RURL-bqhfznkg the
  # mark-led row came back as `http://h.example/`, host `h.example`.
  expect_identical(get_clean_url(c(mark_url, poison_url)),
                   c(paste0("http://", bom, "h.example/"), poison_url))
  expect_identical(
    serialize_url(c(mark_url, poison_url), standard = "rfc3986",
                  form = "normalized")[[1L]],
    paste0("http://", bom, "h.example/")
  )
  expect_identical(
    safe_parse_urls(c(mark_url, poison_url), url_standard = "rfc3986")$host,
    c(paste0(bom, "h.example"), "\uffff.example")
  )
})

test_that("NULL: the frozen selector reaches the fallback (ADR 0016)", {
  skip_if_not(chartr_rejects(), "chartr() accepts U+FFFF on this build")
  # Witness: under the frozen selector, the default call and an explicit NULL
  # agree. Before RURL-bqhfznkg both returned host `h.example` and clean_url
  # `http://h.example/` for the mark-led row whenever its vector held a row
  # chartr() rejects, while the row alone kept the mark. No standard or
  # documented contract lets one row change another, so this is a default-path
  # defect, not selector-caused drift.
  for (p in list(
    safe_parse_urls(c(mark_url, poison_url)),
    safe_parse_urls(c(mark_url, poison_url), url_standard = NULL)
  )) {
    expect_identical(p$host[[1L]], paste0(bom, "h.example"))
    expect_identical(p$clean_url[[1L]], paste0("http://", bom, "h.example/"))
  }
  # Signature: only `host` and `clean_url` of a row whose host starts with
  # U+FEFF move, only when its vector holds a row chartr() rejects, and only
  # to the row's own value alone. Every column of both rows now matches the
  # rows parsed alone; the status never moved. Per arm: whatwg was already
  # right (UTS-46 drops the mark first, pinned above); rfc3986 reads the same
  # host and moves the same way (pinned in the previous test).
  alone <- safe_parse_urls(mark_url)
  mixed <- safe_parse_urls(c(mark_url, poison_url))
  for (col in names(alone)) {
    expect_identical(mixed[[col]][[1L]], alone[[col]], label = col)
  }
  poison_alone <- safe_parse_urls(poison_url)
  for (col in names(poison_alone)) {
    expect_identical(mixed[[col]][[2L]], poison_alone[[col]], label = col)
  }
  expect_identical(alone$parse_status, "warning-invalid-tld")
})
