# RURL-zxnxczsw: path helpers that read past a U+FEFF that starts their input.
#
# Some stringi functions skip a U+FEFF (the byte-order mark) at the start of
# their input (see .starts_with_bom()). A path led by the mark is ordinary path
# data under the WHATWG URL Standard (path state) and under the NULL selector,
# so the path dials must treat it like any other code point. Under rfc3986 it
# is a parse error (RUL-030; RFC 3986 sections 2 and 2.5). Each U+FEFF row
# has a U+200B twin: a zero-width code point stringi does not skip, so a twin
# that already gives the right answer shows the defect is the mark's alone.

bom <- "\ufeff"
zwsp <- "\u200b"

# The parsed path under one arm. `NULL` omits the selector, as the default call
# does; the NULL test below also passes it explicitly.
arm_path <- function(url, arm, ...) {
  if (is.null(arm)) {
    safe_parse_url(url, ...)$path
  } else {
    safe_parse_url(url, url_standard = arm, ...)$path
  }
}

arms <- list(whatwg = "whatwg", rfc3986 = "rfc3986", "NULL" = NULL)

# RUL-030 (owner ruling 2026-10-12, RURL-pntbujzc): under rfc3986 a raw
# non-ASCII code point, U+FEFF and U+200B included, is a parse error (RFC 3986
# sections 2, 2.5 and 3.2.2: a URI is US-ASCII and non-ASCII data is written
# as percent-encoded UTF-8), so `safe_parse_url()` returns NULL for such a row
# there and the path dials never see it. The NULL arm keeps the coverage.
rfc_null <- function(an, value) {
  if (identical(an, "rfc3986")) NULL else value
}

# --- Pins: behavior the fixes must not move -----------------------------------

test_that("the U+200B twins already keep the code point in every arm", {
  # Under whatwg the file state roots the path first, so the code point is not
  # the first character any helper sees. Under rfc3986 and NULL the path is
  # rootless. `._strip_index_page()` roots the directory it keeps
  # (`foo/index.html` gives `/foo`, pinned in test-rurl-coverage.R); the
  # encoder and the trailing-slash strip keep a rootless path rootless.
  rooted <- list(whatwg = TRUE, rfc3986 = FALSE, "NULL" = FALSE)
  for (an in names(arms)) {
    arm <- arms[[an]]
    lead <- if (rooted[[an]]) "/" else ""
    expect_identical(
      arm_path(paste0("file:", zwsp, "/index.html"), arm,
               index_page_handling = "strip"),
      rfc_null(an, paste0("/", zwsp)), label = an
    )
    expect_identical(
      arm_path(paste0("file:", zwsp, "/a/index.html"), arm,
               index_page_handling = "strip"),
      rfc_null(an, paste0("/", zwsp, "/a")), label = an
    )
    expect_identical(
      arm_path(paste0("file:", zwsp, "/a"), arm, path_encoding = "encode"),
      rfc_null(an, paste0(lead, "%E2%80%8B/a")), label = an
    )
    expect_identical(
      arm_path(paste0("file:", zwsp, "/a/"), arm,
               trailing_slash_handling = "strip"),
      rfc_null(an, paste0(lead, zwsp, "/a")), label = an
    )
  }
})

test_that("negative controls: paths with no mark, or a mark not first", {
  for (an in names(arms)) {
    arm <- arms[[an]]
    expect_identical(
      arm_path("file:/index.html", arm, index_page_handling = "strip"), "/",
      label = an
    )
    expect_identical(
      arm_path("file:/a/index.html", arm, index_page_handling = "strip"), "/a",
      label = an
    )
    expect_identical(
      arm_path("file:/a", arm, path_encoding = "encode"), "/a", label = an
    )
    expect_identical(
      arm_path("file:/a/", arm, trailing_slash_handling = "strip"), "/a",
      label = an
    )
    expect_identical(
      arm_path("file:/\ufeff/a/index.html", arm,
               index_page_handling = "strip"),
      rfc_null(an, paste0("/", bom, "/a")), label = an
    )
    expect_identical(
      arm_path("file:/\ufeff/a", arm, path_encoding = "encode"),
      rfc_null(an, "/%EF%BB%BF/a"), label = an
    )
    expect_identical(
      arm_path("file:/\ufeff/a/", arm,
               trailing_slash_handling = "strip"),
      rfc_null(an, paste0("/", bom, "/a")), label = an
    )
  }
  # RUL-030: the raw mark makes this a parse error under rfc3986.
  expect_null(
    arm_path(paste0("http://h.example/", bom, "a/b/"), "rfc3986",
             trailing_slash_handling = "strip")
  )
})

test_that("whatwg: a mark-led file: path is rooted before the dials see it", {
  # The file state gives `file:\ufeff/a` the path `/\ufeff/a`, so the leading
  # character the helpers see is `/`, and this arm was already right.
  expect_identical(
    arm_path(paste0("file:", bom, "/index.html"), "whatwg",
             index_page_handling = "strip"),
    paste0("/", bom)
  )
  expect_identical(
    arm_path(paste0("file:", bom, "/a/index.html"), "whatwg",
             index_page_handling = "strip"),
    paste0("/", bom, "/a")
  )
  expect_identical(
    arm_path(paste0("file:", bom, "/a"), "whatwg", path_encoding = "encode"),
    "/%EF%BB%BF/a"
  )
  expect_identical(
    arm_path(paste0("file:", bom, "/a/"), "whatwg",
             trailing_slash_handling = "strip"),
    paste0("/", bom, "/a")
  )
})

test_that("rfc3986: these rows are parse errors on both acceptance routes", {
  # These `clean_url` values were the source spelling under general
  # acceptance, and absent under `web`. Under RUL-030 the raw U+FEFF is a
  # parse error (RFC 3986 sections 2, 2.5), so there is no record at all.
  cu <- function(url, ...) {
    safe_parse_url(url, url_standard = "rfc3986",
                   scheme_acceptance = "general", ...)$clean_url
  }
  expect_null(
    safe_parse_url(paste0("file:", bom, "/a"), url_standard = "rfc3986",
                   path_encoding = "encode")$clean_url
  )
  expect_null(cu(paste0("file:", bom, "/index.html"),
                 index_page_handling = "strip"))
  expect_null(cu(paste0("file:", bom, "/a"), path_encoding = "encode"))
  expect_null(cu(paste0("file:", bom, "/a/"),
                 trailing_slash_handling = "strip"))
  expect_identical(
    get_parse_status(paste0("file:", bom, "/a"), url_standard = "rfc3986",
                     scheme_acceptance = "general"),
    "error"
  )
})

test_that("the WHATWG dot remover already keeps a mark-led segment", {
  # Every cut in `._remove_dot_segments_whatwg()` starts on a `.`, `%` or `/`
  # its anchored pattern matched, so no cut starts on the mark.
  rows <- list(
    c("%s/./b", "%s/b"), c("%s../x", "%s../x"), c("%s./x", "%s./x"),
    c("%s/.", "%s/"), c("%s/../b", "/b"), c("./%s/b", "%s/b"),
    c("/./%s/b", "/%s/b"), c("%s%%2e/x", "%s%%2e/x"), c("%s/%%2e/b", "%s/b"),
    c("%%2e/%s/b", "%s/b"), c("%%2e%%2e/%s/b", "%s/b")
  )
  fill <- function(template, mark) {
    gsub("%%", "%", gsub("%s", mark, template, fixed = TRUE), fixed = TRUE)
  }
  for (mark in c(bom, zwsp)) {
    for (row in rows) {
      expect_identical(
        ._remove_dot_segments_whatwg(fill(row[[1L]], mark)),
        fill(row[[2L]], mark),
        label = paste(utf8ToInt(mark)[1L], row[[1L]])
      )
    }
  }
})

test_that("whatwg step 1: a U+FEFF is neither stripped nor a strip fact", {
  # The basic URL parser's step 1 removes leading and trailing C0 control or
  # space; U+FEFF is neither, so a mark before a space shields the space, and
  # `new URL()` in Node 26 throws on `\ufeff http://h/`, `\u200b http://h/`
  # and `\ufeffhttp://h/` alike. The strip and the `leading-trailing-stripped`
  # flag both use stringi regex functions, which count the mark, so they agree.
  u <- c(paste0(bom, " http://h/"), paste0(zwsp, " http://h/"),
         " http://h/", paste0(bom, "http://h/"), paste0("http://h/ ", bom),
         paste0(bom, "\thttp://h/"))
  s <- .strip_whatwg_control_chars_vec(u, "whatwg")
  expect_identical(
    s$url,
    c(paste0(bom, " http://h/"), paste0(zwsp, " http://h/"), "http://h/",
      paste0(bom, "http://h/"), paste0("http://h/ ", bom),
      paste0(bom, "http://h/"))
  )
  expect_identical(s$leading_trailing_stripped,
                   c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(s$control_char_stripped,
                   c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(
    serialize_url(u, standard = "whatwg"),
    c(NA, NA, "http://h/", NA, "http://h/%20%EF%BB%BF", NA)
  )
  d <- get_url_diagnostics(u, url_standard = "whatwg")
  expect_false("leading-trailing-stripped" %in% d[[1L]])
  expect_true("leading-trailing-stripped" %in% d[[3L]])
})

# --- Defects: the index-page strip and the legacy segment encoder -------------

test_that("index-page strip keeps a mark that starts the kept directory", {
  # The directory kept from `\ufeff/a/index.html` lost the mark: the stringi
  # match read past it. The helper roots what it keeps, as its U+200B twin
  # shows, so the mark-led directory comes back as `/\ufeff/a`.
  expect_identical(._strip_index_page("\ufeff/index.html"), "/\ufeff")
  expect_identical(._strip_index_page("\ufeff/a/index.html"), "/\ufeff/a")
  expect_identical(._strip_index_page("\ufeff/a/default.htm/"), "/\ufeff/a")
  expect_identical(._strip_index_page("\ufeff/../index.html"), "/")
  expect_identical(._strip_index_page("\u200b/a/index.html"), "/\u200b/a")
  expect_identical(._strip_index_page("\ufeffindex.html"), "\ufeffindex.html")
  expect_identical(._strip_index_page("/a/INDEX.PHP"), "/a")
  expect_identical(._strip_index_page("index.html"), "index.html")
  expect_identical(._strip_index_page("a/index.html"), "/a")
  for (an in c("rfc3986", "NULL")) {
    arm <- arms[[an]]
    expect_identical(
      arm_path("file:\ufeff/index.html", arm, index_page_handling = "strip"),
      rfc_null(an, "/\ufeff"), label = an
    )
    expect_identical(
      arm_path("file:\ufeff/a/index.html", arm, index_page_handling = "strip"),
      rfc_null(an, "/\ufeff/a"), label = an
    )
  }
})

test_that("the segment encoder keeps a mark-led path rootless", {
  # RFC 3986 section 3.3: a path-rootless path does not begin with "/".
  # The encoder read `\ufeff/a` as rooted and returned `/%EF%BB%BF/a`.
  expect_identical(._encode_path_segments("\ufeff/a"), "%EF%BB%BF/a")
  expect_identical(._encode_path_segments("\ufeff/a/"), "%EF%BB%BF/a/")
  expect_identical(._encode_path_segments("\ufeff"), "%EF%BB%BF")
  expect_identical(._encode_path_segments("\u200b/a"), "%E2%80%8B/a")
  expect_identical(._encode_path_segments("/\ufeff/a"), "/%EF%BB%BF/a")
  expect_identical(._encode_path_segments("/a b/"), "/a%20b/")
  for (an in c("rfc3986", "NULL")) {
    expect_identical(
      arm_path("file:\ufeff/a", arms[[an]], path_encoding = "encode"),
      rfc_null(an, "%EF%BB%BF/a"), label = an
    )
  }
  expect_null(
    arm_path("foo:\ufeff/a/b/", "rfc3986", path_encoding = "encode",
             scheme_acceptance = "general")
  )
})

test_that("NULL: the index strip and the encoder keep the mark (ADR 0016)", {
  # Witness: under the frozen selector the parsed path of `file:\ufeff/a`
  # is `\ufeff/a`, and the two dials dropped or rooted the mark. No standard
  # or documented contract sanctions either, so this is a default-path defect,
  # not selector-caused drift. Omitting the selector and passing NULL agree.
  for (p in list(
    safe_parse_url("file:\ufeff/a/index.html", index_page_handling = "strip"),
    safe_parse_url("file:\ufeff/a/index.html", url_standard = NULL,
                   index_page_handling = "strip")
  )) {
    expect_identical(p$path, "/\ufeff/a")
  }
  for (p in list(
    safe_parse_url("file:\ufeff/a", path_encoding = "encode"),
    safe_parse_url("file:\ufeff/a", url_standard = NULL,
                   path_encoding = "encode")
  )) {
    expect_identical(p$path, "%EF%BB%BF/a")
  }
  # Signature: only the `path` of a path that starts with the mark moves, and
  # only under `index_page_handling = "strip"` or `path_encoding = "encode"`.
  # These rows are parse errors under NULL before and after; the status and
  # `clean_url` do not move, nor does the path without the dial. Per arm:
  # whatwg was already right (its file state roots the path first, pinned
  # above); rfc3986 rejects the row (RUL-030, pinned above).
  p <- safe_parse_url("file:\ufeff/a/index.html", index_page_handling = "strip")
  expect_identical(p$parse_status, "error")
  expect_identical(p$clean_url, NA_character_)
  expect_identical(safe_parse_url("file:\ufeff/a/index.html")$path,
                   "\ufeff/a/index.html")
  expect_identical(safe_parse_url("file:\ufeff/a")$path, "\ufeff/a")
})

# --- Defect: the trailing-slash strip -----------------------------------------

test_that("the trailing-slash strip keeps a mark that starts the path", {
  # The strip cut with stri_sub(), which read past the mark while
  # stri_length() counted it, so `\ufeff/a/` became `/a/`: the mark was lost
  # and the slash kept.
  for (an in c("rfc3986", "NULL")) {
    arm <- arms[[an]]
    expect_identical(
      arm_path("file:\ufeff/a/", arm, trailing_slash_handling = "strip"),
      rfc_null(an, "\ufeff/a"), label = an
    )
    expect_identical(
      arm_path("file:\ufeff/", arm, trailing_slash_handling = "strip"),
      rfc_null(an, "\ufeff"), label = an
    )
    expect_identical(
      arm_path("file:\ufeff", arm, trailing_slash_handling = "keep"),
      rfc_null(an, "\ufeff/"), label = an
    )
    expect_identical(
      arm_path("file:\u200b/", arm, trailing_slash_handling = "strip"),
      rfc_null(an, "\u200b"), label = an
    )
  }
  # Under rfc3986 these rows are parse errors (RUL-030), general route
  # included.
  expect_null(
    arm_path("foo:\ufeff/a/b/", "rfc3986", trailing_slash_handling = "strip",
             scheme_acceptance = "general")
  )
  # The strip and the index strip compose: the index strip runs first.
  expect_identical(
    arm_path("file:\ufeff/a/index.html/", NULL,
             index_page_handling = "strip",
             trailing_slash_handling = "strip"),
    "/\ufeff/a"
  )
  expect_null(
    arm_path("file:\ufeff/a/index.html/", "rfc3986",
             index_page_handling = "strip",
             trailing_slash_handling = "strip")
  )
})

test_that("NULL: the trailing-slash strip keeps the mark (ADR 0016)", {
  # Witness: the frozen selector's path of `file:\ufeff/a/` is `\ufeff/a/`
  # and the strip returned `/a/`, which no standard or contract sanctions.
  # Omitting the selector and passing NULL agree.
  for (p in list(
    safe_parse_url("file:\ufeff/a/", trailing_slash_handling = "strip"),
    safe_parse_url("file:\ufeff/a/", url_standard = NULL,
                   trailing_slash_handling = "strip")
  )) {
    expect_identical(p$path, "\ufeff/a")
    # Signature: only the path moves; the row is a parse error under NULL
    # before and after, with no `clean_url`. whatwg was already right (its
    # path is rooted, pinned above) and rfc3986 rejects the row (RUL-030).
    expect_identical(p$parse_status, "error")
    expect_identical(p$clean_url, NA_character_)
  }
  expect_identical(safe_parse_url("file:\ufeff/a/")$path, "\ufeff/a/")
  expect_identical(
    safe_parse_url("http://h.example/a/",
                   trailing_slash_handling = "strip")$path,
    "/a"
  )
})

test_that("the dot removers keep a path longer than 1e6 characters whole", {
  # Before R 4.6.0, `substring(x, first)` defaulted to `last = 1000000L`, so a
  # base-R suffix cut without an explicit `last` truncated a long path.
  long <- strrep("a", 1100000L)
  dotted <- sprintf("/./%s", long)
  rooted <- sprintf("/%s", long)
  expect_identical(._remove_dot_segments_whatwg(dotted), rooted)
  expect_identical(._remove_dot_segments(dotted), rooted)
})
