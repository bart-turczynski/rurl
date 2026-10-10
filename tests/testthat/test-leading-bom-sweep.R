# RURL-zxnxczsw: path helpers that read past a U+FEFF that starts their input.
#
# Some stringi functions skip a U+FEFF (the byte-order mark) at the start of
# their input (see .starts_with_bom()). A path led by the mark is ordinary path
# data under the WHATWG URL Standard (path state) and RFC 3986 (section 3.3),
# so the path dials must treat it like any other code point. Each U+FEFF row
# has a U+200B twin: a zero-width code point stringi does not skip, so a twin
# that already gives the right answer shows the defect is the mark's alone.

bom <- "﻿"
zwsp <- "​"

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
      paste0("/", zwsp), label = an
    )
    expect_identical(
      arm_path(paste0("file:", zwsp, "/a/index.html"), arm,
               index_page_handling = "strip"),
      paste0("/", zwsp, "/a"), label = an
    )
    expect_identical(
      arm_path(paste0("file:", zwsp, "/a"), arm, path_encoding = "encode"),
      paste0(lead, "%E2%80%8B/a"), label = an
    )
    expect_identical(
      arm_path(paste0("file:", zwsp, "/a/"), arm,
               trailing_slash_handling = "strip"),
      paste0(lead, zwsp, "/a"), label = an
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
      paste0("/", bom, "/a"), label = an
    )
    expect_identical(
      arm_path("file:/\ufeff/a", arm, path_encoding = "encode"),
      "/%EF%BB%BF/a", label = an
    )
    expect_identical(
      arm_path("file:/\ufeff/a/", arm,
               trailing_slash_handling = "strip"),
      paste0("/", bom, "/a"), label = an
    )
  }
  expect_identical(
    arm_path(paste0("http://h.example/", bom, "a/b/"), "rfc3986",
             trailing_slash_handling = "strip"),
    paste0("/", bom, "a/b")
  )
})

test_that("whatwg: a mark-led file: path is rooted before the dials see it", {
  # The file state gives `file:﻿/a` the path `/﻿/a`, so the leading
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

test_that("rfc3986: clean_url does not take the path dials on these rows", {
  # Signature: the fixes move the parsed `path` only. These `clean_url` values
  # are the source spelling, before and after; with the default
  # `scheme_acceptance = "web"` the same rows have no `clean_url` at all.
  cu <- function(url, ...) {
    safe_parse_url(url, url_standard = "rfc3986",
                   scheme_acceptance = "general", ...)$clean_url
  }
  expect_identical(
    safe_parse_url(paste0("file:", bom, "/a"), url_standard = "rfc3986",
                   path_encoding = "encode")$clean_url,
    NA_character_
  )
  expect_identical(cu(paste0("file:", bom, "/index.html"),
                      index_page_handling = "strip"),
                   paste0("file:", bom, "/index.html"))
  expect_identical(cu(paste0("file:", bom, "/a"), path_encoding = "encode"),
                   paste0("file:", bom, "/a"))
  expect_identical(cu(paste0("file:", bom, "/a/"),
                      trailing_slash_handling = "strip"),
                   paste0("file:", bom, "/a/"))
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
  # `new URL()` in Node 26 throws on `﻿ http://h/`, `​ http://h/`
  # and `﻿http://h/` alike. The strip and the `leading-trailing-stripped`
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
