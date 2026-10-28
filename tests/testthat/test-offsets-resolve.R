# Pins for the segment split in .whatwg_file_remove_dot_segments()
# (RURL-yqmoztoj, ADR 0019): the last offset-bearing stringi call in
# R/resolve.R. The split keeps a TRAILING empty field (`/a/` is ["a", ""])
# and drops the LEADING one the spliced "/" opens. `strsplit()` reads that
# contract differently (ADR 0005), so these rows bite on either slip: a
# dropped trailing field loses the final "/", a kept leading one doubles the
# first. Non-ASCII segments must come back declared UTF-8, ASCII ones
# "unknown", and a U+FEFF or U+200B inside a segment is data.

test_that("the file dot-segment split keeps the trailing empty segment", {
  f <- .whatwg_file_remove_dot_segments
  cases <- c(
    "/" = "/", "//" = "//", "/a/" = "/a/", "/a//" = "/a//",
    "/a//b" = "/a//b", "/a/./" = "/a/", "/a/." = "/a/", "/a/.." = "/",
    "/a/../" = "/", "/C|/.." = "/C:/", "/C:/" = "/C:/", "/C|" = "/C:",
    "a/b/" = "a/b/", "/x/../.." = "/"
  )
  for (p in names(cases)) {
    expect_identical(f(p), cases[[p]], info = p)
  }
  expect_identical(f(""), "")
  expect_identical(f(NA_character_), NA_character_)
})

test_that("the file dot-segment split keeps marks and multibyte segments", {
  f <- .whatwg_file_remove_dot_segments
  bom_led <- f("\ufeffa/b/")
  expect_identical(bom_led, "\ufeffa/b/")
  expect_identical(Encoding(bom_led), "UTF-8")
  expect_identical(f("/\ufeffa/"), "/\ufeffa/")
  multi <- f("/\u00e9t\u00e9/./")
  expect_identical(multi, "/\u00e9t\u00e9/")
  expect_identical(Encoding(multi), "UTF-8")
  dropped <- f("/a\u200b/../b/")
  expect_identical(dropped, "/b/")
  expect_identical(Encoding(dropped), "unknown")
  expect_identical(f("/x/\ufeff/.."), "/x/")
})

file_refs <- c(
  "x/", "x//", "x//y", ".", "..", "./", "../", "x/./", "x/../", "x/.",
  "x/..", "\u00e9t\u00e9/", "\ufeffx/", "x/\ufeff/", "a\u200b/b//",
  "../../..", "C|/", "/C|/x/..", NA
)
file_base <- "file:///C:/dir/sub/"
file_head <- c(
  "file:///C:/dir/sub/x/", "file:///C:/dir/sub/x//",
  "file:///C:/dir/sub/x//y", "file:///C:/dir/sub/", "file:///C:/dir/",
  "file:///C:/dir/sub/", "file:///C:/dir/", "file:///C:/dir/sub/x/",
  "file:///C:/dir/sub/", "file:///C:/dir/sub/x/", "file:///C:/dir/sub/"
)

test_that("whatwg file resolution through the split is pinned", {
  clean <- resolve_url(file_refs, file_base, url_standard = "whatwg")
  expect_identical(clean, c(
    file_head,
    "file:///C:/dir/sub/\u00e9t\u00e9/", "file:///C:/dir/sub/\ufeffx/",
    "file:///C:/dir/sub/x/\ufeff/", "file:///C:/dir/sub/a\u200b/b//",
    "file:///C:/", "file:///C:/", "file:///C:/", NA
  ))
  expect_identical(which(Encoding(clean) == "UTF-8"), 12:15)
  ser <- resolve_url(file_refs, file_base, url_standard = "whatwg",
                     output = "serialized")
  expect_identical(ser, c(
    file_head,
    "file:///C:/dir/sub/%C3%A9t%C3%A9/", "file:///C:/dir/sub/%EF%BB%BFx/",
    "file:///C:/dir/sub/x/%EF%BB%BF/", "file:///C:/dir/sub/a%E2%80%8B/b//",
    "file:///C:/", "file:///C:/", "file:///C:/", NA
  ))
})

test_that("rfc3986 file resolution stays where it was; the default is whatwg", {
  # The default and NULL are "whatwg" (ADR 0020), pinned above.
  expect_identical(resolve_url(file_refs, file_base),
                   resolve_url(file_refs, file_base, url_standard = "whatwg"))
  expect_identical(resolve_url(file_refs, file_base, url_standard = NULL),
                   resolve_url(file_refs, file_base, url_standard = "whatwg"))
  # The plain RFC 3986 section 5.2 merge.
  want <- c(
    file_head,
    "file:///C:/dir/sub/\u00e9t\u00e9/", "file:///C:/dir/sub/\ufeffx/",
    "file:///C:/dir/sub/x/\ufeff/", "file:///C:/dir/sub/a\u200b/b//",
    "file:///", NA, NA, NA
  )
  # Under rfc3986 the four references holding a raw non-ASCII code point are
  # no RFC 3986 URI-reference (sections 2, 2.5, 4.1; RUL-030, RURL-pntbujzc),
  # so those rows are NA; the ASCII rows do not move.
  want_rfc <- want
  want_rfc[12:15] <- NA_character_
  expect_identical(resolve_url(file_refs, file_base, url_standard = "rfc3986"),
                   want_rfc)
  expect_identical(
    resolve_url(file_refs, file_base, url_standard = "rfc3986",
                output = "serialized"),
    want_rfc
  )
})
