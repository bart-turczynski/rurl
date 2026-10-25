# The ICU-emulating cuts and tests of R/parse-phases.R stay linear in the row
# and under PCRE's match limit (RURL-atlidhyh). Each block pins the helper's
# reading on short rows first, then a row long enough to have hit the limit.

test_that(".icu_cut_through_last_at() cuts through the last \"@\" of line 1", {
  cut <- .icu_cut_through_last_at
  # The last "@" wins; with none, nothing is cut.
  expect_identical(cut("u@h"), "h")
  expect_identical(cut("a@b@c"), "c")
  expect_identical(cut("@@@h"), "h")
  expect_identical(cut("u@"), "")
  expect_identical(cut("@"), "")
  expect_identical(cut("h"), "h")
  expect_identical(cut(""), "")
  expect_identical(cut(NA_character_), NA_character_)
  expect_identical(cut(c("a@b", NA, "c", "d@e@f")), c("b", NA, "c", "f"))
  expect_identical(cut(character()), character())
  # ICU's `.` stops at the first line terminator, so an "@" after one is not
  # reached; the terminator and all after it stay.
  expect_identical(cut("a@b\nc@d"), "b\nc@d")
  expect_identical(cut("a\n@b"), "a\n@b")
  expect_identical(cut("a@b\r\nc@d"), "b\r\nc@d")
  expect_identical(cut("a@b c@d"), "b c@d")
  expect_identical(cut("a@b\u0085c@d"), "b\u0085c@d")
  expect_identical(cut("a@b\vc@d"), "b\vc@d")
  # U+3002 and other multibyte code points are data; positions are bytes.
  expect_identical(cut("u。i@h。b"), "h。b")
  expect_identical(cut("ü@ß@é"), "é")
  # A leading U+FEFF is data, cut with the userinfo or kept with the host.
  expect_identical(cut("﻿u@h"), "h")
  expect_identical(cut("﻿@h"), "h")
  expect_identical(cut("﻿h"), "﻿h")
  expect_identical(cut("u@﻿h"), "﻿h")
  expect_identical(Encoding(cut("u@hé")), "UTF-8")
})

test_that(".classify_input_host_vec() reads the host after the last \"@\"", {
  cls <- .classify_input_host_vec(c(
    "http://u。i@ha。b/p", "http://a@b@1.2.3.4:80/", "//x@[::1]:8/"
  ))
  expect_identical(cls$host_token, c("ha。b", "1.2.3.4", "[::1]"))
  expect_identical(cls$has_userinfo, c(TRUE, TRUE, TRUE))
  expect_identical(cls$is_canonical_ip, c(FALSE, TRUE, TRUE))
})
