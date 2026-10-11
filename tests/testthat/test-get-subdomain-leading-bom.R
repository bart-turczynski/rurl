# RURL-fyiadctm: get_subdomain() reads past a U+FEFF that starts the host.
#
# Some stringi functions skip a U+FEFF (the byte-order mark) at the start of
# their input (see .starts_with_bom() and ADR 0005's U+FEFF amendment). A host
# led by the mark keeps it under rfc3986 and NULL: RFC 3986 has no rule for the
# code point, so it is reg-name data. Under whatwg, UTS-46 maps both U+FEFF and
# U+200B to nothing before the host reaches the subdomain trim. Each U+FEFF row
# has a U+200B twin: a zero-width code point stringi does not skip, so a twin
# that already gives the right answer shows the defect is the mark's alone.

bom <- "\ufeff"
zwsp <- "\u200b"

# get_subdomain() under one arm. `NULL` omits the selector, as the default call
# does; the NULL test below also passes it explicitly.
arm_sub <- function(url, arm, ...) {
  if (is.null(arm)) {
    get_subdomain(url, ...)
  } else {
    get_subdomain(url, url_standard = arm, ...)
  }
}

# The two arms whose host keeps a leading mark.
keeping_arms <- list(rfc3986 = "rfc3986", "NULL" = NULL)
all_arms <- list(whatwg = "whatwg", rfc3986 = "rfc3986", "NULL" = NULL)

# --- Pins: behavior the fix must not move -------------------------------------

test_that("the U+200B twins already keep the code point", {
  for (an in names(keeping_arms)) {
    arm <- keeping_arms[[an]]
    expect_identical(arm_sub(paste0(zwsp, "h.example.com/p"), arm),
                     paste0(zwsp, "h"), label = an)
    expect_identical(arm_sub(paste0("http://", zwsp, "h.example.com/p"), arm),
                     paste0(zwsp, "h"), label = an)
    expect_identical(
      arm_sub(paste0("http://", zwsp, "a.h.example.com/p"), arm,
              format = "labels"),
      list(c(paste0(zwsp, "a"), "h")), label = an
    )
    expect_identical(arm_sub(paste0("http://", zwsp, "www.example.com/"), arm),
                     paste0(zwsp, "www"), label = an)
  }
})

test_that("negative controls: hosts with no mark, or a mark not first", {
  for (an in names(all_arms)) {
    arm <- all_arms[[an]]
    expect_identical(arm_sub("http://h.example.com/p", arm), "h", label = an)
    expect_identical(arm_sub("http://a.h.example.com/p", arm), "a.h",
                     label = an)
    expect_identical(arm_sub("http://www.example.com/p", arm), NA_character_,
                     label = an)
    expect_identical(arm_sub("http://example.com/p", arm), NA_character_,
                     label = an)
  }
  for (an in names(keeping_arms)) {
    arm <- keeping_arms[[an]]
    expect_identical(arm_sub(paste0("http://h", bom, ".example.com/p"), arm),
                     paste0("h", bom), label = an)
    expect_identical(arm_sub(paste0("http://a.", bom, "h.example.com/p"), arm),
                     paste0("a.", bom, "h"), label = an)
  }
})

test_that("whatwg: UTS-46 removes the mark before the subdomain trim", {
  # The WHATWG host parser's domain-to-ASCII step maps U+FEFF and U+200B to
  # nothing (both are `ignored` in the UTS-46 mapping table), so the host the
  # trim sees has no mark and this arm was already right.
  for (mark in c(bom, zwsp)) {
    u <- paste0("http://", mark, "h.example.com/p")
    expect_identical(safe_parse_url(u, url_standard = "whatwg")$host,
                     "h.example.com")
    expect_identical(arm_sub(u, "whatwg"), "h")
    expect_identical(arm_sub(paste0(mark, "h.example.com/p"), "whatwg"), "h")
    expect_identical(arm_sub(paste0("http://", mark, "www.example.com/"),
                             "whatwg"),
                     NA_character_)
  }
})

# --- Defect: the subdomain trim reads past the mark ---------------------------

test_that("a mark-led subdomain keeps its mark", {
  # `.subdomain_labels_vec()` cut the prefix with
  # stri_sub(), which read past the mark, while stri_length() counted it, so
  # `\ufeffh.example.com` gave `h`. A lone `\ufeffwww` lost the mark and was
  # then dropped as `www`.
  for (an in names(keeping_arms)) {
    arm <- keeping_arms[[an]]
    expect_identical(arm_sub(paste0(bom, "h.example.com/p"), arm),
                     paste0(bom, "h"), label = an)
    expect_identical(arm_sub(paste0("http://", bom, "h.example.com/p"), arm),
                     paste0(bom, "h"), label = an)
    expect_identical(arm_sub(paste0("http://", bom, "H.Example.COM/p"), arm),
                     paste0(bom, "h"), label = an)
    expect_identical(
      arm_sub(paste0("http://", bom, "a.h.example.com/p"), arm,
              format = "labels"),
      list(c(paste0(bom, "a"), "h")), label = an
    )
    # The mark-led label is not `www`, as its U+200B twin shows, so it stays.
    expect_identical(arm_sub(paste0("http://", bom, "www.example.com/"), arm),
                     paste0(bom, "www"), label = an)
    expect_identical(
      arm_sub(paste0("http://", bom, "www.example.com/"), arm,
              include_www = TRUE),
      paste0(bom, "www"), label = an
    )
  }
})

test_that("the trim helper keeps the mark, in any LC_CTYPE", {
  # The helper itself, fed the engine's columns. The cut is in bytes, so the
  # session's LC_CTYPE cannot move it.
  host <- paste0(c(bom, zwsp, ""), "h.example.com")
  want <- list(paste0(bom, "h"), paste0(zwsp, "h"), "h")
  expect_identical(
    .subdomain_labels_vec(host, rep("example.com", 3L), rep(FALSE, 3L), FALSE),
    want
  )
  withr::with_locale(c(LC_CTYPE = "C"), {
    expect_identical(
      .subdomain_labels_vec(host, rep("example.com", 3L), rep(FALSE, 3L),
                            FALSE),
      want
    )
  })
  # A host that is the domain with a mark in front has no `.<domain>` suffix.
  expect_identical(
    .subdomain_labels_vec(paste0(bom, "example.com"), "example.com", FALSE,
                          FALSE),
    list(character(0))
  )
  # A mark alone before the suffix is a one-label subdomain, as its twin is.
  expect_identical(
    .subdomain_labels_vec(paste0(c(bom, zwsp), ".example.com"),
                          rep("example.com", 2L), rep(FALSE, 2L), FALSE),
    list(bom, zwsp)
  )
})

test_that("NULL: the subdomain keeps the mark (ADR 0016)", {
  # Witness: under the frozen selector the parsed host of
  # `\ufeffh.example.com/p` is `\ufeffh.example.com` and its domain
  # `example.com`, and get_subdomain() returned `h`, which no standard or
  # documented contract sanctions: a default-path defect, not selector-caused
  # drift. Omitting the selector and passing NULL agree.
  u <- paste0(bom, "h.example.com/p")
  expect_identical(get_subdomain(u), paste0(bom, "h"))
  expect_identical(get_subdomain(u, url_standard = NULL), paste0(bom, "h"))
  # Signature: only get_subdomain() moves, and only on a host whose first code
  # point is U+FEFF; the parse record does not move. Per arm: whatwg was
  # already right (UTS-46 removes the mark first, pinned above); rfc3986 keeps
  # the mark in its host and moves the same way as NULL.
  p <- safe_parse_url(u)
  expect_identical(p$host, paste0(bom, "h.example.com"))
  expect_identical(p$domain, "example.com")
  expect_identical(p$parse_status, "ok")
})
