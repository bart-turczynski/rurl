# RURL-opwjlnla: the case, subdomain and www policies, and the host helpers
# beside them, read past a U+FEFF that starts their input.
#
# Some stringi functions skip a U+FEFF (the byte-order mark) at the start of
# their input (see .starts_with_bom()). A host or path led by the mark holds it
# as ordinary data: the WHATWG URL Standard does not strip it (it is not a C0
# control or space) and RFC 3986 has no rule for it. Each U+FEFF row has a
# U+200B twin: a zero-width code point stringi does not skip, so a twin that
# already gives the right answer shows the defect is the mark's alone.

bom <- "\ufeff"
zwsp <- "\u200b"

arms <- list(whatwg = "whatwg", rfc3986 = "rfc3986", "NULL" = NULL)

# One parse under one arm. `NULL` omits the selector, as the default call does;
# the NULL tests below also pass it explicitly.
arm_parse <- function(url, arm, ...) {
  if (is.null(arm)) {
    safe_parse_urls(url, ...)
  } else {
    safe_parse_urls(url, url_standard = arm, ...)
  }
}

# The columns a fix may not move: all but host, clean_url and `also`.
unmoved_cols <- function(p, also = character(0)) {
  p[setdiff(names(p), c("host", "clean_url", also))]
}

# `case_handling` is governed by both selectors, so a named arm reaches
# `"upper"` or `"lower"` only through a profile with an explicit override
# (ADR 0012 D6): `profile = "rfc-syntax"` is the rfc3986 arm.
case_parse <- function(url, arm, case_handling) {
  switch(arm,
    whatwg = safe_parse_urls(url, profile = "whatwg",
                             case_handling = case_handling),
    rfc3986 = safe_parse_urls(url, profile = "rfc-syntax",
                              case_handling = case_handling),
    "NULL" = safe_parse_urls(url, case_handling = case_handling)
  )
}

# --- Case policy: pins --------------------------------------------------------

test_that("case policy: the U+200B twins already keep the code point", {
  r <- .apply_case_policy_vec(
    paste0(zwsp, "ab.example"),
    c("\u200b/aB", paste0(zwsp, "\u03a3"), paste0(zwsp, "A\u03a3")),
    "http", "upper"
  )
  expect_identical(r$host, paste0(zwsp, "AB.EXAMPLE"))
  expect_identical(r$path[[1L]], "\u200b/AB")
  # Lowering keeps Unicode case mapping, context included: a capital sigma
  # after a cased letter is final, one with nothing cased before it is not.
  r <- .apply_case_policy_vec(
    NA_character_,
    c("\u200b/aB", paste0(zwsp, "\u03a3"), paste0(zwsp, "A\u03a3")),
    NA_character_, "lower"
  )
  expect_identical(
    r$path,
    c("\u200b/ab", paste0(zwsp, "\u03c3"), paste0(zwsp, "a\u03c2"))
  )
  for (an in c("rfc3986", "NULL")) {
    p <- case_parse(paste0("http://", zwsp, "ab.example/x"), an, "upper")
    expect_identical(p$host, paste0(zwsp, "AB.EXAMPLE"), label = an)
    expect_identical(p$clean_url, paste0("HTTP://", zwsp, "AB.EXAMPLE/X"),
                     label = an)
    expect_identical(
      case_parse(paste0("file:", zwsp, "aB"), an, "upper")$path,
      paste0(zwsp, "AB"), label = an
    )
    expect_identical(
      case_parse(paste0("file:", zwsp, "aB"), an, "lower")$path,
      paste0(zwsp, "ab"), label = an
    )
  }
})

test_that("case policy negative controls: no mark, or a mark not first", {
  r <- .apply_case_policy_vec(
    c("Ab.example", paste0("a", bom, "b.example")),
    c("/aB", paste0("/", bom, "aB")), c("http", "http"), "upper"
  )
  expect_identical(r$host, c("AB.EXAMPLE", paste0("A", bom, "B.EXAMPLE")))
  expect_identical(r$path, c("/AB", paste0("/", bom, "AB")))
  r <- .apply_case_policy_vec(
    NA_character_, c("\u03a3", "A\u03a3", paste0("a", bom, "B")),
    NA_character_, "lower"
  )
  expect_identical(r$path, c("\u03c3", "a\u03c2", paste0("a", bom, "b")))
  # `lower` / `lower_host` lower the host with the ASCII helper, which keeps
  # the mark; `keep` and `lower_host` leave the path alone.
  for (ch in c("lower", "lower_host")) {
    expect_identical(
      .apply_case_policy_vec(paste0(bom, "Ab.example"), NA_character_,
                             NA_character_, ch)$host,
      paste0(bom, "ab.example"), label = ch
    )
  }
  for (ch in c("keep", "lower_host")) {
    expect_identical(
      .apply_case_policy_vec(NA_character_, "\ufeff/aB",
                             NA_character_, ch)$path,
      "\ufeff/aB", label = ch
    )
  }
  for (an in c("rfc3986", "NULL")) {
    p <- case_parse(paste0("http://h.example/", bom, "aB"), an, "upper")
    expect_identical(p$path, paste0("/", bom, "AB"), label = an)
    expect_identical(case_parse("http://Ab.example/aB", an, "upper")$host,
                     "AB.EXAMPLE", label = an)
  }
})

test_that("whatwg: no cased string starts with the mark", {
  # The host presentation runs UTS-46 domain to ASCII before the case policy,
  # and UTS-46 maps U+FEFF (and U+200B) to nothing; the file state roots the
  # path first. So this arm was already right, and the fix does not move it.
  for (mark in c(bom, zwsp)) {
    p <- case_parse(paste0("http://", mark, "ab.example/x"), "whatwg", "upper")
    expect_identical(p$host, "AB.EXAMPLE")
    expect_identical(p$clean_url, "HTTP://AB.EXAMPLE/X")
  }
  expect_identical(
    case_parse(paste0("file:", bom, "aB"), "whatwg", "upper")$path,
    paste0("/", bom, "AB")
  )
  expect_identical(
    case_parse(paste0("file:", bom, "aB"), "whatwg", "lower")$path,
    paste0("/", bom, "ab")
  )
})

# --- Case policy: the defect --------------------------------------------------

test_that("case policy keeps a mark that starts the host or the path", {
  # stri_trans_toupper() and stri_trans_tolower() drop a U+FEFF that starts
  # their input: the host became `AB.EXAMPLE` and the paths `/AB`, `/ab`,
  # and the sigma rows lost the mark. Like the U+200B twins above, the mark
  # now stays, a second mark included, and the sigma context is unchanged.
  r <- .apply_case_policy_vec(
    paste0(bom, "ab.example"),
    c("\ufeff/aB", paste0(bom, bom, "aB")), "http", "upper"
  )
  expect_identical(r$host, paste0(bom, "AB.EXAMPLE"))
  expect_identical(r$path, c("\ufeff/AB", paste0(bom, bom, "AB")))
  r <- .apply_case_policy_vec(
    NA_character_,
    c("\ufeff/aB", paste0(bom, "\u03a3"), paste0(bom, "A\u03a3")),
    NA_character_, "lower"
  )
  expect_identical(
    r$path,
    c("\ufeff/ab", paste0(bom, "\u03c3"), paste0(bom, "a\u03c2"))
  )
})

test_that("rfc3986 (rfc-syntax + override): the case policy keeps the mark", {
  # The profile override reaches the same helper, so this arm lost the mark
  # too (host `AB.EXAMPLE`, paths `AB` and `ab`). It keeps it now.
  p <- case_parse(paste0("http://", bom, "ab.example/x"), "rfc3986", "upper")
  expect_identical(p$host, paste0(bom, "AB.EXAMPLE"))
  expect_identical(p$clean_url, paste0("HTTP://", bom, "AB.EXAMPLE/X"))
  expect_identical(p$parse_status, "warning-invalid-tld")
  expect_identical(
    case_parse(paste0("file:", bom, "aB"), "rfc3986", "upper")$path,
    paste0(bom, "AB")
  )
  expect_identical(
    case_parse(paste0("file:", bom, "aB"), "rfc3986", "lower")$path,
    paste0(bom, "ab")
  )
})

test_that("NULL: the case policy keeps the mark (ADR 0016)", {
  # Witness: under the frozen selector, `case_handling = "upper"` dropped a
  # U+FEFF that starts the host or the path (host `AB.EXAMPLE`, clean_url
  # `HTTP://AB.EXAMPLE/X`, path `AB`), and `"lower"` one that starts the path
  # (`ab`). No standard or documented contract sanctions dropping data, so
  # this is a default-path defect, not selector-caused drift. Omitting the
  # selector and passing NULL agree.
  u_host <- paste0("http://", bom, "ab.example/x")
  u_path <- paste0("file:", bom, "aB")
  for (p in list(
    safe_parse_urls(u_host, case_handling = "upper"),
    safe_parse_urls(u_host, url_standard = NULL, case_handling = "upper")
  )) {
    expect_identical(p$host, paste0(bom, "AB.EXAMPLE"))
    expect_identical(p$clean_url, paste0("HTTP://", bom, "AB.EXAMPLE/X"))
  }
  for (p in list(
    safe_parse_urls(u_path, case_handling = "upper"),
    safe_parse_urls(u_path, url_standard = NULL, case_handling = "upper")
  )) {
    expect_identical(p$path, paste0(bom, "AB"))
  }
  for (p in list(
    safe_parse_urls(u_path, case_handling = "lower"),
    safe_parse_urls(u_path, url_standard = NULL, case_handling = "lower")
  )) {
    expect_identical(p$path, paste0(bom, "ab"))
  }
  # Signature: only the host, path and clean_url of a host or path that starts
  # with the mark move, and only under `case_handling = "upper"` (host, path)
  # or `"lower"` (path). Status, domain and scheme do not move; `keep` and
  # `lower_host` do not move. Per arm: rfc3986 moves the same way (asserted
  # above); whatwg was already right (asserted above).
  p <- safe_parse_urls(u_host, case_handling = "upper")
  expect_identical(p$parse_status, "warning-invalid-tld")
  expect_identical(p$scheme, "HTTP")
  expect_identical(p$domain, NA_character_)
  p <- safe_parse_urls(u_path, case_handling = "upper")
  expect_identical(p$parse_status, "error")
  expect_identical(p$clean_url, NA_character_)
  expect_identical(safe_parse_urls(u_host)$host, paste0(bom, "ab.example"))
  expect_identical(safe_parse_urls(u_path)$path, paste0(bom, "aB"))
  # The case policy writes only host, path, scheme and clean_url, so every
  # other column of a moved row equals the `keep` parse, which the fix does
  # not reach.
  for (ch in c("upper", "lower")) {
    for (u in c(u_host, u_path, paste0("http://", bom, "www.a.example.com/"))) {
      p <- safe_parse_urls(u, case_handling = ch)
      expect_identical(unmoved_cols(p, c("path", "scheme")),
                       unmoved_cols(safe_parse_urls(u, case_handling = "keep"),
                                    c("path", "scheme")),
                       label = paste(ch, u))
    }
  }
})

# --- Subdomain trim -----------------------------------------------------------

# Under whatwg the subdomain and www policies read the host before UTS-46 maps
# the mark away; this fix leaves that arm as it was (RURL-fobwctml owns the
# pre-mapping read), so these are pinned for the arm and for the profile.
whatwg_parse <- function(url, arm, ...) {
  switch(arm,
    std = safe_parse_urls(url, url_standard = "whatwg", ...),
    profile = safe_parse_urls(url, profile = "whatwg", ...)
  )
}

test_that("subdomain trim: the U+200B twins and the negative controls", {
  # The twin's mark-led first label is a subdomain label, not the `www.`
  # prefix the trim keeps. Under whatwg the host presentation maps the code
  # point away after the trim.
  for (an in names(arms)) {
    arm <- arms[[an]]
    sub_host <- function(u, k) {
      arm_parse(u, arm, subdomain_levels_to_keep = k)$host
    }
    expect_identical(sub_host(paste0("http://", zwsp, "www.example.com/"), 0),
                     "example.com", label = an)
    expect_identical(
      sub_host(paste0("http://", zwsp, "www.example.com/"), 1),
      if (an == "whatwg") "www.example.com" else
        paste0(zwsp, "www.example.com"),
      label = an
    )
    expect_identical(sub_host("http://www.a.example.com/", 0),
                     "www.example.com", label = an)
    expect_identical(sub_host("http://www.example.com/", 0),
                     "www.example.com", label = an)
    expect_identical(sub_host("http://a.b.example.com/", 0), "example.com",
                     label = an)
    expect_identical(sub_host(paste0("http://", bom, "a.b.example.com/"), 0),
                     "example.com", label = an)
    expect_identical(sub_host(paste0("http://", bom, "a.b.example.com/"), 1),
                     "b.example.com", label = an)
  }
})

test_that("subdomain trim reads a mark-led www label as a subdomain label", {
  # stri_startswith_fixed() read `www.` past the mark, so the trim took the
  # host for www-prefixed: stri_sub() then cut the mark off with `www.`. The
  # mark-led label is a subdomain label, trimmed like its U+200B twin.
  expect_identical(
    .apply_subdomain_policy_vec(
      c(paste0(bom, "www.example.com"), paste0(bom, "WWW.example.com"),
        paste0(zwsp, "www.example.com"), paste0(bom, "www.a.example.com")),
      rep("example.com", 4L), 0, rep(FALSE, 4L)
    ),
    rep("example.com", 4L)
  )
  expect_identical(
    .apply_subdomain_policy_vec(paste0(bom, "www.a.example.com"),
                                "example.com", 1, FALSE),
    "a.example.com"
  )
  for (an in c("rfc3986", "NULL")) {
    sub_host <- function(u, k) {
      arm_parse(u, arms[[an]], subdomain_levels_to_keep = k)$host
    }
    # A host led by the mark, `www.` and a registrable domain: the trim kept
    # it whole; keep = 0 now drops the mark-led label.
    expect_identical(sub_host(paste0("http://", bom, "www.example.com/"), 0),
                     "example.com", label = an)
    expect_identical(sub_host(paste0("http://", bom, "www.example.com/"), 1),
                     paste0(bom, "www.example.com"), label = an)
    # With more labels, the trim dropped the mark, kept `www.` and trimmed
    # the rest (`www.example.com`, `www.a.example.com`). The mark-led label
    # now counts and is trimmed first; a keep that trims nothing leaves the
    # host as written (it was `www.a.example.com`).
    u <- paste0("http://", bom, "www.a.example.com/")
    expect_identical(sub_host(u, 0), "example.com", label = an)
    expect_identical(sub_host(u, 1), "a.example.com", label = an)
    expect_identical(sub_host(u, 2), paste0(bom, "www.a.example.com"),
                     label = an)
    expect_identical(sub_host(u, 5), paste0(bom, "www.a.example.com"),
                     label = an)
  }
})

test_that("rfc-syntax: a mark-led www host left whole keeps its case", {
  # The trim rebuilds a host it acts on and lowercases its registrable labels.
  # The stringi reading never rebuilt `<U+FEFF>WWW.Example.com`, as it found
  # no subdomain past `www.`; a keep that trims none of its labels still
  # leaves it as written. Only a row the policy trims is rebuilt.
  u <- paste0("http://", bom, "WWW.Example.com/A")
  rs <- function(u, k) {
    safe_parse_urls(u, profile = "rfc-syntax", subdomain_levels_to_keep = k)
  }
  for (k in c(1, 2, 5)) {
    p <- rs(u, k)
    expect_identical(p$host, paste0(bom, "WWW.Example.com"), label = k)
    expect_identical(p$clean_url, paste0("http://", bom, "WWW.Example.com/A"),
                     label = k)
  }
  expect_identical(rs(u, 0)$host, "example.com")
  u <- paste0("http://", bom, "www.a.B.Example.com/")
  expect_identical(rs(u, 5)$host, paste0(bom, "www.a.B.Example.com"))
  expect_identical(rs(u, 3)$host, paste0(bom, "www.a.B.Example.com"))
  expect_identical(rs(u, 2)$host, "a.B.example.com")
  # Hosts the stringi reading did not take for www-prefixed are rebuilt as
  # before: no mark, two marks, a mark not followed by `www.`, the twin.
  expect_identical(
    rs(c("http://a.B.Example.com/", paste0("http://", bom, "a.B.Example.com/"),
         paste0("http://", bom, bom, "www.a.Example.com/"),
         paste0("http://", zwsp, "www.a.B.Example.com/")), 5)$host,
    c("a.B.example.com", paste0(bom, "a.B.example.com"),
      paste0(bom, bom, "www.a.example.com"),
      paste0(zwsp, "www.a.B.example.com"))
  )
})

test_that("whatwg: the subdomain trim does not move", {
  # The values the trim gave before this fix: it read `www.` past the mark.
  for (arm in c("std", "profile")) {
    sub_host <- function(u, k) {
      whatwg_parse(u, arm, subdomain_levels_to_keep = k)$host
    }
    for (k in c(0, 1)) {
      expect_identical(
        sub_host(paste0("http://", bom, "www.example.com/"), k),
        "www.example.com", label = paste(arm, k)
      )
      expect_identical(
        sub_host(paste0("http://", bom, "WWW.Example.com/A"), k),
        "www.example.com", label = paste(arm, k)
      )
    }
    u <- paste0("http://", bom, "www.a.example.com/")
    expect_identical(sub_host(u, 0), "www.example.com", label = arm)
    expect_identical(sub_host(u, 1), "www.a.example.com", label = arm)
    expect_identical(sub_host(u, 2), "www.a.example.com", label = arm)
    p <- whatwg_parse(u, arm, subdomain_levels_to_keep = 0)
    expect_identical(p$clean_url, "http://www.example.com/", label = arm)
    expect_identical(p$parse_status, "ok", label = arm)
  }
  # The helper itself, as the pipeline calls it under whatwg.
  expect_identical(
    .apply_subdomain_policy_vec(
      c(paste0(bom, "www.example.com"), paste0(bom, "www.a.example.com")),
      rep("example.com", 2L), 0, rep(FALSE, 2L), url_standard = "whatwg"
    ),
    c(paste0(bom, "www.example.com"), "www.example.com")
  )
})

test_that("NULL: the subdomain trim treats the mark as data (ADR 0016)", {
  # Witness: under the frozen selector `\ufeffwww.example.com` with
  # `subdomain_levels_to_keep = 0` kept its subdomain label, while its U+200B
  # twin trims to `example.com`. Omitting the selector and passing NULL agree.
  u <- paste0("http://", bom, "www.example.com/")
  u2 <- paste0("http://", bom, "www.a.example.com/")
  for (p in list(
    safe_parse_urls(c(u, u2), subdomain_levels_to_keep = 0),
    safe_parse_urls(c(u, u2), url_standard = NULL,
                    subdomain_levels_to_keep = 0)
  )) {
    expect_identical(p$host, c("example.com", "example.com"))
    expect_identical(p$clean_url,
                     c("http://example.com/", "http://example.com/"))
  }
  # Signature: only host and clean_url move, for a host led by one mark and
  # then `www.`, under a non-NULL `subdomain_levels_to_keep`. Every other
  # column equals the parse without the trim, which the fix does not reach.
  # Per arm: rfc3986 moves the same way (asserted above); whatwg does not
  # move (asserted above).
  for (k in c(0, 1, 2, 5)) {
    for (uu in c(u, u2)) {
      expect_identical(
        unmoved_cols(safe_parse_urls(uu, subdomain_levels_to_keep = k)),
        unmoved_cols(safe_parse_urls(uu)), label = paste(k, uu)
      )
    }
  }
  expect_identical(safe_parse_urls(u, subdomain_levels_to_keep = 1)$host,
                   paste0(bom, "www.example.com"))
  expect_identical(safe_parse_urls(u)$host, paste0(bom, "www.example.com"))
  # Other option combinations: without a trim the www policies leave these
  # hosts as they were, and `www_handling = "keep"` puts a `www.` in front of
  # the mark, so the trim never meets it first.
  uu <- c(u, u2, paste0("http://", bom, "www.com/"))
  for (w in c("none", "strip")) {
    expect_identical(
      safe_parse_urls(uu, www_handling = w)$host,
      paste0(bom, c("www.example.com", "www.a.example.com", "www.com")),
      label = w
    )
  }
  expect_identical(
    safe_parse_urls(uu, www_handling = "keep",
                    subdomain_levels_to_keep = 0)$host,
    c("www.example.com", "www.example.com", paste0("www.", bom, "www.com"))
  )
  expect_identical(
    safe_parse_urls(uu, host_encoding = "idna",
                    subdomain_levels_to_keep = 1)$host,
    c("xn--www-3z6s.example.com", "a.example.com", "xn--www-3z6s.com")
  )
})

test_that("rows the subdomain fix does not reach keep their values", {
  # Byte-identical to the trim before this fix: no mark, a mark not first,
  # two marks, a mark not followed by `www.`, and the U+200B twin.
  u <- c("http://www.example.com/", "http://WWW.Example.com/A",
         "http://www.a.B.Example.com/", "http://a.B.Example.com/",
         "http://www.com/", "http://example.com/",
         "http://www.m\u00fcnchen.de/", "http://a.m\u00fcnchen.de/",
         paste0("http://", zwsp, "www.a.B.Example.com/"),
         paste0("http://a", bom, "b.www.Example.com/"),
         paste0("http://", bom, bom, "www.a.Example.com/"),
         paste0("http://", bom, "a.B.Example.com/"),
         paste0("http://", bom, "Example.com/"))
  rs <- function(k) {
    safe_parse_urls(u, profile = "rfc-syntax",
                    subdomain_levels_to_keep = k)$host
  }
  expect_identical(rs(0), c(
    "www.example.com", "WWW.Example.com", "www.example.com", "example.com",
    "www.com", "example.com", "www.m\u00fcnchen.de", "m\u00fcnchen.de",
    "example.com", "example.com", "example.com", "example.com",
    "\ufeffExample.com"
  ))
  expect_identical(rs(1), c(
    "www.example.com", "WWW.Example.com", "www.B.example.com", "B.example.com",
    "www.com", "example.com", "www.m\u00fcnchen.de", "a.m\u00fcnchen.de",
    "B.example.com", "www.example.com", "a.example.com", "B.example.com",
    "\ufeffExample.com"
  ))
  expect_identical(rs(2), c(
    "www.example.com", "WWW.Example.com", "www.a.B.example.com",
    "a.B.example.com", "www.com", "example.com", "www.m\u00fcnchen.de",
    "a.m\u00fcnchen.de", "a.B.example.com", "a\ufeffb.www.example.com",
    "\ufeff\ufeffwww.a.example.com", "\ufeffa.B.example.com",
    "\ufeffExample.com"
  ))
  expect_identical(
    safe_parse_urls(u, url_standard = "whatwg",
                    subdomain_levels_to_keep = 2)$host,
    c("www.example.com", "www.example.com", "www.a.b.example.com",
      "a.b.example.com", "www.com", "example.com", "www.m\u00fcnchen.de",
      "a.m\u00fcnchen.de", "a.b.example.com", "ab.www.example.com",
      "www.a.example.com", "a.b.example.com", "example.com")
  )
  expect_identical(
    safe_parse_urls(u, profile = "rfc-syntax",
                    www_handling = "if_no_subdomain")$host,
    c("www.example.com", "www.Example.com", "www.a.B.Example.com",
      "a.B.Example.com", "www.com", "www.example.com", "www.m\u00fcnchen.de",
      "a.m\u00fcnchen.de", "\u200bwww.a.B.Example.com",
      "a\ufeffb.www.Example.com", "\ufeff\ufeffwww.a.Example.com",
      "\ufeffa.B.Example.com", "www.\ufeffExample.com")
  )
})

# --- www_handling = "if_no_subdomain" -----------------------------------------

test_that("if_no_subdomain: the U+200B twins and the negative controls", {
  # `\u200bwww.com` is an apex (registrable domain, no subdomain), so it gets
  # `www.`; a host that starts with `www.` does not.
  for (an in names(arms)) {
    arm <- arms[[an]]
    www_host <- function(u) {
      arm_parse(u, arm, www_handling = "if_no_subdomain")$host
    }
    expect_identical(
      www_host(paste0("http://", zwsp, "www.com/")),
      if (an == "whatwg") "www.www.com" else paste0("www.", zwsp, "www.com"),
      label = an
    )
    expect_identical(www_host("http://www.com/"), "www.com", label = an)
    expect_identical(www_host("http://example.com/"), "www.example.com",
                     label = an)
    expect_identical(
      www_host(paste0("http://", bom, "example.com/")),
      if (an == "whatwg") "www.example.com" else
        paste0("www.", bom, "example.com"),
      label = an
    )
    expect_identical(
      www_host(paste0("http://", bom, "www.example.com/")),
      if (an == "whatwg") "www.example.com" else
        paste0(bom, "www.example.com"),
      label = an
    )
  }
})

test_that("if_no_subdomain reads a mark-led www label as part of the apex", {
  # stri_startswith_fixed() read `www.` past the mark, so the apex
  # `\ufeffwww.com` counted as already www-prefixed and got no `www.`.
  expect_identical(
    .apply_www_if_no_subdomain_vec(
      c(paste0(bom, "www.com"), paste0(bom, "WWW.com"), paste0(zwsp, "www.com"))
    ),
    c(paste0("www.", bom, "www.com"), paste0("www.", bom, "WWW.com"),
      paste0("www.", zwsp, "www.com"))
  )
  expect_identical(
    arm_parse(paste0("http://", bom, "www.com/"), "rfc3986",
              www_handling = "if_no_subdomain")$host,
    paste0("www.", bom, "www.com")
  )
})

test_that("whatwg: if_no_subdomain does not move", {
  # The values the policy gave before this fix: it read `www.` past the mark,
  # so the apex counted as www-prefixed. WHATWG's own host is `www.com`.
  for (arm in c("std", "profile")) {
    www_parse <- function(u) {
      whatwg_parse(u, arm, www_handling = "if_no_subdomain")
    }
    p <- www_parse(paste0("http://", bom, "www.com/"))
    expect_identical(p$host, "www.com", label = arm)
    expect_identical(p$clean_url, "http://www.com/", label = arm)
    expect_identical(p$parse_status, "ok", label = arm)
    expect_identical(www_parse(paste0("http://", bom, "WWW.com/"))$host,
                     "www.com", label = arm)
    expect_identical(www_parse(paste0("http://", bom, "www.co.uk/"))$host,
                     "www.co.uk", label = arm)
  }
  expect_identical(
    .apply_www_if_no_subdomain_vec(paste0(bom, "www.com"),
                                   url_standard = "whatwg"),
    paste0(bom, "www.com")
  )
  expect_identical(
    .apply_www_policy_vec(paste0(bom, "www.com"), "if_no_subdomain", FALSE,
                          url_standard = "whatwg"),
    paste0(bom, "www.com")
  )
})

test_that("NULL: if_no_subdomain treats the mark as data (ADR 0016)", {
  # Witness: under the frozen selector the apex `\ufeffwww.com` got no `www.`,
  # while its U+200B twin does. Omitting the selector and passing NULL agree.
  u <- paste0("http://", bom, "www.com/")
  for (p in list(
    safe_parse_urls(u, www_handling = "if_no_subdomain"),
    safe_parse_urls(u, url_standard = NULL, www_handling = "if_no_subdomain")
  )) {
    expect_identical(p$host, paste0("www.", bom, "www.com"))
    expect_identical(p$clean_url, paste0("http://www.", bom, "www.com/"))
    # Signature: only host and clean_url move, for an apex host led by one
    # mark and then `www.`, under `www_handling = "if_no_subdomain"`. Every
    # other column equals the parse without the policy. Per arm: rfc3986
    # moves the same way (asserted above); whatwg does not move (asserted
    # above).
    expect_identical(unmoved_cols(p), unmoved_cols(safe_parse_urls(u)))
  }
  expect_identical(safe_parse_urls(u)$host, paste0(bom, "www.com"))
  expect_identical(safe_parse_urls(u, www_handling = "strip")$host,
                   paste0(bom, "www.com"))
  # A host with a subdomain past the mark-led label is no apex either way.
  expect_identical(
    safe_parse_urls(paste0("http://", bom, "www.example.com/"),
                    www_handling = "if_no_subdomain")$host,
    paste0(bom, "www.example.com")
  )
})

# --- Host helpers with no output difference in the pipeline ------------------

test_that("the strict IP detector does not read an address past the mark", {
  # The dotted-quad tail match read past the mark and called the first two
  # hosts addresses. In the pipeline no such host parses, so no output moved.
  expect_identical(
    .detect_ip_host_vec(c(
      paste0(bom, "::1.2.3.4"), paste0(bom, "[::1.2.3.4]"),
      paste0(zwsp, "::1.2.3.4"), paste0(zwsp, "[::1.2.3.4]"),
      "::1.2.3.4", "[::1.2.3.4]", paste0(bom, "1.2.3.4"), "1.2.3.4"
    )),
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("the host-shape gate does not read a bracket past the mark", {
  # stri_startswith_fixed() read `[` past the mark, so the token kept its
  # brackets and read as a canonical IPv6 literal. It now cuts like the twin.
  cl <- .classify_input_host_vec(c(
    paste0("http://", bom, "[::1.2.3.4]/"),
    paste0("http://", zwsp, "[::1.2.3.4]/"),
    "http://[::1.2.3.4]/"
  ))
  expect_identical(
    cl$host_token,
    c(paste0(bom, "[:"), paste0(zwsp, "[:"), "[::1.2.3.4]")
  )
  expect_identical(cl$is_canonical_ip, c(FALSE, FALSE, TRUE))
  # Signature: every arm fails these rows before and after.
  for (an in names(arms)) {
    expect_identical(
      arm_parse(c(paste0("http://", bom, "[::1.2.3.4]/"),
                  paste0("http://", zwsp, "[::1.2.3.4]/")),
                arms[[an]])$parse_status,
      c("error", "error"), label = an
    )
  }
})

test_that("the WHATWG host model does not read a bracket past the mark", {
  # A mark-led host was read as bracketed, so it skipped domain to ASCII and
  # kept the mark. It now maps like its twin; the row fails either way.
  hosts <- c(paste0(bom, "[a.1"), paste0(zwsp, "[a.1"), "[a.1")
  m <- .apply_host_standard_model_vec(hosts, hosts, rep(FALSE, 3L), "whatwg",
                                      rep(FALSE, 3L))
  expect_identical(m$host, c("[a.1", "[a.1", "[a.1"))
  expect_identical(m$fatal, c(TRUE, TRUE, TRUE))
  expect_identical(m$is_ip, c(FALSE, FALSE, FALSE))
})

test_that("the WHATWG userinfo rewrite leaves a mark-led row alone", {
  # The `^scheme:` match read past the mark and the rebuilt row lost it
  # (`http://a%20b@h/`); the row is now left alone, like its twin. Phase 1
  # prefixes `http://` to every row led by the mark, so the pipeline never
  # hands this helper one.
  u <- c(paste0(bom, "http://a b@h/"), paste0(zwsp, "http://a b@h/"),
         "http://a b@h/", paste0("http://", bom, "a b@h/"))
  expect_identical(
    .encode_userinfo_charset_vec(u, "whatwg")$url,
    c(paste0(bom, "http://a b@h/"), paste0(zwsp, "http://a b@h/"),
      "http://a%20b@h/",
      paste0("http://", bom, "a%20b@h/"))
  )
  expect_identical(
    safe_parse_urls(paste0(bom, "http://a b@h/"),
                    url_standard = "whatwg")$parse_status,
    "error"
  )
})
