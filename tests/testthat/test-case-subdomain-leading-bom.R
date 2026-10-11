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
  # DEFECT (pinned before the fix): stri_trans_toupper() and
  # stri_trans_tolower() drop a U+FEFF that starts their input.
  r <- .apply_case_policy_vec(
    paste0(bom, "ab.example"),
    c("\ufeff/aB", paste0(bom, bom, "aB")), "http", "upper"
  )
  expect_identical(r$host, "AB.EXAMPLE")
  expect_identical(r$path, c("/AB", paste0(bom, "AB")))
  r <- .apply_case_policy_vec(
    NA_character_,
    c("\ufeff/aB", paste0(bom, "\u03a3"), paste0(bom, "A\u03a3")),
    NA_character_, "lower"
  )
  expect_identical(r$path, c("/ab", "\u03c3", "a\u03c2"))
})

test_that("rfc3986 (rfc-syntax + override): the case policy keeps the mark", {
  # DEFECT (pinned before the fix).
  p <- case_parse(paste0("http://", bom, "ab.example/x"), "rfc3986", "upper")
  expect_identical(p$host, "AB.EXAMPLE")
  expect_identical(p$clean_url, "HTTP://AB.EXAMPLE/X")
  expect_identical(p$parse_status, "warning-invalid-tld")
  expect_identical(
    case_parse(paste0("file:", bom, "aB"), "rfc3986", "upper")$path, "AB"
  )
  expect_identical(
    case_parse(paste0("file:", bom, "aB"), "rfc3986", "lower")$path, "ab"
  )
})

test_that("NULL: the case policy keeps the mark (ADR 0016)", {
  # Witness: under the frozen selector, `case_handling = "upper"` dropped a
  # U+FEFF that starts the host or the path, and `"lower"` one that starts the
  # path. No standard or documented contract sanctions dropping data, so this
  # is a default-path defect, not selector-caused drift. Omitting the selector
  # and passing NULL agree.
  # DEFECT (pinned before the fix).
  u_host <- paste0("http://", bom, "ab.example/x")
  u_path <- paste0("file:", bom, "aB")
  for (p in list(
    safe_parse_urls(u_host, case_handling = "upper"),
    safe_parse_urls(u_host, url_standard = NULL, case_handling = "upper")
  )) {
    expect_identical(p$host, "AB.EXAMPLE")
    expect_identical(p$clean_url, "HTTP://AB.EXAMPLE/X")
  }
  for (p in list(
    safe_parse_urls(u_path, case_handling = "upper"),
    safe_parse_urls(u_path, url_standard = NULL, case_handling = "upper")
  )) {
    expect_identical(p$path, "AB")
  }
  for (p in list(
    safe_parse_urls(u_path, case_handling = "lower"),
    safe_parse_urls(u_path, url_standard = NULL, case_handling = "lower")
  )) {
    expect_identical(p$path, "ab")
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
})

# --- Subdomain trim -----------------------------------------------------------

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
  # DEFECT (pinned before the fix): stri_startswith_fixed() read `www.` past
  # the mark, so the trim kept the whole host as if it were www-prefixed.
  expect_identical(
    .apply_subdomain_policy_vec(
      c(paste0(bom, "www.example.com"), paste0(bom, "WWW.example.com"),
        paste0(zwsp, "www.example.com")),
      rep("example.com", 3L), 0, rep(FALSE, 3L)
    ),
    c(paste0(bom, "www.example.com"), paste0(bom, "WWW.example.com"),
      "example.com")
  )
  for (an in names(arms)) {
    p <- arm_parse(paste0("http://", bom, "www.example.com/"), arms[[an]],
                   subdomain_levels_to_keep = 0)
    expect_identical(
      p$host,
      if (an == "whatwg") "www.example.com" else
        paste0(bom, "www.example.com"),
      label = an
    )
  }
})

test_that("NULL: the subdomain trim treats the mark as data (ADR 0016)", {
  # Witness: under the frozen selector `\ufeffwww.example.com` with
  # `subdomain_levels_to_keep = 0` kept its subdomain label, while its U+200B
  # twin trims to `example.com`. Omitting the selector and passing NULL agree.
  # DEFECT (pinned before the fix).
  u <- paste0("http://", bom, "www.example.com/")
  for (p in list(
    safe_parse_urls(u, subdomain_levels_to_keep = 0),
    safe_parse_urls(u, url_standard = NULL, subdomain_levels_to_keep = 0)
  )) {
    expect_identical(p$host, paste0(bom, "www.example.com"))
    expect_identical(p$clean_url, paste0("http://", bom, "www.example.com/"))
    # Signature: only host and clean_url move, for a host led by the mark and
    # then `www.`, under a non-NULL `subdomain_levels_to_keep` that drops the
    # label. Status and domain do not move. Per arm: rfc3986 and whatwg move
    # the same way (whatwg maps the mark away only at presentation, after the
    # trim), asserted above.
    expect_identical(p$parse_status, "ok")
    expect_identical(p$domain, "example.com")
  }
  expect_identical(safe_parse_urls(u, subdomain_levels_to_keep = 1)$host,
                   paste0(bom, "www.example.com"))
  expect_identical(safe_parse_urls(u)$host, paste0(bom, "www.example.com"))
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
  # DEFECT (pinned before the fix): stri_startswith_fixed() read `www.` past
  # the mark, so the apex `\ufeffwww.com` counted as already www-prefixed.
  expect_identical(
    .apply_www_if_no_subdomain_vec(
      c(paste0(bom, "www.com"), paste0(bom, "WWW.com"), paste0(zwsp, "www.com"))
    ),
    c(paste0(bom, "www.com"), paste0(bom, "WWW.com"),
      paste0("www.", zwsp, "www.com"))
  )
  expect_identical(
    arm_parse(paste0("http://", bom, "www.com/"), "rfc3986",
              www_handling = "if_no_subdomain")$host,
    paste0(bom, "www.com")
  )
  expect_identical(
    arm_parse(paste0("http://", bom, "www.com/"), "whatwg",
              www_handling = "if_no_subdomain")$host,
    "www.com"
  )
})

test_that("NULL: if_no_subdomain treats the mark as data (ADR 0016)", {
  # Witness: under the frozen selector the apex `\ufeffwww.com` got no `www.`,
  # while its U+200B twin does. Omitting the selector and passing NULL agree.
  # DEFECT (pinned before the fix).
  u <- paste0("http://", bom, "www.com/")
  for (p in list(
    safe_parse_urls(u, www_handling = "if_no_subdomain"),
    safe_parse_urls(u, url_standard = NULL, www_handling = "if_no_subdomain")
  )) {
    expect_identical(p$host, paste0(bom, "www.com"))
    expect_identical(p$clean_url, paste0("http://", bom, "www.com/"))
    # Signature: only host and clean_url move, for an apex host led by the
    # mark and then `www.`, under `www_handling = "if_no_subdomain"`. Status
    # does not move. Per arm: rfc3986 and whatwg move the same way (whatwg
    # maps the mark away only at presentation), asserted above.
    expect_identical(p$parse_status, "ok")
  }
  expect_identical(safe_parse_urls(u)$host, paste0(bom, "www.com"))
  expect_identical(safe_parse_urls(u, www_handling = "strip")$host,
                   paste0(bom, "www.com"))
})

# --- Host helpers with no output difference in the pipeline ------------------

test_that("the strict IP detector does not read an address past the mark", {
  # DEFECT (pinned before the fix): the dotted-quad tail match read past the
  # mark. In the pipeline no such host parses, so no output moved.
  expect_identical(
    .detect_ip_host_vec(c(
      paste0(bom, "::1.2.3.4"), paste0(bom, "[::1.2.3.4]"),
      paste0(zwsp, "::1.2.3.4"), paste0(zwsp, "[::1.2.3.4]"),
      "::1.2.3.4", "[::1.2.3.4]", paste0(bom, "1.2.3.4"), "1.2.3.4"
    )),
    c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("the host-shape gate does not read a bracket past the mark", {
  # DEFECT (pinned before the fix): stri_startswith_fixed() read `[` past the
  # mark, so the token kept its brackets and read as a canonical IPv6 literal.
  cl <- .classify_input_host_vec(c(
    paste0("http://", bom, "[::1.2.3.4]/"),
    paste0("http://", zwsp, "[::1.2.3.4]/"),
    "http://[::1.2.3.4]/"
  ))
  expect_identical(
    cl$host_token,
    c(paste0(bom, "[::1.2.3.4]"), paste0(zwsp, "[:"), "[::1.2.3.4]")
  )
  expect_identical(cl$is_canonical_ip, c(TRUE, FALSE, TRUE))
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
  # DEFECT (pinned before the fix): a mark-led host was read as bracketed, so
  # it skipped domain to ASCII. The row fails either way.
  hosts <- c(paste0(bom, "[a.1"), paste0(zwsp, "[a.1"), "[a.1")
  m <- .apply_host_standard_model_vec(hosts, hosts, rep(FALSE, 3L), "whatwg",
                                      rep(FALSE, 3L))
  expect_identical(m$host, c(paste0(bom, "[a.1"), "[a.1", "[a.1"))
  expect_identical(m$fatal, c(TRUE, TRUE, TRUE))
  expect_identical(m$is_ip, c(FALSE, FALSE, FALSE))
})

test_that("the WHATWG userinfo rewrite leaves a mark-led row alone", {
  # DEFECT (pinned before the fix): the `^scheme:` match read past the mark
  # and the rebuilt row lost it. Phase 1 prefixes `http://` to every row led
  # by the mark, so the pipeline never hands this helper one.
  u <- c(paste0(bom, "http://a b@h/"), paste0(zwsp, "http://a b@h/"),
         "http://a b@h/", paste0("http://", bom, "a b@h/"))
  expect_identical(
    .encode_userinfo_charset_vec(u, "whatwg")$url,
    c("http://a%20b@h/", paste0(zwsp, "http://a b@h/"), "http://a%20b@h/",
      paste0("http://", bom, "a%20b@h/"))
  )
  expect_identical(
    safe_parse_urls(paste0(bom, "http://a b@h/"),
                    url_standard = "whatwg")$parse_status,
    "error"
  )
})
