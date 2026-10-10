# Tests for the bounded browser string fixer (RURL-jynceqrj, ADR 0012 Layer 6a;
# PRD "browser fixer" Part 1). The fixer is a deterministic single pass, gated
# on the internal `fixup_posture == "browser"` axis, that repairs the raw input
# string BEFORE parsing: (1) outer C0/space trim, (2) `;`->`:` for recognized
# special schemes, (3) `://` insertion for authority-table schemes, (3b)
# `file://` before a leading single `/`. The recognized-scheme set is
# .WHATWG_SPECIAL_SCHEMES; the authority table is that set minus `file`
# (RURL-anubcmpd). Step 4 (fallback `http`) is NOT the
# fixer -- it is the existing scheme_policy = "infer" prepend seam (ADR 0012
# D4). `fixup_posture` is internal-only (no public signature yet), so the axis
# is driven through rurl::: like scheme_acceptance = "general".

# The PRD Part 1 worked-examples table, keyed to the PURE fixer output (steps
# 1-3). Row `example.com` is unchanged by the fixer itself; its `http://`
# prepend is step 4 (the infer seam) and is asserted separately below.
test_that("PRD worked examples: pure fixer output (steps 1-3)", {
  expect_identical(
    rurl:::.apply_browser_fixup_vec(
      c(
        "http;//example.com", # step 2: http recognized
        "mailto;x@y.com",     # step 2 skipped: mailto not recognized
        "http:example.com",   # step 3: http in authority table
        "ftps;host/p",        # step 2 skipped: ftps non-special
        "ftps:host/p",        # step 3 skipped: ftps non-special
        "foo:bar",            # opaque; no step applies
        "example.com"         # fixer no-op (step 4 is downstream)
      ),
      "browser"
    ),
    c(
      "http://example.com",
      "mailto;x@y.com",
      "http://example.com",
      "ftps;host/p",
      "ftps:host/p",
      "foo:bar",
      "example.com"
    )
  )
})

test_that("step 4 fallback http is the existing infer seam, fed by the fixer", {
  # Under the browser knob combo, a scheme-less host-shaped input (and the
  # step-2/step-3 outputs) reach the shared scheme_policy = "infer" prepend, so
  # url_to_parse gains the single http:// prepend -- not a second fixer prepend.
  prep <- rurl:::.prepare_urls_for_parse_vec(
    c("http;//example.com", "http:example.com", "example.com"),
    protocol_handling = "keep", scheme_relative_handling = "keep",
    url_standard = "whatwg", scheme_policy = "infer",
    scheme_acceptance = "general", fixup_posture = "browser"
  )
  expect_identical(
    prep$url_to_parse,
    rep("http://example.com", 3L)
  )
})

test_that("recognized special schemes fire step 2; all but file fire step 3", {
  # Recognized-scheme set = .WHATWG_SPECIAL_SCHEMES; the authority table is
  # that set minus `file` (RURL-anubcmpd).
  for (scheme in rurl:::.WHATWG_SPECIAL_SCHEMES) {
    slashed <- paste0(scheme, if (scheme == "file") ":" else "://", "host/p")
    expect_identical(
      rurl:::.apply_browser_fixup_vec(paste0(scheme, ";host/p"), "browser"),
      slashed
    )
    expect_identical(
      rurl:::.apply_browser_fixup_vec(paste0(scheme, ":host/p"), "browser"),
      slashed
    )
  }
})

test_that("step 3 leaves an already-slashed authority untouched", {
  expect_identical(
    rurl:::.apply_browser_fixup_vec("http://example.com/p", "browser"),
    "http://example.com/p"
  )
})

test_that("scheme matching is case-insensitive and preserves case", {
  expect_identical(
    rurl:::.apply_browser_fixup_vec("HTTP;//Example.com", "browser"),
    "HTTP://Example.com"
  )
  expect_identical(
    rurl:::.apply_browser_fixup_vec("Https:Example.com", "browser"),
    "Https://Example.com"
  )
})

test_that("longer scheme is not truncated to a shorter prefix", {
  # `https;` must not be repaired as `http` + `s;`.
  expect_identical(
    rurl:::.apply_browser_fixup_vec("https;//x.com", "browser"),
    "https://x.com"
  )
  # A non-scheme token that merely starts with a recognized scheme is verbatim.
  expect_identical(
    rurl:::.apply_browser_fixup_vec("httpx;//x.com", "browser"),
    "httpx;//x.com"
  )
})

test_that("outer C0/space trim strips leading and trailing controls/spaces", {
  expect_identical(
    rurl:::.apply_browser_fixup_vec("  http://ex.com ", "browser"),
    "http://ex.com"
  )
  # C0 controls (tab/newline/CR here) at either end are trimmed too.
  expect_identical(
    rurl:::.apply_browser_fixup_vec("\t\nhttp://ex.com\r ", "browser"),
    "http://ex.com"
  )
  # Interior spaces are NOT touched by the outer trim.
  expect_identical(
    rurl:::.apply_browser_fixup_vec(" http://ex.com/a b ", "browser"),
    "http://ex.com/a b"
  )
})

test_that("NA passes through the fixer unchanged", {
  expect_identical(
    rurl:::.apply_browser_fixup_vec(c(NA_character_, "http:x.com"), "browser"),
    c(NA_character_, "http://x.com")
  )
})

test_that("default posture is a byte-identical no-op", {
  # Under fixup_posture = "none" (the default) the fixer must not touch the
  # input -- including the tricky `;`/`:`/trim cases it would rewrite under
  # "browser". This is the guard that the default parse path is unperturbed.
  tricky <- c(
    "http;//example.com", "http:example.com", "  http://ex.com ",
    "ftps;host/p", "foo:bar", "example.com", NA_character_
  )
  expect_identical(rurl:::.apply_browser_fixup_vec(tricky, "none"), tricky)
  # And .prepare_urls_for_parse_vec with the default fixup_posture matches an
  # omitted argument (defaults to "none") byte-for-byte.
  with_default <- rurl:::.prepare_urls_for_parse_vec(
    tricky, "keep", "keep", "whatwg", "infer", "general", "none"
  )
  omitted <- rurl:::.prepare_urls_for_parse_vec(
    tricky, "keep", "keep", "whatwg", "infer", "general"
  )
  expect_identical(with_default$url_to_parse, omitted$url_to_parse)
})

test_that("fixup_posture validates via match.arg and defaults to none", {
  expect_identical(rurl:::.parse_options()$fixup_posture, "none")
  expect_identical(
    rurl:::.parse_options(fixup_posture = "browser")$fixup_posture, "browser"
  )
  expect_error(rurl:::.parse_options(fixup_posture = "bogus"))
})

test_that("Stage-A cache key differs when only fixup_posture differs", {
  opts_none <- rurl:::.parse_options(
    scheme_acceptance = "general", url_standard = "whatwg",
    fixup_posture = "none"
  )
  opts_browser <- rurl:::.parse_options(
    scheme_acceptance = "general", url_standard = "whatwg",
    fixup_posture = "browser"
  )
  key_none <- rurl:::.parse_cache_keys("http:x.com", opts_none)
  key_browser <- rurl:::.parse_cache_keys("http:x.com", opts_browser)
  expect_false(identical(key_none, key_browser))
})

# RURL-otfaotzq: the WHATWG file slash state reads `\\`, `/\` and `\/` as the
# authority's two slashes, so step 3 leaves a `file:` row with them alone.
# Inserting `//` made the authority path data, and once the host-shape gate
# stopped judging whatwg `file:` rows a bad authority parsed. Values are Node
# 26's `new URL(u).href`; the `http:` rows keep their insertion.
test_that("step 3 counts backslashes as slashes after file:", {
  u <- c(
    "file:\\\\[::1]/", "FILE:\\\\[::1]/", "file:/\\h/p", "file:\\/h/p",
    "file:\\\\localhost\\C:\\x", "file:\\\\[::1x]\\", "file:/\\[g::1]x/",
    "http:\\\\h/p"
  )
  expect_identical(
    rurl:::.apply_browser_fixup_vec(u, "browser"),
    c(u[1:7], "http://\\\\h/p")
  )
  expect_identical(
    safe_parse_urls(u, profile = "browser")$clean_url,
    c(
      "file://[::1]/", "file://[::1]/", "file://h/p", "file://h/p",
      "file:///C:/x", NA, NA, "http://h/p"
    )
  )
})

# RURL-anubcmpd: step 3 skips `file`. The WHATWG file state sends a host-less
# `file:` input to the path state, so `file:p` is the path `p`, not the host
# `p`. Chromium agrees on POSIX: FixupURLInternal() in
# components/url_formatter/url_fixer.cc hands an explicit `file:` to GURL
# unchanged, and DoParseFileUrl() in url/url_parse_file.cc reads zero slashes
# as a local path. Values are Node 26's `new URL(u).href`.
test_that("step 3 leaves a host-less file: to the WHATWG path state", {
  u <- c(
    "file:p", "File:x", "  file:x  ", "file:host/p", "file:ab:cd", "file;p",
    "file:/p"
  )
  expect_identical(
    rurl:::.apply_browser_fixup_vec(u, "browser"),
    c(
      "file:p", "File:x", "file:x", "file:host/p", "file:ab:cd", "file:p",
      "file:/p"
    )
  )
  expect_identical(
    safe_parse_urls(u, profile = "browser")$clean_url,
    c(
      "file:///p", "file:///x", "file:///x", "file:///host/p",
      "file:///ab:cd", "file:///p", "file:///p"
    )
  )
})

# RURL-anubcmpd: scheme-less input that starts with one `/` is a local path,
# as Chromium's SegmentURLInternal() (components/url_formatter/url_fixer.cc)
# picks `file:` for a leading separator on POSIX. `//` stays scheme-relative,
# `/\` is left as it was, and `~` is left alone: expanding it needs the
# caller's home directory. clean_url shows the path unencoded (ADR 0017).
test_that("a leading single slash reads as a file path", {
  u <- c(
    "/Users/me/a.pdf", "/", "/a b/c \u2014 d.pdf", "//host/p", "/\\h/p",
    "~/a.pdf"
  )
  expect_identical(
    rurl:::.apply_browser_fixup_vec(u, "browser"),
    c(
      "file:///Users/me/a.pdf", "file:///", "file:///a b/c \u2014 d.pdf",
      "//host/p", "/\\h/p", "~/a.pdf"
    )
  )
  expect_identical(
    safe_parse_urls(u, profile = "browser")$clean_url,
    c(
      "file:///Users/me/a.pdf", "file:///", "file:///a b/c \u2014 d.pdf",
      "http://host/p", NA, NA
    )
  )
  # The guess is the browser profile's alone.
  expect_identical(rurl:::.apply_browser_fixup_vec(u, "none"), u)
  expect_identical(
    safe_parse_urls("/Users/me/a.pdf", profile = "whatwg")$clean_url,
    NA_character_
  )
})
