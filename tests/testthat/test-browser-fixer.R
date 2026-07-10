# Tests for the bounded browser string fixer (RURL-jynceqrj, ADR 0012 Layer 6a;
# PRD "browser fixer" Part 1). The fixer is a deterministic single pass, gated
# on the internal `fixup_posture == "browser"` axis, that repairs the raw input
# string BEFORE parsing: (1) outer C0/space trim, (2) `;`->`:` for recognized
# special schemes, (3) `://` insertion for authority-table schemes. Both tables
# resolve to .WHATWG_SPECIAL_SCHEMES. Step 4 (fallback `http`) is NOT the
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
  prep <- rurl:::.prepare_urls_for_curl_vec(
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

test_that("all recognized special schemes fire steps 2 and 3", {
  # Recognized-scheme set = authority table = .WHATWG_SPECIAL_SCHEMES.
  for (scheme in rurl:::.WHATWG_SPECIAL_SCHEMES) {
    expect_identical(
      rurl:::.apply_browser_fixup_vec(paste0(scheme, ";host/p"), "browser"),
      paste0(scheme, "://host/p")
    )
    expect_identical(
      rurl:::.apply_browser_fixup_vec(paste0(scheme, ":host/p"), "browser"),
      paste0(scheme, "://host/p")
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
  # And .prepare_urls_for_curl_vec with the default fixup_posture matches an
  # omitted argument (defaults to "none") byte-for-byte.
  with_default <- rurl:::.prepare_urls_for_curl_vec(
    tricky, "keep", "keep", "whatwg", "infer", "general", "none"
  )
  omitted <- rurl:::.prepare_urls_for_curl_vec(
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
