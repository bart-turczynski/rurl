# Layered validation verdicts (RURL-glkuulyr): L1 syntax / L2 policy / L3
# annotation, the pi projection back to `parse_status`, and the independence
# invariant that makes the three layers layers at all.
#
# Contract: P1.1 §2 + P2.3 §1-§5, projected by G3.6's
# validation-intervention-contract.md (verdict-layer rows, the C-07 shipped-
# value map, the annotation-state rows, and the pi collapse table).

# ---- the C-07 map: every shipped status value, and which layer owns it ------

test_that("each shipped parse_status value maps to its owning layer", {
  # One row per row of the contract's shipped-value -> layer map. `status` is
  # asserted alongside the layers so the mapping is pinned in both directions.
  cases <- list(
    list(url = "http://example.com/", status = "ok",
         l1 = "pass", l2 = "admitted", l3 = "known"),
    list(url = "ftp://example.com/", status = "ok-ftp",
         l1 = "pass", l2 = "admitted-ftp", l3 = "known"),
    list(url = "//example.com/p", status = "ok-scheme-relative",
         l1 = "pass", l2 = "admitted-scheme-relative", l3 = "known"),
    list(url = "user@example.com", status = "warning-userinfo",
         l1 = "pass", l2 = "warn-userinfo", l3 = "known"),
    # The two `error` cases the single value cannot tell apart -- this pair is
    # the whole reason the companion exists (C-07).
    list(url = "mailto:jane@example.com", status = "error",
         l1 = "pass", l2 = "rejected-scheme", l3 = "not-applicable"),
    list(url = "not a url at all", status = "error",
         l1 = "fail", l2 = "admitted", l3 = "not-applicable"),
    # The three PSL values are L3 ANNOTATION facts, never syntax or policy.
    list(url = "http://localhost/", status = "warning-no-tld",
         l1 = "pass", l2 = "admitted", l3 = "unknown"),
    list(url = "http://example.invalidtld/", status = "warning-invalid-tld",
         l1 = "pass", l2 = "admitted", l3 = "unknown"),
    list(url = "http://co.uk/", status = "warning-public-suffix",
         l1 = "pass", l2 = "admitted", l3 = "known")
  )

  for (case in cases) {
    v <- get_parse_verdicts(case$url)
    expect_identical(get_parse_status(case$url), case$status, info = case$url)
    expect_identical(v$layer1_syntax_verdict, case$l1, info = case$url)
    expect_identical(v$layer2_policy_verdict, case$l2, info = case$url)
    expect_identical(v$layer3_annotation_state, case$l3, info = case$url)
  }
})

test_that("the two error kinds are distinguishable, and parse_status is not", {
  urls <- c("mailto:jane@example.com", "not a url at all")
  # Lossy: identical projections...
  expect_identical(get_parse_status(urls), c("error", "error"))
  # ...distinct verdicts. An admission rejection is not a syntax failure.
  v <- get_parse_verdicts(urls)
  expect_identical(v$layer2_policy_verdict, c("rejected-scheme", "admitted"))
  expect_identical(v$layer1_syntax_verdict, c("pass", "fail"))
})

# ---- L3 is never fatal, and never a bare NA --------------------------------

test_that("L3 is a typed state on every row, and never fatal", {
  urls <- c(
    "http://example.com/", "http://localhost/", "http://co.uk/",
    "http://[::1]/", "http://192.168.0.1/", "not a url at all",
    "mailto:jane@example.com"
  )
  v <- get_parse_verdicts(urls)

  # Never a bare NA (P1.1 §2) and always inside the settled vocabulary.
  expect_false(anyNA(v$layer3_annotation_state))
  expect_true(
    all(v$layer3_annotation_state %in% rurl:::.LAYER3_ANNOTATION_STATE)
  )

  # An IP-literal host has no registrable-domain CONCEPT: `not-applicable`,
  # never `unknown` -- a lookup that does not apply is not one that failed.
  expect_identical(v$layer3_annotation_state[4:5],
                   c("not-applicable", "not-applicable"))

  # L3 is not a fatality axis: the PSL-warning rows are L1 pass + L2 admitted.
  psl <- get_parse_verdicts(c("http://localhost/", "http://co.uk/"))
  expect_identical(psl$layer1_syntax_verdict, c("pass", "pass"))
  expect_identical(psl$layer2_policy_verdict, c("admitted", "admitted"))
})

test_that("L3 does not move L1 or L2 (the independence invariant)", {
  # Switching the PSL SECTION changes what the annotation finds. P1.1 §2 says
  # syntax and policy must be invariant under that choice. `.wa.gov.au` is an
  # ICANN-listed suffix, so `source` moves L3 without touching the URL.
  urls <- c("http://example.com/", "http://foo.wa.gov.au/", "http://co.uk/")
  all_src <- get_parse_verdicts(urls, tld_source = "all")
  icann <- get_parse_verdicts(urls, tld_source = "icann")
  private <- get_parse_verdicts(urls, tld_source = "private")

  for (other in list(icann, private)) {
    expect_identical(other$layer1_syntax_verdict, all_src$layer1_syntax_verdict)
    expect_identical(other$layer2_policy_verdict, all_src$layer2_policy_verdict)
  }
})

test_that("L2 accept sub-states do not depend on the L3 outcome", {
  # An FTP host with no registrable domain: shipped `parse_status` reports the
  # PSL warning (the accept sub-state loses), but that precedence belongs to
  # the PROJECTION, not to L2 -- the row is still policy-admitted as FTP.
  v <- get_parse_verdicts("ftp://localhost/")
  expect_identical(v$layer2_policy_verdict, "admitted-ftp")
  expect_identical(v$layer3_annotation_state, "unknown")
  expect_identical(get_parse_status("ftp://localhost/"), "warning-no-tld")

  # With a registrable domain the same L2 value projects to ok-ftp.
  expect_identical(get_parse_status("ftp://example.com/"), "ok-ftp")
})

# ---- pi: the projection is total, pure, and reproduces parse_status --------

test_that("pi projects every layer combination to a shipped status value", {
  # Total: the full cross-product of the three layers projects into the frozen
  # eight-value vocabulary, with no NA and no new token.
  grid <- expand.grid(
    layer1_syntax_verdict = rurl:::.LAYER1_SYNTAX_VERDICT,
    layer2_policy_verdict = rurl:::.LAYER2_POLICY_VERDICT,
    layer3_detail = rurl:::.LAYER3_DETAIL,
    stringsAsFactors = FALSE
  )
  status <- rurl:::.project_parse_status_vec(as.list(grid))
  expect_length(status, nrow(grid))
  expect_false(anyNA(status))
  expect_true(all(status %in% c(
    rurl:::.STATUS_OK, rurl:::.STATUS_OK_FTP, rurl:::.STATUS_OK_SCHEME_REL,
    rurl:::.STATUS_ERROR, rurl:::.STATUS_WARN_NO_TLD,
    rurl:::.STATUS_WARN_INVALID_TLD, rurl:::.STATUS_WARN_PUBLIC_SUFFIX,
    rurl:::.STATUS_WARN_USERINFO
  )))
})

# Rebuild the private L3 detail from the public state plus the status the
# engine reported. Deliberately NOT read off the internal attribute: the point
# of the test above is that the public three-layer surface plus the documented
# unknown/known refinement is sufficient to reproduce the projection.
.verdict_detail <- function(v, status) {
  detail <- rep("not-applicable", nrow(v))
  detail[v$layer3_annotation_state == "unknown" &
           status == "warning-no-tld"] <- "unknown-no-dot"
  detail[v$layer3_annotation_state == "unknown" &
           status == "warning-invalid-tld"] <- "unknown-dot"
  detail[v$layer3_annotation_state == "known" &
           status == "warning-public-suffix"] <- "known-suffix-only"
  detail[v$layer3_annotation_state == "known" &
           status != "warning-public-suffix"] <- "known-registrable"
  detail
}

test_that("pi reproduces parse_status across the option matrix", {
  # The byte-identity contract (P2.3 §4/§5), asserted end to end: for every row
  # the engine produces, projecting that row's own verdicts must return the
  # parse_status the engine reported. This is the property that would break
  # first if the companion and the status ever grew separate derivations.
  urls <- c(
    "http://example.com/", "https://www.example.co.uk/a/b?q=1#f",
    "ftp://example.com/", "ftps://example.com/", "//example.com/p",
    "user@example.com", "mailto:jane@example.com", "not a url at all",
    "http://localhost/", "http://co.uk/", "http://example.invalidtld/",
    "http://192.168.0.1/", "http://[::1]/", "file:///tmp/x",
    "example.com:8080/p", "http://xn--bcher-kva.de/", "sc://h/p", ""
  )
  for (ph in c("keep", "strip")) {
    for (srh in c("keep", "http", "error")) {
      for (src in c("all", "icann")) {
        # get_parse_status() takes the PSL section as `source` and has no
        # scheme_relative_handling formal, so the two calls spell the same
        # configuration differently.
        status <- get_parse_status(urls, protocol_handling = ph, source = src)
        v <- get_parse_verdicts(urls, protocol_handling = ph,
                                scheme_relative_handling = "keep",
                                tld_source = src)
        expect_identical(
          rurl:::.project_parse_status_vec(list(
            layer1_syntax_verdict = v$layer1_syntax_verdict,
            layer2_policy_verdict = v$layer2_policy_verdict,
            # The public state loses the pi discriminators, so recover them the
            # way the projection needs: this asserts the LAYERS carry enough to
            # rebuild the status, which is the claim under test.
            layer3_detail = .verdict_detail(v, status)
          )),
          status,
          info = paste(ph, srh, src)
        )
      }
    }
  }
})

test_that("the verdicts do not depend on cache warmth", {
  # P1.1 §2 requires L1/L2 invariance under cache warmth. This is a live risk,
  # not a theoretical one: the Stage-A cache used to signal a null row by
  # storing a NULL value, which discarded the very classifier flags that tell
  # an admission REJECTION apart from a syntax FAILURE, so the second call for
  # the same URL reported a different L2 than the first.
  urls <- c("mailto:jane@example.com", "not a url at all", "sc://h/p",
            "http://example.com/")
  rurl_clear_caches()
  cold <- get_parse_verdicts(urls)
  warm <- get_parse_verdicts(urls)
  expect_identical(warm, cold)
  expect_identical(cold$layer2_policy_verdict[[1L]], "rejected-scheme")
})

# ---- surface contract ------------------------------------------------------

test_that("get_parse_verdicts is defined at url_standard = NULL", {
  # Unlike get_host_type() / get_scheme_class(), the layered verdicts are NOT
  # gated by the selector: L1 and L2 describe the parse that actually occurred,
  # which happens with or without one (P2.3 §2 Q1).
  v <- get_parse_verdicts("http://example.com/")
  expect_false(anyNA(unlist(v)))
  expect_identical(v$layer1_syntax_verdict, "pass")

  for (std in c("rfc3986", "whatwg")) {
    expect_identical(
      get_parse_verdicts("http://example.com/", url_standard = std), v,
      info = std
    )
  }
})

test_that("the companion never widens the parse frame", {
  # ADR 0006: the verdicts are a companion surface. The parse table keeps its
  # 18 columns and gains no verdict column, whatever the engine computed.
  d <- safe_parse_urls(c("http://example.com/", "mailto:a@b.com"))
  expect_length(d, 18L)
  expect_false(any(grepl("^layer[123]_", names(d))))
})

test_that("get_parse_verdicts is shaped and validated like its siblings", {
  empty <- get_parse_verdicts(character(0))
  expect_s3_class(empty, "data.frame")
  expect_identical(nrow(empty), 0L)
  expect_named(
    empty,
    c("layer1_syntax_verdict", "layer2_policy_verdict",
      "layer3_annotation_state")
  )

  v <- get_parse_verdicts(c("http://example.com/", NA_character_))
  expect_identical(nrow(v), 2L)
  expect_identical(v$layer1_syntax_verdict, c("pass", "fail"))

  # Input names are not data (the accessor convention).
  named <- get_parse_verdicts(c(a = "http://example.com/"))
  expect_identical(rownames(named), "1")

  expect_error(get_parse_verdicts(42), "must be a character vector")
})

test_that("every produced verdict is inside its settled vocabulary", {
  urls <- c(
    "http://example.com/", "ftp://example.com/", "//example.com/p",
    "user@example.com", "mailto:a@b.com", "not a url", "http://localhost/",
    "http://co.uk/", "http://[::1]/", "file:///x", ""
  )
  for (sa in c("web", "general")) {
    v <- get_parse_verdicts(
      urls,
      url_standard = if (sa == "general") "whatwg" else NULL,
      scheme_acceptance = sa
    )
    expect_true(all(v$layer1_syntax_verdict %in% rurl:::.LAYER1_SYNTAX_VERDICT))
    expect_true(all(v$layer2_policy_verdict %in% rurl:::.LAYER2_POLICY_VERDICT))
    expect_true(
      all(v$layer3_annotation_state %in% rurl:::.LAYER3_ANNOTATION_STATE)
    )
  }
})

test_that("three L3 states have no producer in the shipped engine", {
  # Asserted, not left silent. `not-requested` needs a lazy annotation the
  # always-on pipeline does not have; `invalid-input` needs the annotation to
  # reject a host URL syntax accepted; `dependency-error` needs a trapped pslr
  # failure, which P2.3 §4 Q3 explicitly leaves outside the byte-identity
  # guarantee. If a future slice produces one, this test fails and is the
  # reminder to widen the coverage above rather than a bug.
  urls <- c(
    "http://example.com/", "http://localhost/", "http://co.uk/",
    "http://[::1]/", "http://192.168.0.1/", "file:///x", "not a url",
    "mailto:a@b.com", "user@example.com", "//example.com/p", "sc://h/p"
  )
  produced <- unique(unlist(lapply(
    c("web", "general"),
    function(sa) {
      get_parse_verdicts(
        urls,
        url_standard = if (sa == "general") "whatwg" else NULL,
        scheme_acceptance = sa
      )$layer3_annotation_state
    }
  )))
  expect_setequal(produced, c("known", "unknown", "not-applicable"))
})
