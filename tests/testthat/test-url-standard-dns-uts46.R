# Tests for the DNS-length/UTS-46 diagnostic seam (T6, RURL-vowqpmdg; epic
# RURL-uyjheruh, PRD v2 D3/D4, §5.3, §5.4). Locked design in T5
# (RURL-kqmpbwye; _scratch/T5-dns-uts46-probe-design-lock.md), production seam
# is .punycoder_host_probe() (R/domain.R), wired into .derive_url_metadata_vec()
# (R/diagnostics.R). Adds 5 tokens: domain-label-too-long, domain-name-too-long,
# domain-empty-label, domain-hyphen-violation, domain-std3-violation.
#
# These are FACTS about host shape, not policy: they fire identically under
# both "rfc3986" and "whatwg" (PRD D4), and never at all with no selector.

# --- clean host: no false positives ------------------------------------------

test_that("a clean ASCII host has no DNS/UTS-46 diagnostics", {
  expect_identical(
    get_url_diagnostics("http://example.com/", url_standard = "whatwg"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics("http://example.com/", url_standard = "rfc3986"),
    character(0)
  )
})

test_that("a clean Unicode IDN host has no DNS/UTS-46 diagnostics", {
  expect_identical(
    get_url_diagnostics("http://café.com/", url_standard = "whatwg"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics("http://xn--caf-dma.com/", url_standard = "whatwg"),
    character(0)
  )
})

# --- domain-empty-label -------------------------------------------------------

test_that("domain-empty-label fires for a host with a zero-length label", {
  for (host in c("http://a..com/", "http://..com/", "http://a...b.com/")) {
    expect_identical(
      get_url_diagnostics(host, url_standard = "whatwg"), "domain-empty-label",
      info = host
    )
  }
})

test_that("domain-empty-label does NOT fire on a trailing root-dot FQDN", {
  expect_identical(
    get_url_diagnostics("http://example.com./", url_standard = "whatwg"),
    character(0)
  )
})

test_that("an all-relaxed-baseline-failure host gets ONLY domain-empty-label", {
  # An empty-label host is NA even under the all-relaxed baseline, so it must
  # be routed to the structural detector alone -- never misread as failing
  # the hyphen/std3/length checks too (T5's baseline-guard finding).
  diags <- get_url_diagnostics("http://a..com/", url_standard = "whatwg")
  expect_identical(diags, "domain-empty-label")
  expect_false("domain-hyphen-violation" %in% diags)
  expect_false("domain-std3-violation" %in% diags)
  expect_false("domain-label-too-long" %in% diags)
  expect_false("domain-name-too-long" %in% diags)
})

# --- domain-hyphen-violation --------------------------------------------------

test_that("domain-hyphen-violation fires for leading/trailing/double hyphens", {
  for (host in c(
    "http://-example.com/", "http://example-.com/", "http://ex--ample.com/"
  )) {
    expect_identical(
      get_url_diagnostics(host, url_standard = "whatwg"),
      "domain-hyphen-violation",
      info = host
    )
  }
})

test_that("a legitimate interior hyphen does not fire the diagnostic", {
  expect_identical(
    get_url_diagnostics("http://ex-ample.com/", url_standard = "whatwg"),
    character(0)
  )
})

# --- domain-std3-violation ----------------------------------------------------

test_that("domain-std3-violation fires for non-LDH ASCII outside WHATWG set", {
  # Superset behavior (T5 Open Question 2): use_std3 also catches classic
  # STD3/LDH violations WHATWG itself does not forbid at the host-code-point
  # level (underscore, tilde). "+"/"*"/"$" are also caught by use_std3 (per
  # T5's characterization), but libcurl itself rejects them as "Bad hostname"
  # before the URL ever reaches the diagnostics seam, so they cannot be
  # exercised end-to-end through get_url_diagnostics() and are omitted here.
  for (ch in c("_", "~")) {
    host <- paste0("http://exa", ch, "mple.com/")
    expect_identical(
      get_url_diagnostics(host, url_standard = "whatwg"),
      "domain-std3-violation",
      info = ch
    )
  }
})

test_that("legitimate IDN Unicode and digits do not fire the std3 diagnostic", {
  expect_identical(
    get_url_diagnostics("http://café.com/", url_standard = "whatwg"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics("http://exa2mple.com/", url_standard = "whatwg"),
    character(0)
  )
})

# --- domain-label-too-long / domain-name-too-long -----------------------------

test_that("domain-label-too-long fires for a single label over 63 ACE octets", {
  host <- paste0("http://", strrep("a", 64), ".com/")
  expect_identical(
    get_url_diagnostics(host, url_standard = "whatwg"), "domain-label-too-long"
  )
})

test_that("a 63-octet label does not fire domain-label-too-long", {
  host <- paste0("http://", strrep("a", 63), ".com/")
  expect_identical(
    get_url_diagnostics(host, url_standard = "whatwg"), character(0)
  )
})

test_that("domain-name-too-long fires for a name over 253 ACE octets", {
  name_254 <- paste(rep(strrep("a", 60), 5), collapse = ".") # 5*60 + 4 = 304
  # Trim to exactly 254 octets to isolate the name-length rule (no single
  # label exceeds 63 octets here).
  name_254 <- substr(name_254, 1, 254)
  host <- paste0("http://", name_254, "/")
  expect_identical(
    get_url_diagnostics(host, url_standard = "whatwg"), "domain-name-too-long"
  )
})

test_that("both length facts co-fire independently on one 300-octet label", {
  # T5's key finding: validate_domain() collapses this to a single code
  # ("domain_too_long"), dropping the label-level fact. rurl's classifier
  # must report BOTH tokens, unlike that single-code collapse.
  host <- paste0("http://", strrep("a", 300), ".com/")
  diags <- get_url_diagnostics(host, url_standard = "whatwg")
  expect_setequal(diags, c("domain-label-too-long", "domain-name-too-long"))
})

test_that("DNS length is enforced on the ACE-encoded label, not raw Unicode", {
  # 57 raw e-acute chars encode to a 63-octet xn-- label (passes); 58 encode
  # to 64 octets (fails). Both raw counts are well under 63 -- proof the rurl
  # classifier is not counting raw Unicode codepoints (T5 (b)).
  host_57 <- paste0("http://", strrep("é", 57), ".com/")
  host_58 <- paste0("http://", strrep("é", 58), ".com/")
  expect_identical(
    get_url_diagnostics(host_57, url_standard = "whatwg"), character(0)
  )
  expect_identical(
    get_url_diagnostics(host_58, url_standard = "whatwg"),
    "domain-label-too-long"
  )
})

# --- multi-failure hosts: independent facts, not a single-code collapse ------

test_that("a host failing std3 AND length co-fires both tokens", {
  host <- paste0("http://", strrep("y", 70), "_z.com/")
  diags <- get_url_diagnostics(host, url_standard = "whatwg")
  expect_setequal(diags, c("domain-label-too-long", "domain-std3-violation"))
})

test_that("adding a hyphen violation on top adds a third independent token", {
  long_label <- strrep("y", 70)
  host <- paste0("http://-", long_label, "_z-.com/")
  diags <- get_url_diagnostics(host, url_standard = "whatwg")
  expect_setequal(diags, c(
    "domain-label-too-long", "domain-std3-violation", "domain-hyphen-violation"
  ))
})

# --- facts, not policy: identical under BOTH standard modes -------------------

test_that("DNS/UTS-46 diagnostics fire identically under rfc3986 and whatwg", {
  hosts <- c(
    "http://example.com/",
    "http://a..com/",
    "http://-example.com/",
    "http://exa_mple.com/",
    paste0("http://", strrep("a", 64), ".com/")
  )
  whatwg <- lapply(hosts, get_url_diagnostics, url_standard = "whatwg")
  rfc <- lapply(hosts, get_url_diagnostics, url_standard = "rfc3986")
  expect_identical(whatwg, rfc)
})

# --- IP literals are excluded from the probe ----------------------------------

test_that("IPv4/IPv6 hosts never spuriously fire DNS/UTS-46 diagnostics", {
  expect_identical(
    get_url_diagnostics("http://127.0.0.1/", url_standard = "whatwg"),
    character(0)
  )
  # A bracketed IPv6 literal's "[", "]", ":" would otherwise be misread as an
  # STD3 violation by an un-gated isolated use_std3 probe call.
  expect_identical(
    get_url_diagnostics("http://[::1]/", url_standard = "whatwg"),
    character(0)
  )
  expect_identical(
    get_url_diagnostics("http://[2001:db8::1]/", url_standard = "rfc3986"),
    character(0)
  )
})

# --- no selector: no diagnostics, byte-for-byte baseline unchanged -----------

test_that("no url_standard yields no DNS/UTS-46 diagnostics", {
  expect_identical(get_url_diagnostics("http://a..com/"), character(0))
  expect_identical(get_url_diagnostics("http://-example.com/"), character(0))
  expect_identical(
    get_url_diagnostics(paste0("http://", strrep("a", 300), ".com/")),
    character(0)
  )
})

test_that("no url_standard leaves safe_parse_url() byte-for-byte unchanged", {
  urls <- c(
    "http://a..com/", "http://-example.com/", "http://exa_mple.com/"
  )
  no_selector <- lapply(urls, safe_parse_url)
  explicit_null <- lapply(urls, safe_parse_url, url_standard = NULL)
  expect_identical(no_selector, explicit_null)
})

# --- vector input: per-URL token lists ----------------------------------------

test_that("vector input returns one diagnostics vector per URL", {
  urls <- c(
    "http://example.com/", "http://a..com/", "http://-example.com/"
  )
  diags <- get_url_diagnostics(urls, url_standard = "whatwg")
  expect_length(diags, 3L)
  expect_identical(diags[[1L]], character(0))
  expect_identical(diags[[2L]], "domain-empty-label")
  expect_identical(diags[[3L]], "domain-hyphen-violation")
})

# --- no widening of safe_parse_url()/safe_parse_urls() output shape ---------

test_that("the DNS/UTS-46 seam adds no new columns/fields", {
  cols_before <- names(safe_parse_urls("http://host/path"))
  invisible(safe_parse_urls("http://a..com/path", url_standard = "whatwg"))
  cols_after <- names(safe_parse_urls("http://host/path"))
  expect_identical(cols_after, cols_before)

  fields_before <- names(safe_parse_url("http://host/path"))
  fields_after <- names(
    safe_parse_url("http://a..com/path", url_standard = "whatwg")
  )
  expect_identical(fields_after, fields_before)
})

# --- .URL_DIAGNOSTICS vocabulary contains the 5 new tokens -------------------

test_that("the 5 DNS/UTS-46 tokens are registered in .URL_DIAGNOSTICS", {
  expect_true(all(
    c(
      "domain-label-too-long", "domain-name-too-long", "domain-empty-label",
      "domain-hyphen-violation", "domain-std3-violation"
    ) %in% rurl:::.URL_DIAGNOSTICS
  ))
})
