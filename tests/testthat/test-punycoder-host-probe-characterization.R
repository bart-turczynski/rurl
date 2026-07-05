# Characterization tests for the DNS-length/UTS-46 probe design (T5,
# RURL-kqmpbwye; design note: _scratch/T5-dns-uts46-probe-design-lock.md;
# epic RURL-uyjheruh, PRD v2 D3/D4, section 5.4, section 7 Q2/Q6).
#
# IMPORTANT: this file pins the empirically-verified behavior of the
# INSTALLED `punycoder` (>= 1.2.0; verified live against 1.2.0 on
# 2026-07-05) that the design note's accepted probe design relies on. There
# is no rurl production code for this axis yet -- the seam
# (`.punycoder_host_probe()`) is out of scope for this ticket and lands in
# T6 (RURL-vowqpmdg). These tests exercise `punycoder::host_normalize()` /
# `punycoder::validate_domain()` DIRECTLY.
#
# This is a VERSION-DRIFT TRIPWIRE, not a test of rurl behavior. If a future
# punycoder upgrade changes any of these results, these tests must fail
# LOUDLY so nobody ships T6's production seam against stale assumptions
# re-derived from a PRD instead of the currently-installed dependency.

# --- Rejected design 1: validate_domain() does not emit independent codes ---

test_that("validate_domain collapses simultaneous violations to one code", {
  # Simultaneously too-long (>63 octet label) AND STD3-invalid (underscore).
  host <- paste0(strrep("y", 70), "_z.com")
  strict_result <- punycoder::validate_domain(host, strict = TRUE)
  expect_false(strict_result$valid)
  # Only the STD3 fact surfaces; the length fact is silently dropped.
  expect_identical(
    strict_result$error_codes[[1]], "ascii_domain_characters"
  )
  # strict = FALSE does not recover the missing fact -- it just suppresses
  # everything and reports the domain as valid.
  lenient_result <- punycoder::validate_domain(host, strict = FALSE)
  expect_true(lenient_result$valid)
  expect_identical(lenient_result$error_codes[[1]], character(0))
})

# --- Rejected design 2: all-strict baseline + relax-one-flag is ambiguous ---

test_that("relax-one-flag-from-all-strict cannot separate 2-of-3 from 3-of-3", {
  long_label <- strrep("y", 70)
  # h1: fails use_std3 + verify_dns_length only (hyphens clean).
  h1 <- paste0(long_label, "_z.com")
  # h2: fails all three (adds leading/trailing hyphen violations).
  h2 <- paste0("-", long_label, "_z-.com")

  relax_one <- function(host, relax) {
    flags <- list(
      check_hyphens = TRUE, use_std3 = TRUE, verify_dns_length = TRUE
    )
    flags[[relax]] <- FALSE
    do.call(punycoder::host_normalize, c(list(x = host), flags))
  }

  # Every single-flag-relaxed call is NA for h1 -- indistinguishable from h2.
  for (flag in c("check_hyphens", "use_std3", "verify_dns_length")) {
    expect_true(is.na(relax_one(h1, flag)))
    expect_true(is.na(relax_one(h2, flag)))
  }
  # All-strict baseline is also NA for both -- the design gives zero signal.
  all_strict <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = TRUE, use_std3 = TRUE, verify_dns_length = TRUE
    )
  }
  expect_true(is.na(all_strict(h1)))
  expect_true(is.na(all_strict(h2)))
})

# --- Accepted design: all-relaxed baseline + enable-one-flag disambiguates --

test_that("enable-one-flag-from-all-relaxed correctly separates 2-of-3/3", {
  long_label <- strrep("y", 70)
  h1 <- paste0(long_label, "_z.com")          # std3 + length only, hyphens OK
  h2 <- paste0("-", long_label, "_z-.com")    # std3 + length + hyphens

  all_relaxed <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    )
  }
  call_a <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = TRUE, use_std3 = FALSE, verify_dns_length = FALSE
    )
  }
  call_b <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = TRUE, verify_dns_length = FALSE
    )
  }
  call_c <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = TRUE
    )
  }

  # Both fixtures pass the all-relaxed baseline (no structural problem).
  expect_false(is.na(all_relaxed(h1)))
  expect_false(is.na(all_relaxed(h2)))

  # call_a (hyphen-only) is the disambiguator: h1 passes, h2 fails.
  expect_false(is.na(call_a(h1)))
  expect_true(is.na(call_a(h2)))

  # call_b/call_c fail identically for both (both genuinely violate std3
  # and length) -- this is correct, not an ambiguity, since call_a already
  # separated the two hosts.
  expect_true(is.na(call_b(h1)))
  expect_true(is.na(call_b(h2)))
  expect_true(is.na(call_c(h1)))
  expect_true(is.na(call_c(h2)))
})

# --- Baseline guard: empty label is NA even with all three flags relaxed ---

test_that("baseline guard: structural failures stay NA under all-relaxed", {
  all_relaxed <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    )
  }
  for (host in c("a..com", "..com", ".", "", "a...b.com")) {
    expect_true(is.na(all_relaxed(host)), info = host)
  }
  # Control: a trailing root dot is NOT an empty label under host_normalize.
  expect_false(is.na(all_relaxed("a.com.")))
})

# --- domain-empty-label: rurl-owned structural detector (strsplit-based) ---

test_that("the strsplit-based empty-label detector matches host_normalize", {
  has_empty_label <- function(host) {
    labels <- strsplit(host, ".", fixed = TRUE)[[1]]
    !all(nzchar(labels)) || length(labels) == 0L
  }
  expect_true(has_empty_label("a..com"))
  expect_true(has_empty_label("..com"))
  expect_true(has_empty_label("a...b.com"))
  expect_true(has_empty_label("."))
  expect_true(has_empty_label(""))
  # Must NOT false-positive on a valid FQDN trailing dot.
  expect_false(has_empty_label("a.com."))
  expect_false(has_empty_label("example.com"))
})

# --- Length boundaries: exact RFC 1035 octet limits, empirically pinned ---

test_that("label length boundary is exactly 63 ACE octets", {
  probe_c <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = TRUE
    )
  }
  expect_false(is.na(probe_c(paste0(strrep("a", 63), ".com"))))
  expect_true(is.na(probe_c(paste0(strrep("a", 64), ".com"))))
})

test_that("name length boundary is exactly 253 octets, excl. trailing dot", {
  probe_c <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = TRUE
    )
  }
  mk_name_of_len <- function(total_len) {
    labels <- character(0)
    remaining <- total_len
    while (remaining > 0) {
      if (remaining <= 63) {
        labels <- c(labels, strrep("a", remaining))
        remaining <- 0
      } else {
        labels <- c(labels, strrep("a", 63))
        remaining <- remaining - 63 - 1
      }
    }
    paste(labels, collapse = ".")
  }
  name_253 <- mk_name_of_len(253)
  name_254 <- mk_name_of_len(254)
  expect_identical(nchar(name_253), 253L)
  expect_identical(nchar(name_254), 254L)
  expect_false(is.na(probe_c(name_253)))
  expect_true(is.na(probe_c(name_254)))
  # A trailing root dot does not count against the 253-octet limit.
  expect_false(is.na(probe_c(paste0(name_253, "."))))
})

test_that("DNS length is enforced on the ACE-encoded label, not raw Unicode", {
  probe_c <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = TRUE
    )
  }
  baseline <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    )
  }
  # 57 raw "e-acute" chars encode to a 63-octet xn-- label (passes);
  # 58 encode to a 64-octet xn-- label (fails). The raw character counts
  # (57/58) are themselves well under 63 -- proof the check is NOT counting
  # raw Unicode codepoints.
  host_57 <- paste0(strrep("é", 57), ".com")
  host_58 <- paste0(strrep("é", 58), ".com")
  ace_label_57 <- strsplit(baseline(host_57), ".", fixed = TRUE)[[1]][1]
  ace_label_58 <- strsplit(baseline(host_58), ".", fixed = TRUE)[[1]][1]
  expect_identical(nchar(ace_label_57), 63L)
  expect_identical(nchar(ace_label_58), 64L)
  expect_false(is.na(probe_c(host_57)))
  expect_true(is.na(probe_c(host_58)))
})

# --- Length subtyping: validate_domain() collapses combined failures too ---

test_that("validate_domain distinguishes label-vs-name length in isolation", {
  label_too_long_only <- paste0(strrep("a", 64), ".com")
  name_too_long_only <- paste(rep(strrep("a", 60), 5), collapse = ".")
  expect_identical(
    punycoder::validate_domain(
      label_too_long_only, strict = TRUE
    )$error_codes[[1]],
    "domain_label_too_long"
  )
  expect_identical(
    punycoder::validate_domain(
      name_too_long_only, strict = TRUE
    )$error_codes[[1]],
    "domain_too_long"
  )
})

test_that("validate_domain drops the label fact when both length rules fail", {
  # A single 300-octet label independently violates BOTH the 63-octet
  # label limit and the 253-octet name limit.
  both_fail <- paste0(strrep("a", 300), ".com")
  codes <- punycoder::validate_domain(both_fail, strict = TRUE)$error_codes[[1]]
  # Only the name-level code survives; the label-level fact is lost, exactly
  # mirroring the D3 correction-1 single-code-collapse pattern one level
  # down. This is why T6 must NOT use a scoped validate_domain() call for
  # length subtyping.
  expect_identical(codes, "domain_too_long")
  expect_false("domain_label_too_long" %in% codes)
})

test_that("the rurl-owned length classifier reports both facts independently", {
  both_fail <- paste0(strrep("a", 300), ".com")
  labels <- strsplit(both_fail, ".", fixed = TRUE)[[1]]
  name <- sub("[.]$", "", both_fail)
  expect_true(any(nchar(labels) > 63L))
  expect_gt(nchar(name), 253L)
})

# --- Open Question 2: use_std3 vs WHATWG forbidden-host-code-point set -----

test_that("isolated use_std3 catches every directly-testable forbidden char", {
  call_b <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = TRUE, verify_dns_length = FALSE
    )
  }
  # NUL (U+0000) is deliberately excluded: intToUtf8(0x00) yields a
  # zero-length string in R (character vectors cannot embed a literal NUL
  # byte), so it cannot be constructed as a test fixture at all -- this is
  # an R-level limitation, not a punycoder finding either way. The other
  # C0 controls (TAB/LF/CR/ESC/US/DEL) plus SPACE are built via intToUtf8()
  # rather than embedded as literal bytes in this source file.
  control_points <- c(0x09L, 0x0AL, 0x0DL, 0x1BL, 0x1FL, 0x20L, 0x7FL)
  controls <- vapply(control_points, intToUtf8, character(1))
  punctuation <- c(
    "#", "%", "/", ":", "<", ">", "?", "@", "[", "\\", "]", "^", "|"
  )
  forbidden <- c(controls, punctuation)
  for (ch in forbidden) {
    host <- paste0("exa", ch, "mple.com")
    info <- sprintf("code point %d", utf8ToInt(ch))
    expect_true(is.na(call_b(host)), info = info)
  }
})

test_that("use_std3 is a superset: rejects non-LDH ASCII outside WHATWG set", {
  call_b <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = TRUE, verify_dns_length = FALSE
    )
  }
  # These are NOT in WHATWG's forbidden-host-code-point list, but classic
  # STD3/LDH hostname rules reject them anyway.
  for (ch in c("_", "+", "~", "*", "$")) {
    host <- paste0("exa", ch, "mple.com")
    expect_true(is.na(call_b(host)), info = ch)
  }
  # Control: legitimate IDN Unicode and interior hyphen/digit pass through.
  expect_false(is.na(call_b("café.com")))
  expect_false(is.na(call_b("exa-2mple.com")))
})

# --- domain-hyphen-violation: isolated check_hyphens covers full CheckHyphens

test_that("isolated check_hyphens covers leading/trailing/position-3-4 rules", {
  call_a <- function(host) {
    punycoder::host_normalize(
      host, check_hyphens = TRUE, use_std3 = FALSE, verify_dns_length = FALSE
    )
  }
  expect_true(is.na(call_a("-example.com")))
  expect_true(is.na(call_a("example-.com")))
  expect_true(is.na(call_a("-example-.com")))
  # Position-3-4 double hyphen (ACE-lookalike rule).
  expect_true(is.na(call_a("ex--ample.com")))
  # Legitimate interior hyphens pass.
  expect_false(is.na(call_a("ex-ample.com")))
  expect_false(is.na(call_a("exa-2mple.com")))
})
