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

test_that("the trailing root dot and the DNS-length call do not move", {
  relaxed_but <- function(host, verify_dns_length = FALSE) {
    punycoder::host_normalize(
      host, check_hyphens = FALSE, use_std3 = FALSE,
      verify_dns_length = verify_dns_length
    )
  }
  # A trailing root dot is no empty label: it survives the all-relaxed call
  # and the DNS-length call, spelled as written.
  expect_identical(relaxed_but("a.com."), "a.com.")
  expect_identical(relaxed_but("a.com.", verify_dns_length = TRUE), "a.com.")
  # UTS #46 section 4.2 step 4: with VerifyDnsLength true every label must be
  # 1 to 63 octets, so an empty label fails the DNS-length call on every
  # punycoder version. The probe's length subtyping runs on these hosts and
  # must find no length fact unless a label or the name is genuinely too long.
  for (host in c("a..com", "..com", "a...b.com", ".", "", "a..com.")) {
    expect_true(is.na(relaxed_but(host, verify_dns_length = TRUE)), info = host)
  }
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

# --- The Unicode pin rurl inherits from punycoder (RURL-csdmuguh) ------------
#
# rurl passes no `unicode_version =` to punycoder anywhere: it deliberately
# INHERITS punycoder's default table, so every IDN expectation in this package
# is implicitly derived under whatever table the installed punycoder compiled.
# This pins what was inherited, so that a pin move in punycoder fails HERE
# first -- loudly, and naming the table -- rather than surfacing as scattered
# fixture diffs in test-format-url.R and the WPT suite with nothing saying why.
#
# `normalization_profile_info()` is exported by every released punycoder the
# floor admits (read from the CRAN tarball NAMESPACEs of 1.1.0 and 1.2.1), so
# no dev-version skip guard is needed. Only the VALUE differs between released
# and development punycoder, and both populations are pinned below, measured
# 2026-09-04: CRAN 1.2.1, built into a throwaway library, reports Unicode
# 16.0.0 under token `uts46-nontransitional-std3-v1` (its src/ carries exactly
# one table, unicode_tables_16_0_0.cpp); the 1.2.1.9000 checkout reports
# 17.0.0 under `uts46-nontransitional-std3-v2`. The boundary names the DEV
# version because that is the lowest version that truthfully carries the new
# table. When punycoder next moves its pin, update the literal for the
# population it moved on, and record the move in NEWS.md.

test_that("the Unicode pin rurl inherits from punycoder is the recorded one", {
  info <- punycoder::normalization_profile_info()
  expect_s3_class(info, "data.frame")
  expect_identical(nrow(info), 1L)
  expected <- if (utils::packageVersion("punycoder") >= "1.2.1.9000") {
    list(unicode_version = "17.0.0", profile = "uts46-nontransitional-std3-v2")
  } else {
    list(unicode_version = "16.0.0", profile = "uts46-nontransitional-std3-v1")
  }
  expect_identical(info$unicode_version, expected$unicode_version)
  expect_identical(info$profile, expected$profile)
})

# --- domain-invalid-ace-label: the per-label call (RURL-vicyvlvh) -----------
#
# The probe runs each `xn--` label ALONE through the all-relaxed call, which is
# the non-strict UTS #46 processing WHATWG's domain parser asks for
# (CheckHyphens, UseSTD3ASCIIRules, VerifyDnsLength false; CheckBidi and
# CheckJoiners true). These pins record which Validity Criteria (UTS #46
# section 4.1) the installed punycoder enforces on one label, and the one it
# does not (criterion 4), which rurl therefore checks itself on the decoded
# label. Fixtures were Punycode-encoded with Python's standard codec, so none of
# them round-trips through punycoder's own encoder.

test_that("the all-relaxed per-label call rejects every invalid ACE class", {
  relaxed <- function(label) {
    punycoder::host_normalize(
      label, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    )
  }
  invalid <- c(
    decode_fails = "xn--a",
    empty_after_prefix = "xn--",
    ascii_only_result = "xn--ascii-",
    ascii_only_short = "xn--a-",
    non_ascii_in_ace = "xn--bücher",
    not_nfc = "xn--a-ccb", # criterion 1: "a" + U+0308, not NFC
    leading_mark = "xn--a-wbb", # criterion 6: U+0301 then "a"
    status_mapped = "xn--7ba", # criterion 7: U+00C4, a mapped code point
    status_disallowed = "xn--a-ba", # criterion 7: U+0080
    context_j = "xn--ab-m1t", # criterion 8: ZWJ between two letters
    bidi_one_label = "xn--a-zhc" # criterion 9: U+05D0 then "a", one label
  )
  for (nm in names(invalid)) {
    expect_true(is.na(relaxed(invalid[[nm]])), info = nm)
  }
  valid <- c(
    "xn--bcher-kva", "xn--nxasmq6b", "xn--zca", "xn--4dbrk0ce", "xn--ls8h",
    "xn--1-0fa"
  )
  for (label in valid) {
    expect_identical(relaxed(label), label, info = label)
  }
  # ASCII case is folded by the UTS #46 mapping step before decoding.
  expect_identical(relaxed("XN--BCHER-KVA"), "xn--bcher-kva")
})

test_that("the per-label call misses criterion 4 (decoded label is xn--)", {
  # UTS #46 section 4.1 criterion 4: "If not CheckHyphens, the label must not
  # begin with 'xn--'." "xn--xn---ooa" decodes to "xn--" + U+00E4; punycoder
  # accepts it with check_hyphens = FALSE, so rurl tests the decoded label
  # itself. If this starts failing, punycoder closed the gap and rurl's own
  # check has become redundant, not wrong.
  expect_identical(
    punycoder::host_normalize(
      "xn--xn---ooa",
      check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    ),
    "xn--xn---ooa"
  )
  # puny_decode() returns UTF-8 bytes without an encoding mark, so under a
  # non-UTF-8 locale (the gate's LC_ALL=C cell) the strings compare unequal;
  # compare the bytes. The probe itself only tests the ASCII "xn--" prefix.
  decoded_bytes <- function(label) {
    charToRaw(punycoder::puny_decode(label, strict = FALSE))
  }
  expect_identical(decoded_bytes("xn--xn---ooa"), charToRaw("xn--ä"))
})

test_that("punycoder's non-LDH basic code point decode varies by version", {
  # RFC 3492 section 6.2 accepts any basic (ASCII) code point before the last
  # delimiter, and UTS #46 section 4 step 4 decodes with it, so "xn--a_-wia"
  # is "a_" + U+00E4, valid when UseSTD3ASCIIRules is false. punycoder 1.2.1
  # decodes it in both calls below. 1.3.0's in-tree decoder requires
  # letter-digit-hyphen basic code points, so host_normalize(), which always
  # uses it, returns NA. puny_decode() tries libidn2 first where punycoder was
  # built with it (CRAN's Debian checks, not its Windows ones), and libidn2
  # still decodes the label; without libidn2 it returns NA too. That move is
  # why rurl decodes ACE payloads itself (RURL-mfmgauos); these expectations
  # describe punycoder, and rurl depends on none of these answers.
  relaxed <- punycoder::host_normalize(
    "xn--a_-wia",
    check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
  )
  lenient <- punycoder::puny_decode("xn--a_-wia", strict = FALSE)
  expect_true(identical(relaxed, "xn--a_-wia") || is.na(relaxed))
  # No punycoder version or backend accepts the label in host_normalize()
  # while rejecting it in the raw decoder.
  if (!is.na(relaxed)) {
    expect_false(is.na(lenient))
  }
  if (!is.na(lenient)) {
    expect_identical(charToRaw(lenient), charToRaw("a_ä"))
  }
  expect_error(punycoder::puny_decode("xn--a_-wia", strict = TRUE))
  # The decoded Unicode label is accepted by both versions: rurl feeds
  # punycoder decoded labels, never its A-labels.
  expect_identical(
    punycoder::host_normalize(
      "a_ä", check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    ),
    "xn--a_-wia"
  )
})

test_that("rurl's own RFC 3492 decode is independent of punycoder", {
  decode_bytes <- function(payload) charToRaw(rurl:::.rfc3492_decode(payload))
  expect_identical(decode_bytes("a_-wia"), charToRaw("a_ä"))
  expect_identical(decode_bytes("xn--_-kra"), charToRaw("xn--_ä"))
  expect_identical(decode_bytes("bcher-kva"), charToRaw("bücher"))
  # RFC 3492 section 7.1 samples (A), (L) and (M): no basic string, mixed-case
  # basic code points, and a basic string that contains the delimiter.
  expect_identical(
    decode_bytes("egbpdaj6bu4bxfgehfvwxn"),
    charToRaw(intToUtf8(c(
      0x0644, 0x064A, 0x0647, 0x0645, 0x0627, 0x0628, 0x062A, 0x0643, 0x0644,
      0x0645, 0x0648, 0x0634, 0x0639, 0x0631, 0x0628, 0x064A, 0x061F
    )))
  )
  expect_identical(
    decode_bytes("3B-ww4c5e180e575a65lsy2b"),
    charToRaw(intToUtf8(c(
      0x33, 0x5E74, 0x42, 0x7D44, 0x91D1, 0x516B, 0x5148, 0x751F
    )))
  )
  expect_identical(
    decode_bytes("-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n"),
    charToRaw(paste0(
      intToUtf8(c(0x5B89, 0x5BA4, 0x5948, 0x7F8E, 0x6075)),
      "-with-SUPER-MONKEYS"
    ))
  )
  # Failures: a digit outside a-z/0-9, a truncated integer, a delimiter at
  # position 0 read as a digit, overflow, and a non-ASCII payload.
  bad_payloads <- c(
    "a_-w!a", "a-z", "-", "99999999999", "zzzzzzzzzzz", "bücher-"
  )
  for (bad in bad_payloads) {
    expect_true(is.na(rurl:::.rfc3492_decode(bad)), info = bad)
  }
})

test_that("cross-label Bidi fails the whole host but not each label alone", {
  # Criterion 9 applies only in a Bidi domain name, which depends on the OTHER
  # labels: "1" + U+00E4 is valid alone and invalid beside a Hebrew label. The
  # per-label probe cannot see that context, by design; ?get_url_diagnostics
  # states the exclusion.
  relaxed <- function(x) {
    punycoder::host_normalize(
      x, check_hyphens = FALSE, use_std3 = FALSE, verify_dns_length = FALSE
    )
  }
  expect_true(is.na(relaxed("xn--1-0fa.xn--4dbrk0ce")))
  expect_identical(relaxed("xn--1-0fa"), "xn--1-0fa")
  expect_identical(relaxed("xn--4dbrk0ce"), "xn--4dbrk0ce")
})
