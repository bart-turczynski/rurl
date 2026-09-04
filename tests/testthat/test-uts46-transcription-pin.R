# Offline pin on the UTS-46 slice transcribed by the ip-obfuscation oracle
# derivation (RUL-020, carrier RURL-mrnktxqp).
#
# WHAT THIS GUARDS, AND WHAT IT DOES NOT.
# `tools/oracle/derive-ip-obfuscation.R` does not implement UTS-46. It
# transcribes exactly one slice of the IDNA mapping table -- the three
# full-stop variants and the ASCII uppercase mappings, 3 + 26 = 29 facts -- and
# ABORTS on every other non-ASCII code point rather than guessing.
#
# `tools/oracle/check-uts46-mapping-pin.R` already checks those 29 facts
# against every published IdnaMappingTable.txt, digest-first. It reads
# unicode.org, so it is deliberately NOT a gate and runs by hand. That leaves
# one direction unguarded and it is the per-PR-meaningful one: a LOCAL edit to
# `ipobf_uts46_to_ascii()` -- adding a wrong mapping, or dropping a right one --
# is caught by nothing.
#
# This test closes that direction and only that direction. The expectations
# below are DUPLICATED LITERALS, transcribed from
# `check-uts46-mapping-pin.R::uts46_expected_mappings()`, not sourced from it:
# the two copies are deliberately independent so that editing one does not
# silently move the other. They are therefore NOT independent evidence about
# Unicode -- they cannot detect upstream drift and do not claim to. Upstream
# authority stays with the network check and its per-version sha256 pins
# (RUL-020 records this split explicitly). Nothing is vendored here: the
# ip-obfuscation group stays `section_2_3_applies = false`.

# `tools/` ships in the tarball but is not installed into the package library,
# so the derivation is unreachable when the suite runs against an installed
# package (R CMD check does reach it -- the tarball root is the check dir's
# parent). Skip rather than fail: the assertion is about a repository file, and
# its absence is a property of the run, not a defect.
derivation_path <- testthat::test_path(
  "..", "..", "tools", "oracle", "derive-ip-obfuscation.R"
)

test_that("the derivation transcribes exactly the 29 UTS-46 mappings", {
  skip_if_not(
    file.exists(derivation_path),
    "tools/oracle/derive-ip-obfuscation.R is not reachable from this run"
  )

  # Safe to source: the script's driver is guarded by `sys.nframe() == 0L`,
  # which is non-zero inside source().
  env <- new.env(parent = globalenv())
  sys.source(derivation_path, envir = env)
  expect_type(env$ipobf_uts46_to_ascii, "closure")

  # --- the 29 facts, duplicated from check-uts46-mapping-pin.R --------------
  # 3 full-stop variants, each mapped to U+002E by the IDNA mapping table.
  full_stops <- c("。", "．", "｡")
  # 26 ASCII uppercase letters, each mapped to its lowercase.
  upper <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "", fixed = TRUE)[[1]]
  lower <- strsplit("abcdefghijklmnopqrstuvwxyz", "", fixed = TRUE)[[1]]
  expect_identical(length(full_stops) + length(upper), 29L)

  for (fs in full_stops) {
    expect_identical(
      env$ipobf_uts46_to_ascii(paste0("a", fs, "b")), "a.b",
      info = sprintf("full-stop variant U+%04X must map to U+002E",
                     utf8ToInt(fs))
    )
  }

  for (i in seq_along(upper)) {
    expect_identical(
      env$ipobf_uts46_to_ascii(upper[i]), lower[i],
      info = sprintf("%s must map to %s", upper[i], lower[i])
    )
  }

  # The transcription is case-folding only -- an already-lowercase ASCII host
  # and the ASCII full stop are fixed points, so the mappings above cannot be
  # passing by way of some broader rewrite.
  expect_identical(env$ipobf_uts46_to_ascii("example.com"), "example.com")
  expect_identical(env$ipobf_uts46_to_ascii("EXAMPLE.COM"), "example.com")
  expect_identical(env$ipobf_uts46_to_ascii(""), "")
})

test_that("the derivation refuses non-ASCII outside the transcribed set", {
  skip_if_not(
    file.exists(derivation_path),
    "tools/oracle/derive-ip-obfuscation.R is not reachable from this run"
  )

  env <- new.env(parent = globalenv())
  sys.source(derivation_path, envir = env)

  # The refusal is the other half of the pin: it is what makes "exactly 29"
  # true rather than "at least 29". A silently widened transcription would
  # start answering ToASCII questions the derivation never modeled.
  for (host in c("bücher.example", "İ.example", "a​b.com")) {
    expect_error(
      env$ipobf_uts46_to_ascii(host),
      "NOT MODELED",
      info = sprintf("must refuse to guess ToASCII for %s", host)
    )
  }
})
