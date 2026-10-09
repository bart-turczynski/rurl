# TRUE when the installed punycoder keeps empty labels under the all-relaxed
# flags, as UTS #46 section 4.2 step 4 does without VerifyDnsLength. punycoder
# shipped that change with profile revision -v3 (its ADR-018), so the profile
# token, not the version number, tells a build that carries it from one that
# does not: development builds numbered 1.3.0.9000 exist on both sides of the
# change (RURL-tsmevksk).
punycoder_keeps_empty_labels <- function() {
  profile <- punycoder::normalization_profile_info()$profile
  revision <- sub("^uts46-nontransitional-std3-v([0-9]+).*$", "\\1", profile)
  revision <- suppressWarnings(as.integer(revision))
  !is.na(revision) && revision >= 3L
}
