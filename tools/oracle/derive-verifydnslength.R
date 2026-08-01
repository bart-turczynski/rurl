#!/usr/bin/env Rscript
# RURL-ozdejfzl -- the two readings the `ada-verifydnslength` group turns on.
#
# THIS GROUP IS NOT WHAT ITS RECORD SAID IT WAS. Its normative_dependencies
# entry carried pin_status "not-applicable" with reason "no-derivation", and a
# note asserting that "every expected value here is READ OUT of vendored,
# hash-pinned bytes". That is false for 10 of the 17 rows, and false in the
# direction that matters. Upstream marks 10 entries `failure: true`; the fixture
# records standard_expectation = accept for ALL 17. Those ten expectations are
# not copied from anywhere -- they are HAND-DERIVED from the URL Standard's
# text, and under conventions.normative_dependency_scope that is exactly what
# creates a source-pinning duty.
#
# WHY THE TWO READINGS DISAGREE, which is the group's whole point. Ada's
# verify_dns_length is an OPTIONAL check implementing RFC 1035 section 2.3.4.
# The URL Standard does not perform it by default:
#
#   host parser step 6      -- run the DOMAIN PARSER with `domain` and FALSE
#   domain parser ToASCII   -- VerifyDnsLength is set to `beStrict`
#
# so with beStrict = false there is no DNS length verification at all, and step
# 5 returns an ASCII domain lowercased "regardless of Unicode ToASCII's
# outcome, due to web compatibility". Both quoted steps were read at
# whatwg/url 9dc3827fc722ac4af3f11061aa3e9adb44a17c8b, the revision this
# repository already pins for the ip-obfuscation transcription.
#
# THE WHATWG READING IS NOT RE-TRANSCRIBED HERE. tools/oracle/derive-ip-
# obfuscation.R already transcribes #concept-host-parser at that revision, and
# it is falsified nine ways. A second transcription of the same algorithm is a
# second copy that drifts, so this file SOURCES it. That reuse is also what
# found a latent defect in it: the IPv4 number parser applied its
# double-precision guard before the digit-validity check, so a 63-character
# NON-numeric label aborted instead of failing cleanly.
#
# NO PACKAGE IS LOADED. The independence from rurl stays structural.

here_derive <- function(...) file.path("tools", "oracle", ...)
source(here_derive("derive-ip-obfuscation.R"), local = FALSE)

# ---- reading 1: the WHATWG default ------------------------------------------

# Accept-or-reject under the URL Standard's own default, for the `http://<host>`
# and `http://<host>/` shapes this group uses. Anything else aborts rather than
# being guessed: every row must be recognisably one of those two, or the
# derivation has been pointed at a corpus it does not model.
dnslen_whatwg_accepts <- function(url) {
  if (!grepl("^http://", url)) {
    stop("NOT MODELED: '", url, "' is not of the form http://<host>[/] -- this ",
         "derivation transcribes host parsing only.", call. = FALSE)
  }
  rest <- substring(url, nchar("http://") + 1L)
  if (endsWith(rest, "/")) {
    rest <- substring(rest, 1L, nchar(rest) - 1L)
  }
  if (grepl("[/?#]", rest)) {
    stop("NOT MODELED: '", url, "' carries a path, query or fragment beyond a ",
         "single trailing slash.", call. = FALSE)
  }
  ipobf_host_parse(rest)$ok
}

# ---- reading 2: RFC 1035 section 2.3.4, which is what Ada checks -------------

# The upstream suite's own message states the two limits it tests:
#
#   "A domain label must be a mininum of 1 character and a maximum of 63
#    characters."
#   "A domain name must be a maximum of 253 characters (or 254 if there is a
#    dot at the end)."
#
# Transcribed so the DISAGREEMENT with upstream can be checked rather than
# enumerated by hand. A hand-written ledger of "these ten rows differ" records
# THAT they differ; deriving both readings shows the difference is exactly the
# DNS-length axis and nothing else -- so if upstream ever rejects a row for
# some other reason, the disagreement stops being explained and the gate fails.
#
# ipobf_strict_split(), not strsplit(): base R drops TRAILING empty fields, so
# "example.com.br.." would come back with its empty label already discarded and
# the row would score as valid. That is the same trailing-field trap recorded
# for the IPv4 parser, and it silently flipped one of these 17 rows the first
# time this was measured.
dnslen_rfc1035_violates <- function(host) {
  trailing_dot <- endsWith(host, ".")
  bare <- if (trailing_dot) substring(host, 1L, nchar(host) - 1L) else host
  labels <- ipobf_strict_split(bare)
  bad_label <- length(labels) == 0L ||
    any(nchar(labels) < 1L) || any(nchar(labels) > 63L)
  limit <- if (trailing_dot) 254L else 253L
  bad_label || nchar(host) > limit
}

dnslen_host_of <- function(url) {
  rest <- substring(url, nchar("http://") + 1L)
  if (endsWith(rest, "/")) substring(rest, 1L, nchar(rest) - 1L) else rest
}
