#!/usr/bin/env Rscript
# RURL-ozdejfzl -- prove the 24 `ip-obfuscation` rows are re-derivable.
#
# WHAT THIS DISCHARGES. This group carries no `generation_command` at all: it is
# recorded with `section_2_3_applies = false`, because nothing was imported and
# so there is no upstream project, revision, hash or fetch to pin. What stands
# in for that provenance is a prose claim in `oracle-provenance.json`:
#
#     "each row is a decimal/octal/hex re-encoding of an IPv4 literal,
#      derivable from arithmetic alone"   (out_of_scope_reason)
#     "the rows are re-derivable by arithmetic instead"  (relocation_note)
#
# The provenance gate accepts that sentence as written; nothing re-derived
# anything. For a group with no upstream artifact, re-derivability is the ONLY
# evidence there is, which makes it the worst place in the record for an
# untested claim. This turns it into an executable check.
#
# WHY IT LOADS NO PACKAGE. The derivation is a transcription of the WHATWG host
# parser and must stay independent of rurl. `devtools::load_all()` is
# deliberately absent, which makes the independence structural rather than a
# comment.
#
# Usage:
#   Rscript tools/oracle/verify-ip-obfuscation.R
#   Rscript tools/oracle/verify-ip-obfuscation.R --self-test
#
# Exit status 0 = the committed rows re-derive; non-zero = they do not.

FIXTURE <- "tests/testthat/fixtures/external-url-vectors.csv"
GROUP <- "ip-obfuscation"

here <- function(...) file.path("tools", "oracle", ...)
source(here("derive-ip-obfuscation.R"), local = FALSE)

read_fixture <- function(path) {
  if (!file.exists(path)) {
    stop("FATAL: fixture not found: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = "NA", encoding = "UTF-8")
}

# `input_json` is the fixture's source of truth (its own convention: a raw
# `input` cell cannot survive a CSV round trip on every platform). All 24 rows
# here are runnable, so every input is recovered from the JSON spelling.
recover_inputs <- function(d) {
  vapply(d$input_json, jsonlite::fromJSON, character(1), USE.NAMES = FALSE)
}

# ---- checks -----------------------------------------------------------------

# Check A -- ROSTER. The 24 encodings are irreducible data: there is no upstream
# file to re-fetch them from, so the tracked roster is where they live and the
# fixture must carry exactly that set. Order-independent, both directions -- a
# row added to the fixture without being added to the roster is as much a
# provenance break as a row deleted.
check_roster <- function(committed, derived) {
  fail <- character(0)
  extra <- setdiff(committed$input, derived$input)
  short <- setdiff(derived$input, committed$input)
  if (length(extra)) {
    fail <- c(fail, sprintf(
      paste0("%d committed row(s) are not in the tracked roster -- their ",
             "encodings exist nowhere else and are unprovenanced:"),
      length(extra)))
    fail <- c(fail, paste0("    ", encodeString(utils::head(extra, 10L))))
  }
  if (length(short)) {
    fail <- c(fail, sprintf(
      "%d roster encoding(s) are no longer carried by the fixture:",
      length(short)))
    fail <- c(fail, paste0("    ", encodeString(utils::head(short, 10L))))
  }
  if (anyDuplicated(derived$input)) {
    fail <- c(fail, "the roster carries a duplicate encoding")
  }
  fail
}

# Check B -- RE-DERIVATION. Every recorded WHATWG expectation must equal what
# the transcribed host parser computes from the input alone. The group's
# expectation is restated across six columns, and all six are graded: a repair
# applied to one of them and missed on another is precisely the drift a single
# spot-check would not see.
#
# `rfc3986_expected` is deliberately NOT graded here -- it is a different
# oracle, and it already has one: the `oracle-vs-grammar` test in
# test-external-url-vectors.R checks it against a transcription of RFC 3986's
# own ABNF (derivation in tools/oracle-audit-rfc3986.R). Two gates deriving one
# column is how they drift apart.
check_rederivation <- function(committed, derived) {
  fail <- character(0)
  idx <- match(committed$input, derived$input)
  ok <- !is.na(idx)
  if (!any(ok)) {
    return("no committed row matched the roster at all")
  }
  d <- derived[idx[ok], , drop = FALSE]
  cm <- committed[ok, , drop = FALSE]

  # What the WHATWG side of each column must hold, derived rather than quoted.
  want_url <- ifelse(d$kind == "exact", d$href, "failure")
  want <- list(
    standard_expectation = want_url,
    whatwg_expected = want_url,
    oracle_kind = d$kind,
    oracle_value = d$href,
    fsss_whatwg = d$href,
    fsss_host = d$host,
    notes = d$note,
    source_reference = d$source_reference
  )

  for (col in names(want)) {
    got <- cm[[col]]
    exp <- want[[col]]
    bad <- which(!((is.na(got) & is.na(exp)) |
                     (!is.na(got) & !is.na(exp) & got == exp)))
    if (length(bad)) {
      fail <- c(fail, sprintf("%d row(s) disagree on `%s`:", length(bad), col))
      for (i in bad) {
        fail <- c(fail, sprintf(
          "    %s  input=%s\n      recorded=%s\n      derived =%s",
          cm$id[i], encodeString(cm$input[i]), encodeString(got[i]),
          encodeString(exp[i])))
      }
    }
  }

  # The group is a WHATWG-oracle group by construction; a row whose `standard`
  # says otherwise is not one this derivation is entitled to grade.
  wrong_std <- which(cm$standard != "whatwg")
  if (length(wrong_std)) {
    fail <- c(fail, sprintf(
      "%d row(s) do not record standard == 'whatwg': %s", length(wrong_std),
      paste(cm$id[wrong_std], collapse = ", ")))
  }
  fail
}

# Check C -- ARITHMETIC INTENT. The roster states, independently of any
# expectation, which 32-bit address each encoding is MEANT to denote. The spec
# arithmetic must land on that number.
#
# This is the check that is specific to what this group is. Checks A and B would
# both pass if an encoding were mistyped -- `0177` fat-fingered to `0176`
# derives cleanly to 126.0.0.1 and the fixture could be updated to match,
# leaving a row that no longer demonstrates the obfuscation it claims to. The
# declared intent and the derived value are two independent statements, so a
# typo splits them. `ipobf-005` proves the check is not vacuous: it declares
# 2130706688, NOT the 2130706433 its `0x7f` prefix suggests.
check_intent <- function(derived) {
  fail <- character(0)
  declared <- !is.na(derived$denotes)
  coerced <- !is.na(derived$ipv4)

  bad <- which(declared & coerced & derived$denotes != derived$ipv4)
  for (i in bad) {
    fail <- c(fail, sprintf(
      paste0("%s does not denote the address it declares: declared %.0f, ",
             "derived %.0f (%s)"),
      encodeString(derived$input[i]), derived$denotes[i], derived$ipv4[i],
      derived$host[i]))
  }

  # The classification must agree too: a row declaring an address that the
  # parser does not IPv4-coerce, or an IPv4 coercion nobody declared, means the
  # roster and the spec disagree about what kind of row it is.
  undeclared <- which(coerced & !declared)
  if (length(undeclared)) {
    fail <- c(fail, sprintf(
      paste0("%d encoding(s) IPv4-coerce but declare no intended address: %s"),
      length(undeclared),
      paste(encodeString(derived$input[undeclared]), collapse = ", ")))
  }
  uncoerced <- which(declared & !coerced)
  if (length(uncoerced)) {
    fail <- c(fail, sprintf(
      "%d encoding(s) declare an address but do not IPv4-coerce: %s",
      length(uncoerced),
      paste(encodeString(derived$input[uncoerced]), collapse = ", ")))
  }
  fail
}

# ---- self-test --------------------------------------------------------------
#
# Positive and negative coverage of the transcribed algorithms, over synthetic
# inputs rather than the corpus this gate grades -- so a broken transcription is
# caught by this file, not by silently agreeing with a fixture cell.
#
# The negatives are chosen where a plausible implementation goes wrong: the
# radix-prefix edge cases, the two DIFFERENT overflow rules (per-octet vs
# whole-address), the trailing-dot split that base `strsplit()` mangles, the
# zero-run compression tie-break, and the fail-closed guards that must abort
# rather than report "failure".
self_test <- function() {
  pass <- 0L
  fail <- character(0)
  expect <- function(label, got, want) {
    if (identical(got, want)) {
      pass <<- pass + 1L
    } else {
      fail <<- c(fail, sprintf("%s: got %s, want %s", label,
                               encodeString(as.character(got)),
                               encodeString(as.character(want))))
    }
  }
  num <- function(s) {
    r <- ipobf_ipv4_number_parser(s)
    if (r$ok) r$value else NA_real_
  }
  v4 <- function(s) {
    r <- ipobf_ipv4_parser(s)
    if (r$ok) r$value else NA_real_
  }
  v6 <- function(s) {
    r <- ipobf_ipv6_parser(s)
    if (r$ok) ipobf_ipv6_serialize(r$address) else NA_character_
  }
  host <- function(s) {
    r <- ipobf_host_parse(s)
    if (r$ok) r$host else NA_character_
  }
  errs <- function(expr) {
    inherits(try(expr, silent = TRUE), "try-error")
  }

  # -- IPv4 number parser: radix prefixes and their edges -------------------
  expect("hex prefix", num("0x7f"), 127)
  expect("octal prefix", num("0177"), 127)
  expect("bare decimal", num("255"), 255)
  # A lone "0" is one code point, so no prefix is stripped; "0x" strips to
  # nothing and is the number zero rather than a failure.
  expect("lone zero", num("0"), 0)
  expect("empty after 0x prefix", num("0x"), 0)
  expect("empty part fails", num(""), NA_real_)
  expect("8 is not an octal digit", num("08"), NA_real_)
  expect("g is not a hex digit", num("0xg"), NA_real_)
  expect("hex digit needs the prefix", num("1a"), NA_real_)
  expect("uppercase hex accepted", num("0XFF"), 255)
  # The value strtoi() cannot represent; it must be a number here, not NA.
  expect("0xffffffff is a value", num("0xffffffff"), 4294967295)

  # -- IPv4 parser: the two different overflow rules ------------------------
  expect("dotted quad", v4("1.2.3.4"), 16909060)
  expect("2-part fills low 3 octets", v4("127.1"), 2130706433)
  expect("3-part fills low 2 octets", v4("127.0.1"), 2130706433)
  expect("2-part spread", v4("0x7f.256"), 2130706688)
  expect("whole 32 bits in one part", v4("4294967295"), 4294967295)
  expect("one part above 2^32-1 fails", v4("4294967296"), NA_real_)
  expect("five parts fail", v4("1.2.3.4.5"), NA_real_)
  expect("non-last octet above 255 fails", v4("256.1.1.1"), NA_real_)
  # Step 6, not step 7: the address would fit, the first OCTET does not.
  expect("first part overflows its octet", v4("0xffffffff.0"), NA_real_)
  expect("empty interior part fails", v4("1..2"), NA_real_)
  # Trailing dot: base strsplit() drops the empty field, so a naive split never
  # reaches the drop-one-trailing-empty rule and mis-counts the parts.
  expect("trailing dot dropped", v4("127.0.0.1."), 2130706433)
  expect("two trailing dots fail", v4("127.0.0.1.."), NA_real_)

  # -- ends in a number ------------------------------------------------------
  expect("digits end", ipobf_ends_in_number("127.0.0.1"), TRUE)
  expect("hex last part ends", ipobf_ends_in_number("a.0x7f"), TRUE)
  expect("name does not end", ipobf_ends_in_number("example.com"), FALSE)
  expect("trailing dot still ends",
         ipobf_ends_in_number("127.0.0.1."), TRUE)

  # -- IPv6 parse + serialize round trips -----------------------------------
  expect("loopback", v6("::1"), "::1")
  expect("all zeroes", v6("::"), "::")
  expect("trailing compress", v6("1::"), "1::")
  expect("full form", v6("1:2:3:4:5:6:7:8"), "1:2:3:4:5:6:7:8")
  expect("ipv4-mapped", v6("::ffff:127.0.0.1"), "::ffff:7f00:1")
  expect("ipv4-compatible", v6("::127.0.0.1"), "::7f00:1")
  # Compression takes the FIRST LONGEST run and only a run longer than one, so
  # the leading pair stays written out and the interior four collapse.
  expect("first longest run wins", v6("0:0:1:0:0:0:0:1"), "0:0:1::1")
  expect("lone zero not compressed", v6("1:0:1:1:1:1:1:1"),
         "1:0:1:1:1:1:1:1")
  expect("single leading colon fails", v6(":1"), NA_character_)
  expect("seven pieces without compress fails", v6("1:2:3:4:5:6:7"),
         NA_character_)
  expect("nine pieces fail", v6("1:2:3:4:5:6:7:8:9"), NA_character_)
  expect("two compressions fail", v6("1::2::3"), NA_character_)
  # The row-024 rejection: after the fourth octet a "%" is neither "." nor EOF.
  expect("zone-id percent fails", v6("::ffff:127.0.0.1%25"), NA_character_)
  expect("three octets fail", v6("::ffff:1.2.3"), NA_character_)
  expect("octet above 255 fails", v6("::ffff:1.2.3.256"), NA_character_)
  # Leading zeroes are fatal in the IPv6-embedded IPv4, unlike in the IPv4
  # parser where "01" is octal.
  expect("embedded leading zero fails", v6("::01.2.3.4"), NA_character_)

  # -- host parser -----------------------------------------------------------
  expect("dotted quad host", host("127.0.0.1"), "127.0.0.1")
  expect("coerced host", host("0x7f.1"), "127.0.0.1")
  expect("bracketed host", host("[::1]"), "[::1]")
  expect("unterminated bracket fails", host("[::1"), NA_character_)
  expect("empty host fails", host(""), NA_character_)
  expect("plain domain passes through", host("example.com"), "example.com")
  expect("uppercase domain lowercased", host("EXAMPLE.com"), "example.com")
  # Forbidden domain code point.
  expect("space in host fails", host("a b"), NA_character_)
  # The three UTS-46 full-stop variants map to "." and then coerce.
  expect("ideographic full stop", host("127。0。0。1"),
         "127.0.0.1")
  expect("fullwidth full stop", host("127．0．0．1"), "127.0.0.1")
  expect("halfwidth full stop", host("127｡0｡0｡1"), "127.0.0.1")

  # -- fail closed: unmodeled input must ABORT, never report "failure" ------
  # Reporting failure for something unmodeled would agree with the four
  # must-fail rows for the wrong reason.
  expect("other non-ASCII aborts", errs(ipobf_host_parse("café.com")),
         TRUE)
  expect("percent in host aborts", errs(ipobf_host_parse("1%32.0.0.1")), TRUE)
  expect("non-http shape aborts",
         errs(ipobf_split_host("https://127.0.0.1/")), TRUE)
  expect("path beyond / aborts",
         errs(ipobf_split_host("http://127.0.0.1/a")), TRUE)
  expect("inexact radix value aborts",
         errs(ipobf_ipv4_number_parser(strrep("f", 20L))), TRUE)
  expect("empty roster aborts",
         errs(derive_ip_obfuscation(ip_obfuscation_roster()[0L, ])), TRUE)

  # -- the checks themselves must be able to fail --------------------------
  # A gate whose comparison logic is broken passes everything. Drive each check
  # with a deliberately wrong derivation and confirm it objects.
  d <- derive_ip_obfuscation()
  expect("roster check clean on itself", length(check_roster(
    data.frame(input = d$input, stringsAsFactors = FALSE), d)), 0L)
  expect("roster check notices a missing row", length(check_roster(
    data.frame(input = d$input[-1L], stringsAsFactors = FALSE), d)) > 0L, TRUE)
  bent <- d
  bent$denotes[1L] <- bent$denotes[1L] + 1
  expect("intent check notices a wrong address",
         length(check_intent(bent)) > 0L, TRUE)
  bent2 <- d
  bent2$ipv4[1L] <- NA_real_
  expect("intent check notices a lost coercion",
         length(check_intent(bent2)) > 0L, TRUE)
  expect("intent check clean on itself", length(check_intent(d)), 0L)

  cat(sprintf("self-test: %d passed, %d failed\n", pass, length(fail)))
  if (length(fail)) {
    cat(paste0("  - ", fail, collapse = "\n"), "\n", sep = "")
    stop("verify-ip-obfuscation self-test: FAIL", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- main -------------------------------------------------------------------

main <- function() {
  fixture <- read_fixture(FIXTURE)
  committed <- fixture[!is.na(fixture$source) & fixture$source == GROUP, ,
                       drop = FALSE]
  if (!nrow(committed)) {
    stop("FATAL: no rows with source == '", GROUP, "' in ", FIXTURE,
         " -- this gate grades a group that is not there.", call. = FALSE)
  }
  committed$input <- recover_inputs(committed)

  derived <- derive_ip_obfuscation()

  cat(sprintf("group          : %s\n", GROUP))
  cat(sprintf("committed rows : %d\n", nrow(committed)))
  cat(sprintf("roster rows    : %d (%d accept, %d must-fail)\n", nrow(derived),
              sum(derived$kind == "exact"), sum(derived$kind == "failure")))

  fails <- c(
    check_roster(committed, derived),
    check_rederivation(committed, derived),
    check_intent(derived)
  )

  if (length(fails)) {
    cat("\n== failures ==\n")
    cat(paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
    cat("\nORACLE RE-DERIVATION: FAIL\n")
    quit(status = 1L)
  }
  cat(sprintf("roster         : %d/%d encodings accounted for in both ",
              nrow(committed), nrow(derived)))
  cat("directions\n")
  cat(sprintf(
    "re-derivation  : %d/%d expectations re-derive by the WHATWG host parser\n",
    nrow(committed), nrow(committed)))
  cat(sprintf("intent         : %d/%d encodings denote the address they ",
              sum(!is.na(derived$denotes)), sum(!is.na(derived$denotes))))
  cat("declare\n")
  cat("ORACLE RE-DERIVATION: PASS\n")
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
  } else {
    main()
  }
}
