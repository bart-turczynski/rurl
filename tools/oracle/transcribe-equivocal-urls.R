#!/usr/bin/env Rscript
# RURL-ozdejfzl -- the `equivocal-urls` transcription, as tracked code.
#
# Ported from `_scratch/build-equivocal-vectors.R` (RURL-dbazixkr slice 6b).
#
# THE MOST IRRECOVERABLE GROUP IN THE FIXTURE, which is why it is worth tracking
# even though no gate can re-derive it. Reynolds et al., "Equivocal URLs"
# (ESORICS 2022) released NO artifact -- the 98,425-case fuzzing corpus was never
# published, and the paper carries no repository or data link across its 60
# references. So unlike `youarealiar`, there is not even a third-party repo to
# byte-check against. These 12 rows were hand-transcribed from the paper's PDF
# TEXT LAYER, and until this file they existed in exactly two places: the
# committed fixture, and a gitignored builder on one machine.
#
# NOT A DERIVATION -- see `transcribe-youarealiar.R` for the full argument. The
# expected values are the paper's `NodeJS WHATWG` reference column. There is no
# algorithm to re-run, so `verify-equivocal-urls.R` reports TRANSCRIPTION
# INTEGRITY and says in its own output that it cannot establish correctness.
#
# `notes` is deliberately NOT carried here. That boundary was established by
# measurement on `youarealiar`: freezing `notes` produced five disagreements in
# which the FIXTURE was the more current text every time, because it is living
# commentary about rurl's behavior rather than primary-source data. The stable
# part of a citation lives in `source_reference`, which IS carried.
#
# THE OCTET-NOTATION EXCEPTION. Two Table-3 rows cannot be represented as R
# character strings at all, and they are the reason this group needs a check the
# other four did not:
#
#   * U1 `n.pr[0x00]@e.gg`     -- an embedded NUL; R strings cannot hold 0x00.
#   * U7 `n.pr[0xDD9ADCBD]e.gg` -- raw octets DD 9A DC BD are not valid UTF-8.
#
# Both are recorded `runnable = no` with `input = NA`, and their `input_json`
# cell holds the PAPER'S `[0xNN]` NOTATION rather than a JSON-encoded copy of the
# bytes. That is a deliberate, documented exception to the fixture's own
# convention that `input_json` is the byte-exact source of truth -- and nothing
# was checking that the exception stayed an exception. A later pass that
# "normalized" those cells into ordinary escaped strings would silently turn a
# faithful record of un-representable octets into a false claim about
# representable ones, and would also make them look runnable.
#
# The VirusTotal example `http://letsencrypt.org%2Fdocs%2F[redacted]/LS.exe` is
# DEFERRED and deliberately absent: `[redacted]` is the authors' own redaction of
# the live host, so there is no faithful string to transcribe. Fabricating a
# plausible host would be the one unrecoverable error here.

EQ_LF <- "\n"
EQ_IDOT <- "İ" # U+0130 LATIN CAPITAL I WITH DOT ABOVE

EQ_SOURCE_PREFIX <- "Reynolds et al., 'Equivocal URLs', ESORICS 2022, "
EQ_SOURCE_SUFFIX <-
  " (no artifact released; hand-transcribed from the paper PDF text layer)"

# The transcribed rows.
#
# `runnable = FALSE` rows carry `input = NA` and an `input_json` in the paper's
# octet notation; runnable rows carry the string and declare its special code
# points (everything outside printable ASCII, in order) and backslash count.
equivocal_urls_roster <- function() {
  row <- function(id, input, notation, cp_special, n_backslash, expectation,
                  paper, ref) {
    data.frame(id = id, input = input, notation = notation,
               n_backslash = n_backslash, expectation = expectation,
               paper = paper, ref = ref, stringsAsFactors = FALSE,
               cp_special = I(list(cp_special)))
  }
  NO_INPUT <- NA_character_
  NO_NOTATION <- NA_character_
  GSB <- "malware.testing.google.test"

  rbind(
    # ---- Table 3: eight equivocal URLs (Section 5 pitfalls) ---------------
    row("eq-U1", NO_INPUT, "\"https://n.pr[0x00]@e.gg\"", NULL, NA_integer_,
        "e.gg", "A=e.gg|B=n.pr; WHATWG-ref=A, RFC-ref=ERR",
        "Pitfall 1 (Null Bytes), Table 3 U1"),

    row("eq-U2", "https://n.pr\\@e.gg", NO_NOTATION, integer(0), 1L, "n.pr",
        "A=e.gg|B=n.pr; WHATWG-ref=B, RFC-ref=ERR; browsers=B",
        "Pitfall 2 (Backslash Correction), Table 3 U2"),

    row("eq-U3", "https://n.pr][e.gg", NO_NOTATION, integer(0), 0L, "failure",
        "A=e.gg|B=n.pr; WHATWG-ref=ERR, RFC-ref=ERR",
        "Pitfall 4 (IPv6+ Address Syntax), Table 3 U3"),

    row("eq-U4", "https://n.pr#@e.gg", NO_NOTATION, integer(0), 0L, "n.pr",
        "A=n.pr|B=e.gg; WHATWG-ref=A, RFC-ref=ERR; browsers=A",
        "Pitfall 7 (Illegal Extra Delimiters), Table 3 U4"),

    # The paper writes "RFC-ref=err" in lower case for U5 alone. Transcribed as
    # printed: normalizing it would be an edit to primary-source data.
    row("eq-U5", "https://n.pr%2ee.gg", NO_NOTATION, integer(0), 0L,
        "n.pr.e.gg", "A=n.pr.e.gg|B=n.pr; WHATWG-ref=A, RFC-ref=err; browsers=A",
        "Pitfall 3 (Overeager Percent Decoding), Table 3 U5"),

    row("eq-U6", paste0("https://n.pr", EQ_LF, "e.gg"), NO_NOTATION, 10L, 0L,
        "n.pre.gg", "A=n.pre.gg|B=n.pr; WHATWG-ref=A, RFC-ref=ERR; browsers=A",
        "Pitfall 6 (Low ASCII Bytes), Table 3 U6"),

    # standard_expectation is `failure` because the NodeJS-WHATWG reference
    # column threw ERR. Options A/B are what OTHER parsers produced and stay in
    # `paper_claimed_behavior` -- the ADR 0006 separation.
    row("eq-U7", NO_INPUT, "\"https://n.pr[0xDD9ADCBD]e.gg\"", NULL,
        NA_integer_, "failure",
        paste0("A=n.xn--pre-hwf8l.gg|B=n.xn--pre-bda9o3gf.gg; ",
               "WHATWG-ref=ERR, RFC-ref=ERR"),
        "Pitfall 5 (Automatic Punycode Conversion), Table 3 U7"),

    row("eq-U8", paste0("https://n.pr", EQ_IDOT, "@e.gg"), NO_NOTATION, 304L,
        0L, "n.xn--prie-swc.gg",
        paste0("A=n.xn--prie-swc.gg|B=n.xn--pre-tfa3h.gg|C=n.prie.gg|",
               "D=n.xn--pre-tfa3x.gg; WHATWG-ref=A, RFC-ref=ERR"),
        "Pitfall 5 (Automatic Punycode Conversion), Table 3 U8"),

    # ---- Section 6: misdirection attacks on URL classifiers --------------
    row("eq-gsb", paste0("https://", GSB, "/testing/malware/*"), NO_NOTATION,
        integer(0), 0L, GSB,
        paste0("Section 6.2: GSB's always-flagged-malware test vector (the ",
               "benign/known-bad baseline for the evasion PoCs)"),
        "Section 6.2 (GSB test vector)"),

    row("eq-pe1", paste0("http://letsencrypt.org%2F@", GSB,
                         "/testing/malware/*"), NO_NOTATION, integer(0), 0L,
        GSB,
        paste0("Section 6.2: GSB API+web over-decode %2F->/ -> see ",
               "letsencrypt.org as host -> report CLEAN, while browsers fetch ",
               GSB, " (false negative)"),
        "Section 6.2 (GSB API/web evasion via Pitfall 3)"),

    row("eq-pe2", paste0("http://letsencrypt.org%5C@", GSB,
                         "/testing/malware/*"), NO_NOTATION, integer(0), 0L,
        GSB,
        paste0("Section 6.2: GSB API decodes %5C->\\ then backslash-corrects ",
               "-> see letsencrypt.org -> report CLEAN, while browsers fetch ",
               GSB),
        "Section 6.2 (GSB API evasion via backslash correction)"),

    row("eq-bs", paste0("https://", GSB,
                        "\\testing\\malware\\*@letsencrypt.org"), NO_NOTATION,
        integer(0), 3L, GSB,
        paste0("Section 6.2: GSB WEB interface (no backslash correction) ",
               "evaluates letsencrypt.org -> CLEAN, while a browser ",
               "(backslash correction) fetches ", GSB),
        "Section 6.2 (GSB web-interface evasion via backslash correction)"))
}

# The transcribed expectation -> machine restatement mapping.
#
# This group's `standard_expectation` is a BARE HOST rather than the `host=X`
# prose `youarealiar` uses, so the mapping is different -- which is itself worth
# stating, because assuming one shape across both tier-3 groups is the kind of
# assumption that produces a gate that passes for the wrong reason.
#
#   non-runnable      -> "not-runnable", no value. Takes precedence over the
#                        expectation text: eq-U1 records the reference outcome
#                        `e.gg` and eq-U7 records `failure`, but neither can be
#                        run, so neither carries an oracle value.
#   expectation=failure -> "failure", no value.
#   otherwise         -> "host", carrying the host verbatim.
equivocal_urls_expectation_parse <- function(expectation, runnable) {
  if (!runnable) {
    return(list(kind = "not-runnable", value = NA_character_))
  }
  if (identical(expectation, "failure")) {
    return(list(kind = "failure", value = NA_character_))
  }
  list(kind = "host", value = expectation)
}

equivocal_urls_source_reference <- function(ref) {
  paste0(EQ_SOURCE_PREFIX, ref, EQ_SOURCE_SUFFIX)
}

# A row is runnable exactly when the transcription carries a string for it. The
# two that carry notation instead are the un-representable ones.
equivocal_urls_runnable <- function(roster) {
  !is.na(roster$input)
}

if (sys.nframe() == 0L) {
  r <- equivocal_urls_roster()
  run <- equivocal_urls_runnable(r)
  cat("transcribed rows:", nrow(r), sprintf("(%d runnable, %d notation-only)",
                                            sum(run), sum(!run)), "\n")
  for (i in seq_len(nrow(r))) {
    p <- equivocal_urls_expectation_parse(r$expectation[i], run[i])
    cat(sprintf("  %-7s %-13s %s\n", r$id[i], p$kind,
                if (run[i]) {
                  sprintf("special=[%s] backslashes=%d",
                          paste(r$cp_special[[i]], collapse = ","),
                          r$n_backslash[i])
                } else {
                  sprintf("notation %s", r$notation[i])
                }))
  }
}
