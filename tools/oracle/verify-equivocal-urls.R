#!/usr/bin/env Rscript
# RURL-ozdejfzl -- prove the 12 `equivocal-urls` rows are the transcription they
# claim to be.
#
# A TRANSCRIPTION-INTEGRITY GATE, NOT A RE-DERIVATION GATE. The expected values
# are the paper's `NodeJS WHATWG` reference column; there is no algorithm to
# re-run, so no gate can establish that the transcription is CORRECT -- only that
# it is intact, byte-exact and faithfully restated. The gate prints that
# limitation itself rather than leaving it to a README.
#
# This is the group where tracking matters most and re-derivation helps least.
# Reynolds et al. released NO artifact: the fuzzing corpus was never published
# and the paper carries no repository link. `youarealiar` at least has a
# third-party repo its bytes were cross-checked against. Here there is nothing.
# Until these rows were tracked they lived in the committed fixture and one
# gitignored builder, and that was the entire world's copy.
#
# Checks:
#   A TRANSCRIPTION INTEGRITY -- primary-source cells only. `notes` is excluded
#     on purpose; see check A.
#   B BYTE EXACTNESS -- the LF in eq-U6 and the U+0130 in eq-U8 are the point of
#     those rows, and eq-U2/eq-bs carry SINGLE backslashes.
#   C RESTATEMENT FIDELITY -- `oracle_kind`/`oracle_value`, and `fsss_host`
#     CONDITIONALLY (see the check).
#   D CITATION INTEGRITY -- every row must trace to a Table-3 pitfall row or a
#     numbered section, since the paper is the only witness there is.
#   E THE OCTET-NOTATION EXCEPTION -- the check this group needs and the others
#     do not. See check E.
#
# Usage:
#   Rscript tools/oracle/verify-equivocal-urls.R
#   Rscript tools/oracle/verify-equivocal-urls.R --self-test

FIXTURE <- "tests/testthat/fixtures/external-url-vectors.csv"
GROUP <- "equivocal-urls"

here <- function(...) file.path("tools", "oracle", ...)
source(here("transcribe-equivocal-urls.R"), local = FALSE)

read_fixture <- function(path) {
  if (!file.exists(path)) {
    stop("FATAL: fixture not found: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = "NA", encoding = "UTF-8")
}

# Only RUNNABLE rows have a byte-exact `input_json`. The two notation rows must
# keep `input = NA`, which check E enforces, so decoding them is neither
# attempted nor meaningful.
recover_inputs <- function(d) {
  out <- rep(NA_character_, nrow(d))
  run <- d$runnable == "yes"
  if (any(run)) {
    out[run] <- vapply(d$input_json[run], jsonlite::fromJSON, character(1),
                       USE.NAMES = FALSE)
  }
  out
}

special_codepoints <- function(s) {
  if (is.na(s) || !nzchar(s)) {
    return(integer(0))
  }
  cp <- utf8ToInt(enc2utf8(s))
  as.integer(cp[cp < 32L | cp > 126L])
}

count_backslashes <- function(s) {
  m <- gregexpr("\\", s, fixed = TRUE)[[1]]
  length(m[m > 0L])
}

# ---- checks -----------------------------------------------------------------

# Check A -- TRANSCRIPTION INTEGRITY, over PRIMARY-SOURCE cells only.
#
# `notes` is deliberately absent, and that boundary was established by
# measurement on `youarealiar` rather than assumed: freezing it there produced
# five disagreements in which the FIXTURE was the more current text every time,
# because `notes` is living commentary about rurl's behavior and is SUPPOSED to
# move when that behavior moves. Freezing it would turn a correct update into a
# gate failure. The citation is the stable part, and check D owns it.
check_transcription <- function(committed, roster) {
  fail <- character(0)
  extra <- setdiff(committed$id, roster$id)
  short <- setdiff(roster$id, committed$id)
  if (length(extra)) {
    fail <- c(fail, sprintf(
      paste0("%d committed row(s) are not in the tracked transcription. This ",
             "paper released NO artifact, so such a row traces to nothing and ",
             "cannot be recovered: %s"),
      length(extra), paste(extra, collapse = ", ")))
  }
  if (length(short)) {
    fail <- c(fail, sprintf(
      paste0("%d transcribed row(s) are no longer carried by the fixture -- ",
             "and there is no artifact to re-import them from: %s"),
      length(short), paste(short, collapse = ", ")))
  }
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx)
  if (!any(ok)) {
    return(c(fail, "no committed row matched the transcription at all"))
  }
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]

  want <- list(
    input = rs$input,
    standard_expectation = rs$expectation,
    paper_claimed_behavior = rs$paper,
    source_reference = vapply(rs$ref, equivocal_urls_source_reference,
                              character(1), USE.NAMES = FALSE)
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
          "    %s\n      fixture      =%s\n      transcription=%s",
          cm$id[i], encodeString(got[i]), encodeString(exp[i])))
      }
    }
  }
  fail
}

# Check B -- BYTE EXACTNESS, runnable rows only.
check_bytes <- function(committed, roster) {
  fail <- character(0)
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx) & committed$runnable == "yes"
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]

  for (i in seq_len(nrow(cm))) {
    got_cp <- special_codepoints(cm$input[i])
    want_cp <- as.integer(rs$cp_special[[i]])
    if (!identical(got_cp, want_cp)) {
      fail <- c(fail, sprintf(
        paste0("%s carries the wrong special code points:\n",
               "      fixture      =[%s]\n      transcription=[%s]"),
        cm$id[i], paste(got_cp, collapse = ","),
        paste(want_cp, collapse = ",")))
    }
    got_bs <- count_backslashes(cm$input[i])
    if (!identical(got_bs, as.integer(rs$n_backslash[i]))) {
      fail <- c(fail, sprintf(
        "%s carries %d backslash(es), the transcription declares %d",
        cm$id[i], got_bs, as.integer(rs$n_backslash[i])))
    }
  }
  fail
}

# Check C -- RESTATEMENT FIDELITY.
#
# `fsss_host` is checked CONDITIONALLY, on `rurl_deviation` being NA. That is
# not a loosening -- it is the correct rule, and asserting it unconditionally
# would be wrong here: eq-U8 records oracle_value `n.xn--prie-swc.gg` (the
# paper's Option A, where the dotted-I folds into the host) while rurl reads the
# `@` as a userinfo delimiter and lands on `e.gg`. That disagreement is the
# POINT of the row, and it is owned by a `rurl_deviation` citation. A gate that
# demanded equality would force either a false oracle or a deleted deviation --
# exactly the co-confirmation trap RURL-nknytzxz was filed for.
check_restatement <- function(committed, roster) {
  fail <- character(0)
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx)
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]

  for (i in seq_len(nrow(cm))) {
    p <- equivocal_urls_expectation_parse(rs$expectation[i],
                                          cm$runnable[i] == "yes")
    if (!identical(cm$oracle_kind[i], p$kind)) {
      fail <- c(fail, sprintf(
        "%s: oracle_kind is %s, but expectation %s (runnable=%s) restates as %s",
        cm$id[i], encodeString(cm$oracle_kind[i]),
        encodeString(rs$expectation[i]), cm$runnable[i], p$kind))
    }
    got <- cm$oracle_value[i]
    same <- (is.na(got) && is.na(p$value)) ||
      (!is.na(got) && !is.na(p$value) && got == p$value)
    if (!same) {
      fail <- c(fail, sprintf(
        "%s: oracle_value is %s, but the expectation restates as %s",
        cm$id[i], encodeString(got), encodeString(p$value)))
    }
    if (identical(p$kind, "host") && is.na(cm$rurl_deviation[i]) &&
          !identical(cm$fsss_host[i], p$value)) {
      fail <- c(fail, sprintf(
        paste0("%s: fsss_host is %s but the transcribed expectation names ",
               "host %s, and no rurl_deviation owns the difference"),
        cm$id[i], encodeString(cm$fsss_host[i]), encodeString(p$value)))
    }
  }
  fail
}

# Check D -- CITATION INTEGRITY. The paper is the only witness, so a row that
# stops naming a Table-3 pitfall row or a numbered section traces to nothing.
check_citations <- function(committed, roster) {
  fail <- character(0)
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx)
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]
  traceable <- grepl("Table 3 U[0-9]+", rs$ref) | grepl("Section [0-9]", rs$ref)
  if (any(!traceable)) {
    fail <- c(fail, sprintf(
      paste0("%d transcribed row(s) cite neither a Table-3 row nor a numbered ",
             "section: %s"), sum(!traceable),
      paste(cm$id[!traceable], collapse = ", ")))
  }
  if (any(committed$source_class != "C")) {
    fail <- c(fail, "a row in this paper-transcribed group is not source_class C")
  }
  claimed <- !is.na(committed$paper_claimed_behavior) &
    nzchar(committed$paper_claimed_behavior)
  if (any(!claimed)) {
    fail <- c(fail, sprintf(
      paste0("%d row(s) carry no paper_claimed_behavior -- the class-C column ",
             "that keeps the paper's claim SEPARATE from the standard's ",
             "requirement (ADR 0006): %s"), sum(!claimed),
      paste(committed$id[!claimed], collapse = ", ")))
  }
  fail
}

# Check E -- THE OCTET-NOTATION EXCEPTION.
#
# eq-U1 (`n.pr[0x00]@e.gg`, an embedded NUL) and eq-U7
# (`n.pr[0xDD9ADCBD]e.gg`, octets that are not valid UTF-8) cannot be
# represented as R character strings. Their `input_json` therefore holds the
# PAPER'S `[0xNN]` notation rather than a JSON-encoded copy of the bytes -- a
# deliberate exception to the fixture's own convention that `input_json` is the
# byte-exact source of truth.
#
# Nothing was checking that the exception stayed an exception. A later pass that
# "normalized" those cells into ordinary escaped strings would convert a faithful
# record of UN-REPRESENTABLE octets into a false claim about representable ones,
# and would make two rows look runnable that cannot be run. So this asserts, in
# both directions: exactly these two rows are notation-only, they keep
# `input = NA`, their `input_json` still carries `[0x..]`, and -- the other
# direction -- no RUNNABLE row's `input_json` contains octet notation, which is
# what would happen if someone recorded a new un-representable input by copying
# the notation without also marking the row non-runnable.
check_notation <- function(committed, roster) {
  fail <- character(0)
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx)
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]
  want_run <- equivocal_urls_runnable(rs)
  got_run <- cm$runnable == "yes"

  wrong <- which(want_run != got_run)
  if (length(wrong)) {
    fail <- c(fail, sprintf(
      paste0("%d row(s) disagree on runnability -- the transcription only ",
             "carries a string for rows that CAN be run: %s"), length(wrong),
      paste(sprintf("%s (fixture=%s, transcription=%s)", cm$id[wrong],
                    cm$runnable[wrong],
                    ifelse(want_run[wrong], "yes", "no")), collapse = "; ")))
  }
  notation <- !want_run
  if (any(notation)) {
    # The notation cell must still be notation.
    bad_json <- which(notation &
                        !grepl("[0x", cm$input_json, fixed = TRUE))
    if (length(bad_json)) {
      fail <- c(fail, sprintf(
        paste0("%d notation-only row(s) no longer carry the paper's [0xNN] ",
               "octet notation in input_json. Normalizing these into ordinary ",
               "escaped strings turns a record of UN-REPRESENTABLE octets into ",
               "a false claim about representable ones: %s"),
        length(bad_json), paste(cm$id[bad_json], collapse = ", ")))
    }
    for (i in which(notation)) {
      if (!identical(cm$input_json[i], rs$notation[i])) {
        fail <- c(fail, sprintf(
          "%s: input_json is %s, the transcription declares %s", cm$id[i],
          encodeString(cm$input_json[i]), encodeString(rs$notation[i])))
      }
      if (!is.na(cm$input[i])) {
        fail <- c(fail, sprintf(
          paste0("%s carries a non-NA `input`, but its bytes cannot be held ",
                 "in an R string at all"), cm$id[i]))
      }
    }
  }
  # The other direction: octet notation inside a row claiming to be runnable.
  smuggled <- which(got_run & grepl("[0x", cm$input_json, fixed = TRUE))
  if (length(smuggled)) {
    fail <- c(fail, sprintf(
      paste0("%d RUNNABLE row(s) carry [0xNN] octet notation in input_json, so ",
             "they claim to be runnable while recording bytes that are not: %s"),
      length(smuggled), paste(cm$id[smuggled], collapse = ", ")))
  }
  fail
}

# ---- self-test --------------------------------------------------------------

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

  expect("LF is special", special_codepoints("a\nb"), 10L)
  expect("U+0130 is special", special_codepoints("İ"), 304L)
  expect("NA input has no code points", special_codepoints(NA_character_),
         integer(0))
  expect("backslash count", count_backslashes("a\\b\\c"), 2L)

  p <- equivocal_urls_expectation_parse
  expect("bare host is a host claim", p("n.pr", TRUE)$kind, "host")
  expect("bare host carries itself", p("n.pr", TRUE)$value, "n.pr")
  expect("failure is a failure claim", p("failure", TRUE)$kind, "failure")
  expect("failure carries no value", p("failure", TRUE)$value, NA_character_)
  # Non-runnability WINS over the expectation text -- eq-U1 records `e.gg` and
  # eq-U7 records `failure`, and neither carries an oracle value.
  expect("non-runnable beats a host expectation", p("e.gg", FALSE)$kind,
         "not-runnable")
  expect("non-runnable beats a failure expectation", p("failure", FALSE)$kind,
         "not-runnable")
  expect("non-runnable carries no value", p("e.gg", FALSE)$value, NA_character_)

  r <- equivocal_urls_roster()
  expect("twelve rows transcribed", nrow(r), 12L)
  expect("ten runnable", sum(equivocal_urls_runnable(r)), 10L)
  expect("ids unique", anyDuplicated(r$id), 0L)
  run <- equivocal_urls_runnable(r)
  self_cp <- vapply(which(run), function(i) {
    identical(special_codepoints(r$input[i]), as.integer(r$cp_special[[i]]))
  }, logical(1))
  expect("roster code points match its own strings", all(self_cp), TRUE)
  self_bs <- vapply(which(run), function(i) {
    identical(count_backslashes(r$input[i]), as.integer(r$n_backslash[i]))
  }, logical(1))
  expect("roster backslash counts match its own strings", all(self_bs), TRUE)
  expect("the two notation rows are U1 and U7", r$id[!run],
         c("eq-U1", "eq-U7"))
  expect("notation rows carry notation",
         all(grepl("[0x", r$notation[!run], fixed = TRUE)), TRUE)
  expect("runnable rows carry no notation", all(is.na(r$notation[run])), TRUE)

  # -- the checks must be able to fail -------------------------------------
  fx <- read_fixture(FIXTURE)
  cm <- fx[!is.na(fx$source) & fx$source == GROUP, , drop = FALSE]
  cm$input <- recover_inputs(cm)
  expect("transcription check clean", length(check_transcription(cm, r)), 0L)
  expect("byte check clean", length(check_bytes(cm, r)), 0L)
  expect("restatement check clean", length(check_restatement(cm, r)), 0L)
  expect("citation check clean", length(check_citations(cm, r)), 0L)
  expect("notation check clean", length(check_notation(cm, r)), 0L)

  b <- cm
  b$input[b$id == "eq-U6"] <- "https://n.pre.gg"
  expect("byte check notices the LF going missing",
         length(check_bytes(b, r)) > 0L, TRUE)
  b <- cm
  b$input[b$id == "eq-U8"] <- "https://n.prI@e.gg"
  expect("byte check notices U+0130 flattened to ASCII I",
         length(check_bytes(b, r)) > 0L, TRUE)
  b <- cm
  b$oracle_value[b$id == "eq-U2"] <- "e.gg"
  expect("restatement check notices the other equivocal option",
         length(check_restatement(b, r)) > 0L, TRUE)
  # The conditional fsss_host rule, in both directions.
  b <- cm
  b$fsss_host[b$id == "eq-U5"] <- "n.pr"
  expect("restatement check notices an undocumented fsss_host difference",
         length(check_restatement(b, r)) > 0L, TRUE)
  b <- cm
  b$rurl_deviation[b$id == "eq-U8"] <- NA_character_
  expect("restatement check notices a DELETED deviation on eq-U8",
         length(check_restatement(b, r)) > 0L, TRUE)

  # -- the notation exception ----------------------------------------------
  b <- cm
  b$input_json[b$id == "eq-U1"] <- "\"https://n.pr@e.gg\""
  expect("notation check notices a normalized octet cell",
         length(check_notation(b, r)) > 0L, TRUE)
  b <- cm
  b$runnable[b$id == "eq-U7"] <- "yes"
  expect("notation check notices a notation row marked runnable",
         length(check_notation(b, r)) > 0L, TRUE)
  b <- cm
  b$input[b$id == "eq-U1"] <- "https://n.pr@e.gg"
  expect("notation check notices a non-NA input on a notation row",
         length(check_notation(b, r)) > 0L, TRUE)
  b <- cm
  b$input_json[b$id == "eq-U2"] <- "\"https://n.pr[0x5C]@e.gg\""
  expect("notation check notices octets smuggled into a runnable row",
         length(check_notation(b, r)) > 0L, TRUE)
  b <- cm
  expect("transcription check notices a dropped row",
         length(check_transcription(b[-1L, , drop = FALSE], r)) > 0L, TRUE)

  cat(sprintf("self-test: %d passed, %d failed\n", pass, length(fail)))
  if (length(fail)) {
    cat(paste0("  - ", fail, collapse = "\n"), "\n", sep = "")
    stop("verify-equivocal-urls self-test: FAIL", call. = FALSE)
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
  roster <- equivocal_urls_roster()

  cat(sprintf("group           : %s (class C, paper transcription)\n", GROUP))
  cat(sprintf("committed rows  : %d (%d runnable, %d notation-only)\n",
              nrow(committed), sum(committed$runnable == "yes"),
              sum(committed$runnable != "yes")))
  cat(sprintf("transcribed rows: %d\n", nrow(roster)))

  fails <- c(
    check_transcription(committed, roster),
    check_bytes(committed, roster),
    check_restatement(committed, roster),
    check_citations(committed, roster),
    check_notation(committed, roster)
  )

  if (length(fails)) {
    cat("\n== failures ==\n")
    cat(paste0("  - ", fails, collapse = "\n"), "\n", sep = "")
    cat("\nTRANSCRIPTION INTEGRITY: FAIL\n")
    quit(status = 1L)
  }
  cat(sprintf("integrity       : %d/%d rows match the tracked transcription\n",
              nrow(committed), nrow(committed)))
  cat(sprintf(
    "byte exactness  : %d special code point(s), %d backslash(es) exact\n",
    length(unlist(roster$cp_special)), sum(roster$n_backslash, na.rm = TRUE)))
  cat("restatement     : oracle_kind/oracle_value restate the transcription;\n")
  cat("                  fsss_host matches except where a rurl_deviation ")
  cat("owns the difference\n")
  cat("citations       : every row traces to a Table-3 row or a section\n")
  cat(paste0("notation        : eq-U1/eq-U7 keep the paper's [0xNN] octet ",
             "notation and stay\n                  non-runnable; no runnable ",
             "row smuggles octet notation\n"))
  cat("TRANSCRIPTION INTEGRITY: PASS\n")
  cat(paste0("NOTE: this gate cannot tell you the transcription is CORRECT. ",
             "The paper is the\n      primary source, it released NO artifact, ",
             "and there is no algorithm to\n      re-run. It proves the ",
             "committed rows are the transcription recorded.\n"))
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--self-test" %in% args) {
    self_test()
  } else {
    main()
  }
}
