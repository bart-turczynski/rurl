#!/usr/bin/env Rscript
# RURL-ozdejfzl -- prove the 9 `youarealiar` rows are the transcription they
# claim to be.
#
# THIS IS A TRANSCRIPTION-INTEGRITY GATE, NOT A RE-DERIVATION GATE, and the
# distinction is the whole point rather than pedantry. For the two tier-1 groups
# the expected values are computable, so their gates re-derive them and can
# disagree with the fixture on the merits. Here the expected values come from a
# paper's reference-implementation column. There is no algorithm to re-run. The
# transcription IS the primary source, so no gate can ever tell you the
# transcription is RIGHT -- only that it is intact, exact, and faithfully
# restated. Claiming more would be the more dangerous error, because a gate
# labelled "re-derivation" invites the reader to assume an independent check
# happened.
#
# What it does check, and why each one can actually rot:
#
#   A TRANSCRIPTION INTEGRITY -- the transcribed data lived only in gitignored
#     `_scratch/`. It is tracked now, and the roster's source of truth is the
#     BUILDER, not the fixture, so a disagreement means the two have drifted.
#     That is a live possibility: the fixture has been through RURL-yeikpnan's
#     surface re-baseline and RURL-nknytzxz's oracle repair since.
#   B BYTE EXACTNESS -- three rows exist BECAUSE the paper's PDF escaping is
#     ambiguous, and a human resolved it. yal-003's CR bytes provably do not
#     survive a CSV round trip, so `input_json` is the only faithful carrier and
#     nothing was checking that it stayed faithful.
#   C RESTATEMENT FIDELITY -- `oracle_kind`/`oracle_value` are a later
#     machine-readable restatement of the prose expectation. A restatement can
#     drift from what it restates.
#   D CITATION INTEGRITY -- a class-C row's only traceability is its citation. A
#     row that stops naming a paper section stops being traceable to anything.
#   E THE BYTE-VERIFICATION CLAIM -- reported, and deliberately NOT a failure.
#     See the note on that check.
#
# Usage:
#   Rscript tools/oracle/verify-youarealiar.R
#   Rscript tools/oracle/verify-youarealiar.R --self-test
#
# Exit status 0 = the committed rows match the tracked transcription.

FIXTURE <- "tests/testthat/fixtures/external-url-vectors.csv"
PROVENANCE <- "tests/testthat/fixtures/oracle-provenance.json"
GROUP <- "youarealiar"
SENTINEL <- "MISSING[RURL-vwurxmzm]"

here <- function(...) file.path("tools", "oracle", ...)
source(here("transcribe-youarealiar.R"), local = FALSE)

read_fixture <- function(path) {
  if (!file.exists(path)) {
    stop("FATAL: fixture not found: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                  na.strings = "NA", encoding = "UTF-8")
}

# `input_json` is the fixture's declared source of truth, and for this group
# that is load-bearing rather than a formality: yal-003's stored `input` cell has
# lost its CR bytes to CSV normalization, exactly as the transcription warned.
recover_inputs <- function(d) {
  vapply(d$input_json, jsonlite::fromJSON, character(1), USE.NAMES = FALSE)
}

# Every code point outside printable ASCII, in order. This is the shape the
# transcription's ambiguity resolutions are declared in.
special_codepoints <- function(s) {
  if (!nzchar(s)) {
    return(integer(0))
  }
  cp <- utf8ToInt(enc2utf8(s))
  as.integer(cp[cp < 32L | cp > 126L])
}

count_backslashes <- function(s) {
  length(gregexpr("\\", s, fixed = TRUE)[[1]][
    gregexpr("\\", s, fixed = TRUE)[[1]] > 0L])
}

# ---- checks -----------------------------------------------------------------

# Check A -- TRANSCRIPTION INTEGRITY, over PRIMARY-SOURCE cells only.
#
# `notes` is deliberately absent. A class-C row mixes primary-source data with
# living commentary about rurl, and only the former is immutable. Measured:
# freezing `notes` against the builder produced five disagreements and the
# FIXTURE was the more current text in every one, because rurl's behavior moved
# (RURL-qrfrvmkg, RURL-xfbzkico/kmkyicpt, the closed scheme set gaining `file`,
# scheme_policy arriving). Freezing it would turn a correct update into a gate
# failure and pressure the next author into reverting a true statement. The
# stable part of `notes` is its paper citation, and check D owns that.
check_transcription <- function(committed, roster) {
  fail <- character(0)
  extra <- setdiff(committed$id, roster$id)
  short <- setdiff(roster$id, committed$id)
  if (length(extra)) {
    fail <- c(fail, sprintf(
      paste0("%d committed row(s) are not in the tracked transcription -- ",
             "unprovenanced, and this paper released no artifact to recover ",
             "them from: %s"), length(extra), paste(extra, collapse = ", ")))
  }
  if (length(short)) {
    fail <- c(fail, sprintf(
      "%d transcribed row(s) are no longer carried by the fixture: %s",
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
    source_reference = vapply(rs$pitfall, youarealiar_source_reference,
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
          "    %s\n      fixture     =%s\n      transcription=%s",
          cm$id[i], encodeString(got[i]), encodeString(exp[i])))
      }
    }
  }
  fail
}

# Check B -- BYTE EXACTNESS. The three ambiguity resolutions, asserted.
check_bytes <- function(committed, roster) {
  fail <- character(0)
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx)
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]

  for (i in seq_len(nrow(cm))) {
    got_cp <- special_codepoints(cm$input[i])
    want_cp <- as.integer(rs$cp_special[[i]])
    if (!identical(got_cp, want_cp)) {
      fail <- c(fail, sprintf(
        paste0("%s carries the wrong special code points -- the paper's ",
               "escaping is ambiguous here and this is the resolution:\n",
               "      fixture      =[%s]\n      transcription=[%s]"),
        cm$id[i], paste(got_cp, collapse = ","),
        paste(want_cp, collapse = ",")))
    }
    got_bs <- count_backslashes(cm$input[i])
    if (!identical(got_bs, as.integer(rs$n_backslash[i]))) {
      fail <- c(fail, sprintf(
        paste0("%s carries %d backslash(es), the transcription declares %d ",
               "-- a doubled backslash is what transcribing the DISPLAY form ",
               "instead of the decoded bytes produces"),
        cm$id[i], got_bs, as.integer(rs$n_backslash[i])))
    }
  }
  fail
}

# Check C -- RESTATEMENT FIDELITY.
check_restatement <- function(committed, roster) {
  fail <- character(0)
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx)
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]

  for (i in seq_len(nrow(cm))) {
    p <- youarealiar_expectation_parse(rs$expectation[i])
    if (!identical(cm$oracle_kind[i], p$kind)) {
      fail <- c(fail, sprintf(
        "%s: oracle_kind is %s, but the expectation %s restates as %s",
        cm$id[i], encodeString(cm$oracle_kind[i]),
        encodeString(rs$expectation[i]), p$kind))
    }
    got <- cm$oracle_value[i]
    same <- (is.na(got) && is.na(p$value)) ||
      (!is.na(got) && !is.na(p$value) && got == p$value)
    if (!same) {
      fail <- c(fail, sprintf(
        "%s: oracle_value is %s, but the expectation restates as %s",
        cm$id[i], encodeString(got), encodeString(p$value)))
    }
    # Where the expectation names a host, the FSSS host column must be that
    # host -- UNLESS a `rurl_deviation` owns the difference. That condition is
    # the correct rule rather than a loosening, and it is a latent trap without
    # it: yal-005 is a host row that DOES carry a deviation (ADR 0002 -- rurl
    # keeps the host reversibly Unicode and Punycode is a separate presentation
    # phase) and happens to satisfy the equality anyway. An unconditional check
    # therefore passes today by luck, and would fail wrongly the moment the
    # documented presentation phase changed -- forcing either a false oracle or
    # a deleted deviation, which is the co-confirmation trap RURL-nknytzxz was
    # filed for.
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

# Check D -- CITATION INTEGRITY. A class-C row's traceability is its citation
# and nothing else; the paper's own tables are in Section V and Section VI.
check_citations <- function(committed, roster) {
  fail <- character(0)
  idx <- match(committed$id, roster$id)
  ok <- !is.na(idx)
  cm <- committed[ok, , drop = FALSE]
  rs <- roster[idx[ok], , drop = FALSE]
  # The citation is the stable part of an otherwise-mutable `notes` cell, so it
  # is asserted against the section the transcription declares -- not merely
  # pattern-matched, which would accept a row citing the WRONG section.
  want <- sprintf("Section %s", rs$section)
  cited <- mapply(grepl, want, cm$notes, MoreArgs = list(fixed = TRUE),
                  USE.NAMES = FALSE)
  if (any(!cited)) {
    fail <- c(fail, sprintf(
      paste0("%d row(s) do not cite the paper section the transcription ",
             "declares, so they trace to nothing or to the wrong place:"),
      sum(!cited)))
    for (i in which(!cited)) {
      fail <- c(fail, sprintf("    %s  expected to cite %s; notes = %s",
                              cm$id[i], want[i],
                              encodeString(substr(cm$notes[i], 1L, 60L))))
    }
  }
  committed <- cm
  claimed <- !is.na(committed$paper_claimed_behavior) &
    nzchar(committed$paper_claimed_behavior)
  if (any(!claimed)) {
    fail <- c(fail, sprintf(
      paste0("%d row(s) carry no paper_claimed_behavior -- the class-C column ",
             "that keeps the paper's claim SEPARATE from the standard's ",
             "requirement (ADR 0006): %s"), sum(!claimed),
      paste(committed$id[!claimed], collapse = ", ")))
  }
  if (any(committed$source_class != "C")) {
    fail <- c(fail, "a row in this paper-transcribed group is not source_class C")
  }
  fail
}

# Check E -- THE BYTE-VERIFICATION CLAIM, reported rather than enforced.
#
# The transcription's `source_reference` asserts "bytes verified vs
# wspr-ncsu/urlparsing-framework", and the provenance record pins a revision for
# that cross-check. It cannot be re-run here, and the reason is worth surfacing
# rather than hiding behind an offline skip: `upstream_path`,
# `raw_source_sha256` and `import_command` are all MISSING sentinels, so nobody
# recorded WHICH file in that repository the bytes were checked against. The
# claim is therefore not reproducible even with network access -- an offline
# skip would misreport a recording gap as a connectivity problem.
#
# This REPORTS instead of failing, deliberately. Failing on a pre-existing,
# already-tracked recording gap would make the gate permanently red, and a
# permanently red gate stops being read. But it must not be silent either, so
# the state is printed every run.
#
# It DOES fail in one direction: if the sentinels are ever replaced with a real
# path and hash, the claim becomes re-runnable and this gate must stop saying it
# is not. Per the tier-2 rule it then has to abort with the exact fetch command
# rather than quietly continue to pass.
check_byteverification <- function(prov_path = PROVENANCE) {
  if (!file.exists(prov_path)) {
    return(list(fail = sprintf("FATAL: %s not found", prov_path), note = NULL))
  }
  j <- jsonlite::fromJSON(prov_path, simplifyVector = FALSE)
  grp <- NULL
  for (fx in j$fixtures) {
    for (g in fx$source_groups) {
      if (identical(g$group, GROUP)) {
        grp <- g
      }
    }
  }
  if (is.null(grp)) {
    return(list(fail = sprintf(
      "FATAL: no '%s' group in %s; the gate cannot report its provenance",
      GROUP, prov_path), note = NULL))
  }
  missing <- c("upstream_path", "raw_source_sha256", "import_command")
  still_missing <- vapply(missing, function(k) identical(grp[[k]], SENTINEL),
                          logical(1))
  rev <- grp$upstream_revision
  if (all(still_missing)) {
    return(list(fail = character(0), note = sprintf(
      paste0("byte-verify     : NOT re-runnable, and not because we are ",
             "offline.\n",
             "                  The claim cites %s @ %s, but upstream_path, ",
             "raw_source_sha256\n",
             "                  and import_command are all %s -- nobody ",
             "recorded WHICH file\n",
             "                  the bytes were checked against. Tracked on ",
             "RURL-vwurxmzm."),
      grp$upstream_project, substr(rev, 1L, 12L), SENTINEL)))
  }
  list(fail = sprintf(
    paste0("the byte-verification claim is now re-runnable (%s no longer a ",
           "sentinel), so this gate must verify it rather than report it as ",
           "un-runnable. Fetch the pinned bytes and compare:\n",
           "    curl -fsSL https://raw.githubusercontent.com/%s/%s/%s\n",
           "  then check sha256 == %s. Update this gate to enforce it."),
    paste(missing[!still_missing], collapse = "/"), grp$upstream_project, rev,
    if (identical(grp$upstream_path, SENTINEL)) "<path>" else grp$upstream_path,
    if (identical(grp$raw_source_sha256, SENTINEL)) {
      "<hash>"
    } else {
      grp$raw_source_sha256
    }),
    note = NULL)
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

  # -- the byte helpers -----------------------------------------------------
  expect("no special code points", special_codepoints("http://a.b/"),
         integer(0))
  expect("tab is special", special_codepoints("a\tb"), 9L)
  expect("CR LF in order", special_codepoints("a\r\nb"), c(13L, 10L))
  expect("non-ASCII is special", special_codepoints("aヒb"), 12498L)
  expect("empty string", special_codepoints(""), integer(0))
  expect("no backslash", count_backslashes("a/b"), 0L)
  expect("one backslash", count_backslashes("a\\b"), 1L)
  expect("three backslashes", count_backslashes("/\\/\\/\\a"), 3L)

  # -- the prose -> restatement mapping ------------------------------------
  p <- youarealiar_expectation_parse
  expect("bare host claim", p("host=google.com")$value, "google.com")
  expect("bare host kind", p("host=google.com")$kind, "host")
  expect("accept-prefixed host claim",
         p("accept:host=a.b.c (TAB stripped)")$value, "a.b.c")
  expect("trailing paren is not part of the host",
         p("accept:host=xn--pdk.abc.xn--idk (IDNA/Punycode)")$value,
         "xn--pdk.abc.xn--idk")
  # "host=empty" is a claim that there IS no host, not a hostname called
  # "empty" -- the yal-009 case, where WHATWG reads the whole token as a
  # scheme.
  expect("host=empty is not a hostname", p("scheme=www.php.net, host=empty")$kind,
         "accept")
  expect("host=empty carries no value",
         p("scheme=www.php.net, host=empty")$value, NA_character_)
  expect("no host mentioned at all",
         p("accept:non-special scheme, opaque; host empty")$kind, "accept")
  expect("comma ends the host", p("host=a.b, and more")$value, "a.b")

  # -- the roster itself ----------------------------------------------------
  r <- youarealiar_roster()
  expect("nine rows transcribed", nrow(r), 9L)
  expect("ids unique", anyDuplicated(r$id), 0L)
  # The roster's own declarations must match the strings it carries, or the
  # ambiguity resolutions are decoration.
  self_cp <- vapply(seq_len(nrow(r)), function(i) {
    identical(special_codepoints(r$input[i]), as.integer(r$cp_special[[i]]))
  }, logical(1))
  expect("roster code points match its own strings", all(self_cp), TRUE)
  self_bs <- vapply(seq_len(nrow(r)), function(i) {
    identical(count_backslashes(r$input[i]), as.integer(r$n_backslash[i]))
  }, logical(1))
  expect("roster backslash counts match its own strings", all(self_bs), TRUE)
  # The three rows that exist because of the escaping ambiguity.
  expect("yal-002 is a TAB, not a backslash",
         r$cp_special[[which(r$id == "yal-002")]], 9L)
  expect("yal-002 has no backslash",
         r$n_backslash[which(r$id == "yal-002")], 0L)
  expect("yal-003 is three CR LF pairs",
         r$cp_special[[which(r$id == "yal-003")]],
         c(13L, 10L, 13L, 10L, 13L, 10L))
  expect("yal-004 has three single backslashes",
         r$n_backslash[which(r$id == "yal-004")], 3L)

  # -- the checks must be able to fail -------------------------------------
  fx <- read_fixture(FIXTURE)
  cm <- fx[!is.na(fx$source) & fx$source == GROUP, , drop = FALSE]
  cm$input <- recover_inputs(cm)
  expect("transcription check clean on the fixture",
         length(check_transcription(cm, r)), 0L)
  expect("byte check clean on the fixture", length(check_bytes(cm, r)), 0L)
  expect("restatement check clean on the fixture",
         length(check_restatement(cm, r)), 0L)
  expect("citation check clean on the fixture",
         length(check_citations(cm, r)), 0L)

  bent <- cm
  bent$input[bent$id == "yal-002"] <- "https://user:pass@xdavidhu.me\\est.corp.google.com"
  expect("byte check notices TAB turned into a backslash",
         length(check_bytes(bent, r)) > 0L, TRUE)
  bent2 <- cm
  bent2$input[bent2$id == "yal-003"] <- gsub("\r", "", cm$input[cm$id == "yal-003"],
                                             fixed = TRUE)
  expect("byte check notices the CR bytes going missing",
         length(check_bytes(bent2, r)) > 0L, TRUE)
  bent3 <- cm
  bent3$oracle_value[bent3$id == "yal-001"] <- "yahoo.com"
  expect("restatement check notices a flipped host",
         length(check_restatement(bent3, r)) > 0L, TRUE)
  bentd <- cm
  bentd$fsss_host[bentd$id == "yal-001"] <- "yahoo.com"
  expect("restatement check notices an undocumented fsss_host difference",
         length(check_restatement(bentd, r)) > 0L, TRUE)
  # yal-005 carries a deviation, so a differing fsss_host there is ALLOWED.
  bente <- cm
  bente$fsss_host[bente$id == "yal-005"] <- "something.else"
  expect("a documented deviation may differ on fsss_host",
         length(check_restatement(bente, r)), 0L)
  bentf <- bente
  bentf$rurl_deviation[bentf$id == "yal-005"] <- NA_character_
  expect("but deleting the deviation makes it fail",
         length(check_restatement(bentf, r)) > 0L, TRUE)
  bent4 <- cm
  bent4$notes[bent4$id == "yal-001"] <- "no citation here"
  expect("citation check notices a lost citation",
         length(check_citations(bent4, r)) > 0L, TRUE)
  bent5 <- cm
  bent5$notes[bent5$id == "yal-001"] <- "Section V.9: wrong section entirely"
  expect("citation check notices the WRONG section",
         length(check_citations(bent5, r)) > 0L, TRUE)
  expect("transcription check notices a dropped row",
         length(check_transcription(cm[-1L, , drop = FALSE], r)) > 0L, TRUE)

  # -- the byte-verification reporter --------------------------------------
  e <- check_byteverification()
  expect("byte-verify reports rather than fails", length(e$fail), 0L)
  expect("byte-verify says why it is not re-runnable",
         grepl("NOT re-runnable", e$note, fixed = TRUE), TRUE)
  expect("byte-verify is fatal on a missing record",
         grepl("FATAL", check_byteverification(tempfile())$fail, fixed = TRUE),
         TRUE)
  # If the recording gap is ever closed, the gate must demand enforcement.
  filled <- tempfile(fileext = ".json")
  on.exit(unlink(filled), add = TRUE)
  j <- jsonlite::fromJSON(PROVENANCE, simplifyVector = FALSE)
  for (fi in seq_along(j$fixtures)) {
    for (gi in seq_along(j$fixtures[[fi]]$source_groups)) {
      if (identical(j$fixtures[[fi]]$source_groups[[gi]]$group, GROUP)) {
        j$fixtures[[fi]]$source_groups[[gi]]$upstream_path <- "some/file.json"
      }
    }
  }
  writeLines(jsonlite::toJSON(j, auto_unbox = TRUE, null = "null"), filled)
  expect("byte-verify demands enforcement once the path is recorded",
         length(check_byteverification(filled)$fail) > 0L, TRUE)

  cat(sprintf("self-test: %d passed, %d failed\n", pass, length(fail)))
  if (length(fail)) {
    cat(paste0("  - ", fail, collapse = "\n"), "\n", sep = "")
    stop("verify-youarealiar self-test: FAIL", call. = FALSE)
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
  roster <- youarealiar_roster()
  byteverify <- check_byteverification()

  cat(sprintf("group           : %s (class C, paper transcription)\n", GROUP))
  cat(sprintf("committed rows  : %d\n", nrow(committed)))
  cat(sprintf("transcribed rows: %d\n", nrow(roster)))

  fails <- c(
    check_transcription(committed, roster),
    check_bytes(committed, roster),
    check_restatement(committed, roster),
    check_citations(committed, roster),
    byteverify$fail
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
    "byte exactness  : %d special code point(s) and %d backslash(es) exact\n",
    length(unlist(roster$cp_special)), sum(roster$n_backslash)))
  cat("restatement     : oracle_kind/oracle_value/fsss_host restate the prose\n")
  cat("citations       : every row cites the declared paper section\n")
  if (!is.null(byteverify$note)) {
    cat(byteverify$note, "\n", sep = "")
  }
  cat("TRANSCRIPTION INTEGRITY: PASS\n")
  cat(paste0("NOTE: this gate cannot tell you the transcription is CORRECT -- ",
             "the paper is\n      the primary source and there is no ",
             "algorithm to re-run. It proves the\n      committed rows are ",
             "the transcription that was recorded.\n"))
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
