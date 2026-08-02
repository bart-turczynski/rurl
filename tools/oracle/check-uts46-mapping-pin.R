#!/usr/bin/env Rscript
# Re-derive the UTS-46 pin carried by the ip-obfuscation group of
# tests/testthat/fixtures/oracle-provenance.json.
#
# WHY THIS EXISTS. tools/oracle/derive-ip-obfuscation.R does not implement
# UTS-46. It transcribes exactly one slice of the IDNA mapping table -- the
# three full-stop variants and the ASCII uppercase mappings -- and ABORTS on
# every other non-ASCII code point rather than guessing (see
# ipobf_uts46_to_ascii). That slice is a claim about upstream DATA, and a claim
# about upstream data that nobody can re-check is exactly what let six
# fabricated section citations survive review in this group (RURL-qhwktfcw).
# So the claim is made re-derivable rather than asserted in prose.
#
# NOT A GATE, AND DELIBERATELY NOT WIRED INTO CI. It reads unicode.org over the
# network, so it cannot be a blocking step -- the pslr::psl_refresh() posture.
# .github/workflows/verify.yml names the oracle scripts it runs one by one; this
# file is not among them, and must not be added. Run it by hand when bumping the
# pin or when re-checking it:
#
#   Rscript tools/oracle/check-uts46-mapping-pin.R
#
# WHAT IT PROVES, AND WHAT IT DOES NOT. It proves the 29 transcribed mappings
# hold in every published IdnaMappingTable.txt. It does NOT verify UTS-46
# Processing -- CheckBidi, CheckJoiners, CheckHyphens, the transitional flag --
# because the derivation implements none of that and the pin does not claim it.

UTS46_BASE <- "https://www.unicode.org/Public/idna"

# Every version published under Public/idna/, WITH the sha256 of the table this
# repository verified against. Listed rather than scraped so the sweep is
# reproducible: a directory listing that changes shape would silently shrink the
# population, and a sweep that quietly checks fewer versions is the failure mode
# this file guards against.
#
# WHY THE HASHES ARE HERE AND NOT JUST THE VERSION NUMBERS. A pin that names
# only a revision does not fix BYTES. Unicode's versioned directories are meant
# to be immutable, but "meant to be" is not a check: without a recorded digest,
# an in-place edit upstream would leave this sweep re-deriving happily against
# changed content and reporting PASS -- which is the same silent-supersession
# hazard the whole normative_dependencies duty exists to detect, moved one level
# up. The digest is what makes the pin byte-exact.
#
# THIS IS NOT AN IMPORT. No upstream bytes are vendored here; a digest of what
# was checked is a verification record, not an artifact. The ip-obfuscation
# group stays section_2_3_applies = false, and nothing in the section-2.3
# contract is created or discharged by this table.
#
# Append a version AND its digest together when Unicode publishes. A red here is
# a prompt to investigate, not a breakage: check whether upstream reissued the
# file, confirm the 29 mappings still hold, then update the digest deliberately.
UTS46_TABLES <- c(
  "5.2.0"  = "c9f31ec1df7f4b116434757311a84de15fbc89f3f0e089a18697a20093200f0e",
  "6.0.0"  = "4b0fdf51e37534c9ff7ade0d7d6269e692bd5edb75c4f6efbfefee538fd6561b",
  "6.1.0"  = "f9a0b736bf9782bba8e998ee109dfaf9784615fb78018fe4afdbb2e606f2ccb4",
  "6.2.0"  = "0d305363277a7f597b4159e9bb5dd8c769b448444784f3cdffe316561fb17780",
  "6.3.0"  = "84c8f25d2b58531dada678cbfb779c0fb04b1aac42c7744a63fb59784e974775",
  "7.0.0"  = "ecb59e0ff7a701353ad0aa34df8d81b48dfb112a5ca586b995bb9d07606323a8",
  "8.0.0"  = "5e9f5929130b713e698162ac5b60a99ccfb831606686b1c50777cd920b55dee2",
  "9.0.0"  = "a20be6e70dd1e48c2d15615455fef6098ba104756e5e37427bf8bd919b9d1118",
  "10.0.0" = "813a8308aeff8bcb9368751e1fd0ad7cc467130965d53ac860f82c4d0d11523f",
  "11.0.0" = "5150bd818dd2f7639e925f5ef5442f2773e26176427f44e46c1e078572bea370",
  "12.0.0" = "87848ce5634c2b018a88dd8e97dd4dfdf575fd2861c3f930817f272e7433d912",
  "12.1.0" = "fdf938953486fbab6bb8acadfc9416202e4df0c09f0b5c5536cb3c5b2fce64b2",
  "13.0.0" = "87d6553a4b86bc49dcade38bf26b745cd81800eb8af295dc3fb99b4729eaea38",
  "14.0.0" = "d43d9ca367af27b0e4c9dc645cadc23690bdecaf7ec2687f37f01180022d4dfa",
  "15.0.0" = "cc8522199541d60326a42a8f91f8748fd15630a42502dd2cf4878e81e2066ead",
  "15.1.0" = "402cbd285f1f952fcd0834b63541d54f69d3d8f1b8f8599bf71a1a14935f82c4",
  # THE PINNED TABLE. immutable_revision in the provenance record names this
  # one; the sixteen above are the historical-stability evidence behind the
  # entry's claim that the mappings have never moved.
  "16.0.0" = "6db2ef4ed35f3b3de74ebc2e00404a9607f76d499f576b8d4043cf14f1ed175c")

UTS46_PINNED <- "16.0.0"
UTS46_VERSIONS <- names(UTS46_TABLES)

# The transcription's whole surface: derive-ip-obfuscation.R gsub()s the three
# full-stop variants to ".", then chartr()s A-Z to a-z. 3 + 26 = 29 facts.
uts46_expected_mappings <- function() {
  cps <- c(0x3002L, 0xFF0EL, 0xFF61L, utf8ToInt("A"):utf8ToInt("Z"))
  want <- c(0x2EL, 0x2EL, 0x2EL,
            utf8ToInt("a"):utf8ToInt("z"))
  stats::setNames(want, cps)
}

# One code point's row in a mapping table. Returns c(status, mapped_cp) or NULL.
uts46_lookup <- function(lines, cp) {
  for (line in lines) {
    line <- trimws(sub("#.*$", "", line))
    if (!nzchar(line)) next
    f <- trimws(strsplit(line, ";", fixed = TRUE)[[1L]])
    rng <- strsplit(f[[1L]], "..", fixed = TRUE)[[1L]]
    lo <- strtoi(rng[[1L]], 16L)
    hi <- strtoi(rng[[length(rng)]], 16L)
    if (cp >= lo && cp <= hi) {
      mapped <- if (length(f) >= 3L && nzchar(f[[3L]])) {
        strtoi(strsplit(f[[3L]], " +")[[1L]][[1L]], 16L)
      } else {
        NA_integer_
      }
      return(list(status = f[[2L]], mapped = mapped))
    }
  }
  NULL
}

main <- function() {
  expected <- uts46_expected_mappings()
  cps <- as.integer(names(expected))
  checks <- 0L
  bad <- character(0)

  for (v in UTS46_VERSIONS) {
    url <- sprintf("%s/%s/IdnaMappingTable.txt", UTS46_BASE, v)
    tmp <- tempfile(fileext = ".txt")
    on.exit(unlink(tmp), add = TRUE)
    ok_dl <- tryCatch({
      utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) {
      stop(sprintf("cannot read %s: %s", url, conditionMessage(e)),
           call. = FALSE)
    })
    stopifnot(ok_dl)

    # BYTES FIRST. Verify the digest before reading a single mapping: a sweep
    # that parses changed content and then reports which mappings "hold" is
    # answering a question about a file nobody pinned.
    got_sha <- digest::digest(file = tmp, algo = "sha256")
    if (!identical(got_sha, unname(UTS46_TABLES[[v]]))) {
      bad <- c(bad, sprintf(paste("%s: table sha256 %s, recorded %s --",
                                  "upstream reissued this file"),
                            v, substr(got_sha, 1L, 12L),
                            substr(UTS46_TABLES[[v]], 1L, 12L)))
      cat(sprintf("%-8s SHA MISMATCH  got %s  recorded %s\n", v,
                  substr(got_sha, 1L, 12L),
                  substr(UTS46_TABLES[[v]], 1L, 12L)))
      next
    }
    lines <- readLines(tmp, warn = FALSE)
    miss <- character(0)
    for (i in seq_along(cps)) {
      checks <- checks + 1L
      got <- uts46_lookup(lines, cps[[i]])
      ok <- !is.null(got) && identical(got$status, "mapped") &&
        identical(got$mapped, expected[[i]])
      if (!ok) {
        miss <- c(miss, sprintf("U+%04X", cps[[i]]))
      }
    }
    if (length(miss)) {
      bad <- c(bad, sprintf("%s: %s", v, toString(miss)))
    }
    cat(sprintf("%-8s %2d/%d transcribed mappings hold%s\n", v,
                length(cps) - length(miss), length(cps),
                if (length(miss)) sprintf("  MISMATCH %s", toString(miss))
                else ""))
  }

  # THE FLOOR. A sweep that checked nothing would otherwise report success --
  # the same "0 rows scored a truthful 0" trap the derivation's checks guard.
  # Reported as a FAILURE alongside the others rather than as a stop(), because
  # a digest mismatch legitimately skips a version's mappings: stopping here
  # would hide WHY the count fell short behind an arithmetic complaint.
  expect_checks <- length(UTS46_VERSIONS) * length(cps)
  if (checks != expect_checks) {
    bad <- c(bad, sprintf(paste("sweep ran %d mapping checks, expected %d --",
                                "a version was skipped, see above"),
                          checks, expect_checks))
  }

  cat(sprintf(paste("\n%d published version(s) x %d fact(s) = %d of %d",
                    "mapping checks run, %d finding(s)\n"),
              length(UTS46_VERSIONS), length(cps), checks, expect_checks,
              length(bad)))
  if (length(bad)) {
    for (b in bad) cat(sprintf("  %s\n", b))
    stop("UTS-46 PIN: FAIL -- see the findings above", call. = FALSE)
  }
  cat(sprintf("UTS-46 PIN: PASS (pinned table %s, sha256 %s)\n", UTS46_PINNED,
              substr(UTS46_TABLES[[UTS46_PINNED]], 1L, 12L)))
  invisible(TRUE)
}

if (identical(environment(), globalenv())) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("the UTS-46 pin check needs the 'digest' package", call. = FALSE)
  }
  main()
}
