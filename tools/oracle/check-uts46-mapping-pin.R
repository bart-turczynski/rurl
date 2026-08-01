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
# fabricated section citations survive review in this same group (RURL-qhwktfcw).
# So the claim is made re-derivable rather than asserted in prose.
#
# NOT A GATE, AND DELIBERATELY NOT WIRED INTO CI. It reads unicode.org over the
# network, so it cannot be a blocking step -- same posture as pslr::psl_refresh().
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

# Every version published under Public/idna/. Listed rather than scraped so the
# sweep is reproducible: a directory listing that changes shape would silently
# shrink the population, and a sweep that quietly checks fewer versions is the
# failure mode this file guards against. Append here when Unicode publishes.
UTS46_VERSIONS <- c("5.2.0", "6.0.0", "6.1.0", "6.2.0", "6.3.0", "7.0.0",
                    "8.0.0", "9.0.0", "10.0.0", "11.0.0", "12.0.0", "12.1.0",
                    "13.0.0", "14.0.0", "15.0.0", "15.1.0", "16.0.0")

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
    lines <- tryCatch(readLines(url, warn = FALSE),
                      error = function(e) {
                        stop(sprintf("cannot read %s: %s", url,
                                     conditionMessage(e)), call. = FALSE)
                      })
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
  # the same "0 rows scored a truthful 0" trap the derivation's own checks guard.
  expect_checks <- length(UTS46_VERSIONS) * length(cps)
  if (checks != expect_checks) {
    stop(sprintf("sweep ran %d checks, expected %d", checks, expect_checks),
         call. = FALSE)
  }

  cat(sprintf("\n%d published version(s) x %d fact(s) = %d checks, %d version(s) with a mismatch\n",
              length(UTS46_VERSIONS), length(cps), checks, length(bad)))
  if (length(bad)) {
    for (b in bad) cat(sprintf("  %s\n", b))
    stop("UTS-46 PIN: FAIL -- a transcribed mapping does not hold upstream",
         call. = FALSE)
  }
  cat("UTS-46 PIN: PASS\n")
  invisible(TRUE)
}

if (identical(environment(), globalenv())) {
  main()
}
