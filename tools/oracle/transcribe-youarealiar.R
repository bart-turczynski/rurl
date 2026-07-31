#!/usr/bin/env Rscript
# RURL-ozdejfzl -- the `youarealiar` transcription, as tracked code.
#
# Ported from `_scratch/build-youarealiar-vectors.R` (RURL-dbazixkr slice 6a),
# which produced the 9 rows but was never in the repository: `_scratch/` is
# gitignored, so `oracle-provenance.json` recorded
# `generation_command = MISSING[RURL-vwurxmzm]`.
#
# THIS FILE IS NOT A DERIVATION, AND THE FILENAME SAYS SO. The other two ported
# groups have `derive-*.R` modules because their expected values are computable:
# `wpt-credentials-fragments` assembles them with the WHATWG serializer,
# `ip-obfuscation` computes them with the WHATWG host parser. This group's
# expected values are computable from NOTHING. They are transcribed from a
# paper's reference-implementation column, so the transcription IS the primary
# source (README tier 3), and a file claiming to "derive" them would be lying
# about where the authority sits.
#
# What that leaves checkable is real but narrower, and it is worth being exact
# about the difference:
#
#   1. TRANSCRIPTION INTEGRITY. The transcribed data -- input bytes, the
#      expectation, the paper's own per-parser claim, the citation -- lived only
#      in gitignored space. It lives here now, and the fixture must agree.
#
#      NOT `notes`, and that boundary was found by measurement rather than
#      guessed. Freezing `notes` against the builder produced five
#      disagreements, and in every one the FIXTURE was the more current text:
#      `rfc3986` now rejects backslashes so yal-001/007 no longer reproduce the
#      paper's RFC column (RURL-qrfrvmkg), yal-006 was reclassified out of
#      `aligned` (RURL-xfbzkico/RURL-kmkyicpt), yal-008's closed scheme set
#      gained `file`, and yal-009 moved from "needs-investigation" to
#      boundary-by-design once `scheme_policy` existed.
#
#      So a class-C row mixes two kinds of cell and they must be treated
#      differently. `input`/`standard_expectation`/`paper_claimed_behavior`/
#      `source_reference` are PRIMARY-SOURCE data and are immutable -- all four
#      matched the builder exactly across all nine rows. `notes` is LIVING
#      COMMENTARY about rurl's own behavior and is SUPPOSED to change when that
#      behavior changes; freezing it would convert a correct update into a gate
#      failure and pressure the next author to revert a true statement. What is
#      stable inside it is the paper citation, so that is what check D asserts
#      and `section` is what the roster carries.
#   2. BYTE EXACTNESS. Three of the nine rows exist BECAUSE the paper's PDF
#      escaping is ambiguous, and the transcription resolved that ambiguity by
#      hand. Those resolutions are the most perishable thing in the group and
#      nothing tested them. See `cp_special` / `n_backslash` below.
#   3. RESTATEMENT FIDELITY. `oracle_kind` / `oracle_value` are a later,
#      machine-readable restatement of the prose expectation (RURL-yeikpnan).
#      A restatement can drift from what it restates; this makes the mapping
#      executable. See `youarealiar_expectation_parse()`.
#
# It loads no package. The independence argument is weaker here than for a
# derivation -- there is no spec algorithm to be independent OF -- but a
# transcription module that could reach `rurl` would invite exactly the
# "expected := what we currently output" collapse the tier-1 files were shaped
# to prevent.
#
# THE ROSTER'S SOURCE OF TRUTH IS THE BUILDER, NOT THE FIXTURE. Transcribing it
# from the fixture would make the integrity check a tautology. It is transcribed
# from `_scratch/build-youarealiar-vectors.R` instead, so a disagreement means
# the builder and the fixture have DRIFTED -- which is a live possibility, since
# the fixture has been through RURL-yeikpnan's surface re-baseline and
# RURL-nknytzxz's oracle repair since the builder last ran.
#
# Sourced by `tools/oracle/verify-youarealiar.R`.

# Non-ASCII and control bytes are written as escapes so this file stays pure
# ASCII and the strings are unambiguous regardless of source encoding. That is
# not cosmetic here: the whole point of rows 002/003 is WHICH bytes they carry.
YAL_TAB <- "\t"
YAL_CR <- "\r"
YAL_LF <- "\n"
YAL_HI <- "ヒ" # katakana HI
YAL_KI <- "キ" # katakana KI
YAL_NI <- "ニ" # katakana NI

YAL_SOURCE_PREFIX <- "Ajmani et al., 'yoU aRe a Liar', SecWeb 2022, "
YAL_SOURCE_SUFFIX <-
  "; bytes verified vs wspr-ncsu/urlparsing-framework (BSD-3-Clause)"

# The transcribed rows.
#
# `cp_special` and `n_backslash` are the ambiguity resolutions, declared as
# testable facts rather than left in a comment:
#
#   * The paper DISPLAYS yal-002 as `xdavidhu.me\test...`, where `\t` is escape
#     notation for a TAB byte -- not a backslash. Confirmed by every parser
#     column in the paper showing `est` (urllib3 `me%09est`, whatwg-url
#     `meest`, php `me_est`). So: one code point 9, zero backslashes.
#   * yal-003 carries literal CR LF pairs, and CR does not survive a CSV round
#     trip (it normalizes to LF), so the fixture's stored `input` cell is
#     lossy BY DESIGN and `input_json` is the only faithful carrier. Declaring
#     the six code points 13,10,13,10,13,10 is what notices if that carrier
#     ever stops being faithful.
#   * yal-001/004/007 carry SINGLE backslashes. In the framework repo's JSON
#     they appear as `\\`, which is one byte after JSON decoding; transcribing
#     the display form would silently double them.
#
# `cp_special` is every code point outside printable ASCII (32..126), in order.
youarealiar_roster <- function() {
  row <- function(id, input, cp_special, n_backslash, expectation, paper,
                  pitfall, section) {
    data.frame(id = id, input = input, n_backslash = n_backslash,
               expectation = expectation, paper = paper, pitfall = pitfall,
               section = section, stringsAsFactors = FALSE,
               cp_special = I(list(cp_special)))
  }

  rbind(
    row("yal-001", "http://google.com:80\\@yahoo.com", integer(0), 1L,
        "host=google.com",
        paste0("whatwg-url/chrome/urllib3/url-parse host=google.com; ",
               "urllib/uri-js/php/curl host=yahoo.com (CVE-2020-26291)"),
        "hostname confusion (backslash before @)",
        "V.1"),

    row("yal-002",
        paste0("https://user:pass@xdavidhu.me", YAL_TAB,
               "est.corp.google.com"),
        9L, 0L,
        "accept:host=xdavidhu.meest.corp.google.com (TAB stripped)",
        paste0("chrome/url-parse host=xdavidhu.me<TAB>est; urllib3/uri-js ",
               "host=xdavidhu.me%09est; urllib/whatwg-url ",
               "host=xdavidhu.meest; php host=xdavidhu.me_est; curl Error"),
        "control-character confusion (literal TAB 0x09 in host)",
        "V.3"),

    row("yal-003",
        paste0("http://127.0.0.", YAL_CR, YAL_LF, "1:6379?SET", YAL_CR,
               YAL_LF, "test", YAL_CR, YAL_LF, "failure12:80"),
        c(13L, 10L, 13L, 10L, 13L, 10L), 0L,
        "accept:host=127.0.0.1 (CR/LF stripped)",
        paste0("chrome/url-parse host=127.0.0.<CRLF>1; urllib3/uri-js ",
               "host=127.0.0.%0D%0A1; urllib/whatwg-url host=127.0.0.1; php ",
               "host=127.0.0._1; curl Error"),
        paste0("control-character confusion (literal CR LF in host; ",
               "CRLF/SSRF-redis)"),
        "V.3"),

    row("yal-004", "https:/\\/\\/\\github.com/foo/bar", integer(0), 3L,
        "host=github.com",
        paste0("chrome/whatwg-url host=github.com; urllib3/uri-js ",
               "path=/%5C/%5C/%5Cgithub.com/foo/bar; urllib ",
               "path=/\\/\\/\\github.com/foo/bar; php ",
               "path=/\\/\\/\\github.com/foo/bar; curl host=\\"),
        "backslash confusion (mixed //\\ before host)",
        "V.4"),

    row("yal-005",
        paste0("http://", YAL_HI, ":", YAL_KI, "@", YAL_HI, ".abc.", YAL_NI,
               "/", YAL_HI),
        c(12498L, 12461L, 12498L, 12491L, 12498L), 0L,
        "accept:host=xn--pdk.abc.xn--idk (IDNA/Punycode)",
        paste0("chrome/url-parse host=<unicode>; urllib3/uri-js/whatwg-url ",
               "host=xn--pdk.abc.xn--idk; urllib/php/curl host=<raw ",
               "unicode>"),
        "encoding confusion (non-ASCII host, IDNA)",
        "V.7"),

    row("yal-006", "https:///evil.com", integer(0), 0L, "host=evil.com",
        paste0("Section VI.B PoC: RFC-empty-host validators pass the check ",
               "while curl (ignores extra slash) fetches evil.com -> SSRF ",
               "filter bypass"),
        "slash confusion (empty authority, special scheme)",
        "VI.B"),

    row("yal-007", "http://example.com:80\\@localhost:8080/secret.txt",
        integer(0), 1L, "host=example.com",
        paste0("Section VI.A PoC: urllib3 validates host=example.com ",
               "(allow-list pass) while urllib fetches host=localhost -> ",
               "allow-list bypass"),
        "backslash/hostname confusion (validation vs fetch split)",
        "VI.A"),

    row("yal-008", "foo://///////bar.com/", integer(0), 0L,
        "accept:non-special scheme, opaque; host empty",
        paste0("chrome host=bar.com; ",
               "urllib/urllib3/uri-js/url-parse/whatwg-url ",
               "path=///////bar.com/; php Error"),
        "slash confusion (many slashes, non-special scheme)",
        "V.5"),

    row("yal-009", "www.php.net:80/index.php?test=1", integer(0), 0L,
        "scheme=www.php.net, host=empty",
        paste0("urllib/url-parse/uri-js/whatwg-url ",
               "scheme=www.php.net,host=empty; urllib3/php/curl ",
               "host=www.php.net"),
        "scheme confusion (dotted token before colon)",
        "V.2"))
}

# The prose expectation -> machine restatement mapping (RURL-yeikpnan added
# `oracle_kind` / `oracle_value` as a machine-readable restatement of the
# transcribed `standard_expectation` prose).
#
# The rule, read off the transcription's own vocabulary: an expectation that
# names a CONCRETE host is a `host` claim carrying that host; one that says the
# host is empty, or names no host at all, is a bare `accept` claim carrying no
# value. "host=empty" is the second kind despite its spelling -- yal-009's
# expectation is that WHATWG reads the whole dotted token as a SCHEME, leaving
# no host, which is not a claim about a hostname.
youarealiar_expectation_parse <- function(expectation) {
  m <- regmatches(expectation,
                  regexec("host=([^ ,;)]+)", expectation))[[1]]
  if (length(m) != 2L || identical(m[2], "empty")) {
    return(list(kind = "accept", value = NA_character_))
  }
  list(kind = "host", value = m[2])
}

# `source_reference` as the fixture records it: the citation, the pitfall the
# row demonstrates, and the byte-verification claim.
youarealiar_source_reference <- function(pitfall) {
  paste0(YAL_SOURCE_PREFIX, pitfall, YAL_SOURCE_SUFFIX)
}

# Runs only when this file is executed as a script: `sys.nframe()` is 0 at the
# top level of an Rscript invocation and non-zero inside the verifier's
# `source()` call. The earlier form also tested `!is.null(sys.frames())`, which
# is FALSE at top level -- so the block never fired and the header's "running it
# directly prints ..." was untrue of all three modules.
if (sys.nframe() == 0L) {
  r <- youarealiar_roster()
  cat("transcribed rows:", nrow(r), "\n")
  for (i in seq_len(nrow(r))) {
    p <- youarealiar_expectation_parse(r$expectation[i])
    cat(sprintf("  %s  backslashes=%d  special=[%s]  -> %s/%s\n", r$id[i],
                r$n_backslash[i],
                paste(r$cp_special[[i]], collapse = ","), p$kind,
                if (is.na(p$value)) "-" else p$value))
  }
}
