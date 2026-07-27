# External adversarial/conformance URL vectors imported from third-party suites
# (RURL-dbazixkr, epic RURL-wncwfasl — the R Journal paper's disagreement
# table). Unlike the hand-derived golden table in
# test-url-standard-conformance.R, these rows are transcribed from external
# sources and their oracle is the
# SOURCE's expectation, recorded per row in `standard` + `standard_expectation`
# (and, for class-C paper rows, `paper_claimed_behavior` kept SEPARATE from the
# standard's "should"). The fixture doubles as the results table: it records
# rurl's actual output in both profiles and a `diverges` flag vs the row oracle.
#
# Slice 1 seeds it with the WHATWG web-platform-tests `url/resources/
# urltestdata.json` FAILURE cases (BSD-3-Clause) — inputs the WHATWG URL parser
# MUST reject. rurl rejects all but a small, pinned set where its WHATWG profile
# still accepts (forbidden host code points surfaced as `warning-no-tld`). These
# are NOT declared correct-forever: each is triaged in the divergence ledger
# (_scratch/divergence-ledger.md) as documented-boundary vs candidate-bug — some
# (e.g. the UTS-46 soft-hyphen rows) are open bug candidates, not settled
# behavior. This test is a characterization/regression guard: it pins rurl's
# recorded output and the divergence set so BOTH a regression (a new must-fail
# input starts being accepted) AND progress (a tracked divergence gets fixed)
# force a deliberate, reviewed update here.
#
# Part 3a (RURL-moselrwp) adds a DUAL-STANDARD expected-outcome oracle:
# `rfc3986_expected` + `whatwg_expected` record what each STANDARD requires
# (clean_url / "failure" / "accept" sentinel), `oracle_ref` cites the RFC 3986
# section(s) / WHATWG algorithm step(s) it is derived from, and
# `divergence_class` buckets each row (aligned / spec-divergent / both-accept /
# both-reject / not-runnable). This is orthogonal to
# `diverges` (which flags rurl vs the row's SOURCE oracle): `divergence_class`
# is standard-vs-standard.
#
# RURL-nknytzxz then split the one thing this schema still conflated. The
# `rfc3986_expected` cells were transcribed from WPT -- a WHATWG oracle -- so
# on 75 rows they asserted "RFC 3986 rejects this" about strings the RFC
# plainly accepts (empty reg-name, pct-encoded reg-name octets, path-rootless
# read as userinfo), and on 20 more they recorded rurl's tolerant OUTPUT as if
# the RFC mandated it. Because rurl ALSO declines the first 75 -- by policy,
# not by standard -- oracle and implementation confirmed each other and the
# suite stayed green. The repair: `divergence_class` is now purely
# standard-vs-standard (the old `parser-boundary` value, which described rurl,
# is retired), a new `rurl_deviation` column carries "rurl departs here, and
# this ADR/ticket owns the decision", and the `oracle-vs-grammar` test below
# checks the RFC column against a transcription of the RFC's own ABNF so the
# co-confirmation trap cannot reopen. Derivation:
# tools/oracle-audit-rfc3986.R.
# The `aligned` bucket is deliberately un-cited (tiered): where RFC, WHATWG, and
# rurl (both profiles) all agree, a self-consistency invariant stands in for a
# hand-derived oracle, so citation effort concentrates on the diverging rows.

fixture_path <- testthat::test_path("fixtures", "external-url-vectors.csv")

# The adversarial `input` column cannot survive a round-trip through a raw CSV
# cell on every platform: two rows embed raw CR/LF and 13 more carry C0 control
# characters, so a non-UTF-8 locale (Windows) mis-decodes them and even a
# UTF-8 read silently normalizes CRLF -> LF inside quoted fields. The fixture
# therefore carries an ASCII-only, JSON-escaped `input_json` column as the
# SOURCE OF TRUTH; `input` is the human-readable mirror. Reconstruct `input`
# from it -- but ONLY for runnable rows: non-runnable rows are provenance-only
# and must keep `input` NA (invariant asserted above).
read_vectors <- function() {
  fx <- utils::read.csv(
    fixture_path, stringsAsFactors = FALSE, colClasses = "character",
    na.strings = "NA", encoding = "UTF-8"
  )
  testthat::skip_if_not_installed("jsonlite")
  runnable <- fx$runnable == "yes"
  fx$input[runnable] <- vapply(
    fx$input_json[runnable], jsonlite::fromJSON,
    character(1), USE.NAMES = FALSE
  )
  fx
}

test_that("external-url-vectors fixture is well-formed", {
  fx <- read_vectors()
  expect_gt(nrow(fx), 0)
  expect_setequal(
    names(fx),
    c("id", "source_class", "source", "source_reference", "input",
      "input_json", "standard", "standard_expectation",
      "paper_claimed_behavior", "runnable", "rurl_rfc_status",
      "rurl_rfc_clean", "rurl_whatwg_status", "rurl_whatwg_clean",
      "diverges", "notes",
      # Part 3a dual-standard expected-outcome oracle (RURL-moselrwp),
      # + rurl_deviation (RURL-nknytzxz).
      "rfc3986_expected", "whatwg_expected", "oracle_ref", "divergence_class",
      "rurl_deviation",
      # RURL-yeikpnan: the oracle re-baselined onto surface (b). `fsss_*` are
      # the three standard serializations; `oracle_kind`/`oracle_value` restate
      # the source corpus's expectation in one machine-readable shape.
      "fsss_whatwg", "fsss_rfc_source", "fsss_rfc_normalized",
      "oracle_kind", "oracle_value", "fsss_host", "fsss_conforms")
  )
  expect_identical(anyDuplicated(fx$id), 0L)
  expect_true(all(fx$source_class %in% c("A", "B", "C")))
  expect_true(all(fx$standard %in% c("rfc3986", "whatwg", "both")))
  # runnable rows carry an input; non-runnable rows are recorded for provenance
  # (relative-resolution / NUL-byte) with no rurl output.
  runnable <- fx$runnable == "yes"
  expect_false(anyNA(fx$input[runnable]))
  expect_true(all(is.na(fx$rurl_whatwg_status[!runnable])))
})

# Documented rurl deviations from the spec oracle, read off the fixture's
# `rurl_deviation` column: NA where rurl matches both spec oracles, otherwise
# the ADR / tracked ticket that OWNS the decision to differ. For these rows
# rurl's behavior is pinned by the per-source characterization tests below and
# is NOT expected to equal the spec oracle -- so the conformance check skips
# them.
#
# This used to be a hand-maintained id list, and `divergence_class` carried the
# `parser-boundary` value to mark the same rows. That conflated two independent
# facts -- how the two STANDARDS relate to each other, and whether RURL follows
# them -- onto one axis, and it is what let RURL-nknytzxz hide: 75 rows where
# rurl declined by POLICY were recorded as though the STANDARD required
# failure, so the oracle and the implementation confirmed each other and the
# test passed. The two facts now live in two columns and cannot collapse again.
oracle_nonconformance_ids <- function(fx = read_vectors()) {
  fx$id[!is.na(fx$rurl_deviation)]
}

test_that("dual-standard oracle: classes and invariants hold", {
  fx <- read_vectors()

  # every row carries a class; the classes are the closed set.
  # `divergence_class` is STANDARD-vs-STANDARD only -- it says nothing about
  # rurl, which is what `rurl_deviation` is for. (The old `parser-boundary`
  # value mixed the two and is retired, RURL-nknytzxz.)
  expect_false(anyNA(fx$divergence_class))
  expect_true(all(fx$divergence_class %in% c(
    "aligned", "spec-divergent", "both-accept", "both-reject", "not-runnable"
  )))

  # spec-divergent: the standards genuinely disagree -- either exactly one
  # rejects, or both accept with DIFFERENT concrete serializations. Both cited.
  sd <- fx$divergence_class == "spec-divergent"
  expect_true(all(fx$rfc3986_expected[sd] != fx$whatwg_expected[sd]))
  expect_false(anyNA(fx$rfc3986_expected[sd]))
  expect_false(anyNA(fx$whatwg_expected[sd]))
  expect_false(anyNA(fx$oracle_ref[sd]))

  # both-reject: both oracles are "failure".
  br <- fx$divergence_class == "both-reject"
  expect_true(all(fx$rfc3986_expected[br] == "failure"))
  expect_true(all(fx$whatwg_expected[br] == "failure"))

  # both-accept: neither oracle rejects, and they do not contradict each other.
  # "accept" is a sentinel meaning "this standard admits the string" WITHOUT
  # claiming a canonical serialization, so it never contradicts a concrete one.
  ba <- fx$divergence_class == "both-accept"
  expect_false(any(fx$rfc3986_expected[ba] == "failure"))
  expect_false(any(fx$whatwg_expected[ba] == "failure"))
  expect_false(anyNA(fx$oracle_ref[ba]))
  concrete <- ba & fx$rfc3986_expected != "accept" &
    fx$whatwg_expected != "accept"
  expect_identical(
    fx$rfc3986_expected[concrete], fx$whatwg_expected[concrete]
  )

  # aligned is the TIERED bucket: no per-row oracle, and rurl must be
  # self-consistent (both profiles accept and agree on the clean_url) -- that
  # self-consistency IS the assertion that stands in for a hand-cited oracle.
  al <- fx$divergence_class == "aligned"
  expect_true(all(is.na(fx$rfc3986_expected[al])))
  expect_true(all(is.na(fx$whatwg_expected[al])))
  expect_true(all(is.na(fx$oracle_ref[al])))
  # Self-consistency is asserted on surface (b), not on clean_url: `clean_url`
  # is a policy-driven product (P2.2 sec 1c/5.1), so two profiles agreeing on it
  # is agreement about rurl's CLEANING, which is not what this bucket claims.
  expect_identical(fx$fsss_rfc_normalized[al], fx$fsss_whatwg[al])
  expect_false(anyNA(fx$fsss_whatwg[al]))

  # not-runnable rows carry no oracle value (source view stays in
  # standard_expectation).
  nr <- fx$divergence_class == "not-runnable"
  expect_true(all(fx$runnable[nr] != "yes"))
  expect_true(all(is.na(fx$rfc3986_expected[nr])))

  # a rurl_deviation is only meaningful against a stated oracle, and it must
  # cite the ADR / ticket that owns the decision rather than merely asserting
  # rurl is right.
  dev <- !is.na(fx$rurl_deviation)
  expect_true(all(fx$runnable[dev] == "yes"))
  expect_false(anyNA(fx$oracle_ref[dev]))
  expect_true(all(grepl("ADR [0-9]{4}|RURL-[a-z]+", fx$rurl_deviation[dev])))
  # the two axes are independent: deviations occur in every class, so neither
  # column can be reconstructed from the other. This is the invariant whose
  # absence hid RURL-nknytzxz.
  expect_gt(length(unique(fx$divergence_class[dev])), 1L)
})

test_that("rurl conforms to the spec oracle except at documented boundaries", {
  fx <- read_vectors()
  fx <- fx[fx$runnable == "yes", , drop = FALSE]
  skip_ids <- oracle_nonconformance_ids(fx)
  chk <- fx[!(fx$id %in% skip_ids), , drop = FALSE]

  # helper: does rurl's serialization match a spec oracle cell? Scored on
  # surface (b) -- `serialize_url()` -- because that is the only full-string
  # output rurl has that is contractually allowed to carry a conformance claim
  # (P5.3 CLAIM-1; RURL-yeikpnan). `clean_url` is surface (c) and is excluded.
  conforms <- function(oracle, serialized) {
    accepted <- !is.na(serialized)
    # a concrete serialization oracle: rurl must reproduce it exactly. The
    # "accept" sentinel means the standard admits the string but this fixture
    # claims no canonical serialization, so only the verdict is asserted.
    out <- accepted & serialized == oracle
    sentinel <- !is.na(oracle) & oracle == "accept"
    fails <- !is.na(oracle) & oracle == "failure"
    out[sentinel] <- accepted[sentinel]
    out[fails] <- !accepted[fails]
    out
  }

  rfc_ok <- conforms(chk$rfc3986_expected, chk$fsss_rfc_normalized)
  wha_ok <- conforms(chk$whatwg_expected, chk$fsss_whatwg)
  # NA oracle (aligned rows) is covered by the invariant test above; only assert
  # where an oracle value exists.
  rfc_ok[is.na(chk$rfc3986_expected)] <- TRUE
  wha_ok[is.na(chk$whatwg_expected)] <- TRUE

  expect_true(all(rfc_ok),
    info = paste("RFC oracle mismatch:", toString(chk$id[!rfc_ok])))
  expect_true(all(wha_ok),
    info = paste("WHATWG oracle mismatch:", toString(chk$id[!wha_ok])))
})

test_that("the rfc3986 oracle agrees with the RFC 3986 grammar itself", {
  # THE GUARD FOR RURL-nknytzxz. Everything else in this file checks rurl
  # against the fixture; nothing checked the FIXTURE. That is how 75 rows came
  # to assert "RFC 3986 rejects this" about strings the RFC plainly accepts:
  # they were transcribed from the WHATWG web-platform-tests, rurl happened to
  # reject them too (by policy -- ADR 0004's host-shape gate, the closed scheme
  # set), oracle and implementation agreed, and the suite stayed green.
  #
  # A fixture cell cannot be validated by the parser it exists to validate. So
  # the referee here is helper-rfc3986-abnf.R: the RFC's own grammar,
  # transcribed from Appendix A, sharing no code with rurl. If the two ever
  # disagree, either the cell is wrong or the transcription is -- and both are
  # worth stopping for.
  fx <- read_vectors()
  fx <- fx[fx$runnable == "yes" & !is.na(fx$rfc3986_expected), , drop = FALSE]

  grammar_rejects <- !rfc3986_abnf_accepts(fx$input)
  oracle_rejects <- fx$rfc3986_expected == "failure"

  # An oracle claiming the STANDARD rejects must be backed by the standard.
  bad_reject <- oracle_rejects & !grammar_rejects
  expect_false(any(bad_reject), info = paste(
    "rfc3986_expected='failure' but the RFC 3986 grammar ACCEPTS:",
    toString(fx$id[bad_reject]),
    "-- if rurl declines these, that belongs in rurl_deviation, not here."
  ))
  # ...and one claiming it accepts must be a string the grammar admits.
  bad_accept <- !oracle_rejects & grammar_rejects
  expect_false(any(bad_accept), info = paste(
    "rfc3986_expected is an accept but the RFC 3986 grammar REJECTS:",
    toString(fx$id[bad_accept])
  ))
})

test_that("the RFC 3986 grammar transcription is itself sound", {
  # The guard above is only as good as the matcher, and a matcher that
  # accepted everything would silently pass it. These are hand-checked
  # positives and negatives, each naming the production at issue.
  expect_true(all(rfc3986_abnf_accepts(c(
    "http://example.com/a/b?q=1#f", # the ordinary case
    "http://user:pass@/",           # S3.2.2 reg-name = *( ... ), may be empty
    "sc://:12/",                    # empty host + S3.2.3 port = *DIGIT
    "http://ho%00st/",              # pct-encoded octets not decoded here
    "http:@/www.example.com",       # no "//" -> path-rootless, "@" is a pchar
    "urn:isbn:0451450523",          # path-rootless, no authority
    "http://[2001:db8::1]:80/",     # IP-literal + IPv6address
    "http://[v7.aBc]/",             # IPvFuture
    "http://1.2.3.4/",              # IPv4address
    "a+b-c.d://x/"                  # S3.1 scheme = ALPHA *( ALPHA/DIGIT/+/-/. )
  ))))
  expect_false(any(rfc3986_abnf_accepts(c(
    "http://a|b/",           # "|" is in no production
    "http://a b/",           # raw SP
    "http://é.com/",    # raw non-ASCII: S2.5 requires pct-encoding first
    "http://a\\b/",          # backslash
    "http://ho%zzst/",       # malformed pct-encoding
    "http://[2001:db8::1::2]/", # two "::" -- not an IPv6address alternative
    "http://[1.2.3.4]/",     # IPv4 is not valid INSIDE an IP-literal
    "1http://x/",            # scheme must start with ALPHA
    "//example.com/",        # network-path reference, not a URI (no scheme)
    "example.com/x",         # relative reference, not a URI
    "http://x/\001"          # raw C0 control
  ))))
})

test_that("rurl output on external vectors matches recorded characterization", {
  fx <- read_vectors()
  fx <- fx[fx$runnable == "yes", , drop = FALSE]
  inputs <- fx$input

  rfc <- safe_parse_urls(inputs, url_standard = "rfc3986")
  wha <- safe_parse_urls(inputs, url_standard = "whatwg")

  expect_identical(rfc$parse_status, fx$rurl_rfc_status)
  expect_identical(rfc$clean_url, fx$rurl_rfc_clean)
  expect_identical(wha$parse_status, fx$rurl_whatwg_status)
  expect_identical(wha$clean_url, fx$rurl_whatwg_clean)
  # The recorded FSSS columns must equal a live run too -- they are the
  # substrate every conformance assertion in this file now rests on.
  expect_identical(
    serialize_url(fx$input, standard = "whatwg"), fx$fsss_whatwg
  )
  expect_identical(
    serialize_url(fx$input, standard = "rfc3986", form = "source"),
    fx$fsss_rfc_source
  )
  expect_identical(
    serialize_url(fx$input, standard = "rfc3986", form = "normalized"),
    fx$fsss_rfc_normalized
  )
})

test_that("WPT failure divergences are pinned to the documented boundary set", {
  fx <- read_vectors()
  wpt <- fx[fx$source == "wpt-urltestdata" & fx$runnable == "yes", ,
    drop = FALSE]

  # WPT oracle is WHATWG: every runnable row MUST reject. rurl's WHATWG profile
  # now rejects ALL of them -- there are no remaining divergences. The 9
  # forbidden-host-code-point rows that previously slipped through as
  # warning-no-tld (bare `|`, DEL, U+FFFD, U+FFFF, UTS-46-ignored soft-hyphen)
  # are now fatal under whatwg (RURL-jfuqpwvh, flipping the ADR 0004 boundary
  # into a governed axis per ADR 0007). This stays a WATCH list: any new
  # acceptance (a regression) must update it deliberately with a ledger note.
  diverging_ids <- wpt$id[wpt$diverges == "yes"]
  expect_setequal(diverging_ids, character(0))
  # Every runnable WPT row is genuinely rejected under whatwg (NA clean).
  # Acceptance read off surface (b): surface (c) additionally declines by POLICY
  # (the closed scheme set), so a clean_url NA would credit rurl for rejecting
  # URLs the standard accepts (RURL-yeikpnan).
  expect_true(all(is.na(wpt$fsss_whatwg)))
  expect_true(all(wpt$rurl_whatwg_status == "error"))
})

test_that("IPv4-obfuscation vectors all match the WHATWG oracle", {
  fx <- read_vectors()
  ip <- fx[fx$source == "ip-obfuscation", , drop = FALSE]

  # These are hand-generated arithmetic encodings of a few target IPs
  # (obfuscation technique from cujanovic/SSRF-Testing + JorianWoltjer/ipobf,
  # NOT vendored). Oracle is the WHATWG IPv4 parser: octal/hex/dword/short/mixed
  # forms must canonicalize, overflow/zone-id forms must fail. rurl handles all
  # of them correctly, including the three Unicode alternative separators
  # (U+3002/U+FF0E/U+FF61): the whatwg profile now maps them to '.' in the
  # authority before the IPv4 parse (UTS-46 domain-to-ASCII), so
  # `http://127。0。0。1/` coerces to `http://127.0.0.1/` rather than being kept
  # as a literal reg-name. FIXED in RURL-odsmwsxu, rurl 2.3.0 -- the divergence
  # set is now empty (ipobf-011/012/013 dropped out).
  diverging_ids <- ip$id[ip$diverges == "yes"]
  expect_setequal(diverging_ids, character(0))
  # Non-diverging accept rows must equal the recorded conformant WHATWG
  # serialization; the failure rows must genuinely reject (NA clean).
  ok_rows <- ip[ip$diverges == "no" & ip$standard_expectation != "failure", ,
    drop = FALSE]
  # Compared on surface (b): `standard_expectation` here is a full WHATWG
  # serialization, so clean_url was the wrong side of the comparison even where
  # the two happened to agree (RURL-yeikpnan).
  expect_identical(ok_rows$fsss_whatwg, ok_rows$standard_expectation)
  fail_rows <- ip[ip$standard_expectation == "failure", , drop = FALSE]
  expect_true(all(is.na(fail_rows$fsss_whatwg)))
})

test_that("Ada extra-urltestdata divergences pin to the documented set", {
  fx <- read_vectors()
  ada <- fx[fx$source == "ada-extra-urltestdata" & fx$runnable == "yes", ,
    drop = FALSE]

  # Ada's beyond-WPT vectors (Apache-2.0), oracle = WHATWG. Ada IS a conformant
  # WHATWG parser, so `standard_expectation` is its own `href` -- a full WHATWG
  # serialization. Scored against surface (b), rurl matches it on EVERY runnable
  # row, so there is nothing left to watch (RURL-yeikpnan).
  #
  # This list held 11 ids while the comparison ran on `clean_url`. All 11 were
  # surface artifacts, not parse disagreements: 7 rows surface (c) rejects by
  # the closed scheme set (ADR 0004) and the standard serializer accepts, the
  # IDNA host `Yağız.com` (surface (c) keeps the Unicode spelling by ADR 0002;
  # the standard's own host is punycode), and 3 path/percent rows surface (c)
  # renders readably. rurl's PARSE never disagreed with Ada on any of them.
  # Kept as a WATCH list: a new divergence must be added here deliberately.
  diverging_ids <- ada$id[ada$diverges == "yes"]
  expect_setequal(diverging_ids, character(0))
  # Accept rows must equal Ada's href exactly; failure rows must reject.
  ok_rows <- ada[ada$standard_expectation != "failure", , drop = FALSE]
  expect_identical(ok_rows$fsss_whatwg, ok_rows$standard_expectation)
  fail_rows <- ada[ada$standard_expectation == "failure", , drop = FALSE]
  expect_true(all(is.na(fail_rows$fsss_whatwg)))
})

test_that("yoU-aRe-a-Liar paper divergences pin to the documented set", {
  fx <- read_vectors()
  yal <- fx[fx$source == "youarealiar", , drop = FALSE]
  expect_gt(nrow(yal), 0)

  # Curated headline divergences from Ajmani et al. (SecWeb'22), Section V/VI
  # (class C; bytes verified against wspr-ncsu/urlparsing-framework, BSD-3). The
  # oracle is the paper's `whatwg-url` reference column. rurl reproduces both
  # sides of the SOP equivocation across profiles and agrees with the WHATWG
  # reference on the plain hostname-confusion rows; it diverges on 3, each
  # triaged in the divergence ledger:
  #   yal-009 `www.php.net:80/...` -- WHATWG reads a dotted `www.php.net` as a
  #     SCHEME (dots are legal scheme code points) with an opaque path; rurl
  #     rejects. A genuine acceptance-level deviation, filed against the parser.
  # yal-005 (non-ASCII host) and yal-008 (`foo://`) NO LONGER diverge: both were
  # surface artifacts (RURL-yeikpnan). Surface (c) keeps the host reversibly
  # Unicode by ADR 0002 and declines `foo:` by the ADR 0004 closed scheme set;
  # the standard serializer emits the punycode host and accepts the non-special
  # scheme, which is what the paper's WHATWG reference does.
  # yal-002/003 (control chars in the authority) NO LONGER diverge: the whatwg
  # profile now strips ASCII tab/CR/LF before parsing (RURL-tyetpjym), matching
  # WHATWG, and surfaces the mutation via the `control-char-stripped` diagnostic
  # (asserted below). rfc3986 still rejects them (RFC has no strip step).
  diverging_ids <- yal$id[yal$diverges == "yes"]
  expect_setequal(diverging_ids, "yal-009")

  # The two control-character rows now PARSE under whatwg (tab/CR/LF stripped)
  # but still REJECT under rfc3986 (which requires percent-encoding). The whatwg
  # parse must fire the `control-char-stripped` diagnostic on both.
  ctrl <- yal[yal$id %in% c("yal-002", "yal-003"), , drop = FALSE]
  expect_true(all(ctrl$rurl_rfc_status == "error"))
  expect_true(all(is.na(ctrl$fsss_rfc_normalized)))
  expect_false(anyNA(ctrl$fsss_whatwg))
  ctrl_diag <- get_url_diagnostics(ctrl$input, url_standard = "whatwg")
  expect_true(all(vapply(
    ctrl_diag, function(d) "control-char-stripped" %in% d, logical(1)
  )))

  # Non-diverging hostname rows: the host rurl actually parsed must EQUAL the
  # WHATWG-reference host.
  #
  # This used to ask `grepl(host, clean_url, fixed = TRUE)` -- substring
  # containment -- which is the wrong question on a hostname-confusion corpus
  # (RURL-yeikpnan). Every row here is an input where two parsers disagree about
  # WHICH of two hostnames is the authority, and both names appear in the
  # string: `http://letsencrypt.org%2F@malware.testing.google.test/` contains
  # "malware.testing.google.test" whether it is parsed as the host or as part of
  # the userinfo. Containment passes either way, so it could not detect the
  # confusion the corpus exists to detect. `fsss_host` is the extracted host.
  host_rows <- yal[yal$diverges == "no" & yal$oracle_kind == "host", ,
    drop = FALSE]
  expect_gt(nrow(host_rows), 0)
  expect_false(anyNA(host_rows$fsss_host))
  expect_identical(host_rows$fsss_host, host_rows$oracle_value)
})

test_that("Equivocal URLs paper divergences pin to the documented set", {
  fx <- read_vectors()
  eq <- fx[fx$source == "equivocal-urls", , drop = FALSE]
  expect_gt(nrow(eq), 0)

  # Reynolds et al. (ESORICS'22), Table 3 + Section 6 (class C; no artifact was
  # released, hand-transcribed from the PDF text layer). Oracle = the paper's
  # `NodeJS WHATWG` reference column. These URLs are equivocal by design (>=2
  # DNS-compatible hosts). rurl reproduces both options across profiles and
  # agrees with the WHATWG reference on all but 1 row:
  #   eq-U8 İ@ -- rurl takes the clean userinfo parse (host e.gg) vs the paper's
  #     dotted-İ host folding (intentional parser-boundary; RURL-ajnnjzgs).
  # eq-U6 (LF in host) NO LONGER diverges: the whatwg profile now strips the
  # ASCII newline before parsing (RURL-tyetpjym) -> host n.pre.gg, matching the
  # WHATWG reference, with a `control-char-stripped` diagnostic.
  diverging_ids <- eq$id[eq$diverges == "yes" & !is.na(eq$diverges)]
  expect_setequal(diverging_ids, "eq-U8")

  # U1 (NUL) and U7 (invalid-UTF-8 octets) are non-runnable provenance rows:
  # no input, no rurl output, no divergence verdict.
  norun <- eq[eq$runnable == "no", , drop = FALSE]
  expect_setequal(norun$id, c("eq-U1", "eq-U7"))
  expect_true(all(is.na(norun$input)))
  expect_true(all(is.na(norun$rurl_whatwg_status)))
  expect_true(all(is.na(norun$diverges)))

  # Headline result, RE-POINTED by RURL-qrfrvmkg. rurl used to reproduce BOTH
  # sides of the backslash equivocation across profiles: whatwg took Option B
  # (the browser host) and rfc3986 took Option A (the lenient-RFC-parser host,
  # `\` swallowed into userinfo). The paper's own RFC column is `ERR`, and the
  # audited oracle (RURL-nknytzxz) records `failure` on both rows -- "\" is in
  # none of unreserved / pct-encoded / sub-delims / pchar, so no production
  # admits it. Option A was never what the STANDARD says; it was what the
  # PERMISSIVE parsers (urllib, uri-js, php, curl) do, and rurl reproduced it
  # only because the generic gate did not bind on the libcurl route. The
  # equivocation is still demonstrated -- one profile resolves a host, the other
  # refuses the string -- but the rfc3986 profile now answers as a STRICT RFC
  # 3986 parser (Ruby's URI::RFC3986_Parser), not as a lenient one.
  u2 <- eq[eq$id == "eq-U2", ]
  expect_identical(u2$fsss_host, "n.pr")   # Option B
  expect_identical(u2$rurl_rfc_status, "error")                    # paper: ERR
  expect_true(is.na(u2$fsss_rfc_normalized))
  bs <- eq[eq$id == "eq-bs", ]
  # whatwg -> browser side (malware host); rfc3986 -> refuses the string, so
  # the GSB-evasion payoff still holds: the two profiles do not agree, and the
  # classifier's lenient reading (letsencrypt.org) is NOT what a conformant RFC
  # 3986 parser returns.
  # Extracted host, not containment: both hostnames appear in this string, so
  # containment cannot tell a correct parse from an inverted one.
  expect_identical(bs$fsss_host, "malware.testing.google.test")
  expect_identical(bs$rurl_rfc_status, "error")
  expect_true(is.na(bs$fsss_rfc_normalized))
})

test_that("Ada verify_dns_length: rurl accepts, host-length probe matches", {
  fx <- read_vectors()
  dl <- fx[fx$source == "ada-verifydnslength", , drop = FALSE]
  expect_gt(nrow(dl), 0)

  # These test Ada's OPTIONAL verify_dns_length mode (RFC 1035), NOT the core
  # WHATWG URL Standard (which runs UTS-46 with VerifyDnsLength=false and so
  # accepts over-length / empty-label hosts). rurl is WHATWG-URL-conformant: it
  # accepts every one -> no divergence from the standard oracle.
  expect_true(all(dl$diverges == "no"))
  expect_false(anyNA(dl$fsss_whatwg))

  # The payoff: rurl's host-length probe (RURL-vowqpmdg / T5) surfaces the RFC
  # 1035 violation as a diagnostic FACT (ADR 0006). It must fire EXACTLY on the
  # rows Ada's verify_dns_length rejects, and stay silent on the ok rows.
  dns_diag <- c("domain-label-too-long", "domain-name-too-long",
    "domain-empty-label")
  diag <- get_url_diagnostics(dl$input, url_standard = "whatwg")
  probe_fires <- vapply(diag, function(d) length(intersect(d, dns_diag)) > 0L,
    logical(1))
  ada_rejects <- grepl("failure", dl$paper_claimed_behavior, fixed = TRUE)
  expect_identical(probe_fires, ada_rejects)
})

test_that("credential and fragment vectors match the WHATWG serializer", {
  # RURL-yeikpnan: the slice that closes the corpus SHAPE gap. Before it, ZERO
  # expected values in this corpus carried a fragment or credentials -- because
  # those are exactly the rows a `clean_url` comparison could never have passed
  # (surface (c) drops both by contract), so the corpus had grown into the shape
  # its harness could score.
  #
  # Oracle: the WPT import's recorded components (inst/bench/wpt-url-cases.json,
  # BSD-3-Clause), assembled by the WHATWG URL serializer (URL Standard §4.5).
  # Built by _scratch/build-credential-fragment-vectors.R.
  fx <- read_vectors()
  cf <- fx[fx$source == "wpt-credentials-fragments", , drop = FALSE]
  expect_gt(nrow(cf), 40)
  expect_true(all(cf$runnable == "yes"))
  expect_true(all(cf$oracle_kind == "exact"))

  # Every row reproduces the standard's serialization exactly.
  expect_identical(cf$fsss_whatwg, cf$standard_expectation)
  expect_setequal(cf$id[cf$diverges == "yes"], character(0))

  # The shapes that were missing are actually present, not merely counted.
  shapes <- cf$notes
  expect_gt(sum(grepl("credentials", shapes, fixed = TRUE)), 20)
  expect_gt(sum(grepl("fragment", shapes, fixed = TRUE)), 10)

  # P2.5 open question 1, settled by evidence rather than by argument. WHATWG
  # "includes credentials" is FALSE when both halves are empty, so the userinfo
  # is dropped delimiter and all; a non-empty half survives. These rows come
  # from WPT's own component expectations, so they are the standard's answer,
  # not rurl's.
  empty_cred <- cf[grepl("empty-credentials", cf$notes, fixed = TRUE), ,
    drop = FALSE]
  expect_gt(nrow(empty_cred), 0)
  expect_false(any(grepl("@", empty_cred$fsss_host, fixed = TRUE)))
  expect_false(any(grepl(
    "@", sub("^[a-z]+://", "", empty_cred$fsss_whatwg), fixed = TRUE
  )))
})
