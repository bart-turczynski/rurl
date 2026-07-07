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
# `divergence_class` buckets each row (aligned / spec-divergent /
# parser-boundary / both-reject / not-runnable). This is orthogonal to
# `diverges` (which flags rurl vs the row's SOURCE oracle): `divergence_class`
# is standard-vs-standard.
# The `aligned` bucket is deliberately un-cited (tiered): where RFC, WHATWG, and
# rurl (both profiles) all agree, a self-consistency invariant stands in for a
# hand-derived oracle, so citation effort concentrates on the diverging rows.

fixture_path <- testthat::test_path("fixtures", "external-url-vectors.csv")

read_vectors <- function() {
  utils::read.csv(
    fixture_path, stringsAsFactors = FALSE, colClasses = "character",
    na.strings = "NA"
  )
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
      # Part 3a dual-standard expected-outcome oracle (RURL-moselrwp).
      "rfc3986_expected", "whatwg_expected", "oracle_ref", "divergence_class")
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

# Documented rurl deviations from the spec oracle (parser boundaries, each
# backed by an ADR / tracked ticket in oracle_ref). For these rows rurl's
# behavior is pinned by the per-source characterization tests below and is NOT
# expected to equal the spec oracle -- so the conformance check skips them.
oracle_nonconformance_ids <- function() {
  c(
    # closed scheme set (ADR 0004) -- specs accept, rurl rejects
    "ada-007", "ada-012", "ada-013", "ada-014", "ada-018", "ada-019",
    "ada-021", "ada-023", "yal-008",
    # reversible Unicode host default (ADR 0002)
    "ada-006", "yal-005",
    # readable-path default (RURL-ndrgrwcz)
    "ada-003", "ada-022",
    # scheme inference / UTS-46 case fold / IPv6 re-serialization
    "yal-009", "eq-U8", "ipobf-019", "ipobf-020",
    # libcurl host allowed-set boundary (RURL-dxwxeamq / ada-008)
    "ada-008"
  )
}

test_that("dual-standard oracle: classes and invariants hold", {
  fx <- read_vectors()

  # every row carries a class; the classes are the closed set.
  expect_false(anyNA(fx$divergence_class))
  expect_true(all(fx$divergence_class %in% c(
    "aligned", "spec-divergent", "parser-boundary", "both-reject",
    "not-runnable"
  )))

  # spec-divergent <=> the two standards' expected outcomes differ (both cited).
  sd <- fx$divergence_class == "spec-divergent"
  expect_true(all(fx$rfc3986_expected[sd] != fx$whatwg_expected[sd]))
  expect_false(anyNA(fx$rfc3986_expected[sd]))
  expect_false(anyNA(fx$whatwg_expected[sd]))
  expect_false(anyNA(fx$oracle_ref[sd]))

  # both-reject: both oracles are "failure".
  br <- fx$divergence_class == "both-reject"
  expect_true(all(fx$rfc3986_expected[br] == "failure"))
  expect_true(all(fx$whatwg_expected[br] == "failure"))

  # aligned is the TIERED bucket: no per-row oracle, and rurl must be
  # self-consistent (both profiles accept and agree on the clean_url) -- that
  # self-consistency IS the assertion that stands in for a hand-cited oracle.
  al <- fx$divergence_class == "aligned"
  expect_true(all(is.na(fx$rfc3986_expected[al])))
  expect_true(all(is.na(fx$whatwg_expected[al])))
  expect_true(all(is.na(fx$oracle_ref[al])))
  expect_identical(fx$rurl_rfc_clean[al], fx$rurl_whatwg_clean[al])
  expect_false(anyNA(fx$rurl_rfc_clean[al]))

  # not-runnable rows carry no oracle value (source view stays in
  # standard_expectation).
  nr <- fx$divergence_class == "not-runnable"
  expect_true(all(fx$runnable[nr] != "yes"))
  expect_true(all(is.na(fx$rfc3986_expected[nr])))

  # parser-boundary rows all cite the ADR / ticket that justifies the deviation.
  pb <- fx$divergence_class == "parser-boundary"
  expect_false(anyNA(fx$oracle_ref[pb]))
  expect_setequal(fx$id[pb],
    setdiff(oracle_nonconformance_ids(), "ada-008"))
})

test_that("rurl conforms to the spec oracle except at documented boundaries", {
  fx <- read_vectors()
  fx <- fx[fx$runnable == "yes", , drop = FALSE]
  skip_ids <- oracle_nonconformance_ids()
  chk <- fx[!(fx$id %in% skip_ids), , drop = FALSE]

  # helper: does rurl's (status, clean) match a spec oracle cell?
  conforms <- function(oracle, status, clean) {
    ifelse(
      oracle == "failure", status == "error",
      # a concrete clean_url oracle: rurl must reproduce it exactly.
      !is.na(clean) & clean == oracle
    )
  }

  # every checked row's oracle is either "failure" or a concrete clean_url
  # (the "accept" sentinel only appears on skipped boundary rows).
  expect_false(any(chk$rfc3986_expected == "accept", na.rm = TRUE))
  expect_false(any(chk$whatwg_expected == "accept", na.rm = TRUE))

  rfc_ok <- conforms(chk$rfc3986_expected, chk$rurl_rfc_status,
    chk$rurl_rfc_clean)
  wha_ok <- conforms(chk$whatwg_expected, chk$rurl_whatwg_status,
    chk$rurl_whatwg_clean)
  # NA oracle (aligned rows) is covered by the invariant test above; only assert
  # where an oracle value exists.
  rfc_ok[is.na(chk$rfc3986_expected)] <- TRUE
  wha_ok[is.na(chk$whatwg_expected)] <- TRUE

  expect_true(all(rfc_ok),
    info = paste("RFC oracle mismatch:", toString(chk$id[!rfc_ok])))
  expect_true(all(wha_ok),
    info = paste("WHATWG oracle mismatch:", toString(chk$id[!wha_ok])))
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
  expect_true(all(is.na(wpt$rurl_whatwg_clean)))
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
  expect_identical(ok_rows$rurl_whatwg_clean, ok_rows$standard_expectation)
  fail_rows <- ip[ip$standard_expectation == "failure", , drop = FALSE]
  expect_true(all(is.na(fail_rows$rurl_whatwg_clean)))
})

test_that("Ada extra-urltestdata divergences pin to the documented set", {
  fx <- read_vectors()
  ada <- fx[fx$source == "ada-extra-urltestdata" & fx$runnable == "yes", ,
    drop = FALSE]

  # Ada's beyond-WPT vectors (Apache-2.0), oracle = WHATWG. rurl agrees on every
  # must-fail row (rejecting all of them, sometimes for a stricter reason such
  # as the closed scheme set) and on the `http://./` accept. It diverges on 12
  # rows, each triaged in the divergence ledger: 8 closed-scheme rejections and
  # `a:b#` (boundary-by-design, ADR 0004 -- only http/https/ftp(s) supported),
  # the IDNA host `Yağız.com` (boundary-by-design, ADR 0002 -- Unicode host kept
  # reversible, punycode is a separate phase), the backslash-normalization row
  # `http://///\\'` (candidate-bug -- WHATWG maps \\ to / for special schemes),
  # and two path percent-encoding rows (needs-investigation). WATCH list, not an
  # approval list.
  diverging_ids <- ada$id[ada$diverges == "yes"]
  expect_setequal(
    diverging_ids,
    c("ada-003", "ada-006", "ada-007", "ada-008", "ada-012", "ada-013",
      "ada-014", "ada-018", "ada-019", "ada-021", "ada-022", "ada-023")
  )
  # Every non-diverging Ada row: accept rows must equal Ada's href, failure rows
  # must genuinely reject.
  ok_rows <- ada[ada$diverges == "no" & ada$standard_expectation != "failure", ,
    drop = FALSE]
  expect_identical(ok_rows$rurl_whatwg_clean, ok_rows$standard_expectation)
  fail_rows <- ada[ada$standard_expectation == "failure", , drop = FALSE]
  expect_true(all(is.na(fail_rows$rurl_whatwg_clean)))
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
  # triaged in the divergence ledger (NOT blessed):
  #   yal-005 non-ASCII host -- kept reversibly Unicode (boundary, ADR 0002);
  #   yal-008 `foo://` -- outside the closed scheme set (boundary, ADR 0004);
  #   yal-009 `www.php.net:80/...` -- rurl infers http + host vs WHATWG scheme
  #     parse (needs-investigation).
  # yal-002/003 (control chars in the authority) NO LONGER diverge: the whatwg
  # profile now strips ASCII tab/CR/LF before parsing (RURL-tyetpjym), matching
  # WHATWG, and surfaces the mutation via the `control-char-stripped` diagnostic
  # (asserted below). rfc3986 still rejects them (RFC has no strip step).
  diverging_ids <- yal$id[yal$diverges == "yes"]
  expect_setequal(
    diverging_ids,
    c("yal-005", "yal-008", "yal-009")
  )

  # The two control-character rows now PARSE under whatwg (tab/CR/LF stripped)
  # but still REJECT under rfc3986 (which requires percent-encoding). The whatwg
  # parse must fire the `control-char-stripped` diagnostic on both.
  ctrl <- yal[yal$id %in% c("yal-002", "yal-003"), , drop = FALSE]
  expect_true(all(ctrl$rurl_rfc_status == "error"))
  expect_true(all(is.na(ctrl$rurl_rfc_clean)))
  expect_false(anyNA(ctrl$rurl_whatwg_clean))
  ctrl_diag <- get_url_diagnostics(ctrl$input, url_standard = "whatwg")
  expect_true(all(vapply(
    ctrl_diag, function(d) "control-char-stripped" %in% d, logical(1)
  )))

  # Non-diverging hostname rows: rurl's WHATWG clean_url must contain the
  # WHATWG-reference host. The oracle is spelled either `host=X` (rows where
  # rurl's exact serialization is the point) or `accept:host=X (why)` (the
  # control-char rows, where the point is accept-not-reject).
  host_rows <- yal[yal$diverges == "no", , drop = FALSE]
  for (i in seq_len(nrow(host_rows))) {
    exp <- host_rows$standard_expectation[i]
    expect_true(grepl("^(accept:)?host=", exp))
    host <- sub("^(accept:)?host=", "", exp)
    host <- sub(" \\(.*\\)$", "", host)
    expect_false(is.na(host_rows$rurl_whatwg_clean[i]))
    expect_true(grepl(host, host_rows$rurl_whatwg_clean[i], fixed = TRUE))
  }
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
  #     dotted-İ host folding (defensible; needs-investigation).
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

  # Headline result: rurl reproduces BOTH sides of the hostname equivocation
  # across profiles (whatwg vs rfc) for the backslash row and the GSB
  # web-interface evasion row.
  u2 <- eq[eq$id == "eq-U2", ]
  expect_true(grepl("n.pr", u2$rurl_whatwg_clean, fixed = TRUE))   # Option B
  expect_true(grepl("e.gg", u2$rurl_rfc_clean, fixed = TRUE))      # Option A
  bs <- eq[eq$id == "eq-bs", ]
  # whatwg -> browser side (malware host); rfc -> classifier side (letsencrypt).
  expect_true(grepl("malware.testing.google.test", bs$rurl_whatwg_clean,
    fixed = TRUE))
  expect_true(grepl("letsencrypt.org", bs$rurl_rfc_clean, fixed = TRUE))
  expect_false(grepl("malware", bs$rurl_rfc_clean, fixed = TRUE))
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
  expect_false(anyNA(dl$rurl_whatwg_clean))

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
