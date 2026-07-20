#!/usr/bin/env Rscript
# RFC 3986 conformance-oracle audit (RURL-nknytzxz).
#
# NOT a test and NOT shipped in the package. This is the repeatable derivation
# behind the `rfc3986_expected` column of
# tests/testthat/fixtures/external-url-vectors.csv.
#
#   Rscript tools/oracle-audit-rfc3986.R
#
# WHY IT EXISTS
# -------------
# 202 of the fixture's oracle-carrying rows are transcribed from the WHATWG
# web-platform-tests. WPT is a *WHATWG* oracle: its must-fail expectations say
# what the WHATWG URL parser rejects, which is NOT the same question as what
# RFC 3986 rejects. Copying those expectations into a column labelled
# `rfc3986_expected` creates a CO-CONFIRMATION TRAP: the oracle and rurl agree
# with each other, the test passes, and the fact that both disagree with the
# actual RFC goes unseen.
#
# The audit answers a question the fixture cannot currently express -- for each
# row, is a rejection required BY THE STANDARD, or is it rurl declining BY
# POLICY (ADR 0004 host-shape gate, the closed scheme set, D1/D2 host gates)?
# Both currently collapse into `rfc3986_expected = "failure"`.
#
# THE FOUR VERDICTS PER ROW
# -------------------------
#   oracle  what the fixture currently claims RFC 3986 requires
#   rurl    what rurl(url_standard = "rfc3986") actually does now
#   abnf    a direct, self-contained transcription of the RFC 3986 ABNF
#           (Section 3 + Appendix A) -- referee 1
#   ruby    Ruby's URI::RFC3986_Parser -- referee 2, an independent
#           implementation nobody here wrote
#
# Two referees, not one, on purpose: a single referee cannot be distinguished
# from a second opinion of the same mistake. Where the two referees AGREE, the
# syntactic question is settled and any oracle disagreement is a fixture
# defect. Where they DISAGREE, the row is escalated for hand adjudication
# against the RFC text -- the script never picks a winner silently.
#
# READING THE REFEREES (they answer a narrow question)
# ----------------------------------------------------
# Both referees test ONE thing: does the input match the RFC 3986 `URI` rule.
# They are deliberately scheme-agnostic -- they know nothing of RFC 8089's
# narrowing of `file:`, of registrable domains, or of rurl's policy layer. So
# "referee accepts" means "the generic URI grammar admits this string", NOT
# "rurl is wrong to reject it". That gap is exactly the `parser-boundary`
# bucket the fixture already has and does not apply to these rows.
#
# OUTPUT (CSV, into $RURL_AUDIT_OUT, default the gitignored _scratch/) --
# the SCRIPT is the versioned artifact, the tables are regenerated:
#   oracle-audit-rows.csv        one row per audited fixture row, all four
#                                verdicts + construct family + current class
#   oracle-audit-summary.csv     the (oracle, rurl, referee) contingency table
#   oracle-audit-escalations.csv rows where the two referees disagree

suppressWarnings(suppressMessages({
  if (requireNamespace("pkgload", quietly = TRUE) &&
        file.exists("DESCRIPTION")) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(rurl)
  }
}))

out_dir <- Sys.getenv("RURL_AUDIT_OUT", unset = "_scratch")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

fixture_path <- "tests/testthat/fixtures/external-url-vectors.csv"

# ---------------------------------------------------------------------------
# Referee 1: the RFC 3986 ABNF, transcribed.
# ---------------------------------------------------------------------------
# Built bottom-up from Appendix A so each production is checkable against the
# RFC by eye. Anchored, ASCII-only, applied with useBytes = TRUE: the grammar
# admits no byte outside its terminals, so any raw control character, space, or
# non-ASCII octet fails structurally rather than by a special case.

`%+%` <- function(a, b) paste0(a, b)
grp <- function(x) "(?:" %+% x %+% ")"
opt <- function(x) grp(x) %+% "?"
rep_n <- function(x, n) grp(x) %+% "{" %+% n %+% "}"

abnf <- local({
  alpha <- "[A-Za-z]"
  digit <- "[0-9]"
  hexdig <- "[0-9A-Fa-f]"
  unreserved <- "[A-Za-z0-9._~-]"
  sub_delims <- "[!$&'()*+,;=]"
  pct <- "%" %+% hexdig %+% hexdig

  # pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
  pchar <- grp(unreserved %+% "|" %+% pct %+% "|" %+% sub_delims %+% "|[:@]")

  # dec-octet = "0"-"9" / "10"-"99" / "100"-"199" / "200"-"249" / "250"-"255"
  dec_octet <- grp(paste(
    "25[0-5]", "2[0-4][0-9]", "1[0-9][0-9]", "[1-9][0-9]", "[0-9]",
    sep = "|"
  ))
  ipv4 <- rep_n(dec_octet %+% "\\.", 3) %+% dec_octet

  h16 <- hexdig %+% "{1,4}"
  ls32 <- grp(h16 %+% ":" %+% h16 %+% "|" %+% ipv4)
  # The nine IPv6address alternatives, in Appendix A order.
  ipv6 <- grp(paste(
    rep_n(h16 %+% ":", 6) %+% ls32,
    "::" %+% rep_n(h16 %+% ":", 5) %+% ls32,
    opt(h16) %+% "::" %+% rep_n(h16 %+% ":", 4) %+% ls32,
    opt(grp(h16 %+% ":") %+% "{0,1}" %+% h16) %+% "::" %+%
      rep_n(h16 %+% ":", 3) %+% ls32,
    opt(grp(h16 %+% ":") %+% "{0,2}" %+% h16) %+% "::" %+%
      rep_n(h16 %+% ":", 2) %+% ls32,
    opt(grp(h16 %+% ":") %+% "{0,3}" %+% h16) %+% "::" %+% h16 %+% ":" %+%
      ls32,
    opt(grp(h16 %+% ":") %+% "{0,4}" %+% h16) %+% "::" %+% ls32,
    opt(grp(h16 %+% ":") %+% "{0,5}" %+% h16) %+% "::" %+% h16,
    opt(grp(h16 %+% ":") %+% "{0,6}" %+% h16) %+% "::",
    sep = "|"
  ))
  ipvfuture <- "v" %+% hexdig %+% "+\\." %+%
    grp(unreserved %+% "|" %+% sub_delims %+% "|:") %+% "+"
  ip_literal <- "\\[" %+% grp(ipv6 %+% "|" %+% ipvfuture) %+% "\\]"

  # reg-name = *( unreserved / pct-encoded / sub-delims )  -- may be EMPTY, and
  # is NOT decoded for validity (S3.2.2). IPv4address is syntactically a subset
  # of reg-name, so listing it separately changes nothing about acceptance.
  reg_name <- grp(unreserved %+% "|" %+% pct %+% "|" %+% sub_delims) %+% "*"
  host <- grp(ip_literal %+% "|" %+% ipv4 %+% "|" %+% reg_name)
  # userinfo may also be empty; port = *DIGIT, so ":" with no digits is legal.
  userinfo <- grp(unreserved %+% "|" %+% pct %+% "|" %+% sub_delims %+%
    "|:") %+% "*"
  authority <- opt(userinfo %+% "@") %+% host %+% opt(":" %+% digit %+% "*")

  segment <- pchar %+% "*"
  segment_nz <- pchar %+% "+"
  # (segment-nz-nc is deliberately absent: path-noscheme only occurs in
  # relative-part, and the `URI` rule this matcher tests has no relative form.)
  path_abempty <- grp("/" %+% segment) %+% "*"
  path_absolute <- "/" %+% opt(segment_nz %+% grp("/" %+% segment) %+% "*")
  path_rootless <- segment_nz %+% grp("/" %+% segment) %+% "*"
  path_empty <- ""

  hier_part <- grp(paste(
    "//" %+% authority %+% path_abempty,
    path_absolute,
    path_rootless,
    path_empty,
    sep = "|"
  ))
  scheme <- alpha %+% "[A-Za-z0-9+.-]*"
  qf <- grp(pchar %+% "|[/?]") %+% "*"

  "^" %+% scheme %+% ":" %+% hier_part %+% opt("\\?" %+% qf) %+%
    opt("#" %+% qf) %+% "$"
})

abnf_accepts <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA)
    # The grammar's terminals are all printable ASCII; anything else (raw C0,
    # DEL, space, any non-ASCII octet) has no production and fails here.
    if (grepl("[^\\x21-\\x7e]", s, perl = TRUE, useBytes = TRUE)) return(FALSE)
    grepl(abnf, s, perl = TRUE, useBytes = TRUE)
  }, logical(1), USE.NAMES = FALSE)
}

# ---------------------------------------------------------------------------
# Referee 2: Ruby's URI::RFC3986_Parser.
# ---------------------------------------------------------------------------
# Fed the JSON-escaped inputs so the adversarial bytes (raw CR/LF, C0 controls,
# non-ASCII) survive the hand-off exactly; Ruby decodes them itself.

ruby_accepts <- function(input_json) {
  ruby <- Sys.which("ruby")
  if (!nzchar(ruby)) {
    warning("ruby not found; referee 2 unavailable", call. = FALSE)
    return(rep(NA, length(input_json)))
  }
  in_file <- file.path(tempdir(), "oracle-audit-inputs.json")
  writeLines(input_json, in_file, useBytes = TRUE)
  # `split`, not `parse`, and a non-nil scheme:
  #   * `parse` builds a scheme-specific class and applies ITS rules, so it
  #     rejects `mailto://:443` on RFC 6068 grounds -- scheme knowledge, not
  #     RFC 3986 syntax. `split` is the pure-grammar entry point.
  #   * `split` matches URI-REFERENCE, so it accepts relative references
  #     (`a`, `a/`, `#`, ``). The fixture's rows are absolute-URL inputs and
  #     referee 1 tests the `URI` rule, so requiring a scheme keeps both
  #     referees on the same question.
  script <- '
    require "uri"
    require "json"
    STDIN.each_line do |line|
      s = JSON.parse(line.chomp, quirks_mode: true)
      begin
        comps = URI::RFC3986_PARSER.split(s)
        puts comps[0].nil? ? "reject" : "accept"
      rescue StandardError
        puts "reject"
      end
    end
  '
  res <- system2(ruby, c("-e", shQuote(script)), stdin = in_file,
    stdout = TRUE, stderr = FALSE)
  if (length(res) != length(input_json)) {
    stop("ruby referee returned ", length(res), " verdicts for ",
      length(input_json), " inputs")
  }
  res == "accept"
}

# ---------------------------------------------------------------------------
# Construct families.
# ---------------------------------------------------------------------------
# Deciding ~200 rows one at a time is slow and error-prone; deciding them by
# construct family against one cited clause is neither. The families are named
# for the RFC production actually in dispute, so each group maps to a clause.

classify_family <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    if (grepl("[^\\x20-\\x7e]", s, perl = TRUE, useBytes = TRUE)) {
      return("non-ascii-or-control")
    }
    if (grepl("^[^:/?#]*:", s) && grepl("^[^A-Za-z]", s)) {
      return("scheme-shape")
    }
    auth <- sub("^[^:]*://", "", s)
    auth <- sub("[/?#].*$", "", auth)
    if (grepl("\\", s, fixed = TRUE)) return("backslash")
    if (grepl("^\\[|\\]", auth)) return("ip-literal")
    if (grepl("^//", sub("^[^:]*:", "", s)) && !nzchar(sub(".*@", "", auth))) {
      return("empty-host")
    }
    if (grepl("@", auth, fixed = TRUE)) return("userinfo")
    if (grepl(":", auth, fixed = TRUE) && !grepl(":[0-9]*$", auth)) {
      return("port-shape")
    }
    if (grepl("%", auth, fixed = TRUE)) return("pct-encoded-reg-name")
    if (grepl("[^A-Za-z0-9._~%!$&'()*+,;=:@-]", auth)) return("host-charset")
    if (grepl("^//", sub("^[^:]*:", "", s))) return("reg-name-other")
    "no-authority"
  }, character(1), USE.NAMES = FALSE)
}

# ---------------------------------------------------------------------------
# Run.
# ---------------------------------------------------------------------------

fx <- utils::read.csv(fixture_path, stringsAsFactors = FALSE,
  colClasses = "character", na.strings = "NA", encoding = "UTF-8")
runnable <- fx$runnable == "yes"
fx$input[runnable] <- vapply(fx$input_json[runnable], jsonlite::fromJSON,
  character(1), USE.NAMES = FALSE)

aud <- fx[runnable & !is.na(fx$rfc3986_expected), , drop = FALSE]
message("auditing ", nrow(aud), " runnable rows carrying an rfc3986 oracle (",
  sum(aud$source == "wpt-urltestdata"), " WPT-sourced)")

parsed <- safe_parse_urls(aud$input, url_standard = "rfc3986")

verdict <- function(ok) {
  c("reject", "accept")[as.integer(ok) + 1L]
}

aud$v_oracle <- ifelse(aud$rfc3986_expected == "failure", "reject", "accept")
aud$v_rurl <- verdict(parsed$parse_status != "error")
aud$v_abnf <- verdict(abnf_accepts(aud$input))
aud$v_ruby <- verdict(ruby_accepts(aud$input_json))
aud$family <- classify_family(aud$input)
aud$referees_agree <- aud$v_abnf == aud$v_ruby
# The defect signature: the fixture claims the STANDARD rejects, while both
# independent readings of the standard accept. rurl's own verdict is
# deliberately not part of this test -- rurl may still decline by policy.
aud$oracle_suspect <- aud$v_oracle == "reject" & aud$referees_agree &
  aud$v_abnf == "accept"

keep <- c("id", "source", "input_json", "family", "v_oracle", "v_rurl",
  "v_abnf", "v_ruby", "referees_agree", "oracle_suspect", "divergence_class",
  "oracle_ref", "rfc3986_expected", "whatwg_expected")
rows <- aud[, keep, drop = FALSE]

summ <- as.data.frame(table(
  oracle = aud$v_oracle, rurl = aud$v_rurl, abnf = aud$v_abnf,
  ruby = aud$v_ruby
), stringsAsFactors = FALSE)
summ <- summ[summ$Freq > 0, , drop = FALSE]
summ <- summ[order(-summ$Freq), , drop = FALSE]

esc <- rows[!rows$referees_agree, , drop = FALSE]

utils::write.csv(rows, file.path(out_dir, "oracle-audit-rows.csv"),
  row.names = FALSE)
utils::write.csv(summ, file.path(out_dir, "oracle-audit-summary.csv"),
  row.names = FALSE)
utils::write.csv(esc, file.path(out_dir, "oracle-audit-escalations.csv"),
  row.names = FALSE)

cat("\n== four-way verdict table ==\n")
print(summ, row.names = FALSE)
cat("\n== suspect rows (oracle says the STANDARD rejects; both referees",
  "accept) ==\n")
print(table(family = rows$family[rows$oracle_suspect],
  rurl = rows$v_rurl[rows$oracle_suspect]))
cat("\nsuspect total:", sum(rows$oracle_suspect), "of", nrow(rows), "\n")
cat("referee disagreements needing hand adjudication:", nrow(esc), "\n")
cat("written to", normalizePath(out_dir), "\n")
