# A direct transcription of the RFC 3986 ABNF (Section 3 + Appendix A) --
# an oracle for the oracle.
#
# WHY THIS EXISTS (RURL-nknytzxz). The `rfc3986_expected` column of
# fixtures/external-url-vectors.csv was transcribed from the WHATWG
# web-platform-tests, whose must-fail expectations answer "what does the WHATWG
# parser reject" -- a different question from "what does RFC 3986 reject". On
# every row where rurl ALSO rejected, the oracle and the implementation
# confirmed each other, the conformance test passed, and the fact that both
# disagreed with the RFC stayed invisible. 75 rows sat in that trap.
#
# A fixture cell cannot be checked by the parser it exists to check. This
# matcher is the independent third opinion that breaks the loop: it is derived
# from the RFC's grammar alone and shares no code with rurl, so
# `oracle-vs-grammar` (below) can assert that the fixture's claims about the
# STANDARD are true of the standard. It is deliberately scheme-agnostic --
# it knows nothing of RFC 8089, of registrable domains, or of rurl's policy
# layer, so "accepts" means only "the generic URI grammar admits this string",
# never "rurl is wrong to reject it".
#
# Cross-checked against Ruby's URI::RFC3986_Parser over all 282 runnable
# fixture rows; the two agree on every one (tools/oracle-audit-rfc3986.R).
# Ruby is not available in CI, which is why the grammar is transcribed here
# rather than shelled out to.

`%+%` <- function(a, b) paste0(a, b)
grp <- function(x) "(?:" %+% x %+% ")"
opt <- function(x) grp(x) %+% "?"
rep_n <- function(x, n) grp(x) %+% "{" %+% n %+% "}"

.rfc3986_abnf_re <- local({
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

rfc3986_abnf_accepts <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA)
    # The grammar's terminals are all printable ASCII; anything else (raw C0,
    # DEL, space, any non-ASCII octet) has no production and fails here.
    if (grepl("[^\\x21-\\x7e]", s, perl = TRUE, useBytes = TRUE)) return(FALSE)
    grepl(.rfc3986_abnf_re, s, perl = TRUE, useBytes = TRUE)
  }, logical(1), USE.NAMES = FALSE)
}
