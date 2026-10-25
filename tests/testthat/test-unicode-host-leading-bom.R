# RURL-tbmnnauj: `host_encoding = "unicode"` and a host that starts with U+FEFF.
#
# The Unicode rendering splits the host into labels with `.split_host_labels()`
# (R/domain.R), decodes each A-label and rejoins them. The split is a stringi
# call, and stringi skips a U+FEFF that starts its input (ADR 0005, the U+FEFF
# amendment), so the first label lost the mark and a mark-led `xn--` label was
# read as an A-label. The rfc3986 arm keeps a raw non-ASCII host as written
# (the RFC 3986 reg-name, section 3.2.2, admits only its percent-encoded
# form), and the mark is no exception; a label that starts with it does not
# start with the ACE prefix (RFC 5890 section 2.3.2.1), so it is not an
# A-label and keeps its spelling. Each U+FEFF row has a U+200B twin: a
# zero-width code point stringi does not skip, so a twin that already gives
# the right answer shows the defect is the mark's alone.

bom <- "\ufeff"
zwsp <- "\u200b"
beta <- "\u03b2\u03cc\u03bb\u03bf\u03c3" # the U-label of xn--nxasmq6b

# The parsed record under one arm. `NULL` omits the selector, as the default
# call does; the NULL test below also passes it explicitly.
arm_parse <- function(url, arm, ...) {
  if (is.null(arm)) {
    safe_parse_url(url, ...)
  } else {
    safe_parse_url(url, url_standard = arm, ...)
  }
}

arms <- list(whatwg = "whatwg", rfc3986 = "rfc3986", "NULL" = NULL)

# RUL-030 (RURL-pntbujzc): under rfc3986 every host below that carries
# U+FEFF, U+200B or a U-label is a parse error, not a reg-name kept as
# written: RFC 3986 section 2 bounds a URI to US-ASCII, section 2.5 writes
# non-ASCII data as percent-encoded UTF-8, and section 3.2.2's reg-name is
# ASCII. Each rfc3986 pin that used to read the kept host now reads that
# error, under every host_encoding; the NULL arm keeps the rendering pins.
expect_rfc3986_rejects <- function(u, ...) {
  for (enc in c("unicode", "keep", "idna")) {
    expect_null(safe_parse_url(u, url_standard = "rfc3986",
                               host_encoding = enc), label = paste(u, enc))
    expect_identical(
      safe_parse_urls(u, url_standard = "rfc3986",
                      host_encoding = enc)$parse_status,
      "error", label = paste(u, enc)
    )
  }
}

# --- Pins: behavior the fix must not move -------------------------------------

test_that("the split keeps the label count, root dot included", {
  # The reason `.split_host_labels()` takes stringi over `strsplit()`
  # (RURL-eikgtrqf): a root-dot FQDN keeps its trailing empty label.
  expect_identical(.split_host_labels("example.com.")[[1L]],
                   c("example", "com", ""))
  expect_identical(.split_host_labels("a..b")[[1L]], c("a", "", "b"))
  expect_identical(.split_host_labels(".")[[1L]], c("", ""))
  expect_identical(.split_host_labels("")[[1L]], "")
  expect_identical(.split_host_labels(NA_character_)[[1L]], NA_character_)
  expect_identical(
    .split_host_labels(c("a.b", "c.d.", NA, "e")),
    list(c("a", "b"), c("c", "d", ""), NA_character_, "e")
  )
})

test_that("the U+200B twins and a mark not first already keep the code point", {
  expect_identical(
    .split_host_labels(paste0(zwsp, "xn--nxasmq6b.example"))[[1L]],
    c(paste0(zwsp, "xn--nxasmq6b"), "example")
  )
  expect_identical(.split_host_labels(paste0(zwsp, "a.b."))[[1L]],
                   c(paste0(zwsp, "a"), "b", ""))
  expect_identical(.split_host_labels(zwsp)[[1L]], zwsp)
  expect_identical(
    .split_host_labels(paste0("a.", bom, "b"))[[1L]],
    c("a", paste0(bom, "b"))
  )
  expect_identical(
    .punycode_to_unicode_vec(c(
      paste0(zwsp, "xn--nxasmq6b.example"),
      paste0("xn--nxasmq6b.", bom, "xn--nxasmq6b.example"),
      "xn--nxasmq6b.example"
    )),
    c(paste0(zwsp, "xn--nxasmq6b.example"),
      paste0(beta, ".", bom, "xn--nxasmq6b.example"),
      paste0(beta, ".example"))
  )
})

test_that("host_encoding = 'unicode': U+200B twins and negative controls", {
  expect_rfc3986_rejects(paste0("http://", zwsp, "xn--nxasmq6b.example/"))
  expect_rfc3986_rejects(paste0("http://", zwsp, "a.example./"))
  expect_rfc3986_rejects(
    paste0("http://xn--nxasmq6b.", bom, "xn--nxasmq6b.example/")
  )
  # The all-ASCII control still renders under rfc3986.
  expect_identical(
    safe_parse_url("http://xn--nxasmq6b.example/", url_standard = "rfc3986",
                   host_encoding = "unicode")$host,
    paste0(beta, ".example")
  )
  for (an in "NULL") {
    arm <- arms[[an]]
    expect_identical(
      arm_parse(paste0("http://", zwsp, "xn--nxasmq6b.example/"), arm,
                host_encoding = "unicode")$host,
      paste0(zwsp, "xn--nxasmq6b.example"), label = an
    )
    expect_identical(
      arm_parse(paste0("http://", zwsp, "a.example./"), arm,
                host_encoding = "unicode")$host,
      paste0(zwsp, "a.example."), label = an
    )
    expect_identical(
      arm_parse("http://xn--nxasmq6b.example/", arm,
                host_encoding = "unicode")$host,
      paste0(beta, ".example"), label = an
    )
    expect_identical(
      arm_parse(paste0("http://xn--nxasmq6b.", bom, "xn--nxasmq6b.example/"),
                arm, host_encoding = "unicode")$host,
      paste0(beta, ".", bom, "xn--nxasmq6b.example"), label = an
    )
  }
})

test_that("whatwg: UTS #46 maps both marks to nothing, already right", {
  # Domain to ASCII (UTS #46 section 4, mapping step) maps U+FEFF and U+200B
  # to nothing, so the host the rendering sees has no mark and the first
  # label is the A-label. `.split_host_labels()` never sees the mark here.
  for (m in c(bom, zwsp)) {
    for (enc in c("unicode", "keep")) {
      expect_identical(
        arm_parse(paste0("http://", m, "xn--nxasmq6b.example/"), "whatwg",
                  host_encoding = enc)$host,
        paste0(beta, ".example"), label = paste(utf8ToInt(m), enc)
      )
    }
  }
})

test_that("signature: the other encodings and the PSL columns do not move", {
  # Only `host_encoding = "unicode"` renders through the split. "keep"
  # returns the host as parsed, and "idna" encodes the whole label, mark and
  # all; the domain and TLD come from the PSL seam (pslr), not the split.
  # Under rfc3986 the mark-led host is a parse error (see the header), so the
  # "known gap" below -- a domain that is not a suffix of its host -- cannot
  # arise there; the pin stays on the frozen NULL arm.
  expect_rfc3986_rejects(paste0("http://", bom, "xn--nxasmq6b.com/"))
  for (an in "NULL") {
    arm <- arms[[an]]
    u <- paste0("http://", bom, "xn--nxasmq6b.com/")
    k <- arm_parse(u, arm, host_encoding = "keep")
    expect_identical(k$host, paste0(bom, "xn--nxasmq6b.com"), label = an)
    expect_identical(k$clean_url, paste0("http://", bom, "xn--nxasmq6b.com/"),
                     label = an)
    expect_identical(arm_parse(u, arm, host_encoding = "idna")$host,
                     "xn--xn--nxasmq6b-7n50e.com", label = an)
    p <- arm_parse(u, arm, host_encoding = "unicode")
    expect_identical(p$parse_status, "ok", label = an)
    # Known gap, pinned as the ADR 0016 signature: pslr's UTS-46 mapping
    # drops the mark, so `domain` stays the decoded spelling and is no longer
    # a suffix of the mark-led `host` (RURL-tbmnnauj review; not this fix).
    expect_identical(p$domain, paste0(beta, ".com"), label = an)
    expect_identical(p$tld, "com", label = an)
  }
})

# --- Defect: the split read past the mark -------------------------------------

test_that("the split keeps a mark that starts the first label", {
  # stri_split_fixed() skipped the mark, so the first label came back as
  # `xn--nxasmq6b`. The fix splices an ASCII byte on first and cuts it off the
  # first label as a byte; the label count, root dot included, is unchanged.
  expect_identical(
    .split_host_labels(paste0(bom, "xn--nxasmq6b.example"))[[1L]],
    c(paste0(bom, "xn--nxasmq6b"), "example")
  )
  expect_identical(.split_host_labels(paste0(bom, "a.b."))[[1L]],
                   c(paste0(bom, "a"), "b", ""))
  expect_identical(.split_host_labels(bom)[[1L]], bom)
  expect_identical(.split_host_labels(paste0(bom, "."))[[1L]], c(bom, ""))
  expect_identical(.split_host_labels(paste0(bom, bom, "a"))[[1L]],
                   paste0(bom, bom, "a"))
  expect_identical(
    .split_host_labels(c(paste0(bom, "a.b"), "c.d.", NA)),
    list(c(paste0(bom, "a"), "b"), c("c", "d", ""), NA_character_)
  )
})

test_that("a mark-led xn-- label is not decoded as an A-label", {
  # Both decode paths read the label as `xn--nxasmq6b` and decoded it to the
  # U-label, and a plain first label lost the mark. The label starts with
  # U+FEFF, not the ACE prefix (RFC 5890 section 2.3.2.1), so it keeps its
  # spelling, as its U+200B twin does.
  expect_identical(
    .punycode_to_unicode_vec(c(paste0(bom, "xn--nxasmq6b.example"),
                               paste0(bom, "a.example."))),
    c(paste0(bom, "xn--nxasmq6b.example"), paste0(bom, "a.example."))
  )
  double <- function(x, strict) punycoder::puny_decode(x, strict = strict)
  expect_identical(
    .punycode_to_unicode(paste0(bom, "xn--nxasmq6b.example"),
                         decode_fn = double),
    paste0(bom, "xn--nxasmq6b.example")
  )
  expect_rfc3986_rejects(paste0("http://", bom, "xn--nxasmq6b.example/"))
  expect_rfc3986_rejects(paste0("http://", bom, "a.example./"))
  for (arm in list(NULL)) {
    p <- arm_parse(paste0("http://", bom, "xn--nxasmq6b.example/"), arm,
                   host_encoding = "unicode")
    expect_identical(p$host, paste0(bom, "xn--nxasmq6b.example"))
    expect_identical(p$clean_url,
                     paste0("http://", bom, "xn--nxasmq6b.example/"))
    expect_identical(
      arm_parse(paste0("http://", bom, "a.example./"), arm,
                host_encoding = "unicode")$host,
      paste0(bom, "a.example.")
    )
  }
})

test_that("NULL: the unicode rendering keeps the mark (ADR 0016)", {
  # Witness: under the frozen selector the host of
  # `http://\ufeffxn--nxasmq6b.com/` is `\ufeffxn--nxasmq6b.com` (keep, pinned
  # above), and before the fix the unicode rendering returned the U-label
  # `beta.com`: it dropped a code point and decoded a label that is not an
  # A-label. No standard or documented contract sanctions that, so it is a
  # default-path defect, not selector-caused drift. Omitting the selector and
  # passing NULL agree.
  u <- paste0("http://", bom, "xn--nxasmq6b.com/")
  for (p in list(
    safe_parse_url(u, host_encoding = "unicode"),
    safe_parse_url(u, url_standard = NULL, host_encoding = "unicode")
  )) {
    expect_identical(p$host, paste0(bom, "xn--nxasmq6b.com"))
    expect_identical(p$clean_url, paste0("http://", bom, "xn--nxasmq6b.com/"))
    expect_identical(p$parse_status, "ok")
    expect_identical(p$domain, paste0(beta, ".com"))
  }
  expect_identical(
    unname(get_host(u, host_encoding = "unicode")),
    unname(get_host(u, url_standard = NULL, host_encoding = "unicode"))
  )
  # Signature: under NULL and rfc3986 only `host`, and `clean_url` that
  # carries it, move, only for a host whose first label starts with U+FEFF,
  # and only under host_encoding = "unicode". The status, domain and TLD do
  # not move (pinned above), nor do "keep" and "idna". Per arm: whatwg was
  # already right (UTS #46 maps the mark to nothing before rendering);
  # rfc3986 rendered through the same split as NULL until RUL-030 made the
  # mark-led host a parse error there (RFC 3986 sections 2, 2.5, 3.2.2).
})
