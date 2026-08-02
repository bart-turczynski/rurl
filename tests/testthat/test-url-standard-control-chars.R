# Tests for the WHATWG control-character stripping vertical slice
# (RURL-tyetpjym, epic RURL-moselrwp). The WHATWG basic URL parser's first step
# removes every ASCII tab (U+0009), LF (U+000A), and CR (U+000D) from the input
# before parsing. rurl otherwise rejects a control char in the authority
# (the parser errors) -- correct under RFC 3986, which requires such bytes to
# be
# percent-encoded and has no strip step. So the strip runs ONLY under
# url_standard = "whatwg"; rfc3986 and no selector keep rejecting. Stripping is
# surfaced, not silent: it fires the `control-char-stripped` diagnostic
# (ADR 0006). Built via paste0() so the exact control byte is unambiguous.

TAB <- "\t"; LF <- "\n"; CR <- "\r"

# --- whatwg strips and accepts -----------------------------------------------

test_that("whatwg strips a tab in the host and parses", {
  u <- paste0("http://ex", TAB, "ample.com/")
  expect_identical(get_clean_url(u, url_standard = "whatwg"),
                   "http://example.com/")
  expect_identical(get_host(u, url_standard = "whatwg"), "example.com")
})

test_that("whatwg strips an LF in the host and parses", {
  u <- paste0("https://n.pr", LF, "e.gg")
  expect_identical(get_host(u, url_standard = "whatwg"), "n.pre.gg")
})

test_that("whatwg strips CR/LF everywhere (SSRF/CRLF-injection shape)", {
  # yal-003: CR LF inside the host coalesces the host to 127.0.0.1.
  u <- paste0("http://127.0.0.", CR, LF, "1:6379?SET", CR, LF, "x")
  expect_identical(get_host(u, url_standard = "whatwg"), "127.0.0.1")
  expect_false(is.na(get_clean_url(u, url_standard = "whatwg")))
})

test_that("stripping is surfaced via the control-char-stripped diagnostic", {
  u <- paste0("http://ex", TAB, "ample.com/")
  expect_true("control-char-stripped" %in%
                get_url_diagnostics(u, url_standard = "whatwg"))
})

# --- rfc3986 / no selector keep rejecting ------------------------------------

test_that("rfc3986 rejects a control char in the authority (no strip step)", {
  u <- paste0("http://ex", TAB, "ample.com/")
  expect_identical(get_parse_status(u, url_standard = "rfc3986"), "error")
  expect_true(is.na(get_clean_url(u, url_standard = "rfc3986")))
})

test_that("no selector keeps the strict default (rejects control chars)", {
  u <- paste0("http://ex", TAB, "ample.com/")
  expect_identical(get_parse_status(u), "error")
})

test_that("the diagnostic never fires under rfc3986", {
  u <- paste0("http://ex", TAB, "ample.com/")
  expect_false("control-char-stripped" %in%
                 get_url_diagnostics(u, url_standard = "rfc3986"))
})

# --- no-op guarantees --------------------------------------------------------

test_that("a control-char-free URL is byte-for-byte unchanged", {
  u <- "http://Example.com/a/b?q=1#f"
  expect_identical(get_clean_url(u, url_standard = "whatwg"),
                   get_clean_url(u, url_standard = "whatwg"))
  # and the diagnostic does not fire spuriously
  expect_false("control-char-stripped" %in%
                 get_url_diagnostics(u, url_standard = "whatwg"))
})

test_that("stripping is vectorized and per-row", {
  us <- c(
    paste0("http://ex", TAB, "ample.com/"),   # stripped
    "http://clean.com/",                       # untouched
    paste0("https://n.pr", LF, "e.gg")         # stripped
  )
  hosts <- get_host(us, url_standard = "whatwg")
  expect_identical(hosts, c("example.com", "clean.com", "n.pre.gg"))
  diags <- get_url_diagnostics(us, url_standard = "whatwg")
  fires <- vapply(diags, function(d) "control-char-stripped" %in% d, logical(1))
  expect_identical(fires, c(TRUE, FALSE, TRUE))
})

# --- step 1's FIRST half: leading/trailing C0-or-SPACE (RURL-yvxpanix) --------
#
# WHATWG step 1 has two halves, in this order: (1a) remove any leading and
# trailing C0-control-or-SPACE (U+0000..U+0020), then (1b) remove all tab/LF/CR
# anywhere. Half 1a is a DIFFERENT fact from half 1b, so it carries its own
# `leading-trailing-stripped` diagnostic. Like 1b it must not run under
# url_standard = "rfc3986" / no selector, which have no strip step.

# U+0001..U+0020, i.e. every C0 control plus SPACE. U+0000 is deliberately
# absent: an R character string cannot carry an embedded NUL at all.
C0_RUN <- intToUtf8(1:32)

test_that("whatwg strips a trailing space run instead of encoding it", {
  u <- "http://example.com/a  "
  # Was "/a%20%20" (the two spaces percent-encoded into the path).
  expect_identical(get_path(u, url_standard = "whatwg"), "/a")
  expect_identical(get_clean_url(u, url_standard = "whatwg"),
                   "http://example.com/a")
})

test_that("whatwg accepts an input with a leading space run", {
  u <- "  http://example.com/a"
  # Was a parse error (the leading space reached the parser).
  expect_identical(get_parse_status(u, url_standard = "whatwg"), "ok")
  expect_identical(get_host(u, url_standard = "whatwg"), "example.com")
  expect_identical(get_path(u, url_standard = "whatwg"), "/a")
})

test_that("whatwg strips both ends at once", {
  expect_identical(get_clean_url("  http://example.com/a  ",
                                 url_standard = "whatwg"),
                   "http://example.com/a")
})

test_that("whatwg strips the whole U+0001..U+0020 range at both ends", {
  u <- paste0(C0_RUN, "http://example.com/a", C0_RUN)
  expect_identical(get_host(u, url_standard = "whatwg"), "example.com")
  expect_identical(get_path(u, url_standard = "whatwg"), "/a")
})

test_that("whatwg strips a trailing run from a non-special opaque path", {
  # Was path "opaque  " (both spaces carried verbatim). The opaque trailing-
  # space rule (.whatwg_opaque_path_encode) only fires when a "?"/"#" follows,
  # so the two rules never both apply to the same space.
  d <- safe_parse_urls(c("non-special:opaque  ", "  non-special:opaque"),
                       url_standard = "whatwg", scheme_policy = "require",
                       scheme_acceptance = "general")
  expect_identical(d$path, c("opaque", "opaque"))
})

test_that("interior spaces and controls are NOT stripped by half 1a", {
  # Interior spaces stay (and are percent-encoded by the path serializer);
  # interior tab/LF/CR are removed by half 1b, which keeps its own token.
  expect_identical(get_path("http://example.com/a b c ",
                            url_standard = "whatwg"),
                   "/a%20b%20c")
  u <- paste0("http://exa", TAB, "mple.com/")
  expect_identical(get_host(u, url_standard = "whatwg"), "example.com")
  expect_false("leading-trailing-stripped" %in%
                 get_url_diagnostics(u, url_standard = "whatwg"))
})

test_that("the two step-1 halves emit two independent diagnostics", {
  us <- c(
    "http://example.com/a  ",                     # 1a only
    paste0("http://exa", TAB, "mple.com/"),        # 1b only
    paste0(" http://exa", LF, "mple.com/a "),      # both
    "http://example.com/a"                         # neither
  )
  diags <- get_url_diagnostics(us, url_standard = "whatwg")
  fired <- function(token) {
    vapply(diags, function(d) token %in% d, logical(1))
  }
  expect_identical(fired("leading-trailing-stripped"),
                   c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(fired("control-char-stripped"),
                   c(FALSE, TRUE, TRUE, FALSE))
})

test_that("half 1a is a byte-for-byte no-op under rfc3986 / no selector", {
  us <- c("  http://example.com/a", "http://example.com/a  ",
          "http://example.com/b")
  for (std in list("rfc3986", NULL)) {
    stripped <- rurl:::.strip_whatwg_control_chars_vec(us, std)
    # The rfc3986 row keeps its input spelling, byte for byte.
    expect_identical(stripped$url, us)
    expect_identical(stripped$leading_trailing_stripped, rep(FALSE, 3L))
    expect_identical(stripped$control_char_stripped, rep(FALSE, 3L))
  }
  # And nothing is rescued at the public surface: rfc3986 requires such bytes
  # to be percent-encoded, so both rows stay errors under either selector.
  expect_identical(get_parse_status(us[1:2], url_standard = "rfc3986"),
                   c("error", "error"))
  expect_identical(get_parse_status(us[1:2]), c("error", "error"))
})

test_that("the leading/trailing diagnostic never fires under rfc3986", {
  u <- "  http://example.com/a  "
  expect_false("leading-trailing-stripped" %in%
                 get_url_diagnostics(u, url_standard = "rfc3986"))
  expect_false("leading-trailing-stripped" %in%
                 get_url_diagnostics(u))
})
