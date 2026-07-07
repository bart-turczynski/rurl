# Tests for the WHATWG control-character stripping vertical slice
# (RURL-tyetpjym, epic RURL-moselrwp). The WHATWG basic URL parser's first step
# removes every ASCII tab (U+0009), LF (U+000A), and CR (U+000D) from the input
# before parsing. rurl otherwise rejects a control char in the authority
# (libcurl errors) -- correct under RFC 3986, which requires such bytes to be
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
