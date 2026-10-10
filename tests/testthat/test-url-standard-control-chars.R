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
})

# --- A URL led by U+FEFF (RURL-vhionecz) --------------------------------------
#
# Step 1 strips only C0 control or space, so a leading U+FEFF (a byte-order
# mark, as on the first line of a file saved with one) stays, and the scheme
# start state, seeing no ASCII alpha, finds no scheme. The row must behave like
# one led by any other non-scheme code point. stringi drops a U+FEFF that starts
# its input, which made rurl parse such a row as if the mark were absent.

BOM <- intToUtf8(0xFEFF)
E_ACUTE <- intToUtf8(0xE9)

test_that("whatwg finds no scheme after a leading U+FEFF", {
  rest <- c("http://a.com/p", "http:\\\\a.com\\p", "http:a.com/p", "//a.com/p")
  for (r in rest) {
    u <- paste0(BOM, r)
    expect_null(safe_parse_url(u, profile = "whatwg"), info = r)
    for (pol in c("infer", "require")) {
      expect_identical(
        is.na(get_host(u, url_standard = "whatwg", scheme_policy = pol)),
        is.na(get_host(paste0(E_ACUTE, r),
          url_standard = "whatwg", scheme_policy = pol
        )),
        info = paste(pol, r)
      )
    }
    expect_true(is.na(get_host(u, url_standard = "whatwg")), info = r)
  }
  # The general route reads the scheme on its own, and must not either.
  for (r in c("foo://a.com/p", "mailto:x@y.z")) {
    u <- paste0(BOM, r)
    expect_null(safe_parse_url(u, profile = "whatwg"), info = r)
    expect_true(is.na(get_host(u,
      url_standard = "whatwg", scheme_policy = "require",
      scheme_acceptance = "general"
    )), info = r)
  }
})

test_that("step 1 keeps a leading U+FEFF and what follows it", {
  # The space after the mark is not leading, so it is not stripped.
  expect_true(is.na(
    get_host(paste0(BOM, " http://a.com/"), url_standard = "whatwg")
  ))
  # A space before the mark is stripped; the mark then leads.
  expect_true(is.na(
    get_host(paste0(" ", BOM, "http://a.com/"), url_standard = "whatwg")
  ))
  # Trailing C0 or space is still stripped from a row led by the mark, and
  # scheme inference reads the row as a host, where UTS #46 maps the mark away.
  u <- paste0(BOM, "a.com/p", " ", TAB)
  expect_identical(
    unname(get_host(u, url_standard = "whatwg", scheme_policy = "infer")),
    "a.com"
  )
  expect_true("leading-trailing-stripped" %in%
    get_url_diagnostics(u, url_standard = "whatwg"))
})

test_that("no WHATWG rewrite reads past a leading U+FEFF", {
  # The alternative full stop sends a row through the separator map, which
  # rebuilt the row from a match that skipped the mark.
  dot <- intToUtf8(0x3002)
  for (r in paste0(c("http://a", "//a"), dot, "com/p")) {
    u <- paste0(BOM, r)
    expect_null(safe_parse_url(u, profile = "whatwg"), info = r)
    expect_true(is.na(get_host(u, url_standard = "whatwg")), info = r)
  }
  # Without a scheme there is no host:port carve-out, as for any other lead.
  for (lead in c(BOM, E_ACUTE)) {
    expect_true(is.na(
      get_host(paste0(lead, "localhost:80/"), url_standard = "whatwg")
    ))
  }
})

test_that("a host led by U+FEFF keeps the whole path", {
  # stri_locate_*_regex() counts a leading U+FEFF and stri_sub() skips it, so
  # the path was cut one character late: "//x" read "/x" (RURL-vhionecz).
  for (std in list(NULL, "rfc3986", "whatwg")) {
    r <- safe_parse_urls(paste0("http://", BOM, "a.com//x"), url_standard = std)
    expect_identical(r$path, "//x", info = format(std))
  }
  r <- safe_parse_urls(paste0("http://", BOM, "a.com/%7e"),
    url_standard = "whatwg"
  )
  expect_identical(r$path, "/%7e")
  # The same cut, reached through scheme inference.
  r <- safe_parse_urls(paste0(BOM, "a.com://x/y"), url_standard = "whatwg")
  expect_identical(r$path, "//x/y")
})

test_that("a WHATWG file: host led by U+FEFF parses like its U+200B twin", {
  # The file parser located the slash with a regex that counts a leading
  # U+FEFF and cut with stri_sub(), which skips it, so the host kept the slash
  # and failed, and the path lost its first character (RURL-azcvukyh).
  zwsp <- intToUtf8(0x200B)
  bs <- "\\"
  for (lead in c(BOM, zwsp)) {
    u <- paste0(
      c("file://", paste0("file:", bs, bs)), lead, "host",
      c("/p", paste0(bs, "p"))
    )
    r <- safe_parse_urls(u, url_standard = "whatwg")
    cp <- sprintf("U+%04X", utf8ToInt(lead))
    expect_identical(r$host, c("host", "host"), info = cp)
    expect_identical(r$path, c("/p", "/p"), info = cp)
  }
  r <- safe_parse_urls(paste0("file://", BOM, c("h.x/p", "%41/p", "host?q")),
    url_standard = "whatwg"
  )
  expect_identical(r$host, c("h.x", "a", "host"))
  expect_identical(r$path, c("/p", "/p", "/"))
  expect_identical(r$query, c(NA, NA, "q"))
  # UTS #46 maps the mark to nothing: "localhost" is then the empty host, and
  # a host made of the mark alone is empty, which fails. The rewrite of a
  # slash-less authority dropped such a mark, so "file://<U+FEFF>" had the
  # empty host and "file://<U+FEFF>C:" a drive letter. "[::1]" after the mark
  # is no IPv6 literal, and "[" is a forbidden domain code point.
  expect_identical(
    serialize_url(paste0("file://", BOM, "localhost/p"), standard = "whatwg"),
    "file:///p"
  )
  for (rest in c("", "?q", "/p", "C:", "[::1]/p", "C:/p", "host:80/p")) {
    expect_identical(
      safe_parse_urls(paste0("file://", BOM, rest),
        url_standard = "whatwg"
      )$parse_status,
      "error",
      info = rest
    )
  }
})

# --- U+FEFF under rfc3986 and NULL (RURL-biunpazk) ----------------------------
#
# RFC 3986 S3.1 makes a scheme start with an ASCII alpha, so a row led by U+FEFF
# has none, and NULL reads such a row like one led by any other code point that
# cannot start a scheme. The scheme readers on both arms used stringi matches
# that read past the mark. ZWSP (U+200B) is the twin: it cannot start a scheme
# either, and no stringi function skips it.
#
# NULL moves (ADR 0016: a default-path defect, not a selector-caused change).
# Witness: the pre-fix NULL probes below, which the fix turns. Signature: only
# rows led by U+FEFF, or whose authority starts with it, move, in every column,
# under every scheme_policy; every row moved to its twin's reading. whatwg was
# already right for a leading mark (RURL-vhionecz); its authority-led rows move
# with the shared authority split.

ZWSP <- intToUtf8(0x200B)

# Every column of `safe_parse_urls()`, with the lead mark spelled the same.
twin_rows <- function(mark, rest, ...) {
  r <- safe_parse_urls(paste0(mark, rest), ...)
  r$original_url <- NULL
  r[] <- lapply(r, function(v) {
    if (is.character(v)) gsub(mark, "<mark>", v, fixed = TRUE, useBytes = TRUE)
    else v
  })
  r
}

BOM_RESTS <- c(
  "foo://a.com/p", "http:a.com/p", "ftp:a.com", "http://a.com/p",
  "mailto:x@y.com", "foo:bar", "urn:a:1", "file:///x", "FILE:/x",
  "file://localhost/path/to/file.txt", "http:///a.com", "https:///evil.com",
  "http://?", "//a.com/p", "a.com/p", "a.com:8080", "a.com:80/p",
  "user:pass@example.com", "http:a:b@www.example.com"
)

test_that("rfc3986 and NULL find no scheme after a leading U+FEFF", {
  for (pol in c("infer", "require")) {
    for (acc in c("web", "general")) {
      got <- twin_rows(BOM, BOM_RESTS,
        url_standard = "rfc3986", scheme_policy = pol, scheme_acceptance = acc
      )
      expect_identical(
        got,
        twin_rows(ZWSP, BOM_RESTS,
          url_standard = "rfc3986", scheme_policy = pol,
          scheme_acceptance = acc
        ),
        info = paste(pol, acc)
      )
      # The read-past scheme never surfaces: only an inferred `http` does.
      expect_true(all(is.na(got$scheme) | got$scheme == "http"),
        info = paste(pol, acc)
      )
    }
    got <- twin_rows(BOM, BOM_RESTS, scheme_policy = pol)
    expect_identical(got, twin_rows(ZWSP, BOM_RESTS, scheme_policy = pol),
      info = pol
    )
    expect_identical(got, twin_rows(BOM, BOM_RESTS,
      url_standard = NULL, scheme_policy = pol
    ), info = pol)
  }
  # The issue's own probes.
  r <- safe_parse_url(paste0(BOM, "foo://a.com/p"),
    url_standard = "rfc3986", scheme_acceptance = "general"
  )
  expect_null(r)
  expect_null(safe_parse_url(paste0(BOM, "http:a.com/p"),
    url_standard = "rfc3986", scheme_policy = "require"
  ))
})

test_that("NULL no longer reads a scheme past a leading U+FEFF (ADR 0016)", {
  # Before RURL-biunpazk, NULL read scheme `file` here, with status `ok` or
  # `error` depending on the other URLs in the call...
  u <- paste0(BOM, "file://localhost/path/to/file.txt")
  for (r in list(safe_parse_urls(u), safe_parse_urls(u, url_standard = NULL))) {
    expect_identical(r$parse_status, "error")
    expect_true(is.na(r$scheme))
  }
  rurl_clear_caches()
  neighbor <- paste0(BOM, "https://", intToUtf8(0xFFFF), "y")
  r <- safe_parse_urls(c(u, neighbor))
  expect_identical(r$parse_status[[1L]], "error")
  expect_true(is.na(r$scheme[[1L]]))
  # ...and rejected this one for its read-past scheme `mailto`, where its twin
  # takes scheme inference.
  r <- safe_parse_urls(paste0(BOM, "mailto:a@b.com"))
  expect_identical(r$parse_status, "warning-userinfo")
  expect_identical(r$host, "b.com")
  expect_identical(r$user, paste0(BOM, "mailto"))
})

test_that("an authority led by U+FEFF splits at the right place", {
  # stri_locate_*_fixed() reads past a U+FEFF that starts the authority, so a
  # cut fell one character early: the host lost its last character to the path,
  # and the port kept its colon.
  u <- paste0("file://", BOM, "localhost/path/to/file.txt")
  for (std in list(NULL, "rfc3986")) {
    r <- safe_parse_urls(u, url_standard = std)
    expect_identical(r$host, paste0(BOM, "localhost"), info = format(std))
    expect_identical(r$path, "/path/to/file.txt", info = format(std))
  }
  r <- safe_parse_urls(paste0("ftps://", BOM, "files.example.org/pub/"),
    profile = "whatwg"
  )
  expect_identical(r$host, "%EF%BB%BFfiles.example.org")
  expect_identical(r$path, "/pub/")
  # The RFC 3986 grammar gate rejected every such authority with a port or a
  # userinfo.
  r <- safe_parse_urls(paste0("http://", BOM, "a.com:8080/"),
    url_standard = "rfc3986"
  )
  expect_identical(r$host, paste0(BOM, "a.com"))
  expect_identical(r$port, 8080L)
  expect_identical(r$parse_status, "ok")
  r <- safe_parse_urls(paste0("http://", BOM, "u:p@a.com/"),
    url_standard = "rfc3986"
  )
  expect_identical(r$user, paste0(BOM, "u"))
  expect_identical(r$host, "a.com")
  expect_identical(r$parse_status, "ok")
  # WHATWG opaque hosts: the mark is a host code point, so the host is not
  # empty, and credentials with an empty host still fail.
  r <- safe_parse_urls(paste0("sc://", BOM, ":12/"), profile = "whatwg")
  expect_identical(r$host, "%EF%BB%BF")
  expect_identical(r$port, 12L)
  expect_identical(r$path, "/")
  expect_null(safe_parse_url(paste0("sc://", BOM, "@/"), profile = "whatwg"))
})

test_that("the RFC 3986 grammar gate finds no scheme after a leading U+FEFF", {
  expect_identical(
    .rfc3986_generic_uri_ok(paste0(c(BOM, ZWSP, ""), "foo:bar"))$ok,
    c(FALSE, FALSE, TRUE)
  )
  rests <- c("a.com:8080", "mailto:x@y.com")
  for (rest in rests) {
    expect_identical(
      get_url_diagnostics(paste0(BOM, rest),
        url_standard = "rfc3986", scheme_acceptance = "general"
      ),
      get_url_diagnostics(paste0(ZWSP, rest),
        url_standard = "rfc3986", scheme_acceptance = "general"
      ),
      info = rest
    )
  }
})

test_that("the WHATWG full-stop map keeps a U+FEFF that starts the userinfo", {
  # The map split the authority with stri_locate_last_fixed() and stri_sub(),
  # which read past a U+FEFF that starts it, so the rebuilt row lost the mark
  # (RURL-exdlurql). Its U+200B twin and the ASCII-dot row always kept theirs.
  stop3002 <- "\u3002"
  for (dot in c(stop3002, "\uff0e", "\uff61", ".")) {
    r <- safe_parse_urls(paste0("http://", BOM, "u@a", dot, "com/"),
      url_standard = "whatwg"
    )
    expect_identical(r$user, "%EF%BB%BFu", info = dot)
    expect_identical(r$host, "a.com", info = dot)
    expect_identical(r$path, "/", info = dot)
  }
  r <- safe_parse_urls(paste0("http://", ZWSP, "u@a", stop3002, "com/"),
    url_standard = "whatwg"
  )
  expect_identical(r$user, "%E2%80%8Bu")
  # Userinfo is not a domain: its own full stop stays encoded, and only the
  # host after the last "@" is mapped.
  r <- safe_parse_urls(
    paste0("http://", BOM, "u", stop3002, "x@y@a", stop3002, "com/p", stop3002),
    url_standard = "whatwg"
  )
  expect_identical(r$user, "%EF%BB%BFu%E3%80%82x%40y")
  expect_identical(r$host, "a.com")
  expect_identical(r$path, paste0("/p", stop3002))
  r <- safe_parse_urls(paste0("//", BOM, "u@a", stop3002, "com/"),
    url_standard = "whatwg"
  )
  expect_identical(r$user, "%EF%BB%BFu")
  expect_identical(r$host, "a.com")
  # A host led by the mark maps it to nothing (UTS #46).
  r <- safe_parse_urls(paste0("http://u@", BOM, "a", stop3002, "com/"),
    url_standard = "whatwg"
  )
  expect_identical(r$user, "u")
  expect_identical(r$host, "a.com")
})

test_that("the WHATWG userinfo encoder keeps a U+FEFF that leads it", {
  # The authority state percent-encodes userinfo with the userinfo
  # percent-encode set and removes nothing. A space, a C0 control or DEL sends
  # the userinfo through rurl's encoder (RURL-rfgbozdr), whose
  # stri_replace_all_fixed() read past a U+FEFF that starts its input, so the
  # mark was dropped: "http://<U+FEFF>a b@a.com/" had user "a%20b".
  r <- safe_parse_urls(
    paste0("http://", BOM, c("a b", "a\001b", "a\177b", " b"), "@a.com/"),
    url_standard = "whatwg"
  )
  expect_identical(
    r$user,
    c("%EF%BB%BFa%20b", "%EF%BB%BFa%01b", "%EF%BB%BFa%7Fb", "%EF%BB%BF%20b")
  )
  expect_identical(r$host, rep("a.com", 4L))
  # Only one of two marks was dropped, and a user made of the mark alone was
  # lost before its ":".
  r <- safe_parse_urls(
    paste0("http://", BOM, c(BOM, ":"), "a b@a.com/"),
    url_standard = "whatwg"
  )
  expect_identical(r$user, c("%EF%BB%BF%EF%BB%BFa%20b", "%EF%BB%BF"))
  expect_identical(r$password, c(NA, "a%20b"))
  # Its U+200B twin keeps the mark, for each kind of refused code point.
  r <- safe_parse_urls(
    paste0("http://", ZWSP, c("a b", "a\001b", "a\177b"), "@a.com/"),
    url_standard = "whatwg"
  )
  expect_identical(
    r$user, c("%E2%80%8Ba%20b", "%E2%80%8Ba%01b", "%E2%80%8Ba%7Fb")
  )
  expect_identical(r$host, rep("a.com", 3L))
  # A userinfo with none of the refused code points never reaches the encoder.
  r <- safe_parse_urls(paste0("http://", BOM, "ab@a.com/"),
    url_standard = "whatwg"
  )
  expect_identical(r$user, "%EF%BB%BFab")
  # A mark after the userinfo's first code point was always kept.
  r <- safe_parse_urls(paste0("http://u:", BOM, "a b@a.com/"),
    url_standard = "whatwg"
  )
  expect_identical(r$user, "u")
  expect_identical(r$password, "%EF%BB%BFa%20b")
  expect_identical(r$host, "a.com")
})

test_that("a WHATWG file: path led by U+FEFF keeps the mark", {
  # With no authority, the file state hands the input to the path state, which
  # percent-encodes the path with the path percent-encode set and removes
  # nothing (RURL-rjpljsui). The file: parser rewrote "\" with
  # stri_replace_all_fixed() and tested for a leading "/" with
  # stri_startswith_fixed(), which both read past a U+FEFF that starts their
  # input, so the mark was dropped: "file:<U+FEFF>p" was "file:///p".
  expect_identical(
    serialize_url(
      paste0("file:", BOM, c("p", "/p", "", "\\p", "?q")),
      standard = "whatwg"
    ),
    c(
      "file:///%EF%BB%BFp", "file:///%EF%BB%BF/p", "file:///%EF%BB%BF",
      "file:///%EF%BB%BF/p", "file:///%EF%BB%BF?q"
    )
  )
  # Only one of two marks was dropped. A path led by the mark does not start
  # with a Windows drive letter, so "C|" is kept and ".." removes "C:".
  expect_identical(
    serialize_url(
      paste0("file:", BOM, c(BOM, "", ""), c("p", "C|/x", "/C:/../x")),
      standard = "whatwg"
    ),
    c("file:///%EF%BB%BF%EF%BB%BFp", "file:///%EF%BB%BFC|/x",
      "file:///%EF%BB%BF/x")
  )
  # A colon before any slash (RURL-otfaotzq). The mark is not "/" or "\", so
  # the file state hands the input to the path state; a later "\" is a segment
  # separator, and the colon is path data. rurl read the colon as an IPv6 host
  # attempt and failed each row before the file: parser ran. Values match Node.
  expect_identical(
    serialize_url(
      paste0("file:", rep(c(BOM, ZWSP), each = 2L), c("C:/x", "\\C:/x")),
      standard = "whatwg"
    ),
    c(
      "file:///%EF%BB%BFC:/x", "file:///%EF%BB%BF/C:/x",
      "file:///%E2%80%8BC:/x", "file:///%E2%80%8B/C:/x"
    )
  )
  # Negative controls: NULL and rfc3986 refuse all four rows.
  for (std in list(NULL, "rfc3986")) {
    expect_identical(
      suppressWarnings(get_parse_status(
        paste0("file:", rep(c(BOM, ZWSP), each = 2L), c("C:/x", "\\C:/x")),
        url_standard = std
      )),
      rep("error", 4L)
    )
  }
  # The U+200B twin keeps its mark.
  expect_identical(
    serialize_url(paste0("file:", ZWSP, c("p", "/p", "")), standard = "whatwg"),
    c("file:///%E2%80%8Bp", "file:///%E2%80%8B/p", "file:///%E2%80%8B")
  )
  # A mark after a leading "/" or "\" was always kept, and is not a Windows
  # drive letter.
  expect_identical(
    serialize_url(
      paste0("file:", c("/", "\\", "/"), BOM, c("p", "p", "C|/x")),
      standard = "whatwg"
    ),
    c("file:///%EF%BB%BFp", "file:///%EF%BB%BFp", "file:///%EF%BB%BFC|/x")
  )
  # Host-less paths without the mark.
  expect_identical(
    serialize_url(c("file:p", "file:/p", "file:\\p", "file:"),
      standard = "whatwg"
    ),
    c("file:///p", "file:///p", "file:///p", "file:///")
  )
})
