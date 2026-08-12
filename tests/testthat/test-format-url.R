# Output surface (d) -- safe display.
#
# The specification under test is P2.7 D-D
# (`design/work/url-v3/decisions/P2.7-display-and-resolver-output.md`), and
# P2.7 §7 names S3-F3's five worked examples as the acceptance criteria D-D must
# be tested against. Those five come first, by name.

fmt <- function(...) format_url(...)

# Build a string from raw octets without going through the locale. Needed for
# the E3 cases, whose whole point is bytes that are not valid UTF-8 -- a source
# literal would either be rejected by the parser or silently transcoded.
octets <- function(bytes) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- "UTF-8"
  s
}

# --- S3-F3's five worked examples (P2.7 §7 acceptance criteria) --------------

test_that("S3-F3 example 1: /a%2Fb does not display as /a/b", {
  expect_equal(
    fmt("https://example.com/a%2Fb"), "https://example.com/a%2Fb"
  )
})

test_that("S3-F3 example 2: ?x=a%26b%3Dc does not display as ?x=a&b=c", {
  expect_equal(
    fmt("https://example.com/p?x=a%26b%3Dc"),
    "https://example.com/p?x=a%26b%3Dc"
  )
})

test_that("S3-F3 example 3: U+202E, LF and NUL get exact visible outputs", {
  expect_equal(
    fmt("https://example.com/%E2%80%AEcod.exe"),
    "https://example.com/<U+202E>cod.exe"
  )
  expect_equal(
    fmt("https://example.com/a%0Ab"), "https://example.com/a<U+000A>b"
  )
  expect_equal(
    fmt("https://example.com/a%00b"), "https://example.com/a<U+0000>b"
  )
})

test_that("S3-F3 example 4: %FF is emitted, not decoded and not dropped", {
  expect_equal(fmt("https://example.com/a%FFb"), "https://example.com/a%FFb")
})

test_that("S3-F3 example 5: both host spellings are shown when they differ", {
  expect_equal(
    fmt("https://xn--mnchen-3ya.de/p"),
    "https://münchen.de/p  [host: xn--mnchen-3ya.de]"
  )
  # The Unicode source spelling formats identically: the parse commits to the
  # A-label either way, so display is a decode of one host, not a choice.
  expect_equal(
    fmt("https://münchen.de/p"),
    "https://münchen.de/p  [host: xn--mnchen-3ya.de]"
  )
})

# --- E1: `<` and `>` are never emitted from data ------------------------------

test_that("E1 escapes a literal angle bracket and keeps an encoded one", {
  expect_equal(
    fmt("https://example.com/a<b>c"),
    "https://example.com/a<U+003C>b<U+003E>c"
  )
  expect_equal(
    fmt("https://example.com/a%3Cb%3Ec"), "https://example.com/a%3Cb%3Ec"
  )
  # Lower-case triplets are normalized to the uppercase spelling E3 names.
  expect_equal(
    fmt("https://example.com/a%3cb%3ec"), "https://example.com/a%3Cb%3Ec"
  )
})

test_that("every angle bracket in the output was written by the formatter", {
  # The invariant `<redacted>` depends on: strip the formatter's own tokens and
  # no bracket may survive. Run over every component that can carry data.
  corpus <- c(
    "https://u:p<>@example.com/<a>?q=<b>#<c>",
    "https://example.com/%3C%3E?x=%3c%3e#%3C",
    "https://example.com/a<%3Eb",
    "foo://a%3Cb/p",
    "https://xn--mnchen-3ya.de/<p>"
  )
  out <- fmt(corpus)
  stripped <- gsub("<redacted>|<U\\+[0-9A-F]{4,6}>", "", out, perl = TRUE)
  expect_false(any(grepl("[<>]", stripped)))
})

# --- E2a: the immutable blocks ------------------------------------------------

test_that("E2a escapes C0, DEL, C1 and private-use code points", {
  expect_equal(
    fmt("https://example.com/%01%1F"), "https://example.com/<U+0001><U+001F>"
  )
  expect_equal(
    fmt("https://example.com/%7F%C2%80%C2%9F"),
    "https://example.com/<U+007F><U+0080><U+009F>"
  )
  expect_equal(
    fmt(paste0("https://example.com/", intToUtf8(0xE000))),
    "https://example.com/<U+E000>"
  )
  expect_equal(
    fmt(paste0("https://example.com/", intToUtf8(0x10FFFD))),
    "https://example.com/<U+10FFFD>"
  )
})

# --- E2b: the hazard list -----------------------------------------------------

test_that("E2b escapes every bidi formatting character D-D names", {
  # U+202A-U+202E (LRE, RLE, PDF, LRO, RLO) and U+2066-U+2069 (LRI, RLI, FSI,
  # PDI): the set D-D says the hazard list "must include, and does include".
  bidi <- c(0x202A:0x202E, 0x2066:0x2069)
  for (cp in bidi) {
    expect_equal(
      fmt(paste0("https://example.com/", intToUtf8(cp))),
      sprintf("https://example.com/<U+%04X>", cp)
    )
  }
})

test_that("E2b escapes the marks, joiners and invisibles D-D names", {
  rest <- c(0x00AD, 0x061C, 0x180E, 0x200B:0x200F, 0x2060:0x2064,
            0x206A:0x206F, 0xFEFF, 0xFFF9:0xFFFB)
  for (cp in rest) {
    expect_equal(
      fmt(paste0("https://example.com/", intToUtf8(cp))),
      sprintf("https://example.com/<U+%04X>", cp)
    )
  }
})

# --- E2c and the Unicode-version invariance duty (P2.7 §7) --------------------

test_that("E2c passes through code points no enumeration lists", {
  # These are the DRIFT PROBES, and they are what makes the invariance duty
  # non-vacuous: each is a code point a general-category implementation would
  # escape, and whose category is exactly what moves between Unicode releases.
  #
  #   U+2065  -- unassigned (Cn), and deliberately inside the gap D-D leaves
  #              between U+2060-U+2064 and U+2066-U+206F
  #   U+FFF8  -- unassigned (Cn), immediately below the U+FFF9-U+FFFB hazards
  #   U+110BD -- assigned format character (Cf) that is NOT a display hazard
  #   U+E0001 -- deprecated LANGUAGE TAG (Cf), likewise not listed
  #
  # An implementation spelled "escape Cc/Cf/Cs/Co/Cn" passes every other test
  # in this file and fails this one.
  for (cp in c(0x2065, 0xFFF8, 0x110BD, 0xE0001)) {
    expect_equal(
      fmt(paste0("https://example.com/", intToUtf8(cp))),
      paste0("https://example.com/", intToUtf8(cp))
    )
  }
})

test_that("the escape decision performs no runtime category lookup", {
  # The structural half of the same duty. E2 must be a pair of constants: if a
  # category class or a Unicode-version argument ever enters the display path,
  # the output becomes a function of the installed Unicode data rather than of
  # the URL, which is the drift RURL-gxqdmpcp spent an epic removing.
  #
  # Read off the NAMESPACE OBJECTS, not off `R/format.R`. A source path does
  # not exist under `R CMD check`, which runs against the installed package --
  # and deparsing the live objects is the stronger check anyway, since it is
  # the code that will actually run.
  ns <- asNamespace("rurl")
  nms <- grep(
    "^(\\.format|\\.FORMAT|format_url$)", ls(ns, all.names = TRUE), value = TRUE
  )
  # Guard against the predicate going vacuous: a rename that empties `nms`
  # would make every expectation below pass for the wrong reason.
  expect_gte(length(nms), 10L)
  code <- unlist(lapply(nms, function(nm) deparse(get(nm, envir = ns))))
  expect_false(any(grepl("p{", code, fixed = TRUE)))
  expect_false(any(grepl("charclass", code, fixed = TRUE)))
  expect_false(any(grepl("unicode_version", code, fixed = TRUE)))
})

test_that("the pinned IDN fixture host is stable across Unicode versions", {
  # The one seam that could still move the host leg under a data bump is
  # punycoder's own UTS-46 mapping. rurl passes no `unicode_version`, so this
  # asserts the fixture host used in the S3-F3 example above is not one of the
  # code points whose mapping differs between the offered versions -- i.e. the
  # pin above cannot go red for a reason outside rurl.
  versions <- punycoder::unicode_versions()
  expect_gte(length(versions), 1L)
  labels <- vapply(
    versions,
    function(v) {
      punycoder::host_normalize(
        "münchen.de", check_hyphens = FALSE, use_std3 = FALSE,
        verify_dns_length = FALSE, unicode_version = v
      )
    },
    character(1)
  )
  expect_equal(unname(unique(labels)), "xn--mnchen-3ya.de")
})

# --- E3: invalid UTF-8 -------------------------------------------------------

test_that("E3 leaves an undecodable triplet run as uppercase %XX", {
  # A lone surrogate cannot occur in valid UTF-8, so it reaches the formatter
  # only as bytes, and E3 -- not E2a's surrogate range -- is what handles it.
  expect_equal(
    fmt("https://example.com/%ED%A0%80"), "https://example.com/%ED%A0%80"
  )
  expect_equal(
    fmt("https://example.com/%ff%fe"), "https://example.com/%FF%FE"
  )
  # A truncated sequence: the valid tail still decodes.
  expect_equal(
    fmt("https://example.com/%C3%A9%C3"), "https://example.com/é%C3"
  )
})

test_that("a malformed percent sign is not a triplet and is shown as data", {
  expect_equal(fmt("https://example.com/a%zz"), "https://example.com/a%zz")
  expect_equal(fmt("https://example.com/a%"), "https://example.com/a%")
})

# --- E4: decoding never fabricates structure ----------------------------------

test_that("E4 keeps encoded grammar delimiters encoded in every component", {
  expect_equal(
    fmt("https://example.com/a%2Fb%3Fc%23d%40e%3Af%5Bg%5Dh%25i%5Cj"),
    "https://example.com/a%2Fb%3Fc%23d%40e%3Af%5Bg%5Dh%25i%5Cj"
  )
  expect_equal(
    fmt("https://example.com/p#a%2Fb%23c"), "https://example.com/p#a%2Fb%23c"
  )
})

test_that("E4 adds & = + to the delimiter set inside the query only", {
  expect_equal(
    fmt("https://example.com/p?a%26b%3Dc%2Bd"),
    "https://example.com/p?a%26b%3Dc%2Bd"
  )
  # The same three are ordinary data in a path, and decode there.
  expect_equal(
    fmt("https://example.com/a%26b%3Dc%2Bd"), "https://example.com/a&b=c+d"
  )
})

test_that("E4 preserves a percent sign so decoding happens exactly once", {
  expect_equal(fmt("https://example.com/p%252F"), "https://example.com/p%252F")
})

test_that("a literal delimiter is structure and is shown as itself", {
  expect_equal(
    fmt("https://example.com/a/b?c=d&e=f#g"),
    "https://example.com/a/b?c=d&e=f#g"
  )
})

# --- E5: everything else decodes ----------------------------------------------

test_that("E5 decodes an ordinary percent-encoded character", {
  expect_equal(
    fmt("https://example.com/caf%C3%A9"), "https://example.com/café"
  )
  expect_equal(
    fmt("https://example.com/a%20b"), "https://example.com/a b"
  )
})

# --- Userinfo -----------------------------------------------------------------

test_that("userinfo is redacted whenever any was present, including a bare @", {
  present <- c(
    "https://user:password@example.com/x",
    "https://u@example.com/x",
    "https://:p@example.com/x",
    "https://@example.com/x"
  )
  expect_equal(
    fmt(present), rep("https://<redacted>@example.com/x", length(present))
  )
})

test_that("the redaction token is fixed-width and leaks no length", {
  short <- fmt("https://a:b@example.com/x")
  long <- fmt("https://averylongusername:andanevenlongerpassword@example.com/x")
  expect_equal(short, long)
})

test_that("no userinfo emits no token and no delimiter", {
  expect_equal(fmt("https://example.com/x"), "https://example.com/x")
})

test_that("a generic-authority scheme's credentials are redacted too", {
  # P2.2 §5.5 / P4.1 B9 govern "all preserved credentials", web AND
  # generic-authority alike. A rule that only reached the special schemes would
  # leak the secret for exactly the schemes nobody writes a fixture for.
  expect_equal(
    fmt(c("foo://user:pw@host.example/x", "git+ssh://u:p@host/r.git")),
    c("foo://<redacted>@host.example/x", "git+ssh://<redacted>@host/r.git")
  )
})

# --- Host ---------------------------------------------------------------------

test_that("an ASCII host gets no annotation", {
  expect_equal(fmt("https://example.com/p"), "https://example.com/p")
  expect_false(grepl("[host:", fmt("https://example.com/p"), fixed = TRUE))
})

test_that("IP literals are shown verbatim and never annotated", {
  expect_equal(
    fmt("https://[2001:db8::1]:8080/x"), "https://[2001:db8::1]:8080/x"
  )
  expect_equal(fmt("https://127.0.0.1/x"), "https://127.0.0.1/x")
})

test_that("an empty host keeps its authority delimiter", {
  expect_equal(fmt("file:///etc/passwd"), "file:///etc/passwd")
})

test_that("a punycode label decodes into the display spelling", {
  expect_equal(
    fmt("https://xn--n3h.example/p"),
    "https://☃.example/p  [host: xn--n3h.example]"
  )
})

# --- Port ---------------------------------------------------------------------

test_that("the port is shown verbatim with no default-port elision", {
  # Elision is normalization -- surface (b)'s business. A reader of surface (d)
  # is told what the URL actually spells.
  expect_equal(fmt("https://example.com:443/x"), "https://example.com:443/x")
  expect_equal(fmt("http://example.com:80/x"), "http://example.com:80/x")
  expect_equal(fmt("https://example.com:8080/x"), "https://example.com:8080/x")
  expect_equal(fmt("https://example.com/x"), "https://example.com/x")
})

# --- Delimiter presence -------------------------------------------------------

test_that("the fragment is shown, and empty delimiters survive", {
  expect_equal(fmt("https://example.com/p#frag"), "https://example.com/p#frag")
  expect_equal(fmt("https://example.com/#"), "https://example.com/#")
  expect_equal(fmt("https://example.com/?"), "https://example.com/?")
  expect_equal(fmt("https://example.com/"), "https://example.com/")
})

test_that("an opaque path and a path-only URL keep the serializer's shape", {
  expect_equal(fmt("mailto:a@b.com"), "mailto:a@b.com")
  expect_equal(fmt("urn:ietf:rfc:2648"), "urn:ietf:rfc:2648")
  # The `/.` guard: a hostless path beginning `//` must not read as authority.
  expect_equal(fmt("foo:/.//p"), "foo:/.//p")
})

# --- Vector contract ----------------------------------------------------------

test_that("input that the WHATWG parser does not accept formats as NA", {
  expect_true(is.na(fmt("example.com/x")))
  expect_true(is.na(fmt(NA_character_)))
  expect_true(is.na(fmt("https://exa%E2%80%AEmple.com/")))
})

test_that("the vector contract matches the sibling full-string surface", {
  expect_equal(fmt(character(0)), character(0))
  expect_equal(
    fmt(factor(c("https://a.com/", "https://b.com/"))),
    c("https://a.com/", "https://b.com/")
  )
  expect_null(names(fmt(c(x = "https://a.com/"))))
  expect_equal(
    fmt(list("https://a.com/", 42)), c("https://a.com/", NA_character_)
  )
  expect_length(fmt(rep("https://a.com/", 3L)), 3L)
})

# --- Surface discipline -------------------------------------------------------

test_that("format_url takes no presentation dial", {
  # D-D: the escape matrix IS the presentation rule. `path_encoding = "decode"`
  # would hand back the exact /a%2Fb -> /a/b hazard E4 exists to prevent.
  expect_named(formals(format_url), c("url", "engine"))
})

test_that("the display surface is not the serialization surface", {
  u <- "https://user:pw@xn--mnchen-3ya.de/%E2%80%AEa%2Fb#f"
  expect_false(identical(fmt(u), serialize_url(u, standard = "whatwg")))
  # Not reparsable, by contract (output-contracts.md:209): the formatter's own
  # tokens are not URL syntax and re-parsing them is not a supported operation.
  expect_true(grepl("<U+202E>", fmt(u), fixed = TRUE))
  expect_true(grepl("<redacted>", fmt(u), fixed = TRUE))
})

test_that("the result is declared UTF-8", {
  expect_equal(Encoding(fmt("https://xn--mnchen-3ya.de/p")), "UTF-8")
})

test_that("output is byte-identical under the C locale", {
  corpus <- c(
    "https://user:pw@xn--mnchen-3ya.de:8443/caf%C3%A9/a%2Fb?x=a%26b#%E2%80%AE",
    "https://example.com/%FF%00%01",
    "https://münchen.de/p",
    "foo:/.//p"
  )
  baseline <- fmt(corpus)
  under_c <- withr::with_collate("C", fmt(corpus))
  expect_equal(under_c, baseline)
})
