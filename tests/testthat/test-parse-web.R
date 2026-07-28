# Literal-oracle tests for the in-tree web/special-scheme parser
# (R/parse-web.R, RURL-robgajml step 3).
#
# EVERY expectation here is a LITERAL, never a differential against
# `curl::curl_parse_url()`. That is a hard requirement, not a preference:
# `RURL-cunfohwy`'s curl-zero-reference gate forbids a curl reference anywhere
# in `tests/`, and an oracle that outlives the dependency is the only kind
# worth having. The literals were PRODUCED by differential sweeps against
# libcurl (106,898 inputs across a structural grid, a per-octet sweep of every
# position, an IPv6/IPv4/percent-escape fuzz corpus and the WPT + in-repo URL
# corpora, all at zero differences) and then frozen here by hand. The sweep is
# how they were found; this file is what holds them.

p <- function(url, last = FALSE, pct = "narrow", pqf = "reject") {
  rurl:::.parse_web_url_one(url,
    last_at_userinfo = last, host_pct = pct, pqf_bytes = pqf
  )
}

# Compact fingerprint of one parse, so a whole rule reads as one table.
fp <- function(url, last = FALSE, pct = "narrow", pqf = "reject") {
  r <- p(url, last, pct, pqf)
  if (is.null(r)) {
    return("REJECT")
  }
  paste(
    vapply(
      c("scheme", "host", "port", "path", "query", "fragment",
        "user", "password"),
      function(k) if (is.null(r[[k]])) "-" else r[[k]],
      character(1)
    ),
    collapse = "|"
  )
}

# Compare a field to the OCTETS it must carry, never to a source literal.
# The parser hands back raw bytes with no encoding declaration, while a
# `"\uXXXX"` literal is UTF-8-marked; `identical()` compares the two only
# after translating both through the session's native encoding, so it agrees
# under a UTF-8 locale and disagrees under `LC_ALL=C` on the very same bytes.
# Octets are the locale-invariant statement, and they are the statement the
# parser actually makes (RURL-cpmxhbgg). Integers, not raws, so that a failure
# renders in any locale instead of warning "unable to translate".
expect_bytes <- function(actual, expected) {
  expect_identical(
    if (is.null(actual)) NULL else as.integer(charToRaw(actual)),
    as.integer(expected)
  )
}

test_that("a plain URL decomposes into libcurl's field names and spellings", {
  r <- p("http://u:pw@example.com:8080/a/b?q=1#frag")
  expect_identical(r$scheme, "http")
  expect_identical(r$host, "example.com")
  expect_identical(r$port, "8080")
  expect_identical(r$path, "/a/b")
  expect_identical(r$query, "q=1")
  expect_identical(r$fragment, "frag")
  expect_identical(r$user, "u")
  expect_identical(r$password, "pw")
  expect_identical(r$url, "http://u:pw@example.com:8080/a/b?q=1#frag")
})

test_that("absent components are NULL, not NA or empty string", {
  r <- p("http://example.com/")
  expect_null(r$port)
  expect_null(r$query)
  expect_null(r$fragment)
  expect_null(r$user)
  expect_null(r$password)
  # A present-but-empty query/fragment is also NULL: the "?" alone carries no
  # component. (`.blank_to_na()` downstream never sees an "" from here.)
  expect_null(p("http://example.com/?")$query)
  expect_null(p("http://example.com/#")$fragment)
  expect_null(p("http://example.com/?#")$query)
})

test_that("the scheme is ASCII-lowercased and the grammar is enforced", {
  expect_identical(p("HtTp://Ex.CoM/Path?Q#F")$scheme, "http")
  # Host, path, query and fragment case is NOT touched -- only the scheme.
  expect_identical(p("HtTp://Ex.CoM/Path?Q#F")$host, "Ex.CoM")
  expect_identical(p("HtTp://Ex.CoM/Path?Q#F")$path, "/Path")
  expect_identical(p("h+t-p.x://example.com/")$scheme, "h+t-p.x")
  expect_null(p("1http://example.com/"))   # must start with ALPHA
  expect_null(p("://example.com/"))        # no scheme at all
  expect_null(p("h t://example.com/"))     # space is not a scheme character
})

test_that("1 to 3 slashes are accepted and 3 promotes a path segment to host", {
  expect_identical(fp("http:/example.com/"), "http|example.com|-|/|-|-|-|-")
  expect_identical(fp("http://example.com/"), "http|example.com|-|/|-|-|-|-")
  # Three slashes = EMPTY authority; libcurl slides the first path segment into
  # the host. This is the shape `.extract_raw_path_vec()` compensates for.
  expect_identical(fp("http:///a/b"), "http|a|-|/b|-|-|-|-")
  expect_identical(fp("http:///a?q#f"), "http|a|-|/|q|f|-|-")
  # No slash, or four or more, is a parse error.
  expect_null(p("http:example.com/"))
  expect_null(p("http:////a"))
  expect_null(p("http://///a"))
  expect_null(p("http:///"))    # empty authority with nothing to promote
  expect_null(p("http://"))     # empty authority
})

test_that("the port must be digits within range, and loses leading zeros", {
  expect_identical(p("http://example.com:80/")$port, "80")
  expect_identical(p("http://example.com:080/")$port, "80")
  expect_identical(p("http://example.com:00080/")$port, "80")
  expect_identical(p("http://example.com:0/")$port, "0")
  expect_identical(p("http://example.com:65535/")$port, "65535")
  # An EMPTY port is no port, not an error.
  expect_null(p("http://example.com:/")$port)
  expect_identical(p("http://example.com:/")$host, "example.com")
  expect_null(p("http://example.com:65536/"))
  expect_null(p("http://example.com:8a/"))
  expect_null(p("http://example.com:+80/"))
  expect_null(p("http://example.com: 80/"))
})

test_that("userinfo splits at the first colon and admits at most one '@'", {
  expect_identical(fp("http://u@example.com/"), "http|example.com|-|/|-|-|u|-")
  expect_identical(
    fp("http://u:p:q@example.com/"), "http|example.com|-|/|-|-|u|p:q"
  )
  # Empty sides are reported as "" (the caller's `.blank_to_na()` maps them).
  expect_identical(p("http://@example.com/")$user, "")
  expect_identical(p("http://u:@example.com/")$password, "")
  expect_identical(p("http://:p@example.com/")$user, "")
  # A second "@" is a parse error whichever side it lands on -- under the
  # DEFAULT dial. `last_at_userinfo = TRUE` recovers it; see below.
  expect_null(p("http://a@b@example.com/"))
  # An already-encoded "@" is not a second delimiter and never was.
  expect_identical(p("http://a%40b@example.com/")$user, "a%40b")
})

test_that("last_at_userinfo splits at the last '@' and encodes the earlier", {
  # WHATWG's authority state buffers to the FINAL "@" and prepends "%40" for
  # each earlier one. Until RURL-ezhzpkhg deletion 3 a pre-parse rewrite
  # (`.encode_excess_authority_at_vec`) did this to the input string because
  # libcurl refused a second "@"; it is parser behaviour now, and these
  # literals are the ones that rewrite used to produce.
  expect_identical(
    fp("http://username@@@@example.com/", last = TRUE),
    "http|example.com|-|/|-|-|username%40%40%40|-"
  )
  # The dial is OFF by default, and off means reject, not "parse loosely".
  expect_identical(fp("http://username@@@@example.com/"), "REJECT")

  # The split is by the LAST "@", so "@" on either side of the ":" lands in
  # whichever half it falls in -- the ":" split runs on the encoded userinfo,
  # and "%40" can neither create nor destroy a ":".
  expect_identical(
    fp("http://u@v:p@example.com/", last = TRUE),
    "http|example.com|-|/|-|-|u%40v|p"
  )
  expect_identical(
    fp("http://u:p@v@example.com/", last = TRUE),
    "http|example.com|-|/|-|-|u|p%40v"
  )
  expect_identical(
    fp("http://u:@v@example.com/", last = TRUE),
    "http|example.com|-|/|-|-|u|%40v"
  )

  # Degenerate authorities: an empty userinfo, and a host that is only "@"s.
  expect_identical(p("http://@@example.com/", last = TRUE)$user, "%40")
  expect_identical(fp("http://a@@/", last = TRUE), "REJECT") # empty host
  expect_identical(
    fp("http://a@@b.com:80/", last = TRUE),
    "http|b.com|80|/|-|-|a%40|-"
  )

  # A single "@" is unaffected by the dial, in either position.
  expect_identical(
    fp("http://u:p@example.com/"), fp("http://u:p@example.com/", last = TRUE)
  )

  # "@" past the authority is not the authority's business and never moves.
  expect_identical(
    fp("http://a@@b.com/p@q?r@s#t@u", last = TRUE),
    "http|b.com|-|/p@q|r@s|t@u|a%40|-"
  )

  # An already-encoded "@" is not double-encoded: "%40" carries no 0x40 byte.
  expect_identical(
    fp("http://a%40b@@example.com/", last = TRUE),
    "http|example.com|-|/|-|-|a%40b%40|-"
  )
})

test_that("userinfo is percent-uppercased but never percent-decoded", {
  expect_identical(p("http://a%2eb@example.com/")$user, "a%2Eb")
  expect_identical(p("http://a%41b@example.com/")$user, "a%41b")
  # Uppercasing is applied to each side of the ":" independently.
  expect_identical(p("http://a%2eb:c%3fd@example.com/")$password, "c%3Fd")
})

test_that("the host is percent-decoded, then validated", {
  expect_identical(p("http://a%2Eb/")$host, "a.b")
  expect_identical(p("http://a%41b/")$host, "aAb")
  # A "%" without two hex digits after it is a parse error in the host
  # (unlike in the path, where it is simply left alone).
  expect_null(p("http://a%zz/"))
  expect_null(p("http://a%2/"))
  # Escapes that decode to a forbidden byte reject just as the literal does.
  expect_null(p("http://a%2Fb/"))   # "/"
  expect_null(p("http://a%25b/"))   # "%"
  expect_null(p("http://a%40b/"))   # "@"
  expect_null(p("http://a%3Ab/"))   # ":"
  expect_null(p("http://a%20b/"))   # SP
  expect_null(p("http://a%00b/"))   # NUL
  # DEL is the ONE byte whose pre- and post-decode verdicts differ: rejected
  # written literally, kept when written as an escape.
  expect_identical(p("http://ho%7Fst/")$host, "ho\u007fst")
})

# `host_pct` (RURL-rgjpcbuk). The three settings differ on TWO things at once,
# and the pair is the whole design: which decoded bytes are ADMITTED, and how
# the admitted host is SPELLED. Validation always runs on the fully decoded
# host -- a "/" is a "/" however it is written -- so a spelling rule can never
# widen acceptance. That separation is what the pre-parse mask could not
# express: masking a triplet to change the spelling also stopped every OTHER
# triplet in the same host from being judged at all.
test_that("host_pct = 'wide' admits the code points WHATWG keeps in a host", {
  # Same code point, both spellings, same verdict -- the RURL-rgjpcbuk report.
  expect_identical(p("http://ex.com%60x/", pct = "wide")$host, "ex.com`x")
  expect_identical(p("http://ex.com%21x/", pct = "wide")$host, "ex.com!x")
  expect_identical(p("http://ex.com%7Bx/", pct = "wide")$host, "ex.com{x")
  # ...and "narrow" still refuses it, so the widening is the dial's alone.
  expect_null(p("http://ex.com%60x/"))
  # Only the 15. A forbidden domain code point rejects however it is written.
  expect_null(p("http://a%2Fb/", pct = "wide")) # "/"
  expect_null(p("http://a%25b/", pct = "wide")) # "%"
  expect_null(p("http://a%01b/", pct = "wide")) # C0
  # The LITERAL gap character is still rejected here: the raw-token check is
  # untouched, so ADR 0009's shim still owns that spelling (deletion 1).
  expect_null(p("http://ex.com`x/", pct = "wide"))
})

test_that("host_pct = 'keep' decodes only the unreserved triplets", {
  # RFC 3986 section 6.2.2.2: unreserved decodes, everything else stands.
  expect_identical(p("http://a%2Eb/", pct = "keep")$host, "a.b")
  expect_identical(p("http://a%41b/", pct = "keep")$host, "aAb")
  expect_identical(p("http://a%7Eb/", pct = "keep")$host, "a~b")
  expect_identical(p("http://ex.com%60x/", pct = "keep")$host, "ex.com%60x")
  expect_identical(p("http://ho%7Fst/", pct = "keep")$host, "ho%7Fst")
  # Section 6.2.2.1: the retained triplet's hex is uppercased.
  expect_identical(p("http://ex.com%7bx/", pct = "keep")$host, "ex.com%7Bx")
  # A percent-encoded non-ASCII host keeps its source spelling rather than
  # becoming raw UTF-8 -- the shape that makes this dial visible in practice.
  expect_identical(p("http://a%C3%A9b.com/", pct = "keep")$host, "a%C3%A9b.com")
  # Judged decoded all the same: these reject exactly as under "wide".
  expect_null(p("http://a%2Fb/", pct = "keep"))
  expect_null(p("http://a%01b/", pct = "keep"))
  expect_null(p("http://a%00b/", pct = "keep"))
  expect_null(p("http://a%C3b/", pct = "keep")) # invalid UTF-8 once decoded
  # Malformed "%" is a parse error under every setting.
  expect_null(p("http://a%zz/", pct = "keep"))
  expect_null(p("http://a%2/", pct = "keep"))
})

test_that("host_pct changes the host alone, never the other components", {
  u <- "http://u%60v:p%60w@ex.com%60x:8080/a%60b?q%60=1#f%60g"
  # userinfo/path/query/fragment are byte-identical across the settings; only
  # `host` moves. The mask this replaces reached them by construction.
  rest <- function(pct) {
    r <- p(u, pct = pct)
    paste(r$scheme, r$port, r$path, r$query, r$fragment, r$user, r$password)
  }
  expect_identical(rest("wide"), rest("keep"))
  expect_identical(rest("wide"), "http 8080 /a%60b q%60=1 f%60g u%60v p%60w")
  expect_identical(p(u, pct = "wide")$host, "ex.com`x")
  expect_identical(p(u, pct = "keep")$host, "ex.com%60x")
  expect_null(p(u))
})

# `pqf_bytes` (RURL-ezhzpkhg deletion 5). The dial decides ONE thing: whether a
# C0 control, SP or DEL outside the authority is a parse error or is escaped in
# place. It is deliberately NOT the rest of the WHATWG percent-encode sets --
# those are rendering, they belong to the serializer, and conflating them is
# what the deleted pqf fallback did.

test_that("pqf_bytes = 'reject' refuses C0/SP/DEL outside the authority", {
  for (u in c("http://h.com/a b", "http://h.com/a\u0001b",
              "http://h.com/a\u007fb", "http://h.com/a\u000bb",
              "http://h.com/p?q= 1", "http://h.com/p?q=\u0001",
              "http://h.com/p#f g", "http://h.com/p#f\u007f")) {
    expect_null(p(u), info = u)
  }
})

test_that("pqf_bytes = 'encode' escapes those bytes instead of refusing", {
  expect_identical(p("http://h.com/a b", pqf = "encode")$path, "/a%20b")
  expect_identical(p("http://h.com/a\u0001b", pqf = "encode")$path, "/a%01b")
  expect_identical(p("http://h.com/a\u007fb", pqf = "encode")$path, "/a%7Fb")
  # VT and FF are ordinary C0 controls: WHATWG's leading/trailing strip and its
  # tab/LF/CR removal do not touch them, so they escape like any other. The
  # deleted fallback could never reach them -- it matched the post-authority
  # remainder with an ICU `.`, which excludes the Unicode line terminators.
  expect_identical(p("http://h.com/a\u000bb", pqf = "encode")$path, "/a%0Bb")
  expect_identical(p("http://h.com/a\u000cb", pqf = "encode")$path, "/a%0Cb")
  expect_identical(p("http://h.com/p?q= 1", pqf = "encode")$query, "q=%201")
  expect_identical(p("http://h.com/p#f g", pqf = "encode")$fragment, "f%20g")
})

test_that("pqf_bytes = 'encode' escapes ONLY those bytes", {
  # Every other member of the WHATWG path/query/fragment encode sets stays
  # literal here. The serializer applies them; the parser does not.
  expect_identical(
    p("http://h.com/a<b>c`d{e}f\"g", pqf = "encode")$path,
    "/a<b>c`d{e}f\"g"
  )
  expect_identical(p("http://h.com/p?a<b'c", pqf = "encode")$query, "a<b'c")
  expect_identical(p("http://h.com/p#a<b`c", pqf = "encode")$fragment, "a<b`c")
  # And an existing triplet is left exactly as written apart from the standing
  # "%XX" uppercase pass, which is not this dial's doing.
  expect_identical(p("http://h.com/a%2fb c", pqf = "encode")$path, "/a%2Fb%20c")
})

test_that("pqf_bytes does not reach the authority", {
  # A space or control in the host/userinfo is still a parse error under
  # `"encode"`: this dial is scoped to path/query/fragment.
  expect_null(p("http://a b.com/p", pqf = "encode"))
  expect_null(p("http://a\u0001b.com/p", pqf = "encode"))
  expect_null(p("http://u v@h.com/p", pqf = "encode"))
  expect_null(p("http://h.com:8 0/p", pqf = "encode"))
})

test_that("pqf_bytes leaves everything else about a parse alone", {
  u <- "http://u:p@ex.com:8080/a b?q= 1#f g"
  expect_identical(
    fp(u, pqf = "encode"),
    "http|ex.com|8080|/a%20b|q=%201|f%20g|u|p"
  )
  # Dot segments still resolve, and they resolve AFTER the escaping -- so a
  # space cannot smuggle a segment past the removal.
  expect_identical(p("http://h.com/a/../b c", pqf = "encode")$path, "/b%20c")
  expect_identical(p("http://h.com/a b/../c", pqf = "encode")$path, "/c")
  # An absent component stays absent; escaping never fabricates one.
  expect_null(p("http://h.com/a b", pqf = "encode")$query)
  expect_null(p("http://h.com/a b", pqf = "encode")$fragment)
})

test_that("host code points outside libcurl's set are rejected", {
  # The full measured allowed set is [A-Za-z0-9._~|-]. Note "|" is IN it,
  # although WHATWG forbids it in a host.
  expect_identical(p("http://a|b/")$host, "a|b")
  expect_identical(p("http://a_b~c-d.e/")$host, "a_b~c-d.e")
  for (ch in c("!", "\"", "$", "%", "&", "'", "(", ")", "*", "+", ",",
               ";", "<", "=", ">", "^", "`", "{", "}")) {
    expect_null(p(paste0("http://a", ch, "b/")), info = ch)
  }
  # C0 controls, space and DEL, written literally.
  expect_null(p("http://a\u0001b/"))
  expect_null(p("http://a b/"))
  expect_null(p("http://a\u007fb/"))
})

test_that("host and userinfo take raw high bytes only as well-formed UTF-8", {
  # Expectations are OCTETS -- see `expect_bytes()` above. The comments name
  # the characters those octets spell.
  expect_bytes(p("http://a\u00e9b/")$host,             # "a\u00e9b"
               as.raw(c(0x61, 0xC3, 0xA9, 0x62)))
  expect_bytes(p("http://\u4e2d\u6587.com/")$host,     # "\u4e2d\u6587.com"
               as.raw(c(0xE4, 0xB8, 0xAD, 0xE6, 0x96, 0x87,
                        0x2E, 0x63, 0x6F, 0x6D)))
  expect_bytes(p("http://u\u00e9x@example.com/")$user, # "u\u00e9x"
               as.raw(c(0x75, 0xC3, 0xA9, 0x78)))
  # A lone continuation byte is not valid UTF-8 -> parse error, in EVERY
  # locale. This is the locale-invariance pin, held here by construction.
  lone <- function(byte) rawToChar(as.raw(byte))
  expect_null(p(paste0("http://a", lone(0x80L), "b/")))
  expect_null(p(paste0("http://a", lone(0xC3L), "b/")))
  expect_null(p(paste0("http://u", lone(0x80L), "x@example.com/")))
  # Percent-escapes decode to the same test: valid UTF-8 passes, a lone byte
  # does not.
  expect_bytes(p("http://a%C3%A9b/")$host,             # "a\u00e9b"
               as.raw(c(0x61, 0xC3, 0xA9, 0x62)))
  expect_null(p("http://a%80b/"))
  # The bytes come back UNDECLARED: the parser slices the input and never
  # re-marks an encoding. Pinned because a mark is precisely what made the
  # comparisons above locale-dependent when they were written as literals.
  expect_identical(Encoding(p("http://a\u00e9b/")$host), "unknown")
  expect_identical(Encoding(p("http://a%C3%A9b/")$host), "unknown")
})

test_that("IPv4 normalization is reproduced, including its refusals", {
  expect_identical(p("http://0x7f.1/")$host, "127.0.0.1")
  expect_identical(p("http://127.1/")$host, "127.0.0.1")
  expect_identical(p("http://0177.0.0.1/")$host, "127.0.0.1")
  expect_identical(p("http://2130706433/")$host, "127.0.0.1")
  expect_identical(p("http://1.2.3.04/")$host, "1.2.3.4")
  expect_identical(p("http://0xffffffff/")$host, "255.255.255.255")
  # Tokens that are NOT addresses stay registered names -- never an error.
  expect_identical(p("http://1.2.3.4.5/")$host, "1.2.3.4.5")
  expect_identical(p("http://999.1.1.1/")$host, "999.1.1.1")
  expect_identical(p("http://4294967296/")$host, "4294967296")
  expect_identical(p("http://0x/")$host, "0x")           # empty hex digits
  expect_identical(p("http://1.2.3.4./")$host, "1.2.3.4.") # trailing dot
  # Only a LOWERCASE "0x" prefix is hex to libcurl.
  expect_identical(p("http://0Xff/")$host, "0Xff")
  expect_identical(p("http://0xff/")$host, "0.0.0.255")
  # A percent-escape anywhere in the host SUPPRESSES the numeric reading.
  expect_identical(
    p("http://%30%78%63%30%2e%30%32%35%30.01/")$host, "0xc0.0250.01"
  )
})

test_that("an IPv6 literal is normalized only when that comes out shorter", {
  # THE rule: libcurl adopts inet_ntop's spelling only if it is strictly
  # shorter than the source; otherwise the source stands, case and all.
  # Shorter -> rewritten (and inet_ntop lowercases).
  expect_identical(p("http://[0:0:0:0:0:0:0:1]/")$host, "[::1]")
  expect_identical(p("http://[0001:0002::]/")$host, "[1:2::]")
  expect_identical(p("http://[0ABC::0DEF]/")$host, "[abc::def]")
  expect_identical(p("http://[ABCD:0::EF]/")$host, "[abcd::ef]")
  expect_identical(p("http://[1:2:3:4:5:6:0:0]/")$host, "[1:2:3:4:5:6::]")
  expect_identical(p("http://[1:0:0:0:2:0:0:3]/")$host, "[1::2:0:0:3]")
  expect_identical(p("http://[::0.0.0.0]/")$host, "[::]")
  # Not shorter -> verbatim, INCLUDING uppercase and leading zeros.
  expect_identical(p("http://[AB::CD]/")$host, "[AB::CD]")
  expect_identical(p("http://[ABCD::EF]/")$host, "[ABCD::EF]")
  expect_identical(p("http://[::ffff:0:0]/")$host, "[::ffff:0:0]")
  expect_identical(p("http://[::1:000]/")$host, "[::1:000]")
  expect_identical(p("http://[::0fff:0001]/")$host, "[::0fff:0001]")
  # The same ADDRESS, two spellings, two answers -- this is the pair that
  # makes the rule look value-dependent when it is purely about length.
  expect_identical(p("http://[::1:2]/")$host, "[::1:2]")
  expect_identical(p("http://[::0001:0002]/")$host, "[::0.1.0.2]")
  # A "%" anywhere inside the brackets is a parse error (RURL-ezhzpkhg). This
  # is a DELIBERATE departure from libcurl, which dropped a "%zone" suffix and
  # kept the address; the seam previously copied that and so accepted
  # `[::1%]`, which libcurl itself rejected. Neither host model rurl ships has
  # a zone production -- WHATWG forbids "%" in an IPv6 address, and RFC 9844
  # restored RFC 3986's zone-less `IP-literal` -- and the downstream host gate
  # was already rejecting every one of these rows, so this narrows the seam
  # onto the gates behind it rather than moving any public output.
  expect_null(p("http://[::1%25eth0]/"))
  expect_null(p("http://[::1%eth0]/"))
  expect_null(p("http://[::1%]/"))
  # Invalid literals are parse errors.
  expect_null(p("http://[zz]/"))
  expect_null(p("http://[1:2:3]/"))
  expect_null(p("http://[1:2:3:4:5:6:7:8:9]/"))
  expect_null(p("http://[::1::2]/"))
  expect_null(p("http://[v1.x]/"))   # no IPvFuture
})

test_that("inet_ntop's dotted-quad rule is libcurl's narrowed one", {
  # Dotted only when the leading zero run is 6, or is 5 and word[5] is ffff.
  expect_identical(p("http://[::0808:0808]/")$host, "[::8.8.8.8]")
  expect_identical(p("http://[::ffff:0808:0808]/")$host, "[::ffff:8.8.8.8]")
  expect_identical(
    p("http://[0:0:0:0:0:ffff:1.2.3.4]/")$host, "[::ffff:1.2.3.4]"
  )
  # A run of 7 does NOT go dotted -- the BSD/glibc clause libcurl omits. Were
  # it present, `[::0:2]` would come back "[::0.0.0.2]".
  expect_identical(p("http://[::0:2]/")$host, "[::2]")
  expect_identical(p("http://[::0:1]/")$host, "[::1]")
})

test_that("the path defaults to '/' and resolves dot segments", {
  expect_identical(p("http://example.com")$path, "/")
  expect_identical(p("http://example.com?q")$path, "/")
  expect_identical(p("http://example.com#f")$path, "/")
  expect_identical(p("http://example.com/a/../b")$path, "/b")
  expect_identical(p("http://example.com/a/./b")$path, "/a/b")
  expect_identical(p("http://example.com/a/../../../b")$path, "/b")
  expect_identical(p("http://example.com/..")$path, "/")
  # Encoded dot segments resolve too -- unlike RFC 3986 section 5.2.4, which
  # recognizes only literal "." and "..".
  expect_identical(p("http://example.com/%2e%2e/b")$path, "/b")
  expect_identical(p("http://example.com/a/%2E%2E/b")$path, "/b")
  # ...but only when the WHOLE segment is a dot segment.
  expect_identical(p("http://example.com/a%2e.")$path, "/a%2E.")
  # Empty segments are preserved; no slash collapsing happens here.
  expect_identical(p("http://example.com//a")$path, "//a")
})

test_that("path, query and fragment reject C0, SP and DEL and encode >= 0x80", {
  expect_null(p("http://example.com/a b"))
  expect_null(p("http://example.com/?a b"))
  expect_null(p("http://example.com/#a b"))
  expect_null(p("http://example.com/a\u0001b"))
  expect_null(p("http://example.com/a\u007fb"))
  expect_identical(p("http://example.com/\u00e9")$path, "/%C3%A9")
  expect_identical(p("http://example.com/?a\u00e9b")$query, "a%C3%A9b")
  expect_identical(p("http://example.com/#a\u00e9b")$fragment, "a%C3%A9b")
})

test_that("percent pairs are uppercased by a sequential three-byte scan", {
  expect_identical(p("http://example.com/a%2fb")$path, "/a%2Fb")
  expect_identical(p("http://example.com/?a%2fb")$query, "a%2Fb")
  expect_identical(p("http://example.com/#a%2fb")$fragment, "a%2Fb")
  # Not just valid hex: the PAIR is uppercased.
  expect_identical(p("http://example.com/?a%zzb")$query, "a%ZZb")
  # A "%" with fewer than two bytes after it is left alone.
  expect_identical(p("http://example.com/?a%b")$query, "a%b")
  expect_identical(p("http://example.com/a%b")$path, "/a%b")
  # The scan consumes three bytes per "%", so in "%%2e" the first "%" claims
  # the pair "%2" and the "e" is never a pair member.
  expect_identical(p("http://example.com/?%%2e")$query, "%%2e")
  # ...and a freshly encoded high byte can be swallowed the same way, which is
  # why the encoder emits lowercase and the scan decides the final case.
  high <- paste0("http://example.com/foo%2", rawToChar(as.raw(0xC3L)),
                 rawToChar(as.raw(0x82L)), "z")
  expect_identical(p(high)$path, "/foo%2%c3%82z")
})

test_that("the query/fragment split is on the FIRST '#' then the FIRST '?'", {
  expect_identical(fp("http://e.com/a?b#c?d"), "http|e.com|-|/a|b|c?d|-|-")
  expect_identical(fp("http://e.com/#a#b"), "http|e.com|-|/|-|a#b|-|-")
  # A "?" after the "#" belongs to the fragment, not the query.
  expect_null(p("http://e.com/#a?b")$query)
})

test_that("NA input is a parse error rather than an error condition", {
  expect_null(p(NA_character_))
})
